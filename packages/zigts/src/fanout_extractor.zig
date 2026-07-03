//! Walks the IR for every `fanout([...])` call site (imported from
//! `zigttp:workflow`) and extracts each array descriptor's dispatch target
//! into a `WorkflowCallInfo`, appended directly into the caller-owned list
//! that `call(name, init)`'s own `contract_extractions` also populates
//! (`ContractBuilder.workflow_calls` / `HandlerContract.workflow_calls`) so
//! the system linker resolves both through one uniform proof. Mirrors
//! `saga_extractor.zig`'s literal-walk-or-bail discipline: a static array of
//! descriptor object literals extracts cleanly; anything else discards the
//! whole batch and appends ONE dynamic sentinel for that call site, matching
//! `SagaCallInfo`'s "dynamic implies empty" convention.
//!
//! ## Expected literal shape
//!
//! ```ts
//! fanout([
//!   { name: "inventory", path: "/check" },
//!   { name: "pricing", method: "POST", path: "/quote" },
//! ]);
//! ```
//!
//! Recognized keys: `name` (literal string, required), `method`/`path`
//! (literal strings, optional, default "GET"/"/"), `body`/`headers` (presence
//! only, no proof surface). An unknown sibling key or non-literal method/path
//! marks the whole batch dynamic, matching `intent_extractor.zig`'s "reject
//! unknown sibling keys to keep the surface deterministic" convention.

const std = @import("std");
const ir_mod = @import("parser/ir.zig");
const contract_types = @import("contract_types.zig");

const IrView = ir_mod.IrView;
const NodeIndex = ir_mod.NodeIndex;
const WorkflowCallInfo = contract_types.WorkflowCallInfo;

pub const AtomResolver = *const fn (atom_idx: u16, ctx: *const anyopaque) ?[]const u8;

pub const Deps = struct {
    allocator: std.mem.Allocator,
    ir_view: IrView,
    resolver: AtomResolver,
    resolver_ctx: *const anyopaque,
};

const workflow_module_specifier = "zigttp:workflow";
const fanout_export_name = "fanout";

/// Extract every `fanout([...])` call site's descriptors in the module,
/// appending each into `out`. No-op when `zigttp:workflow`'s `fanout` is
/// never imported.
pub fn extract(deps: Deps, out: *std.ArrayList(WorkflowCallInfo)) !void {
    const fanout_slot = findFanoutImportSlot(deps) orelse return;

    const node_count = deps.ir_view.nodeCount();
    var i: usize = 0;
    while (i < node_count) : (i += 1) {
        const idx: NodeIndex = @intCast(i);
        const tag = deps.ir_view.getTag(idx) orelse continue;
        if (tag != .call) continue;

        const call = deps.ir_view.getCall(idx) orelse continue;
        if (deps.ir_view.getTag(call.callee) != .identifier) continue;
        const binding = deps.ir_view.getBinding(call.callee) orelse continue;
        if (binding.slot != fanout_slot) continue;

        try extractCall(deps, call, out);
    }
}

/// Find `fanout`'s local binding slot from a `zigttp:workflow` import.
/// Returns `null` if `fanout` is never imported from that module.
fn findFanoutImportSlot(deps: Deps) ?u16 {
    const node_count = deps.ir_view.nodeCount();
    var i: usize = 0;
    while (i < node_count) : (i += 1) {
        const idx: NodeIndex = @intCast(i);
        const tag = deps.ir_view.getTag(idx) orelse continue;
        if (tag != .import_decl) continue;

        const decl = deps.ir_view.getImportDecl(idx) orelse continue;
        const module_str = deps.ir_view.getString(decl.module_idx) orelse continue;
        if (!std.mem.eql(u8, module_str, workflow_module_specifier)) continue;

        var j: u8 = 0;
        while (j < decl.specifiers_count) : (j += 1) {
            const spec_idx = deps.ir_view.getListIndex(decl.specifiers_start, j);
            const spec = deps.ir_view.getImportSpec(spec_idx) orelse continue;
            const imported_name = resolverCall(deps, spec.imported_atom) orelse continue;
            if (std.mem.eql(u8, imported_name, fanout_export_name)) return spec.local_binding.slot;
        }
    }
    return null;
}

fn extractCall(deps: Deps, call: ir_mod.Node.CallExpr, out: *std.ArrayList(WorkflowCallInfo)) !void {
    if (call.args_count == 0) {
        try out.append(deps.allocator, try dynamicSentinel(deps));
        return;
    }
    const arr_idx = deps.ir_view.getListIndex(call.args_start, 0);
    try collectDescriptors(deps, arr_idx, out);
}

fn dynamicSentinel(deps: Deps) !WorkflowCallInfo {
    return .{
        .target = try deps.allocator.dupe(u8, ""),
        .route_pattern = try deps.allocator.dupe(u8, ""),
        .dynamic = true,
    };
}

/// Collect every descriptor from a `fanout([...])` array into a function-
/// local batch, transferring the whole batch into `out` on full success, or
/// discarding it and appending one dynamic sentinel on any structural
/// failure. A single `defer` owns the batch for the entire function: on
/// success the batch is emptied after its payloads move to `out`, so the
/// defer frees only the backing buffer; on failure (or any error return) the
/// defer frees every partially-collected descriptor.
fn collectDescriptors(deps: Deps, array_idx: NodeIndex, out: *std.ArrayList(WorkflowCallInfo)) !void {
    var batch: std.ArrayList(WorkflowCallInfo) = .empty;
    defer {
        for (batch.items) |*info| info.deinit(deps.allocator);
        batch.deinit(deps.allocator);
    }

    if (try collectDescriptorsInto(deps, array_idx, &batch)) {
        try out.appendSlice(deps.allocator, batch.items);
        // Ownership of each descriptor's slices moved to `out`; empty the batch
        // so the defer frees only the backing buffer, not the moved payloads.
        batch.clearRetainingCapacity();
    } else {
        try out.append(deps.allocator, try dynamicSentinel(deps));
    }
}

/// Returns `true` when every element of the array literal at `array_idx` is
/// a valid descriptor object literal (all appended to `batch`), `false` on
/// the first structural failure (not an array literal, spread element,
/// non-object element, or an unparseable descriptor).
fn collectDescriptorsInto(deps: Deps, array_idx: NodeIndex, batch: *std.ArrayList(WorkflowCallInfo)) !bool {
    const arr_tag = deps.ir_view.getTag(array_idx) orelse return false;
    if (arr_tag != .array_literal) return false;
    const arr = deps.ir_view.getArray(array_idx) orelse return false;
    if (arr.has_spread) return false;

    var i: u32 = 0;
    while (i < arr.elements_count) : (i += 1) {
        const elem_idx = deps.ir_view.getListIndex(arr.elements_start, @intCast(i));
        const elem_tag = deps.ir_view.getTag(elem_idx) orelse return false;
        if (elem_tag != .object_literal) return false;

        const info = parseDescriptor(deps, elem_idx) catch |err| switch (err) {
            error.NotLiteral => return false,
            else => return err,
        };
        try batch.append(deps.allocator, info);
    }
    return true;
}

const ParseError = error{ NotLiteral, OutOfMemory };

fn parseDescriptor(deps: Deps, obj_idx: NodeIndex) ParseError!WorkflowCallInfo {
    const obj = deps.ir_view.getObject(obj_idx) orelse return error.NotLiteral;

    var target: ?[]u8 = null;
    var method: []const u8 = "GET";
    var path: []const u8 = "/";
    errdefer if (target) |t| deps.allocator.free(t);

    var p: u32 = 0;
    while (p < obj.properties_count) : (p += 1) {
        const prop_idx = deps.ir_view.getListIndex(obj.properties_start, @intCast(p));
        const prop_tag = deps.ir_view.getTag(prop_idx) orelse return error.NotLiteral;
        if (prop_tag != .object_property) return error.NotLiteral;
        const prop = deps.ir_view.getProperty(prop_idx) orelse return error.NotLiteral;
        if (prop.is_computed) return error.NotLiteral;

        const key = propKeyName(deps, prop.key) orelse return error.NotLiteral;

        if (std.mem.eql(u8, key, "name")) {
            if (target != null) return error.NotLiteral;
            const s = literalString(deps, prop.value) orelse return error.NotLiteral;
            target = try deps.allocator.dupe(u8, s);
        } else if (std.mem.eql(u8, key, "method")) {
            method = literalString(deps, prop.value) orelse return error.NotLiteral;
        } else if (std.mem.eql(u8, key, "path")) {
            path = literalString(deps, prop.value) orelse return error.NotLiteral;
        } else if (std.mem.eql(u8, key, "body") or std.mem.eql(u8, key, "headers")) {
            // Presence only; no proof surface.
        } else {
            return error.NotLiteral;
        }
    }

    const t = target orelse return error.NotLiteral;
    const route_pattern = try std.fmt.allocPrint(deps.allocator, "{s} {s}", .{ method, path });

    return .{ .target = t, .route_pattern = route_pattern };
}

fn literalString(deps: Deps, idx: NodeIndex) ?[]const u8 {
    const tag = deps.ir_view.getTag(idx) orelse return null;
    if (tag != .lit_string) return null;
    const str_idx = deps.ir_view.getStringIdx(idx) orelse return null;
    return deps.ir_view.getString(str_idx);
}

fn propKeyName(deps: Deps, key_idx: NodeIndex) ?[]const u8 {
    const tag = deps.ir_view.getTag(key_idx) orelse return null;
    switch (tag) {
        .lit_string => return literalString(deps, key_idx),
        .identifier => {
            const binding = deps.ir_view.getBinding(key_idx) orelse return null;
            return resolverCall(deps, binding.slot);
        },
        else => return null,
    }
}

inline fn resolverCall(deps: Deps, atom_idx: u16) ?[]const u8 {
    return deps.resolver(atom_idx, deps.resolver_ctx);
}
