//! Walks the IR for every `saga([...])` call site (imported from
//! `zigttp:workflow`) and extracts its step structure for ZTS510's
//! compensation-coverage proof. Mirrors `intent_extractor.zig`'s
//! literal-walk-or-bail discipline: a static array of step object literals
//! extracts cleanly; anything else marks the call `dynamic = true` and
//! clears any partially-collected steps, so a caller never sees a
//! half-populated proof used as if sound.
//!
//! ## Expected literal shape
//!
//! ```ts
//! saga([
//!   { name: "reserve", run: () => call("inv", {}), compensate: () => call("inv", { path: "/release" }) },
//!   { name: "ship", run: () => call("ship", {}) },   // last step, compensate optional
//! ]);
//! ```
//!
//! Only `name` (a literal string, required), `run` (any value, presence
//! only), and `compensate` (any value, presence only) are recognized keys;
//! an unknown sibling key marks the call dynamic, matching
//! `intent_extractor.zig`'s "reject unknown sibling keys to keep the
//! surface deterministic" convention.

const std = @import("std");
const ir_mod = @import("parser/ir.zig");
const handler_contract = @import("handler_contract.zig");

const IrView = ir_mod.IrView;
const NodeIndex = ir_mod.NodeIndex;
const SagaCallInfo = handler_contract.SagaCallInfo;
const SagaStep = handler_contract.SagaStep;

pub const AtomResolver = *const fn (atom_idx: u16, ctx: *const anyopaque) ?[]const u8;

pub const Deps = struct {
    allocator: std.mem.Allocator,
    ir_view: IrView,
    resolver: AtomResolver,
    resolver_ctx: *const anyopaque,
};

const workflow_module_specifier = "zigttp:workflow";
const saga_export_name = "saga";

/// Extract every `saga([...])` call site in the module. Returns an empty
/// list when `zigttp:workflow`'s `saga` is never imported. The caller owns
/// the returned list and must `deinit` each entry before deiniting the list.
pub fn extract(deps: Deps) !std.ArrayList(SagaCallInfo) {
    var out: std.ArrayList(SagaCallInfo) = .empty;
    errdefer {
        for (out.items) |*info| info.deinit(deps.allocator);
        out.deinit(deps.allocator);
    }

    const saga_slot = findSagaImportSlot(deps) orelse return out;

    const node_count = deps.ir_view.nodeCount();
    var i: usize = 0;
    while (i < node_count) : (i += 1) {
        const idx: NodeIndex = @intCast(i);
        const tag = deps.ir_view.getTag(idx) orelse continue;
        if (tag != .call) continue;

        const call = deps.ir_view.getCall(idx) orelse continue;
        if (deps.ir_view.getTag(call.callee) != .identifier) continue;
        const binding = deps.ir_view.getBinding(call.callee) orelse continue;
        if (binding.slot != saga_slot) continue;

        try out.append(deps.allocator, try extractCall(deps, idx, call));
    }

    return out;
}

/// Find `saga`'s local binding slot from a `zigttp:workflow` import. Returns
/// `null` if `saga` is never imported from that module (the common case for
/// handlers that don't use sagas at all).
fn findSagaImportSlot(deps: Deps) ?u16 {
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
            if (std.mem.eql(u8, imported_name, saga_export_name)) return spec.local_binding.slot;
        }
    }
    return null;
}

fn extractCall(deps: Deps, call_node: NodeIndex, call: ir_mod.Node.CallExpr) !SagaCallInfo {
    var info = SagaCallInfo{
        .source_line = nodeLine(deps, call_node),
        .source_column = nodeColumn(deps, call_node),
    };
    errdefer info.deinit(deps.allocator);

    if (call.args_count == 0) {
        info.dynamic = true;
        return info;
    }
    const arg_idx = deps.ir_view.getListIndex(call.args_start, 0);
    try collectSteps(deps, arg_idx, &info);
    return info;
}

/// Honor the dynamic-implies-empty invariant: free and clear any steps
/// collected so far before flagging the call as dynamic.
fn markDynamic(deps: Deps, info: *SagaCallInfo) void {
    for (info.steps.items) |*s| s.deinit(deps.allocator);
    info.steps.clearRetainingCapacity();
    info.dynamic = true;
}

fn collectSteps(deps: Deps, array_idx: NodeIndex, info: *SagaCallInfo) !void {
    const arr_tag = deps.ir_view.getTag(array_idx) orelse {
        markDynamic(deps, info);
        return;
    };
    if (arr_tag != .array_literal) {
        markDynamic(deps, info);
        return;
    }
    const arr = deps.ir_view.getArray(array_idx) orelse {
        markDynamic(deps, info);
        return;
    };
    if (arr.has_spread) {
        markDynamic(deps, info);
        return;
    }

    var i: u32 = 0;
    while (i < arr.elements_count) : (i += 1) {
        const elem_idx = deps.ir_view.getListIndex(arr.elements_start, @intCast(i));
        const elem_tag = deps.ir_view.getTag(elem_idx) orelse {
            markDynamic(deps, info);
            return;
        };
        if (elem_tag != .object_literal) {
            markDynamic(deps, info);
            return;
        }

        const step = parseStep(deps, elem_idx) catch |err| switch (err) {
            error.NotLiteral => {
                markDynamic(deps, info);
                return;
            },
            else => return err,
        };
        try info.steps.append(deps.allocator, step);
    }
}

const ParseError = error{ NotLiteral, OutOfMemory };

fn parseStep(deps: Deps, obj_idx: NodeIndex) ParseError!SagaStep {
    const obj = deps.ir_view.getObject(obj_idx) orelse return error.NotLiteral;

    var name_str: ?[]u8 = null;
    var has_run = false;
    var has_compensate = false;
    errdefer if (name_str) |s| deps.allocator.free(s);

    var p: u32 = 0;
    while (p < obj.properties_count) : (p += 1) {
        const prop_idx = deps.ir_view.getListIndex(obj.properties_start, @intCast(p));
        const prop_tag = deps.ir_view.getTag(prop_idx) orelse return error.NotLiteral;
        if (prop_tag != .object_property) return error.NotLiteral;
        const prop = deps.ir_view.getProperty(prop_idx) orelse return error.NotLiteral;
        if (prop.is_computed) return error.NotLiteral;

        const key = propKeyName(deps, prop.key) orelse return error.NotLiteral;

        if (std.mem.eql(u8, key, "name")) {
            if (name_str != null) return error.NotLiteral;
            const s = literalString(deps, prop.value) orelse return error.NotLiteral;
            name_str = try deps.allocator.dupe(u8, s);
        } else if (std.mem.eql(u8, key, "run")) {
            if (has_run) return error.NotLiteral;
            has_run = true;
        } else if (std.mem.eql(u8, key, "compensate")) {
            if (has_compensate) return error.NotLiteral;
            has_compensate = true;
        } else {
            return error.NotLiteral;
        }
    }

    const n = name_str orelse return error.NotLiteral;
    if (!has_run) {
        deps.allocator.free(n);
        return error.NotLiteral;
    }

    return .{ .name = n, .has_compensate = has_compensate };
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

fn nodeLine(deps: Deps, idx: NodeIndex) u32 {
    const loc = deps.ir_view.getLoc(idx) orelse return 0;
    return loc.line;
}

fn nodeColumn(deps: Deps, idx: NodeIndex) u32 {
    const loc = deps.ir_view.getLoc(idx) orelse return 0;
    return loc.column;
}

inline fn resolverCall(deps: Deps, atom_idx: u16) ?[]const u8 {
    return deps.resolver(atom_idx, deps.resolver_ctx);
}
