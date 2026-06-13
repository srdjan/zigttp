//! Walks the IR for a top-level `export const intent = { assertions: [...] }`
//! static literal and populates `HandlerContract.intent`. The result is
//! NOT a proof obligation - intent assertions live outside the proof
//! boundary and are exercised separately by `zigttp assert-intent`.
//!
//! ## Determinism guardrail
//!
//! Extraction is strict. The intent literal must be composed entirely of
//! JSON-compatible literal forms (string/int/float/bool/null, object
//! literal with literal keys/values, array literal with literal elements).
//! Any non-literal form (function, spread, identifier reference, computed
//! key, template, call, ternary, etc.) causes the extractor to set
//! `intent.dynamic = true` and clear the assertion list. The CLI runner
//! treats dynamic = true as a hard error rather than degrading to
//! "unknown", per the no-LLM-extraction line.
//!
//! ## Expected literal shape
//!
//! ```ts
//! export const intent = {
//!   assertions: [
//!     {
//!       name: "label",
//!       request: { method: "GET", path: "/health", body?: <json> },
//!       expect: { status?: 200, json?: <json>, headers?: { "x": "y" } }
//!     }
//!   ]
//! };
//! ```

const std = @import("std");
const ir_mod = @import("parser/ir.zig");
const handler_contract = @import("handler_contract.zig");
const object = @import("object.zig");

const IrView = ir_mod.IrView;
const NodeIndex = ir_mod.NodeIndex;
const IntentInfo = handler_contract.IntentInfo;
const IntentAssertion = handler_contract.IntentAssertion;
const IntentExpectedHeader = handler_contract.IntentExpectedHeader;

pub const AtomResolver = *const fn (atom_idx: u16, ctx: *const anyopaque) ?[]const u8;

pub const Deps = struct {
    allocator: std.mem.Allocator,
    ir_view: IrView,
    /// Caller-provided atom name resolver. Receives the atom slot index
    /// (BindingRef.slot) and returns the source-level identifier, or null
    /// if the atom is not user-declared. Used to find `intent` and to
    /// read object property keys written as identifiers.
    resolver: AtomResolver,
    resolver_ctx: *const anyopaque,
};

/// Extract `intent` if present. Returns `null` when no top-level
/// `intent` declaration exists. Returns a populated `IntentInfo` when
/// the declaration exists, with `dynamic = true` when the literal was
/// rejected. The caller owns the returned value and must call deinit.
pub fn extract(deps: Deps) !?IntentInfo {
    const node_count = deps.ir_view.nodeCount();
    var i: usize = 0;
    while (i < node_count) : (i += 1) {
        const idx: NodeIndex = @intCast(i);
        const tag = deps.ir_view.getTag(idx) orelse continue;
        if (tag != .var_decl) continue;

        const decl = deps.ir_view.getVarDecl(idx) orelse continue;
        if (decl.binding.kind != .global) continue;

        const name = resolverCall(deps, decl.binding.slot) orelse continue;
        if (!std.mem.eql(u8, name, "intent")) continue;

        return try extractFromInit(deps, decl.init);
    }
    return null;
}

fn extractFromInit(deps: Deps, init_idx: NodeIndex) !IntentInfo {
    var info = IntentInfo{};
    errdefer info.deinit(deps.allocator);

    const init_tag = deps.ir_view.getTag(init_idx) orelse {
        info.dynamic = true;
        return info;
    };
    if (init_tag != .object_literal) {
        info.dynamic = true;
        return info;
    }

    const obj = deps.ir_view.getObject(init_idx) orelse {
        info.dynamic = true;
        return info;
    };

    var p: u32 = 0;
    while (p < obj.properties_count) : (p += 1) {
        const prop_idx = deps.ir_view.getListIndex(obj.properties_start, @intCast(p));
        const prop_tag = deps.ir_view.getTag(prop_idx) orelse {
            info.dynamic = true;
            return info;
        };
        if (prop_tag != .object_property) {
            info.dynamic = true;
            return info;
        }

        const prop = deps.ir_view.getProperty(prop_idx) orelse {
            info.dynamic = true;
            return info;
        };
        if (prop.is_computed) {
            info.dynamic = true;
            return info;
        }

        const key = propKeyName(deps, prop.key) orelse {
            info.dynamic = true;
            return info;
        };
        if (!std.mem.eql(u8, key, "assertions")) {
            // Reject unknown sibling keys to keep the surface deterministic.
            info.dynamic = true;
            return info;
        }

        const arr_tag = deps.ir_view.getTag(prop.value) orelse {
            info.dynamic = true;
            return info;
        };
        if (arr_tag != .array_literal) {
            info.dynamic = true;
            return info;
        }

        try collectAssertions(deps, prop.value, &info);
        if (info.dynamic) return info;
    }

    return info;
}

/// Honor the documented invariant that `dynamic = true` implies an empty
/// assertion list: free and clear any assertions collected so far before
/// flagging the intent as dynamic. Without this, a literal array whose final
/// element is non-literal would leave the earlier (already-appended) assertions
/// in the list while also reporting dynamic, contradicting the contract and
/// leaking those assertions' owned strings if the caller trusts `dynamic`.
fn markDynamic(deps: Deps, info: *IntentInfo) void {
    for (info.assertions.items) |*a| a.deinit(deps.allocator);
    info.assertions.clearRetainingCapacity();
    info.dynamic = true;
}

fn collectAssertions(deps: Deps, array_idx: NodeIndex, info: *IntentInfo) !void {
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

        const assertion = parseAssertion(deps, elem_idx) catch |err| switch (err) {
            error.NotLiteral, error.MissingField => {
                markDynamic(deps, info);
                return;
            },
            else => return err,
        };
        try info.assertions.append(deps.allocator, assertion);
    }
}

const ParseError = error{ NotLiteral, MissingField, OutOfMemory };

fn parseAssertion(deps: Deps, obj_idx: NodeIndex) ParseError!IntentAssertion {
    const obj = deps.ir_view.getObject(obj_idx) orelse return error.NotLiteral;

    var name_str: ?[]u8 = null;
    var method_str: ?[]u8 = null;
    var path_str: ?[]u8 = null;
    var request_body: ?[]u8 = null;
    var expected_status: ?u16 = null;
    var expected_body: ?[]u8 = null;
    var headers = std.ArrayList(IntentExpectedHeader).empty;
    errdefer {
        if (name_str) |s| deps.allocator.free(s);
        if (method_str) |s| deps.allocator.free(s);
        if (path_str) |s| deps.allocator.free(s);
        if (request_body) |s| deps.allocator.free(s);
        if (expected_body) |s| deps.allocator.free(s);
        for (headers.items) |*h| h.deinit(deps.allocator);
        headers.deinit(deps.allocator);
    }

    var p: u32 = 0;
    while (p < obj.properties_count) : (p += 1) {
        const prop_idx = deps.ir_view.getListIndex(obj.properties_start, @intCast(p));
        const prop_tag = deps.ir_view.getTag(prop_idx) orelse return error.NotLiteral;
        if (prop_tag != .object_property) return error.NotLiteral;
        const prop = deps.ir_view.getProperty(prop_idx) orelse return error.NotLiteral;
        if (prop.is_computed) return error.NotLiteral;

        const key = propKeyName(deps, prop.key) orelse return error.NotLiteral;

        if (std.mem.eql(u8, key, "name")) {
            // A duplicate key would overwrite (and leak) the prior allocation;
            // the strict-literal surface rejects it instead.
            if (name_str != null) return error.NotLiteral;
            const s = literalString(deps, prop.value) orelse return error.NotLiteral;
            name_str = try deps.allocator.dupe(u8, s);
        } else if (std.mem.eql(u8, key, "request")) {
            try parseRequest(deps, prop.value, &method_str, &path_str, &request_body);
        } else if (std.mem.eql(u8, key, "expect")) {
            try parseExpect(deps, prop.value, &expected_status, &expected_body, &headers);
        } else {
            return error.NotLiteral;
        }
    }

    const n = name_str orelse return error.MissingField;
    const m = method_str orelse return error.MissingField;
    const pth = path_str orelse return error.MissingField;

    return .{
        .name = n,
        .method = m,
        .path = pth,
        .request_body_json = request_body,
        .expected_status = expected_status,
        .expected_body_json = expected_body,
        .expected_headers = headers,
        .source_line = nodeLine(deps, obj_idx),
        .source_column = nodeColumn(deps, obj_idx),
    };
}

fn parseRequest(
    deps: Deps,
    value_idx: NodeIndex,
    method_out: *?[]u8,
    path_out: *?[]u8,
    body_out: *?[]u8,
) ParseError!void {
    const tag = deps.ir_view.getTag(value_idx) orelse return error.NotLiteral;
    if (tag != .object_literal) return error.NotLiteral;
    const obj = deps.ir_view.getObject(value_idx) orelse return error.NotLiteral;

    var p: u32 = 0;
    while (p < obj.properties_count) : (p += 1) {
        const prop_idx = deps.ir_view.getListIndex(obj.properties_start, @intCast(p));
        if (deps.ir_view.getTag(prop_idx) != .object_property) return error.NotLiteral;
        const prop = deps.ir_view.getProperty(prop_idx) orelse return error.NotLiteral;
        if (prop.is_computed) return error.NotLiteral;
        const key = propKeyName(deps, prop.key) orelse return error.NotLiteral;

        // Duplicate keys would overwrite (and leak) the prior allocation; the
        // strict-literal surface rejects them instead.
        if (std.mem.eql(u8, key, "method")) {
            if (method_out.* != null) return error.NotLiteral;
            const s = literalString(deps, prop.value) orelse return error.NotLiteral;
            const upper = try deps.allocator.alloc(u8, s.len);
            _ = std.ascii.upperString(upper, s);
            method_out.* = upper;
        } else if (std.mem.eql(u8, key, "path")) {
            if (path_out.* != null) return error.NotLiteral;
            const s = literalString(deps, prop.value) orelse return error.NotLiteral;
            path_out.* = try deps.allocator.dupe(u8, s);
        } else if (std.mem.eql(u8, key, "body")) {
            if (body_out.* != null) return error.NotLiteral;
            body_out.* = try jsonSerializeLiteral(deps, prop.value);
        } else {
            return error.NotLiteral;
        }
    }
}

fn parseExpect(
    deps: Deps,
    value_idx: NodeIndex,
    status_out: *?u16,
    body_out: *?[]u8,
    headers_out: *std.ArrayList(IntentExpectedHeader),
) ParseError!void {
    const tag = deps.ir_view.getTag(value_idx) orelse return error.NotLiteral;
    if (tag != .object_literal) return error.NotLiteral;
    const obj = deps.ir_view.getObject(value_idx) orelse return error.NotLiteral;

    var headers_seen = false;
    var p: u32 = 0;
    while (p < obj.properties_count) : (p += 1) {
        const prop_idx = deps.ir_view.getListIndex(obj.properties_start, @intCast(p));
        if (deps.ir_view.getTag(prop_idx) != .object_property) return error.NotLiteral;
        const prop = deps.ir_view.getProperty(prop_idx) orelse return error.NotLiteral;
        if (prop.is_computed) return error.NotLiteral;
        const key = propKeyName(deps, prop.key) orelse return error.NotLiteral;

        // Duplicate keys are rejected: `json` would overwrite (and leak) the
        // prior allocation, and a repeated `headers` would silently double-fill.
        if (std.mem.eql(u8, key, "status")) {
            if (status_out.* != null) return error.NotLiteral;
            const n = literalInt(deps, prop.value) orelse return error.NotLiteral;
            if (n < 0 or n > 65535) return error.NotLiteral;
            status_out.* = @intCast(n);
        } else if (std.mem.eql(u8, key, "json")) {
            if (body_out.* != null) return error.NotLiteral;
            body_out.* = try jsonSerializeLiteral(deps, prop.value);
        } else if (std.mem.eql(u8, key, "headers")) {
            if (headers_seen) return error.NotLiteral;
            headers_seen = true;
            try parseHeaders(deps, prop.value, headers_out);
        } else {
            return error.NotLiteral;
        }
    }
}

fn parseHeaders(
    deps: Deps,
    value_idx: NodeIndex,
    headers_out: *std.ArrayList(IntentExpectedHeader),
) ParseError!void {
    const tag = deps.ir_view.getTag(value_idx) orelse return error.NotLiteral;
    if (tag != .object_literal) return error.NotLiteral;
    const obj = deps.ir_view.getObject(value_idx) orelse return error.NotLiteral;

    var p: u32 = 0;
    while (p < obj.properties_count) : (p += 1) {
        const prop_idx = deps.ir_view.getListIndex(obj.properties_start, @intCast(p));
        if (deps.ir_view.getTag(prop_idx) != .object_property) return error.NotLiteral;
        const prop = deps.ir_view.getProperty(prop_idx) orelse return error.NotLiteral;
        if (prop.is_computed) return error.NotLiteral;
        const key = propKeyName(deps, prop.key) orelse return error.NotLiteral;
        const val = literalString(deps, prop.value) orelse return error.NotLiteral;

        const name_dup = try deps.allocator.dupe(u8, key);
        errdefer deps.allocator.free(name_dup);
        const val_dup = try deps.allocator.dupe(u8, val);
        errdefer deps.allocator.free(val_dup);
        try headers_out.append(deps.allocator, .{ .name = name_dup, .value = val_dup });
    }
}

// ---------------------------------------------------------------------------
// Literal serializers and accessors
// ---------------------------------------------------------------------------

fn jsonSerializeLiteral(deps: Deps, idx: NodeIndex) ParseError![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(deps.allocator);
    try writeLiteralAsJson(deps, idx, &out);
    return out.toOwnedSlice(deps.allocator);
}

fn writeLiteralAsJson(deps: Deps, idx: NodeIndex, out: *std.ArrayList(u8)) ParseError!void {
    const tag = deps.ir_view.getTag(idx) orelse return error.NotLiteral;
    switch (tag) {
        .lit_string => {
            const s = literalString(deps, idx) orelse return error.NotLiteral;
            try out.append(deps.allocator, '"');
            for (s) |c| {
                switch (c) {
                    '"' => try out.appendSlice(deps.allocator, "\\\""),
                    '\\' => try out.appendSlice(deps.allocator, "\\\\"),
                    '\n' => try out.appendSlice(deps.allocator, "\\n"),
                    '\r' => try out.appendSlice(deps.allocator, "\\r"),
                    '\t' => try out.appendSlice(deps.allocator, "\\t"),
                    0x00...0x08, 0x0b...0x0c, 0x0e...0x1f => {
                        var esc: [6]u8 = undefined;
                        const written = std.fmt.bufPrint(&esc, "\\u{x:0>4}", .{@as(u16, c)}) catch unreachable;
                        try out.appendSlice(deps.allocator, written);
                    },
                    else => try out.append(deps.allocator, c),
                }
            }
            try out.append(deps.allocator, '"');
        },
        .lit_int => {
            const n = deps.ir_view.getIntValue(idx) orelse return error.NotLiteral;
            var buf: [16]u8 = undefined;
            const written = std.fmt.bufPrint(&buf, "{d}", .{n}) catch return error.NotLiteral;
            try out.appendSlice(deps.allocator, written);
        },
        .lit_float => {
            const f_idx = deps.ir_view.getFloatIdx(idx) orelse return error.NotLiteral;
            const f = deps.ir_view.getFloat(f_idx) orelse return error.NotLiteral;
            var buf: [32]u8 = undefined;
            const written = std.fmt.bufPrint(&buf, "{d}", .{f}) catch return error.NotLiteral;
            try out.appendSlice(deps.allocator, written);
        },
        .lit_bool => {
            const b = deps.ir_view.getBoolValue(idx) orelse return error.NotLiteral;
            try out.appendSlice(deps.allocator, if (b) "true" else "false");
        },
        .lit_null, .lit_undefined => {
            try out.appendSlice(deps.allocator, "null");
        },
        .array_literal => {
            const arr = deps.ir_view.getArray(idx) orelse return error.NotLiteral;
            if (arr.has_spread) return error.NotLiteral;
            try out.append(deps.allocator, '[');
            var i: u32 = 0;
            while (i < arr.elements_count) : (i += 1) {
                if (i > 0) try out.append(deps.allocator, ',');
                const e = deps.ir_view.getListIndex(arr.elements_start, @intCast(i));
                try writeLiteralAsJson(deps, e, out);
            }
            try out.append(deps.allocator, ']');
        },
        .object_literal => {
            const obj = deps.ir_view.getObject(idx) orelse return error.NotLiteral;
            try out.append(deps.allocator, '{');
            var p: u32 = 0;
            while (p < obj.properties_count) : (p += 1) {
                const prop_idx = deps.ir_view.getListIndex(obj.properties_start, @intCast(p));
                if (deps.ir_view.getTag(prop_idx) != .object_property) return error.NotLiteral;
                const prop = deps.ir_view.getProperty(prop_idx) orelse return error.NotLiteral;
                if (prop.is_computed) return error.NotLiteral;
                const key = propKeyName(deps, prop.key) orelse return error.NotLiteral;
                if (p > 0) try out.append(deps.allocator, ',');
                try out.append(deps.allocator, '"');
                for (key) |c| {
                    switch (c) {
                        '"' => try out.appendSlice(deps.allocator, "\\\""),
                        '\\' => try out.appendSlice(deps.allocator, "\\\\"),
                        else => try out.append(deps.allocator, c),
                    }
                }
                try out.appendSlice(deps.allocator, "\":");
                try writeLiteralAsJson(deps, prop.value, out);
            }
            try out.append(deps.allocator, '}');
        },
        else => return error.NotLiteral,
    }
}

fn literalString(deps: Deps, idx: NodeIndex) ?[]const u8 {
    const tag = deps.ir_view.getTag(idx) orelse return null;
    if (tag != .lit_string) return null;
    const str_idx = deps.ir_view.getStringIdx(idx) orelse return null;
    return deps.ir_view.getString(str_idx);
}

fn literalInt(deps: Deps, idx: NodeIndex) ?i32 {
    const tag = deps.ir_view.getTag(idx) orelse return null;
    if (tag != .lit_int) return null;
    return deps.ir_view.getIntValue(idx);
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
