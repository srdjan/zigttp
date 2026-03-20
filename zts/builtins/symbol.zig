const std = @import("std");
const h = @import("helpers.zig");
const value = h.value;
const object = h.object;
const context = h.context;

const wrap = h.wrap;
const addMethodDynamic = h.addMethodDynamic;

// ============================================================================
// Symbol implementation
// ============================================================================

/// Global symbol counter for unique IDs
pub var symbol_counter: u32 = 100; // Start after well-known symbols

/// Global symbol registry for Symbol.for()
pub var symbol_registry: ?std.StringHashMap(value.JSValue) = null;

/// Create a new unique symbol
pub fn createSymbol(allocator: std.mem.Allocator, description: ?[]const u8) !value.JSValue {
    const symbol_box = try allocator.create(value.JSValue.SymbolBox);
    symbol_counter += 1;
    symbol_box.* = .{
        .header = (@as(u32, 8) << 1), // MemTag.symbol = 8
        .id = symbol_counter,
        .description_ptr = if (description) |d| d.ptr else null,
        .description_len = if (description) |d| @intCast(d.len) else 0,
    };
    return value.JSValue.fromExternPtr(symbol_box);
}

/// Create a well-known symbol
pub fn createWellKnownSymbol(allocator: std.mem.Allocator, which: value.JSValue.WellKnownSymbol, description: []const u8) !value.JSValue {
    const symbol_box = try allocator.create(value.JSValue.SymbolBox);
    symbol_box.* = .{
        .header = (@as(u32, 8) << 1), // MemTag.symbol = 8
        .id = @intFromEnum(which),
        .description_ptr = description.ptr,
        .description_len = @intCast(description.len),
    };
    return value.JSValue.fromExternPtr(symbol_box);
}

/// Symbol() - Create a new unique symbol
pub fn symbolConstructor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Symbol() must be called as a function, not with new
    const allocator = if (ctx.hybrid) |hybrid| hybrid.arena.allocator() else ctx.allocator;

    // Get optional description
    var description: ?[]const u8 = null;
    if (args.len > 0 and args[0].isString()) {
        description = ctx.getString(args[0]);
    }

    const symbol = createSymbol(allocator, description) catch return value.JSValue.undefined_val;
    return symbol;
}

/// Symbol.for(key) - Get or create a symbol in the global registry
pub fn symbolFor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.undefined_val;

    const key = ctx.getString(args[0]) orelse return value.JSValue.undefined_val;

    // Initialize registry if needed
    if (symbol_registry == null) {
        symbol_registry = std.StringHashMap(value.JSValue).init(ctx.allocator);
    }

    // Check if symbol exists
    if (symbol_registry.?.get(key)) |existing| {
        return existing;
    }

    // Create new symbol and register it
    const key_copy = if (ctx.hybrid != null)
        (ctx.allocator.dupe(u8, key) catch return value.JSValue.undefined_val)
    else
        key;
    const symbol = createSymbol(ctx.allocator, key_copy) catch return value.JSValue.undefined_val;
    symbol_registry.?.put(key_copy, symbol) catch return value.JSValue.undefined_val;
    return symbol;
}

/// Symbol.keyFor(sym) - Get the key for a registered symbol
pub fn symbolKeyFor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.undefined_val;

    const sym = args[0];
    if (!sym.isSymbol()) return value.JSValue.undefined_val;

    const sym_id = sym.getSymbolId();

    // Search registry for matching symbol
    if (symbol_registry) |*reg| {
        var iter = reg.iterator();
        while (iter.next()) |entry| {
            if (entry.value_ptr.*.getSymbolId() == sym_id) {
                return ctx.createString(entry.key_ptr.*) catch return value.JSValue.undefined_val;
            }
        }
    }

    return value.JSValue.undefined_val;
}

/// Symbol.prototype.toString() - Get symbol as string
pub fn symbolToString(ctx: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isSymbol()) return value.JSValue.undefined_val;

    const desc = this.getSymbolDescription();
    if (desc) |d| {
        // Return "Symbol(description)"
        var buf: [256]u8 = undefined;
        const result = std.fmt.bufPrint(&buf, "Symbol({s})", .{d}) catch return value.JSValue.undefined_val;
        return ctx.createString(result) catch return value.JSValue.undefined_val;
    }

    return ctx.createString("Symbol()") catch return value.JSValue.undefined_val;
}

/// Symbol.prototype.description getter
pub fn symbolDescription(ctx: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isSymbol()) return value.JSValue.undefined_val;

    const desc = this.getSymbolDescription();
    if (desc) |d| {
        return ctx.createString(d) catch return value.JSValue.undefined_val;
    }

    return value.JSValue.undefined_val;
}

/// Initialize Symbol built-in and well-known symbols
pub fn initSymbol(ctx: *context.Context) !void {
    const allocator = ctx.allocator;
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const root_class_idx = ctx.root_class_idx;

    // Create Symbol prototype
    const symbol_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, symbol_proto, "toString", wrap(symbolToString), 0);
    try addMethodDynamic(ctx, symbol_proto, "valueOf", wrap(symbolDescription), 0);

    // Create Symbol constructor
    const symbol_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(symbolConstructor), .Symbol, 1);
    try ctx.setPropertyChecked(symbol_ctor, .prototype, symbol_proto.toValue());

    // Add static methods
    try addMethodDynamic(ctx, symbol_ctor, "for", wrap(symbolFor), 1);
    try addMethodDynamic(ctx, symbol_ctor, "keyFor", wrap(symbolKeyFor), 1);

    // Add well-known symbols as static properties
    const iterator_sym = try createWellKnownSymbol(allocator, .iterator, "Symbol.iterator");
    const async_iterator_sym = try createWellKnownSymbol(allocator, .asyncIterator, "Symbol.asyncIterator");
    const to_string_tag_sym = try createWellKnownSymbol(allocator, .toStringTag, "Symbol.toStringTag");
    const to_primitive_sym = try createWellKnownSymbol(allocator, .toPrimitive, "Symbol.toPrimitive");
    const has_instance_sym = try createWellKnownSymbol(allocator, .hasInstance, "Symbol.hasInstance");

    const iterator_atom = try ctx.atoms.intern("iterator");
    const async_iterator_atom = try ctx.atoms.intern("asyncIterator");
    const to_string_tag_atom = try ctx.atoms.intern("toStringTag");
    const to_primitive_atom = try ctx.atoms.intern("toPrimitive");
    const has_instance_atom = try ctx.atoms.intern("hasInstance");

    try ctx.setPropertyChecked(symbol_ctor, iterator_atom, iterator_sym);
    try ctx.setPropertyChecked(symbol_ctor, async_iterator_atom, async_iterator_sym);
    try ctx.setPropertyChecked(symbol_ctor, to_string_tag_atom, to_string_tag_sym);
    try ctx.setPropertyChecked(symbol_ctor, to_primitive_atom, to_primitive_sym);
    try ctx.setPropertyChecked(symbol_ctor, has_instance_atom, has_instance_sym);

    try ctx.setGlobal(.Symbol, symbol_ctor.toValue());
}
