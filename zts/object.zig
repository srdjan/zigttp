//! JavaScript objects with hidden classes and inline caches
//!
//! Fast property access via shape-based inline caching.

const std = @import("std");
const heap = @import("heap.zig");
const value = @import("value.zig");

/// Atom - interned property name identifier
pub const Atom = enum(u32) {
    // Predefined atoms (0-160)
    null = 0,
    true = 1,
    false = 2,
    undefined = 3,
    length = 4,
    prototype = 5,
    constructor = 6,
    toString = 7,
    valueOf = 8,
    name = 9,
    message = 10,
    arguments = 11,
    caller = 12,
    callee = 13,
    this = 14,
    @"return" = 15,
    throw = 16,
    @"try" = 17,
    @"catch" = 18,
    finally = 19,
    @"if" = 20,
    @"else" = 21,
    @"while" = 22,
    @"for" = 23,
    do = 24,
    @"switch" = 25,
    case = 26,
    default = 27,
    @"break" = 28,
    @"continue" = 29,
    @"var" = 30,
    let = 31,
    @"const" = 32,
    function = 33,
    new = 34,
    delete = 35,
    typeof = 36,
    void = 37,
    in = 38,
    instanceof = 39,
    // Object properties
    __proto__ = 40,
    hasOwnProperty = 41,
    isPrototypeOf = 42,
    propertyIsEnumerable = 43,
    toLocaleString = 44,
    // Array
    push = 45,
    pop = 46,
    shift = 47,
    unshift = 48,
    slice = 49,
    splice = 50,
    concat = 51,
    join = 52,
    reverse = 53,
    sort = 54,
    indexOf = 55,
    lastIndexOf = 56,
    forEach = 57,
    map = 58,
    filter = 59,
    reduce = 60,
    reduceRight = 61,
    every = 62,
    some = 63,
    find = 64,
    findIndex = 65,
    includes = 66,
    fill = 67,
    copyWithin = 68,
    entries = 69,
    keys = 70,
    values = 71,
    // String
    charAt = 72,
    charCodeAt = 73,
    codePointAt = 74,
    split = 75,
    substring = 76,
    substr = 77,
    toLowerCase = 78,
    toUpperCase = 79,
    trim = 80,
    trimStart = 81,
    trimEnd = 82,
    padStart = 83,
    padEnd = 84,
    repeat = 85,
    replace = 86,
    replaceAll = 87,
    match = 88,
    search = 89,
    startsWith = 90,
    endsWith = 91,
    // Function
    call = 92,
    apply = 93,
    bind = 94,
    // Math
    abs = 95,
    floor = 96,
    ceil = 97,
    round = 98,
    min = 99,
    max = 100,
    pow = 101,
    sqrt = 102,
    random = 103,
    sin = 104,
    cos = 105,
    tan = 106,
    log = 107,
    exp = 108,
    // JSON
    parse = 109,
    stringify = 110,
    // Symbol
    iterator = 111,
    toStringTag = 112,
    // Promise
    then = 113,
    catch_method = 114,
    finally_method = 115,
    resolve = 116,
    reject = 117,
    all = 118,
    race = 119,
    // Error types
    Error = 120,
    TypeError = 121,
    RangeError = 122,
    SyntaxError = 123,
    ReferenceError = 124,
    URIError = 125,
    EvalError = 126,
    // Global
    console = 127,
    globalThis = 128,
    Infinity = 129,
    NaN_atom = 130,
    Object = 131,
    Array = 132,
    String = 133,
    Number = 134,
    Boolean = 135,
    Function = 136,
    Symbol = 137,
    Date = 138,
    RegExp = 139,
    Math = 140,
    JSON = 141,
    Promise = 142,
    Proxy = 143,
    Reflect = 144,
    Map = 145,
    Set = 146,
    WeakMap = 147,
    WeakSet = 148,
    ArrayBuffer = 149,
    DataView = 150,
    Int8Array = 151,
    Uint8Array = 152,
    Int16Array = 153,
    Uint16Array = 154,
    Int32Array = 155,
    Uint32Array = 156,
    Float32Array = 157,
    Float64Array = 158,
    handler = 159, // Used for handler function lookup
    Response = 160, // Response object for HTTP handlers
    text = 161,
    html = 162,
    json = 163,
    body = 164,
    status = 165,
    headers = 166,
    method = 167,
    url = 168,
    // JSX runtime atoms
    h = 169, // h(tag, props, ...children)
    renderToString = 170, // renderToString(node)
    Fragment = 171, // Fragment constant
    tag = 172, // Virtual DOM tag property
    props = 173, // Virtual DOM props property
    children = 174, // Virtual DOM children property
    // Common HTML attributes for JSX
    class = 175, // class attribute
    className = 176, // React-style class
    id = 177,
    style = 178,
    type = 179,
    value = 180,
    href = 181,
    src = 182,
    alt = 183,
    placeholder = 184,
    disabled = 185,
    checked = 186,
    required = 187,
    autocomplete = 188,
    // HTMX attributes
    @"hx-get" = 189,
    @"hx-post" = 190,
    @"hx-put" = 191,
    @"hx-delete" = 192,
    @"hx-target" = 193,
    @"hx-swap" = 194,
    @"hx-trigger" = 195,
    @"hx-on--after-request" = 196,
    // Result type atoms
    Result = 197,
    ok = 198,
    err = 199,
    isOk = 200,
    isErr = 201,
    unwrap = 202,
    unwrapOr = 203,
    unwrapErr = 204,
    mapErr = 205,
    // JSON methods
    tryParse = 206,
    // Reserved for more builtins
    __count__ = 220,

    // Dynamic atoms start at 221
    _,

    pub const FIRST_DYNAMIC: u32 = 221;

    /// Check if atom is a predefined (static) atom
    pub fn isPredefined(self: Atom) bool {
        return @intFromEnum(self) < FIRST_DYNAMIC;
    }

    /// Get the string name for a predefined atom
    pub fn toPredefinedName(self: Atom) ?[]const u8 {
        return switch (self) {
            .null => "null",
            .true => "true",
            .false => "false",
            .undefined => "undefined",
            .length => "length",
            .prototype => "prototype",
            .constructor => "constructor",
            .toString => "toString",
            .valueOf => "valueOf",
            .name => "name",
            .message => "message",
            .push => "push",
            .pop => "pop",
            .shift => "shift",
            .unshift => "unshift",
            .slice => "slice",
            .splice => "splice",
            .concat => "concat",
            .join => "join",
            .indexOf => "indexOf",
            .handler => "handler",
            .Response => "Response",
            .text => "text",
            .html => "html",
            .json => "json",
            .body => "body",
            .status => "status",
            .headers => "headers",
            .method => "method",
            .url => "url",
            .h => "h",
            .renderToString => "renderToString",
            .Fragment => "Fragment",
            .tag => "tag",
            .props => "props",
            .children => "children",
            // HTML attributes
            .class => "class",
            .className => "className",
            .id => "id",
            .style => "style",
            .type => "type",
            .value => "value",
            .href => "href",
            .src => "src",
            .alt => "alt",
            .placeholder => "placeholder",
            .disabled => "disabled",
            .checked => "checked",
            .required => "required",
            .autocomplete => "autocomplete",
            // HTMX
            .@"hx-get" => "hx-get",
            .@"hx-post" => "hx-post",
            .@"hx-put" => "hx-put",
            .@"hx-delete" => "hx-delete",
            .@"hx-target" => "hx-target",
            .@"hx-swap" => "hx-swap",
            .@"hx-trigger" => "hx-trigger",
            .@"hx-on--after-request" => "hx-on--after-request",
            // Result type
            .Result => "Result",
            .ok => "ok",
            .err => "err",
            .isOk => "isOk",
            .isErr => "isErr",
            .unwrap => "unwrap",
            .unwrapOr => "unwrapOr",
            .unwrapErr => "unwrapErr",
            .mapErr => "mapErr",
            // JSON methods
            .tryParse => "tryParse",
            else => null,
        };
    }
};

/// Compile-time string map for O(1) predefined atom lookup
const predefined_atom_map = std.StaticStringMap(Atom).initComptime(.{
    // Common property names
    .{ "length", .length },
    .{ "prototype", .prototype },
    .{ "constructor", .constructor },
    .{ "toString", .toString },
    .{ "valueOf", .valueOf },
    .{ "name", .name },
    .{ "message", .message },
    .{ "arguments", .arguments },
    .{ "caller", .caller },
    .{ "callee", .callee },
    // Object properties
    .{ "__proto__", .__proto__ },
    .{ "hasOwnProperty", .hasOwnProperty },
    .{ "isPrototypeOf", .isPrototypeOf },
    .{ "propertyIsEnumerable", .propertyIsEnumerable },
    .{ "toLocaleString", .toLocaleString },
    // Array methods
    .{ "push", .push },
    .{ "pop", .pop },
    .{ "shift", .shift },
    .{ "unshift", .unshift },
    .{ "slice", .slice },
    .{ "splice", .splice },
    .{ "concat", .concat },
    .{ "join", .join },
    .{ "reverse", .reverse },
    .{ "sort", .sort },
    .{ "indexOf", .indexOf },
    .{ "lastIndexOf", .lastIndexOf },
    .{ "forEach", .forEach },
    .{ "map", .map },
    .{ "filter", .filter },
    .{ "reduce", .reduce },
    .{ "reduceRight", .reduceRight },
    .{ "every", .every },
    .{ "some", .some },
    .{ "find", .find },
    .{ "findIndex", .findIndex },
    .{ "includes", .includes },
    .{ "fill", .fill },
    .{ "copyWithin", .copyWithin },
    .{ "entries", .entries },
    .{ "keys", .keys },
    .{ "values", .values },
    // String methods
    .{ "charAt", .charAt },
    .{ "charCodeAt", .charCodeAt },
    .{ "codePointAt", .codePointAt },
    .{ "split", .split },
    .{ "substring", .substring },
    .{ "substr", .substr },
    .{ "toLowerCase", .toLowerCase },
    .{ "toUpperCase", .toUpperCase },
    .{ "trim", .trim },
    .{ "trimStart", .trimStart },
    .{ "trimEnd", .trimEnd },
    .{ "padStart", .padStart },
    .{ "padEnd", .padEnd },
    .{ "repeat", .repeat },
    .{ "replace", .replace },
    .{ "replaceAll", .replaceAll },
    .{ "match", .match },
    .{ "search", .search },
    .{ "startsWith", .startsWith },
    .{ "endsWith", .endsWith },
    // Function methods
    .{ "call", .call },
    .{ "apply", .apply },
    .{ "bind", .bind },
    // Math methods
    .{ "abs", .abs },
    .{ "floor", .floor },
    .{ "ceil", .ceil },
    .{ "round", .round },
    .{ "min", .min },
    .{ "max", .max },
    .{ "pow", .pow },
    .{ "sqrt", .sqrt },
    .{ "random", .random },
    .{ "sin", .sin },
    .{ "cos", .cos },
    .{ "tan", .tan },
    .{ "log", .log },
    .{ "exp", .exp },
    // JSON
    .{ "parse", .parse },
    .{ "stringify", .stringify },
    // Symbol
    .{ "iterator", .iterator },
    .{ "toStringTag", .toStringTag },
    // Promise
    .{ "then", .then },
    .{ "resolve", .resolve },
    .{ "reject", .reject },
    .{ "all", .all },
    .{ "race", .race },
    // Error types
    .{ "Error", .Error },
    .{ "TypeError", .TypeError },
    .{ "RangeError", .RangeError },
    .{ "SyntaxError", .SyntaxError },
    .{ "ReferenceError", .ReferenceError },
    .{ "URIError", .URIError },
    .{ "EvalError", .EvalError },
    // Globals
    .{ "console", .console },
    .{ "globalThis", .globalThis },
    .{ "Infinity", .Infinity },
    .{ "NaN", .NaN_atom },
    .{ "Object", .Object },
    .{ "Array", .Array },
    .{ "String", .String },
    .{ "Number", .Number },
    .{ "Boolean", .Boolean },
    .{ "Function", .Function },
    .{ "Symbol", .Symbol },
    .{ "Date", .Date },
    .{ "RegExp", .RegExp },
    .{ "Math", .Math },
    .{ "JSON", .JSON },
    .{ "Promise", .Promise },
    .{ "Proxy", .Proxy },
    .{ "Reflect", .Reflect },
    .{ "Map", .Map },
    .{ "Set", .Set },
    .{ "WeakMap", .WeakMap },
    .{ "WeakSet", .WeakSet },
    .{ "ArrayBuffer", .ArrayBuffer },
    .{ "DataView", .DataView },
    .{ "Int8Array", .Int8Array },
    .{ "Uint8Array", .Uint8Array },
    .{ "Int16Array", .Int16Array },
    .{ "Uint16Array", .Uint16Array },
    .{ "Int32Array", .Int32Array },
    .{ "Uint32Array", .Uint32Array },
    .{ "Float32Array", .Float32Array },
    .{ "Float64Array", .Float64Array },
    // HTTP/Response
    .{ "handler", .handler },
    .{ "Response", .Response },
    .{ "text", .text },
    .{ "html", .html },
    .{ "json", .json },
    .{ "body", .body },
    .{ "status", .status },
    .{ "headers", .headers },
    .{ "method", .method },
    .{ "url", .url },
    // JSX runtime
    .{ "h", .h },
    .{ "renderToString", .renderToString },
    .{ "Fragment", .Fragment },
    .{ "tag", .tag },
    .{ "props", .props },
    .{ "children", .children },
    // HTML attributes for JSX
    .{ "class", .class },
    .{ "className", .className },
    .{ "id", .id },
    .{ "style", .style },
    .{ "type", .type },
    .{ "value", .value },
    .{ "href", .href },
    .{ "src", .src },
    .{ "alt", .alt },
    .{ "placeholder", .placeholder },
    .{ "disabled", .disabled },
    .{ "checked", .checked },
    .{ "required", .required },
    .{ "autocomplete", .autocomplete },
    // HTMX attributes
    .{ "hx-get", .@"hx-get" },
    .{ "hx-post", .@"hx-post" },
    .{ "hx-put", .@"hx-put" },
    .{ "hx-delete", .@"hx-delete" },
    .{ "hx-target", .@"hx-target" },
    .{ "hx-swap", .@"hx-swap" },
    .{ "hx-trigger", .@"hx-trigger" },
    .{ "hx-on--after-request", .@"hx-on--after-request" },
    // Result type
    .{ "Result", .Result },
    .{ "ok", .ok },
    .{ "err", .err },
    .{ "isOk", .isOk },
    .{ "isErr", .isErr },
    .{ "unwrap", .unwrap },
    .{ "unwrapOr", .unwrapOr },
    .{ "unwrapErr", .unwrapErr },
    .{ "mapErr", .mapErr },
    // JSON methods
    .{ "tryParse", .tryParse },
});

/// Lookup predefined atom by name - O(1) using compile-time hash map
pub fn lookupPredefinedAtom(name: []const u8) ?Atom {
    return predefined_atom_map.get(name);
}

/// Native function signature
/// ctx: execution context, this: this value, args: argument slice
/// Returns: result value or error
pub const NativeFn = *const fn (ctx: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue;

/// Native function data stored in function objects
pub const NativeFunctionData = struct {
    func: NativeFn,
    name: Atom,
    arg_count: u8, // Expected argument count (0 = variadic)
};

/// JS bytecode function data stored in function objects
pub const BytecodeFunctionData = struct {
    bytecode: *const @import("bytecode.zig").FunctionBytecode,
    name: Atom,
};

/// Upvalue - captures a variable from an enclosing scope
/// Can be "open" (pointing to a stack slot) or "closed" (value copied to heap)
pub const Upvalue = struct {
    /// When open, points to a stack location
    /// When closed, contains the value directly
    location: union(enum) {
        open: *value.JSValue, // Points to stack slot
        closed: value.JSValue, // Value copied here when closed
    },
    /// Next upvalue in the linked list of open upvalues (for closing on scope exit)
    next: ?*Upvalue,

    pub fn init(slot: *value.JSValue) Upvalue {
        return .{
            .location = .{ .open = slot },
            .next = null,
        };
    }

    pub fn get(self: *const Upvalue) value.JSValue {
        return switch (self.location) {
            .open => |ptr| ptr.*,
            .closed => |val| val,
        };
    }

    pub fn set(self: *Upvalue, val: value.JSValue) void {
        switch (self.location) {
            .open => |ptr| ptr.* = val,
            .closed => self.location = .{ .closed = val },
        }
    }

    /// Close the upvalue - copy the value and stop referencing the stack
    pub fn close(self: *Upvalue) void {
        switch (self.location) {
            .open => |ptr| {
                self.location = .{ .closed = ptr.* };
            },
            .closed => {}, // Already closed
        }
    }
};

/// Closure data - a function with captured upvalues
pub const ClosureData = struct {
    bytecode: *const @import("bytecode.zig").FunctionBytecode,
    name: Atom,
    upvalues: []*Upvalue, // Array of pointers to upvalues

    pub fn deinit(self: *ClosureData, allocator: std.mem.Allocator) void {
        allocator.free(self.upvalues);
    }
};

/// Generator state - stores suspended execution state
pub const GeneratorState = enum(u8) {
    suspended_start, // Initial state, not yet started
    suspended_yield, // Suspended at yield
    executing, // Currently running
    completed, // Done (returned or threw)
};

/// Generator data stored in generator objects
pub const GeneratorData = struct {
    bytecode: *const @import("bytecode.zig").FunctionBytecode,
    state: GeneratorState,
    pc_offset: u32, // Current PC offset within bytecode
    locals: []value.JSValue, // Local variables
    stack: []value.JSValue, // Saved stack values
    stack_len: u32, // Number of values on stack
};

/// Property descriptor flags
pub const PropertyFlags = packed struct {
    writable: bool = true,
    enumerable: bool = true,
    configurable: bool = true,
    is_accessor: bool = false,
    _reserved: u4 = 0,
};

/// Property slot in hidden class
pub const PropertySlot = struct {
    name: Atom,
    offset: u16,
    flags: PropertyFlags,
};

// ============================================================================
// Index-Based Hidden Class System (Zig Compiler Pattern)
// ============================================================================
//
// STATUS: Infrastructure ready, not yet integrated with JSObject
//
// This index-based system provides better memory efficiency than pointer-based:
// - 4 bytes per reference vs 8 bytes (50% savings)
// - Structure-of-Arrays (SoA) layout for cache-friendly property lookups
// - Suitable for serialization/caching
//
// MIGRATION PATH (not yet implemented):
// 1. Change JSObject.hidden_class from *HiddenClass to HiddenClassIndex
// 2. Add pool reference to JSObject or pass through method parameters
// 3. Update InlineCacheEntry to use HiddenClassIndex
// 4. Remove legacy HiddenClass struct after migration complete
//
// The legacy pointer-based HiddenClass (below) is currently used by JSObject
// and the interpreter's inline cache.
// ============================================================================

/// Index into HiddenClassPool - uses u32 for 50% memory savings vs pointers
pub const HiddenClassIndex = enum(u32) {
    /// Empty/root hidden class (no properties)
    empty = 0,
    /// Sentinel for null/invalid
    none = std.math.maxInt(u32),
    /// Dynamic indices
    _,

    pub fn isNone(self: HiddenClassIndex) bool {
        return self == .none;
    }

    pub fn toInt(self: HiddenClassIndex) u32 {
        return @intFromEnum(self);
    }

    pub fn fromInt(val: u32) HiddenClassIndex {
        return @enumFromInt(val);
    }
};

/// Pooled hidden class storage using MultiArrayList pattern
/// All hidden classes share a single pool for memory efficiency and serialization
pub const HiddenClassPool = struct {
    allocator: std.mem.Allocator,

    /// Core data stored as structure-of-arrays for cache efficiency
    /// Each array index corresponds to a HiddenClassIndex
    property_counts: std.ArrayListUnmanaged(u16),
    properties_starts: std.ArrayListUnmanaged(u32),
    prototype_indices: std.ArrayListUnmanaged(HiddenClassIndex),

    /// Shared property storage using true structure-of-arrays (Phase 2 optimization)
    /// 7 bytes per property vs 8 bytes in AoS format (12.5% memory savings)
    /// Better cache locality for name lookups (most common operation)
    property_names: std.ArrayListUnmanaged(Atom),
    property_offsets: std.ArrayListUnmanaged(u16),
    property_flags: std.ArrayListUnmanaged(PropertyFlags),

    /// Transition table: flat array with entries [from_class: u32, atom: u32, to_class: u32]
    /// Looked up via linear scan (small number of transitions per class)
    transitions: std.ArrayListUnmanaged(u32),

    /// Number of allocated classes
    count: u32,

    const TRANSITION_ENTRY_SIZE = 3; // from, atom, to

    pub fn init(allocator: std.mem.Allocator) !*HiddenClassPool {
        const pool = try allocator.create(HiddenClassPool);
        errdefer allocator.destroy(pool);

        pool.* = .{
            .allocator = allocator,
            .property_counts = .empty,
            .properties_starts = .empty,
            .prototype_indices = .empty,
            .property_names = .empty,
            .property_offsets = .empty,
            .property_flags = .empty,
            .transitions = .empty,
            .count = 0,
        };

        // Allocate the empty root class at index 0
        _ = try pool.allocClass(0, .none);

        return pool;
    }

    pub fn deinit(self: *HiddenClassPool) void {
        self.property_counts.deinit(self.allocator);
        self.properties_starts.deinit(self.allocator);
        self.prototype_indices.deinit(self.allocator);
        self.property_names.deinit(self.allocator);
        self.property_offsets.deinit(self.allocator);
        self.property_flags.deinit(self.allocator);
        self.transitions.deinit(self.allocator);
        self.allocator.destroy(self);
    }

    /// Allocate a new hidden class with given property count
    fn allocClass(self: *HiddenClassPool, prop_count: u16, prototype: HiddenClassIndex) !HiddenClassIndex {
        const idx = self.count;
        self.count += 1;

        try self.property_counts.append(self.allocator, prop_count);
        try self.properties_starts.append(self.allocator, @intCast(self.property_names.items.len));
        try self.prototype_indices.append(self.allocator, prototype);

        return HiddenClassIndex.fromInt(idx);
    }

    /// Get property count for a class
    pub fn getPropertyCount(self: *const HiddenClassPool, idx: HiddenClassIndex) u16 {
        if (idx.isNone()) return 0;
        const i = idx.toInt();
        if (i >= self.count) return 0;
        return self.property_counts.items[i];
    }

    /// Get prototype index for a class
    pub fn getPrototype(self: *const HiddenClassPool, idx: HiddenClassIndex) HiddenClassIndex {
        if (idx.isNone()) return .none;
        const i = idx.toInt();
        if (i >= self.count) return .none;
        return self.prototype_indices.items[i];
    }

    /// Find property by name in a class, returns slot offset or null
    /// Uses SoA layout for cache-efficient name comparison
    pub fn findProperty(self: *const HiddenClassPool, idx: HiddenClassIndex, name: Atom) ?u16 {
        if (idx.isNone()) return null;
        const i = idx.toInt();
        if (i >= self.count) return null;

        const prop_count = self.property_counts.items[i];
        if (prop_count == 0) return null;

        const start = self.properties_starts.items[i];
        const names = self.property_names.items[start..][0..prop_count];

        // Linear scan over contiguous name array (cache-friendly)
        for (names, 0..) |n, slot_idx| {
            if (n == name) {
                return self.property_offsets.items[start + slot_idx];
            }
        }
        return null;
    }

    /// Get or create transition to new class with added property
    pub fn addProperty(self: *HiddenClassPool, from_idx: HiddenClassIndex, name: Atom) !HiddenClassIndex {
        const from = from_idx.toInt();

        // Check for existing transition
        const trans = self.transitions.items;
        var i: usize = 0;
        while (i + TRANSITION_ENTRY_SIZE <= trans.len) : (i += TRANSITION_ENTRY_SIZE) {
            if (trans[i] == from and trans[i + 1] == @intFromEnum(name)) {
                return HiddenClassIndex.fromInt(trans[i + 2]);
            }
        }

        // Create new class
        const old_prop_count = self.getPropertyCount(from_idx);
        const new_prop_count = old_prop_count + 1;
        const prototype = self.getPrototype(from_idx);

        const new_idx = try self.allocClass(new_prop_count, prototype);

        // Copy old properties to new location (SoA format)
        if (old_prop_count > 0) {
            const old_start = self.properties_starts.items[from];
            try self.property_names.appendSlice(self.allocator, self.property_names.items[old_start..][0..old_prop_count]);
            try self.property_offsets.appendSlice(self.allocator, self.property_offsets.items[old_start..][0..old_prop_count]);
            try self.property_flags.appendSlice(self.allocator, self.property_flags.items[old_start..][0..old_prop_count]);
        }

        // Add new property using SoA arrays
        try self.property_names.append(self.allocator, name);
        try self.property_offsets.append(self.allocator, old_prop_count); // offset = old count
        try self.property_flags.append(self.allocator, .{}); // default flags

        // Record transition
        try self.transitions.append(self.allocator, from);
        try self.transitions.append(self.allocator, @intFromEnum(name));
        try self.transitions.append(self.allocator, new_idx.toInt());

        return new_idx;
    }

    /// Get empty root class index
    pub fn getEmptyClass(self: *const HiddenClassPool) HiddenClassIndex {
        _ = self;
        return .empty;
    }

    /// Get property flags by name
    pub fn getPropertyFlags(self: *const HiddenClassPool, idx: HiddenClassIndex, name: Atom) ?PropertyFlags {
        if (idx.isNone()) return null;
        const i = idx.toInt();
        if (i >= self.count) return null;

        const prop_count = self.property_counts.items[i];
        if (prop_count == 0) return null;

        const start = self.properties_starts.items[i];
        const names = self.property_names.items[start..][0..prop_count];

        for (names, 0..) |n, slot_idx| {
            if (n == name) {
                return self.property_flags.items[start + slot_idx];
            }
        }
        return null;
    }

    /// Iterator for property names in a class
    pub fn propertyNames(self: *const HiddenClassPool, idx: HiddenClassIndex) []const Atom {
        if (idx.isNone()) return &.{};
        const i = idx.toInt();
        if (i >= self.count) return &.{};

        const prop_count = self.property_counts.items[i];
        if (prop_count == 0) return &.{};

        const start = self.properties_starts.items[i];
        return self.property_names.items[start..][0..prop_count];
    }
};

// ============================================================================
// Pointer-Based Hidden Class (Currently Active)
// ============================================================================
//
// This is the active hidden class implementation used by JSObject.
// It uses pointer-based transitions and is integrated with the inline cache.
//
// Pros: Simple, works with existing JSObject design, fast pointer comparison
// Cons: 8 bytes per reference, not suitable for serialization
//
// See HiddenClassPool above for the index-based alternative.
// ============================================================================

/// Hidden class (shape) for objects - pointer-based implementation
/// Used by JSObject for property storage layout and inline caching
pub const HiddenClass = struct {
    properties: []const PropertySlot,
    transitions: TransitionMap,
    prototype: ?*HiddenClass,
    property_count: u16,

    const TransitionMap = std.AutoHashMap(Atom, *HiddenClass);

    pub fn init(allocator: std.mem.Allocator) !*HiddenClass {
        const class = try allocator.create(HiddenClass);
        class.* = .{
            .properties = &.{},
            .transitions = TransitionMap.init(allocator),
            .prototype = null,
            .property_count = 0,
        };
        return class;
    }

    pub fn deinit(self: *HiddenClass, allocator: std.mem.Allocator) void {
        self.transitions.deinit();
        allocator.destroy(self);
    }

    /// Clear all cached transitions to free memory
    /// Call during GC or context reset to prevent memory leaks
    pub fn clearTransitions(self: *HiddenClass, allocator: std.mem.Allocator) void {
        // Free all transitioned hidden classes recursively
        var it = self.transitions.valueIterator();
        while (it.next()) |child_class| {
            // Recursively clear children's transitions first
            child_class.*.clearTransitions(allocator);
            // Free child's properties array if allocated
            if (child_class.*.properties.len > 0) {
                allocator.free(child_class.*.properties);
            }
            // Free the child class itself
            allocator.destroy(child_class.*);
        }
        // Clear our transitions map (releases hash map memory)
        self.transitions.clearAndFree();
    }

    /// Full cleanup including self - use when destroying entire class hierarchy
    pub fn deinitRecursive(self: *HiddenClass, allocator: std.mem.Allocator) void {
        // Clear all child transitions first (this frees child classes and clears the map)
        self.clearTransitions(allocator);
        // Free our own properties if allocated
        if (self.properties.len > 0) {
            allocator.free(self.properties);
        }
        // Free self (transitions map already cleared by clearTransitions via clearAndFree)
        allocator.destroy(self);
    }

    /// Get or create transition to new shape with added property
    pub fn addProperty(self: *HiddenClass, allocator: std.mem.Allocator, name: Atom) !*HiddenClass {
        // Check for existing transition
        if (self.transitions.get(name)) |existing| {
            return existing;
        }

        // Create new hidden class
        const new_class = try allocator.create(HiddenClass);
        errdefer allocator.destroy(new_class);

        // Copy properties and add new one
        var new_props = try allocator.alloc(PropertySlot, self.property_count + 1);
        @memcpy(new_props[0..self.property_count], self.properties);
        new_props[self.property_count] = .{
            .name = name,
            .offset = self.property_count,
            .flags = .{},
        };

        new_class.* = .{
            .properties = new_props,
            .transitions = TransitionMap.init(allocator),
            .prototype = self.prototype,
            .property_count = self.property_count + 1,
        };

        // Cache transition
        try self.transitions.put(name, new_class);

        return new_class;
    }

    /// Find property slot by name
    pub fn findProperty(self: *HiddenClass, name: Atom) ?*const PropertySlot {
        for (self.properties) |*slot| {
            if (slot.name == name) return slot;
        }
        return null;
    }

    /// Get or create a hidden class for functions with 2 reserved slots
    /// This prevents property additions from overwriting internal function data
    /// stored in inline_slots[0] and inline_slots[1]
    pub fn getOrCreateFunctionClass(self: *HiddenClass, allocator: std.mem.Allocator) !*HiddenClass {
        // Use special atom for function class transition (slot 0 reserved)
        const reserved0: Atom = @enumFromInt(0xFFFE);
        const reserved1: Atom = @enumFromInt(0xFFFF);

        // Check for cached transition
        if (self.transitions.get(reserved0)) |existing| {
            return existing;
        }

        // Create function class with 2 reserved slots
        const func_class = try allocator.create(HiddenClass);
        errdefer allocator.destroy(func_class);

        // Allocate properties array with 2 reserved entries
        var props = try allocator.alloc(PropertySlot, 2);
        props[0] = .{ .name = reserved0, .offset = 0, .flags = .{} };
        props[1] = .{ .name = reserved1, .offset = 1, .flags = .{} };

        func_class.* = .{
            .properties = props,
            .property_count = 2, // Start new properties at offset 2
            .prototype = self,
            .transitions = TransitionMap.init(allocator),
        };

        // Cache this transition
        try self.transitions.put(reserved0, func_class);

        return func_class;
    }
};

/// Object class ID for built-in types
pub const ClassId = enum(u8) {
    object = 0,
    array = 1,
    function = 2,
    bound_function = 3,
    generator = 4,
    @"error" = 5,
    array_buffer = 6,
    typed_array = 7,
    data_view = 8,
    promise = 9,
    map = 10,
    set = 11,
    weak_map = 12,
    weak_set = 13,
    regexp = 14,
    date = 15,
    proxy = 16,
    string_object = 17,
    number_object = 18,
    boolean_object = 19,
    symbol_object = 20,
    arguments = 21,
    result = 22, // Result type for functional error handling
    range_iterator = 23, // Lazy range iterator for for...of
    // Custom class IDs start here
    _,

    pub const FIRST_CUSTOM: u8 = 64;
};

/// JavaScript object
/// Uses extern struct for predictable C-compatible memory layout
pub const JSObject = extern struct {
    header: heap.MemBlockHeader,
    hidden_class: *HiddenClass,
    prototype: ?*JSObject,
    class_id: ClassId,
    flags: ObjectFlags,
    inline_slots: [INLINE_SLOT_COUNT]value.JSValue,
    overflow_slots: ?[*]value.JSValue,
    overflow_capacity: u16,

    pub const INLINE_SLOT_COUNT = 8;

    /// Inline slot indices with semantic meaning
    /// Different object types use slots for different purposes:
    pub const Slots = struct {
        /// Function objects: data pointer (NativeFunctionData/BytecodeFunctionData/ClosureData)
        pub const FUNC_DATA: usize = 0;
        /// Function objects: true_val for bytecode function, undefined for native
        pub const FUNC_IS_BYTECODE: usize = 1;
        /// Closure objects: true_val marker to identify closures
        pub const FUNC_IS_CLOSURE: usize = 4;

        /// Array objects: length as integer
        pub const ARRAY_LENGTH: usize = 0;
        /// Array objects: elements start at index 1
        pub const ARRAY_ELEMENTS_START: usize = 1;

        /// Result objects: isOk boolean (true = ok, false = err)
        pub const RESULT_IS_OK: usize = 0;
        /// Result objects: the value (either ok value or error)
        pub const RESULT_VALUE: usize = 1;

        /// WeakMap/WeakSet: internal hash map data pointer
        pub const WEAK_COLLECTION_DATA: usize = 0;

        /// Generator objects: generator state data pointer
        pub const GENERATOR_DATA: usize = 0;

        /// Range iterator: start value (i32 stored as JSValue)
        pub const RANGE_START: usize = 0;
        /// Range iterator: end value (i32 stored as JSValue)
        pub const RANGE_END: usize = 1;
        /// Range iterator: step value (i32 stored as JSValue)
        pub const RANGE_STEP: usize = 2;
        /// Range iterator: cached length (computed once at creation)
        pub const RANGE_LENGTH: usize = 3;
    };

    pub const ObjectFlags = packed struct {
        extensible: bool = true,
        is_exotic: bool = false, // Array, TypedArray, etc.
        is_callable: bool = false,
        is_constructor: bool = false,
        has_small_array: bool = false, // Dense array optimization
        is_generator: bool = false, // Generator function (function*)
        is_async: bool = false, // Async function (async function)
        _reserved: u1 = 0,
    };

    /// Create a new object
    pub fn create(allocator: std.mem.Allocator, class: *HiddenClass, prototype: ?*JSObject) !*JSObject {
        const obj = try allocator.create(JSObject);
        obj.* = .{
            .header = heap.MemBlockHeader.init(.object, @sizeOf(JSObject)),
            .hidden_class = class,
            .prototype = prototype,
            .class_id = .object,
            .flags = .{},
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
        };
        return obj;
    }

    /// Destroy object
    pub fn destroy(self: *JSObject, allocator: std.mem.Allocator) void {
        if (self.overflow_slots) |slots| {
            allocator.free(slots[0..self.overflow_capacity]);
        }
        allocator.destroy(self);
    }

    /// Create a native function object
    pub fn createNativeFunction(allocator: std.mem.Allocator, class: *HiddenClass, func: NativeFn, name: Atom, arg_count: u8) !*JSObject {
        // Allocate native function data
        const data = try allocator.create(NativeFunctionData);
        errdefer allocator.destroy(data);
        data.* = .{
            .func = func,
            .name = name,
            .arg_count = arg_count,
        };

        // Create a hidden class that reserves slots 0-1 for internal function data
        // This prevents setProperty from overwriting the native function data
        const func_class = try class.getOrCreateFunctionClass(allocator);

        // Create function object
        const obj = try allocator.create(JSObject);
        obj.* = .{
            .header = heap.MemBlockHeader.init(.object, @sizeOf(JSObject)),
            .hidden_class = func_class,
            .prototype = null,
            .class_id = .function,
            .flags = .{ .is_callable = true },
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
        };
        // Store native function data pointer
        obj.inline_slots[Slots.FUNC_DATA] = value.JSValue.fromPtr(data);
        return obj;
    }

    /// Get native function data (if this is a native function)
    pub fn getNativeFunctionData(self: *const JSObject) ?*NativeFunctionData {
        if (self.class_id != .function or !self.flags.is_callable) return null;
        const slot = self.inline_slots[Slots.FUNC_DATA];
        if (!slot.isPtr()) return null;
        // Native functions have undefined in FUNC_IS_BYTECODE slot
        if (!self.inline_slots[Slots.FUNC_IS_BYTECODE].isUndefined()) return null;
        return slot.toPtr(NativeFunctionData);
    }

    /// Create a JS bytecode function object
    /// Copies is_generator and is_async flags from bytecode for efficient call-time checks
    pub fn createBytecodeFunction(allocator: std.mem.Allocator, class: *HiddenClass, bytecode_ptr: *const @import("bytecode.zig").FunctionBytecode, name: Atom) !*JSObject {
        // Allocate bytecode function data
        const data = try allocator.create(BytecodeFunctionData);
        errdefer allocator.destroy(data);
        data.* = .{
            .bytecode = bytecode_ptr,
            .name = name,
        };

        // Create function object with flags copied from bytecode
        const obj = try allocator.create(JSObject);
        obj.* = .{
            .header = heap.MemBlockHeader.init(.object, @sizeOf(JSObject)),
            .hidden_class = class,
            .prototype = null,
            .class_id = .function,
            .flags = .{
                .is_callable = true,
                .is_generator = bytecode_ptr.flags.is_generator,
                .is_async = bytecode_ptr.flags.is_async,
            },
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
        };
        // Store bytecode function data and marker
        obj.inline_slots[Slots.FUNC_DATA] = value.JSValue.fromPtr(data);
        obj.inline_slots[Slots.FUNC_IS_BYTECODE] = value.JSValue.true_val;
        return obj;
    }

    /// Get bytecode function data (if this is a bytecode function)
    pub fn getBytecodeFunctionData(self: *const JSObject) ?*BytecodeFunctionData {
        if (self.class_id != .function or !self.flags.is_callable) {
            std.log.debug("getBytecodeFunc fail: class_id={} is_callable={}", .{ @intFromEnum(self.class_id), self.flags.is_callable });
            return null;
        }
        const slot = self.inline_slots[Slots.FUNC_DATA];
        if (!slot.isPtr()) {
            std.log.debug("getBytecodeFunc fail: FUNC_DATA not ptr, raw={x}", .{slot.raw});
            return null;
        }
        // Bytecode functions have non-undefined in FUNC_IS_BYTECODE slot
        if (self.inline_slots[Slots.FUNC_IS_BYTECODE].isUndefined()) {
            std.log.debug("getBytecodeFunc fail: FUNC_IS_BYTECODE is undefined", .{});
            return null;
        }
        std.log.debug("getBytecodeFunc success!", .{});
        return slot.toPtr(BytecodeFunctionData);
    }

    /// Create a closure (function with captured upvalues)
    /// Copies is_generator and is_async flags from bytecode for efficient call-time checks
    pub fn createClosure(
        allocator: std.mem.Allocator,
        class: *HiddenClass,
        bytecode_ptr: *const @import("bytecode.zig").FunctionBytecode,
        name: Atom,
        upvalues: []*Upvalue,
    ) !*JSObject {
        // Allocate closure data
        const data = try allocator.create(ClosureData);
        errdefer allocator.destroy(data);
        data.* = .{
            .bytecode = bytecode_ptr,
            .name = name,
            .upvalues = upvalues,
        };

        // Create function object with flags copied from bytecode
        const obj = try allocator.create(JSObject);
        obj.* = .{
            .header = heap.MemBlockHeader.init(.object, @sizeOf(JSObject)),
            .hidden_class = class,
            .prototype = null,
            .class_id = .function,
            .flags = .{
                .is_callable = true,
                .is_generator = bytecode_ptr.flags.is_generator,
                .is_async = bytecode_ptr.flags.is_async,
            },
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
        };
        // Store closure data and markers
        obj.inline_slots[Slots.FUNC_DATA] = value.JSValue.fromPtr(data);
        obj.inline_slots[Slots.FUNC_IS_BYTECODE] = value.JSValue.true_val;
        obj.inline_slots[Slots.FUNC_IS_CLOSURE] = value.JSValue.true_val;
        return obj;
    }

    /// Get closure data (if this is a closure)
    pub fn getClosureData(self: *const JSObject) ?*ClosureData {
        if (self.class_id != .function or !self.flags.is_callable) return null;
        // Check if it's a closure
        if (!self.inline_slots[Slots.FUNC_IS_CLOSURE].isTrue()) return null;
        const slot = self.inline_slots[Slots.FUNC_DATA];
        if (!slot.isPtr()) return null;
        return slot.toPtr(ClosureData);
    }

    /// Create a generator object from bytecode function
    pub fn createGenerator(allocator: std.mem.Allocator, class: *HiddenClass, bytecode_ptr: *const @import("bytecode.zig").FunctionBytecode, prototype: ?*JSObject) !*JSObject {
        // Allocate generator data
        const data = try allocator.create(GeneratorData);
        errdefer allocator.destroy(data);

        // Allocate space for locals
        const locals = try allocator.alloc(value.JSValue, bytecode_ptr.local_count);
        errdefer allocator.free(locals);
        @memset(locals, value.JSValue.undefined_val);

        // Allocate initial stack space
        const stack = try allocator.alloc(value.JSValue, 64);
        errdefer allocator.free(stack);

        data.* = .{
            .bytecode = bytecode_ptr,
            .state = .suspended_start,
            .pc_offset = 0,
            .locals = locals,
            .stack = stack,
            .stack_len = 0,
        };

        // Create generator object
        const obj = try allocator.create(JSObject);
        obj.* = .{
            .header = heap.MemBlockHeader.init(.object, @sizeOf(JSObject)),
            .hidden_class = class,
            .prototype = prototype,
            .class_id = .generator,
            .flags = .{},
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
        };
        // Store generator data pointer
        obj.inline_slots[Slots.GENERATOR_DATA] = value.JSValue.fromPtr(data);
        return obj;
    }

    /// Get generator data (if this is a generator object)
    pub fn getGeneratorData(self: *const JSObject) ?*GeneratorData {
        if (self.class_id != .generator) return null;
        const slot = self.inline_slots[Slots.GENERATOR_DATA];
        if (!slot.isPtr()) return null;
        return slot.toPtr(GeneratorData);
    }

    /// Fast property access by slot offset
    pub inline fn getSlot(self: *const JSObject, slot: u16) value.JSValue {
        if (slot < INLINE_SLOT_COUNT) {
            return self.inline_slots[slot];
        }
        if (self.overflow_slots) |slots| {
            return slots[slot - INLINE_SLOT_COUNT];
        }
        return value.JSValue.undefined_val;
    }

    /// Fast property store by slot offset
    pub inline fn setSlot(self: *JSObject, slot: u16, val: value.JSValue) void {
        if (slot < INLINE_SLOT_COUNT) {
            self.inline_slots[slot] = val;
        } else if (self.overflow_slots) |slots| {
            slots[slot - INLINE_SLOT_COUNT] = val;
        }
    }

    /// Ultra-fast property access for inline caching (no bounds check)
    pub inline fn getPropertyFast(self: *const JSObject, slot: u16) value.JSValue {
        return self.getSlot(slot);
    }

    /// Ultra-fast property store for inline caching
    pub inline fn setPropertyFast(self: *JSObject, slot: u16, val: value.JSValue) void {
        self.setSlot(slot, val);
    }

    /// Get property by name (with prototype chain lookup)
    pub fn getProperty(self: *const JSObject, name: Atom) ?value.JSValue {
        if (self.class_id == .array and name == .length) {
            return self.inline_slots[Slots.ARRAY_LENGTH];
        }
        // Range iterator: compute length on demand
        if (self.class_id == .range_iterator and name == .length) {
            return value.JSValue.fromInt(@intCast(self.getRangeLength()));
        }
        // Check own properties first
        if (self.hidden_class.findProperty(name)) |slot| {
            return self.getSlot(slot.offset);
        }

        // Walk prototype chain
        var proto = self.prototype;
        while (proto) |p| {
            if (p.hidden_class.findProperty(name)) |slot| {
                return p.getSlot(slot.offset);
            }
            proto = p.prototype;
        }

        return null;
    }

    /// Get own property (no prototype lookup)
    pub fn getOwnProperty(self: *const JSObject, name: Atom) ?value.JSValue {
        if (self.class_id == .array and name == .length) {
            return self.inline_slots[Slots.ARRAY_LENGTH];
        }
        // Range iterator: compute length on demand
        if (self.class_id == .range_iterator and name == .length) {
            return value.JSValue.fromInt(@intCast(self.getRangeLength()));
        }
        if (self.hidden_class.findProperty(name)) |slot| {
            return self.getSlot(slot.offset);
        }
        return null;
    }

    /// Set property (with hidden class transition)
    pub fn setProperty(self: *JSObject, allocator: std.mem.Allocator, name: Atom, val: value.JSValue) !void {
        if (self.class_id == .array and name == .length) {
            if (val.isInt()) {
                const len = val.getInt();
                if (len >= 0) {
                    self.setArrayLength(@intCast(len));
                }
            }
            return;
        }
        // Check if property exists
        if (self.hidden_class.findProperty(name)) |slot| {
            self.setSlot(slot.offset, val);
            return;
        }

        // Check extensibility
        if (!self.flags.extensible) {
            return; // Silently fail in non-strict mode
        }

        // Transition to new hidden class
        const new_class = try self.hidden_class.addProperty(allocator, name);
        const new_slot = new_class.property_count - 1;

        // Ensure we have space
        if (new_slot >= INLINE_SLOT_COUNT) {
            try self.ensureOverflowCapacity(allocator, new_slot - INLINE_SLOT_COUNT + 1);
        }

        self.hidden_class = new_class;
        self.setSlot(new_slot, val);
    }

    /// Ensure overflow slots have enough capacity
    fn ensureOverflowCapacity(self: *JSObject, allocator: std.mem.Allocator, min_capacity: u16) !void {
        if (self.overflow_capacity >= min_capacity) return;

        const new_capacity = @max(min_capacity, self.overflow_capacity * 2, 4);
        const new_slots = try allocator.alloc(value.JSValue, new_capacity);

        // Copy existing
        if (self.overflow_slots) |old_slots| {
            @memcpy(new_slots[0..self.overflow_capacity], old_slots[0..self.overflow_capacity]);
            allocator.free(old_slots[0..self.overflow_capacity]);
        }

        // Initialize new slots
        for (self.overflow_capacity..new_capacity) |i| {
            new_slots[i] = value.JSValue.undefined_val;
        }

        self.overflow_slots = new_slots.ptr;
        self.overflow_capacity = new_capacity;
    }

    /// Check if object has own property
    pub fn hasOwnProperty(self: *const JSObject, name: Atom) bool {
        return self.hidden_class.findProperty(name) != null;
    }

    /// Check if property exists (including prototype chain)
    pub fn hasProperty(self: *const JSObject, name: Atom) bool {
        return self.getProperty(name) != null;
    }

    /// Delete property
    pub fn deleteProperty(self: *JSObject, name: Atom) bool {
        if (self.class_id == .array and name == .length) return false;
        if (self.hidden_class.findProperty(name)) |slot| {
            if (!slot.flags.configurable) return false;
            // Note: In a full implementation, we'd transition to a new hidden class
            // For now, just set to undefined
            self.setSlot(slot.offset, value.JSValue.undefined_val);
            return true;
        }
        return true; // Deleting non-existent property succeeds
    }

    /// Prevent extensions
    pub fn preventExtensions(self: *JSObject) void {
        self.flags.extensible = false;
    }

    /// Check if object is extensible
    pub fn isExtensible(self: *const JSObject) bool {
        return self.flags.extensible;
    }

    /// Get all own enumerable property names
    pub fn getOwnEnumerableKeys(self: *const JSObject, allocator: std.mem.Allocator) ![]Atom {
        var keys = std.ArrayList(Atom).empty;
        errdefer keys.deinit(allocator);

        for (self.hidden_class.properties) |prop| {
            if (prop.flags.enumerable) {
                try keys.append(allocator, prop.name);
            }
        }

        return keys.toOwnedSlice(allocator);
    }

    /// Convert to JSValue
    pub fn toValue(self: *JSObject) value.JSValue {
        return value.JSValue.fromPtr(self);
    }

    /// Get from JSValue (unsafe - caller must verify isObject)
    pub fn fromValue(val: value.JSValue) *JSObject {
        return val.toPtr(JSObject);
    }

    // ========================================================================
    // Array Methods
    // ========================================================================

    /// Create an array object
    pub fn createArray(allocator: std.mem.Allocator, class: *HiddenClass) !*JSObject {
        const obj = try allocator.create(JSObject);
        obj.* = .{
            .header = heap.MemBlockHeader.init(.object, @sizeOf(JSObject)),
            .hidden_class = class,
            .prototype = null,
            .class_id = .array,
            .flags = .{ .is_exotic = true, .has_small_array = true },
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
        };
        // Initialize array length to 0
        obj.inline_slots[Slots.ARRAY_LENGTH] = value.JSValue.fromInt(0);
        return obj;
    }

    /// Create a lazy range iterator object
    /// Unlike createArray, this doesn't pre-allocate elements - values are computed on access
    pub fn createRangeIterator(allocator: std.mem.Allocator, class: *HiddenClass, start: i32, end: i32, step: i32) !*JSObject {
        // Compute length once at creation (avoid division in hot loop)
        const length: u32 = blk: {
            if (step == 0) break :blk 0;
            if (step > 0) {
                if (end <= start) break :blk 0;
                break :blk @intCast(@divTrunc(end - start + step - 1, step));
            } else {
                if (start <= end) break :blk 0;
                break :blk @intCast(@divTrunc(start - end - step - 1, -step));
            }
        };

        const obj = try allocator.create(JSObject);
        obj.* = .{
            .header = heap.MemBlockHeader.init(.object, @sizeOf(JSObject)),
            .hidden_class = class,
            .prototype = null,
            .class_id = .range_iterator,
            .flags = .{ .is_exotic = true },
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
        };
        // Store range parameters and cached length
        obj.inline_slots[Slots.RANGE_START] = value.JSValue.fromInt(start);
        obj.inline_slots[Slots.RANGE_END] = value.JSValue.fromInt(end);
        obj.inline_slots[Slots.RANGE_STEP] = value.JSValue.fromInt(step);
        obj.inline_slots[Slots.RANGE_LENGTH] = value.JSValue.fromInt(@intCast(length));
        return obj;
    }

    /// Get range iterator length (cached at creation time)
    pub inline fn getRangeLength(self: *const JSObject) u32 {
        if (self.class_id != .range_iterator) return 0;
        return @intCast(self.inline_slots[Slots.RANGE_LENGTH].getInt());
    }

    /// Get range element at index (computed, not stored)
    pub fn getRangeIndex(self: *const JSObject, index: u32) ?value.JSValue {
        if (self.class_id != .range_iterator) return null;
        const len = self.getRangeLength();
        if (index >= len) return null;
        const start = self.inline_slots[Slots.RANGE_START].getInt();
        const step = self.inline_slots[Slots.RANGE_STEP].getInt();
        const val = start + @as(i32, @intCast(index)) * step;
        return value.JSValue.fromInt(val);
    }

    /// Check if this is an array
    pub fn isArray(self: *const JSObject) bool {
        return self.class_id == .array;
    }

    /// Get array length (for arrays)
    pub fn getArrayLength(self: *const JSObject) u32 {
        if (self.class_id != .array) return 0;
        const len_val = self.inline_slots[Slots.ARRAY_LENGTH];
        if (len_val.isInt()) {
            const len = len_val.getInt();
            return if (len >= 0) @intCast(len) else 0;
        }
        return 0;
    }

    /// Set array length
    pub fn setArrayLength(self: *JSObject, len: u32) void {
        if (self.class_id != .array) return;
        self.inline_slots[Slots.ARRAY_LENGTH] = value.JSValue.fromInt(@intCast(len));
    }

    /// Get element at index (for arrays)
    /// Array elements are stored starting at inline_slots[1]
    pub fn getIndex(self: *const JSObject, index: u32) ?value.JSValue {
        if (self.class_id != .array) return null;
        const len = self.getArrayLength();
        if (index >= len) return null;

        // Element storage starts at slot 1 (slot 0 is length)
        const slot = index + 1;
        if (slot < INLINE_SLOT_COUNT) {
            const val = self.inline_slots[slot];
            if (val.isUndefined()) return null;
            return val;
        }

        // Check overflow slots
        const overflow_idx = slot - INLINE_SLOT_COUNT;
        if (self.overflow_slots) |slots| {
            if (overflow_idx < self.overflow_capacity) {
                const val = slots[overflow_idx];
                if (val.isUndefined()) return null;
                return val;
            }
        }
        return null;
    }

    /// Get array element without bounds check - caller must ensure array type and valid index
    pub inline fn getIndexUnchecked(self: *const JSObject, index: u32) value.JSValue {
        const slot = index + 1;
        if (slot < INLINE_SLOT_COUNT) {
            return self.inline_slots[slot];
        }
        return self.overflow_slots.?[slot - INLINE_SLOT_COUNT];
    }

    /// Set element at index (for arrays)
    pub fn setIndex(self: *JSObject, allocator: std.mem.Allocator, index: u32, val: value.JSValue) !void {
        if (self.class_id != .array) return;

        // Update length if needed
        const current_len = self.getArrayLength();
        if (index >= current_len) {
            self.setArrayLength(index + 1);
        }

        // Element storage starts at slot 1
        const slot = index + 1;
        if (slot < INLINE_SLOT_COUNT) {
            self.inline_slots[slot] = val;
            return;
        }

        // Need overflow slots
        const overflow_idx: u16 = @intCast(slot - INLINE_SLOT_COUNT);
        try self.ensureOverflowCapacity(allocator, overflow_idx + 1);
        self.overflow_slots.?[overflow_idx] = val;
    }

    /// Push element to end of array
    pub fn arrayPush(self: *JSObject, allocator: std.mem.Allocator, val: value.JSValue) !void {
        if (self.class_id != .array) return;
        const len = self.getArrayLength();
        try self.setIndex(allocator, len, val);
    }

    // ========================================================================
    // Property Iterator
    // ========================================================================

    pub const PropertyEntry = struct {
        atom: Atom,
        value: value.JSValue,
    };

    pub const PropertyIterator = struct {
        obj: *const JSObject,
        index: usize,

        pub fn next(self: *PropertyIterator) ?PropertyEntry {
            const props = self.obj.hidden_class.properties;
            if (self.index >= props.len) return null;

            const slot = props[self.index];
            const val = self.obj.getSlot(slot.offset);
            self.index += 1;

            return .{
                .atom = slot.name,
                .value = val,
            };
        }
    };

    /// Get property iterator
    pub fn propertyIterator(self: *const JSObject) PropertyIterator {
        return .{ .obj = self, .index = 0 };
    }
};

/// Inline cache for property access
pub const InlineCache = struct {
    cached_class: ?*HiddenClass = null,
    cached_slot: u16 = 0,
    hit_count: u32 = 0,
    miss_count: u32 = 0,

    /// Try fast property lookup via cached shape
    pub fn get(self: *InlineCache, obj: *JSObject) ?value.JSValue {
        if (obj.hidden_class == self.cached_class) {
            self.hit_count += 1;
            return obj.getPropertyFast(self.cached_slot);
        }
        self.miss_count += 1;
        return null;
    }

    /// Update cache after miss
    pub fn update(self: *InlineCache, class: *HiddenClass, slot: u16) void {
        self.cached_class = class;
        self.cached_slot = slot;
    }

    /// Cache hit ratio (for profiling)
    pub fn hitRatio(self: *InlineCache) f64 {
        const total = self.hit_count + self.miss_count;
        if (total == 0) return 0.0;
        return @as(f64, @floatFromInt(self.hit_count)) / @as(f64, @floatFromInt(total));
    }
};

// ============================================================================
// Tests
// ============================================================================

test "Atom predefined check" {
    try std.testing.expect(Atom.length.isPredefined());
    try std.testing.expect(Atom.prototype.isPredefined());
    try std.testing.expect(!(@as(Atom, @enumFromInt(Atom.FIRST_DYNAMIC)).isPredefined()));
}

test "HiddenClass property transitions" {
    const allocator = std.testing.allocator;

    var root = try HiddenClass.init(allocator);
    defer root.deinit(allocator);

    // Add 'length' property
    var class_x = try root.addProperty(allocator, .length);
    defer {
        allocator.free(class_x.properties);
        class_x.deinit(allocator);
    }

    try std.testing.expectEqual(@as(u16, 1), class_x.property_count);

    // Same transition should return cached class
    const class_x2 = try root.addProperty(allocator, .length);
    try std.testing.expectEqual(class_x, class_x2);
}

test "HiddenClass multiple properties" {
    const allocator = std.testing.allocator;

    var root = try HiddenClass.init(allocator);
    defer root.deinit(allocator);

    // Add 'x' then 'y'
    var class_x = try root.addProperty(allocator, .length);
    defer {
        allocator.free(class_x.properties);
        class_x.deinit(allocator);
    }

    var class_xy = try class_x.addProperty(allocator, .prototype);
    defer {
        allocator.free(class_xy.properties);
        class_xy.deinit(allocator);
    }

    try std.testing.expectEqual(@as(u16, 2), class_xy.property_count);

    // Find properties
    try std.testing.expect(class_xy.findProperty(.length) != null);
    try std.testing.expect(class_xy.findProperty(.prototype) != null);
    try std.testing.expect(class_xy.findProperty(.constructor) == null);
}

test "InlineCache hit tracking" {
    var ic = InlineCache{};
    try std.testing.expectEqual(@as(f64, 0.0), ic.hitRatio());

    ic.hit_count = 9;
    ic.miss_count = 1;
    try std.testing.expectEqual(@as(f64, 0.9), ic.hitRatio());
}

test "JSObject creation and property access" {
    const allocator = std.testing.allocator;

    var root_class = try HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var obj = try JSObject.create(allocator, root_class, null);
    defer obj.destroy(allocator);

    // Initially no properties
    try std.testing.expect(obj.getOwnProperty(.length) == null);

    // Set property
    try obj.setProperty(allocator, .length, value.JSValue.fromInt(42));
    defer allocator.free(obj.hidden_class.properties);
    defer obj.hidden_class.deinit(allocator);

    // Get property
    const val = obj.getOwnProperty(.length);
    try std.testing.expect(val != null);
    try std.testing.expectEqual(@as(i32, 42), val.?.getInt());
}

test "JSObject prototype chain lookup" {
    const allocator = std.testing.allocator;

    // Create prototype
    var proto_class = try HiddenClass.init(allocator);
    defer proto_class.deinit(allocator);

    var proto = try JSObject.create(allocator, proto_class, null);
    defer proto.destroy(allocator);

    try proto.setProperty(allocator, .toString, value.JSValue.fromInt(1));
    defer allocator.free(proto.hidden_class.properties);
    defer proto.hidden_class.deinit(allocator);

    // Create child object with prototype
    var child_class = try HiddenClass.init(allocator);
    defer child_class.deinit(allocator);

    var child = try JSObject.create(allocator, child_class, proto);
    defer child.destroy(allocator);

    // Child can access prototype property
    const val = child.getProperty(.toString);
    try std.testing.expect(val != null);
    try std.testing.expectEqual(@as(i32, 1), val.?.getInt());

    // But not as own property
    try std.testing.expect(child.getOwnProperty(.toString) == null);
}

test "JSObject hasProperty" {
    const allocator = std.testing.allocator;

    var root_class = try HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var obj = try JSObject.create(allocator, root_class, null);
    defer obj.destroy(allocator);

    try std.testing.expect(!obj.hasOwnProperty(.length));
    try std.testing.expect(!obj.hasProperty(.length));

    try obj.setProperty(allocator, .length, value.JSValue.fromInt(5));
    defer allocator.free(obj.hidden_class.properties);
    defer obj.hidden_class.deinit(allocator);

    try std.testing.expect(obj.hasOwnProperty(.length));
    try std.testing.expect(obj.hasProperty(.length));
}

test "JSObject extensibility" {
    const allocator = std.testing.allocator;

    var root_class = try HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var obj = try JSObject.create(allocator, root_class, null);
    defer obj.destroy(allocator);

    try std.testing.expect(obj.isExtensible());

    obj.preventExtensions();
    try std.testing.expect(!obj.isExtensible());

    // Can't add new properties
    try obj.setProperty(allocator, .length, value.JSValue.fromInt(10));
    try std.testing.expect(obj.getOwnProperty(.length) == null);
}

test "JSObject to/from value" {
    const allocator = std.testing.allocator;

    var root_class = try HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var obj = try JSObject.create(allocator, root_class, null);
    defer obj.destroy(allocator);

    const js_val = obj.toValue();
    try std.testing.expect(js_val.isPtr());

    const recovered = JSObject.fromValue(js_val);
    try std.testing.expectEqual(obj, recovered);
}

test "InlineCache with object" {
    const allocator = std.testing.allocator;

    var root_class = try HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var obj = try JSObject.create(allocator, root_class, null);
    defer obj.destroy(allocator);

    try obj.setProperty(allocator, .length, value.JSValue.fromInt(100));
    defer allocator.free(obj.hidden_class.properties);
    defer obj.hidden_class.deinit(allocator);

    var ic = InlineCache{};

    // First access - cache miss
    const miss_result = ic.get(obj);
    try std.testing.expect(miss_result == null);
    try std.testing.expectEqual(@as(u32, 1), ic.miss_count);

    // Update cache
    if (obj.hidden_class.findProperty(.length)) |slot| {
        ic.update(obj.hidden_class, slot.offset);
    }

    // Second access - cache hit
    const hit_result = ic.get(obj);
    try std.testing.expect(hit_result != null);
    try std.testing.expectEqual(@as(i32, 100), hit_result.?.getInt());
    try std.testing.expectEqual(@as(u32, 1), ic.hit_count);
}

test "lookupPredefinedAtom" {
    const length_atom = lookupPredefinedAtom("length");
    try std.testing.expect(length_atom != null);
    try std.testing.expectEqual(Atom.length, length_atom.?);

    const prototype_atom = lookupPredefinedAtom("prototype");
    try std.testing.expect(prototype_atom != null);
    try std.testing.expectEqual(Atom.prototype, prototype_atom.?);

    const unknown_atom = lookupPredefinedAtom("unknownAtomName");
    try std.testing.expect(unknown_atom == null);
}

test "Upvalue operations" {
    var slot: value.JSValue = value.JSValue.fromInt(42);
    var uv = Upvalue.init(&slot);

    // Get initial value
    try std.testing.expectEqual(@as(i32, 42), uv.get().getInt());

    // Set new value
    uv.set(value.JSValue.fromInt(100));
    try std.testing.expectEqual(@as(i32, 100), uv.get().getInt());

    // Close the upvalue
    uv.close();
    // Check that value is preserved after closing
    try std.testing.expectEqual(@as(i32, 100), uv.get().getInt());
}

test "JSObject deleteProperty" {
    const allocator = std.testing.allocator;

    var root_class = try HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var obj = try JSObject.create(allocator, root_class, null);
    defer obj.destroy(allocator);

    try obj.setProperty(allocator, .length, value.JSValue.fromInt(10));
    defer allocator.free(obj.hidden_class.properties);
    defer obj.hidden_class.deinit(allocator);

    // Property exists
    try std.testing.expect(obj.hasOwnProperty(.length));

    // Delete property - returns true
    const deleted = obj.deleteProperty(.length);
    try std.testing.expect(deleted);

    // Deleting non-existent property also returns true (JS spec behavior)
    const deleted_again = obj.deleteProperty(.prototype);
    try std.testing.expect(deleted_again);
}

test "JSObject hasOwnProperty" {
    const allocator = std.testing.allocator;

    var root_class = try HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var obj = try JSObject.create(allocator, root_class, null);
    defer obj.destroy(allocator);

    // Initially no property
    try std.testing.expect(!obj.hasOwnProperty(.length));

    try obj.setProperty(allocator, .length, value.JSValue.fromInt(5));
    defer allocator.free(obj.hidden_class.properties);
    defer obj.hidden_class.deinit(allocator);

    try std.testing.expect(obj.hasOwnProperty(.length));
}

test "JSObject array operations" {
    const allocator = std.testing.allocator;

    var root_class = try HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var arr = try JSObject.createArray(allocator, root_class);
    defer arr.destroy(allocator);

    // Check it's an array
    try std.testing.expect(arr.isArray());

    // Initial length is 0
    try std.testing.expectEqual(@as(u32, 0), arr.getArrayLength());

    // Set length
    arr.setArrayLength(5);
    try std.testing.expectEqual(@as(u32, 5), arr.getArrayLength());
}

test "JSObject getIndex and setIndex" {
    const allocator = std.testing.allocator;

    var root_class = try HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var arr = try JSObject.createArray(allocator, root_class);
    defer arr.destroy(allocator);

    // Initially no element at index 0
    try std.testing.expect(arr.getIndex(0) == null);

    // Set element
    try arr.setIndex(allocator, 0, value.JSValue.fromInt(42));
    const val = arr.getIndex(0);
    try std.testing.expect(val != null);
    try std.testing.expectEqual(@as(i32, 42), val.?.getInt());
}

test "JSObject arrayPush" {
    const allocator = std.testing.allocator;

    var root_class = try HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var arr = try JSObject.createArray(allocator, root_class);
    defer arr.destroy(allocator);

    try std.testing.expectEqual(@as(u32, 0), arr.getArrayLength());

    try arr.arrayPush(allocator, value.JSValue.fromInt(1));
    try std.testing.expectEqual(@as(u32, 1), arr.getArrayLength());
    try std.testing.expectEqual(@as(i32, 1), arr.getIndex(0).?.getInt());

    try arr.arrayPush(allocator, value.JSValue.fromInt(2));
    try std.testing.expectEqual(@as(u32, 2), arr.getArrayLength());
    try std.testing.expectEqual(@as(i32, 2), arr.getIndex(1).?.getInt());
}

test "JSObject getOwnEnumerableKeys" {
    const allocator = std.testing.allocator;

    var root_class = try HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var obj = try JSObject.create(allocator, root_class, null);
    defer obj.destroy(allocator);

    try obj.setProperty(allocator, .length, value.JSValue.fromInt(10));
    defer allocator.free(obj.hidden_class.properties);
    defer obj.hidden_class.deinit(allocator);

    const keys = try obj.getOwnEnumerableKeys(allocator);
    defer allocator.free(keys);

    try std.testing.expectEqual(@as(usize, 1), keys.len);
    try std.testing.expectEqual(Atom.length, keys[0]);
}

test "InlineCache hitRatio" {
    var ic = InlineCache{};

    // Initially 0/0 - returns 0
    try std.testing.expectEqual(@as(f64, 0.0), ic.hitRatio());

    // Simulate some hits and misses
    ic.hit_count = 3;
    ic.miss_count = 1;

    try std.testing.expectEqual(@as(f64, 0.75), ic.hitRatio());
}

test "PropertyIterator" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const root_class = try HiddenClass.init(allocator);

    const obj = try JSObject.create(allocator, root_class, null);

    try obj.setProperty(allocator, .length, value.JSValue.fromInt(10));
    try obj.setProperty(allocator, .prototype, value.JSValue.fromInt(20));

    var iter = obj.propertyIterator();
    var count: usize = 0;
    while (iter.next()) |_| {
        count += 1;
    }

    try std.testing.expectEqual(@as(usize, 2), count);
}

// ============================================================================
// HiddenClassPool Tests (Index-Based System)
// ============================================================================

test "HiddenClassPool init and empty class" {
    const allocator = std.testing.allocator;

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    // Empty class at index 0
    const empty = pool.getEmptyClass();
    try std.testing.expectEqual(HiddenClassIndex.empty, empty);
    try std.testing.expectEqual(@as(u16, 0), pool.getPropertyCount(empty));
    try std.testing.expect(pool.getPrototype(empty).isNone());
}

test "HiddenClassPool addProperty transitions" {
    const allocator = std.testing.allocator;

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    const empty = pool.getEmptyClass();

    // Add 'length' property
    const class1 = try pool.addProperty(empty, .length);
    try std.testing.expectEqual(@as(u16, 1), pool.getPropertyCount(class1));

    // Find property
    const slot = pool.findProperty(class1, .length);
    try std.testing.expect(slot != null);
    try std.testing.expectEqual(@as(u16, 0), slot.?);

    // Property not found in empty class
    try std.testing.expect(pool.findProperty(empty, .length) == null);
}

test "HiddenClassPool transition caching" {
    const allocator = std.testing.allocator;

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    const empty = pool.getEmptyClass();

    // Add same property twice should return same class
    const class1 = try pool.addProperty(empty, .length);
    const class2 = try pool.addProperty(empty, .length);
    try std.testing.expectEqual(class1, class2);
}

test "HiddenClassPool multiple properties" {
    const allocator = std.testing.allocator;

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    const empty = pool.getEmptyClass();

    // Add length, then prototype
    const class1 = try pool.addProperty(empty, .length);
    const class2 = try pool.addProperty(class1, .prototype);

    try std.testing.expectEqual(@as(u16, 2), pool.getPropertyCount(class2));

    // Both properties should be findable
    const length_slot = pool.findProperty(class2, .length);
    const proto_slot = pool.findProperty(class2, .prototype);
    try std.testing.expect(length_slot != null);
    try std.testing.expect(proto_slot != null);
    try std.testing.expectEqual(@as(u16, 0), length_slot.?);
    try std.testing.expectEqual(@as(u16, 1), proto_slot.?);
}

test "HiddenClassIndex none check" {
    try std.testing.expect(HiddenClassIndex.none.isNone());
    try std.testing.expect(!HiddenClassIndex.empty.isNone());
    try std.testing.expect(!HiddenClassIndex.fromInt(5).isNone());
}

test "HiddenClassPool propertyNames SoA access" {
    const allocator = std.testing.allocator;

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    const empty = pool.getEmptyClass();

    // Empty class has no properties
    try std.testing.expectEqual(@as(usize, 0), pool.propertyNames(empty).len);

    // Add three properties
    const class1 = try pool.addProperty(empty, .length);
    const class2 = try pool.addProperty(class1, .name);
    const class3 = try pool.addProperty(class2, .message);

    // Verify propertyNames returns correct slice
    const names = pool.propertyNames(class3);
    try std.testing.expectEqual(@as(usize, 3), names.len);
    try std.testing.expectEqual(Atom.length, names[0]);
    try std.testing.expectEqual(Atom.name, names[1]);
    try std.testing.expectEqual(Atom.message, names[2]);

    // Verify getPropertyFlags works
    const flags = pool.getPropertyFlags(class3, .name);
    try std.testing.expect(flags != null);
    try std.testing.expect(flags.?.enumerable == true); // default is true
}
