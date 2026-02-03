//! JavaScript objects with hidden classes and inline caches
//!
//! Fast property access via shape-based inline caching.

const std = @import("std");
const heap = @import("heap.zig");
const bytecode = @import("bytecode.zig");
const value = @import("value.zig");
const string = @import("string.zig");
const arena_mod = @import("arena.zig");

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
    // Common HTTP header atoms (pre-interned for fast request object creation)
    @"content-type" = 207,
    @"content-length" = 208,
    accept = 209,
    host = 210,
    @"user-agent" = 211,
    authorization = 212,
    @"cache-control" = 213,
    connection = 214,
    @"accept-encoding" = 215,
    cookie = 216,
    @"x-forwarded-for" = 217,
    @"x-request-id" = 218,
    // Additional CORS and caching headers
    @"content-encoding" = 219,
    @"transfer-encoding" = 220,
    @"if-modified-since" = 221,
    @"if-none-match" = 222,
    etag = 223,
    @"last-modified" = 224,
    vary = 225,
    origin = 226,
    @"access-control-allow-origin" = 227,
    @"access-control-allow-methods" = 228,
    @"access-control-allow-headers" = 229,
    @"access-control-allow-credentials" = 230,
    @"access-control-max-age" = 231,
    expires = 232,
    pragma = 233,
    toJSON = 234,
    rawJson = 235, // Response.rawJson() for pre-serialized JSON
    // Query parameter support
    path = 236, // URL path without query string
    query = 237, // Query parameters object
    // Reserved for more builtins
    __count__ = 238,

    // Dynamic atoms start at 239
    _,

    pub const FIRST_DYNAMIC: u32 = 239;

    /// Check if atom is a predefined (static) atom
    pub fn isPredefined(self: Atom) bool {
        return @intFromEnum(self) < FIRST_DYNAMIC;
    }

    /// Get the string name for a predefined atom
    pub fn toPredefinedName(self: Atom) ?[]const u8 {
        return switch (self) {
            // Primitives and keywords (0-39)
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
            .arguments => "arguments",
            .caller => "caller",
            .callee => "callee",
            .this => "this",
            .@"return" => "return",
            .throw => "throw",
            .@"try" => "try",
            .@"catch" => "catch",
            .finally => "finally",
            .@"if" => "if",
            .@"else" => "else",
            .@"while" => "while",
            .@"for" => "for",
            .do => "do",
            .@"switch" => "switch",
            .case => "case",
            .default => "default",
            .@"break" => "break",
            .@"continue" => "continue",
            .@"var" => "var",
            .let => "let",
            .@"const" => "const",
            .function => "function",
            .new => "new",
            .delete => "delete",
            .typeof => "typeof",
            .void => "void",
            .in => "in",
            .instanceof => "instanceof",
            // Object properties (40-44)
            .__proto__ => "__proto__",
            .hasOwnProperty => "hasOwnProperty",
            .isPrototypeOf => "isPrototypeOf",
            .propertyIsEnumerable => "propertyIsEnumerable",
            .toLocaleString => "toLocaleString",
            // Array methods (45-71)
            .push => "push",
            .pop => "pop",
            .shift => "shift",
            .unshift => "unshift",
            .slice => "slice",
            .splice => "splice",
            .concat => "concat",
            .join => "join",
            .reverse => "reverse",
            .sort => "sort",
            .indexOf => "indexOf",
            .lastIndexOf => "lastIndexOf",
            .forEach => "forEach",
            .map => "map",
            .filter => "filter",
            .reduce => "reduce",
            .reduceRight => "reduceRight",
            .every => "every",
            .some => "some",
            .find => "find",
            .findIndex => "findIndex",
            .includes => "includes",
            .fill => "fill",
            .copyWithin => "copyWithin",
            .entries => "entries",
            .keys => "keys",
            .values => "values",
            // String methods (72-91)
            .charAt => "charAt",
            .charCodeAt => "charCodeAt",
            .codePointAt => "codePointAt",
            .split => "split",
            .substring => "substring",
            .substr => "substr",
            .toLowerCase => "toLowerCase",
            .toUpperCase => "toUpperCase",
            .trim => "trim",
            .trimStart => "trimStart",
            .trimEnd => "trimEnd",
            .padStart => "padStart",
            .padEnd => "padEnd",
            .repeat => "repeat",
            .replace => "replace",
            .replaceAll => "replaceAll",
            .match => "match",
            .search => "search",
            .startsWith => "startsWith",
            .endsWith => "endsWith",
            // Function methods (92-94)
            .call => "call",
            .apply => "apply",
            .bind => "bind",
            // Math methods (95-108)
            .abs => "abs",
            .floor => "floor",
            .ceil => "ceil",
            .round => "round",
            .min => "min",
            .max => "max",
            .pow => "pow",
            .sqrt => "sqrt",
            .random => "random",
            .sin => "sin",
            .cos => "cos",
            .tan => "tan",
            .log => "log",
            .exp => "exp",
            // JSON (109-110)
            .parse => "parse",
            .stringify => "stringify",
            // Symbol (111-112)
            .iterator => "iterator",
            .toStringTag => "toStringTag",
            // Promise (113-119)
            .then => "then",
            .catch_method => "catch",
            .finally_method => "finally",
            .resolve => "resolve",
            .reject => "reject",
            .all => "all",
            .race => "race",
            // Error types (120-126)
            .Error => "Error",
            .TypeError => "TypeError",
            .RangeError => "RangeError",
            .SyntaxError => "SyntaxError",
            .ReferenceError => "ReferenceError",
            .URIError => "URIError",
            .EvalError => "EvalError",
            // Globals (127-158)
            .console => "console",
            .globalThis => "globalThis",
            .Infinity => "Infinity",
            .NaN_atom => "NaN",
            .Object => "Object",
            .Array => "Array",
            .String => "String",
            .Number => "Number",
            .Boolean => "Boolean",
            .Function => "Function",
            .Symbol => "Symbol",
            .Date => "Date",
            .RegExp => "RegExp",
            .Math => "Math",
            .JSON => "JSON",
            .Promise => "Promise",
            .Proxy => "Proxy",
            .Reflect => "Reflect",
            .Map => "Map",
            .Set => "Set",
            .WeakMap => "WeakMap",
            .WeakSet => "WeakSet",
            .ArrayBuffer => "ArrayBuffer",
            .DataView => "DataView",
            .Int8Array => "Int8Array",
            .Uint8Array => "Uint8Array",
            .Int16Array => "Int16Array",
            .Uint16Array => "Uint16Array",
            .Int32Array => "Int32Array",
            .Uint32Array => "Uint32Array",
            .Float32Array => "Float32Array",
            .Float64Array => "Float64Array",
            // HTTP/Response (159-168)
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
            // JSX runtime (169-174)
            .h => "h",
            .renderToString => "renderToString",
            .Fragment => "Fragment",
            .tag => "tag",
            .props => "props",
            .children => "children",
            // HTML attributes (175-188)
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
            // HTMX (189-196)
            .@"hx-get" => "hx-get",
            .@"hx-post" => "hx-post",
            .@"hx-put" => "hx-put",
            .@"hx-delete" => "hx-delete",
            .@"hx-target" => "hx-target",
            .@"hx-swap" => "hx-swap",
            .@"hx-trigger" => "hx-trigger",
            .@"hx-on--after-request" => "hx-on--after-request",
            // Result type (197-205)
            .Result => "Result",
            .ok => "ok",
            .err => "err",
            .isOk => "isOk",
            .isErr => "isErr",
            .unwrap => "unwrap",
            .unwrapOr => "unwrapOr",
            .unwrapErr => "unwrapErr",
            .mapErr => "mapErr",
            // JSON methods (206)
            .tryParse => "tryParse",
            // HTTP headers (207-233)
            .@"content-type" => "content-type",
            .@"content-length" => "content-length",
            .accept => "accept",
            .host => "host",
            .@"user-agent" => "user-agent",
            .authorization => "authorization",
            .@"cache-control" => "cache-control",
            .connection => "connection",
            .@"accept-encoding" => "accept-encoding",
            .cookie => "cookie",
            .@"x-forwarded-for" => "x-forwarded-for",
            .@"x-request-id" => "x-request-id",
            .@"content-encoding" => "content-encoding",
            .@"transfer-encoding" => "transfer-encoding",
            .@"if-modified-since" => "if-modified-since",
            .@"if-none-match" => "if-none-match",
            .etag => "etag",
            .@"last-modified" => "last-modified",
            .vary => "vary",
            .origin => "origin",
            .@"access-control-allow-origin" => "access-control-allow-origin",
            .@"access-control-allow-methods" => "access-control-allow-methods",
            .@"access-control-allow-headers" => "access-control-allow-headers",
            .@"access-control-allow-credentials" => "access-control-allow-credentials",
            .@"access-control-max-age" => "access-control-max-age",
            .expires => "expires",
            .pragma => "pragma",
            // Response methods (234-237)
            .toJSON => "toJSON",
            .rawJson => "rawJson",
            .path => "path",
            .query => "query",
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
    // HTTP headers (pre-interned for fast request object creation)
    .{ "content-type", .@"content-type" },
    .{ "content-length", .@"content-length" },
    .{ "accept", .accept },
    .{ "host", .host },
    .{ "user-agent", .@"user-agent" },
    .{ "authorization", .authorization },
    .{ "cache-control", .@"cache-control" },
    .{ "connection", .connection },
    .{ "accept-encoding", .@"accept-encoding" },
    .{ "cookie", .cookie },
    .{ "x-forwarded-for", .@"x-forwarded-for" },
    .{ "x-request-id", .@"x-request-id" },
    // Additional CORS and caching headers
    .{ "content-encoding", .@"content-encoding" },
    .{ "transfer-encoding", .@"transfer-encoding" },
    .{ "if-modified-since", .@"if-modified-since" },
    .{ "if-none-match", .@"if-none-match" },
    .{ "etag", .etag },
    .{ "last-modified", .@"last-modified" },
    .{ "vary", .vary },
    .{ "origin", .origin },
    .{ "access-control-allow-origin", .@"access-control-allow-origin" },
    .{ "access-control-allow-methods", .@"access-control-allow-methods" },
    .{ "access-control-allow-headers", .@"access-control-allow-headers" },
    .{ "access-control-allow-credentials", .@"access-control-allow-credentials" },
    .{ "access-control-max-age", .@"access-control-max-age" },
    .{ "expires", .expires },
    .{ "pragma", .pragma },
    .{ "toJSON", .toJSON },
    .{ "rawJson", .rawJson },
    // Query parameter support
    .{ "path", .path },
    .{ "query", .query },
});

/// Lookup predefined atom by name - O(1) using compile-time hash map
pub fn lookupPredefinedAtom(name: []const u8) ?Atom {
    return predefined_atom_map.get(name);
}

/// Native function signature
/// ctx: execution context, this: this value, args: argument slice
/// Returns: result value or error
pub const NativeFn = *const fn (ctx: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue;

/// Builtin function identifiers for fast dispatch in interpreter.
/// Hot builtins bypass the generic wrapper for reduced call overhead.
pub const BuiltinId = enum(u8) {
    none = 0,
    // JSON methods - high frequency in HTTP handlers
    json_parse = 1,
    json_stringify = 2,
    // String methods - common in request processing
    string_index_of = 3,
    string_slice = 4,
    // Math methods - high frequency in pagination/calculations
    math_floor = 5,
    math_ceil = 6,
    math_round = 7,
    math_abs = 8,
    math_min = 9,
    math_max = 10,
    // Number parsing - high frequency for query params/IDs
    parse_int = 11,
    parse_float = 12,
};

/// Native function data stored in function objects
pub const NativeFunctionData = struct {
    func: NativeFn,
    name: Atom,
    arg_count: u8, // Expected argument count (0 = variadic)
    builtin_id: BuiltinId = .none, // For fast dispatch of hot builtins
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

    /// Optional sorted property names for binary search on large classes
    sorted_property_names: std.ArrayListUnmanaged(Atom),
    sorted_property_offsets: std.ArrayListUnmanaged(u16),
    sorted_starts: std.ArrayListUnmanaged(u32),

    /// Transition table: flat array with entries [from_class: u32, atom: u32, to_class: u32]
    /// Kept for serialization, but lookups use transition_map
    transitions: std.ArrayListUnmanaged(u32),

    /// Fast O(1) transition lookup: (from_class << 32 | atom) -> to_class
    transition_map: std.AutoHashMapUnmanaged(u64, HiddenClassIndex),

    /// Number of allocated classes
    count: u32,

    const TRANSITION_ENTRY_SIZE = 3; // from, atom, to
    const SORTED_START_INVALID: u32 = std.math.maxInt(u32);
    const BINARY_SEARCH_THRESHOLD: u16 = 8;

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
            .sorted_property_names = .empty,
            .sorted_property_offsets = .empty,
            .sorted_starts = .empty,
            .transitions = .empty,
            .transition_map = .empty,
            .count = 0,
        };

        // Clean up internal arrays if allocClass fails
        errdefer {
            pool.property_counts.deinit(allocator);
            pool.properties_starts.deinit(allocator);
            pool.prototype_indices.deinit(allocator);
            pool.sorted_starts.deinit(allocator);
        }

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
        self.sorted_property_names.deinit(self.allocator);
        self.sorted_property_offsets.deinit(self.allocator);
        self.sorted_starts.deinit(self.allocator);
        self.transitions.deinit(self.allocator);
        self.transition_map.deinit(self.allocator);
        self.allocator.destroy(self);
    }

    /// Allocate a new hidden class with given property count
    fn allocClass(self: *HiddenClassPool, prop_count: u16, prototype: HiddenClassIndex) !HiddenClassIndex {
        const idx = self.count;
        self.count += 1;

        try self.property_counts.append(self.allocator, prop_count);
        try self.properties_starts.append(self.allocator, @intCast(self.property_names.items.len));
        try self.prototype_indices.append(self.allocator, prototype);
        try self.sorted_starts.append(self.allocator, SORTED_START_INVALID);

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

        if (prop_count >= BINARY_SEARCH_THRESHOLD) {
            const sorted_start = self.sorted_starts.items[i];
            if (sorted_start != SORTED_START_INVALID) {
                if (sorted_start + prop_count > self.sorted_property_names.items.len or
                    sorted_start + prop_count > self.sorted_property_offsets.items.len)
                    return null;
                const names = self.sorted_property_names.items[sorted_start..][0..prop_count];
                const offsets = self.sorted_property_offsets.items[sorted_start..][0..prop_count];
                var lo: usize = 0;
                var hi: usize = names.len;
                const target = @intFromEnum(name);
                while (lo < hi) {
                    const mid = (lo + hi) / 2;
                    const mid_val = @intFromEnum(names[mid]);
                    if (mid_val == target) return offsets[mid];
                    if (mid_val < target) {
                        lo = mid + 1;
                    } else {
                        hi = mid;
                    }
                }
                return null;
            }
        }

        const start = self.properties_starts.items[i];
        if (start + prop_count > self.property_names.items.len or
            start + prop_count > self.property_offsets.items.len)
            return null;
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

        // O(1) lookup for existing transition using hash map
        const key: u64 = (@as(u64, from) << 32) | @intFromEnum(name);
        if (self.transition_map.get(key)) |to_idx| {
            return to_idx;
        }

        // Create new class
        const old_prop_count = self.getPropertyCount(from_idx);
        const new_prop_count = old_prop_count + 1;
        const prototype = self.getPrototype(from_idx);

        const new_idx = try self.allocClass(new_prop_count, prototype);

        // Copy old properties to new location (SoA format)
        // IMPORTANT: Ensure capacity BEFORE copying to avoid aliasing bug where
        // appendSlice would reallocate while reading from the same array
        if (old_prop_count > 0) {
            const old_start = self.properties_starts.items[from];

            // Ensure capacity for old properties + 1 new property
            try self.property_names.ensureUnusedCapacity(self.allocator, new_prop_count);
            try self.property_offsets.ensureUnusedCapacity(self.allocator, new_prop_count);
            try self.property_flags.ensureUnusedCapacity(self.allocator, new_prop_count);

            // Now safe to copy - no reallocation will occur
            self.property_names.appendSliceAssumeCapacity(self.property_names.items[old_start..][0..old_prop_count]);
            self.property_offsets.appendSliceAssumeCapacity(self.property_offsets.items[old_start..][0..old_prop_count]);
            self.property_flags.appendSliceAssumeCapacity(self.property_flags.items[old_start..][0..old_prop_count]);
        }

        // Add new property using SoA arrays
        try self.property_names.append(self.allocator, name);
        try self.property_offsets.append(self.allocator, old_prop_count); // offset = old count
        try self.property_flags.append(self.allocator, .{}); // default flags

        // Record transition in both flat array (for serialization) and hash map (for O(1) lookup)
        try self.transitions.append(self.allocator, from);
        try self.transitions.append(self.allocator, @intFromEnum(name));
        try self.transitions.append(self.allocator, new_idx.toInt());
        try self.transition_map.put(self.allocator, key, new_idx);

        if (new_prop_count >= BINARY_SEARCH_THRESHOLD) {
            try self.buildSortedProperties(new_idx, new_prop_count);
        }

        return new_idx;
    }

    fn buildSortedProperties(self: *HiddenClassPool, idx: HiddenClassIndex, prop_count: u16) !void {
        const i = idx.toInt();
        if (i >= self.count) return;
        const start = self.properties_starts.items[i];
        const names = self.property_names.items[start..][0..prop_count];
        const offsets = self.property_offsets.items[start..][0..prop_count];

        const Entry = struct {
            name: Atom,
            offset: u16,
        };

        var entries = try self.allocator.alloc(Entry, prop_count);
        defer self.allocator.free(entries);

        for (names, 0..) |n, idx_entry| {
            entries[idx_entry] = .{ .name = n, .offset = offsets[idx_entry] };
        }

        std.mem.sort(Entry, entries, {}, struct {
            fn lessThan(_: void, a: Entry, b: Entry) bool {
                return @intFromEnum(a.name) < @intFromEnum(b.name);
            }
        }.lessThan);

        const sorted_start: u32 = @intCast(self.sorted_property_names.items.len);
        try self.sorted_property_names.ensureUnusedCapacity(self.allocator, prop_count);
        try self.sorted_property_offsets.ensureUnusedCapacity(self.allocator, prop_count);

        for (entries) |entry| {
            self.sorted_property_names.appendAssumeCapacity(entry.name);
            self.sorted_property_offsets.appendAssumeCapacity(entry.offset);
        }

        self.sorted_starts.items[i] = sorted_start;
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
        if (start + prop_count > self.property_names.items.len or
            start + prop_count > self.property_flags.items.len)
            return null;
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
        if (start + prop_count > self.property_names.items.len) return &.{};
        return self.property_names.items[start..][0..prop_count];
    }

    /// Get or create a hidden class for functions with 2 reserved slots
    /// This prevents property additions from overwriting internal function data
    /// stored in inline_slots[0] and inline_slots[1]
    pub fn getOrCreateFunctionClass(self: *HiddenClassPool, from_idx: HiddenClassIndex) !HiddenClassIndex {
        const from = from_idx.toInt();

        // Use special atoms for function class transition
        const reserved0: Atom = @enumFromInt(0xFFFE);
        const reserved1: Atom = @enumFromInt(0xFFFF);

        // Check for existing transition
        const trans = self.transitions.items;
        var i: usize = 0;
        while (i + TRANSITION_ENTRY_SIZE <= trans.len) : (i += TRANSITION_ENTRY_SIZE) {
            if (trans[i] == from and trans[i + 1] == @intFromEnum(reserved0)) {
                return HiddenClassIndex.fromInt(trans[i + 2]);
            }
        }

        // Create function class with 2 reserved slots
        const prototype = self.getPrototype(from_idx);
        const new_idx = try self.allocClass(2, prototype);

        // Add reserved property slots (using SoA arrays)
        try self.property_names.append(self.allocator, reserved0);
        try self.property_offsets.append(self.allocator, 0);
        try self.property_flags.append(self.allocator, .{});

        try self.property_names.append(self.allocator, reserved1);
        try self.property_offsets.append(self.allocator, 1);
        try self.property_flags.append(self.allocator, .{});

        // Record transition
        try self.transitions.append(self.allocator, from);
        try self.transitions.append(self.allocator, @intFromEnum(reserved0));
        try self.transitions.append(self.allocator, new_idx.toInt());

        return new_idx;
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
    hidden_class_idx: HiddenClassIndex,
    _padding: u32 = 0, // Alignment padding (was pointer, now u32 index)
    prototype: ?*JSObject,
    class_id: ClassId,
    flags: ObjectFlags,
    inline_slots: [INLINE_SLOT_COUNT]value.JSValue,
    overflow_slots: ?[*]value.JSValue,
    overflow_capacity: u16,
    /// Arena pointer for arena-allocated objects (null for standard allocator)
    arena_ptr: ?*arena_mod.Arena,

    pub const INLINE_SLOT_COUNT = 8;

    /// Inline slot indices with semantic meaning
    /// Different object types use slots for different purposes:
    pub const Slots = struct {
        /// Function objects: data pointer (NativeFunctionData/BytecodeFunctionData/ClosureData)
        pub const FUNC_DATA: usize = 0;
        /// Function objects: true_val for bytecode function, undefined for native
        pub const FUNC_IS_BYTECODE: usize = 1;
        /// Bytecode function objects: guard_id for fast JIT monomorphic call checks
        /// Stored as raw u64 - single comparison replaces 5-check guard sequence
        pub const FUNC_GUARD_ID: usize = 2;
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
        is_arena: bool = false, // Allocated from request arena (ephemeral)
    };

    /// Create a new object
    /// When pool is provided, pre-allocates overflow_slots if the hidden class has >8 properties.
    pub fn create(allocator: std.mem.Allocator, class_idx: HiddenClassIndex, prototype: ?*JSObject, pool: ?*const HiddenClassPool) !*JSObject {
        const obj = try allocator.create(JSObject);
        obj.* = .{
            .header = heap.MemBlockHeader.init(.object, @sizeOf(JSObject)),
            .hidden_class_idx = class_idx,
            .prototype = prototype,
            .class_id = .object,
            .flags = .{},
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
            .arena_ptr = null,
        };
        // Pre-allocate overflow slots if the hidden class has >8 properties
        if (pool) |p| {
            const prop_count = p.getPropertyCount(class_idx);
            if (prop_count > INLINE_SLOT_COUNT) {
                try obj.ensureOverflowCapacity(allocator, prop_count - INLINE_SLOT_COUNT);
            }
        }
        return obj;
    }

    /// Create a new object using arena allocation (ephemeral, dies at request end)
    /// When pool is provided, pre-allocates overflow_slots if the hidden class has >8 properties.
    pub fn createWithArena(arena: *arena_mod.Arena, class_idx: HiddenClassIndex, prototype: ?*JSObject, pool: ?*const HiddenClassPool) ?*JSObject {
        const obj = arena.create(JSObject) orelse return null;
        obj.* = .{
            .header = heap.MemBlockHeader.init(.object, @sizeOf(JSObject)),
            .hidden_class_idx = class_idx,
            .prototype = prototype,
            .class_id = .object,
            .flags = .{ .is_arena = true },
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
            .arena_ptr = arena,
        };
        // Pre-allocate overflow slots if the hidden class has >8 properties
        if (pool) |p| {
            const prop_count = p.getPropertyCount(class_idx);
            if (prop_count > INLINE_SLOT_COUNT) {
                const overflow_needed: u16 = prop_count - INLINE_SLOT_COUNT;
                const overflow_capacity = @max(overflow_needed, 4);
                const slots = arena.allocSlice(value.JSValue, overflow_capacity) orelse return null;
                for (0..overflow_capacity) |i| {
                    slots[i] = value.JSValue.undefined_val;
                }
                obj.overflow_slots = slots.ptr;
                obj.overflow_capacity = overflow_capacity;
            }
        }
        return obj;
    }

    /// Create object with arena allocation, only initializing needed slots (fast path)
    /// Used by JIT when property count is known at compile time
    pub fn createWithArenaFast(arena: *arena_mod.Arena, class_idx: HiddenClassIndex, slot_count: u8) ?*JSObject {
        const obj = arena.create(JSObject) orelse return null;
        // Initialize header fields
        obj.header = heap.MemBlockHeader.init(.object, @sizeOf(JSObject));
        obj.hidden_class_idx = class_idx;
        obj.prototype = null;
        obj.class_id = .object;
        obj.flags = .{ .is_arena = true };
        obj.overflow_slots = null;
        obj.overflow_capacity = 0;
        obj.arena_ptr = arena;
        // Only initialize the slots we need (up to INLINE_SLOT_COUNT)
        const count = @min(slot_count, INLINE_SLOT_COUNT);
        for (0..count) |i| {
            obj.inline_slots[i] = value.JSValue.undefined_val;
        }
        return obj;
    }

    /// Create an array object using arena allocation
    pub fn createArrayWithArena(arena: *arena_mod.Arena, class_idx: HiddenClassIndex) ?*JSObject {
        const obj = arena.create(JSObject) orelse return null;
        obj.* = .{
            .header = heap.MemBlockHeader.init(.object, @sizeOf(JSObject)),
            .hidden_class_idx = class_idx,
            .prototype = null,
            .class_id = .array,
            .flags = .{ .is_exotic = true, .has_small_array = true, .is_arena = true },
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
            .arena_ptr = arena,
        };
        obj.inline_slots[Slots.ARRAY_LENGTH] = value.JSValue.fromInt(0);
        return obj;
    }

    /// Destroy object
    pub fn destroy(self: *JSObject, allocator: std.mem.Allocator) void {
        if (self.overflow_slots) |slots| {
            allocator.free(slots[0..self.overflow_capacity]);
        }
        allocator.destroy(self);
    }

    fn destroyConstant(allocator: std.mem.Allocator, constant: value.JSValue) void {
        if (constant.isExternPtr()) {
            const magic = constant.toExternPtr(u32);
            if (magic.* == bytecode.MAGIC) {
                const nested = constant.toExternPtr(bytecode.FunctionBytecode);
                destroyFunctionBytecode(allocator, nested);
            }
            return;
        }
        if (constant.isBoxedFloat64()) {
            const box = constant.toPtr(value.JSValue.Float64Box);
            allocator.destroy(box);
            return;
        }
        if (constant.isString()) {
            const str = constant.toPtr(string.JSString);
            if (!str.flags.is_unique) {
                string.freeString(allocator, str);
            }
        }
    }

    fn destroyFunctionBytecode(allocator: std.mem.Allocator, func: *bytecode.FunctionBytecode) void {
        const jit = @import("jit/alloc.zig");

        if (func.compiled_code) |cc| {
            const compiled: *jit.CompiledCode = @ptrCast(@alignCast(cc));
            allocator.destroy(compiled);
        }
        if (func.type_feedback_ptr) |tf| {
            tf.deinit();
        }
        if (func.feedback_site_map) |site_map| {
            allocator.free(site_map);
        }
        if (func.pattern_dispatch) |dispatch| {
            dispatch.deinit();
            allocator.destroy(dispatch);
        }

        for (func.constants) |constant| {
            destroyConstant(allocator, constant);
        }

        if (func.code.len > 0) allocator.free(@constCast(func.code));
        if (func.constants.len > 0) allocator.free(@constCast(func.constants));
        if (func.upvalue_info.len > 0) allocator.free(@constCast(func.upvalue_info));
        if (func.source_map) |sm| allocator.free(@constCast(sm));
        allocator.destroy(func);
    }

    /// Destroy object including internal data (e.g., NativeFunctionData, BytecodeFunctionData)
    /// Use this for builtin objects that won't be GC'd
    pub fn destroyFull(self: *JSObject, allocator: std.mem.Allocator) void {
        // Free function data if this is a function
        if (self.class_id == .function and self.flags.is_callable) {
            const is_bytecode_val = self.inline_slots[Slots.FUNC_IS_BYTECODE];
            const data_val = self.inline_slots[Slots.FUNC_DATA];
            if (is_bytecode_val.isUndefined()) {
                // Native function - free the NativeFunctionData
                if (data_val.isExternPtr()) {
                    const data = data_val.toExternPtr(NativeFunctionData);
                    allocator.destroy(data);
                }
            } else if (data_val.isExternPtr()) {
                // Bytecode function - free the BytecodeFunctionData and FunctionBytecode internals
                const bc_data = data_val.toExternPtr(BytecodeFunctionData);
                destroyFunctionBytecode(allocator, @constCast(bc_data.bytecode));
                // Free the BytecodeFunctionData
                allocator.destroy(bc_data);
            }
        }
        // Free overflow slots and object
        self.destroy(allocator);
    }

    /// Destroy a builtin object and all its function properties
    /// Used for cleaning up Math, JSON, Object, etc. during Context.deinit
    pub fn destroyBuiltin(self: *JSObject, allocator: std.mem.Allocator, pool: *const HiddenClassPool) void {
        // Iterate through all property slots
        const prop_count = pool.getPropertyCount(self.hidden_class_idx);
        var slot_idx: u16 = 0;
        while (slot_idx < prop_count) : (slot_idx += 1) {
            const val = if (slot_idx < INLINE_SLOT_COUNT)
                self.inline_slots[slot_idx]
            else if (self.overflow_slots) |slots|
                slots[slot_idx - INLINE_SLOT_COUNT]
            else
                continue;

            // If property is a function object, destroy it recursively
            // (functions like Response constructor may have their own method properties)
            if (val.isObject()) {
                const obj = val.toPtr(JSObject);
                if (obj.class_id == .function) {
                    obj.destroyBuiltin(allocator, pool);
                }
            } else if (val.isString()) {
                // Free string values (e.g., Fragment constant)
                const str = val.toPtr(string.JSString);
                string.freeString(allocator, str);
            }
        }
        // Destroy the builtin object itself (use destroyFull to handle NativeFunctionData)
        self.destroyFull(allocator);
    }

    /// Create a native function object
    /// Set builtin_id for hot builtins to enable fast dispatch in interpreter
    pub fn createNativeFunction(allocator: std.mem.Allocator, pool: *HiddenClassPool, class_idx: HiddenClassIndex, func: NativeFn, name: Atom, arg_count: u8) !*JSObject {
        return createNativeFunctionWithId(allocator, pool, class_idx, func, name, arg_count, .none);
    }

    /// Create a native function object with explicit builtin ID for fast dispatch
    pub fn createNativeFunctionWithId(allocator: std.mem.Allocator, pool: *HiddenClassPool, class_idx: HiddenClassIndex, func: NativeFn, name: Atom, arg_count: u8, builtin_id: BuiltinId) !*JSObject {
        // Allocate native function data
        const data = try allocator.create(NativeFunctionData);
        errdefer allocator.destroy(data);
        data.* = .{
            .func = func,
            .name = name,
            .arg_count = arg_count,
            .builtin_id = builtin_id,
        };

        // Create a hidden class that reserves slots 0-1 for internal function data
        // This prevents setProperty from overwriting the native function data
        const func_class_idx = try pool.getOrCreateFunctionClass(class_idx);

        // Create function object
        const obj = try allocator.create(JSObject);
        obj.* = .{
            .header = heap.MemBlockHeader.init(.object, @sizeOf(JSObject)),
            .hidden_class_idx = func_class_idx,
            .prototype = null,
            .class_id = .function,
            .flags = .{ .is_callable = true },
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
            .arena_ptr = null,
        };
        // Store native function data pointer
        obj.inline_slots[Slots.FUNC_DATA] = value.JSValue.fromExternPtr(data);
        return obj;
    }

    /// Get native function data (if this is a native function)
    pub fn getNativeFunctionData(self: *const JSObject) ?*NativeFunctionData {
        if (self.class_id != .function or !self.flags.is_callable) return null;
        const slot = self.inline_slots[Slots.FUNC_DATA];
        if (!slot.isExternPtr()) return null;
        // Native functions have undefined in FUNC_IS_BYTECODE slot
        if (!self.inline_slots[Slots.FUNC_IS_BYTECODE].isUndefined()) return null;
        return slot.toExternPtr(NativeFunctionData);
    }

    /// Create a JS bytecode function object
    /// Copies is_generator and is_async flags from bytecode for efficient call-time checks
    pub fn createBytecodeFunction(allocator: std.mem.Allocator, class_idx: HiddenClassIndex, bytecode_ptr: *const @import("bytecode.zig").FunctionBytecode, name: Atom) !*JSObject {
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
            .hidden_class_idx = class_idx,
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
            .arena_ptr = null,
        };
        // Store bytecode function data and marker
        obj.inline_slots[Slots.FUNC_DATA] = value.JSValue.fromExternPtr(data);
        obj.inline_slots[Slots.FUNC_IS_BYTECODE] = value.JSValue.true_val;

        // Store guard_id for fast JIT monomorphic call checks
        // This enables single 64-bit comparison instead of 5-check guard sequence
        const bc_mut = @constCast(bytecode_ptr);
        bc_mut.ensureGuardId();
        obj.inline_slots[Slots.FUNC_GUARD_ID] = .{ .raw = bc_mut.guard_id };
        return obj;
    }

    /// Get bytecode function data (if this is a bytecode function)
    pub fn getBytecodeFunctionData(self: *const JSObject) ?*BytecodeFunctionData {
        if (self.class_id != .function or !self.flags.is_callable) {
            std.log.debug("getBytecodeFunc fail: class_id={} is_callable={}", .{ @intFromEnum(self.class_id), self.flags.is_callable });
            return null;
        }
        const slot = self.inline_slots[Slots.FUNC_DATA];
        if (!slot.isExternPtr()) {
            std.log.debug("getBytecodeFunc fail: FUNC_DATA not ext ptr, raw={x}", .{slot.raw});
            return null;
        }
        // Bytecode functions have non-undefined in FUNC_IS_BYTECODE slot
        if (self.inline_slots[Slots.FUNC_IS_BYTECODE].isUndefined()) {
            std.log.debug("getBytecodeFunc fail: FUNC_IS_BYTECODE is undefined", .{});
            return null;
        }
        std.log.debug("getBytecodeFunc success!", .{});
        return slot.toExternPtr(BytecodeFunctionData);
    }

    /// Create a closure (function with captured upvalues)
    /// Copies is_generator and is_async flags from bytecode for efficient call-time checks
    pub fn createClosure(
        allocator: std.mem.Allocator,
        class_idx: HiddenClassIndex,
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
            .hidden_class_idx = class_idx,
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
            .arena_ptr = null,
        };
        // Store closure data and markers
        obj.inline_slots[Slots.FUNC_DATA] = value.JSValue.fromExternPtr(data);
        obj.inline_slots[Slots.FUNC_IS_BYTECODE] = value.JSValue.true_val;
        obj.inline_slots[Slots.FUNC_IS_CLOSURE] = value.JSValue.true_val;

        // Store guard_id for fast JIT monomorphic call checks
        const bc_mut = @constCast(bytecode_ptr);
        bc_mut.ensureGuardId();
        obj.inline_slots[Slots.FUNC_GUARD_ID] = .{ .raw = bc_mut.guard_id };
        return obj;
    }

    /// Get closure data (if this is a closure)
    pub fn getClosureData(self: *const JSObject) ?*ClosureData {
        if (self.class_id != .function or !self.flags.is_callable) return null;
        // Check if it's a closure
        if (!self.inline_slots[Slots.FUNC_IS_CLOSURE].isTrue()) return null;
        const slot = self.inline_slots[Slots.FUNC_DATA];
        if (!slot.isExternPtr()) return null;
        return slot.toExternPtr(ClosureData);
    }

    /// Create a generator object from bytecode function
    pub fn createGenerator(allocator: std.mem.Allocator, class_idx: HiddenClassIndex, bytecode_ptr: *const @import("bytecode.zig").FunctionBytecode, prototype: ?*JSObject) !*JSObject {
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
            .hidden_class_idx = class_idx,
            .prototype = prototype,
            .class_id = .generator,
            .flags = .{},
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
            .arena_ptr = null,
        };
        // Store generator data pointer
        obj.inline_slots[Slots.GENERATOR_DATA] = value.JSValue.fromExternPtr(data);
        return obj;
    }

    /// Create a generator object using arena allocation (ephemeral)
    pub fn createGeneratorWithArena(arena: *arena_mod.Arena, class_idx: HiddenClassIndex, bytecode_ptr: *const @import("bytecode.zig").FunctionBytecode, prototype: ?*JSObject) ?*JSObject {
        const data = arena.create(GeneratorData) orelse return null;
        const locals = arena.allocSlice(value.JSValue, bytecode_ptr.local_count) orelse return null;
        @memset(locals, value.JSValue.undefined_val);
        const stack = arena.allocSlice(value.JSValue, 64) orelse return null;
        @memset(stack, value.JSValue.undefined_val);

        data.* = .{
            .bytecode = bytecode_ptr,
            .state = .suspended_start,
            .pc_offset = 0,
            .locals = locals,
            .stack = stack,
            .stack_len = 0,
        };

        const obj = arena.create(JSObject) orelse return null;
        obj.* = .{
            .header = heap.MemBlockHeader.init(.object, @sizeOf(JSObject)),
            .hidden_class_idx = class_idx,
            .prototype = prototype,
            .class_id = .generator,
            .flags = .{ .is_arena = true },
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
            .arena_ptr = arena,
        };
        obj.inline_slots[Slots.GENERATOR_DATA] = value.JSValue.fromExternPtr(data);
        return obj;
    }

    /// Get generator data (if this is a generator object)
    pub fn getGeneratorData(self: *const JSObject) ?*GeneratorData {
        if (self.class_id != .generator) return null;
        const slot = self.inline_slots[Slots.GENERATOR_DATA];
        if (!slot.isExternPtr()) return null;
        return slot.toExternPtr(GeneratorData);
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
    pub fn getProperty(self: *const JSObject, pool: *const HiddenClassPool, name: Atom) ?value.JSValue {
        if (self.class_id == .array and name == .length) {
            return self.inline_slots[Slots.ARRAY_LENGTH];
        }
        // Range iterator: compute length on demand
        if (self.class_id == .range_iterator and name == .length) {
            return value.JSValue.fromInt(@intCast(self.getRangeLength()));
        }
        // Check own properties first
        if (pool.findProperty(self.hidden_class_idx, name)) |offset| {
            return self.getSlot(offset);
        }

        // Walk prototype chain
        var proto = self.prototype;
        while (proto) |p| {
            if (pool.findProperty(p.hidden_class_idx, name)) |offset| {
                return p.getSlot(offset);
            }
            proto = p.prototype;
        }

        return null;
    }

    /// Get own property (no prototype lookup)
    pub fn getOwnProperty(self: *const JSObject, pool: *const HiddenClassPool, name: Atom) ?value.JSValue {
        if (self.class_id == .array and name == .length) {
            return self.inline_slots[Slots.ARRAY_LENGTH];
        }
        // Range iterator: compute length on demand
        if (self.class_id == .range_iterator and name == .length) {
            return value.JSValue.fromInt(@intCast(self.getRangeLength()));
        }
        if (pool.findProperty(self.hidden_class_idx, name)) |offset| {
            return self.getSlot(offset);
        }
        return null;
    }

    /// Set property (with hidden class transition)
    /// Returns error.ArenaObjectEscape if storing arena object into persistent object
    pub fn setProperty(self: *JSObject, allocator: std.mem.Allocator, pool: *HiddenClassPool, name: Atom, val: value.JSValue) !void {
        if (self.class_id == .array and name == .length) {
            if (val.isInt()) {
                const len = val.getInt();
                if (len >= 0) {
                    self.setArrayLength(@intCast(len));
                }
            }
            return;
        }

        // Note: Arena escape checking is done in Context.setPropertyChecked
        // which respects the enforce_arena_escape flag

        // Check if property exists
        if (pool.findProperty(self.hidden_class_idx, name)) |offset| {
            self.setSlot(offset, val);
            return;
        }

        // Check extensibility
        if (!self.flags.extensible) {
            return; // Silently fail in non-strict mode
        }

        // Transition to new hidden class
        const new_class_idx = try pool.addProperty(self.hidden_class_idx, name);
        const new_slot = pool.getPropertyCount(new_class_idx) - 1;

        // Ensure we have space
        if (new_slot >= INLINE_SLOT_COUNT) {
            try self.ensureOverflowCapacity(allocator, new_slot - INLINE_SLOT_COUNT + 1);
        }

        self.hidden_class_idx = new_class_idx;
        self.setSlot(new_slot, val);
    }

    /// Ensure overflow slots have enough capacity
    /// Uses arena allocation for arena objects, standard allocator otherwise
    fn ensureOverflowCapacity(self: *JSObject, allocator: std.mem.Allocator, min_capacity: u16) !void {
        if (self.overflow_capacity >= min_capacity) return;

        const new_capacity = @max(min_capacity, self.overflow_capacity * 2, 4);

        // Use arena for arena objects, standard allocator otherwise
        const new_slots: []value.JSValue = if (self.arena_ptr) |arena| blk: {
            break :blk arena.allocSlice(value.JSValue, new_capacity) orelse return error.OutOfMemory;
        } else blk: {
            break :blk try allocator.alloc(value.JSValue, new_capacity);
        };

        // Copy existing
        if (self.overflow_slots) |old_slots| {
            @memcpy(new_slots[0..self.overflow_capacity], old_slots[0..self.overflow_capacity]);
            // Only free if not arena (arena memory freed on reset)
            if (self.arena_ptr == null) {
                allocator.free(old_slots[0..self.overflow_capacity]);
            }
        }

        // Initialize new slots
        for (self.overflow_capacity..new_capacity) |i| {
            new_slots[i] = value.JSValue.undefined_val;
        }

        self.overflow_slots = new_slots.ptr;
        self.overflow_capacity = new_capacity;
    }

    /// Check if object has own property
    pub fn hasOwnProperty(self: *const JSObject, pool: *const HiddenClassPool, name: Atom) bool {
        return pool.findProperty(self.hidden_class_idx, name) != null;
    }

    /// Check if property exists (including prototype chain)
    pub fn hasProperty(self: *const JSObject, pool: *const HiddenClassPool, name: Atom) bool {
        return self.getProperty(pool, name) != null;
    }

    /// Delete property
    pub fn deleteProperty(self: *JSObject, pool: *const HiddenClassPool, name: Atom) bool {
        if (self.class_id == .array and name == .length) return false;
        if (pool.findProperty(self.hidden_class_idx, name)) |offset| {
            const flags = pool.getPropertyFlags(self.hidden_class_idx, name) orelse return true;
            if (!flags.configurable) return false;
            // Note: In a full implementation, we'd transition to a new hidden class
            // For now, just set to undefined
            self.setSlot(offset, value.JSValue.undefined_val);
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
    pub fn getOwnEnumerableKeys(self: *const JSObject, allocator: std.mem.Allocator, pool: *const HiddenClassPool) ![]Atom {
        var keys = std.ArrayList(Atom).empty;
        errdefer keys.deinit(allocator);

        const prop_names = pool.propertyNames(self.hidden_class_idx);
        for (prop_names) |name| {
            const flags = pool.getPropertyFlags(self.hidden_class_idx, name) orelse continue;
            if (flags.enumerable) {
                try keys.append(allocator, name);
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
    pub fn createArray(allocator: std.mem.Allocator, class_idx: HiddenClassIndex) !*JSObject {
        const obj = try allocator.create(JSObject);
        obj.* = .{
            .header = heap.MemBlockHeader.init(.object, @sizeOf(JSObject)),
            .hidden_class_idx = class_idx,
            .prototype = null,
            .class_id = .array,
            .flags = .{ .is_exotic = true, .has_small_array = true },
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
            .arena_ptr = null,
        };
        // Initialize array length to 0
        obj.inline_slots[Slots.ARRAY_LENGTH] = value.JSValue.fromInt(0);
        return obj;
    }

    /// Create a lazy range iterator object
    /// Unlike createArray, this doesn't pre-allocate elements - values are computed on access
    pub fn createRangeIterator(allocator: std.mem.Allocator, class_idx: HiddenClassIndex, start: i32, end: i32, step: i32) !*JSObject {
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
            .hidden_class_idx = class_idx,
            .prototype = null,
            .class_id = .range_iterator,
            .flags = .{ .is_exotic = true },
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
            .arena_ptr = null,
        };
        // Store range parameters and cached length
        obj.inline_slots[Slots.RANGE_START] = value.JSValue.fromInt(start);
        obj.inline_slots[Slots.RANGE_END] = value.JSValue.fromInt(end);
        obj.inline_slots[Slots.RANGE_STEP] = value.JSValue.fromInt(step);
        obj.inline_slots[Slots.RANGE_LENGTH] = value.JSValue.fromInt(@intCast(length));
        return obj;
    }

    /// Create a lazy range iterator object using arena allocation (ephemeral)
    pub fn createRangeIteratorWithArena(arena: *arena_mod.Arena, class_idx: HiddenClassIndex, start: i32, end: i32, step: i32) ?*JSObject {
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

        const obj = arena.create(JSObject) orelse return null;
        obj.* = .{
            .header = heap.MemBlockHeader.init(.object, @sizeOf(JSObject)),
            .hidden_class_idx = class_idx,
            .prototype = null,
            .class_id = .range_iterator,
            .flags = .{ .is_exotic = true, .is_arena = true },
            .inline_slots = [_]value.JSValue{value.JSValue.undefined_val} ** INLINE_SLOT_COUNT,
            .overflow_slots = null,
            .overflow_capacity = 0,
            .arena_ptr = arena,
        };
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
        pool: *const HiddenClassPool,
        index: u16,
        prop_count: u16,

        pub fn next(self: *PropertyIterator) ?PropertyEntry {
            if (self.index >= self.prop_count) return null;

            const prop_names = self.pool.propertyNames(self.obj.hidden_class_idx);
            if (self.index >= prop_names.len) return null;

            const name = prop_names[self.index];
            const val = self.obj.getSlot(self.index);
            self.index += 1;

            return .{
                .atom = name,
                .value = val,
            };
        }
    };

    /// Get property iterator
    pub fn propertyIterator(self: *const JSObject, pool: *const HiddenClassPool) PropertyIterator {
        return .{
            .obj = self,
            .pool = pool,
            .index = 0,
            .prop_count = pool.getPropertyCount(self.hidden_class_idx),
        };
    }
};

/// Inline cache for property access
pub const InlineCache = struct {
    cached_class_idx: HiddenClassIndex = .none,
    cached_slot: u16 = 0,
    hit_count: u32 = 0,
    miss_count: u32 = 0,

    /// Try fast property lookup via cached shape
    pub fn get(self: *InlineCache, obj: *JSObject) ?value.JSValue {
        if (obj.hidden_class_idx == self.cached_class_idx and !self.cached_class_idx.isNone()) {
            self.hit_count += 1;
            return obj.getPropertyFast(self.cached_slot);
        }
        self.miss_count += 1;
        return null;
    }

    /// Update cache after miss
    pub fn update(self: *InlineCache, class_idx: HiddenClassIndex, slot: u16) void {
        self.cached_class_idx = class_idx;
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

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    var obj = try JSObject.create(allocator, pool.getEmptyClass(), null, pool);
    defer obj.destroy(allocator);

    // Initially no properties
    try std.testing.expect(obj.getOwnProperty(pool, .length) == null);

    // Set property
    try obj.setProperty(allocator, pool, .length, value.JSValue.fromInt(42));

    // Get property
    const val = obj.getOwnProperty(pool, .length);
    try std.testing.expect(val != null);
    try std.testing.expectEqual(@as(i32, 42), val.?.getInt());
}

test "JSObject prototype chain lookup" {
    const allocator = std.testing.allocator;

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    // Create prototype
    var proto = try JSObject.create(allocator, pool.getEmptyClass(), null, pool);
    defer proto.destroy(allocator);

    try proto.setProperty(allocator, pool, .toString, value.JSValue.fromInt(1));

    // Create child object with prototype
    var child = try JSObject.create(allocator, pool.getEmptyClass(), proto, pool);
    defer child.destroy(allocator);

    // Child can access prototype property
    const val = child.getProperty(pool, .toString);
    try std.testing.expect(val != null);
    try std.testing.expectEqual(@as(i32, 1), val.?.getInt());

    // But not as own property
    try std.testing.expect(child.getOwnProperty(pool, .toString) == null);
}

test "JSObject hasProperty" {
    const allocator = std.testing.allocator;

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    var obj = try JSObject.create(allocator, pool.getEmptyClass(), null, pool);
    defer obj.destroy(allocator);

    try std.testing.expect(!obj.hasOwnProperty(pool, .length));
    try std.testing.expect(!obj.hasProperty(pool, .length));

    try obj.setProperty(allocator, pool, .length, value.JSValue.fromInt(5));

    try std.testing.expect(obj.hasOwnProperty(pool, .length));
    try std.testing.expect(obj.hasProperty(pool, .length));
}

test "JSObject extensibility" {
    const allocator = std.testing.allocator;

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    var obj = try JSObject.create(allocator, pool.getEmptyClass(), null, pool);
    defer obj.destroy(allocator);

    try std.testing.expect(obj.isExtensible());

    obj.preventExtensions();
    try std.testing.expect(!obj.isExtensible());

    // Can't add new properties
    try obj.setProperty(allocator, pool, .length, value.JSValue.fromInt(10));
    try std.testing.expect(obj.getOwnProperty(pool, .length) == null);
}

test "JSObject to/from value" {
    const allocator = std.testing.allocator;

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    var obj = try JSObject.create(allocator, pool.getEmptyClass(), null, pool);
    defer obj.destroy(allocator);

    const js_val = obj.toValue();
    try std.testing.expect(js_val.isPtr());

    const recovered = JSObject.fromValue(js_val);
    try std.testing.expectEqual(obj, recovered);
}

test "InlineCache with object" {
    const allocator = std.testing.allocator;

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    var obj = try JSObject.create(allocator, pool.getEmptyClass(), null, pool);
    defer obj.destroy(allocator);

    try obj.setProperty(allocator, pool, .length, value.JSValue.fromInt(100));

    var ic = InlineCache{};

    // First access - cache miss
    const miss_result = ic.get(obj);
    try std.testing.expect(miss_result == null);
    try std.testing.expectEqual(@as(u32, 1), ic.miss_count);

    // Update cache
    if (pool.findProperty(obj.hidden_class_idx, .length)) |slot| {
        ic.update(obj.hidden_class_idx, slot);
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

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    var obj = try JSObject.create(allocator, pool.getEmptyClass(), null, pool);
    defer obj.destroy(allocator);

    try obj.setProperty(allocator, pool, .length, value.JSValue.fromInt(10));

    // Property exists
    try std.testing.expect(obj.hasOwnProperty(pool, .length));

    // Delete property - returns true
    const deleted = obj.deleteProperty(pool, .length);
    try std.testing.expect(deleted);

    // Deleting non-existent property also returns true (JS spec behavior)
    const deleted_again = obj.deleteProperty(pool, .prototype);
    try std.testing.expect(deleted_again);
}

test "JSObject hasOwnProperty" {
    const allocator = std.testing.allocator;

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    var obj = try JSObject.create(allocator, pool.getEmptyClass(), null, pool);
    defer obj.destroy(allocator);

    // Initially no property
    try std.testing.expect(!obj.hasOwnProperty(pool, .length));

    try obj.setProperty(allocator, pool, .length, value.JSValue.fromInt(5));

    try std.testing.expect(obj.hasOwnProperty(pool, .length));
}

test "JSObject array operations" {
    const allocator = std.testing.allocator;

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    var arr = try JSObject.createArray(allocator, pool.getEmptyClass());
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

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    var arr = try JSObject.createArray(allocator, pool.getEmptyClass());
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

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    var arr = try JSObject.createArray(allocator, pool.getEmptyClass());
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

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    var obj = try JSObject.create(allocator, pool.getEmptyClass(), null, pool);
    defer obj.destroy(allocator);

    try obj.setProperty(allocator, pool, .length, value.JSValue.fromInt(10));

    const keys = try obj.getOwnEnumerableKeys(allocator, pool);
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

    var pool = try HiddenClassPool.init(allocator);

    const obj = try JSObject.create(allocator, pool.getEmptyClass(), null, pool);

    try obj.setProperty(allocator, pool, .length, value.JSValue.fromInt(10));
    try obj.setProperty(allocator, pool, .prototype, value.JSValue.fromInt(20));

    var iter = obj.propertyIterator(pool);
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

test "JSObject setProperty/getProperty with pool" {
    const allocator = std.testing.allocator;

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    const empty = pool.getEmptyClass();

    // Create object with empty class
    const obj = try JSObject.create(allocator, empty, null, pool);
    defer obj.destroy(allocator);

    // Verify empty class
    try std.testing.expectEqual(HiddenClassIndex.empty, obj.hidden_class_idx);

    // Set property
    try obj.setProperty(allocator, pool, .console, value.JSValue.fromInt(42));

    // Verify class transitioned
    try std.testing.expect(obj.hidden_class_idx != HiddenClassIndex.empty);
    try std.testing.expectEqual(@as(u16, 1), pool.getPropertyCount(obj.hidden_class_idx));

    // Get property
    const val = obj.getProperty(pool, .console);
    try std.testing.expect(val != null);
    try std.testing.expectEqual(@as(i32, 42), val.?.getInt());
}

test "JSObject overflow slots pre-allocation for >8 properties" {
    const allocator = std.testing.allocator;

    var pool = try HiddenClassPool.init(allocator);
    defer pool.deinit();

    // Build a hidden class with 10 properties (more than INLINE_SLOT_COUNT=8)
    var class_idx = pool.getEmptyClass();
    class_idx = try pool.addProperty(class_idx, .length);
    class_idx = try pool.addProperty(class_idx, .prototype);
    class_idx = try pool.addProperty(class_idx, .constructor);
    class_idx = try pool.addProperty(class_idx, .name);
    class_idx = try pool.addProperty(class_idx, .message);
    class_idx = try pool.addProperty(class_idx, .valueOf);
    class_idx = try pool.addProperty(class_idx, .toString);
    class_idx = try pool.addProperty(class_idx, .toJSON);
    class_idx = try pool.addProperty(class_idx, .caller); // slot 8 (first overflow)
    class_idx = try pool.addProperty(class_idx, .arguments); // slot 9 (second overflow)

    try std.testing.expectEqual(@as(u16, 10), pool.getPropertyCount(class_idx));

    // Create object with this class - should pre-allocate overflow_slots
    const obj = try JSObject.create(allocator, class_idx, null, pool);
    defer obj.destroy(allocator);

    // Verify overflow_slots was allocated
    try std.testing.expect(obj.overflow_slots != null);
    try std.testing.expect(obj.overflow_capacity >= 2);

    // Set values to ALL slots including overflow
    obj.setSlot(0, value.JSValue.fromInt(100)); // length
    obj.setSlot(1, value.JSValue.fromInt(101)); // prototype
    obj.setSlot(7, value.JSValue.fromInt(107)); // toJSON (last inline)
    obj.setSlot(8, value.JSValue.fromInt(108)); // caller (first overflow)
    obj.setSlot(9, value.JSValue.fromInt(109)); // arguments (second overflow)

    // Verify all values were stored correctly
    try std.testing.expectEqual(@as(i32, 100), obj.getSlot(0).getInt());
    try std.testing.expectEqual(@as(i32, 101), obj.getSlot(1).getInt());
    try std.testing.expectEqual(@as(i32, 107), obj.getSlot(7).getInt());
    try std.testing.expectEqual(@as(i32, 108), obj.getSlot(8).getInt());
    try std.testing.expectEqual(@as(i32, 109), obj.getSlot(9).getInt());
}
