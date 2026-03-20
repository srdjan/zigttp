//! Built-in JavaScript objects and methods
//!
//! Standard library implementation for Object, Array, String, etc.
//! Split into sub-modules by domain; this root re-exports the public API
//! and contains the initBuiltins wiring function.

const std = @import("std");

// Sub-modules
pub const helpers = @import("helpers.zig");
pub const object_builtins = @import("object_builtins.zig");
pub const date = @import("date.zig");
pub const json = @import("json.zig");
pub const err = @import("error.zig");
pub const number = @import("number.zig");
pub const map = @import("map.zig");
pub const set = @import("set.zig");
pub const array = @import("array.zig");
pub const string_builtins = @import("string_builtins.zig");
pub const math = @import("math.zig");
pub const console = @import("console.zig");
pub const result = @import("result.zig");
pub const symbol = @import("symbol.zig");
pub const weak_collections = @import("weak_collections.zig");

// Re-export shared types from helpers
const value = helpers.value;
const object = helpers.object;
const context = helpers.context;
const http = helpers.http;
const wrap = helpers.wrap;
const addMethod = helpers.addMethod;
const addMethodWithId = helpers.addMethodWithId;
const addMethodDynamic = helpers.addMethodDynamic;
const addMethodDynamicWithId = helpers.addMethodDynamicWithId;

// Public re-exports consumed by other codebase modules
pub const ClassId = helpers.ClassId;
pub const JsonError = json.JsonError;
pub const WeakMapData = weak_collections.WeakMapData;
pub const WeakSetData = weak_collections.WeakSetData;

// Functions referenced by external code
pub const jsonParse = json.jsonParse;
pub const jsonStringify = json.jsonStringify;
pub const jsonTryParse = json.jsonTryParse;
pub const parseJsonValue = json.parseJsonValue;
pub const clearJsonShapeCache = json.clearJsonShapeCache;
pub const createResultOk = helpers.createResultOk;
pub const createResultErr = helpers.createResultErr;
pub const valueToStringSimple = helpers.valueToStringSimple;
pub const dateNow = date.dateNow;
pub const performanceNow = date.performanceNow;
pub const math_constants = math.math_constants;
pub const allocFloat = helpers.allocFloat;
pub const getStringData = helpers.getStringData;
pub const createSymbol = symbol.createSymbol;
pub const createWellKnownSymbol = symbol.createWellKnownSymbol;
pub const initWeakCollections = weak_collections.initWeakCollections;
pub const initSymbol = symbol.initSymbol;
pub const errorToString = err.errorToString;
pub const typeErrorConstructor = err.typeErrorConstructor;
pub const errorConstructor = err.errorConstructor;
pub const rangeErrorConstructor = err.rangeErrorConstructor;
pub const syntaxErrorConstructor = err.syntaxErrorConstructor;
pub const referenceErrorConstructor = err.referenceErrorConstructor;

// Math functions used by interpreter fast paths
pub const mathFloor = math.mathFloor;
pub const mathCeil = math.mathCeil;
pub const mathRound = math.mathRound;
pub const mathAbs = math.mathAbs;
pub const mathMin = math.mathMin;
pub const mathMax = math.mathMax;

// String functions used by interpreter fast paths
pub const stringIndexOf = string_builtins.stringIndexOf;
pub const stringSlice = string_builtins.stringSlice;

// Number functions used by interpreter fast paths
pub const numberParseInt = number.numberParseInt;
pub const numberParseFloat = number.numberParseFloat;

/// Initialize all built-in objects on global
pub fn initBuiltins(ctx: *context.Context) !void {
    const allocator = ctx.allocator;
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const root_class_idx = ctx.root_class_idx;

    // Create console object
    const console_obj = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethod(ctx, allocator, pool, console_obj, root_class_idx, .log, wrap(console.consoleLog), 0);
    try ctx.builtin_objects.append(allocator, console_obj);
    try ctx.setGlobal(.console, console_obj.toValue());

    // Create Math object - hot methods use fast dispatch IDs
    const math_obj = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodWithId(ctx, allocator, pool, math_obj, root_class_idx, .abs, wrap(math.mathAbs), 1, .math_abs);
    try addMethodWithId(ctx, allocator, pool, math_obj, root_class_idx, .floor, wrap(math.mathFloor), 1, .math_floor);
    try addMethodWithId(ctx, allocator, pool, math_obj, root_class_idx, .ceil, wrap(math.mathCeil), 1, .math_ceil);
    try addMethodWithId(ctx, allocator, pool, math_obj, root_class_idx, .round, wrap(math.mathRound), 1, .math_round);
    try addMethodWithId(ctx, allocator, pool, math_obj, root_class_idx, .min, wrap(math.mathMin), 2, .math_min);
    try addMethodWithId(ctx, allocator, pool, math_obj, root_class_idx, .max, wrap(math.mathMax), 2, .math_max);
    try addMethod(ctx, allocator, pool, math_obj, root_class_idx, .pow, wrap(math.mathPow), 2);
    try addMethodDynamic(ctx, math_obj, "trunc", wrap(math.mathTrunc), 1);
    try addMethod(ctx, allocator, pool, math_obj, root_class_idx, .sqrt, wrap(math.mathSqrt), 1);
    try addMethod(ctx, allocator, pool, math_obj, root_class_idx, .sin, wrap(math.mathSin), 1);
    try addMethod(ctx, allocator, pool, math_obj, root_class_idx, .cos, wrap(math.mathCos), 1);
    try addMethod(ctx, allocator, pool, math_obj, root_class_idx, .tan, wrap(math.mathTan), 1);
    try addMethod(ctx, allocator, pool, math_obj, root_class_idx, .log, wrap(math.mathLog), 1);
    try addMethod(ctx, allocator, pool, math_obj, root_class_idx, .exp, wrap(math.mathExp), 1);
    try addMethod(ctx, allocator, pool, math_obj, root_class_idx, .random, wrap(math.mathRandom), 0);

    // Add Math constants as properties (NaN-boxing: no allocation needed)
    try ctx.setPropertyChecked(math_obj, @enumFromInt(ctx.atoms.next_id), value.JSValue.fromFloat(math.math_constants.PI));
    try ctx.builtin_objects.append(allocator, math_obj);
    try ctx.setGlobal(.Math, math_obj.toValue());

    // Create JSON object - parse and stringify are hot builtins with fast dispatch
    const json_obj = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodWithId(ctx, allocator, pool, json_obj, root_class_idx, .parse, wrap(json.jsonParse), 1, .json_parse);
    try addMethod(ctx, allocator, pool, json_obj, root_class_idx, .tryParse, wrap(json.jsonTryParse), 1);
    try addMethodWithId(ctx, allocator, pool, json_obj, root_class_idx, .stringify, wrap(json.jsonStringify), 1, .json_stringify);
    try ctx.builtin_objects.append(allocator, json_obj);
    try ctx.setGlobal(.JSON, json_obj.toValue());

    // Create Object constructor with static methods
    const object_obj = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, object_obj, "keys", wrap(object_builtins.objectKeys), 1);
    try addMethodDynamic(ctx, object_obj, "values", wrap(object_builtins.objectValues), 1);
    try addMethodDynamic(ctx, object_obj, "entries", wrap(object_builtins.objectEntries), 1);
    try addMethodDynamic(ctx, object_obj, "hasOwn", wrap(object_builtins.objectHasOwn), 2);
    try ctx.builtin_objects.append(allocator, object_obj);
    try ctx.setGlobal(.Object, object_obj.toValue());

    // Create Error prototype with toString method
    const error_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, error_proto, "toString", wrap(err.errorToString), 0);
    try ctx.builtin_objects.append(allocator, error_proto);

    // Error constructors
    const error_ctor_func = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(err.errorConstructor), .Error, 1);
    try ctx.setPropertyChecked(error_ctor_func, .prototype, error_proto.toValue());
    try ctx.builtin_objects.append(allocator, error_ctor_func);
    try ctx.setGlobal(.Error, error_ctor_func.toValue());

    const type_error_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(err.typeErrorConstructor), .TypeError, 1);
    try ctx.setPropertyChecked(type_error_ctor, .prototype, error_proto.toValue());
    try ctx.builtin_objects.append(allocator, type_error_ctor);
    try ctx.setGlobal(.TypeError, type_error_ctor.toValue());

    const range_error_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(err.rangeErrorConstructor), .RangeError, 1);
    try ctx.setPropertyChecked(range_error_ctor, .prototype, error_proto.toValue());
    try ctx.builtin_objects.append(allocator, range_error_ctor);
    try ctx.setGlobal(.RangeError, range_error_ctor.toValue());

    const syntax_error_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(err.syntaxErrorConstructor), .SyntaxError, 1);
    try ctx.setPropertyChecked(syntax_error_ctor, .prototype, error_proto.toValue());
    try ctx.builtin_objects.append(allocator, syntax_error_ctor);
    try ctx.setGlobal(.SyntaxError, syntax_error_ctor.toValue());

    const ref_error_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(err.referenceErrorConstructor), .ReferenceError, 1);
    try ctx.setPropertyChecked(ref_error_ctor, .prototype, error_proto.toValue());
    try ctx.builtin_objects.append(allocator, ref_error_ctor);
    try ctx.setGlobal(.ReferenceError, ref_error_ctor.toValue());

    // Create Number object with static methods
    const number_obj = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, number_obj, "isInteger", wrap(number.numberIsInteger), 1);
    try addMethodDynamic(ctx, number_obj, "isNaN", wrap(number.numberIsNaN), 1);
    try addMethodDynamic(ctx, number_obj, "isFinite", wrap(number.numberIsFinite), 1);
    try addMethodDynamic(ctx, number_obj, "parseFloat", wrap(number.numberParseFloat), 1);
    try addMethodDynamic(ctx, number_obj, "parseInt", wrap(number.numberParseInt), 2);

    // Number constants
    const max_value_atom = try ctx.atoms.intern("MAX_VALUE");
    try ctx.setPropertyChecked(number_obj, max_value_atom, value.JSValue.fromFloat(1.7976931348623157e+308));
    const min_value_atom = try ctx.atoms.intern("MIN_VALUE");
    try ctx.setPropertyChecked(number_obj, min_value_atom, value.JSValue.fromFloat(5e-324));
    const nan_atom = try ctx.atoms.intern("NaN");
    try ctx.setPropertyChecked(number_obj, nan_atom, value.JSValue.nan_val);
    const pos_inf_atom = try ctx.atoms.intern("POSITIVE_INFINITY");
    try ctx.setPropertyChecked(number_obj, pos_inf_atom, value.JSValue.fromFloat(std.math.inf(f64)));
    const neg_inf_atom = try ctx.atoms.intern("NEGATIVE_INFINITY");
    try ctx.setPropertyChecked(number_obj, neg_inf_atom, value.JSValue.fromFloat(-std.math.inf(f64)));
    try ctx.builtin_objects.append(allocator, number_obj);
    try ctx.setGlobal(.Number, number_obj.toValue());

    // Global parseFloat/parseInt
    const global_parse_float_atom = try ctx.atoms.intern("parseFloat");
    const parse_float_func = try object.JSObject.createNativeFunctionWithId(allocator, pool, root_class_idx, wrap(number.numberParseFloat), global_parse_float_atom, 1, .parse_float);
    try ctx.builtin_objects.append(allocator, parse_float_func);
    try ctx.setGlobal(global_parse_float_atom, parse_float_func.toValue());

    const global_parse_int_atom = try ctx.atoms.intern("parseInt");
    const parse_int_func = try object.JSObject.createNativeFunctionWithId(allocator, pool, root_class_idx, wrap(number.numberParseInt), global_parse_int_atom, 2, .parse_int);
    try ctx.builtin_objects.append(allocator, parse_int_func);
    try ctx.setGlobal(global_parse_int_atom, parse_int_func.toValue());

    // Global isNaN/isFinite
    const global_is_nan_atom = try ctx.atoms.intern("isNaN");
    const global_is_nan_func = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(number.globalIsNaN), global_is_nan_atom, 1);
    try ctx.builtin_objects.append(allocator, global_is_nan_func);
    try ctx.setGlobal(global_is_nan_atom, global_is_nan_func.toValue());

    const global_is_finite_atom = try ctx.atoms.intern("isFinite");
    const global_is_finite_func = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(number.globalIsFinite), global_is_finite_atom, 1);
    try ctx.builtin_objects.append(allocator, global_is_finite_func);
    try ctx.setGlobal(global_is_finite_atom, global_is_finite_func.toValue());

    // Global range()
    const range_atom = try ctx.atoms.intern("range");
    const range_func = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(number.globalRange), range_atom, 1);
    try ctx.builtin_objects.append(allocator, range_func);
    try ctx.setGlobal(range_atom, range_func.toValue());

    // Global _processRequest
    const process_req_atom = try ctx.atoms.intern("_processRequest");
    const process_req_func = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(number.globalProcessRequest), process_req_atom, 3);
    try ctx.builtin_objects.append(allocator, process_req_func);
    try ctx.setGlobal(process_req_atom, process_req_func.toValue());

    // Map
    const map_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, map_proto, "set", wrap(map.mapSet), 2);
    try addMethodDynamic(ctx, map_proto, "get", wrap(map.mapGet), 1);
    try addMethodDynamic(ctx, map_proto, "has", wrap(map.mapHas), 1);
    try addMethodDynamic(ctx, map_proto, "delete", wrap(map.mapDelete), 1);
    try addMethodDynamic(ctx, map_proto, "clear", wrap(map.mapClear), 0);
    try ctx.builtin_objects.append(allocator, map_proto);
    const map_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(map.mapConstructor), .Map, 0);
    try ctx.setPropertyChecked(map_ctor, .prototype, map_proto.toValue());
    try ctx.builtin_objects.append(allocator, map_ctor);
    try ctx.setGlobal(.Map, map_ctor.toValue());

    // Set
    const set_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, set_proto, "add", wrap(set.setAdd), 1);
    try addMethodDynamic(ctx, set_proto, "has", wrap(set.setHas), 1);
    try addMethodDynamic(ctx, set_proto, "delete", wrap(set.setDelete), 1);
    try addMethodDynamic(ctx, set_proto, "clear", wrap(set.setClear), 0);
    try ctx.builtin_objects.append(allocator, set_proto);
    const set_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(set.setConstructor), .Set, 0);
    try ctx.setPropertyChecked(set_ctor, .prototype, set_proto.toValue());
    try ctx.builtin_objects.append(allocator, set_ctor);
    try ctx.setGlobal(.Set, set_ctor.toValue());

    // Response constructor with static methods
    const response_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, http.responseConstructor, .Response, 2);
    const json_atom: object.Atom = .json;
    const text_atom: object.Atom = .text;
    const html_atom: object.Atom = .html;
    const redirect_atom = try ctx.atoms.intern("redirect");
    const json_fn = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, http.responseJson, json_atom, 1);
    try ctx.setPropertyChecked(response_ctor, json_atom, json_fn.toValue());
    const text_fn = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, http.responseText, text_atom, 1);
    try ctx.setPropertyChecked(response_ctor, text_atom, text_fn.toValue());
    const html_fn = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, http.responseHtml, html_atom, 1);
    try ctx.setPropertyChecked(response_ctor, html_atom, html_fn.toValue());
    const redirect_fn = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, http.responseRedirect, redirect_atom, 1);
    try ctx.setPropertyChecked(response_ctor, redirect_atom, redirect_fn.toValue());
    const rawjson_fn = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, http.responseRawJson, .rawJson, 1);
    try ctx.setPropertyChecked(response_ctor, .rawJson, rawjson_fn.toValue());
    try ctx.builtin_objects.append(allocator, response_ctor);
    try ctx.setGlobal(.Response, response_ctor.toValue());

    // JSX: h(), renderToString(), Fragment
    const h_atom: object.Atom = .h;
    const h_fn = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, http.h, h_atom, 2);
    try ctx.builtin_objects.append(allocator, h_fn);
    try ctx.setGlobal(h_atom, h_fn.toValue());
    const render_atom: object.Atom = .renderToString;
    const render_fn = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, http.renderToString, render_atom, 1);
    try ctx.builtin_objects.append(allocator, render_fn);
    try ctx.setGlobal(render_atom, render_fn.toValue());
    const fragment_atom: object.Atom = .Fragment;
    try ctx.setGlobal(fragment_atom, value.JSValue.undefined_val);

    // Date and performance
    const date_obj = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, date_obj, "now", wrap(date.dateNow), 0);
    try ctx.builtin_objects.append(allocator, date_obj);
    try ctx.setGlobal(.Date, date_obj.toValue());
    const performance_obj = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, performance_obj, "now", wrap(date.performanceNow), 0);
    try ctx.builtin_objects.append(allocator, performance_obj);
    const performance_atom = try ctx.atoms.intern("performance");
    try ctx.setGlobal(performance_atom, performance_obj.toValue());

    // Array.prototype
    const array_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, array_proto, "push", wrap(array.arrayPush), 1);
    try addMethodDynamic(ctx, array_proto, "pop", wrap(array.arrayPop), 0);
    try addMethodDynamic(ctx, array_proto, "shift", wrap(array.arrayShift), 0);
    try addMethodDynamic(ctx, array_proto, "unshift", wrap(array.arrayUnshift), 1);
    try addMethodDynamic(ctx, array_proto, "splice", wrap(array.arraySplice), 2);
    try addMethodDynamic(ctx, array_proto, "indexOf", wrap(array.arrayIndexOf), 1);
    try addMethodDynamic(ctx, array_proto, "includes", wrap(array.arrayIncludes), 1);
    try addMethodDynamic(ctx, array_proto, "join", wrap(array.arrayJoin), 1);
    try addMethodDynamic(ctx, array_proto, "slice", wrap(array.arraySlice), 2);
    try addMethodDynamic(ctx, array_proto, "concat", wrap(array.arrayConcat), 1);
    try addMethodDynamic(ctx, array_proto, "map", wrap(array.arrayMap), 1);
    try addMethodDynamic(ctx, array_proto, "filter", wrap(array.arrayFilter), 1);
    try addMethodDynamic(ctx, array_proto, "reduce", wrap(array.arrayReduce), 2);
    try addMethodDynamic(ctx, array_proto, "forEach", wrap(array.arrayForEach), 1);
    try addMethodDynamic(ctx, array_proto, "every", wrap(array.arrayEvery), 1);
    try addMethodDynamic(ctx, array_proto, "some", wrap(array.arraySome), 1);
    try addMethodDynamic(ctx, array_proto, "find", wrap(array.arrayFind), 1);
    try addMethodDynamic(ctx, array_proto, "findIndex", wrap(array.arrayFindIndex), 1);
    try addMethodDynamic(ctx, array_proto, "toSorted", wrap(array.arrayToSorted), 1);
    try addMethodDynamic(ctx, array_proto, "toReversed", wrap(array.arrayToReversed), 0);
    ctx.array_prototype = array_proto;
    const array_ctor = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, array_ctor, "isArray", wrap(array.arrayIsArray), 1);
    try addMethodDynamic(ctx, array_ctor, "from", wrap(array.arrayFrom), 1);
    try addMethodDynamic(ctx, array_ctor, "of", wrap(array.arrayOf), 0);
    try ctx.builtin_objects.append(allocator, array_ctor);
    try ctx.setGlobal(.Array, array_ctor.toValue());

    // String.prototype
    const string_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, string_proto, "charAt", wrap(string_builtins.stringCharAt), 1);
    try addMethodDynamic(ctx, string_proto, "charCodeAt", wrap(string_builtins.stringCharCodeAt), 1);
    try addMethodDynamicWithId(ctx, string_proto, "indexOf", wrap(string_builtins.stringIndexOf), 1, .string_index_of);
    try addMethodDynamic(ctx, string_proto, "lastIndexOf", wrap(string_builtins.stringLastIndexOf), 1);
    try addMethodDynamic(ctx, string_proto, "startsWith", wrap(string_builtins.stringStartsWith), 1);
    try addMethodDynamic(ctx, string_proto, "endsWith", wrap(string_builtins.stringEndsWith), 1);
    try addMethodDynamic(ctx, string_proto, "includes", wrap(string_builtins.stringIncludes), 1);
    try addMethodDynamicWithId(ctx, string_proto, "slice", wrap(string_builtins.stringSlice), 2, .string_slice);
    try addMethodDynamic(ctx, string_proto, "substring", wrap(string_builtins.stringSubstring), 2);
    try addMethodDynamic(ctx, string_proto, "toLowerCase", wrap(string_builtins.stringToLowerCase), 0);
    try addMethodDynamic(ctx, string_proto, "toUpperCase", wrap(string_builtins.stringToUpperCase), 0);
    try addMethodDynamic(ctx, string_proto, "trim", wrap(string_builtins.stringTrim), 0);
    try addMethodDynamic(ctx, string_proto, "trimStart", wrap(string_builtins.stringTrimStart), 0);
    try addMethodDynamic(ctx, string_proto, "trimEnd", wrap(string_builtins.stringTrimEnd), 0);
    try addMethodDynamic(ctx, string_proto, "split", wrap(string_builtins.stringSplit), 2);
    try addMethodDynamic(ctx, string_proto, "repeat", wrap(string_builtins.stringRepeat), 1);
    try addMethodDynamic(ctx, string_proto, "padStart", wrap(string_builtins.stringPadStart), 2);
    try addMethodDynamic(ctx, string_proto, "padEnd", wrap(string_builtins.stringPadEnd), 2);
    try addMethodDynamic(ctx, string_proto, "concat", wrap(string_builtins.stringConcat), 1);
    try addMethodDynamic(ctx, string_proto, "replace", wrap(string_builtins.stringReplace), 2);
    try addMethodDynamic(ctx, string_proto, "replaceAll", wrap(string_builtins.stringReplaceAll), 2);
    ctx.string_prototype = string_proto;
    const string_ctor = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, string_ctor, "fromCharCode", wrap(string_builtins.stringFromCharCode), 1);
    try ctx.builtin_objects.append(allocator, string_ctor);
    try ctx.setGlobal(.String, string_ctor.toValue());

    // Result type
    const result_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethod(ctx, allocator, pool, result_proto, root_class_idx, .isOk, wrap(result.resultIsOk), 0);
    try addMethod(ctx, allocator, pool, result_proto, root_class_idx, .isErr, wrap(result.resultIsErr), 0);
    try addMethod(ctx, allocator, pool, result_proto, root_class_idx, .unwrap, wrap(result.resultUnwrap), 0);
    try addMethod(ctx, allocator, pool, result_proto, root_class_idx, .unwrapOr, wrap(result.resultUnwrapOr), 1);
    try addMethod(ctx, allocator, pool, result_proto, root_class_idx, .unwrapErr, wrap(result.resultUnwrapErr), 0);
    try addMethod(ctx, allocator, pool, result_proto, root_class_idx, .map, wrap(result.resultMap), 1);
    try addMethod(ctx, allocator, pool, result_proto, root_class_idx, .mapErr, wrap(result.resultMapErr), 1);
    try addMethodDynamic(ctx, result_proto, "match", wrap(result.resultMatch), 1);
    const result_obj = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethod(ctx, allocator, pool, result_obj, root_class_idx, .ok, wrap(result.resultOk), 1);
    try addMethod(ctx, allocator, pool, result_obj, root_class_idx, .err, wrap(result.resultErr), 1);
    try ctx.setPropertyChecked(result_obj, .prototype, result_proto.toValue());
    try ctx.builtin_objects.append(allocator, result_obj);
    try ctx.setGlobal(.Result, result_obj.toValue());
    ctx.result_prototype = result_proto;
}

// Run all sub-module tests
test {
    std.testing.refAllDecls(@This());
}
