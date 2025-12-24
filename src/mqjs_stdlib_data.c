#include "mqjs_stdlib_bindings.h"

const JSCFunctionDef *mqjs_c_function_table = js_c_function_table;
const size_t mqjs_c_function_table_len = sizeof(js_c_function_table) / sizeof(js_c_function_table[0]);
const JSCFinalizer *mqjs_c_finalizer_table = js_c_finalizer_table;
const JSSTDLibraryDef *mqjs_stdlib = &js_stdlib;
