#ifndef MQJS_STDLIB_DECL_H
#define MQJS_STDLIB_DECL_H

#include "mquickjs.h"

#ifdef __cplusplus
extern "C" {
#endif

extern const JSSTDLibraryDef *mqjs_stdlib;
extern const JSCFunctionDef *mqjs_c_function_table;
extern const JSCFinalizer *mqjs_c_finalizer_table;
extern const size_t mqjs_c_function_table_len;

#ifdef __cplusplus
}
#endif

#endif /* MQJS_STDLIB_DECL_H */
