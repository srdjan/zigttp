#ifndef MQJS_STDLIB_BINDINGS_H
#define MQJS_STDLIB_BINDINGS_H

#include <stddef.h>
#include "mquickjs.h"

#ifdef __cplusplus
extern "C" {
#endif

JSValue js_print(JSContext *ctx, JSValue *this_val, int argc, JSValue *argv);
JSValue js_gc(JSContext *ctx, JSValue *this_val, int argc, JSValue *argv);
JSValue js_date_now(JSContext *ctx, JSValue *this_val, int argc, JSValue *argv);
JSValue js_performance_now(JSContext *ctx, JSValue *this_val, int argc, JSValue *argv);
JSValue js_load(JSContext *ctx, JSValue *this_val, int argc, JSValue *argv);
JSValue js_setTimeout(JSContext *ctx, JSValue *this_val, int argc, JSValue *argv);
JSValue js_clearTimeout(JSContext *ctx, JSValue *this_val, int argc, JSValue *argv);

#ifdef __cplusplus
}
#endif

#include "mqjs_stdlib.h"

#endif /* MQJS_STDLIB_BINDINGS_H */
