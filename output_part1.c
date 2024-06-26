/*
 * QuickJS Javascript Engine
 * 
 * Copyright (c) 2017-2021 Fabrice Bellard
 * Copyright (c) 2017-2021 Charlie Gordon
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <inttypes.h>
#include <string.h>
#include <assert.h>
#include <sys/time.h>
#include <time.h>
#include <fenv.h>
#include <math.h>
#if defined(__APPLE__)
#include <malloc/malloc.h>
#elif defined(__linux__)
#include <malloc.h>
#elif defined(__FreeBSD__)
#include <malloc_np.h>
#endif

#include "cutils.h"
#include "list.h"
#include "quickjs.h"
#include "libregexp.h"
#ifdef CONFIG_BIGNUM
#include "libbf.h"
#endif

#define OPTIMIZE         1
#define SHORT_OPCODES    1
#if defined(EMSCRIPTEN)
#define DIRECT_DISPATCH  0
#else
#define DIRECT_DISPATCH  1
#endif

#if defined(__APPLE__)
#define MALLOC_OVERHEAD  0
#else
#define MALLOC_OVERHEAD  8
#endif

#if !defined(_WIN32)
/* define it if printf uses the RNDN rounding mode instead of RNDNA */
#define CONFIG_PRINTF_RNDN
#endif

/* define to include Atomics.* operations which depend on the OS
   threads */
#if !defined(EMSCRIPTEN)
#define CONFIG_ATOMICS
#endif

#if !defined(EMSCRIPTEN)
/* enable stack limitation */
#define CONFIG_STACK_CHECK
#endif


/* dump object free */
//#define DUMP_FREE
//#define DUMP_CLOSURE
/* dump the bytecode of the compiled functions: combination of bits
   1: dump pass 3 final byte code
   2: dump pass 2 code
   4: dump pass 1 code
   8: dump stdlib functions
  16: dump bytecode in hex
  32: dump line number table
 */
//#define DUMP_BYTECODE  (1)
/* dump the occurence of the automatic GC */
//#define DUMP_GC
/* dump objects freed by the garbage collector */
//#define DUMP_GC_FREE
/* dump objects leaking when freeing the runtime */
//#define DUMP_LEAKS  1
/* dump memory usage before running the garbage collector */
//#define DUMP_MEM
//#define DUMP_OBJECTS    /* dump objects in JS_FreeContext */
//#define DUMP_ATOMS      /* dump atoms in JS_FreeContext */
//#define DUMP_SHAPES     /* dump shapes in JS_FreeContext */
//#define DUMP_MODULE_RESOLVE
//#define DUMP_PROMISE
//#define DUMP_READ_OBJECT

/* test the GC by forcing it before each object allocation */
//#define FORCE_GC_AT_MALLOC

#ifdef CONFIG_ATOMICS
#include <pthread.h>
#include <stdatomic.h>
#include <errno.h>
#endif

enum {
    /* classid tag        */    /* union usage   | properties */
    JS_CLASS_OBJECT = 1,        /* must be first */
    JS_CLASS_ARRAY,             /* u.array       | length */
    JS_CLASS_ERROR,
    JS_CLASS_NUMBER,            /* u.object_data */
    JS_CLASS_STRING,            /* u.object_data */
    JS_CLASS_BOOLEAN,           /* u.object_data */
    JS_CLASS_SYMBOL,            /* u.object_data */
    JS_CLASS_ARGUMENTS,         /* u.array       | length */
    JS_CLASS_MAPPED_ARGUMENTS,  /*               | length */
    JS_CLASS_DATE,              /* u.object_data */
    JS_CLASS_MODULE_NS,
    JS_CLASS_C_FUNCTION,        /* u.cfunc */
    JS_CLASS_BYTECODE_FUNCTION, /* u.func */
    JS_CLASS_BOUND_FUNCTION,    /* u.bound_function */
    JS_CLASS_C_FUNCTION_DATA,   /* u.c_function_data_record */
    JS_CLASS_GENERATOR_FUNCTION, /* u.func */
    JS_CLASS_FOR_IN_ITERATOR,   /* u.for_in_iterator */
    JS_CLASS_REGEXP,            /* u.regexp */
    JS_CLASS_ARRAY_BUFFER,      /* u.array_buffer */
    JS_CLASS_SHARED_ARRAY_BUFFER, /* u.array_buffer */
    JS_CLASS_UINT8C_ARRAY,      /* u.array (typed_array) */
    JS_CLASS_INT8_ARRAY,        /* u.array (typed_array) */
    JS_CLASS_UINT8_ARRAY,       /* u.array (typed_array) */
    JS_CLASS_INT16_ARRAY,       /* u.array (typed_array) */
    JS_CLASS_UINT16_ARRAY,      /* u.array (typed_array) */
    JS_CLASS_INT32_ARRAY,       /* u.array (typed_array) */
    JS_CLASS_UINT32_ARRAY,      /* u.array (typed_array) */
#ifdef CONFIG_BIGNUM
    JS_CLASS_BIG_INT64_ARRAY,   /* u.array (typed_array) */
    JS_CLASS_BIG_UINT64_ARRAY,  /* u.array (typed_array) */
#endif
    JS_CLASS_FLOAT32_ARRAY,     /* u.array (typed_array) */
    JS_CLASS_FLOAT64_ARRAY,     /* u.array (typed_array) */
    JS_CLASS_DATAVIEW,          /* u.typed_array */
#ifdef CONFIG_BIGNUM
    JS_CLASS_BIG_INT,           /* u.object_data */
    JS_CLASS_BIG_FLOAT,         /* u.object_data */
    JS_CLASS_FLOAT_ENV,         /* u.float_env */
    JS_CLASS_BIG_DECIMAL,       /* u.object_data */
    JS_CLASS_OPERATOR_SET,      /* u.operator_set */
#endif
    JS_CLASS_MAP,               /* u.map_state */
    JS_CLASS_SET,               /* u.map_state */
    JS_CLASS_WEAKMAP,           /* u.map_state */
    JS_CLASS_WEAKSET,           /* u.map_state */
    JS_CLASS_MAP_ITERATOR,      /* u.map_iterator_data */
    JS_CLASS_SET_ITERATOR,      /* u.map_iterator_data */
    JS_CLASS_ARRAY_ITERATOR,    /* u.array_iterator_data */
    JS_CLASS_STRING_ITERATOR,   /* u.array_iterator_data */
    JS_CLASS_REGEXP_STRING_ITERATOR,   /* u.regexp_string_iterator_data */
    JS_CLASS_GENERATOR,         /* u.generator_data */
    JS_CLASS_PROXY,             /* u.proxy_data */
    JS_CLASS_PROMISE,           /* u.promise_data */
    JS_CLASS_PROMISE_RESOLVE_FUNCTION,  /* u.promise_function_data */
    JS_CLASS_PROMISE_REJECT_FUNCTION,   /* u.promise_function_data */
    JS_CLASS_ASYNC_FUNCTION,            /* u.func */
    JS_CLASS_ASYNC_FUNCTION_RESOLVE,    /* u.async_function_data */
    JS_CLASS_ASYNC_FUNCTION_REJECT,     /* u.async_function_data */
    JS_CLASS_ASYNC_FROM_SYNC_ITERATOR,  /* u.async_from_sync_iterator_data */
    JS_CLASS_ASYNC_GENERATOR_FUNCTION,  /* u.func */
    JS_CLASS_ASYNC_GENERATOR,   /* u.async_generator_data */

    JS_CLASS_INIT_COUNT, /* last entry for predefined classes */
};

/* number of typed array types */
#define JS_TYPED_ARRAY_COUNT  (JS_CLASS_FLOAT64_ARRAY - JS_CLASS_UINT8C_ARRAY + 1)
static uint8_t const typed_array_size_log2[JS_TYPED_ARRAY_COUNT];
#define typed_array_size_log2(classid)  (typed_array_size_log2[(classid)- JS_CLASS_UINT8C_ARRAY])

typedef enum JSErrorEnum {
    JS_EVAL_ERROR,
    JS_RANGE_ERROR,
    JS_REFERENCE_ERROR,
    JS_SYNTAX_ERROR,
    JS_TYPE_ERROR,
    JS_URI_ERROR,
    JS_INTERNAL_ERROR,
    JS_AGGREGATE_ERROR,
    
    JS_NATIVE_ERROR_COUNT, /* number of different NativeError objects */
} JSErrorEnum;

#define JS_MAX_LOCAL_VARS 65536
#define JS_STACK_SIZE_MAX 65534
#define JS_STRING_LEN_MAX ((1 << 30) - 1)

#define __exception __attribute__((warn_unused_result))

typedef struct JSShape JSShape;
typedef struct JSString JSString;
typedef struct JSString JSAtomStruct;

typedef enum {
    JS_GC_PHASE_NONE,
    JS_GC_PHASE_DECREF,
    JS_GC_PHASE_REMOVE_CYCLES,
} JSGCPhaseEnum;

typedef enum OPCodeEnum OPCodeEnum;

#ifdef CONFIG_BIGNUM
/* function pointers are used for numeric operations so that it is
   possible to remove some numeric types */
typedef struct {
    JSValue (*to_string)(JSContext *ctx, JSValueConst val);
    JSValue (*from_string)(JSContext *ctx, const char *buf,
                           int radix, int flags, slimb_t *pexponent);
    int (*unary_arith)(JSContext *ctx,
                       JSValue *pres, OPCodeEnum op, JSValue op1);
    int (*binary_arith)(JSContext *ctx, OPCodeEnum op,
                        JSValue *pres, JSValue op1, JSValue op2);
    int (*compare)(JSContext *ctx, OPCodeEnum op,
                   JSValue op1, JSValue op2);
    /* only for bigfloat: */
    JSValue (*mul_pow10_to_float64)(JSContext *ctx, const bf_t *a,
                                    int64_t exponent);
    int (*mul_pow10)(JSContext *ctx, JSValue *sp);
} JSNumericOperations;
#endif

struct JSRuntime {
    JSMallocFunctions mf;
    JSMallocState malloc_state;
    const char *rt_info;

    int atom_hash_size; /* power of two */
    int atom_count;
    int atom_size;
    int atom_count_resize; /* resize hash table at this count */
    uint32_t *atom_hash;
    JSAtomStruct **atom_array;
    int atom_free_index; /* 0 = none */

    int class_count;    /* size of class_array */
    JSClass *class_array;

    struct list_head context_list; /* list of JSContext.link */
    /* list of JSGCObjectHeader.link. List of allocated GC objects (used
       by the garbage collector) */
    struct list_head gc_obj_list;
    /* list of JSGCObjectHeader.link. Used during JS_FreeValueRT() */
    struct list_head gc_zero_ref_count_list; 
    struct list_head tmp_obj_list; /* used during GC */
    JSGCPhaseEnum gc_phase : 8;
    size_t malloc_gc_threshold;
#ifdef DUMP_LEAKS
    struct list_head string_list; /* list of JSString.link */
#endif
    /* stack limitation */
    uintptr_t stack_size; /* in bytes, 0 if no limit */
    uintptr_t stack_top;
    uintptr_t stack_limit; /* lower stack limit */
    
    JSValue current_exception;
    /* true if inside an out of memory error, to avoid recursing */
    BOOL in_out_of_memory : 8;

    struct JSStackFrame *current_stack_frame;

    JSInterruptHandler *interrupt_handler;
    void *interrupt_opaque;

    JSHostPromiseRejectionTracker *host_promise_rejection_tracker;
    void *host_promise_rejection_tracker_opaque;
    
    struct list_head job_list; /* list of JSJobEntry.link */

    JSModuleNormalizeFunc *module_normalize_func;
    JSModuleLoaderFunc *module_loader_func;
    void *module_loader_opaque;

    BOOL can_block : 8; /* TRUE if Atomics.wait can block */
    /* used to allocate, free and clone SharedArrayBuffers */
    JSSharedArrayBufferFunctions sab_funcs;
    
    /* Shape hash table */
    int shape_hash_bits;
    int shape_hash_size;
    int shape_hash_count; /* number of hashed shapes */
    JSShape **shape_hash;
#ifdef CONFIG_BIGNUM
    bf_context_t bf_ctx;
    JSNumericOperations bigint_ops;
    JSNumericOperations bigfloat_ops;
    JSNumericOperations bigdecimal_ops;
    uint32_t operator_count;
#endif
    void *user_opaque;
};

struct JSClass {
    uint32_t class_id; /* 0 means free entry */
    JSAtom class_name;
    JSClassFinalizer *finalizer;
    JSClassGCMark *gc_mark;
    JSClassCall *call;
    /* pointers for exotic behavior, can be NULL if none are present */
    const JSClassExoticMethods *exotic;
};

#define JS_MODE_STRICT (1 << 0)
#define JS_MODE_STRIP  (1 << 1)
#define JS_MODE_MATH   (1 << 2)

typedef struct JSStackFrame {
    struct JSStackFrame *prev_frame; /* NULL if first stack frame */
    JSValue cur_func; /* current function, JS_UNDEFINED if the frame is detached */
    JSValue *arg_buf; /* arguments */
    JSValue *var_buf; /* variables */
    struct list_head var_ref_list; /* list of JSVarRef.link */
    const uint8_t *cur_pc; /* only used in bytecode functions : PC of the
                        instruction after the call */
    int arg_count;
    int js_mode; /* 0 or JS_MODE_MATH for C functions */
    /* only used in generators. Current stack pointer value. NULL if
       the function is running. */ 
    JSValue *cur_sp;
} JSStackFrame;

typedef enum {
    JS_GC_OBJ_TYPE_JS_OBJECT,
    JS_GC_OBJ_TYPE_FUNCTION_BYTECODE,
    JS_GC_OBJ_TYPE_SHAPE,
    JS_GC_OBJ_TYPE_VAR_REF,
    JS_GC_OBJ_TYPE_ASYNC_FUNCTION,
    JS_GC_OBJ_TYPE_JS_CONTEXT,
} JSGCObjectTypeEnum;

/* header for GC objects. GC objects are C data structures with a
   reference count that can reference other GC objects. JS Objects are
   a particular type of GC object. */
struct JSGCObjectHeader {
    int ref_count; /* must come first, 32-bit */
    JSGCObjectTypeEnum gc_obj_type : 4;
    uint8_t mark : 4; /* used by the GC */
    uint8_t dummy1; /* not used by the GC */
    uint16_t dummy2; /* not used by the GC */
    struct list_head link;
};

typedef struct JSVarRef {
    union {
        JSGCObjectHeader header; /* must come first */
        struct {
            int __gc_ref_count; /* corresponds to header.ref_count */
            uint8_t __gc_mark; /* corresponds to header.mark/gc_obj_type */

            /* 0 : the JSVarRef is on the stack. header.link is an element
               of JSStackFrame.var_ref_list.
               1 : the JSVarRef is detached. header.link has the normal meanning 
            */
            uint8_t is_detached : 1; 
            uint8_t is_arg : 1;
            uint16_t var_idx; /* index of the corresponding function variable on
                                 the stack */
        };
    };
    JSValue *pvalue; /* pointer to the value, either on the stack or
                        to 'value' */
    JSValue value; /* used when the variable is no longer on the stack */
} JSVarRef;

#ifdef CONFIG_BIGNUM
typedef struct JSFloatEnv {
    limb_t prec;
    bf_flags_t flags;
    unsigned int status;
} JSFloatEnv;

/* the same structure is used for big integers and big floats. Big
   integers are never infinite or NaNs */
typedef struct JSBigFloat {
    JSRefCountHeader header; /* must come first, 32-bit */
    bf_t num;
} JSBigFloat;

typedef struct JSBigDecimal {
    JSRefCountHeader header; /* must come first, 32-bit */
    bfdec_t num;
} JSBigDecimal;
#endif

typedef enum {
    JS_AUTOINIT_ID_PROTOTYPE,
    JS_AUTOINIT_ID_MODULE_NS,
    JS_AUTOINIT_ID_PROP,
} JSAutoInitIDEnum;

/* must be large enough to have a negligible runtime cost and small
   enough to call the interrupt callback often. */
#define JS_INTERRUPT_COUNTER_INIT 10000

struct JSContext {
    JSGCObjectHeader header; /* must come first */
    JSRuntime *rt;
    struct list_head link;

    uint16_t binary_object_count;
    int binary_object_size;

    JSShape *array_shape;   /* initial shape for Array objects */

    JSValue *class_proto;
    JSValue function_proto;
    JSValue function_ctor;
    JSValue array_ctor;
    JSValue regexp_ctor;
    JSValue promise_ctor;
    JSValue native_error_proto[JS_NATIVE_ERROR_COUNT];
    JSValue iterator_proto;
    JSValue async_iterator_proto;
    JSValue array_proto_values;
    JSValue throw_type_error;
    JSValue eval_obj;

    JSValue global_obj; /* global object */
    JSValue global_var_obj; /* contains the global let/const definitions */

    uint64_t random_state;
#ifdef CONFIG_BIGNUM
    bf_context_t *bf_ctx;   /* points to rt->bf_ctx, shared by all contexts */
    JSFloatEnv fp_env; /* global FP environment */
    BOOL bignum_ext : 8; /* enable math mode */
    BOOL allow_operator_overloading : 8;
#endif
    /* when the counter reaches zero, JSRutime.interrupt_handler is called */
    int interrupt_counter;
    BOOL is_error_property_enabled;

    struct list_head loaded_modules; /* list of JSModuleDef.link */

    /* if NULL, RegExp compilation is not supported */
    JSValue (*compile_regexp)(JSContext *ctx, JSValueConst pattern,
                              JSValueConst flags);
    /* if NULL, eval is not supported */
    JSValue (*eval_internal)(JSContext *ctx, JSValueConst this_obj,
                             const char *input, size_t input_len,
                             const char *filename, int flags, int scope_idx);
    void *user_opaque;
};

typedef union JSFloat64Union {
    double d;
    uint64_t u64;
    uint32_t u32[2];
} JSFloat64Union;

enum {
    JS_ATOM_TYPE_STRING = 1,
    JS_ATOM_TYPE_GLOBAL_SYMBOL,
    JS_ATOM_TYPE_SYMBOL,
    JS_ATOM_TYPE_PRIVATE,
};

enum {
    JS_ATOM_HASH_SYMBOL,
    JS_ATOM_HASH_PRIVATE,
};

typedef enum {
    JS_ATOM_KIND_STRING,
    JS_ATOM_KIND_SYMBOL,
    JS_ATOM_KIND_PRIVATE,
} JSAtomKindEnum;

#define JS_ATOM_HASH_MASK  ((1 << 30) - 1)

struct JSString {
    JSRefCountHeader header; /* must come first, 32-bit */
    uint32_t len : 31;
    uint8_t is_wide_char : 1; /* 0 = 8 bits, 1 = 16 bits characters */
    /* for JS_ATOM_TYPE_SYMBOL: hash = 0, atom_type = 3,
       for JS_ATOM_TYPE_PRIVATE: hash = 1, atom_type = 3
       XXX: could change encoding to have one more bit in hash */
    uint32_t hash : 30;
    uint8_t atom_type : 2; /* != 0 if atom, JS_ATOM_TYPE_x */
    uint32_t hash_next; /* atom_index for JS_ATOM_TYPE_SYMBOL */
#ifdef DUMP_LEAKS
    struct list_head link; /* string list */
#endif
    union {
        uint8_t str8[0]; /* 8 bit strings will get an extra null terminator */
        uint16_t str16[0];
    } u;
};

typedef struct JSClosureVar {
    uint8_t is_local : 1;
    uint8_t is_arg : 1;
    uint8_t is_const : 1;
    uint8_t is_lexical : 1;
    uint8_t var_kind : 4; /* see JSVarKindEnum */
    /* 8 bits available */
    uint16_t var_idx; /* is_local = TRUE: index to a normal variable of the
                    parent function. otherwise: index to a closure
                    variable of the parent function */
    JSAtom var_name;
} JSClosureVar;

#define ARG_SCOPE_INDEX 1
#define ARG_SCOPE_END (-2)

typedef struct JSVarScope {
    int parent;  /* index into fd->scopes of the enclosing scope */
    int first;   /* index into fd->vars of the last variable in this scope */
} JSVarScope;

typedef enum {
    /* XXX: add more variable kinds here instead of using bit fields */
    JS_VAR_NORMAL,
    JS_VAR_FUNCTION_DECL, /* lexical var with function declaration */
    JS_VAR_NEW_FUNCTION_DECL, /* lexical var with async/generator
                                 function declaration */
    JS_VAR_CATCH,
    JS_VAR_FUNCTION_NAME, /* function expression name */
    JS_VAR_PRIVATE_FIELD,
    JS_VAR_PRIVATE_METHOD,
    JS_VAR_PRIVATE_GETTER,
    JS_VAR_PRIVATE_SETTER, /* must come after JS_VAR_PRIVATE_GETTER */
    JS_VAR_PRIVATE_GETTER_SETTER, /* must come after JS_VAR_PRIVATE_SETTER */
} JSVarKindEnum;

/* XXX: could use a different structure in bytecode functions to save
   memory */
typedef struct JSVarDef {
    JSAtom var_name;
    /* index into fd->scopes of this variable lexical scope */
    int scope_level;
    /* during compilation: 
        - if scope_level = 0: scope in which the variable is defined
        - if scope_level != 0: index into fd->vars of the next
          variable in the same or enclosing lexical scope
       in a bytecode function:
       index into fd->vars of the next
       variable in the same or enclosing lexical scope
    */
    int scope_next;    
    uint8_t is_const : 1;
    uint8_t is_lexical : 1;
    uint8_t is_captured : 1;
    uint8_t var_kind : 4; /* see JSVarKindEnum */
    /* only used during compilation: function pool index for lexical
       variables with var_kind =
       JS_VAR_FUNCTION_DECL/JS_VAR_NEW_FUNCTION_DECL or scope level of
       the definition of the 'var' variables (they have scope_level =
       0) */
    int func_pool_idx : 24; /* only used during compilation : index in
                               the constant pool for hoisted function
                               definition */
} JSVarDef;

/* for the encoding of the pc2line table */
#define PC2LINE_BASE     (-1)
#define PC2LINE_RANGE    5
#define PC2LINE_OP_FIRST 1
#define PC2LINE_DIFF_PC_MAX ((255 - PC2LINE_OP_FIRST) / PC2LINE_RANGE)

typedef enum JSFunctionKindEnum {
    JS_FUNC_NORMAL = 0,
    JS_FUNC_GENERATOR = (1 << 0),
    JS_FUNC_ASYNC = (1 << 1),
    JS_FUNC_ASYNC_GENERATOR = (JS_FUNC_GENERATOR | JS_FUNC_ASYNC),
} JSFunctionKindEnum;

typedef struct JSFunctionBytecode {
    JSGCObjectHeader header; /* must come first */
    uint8_t js_mode;
    uint8_t has_prototype : 1; /* true if a prototype field is necessary */
    uint8_t has_simple_parameter_list : 1;
    uint8_t is_derived_class_constructor : 1;
    /* true if home_object needs to be initialized */
    uint8_t need_home_object : 1;
    uint8_t func_kind : 2;
    uint8_t new_target_allowed : 1;
    uint8_t super_call_allowed : 1;
    uint8_t super_allowed : 1;
    uint8_t arguments_allowed : 1;
    uint8_t has_debug : 1;
    uint8_t backtrace_barrier : 1; /* stop backtrace on this function */
    uint8_t read_only_bytecode : 1;
    /* XXX: 4 bits available */
    uint8_t *byte_code_buf; /* (self pointer) */
    int byte_code_len;
    JSAtom func_name;
    JSVarDef *vardefs; /* arguments + local variables (arg_count + var_count) (self pointer) */
    JSClosureVar *closure_var; /* list of variables in the closure (self pointer) */
    uint16_t arg_count;
    uint16_t var_count;
    uint16_t defined_arg_count; /* for length function property */
    uint16_t stack_size; /* maximum stack size */
    JSContext *realm; /* function realm */
    JSValue *cpool; /* constant pool (self pointer) */
    int cpool_count;
    int closure_var_count;
    struct {
        /* debug info, move to separate structure to save memory? */
        JSAtom filename;
        int line_num;
        int source_len;
        int pc2line_len;
        uint8_t *pc2line_buf;
        char *source;
    } debug;
} JSFunctionBytecode;

typedef struct JSBoundFunction {
    JSValue func_obj;
    JSValue this_val;
    int argc;
    JSValue argv[0];
} JSBoundFunction;

typedef enum JSIteratorKindEnum {
    JS_ITERATOR_KIND_KEY,
    JS_ITERATOR_KIND_VALUE,
    JS_ITERATOR_KIND_KEY_AND_VALUE,
} JSIteratorKindEnum;

typedef struct JSForInIterator {
    JSValue obj;
    BOOL is_array;
    uint32_t array_length;
    uint32_t idx;
} JSForInIterator;

typedef struct JSRegExp {
    JSString *pattern;
    JSString *bytecode; /* also contains the flags */
} JSRegExp;

typedef struct JSProxyData {
    JSValue target;
    JSValue handler;
    uint8_t is_func;
    uint8_t is_revoked;
} JSProxyData;

typedef struct JSArrayBuffer {
    int byte_length; /* 0 if detached */
    uint8_t detached;
    uint8_t shared; /* if shared, the array buffer cannot be detached */
    uint8_t *data; /* NULL if detached */
    struct list_head array_list;
    void *opaque;
    JSFreeArrayBufferDataFunc *free_func;
} JSArrayBuffer;

typedef struct JSTypedArray {
    struct list_head link; /* link to arraybuffer */
    JSObject *obj; /* back pointer to the TypedArray/DataView object */
    JSObject *buffer; /* based array buffer */
    uint32_t offset; /* offset in the array buffer */
    uint32_t length; /* length in the array buffer */
} JSTypedArray;

typedef struct JSAsyncFunctionState {
    JSValue this_val; /* 'this' generator argument */
    int argc; /* number of function arguments */
    BOOL throw_flag; /* used to throw an exception in JS_CallInternal() */
    JSStackFrame frame;
} JSAsyncFunctionState;

/* XXX: could use an object instead to avoid the
   JS_TAG_ASYNC_FUNCTION tag for the GC */
typedef struct JSAsyncFunctionData {
    JSGCObjectHeader header; /* must come first */
    JSValue resolving_funcs[2];
    BOOL is_active; /* true if the async function state is valid */
    JSAsyncFunctionState func_state;
} JSAsyncFunctionData;

typedef enum {
   /* binary operators */
   JS_OVOP_ADD,
   JS_OVOP_SUB,
   JS_OVOP_MUL,
   JS_OVOP_DIV,
   JS_OVOP_MOD,
   JS_OVOP_POW,
   JS_OVOP_OR,
   JS_OVOP_AND,
   JS_OVOP_XOR,
   JS_OVOP_SHL,
   JS_OVOP_SAR,
   JS_OVOP_SHR,
   JS_OVOP_EQ,
   JS_OVOP_LESS,

   JS_OVOP_BINARY_COUNT,
   /* unary operators */
   JS_OVOP_POS = JS_OVOP_BINARY_COUNT,
   JS_OVOP_NEG,
   JS_OVOP_INC,
   JS_OVOP_DEC,
   JS_OVOP_NOT,

   JS_OVOP_COUNT,
} JSOverloadableOperatorEnum;

typedef struct {
    uint32_t operator_index;
    JSObject *ops[JS_OVOP_BINARY_COUNT]; /* self operators */
} JSBinaryOperatorDefEntry;

typedef struct {
    int count;
    JSBinaryOperatorDefEntry *tab;
} JSBinaryOperatorDef;

typedef struct {
    uint32_t operator_counter;
    BOOL is_primitive; /* OperatorSet for a primitive type */
    /* NULL if no operator is defined */
    JSObject *self_ops[JS_OVOP_COUNT]; /* self operators */
    JSBinaryOperatorDef left;
    JSBinaryOperatorDef right;
} JSOperatorSetData;

typedef struct JSReqModuleEntry {
    JSAtom module_name;
    JSModuleDef *module; /* used using resolution */
} JSReqModuleEntry;

typedef enum JSExportTypeEnum {
    JS_EXPORT_TYPE_LOCAL,
    JS_EXPORT_TYPE_INDIRECT,
} JSExportTypeEnum;

typedef struct JSExportEntry {
    union {
        struct {
            int var_idx; /* closure variable index */
            JSVarRef *var_ref; /* if != NULL, reference to the variable */
        } local; /* for local export */
        int req_module_idx; /* module for indirect export */
    } u;
    JSExportTypeEnum export_type;
    JSAtom local_name; /* '*' if export ns from. not used for local
                          export after compilation */
    JSAtom export_name; /* exported variable name */
} JSExportEntry;

typedef struct JSStarExportEntry {
    int req_module_idx; /* in req_module_entries */
} JSStarExportEntry;

typedef struct JSImportEntry {
    int var_idx; /* closure variable index */
    JSAtom import_name;
    int req_module_idx; /* in req_module_entries */
} JSImportEntry;

struct JSModuleDef {
    JSRefCountHeader header; /* must come first, 32-bit */
    JSAtom module_name;
    struct list_head link;

    JSReqModuleEntry *req_module_entries;
    int req_module_entries_count;
    int req_module_entries_size;

    JSExportEntry *export_entries;
    int export_entries_count;
    int export_entries_size;

    JSStarExportEntry *star_export_entries;
    int star_export_entries_count;
    int star_export_entries_size;

    JSImportEntry *import_entries;
    int import_entries_count;
    int import_entries_size;

    JSValue module_ns;
    JSValue func_obj; /* only used for JS modules */
    JSModuleInitFunc *init_func; /* only used for C modules */
    BOOL resolved : 8;
    BOOL func_created : 8;
    BOOL instantiated : 8;
    BOOL evaluated : 8;
    BOOL eval_mark : 8; /* temporary use during js_evaluate_module() */
    /* true if evaluation yielded an exception. It is saved in
       eval_exception */
    BOOL eval_has_exception : 8; 
    JSValue eval_exception;
    JSValue meta_obj; /* for import.meta */
};

typedef struct JSJobEntry {
    struct list_head link;
    JSContext *ctx;
    JSJobFunc *job_func;
    int argc;
    JSValue argv[0];
} JSJobEntry;

typedef struct JSProperty {
    union {
        JSValue value;      /* JS_PROP_NORMAL */
        struct {            /* JS_PROP_GETSET */
            JSObject *getter; /* NULL if undefined */
            JSObject *setter; /* NULL if undefined */
        } getset;
        JSVarRef *var_ref;  /* JS_PROP_VARREF */
        struct {            /* JS_PROP_AUTOINIT */
            /* in order to use only 2 pointers, we compress the realm
               and the init function pointer */
            uintptr_t realm_and_id; /* realm and init_id (JS_AUTOINIT_ID_x)
                                       in the 2 low bits */
            void *opaque;
        } init;
    } u;
} JSProperty;

#define JS_PROP_INITIAL_SIZE 2
#define JS_PROP_INITIAL_HASH_SIZE 4 /* must be a power of two */
#define JS_ARRAY_INITIAL_SIZE 2

typedef struct JSShapeProperty {
    uint32_t hash_next : 26; /* 0 if last in list */
    uint32_t flags : 6;   /* JS_PROP_XXX */
    JSAtom atom; /* JS_ATOM_NULL = free property entry */
} JSShapeProperty;

struct JSShape {
    /* hash table of size hash_mask + 1 before the start of the
       structure (see prop_hash_end()). */
    JSGCObjectHeader header;
    /* true if the shape is inserted in the shape hash table. If not,
       JSShape.hash is not valid */
    uint8_t is_hashed;
    /* If true, the shape may have small array index properties 'n' with 0
       <= n <= 2^31-1. If false, the shape is guaranteed not to have
       small array index properties */
    uint8_t has_small_array_index;
    uint32_t hash; /* current hash value */
    uint32_t prop_hash_mask;
    int prop_size; /* allocated properties */
    int prop_count; /* include deleted properties */
    int deleted_prop_count;
    JSShape *shape_hash_next; /* in JSRuntime.shape_hash[h] list */
    JSObject *proto;
    JSShapeProperty prop[0]; /* prop_size elements */
};

struct JSObject {
    union {
        JSGCObjectHeader header;
        struct {
            int __gc_ref_count; /* corresponds to header.ref_count */
            uint8_t __gc_mark; /* corresponds to header.mark/gc_obj_type */
            
            uint8_t extensible : 1;
            uint8_t free_mark : 1; /* only used when freeing objects with cycles */
            uint8_t is_exotic : 1; /* TRUE if object has exotic property handlers */
            uint8_t fast_array : 1; /* TRUE if u.array is used for get/put (for JS_CLASS_ARRAY, JS_CLASS_ARGUMENTS and typed arrays) */
            uint8_t is_constructor : 1; /* TRUE if object is a constructor function */
            uint8_t is_uncatchable_error : 1; /* if TRUE, error is not catchable */
            uint8_t tmp_mark : 1; /* used in JS_WriteObjectRec() */
            uint8_t is_HTMLDDA : 1; /* specific annex B IsHtmlDDA behavior */
            uint16_t class_id; /* see JS_CLASS_x */
        };
    };
    /* byte offsets: 16/24 */
    JSShape *shape; /* prototype and property names + flag */
    JSProperty *prop; /* array of properties */
    /* byte offsets: 24/40 */
    struct JSMapRecord *first_weak_ref; /* XXX: use a bit and an external hash table? */
    /* byte offsets: 28/48 */
    union {
        void *opaque;
        struct JSBoundFunction *bound_function; /* JS_CLASS_BOUND_FUNCTION */
        struct JSCFunctionDataRecord *c_function_data_record; /* JS_CLASS_C_FUNCTION_DATA */
        struct JSForInIterator *for_in_iterator; /* JS_CLASS_FOR_IN_ITERATOR */
        struct JSArrayBuffer *array_buffer; /* JS_CLASS_ARRAY_BUFFER, JS_CLASS_SHARED_ARRAY_BUFFER */
        struct JSTypedArray *typed_array; /* JS_CLASS_UINT8C_ARRAY..JS_CLASS_DATAVIEW */
#ifdef CONFIG_BIGNUM
        struct JSFloatEnv *float_env; /* JS_CLASS_FLOAT_ENV */
        struct JSOperatorSetData *operator_set; /* JS_CLASS_OPERATOR_SET */
#endif
        struct JSMapState *map_state;   /* JS_CLASS_MAP..JS_CLASS_WEAKSET */
        struct JSMapIteratorData *map_iterator_data; /* JS_CLASS_MAP_ITERATOR, JS_CLASS_SET_ITERATOR */
        struct JSArrayIteratorData *array_iterator_data; /* JS_CLASS_ARRAY_ITERATOR, JS_CLASS_STRING_ITERATOR */
        struct JSRegExpStringIteratorData *regexp_string_iterator_data; /* JS_CLASS_REGEXP_STRING_ITERATOR */
        struct JSGeneratorData *generator_data; /* JS_CLASS_GENERATOR */
        struct JSProxyData *proxy_data; /* JS_CLASS_PROXY */
        struct JSPromiseData *promise_data; /* JS_CLASS_PROMISE */
        struct JSPromiseFunctionData *promise_function_data; /* JS_CLASS_PROMISE_RESOLVE_FUNCTION, JS_CLASS_PROMISE_REJECT_FUNCTION */
        struct JSAsyncFunctionData *async_function_data; /* JS_CLASS_ASYNC_FUNCTION_RESOLVE, JS_CLASS_ASYNC_FUNCTION_REJECT */
        struct JSAsyncFromSyncIteratorData *async_from_sync_iterator_data; /* JS_CLASS_ASYNC_FROM_SYNC_ITERATOR */
        struct JSAsyncGeneratorData *async_generator_data; /* JS_CLASS_ASYNC_GENERATOR */
        struct { /* JS_CLASS_BYTECODE_FUNCTION: 12/24 bytes */
            /* also used by JS_CLASS_GENERATOR_FUNCTION, JS_CLASS_ASYNC_FUNCTION and JS_CLASS_ASYNC_GENERATOR_FUNCTION */
            struct JSFunctionBytecode *function_bytecode;
            JSVarRef **var_refs;
            JSObject *home_object; /* for 'super' access */
        } func;
        struct { /* JS_CLASS_C_FUNCTION: 12/20 bytes */
            JSContext *realm;
            JSCFunctionType c_function;
            uint8_t length;
            uint8_t cproto;
            int16_t magic;
        } cfunc;
        /* array part for fast arrays and typed arrays */
        struct { /* JS_CLASS_ARRAY, JS_CLASS_ARGUMENTS, JS_CLASS_UINT8C_ARRAY..JS_CLASS_FLOAT64_ARRAY */
            union {
                uint32_t size;          /* JS_CLASS_ARRAY, JS_CLASS_ARGUMENTS */
                struct JSTypedArray *typed_array; /* JS_CLASS_UINT8C_ARRAY..JS_CLASS_FLOAT64_ARRAY */
            } u1;
            union {
                JSValue *values;        /* JS_CLASS_ARRAY, JS_CLASS_ARGUMENTS */ 
                void *ptr;              /* JS_CLASS_UINT8C_ARRAY..JS_CLASS_FLOAT64_ARRAY */
                int8_t *int8_ptr;       /* JS_CLASS_INT8_ARRAY */
                uint8_t *uint8_ptr;     /* JS_CLASS_UINT8_ARRAY, JS_CLASS_UINT8C_ARRAY */
                int16_t *int16_ptr;     /* JS_CLASS_INT16_ARRAY */
                uint16_t *uint16_ptr;   /* JS_CLASS_UINT16_ARRAY */
                int32_t *int32_ptr;     /* JS_CLASS_INT32_ARRAY */
                uint32_t *uint32_ptr;   /* JS_CLASS_UINT32_ARRAY */
                int64_t *int64_ptr;     /* JS_CLASS_INT64_ARRAY */
                uint64_t *uint64_ptr;   /* JS_CLASS_UINT64_ARRAY */
                float *float_ptr;       /* JS_CLASS_FLOAT32_ARRAY */
                double *double_ptr;     /* JS_CLASS_FLOAT64_ARRAY */
            } u;
            uint32_t count; /* <= 2^31-1. 0 for a detached typed array */
        } array;    /* 12/20 bytes */
        JSRegExp regexp;    /* JS_CLASS_REGEXP: 8/16 bytes */
        JSValue object_data;    /* for JS_SetObjectData(): 8/16/16 bytes */
    } u;
    /* byte sizes: 40/48/72 */
};
enum {
    __JS_ATOM_NULL = JS_ATOM_NULL,
#define DEF(name, str) JS_ATOM_ ## name,
#include "quickjs-atom.h"
#undef DEF
    JS_ATOM_END,
};
#define JS_ATOM_LAST_KEYWORD JS_ATOM_super
#define JS_ATOM_LAST_STRICT_KEYWORD JS_ATOM_yield

static const char js_atom_init[] =
#define DEF(name, str) str "\0"
#include "quickjs-atom.h"
#undef DEF
;

typedef enum OPCodeFormat {
#define FMT(f) OP_FMT_ ## f,
#define DEF(id, size, n_pop, n_push, f)
#include "quickjs-opcode.h"
#undef DEF
#undef FMT
} OPCodeFormat;

enum OPCodeEnum {
#define FMT(f)
#define DEF(id, size, n_pop, n_push, f) OP_ ## id,
#define def(id, size, n_pop, n_push, f)
#include "quickjs-opcode.h"
#undef def
#undef DEF
#undef FMT
    OP_COUNT, /* excluding temporary opcodes */
    /* temporary opcodes : overlap with the short opcodes */
    OP_TEMP_START = OP_nop + 1,
    OP___dummy = OP_TEMP_START - 1,
#define FMT(f)
#define DEF(id, size, n_pop, n_push, f)
#define def(id, size, n_pop, n_push, f) OP_ ## id,
#include "quickjs-opcode.h"
#undef def
#undef DEF
#undef FMT
    OP_TEMP_END,
};

static int JS_InitAtoms(JSRuntime *rt);
static JSAtom __JS_NewAtomInit(JSRuntime *rt, const char *str, int len,
                               int atom_type);
static void JS_FreeAtomStruct(JSRuntime *rt, JSAtomStruct *p);
static void free_function_bytecode(JSRuntime *rt, JSFunctionBytecode *b);
static JSValue js_call_c_function(JSContext *ctx, JSValueConst func_obj,
                                  JSValueConst this_obj,
                                  int argc, JSValueConst *argv, int flags);
static JSValue js_call_bound_function(JSContext *ctx, JSValueConst func_obj,
                                      JSValueConst this_obj,
                                      int argc, JSValueConst *argv, int flags);
static JSValue JS_CallInternal(JSContext *ctx, JSValueConst func_obj,
                               JSValueConst this_obj, JSValueConst new_target,
                               int argc, JSValue *argv, int flags);
static JSValue JS_CallConstructorInternal(JSContext *ctx,
                                          JSValueConst func_obj,
                                          JSValueConst new_target,
                                          int argc, JSValue *argv, int flags);
static JSValue JS_CallFree(JSContext *ctx, JSValue func_obj, JSValueConst this_obj,
                           int argc, JSValueConst *argv);
static JSValue JS_InvokeFree(JSContext *ctx, JSValue this_val, JSAtom atom,
                             int argc, JSValueConst *argv);
static __exception int JS_ToArrayLengthFree(JSContext *ctx, uint32_t *plen,
                                            JSValue val, BOOL is_array_ctor);
static JSValue JS_EvalObject(JSContext *ctx, JSValueConst this_obj,
                             JSValueConst val, int flags, int scope_idx);
JSValue __attribute__((format(printf, 2, 3))) JS_ThrowInternalError(JSContext *ctx, const char *fmt, ...);
static __maybe_unused void JS_DumpAtoms(JSRuntime *rt);
static __maybe_unused void JS_DumpString(JSRuntime *rt,
                                                  const JSString *p);
static __maybe_unused void JS_DumpObjectHeader(JSRuntime *rt);
static __maybe_unused void JS_DumpObject(JSRuntime *rt, JSObject *p);
static __maybe_unused void JS_DumpGCObject(JSRuntime *rt, JSGCObjectHeader *p);
static __maybe_unused void JS_DumpValueShort(JSRuntime *rt,
                                                      JSValueConst val);
static __maybe_unused void JS_DumpValue(JSContext *ctx, JSValueConst val);
static __maybe_unused void JS_PrintValue(JSContext *ctx,
                                                  const char *str,
                                                  JSValueConst val);
static __maybe_unused void JS_DumpShapes(JSRuntime *rt);
static JSValue js_function_apply(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv, int magic);
static void js_array_finalizer(JSRuntime *rt, JSValue val);
static void js_array_mark(JSRuntime *rt, JSValueConst val,
                          JS_MarkFunc *mark_func);
static void js_object_data_finalizer(JSRuntime *rt, JSValue val);
static void js_object_data_mark(JSRuntime *rt, JSValueConst val,
                                JS_MarkFunc *mark_func);
static void js_c_function_finalizer(JSRuntime *rt, JSValue val);
static void js_c_function_mark(JSRuntime *rt, JSValueConst val,
                               JS_MarkFunc *mark_func);
static void js_bytecode_function_finalizer(JSRuntime *rt, JSValue val);
static void js_bytecode_function_mark(JSRuntime *rt, JSValueConst val,
                                JS_MarkFunc *mark_func);
static void js_bound_function_finalizer(JSRuntime *rt, JSValue val);
static void js_bound_function_mark(JSRuntime *rt, JSValueConst val,
                                JS_MarkFunc *mark_func);
static void js_for_in_iterator_finalizer(JSRuntime *rt, JSValue val);
static void js_for_in_iterator_mark(JSRuntime *rt, JSValueConst val,
                                JS_MarkFunc *mark_func);
static void js_regexp_finalizer(JSRuntime *rt, JSValue val);
static void js_array_buffer_finalizer(JSRuntime *rt, JSValue val);
static void js_typed_array_finalizer(JSRuntime *rt, JSValue val);
static void js_typed_array_mark(JSRuntime *rt, JSValueConst val,
                                JS_MarkFunc *mark_func);
static void js_proxy_finalizer(JSRuntime *rt, JSValue val);
static void js_proxy_mark(JSRuntime *rt, JSValueConst val,
                                JS_MarkFunc *mark_func);
static void js_map_finalizer(JSRuntime *rt, JSValue val);
static void js_map_mark(JSRuntime *rt, JSValueConst val,
                                JS_MarkFunc *mark_func);
static void js_map_iterator_finalizer(JSRuntime *rt, JSValue val);
static void js_map_iterator_mark(JSRuntime *rt, JSValueConst val,
                                JS_MarkFunc *mark_func);
static void js_array_iterator_finalizer(JSRuntime *rt, JSValue val);
static void js_array_iterator_mark(JSRuntime *rt, JSValueConst val,
                                JS_MarkFunc *mark_func);
static void js_regexp_string_iterator_finalizer(JSRuntime *rt, JSValue val);
static void js_regexp_string_iterator_mark(JSRuntime *rt, JSValueConst val,
                                JS_MarkFunc *mark_func);
static void js_generator_finalizer(JSRuntime *rt, JSValue obj);
static void js_generator_mark(JSRuntime *rt, JSValueConst val,
                                JS_MarkFunc *mark_func);
static void js_promise_finalizer(JSRuntime *rt, JSValue val);
static void js_promise_mark(JSRuntime *rt, JSValueConst val,
                                JS_MarkFunc *mark_func);
static void js_promise_resolve_function_finalizer(JSRuntime *rt, JSValue val);
static void js_promise_resolve_function_mark(JSRuntime *rt, JSValueConst val,
                                JS_MarkFunc *mark_func);
#ifdef CONFIG_BIGNUM
static void js_operator_set_finalizer(JSRuntime *rt, JSValue val);
static void js_operator_set_mark(JSRuntime *rt, JSValueConst val,
                                 JS_MarkFunc *mark_func);
#endif
static JSValue JS_ToStringFree(JSContext *ctx, JSValue val);
static int JS_ToBoolFree(JSContext *ctx, JSValue val);
static int JS_ToInt32Free(JSContext *ctx, int32_t *pres, JSValue val);
static int JS_ToFloat64Free(JSContext *ctx, double *pres, JSValue val);
static int JS_ToUint8ClampFree(JSContext *ctx, int32_t *pres, JSValue val);
static JSValue js_compile_regexp(JSContext *ctx, JSValueConst pattern,
                                 JSValueConst flags);
static JSValue js_regexp_constructor_internal(JSContext *ctx, JSValueConst ctor,
                                              JSValue pattern, JSValue bc);
static void gc_decref(JSRuntime *rt);
static int JS_NewClass1(JSRuntime *rt, JSClassID class_id,
                        const JSClassDef *class_def, JSAtom name);

typedef enum JSStrictEqModeEnum {
    JS_EQ_STRICT,
    JS_EQ_SAME_VALUE,
    JS_EQ_SAME_VALUE_ZERO,
} JSStrictEqModeEnum;

static BOOL js_strict_eq2(JSContext *ctx, JSValue op1, JSValue op2,
                          JSStrictEqModeEnum eq_mode);
static BOOL js_strict_eq(JSContext *ctx, JSValue op1, JSValue op2);
static BOOL js_same_value(JSContext *ctx, JSValueConst op1, JSValueConst op2);
static BOOL js_same_value_zero(JSContext *ctx, JSValueConst op1, JSValueConst op2);
static JSValue JS_ToObject(JSContext *ctx, JSValueConst val);
static JSValue JS_ToObjectFree(JSContext *ctx, JSValue val);
static JSProperty *add_property(JSContext *ctx,
                                JSObject *p, JSAtom prop, int prop_flags);
#ifdef CONFIG_BIGNUM
static void js_float_env_finalizer(JSRuntime *rt, JSValue val);
static JSValue JS_NewBigFloat(JSContext *ctx);
static inline bf_t *JS_GetBigFloat(JSValueConst val)
{
    JSBigFloat *p = JS_VALUE_GET_PTR(val);
    return &p->num;
}
static JSValue JS_NewBigDecimal(JSContext *ctx);
static inline bfdec_t *JS_GetBigDecimal(JSValueConst val)
{
    JSBigDecimal *p = JS_VALUE_GET_PTR(val);
    return &p->num;
}
static JSValue JS_NewBigInt(JSContext *ctx);
static inline bf_t *JS_GetBigInt(JSValueConst val)
{
    JSBigFloat *p = JS_VALUE_GET_PTR(val);
    return &p->num;
}
static JSValue JS_CompactBigInt1(JSContext *ctx, JSValue val,
                                 BOOL convert_to_safe_integer);
static JSValue JS_CompactBigInt(JSContext *ctx, JSValue val);
static int JS_ToBigInt64Free(JSContext *ctx, int64_t *pres, JSValue val);
static bf_t *JS_ToBigInt(JSContext *ctx, bf_t *buf, JSValueConst val);
static void JS_FreeBigInt(JSContext *ctx, bf_t *a, bf_t *buf);
static bf_t *JS_ToBigFloat(JSContext *ctx, bf_t *buf, JSValueConst val);
static JSValue JS_ToBigDecimalFree(JSContext *ctx, JSValue val,
                                   BOOL allow_null_or_undefined);
static bfdec_t *JS_ToBigDecimal(JSContext *ctx, JSValueConst val);
#endif
JSValue JS_ThrowOutOfMemory(JSContext *ctx);
static JSValue JS_ThrowTypeErrorRevokedProxy(JSContext *ctx);
static JSValue js_proxy_getPrototypeOf(JSContext *ctx, JSValueConst obj);
static int js_proxy_setPrototypeOf(JSContext *ctx, JSValueConst obj,
                                   JSValueConst proto_val, BOOL throw_flag);
static int js_proxy_isExtensible(JSContext *ctx, JSValueConst obj);
static int js_proxy_preventExtensions(JSContext *ctx, JSValueConst obj);
static int js_proxy_isArray(JSContext *ctx, JSValueConst obj);
static int JS_CreateProperty(JSContext *ctx, JSObject *p,
                             JSAtom prop, JSValueConst val,
                             JSValueConst getter, JSValueConst setter,
                             int flags);
static int js_string_memcmp(const JSString *p1, const JSString *p2, int len);
static void reset_weak_ref(JSRuntime *rt, JSObject *p);
static JSValue js_array_buffer_constructor3(JSContext *ctx,
                                            JSValueConst new_target,
                                            uint64_t len, JSClassID class_id,
                                            uint8_t *buf,
                                            JSFreeArrayBufferDataFunc *free_func,
                                            void *opaque, BOOL alloc_flag);
static JSArrayBuffer *js_get_array_buffer(JSContext *ctx, JSValueConst obj);
static JSValue js_typed_array_constructor(JSContext *ctx,
                                          JSValueConst this_val,
                                          int argc, JSValueConst *argv,
                                          int classid);
static BOOL typed_array_is_detached(JSContext *ctx, JSObject *p);
static uint32_t typed_array_get_length(JSContext *ctx, JSObject *p);
static JSValue JS_ThrowTypeErrorDetachedArrayBuffer(JSContext *ctx);
static JSVarRef *get_var_ref(JSContext *ctx, JSStackFrame *sf, int var_idx,
                             BOOL is_arg);
static JSValue js_generator_function_call(JSContext *ctx, JSValueConst func_obj,
                                          JSValueConst this_obj,
                                          int argc, JSValueConst *argv,
                                          int flags);
static void js_async_function_resolve_finalizer(JSRuntime *rt, JSValue val);
static void js_async_function_resolve_mark(JSRuntime *rt, JSValueConst val,
                                           JS_MarkFunc *mark_func);
static JSValue JS_EvalInternal(JSContext *ctx, JSValueConst this_obj,
                               const char *input, size_t input_len,
                               const char *filename, int flags, int scope_idx);
static void js_free_module_def(JSContext *ctx, JSModuleDef *m);
static void js_mark_module_def(JSRuntime *rt, JSModuleDef *m,
                               JS_MarkFunc *mark_func);
static JSValue js_import_meta(JSContext *ctx);
static JSValue js_dynamic_import(JSContext *ctx, JSValueConst specifier);
static void free_var_ref(JSRuntime *rt, JSVarRef *var_ref);
static JSValue js_new_promise_capability(JSContext *ctx,
                                         JSValue *resolving_funcs,
                                         JSValueConst ctor);
static __exception int perform_promise_then(JSContext *ctx,
                                            JSValueConst promise,
                                            JSValueConst *resolve_reject,
                                            JSValueConst *cap_resolving_funcs);
static JSValue js_promise_resolve(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv, int magic);
static int js_string_compare(JSContext *ctx,
                             const JSString *p1, const JSString *p2);
static JSValue JS_ToNumber(JSContext *ctx, JSValueConst val);
static int JS_SetPropertyValue(JSContext *ctx, JSValueConst this_obj,
                               JSValue prop, JSValue val, int flags);
static int JS_NumberIsInteger(JSContext *ctx, JSValueConst val);
static BOOL JS_NumberIsNegativeOrMinusZero(JSContext *ctx, JSValueConst val);
static JSValue JS_ToNumberFree(JSContext *ctx, JSValue val);
static int JS_GetOwnPropertyInternal(JSContext *ctx, JSPropertyDescriptor *desc,
                                     JSObject *p, JSAtom prop);
static void js_free_desc(JSContext *ctx, JSPropertyDescriptor *desc);
static void async_func_mark(JSRuntime *rt, JSAsyncFunctionState *s,
                            JS_MarkFunc *mark_func);
static void JS_AddIntrinsicBasicObjects(JSContext *ctx);
static void js_free_shape(JSRuntime *rt, JSShape *sh);
static void js_free_shape_null(JSRuntime *rt, JSShape *sh);
static int js_shape_prepare_update(JSContext *ctx, JSObject *p,
                                   JSShapeProperty **pprs);
static int init_shape_hash(JSRuntime *rt);
static __exception int js_get_length32(JSContext *ctx, uint32_t *pres,
                                       JSValueConst obj);
static __exception int js_get_length64(JSContext *ctx, int64_t *pres,
                                       JSValueConst obj);
static void free_arg_list(JSContext *ctx, JSValue *tab, uint32_t len);
static JSValue *build_arg_list(JSContext *ctx, uint32_t *plen,
                               JSValueConst array_arg);
static BOOL js_get_fast_array(JSContext *ctx, JSValueConst obj,
                              JSValue **arrpp, uint32_t *countp);
static JSValue JS_CreateAsyncFromSyncIterator(JSContext *ctx,
                                              JSValueConst sync_iter);
static void js_c_function_data_finalizer(JSRuntime *rt, JSValue val);
static void js_c_function_data_mark(JSRuntime *rt, JSValueConst val,
                                    JS_MarkFunc *mark_func);
static JSValue js_c_function_data_call(JSContext *ctx, JSValueConst func_obj,
                                       JSValueConst this_val,
                                       int argc, JSValueConst *argv, int flags);
static JSAtom js_symbol_to_atom(JSContext *ctx, JSValue val);
static void add_gc_object(JSRuntime *rt, JSGCObjectHeader *h,
                          JSGCObjectTypeEnum type);
static void remove_gc_object(JSGCObjectHeader *h);
static void js_async_function_free0(JSRuntime *rt, JSAsyncFunctionData *s);
static JSValue js_instantiate_prototype(JSContext *ctx, JSObject *p, JSAtom atom, void *opaque);
static JSValue js_module_ns_autoinit(JSContext *ctx, JSObject *p, JSAtom atom,
                                 void *opaque);
static JSValue JS_InstantiateFunctionListItem2(JSContext *ctx, JSObject *p,
                                               JSAtom atom, void *opaque);
void JS_SetUncatchableError(JSContext *ctx, JSValueConst val, BOOL flag);

static const JSClassExoticMethods js_arguments_exotic_methods;
static const JSClassExoticMethods js_string_exotic_methods;
static const JSClassExoticMethods js_proxy_exotic_methods;
static const JSClassExoticMethods js_module_ns_exotic_methods;
static JSClassID js_class_id_alloc = JS_CLASS_INIT_COUNT;

static void js_trigger_gc(JSRuntime *rt, size_t size)
{
    BOOL force_gc;
#ifdef FORCE_GC_AT_MALLOC
    force_gc = TRUE;
#else
    force_gc = ((rt->malloc_state.malloc_size + size) >
                rt->malloc_gc_threshold);
#endif
    if (force_gc) {
#ifdef DUMP_GC
        printf("GC: size=%" PRIu64 "\n",
               (uint64_t)rt->malloc_state.malloc_size);
#endif
        JS_RunGC(rt);
        rt->malloc_gc_threshold = rt->malloc_state.malloc_size +
            (rt->malloc_state.malloc_size >> 1);
    }
}

static size_t js_malloc_usable_size_unknown(const void *ptr)
{
    return 0;
}

void *js_malloc_rt(JSRuntime *rt, size_t size)
{
    return rt->mf.js_malloc(&rt->malloc_state, size);
}

void js_free_rt(JSRuntime *rt, void *ptr)
{
    rt->mf.js_free(&rt->malloc_state, ptr);
}

void *js_realloc_rt(JSRuntime *rt, void *ptr, size_t size)
{
    return rt->mf.js_realloc(&rt->malloc_state, ptr, size);
}

size_t js_malloc_usable_size_rt(JSRuntime *rt, const void *ptr)
{
    return rt->mf.js_malloc_usable_size(ptr);
}

void *js_mallocz_rt(JSRuntime *rt, size_t size)
{
    void *ptr;
    ptr = js_malloc_rt(rt, size);
    if (!ptr)
        return NULL;
    return memset(ptr, 0, size);
}

#ifdef CONFIG_BIGNUM
/* called by libbf */
static void *js_bf_realloc(void *opaque, void *ptr, size_t size)
{
    JSRuntime *rt = opaque;
    return js_realloc_rt(rt, ptr, size);
}
#endif /* CONFIG_BIGNUM */

/* Throw out of memory in case of error */
void *js_malloc(JSContext *ctx, size_t size)
{
    void *ptr;
    ptr = js_malloc_rt(ctx->rt, size);
    if (unlikely(!ptr)) {
        JS_ThrowOutOfMemory(ctx);
        return NULL;
    }
    return ptr;
}

/* Throw out of memory in case of error */
void *js_mallocz(JSContext *ctx, size_t size)
{
    void *ptr;
    ptr = js_mallocz_rt(ctx->rt, size);
    if (unlikely(!ptr)) {
        JS_ThrowOutOfMemory(ctx);
        return NULL;
    }
    return ptr;
}

void js_free(JSContext *ctx, void *ptr)
{
    js_free_rt(ctx->rt, ptr);
}

/* Throw out of memory in case of error */
void *js_realloc(JSContext *ctx, void *ptr, size_t size)
{
    void *ret;
    ret = js_realloc_rt(ctx->rt, ptr, size);
    if (unlikely(!ret && size != 0)) {
        JS_ThrowOutOfMemory(ctx);
        return NULL;
    }
    return ret;
}

/* store extra allocated size in *pslack if successful */
void *js_realloc2(JSContext *ctx, void *ptr, size_t size, size_t *pslack)
{
    void *ret;
    ret = js_realloc_rt(ctx->rt, ptr, size);
    if (unlikely(!ret && size != 0)) {
        JS_ThrowOutOfMemory(ctx);
        return NULL;
    }
    if (pslack) {
        size_t new_size = js_malloc_usable_size_rt(ctx->rt, ret);
        *pslack = (new_size > size) ? new_size - size : 0;
    }
    return ret;
}

size_t js_malloc_usable_size(JSContext *ctx, const void *ptr)
{
    return js_malloc_usable_size_rt(ctx->rt, ptr);
}

/* Throw out of memory exception in case of error */
char *js_strndup(JSContext *ctx, const char *s, size_t n)
{
    char *ptr;
    ptr = js_malloc(ctx, n + 1);
    if (ptr) {
        memcpy(ptr, s, n);
        ptr[n] = '\0';
    }
    return ptr;
}

char *js_strdup(JSContext *ctx, const char *str)
{
    return js_strndup(ctx, str, strlen(str));
}

static no_inline int js_realloc_array(JSContext *ctx, void **parray,
                                      int elem_size, int *psize, int req_size)
{
    int new_size;
    size_t slack;
    void *new_array;
    /* XXX: potential arithmetic overflow */
    new_size = max_int(req_size, *psize * 3 / 2);
    new_array = js_realloc2(ctx, *parray, new_size * elem_size, &slack);
    if (!new_array)
        return -1;
    new_size += slack / elem_size;
    *psize = new_size;
    *parray = new_array;
    return 0;
}

/* resize the array and update its size if req_size > *psize */
static inline int js_resize_array(JSContext *ctx, void **parray, int elem_size,
                                  int *psize, int req_size)
{
    if (unlikely(req_size > *psize))
        return js_realloc_array(ctx, parray, elem_size, psize, req_size);
    else
        return 0;
}

static inline void js_dbuf_init(JSContext *ctx, DynBuf *s)
{
    dbuf_init2(s, ctx->rt, (DynBufReallocFunc *)js_realloc_rt);
}

static inline int is_digit(int c) {
    return c >= '0' && c <= '9';
}

typedef struct JSClassShortDef {
    JSAtom class_name;
    JSClassFinalizer *finalizer;
    JSClassGCMark *gc_mark;
} JSClassShortDef;

static JSClassShortDef const js_std_class_def[] = {
    { JS_ATOM_Object, NULL, NULL },                             /* JS_CLASS_OBJECT */
    { JS_ATOM_Array, js_array_finalizer, js_array_mark },       /* JS_CLASS_ARRAY */
    { JS_ATOM_Error, NULL, NULL }, /* JS_CLASS_ERROR */
    { JS_ATOM_Number, js_object_data_finalizer, js_object_data_mark }, /* JS_CLASS_NUMBER */
    { JS_ATOM_String, js_object_data_finalizer, js_object_data_mark }, /* JS_CLASS_STRING */
    { JS_ATOM_Boolean, js_object_data_finalizer, js_object_data_mark }, /* JS_CLASS_BOOLEAN */
    { JS_ATOM_Symbol, js_object_data_finalizer, js_object_data_mark }, /* JS_CLASS_SYMBOL */
    { JS_ATOM_Arguments, js_array_finalizer, js_array_mark },   /* JS_CLASS_ARGUMENTS */
    { JS_ATOM_Arguments, NULL, NULL },                          /* JS_CLASS_MAPPED_ARGUMENTS */
    { JS_ATOM_Date, js_object_data_finalizer, js_object_data_mark }, /* JS_CLASS_DATE */
    { JS_ATOM_Object, NULL, NULL },                             /* JS_CLASS_MODULE_NS */
    { JS_ATOM_Function, js_c_function_finalizer, js_c_function_mark }, /* JS_CLASS_C_FUNCTION */
    { JS_ATOM_Function, js_bytecode_function_finalizer, js_bytecode_function_mark }, /* JS_CLASS_BYTECODE_FUNCTION */
    { JS_ATOM_Function, js_bound_function_finalizer, js_bound_function_mark }, /* JS_CLASS_BOUND_FUNCTION */
    { JS_ATOM_Function, js_c_function_data_finalizer, js_c_function_data_mark }, /* JS_CLASS_C_FUNCTION_DATA */
    { JS_ATOM_GeneratorFunction, js_bytecode_function_finalizer, js_bytecode_function_mark },  /* JS_CLASS_GENERATOR_FUNCTION */
    { JS_ATOM_ForInIterator, js_for_in_iterator_finalizer, js_for_in_iterator_mark },      /* JS_CLASS_FOR_IN_ITERATOR */
    { JS_ATOM_RegExp, js_regexp_finalizer, NULL },                              /* JS_CLASS_REGEXP */
    { JS_ATOM_ArrayBuffer, js_array_buffer_finalizer, NULL },                   /* JS_CLASS_ARRAY_BUFFER */
    { JS_ATOM_SharedArrayBuffer, js_array_buffer_finalizer, NULL },             /* JS_CLASS_SHARED_ARRAY_BUFFER */
    { JS_ATOM_Uint8ClampedArray, js_typed_array_finalizer, js_typed_array_mark }, /* JS_CLASS_UINT8C_ARRAY */
    { JS_ATOM_Int8Array, js_typed_array_finalizer, js_typed_array_mark },       /* JS_CLASS_INT8_ARRAY */
    { JS_ATOM_Uint8Array, js_typed_array_finalizer, js_typed_array_mark },      /* JS_CLASS_UINT8_ARRAY */
    { JS_ATOM_Int16Array, js_typed_array_finalizer, js_typed_array_mark },      /* JS_CLASS_INT16_ARRAY */
    { JS_ATOM_Uint16Array, js_typed_array_finalizer, js_typed_array_mark },     /* JS_CLASS_UINT16_ARRAY */
    { JS_ATOM_Int32Array, js_typed_array_finalizer, js_typed_array_mark },      /* JS_CLASS_INT32_ARRAY */
    { JS_ATOM_Uint32Array, js_typed_array_finalizer, js_typed_array_mark },     /* JS_CLASS_UINT32_ARRAY */
#ifdef CONFIG_BIGNUM
    { JS_ATOM_BigInt64Array, js_typed_array_finalizer, js_typed_array_mark },   /* JS_CLASS_BIG_INT64_ARRAY */
    { JS_ATOM_BigUint64Array, js_typed_array_finalizer, js_typed_array_mark },  /* JS_CLASS_BIG_UINT64_ARRAY */
#endif
    { JS_ATOM_Float32Array, js_typed_array_finalizer, js_typed_array_mark },    /* JS_CLASS_FLOAT32_ARRAY */
    { JS_ATOM_Float64Array, js_typed_array_finalizer, js_typed_array_mark },    /* JS_CLASS_FLOAT64_ARRAY */
    { JS_ATOM_DataView, js_typed_array_finalizer, js_typed_array_mark },        /* JS_CLASS_DATAVIEW */
#ifdef CONFIG_BIGNUM
    { JS_ATOM_BigInt, js_object_data_finalizer, js_object_data_mark },      /* JS_CLASS_BIG_INT */
    { JS_ATOM_BigFloat, js_object_data_finalizer, js_object_data_mark },    /* JS_CLASS_BIG_FLOAT */
    { JS_ATOM_BigFloatEnv, js_float_env_finalizer, NULL },      /* JS_CLASS_FLOAT_ENV */
    { JS_ATOM_BigDecimal, js_object_data_finalizer, js_object_data_mark },    /* JS_CLASS_BIG_DECIMAL */
    { JS_ATOM_OperatorSet, js_operator_set_finalizer, js_operator_set_mark },    /* JS_CLASS_OPERATOR_SET */
#endif
    { JS_ATOM_Map, js_map_finalizer, js_map_mark },             /* JS_CLASS_MAP */
    { JS_ATOM_Set, js_map_finalizer, js_map_mark },             /* JS_CLASS_SET */
    { JS_ATOM_WeakMap, js_map_finalizer, js_map_mark },         /* JS_CLASS_WEAKMAP */
    { JS_ATOM_WeakSet, js_map_finalizer, js_map_mark },         /* JS_CLASS_WEAKSET */
    { JS_ATOM_Map_Iterator, js_map_iterator_finalizer, js_map_iterator_mark }, /* JS_CLASS_MAP_ITERATOR */
    { JS_ATOM_Set_Iterator, js_map_iterator_finalizer, js_map_iterator_mark }, /* JS_CLASS_SET_ITERATOR */
    { JS_ATOM_Array_Iterator, js_array_iterator_finalizer, js_array_iterator_mark }, /* JS_CLASS_ARRAY_ITERATOR */
    { JS_ATOM_String_Iterator, js_array_iterator_finalizer, js_array_iterator_mark }, /* JS_CLASS_STRING_ITERATOR */
    { JS_ATOM_RegExp_String_Iterator, js_regexp_string_iterator_finalizer, js_regexp_string_iterator_mark }, /* JS_CLASS_REGEXP_STRING_ITERATOR */
    { JS_ATOM_Generator, js_generator_finalizer, js_generator_mark }, /* JS_CLASS_GENERATOR */
};

static int init_class_range(JSRuntime *rt, JSClassShortDef const *tab,
                            int start, int count)
{
    JSClassDef cm_s, *cm = &cm_s;
    int i, class_id;

    for(i = 0; i < count; i++) {
        class_id = i + start;
        memset(cm, 0, sizeof(*cm));
        cm->finalizer = tab[i].finalizer;
        cm->gc_mark = tab[i].gc_mark;
        if (JS_NewClass1(rt, class_id, cm, tab[i].class_name) < 0)
            return -1;
    }
    return 0;
}

#ifdef CONFIG_BIGNUM
static JSValue JS_ThrowUnsupportedOperation(JSContext *ctx)
{
    return JS_ThrowTypeError(ctx, "unsupported operation");
}

static JSValue invalid_to_string(JSContext *ctx, JSValueConst val)
{
    return JS_ThrowUnsupportedOperation(ctx);
}

static JSValue invalid_from_string(JSContext *ctx, const char *buf,
                                   int radix, int flags, slimb_t *pexponent)
{
    return JS_NAN;
}

static int invalid_unary_arith(JSContext *ctx,
                               JSValue *pres, OPCodeEnum op, JSValue op1)
{
    JS_FreeValue(ctx, op1);
    JS_ThrowUnsupportedOperation(ctx);
    return -1;
}

static int invalid_binary_arith(JSContext *ctx, OPCodeEnum op,
                                JSValue *pres, JSValue op1, JSValue op2)
{
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    JS_ThrowUnsupportedOperation(ctx);
    return -1;
}

static JSValue invalid_mul_pow10_to_float64(JSContext *ctx, const bf_t *a,
                                            int64_t exponent)
{
    return JS_ThrowUnsupportedOperation(ctx);
}

static int invalid_mul_pow10(JSContext *ctx, JSValue *sp)
{
    JS_ThrowUnsupportedOperation(ctx);
    return -1;
}

static void set_dummy_numeric_ops(JSNumericOperations *ops)
{
    ops->to_string = invalid_to_string;
    ops->from_string = invalid_from_string;
    ops->unary_arith = invalid_unary_arith;
    ops->binary_arith = invalid_binary_arith;
    ops->mul_pow10_to_float64 = invalid_mul_pow10_to_float64;
    ops->mul_pow10 = invalid_mul_pow10;
}

#endif /* CONFIG_BIGNUM */

#if !defined(CONFIG_STACK_CHECK)
/* no stack limitation */
static inline uintptr_t js_get_stack_pointer(void)
{
    return 0;
}

static inline BOOL js_check_stack_overflow(JSRuntime *rt, size_t alloca_size)
{
    return FALSE;
}
#else
/* Note: OS and CPU dependent */
static inline uintptr_t js_get_stack_pointer(void)
{
    return (uintptr_t)__builtin_frame_address(0);
}

static inline BOOL js_check_stack_overflow(JSRuntime *rt, size_t alloca_size)
{
    uintptr_t sp;
    sp = js_get_stack_pointer() - alloca_size;
    return unlikely(sp < rt->stack_limit);
}
#endif

JSRuntime *JS_NewRuntime2(const JSMallocFunctions *mf, void *opaque)
{
    JSRuntime *rt;
    JSMallocState ms;

    memset(&ms, 0, sizeof(ms));
    ms.opaque = opaque;
    ms.malloc_limit = -1;

    rt = mf->js_malloc(&ms, sizeof(JSRuntime));
    if (!rt)
        return NULL;
    memset(rt, 0, sizeof(*rt));
    rt->mf = *mf;
    if (!rt->mf.js_malloc_usable_size) {
        /* use dummy function if none provided */
        rt->mf.js_malloc_usable_size = js_malloc_usable_size_unknown;
    }
    rt->malloc_state = ms;
    rt->malloc_gc_threshold = 256 * 1024;

#ifdef CONFIG_BIGNUM
    bf_context_init(&rt->bf_ctx, js_bf_realloc, rt);
    set_dummy_numeric_ops(&rt->bigint_ops);
    set_dummy_numeric_ops(&rt->bigfloat_ops);
    set_dummy_numeric_ops(&rt->bigdecimal_ops);
#endif

    init_list_head(&rt->context_list);
    init_list_head(&rt->gc_obj_list);
    init_list_head(&rt->gc_zero_ref_count_list);
    rt->gc_phase = JS_GC_PHASE_NONE;
    
#ifdef DUMP_LEAKS
    init_list_head(&rt->string_list);
#endif
    init_list_head(&rt->job_list);

    if (JS_InitAtoms(rt))
        goto fail;

    /* create the object, array and function classes */
    if (init_class_range(rt, js_std_class_def, JS_CLASS_OBJECT,
                         countof(js_std_class_def)) < 0)
        goto fail;
    rt->class_array[JS_CLASS_ARGUMENTS].exotic = &js_arguments_exotic_methods;
    rt->class_array[JS_CLASS_STRING].exotic = &js_string_exotic_methods;
    rt->class_array[JS_CLASS_MODULE_NS].exotic = &js_module_ns_exotic_methods;

    rt->class_array[JS_CLASS_C_FUNCTION].call = js_call_c_function;
    rt->class_array[JS_CLASS_C_FUNCTION_DATA].call = js_c_function_data_call;
    rt->class_array[JS_CLASS_BOUND_FUNCTION].call = js_call_bound_function;
    rt->class_array[JS_CLASS_GENERATOR_FUNCTION].call = js_generator_function_call;
    if (init_shape_hash(rt))
        goto fail;

    rt->stack_size = JS_DEFAULT_STACK_SIZE;
    JS_UpdateStackTop(rt);

    rt->current_exception = JS_NULL;

    return rt;
 fail:
    JS_FreeRuntime(rt);
    return NULL;
}

void *JS_GetRuntimeOpaque(JSRuntime *rt)
{
    return rt->user_opaque;
}

void JS_SetRuntimeOpaque(JSRuntime *rt, void *opaque)
{
    rt->user_opaque = opaque;
}

/* default memory allocation functions with memory limitation */
static inline size_t js_def_malloc_usable_size(void *ptr)
{
#if defined(__APPLE__)
    return malloc_size(ptr);
#elif defined(_WIN32)
    return _msize(ptr);
#elif defined(EMSCRIPTEN)
    return 0;
#elif defined(__linux__)
    return malloc_usable_size(ptr);
#else
    /* change this to `return 0;` if compilation fails */
    return malloc_usable_size(ptr);
#endif
}

static void *js_def_malloc(JSMallocState *s, size_t size)
{
    void *ptr;

    /* Do not allocate zero bytes: behavior is platform dependent */
    assert(size != 0);

    if (unlikely(s->malloc_size + size > s->malloc_limit))
        return NULL;

    ptr = malloc(size);
    if (!ptr)
        return NULL;

    s->malloc_count++;
    s->malloc_size += js_def_malloc_usable_size(ptr) + MALLOC_OVERHEAD;
    return ptr;
}

static void js_def_free(JSMallocState *s, void *ptr)
{
    if (!ptr)
        return;

    s->malloc_count--;
    s->malloc_size -= js_def_malloc_usable_size(ptr) + MALLOC_OVERHEAD;
    free(ptr);
}

static void *js_def_realloc(JSMallocState *s, void *ptr, size_t size)
{
    size_t old_size;

    if (!ptr) {
        if (size == 0)
            return NULL;
        return js_def_malloc(s, size);
    }
    old_size = js_def_malloc_usable_size(ptr);
    if (size == 0) {
        s->malloc_count--;
        s->malloc_size -= old_size + MALLOC_OVERHEAD;
        free(ptr);
        return NULL;
    }
    if (s->malloc_size + size - old_size > s->malloc_limit)
        return NULL;

    ptr = realloc(ptr, size);
    if (!ptr)
        return NULL;

    s->malloc_size += js_def_malloc_usable_size(ptr) - old_size;
    return ptr;
}

static const JSMallocFunctions def_malloc_funcs = {
    js_def_malloc,
    js_def_free,
    js_def_realloc,
#if defined(__APPLE__)
    malloc_size,
#elif defined(_WIN32)
    (size_t (*)(const void *))_msize,
#elif defined(EMSCRIPTEN)
    NULL,
#elif defined(__linux__)
    (size_t (*)(const void *))malloc_usable_size,
#else
    /* change this to `NULL,` if compilation fails */
    malloc_usable_size,
#endif
};

JSRuntime *JS_NewRuntime(void)
{
    return JS_NewRuntime2(&def_malloc_funcs, NULL);
}

void JS_SetMemoryLimit(JSRuntime *rt, size_t limit)
{
    rt->malloc_state.malloc_limit = limit;
}

/* use -1 to disable automatic GC */
void JS_SetGCThreshold(JSRuntime *rt, size_t gc_threshold)
{
    rt->malloc_gc_threshold = gc_threshold;
}

#define malloc(s) malloc_is_forbidden(s)
#define free(p) free_is_forbidden(p)
#define realloc(p,s) realloc_is_forbidden(p,s)

void JS_SetInterruptHandler(JSRuntime *rt, JSInterruptHandler *cb, void *opaque)
{
    rt->interrupt_handler = cb;
    rt->interrupt_opaque = opaque;
}

void JS_SetCanBlock(JSRuntime *rt, BOOL can_block)
{
    rt->can_block = can_block;
}

void JS_SetSharedArrayBufferFunctions(JSRuntime *rt,
                                      const JSSharedArrayBufferFunctions *sf)
{
    rt->sab_funcs = *sf;
}

/* return 0 if OK, < 0 if exception */
int JS_EnqueueJob(JSContext *ctx, JSJobFunc *job_func,
                  int argc, JSValueConst *argv)
{
    JSRuntime *rt = ctx->rt;
    JSJobEntry *e;
    int i;

    e = js_malloc(ctx, sizeof(*e) + argc * sizeof(JSValue));
    if (!e)
        return -1;
    e->ctx = ctx;
    e->job_func = job_func;
    e->argc = argc;
    for(i = 0; i < argc; i++) {
        e->argv[i] = JS_DupValue(ctx, argv[i]);
    }
    list_add_tail(&e->link, &rt->job_list);
    return 0;
}

BOOL JS_IsJobPending(JSRuntime *rt)
{
    return !list_empty(&rt->job_list);
}

/* return < 0 if exception, 0 if no job pending, 1 if a job was
   executed successfully. the context of the job is stored in '*pctx' */
int JS_ExecutePendingJob(JSRuntime *rt, JSContext **pctx)
{
    JSContext *ctx;
    JSJobEntry *e;
    JSValue res;
    int i, ret;

    if (list_empty(&rt->job_list)) {
        *pctx = NULL;
        return 0;
    }

    /* get the first pending job and execute it */
    e = list_entry(rt->job_list.next, JSJobEntry, link);
    list_del(&e->link);
    ctx = e->ctx;
    res = e->job_func(e->ctx, e->argc, (JSValueConst *)e->argv);
    for(i = 0; i < e->argc; i++)
        JS_FreeValue(ctx, e->argv[i]);
    if (JS_IsException(res))
        ret = -1;
    else
        ret = 1;
    JS_FreeValue(ctx, res);
    js_free(ctx, e);
    *pctx = ctx;
    return ret;
}

static inline uint32_t atom_get_free(const JSAtomStruct *p)
{
    return (uintptr_t)p >> 1;
}

static inline BOOL atom_is_free(const JSAtomStruct *p)
{
    return (uintptr_t)p & 1;
}

static inline JSAtomStruct *atom_set_free(uint32_t v)
{
    return (JSAtomStruct *)(((uintptr_t)v << 1) | 1);
}

/* Note: the string contents are uninitialized */
static JSString *js_alloc_string_rt(JSRuntime *rt, int max_len, int is_wide_char)
{
    JSString *str;
    str = js_malloc_rt(rt, sizeof(JSString) + (max_len << is_wide_char) + 1 - is_wide_char);
    if (unlikely(!str))
        return NULL;
    str->header.ref_count = 1;
    str->is_wide_char = is_wide_char;
    str->len = max_len;
    str->atom_type = 0;
    str->hash = 0;          /* optional but costless */
    str->hash_next = 0;     /* optional */
#ifdef DUMP_LEAKS
    list_add_tail(&str->link, &rt->string_list);
#endif
    return str;
}

static JSString *js_alloc_string(JSContext *ctx, int max_len, int is_wide_char)
{
    JSString *p;
    p = js_alloc_string_rt(ctx->rt, max_len, is_wide_char);
    if (unlikely(!p)) {
        JS_ThrowOutOfMemory(ctx);
        return NULL;
    }
    return p;
}

/* same as JS_FreeValueRT() but faster */
static inline void js_free_string(JSRuntime *rt, JSString *str)
{
    if (--str->header.ref_count <= 0) {
        if (str->atom_type) {
            JS_FreeAtomStruct(rt, str);
        } else {
#ifdef DUMP_LEAKS
            list_del(&str->link);
#endif
            js_free_rt(rt, str);
        }
    }
}

void JS_SetRuntimeInfo(JSRuntime *rt, const char *s)
{
    if (rt)
        rt->rt_info = s;
}

void JS_FreeRuntime(JSRuntime *rt)
{
    struct list_head *el, *el1;
    int i;

    JS_FreeValueRT(rt, rt->current_exception);

    list_for_each_safe(el, el1, &rt->job_list) {
        JSJobEntry *e = list_entry(el, JSJobEntry, link);
        for(i = 0; i < e->argc; i++)
            JS_FreeValueRT(rt, e->argv[i]);
        js_free_rt(rt, e);
    }
    init_list_head(&rt->job_list);

    JS_RunGC(rt);

#ifdef DUMP_LEAKS
    /* leaking objects */
    {
        BOOL header_done;
        JSGCObjectHeader *p;
        int count;

        /* remove the internal refcounts to display only the object
           referenced externally */
        list_for_each(el, &rt->gc_obj_list) {
            p = list_entry(el, JSGCObjectHeader, link);
            p->mark = 0;
        }
        gc_decref(rt);

        header_done = FALSE;
        list_for_each(el, &rt->gc_obj_list) {
            p = list_entry(el, JSGCObjectHeader, link);
            if (p->ref_count != 0) {
                if (!header_done) {
                    printf("Object leaks:\n");
                    JS_DumpObjectHeader(rt);
                    header_done = TRUE;
                }
                JS_DumpGCObject(rt, p);
            }
        }

        count = 0;
        list_for_each(el, &rt->gc_obj_list) {
            p = list_entry(el, JSGCObjectHeader, link);
            if (p->ref_count == 0) {
                count++;
            }
        }
        if (count != 0)
            printf("Secondary object leaks: %d\n", count);
    }
#endif
    assert(list_empty(&rt->gc_obj_list));

    /* free the classes */
    for(i = 0; i < rt->class_count; i++) {
        JSClass *cl = &rt->class_array[i];
        if (cl->class_id != 0) {
            JS_FreeAtomRT(rt, cl->class_name);
        }
    }
    js_free_rt(rt, rt->class_array);

#ifdef CONFIG_BIGNUM
    bf_context_end(&rt->bf_ctx);
#endif

#ifdef DUMP_LEAKS
    /* only the atoms defined in JS_InitAtoms() should be left */
    {
        BOOL header_done = FALSE;

        for(i = 0; i < rt->atom_size; i++) {
            JSAtomStruct *p = rt->atom_array[i];
            if (!atom_is_free(p) /* && p->str*/) {
                if (i >= JS_ATOM_END || p->header.ref_count != 1) {
                    if (!header_done) {
                        header_done = TRUE;
                        if (rt->rt_info) {
                            printf("%s:1: atom leakage:", rt->rt_info);
                        } else {
                            printf("Atom leaks:\n"
                                   "    %6s %6s %s\n",
                                   "ID", "REFCNT", "NAME");
                        }
                    }
                    if (rt->rt_info) {
                        printf(" ");
                    } else {
                        printf("    %6u %6u ", i, p->header.ref_count);
                    }
                    switch (p->atom_type) {
                    case JS_ATOM_TYPE_STRING:
                        JS_DumpString(rt, p);
                        break;
                    case JS_ATOM_TYPE_GLOBAL_SYMBOL:
                        printf("Symbol.for(");
                        JS_DumpString(rt, p);
                        printf(")");
                        break;
                    case JS_ATOM_TYPE_SYMBOL:
                        if (p->hash == JS_ATOM_HASH_SYMBOL) {
                            printf("Symbol(");
                            JS_DumpString(rt, p);
                            printf(")");
                        } else {
                            printf("Private(");
                            JS_DumpString(rt, p);
                            printf(")");
                        }
                        break;
                    }
                    if (rt->rt_info) {
                        printf(":%u", p->header.ref_count);
                    } else {
                        printf("\n");
                    }
                }
            }
        }
        if (rt->rt_info && header_done)
            printf("\n");
    }
#endif

    /* free the atoms */
    for(i = 0; i < rt->atom_size; i++) {
        JSAtomStruct *p = rt->atom_array[i];
        if (!atom_is_free(p)) {
#ifdef DUMP_LEAKS
            list_del(&p->link);
#endif
            js_free_rt(rt, p);
        }
    }
    js_free_rt(rt, rt->atom_array);
    js_free_rt(rt, rt->atom_hash);
    js_free_rt(rt, rt->shape_hash);
#ifdef DUMP_LEAKS
    if (!list_empty(&rt->string_list)) {
        if (rt->rt_info) {
            printf("%s:1: string leakage:", rt->rt_info);
        } else {
            printf("String leaks:\n"
                   "    %6s %s\n",
                   "REFCNT", "VALUE");
        }
        list_for_each_safe(el, el1, &rt->string_list) {
            JSString *str = list_entry(el, JSString, link);
            if (rt->rt_info) {
                printf(" ");
            } else {
                printf("    %6u ", str->header.ref_count);
            }
            JS_DumpString(rt, str);
            if (rt->rt_info) {
                printf(":%u", str->header.ref_count);
            } else {
                printf("\n");
            }
            list_del(&str->link);
            js_free_rt(rt, str);
        }
        if (rt->rt_info)
            printf("\n");
    }
    {
        JSMallocState *s = &rt->malloc_state;
        if (s->malloc_count > 1) {
            if (rt->rt_info)
                printf("%s:1: ", rt->rt_info);
            printf("Memory leak: %"PRIu64" bytes lost in %"PRIu64" block%s\n",
                   (uint64_t)(s->malloc_size - sizeof(JSRuntime)),
                   (uint64_t)(s->malloc_count - 1), &"s"[s->malloc_count == 2]);
        }
    }
#endif

    {
        JSMallocState ms = rt->malloc_state;
        rt->mf.js_free(&ms, rt);
    }
}

JSContext *JS_NewContextRaw(JSRuntime *rt)
{
    JSContext *ctx;
    int i;

    ctx = js_mallocz_rt(rt, sizeof(JSContext));
    if (!ctx)
        return NULL;
    ctx->header.ref_count = 1;
    add_gc_object(rt, &ctx->header, JS_GC_OBJ_TYPE_JS_CONTEXT);

    ctx->class_proto = js_malloc_rt(rt, sizeof(ctx->class_proto[0]) *
                                    rt->class_count);
    if (!ctx->class_proto) {
        js_free_rt(rt, ctx);
        return NULL;
    }
    ctx->rt = rt;
    list_add_tail(&ctx->link, &rt->context_list);
#ifdef CONFIG_BIGNUM
    ctx->bf_ctx = &rt->bf_ctx;
    ctx->fp_env.prec = 113;
    ctx->fp_env.flags = bf_set_exp_bits(15) | BF_RNDN | BF_FLAG_SUBNORMAL;
#endif
    for(i = 0; i < rt->class_count; i++)
        ctx->class_proto[i] = JS_NULL;
    ctx->array_ctor = JS_NULL;
    ctx->regexp_ctor = JS_NULL;
    ctx->promise_ctor = JS_NULL;
    init_list_head(&ctx->loaded_modules);

    JS_AddIntrinsicBasicObjects(ctx);
    return ctx;
}

JSContext *JS_NewContext(JSRuntime *rt)
{
    JSContext *ctx;

    ctx = JS_NewContextRaw(rt);
    if (!ctx)
        return NULL;

    JS_AddIntrinsicBaseObjects(ctx);
    JS_AddIntrinsicDate(ctx);
    JS_AddIntrinsicEval(ctx);
    JS_AddIntrinsicStringNormalize(ctx);
    JS_AddIntrinsicRegExp(ctx);
    JS_AddIntrinsicJSON(ctx);
    JS_AddIntrinsicProxy(ctx);
    JS_AddIntrinsicMapSet(ctx);
    JS_AddIntrinsicTypedArrays(ctx);
    JS_AddIntrinsicPromise(ctx);
#ifdef CONFIG_BIGNUM
    JS_AddIntrinsicBigInt(ctx);
#endif
    return ctx;
}

void *JS_GetContextOpaque(JSContext *ctx)
{
    return ctx->user_opaque;
}

void JS_SetContextOpaque(JSContext *ctx, void *opaque)
{
    ctx->user_opaque = opaque;
}

/* set the new value and free the old value after (freeing the value
   can reallocate the object data) */
static inline void set_value(JSContext *ctx, JSValue *pval, JSValue new_val)
{
    JSValue old_val;
    old_val = *pval;
    *pval = new_val;
    JS_FreeValue(ctx, old_val);
}

void JS_SetClassProto(JSContext *ctx, JSClassID class_id, JSValue obj)
{
    JSRuntime *rt = ctx->rt;
    assert(class_id < rt->class_count);
    set_value(ctx, &ctx->class_proto[class_id], obj);
}

JSValue JS_GetClassProto(JSContext *ctx, JSClassID class_id)
{
    JSRuntime *rt = ctx->rt;
    assert(class_id < rt->class_count);
    return JS_DupValue(ctx, ctx->class_proto[class_id]);
}

typedef enum JSFreeModuleEnum {
    JS_FREE_MODULE_ALL,
    JS_FREE_MODULE_NOT_RESOLVED,
    JS_FREE_MODULE_NOT_EVALUATED,
} JSFreeModuleEnum;

/* XXX: would be more efficient with separate module lists */
static void js_free_modules(JSContext *ctx, JSFreeModuleEnum flag)
{
    struct list_head *el, *el1;
    list_for_each_safe(el, el1, &ctx->loaded_modules) {
        JSModuleDef *m = list_entry(el, JSModuleDef, link);
        if (flag == JS_FREE_MODULE_ALL ||
            (flag == JS_FREE_MODULE_NOT_RESOLVED && !m->resolved) ||
            (flag == JS_FREE_MODULE_NOT_EVALUATED && !m->evaluated)) {
            js_free_module_def(ctx, m);
        }
    }
}

JSContext *JS_DupContext(JSContext *ctx)
{
    ctx->header.ref_count++;
    return ctx;
}

/* used by the GC */
static void JS_MarkContext(JSRuntime *rt, JSContext *ctx,
                           JS_MarkFunc *mark_func)
{
    int i;
    struct list_head *el;

    /* modules are not seen by the GC, so we directly mark the objects
       referenced by each module */
    list_for_each(el, &ctx->loaded_modules) {
        JSModuleDef *m = list_entry(el, JSModuleDef, link);
        js_mark_module_def(rt, m, mark_func);
    }

    JS_MarkValue(rt, ctx->global_obj, mark_func);
    JS_MarkValue(rt, ctx->global_var_obj, mark_func);

    JS_MarkValue(rt, ctx->throw_type_error, mark_func);
    JS_MarkValue(rt, ctx->eval_obj, mark_func);

    JS_MarkValue(rt, ctx->array_proto_values, mark_func);
    for(i = 0; i < JS_NATIVE_ERROR_COUNT; i++) {
        JS_MarkValue(rt, ctx->native_error_proto[i], mark_func);
    }
    for(i = 0; i < rt->class_count; i++) {
        JS_MarkValue(rt, ctx->class_proto[i], mark_func);
    }
    JS_MarkValue(rt, ctx->iterator_proto, mark_func);
    JS_MarkValue(rt, ctx->async_iterator_proto, mark_func);
    JS_MarkValue(rt, ctx->promise_ctor, mark_func);
    JS_MarkValue(rt, ctx->array_ctor, mark_func);
    JS_MarkValue(rt, ctx->regexp_ctor, mark_func);
    JS_MarkValue(rt, ctx->function_ctor, mark_func);
    JS_MarkValue(rt, ctx->function_proto, mark_func);

    if (ctx->array_shape)
        mark_func(rt, &ctx->array_shape->header);
}

void JS_FreeContext(JSContext *ctx)
{
    JSRuntime *rt = ctx->rt;
    int i;

    if (--ctx->header.ref_count > 0)
        return;
    assert(ctx->header.ref_count == 0);
    
#ifdef DUMP_ATOMS
    JS_DumpAtoms(ctx->rt);
#endif
#ifdef DUMP_SHAPES
    JS_DumpShapes(ctx->rt);
#endif
#ifdef DUMP_OBJECTS
    {
        struct list_head *el;
        JSGCObjectHeader *p;
        printf("JSObjects: {\n");
        JS_DumpObjectHeader(ctx->rt);
        list_for_each(el, &rt->gc_obj_list) {
            p = list_entry(el, JSGCObjectHeader, link);
            JS_DumpGCObject(rt, p);
        }
        printf("}\n");
    }
#endif
#ifdef DUMP_MEM
    {
        JSMemoryUsage stats;
        JS_ComputeMemoryUsage(rt, &stats);
        JS_DumpMemoryUsage(stdout, &stats, rt);
    }
#endif

    js_free_modules(ctx, JS_FREE_MODULE_ALL);

    JS_FreeValue(ctx, ctx->global_obj);
    JS_FreeValue(ctx, ctx->global_var_obj);

    JS_FreeValue(ctx, ctx->throw_type_error);
    JS_FreeValue(ctx, ctx->eval_obj);

    JS_FreeValue(ctx, ctx->array_proto_values);
    for(i = 0; i < JS_NATIVE_ERROR_COUNT; i++) {
        JS_FreeValue(ctx, ctx->native_error_proto[i]);
    }
    for(i = 0; i < rt->class_count; i++) {
        JS_FreeValue(ctx, ctx->class_proto[i]);
    }
    js_free_rt(rt, ctx->class_proto);
    JS_FreeValue(ctx, ctx->iterator_proto);
    JS_FreeValue(ctx, ctx->async_iterator_proto);
    JS_FreeValue(ctx, ctx->promise_ctor);
    JS_FreeValue(ctx, ctx->array_ctor);
    JS_FreeValue(ctx, ctx->regexp_ctor);
    JS_FreeValue(ctx, ctx->function_ctor);
    JS_FreeValue(ctx, ctx->function_proto);

    js_free_shape_null(ctx->rt, ctx->array_shape);

    list_del(&ctx->link);
    remove_gc_object(&ctx->header);
    js_free_rt(ctx->rt, ctx);
}

JSRuntime *JS_GetRuntime(JSContext *ctx)
{
    return ctx->rt;
}

static void update_stack_limit(JSRuntime *rt)
{
    if (rt->stack_size == 0) {
        rt->stack_limit = 0; /* no limit */
    } else {
        rt->stack_limit = rt->stack_top - rt->stack_size;
    }
}

void JS_SetMaxStackSize(JSRuntime *rt, size_t stack_size)
{
    rt->stack_size = stack_size;
    update_stack_limit(rt);
}

void JS_UpdateStackTop(JSRuntime *rt)
{
    rt->stack_top = js_get_stack_pointer();
    update_stack_limit(rt);
}

static inline BOOL is_strict_mode(JSContext *ctx)
{
    JSStackFrame *sf = ctx->rt->current_stack_frame;
    return (sf && (sf->js_mode & JS_MODE_STRICT));
}

#ifdef CONFIG_BIGNUM
static inline BOOL is_math_mode(JSContext *ctx)
{
    JSStackFrame *sf = ctx->rt->current_stack_frame;
    return (sf && (sf->js_mode & JS_MODE_MATH));
}
#endif

/* JSAtom support */

#define JS_ATOM_TAG_INT (1U << 31)
#define JS_ATOM_MAX_INT (JS_ATOM_TAG_INT - 1)
#define JS_ATOM_MAX     ((1U << 30) - 1)

/* return the max count from the hash size */
#define JS_ATOM_COUNT_RESIZE(n) ((n) * 2)

static inline BOOL __JS_AtomIsConst(JSAtom v)
{
#if defined(DUMP_LEAKS) && DUMP_LEAKS > 1
        return (int32_t)v <= 0;
#else
        return (int32_t)v < JS_ATOM_END;
#endif
}

static inline BOOL __JS_AtomIsTaggedInt(JSAtom v)
{
    return (v & JS_ATOM_TAG_INT) != 0;
}

static inline JSAtom __JS_AtomFromUInt32(uint32_t v)
{
    return v | JS_ATOM_TAG_INT;
}

static inline uint32_t __JS_AtomToUInt32(JSAtom atom)
{
    return atom & ~JS_ATOM_TAG_INT;
}

static inline int is_num(int c)
{
    return c >= '0' && c <= '9';
}

/* return TRUE if the string is a number n with 0 <= n <= 2^32-1 */
static inline BOOL is_num_string(uint32_t *pval, const JSString *p)
{
    uint32_t n;
    uint64_t n64;
    int c, i, len;

    len = p->len;
    if (len == 0 || len > 10)
        return FALSE;
    if (p->is_wide_char)
        c = p->u.str16[0];
    else
        c = p->u.str8[0];
    if (is_num(c)) {
        if (c == '0') {
            if (len != 1)
                return FALSE;
            n = 0;
        } else {
            n = c - '0';
            for(i = 1; i < len; i++) {
                if (p->is_wide_char)
                    c = p->u.str16[i];
                else
                    c = p->u.str8[i];
                if (!is_num(c))
                    return FALSE;
                n64 = (uint64_t)n * 10 + (c - '0');
                if ((n64 >> 32) != 0)
                    return FALSE;
                n = n64;
            }
        }
        *pval = n;
        return TRUE;
    } else {
        return FALSE;
    }
}

/* XXX: could use faster version ? */
static inline uint32_t hash_string8(const uint8_t *str, size_t len, uint32_t h)
{
    size_t i;

    for(i = 0; i < len; i++)
        h = h * 263 + str[i];
    return h;
}

static inline uint32_t hash_string16(const uint16_t *str,
                                     size_t len, uint32_t h)
{
    size_t i;

    for(i = 0; i < len; i++)
        h = h * 263 + str[i];
    return h;
}

static uint32_t hash_string(const JSString *str, uint32_t h)
{
    if (str->is_wide_char)
        h = hash_string16(str->u.str16, str->len, h);
    else
        h = hash_string8(str->u.str8, str->len, h);
    return h;
}

static __maybe_unused void JS_DumpString(JSRuntime *rt,
                                                  const JSString *p)
{
    int i, c, sep;

    if (p == NULL) {
        printf("<null>");
        return;
    }
    printf("%d", p->header.ref_count);
    sep = (p->header.ref_count == 1) ? '\"' : '\'';
    putchar(sep);
    for(i = 0; i < p->len; i++) {
        if (p->is_wide_char)
            c = p->u.str16[i];
        else
            c = p->u.str8[i];
        if (c == sep || c == '\\') {
            putchar('\\');
            putchar(c);
        } else if (c >= ' ' && c <= 126) {
            putchar(c);
        } else if (c == '\n') {
            putchar('\\');
            putchar('n');
        } else {
            printf("\\u%04x", c);
        }
    }
    putchar(sep);
}

static __maybe_unused void JS_DumpAtoms(JSRuntime *rt)
{
    JSAtomStruct *p;
    int h, i;
    /* This only dumps hashed atoms, not JS_ATOM_TYPE_SYMBOL atoms */
    printf("JSAtom count=%d size=%d hash_size=%d:\n",
           rt->atom_count, rt->atom_size, rt->atom_hash_size);
    printf("JSAtom hash table: {\n");
    for(i = 0; i < rt->atom_hash_size; i++) {
        h = rt->atom_hash[i];
        if (h) {
            printf("  %d:", i);
            while (h) {
                p = rt->atom_array[h];
                printf(" ");
                JS_DumpString(rt, p);
                h = p->hash_next;
            }
            printf("\n");
        }
    }
    printf("}\n");
    printf("JSAtom table: {\n");
    for(i = 0; i < rt->atom_size; i++) {
        p = rt->atom_array[i];
        if (!atom_is_free(p)) {
            printf("  %d: { %d %08x ", i, p->atom_type, p->hash);
            if (!(p->len == 0 && p->is_wide_char != 0))
                JS_DumpString(rt, p);
            printf(" %d }\n", p->hash_next);
        }
    }
    printf("}\n");
}

static int JS_ResizeAtomHash(JSRuntime *rt, int new_hash_size)
{
    JSAtomStruct *p;
    uint32_t new_hash_mask, h, i, hash_next1, j, *new_hash;

    assert((new_hash_size & (new_hash_size - 1)) == 0); /* power of two */
    new_hash_mask = new_hash_size - 1;
    new_hash = js_mallocz_rt(rt, sizeof(rt->atom_hash[0]) * new_hash_size);
    if (!new_hash)
        return -1;
    for(i = 0; i < rt->atom_hash_size; i++) {
        h = rt->atom_hash[i];
        while (h != 0) {
            p = rt->atom_array[h];
            hash_next1 = p->hash_next;
            /* add in new hash table */
            j = p->hash & new_hash_mask;
            p->hash_next = new_hash[j];
            new_hash[j] = h;
            h = hash_next1;
        }
    }
    js_free_rt(rt, rt->atom_hash);
    rt->atom_hash = new_hash;
    rt->atom_hash_size = new_hash_size;
    rt->atom_count_resize = JS_ATOM_COUNT_RESIZE(new_hash_size);
    //    JS_DumpAtoms(rt);
    return 0;
}

static int JS_InitAtoms(JSRuntime *rt)
{
    int i, len, atom_type;
    const char *p;

    rt->atom_hash_size = 0;
    rt->atom_hash = NULL;
    rt->atom_count = 0;
    rt->atom_size = 0;
    rt->atom_free_index = 0;
    if (JS_ResizeAtomHash(rt, 256))     /* there are at least 195 predefined atoms */
        return -1;

    p = js_atom_init;
    for(i = 1; i < JS_ATOM_END; i++) {
        if (i == JS_ATOM_Private_brand)
            atom_type = JS_ATOM_TYPE_PRIVATE;
        else if (i >= JS_ATOM_Symbol_toPrimitive)
            atom_type = JS_ATOM_TYPE_SYMBOL;
        else
            atom_type = JS_ATOM_TYPE_STRING;
        len = strlen(p);
        if (__JS_NewAtomInit(rt, p, len, atom_type) == JS_ATOM_NULL)
            return -1;
        p = p + len + 1;
    }
    return 0;
}

static JSAtom JS_DupAtomRT(JSRuntime *rt, JSAtom v)
{
    JSAtomStruct *p;

    if (!__JS_AtomIsConst(v)) {
        p = rt->atom_array[v];
        p->header.ref_count++;
    }
    return v;
}

JSAtom JS_DupAtom(JSContext *ctx, JSAtom v)
{
    JSRuntime *rt;
    JSAtomStruct *p;

    if (!__JS_AtomIsConst(v)) {
        rt = ctx->rt;
        p = rt->atom_array[v];
        p->header.ref_count++;
    }
    return v;
}

static JSAtomKindEnum JS_AtomGetKind(JSContext *ctx, JSAtom v)
{
    JSRuntime *rt;
    JSAtomStruct *p;

    rt = ctx->rt;
    if (__JS_AtomIsTaggedInt(v))
        return JS_ATOM_KIND_STRING;
    p = rt->atom_array[v];
    switch(p->atom_type) {
    case JS_ATOM_TYPE_STRING:
        return JS_ATOM_KIND_STRING;
    case JS_ATOM_TYPE_GLOBAL_SYMBOL:
        return JS_ATOM_KIND_SYMBOL;
    case JS_ATOM_TYPE_SYMBOL:
        switch(p->hash) {
        case JS_ATOM_HASH_SYMBOL:
            return JS_ATOM_KIND_SYMBOL;
        case JS_ATOM_HASH_PRIVATE:
            return JS_ATOM_KIND_PRIVATE;
        default:
            abort();
        }
    default:
        abort();
    }
}

static BOOL JS_AtomIsString(JSContext *ctx, JSAtom v)
{
    return JS_AtomGetKind(ctx, v) == JS_ATOM_KIND_STRING;
}

static JSAtom js_get_atom_index(JSRuntime *rt, JSAtomStruct *p)
{
    uint32_t i = p->hash_next;  /* atom_index */
    if (p->atom_type != JS_ATOM_TYPE_SYMBOL) {
        JSAtomStruct *p1;

        i = rt->atom_hash[p->hash & (rt->atom_hash_size - 1)];
        p1 = rt->atom_array[i];
        while (p1 != p) {
            assert(i != 0);
            i = p1->hash_next;
            p1 = rt->atom_array[i];
        }
    }
    return i;
}

/* string case (internal). Return JS_ATOM_NULL if error. 'str' is
   freed. */
static JSAtom __JS_NewAtom(JSRuntime *rt, JSString *str, int atom_type)
{
    uint32_t h, h1, i;
    JSAtomStruct *p;
    int len;

#if 0
    printf("__JS_NewAtom: ");  JS_DumpString(rt, str); printf("\n");
#endif
    if (atom_type < JS_ATOM_TYPE_SYMBOL) {
        /* str is not NULL */
        if (str->atom_type == atom_type) {
            /* str is the atom, return its index */
            i = js_get_atom_index(rt, str);
            /* reduce string refcount and increase atom's unless constant */
            if (__JS_AtomIsConst(i))
                str->header.ref_count--;
            return i;
        }
        /* try and locate an already registered atom */
        len = str->len;
        h = hash_string(str, atom_type);
        h &= JS_ATOM_HASH_MASK;
        h1 = h & (rt->atom_hash_size - 1);
        i = rt->atom_hash[h1];
        while (i != 0) {
            p = rt->atom_array[i];
            if (p->hash == h &&
                p->atom_type == atom_type &&
                p->len == len &&
                js_string_memcmp(p, str, len) == 0) {
                if (!__JS_AtomIsConst(i))
                    p->header.ref_count++;
                goto done;
            }
            i = p->hash_next;
        }
    } else {
        h1 = 0; /* avoid warning */
        if (atom_type == JS_ATOM_TYPE_SYMBOL) {
            h = JS_ATOM_HASH_SYMBOL;
        } else {
            h = JS_ATOM_HASH_PRIVATE;
            atom_type = JS_ATOM_TYPE_SYMBOL;
        }
    }

    if (rt->atom_free_index == 0) {
        /* allow new atom entries */
        uint32_t new_size, start;
        JSAtomStruct **new_array;

        /* alloc new with size progression 3/2:
           4 6 9 13 19 28 42 63 94 141 211 316 474 711 1066 1599 2398 3597 5395 8092
           preallocating space for predefined atoms (at least 195).
         */
        new_size = max_int(211, rt->atom_size * 3 / 2);
        if (new_size > JS_ATOM_MAX)
            goto fail;
        /* XXX: should use realloc2 to use slack space */
        new_array = js_realloc_rt(rt, rt->atom_array, sizeof(*new_array) * new_size);
        if (!new_array)
            goto fail;
        /* Note: the atom 0 is not used */
        start = rt->atom_size;
        if (start == 0) {
            /* JS_ATOM_NULL entry */
            p = js_mallocz_rt(rt, sizeof(JSAtomStruct));
            if (!p) {
                js_free_rt(rt, new_array);
                goto fail;
            }
            p->header.ref_count = 1;  /* not refcounted */
            p->atom_type = JS_ATOM_TYPE_SYMBOL;
#ifdef DUMP_LEAKS
            list_add_tail(&p->link, &rt->string_list);
#endif
            new_array[0] = p;
            rt->atom_count++;
            start = 1;
        }
        rt->atom_size = new_size;
        rt->atom_array = new_array;
        rt->atom_free_index = start;
        for(i = start; i < new_size; i++) {
            uint32_t next;
            if (i == (new_size - 1))
                next = 0;
            else
                next = i + 1;
            rt->atom_array[i] = atom_set_free(next);
        }
    }

    if (str) {
        if (str->atom_type == 0) {
            p = str;
            p->atom_type = atom_type;
        } else {
            p = js_malloc_rt(rt, sizeof(JSString) +
                             (str->len << str->is_wide_char) +
                             1 - str->is_wide_char);
            if (unlikely(!p))
                goto fail;
            p->header.ref_count = 1;
            p->is_wide_char = str->is_wide_char;
            p->len = str->len;
#ifdef DUMP_LEAKS
            list_add_tail(&p->link, &rt->string_list);
#endif
            memcpy(p->u.str8, str->u.str8, (str->len << str->is_wide_char) +
                   1 - str->is_wide_char);
            js_free_string(rt, str);
        }
    } else {
        p = js_malloc_rt(rt, sizeof(JSAtomStruct)); /* empty wide string */
        if (!p)
            return JS_ATOM_NULL;
        p->header.ref_count = 1;
        p->is_wide_char = 1;    /* Hack to represent NULL as a JSString */
        p->len = 0;
#ifdef DUMP_LEAKS
        list_add_tail(&p->link, &rt->string_list);
#endif
    }

    /* use an already free entry */
    i = rt->atom_free_index;
    rt->atom_free_index = atom_get_free(rt->atom_array[i]);
    rt->atom_array[i] = p;

    p->hash = h;
    p->hash_next = i;   /* atom_index */
    p->atom_type = atom_type;

    rt->atom_count++;

    if (atom_type != JS_ATOM_TYPE_SYMBOL) {
        p->hash_next = rt->atom_hash[h1];
        rt->atom_hash[h1] = i;
        if (unlikely(rt->atom_count >= rt->atom_count_resize))
            JS_ResizeAtomHash(rt, rt->atom_hash_size * 2);
    }

    //    JS_DumpAtoms(rt);
    return i;

 fail:
    i = JS_ATOM_NULL;
 done:
    if (str)
        js_free_string(rt, str);
    return i;
}

/* only works with zero terminated 8 bit strings */
static JSAtom __JS_NewAtomInit(JSRuntime *rt, const char *str, int len,
                               int atom_type)
{
    JSString *p;
    p = js_alloc_string_rt(rt, len, 0);
    if (!p)
        return JS_ATOM_NULL;
    memcpy(p->u.str8, str, len);
    p->u.str8[len] = '\0';
    return __JS_NewAtom(rt, p, atom_type);
}

static JSAtom __JS_FindAtom(JSRuntime *rt, const char *str, size_t len,
                            int atom_type)
{
    uint32_t h, h1, i;
    JSAtomStruct *p;

    h = hash_string8((const uint8_t *)str, len, JS_ATOM_TYPE_STRING);
    h &= JS_ATOM_HASH_MASK;
    h1 = h & (rt->atom_hash_size - 1);
    i = rt->atom_hash[h1];
    while (i != 0) {
        p = rt->atom_array[i];
        if (p->hash == h &&
            p->atom_type == JS_ATOM_TYPE_STRING &&
            p->len == len &&
            p->is_wide_char == 0 &&
            memcmp(p->u.str8, str, len) == 0) {
            if (!__JS_AtomIsConst(i))
                p->header.ref_count++;
            return i;
        }
        i = p->hash_next;
    }
    return JS_ATOM_NULL;
}

static void JS_FreeAtomStruct(JSRuntime *rt, JSAtomStruct *p)
{
#if 0   /* JS_ATOM_NULL is not refcounted: __JS_AtomIsConst() includes 0 */
    if (unlikely(i == JS_ATOM_NULL)) {
        p->header.ref_count = INT32_MAX / 2;
        return;
    }
#endif
    uint32_t i = p->hash_next;  /* atom_index */
    if (p->atom_type != JS_ATOM_TYPE_SYMBOL) {
        JSAtomStruct *p0, *p1;
        uint32_t h0;

        h0 = p->hash & (rt->atom_hash_size - 1);
        i = rt->atom_hash[h0];
        p1 = rt->atom_array[i];
        if (p1 == p) {
            rt->atom_hash[h0] = p1->hash_next;
        } else {
            for(;;) {
                assert(i != 0);
                p0 = p1;
                i = p1->hash_next;
                p1 = rt->atom_array[i];
                if (p1 == p) {
                    p0->hash_next = p1->hash_next;
                    break;
                }
            }
        }
    }
    /* insert in free atom list */
    rt->atom_array[i] = atom_set_free(rt->atom_free_index);
    rt->atom_free_index = i;
    /* free the string structure */
#ifdef DUMP_LEAKS
    list_del(&p->link);
#endif
    js_free_rt(rt, p);
    rt->atom_count--;
    assert(rt->atom_count >= 0);
}

static void __JS_FreeAtom(JSRuntime *rt, uint32_t i)
{
    JSAtomStruct *p;

    p = rt->atom_array[i];
    if (--p->header.ref_count > 0)
        return;
    JS_FreeAtomStruct(rt, p);
}

/* Warning: 'p' is freed */
static JSAtom JS_NewAtomStr(JSContext *ctx, JSString *p)
{
    JSRuntime *rt = ctx->rt;
    uint32_t n;
    if (is_num_string(&n, p)) {
        if (n <= JS_ATOM_MAX_INT) {
            js_free_string(rt, p);
            return __JS_AtomFromUInt32(n);
        }
    }
    /* XXX: should generate an exception */
    return __JS_NewAtom(rt, p, JS_ATOM_TYPE_STRING);
}

JSAtom JS_NewAtomLen(JSContext *ctx, const char *str, size_t len)
{
    JSValue val;

    if (len == 0 || !is_digit(*str)) {
        JSAtom atom = __JS_FindAtom(ctx->rt, str, len, JS_ATOM_TYPE_STRING);
        if (atom)
            return atom;
    }
    val = JS_NewStringLen(ctx, str, len);
    if (JS_IsException(val))
        return JS_ATOM_NULL;
    return JS_NewAtomStr(ctx, JS_VALUE_GET_STRING(val));
}

JSAtom JS_NewAtom(JSContext *ctx, const char *str)
{
    return JS_NewAtomLen(ctx, str, strlen(str));
}

JSAtom JS_NewAtomUInt32(JSContext *ctx, uint32_t n)
{
    if (n <= JS_ATOM_MAX_INT) {
        return __JS_AtomFromUInt32(n);
    } else {
        char buf[11];
        JSValue val;
        snprintf(buf, sizeof(buf), "%u", n);
        val = JS_NewString(ctx, buf);
        if (JS_IsException(val))
            return JS_ATOM_NULL;
        return __JS_NewAtom(ctx->rt, JS_VALUE_GET_STRING(val),
                            JS_ATOM_TYPE_STRING);
    }
}

static JSAtom JS_NewAtomInt64(JSContext *ctx, int64_t n)
{
    if ((uint64_t)n <= JS_ATOM_MAX_INT) {
        return __JS_AtomFromUInt32((uint32_t)n);
    } else {
        char buf[24];
        JSValue val;
        snprintf(buf, sizeof(buf), "%" PRId64 , n);
        val = JS_NewString(ctx, buf);
        if (JS_IsException(val))
            return JS_ATOM_NULL;
        return __JS_NewAtom(ctx->rt, JS_VALUE_GET_STRING(val),
                            JS_ATOM_TYPE_STRING);
    }
}

/* 'p' is freed */
static JSValue JS_NewSymbol(JSContext *ctx, JSString *p, int atom_type)
{
    JSRuntime *rt = ctx->rt;
    JSAtom atom;
    atom = __JS_NewAtom(rt, p, atom_type);
    if (atom == JS_ATOM_NULL)
        return JS_ThrowOutOfMemory(ctx);
    return JS_MKPTR(JS_TAG_SYMBOL, rt->atom_array[atom]);
}

/* descr must be a non-numeric string atom */
static JSValue JS_NewSymbolFromAtom(JSContext *ctx, JSAtom descr,
                                    int atom_type)
{
    JSRuntime *rt = ctx->rt;
    JSString *p;

    assert(!__JS_AtomIsTaggedInt(descr));
    assert(descr < rt->atom_size);
    p = rt->atom_array[descr];
    JS_DupValue(ctx, JS_MKPTR(JS_TAG_STRING, p));
    return JS_NewSymbol(ctx, p, atom_type);
}

#define ATOM_GET_STR_BUF_SIZE 64

/* Should only be used for debug. */
static const char *JS_AtomGetStrRT(JSRuntime *rt, char *buf, int buf_size,
                                   JSAtom atom)
{
    if (__JS_AtomIsTaggedInt(atom)) {
        snprintf(buf, buf_size, "%u", __JS_AtomToUInt32(atom));
    } else {
        JSAtomStruct *p;
        assert(atom < rt->atom_size);
        if (atom == JS_ATOM_NULL) {
            snprintf(buf, buf_size, "<null>");
        } else {
            int i, c;
            char *q;
            JSString *str;

            q = buf;
            p = rt->atom_array[atom];
            assert(!atom_is_free(p));
            str = p;
            if (str) {
                if (!str->is_wide_char) {
                    /* special case ASCII strings */
                    c = 0;
                    for(i = 0; i < str->len; i++) {
                        c |= str->u.str8[i];
                    }
                    if (c < 0x80)
                        return (const char *)str->u.str8;
                }
                for(i = 0; i < str->len; i++) {
                    if (str->is_wide_char)
                        c = str->u.str16[i];
                    else
                        c = str->u.str8[i];
                    if ((q - buf) >= buf_size - UTF8_CHAR_LEN_MAX)
                        break;
                    if (c < 128) {
                        *q++ = c;
                    } else {
                        q += unicode_to_utf8((uint8_t *)q, c);
                    }
                }
            }
            *q = '\0';
        }
    }
    return buf;
}

static const char *JS_AtomGetStr(JSContext *ctx, char *buf, int buf_size, JSAtom atom)
{
    return JS_AtomGetStrRT(ctx->rt, buf, buf_size, atom);
}

static JSValue __JS_AtomToValue(JSContext *ctx, JSAtom atom, BOOL force_string)
{
    char buf[ATOM_GET_STR_BUF_SIZE];

    if (__JS_AtomIsTaggedInt(atom)) {
        snprintf(buf, sizeof(buf), "%u", __JS_AtomToUInt32(atom));
        return JS_NewString(ctx, buf);
    } else {
        JSRuntime *rt = ctx->rt;
        JSAtomStruct *p;
        assert(atom < rt->atom_size);
        p = rt->atom_array[atom];
        if (p->atom_type == JS_ATOM_TYPE_STRING) {
            goto ret_string;
        } else if (force_string) {
            if (p->len == 0 && p->is_wide_char != 0) {
                /* no description string */
                p = rt->atom_array[JS_ATOM_empty_string];
            }
        ret_string:
            return JS_DupValue(ctx, JS_MKPTR(JS_TAG_STRING, p));
        } else {
            return JS_DupValue(ctx, JS_MKPTR(JS_TAG_SYMBOL, p));
        }
    }
}

JSValue JS_AtomToValue(JSContext *ctx, JSAtom atom)
{
    return __JS_AtomToValue(ctx, atom, FALSE);
}

JSValue JS_AtomToString(JSContext *ctx, JSAtom atom)
{
    return __JS_AtomToValue(ctx, atom, TRUE);
}

/* return TRUE if the atom is an array index (i.e. 0 <= index <=
   2^32-2 and return its value */
static BOOL JS_AtomIsArrayIndex(JSContext *ctx, uint32_t *pval, JSAtom atom)
{
    if (__JS_AtomIsTaggedInt(atom)) {
        *pval = __JS_AtomToUInt32(atom);
        return TRUE;
    } else {
        JSRuntime *rt = ctx->rt;
        JSAtomStruct *p;
        uint32_t val;

        assert(atom < rt->atom_size);
        p = rt->atom_array[atom];
        if (p->atom_type == JS_ATOM_TYPE_STRING &&
            is_num_string(&val, p) && val != -1) {
            *pval = val;
            return TRUE;
        } else {
            *pval = 0;
            return FALSE;
        }
    }
}

/* This test must be fast if atom is not a numeric index (e.g. a
   method name). Return JS_UNDEFINED if not a numeric
   index. JS_EXCEPTION can also be returned. */
static JSValue JS_AtomIsNumericIndex1(JSContext *ctx, JSAtom atom)
{
    JSRuntime *rt = ctx->rt;
    JSAtomStruct *p1;
    JSString *p;
    int c, len, ret;
    JSValue num, str;

    if (__JS_AtomIsTaggedInt(atom))
        return JS_NewInt32(ctx, __JS_AtomToUInt32(atom));
    assert(atom < rt->atom_size);
    p1 = rt->atom_array[atom];
    if (p1->atom_type != JS_ATOM_TYPE_STRING)
        return JS_UNDEFINED;
    p = p1;
    len = p->len;
    if (p->is_wide_char) {
        const uint16_t *r = p->u.str16, *r_end = p->u.str16 + len;
        if (r >= r_end)
            return JS_UNDEFINED;
        c = *r;
        if (c == '-') {
            if (r >= r_end)
                return JS_UNDEFINED;
            r++;
            c = *r;
            /* -0 case is specific */
            if (c == '0' && len == 2)
                goto minus_zero;
        }
        /* XXX: should test NaN, but the tests do not check it */
        if (!is_num(c)) {
            /* XXX: String should be normalized, therefore 8-bit only */
            const uint16_t nfinity16[7] = { 'n', 'f', 'i', 'n', 'i', 't', 'y' };
            if (!(c =='I' && (r_end - r) == 8 &&
                  !memcmp(r + 1, nfinity16, sizeof(nfinity16))))
                return JS_UNDEFINED;
        }
    } else {
        const uint8_t *r = p->u.str8, *r_end = p->u.str8 + len;
        if (r >= r_end)
            return JS_UNDEFINED;
        c = *r;
        if (c == '-') {
            if (r >= r_end)
                return JS_UNDEFINED;
            r++;
            c = *r;
            /* -0 case is specific */
            if (c == '0' && len == 2) {
            minus_zero:
                return __JS_NewFloat64(ctx, -0.0);
            }
        }
        if (!is_num(c)) {
            if (!(c =='I' && (r_end - r) == 8 &&
                  !memcmp(r + 1, "nfinity", 7)))
                return JS_UNDEFINED;
        }
    }
    /* XXX: bignum: would be better to only accept integer to avoid
       relying on current floating point precision */
    /* this is ECMA CanonicalNumericIndexString primitive */
    num = JS_ToNumber(ctx, JS_MKPTR(JS_TAG_STRING, p));
    if (JS_IsException(num))
        return num;
    str = JS_ToString(ctx, num);
    if (JS_IsException(str)) {
        JS_FreeValue(ctx, num);
        return str;
    }
    ret = js_string_compare(ctx, p, JS_VALUE_GET_STRING(str));
    JS_FreeValue(ctx, str);
    if (ret == 0) {
        return num;
    } else {
        JS_FreeValue(ctx, num);
        return JS_UNDEFINED;
    }
}

/* return -1 if exception or TRUE/FALSE */
static int JS_AtomIsNumericIndex(JSContext *ctx, JSAtom atom)
{
    JSValue num;
    num = JS_AtomIsNumericIndex1(ctx, atom);
    if (likely(JS_IsUndefined(num)))
        return FALSE;
    if (JS_IsException(num))
        return -1;
    JS_FreeValue(ctx, num);
    return TRUE;
}

void JS_FreeAtom(JSContext *ctx, JSAtom v)
{
    if (!__JS_AtomIsConst(v))
        __JS_FreeAtom(ctx->rt, v);
}

void JS_FreeAtomRT(JSRuntime *rt, JSAtom v)
{
    if (!__JS_AtomIsConst(v))
        __JS_FreeAtom(rt, v);
}

/* return TRUE if 'v' is a symbol with a string description */
static BOOL JS_AtomSymbolHasDescription(JSContext *ctx, JSAtom v)
{
    JSRuntime *rt;
    JSAtomStruct *p;

    rt = ctx->rt;
    if (__JS_AtomIsTaggedInt(v))
        return FALSE;
    p = rt->atom_array[v];
    return (((p->atom_type == JS_ATOM_TYPE_SYMBOL &&
              p->hash == JS_ATOM_HASH_SYMBOL) ||
             p->atom_type == JS_ATOM_TYPE_GLOBAL_SYMBOL) &&
            !(p->len == 0 && p->is_wide_char != 0));
}

static __maybe_unused void print_atom(JSContext *ctx, JSAtom atom)
{
    char buf[ATOM_GET_STR_BUF_SIZE];
    const char *p;
    int i;

    /* XXX: should handle embedded null characters */
    /* XXX: should move encoding code to JS_AtomGetStr */
    p = JS_AtomGetStr(ctx, buf, sizeof(buf), atom);
    for (i = 0; p[i]; i++) {
        int c = (unsigned char)p[i];
        if (!((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
              (c == '_' || c == '$') || (c >= '0' && c <= '9' && i > 0)))
            break;
    }
    if (i > 0 && p[i] == '\0') {
        printf("%s", p);
    } else {
        putchar('"');
        printf("%.*s", i, p);
        for (; p[i]; i++) {
            int c = (unsigned char)p[i];
            if (c == '\"' || c == '\\') {
                putchar('\\');
                putchar(c);
            } else if (c >= ' ' && c <= 126) {
                putchar(c);
            } else if (c == '\n') {
                putchar('\\');
                putchar('n');
            } else {
                printf("\\u%04x", c);
            }
        }
        putchar('\"');
    }
}

/* free with JS_FreeCString() */
const char *JS_AtomToCString(JSContext *ctx, JSAtom atom)
{
    JSValue str;
    const char *cstr;

    str = JS_AtomToString(ctx, atom);
    if (JS_IsException(str))
        return NULL;
    cstr = JS_ToCString(ctx, str);
    JS_FreeValue(ctx, str);
    return cstr;
}

/* return a string atom containing name concatenated with str1 */
static JSAtom js_atom_concat_str(JSContext *ctx, JSAtom name, const char *str1)
{
    JSValue str;
    JSAtom atom;
    const char *cstr;
    char *cstr2;
    size_t len, len1;
    
    str = JS_AtomToString(ctx, name);
    if (JS_IsException(str))
        return JS_ATOM_NULL;
    cstr = JS_ToCStringLen(ctx, &len, str);
    if (!cstr)
        goto fail;
    len1 = strlen(str1);
    cstr2 = js_malloc(ctx, len + len1 + 1);
    if (!cstr2)
        goto fail;
    memcpy(cstr2, cstr, len);
    memcpy(cstr2 + len, str1, len1);
    cstr2[len + len1] = '\0';
    atom = JS_NewAtomLen(ctx, cstr2, len + len1);
    js_free(ctx, cstr2);
    JS_FreeCString(ctx, cstr);
    JS_FreeValue(ctx, str);
    return atom;
 fail:
    JS_FreeCString(ctx, cstr);
    JS_FreeValue(ctx, str);
    return JS_ATOM_NULL;
}

static JSAtom js_atom_concat_num(JSContext *ctx, JSAtom name, uint32_t n)
{
    char buf[16];
    snprintf(buf, sizeof(buf), "%u", n);
    return js_atom_concat_str(ctx, name, buf);
}

static inline BOOL JS_IsEmptyString(JSValueConst v)
{
    return JS_VALUE_GET_TAG(v) == JS_TAG_STRING && JS_VALUE_GET_STRING(v)->len == 0;
}

/* JSClass support */

/* a new class ID is allocated if *pclass_id != 0 */
JSClassID JS_NewClassID(JSClassID *pclass_id)
{
    JSClassID class_id;
    /* XXX: make it thread safe */
    class_id = *pclass_id;
    if (class_id == 0) {
        class_id = js_class_id_alloc++;
        *pclass_id = class_id;
    }
    return class_id;
}

BOOL JS_IsRegisteredClass(JSRuntime *rt, JSClassID class_id)
{
    return (class_id < rt->class_count &&
            rt->class_array[class_id].class_id != 0);
}

/* create a new object internal class. Return -1 if error, 0 if
   OK. The finalizer can be NULL if none is needed. */
static int JS_NewClass1(JSRuntime *rt, JSClassID class_id,
                        const JSClassDef *class_def, JSAtom name)
{
    int new_size, i;
    JSClass *cl, *new_class_array;
    struct list_head *el;

    if (class_id >= (1 << 16))
        return -1;
    if (class_id < rt->class_count &&
        rt->class_array[class_id].class_id != 0)
        return -1;

    if (class_id >= rt->class_count) {
        new_size = max_int(JS_CLASS_INIT_COUNT,
                           max_int(class_id + 1, rt->class_count * 3 / 2));

        /* reallocate the context class prototype array, if any */
        list_for_each(el, &rt->context_list) {
            JSContext *ctx = list_entry(el, JSContext, link);
            JSValue *new_tab;
            new_tab = js_realloc_rt(rt, ctx->class_proto,
                                    sizeof(ctx->class_proto[0]) * new_size);
            if (!new_tab)
                return -1;
            for(i = rt->class_count; i < new_size; i++)
                new_tab[i] = JS_NULL;
            ctx->class_proto = new_tab;
        }
        /* reallocate the class array */
        new_class_array = js_realloc_rt(rt, rt->class_array,
                                        sizeof(JSClass) * new_size);
        if (!new_class_array)
            return -1;
        memset(new_class_array + rt->class_count, 0,
               (new_size - rt->class_count) * sizeof(JSClass));
        rt->class_array = new_class_array;
        rt->class_count = new_size;
    }
    cl = &rt->class_array[class_id];
    cl->class_id = class_id;
    cl->class_name = JS_DupAtomRT(rt, name);
    cl->finalizer = class_def->finalizer;
    cl->gc_mark = class_def->gc_mark;
    cl->call = class_def->call;
    cl->exotic = class_def->exotic;
    return 0;
}

int JS_NewClass(JSRuntime *rt, JSClassID class_id, const JSClassDef *class_def)
{
    int ret, len;
    JSAtom name;

    len = strlen(class_def->class_name);
    name = __JS_FindAtom(rt, class_def->class_name, len, JS_ATOM_TYPE_STRING);
    if (name == JS_ATOM_NULL) {
        name = __JS_NewAtomInit(rt, class_def->class_name, len, JS_ATOM_TYPE_STRING);
        if (name == JS_ATOM_NULL)
            return -1;
    }
    ret = JS_NewClass1(rt, class_id, class_def, name);
    JS_FreeAtomRT(rt, name);
    return ret;
}

static JSValue js_new_string8(JSContext *ctx, const uint8_t *buf, int len)
{
    JSString *str;

    if (len <= 0) {
        return JS_AtomToString(ctx, JS_ATOM_empty_string);
    }
    str = js_alloc_string(ctx, len, 0);
    if (!str)
        return JS_EXCEPTION;
    memcpy(str->u.str8, buf, len);
    str->u.str8[len] = '\0';
    return JS_MKPTR(JS_TAG_STRING, str);
}

static JSValue js_new_string16(JSContext *ctx, const uint16_t *buf, int len)
{
    JSString *str;
    str = js_alloc_string(ctx, len, 1);
    if (!str)
        return JS_EXCEPTION;
    memcpy(str->u.str16, buf, len * 2);
    return JS_MKPTR(JS_TAG_STRING, str);
}

static JSValue js_new_string_char(JSContext *ctx, uint16_t c)
{
    if (c < 0x100) {
        uint8_t ch8 = c;
        return js_new_string8(ctx, &ch8, 1);
    } else {
        uint16_t ch16 = c;
        return js_new_string16(ctx, &ch16, 1);
    }
}

static JSValue js_sub_string(JSContext *ctx, JSString *p, int start, int end)
{
    int len = end - start;
    if (start == 0 && end == p->len) {
        return JS_DupValue(ctx, JS_MKPTR(JS_TAG_STRING, p));
    }
    if (p->is_wide_char && len > 0) {
        JSString *str;
        int i;
        uint16_t c = 0;
        for (i = start; i < end; i++) {
            c |= p->u.str16[i];
        }
        if (c > 0xFF)
            return js_new_string16(ctx, p->u.str16 + start, len);

        str = js_alloc_string(ctx, len, 0);
        if (!str)
            return JS_EXCEPTION;
        for (i = 0; i < len; i++) {
            str->u.str8[i] = p->u.str16[start + i];
        }
        str->u.str8[len] = '\0';
        return JS_MKPTR(JS_TAG_STRING, str);
    } else {
        return js_new_string8(ctx, p->u.str8 + start, len);
    }
}

typedef struct StringBuffer {
    JSContext *ctx;
    JSString *str;
    int len;
    int size;
    int is_wide_char;
    int error_status;
} StringBuffer;

/* It is valid to call string_buffer_end() and all string_buffer functions even
   if string_buffer_init() or another string_buffer function returns an error.
   If the error_status is set, string_buffer_end() returns JS_EXCEPTION.
 */
static int string_buffer_init2(JSContext *ctx, StringBuffer *s, int size,
                               int is_wide)
{
    s->ctx = ctx;
    s->size = size;
    s->len = 0;
    s->is_wide_char = is_wide;
    s->error_status = 0;
    s->str = js_alloc_string(ctx, size, is_wide);
    if (unlikely(!s->str)) {
        s->size = 0;
        return s->error_status = -1;
    }
#ifdef DUMP_LEAKS
    /* the StringBuffer may reallocate the JSString, only link it at the end */
    list_del(&s->str->link);
#endif
    return 0;
}

static inline int string_buffer_init(JSContext *ctx, StringBuffer *s, int size)
{
    return string_buffer_init2(ctx, s, size, 0);
}

static void string_buffer_free(StringBuffer *s)
{
    js_free(s->ctx, s->str);
    s->str = NULL;
}

static int string_buffer_set_error(StringBuffer *s)
{
    js_free(s->ctx, s->str);
    s->str = NULL;
    s->size = 0;
    s->len = 0;
    return s->error_status = -1;
}

static no_inline int string_buffer_widen(StringBuffer *s, int size)
{
    JSString *str;
    size_t slack;
    int i;

    if (s->error_status)
        return -1;

    str = js_realloc2(s->ctx, s->str, sizeof(JSString) + (size << 1), &slack);
    if (!str)
        return string_buffer_set_error(s);
    size += slack >> 1;
    for(i = s->len; i-- > 0;) {
        str->u.str16[i] = str->u.str8[i];
    }
    s->is_wide_char = 1;
    s->size = size;
    s->str = str;
    return 0;
}

static no_inline int string_buffer_realloc(StringBuffer *s, int new_len, int c)
{
    JSString *new_str;
    int new_size;
    size_t new_size_bytes, slack;

    if (s->error_status)
        return -1;

    if (new_len > JS_STRING_LEN_MAX) {
        JS_ThrowInternalError(s->ctx, "string too long");
        return string_buffer_set_error(s);
    }
    new_size = min_int(max_int(new_len, s->size * 3 / 2), JS_STRING_LEN_MAX);
    if (!s->is_wide_char && c >= 0x100) {
        return string_buffer_widen(s, new_size);
    }
    new_size_bytes = sizeof(JSString) + (new_size << s->is_wide_char) + 1 - s->is_wide_char;
    new_str = js_realloc2(s->ctx, s->str, new_size_bytes, &slack);
    if (!new_str)
        return string_buffer_set_error(s);
    new_size = min_int(new_size + (slack >> s->is_wide_char), JS_STRING_LEN_MAX);
    s->size = new_size;
    s->str = new_str;
    return 0;
}

static no_inline int string_buffer_putc_slow(StringBuffer *s, uint32_t c)
{
    if (unlikely(s->len >= s->size)) {
        if (string_buffer_realloc(s, s->len + 1, c))
            return -1;
    }
    if (s->is_wide_char) {
        s->str->u.str16[s->len++] = c;
    } else if (c < 0x100) {
        s->str->u.str8[s->len++] = c;
    } else {
        if (string_buffer_widen(s, s->size))
            return -1;
        s->str->u.str16[s->len++] = c;
    }
    return 0;
}

/* 0 <= c <= 0xff */
static int string_buffer_putc8(StringBuffer *s, uint32_t c)
{
    if (unlikely(s->len >= s->size)) {
        if (string_buffer_realloc(s, s->len + 1, c))
            return -1;
    }
    if (s->is_wide_char) {
        s->str->u.str16[s->len++] = c;
    } else {
        s->str->u.str8[s->len++] = c;
    }
    return 0;
}

/* 0 <= c <= 0xffff */
static int string_buffer_putc16(StringBuffer *s, uint32_t c)
{
    if (likely(s->len < s->size)) {
        if (s->is_wide_char) {
            s->str->u.str16[s->len++] = c;
            return 0;
        } else if (c < 0x100) {
            s->str->u.str8[s->len++] = c;
            return 0;
        }
    }
    return string_buffer_putc_slow(s, c);
}

/* 0 <= c <= 0x10ffff */
static int string_buffer_putc(StringBuffer *s, uint32_t c)
{
    if (unlikely(c >= 0x10000)) {
        /* surrogate pair */
        c -= 0x10000;
        if (string_buffer_putc16(s, (c >> 10) + 0xd800))
            return -1;
        c = (c & 0x3ff) + 0xdc00;
    }
    return string_buffer_putc16(s, c);
}

static int string_get(const JSString *p, int idx) {
    return p->is_wide_char ? p->u.str16[idx] : p->u.str8[idx];
}

static int string_getc(const JSString *p, int *pidx)
{
    int idx, c, c1;
    idx = *pidx;
    if (p->is_wide_char) {
        c = p->u.str16[idx++];
        if (c >= 0xd800 && c < 0xdc00 && idx < p->len) {
            c1 = p->u.str16[idx];
            if (c1 >= 0xdc00 && c1 < 0xe000) {
                c = (((c & 0x3ff) << 10) | (c1 & 0x3ff)) + 0x10000;
                idx++;
            }
        }
    } else {
        c = p->u.str8[idx++];
    }
    *pidx = idx;
    return c;
}

static int string_buffer_write8(StringBuffer *s, const uint8_t *p, int len)
{
    int i;

    if (s->len + len > s->size) {
        if (string_buffer_realloc(s, s->len + len, 0))
            return -1;
    }
    if (s->is_wide_char) {
        for (i = 0; i < len; i++) {
            s->str->u.str16[s->len + i] = p[i];
        }
        s->len += len;
    } else {
        memcpy(&s->str->u.str8[s->len], p, len);
        s->len += len;
    }
    return 0;
}

static int string_buffer_write16(StringBuffer *s, const uint16_t *p, int len)
{
    int c = 0, i;

    for (i = 0; i < len; i++) {
        c |= p[i];
    }
    if (s->len + len > s->size) {
        if (string_buffer_realloc(s, s->len + len, c))
            return -1;
    } else if (!s->is_wide_char && c >= 0x100) {
        if (string_buffer_widen(s, s->size))
            return -1;
    }
    if (s->is_wide_char) {
        memcpy(&s->str->u.str16[s->len], p, len << 1);
        s->len += len;
    } else {
        for (i = 0; i < len; i++) {
            s->str->u.str8[s->len + i] = p[i];
        }
        s->len += len;
    }
    return 0;
}

/* appending an ASCII string */
static int string_buffer_puts8(StringBuffer *s, const char *str)
{
    return string_buffer_write8(s, (const uint8_t *)str, strlen(str));
}

static int string_buffer_concat(StringBuffer *s, const JSString *p,
                                uint32_t from, uint32_t to)
{
    if (to <= from)
        return 0;
    if (p->is_wide_char)
        return string_buffer_write16(s, p->u.str16 + from, to - from);
    else
        return string_buffer_write8(s, p->u.str8 + from, to - from);
}

static int string_buffer_concat_value(StringBuffer *s, JSValueConst v)
{
    JSString *p;
    JSValue v1;
    int res;

    if (s->error_status) {
        /* prevent exception overload */
        return -1;
    }
    if (unlikely(JS_VALUE_GET_TAG(v) != JS_TAG_STRING)) {
        v1 = JS_ToString(s->ctx, v);
        if (JS_IsException(v1))
            return string_buffer_set_error(s);
        p = JS_VALUE_GET_STRING(v1);
        res = string_buffer_concat(s, p, 0, p->len);
        JS_FreeValue(s->ctx, v1);
        return res;
    }
    p = JS_VALUE_GET_STRING(v);
    return string_buffer_concat(s, p, 0, p->len);
}

static int string_buffer_concat_value_free(StringBuffer *s, JSValue v)
{
    JSString *p;
    int res;

    if (s->error_status) {
        /* prevent exception overload */
        JS_FreeValue(s->ctx, v);
        return -1;
    }
    if (unlikely(JS_VALUE_GET_TAG(v) != JS_TAG_STRING)) {
        v = JS_ToStringFree(s->ctx, v);
        if (JS_IsException(v))
            return string_buffer_set_error(s);
    }
    p = JS_VALUE_GET_STRING(v);
    res = string_buffer_concat(s, p, 0, p->len);
    JS_FreeValue(s->ctx, v);
    return res;
}

static int string_buffer_fill(StringBuffer *s, int c, int count)
{
    /* XXX: optimize */
    if (s->len + count > s->size) {
        if (string_buffer_realloc(s, s->len + count, c))
            return -1;
    }
    while (count-- > 0) {
        if (string_buffer_putc16(s, c))
            return -1;
    }
    return 0;
}

static JSValue string_buffer_end(StringBuffer *s)
{
    JSString *str;
    str = s->str;
    if (s->error_status)
        return JS_EXCEPTION;
    if (s->len == 0) {
        js_free(s->ctx, str);
        s->str = NULL;
        return JS_AtomToString(s->ctx, JS_ATOM_empty_string);
    }
    if (s->len < s->size) {
        /* smaller size so js_realloc should not fail, but OK if it does */
        /* XXX: should add some slack to avoid unnecessary calls */
        /* XXX: might need to use malloc+free to ensure smaller size */
        str = js_realloc_rt(s->ctx->rt, str, sizeof(JSString) +
                            (s->len << s->is_wide_char) + 1 - s->is_wide_char);
        if (str == NULL)
            str = s->str;
        s->str = str;
    }
    if (!s->is_wide_char)
        str->u.str8[s->len] = 0;
#ifdef DUMP_LEAKS
    list_add_tail(&str->link, &s->ctx->rt->string_list);
#endif
    str->is_wide_char = s->is_wide_char;
    str->len = s->len;
    s->str = NULL;
    return JS_MKPTR(JS_TAG_STRING, str);
}

/* create a string from a UTF-8 buffer */
JSValue JS_NewStringLen(JSContext *ctx, const char *buf, size_t buf_len)
{
    const uint8_t *p, *p_end, *p_start, *p_next;
    uint32_t c;
    StringBuffer b_s, *b = &b_s;
    size_t len1;
    
    p_start = (const uint8_t *)buf;
    p_end = p_start + buf_len;
    p = p_start;
    while (p < p_end && *p < 128)
        p++;
    len1 = p - p_start;
    if (len1 > JS_STRING_LEN_MAX)
        return JS_ThrowInternalError(ctx, "string too long");
    if (p == p_end) {
        /* ASCII string */
        return js_new_string8(ctx, (const uint8_t *)buf, buf_len);
    } else {
        if (string_buffer_init(ctx, b, buf_len))
            goto fail;
        string_buffer_write8(b, p_start, len1);
        while (p < p_end) {
            if (*p < 128) {
                string_buffer_putc8(b, *p++);
            } else {
                /* parse utf-8 sequence, return 0xFFFFFFFF for error */
                c = unicode_from_utf8(p, p_end - p, &p_next);
                if (c < 0x10000) {
                    p = p_next;
                } else if (c <= 0x10FFFF) {
                    p = p_next;
                    /* surrogate pair */
                    c -= 0x10000;
                    string_buffer_putc16(b, (c >> 10) + 0xd800);
                    c = (c & 0x3ff) + 0xdc00;
                } else {
                    /* invalid char */
                    c = 0xfffd;
                    /* skip the invalid chars */
                    /* XXX: seems incorrect. Why not just use c = *p++; ? */
                    while (p < p_end && (*p >= 0x80 && *p < 0xc0))
                        p++;
                    if (p < p_end) {
                        p++;
                        while (p < p_end && (*p >= 0x80 && *p < 0xc0))
                            p++;
                    }
                }
                string_buffer_putc16(b, c);
            }
        }
    }
    return string_buffer_end(b);

 fail:
    string_buffer_free(b);
    return JS_EXCEPTION;
}

static JSValue JS_ConcatString3(JSContext *ctx, const char *str1,
                                JSValue str2, const char *str3)
{
    StringBuffer b_s, *b = &b_s;
    int len1, len3;
    JSString *p;

    if (unlikely(JS_VALUE_GET_TAG(str2) != JS_TAG_STRING)) {
        str2 = JS_ToStringFree(ctx, str2);
        if (JS_IsException(str2))
            goto fail;
    }
    p = JS_VALUE_GET_STRING(str2);
    len1 = strlen(str1);
    len3 = strlen(str3);

    if (string_buffer_init2(ctx, b, len1 + p->len + len3, p->is_wide_char))
        goto fail;

    string_buffer_write8(b, (const uint8_t *)str1, len1);
    string_buffer_concat(b, p, 0, p->len);
    string_buffer_write8(b, (const uint8_t *)str3, len3);

    JS_FreeValue(ctx, str2);
    return string_buffer_end(b);

 fail:
    JS_FreeValue(ctx, str2);
    return JS_EXCEPTION;
}

JSValue JS_NewString(JSContext *ctx, const char *str)
{
    return JS_NewStringLen(ctx, str, strlen(str));
}

JSValue JS_NewAtomString(JSContext *ctx, const char *str)
{
    JSAtom atom = JS_NewAtom(ctx, str);
    if (atom == JS_ATOM_NULL)
        return JS_EXCEPTION;
    JSValue val = JS_AtomToString(ctx, atom);
    JS_FreeAtom(ctx, atom);
    return val;
}

/* return (NULL, 0) if exception. */
/* return pointer into a JSString with a live ref_count */
/* cesu8 determines if non-BMP1 codepoints are encoded as 1 or 2 utf-8 sequences */
const char *JS_ToCStringLen2(JSContext *ctx, size_t *plen, JSValueConst val1, BOOL cesu8)
{
    JSValue val;
    JSString *str, *str_new;
    int pos, len, c, c1;
    uint8_t *q;

    if (JS_VALUE_GET_TAG(val1) != JS_TAG_STRING) {
        val = JS_ToString(ctx, val1);
        if (JS_IsException(val))
            goto fail;
    } else {
        val = JS_DupValue(ctx, val1);
    }

    str = JS_VALUE_GET_STRING(val);
    len = str->len;
    if (!str->is_wide_char) {
        const uint8_t *src = str->u.str8;
        int count;

        /* count the number of non-ASCII characters */
        /* Scanning the whole string is required for ASCII strings,
           and computing the number of non-ASCII bytes is less expensive
           than testing each byte, hence this method is faster for ASCII
           strings, which is the most common case.
         */
        count = 0;
        for (pos = 0; pos < len; pos++) {
            count += src[pos] >> 7;
        }
        if (count == 0) {
            if (plen)
                *plen = len;
            return (const char *)src;
        }
        str_new = js_alloc_string(ctx, len + count, 0);
        if (!str_new)
            goto fail;
        q = str_new->u.str8;
        for (pos = 0; pos < len; pos++) {
            c = src[pos];
            if (c < 0x80) {
                *q++ = c;
            } else {
                *q++ = (c >> 6) | 0xc0;
                *q++ = (c & 0x3f) | 0x80;
            }
        }
    } else {
        const uint16_t *src = str->u.str16;
        /* Allocate 3 bytes per 16 bit code point. Surrogate pairs may
           produce 4 bytes but use 2 code points.
         */
        str_new = js_alloc_string(ctx, len * 3, 0);
        if (!str_new)
            goto fail;
        q = str_new->u.str8;
        pos = 0;
        while (pos < len) {
            c = src[pos++];
            if (c < 0x80) {
                *q++ = c;
            } else {
                if (c >= 0xd800 && c < 0xdc00) {
                    if (pos < len && !cesu8) {
                        c1 = src[pos];
                        if (c1 >= 0xdc00 && c1 < 0xe000) {
                            pos++;
                            /* surrogate pair */
                            c = (((c & 0x3ff) << 10) | (c1 & 0x3ff)) + 0x10000;
                        } else {
                            /* Keep unmatched surrogate code points */
                            /* c = 0xfffd; */ /* error */
                        }
                    } else {
                        /* Keep unmatched surrogate code points */
                        /* c = 0xfffd; */ /* error */
                    }
                }
                q += unicode_to_utf8(q, c);
            }
        }
    }

    *q = '\0';
    str_new->len = q - str_new->u.str8;
    JS_FreeValue(ctx, val);
    if (plen)
        *plen = str_new->len;
    return (const char *)str_new->u.str8;
 fail:
    if (plen)
        *plen = 0;
    return NULL;
}

void JS_FreeCString(JSContext *ctx, const char *ptr)
{
    JSString *p;
    if (!ptr)
        return;
    /* purposely removing constness */
    p = (JSString *)(void *)(ptr - offsetof(JSString, u));
    JS_FreeValue(ctx, JS_MKPTR(JS_TAG_STRING, p));
}

static int memcmp16_8(const uint16_t *src1, const uint8_t *src2, int len)
{
    int c, i;
    for(i = 0; i < len; i++) {
        c = src1[i] - src2[i];
        if (c != 0)
            return c;
    }
    return 0;
}

static int memcmp16(const uint16_t *src1, const uint16_t *src2, int len)
{
    int c, i;
    for(i = 0; i < len; i++) {
        c = src1[i] - src2[i];
        if (c != 0)
            return c;
    }
    return 0;
}

static int js_string_memcmp(const JSString *p1, const JSString *p2, int len)
{
    int res;

    if (likely(!p1->is_wide_char)) {
        if (likely(!p2->is_wide_char))
            res = memcmp(p1->u.str8, p2->u.str8, len);
        else
            res = -memcmp16_8(p2->u.str16, p1->u.str8, len);
    } else {
        if (!p2->is_wide_char)
            res = memcmp16_8(p1->u.str16, p2->u.str8, len);
        else
            res = memcmp16(p1->u.str16, p2->u.str16, len);
    }
    return res;
}

/* return < 0, 0 or > 0 */
static int js_string_compare(JSContext *ctx,
                             const JSString *p1, const JSString *p2)
{
    int res, len;
    len = min_int(p1->len, p2->len);
    res = js_string_memcmp(p1, p2, len);
    if (res == 0) {
        if (p1->len == p2->len)
            res = 0;
        else if (p1->len < p2->len)
            res = -1;
        else
            res = 1;
    }
    return res;
}

static void copy_str16(uint16_t *dst, const JSString *p, int offset, int len)
{
    if (p->is_wide_char) {
        memcpy(dst, p->u.str16 + offset, len * 2);
    } else {
        const uint8_t *src1 = p->u.str8 + offset;
        int i;

        for(i = 0; i < len; i++)
            dst[i] = src1[i];
    }
}

static JSValue JS_ConcatString1(JSContext *ctx,
                                const JSString *p1, const JSString *p2)
{
    JSString *p;
    uint32_t len;
    int is_wide_char;

    len = p1->len + p2->len;
    if (len > JS_STRING_LEN_MAX)
        return JS_ThrowInternalError(ctx, "string too long");
    is_wide_char = p1->is_wide_char | p2->is_wide_char;
    p = js_alloc_string(ctx, len, is_wide_char);
    if (!p)
        return JS_EXCEPTION;
    if (!is_wide_char) {
        memcpy(p->u.str8, p1->u.str8, p1->len);
        memcpy(p->u.str8 + p1->len, p2->u.str8, p2->len);
        p->u.str8[len] = '\0';
    } else {
        copy_str16(p->u.str16, p1, 0, p1->len);
        copy_str16(p->u.str16 + p1->len, p2, 0, p2->len);
    }
    return JS_MKPTR(JS_TAG_STRING, p);
}

/* op1 and op2 are converted to strings. For convience, op1 or op2 =
   JS_EXCEPTION are accepted and return JS_EXCEPTION.  */
static JSValue JS_ConcatString(JSContext *ctx, JSValue op1, JSValue op2)
{
    JSValue ret;
    JSString *p1, *p2;

    if (unlikely(JS_VALUE_GET_TAG(op1) != JS_TAG_STRING)) {
        op1 = JS_ToStringFree(ctx, op1);
        if (JS_IsException(op1)) {
            JS_FreeValue(ctx, op2);
            return JS_EXCEPTION;
        }
    }
    if (unlikely(JS_VALUE_GET_TAG(op2) != JS_TAG_STRING)) {
        op2 = JS_ToStringFree(ctx, op2);
        if (JS_IsException(op2)) {
            JS_FreeValue(ctx, op1);
            return JS_EXCEPTION;
        }
    }
    p1 = JS_VALUE_GET_STRING(op1);
    p2 = JS_VALUE_GET_STRING(op2);

    /* XXX: could also check if p1 is empty */
    if (p2->len == 0) {
        goto ret_op1;
    }
    if (p1->header.ref_count == 1 && p1->is_wide_char == p2->is_wide_char
    &&  js_malloc_usable_size(ctx, p1) >= sizeof(*p1) + ((p1->len + p2->len) << p2->is_wide_char) + 1 - p1->is_wide_char) {
        /* Concatenate in place in available space at the end of p1 */
        if (p1->is_wide_char) {
            memcpy(p1->u.str16 + p1->len, p2->u.str16, p2->len << 1);
            p1->len += p2->len;
        } else {
            memcpy(p1->u.str8 + p1->len, p2->u.str8, p2->len);
            p1->len += p2->len;
            p1->u.str8[p1->len] = '\0';
        }
    ret_op1:
        JS_FreeValue(ctx, op2);
        return op1;
    }
    ret = JS_ConcatString1(ctx, p1, p2);
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    return ret;
}

/* Shape support */

static inline size_t get_shape_size(size_t hash_size, size_t prop_size)
{
    return hash_size * sizeof(uint32_t) + sizeof(JSShape) +
        prop_size * sizeof(JSShapeProperty);
}

static inline JSShape *get_shape_from_alloc(void *sh_alloc, size_t hash_size)
{
    return (JSShape *)(void *)((uint32_t *)sh_alloc + hash_size);
}

static inline uint32_t *prop_hash_end(JSShape *sh)
{
    return (uint32_t *)sh;
}

static inline void *get_alloc_from_shape(JSShape *sh)
{
    return prop_hash_end(sh) - ((intptr_t)sh->prop_hash_mask + 1);
}

static inline JSShapeProperty *get_shape_prop(JSShape *sh)
{
    return sh->prop;
}

static int init_shape_hash(JSRuntime *rt)
{
    rt->shape_hash_bits = 4;   /* 16 shapes */
    rt->shape_hash_size = 1 << rt->shape_hash_bits;
    rt->shape_hash_count = 0;
    rt->shape_hash = js_mallocz_rt(rt, sizeof(rt->shape_hash[0]) *
                                   rt->shape_hash_size);
    if (!rt->shape_hash)
        return -1;
    return 0;
}

/* same magic hash multiplier as the Linux kernel */
static uint32_t shape_hash(uint32_t h, uint32_t val)
{
    return (h + val) * 0x9e370001;
}

/* truncate the shape hash to 'hash_bits' bits */
static uint32_t get_shape_hash(uint32_t h, int hash_bits)
{
    return h >> (32 - hash_bits);
}

static uint32_t shape_initial_hash(JSObject *proto)
{
    uint32_t h;
    h = shape_hash(1, (uintptr_t)proto);
    if (sizeof(proto) > 4)
        h = shape_hash(h, (uint64_t)(uintptr_t)proto >> 32);
    return h;
}

static int resize_shape_hash(JSRuntime *rt, int new_shape_hash_bits)
{
    int new_shape_hash_size, i;
    uint32_t h;
    JSShape **new_shape_hash, *sh, *sh_next;

    new_shape_hash_size = 1 << new_shape_hash_bits;
    new_shape_hash = js_mallocz_rt(rt, sizeof(rt->shape_hash[0]) *
                                   new_shape_hash_size);
    if (!new_shape_hash)
        return -1;
    for(i = 0; i < rt->shape_hash_size; i++) {
        for(sh = rt->shape_hash[i]; sh != NULL; sh = sh_next) {
            sh_next = sh->shape_hash_next;
            h = get_shape_hash(sh->hash, new_shape_hash_bits);
            sh->shape_hash_next = new_shape_hash[h];
            new_shape_hash[h] = sh;
        }
    }
    js_free_rt(rt, rt->shape_hash);
    rt->shape_hash_bits = new_shape_hash_bits;
    rt->shape_hash_size = new_shape_hash_size;
    rt->shape_hash = new_shape_hash;
    return 0;
}

static void js_shape_hash_link(JSRuntime *rt, JSShape *sh)
{
    uint32_t h;
    h = get_shape_hash(sh->hash, rt->shape_hash_bits);
    sh->shape_hash_next = rt->shape_hash[h];
    rt->shape_hash[h] = sh;
    rt->shape_hash_count++;
}

static void js_shape_hash_unlink(JSRuntime *rt, JSShape *sh)
{
    uint32_t h;
    JSShape **psh;

    h = get_shape_hash(sh->hash, rt->shape_hash_bits);
    psh = &rt->shape_hash[h];
    while (*psh != sh)
        psh = &(*psh)->shape_hash_next;
    *psh = sh->shape_hash_next;
    rt->shape_hash_count--;
}

/* create a new empty shape with prototype 'proto' */
static no_inline JSShape *js_new_shape2(JSContext *ctx, JSObject *proto,
                                        int hash_size, int prop_size)
{
    JSRuntime *rt = ctx->rt;
    void *sh_alloc;
    JSShape *sh;

    /* resize the shape hash table if necessary */
    if (2 * (rt->shape_hash_count + 1) > rt->shape_hash_size) {
        resize_shape_hash(rt, rt->shape_hash_bits + 1);
    }

    sh_alloc = js_malloc(ctx, get_shape_size(hash_size, prop_size));
    if (!sh_alloc)
        return NULL;
    sh = get_shape_from_alloc(sh_alloc, hash_size);
    sh->header.ref_count = 1;
    add_gc_object(rt, &sh->header, JS_GC_OBJ_TYPE_SHAPE);
    if (proto)
        JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, proto));
    sh->proto = proto;
    memset(prop_hash_end(sh) - hash_size, 0, sizeof(prop_hash_end(sh)[0]) *
           hash_size);
    sh->prop_hash_mask = hash_size - 1;
    sh->prop_size = prop_size;
    sh->prop_count = 0;
    sh->deleted_prop_count = 0;
    
    /* insert in the hash table */
    sh->hash = shape_initial_hash(proto);
    sh->is_hashed = TRUE;
    sh->has_small_array_index = FALSE;
    js_shape_hash_link(ctx->rt, sh);
    return sh;
}

static JSShape *js_new_shape(JSContext *ctx, JSObject *proto)
{
    return js_new_shape2(ctx, proto, JS_PROP_INITIAL_HASH_SIZE,
                         JS_PROP_INITIAL_SIZE);
}

/* The shape is cloned. The new shape is not inserted in the shape
   hash table */
static JSShape *js_clone_shape(JSContext *ctx, JSShape *sh1)
{
    JSShape *sh;
    void *sh_alloc, *sh_alloc1;
    size_t size;
    JSShapeProperty *pr;
    uint32_t i, hash_size;

    hash_size = sh1->prop_hash_mask + 1;
    size = get_shape_size(hash_size, sh1->prop_size);
    sh_alloc = js_malloc(ctx, size);
    if (!sh_alloc)
        return NULL;
    sh_alloc1 = get_alloc_from_shape(sh1);
    memcpy(sh_alloc, sh_alloc1, size);
    sh = get_shape_from_alloc(sh_alloc, hash_size);
    sh->header.ref_count = 1;
    add_gc_object(ctx->rt, &sh->header, JS_GC_OBJ_TYPE_SHAPE);
    sh->is_hashed = FALSE;
    if (sh->proto) {
        JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, sh->proto));
    }
    for(i = 0, pr = get_shape_prop(sh); i < sh->prop_count; i++, pr++) {
        JS_DupAtom(ctx, pr->atom);
    }
    return sh;
}

static JSShape *js_dup_shape(JSShape *sh)
{
    sh->header.ref_count++;
    return sh;
}

static void js_free_shape0(JSRuntime *rt, JSShape *sh)
{
    uint32_t i;
    JSShapeProperty *pr;

    assert(sh->header.ref_count == 0);
    if (sh->is_hashed)
        js_shape_hash_unlink(rt, sh);
    if (sh->proto != NULL) {
        JS_FreeValueRT(rt, JS_MKPTR(JS_TAG_OBJECT, sh->proto));
    }
    pr = get_shape_prop(sh);
    for(i = 0; i < sh->prop_count; i++) {
        JS_FreeAtomRT(rt, pr->atom);
        pr++;
    }
    remove_gc_object(&sh->header);
    js_free_rt(rt, get_alloc_from_shape(sh));
}

static void js_free_shape(JSRuntime *rt, JSShape *sh)
{
    if (unlikely(--sh->header.ref_count <= 0)) {
        js_free_shape0(rt, sh);
    }
}

static void js_free_shape_null(JSRuntime *rt, JSShape *sh)
{
    if (sh)
        js_free_shape(rt, sh);
}

/* make space to hold at least 'count' properties */
static no_inline int resize_properties(JSContext *ctx, JSShape **psh,
                                       JSObject *p, uint32_t count)
{
    JSShape *sh;
    uint32_t new_size, new_hash_size, new_hash_mask, i;
    JSShapeProperty *pr;
    void *sh_alloc;
    intptr_t h;

    sh = *psh;
    new_size = max_int(count, sh->prop_size * 3 / 2);
    /* Reallocate prop array first to avoid crash or size inconsistency
       in case of memory allocation failure */
    if (p) {
        JSProperty *new_prop;
        new_prop = js_realloc(ctx, p->prop, sizeof(new_prop[0]) * new_size);
        if (unlikely(!new_prop))
            return -1;
        p->prop = new_prop;
    }
    new_hash_size = sh->prop_hash_mask + 1;
    while (new_hash_size < new_size)
        new_hash_size = 2 * new_hash_size;
    if (new_hash_size != (sh->prop_hash_mask + 1)) {
        JSShape *old_sh;
        /* resize the hash table and the properties */
        old_sh = sh;
        sh_alloc = js_malloc(ctx, get_shape_size(new_hash_size, new_size));
        if (!sh_alloc)
            return -1;
        sh = get_shape_from_alloc(sh_alloc, new_hash_size);
        list_del(&old_sh->header.link);
        /* copy all the fields and the properties */
        memcpy(sh, old_sh,
               sizeof(JSShape) + sizeof(sh->prop[0]) * old_sh->prop_count);
        list_add_tail(&sh->header.link, &ctx->rt->gc_obj_list);
        new_hash_mask = new_hash_size - 1;
        sh->prop_hash_mask = new_hash_mask;
        memset(prop_hash_end(sh) - new_hash_size, 0,
               sizeof(prop_hash_end(sh)[0]) * new_hash_size);
        for(i = 0, pr = sh->prop; i < sh->prop_count; i++, pr++) {
            if (pr->atom != JS_ATOM_NULL) {
                h = ((uintptr_t)pr->atom & new_hash_mask);
                pr->hash_next = prop_hash_end(sh)[-h - 1];
                prop_hash_end(sh)[-h - 1] = i + 1;
            }
        }
        js_free(ctx, get_alloc_from_shape(old_sh));
    } else {
        /* only resize the properties */
        list_del(&sh->header.link);
        sh_alloc = js_realloc(ctx, get_alloc_from_shape(sh),
                              get_shape_size(new_hash_size, new_size));
        if (unlikely(!sh_alloc)) {
            /* insert again in the GC list */
            list_add_tail(&sh->header.link, &ctx->rt->gc_obj_list);
            return -1;
        }
        sh = get_shape_from_alloc(sh_alloc, new_hash_size);
        list_add_tail(&sh->header.link, &ctx->rt->gc_obj_list);
    }
    *psh = sh;
    sh->prop_size = new_size;
    return 0;
}

/* remove the deleted properties. */
static int compact_properties(JSContext *ctx, JSObject *p)
{
    JSShape *sh, *old_sh;
    void *sh_alloc;
    intptr_t h;
    uint32_t new_hash_size, i, j, new_hash_mask, new_size;
    JSShapeProperty *old_pr, *pr;
    JSProperty *prop, *new_prop;
    
    sh = p->shape;
    assert(!sh->is_hashed);

    new_size = max_int(JS_PROP_INITIAL_SIZE,
                       sh->prop_count - sh->deleted_prop_count);
    assert(new_size <= sh->prop_size);

    new_hash_size = sh->prop_hash_mask + 1;
    while ((new_hash_size / 2) >= new_size)
        new_hash_size = new_hash_size / 2;
    new_hash_mask = new_hash_size - 1;

    /* resize the hash table and the properties */
    old_sh = sh;
    sh_alloc = js_malloc(ctx, get_shape_size(new_hash_size, new_size));
    if (!sh_alloc)
        return -1;
    sh = get_shape_from_alloc(sh_alloc, new_hash_size);
    list_del(&old_sh->header.link);
    memcpy(sh, old_sh, sizeof(JSShape));
    list_add_tail(&sh->header.link, &ctx->rt->gc_obj_list);
    
    memset(prop_hash_end(sh) - new_hash_size, 0,
           sizeof(prop_hash_end(sh)[0]) * new_hash_size);

    j = 0;
    old_pr = old_sh->prop;
    pr = sh->prop;
    prop = p->prop;
    for(i = 0; i < sh->prop_count; i++) {
        if (old_pr->atom != JS_ATOM_NULL) {
            pr->atom = old_pr->atom;
            pr->flags = old_pr->flags;
            h = ((uintptr_t)old_pr->atom & new_hash_mask);
            pr->hash_next = prop_hash_end(sh)[-h - 1];
            prop_hash_end(sh)[-h - 1] = j + 1;
            prop[j] = prop[i];
            j++;
            pr++;
        }
        old_pr++;
    }
    assert(j == (sh->prop_count - sh->deleted_prop_count));
    sh->prop_hash_mask = new_hash_mask;
    sh->prop_size = new_size;
    sh->deleted_prop_count = 0;
    sh->prop_count = j;

    p->shape = sh;
    js_free(ctx, get_alloc_from_shape(old_sh));
    
    /* reduce the size of the object properties */
    new_prop = js_realloc(ctx, p->prop, sizeof(new_prop[0]) * new_size);
    if (new_prop)
        p->prop = new_prop;
    return 0;
}

static int add_shape_property(JSContext *ctx, JSShape **psh,
                              JSObject *p, JSAtom atom, int prop_flags)
{
    JSRuntime *rt = ctx->rt;
    JSShape *sh = *psh;
    JSShapeProperty *pr, *prop;
    uint32_t hash_mask, new_shape_hash = 0;
    intptr_t h;

    /* update the shape hash */
    if (sh->is_hashed) {
        js_shape_hash_unlink(rt, sh);
        new_shape_hash = shape_hash(shape_hash(sh->hash, atom), prop_flags);
    }

    if (unlikely(sh->prop_count >= sh->prop_size)) {
        if (resize_properties(ctx, psh, p, sh->prop_count + 1)) {
            /* in case of error, reinsert in the hash table.
               sh is still valid if resize_properties() failed */
            if (sh->is_hashed)
                js_shape_hash_link(rt, sh);
            return -1;
        }
        sh = *psh;
    }
    if (sh->is_hashed) {
        sh->hash = new_shape_hash;
        js_shape_hash_link(rt, sh);
    }
    /* Initialize the new shape property.
       The object property at p->prop[sh->prop_count] is uninitialized */
    prop = get_shape_prop(sh);
    pr = &prop[sh->prop_count++];
    pr->atom = JS_DupAtom(ctx, atom);
    pr->flags = prop_flags;
    sh->has_small_array_index |= __JS_AtomIsTaggedInt(atom);
    /* add in hash table */
    hash_mask = sh->prop_hash_mask;
    h = atom & hash_mask;
    pr->hash_next = prop_hash_end(sh)[-h - 1];
    prop_hash_end(sh)[-h - 1] = sh->prop_count;
    return 0;
}

/* find a hashed empty shape matching the prototype. Return NULL if
   not found */
static JSShape *find_hashed_shape_proto(JSRuntime *rt, JSObject *proto)
{
    JSShape *sh1;
    uint32_t h, h1;

    h = shape_initial_hash(proto);
    h1 = get_shape_hash(h, rt->shape_hash_bits);
    for(sh1 = rt->shape_hash[h1]; sh1 != NULL; sh1 = sh1->shape_hash_next) {
        if (sh1->hash == h &&
            sh1->proto == proto &&
            sh1->prop_count == 0) {
            return sh1;
        }
    }
    return NULL;
}

/* find a hashed shape matching sh + (prop, prop_flags). Return NULL if
   not found */
static JSShape *find_hashed_shape_prop(JSRuntime *rt, JSShape *sh,
                                       JSAtom atom, int prop_flags)
{
    JSShape *sh1;
    uint32_t h, h1, i, n;

    h = sh->hash;
    h = shape_hash(h, atom);
    h = shape_hash(h, prop_flags);
    h1 = get_shape_hash(h, rt->shape_hash_bits);
    for(sh1 = rt->shape_hash[h1]; sh1 != NULL; sh1 = sh1->shape_hash_next) {
        /* we test the hash first so that the rest is done only if the
           shapes really match */
        if (sh1->hash == h &&
            sh1->proto == sh->proto &&
            sh1->prop_count == ((n = sh->prop_count) + 1)) {
            for(i = 0; i < n; i++) {
                if (unlikely(sh1->prop[i].atom != sh->prop[i].atom) ||
                    unlikely(sh1->prop[i].flags != sh->prop[i].flags))
                    goto next;
            }
            if (unlikely(sh1->prop[n].atom != atom) ||
                unlikely(sh1->prop[n].flags != prop_flags))
                goto next;
            return sh1;
        }
    next: ;
    }
    return NULL;
}

static __maybe_unused void JS_DumpShape(JSRuntime *rt, int i, JSShape *sh)
{
    char atom_buf[ATOM_GET_STR_BUF_SIZE];
    int j;

    /* XXX: should output readable class prototype */
    printf("%5d %3d%c %14p %5d %5d", i,
           sh->header.ref_count, " *"[sh->is_hashed],
           (void *)sh->proto, sh->prop_size, sh->prop_count);
    for(j = 0; j < sh->prop_count; j++) {
        printf(" %s", JS_AtomGetStrRT(rt, atom_buf, sizeof(atom_buf),
                                      sh->prop[j].atom));
    }
    printf("\n");
}

static __maybe_unused void JS_DumpShapes(JSRuntime *rt)
{
    int i;
    JSShape *sh;
    struct list_head *el;
    JSObject *p;
    JSGCObjectHeader *gp;
    
    printf("JSShapes: {\n");
    printf("%5s %4s %14s %5s %5s %s\n", "SLOT", "REFS", "PROTO", "SIZE", "COUNT", "PROPS");
    for(i = 0; i < rt->shape_hash_size; i++) {
        for(sh = rt->shape_hash[i]; sh != NULL; sh = sh->shape_hash_next) {
            JS_DumpShape(rt, i, sh);
            assert(sh->is_hashed);
        }
    }
    /* dump non-hashed shapes */
    list_for_each(el, &rt->gc_obj_list) {
        gp = list_entry(el, JSGCObjectHeader, link);
        if (gp->gc_obj_type == JS_GC_OBJ_TYPE_JS_OBJECT) {
            p = (JSObject *)gp;
            if (!p->shape->is_hashed) {
                JS_DumpShape(rt, -1, p->shape);
            }
        }
    }
    printf("}\n");
}

static JSValue JS_NewObjectFromShape(JSContext *ctx, JSShape *sh, JSClassID class_id)
{
    JSObject *p;

    js_trigger_gc(ctx->rt, sizeof(JSObject));
    p = js_malloc(ctx, sizeof(JSObject));
    if (unlikely(!p))
        goto fail;
    p->class_id = class_id;
    p->extensible = TRUE;
    p->free_mark = 0;
    p->is_exotic = 0;
    p->fast_array = 0;
    p->is_constructor = 0;
    p->is_uncatchable_error = 0;
    p->tmp_mark = 0;
    p->is_HTMLDDA = 0;
    p->first_weak_ref = NULL;
    p->u.opaque = NULL;
    p->shape = sh;
    p->prop = js_malloc(ctx, sizeof(JSProperty) * sh->prop_size);
    if (unlikely(!p->prop)) {
        js_free(ctx, p);
    fail:
        js_free_shape(ctx->rt, sh);
        return JS_EXCEPTION;
    }

    switch(class_id) {
    case JS_CLASS_OBJECT:
        break;
    case JS_CLASS_ARRAY:
        {
            JSProperty *pr;
            p->is_exotic = 1;
            p->fast_array = 1;
            p->u.array.u.values = NULL;
            p->u.array.count = 0;
            p->u.array.u1.size = 0;
            /* the length property is always the first one */
            if (likely(sh == ctx->array_shape)) {
                pr = &p->prop[0];
            } else {
                /* only used for the first array */
                /* cannot fail */
                pr = add_property(ctx, p, JS_ATOM_length,
                                  JS_PROP_WRITABLE | JS_PROP_LENGTH);
            }
            pr->u.value = JS_NewInt32(ctx, 0);
        }
        break;
    case JS_CLASS_C_FUNCTION:
        p->prop[0].u.value = JS_UNDEFINED;
        break;
    case JS_CLASS_ARGUMENTS:
    case JS_CLASS_UINT8C_ARRAY:
    case JS_CLASS_INT8_ARRAY:
    case JS_CLASS_UINT8_ARRAY:
    case JS_CLASS_INT16_ARRAY:
    case JS_CLASS_UINT16_ARRAY:
    case JS_CLASS_INT32_ARRAY:
    case JS_CLASS_UINT32_ARRAY:
#ifdef CONFIG_BIGNUM
    case JS_CLASS_BIG_INT64_ARRAY:
    case JS_CLASS_BIG_UINT64_ARRAY:
#endif
    case JS_CLASS_FLOAT32_ARRAY:
    case JS_CLASS_FLOAT64_ARRAY:
        p->is_exotic = 1;
        p->fast_array = 1;
        p->u.array.u.ptr = NULL;
        p->u.array.count = 0;
        break;
    case JS_CLASS_DATAVIEW:
        p->u.array.u.ptr = NULL;
        p->u.array.count = 0;
        break;
    case JS_CLASS_NUMBER:
    case JS_CLASS_STRING:
    case JS_CLASS_BOOLEAN:
    case JS_CLASS_SYMBOL:
    case JS_CLASS_DATE:
#ifdef CONFIG_BIGNUM
    case JS_CLASS_BIG_INT:
    case JS_CLASS_BIG_FLOAT:
    case JS_CLASS_BIG_DECIMAL:
#endif
        p->u.object_data = JS_UNDEFINED;
        goto set_exotic;
    case JS_CLASS_REGEXP:
        p->u.regexp.pattern = NULL;
        p->u.regexp.bytecode = NULL;
        goto set_exotic;
    default:
    set_exotic:
        if (ctx->rt->class_array[class_id].exotic) {
            p->is_exotic = 1;
        }
        break;
    }
    p->header.ref_count = 1;
    add_gc_object(ctx->rt, &p->header, JS_GC_OBJ_TYPE_JS_OBJECT);
    return JS_MKPTR(JS_TAG_OBJECT, p);
}

static JSObject *get_proto_obj(JSValueConst proto_val)
{
    if (JS_VALUE_GET_TAG(proto_val) != JS_TAG_OBJECT)
        return NULL;
    else
        return JS_VALUE_GET_OBJ(proto_val);
}

/* WARNING: proto must be an object or JS_NULL */
JSValue JS_NewObjectProtoClass(JSContext *ctx, JSValueConst proto_val,
                               JSClassID class_id)
{
    JSShape *sh;
    JSObject *proto;

    proto = get_proto_obj(proto_val);
    sh = find_hashed_shape_proto(ctx->rt, proto);
    if (likely(sh)) {
        sh = js_dup_shape(sh);
    } else {
        sh = js_new_shape(ctx, proto);
        if (!sh)
            return JS_EXCEPTION;
    }
    return JS_NewObjectFromShape(ctx, sh, class_id);
}

#if 0
static JSValue JS_GetObjectData(JSContext *ctx, JSValueConst obj)
{
    JSObject *p;

    if (JS_VALUE_GET_TAG(obj) == JS_TAG_OBJECT) {
        p = JS_VALUE_GET_OBJ(obj);
        switch(p->class_id) {
        case JS_CLASS_NUMBER:
        case JS_CLASS_STRING:
        case JS_CLASS_BOOLEAN:
        case JS_CLASS_SYMBOL:
        case JS_CLASS_DATE:
#ifdef CONFIG_BIGNUM
        case JS_CLASS_BIG_INT:
        case JS_CLASS_BIG_FLOAT:
        case JS_CLASS_BIG_DECIMAL:
#endif
            return JS_DupValue(ctx, p->u.object_data);
        }
    }
    return JS_UNDEFINED;
}
#endif

static int JS_SetObjectData(JSContext *ctx, JSValueConst obj, JSValue val)
{
    JSObject *p;

    if (JS_VALUE_GET_TAG(obj) == JS_TAG_OBJECT) {
        p = JS_VALUE_GET_OBJ(obj);
        switch(p->class_id) {
        case JS_CLASS_NUMBER:
        case JS_CLASS_STRING:
        case JS_CLASS_BOOLEAN:
        case JS_CLASS_SYMBOL:
        case JS_CLASS_DATE:
#ifdef CONFIG_BIGNUM
        case JS_CLASS_BIG_INT:
        case JS_CLASS_BIG_FLOAT:
        case JS_CLASS_BIG_DECIMAL:
#endif
            JS_FreeValue(ctx, p->u.object_data);
            p->u.object_data = val;
            return 0;
        }
    }
    JS_FreeValue(ctx, val);
    if (!JS_IsException(obj))
        JS_ThrowTypeError(ctx, "invalid object type");
    return -1;
}

JSValue JS_NewObjectClass(JSContext *ctx, int class_id)
{
    return JS_NewObjectProtoClass(ctx, ctx->class_proto[class_id], class_id);
}

JSValue JS_NewObjectProto(JSContext *ctx, JSValueConst proto)
{
    return JS_NewObjectProtoClass(ctx, proto, JS_CLASS_OBJECT);
}

JSValue JS_NewArray(JSContext *ctx)
{
    return JS_NewObjectFromShape(ctx, js_dup_shape(ctx->array_shape),
                                 JS_CLASS_ARRAY);
}

JSValue JS_NewObject(JSContext *ctx)
{
    /* inline JS_NewObjectClass(ctx, JS_CLASS_OBJECT); */
    return JS_NewObjectProtoClass(ctx, ctx->class_proto[JS_CLASS_OBJECT], JS_CLASS_OBJECT);
}

static void js_function_set_properties(JSContext *ctx, JSValueConst func_obj,
                                       JSAtom name, int len)
{
    /* ES6 feature non compatible with ES5.1: length is configurable */
    JS_DefinePropertyValue(ctx, func_obj, JS_ATOM_length, JS_NewInt32(ctx, len),
                           JS_PROP_CONFIGURABLE);
    JS_DefinePropertyValue(ctx, func_obj, JS_ATOM_name,
                           JS_AtomToString(ctx, name), JS_PROP_CONFIGURABLE);
}

static BOOL js_class_has_bytecode(JSClassID class_id)
{
    return (class_id == JS_CLASS_BYTECODE_FUNCTION ||
            class_id == JS_CLASS_GENERATOR_FUNCTION ||
            class_id == JS_CLASS_ASYNC_FUNCTION ||
            class_id == JS_CLASS_ASYNC_GENERATOR_FUNCTION);
}

/* return NULL without exception if not a function or no bytecode */
static JSFunctionBytecode *JS_GetFunctionBytecode(JSValueConst val)
{
    JSObject *p;
    if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
        return NULL;
    p = JS_VALUE_GET_OBJ(val);
    if (!js_class_has_bytecode(p->class_id))
        return NULL;
    return p->u.func.function_bytecode;
}

static void js_method_set_home_object(JSContext *ctx, JSValueConst func_obj,
                                      JSValueConst home_obj)
{
    JSObject *p, *p1;
    JSFunctionBytecode *b;

    if (JS_VALUE_GET_TAG(func_obj) != JS_TAG_OBJECT)
        return;
    p = JS_VALUE_GET_OBJ(func_obj);
    if (!js_class_has_bytecode(p->class_id))
        return;
    b = p->u.func.function_bytecode;
    if (b->need_home_object) {
        p1 = p->u.func.home_object;
        if (p1) {
            JS_FreeValue(ctx, JS_MKPTR(JS_TAG_OBJECT, p1));
        }
        if (JS_VALUE_GET_TAG(home_obj) == JS_TAG_OBJECT)
            p1 = JS_VALUE_GET_OBJ(JS_DupValue(ctx, home_obj));
        else
            p1 = NULL;
        p->u.func.home_object = p1;
    }
}

static JSValue js_get_function_name(JSContext *ctx, JSAtom name)
{
    JSValue name_str;

    name_str = JS_AtomToString(ctx, name);
    if (JS_AtomSymbolHasDescription(ctx, name)) {
        name_str = JS_ConcatString3(ctx, "[", name_str, "]");
    }
    return name_str;
}

/* Modify the name of a method according to the atom and
   'flags'. 'flags' is a bitmask of JS_PROP_HAS_GET and
   JS_PROP_HAS_SET. Also set the home object of the method.
   Return < 0 if exception. */
static int js_method_set_properties(JSContext *ctx, JSValueConst func_obj,
                                    JSAtom name, int flags, JSValueConst home_obj)
{
    JSValue name_str;

    name_str = js_get_function_name(ctx, name);
    if (flags & JS_PROP_HAS_GET) {
        name_str = JS_ConcatString3(ctx, "get ", name_str, "");
    } else if (flags & JS_PROP_HAS_SET) {
        name_str = JS_ConcatString3(ctx, "set ", name_str, "");
    }
    if (JS_IsException(name_str))
        return -1;
    if (JS_DefinePropertyValue(ctx, func_obj, JS_ATOM_name, name_str,
                               JS_PROP_CONFIGURABLE) < 0)
        return -1;
    js_method_set_home_object(ctx, func_obj, home_obj);
    return 0;
}

/* Note: at least 'length' arguments will be readable in 'argv' */
static JSValue JS_NewCFunction3(JSContext *ctx, JSCFunction *func,
                                const char *name,
                                int length, JSCFunctionEnum cproto, int magic,
                                JSValueConst proto_val)
{
    JSValue func_obj;
    JSObject *p;
    JSAtom name_atom;
    
    func_obj = JS_NewObjectProtoClass(ctx, proto_val, JS_CLASS_C_FUNCTION);
    if (JS_IsException(func_obj))
        return func_obj;
    p = JS_VALUE_GET_OBJ(func_obj);
    p->u.cfunc.realm = JS_DupContext(ctx);
    p->u.cfunc.c_function.generic = func;
    p->u.cfunc.length = length;
    p->u.cfunc.cproto = cproto;
    p->u.cfunc.magic = magic;
    p->is_constructor = (cproto == JS_CFUNC_constructor ||
                         cproto == JS_CFUNC_constructor_magic ||
                         cproto == JS_CFUNC_constructor_or_func ||
                         cproto == JS_CFUNC_constructor_or_func_magic);
    if (!name)
        name = "";
    name_atom = JS_NewAtom(ctx, name);
    js_function_set_properties(ctx, func_obj, name_atom, length);
    JS_FreeAtom(ctx, name_atom);
    return func_obj;
}

/* Note: at least 'length' arguments will be readable in 'argv' */
JSValue JS_NewCFunction2(JSContext *ctx, JSCFunction *func,
                         const char *name,
                         int length, JSCFunctionEnum cproto, int magic)
{
    return JS_NewCFunction3(ctx, func, name, length, cproto, magic,
                            ctx->function_proto);
}

typedef struct JSCFunctionDataRecord {
    JSCFunctionData *func;
    uint8_t length;
    uint8_t data_len;
    uint16_t magic;
    JSValue data[0];
} JSCFunctionDataRecord;

static void js_c_function_data_finalizer(JSRuntime *rt, JSValue val)
{
    JSCFunctionDataRecord *s = JS_GetOpaque(val, JS_CLASS_C_FUNCTION_DATA);
    int i;

    if (s) {
        for(i = 0; i < s->data_len; i++) {
            JS_FreeValueRT(rt, s->data[i]);
        }
        js_free_rt(rt, s);
    }
}

static void js_c_function_data_mark(JSRuntime *rt, JSValueConst val,
                                    JS_MarkFunc *mark_func)
{
    JSCFunctionDataRecord *s = JS_GetOpaque(val, JS_CLASS_C_FUNCTION_DATA);
    int i;

    if (s) {
        for(i = 0; i < s->data_len; i++) {
            JS_MarkValue(rt, s->data[i], mark_func);
        }
    }
}

static JSValue js_c_function_data_call(JSContext *ctx, JSValueConst func_obj,
                                       JSValueConst this_val,
                                       int argc, JSValueConst *argv, int flags)
{
    JSCFunctionDataRecord *s = JS_GetOpaque(func_obj, JS_CLASS_C_FUNCTION_DATA);
    JSValueConst *arg_buf;
    int i;

    /* XXX: could add the function on the stack for debug */
    if (unlikely(argc < s->length)) {
        arg_buf = alloca(sizeof(arg_buf[0]) * s->length);
        for(i = 0; i < argc; i++)
            arg_buf[i] = argv[i];
        for(i = argc; i < s->length; i++)
            arg_buf[i] = JS_UNDEFINED;
    } else {
        arg_buf = argv;
    }

    return s->func(ctx, this_val, argc, arg_buf, s->magic, s->data);
}

JSValue JS_NewCFunctionData(JSContext *ctx, JSCFunctionData *func,
                            int length, int magic, int data_len,
                            JSValueConst *data)
{
    JSCFunctionDataRecord *s;
    JSValue func_obj;
    int i;

    func_obj = JS_NewObjectProtoClass(ctx, ctx->function_proto,
                                      JS_CLASS_C_FUNCTION_DATA);
    if (JS_IsException(func_obj))
        return func_obj;
    s = js_malloc(ctx, sizeof(*s) + data_len * sizeof(JSValue));
    if (!s) {
        JS_FreeValue(ctx, func_obj);
        return JS_EXCEPTION;
    }
    s->func = func;
    s->length = length;
    s->data_len = data_len;
    s->magic = magic;
    for(i = 0; i < data_len; i++)
        s->data[i] = JS_DupValue(ctx, data[i]);
    JS_SetOpaque(func_obj, s);
    js_function_set_properties(ctx, func_obj,
                               JS_ATOM_empty_string, length);
    return func_obj;
}

static JSContext *js_autoinit_get_realm(JSProperty *pr)
{
    return (JSContext *)(pr->u.init.realm_and_id & ~3);
}

static JSAutoInitIDEnum js_autoinit_get_id(JSProperty *pr)
{
    return pr->u.init.realm_and_id & 3;
}

static void js_autoinit_free(JSRuntime *rt, JSProperty *pr)
{
    JS_FreeContext(js_autoinit_get_realm(pr));
}

static void js_autoinit_mark(JSRuntime *rt, JSProperty *pr,
                             JS_MarkFunc *mark_func)
{
    mark_func(rt, &js_autoinit_get_realm(pr)->header);
}

static void free_property(JSRuntime *rt, JSProperty *pr, int prop_flags)
{
    if (unlikely(prop_flags & JS_PROP_TMASK)) {
        if ((prop_flags & JS_PROP_TMASK) == JS_PROP_GETSET) {
            if (pr->u.getset.getter)
                JS_FreeValueRT(rt, JS_MKPTR(JS_TAG_OBJECT, pr->u.getset.getter));
            if (pr->u.getset.setter)
                JS_FreeValueRT(rt, JS_MKPTR(JS_TAG_OBJECT, pr->u.getset.setter));
        } else if ((prop_flags & JS_PROP_TMASK) == JS_PROP_VARREF) {
            free_var_ref(rt, pr->u.var_ref);
        } else if ((prop_flags & JS_PROP_TMASK) == JS_PROP_AUTOINIT) {
            js_autoinit_free(rt, pr);
        }
    } else {
        JS_FreeValueRT(rt, pr->u.value);
    }
}

static force_inline JSShapeProperty *find_own_property1(JSObject *p,
                                                        JSAtom atom)
{
    JSShape *sh;
    JSShapeProperty *pr, *prop;
    intptr_t h;
    sh = p->shape;
    h = (uintptr_t)atom & sh->prop_hash_mask;
    h = prop_hash_end(sh)[-h - 1];
    prop = get_shape_prop(sh);
    while (h) {
        pr = &prop[h - 1];
        if (likely(pr->atom == atom)) {
            return pr;
        }
        h = pr->hash_next;
    }
    return NULL;
}

static force_inline JSShapeProperty *find_own_property(JSProperty **ppr,
                                                       JSObject *p,
                                                       JSAtom atom)
{
    JSShape *sh;
    JSShapeProperty *pr, *prop;
    intptr_t h;
    sh = p->shape;
    h = (uintptr_t)atom & sh->prop_hash_mask;
    h = prop_hash_end(sh)[-h - 1];
    prop = get_shape_prop(sh);
    while (h) {
        pr = &prop[h - 1];
        if (likely(pr->atom == atom)) {
            *ppr = &p->prop[h - 1];
            /* the compiler should be able to assume that pr != NULL here */
            return pr;
        }
        h = pr->hash_next;
    }
    *ppr = NULL;
    return NULL;
}

/* indicate that the object may be part of a function prototype cycle */
static void set_cycle_flag(JSContext *ctx, JSValueConst obj)
{
}

static void free_var_ref(JSRuntime *rt, JSVarRef *var_ref)
{
    if (var_ref) {
        assert(var_ref->header.ref_count > 0);
        if (--var_ref->header.ref_count == 0) {
            if (var_ref->is_detached) {
                JS_FreeValueRT(rt, var_ref->value);
                remove_gc_object(&var_ref->header);
            } else {
                list_del(&var_ref->header.link); /* still on the stack */
            }
            js_free_rt(rt, var_ref);
        }
    }
}

static void js_array_finalizer(JSRuntime *rt, JSValue val)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    int i;

    for(i = 0; i < p->u.array.count; i++) {
        JS_FreeValueRT(rt, p->u.array.u.values[i]);
    }
    js_free_rt(rt, p->u.array.u.values);
}

static void js_array_mark(JSRuntime *rt, JSValueConst val,
                          JS_MarkFunc *mark_func)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    int i;

    for(i = 0; i < p->u.array.count; i++) {
        JS_MarkValue(rt, p->u.array.u.values[i], mark_func);
    }
}

static void js_object_data_finalizer(JSRuntime *rt, JSValue val)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JS_FreeValueRT(rt, p->u.object_data);
    p->u.object_data = JS_UNDEFINED;
}

static void js_object_data_mark(JSRuntime *rt, JSValueConst val,
                                JS_MarkFunc *mark_func)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JS_MarkValue(rt, p->u.object_data, mark_func);
}

static void js_c_function_finalizer(JSRuntime *rt, JSValue val)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);

    if (p->u.cfunc.realm)
        JS_FreeContext(p->u.cfunc.realm);
}

static void js_c_function_mark(JSRuntime *rt, JSValueConst val,
                               JS_MarkFunc *mark_func)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);

    if (p->u.cfunc.realm)
        mark_func(rt, &p->u.cfunc.realm->header);
}

static void js_bytecode_function_finalizer(JSRuntime *rt, JSValue val)
{
    JSObject *p1, *p = JS_VALUE_GET_OBJ(val);
    JSFunctionBytecode *b;
    JSVarRef **var_refs;
    int i;

    p1 = p->u.func.home_object;
    if (p1) {
        JS_FreeValueRT(rt, JS_MKPTR(JS_TAG_OBJECT, p1));
    }
    b = p->u.func.function_bytecode;
    if (b) {
        var_refs = p->u.func.var_refs;
        if (var_refs) {
            for(i = 0; i < b->closure_var_count; i++)
                free_var_ref(rt, var_refs[i]);
            js_free_rt(rt, var_refs);
        }
        JS_FreeValueRT(rt, JS_MKPTR(JS_TAG_FUNCTION_BYTECODE, b));
    }
}

static void js_bytecode_function_mark(JSRuntime *rt, JSValueConst val,
                                      JS_MarkFunc *mark_func)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSVarRef **var_refs = p->u.func.var_refs;
    JSFunctionBytecode *b = p->u.func.function_bytecode;
    int i;

    if (p->u.func.home_object) {
        JS_MarkValue(rt, JS_MKPTR(JS_TAG_OBJECT, p->u.func.home_object),
                     mark_func);
    }
    if (b) {
        if (var_refs) {
            for(i = 0; i < b->closure_var_count; i++) {
                JSVarRef *var_ref = var_refs[i];
                if (var_ref && var_ref->is_detached) {
                    mark_func(rt, &var_ref->header);
                }
            }
        }
        /* must mark the function bytecode because template objects may be
           part of a cycle */
        JS_MarkValue(rt, JS_MKPTR(JS_TAG_FUNCTION_BYTECODE, b), mark_func);
    }
}

static void js_bound_function_finalizer(JSRuntime *rt, JSValue val)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSBoundFunction *bf = p->u.bound_function;
    int i;

    JS_FreeValueRT(rt, bf->func_obj);
    JS_FreeValueRT(rt, bf->this_val);
    for(i = 0; i < bf->argc; i++) {
        JS_FreeValueRT(rt, bf->argv[i]);
    }
    js_free_rt(rt, bf);
}

static void js_bound_function_mark(JSRuntime *rt, JSValueConst val,
                                JS_MarkFunc *mark_func)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSBoundFunction *bf = p->u.bound_function;
    int i;

    JS_MarkValue(rt, bf->func_obj, mark_func);
    JS_MarkValue(rt, bf->this_val, mark_func);
    for(i = 0; i < bf->argc; i++)
        JS_MarkValue(rt, bf->argv[i], mark_func);
}

static void js_for_in_iterator_finalizer(JSRuntime *rt, JSValue val)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSForInIterator *it = p->u.for_in_iterator;
    JS_FreeValueRT(rt, it->obj);
    js_free_rt(rt, it);
}

static void js_for_in_iterator_mark(JSRuntime *rt, JSValueConst val,
                                JS_MarkFunc *mark_func)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSForInIterator *it = p->u.for_in_iterator;
    JS_MarkValue(rt, it->obj, mark_func);
}

static void free_object(JSRuntime *rt, JSObject *p)
{
    int i;
    JSClassFinalizer *finalizer;
    JSShape *sh;
    JSShapeProperty *pr;

    p->free_mark = 1; /* used to tell the object is invalid when
                         freeing cycles */
    /* free all the fields */
    sh = p->shape;
    pr = get_shape_prop(sh);
    for(i = 0; i < sh->prop_count; i++) {
        free_property(rt, &p->prop[i], pr->flags);
        pr++;
    }
    js_free_rt(rt, p->prop);
    /* as an optimization we destroy the shape immediately without
       putting it in gc_zero_ref_count_list */
    js_free_shape(rt, sh);

    /* fail safe */
    p->shape = NULL;
    p->prop = NULL;

    if (unlikely(p->first_weak_ref)) {
        reset_weak_ref(rt, p);
    }

    finalizer = rt->class_array[p->class_id].finalizer;
    if (finalizer)
        (*finalizer)(rt, JS_MKPTR(JS_TAG_OBJECT, p));

    /* fail safe */
    p->class_id = 0;
    p->u.opaque = NULL;
    p->u.func.var_refs = NULL;
    p->u.func.home_object = NULL;

    remove_gc_object(&p->header);
    if (rt->gc_phase == JS_GC_PHASE_REMOVE_CYCLES && p->header.ref_count != 0) {
        list_add_tail(&p->header.link, &rt->gc_zero_ref_count_list);
    } else {
        js_free_rt(rt, p);
    }
}

static void free_gc_object(JSRuntime *rt, JSGCObjectHeader *gp)
{
    switch(gp->gc_obj_type) {
    case JS_GC_OBJ_TYPE_JS_OBJECT:
        free_object(rt, (JSObject *)gp);
        break;
    case JS_GC_OBJ_TYPE_FUNCTION_BYTECODE:
        free_function_bytecode(rt, (JSFunctionBytecode *)gp);
        break;
    default:
        abort();
    }
}

static void free_zero_refcount(JSRuntime *rt)
{
    struct list_head *el;
    JSGCObjectHeader *p;
    
    rt->gc_phase = JS_GC_PHASE_DECREF;
    for(;;) {
        el = rt->gc_zero_ref_count_list.next;
        if (el == &rt->gc_zero_ref_count_list)
            break;
        p = list_entry(el, JSGCObjectHeader, link);
        assert(p->ref_count == 0);
        free_gc_object(rt, p);
    }
    rt->gc_phase = JS_GC_PHASE_NONE;
}

/* called with the ref_count of 'v' reaches zero. */
void __JS_FreeValueRT(JSRuntime *rt, JSValue v)
{
    uint32_t tag = JS_VALUE_GET_TAG(v);

#ifdef DUMP_FREE
    {
        printf("Freeing ");
        if (tag == JS_TAG_OBJECT) {
            JS_DumpObject(rt, JS_VALUE_GET_OBJ(v));
        } else {
            JS_DumpValueShort(rt, v);
            printf("\n");
        }
    }
#endif

    switch(tag) {
    case JS_TAG_STRING:
        {
            JSString *p = JS_VALUE_GET_STRING(v);
            if (p->atom_type) {
                JS_FreeAtomStruct(rt, p);
            } else {
#ifdef DUMP_LEAKS
                list_del(&p->link);
#endif
                js_free_rt(rt, p);
            }
        }
        break;
    case JS_TAG_OBJECT:
    case JS_TAG_FUNCTION_BYTECODE:
        {
            JSGCObjectHeader *p = JS_VALUE_GET_PTR(v);
            if (rt->gc_phase != JS_GC_PHASE_REMOVE_CYCLES) {
                list_del(&p->link);
                list_add(&p->link, &rt->gc_zero_ref_count_list);
                if (rt->gc_phase == JS_GC_PHASE_NONE) {
                    free_zero_refcount(rt);
                }
            }
        }
        break;
    case JS_TAG_MODULE:
        abort(); /* never freed here */
        break;
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_INT:
    case JS_TAG_BIG_FLOAT:
        {
            JSBigFloat *bf = JS_VALUE_GET_PTR(v);
            bf_delete(&bf->num);
            js_free_rt(rt, bf);
        }
        break;
    case JS_TAG_BIG_DECIMAL:
        {
            JSBigDecimal *bf = JS_VALUE_GET_PTR(v);
            bfdec_delete(&bf->num);
            js_free_rt(rt, bf);
        }
        break;
#endif
    case JS_TAG_SYMBOL:
        {
            JSAtomStruct *p = JS_VALUE_GET_PTR(v);
            JS_FreeAtomStruct(rt, p);
        }
        break;
    default:
        printf("__JS_FreeValue: unknown tag=%d\n", tag);
        abort();
    }
}

void __JS_FreeValue(JSContext *ctx, JSValue v)
{
    __JS_FreeValueRT(ctx->rt, v);
}

/* garbage collection */

static void add_gc_object(JSRuntime *rt, JSGCObjectHeader *h,
                          JSGCObjectTypeEnum type)
{
    h->mark = 0;
    h->gc_obj_type = type;
    list_add_tail(&h->link, &rt->gc_obj_list);
}

static void remove_gc_object(JSGCObjectHeader *h)
{
    list_del(&h->link);
}

void JS_MarkValue(JSRuntime *rt, JSValueConst val, JS_MarkFunc *mark_func)
{
    if (JS_VALUE_HAS_REF_COUNT(val)) {
        switch(JS_VALUE_GET_TAG(val)) {
        case JS_TAG_OBJECT:
        case JS_TAG_FUNCTION_BYTECODE:
            mark_func(rt, JS_VALUE_GET_PTR(val));
            break;
        default:
            break;
        }
    }
}

static void mark_children(JSRuntime *rt, JSGCObjectHeader *gp,
                          JS_MarkFunc *mark_func)
{
    switch(gp->gc_obj_type) {
    case JS_GC_OBJ_TYPE_JS_OBJECT:
        {
            JSObject *p = (JSObject *)gp;
            JSShapeProperty *prs;
            JSShape *sh;
            int i;
            sh = p->shape;
            mark_func(rt, &sh->header);
            /* mark all the fields */
            prs = get_shape_prop(sh);
            for(i = 0; i < sh->prop_count; i++) {
                JSProperty *pr = &p->prop[i];
                if (prs->atom != JS_ATOM_NULL) {
                    if (prs->flags & JS_PROP_TMASK) {
                        if ((prs->flags & JS_PROP_TMASK) == JS_PROP_GETSET) {
                            if (pr->u.getset.getter)
                                mark_func(rt, &pr->u.getset.getter->header);
                            if (pr->u.getset.setter)
                                mark_func(rt, &pr->u.getset.setter->header);
                        } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_VARREF) {
                            if (pr->u.var_ref->is_detached) {
                                /* Note: the tag does not matter
                                   provided it is a GC object */
                                mark_func(rt, &pr->u.var_ref->header);
                            }
                        } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_AUTOINIT) {
                            js_autoinit_mark(rt, pr, mark_func);
                        }
                    } else {
                        JS_MarkValue(rt, pr->u.value, mark_func);
                    }
                }
                prs++;
            }

            if (p->class_id != JS_CLASS_OBJECT) {
                JSClassGCMark *gc_mark;
                gc_mark = rt->class_array[p->class_id].gc_mark;
                if (gc_mark)
                    gc_mark(rt, JS_MKPTR(JS_TAG_OBJECT, p), mark_func);
            }
        }
        break;
    case JS_GC_OBJ_TYPE_FUNCTION_BYTECODE:
        /* the template objects can be part of a cycle */
        {
            JSFunctionBytecode *b = (JSFunctionBytecode *)gp;
            int i;
            for(i = 0; i < b->cpool_count; i++) {
                JS_MarkValue(rt, b->cpool[i], mark_func);
            }
            if (b->realm)
                mark_func(rt, &b->realm->header);
        }
        break;
    case JS_GC_OBJ_TYPE_VAR_REF:
        {
            JSVarRef *var_ref = (JSVarRef *)gp;
            /* only detached variable referenced are taken into account */
            assert(var_ref->is_detached);
            JS_MarkValue(rt, *var_ref->pvalue, mark_func);
        }
        break;
    case JS_GC_OBJ_TYPE_ASYNC_FUNCTION:
        {
            JSAsyncFunctionData *s = (JSAsyncFunctionData *)gp;
            if (s->is_active)
                async_func_mark(rt, &s->func_state, mark_func);
            JS_MarkValue(rt, s->resolving_funcs[0], mark_func);
            JS_MarkValue(rt, s->resolving_funcs[1], mark_func);
        }
        break;
    case JS_GC_OBJ_TYPE_SHAPE:
        {
            JSShape *sh = (JSShape *)gp;
            if (sh->proto != NULL) {
                mark_func(rt, &sh->proto->header);
            }
        }
        break;
    case JS_GC_OBJ_TYPE_JS_CONTEXT:
        {
            JSContext *ctx = (JSContext *)gp;
            JS_MarkContext(rt, ctx, mark_func);
        }
        break;
    default:
        abort();
    }
}

static void gc_decref_child(JSRuntime *rt, JSGCObjectHeader *p)
{
    assert(p->ref_count > 0);
    p->ref_count--;
    if (p->ref_count == 0 && p->mark == 1) {
        list_del(&p->link);
        list_add_tail(&p->link, &rt->tmp_obj_list);
    }
}

static void gc_decref(JSRuntime *rt)
{
    struct list_head *el, *el1;
    JSGCObjectHeader *p;
    
    init_list_head(&rt->tmp_obj_list);

    /* decrement the refcount of all the children of all the GC
       objects and move the GC objects with zero refcount to
       tmp_obj_list */
    list_for_each_safe(el, el1, &rt->gc_obj_list) {
        p = list_entry(el, JSGCObjectHeader, link);
        assert(p->mark == 0);
        mark_children(rt, p, gc_decref_child);
        p->mark = 1;
        if (p->ref_count == 0) {
            list_del(&p->link);
            list_add_tail(&p->link, &rt->tmp_obj_list);
        }
    }
}

static void gc_scan_incref_child(JSRuntime *rt, JSGCObjectHeader *p)
{
    p->ref_count++;
    if (p->ref_count == 1) {
        /* ref_count was 0: remove from tmp_obj_list and add at the
           end of gc_obj_list */
        list_del(&p->link);
        list_add_tail(&p->link, &rt->gc_obj_list);
        p->mark = 0; /* reset the mark for the next GC call */
    }
}

static void gc_scan_incref_child2(JSRuntime *rt, JSGCObjectHeader *p)
{
    p->ref_count++;
}

static void gc_scan(JSRuntime *rt)
{
    struct list_head *el;
    JSGCObjectHeader *p;

    /* keep the objects with a refcount > 0 and their children. */
    list_for_each(el, &rt->gc_obj_list) {
        p = list_entry(el, JSGCObjectHeader, link);
        assert(p->ref_count > 0);
        p->mark = 0; /* reset the mark for the next GC call */
        mark_children(rt, p, gc_scan_incref_child);
    }
    
    /* restore the refcount of the objects to be deleted. */
    list_for_each(el, &rt->tmp_obj_list) {
        p = list_entry(el, JSGCObjectHeader, link);
        mark_children(rt, p, gc_scan_incref_child2);
    }
}

static void gc_free_cycles(JSRuntime *rt)
{
    struct list_head *el, *el1;
    JSGCObjectHeader *p;
#ifdef DUMP_GC_FREE
    BOOL header_done = FALSE;
#endif

    rt->gc_phase = JS_GC_PHASE_REMOVE_CYCLES;

    for(;;) {
        el = rt->tmp_obj_list.next;
        if (el == &rt->tmp_obj_list)
            break;
        p = list_entry(el, JSGCObjectHeader, link);
        /* Only need to free the GC object associated with JS
           values. The rest will be automatically removed because they
           must be referenced by them. */
        switch(p->gc_obj_type) {
        case JS_GC_OBJ_TYPE_JS_OBJECT:
        case JS_GC_OBJ_TYPE_FUNCTION_BYTECODE:
#ifdef DUMP_GC_FREE
            if (!header_done) {
                printf("Freeing cycles:\n");
                JS_DumpObjectHeader(rt);
                header_done = TRUE;
            }
            JS_DumpGCObject(rt, p);
#endif
            free_gc_object(rt, p);
            break;
        default:
            list_del(&p->link);
            list_add_tail(&p->link, &rt->gc_zero_ref_count_list);
            break;
        }
    }
    rt->gc_phase = JS_GC_PHASE_NONE;
           
    list_for_each_safe(el, el1, &rt->gc_zero_ref_count_list) {
        p = list_entry(el, JSGCObjectHeader, link);
        assert(p->gc_obj_type == JS_GC_OBJ_TYPE_JS_OBJECT ||
               p->gc_obj_type == JS_GC_OBJ_TYPE_FUNCTION_BYTECODE);
        js_free_rt(rt, p);
    }

    init_list_head(&rt->gc_zero_ref_count_list);
}

void JS_RunGC(JSRuntime *rt)
{
    /* decrement the reference of the children of each object. mark =
       1 after this pass. */
    gc_decref(rt);

    /* keep the GC objects with a non zero refcount and their childs */
    gc_scan(rt);

    /* free the GC objects in a cycle */
    gc_free_cycles(rt);
}

/* Return false if not an object or if the object has already been
   freed (zombie objects are visible in finalizers when freeing
   cycles). */
BOOL JS_IsLiveObject(JSRuntime *rt, JSValueConst obj)
{
    JSObject *p;
    if (!JS_IsObject(obj))
        return FALSE;
    p = JS_VALUE_GET_OBJ(obj);
    return !p->free_mark;
}

/* Compute memory used by various object types */
/* XXX: poor man's approach to handling multiply referenced objects */
typedef struct JSMemoryUsage_helper {
    double memory_used_count;
    double str_count;
    double str_size;
    int64_t js_func_count;
    double js_func_size;
    int64_t js_func_code_size;
    int64_t js_func_pc2line_count;
    int64_t js_func_pc2line_size;
} JSMemoryUsage_helper;

static void compute_value_size(JSValueConst val, JSMemoryUsage_helper *hp);

static void compute_jsstring_size(JSString *str, JSMemoryUsage_helper *hp)
{
    if (!str->atom_type) {  /* atoms are handled separately */
        double s_ref_count = str->header.ref_count;
        hp->str_count += 1 / s_ref_count;
        hp->str_size += ((sizeof(*str) + (str->len << str->is_wide_char) +
                          1 - str->is_wide_char) / s_ref_count);
    }
}

static void compute_bytecode_size(JSFunctionBytecode *b, JSMemoryUsage_helper *hp)
{
    int memory_used_count, js_func_size, i;

    memory_used_count = 0;
    js_func_size = offsetof(JSFunctionBytecode, debug);
    if (b->vardefs) {
        js_func_size += (b->arg_count + b->var_count) * sizeof(*b->vardefs);
    }
    if (b->cpool) {
        js_func_size += b->cpool_count * sizeof(*b->cpool);
        for (i = 0; i < b->cpool_count; i++) {
            JSValueConst val = b->cpool[i];
            compute_value_size(val, hp);
        }
    }
    if (b->closure_var) {
        js_func_size += b->closure_var_count * sizeof(*b->closure_var);
    }
    if (!b->read_only_bytecode && b->byte_code_buf) {
        hp->js_func_code_size += b->byte_code_len;
    }
    if (b->has_debug) {
        js_func_size += sizeof(*b) - offsetof(JSFunctionBytecode, debug);
        if (b->debug.source) {
            memory_used_count++;
            js_func_size += b->debug.source_len + 1;
        }
        if (b->debug.pc2line_len) {
            memory_used_count++;
            hp->js_func_pc2line_count += 1;
            hp->js_func_pc2line_size += b->debug.pc2line_len;
        }
    }
    hp->js_func_size += js_func_size;
    hp->js_func_count += 1;
    hp->memory_used_count += memory_used_count;
}

static void compute_value_size(JSValueConst val, JSMemoryUsage_helper *hp)
{
    switch(JS_VALUE_GET_TAG(val)) {
    case JS_TAG_STRING:
        compute_jsstring_size(JS_VALUE_GET_STRING(val), hp);
        break;
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_INT:
    case JS_TAG_BIG_FLOAT:
    case JS_TAG_BIG_DECIMAL:
        /* should track JSBigFloat usage */
        break;
#endif
    }
}

void JS_ComputeMemoryUsage(JSRuntime *rt, JSMemoryUsage *s)
{
    struct list_head *el, *el1;
    int i;
    JSMemoryUsage_helper mem = { 0 }, *hp = &mem;

    memset(s, 0, sizeof(*s));
    s->malloc_count = rt->malloc_state.malloc_count;
    s->malloc_size = rt->malloc_state.malloc_size;
    s->malloc_limit = rt->malloc_state.malloc_limit;

    s->memory_used_count = 2; /* rt + rt->class_array */
    s->memory_used_size = sizeof(JSRuntime) + sizeof(JSValue) * rt->class_count;

    list_for_each(el, &rt->context_list) {
        JSContext *ctx = list_entry(el, JSContext, link);
        JSShape *sh = ctx->array_shape;
        s->memory_used_count += 2; /* ctx + ctx->class_proto */
        s->memory_used_size += sizeof(JSContext) +
            sizeof(JSValue) * rt->class_count;
        s->binary_object_count += ctx->binary_object_count;
        s->binary_object_size += ctx->binary_object_size;

        /* the hashed shapes are counted separately */
        if (sh && !sh->is_hashed) {
            int hash_size = sh->prop_hash_mask + 1;
            s->shape_count++;
            s->shape_size += get_shape_size(hash_size, sh->prop_size);
        }
        list_for_each(el1, &ctx->loaded_modules) {
            JSModuleDef *m = list_entry(el1, JSModuleDef, link);
            s->memory_used_count += 1;
            s->memory_used_size += sizeof(*m);
            if (m->req_module_entries) {
                s->memory_used_count += 1;
                s->memory_used_size += m->req_module_entries_count * sizeof(*m->req_module_entries);
            }
            if (m->export_entries) {
                s->memory_used_count += 1;
                s->memory_used_size += m->export_entries_count * sizeof(*m->export_entries);
                for (i = 0; i < m->export_entries_count; i++) {
                    JSExportEntry *me = &m->export_entries[i];
                    if (me->export_type == JS_EXPORT_TYPE_LOCAL && me->u.local.var_ref) {
                        /* potential multiple count */
                        s->memory_used_count += 1;
                        compute_value_size(me->u.local.var_ref->value, hp);
                    }
                }
            }
            if (m->star_export_entries) {
                s->memory_used_count += 1;
                s->memory_used_size += m->star_export_entries_count * sizeof(*m->star_export_entries);
            }
            if (m->import_entries) {
                s->memory_used_count += 1;
                s->memory_used_size += m->import_entries_count * sizeof(*m->import_entries);
            }
            compute_value_size(m->module_ns, hp);
            compute_value_size(m->func_obj, hp);
        }
    }

    list_for_each(el, &rt->gc_obj_list) {
        JSGCObjectHeader *gp = list_entry(el, JSGCObjectHeader, link);
        JSObject *p;
        JSShape *sh;
        JSShapeProperty *prs;

        /* XXX: could count the other GC object types too */
        if (gp->gc_obj_type == JS_GC_OBJ_TYPE_FUNCTION_BYTECODE) {
            compute_bytecode_size((JSFunctionBytecode *)gp, hp);
            continue;
        } else if (gp->gc_obj_type != JS_GC_OBJ_TYPE_JS_OBJECT) {
            continue;
        }
        p = (JSObject *)gp;
        sh = p->shape;
        s->obj_count++;
        if (p->prop) {
            s->memory_used_count++;
            s->prop_size += sh->prop_size * sizeof(*p->prop);
            s->prop_count += sh->prop_count;
            prs = get_shape_prop(sh);
            for(i = 0; i < sh->prop_count; i++) {
                JSProperty *pr = &p->prop[i];
                if (prs->atom != JS_ATOM_NULL && !(prs->flags & JS_PROP_TMASK)) {
                    compute_value_size(pr->u.value, hp);
                }
                prs++;
            }
        }
        /* the hashed shapes are counted separately */
        if (!sh->is_hashed) {
            int hash_size = sh->prop_hash_mask + 1;
            s->shape_count++;
            s->shape_size += get_shape_size(hash_size, sh->prop_size);
        }

        switch(p->class_id) {
        case JS_CLASS_ARRAY:             /* u.array | length */
        case JS_CLASS_ARGUMENTS:         /* u.array | length */
            s->array_count++;
            if (p->fast_array) {
                s->fast_array_count++;
                if (p->u.array.u.values) {
                    s->memory_used_count++;
                    s->memory_used_size += p->u.array.count *
                        sizeof(*p->u.array.u.values);
                    s->fast_array_elements += p->u.array.count;
                    for (i = 0; i < p->u.array.count; i++) {
                        compute_value_size(p->u.array.u.values[i], hp);
                    }
                }
            }
            break;
        case JS_CLASS_NUMBER:            /* u.object_data */
        case JS_CLASS_STRING:            /* u.object_data */
        case JS_CLASS_BOOLEAN:           /* u.object_data */
        case JS_CLASS_SYMBOL:            /* u.object_data */
        case JS_CLASS_DATE:              /* u.object_data */
#ifdef CONFIG_BIGNUM
        case JS_CLASS_BIG_INT:           /* u.object_data */
        case JS_CLASS_BIG_FLOAT:         /* u.object_data */
        case JS_CLASS_BIG_DECIMAL:         /* u.object_data */
#endif
            compute_value_size(p->u.object_data, hp);
            break;
        case JS_CLASS_C_FUNCTION:        /* u.cfunc */
            s->c_func_count++;
            break;
        case JS_CLASS_BYTECODE_FUNCTION: /* u.func */
            {
                JSFunctionBytecode *b = p->u.func.function_bytecode;
                JSVarRef **var_refs = p->u.func.var_refs;
                /* home_object: object will be accounted for in list scan */
                if (var_refs) {
                    s->memory_used_count++;
                    s->js_func_size += b->closure_var_count * sizeof(*var_refs);
                    for (i = 0; i < b->closure_var_count; i++) {
                        if (var_refs[i]) {
                            double ref_count = var_refs[i]->header.ref_count;
                            s->memory_used_count += 1 / ref_count;
                            s->js_func_size += sizeof(*var_refs[i]) / ref_count;
                            /* handle non object closed values */
                            if (var_refs[i]->pvalue == &var_refs[i]->value) {
                                /* potential multiple count */
                                compute_value_size(var_refs[i]->value, hp);
                            }
                        }
                    }
                }
            }
            break;
        case JS_CLASS_BOUND_FUNCTION:    /* u.bound_function */
            {
                JSBoundFunction *bf = p->u.bound_function;
                /* func_obj and this_val are objects */
                for (i = 0; i < bf->argc; i++) {
                    compute_value_size(bf->argv[i], hp);
                }
                s->memory_used_count += 1;
                s->memory_used_size += sizeof(*bf) + bf->argc * sizeof(*bf->argv);
            }
            break;
        case JS_CLASS_C_FUNCTION_DATA:   /* u.c_function_data_record */
            {
                JSCFunctionDataRecord *fd = p->u.c_function_data_record;
                if (fd) {
                    for (i = 0; i < fd->data_len; i++) {
                        compute_value_size(fd->data[i], hp);
                    }
                    s->memory_used_count += 1;
                    s->memory_used_size += sizeof(*fd) + fd->data_len * sizeof(*fd->data);
                }
            }
            break;
        case JS_CLASS_REGEXP:            /* u.regexp */
            compute_jsstring_size(p->u.regexp.pattern, hp);
            compute_jsstring_size(p->u.regexp.bytecode, hp);
            break;

        case JS_CLASS_FOR_IN_ITERATOR:   /* u.for_in_iterator */
            {
                JSForInIterator *it = p->u.for_in_iterator;
                if (it) {
                    compute_value_size(it->obj, hp);
                    s->memory_used_count += 1;
                    s->memory_used_size += sizeof(*it);
                }
            }
            break;
        case JS_CLASS_ARRAY_BUFFER:      /* u.array_buffer */
        case JS_CLASS_SHARED_ARRAY_BUFFER: /* u.array_buffer */
            {
                JSArrayBuffer *abuf = p->u.array_buffer;
                if (abuf) {
                    s->memory_used_count += 1;
                    s->memory_used_size += sizeof(*abuf);
                    if (abuf->data) {
                        s->memory_used_count += 1;
                        s->memory_used_size += abuf->byte_length;
                    }
                }
            }
            break;
        case JS_CLASS_GENERATOR:         /* u.generator_data */
        case JS_CLASS_UINT8C_ARRAY:      /* u.typed_array / u.array */
        case JS_CLASS_INT8_ARRAY:        /* u.typed_array / u.array */
        case JS_CLASS_UINT8_ARRAY:       /* u.typed_array / u.array */
        case JS_CLASS_INT16_ARRAY:       /* u.typed_array / u.array */
        case JS_CLASS_UINT16_ARRAY:      /* u.typed_array / u.array */
        case JS_CLASS_INT32_ARRAY:       /* u.typed_array / u.array */
        case JS_CLASS_UINT32_ARRAY:      /* u.typed_array / u.array */
#ifdef CONFIG_BIGNUM
        case JS_CLASS_BIG_INT64_ARRAY:   /* u.typed_array / u.array */
        case JS_CLASS_BIG_UINT64_ARRAY:  /* u.typed_array / u.array */
#endif
        case JS_CLASS_FLOAT32_ARRAY:     /* u.typed_array / u.array */
        case JS_CLASS_FLOAT64_ARRAY:     /* u.typed_array / u.array */
        case JS_CLASS_DATAVIEW:          /* u.typed_array */
#ifdef CONFIG_BIGNUM
        case JS_CLASS_FLOAT_ENV:         /* u.float_env */
#endif
        case JS_CLASS_MAP:               /* u.map_state */
        case JS_CLASS_SET:               /* u.map_state */
        case JS_CLASS_WEAKMAP:           /* u.map_state */
        case JS_CLASS_WEAKSET:           /* u.map_state */
        case JS_CLASS_MAP_ITERATOR:      /* u.map_iterator_data */
        case JS_CLASS_SET_ITERATOR:      /* u.map_iterator_data */
        case JS_CLASS_ARRAY_ITERATOR:    /* u.array_iterator_data */
        case JS_CLASS_STRING_ITERATOR:   /* u.array_iterator_data */
        case JS_CLASS_PROXY:             /* u.proxy_data */
        case JS_CLASS_PROMISE:           /* u.promise_data */
        case JS_CLASS_PROMISE_RESOLVE_FUNCTION:  /* u.promise_function_data */
        case JS_CLASS_PROMISE_REJECT_FUNCTION:   /* u.promise_function_data */
        case JS_CLASS_ASYNC_FUNCTION_RESOLVE:    /* u.async_function_data */
        case JS_CLASS_ASYNC_FUNCTION_REJECT:     /* u.async_function_data */
        case JS_CLASS_ASYNC_FROM_SYNC_ITERATOR:  /* u.async_from_sync_iterator_data */
        case JS_CLASS_ASYNC_GENERATOR:   /* u.async_generator_data */
            /* TODO */
        default:
            /* XXX: class definition should have an opaque block size */
            if (p->u.opaque) {
                s->memory_used_count += 1;
            }
            break;
        }
    }
    s->obj_size += s->obj_count * sizeof(JSObject);

    /* hashed shapes */
    s->memory_used_count++; /* rt->shape_hash */
    s->memory_used_size += sizeof(rt->shape_hash[0]) * rt->shape_hash_size;
    for(i = 0; i < rt->shape_hash_size; i++) {
        JSShape *sh;
        for(sh = rt->shape_hash[i]; sh != NULL; sh = sh->shape_hash_next) {
            int hash_size = sh->prop_hash_mask + 1;
            s->shape_count++;
            s->shape_size += get_shape_size(hash_size, sh->prop_size);
        }
    }

    /* atoms */
    s->memory_used_count += 2; /* rt->atom_array, rt->atom_hash */
    s->atom_count = rt->atom_count;
    s->atom_size = sizeof(rt->atom_array[0]) * rt->atom_size +
        sizeof(rt->atom_hash[0]) * rt->atom_hash_size;
    for(i = 0; i < rt->atom_size; i++) {
        JSAtomStruct *p = rt->atom_array[i];
        if (!atom_is_free(p)) {
            s->atom_size += (sizeof(*p) + (p->len << p->is_wide_char) +
                             1 - p->is_wide_char);
        }
    }
    s->str_count = round(mem.str_count);
    s->str_size = round(mem.str_size);
    s->js_func_count = mem.js_func_count;
    s->js_func_size = round(mem.js_func_size);
    s->js_func_code_size = mem.js_func_code_size;
    s->js_func_pc2line_count = mem.js_func_pc2line_count;
    s->js_func_pc2line_size = mem.js_func_pc2line_size;
    s->memory_used_count += round(mem.memory_used_count) +
        s->atom_count + s->str_count +
        s->obj_count + s->shape_count +
        s->js_func_count + s->js_func_pc2line_count;
    s->memory_used_size += s->atom_size + s->str_size +
        s->obj_size + s->prop_size + s->shape_size +
        s->js_func_size + s->js_func_code_size + s->js_func_pc2line_size;
}

void JS_DumpMemoryUsage(FILE *fp, const JSMemoryUsage *s, JSRuntime *rt)
{
    fprintf(fp, "QuickJS memory usage -- "
#ifdef CONFIG_BIGNUM
            "BigNum "
#endif
            CONFIG_VERSION " version, %d-bit, malloc limit: %"PRId64"\n\n",
            (int)sizeof(void *) * 8, (int64_t)(ssize_t)s->malloc_limit);
#if 1
    if (rt) {
        static const struct {
            const char *name;
            size_t size;
        } object_types[] = {
            { "JSRuntime", sizeof(JSRuntime) },
            { "JSContext", sizeof(JSContext) },
            { "JSObject", sizeof(JSObject) },
            { "JSString", sizeof(JSString) },
            { "JSFunctionBytecode", sizeof(JSFunctionBytecode) },
        };
        int i, usage_size_ok = 0;
        for(i = 0; i < countof(object_types); i++) {
            unsigned int size = object_types[i].size;
            void *p = js_malloc_rt(rt, size);
            if (p) {
                unsigned int size1 = js_malloc_usable_size_rt(rt, p);
                if (size1 >= size) {
                    usage_size_ok = 1;
                    fprintf(fp, "  %3u + %-2u  %s\n",
                            size, size1 - size, object_types[i].name);
                }
                js_free_rt(rt, p);
            }
        }
        if (!usage_size_ok) {
            fprintf(fp, "  malloc_usable_size unavailable\n");
        }
        {
            int obj_classes[JS_CLASS_INIT_COUNT + 1] = { 0 };
            int class_id;
            struct list_head *el;
            list_for_each(el, &rt->gc_obj_list) {
                JSGCObjectHeader *gp = list_entry(el, JSGCObjectHeader, link);
                JSObject *p;
                if (gp->gc_obj_type == JS_GC_OBJ_TYPE_JS_OBJECT) {
                    p = (JSObject *)gp;
                    obj_classes[min_uint32(p->class_id, JS_CLASS_INIT_COUNT)]++;
                }
            }
            fprintf(fp, "\n" "JSObject classes\n");
            if (obj_classes[0])
                fprintf(fp, "  %5d  %2.0d %s\n", obj_classes[0], 0, "none");
            for (class_id = 1; class_id < JS_CLASS_INIT_COUNT; class_id++) {
                if (obj_classes[class_id]) {
                    char buf[ATOM_GET_STR_BUF_SIZE];
                    fprintf(fp, "  %5d  %2.0d %s\n", obj_classes[class_id], class_id,
                            JS_AtomGetStrRT(rt, buf, sizeof(buf), js_std_class_def[class_id - 1].class_name));
                }
            }
            if (obj_classes[JS_CLASS_INIT_COUNT])
                fprintf(fp, "  %5d  %2.0d %s\n", obj_classes[JS_CLASS_INIT_COUNT], 0, "other");
        }
        fprintf(fp, "\n");
    }
#endif
    fprintf(fp, "%-20s %8s %8s\n", "NAME", "COUNT", "SIZE");

    if (s->malloc_count) {
        fprintf(fp, "%-20s %8"PRId64" %8"PRId64"  (%0.1f per block)\n",
                "memory allocated", s->malloc_count, s->malloc_size,
                (double)s->malloc_size / s->malloc_count);
        fprintf(fp, "%-20s %8"PRId64" %8"PRId64"  (%d overhead, %0.1f average slack)\n",
                "memory used", s->memory_used_count, s->memory_used_size,
                MALLOC_OVERHEAD, ((double)(s->malloc_size - s->memory_used_size) /
                                  s->memory_used_count));
    }
    if (s->atom_count) {
        fprintf(fp, "%-20s %8"PRId64" %8"PRId64"  (%0.1f per atom)\n",
                "atoms", s->atom_count, s->atom_size,
                (double)s->atom_size / s->atom_count);
    }
    if (s->str_count) {
        fprintf(fp, "%-20s %8"PRId64" %8"PRId64"  (%0.1f per string)\n",
                "strings", s->str_count, s->str_size,
                (double)s->str_size / s->str_count);
    }
    if (s->obj_count) {
        fprintf(fp, "%-20s %8"PRId64" %8"PRId64"  (%0.1f per object)\n",
                "objects", s->obj_count, s->obj_size,
                (double)s->obj_size / s->obj_count);
        fprintf(fp, "%-20s %8"PRId64" %8"PRId64"  (%0.1f per object)\n",
                "  properties", s->prop_count, s->prop_size,
                (double)s->prop_count / s->obj_count);
        fprintf(fp, "%-20s %8"PRId64" %8"PRId64"  (%0.1f per shape)\n",
                "  shapes", s->shape_count, s->shape_size,
                (double)s->shape_size / s->shape_count);
    }
    if (s->js_func_count) {
        fprintf(fp, "%-20s %8"PRId64" %8"PRId64"\n",
                "bytecode functions", s->js_func_count, s->js_func_size);
        fprintf(fp, "%-20s %8"PRId64" %8"PRId64"  (%0.1f per function)\n",
                "  bytecode", s->js_func_count, s->js_func_code_size,
                (double)s->js_func_code_size / s->js_func_count);
        if (s->js_func_pc2line_count) {
            fprintf(fp, "%-20s %8"PRId64" %8"PRId64"  (%0.1f per function)\n",
                    "  pc2line", s->js_func_pc2line_count,
                    s->js_func_pc2line_size,
                    (double)s->js_func_pc2line_size / s->js_func_pc2line_count);
        }
    }
    if (s->c_func_count) {
        fprintf(fp, "%-20s %8"PRId64"\n", "C functions", s->c_func_count);
    }
    if (s->array_count) {
        fprintf(fp, "%-20s %8"PRId64"\n", "arrays", s->array_count);
        if (s->fast_array_count) {
            fprintf(fp, "%-20s %8"PRId64"\n", "  fast arrays", s->fast_array_count);
            fprintf(fp, "%-20s %8"PRId64" %8"PRId64"  (%0.1f per fast array)\n",
                    "  elements", s->fast_array_elements,
                    s->fast_array_elements * (int)sizeof(JSValue),
                    (double)s->fast_array_elements / s->fast_array_count);
        }
    }
    if (s->binary_object_count) {
        fprintf(fp, "%-20s %8"PRId64" %8"PRId64"\n",
                "binary objects", s->binary_object_count, s->binary_object_size);
    }
}

JSValue JS_GetGlobalObject(JSContext *ctx)
{
    return JS_DupValue(ctx, ctx->global_obj);
}

/* WARNING: obj is freed */
JSValue JS_Throw(JSContext *ctx, JSValue obj)
{
    JSRuntime *rt = ctx->rt;
    JS_FreeValue(ctx, rt->current_exception);
    rt->current_exception = obj;
    return JS_EXCEPTION;
}

/* return the pending exception (cannot be called twice). */
JSValue JS_GetException(JSContext *ctx)
{
    JSValue val;
    JSRuntime *rt = ctx->rt;
    val = rt->current_exception;
    rt->current_exception = JS_NULL;
    return val;
}

static void dbuf_put_leb128(DynBuf *s, uint32_t v)
{
    uint32_t a;
    for(;;) {
        a = v & 0x7f;
        v >>= 7;
        if (v != 0) {
            dbuf_putc(s, a | 0x80);
        } else {
            dbuf_putc(s, a);
            break;
        }
    }
}

static void dbuf_put_sleb128(DynBuf *s, int32_t v1)
{
    uint32_t v = v1;
    dbuf_put_leb128(s, (2 * v) ^ -(v >> 31));
}

static int get_leb128(uint32_t *pval, const uint8_t *buf,
                      const uint8_t *buf_end)
{
    const uint8_t *ptr = buf;
    uint32_t v, a, i;
    v = 0;
    for(i = 0; i < 5; i++) {
        if (unlikely(ptr >= buf_end))
            break;
        a = *ptr++;
        v |= (a & 0x7f) << (i * 7);
        if (!(a & 0x80)) {
            *pval = v;
            return ptr - buf;
        }
    }
    *pval = 0;
    return -1;
}

static int get_sleb128(int32_t *pval, const uint8_t *buf,
                       const uint8_t *buf_end)
{
    int ret;
    uint32_t val;
    ret = get_leb128(&val, buf, buf_end);
    if (ret < 0) {
        *pval = 0;
        return -1;
    }
    *pval = (val >> 1) ^ -(val & 1);
    return ret;
}

static int find_line_num(JSContext *ctx, JSFunctionBytecode *b,
                         uint32_t pc_value)
{
    const uint8_t *p_end, *p;
    int new_line_num, line_num, pc, v, ret;
    unsigned int op;

    if (!b->has_debug || !b->debug.pc2line_buf) {
        /* function was stripped */
        return -1;
    }

    p = b->debug.pc2line_buf;
    p_end = p + b->debug.pc2line_len;
    pc = 0;
    line_num = b->debug.line_num;
    while (p < p_end) {
        op = *p++;
        if (op == 0) {
            uint32_t val;
            ret = get_leb128(&val, p, p_end);
            if (ret < 0)
                goto fail;
            pc += val;
            p += ret;
            ret = get_sleb128(&v, p, p_end);
            if (ret < 0) {
            fail:
                /* should never happen */
                return b->debug.line_num;
            }
            p += ret;
            new_line_num = line_num + v;
        } else {
            op -= PC2LINE_OP_FIRST;
            pc += (op / PC2LINE_RANGE);
            new_line_num = line_num + (op % PC2LINE_RANGE) + PC2LINE_BASE;
        }
        if (pc_value < pc)
            return line_num;
        line_num = new_line_num;
    }
    return line_num;
}

/* in order to avoid executing arbitrary code during the stack trace
   generation, we only look at simple 'name' properties containing a
   string. */
static const char *get_func_name(JSContext *ctx, JSValueConst func)
{
    JSProperty *pr;
    JSShapeProperty *prs;
    JSValueConst val;
    
    if (JS_VALUE_GET_TAG(func) != JS_TAG_OBJECT)
        return NULL;
    prs = find_own_property(&pr, JS_VALUE_GET_OBJ(func), JS_ATOM_name);
    if (!prs)
        return NULL;
    if ((prs->flags & JS_PROP_TMASK) != JS_PROP_NORMAL)
        return NULL;
    val = pr->u.value;
    if (JS_VALUE_GET_TAG(val) != JS_TAG_STRING)
        return NULL;
    return JS_ToCString(ctx, val);
}

#define JS_BACKTRACE_FLAG_SKIP_FIRST_LEVEL (1 << 0)
/* only taken into account if filename is provided */
#define JS_BACKTRACE_FLAG_SINGLE_LEVEL     (1 << 1)

/* if filename != NULL, an additional level is added with the filename
   and line number information (used for parse error). */
static void build_backtrace(JSContext *ctx, JSValueConst error_obj,
                            const char *filename, int line_num,
                            int backtrace_flags)
{
    JSStackFrame *sf;
    JSValue str;
    DynBuf dbuf;
    const char *func_name_str;
    const char *str1;
    JSObject *p;
    BOOL backtrace_barrier;
    
    js_dbuf_init(ctx, &dbuf);
    if (filename) {
        dbuf_printf(&dbuf, "    at %s", filename);
        if (line_num != -1)
            dbuf_printf(&dbuf, ":%d", line_num);
        dbuf_putc(&dbuf, '\n');
        str = JS_NewString(ctx, filename);
        JS_DefinePropertyValue(ctx, error_obj, JS_ATOM_fileName, str,
                               JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
        JS_DefinePropertyValue(ctx, error_obj, JS_ATOM_lineNumber, JS_NewInt32(ctx, line_num),
                               JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
        if (backtrace_flags & JS_BACKTRACE_FLAG_SINGLE_LEVEL)
            goto done;
    }
    for(sf = ctx->rt->current_stack_frame; sf != NULL; sf = sf->prev_frame) {
        if (backtrace_flags & JS_BACKTRACE_FLAG_SKIP_FIRST_LEVEL) {
            backtrace_flags &= ~JS_BACKTRACE_FLAG_SKIP_FIRST_LEVEL;
            continue;
        }
        func_name_str = get_func_name(ctx, sf->cur_func);
        if (!func_name_str || func_name_str[0] == '\0')
            str1 = "<anonymous>";
        else
            str1 = func_name_str;
        dbuf_printf(&dbuf, "    at %s", str1);
        JS_FreeCString(ctx, func_name_str);

        p = JS_VALUE_GET_OBJ(sf->cur_func);
        backtrace_barrier = FALSE;
        if (js_class_has_bytecode(p->class_id)) {
            JSFunctionBytecode *b;
            const char *atom_str;
            int line_num1;

            b = p->u.func.function_bytecode;
            backtrace_barrier = b->backtrace_barrier;
            if (b->has_debug) {
                line_num1 = find_line_num(ctx, b,
                                          sf->cur_pc - b->byte_code_buf - 1);
                atom_str = JS_AtomToCString(ctx, b->debug.filename);
                dbuf_printf(&dbuf, " (%s",
                            atom_str ? atom_str : "<null>");
                JS_FreeCString(ctx, atom_str);
                if (line_num1 != -1)
                    dbuf_printf(&dbuf, ":%d", line_num1);
                dbuf_putc(&dbuf, ')');
            }
        } else {
            dbuf_printf(&dbuf, " (native)");
        }
        dbuf_putc(&dbuf, '\n');
        /* stop backtrace if JS_EVAL_FLAG_BACKTRACE_BARRIER was used */
        if (backtrace_barrier)
            break;
    }
 done:
    dbuf_putc(&dbuf, '\0');
    if (dbuf_error(&dbuf))
        str = JS_NULL;
    else
        str = JS_NewString(ctx, (char *)dbuf.buf);
    dbuf_free(&dbuf);
    JS_DefinePropertyValue(ctx, error_obj, JS_ATOM_stack, str,
                           JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
}

/* Note: it is important that no exception is returned by this function */
static BOOL is_backtrace_needed(JSContext *ctx, JSValueConst obj)
{
    JSObject *p;
    if (JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT)
        return FALSE;
    p = JS_VALUE_GET_OBJ(obj);
    if (p->class_id != JS_CLASS_ERROR)
        return FALSE;
    if (find_own_property1(p, JS_ATOM_stack))
        return FALSE;
    return TRUE;
}

JSValue JS_NewError(JSContext *ctx)
{
    return JS_NewObjectClass(ctx, JS_CLASS_ERROR);
}

static JSValue JS_ThrowError2(JSContext *ctx, JSErrorEnum error_num,
                              const char *fmt, va_list ap, BOOL add_backtrace)
{
    char buf[256];
    JSValue obj, ret;

    vsnprintf(buf, sizeof(buf), fmt, ap);
    obj = JS_NewObjectProtoClass(ctx, ctx->native_error_proto[error_num],
                                 JS_CLASS_ERROR);
    if (unlikely(JS_IsException(obj))) {
        /* out of memory: throw JS_NULL to avoid recursing */
        obj = JS_NULL;
    } else {
        JS_DefinePropertyValue(ctx, obj, JS_ATOM_message,
                               JS_NewString(ctx, buf),
                               JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
    }
    if (add_backtrace) {
        build_backtrace(ctx, obj, NULL, 0, 0);
    }
    ret = JS_Throw(ctx, obj);
    return ret;
}

static JSValue JS_ThrowError(JSContext *ctx, JSErrorEnum error_num,
                             const char *fmt, va_list ap)
{
    JSRuntime *rt = ctx->rt;
    JSStackFrame *sf;
    BOOL add_backtrace;

    /* the backtrace is added later if called from a bytecode function */
    sf = rt->current_stack_frame;
    add_backtrace = !rt->in_out_of_memory &&
        (!sf || (JS_GetFunctionBytecode(sf->cur_func) == NULL));
    return JS_ThrowError2(ctx, error_num, fmt, ap, add_backtrace);
}

JSValue __attribute__((format(printf, 2, 3))) JS_ThrowSyntaxError(JSContext *ctx, const char *fmt, ...)
{
    JSValue val;
    va_list ap;

    va_start(ap, fmt);
    val = JS_ThrowError(ctx, JS_SYNTAX_ERROR, fmt, ap);
    va_end(ap);
    return val;
}

JSValue __attribute__((format(printf, 2, 3))) JS_ThrowTypeError(JSContext *ctx, const char *fmt, ...)
{
    JSValue val;
    va_list ap;

    va_start(ap, fmt);
    val = JS_ThrowError(ctx, JS_TYPE_ERROR, fmt, ap);
    va_end(ap);
    return val;
}

static int __attribute__((format(printf, 3, 4))) JS_ThrowTypeErrorOrFalse(JSContext *ctx, int flags, const char *fmt, ...)
{
    va_list ap;

    if ((flags & JS_PROP_THROW) ||
        ((flags & JS_PROP_THROW_STRICT) && is_strict_mode(ctx))) {
        va_start(ap, fmt);
        JS_ThrowError(ctx, JS_TYPE_ERROR, fmt, ap);
        va_end(ap);
        return -1;
    } else {
        return FALSE;
    }
}

/* never use it directly */
static JSValue __attribute__((format(printf, 3, 4))) __JS_ThrowTypeErrorAtom(JSContext *ctx, JSAtom atom, const char *fmt, ...)
{
    char buf[ATOM_GET_STR_BUF_SIZE];
    return JS_ThrowTypeError(ctx, fmt,
                             JS_AtomGetStr(ctx, buf, sizeof(buf), atom));
}

/* never use it directly */
static JSValue __attribute__((format(printf, 3, 4))) __JS_ThrowSyntaxErrorAtom(JSContext *ctx, JSAtom atom, const char *fmt, ...)
{
    char buf[ATOM_GET_STR_BUF_SIZE];
    return JS_ThrowSyntaxError(ctx, fmt,
                             JS_AtomGetStr(ctx, buf, sizeof(buf), atom));
}

/* %s is replaced by 'atom'. The macro is used so that gcc can check
    the format string. */
#define JS_ThrowTypeErrorAtom(ctx, fmt, atom) __JS_ThrowTypeErrorAtom(ctx, atom, fmt, "")
#define JS_ThrowSyntaxErrorAtom(ctx, fmt, atom) __JS_ThrowSyntaxErrorAtom(ctx, atom, fmt, "")

static int JS_ThrowTypeErrorReadOnly(JSContext *ctx, int flags, JSAtom atom)
{
    if ((flags & JS_PROP_THROW) ||
        ((flags & JS_PROP_THROW_STRICT) && is_strict_mode(ctx))) {
        JS_ThrowTypeErrorAtom(ctx, "'%s' is read-only", atom);
        return -1;
    } else {
        return FALSE;
    }
}

JSValue __attribute__((format(printf, 2, 3))) JS_ThrowReferenceError(JSContext *ctx, const char *fmt, ...)
{
    JSValue val;
    va_list ap;

    va_start(ap, fmt);
    val = JS_ThrowError(ctx, JS_REFERENCE_ERROR, fmt, ap);
    va_end(ap);
    return val;
}

JSValue __attribute__((format(printf, 2, 3))) JS_ThrowRangeError(JSContext *ctx, const char *fmt, ...)
{
    JSValue val;
    va_list ap;

    va_start(ap, fmt);
    val = JS_ThrowError(ctx, JS_RANGE_ERROR, fmt, ap);
    va_end(ap);
    return val;
}

JSValue __attribute__((format(printf, 2, 3))) JS_ThrowInternalError(JSContext *ctx, const char *fmt, ...)
{
    JSValue val;
    va_list ap;

    va_start(ap, fmt);
    val = JS_ThrowError(ctx, JS_INTERNAL_ERROR, fmt, ap);
    va_end(ap);
    return val;
}

JSValue JS_ThrowOutOfMemory(JSContext *ctx)
{
    JSRuntime *rt = ctx->rt;
    if (!rt->in_out_of_memory) {
        rt->in_out_of_memory = TRUE;
        JS_ThrowInternalError(ctx, "out of memory");
        rt->in_out_of_memory = FALSE;
    }
    return JS_EXCEPTION;
}

static JSValue JS_ThrowStackOverflow(JSContext *ctx)
{
    return JS_ThrowInternalError(ctx, "stack overflow");
}

static JSValue JS_ThrowTypeErrorNotAnObject(JSContext *ctx)
{
    return JS_ThrowTypeError(ctx, "not an object");
}

static JSValue JS_ThrowTypeErrorNotASymbol(JSContext *ctx)
{
    return JS_ThrowTypeError(ctx, "not a symbol");
}

static JSValue JS_ThrowReferenceErrorNotDefined(JSContext *ctx, JSAtom name)
{
    char buf[ATOM_GET_STR_BUF_SIZE];
    return JS_ThrowReferenceError(ctx, "'%s' is not defined",
                                  JS_AtomGetStr(ctx, buf, sizeof(buf), name));
}

static JSValue JS_ThrowReferenceErrorUninitialized(JSContext *ctx, JSAtom name)
{
    char buf[ATOM_GET_STR_BUF_SIZE];
    return JS_ThrowReferenceError(ctx, "%s is not initialized",
                                  name == JS_ATOM_NULL ? "lexical variable" :
                                  JS_AtomGetStr(ctx, buf, sizeof(buf), name));
}

static JSValue JS_ThrowReferenceErrorUninitialized2(JSContext *ctx,
                                                    JSFunctionBytecode *b,
                                                    int idx, BOOL is_ref)
{
    JSAtom atom = JS_ATOM_NULL;
    if (is_ref) {
        atom = b->closure_var[idx].var_name;
    } else {
        /* not present if the function is stripped and contains no eval() */
        if (b->vardefs)
            atom = b->vardefs[b->arg_count + idx].var_name;
    }
    return JS_ThrowReferenceErrorUninitialized(ctx, atom);
}

static JSValue JS_ThrowTypeErrorInvalidClass(JSContext *ctx, int class_id)
{
    JSRuntime *rt = ctx->rt;
    JSAtom name;
    name = rt->class_array[class_id].class_name;
    return JS_ThrowTypeErrorAtom(ctx, "%s object expected", name);
}

static no_inline __exception int __js_poll_interrupts(JSContext *ctx)
{
    JSRuntime *rt = ctx->rt;
    ctx->interrupt_counter = JS_INTERRUPT_COUNTER_INIT;
    if (rt->interrupt_handler) {
        if (rt->interrupt_handler(rt, rt->interrupt_opaque)) {
            /* XXX: should set a specific flag to avoid catching */
            JS_ThrowInternalError(ctx, "interrupted");
            JS_SetUncatchableError(ctx, ctx->rt->current_exception, TRUE);
            return -1;
        }
    }
    return 0;
}

static inline __exception int js_poll_interrupts(JSContext *ctx)
{
    if (unlikely(--ctx->interrupt_counter <= 0)) {
        return __js_poll_interrupts(ctx);
    } else {
        return 0;
    }
}

/* return -1 (exception) or TRUE/FALSE */
static int JS_SetPrototypeInternal(JSContext *ctx, JSValueConst obj,
                                   JSValueConst proto_val,
                                   BOOL throw_flag)
{
    JSObject *proto, *p, *p1;
    JSShape *sh;

    if (throw_flag) {
        if (JS_VALUE_GET_TAG(obj) == JS_TAG_NULL ||
            JS_VALUE_GET_TAG(obj) == JS_TAG_UNDEFINED)
            goto not_obj;
    } else {
        if (JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT)
            goto not_obj;
    }
    p = JS_VALUE_GET_OBJ(obj);
    if (JS_VALUE_GET_TAG(proto_val) != JS_TAG_OBJECT) {
        if (JS_VALUE_GET_TAG(proto_val) != JS_TAG_NULL) {
        not_obj:
            JS_ThrowTypeErrorNotAnObject(ctx);
            return -1;
        }
        proto = NULL;
    } else {
        proto = JS_VALUE_GET_OBJ(proto_val);
    }

    if (throw_flag && JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT)
        return TRUE;

    if (unlikely(p->class_id == JS_CLASS_PROXY))
        return js_proxy_setPrototypeOf(ctx, obj, proto_val, throw_flag);
    sh = p->shape;
    if (sh->proto == proto)
        return TRUE;
    if (!p->extensible) {
        if (throw_flag) {
            JS_ThrowTypeError(ctx, "object is not extensible");
            return -1;
        } else {
            return FALSE;
        }
    }
    if (proto) {
        /* check if there is a cycle */
        p1 = proto;
        do {
            if (p1 == p) {
                if (throw_flag) {
                    JS_ThrowTypeError(ctx, "circular prototype chain");
                    return -1;
                } else {
                    return FALSE;
                }
            }
            /* Note: for Proxy objects, proto is NULL */
            p1 = p1->shape->proto;
        } while (p1 != NULL);
        JS_DupValue(ctx, proto_val);
    }

    if (js_shape_prepare_update(ctx, p, NULL))
        return -1;
    sh = p->shape;
    if (sh->proto)
        JS_FreeValue(ctx, JS_MKPTR(JS_TAG_OBJECT, sh->proto));
    sh->proto = proto;
    return TRUE;
}

/* return -1 (exception) or TRUE/FALSE */
int JS_SetPrototype(JSContext *ctx, JSValueConst obj, JSValueConst proto_val)
{
    return JS_SetPrototypeInternal(ctx, obj, proto_val, TRUE);
}

/* Only works for primitive types, otherwise return JS_NULL. */
static JSValueConst JS_GetPrototypePrimitive(JSContext *ctx, JSValueConst val)
{
    switch(JS_VALUE_GET_NORM_TAG(val)) {
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_INT:
        val = ctx->class_proto[JS_CLASS_BIG_INT];
        break;
    case JS_TAG_BIG_FLOAT:
        val = ctx->class_proto[JS_CLASS_BIG_FLOAT];
        break;
    case JS_TAG_BIG_DECIMAL:
        val = ctx->class_proto[JS_CLASS_BIG_DECIMAL];
        break;
#endif
    case JS_TAG_INT:
    case JS_TAG_FLOAT64:
        val = ctx->class_proto[JS_CLASS_NUMBER];
        break;
    case JS_TAG_BOOL:
        val = ctx->class_proto[JS_CLASS_BOOLEAN];
        break;
    case JS_TAG_STRING:
        val = ctx->class_proto[JS_CLASS_STRING];
        break;
    case JS_TAG_SYMBOL:
        val = ctx->class_proto[JS_CLASS_SYMBOL];
        break;
    case JS_TAG_OBJECT:
    case JS_TAG_NULL:
    case JS_TAG_UNDEFINED:
    default:
        val = JS_NULL;
        break;
    }
    return val;
}

/* Return an Object, JS_NULL or JS_EXCEPTION in case of Proxy object. */
JSValue JS_GetPrototype(JSContext *ctx, JSValueConst obj)
{
    JSValue val;
    if (JS_VALUE_GET_TAG(obj) == JS_TAG_OBJECT) {
        JSObject *p;
        p = JS_VALUE_GET_OBJ(obj);
        if (unlikely(p->class_id == JS_CLASS_PROXY)) {
            val = js_proxy_getPrototypeOf(ctx, obj);
        } else {
            p = p->shape->proto;
            if (!p)
                val = JS_NULL;
            else
                val = JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, p));
        }
    } else {
        val = JS_DupValue(ctx, JS_GetPrototypePrimitive(ctx, obj));
    }
    return val;
}

static JSValue JS_GetPrototypeFree(JSContext *ctx, JSValue obj)
{
    JSValue obj1;
    obj1 = JS_GetPrototype(ctx, obj);
    JS_FreeValue(ctx, obj);
    return obj1;
}

/* return TRUE, FALSE or (-1) in case of exception */
static int JS_OrdinaryIsInstanceOf(JSContext *ctx, JSValueConst val,
                                   JSValueConst obj)
{
    JSValue obj_proto;
    JSObject *proto;
    const JSObject *p, *proto1;
    BOOL ret;

    if (!JS_IsFunction(ctx, obj))
        return FALSE;
    p = JS_VALUE_GET_OBJ(obj);
    if (p->class_id == JS_CLASS_BOUND_FUNCTION) {
        JSBoundFunction *s = p->u.bound_function;
        return JS_IsInstanceOf(ctx, val, s->func_obj);
    }

    /* Only explicitly boxed values are instances of constructors */
    if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
        return FALSE;
    obj_proto = JS_GetProperty(ctx, obj, JS_ATOM_prototype);
    if (JS_VALUE_GET_TAG(obj_proto) != JS_TAG_OBJECT) {
        if (!JS_IsException(obj_proto))
            JS_ThrowTypeError(ctx, "operand 'prototype' property is not an object");
        ret = -1;
        goto done;
    }
    proto = JS_VALUE_GET_OBJ(obj_proto);
    p = JS_VALUE_GET_OBJ(val);
    for(;;) {
        proto1 = p->shape->proto;
        if (!proto1) {
            /* slow case if proxy in the prototype chain */
            if (unlikely(p->class_id == JS_CLASS_PROXY)) {
                JSValue obj1;
                obj1 = JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, (JSObject *)p));
                for(;;) {
                    obj1 = JS_GetPrototypeFree(ctx, obj1);
                    if (JS_IsException(obj1)) {
                        ret = -1;
                        break;
                    }
                    if (JS_IsNull(obj1)) {
                        ret = FALSE;
                        break;
                    }
                    if (proto == JS_VALUE_GET_OBJ(obj1)) {
                        JS_FreeValue(ctx, obj1);
                        ret = TRUE;
                        break;
                    }
                    /* must check for timeout to avoid infinite loop */
                    if (js_poll_interrupts(ctx)) {
                        JS_FreeValue(ctx, obj1);
                        ret = -1;
                        break;
                    }
                }
            } else {
                ret = FALSE;
            }
            break;
        }
        p = proto1;
        if (proto == p) {
            ret = TRUE;
            break;
        }
    }
done:
    JS_FreeValue(ctx, obj_proto);
    return ret;
}

/* return TRUE, FALSE or (-1) in case of exception */
int JS_IsInstanceOf(JSContext *ctx, JSValueConst val, JSValueConst obj)
{
    JSValue method;

    if (!JS_IsObject(obj))
        goto fail;
    method = JS_GetProperty(ctx, obj, JS_ATOM_Symbol_hasInstance);
    if (JS_IsException(method))
        return -1;
    if (!JS_IsNull(method) && !JS_IsUndefined(method)) {
        JSValue ret;
        ret = JS_CallFree(ctx, method, obj, 1, &val);
        return JS_ToBoolFree(ctx, ret);
    }

    /* legacy case */
    if (!JS_IsFunction(ctx, obj)) {
    fail:
        JS_ThrowTypeError(ctx, "invalid 'instanceof' right operand");
        return -1;
    }
    return JS_OrdinaryIsInstanceOf(ctx, val, obj);
}

/* return the value associated to the autoinit property or an exception */
typedef JSValue JSAutoInitFunc(JSContext *ctx, JSObject *p, JSAtom atom, void *opaque);

static JSAutoInitFunc *js_autoinit_func_table[] = {
    js_instantiate_prototype, /* JS_AUTOINIT_ID_PROTOTYPE */
    js_module_ns_autoinit, /* JS_AUTOINIT_ID_MODULE_NS */
    JS_InstantiateFunctionListItem2, /* JS_AUTOINIT_ID_PROP */
};

/* warning: 'prs' is reallocated after it */
static int JS_AutoInitProperty(JSContext *ctx, JSObject *p, JSAtom prop,
                               JSProperty *pr, JSShapeProperty *prs)
{
    JSValue val;
    JSContext *realm;
    JSAutoInitFunc *func;

    if (js_shape_prepare_update(ctx, p, &prs))
        return -1;

    realm = js_autoinit_get_realm(pr);
    func = js_autoinit_func_table[js_autoinit_get_id(pr)];
    /* 'func' shall not modify the object properties 'pr' */
    val = func(realm, p, prop, pr->u.init.opaque);
    js_autoinit_free(ctx->rt, pr);
    prs->flags &= ~JS_PROP_TMASK;
    pr->u.value = JS_UNDEFINED;
    if (JS_IsException(val))
        return -1;
    pr->u.value = val;
    return 0;
}

JSValue JS_GetPropertyInternal(JSContext *ctx, JSValueConst obj,
                               JSAtom prop, JSValueConst this_obj,
                               BOOL throw_ref_error)
{
    JSObject *p;
    JSProperty *pr;
    JSShapeProperty *prs;
    uint32_t tag;

    tag = JS_VALUE_GET_TAG(obj);
    if (unlikely(tag != JS_TAG_OBJECT)) {
        switch(tag) {
        case JS_TAG_NULL:
            return JS_ThrowTypeErrorAtom(ctx, "cannot read property '%s' of null", prop);
        case JS_TAG_UNDEFINED:
            return JS_ThrowTypeErrorAtom(ctx, "cannot read property '%s' of undefined", prop);
        case JS_TAG_EXCEPTION:
            return JS_EXCEPTION;
        case JS_TAG_STRING:
            {
                JSString *p1 = JS_VALUE_GET_STRING(obj);
                if (__JS_AtomIsTaggedInt(prop)) {
                    uint32_t idx, ch;
                    idx = __JS_AtomToUInt32(prop);
                    if (idx < p1->len) {
                        if (p1->is_wide_char)
                            ch = p1->u.str16[idx];
                        else
                            ch = p1->u.str8[idx];
                        return js_new_string_char(ctx, ch);
                    }
                } else if (prop == JS_ATOM_length) {
                    return JS_NewInt32(ctx, p1->len);
                }
            }
            break;
        default:
            break;
        }
        /* cannot raise an exception */
        p = JS_VALUE_GET_OBJ(JS_GetPrototypePrimitive(ctx, obj));
        if (!p)
            return JS_UNDEFINED;
    } else {
        p = JS_VALUE_GET_OBJ(obj);
    }

    for(;;) {
        prs = find_own_property(&pr, p, prop);
        if (prs) {
            /* found */
            if (unlikely(prs->flags & JS_PROP_TMASK)) {
                if ((prs->flags & JS_PROP_TMASK) == JS_PROP_GETSET) {
                    if (unlikely(!pr->u.getset.getter)) {
                        return JS_UNDEFINED;
                    } else {
                        JSValue func = JS_MKPTR(JS_TAG_OBJECT, pr->u.getset.getter);
                        /* Note: the field could be removed in the getter */
                        func = JS_DupValue(ctx, func);
                        return JS_CallFree(ctx, func, this_obj, 0, NULL);
                    }
                } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_VARREF) {
                    JSValue val = *pr->u.var_ref->pvalue;
                    if (unlikely(JS_IsUninitialized(val)))
                        return JS_ThrowReferenceErrorUninitialized(ctx, prs->atom);
                    return JS_DupValue(ctx, val);
                } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_AUTOINIT) {
                    /* Instantiate property and retry */
                    if (JS_AutoInitProperty(ctx, p, prop, pr, prs))
                        return JS_EXCEPTION;
                    continue;
                }
            } else {
                return JS_DupValue(ctx, pr->u.value);
            }
        }
        if (unlikely(p->is_exotic)) {
            /* exotic behaviors */
            if (p->fast_array) {
                if (__JS_AtomIsTaggedInt(prop)) {
                    uint32_t idx = __JS_AtomToUInt32(prop);
                    if (idx < p->u.array.count) {
                        /* we avoid duplicating the code */
                        return JS_GetPropertyUint32(ctx, JS_MKPTR(JS_TAG_OBJECT, p), idx);
                    } else if (p->class_id >= JS_CLASS_UINT8C_ARRAY &&
                               p->class_id <= JS_CLASS_FLOAT64_ARRAY) {
                        return JS_UNDEFINED;
                    }
                } else if (p->class_id >= JS_CLASS_UINT8C_ARRAY &&
                           p->class_id <= JS_CLASS_FLOAT64_ARRAY) {
                    int ret;
                    ret = JS_AtomIsNumericIndex(ctx, prop);
                    if (ret != 0) {
                        if (ret < 0)
                            return JS_EXCEPTION;
                        return JS_UNDEFINED;
                    }
                }
            } else {
                const JSClassExoticMethods *em = ctx->rt->class_array[p->class_id].exotic;
                if (em) {
                    if (em->get_property) {
                        JSValue obj1, retval;
                        /* XXX: should pass throw_ref_error */
                        /* Note: if 'p' is a prototype, it can be
                           freed in the called function */
                        obj1 = JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, p));
                        retval = em->get_property(ctx, obj1, prop, this_obj);
                        JS_FreeValue(ctx, obj1);
                        return retval;
                    }
                    if (em->get_own_property) {
                        JSPropertyDescriptor desc;
                        int ret;
                        JSValue obj1;

                        /* Note: if 'p' is a prototype, it can be
                           freed in the called function */
                        obj1 = JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, p));
                        ret = em->get_own_property(ctx, &desc, obj1, prop);
                        JS_FreeValue(ctx, obj1);
                        if (ret < 0)
                            return JS_EXCEPTION;
                        if (ret) {
                            if (desc.flags & JS_PROP_GETSET) {
                                JS_FreeValue(ctx, desc.setter);
                                return JS_CallFree(ctx, desc.getter, this_obj, 0, NULL);
                            } else {
                                return desc.value;
                            }
                        }
                    }
                }
            }
        }
        p = p->shape->proto;
        if (!p)
            break;
    }
    if (unlikely(throw_ref_error)) {
        return JS_ThrowReferenceErrorNotDefined(ctx, prop);
    } else {
        return JS_UNDEFINED;
    }
}

static JSValue JS_ThrowTypeErrorPrivateNotFound(JSContext *ctx, JSAtom atom)
{
    return JS_ThrowTypeErrorAtom(ctx, "private class field '%s' does not exist",
                                 atom);
}

/* Private fields can be added even on non extensible objects or
   Proxies */
static int JS_DefinePrivateField(JSContext *ctx, JSValueConst obj,
                                 JSValueConst name, JSValue val)
{
    JSObject *p;
    JSShapeProperty *prs;
    JSProperty *pr;
    JSAtom prop;

    if (unlikely(JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT)) {
        JS_ThrowTypeErrorNotAnObject(ctx);
        goto fail;
    }
    /* safety check */
    if (unlikely(JS_VALUE_GET_TAG(name) != JS_TAG_SYMBOL)) {
        JS_ThrowTypeErrorNotASymbol(ctx);
        goto fail;
    }
    prop = js_symbol_to_atom(ctx, (JSValue)name);
    p = JS_VALUE_GET_OBJ(obj);
    prs = find_own_property(&pr, p, prop);
    if (prs) {
        JS_ThrowTypeErrorAtom(ctx, "private class field '%s' already exists",
                              prop);
        goto fail;
    }
    pr = add_property(ctx, p, prop, JS_PROP_C_W_E);
    if (unlikely(!pr)) {
    fail:
        JS_FreeValue(ctx, val);
        return -1;
    }
    pr->u.value = val;
    return 0;
}

static JSValue JS_GetPrivateField(JSContext *ctx, JSValueConst obj,
                                  JSValueConst name)
{
    JSObject *p;
    JSShapeProperty *prs;
    JSProperty *pr;
    JSAtom prop;

    if (unlikely(JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT))
        return JS_ThrowTypeErrorNotAnObject(ctx);
    /* safety check */
    if (unlikely(JS_VALUE_GET_TAG(name) != JS_TAG_SYMBOL))
        return JS_ThrowTypeErrorNotASymbol(ctx);
    prop = js_symbol_to_atom(ctx, (JSValue)name);
    p = JS_VALUE_GET_OBJ(obj);
    prs = find_own_property(&pr, p, prop);
    if (!prs) {
        JS_ThrowTypeErrorPrivateNotFound(ctx, prop);
        return JS_EXCEPTION;
    }
    return JS_DupValue(ctx, pr->u.value);
}

static int JS_SetPrivateField(JSContext *ctx, JSValueConst obj,
                              JSValueConst name, JSValue val)
{
    JSObject *p;
    JSShapeProperty *prs;
    JSProperty *pr;
    JSAtom prop;

    if (unlikely(JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT)) {
        JS_ThrowTypeErrorNotAnObject(ctx);
        goto fail;
    }
    /* safety check */
    if (unlikely(JS_VALUE_GET_TAG(name) != JS_TAG_SYMBOL)) {
        JS_ThrowTypeErrorNotASymbol(ctx);
        goto fail;
    }
    prop = js_symbol_to_atom(ctx, (JSValue)name);
    p = JS_VALUE_GET_OBJ(obj);
    prs = find_own_property(&pr, p, prop);
    if (!prs) {
        JS_ThrowTypeErrorPrivateNotFound(ctx, prop);
    fail:
        JS_FreeValue(ctx, val);
        return -1;
    }
    set_value(ctx, &pr->u.value, val);
    return 0;
}

static int JS_AddBrand(JSContext *ctx, JSValueConst obj, JSValueConst home_obj)
{
    JSObject *p, *p1;
    JSShapeProperty *prs;
    JSProperty *pr;
    JSValue brand;
    JSAtom brand_atom;
    
    if (unlikely(JS_VALUE_GET_TAG(home_obj) != JS_TAG_OBJECT)) {
        JS_ThrowTypeErrorNotAnObject(ctx);
        return -1;
    }
    p = JS_VALUE_GET_OBJ(home_obj);
    prs = find_own_property(&pr, p, JS_ATOM_Private_brand);
    if (!prs) {
        brand = JS_NewSymbolFromAtom(ctx, JS_ATOM_brand, JS_ATOM_TYPE_PRIVATE);
        if (JS_IsException(brand))
            return -1;
        /* if the brand is not present, add it */
        pr = add_property(ctx, p, JS_ATOM_Private_brand, JS_PROP_C_W_E);
        if (!pr) {
            JS_FreeValue(ctx, brand);
            return -1;
        }
        pr->u.value = JS_DupValue(ctx, brand);
    } else {
        brand = JS_DupValue(ctx, pr->u.value);
    }
    brand_atom = js_symbol_to_atom(ctx, brand);
    
    if (unlikely(JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT)) {
        JS_ThrowTypeErrorNotAnObject(ctx);
        JS_FreeAtom(ctx, brand_atom);
        return -1;
    }
    p1 = JS_VALUE_GET_OBJ(obj);
    pr = add_property(ctx, p1, brand_atom, JS_PROP_C_W_E);
    JS_FreeAtom(ctx, brand_atom);
    if (!pr)
        return -1;
    pr->u.value = JS_UNDEFINED;
    return 0;
}

static int JS_CheckBrand(JSContext *ctx, JSValueConst obj, JSValueConst func)
{
    JSObject *p, *p1, *home_obj;
    JSShapeProperty *prs;
    JSProperty *pr;
    JSValueConst brand;
    
    /* get the home object of 'func' */
    if (unlikely(JS_VALUE_GET_TAG(func) != JS_TAG_OBJECT)) {
    not_obj:
        JS_ThrowTypeErrorNotAnObject(ctx);
        return -1;
    }
    p1 = JS_VALUE_GET_OBJ(func);
    if (!js_class_has_bytecode(p1->class_id))
        goto not_obj;
    home_obj = p1->u.func.home_object;
    if (!home_obj)
        goto not_obj;
    prs = find_own_property(&pr, home_obj, JS_ATOM_Private_brand);
    if (!prs) {
        JS_ThrowTypeError(ctx, "expecting <brand> private field");
        return -1;
    }
    brand = pr->u.value;
    /* safety check */
    if (unlikely(JS_VALUE_GET_TAG(brand) != JS_TAG_SYMBOL))
        goto not_obj;
    
    /* get the brand array of 'obj' */
    if (unlikely(JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT))
        goto not_obj;
    p = JS_VALUE_GET_OBJ(obj);
    prs = find_own_property(&pr, p, js_symbol_to_atom(ctx, (JSValue)brand));
    if (!prs) {
        JS_ThrowTypeError(ctx, "invalid brand on object");
        return -1;
    }
    return 0;
}

static uint32_t js_string_obj_get_length(JSContext *ctx,
                                         JSValueConst obj)
{
    JSObject *p;
    JSString *p1;
    uint32_t len = 0;

    /* This is a class exotic method: obj class_id is JS_CLASS_STRING */
    p = JS_VALUE_GET_OBJ(obj);
    if (JS_VALUE_GET_TAG(p->u.object_data) == JS_TAG_STRING) {
        p1 = JS_VALUE_GET_STRING(p->u.object_data);
        len = p1->len;
    }
    return len;
}

static int num_keys_cmp(const void *p1, const void *p2, void *opaque)
{
    JSContext *ctx = opaque;
    JSAtom atom1 = ((const JSPropertyEnum *)p1)->atom;
    JSAtom atom2 = ((const JSPropertyEnum *)p2)->atom;
    uint32_t v1, v2;
    BOOL atom1_is_integer, atom2_is_integer;

    atom1_is_integer = JS_AtomIsArrayIndex(ctx, &v1, atom1);
    atom2_is_integer = JS_AtomIsArrayIndex(ctx, &v2, atom2);
    assert(atom1_is_integer && atom2_is_integer);
    if (v1 < v2)
        return -1;
    else if (v1 == v2)
        return 0;
    else
        return 1;
}

static void js_free_prop_enum(JSContext *ctx, JSPropertyEnum *tab, uint32_t len)
{
    uint32_t i;
    if (tab) {
        for(i = 0; i < len; i++)
            JS_FreeAtom(ctx, tab[i].atom);
        js_free(ctx, tab);
    }
}

/* return < 0 in case if exception, 0 if OK. ptab and its atoms must
   be freed by the user. */
static int __exception JS_GetOwnPropertyNamesInternal(JSContext *ctx,
                                                      JSPropertyEnum **ptab,
                                                      uint32_t *plen,
                                                      JSObject *p, int flags)
{
    int i, j;
    JSShape *sh;
    JSShapeProperty *prs;
    JSPropertyEnum *tab_atom, *tab_exotic;
    JSAtom atom;
    uint32_t num_keys_count, str_keys_count, sym_keys_count, atom_count;
    uint32_t num_index, str_index, sym_index, exotic_count, exotic_keys_count;
    BOOL is_enumerable, num_sorted;
    uint32_t num_key;
    JSAtomKindEnum kind;
    
    /* clear pointer for consistency in case of failure */
    *ptab = NULL;
    *plen = 0;

    /* compute the number of returned properties */
    num_keys_count = 0;
    str_keys_count = 0;
    sym_keys_count = 0;
    exotic_keys_count = 0;
    exotic_count = 0;
    tab_exotic = NULL;
    sh = p->shape;
    for(i = 0, prs = get_shape_prop(sh); i < sh->prop_count; i++, prs++) {
        atom = prs->atom;
        if (atom != JS_ATOM_NULL) {
            is_enumerable = ((prs->flags & JS_PROP_ENUMERABLE) != 0);
            kind = JS_AtomGetKind(ctx, atom);
            if ((!(flags & JS_GPN_ENUM_ONLY) || is_enumerable) &&
                ((flags >> kind) & 1) != 0) {
                /* need to raise an exception in case of the module
                   name space (implicit GetOwnProperty) */
                if (unlikely((prs->flags & JS_PROP_TMASK) == JS_PROP_VARREF) &&
                    (flags & (JS_GPN_SET_ENUM | JS_GPN_ENUM_ONLY))) {
                    JSVarRef *var_ref = p->prop[i].u.var_ref;
                    if (unlikely(JS_IsUninitialized(*var_ref->pvalue))) {
                        JS_ThrowReferenceErrorUninitialized(ctx, prs->atom);
                        return -1;
                    }
                }
                if (JS_AtomIsArrayIndex(ctx, &num_key, atom)) {
                    num_keys_count++;
                } else if (kind == JS_ATOM_KIND_STRING) {
                    str_keys_count++;
                } else {
                    sym_keys_count++;
                }
            }
        }
    }

    if (p->is_exotic) {
        if (p->fast_array) {
            if (flags & JS_GPN_STRING_MASK) {
                num_keys_count += p->u.array.count;
            }
        } else if (p->class_id == JS_CLASS_STRING) {
            if (flags & JS_GPN_STRING_MASK) {
                num_keys_count += js_string_obj_get_length(ctx, JS_MKPTR(JS_TAG_OBJECT, p));
            }
        } else {
            const JSClassExoticMethods *em = ctx->rt->class_array[p->class_id].exotic;
            if (em && em->get_own_property_names) {
                if (em->get_own_property_names(ctx, &tab_exotic, &exotic_count,
                                               JS_MKPTR(JS_TAG_OBJECT, p)))
                    return -1;
                for(i = 0; i < exotic_count; i++) {
                    atom = tab_exotic[i].atom;
                    kind = JS_AtomGetKind(ctx, atom);
                    if (((flags >> kind) & 1) != 0) {
                        is_enumerable = FALSE;
                        if (flags & (JS_GPN_SET_ENUM | JS_GPN_ENUM_ONLY)) {
                            JSPropertyDescriptor desc;
                            int res;
                            /* set the "is_enumerable" field if necessary */
                            res = JS_GetOwnPropertyInternal(ctx, &desc, p, atom);
                            if (res < 0) {
                                js_free_prop_enum(ctx, tab_exotic, exotic_count);
                                return -1;
                            }
                            if (res) {
                                is_enumerable =
                                    ((desc.flags & JS_PROP_ENUMERABLE) != 0);
                                js_free_desc(ctx, &desc);
                            }
                            tab_exotic[i].is_enumerable = is_enumerable;
                        }
                        if (!(flags & JS_GPN_ENUM_ONLY) || is_enumerable) {
                            exotic_keys_count++;
                        }
                    }
                }
            }
        }
    }

    /* fill them */

    atom_count = num_keys_count + str_keys_count + sym_keys_count + exotic_keys_count;
    /* avoid allocating 0 bytes */
    tab_atom = js_malloc(ctx, sizeof(tab_atom[0]) * max_int(atom_count, 1));
    if (!tab_atom) {
        js_free_prop_enum(ctx, tab_exotic, exotic_count);
        return -1;
    }

    num_index = 0;
    str_index = num_keys_count;
    sym_index = str_index + str_keys_count;

    num_sorted = TRUE;
    sh = p->shape;
    for(i = 0, prs = get_shape_prop(sh); i < sh->prop_count; i++, prs++) {
        atom = prs->atom;
        if (atom != JS_ATOM_NULL) {
            is_enumerable = ((prs->flags & JS_PROP_ENUMERABLE) != 0);
            kind = JS_AtomGetKind(ctx, atom);
            if ((!(flags & JS_GPN_ENUM_ONLY) || is_enumerable) &&
                ((flags >> kind) & 1) != 0) {
                if (JS_AtomIsArrayIndex(ctx, &num_key, atom)) {
                    j = num_index++;
                    num_sorted = FALSE;
                } else if (kind == JS_ATOM_KIND_STRING) {
                    j = str_index++;
                } else {
                    j = sym_index++;
                }
                tab_atom[j].atom = JS_DupAtom(ctx, atom);
                tab_atom[j].is_enumerable = is_enumerable;
            }
        }
    }

    if (p->is_exotic) {
        int len;
        if (p->fast_array) {
            if (flags & JS_GPN_STRING_MASK) {
                len = p->u.array.count;
                goto add_array_keys;
            }
        } else if (p->class_id == JS_CLASS_STRING) {
            if (flags & JS_GPN_STRING_MASK) {
                len = js_string_obj_get_length(ctx, JS_MKPTR(JS_TAG_OBJECT, p));
            add_array_keys:
                for(i = 0; i < len; i++) {
                    tab_atom[num_index].atom = __JS_AtomFromUInt32(i);
                    if (tab_atom[num_index].atom == JS_ATOM_NULL) {
                        js_free_prop_enum(ctx, tab_atom, num_index);
                        return -1;
                    }
                    tab_atom[num_index].is_enumerable = TRUE;
                    num_index++;
                }
            }
        } else {
            /* Note: exotic keys are not reordered and comes after the object own properties. */
            for(i = 0; i < exotic_count; i++) {
                atom = tab_exotic[i].atom;
                is_enumerable = tab_exotic[i].is_enumerable;
                kind = JS_AtomGetKind(ctx, atom);
                if ((!(flags & JS_GPN_ENUM_ONLY) || is_enumerable) &&
                    ((flags >> kind) & 1) != 0) {
                    tab_atom[sym_index].atom = atom;
                    tab_atom[sym_index].is_enumerable = is_enumerable;
                    sym_index++;
                } else {
                    JS_FreeAtom(ctx, atom);
                }
            }
            js_free(ctx, tab_exotic);
        }
    }

    assert(num_index == num_keys_count);
    assert(str_index == num_keys_count + str_keys_count);
    assert(sym_index == atom_count);

    if (num_keys_count != 0 && !num_sorted) {
        rqsort(tab_atom, num_keys_count, sizeof(tab_atom[0]), num_keys_cmp,
               ctx);
    }
    *ptab = tab_atom;
    *plen = atom_count;
    return 0;
}

int JS_GetOwnPropertyNames(JSContext *ctx, JSPropertyEnum **ptab,
                           uint32_t *plen, JSValueConst obj, int flags)
{
    if (JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT) {
        JS_ThrowTypeErrorNotAnObject(ctx);
        return -1;
    }
    return JS_GetOwnPropertyNamesInternal(ctx, ptab, plen,
                                          JS_VALUE_GET_OBJ(obj), flags);
}

/* Return -1 if exception,
   FALSE if the property does not exist, TRUE if it exists. If TRUE is
   returned, the property descriptor 'desc' is filled present. */
static int JS_GetOwnPropertyInternal(JSContext *ctx, JSPropertyDescriptor *desc,
                                     JSObject *p, JSAtom prop)
{
    JSShapeProperty *prs;
    JSProperty *pr;

retry:
    prs = find_own_property(&pr, p, prop);
    if (prs) {
        if (desc) {
            desc->flags = prs->flags & JS_PROP_C_W_E;
            desc->getter = JS_UNDEFINED;
            desc->setter = JS_UNDEFINED;
            desc->value = JS_UNDEFINED;
            if (unlikely(prs->flags & JS_PROP_TMASK)) {
                if ((prs->flags & JS_PROP_TMASK) == JS_PROP_GETSET) {
                    desc->flags |= JS_PROP_GETSET;
                    if (pr->u.getset.getter)
                        desc->getter = JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, pr->u.getset.getter));
                    if (pr->u.getset.setter)
                        desc->setter = JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, pr->u.getset.setter));
                } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_VARREF) {
                    JSValue val = *pr->u.var_ref->pvalue;
                    if (unlikely(JS_IsUninitialized(val))) {
                        JS_ThrowReferenceErrorUninitialized(ctx, prs->atom);
                        return -1;
                    }
                    desc->value = JS_DupValue(ctx, val);
                } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_AUTOINIT) {
                    /* Instantiate property and retry */
                    if (JS_AutoInitProperty(ctx, p, prop, pr, prs))
                        return -1;
                    goto retry;
                }
            } else {
                desc->value = JS_DupValue(ctx, pr->u.value);
            }
        } else {
            /* for consistency, send the exception even if desc is NULL */
            if (unlikely((prs->flags & JS_PROP_TMASK) == JS_PROP_VARREF)) {
                if (unlikely(JS_IsUninitialized(*pr->u.var_ref->pvalue))) {
                    JS_ThrowReferenceErrorUninitialized(ctx, prs->atom);
                    return -1;
                }
            } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_AUTOINIT) {
                /* nothing to do: delay instantiation until actual value and/or attributes are read */
            }
        }
        return TRUE;
    }
    if (p->is_exotic) {
        if (p->fast_array) {
            /* specific case for fast arrays */
            if (__JS_AtomIsTaggedInt(prop)) {
                uint32_t idx;
                idx = __JS_AtomToUInt32(prop);
                if (idx < p->u.array.count) {
                    if (desc) {
                        desc->flags = JS_PROP_WRITABLE | JS_PROP_ENUMERABLE |
                            JS_PROP_CONFIGURABLE;
                        desc->getter = JS_UNDEFINED;
                        desc->setter = JS_UNDEFINED;
                        desc->value = JS_GetPropertyUint32(ctx, JS_MKPTR(JS_TAG_OBJECT, p), idx);
                    }
                    return TRUE;
                }
            }
        } else {
            const JSClassExoticMethods *em = ctx->rt->class_array[p->class_id].exotic;
            if (em && em->get_own_property) {
                return em->get_own_property(ctx, desc,
                                            JS_MKPTR(JS_TAG_OBJECT, p), prop);
            }
        }
    }
    return FALSE;
}

int JS_GetOwnProperty(JSContext *ctx, JSPropertyDescriptor *desc,
                      JSValueConst obj, JSAtom prop)
{
    if (JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT) {
        JS_ThrowTypeErrorNotAnObject(ctx);
        return -1;
    }
    return JS_GetOwnPropertyInternal(ctx, desc, JS_VALUE_GET_OBJ(obj), prop);
}

/* return -1 if exception (Proxy object only) or TRUE/FALSE */
int JS_IsExtensible(JSContext *ctx, JSValueConst obj)
{
    JSObject *p;

    if (unlikely(JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT))
        return FALSE;
    p = JS_VALUE_GET_OBJ(obj);
    if (unlikely(p->class_id == JS_CLASS_PROXY))
        return js_proxy_isExtensible(ctx, obj);
    else
        return p->extensible;
}

/* return -1 if exception (Proxy object only) or TRUE/FALSE */
int JS_PreventExtensions(JSContext *ctx, JSValueConst obj)
{
    JSObject *p;

    if (unlikely(JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT))
        return FALSE;
    p = JS_VALUE_GET_OBJ(obj);
    if (unlikely(p->class_id == JS_CLASS_PROXY))
        return js_proxy_preventExtensions(ctx, obj);
    p->extensible = FALSE;
    return TRUE;
}

/* return -1 if exception otherwise TRUE or FALSE */
int JS_HasProperty(JSContext *ctx, JSValueConst obj, JSAtom prop)
{
    JSObject *p;
    int ret;
    JSValue obj1;

    if (unlikely(JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT))
        return FALSE;
    p = JS_VALUE_GET_OBJ(obj);
    for(;;) {
        if (p->is_exotic) {
            const JSClassExoticMethods *em = ctx->rt->class_array[p->class_id].exotic;
            if (em && em->has_property) {
                /* has_property can free the prototype */
                obj1 = JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, p));
                ret = em->has_property(ctx, obj1, prop);
                JS_FreeValue(ctx, obj1);
                return ret;
            }
        }
        /* JS_GetOwnPropertyInternal can free the prototype */
        JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, p));
        ret = JS_GetOwnPropertyInternal(ctx, NULL, p, prop);
        JS_FreeValue(ctx, JS_MKPTR(JS_TAG_OBJECT, p));
        if (ret != 0)
            return ret;
        if (p->class_id >= JS_CLASS_UINT8C_ARRAY &&
            p->class_id <= JS_CLASS_FLOAT64_ARRAY) {
            ret = JS_AtomIsNumericIndex(ctx, prop);
            if (ret != 0) {
                if (ret < 0)
                    return -1;
                return FALSE;
            }
        }
        p = p->shape->proto;
        if (!p)
            break;
    }
    return FALSE;
}

/* val must be a symbol */
static JSAtom js_symbol_to_atom(JSContext *ctx, JSValue val)
{
    JSAtomStruct *p = JS_VALUE_GET_PTR(val);
    return js_get_atom_index(ctx->rt, p);
}

/* return JS_ATOM_NULL in case of exception */
JSAtom JS_ValueToAtom(JSContext *ctx, JSValueConst val)
{
    JSAtom atom;
    uint32_t tag;
    tag = JS_VALUE_GET_TAG(val);
    if (tag == JS_TAG_INT &&
        (uint32_t)JS_VALUE_GET_INT(val) <= JS_ATOM_MAX_INT) {
        /* fast path for integer values */
        atom = __JS_AtomFromUInt32(JS_VALUE_GET_INT(val));
    } else if (tag == JS_TAG_SYMBOL) {
        JSAtomStruct *p = JS_VALUE_GET_PTR(val);
        atom = JS_DupAtom(ctx, js_get_atom_index(ctx->rt, p));
    } else {
        JSValue str;
        str = JS_ToPropertyKey(ctx, val);
        if (JS_IsException(str))
            return JS_ATOM_NULL;
        if (JS_VALUE_GET_TAG(str) == JS_TAG_SYMBOL) {
            atom = js_symbol_to_atom(ctx, str);
        } else {
            atom = JS_NewAtomStr(ctx, JS_VALUE_GET_STRING(str));
        }
    }
    return atom;
}

static JSValue JS_GetPropertyValue(JSContext *ctx, JSValueConst this_obj,
                                   JSValue prop)
{
    JSAtom atom;
    JSValue ret;

    if (likely(JS_VALUE_GET_TAG(this_obj) == JS_TAG_OBJECT &&
               JS_VALUE_GET_TAG(prop) == JS_TAG_INT)) {
        JSObject *p;
        uint32_t idx, len;
        /* fast path for array access */
        p = JS_VALUE_GET_OBJ(this_obj);
        idx = JS_VALUE_GET_INT(prop);
        len = (uint32_t)p->u.array.count;
        if (unlikely(idx >= len))
            goto slow_path;
        switch(p->class_id) {
        case JS_CLASS_ARRAY:
        case JS_CLASS_ARGUMENTS:
            return JS_DupValue(ctx, p->u.array.u.values[idx]);
        case JS_CLASS_INT8_ARRAY:
            return JS_NewInt32(ctx, p->u.array.u.int8_ptr[idx]);
        case JS_CLASS_UINT8C_ARRAY:
        case JS_CLASS_UINT8_ARRAY:
            return JS_NewInt32(ctx, p->u.array.u.uint8_ptr[idx]);
        case JS_CLASS_INT16_ARRAY:
            return JS_NewInt32(ctx, p->u.array.u.int16_ptr[idx]);
        case JS_CLASS_UINT16_ARRAY:
            return JS_NewInt32(ctx, p->u.array.u.uint16_ptr[idx]);
        case JS_CLASS_INT32_ARRAY:
            return JS_NewInt32(ctx, p->u.array.u.int32_ptr[idx]);
        case JS_CLASS_UINT32_ARRAY:
            return JS_NewUint32(ctx, p->u.array.u.uint32_ptr[idx]);
#ifdef CONFIG_BIGNUM
        case JS_CLASS_BIG_INT64_ARRAY:
            return JS_NewBigInt64(ctx, p->u.array.u.int64_ptr[idx]);
        case JS_CLASS_BIG_UINT64_ARRAY:
            return JS_NewBigUint64(ctx, p->u.array.u.uint64_ptr[idx]);
#endif
        case JS_CLASS_FLOAT32_ARRAY:
            return __JS_NewFloat64(ctx, p->u.array.u.float_ptr[idx]);
        case JS_CLASS_FLOAT64_ARRAY:
            return __JS_NewFloat64(ctx, p->u.array.u.double_ptr[idx]);
        default:
            goto slow_path;
        }
    } else {
    slow_path:
        atom = JS_ValueToAtom(ctx, prop);
        JS_FreeValue(ctx, prop);
        if (unlikely(atom == JS_ATOM_NULL))
            return JS_EXCEPTION;
        ret = JS_GetProperty(ctx, this_obj, atom);
        JS_FreeAtom(ctx, atom);
        return ret;
    }
}

JSValue JS_GetPropertyUint32(JSContext *ctx, JSValueConst this_obj,
                             uint32_t idx)
{
    return JS_GetPropertyValue(ctx, this_obj, JS_NewUint32(ctx, idx));
}

/* Check if an object has a generalized numeric property. Return value:
   -1 for exception,
   TRUE if property exists, stored into *pval,
   FALSE if proprty does not exist.
 */
static int JS_TryGetPropertyInt64(JSContext *ctx, JSValueConst obj, int64_t idx, JSValue *pval)
{
    JSValue val = JS_UNDEFINED;
    JSAtom prop;
    int present;

    if (likely((uint64_t)idx <= JS_ATOM_MAX_INT)) {
        /* fast path */
        present = JS_HasProperty(ctx, obj, __JS_AtomFromUInt32(idx));
        if (present > 0) {
            val = JS_GetPropertyValue(ctx, obj, JS_NewInt32(ctx, idx));
            if (unlikely(JS_IsException(val)))
                present = -1;
        }
    } else {
        prop = JS_NewAtomInt64(ctx, idx);
        present = -1;
        if (likely(prop != JS_ATOM_NULL)) {
            present = JS_HasProperty(ctx, obj, prop);
            if (present > 0) {
                val = JS_GetProperty(ctx, obj, prop);
                if (unlikely(JS_IsException(val)))
                    present = -1;
            }
            JS_FreeAtom(ctx, prop);
        }
    }
    *pval = val;
    return present;
}

static JSValue JS_GetPropertyInt64(JSContext *ctx, JSValueConst obj, int64_t idx)
{
    JSAtom prop;
    JSValue val;

    if ((uint64_t)idx <= INT32_MAX) {
        /* fast path for fast arrays */
        return JS_GetPropertyValue(ctx, obj, JS_NewInt32(ctx, idx));
    }
    prop = JS_NewAtomInt64(ctx, idx);
    if (prop == JS_ATOM_NULL)
        return JS_EXCEPTION;

    val = JS_GetProperty(ctx, obj, prop);
    JS_FreeAtom(ctx, prop);
    return val;
}

JSValue JS_GetPropertyStr(JSContext *ctx, JSValueConst this_obj,
                          const char *prop)
{
    JSAtom atom;
    JSValue ret;
    atom = JS_NewAtom(ctx, prop);
    ret = JS_GetProperty(ctx, this_obj, atom);
    JS_FreeAtom(ctx, atom);
    return ret;
}

/* Note: the property value is not initialized. Return NULL if memory
   error. */
static JSProperty *add_property(JSContext *ctx,
                                JSObject *p, JSAtom prop, int prop_flags)
{
    JSShape *sh, *new_sh;

    sh = p->shape;
    if (sh->is_hashed) {
        /* try to find an existing shape */
        new_sh = find_hashed_shape_prop(ctx->rt, sh, prop, prop_flags);
        if (new_sh) {
            /* matching shape found: use it */
            /*  the property array may need to be resized */
            if (new_sh->prop_size != sh->prop_size) {
                JSProperty *new_prop;
                new_prop = js_realloc(ctx, p->prop, sizeof(p->prop[0]) *
                                      new_sh->prop_size);
                if (!new_prop)
                    return NULL;
                p->prop = new_prop;
            }
            p->shape = js_dup_shape(new_sh);
            js_free_shape(ctx->rt, sh);
            return &p->prop[new_sh->prop_count - 1];
        } else if (sh->header.ref_count != 1) {
            /* if the shape is shared, clone it */
            new_sh = js_clone_shape(ctx, sh);
            if (!new_sh)
                return NULL;
            /* hash the cloned shape */
            new_sh->is_hashed = TRUE;
            js_shape_hash_link(ctx->rt, new_sh);
            js_free_shape(ctx->rt, p->shape);
            p->shape = new_sh;
        }
    }
    assert(p->shape->header.ref_count == 1);
    if (add_shape_property(ctx, &p->shape, p, prop, prop_flags))
        return NULL;
    return &p->prop[p->shape->prop_count - 1];
}

/* can be called on Array or Arguments objects. return < 0 if
   memory alloc error. */
static no_inline __exception int convert_fast_array_to_array(JSContext *ctx,
                                                             JSObject *p)
{
    JSProperty *pr;
    JSShape *sh;
    JSValue *tab;
    uint32_t i, len, new_count;

    if (js_shape_prepare_update(ctx, p, NULL))
        return -1;
    len = p->u.array.count;
    /* resize the properties once to simplify the error handling */
    sh = p->shape;
    new_count = sh->prop_count + len;
    if (new_count > sh->prop_size) {
        if (resize_properties(ctx, &p->shape, p, new_count))
            return -1;
    }

    tab = p->u.array.u.values;
    for(i = 0; i < len; i++) {
        /* add_property cannot fail here but
           __JS_AtomFromUInt32(i) fails for i > INT32_MAX */
        pr = add_property(ctx, p, __JS_AtomFromUInt32(i), JS_PROP_C_W_E);
        pr->u.value = *tab++;
    }
    js_free(ctx, p->u.array.u.values);
    p->u.array.count = 0;
    p->u.array.u.values = NULL; /* fail safe */
    p->u.array.u1.size = 0;
    p->fast_array = 0;
    return 0;
}

static int delete_property(JSContext *ctx, JSObject *p, JSAtom atom)
{
    JSShape *sh;
    JSShapeProperty *pr, *lpr, *prop;
    JSProperty *pr1;
    uint32_t lpr_idx;
    intptr_t h, h1;

 redo:
    sh = p->shape;
    h1 = atom & sh->prop_hash_mask;
    h = prop_hash_end(sh)[-h1 - 1];
    prop = get_shape_prop(sh);
    lpr = NULL;
    lpr_idx = 0;   /* prevent warning */
    while (h != 0) {
        pr = &prop[h - 1];
        if (likely(pr->atom == atom)) {
            /* found ! */
            if (!(pr->flags & JS_PROP_CONFIGURABLE))
                return FALSE;
            /* realloc the shape if needed */
            if (lpr)
                lpr_idx = lpr - get_shape_prop(sh);
            if (js_shape_prepare_update(ctx, p, &pr))
                return -1;
            sh = p->shape;
            /* remove property */
            if (lpr) {
                lpr = get_shape_prop(sh) + lpr_idx;
                lpr->hash_next = pr->hash_next;
            } else {
                prop_hash_end(sh)[-h1 - 1] = pr->hash_next;
            }
            sh->deleted_prop_count++;
            /* free the entry */
            pr1 = &p->prop[h - 1];
            free_property(ctx->rt, pr1, pr->flags);
            JS_FreeAtom(ctx, pr->atom);
            /* put default values */
            pr->flags = 0;
            pr->atom = JS_ATOM_NULL;
            pr1->u.value = JS_UNDEFINED;

            /* compact the properties if too many deleted properties */
            if (sh->deleted_prop_count >= 8 &&
                sh->deleted_prop_count >= ((unsigned)sh->prop_count / 2)) {
                compact_properties(ctx, p);
            }
            return TRUE;
        }
        lpr = pr;
        h = pr->hash_next;
    }

    if (p->is_exotic) {
        if (p->fast_array) {
            uint32_t idx;
            if (JS_AtomIsArrayIndex(ctx, &idx, atom) &&
                idx < p->u.array.count) {
                if (p->class_id == JS_CLASS_ARRAY ||
                    p->class_id == JS_CLASS_ARGUMENTS) {
                    /* Special case deleting the last element of a fast Array */
                    if (idx == p->u.array.count - 1) {
                        JS_FreeValue(ctx, p->u.array.u.values[idx]);
                        p->u.array.count = idx;
                        return TRUE;
                    }
                    if (convert_fast_array_to_array(ctx, p))
                        return -1;
                    goto redo;
                } else {
                    return FALSE;
                }
            }
        } else {
            const JSClassExoticMethods *em = ctx->rt->class_array[p->class_id].exotic;
            if (em && em->delete_property) {
                return em->delete_property(ctx, JS_MKPTR(JS_TAG_OBJECT, p), atom);
            }
        }
    }
    /* not found */
    return TRUE;
}

static int call_setter(JSContext *ctx, JSObject *setter,
                       JSValueConst this_obj, JSValue val, int flags)
{
    JSValue ret, func;
    if (likely(setter)) {
        func = JS_MKPTR(JS_TAG_OBJECT, setter);
        /* Note: the field could be removed in the setter */
        func = JS_DupValue(ctx, func);
        ret = JS_CallFree(ctx, func, this_obj, 1, (JSValueConst *)&val);
        JS_FreeValue(ctx, val);
        if (JS_IsException(ret))
            return -1;
        JS_FreeValue(ctx, ret);
        return TRUE;
    } else {
        JS_FreeValue(ctx, val);
        if ((flags & JS_PROP_THROW) ||
            ((flags & JS_PROP_THROW_STRICT) && is_strict_mode(ctx))) {
            JS_ThrowTypeError(ctx, "no setter for property");
            return -1;
        }
        return FALSE;
    }
}

/* set the array length and remove the array elements if necessary. */
static int set_array_length(JSContext *ctx, JSObject *p, JSValue val,
                            int flags)
{
    uint32_t len, idx, cur_len;
    int i, ret;

    /* Note: this call can reallocate the properties of 'p' */
    ret = JS_ToArrayLengthFree(ctx, &len, val, FALSE);
    if (ret)
        return -1;
    /* JS_ToArrayLengthFree() must be done before the read-only test */
    if (unlikely(!(p->shape->prop[0].flags & JS_PROP_WRITABLE)))
        return JS_ThrowTypeErrorReadOnly(ctx, flags, JS_ATOM_length);

    if (likely(p->fast_array)) {
        uint32_t old_len = p->u.array.count;
        if (len < old_len) {
            for(i = len; i < old_len; i++) {
                JS_FreeValue(ctx, p->u.array.u.values[i]);
            }
            p->u.array.count = len;
        }
        p->prop[0].u.value = JS_NewUint32(ctx, len);
    } else {
        /* Note: length is always a uint32 because the object is an
           array */
        JS_ToUint32(ctx, &cur_len, p->prop[0].u.value);
        if (len < cur_len) {
            uint32_t d;
            JSShape *sh;
            JSShapeProperty *pr;

            d = cur_len - len;
            sh = p->shape;
            if (d <= sh->prop_count) {
                JSAtom atom;

                /* faster to iterate */
                while (cur_len > len) {
                    atom = JS_NewAtomUInt32(ctx, cur_len - 1);
                    ret = delete_property(ctx, p, atom);
                    JS_FreeAtom(ctx, atom);
                    if (unlikely(!ret)) {
                        /* unlikely case: property is not
                           configurable */
                        break;
                    }
                    cur_len--;
                }
            } else {
                /* faster to iterate thru all the properties. Need two
                   passes in case one of the property is not
                   configurable */
                cur_len = len;
                for(i = 0, pr = get_shape_prop(sh); i < sh->prop_count;
                    i++, pr++) {
                    if (pr->atom != JS_ATOM_NULL &&
                        JS_AtomIsArrayIndex(ctx, &idx, pr->atom)) {
                        if (idx >= cur_len &&
                            !(pr->flags & JS_PROP_CONFIGURABLE)) {
                            cur_len = idx + 1;
                        }
                    }
                }

                for(i = 0, pr = get_shape_prop(sh); i < sh->prop_count;
                    i++, pr++) {
                    if (pr->atom != JS_ATOM_NULL &&
                        JS_AtomIsArrayIndex(ctx, &idx, pr->atom)) {
                        if (idx >= cur_len) {
                            /* remove the property */
                            delete_property(ctx, p, pr->atom);
                            /* WARNING: the shape may have been modified */
                            sh = p->shape;
                            pr = get_shape_prop(sh) + i;
                        }
                    }
                }
            }
        } else {
            cur_len = len;
        }
        set_value(ctx, &p->prop[0].u.value, JS_NewUint32(ctx, cur_len));
        if (unlikely(cur_len > len)) {
            return JS_ThrowTypeErrorOrFalse(ctx, flags, "not configurable");
        }
    }
    return TRUE;
}

/* return -1 if exception */
static int expand_fast_array(JSContext *ctx, JSObject *p, uint32_t new_len)
{
    uint32_t new_size;
    size_t slack;
    JSValue *new_array_prop;
    /* XXX: potential arithmetic overflow */
    new_size = max_int(new_len, p->u.array.u1.size * 3 / 2);
    new_array_prop = js_realloc2(ctx, p->u.array.u.values, sizeof(JSValue) * new_size, &slack);
    if (!new_array_prop)
        return -1;
    new_size += slack / sizeof(*new_array_prop);
    p->u.array.u.values = new_array_prop;
    p->u.array.u1.size = new_size;
    return 0;
}

/* Preconditions: 'p' must be of class JS_CLASS_ARRAY, p->fast_array =
   TRUE and p->extensible = TRUE */
static int add_fast_array_element(JSContext *ctx, JSObject *p,
                                  JSValue val, int flags)
{
    uint32_t new_len, array_len;
    /* extend the array by one */
    /* XXX: convert to slow array if new_len > 2^31-1 elements */
    new_len = p->u.array.count + 1;
    /* update the length if necessary. We assume that if the length is
       not an integer, then if it >= 2^31.  */
    if (likely(JS_VALUE_GET_TAG(p->prop[0].u.value) == JS_TAG_INT)) {
        array_len = JS_VALUE_GET_INT(p->prop[0].u.value);
        if (new_len > array_len) {
            if (unlikely(!(get_shape_prop(p->shape)->flags & JS_PROP_WRITABLE))) {
                JS_FreeValue(ctx, val);
                return JS_ThrowTypeErrorReadOnly(ctx, flags, JS_ATOM_length);
            }
            p->prop[0].u.value = JS_NewInt32(ctx, new_len);
        }
    }
    if (unlikely(new_len > p->u.array.u1.size)) {
        if (expand_fast_array(ctx, p, new_len)) {
            JS_FreeValue(ctx, val);
            return -1;
        }
    }
    p->u.array.u.values[new_len - 1] = val;
    p->u.array.count = new_len;
    return TRUE;
}

static void js_free_desc(JSContext *ctx, JSPropertyDescriptor *desc)
{
    JS_FreeValue(ctx, desc->getter);
    JS_FreeValue(ctx, desc->setter);
    JS_FreeValue(ctx, desc->value);
}

/* generic (and slower) version of JS_SetProperty() for
 * Reflect.set(). 'obj' must be an object.  */
static int JS_SetPropertyGeneric(JSContext *ctx,
                                 JSValueConst obj, JSAtom prop,
                                 JSValue val, JSValueConst this_obj,
                                 int flags)
{
    int ret;
    JSPropertyDescriptor desc;
    JSValue obj1;
    JSObject *p;
    
    obj1 = JS_DupValue(ctx, obj);
    for(;;) {
        p = JS_VALUE_GET_OBJ(obj1);
        if (p->is_exotic) {
            const JSClassExoticMethods *em = ctx->rt->class_array[p->class_id].exotic;
            if (em && em->set_property) {
                ret = em->set_property(ctx, obj1, prop,
                                       val, this_obj, flags);
                JS_FreeValue(ctx, obj1);
                JS_FreeValue(ctx, val);
                return ret;
            }
        }

        ret = JS_GetOwnPropertyInternal(ctx, &desc, p, prop);
        if (ret < 0) {
            JS_FreeValue(ctx, obj1);
            JS_FreeValue(ctx, val);
            return ret;
        }
        if (ret) {
            if (desc.flags & JS_PROP_GETSET) {
                JSObject *setter;
                if (JS_IsUndefined(desc.setter))
                    setter = NULL;
                else
                    setter = JS_VALUE_GET_OBJ(desc.setter);
                ret = call_setter(ctx, setter, this_obj, val, flags);
                JS_FreeValue(ctx, desc.getter);
                JS_FreeValue(ctx, desc.setter);
                JS_FreeValue(ctx, obj1);
                return ret;
            } else {
                JS_FreeValue(ctx, desc.value);
                if (!(desc.flags & JS_PROP_WRITABLE)) {
                    JS_FreeValue(ctx, obj1);
                    goto read_only_error;
                }
            }
            break;
        }
        /* Note: at this point 'obj1' cannot be a proxy. XXX: may have
           to check recursion */
        obj1 = JS_GetPrototypeFree(ctx, obj1);
        if (JS_IsNull(obj1))
            break;
    }
    JS_FreeValue(ctx, obj1);

    if (!JS_IsObject(this_obj)) {
        JS_FreeValue(ctx, val);
        return JS_ThrowTypeErrorOrFalse(ctx, flags, "receiver is not an object");
    }
    
    p = JS_VALUE_GET_OBJ(this_obj);

    /* modify the property in this_obj if it already exists */
    ret = JS_GetOwnPropertyInternal(ctx, &desc, p, prop);
    if (ret < 0) {
        JS_FreeValue(ctx, val);
        return ret;
    }
    if (ret) {
        if (desc.flags & JS_PROP_GETSET) {
            JS_FreeValue(ctx, desc.getter);
            JS_FreeValue(ctx, desc.setter);
            JS_FreeValue(ctx, val);
            return JS_ThrowTypeErrorOrFalse(ctx, flags, "setter is forbidden");
        } else {
            JS_FreeValue(ctx, desc.value);
            if (!(desc.flags & JS_PROP_WRITABLE) ||
                p->class_id == JS_CLASS_MODULE_NS) {
            read_only_error:
                JS_FreeValue(ctx, val);
                return JS_ThrowTypeErrorReadOnly(ctx, flags, prop);
            }
        }
        ret = JS_DefineProperty(ctx, this_obj, prop, val,
                                JS_UNDEFINED, JS_UNDEFINED,
                                JS_PROP_HAS_VALUE);
        JS_FreeValue(ctx, val);
        return ret;
    }

    ret = JS_CreateProperty(ctx, p, prop, val, JS_UNDEFINED, JS_UNDEFINED,
                            flags |
                            JS_PROP_HAS_VALUE |
                            JS_PROP_HAS_ENUMERABLE |
                            JS_PROP_HAS_WRITABLE |
                            JS_PROP_HAS_CONFIGURABLE |
                            JS_PROP_C_W_E);
    JS_FreeValue(ctx, val);
    return ret;
}

/* return -1 in case of exception or TRUE or FALSE. Warning: 'val' is
   freed by the function. 'flags' is a bitmask of JS_PROP_NO_ADD,
   JS_PROP_THROW or JS_PROP_THROW_STRICT. If JS_PROP_NO_ADD is set,
   the new property is not added and an error is raised. */
int JS_SetPropertyInternal(JSContext *ctx, JSValueConst this_obj,
                           JSAtom prop, JSValue val, int flags)
{
    JSObject *p, *p1;
    JSShapeProperty *prs;
    JSProperty *pr;
    uint32_t tag;
    JSPropertyDescriptor desc;
    int ret;
#if 0
    printf("JS_SetPropertyInternal: "); print_atom(ctx, prop); printf("\n");
#endif
    tag = JS_VALUE_GET_TAG(this_obj);
    if (unlikely(tag != JS_TAG_OBJECT)) {
        switch(tag) {
        case JS_TAG_NULL:
            JS_FreeValue(ctx, val);
            JS_ThrowTypeErrorAtom(ctx, "cannot set property '%s' of null", prop);
            return -1;
        case JS_TAG_UNDEFINED:
            JS_FreeValue(ctx, val);
            JS_ThrowTypeErrorAtom(ctx, "cannot set property '%s' of undefined", prop);
            return -1;
        default:
            /* even on a primitive type we can have setters on the prototype */
            p = NULL;
            p1 = JS_VALUE_GET_OBJ(JS_GetPrototypePrimitive(ctx, this_obj));
            goto prototype_lookup;
        }
    }
    p = JS_VALUE_GET_OBJ(this_obj);
retry:
    prs = find_own_property(&pr, p, prop);
    if (prs) {
        if (likely((prs->flags & (JS_PROP_TMASK | JS_PROP_WRITABLE |
                                  JS_PROP_LENGTH)) == JS_PROP_WRITABLE)) {
            /* fast case */
            set_value(ctx, &pr->u.value, val);
            return TRUE;
        } else if (prs->flags & JS_PROP_LENGTH) {
            assert(p->class_id == JS_CLASS_ARRAY);
            assert(prop == JS_ATOM_length);
            return set_array_length(ctx, p, val, flags);
        } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_GETSET) {
            return call_setter(ctx, pr->u.getset.setter, this_obj, val, flags);
        } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_VARREF) {
            /* JS_PROP_WRITABLE is always true for variable
               references, but they are write protected in module name
               spaces. */
            if (p->class_id == JS_CLASS_MODULE_NS)
                goto read_only_prop;
            set_value(ctx, pr->u.var_ref->pvalue, val);
            return TRUE;
        } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_AUTOINIT) {
            /* Instantiate property and retry (potentially useless) */
            if (JS_AutoInitProperty(ctx, p, prop, pr, prs)) {
                JS_FreeValue(ctx, val);
                return -1;
            }
            goto retry;
        } else {
            goto read_only_prop;
        }
    }

    p1 = p;
    for(;;) {
        if (p1->is_exotic) {
            if (p1->fast_array) {
                if (__JS_AtomIsTaggedInt(prop)) {
                    uint32_t idx = __JS_AtomToUInt32(prop);
                    if (idx < p1->u.array.count) {
                        if (unlikely(p == p1))
                            return JS_SetPropertyValue(ctx, this_obj, JS_NewInt32(ctx, idx), val, flags);
                        else
                            break;
                    } else if (p1->class_id >= JS_CLASS_UINT8C_ARRAY &&
                               p1->class_id <= JS_CLASS_FLOAT64_ARRAY) {
                        goto typed_array_oob;
                    }
                } else if (p1->class_id >= JS_CLASS_UINT8C_ARRAY &&
                           p1->class_id <= JS_CLASS_FLOAT64_ARRAY) {
                    ret = JS_AtomIsNumericIndex(ctx, prop);
                    if (ret != 0) {
                        if (ret < 0) {
                            JS_FreeValue(ctx, val);
                            return -1;
                        }
                    typed_array_oob:
                        val = JS_ToNumberFree(ctx, val);
                        JS_FreeValue(ctx, val);
                        if (JS_IsException(val))
                            return -1;
                        return JS_ThrowTypeErrorOrFalse(ctx, flags, "out-of-bound numeric index");
                    }
                }
            } else {
                const JSClassExoticMethods *em = ctx->rt->class_array[p1->class_id].exotic;
                if (em) {
                    JSValue obj1;
                    if (em->set_property) {
                        /* set_property can free the prototype */
                        obj1 = JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, p1));
                        ret = em->set_property(ctx, obj1, prop,
                                               val, this_obj, flags);
                        JS_FreeValue(ctx, obj1);
                        JS_FreeValue(ctx, val);
                        return ret;
                    }
                    if (em->get_own_property) {
                        /* get_own_property can free the prototype */
                        obj1 = JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, p1));
                        ret = em->get_own_property(ctx, &desc,
                                                   obj1, prop);
                        JS_FreeValue(ctx, obj1);
                        if (ret < 0) {
                            JS_FreeValue(ctx, val);
                            return ret;
                        }
                        if (ret) {
                            if (desc.flags & JS_PROP_GETSET) {
                                JSObject *setter;
                                if (JS_IsUndefined(desc.setter))
                                    setter = NULL;
                                else
                                    setter = JS_VALUE_GET_OBJ(desc.setter);
                                ret = call_setter(ctx, setter, this_obj, val, flags);
                                JS_FreeValue(ctx, desc.getter);
                                JS_FreeValue(ctx, desc.setter);
                                return ret;
                            } else {
                                JS_FreeValue(ctx, desc.value);
                                if (!(desc.flags & JS_PROP_WRITABLE))
                                    goto read_only_prop;
                                if (likely(p == p1)) {
                                    ret = JS_DefineProperty(ctx, this_obj, prop, val,
                                                            JS_UNDEFINED, JS_UNDEFINED,
                                                            JS_PROP_HAS_VALUE);
                                    JS_FreeValue(ctx, val);
                                    return ret;
                                } else {
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
        p1 = p1->shape->proto;
    prototype_lookup:
        if (!p1)
            break;

    retry2:
        prs = find_own_property(&pr, p1, prop);
        if (prs) {
            if ((prs->flags & JS_PROP_TMASK) == JS_PROP_GETSET) {
                return call_setter(ctx, pr->u.getset.setter, this_obj, val, flags);
            } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_AUTOINIT) {
                /* Instantiate property and retry (potentially useless) */
                if (JS_AutoInitProperty(ctx, p1, prop, pr, prs))
                    return -1;
                goto retry2;
            } else if (!(prs->flags & JS_PROP_WRITABLE)) {
            read_only_prop:
                JS_FreeValue(ctx, val);
                return JS_ThrowTypeErrorReadOnly(ctx, flags, prop);
            }
        }
    }

    if (unlikely(flags & JS_PROP_NO_ADD)) {
        JS_FreeValue(ctx, val);
        JS_ThrowReferenceErrorNotDefined(ctx, prop);
        return -1;
    }

    if (unlikely(!p)) {
        JS_FreeValue(ctx, val);
        return JS_ThrowTypeErrorOrFalse(ctx, flags, "not an object");
    }

    if (unlikely(!p->extensible)) {
        JS_FreeValue(ctx, val);
        return JS_ThrowTypeErrorOrFalse(ctx, flags, "object is not extensible");
    }

    if (p->is_exotic) {
        if (p->class_id == JS_CLASS_ARRAY && p->fast_array &&
            __JS_AtomIsTaggedInt(prop)) {
            uint32_t idx = __JS_AtomToUInt32(prop);
            if (idx == p->u.array.count) {
                /* fast case */
                return add_fast_array_element(ctx, p, val, flags);
            } else {
                goto generic_create_prop;
            }
        } else {
        generic_create_prop:
            ret = JS_CreateProperty(ctx, p, prop, val, JS_UNDEFINED, JS_UNDEFINED,
                                    flags |
                                    JS_PROP_HAS_VALUE |
                                    JS_PROP_HAS_ENUMERABLE |
                                    JS_PROP_HAS_WRITABLE |
                                    JS_PROP_HAS_CONFIGURABLE |
                                    JS_PROP_C_W_E);
            JS_FreeValue(ctx, val);
            return ret;
        }
    }

    pr = add_property(ctx, p, prop, JS_PROP_C_W_E);
    if (unlikely(!pr)) {
        JS_FreeValue(ctx, val);
        return -1;
    }
    pr->u.value = val;
    return TRUE;
}

/* flags can be JS_PROP_THROW or JS_PROP_THROW_STRICT */
static int JS_SetPropertyValue(JSContext *ctx, JSValueConst this_obj,
                               JSValue prop, JSValue val, int flags)
{
    if (likely(JS_VALUE_GET_TAG(this_obj) == JS_TAG_OBJECT &&
               JS_VALUE_GET_TAG(prop) == JS_TAG_INT)) {
        JSObject *p;
        uint32_t idx;
        double d;
        int32_t v;

        /* fast path for array access */
        p = JS_VALUE_GET_OBJ(this_obj);
        idx = JS_VALUE_GET_INT(prop);
        switch(p->class_id) {
        case JS_CLASS_ARRAY:
            if (unlikely(idx >= (uint32_t)p->u.array.count)) {
                JSObject *p1;
                JSShape *sh1;

                /* fast path to add an element to the array */
                if (idx != (uint32_t)p->u.array.count ||
                    !p->fast_array || !p->extensible)
                    goto slow_path;
                /* check if prototype chain has a numeric property */
                p1 = p->shape->proto;
                while (p1 != NULL) {
                    sh1 = p1->shape;
                    if (p1->class_id == JS_CLASS_ARRAY) {
                        if (unlikely(!p1->fast_array))
                            goto slow_path;
                    } else if (p1->class_id == JS_CLASS_OBJECT) {
                        if (unlikely(sh1->has_small_array_index))
                            goto slow_path;
                    } else {
                        goto slow_path;
                    }
                    p1 = sh1->proto;
                }
                /* add element */
                return add_fast_array_element(ctx, p, val, flags);
            }
            set_value(ctx, &p->u.array.u.values[idx], val);
            break;
        case JS_CLASS_ARGUMENTS:
            if (unlikely(idx >= (uint32_t)p->u.array.count))
                goto slow_path;
            set_value(ctx, &p->u.array.u.values[idx], val);
            break;
        case JS_CLASS_UINT8C_ARRAY:
            if (JS_ToUint8ClampFree(ctx, &v, val))
                return -1;
            /* Note: the conversion can detach the typed array, so the
               array bound check must be done after */
            if (unlikely(idx >= (uint32_t)p->u.array.count))
                goto ta_out_of_bound;
            p->u.array.u.uint8_ptr[idx] = v;
            break;
        case JS_CLASS_INT8_ARRAY:
        case JS_CLASS_UINT8_ARRAY:
            if (JS_ToInt32Free(ctx, &v, val))
                return -1;
            if (unlikely(idx >= (uint32_t)p->u.array.count))
                goto ta_out_of_bound;
            p->u.array.u.uint8_ptr[idx] = v;
            break;
        case JS_CLASS_INT16_ARRAY:
        case JS_CLASS_UINT16_ARRAY:
            if (JS_ToInt32Free(ctx, &v, val))
                return -1;
            if (unlikely(idx >= (uint32_t)p->u.array.count))
                goto ta_out_of_bound;
            p->u.array.u.uint16_ptr[idx] = v;
            break;
        case JS_CLASS_INT32_ARRAY:
        case JS_CLASS_UINT32_ARRAY:
            if (JS_ToInt32Free(ctx, &v, val))
                return -1;
            if (unlikely(idx >= (uint32_t)p->u.array.count))
                goto ta_out_of_bound;
            p->u.array.u.uint32_ptr[idx] = v;
            break;
#ifdef CONFIG_BIGNUM
        case JS_CLASS_BIG_INT64_ARRAY:
        case JS_CLASS_BIG_UINT64_ARRAY:
            /* XXX: need specific conversion function */
            {
                int64_t v;
                if (JS_ToBigInt64Free(ctx, &v, val))
                    return -1;
                if (unlikely(idx >= (uint32_t)p->u.array.count))
                    goto ta_out_of_bound;
                p->u.array.u.uint64_ptr[idx] = v;
            }
            break;
#endif
        case JS_CLASS_FLOAT32_ARRAY:
            if (JS_ToFloat64Free(ctx, &d, val))
                return -1;
            if (unlikely(idx >= (uint32_t)p->u.array.count))
                goto ta_out_of_bound;
            p->u.array.u.float_ptr[idx] = d;
            break;
        case JS_CLASS_FLOAT64_ARRAY:
            if (JS_ToFloat64Free(ctx, &d, val))
                return -1;
            if (unlikely(idx >= (uint32_t)p->u.array.count)) {
            ta_out_of_bound:
                return JS_ThrowTypeErrorOrFalse(ctx, flags, "out-of-bound numeric index");
            }
            p->u.array.u.double_ptr[idx] = d;
            break;
        default:
            goto slow_path;
        }
        return TRUE;
    } else {
        JSAtom atom;
        int ret;
    slow_path:
        atom = JS_ValueToAtom(ctx, prop);
        JS_FreeValue(ctx, prop);
        if (unlikely(atom == JS_ATOM_NULL)) {
            JS_FreeValue(ctx, val);
            return -1;
        }
        ret = JS_SetPropertyInternal(ctx, this_obj, atom, val, flags);
        JS_FreeAtom(ctx, atom);
        return ret;
    }
}

int JS_SetPropertyUint32(JSContext *ctx, JSValueConst this_obj,
                         uint32_t idx, JSValue val)
{
    return JS_SetPropertyValue(ctx, this_obj, JS_NewUint32(ctx, idx), val,
                               JS_PROP_THROW);
}

int JS_SetPropertyInt64(JSContext *ctx, JSValueConst this_obj,
                        int64_t idx, JSValue val)
{
    JSAtom prop;
    int res;

    if ((uint64_t)idx <= INT32_MAX) {
        /* fast path for fast arrays */
        return JS_SetPropertyValue(ctx, this_obj, JS_NewInt32(ctx, idx), val,
                                   JS_PROP_THROW);
    }
    prop = JS_NewAtomInt64(ctx, idx);
    if (prop == JS_ATOM_NULL) {
        JS_FreeValue(ctx, val);
        return -1;
    }
    res = JS_SetProperty(ctx, this_obj, prop, val);
    JS_FreeAtom(ctx, prop);
    return res;
}

int JS_SetPropertyStr(JSContext *ctx, JSValueConst this_obj,
                      const char *prop, JSValue val)
{
    JSAtom atom;
    int ret;
    atom = JS_NewAtom(ctx, prop);
    ret = JS_SetPropertyInternal(ctx, this_obj, atom, val, JS_PROP_THROW);
    JS_FreeAtom(ctx, atom);
    return ret;
}

/* compute the property flags. For each flag: (JS_PROP_HAS_x forces
   it, otherwise def_flags is used)
   Note: makes assumption about the bit pattern of the flags
*/
static int get_prop_flags(int flags, int def_flags)
{
    int mask;
    mask = (flags >> JS_PROP_HAS_SHIFT) & JS_PROP_C_W_E;
    return (flags & mask) | (def_flags & ~mask);
}

static int JS_CreateProperty(JSContext *ctx, JSObject *p,
                             JSAtom prop, JSValueConst val,
                             JSValueConst getter, JSValueConst setter,
                             int flags)
{
    JSProperty *pr;
    int ret, prop_flags;

    /* add a new property or modify an existing exotic one */
    if (p->is_exotic) {
        if (p->class_id == JS_CLASS_ARRAY) {
            uint32_t idx, len;

            if (p->fast_array) {
                if (__JS_AtomIsTaggedInt(prop)) {
                    idx = __JS_AtomToUInt32(prop);
                    if (idx == p->u.array.count) {
                        if (!p->extensible)
                            goto not_extensible;
                        if (flags & (JS_PROP_HAS_GET | JS_PROP_HAS_SET))
                            goto convert_to_array;
                        prop_flags = get_prop_flags(flags, 0);
                        if (prop_flags != JS_PROP_C_W_E)
                            goto convert_to_array;
                        return add_fast_array_element(ctx, p,
                                                      JS_DupValue(ctx, val), flags);
                    } else {
                        goto convert_to_array;
                    }
                } else if (JS_AtomIsArrayIndex(ctx, &idx, prop)) {
                    /* convert the fast array to normal array */
                convert_to_array:
                    if (convert_fast_array_to_array(ctx, p))
                        return -1;
                    goto generic_array;
                }
            } else if (JS_AtomIsArrayIndex(ctx, &idx, prop)) {
                JSProperty *plen;
                JSShapeProperty *pslen;
            generic_array:
                /* update the length field */
                plen = &p->prop[0];
                JS_ToUint32(ctx, &len, plen->u.value);
                if ((idx + 1) > len) {
                    pslen = get_shape_prop(p->shape);
                    if (unlikely(!(pslen->flags & JS_PROP_WRITABLE)))
                        return JS_ThrowTypeErrorReadOnly(ctx, flags, JS_ATOM_length);
                    /* XXX: should update the length after defining
                       the property */
                    len = idx + 1;
                    set_value(ctx, &plen->u.value, JS_NewUint32(ctx, len));
                }
            }
        } else if (p->class_id >= JS_CLASS_UINT8C_ARRAY &&
                   p->class_id <= JS_CLASS_FLOAT64_ARRAY) {
            ret = JS_AtomIsNumericIndex(ctx, prop);
            if (ret != 0) {
                if (ret < 0)
                    return -1;
                return JS_ThrowTypeErrorOrFalse(ctx, flags, "cannot create numeric index in typed array");
            }
        } else if (!(flags & JS_PROP_NO_EXOTIC)) {
            const JSClassExoticMethods *em = ctx->rt->class_array[p->class_id].exotic;
            if (em) {
                if (em->define_own_property) {
                    return em->define_own_property(ctx, JS_MKPTR(JS_TAG_OBJECT, p),
                                                   prop, val, getter, setter, flags);
                }
                ret = JS_IsExtensible(ctx, JS_MKPTR(JS_TAG_OBJECT, p));
                if (ret < 0)
                    return -1;
                if (!ret)
                    goto not_extensible;
            }
        }
    }

    if (!p->extensible) {
    not_extensible:
        return JS_ThrowTypeErrorOrFalse(ctx, flags, "object is not extensible");
    }

    if (flags & (JS_PROP_HAS_GET | JS_PROP_HAS_SET)) {
        prop_flags = (flags & (JS_PROP_CONFIGURABLE | JS_PROP_ENUMERABLE)) |
            JS_PROP_GETSET;
    } else {
        prop_flags = flags & JS_PROP_C_W_E;
    }
    pr = add_property(ctx, p, prop, prop_flags);
    if (unlikely(!pr))
        return -1;
    if (flags & (JS_PROP_HAS_GET | JS_PROP_HAS_SET)) {
        pr->u.getset.getter = NULL;
        if ((flags & JS_PROP_HAS_GET) && JS_IsFunction(ctx, getter)) {
            pr->u.getset.getter =
                JS_VALUE_GET_OBJ(JS_DupValue(ctx, getter));
        }
        pr->u.getset.setter = NULL;
        if ((flags & JS_PROP_HAS_SET) && JS_IsFunction(ctx, setter)) {
            pr->u.getset.setter =
                JS_VALUE_GET_OBJ(JS_DupValue(ctx, setter));
        }
    } else {
        if (flags & JS_PROP_HAS_VALUE) {
            pr->u.value = JS_DupValue(ctx, val);
        } else {
            pr->u.value = JS_UNDEFINED;
        }
    }
    return TRUE;
}

/* return FALSE if not OK */
static BOOL check_define_prop_flags(int prop_flags, int flags)
{
    BOOL has_accessor, is_getset;

    if (!(prop_flags & JS_PROP_CONFIGURABLE)) {
        if ((flags & (JS_PROP_HAS_CONFIGURABLE | JS_PROP_CONFIGURABLE)) ==
            (JS_PROP_HAS_CONFIGURABLE | JS_PROP_CONFIGURABLE)) {
            return FALSE;
        }
        if ((flags & JS_PROP_HAS_ENUMERABLE) &&
            (flags & JS_PROP_ENUMERABLE) != (prop_flags & JS_PROP_ENUMERABLE))
            return FALSE;
    }
    if (flags & (JS_PROP_HAS_VALUE | JS_PROP_HAS_WRITABLE |
                 JS_PROP_HAS_GET | JS_PROP_HAS_SET)) {
        if (!(prop_flags & JS_PROP_CONFIGURABLE)) {
            has_accessor = ((flags & (JS_PROP_HAS_GET | JS_PROP_HAS_SET)) != 0);
            is_getset = ((prop_flags & JS_PROP_TMASK) == JS_PROP_GETSET);
            if (has_accessor != is_getset)
                return FALSE;
            if (!has_accessor && !is_getset && !(prop_flags & JS_PROP_WRITABLE)) {
                /* not writable: cannot set the writable bit */
                if ((flags & (JS_PROP_HAS_WRITABLE | JS_PROP_WRITABLE)) ==
                    (JS_PROP_HAS_WRITABLE | JS_PROP_WRITABLE))
                    return FALSE;
            }
        }
    }
    return TRUE;
}

/* ensure that the shape can be safely modified */
static int js_shape_prepare_update(JSContext *ctx, JSObject *p,
                                   JSShapeProperty **pprs)
{
    JSShape *sh;
    uint32_t idx = 0;    /* prevent warning */

    sh = p->shape;
    if (sh->is_hashed) {
        if (sh->header.ref_count != 1) {
            if (pprs)
                idx = *pprs - get_shape_prop(sh);
            /* clone the shape (the resulting one is no longer hashed) */
            sh = js_clone_shape(ctx, sh);
            if (!sh)
                return -1;
            js_free_shape(ctx->rt, p->shape);
            p->shape = sh;
            if (pprs)
                *pprs = get_shape_prop(sh) + idx;
        } else {
            js_shape_hash_unlink(ctx->rt, sh);
            sh->is_hashed = FALSE;
        }
    }
    return 0;
}

static int js_update_property_flags(JSContext *ctx, JSObject *p,
                                    JSShapeProperty **pprs, int flags)
{
    if (flags != (*pprs)->flags) {
        if (js_shape_prepare_update(ctx, p, pprs))
            return -1;
        (*pprs)->flags = flags;
    }
    return 0;
}

/* allowed flags:
   JS_PROP_CONFIGURABLE, JS_PROP_WRITABLE, JS_PROP_ENUMERABLE
   JS_PROP_HAS_GET, JS_PROP_HAS_SET, JS_PROP_HAS_VALUE,
   JS_PROP_HAS_CONFIGURABLE, JS_PROP_HAS_WRITABLE, JS_PROP_HAS_ENUMERABLE,
   JS_PROP_THROW, JS_PROP_NO_EXOTIC.
   If JS_PROP_THROW is set, return an exception instead of FALSE.
   if JS_PROP_NO_EXOTIC is set, do not call the exotic
   define_own_property callback.
   return -1 (exception), FALSE or TRUE.
*/
int JS_DefineProperty(JSContext *ctx, JSValueConst this_obj,
                      JSAtom prop, JSValueConst val,
                      JSValueConst getter, JSValueConst setter, int flags)
{
    JSObject *p;
    JSShapeProperty *prs;
    JSProperty *pr;
    int mask, res;

    if (JS_VALUE_GET_TAG(this_obj) != JS_TAG_OBJECT) {
        JS_ThrowTypeErrorNotAnObject(ctx);
        return -1;
    }
    p = JS_VALUE_GET_OBJ(this_obj);

 redo_prop_update:
    prs = find_own_property(&pr, p, prop);
    if (prs) {
        /* the range of the Array length property is always tested before */
        if ((prs->flags & JS_PROP_LENGTH) && (flags & JS_PROP_HAS_VALUE)) {
            uint32_t array_length;
            if (JS_ToArrayLengthFree(ctx, &array_length,
                                     JS_DupValue(ctx, val), FALSE)) {
                return -1;
            }
            /* this code relies on the fact that Uint32 are never allocated */
            val = (JSValueConst)JS_NewUint32(ctx, array_length);
            /* prs may have been modified */
            prs = find_own_property(&pr, p, prop);
            assert(prs != NULL);
        }
        /* property already exists */
        if (!check_define_prop_flags(prs->flags, flags)) {
        not_configurable:
            return JS_ThrowTypeErrorOrFalse(ctx, flags, "property is not configurable");
        }

        if ((prs->flags & JS_PROP_TMASK) == JS_PROP_AUTOINIT) {
            /* Instantiate property and retry */
            if (JS_AutoInitProperty(ctx, p, prop, pr, prs))
                return -1;
            goto redo_prop_update;
        }

        if (flags & (JS_PROP_HAS_VALUE | JS_PROP_HAS_WRITABLE |
                     JS_PROP_HAS_GET | JS_PROP_HAS_SET)) {
            if (flags & (JS_PROP_HAS_GET | JS_PROP_HAS_SET)) {
                JSObject *new_getter, *new_setter;

                if (JS_IsFunction(ctx, getter)) {
                    new_getter = JS_VALUE_GET_OBJ(getter);
                } else {
                    new_getter = NULL;
                }
                if (JS_IsFunction(ctx, setter)) {
                    new_setter = JS_VALUE_GET_OBJ(setter);
                } else {
                    new_setter = NULL;
                }

                if ((prs->flags & JS_PROP_TMASK) != JS_PROP_GETSET) {
                    if (js_shape_prepare_update(ctx, p, &prs))
                        return -1;
                    /* convert to getset */
                    if ((prs->flags & JS_PROP_TMASK) == JS_PROP_VARREF) {
                        free_var_ref(ctx->rt, pr->u.var_ref);
                    } else {
                        JS_FreeValue(ctx, pr->u.value);
                    }
                    prs->flags = (prs->flags &
                                  (JS_PROP_CONFIGURABLE | JS_PROP_ENUMERABLE)) |
                        JS_PROP_GETSET;
                    pr->u.getset.getter = NULL;
                    pr->u.getset.setter = NULL;
                } else {
                    if (!(prs->flags & JS_PROP_CONFIGURABLE)) {
                        if ((flags & JS_PROP_HAS_GET) &&
                            new_getter != pr->u.getset.getter) {
                            goto not_configurable;
                        }
                        if ((flags & JS_PROP_HAS_SET) &&
                            new_setter != pr->u.getset.setter) {
                            goto not_configurable;
                        }
                    }
                }
                if (flags & JS_PROP_HAS_GET) {
                    if (pr->u.getset.getter)
                        JS_FreeValue(ctx, JS_MKPTR(JS_TAG_OBJECT, pr->u.getset.getter));
                    if (new_getter)
                        JS_DupValue(ctx, getter);
                    pr->u.getset.getter = new_getter;
                }
                if (flags & JS_PROP_HAS_SET) {
                    if (pr->u.getset.setter)
                        JS_FreeValue(ctx, JS_MKPTR(JS_TAG_OBJECT, pr->u.getset.setter));
                    if (new_setter)
                        JS_DupValue(ctx, setter);
                    pr->u.getset.setter = new_setter;
                }
            } else {
                if ((prs->flags & JS_PROP_TMASK) == JS_PROP_GETSET) {
                    /* convert to data descriptor */
                    if (js_shape_prepare_update(ctx, p, &prs))
                        return -1;
                    if (pr->u.getset.getter)
                        JS_FreeValue(ctx, JS_MKPTR(JS_TAG_OBJECT, pr->u.getset.getter));
                    if (pr->u.getset.setter)
                        JS_FreeValue(ctx, JS_MKPTR(JS_TAG_OBJECT, pr->u.getset.setter));
                    prs->flags &= ~(JS_PROP_TMASK | JS_PROP_WRITABLE);
                    pr->u.value = JS_UNDEFINED;
                } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_VARREF) {
                    /* Note: JS_PROP_VARREF is always writable */
                } else {
                    if ((prs->flags & (JS_PROP_CONFIGURABLE | JS_PROP_WRITABLE)) == 0 &&
                        (flags & JS_PROP_HAS_VALUE)) {
                        if (!js_same_value(ctx, val, pr->u.value)) {
                            goto not_configurable;
                        } else {
                            return TRUE;
                        }
                    }
                }
                if ((prs->flags & JS_PROP_TMASK) == JS_PROP_VARREF) {
                    if (flags & JS_PROP_HAS_VALUE) {
                        if (p->class_id == JS_CLASS_MODULE_NS) {
                            /* JS_PROP_WRITABLE is always true for variable
                               references, but they are write protected in module name
                               spaces. */
                            if (!js_same_value(ctx, val, *pr->u.var_ref->pvalue))
                                goto not_configurable;
                        }
                        /* update the reference */
                        set_value(ctx, pr->u.var_ref->pvalue,
                                  JS_DupValue(ctx, val));
                    }
                    /* if writable is set to false, no longer a
                       reference (for mapped arguments) */
                    if ((flags & (JS_PROP_HAS_WRITABLE | JS_PROP_WRITABLE)) == JS_PROP_HAS_WRITABLE) {
                        JSValue val1;
                        if (js_shape_prepare_update(ctx, p, &prs))
                            return -1;
                        val1 = JS_DupValue(ctx, *pr->u.var_ref->pvalue);
                        free_var_ref(ctx->rt, pr->u.var_ref);
                        pr->u.value = val1;
                        prs->flags &= ~(JS_PROP_TMASK | JS_PROP_WRITABLE);
                    }
                } else if (prs->flags & JS_PROP_LENGTH) {
                    if (flags & JS_PROP_HAS_VALUE) {
                        /* Note: no JS code is executable because
                           'val' is guaranted to be a Uint32 */
                        res = set_array_length(ctx, p, JS_DupValue(ctx, val),
                                               flags);
                    } else {
                        res = TRUE;
                    }
                    /* still need to reset the writable flag if
                       needed.  The JS_PROP_LENGTH is kept because the
                       Uint32 test is still done if the length
                       property is read-only. */
                    if ((flags & (JS_PROP_HAS_WRITABLE | JS_PROP_WRITABLE)) ==
                        JS_PROP_HAS_WRITABLE) {
                        prs = get_shape_prop(p->shape);
                        if (js_update_property_flags(ctx, p, &prs,
                                                     prs->flags & ~JS_PROP_WRITABLE))
                            return -1;
                    }
                    return res;
                } else {
                    if (flags & JS_PROP_HAS_VALUE) {
                        JS_FreeValue(ctx, pr->u.value);
                        pr->u.value = JS_DupValue(ctx, val);
                    }
                    if (flags & JS_PROP_HAS_WRITABLE) {
                        if (js_update_property_flags(ctx, p, &prs,
                                                     (prs->flags & ~JS_PROP_WRITABLE) |
                                                     (flags & JS_PROP_WRITABLE)))
                            return -1;
                    }
                }
            }
        }
        mask = 0;
        if (flags & JS_PROP_HAS_CONFIGURABLE)
            mask |= JS_PROP_CONFIGURABLE;
        if (flags & JS_PROP_HAS_ENUMERABLE)
            mask |= JS_PROP_ENUMERABLE;
        if (js_update_property_flags(ctx, p, &prs,
                                     (prs->flags & ~mask) | (flags & mask)))
            return -1;
        return TRUE;
    }

    /* handle modification of fast array elements */
    if (p->fast_array) {
        uint32_t idx;
        uint32_t prop_flags;
        if (p->class_id == JS_CLASS_ARRAY) {
            if (__JS_AtomIsTaggedInt(prop)) {
                idx = __JS_AtomToUInt32(prop);
                if (idx < p->u.array.count) {
                    prop_flags = get_prop_flags(flags, JS_PROP_C_W_E);
                    if (prop_flags != JS_PROP_C_W_E)
                        goto convert_to_slow_array;
                    if (flags & (JS_PROP_HAS_GET | JS_PROP_HAS_SET)) {
                    convert_to_slow_array:
                        if (convert_fast_array_to_array(ctx, p))
                            return -1;
                        else
                            goto redo_prop_update;
                    }
                    if (flags & JS_PROP_HAS_VALUE) {
                        set_value(ctx, &p->u.array.u.values[idx], JS_DupValue(ctx, val));
                    }
                    return TRUE;
                }
            }
        } else if (p->class_id >= JS_CLASS_UINT8C_ARRAY &&
                   p->class_id <= JS_CLASS_FLOAT64_ARRAY) {
            JSValue num;
            int ret;

            if (!__JS_AtomIsTaggedInt(prop)) {
                /* slow path with to handle all numeric indexes */
                num = JS_AtomIsNumericIndex1(ctx, prop);
                if (JS_IsUndefined(num))
                    goto typed_array_done;
                if (JS_IsException(num))
                    return -1;
                ret = JS_NumberIsInteger(ctx, num);
                if (ret < 0) {
                    JS_FreeValue(ctx, num);
                    return -1;
                }
                if (!ret) {
                    JS_FreeValue(ctx, num);
                    return JS_ThrowTypeErrorOrFalse(ctx, flags, "non integer index in typed array");
                }
                ret = JS_NumberIsNegativeOrMinusZero(ctx, num);
                JS_FreeValue(ctx, num);
                if (ret) {
                    return JS_ThrowTypeErrorOrFalse(ctx, flags, "negative index in typed array");
                }
                if (!__JS_AtomIsTaggedInt(prop))
                    goto typed_array_oob;
            }
            idx = __JS_AtomToUInt32(prop);
            /* if the typed array is detached, p->u.array.count = 0 */
            if (idx >= typed_array_get_length(ctx, p)) {
            typed_array_oob:
                return JS_ThrowTypeErrorOrFalse(ctx, flags, "out-of-bound index in typed array");
            }
            prop_flags = get_prop_flags(flags, JS_PROP_ENUMERABLE | JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
            if (flags & (JS_PROP_HAS_GET | JS_PROP_HAS_SET) ||
                prop_flags != (JS_PROP_ENUMERABLE | JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE)) {
                return JS_ThrowTypeErrorOrFalse(ctx, flags, "invalid descriptor flags");
            }
            if (flags & JS_PROP_HAS_VALUE) {
                return JS_SetPropertyValue(ctx, this_obj, JS_NewInt32(ctx, idx), JS_DupValue(ctx, val), flags);
            }
            return TRUE;
        typed_array_done: ;
        }
    }

    return JS_CreateProperty(ctx, p, prop, val, getter, setter, flags);
}

static int JS_DefineAutoInitProperty(JSContext *ctx, JSValueConst this_obj,
                                     JSAtom prop, JSAutoInitIDEnum id,
                                     void *opaque, int flags)
{
    JSObject *p;
    JSProperty *pr;

    if (JS_VALUE_GET_TAG(this_obj) != JS_TAG_OBJECT)
        return FALSE;

    p = JS_VALUE_GET_OBJ(this_obj);

    if (find_own_property(&pr, p, prop)) {
        /* property already exists */
        abort();
        return FALSE;
    }

    /* Specialized CreateProperty */
    pr = add_property(ctx, p, prop, (flags & JS_PROP_C_W_E) | JS_PROP_AUTOINIT);
    if (unlikely(!pr))
        return -1;
    pr->u.init.realm_and_id = (uintptr_t)JS_DupContext(ctx);
    assert((pr->u.init.realm_and_id & 3) == 0);
    assert(id <= 3);
    pr->u.init.realm_and_id |= id;
    pr->u.init.opaque = opaque;
    return TRUE;
}

/* shortcut to add or redefine a new property value */
int JS_DefinePropertyValue(JSContext *ctx, JSValueConst this_obj,
                           JSAtom prop, JSValue val, int flags)
{
    int ret;
    ret = JS_DefineProperty(ctx, this_obj, prop, val, JS_UNDEFINED, JS_UNDEFINED,
                            flags | JS_PROP_HAS_VALUE | JS_PROP_HAS_CONFIGURABLE | JS_PROP_HAS_WRITABLE | JS_PROP_HAS_ENUMERABLE);
    JS_FreeValue(ctx, val);
    return ret;
}

int JS_DefinePropertyValueValue(JSContext *ctx, JSValueConst this_obj,
                                JSValue prop, JSValue val, int flags)
{
    JSAtom atom;
    int ret;
    atom = JS_ValueToAtom(ctx, prop);
    JS_FreeValue(ctx, prop);
    if (unlikely(atom == JS_ATOM_NULL)) {
        JS_FreeValue(ctx, val);
        return -1;
    }
    ret = JS_DefinePropertyValue(ctx, this_obj, atom, val, flags);
    JS_FreeAtom(ctx, atom);
    return ret;
}

int JS_DefinePropertyValueUint32(JSContext *ctx, JSValueConst this_obj,
                                 uint32_t idx, JSValue val, int flags)
{
    return JS_DefinePropertyValueValue(ctx, this_obj, JS_NewUint32(ctx, idx),
                                       val, flags);
}

int JS_DefinePropertyValueInt64(JSContext *ctx, JSValueConst this_obj,
                                int64_t idx, JSValue val, int flags)
{
    return JS_DefinePropertyValueValue(ctx, this_obj, JS_NewInt64(ctx, idx),
                                       val, flags);
}

int JS_DefinePropertyValueStr(JSContext *ctx, JSValueConst this_obj,
                              const char *prop, JSValue val, int flags)
{
    JSAtom atom;
    int ret;
    atom = JS_NewAtom(ctx, prop);
    ret = JS_DefinePropertyValue(ctx, this_obj, atom, val, flags);
    JS_FreeAtom(ctx, atom);
    return ret;
}

/* shortcut to add getter & setter */
int JS_DefinePropertyGetSet(JSContext *ctx, JSValueConst this_obj,
                            JSAtom prop, JSValue getter, JSValue setter,
                            int flags)
{
    int ret;
    ret = JS_DefineProperty(ctx, this_obj, prop, JS_UNDEFINED, getter, setter,
                            flags | JS_PROP_HAS_GET | JS_PROP_HAS_SET |
                            JS_PROP_HAS_CONFIGURABLE | JS_PROP_HAS_ENUMERABLE);
    JS_FreeValue(ctx, getter);
    JS_FreeValue(ctx, setter);
    return ret;
}

static int JS_CreateDataPropertyUint32(JSContext *ctx, JSValueConst this_obj,
                                       int64_t idx, JSValue val, int flags)
{
    return JS_DefinePropertyValueValue(ctx, this_obj, JS_NewInt64(ctx, idx),
                                       val, flags | JS_PROP_CONFIGURABLE |
                                       JS_PROP_ENUMERABLE | JS_PROP_WRITABLE);
}


/* return TRUE if 'obj' has a non empty 'name' string */
static BOOL js_object_has_name(JSContext *ctx, JSValueConst obj)
{
    JSProperty *pr;
    JSShapeProperty *prs;
    JSValueConst val;
    JSString *p;
    
    prs = find_own_property(&pr, JS_VALUE_GET_OBJ(obj), JS_ATOM_name);
    if (!prs)
        return FALSE;
    if ((prs->flags & JS_PROP_TMASK) != JS_PROP_NORMAL)
        return TRUE;
    val = pr->u.value;
    if (JS_VALUE_GET_TAG(val) != JS_TAG_STRING)
        return TRUE;
    p = JS_VALUE_GET_STRING(val);
    return (p->len != 0);
}

static int JS_DefineObjectName(JSContext *ctx, JSValueConst obj,
                               JSAtom name, int flags)
{
    if (name != JS_ATOM_NULL
    &&  JS_IsObject(obj)
    &&  !js_object_has_name(ctx, obj)
    &&  JS_DefinePropertyValue(ctx, obj, JS_ATOM_name, JS_AtomToString(ctx, name), flags) < 0) {
        return -1;
    }
    return 0;
}

static int JS_DefineObjectNameComputed(JSContext *ctx, JSValueConst obj,
                                       JSValueConst str, int flags)
{
    if (JS_IsObject(obj) &&
        !js_object_has_name(ctx, obj)) {
        JSAtom prop;
        JSValue name_str;
        prop = JS_ValueToAtom(ctx, str);
        if (prop == JS_ATOM_NULL)
            return -1;
        name_str = js_get_function_name(ctx, prop);
        JS_FreeAtom(ctx, prop);
        if (JS_IsException(name_str))
            return -1;
        if (JS_DefinePropertyValue(ctx, obj, JS_ATOM_name, name_str, flags) < 0)
            return -1;
    }
    return 0;
}

#define DEFINE_GLOBAL_LEX_VAR (1 << 7)
#define DEFINE_GLOBAL_FUNC_VAR (1 << 6)

static JSValue JS_ThrowSyntaxErrorVarRedeclaration(JSContext *ctx, JSAtom prop)
{
    return JS_ThrowSyntaxErrorAtom(ctx, "redeclaration of '%s'", prop);
}

/* flags is 0, DEFINE_GLOBAL_LEX_VAR or DEFINE_GLOBAL_FUNC_VAR */
/* XXX: could support exotic global object. */
static int JS_CheckDefineGlobalVar(JSContext *ctx, JSAtom prop, int flags)
{
    JSObject *p;
    JSShapeProperty *prs;

    p = JS_VALUE_GET_OBJ(ctx->global_obj);
    prs = find_own_property1(p, prop);
    /* XXX: should handle JS_PROP_AUTOINIT */
    if (flags & DEFINE_GLOBAL_LEX_VAR) {
        if (prs && !(prs->flags & JS_PROP_CONFIGURABLE))
            goto fail_redeclaration;
    } else {
        if (!prs && !p->extensible)
            goto define_error;
        if (flags & DEFINE_GLOBAL_FUNC_VAR) {
            if (prs) {
                if (!(prs->flags & JS_PROP_CONFIGURABLE) &&
                    ((prs->flags & JS_PROP_TMASK) == JS_PROP_GETSET ||
                     ((prs->flags & (JS_PROP_WRITABLE | JS_PROP_ENUMERABLE)) !=
                      (JS_PROP_WRITABLE | JS_PROP_ENUMERABLE)))) {
                define_error:
                    JS_ThrowTypeErrorAtom(ctx, "cannot define variable '%s'",
                                          prop);
                    return -1;
                }
            }
        }
    }
    /* check if there already is a lexical declaration */
    p = JS_VALUE_GET_OBJ(ctx->global_var_obj);
    prs = find_own_property1(p, prop);
    if (prs) {
    fail_redeclaration:
        JS_ThrowSyntaxErrorVarRedeclaration(ctx, prop);
        return -1;
    }
    return 0;
}

/* def_flags is (0, DEFINE_GLOBAL_LEX_VAR) |
   JS_PROP_CONFIGURABLE | JS_PROP_WRITABLE */
/* XXX: could support exotic global object. */
static int JS_DefineGlobalVar(JSContext *ctx, JSAtom prop, int def_flags)
{
    JSObject *p;
    JSShapeProperty *prs;
    JSProperty *pr;
    JSValue val;
    int flags;

    if (def_flags & DEFINE_GLOBAL_LEX_VAR) {
        p = JS_VALUE_GET_OBJ(ctx->global_var_obj);
        flags = JS_PROP_ENUMERABLE | (def_flags & JS_PROP_WRITABLE) |
            JS_PROP_CONFIGURABLE;
        val = JS_UNINITIALIZED;
    } else {
        p = JS_VALUE_GET_OBJ(ctx->global_obj);
        flags = JS_PROP_ENUMERABLE | JS_PROP_WRITABLE |
            (def_flags & JS_PROP_CONFIGURABLE);
        val = JS_UNDEFINED;
    }
    prs = find_own_property1(p, prop);
    if (prs)
        return 0;
    if (!p->extensible)
        return 0;
    pr = add_property(ctx, p, prop, flags);
    if (unlikely(!pr))
        return -1;
    pr->u.value = val;
    return 0;
}

/* 'def_flags' is 0 or JS_PROP_CONFIGURABLE. */
/* XXX: could support exotic global object. */
static int JS_DefineGlobalFunction(JSContext *ctx, JSAtom prop,
                                   JSValueConst func, int def_flags)
{

    JSObject *p;
    JSShapeProperty *prs;
    int flags;

    p = JS_VALUE_GET_OBJ(ctx->global_obj);
    prs = find_own_property1(p, prop);
    flags = JS_PROP_HAS_VALUE | JS_PROP_THROW;
    if (!prs || (prs->flags & JS_PROP_CONFIGURABLE)) {
        flags |= JS_PROP_ENUMERABLE | JS_PROP_WRITABLE | def_flags |
            JS_PROP_HAS_CONFIGURABLE | JS_PROP_HAS_WRITABLE | JS_PROP_HAS_ENUMERABLE;
    }
    if (JS_DefineProperty(ctx, ctx->global_obj, prop, func,
                          JS_UNDEFINED, JS_UNDEFINED, flags) < 0)
        return -1;
    return 0;
}

static JSValue JS_GetGlobalVar(JSContext *ctx, JSAtom prop,
                               BOOL throw_ref_error)
{
    JSObject *p;
    JSShapeProperty *prs;
    JSProperty *pr;

    /* no exotic behavior is possible in global_var_obj */
    p = JS_VALUE_GET_OBJ(ctx->global_var_obj);
    prs = find_own_property(&pr, p, prop);
    if (prs) {
        /* XXX: should handle JS_PROP_TMASK properties */
        if (unlikely(JS_IsUninitialized(pr->u.value)))
            return JS_ThrowReferenceErrorUninitialized(ctx, prs->atom);
        return JS_DupValue(ctx, pr->u.value);
    }
    return JS_GetPropertyInternal(ctx, ctx->global_obj, prop,
                                 ctx->global_obj, throw_ref_error);
}

/* construct a reference to a global variable */
static int JS_GetGlobalVarRef(JSContext *ctx, JSAtom prop, JSValue *sp)
{
    JSObject *p;
    JSShapeProperty *prs;
    JSProperty *pr;

    /* no exotic behavior is possible in global_var_obj */
    p = JS_VALUE_GET_OBJ(ctx->global_var_obj);
    prs = find_own_property(&pr, p, prop);
    if (prs) {
        /* XXX: should handle JS_PROP_AUTOINIT properties? */
        /* XXX: conformance: do these tests in
           OP_put_var_ref/OP_get_var_ref ? */
        if (unlikely(JS_IsUninitialized(pr->u.value))) {
            JS_ThrowReferenceErrorUninitialized(ctx, prs->atom);
            return -1;
        }
        if (unlikely(!(prs->flags & JS_PROP_WRITABLE))) {
            return JS_ThrowTypeErrorReadOnly(ctx, JS_PROP_THROW, prop);
        }
        sp[0] = JS_DupValue(ctx, ctx->global_var_obj);
    } else {
        int ret;
        ret = JS_HasProperty(ctx, ctx->global_obj, prop);
        if (ret < 0)
            return -1;
        if (ret) {
            sp[0] = JS_DupValue(ctx, ctx->global_obj);
        } else {
            sp[0] = JS_UNDEFINED;
        }
    }
    sp[1] = JS_AtomToValue(ctx, prop);
    return 0;
}

/* use for strict variable access: test if the variable exists */
static int JS_CheckGlobalVar(JSContext *ctx, JSAtom prop)
{
    JSObject *p;
    JSShapeProperty *prs;
    int ret;

    /* no exotic behavior is possible in global_var_obj */
    p = JS_VALUE_GET_OBJ(ctx->global_var_obj);
    prs = find_own_property1(p, prop);
    if (prs) {
        ret = TRUE;
    } else {
        ret = JS_HasProperty(ctx, ctx->global_obj, prop);
        if (ret < 0)
            return -1;
    }
    return ret;
}

/* flag = 0: normal variable write
   flag = 1: initialize lexical variable
   flag = 2: normal variable write, strict check was done before
*/
static int JS_SetGlobalVar(JSContext *ctx, JSAtom prop, JSValue val,
                           int flag)
{
    JSObject *p;
    JSShapeProperty *prs;
    JSProperty *pr;
    int flags;

    /* no exotic behavior is possible in global_var_obj */
    p = JS_VALUE_GET_OBJ(ctx->global_var_obj);
    prs = find_own_property(&pr, p, prop);
    if (prs) {
        /* XXX: should handle JS_PROP_AUTOINIT properties? */
        if (flag != 1) {
            if (unlikely(JS_IsUninitialized(pr->u.value))) {
                JS_FreeValue(ctx, val);
                JS_ThrowReferenceErrorUninitialized(ctx, prs->atom);
                return -1;
            }
            if (unlikely(!(prs->flags & JS_PROP_WRITABLE))) {
                JS_FreeValue(ctx, val);
                return JS_ThrowTypeErrorReadOnly(ctx, JS_PROP_THROW, prop);
            }
        }
        set_value(ctx, &pr->u.value, val);
        return 0;
    }
    flags = JS_PROP_THROW_STRICT;
    if (is_strict_mode(ctx)) 
        flags |= JS_PROP_NO_ADD;
    return JS_SetPropertyInternal(ctx, ctx->global_obj, prop, val, flags);
}

/* return -1, FALSE or TRUE. return FALSE if not configurable or
   invalid object. return -1 in case of exception.
   flags can be 0, JS_PROP_THROW or JS_PROP_THROW_STRICT */
int JS_DeleteProperty(JSContext *ctx, JSValueConst obj, JSAtom prop, int flags)
{
    JSValue obj1;
    JSObject *p;
    int res;
    
    obj1 = JS_ToObject(ctx, obj);
    if (JS_IsException(obj1))
        return -1;
    p = JS_VALUE_GET_OBJ(obj1);
    res = delete_property(ctx, p, prop);
    JS_FreeValue(ctx, obj1);
    if (res != FALSE)
        return res;
    if ((flags & JS_PROP_THROW) ||
        ((flags & JS_PROP_THROW_STRICT) && is_strict_mode(ctx))) {
        JS_ThrowTypeError(ctx, "could not delete property");
        return -1;
    }
    return FALSE;
}

int JS_DeletePropertyInt64(JSContext *ctx, JSValueConst obj, int64_t idx, int flags)
{
    JSAtom prop;
    int res;

    if ((uint64_t)idx <= JS_ATOM_MAX_INT) {
        /* fast path for fast arrays */
        return JS_DeleteProperty(ctx, obj, __JS_AtomFromUInt32(idx), flags);
    }
    prop = JS_NewAtomInt64(ctx, idx);
    if (prop == JS_ATOM_NULL)
        return -1;
    res = JS_DeleteProperty(ctx, obj, prop, flags);
    JS_FreeAtom(ctx, prop);
    return res;
}

BOOL JS_IsFunction(JSContext *ctx, JSValueConst val)
{
    JSObject *p;
    if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
        return FALSE;
    p = JS_VALUE_GET_OBJ(val);
    switch(p->class_id) {
    case JS_CLASS_BYTECODE_FUNCTION:
        return TRUE;
    case JS_CLASS_PROXY:
        return p->u.proxy_data->is_func;
    default:
        return (ctx->rt->class_array[p->class_id].call != NULL);
    }
}

BOOL JS_IsCFunction(JSContext *ctx, JSValueConst val, JSCFunction *func, int magic)
{
    JSObject *p;
    if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
        return FALSE;
    p = JS_VALUE_GET_OBJ(val);
    if (p->class_id == JS_CLASS_C_FUNCTION)
        return (p->u.cfunc.c_function.generic == func && p->u.cfunc.magic == magic);
    else
        return FALSE;
}

BOOL JS_IsConstructor(JSContext *ctx, JSValueConst val)
{
    JSObject *p;
    if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
        return FALSE;
    p = JS_VALUE_GET_OBJ(val);
    return p->is_constructor;
}

BOOL JS_SetConstructorBit(JSContext *ctx, JSValueConst func_obj, BOOL val)
{
    JSObject *p;
    if (JS_VALUE_GET_TAG(func_obj) != JS_TAG_OBJECT)
        return FALSE;
    p = JS_VALUE_GET_OBJ(func_obj);
    p->is_constructor = val;
    return TRUE;
}

BOOL JS_IsError(JSContext *ctx, JSValueConst val)
{
    JSObject *p;
    if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
        return FALSE;
    p = JS_VALUE_GET_OBJ(val);
    return (p->class_id == JS_CLASS_ERROR);
}

/* used to avoid catching interrupt exceptions */
BOOL JS_IsUncatchableError(JSContext *ctx, JSValueConst val)
{
    JSObject *p;
    if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
        return FALSE;
    p = JS_VALUE_GET_OBJ(val);
    return p->class_id == JS_CLASS_ERROR && p->is_uncatchable_error;
}

void JS_SetUncatchableError(JSContext *ctx, JSValueConst val, BOOL flag)
{
    JSObject *p;
    if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
        return;
    p = JS_VALUE_GET_OBJ(val);
    if (p->class_id == JS_CLASS_ERROR)
        p->is_uncatchable_error = flag;
}

void JS_ResetUncatchableError(JSContext *ctx)
{
    JS_SetUncatchableError(ctx, ctx->rt->current_exception, FALSE);
}

void JS_SetOpaque(JSValue obj, void *opaque)
{
   JSObject *p;
    if (JS_VALUE_GET_TAG(obj) == JS_TAG_OBJECT) {
        p = JS_VALUE_GET_OBJ(obj);
        p->u.opaque = opaque;
    }
}

/* return NULL if not an object of class class_id */
void *JS_GetOpaque(JSValueConst obj, JSClassID class_id)
{
    JSObject *p;
    if (JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT)
        return NULL;
    p = JS_VALUE_GET_OBJ(obj);
    if (p->class_id != class_id)
        return NULL;
    return p->u.opaque;
}

void *JS_GetOpaque2(JSContext *ctx, JSValueConst obj, JSClassID class_id)
{
    void *p = JS_GetOpaque(obj, class_id);
    if (unlikely(!p)) {
        JS_ThrowTypeErrorInvalidClass(ctx, class_id);
    }
    return p;
}

#define HINT_STRING  0
#define HINT_NUMBER  1
#define HINT_NONE    2
/* don't try Symbol.toPrimitive */
#define HINT_FORCE_ORDINARY (1 << 4)

static JSValue JS_ToPrimitiveFree(JSContext *ctx, JSValue val, int hint)
{
    int i;
    BOOL force_ordinary;

    JSAtom method_name;
    JSValue method, ret;
    if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
        return val;
    force_ordinary = hint & HINT_FORCE_ORDINARY;
    hint &= ~HINT_FORCE_ORDINARY;
    if (!force_ordinary) {
        method = JS_GetProperty(ctx, val, JS_ATOM_Symbol_toPrimitive);
        if (JS_IsException(method))
            goto exception;
        /* ECMA says *If exoticToPrim is not undefined* but tests in
           test262 use null as a non callable converter */
        if (!JS_IsUndefined(method) && !JS_IsNull(method)) {
            JSAtom atom;
            JSValue arg;
            switch(hint) {
            case HINT_STRING:
                atom = JS_ATOM_string;
                break;
            case HINT_NUMBER:
                atom = JS_ATOM_number;
                break;
            default:
            case HINT_NONE:
                atom = JS_ATOM_default;
                break;
            }
            arg = JS_AtomToString(ctx, atom);
            ret = JS_CallFree(ctx, method, val, 1, (JSValueConst *)&arg);
            JS_FreeValue(ctx, arg);
            if (JS_IsException(ret))
                goto exception;
            JS_FreeValue(ctx, val);
            if (JS_VALUE_GET_TAG(ret) != JS_TAG_OBJECT)
                return ret;
            JS_FreeValue(ctx, ret);
            return JS_ThrowTypeError(ctx, "toPrimitive");
        }
    }
    if (hint != HINT_STRING)
        hint = HINT_NUMBER;
    for(i = 0; i < 2; i++) {
        if ((i ^ hint) == 0) {
            method_name = JS_ATOM_toString;
        } else {
            method_name = JS_ATOM_valueOf;
        }
        method = JS_GetProperty(ctx, val, method_name);
        if (JS_IsException(method))
            goto exception;
        if (JS_IsFunction(ctx, method)) {
            ret = JS_CallFree(ctx, method, val, 0, NULL);
            if (JS_IsException(ret))
                goto exception;
            if (JS_VALUE_GET_TAG(ret) != JS_TAG_OBJECT) {
                JS_FreeValue(ctx, val);
                return ret;
            }
            JS_FreeValue(ctx, ret);
        } else {
            JS_FreeValue(ctx, method);
        }
    }
    JS_ThrowTypeError(ctx, "toPrimitive");
exception:
    JS_FreeValue(ctx, val);
    return JS_EXCEPTION;
}

static JSValue JS_ToPrimitive(JSContext *ctx, JSValueConst val, int hint)
{
    return JS_ToPrimitiveFree(ctx, JS_DupValue(ctx, val), hint);
}

void JS_SetIsHTMLDDA(JSContext *ctx, JSValueConst obj)
{
    JSObject *p;
    if (JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT)
        return;
    p = JS_VALUE_GET_OBJ(obj);
    p->is_HTMLDDA = TRUE;
}

static inline BOOL JS_IsHTMLDDA(JSContext *ctx, JSValueConst obj)
{
    JSObject *p;
    if (JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT)
        return FALSE;
    p = JS_VALUE_GET_OBJ(obj);
    return p->is_HTMLDDA;
}
                         
static int JS_ToBoolFree(JSContext *ctx, JSValue val)
{
    uint32_t tag = JS_VALUE_GET_TAG(val);
    switch(tag) {
    case JS_TAG_INT:
        return JS_VALUE_GET_INT(val) != 0;
    case JS_TAG_BOOL:
    case JS_TAG_NULL:
    case JS_TAG_UNDEFINED:
        return JS_VALUE_GET_INT(val);
    case JS_TAG_EXCEPTION:
        return -1;
    case JS_TAG_STRING:
        {
            BOOL ret = JS_VALUE_GET_STRING(val)->len != 0;
            JS_FreeValue(ctx, val);
            return ret;
        }
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_INT:
    case JS_TAG_BIG_FLOAT:
        {
            JSBigFloat *p = JS_VALUE_GET_PTR(val);
            BOOL ret;
            ret = p->num.expn != BF_EXP_ZERO && p->num.expn != BF_EXP_NAN;
            JS_FreeValue(ctx, val);
            return ret;
        }
    case JS_TAG_BIG_DECIMAL:
        {
            JSBigDecimal *p = JS_VALUE_GET_PTR(val);
            BOOL ret;
            ret = p->num.expn != BF_EXP_ZERO && p->num.expn != BF_EXP_NAN;
            JS_FreeValue(ctx, val);
            return ret;
        }
#endif
    case JS_TAG_OBJECT:
        {
            JSObject *p = JS_VALUE_GET_OBJ(val);
            BOOL ret;
            ret = !p->is_HTMLDDA;
            JS_FreeValue(ctx, val);
            return ret;
        }
        break;
    default:
        if (JS_TAG_IS_FLOAT64(tag)) {
            double d = JS_VALUE_GET_FLOAT64(val);
            return !isnan(d) && d != 0;
        } else {
            JS_FreeValue(ctx, val);
            return TRUE;
        }
    }
}

int JS_ToBool(JSContext *ctx, JSValueConst val)
{
    return JS_ToBoolFree(ctx, JS_DupValue(ctx, val));
}

static int skip_spaces(const char *pc)
{
    const uint8_t *p, *p_next, *p_start;
    uint32_t c;

    p = p_start = (const uint8_t *)pc;
    for (;;) {
        c = *p;
        if (c < 128) {
            if (!((c >= 0x09 && c <= 0x0d) || (c == 0x20)))
                break;
            p++;
        } else {
            c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p_next);
            if (!lre_is_space(c))
                break;
            p = p_next;
        }
    }
    return p - p_start;
}

static inline int to_digit(int c)
{
    if (c >= '0' && c <= '9')
        return c - '0';
    else if (c >= 'A' && c <= 'Z')
        return c - 'A' + 10;
    else if (c >= 'a' && c <= 'z')
        return c - 'a' + 10;
    else
        return 36;
}

/* XXX: remove */
static double js_strtod(const char *p, int radix, BOOL is_float)
{
    double d;
    int c;
    
    if (!is_float || radix != 10) {
        uint64_t n_max, n;
        int int_exp, is_neg;
        
        is_neg = 0;
        if (*p == '-') {
            is_neg = 1;
            p++;
        }

        /* skip leading zeros */
        while (*p == '0')
            p++;
        n = 0;
        if (radix == 10)
            n_max = ((uint64_t)-1 - 9) / 10; /* most common case */
        else
            n_max = ((uint64_t)-1 - (radix - 1)) / radix;
        /* XXX: could be more precise */
        int_exp = 0;
        while (*p != '\0') {
            c = to_digit((uint8_t)*p);
            if (c >= radix)
                break;
            if (n <= n_max) {
                n = n * radix + c;
            } else {
                int_exp++;
            }
            p++;
        }
        d = n;
        if (int_exp != 0) {
            d *= pow(radix, int_exp);
        }
        if (is_neg)
            d = -d;
    } else {
        d = strtod(p, NULL);
    }
    return d;
}

#define ATOD_INT_ONLY        (1 << 0)
/* accept Oo and Ob prefixes in addition to 0x prefix if radix = 0 */
#define ATOD_ACCEPT_BIN_OCT  (1 << 2)
/* accept O prefix as octal if radix == 0 and properly formed (Annex B) */
#define ATOD_ACCEPT_LEGACY_OCTAL  (1 << 4)
/* accept _ between digits as a digit separator */
#define ATOD_ACCEPT_UNDERSCORES  (1 << 5)
/* allow a suffix to override the type */
#define ATOD_ACCEPT_SUFFIX    (1 << 6) 
/* default type */
#define ATOD_TYPE_MASK        (3 << 7)
#define ATOD_TYPE_FLOAT64     (0 << 7)
#define ATOD_TYPE_BIG_INT     (1 << 7)
#define ATOD_TYPE_BIG_FLOAT   (2 << 7)
#define ATOD_TYPE_BIG_DECIMAL (3 << 7)
/* assume bigint mode: floats are parsed as integers if no decimal
   point nor exponent */
#define ATOD_MODE_BIGINT      (1 << 9) 
/* accept -0x1 */
#define ATOD_ACCEPT_PREFIX_AFTER_SIGN (1 << 10)

#ifdef CONFIG_BIGNUM
static JSValue js_string_to_bigint(JSContext *ctx, const char *buf,
                                   int radix, int flags, slimb_t *pexponent)
{
    bf_t a_s, *a = &a_s;
    int ret;
    JSValue val;
    val = JS_NewBigInt(ctx);
    if (JS_IsException(val))
        return val;
    a = JS_GetBigInt(val);
    ret = bf_atof(a, buf, NULL, radix, BF_PREC_INF, BF_RNDZ);
    if (ret & BF_ST_MEM_ERROR) {
        JS_FreeValue(ctx, val);
        return JS_ThrowOutOfMemory(ctx);
    }
    val = JS_CompactBigInt1(ctx, val, (flags & ATOD_MODE_BIGINT) != 0);
    return val;
}

static JSValue js_string_to_bigfloat(JSContext *ctx, const char *buf,
                                     int radix, int flags, slimb_t *pexponent)
{
    bf_t *a;
    int ret;
    JSValue val;
    
    val = JS_NewBigFloat(ctx);
    if (JS_IsException(val))
        return val;
    a = JS_GetBigFloat(val);
    if (flags & ATOD_ACCEPT_SUFFIX) {
        /* return the exponent to get infinite precision */
        ret = bf_atof2(a, pexponent, buf, NULL, radix, BF_PREC_INF,
                       BF_RNDZ | BF_ATOF_EXPONENT);
    } else {
        ret = bf_atof(a, buf, NULL, radix, ctx->fp_env.prec,
                      ctx->fp_env.flags);
    }
    if (ret & BF_ST_MEM_ERROR) {
        JS_FreeValue(ctx, val);
        return JS_ThrowOutOfMemory(ctx);
    }
    return val;
}

static JSValue js_string_to_bigdecimal(JSContext *ctx, const char *buf,
                                       int radix, int flags, slimb_t *pexponent)
{
    bfdec_t *a;
    int ret;
    JSValue val;
    
    val = JS_NewBigDecimal(ctx);
    if (JS_IsException(val))
        return val;
    a = JS_GetBigDecimal(val);
    ret = bfdec_atof(a, buf, NULL, BF_PREC_INF,
                     BF_RNDZ | BF_ATOF_NO_NAN_INF);
    if (ret & BF_ST_MEM_ERROR) {
        JS_FreeValue(ctx, val);
        return JS_ThrowOutOfMemory(ctx);
    }
    return val;
}

#endif

/* return an exception in case of memory error. Return JS_NAN if
   invalid syntax */
#ifdef CONFIG_BIGNUM
static JSValue js_atof2(JSContext *ctx, const char *str, const char **pp,
                        int radix, int flags, slimb_t *pexponent)
#else
static JSValue js_atof(JSContext *ctx, const char *str, const char **pp,
                       int radix, int flags)
#endif
{
    const char *p, *p_start;
    int sep, is_neg;
    BOOL is_float, has_legacy_octal;
    int atod_type = flags & ATOD_TYPE_MASK;
    char buf1[64], *buf;
    int i, j, len;
    BOOL buf_allocated = FALSE;
    JSValue val;
    
    /* optional separator between digits */
    sep = (flags & ATOD_ACCEPT_UNDERSCORES) ? '_' : 256;
    has_legacy_octal = FALSE;
    
    p = str;
    p_start = p;
    is_neg = 0;
    if (p[0] == '+') {
        p++;
        p_start++;
        if (!(flags & ATOD_ACCEPT_PREFIX_AFTER_SIGN))
            goto no_radix_prefix;
    } else if (p[0] == '-') {
        p++;
        p_start++;
        is_neg = 1;
        if (!(flags & ATOD_ACCEPT_PREFIX_AFTER_SIGN))
            goto no_radix_prefix;
    }
    if (p[0] == '0') {
        if ((p[1] == 'x' || p[1] == 'X') &&
            (radix == 0 || radix == 16)) {
            p += 2;
            radix = 16;
        } else if ((p[1] == 'o' || p[1] == 'O') &&
                   radix == 0 && (flags & ATOD_ACCEPT_BIN_OCT)) {
            p += 2;
            radix = 8;
        } else if ((p[1] == 'b' || p[1] == 'B') &&
                   radix == 0 && (flags & ATOD_ACCEPT_BIN_OCT)) {
            p += 2;
            radix = 2;
        } else if ((p[1] >= '0' && p[1] <= '9') &&
                   radix == 0 && (flags & ATOD_ACCEPT_LEGACY_OCTAL)) {
            int i;
            has_legacy_octal = TRUE;
            sep = 256;
            for (i = 1; (p[i] >= '0' && p[i] <= '7'); i++)
                continue;
            if (p[i] == '8' || p[i] == '9')
                goto no_prefix;
            p += 1;
            radix = 8;
        } else {
            goto no_prefix;
        }
        /* there must be a digit after the prefix */
        if (to_digit((uint8_t)*p) >= radix)
            goto fail;
    no_prefix: ;
    } else {
 no_radix_prefix:
        if (!(flags & ATOD_INT_ONLY) &&
            (atod_type == ATOD_TYPE_FLOAT64 ||
             atod_type == ATOD_TYPE_BIG_FLOAT) &&
            strstart(p, "Infinity", &p)) {
#ifdef CONFIG_BIGNUM
            if (atod_type == ATOD_TYPE_BIG_FLOAT) {
                bf_t *a;
                val = JS_NewBigFloat(ctx);
                if (JS_IsException(val))
                    goto done;
                a = JS_GetBigFloat(val);
                bf_set_inf(a, is_neg);
            } else
#endif
            {
                double d = 1.0 / 0.0;
                if (is_neg)
                    d = -d;
                val = JS_NewFloat64(ctx, d);
            }
            goto done;
        }
    }
    if (radix == 0)
        radix = 10;
    is_float = FALSE;
    p_start = p;
    while (to_digit((uint8_t)*p) < radix
           ||  (*p == sep && (radix != 10 ||
                              p != p_start + 1 || p[-1] != '0') &&
                to_digit((uint8_t)p[1]) < radix)) {
        p++;
    }
    if (!(flags & ATOD_INT_ONLY)) {
        if (*p == '.' && (p > p_start || to_digit((uint8_t)p[1]) < radix)) {
            is_float = TRUE;
            p++;
            if (*p == sep)
                goto fail;
            while (to_digit((uint8_t)*p) < radix ||
                   (*p == sep && to_digit((uint8_t)p[1]) < radix))
                p++;
        }
        if (p > p_start &&
            (((*p == 'e' || *p == 'E') && radix == 10) ||
             ((*p == 'p' || *p == 'P') && (radix == 2 || radix == 8 || radix == 16)))) {
            const char *p1 = p + 1;
            is_float = TRUE;
            if (*p1 == '+') {
                p1++;
            } else if (*p1 == '-') {
                p1++;
            }
            if (is_digit((uint8_t)*p1)) {
                p = p1 + 1;
                while (is_digit((uint8_t)*p) || (*p == sep && is_digit((uint8_t)p[1])))
                    p++;
            }
        }
    }
    if (p == p_start)
        goto fail;

    buf = buf1;
    buf_allocated = FALSE;
    len = p - p_start;
    if (unlikely((len + 2) > sizeof(buf1))) {
        buf = js_malloc_rt(ctx->rt, len + 2); /* no exception raised */
        if (!buf)
            goto mem_error;
        buf_allocated = TRUE;
    }
    /* remove the separators and the radix prefixes */
    j = 0;
    if (is_neg)
        buf[j++] = '-';
    for (i = 0; i < len; i++) {
        if (p_start[i] != '_')
            buf[j++] = p_start[i];
    }
    buf[j] = '\0';

#ifdef CONFIG_BIGNUM
    if (flags & ATOD_ACCEPT_SUFFIX) {
        if (*p == 'n') {
            p++;
            atod_type = ATOD_TYPE_BIG_INT;
        } else if (*p == 'l') {
            p++;
            atod_type = ATOD_TYPE_BIG_FLOAT;
        } else if (*p == 'm') {
            p++;
            atod_type = ATOD_TYPE_BIG_DECIMAL;
        } else {
            if (flags & ATOD_MODE_BIGINT) {
                if (!is_float)
                    atod_type = ATOD_TYPE_BIG_INT;
                if (has_legacy_octal)
                    goto fail;
            } else {
                if (is_float && radix != 10)
                    goto fail;
            }
        }
    } else {
        if (atod_type == ATOD_TYPE_FLOAT64) {
            if (flags & ATOD_MODE_BIGINT) {
                if (!is_float)
                    atod_type = ATOD_TYPE_BIG_INT;
                if (has_legacy_octal)
                    goto fail;
            } else {
                if (is_float && radix != 10)
                    goto fail;
            }
        }
    }

    switch(atod_type) {
    case ATOD_TYPE_FLOAT64:
        {
            double d;
            d = js_strtod(buf, radix, is_float);
            /* return int or float64 */
            val = JS_NewFloat64(ctx, d);
        }
        break;
    case ATOD_TYPE_BIG_INT:
        if (has_legacy_octal || is_float)
            goto fail;
        val = ctx->rt->bigint_ops.from_string(ctx, buf, radix, flags, NULL);
        break;
    case ATOD_TYPE_BIG_FLOAT:
        if (has_legacy_octal)
            goto fail;
        val = ctx->rt->bigfloat_ops.from_string(ctx, buf, radix, flags,
                                                pexponent);
        break;
    case ATOD_TYPE_BIG_DECIMAL:
        if (radix != 10)
            goto fail;
        val = ctx->rt->bigdecimal_ops.from_string(ctx, buf, radix, flags, NULL);
        break;
    default:
        abort();
    }
#else
    {
        double d;
        (void)has_legacy_octal;
        if (is_float && radix != 10)
            goto fail;
        d = js_strtod(buf, radix, is_float);
        val = JS_NewFloat64(ctx, d);
    }
#endif
    
done:
    if (buf_allocated)
        js_free_rt(ctx->rt, buf);
    if (pp)
        *pp = p;
    return val;
 fail:
    val = JS_NAN;
    goto done;
 mem_error:
    val = JS_ThrowOutOfMemory(ctx);
    goto done;
}

#ifdef CONFIG_BIGNUM
static JSValue js_atof(JSContext *ctx, const char *str, const char **pp,
                       int radix, int flags)
{
    return js_atof2(ctx, str, pp, radix, flags, NULL);
}
#endif

typedef enum JSToNumberHintEnum {
    TON_FLAG_NUMBER,
    TON_FLAG_NUMERIC,
} JSToNumberHintEnum;

static JSValue JS_ToNumberHintFree(JSContext *ctx, JSValue val,
                                   JSToNumberHintEnum flag)
{
    uint32_t tag;
    JSValue ret;

 redo:
    tag = JS_VALUE_GET_NORM_TAG(val);
    switch(tag) {
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_DECIMAL:
        if (flag != TON_FLAG_NUMERIC) {
            JS_FreeValue(ctx, val);
            return JS_ThrowTypeError(ctx, "cannot convert bigdecimal to number");
        }
        ret = val;
        break;
    case JS_TAG_BIG_INT:
        if (flag != TON_FLAG_NUMERIC) {
            JS_FreeValue(ctx, val);
            return JS_ThrowTypeError(ctx, "cannot convert bigint to number");
        }
        ret = val;
        break;
    case JS_TAG_BIG_FLOAT:
        if (flag != TON_FLAG_NUMERIC) {
            JS_FreeValue(ctx, val);
            return JS_ThrowTypeError(ctx, "cannot convert bigfloat to number");
        }
        ret = val;
        break;
#endif
    case JS_TAG_FLOAT64:
    case JS_TAG_INT:
    case JS_TAG_EXCEPTION:
        ret = val;
        break;
    case JS_TAG_BOOL:
    case JS_TAG_NULL:
        ret = JS_NewInt32(ctx, JS_VALUE_GET_INT(val));
        break;
    case JS_TAG_UNDEFINED:
        ret = JS_NAN;
        break;
    case JS_TAG_OBJECT:
        val = JS_ToPrimitiveFree(ctx, val, HINT_NUMBER);
        if (JS_IsException(val))
            return JS_EXCEPTION;
        goto redo;
    case JS_TAG_STRING:
        {
            const char *str;
            const char *p;
            size_t len;
            
            str = JS_ToCStringLen(ctx, &len, val);
            JS_FreeValue(ctx, val);
            if (!str)
                return JS_EXCEPTION;
            p = str;
            p += skip_spaces(p);
            if ((p - str) == len) {
                ret = JS_NewInt32(ctx, 0);
            } else {
                int flags = ATOD_ACCEPT_BIN_OCT;
                ret = js_atof(ctx, p, &p, 0, flags);
                if (!JS_IsException(ret)) {
                    p += skip_spaces(p);
                    if ((p - str) != len) {
                        JS_FreeValue(ctx, ret);
                        ret = JS_NAN;
                    }
                }
            }
            JS_FreeCString(ctx, str);
        }
        break;
    case JS_TAG_SYMBOL:
        JS_FreeValue(ctx, val);
        return JS_ThrowTypeError(ctx, "cannot convert symbol to number");
    default:
        JS_FreeValue(ctx, val);
        ret = JS_NAN;
        break;
    }
    return ret;
}

static JSValue JS_ToNumberFree(JSContext *ctx, JSValue val)
{
    return JS_ToNumberHintFree(ctx, val, TON_FLAG_NUMBER);
}

static JSValue JS_ToNumericFree(JSContext *ctx, JSValue val)
{
    return JS_ToNumberHintFree(ctx, val, TON_FLAG_NUMERIC);
}

static JSValue JS_ToNumeric(JSContext *ctx, JSValueConst val)
{
    return JS_ToNumericFree(ctx, JS_DupValue(ctx, val));
}

static __exception int __JS_ToFloat64Free(JSContext *ctx, double *pres,
                                          JSValue val)
{
    double d;
    uint32_t tag;

    val = JS_ToNumberFree(ctx, val);
    if (JS_IsException(val)) {
        *pres = JS_FLOAT64_NAN;
        return -1;
    }
    tag = JS_VALUE_GET_NORM_TAG(val);
    switch(tag) {
    case JS_TAG_INT:
        d = JS_VALUE_GET_INT(val);
        break;
    case JS_TAG_FLOAT64:
        d = JS_VALUE_GET_FLOAT64(val);
        break;
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_INT:
    case JS_TAG_BIG_FLOAT:
        {
            JSBigFloat *p = JS_VALUE_GET_PTR(val);
            /* XXX: there can be a double rounding issue with some
               primitives (such as JS_ToUint8ClampFree()), but it is
               not critical to fix it. */
            bf_get_float64(&p->num, &d, BF_RNDN);
            JS_FreeValue(ctx, val);
        }
        break;
#endif
    default:
        abort();
    }
    *pres = d;
    return 0;
}

static inline int JS_ToFloat64Free(JSContext *ctx, double *pres, JSValue val)
{
    uint32_t tag;

    tag = JS_VALUE_GET_TAG(val);
    if (tag <= JS_TAG_NULL) {
        *pres = JS_VALUE_GET_INT(val);
        return 0;
    } else if (JS_TAG_IS_FLOAT64(tag)) {
        *pres = JS_VALUE_GET_FLOAT64(val);
        return 0;
    } else {
        return __JS_ToFloat64Free(ctx, pres, val);
    }
}

int JS_ToFloat64(JSContext *ctx, double *pres, JSValueConst val)
{
    return JS_ToFloat64Free(ctx, pres, JS_DupValue(ctx, val));
}

static JSValue JS_ToNumber(JSContext *ctx, JSValueConst val)
{
    return JS_ToNumberFree(ctx, JS_DupValue(ctx, val));
}

/* same as JS_ToNumber() but return 0 in case of NaN/Undefined */
static __maybe_unused JSValue JS_ToIntegerFree(JSContext *ctx, JSValue val)
{
    uint32_t tag;
    JSValue ret;

 redo:
    tag = JS_VALUE_GET_NORM_TAG(val);
    switch(tag) {
    case JS_TAG_INT:
    case JS_TAG_BOOL:
    case JS_TAG_NULL:
    case JS_TAG_UNDEFINED:
        ret = JS_NewInt32(ctx, JS_VALUE_GET_INT(val));
        break;
    case JS_TAG_FLOAT64:
        {
            double d = JS_VALUE_GET_FLOAT64(val);
            if (isnan(d)) {
                ret = JS_NewInt32(ctx, 0);
            } else {
                /* convert -0 to +0 */
                d = trunc(d) + 0.0;
                ret = JS_NewFloat64(ctx, d);
            }
        }
        break;
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_FLOAT:
        {
            bf_t a_s, *a, r_s, *r = &r_s;
            BOOL is_nan;

            a = JS_ToBigFloat(ctx, &a_s, val);
            if (!bf_is_finite(a)) {
                is_nan = bf_is_nan(a);
                if (is_nan)
                    ret = JS_NewInt32(ctx, 0);
                else
                    ret = JS_DupValue(ctx, val);
            } else {
                ret = JS_NewBigInt(ctx);
                if (!JS_IsException(ret)) {
                    r = JS_GetBigInt(ret);
                    bf_set(r, a);
                    bf_rint(r, BF_RNDZ);
                    ret = JS_CompactBigInt(ctx, ret);
                }
            }
            if (a == &a_s)
                bf_delete(a);
            JS_FreeValue(ctx, val);
        }
        break;
#endif
    default:
        val = JS_ToNumberFree(ctx, val);
        if (JS_IsException(val))
            return val;
        goto redo;
    }
    return ret;
}

/* Note: the integer value is satured to 32 bits */
static int JS_ToInt32SatFree(JSContext *ctx, int *pres, JSValue val)
{
    uint32_t tag;
    int ret;

 redo:
    tag = JS_VALUE_GET_NORM_TAG(val);
    switch(tag) {
    case JS_TAG_INT:
    case JS_TAG_BOOL:
    case JS_TAG_NULL:
    case JS_TAG_UNDEFINED:
        ret = JS_VALUE_GET_INT(val);
        break;
    case JS_TAG_EXCEPTION:
        *pres = 0;
        return -1;
    case JS_TAG_FLOAT64:
        {
            double d = JS_VALUE_GET_FLOAT64(val);
            if (isnan(d)) {
                ret = 0;
            } else {
                if (d < INT32_MIN)
                    ret = INT32_MIN;
                else if (d > INT32_MAX)
                    ret = INT32_MAX;
                else
                    ret = (int)d;
            }
        }
        break;
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_FLOAT:
        {
            JSBigFloat *p = JS_VALUE_GET_PTR(val);
            bf_get_int32(&ret, &p->num, 0);
            JS_FreeValue(ctx, val);
        }
        break;
#endif
    default:
        val = JS_ToNumberFree(ctx, val);
        if (JS_IsException(val)) {
            *pres = 0;
            return -1;
        }
        goto redo;
    }
    *pres = ret;
    return 0;
}

int JS_ToInt32Sat(JSContext *ctx, int *pres, JSValueConst val)
{
    return JS_ToInt32SatFree(ctx, pres, JS_DupValue(ctx, val));
}

int JS_ToInt32Clamp(JSContext *ctx, int *pres, JSValueConst val,
                    int min, int max, int min_offset)
{
    int res = JS_ToInt32SatFree(ctx, pres, JS_DupValue(ctx, val));
    if (res == 0) {
        if (*pres < min) {
            *pres += min_offset;
            if (*pres < min)
                *pres = min;
        } else {
            if (*pres > max)
                *pres = max;
        }
    }
    return res;
}

static int JS_ToInt64SatFree(JSContext *ctx, int64_t *pres, JSValue val)
{
    uint32_t tag;

 redo:
    tag = JS_VALUE_GET_NORM_TAG(val);
    switch(tag) {
    case JS_TAG_INT:
    case JS_TAG_BOOL:
    case JS_TAG_NULL:
    case JS_TAG_UNDEFINED:
        *pres = JS_VALUE_GET_INT(val);
        return 0;
    case JS_TAG_EXCEPTION:
        *pres = 0;
        return -1;
    case JS_TAG_FLOAT64:
        {
            double d = JS_VALUE_GET_FLOAT64(val);
            if (isnan(d)) {
                *pres = 0;
            } else {
                if (d < INT64_MIN)
                    *pres = INT64_MIN;
                else if (d > INT64_MAX)
                    *pres = INT64_MAX;
                else
                    *pres = (int64_t)d;
            }
        }
        return 0;
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_FLOAT:
        {
            JSBigFloat *p = JS_VALUE_GET_PTR(val);
            bf_get_int64(pres, &p->num, 0);
            JS_FreeValue(ctx, val);
        }
        return 0;
#endif
    default:
        val = JS_ToNumberFree(ctx, val);
        if (JS_IsException(val)) {
            *pres = 0;
            return -1;
        }
        goto redo;
    }
}

int JS_ToInt64Sat(JSContext *ctx, int64_t *pres, JSValueConst val)
{
    return JS_ToInt64SatFree(ctx, pres, JS_DupValue(ctx, val));
}

int JS_ToInt64Clamp(JSContext *ctx, int64_t *pres, JSValueConst val,
                    int64_t min, int64_t max, int64_t neg_offset)
{
    int res = JS_ToInt64SatFree(ctx, pres, JS_DupValue(ctx, val));
    if (res == 0) {
        if (*pres < 0)
            *pres += neg_offset;
        if (*pres < min)
            *pres = min;
        else if (*pres > max)
            *pres = max;
    }
    return res;
}

/* Same as JS_ToInt32Free() but with a 64 bit result. Return (<0, 0)
   in case of exception */
static int JS_ToInt64Free(JSContext *ctx, int64_t *pres, JSValue val)
{
    uint32_t tag;
    int64_t ret;

 redo:
    tag = JS_VALUE_GET_NORM_TAG(val);
    switch(tag) {
    case JS_TAG_INT:
    case JS_TAG_BOOL:
    case JS_TAG_NULL:
    case JS_TAG_UNDEFINED:
        ret = JS_VALUE_GET_INT(val);
        break;
    case JS_TAG_FLOAT64:
        {
            JSFloat64Union u;
            double d;
            int e;
            d = JS_VALUE_GET_FLOAT64(val);
            u.d = d;
            /* we avoid doing fmod(x, 2^64) */
            e = (u.u64 >> 52) & 0x7ff;
            if (likely(e <= (1023 + 62))) {
                /* fast case */
                ret = (int64_t)d;
            } else if (e <= (1023 + 62 + 53)) {
                uint64_t v;
                /* remainder modulo 2^64 */
                v = (u.u64 & (((uint64_t)1 << 52) - 1)) | ((uint64_t)1 << 52);
                ret = v << ((e - 1023) - 52);
                /* take the sign into account */
                if (u.u64 >> 63)
                    ret = -ret;
            } else {
                ret = 0; /* also handles NaN and +inf */
            }
        }
        break;
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_FLOAT:
        {
            JSBigFloat *p = JS_VALUE_GET_PTR(val);
            bf_get_int64(&ret, &p->num, BF_GET_INT_MOD);
            JS_FreeValue(ctx, val);
        }
        break;
#endif
    default:
        val = JS_ToNumberFree(ctx, val);
        if (JS_IsException(val)) {
            *pres = 0;
            return -1;
        }
        goto redo;
    }
    *pres = ret;
    return 0;
}

int JS_ToInt64(JSContext *ctx, int64_t *pres, JSValueConst val)
{
    return JS_ToInt64Free(ctx, pres, JS_DupValue(ctx, val));
}

int JS_ToInt64Ext(JSContext *ctx, int64_t *pres, JSValueConst val)
{
    if (JS_IsBigInt(ctx, val))
        return JS_ToBigInt64(ctx, pres, val);
    else
        return JS_ToInt64(ctx, pres, val);
}

/* return (<0, 0) in case of exception */
static int JS_ToInt32Free(JSContext *ctx, int32_t *pres, JSValue val)
{
    uint32_t tag;
    int32_t ret;

 redo:
    tag = JS_VALUE_GET_NORM_TAG(val);
    switch(tag) {
    case JS_TAG_INT:
    case JS_TAG_BOOL:
    case JS_TAG_NULL:
    case JS_TAG_UNDEFINED:
        ret = JS_VALUE_GET_INT(val);
        break;
    case JS_TAG_FLOAT64:
        {
            JSFloat64Union u;
            double d;
            int e;
            d = JS_VALUE_GET_FLOAT64(val);
            u.d = d;
            /* we avoid doing fmod(x, 2^32) */
            e = (u.u64 >> 52) & 0x7ff;
            if (likely(e <= (1023 + 30))) {
                /* fast case */
                ret = (int32_t)d;
            } else if (e <= (1023 + 30 + 53)) {
                uint64_t v;
                /* remainder modulo 2^32 */
                v = (u.u64 & (((uint64_t)1 << 52) - 1)) | ((uint64_t)1 << 52);
                v = v << ((e - 1023) - 52 + 32);
                ret = v >> 32;
                /* take the sign into account */
                if (u.u64 >> 63)
                    ret = -ret;
            } else {
                ret = 0; /* also handles NaN and +inf */
            }
        }
        break;
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_FLOAT:
        {
            JSBigFloat *p = JS_VALUE_GET_PTR(val);
            bf_get_int32(&ret, &p->num, BF_GET_INT_MOD);
            JS_FreeValue(ctx, val);
        }
        break;
#endif
    default:
        val = JS_ToNumberFree(ctx, val);
        if (JS_IsException(val)) {
            *pres = 0;
            return -1;
        }
        goto redo;
    }
    *pres = ret;
    return 0;
}

int JS_ToInt32(JSContext *ctx, int32_t *pres, JSValueConst val)
{
    return JS_ToInt32Free(ctx, pres, JS_DupValue(ctx, val));
}

static inline int JS_ToUint32Free(JSContext *ctx, uint32_t *pres, JSValue val)
{
    return JS_ToInt32Free(ctx, (int32_t *)pres, val);
}

static int JS_ToUint8ClampFree(JSContext *ctx, int32_t *pres, JSValue val)
{
    uint32_t tag;
    int res;

 redo:
    tag = JS_VALUE_GET_NORM_TAG(val);
    switch(tag) {
    case JS_TAG_INT:
    case JS_TAG_BOOL:
    case JS_TAG_NULL:
    case JS_TAG_UNDEFINED:
        res = JS_VALUE_GET_INT(val);
#ifdef CONFIG_BIGNUM
    int_clamp:
#endif
        res = max_int(0, min_int(255, res));
        break;
    case JS_TAG_FLOAT64:
        {
            double d = JS_VALUE_GET_FLOAT64(val);
            if (isnan(d)) {
                res = 0;
            } else {
                if (d < 0)
                    res = 0;
                else if (d > 255)
                    res = 255;
                else
                    res = lrint(d);
            }
        }
        break;
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_FLOAT:
        {
            JSBigFloat *p = JS_VALUE_GET_PTR(val);
            bf_t r_s, *r = &r_s;
            bf_init(ctx->bf_ctx, r);
            bf_set(r, &p->num);
            bf_rint(r, BF_RNDN);
            bf_get_int32(&res, r, 0);
            bf_delete(r);
            JS_FreeValue(ctx, val);
        }
        goto int_clamp;
#endif
    default:
        val = JS_ToNumberFree(ctx, val);
        if (JS_IsException(val)) {
            *pres = 0;
            return -1;
        }
        goto redo;
    }
    *pres = res;
    return 0;
}

static __exception int JS_ToArrayLengthFree(JSContext *ctx, uint32_t *plen,
                                            JSValue val, BOOL is_array_ctor)
{
    uint32_t tag, len;

    tag = JS_VALUE_GET_TAG(val);
    switch(tag) {
    case JS_TAG_INT:
    case JS_TAG_BOOL:
    case JS_TAG_NULL:
        {
            int v;
            v = JS_VALUE_GET_INT(val);
            if (v < 0)
                goto fail;
            len = v;
        }
        break;
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_INT:
    case JS_TAG_BIG_FLOAT:
        {
            JSBigFloat *p = JS_VALUE_GET_PTR(val);
            bf_t a;
            BOOL res;
            bf_get_int32((int32_t *)&len, &p->num, BF_GET_INT_MOD);
            bf_init(ctx->bf_ctx, &a);
            bf_set_ui(&a, len);
            res = bf_cmp_eq(&a, &p->num);
            bf_delete(&a);
            JS_FreeValue(ctx, val);
            if (!res)
                goto fail;
        }
        break;
#endif
    default:
        if (JS_TAG_IS_FLOAT64(tag)) {
            double d;
            d = JS_VALUE_GET_FLOAT64(val);
            len = (uint32_t)d;
            if (len != d)
                goto fail;
        } else {
            uint32_t len1;

            if (is_array_ctor) {
                val = JS_ToNumberFree(ctx, val);
                if (JS_IsException(val))
                    return -1;
                /* cannot recurse because val is a number */
                if (JS_ToArrayLengthFree(ctx, &len, val, TRUE))
                    return -1;
            } else {
                /* legacy behavior: must do the conversion twice and compare */
                if (JS_ToUint32(ctx, &len, val)) {
                    JS_FreeValue(ctx, val);
                    return -1;
                }
                val = JS_ToNumberFree(ctx, val);
                if (JS_IsException(val))
                    return -1;
                /* cannot recurse because val is a number */
                if (JS_ToArrayLengthFree(ctx, &len1, val, FALSE))
                    return -1;
                if (len1 != len) {
                fail:
                    JS_ThrowRangeError(ctx, "invalid array length");
                    return -1;
                }
            }
        }
        break;
    }
    *plen = len;
    return 0;
}

#define MAX_SAFE_INTEGER (((int64_t)1 << 53) - 1)

static BOOL is_safe_integer(double d)
{
    return isfinite(d) && floor(d) == d &&
        fabs(d) <= (double)MAX_SAFE_INTEGER;
}

int JS_ToIndex(JSContext *ctx, uint64_t *plen, JSValueConst val)
{
    int64_t v;
    if (JS_ToInt64Sat(ctx, &v, val))
        return -1;
    if (v < 0 || v > MAX_SAFE_INTEGER) {
        JS_ThrowRangeError(ctx, "invalid array index");
        *plen = 0;
        return -1;
    }
    *plen = v;
    return 0;
}

/* convert a value to a length between 0 and MAX_SAFE_INTEGER.
   return -1 for exception */
static __exception int JS_ToLengthFree(JSContext *ctx, int64_t *plen,
                                       JSValue val)
{
    int res = JS_ToInt64Clamp(ctx, plen, val, 0, MAX_SAFE_INTEGER, 0);
    JS_FreeValue(ctx, val);
    return res;
}

/* Note: can return an exception */
static int JS_NumberIsInteger(JSContext *ctx, JSValueConst val)
{
    double d;
    if (!JS_IsNumber(val))
        return FALSE;
    if (unlikely(JS_ToFloat64(ctx, &d, val)))
        return -1;
    return isfinite(d) && floor(d) == d;
}

static BOOL JS_NumberIsNegativeOrMinusZero(JSContext *ctx, JSValueConst val)
{
    uint32_t tag;

    tag = JS_VALUE_GET_NORM_TAG(val);
    switch(tag) {
    case JS_TAG_INT:
        {
            int v;
            v = JS_VALUE_GET_INT(val);
            return (v < 0);
        }
    case JS_TAG_FLOAT64:
        {
            JSFloat64Union u;
            u.d = JS_VALUE_GET_FLOAT64(val);
            return (u.u64 >> 63);
        }
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_INT:
        {
            JSBigFloat *p = JS_VALUE_GET_PTR(val);
            /* Note: integer zeros are not necessarily positive */
            return p->num.sign && !bf_is_zero(&p->num);
        }
    case JS_TAG_BIG_FLOAT:
        {
            JSBigFloat *p = JS_VALUE_GET_PTR(val);
            return p->num.sign;
        }
        break;
    case JS_TAG_BIG_DECIMAL:
        {
            JSBigDecimal *p = JS_VALUE_GET_PTR(val);
            return p->num.sign;
        }
        break;
#endif
    default:
        return FALSE;
    }
}

#ifdef CONFIG_BIGNUM

static JSValue js_bigint_to_string1(JSContext *ctx, JSValueConst val, int radix)
{
    JSValue ret;
    bf_t a_s, *a;
    char *str;
    int saved_sign;

    a = JS_ToBigInt(ctx, &a_s, val);
    if (!a)
        return JS_EXCEPTION;
    saved_sign = a->sign;
    if (a->expn == BF_EXP_ZERO)
        a->sign = 0;
    str = bf_ftoa(NULL, a, radix, 0, BF_RNDZ | BF_FTOA_FORMAT_FRAC |
                  BF_FTOA_JS_QUIRKS);
    a->sign = saved_sign;
    JS_FreeBigInt(ctx, a, &a_s);
    if (!str)
        return JS_ThrowOutOfMemory(ctx);
    ret = JS_NewString(ctx, str);
    bf_free(ctx->bf_ctx, str);
    return ret;
}

static JSValue js_bigint_to_string(JSContext *ctx, JSValueConst val)
{
    return js_bigint_to_string1(ctx, val, 10);
}

static JSValue js_ftoa(JSContext *ctx, JSValueConst val1, int radix,
                       limb_t prec, bf_flags_t flags)
{
    JSValue val, ret;
    bf_t a_s, *a;
    char *str;
    int saved_sign;

    val = JS_ToNumeric(ctx, val1);
    if (JS_IsException(val))
        return val;
    a = JS_ToBigFloat(ctx, &a_s, val);
    saved_sign = a->sign;
    if (a->expn == BF_EXP_ZERO)
        a->sign = 0;
    flags |= BF_FTOA_JS_QUIRKS;
    if ((flags & BF_FTOA_FORMAT_MASK) == BF_FTOA_FORMAT_FREE_MIN) {
        /* Note: for floating point numbers with a radix which is not
           a power of two, the current precision is used to compute
           the number of digits. */
        if ((radix & (radix - 1)) != 0) {
            bf_t r_s, *r = &r_s;
            int prec, flags1;
            /* must round first */
            if (JS_VALUE_GET_TAG(val) == JS_TAG_BIG_FLOAT) {
                prec = ctx->fp_env.prec;
                flags1 = ctx->fp_env.flags &
                    (BF_FLAG_SUBNORMAL | (BF_EXP_BITS_MASK << BF_EXP_BITS_SHIFT));
            } else {
                prec = 53;
                flags1 = bf_set_exp_bits(11) | BF_FLAG_SUBNORMAL;
            }
            bf_init(ctx->bf_ctx, r);
            bf_set(r, a);
            bf_round(r, prec, flags1 | BF_RNDN);
            str = bf_ftoa(NULL, r, radix, prec, flags1 | flags);
            bf_delete(r);
        } else {
            str = bf_ftoa(NULL, a, radix, BF_PREC_INF, flags);
        }
    } else {
        str = bf_ftoa(NULL, a, radix, prec, flags);
    }
    a->sign = saved_sign;
    if (a == &a_s)
        bf_delete(a);
    JS_FreeValue(ctx, val);
    if (!str)
        return JS_ThrowOutOfMemory(ctx);
    ret = JS_NewString(ctx, str);
    bf_free(ctx->bf_ctx, str);
    return ret;
}

static JSValue js_bigfloat_to_string(JSContext *ctx, JSValueConst val)
{
    return js_ftoa(ctx, val, 10, 0, BF_RNDN | BF_FTOA_FORMAT_FREE_MIN);
}

static JSValue js_bigdecimal_to_string1(JSContext *ctx, JSValueConst val,
                                        limb_t prec, int flags)
{
    JSValue ret;
    bfdec_t *a;
    char *str;
    int saved_sign;

    a = JS_ToBigDecimal(ctx, val);
    saved_sign = a->sign;
    if (a->expn == BF_EXP_ZERO)
        a->sign = 0;
    str = bfdec_ftoa(NULL, a, prec, flags | BF_FTOA_JS_QUIRKS);
    a->sign = saved_sign;
    if (!str)
        return JS_ThrowOutOfMemory(ctx);
    ret = JS_NewString(ctx, str);
    bf_free(ctx->bf_ctx, str);
    return ret;
}

static JSValue js_bigdecimal_to_string(JSContext *ctx, JSValueConst val)
{
    return js_bigdecimal_to_string1(ctx, val, 0,
                                    BF_RNDZ | BF_FTOA_FORMAT_FREE);
}

#endif /* CONFIG_BIGNUM */

/* 2 <= base <= 36 */
static char *i64toa(char *buf_end, int64_t n, unsigned int base)
{
    char *q = buf_end;
    int digit, is_neg;

    is_neg = 0;
    if (n < 0) {
        is_neg = 1;
        n = -n;
    }
    *--q = '\0';
    do {
        digit = (uint64_t)n % base;
        n = (uint64_t)n / base;
        if (digit < 10)
            digit += '0';
        else
            digit += 'a' - 10;
        *--q = digit;
    } while (n != 0);
    if (is_neg)
        *--q = '-';
    return q;
}

/* buf1 contains the printf result */
static void js_ecvt1(double d, int n_digits, int *decpt, int *sign, char *buf,
                     int rounding_mode, char *buf1, int buf1_size)
{
    if (rounding_mode != FE_TONEAREST)
        fesetround(rounding_mode);
    snprintf(buf1, buf1_size, "%+.*e", n_digits - 1, d);
    if (rounding_mode != FE_TONEAREST)
        fesetround(FE_TONEAREST);
    *sign = (buf1[0] == '-');
    /* mantissa */
    buf[0] = buf1[1];
    if (n_digits > 1)
        memcpy(buf + 1, buf1 + 3, n_digits - 1);
    buf[n_digits] = '\0';
    /* exponent */
    *decpt = atoi(buf1 + n_digits + 2 + (n_digits > 1)) + 1;
}

/* maximum buffer size for js_dtoa */
#define JS_DTOA_BUF_SIZE 128

/* needed because ecvt usually limits the number of digits to
   17. Return the number of digits. */
static int js_ecvt(double d, int n_digits, int *decpt, int *sign, char *buf,
                   BOOL is_fixed)
{
    int rounding_mode;
    char buf_tmp[JS_DTOA_BUF_SIZE];

    if (!is_fixed) {
        unsigned int n_digits_min, n_digits_max;
        /* find the minimum amount of digits (XXX: inefficient but simple) */
        n_digits_min = 1;
        n_digits_max = 17;
        while (n_digits_min < n_digits_max) {
            n_digits = (n_digits_min + n_digits_max) / 2;
            js_ecvt1(d, n_digits, decpt, sign, buf, FE_TONEAREST,
                     buf_tmp, sizeof(buf_tmp));
            if (strtod(buf_tmp, NULL) == d) {
                /* no need to keep the trailing zeros */
                while (n_digits >= 2 && buf[n_digits - 1] == '0')
                    n_digits--;
                n_digits_max = n_digits;
            } else {
                n_digits_min = n_digits + 1;
            }
        }
        n_digits = n_digits_max;
        rounding_mode = FE_TONEAREST;
    } else {
        rounding_mode = FE_TONEAREST;
#ifdef CONFIG_PRINTF_RNDN
        {
            char buf1[JS_DTOA_BUF_SIZE], buf2[JS_DTOA_BUF_SIZE];
            int decpt1, sign1, decpt2, sign2;
            /* The JS rounding is specified as round to nearest ties away
               from zero (RNDNA), but in printf the "ties" case is not
               specified (for example it is RNDN for glibc, RNDNA for
               Windows), so we must round manually. */
            js_ecvt1(d, n_digits + 1, &decpt1, &sign1, buf1, FE_TONEAREST,
                     buf_tmp, sizeof(buf_tmp));
            /* XXX: could use 2 digits to reduce the average running time */
            if (buf1[n_digits] == '5') {
                js_ecvt1(d, n_digits + 1, &decpt1, &sign1, buf1, FE_DOWNWARD,
                         buf_tmp, sizeof(buf_tmp));
                js_ecvt1(d, n_digits + 1, &decpt2, &sign2, buf2, FE_UPWARD,
                         buf_tmp, sizeof(buf_tmp));
                if (memcmp(buf1, buf2, n_digits + 1) == 0 && decpt1 == decpt2) {
                    /* exact result: round away from zero */
                    if (sign1)
                        rounding_mode = FE_DOWNWARD;
                    else
                        rounding_mode = FE_UPWARD;
                }
            }
        }
#endif /* CONFIG_PRINTF_RNDN */
    }
    js_ecvt1(d, n_digits, decpt, sign, buf, rounding_mode,
             buf_tmp, sizeof(buf_tmp));
    return n_digits;
}

static int js_fcvt1(char *buf, int buf_size, double d, int n_digits,
                    int rounding_mode)
{
    int n;
    if (rounding_mode != FE_TONEAREST)
        fesetround(rounding_mode);
    n = snprintf(buf, buf_size, "%.*f", n_digits, d);
    if (rounding_mode != FE_TONEAREST)
        fesetround(FE_TONEAREST);
    assert(n < buf_size);
    return n;
}

static void js_fcvt(char *buf, int buf_size, double d, int n_digits)
{
    int rounding_mode;
    rounding_mode = FE_TONEAREST;
#ifdef CONFIG_PRINTF_RNDN
    {
        int n1, n2;
        char buf1[JS_DTOA_BUF_SIZE];
        char buf2[JS_DTOA_BUF_SIZE];

        /* The JS rounding is specified as round to nearest ties away from
           zero (RNDNA), but in printf the "ties" case is not specified
           (for example it is RNDN for glibc, RNDNA for Windows), so we
           must round manually. */
        n1 = js_fcvt1(buf1, sizeof(buf1), d, n_digits + 1, FE_TONEAREST);
        rounding_mode = FE_TONEAREST;
        /* XXX: could use 2 digits to reduce the average running time */
        if (buf1[n1 - 1] == '5') {
            n1 = js_fcvt1(buf1, sizeof(buf1), d, n_digits + 1, FE_DOWNWARD);
            n2 = js_fcvt1(buf2, sizeof(buf2), d, n_digits + 1, FE_UPWARD);
            if (n1 == n2 && memcmp(buf1, buf2, n1) == 0) {
                /* exact result: round away from zero */
                if (buf1[0] == '-')
                    rounding_mode = FE_DOWNWARD;
                else
                    rounding_mode = FE_UPWARD;
            }
        }
    }
#endif /* CONFIG_PRINTF_RNDN */
    js_fcvt1(buf, buf_size, d, n_digits, rounding_mode);
}

/* radix != 10 is only supported with flags = JS_DTOA_VAR_FORMAT */
/* use as many digits as necessary */
#define JS_DTOA_VAR_FORMAT   (0 << 0)
/* use n_digits significant digits (1 <= n_digits <= 101) */
#define JS_DTOA_FIXED_FORMAT (1 << 0)
/* force fractional format: [-]dd.dd with n_digits fractional digits */
#define JS_DTOA_FRAC_FORMAT  (2 << 0)
/* force exponential notation either in fixed or variable format */
#define JS_DTOA_FORCE_EXP    (1 << 2)

/* XXX: slow and maybe not fully correct. Use libbf when it is fast enough.
   XXX: radix != 10 is only supported for small integers
*/
static void js_dtoa1(char *buf, double d, int radix, int n_digits, int flags)
{
    char *q;

    if (!isfinite(d)) {
        if (isnan(d)) {
            strcpy(buf, "NaN");
        } else {
            q = buf;
            if (d < 0)
                *q++ = '-';
            strcpy(q, "Infinity");
        }
    } else if (flags == JS_DTOA_VAR_FORMAT) {
        int64_t i64;
        char buf1[70], *ptr;
        i64 = (int64_t)d;
        if (d != i64 || i64 > MAX_SAFE_INTEGER || i64 < -MAX_SAFE_INTEGER)
            goto generic_conv;
        /* fast path for integers */
        ptr = i64toa(buf1 + sizeof(buf1), i64, radix);
        strcpy(buf, ptr);
    } else {
        if (d == 0.0)
            d = 0.0; /* convert -0 to 0 */
        if (flags == JS_DTOA_FRAC_FORMAT) {
            js_fcvt(buf, JS_DTOA_BUF_SIZE, d, n_digits);
        } else {
            char buf1[JS_DTOA_BUF_SIZE];
            int sign, decpt, k, n, i, p, n_max;
            BOOL is_fixed;
        generic_conv:
            is_fixed = ((flags & 3) == JS_DTOA_FIXED_FORMAT);
            if (is_fixed) {
                n_max = n_digits;
            } else {
                n_max = 21;
            }
            /* the number has k digits (k >= 1) */
            k = js_ecvt(d, n_digits, &decpt, &sign, buf1, is_fixed);
            n = decpt; /* d=10^(n-k)*(buf1) i.e. d= < x.yyyy 10^(n-1) */
            q = buf;
            if (sign)
                *q++ = '-';
            if (flags & JS_DTOA_FORCE_EXP)
                goto force_exp;
            if (n >= 1 && n <= n_max) {
                if (k <= n) {
                    memcpy(q, buf1, k);
                    q += k;
                    for(i = 0; i < (n - k); i++)
                        *q++ = '0';
                    *q = '\0';
                } else {
                    /* k > n */
                    memcpy(q, buf1, n);
                    q += n;
                    *q++ = '.';
                    for(i = 0; i < (k - n); i++)
                        *q++ = buf1[n + i];
                    *q = '\0';
                }
            } else if (n >= -5 && n <= 0) {
                *q++ = '0';
                *q++ = '.';
                for(i = 0; i < -n; i++)
                    *q++ = '0';
                memcpy(q, buf1, k);
                q += k;
                *q = '\0';
            } else {
            force_exp:
                /* exponential notation */
                *q++ = buf1[0];
                if (k > 1) {
                    *q++ = '.';
                    for(i = 1; i < k; i++)
                        *q++ = buf1[i];
                }
                *q++ = 'e';
                p = n - 1;
                if (p >= 0)
                    *q++ = '+';
                sprintf(q, "%d", p);
            }
        }
    }
}

static JSValue js_dtoa(JSContext *ctx,
                       double d, int radix, int n_digits, int flags)
{
    char buf[JS_DTOA_BUF_SIZE];
    js_dtoa1(buf, d, radix, n_digits, flags);
    return JS_NewString(ctx, buf);
}

JSValue JS_ToStringInternal(JSContext *ctx, JSValueConst val, BOOL is_ToPropertyKey)
{
    uint32_t tag;
    const char *str;
    char buf[32];

    tag = JS_VALUE_GET_NORM_TAG(val);
    switch(tag) {
    case JS_TAG_STRING:
        return JS_DupValue(ctx, val);
    case JS_TAG_INT:
        snprintf(buf, sizeof(buf), "%d", JS_VALUE_GET_INT(val));
        str = buf;
        goto new_string;
    case JS_TAG_BOOL:
        return JS_AtomToString(ctx, JS_VALUE_GET_BOOL(val) ?
                          JS_ATOM_true : JS_ATOM_false);
    case JS_TAG_NULL:
        return JS_AtomToString(ctx, JS_ATOM_null);
    case JS_TAG_UNDEFINED:
        return JS_AtomToString(ctx, JS_ATOM_undefined);
    case JS_TAG_EXCEPTION:
        return JS_EXCEPTION;
    case JS_TAG_OBJECT:
        {
            JSValue val1, ret;
            val1 = JS_ToPrimitive(ctx, val, HINT_STRING);
            if (JS_IsException(val1))
                return val1;
            ret = JS_ToStringInternal(ctx, val1, is_ToPropertyKey);
            JS_FreeValue(ctx, val1);
            return ret;
        }
        break;
    case JS_TAG_FUNCTION_BYTECODE:
        str = "[function bytecode]";
        goto new_string;
    case JS_TAG_SYMBOL:
        if (is_ToPropertyKey) {
            return JS_DupValue(ctx, val);
        } else {
            return JS_ThrowTypeError(ctx, "cannot convert symbol to string");
        }
    case JS_TAG_FLOAT64:
        return js_dtoa(ctx, JS_VALUE_GET_FLOAT64(val), 10, 0,
                       JS_DTOA_VAR_FORMAT);
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_INT:
        return ctx->rt->bigint_ops.to_string(ctx, val);
    case JS_TAG_BIG_FLOAT:
        return ctx->rt->bigfloat_ops.to_string(ctx, val);
    case JS_TAG_BIG_DECIMAL:
        return ctx->rt->bigdecimal_ops.to_string(ctx, val);
#endif
    default:
        str = "[unsupported type]";
    new_string:
        return JS_NewString(ctx, str);
    }
}

JSValue JS_ToString(JSContext *ctx, JSValueConst val)
{
    return JS_ToStringInternal(ctx, val, FALSE);
}

static JSValue JS_ToStringFree(JSContext *ctx, JSValue val)
{
    JSValue ret;
    ret = JS_ToString(ctx, val);
    JS_FreeValue(ctx, val);
    return ret;
}

static JSValue JS_ToLocaleStringFree(JSContext *ctx, JSValue val)
{
    if (JS_IsUndefined(val) || JS_IsNull(val))
        return JS_ToStringFree(ctx, val);
    return JS_InvokeFree(ctx, val, JS_ATOM_toLocaleString, 0, NULL);
}

JSValue JS_ToPropertyKey(JSContext *ctx, JSValueConst val)
{
    return JS_ToStringInternal(ctx, val, TRUE);
}

static JSValue JS_ToStringCheckObject(JSContext *ctx, JSValueConst val)
{
    uint32_t tag = JS_VALUE_GET_TAG(val);
    if (tag == JS_TAG_NULL || tag == JS_TAG_UNDEFINED)
        return JS_ThrowTypeError(ctx, "null or undefined are forbidden");
    return JS_ToString(ctx, val);
}

static JSValue JS_ToQuotedString(JSContext *ctx, JSValueConst val1)
{
    JSValue val;
    JSString *p;
    int i;
    uint32_t c;
    StringBuffer b_s, *b = &b_s;
    char buf[16];

    val = JS_ToStringCheckObject(ctx, val1);
    if (JS_IsException(val))
        return val;
    p = JS_VALUE_GET_STRING(val);

    if (string_buffer_init(ctx, b, p->len + 2))
        goto fail;

    if (string_buffer_putc8(b, '\"'))
        goto fail;
    for(i = 0; i < p->len; ) {
        c = string_getc(p, &i);
        switch(c) {
        case '\t':
            c = 't';
            goto quote;
        case '\r':
            c = 'r';
            goto quote;
        case '\n':
            c = 'n';
            goto quote;
        case '\b':
            c = 'b';
            goto quote;
        case '\f':
            c = 'f';
            goto quote;
        case '\"':
        case '\\':
        quote:
            if (string_buffer_putc8(b, '\\'))
                goto fail;
            if (string_buffer_putc8(b, c))
                goto fail;
            break;
        default:
            if (c < 32 || (c >= 0xd800 && c < 0xe000)) {
                snprintf(buf, sizeof(buf), "\\u%04x", c);
                if (string_buffer_puts8(b, buf))
                    goto fail;
            } else {
                if (string_buffer_putc(b, c))
                    goto fail;
            }
            break;
        }
    }
    if (string_buffer_putc8(b, '\"'))
        goto fail;
    JS_FreeValue(ctx, val);
    return string_buffer_end(b);
 fail:
    JS_FreeValue(ctx, val);
    string_buffer_free(b);
    return JS_EXCEPTION;
}

static __maybe_unused void JS_DumpObjectHeader(JSRuntime *rt)
{
    printf("%14s %4s %4s %14s %10s %s\n",
           "ADDRESS", "REFS", "SHRF", "PROTO", "CLASS", "PROPS");
}

/* for debug only: dump an object without side effect */
static __maybe_unused void JS_DumpObject(JSRuntime *rt, JSObject *p)
{
    uint32_t i;
    char atom_buf[ATOM_GET_STR_BUF_SIZE];
    JSShape *sh;
    JSShapeProperty *prs;
    JSProperty *pr;
    BOOL is_first = TRUE;

    /* XXX: should encode atoms with special characters */
    sh = p->shape; /* the shape can be NULL while freeing an object */
    printf("%14p %4d ",
           (void *)p,
           p->header.ref_count);
    if (sh) {
        printf("%3d%c %14p ",
               sh->header.ref_count,
               " *"[sh->is_hashed],
               (void *)sh->proto);
    } else {
        printf("%3s  %14s ", "-", "-");
    }
    printf("%10s ",
           JS_AtomGetStrRT(rt, atom_buf, sizeof(atom_buf), rt->class_array[p->class_id].class_name));
    if (p->is_exotic && p->fast_array) {
        printf("[ ");
        for(i = 0; i < p->u.array.count; i++) {
            if (i != 0)
                printf(", ");
            switch (p->class_id) {
            case JS_CLASS_ARRAY:
            case JS_CLASS_ARGUMENTS:
                JS_DumpValueShort(rt, p->u.array.u.values[i]);
                break;
            case JS_CLASS_UINT8C_ARRAY:
            case JS_CLASS_INT8_ARRAY:
            case JS_CLASS_UINT8_ARRAY:
            case JS_CLASS_INT16_ARRAY:
            case JS_CLASS_UINT16_ARRAY:
            case JS_CLASS_INT32_ARRAY:
            case JS_CLASS_UINT32_ARRAY:
#ifdef CONFIG_BIGNUM
            case JS_CLASS_BIG_INT64_ARRAY:
            case JS_CLASS_BIG_UINT64_ARRAY:
#endif
            case JS_CLASS_FLOAT32_ARRAY:
            case JS_CLASS_FLOAT64_ARRAY:
                {
                    int size = 1 << typed_array_size_log2(p->class_id);
                    const uint8_t *b = p->u.array.u.uint8_ptr + i * size;
                    while (size-- > 0)
                        printf("%02X", *b++);
                }
                break;
            }
        }
        printf(" ] ");
    }

    if (sh) {
        printf("{ ");
        for(i = 0, prs = get_shape_prop(sh); i < sh->prop_count; i++, prs++) {
            if (prs->atom != JS_ATOM_NULL) {
                pr = &p->prop[i];
                if (!is_first)
                    printf(", ");
                printf("%s: ",
                       JS_AtomGetStrRT(rt, atom_buf, sizeof(atom_buf), prs->atom));
                if ((prs->flags & JS_PROP_TMASK) == JS_PROP_GETSET) {
                    printf("[getset %p %p]", (void *)pr->u.getset.getter,
                           (void *)pr->u.getset.setter);
                } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_VARREF) {
                    printf("[varref %p]", (void *)pr->u.var_ref);
                } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_AUTOINIT) {
                    printf("[autoinit %p %d %p]",
                           (void *)js_autoinit_get_realm(pr),
                           js_autoinit_get_id(pr),
                           (void *)pr->u.init.opaque);
                } else {
                    JS_DumpValueShort(rt, pr->u.value);
                }
                is_first = FALSE;
            }
        }
        printf(" }");
    }
    
    if (js_class_has_bytecode(p->class_id)) {
        JSFunctionBytecode *b = p->u.func.function_bytecode;
        JSVarRef **var_refs;
        if (b->closure_var_count) {
            var_refs = p->u.func.var_refs;
            printf(" Closure:");
            for(i = 0; i < b->closure_var_count; i++) {
                printf(" ");
                JS_DumpValueShort(rt, var_refs[i]->value);
            }
            if (p->u.func.home_object) {
                printf(" HomeObject: ");
                JS_DumpValueShort(rt, JS_MKPTR(JS_TAG_OBJECT, p->u.func.home_object));
            }
        }
    }
    printf("\n");
}

static __maybe_unused void JS_DumpGCObject(JSRuntime *rt, JSGCObjectHeader *p)
{
    if (p->gc_obj_type == JS_GC_OBJ_TYPE_JS_OBJECT) {
        JS_DumpObject(rt, (JSObject *)p);
    } else {
        printf("%14p %4d ",
               (void *)p,
               p->ref_count);
        switch(p->gc_obj_type) {
        case JS_GC_OBJ_TYPE_FUNCTION_BYTECODE:
            printf("[function bytecode]");
            break;
        case JS_GC_OBJ_TYPE_SHAPE:
            printf("[shape]");
            break;
        case JS_GC_OBJ_TYPE_VAR_REF:
            printf("[var_ref]");
            break;
        case JS_GC_OBJ_TYPE_ASYNC_FUNCTION:
            printf("[async_function]");
            break;
        case JS_GC_OBJ_TYPE_JS_CONTEXT:
            printf("[js_context]");
            break;
        default:
            printf("[unknown %d]", p->gc_obj_type);
            break;
        }
        printf("\n");
    }
}

static __maybe_unused void JS_DumpValueShort(JSRuntime *rt,
                                                      JSValueConst val)
{
    uint32_t tag = JS_VALUE_GET_NORM_TAG(val);
    const char *str;

    switch(tag) {
    case JS_TAG_INT:
        printf("%d", JS_VALUE_GET_INT(val));
        break;
    case JS_TAG_BOOL:
        if (JS_VALUE_GET_BOOL(val))
            str = "true";
        else
            str = "false";
        goto print_str;
    case JS_TAG_NULL:
        str = "null";
        goto print_str;
    case JS_TAG_EXCEPTION:
        str = "exception";
        goto print_str;
    case JS_TAG_UNINITIALIZED:
        str = "uninitialized";
        goto print_str;
    case JS_TAG_UNDEFINED:
        str = "undefined";
    print_str:
        printf("%s", str);
        break;
    case JS_TAG_FLOAT64:
        printf("%.14g", JS_VALUE_GET_FLOAT64(val));
        break;
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_INT:
        {
            JSBigFloat *p = JS_VALUE_GET_PTR(val);
            char *str;
            str = bf_ftoa(NULL, &p->num, 10, 0,
                          BF_RNDZ | BF_FTOA_FORMAT_FRAC);
            printf("%sn", str);
            bf_realloc(&rt->bf_ctx, str, 0);
        }
        break;
    case JS_TAG_BIG_FLOAT:
        {
            JSBigFloat *p = JS_VALUE_GET_PTR(val);
            char *str;
            str = bf_ftoa(NULL, &p->num, 16, BF_PREC_INF,
                          BF_RNDZ | BF_FTOA_FORMAT_FREE | BF_FTOA_ADD_PREFIX);
            printf("%sl", str);
            bf_free(&rt->bf_ctx, str);
        }
        break;
    case JS_TAG_BIG_DECIMAL:
        {
            JSBigDecimal *p = JS_VALUE_GET_PTR(val);
            char *str;
            str = bfdec_ftoa(NULL, &p->num, BF_PREC_INF,
                             BF_RNDZ | BF_FTOA_FORMAT_FREE);
            printf("%sm", str);
            bf_free(&rt->bf_ctx, str);
        }
        break;
#endif
    case JS_TAG_STRING:
        {
            JSString *p;
            p = JS_VALUE_GET_STRING(val);
            JS_DumpString(rt, p);
        }
        break;
    case JS_TAG_FUNCTION_BYTECODE:
        {
            JSFunctionBytecode *b = JS_VALUE_GET_PTR(val);
            char buf[ATOM_GET_STR_BUF_SIZE];
            printf("[bytecode %s]", JS_AtomGetStrRT(rt, buf, sizeof(buf), b->func_name));
        }
        break;
    case JS_TAG_OBJECT:
        {
            JSObject *p = JS_VALUE_GET_OBJ(val);
            JSAtom atom = rt->class_array[p->class_id].class_name;
            char atom_buf[ATOM_GET_STR_BUF_SIZE];
            printf("[%s %p]",
                   JS_AtomGetStrRT(rt, atom_buf, sizeof(atom_buf), atom), (void *)p);
        }
        break;
    case JS_TAG_SYMBOL:
        {
            JSAtomStruct *p = JS_VALUE_GET_PTR(val);
            char atom_buf[ATOM_GET_STR_BUF_SIZE];
            printf("Symbol(%s)",
                   JS_AtomGetStrRT(rt, atom_buf, sizeof(atom_buf), js_get_atom_index(rt, p)));
        }
        break;
    case JS_TAG_MODULE:
        printf("[module]");
        break;
    default:
        printf("[unknown tag %d]", tag);
        break;
    }
}

static __maybe_unused void JS_DumpValue(JSContext *ctx,
                                                 JSValueConst val)
{
    JS_DumpValueShort(ctx->rt, val);
}

static __maybe_unused void JS_PrintValue(JSContext *ctx,
                                                  const char *str,
                                                  JSValueConst val)
{
    printf("%s=", str);
    JS_DumpValueShort(ctx->rt, val);
    printf("\n");
}

/* return -1 if exception (proxy case) or TRUE/FALSE */
int JS_IsArray(JSContext *ctx, JSValueConst val)
{
    JSObject *p;
    if (JS_VALUE_GET_TAG(val) == JS_TAG_OBJECT) {
        p = JS_VALUE_GET_OBJ(val);
        if (unlikely(p->class_id == JS_CLASS_PROXY))
            return js_proxy_isArray(ctx, val);
        else
            return p->class_id == JS_CLASS_ARRAY;
    } else {
        return FALSE;
    }
}

static double js_pow(double a, double b)
{
    if (unlikely(!isfinite(b)) && fabs(a) == 1) {
        /* not compatible with IEEE 754 */
        return JS_FLOAT64_NAN;
    } else {
        return pow(a, b);
    }
}

#ifdef CONFIG_BIGNUM

JSValue JS_NewBigInt64_1(JSContext *ctx, int64_t v)
{
    JSValue val;
    bf_t *a;
    val = JS_NewBigInt(ctx);
    if (JS_IsException(val))
        return val;
    a = JS_GetBigInt(val);
    if (bf_set_si(a, v)) {
        JS_FreeValue(ctx, val);
        return JS_ThrowOutOfMemory(ctx);
    }
    return val;
}

JSValue JS_NewBigInt64(JSContext *ctx, int64_t v)
{
    if (is_math_mode(ctx) &&
        v >= -MAX_SAFE_INTEGER && v <= MAX_SAFE_INTEGER) {
        return JS_NewInt64(ctx, v);
    } else {
        return JS_NewBigInt64_1(ctx, v);
    }
}

JSValue JS_NewBigUint64(JSContext *ctx, uint64_t v)
{
    JSValue val;
    if (is_math_mode(ctx) && v <= MAX_SAFE_INTEGER) {
        val = JS_NewInt64(ctx, v);
    } else {
        bf_t *a;
        val = JS_NewBigInt(ctx);
        if (JS_IsException(val))
            return val;
        a = JS_GetBigInt(val);
        if (bf_set_ui(a, v)) {
            JS_FreeValue(ctx, val);
            return JS_ThrowOutOfMemory(ctx);
        }
    }
    return val;
}

/* if the returned bigfloat is allocated it is equal to
   'buf'. Otherwise it is a pointer to the bigfloat in 'val'. Return
   NULL in case of error. */
static bf_t *JS_ToBigFloat(JSContext *ctx, bf_t *buf, JSValueConst val)
{
    uint32_t tag;
    bf_t *r;
    JSBigFloat *p;

    tag = JS_VALUE_GET_NORM_TAG(val);
    switch(tag) {
    case JS_TAG_INT:
    case JS_TAG_BOOL:
    case JS_TAG_NULL:
        r = buf;
        bf_init(ctx->bf_ctx, r);
        if (bf_set_si(r, JS_VALUE_GET_INT(val)))
            goto fail;
        break;
    case JS_TAG_FLOAT64:
        r = buf;
        bf_init(ctx->bf_ctx, r);
        if (bf_set_float64(r, JS_VALUE_GET_FLOAT64(val))) {
        fail:
            bf_delete(r);
            return NULL;
        }
        break;
    case JS_TAG_BIG_INT:
    case JS_TAG_BIG_FLOAT:
        p = JS_VALUE_GET_PTR(val);
        r = &p->num;
        break;
    case JS_TAG_UNDEFINED:
    default:
        r = buf;
        bf_init(ctx->bf_ctx, r);
        bf_set_nan(r);
        break;
    }
    return r;
}

/* return NULL if invalid type */
static bfdec_t *JS_ToBigDecimal(JSContext *ctx, JSValueConst val)
{
    uint32_t tag;
    JSBigDecimal *p;
    bfdec_t *r;
    
    tag = JS_VALUE_GET_NORM_TAG(val);
    switch(tag) {
    case JS_TAG_BIG_DECIMAL:
        p = JS_VALUE_GET_PTR(val);
        r = &p->num;
        break;
    default:
        JS_ThrowTypeError(ctx, "bigdecimal expected");
        r = NULL;
        break;
    }
    return r;
}

/* return NaN if bad bigint literal */
static JSValue JS_StringToBigInt(JSContext *ctx, JSValue val)
{
    const char *str, *p;
    size_t len;
    int flags;
    
    str = JS_ToCStringLen(ctx, &len, val);
    JS_FreeValue(ctx, val);
    if (!str)
        return JS_EXCEPTION;
    p = str;
    p += skip_spaces(p);
    if ((p - str) == len) {
        val = JS_NewBigInt64(ctx, 0);
    } else {
        flags = ATOD_INT_ONLY | ATOD_ACCEPT_BIN_OCT | ATOD_TYPE_BIG_INT;
        if (is_math_mode(ctx))
            flags |= ATOD_MODE_BIGINT;
        val = js_atof(ctx, p, &p, 0, flags);
        p += skip_spaces(p);
        if (!JS_IsException(val)) {
            if ((p - str) != len) {
                JS_FreeValue(ctx, val);
                val = JS_NAN;
            }
        }
    }
    JS_FreeCString(ctx, str);
    return val;
}

static JSValue JS_StringToBigIntErr(JSContext *ctx, JSValue val)
{
    val = JS_StringToBigInt(ctx, val);
    if (JS_VALUE_IS_NAN(val))
        return JS_ThrowSyntaxError(ctx, "invalid bigint literal");
    return val;
}

/* if the returned bigfloat is allocated it is equal to
   'buf'. Otherwise it is a pointer to the bigfloat in 'val'. */
static bf_t *JS_ToBigIntFree(JSContext *ctx, bf_t *buf, JSValue val)
{
    uint32_t tag;
    bf_t *r;
    JSBigFloat *p;

 redo:
    tag = JS_VALUE_GET_NORM_TAG(val);
    switch(tag) {
    case JS_TAG_INT:
    case JS_TAG_NULL:
    case JS_TAG_UNDEFINED:
        if (!is_math_mode(ctx))
            goto fail;
        /* fall tru */
    case JS_TAG_BOOL:
        r = buf;
        bf_init(ctx->bf_ctx, r);
        bf_set_si(r, JS_VALUE_GET_INT(val));
        break;
    case JS_TAG_FLOAT64:
        {
            double d = JS_VALUE_GET_FLOAT64(val);
            if (!is_math_mode(ctx))
                goto fail;
            if (!isfinite(d))
                goto fail;
            r = buf;
            bf_init(ctx->bf_ctx, r);
            d = trunc(d);
            bf_set_float64(r, d);
        }
        break;
    case JS_TAG_BIG_INT:
        p = JS_VALUE_GET_PTR(val);
        r = &p->num;
        break;
    case JS_TAG_BIG_FLOAT:
        if (!is_math_mode(ctx))
            goto fail;
        p = JS_VALUE_GET_PTR(val);
        if (!bf_is_finite(&p->num))
            goto fail;
        r = buf;
        bf_init(ctx->bf_ctx, r);
        bf_set(r, &p->num);
        bf_rint(r, BF_RNDZ);
        JS_FreeValue(ctx, val);
        break;
    case JS_TAG_STRING:
        val = JS_StringToBigIntErr(ctx, val);
        if (JS_IsException(val))
            return NULL;
        goto redo;
    case JS_TAG_OBJECT:
        val = JS_ToPrimitiveFree(ctx, val, HINT_NUMBER);
        if (JS_IsException(val))
            return NULL;
        goto redo;
    default:
    fail:
        JS_FreeValue(ctx, val);
        JS_ThrowTypeError(ctx, "cannot convert to bigint");
        return NULL;
    }
    return r;
}

static bf_t *JS_ToBigInt(JSContext *ctx, bf_t *buf, JSValueConst val)
{
    return JS_ToBigIntFree(ctx, buf, JS_DupValue(ctx, val));
}

static __maybe_unused JSValue JS_ToBigIntValueFree(JSContext *ctx, JSValue val)
{
    if (JS_VALUE_GET_TAG(val) == JS_TAG_BIG_INT) {
        return val;
    } else {
        bf_t a_s, *a, *r;
        int ret;
        JSValue res; 

        res = JS_NewBigInt(ctx);
        if (JS_IsException(res))
            return JS_EXCEPTION;
        a = JS_ToBigIntFree(ctx, &a_s, val);
        if (!a) {
            JS_FreeValue(ctx, res);
            return JS_EXCEPTION;
        }
        r = JS_GetBigInt(res);
        ret = bf_set(r, a);
        JS_FreeBigInt(ctx, a, &a_s);
        if (ret) {
            JS_FreeValue(ctx, res);
            return JS_ThrowOutOfMemory(ctx);
        }
        return JS_CompactBigInt(ctx, res);
    }
}

/* free the bf_t allocated by JS_ToBigInt */
static void JS_FreeBigInt(JSContext *ctx, bf_t *a, bf_t *buf)
{
    if (a == buf) {
        bf_delete(a);
    } else {
        JSBigFloat *p = (JSBigFloat *)((uint8_t *)a -
                                       offsetof(JSBigFloat, num));
        JS_FreeValue(ctx, JS_MKPTR(JS_TAG_BIG_FLOAT, p));
    }
}

/* XXX: merge with JS_ToInt64Free with a specific flag */
static int JS_ToBigInt64Free(JSContext *ctx, int64_t *pres, JSValue val)
{
    bf_t a_s, *a;

    a = JS_ToBigIntFree(ctx, &a_s, val);
    if (!a) {
        *pres = 0;
        return -1;
    }
    bf_get_int64(pres, a, BF_GET_INT_MOD);
    JS_FreeBigInt(ctx, a, &a_s);
    return 0;
}

int JS_ToBigInt64(JSContext *ctx, int64_t *pres, JSValueConst val)
{
    return JS_ToBigInt64Free(ctx, pres, JS_DupValue(ctx, val));
}

static JSBigFloat *js_new_bf(JSContext *ctx)
{
    JSBigFloat *p;
    p = js_malloc(ctx, sizeof(*p));
    if (!p)
        return NULL;
    p->header.ref_count = 1;
    bf_init(ctx->bf_ctx, &p->num);
    return p;
}

static JSValue JS_NewBigFloat(JSContext *ctx)
{
    JSBigFloat *p;
    p = js_malloc(ctx, sizeof(*p));
    if (!p)
        return JS_EXCEPTION;
    p->header.ref_count = 1;
    bf_init(ctx->bf_ctx, &p->num);
    return JS_MKPTR(JS_TAG_BIG_FLOAT, p);
}

static JSValue JS_NewBigDecimal(JSContext *ctx)
{
    JSBigDecimal *p;
    p = js_malloc(ctx, sizeof(*p));
    if (!p)
        return JS_EXCEPTION;
    p->header.ref_count = 1;
    bfdec_init(ctx->bf_ctx, &p->num);
    return JS_MKPTR(JS_TAG_BIG_DECIMAL, p);
}

static JSValue JS_NewBigInt(JSContext *ctx)
{
    JSBigFloat *p;
    p = js_malloc(ctx, sizeof(*p));
    if (!p)
        return JS_EXCEPTION;
    p->header.ref_count = 1;
    bf_init(ctx->bf_ctx, &p->num);
    return JS_MKPTR(JS_TAG_BIG_INT, p);
}

static JSValue JS_CompactBigInt1(JSContext *ctx, JSValue val,
                                 BOOL convert_to_safe_integer)
{
    int64_t v;
    bf_t *a;
    
    if (JS_VALUE_GET_TAG(val) != JS_TAG_BIG_INT)
        return val; /* fail safe */
    a = JS_GetBigInt(val);
    if (convert_to_safe_integer && bf_get_int64(&v, a, 0) == 0 &&
        v >= -MAX_SAFE_INTEGER && v <= MAX_SAFE_INTEGER) {
        JS_FreeValue(ctx, val);
        return JS_NewInt64(ctx, v);
    } else if (a->expn == BF_EXP_ZERO && a->sign) {
        JSBigFloat *p = JS_VALUE_GET_PTR(val);
        assert(p->header.ref_count == 1);
        a->sign = 0;
    }
    return val;
}

/* Convert the big int to a safe integer if in math mode. normalize
   the zero representation. Could also be used to convert the bigint
   to a short bigint value. The reference count of the value must be
   1. Cannot fail */
static JSValue JS_CompactBigInt(JSContext *ctx, JSValue val)
{
    return JS_CompactBigInt1(ctx, val, is_math_mode(ctx));
}

/* must be kept in sync with JSOverloadableOperatorEnum */
/* XXX: use atoms ? */
static const char js_overloadable_operator_names[JS_OVOP_COUNT][4] = {
    "+",
    "-",
    "*",
    "/",
    "%",
    "**",
    "|",
    "&",
    "^",
    "<<",
    ">>",
    ">>>",
    "==",
    "<",
    "pos",
    "neg",
    "++",
    "--",
    "~",
};

static int get_ovop_from_opcode(OPCodeEnum op)
{
    switch(op) {
    case OP_add:
        return JS_OVOP_ADD;
    case OP_sub:
        return JS_OVOP_SUB;
    case OP_mul:
        return JS_OVOP_MUL;
    case OP_div:
        return JS_OVOP_DIV;
    case OP_mod:
    case OP_math_mod:
        return JS_OVOP_MOD;
    case OP_pow:
        return JS_OVOP_POW;
    case OP_or:
        return JS_OVOP_OR;
    case OP_and:
        return JS_OVOP_AND;
    case OP_xor:
        return JS_OVOP_XOR;
    case OP_shl:
        return JS_OVOP_SHL;
    case OP_sar:
        return JS_OVOP_SAR;
    case OP_shr:
        return JS_OVOP_SHR;
    case OP_eq:
    case OP_neq:
        return JS_OVOP_EQ;
    case OP_lt:
    case OP_lte:
    case OP_gt:
    case OP_gte:
        return JS_OVOP_LESS;
    case OP_plus:
        return JS_OVOP_POS;
    case OP_neg:
        return JS_OVOP_NEG;
    case OP_inc:
        return JS_OVOP_INC;
    case OP_dec:
        return JS_OVOP_DEC;
    default:
        abort();
    }
}

/* return NULL if not present */
static JSObject *find_binary_op(JSBinaryOperatorDef *def,
                                uint32_t operator_index,
                                JSOverloadableOperatorEnum op)
{
    JSBinaryOperatorDefEntry *ent;
    int i;
    for(i = 0; i < def->count; i++) {
        ent = &def->tab[i];
        if (ent->operator_index == operator_index)
            return ent->ops[op];
    }
    return NULL;
}

/* return -1 if exception, 0 if no operator overloading, 1 if
   overloaded operator called */
static __exception int js_call_binary_op_fallback(JSContext *ctx,
                                                  JSValue *pret,
                                                  JSValueConst op1,
                                                  JSValueConst op2,
                                                  OPCodeEnum op,
                                                  BOOL is_numeric,
                                                  int hint)
{
    JSValue opset1_obj, opset2_obj, method, ret, new_op1, new_op2;
    JSOperatorSetData *opset1, *opset2;
    JSOverloadableOperatorEnum ovop;
    JSObject *p;
    JSValueConst args[2];
    
    if (!ctx->allow_operator_overloading)
        return 0;
    
    opset2_obj = JS_UNDEFINED;
    opset1_obj = JS_GetProperty(ctx, op1, JS_ATOM_Symbol_operatorSet);
    if (JS_IsException(opset1_obj))
        goto exception;
    if (JS_IsUndefined(opset1_obj))
        return 0;
    opset1 = JS_GetOpaque2(ctx, opset1_obj, JS_CLASS_OPERATOR_SET);
    if (!opset1)
        goto exception;

    opset2_obj = JS_GetProperty(ctx, op2, JS_ATOM_Symbol_operatorSet);
    if (JS_IsException(opset2_obj))
        goto exception;
    if (JS_IsUndefined(opset2_obj)) {
        JS_FreeValue(ctx, opset1_obj);
        return 0;
    }
    opset2 = JS_GetOpaque2(ctx, opset2_obj, JS_CLASS_OPERATOR_SET);
    if (!opset2)
        goto exception;

    if (opset1->is_primitive && opset2->is_primitive) {
        JS_FreeValue(ctx, opset1_obj);
        JS_FreeValue(ctx, opset2_obj);
        return 0;
    }

    ovop = get_ovop_from_opcode(op);
    
    if (opset1->operator_counter == opset2->operator_counter) {
        p = opset1->self_ops[ovop];
    } else if (opset1->operator_counter > opset2->operator_counter) {
        p = find_binary_op(&opset1->left, opset2->operator_counter, ovop);
    } else {
        p = find_binary_op(&opset2->right, opset1->operator_counter, ovop);
    }
    if (!p) {
        JS_ThrowTypeError(ctx, "operator %s: no function defined",
                          js_overloadable_operator_names[ovop]);
        goto exception;
    }

    if (opset1->is_primitive) {
        if (is_numeric) {
            new_op1 = JS_ToNumeric(ctx, op1);
        } else {
            new_op1 = JS_ToPrimitive(ctx, op1, hint);
        }
        if (JS_IsException(new_op1))
            goto exception;
    } else {
        new_op1 = JS_DupValue(ctx, op1);
    }
    
    if (opset2->is_primitive) {
        if (is_numeric) {
            new_op2 = JS_ToNumeric(ctx, op2);
        } else {
            new_op2 = JS_ToPrimitive(ctx, op2, hint);
        }
        if (JS_IsException(new_op2)) {
            JS_FreeValue(ctx, new_op1);
            goto exception;
        }
    } else {
        new_op2 = JS_DupValue(ctx, op2);
    }

    /* XXX: could apply JS_ToPrimitive() if primitive type so that the
       operator function does not get a value object */
    
    method = JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, p));
    if (ovop == JS_OVOP_LESS && (op == OP_lte || op == OP_gt)) {
        args[0] = new_op2;
        args[1] = new_op1;
    } else {
        args[0] = new_op1;
        args[1] = new_op2;
    }
    ret = JS_CallFree(ctx, method, JS_UNDEFINED, 2, args);
    JS_FreeValue(ctx, new_op1);
    JS_FreeValue(ctx, new_op2);
    if (JS_IsException(ret))
        goto exception;
    if (ovop == JS_OVOP_EQ) {
        BOOL res = JS_ToBoolFree(ctx, ret);
        if (op == OP_neq)
            res ^= 1;
        ret = JS_NewBool(ctx, res);
    } else if (ovop == JS_OVOP_LESS) {
        if (JS_IsUndefined(ret)) {
            ret = JS_FALSE;
        } else {
            BOOL res = JS_ToBoolFree(ctx, ret);
            if (op == OP_lte || op == OP_gte)
                res ^= 1;
            ret = JS_NewBool(ctx, res);
        }
    }
    JS_FreeValue(ctx, opset1_obj);
    JS_FreeValue(ctx, opset2_obj);
    *pret = ret;
    return 1;
 exception:
    JS_FreeValue(ctx, opset1_obj);
    JS_FreeValue(ctx, opset2_obj);
    *pret = JS_UNDEFINED;
    return -1;
}

/* try to call the operation on the operatorSet field of 'obj'. Only
   used for "/" and "**" on the BigInt prototype in math mode */
static __exception int js_call_binary_op_simple(JSContext *ctx,
                                                JSValue *pret,
                                                JSValueConst obj,
                                                JSValueConst op1,
                                                JSValueConst op2,
                                                OPCodeEnum op)
{
    JSValue opset1_obj, method, ret, new_op1, new_op2;
    JSOperatorSetData *opset1;
    JSOverloadableOperatorEnum ovop;
    JSObject *p;
    JSValueConst args[2];

    opset1_obj = JS_GetProperty(ctx, obj, JS_ATOM_Symbol_operatorSet);
    if (JS_IsException(opset1_obj))
        goto exception;
    if (JS_IsUndefined(opset1_obj))
        return 0;
    opset1 = JS_GetOpaque2(ctx, opset1_obj, JS_CLASS_OPERATOR_SET);
    if (!opset1)
        goto exception;
    ovop = get_ovop_from_opcode(op);

    p = opset1->self_ops[ovop];
    if (!p) {
        JS_FreeValue(ctx, opset1_obj);
        return 0;
    }

    new_op1 = JS_ToNumeric(ctx, op1);
    if (JS_IsException(new_op1))
        goto exception;
    new_op2 = JS_ToNumeric(ctx, op2);
    if (JS_IsException(new_op2)) {
        JS_FreeValue(ctx, new_op1);
        goto exception;
    }

    method = JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, p));
    args[0] = new_op1;
    args[1] = new_op2;
    ret = JS_CallFree(ctx, method, JS_UNDEFINED, 2, args);
    JS_FreeValue(ctx, new_op1);
    JS_FreeValue(ctx, new_op2);
    if (JS_IsException(ret))
        goto exception;
    JS_FreeValue(ctx, opset1_obj);
    *pret = ret;
    return 1;
 exception:
    JS_FreeValue(ctx, opset1_obj);
    *pret = JS_UNDEFINED;
    return -1;
}

/* return -1 if exception, 0 if no operator overloading, 1 if
   overloaded operator called */
static __exception int js_call_unary_op_fallback(JSContext *ctx,
                                                 JSValue *pret,
                                                 JSValueConst op1,
                                                 OPCodeEnum op)
{
    JSValue opset1_obj, method, ret;
    JSOperatorSetData *opset1;
    JSOverloadableOperatorEnum ovop;
    JSObject *p;

    if (!ctx->allow_operator_overloading)
        return 0;
    
    opset1_obj = JS_GetProperty(ctx, op1, JS_ATOM_Symbol_operatorSet);
    if (JS_IsException(opset1_obj))
        goto exception;
    if (JS_IsUndefined(opset1_obj))
        return 0;
    opset1 = JS_GetOpaque2(ctx, opset1_obj, JS_CLASS_OPERATOR_SET);
    if (!opset1)
        goto exception;
    if (opset1->is_primitive) {
        JS_FreeValue(ctx, opset1_obj);
        return 0;
    }

    ovop = get_ovop_from_opcode(op);

    p = opset1->self_ops[ovop];
    if (!p) {
        JS_ThrowTypeError(ctx, "no overloaded operator %s",
                          js_overloadable_operator_names[ovop]);
        goto exception;
    }
    method = JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, p));
    ret = JS_CallFree(ctx, method, JS_UNDEFINED, 1, &op1);
    if (JS_IsException(ret))
        goto exception;
    JS_FreeValue(ctx, opset1_obj);
    *pret = ret;
    return 1;
 exception:
    JS_FreeValue(ctx, opset1_obj);
    *pret = JS_UNDEFINED;
    return -1;
}

static JSValue throw_bf_exception(JSContext *ctx, int status)
{
    const char *str;
    if (status & BF_ST_MEM_ERROR)
        return JS_ThrowOutOfMemory(ctx);
    if (status & BF_ST_DIVIDE_ZERO) {
        str = "division by zero";
    } else if (status & BF_ST_INVALID_OP) {
        str = "invalid operation";
    } else {
        str = "integer overflow";
    }
    return JS_ThrowRangeError(ctx, "%s", str);
}

static int js_unary_arith_bigint(JSContext *ctx,
                                 JSValue *pres, OPCodeEnum op, JSValue op1)
{
    bf_t a_s, *r, *a;
    int ret, v;
    JSValue res;
    
    if (op == OP_plus && !is_math_mode(ctx)) {
        JS_ThrowTypeError(ctx, "bigint argument with unary +");
        JS_FreeValue(ctx, op1);
        return -1;
    }
    res = JS_NewBigInt(ctx);
    if (JS_IsException(res)) {
        JS_FreeValue(ctx, op1);
        return -1;
    }
    r = JS_GetBigInt(res);
    a = JS_ToBigInt(ctx, &a_s, op1);
    ret = 0;
    switch(op) {
    case OP_inc:
    case OP_dec:
        v = 2 * (op - OP_dec) - 1;
        ret = bf_add_si(r, a, v, BF_PREC_INF, BF_RNDZ);
        break;
    case OP_plus:
        ret = bf_set(r, a);
        break;
    case OP_neg:
        ret = bf_set(r, a);
        bf_neg(r);
        break;
    case OP_not:
        ret = bf_add_si(r, a, 1, BF_PREC_INF, BF_RNDZ);
        bf_neg(r);
        break;
    default:
        abort();
    }
    JS_FreeBigInt(ctx, a, &a_s);
    JS_FreeValue(ctx, op1);
    if (unlikely(ret)) {
        JS_FreeValue(ctx, res);
        throw_bf_exception(ctx, ret);
        return -1;
    }
    res = JS_CompactBigInt(ctx, res);
    *pres = res;
    return 0;
}

static int js_unary_arith_bigfloat(JSContext *ctx,
                                   JSValue *pres, OPCodeEnum op, JSValue op1)
{
    bf_t a_s, *r, *a;
    int ret, v;
    JSValue res;
    
    if (op == OP_plus && !is_math_mode(ctx)) {
        JS_ThrowTypeError(ctx, "bigfloat argument with unary +");
        JS_FreeValue(ctx, op1);
        return -1;
    }

    res = JS_NewBigFloat(ctx);
    if (JS_IsException(res)) {
        JS_FreeValue(ctx, op1);
        return -1;
    }
    r = JS_GetBigFloat(res);
    a = JS_ToBigFloat(ctx, &a_s, op1);
    ret = 0;
    switch(op) {
    case OP_inc:
    case OP_dec:
        v = 2 * (op - OP_dec) - 1;
        ret = bf_add_si(r, a, v, ctx->fp_env.prec, ctx->fp_env.flags);
        break;
    case OP_plus:
        ret = bf_set(r, a);
        break;
    case OP_neg:
        ret = bf_set(r, a);
        bf_neg(r);
        break;
    default:
        abort();
    }
    if (a == &a_s)
        bf_delete(a);
    JS_FreeValue(ctx, op1);
    if (unlikely(ret & BF_ST_MEM_ERROR)) {
        JS_FreeValue(ctx, res);
        throw_bf_exception(ctx, ret);
        return -1;
    }
    *pres = res;
    return 0;
}

static int js_unary_arith_bigdecimal(JSContext *ctx,
                                     JSValue *pres, OPCodeEnum op, JSValue op1)
{
    bfdec_t *r, *a;
    int ret, v;
    JSValue res;
    
    if (op == OP_plus && !is_math_mode(ctx)) {
        JS_ThrowTypeError(ctx, "bigdecimal argument with unary +");
        JS_FreeValue(ctx, op1);
        return -1;
    }

    res = JS_NewBigDecimal(ctx);
    if (JS_IsException(res)) {
        JS_FreeValue(ctx, op1);
        return -1;
    }
    r = JS_GetBigDecimal(res);
    a = JS_ToBigDecimal(ctx, op1);
    ret = 0;
    switch(op) {
    case OP_inc:
    case OP_dec:
        v = 2 * (op - OP_dec) - 1;
        ret = bfdec_add_si(r, a, v, BF_PREC_INF, BF_RNDZ);
        break;
    case OP_plus:
        ret = bfdec_set(r, a);
        break;
    case OP_neg:
        ret = bfdec_set(r, a);
        bfdec_neg(r);
        break;
    default:
        abort();
    }
    JS_FreeValue(ctx, op1);
    if (unlikely(ret)) {
        JS_FreeValue(ctx, res);
        throw_bf_exception(ctx, ret);
        return -1;
    }
    *pres = res;
    return 0;
}

static no_inline __exception int js_unary_arith_slow(JSContext *ctx,
                                                     JSValue *sp,
                                                     OPCodeEnum op)
{
    JSValue op1, val;
    int v, ret;
    uint32_t tag;

    op1 = sp[-1];
    /* fast path for float64 */
    if (JS_TAG_IS_FLOAT64(JS_VALUE_GET_TAG(op1)))
        goto handle_float64;
    if (JS_IsObject(op1)) {
        ret = js_call_unary_op_fallback(ctx, &val, op1, op);
        if (ret < 0)
            return -1;
        if (ret) {
            JS_FreeValue(ctx, op1);
            sp[-1] = val;
            return 0;
        }
    }

    op1 = JS_ToNumericFree(ctx, op1);
    if (JS_IsException(op1))
        goto exception;
    tag = JS_VALUE_GET_TAG(op1);
    switch(tag) {
    case JS_TAG_INT:
        {
            int64_t v64;
            v64 = JS_VALUE_GET_INT(op1);
            switch(op) {
            case OP_inc:
            case OP_dec:
                v = 2 * (op - OP_dec) - 1;
                v64 += v;
                break;
            case OP_plus:
                break;
            case OP_neg:
                if (v64 == 0) {
                    sp[-1] = __JS_NewFloat64(ctx, -0.0);
                    return 0;
                } else {
                    v64 = -v64;
                }
                break;
            default:
                abort();
            }
            sp[-1] = JS_NewInt64(ctx, v64);
        }
        break;
    case JS_TAG_BIG_INT:
    handle_bigint:
        if (ctx->rt->bigint_ops.unary_arith(ctx, sp - 1, op, op1))
            goto exception;
        break;
    case JS_TAG_BIG_FLOAT:
        if (ctx->rt->bigfloat_ops.unary_arith(ctx, sp - 1, op, op1))
            goto exception;
        break;
    case JS_TAG_BIG_DECIMAL:
        if (ctx->rt->bigdecimal_ops.unary_arith(ctx, sp - 1, op, op1))
            goto exception;
        break;
    default:
    handle_float64:
        {
            double d;
            if (is_math_mode(ctx))
                goto handle_bigint;
            d = JS_VALUE_GET_FLOAT64(op1);
            switch(op) {
            case OP_inc:
            case OP_dec:
                v = 2 * (op - OP_dec) - 1;
                d += v;
                break;
            case OP_plus:
                break;
            case OP_neg:
                d = -d;
                break;
            default:
                abort();
            }
            sp[-1] = __JS_NewFloat64(ctx, d);
        }
        break;
    }
    return 0;
 exception:
    sp[-1] = JS_UNDEFINED;
    return -1;
}

static __exception int js_post_inc_slow(JSContext *ctx,
                                        JSValue *sp, OPCodeEnum op)
{
    JSValue op1;

    /* XXX: allow custom operators */
    op1 = sp[-1];
    op1 = JS_ToNumericFree(ctx, op1);
    if (JS_IsException(op1)) {
        sp[-1] = JS_UNDEFINED;
        return -1;
    }
    sp[-1] = op1;
    sp[0] = JS_DupValue(ctx, op1);
    return js_unary_arith_slow(ctx, sp + 1, op - OP_post_dec + OP_dec);
}

static no_inline int js_not_slow(JSContext *ctx, JSValue *sp)
{
    JSValue op1, val;
    int ret;
    
    op1 = sp[-1];
    if (JS_IsObject(op1)) {
        ret = js_call_unary_op_fallback(ctx, &val, op1, OP_not);
        if (ret < 0)
            return -1;
        if (ret) {
            JS_FreeValue(ctx, op1);
            sp[-1] = val;
            return 0;
        }
    }

    op1 = JS_ToNumericFree(ctx, op1);
    if (JS_IsException(op1))
        goto exception;
    if (is_math_mode(ctx) || JS_VALUE_GET_TAG(op1) == JS_TAG_BIG_INT) {
        if (ctx->rt->bigint_ops.unary_arith(ctx, sp - 1, OP_not, op1))
            goto exception;
    } else {
        int32_t v1;
        if (unlikely(JS_ToInt32Free(ctx, &v1, op1)))
            goto exception;
        sp[-1] = JS_NewInt32(ctx, ~v1);
    }
    return 0;
 exception:
    sp[-1] = JS_UNDEFINED;
    return -1;
}

static int js_binary_arith_bigfloat(JSContext *ctx, OPCodeEnum op,
                                    JSValue *pres, JSValue op1, JSValue op2)
{
    bf_t a_s, b_s, *r, *a, *b;
    int ret;
    JSValue res;
    
    res = JS_NewBigFloat(ctx);
    if (JS_IsException(res)) {
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
        return -1;
    }
    r = JS_GetBigFloat(res);
    a = JS_ToBigFloat(ctx, &a_s, op1);
    b = JS_ToBigFloat(ctx, &b_s, op2);
    bf_init(ctx->bf_ctx, r);
    switch(op) {
    case OP_add:
        ret = bf_add(r, a, b, ctx->fp_env.prec, ctx->fp_env.flags);
        break;
    case OP_sub:
        ret = bf_sub(r, a, b, ctx->fp_env.prec, ctx->fp_env.flags);
        break;
    case OP_mul:
        ret = bf_mul(r, a, b, ctx->fp_env.prec, ctx->fp_env.flags);
        break;
    case OP_div:
        ret = bf_div(r, a, b, ctx->fp_env.prec, ctx->fp_env.flags);
        break;
    case OP_math_mod:
        /* Euclidian remainder */
        ret = bf_rem(r, a, b, ctx->fp_env.prec, ctx->fp_env.flags,
                     BF_DIVREM_EUCLIDIAN);
        break;
    case OP_mod:
        ret = bf_rem(r, a, b, ctx->fp_env.prec, ctx->fp_env.flags,
                     BF_RNDZ);
        break;
    case OP_pow:
        ret = bf_pow(r, a, b, ctx->fp_env.prec,
                     ctx->fp_env.flags | BF_POW_JS_QUIRKS);
        break;
    default:
        abort();
    }
    if (a == &a_s)
        bf_delete(a);
    if (b == &b_s)
        bf_delete(b);
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    if (unlikely(ret & BF_ST_MEM_ERROR)) {
        JS_FreeValue(ctx, res);
        throw_bf_exception(ctx, ret);
        return -1;
    }
    *pres = res;
    return 0;
}

static int js_binary_arith_bigint(JSContext *ctx, OPCodeEnum op,
                                  JSValue *pres, JSValue op1, JSValue op2)
{
    bf_t a_s, b_s, *r, *a, *b;
    int ret;
    JSValue res;
    
    res = JS_NewBigInt(ctx);
    if (JS_IsException(res))
        goto fail;
    a = JS_ToBigInt(ctx, &a_s, op1);
    if (!a)
        goto fail;
    b = JS_ToBigInt(ctx, &b_s, op2);
    if (!b) {
        JS_FreeBigInt(ctx, a, &a_s);
        goto fail;
    }
    r = JS_GetBigInt(res);
    ret = 0;
    switch(op) {
    case OP_add:
        ret = bf_add(r, a, b, BF_PREC_INF, BF_RNDZ);
        break;
    case OP_sub:
        ret = bf_sub(r, a, b, BF_PREC_INF, BF_RNDZ);
        break;
    case OP_mul:
        ret = bf_mul(r, a, b, BF_PREC_INF, BF_RNDZ);
        break;
    case OP_div:
        if (!is_math_mode(ctx)) {
            bf_t rem_s, *rem = &rem_s;
            bf_init(ctx->bf_ctx, rem);
            ret = bf_divrem(r, rem, a, b, BF_PREC_INF, BF_RNDZ,
                            BF_RNDZ);
            bf_delete(rem);
        } else {
            goto math_mode_div_pow;
        }
        break;
    case OP_math_mod:
        /* Euclidian remainder */
        ret = bf_rem(r, a, b, BF_PREC_INF, BF_RNDZ,
                     BF_DIVREM_EUCLIDIAN) & BF_ST_INVALID_OP;
        break;
    case OP_mod:
        ret = bf_rem(r, a, b, BF_PREC_INF, BF_RNDZ,
                     BF_RNDZ) & BF_ST_INVALID_OP;
        break;
    case OP_pow:
        if (b->sign) {
            if (!is_math_mode(ctx)) {
                ret = BF_ST_INVALID_OP;
            } else {
            math_mode_div_pow:
                JS_FreeValue(ctx, res);
                ret = js_call_binary_op_simple(ctx, &res, ctx->class_proto[JS_CLASS_BIG_INT], op1, op2, op);
                if (ret != 0) {
                    JS_FreeBigInt(ctx, a, &a_s);
                    JS_FreeBigInt(ctx, b, &b_s);
                    JS_FreeValue(ctx, op1);
                    JS_FreeValue(ctx, op2);
                    if (ret < 0) {
                        return -1;
                    } else {
                        *pres = res;
                        return 0;
                    }
                }
                /* if no BigInt power operator defined, return a
                   bigfloat */
                res = JS_NewBigFloat(ctx);
                if (JS_IsException(res)) {
                    JS_FreeBigInt(ctx, a, &a_s);
                    JS_FreeBigInt(ctx, b, &b_s);
                    goto fail;
                }
                r = JS_GetBigFloat(res);
                if (op == OP_div) {
                    ret = bf_div(r, a, b, ctx->fp_env.prec, ctx->fp_env.flags) & BF_ST_MEM_ERROR;
                } else {
                    ret = bf_pow(r, a, b, ctx->fp_env.prec,
                                 ctx->fp_env.flags | BF_POW_JS_QUIRKS) & BF_ST_MEM_ERROR;
                }
                JS_FreeBigInt(ctx, a, &a_s);
                JS_FreeBigInt(ctx, b, &b_s);
                JS_FreeValue(ctx, op1);
                JS_FreeValue(ctx, op2);
                if (unlikely(ret)) {
                    JS_FreeValue(ctx, res);
                    throw_bf_exception(ctx, ret);
                    return -1;
                }
                *pres = res;
                return 0;
            }
        } else {
            ret = bf_pow(r, a, b, BF_PREC_INF, BF_RNDZ | BF_POW_JS_QUIRKS);
        }
        break;

        /* logical operations */
    case OP_shl:
    case OP_sar:
        {
            slimb_t v2;
#if LIMB_BITS == 32
            bf_get_int32(&v2, b, 0);
            if (v2 == INT32_MIN)
                v2 = INT32_MIN + 1;
#else
            bf_get_int64(&v2, b, 0);
            if (v2 == INT64_MIN)
                v2 = INT64_MIN + 1;
#endif
            if (op == OP_sar)
                v2 = -v2;
            ret = bf_set(r, a);
            ret |= bf_mul_2exp(r, v2, BF_PREC_INF, BF_RNDZ);
            if (v2 < 0) {
                ret |= bf_rint(r, BF_RNDD) & (BF_ST_OVERFLOW | BF_ST_MEM_ERROR);
            }
        }
        break;
    case OP_and:
        ret = bf_logic_and(r, a, b);
        break;
    case OP_or:
        ret = bf_logic_or(r, a, b);
        break;
    case OP_xor:
        ret = bf_logic_xor(r, a, b);
        break;
    default:
        abort();
    }
    JS_FreeBigInt(ctx, a, &a_s);
    JS_FreeBigInt(ctx, b, &b_s);
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    if (unlikely(ret)) {
        JS_FreeValue(ctx, res);
        throw_bf_exception(ctx, ret);
        return -1;
    }
    *pres = JS_CompactBigInt(ctx, res);
    return 0;
 fail:
    JS_FreeValue(ctx, res);
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    return -1;
}

/* b must be a positive integer */
static int js_bfdec_pow(bfdec_t *r, const bfdec_t *a, const bfdec_t *b)
{
    bfdec_t b1;
    int32_t b2;
    int ret;

    bfdec_init(b->ctx, &b1);
    ret = bfdec_set(&b1, b);
    if (ret) {
        bfdec_delete(&b1);
        return ret;
    }
    ret = bfdec_rint(&b1, BF_RNDZ);
    if (ret) {
        bfdec_delete(&b1);
        return BF_ST_INVALID_OP; /* must be an integer */
    }
    ret = bfdec_get_int32(&b2, &b1);
    bfdec_delete(&b1);
    if (ret)
        return ret; /* overflow */
    if (b2 < 0)
        return BF_ST_INVALID_OP; /* must be positive */
    return bfdec_pow_ui(r, a, b2);
}

static int js_binary_arith_bigdecimal(JSContext *ctx, OPCodeEnum op,
                                      JSValue *pres, JSValue op1, JSValue op2)
{
    bfdec_t *r, *a, *b;
    int ret;
    JSValue res;

    res = JS_NewBigDecimal(ctx);
    if (JS_IsException(res))
        goto fail;
    r = JS_GetBigDecimal(res);
    
    a = JS_ToBigDecimal(ctx, op1);
    if (!a)
        goto fail;
    b = JS_ToBigDecimal(ctx, op2);
    if (!b)
        goto fail;
    switch(op) {
    case OP_add:
        ret = bfdec_add(r, a, b, BF_PREC_INF, BF_RNDZ);
        break;
    case OP_sub:
        ret = bfdec_sub(r, a, b, BF_PREC_INF, BF_RNDZ);
        break;
    case OP_mul:
        ret = bfdec_mul(r, a, b, BF_PREC_INF, BF_RNDZ);
        break;
    case OP_div:
        ret = bfdec_div(r, a, b, BF_PREC_INF, BF_RNDZ);
        break;
    case OP_math_mod:
        /* Euclidian remainder */
        ret = bfdec_rem(r, a, b, BF_PREC_INF, BF_RNDZ, BF_DIVREM_EUCLIDIAN);
        break;
    case OP_mod:
        ret = bfdec_rem(r, a, b, BF_PREC_INF, BF_RNDZ, BF_RNDZ);
        break;
    case OP_pow:
        ret = js_bfdec_pow(r, a, b);
        break;
    default:
        abort();
    }
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    if (unlikely(ret)) {
        JS_FreeValue(ctx, res);
        throw_bf_exception(ctx, ret);
        return -1;
    }
    *pres = res;
    return 0;
 fail:
    JS_FreeValue(ctx, res);
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    return -1;
}

static no_inline __exception int js_binary_arith_slow(JSContext *ctx, JSValue *sp,
                                                      OPCodeEnum op)
{
    JSValue op1, op2, res;
    uint32_t tag1, tag2;
    int ret;
    double d1, d2;

    op1 = sp[-2];
    op2 = sp[-1];
    tag1 = JS_VALUE_GET_NORM_TAG(op1);
    tag2 = JS_VALUE_GET_NORM_TAG(op2);
    /* fast path for float operations */
    if (tag1 == JS_TAG_FLOAT64 && tag2 == JS_TAG_FLOAT64) {
        d1 = JS_VALUE_GET_FLOAT64(op1);
        d2 = JS_VALUE_GET_FLOAT64(op2);
        goto handle_float64;
    }

    /* try to call an overloaded operator */
    if ((tag1 == JS_TAG_OBJECT &&
         (tag2 != JS_TAG_NULL && tag2 != JS_TAG_UNDEFINED)) ||
        (tag2 == JS_TAG_OBJECT &&
         (tag1 != JS_TAG_NULL && tag1 != JS_TAG_UNDEFINED))) {
        ret = js_call_binary_op_fallback(ctx, &res, op1, op2, op, TRUE, 0);
        if (ret != 0) {
            JS_FreeValue(ctx, op1);
            JS_FreeValue(ctx, op2);
            if (ret < 0) {
                goto exception;
            } else {
                sp[-2] = res;
                return 0;
            }
        }
    }

    op1 = JS_ToNumericFree(ctx, op1);
    if (JS_IsException(op1)) {
        JS_FreeValue(ctx, op2);
        goto exception;
    }
    op2 = JS_ToNumericFree(ctx, op2);
    if (JS_IsException(op2)) {
        JS_FreeValue(ctx, op1);
        goto exception;
    }
    tag1 = JS_VALUE_GET_NORM_TAG(op1);
    tag2 = JS_VALUE_GET_NORM_TAG(op2);

    if (tag1 == JS_TAG_INT && tag2 == JS_TAG_INT) {
        int32_t v1, v2;
        int64_t v;
        v1 = JS_VALUE_GET_INT(op1);
        v2 = JS_VALUE_GET_INT(op2);
        switch(op) {
        case OP_sub:
            v = (int64_t)v1 - (int64_t)v2;
            break;
        case OP_mul:
            v = (int64_t)v1 * (int64_t)v2;
            if (is_math_mode(ctx) &&
                (v < -MAX_SAFE_INTEGER || v > MAX_SAFE_INTEGER))
                goto handle_bigint;
            if (v == 0 && (v1 | v2) < 0) {
                sp[-2] = __JS_NewFloat64(ctx, -0.0);
                return 0;
            }
            break;
        case OP_div:
            if (is_math_mode(ctx))
                goto handle_bigint;
            sp[-2] = __JS_NewFloat64(ctx, (double)v1 / (double)v2);
            return 0;
        case OP_math_mod:
            if (unlikely(v2 == 0)) {
                throw_bf_exception(ctx, BF_ST_DIVIDE_ZERO);
                goto exception;
            }
            v = (int64_t)v1 % (int64_t)v2;
            if (v < 0) {
                if (v2 < 0)
                    v -= v2;
                else
                    v += v2;
            }
            break;
        case OP_mod:
            if (v1 < 0 || v2 <= 0) {
                sp[-2] = JS_NewFloat64(ctx, fmod(v1, v2));
                return 0;
            } else {
                v = (int64_t)v1 % (int64_t)v2;
            }
            break;
        case OP_pow:
            if (!is_math_mode(ctx)) {
                sp[-2] = JS_NewFloat64(ctx, js_pow(v1, v2));
                return 0;
            } else {
                goto handle_bigint;
            }
            break;
        default:
            abort();
        }
        sp[-2] = JS_NewInt64(ctx, v);
    } else if (tag1 == JS_TAG_BIG_DECIMAL || tag2 == JS_TAG_BIG_DECIMAL) {
        if (ctx->rt->bigdecimal_ops.binary_arith(ctx, op, sp - 2, op1, op2))
            goto exception;
    } else if (tag1 == JS_TAG_BIG_FLOAT || tag2 == JS_TAG_BIG_FLOAT) {
        if (ctx->rt->bigfloat_ops.binary_arith(ctx, op, sp - 2, op1, op2))
            goto exception;
    } else if (tag1 == JS_TAG_BIG_INT || tag2 == JS_TAG_BIG_INT) {
    handle_bigint:
        if (ctx->rt->bigint_ops.binary_arith(ctx, op, sp - 2, op1, op2))
            goto exception;
    } else {
        double dr;
        /* float64 result */
        if (JS_ToFloat64Free(ctx, &d1, op1)) {
            JS_FreeValue(ctx, op2);
            goto exception;
        }
        if (JS_ToFloat64Free(ctx, &d2, op2))
            goto exception;
    handle_float64:
        if (is_math_mode(ctx) && is_safe_integer(d1) && is_safe_integer(d2))
            goto handle_bigint;
        switch(op) {
        case OP_sub:
            dr = d1 - d2;
            break;
        case OP_mul:
            dr = d1 * d2;
            break;
        case OP_div:
            dr = d1 / d2;
            break;
        case OP_mod:
            dr = fmod(d1, d2);
            break;
        case OP_math_mod:
            d2 = fabs(d2);
            dr = fmod(d1, d2);
            /* XXX: loss of accuracy if dr < 0 */
            if (dr < 0)
                dr += d2;
            break;
        case OP_pow:
            dr = js_pow(d1, d2);
            break;
        default:
            abort();
        }
        sp[-2] = __JS_NewFloat64(ctx, dr);
    }
    return 0;
 exception:
    sp[-2] = JS_UNDEFINED;
    sp[-1] = JS_UNDEFINED;
    return -1;
}

static no_inline __exception int js_add_slow(JSContext *ctx, JSValue *sp)
{
    JSValue op1, op2, res;
    uint32_t tag1, tag2;
    int ret;

    op1 = sp[-2];
    op2 = sp[-1];

    tag1 = JS_VALUE_GET_NORM_TAG(op1);
    tag2 = JS_VALUE_GET_NORM_TAG(op2);
    /* fast path for float64 */
    if (tag1 == JS_TAG_FLOAT64 && tag2 == JS_TAG_FLOAT64) {
        double d1, d2;
        d1 = JS_VALUE_GET_FLOAT64(op1);
        d2 = JS_VALUE_GET_FLOAT64(op2);
        sp[-2] = __JS_NewFloat64(ctx, d1 + d2);
        return 0;
    }

    if (tag1 == JS_TAG_OBJECT || tag2 == JS_TAG_OBJECT) {
        /* try to call an overloaded operator */
        if ((tag1 == JS_TAG_OBJECT &&
             (tag2 != JS_TAG_NULL && tag2 != JS_TAG_UNDEFINED &&
              tag2 != JS_TAG_STRING)) ||
            (tag2 == JS_TAG_OBJECT &&
             (tag1 != JS_TAG_NULL && tag1 != JS_TAG_UNDEFINED &&
              tag1 != JS_TAG_STRING))) {
            ret = js_call_binary_op_fallback(ctx, &res, op1, op2, OP_add,
                                             FALSE, HINT_NONE);
            if (ret != 0) {
                JS_FreeValue(ctx, op1);
                JS_FreeValue(ctx, op2);
                if (ret < 0) {
                    goto exception;
                } else {
                    sp[-2] = res;
                    return 0;
                }
            }
        }

        op1 = JS_ToPrimitiveFree(ctx, op1, HINT_NONE);
        if (JS_IsException(op1)) {
            JS_FreeValue(ctx, op2);
            goto exception;
        }

        op2 = JS_ToPrimitiveFree(ctx, op2, HINT_NONE);
        if (JS_IsException(op2)) {
            JS_FreeValue(ctx, op1);
            goto exception;
        }
        tag1 = JS_VALUE_GET_NORM_TAG(op1);
        tag2 = JS_VALUE_GET_NORM_TAG(op2);
    }

    if (tag1 == JS_TAG_STRING || tag2 == JS_TAG_STRING) {
        sp[-2] = JS_ConcatString(ctx, op1, op2);
        if (JS_IsException(sp[-2]))
            goto exception;
        return 0;
    }

    op1 = JS_ToNumericFree(ctx, op1);
    if (JS_IsException(op1)) {
        JS_FreeValue(ctx, op2);
        goto exception;
    }
    op2 = JS_ToNumericFree(ctx, op2);
    if (JS_IsException(op2)) {
        JS_FreeValue(ctx, op1);
        goto exception;
    }
    tag1 = JS_VALUE_GET_NORM_TAG(op1);
    tag2 = JS_VALUE_GET_NORM_TAG(op2);

    if (tag1 == JS_TAG_INT && tag2 == JS_TAG_INT) {
        int32_t v1, v2;
        int64_t v;
        v1 = JS_VALUE_GET_INT(op1);
        v2 = JS_VALUE_GET_INT(op2);
        v = (int64_t)v1 + (int64_t)v2;
        sp[-2] = JS_NewInt64(ctx, v);
    } else if (tag1 == JS_TAG_BIG_DECIMAL || tag2 == JS_TAG_BIG_DECIMAL) {
        if (ctx->rt->bigdecimal_ops.binary_arith(ctx, OP_add, sp - 2, op1, op2))
            goto exception;
    } else if (tag1 == JS_TAG_BIG_FLOAT || tag2 == JS_TAG_BIG_FLOAT) {
        if (ctx->rt->bigfloat_ops.binary_arith(ctx, OP_add, sp - 2, op1, op2))
            goto exception;
    } else if (tag1 == JS_TAG_BIG_INT || tag2 == JS_TAG_BIG_INT) {
    handle_bigint:
        if (ctx->rt->bigint_ops.binary_arith(ctx, OP_add, sp - 2, op1, op2))
            goto exception;
    } else {
        double d1, d2;
        /* float64 result */
        if (JS_ToFloat64Free(ctx, &d1, op1)) {
            JS_FreeValue(ctx, op2);
            goto exception;
        }
        if (JS_ToFloat64Free(ctx, &d2, op2))
            goto exception;
        if (is_math_mode(ctx) && is_safe_integer(d1) && is_safe_integer(d2))
            goto handle_bigint;
        sp[-2] = __JS_NewFloat64(ctx, d1 + d2);
    }
    return 0;
 exception:
    sp[-2] = JS_UNDEFINED;
    sp[-1] = JS_UNDEFINED;
    return -1;
}

static no_inline __exception int js_binary_logic_slow(JSContext *ctx,
                                                      JSValue *sp,
                                                      OPCodeEnum op)
{
    JSValue op1, op2, res;
    int ret;
    uint32_t tag1, tag2;
    uint32_t v1, v2, r;

    op1 = sp[-2];
    op2 = sp[-1];
    tag1 = JS_VALUE_GET_NORM_TAG(op1);
    tag2 = JS_VALUE_GET_NORM_TAG(op2);

    /* try to call an overloaded operator */
    if ((tag1 == JS_TAG_OBJECT &&
         (tag2 != JS_TAG_NULL && tag2 != JS_TAG_UNDEFINED)) ||
        (tag2 == JS_TAG_OBJECT &&
         (tag1 != JS_TAG_NULL && tag1 != JS_TAG_UNDEFINED))) {
        ret = js_call_binary_op_fallback(ctx, &res, op1, op2, op, TRUE, 0);
        if (ret != 0) {
            JS_FreeValue(ctx, op1);
            JS_FreeValue(ctx, op2);
            if (ret < 0) {
                goto exception;
            } else {
                sp[-2] = res;
                return 0;
            }
        }
    }

    op1 = JS_ToNumericFree(ctx, op1);
    if (JS_IsException(op1)) {
        JS_FreeValue(ctx, op2);
        goto exception;
    }
    op2 = JS_ToNumericFree(ctx, op2);
    if (JS_IsException(op2)) {
        JS_FreeValue(ctx, op1);
        goto exception;
    }

    if (is_math_mode(ctx))
        goto bigint_op;

    tag1 = JS_VALUE_GET_TAG(op1);
    tag2 = JS_VALUE_GET_TAG(op2);
    if (tag1 == JS_TAG_BIG_INT || tag2 == JS_TAG_BIG_INT) {
        if (tag1 != tag2) {
            JS_FreeValue(ctx, op1);
            JS_FreeValue(ctx, op2);
            JS_ThrowTypeError(ctx, "both operands must be bigint");
            goto exception;
        } else {
        bigint_op:
            if (ctx->rt->bigint_ops.binary_arith(ctx, op, sp - 2, op1, op2))
                goto exception;
        }
    } else {
        if (unlikely(JS_ToInt32Free(ctx, (int32_t *)&v1, op1))) {
            JS_FreeValue(ctx, op2);
            goto exception;
        }
        if (unlikely(JS_ToInt32Free(ctx, (int32_t *)&v2, op2)))
            goto exception;
        switch(op) {
        case OP_shl:
            r = v1 << (v2 & 0x1f);
            break;
        case OP_sar:
            r = (int)v1 >> (v2 & 0x1f);
            break;
        case OP_and:
            r = v1 & v2;
            break;
        case OP_or:
            r = v1 | v2;
            break;
        case OP_xor:
            r = v1 ^ v2;
            break;
        default:
            abort();
        }
        sp[-2] = JS_NewInt32(ctx, r);
    }
    return 0;
 exception:
    sp[-2] = JS_UNDEFINED;
    sp[-1] = JS_UNDEFINED;
    return -1;
}

/* Note: also used for bigint */
static int js_compare_bigfloat(JSContext *ctx, OPCodeEnum op,
                               JSValue op1, JSValue op2)
{
    bf_t a_s, b_s, *a, *b;
    int res;
    
    a = JS_ToBigFloat(ctx, &a_s, op1);
    if (!a) {
        JS_FreeValue(ctx, op2);
        return -1;
    }
    b = JS_ToBigFloat(ctx, &b_s, op2);
    if (!b) {
        if (a == &a_s)
            bf_delete(a);
        JS_FreeValue(ctx, op1);
        return -1;
    }
    switch(op) {
    case OP_lt:
        res = bf_cmp_lt(a, b); /* if NaN return false */
        break;
    case OP_lte:
        res = bf_cmp_le(a, b); /* if NaN return false */
        break;
    case OP_gt:
        res = bf_cmp_lt(b, a); /* if NaN return false */
        break;
    case OP_gte:
        res = bf_cmp_le(b, a); /* if NaN return false */
        break;
    case OP_eq:
        res = bf_cmp_eq(a, b); /* if NaN return false */
        break;
    default:
        abort();
    }
    if (a == &a_s)
        bf_delete(a);
    if (b == &b_s)
        bf_delete(b);
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    return res;
}

static int js_compare_bigdecimal(JSContext *ctx, OPCodeEnum op,
                                 JSValue op1, JSValue op2)
{
    bfdec_t *a, *b;
    int res;

    /* Note: binary floats are converted to bigdecimal with
       toString(). It is not mathematically correct but is consistent
       with the BigDecimal() constructor behavior */
    op1 = JS_ToBigDecimalFree(ctx, op1, TRUE);
    if (JS_IsException(op1)) {
        JS_FreeValue(ctx, op2);
        return -1;
    }
    op2 = JS_ToBigDecimalFree(ctx, op2, TRUE);
    if (JS_IsException(op2)) {
        JS_FreeValue(ctx, op1);
        return -1;
    }
    a = JS_ToBigDecimal(ctx, op1);
    b = JS_ToBigDecimal(ctx, op2);
    
    switch(op) {
    case OP_lt:
        res = bfdec_cmp_lt(a, b); /* if NaN return false */
        break;
    case OP_lte:
        res = bfdec_cmp_le(a, b); /* if NaN return false */
        break;
    case OP_gt:
        res = bfdec_cmp_lt(b, a); /* if NaN return false */
        break;
    case OP_gte:
        res = bfdec_cmp_le(b, a); /* if NaN return false */
        break;
    case OP_eq:
        res = bfdec_cmp_eq(a, b); /* if NaN return false */
        break;
    default:
        abort();
    }
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    return res;
}

static no_inline int js_relational_slow(JSContext *ctx, JSValue *sp,
                                        OPCodeEnum op)
{
    JSValue op1, op2, ret;
    int res;
    uint32_t tag1, tag2;

    op1 = sp[-2];
    op2 = sp[-1];
    tag1 = JS_VALUE_GET_NORM_TAG(op1);
    tag2 = JS_VALUE_GET_NORM_TAG(op2);
    /* try to call an overloaded operator */
    if ((tag1 == JS_TAG_OBJECT &&
         (tag2 != JS_TAG_NULL && tag2 != JS_TAG_UNDEFINED)) ||
        (tag2 == JS_TAG_OBJECT &&
         (tag1 != JS_TAG_NULL && tag1 != JS_TAG_UNDEFINED))) {
        res = js_call_binary_op_fallback(ctx, &ret, op1, op2, op,
                                         FALSE, HINT_NUMBER);
        if (res != 0) {
            JS_FreeValue(ctx, op1);
            JS_FreeValue(ctx, op2);
            if (res < 0) {
                goto exception;
            } else {
                sp[-2] = ret;
                return 0;
            }
        }
    }
    op1 = JS_ToPrimitiveFree(ctx, op1, HINT_NUMBER);
    if (JS_IsException(op1)) {
        JS_FreeValue(ctx, op2);
        goto exception;
    }
    op2 = JS_ToPrimitiveFree(ctx, op2, HINT_NUMBER);
    if (JS_IsException(op2)) {
        JS_FreeValue(ctx, op1);
        goto exception;
    }
    tag1 = JS_VALUE_GET_NORM_TAG(op1);
    tag2 = JS_VALUE_GET_NORM_TAG(op2);

    if (tag1 == JS_TAG_STRING && tag2 == JS_TAG_STRING) {
        JSString *p1, *p2;
        p1 = JS_VALUE_GET_STRING(op1);
        p2 = JS_VALUE_GET_STRING(op2);
        res = js_string_compare(ctx, p1, p2);
        switch(op) {
        case OP_lt:
            res = (res < 0);
            break;
        case OP_lte:
            res = (res <= 0);
            break;
        case OP_gt:
            res = (res > 0);
            break;
        default:
        case OP_gte:
            res = (res >= 0);
            break;
        }
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
    } else if ((tag1 <= JS_TAG_NULL || tag1 == JS_TAG_FLOAT64) &&
               (tag2 <= JS_TAG_NULL || tag2 == JS_TAG_FLOAT64)) {
        /* fast path for float64/int */
        goto float64_compare;
    } else {
        if (((tag1 == JS_TAG_BIG_INT && tag2 == JS_TAG_STRING) ||
             (tag2 == JS_TAG_BIG_INT && tag1 == JS_TAG_STRING)) &&
            !is_math_mode(ctx)) {
            if (tag1 == JS_TAG_STRING) {
                op1 = JS_StringToBigInt(ctx, op1);
                if (JS_VALUE_GET_TAG(op1) != JS_TAG_BIG_INT)
                    goto invalid_bigint_string;
            }
            if (tag2 == JS_TAG_STRING) {
                op2 = JS_StringToBigInt(ctx, op2);
                if (JS_VALUE_GET_TAG(op2) != JS_TAG_BIG_INT) {
                invalid_bigint_string:
                    JS_FreeValue(ctx, op1);
                    JS_FreeValue(ctx, op2);
                    res = FALSE;
                    goto done;
                }
            }
        } else {
            op1 = JS_ToNumericFree(ctx, op1);
            if (JS_IsException(op1)) {
                JS_FreeValue(ctx, op2);
                goto exception;
            }
            op2 = JS_ToNumericFree(ctx, op2);
            if (JS_IsException(op2)) {
                JS_FreeValue(ctx, op1);
                goto exception;
            }
        }

        tag1 = JS_VALUE_GET_NORM_TAG(op1);
        tag2 = JS_VALUE_GET_NORM_TAG(op2);

        if (tag1 == JS_TAG_BIG_DECIMAL || tag2 == JS_TAG_BIG_DECIMAL) {
            res = ctx->rt->bigdecimal_ops.compare(ctx, op, op1, op2);
            if (res < 0)
                goto exception;
        } else if (tag1 == JS_TAG_BIG_FLOAT || tag2 == JS_TAG_BIG_FLOAT) {
            res = ctx->rt->bigfloat_ops.compare(ctx, op, op1, op2);
            if (res < 0)
                goto exception;
        } else if (tag1 == JS_TAG_BIG_INT || tag2 == JS_TAG_BIG_INT) {
            res = ctx->rt->bigint_ops.compare(ctx, op, op1, op2);
            if (res < 0)
                goto exception;
        } else {
            double d1, d2;

        float64_compare:
            /* can use floating point comparison */
            if (tag1 == JS_TAG_FLOAT64) {
                d1 = JS_VALUE_GET_FLOAT64(op1);
            } else {
                d1 = JS_VALUE_GET_INT(op1);
            }
            if (tag2 == JS_TAG_FLOAT64) {
                d2 = JS_VALUE_GET_FLOAT64(op2);
            } else {
                d2 = JS_VALUE_GET_INT(op2);
            }
            switch(op) {
            case OP_lt:
                res = (d1 < d2); /* if NaN return false */
                break;
            case OP_lte:
                res = (d1 <= d2); /* if NaN return false */
                break;
            case OP_gt:
                res = (d1 > d2); /* if NaN return false */
                break;
            default:
            case OP_gte:
                res = (d1 >= d2); /* if NaN return false */
                break;
            }
        }
    }
 done:
    sp[-2] = JS_NewBool(ctx, res);
    return 0;
 exception:
    sp[-2] = JS_UNDEFINED;
    sp[-1] = JS_UNDEFINED;
    return -1;
}

static BOOL tag_is_number(uint32_t tag)
{
    return (tag == JS_TAG_INT || tag == JS_TAG_BIG_INT ||
            tag == JS_TAG_FLOAT64 || tag == JS_TAG_BIG_FLOAT ||
            tag == JS_TAG_BIG_DECIMAL);
}

static no_inline __exception int js_eq_slow(JSContext *ctx, JSValue *sp,
                                            BOOL is_neq)
{
    JSValue op1, op2, ret;
    int res;
    uint32_t tag1, tag2;

    op1 = sp[-2];
    op2 = sp[-1];
 redo:
    tag1 = JS_VALUE_GET_NORM_TAG(op1);
    tag2 = JS_VALUE_GET_NORM_TAG(op2);
    if (tag_is_number(tag1) && tag_is_number(tag2)) {
        if (tag1 == JS_TAG_INT && tag2 == JS_TAG_INT) {
            res = JS_VALUE_GET_INT(op1) == JS_VALUE_GET_INT(op2);
        } else if ((tag1 == JS_TAG_FLOAT64 &&
                    (tag2 == JS_TAG_INT || tag2 == JS_TAG_FLOAT64)) ||
                   (tag2 == JS_TAG_FLOAT64 &&
                    (tag1 == JS_TAG_INT || tag1 == JS_TAG_FLOAT64))) {
            double d1, d2;
            if (tag1 == JS_TAG_FLOAT64) {
                d1 = JS_VALUE_GET_FLOAT64(op1);
            } else {
                d1 = JS_VALUE_GET_INT(op1);
            }
            if (tag2 == JS_TAG_FLOAT64) {
                d2 = JS_VALUE_GET_FLOAT64(op2);
            } else {
                d2 = JS_VALUE_GET_INT(op2);
            }
            res = (d1 == d2);
        } else if (tag1 == JS_TAG_BIG_DECIMAL || tag2 == JS_TAG_BIG_DECIMAL) {
            res = ctx->rt->bigdecimal_ops.compare(ctx, OP_eq, op1, op2);
            if (res < 0)
                goto exception;
        } else if (tag1 == JS_TAG_BIG_FLOAT || tag2 == JS_TAG_BIG_FLOAT) {
            res = ctx->rt->bigfloat_ops.compare(ctx, OP_eq, op1, op2);
            if (res < 0)
                goto exception;
        } else {
            res = ctx->rt->bigint_ops.compare(ctx, OP_eq, op1, op2);
            if (res < 0)
                goto exception;
        }
    } else if (tag1 == tag2) {
        if (tag1 == JS_TAG_OBJECT) {
            /* try the fallback operator */
            res = js_call_binary_op_fallback(ctx, &ret, op1, op2,
                                             is_neq ? OP_neq : OP_eq,
                                             FALSE, HINT_NONE);
            if (res != 0) {
                JS_FreeValue(ctx, op1);
                JS_FreeValue(ctx, op2);
                if (res < 0) {
                    goto exception;
                } else {
                    sp[-2] = ret;
                    return 0;
                }
            }
        }
        res = js_strict_eq2(ctx, op1, op2, JS_EQ_STRICT);
    } else if ((tag1 == JS_TAG_NULL && tag2 == JS_TAG_UNDEFINED) ||
               (tag2 == JS_TAG_NULL && tag1 == JS_TAG_UNDEFINED)) {
        res = TRUE;
    } else if ((tag1 == JS_TAG_STRING && tag_is_number(tag2)) ||
               (tag2 == JS_TAG_STRING && tag_is_number(tag1))) {

        if ((tag1 == JS_TAG_BIG_INT || tag2 == JS_TAG_BIG_INT) &&
            !is_math_mode(ctx)) {
            if (tag1 == JS_TAG_STRING) {
                op1 = JS_StringToBigInt(ctx, op1);
                if (JS_VALUE_GET_TAG(op1) != JS_TAG_BIG_INT)
                    goto invalid_bigint_string;
            }
            if (tag2 == JS_TAG_STRING) {
                op2 = JS_StringToBigInt(ctx, op2);
                if (JS_VALUE_GET_TAG(op2) != JS_TAG_BIG_INT) {
                invalid_bigint_string:
                    JS_FreeValue(ctx, op1);
                    JS_FreeValue(ctx, op2);
                    res = FALSE;
                    goto done;
                }
            }
        } else {
            op1 = JS_ToNumericFree(ctx, op1);
            if (JS_IsException(op1)) {
                JS_FreeValue(ctx, op2);
                goto exception;
            }
            op2 = JS_ToNumericFree(ctx, op2);
            if (JS_IsException(op2)) {
                JS_FreeValue(ctx, op1);
                goto exception;
            }
        }
        res = js_strict_eq(ctx, op1, op2);
    } else if (tag1 == JS_TAG_BOOL) {
        op1 = JS_NewInt32(ctx, JS_VALUE_GET_INT(op1));
        goto redo;
    } else if (tag2 == JS_TAG_BOOL) {
        op2 = JS_NewInt32(ctx, JS_VALUE_GET_INT(op2));
        goto redo;
    } else if ((tag1 == JS_TAG_OBJECT &&
                (tag_is_number(tag2) || tag2 == JS_TAG_STRING || tag2 == JS_TAG_SYMBOL)) ||
               (tag2 == JS_TAG_OBJECT &&
                (tag_is_number(tag1) || tag1 == JS_TAG_STRING || tag1 == JS_TAG_SYMBOL))) {

        /* try the fallback operator */
        res = js_call_binary_op_fallback(ctx, &ret, op1, op2,
                                         is_neq ? OP_neq : OP_eq,
                                         FALSE, HINT_NONE);
        if (res != 0) {
            JS_FreeValue(ctx, op1);
            JS_FreeValue(ctx, op2);
            if (res < 0) {
                goto exception;
            } else {
                sp[-2] = ret;
                return 0;
            }
        }

        op1 = JS_ToPrimitiveFree(ctx, op1, HINT_NONE);
        if (JS_IsException(op1)) {
            JS_FreeValue(ctx, op2);
            goto exception;
        }
        op2 = JS_ToPrimitiveFree(ctx, op2, HINT_NONE);
        if (JS_IsException(op2)) {
            JS_FreeValue(ctx, op1);
            goto exception;
        }
        goto redo;
    } else {
        /* IsHTMLDDA object is equivalent to undefined for '==' and '!=' */
        if ((JS_IsHTMLDDA(ctx, op1) &&
             (tag2 == JS_TAG_NULL || tag2 == JS_TAG_UNDEFINED)) ||
            (JS_IsHTMLDDA(ctx, op2) &&
             (tag1 == JS_TAG_NULL || tag1 == JS_TAG_UNDEFINED))) {
            res = TRUE;
        } else {
            res = FALSE;
        }
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
    }
 done:
    sp[-2] = JS_NewBool(ctx, res ^ is_neq);
    return 0;
 exception:
    sp[-2] = JS_UNDEFINED;
    sp[-1] = JS_UNDEFINED;
    return -1;
}

static no_inline int js_shr_slow(JSContext *ctx, JSValue *sp)
{
    JSValue op1, op2;
    uint32_t v1, v2, r;

    op1 = sp[-2];
    op2 = sp[-1];
    op1 = JS_ToNumericFree(ctx, op1);
    if (JS_IsException(op1)) {
        JS_FreeValue(ctx, op2);
        goto exception;
    }
    op2 = JS_ToNumericFree(ctx, op2);
    if (JS_IsException(op2)) {
        JS_FreeValue(ctx, op1);
        goto exception;
    }
    /* XXX: could forbid >>> in bignum mode */
    if (!is_math_mode(ctx) &&
        (JS_VALUE_GET_TAG(op1) == JS_TAG_BIG_INT ||
         JS_VALUE_GET_TAG(op2) == JS_TAG_BIG_INT)) {
        JS_ThrowTypeError(ctx, "bigint operands are forbidden for >>>");
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
        goto exception;
    }
    /* cannot give an exception */
    JS_ToUint32Free(ctx, &v1, op1);
    JS_ToUint32Free(ctx, &v2, op2);
    r = v1 >> (v2 & 0x1f);
    sp[-2] = JS_NewUint32(ctx, r);
    return 0;
 exception:
    sp[-2] = JS_UNDEFINED;
    sp[-1] = JS_UNDEFINED;
    return -1;
}

static JSValue js_mul_pow10_to_float64(JSContext *ctx, const bf_t *a,
                                       int64_t exponent)
{
    bf_t r_s, *r = &r_s;
    double d;
    int ret;
    
    /* always convert to Float64 */
    bf_init(ctx->bf_ctx, r);
    ret = bf_mul_pow_radix(r, a, 10, exponent,
                           53, bf_set_exp_bits(11) | BF_RNDN |
                           BF_FLAG_SUBNORMAL);
    bf_get_float64(r, &d, BF_RNDN);
    bf_delete(r);
    if (ret & BF_ST_MEM_ERROR)
        return JS_ThrowOutOfMemory(ctx);
    else
        return __JS_NewFloat64(ctx, d);
}

static no_inline int js_mul_pow10(JSContext *ctx, JSValue *sp)
{
    bf_t a_s, *a, *r;
    JSValue op1, op2, res;
    int64_t e;
    int ret;

    res = JS_NewBigFloat(ctx);
    if (JS_IsException(res))
        return -1;
    r = JS_GetBigFloat(res);
    op1 = sp[-2];
    op2 = sp[-1];
    a = JS_ToBigFloat(ctx, &a_s, op1);
    if (!a)
        return -1;
    if (JS_IsBigInt(ctx, op2)) {
        ret = JS_ToBigInt64(ctx, &e, op2);
    } else {
        ret = JS_ToInt64(ctx, &e, op2);
    }
    if (ret) {
        if (a == &a_s)
            bf_delete(a);
        JS_FreeValue(ctx, res);
        return -1;
    }

    bf_mul_pow_radix(r, a, 10, e, ctx->fp_env.prec, ctx->fp_env.flags);
    if (a == &a_s)
        bf_delete(a);
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    sp[-2] = res;
    return 0;
}

#else /* !CONFIG_BIGNUM */

static JSValue JS_ThrowUnsupportedBigint(JSContext *ctx)
{
    return JS_ThrowTypeError(ctx, "bigint is not supported");
}

JSValue JS_NewBigInt64(JSContext *ctx, int64_t v)
{
    return JS_ThrowUnsupportedBigint(ctx);
}

JSValue JS_NewBigUint64(JSContext *ctx, uint64_t v)
{
    return JS_ThrowUnsupportedBigint(ctx);
}

int JS_ToBigInt64(JSContext *ctx, int64_t *pres, JSValueConst val)
{
    JS_ThrowUnsupportedBigint(ctx);
    *pres = 0;
    return -1;
}

static no_inline __exception int js_unary_arith_slow(JSContext *ctx,
                                                     JSValue *sp,
                                                     OPCodeEnum op)
{
    JSValue op1;
    double d;

    op1 = sp[-1];
    if (unlikely(JS_ToFloat64Free(ctx, &d, op1))) {
        sp[-1] = JS_UNDEFINED;
        return -1;
    }
    switch(op) {
    case OP_inc:
        d++;
        break;
    case OP_dec:
        d--;
        break;
    case OP_plus:
        break;
    case OP_neg:
        d = -d;
        break;
    default:
        abort();
    }
    sp[-1] = JS_NewFloat64(ctx, d);
    return 0;
}

/* specific case necessary for correct return value semantics */
static __exception int js_post_inc_slow(JSContext *ctx,
                                        JSValue *sp, OPCodeEnum op)
{
    JSValue op1;
    double d, r;

    op1 = sp[-1];
    if (unlikely(JS_ToFloat64Free(ctx, &d, op1))) {
        sp[-1] = JS_UNDEFINED;
        return -1;
    }
    r = d + 2 * (op - OP_post_dec) - 1;
    sp[0] = JS_NewFloat64(ctx, r);
    sp[-1] = JS_NewFloat64(ctx, d);
    return 0;
}

static no_inline __exception int js_binary_arith_slow(JSContext *ctx, JSValue *sp,
                                                      OPCodeEnum op)
{
    JSValue op1, op2;
    double d1, d2, r;

    op1 = sp[-2];
    op2 = sp[-1];
    if (unlikely(JS_ToFloat64Free(ctx, &d1, op1))) {
        JS_FreeValue(ctx, op2);
        goto exception;
    }
    if (unlikely(JS_ToFloat64Free(ctx, &d2, op2))) {
        goto exception;
    }
    switch(op) {
    case OP_sub:
        r = d1 - d2;
        break;
    case OP_mul:
        r = d1 * d2;
        break;
    case OP_div:
        r = d1 / d2;
        break;
    case OP_mod:
        r = fmod(d1, d2);
        break;
    case OP_pow:
        r = js_pow(d1, d2);
        break;
    default:
        abort();
    }
    sp[-2] = JS_NewFloat64(ctx, r);
    return 0;
 exception:
    sp[-2] = JS_UNDEFINED;
    sp[-1] = JS_UNDEFINED;
    return -1;
}

static no_inline __exception int js_add_slow(JSContext *ctx, JSValue *sp)
{
    JSValue op1, op2;
    uint32_t tag1, tag2;

    op1 = sp[-2];
    op2 = sp[-1];
    tag1 = JS_VALUE_GET_TAG(op1);
    tag2 = JS_VALUE_GET_TAG(op2);
    if ((tag1 == JS_TAG_INT || JS_TAG_IS_FLOAT64(tag1)) &&
        (tag2 == JS_TAG_INT || JS_TAG_IS_FLOAT64(tag2))) {
        goto add_numbers;
    } else {
        op1 = JS_ToPrimitiveFree(ctx, op1, HINT_NONE);
        if (JS_IsException(op1)) {
            JS_FreeValue(ctx, op2);
            goto exception;
        }
        op2 = JS_ToPrimitiveFree(ctx, op2, HINT_NONE);
        if (JS_IsException(op2)) {
            JS_FreeValue(ctx, op1);
            goto exception;
        }
        tag1 = JS_VALUE_GET_TAG(op1);
        tag2 = JS_VALUE_GET_TAG(op2);
        if (tag1 == JS_TAG_STRING || tag2 == JS_TAG_STRING) {
            sp[-2] = JS_ConcatString(ctx, op1, op2);
            if (JS_IsException(sp[-2]))
                goto exception;
        } else {
            double d1, d2;
        add_numbers:
            if (JS_ToFloat64Free(ctx, &d1, op1)) {
                JS_FreeValue(ctx, op2);
                goto exception;
            }
            if (JS_ToFloat64Free(ctx, &d2, op2))
                goto exception;
            sp[-2] = JS_NewFloat64(ctx, d1 + d2);
        }
    }
    return 0;
 exception:
    sp[-2] = JS_UNDEFINED;
    sp[-1] = JS_UNDEFINED;
    return -1;
}

static no_inline __exception int js_binary_logic_slow(JSContext *ctx,
                                                      JSValue *sp,
                                                      OPCodeEnum op)
{
    JSValue op1, op2;
    uint32_t v1, v2, r;

    op1 = sp[-2];
    op2 = sp[-1];
    if (unlikely(JS_ToInt32Free(ctx, (int32_t *)&v1, op1))) {
        JS_FreeValue(ctx, op2);
        goto exception;
    }
    if (unlikely(JS_ToInt32Free(ctx, (int32_t *)&v2, op2)))
        goto exception;
    switch(op) {
    case OP_shl:
        r = v1 << (v2 & 0x1f);
        break;
    case OP_sar:
        r = (int)v1 >> (v2 & 0x1f);
        break;
    case OP_and:
        r = v1 & v2;
        break;
    case OP_or:
        r = v1 | v2;
        break;
    case OP_xor:
        r = v1 ^ v2;
        break;
    default:
        abort();
    }
    sp[-2] = JS_NewInt32(ctx, r);
    return 0;
 exception:
    sp[-2] = JS_UNDEFINED;
    sp[-1] = JS_UNDEFINED;
    return -1;
}

static no_inline int js_not_slow(JSContext *ctx, JSValue *sp)
{
    int32_t v1;

    if (unlikely(JS_ToInt32Free(ctx, &v1, sp[-1]))) {
        sp[-1] = JS_UNDEFINED;
        return -1;
    }
    sp[-1] = JS_NewInt32(ctx, ~v1);
    return 0;
}

static no_inline int js_relational_slow(JSContext *ctx, JSValue *sp,
                                        OPCodeEnum op)
{
    JSValue op1, op2;
    int res;

    op1 = sp[-2];
    op2 = sp[-1];
    op1 = JS_ToPrimitiveFree(ctx, op1, HINT_NUMBER);
    if (JS_IsException(op1)) {
        JS_FreeValue(ctx, op2);
        goto exception;
    }
    op2 = JS_ToPrimitiveFree(ctx, op2, HINT_NUMBER);
    if (JS_IsException(op2)) {
        JS_FreeValue(ctx, op1);
        goto exception;
    }
    if (JS_VALUE_GET_TAG(op1) == JS_TAG_STRING &&
        JS_VALUE_GET_TAG(op2) == JS_TAG_STRING) {
        JSString *p1, *p2;
        p1 = JS_VALUE_GET_STRING(op1);
        p2 = JS_VALUE_GET_STRING(op2);
        res = js_string_compare(ctx, p1, p2);
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
        switch(op) {
        case OP_lt:
            res = (res < 0);
            break;
        case OP_lte:
            res = (res <= 0);
            break;
        case OP_gt:
            res = (res > 0);
            break;
        default:
        case OP_gte:
            res = (res >= 0);
            break;
        }
    } else {
        double d1, d2;
        if (JS_ToFloat64Free(ctx, &d1, op1)) {
            JS_FreeValue(ctx, op2);
            goto exception;
        }
        if (JS_ToFloat64Free(ctx, &d2, op2))
            goto exception;
        switch(op) {
        case OP_lt:
            res = (d1 < d2); /* if NaN return false */
            break;
        case OP_lte:
            res = (d1 <= d2); /* if NaN return false */
            break;
        case OP_gt:
            res = (d1 > d2); /* if NaN return false */
            break;
        default:
        case OP_gte:
            res = (d1 >= d2); /* if NaN return false */
            break;
        }
    }
    sp[-2] = JS_NewBool(ctx, res);
    return 0;
 exception:
    sp[-2] = JS_UNDEFINED;
    sp[-1] = JS_UNDEFINED;
    return -1;
}

static no_inline __exception int js_eq_slow(JSContext *ctx, JSValue *sp,
                                            BOOL is_neq)
{
    JSValue op1, op2;
    int tag1, tag2;
    BOOL res;

    op1 = sp[-2];
    op2 = sp[-1];
 redo:
    tag1 = JS_VALUE_GET_NORM_TAG(op1);
    tag2 = JS_VALUE_GET_NORM_TAG(op2);
    if (tag1 == tag2 ||
        (tag1 == JS_TAG_INT && tag2 == JS_TAG_FLOAT64) ||
        (tag2 == JS_TAG_INT && tag1 == JS_TAG_FLOAT64)) {
        res = js_strict_eq(ctx, op1, op2);
    } else if ((tag1 == JS_TAG_NULL && tag2 == JS_TAG_UNDEFINED) ||
               (tag2 == JS_TAG_NULL && tag1 == JS_TAG_UNDEFINED)) {
        res = TRUE;
    } else if ((tag1 == JS_TAG_STRING && (tag2 == JS_TAG_INT ||
                                   tag2 == JS_TAG_FLOAT64)) ||
        (tag2 == JS_TAG_STRING && (tag1 == JS_TAG_INT ||
                                   tag1 == JS_TAG_FLOAT64))) {
        double d1;
        double d2;
        if (JS_ToFloat64Free(ctx, &d1, op1)) {
            JS_FreeValue(ctx, op2);
            goto exception;
        }
        if (JS_ToFloat64Free(ctx, &d2, op2))
            goto exception;
        res = (d1 == d2);
    } else if (tag1 == JS_TAG_BOOL) {
        op1 = JS_NewInt32(ctx, JS_VALUE_GET_INT(op1));
        goto redo;
    } else if (tag2 == JS_TAG_BOOL) {
        op2 = JS_NewInt32(ctx, JS_VALUE_GET_INT(op2));
        goto redo;
    } else if (tag1 == JS_TAG_OBJECT &&
               (tag2 == JS_TAG_INT || tag2 == JS_TAG_FLOAT64 || tag2 == JS_TAG_STRING || tag2 == JS_TAG_SYMBOL)) {
        op1 = JS_ToPrimitiveFree(ctx, op1, HINT_NONE);
        if (JS_IsException(op1)) {
            JS_FreeValue(ctx, op2);
            goto exception;
        }
        goto redo;
    } else if (tag2 == JS_TAG_OBJECT &&
               (tag1 == JS_TAG_INT || tag1 == JS_TAG_FLOAT64 || tag1 == JS_TAG_STRING || tag1 == JS_TAG_SYMBOL)) {
        op2 = JS_ToPrimitiveFree(ctx, op2, HINT_NONE);
        if (JS_IsException(op2)) {
            JS_FreeValue(ctx, op1);
            goto exception;
        }
        goto redo;
    } else {
        /* IsHTMLDDA object is equivalent to undefined for '==' and '!=' */
        if ((JS_IsHTMLDDA(ctx, op1) &&
             (tag2 == JS_TAG_NULL || tag2 == JS_TAG_UNDEFINED)) ||
            (JS_IsHTMLDDA(ctx, op2) &&
             (tag1 == JS_TAG_NULL || tag1 == JS_TAG_UNDEFINED))) {
            res = TRUE;
        } else {
            res = FALSE;
        }
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
    }
    sp[-2] = JS_NewBool(ctx, res ^ is_neq);
    return 0;
 exception:
    sp[-2] = JS_UNDEFINED;
    sp[-1] = JS_UNDEFINED;
    return -1;
}

static no_inline int js_shr_slow(JSContext *ctx, JSValue *sp)
{
    JSValue op1, op2;
    uint32_t v1, v2, r;

    op1 = sp[-2];
    op2 = sp[-1];
    if (unlikely(JS_ToUint32Free(ctx, &v1, op1))) {
        JS_FreeValue(ctx, op2);
        goto exception;
    }
    if (unlikely(JS_ToUint32Free(ctx, &v2, op2)))
        goto exception;
    r = v1 >> (v2 & 0x1f);
    sp[-2] = JS_NewUint32(ctx, r);
    return 0;
 exception:
    sp[-2] = JS_UNDEFINED;
    sp[-1] = JS_UNDEFINED;
    return -1;
}

#endif /* !CONFIG_BIGNUM */

/* XXX: Should take JSValueConst arguments */
static BOOL js_strict_eq2(JSContext *ctx, JSValue op1, JSValue op2,
                          JSStrictEqModeEnum eq_mode)
{
    BOOL res;
    int tag1, tag2;
    double d1, d2;

    tag1 = JS_VALUE_GET_NORM_TAG(op1);
    tag2 = JS_VALUE_GET_NORM_TAG(op2);
    switch(tag1) {
    case JS_TAG_BOOL:
        if (tag1 != tag2) {
            res = FALSE;
        } else {
            res = JS_VALUE_GET_INT(op1) == JS_VALUE_GET_INT(op2);
            goto done_no_free;
        }
        break;
    case JS_TAG_NULL:
    case JS_TAG_UNDEFINED:
        res = (tag1 == tag2);
        break;
    case JS_TAG_STRING:
        {
            JSString *p1, *p2;
            if (tag1 != tag2) {
                res = FALSE;
            } else {
                p1 = JS_VALUE_GET_STRING(op1);
                p2 = JS_VALUE_GET_STRING(op2);
                res = (js_string_compare(ctx, p1, p2) == 0);
            }
        }
        break;
    case JS_TAG_SYMBOL:
        {
            JSAtomStruct *p1, *p2;
            if (tag1 != tag2) {
                res = FALSE;
            } else {
                p1 = JS_VALUE_GET_PTR(op1);
                p2 = JS_VALUE_GET_PTR(op2);
                res = (p1 == p2);
            }
        }
        break;
    case JS_TAG_OBJECT:
        if (tag1 != tag2)
            res = FALSE;
        else
            res = JS_VALUE_GET_OBJ(op1) == JS_VALUE_GET_OBJ(op2);
        break;
    case JS_TAG_INT:
        d1 = JS_VALUE_GET_INT(op1);
        if (tag2 == JS_TAG_INT) {
            d2 = JS_VALUE_GET_INT(op2);
            goto number_test;
        } else if (tag2 == JS_TAG_FLOAT64) {
            d2 = JS_VALUE_GET_FLOAT64(op2);
            goto number_test;
        } else {
            res = FALSE;
        }
        break;
    case JS_TAG_FLOAT64:
        d1 = JS_VALUE_GET_FLOAT64(op1);
        if (tag2 == JS_TAG_FLOAT64) {
            d2 = JS_VALUE_GET_FLOAT64(op2);
        } else if (tag2 == JS_TAG_INT) {
            d2 = JS_VALUE_GET_INT(op2);
        } else {
            res = FALSE;
            break;
        }
    number_test:
        if (unlikely(eq_mode >= JS_EQ_SAME_VALUE)) {
            JSFloat64Union u1, u2;
            /* NaN is not always normalized, so this test is necessary */
            if (isnan(d1) || isnan(d2)) {
                res = isnan(d1) == isnan(d2);
            } else if (eq_mode == JS_EQ_SAME_VALUE_ZERO) {
                res = (d1 == d2); /* +0 == -0 */
            } else {
                u1.d = d1;
                u2.d = d2;
                res = (u1.u64 == u2.u64); /* +0 != -0 */
            }
        } else {
            res = (d1 == d2); /* if NaN return false and +0 == -0 */
        }
        goto done_no_free;
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_INT:
        {
            bf_t a_s, *a, b_s, *b;
            if (tag1 != tag2) {
                res = FALSE;
                break;
            }
            a = JS_ToBigFloat(ctx, &a_s, op1);
            b = JS_ToBigFloat(ctx, &b_s, op2);
            res = bf_cmp_eq(a, b);
            if (a == &a_s)
                bf_delete(a);
            if (b == &b_s)
                bf_delete(b);
        }
        break;
    case JS_TAG_BIG_FLOAT:
        {
            JSBigFloat *p1, *p2;
            const bf_t *a, *b;
            if (tag1 != tag2) {
                res = FALSE;
                break;
            }
            p1 = JS_VALUE_GET_PTR(op1);
            p2 = JS_VALUE_GET_PTR(op2);
            a = &p1->num;
            b = &p2->num;
            if (unlikely(eq_mode >= JS_EQ_SAME_VALUE)) {
                if (eq_mode == JS_EQ_SAME_VALUE_ZERO &&
                           a->expn == BF_EXP_ZERO && b->expn == BF_EXP_ZERO) {
                    res = TRUE;
                } else {
                    res = (bf_cmp_full(a, b) == 0);
                }
            } else {
                res = bf_cmp_eq(a, b);
            }
        }
        break;
    case JS_TAG_BIG_DECIMAL:
        {
            JSBigDecimal *p1, *p2;
            const bfdec_t *a, *b;
            if (tag1 != tag2) {
                res = FALSE;
                break;
            }
            p1 = JS_VALUE_GET_PTR(op1);
            p2 = JS_VALUE_GET_PTR(op2);
            a = &p1->num;
            b = &p2->num;
            res = bfdec_cmp_eq(a, b);
        }
        break;
#endif
    default:
        res = FALSE;
        break;
    }
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
 done_no_free:
    return res;
}

static BOOL js_strict_eq(JSContext *ctx, JSValue op1, JSValue op2)
{
    return js_strict_eq2(ctx, op1, op2, JS_EQ_STRICT);
}

static BOOL js_same_value(JSContext *ctx, JSValueConst op1, JSValueConst op2)
{
    return js_strict_eq2(ctx,
                         JS_DupValue(ctx, op1), JS_DupValue(ctx, op2),
                         JS_EQ_SAME_VALUE);
}

static BOOL js_same_value_zero(JSContext *ctx, JSValueConst op1, JSValueConst op2)
{
    return js_strict_eq2(ctx,
                         JS_DupValue(ctx, op1), JS_DupValue(ctx, op2),
                         JS_EQ_SAME_VALUE_ZERO);
}

static no_inline int js_strict_eq_slow(JSContext *ctx, JSValue *sp,
                                       BOOL is_neq)
{
    BOOL res;
    res = js_strict_eq(ctx, sp[-2], sp[-1]);
    sp[-2] = JS_NewBool(ctx, res ^ is_neq);
    return 0;
}

static __exception int js_operator_in(JSContext *ctx, JSValue *sp)
{
    JSValue op1, op2;
    JSAtom atom;
    int ret;

    op1 = sp[-2];
    op2 = sp[-1];

    if (JS_VALUE_GET_TAG(op2) != JS_TAG_OBJECT) {
        JS_ThrowTypeError(ctx, "invalid 'in' operand");
        return -1;
    }
    atom = JS_ValueToAtom(ctx, op1);
    if (unlikely(atom == JS_ATOM_NULL))
        return -1;
    ret = JS_HasProperty(ctx, op2, atom);
    JS_FreeAtom(ctx, atom);
    if (ret < 0)
        return -1;
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    sp[-2] = JS_NewBool(ctx, ret);
    return 0;
}

static __exception int js_has_unscopable(JSContext *ctx, JSValueConst obj,
                                         JSAtom atom)
{
    JSValue arr, val;
    int ret;

    arr = JS_GetProperty(ctx, obj, JS_ATOM_Symbol_unscopables);
    if (JS_IsException(arr))
        return -1;
    ret = 0;
    if (JS_IsObject(arr)) {
        val = JS_GetProperty(ctx, arr, atom);
        ret = JS_ToBoolFree(ctx, val);
    }
    JS_FreeValue(ctx, arr);
    return ret;
}

static __exception int js_operator_instanceof(JSContext *ctx, JSValue *sp)
{
    JSValue op1, op2;
    BOOL ret;

    op1 = sp[-2];
    op2 = sp[-1];
    ret = JS_IsInstanceOf(ctx, op1, op2);
    if (ret < 0)
        return ret;
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    sp[-2] = JS_NewBool(ctx, ret);
    return 0;
}

static __exception int js_operator_typeof(JSContext *ctx, JSValueConst op1)
{
    JSAtom atom;
    uint32_t tag;

    tag = JS_VALUE_GET_NORM_TAG(op1);
    switch(tag) {
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_INT:
        atom = JS_ATOM_bigint;
        break;
    case JS_TAG_BIG_FLOAT:
        atom = JS_ATOM_bigfloat;
        break;
    case JS_TAG_BIG_DECIMAL:
        atom = JS_ATOM_bigdecimal;
        break;
#endif
    case JS_TAG_INT:
    case JS_TAG_FLOAT64:
        atom = JS_ATOM_number;
        break;
    case JS_TAG_UNDEFINED:
        atom = JS_ATOM_undefined;
        break;
    case JS_TAG_BOOL:
        atom = JS_ATOM_boolean;
        break;
    case JS_TAG_STRING:
        atom = JS_ATOM_string;
        break;
    case JS_TAG_OBJECT:
        {
            JSObject *p;
            p = JS_VALUE_GET_OBJ(op1);
            if (unlikely(p->is_HTMLDDA)) 
                atom = JS_ATOM_undefined;
            else if (JS_IsFunction(ctx, op1))
                atom = JS_ATOM_function;
            else
                goto obj_type;
        }
        break;
    case JS_TAG_NULL:
    obj_type:
        atom = JS_ATOM_object;
        break;
    case JS_TAG_SYMBOL:
        atom = JS_ATOM_symbol;
        break;
    default:
        atom = JS_ATOM_unknown;
        break;
    }
    return atom;
}

static __exception int js_operator_delete(JSContext *ctx, JSValue *sp)
{
    JSValue op1, op2;
    JSAtom atom;
    int ret;

    op1 = sp[-2];
    op2 = sp[-1];
    atom = JS_ValueToAtom(ctx, op2);
    if (unlikely(atom == JS_ATOM_NULL))
        return -1;
    ret = JS_DeleteProperty(ctx, op1, atom, JS_PROP_THROW_STRICT);
    JS_FreeAtom(ctx, atom);
    if (unlikely(ret < 0))
        return -1;
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    sp[-2] = JS_NewBool(ctx, ret);
    return 0;
}

static JSValue js_throw_type_error(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv)
{
    return JS_ThrowTypeError(ctx, "invalid property access");
}

/* XXX: not 100% compatible, but mozilla seems to use a similar
   implementation to ensure that caller in non strict mode does not
   throw (ES5 compatibility) */
static JSValue js_function_proto_caller(JSContext *ctx, JSValueConst this_val,
                                        int argc, JSValueConst *argv)
{
    JSFunctionBytecode *b = JS_GetFunctionBytecode(this_val);
    if (!b || (b->js_mode & JS_MODE_STRICT) || !b->has_prototype) {
        return js_throw_type_error(ctx, this_val, 0, NULL);
    }
    return JS_UNDEFINED;
}

static JSValue js_function_proto_fileName(JSContext *ctx,
                                          JSValueConst this_val)
{
    JSFunctionBytecode *b = JS_GetFunctionBytecode(this_val);
    if (b && b->has_debug) {
        return JS_AtomToString(ctx, b->debug.filename);
    }
    return JS_UNDEFINED;
}

static JSValue js_function_proto_lineNumber(JSContext *ctx,
                                            JSValueConst this_val)
{
    JSFunctionBytecode *b = JS_GetFunctionBytecode(this_val);
    if (b && b->has_debug) {
        return JS_NewInt32(ctx, b->debug.line_num);
    }
    return JS_UNDEFINED;
}

static int js_arguments_define_own_property(JSContext *ctx,
                                            JSValueConst this_obj,
                                            JSAtom prop, JSValueConst val,
                                            JSValueConst getter, JSValueConst setter, int flags)
{
    JSObject *p;
    uint32_t idx;
    p = JS_VALUE_GET_OBJ(this_obj);
    /* convert to normal array when redefining an existing numeric field */
    if (p->fast_array && JS_AtomIsArrayIndex(ctx, &idx, prop) &&
        idx < p->u.array.count) {
        if (convert_fast_array_to_array(ctx, p))
            return -1;
    }
    /* run the default define own property */
    return JS_DefineProperty(ctx, this_obj, prop, val, getter, setter,
                             flags | JS_PROP_NO_EXOTIC);
}

static const JSClassExoticMethods js_arguments_exotic_methods = {
    .define_own_property = js_arguments_define_own_property,
};

static JSValue js_build_arguments(JSContext *ctx, int argc, JSValueConst *argv)
{
    JSValue val, *tab;
    JSProperty *pr;
    JSObject *p;
    int i;

    val = JS_NewObjectProtoClass(ctx, ctx->class_proto[JS_CLASS_OBJECT],
                                 JS_CLASS_ARGUMENTS);
    if (JS_IsException(val))
        return val;
    p = JS_VALUE_GET_OBJ(val);

    /* add the length field (cannot fail) */
    pr = add_property(ctx, p, JS_ATOM_length,
                      JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
    pr->u.value = JS_NewInt32(ctx, argc);

    /* initialize the fast array part */
    tab = NULL;
    if (argc > 0) {
        tab = js_malloc(ctx, sizeof(tab[0]) * argc);
        if (!tab) {
            JS_FreeValue(ctx, val);
            return JS_EXCEPTION;
        }
        for(i = 0; i < argc; i++) {
            tab[i] = JS_DupValue(ctx, argv[i]);
        }
    }
    p->u.array.u.values = tab;
    p->u.array.count = argc;

    JS_DefinePropertyValue(ctx, val, JS_ATOM_Symbol_iterator,
                           JS_DupValue(ctx, ctx->array_proto_values),
                           JS_PROP_CONFIGURABLE | JS_PROP_WRITABLE);
    /* add callee property to throw a TypeError in strict mode */
    JS_DefineProperty(ctx, val, JS_ATOM_callee, JS_UNDEFINED,
                      ctx->throw_type_error, ctx->throw_type_error,
                      JS_PROP_HAS_GET | JS_PROP_HAS_SET);
    return val;
}

#define GLOBAL_VAR_OFFSET 0x40000000
#define ARGUMENT_VAR_OFFSET 0x20000000

/* legacy arguments object: add references to the function arguments */
static JSValue js_build_mapped_arguments(JSContext *ctx, int argc,
                                         JSValueConst *argv,
                                         JSStackFrame *sf, int arg_count)
{
    JSValue val;
    JSProperty *pr;
    JSObject *p;
    int i;

    val = JS_NewObjectProtoClass(ctx, ctx->class_proto[JS_CLASS_OBJECT],
                                 JS_CLASS_MAPPED_ARGUMENTS);
    if (JS_IsException(val))
        return val;
    p = JS_VALUE_GET_OBJ(val);

    /* add the length field (cannot fail) */
    pr = add_property(ctx, p, JS_ATOM_length,
                      JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
    pr->u.value = JS_NewInt32(ctx, argc);

    for(i = 0; i < arg_count; i++) {
        JSVarRef *var_ref;
        var_ref = get_var_ref(ctx, sf, i, TRUE);
        if (!var_ref)
            goto fail;
        pr = add_property(ctx, p, __JS_AtomFromUInt32(i), JS_PROP_C_W_E | JS_PROP_VARREF);
        if (!pr) {
            free_var_ref(ctx->rt, var_ref);
            goto fail;
        }
        pr->u.var_ref = var_ref;
    }

    /* the arguments not mapped to the arguments of the function can
       be normal properties */
    for(i = arg_count; i < argc; i++) {
        if (JS_DefinePropertyValueUint32(ctx, val, i,
                                         JS_DupValue(ctx, argv[i]),
                                         JS_PROP_C_W_E) < 0)
            goto fail;
    }

    JS_DefinePropertyValue(ctx, val, JS_ATOM_Symbol_iterator,
                           JS_DupValue(ctx, ctx->array_proto_values),
                           JS_PROP_CONFIGURABLE | JS_PROP_WRITABLE);
    /* callee returns this function in non strict mode */
    JS_DefinePropertyValue(ctx, val, JS_ATOM_callee,
                           JS_DupValue(ctx, ctx->rt->current_stack_frame->cur_func),
                           JS_PROP_CONFIGURABLE | JS_PROP_WRITABLE);
    return val;
 fail:
    JS_FreeValue(ctx, val);
    return JS_EXCEPTION;
}

static JSValue js_build_rest(JSContext *ctx, int first, int argc, JSValueConst *argv)
{
    JSValue val;
    int i, ret;

    val = JS_NewArray(ctx);
    if (JS_IsException(val))
        return val;
    for (i = first; i < argc; i++) {
        ret = JS_DefinePropertyValueUint32(ctx, val, i - first,
                                           JS_DupValue(ctx, argv[i]),
                                           JS_PROP_C_W_E);
        if (ret < 0) {
            JS_FreeValue(ctx, val);
            return JS_EXCEPTION;
        }
    }
    return val;
}

static JSValue build_for_in_iterator(JSContext *ctx, JSValue obj)
{
    JSObject *p;
    JSPropertyEnum *tab_atom;
    int i;
    JSValue enum_obj, obj1;
    JSForInIterator *it;
    uint32_t tag, tab_atom_count;

    tag = JS_VALUE_GET_TAG(obj);
    if (tag != JS_TAG_OBJECT && tag != JS_TAG_NULL && tag != JS_TAG_UNDEFINED) {
        obj = JS_ToObjectFree(ctx, obj);
    }

    it = js_malloc(ctx, sizeof(*it));
    if (!it) {
        JS_FreeValue(ctx, obj);
        return JS_EXCEPTION;
    }
    enum_obj = JS_NewObjectProtoClass(ctx, JS_NULL, JS_CLASS_FOR_IN_ITERATOR);
    if (JS_IsException(enum_obj)) {
        js_free(ctx, it);
        JS_FreeValue(ctx, obj);
        return JS_EXCEPTION;
    }
    it->is_array = FALSE;
    it->obj = obj;
    it->idx = 0;
    p = JS_VALUE_GET_OBJ(enum_obj);
    p->u.for_in_iterator = it;

    if (tag == JS_TAG_NULL || tag == JS_TAG_UNDEFINED)
        return enum_obj;

    /* fast path: assume no enumerable properties in the prototype chain */
    obj1 = JS_DupValue(ctx, obj);
    for(;;) {
        obj1 = JS_GetPrototypeFree(ctx, obj1);
        if (JS_IsNull(obj1))
            break;
        if (JS_IsException(obj1))
            goto fail;
        if (JS_GetOwnPropertyNamesInternal(ctx, &tab_atom, &tab_atom_count,
                                           JS_VALUE_GET_OBJ(obj1),
                                           JS_GPN_STRING_MASK | JS_GPN_ENUM_ONLY)) {
            JS_FreeValue(ctx, obj1);
            goto fail;
        }
        js_free_prop_enum(ctx, tab_atom, tab_atom_count);
        if (tab_atom_count != 0) {
            JS_FreeValue(ctx, obj1);
            goto slow_path;
        }
        /* must check for timeout to avoid infinite loop */
        if (js_poll_interrupts(ctx)) {
            JS_FreeValue(ctx, obj1);
            goto fail;
        }
    }

    p = JS_VALUE_GET_OBJ(obj);

    if (p->fast_array) {
        JSShape *sh;
        JSShapeProperty *prs;
        /* check that there are no enumerable normal fields */
        sh = p->shape;
        for(i = 0, prs = get_shape_prop(sh); i < sh->prop_count; i++, prs++) {
            if (prs->flags & JS_PROP_ENUMERABLE)
                goto normal_case;
        }
        /* for fast arrays, we only store the number of elements */
        it->is_array = TRUE;
        it->array_length = p->u.array.count;
    } else {
    normal_case:
        if (JS_GetOwnPropertyNamesInternal(ctx, &tab_atom, &tab_atom_count, p,
                                   JS_GPN_STRING_MASK | JS_GPN_ENUM_ONLY))
            goto fail;
        for(i = 0; i < tab_atom_count; i++) {
            JS_SetPropertyInternal(ctx, enum_obj, tab_atom[i].atom, JS_NULL, 0);
        }
        js_free_prop_enum(ctx, tab_atom, tab_atom_count);
    }
    return enum_obj;

 slow_path:
    /* non enumerable properties hide the enumerables ones in the
       prototype chain */
    obj1 = JS_DupValue(ctx, obj);
    for(;;) {
        if (JS_GetOwnPropertyNamesInternal(ctx, &tab_atom, &tab_atom_count,
                                           JS_VALUE_GET_OBJ(obj1),
                                           JS_GPN_STRING_MASK | JS_GPN_SET_ENUM)) {
            JS_FreeValue(ctx, obj1);
            goto fail;
        }
        for(i = 0; i < tab_atom_count; i++) {
            JS_DefinePropertyValue(ctx, enum_obj, tab_atom[i].atom, JS_NULL,
                                   (tab_atom[i].is_enumerable ?
                                    JS_PROP_ENUMERABLE : 0));
        }
        js_free_prop_enum(ctx, tab_atom, tab_atom_count);
        obj1 = JS_GetPrototypeFree(ctx, obj1);
        if (JS_IsNull(obj1))
            break;
        if (JS_IsException(obj1))
            goto fail;
        /* must check for timeout to avoid infinite loop */
        if (js_poll_interrupts(ctx)) {
            JS_FreeValue(ctx, obj1);
            goto fail;
        }
    }
    return enum_obj;

 fail:
    JS_FreeValue(ctx, enum_obj);
    return JS_EXCEPTION;
}

/* obj -> enum_obj */
static __exception int js_for_in_start(JSContext *ctx, JSValue *sp)
{
    sp[-1] = build_for_in_iterator(ctx, sp[-1]);
    if (JS_IsException(sp[-1]))
        return -1;
    return 0;
}

/* enum_obj -> enum_obj value done */
static __exception int js_for_in_next(JSContext *ctx, JSValue *sp)
{
    JSValueConst enum_obj;
    JSObject *p;
    JSAtom prop;
    JSForInIterator *it;
    int ret;

    enum_obj = sp[-1];
    /* fail safe */
    if (JS_VALUE_GET_TAG(enum_obj) != JS_TAG_OBJECT)
        goto done;
    p = JS_VALUE_GET_OBJ(enum_obj);
    if (p->class_id != JS_CLASS_FOR_IN_ITERATOR)
        goto done;
    it = p->u.for_in_iterator;

    for(;;) {
        if (it->is_array) {
            if (it->idx >= it->array_length)
                goto done;
            prop = __JS_AtomFromUInt32(it->idx);
            it->idx++;
        } else {
            JSShape *sh = p->shape;
            JSShapeProperty *prs;
            if (it->idx >= sh->prop_count)
                goto done;
            prs = get_shape_prop(sh) + it->idx;
            prop = prs->atom;
            it->idx++;
            if (prop == JS_ATOM_NULL || !(prs->flags & JS_PROP_ENUMERABLE))
                continue;
        }
        /* check if the property was deleted */
        ret = JS_HasProperty(ctx, it->obj, prop);
        if (ret < 0)
            return ret;
        if (ret)
            break;
    }
    /* return the property */
    sp[0] = JS_AtomToValue(ctx, prop);
    sp[1] = JS_FALSE;
    return 0;
 done:
    /* return the end */
    sp[0] = JS_UNDEFINED;
    sp[1] = JS_TRUE;
    return 0;
}

static JSValue JS_GetIterator2(JSContext *ctx, JSValueConst obj,
                               JSValueConst method)
{
    JSValue enum_obj;

    enum_obj = JS_Call(ctx, method, obj, 0, NULL);
    if (JS_IsException(enum_obj))
        return enum_obj;
    if (!JS_IsObject(enum_obj)) {
        JS_FreeValue(ctx, enum_obj);
        return JS_ThrowTypeErrorNotAnObject(ctx);
    }
    return enum_obj;
}

static JSValue JS_GetIterator(JSContext *ctx, JSValueConst obj, BOOL is_async)
{
    JSValue method, ret, sync_iter;

    if (is_async) {
        method = JS_GetProperty(ctx, obj, JS_ATOM_Symbol_asyncIterator);
        if (JS_IsException(method))
            return method;
        if (JS_IsUndefined(method) || JS_IsNull(method)) {
            method = JS_GetProperty(ctx, obj, JS_ATOM_Symbol_iterator);
            if (JS_IsException(method))
                return method;
            sync_iter = JS_GetIterator2(ctx, obj, method);
            JS_FreeValue(ctx, method);
            if (JS_IsException(sync_iter))
                return sync_iter;
            ret = JS_CreateAsyncFromSyncIterator(ctx, sync_iter);
            JS_FreeValue(ctx, sync_iter);
            return ret;
        }
    } else {
        method = JS_GetProperty(ctx, obj, JS_ATOM_Symbol_iterator);
        if (JS_IsException(method))
            return method;
    }
    if (!JS_IsFunction(ctx, method)) {
        JS_FreeValue(ctx, method);
        return JS_ThrowTypeError(ctx, "value is not iterable");
    }
    ret = JS_GetIterator2(ctx, obj, method);
    JS_FreeValue(ctx, method);
    return ret;
}

/* return *pdone = 2 if the iterator object is not parsed */
static JSValue JS_IteratorNext2(JSContext *ctx, JSValueConst enum_obj,
                                JSValueConst method,
                                int argc, JSValueConst *argv, int *pdone)
{
    JSValue obj;

    /* fast path for the built-in iterators (avoid creating the
       intermediate result object) */
    if (JS_IsObject(method)) {
        JSObject *p = JS_VALUE_GET_OBJ(method);
        if (p->class_id == JS_CLASS_C_FUNCTION &&
            p->u.cfunc.cproto == JS_CFUNC_iterator_next) {
            JSCFunctionType func;
            JSValueConst args[1];

            /* in case the function expects one argument */
            if (argc == 0) {
                args[0] = JS_UNDEFINED;
                argv = args;
            }
            func = p->u.cfunc.c_function;
            return func.iterator_next(ctx, enum_obj, argc, argv,
                                      pdone, p->u.cfunc.magic);
        }
    }
    obj = JS_Call(ctx, method, enum_obj, argc, argv);
    if (JS_IsException(obj))
        goto fail;
    if (!JS_IsObject(obj)) {
        JS_FreeValue(ctx, obj);
        JS_ThrowTypeError(ctx, "iterator must return an object");
        goto fail;
    }
    *pdone = 2;
    return obj;
 fail:
    *pdone = FALSE;
    return JS_EXCEPTION;
}

static JSValue JS_IteratorNext(JSContext *ctx, JSValueConst enum_obj,
                               JSValueConst method,
                               int argc, JSValueConst *argv, BOOL *pdone)
{
    JSValue obj, value, done_val;
    int done;

    obj = JS_IteratorNext2(ctx, enum_obj, method, argc, argv, &done);
    if (JS_IsException(obj))
        goto fail;
    if (done != 2) {
        *pdone = done;
        return obj;
    } else {
        done_val = JS_GetProperty(ctx, obj, JS_ATOM_done);
        if (JS_IsException(done_val))
            goto fail;
        *pdone = JS_ToBoolFree(ctx, done_val);
        value = JS_UNDEFINED;
        if (!*pdone) {
            value = JS_GetProperty(ctx, obj, JS_ATOM_value);
        }
        JS_FreeValue(ctx, obj);
        return value;
    }
 fail:
    JS_FreeValue(ctx, obj);
    *pdone = FALSE;
    return JS_EXCEPTION;
}

/* return < 0 in case of exception */
static int JS_IteratorClose(JSContext *ctx, JSValueConst enum_obj,
                            BOOL is_exception_pending)
{
    JSValue method, ret, ex_obj;
    int res;

    if (is_exception_pending) {
        ex_obj = ctx->rt->current_exception;
        ctx->rt->current_exception = JS_NULL;
        res = -1;
    } else {
        ex_obj = JS_UNDEFINED;
        res = 0;
    }
    method = JS_GetProperty(ctx, enum_obj, JS_ATOM_return);
    if (JS_IsException(method)) {
        res = -1;
        goto done;
    }
    if (JS_IsUndefined(method) || JS_IsNull(method)) {
        goto done;
    }
    ret = JS_CallFree(ctx, method, enum_obj, 0, NULL);
    if (!is_exception_pending) {
        if (JS_IsException(ret)) {
            res = -1;
        } else if (!JS_IsObject(ret)) {
            JS_ThrowTypeErrorNotAnObject(ctx);
            res = -1;
        }
    }
    JS_FreeValue(ctx, ret);
 done:
    if (is_exception_pending) {
        JS_Throw(ctx, ex_obj);
    }
    return res;
}

/* obj -> enum_rec (3 slots) */
static __exception int js_for_of_start(JSContext *ctx, JSValue *sp,
                                       BOOL is_async)
{
    JSValue op1, obj, method;
    op1 = sp[-1];
    obj = JS_GetIterator(ctx, op1, is_async);
    if (JS_IsException(obj))
        return -1;
    JS_FreeValue(ctx, op1);
    sp[-1] = obj;
    method = JS_GetProperty(ctx, obj, JS_ATOM_next);
    if (JS_IsException(method))
        return -1;
    sp[0] = method;
    return 0;
}

/* enum_rec [objs] -> enum_rec [objs] value done. There are 'offset'
   objs. If 'done' is true or in case of exception, 'enum_rec' is set
   to undefined. If 'done' is true, 'value' is always set to
   undefined. */
static __exception int js_for_of_next(JSContext *ctx, JSValue *sp, int offset)
{
    JSValue value = JS_UNDEFINED;
    int done = 1;

    if (likely(!JS_IsUndefined(sp[offset]))) {
        value = JS_IteratorNext(ctx, sp[offset], sp[offset + 1], 0, NULL, &done);
        if (JS_IsException(value))
            done = -1;
        if (done) {
            /* value is JS_UNDEFINED or JS_EXCEPTION */
            /* replace the iteration object with undefined */
            JS_FreeValue(ctx, sp[offset]);
            sp[offset] = JS_UNDEFINED;
            if (done < 0) {
                return -1;
            } else {
                JS_FreeValue(ctx, value);
                value = JS_UNDEFINED;
            }
        }
    }
    sp[0] = value;
    sp[1] = JS_NewBool(ctx, done);
    return 0;
}

static JSValue JS_IteratorGetCompleteValue(JSContext *ctx, JSValueConst obj,
                                           BOOL *pdone)
{
    JSValue done_val, value;
    BOOL done;
    done_val = JS_GetProperty(ctx, obj, JS_ATOM_done);
    if (JS_IsException(done_val))
        goto fail;
    done = JS_ToBoolFree(ctx, done_val);
    value = JS_GetProperty(ctx, obj, JS_ATOM_value);
    if (JS_IsException(value))
        goto fail;
    *pdone = done;
    return value;
 fail:
    *pdone = FALSE;
    return JS_EXCEPTION;
}

static __exception int js_iterator_get_value_done(JSContext *ctx, JSValue *sp)
{
    JSValue obj, value;
    BOOL done;
    obj = sp[-1];
    if (!JS_IsObject(obj)) {
        JS_ThrowTypeError(ctx, "iterator must return an object");
        return -1;
    }
    value = JS_IteratorGetCompleteValue(ctx, obj, &done);
    if (JS_IsException(value))
        return -1;
    JS_FreeValue(ctx, obj);
    sp[-1] = value;
    sp[0] = JS_NewBool(ctx, done);
    return 0;
}

static JSValue js_create_iterator_result(JSContext *ctx,
                                         JSValue val,
                                         BOOL done)
{
    JSValue obj;
    obj = JS_NewObject(ctx);
    if (JS_IsException(obj)) {
        JS_FreeValue(ctx, val);
        return obj;
    }
    if (JS_DefinePropertyValue(ctx, obj, JS_ATOM_value,
                               val, JS_PROP_C_W_E) < 0) {
        goto fail;
    }
    if (JS_DefinePropertyValue(ctx, obj, JS_ATOM_done,
                               JS_NewBool(ctx, done), JS_PROP_C_W_E) < 0) {
    fail:
        JS_FreeValue(ctx, obj);
        return JS_EXCEPTION;
    }
    return obj;
}

static JSValue js_array_iterator_next(JSContext *ctx, JSValueConst this_val,
                                      int argc, JSValueConst *argv,
                                      BOOL *pdone, int magic);

static JSValue js_create_array_iterator(JSContext *ctx, JSValueConst this_val,
                                        int argc, JSValueConst *argv, int magic);

static BOOL js_is_fast_array(JSContext *ctx, JSValueConst obj)
{
    /* Try and handle fast arrays explicitly */
    if (JS_VALUE_GET_TAG(obj) == JS_TAG_OBJECT) {
        JSObject *p = JS_VALUE_GET_OBJ(obj);
        if (p->class_id == JS_CLASS_ARRAY && p->fast_array) {
            return TRUE;
        }
    }
    return FALSE;
}

/* Access an Array's internal JSValue array if available */
static BOOL js_get_fast_array(JSContext *ctx, JSValueConst obj,
                              JSValue **arrpp, uint32_t *countp)
{
    /* Try and handle fast arrays explicitly */
    if (JS_VALUE_GET_TAG(obj) == JS_TAG_OBJECT) {
        JSObject *p = JS_VALUE_GET_OBJ(obj);
        if (p->class_id == JS_CLASS_ARRAY && p->fast_array) {
            *countp = p->u.array.count;
            *arrpp = p->u.array.u.values;
            return TRUE;
        }
    }
    return FALSE;
}

static __exception int js_append_enumerate(JSContext *ctx, JSValue *sp)
{
    JSValue iterator, enumobj, method, value;
    int is_array_iterator;
    JSValue *arrp;
    uint32_t i, count32, pos;
    
    if (JS_VALUE_GET_TAG(sp[-2]) != JS_TAG_INT) {
        JS_ThrowInternalError(ctx, "invalid index for append");
        return -1;
    }

    pos = JS_VALUE_GET_INT(sp[-2]);

    /* XXX: further optimisations:
       - use ctx->array_proto_values?
       - check if array_iterator_prototype next method is built-in and
         avoid constructing actual iterator object?
       - build this into js_for_of_start and use in all `for (x of o)` loops
     */
    iterator = JS_GetProperty(ctx, sp[-1], JS_ATOM_Symbol_iterator);
    if (JS_IsException(iterator))
        return -1;
    is_array_iterator = JS_IsCFunction(ctx, iterator,
                                       (JSCFunction *)js_create_array_iterator,
                                       JS_ITERATOR_KIND_VALUE);
    JS_FreeValue(ctx, iterator);

    enumobj = JS_GetIterator(ctx, sp[-1], FALSE);
    if (JS_IsException(enumobj))
        return -1;
    method = JS_GetProperty(ctx, enumobj, JS_ATOM_next);
    if (JS_IsException(method)) {
        JS_FreeValue(ctx, enumobj);
        return -1;
    }
    if (is_array_iterator
    &&  JS_IsCFunction(ctx, method, (JSCFunction *)js_array_iterator_next, 0)
    &&  js_get_fast_array(ctx, sp[-1], &arrp, &count32)) {
        uint32_t len;
        if (js_get_length32(ctx, &len, sp[-1]))
            goto exception;
        /* if len > count32, the elements >= count32 might be read in
           the prototypes and might have side effects */
        if (len != count32)
            goto general_case;
        /* Handle fast arrays explicitly */
        for (i = 0; i < count32; i++) {
            if (JS_DefinePropertyValueUint32(ctx, sp[-3], pos++,
                                             JS_DupValue(ctx, arrp[i]), JS_PROP_C_W_E) < 0)
                goto exception;
        }
    } else {
    general_case:
        for (;;) {
            BOOL done;
            value = JS_IteratorNext(ctx, enumobj, method, 0, NULL, &done);
            if (JS_IsException(value))
                goto exception;
            if (done) {
                /* value is JS_UNDEFINED */
                break;
            }
            if (JS_DefinePropertyValueUint32(ctx, sp[-3], pos++, value, JS_PROP_C_W_E) < 0)
                goto exception;
        }
    }
    /* Note: could raise an error if too many elements */
    sp[-2] = JS_NewInt32(ctx, pos);
    JS_FreeValue(ctx, enumobj);
    JS_FreeValue(ctx, method);
    return 0;

exception:
    JS_IteratorClose(ctx, enumobj, TRUE);
    JS_FreeValue(ctx, enumobj);
    JS_FreeValue(ctx, method);
    return -1;
}

static __exception int JS_CopyDataProperties(JSContext *ctx,
                                             JSValueConst target,
                                             JSValueConst source,
                                             JSValueConst excluded,
                                             BOOL setprop)
{
    JSPropertyEnum *tab_atom;
    JSValue val;
    uint32_t i, tab_atom_count;
    JSObject *p;
    JSObject *pexcl = NULL;
    int ret, gpn_flags;
    JSPropertyDescriptor desc;
    BOOL is_enumerable;
    
    if (JS_VALUE_GET_TAG(source) != JS_TAG_OBJECT)
        return 0;

    if (JS_VALUE_GET_TAG(excluded) == JS_TAG_OBJECT)
        pexcl = JS_VALUE_GET_OBJ(excluded);

    p = JS_VALUE_GET_OBJ(source);

    gpn_flags = JS_GPN_STRING_MASK | JS_GPN_SYMBOL_MASK | JS_GPN_ENUM_ONLY;
    if (p->is_exotic) {
        const JSClassExoticMethods *em = ctx->rt->class_array[p->class_id].exotic;
        /* cannot use JS_GPN_ENUM_ONLY with e.g. proxies because it
           introduces a visible change */
        if (em && em->get_own_property_names) {
            gpn_flags &= ~JS_GPN_ENUM_ONLY;
        }
    }
    if (JS_GetOwnPropertyNamesInternal(ctx, &tab_atom, &tab_atom_count, p,
                                       gpn_flags))
        return -1;
    
    for (i = 0; i < tab_atom_count; i++) {
        if (pexcl) {
            ret = JS_GetOwnPropertyInternal(ctx, NULL, pexcl, tab_atom[i].atom);
            if (ret) {
                if (ret < 0)
                    goto exception;
                continue;
            }
        }
        if (!(gpn_flags & JS_GPN_ENUM_ONLY)) {
            /* test if the property is enumerable */
            ret = JS_GetOwnPropertyInternal(ctx, &desc, p, tab_atom[i].atom);
            if (ret < 0)
                goto exception;
            if (!ret)
                continue;
            is_enumerable = (desc.flags & JS_PROP_ENUMERABLE) != 0;
            js_free_desc(ctx, &desc);
            if (!is_enumerable)
                continue;
        }
        val = JS_GetProperty(ctx, source, tab_atom[i].atom);
        if (JS_IsException(val))
            goto exception;
        if (setprop)
            ret = JS_SetProperty(ctx, target, tab_atom[i].atom, val);
        else
            ret = JS_DefinePropertyValue(ctx, target, tab_atom[i].atom, val,
                                         JS_PROP_C_W_E);
        if (ret < 0)
            goto exception;
    }
    js_free_prop_enum(ctx, tab_atom, tab_atom_count);
    return 0;
 exception:
    js_free_prop_enum(ctx, tab_atom, tab_atom_count);
    return -1;
}

/* only valid inside C functions */
static JSValueConst JS_GetActiveFunction(JSContext *ctx)
{
    return ctx->rt->current_stack_frame->cur_func;
}

static JSVarRef *get_var_ref(JSContext *ctx, JSStackFrame *sf,
                             int var_idx, BOOL is_arg)
{
    JSVarRef *var_ref;
    struct list_head *el;

    list_for_each(el, &sf->var_ref_list) {
        var_ref = list_entry(el, JSVarRef, header.link);
        if (var_ref->var_idx == var_idx && var_ref->is_arg == is_arg) {
            var_ref->header.ref_count++;
            return var_ref;
        }
    }
    /* create a new one */
    var_ref = js_malloc(ctx, sizeof(JSVarRef));
    if (!var_ref)
        return NULL;
    var_ref->header.ref_count = 1;
    var_ref->is_detached = FALSE;
    var_ref->is_arg = is_arg;
    var_ref->var_idx = var_idx;
    list_add_tail(&var_ref->header.link, &sf->var_ref_list);
    if (is_arg)
        var_ref->pvalue = &sf->arg_buf[var_idx];
    else
        var_ref->pvalue = &sf->var_buf[var_idx];
    var_ref->value = JS_UNDEFINED;
    return var_ref;
}

static JSValue js_closure2(JSContext *ctx, JSValue func_obj,
                           JSFunctionBytecode *b,
                           JSVarRef **cur_var_refs,
                           JSStackFrame *sf)
{
    JSObject *p;
    JSVarRef **var_refs;
    int i;

    p = JS_VALUE_GET_OBJ(func_obj);
    p->u.func.function_bytecode = b;
    p->u.func.home_object = NULL;
    p->u.func.var_refs = NULL;
    if (b->closure_var_count) {
        var_refs = js_mallocz(ctx, sizeof(var_refs[0]) * b->closure_var_count);
        if (!var_refs)
            goto fail;
        p->u.func.var_refs = var_refs;
        for(i = 0; i < b->closure_var_count; i++) {
            JSClosureVar *cv = &b->closure_var[i];
            JSVarRef *var_ref;
            if (cv->is_local) {
                /* reuse the existing variable reference if it already exists */
                var_ref = get_var_ref(ctx, sf, cv->var_idx, cv->is_arg);
                if (!var_ref)
                    goto fail;
            } else {
                var_ref = cur_var_refs[cv->var_idx];
                var_ref->header.ref_count++;
            }
            var_refs[i] = var_ref;
        }
    }
    return func_obj;
 fail:
    /* bfunc is freed when func_obj is freed */
    JS_FreeValue(ctx, func_obj);
    return JS_EXCEPTION;
}

static JSValue js_instantiate_prototype(JSContext *ctx, JSObject *p, JSAtom atom, void *opaque)
{
    JSValue obj, this_val;
    int ret;

    this_val = JS_MKPTR(JS_TAG_OBJECT, p);
    obj = JS_NewObject(ctx);
    if (JS_IsException(obj))
        return JS_EXCEPTION;
    set_cycle_flag(ctx, obj);
    set_cycle_flag(ctx, this_val);
    ret = JS_DefinePropertyValue(ctx, obj, JS_ATOM_constructor,
                                 JS_DupValue(ctx, this_val),
                                 JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
    if (ret < 0) {
        JS_FreeValue(ctx, obj);
        return JS_EXCEPTION;
    }
    return obj;
}

static const uint16_t func_kind_to_class_id[] = {
    [JS_FUNC_NORMAL] = JS_CLASS_BYTECODE_FUNCTION,
    [JS_FUNC_GENERATOR] = JS_CLASS_GENERATOR_FUNCTION,
    [JS_FUNC_ASYNC] = JS_CLASS_ASYNC_FUNCTION,
    [JS_FUNC_ASYNC_GENERATOR] = JS_CLASS_ASYNC_GENERATOR_FUNCTION,
};

static JSValue js_closure(JSContext *ctx, JSValue bfunc,
                          JSVarRef **cur_var_refs,
                          JSStackFrame *sf)
{
    JSFunctionBytecode *b;
    JSValue func_obj;
    JSAtom name_atom;

    b = JS_VALUE_GET_PTR(bfunc);
    func_obj = JS_NewObjectClass(ctx, func_kind_to_class_id[b->func_kind]);
    if (JS_IsException(func_obj)) {
        JS_FreeValue(ctx, bfunc);
        return JS_EXCEPTION;
    }
    func_obj = js_closure2(ctx, func_obj, b, cur_var_refs, sf);
    if (JS_IsException(func_obj)) {
        /* bfunc has been freed */
        goto fail;
    }
    name_atom = b->func_name;
    if (name_atom == JS_ATOM_NULL)
        name_atom = JS_ATOM_empty_string;
    js_function_set_properties(ctx, func_obj, name_atom,
                               b->defined_arg_count);

    if (b->func_kind & JS_FUNC_GENERATOR) {
        JSValue proto;
        int proto_class_id;
        /* generators have a prototype field which is used as
           prototype for the generator object */
        if (b->func_kind == JS_FUNC_ASYNC_GENERATOR)
            proto_class_id = JS_CLASS_ASYNC_GENERATOR;
        else
            proto_class_id = JS_CLASS_GENERATOR;
        proto = JS_NewObjectProto(ctx, ctx->class_proto[proto_class_id]);
        if (JS_IsException(proto))
            goto fail;
        JS_DefinePropertyValue(ctx, func_obj, JS_ATOM_prototype, proto,
                               JS_PROP_WRITABLE);
    } else if (b->has_prototype) {
        /* add the 'prototype' property: delay instantiation to avoid
           creating cycles for every javascript function. The prototype
           object is created on the fly when first accessed */
        JS_SetConstructorBit(ctx, func_obj, TRUE);
        JS_DefineAutoInitProperty(ctx, func_obj, JS_ATOM_prototype,
                                  JS_AUTOINIT_ID_PROTOTYPE, NULL,
                                  JS_PROP_WRITABLE);
    }
    return func_obj;
 fail:
    /* bfunc is freed when func_obj is freed */
    JS_FreeValue(ctx, func_obj);
    return JS_EXCEPTION;
}

#define JS_DEFINE_CLASS_HAS_HERITAGE     (1 << 0)

static int js_op_define_class(JSContext *ctx, JSValue *sp,
                              JSAtom class_name, int class_flags,
                              JSVarRef **cur_var_refs,
                              JSStackFrame *sf, BOOL is_computed_name)
{
    JSValue bfunc, parent_class, proto = JS_UNDEFINED;
    JSValue ctor = JS_UNDEFINED, parent_proto = JS_UNDEFINED;
    JSFunctionBytecode *b;

    parent_class = sp[-2];
    bfunc = sp[-1];

    if (class_flags & JS_DEFINE_CLASS_HAS_HERITAGE) {
        if (JS_IsNull(parent_class)) {
            parent_proto = JS_NULL;
            parent_class = JS_DupValue(ctx, ctx->function_proto);
        } else {
            if (!JS_IsConstructor(ctx, parent_class)) {
                JS_ThrowTypeError(ctx, "parent class must be constructor");
                goto fail;
            }
            parent_proto = JS_GetProperty(ctx, parent_class, JS_ATOM_prototype);
            if (JS_IsException(parent_proto))
                goto fail;
            if (!JS_IsNull(parent_proto) && !JS_IsObject(parent_proto)) {
                JS_ThrowTypeError(ctx, "parent prototype must be an object or null");
                goto fail;
            }
        }
    } else {
        /* parent_class is JS_UNDEFINED in this case */
        parent_proto = JS_DupValue(ctx, ctx->class_proto[JS_CLASS_OBJECT]);
        parent_class = JS_DupValue(ctx, ctx->function_proto);
    }
    proto = JS_NewObjectProto(ctx, parent_proto);
    if (JS_IsException(proto))
        goto fail;

    b = JS_VALUE_GET_PTR(bfunc);
    assert(b->func_kind == JS_FUNC_NORMAL);
    ctor = JS_NewObjectProtoClass(ctx, parent_class,
                                  JS_CLASS_BYTECODE_FUNCTION);
    if (JS_IsException(ctor))
        goto fail;
    ctor = js_closure2(ctx, ctor, b, cur_var_refs, sf);
    bfunc = JS_UNDEFINED;
    if (JS_IsException(ctor))
        goto fail;
    js_method_set_home_object(ctx, ctor, proto);
    JS_SetConstructorBit(ctx, ctor, TRUE);

    JS_DefinePropertyValue(ctx, ctor, JS_ATOM_length,
                           JS_NewInt32(ctx, b->defined_arg_count),
                           JS_PROP_CONFIGURABLE);

    if (is_computed_name) {
        if (JS_DefineObjectNameComputed(ctx, ctor, sp[-3],
                                        JS_PROP_CONFIGURABLE) < 0)
            goto fail;
    } else {
        if (JS_DefineObjectName(ctx, ctor, class_name, JS_PROP_CONFIGURABLE) < 0)
            goto fail;
    }

    /* the constructor property must be first. It can be overriden by
       computed property names */
    if (JS_DefinePropertyValue(ctx, proto, JS_ATOM_constructor,
                               JS_DupValue(ctx, ctor),
                               JS_PROP_CONFIGURABLE |
                               JS_PROP_WRITABLE | JS_PROP_THROW) < 0)
        goto fail;
    /* set the prototype property */
    if (JS_DefinePropertyValue(ctx, ctor, JS_ATOM_prototype,
                               JS_DupValue(ctx, proto), JS_PROP_THROW) < 0)
        goto fail;
    set_cycle_flag(ctx, ctor);
    set_cycle_flag(ctx, proto);

    JS_FreeValue(ctx, parent_proto);
    JS_FreeValue(ctx, parent_class);

    sp[-2] = ctor;
    sp[-1] = proto;
    return 0;
 fail:
    JS_FreeValue(ctx, parent_class);
    JS_FreeValue(ctx, parent_proto);
    JS_FreeValue(ctx, bfunc);
    JS_FreeValue(ctx, proto);
    JS_FreeValue(ctx, ctor);
    sp[-2] = JS_UNDEFINED;
    sp[-1] = JS_UNDEFINED;
    return -1;
}

static void close_var_refs(JSRuntime *rt, JSStackFrame *sf)
{
    struct list_head *el, *el1;
    JSVarRef *var_ref;
    int var_idx;

    list_for_each_safe(el, el1, &sf->var_ref_list) {
        var_ref = list_entry(el, JSVarRef, header.link);
        var_idx = var_ref->var_idx;
        if (var_ref->is_arg)
            var_ref->value = JS_DupValueRT(rt, sf->arg_buf[var_idx]);
        else
            var_ref->value = JS_DupValueRT(rt, sf->var_buf[var_idx]);
        var_ref->pvalue = &var_ref->value;
        /* the reference is no longer to a local variable */
        var_ref->is_detached = TRUE;
        add_gc_object(rt, &var_ref->header, JS_GC_OBJ_TYPE_VAR_REF);
    }
}

static void close_lexical_var(JSContext *ctx, JSStackFrame *sf, int idx, int is_arg)
{
    struct list_head *el, *el1;
    JSVarRef *var_ref;
    int var_idx = idx;

    list_for_each_safe(el, el1, &sf->var_ref_list) {
        var_ref = list_entry(el, JSVarRef, header.link);
        if (var_idx == var_ref->var_idx && var_ref->is_arg == is_arg) {
            var_ref->value = JS_DupValue(ctx, sf->var_buf[var_idx]);
            var_ref->pvalue = &var_ref->value;
            list_del(&var_ref->header.link);
            /* the reference is no longer to a local variable */
            var_ref->is_detached = TRUE;
            add_gc_object(ctx->rt, &var_ref->header, JS_GC_OBJ_TYPE_VAR_REF);
        }
    }
}

#define JS_CALL_FLAG_COPY_ARGV   (1 << 1)
#define JS_CALL_FLAG_GENERATOR   (1 << 2)

static JSValue js_call_c_function(JSContext *ctx, JSValueConst func_obj,
                                  JSValueConst this_obj,
                                  int argc, JSValueConst *argv, int flags)
{
    JSRuntime *rt = ctx->rt;
    JSCFunctionType func;
    JSObject *p;
    JSStackFrame sf_s, *sf = &sf_s, *prev_sf;
    JSValue ret_val;
    JSValueConst *arg_buf;
    int arg_count, i;
    JSCFunctionEnum cproto;

    p = JS_VALUE_GET_OBJ(func_obj);
    cproto = p->u.cfunc.cproto;
    arg_count = p->u.cfunc.length;

    /* better to always check stack overflow */
    if (js_check_stack_overflow(rt, sizeof(arg_buf[0]) * arg_count))
        return JS_ThrowStackOverflow(ctx);

    prev_sf = rt->current_stack_frame;
    sf->prev_frame = prev_sf;
    rt->current_stack_frame = sf;
    ctx = p->u.cfunc.realm; /* change the current realm */
    
#ifdef CONFIG_BIGNUM
    /* we only propagate the bignum mode as some runtime functions
       test it */
    if (prev_sf)
        sf->js_mode = prev_sf->js_mode & JS_MODE_MATH;
    else
        sf->js_mode = 0;
#else
    sf->js_mode = 0;
#endif
    sf->cur_func = (JSValue)func_obj;
    sf->arg_count = argc;
    arg_buf = argv;

    if (unlikely(argc < arg_count)) {
        /* ensure that at least argc_count arguments are readable */
        arg_buf = alloca(sizeof(arg_buf[0]) * arg_count);
        for(i = 0; i < argc; i++)
            arg_buf[i] = argv[i];
        for(i = argc; i < arg_count; i++)
            arg_buf[i] = JS_UNDEFINED;
        sf->arg_count = arg_count;
    }
    sf->arg_buf = (JSValue*)arg_buf;

    func = p->u.cfunc.c_function;
    switch(cproto) {
    case JS_CFUNC_constructor:
    case JS_CFUNC_constructor_or_func:
        if (!(flags & JS_CALL_FLAG_CONSTRUCTOR)) {
            if (cproto == JS_CFUNC_constructor) {
            not_a_constructor:
                ret_val = JS_ThrowTypeError(ctx, "must be called with new");
                break;
            } else {
                this_obj = JS_UNDEFINED;
            }
        }
        /* here this_obj is new_target */
        /* fall thru */
    case JS_CFUNC_generic:
        ret_val = func.generic(ctx, this_obj, argc, arg_buf);
        break;
    case JS_CFUNC_constructor_magic:
    case JS_CFUNC_constructor_or_func_magic:
        if (!(flags & JS_CALL_FLAG_CONSTRUCTOR)) {
            if (cproto == JS_CFUNC_constructor_magic) {
                goto not_a_constructor;
            } else {
                this_obj = JS_UNDEFINED;
            }
        }
        /* fall thru */
    case JS_CFUNC_generic_magic:
        ret_val = func.generic_magic(ctx, this_obj, argc, arg_buf,
                                     p->u.cfunc.magic);
        break;
    case JS_CFUNC_getter:
        ret_val = func.getter(ctx, this_obj);
        break;
    case JS_CFUNC_setter:
        ret_val = func.setter(ctx, this_obj, arg_buf[0]);
        break;
    case JS_CFUNC_getter_magic:
        ret_val = func.getter_magic(ctx, this_obj, p->u.cfunc.magic);
        break;
    case JS_CFUNC_setter_magic:
        ret_val = func.setter_magic(ctx, this_obj, arg_buf[0], p->u.cfunc.magic);
        break;
    case JS_CFUNC_f_f:
        {
            double d1;

            if (unlikely(JS_ToFloat64(ctx, &d1, arg_buf[0]))) {
                ret_val = JS_EXCEPTION;
                break;
            }
            ret_val = JS_NewFloat64(ctx, func.f_f(d1));
        }
        break;
    case JS_CFUNC_f_f_f:
        {
            double d1, d2;

            if (unlikely(JS_ToFloat64(ctx, &d1, arg_buf[0]))) {
                ret_val = JS_EXCEPTION;
                break;
            }
            if (unlikely(JS_ToFloat64(ctx, &d2, arg_buf[1]))) {
                ret_val = JS_EXCEPTION;
                break;
            }
            ret_val = JS_NewFloat64(ctx, func.f_f_f(d1, d2));
        }
        break;
    case JS_CFUNC_iterator_next:
        {
            int done;
            ret_val = func.iterator_next(ctx, this_obj, argc, arg_buf,
                                         &done, p->u.cfunc.magic);
            if (!JS_IsException(ret_val) && done != 2) {
                ret_val = js_create_iterator_result(ctx, ret_val, done);
            }
        }
        break;
    default:
        abort();
    }

    rt->current_stack_frame = sf->prev_frame;
    return ret_val;
}

static JSValue js_call_bound_function(JSContext *ctx, JSValueConst func_obj,
                                      JSValueConst this_obj,
                                      int argc, JSValueConst *argv, int flags)
{
    JSObject *p;
    JSBoundFunction *bf;
    JSValueConst *arg_buf, new_target;
    int arg_count, i;

    p = JS_VALUE_GET_OBJ(func_obj);
    bf = p->u.bound_function;
    arg_count = bf->argc + argc;
    if (js_check_stack_overflow(ctx->rt, sizeof(JSValue) * arg_count))
        return JS_ThrowStackOverflow(ctx);
    arg_buf = alloca(sizeof(JSValue) * arg_count);
    for(i = 0; i < bf->argc; i++) {
        arg_buf[i] = bf->argv[i];
    }
    for(i = 0; i < argc; i++) {
        arg_buf[bf->argc + i] = argv[i];
    }
    if (flags & JS_CALL_FLAG_CONSTRUCTOR) {
        new_target = this_obj;
        if (js_same_value(ctx, func_obj, new_target))
            new_target = bf->func_obj;
        return JS_CallConstructor2(ctx, bf->func_obj, new_target,
                                   arg_count, arg_buf);
    } else {
        return JS_Call(ctx, bf->func_obj, bf->this_val,
                       arg_count, arg_buf);
    }
}

/* argument of OP_special_object */
typedef enum {
    OP_SPECIAL_OBJECT_ARGUMENTS,
    OP_SPECIAL_OBJECT_MAPPED_ARGUMENTS,
    OP_SPECIAL_OBJECT_THIS_FUNC,
    OP_SPECIAL_OBJECT_NEW_TARGET,
    OP_SPECIAL_OBJECT_HOME_OBJECT,
    OP_SPECIAL_OBJECT_VAR_OBJECT,
    OP_SPECIAL_OBJECT_IMPORT_META,
} OPSpecialObjectEnum;

#define FUNC_RET_AWAIT      0
#define FUNC_RET_YIELD      1
#define FUNC_RET_YIELD_STAR 2

/* argv[] is modified if (flags & JS_CALL_FLAG_COPY_ARGV) = 0. */
static JSValue JS_CallInternal(JSContext *caller_ctx, JSValueConst func_obj,
                               JSValueConst this_obj, JSValueConst new_target,
                               int argc, JSValue *argv, int flags)
{
    JSRuntime *rt = caller_ctx->rt;
    JSContext *ctx;
    JSObject *p;
    JSFunctionBytecode *b;
    JSStackFrame sf_s, *sf = &sf_s;
    const uint8_t *pc;
    int opcode, arg_allocated_size, i;
    JSValue *local_buf, *stack_buf, *var_buf, *arg_buf, *sp, ret_val, *pval;
    JSVarRef **var_refs;
    size_t alloca_size;

#if !DIRECT_DISPATCH
#define SWITCH(pc)      switch (opcode = *pc++)
#define CASE(op)        case op
#define DEFAULT         default
#define BREAK           break
#else
    static const void * const dispatch_table[256] = {
#define DEF(id, size, n_pop, n_push, f) && case_OP_ ## id,
#if SHORT_OPCODES
#define def(id, size, n_pop, n_push, f)
#else                                                     
#define def(id, size, n_pop, n_push, f) && case_default,
#endif
#include "quickjs-opcode.h"
        [ OP_COUNT ... 255 ] = &&case_default
    };
#define SWITCH(pc)      goto *dispatch_table[opcode = *pc++];
#define CASE(op)        case_ ## op
#define DEFAULT         case_default
#define BREAK           SWITCH(pc)
#endif

    if (js_poll_interrupts(caller_ctx))
        return JS_EXCEPTION;
    if (unlikely(JS_VALUE_GET_TAG(func_obj) != JS_TAG_OBJECT)) {
        if (flags & JS_CALL_FLAG_GENERATOR) {
            JSAsyncFunctionState *s = JS_VALUE_GET_PTR(func_obj);
            /* func_obj get contains a pointer to JSFuncAsyncState */
            /* the stack frame is already allocated */
            sf = &s->frame;
            p = JS_VALUE_GET_OBJ(sf->cur_func);
            b = p->u.func.function_bytecode;
            ctx = b->realm;
            var_refs = p->u.func.var_refs;
            local_buf = arg_buf = sf->arg_buf;
            var_buf = sf->var_buf;
            stack_buf = sf->var_buf + b->var_count;
            sp = sf->cur_sp;
            sf->cur_sp = NULL; /* cur_sp is NULL if the function is running */
            pc = sf->cur_pc;
            sf->prev_frame = rt->current_stack_frame;
            rt->current_stack_frame = sf;
            if (s->throw_flag)
                goto exception;
            else
                goto restart;
        } else {
            goto not_a_function;
        }
    }
    p = JS_VALUE_GET_OBJ(func_obj);
    if (unlikely(p->class_id != JS_CLASS_BYTECODE_FUNCTION)) {
        JSClassCall *call_func;
        call_func = rt->class_array[p->class_id].call;
        if (!call_func) {
        not_a_function:
            return JS_ThrowTypeError(caller_ctx, "not a function");
        }
        return call_func(caller_ctx, func_obj, this_obj, argc,
                         (JSValueConst *)argv, flags);
    }
    b = p->u.func.function_bytecode;

    if (unlikely(argc < b->arg_count || (flags & JS_CALL_FLAG_COPY_ARGV))) {
        arg_allocated_size = b->arg_count;
    } else {
        arg_allocated_size = 0;
    }

    alloca_size = sizeof(JSValue) * (arg_allocated_size + b->var_count +
                                     b->stack_size);
    if (js_check_stack_overflow(rt, alloca_size))
        return JS_ThrowStackOverflow(caller_ctx);

    sf->js_mode = b->js_mode;
    arg_buf = argv;
    sf->arg_count = argc;
    sf->cur_func = (JSValue)func_obj;
    init_list_head(&sf->var_ref_list);
    var_refs = p->u.func.var_refs;

    local_buf = alloca(alloca_size);
    if (unlikely(arg_allocated_size)) {
        int n = min_int(argc, b->arg_count);
        arg_buf = local_buf;
        for(i = 0; i < n; i++)
            arg_buf[i] = JS_DupValue(caller_ctx, argv[i]);
        for(; i < b->arg_count; i++)
            arg_buf[i] = JS_UNDEFINED;
        sf->arg_count = b->arg_count;
    }
    var_buf = local_buf + arg_allocated_size;
    sf->var_buf = var_buf;
    sf->arg_buf = arg_buf;

    for(i = 0; i < b->var_count; i++)
        var_buf[i] = JS_UNDEFINED;

    stack_buf = var_buf + b->var_count;
    sp = stack_buf;
    pc = b->byte_code_buf;
    sf->prev_frame = rt->current_stack_frame;
    rt->current_stack_frame = sf;
    ctx = b->realm; /* set the current realm */
    
 restart:
    for(;;) {
        int call_argc;
        JSValue *call_argv;

        SWITCH(pc) {
        CASE(OP_push_i32):
            *sp++ = JS_NewInt32(ctx, get_u32(pc));
            pc += 4;
            BREAK;
        CASE(OP_push_const):
            *sp++ = JS_DupValue(ctx, b->cpool[get_u32(pc)]);
            pc += 4;
            BREAK;
#if SHORT_OPCODES
        CASE(OP_push_minus1):
        CASE(OP_push_0):
        CASE(OP_push_1):
        CASE(OP_push_2):
        CASE(OP_push_3):
        CASE(OP_push_4):
        CASE(OP_push_5):
        CASE(OP_push_6):
        CASE(OP_push_7):
            *sp++ = JS_NewInt32(ctx, opcode - OP_push_0);
            BREAK;
        CASE(OP_push_i8):
            *sp++ = JS_NewInt32(ctx, get_i8(pc));
            pc += 1;
            BREAK;
        CASE(OP_push_i16):
            *sp++ = JS_NewInt32(ctx, get_i16(pc));
            pc += 2;
            BREAK;
        CASE(OP_push_const8):
            *sp++ = JS_DupValue(ctx, b->cpool[*pc++]);
            BREAK;
        CASE(OP_fclosure8):
            *sp++ = js_closure(ctx, JS_DupValue(ctx, b->cpool[*pc++]), var_refs, sf);
            if (unlikely(JS_IsException(sp[-1])))
                goto exception;
            BREAK;
        CASE(OP_push_empty_string):
            *sp++ = JS_AtomToString(ctx, JS_ATOM_empty_string);
            BREAK;
        CASE(OP_get_length):
            {
                JSValue val;

                val = JS_GetProperty(ctx, sp[-1], JS_ATOM_length);
                if (unlikely(JS_IsException(val)))
                    goto exception;
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = val;
            }
            BREAK;
#endif
        CASE(OP_push_atom_value):
            *sp++ = JS_AtomToValue(ctx, get_u32(pc));
            pc += 4;
            BREAK;
        CASE(OP_undefined):
            *sp++ = JS_UNDEFINED;
            BREAK;
        CASE(OP_null):
            *sp++ = JS_NULL;
            BREAK;
        CASE(OP_push_this):
            /* OP_push_this is only called at the start of a function */
            {
                JSValue val;
                if (!(b->js_mode & JS_MODE_STRICT)) {
                    uint32_t tag = JS_VALUE_GET_TAG(this_obj);
                    if (likely(tag == JS_TAG_OBJECT))
                        goto normal_this;
                    if (tag == JS_TAG_NULL || tag == JS_TAG_UNDEFINED) {
                        val = JS_DupValue(ctx, ctx->global_obj);
                    } else {
                        val = JS_ToObject(ctx, this_obj);
                        if (JS_IsException(val))
                            goto exception;
                    }
                } else {
                normal_this:
                    val = JS_DupValue(ctx, this_obj);
                }
                *sp++ = val;
            }
            BREAK;
        CASE(OP_push_false):
            *sp++ = JS_FALSE;
            BREAK;
        CASE(OP_push_true):
            *sp++ = JS_TRUE;
            BREAK;
        CASE(OP_object):
            *sp++ = JS_NewObject(ctx);
            if (unlikely(JS_IsException(sp[-1])))
                goto exception;
            BREAK;
        CASE(OP_special_object):
            {
                int arg = *pc++;
                switch(arg) {
                case OP_SPECIAL_OBJECT_ARGUMENTS:
                    *sp++ = js_build_arguments(ctx, argc, (JSValueConst *)argv);
                    if (unlikely(JS_IsException(sp[-1])))
                        goto exception;
                    break;
                case OP_SPECIAL_OBJECT_MAPPED_ARGUMENTS:
                    *sp++ = js_build_mapped_arguments(ctx, argc, (JSValueConst *)argv,
                                                      sf, min_int(argc, b->arg_count));
                    if (unlikely(JS_IsException(sp[-1])))
                        goto exception;
                    break;
                case OP_SPECIAL_OBJECT_THIS_FUNC:
                    *sp++ = JS_DupValue(ctx, sf->cur_func);
                    break;
                case OP_SPECIAL_OBJECT_NEW_TARGET:
                    *sp++ = JS_DupValue(ctx, new_target);
                    break;
                case OP_SPECIAL_OBJECT_HOME_OBJECT:
                    {
                        JSObject *p1;
                        p1 = p->u.func.home_object;
                        if (unlikely(!p1))
                            *sp++ = JS_UNDEFINED;
                        else
                            *sp++ = JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, p1));
                    }
                    break;
                case OP_SPECIAL_OBJECT_VAR_OBJECT:
                    *sp++ = JS_NewObjectProto(ctx, JS_NULL);
                    if (unlikely(JS_IsException(sp[-1])))
                        goto exception;
                    break;
                case OP_SPECIAL_OBJECT_IMPORT_META:
                    *sp++ = js_import_meta(ctx);
                    if (unlikely(JS_IsException(sp[-1])))
                        goto exception;
                    break;
                default:
                    abort();
                }
            }
            BREAK;
        CASE(OP_rest):
            {
                int first = get_u16(pc);
                pc += 2;
                *sp++ = js_build_rest(ctx, first, argc, (JSValueConst *)argv);
                if (unlikely(JS_IsException(sp[-1])))
                    goto exception;
            }
            BREAK;

        CASE(OP_drop):
            JS_FreeValue(ctx, sp[-1]);
            sp--;
            BREAK;
        CASE(OP_nip):
            JS_FreeValue(ctx, sp[-2]);
            sp[-2] = sp[-1];
            sp--;
            BREAK;
        CASE(OP_nip1): /* a b c -> b c */
            JS_FreeValue(ctx, sp[-3]);
            sp[-3] = sp[-2];
            sp[-2] = sp[-1];
            sp--;
            BREAK;
        CASE(OP_dup):
            sp[0] = JS_DupValue(ctx, sp[-1]);
            sp++;
            BREAK;
        CASE(OP_dup2): /* a b -> a b a b */
            sp[0] = JS_DupValue(ctx, sp[-2]);
            sp[1] = JS_DupValue(ctx, sp[-1]);
            sp += 2;
            BREAK;
        CASE(OP_dup3): /* a b c -> a b c a b c */
            sp[0] = JS_DupValue(ctx, sp[-3]);
            sp[1] = JS_DupValue(ctx, sp[-2]);
            sp[2] = JS_DupValue(ctx, sp[-1]);
            sp += 3;
            BREAK;
        CASE(OP_dup1): /* a b -> a a b */
            sp[0] = sp[-1];
            sp[-1] = JS_DupValue(ctx, sp[-2]);
            sp++;
            BREAK;
        CASE(OP_insert2): /* obj a -> a obj a (dup_x1) */
            sp[0] = sp[-1];
            sp[-1] = sp[-2];
            sp[-2] = JS_DupValue(ctx, sp[0]);
            sp++;
            BREAK;
        CASE(OP_insert3): /* obj prop a -> a obj prop a (dup_x2) */
            sp[0] = sp[-1];
            sp[-1] = sp[-2];
            sp[-2] = sp[-3];
            sp[-3] = JS_DupValue(ctx, sp[0]);
            sp++;
            BREAK;
        CASE(OP_insert4): /* this obj prop a -> a this obj prop a */
            sp[0] = sp[-1];
            sp[-1] = sp[-2];
            sp[-2] = sp[-3];
            sp[-3] = sp[-4];
            sp[-4] = JS_DupValue(ctx, sp[0]);
            sp++;
            BREAK;
        CASE(OP_perm3): /* obj a b -> a obj b (213) */
            {
                JSValue tmp;
                tmp = sp[-2];
                sp[-2] = sp[-3];
                sp[-3] = tmp;
            }
            BREAK;
        CASE(OP_rot3l): /* x a b -> a b x (231) */
            {
                JSValue tmp;
                tmp = sp[-3];
                sp[-3] = sp[-2];
                sp[-2] = sp[-1];
                sp[-1] = tmp;
            }
            BREAK;
        CASE(OP_rot4l): /* x a b c -> a b c x */
            {
                JSValue tmp;
                tmp = sp[-4];
                sp[-4] = sp[-3];
                sp[-3] = sp[-2];
                sp[-2] = sp[-1];
                sp[-1] = tmp;
            }
            BREAK;
        CASE(OP_rot5l): /* x a b c d -> a b c d x */
            {
                JSValue tmp;
                tmp = sp[-5];
                sp[-5] = sp[-4];
                sp[-4] = sp[-3];
                sp[-3] = sp[-2];
                sp[-2] = sp[-1];
                sp[-1] = tmp;
            }
            BREAK;
        CASE(OP_rot3r): /* a b x -> x a b (312) */
            {
                JSValue tmp;
                tmp = sp[-1];
                sp[-1] = sp[-2];
                sp[-2] = sp[-3];
                sp[-3] = tmp;
            }
            BREAK;
        CASE(OP_perm4): /* obj prop a b -> a obj prop b */
            {
                JSValue tmp;
                tmp = sp[-2];
                sp[-2] = sp[-3];
                sp[-3] = sp[-4];
                sp[-4] = tmp;
            }
            BREAK;
        CASE(OP_perm5): /* this obj prop a b -> a this obj prop b */
            {
                JSValue tmp;
                tmp = sp[-2];
                sp[-2] = sp[-3];
                sp[-3] = sp[-4];
                sp[-4] = sp[-5];
                sp[-5] = tmp;
            }
            BREAK;
        CASE(OP_swap): /* a b -> b a */
            {
                JSValue tmp;
                tmp = sp[-2];
                sp[-2] = sp[-1];
                sp[-1] = tmp;
            }
            BREAK;
        CASE(OP_swap2): /* a b c d -> c d a b */
            {
                JSValue tmp1, tmp2;
                tmp1 = sp[-4];
                tmp2 = sp[-3];
                sp[-4] = sp[-2];
                sp[-3] = sp[-1];
                sp[-2] = tmp1;
                sp[-1] = tmp2;
            }
            BREAK;

        CASE(OP_fclosure):
            {
                JSValue bfunc = JS_DupValue(ctx, b->cpool[get_u32(pc)]);
                pc += 4;
                *sp++ = js_closure(ctx, bfunc, var_refs, sf);
                if (unlikely(JS_IsException(sp[-1])))
                    goto exception;
            }
            BREAK;
#if SHORT_OPCODES
        CASE(OP_call0):
        CASE(OP_call1):
        CASE(OP_call2):
        CASE(OP_call3):
            call_argc = opcode - OP_call0;
            goto has_call_argc;
#endif
        CASE(OP_call):
        CASE(OP_tail_call):
            {
                call_argc = get_u16(pc);
                pc += 2;
                goto has_call_argc;
            has_call_argc:
                call_argv = sp - call_argc;
                sf->cur_pc = pc;
                ret_val = JS_CallInternal(ctx, call_argv[-1], JS_UNDEFINED,
                                          JS_UNDEFINED, call_argc, call_argv, 0);
                if (unlikely(JS_IsException(ret_val)))
                    goto exception;
                if (opcode == OP_tail_call)
                    goto done;
                for(i = -1; i < call_argc; i++)
                    JS_FreeValue(ctx, call_argv[i]);
                sp -= call_argc + 1;
                *sp++ = ret_val;
            }
            BREAK;
        CASE(OP_call_constructor):
            {
                call_argc = get_u16(pc);
                pc += 2;
                call_argv = sp - call_argc;
                sf->cur_pc = pc;
                ret_val = JS_CallConstructorInternal(ctx, call_argv[-2],
                                                     call_argv[-1],
                                                     call_argc, call_argv, 0);
                if (unlikely(JS_IsException(ret_val)))
                    goto exception;
                for(i = -2; i < call_argc; i++)
                    JS_FreeValue(ctx, call_argv[i]);
                sp -= call_argc + 2;
                *sp++ = ret_val;
            }
            BREAK;
        CASE(OP_call_method):
        CASE(OP_tail_call_method):
            {
                call_argc = get_u16(pc);
                pc += 2;
                call_argv = sp - call_argc;
                sf->cur_pc = pc;
                ret_val = JS_CallInternal(ctx, call_argv[-1], call_argv[-2],
                                          JS_UNDEFINED, call_argc, call_argv, 0);
                if (unlikely(JS_IsException(ret_val)))
                    goto exception;
                if (opcode == OP_tail_call_method)
                    goto done;
                for(i = -2; i < call_argc; i++)
                    JS_FreeValue(ctx, call_argv[i]);
                sp -= call_argc + 2;
                *sp++ = ret_val;
            }
            BREAK;
        CASE(OP_array_from):
            {
                int i, ret;

                call_argc = get_u16(pc);
                pc += 2;
                ret_val = JS_NewArray(ctx);
                if (unlikely(JS_IsException(ret_val)))
                    goto exception;
                call_argv = sp - call_argc;
                for(i = 0; i < call_argc; i++) {
                    ret = JS_DefinePropertyValue(ctx, ret_val, __JS_AtomFromUInt32(i), call_argv[i],
                                                 JS_PROP_C_W_E | JS_PROP_THROW);
                    call_argv[i] = JS_UNDEFINED;
                    if (ret < 0) {
                        JS_FreeValue(ctx, ret_val);
                        goto exception;
                    }
                }
                sp -= call_argc;
                *sp++ = ret_val;
            }
            BREAK;

        CASE(OP_apply):
            {
                int magic;
                magic = get_u16(pc);
                pc += 2;

                ret_val = js_function_apply(ctx, sp[-3], 2, (JSValueConst *)&sp[-2], magic);
                if (unlikely(JS_IsException(ret_val)))
                    goto exception;
                JS_FreeValue(ctx, sp[-3]);
                JS_FreeValue(ctx, sp[-2]);
                JS_FreeValue(ctx, sp[-1]);
                sp -= 3;
                *sp++ = ret_val;
            }
            BREAK;
        CASE(OP_return):
            ret_val = *--sp;
            goto done;
        CASE(OP_return_undef):
            ret_val = JS_UNDEFINED;
            goto done;

        CASE(OP_check_ctor_return):
            /* return TRUE if 'this' should be returned */
            if (!JS_IsObject(sp[-1])) {
                if (!JS_IsUndefined(sp[-1])) {
                    JS_ThrowTypeError(caller_ctx, "derived class constructor must return an object or undefined");
                    goto exception;
                }
                sp[0] = JS_TRUE;
            } else {
                sp[0] = JS_FALSE;
            }
            sp++;
            BREAK;
        CASE(OP_check_ctor):
            if (JS_IsUndefined(new_target)) {
                JS_ThrowTypeError(ctx, "class constructors must be invoked with 'new'");
                goto exception;
            }
            BREAK;
        CASE(OP_check_brand):
            if (JS_CheckBrand(ctx, sp[-2], sp[-1]) < 0)
                goto exception;
            BREAK;
        CASE(OP_add_brand):
            if (JS_AddBrand(ctx, sp[-2], sp[-1]) < 0)
                goto exception;
            JS_FreeValue(ctx, sp[-2]);
            JS_FreeValue(ctx, sp[-1]);
            sp -= 2;
            BREAK;
            
        CASE(OP_throw):
            JS_Throw(ctx, *--sp);
            goto exception;

        CASE(OP_throw_error):
#define JS_THROW_VAR_RO             0
#define JS_THROW_VAR_REDECL         1
#define JS_THROW_VAR_UNINITIALIZED  2
#define JS_THROW_ERROR_DELETE_SUPER   3
#define JS_THROW_ERROR_ITERATOR_THROW 4
            {
                JSAtom atom;
                int type;
                atom = get_u32(pc);
                type = pc[4];
                pc += 5;
                if (type == JS_THROW_VAR_RO)
                    JS_ThrowTypeErrorReadOnly(ctx, JS_PROP_THROW, atom);
                else
                if (type == JS_THROW_VAR_REDECL)
                    JS_ThrowSyntaxErrorVarRedeclaration(ctx, atom);
                else
                if (type == JS_THROW_VAR_UNINITIALIZED)
                    JS_ThrowReferenceErrorUninitialized(ctx, atom);
                else
                if (type == JS_THROW_ERROR_DELETE_SUPER)
                    JS_ThrowReferenceError(ctx, "unsupported reference to 'super'");
                else
                if (type == JS_THROW_ERROR_ITERATOR_THROW)
                    JS_ThrowTypeError(ctx, "iterator does not have a throw method");
                else
                    JS_ThrowInternalError(ctx, "invalid throw var type %d", type);
            }
            goto exception;

        CASE(OP_eval):
            {
                JSValueConst obj;
                int scope_idx;
                call_argc = get_u16(pc);
                scope_idx = get_u16(pc + 2) - 1;
                pc += 4;
                call_argv = sp - call_argc;
                sf->cur_pc = pc;
                if (js_same_value(ctx, call_argv[-1], ctx->eval_obj)) {
                    if (call_argc >= 1)
                        obj = call_argv[0];
                    else
                        obj = JS_UNDEFINED;
                    ret_val = JS_EvalObject(ctx, JS_UNDEFINED, obj,
                                            JS_EVAL_TYPE_DIRECT, scope_idx);
                } else {
                    ret_val = JS_CallInternal(ctx, call_argv[-1], JS_UNDEFINED,
                                              JS_UNDEFINED, call_argc, call_argv, 0);
                }
                if (unlikely(JS_IsException(ret_val)))
                    goto exception;
                for(i = -1; i < call_argc; i++)
                    JS_FreeValue(ctx, call_argv[i]);
                sp -= call_argc + 1;
                *sp++ = ret_val;
            }
            BREAK;
            /* could merge with OP_apply */
        CASE(OP_apply_eval):
            {
                int scope_idx;
                uint32_t len;
                JSValue *tab;
                JSValueConst obj;

                scope_idx = get_u16(pc) - 1;
                pc += 2;
                tab = build_arg_list(ctx, &len, sp[-1]);
                if (!tab)
                    goto exception;
                if (js_same_value(ctx, sp[-2], ctx->eval_obj)) {
                    if (len >= 1)
                        obj = tab[0];
                    else
                        obj = JS_UNDEFINED;
                    ret_val = JS_EvalObject(ctx, JS_UNDEFINED, obj,
                                            JS_EVAL_TYPE_DIRECT, scope_idx);
                } else {
                    ret_val = JS_Call(ctx, sp[-2], JS_UNDEFINED, len,
                                      (JSValueConst *)tab);
                }
                free_arg_list(ctx, tab, len);
                if (unlikely(JS_IsException(ret_val)))
                    goto exception;
                JS_FreeValue(ctx, sp[-2]);
                JS_FreeValue(ctx, sp[-1]);
                sp -= 2;
                *sp++ = ret_val;
            }
            BREAK;

        CASE(OP_regexp):
            {
                sp[-2] = js_regexp_constructor_internal(ctx, JS_UNDEFINED,
                                                        sp[-2], sp[-1]);
                sp--;
            }
            BREAK;

        CASE(OP_get_super):
            {
                JSValue proto;
                proto = JS_GetPrototype(ctx, sp[-1]);
                if (JS_IsException(proto))
                    goto exception;
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = proto;
            }
            BREAK;

        CASE(OP_import):
            {
                JSValue val;
                val = js_dynamic_import(ctx, sp[-1]);
                if (JS_IsException(val))
                    goto exception;
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = val;
            }
            BREAK;

        CASE(OP_check_var):
            {
                int ret;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;

                ret = JS_CheckGlobalVar(ctx, atom);
                if (ret < 0)
                    goto exception;
                *sp++ = JS_NewBool(ctx, ret);
            }
            BREAK;

        CASE(OP_get_var_undef):
        CASE(OP_get_var):
            {
                JSValue val;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;

                val = JS_GetGlobalVar(ctx, atom, opcode - OP_get_var_undef);
                if (unlikely(JS_IsException(val)))
                    goto exception;
                *sp++ = val;
            }
            BREAK;

        CASE(OP_put_var):
        CASE(OP_put_var_init):
            {
                int ret;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;

                ret = JS_SetGlobalVar(ctx, atom, sp[-1], opcode - OP_put_var);
                sp--;
                if (unlikely(ret < 0))
                    goto exception;
            }
            BREAK;

        CASE(OP_put_var_strict):
            {
                int ret;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;

                /* sp[-2] is JS_TRUE or JS_FALSE */
                if (unlikely(!JS_VALUE_GET_INT(sp[-2]))) {
                    JS_ThrowReferenceErrorNotDefined(ctx, atom);
                    goto exception;
                }
                ret = JS_SetGlobalVar(ctx, atom, sp[-1], 2);
                sp -= 2;
                if (unlikely(ret < 0))
                    goto exception;
            }
            BREAK;

        CASE(OP_check_define_var):
            {
                JSAtom atom;
                int flags;
                atom = get_u32(pc);
                flags = pc[4];
                pc += 5;
                if (JS_CheckDefineGlobalVar(ctx, atom, flags))
                    goto exception;
            }
            BREAK;
        CASE(OP_define_var):
            {
                JSAtom atom;
                int flags;
                atom = get_u32(pc);
                flags = pc[4];
                pc += 5;
                if (JS_DefineGlobalVar(ctx, atom, flags))
                    goto exception;
            }
            BREAK;
        CASE(OP_define_func):
            {
                JSAtom atom;
                int flags;
                atom = get_u32(pc);
                flags = pc[4];
                pc += 5;
                if (JS_DefineGlobalFunction(ctx, atom, sp[-1], flags))
                    goto exception;
                JS_FreeValue(ctx, sp[-1]);
                sp--;
            }
            BREAK;

        CASE(OP_get_loc):
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                sp[0] = JS_DupValue(ctx, var_buf[idx]);
                sp++;
            }
            BREAK;
        CASE(OP_put_loc):
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                set_value(ctx, &var_buf[idx], sp[-1]);
                sp--;
            }
            BREAK;
        CASE(OP_set_loc):
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                set_value(ctx, &var_buf[idx], JS_DupValue(ctx, sp[-1]));
            }
            BREAK;
        CASE(OP_get_arg):
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                sp[0] = JS_DupValue(ctx, arg_buf[idx]);
                sp++;
            }
            BREAK;
        CASE(OP_put_arg):
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                set_value(ctx, &arg_buf[idx], sp[-1]);
                sp--;
            }
            BREAK;
        CASE(OP_set_arg):
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                set_value(ctx, &arg_buf[idx], JS_DupValue(ctx, sp[-1]));
            }
            BREAK;

#if SHORT_OPCODES
        CASE(OP_get_loc8): *sp++ = JS_DupValue(ctx, var_buf[*pc++]); BREAK;
        CASE(OP_put_loc8): set_value(ctx, &var_buf[*pc++], *--sp); BREAK;
        CASE(OP_set_loc8): set_value(ctx, &var_buf[*pc++], JS_DupValue(ctx, sp[-1])); BREAK;

        CASE(OP_get_loc0): *sp++ = JS_DupValue(ctx, var_buf[0]); BREAK;
        CASE(OP_get_loc1): *sp++ = JS_DupValue(ctx, var_buf[1]); BREAK;
        CASE(OP_get_loc2): *sp++ = JS_DupValue(ctx, var_buf[2]); BREAK;
        CASE(OP_get_loc3): *sp++ = JS_DupValue(ctx, var_buf[3]); BREAK;
        CASE(OP_put_loc0): set_value(ctx, &var_buf[0], *--sp); BREAK;
        CASE(OP_put_loc1): set_value(ctx, &var_buf[1], *--sp); BREAK;
        CASE(OP_put_loc2): set_value(ctx, &var_buf[2], *--sp); BREAK;
        CASE(OP_put_loc3): set_value(ctx, &var_buf[3], *--sp); BREAK;
        CASE(OP_set_loc0): set_value(ctx, &var_buf[0], JS_DupValue(ctx, sp[-1])); BREAK;
        CASE(OP_set_loc1): set_value(ctx, &var_buf[1], JS_DupValue(ctx, sp[-1])); BREAK;
        CASE(OP_set_loc2): set_value(ctx, &var_buf[2], JS_DupValue(ctx, sp[-1])); BREAK;
        CASE(OP_set_loc3): set_value(ctx, &var_buf[3], JS_DupValue(ctx, sp[-1])); BREAK;
        CASE(OP_get_arg0): *sp++ = JS_DupValue(ctx, arg_buf[0]); BREAK;
        CASE(OP_get_arg1): *sp++ = JS_DupValue(ctx, arg_buf[1]); BREAK;
        CASE(OP_get_arg2): *sp++ = JS_DupValue(ctx, arg_buf[2]); BREAK;
        CASE(OP_get_arg3): *sp++ = JS_DupValue(ctx, arg_buf[3]); BREAK;
        CASE(OP_put_arg0): set_value(ctx, &arg_buf[0], *--sp); BREAK;
        CASE(OP_put_arg1): set_value(ctx, &arg_buf[1], *--sp); BREAK;
        CASE(OP_put_arg2): set_value(ctx, &arg_buf[2], *--sp); BREAK;
        CASE(OP_put_arg3): set_value(ctx, &arg_buf[3], *--sp); BREAK;
        CASE(OP_set_arg0): set_value(ctx, &arg_buf[0], JS_DupValue(ctx, sp[-1])); BREAK;
        CASE(OP_set_arg1): set_value(ctx, &arg_buf[1], JS_DupValue(ctx, sp[-1])); BREAK;
        CASE(OP_set_arg2): set_value(ctx, &arg_buf[2], JS_DupValue(ctx, sp[-1])); BREAK;
        CASE(OP_set_arg3): set_value(ctx, &arg_buf[3], JS_DupValue(ctx, sp[-1])); BREAK;
        CASE(OP_get_var_ref0): *sp++ = JS_DupValue(ctx, *var_refs[0]->pvalue); BREAK;
        CASE(OP_get_var_ref1): *sp++ = JS_DupValue(ctx, *var_refs[1]->pvalue); BREAK;
        CASE(OP_get_var_ref2): *sp++ = JS_DupValue(ctx, *var_refs[2]->pvalue); BREAK;
        CASE(OP_get_var_ref3): *sp++ = JS_DupValue(ctx, *var_refs[3]->pvalue); BREAK;
        CASE(OP_put_var_ref0): set_value(ctx, var_refs[0]->pvalue, *--sp); BREAK;
        CASE(OP_put_var_ref1): set_value(ctx, var_refs[1]->pvalue, *--sp); BREAK;
        CASE(OP_put_var_ref2): set_value(ctx, var_refs[2]->pvalue, *--sp); BREAK;
        CASE(OP_put_var_ref3): set_value(ctx, var_refs[3]->pvalue, *--sp); BREAK;
        CASE(OP_set_var_ref0): set_value(ctx, var_refs[0]->pvalue, JS_DupValue(ctx, sp[-1])); BREAK;
        CASE(OP_set_var_ref1): set_value(ctx, var_refs[1]->pvalue, JS_DupValue(ctx, sp[-1])); BREAK;
        CASE(OP_set_var_ref2): set_value(ctx, var_refs[2]->pvalue, JS_DupValue(ctx, sp[-1])); BREAK;
        CASE(OP_set_var_ref3): set_value(ctx, var_refs[3]->pvalue, JS_DupValue(ctx, sp[-1])); BREAK;
#endif

        CASE(OP_get_var_ref):
            {
                int idx;
                JSValue val;
                idx = get_u16(pc);
                pc += 2;
                val = *var_refs[idx]->pvalue;
                sp[0] = JS_DupValue(ctx, val);
                sp++;
            }
            BREAK;
        CASE(OP_put_var_ref):
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                set_value(ctx, var_refs[idx]->pvalue, sp[-1]);
                sp--;
            }
            BREAK;
        CASE(OP_set_var_ref):
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                set_value(ctx, var_refs[idx]->pvalue, JS_DupValue(ctx, sp[-1]));
            }
            BREAK;
        CASE(OP_get_var_ref_check):
            {
                int idx;
                JSValue val;
                idx = get_u16(pc);
                pc += 2;
                val = *var_refs[idx]->pvalue;
                if (unlikely(JS_IsUninitialized(val))) {
                    JS_ThrowReferenceErrorUninitialized2(ctx, b, idx, TRUE);
                    goto exception;
                }
                sp[0] = JS_DupValue(ctx, val);
                sp++;
            }
            BREAK;
        CASE(OP_put_var_ref_check):
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                if (unlikely(JS_IsUninitialized(*var_refs[idx]->pvalue))) {
                    JS_ThrowReferenceErrorUninitialized2(ctx, b, idx, TRUE);
                    goto exception;
                }
                set_value(ctx, var_refs[idx]->pvalue, sp[-1]);
                sp--;
            }
            BREAK;
        CASE(OP_put_var_ref_check_init):
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                if (unlikely(!JS_IsUninitialized(*var_refs[idx]->pvalue))) {
                    JS_ThrowReferenceErrorUninitialized2(ctx, b, idx, TRUE);
                    goto exception;
                }
                set_value(ctx, var_refs[idx]->pvalue, sp[-1]);
                sp--;
            }
            BREAK;
        CASE(OP_set_loc_uninitialized):
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                set_value(ctx, &var_buf[idx], JS_UNINITIALIZED);
            }
            BREAK;
        CASE(OP_get_loc_check):
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                if (unlikely(JS_IsUninitialized(var_buf[idx]))) {
                    JS_ThrowReferenceErrorUninitialized2(ctx, b, idx, FALSE);
                    goto exception;
                }
                sp[0] = JS_DupValue(ctx, var_buf[idx]);
                sp++;
            }
            BREAK;
        CASE(OP_put_loc_check):
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                if (unlikely(JS_IsUninitialized(var_buf[idx]))) {
                    JS_ThrowReferenceErrorUninitialized2(ctx, b, idx, FALSE);
                    goto exception;
                }
                set_value(ctx, &var_buf[idx], sp[-1]);
                sp--;
            }
            BREAK;
        CASE(OP_put_loc_check_init):
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                if (unlikely(!JS_IsUninitialized(var_buf[idx]))) {
                    JS_ThrowReferenceError(ctx, "'this' can be initialized only once");
                    goto exception;
                }
                set_value(ctx, &var_buf[idx], sp[-1]);
                sp--;
            }
            BREAK;
        CASE(OP_close_loc):
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                close_lexical_var(ctx, sf, idx, FALSE);
            }
            BREAK;

        CASE(OP_make_loc_ref):
        CASE(OP_make_arg_ref):
        CASE(OP_make_var_ref_ref):
            {
                JSVarRef *var_ref;
                JSProperty *pr;
                JSAtom atom;
                int idx;
                atom = get_u32(pc);
                idx = get_u16(pc + 4);
                pc += 6;
                *sp++ = JS_NewObjectProto(ctx, JS_NULL);
                if (unlikely(JS_IsException(sp[-1])))
                    goto exception;
                if (opcode == OP_make_var_ref_ref) {
                    var_ref = var_refs[idx];
                    var_ref->header.ref_count++;
                } else {
                    var_ref = get_var_ref(ctx, sf, idx, opcode == OP_make_arg_ref);
                    if (!var_ref)
                        goto exception;
                }
                pr = add_property(ctx, JS_VALUE_GET_OBJ(sp[-1]), atom,
                                  JS_PROP_WRITABLE | JS_PROP_VARREF);
                if (!pr) {
                    free_var_ref(rt, var_ref);
                    goto exception;
                }
                pr->u.var_ref = var_ref;
                *sp++ = JS_AtomToValue(ctx, atom);
            }
            BREAK;
        CASE(OP_make_var_ref):
            {
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;

                if (JS_GetGlobalVarRef(ctx, atom, sp))
                    goto exception;
                sp += 2;
            }
            BREAK;

        CASE(OP_goto):
            pc += (int32_t)get_u32(pc);
            if (unlikely(js_poll_interrupts(ctx)))
                goto exception;
            BREAK;
#if SHORT_OPCODES
        CASE(OP_goto16):
            pc += (int16_t)get_u16(pc);
            if (unlikely(js_poll_interrupts(ctx)))
                goto exception;
            BREAK;
        CASE(OP_goto8):
            pc += (int8_t)pc[0];
            if (unlikely(js_poll_interrupts(ctx)))
                goto exception;
            BREAK;
#endif
        CASE(OP_if_true):
            {
                int res;
                JSValue op1;

                op1 = sp[-1];
                pc += 4;
                if ((uint32_t)JS_VALUE_GET_TAG(op1) <= JS_TAG_UNDEFINED) {
                    res = JS_VALUE_GET_INT(op1);
                } else {
                    res = JS_ToBoolFree(ctx, op1);
                }
                sp--;
                if (res) {
                    pc += (int32_t)get_u32(pc - 4) - 4;
                }
                if (unlikely(js_poll_interrupts(ctx)))
                    goto exception;
            }
            BREAK;
        CASE(OP_if_false):
            {
                int res;
                JSValue op1;

                op1 = sp[-1];
                pc += 4;
                if ((uint32_t)JS_VALUE_GET_TAG(op1) <= JS_TAG_UNDEFINED) {
                    res = JS_VALUE_GET_INT(op1);
                } else {
                    res = JS_ToBoolFree(ctx, op1);
                }
                sp--;
                if (!res) {
                    pc += (int32_t)get_u32(pc - 4) - 4;
                }
                if (unlikely(js_poll_interrupts(ctx)))
                    goto exception;
            }
            BREAK;
#if SHORT_OPCODES
        CASE(OP_if_true8):
            {
                int res;
                JSValue op1;

                op1 = sp[-1];
                pc += 1;
                if ((uint32_t)JS_VALUE_GET_TAG(op1) <= JS_TAG_UNDEFINED) {
                    res = JS_VALUE_GET_INT(op1);
                } else {
                    res = JS_ToBoolFree(ctx, op1);
                }
                sp--;
                if (res) {
                    pc += (int8_t)pc[-1] - 1;
                }
                if (unlikely(js_poll_interrupts(ctx)))
                    goto exception;
            }
            BREAK;
        CASE(OP_if_false8):
            {
                int res;
                JSValue op1;

                op1 = sp[-1];
                pc += 1;
                if ((uint32_t)JS_VALUE_GET_TAG(op1) <= JS_TAG_UNDEFINED) {
                    res = JS_VALUE_GET_INT(op1);
                } else {
                    res = JS_ToBoolFree(ctx, op1);
                }
                sp--;
                if (!res) {
                    pc += (int8_t)pc[-1] - 1;
                }
                if (unlikely(js_poll_interrupts(ctx)))
                    goto exception;
            }
            BREAK;
#endif
        CASE(OP_catch):
            {
                int32_t diff;
                diff = get_u32(pc);
                sp[0] = JS_NewCatchOffset(ctx, pc + diff - b->byte_code_buf);
                sp++;
                pc += 4;
            }
            BREAK;
        CASE(OP_gosub):
            {
                int32_t diff;
                diff = get_u32(pc);
                /* XXX: should have a different tag to avoid security flaw */
                sp[0] = JS_NewInt32(ctx, pc + 4 - b->byte_code_buf);
                sp++;
                pc += diff;
            }
            BREAK;
        CASE(OP_ret):
            {
                JSValue op1;
                uint32_t pos;
                op1 = sp[-1];
                if (unlikely(JS_VALUE_GET_TAG(op1) != JS_TAG_INT))
                    goto ret_fail;
                pos = JS_VALUE_GET_INT(op1);
                if (unlikely(pos >= b->byte_code_len)) {
                ret_fail:
                    JS_ThrowInternalError(ctx, "invalid ret value");
                    goto exception;
                }
                sp--;
                pc = b->byte_code_buf + pos;
            }
            BREAK;

        CASE(OP_for_in_start):
            if (js_for_in_start(ctx, sp))
                goto exception;
            BREAK;
        CASE(OP_for_in_next):
            if (js_for_in_next(ctx, sp))
                goto exception;
            sp += 2;
            BREAK;
        CASE(OP_for_of_start):
            if (js_for_of_start(ctx, sp, FALSE))
                goto exception;
            sp += 1;
            *sp++ = JS_NewCatchOffset(ctx, 0);
            BREAK;
        CASE(OP_for_of_next):
            {
                int offset = -3 - pc[0];
                pc += 1;
                if (js_for_of_next(ctx, sp, offset))
                    goto exception;
                sp += 2;
            }
            BREAK;
        CASE(OP_for_await_of_start):
            if (js_for_of_start(ctx, sp, TRUE))
                goto exception;
            sp += 1;
            *sp++ = JS_NewCatchOffset(ctx, 0);
            BREAK;
        CASE(OP_iterator_get_value_done):
            if (js_iterator_get_value_done(ctx, sp))
                goto exception;
            sp += 1;
            BREAK;
        CASE(OP_iterator_check_object):
            if (unlikely(!JS_IsObject(sp[-1]))) {
                JS_ThrowTypeError(ctx, "iterator must return an object");
                goto exception;
            }
            BREAK;

        CASE(OP_iterator_close):
            /* iter_obj next catch_offset -> */
            sp--; /* drop the catch offset to avoid getting caught by exception */
            JS_FreeValue(ctx, sp[-1]); /* drop the next method */
            sp--;
            if (!JS_IsUndefined(sp[-1])) {
                if (JS_IteratorClose(ctx, sp[-1], FALSE))
                    goto exception;
                JS_FreeValue(ctx, sp[-1]);
            }
            sp--;
            BREAK;
        CASE(OP_iterator_close_return):
            {
                JSValue ret_val;
                /* iter_obj next catch_offset ... ret_val ->
                   ret_eval iter_obj next catch_offset */
                ret_val = *--sp;
                while (sp > stack_buf &&
                       JS_VALUE_GET_TAG(sp[-1]) != JS_TAG_CATCH_OFFSET) {
                    JS_FreeValue(ctx, *--sp);
                }
                if (unlikely(sp < stack_buf + 3)) {
                    JS_ThrowInternalError(ctx, "iterator_close_return");
                    JS_FreeValue(ctx, ret_val);
                    goto exception;
                }
                sp[0] = sp[-1];
                sp[-1] = sp[-2];
                sp[-2] = sp[-3];
                sp[-3] = ret_val;
                sp++;
            }
            BREAK;

        CASE(OP_iterator_next):
            /* stack: iter_obj next catch_offset val */
            {
                JSValue ret;
                ret = JS_Call(ctx, sp[-3], sp[-4],
                              1, (JSValueConst *)(sp - 1));
                if (JS_IsException(ret))
                    goto exception;
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = ret;
            }
            BREAK;

        CASE(OP_iterator_call):
            /* stack: iter_obj next catch_offset val */
            {
                JSValue method, ret;
                BOOL ret_flag;
                int flags;
                flags = *pc++;
                method = JS_GetProperty(ctx, sp[-4], (flags & 1) ?
                                        JS_ATOM_throw : JS_ATOM_return);
                if (JS_IsException(method))
                    goto exception;
                if (JS_IsUndefined(method) || JS_IsNull(method)) {
                    ret_flag = TRUE;
                } else {
                    if (flags & 2) {
                        /* no argument */
                        ret = JS_CallFree(ctx, method, sp[-4],
                                          0, NULL);
                    } else {
                        ret = JS_CallFree(ctx, method, sp[-4],
                                          1, (JSValueConst *)(sp - 1));
                    }
                    if (JS_IsException(ret))
                        goto exception;
                    JS_FreeValue(ctx, sp[-1]);
                    sp[-1] = ret;
                    ret_flag = FALSE;
                }
                sp[0] = JS_NewBool(ctx, ret_flag);
                sp += 1;
            }
            BREAK;

        CASE(OP_lnot):
            {
                int res;
                JSValue op1;

                op1 = sp[-1];
                if ((uint32_t)JS_VALUE_GET_TAG(op1) <= JS_TAG_UNDEFINED) {
                    res = JS_VALUE_GET_INT(op1) != 0;
                } else {
                    res = JS_ToBoolFree(ctx, op1);
                }
                sp[-1] = JS_NewBool(ctx, !res);
            }
            BREAK;

        CASE(OP_get_field):
            {
                JSValue val;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;

                val = JS_GetProperty(ctx, sp[-1], atom);
                if (unlikely(JS_IsException(val)))
                    goto exception;
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = val;
            }
            BREAK;

        CASE(OP_get_field2):
            {
                JSValue val;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;

                val = JS_GetProperty(ctx, sp[-1], atom);
                if (unlikely(JS_IsException(val)))
                    goto exception;
                *sp++ = val;
            }
            BREAK;

        CASE(OP_put_field):
            {
                int ret;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;

                ret = JS_SetPropertyInternal(ctx, sp[-2], atom, sp[-1],
                                             JS_PROP_THROW_STRICT);
                JS_FreeValue(ctx, sp[-2]);
                sp -= 2;
                if (unlikely(ret < 0))
                    goto exception;
            }
            BREAK;

        CASE(OP_private_symbol):
            {
                JSAtom atom;
                JSValue val;
                
                atom = get_u32(pc);
                pc += 4;
                val = JS_NewSymbolFromAtom(ctx, atom, JS_ATOM_TYPE_PRIVATE);
                if (JS_IsException(val))
                    goto exception;
                *sp++ = val;
            }
            BREAK;
            
        CASE(OP_get_private_field):
            {
                JSValue val;

                val = JS_GetPrivateField(ctx, sp[-2], sp[-1]);
                JS_FreeValue(ctx, sp[-1]);
                JS_FreeValue(ctx, sp[-2]);
                sp[-2] = val;
                sp--;
                if (unlikely(JS_IsException(val)))
                    goto exception;
            }
            BREAK;

        CASE(OP_put_private_field):
            {
                int ret;
                ret = JS_SetPrivateField(ctx, sp[-3], sp[-1], sp[-2]);
                JS_FreeValue(ctx, sp[-3]);
                JS_FreeValue(ctx, sp[-1]);
                sp -= 3;
                if (unlikely(ret < 0))
                    goto exception;
            }
            BREAK;

        CASE(OP_define_private_field):
            {
                int ret;
                ret = JS_DefinePrivateField(ctx, sp[-3], sp[-2], sp[-1]);
                JS_FreeValue(ctx, sp[-2]);
                sp -= 2;
                if (unlikely(ret < 0))
                    goto exception;
            }
            BREAK;

        CASE(OP_define_field):
            {
                int ret;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;

                ret = JS_DefinePropertyValue(ctx, sp[-2], atom, sp[-1],
                                             JS_PROP_C_W_E | JS_PROP_THROW);
                sp--;
                if (unlikely(ret < 0))
                    goto exception;
            }
            BREAK;

        CASE(OP_set_name):
            {
                int ret;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;

                ret = JS_DefineObjectName(ctx, sp[-1], atom, JS_PROP_CONFIGURABLE);
                if (unlikely(ret < 0))
                    goto exception;
            }
            BREAK;
        CASE(OP_set_name_computed):
            {
                int ret;
                ret = JS_DefineObjectNameComputed(ctx, sp[-1], sp[-2], JS_PROP_CONFIGURABLE);
                if (unlikely(ret < 0))
                    goto exception;
            }
            BREAK;
        CASE(OP_set_proto):
            {
                JSValue proto;
                proto = sp[-1];
                if (JS_IsObject(proto) || JS_IsNull(proto)) {
                    if (JS_SetPrototypeInternal(ctx, sp[-2], proto, TRUE) < 0)
                        goto exception;
                }
                JS_FreeValue(ctx, proto);
                sp--;
            }
            BREAK;
        CASE(OP_set_home_object):
            js_method_set_home_object(ctx, sp[-1], sp[-2]);
            BREAK;
        CASE(OP_define_method):
        CASE(OP_define_method_computed):
            {
                JSValue getter, setter, value;
                JSValueConst obj;
                JSAtom atom;
                int flags, ret, op_flags;
                BOOL is_computed;
#define OP_DEFINE_METHOD_METHOD 0
#define OP_DEFINE_METHOD_GETTER 1
#define OP_DEFINE_METHOD_SETTER 2
#define OP_DEFINE_METHOD_ENUMERABLE 4

                is_computed = (opcode == OP_define_method_computed);
                if (is_computed) {
                    atom = JS_ValueToAtom(ctx, sp[-2]);
                    if (unlikely(atom == JS_ATOM_NULL))
                        goto exception;
                    opcode += OP_define_method - OP_define_method_computed;
                } else {
                    atom = get_u32(pc);
                    pc += 4;
                }
                op_flags = *pc++;

                obj = sp[-2 - is_computed];
                flags = JS_PROP_HAS_CONFIGURABLE | JS_PROP_CONFIGURABLE |
                    JS_PROP_HAS_ENUMERABLE | JS_PROP_THROW;
                if (op_flags & OP_DEFINE_METHOD_ENUMERABLE)
                    flags |= JS_PROP_ENUMERABLE;
                op_flags &= 3;
                value = JS_UNDEFINED;
                getter = JS_UNDEFINED;
                setter = JS_UNDEFINED;
                if (op_flags == OP_DEFINE_METHOD_METHOD) {
                    value = sp[-1];
                    flags |= JS_PROP_HAS_VALUE | JS_PROP_HAS_WRITABLE | JS_PROP_WRITABLE;
                } else if (op_flags == OP_DEFINE_METHOD_GETTER) {
                    getter = sp[-1];
                    flags |= JS_PROP_HAS_GET;
                } else {
                    setter = sp[-1];
                    flags |= JS_PROP_HAS_SET;
                }
                ret = js_method_set_properties(ctx, sp[-1], atom, flags, obj);
                if (ret >= 0) {
                    ret = JS_DefineProperty(ctx, obj, atom, value,
                                            getter, setter, flags);
                }
                JS_FreeValue(ctx, sp[-1]);
                if (is_computed) {
                    JS_FreeAtom(ctx, atom);
                    JS_FreeValue(ctx, sp[-2]);
                }
                sp -= 1 + is_computed;
                if (unlikely(ret < 0))
                    goto exception;
            }
            BREAK;

        CASE(OP_define_class):
        CASE(OP_define_class_computed):
            {
                int class_flags;
                JSAtom atom;
                
                atom = get_u32(pc);
                class_flags = pc[4];
                pc += 5;
                if (js_op_define_class(ctx, sp, atom, class_flags,
                                       var_refs, sf,
                                       (opcode == OP_define_class_computed)) < 0)
                    goto exception;
            }
            BREAK;

        CASE(OP_get_array_el):
            {
                JSValue val;

                val = JS_GetPropertyValue(ctx, sp[-2], sp[-1]);
                JS_FreeValue(ctx, sp[-2]);
                sp[-2] = val;
                sp--;
                if (unlikely(JS_IsException(val)))
                    goto exception;
            }
            BREAK;

        CASE(OP_get_array_el2):
            {
                JSValue val;

                val = JS_GetPropertyValue(ctx, sp[-2], sp[-1]);
                sp[-1] = val;
                if (unlikely(JS_IsException(val)))
                    goto exception;
            }
            BREAK;

        CASE(OP_get_ref_value):
            {
                JSValue val;
                if (unlikely(JS_IsUndefined(sp[-2]))) {
                    JSAtom atom = JS_ValueToAtom(ctx, sp[-1]);
                    if (atom != JS_ATOM_NULL) {
                        JS_ThrowReferenceErrorNotDefined(ctx, atom);
                        JS_FreeAtom(ctx, atom);
                    }
                    goto exception;
                }
                val = JS_GetPropertyValue(ctx, sp[-2],
                                          JS_DupValue(ctx, sp[-1]));
                if (unlikely(JS_IsException(val)))
                    goto exception;
                sp[0] = val;
                sp++;
            }
            BREAK;

        CASE(OP_get_super_value):
            {
                JSValue val;
                JSAtom atom;
                atom = JS_ValueToAtom(ctx, sp[-1]);
                if (unlikely(atom == JS_ATOM_NULL))
                    goto exception;
                val = JS_GetPropertyInternal(ctx, sp[-2], atom, sp[-3], FALSE);
                JS_FreeAtom(ctx, atom);
                if (unlikely(JS_IsException(val)))
                    goto exception;
                JS_FreeValue(ctx, sp[-1]);
                JS_FreeValue(ctx, sp[-2]);
                JS_FreeValue(ctx, sp[-3]);
                sp[-3] = val;
                sp -= 2;
            }
            BREAK;

        CASE(OP_put_array_el):
            {
                int ret;

                ret = JS_SetPropertyValue(ctx, sp[-3], sp[-2], sp[-1], JS_PROP_THROW_STRICT);
                JS_FreeValue(ctx, sp[-3]);
                sp -= 3;
                if (unlikely(ret < 0))
                    goto exception;
            }
            BREAK;

        CASE(OP_put_ref_value):
            {
                int ret, flags;
                flags = JS_PROP_THROW_STRICT;
                if (unlikely(JS_IsUndefined(sp[-3]))) {
                    if (is_strict_mode(ctx)) {
                        JSAtom atom = JS_ValueToAtom(ctx, sp[-2]);
                        if (atom != JS_ATOM_NULL) {
                            JS_ThrowReferenceErrorNotDefined(ctx, atom);
                            JS_FreeAtom(ctx, atom);
                        }
                        goto exception;
                    } else {
                        sp[-3] = JS_DupValue(ctx, ctx->global_obj);
                    }
                } else {
                    if (is_strict_mode(ctx))
                        flags |= JS_PROP_NO_ADD;
                }
                ret = JS_SetPropertyValue(ctx, sp[-3], sp[-2], sp[-1], flags);
                JS_FreeValue(ctx, sp[-3]);
                sp -= 3;
                if (unlikely(ret < 0))
                    goto exception;
            }
            BREAK;

        CASE(OP_put_super_value):
            {
                int ret;
                JSAtom atom;
                if (JS_VALUE_GET_TAG(sp[-3]) != JS_TAG_OBJECT) {
                    JS_ThrowTypeErrorNotAnObject(ctx);
                    goto exception;
                }
                atom = JS_ValueToAtom(ctx, sp[-2]);
                if (unlikely(atom == JS_ATOM_NULL))
                    goto exception;
                ret = JS_SetPropertyGeneric(ctx, sp[-3], atom, sp[-1], sp[-4],
                                            JS_PROP_THROW_STRICT);
                JS_FreeAtom(ctx, atom);
                JS_FreeValue(ctx, sp[-4]);
                JS_FreeValue(ctx, sp[-3]);
                JS_FreeValue(ctx, sp[-2]);
                sp -= 4;
                if (ret < 0)
                    goto exception;
            }
            BREAK;

        CASE(OP_define_array_el):
            {
                int ret;
                ret = JS_DefinePropertyValueValue(ctx, sp[-3], JS_DupValue(ctx, sp[-2]), sp[-1],
                                                  JS_PROP_C_W_E | JS_PROP_THROW);
                sp -= 1;
                if (unlikely(ret < 0))
                    goto exception;
            }
            BREAK;

        CASE(OP_append):    /* array pos enumobj -- array pos */
            {
                if (js_append_enumerate(ctx, sp))
                    goto exception;
                JS_FreeValue(ctx, *--sp);
            }
            BREAK;

        CASE(OP_copy_data_properties):    /* target source excludeList */
            {
                /* stack offsets (-1 based):
                   2 bits for target,
                   3 bits for source,
                   2 bits for exclusionList */
                int mask;

                mask = *pc++;
                if (JS_CopyDataProperties(ctx, sp[-1 - (mask & 3)],
                                          sp[-1 - ((mask >> 2) & 7)],
                                          sp[-1 - ((mask >> 5) & 7)], 0))
                    goto exception;
            }
            BREAK;

        CASE(OP_add):
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    int64_t r;
                    r = (int64_t)JS_VALUE_GET_INT(op1) + JS_VALUE_GET_INT(op2);
                    if (unlikely((int)r != r))
                        goto add_slow;
                    sp[-2] = JS_NewInt32(ctx, r);
                    sp--;
                } else if (JS_VALUE_IS_BOTH_FLOAT(op1, op2)) {
                    sp[-2] = __JS_NewFloat64(ctx, JS_VALUE_GET_FLOAT64(op1) +
                                             JS_VALUE_GET_FLOAT64(op2));
                    sp--;
                } else {
                add_slow:
                    if (js_add_slow(ctx, sp))
                        goto exception;
                    sp--;
                }
            }
            BREAK;
        CASE(OP_add_loc):
            {
                JSValue *pv;
                int idx;
                idx = *pc;
                pc += 1;

                pv = &var_buf[idx];
                if (likely(JS_VALUE_IS_BOTH_INT(*pv, sp[-1]))) {
                    int64_t r;
                    r = (int64_t)JS_VALUE_GET_INT(*pv) +
                        JS_VALUE_GET_INT(sp[-1]);
                    if (unlikely((int)r != r))
                        goto add_loc_slow;
                    *pv = JS_NewInt32(ctx, r);
                    sp--;
                } else if (JS_VALUE_GET_TAG(*pv) == JS_TAG_STRING) {
                    JSValue op1;
                    op1 = sp[-1];
                    sp--;
                    op1 = JS_ToPrimitiveFree(ctx, op1, HINT_NONE);
                    if (JS_IsException(op1))
                        goto exception;
                    op1 = JS_ConcatString(ctx, JS_DupValue(ctx, *pv), op1);
                    if (JS_IsException(op1))
                        goto exception;
                    set_value(ctx, pv, op1);
                } else {
                    JSValue ops[2];
                add_loc_slow:
                    /* In case of exception, js_add_slow frees ops[0]
                       and ops[1], so we must duplicate *pv */
                    ops[0] = JS_DupValue(ctx, *pv);
                    ops[1] = sp[-1];
                    sp--;
                    if (js_add_slow(ctx, ops + 2))
                        goto exception;
                    set_value(ctx, pv, ops[0]);
                }
            }
            BREAK;
        CASE(OP_sub):
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    int64_t r;
                    r = (int64_t)JS_VALUE_GET_INT(op1) - JS_VALUE_GET_INT(op2);
                    if (unlikely((int)r != r))
                        goto binary_arith_slow;
                    sp[-2] = JS_NewInt32(ctx, r);
                    sp--;
                } else if (JS_VALUE_IS_BOTH_FLOAT(op1, op2)) {
                    sp[-2] = __JS_NewFloat64(ctx, JS_VALUE_GET_FLOAT64(op1) -
                                             JS_VALUE_GET_FLOAT64(op2));
                    sp--;
                } else {
                    goto binary_arith_slow;
                }
            }
            BREAK;
        CASE(OP_mul):
            {
                JSValue op1, op2;
                double d;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    int32_t v1, v2;
                    int64_t r;
                    v1 = JS_VALUE_GET_INT(op1);
                    v2 = JS_VALUE_GET_INT(op2);
                    r = (int64_t)v1 * v2;
                    if (unlikely((int)r != r)) {
#ifdef CONFIG_BIGNUM
                        if (unlikely(sf->js_mode & JS_MODE_MATH) &&
                            (r < -MAX_SAFE_INTEGER || r > MAX_SAFE_INTEGER))
                            goto binary_arith_slow;
#endif
                        d = (double)r;
                        goto mul_fp_res;
                    }
                    /* need to test zero case for -0 result */
                    if (unlikely(r == 0 && (v1 | v2) < 0)) {
                        d = -0.0;
                        goto mul_fp_res;
                    }
                    sp[-2] = JS_NewInt32(ctx, r);
                    sp--;
                } else if (JS_VALUE_IS_BOTH_FLOAT(op1, op2)) {
#ifdef CONFIG_BIGNUM
                    if (unlikely(sf->js_mode & JS_MODE_MATH))
                        goto binary_arith_slow;
#endif
                    d = JS_VALUE_GET_FLOAT64(op1) * JS_VALUE_GET_FLOAT64(op2);
                mul_fp_res:
                    sp[-2] = __JS_NewFloat64(ctx, d);
                    sp--;
                } else {
                    goto binary_arith_slow;
                }
            }
            BREAK;
        CASE(OP_div):
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    int v1, v2;
                    if (unlikely(sf->js_mode & JS_MODE_MATH))
                        goto binary_arith_slow;
                    v1 = JS_VALUE_GET_INT(op1);
                    v2 = JS_VALUE_GET_INT(op2);
                    sp[-2] = JS_NewFloat64(ctx, (double)v1 / (double)v2);
                    sp--;
                } else {
                    goto binary_arith_slow;
                }
            }
            BREAK;
        CASE(OP_mod):
#ifdef CONFIG_BIGNUM
        CASE(OP_math_mod):
#endif
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    int v1, v2, r;
                    v1 = JS_VALUE_GET_INT(op1);
                    v2 = JS_VALUE_GET_INT(op2);
                    /* We must avoid v2 = 0, v1 = INT32_MIN and v2 =
                       -1 and the cases where the result is -0. */
                    if (unlikely(v1 < 0 || v2 <= 0))
                        goto binary_arith_slow;
                    r = v1 % v2;
                    sp[-2] = JS_NewInt32(ctx, r);
                    sp--;
                } else {
                    goto binary_arith_slow;
                }
            }
            BREAK;
        CASE(OP_pow):
        binary_arith_slow:
            if (js_binary_arith_slow(ctx, sp, opcode))
                goto exception;
            sp--;
            BREAK;

        CASE(OP_plus):
            {
                JSValue op1;
                uint32_t tag;
                op1 = sp[-1];
                tag = JS_VALUE_GET_TAG(op1);
                if (tag == JS_TAG_INT || JS_TAG_IS_FLOAT64(tag)) {
                } else {
                    if (js_unary_arith_slow(ctx, sp, opcode))
                        goto exception;
                }
            }
            BREAK;
        CASE(OP_neg):
            {
                JSValue op1;
                uint32_t tag;
                int val;
                double d;
                op1 = sp[-1];
                tag = JS_VALUE_GET_TAG(op1);
                if (tag == JS_TAG_INT) {
                    val = JS_VALUE_GET_INT(op1);
                    /* Note: -0 cannot be expressed as integer */
                    if (unlikely(val == 0)) {
                        d = -0.0;
                        goto neg_fp_res;
                    }
                    if (unlikely(val == INT32_MIN)) {
                        d = -(double)val;
                        goto neg_fp_res;
                    }
                    sp[-1] = JS_NewInt32(ctx, -val);
                } else if (JS_TAG_IS_FLOAT64(tag)) {
                    d = -JS_VALUE_GET_FLOAT64(op1);
                neg_fp_res:
                    sp[-1] = __JS_NewFloat64(ctx, d);
                } else {
                    if (js_unary_arith_slow(ctx, sp, opcode))
                        goto exception;
                }
            }
            BREAK;
        CASE(OP_inc):
            {
                JSValue op1;
                int val;
                op1 = sp[-1];
                if (JS_VALUE_GET_TAG(op1) == JS_TAG_INT) {
                    val = JS_VALUE_GET_INT(op1);
                    if (unlikely(val == INT32_MAX))
                        goto inc_slow;
                    sp[-1] = JS_NewInt32(ctx, val + 1);
                } else {
                inc_slow:
                    if (js_unary_arith_slow(ctx, sp, opcode))
                        goto exception;
                }
            }
            BREAK;
        CASE(OP_dec):
            {
                JSValue op1;
                int val;
                op1 = sp[-1];
                if (JS_VALUE_GET_TAG(op1) == JS_TAG_INT) {
                    val = JS_VALUE_GET_INT(op1);
                    if (unlikely(val == INT32_MIN))
                        goto dec_slow;
                    sp[-1] = JS_NewInt32(ctx, val - 1);
                } else {
                dec_slow:
                    if (js_unary_arith_slow(ctx, sp, opcode))
                        goto exception;
                }
            }
            BREAK;
        CASE(OP_post_inc):
        CASE(OP_post_dec):
            if (js_post_inc_slow(ctx, sp, opcode))
                goto exception;
            sp++;
            BREAK;
        CASE(OP_inc_loc):
            {
                JSValue op1;
                int val;
                int idx;
                idx = *pc;
                pc += 1;

                op1 = var_buf[idx];
                if (JS_VALUE_GET_TAG(op1) == JS_TAG_INT) {
                    val = JS_VALUE_GET_INT(op1);
                    if (unlikely(val == INT32_MAX))
                        goto inc_loc_slow;
                    var_buf[idx] = JS_NewInt32(ctx, val + 1);
                } else {
                inc_loc_slow:
                    /* must duplicate otherwise the variable value may
                       be destroyed before JS code accesses it */
                    op1 = JS_DupValue(ctx, op1);
                    if (js_unary_arith_slow(ctx, &op1 + 1, OP_inc))
                        goto exception;
                    set_value(ctx, &var_buf[idx], op1);
                }
            }
            BREAK;
        CASE(OP_dec_loc):
            {
                JSValue op1;
                int val;
                int idx;
                idx = *pc;
                pc += 1;

                op1 = var_buf[idx];
                if (JS_VALUE_GET_TAG(op1) == JS_TAG_INT) {
                    val = JS_VALUE_GET_INT(op1);
                    if (unlikely(val == INT32_MIN))
                        goto dec_loc_slow;
                    var_buf[idx] = JS_NewInt32(ctx, val - 1);
                } else {
                dec_loc_slow:
                    /* must duplicate otherwise the variable value may
                       be destroyed before JS code accesses it */
                    op1 = JS_DupValue(ctx, op1);
                    if (js_unary_arith_slow(ctx, &op1 + 1, OP_dec))
                        goto exception;
                    set_value(ctx, &var_buf[idx], op1);
                }
            }
            BREAK;
        CASE(OP_not):
            {
                JSValue op1;
                op1 = sp[-1];
                if (JS_VALUE_GET_TAG(op1) == JS_TAG_INT) {
                    sp[-1] = JS_NewInt32(ctx, ~JS_VALUE_GET_INT(op1));
                } else {
                    if (js_not_slow(ctx, sp))
                        goto exception;
                }
            }
            BREAK;

        CASE(OP_shl):
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    uint32_t v1, v2;
                    v1 = JS_VALUE_GET_INT(op1);
                    v2 = JS_VALUE_GET_INT(op2);
#ifdef CONFIG_BIGNUM
                    {
                        int64_t r;
                        if (unlikely(sf->js_mode & JS_MODE_MATH)) {
                            if (v2 > 0x1f)
                                goto shl_slow;
                            r = (int64_t)v1 << v2;
                            if ((int)r != r)
                                goto shl_slow;
                        } else {
                            v2 &= 0x1f;
                        }
                    }
#else
                    v2 &= 0x1f;
#endif
                    sp[-2] = JS_NewInt32(ctx, v1 << v2);
                    sp--;
                } else {
#ifdef CONFIG_BIGNUM
                shl_slow:
#endif
                    if (js_binary_logic_slow(ctx, sp, opcode))
                        goto exception;
                    sp--;
                }
            }
            BREAK;
        CASE(OP_shr):
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    uint32_t v2;
                    v2 = JS_VALUE_GET_INT(op2);
                    /* v1 >>> v2 retains its JS semantics if CONFIG_BIGNUM */
                    v2 &= 0x1f;
                    sp[-2] = JS_NewUint32(ctx,
                                          (uint32_t)JS_VALUE_GET_INT(op1) >>
                                          v2);
                    sp--;
                } else {
                    if (js_shr_slow(ctx, sp))
                        goto exception;
                    sp--;
                }
            }
            BREAK;
        CASE(OP_sar):
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    uint32_t v2;
                    v2 = JS_VALUE_GET_INT(op2);
#ifdef CONFIG_BIGNUM
                    if (unlikely(v2 > 0x1f)) {
                        if (unlikely(sf->js_mode & JS_MODE_MATH))
                            goto sar_slow;
                        else
                            v2 &= 0x1f;
                    }
#else
                    v2 &= 0x1f;
#endif
                    sp[-2] = JS_NewInt32(ctx,
                                          (int)JS_VALUE_GET_INT(op1) >> v2);
                    sp--;
                } else {
#ifdef CONFIG_BIGNUM
                sar_slow:
#endif
                    if (js_binary_logic_slow(ctx, sp, opcode))
                        goto exception;
                    sp--;
                }
            }
            BREAK;
        CASE(OP_and):
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    sp[-2] = JS_NewInt32(ctx,
                                         JS_VALUE_GET_INT(op1) &
                                         JS_VALUE_GET_INT(op2));
                    sp--;
                } else {
                    if (js_binary_logic_slow(ctx, sp, opcode))
                        goto exception;
                    sp--;
                }
            }
            BREAK;
        CASE(OP_or):
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    sp[-2] = JS_NewInt32(ctx,
                                         JS_VALUE_GET_INT(op1) |
                                         JS_VALUE_GET_INT(op2));
                    sp--;
                } else {
                    if (js_binary_logic_slow(ctx, sp, opcode))
                        goto exception;
                    sp--;
                }
            }
            BREAK;
        CASE(OP_xor):
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    sp[-2] = JS_NewInt32(ctx,
                                         JS_VALUE_GET_INT(op1) ^
                                         JS_VALUE_GET_INT(op2));
                    sp--;
                } else {
                    if (js_binary_logic_slow(ctx, sp, opcode))
                        goto exception;
                    sp--;
                }
            }
            BREAK;


#define OP_CMP(opcode, binary_op, slow_call)              \
            CASE(opcode):                                 \
                {                                         \
                JSValue op1, op2;                         \
                op1 = sp[-2];                             \
                op2 = sp[-1];                                   \
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {           \
                    sp[-2] = JS_NewBool(ctx, JS_VALUE_GET_INT(op1) binary_op JS_VALUE_GET_INT(op2)); \
                    sp--;                                               \
                } else {                                                \
                    if (slow_call)                                      \
                        goto exception;                                 \
                    sp--;                                               \
                }                                                       \
                }                                                       \
            BREAK

            OP_CMP(OP_lt, <, js_relational_slow(ctx, sp, opcode));
            OP_CMP(OP_lte, <=, js_relational_slow(ctx, sp, opcode));
            OP_CMP(OP_gt, >, js_relational_slow(ctx, sp, opcode));
            OP_CMP(OP_gte, >=, js_relational_slow(ctx, sp, opcode));
            OP_CMP(OP_eq, ==, js_eq_slow(ctx, sp, 0));
            OP_CMP(OP_neq, !=, js_eq_slow(ctx, sp, 1));
            OP_CMP(OP_strict_eq, ==, js_strict_eq_slow(ctx, sp, 0));
            OP_CMP(OP_strict_neq, !=, js_strict_eq_slow(ctx, sp, 1));

#ifdef CONFIG_BIGNUM
        CASE(OP_mul_pow10):
            if (rt->bigfloat_ops.mul_pow10(ctx, sp))
                goto exception;
            sp--;
            BREAK;
#endif
        CASE(OP_in):
            if (js_operator_in(ctx, sp))
                goto exception;
            sp--;
            BREAK;
        CASE(OP_instanceof):
            if (js_operator_instanceof(ctx, sp))
                goto exception;
            sp--;
            BREAK;
        CASE(OP_typeof):
            {
                JSValue op1;
                JSAtom atom;

                op1 = sp[-1];
                atom = js_operator_typeof(ctx, op1);
                JS_FreeValue(ctx, op1);
                sp[-1] = JS_AtomToString(ctx, atom);
            }
            BREAK;
        CASE(OP_delete):
            if (js_operator_delete(ctx, sp))
                goto exception;
            sp--;
            BREAK;
        CASE(OP_delete_var):
            {
                JSAtom atom;
                int ret;

                atom = get_u32(pc);
                pc += 4;

                ret = JS_DeleteProperty(ctx, ctx->global_obj, atom, 0);
                if (unlikely(ret < 0))
                    goto exception;
                *sp++ = JS_NewBool(ctx, ret);
            }
            BREAK;

        CASE(OP_to_object):
            if (JS_VALUE_GET_TAG(sp[-1]) != JS_TAG_OBJECT) {
                ret_val = JS_ToObject(ctx, sp[-1]);
                if (JS_IsException(ret_val))
                    goto exception;
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = ret_val;
            }
            BREAK;

        CASE(OP_to_propkey):
            switch (JS_VALUE_GET_TAG(sp[-1])) {
            case JS_TAG_INT:
            case JS_TAG_STRING:
            case JS_TAG_SYMBOL:
                break;
            default:
                ret_val = JS_ToPropertyKey(ctx, sp[-1]);
                if (JS_IsException(ret_val))
                    goto exception;
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = ret_val;
                break;
            }
            BREAK;

        CASE(OP_to_propkey2):
            /* must be tested first */
            if (unlikely(JS_IsUndefined(sp[-2]) || JS_IsNull(sp[-2]))) {
                JS_ThrowTypeError(ctx, "value has no property");
                goto exception;
            }
            switch (JS_VALUE_GET_TAG(sp[-1])) {
            case JS_TAG_INT:
            case JS_TAG_STRING:
            case JS_TAG_SYMBOL:
                break;
            default:
                ret_val = JS_ToPropertyKey(ctx, sp[-1]);
                if (JS_IsException(ret_val))
                    goto exception;
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = ret_val;
                break;
            }
            BREAK;
#if 0
        CASE(OP_to_string):
            if (JS_VALUE_GET_TAG(sp[-1]) != JS_TAG_STRING) {
                ret_val = JS_ToString(ctx, sp[-1]);
                if (JS_IsException(ret_val))
                    goto exception;
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = ret_val;
            }
            BREAK;
#endif
        CASE(OP_with_get_var):
        CASE(OP_with_put_var):
        CASE(OP_with_delete_var):
        CASE(OP_with_make_ref):
        CASE(OP_with_get_ref):
        CASE(OP_with_get_ref_undef):
            {
                JSAtom atom;
                int32_t diff;
                JSValue obj, val;
                int ret, is_with;
                atom = get_u32(pc);
                diff = get_u32(pc + 4);
                is_with = pc[8];
                pc += 9;

                obj = sp[-1];
                ret = JS_HasProperty(ctx, obj, atom);
                if (unlikely(ret < 0))
                    goto exception;
                if (ret) {
                    if (is_with) {
                        ret = js_has_unscopable(ctx, obj, atom);
                        if (unlikely(ret < 0))
                            goto exception;
                        if (ret)
                            goto no_with;
                    }
                    switch (opcode) {
                    case OP_with_get_var:
                        val = JS_GetProperty(ctx, obj, atom);
                        if (unlikely(JS_IsException(val)))
                            goto exception;
                        set_value(ctx, &sp[-1], val);
                        break;
                    case OP_with_put_var:
                        /* XXX: check if strict mode */
                        ret = JS_SetPropertyInternal(ctx, obj, atom, sp[-2],
                                                     JS_PROP_THROW_STRICT);
                        JS_FreeValue(ctx, sp[-1]);
                        sp -= 2;
                        if (unlikely(ret < 0))
                            goto exception;
                        break;
                    case OP_with_delete_var:
                        ret = JS_DeleteProperty(ctx, obj, atom, 0);
                        if (unlikely(ret < 0))
                            goto exception;
                        JS_FreeValue(ctx, sp[-1]);
                        sp[-1] = JS_NewBool(ctx, ret);
                        break;
                    case OP_with_make_ref:
                        /* produce a pair object/propname on the stack */
                        *sp++ = JS_AtomToValue(ctx, atom);
                        break;
                    case OP_with_get_ref:
                        /* produce a pair object/method on the stack */
                        val = JS_GetProperty(ctx, obj, atom);
                        if (unlikely(JS_IsException(val)))
                            goto exception;
                        *sp++ = val;
                        break;
                    case OP_with_get_ref_undef:
                        /* produce a pair undefined/function on the stack */
                        val = JS_GetProperty(ctx, obj, atom);
                        if (unlikely(JS_IsException(val)))
                            goto exception;
                        JS_FreeValue(ctx, sp[-1]);
                        sp[-1] = JS_UNDEFINED;
                        *sp++ = val;
                        break;
                    }
                    pc += diff - 5;
                } else {
                no_with:
                    /* if not jumping, drop the object argument */
                    JS_FreeValue(ctx, sp[-1]);
                    sp--;
                }
            }
            BREAK;

        CASE(OP_await):
            ret_val = JS_NewInt32(ctx, FUNC_RET_AWAIT);
            goto done_generator;
        CASE(OP_yield):
            ret_val = JS_NewInt32(ctx, FUNC_RET_YIELD);
            goto done_generator;
        CASE(OP_yield_star):
        CASE(OP_async_yield_star):
            ret_val = JS_NewInt32(ctx, FUNC_RET_YIELD_STAR);
            goto done_generator;
        CASE(OP_return_async):
        CASE(OP_initial_yield):
            ret_val = JS_UNDEFINED;
            goto done_generator;

        CASE(OP_nop):
            BREAK;
        CASE(OP_is_undefined_or_null):
            if (JS_VALUE_GET_TAG(sp[-1]) == JS_TAG_UNDEFINED ||
                JS_VALUE_GET_TAG(sp[-1]) == JS_TAG_NULL) {
                goto set_true;
            } else {
                goto free_and_set_false;
            }
#if SHORT_OPCODES
        CASE(OP_is_undefined):
            if (JS_VALUE_GET_TAG(sp[-1]) == JS_TAG_UNDEFINED) {
                goto set_true;
            } else {
                goto free_and_set_false;
            }
        CASE(OP_is_null):
            if (JS_VALUE_GET_TAG(sp[-1]) == JS_TAG_NULL) {
                goto set_true;
            } else {
                goto free_and_set_false;
            }
            /* XXX: could merge to a single opcode */
        CASE(OP_typeof_is_undefined):
            /* different from OP_is_undefined because of isHTMLDDA */
            if (js_operator_typeof(ctx, sp[-1]) == JS_ATOM_undefined) {
                goto free_and_set_true;
            } else {
                goto free_and_set_false;
            }
        CASE(OP_typeof_is_function):
            if (js_operator_typeof(ctx, sp[-1]) == JS_ATOM_function) {
                goto free_and_set_true;
            } else {
                goto free_and_set_false;
            }
        free_and_set_true:
            JS_FreeValue(ctx, sp[-1]);
#endif
        set_true:
            sp[-1] = JS_TRUE;
            BREAK;
        free_and_set_false:
            JS_FreeValue(ctx, sp[-1]);
            sp[-1] = JS_FALSE;
            BREAK;
        CASE(OP_invalid):
        DEFAULT:
            JS_ThrowInternalError(ctx, "invalid opcode: pc=%u opcode=0x%02x",
                                  (int)(pc - b->byte_code_buf - 1), opcode);
            goto exception;
        }
    }
 exception:
    if (is_backtrace_needed(ctx, rt->current_exception)) {
        /* add the backtrace information now (it is not done
           before if the exception happens in a bytecode
           operation */
        sf->cur_pc = pc;
        build_backtrace(ctx, rt->current_exception, NULL, 0, 0);
    }
    if (!JS_IsUncatchableError(ctx, rt->current_exception)) {
        while (sp > stack_buf) {
            JSValue val = *--sp;
            JS_FreeValue(ctx, val);
            if (JS_VALUE_GET_TAG(val) == JS_TAG_CATCH_OFFSET) {
                int pos = JS_VALUE_GET_INT(val);
                if (pos == 0) {
                    /* enumerator: close it with a throw */
                    JS_FreeValue(ctx, sp[-1]); /* drop the next method */
                    sp--;
                    JS_IteratorClose(ctx, sp[-1], TRUE);
                } else {
                    *sp++ = rt->current_exception;
                    rt->current_exception = JS_NULL;
                    pc = b->byte_code_buf + pos;
                    goto restart;
                }
            }
        }
    }
    ret_val = JS_EXCEPTION;
    /* the local variables are freed by the caller in the generator
       case. Hence the label 'done' should never be reached in a
       generator function. */
    if (b->func_kind != JS_FUNC_NORMAL) {
    done_generator:
        sf->cur_pc = pc;
        sf->cur_sp = sp;
    } else {
    done:
        if (unlikely(!list_empty(&sf->var_ref_list))) {
            /* variable references reference the stack: must close them */
            close_var_refs(rt, sf);
        }
        /* free the local variables and stack */
        for(pval = local_buf; pval < sp; pval++) {
            JS_FreeValue(ctx, *pval);
        }
    }
    rt->current_stack_frame = sf->prev_frame;
    return ret_val;
}

JSValue JS_Call(JSContext *ctx, JSValueConst func_obj, JSValueConst this_obj,
                int argc, JSValueConst *argv)
{
    return JS_CallInternal(ctx, func_obj, this_obj, JS_UNDEFINED,
                           argc, (JSValue *)argv, JS_CALL_FLAG_COPY_ARGV);
}

static JSValue JS_CallFree(JSContext *ctx, JSValue func_obj, JSValueConst this_obj,
                           int argc, JSValueConst *argv)
{
    JSValue res = JS_CallInternal(ctx, func_obj, this_obj, JS_UNDEFINED,
                                  argc, (JSValue *)argv, JS_CALL_FLAG_COPY_ARGV);
    JS_FreeValue(ctx, func_obj);
    return res;
}

/* warning: the refcount of the context is not incremented. Return
   NULL in case of exception (case of revoked proxy only) */
static JSContext *JS_GetFunctionRealm(JSContext *ctx, JSValueConst func_obj)
{
    JSObject *p;
    JSContext *realm;
    
    if (JS_VALUE_GET_TAG(func_obj) != JS_TAG_OBJECT)
        return ctx;
    p = JS_VALUE_GET_OBJ(func_obj);
    switch(p->class_id) {
    case JS_CLASS_C_FUNCTION:
        realm = p->u.cfunc.realm;
        break;
    case JS_CLASS_BYTECODE_FUNCTION:
    case JS_CLASS_GENERATOR_FUNCTION:
    case JS_CLASS_ASYNC_FUNCTION:
    case JS_CLASS_ASYNC_GENERATOR_FUNCTION:
        {
            JSFunctionBytecode *b;
            b = p->u.func.function_bytecode;
            realm = b->realm;
        }
        break;
    case JS_CLASS_PROXY:
        {
            JSProxyData *s = p->u.opaque;
            if (!s)
                return ctx;
            if (s->is_revoked) {
                JS_ThrowTypeErrorRevokedProxy(ctx);
                return NULL;
            } else {
                realm = JS_GetFunctionRealm(ctx, s->target);
            }
        }
        break;
    case JS_CLASS_BOUND_FUNCTION:
        {
            JSBoundFunction *bf = p->u.bound_function;
            realm = JS_GetFunctionRealm(ctx, bf->func_obj);
        }
        break;
    default:
        realm = ctx;
        break;
    }
    return realm;
}

static JSValue js_create_from_ctor(JSContext *ctx, JSValueConst ctor,
                                   int class_id)
{
    JSValue proto, obj;
    JSContext *realm;
    
    if (JS_IsUndefined(ctor)) {
        proto = JS_DupValue(ctx, ctx->class_proto[class_id]);
    } else {
        proto = JS_GetProperty(ctx, ctor, JS_ATOM_prototype);
        if (JS_IsException(proto))
            return proto;
        if (!JS_IsObject(proto)) {
            JS_FreeValue(ctx, proto);
            realm = JS_GetFunctionRealm(ctx, ctor);
            if (!realm)
                return JS_EXCEPTION;
            proto = JS_DupValue(ctx, realm->class_proto[class_id]);
        }
    }
    obj = JS_NewObjectProtoClass(ctx, proto, class_id);
    JS_FreeValue(ctx, proto);
    return obj;
}

/* argv[] is modified if (flags & JS_CALL_FLAG_COPY_ARGV) = 0. */
static JSValue JS_CallConstructorInternal(JSContext *ctx,
                                          JSValueConst func_obj,
                                          JSValueConst new_target,
                                          int argc, JSValue *argv, int flags)
{
    JSObject *p;
    JSFunctionBytecode *b;

    if (js_poll_interrupts(ctx))
        return JS_EXCEPTION;
    flags |= JS_CALL_FLAG_CONSTRUCTOR;
    if (unlikely(JS_VALUE_GET_TAG(func_obj) != JS_TAG_OBJECT))
        goto not_a_function;
    p = JS_VALUE_GET_OBJ(func_obj);
    if (unlikely(!p->is_constructor))
        return JS_ThrowTypeError(ctx, "not a constructor");
    if (unlikely(p->class_id != JS_CLASS_BYTECODE_FUNCTION)) {
        JSClassCall *call_func;
        call_func = ctx->rt->class_array[p->class_id].call;
        if (!call_func) {
        not_a_function:
            return JS_ThrowTypeError(ctx, "not a function");
        }
        return call_func(ctx, func_obj, new_target, argc,
                         (JSValueConst *)argv, flags);
    }

    b = p->u.func.function_bytecode;
    if (b->is_derived_class_constructor) {
        return JS_CallInternal(ctx, func_obj, JS_UNDEFINED, new_target, argc, argv, flags);
    } else {
        JSValue obj, ret;
        /* legacy constructor behavior */
        obj = js_create_from_ctor(ctx, new_target, JS_CLASS_OBJECT);
        if (JS_IsException(obj))
            return JS_EXCEPTION;
        ret = JS_CallInternal(ctx, func_obj, obj, new_target, argc, argv, flags);
        if (JS_VALUE_GET_TAG(ret) == JS_TAG_OBJECT ||
            JS_IsException(ret)) {
            JS_FreeValue(ctx, obj);
            return ret;
        } else {
            JS_FreeValue(ctx, ret);
            return obj;
        }
    }
}

JSValue JS_CallConstructor2(JSContext *ctx, JSValueConst func_obj,
                            JSValueConst new_target,
                            int argc, JSValueConst *argv)
{
    return JS_CallConstructorInternal(ctx, func_obj, new_target,
                                      argc, (JSValue *)argv,
                                      JS_CALL_FLAG_COPY_ARGV);
}

JSValue JS_CallConstructor(JSContext *ctx, JSValueConst func_obj,
                           int argc, JSValueConst *argv)
{
    return JS_CallConstructorInternal(ctx, func_obj, func_obj,
                                      argc, (JSValue *)argv,
                                      JS_CALL_FLAG_COPY_ARGV);
}

JSValue JS_Invoke(JSContext *ctx, JSValueConst this_val, JSAtom atom,
                  int argc, JSValueConst *argv)
{
    JSValue func_obj;
    func_obj = JS_GetProperty(ctx, this_val, atom);
    if (JS_IsException(func_obj))
        return func_obj;
    return JS_CallFree(ctx, func_obj, this_val, argc, argv);
}

static JSValue JS_InvokeFree(JSContext *ctx, JSValue this_val, JSAtom atom,
                             int argc, JSValueConst *argv)
{
    JSValue res = JS_Invoke(ctx, this_val, atom, argc, argv);
    JS_FreeValue(ctx, this_val);
    return res;
}

/* JSAsyncFunctionState (used by generator and async functions) */
static __exception int async_func_init(JSContext *ctx, JSAsyncFunctionState *s,
                                       JSValueConst func_obj, JSValueConst this_obj,
                                       int argc, JSValueConst *argv)
{
    JSObject *p;
    JSFunctionBytecode *b;
    JSStackFrame *sf;
    int local_count, i, arg_buf_len, n;

    sf = &s->frame;
    init_list_head(&sf->var_ref_list);
    p = JS_VALUE_GET_OBJ(func_obj);
    b = p->u.func.function_bytecode;
    sf->js_mode = b->js_mode;
    sf->cur_pc = b->byte_code_buf;
    arg_buf_len = max_int(b->arg_count, argc);
    local_count = arg_buf_len + b->var_count + b->stack_size;
    sf->arg_buf = js_malloc(ctx, sizeof(JSValue) * max_int(local_count, 1));
    if (!sf->arg_buf)
        return -1;
    sf->cur_func = JS_DupValue(ctx, func_obj);
    s->this_val = JS_DupValue(ctx, this_obj);
    s->argc = argc;
    sf->arg_count = arg_buf_len;
    sf->var_buf = sf->arg_buf + arg_buf_len;
    sf->cur_sp = sf->var_buf + b->var_count;
    for(i = 0; i < argc; i++)
        sf->arg_buf[i] = JS_DupValue(ctx, argv[i]);
    n = arg_buf_len + b->var_count;
    for(i = argc; i < n; i++)
        sf->arg_buf[i] = JS_UNDEFINED;
    return 0;
}

static void async_func_mark(JSRuntime *rt, JSAsyncFunctionState *s,
                            JS_MarkFunc *mark_func)
{
    JSStackFrame *sf;
    JSValue *sp;

    sf = &s->frame;
    JS_MarkValue(rt, sf->cur_func, mark_func);
    JS_MarkValue(rt, s->this_val, mark_func);
    if (sf->cur_sp) {
        /* if the function is running, cur_sp is not known so we
           cannot mark the stack. Marking the variables is not needed
           because a running function cannot be part of a removable
           cycle */
        for(sp = sf->arg_buf; sp < sf->cur_sp; sp++)
            JS_MarkValue(rt, *sp, mark_func);
    }
}

static void async_func_free(JSRuntime *rt, JSAsyncFunctionState *s)
{
    JSStackFrame *sf;
    JSValue *sp;

    sf = &s->frame;

    /* close the closure variables. */
    close_var_refs(rt, sf);
    
    if (sf->arg_buf) {
        /* cannot free the function if it is running */
        assert(sf->cur_sp != NULL);
        for(sp = sf->arg_buf; sp < sf->cur_sp; sp++) {
            JS_FreeValueRT(rt, *sp);
        }
        js_free_rt(rt, sf->arg_buf);
    }
    JS_FreeValueRT(rt, sf->cur_func);
    JS_FreeValueRT(rt, s->this_val);
}

static JSValue async_func_resume(JSContext *ctx, JSAsyncFunctionState *s)
{
    JSValue func_obj;

    if (js_check_stack_overflow(ctx->rt, 0))
        return JS_ThrowStackOverflow(ctx);

    /* the tag does not matter provided it is not an object */
    func_obj = JS_MKPTR(JS_TAG_INT, s);
    return JS_CallInternal(ctx, func_obj, s->this_val, JS_UNDEFINED,
                           s->argc, s->frame.arg_buf, JS_CALL_FLAG_GENERATOR);
}


/* Generators */

typedef enum JSGeneratorStateEnum {
    JS_GENERATOR_STATE_SUSPENDED_START,
    JS_GENERATOR_STATE_SUSPENDED_YIELD,
    JS_GENERATOR_STATE_SUSPENDED_YIELD_STAR,
    JS_GENERATOR_STATE_EXECUTING,
    JS_GENERATOR_STATE_COMPLETED,
} JSGeneratorStateEnum;

typedef struct JSGeneratorData {
    JSGeneratorStateEnum state;
    JSAsyncFunctionState func_state;
} JSGeneratorData;

static void free_generator_stack_rt(JSRuntime *rt, JSGeneratorData *s)
{
    if (s->state == JS_GENERATOR_STATE_COMPLETED)
        return;
    async_func_free(rt, &s->func_state);
    s->state = JS_GENERATOR_STATE_COMPLETED;
}

static void js_generator_finalizer(JSRuntime *rt, JSValue obj)
{
    JSGeneratorData *s = JS_GetOpaque(obj, JS_CLASS_GENERATOR);

    if (s) {
        free_generator_stack_rt(rt, s);
        js_free_rt(rt, s);
    }
}

static void free_generator_stack(JSContext *ctx, JSGeneratorData *s)
{
    free_generator_stack_rt(ctx->rt, s);
}

static void js_generator_mark(JSRuntime *rt, JSValueConst val,
                              JS_MarkFunc *mark_func)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSGeneratorData *s = p->u.generator_data;

    if (!s || s->state == JS_GENERATOR_STATE_COMPLETED)
        return;
    async_func_mark(rt, &s->func_state, mark_func);
}

/* XXX: use enum */
#define GEN_MAGIC_NEXT   0
#define GEN_MAGIC_RETURN 1
#define GEN_MAGIC_THROW  2

static JSValue js_generator_next(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv,
                                 BOOL *pdone, int magic)
{
    JSGeneratorData *s = JS_GetOpaque(this_val, JS_CLASS_GENERATOR);
    JSStackFrame *sf;
    JSValue ret, func_ret;

    *pdone = TRUE;
    if (!s)
        return JS_ThrowTypeError(ctx, "not a generator");
    sf = &s->func_state.frame;
    switch(s->state) {
    default:
    case JS_GENERATOR_STATE_SUSPENDED_START:
        if (magic == GEN_MAGIC_NEXT) {
            goto exec_no_arg;
        } else {
            free_generator_stack(ctx, s);
            goto done;
        }
        break;
    case JS_GENERATOR_STATE_SUSPENDED_YIELD_STAR:
    case JS_GENERATOR_STATE_SUSPENDED_YIELD:
        /* cur_sp[-1] was set to JS_UNDEFINED in the previous call */
        ret = JS_DupValue(ctx, argv[0]);
        if (magic == GEN_MAGIC_THROW &&
            s->state == JS_GENERATOR_STATE_SUSPENDED_YIELD) {
            JS_Throw(ctx, ret);
            s->func_state.throw_flag = TRUE;
        } else {
            sf->cur_sp[-1] = ret;
            sf->cur_sp[0] = JS_NewInt32(ctx, magic);
            sf->cur_sp++;
        exec_no_arg:
            s->func_state.throw_flag = FALSE;
        }
        s->state = JS_GENERATOR_STATE_EXECUTING;
        func_ret = async_func_resume(ctx, &s->func_state);
        s->state = JS_GENERATOR_STATE_SUSPENDED_YIELD;
        if (JS_IsException(func_ret)) {
            /* finalize the execution in case of exception */
            free_generator_stack(ctx, s);
            return func_ret;
        }
        if (JS_VALUE_GET_TAG(func_ret) == JS_TAG_INT) {
            /* get the returned yield value at the top of the stack */
            ret = sf->cur_sp[-1];
            sf->cur_sp[-1] = JS_UNDEFINED;
            if (JS_VALUE_GET_INT(func_ret) == FUNC_RET_YIELD_STAR) {
                s->state = JS_GENERATOR_STATE_SUSPENDED_YIELD_STAR;
                /* return (value, done) object */
                *pdone = 2;
            } else {
                *pdone = FALSE;
            }
        } else {
            /* end of iterator */
            ret = sf->cur_sp[-1];
            sf->cur_sp[-1] = JS_UNDEFINED;
            JS_FreeValue(ctx, func_ret);
            free_generator_stack(ctx, s);
        }
        break;
    case JS_GENERATOR_STATE_COMPLETED:
    done:
        /* execution is finished */
        switch(magic) {
        default:
        case GEN_MAGIC_NEXT:
            ret = JS_UNDEFINED;
            break;
        case GEN_MAGIC_RETURN:
            ret = JS_DupValue(ctx, argv[0]);
            break;
        case GEN_MAGIC_THROW:
            ret = JS_Throw(ctx, JS_DupValue(ctx, argv[0]));
            break;
        }
        break;
    case JS_GENERATOR_STATE_EXECUTING:
        ret = JS_ThrowTypeError(ctx, "cannot invoke a running generator");
        break;
    }
    return ret;
}

static JSValue js_generator_function_call(JSContext *ctx, JSValueConst func_obj,
                                          JSValueConst this_obj,
                                          int argc, JSValueConst *argv,
                                          int flags)
{
    JSValue obj, func_ret;
    JSGeneratorData *s;

    s = js_mallocz(ctx, sizeof(*s));
    if (!s)
        return JS_EXCEPTION;
    s->state = JS_GENERATOR_STATE_SUSPENDED_START;
    if (async_func_init(ctx, &s->func_state, func_obj, this_obj, argc, argv)) {
        s->state = JS_GENERATOR_STATE_COMPLETED;
        goto fail;
    }

    /* execute the function up to 'OP_initial_yield' */
    func_ret = async_func_resume(ctx, &s->func_state);
    if (JS_IsException(func_ret))
        goto fail;
    JS_FreeValue(ctx, func_ret);

    obj = js_create_from_ctor(ctx, func_obj, JS_CLASS_GENERATOR);
    if (JS_IsException(obj))
        goto fail;
    JS_SetOpaque(obj, s);
    return obj;
 fail:
    free_generator_stack_rt(ctx->rt, s);
    js_free(ctx, s);
    return JS_EXCEPTION;
}

/* AsyncFunction */

static void js_async_function_terminate(JSRuntime *rt, JSAsyncFunctionData *s)
{
    if (s->is_active) {
        async_func_free(rt, &s->func_state);
        s->is_active = FALSE;
    }
}

static void js_async_function_free0(JSRuntime *rt, JSAsyncFunctionData *s)
{
    js_async_function_terminate(rt, s);
    JS_FreeValueRT(rt, s->resolving_funcs[0]);
    JS_FreeValueRT(rt, s->resolving_funcs[1]);
    remove_gc_object(&s->header);
    js_free_rt(rt, s);
}

static void js_async_function_free(JSRuntime *rt, JSAsyncFunctionData *s)
{
    if (--s->header.ref_count == 0) {
        js_async_function_free0(rt, s);
    }
}

static void js_async_function_resolve_finalizer(JSRuntime *rt, JSValue val)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSAsyncFunctionData *s = p->u.async_function_data;
    if (s) {
        js_async_function_free(rt, s);
    }
}

static void js_async_function_resolve_mark(JSRuntime *rt, JSValueConst val,
                                           JS_MarkFunc *mark_func)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSAsyncFunctionData *s = p->u.async_function_data;
    if (s) {
        mark_func(rt, &s->header);
    }
}

static int js_async_function_resolve_create(JSContext *ctx,
                                            JSAsyncFunctionData *s,
                                            JSValue *resolving_funcs)
{
    int i;
    JSObject *p;

    for(i = 0; i < 2; i++) {
        resolving_funcs[i] =
            JS_NewObjectProtoClass(ctx, ctx->function_proto,
                                   JS_CLASS_ASYNC_FUNCTION_RESOLVE + i);
        if (JS_IsException(resolving_funcs[i])) {
            if (i == 1)
                JS_FreeValue(ctx, resolving_funcs[0]);
            return -1;
        }
        p = JS_VALUE_GET_OBJ(resolving_funcs[i]);
        s->header.ref_count++;
        p->u.async_function_data = s;
    }
    return 0;
}

static void js_async_function_resume(JSContext *ctx, JSAsyncFunctionData *s)
{
    JSValue func_ret, ret2;

    func_ret = async_func_resume(ctx, &s->func_state);
    if (JS_IsException(func_ret)) {
        JSValue error;
    fail:
        error = JS_GetException(ctx);
        ret2 = JS_Call(ctx, s->resolving_funcs[1], JS_UNDEFINED,
                       1, (JSValueConst *)&error);
        JS_FreeValue(ctx, error);
        js_async_function_terminate(ctx->rt, s);
        JS_FreeValue(ctx, ret2); /* XXX: what to do if exception ? */
    } else {
        JSValue value;
        value = s->func_state.frame.cur_sp[-1];
        s->func_state.frame.cur_sp[-1] = JS_UNDEFINED;
        if (JS_IsUndefined(func_ret)) {
            /* function returned */
            ret2 = JS_Call(ctx, s->resolving_funcs[0], JS_UNDEFINED,
                           1, (JSValueConst *)&value);
            JS_FreeValue(ctx, ret2); /* XXX: what to do if exception ? */
            JS_FreeValue(ctx, value);
            js_async_function_terminate(ctx->rt, s);
        } else {
            JSValue promise, resolving_funcs[2], resolving_funcs1[2];
            int i, res;

            /* await */
            JS_FreeValue(ctx, func_ret); /* not used */
            promise = js_promise_resolve(ctx, ctx->promise_ctor,
                                         1, (JSValueConst *)&value, 0);
            JS_FreeValue(ctx, value);
            if (JS_IsException(promise))
                goto fail;
            if (js_async_function_resolve_create(ctx, s, resolving_funcs)) {
                JS_FreeValue(ctx, promise);
                goto fail;
            }

            /* Note: no need to create 'thrownawayCapability' as in
               the spec */
            for(i = 0; i < 2; i++)
                resolving_funcs1[i] = JS_UNDEFINED;
            res = perform_promise_then(ctx, promise,
                                       (JSValueConst *)resolving_funcs,
                                       (JSValueConst *)resolving_funcs1);
            JS_FreeValue(ctx, promise);
            for(i = 0; i < 2; i++)
                JS_FreeValue(ctx, resolving_funcs[i]);
            if (res)
                goto fail;
        }
    }
}

static JSValue js_async_function_resolve_call(JSContext *ctx,
                                              JSValueConst func_obj,
                                              JSValueConst this_obj,
                                              int argc, JSValueConst *argv,
                                              int flags)
{
    JSObject *p = JS_VALUE_GET_OBJ(func_obj);
    JSAsyncFunctionData *s = p->u.async_function_data;
    BOOL is_reject = p->class_id - JS_CLASS_ASYNC_FUNCTION_RESOLVE;
    JSValueConst arg;

    if (argc > 0)
        arg = argv[0];
    else
        arg = JS_UNDEFINED;
    s->func_state.throw_flag = is_reject;
    if (is_reject) {
        JS_Throw(ctx, JS_DupValue(ctx, arg));
    } else {
        /* return value of await */
        s->func_state.frame.cur_sp[-1] = JS_DupValue(ctx, arg);
    }
    js_async_function_resume(ctx, s);
    return JS_UNDEFINED;
}

static JSValue js_async_function_call(JSContext *ctx, JSValueConst func_obj,
                                      JSValueConst this_obj,
                                      int argc, JSValueConst *argv, int flags)
{
    JSValue promise;
    JSAsyncFunctionData *s;

    s = js_mallocz(ctx, sizeof(*s));
    if (!s)
        return JS_EXCEPTION;
    s->header.ref_count = 1;
    add_gc_object(ctx->rt, &s->header, JS_GC_OBJ_TYPE_ASYNC_FUNCTION);
    s->is_active = FALSE;
    s->resolving_funcs[0] = JS_UNDEFINED;
    s->resolving_funcs[1] = JS_UNDEFINED;

    promise = JS_NewPromiseCapability(ctx, s->resolving_funcs);
    if (JS_IsException(promise))
        goto fail;

    if (async_func_init(ctx, &s->func_state, func_obj, this_obj, argc, argv)) {
    fail:
        JS_FreeValue(ctx, promise);
        js_async_function_free(ctx->rt, s);
        return JS_EXCEPTION;
    }
    s->is_active = TRUE;

    js_async_function_resume(ctx, s);

    js_async_function_free(ctx->rt, s);

    return promise;
}

/* AsyncGenerator */

typedef enum JSAsyncGeneratorStateEnum {
    JS_ASYNC_GENERATOR_STATE_SUSPENDED_START,
    JS_ASYNC_GENERATOR_STATE_SUSPENDED_YIELD,
    JS_ASYNC_GENERATOR_STATE_SUSPENDED_YIELD_STAR,
    JS_ASYNC_GENERATOR_STATE_EXECUTING,
    JS_ASYNC_GENERATOR_STATE_AWAITING_RETURN,
    JS_ASYNC_GENERATOR_STATE_COMPLETED,
} JSAsyncGeneratorStateEnum;

typedef struct JSAsyncGeneratorRequest {
    struct list_head link;
    /* completion */
    int completion_type; /* GEN_MAGIC_x */
    JSValue result;
    /* promise capability */
    JSValue promise;
    JSValue resolving_funcs[2];
} JSAsyncGeneratorRequest;

typedef struct JSAsyncGeneratorData {
    JSObject *generator; /* back pointer to the object (const) */
    JSAsyncGeneratorStateEnum state;
    JSAsyncFunctionState func_state;
    struct list_head queue; /* list of JSAsyncGeneratorRequest.link */
} JSAsyncGeneratorData;

static void js_async_generator_free(JSRuntime *rt,
                                    JSAsyncGeneratorData *s)
{
    struct list_head *el, *el1;
    JSAsyncGeneratorRequest *req;

    list_for_each_safe(el, el1, &s->queue) {
        req = list_entry(el, JSAsyncGeneratorRequest, link);
        JS_FreeValueRT(rt, req->result);
        JS_FreeValueRT(rt, req->promise);
        JS_FreeValueRT(rt, req->resolving_funcs[0]);
        JS_FreeValueRT(rt, req->resolving_funcs[1]);
        js_free_rt(rt, req);
    }
    if (s->state != JS_ASYNC_GENERATOR_STATE_COMPLETED &&
        s->state != JS_ASYNC_GENERATOR_STATE_AWAITING_RETURN) {
        async_func_free(rt, &s->func_state);
    }
    js_free_rt(rt, s);
}

static void js_async_generator_finalizer(JSRuntime *rt, JSValue obj)
{
    JSAsyncGeneratorData *s = JS_GetOpaque(obj, JS_CLASS_ASYNC_GENERATOR);

    if (s) {
        js_async_generator_free(rt, s);
    }
}

static void js_async_generator_mark(JSRuntime *rt, JSValueConst val,
                                    JS_MarkFunc *mark_func)
{
    JSAsyncGeneratorData *s = JS_GetOpaque(val, JS_CLASS_ASYNC_GENERATOR);
    struct list_head *el;
    JSAsyncGeneratorRequest *req;
    if (s) {
        list_for_each(el, &s->queue) {
            req = list_entry(el, JSAsyncGeneratorRequest, link);
            JS_MarkValue(rt, req->result, mark_func);
            JS_MarkValue(rt, req->promise, mark_func);
            JS_MarkValue(rt, req->resolving_funcs[0], mark_func);
            JS_MarkValue(rt, req->resolving_funcs[1], mark_func);
        }
        if (s->state != JS_ASYNC_GENERATOR_STATE_COMPLETED &&
            s->state != JS_ASYNC_GENERATOR_STATE_AWAITING_RETURN) {
            async_func_mark(rt, &s->func_state, mark_func);
        }
    }
}

static JSValue js_async_generator_resolve_function(JSContext *ctx,
                                          JSValueConst this_obj,
                                          int argc, JSValueConst *argv,
                                          int magic, JSValue *func_data);

static int js_async_generator_resolve_function_create(JSContext *ctx,
                                                      JSValueConst generator,
                                                      JSValue *resolving_funcs,
                                                      BOOL is_resume_next)
{
    int i;
    JSValue func;

    for(i = 0; i < 2; i++) {
        func = JS_NewCFunctionData(ctx, js_async_generator_resolve_function, 1,
                                   i + is_resume_next * 2, 1, &generator);
        if (JS_IsException(func)) {
            if (i == 1)
                JS_FreeValue(ctx, resolving_funcs[0]);
            return -1;
        }
        resolving_funcs[i] = func;
    }
    return 0;
}

static int js_async_generator_await(JSContext *ctx,
                                    JSAsyncGeneratorData *s,
                                    JSValueConst value)
{
    JSValue promise, resolving_funcs[2], resolving_funcs1[2];
    int i, res;

    promise = js_promise_resolve(ctx, ctx->promise_ctor,
                                 1, &value, 0);
    if (JS_IsException(promise))
        goto fail;

    if (js_async_generator_resolve_function_create(ctx, JS_MKPTR(JS_TAG_OBJECT, s->generator),
                                                   resolving_funcs, FALSE)) {
        JS_FreeValue(ctx, promise);
        goto fail;
    }

    /* Note: no need to create 'thrownawayCapability' as in
       the spec */
    for(i = 0; i < 2; i++)
        resolving_funcs1[i] = JS_UNDEFINED;
    res = perform_promise_then(ctx, promise,
                               (JSValueConst *)resolving_funcs,
                               (JSValueConst *)resolving_funcs1);
    JS_FreeValue(ctx, promise);
    for(i = 0; i < 2; i++)
        JS_FreeValue(ctx, resolving_funcs[i]);
    if (res)
        goto fail;
    return 0;
 fail:
    return -1;
}

static void js_async_generator_resolve_or_reject(JSContext *ctx,
                                                 JSAsyncGeneratorData *s,
                                                 JSValueConst result,
                                                 int is_reject)
{
    JSAsyncGeneratorRequest *next;
    JSValue ret;

    next = list_entry(s->queue.next, JSAsyncGeneratorRequest, link);
    list_del(&next->link);
    ret = JS_Call(ctx, next->resolving_funcs[is_reject], JS_UNDEFINED, 1,
                  &result);
    JS_FreeValue(ctx, ret);
    JS_FreeValue(ctx, next->result);
    JS_FreeValue(ctx, next->promise);
    JS_FreeValue(ctx, next->resolving_funcs[0]);
    JS_FreeValue(ctx, next->resolving_funcs[1]);
    js_free(ctx, next);
}

static void js_async_generator_resolve(JSContext *ctx,
                                       JSAsyncGeneratorData *s,
                                       JSValueConst value,
                                       BOOL done)
{
    JSValue result;
    result = js_create_iterator_result(ctx, JS_DupValue(ctx, value), done);
    /* XXX: better exception handling ? */
    js_async_generator_resolve_or_reject(ctx, s, result, 0);
    JS_FreeValue(ctx, result);
 }

static void js_async_generator_reject(JSContext *ctx,
                                       JSAsyncGeneratorData *s,
                                       JSValueConst exception)
{
    js_async_generator_resolve_or_reject(ctx, s, exception, 1);
}

static void js_async_generator_complete(JSContext *ctx,
                                        JSAsyncGeneratorData *s)
{
    if (s->state != JS_ASYNC_GENERATOR_STATE_COMPLETED) {
        s->state = JS_ASYNC_GENERATOR_STATE_COMPLETED;
        async_func_free(ctx->rt, &s->func_state);
    }
}

static int js_async_generator_completed_return(JSContext *ctx,
                                               JSAsyncGeneratorData *s,
                                               JSValueConst value)
{
    JSValue promise, resolving_funcs[2], resolving_funcs1[2];
    int res;

    promise = js_promise_resolve(ctx, ctx->promise_ctor,
                                 1, (JSValueConst *)&value, 0);
    if (JS_IsException(promise))
        return -1;
    if (js_async_generator_resolve_function_create(ctx,
                                                   JS_MKPTR(JS_TAG_OBJECT, s->generator),
                                                   resolving_funcs1,
                                                   TRUE)) {
        JS_FreeValue(ctx, promise);
        return -1;
    }
    resolving_funcs[0] = JS_UNDEFINED;
    resolving_funcs[1] = JS_UNDEFINED;
    res = perform_promise_then(ctx, promise,
                               (JSValueConst *)resolving_funcs1,
                               (JSValueConst *)resolving_funcs);
    JS_FreeValue(ctx, resolving_funcs1[0]);
    JS_FreeValue(ctx, resolving_funcs1[1]);
    JS_FreeValue(ctx, promise);
    return res;
}

static void js_async_generator_resume_next(JSContext *ctx,
                                           JSAsyncGeneratorData *s)
{
    JSAsyncGeneratorRequest *next;
    JSValue func_ret, value;

    for(;;) {
        if (list_empty(&s->queue))
            break;
        next = list_entry(s->queue.next, JSAsyncGeneratorRequest, link);
        switch(s->state) {
        case JS_ASYNC_GENERATOR_STATE_EXECUTING:
            /* only happens when restarting execution after await() */
            goto resume_exec;
        case JS_ASYNC_GENERATOR_STATE_AWAITING_RETURN:
            goto done;
        case JS_ASYNC_GENERATOR_STATE_SUSPENDED_START:
            if (next->completion_type == GEN_MAGIC_NEXT) {
                goto exec_no_arg;
            } else {
                js_async_generator_complete(ctx, s);
            }
            break;
        case JS_ASYNC_GENERATOR_STATE_COMPLETED:
            if (next->completion_type == GEN_MAGIC_NEXT) {
                js_async_generator_resolve(ctx, s, JS_UNDEFINED, TRUE);
            } else if (next->completion_type == GEN_MAGIC_RETURN) {
                s->state = JS_ASYNC_GENERATOR_STATE_AWAITING_RETURN;
                js_async_generator_completed_return(ctx, s, next->result);
                goto done;
            } else {
                js_async_generator_reject(ctx, s, next->result);
            }
            goto done;
        case JS_ASYNC_GENERATOR_STATE_SUSPENDED_YIELD:
        case JS_ASYNC_GENERATOR_STATE_SUSPENDED_YIELD_STAR:
            value = JS_DupValue(ctx, next->result);
            if (next->completion_type == GEN_MAGIC_THROW &&
                s->state == JS_ASYNC_GENERATOR_STATE_SUSPENDED_YIELD) {
                JS_Throw(ctx, value);
                s->func_state.throw_flag = TRUE;
            } else {
                /* 'yield' returns a value. 'yield *' also returns a value
                   in case the 'throw' method is called */
                s->func_state.frame.cur_sp[-1] = value;
                s->func_state.frame.cur_sp[0] =
                    JS_NewInt32(ctx, next->completion_type);
                s->func_state.frame.cur_sp++;
            exec_no_arg:
                s->func_state.throw_flag = FALSE;
            }
            s->state = JS_ASYNC_GENERATOR_STATE_EXECUTING;
        resume_exec:
            func_ret = async_func_resume(ctx, &s->func_state);
            if (JS_IsException(func_ret)) {
                value = JS_GetException(ctx);
                js_async_generator_complete(ctx, s);
                js_async_generator_reject(ctx, s, value);
                JS_FreeValue(ctx, value);
            } else if (JS_VALUE_GET_TAG(func_ret) == JS_TAG_INT) {
                int func_ret_code;
                value = s->func_state.frame.cur_sp[-1];
                s->func_state.frame.cur_sp[-1] = JS_UNDEFINED;
                func_ret_code = JS_VALUE_GET_INT(func_ret);
                switch(func_ret_code) {
                case FUNC_RET_YIELD:
                case FUNC_RET_YIELD_STAR:
                    if (func_ret_code == FUNC_RET_YIELD_STAR)
                        s->state = JS_ASYNC_GENERATOR_STATE_SUSPENDED_YIELD_STAR;
                    else
                        s->state = JS_ASYNC_GENERATOR_STATE_SUSPENDED_YIELD;
                    js_async_generator_resolve(ctx, s, value, FALSE);
                    JS_FreeValue(ctx, value);
                    break;
                case FUNC_RET_AWAIT:
                    js_async_generator_await(ctx, s, value);
                    JS_FreeValue(ctx, value);
                    goto done;
                default:
                    abort();
                }
            } else {
                assert(JS_IsUndefined(func_ret));
                /* end of function */
                value = s->func_state.frame.cur_sp[-1];
                s->func_state.frame.cur_sp[-1] = JS_UNDEFINED;
                js_async_generator_complete(ctx, s);
                js_async_generator_resolve(ctx, s, value, TRUE);
                JS_FreeValue(ctx, value);
            }
            break;
        default:
            abort();
        }
    }
 done: ;
}

static JSValue js_async_generator_resolve_function(JSContext *ctx,
                                                   JSValueConst this_obj,
                                                   int argc, JSValueConst *argv,
                                                   int magic, JSValue *func_data)
{
    BOOL is_reject = magic & 1;
    JSAsyncGeneratorData *s = JS_GetOpaque(func_data[0], JS_CLASS_ASYNC_GENERATOR);
    JSValueConst arg = argv[0];

    /* XXX: what if s == NULL */

    if (magic >= 2) {
        /* resume next case in AWAITING_RETURN state */
        assert(s->state == JS_ASYNC_GENERATOR_STATE_AWAITING_RETURN ||
               s->state == JS_ASYNC_GENERATOR_STATE_COMPLETED);
        s->state = JS_ASYNC_GENERATOR_STATE_COMPLETED;
        if (is_reject) {
            js_async_generator_reject(ctx, s, arg);
        } else {
            js_async_generator_resolve(ctx, s, arg, TRUE);
        }
    } else {
        /* restart function execution after await() */
        assert(s->state == JS_ASYNC_GENERATOR_STATE_EXECUTING);
        s->func_state.throw_flag = is_reject;
        if (is_reject) {
            JS_Throw(ctx, JS_DupValue(ctx, arg));
        } else {
            /* return value of await */
            s->func_state.frame.cur_sp[-1] = JS_DupValue(ctx, arg);
        }
        js_async_generator_resume_next(ctx, s);
    }
    return JS_UNDEFINED;
}

/* magic = GEN_MAGIC_x */
static JSValue js_async_generator_next(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv,
                                       int magic)
{
    JSAsyncGeneratorData *s = JS_GetOpaque(this_val, JS_CLASS_ASYNC_GENERATOR);
    JSValue promise, resolving_funcs[2];
    JSAsyncGeneratorRequest *req;

    promise = JS_NewPromiseCapability(ctx, resolving_funcs);
    if (JS_IsException(promise))
        return JS_EXCEPTION;
    if (!s) {
        JSValue err, res2;
        JS_ThrowTypeError(ctx, "not an AsyncGenerator object");
        err = JS_GetException(ctx);
        res2 = JS_Call(ctx, resolving_funcs[1], JS_UNDEFINED,
                       1, (JSValueConst *)&err);
        JS_FreeValue(ctx, err);
        JS_FreeValue(ctx, res2);
        JS_FreeValue(ctx, resolving_funcs[0]);
        JS_FreeValue(ctx, resolving_funcs[1]);
        return promise;
    }
    req = js_mallocz(ctx, sizeof(*req));
    if (!req)
        goto fail;
    req->completion_type = magic;
    req->result = JS_DupValue(ctx, argv[0]);
    req->promise = JS_DupValue(ctx, promise);
    req->resolving_funcs[0] = resolving_funcs[0];
    req->resolving_funcs[1] = resolving_funcs[1];
    list_add_tail(&req->link, &s->queue);
    if (s->state != JS_ASYNC_GENERATOR_STATE_EXECUTING) {
        js_async_generator_resume_next(ctx, s);
    }
    return promise;
 fail:
    JS_FreeValue(ctx, resolving_funcs[0]);
    JS_FreeValue(ctx, resolving_funcs[1]);
    JS_FreeValue(ctx, promise);
    return JS_EXCEPTION;
}

static JSValue js_async_generator_function_call(JSContext *ctx, JSValueConst func_obj,
                                                JSValueConst this_obj,
                                                int argc, JSValueConst *argv,
                                                int flags)
{
    JSValue obj, func_ret;
    JSAsyncGeneratorData *s;

    s = js_mallocz(ctx, sizeof(*s));
    if (!s)
        return JS_EXCEPTION;
    s->state = JS_ASYNC_GENERATOR_STATE_SUSPENDED_START;
    init_list_head(&s->queue);
    if (async_func_init(ctx, &s->func_state, func_obj, this_obj, argc, argv)) {
        s->state = JS_ASYNC_GENERATOR_STATE_COMPLETED;
        goto fail;
    }

    /* execute the function up to 'OP_initial_yield' (no yield nor
       await are possible) */
    func_ret = async_func_resume(ctx, &s->func_state);
    if (JS_IsException(func_ret))
        goto fail;
    JS_FreeValue(ctx, func_ret);

    obj = js_create_from_ctor(ctx, func_obj, JS_CLASS_ASYNC_GENERATOR);
    if (JS_IsException(obj))
        goto fail;
    s->generator = JS_VALUE_GET_OBJ(obj);
    JS_SetOpaque(obj, s);
    return obj;
 fail:
    js_async_generator_free(ctx->rt, s);
    return JS_EXCEPTION;
}

/* JS parser */

enum {
    TOK_NUMBER = -128,
    TOK_STRING,
    TOK_TEMPLATE,
    TOK_IDENT,
    TOK_REGEXP,
    /* warning: order matters (see js_parse_assign_expr) */
    TOK_MUL_ASSIGN,
    TOK_DIV_ASSIGN,
    TOK_MOD_ASSIGN,
    TOK_PLUS_ASSIGN,
    TOK_MINUS_ASSIGN,
    TOK_SHL_ASSIGN,
    TOK_SAR_ASSIGN,
    TOK_SHR_ASSIGN,
    TOK_AND_ASSIGN,
    TOK_XOR_ASSIGN,
    TOK_OR_ASSIGN,
#ifdef CONFIG_BIGNUM
    TOK_MATH_POW_ASSIGN,
#endif
    TOK_POW_ASSIGN,
    TOK_LAND_ASSIGN,
    TOK_LOR_ASSIGN,
    TOK_DOUBLE_QUESTION_MARK_ASSIGN,
    TOK_DEC,
    TOK_INC,
    TOK_SHL,
    TOK_SAR,
    TOK_SHR,
    TOK_LT,
    TOK_LTE,
    TOK_GT,
    TOK_GTE,
    TOK_EQ,
    TOK_STRICT_EQ,
    TOK_NEQ,
    TOK_STRICT_NEQ,
    TOK_LAND,
    TOK_LOR,
#ifdef CONFIG_BIGNUM
    TOK_MATH_POW,
#endif
    TOK_POW,
    TOK_ARROW,
    TOK_ELLIPSIS,
    TOK_DOUBLE_QUESTION_MARK,
    TOK_QUESTION_MARK_DOT,
    TOK_ERROR,
    TOK_PRIVATE_NAME,
    TOK_EOF,
    /* keywords: WARNING: same order as atoms */
    TOK_NULL, /* must be first */
    TOK_FALSE,
    TOK_TRUE,
    TOK_IF,
    TOK_ELSE,
    TOK_RETURN,
    TOK_VAR,
    TOK_THIS,
    TOK_DELETE,
    TOK_VOID,
    TOK_TYPEOF,
    TOK_NEW,
    TOK_IN,
    TOK_INSTANCEOF,
    TOK_DO,
    TOK_WHILE,
    TOK_FOR,
    TOK_BREAK,
    TOK_CONTINUE,
    TOK_SWITCH,
    TOK_CASE,
    TOK_DEFAULT,
    TOK_THROW,
    TOK_TRY,
    TOK_CATCH,
    TOK_FINALLY,
    TOK_FUNCTION,
    TOK_DEBUGGER,
    TOK_WITH,
    /* FutureReservedWord */
    TOK_CLASS,
    TOK_CONST,
    TOK_ENUM,
    TOK_EXPORT,
    TOK_EXTENDS,
    TOK_IMPORT,
    TOK_SUPER,
    /* FutureReservedWords when parsing strict mode code */
    TOK_IMPLEMENTS,
    TOK_INTERFACE,
    TOK_LET,
    TOK_PACKAGE,
    TOK_PRIVATE,
    TOK_PROTECTED,
    TOK_PUBLIC,
    TOK_STATIC,
    TOK_YIELD,
    TOK_AWAIT, /* must be last */
    TOK_OF,     /* only used for js_parse_skip_parens_token() */
};

#define TOK_FIRST_KEYWORD   TOK_NULL
#define TOK_LAST_KEYWORD    TOK_AWAIT

/* unicode code points */
#define CP_NBSP 0x00a0
#define CP_BOM  0xfeff

#define CP_LS   0x2028
#define CP_PS   0x2029

typedef struct BlockEnv {
    struct BlockEnv *prev;
    JSAtom label_name; /* JS_ATOM_NULL if none */
    int label_break; /* -1 if none */
    int label_cont; /* -1 if none */
    int drop_count; /* number of stack elements to drop */
    int label_finally; /* -1 if none */
    int scope_level;
    int has_iterator;
} BlockEnv;

typedef struct JSGlobalVar {
    int cpool_idx; /* if >= 0, index in the constant pool for hoisted
                      function defintion*/
    uint8_t force_init : 1; /* force initialization to undefined */
    uint8_t is_lexical : 1; /* global let/const definition */
    uint8_t is_const   : 1; /* const definition */
    int scope_level;    /* scope of definition */
    JSAtom var_name;  /* variable name */
} JSGlobalVar;

typedef struct RelocEntry {
    struct RelocEntry *next;
    uint32_t addr; /* address to patch */
    int size;   /* address size: 1, 2 or 4 bytes */
} RelocEntry;

typedef struct JumpSlot {
    int op;
    int size;
    int pos;
    int label;
} JumpSlot;

typedef struct LabelSlot {
    int ref_count;
    int pos;    /* phase 1 address, -1 means not resolved yet */
    int pos2;   /* phase 2 address, -1 means not resolved yet */
    int addr;   /* phase 3 address, -1 means not resolved yet */
    RelocEntry *first_reloc;
} LabelSlot;

typedef struct LineNumberSlot {
    uint32_t pc;
    int line_num;
} LineNumberSlot;

typedef enum JSParseFunctionEnum {
    JS_PARSE_FUNC_STATEMENT,
    JS_PARSE_FUNC_VAR,
    JS_PARSE_FUNC_EXPR,
    JS_PARSE_FUNC_ARROW,
    JS_PARSE_FUNC_GETTER,
    JS_PARSE_FUNC_SETTER,
    JS_PARSE_FUNC_METHOD,
    JS_PARSE_FUNC_CLASS_CONSTRUCTOR,
    JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR,
} JSParseFunctionEnum;

typedef enum JSParseExportEnum {
    JS_PARSE_EXPORT_NONE,
    JS_PARSE_EXPORT_NAMED,
    JS_PARSE_EXPORT_DEFAULT,
} JSParseExportEnum;

typedef struct JSFunctionDef {
    JSContext *ctx;
    struct JSFunctionDef *parent;
    int parent_cpool_idx; /* index in the constant pool of the parent
                             or -1 if none */
    int parent_scope_level; /* scope level in parent at point of definition */
    struct list_head child_list; /* list of JSFunctionDef.link */
    struct list_head link;

    BOOL is_eval; /* TRUE if eval code */
    int eval_type; /* only valid if is_eval = TRUE */
    BOOL is_global_var; /* TRUE if variables are not defined locally:
                           eval global, eval module or non strict eval */
    BOOL is_func_expr; /* TRUE if function expression */
    BOOL has_home_object; /* TRUE if the home object is available */
    BOOL has_prototype; /* true if a prototype field is necessary */
    BOOL has_simple_parameter_list;
    BOOL has_parameter_expressions; /* if true, an argument scope is created */
    BOOL has_use_strict; /* to reject directive in special cases */
    BOOL has_eval_call; /* true if the function contains a call to eval() */
    BOOL has_arguments_binding; /* true if the 'arguments' binding is
                                   available in the function */
    BOOL has_this_binding; /* true if the 'this' and new.target binding are
                              available in the function */
    BOOL new_target_allowed; /* true if the 'new.target' does not
                                throw a syntax error */
    BOOL super_call_allowed; /* true if super() is allowed */
    BOOL super_allowed; /* true if super. or super[] is allowed */
    BOOL arguments_allowed; /* true if the 'arguments' identifier is allowed */
    BOOL is_derived_class_constructor;
    BOOL in_function_body;
    BOOL backtrace_barrier;
    JSFunctionKindEnum func_kind : 8;
    JSParseFunctionEnum func_type : 8;
    uint8_t js_mode; /* bitmap of JS_MODE_x */
    JSAtom func_name; /* JS_ATOM_NULL if no name */

    JSVarDef *vars;
    int var_size; /* allocated size for vars[] */
    int var_count;
    JSVarDef *args;
    int arg_size; /* allocated size for args[] */
    int arg_count; /* number of arguments */
    int defined_arg_count;
    int var_object_idx; /* -1 if none */
    int arg_var_object_idx; /* -1 if none (var object for the argument scope) */
    int arguments_var_idx; /* -1 if none */
    int arguments_arg_idx; /* argument variable definition in argument scope, 
                              -1 if none */
    int func_var_idx; /* variable containing the current function (-1
                         if none, only used if is_func_expr is true) */
    int eval_ret_idx; /* variable containing the return value of the eval, -1 if none */
    int this_var_idx; /* variable containg the 'this' value, -1 if none */
    int new_target_var_idx; /* variable containg the 'new.target' value, -1 if none */
    int this_active_func_var_idx; /* variable containg the 'this.active_func' value, -1 if none */
    int home_object_var_idx;
    BOOL need_home_object;
    
    int scope_level;    /* index into fd->scopes if the current lexical scope */
    int scope_first;    /* index into vd->vars of first lexically scoped variable */
    int scope_size;     /* allocated size of fd->scopes array */
    int scope_count;    /* number of entries used in the fd->scopes array */
    JSVarScope *scopes;
    JSVarScope def_scope_array[4];
    int body_scope; /* scope of the body of the function or eval */

    int global_var_count;
    int global_var_size;
    JSGlobalVar *global_vars;

    DynBuf byte_code;
    int last_opcode_pos; /* -1 if no last opcode */
    int last_opcode_line_num;
    BOOL use_short_opcodes; /* true if short opcodes are used in byte_code */
    
    LabelSlot *label_slots;
    int label_size; /* allocated size for label_slots[] */
    int label_count;
    BlockEnv *top_break; /* break/continue label stack */

    /* constant pool (strings, functions, numbers) */
    JSValue *cpool;
    int cpool_count;
    int cpool_size;

    /* list of variables in the closure */
    int closure_var_count;
    int closure_var_size;
    JSClosureVar *closure_var;

    JumpSlot *jump_slots;
    int jump_size;
    int jump_count;

    LineNumberSlot *line_number_slots;
    int line_number_size;
    int line_number_count;
    int line_number_last;
    int line_number_last_pc;

    /* pc2line table */
    JSAtom filename;
    int line_num;
    DynBuf pc2line;

    char *source;  /* raw source, utf-8 encoded */
    int source_len;

    JSModuleDef *module; /* != NULL when parsing a module */
} JSFunctionDef;

typedef struct JSToken {
    int val;
    int line_num;   /* line number of token start */
    const uint8_t *ptr;
    union {
        struct {
            JSValue str;
            int sep;
        } str;
        struct {
            JSValue val;
#ifdef CONFIG_BIGNUM
            slimb_t exponent; /* may be != 0 only if val is a float */
#endif
        } num;
        struct {
            JSAtom atom;
            BOOL has_escape;
            BOOL is_reserved;
        } ident;
        struct {
            JSValue body;
            JSValue flags;
        } regexp;
    } u;
} JSToken;

typedef struct JSParseState {
    JSContext *ctx;
    int last_line_num;  /* line number of last token */
    int line_num;       /* line number of current offset */
    const char *filename;
    JSToken token;
    BOOL got_lf; /* true if got line feed before the current token */
    const uint8_t *last_ptr;
    const uint8_t *buf_ptr;
    const uint8_t *buf_end;

    /* current function code */
    JSFunctionDef *cur_func;
    BOOL is_module; /* parsing a module */
    BOOL allow_html_comments;
    BOOL ext_json; /* true if accepting JSON superset */
} JSParseState;

typedef struct JSOpCode {
#ifdef DUMP_BYTECODE
    const char *name;
#endif
    uint8_t size; /* in bytes */
    /* the opcodes remove n_pop items from the top of the stack, then
       pushes n_push items */
    uint8_t n_pop;
    uint8_t n_push;
    uint8_t fmt;
} JSOpCode;

static const JSOpCode opcode_info[OP_COUNT + (OP_TEMP_END - OP_TEMP_START)] = {
#define FMT(f)
#ifdef DUMP_BYTECODE
#define DEF(id, size, n_pop, n_push, f) { #id, size, n_pop, n_push, OP_FMT_ ## f },
#else
#define DEF(id, size, n_pop, n_push, f) { size, n_pop, n_push, OP_FMT_ ## f },
#endif
#include "quickjs-opcode.h"
#undef DEF
#undef FMT
};

#if SHORT_OPCODES
/* After the final compilation pass, short opcodes are used. Their
   opcodes overlap with the temporary opcodes which cannot appear in
   the final bytecode. Their description is after the temporary
   opcodes in opcode_info[]. */
#define short_opcode_info(op)           \
    opcode_info[(op) >= OP_TEMP_START ? \
                (op) + (OP_TEMP_END - OP_TEMP_START) : (op)]
#else
#define short_opcode_info(op) opcode_info[op]
#endif

static __exception int next_token(JSParseState *s);

static void free_token(JSParseState *s, JSToken *token)
{
    switch(token->val) {
#ifdef CONFIG_BIGNUM
    case TOK_NUMBER:
        JS_FreeValue(s->ctx, token->u.num.val);
        break;
#endif
    case TOK_STRING:
    case TOK_TEMPLATE:
        JS_FreeValue(s->ctx, token->u.str.str);
        break;
    case TOK_REGEXP:
        JS_FreeValue(s->ctx, token->u.regexp.body);
        JS_FreeValue(s->ctx, token->u.regexp.flags);
        break;
    case TOK_IDENT:
    case TOK_PRIVATE_NAME:
        JS_FreeAtom(s->ctx, token->u.ident.atom);
        break;
    default:
        if (token->val >= TOK_FIRST_KEYWORD &&
            token->val <= TOK_LAST_KEYWORD) {
            JS_FreeAtom(s->ctx, token->u.ident.atom);
        }
        break;
    }
}

static void __attribute((unused)) dump_token(JSParseState *s,
                                             const JSToken *token)
{
    switch(token->val) {
    case TOK_NUMBER:
        {
            double d;
            JS_ToFloat64(s->ctx, &d, token->u.num.val);  /* no exception possible */
            printf("number: %.14g\n", d);
        }
        break;
    case TOK_IDENT:
    dump_atom:
        {
            char buf[ATOM_GET_STR_BUF_SIZE];
            printf("ident: '%s'\n",
                   JS_AtomGetStr(s->ctx, buf, sizeof(buf), token->u.ident.atom));
        }
        break;
    case TOK_STRING:
        {
            const char *str;
            /* XXX: quote the string */
            str = JS_ToCString(s->ctx, token->u.str.str);
            printf("string: '%s'\n", str);
            JS_FreeCString(s->ctx, str);
        }
        break;
    case TOK_TEMPLATE:
        {
            const char *str;
            str = JS_ToCString(s->ctx, token->u.str.str);
            printf("template: `%s`\n", str);
            JS_FreeCString(s->ctx, str);
        }
        break;
    case TOK_REGEXP:
        {
            const char *str, *str2;
            str = JS_ToCString(s->ctx, token->u.regexp.body);
            str2 = JS_ToCString(s->ctx, token->u.regexp.flags);
            printf("regexp: '%s' '%s'\n", str, str2);
            JS_FreeCString(s->ctx, str);
            JS_FreeCString(s->ctx, str2);
        }
        break;
    case TOK_EOF:
        printf("eof\n");
        break;
    default:
        if (s->token.val >= TOK_NULL && s->token.val <= TOK_LAST_KEYWORD) {
            goto dump_atom;
        } else if (s->token.val >= 256) {
            printf("token: %d\n", token->val);
        } else {
            printf("token: '%c'\n", token->val);
        }
        break;
    }
}

int __attribute__((format(printf, 2, 3))) js_parse_error(JSParseState *s, const char *fmt, ...)
{
    JSContext *ctx = s->ctx;
    va_list ap;
    int backtrace_flags;
    
    va_start(ap, fmt);
    JS_ThrowError2(ctx, JS_SYNTAX_ERROR, fmt, ap, FALSE);
    va_end(ap);
    backtrace_flags = 0;
    if (s->cur_func && s->cur_func->backtrace_barrier)
        backtrace_flags = JS_BACKTRACE_FLAG_SINGLE_LEVEL;
    build_backtrace(ctx, ctx->rt->current_exception, s->filename, s->line_num,
                    backtrace_flags);
    return -1;
}

static int js_parse_expect(JSParseState *s, int tok)
{
    if (s->token.val != tok) {
        /* XXX: dump token correctly in all cases */
        return js_parse_error(s, "expecting '%c'", tok);
    }
    return next_token(s);
}

static int js_parse_expect_semi(JSParseState *s)
{
    if (s->token.val != ';') {
        /* automatic insertion of ';' */
        if (s->token.val == TOK_EOF || s->token.val == '}' || s->got_lf) {
            return 0;
        }
        return js_parse_error(s, "expecting '%c'", ';');
    }
    return next_token(s);
}

static int js_parse_error_reserved_identifier(JSParseState *s)
{
    char buf1[ATOM_GET_STR_BUF_SIZE];
    return js_parse_error(s, "'%s' is a reserved identifier",
                          JS_AtomGetStr(s->ctx, buf1, sizeof(buf1),
                                        s->token.u.ident.atom));
}

static __exception int js_parse_template_part(JSParseState *s, const uint8_t *p)
{
    uint32_t c;
    StringBuffer b_s, *b = &b_s;

    /* p points to the first byte of the template part */
    if (string_buffer_init(s->ctx, b, 32))
        goto fail;
    for(;;) {
        if (p >= s->buf_end)
            goto unexpected_eof;
        c = *p++;
        if (c == '`') {
            /* template end part */
            break;
        }
        if (c == '$' && *p == '{') {
            /* template start or middle part */
            p++;
            break;
        }
        if (c == '\\') {
            if (string_buffer_putc8(b, c))
                goto fail;
            if (p >= s->buf_end)
                goto unexpected_eof;
            c = *p++;
        }
        /* newline sequences are normalized as single '\n' bytes */
        if (c == '\r') {
            if (*p == '\n')
                p++;
            c = '\n';
        }
        if (c == '\n') {
            s->line_num++;
        } else if (c >= 0x80) {
            const uint8_t *p_next;
            c = unicode_from_utf8(p - 1, UTF8_CHAR_LEN_MAX, &p_next);
            if (c > 0x10FFFF) {
                js_parse_error(s, "invalid UTF-8 sequence");
                goto fail;
            }
            p = p_next;
        }
        if (string_buffer_putc(b, c))
            goto fail;
    }
    s->token.val = TOK_TEMPLATE;
    s->token.u.str.sep = c;
    s->token.u.str.str = string_buffer_end(b);
    s->buf_ptr = p;
    return 0;

 unexpected_eof:
    js_parse_error(s, "unexpected end of string");
 fail:
    string_buffer_free(b);
    return -1;
}

static __exception int js_parse_string(JSParseState *s, int sep,
                                       BOOL do_throw, const uint8_t *p,
                                       JSToken *token, const uint8_t **pp)
{
    int ret;
    uint32_t c;
    StringBuffer b_s, *b = &b_s;

    /* string */
    if (string_buffer_init(s->ctx, b, 32))
        goto fail;
    for(;;) {
        if (p >= s->buf_end)
            goto invalid_char;
        c = *p;
        if (c < 0x20) {
            if (!s->cur_func) {
                if (do_throw)
                    js_parse_error(s, "invalid character in a JSON string");
                goto fail;
            }
            if (sep == '`') {
                if (c == '\r') {
                    if (p[1] == '\n')
                        p++;
                    c = '\n';
                }
                /* do not update s->line_num */
            } else if (c == '\n' || c == '\r')
                goto invalid_char;
        }
        p++;
        if (c == sep)
            break;
        if (c == '$' && *p == '{' && sep == '`') {
            /* template start or middle part */
            p++;
            break;
        }
        if (c == '\\') {
            c = *p;
            /* XXX: need a specific JSON case to avoid
               accepting invalid escapes */
            switch(c) {
            case '\0':
                if (p >= s->buf_end)
                    goto invalid_char;
                p++;
                break;
            case '\'':
            case '\"':
            case '\\':
                p++;
                break;
            case '\r':  /* accept DOS and MAC newline sequences */
                if (p[1] == '\n') {
                    p++;
                }
                /* fall thru */
            case '\n':
                /* ignore escaped newline sequence */
                p++;
                if (sep != '`')
                    s->line_num++;
                continue;
            default:
                if (c >= '0' && c <= '9') {
                    if (!s->cur_func)
                        goto invalid_escape; /* JSON case */
                    if (!(s->cur_func->js_mode & JS_MODE_STRICT) && sep != '`')
                        goto parse_escape;
                    if (c == '0' && !(p[1] >= '0' && p[1] <= '9')) {
                        p++;
                        c = '\0';
                    } else {
                        if (c >= '8' || sep == '`') {
                            /* Note: according to ES2021, \8 and \9 are not
                               accepted in strict mode or in templates. */
                            goto invalid_escape;
                        } else {
                            if (do_throw)
                                js_parse_error(s, "octal escape sequences are not allowed in strict mode");
                        }
                        goto fail;
                    }
                } else if (c >= 0x80) {
                    const uint8_t *p_next;
                    c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p_next);
                    if (c > 0x10FFFF) {
                        goto invalid_utf8;
                    }
                    p = p_next;
                    /* LS or PS are skipped */
                    if (c == CP_LS || c == CP_PS)
                        continue;
                } else {
                parse_escape:
                    ret = lre_parse_escape(&p, TRUE);
                    if (ret == -1) {
                    invalid_escape:
                        if (do_throw)
                            js_parse_error(s, "malformed escape sequence in string literal");
                        goto fail;
                    } else if (ret < 0) {
                        /* ignore the '\' (could output a warning) */
                        p++;
                    } else {
                        c = ret;
                    }
                }
                break;
            }
        } else if (c >= 0x80) {
            const uint8_t *p_next;
            c = unicode_from_utf8(p - 1, UTF8_CHAR_LEN_MAX, &p_next);
            if (c > 0x10FFFF)
                goto invalid_utf8;
            p = p_next;
        }
        if (string_buffer_putc(b, c))
            goto fail;
    }
    token->val = TOK_STRING;
    token->u.str.sep = c;
    token->u.str.str = string_buffer_end(b);
    *pp = p;
    return 0;

 invalid_utf8:
    if (do_throw)
        js_parse_error(s, "invalid UTF-8 sequence");
    goto fail;
 invalid_char:
    if (do_throw)
        js_parse_error(s, "unexpected end of string");
 fail:
    string_buffer_free(b);
    return -1;
}

static inline BOOL token_is_pseudo_keyword(JSParseState *s, JSAtom atom) {
    return s->token.val == TOK_IDENT && s->token.u.ident.atom == atom &&
        !s->token.u.ident.has_escape;
}

static __exception int js_parse_regexp(JSParseState *s)
{
    const uint8_t *p;
    BOOL in_class;
    StringBuffer b_s, *b = &b_s;
    StringBuffer b2_s, *b2 = &b2_s;
    uint32_t c;

    p = s->buf_ptr;
    p++;
    in_class = FALSE;
    if (string_buffer_init(s->ctx, b, 32))
        return -1;
    if (string_buffer_init(s->ctx, b2, 1))
        goto fail;
    for(;;) {
        if (p >= s->buf_end) {
        eof_error:
            js_parse_error(s, "unexpected end of regexp");
            goto fail;
        }
        c = *p++;
        if (c == '\n' || c == '\r') {
            goto eol_error;
        } else if (c == '/') {
            if (!in_class)
                break;
        } else if (c == '[') {
            in_class = TRUE;
        } else if (c == ']') {
            /* XXX: incorrect as the first character in a class */
            in_class = FALSE;
        } else if (c == '\\') {
            if (string_buffer_putc8(b, c))
                goto fail;
            c = *p++;
            if (c == '\n' || c == '\r')
                goto eol_error;
            else if (c == '\0' && p >= s->buf_end)
                goto eof_error;
            else if (c >= 0x80) {
                const uint8_t *p_next;
                c = unicode_from_utf8(p - 1, UTF8_CHAR_LEN_MAX, &p_next);
                if (c > 0x10FFFF) {
                    goto invalid_utf8;
                }
                p = p_next;
                if (c == CP_LS || c == CP_PS)
                    goto eol_error;
            }
        } else if (c >= 0x80) {
            const uint8_t *p_next;
            c = unicode_from_utf8(p - 1, UTF8_CHAR_LEN_MAX, &p_next);
            if (c > 0x10FFFF) {
            invalid_utf8:
                js_parse_error(s, "invalid UTF-8 sequence");
                goto fail;
            }
            p = p_next;
            /* LS or PS are considered as line terminator */
            if (c == CP_LS || c == CP_PS) {
            eol_error:
                js_parse_error(s, "unexpected line terminator in regexp");
                goto fail;
            }
        }
        if (string_buffer_putc(b, c))
            goto fail;
    }

    /* flags */
    for(;;) {
        const uint8_t *p_next = p;
        c = *p_next++;
        if (c >= 0x80) {
            c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p_next);
            if (c > 0x10FFFF) {
                goto invalid_utf8;
            }
        }
        if (!lre_js_is_ident_next(c))
            break;
        if (string_buffer_putc(b2, c))
            goto fail;
        p = p_next;
    }

    s->token.val = TOK_REGEXP;
    s->token.u.regexp.body = string_buffer_end(b);
    s->token.u.regexp.flags = string_buffer_end(b2);
    s->buf_ptr = p;
    return 0;
 fail:
    string_buffer_free(b);
    string_buffer_free(b2);
    return -1;
}

static __exception int ident_realloc(JSContext *ctx, char **pbuf, size_t *psize,
                                     char *static_buf)
{
    char *buf, *new_buf;
    size_t size, new_size;
    
    buf = *pbuf;
    size = *psize;
    if (size >= (SIZE_MAX / 3) * 2)
        new_size = SIZE_MAX;
    else
        new_size = size + (size >> 1);
    if (buf == static_buf) {
        new_buf = js_malloc(ctx, new_size);
        if (!new_buf)
            return -1;
        memcpy(new_buf, buf, size);
    } else {
        new_buf = js_realloc(ctx, buf, new_size);
        if (!new_buf)
            return -1;
    }
    *pbuf = new_buf;
    *psize = new_size;
    return 0;
}

/* 'c' is the first character. Return JS_ATOM_NULL in case of error */
static JSAtom parse_ident(JSParseState *s, const uint8_t **pp,
                          BOOL *pident_has_escape, int c, BOOL is_private)
{
    const uint8_t *p, *p1;
    char ident_buf[128], *buf;
    size_t ident_size, ident_pos;
    JSAtom atom;
    
    p = *pp;
    buf = ident_buf;
    ident_size = sizeof(ident_buf);
    ident_pos = 0;
    if (is_private)
        buf[ident_pos++] = '#';
    for(;;) {
        p1 = p;
        
        if (c < 128) {
            buf[ident_pos++] = c;
        } else {
            ident_pos += unicode_to_utf8((uint8_t*)buf + ident_pos, c);
        }
        c = *p1++;
        if (c == '\\' && *p1 == 'u') {
            c = lre_parse_escape(&p1, TRUE);
            *pident_has_escape = TRUE;
        } else if (c >= 128) {
            c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p1);
        }
        if (!lre_js_is_ident_next(c))
            break;
        p = p1;
        if (unlikely(ident_pos >= ident_size - UTF8_CHAR_LEN_MAX)) {
            if (ident_realloc(s->ctx, &buf, &ident_size, ident_buf)) {
                atom = JS_ATOM_NULL;
                goto done;
            }
        }
    }
    atom = JS_NewAtomLen(s->ctx, buf, ident_pos);
 done:
    if (unlikely(buf != ident_buf))
        js_free(s->ctx, buf);
    *pp = p;
    return atom;
}


static __exception int next_token(JSParseState *s)
{
    const uint8_t *p;
    int c;
    BOOL ident_has_escape;
    JSAtom atom;
    
    if (js_check_stack_overflow(s->ctx->rt, 0)) {
        return js_parse_error(s, "stack overflow");
    }
    
    free_token(s, &s->token);

    p = s->last_ptr = s->buf_ptr;
    s->got_lf = FALSE;
    s->last_line_num = s->token.line_num;
 redo:
    s->token.line_num = s->line_num;
    s->token.ptr = p;
    c = *p;
    switch(c) {
    case 0:
        if (p >= s->buf_end) {
            s->token.val = TOK_EOF;
        } else {
            goto def_token;
        }
        break;
    case '`':
        if (js_parse_template_part(s, p + 1))
            goto fail;
        p = s->buf_ptr;
        break;
    case '\'':
    case '\"':
        if (js_parse_string(s, c, TRUE, p + 1, &s->token, &p))
            goto fail;
        break;
    case '\r':  /* accept DOS and MAC newline sequences */
        if (p[1] == '\n') {
            p++;
        }
        /* fall thru */
    case '\n':
        p++;
    line_terminator:
        s->got_lf = TRUE;
        s->line_num++;
        goto redo;
    case '\f':
    case '\v':
    case ' ':
    case '\t':
        p++;
        goto redo;
    case '/':
        if (p[1] == '*') {
            /* comment */
            p += 2;
            for(;;) {
                if (*p == '\0' && p >= s->buf_end) {
                    js_parse_error(s, "unexpected end of comment");
                    goto fail;
                }
                if (p[0] == '*' && p[1] == '/') {
                    p += 2;
                    break;
                }
                if (*p == '\n') {
                    s->line_num++;
                    s->got_lf = TRUE; /* considered as LF for ASI */
                    p++;
                } else if (*p == '\r') {
                    s->got_lf = TRUE; /* considered as LF for ASI */
                    p++;
                } else if (*p >= 0x80) {
                    c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p);
                    if (c == CP_LS || c == CP_PS) {
                        s->got_lf = TRUE; /* considered as LF for ASI */
                    } else if (c == -1) {
                        p++; /* skip invalid UTF-8 */
                    }
                } else {
                    p++;
                }
            }
            goto redo;
        } else if (p[1] == '/') {
            /* line comment */
            p += 2;
        skip_line_comment:
            for(;;) {
                if (*p == '\0' && p >= s->buf_end)
                    break;
                if (*p == '\r' || *p == '\n')
                    break;
                if (*p >= 0x80) {
                    c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p);
                    /* LS or PS are considered as line terminator */
                    if (c == CP_LS || c == CP_PS) {
                        break;
                    } else if (c == -1) {
                        p++; /* skip invalid UTF-8 */
                    }
                } else {
                    p++;
                }
            }
            goto redo;
        } else if (p[1] == '=') {
            p += 2;
            s->token.val = TOK_DIV_ASSIGN;
        } else {
            p++;
            s->token.val = c;
        }
        break;
    case '\\':
        if (p[1] == 'u') {
            const uint8_t *p1 = p + 1;
            int c1 = lre_parse_escape(&p1, TRUE);
            if (c1 >= 0 && lre_js_is_ident_first(c1)) {
                c = c1;
                p = p1;
                ident_has_escape = TRUE;
                goto has_ident;
            } else {
                /* XXX: syntax error? */
            }
        }
        goto def_token;
    case 'a': case 'b': case 'c': case 'd':
    case 'e': case 'f': case 'g': case 'h':
    case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p':
    case 'q': case 'r': case 's': case 't':
    case 'u': case 'v': case 'w': case 'x':
    case 'y': case 'z': 
    case 'A': case 'B': case 'C': case 'D':
    case 'E': case 'F': case 'G': case 'H':
    case 'I': case 'J': case 'K': case 'L':
    case 'M': case 'N': case 'O': case 'P':
    case 'Q': case 'R': case 'S': case 'T':
    case 'U': case 'V': case 'W': case 'X':
    case 'Y': case 'Z': 
    case '_':
    case '$':
        /* identifier */
        p++;
        ident_has_escape = FALSE;
    has_ident:
        atom = parse_ident(s, &p, &ident_has_escape, c, FALSE);
        if (atom == JS_ATOM_NULL)
            goto fail;
        s->token.u.ident.atom = atom;
        s->token.u.ident.has_escape = ident_has_escape;
        s->token.u.ident.is_reserved = FALSE;
        if (s->token.u.ident.atom <= JS_ATOM_LAST_KEYWORD ||
            (s->token.u.ident.atom <= JS_ATOM_LAST_STRICT_KEYWORD &&
             (s->cur_func->js_mode & JS_MODE_STRICT)) ||
            (s->token.u.ident.atom == JS_ATOM_yield &&
             ((s->cur_func->func_kind & JS_FUNC_GENERATOR) ||
              (s->cur_func->func_type == JS_PARSE_FUNC_ARROW &&
               !s->cur_func->in_function_body && s->cur_func->parent &&
               (s->cur_func->parent->func_kind & JS_FUNC_GENERATOR)))) ||
            (s->token.u.ident.atom == JS_ATOM_await &&
             (s->is_module ||
              (((s->cur_func->func_kind & JS_FUNC_ASYNC) ||
                (s->cur_func->func_type == JS_PARSE_FUNC_ARROW &&
                 !s->cur_func->in_function_body && s->cur_func->parent &&
                 (s->cur_func->parent->func_kind & JS_FUNC_ASYNC))))))) {
                  if (ident_has_escape) {
                      s->token.u.ident.is_reserved = TRUE;
                      s->token.val = TOK_IDENT;
                  } else {
                      /* The keywords atoms are pre allocated */
                      s->token.val = s->token.u.ident.atom - 1 + TOK_FIRST_KEYWORD;
                  }
        } else {
            s->token.val = TOK_IDENT;
        }
        break;
    case '#':
        /* private name */
        {
            const uint8_t *p1;
            p++;
            p1 = p;
            c = *p1++;
            if (c == '\\' && *p1 == 'u') {
                c = lre_parse_escape(&p1, TRUE);
            } else if (c >= 128) {
                c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p1);
            }
            if (!lre_js_is_ident_first(c)) {
                js_parse_error(s, "invalid first character of private name");
                goto fail;
            }
            p = p1;
            ident_has_escape = FALSE; /* not used */
            atom = parse_ident(s, &p, &ident_has_escape, c, TRUE);
            if (atom == JS_ATOM_NULL)
                goto fail;
            s->token.u.ident.atom = atom;
            s->token.val = TOK_PRIVATE_NAME;
        }
        break;
    case '.':
        if (p[1] == '.' && p[2] == '.') {
            p += 3;
            s->token.val = TOK_ELLIPSIS;
            break;
        }
        if (p[1] >= '0' && p[1] <= '9') {
            goto parse_number;
        } else {
            goto def_token;
        }
        break;
    case '0':
        /* in strict mode, octal literals are not accepted */
        if (is_digit(p[1]) && (s->cur_func->js_mode & JS_MODE_STRICT)) {
            js_parse_error(s, "octal literals are deprecated in strict mode");
            goto fail;
        }
        goto parse_number;
    case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8':
    case '9': 
        /* number */
    parse_number:
        {
            JSValue ret;
            const uint8_t *p1;
            int flags, radix;
            flags = ATOD_ACCEPT_BIN_OCT | ATOD_ACCEPT_LEGACY_OCTAL |
                ATOD_ACCEPT_UNDERSCORES;
#ifdef CONFIG_BIGNUM
            flags |= ATOD_ACCEPT_SUFFIX;
            if (s->cur_func->js_mode & JS_MODE_MATH) {
                flags |= ATOD_MODE_BIGINT;
                if (s->cur_func->js_mode & JS_MODE_MATH)
                    flags |= ATOD_TYPE_BIG_FLOAT;
            }
#endif
            radix = 0;
#ifdef CONFIG_BIGNUM
            s->token.u.num.exponent = 0;
            ret = js_atof2(s->ctx, (const char *)p, (const char **)&p, radix,
                           flags, &s->token.u.num.exponent);
#else
            ret = js_atof(s->ctx, (const char *)p, (const char **)&p, radix,
                          flags);
#endif
            if (JS_IsException(ret))
                goto fail;
            /* reject `10instanceof Number` */
            if (JS_VALUE_IS_NAN(ret) ||
                lre_js_is_ident_next(unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p1))) {
                JS_FreeValue(s->ctx, ret);
                js_parse_error(s, "invalid number literal");
                goto fail;
            }
            s->token.val = TOK_NUMBER;
            s->token.u.num.val = ret;
        }
        break;
    case '*':
        if (p[1] == '=') {
            p += 2;
            s->token.val = TOK_MUL_ASSIGN;
        } else if (p[1] == '*') {
            if (p[2] == '=') {
                p += 3;
                s->token.val = TOK_POW_ASSIGN;
            } else {
                p += 2;
                s->token.val = TOK_POW;
            }
        } else {
            goto def_token;
        }
        break;
    case '%':
        if (p[1] == '=') {
            p += 2;
            s->token.val = TOK_MOD_ASSIGN;
        } else {
            goto def_token;
        }
        break;
    case '+':
        if (p[1] == '=') {
            p += 2;
            s->token.val = TOK_PLUS_ASSIGN;
        } else if (p[1] == '+') {
            p += 2;
            s->token.val = TOK_INC;
        } else {
            goto def_token;
        }
        break;
    case '-':
        if (p[1] == '=') {
            p += 2;
            s->token.val = TOK_MINUS_ASSIGN;
        } else if (p[1] == '-') {
            if (s->allow_html_comments &&
                p[2] == '>' && s->last_line_num != s->line_num) {
                /* Annex B: `-->` at beginning of line is an html comment end.
                   It extends to the end of the line.
                 */
                goto skip_line_comment;
            }
            p += 2;
            s->token.val = TOK_DEC;
        } else {
            goto def_token;
        }
        break;
    case '<':
        if (p[1] == '=') {
            p += 2;
            s->token.val = TOK_LTE;
        } else if (p[1] == '<') {
            if (p[2] == '=') {
                p += 3;
                s->token.val = TOK_SHL_ASSIGN;
            } else {
                p += 2;
                s->token.val = TOK_SHL;
            }
        } else if (s->allow_html_comments &&
                   p[1] == '!' && p[2] == '-' && p[3] == '-') {
            /* Annex B: handle `<!--` single line html comments */
            goto skip_line_comment;
        } else {
            goto def_token;
        }
        break;
    case '>':
        if (p[1] == '=') {
            p += 2;
            s->token.val = TOK_GTE;
        } else if (p[1] == '>') {
            if (p[2] == '>') {
                if (p[3] == '=') {
                    p += 4;
                    s->token.val = TOK_SHR_ASSIGN;
                } else {
                    p += 3;
                    s->token.val = TOK_SHR;
                }
            } else if (p[2] == '=') {
                p += 3;
                s->token.val = TOK_SAR_ASSIGN;
            } else {
                p += 2;
                s->token.val = TOK_SAR;
            }
        } else {
            goto def_token;
        }
        break;
    case '=':
        if (p[1] == '=') {
            if (p[2] == '=') {
                p += 3;
                s->token.val = TOK_STRICT_EQ;
            } else {
                p += 2;
                s->token.val = TOK_EQ;
            }
        } else if (p[1] == '>') {
            p += 2;
            s->token.val = TOK_ARROW;
        } else {
            goto def_token;
        }
        break;
    case '!':
        if (p[1] == '=') {
            if (p[2] == '=') {
                p += 3;
                s->token.val = TOK_STRICT_NEQ;
            } else {
                p += 2;
                s->token.val = TOK_NEQ;
            }
        } else {
            goto def_token;
        }
        break;
    case '&':
        if (p[1] == '=') {
            p += 2;
            s->token.val = TOK_AND_ASSIGN;
        } else if (p[1] == '&') {
            if (p[2] == '=') {
                p += 3;
                s->token.val = TOK_LAND_ASSIGN;
            } else {
                p += 2;
                s->token.val = TOK_LAND;
            }
        } else {
            goto def_token;
        }
        break;
#ifdef CONFIG_BIGNUM
        /* in math mode, '^' is the power operator. '^^' is always the
           xor operator and '**' is always the power operator */
    case '^':
        if (p[1] == '=') {
            p += 2;
            if (s->cur_func->js_mode & JS_MODE_MATH)
                s->token.val = TOK_MATH_POW_ASSIGN;
            else
                s->token.val = TOK_XOR_ASSIGN;
        } else if (p[1] == '^') {
            if (p[2] == '=') {
                p += 3;
                s->token.val = TOK_XOR_ASSIGN;
            } else {
                p += 2;
                s->token.val = '^';
            }
        } else {
            p++;
            if (s->cur_func->js_mode & JS_MODE_MATH)
                s->token.val = TOK_MATH_POW;
            else
                s->token.val = '^';
        }
        break;
#else
    case '^':
        if (p[1] == '=') {
            p += 2;
            s->token.val = TOK_XOR_ASSIGN;
        } else {
            goto def_token;
        }
        break;
#endif
    case '|':
        if (p[1] == '=') {
            p += 2;
            s->token.val = TOK_OR_ASSIGN;
        } else if (p[1] == '|') {
            if (p[2] == '=') {
                p += 3;
                s->token.val = TOK_LOR_ASSIGN;
            } else {
                p += 2;
                s->token.val = TOK_LOR;
            }
        } else {
            goto def_token;
        }
        break;
    case '?':
        if (p[1] == '?') {
            if (p[2] == '=') {
                p += 3;
                s->token.val = TOK_DOUBLE_QUESTION_MARK_ASSIGN;
            } else {
                p += 2;
                s->token.val = TOK_DOUBLE_QUESTION_MARK;
            }
        } else if (p[1] == '.' && !(p[2] >= '0' && p[2] <= '9')) {
            p += 2;
            s->token.val = TOK_QUESTION_MARK_DOT;
        } else {
            goto def_token;
        }
        break;
    default:
        if (c >= 128) {
            /* unicode value */
            c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p);
            switch(c) {
            case CP_PS:
            case CP_LS:
                /* XXX: should avoid incrementing line_number, but
                   needed to handle HTML comments */
                goto line_terminator; 
            default:
                if (lre_is_space(c)) {
                    goto redo;
                } else if (lre_js_is_ident_first(c)) {
                    ident_has_escape = FALSE;
                    goto has_ident;
                } else {
                    js_parse_error(s, "unexpected character");
                    goto fail;
                }
            }
        }
    def_token:
        s->token.val = c;
        p++;
        break;
    }
    s->buf_ptr = p;

    //    dump_token(s, &s->token);
    return 0;

 fail:
    s->token.val = TOK_ERROR;
    return -1;
}

/* 'c' is the first character. Return JS_ATOM_NULL in case of error */
static JSAtom json_parse_ident(JSParseState *s, const uint8_t **pp, int c)
{
    const uint8_t *p;
    char ident_buf[128], *buf;
    size_t ident_size, ident_pos;
    JSAtom atom;
    
    p = *pp;
    buf = ident_buf;
    ident_size = sizeof(ident_buf);
    ident_pos = 0;
    for(;;) {
        buf[ident_pos++] = c;
        c = *p;
        if (c >= 128 ||
            !((lre_id_continue_table_ascii[c >> 5] >> (c & 31)) & 1))
            break;
        p++;
        if (unlikely(ident_pos >= ident_size - UTF8_CHAR_LEN_MAX)) {
            if (ident_realloc(s->ctx, &buf, &ident_size, ident_buf)) {
                atom = JS_ATOM_NULL;
                goto done;
            }
        }
    }
    atom = JS_NewAtomLen(s->ctx, buf, ident_pos);
 done:
    if (unlikely(buf != ident_buf))
        js_free(s->ctx, buf);
    *pp = p;
    return atom;
}

static __exception int json_next_token(JSParseState *s)
{
    const uint8_t *p;
    int c;
    JSAtom atom;
    
    if (js_check_stack_overflow(s->ctx->rt, 0)) {
        return js_parse_error(s, "stack overflow");
    }
    
    free_token(s, &s->token);

    p = s->last_ptr = s->buf_ptr;
    s->last_line_num = s->token.line_num;
 redo:
    s->token.line_num = s->line_num;
    s->token.ptr = p;
    c = *p;
    switch(c) {
    case 0:
        if (p >= s->buf_end) {
            s->token.val = TOK_EOF;
        } else {
            goto def_token;
        }
        break;
    case '\'':
        if (!s->ext_json) {
            /* JSON does not accept single quoted strings */
            goto def_token;
        }
        /* fall through */
    case '\"':
        if (js_parse_string(s, c, TRUE, p + 1, &s->token, &p))
            goto fail;
        break;
    case '\r':  /* accept DOS and MAC newline sequences */
        if (p[1] == '\n') {
            p++;
        }
        /* fall thru */
    case '\n':
        p++;
        s->line_num++;
        goto redo;
    case '\f':
    case '\v':
        if (!s->ext_json) {
            /* JSONWhitespace does not match <VT>, nor <FF> */
            goto def_token;
        }
        /* fall through */
    case ' ':
    case '\t':
        p++;
        goto redo;
    case '/':
        if (!s->ext_json) {
            /* JSON does not accept comments */
            goto def_token;
        }
        if (p[1] == '*') {
            /* comment */
            p += 2;
            for(;;) {
                if (*p == '\0' && p >= s->buf_end) {
                    js_parse_error(s, "unexpected end of comment");
                    goto fail;
                }
                if (p[0] == '*' && p[1] == '/') {
                    p += 2;
                    break;
                }
                if (*p == '\n') {
                    s->line_num++;
                    p++;
                } else if (*p == '\r') {
                    p++;
                } else if (*p >= 0x80) {
                    c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p);
                    if (c == -1) {
                        p++; /* skip invalid UTF-8 */
                    }
                } else {
                    p++;
                }
            }
            goto redo;
        } else if (p[1] == '/') {
            /* line comment */
            p += 2;
            for(;;) {
                if (*p == '\0' && p >= s->buf_end)
                    break;
                if (*p == '\r' || *p == '\n')
                    break;
                if (*p >= 0x80) {
                    c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p);
                    /* LS or PS are considered as line terminator */
                    if (c == CP_LS || c == CP_PS) {
                        break;
                    } else if (c == -1) {
                        p++; /* skip invalid UTF-8 */
                    }
                } else {
                    p++;
                }
            }
            goto redo;
        } else {
            goto def_token;
        }
        break;
    case 'a': case 'b': case 'c': case 'd':
    case 'e': case 'f': case 'g': case 'h':
    case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p':
    case 'q': case 'r': case 's': case 't':
    case 'u': case 'v': case 'w': case 'x':
    case 'y': case 'z': 
    case 'A': case 'B': case 'C': case 'D':
    case 'E': case 'F': case 'G': case 'H':
    case 'I': case 'J': case 'K': case 'L':
    case 'M': case 'N': case 'O': case 'P':
    case 'Q': case 'R': case 'S': case 'T':
    case 'U': case 'V': case 'W': case 'X':
    case 'Y': case 'Z': 
    case '_':
    case '$':
        /* identifier : only pure ascii characters are accepted */
        p++;
        atom = json_parse_ident(s, &p, c);
        if (atom == JS_ATOM_NULL)
            goto fail;
        s->token.u.ident.atom = atom;
        s->token.u.ident.has_escape = FALSE;
        s->token.u.ident.is_reserved = FALSE;
        s->token.val = TOK_IDENT;
        break;
    case '+':
        if (!s->ext_json || !is_digit(p[1]))
            goto def_token;
        goto parse_number;
    case '0':
        if (is_digit(p[1]))
            goto def_token;
        goto parse_number;
    case '-':
        if (!is_digit(p[1]))
            goto def_token;
        goto parse_number;
    case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8':
    case '9': 
        /* number */
    parse_number:
        {
            JSValue ret;
            int flags, radix;
            if (!s->ext_json) {
                flags = 0;
                radix = 10;
            } else {
                flags = ATOD_ACCEPT_BIN_OCT;
                radix = 0;
            }
            ret = js_atof(s->ctx, (const char *)p, (const char **)&p, radix,
                          flags);
            if (JS_IsException(ret))
                goto fail;
            s->token.val = TOK_NUMBER;
            s->token.u.num.val = ret;
        }
        break;
    default:
        if (c >= 128) {
            js_parse_error(s, "unexpected character");
            goto fail;
        }
    def_token:
        s->token.val = c;
        p++;
        break;
    }
    s->buf_ptr = p;

    //    dump_token(s, &s->token);
    return 0;

 fail:
    s->token.val = TOK_ERROR;
    return -1;
}

/* only used for ':' and '=>', 'let' or 'function' look-ahead. *pp is
   only set if TOK_IMPORT is returned */
/* XXX: handle all unicode cases */
static int simple_next_token(const uint8_t **pp, BOOL no_line_terminator)
{
    const uint8_t *p;
    uint32_t c;
    
    /* skip spaces and comments */
    p = *pp;
    for (;;) {
        switch(c = *p++) {
        case '\r':
        case '\n':
            if (no_line_terminator)
                return '\n';
            continue;
        case ' ':
        case '\t':
        case '\v':
        case '\f':
            continue;
        case '/':
            if (*p == '/') {
                if (no_line_terminator)
                    return '\n';
                while (*p && *p != '\r' && *p != '\n')
                    p++;
                continue;
            }
            if (*p == '*') {
                while (*++p) {
                    if ((*p == '\r' || *p == '\n') && no_line_terminator)
                        return '\n';
                    if (*p == '*' && p[1] == '/') {
                        p += 2;
                        break;
                    }
                }
                continue;
            }
            break;
        case '=':
            if (*p == '>')
                return TOK_ARROW;
            break;
        default:
            if (lre_js_is_ident_first(c)) {
                if (c == 'i') {
                    if (p[0] == 'n' && !lre_js_is_ident_next(p[1])) {
                        return TOK_IN;
                    }
                    if (p[0] == 'm' && p[1] == 'p' && p[2] == 'o' &&
                        p[3] == 'r' && p[4] == 't' &&
                        !lre_js_is_ident_next(p[5])) {
                        *pp = p + 5;
                        return TOK_IMPORT;
                    }
                } else if (c == 'o' && *p == 'f' && !lre_js_is_ident_next(p[1])) {
                    return TOK_OF;
                } else if (c == 'e' &&
                           p[0] == 'x' && p[1] == 'p' && p[2] == 'o' &&
                           p[3] == 'r' && p[4] == 't' &&
                           !lre_js_is_ident_next(p[5])) {
                    *pp = p + 5;
                    return TOK_EXPORT;
                } else if (c == 'f' && p[0] == 'u' && p[1] == 'n' &&
                         p[2] == 'c' && p[3] == 't' && p[4] == 'i' &&
                         p[5] == 'o' && p[6] == 'n' && !lre_js_is_ident_next(p[7])) {
                    return TOK_FUNCTION;
                }
                return TOK_IDENT;
            }
            break;
        }
        return c;
    }
}

static int peek_token(JSParseState *s, BOOL no_line_terminator)
{
    const uint8_t *p = s->buf_ptr;
    return simple_next_token(&p, no_line_terminator);
}

/* return true if 'input' contains the source of a module
   (heuristic). 'input' must be a zero terminated.

   Heuristic: skip comments and expect 'import' keyword not followed
   by '(' or '.' or export keyword.
*/
BOOL JS_DetectModule(const char *input, size_t input_len)
{
    const uint8_t *p = (const uint8_t *)input;
    int tok;
    switch(simple_next_token(&p, FALSE)) {
    case TOK_IMPORT:
        tok = simple_next_token(&p, FALSE);
        return (tok != '.' && tok != '(');
    case TOK_EXPORT:
        return TRUE;
    default:
        return FALSE;
    }
}

static inline int get_prev_opcode(JSFunctionDef *fd) {
    if (fd->last_opcode_pos < 0)
        return OP_invalid;
    else
        return fd->byte_code.buf[fd->last_opcode_pos];
}

static BOOL js_is_live_code(JSParseState *s) {
    switch (get_prev_opcode(s->cur_func)) {
    case OP_tail_call:
    case OP_tail_call_method:
    case OP_return:
    case OP_return_undef:
    case OP_return_async:
    case OP_throw:
    case OP_throw_error:
    case OP_goto:
#if SHORT_OPCODES
    case OP_goto8:
    case OP_goto16:
#endif
    case OP_ret:
        return FALSE;
    default:
        return TRUE;
    }
}

static void emit_u8(JSParseState *s, uint8_t val)
{
    dbuf_putc(&s->cur_func->byte_code, val);
}

static void emit_u16(JSParseState *s, uint16_t val)
{
    dbuf_put_u16(&s->cur_func->byte_code, val);
}

static void emit_u32(JSParseState *s, uint32_t val)
{
    dbuf_put_u32(&s->cur_func->byte_code, val);
}

static void emit_op(JSParseState *s, uint8_t val)
{
    JSFunctionDef *fd = s->cur_func;
    DynBuf *bc = &fd->byte_code;

    /* Use the line number of the last token used, not the next token,
       nor the current offset in the source file.
     */
    if (unlikely(fd->last_opcode_line_num != s->last_line_num)) {
        dbuf_putc(bc, OP_line_num);
        dbuf_put_u32(bc, s->last_line_num);
        fd->last_opcode_line_num = s->last_line_num;
    }
    fd->last_opcode_pos = bc->size;
    dbuf_putc(bc, val);
}

static void emit_atom(JSParseState *s, JSAtom name)
{
    emit_u32(s, JS_DupAtom(s->ctx, name));
}

static int update_label(JSFunctionDef *s, int label, int delta)
{
    LabelSlot *ls;

    assert(label >= 0 && label < s->label_count);
    ls = &s->label_slots[label];
    ls->ref_count += delta;
    assert(ls->ref_count >= 0);
    return ls->ref_count;
}

static int new_label_fd(JSFunctionDef *fd, int label)
{
    LabelSlot *ls;

    if (label < 0) {
        if (js_resize_array(fd->ctx, (void *)&fd->label_slots,
                            sizeof(fd->label_slots[0]),
                            &fd->label_size, fd->label_count + 1))
            return -1;
        label = fd->label_count++;
        ls = &fd->label_slots[label];
        ls->ref_count = 0;
        ls->pos = -1;
        ls->pos2 = -1;
        ls->addr = -1;
        ls->first_reloc = NULL;
    }
    return label;
}

static int new_label(JSParseState *s)
{
    return new_label_fd(s->cur_func, -1);
}

/* return the label ID offset */
static int emit_label(JSParseState *s, int label)
{
    if (label >= 0) {
        emit_op(s, OP_label);
        emit_u32(s, label);
        s->cur_func->label_slots[label].pos = s->cur_func->byte_code.size;
        return s->cur_func->byte_code.size - 4;
    } else {
        return -1;
    }
}

/* return label or -1 if dead code */
static int emit_goto(JSParseState *s, int opcode, int label)
{
    if (js_is_live_code(s)) {
        if (label < 0)
            label = new_label(s);
        emit_op(s, opcode);
        emit_u32(s, label);
        s->cur_func->label_slots[label].ref_count++;
        return label;
    }
    return -1;
}

/* return the constant pool index. 'val' is not duplicated. */
static int cpool_add(JSParseState *s, JSValue val)
{
    JSFunctionDef *fd = s->cur_func;
    
    if (js_resize_array(s->ctx, (void *)&fd->cpool, sizeof(fd->cpool[0]),
                        &fd->cpool_size, fd->cpool_count + 1))
        return -1;
    fd->cpool[fd->cpool_count++] = val;
    return fd->cpool_count - 1;
}

static __exception int emit_push_const(JSParseState *s, JSValueConst val,
                                       BOOL as_atom)
{
    int idx;

    if (JS_VALUE_GET_TAG(val) == JS_TAG_STRING && as_atom) {
        JSAtom atom;
        /* warning: JS_NewAtomStr frees the string value */
        JS_DupValue(s->ctx, val);
        atom = JS_NewAtomStr(s->ctx, JS_VALUE_GET_STRING(val));
        if (atom != JS_ATOM_NULL && !__JS_AtomIsTaggedInt(atom)) {
            emit_op(s, OP_push_atom_value);
            emit_u32(s, atom);
            return 0;
        }
    }

    idx = cpool_add(s, JS_DupValue(s->ctx, val));
    if (idx < 0)
        return -1;
    emit_op(s, OP_push_const);
    emit_u32(s, idx);
    return 0;
}

/* return the variable index or -1 if not found,
   add ARGUMENT_VAR_OFFSET for argument variables */
static int find_arg(JSContext *ctx, JSFunctionDef *fd, JSAtom name)
{
    int i;
    for(i = fd->arg_count; i-- > 0;) {
        if (fd->args[i].var_name == name)
            return i | ARGUMENT_VAR_OFFSET;
    }
    return -1;
}

static int find_var(JSContext *ctx, JSFunctionDef *fd, JSAtom name)
{
    int i;
    for(i = fd->var_count; i-- > 0;) {
        if (fd->vars[i].var_name == name && fd->vars[i].scope_level == 0)
            return i;
    }
    return find_arg(ctx, fd, name);
}

/* find a variable declaration in a given scope */
static int find_var_in_scope(JSContext *ctx, JSFunctionDef *fd,
                             JSAtom name, int scope_level)
{
    int scope_idx;
    for(scope_idx = fd->scopes[scope_level].first; scope_idx >= 0;
        scope_idx = fd->vars[scope_idx].scope_next) {
        if (fd->vars[scope_idx].scope_level != scope_level)
            break;
        if (fd->vars[scope_idx].var_name == name)
            return scope_idx;
    }
    return -1;
}

/* return true if scope == parent_scope or if scope is a child of
   parent_scope */
static BOOL is_child_scope(JSContext *ctx, JSFunctionDef *fd,
                           int scope, int parent_scope)
{
    while (scope >= 0) {
        if (scope == parent_scope)
            return TRUE;
        scope = fd->scopes[scope].parent;
    }
    return FALSE;
}

/* find a 'var' declaration in the same scope or a child scope */
static int find_var_in_child_scope(JSContext *ctx, JSFunctionDef *fd,
                                   JSAtom name, int scope_level)
{
    int i;
    for(i = 0; i < fd->var_count; i++) {
        JSVarDef *vd = &fd->vars[i];
        if (vd->var_name == name && vd->scope_level == 0) {
            if (is_child_scope(ctx, fd, vd->scope_next,
                               scope_level))
                return i;
        }
    }
    return -1;
}


static JSGlobalVar *find_global_var(JSFunctionDef *fd, JSAtom name)
{
    int i;
    for(i = 0; i < fd->global_var_count; i++) {
        JSGlobalVar *hf = &fd->global_vars[i];
        if (hf->var_name == name)
            return hf;
    }
    return NULL;

}

static JSGlobalVar *find_lexical_global_var(JSFunctionDef *fd, JSAtom name)
{
    JSGlobalVar *hf = find_global_var(fd, name);
    if (hf && hf->is_lexical)
        return hf;
    else
        return NULL;
}

static int find_lexical_decl(JSContext *ctx, JSFunctionDef *fd, JSAtom name,
                             int scope_idx, BOOL check_catch_var)
{
    while (scope_idx >= 0) {
        JSVarDef *vd = &fd->vars[scope_idx];
        if (vd->var_name == name &&
            (vd->is_lexical || (vd->var_kind == JS_VAR_CATCH &&
                                check_catch_var)))
            return scope_idx;
        scope_idx = vd->scope_next;
    }

    if (fd->is_eval && fd->eval_type == JS_EVAL_TYPE_GLOBAL) {
        if (find_lexical_global_var(fd, name))
            return GLOBAL_VAR_OFFSET;
    }
    return -1;
}

static int push_scope(JSParseState *s) {
    if (s->cur_func) {
        JSFunctionDef *fd = s->cur_func;
        int scope = fd->scope_count;
        /* XXX: should check for scope overflow */
        if ((fd->scope_count + 1) > fd->scope_size) {
            int new_size;
            size_t slack;
            JSVarScope *new_buf;
            /* XXX: potential arithmetic overflow */
            new_size = max_int(fd->scope_count + 1, fd->scope_size * 3 / 2);
            if (fd->scopes == fd->def_scope_array) {
                new_buf = js_realloc2(s->ctx, NULL, new_size * sizeof(*fd->scopes), &slack);
                if (!new_buf)
                    return -1;
                memcpy(new_buf, fd->scopes, fd->scope_count * sizeof(*fd->scopes));
            } else {
                new_buf = js_realloc2(s->ctx, fd->scopes, new_size * sizeof(*fd->scopes), &slack);
                if (!new_buf)
                    return -1;
            }
            new_size += slack / sizeof(*new_buf);
            fd->scopes = new_buf;
            fd->scope_size = new_size;
        }
        fd->scope_count++;
        fd->scopes[scope].parent = fd->scope_level;
        fd->scopes[scope].first = fd->scope_first;
        emit_op(s, OP_enter_scope);
        emit_u16(s, scope);
        return fd->scope_level = scope;
    }
    return 0;
}

static int get_first_lexical_var(JSFunctionDef *fd, int scope)
{
    while (scope >= 0) {
        int scope_idx = fd->scopes[scope].first;
        if (scope_idx >= 0)
            return scope_idx;
        scope = fd->scopes[scope].parent;
    }
    return -1;
}

static void pop_scope(JSParseState *s) {
    if (s->cur_func) {
        /* disable scoped variables */
        JSFunctionDef *fd = s->cur_func;
        int scope = fd->scope_level;
        emit_op(s, OP_leave_scope);
        emit_u16(s, scope);
        fd->scope_level = fd->scopes[scope].parent;
        fd->scope_first = get_first_lexical_var(fd, fd->scope_level);
    }
}

static void close_scopes(JSParseState *s, int scope, int scope_stop)
{
    while (scope > scope_stop) {
        emit_op(s, OP_leave_scope);
        emit_u16(s, scope);
        scope = s->cur_func->scopes[scope].parent;
    }
}

/* return the variable index or -1 if error */
static int add_var(JSContext *ctx, JSFunctionDef *fd, JSAtom name)
{
    JSVarDef *vd;

    /* the local variable indexes are currently stored on 16 bits */
    if (fd->var_count >= JS_MAX_LOCAL_VARS) {
        JS_ThrowInternalError(ctx, "too many local variables");
        return -1;
    }
    if (js_resize_array(ctx, (void **)&fd->vars, sizeof(fd->vars[0]),
                        &fd->var_size, fd->var_count + 1))
        return -1;
    vd = &fd->vars[fd->var_count++];
    memset(vd, 0, sizeof(*vd));
    vd->var_name = JS_DupAtom(ctx, name);
    vd->func_pool_idx = -1;
    return fd->var_count - 1;
}

static int add_scope_var(JSContext *ctx, JSFunctionDef *fd, JSAtom name,
                         JSVarKindEnum var_kind)
{
    int idx = add_var(ctx, fd, name);
    if (idx >= 0) {
        JSVarDef *vd = &fd->vars[idx];
        vd->var_kind = var_kind;
        vd->scope_level = fd->scope_level;
        vd->scope_next = fd->scope_first;
        fd->scopes[fd->scope_level].first = idx;
        fd->scope_first = idx;
    }
    return idx;
}

static int add_func_var(JSContext *ctx, JSFunctionDef *fd, JSAtom name)
{
    int idx = fd->func_var_idx;
    if (idx < 0 && (idx = add_var(ctx, fd, name)) >= 0) {
        fd->func_var_idx = idx;
        fd->vars[idx].var_kind = JS_VAR_FUNCTION_NAME;
        if (fd->js_mode & JS_MODE_STRICT)
            fd->vars[idx].is_const = TRUE;
    }
    return idx;
}

static int add_arguments_var(JSContext *ctx, JSFunctionDef *fd)
{
    int idx = fd->arguments_var_idx;
    if (idx < 0 && (idx = add_var(ctx, fd, JS_ATOM_arguments)) >= 0) {
        fd->arguments_var_idx = idx;
    }
    return idx;
}

/* add an argument definition in the argument scope. Only needed when
   "eval()" may be called in the argument scope. Return 0 if OK. */
static int add_arguments_arg(JSContext *ctx, JSFunctionDef *fd)
{
    int idx;
    if (fd->arguments_arg_idx < 0) {
        idx = find_var_in_scope(ctx, fd, JS_ATOM_arguments, ARG_SCOPE_INDEX);
        if (idx < 0) {
            /* XXX: the scope links are not fully updated. May be an
               issue if there are child scopes of the argument
               scope */
            idx = add_var(ctx, fd, JS_ATOM_arguments);
            if (idx < 0)
                return -1;
            fd->vars[idx].scope_next = fd->scopes[ARG_SCOPE_INDEX].first;
            fd->scopes[ARG_SCOPE_INDEX].first = idx;
            fd->vars[idx].scope_level = ARG_SCOPE_INDEX;
            fd->vars[idx].is_lexical = TRUE;

            fd->arguments_arg_idx = idx;
        }
    }
    return 0;
}

static int add_arg(JSContext *ctx, JSFunctionDef *fd, JSAtom name)
{
    JSVarDef *vd;

    /* the local variable indexes are currently stored on 16 bits */
    if (fd->arg_count >= JS_MAX_LOCAL_VARS) {
        JS_ThrowInternalError(ctx, "too many arguments");
        return -1;
    }
    if (js_resize_array(ctx, (void **)&fd->args, sizeof(fd->args[0]),
                        &fd->arg_size, fd->arg_count + 1))
        return -1;
    vd = &fd->args[fd->arg_count++];
    memset(vd, 0, sizeof(*vd));
    vd->var_name = JS_DupAtom(ctx, name);
    vd->func_pool_idx = -1;
    return fd->arg_count - 1;
}

/* add a global variable definition */
static JSGlobalVar *add_global_var(JSContext *ctx, JSFunctionDef *s,
                                     JSAtom name)
{
    JSGlobalVar *hf;

    if (js_resize_array(ctx, (void **)&s->global_vars,
                        sizeof(s->global_vars[0]),
                        &s->global_var_size, s->global_var_count + 1))
        return NULL;
    hf = &s->global_vars[s->global_var_count++];
    hf->cpool_idx = -1;
    hf->force_init = FALSE;
    hf->is_lexical = FALSE;
    hf->is_const = FALSE;
    hf->scope_level = s->scope_level;
    hf->var_name = JS_DupAtom(ctx, name);
    return hf;
}

typedef enum {
    JS_VAR_DEF_WITH,
    JS_VAR_DEF_LET,
    JS_VAR_DEF_CONST,
    JS_VAR_DEF_FUNCTION_DECL, /* function declaration */
    JS_VAR_DEF_NEW_FUNCTION_DECL, /* async/generator function declaration */
    JS_VAR_DEF_CATCH,
    JS_VAR_DEF_VAR,
} JSVarDefEnum;

static int define_var(JSParseState *s, JSFunctionDef *fd, JSAtom name,
                      JSVarDefEnum var_def_type)
{
    JSContext *ctx = s->ctx;
    JSVarDef *vd;
    int idx;

    switch (var_def_type) {
    case JS_VAR_DEF_WITH:
        idx = add_scope_var(ctx, fd, name, JS_VAR_NORMAL);
        break;

    case JS_VAR_DEF_LET:
    case JS_VAR_DEF_CONST:
    case JS_VAR_DEF_FUNCTION_DECL:
    case JS_VAR_DEF_NEW_FUNCTION_DECL:
        idx = find_lexical_decl(ctx, fd, name, fd->scope_first, TRUE);
        if (idx >= 0) {
            if (idx < GLOBAL_VAR_OFFSET) {
                if (fd->vars[idx].scope_level == fd->scope_level) {
                    /* same scope: in non strict mode, functions
                       can be redefined (annex B.3.3.4). */
                    if (!(!(fd->js_mode & JS_MODE_STRICT) &&
                          var_def_type == JS_VAR_DEF_FUNCTION_DECL &&
                          fd->vars[idx].var_kind == JS_VAR_FUNCTION_DECL)) {
                        goto redef_lex_error;
                    }
                } else if (fd->vars[idx].var_kind == JS_VAR_CATCH && (fd->vars[idx].scope_level + 2) == fd->scope_level) {
                    goto redef_lex_error;
                }
            } else {
                if (fd->scope_level == fd->body_scope) {
                redef_lex_error:
                    /* redefining a scoped var in the same scope: error */
                    return js_parse_error(s, "invalid redefinition of lexical identifier");
                }
            }
        }
        if (var_def_type != JS_VAR_DEF_FUNCTION_DECL &&
            var_def_type != JS_VAR_DEF_NEW_FUNCTION_DECL &&
            fd->scope_level == fd->body_scope &&
            find_arg(ctx, fd, name) >= 0) {
            /* lexical variable redefines a parameter name */
            return js_parse_error(s, "invalid redefinition of parameter name");
        }

        if (find_var_in_child_scope(ctx, fd, name, fd->scope_level) >= 0) {
            return js_parse_error(s, "invalid redefinition of a variable");
        }
        
        if (fd->is_global_var) {
            JSGlobalVar *hf;
            hf = find_global_var(fd, name);
            if (hf && is_child_scope(ctx, fd, hf->scope_level,
                                     fd->scope_level)) {
                return js_parse_error(s, "invalid redefinition of global identifier");
            }
        }
        
        if (fd->is_eval &&
            (fd->eval_type == JS_EVAL_TYPE_GLOBAL ||
             fd->eval_type == JS_EVAL_TYPE_MODULE) &&
            fd->scope_level == fd->body_scope) {
            JSGlobalVar *hf;
            hf = add_global_var(s->ctx, fd, name);
            if (!hf)
                return -1;
            hf->is_lexical = TRUE;
            hf->is_const = (var_def_type == JS_VAR_DEF_CONST);
            idx = GLOBAL_VAR_OFFSET;
        } else {
            JSVarKindEnum var_kind;
            if (var_def_type == JS_VAR_DEF_FUNCTION_DECL)
                var_kind = JS_VAR_FUNCTION_DECL;
            else if (var_def_type == JS_VAR_DEF_NEW_FUNCTION_DECL)
                var_kind = JS_VAR_NEW_FUNCTION_DECL;
            else
                var_kind = JS_VAR_NORMAL;
            idx = add_scope_var(ctx, fd, name, var_kind);
            if (idx >= 0) {
                vd = &fd->vars[idx];
                vd->is_lexical = 1;
                vd->is_const = (var_def_type == JS_VAR_DEF_CONST);
            }
        }
        break;

    case JS_VAR_DEF_CATCH:
        idx = add_scope_var(ctx, fd, name, JS_VAR_CATCH);
        break;

    case JS_VAR_DEF_VAR:
        if (find_lexical_decl(ctx, fd, name, fd->scope_first,
                              FALSE) >= 0) {
       invalid_lexical_redefinition:
            /* error to redefine a var that inside a lexical scope */
            return js_parse_error(s, "invalid redefinition of lexical identifier");
        }
        if (fd->is_global_var) {
            JSGlobalVar *hf;
            hf = find_global_var(fd, name);
            if (hf && hf->is_lexical && hf->scope_level == fd->scope_level &&
                fd->eval_type == JS_EVAL_TYPE_MODULE) {
                goto invalid_lexical_redefinition;
            }
            hf = add_global_var(s->ctx, fd, name);
            if (!hf)
                return -1;
            idx = GLOBAL_VAR_OFFSET;
        } else {
            /* if the variable already exists, don't add it again  */
            idx = find_var(ctx, fd, name);
            if (idx >= 0)
                break;
            idx = add_var(ctx, fd, name);
            if (idx >= 0) {
                if (name == JS_ATOM_arguments && fd->has_arguments_binding)
                    fd->arguments_var_idx = idx;
                fd->vars[idx].scope_next = fd->scope_level;
            }
        }
        break;
    default:
        abort();
    }
    return idx;
}

/* add a private field variable in the current scope */
static int add_private_class_field(JSParseState *s, JSFunctionDef *fd,
                                   JSAtom name, JSVarKindEnum var_kind)
{
    JSContext *ctx = s->ctx;
    JSVarDef *vd;
    int idx;

    idx = add_scope_var(ctx, fd, name, var_kind);
    if (idx < 0)
        return idx;
    vd = &fd->vars[idx];
    vd->is_lexical = 1;
    vd->is_const = 1;
    return idx;
}

static __exception int js_parse_expr(JSParseState *s);
static __exception int js_parse_function_decl(JSParseState *s,
                                              JSParseFunctionEnum func_type,
                                              JSFunctionKindEnum func_kind,
                                              JSAtom func_name, const uint8_t *ptr,
                                              int start_line);
static JSFunctionDef *js_parse_function_class_fields_init(JSParseState *s);
static __exception int js_parse_function_decl2(JSParseState *s,
                                               JSParseFunctionEnum func_type,
                                               JSFunctionKindEnum func_kind,
                                               JSAtom func_name,
                                               const uint8_t *ptr,
                                               int function_line_num,
                                               JSParseExportEnum export_flag,
                                               JSFunctionDef **pfd);
static __exception int js_parse_assign_expr2(JSParseState *s, int parse_flags);
static __exception int js_parse_assign_expr(JSParseState *s);
static __exception int js_parse_unary(JSParseState *s, int parse_flags);
static void push_break_entry(JSFunctionDef *fd, BlockEnv *be,
                             JSAtom label_name,
                             int label_break, int label_cont,
                             int drop_count);
static void pop_break_entry(JSFunctionDef *fd);
static JSExportEntry *add_export_entry(JSParseState *s, JSModuleDef *m,
                                       JSAtom local_name, JSAtom export_name,
                                       JSExportTypeEnum export_type);

/* Note: all the fields are already sealed except length */
static int seal_template_obj(JSContext *ctx, JSValueConst obj)
{
    JSObject *p;
    JSShapeProperty *prs;

    p = JS_VALUE_GET_OBJ(obj);
    prs = find_own_property1(p, JS_ATOM_length);
    if (prs) {
        if (js_update_property_flags(ctx, p, &prs,
                                     prs->flags & ~(JS_PROP_CONFIGURABLE | JS_PROP_WRITABLE)))
            return -1;
    }
    p->extensible = FALSE;
    return 0;
}

static __exception int js_parse_template(JSParseState *s, int call, int *argc)
{
    JSContext *ctx = s->ctx;
    JSValue raw_array, template_object;
    JSToken cooked;
    int depth, ret;

    raw_array = JS_UNDEFINED; /* avoid warning */
    template_object = JS_UNDEFINED; /* avoid warning */
    if (call) {
        /* Create a template object: an array of cooked strings */
        /* Create an array of raw strings and store it to the raw property */
        template_object = JS_NewArray(ctx);
        if (JS_IsException(template_object))
            return -1;
        //        pool_idx = s->cur_func->cpool_count;
        ret = emit_push_const(s, template_object, 0);
        JS_FreeValue(ctx, template_object);
        if (ret)
            return -1;
        raw_array = JS_NewArray(ctx);
        if (JS_IsException(raw_array))
            return -1;
        if (JS_DefinePropertyValue(ctx, template_object, JS_ATOM_raw,
                                   raw_array, JS_PROP_THROW) < 0) {
            return -1;
        }
    }

    depth = 0;
    while (s->token.val == TOK_TEMPLATE) {
        const uint8_t *p = s->token.ptr + 1;
        cooked = s->token;
        if (call) {
            if (JS_DefinePropertyValueUint32(ctx, raw_array, depth,
                                             JS_DupValue(ctx, s->token.u.str.str),
                                             JS_PROP_ENUMERABLE | JS_PROP_THROW) < 0) {
                return -1;
            }
            /* re-parse the string with escape sequences but do not throw a
               syntax error if it contains invalid sequences
             */
            if (js_parse_string(s, '`', FALSE, p, &cooked, &p)) {
                cooked.u.str.str = JS_UNDEFINED;
            }
            if (JS_DefinePropertyValueUint32(ctx, template_object, depth,
                                             cooked.u.str.str,
                                             JS_PROP_ENUMERABLE | JS_PROP_THROW) < 0) {
                return -1;
            }
        } else {
            JSString *str;
            /* re-parse the string with escape sequences and throw a
               syntax error if it contains invalid sequences
             */
            JS_FreeValue(ctx, s->token.u.str.str);
            s->token.u.str.str = JS_UNDEFINED;
            if (js_parse_string(s, '`', TRUE, p, &cooked, &p))
                return -1;
            str = JS_VALUE_GET_STRING(cooked.u.str.str);
            if (str->len != 0 || depth == 0) {
                ret = emit_push_const(s, cooked.u.str.str, 1);
                JS_FreeValue(s->ctx, cooked.u.str.str);
                if (ret)
                    return -1;
                if (depth == 0) {
                    if (s->token.u.str.sep == '`')
                        goto done1;
                    emit_op(s, OP_get_field2);
                    emit_atom(s, JS_ATOM_concat);
                }
                depth++;
            } else {
                JS_FreeValue(s->ctx, cooked.u.str.str);
            }
        }
        if (s->token.u.str.sep == '`')
            goto done;
        if (next_token(s))
            return -1;
        if (js_parse_expr(s))
            return -1;
        depth++;
        if (s->token.val != '}') {
            return js_parse_error(s, "expected '}' after template expression");
        }
        /* XXX: should convert to string at this stage? */
        free_token(s, &s->token);
        /* Resume TOK_TEMPLATE parsing (s->token.line_num and
         * s->token.ptr are OK) */
        s->got_lf = FALSE;
        s->last_line_num = s->token.line_num;
        if (js_parse_template_part(s, s->buf_ptr))
            return -1;
    }
    return js_parse_expect(s, TOK_TEMPLATE);

 done:
    if (call) {
        /* Seal the objects */
        seal_template_obj(ctx, raw_array);
        seal_template_obj(ctx, template_object);
        *argc = depth + 1;
    } else {
        emit_op(s, OP_call_method);
        emit_u16(s, depth - 1);
    }
 done1:
    return next_token(s);
}


#define PROP_TYPE_IDENT 0
#define PROP_TYPE_VAR   1
#define PROP_TYPE_GET   2
#define PROP_TYPE_SET   3
#define PROP_TYPE_STAR  4
#define PROP_TYPE_ASYNC 5
#define PROP_TYPE_ASYNC_STAR 6

#define PROP_TYPE_PRIVATE (1 << 4)

static BOOL token_is_ident(int tok)
{
    /* Accept keywords and reserved words as property names */
    return (tok == TOK_IDENT ||
            (tok >= TOK_FIRST_KEYWORD &&
             tok <= TOK_LAST_KEYWORD));
}

/* if the property is an expression, name = JS_ATOM_NULL */
static int __exception js_parse_property_name(JSParseState *s,
                                              JSAtom *pname,
                                              BOOL allow_method, BOOL allow_var,
                                              BOOL allow_private)
{
    int is_private = 0;
    BOOL is_non_reserved_ident;
    JSAtom name;
    int prop_type;
    
    prop_type = PROP_TYPE_IDENT;
    if (allow_method) {
        if (token_is_pseudo_keyword(s, JS_ATOM_get)
        ||  token_is_pseudo_keyword(s, JS_ATOM_set)) {
            /* get x(), set x() */
            name = JS_DupAtom(s->ctx, s->token.u.ident.atom);
            if (next_token(s))
                goto fail1;
            if (s->token.val == ':' || s->token.val == ',' ||
                s->token.val == '}' || s->token.val == '(') {
                is_non_reserved_ident = TRUE;
                goto ident_found;
            }
            prop_type = PROP_TYPE_GET + (name == JS_ATOM_set);
            JS_FreeAtom(s->ctx, name);
        } else if (s->token.val == '*') {
            if (next_token(s))
                goto fail;
            prop_type = PROP_TYPE_STAR;
        } else if (token_is_pseudo_keyword(s, JS_ATOM_async) &&
                   peek_token(s, TRUE) != '\n') {
            name = JS_DupAtom(s->ctx, s->token.u.ident.atom);
            if (next_token(s))
                goto fail1;
            if (s->token.val == ':' || s->token.val == ',' ||
                s->token.val == '}' || s->token.val == '(') {
                is_non_reserved_ident = TRUE;
                goto ident_found;
            }
            JS_FreeAtom(s->ctx, name);
            if (s->token.val == '*') {
                if (next_token(s))
                    goto fail;
                prop_type = PROP_TYPE_ASYNC_STAR;
            } else {
                prop_type = PROP_TYPE_ASYNC;
            }
        }
    }

    if (token_is_ident(s->token.val)) {
        /* variable can only be a non-reserved identifier */
        is_non_reserved_ident =
            (s->token.val == TOK_IDENT && !s->token.u.ident.is_reserved);
        /* keywords and reserved words have a valid atom */
        name = JS_DupAtom(s->ctx, s->token.u.ident.atom);
        if (next_token(s))
            goto fail1;
    ident_found:
        if (is_non_reserved_ident &&
            prop_type == PROP_TYPE_IDENT && allow_var) {
            if (!(s->token.val == ':' ||
                  (s->token.val == '(' && allow_method))) {
                prop_type = PROP_TYPE_VAR;
            }
        }
    } else if (s->token.val == TOK_STRING) {
        name = JS_ValueToAtom(s->ctx, s->token.u.str.str);
        if (name == JS_ATOM_NULL)
            goto fail;
        if (next_token(s))
            goto fail1;
    } else if (s->token.val == TOK_NUMBER) {
        JSValue val;
        val = s->token.u.num.val;
#ifdef CONFIG_BIGNUM
        if (JS_VALUE_GET_TAG(val) == JS_TAG_BIG_FLOAT) {
            JSBigFloat *p = JS_VALUE_GET_PTR(val);
            val = s->ctx->rt->bigfloat_ops.
                mul_pow10_to_float64(s->ctx, &p->num,
                                     s->token.u.num.exponent);
            if (JS_IsException(val))
                goto fail;
            name = JS_ValueToAtom(s->ctx, val);
            JS_FreeValue(s->ctx, val);
        } else
#endif
        {
            name = JS_ValueToAtom(s->ctx, val);
        }
        if (name == JS_ATOM_NULL)
            goto fail;
        if (next_token(s))
            goto fail1;
    } else if (s->token.val == '[') {
        if (next_token(s))
            goto fail;
        if (js_parse_expr(s))
            goto fail;
        if (js_parse_expect(s, ']'))
            goto fail;
        name = JS_ATOM_NULL;
    } else if (s->token.val == TOK_PRIVATE_NAME && allow_private) {
        name = JS_DupAtom(s->ctx, s->token.u.ident.atom);
        if (next_token(s))
            goto fail1;
        is_private = PROP_TYPE_PRIVATE;
    } else {
        goto invalid_prop;
    }
    if (prop_type != PROP_TYPE_IDENT && prop_type != PROP_TYPE_VAR &&
        s->token.val != '(') {
        JS_FreeAtom(s->ctx, name);
    invalid_prop:
        js_parse_error(s, "invalid property name");
        goto fail;
    }
    *pname = name;
    return prop_type | is_private;
 fail1:
    JS_FreeAtom(s->ctx, name);
 fail:
    *pname = JS_ATOM_NULL;
    return -1;
}

typedef struct JSParsePos {
    int last_line_num;
    int line_num;
    BOOL got_lf;
    const uint8_t *ptr;
} JSParsePos;

static int js_parse_get_pos(JSParseState *s, JSParsePos *sp)
{
    sp->last_line_num = s->last_line_num;
    sp->line_num = s->token.line_num;
    sp->ptr = s->token.ptr;
    sp->got_lf = s->got_lf;
    return 0;
}

static __exception int js_parse_seek_token(JSParseState *s, const JSParsePos *sp)
{
    s->token.line_num = sp->last_line_num;
    s->line_num = sp->line_num;
    s->buf_ptr = sp->ptr;
    s->got_lf = sp->got_lf;
    return next_token(s);
}

/* return TRUE if a regexp literal is allowed after this token */
static BOOL is_regexp_allowed(int tok)
{
    switch (tok) {
    case TOK_NUMBER:
    case TOK_STRING:
    case TOK_REGEXP:
    case TOK_DEC:
    case TOK_INC:
    case TOK_NULL:
    case TOK_FALSE:
    case TOK_TRUE:
    case TOK_THIS:
    case ')':
    case ']':
    case '}': /* XXX: regexp may occur after */
    case TOK_IDENT:
        return FALSE;
    default:
        return TRUE;
    }
}

#define SKIP_HAS_SEMI       (1 << 0)
#define SKIP_HAS_ELLIPSIS   (1 << 1)
#define SKIP_HAS_ASSIGNMENT (1 << 2)

/* XXX: improve speed with early bailout */
/* XXX: no longer works if regexps are present. Could use previous
   regexp parsing heuristics to handle most cases */
static int js_parse_skip_parens_token(JSParseState *s, int *pbits, BOOL no_line_terminator)
{
    char state[256];
    size_t level = 0;
    JSParsePos pos;
    int last_tok, tok = TOK_EOF;
    int c, tok_len, bits = 0;

    /* protect from underflow */
    state[level++] = 0;

    js_parse_get_pos(s, &pos);
    last_tok = 0;
    for (;;) {
        switch(s->token.val) {
        case '(':
        case '[':
        case '{':
            if (level >= sizeof(state))
                goto done;
            state[level++] = s->token.val;
            break;
        case ')':
            if (state[--level] != '(')
                goto done;
            break;
        case ']':
            if (state[--level] != '[')
                goto done;
            break;
        case '}':
            c = state[--level];
            if (c == '`') {
                /* continue the parsing of the template */
                free_token(s, &s->token);
                /* Resume TOK_TEMPLATE parsing (s->token.line_num and
                 * s->token.ptr are OK) */
                s->got_lf = FALSE;
                s->last_line_num = s->token.line_num;
                if (js_parse_template_part(s, s->buf_ptr))
                    goto done;
                goto handle_template;
            } else if (c != '{') {
                goto done;
            }
            break;
        case TOK_TEMPLATE:
        handle_template:
            if (s->token.u.str.sep != '`') {
                /* '${' inside the template : closing '}' and continue
                   parsing the template */
                if (level >= sizeof(state))
                    goto done;
                state[level++] = '`';
            } 
            break;
        case TOK_EOF:
            goto done;
        case ';':
            if (level == 2) {
                bits |= SKIP_HAS_SEMI;
            }
            break;
        case TOK_ELLIPSIS:
            if (level == 2) {
                bits |= SKIP_HAS_ELLIPSIS;
            }
            break;
        case '=':
            bits |= SKIP_HAS_ASSIGNMENT;
            break;
            
        case TOK_DIV_ASSIGN:
            tok_len = 2;
            goto parse_regexp;
        case '/':
            tok_len = 1;
        parse_regexp:
            if (is_regexp_allowed(last_tok)) {
                s->buf_ptr -= tok_len;
                if (js_parse_regexp(s)) {
                    /* XXX: should clear the exception */
                    goto done;
                }
            }
            break;
        }
        /* last_tok is only used to recognize regexps */
        if (s->token.val == TOK_IDENT &&
            (token_is_pseudo_keyword(s, JS_ATOM_of) ||
             token_is_pseudo_keyword(s, JS_ATOM_yield))) {
            last_tok = TOK_OF;
        } else {
            last_tok = s->token.val;
        }
        if (next_token(s)) {
            /* XXX: should clear the exception generated by next_token() */
            break;
        }
        if (level <= 1) {
            tok = s->token.val;
            if (token_is_pseudo_keyword(s, JS_ATOM_of))
                tok = TOK_OF;
            if (no_line_terminator && s->last_line_num != s->token.line_num)
                tok = '\n';
            break;
        }
    }
 done:
    if (pbits) {
        *pbits = bits;
    }
    if (js_parse_seek_token(s, &pos))
        return -1;
    return tok;
}

static void set_object_name(JSParseState *s, JSAtom name)
{
    JSFunctionDef *fd = s->cur_func;
    int opcode;

    opcode = get_prev_opcode(fd);
    if (opcode == OP_set_name) {
        /* XXX: should free atom after OP_set_name? */
        fd->byte_code.size = fd->last_opcode_pos;
        fd->last_opcode_pos = -1;
        emit_op(s, OP_set_name);
        emit_atom(s, name);
    } else if (opcode == OP_set_class_name) {
        int define_class_pos;
        JSAtom atom;
        define_class_pos = fd->last_opcode_pos + 1 -
            get_u32(fd->byte_code.buf + fd->last_opcode_pos + 1);
        assert(fd->byte_code.buf[define_class_pos] == OP_define_class);
        /* for consistency we free the previous atom which is
           JS_ATOM_empty_string */
        atom = get_u32(fd->byte_code.buf + define_class_pos + 1);
        JS_FreeAtom(s->ctx, atom);
        put_u32(fd->byte_code.buf + define_class_pos + 1,
                JS_DupAtom(s->ctx, name));
        fd->last_opcode_pos = -1;
    }
}

static void set_object_name_computed(JSParseState *s)
{
    JSFunctionDef *fd = s->cur_func;
    int opcode;

    opcode = get_prev_opcode(fd);
    if (opcode == OP_set_name) {
        /* XXX: should free atom after OP_set_name? */
        fd->byte_code.size = fd->last_opcode_pos;
        fd->last_opcode_pos = -1;
        emit_op(s, OP_set_name_computed);
    } else if (opcode == OP_set_class_name) {
        int define_class_pos;
        define_class_pos = fd->last_opcode_pos + 1 -
            get_u32(fd->byte_code.buf + fd->last_opcode_pos + 1);
        assert(fd->byte_code.buf[define_class_pos] == OP_define_class);
        fd->byte_code.buf[define_class_pos] = OP_define_class_computed;
        fd->last_opcode_pos = -1;
    }
}

static __exception int js_parse_object_literal(JSParseState *s)
{
    JSAtom name = JS_ATOM_NULL;
    const uint8_t *start_ptr;
    int start_line, prop_type;
    BOOL has_proto;

    if (next_token(s))
        goto fail;
    /* XXX: add an initial length that will be patched back */
    emit_op(s, OP_object);
    has_proto = FALSE;
    while (s->token.val != '}') {
        /* specific case for getter/setter */
        start_ptr = s->token.ptr;
        start_line = s->token.line_num;

        if (s->token.val == TOK_ELLIPSIS) {
            if (next_token(s))
                return -1;
            if (js_parse_assign_expr(s))
                return -1;
            emit_op(s, OP_null);  /* dummy excludeList */
            emit_op(s, OP_copy_data_properties);
            emit_u8(s, 2 | (1 << 2) | (0 << 5));
            emit_op(s, OP_drop); /* pop excludeList */
            emit_op(s, OP_drop); /* pop src object */
            goto next;
        }

        prop_type = js_parse_property_name(s, &name, TRUE, TRUE, FALSE);
        if (prop_type < 0)
            goto fail;

        if (prop_type == PROP_TYPE_VAR) {
            /* shortcut for x: x */
            emit_op(s, OP_scope_get_var);
            emit_atom(s, name);
            emit_u16(s, s->cur_func->scope_level);
            emit_op(s, OP_define_field);
            emit_atom(s, name);
        } else if (s->token.val == '(') {
            BOOL is_getset = (prop_type == PROP_TYPE_GET ||
                              prop_type == PROP_TYPE_SET);
            JSParseFunctionEnum func_type;
            JSFunctionKindEnum func_kind;
            int op_flags;

            func_kind = JS_FUNC_NORMAL;
            if (is_getset) {
                func_type = JS_PARSE_FUNC_GETTER + prop_type - PROP_TYPE_GET;
            } else {
                func_type = JS_PARSE_FUNC_METHOD;
                if (prop_type == PROP_TYPE_STAR)
                    func_kind = JS_FUNC_GENERATOR;
                else if (prop_type == PROP_TYPE_ASYNC)
                    func_kind = JS_FUNC_ASYNC;
                else if (prop_type == PROP_TYPE_ASYNC_STAR)
                    func_kind = JS_FUNC_ASYNC_GENERATOR;
            }
            if (js_parse_function_decl(s, func_type, func_kind, JS_ATOM_NULL,
                                       start_ptr, start_line))
                goto fail;
            if (name == JS_ATOM_NULL) {
                emit_op(s, OP_define_method_computed);
            } else {
                emit_op(s, OP_define_method);
                emit_atom(s, name);
            }
            if (is_getset) {
                op_flags = OP_DEFINE_METHOD_GETTER +
                    prop_type - PROP_TYPE_GET;
            } else {
                op_flags = OP_DEFINE_METHOD_METHOD;
            }
            emit_u8(s, op_flags | OP_DEFINE_METHOD_ENUMERABLE);
        } else {
            if (js_parse_expect(s, ':'))
                goto fail;
            if (js_parse_assign_expr(s))
                goto fail;
            if (name == JS_ATOM_NULL) {
                set_object_name_computed(s);
                emit_op(s, OP_define_array_el);
                emit_op(s, OP_drop);
            } else if (name == JS_ATOM___proto__) {
                if (has_proto) {
                    js_parse_error(s, "duplicate __proto__ property name");
                    goto fail;
                }
                emit_op(s, OP_set_proto);
                has_proto = TRUE;
            } else {
                set_object_name(s, name);
                emit_op(s, OP_define_field);
                emit_atom(s, name);
            }
        }
        JS_FreeAtom(s->ctx, name);
    next:
        name = JS_ATOM_NULL;
        if (s->token.val != ',')
            break;
        if (next_token(s))
            goto fail;
    }
    if (js_parse_expect(s, '}'))
        goto fail;
    return 0;
 fail:
    JS_FreeAtom(s->ctx, name);
    return -1;
}

/* allow the 'in' binary operator */
#define PF_IN_ACCEPTED  (1 << 0) 
/* allow function calls parsing in js_parse_postfix_expr() */
#define PF_POSTFIX_CALL (1 << 1) 
/* allow arrow functions parsing in js_parse_postfix_expr() */
#define PF_ARROW_FUNC   (1 << 2) 
/* allow the exponentiation operator in js_parse_unary() */
#define PF_POW_ALLOWED  (1 << 3) 
/* forbid the exponentiation operator in js_parse_unary() */
#define PF_POW_FORBIDDEN (1 << 4) 

static __exception int js_parse_postfix_expr(JSParseState *s, int parse_flags);

static __exception int js_parse_left_hand_side_expr(JSParseState *s)
{
    return js_parse_postfix_expr(s, PF_POSTFIX_CALL);
}

/* XXX: could generate specific bytecode */
static __exception int js_parse_class_default_ctor(JSParseState *s,
                                                   BOOL has_super,
                                                   JSFunctionDef **pfd)
{
    JSParsePos pos;
    const char *str;
    int ret, line_num;
    JSParseFunctionEnum func_type;
    const uint8_t *saved_buf_end;
    
    js_parse_get_pos(s, &pos);
    if (has_super) {
        /* spec change: no argument evaluation */
        str = "(){super(...arguments);}";
        func_type = JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR;
    } else {
        str = "(){}";
        func_type = JS_PARSE_FUNC_CLASS_CONSTRUCTOR;
    }
    line_num = s->token.line_num;
    saved_buf_end = s->buf_end;
    s->buf_ptr = (uint8_t *)str;
    s->buf_end = (uint8_t *)(str + strlen(str));
    ret = next_token(s);
    if (!ret) {
        ret = js_parse_function_decl2(s, func_type, JS_FUNC_NORMAL,
                                      JS_ATOM_NULL, (uint8_t *)str,
                                      line_num, JS_PARSE_EXPORT_NONE, pfd);
    }
    s->buf_end = saved_buf_end;
    ret |= js_parse_seek_token(s, &pos);
    return ret;
}

/* find field in the current scope */
static int find_private_class_field(JSContext *ctx, JSFunctionDef *fd,
                                    JSAtom name, int scope_level)
{
    int idx;
    idx = fd->scopes[scope_level].first;
    while (idx != -1) {
        if (fd->vars[idx].scope_level != scope_level)
            break;
        if (fd->vars[idx].var_name == name)
            return idx;
        idx = fd->vars[idx].scope_next;
    }
    return -1;
}

/* initialize the class fields, called by the constructor. Note:
   super() can be called in an arrow function, so <this> and
   <class_fields_init> can be variable references */
static void emit_class_field_init(JSParseState *s)
{
    int label_next;
    
    emit_op(s, OP_scope_get_var);
    emit_atom(s, JS_ATOM_class_fields_init);
    emit_u16(s, s->cur_func->scope_level);

    /* no need to call the class field initializer if not defined */
    emit_op(s, OP_dup);
    label_next = emit_goto(s, OP_if_false, -1);
    
    emit_op(s, OP_scope_get_var);
    emit_atom(s, JS_ATOM_this);
    emit_u16(s, 0);
    
    emit_op(s, OP_swap);
    
    emit_op(s, OP_call_method);
    emit_u16(s, 0);

    emit_label(s, label_next);
    emit_op(s, OP_drop);
}

/* build a private setter function name from the private getter name */
static JSAtom get_private_setter_name(JSContext *ctx, JSAtom name)
{
    return js_atom_concat_str(ctx, name, "<set>");
}

typedef struct {
    JSFunctionDef *fields_init_fd;
    int computed_fields_count;
    BOOL has_brand;
    int brand_push_pos;
} ClassFieldsDef;

static __exception int emit_class_init_start(JSParseState *s,
                                             ClassFieldsDef *cf)
{
    int label_add_brand;
    
    cf->fields_init_fd = js_parse_function_class_fields_init(s);
    if (!cf->fields_init_fd)
        return -1;

    s->cur_func = cf->fields_init_fd;
    
    /* XXX: would be better to add the code only if needed, maybe in a
       later pass */
    emit_op(s, OP_push_false); /* will be patched later */
    cf->brand_push_pos = cf->fields_init_fd->last_opcode_pos;
    label_add_brand = emit_goto(s, OP_if_false, -1);
    
    emit_op(s, OP_scope_get_var);
    emit_atom(s, JS_ATOM_this);
    emit_u16(s, 0);
    
    emit_op(s, OP_scope_get_var);
    emit_atom(s, JS_ATOM_home_object);
    emit_u16(s, 0);
    
    emit_op(s, OP_add_brand);
    
    emit_label(s, label_add_brand);

    s->cur_func = s->cur_func->parent;
    return 0;
}

static __exception int add_brand(JSParseState *s, ClassFieldsDef *cf)
{
    if (!cf->has_brand) {
        /* define the brand field in 'this' of the initializer */
        if (!cf->fields_init_fd) {
            if (emit_class_init_start(s, cf))
                return -1;
        }
        /* patch the start of the function to enable the OP_add_brand code */
        cf->fields_init_fd->byte_code.buf[cf->brand_push_pos] = OP_push_true;
        
        cf->has_brand = TRUE;
    }
    return 0;
}

static void emit_class_init_end(JSParseState *s, ClassFieldsDef *cf)
{
    int cpool_idx;
        
    s->cur_func = cf->fields_init_fd;
    emit_op(s, OP_return_undef);
    s->cur_func = s->cur_func->parent;
    
    cpool_idx = cpool_add(s, JS_NULL);
    cf->fields_init_fd->parent_cpool_idx = cpool_idx;
    emit_op(s, OP_fclosure);
    emit_u32(s, cpool_idx);
    emit_op(s, OP_set_home_object);
}


static __exception int js_parse_class(JSParseState *s, BOOL is_class_expr,
                                      JSParseExportEnum export_flag)
{
    JSContext *ctx = s->ctx;
    JSFunctionDef *fd = s->cur_func;
    JSAtom name = JS_ATOM_NULL, class_name = JS_ATOM_NULL, class_name1;
    JSAtom class_var_name = JS_ATOM_NULL;
    JSFunctionDef *method_fd, *ctor_fd;
    int saved_js_mode, class_name_var_idx, prop_type, ctor_cpool_offset;
    int class_flags = 0, i, define_class_offset;
    BOOL is_static, is_private;
    const uint8_t *class_start_ptr = s->token.ptr;
    const uint8_t *start_ptr;
    ClassFieldsDef class_fields[2];
        
    /* classes are parsed and executed in strict mode */
    saved_js_mode = fd->js_mode;
    fd->js_mode |= JS_MODE_STRICT;
    if (next_token(s))
        goto fail;
    if (s->token.val == TOK_IDENT) {
        if (s->token.u.ident.is_reserved) {
            js_parse_error_reserved_identifier(s);
            goto fail;
        }
        class_name = JS_DupAtom(ctx, s->token.u.ident.atom);
        if (next_token(s))
            goto fail;
    } else if (!is_class_expr && export_flag != JS_PARSE_EXPORT_DEFAULT) {
        js_parse_error(s, "class statement requires a name");
        goto fail;
    }
    if (!is_class_expr) {
        if (class_name == JS_ATOM_NULL)
            class_var_name = JS_ATOM__default_; /* export default */
        else
            class_var_name = class_name;
        class_var_name = JS_DupAtom(ctx, class_var_name);
    }

    push_scope(s);

    if (s->token.val == TOK_EXTENDS) {
        class_flags = JS_DEFINE_CLASS_HAS_HERITAGE;
        if (next_token(s))
            goto fail;
        if (js_parse_left_hand_side_expr(s))
            goto fail;
    } else {
        emit_op(s, OP_undefined);
    }

    /* add a 'const' definition for the class name */
    if (class_name != JS_ATOM_NULL) {
        class_name_var_idx = define_var(s, fd, class_name, JS_VAR_DEF_CONST);
        if (class_name_var_idx < 0)
            goto fail;
    }

    if (js_parse_expect(s, '{'))
        goto fail;

    /* this scope contains the private fields */
    push_scope(s);

    emit_op(s, OP_push_const);
    ctor_cpool_offset = fd->byte_code.size;
    emit_u32(s, 0); /* will be patched at the end of the class parsing */

    if (class_name == JS_ATOM_NULL) {
        if (class_var_name != JS_ATOM_NULL)
            class_name1 = JS_ATOM_default;
        else
            class_name1 = JS_ATOM_empty_string;
    } else {
        class_name1 = class_name;
    }
    
    emit_op(s, OP_define_class);
    emit_atom(s, class_name1);
    emit_u8(s, class_flags);
    define_class_offset = fd->last_opcode_pos;

    for(i = 0; i < 2; i++) {
        ClassFieldsDef *cf = &class_fields[i];
        cf->fields_init_fd = NULL;
        cf->computed_fields_count = 0;
        cf->has_brand = FALSE;
    }
    
    ctor_fd = NULL;
    while (s->token.val != '}') {
        if (s->token.val == ';') {
            if (next_token(s))
                goto fail;
            continue;
        }
        is_static = (s->token.val == TOK_STATIC);
        prop_type = -1;
        if (is_static) {
            if (next_token(s))
                goto fail;
            /* allow "static" field name */
            if (s->token.val == ';' || s->token.val == '=') {
                is_static = FALSE;
                name = JS_DupAtom(ctx, JS_ATOM_static);
                prop_type = PROP_TYPE_IDENT;
            }
        }
        if (is_static)
            emit_op(s, OP_swap);
        start_ptr = s->token.ptr;
        if (prop_type < 0) {
            prop_type = js_parse_property_name(s, &name, TRUE, FALSE, TRUE);
            if (prop_type < 0)
                goto fail;
        }
        is_private = prop_type & PROP_TYPE_PRIVATE;
        prop_type &= ~PROP_TYPE_PRIVATE;
        
        if ((name == JS_ATOM_constructor && !is_static &&
             prop_type != PROP_TYPE_IDENT) ||
            (name == JS_ATOM_prototype && is_static) ||
            name == JS_ATOM_hash_constructor) {
            js_parse_error(s, "invalid method name");
            goto fail;
        }
        if (prop_type == PROP_TYPE_GET || prop_type == PROP_TYPE_SET) {
            BOOL is_set = prop_type - PROP_TYPE_GET;
            JSFunctionDef *method_fd;

            if (is_private) {
                int idx, var_kind;
                idx = find_private_class_field(ctx, fd, name, fd->scope_level);
                if (idx >= 0) {
                    var_kind = fd->vars[idx].var_kind;
                    if (var_kind == JS_VAR_PRIVATE_FIELD ||
                        var_kind == JS_VAR_PRIVATE_METHOD ||
                        var_kind == JS_VAR_PRIVATE_GETTER_SETTER ||
                        var_kind == (JS_VAR_PRIVATE_GETTER + is_set)) {
                        goto private_field_already_defined;
                    }
                    fd->vars[idx].var_kind = JS_VAR_PRIVATE_GETTER_SETTER;
                } else {
                    if (add_private_class_field(s, fd, name,
                                                JS_VAR_PRIVATE_GETTER + is_set) < 0)
                        goto fail;
                }
                if (add_brand(s, &class_fields[is_static]) < 0)
                    goto fail;
            }

            if (js_parse_function_decl2(s, JS_PARSE_FUNC_GETTER + is_set,
                                        JS_FUNC_NORMAL, JS_ATOM_NULL,
                                        start_ptr, s->token.line_num,
                                        JS_PARSE_EXPORT_NONE, &method_fd))
                goto fail;
            if (is_private) {
                method_fd->need_home_object = TRUE; /* needed for brand check */
                emit_op(s, OP_set_home_object);
                /* XXX: missing function name */
                emit_op(s, OP_scope_put_var_init);
                if (is_set) {
                    JSAtom setter_name;
                    int ret;
                    
                    setter_name = get_private_setter_name(ctx, name);
                    if (setter_name == JS_ATOM_NULL)
                        goto fail;
                    emit_atom(s, setter_name);
                    ret = add_private_class_field(s, fd, setter_name,
                                                  JS_VAR_PRIVATE_SETTER);
                    JS_FreeAtom(ctx, setter_name);
                    if (ret < 0)
                        goto fail;
                } else {
                    emit_atom(s, name);
                }
                emit_u16(s, s->cur_func->scope_level);
            } else {
                if (name == JS_ATOM_NULL) {
                    emit_op(s, OP_define_method_computed);
                } else {
                    emit_op(s, OP_define_method);
                    emit_atom(s, name);
                }
                emit_u8(s, OP_DEFINE_METHOD_GETTER + is_set);
            }
        } else if (prop_type == PROP_TYPE_IDENT && s->token.val != '(') {
            ClassFieldsDef *cf = &class_fields[is_static];
            JSAtom field_var_name = JS_ATOM_NULL;
            
            /* class field */

            /* XXX: spec: not consistent with method name checks */
            if (name == JS_ATOM_constructor || name == JS_ATOM_prototype) {
                js_parse_error(s, "invalid field name");
                goto fail;
            }
            
            if (is_private) {
                if (find_private_class_field(ctx, fd, name,
                                             fd->scope_level) >= 0) {
                    goto private_field_already_defined;
                }
                if (add_private_class_field(s, fd, name,
                                            JS_VAR_PRIVATE_FIELD) < 0)
                    goto fail;
                emit_op(s, OP_private_symbol);
                emit_atom(s, name);
                emit_op(s, OP_scope_put_var_init);
                emit_atom(s, name);
                emit_u16(s, s->cur_func->scope_level);
            }

            if (!cf->fields_init_fd) {
                if (emit_class_init_start(s, cf))
                    goto fail;
            }
            if (name == JS_ATOM_NULL ) {
                /* save the computed field name into a variable */
                field_var_name = js_atom_concat_num(ctx, JS_ATOM_computed_field + is_static, cf->computed_fields_count);
                if (field_var_name == JS_ATOM_NULL)
                    goto fail;
                if (define_var(s, fd, field_var_name, JS_VAR_DEF_CONST) < 0) {
                    JS_FreeAtom(ctx, field_var_name);
                    goto fail;
                }
                emit_op(s, OP_to_propkey);
                emit_op(s, OP_scope_put_var_init);
                emit_atom(s, field_var_name);
                emit_u16(s, s->cur_func->scope_level);
            }
            s->cur_func = cf->fields_init_fd;
            emit_op(s, OP_scope_get_var);
            emit_atom(s, JS_ATOM_this);
            emit_u16(s, 0);

            if (name == JS_ATOM_NULL) {
                emit_op(s, OP_scope_get_var);
                emit_atom(s, field_var_name);
                emit_u16(s, s->cur_func->scope_level);
                cf->computed_fields_count++;
                JS_FreeAtom(ctx, field_var_name);
            } else if (is_private) {
                emit_op(s, OP_scope_get_var);
                emit_atom(s, name);
                emit_u16(s, s->cur_func->scope_level);
            }
            
            if (s->token.val == '=') {
                if (next_token(s))
                    goto fail;
                if (js_parse_assign_expr(s))
                    goto fail;
            } else {
                emit_op(s, OP_undefined);
            }
            if (is_private) {
                set_object_name_computed(s);
                emit_op(s, OP_define_private_field);
            } else if (name == JS_ATOM_NULL) {
                set_object_name_computed(s);
                emit_op(s, OP_define_array_el);
                emit_op(s, OP_drop);
            } else {
                set_object_name(s, name);
                emit_op(s, OP_define_field);
                emit_atom(s, name);
            }
            s->cur_func = s->cur_func->parent;
            if (js_parse_expect_semi(s))
                goto fail;
        } else {
            JSParseFunctionEnum func_type;
            JSFunctionKindEnum func_kind;
            
            func_type = JS_PARSE_FUNC_METHOD;
            func_kind = JS_FUNC_NORMAL;
            if (prop_type == PROP_TYPE_STAR) {
                func_kind = JS_FUNC_GENERATOR;
            } else if (prop_type == PROP_TYPE_ASYNC) {
                func_kind = JS_FUNC_ASYNC;
            } else if (prop_type == PROP_TYPE_ASYNC_STAR) {
                func_kind = JS_FUNC_ASYNC_GENERATOR;
            } else if (name == JS_ATOM_constructor && !is_static) {
                if (ctor_fd) {
                    js_parse_error(s, "property constructor appears more than once");
                    goto fail;
                }
                if (class_flags & JS_DEFINE_CLASS_HAS_HERITAGE)
                    func_type = JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR;
                else
                    func_type = JS_PARSE_FUNC_CLASS_CONSTRUCTOR;
            }
            if (is_private) {
                if (add_brand(s, &class_fields[is_static]) < 0)
                    goto fail;
            }
            if (js_parse_function_decl2(s, func_type, func_kind, JS_ATOM_NULL, start_ptr, s->token.line_num, JS_PARSE_EXPORT_NONE, &method_fd))
                goto fail;
            if (func_type == JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR ||
                func_type == JS_PARSE_FUNC_CLASS_CONSTRUCTOR) {
                ctor_fd = method_fd;
            } else if (is_private) {
                method_fd->need_home_object = TRUE; /* needed for brand check */
                if (find_private_class_field(ctx, fd, name,
                                             fd->scope_level) >= 0) {
                private_field_already_defined:
                    js_parse_error(s, "private class field is already defined");
                    goto fail;
                }
                if (add_private_class_field(s, fd, name,
                                            JS_VAR_PRIVATE_METHOD) < 0)
                    goto fail;
                emit_op(s, OP_set_home_object);
                emit_op(s, OP_set_name);
                emit_atom(s, name);
                emit_op(s, OP_scope_put_var_init);
                emit_atom(s, name);
                emit_u16(s, s->cur_func->scope_level);
            } else {
                if (name == JS_ATOM_NULL) {
                    emit_op(s, OP_define_method_computed);
                } else {
                    emit_op(s, OP_define_method);
                    emit_atom(s, name);
                }
                emit_u8(s, OP_DEFINE_METHOD_METHOD);
            }
        }
        if (is_static)
            emit_op(s, OP_swap);
        JS_FreeAtom(ctx, name);
        name = JS_ATOM_NULL;
    }

    if (s->token.val != '}') {
        js_parse_error(s, "expecting '%c'", '}');
        goto fail;
    }

    if (!ctor_fd) {
        if (js_parse_class_default_ctor(s, class_flags & JS_DEFINE_CLASS_HAS_HERITAGE, &ctor_fd))
            goto fail;
    }
    /* patch the constant pool index for the constructor */
    put_u32(fd->byte_code.buf + ctor_cpool_offset, ctor_fd->parent_cpool_idx);

    /* store the class source code in the constructor. */
    if (!(fd->js_mode & JS_MODE_STRIP)) {
        js_free(ctx, ctor_fd->source);
        ctor_fd->source_len = s->buf_ptr - class_start_ptr;
        ctor_fd->source = js_strndup(ctx, (const char *)class_start_ptr,
                                     ctor_fd->source_len);
        if (!ctor_fd->source)
            goto fail;
    }

    /* consume the '}' */
    if (next_token(s))
        goto fail;

    /* store the function to initialize the fields to that it can be
       referenced by the constructor */
    {
        ClassFieldsDef *cf = &class_fields[0];
        int var_idx;
        
        var_idx = define_var(s, fd, JS_ATOM_class_fields_init,
                             JS_VAR_DEF_CONST);
        if (var_idx < 0)
            goto fail;
        if (cf->fields_init_fd) {
            emit_class_init_end(s, cf);
        } else {
            emit_op(s, OP_undefined);
        }
        emit_op(s, OP_scope_put_var_init);
        emit_atom(s, JS_ATOM_class_fields_init);
        emit_u16(s, s->cur_func->scope_level);
    }

    /* drop the prototype */
    emit_op(s, OP_drop);

    /* initialize the static fields */
    if (class_fields[1].fields_init_fd != NULL) {
        ClassFieldsDef *cf = &class_fields[1];
        emit_op(s, OP_dup);
        emit_class_init_end(s, cf);
        emit_op(s, OP_call_method);
        emit_u16(s, 0);
        emit_op(s, OP_drop);
    }
    
    if (class_name != JS_ATOM_NULL) {
        /* store the class name in the scoped class name variable (it
           is independent from the class statement variable
           definition) */
        emit_op(s, OP_dup);
        emit_op(s, OP_scope_put_var_init);
        emit_atom(s, class_name);
        emit_u16(s, fd->scope_level);
    }
    pop_scope(s);
    pop_scope(s);

    /* the class statements have a block level scope */
    if (class_var_name != JS_ATOM_NULL) {
        if (define_var(s, fd, class_var_name, JS_VAR_DEF_LET) < 0)
            goto fail;
        emit_op(s, OP_scope_put_var_init);
        emit_atom(s, class_var_name);
        emit_u16(s, fd->scope_level);
    } else {
        if (class_name == JS_ATOM_NULL) {
            /* cannot use OP_set_name because the name of the class
               must be defined before the static initializers are
               executed */
            emit_op(s, OP_set_class_name);
            emit_u32(s, fd->last_opcode_pos + 1 - define_class_offset);
        }
    }

    if (export_flag != JS_PARSE_EXPORT_NONE) {
        if (!add_export_entry(s, fd->module,
                              class_var_name,
                              export_flag == JS_PARSE_EXPORT_NAMED ? class_var_name : JS_ATOM_default,
                              JS_EXPORT_TYPE_LOCAL))
            goto fail;
    }

    JS_FreeAtom(ctx, class_name);
    JS_FreeAtom(ctx, class_var_name);
    fd->js_mode = saved_js_mode;
    return 0;
 fail:
    JS_FreeAtom(ctx, name);
    JS_FreeAtom(ctx, class_name);
    JS_FreeAtom(ctx, class_var_name);
    fd->js_mode = saved_js_mode;
    return -1;
}

static __exception int js_parse_array_literal(JSParseState *s)
{
    uint32_t idx;
    BOOL need_length;

    if (next_token(s))
        return -1;
    /* small regular arrays are created on the stack */
    idx = 0;
    while (s->token.val != ']' && idx < 32) {
        if (s->token.val == ',' || s->token.val == TOK_ELLIPSIS)
            break;
        if (js_parse_assign_expr(s))
            return -1;
        idx++;
        /* accept trailing comma */
        if (s->token.val == ',') {
            if (next_token(s))
                return -1;
        } else
        if (s->token.val != ']')
            goto done;
    }
    emit_op(s, OP_array_from);
    emit_u16(s, idx);

    /* larger arrays and holes are handled with explicit indices */
    need_length = FALSE;
    while (s->token.val != ']' && idx < 0x7fffffff) {
        if (s->token.val == TOK_ELLIPSIS)
            break;
        need_length = TRUE;
        if (s->token.val != ',') {
            if (js_parse_assign_expr(s))
                return -1;
            emit_op(s, OP_define_field);
            emit_u32(s, __JS_AtomFromUInt32(idx));
            need_length = FALSE;
        }
        idx++;
        /* accept trailing comma */
        if (s->token.val == ',') {
            if (next_token(s))
                return -1;
        }
    }
    if (s->token.val == ']') {
        if (need_length) {
            /* Set the length: Cannot use OP_define_field because
               length is not configurable */
            emit_op(s, OP_dup);
            emit_op(s, OP_push_i32);
            emit_u32(s, idx);
            emit_op(s, OP_put_field);
            emit_atom(s, JS_ATOM_length);
        }
        goto done;
    }

    /* huge arrays and spread elements require a dynamic index on the stack */
    emit_op(s, OP_push_i32);
    emit_u32(s, idx);

    /* stack has array, index */
    while (s->token.val != ']') {
        if (s->token.val == TOK_ELLIPSIS) {
            if (next_token(s))
                return -1;
            if (js_parse_assign_expr(s))
                return -1;
#if 1
            emit_op(s, OP_append);
#else
            int label_next, label_done;
            label_next = new_label(s);
            label_done = new_label(s);
            /* enumerate object */
            emit_op(s, OP_for_of_start);
            emit_op(s, OP_rot5l);
            emit_op(s, OP_rot5l);
            emit_label(s, label_next);
            /* on stack: enum_rec array idx */
            emit_op(s, OP_for_of_next);
            emit_u8(s, 2);
            emit_goto(s, OP_if_true, label_done);
            /* append element */
            /* enum_rec array idx val -> enum_rec array new_idx */
            emit_op(s, OP_define_array_el);
            emit_op(s, OP_inc);
            emit_goto(s, OP_goto, label_next);
            emit_label(s, label_done);
            /* close enumeration */
            emit_op(s, OP_drop); /* drop undef val */
            emit_op(s, OP_nip1); /* drop enum_rec */
            emit_op(s, OP_nip1);
            emit_op(s, OP_nip1);
#endif
        } else {
            need_length = TRUE;
            if (s->token.val != ',') {
                if (js_parse_assign_expr(s))
                    return -1;
                /* a idx val */
                emit_op(s, OP_define_array_el);
                need_length = FALSE;
            }
            emit_op(s, OP_inc);
        }
        if (s->token.val != ',')
            break;
        if (next_token(s))
            return -1;
    }
    if (need_length) {
        /* Set the length: cannot use OP_define_field because
           length is not configurable */
        emit_op(s, OP_dup1);    /* array length - array array length */
        emit_op(s, OP_put_field);
        emit_atom(s, JS_ATOM_length);
    } else {
        emit_op(s, OP_drop);    /* array length - array */
    }
done:
    return js_parse_expect(s, ']');
}

/* XXX: remove */
static BOOL has_with_scope(JSFunctionDef *s, int scope_level)
{
    /* check if scope chain contains a with statement */
    while (s) {
        int scope_idx = s->scopes[scope_level].first;
        while (scope_idx >= 0) {
            JSVarDef *vd = &s->vars[scope_idx];

            if (vd->var_name == JS_ATOM__with_)
                return TRUE;
            scope_idx = vd->scope_next;
        }
        /* check parent scopes */
        scope_level = s->parent_scope_level;
        s = s->parent;
    }
    return FALSE;
}

static __exception int get_lvalue(JSParseState *s, int *popcode, int *pscope,
                                  JSAtom *pname, int *plabel, int *pdepth, BOOL keep,
                                  int tok)
{
    JSFunctionDef *fd;
    int opcode, scope, label, depth;
    JSAtom name;

    /* we check the last opcode to get the lvalue type */
    fd = s->cur_func;
    scope = 0;
    name = JS_ATOM_NULL;
    label = -1;
    depth = 0;
    switch(opcode = get_prev_opcode(fd)) {
    case OP_scope_get_var:
        name = get_u32(fd->byte_code.buf + fd->last_opcode_pos + 1);
        scope = get_u16(fd->byte_code.buf + fd->last_opcode_pos + 5);
        if ((name == JS_ATOM_arguments || name == JS_ATOM_eval) &&
            (fd->js_mode & JS_MODE_STRICT)) {
            return js_parse_error(s, "invalid lvalue in strict mode");
        }
        if (name == JS_ATOM_this || name == JS_ATOM_new_target)
            goto invalid_lvalue;
        depth = 2;  /* will generate OP_get_ref_value */
        break;
    case OP_get_field:
        name = get_u32(fd->byte_code.buf + fd->last_opcode_pos + 1);
        depth = 1;
        break;
    case OP_scope_get_private_field:
        name = get_u32(fd->byte_code.buf + fd->last_opcode_pos + 1);
        scope = get_u16(fd->byte_code.buf + fd->last_opcode_pos + 5);
        depth = 1;
        break;
    case OP_get_array_el:
        depth = 2;
        break;
    case OP_get_super_value:
        depth = 3;
        break;
    default:
    invalid_lvalue:
        if (tok == TOK_FOR) {
            return js_parse_error(s, "invalid for in/of left hand-side");
        } else if (tok == TOK_INC || tok == TOK_DEC) {
            return js_parse_error(s, "invalid increment/decrement operand");
        } else if (tok == '[' || tok == '{') {
            return js_parse_error(s, "invalid destructuring target");
        } else {
            return js_parse_error(s, "invalid assignment left-hand side");
        }
    }
    /* remove the last opcode */
    fd->byte_code.size = fd->last_opcode_pos;
    fd->last_opcode_pos = -1;

    if (keep) {
        /* get the value but keep the object/fields on the stack */
        switch(opcode) {
        case OP_scope_get_var:
            label = new_label(s);
            emit_op(s, OP_scope_make_ref);
            emit_atom(s, name);
            emit_u32(s, label);
            emit_u16(s, scope);
            update_label(fd, label, 1);
            emit_op(s, OP_get_ref_value);
            opcode = OP_get_ref_value;
            break;
        case OP_get_field:
            emit_op(s, OP_get_field2);
            emit_atom(s, name);
            break;
        case OP_scope_get_private_field:
            emit_op(s, OP_scope_get_private_field2);
            emit_atom(s, name);
            emit_u16(s, scope);
            break;
        case OP_get_array_el:
            /* XXX: replace by a single opcode ? */
            emit_op(s, OP_to_propkey2);
            emit_op(s, OP_dup2);
            emit_op(s, OP_get_array_el);
            break;
        case OP_get_super_value:
            emit_op(s, OP_to_propkey);
            emit_op(s, OP_dup3);
            emit_op(s, OP_get_super_value);
            break;
        default:
            abort();
        }
    } else {
        switch(opcode) {
        case OP_scope_get_var:
            label = new_label(s);
            emit_op(s, OP_scope_make_ref);
            emit_atom(s, name);
            emit_u32(s, label);
            emit_u16(s, scope);
            update_label(fd, label, 1);
            opcode = OP_get_ref_value;
            break;
        case OP_get_array_el:
            emit_op(s, OP_to_propkey2);
            break;
        case OP_get_super_value:
            emit_op(s, OP_to_propkey);
            break;
        }
    }

    *popcode = opcode;
    *pscope = scope;
    /* name has refcount for OP_get_field and OP_get_ref_value,
       and JS_ATOM_NULL for other opcodes */
    *pname = name;
    *plabel = label;
    if (pdepth)
        *pdepth = depth;
    return 0;
}

typedef enum {
    PUT_LVALUE_NOKEEP, /* [depth] v -> */
    PUT_LVALUE_NOKEEP_DEPTH, /* [depth] v -> , keep depth (currently
                                just disable optimizations) */
    PUT_LVALUE_KEEP_TOP,  /* [depth] v -> v */
    PUT_LVALUE_KEEP_SECOND, /* [depth] v0 v -> v0 */
    PUT_LVALUE_NOKEEP_BOTTOM, /* v [depth] -> */
} PutLValueEnum;

/* name has a live reference. 'is_let' is only used with opcode =
   OP_scope_get_var which is never generated by get_lvalue(). */
static void put_lvalue(JSParseState *s, int opcode, int scope,
                       JSAtom name, int label, PutLValueEnum special,
                       BOOL is_let)
{
    switch(opcode) {
    case OP_get_field:
    case OP_scope_get_private_field:
        /* depth = 1 */
        switch(special) {
        case PUT_LVALUE_NOKEEP:
        case PUT_LVALUE_NOKEEP_DEPTH:
            break;
        case PUT_LVALUE_KEEP_TOP:
            emit_op(s, OP_insert2); /* obj v -> v obj v */
            break;
        case PUT_LVALUE_KEEP_SECOND:
            emit_op(s, OP_perm3); /* obj v0 v -> v0 obj v */
            break;
        case PUT_LVALUE_NOKEEP_BOTTOM:
            emit_op(s, OP_swap);
            break;
        default:
            abort();
        }
        break;
    case OP_get_array_el:
    case OP_get_ref_value:
        /* depth = 2 */
        if (opcode == OP_get_ref_value) {
            JS_FreeAtom(s->ctx, name);
            emit_label(s, label);
        }
        switch(special) {
        case PUT_LVALUE_NOKEEP:
            emit_op(s, OP_nop); /* will trigger optimization */
            break;
        case PUT_LVALUE_NOKEEP_DEPTH:
            break;
        case PUT_LVALUE_KEEP_TOP:
            emit_op(s, OP_insert3); /* obj prop v -> v obj prop v */
            break;
        case PUT_LVALUE_KEEP_SECOND:
            emit_op(s, OP_perm4); /* obj prop v0 v -> v0 obj prop v */
            break;
        case PUT_LVALUE_NOKEEP_BOTTOM:
            emit_op(s, OP_rot3l);
            break;
        default:
            abort();
        }
        break;
    case OP_get_super_value:
        /* depth = 3 */
        switch(special) {
        case PUT_LVALUE_NOKEEP:
        case PUT_LVALUE_NOKEEP_DEPTH:
            break;
        case PUT_LVALUE_KEEP_TOP:
            emit_op(s, OP_insert4); /* this obj prop v -> v this obj prop v */
            break;
        case PUT_LVALUE_KEEP_SECOND:
            emit_op(s, OP_perm5); /* this obj prop v0 v -> v0 this obj prop v */
            break;
        case PUT_LVALUE_NOKEEP_BOTTOM:
            emit_op(s, OP_rot4l);
            break;
        default:
            abort();
        }
        break;
    default:
        break;
    }
    
    switch(opcode) {
    case OP_scope_get_var:  /* val -- */
        assert(special == PUT_LVALUE_NOKEEP ||
               special == PUT_LVALUE_NOKEEP_DEPTH);
        emit_op(s, is_let ? OP_scope_put_var_init : OP_scope_put_var);
        emit_u32(s, name);  /* has refcount */
        emit_u16(s, scope);
        break;
    case OP_get_field:
        emit_op(s, OP_put_field);
        emit_u32(s, name);  /* name has refcount */
        break;
    case OP_scope_get_private_field:
        emit_op(s, OP_scope_put_private_field);
        emit_u32(s, name);  /* name has refcount */
        emit_u16(s, scope);
        break;
    case OP_get_array_el:
        emit_op(s, OP_put_array_el);
        break;
    case OP_get_ref_value:
        emit_op(s, OP_put_ref_value);
        break;
    case OP_get_super_value:
        emit_op(s, OP_put_super_value);
        break;
    default:
        abort();
    }
}

static __exception int js_parse_expr_paren(JSParseState *s)
{
    if (js_parse_expect(s, '('))
        return -1;
    if (js_parse_expr(s))
        return -1;
    if (js_parse_expect(s, ')'))
        return -1;
    return 0;
}

static int js_unsupported_keyword(JSParseState *s, JSAtom atom)
{
    char buf[ATOM_GET_STR_BUF_SIZE];
    return js_parse_error(s, "unsupported keyword: %s",
                          JS_AtomGetStr(s->ctx, buf, sizeof(buf), atom));
}

static __exception int js_define_var(JSParseState *s, JSAtom name, int tok)
{
    JSFunctionDef *fd = s->cur_func;
    JSVarDefEnum var_def_type;
    
    if (name == JS_ATOM_yield && fd->func_kind == JS_FUNC_GENERATOR) {
        return js_parse_error(s, "yield is a reserved identifier");
    }
    if ((name == JS_ATOM_arguments || name == JS_ATOM_eval)
    &&  (fd->js_mode & JS_MODE_STRICT)) {
        return js_parse_error(s, "invalid variable name in strict mode");
    }
    if ((name == JS_ATOM_let || name == JS_ATOM_undefined)
    &&  (tok == TOK_LET || tok == TOK_CONST)) {
        return js_parse_error(s, "invalid lexical variable name");
    }
    switch(tok) {
    case TOK_LET:
        var_def_type = JS_VAR_DEF_LET;
        break;
    case TOK_CONST:
        var_def_type = JS_VAR_DEF_CONST;
        break;
    case TOK_VAR:
        var_def_type = JS_VAR_DEF_VAR;
        break;
    case TOK_CATCH:
        var_def_type = JS_VAR_DEF_CATCH;
        break;
    default:
        abort();
    }
    if (define_var(s, fd, name, var_def_type) < 0)
        return -1;
    return 0;
}

static void js_emit_spread_code(JSParseState *s, int depth)
{
    int label_rest_next, label_rest_done;

    /* XXX: could check if enum object is an actual array and optimize
       slice extraction. enumeration record and target array are in a
       different order from OP_append case. */
    /* enum_rec xxx -- enum_rec xxx array 0 */
    emit_op(s, OP_array_from);
    emit_u16(s, 0);
    emit_op(s, OP_push_i32);
    emit_u32(s, 0);
    emit_label(s, label_rest_next = new_label(s));
    emit_op(s, OP_for_of_next);
    emit_u8(s, 2 + depth);
    label_rest_done = emit_goto(s, OP_if_true, -1);
    /* array idx val -- array idx */
    emit_op(s, OP_define_array_el);
    emit_op(s, OP_inc);
    emit_goto(s, OP_goto, label_rest_next);
    emit_label(s, label_rest_done);
    /* enum_rec xxx array idx undef -- enum_rec xxx array */
    emit_op(s, OP_drop);
    emit_op(s, OP_drop);
}

static int js_parse_check_duplicate_parameter(JSParseState *s, JSAtom name)
{
    /* Check for duplicate parameter names */
    JSFunctionDef *fd = s->cur_func;
    int i;
    for (i = 0; i < fd->arg_count; i++) {
        if (fd->args[i].var_name == name)
            goto duplicate;
    }
    for (i = 0; i < fd->var_count; i++) {
        if (fd->vars[i].var_name == name)
            goto duplicate;
    }
    return 0;

duplicate:
    return js_parse_error(s, "duplicate parameter names not allowed in this context");
}

static JSAtom js_parse_destructuring_var(JSParseState *s, int tok, int is_arg)
{
    JSAtom name;

    if (!(s->token.val == TOK_IDENT && !s->token.u.ident.is_reserved)
    ||  ((s->cur_func->js_mode & JS_MODE_STRICT) &&
         (s->token.u.ident.atom == JS_ATOM_eval || s->token.u.ident.atom == JS_ATOM_arguments))) {
        js_parse_error(s, "invalid destructuring target");
        return JS_ATOM_NULL;
    }
    name = JS_DupAtom(s->ctx, s->token.u.ident.atom);
    if (is_arg && js_parse_check_duplicate_parameter(s, name))
        goto fail;
    if (next_token(s))
        goto fail;

    return name;
fail:
    JS_FreeAtom(s->ctx, name);
    return JS_ATOM_NULL;
}

/* Return -1 if error, 0 if no initializer, 1 if an initializer is
   present at the top level. */
static int js_parse_destructuring_element(JSParseState *s, int tok, int is_arg,
                                        int hasval, int has_ellipsis,
                                        BOOL allow_initializer)
{
    int label_parse, label_assign, label_done, label_lvalue, depth_lvalue;
    int start_addr, assign_addr;
    JSAtom prop_name, var_name;
    int opcode, scope, tok1, skip_bits;
    BOOL has_initializer;
    
    if (has_ellipsis < 0) {
        /* pre-parse destructuration target for spread detection */
        js_parse_skip_parens_token(s, &skip_bits, FALSE);
        has_ellipsis = skip_bits & SKIP_HAS_ELLIPSIS;
    }

    label_parse = new_label(s);
    label_assign = new_label(s);

    start_addr = s->cur_func->byte_code.size;
    if (hasval) {
        /* consume value from the stack */
        emit_op(s, OP_dup);
        emit_op(s, OP_undefined);
        emit_op(s, OP_strict_eq);
        emit_goto(s, OP_if_true, label_parse);
        emit_label(s, label_assign);
    } else {
        emit_goto(s, OP_goto, label_parse);
        emit_label(s, label_assign);
        /* leave value on the stack */
        emit_op(s, OP_dup);
    }
    assign_addr = s->cur_func->byte_code.size;
    if (s->token.val == '{') {
        if (next_token(s))
            return -1;
        /* throw an exception if the value cannot be converted to an object */
        emit_op(s, OP_to_object);
        if (has_ellipsis) {
            /* add excludeList on stack just below src object */
            emit_op(s, OP_object);
            emit_op(s, OP_swap);
        }
        while (s->token.val != '}') {
            int prop_type;
            if (s->token.val == TOK_ELLIPSIS) {
                if (!has_ellipsis) {
                    JS_ThrowInternalError(s->ctx, "unexpected ellipsis token");
                    return -1;
                }
                if (next_token(s))
                    return -1;
                if (tok) {
                    var_name = js_parse_destructuring_var(s, tok, is_arg);
                    if (var_name == JS_ATOM_NULL)
                        return -1;
                    opcode = OP_scope_get_var;
                    scope = s->cur_func->scope_level;
                    label_lvalue = -1;
                    depth_lvalue = 0;
                } else {
                    if (js_parse_left_hand_side_expr(s))
                        return -1;

                    if (get_lvalue(s, &opcode, &scope, &var_name,
                                   &label_lvalue, &depth_lvalue, FALSE, '{'))
                        return -1;
                }
                if (s->token.val != '}') {
                    js_parse_error(s, "assignment rest property must be last");
                    goto var_error;
                }
                emit_op(s, OP_object);  /* target */
                emit_op(s, OP_copy_data_properties);
                emit_u8(s, 0 | ((depth_lvalue + 1) << 2) | ((depth_lvalue + 2) << 5));
                goto set_val;
            }
            prop_type = js_parse_property_name(s, &prop_name, FALSE, TRUE, FALSE);
            if (prop_type < 0)
                return -1;
            var_name = JS_ATOM_NULL;
            opcode = OP_scope_get_var;
            scope = s->cur_func->scope_level;
            label_lvalue = -1;
            depth_lvalue = 0;
            if (prop_type == PROP_TYPE_IDENT) {
                if (next_token(s))
                    goto prop_error;
                if ((s->token.val == '[' || s->token.val == '{')
                    &&  ((tok1 = js_parse_skip_parens_token(s, &skip_bits, FALSE)) == ',' ||
                         tok1 == '=' || tok1 == '}')) {
                    if (prop_name == JS_ATOM_NULL) {
                        /* computed property name on stack */
                        if (has_ellipsis) {
                            /* define the property in excludeList */
                            emit_op(s, OP_to_propkey); /* avoid calling ToString twice */
                            emit_op(s, OP_perm3); /* TOS: src excludeList prop */
                            emit_op(s, OP_null); /* TOS: src excludeList prop null */
                            emit_op(s, OP_define_array_el); /* TOS: src excludeList prop */
                            emit_op(s, OP_perm3); /* TOS: excludeList src prop */
                        }
                        /* get the computed property from the source object */
                        emit_op(s, OP_get_array_el2);
                    } else {
                        /* named property */
                        if (has_ellipsis) {
                            /* define the property in excludeList */
                            emit_op(s, OP_swap); /* TOS: src excludeList */
                            emit_op(s, OP_null); /* TOS: src excludeList null */
                            emit_op(s, OP_define_field); /* TOS: src excludeList */
                            emit_atom(s, prop_name);
                            emit_op(s, OP_swap); /* TOS: excludeList src */
                        }
                        /* get the named property from the source object */
                        emit_op(s, OP_get_field2);
                        emit_u32(s, prop_name);
                    }
                    if (js_parse_destructuring_element(s, tok, is_arg, TRUE, -1, TRUE) < 0)
                        return -1;
                    if (s->token.val == '}')
                        break;
                    /* accept a trailing comma before the '}' */
                    if (js_parse_expect(s, ','))
                        return -1;
                    continue;
                }
                if (prop_name == JS_ATOM_NULL) {
                    emit_op(s, OP_to_propkey2);
                    if (has_ellipsis) {
                        /* define the property in excludeList */
                        emit_op(s, OP_perm3);
                        emit_op(s, OP_null);
                        emit_op(s, OP_define_array_el);
                        emit_op(s, OP_perm3);
                    }
                    /* source prop -- source source prop */
                    emit_op(s, OP_dup1);
                } else {
                    if (has_ellipsis) {
                        /* define the property in excludeList */
                        emit_op(s, OP_swap);
                        emit_op(s, OP_null);
                        emit_op(s, OP_define_field);
                        emit_atom(s, prop_name);
                        emit_op(s, OP_swap);
                    }
                    /* source -- source source */
                    emit_op(s, OP_dup);
                }
                if (tok) {
                    var_name = js_parse_destructuring_var(s, tok, is_arg);
                    if (var_name == JS_ATOM_NULL)
                        goto prop_error;
                } else {
                    if (js_parse_left_hand_side_expr(s))
                        goto prop_error;
                lvalue:
                    if (get_lvalue(s, &opcode, &scope, &var_name,
                                   &label_lvalue, &depth_lvalue, FALSE, '{'))
                        goto prop_error;
                    /* swap ref and lvalue object if any */
                    if (prop_name == JS_ATOM_NULL) {
                        switch(depth_lvalue) {
                        case 1:
                            /* source prop x -> x source prop */
                            emit_op(s, OP_rot3r);
                            break;
                        case 2:
                            /* source prop x y -> x y source prop */
                            emit_op(s, OP_swap2);   /* t p2 s p1 */
                            break;
                        case 3:
                            /* source prop x y z -> x y z source prop */
                            emit_op(s, OP_rot5l);
                            emit_op(s, OP_rot5l);
                            break;
                        }
                    } else {
                        switch(depth_lvalue) {
                        case 1:
                            /* source x -> x source */
                            emit_op(s, OP_swap);
                            break;
                        case 2:
                            /* source x y -> x y source */
                            emit_op(s, OP_rot3l);
                            break;
                        case 3:
                            /* source x y z -> x y z source */
                            emit_op(s, OP_rot4l);
                            break;
                        }
                    }
                }
                if (prop_name == JS_ATOM_NULL) {
                    /* computed property name on stack */
                    /* XXX: should have OP_get_array_el2x with depth */
                    /* source prop -- val */
                    emit_op(s, OP_get_array_el);
                } else {
                    /* named property */
                    /* XXX: should have OP_get_field2x with depth */
                    /* source -- val */
                    emit_op(s, OP_get_field);
                    emit_u32(s, prop_name);
                }
            } else {
                /* prop_type = PROP_TYPE_VAR, cannot be a computed property */
                if (is_arg && js_parse_check_duplicate_parameter(s, prop_name))
                    goto prop_error;
                if ((s->cur_func->js_mode & JS_MODE_STRICT) &&
                    (prop_name == JS_ATOM_eval || prop_name == JS_ATOM_arguments)) {
                    js_parse_error(s, "invalid destructuring target");
                    goto prop_error;
                }
                if (has_ellipsis) {
                    /* define the property in excludeList */
                    emit_op(s, OP_swap);
                    emit_op(s, OP_null);
                    emit_op(s, OP_define_field);
                    emit_atom(s, prop_name);
                    emit_op(s, OP_swap);
                }
                if (!tok || tok == TOK_VAR) {
                    /* generate reference */
                    /* source -- source source */
                    emit_op(s, OP_dup);
                    emit_op(s, OP_scope_get_var);
                    emit_atom(s, prop_name);
                    emit_u16(s, s->cur_func->scope_level);
                    goto lvalue;
                }
                var_name = JS_DupAtom(s->ctx, prop_name);
                /* source -- source val */
                emit_op(s, OP_get_field2);
                emit_u32(s, prop_name);
            }
        set_val:
            if (tok) {
                if (js_define_var(s, var_name, tok))
                    goto var_error;
                scope = s->cur_func->scope_level;
            }
            if (s->token.val == '=') {  /* handle optional default value */
                int label_hasval;
                emit_op(s, OP_dup);
                emit_op(s, OP_undefined);
                emit_op(s, OP_strict_eq);
                label_hasval = emit_goto(s, OP_if_false, -1);
                if (next_token(s))
                    goto var_error;
                emit_op(s, OP_drop);
                if (js_parse_assign_expr(s))
                    goto var_error;
                if (opcode == OP_scope_get_var || opcode == OP_get_ref_value)
                    set_object_name(s, var_name);
                emit_label(s, label_hasval);
            }
            /* store value into lvalue object */
            put_lvalue(s, opcode, scope, var_name, label_lvalue,
                       PUT_LVALUE_NOKEEP_DEPTH,
                       (tok == TOK_CONST || tok == TOK_LET));
            if (s->token.val == '}')
                break;
            /* accept a trailing comma before the '}' */
            if (js_parse_expect(s, ','))
                return -1;
        }
        /* drop the source object */
        emit_op(s, OP_drop);
        if (has_ellipsis) {
            emit_op(s, OP_drop); /* pop excludeList */
        }
        if (next_token(s))
            return -1;
    } else if (s->token.val == '[') {
        BOOL has_spread;
        int enum_depth;
        BlockEnv block_env;

        if (next_token(s))
            return -1;
        /* the block environment is only needed in generators in case
           'yield' triggers a 'return' */
        push_break_entry(s->cur_func, &block_env,
                         JS_ATOM_NULL, -1, -1, 2);
        block_env.has_iterator = TRUE;
        emit_op(s, OP_for_of_start);
        has_spread = FALSE;
        while (s->token.val != ']') {
            /* get the next value */
            if (s->token.val == TOK_ELLIPSIS) {
                if (next_token(s))
                    return -1;
                if (s->token.val == ',' || s->token.val == ']')
                    return js_parse_error(s, "missing binding pattern...");
                has_spread = TRUE;
            }
            if (s->token.val == ',') {
                /* do nothing, skip the value, has_spread is false */
                emit_op(s, OP_for_of_next);
                emit_u8(s, 0);
                emit_op(s, OP_drop);
                emit_op(s, OP_drop);
            } else if ((s->token.val == '[' || s->token.val == '{')
                   &&  ((tok1 = js_parse_skip_parens_token(s, &skip_bits, FALSE)) == ',' ||
                        tok1 == '=' || tok1 == ']')) {
                if (has_spread) {
                    if (tok1 == '=')
                        return js_parse_error(s, "rest element cannot have a default value");
                    js_emit_spread_code(s, 0);
                } else {
                    emit_op(s, OP_for_of_next);
                    emit_u8(s, 0);
                    emit_op(s, OP_drop);
                }
                if (js_parse_destructuring_element(s, tok, is_arg, TRUE, skip_bits & SKIP_HAS_ELLIPSIS, TRUE) < 0)
                    return -1;
            } else {
                var_name = JS_ATOM_NULL;
                enum_depth = 0;
                if (tok) {
                    var_name = js_parse_destructuring_var(s, tok, is_arg);
                    if (var_name == JS_ATOM_NULL)
                        goto var_error;
                    if (js_define_var(s, var_name, tok))
                        goto var_error;
                    opcode = OP_scope_get_var;
                    scope = s->cur_func->scope_level;
                } else {
                    if (js_parse_left_hand_side_expr(s))
                        return -1;
                    if (get_lvalue(s, &opcode, &scope, &var_name,
                                   &label_lvalue, &enum_depth, FALSE, '[')) {
                        return -1;
                    }
                }
                if (has_spread) {
                    js_emit_spread_code(s, enum_depth);
                } else {
                    emit_op(s, OP_for_of_next);
                    emit_u8(s, enum_depth);
                    emit_op(s, OP_drop);
                }
                if (s->token.val == '=' && !has_spread) {
                    /* handle optional default value */
                    int label_hasval;
                    emit_op(s, OP_dup);
                    emit_op(s, OP_undefined);
                    emit_op(s, OP_strict_eq);
                    label_hasval = emit_goto(s, OP_if_false, -1);
                    if (next_token(s))
                        goto var_error;
                    emit_op(s, OP_drop);
                    if (js_parse_assign_expr(s))
                        goto var_error;
                    if (opcode == OP_scope_get_var || opcode == OP_get_ref_value)
                        set_object_name(s, var_name);
                    emit_label(s, label_hasval);
                }
                /* store value into lvalue object */
                put_lvalue(s, opcode, scope, var_name,
                           label_lvalue, PUT_LVALUE_NOKEEP_DEPTH,
                           (tok == TOK_CONST || tok == TOK_LET));
            }
            if (s->token.val == ']')
                break;
            if (has_spread)
                return js_parse_error(s, "rest element must be the last one");
            /* accept a trailing comma before the ']' */
            if (js_parse_expect(s, ','))
                return -1;
        }
        /* close iterator object:
           if completed, enum_obj has been replaced by undefined */
        emit_op(s, OP_iterator_close);
        pop_break_entry(s->cur_func);
        if (next_token(s))
            return -1;
    } else {
        return js_parse_error(s, "invalid assignment syntax");
    }
    if (s->token.val == '=' && allow_initializer) {
        label_done = emit_goto(s, OP_goto, -1);
        if (next_token(s))
            return -1;
        emit_label(s, label_parse);
        if (hasval)
            emit_op(s, OP_drop);
        if (js_parse_assign_expr(s))
            return -1;
        emit_goto(s, OP_goto, label_assign);
        emit_label(s, label_done);
        has_initializer = TRUE;
    } else {
        /* normally hasval is true except if
           js_parse_skip_parens_token() was wrong in the parsing */
        //        assert(hasval);
        if (!hasval) {
            js_parse_error(s, "too complicated destructuring expression");
            return -1;
        }
        /* remove test and decrement label ref count */
        memset(s->cur_func->byte_code.buf + start_addr, OP_nop,
               assign_addr - start_addr);
        s->cur_func->label_slots[label_parse].ref_count--;
        has_initializer = FALSE;
    }
    return has_initializer;

 prop_error:
    JS_FreeAtom(s->ctx, prop_name);
 var_error:
    JS_FreeAtom(s->ctx, var_name);
    return -1;
}

typedef enum FuncCallType {
    FUNC_CALL_NORMAL,
    FUNC_CALL_NEW,
    FUNC_CALL_SUPER_CTOR,
    FUNC_CALL_TEMPLATE,
} FuncCallType;

static void optional_chain_test(JSParseState *s, int *poptional_chaining_label,
                                int drop_count)
{
    int label_next, i;
    if (*poptional_chaining_label < 0)
        *poptional_chaining_label = new_label(s);
   /* XXX: could be more efficient with a specific opcode */
    emit_op(s, OP_dup);
    emit_op(s, OP_is_undefined_or_null);
    label_next = emit_goto(s, OP_if_false, -1);
    for(i = 0; i < drop_count; i++)
        emit_op(s, OP_drop);
    emit_op(s, OP_undefined);
    emit_goto(s, OP_goto, *poptional_chaining_label);
    emit_label(s, label_next);
}

/* allowed parse_flags: PF_POSTFIX_CALL, PF_ARROW_FUNC */
static __exception int js_parse_postfix_expr(JSParseState *s, int parse_flags)
{
    FuncCallType call_type;
    int optional_chaining_label;
    BOOL accept_lparen = (parse_flags & PF_POSTFIX_CALL) != 0;
    
    call_type = FUNC_CALL_NORMAL;
    switch(s->token.val) {
    case TOK_NUMBER:
        {
            JSValue val;
            val = s->token.u.num.val;

            if (JS_VALUE_GET_TAG(val) == JS_TAG_INT) {
                emit_op(s, OP_push_i32);
                emit_u32(s, JS_VALUE_GET_INT(val));
            } else
#ifdef CONFIG_BIGNUM
            if (JS_VALUE_GET_TAG(val) == JS_TAG_BIG_FLOAT) {
                slimb_t e;
                int ret;

                /* need a runtime conversion */
                /* XXX: could add a cache and/or do it once at
                   the start of the function */
                if (emit_push_const(s, val, 0) < 0)
                    return -1;
                e = s->token.u.num.exponent;
                if (e == (int32_t)e) {
                    emit_op(s, OP_push_i32);
                    emit_u32(s, e);
                } else {
                    val = JS_NewBigInt64_1(s->ctx, e);
                    if (JS_IsException(val))
                        return -1;
                    ret = emit_push_const(s, val, 0);
                    JS_FreeValue(s->ctx, val);
                    if (ret < 0)
                        return -1;
                }
                emit_op(s, OP_mul_pow10);
            } else
#endif
            {
                if (emit_push_const(s, val, 0) < 0)
                    return -1;
            }
        }
        if (next_token(s))
            return -1;
        break;
    case TOK_TEMPLATE:
        if (js_parse_template(s, 0, NULL))
            return -1;
        break;
    case TOK_STRING:
        if (emit_push_const(s, s->token.u.str.str, 1))
            return -1;
        if (next_token(s))
            return -1;
        break;
        
    case TOK_DIV_ASSIGN:
        s->buf_ptr -= 2;
        goto parse_regexp;
    case '/':
        s->buf_ptr--;
    parse_regexp:
        {
            JSValue str;
            int ret, backtrace_flags;
            if (!s->ctx->compile_regexp)
                return js_parse_error(s, "RegExp are not supported");
            /* the previous token is '/' or '/=', so no need to free */
            if (js_parse_regexp(s))
                return -1;
            ret = emit_push_const(s, s->token.u.regexp.body, 0);
            str = s->ctx->compile_regexp(s->ctx, s->token.u.regexp.body,
                                         s->token.u.regexp.flags);
            if (JS_IsException(str)) {
                /* add the line number info */
                backtrace_flags = 0;
                if (s->cur_func && s->cur_func->backtrace_barrier)
                    backtrace_flags = JS_BACKTRACE_FLAG_SINGLE_LEVEL;
                build_backtrace(s->ctx, s->ctx->rt->current_exception,
                                s->filename, s->token.line_num,
                                backtrace_flags);
                return -1;
            }
            ret = emit_push_const(s, str, 0);
            JS_FreeValue(s->ctx, str);
            if (ret)
                return -1;
            /* we use a specific opcode to be sure the correct
               function is called (otherwise the bytecode would have
               to be verified by the RegExp constructor) */
            emit_op(s, OP_regexp);
            if (next_token(s))
                return -1;
        }
        break;
    case '(':
        if ((parse_flags & PF_ARROW_FUNC) &&
            js_parse_skip_parens_token(s, NULL, TRUE) == TOK_ARROW) {
            if (js_parse_function_decl(s, JS_PARSE_FUNC_ARROW,
                                       JS_FUNC_NORMAL, JS_ATOM_NULL,
                                       s->token.ptr, s->token.line_num))
                return -1;
        } else {
            if (js_parse_expr_paren(s))
                return -1;
        }
        break;
    case TOK_FUNCTION:
        if (js_parse_function_decl(s, JS_PARSE_FUNC_EXPR,
                                   JS_FUNC_NORMAL, JS_ATOM_NULL,
                                   s->token.ptr, s->token.line_num))
            return -1;
        break;
    case TOK_CLASS:
        if (js_parse_class(s, TRUE, JS_PARSE_EXPORT_NONE))
            return -1;
        break;
    case TOK_NULL:
        if (next_token(s))
            return -1;
        emit_op(s, OP_null);
        break;
    case TOK_THIS:
        if (next_token(s))
            return -1;
        emit_op(s, OP_scope_get_var);
        emit_atom(s, JS_ATOM_this);
        emit_u16(s, 0);
        break;
    case TOK_FALSE:
        if (next_token(s))
            return -1;
        emit_op(s, OP_push_false);
        break;
    case TOK_TRUE:
        if (next_token(s))
            return -1;
        emit_op(s, OP_push_true);
        break;
    case TOK_IDENT:
        {
            JSAtom name;
            if (s->token.u.ident.is_reserved) {
                return js_parse_error_reserved_identifier(s);
            }
            if ((parse_flags & PF_ARROW_FUNC) &&
                peek_token(s, TRUE) == TOK_ARROW) {
                if (js_parse_function_decl(s, JS_PARSE_FUNC_ARROW,
                                           JS_FUNC_NORMAL, JS_ATOM_NULL,
                                           s->token.ptr, s->token.line_num))
                    return -1;
            } else if (token_is_pseudo_keyword(s, JS_ATOM_async) &&
                       peek_token(s, TRUE) != '\n') {
                const uint8_t *source_ptr;
                int source_line_num;

                source_ptr = s->token.ptr;
                source_line_num = s->token.line_num;
                if (next_token(s))
                    return -1;
                if (s->token.val == TOK_FUNCTION) {
                    if (js_parse_function_decl(s, JS_PARSE_FUNC_EXPR,
                                               JS_FUNC_ASYNC, JS_ATOM_NULL,
                                               source_ptr, source_line_num))
                        return -1;
                } else if ((parse_flags & PF_ARROW_FUNC) &&
                           ((s->token.val == '(' &&
                             js_parse_skip_parens_token(s, NULL, TRUE) == TOK_ARROW) ||
                            (s->token.val == TOK_IDENT && !s->token.u.ident.is_reserved &&
                             peek_token(s, TRUE) == TOK_ARROW))) {
                    if (js_parse_function_decl(s, JS_PARSE_FUNC_ARROW,
                                               JS_FUNC_ASYNC, JS_ATOM_NULL,
                                               source_ptr, source_line_num))
                        return -1;
                } else {
                    name = JS_DupAtom(s->ctx, JS_ATOM_async);
                    goto do_get_var;
                }
            } else {
                if (s->token.u.ident.atom == JS_ATOM_arguments &&
                    !s->cur_func->arguments_allowed) {
                    js_parse_error(s, "'arguments' identifier is not allowed in class field initializer");
                    return -1;
                }
                name = JS_DupAtom(s->ctx, s->token.u.ident.atom);
                if (next_token(s))  /* update line number before emitting code */
                    return -1;
            do_get_var:
                emit_op(s, OP_scope_get_var);
                emit_u32(s, name);
                emit_u16(s, s->cur_func->scope_level);
            }
        }
        break;
    case '{':
    case '[':
        {
            int skip_bits;
            if (js_parse_skip_parens_token(s, &skip_bits, FALSE) == '=') {
                if (js_parse_destructuring_element(s, 0, 0, FALSE, skip_bits & SKIP_HAS_ELLIPSIS, TRUE) < 0)
                    return -1;
            } else {
                if (s->token.val == '{') {
                    if (js_parse_object_literal(s))
                        return -1;
                } else {
                    if (js_parse_array_literal(s))
                        return -1;
                }
            }
        }
        break;
    case TOK_NEW:
        if (next_token(s))
            return -1;
        if (s->token.val == '.') {
            if (next_token(s))
                return -1;
            if (!token_is_pseudo_keyword(s, JS_ATOM_target))
                return js_parse_error(s, "expecting target");
            if (!s->cur_func->new_target_allowed)
                return js_parse_error(s, "new.target only allowed within functions");
            if (next_token(s))
                return -1;
            emit_op(s, OP_scope_get_var);
            emit_atom(s, JS_ATOM_new_target);
            emit_u16(s, 0);
        } else {
            if (js_parse_postfix_expr(s, 0))
                return -1;
            accept_lparen = TRUE;
            if (s->token.val != '(') {
                /* new operator on an object */
                emit_op(s, OP_dup);
                emit_op(s, OP_call_constructor);
                emit_u16(s, 0);
            } else {
                call_type = FUNC_CALL_NEW;
            }
        }
        break;
    case TOK_SUPER:
        if (next_token(s))
            return -1;
        if (s->token.val == '(') {
            if (!s->cur_func->super_call_allowed)
                return js_parse_error(s, "super() is only valid in a derived class constructor");
            call_type = FUNC_CALL_SUPER_CTOR;
        } else if (s->token.val == '.' || s->token.val == '[') {
            if (!s->cur_func->super_allowed)
                return js_parse_error(s, "'super' is only valid in a method");
            emit_op(s, OP_scope_get_var);
            emit_atom(s, JS_ATOM_this);
            emit_u16(s, 0);
            emit_op(s, OP_scope_get_var);
            emit_atom(s, JS_ATOM_home_object);
            emit_u16(s, 0);
            emit_op(s, OP_get_super);
        } else {
            return js_parse_error(s, "invalid use of 'super'");
        }
        break;
    case TOK_IMPORT:
        if (next_token(s))
            return -1;
        if (s->token.val == '.') {
            if (next_token(s))
                return -1;
            if (!token_is_pseudo_keyword(s, JS_ATOM_meta))
                return js_parse_error(s, "meta expected");
            if (!s->is_module)
                return js_parse_error(s, "import.meta only valid in module code");
            if (next_token(s))
                return -1;
            emit_op(s, OP_special_object);
            emit_u8(s, OP_SPECIAL_OBJECT_IMPORT_META);
        } else {
            if (js_parse_expect(s, '('))
                return -1;
            if (!accept_lparen)
                return js_parse_error(s, "invalid use of 'import()'");
            if (js_parse_assign_expr(s))
                return -1;
            if (js_parse_expect(s, ')'))
                return -1;
            emit_op(s, OP_import);
        }
        break;
    default:
        return js_parse_error(s, "unexpected token in expression: '%.*s'",
                              (int)(s->buf_ptr - s->token.ptr), s->token.ptr);
    }

    optional_chaining_label = -1;
    for(;;) {
        JSFunctionDef *fd = s->cur_func;
        BOOL has_optional_chain = FALSE;
        
        if (s->token.val == TOK_QUESTION_MARK_DOT) {
            /* optional chaining */
            if (next_token(s))
                return -1;
            has_optional_chain = TRUE;
            if (s->token.val == '(' && accept_lparen) {
                goto parse_func_call;
            } else if (s->token.val == '[') {
                goto parse_array_access;
            } else {
                goto parse_property;
            }
        } else if (s->token.val == TOK_TEMPLATE &&
                   call_type == FUNC_CALL_NORMAL) {
            if (optional_chaining_label >= 0) {
                return js_parse_error(s, "template literal cannot appear in an optional chain");
            }
            call_type = FUNC_CALL_TEMPLATE;
            goto parse_func_call2;
        } else if (s->token.val == '(' && accept_lparen) {
            int opcode, arg_count, drop_count;

            /* function call */
        parse_func_call:
            if (next_token(s))
                return -1;

            if (call_type == FUNC_CALL_NORMAL) {
            parse_func_call2:
                switch(opcode = get_prev_opcode(fd)) {
                case OP_get_field:
                    /* keep the object on the stack */
                    fd->byte_code.buf[fd->last_opcode_pos] = OP_get_field2;
                    drop_count = 2;
                    break;
                case OP_scope_get_private_field:
                    /* keep the object on the stack */
                    fd->byte_code.buf[fd->last_opcode_pos] = OP_scope_get_private_field2;
                    drop_count = 2;
                    break;
                case OP_get_array_el:
                    /* keep the object on the stack */
                    fd->byte_code.buf[fd->last_opcode_pos] = OP_get_array_el2;
                    drop_count = 2;
                    break;
                case OP_scope_get_var:
                    {
                        JSAtom name;
                        int scope;
                        name = get_u32(fd->byte_code.buf + fd->last_opcode_pos + 1);
                        scope = get_u16(fd->byte_code.buf + fd->last_opcode_pos + 5);
                        if (name == JS_ATOM_eval && call_type == FUNC_CALL_NORMAL && !has_optional_chain) {
                            /* direct 'eval' */
                            opcode = OP_eval;
                        } else {
                            /* verify if function name resolves to a simple
                               get_loc/get_arg: a function call inside a `with`
                               statement can resolve to a method call of the
                               `with` context object
                             */
                            /* XXX: always generate the OP_scope_get_ref
                               and remove it in variable resolution
                               pass ? */
                            if (has_with_scope(fd, scope)) {
                                opcode = OP_scope_get_ref;
                                fd->byte_code.buf[fd->last_opcode_pos] = opcode;
                            }
                        }
                        drop_count = 1;
                    }
                    break;
                case OP_get_super_value:
                    fd->byte_code.buf[fd->last_opcode_pos] = OP_get_array_el;
                    /* on stack: this func_obj */
                    opcode = OP_get_array_el;
                    drop_count = 2;
                    break;
                default:
                    opcode = OP_invalid;
                    drop_count = 1;
                    break;
                }
                if (has_optional_chain) {
                    optional_chain_test(s, &optional_chaining_label,
                                        drop_count);
                }
            } else {
                opcode = OP_invalid;
            }

            if (call_type == FUNC_CALL_TEMPLATE) {
                if (js_parse_template(s, 1, &arg_count))
                    return -1;
                goto emit_func_call;
            } else if (call_type == FUNC_CALL_SUPER_CTOR) {
                emit_op(s, OP_scope_get_var);
                emit_atom(s, JS_ATOM_this_active_func);
                emit_u16(s, 0);

                emit_op(s, OP_get_super);

                emit_op(s, OP_scope_get_var);
                emit_atom(s, JS_ATOM_new_target);
                emit_u16(s, 0);
            } else if (call_type == FUNC_CALL_NEW) {
                emit_op(s, OP_dup); /* new.target = function */
            }

            /* parse arguments */
            arg_count = 0;
            while (s->token.val != ')') {
                if (arg_count >= 65535) {
                    return js_parse_error(s, "Too many call arguments");
                }
                if (s->token.val == TOK_ELLIPSIS)
                    break;
                if (js_parse_assign_expr(s))
                    return -1;
                arg_count++;
                if (s->token.val == ')')
                    break;
                /* accept a trailing comma before the ')' */
                if (js_parse_expect(s, ','))
                    return -1;
            }
            if (s->token.val == TOK_ELLIPSIS) {
                emit_op(s, OP_array_from);
                emit_u16(s, arg_count);
                emit_op(s, OP_push_i32);
                emit_u32(s, arg_count);

                /* on stack: array idx */
                while (s->token.val != ')') {
                    if (s->token.val == TOK_ELLIPSIS) {
                        if (next_token(s))
                            return -1;
                        if (js_parse_assign_expr(s))
                            return -1;
#if 1
                        /* XXX: could pass is_last indicator? */
                        emit_op(s, OP_append);
#else
                        int label_next, label_done;
                        label_next = new_label(s);
                        label_done = new_label(s);
                        /* push enumerate object below array/idx pair */
                        emit_op(s, OP_for_of_start);
                        emit_op(s, OP_rot5l);
                        emit_op(s, OP_rot5l);
                        emit_label(s, label_next);
                        /* on stack: enum_rec array idx */
                        emit_op(s, OP_for_of_next);
                        emit_u8(s, 2);
                        emit_goto(s, OP_if_true, label_done);
                        /* append element */
                        /* enum_rec array idx val -> enum_rec array new_idx */
                        emit_op(s, OP_define_array_el);
                        emit_op(s, OP_inc);
                        emit_goto(s, OP_goto, label_next);
                        emit_label(s, label_done);
                        /* close enumeration, drop enum_rec and idx */
                        emit_op(s, OP_drop); /* drop undef */
                        emit_op(s, OP_nip1); /* drop enum_rec */
                        emit_op(s, OP_nip1);
                        emit_op(s, OP_nip1);
#endif
                    } else {
                        if (js_parse_assign_expr(s))
                            return -1;
                        /* array idx val */
                        emit_op(s, OP_define_array_el);
                        emit_op(s, OP_inc);
                    }
                    if (s->token.val == ')')
                        break;
                    /* accept a trailing comma before the ')' */
                    if (js_parse_expect(s, ','))
                        return -1;
                }
                if (next_token(s))
                    return -1;
                /* drop the index */
                emit_op(s, OP_drop);

                /* apply function call */
                switch(opcode) {
                case OP_get_field:
                case OP_scope_get_private_field:
                case OP_get_array_el:
                case OP_scope_get_ref:
                    /* obj func array -> func obj array */
                    emit_op(s, OP_perm3);
                    emit_op(s, OP_apply);
                    emit_u16(s, call_type == FUNC_CALL_NEW);
                    break;
                case OP_eval:
                    emit_op(s, OP_apply_eval);
                    emit_u16(s, fd->scope_level);
                    fd->has_eval_call = TRUE;
                    break;
                default:
                    if (call_type == FUNC_CALL_SUPER_CTOR) {
                        emit_op(s, OP_apply);
                        emit_u16(s, 1);
                        /* set the 'this' value */
                        emit_op(s, OP_dup);
                        emit_op(s, OP_scope_put_var_init);
                        emit_atom(s, JS_ATOM_this);
                        emit_u16(s, 0);

                        emit_class_field_init(s);
                    } else if (call_type == FUNC_CALL_NEW) {
                        /* obj func array -> func obj array */
                        emit_op(s, OP_perm3);
                        emit_op(s, OP_apply);
                        emit_u16(s, 1);
                    } else {
                        /* func array -> func undef array */
                        emit_op(s, OP_undefined);
                        emit_op(s, OP_swap);
                        emit_op(s, OP_apply);
                        emit_u16(s, 0);
                    }
                    break;
                }
            } else {
                if (next_token(s))
                    return -1;
            emit_func_call:
                switch(opcode) {
                case OP_get_field:
                case OP_scope_get_private_field:
                case OP_get_array_el:
                case OP_scope_get_ref:
                    emit_op(s, OP_call_method);
                    emit_u16(s, arg_count);
                    break;
                case OP_eval:
                    emit_op(s, OP_eval);
                    emit_u16(s, arg_count);
                    emit_u16(s, fd->scope_level);
                    fd->has_eval_call = TRUE;
                    break;
                default:
                    if (call_type == FUNC_CALL_SUPER_CTOR) {
                        emit_op(s, OP_call_constructor);
                        emit_u16(s, arg_count);

                        /* set the 'this' value */
                        emit_op(s, OP_dup);
                        emit_op(s, OP_scope_put_var_init);
                        emit_atom(s, JS_ATOM_this);
                        emit_u16(s, 0);

                        emit_class_field_init(s);
                    } else if (call_type == FUNC_CALL_NEW) {
                        emit_op(s, OP_call_constructor);
                        emit_u16(s, arg_count);
                    } else {
                        emit_op(s, OP_call);
                        emit_u16(s, arg_count);
                    }
                    break;
                }
            }
            call_type = FUNC_CALL_NORMAL;
        } else if (s->token.val == '.') {
            if (next_token(s))
                return -1;
        parse_property:
            if (s->token.val == TOK_PRIVATE_NAME) {
                /* private class field */
                if (get_prev_opcode(fd) == OP_get_super) {
                    return js_parse_error(s, "private class field forbidden after super");
                }
                if (has_optional_chain) {
                    optional_chain_test(s, &optional_chaining_label, 1);
                }
                emit_op(s, OP_scope_get_private_field);
                emit_atom(s, s->token.u.ident.atom);
                emit_u16(s, s->cur_func->scope_level);
            } else {
                if (!token_is_ident(s->token.val)) {
                    return js_parse_error(s, "expecting field name");
                }
                if (get_prev_opcode(fd) == OP_get_super) {
                    JSValue val;
                    int ret;
                    val = JS_AtomToValue(s->ctx, s->token.u.ident.atom);
                    ret = emit_push_const(s, val, 1);
                    JS_FreeValue(s->ctx, val);
                    if (ret)
                        return -1;
                    emit_op(s, OP_get_super_value);
                } else {
                    if (has_optional_chain) {
                        optional_chain_test(s, &optional_chaining_label, 1);
                    }
                    emit_op(s, OP_get_field);
                    emit_atom(s, s->token.u.ident.atom);
                }
            }
            if (next_token(s))
                return -1;
        } else if (s->token.val == '[') {
            int prev_op;

        parse_array_access:
            prev_op = get_prev_opcode(fd);
            if (has_optional_chain) {
                optional_chain_test(s, &optional_chaining_label, 1);
            }
            if (next_token(s))
                return -1;
            if (js_parse_expr(s))
                return -1;
            if (js_parse_expect(s, ']'))
                return -1;
            if (prev_op == OP_get_super) {
                emit_op(s, OP_get_super_value);
            } else {
                emit_op(s, OP_get_array_el);
            }
        } else {
            break;
        }
    }
    if (optional_chaining_label >= 0)
        emit_label(s, optional_chaining_label);
    return 0;
}

static __exception int js_parse_delete(JSParseState *s)
{
    JSFunctionDef *fd = s->cur_func;
    JSAtom name;
    int opcode;

    if (next_token(s))
        return -1;
    if (js_parse_unary(s, PF_POW_FORBIDDEN))
        return -1;
    switch(opcode = get_prev_opcode(fd)) {
    case OP_get_field:
        {
            JSValue val;
            int ret;

            name = get_u32(fd->byte_code.buf + fd->last_opcode_pos + 1);
            fd->byte_code.size = fd->last_opcode_pos;
            fd->last_opcode_pos = -1;
            val = JS_AtomToValue(s->ctx, name);
            ret = emit_push_const(s, val, 1);
            JS_FreeValue(s->ctx, val);
            JS_FreeAtom(s->ctx, name);
            if (ret)
                return ret;
        }
        goto do_delete;
    case OP_get_array_el:
        fd->byte_code.size = fd->last_opcode_pos;
        fd->last_opcode_pos = -1;
    do_delete:
        emit_op(s, OP_delete);
        break;
    case OP_scope_get_var:
        /* 'delete this': this is not a reference */
        name = get_u32(fd->byte_code.buf + fd->last_opcode_pos + 1);
        if (name == JS_ATOM_this || name == JS_ATOM_new_target)
            goto ret_true;
        if (fd->js_mode & JS_MODE_STRICT) {
            return js_parse_error(s, "cannot delete a direct reference in strict mode");
        } else {
            fd->byte_code.buf[fd->last_opcode_pos] = OP_scope_delete_var;
        }
        break;
    case OP_scope_get_private_field:
        return js_parse_error(s, "cannot delete a private class field");
    case OP_get_super_value:
        emit_op(s, OP_throw_error);
        emit_atom(s, JS_ATOM_NULL);
        emit_u8(s, JS_THROW_ERROR_DELETE_SUPER);
        break;
    default:
    ret_true:
        emit_op(s, OP_drop);
        emit_op(s, OP_push_true);
        break;
    }
    return 0;
}

/* allowed parse_flags: PF_ARROW_FUNC, PF_POW_ALLOWED, PF_POW_FORBIDDEN */
static __exception int js_parse_unary(JSParseState *s, int parse_flags)
{
    int op;

    switch(s->token.val) {
    case '+':
    case '-':
    case '!':
    case '~':
    case TOK_VOID:
        op = s->token.val;
        if (next_token(s))
            return -1;
        if (js_parse_unary(s, PF_POW_FORBIDDEN))
            return -1;
        switch(op) {
        case '-':
            emit_op(s, OP_neg);
            break;
        case '+':
            emit_op(s, OP_plus);
            break;
        case '!':
            emit_op(s, OP_lnot);
            break;
        case '~':
            emit_op(s, OP_not);
            break;
        case TOK_VOID:
            emit_op(s, OP_drop);
            emit_op(s, OP_undefined);
            break;
        default:
            abort();
        }
        parse_flags = 0;
        break;
    case TOK_DEC:
    case TOK_INC:
        {
            int opcode, op, scope, label;
            JSAtom name;
            op = s->token.val;
            if (next_token(s))
                return -1;
            if (js_parse_unary(s, 0))
                return -1;
            if (get_lvalue(s, &opcode, &scope, &name, &label, NULL, TRUE, op))
                return -1;
            emit_op(s, OP_dec + op - TOK_DEC);
            put_lvalue(s, opcode, scope, name, label, PUT_LVALUE_KEEP_TOP,
                       FALSE);
        }
        break;
    case TOK_TYPEOF:
        {
            JSFunctionDef *fd;
            if (next_token(s))
                return -1;
            if (js_parse_unary(s, PF_POW_FORBIDDEN))
                return -1;
            /* reference access should not return an exception, so we
               patch the get_var */
            fd = s->cur_func;
            if (get_prev_opcode(fd) == OP_scope_get_var) {
                fd->byte_code.buf[fd->last_opcode_pos] = OP_scope_get_var_undef;
            }
            emit_op(s, OP_typeof);
            parse_flags = 0;
        }
        break;
    case TOK_DELETE:
        if (js_parse_delete(s))
            return -1;
        parse_flags = 0;
        break;
    case TOK_AWAIT:
        if (!(s->cur_func->func_kind & JS_FUNC_ASYNC))
            return js_parse_error(s, "unexpected 'await' keyword");
        if (!s->cur_func->in_function_body)
            return js_parse_error(s, "await in default expression");
        if (next_token(s))
            return -1;
        if (js_parse_unary(s, PF_POW_FORBIDDEN))
            return -1;
        emit_op(s, OP_await);
        parse_flags = 0;
        break;
    default:
        if (js_parse_postfix_expr(s, (parse_flags & PF_ARROW_FUNC) |
                                  PF_POSTFIX_CALL))
            return -1;
        if (!s->got_lf &&
            (s->token.val == TOK_DEC || s->token.val == TOK_INC)) {
            int opcode, op, scope, label;
            JSAtom name;
            op = s->token.val;
            if (get_lvalue(s, &opcode, &scope, &name, &label, NULL, TRUE, op))
                return -1;
            emit_op(s, OP_post_dec + op - TOK_DEC);
            put_lvalue(s, opcode, scope, name, label, PUT_LVALUE_KEEP_SECOND,
                       FALSE);
            if (next_token(s))
                return -1;        
        }
        break;
    }
    if (parse_flags & (PF_POW_ALLOWED | PF_POW_FORBIDDEN)) {
#ifdef CONFIG_BIGNUM
        if (s->token.val == TOK_POW || s->token.val == TOK_MATH_POW) {
            /* Extended exponentiation syntax rules: we extend the ES7
               grammar in order to have more intuitive semantics:
               -2**2 evaluates to -4. */
            if (!(s->cur_func->js_mode & JS_MODE_MATH)) {
                if (parse_flags & PF_POW_FORBIDDEN) {
                    JS_ThrowSyntaxError(s->ctx, "unparenthesized unary expression can't appear on the left-hand side of '**'");
                    return -1;
                }
            }
            if (next_token(s))
                return -1;
            if (js_parse_unary(s, PF_POW_ALLOWED))
                return -1;
            emit_op(s, OP_pow);
        }
#else
        if (s->token.val == TOK_POW) {
            /* Strict ES7 exponentiation syntax rules: To solve
               conficting semantics between different implementations
               regarding the precedence of prefix operators and the
               postifx exponential, ES7 specifies that -2**2 is a
               syntax error. */
            if (parse_flags & PF_POW_FORBIDDEN) {
                JS_ThrowSyntaxError(s->ctx, "unparenthesized unary expression can't appear on the left-hand side of '**'");
                return -1;
            }
            if (next_token(s))
                return -1;
            if (js_parse_unary(s, PF_POW_ALLOWED))
                return -1;
            emit_op(s, OP_pow);
        }
#endif
    }
    return 0;
}

/* allowed parse_flags: PF_ARROW_FUNC, PF_IN_ACCEPTED */
static __exception int js_parse_expr_binary(JSParseState *s, int level,
                                            int parse_flags)
{
    int op, opcode;

    if (level == 0) {
        return js_parse_unary(s, (parse_flags & PF_ARROW_FUNC) |
                              PF_POW_ALLOWED);
    }
    if (js_parse_expr_binary(s, level - 1, parse_flags))
        return -1;
    for(;;) {
        op = s->token.val;
        switch(level) {
        case 1:
            switch(op) {
            case '*':
                opcode = OP_mul;
                break;
            case '/':
                opcode = OP_div;
                break;
            case '%':
#ifdef CONFIG_BIGNUM
                if (s->cur_func->js_mode & JS_MODE_MATH)
                    opcode = OP_math_mod;
                else
#endif
                    opcode = OP_mod;
                break;
            default:
                return 0;
            }
            break;
        case 2:
            switch(op) {
            case '+':
                opcode = OP_add;
                break;
            case '-':
                opcode = OP_sub;
                break;
            default:
                return 0;
            }
            break;
        case 3:
            switch(op) {
            case TOK_SHL:
                opcode = OP_shl;
                break;
            case TOK_SAR:
                opcode = OP_sar;
                break;
            case TOK_SHR:
                opcode = OP_shr;
                break;
            default:
                return 0;
            }
            break;
        case 4:
            switch(op) {
            case '<':
                opcode = OP_lt;
                break;
            case '>':
                opcode = OP_gt;
                break;
            case TOK_LTE:
                opcode = OP_lte;
                break;
            case TOK_GTE:
                opcode = OP_gte;
                break;
            case TOK_INSTANCEOF:
                opcode = OP_instanceof;
                break;
            case TOK_IN:
                if (parse_flags & PF_IN_ACCEPTED) {
                    opcode = OP_in;
                } else {
                    return 0;
                }
                break;
            default:
                return 0;
            }
            break;
        case 5:
            switch(op) {
            case TOK_EQ:
                opcode = OP_eq;
                break;
            case TOK_NEQ:
                opcode = OP_neq;
                break;
            case TOK_STRICT_EQ:
                opcode = OP_strict_eq;
                break;
            case TOK_STRICT_NEQ:
                opcode = OP_strict_neq;
                break;
            default:
                return 0;
            }
            break;
        case 6:
            switch(op) {
            case '&':
                opcode = OP_and;
                break;
            default:
                return 0;
            }
            break;
        case 7:
            switch(op) {
            case '^':
                opcode = OP_xor;
                break;
            default:
                return 0;
            }
            break;
        case 8:
            switch(op) {
            case '|':
                opcode = OP_or;
                break;
            default:
                return 0;
            }
            break;
        default:
            abort();
        }
        if (next_token(s))
            return -1;
        if (js_parse_expr_binary(s, level - 1, parse_flags & ~PF_ARROW_FUNC))
            return -1;
        emit_op(s, opcode);
    }
    return 0;
}

/* allowed parse_flags: PF_ARROW_FUNC, PF_IN_ACCEPTED */
static __exception int js_parse_logical_and_or(JSParseState *s, int op,
                                               int parse_flags)
{
    int label1;

    if (op == TOK_LAND) {
        if (js_parse_expr_binary(s, 8, parse_flags))
            return -1;
    } else {
        if (js_parse_logical_and_or(s, TOK_LAND, parse_flags))
            return -1;
    }
    if (s->token.val == op) {
        label1 = new_label(s);

        for(;;) {
            if (next_token(s))
                return -1;
            emit_op(s, OP_dup);
            emit_goto(s, op == TOK_LAND ? OP_if_false : OP_if_true, label1);
            emit_op(s, OP_drop);

            if (op == TOK_LAND) {
                if (js_parse_expr_binary(s, 8, parse_flags & ~PF_ARROW_FUNC))
                    return -1;
            } else {
                if (js_parse_logical_and_or(s, TOK_LAND,
                                            parse_flags & ~PF_ARROW_FUNC))
                    return -1;
            }
            if (s->token.val != op) {
                if (s->token.val == TOK_DOUBLE_QUESTION_MARK)
                    return js_parse_error(s, "cannot mix ?? with && or ||");
                break;
            }
        }

        emit_label(s, label1);
    }
    return 0;
}

static __exception int js_parse_coalesce_expr(JSParseState *s, int parse_flags)
{
    int label1;
    
    if (js_parse_logical_and_or(s, TOK_LOR, parse_flags))
        return -1;
    if (s->token.val == TOK_DOUBLE_QUESTION_MARK) {
        label1 = new_label(s);
        for(;;) {
            if (next_token(s))
                return -1;
            
            emit_op(s, OP_dup);
            emit_op(s, OP_is_undefined_or_null);
            emit_goto(s, OP_if