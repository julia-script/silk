typedef struct Function Function;


#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include "typedef.h"


typedef enum CCallConv {
  Fast,
  Cold,
  Tail,
  SystemV,
  WindowsFastcall,
  AppleAarch64,
  Probestack,
  Winch,
} CCallConv;

typedef enum CType {
  /**
   * An integer type with 8 bits.
   * WARNING: arithmetic on 8bit integers is incomplete
   */
  I8,
  /**
   * An integer type with 16 bits.
   * WARNING: arithmetic on 16bit integers is incomplete
   */
  I16,
  /**
   * An integer type with 32 bits.
   */
  I32,
  /**
   * An integer type with 64 bits.
   */
  I64,
  /**
   * An integer type with 128 bits.
   */
  I128,
  /**
   * A 16-bit floating point type represented in the IEEE 754-2008
   * *binary16* interchange format. This corresponds to the :c:type:`_Float16`
   * type in most C implementations.
   * WARNING: f16 support is a work-in-progress and is incomplete
   */
  F16,
  /**
   * A 32-bit floating point type represented in the IEEE 754-2008
   * *binary32* interchange format. This corresponds to the :c:type:`float`
   * type in most C implementations.
   */
  F32,
  /**
   * A 64-bit floating point type represented in the IEEE 754-2008
   * *binary64* interchange format. This corresponds to the :c:type:`double`
   * type in most C implementations.
   */
  F64,
  /**
   * A 128-bit floating point type represented in the IEEE 754-2008
   * *binary128* interchange format. This corresponds to the :c:type:`_Float128`
   * type in most C implementations.
   * WARNING: f128 support is a work-in-progress and is incomplete
   */
  F128,
  /**
   * A SIMD vector with 2 lanes containing a `i8` each.
   */
  I8X2,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `i8` bits each.
   */
  I8X2XN,
  /**
   * A SIMD vector with 4 lanes containing a `i8` each.
   */
  I8X4,
  /**
   * A SIMD vector with 2 lanes containing a `i16` each.
   */
  I16X2,
  /**
   * A SIMD vector with 2 lanes containing a `f16` each.
   */
  F16X2,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `i8` bits each.
   */
  I8X4XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `i16` bits each.
   */
  I16X2XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `f16` bits each.
   */
  F16X2XN,
  /**
   * A SIMD vector with 8 lanes containing a `i8` each.
   */
  I8X8,
  /**
   * A SIMD vector with 4 lanes containing a `i16` each.
   */
  I16X4,
  /**
   * A SIMD vector with 2 lanes containing a `i32` each.
   */
  I32X2,
  /**
   * A SIMD vector with 4 lanes containing a `f16` each.
   */
  F16X4,
  /**
   * A SIMD vector with 2 lanes containing a `f32` each.
   */
  F32X2,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 8 lanes containing `i8` bits each.
   */
  I8X8XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `i16` bits each.
   */
  I16X4XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `i32` bits each.
   */
  I32X2XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `f16` bits each.
   */
  F16X4XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `f32` bits each.
   */
  F32X2XN,
  /**
   * A SIMD vector with 16 lanes containing a `i8` each.
   */
  I8X16,
  /**
   * A SIMD vector with 8 lanes containing a `i16` each.
   */
  I16X8,
  /**
   * A SIMD vector with 4 lanes containing a `i32` each.
   */
  I32X4,
  /**
   * A SIMD vector with 2 lanes containing a `i64` each.
   */
  I64X2,
  /**
   * A SIMD vector with 8 lanes containing a `f16` each.
   */
  F16X8,
  /**
   * A SIMD vector with 4 lanes containing a `f32` each.
   */
  F32X4,
  /**
   * A SIMD vector with 2 lanes containing a `f64` each.
   */
  F64X2,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 16 lanes containing `i8` bits each.
   */
  I8X16XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 8 lanes containing `i16` bits each.
   */
  I16X8XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `i32` bits each.
   */
  I32X4XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `i64` bits each.
   */
  I64X2XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 8 lanes containing `f16` bits each.
   */
  F16X8XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `f32` bits each.
   */
  F32X4XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `f64` bits each.
   */
  F64X2XN,
  /**
   * A SIMD vector with 32 lanes containing a `i8` each.
   */
  I8X32,
  /**
   * A SIMD vector with 16 lanes containing a `i16` each.
   */
  I16X16,
  /**
   * A SIMD vector with 8 lanes containing a `i32` each.
   */
  I32X8,
  /**
   * A SIMD vector with 4 lanes containing a `i64` each.
   */
  I64X4,
  /**
   * A SIMD vector with 2 lanes containing a `i128` each.
   */
  I128X2,
  /**
   * A SIMD vector with 16 lanes containing a `f16` each.
   */
  F16X16,
  /**
   * A SIMD vector with 8 lanes containing a `f32` each.
   */
  F32X8,
  /**
   * A SIMD vector with 4 lanes containing a `f64` each.
   */
  F64X4,
  /**
   * A SIMD vector with 2 lanes containing a `f128` each.
   */
  F128X2,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 32 lanes containing `i8` bits each.
   */
  I8X32XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 16 lanes containing `i16` bits each.
   */
  I16X16XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 8 lanes containing `i32` bits each.
   */
  I32X8XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `i64` bits each.
   */
  I64X4XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `i128` bits each.
   */
  I128X2XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 16 lanes containing `f16` bits each.
   */
  F16X16XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 8 lanes containing `f32` bits each.
   */
  F32X8XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `f64` bits each.
   */
  F64X4XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `f128` bits each.
   */
  F128X2XN,
  /**
   * A SIMD vector with 64 lanes containing a `i8` each.
   */
  I8X64,
  /**
   * A SIMD vector with 32 lanes containing a `i16` each.
   */
  I16X32,
  /**
   * A SIMD vector with 16 lanes containing a `i32` each.
   */
  I32X16,
  /**
   * A SIMD vector with 8 lanes containing a `i64` each.
   */
  I64X8,
  /**
   * A SIMD vector with 4 lanes containing a `i128` each.
   */
  I128X4,
  /**
   * A SIMD vector with 32 lanes containing a `f16` each.
   */
  F16X32,
  /**
   * A SIMD vector with 16 lanes containing a `f32` each.
   */
  F32X16,
  /**
   * A SIMD vector with 8 lanes containing a `f64` each.
   */
  F64X8,
  /**
   * A SIMD vector with 4 lanes containing a `f128` each.
   */
  F128X4,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 64 lanes containing `i8` bits each.
   */
  I8X64XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 32 lanes containing `i16` bits each.
   */
  I16X32XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 16 lanes containing `i32` bits each.
   */
  I32X16XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 8 lanes containing `i64` bits each.
   */
  I64X8XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `i128` bits each.
   */
  I128X4XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 32 lanes containing `f16` bits each.
   */
  F16X32XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 16 lanes containing `f32` bits each.
   */
  F32X16XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 8 lanes containing `f64` bits each.
   */
  F64X8XN,
  /**
   * A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `f128` bits each.
   */
  F128X4XN,
} CType;

typedef struct IsaBuilder IsaBuilder;

typedef struct TargetIsaHandle TargetIsaHandle;

typedef struct ByteBuffer {
  uint8_t *data;
  size_t len;
} ByteBuffer;

Flags *Flags_new(Builder *settings_builder);

void FunctionBuilderContext_free(FunctionBuilderContext *ctx);

FunctionBuilderContext *FunctionBuilderContext_new(void);

void FunctionBuilder_append_block_params_for_function_params(FunctionBuilder *builder,
                                                             uint32_t block);

uint32_t FunctionBuilder_block_param(FunctionBuilder *builder, uint32_t block, uint32_t index);

uint32_t FunctionBuilder_create_block(FunctionBuilder *builder);

void FunctionBuilder_declare_var(FunctionBuilder *builder, uint32_t variable, enum CType ty);

void FunctionBuilder_def_var(FunctionBuilder *builder, uint32_t variable, uint32_t value);

void FunctionBuilder_finalize(FunctionBuilder *builder);

void FunctionBuilder_free(FunctionBuilder *builder);

void FunctionBuilder_ins_brif(FunctionBuilder *builder,
                              uint32_t condition,
                              uint32_t block_then,
                              const Value *block_then_args,
                              uint32_t block_then_args_count,
                              uint32_t block_else,
                              const Value *block_else_args,
                              uint32_t block_else_args_count);

uint32_t FunctionBuilder_ins_f32const(FunctionBuilder *builder, float value);

uint32_t FunctionBuilder_ins_f64const(FunctionBuilder *builder, double value);

uint32_t FunctionBuilder_ins_iadd(FunctionBuilder *builder, uint32_t a, uint32_t b);

uint32_t FunctionBuilder_ins_iconst(FunctionBuilder *builder, enum CType narrow_int, int64_t value);

uint32_t FunctionBuilder_ins_imul(FunctionBuilder *builder, uint32_t a, uint32_t b);

uint32_t FunctionBuilder_ins_isub(FunctionBuilder *builder, uint32_t a, uint32_t b);

void FunctionBuilder_ins_jump(FunctionBuilder *builder,
                              uint32_t block,
                              const Value *values,
                              uint32_t count);

void FunctionBuilder_ins_return(FunctionBuilder *builder, const Value *values, uint32_t count);

FunctionBuilder *FunctionBuilder_new(Function *func, FunctionBuilderContext *ctx);

void FunctionBuilder_seal_all_blocks(FunctionBuilder *builder);

void FunctionBuilder_seal_block(FunctionBuilder *builder, uint32_t block);

void FunctionBuilder_switch_to_block(FunctionBuilder *builder, uint32_t block);

uint32_t FunctionBuilder_use_var(FunctionBuilder *builder, uint32_t variable);

void Function_dump(Function *func);

void Function_free(Function *func);

Function *Function_new(void);

void Function_verify(Function *func);

Function *Function_with_name_signature(uint32_t namespace_, uint32_t index, Signature *signature);

struct TargetIsaHandle *IsaBuilder_finish(struct IsaBuilder *builder, Flags *flags);

struct IsaBuilder *IsaBuilder_native_builder(void);

ObjectBuilder *ObjectBuilder_new(struct TargetIsaHandle *isa, const char *name);

uint32_t ObjectModule_declare_function(ObjectModule *object_module,
                                       const char *name,
                                       const Signature *signature);

void ObjectModule_define_function(ObjectModule *object_module, uint32_t func_id, Context *ctx);

ObjectProduct *ObjectModule_finish(ObjectModule *object_module);

ObjectModule *ObjectModule_new(ObjectBuilder *object_builder);

struct ByteBuffer *ObjectProduct_emit(ObjectProduct *product);

void SettingsBuilder_free(Builder *builder);

Builder *SettingsBuilder_new(void);

void SettingsBuilder_set(Builder *builder, const char *name, const char *value);

void Signature_add_param(Signature *sig, enum CType param);

void Signature_add_return(Signature *sig, enum CType ret);

void Signature_free(Signature *sig);

Signature *Signature_new(enum CCallConv conv);

const uint8_t *byte_buffer_data(const struct ByteBuffer *buffer);

size_t byte_buffer_len(const struct ByteBuffer *buffer);

Context *codegen_Context_for_function(Function *func);

struct ByteBuffer *create_byte_buffer(const uint8_t *data, size_t len);

void free_byte_buffer(struct ByteBuffer *buffer);

void hello_world(void);
