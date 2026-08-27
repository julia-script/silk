import type * as Builder from '@silklang/llvm/Builder'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as LlvmType from '@silklang/llvm/Type'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as FloatingPoint from './FloatingPoint.js'
import type * as Mir from './Mir.js'
import * as Scalar from './Scalar.js'
import * as Transcendental from './Transcendental.js'

export interface Context {
  readonly builder: Builder.Builder
  readonly i32: LlvmType.Type
  readonly i64?: LlvmType.Type
  readonly f32: LlvmType.Type
  readonly f64: LlvmType.Type
}

/** Emits the shared deterministic range-reduction and polynomial kernel for native sin/cos. */
export const emit = Effect.fnUntraced(function* (
  self: Context,
  body: FunctionBody.FunctionBody,
  operation: Extract<Mir.Operation, { readonly _tag: 'FloatTranscendental' }>,
  subject: Value.Input,
) {
  const source = Scalar.find(operation.sourceType._tag)
  if (source?.category !== 'Floating')
    throw new RangeError('LLVM transcendental lost its source type')
  const width = source.spelling === 'f32' ? 32 : 64
  const floatType = source.spelling === 'f32' ? self.f32 : self.f64
  const format = source.spelling === 'f32' ? 'float' : 'double'
  const plan = Transcendental.plan(width)
  const suffix = operation.destination.ordinal
  const constant = Effect.fnUntraced(function* (bits: bigint) {
    return yield* Constant.floatingRaw(
      self.builder,
      floatType,
      format,
      FloatingPoint.littleEndianBytes({ width, bits }),
    )
  })
  const binary = Effect.fnUntraced(function* (
    kind: FunctionBody.FloatingBinaryKind,
    left: Value.Input,
    right: Value.Input,
    name: string,
  ) {
    return yield* FunctionBody.binary(body, kind, left, right, name)
  })
  const zero = yield* constant(0n)
  const half = yield* constant(plan.half)
  const negativeHalf = yield* FunctionBody.unary(body, 'fneg', half, `trans_half_neg${suffix}`)
  const negative = yield* FunctionBody.floatingCompare(
    body,
    'olt',
    subject,
    zero,
    `trans_negative${suffix}`,
  )
  const offset = yield* FunctionBody.select(
    body,
    negative,
    negativeHalf,
    half,
    `trans_offset${suffix}`,
  )
  const scaled = yield* binary(
    'fmul',
    subject,
    yield* constant(plan.inverseHalfPi),
    `trans_scaled${suffix}`,
  )
  const shifted = yield* binary('fadd', scaled, offset, `trans_shifted${suffix}`)
  const i64 = self.i64 ?? self.i32
  const quadrantInteger = yield* FunctionBody.cast(
    body,
    'fptosi',
    shifted,
    i64,
    `trans_quadrant_integer${suffix}`,
  )
  const quadrantFloat = yield* FunctionBody.cast(
    body,
    'sitofp',
    quadrantInteger,
    floatType,
    `trans_quadrant_float${suffix}`,
  )
  let residual: Value.Input = subject
  for (const [index, part] of plan.halfPi.entries()) {
    const product = yield* binary(
      'fmul',
      quadrantFloat,
      yield* constant(part),
      `trans_reduce_product${suffix}_${index}`,
    )
    residual = yield* binary('fsub', residual, product, `trans_reduce${suffix}_${index}`)
  }
  const squared = yield* binary('fmul', residual, residual, `trans_squared${suffix}`)
  const polynomial = Effect.fnUntraced(function* (
    coefficients: ReadonlyArray<bigint>,
    name: string,
  ) {
    let result: Value.Input = yield* constant(coefficients.at(-1) ?? 0n)
    for (let index = coefficients.length - 2; index >= 0; index -= 1) {
      result = yield* binary(
        'fadd',
        yield* constant(coefficients[index] ?? 0n),
        yield* binary('fmul', squared, result, `${name}_mul${index}`),
        `${name}_add${index}`,
      )
    }
    return result
  })
  const sineTail = yield* polynomial(plan.sine, `trans_sine_tail${suffix}`)
  const residualSquared = yield* binary(
    'fmul',
    residual,
    squared,
    `trans_residual_squared${suffix}`,
  )
  const sine = yield* binary(
    'fadd',
    residual,
    yield* binary('fmul', residualSquared, sineTail, `trans_sine_product${suffix}`),
    `trans_sine${suffix}`,
  )
  const cosineTail = yield* polynomial(plan.cosine, `trans_cosine_tail${suffix}`)
  const cosineBase = yield* binary(
    'fsub',
    yield* constant(plan.one),
    yield* binary('fmul', half, squared, `trans_cosine_half${suffix}`),
    `trans_cosine_base${suffix}`,
  )
  const cosine = yield* binary(
    'fadd',
    cosineBase,
    yield* binary(
      'fmul',
      yield* binary('fmul', squared, squared, `trans_fourth${suffix}`),
      cosineTail,
      `trans_cosine_product${suffix}`,
    ),
    `trans_cosine${suffix}`,
  )
  const quadrant = yield* FunctionBody.binary(
    body,
    'and',
    quadrantInteger,
    yield* Constant.integerUnsigned(self.builder, i64, 3n),
    `trans_quadrant${suffix}`,
  )
  const isQuadrant = Effect.fnUntraced(function* (value: bigint) {
    return yield* FunctionBody.integerCompare(
      body,
      'eq',
      quadrant,
      yield* Constant.integerUnsigned(self.builder, i64, value),
      `trans_quadrant_${value.toString()}_${suffix}`,
    )
  })
  const negativeSine = yield* FunctionBody.unary(body, 'fneg', sine, `trans_sine_neg${suffix}`)
  const negativeCosine = yield* FunctionBody.unary(
    body,
    'fneg',
    cosine,
    `trans_cosine_neg${suffix}`,
  )
  const q2 = yield* FunctionBody.select(
    body,
    yield* isQuadrant(2n),
    operation.operation === 'Sin' ? negativeSine : negativeCosine,
    operation.operation === 'Sin' ? negativeCosine : sine,
    `trans_q2${suffix}`,
  )
  const q1 = yield* FunctionBody.select(
    body,
    yield* isQuadrant(1n),
    operation.operation === 'Sin' ? cosine : negativeSine,
    q2,
    `trans_q1${suffix}`,
  )
  const finite = yield* FunctionBody.select(
    body,
    yield* isQuadrant(0n),
    operation.operation === 'Sin' ? sine : cosine,
    q1,
    `trans_finite${suffix}`,
  )
  const unordered = yield* FunctionBody.floatingCompare(
    body,
    'uno',
    subject,
    subject,
    `trans_nan${suffix}`,
  )
  const positiveInfinite = yield* FunctionBody.floatingCompare(
    body,
    'oeq',
    subject,
    yield* constant(width === 32 ? 0x7f800000n : 0x7ff0000000000000n),
    `trans_positive_infinite${suffix}`,
  )
  const negativeInfinite = yield* FunctionBody.floatingCompare(
    body,
    'oeq',
    subject,
    yield* constant(width === 32 ? 0xff800000n : 0xfff0000000000000n),
    `trans_negative_infinite${suffix}`,
  )
  const infinite = yield* FunctionBody.binary(
    body,
    'or',
    positiveInfinite,
    negativeInfinite,
    `trans_infinite${suffix}`,
  )
  const nonFinite = yield* FunctionBody.binary(
    body,
    'or',
    unordered,
    infinite,
    `trans_nonfinite${suffix}`,
  )
  const isZero = yield* FunctionBody.floatingCompare(
    body,
    'oeq',
    subject,
    zero,
    `trans_zero${suffix}`,
  )
  const finiteWithZero = yield* FunctionBody.select(
    body,
    isZero,
    operation.operation === 'Sin' ? subject : yield* constant(plan.one),
    finite,
    `trans_finite_zero${suffix}`,
  )
  return yield* FunctionBody.select(
    body,
    nonFinite,
    yield* constant(plan.canonicalNaN),
    finiteWithZero,
    `transcendental${suffix}`,
  )
})
