import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Builder from '../Builder.js'
import type * as ByteString from '../ByteString.js'
import * as Constant from '../Constant.js'
import * as FastMathActor from '../FastMath.js'
import * as IntegerMath from '../IntegerMath.js'
import type * as FunctionBodyDescription from '../internal/FunctionBodyDescription.js'
import * as FunctionBodyState from '../internal/FunctionBodyState.js'
import { invalidInput, invalidState, type LlvmError } from '../LlvmError.js'
import * as Type from '../Type.js'
import type * as Value from '../Value.js'
import { type FastMathInput, type FunctionBody, fastMath, sameOperands } from './_internal.js'

/**
 * Integer arithmetic operation names accepted by {@link binary}.
 *
 * @category instructions
 * @since 0.0.0
 */
export type IntegerBinaryKind = FunctionBodyDescription.IntegerBinaryKind

/**
 * Floating arithmetic operation names accepted by {@link binary}.
 *
 * @category instructions
 * @since 0.0.0
 */
export type FloatingBinaryKind = FunctionBodyDescription.FloatingBinaryKind

/**
 * Every arithmetic operation name accepted by {@link binary}.
 *
 * @category instructions
 * @since 0.0.0
 */
export type BinaryKind = FunctionBodyDescription.BinaryKind

/**
 * LLVM integer comparison predicates.
 *
 * @category instructions
 * @since 0.0.0
 */
export type IntegerPredicate = FunctionBodyDescription.IntegerPredicate

/**
 * LLVM floating-point comparison predicates.
 *
 * @category instructions
 * @since 0.0.0
 */
export type FloatingPredicate = FunctionBodyDescription.FloatingPredicate

/**
 * Optional arithmetic promises accepted by {@link binary}.
 *
 * @category instructions
 * @since 0.0.0
 */
export interface BinaryOptions {
  readonly noSignedWrap?: boolean
  readonly noUnsignedWrap?: boolean
  readonly exact?: boolean
  readonly fastMath?: FastMathInput
  readonly integerMath?: IntegerMath.Input
}

/**
 * Appends a type-checked integer or floating binary instruction.
 *
 * **Gotchas**
 *
 * Both operands must have one type. Fast-math flags are limited to floating operations, `exact` to
 * division and right shifts, and no-wrap promises to add, subtract, multiply, and left shift.
 *
 * **Example** (Building an addition)
 *
 * Define an `i32` function that adds its two parameters.
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Block from '@silklang/llvm/Block'
 * import * as Builder from '@silklang/llvm/Builder'
 * import * as FunctionActor from '@silklang/llvm/Function'
 * import * as FunctionBody from '@silklang/llvm/FunctionBody'
 * import * as Type from '@silklang/llvm/Type'
 * import * as Value from '@silklang/llvm/Value'
 *
 * await Effect.runPromise(Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const i32 = yield* Type.integer(builder, 32)
 *   const signature = yield* Type.functionType(builder, i32, [i32, i32])
 *   const add = yield* FunctionActor.declare(builder, 'add', signature)
 *   yield* FunctionActor.buildBody(builder, add, Effect.fnUntraced(function* (body) {
 *     yield* Block.make(body, 'entry')
 *     const left = yield* Value.argument(body, 0)
 *     const right = yield* Value.argument(body, 1)
 *     const sum = yield* FunctionBody.binary(body, 'add', left, right, 'sum')
 *     yield* FunctionBody.returnValue(body, sum)
 *   }))
 * }))
 * ```
 *
 * @category instructions
 * @since 0.0.0
 */
export const binary = Effect.fnUntraced(function* (
  self: FunctionBody,
  kind: BinaryKind,
  left: Value.Input,
  right: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
  options: BinaryOptions = {},
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.binary', (draft, module) =>
    Result.gen(function* () {
      const operands = yield* sameOperands(draft, module, left, right, 'FunctionBody.binary')
      const floating = kind.startsWith('f')
      const integerMath = IntegerMath.make(
        options.integerMath ?? {
          noSignedWrap: options.noSignedWrap,
          noUnsignedWrap: options.noUnsignedWrap,
          exact: options.exact,
        },
      )
      const valid = floating
        ? yield* FunctionBodyState.isFloatingType(
            module,
            operands.leftValue.type,
            'FunctionBody.binary',
          )
        : yield* FunctionBodyState.isIntegerType(
            module,
            operands.leftValue.type,
            'FunctionBody.binary',
          )
      if (!valid) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.binary',
            message: `${kind} has incompatible operand types`,
            input: { left, right },
          }),
        )
      }
      if (!floating && FastMathActor.toBitcode(fastMath(options.fastMath)) !== 0) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.binary',
            message: 'Fast-math flags only apply to floating-point binary operations',
            input: kind,
          }),
        )
      }
      const exactKinds: ReadonlyArray<BinaryKind> = ['udiv', 'sdiv', 'lshr', 'ashr']
      const wrapKinds: ReadonlyArray<BinaryKind> = ['add', 'sub', 'mul', 'shl']
      if (integerMath.exact && !exactKinds.includes(kind)) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.binary',
            message: 'The exact flag is only valid on division and right shifts',
            input: kind,
          }),
        )
      }
      if ((integerMath.noSignedWrap || integerMath.noUnsignedWrap) && !wrapKinds.includes(kind)) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.binary',
            message: 'No-wrap flags are only valid on add, sub, mul, and shl',
            input: kind,
          }),
        )
      }
      return (yield* FunctionBodyState.appendResult(
        draft,
        operands.leftValue.type,
        name,
        (result, finalName) =>
          Object.freeze({
            _tag: 'Binary',
            kind,
            left: operands.leftValue.operand,
            right: operands.rightValue.operand,
            integerFlags: Object.freeze({
              noSignedWrap: integerMath.noSignedWrap,
              noUnsignedWrap: integerMath.noUnsignedWrap,
              exact: integerMath.exact,
            }),
            fastMath: fastMath(options.fastMath),
            result,
            name: finalName,
          }),
      )).value
    }),
  )
})

/** @internal */
const integerOperandType = Effect.fnUntraced(function* (
  self: FunctionBody,
  operand: Value.Input,
): Effect.fn.Return<{ readonly builder: Builder.Builder; readonly type: Type.Type }, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.integerUnary', (draft, module) =>
    Result.gen(function* () {
      const resolved = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        operand,
        'FunctionBody.integerUnary',
      )
      const description = yield* FunctionBodyState.typeAt(
        module,
        resolved.type,
        'FunctionBody.integerUnary',
      )
      const scalar =
        description._tag === 'Vector'
          ? yield* FunctionBodyState.typeAt(module, description.child, 'FunctionBody.integerUnary')
          : description
      if (scalar._tag !== 'Integer') {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.integerUnary',
            message: 'Integer unary operations require an integer scalar or vector',
            input: operand,
          }),
        )
      }
      const type = module.types.handles[resolved.type]
      if (type === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.integerUnary',
            message: 'Operand type handle is missing',
            state: operand,
          }),
        )
      }
      return { builder: draft.builder, type }
    }),
  )
})

/**
 * Appends integer negation as subtraction from a same-typed zero constant.
 *
 * @category instructions
 * @since 0.0.0
 */
export const negate = Effect.fnUntraced(function* (
  self: FunctionBody,
  operand: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  const context = yield* integerOperandType(self, operand)
  const zero = yield* Constant.zero(context.builder, context.type)
  return yield* binary(self, 'sub', zero, operand, name)
})

/**
 * Appends integer bitwise complement as XOR with a same-shaped all-ones constant.
 *
 * @category instructions
 * @since 0.0.0
 */
export const bitwiseNot = Effect.fnUntraced(function* (
  self: FunctionBody,
  operand: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  const context = yield* integerOperandType(self, operand)
  const tag = yield* Type.tag(context.builder, context.type)
  const allOnes =
    tag === 'Vector'
      ? yield* Constant.splat(
          context.builder,
          context.type,
          yield* Constant.integerSigned(
            context.builder,
            yield* Type.childType(context.builder, context.type),
            -1,
          ),
        )
      : yield* Constant.integerSigned(context.builder, context.type, -1)
  return yield* binary(self, 'xor', operand, allOnes, name)
})

/**
 * Appends `icmp`, returning `i1` or a same-shaped vector of `i1`.
 *
 * @category instructions
 * @since 0.0.0
 */
export const integerCompare = Effect.fnUntraced(function* (
  self: FunctionBody,
  predicate: IntegerPredicate,
  left: Value.Input,
  right: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.integerCompare',
    (draft, module) =>
      Result.gen(function* () {
        const operands = yield* sameOperands(
          draft,
          module,
          left,
          right,
          'FunctionBody.integerCompare',
        )
        if (
          !(yield* FunctionBodyState.isIntegerType(
            module,
            operands.leftValue.type,
            'FunctionBody.integerCompare',
          ))
        ) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.integerCompare',
              message: 'icmp requires integer scalar or vector operands',
              input: { left, right },
            }),
          )
        }
        const type = yield* FunctionBodyState.comparisonType(draft, module, operands.leftValue.type)
        return (yield* FunctionBodyState.appendResult(draft, type, name, (result, finalName) =>
          Object.freeze({
            _tag: 'Compare',
            kind: 'integer',
            predicate,
            left: operands.leftValue.operand,
            right: operands.rightValue.operand,
            fastMath: FastMathActor.none,
            result,
            name: finalName,
          }),
        )).value
      }),
  )
})

/**
 * Appends fast-math-aware `fcmp`, returning `i1` or a same-shaped vector of `i1`.
 *
 * @category instructions
 * @since 0.0.0
 */
export const floatingCompare = Effect.fnUntraced(function* (
  self: FunctionBody,
  predicate: FloatingPredicate,
  left: Value.Input,
  right: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
  options: { readonly fastMath?: FastMathInput } = {},
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.floatingCompare',
    (draft, module) =>
      Result.gen(function* () {
        const operands = yield* sameOperands(
          draft,
          module,
          left,
          right,
          'FunctionBody.floatingCompare',
        )
        if (
          !(yield* FunctionBodyState.isFloatingType(
            module,
            operands.leftValue.type,
            'FunctionBody.floatingCompare',
          ))
        ) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.floatingCompare',
              message: 'fcmp requires floating-point scalar or vector operands',
              input: { left, right },
            }),
          )
        }
        const type = yield* FunctionBodyState.comparisonType(draft, module, operands.leftValue.type)
        return (yield* FunctionBodyState.appendResult(draft, type, name, (result, finalName) =>
          Object.freeze({
            _tag: 'Compare',
            kind: 'floating',
            predicate,
            left: operands.leftValue.operand,
            right: operands.rightValue.operand,
            fastMath: fastMath(options.fastMath),
            result,
            name: finalName,
          }),
        )).value
      }),
  )
})

/**
 * Appends scalar or shape-matched vector `select` after validating its `i1` condition.
 *
 * @category instructions
 * @since 0.0.0
 */
export const select = Effect.fnUntraced(function* (
  self: FunctionBody,
  condition: Value.Input,
  onTrue: Value.Input,
  onFalse: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
  options: { readonly fastMath?: FastMathInput } = {},
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.select', (draft, module) =>
    Result.gen(function* () {
      const choices = yield* sameOperands(draft, module, onTrue, onFalse, 'FunctionBody.select')
      const selected = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        condition,
        'FunctionBody.select',
      )
      const conditionType = yield* FunctionBodyState.typeAt(
        module,
        selected.type,
        'FunctionBody.select',
      )
      const conditionScalar =
        conditionType._tag === 'Vector'
          ? yield* FunctionBodyState.typeAt(module, conditionType.child, 'FunctionBody.select')
          : conditionType
      if (conditionScalar._tag !== 'Integer' || conditionScalar.bitWidth !== 1) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.select',
            message: 'select requires an i1 scalar or vector condition',
            input: condition,
          }),
        )
      }
      if (conditionType._tag === 'Vector') {
        const choiceType = yield* FunctionBodyState.typeAt(
          module,
          choices.leftValue.type,
          'FunctionBody.select',
        )
        if (
          choiceType._tag !== 'Vector' ||
          choiceType.length !== conditionType.length ||
          choiceType.scalable !== conditionType.scalable
        ) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.select',
              message: 'Vector select condition and choices must have the same shape',
              input: { condition, onTrue, onFalse },
            }),
          )
        }
      }
      if (
        FastMathActor.toBitcode(fastMath(options.fastMath)) !== 0 &&
        !(yield* FunctionBodyState.isFloatingType(
          module,
          choices.leftValue.type,
          'FunctionBody.select',
        ))
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.select',
            message: 'Fast-math select requires floating-point choices',
            input: { onTrue, onFalse },
          }),
        )
      }
      return (yield* FunctionBodyState.appendResult(
        draft,
        choices.leftValue.type,
        name,
        (result, finalName) =>
          Object.freeze({
            _tag: 'Select',
            condition: selected.operand,
            onTrue: choices.leftValue.operand,
            onFalse: choices.rightValue.operand,
            fastMath: fastMath(options.fastMath),
            result,
            name: finalName,
          }),
      )).value
    }),
  )
})
