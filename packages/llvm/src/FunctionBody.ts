import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import * as AddrSpace from './AddrSpace.js'
import * as Alignment from './Alignment.js'
import type * as Attribute from './Attribute.js'
import type * as Block from './Block.js'
import type * as Builder from './Builder.js'
import * as ByteString from './ByteString.js'
import * as Constant from './Constant.js'
import * as FastMathActor from './FastMath.js'
import * as FunctionActor from './Function.js'
import type * as Global from './Global.js'
import * as IntegerMath from './IntegerMath.js'
import type * as BuilderState from './internal/BuilderState.js'
import type * as FunctionBodyDescription from './internal/FunctionBodyDescription.js'
import * as FunctionBodyState from './internal/FunctionBodyState.js'
import * as Handle from './internal/Handle.js'
import { invalidInput, invalidState, type LlvmError } from './LlvmError.js'
import * as MemoryAccess from './MemoryAccess.js'
import * as Metadata from './Metadata.js'
import * as Type from './Type.js'
import type * as Value from './Value.js'

/**
 * Ephemeral body transaction passed only to {@link FunctionActor.buildBody}.
 *
 * @category instructions
 * @since 0.0.0
 */
export interface FunctionBody extends Handle.Handle<'FunctionBody'> {}
/**
 * Opaque identity for an instruction in the current body transaction.
 *
 * @category instructions
 * @since 0.0.0
 */
export interface Instruction extends Handle.Handle<'Instruction'> {}
/**
 * Two-phase identity for a phi instruction whose incoming edges are still being assembled.
 *
 * @category instructions
 * @since 0.0.0
 */
export interface Phi extends Handle.Handle<'Phi'> {}
/**
 * Two-phase identity for a switch terminator whose cases are still being assembled.
 *
 * @category instructions
 * @since 0.0.0
 */
export interface Switch extends Handle.Handle<'Switch'> {}

/**
 * Immutable fast-math flags accepted by floating instructions.
 *
 * @category instructions
 * @since 0.0.0
 */
export type FastMath = FastMathActor.FastMath
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
 * Scalar and vector conversion operations accepted by {@link cast}.
 *
 * @category instructions
 * @since 0.0.0
 */
export type CastKind = FunctionBodyDescription.CastKind
/**
 * Tail-call marker accepted by call operations.
 *
 * @category instructions
 * @since 0.0.0
 */
export type TailKind = FunctionBodyDescription.TailKind

/**
 * Boolean shorthand or partial fast-math settings.
 *
 * @category instructions
 * @since 0.0.0
 */
export type FastMathInput = FastMathActor.Input

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
 * No-wrap promises supported by compatible cast operations.
 *
 * @category instructions
 * @since 0.0.0
 */
export interface CastOptions {
  readonly noSignedWrap?: boolean
  readonly noUnsignedWrap?: boolean
}

/**
 * Calling convention, attributes, tail marker, fast-math, and operand bundles for a call.
 *
 * @category instructions
 * @since 0.0.0
 */
export interface CallOptions {
  readonly callingConvention?: number
  readonly attributes?: Attribute.FunctionSet
  readonly tail?: TailKind
  readonly fastMath?: FastMathInput
  readonly operandBundles?: ReadonlyArray<{
    readonly tag: ByteString.ByteString | Uint8Array | string
    readonly operands: ReadonlyArray<Value.Input>
  }>
}

/**
 * Element count, alignment, address space, and `inalloca` settings for {@link alloca}.
 *
 * @category instructions
 * @since 0.0.0
 */
export interface AllocaOptions {
  readonly count?: Value.Input
  readonly alignment?: Alignment.Alignment
  readonly addressSpace?: AddrSpace.AddrSpace
  readonly inAlloca?: boolean
}

/**
 * Bounds promises and optional in-range index for get-element-pointer operations.
 *
 * @category instructions
 * @since 0.0.0
 */
export interface GetElementPtrOptions {
  readonly inbounds?: boolean
  readonly inrange?: number
}

/**
 * Memory-access settings plus mandatory failure ordering for {@link compareExchange}.
 *
 * @category instructions
 * @since 0.0.0
 */
export interface CompareExchangeOptions extends MemoryAccess.Input {
  readonly failureOrdering: Exclude<
    MemoryAccess.AtomicOrdering,
    'none' | 'unordered' | 'release' | 'acq_rel'
  >
  readonly weak?: boolean
}

const fastMath = FastMathActor.make

/** @internal */
const bytes = (value: ByteString.ByteString | Uint8Array | string): ByteString.ByteString =>
  typeof value === 'string'
    ? ByteString.fromString(value)
    : value instanceof Uint8Array
      ? ByteString.fromUint8Array(value)
      : value

/** @internal */
const sameOperands = (
  draft: FunctionBodyState.Draft,
  module: BuilderState.MutableState,
  left: Value.Input,
  right: Value.Input,
  operation: string,
): Result.Result<
  {
    readonly leftValue: { readonly operand: FunctionBodyDescription.Operand; readonly type: number }
    readonly rightValue: {
      readonly operand: FunctionBodyDescription.Operand
      readonly type: number
    }
  },
  LlvmError
> =>
  Result.gen(function* () {
    const leftValue = yield* FunctionBodyState.resolveOperand(draft, module, left, operation)
    const rightValue = yield* FunctionBodyState.resolveOperand(draft, module, right, operation)
    if (leftValue.type !== rightValue.type) {
      return yield* Result.fail(
        invalidInput({
          operation,
          message: 'Instruction operands must have one LLVM type',
          input: { left, right },
        }),
      )
    }
    return { leftValue, rightValue }
  })

/**
 * Appends floating-point negation after scalar/vector type validation.
 *
 * @category instructions
 * @since 0.0.0
 */
export const unary = Effect.fn('FunctionBody.unary')(function* (
  self: FunctionBody,
  kind: 'fneg',
  operand: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
  options: { readonly fastMath?: FastMathInput } = {},
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.unary', (draft, module) =>
    Result.gen(function* () {
      const resolved = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        operand,
        'FunctionBody.unary',
      )
      if (!(yield* FunctionBodyState.isFloatingType(module, resolved.type, 'FunctionBody.unary'))) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.unary',
            message: 'fneg requires a floating-point scalar or vector',
            input: operand,
          }),
        )
      }
      return (yield* FunctionBodyState.appendResult(
        draft,
        resolved.type,
        name,
        (result, finalName) =>
          Object.freeze({
            _tag: 'Unary',
            kind,
            operand: resolved.operand,
            fastMath: fastMath(options.fastMath),
            result,
            name: finalName,
          }),
      )).value
    }),
  )
})

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
 * import * as Block from '@silk-effect/llvm/Block'
 * import * as Builder from '@silk-effect/llvm/Builder'
 * import * as FunctionActor from '@silk-effect/llvm/Function'
 * import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
 * import * as Type from '@silk-effect/llvm/Type'
 * import * as Value from '@silk-effect/llvm/Value'
 *
 * await Effect.runPromise(Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const i32 = yield* Type.integer(builder, 32)
 *   const signature = yield* Type.functionType(builder, i32, [i32, i32])
 *   const add = yield* FunctionActor.declare(builder, 'add', signature)
 *   yield* FunctionActor.buildBody(builder, add, Effect.fn('Example.add')(function* (body) {
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
export const binary = Effect.fn('FunctionBody.binary')(function* (
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
const integerOperandType = Effect.fn('FunctionBody.integerOperandType')(function* (
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
      const type = module.typeHandles[resolved.type]
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
export const negate = Effect.fn('FunctionBody.negate')(function* (
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
export const bitwiseNot = Effect.fn('FunctionBody.bitwiseNot')(function* (
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
export const integerCompare = Effect.fn('FunctionBody.integerCompare')(function* (
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
export const floatingCompare = Effect.fn('FunctionBody.floatingCompare')(function* (
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
export const select = Effect.fn('FunctionBody.select')(function* (
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

/** @internal */
const scalarWidth = (
  module: BuilderState.MutableState,
  type: number,
): Result.Result<number | undefined, LlvmError> =>
  Result.gen(function* () {
    const description = yield* FunctionBodyState.typeAt(module, type, 'FunctionBody.cast')
    const scalar =
      description._tag === 'Vector'
        ? yield* FunctionBodyState.typeAt(module, description.child, 'FunctionBody.cast')
        : description
    if (scalar._tag === 'Integer') return scalar.bitWidth
    if (scalar._tag !== 'Simple') return undefined
    switch (scalar.tag) {
      case 'Half':
      case 'BFloat':
        return 16
      case 'Float':
        return 32
      case 'Double':
        return 64
      case 'X86Fp80':
        return 80
      case 'Fp128':
      case 'PpcFp128':
        return 128
      default:
        return undefined
    }
  })

/**
 * Appends a legal scalar or vector cast, returning the original operand when its type is unchanged.
 *
 * **Details**
 *
 * Vector conversions preserve lane count and scalability. Width and source/destination
 * families are validated for the selected cast rather than delegated to LLVM diagnostics.
 *
 * @category instructions
 * @since 0.0.0
 */
export const cast = Effect.fn('FunctionBody.cast')(function* (
  self: FunctionBody,
  kind: CastKind,
  operand: Value.Input,
  destinationType: Type.Type,
  name?: ByteString.ByteString | Uint8Array | string,
  options: CastOptions = {},
): Effect.fn.Return<Value.Input, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.cast', (draft, module) =>
    Result.gen(function* () {
      const source = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        operand,
        'FunctionBody.cast',
      )
      const destination = yield* Handle.resolve(
        draft.builder,
        draft.moduleOwner,
        destinationType,
        'Type',
        'FunctionBody.cast',
      )
      if (source.type === destination) return operand
      const sourceInteger = yield* FunctionBodyState.isIntegerType(
        module,
        source.type,
        'FunctionBody.cast',
      )
      const destinationInteger = yield* FunctionBodyState.isIntegerType(
        module,
        destination,
        'FunctionBody.cast',
      )
      const sourceFloating = yield* FunctionBodyState.isFloatingType(
        module,
        source.type,
        'FunctionBody.cast',
      )
      const destinationFloating = yield* FunctionBodyState.isFloatingType(
        module,
        destination,
        'FunctionBody.cast',
      )
      const sourcePointer = yield* FunctionBodyState.isPointerType(
        module,
        source.type,
        'FunctionBody.cast',
      )
      const destinationPointer = yield* FunctionBodyState.isPointerType(
        module,
        destination,
        'FunctionBody.cast',
      )
      const sourceWidth = yield* scalarWidth(module, source.type)
      const destinationWidth = yield* scalarWidth(module, destination)
      const valid =
        (kind === 'trunc' &&
          sourceInteger &&
          destinationInteger &&
          (sourceWidth ?? 0) > (destinationWidth ?? 0)) ||
        ((kind === 'zext' || kind === 'sext') &&
          sourceInteger &&
          destinationInteger &&
          (sourceWidth ?? 0) < (destinationWidth ?? 0)) ||
        ((kind === 'fptoui' || kind === 'fptosi') && sourceFloating && destinationInteger) ||
        ((kind === 'uitofp' || kind === 'sitofp') && sourceInteger && destinationFloating) ||
        (kind === 'fptrunc' &&
          sourceFloating &&
          destinationFloating &&
          (sourceWidth ?? 0) > (destinationWidth ?? 0)) ||
        (kind === 'fpext' &&
          sourceFloating &&
          destinationFloating &&
          (sourceWidth ?? 0) < (destinationWidth ?? 0)) ||
        (kind === 'ptrtoint' && sourcePointer && destinationInteger) ||
        (kind === 'inttoptr' && sourceInteger && destinationPointer) ||
        (kind === 'bitcast' &&
          (sourceWidth === destinationWidth || (sourcePointer && destinationPointer))) ||
        (kind === 'addrspacecast' && sourcePointer && destinationPointer)
      if (!valid) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.cast',
            message: `${kind} is invalid for the source and destination types`,
            input: { operand, destinationType },
          }),
        )
      }
      if ((options.noSignedWrap || options.noUnsignedWrap) && kind !== 'trunc') {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.cast',
            message: 'No-wrap cast flags are only valid on trunc',
            input: kind,
          }),
        )
      }
      return (yield* FunctionBodyState.appendResult(draft, destination, name, (result, finalName) =>
        Object.freeze({
          _tag: 'Cast',
          kind,
          operand: source.operand,
          destinationType: destination,
          noSignedWrap: options.noSignedWrap ?? false,
          noUnsignedWrap: options.noUnsignedWrap ?? false,
          result,
          name: finalName,
        }),
      )).value
    }),
  )
})

/** @internal */
const aggregatePath = (
  module: BuilderState.MutableState,
  root: number,
  indices: ReadonlyArray<number>,
  operation: string,
): Result.Result<number, LlvmError> =>
  Result.gen(function* () {
    if (indices.length === 0) {
      return yield* Result.fail(
        invalidInput({ operation, message: 'Aggregate paths cannot be empty', input: indices }),
      )
    }
    let current = root
    for (const index of indices) {
      if (!Number.isSafeInteger(index) || index < 0) {
        return yield* Result.fail(
          invalidInput({
            operation,
            message: 'Aggregate indices must be non-negative integers',
            input: index,
          }),
        )
      }
      const description = yield* FunctionBodyState.typeAt(module, current, operation)
      if (description._tag === 'Array' || description._tag === 'Vector') {
        const length =
          description._tag === 'Array' ? description.length : BigInt(description.length)
        if (BigInt(index) >= length) {
          return yield* Result.fail(
            invalidInput({
              operation,
              message: 'Aggregate index is outside the aggregate',
              input: index,
            }),
          )
        }
        current = description.child
        continue
      }
      const fields =
        description._tag === 'Structure'
          ? description.fields
          : description._tag === 'NamedStructure'
            ? description.body?.fields
            : undefined
      const field = fields?.[index]
      if (field === undefined) {
        return yield* Result.fail(
          invalidInput({
            operation,
            message: 'Aggregate path does not select a field',
            input: indices,
          }),
        )
      }
      current = field
    }
    return current
  })

/**
 * Extracts a value along a non-empty, statically indexed structure path.
 *
 * @category instructions
 * @since 0.0.0
 */
export const extractValue = Effect.fn('FunctionBody.extractValue')(function* (
  self: FunctionBody,
  aggregate: Value.Input,
  indices: ReadonlyArray<number>,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.extractValue', (draft, module) =>
    Result.gen(function* () {
      const resolved = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        aggregate,
        'FunctionBody.extractValue',
      )
      const resultType = yield* aggregatePath(
        module,
        resolved.type,
        indices,
        'FunctionBody.extractValue',
      )
      return (yield* FunctionBodyState.appendResult(draft, resultType, name, (result, finalName) =>
        Object.freeze({
          _tag: 'ExtractValue',
          aggregate: resolved.operand,
          indices: Object.freeze([...indices]),
          result,
          name: finalName,
        }),
      )).value
    }),
  )
})

/**
 * Inserts a same-typed value along a validated aggregate path.
 *
 * @category instructions
 * @since 0.0.0
 */
export const insertValue = Effect.fn('FunctionBody.insertValue')(function* (
  self: FunctionBody,
  aggregate: Value.Input,
  element: Value.Input,
  indices: ReadonlyArray<number>,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.insertValue', (draft, module) =>
    Result.gen(function* () {
      const aggregateValue = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        aggregate,
        'FunctionBody.insertValue',
      )
      const elementValue = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        element,
        'FunctionBody.insertValue',
      )
      const selected = yield* aggregatePath(
        module,
        aggregateValue.type,
        indices,
        'FunctionBody.insertValue',
      )
      if (elementValue.type !== selected) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.insertValue',
            message: 'Inserted value does not match the aggregate path type',
            input: { aggregate, element, indices },
          }),
        )
      }
      return (yield* FunctionBodyState.appendResult(
        draft,
        aggregateValue.type,
        name,
        (result, finalName) =>
          Object.freeze({
            _tag: 'InsertValue',
            aggregate: aggregateValue.operand,
            element: elementValue.operand,
            indices: Object.freeze([...indices]),
            result,
            name: finalName,
          }),
      )).value
    }),
  )
})

/** @internal */
const accessInfo = (input: MemoryAccess.Input): FunctionBodyDescription.MemoryInfo => {
  const access = MemoryAccess.make(input)
  return Object.freeze({
    kind: access.kind,
    alignment: access.alignment,
    syncScope: access.syncScope,
    ordering: access.ordering,
  })
}

/**
 * Appends stack allocation for a sized element type and optional integer count.
 *
 * @category instructions
 * @since 0.0.0
 */
export const alloca = Effect.fn('FunctionBody.alloca')(function* (
  self: FunctionBody,
  allocationType: Type.Type,
  name?: ByteString.ByteString | Uint8Array | string,
  options: AllocaOptions = {},
): Effect.fn.Return<Value.Value, LlvmError> {
  const builder = yield* FunctionBodyState.builder(self)
  const addressSpace = options.addressSpace ?? AddrSpace.defaultAddrSpace
  const pointerType = yield* Type.pointer(builder, addressSpace)
  const count =
    options.count ?? (yield* Constant.integerUnsigned(builder, yield* Type.integer(builder, 32), 1))
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.alloca', (draft, module) =>
    Result.gen(function* () {
      const allocationTypeIndex = yield* Handle.resolve(
        draft.builder,
        draft.moduleOwner,
        allocationType,
        'Type',
        'FunctionBody.alloca',
      )
      const allocation = yield* FunctionBodyState.typeAt(
        module,
        allocationTypeIndex,
        'FunctionBody.alloca',
      )
      if (
        allocation._tag === 'Simple' &&
        (allocation.tag === 'Void' || allocation.tag === 'Label')
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.alloca',
            message: 'alloca requires an allocatable value type',
            input: allocationType,
          }),
        )
      }
      if (allocation._tag === 'Function') {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.alloca',
            message: 'alloca cannot allocate a function type',
            input: allocationType,
          }),
        )
      }
      const countValue = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        count,
        'FunctionBody.alloca',
      )
      if (
        !(yield* FunctionBodyState.isIntegerType(module, countValue.type, 'FunctionBody.alloca'))
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.alloca',
            message: 'alloca count must have integer type',
            input: count,
          }),
        )
      }
      const resultType = yield* Handle.resolve(
        draft.builder,
        draft.moduleOwner,
        pointerType,
        'Type',
        'FunctionBody.alloca',
      )
      return (yield* FunctionBodyState.appendResult(draft, resultType, name, (result, finalName) =>
        Object.freeze({
          _tag: 'Alloca',
          allocationType: allocationTypeIndex,
          count: countValue.operand,
          addressSpace: addressSpace.value,
          alignment: options.alignment ?? Alignment.defaultAlignment,
          inAlloca: options.inAlloca ?? false,
          result,
          name: finalName,
        }),
      )).value
    }),
  )
})

/**
 * Appends a typed load from a pointer or vector of pointers after ordering validation.
 *
 * @category instructions
 * @since 0.0.0
 */
export const load = Effect.fn('FunctionBody.load')(function* (
  self: FunctionBody,
  valueType: Type.Type,
  pointer: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
  options: MemoryAccess.Input = {},
): Effect.fn.Return<Value.Value, LlvmError> {
  const access = accessInfo(options)
  yield* MemoryAccess.validateLoadOrdering(access.ordering)
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.load', (draft, module) =>
    Result.gen(function* () {
      const pointerValue = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        pointer,
        'FunctionBody.load',
      )
      if (
        !(yield* FunctionBodyState.isPointerType(module, pointerValue.type, 'FunctionBody.load'))
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.load',
            message: 'load requires a pointer operand',
            input: pointer,
          }),
        )
      }
      const type = yield* Handle.resolve(
        draft.builder,
        draft.moduleOwner,
        valueType,
        'Type',
        'FunctionBody.load',
      )
      return (yield* FunctionBodyState.appendResult(draft, type, name, (result, finalName) =>
        Object.freeze({
          _tag: 'Load',
          valueType: type,
          pointer: pointerValue.operand,
          access,
          result,
          name: finalName,
        }),
      )).value
    }),
  )
})

/**
 * Appends a store to a pointer after validating atomic ordering and alignment settings.
 *
 * @category instructions
 * @since 0.0.0
 */
export const store = Effect.fn('FunctionBody.store')(function* (
  self: FunctionBody,
  value: Value.Input,
  pointer: Value.Input,
  options: MemoryAccess.Input = {},
): Effect.fn.Return<Instruction, LlvmError> {
  const access = accessInfo(options)
  yield* MemoryAccess.validateStoreOrdering(access.ordering)
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.store', (draft, module) =>
    Result.gen(function* () {
      const stored = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        value,
        'FunctionBody.store',
      )
      const destination = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        pointer,
        'FunctionBody.store',
      )
      if (
        !(yield* FunctionBodyState.isPointerType(module, destination.type, 'FunctionBody.store'))
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.store',
            message: 'store requires a pointer destination',
            input: pointer,
          }),
        )
      }
      return yield* FunctionBodyState.appendInstruction(
        draft,
        Object.freeze({
          _tag: 'Store',
          value: stored.operand,
          pointer: destination.operand,
          access,
          result: undefined,
          name: ByteString.empty,
        }),
      )
    }),
  )
})

/** @internal */
const gepPlan = Effect.fn('FunctionBody.gepPlan')(function* (
  self: FunctionBody,
  sourceType: Type.Type,
  base: Value.Input,
  indices: ReadonlyArray<Value.Input>,
  options: GetElementPtrOptions,
): Effect.fn.Return<
  {
    readonly sourceType: number
    readonly base: FunctionBodyDescription.Operand
    readonly indices: ReadonlyArray<FunctionBodyDescription.Operand>
    readonly pointerType: Type.Type
    readonly pointerScalarType: Type.Type
    readonly baseIsVector: boolean
    readonly vector: { readonly length: number; readonly scalable: boolean } | undefined
  },
  LlvmError
> {
  return yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.getElementPtr',
    (draft, module) =>
      Result.gen(function* () {
        if (indices.length === 0) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.getElementPtr',
              message: 'getelementptr requires at least one index',
              input: indices,
            }),
          )
        }
        if (
          options.inrange !== undefined &&
          (!Number.isSafeInteger(options.inrange) ||
            options.inrange < 0 ||
            options.inrange >= indices.length)
        ) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.getElementPtr',
              message: 'inrange must identify an existing GEP index',
              input: options.inrange,
            }),
          )
        }
        const source = yield* Handle.resolve(
          draft.builder,
          draft.moduleOwner,
          sourceType,
          'Type',
          'FunctionBody.getElementPtr',
        )
        const pointer = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          base,
          'FunctionBody.getElementPtr',
        )
        const pointerDescription = yield* FunctionBodyState.typeAt(
          module,
          pointer.type,
          'FunctionBody.getElementPtr',
        )
        const pointerScalar =
          pointerDescription._tag === 'Vector'
            ? yield* FunctionBodyState.typeAt(
                module,
                pointerDescription.child,
                'FunctionBody.getElementPtr',
              )
            : pointerDescription
        if (pointerScalar._tag !== 'Pointer') {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.getElementPtr',
              message: 'getelementptr base must be a pointer or vector of pointers',
              input: base,
            }),
          )
        }
        const pointerType = module.typeHandles[pointer.type]
        const pointerScalarType =
          module.typeHandles[
            pointerDescription._tag === 'Vector' ? pointerDescription.child : pointer.type
          ]
        if (pointerType === undefined || pointerScalarType === undefined) {
          return yield* Result.fail(
            invalidState({
              operation: 'FunctionBody.getElementPtr',
              message: 'GEP pointer type handle is missing',
              state: base,
            }),
          )
        }
        let current = source
        let vector =
          pointerDescription._tag === 'Vector'
            ? { length: pointerDescription.length, scalable: pointerDescription.scalable }
            : undefined
        const resolved: Array<FunctionBodyDescription.Operand> = []
        for (let position = 0; position < indices.length; position += 1) {
          const input = indices[position]
          if (input === undefined) continue
          const index = yield* FunctionBodyState.resolveOperand(
            draft,
            module,
            input,
            'FunctionBody.getElementPtr',
          )
          const indexType = yield* FunctionBodyState.typeAt(
            module,
            index.type,
            'FunctionBody.getElementPtr',
          )
          const indexScalar =
            indexType._tag === 'Vector'
              ? yield* FunctionBodyState.typeAt(
                  module,
                  indexType.child,
                  'FunctionBody.getElementPtr',
                )
              : indexType
          if (indexScalar._tag !== 'Integer') {
            return yield* Result.fail(
              invalidInput({
                operation: 'FunctionBody.getElementPtr',
                message: 'getelementptr indices must be integer scalars or vectors',
                input: input,
              }),
            )
          }
          if (indexType._tag === 'Vector') {
            const shape = { length: indexType.length, scalable: indexType.scalable }
            if (
              vector !== undefined &&
              (vector.length !== shape.length || vector.scalable !== shape.scalable)
            ) {
              return yield* Result.fail(
                invalidInput({
                  operation: 'FunctionBody.getElementPtr',
                  message: 'Vector GEP operands must have one vector shape',
                  input: input,
                }),
              )
            }
            vector = shape
          }
          if (position === 0) {
            resolved.push(index.operand)
            continue
          }
          const aggregate = yield* FunctionBodyState.typeAt(
            module,
            current,
            'FunctionBody.getElementPtr',
          )
          if (aggregate._tag === 'Array' || aggregate._tag === 'Vector') {
            current = aggregate.child
            resolved.push(index.operand)
            continue
          }
          const body =
            aggregate._tag === 'Structure'
              ? aggregate
              : aggregate._tag === 'NamedStructure'
                ? aggregate.body
                : undefined
          if (body === undefined || index.operand._tag !== 'Constant') {
            return yield* Result.fail(
              invalidInput({
                operation: 'FunctionBody.getElementPtr',
                message: 'Structure GEP indices must be exact integer constants',
                input: input,
              }),
            )
          }
          const constant = module.constants[index.operand.constant]
          const field =
            constant?._tag === 'Integer' && constant.bitPattern <= BigInt(Number.MAX_SAFE_INTEGER)
              ? body.fields[Number(constant.bitPattern)]
              : undefined
          if (field === undefined) {
            return yield* Result.fail(
              invalidInput({
                operation: 'FunctionBody.getElementPtr',
                message: 'Structure GEP index is outside the selected aggregate',
                input: input,
              }),
            )
          }
          current = field
          resolved.push(index.operand)
        }
        return {
          sourceType: source,
          base: pointer.operand,
          indices: Object.freeze(resolved),
          pointerType,
          pointerScalarType,
          baseIsVector: pointerDescription._tag === 'Vector',
          vector,
        }
      }),
  )
})

/**
 * Appends scalar or vector `getelementptr` with fully typed dynamic indices.
 *
 * **Gotchas**
 *
 * Aggregate paths, integer index types, vector shapes, and `inrange` bounds are validated
 * before the instruction is appended. Use {@link structuredGetElementPtr} for numeric field paths.
 *
 * @category instructions
 * @since 0.0.0
 */
export const getElementPtr = Effect.fn('FunctionBody.getElementPtr')(function* (
  self: FunctionBody,
  sourceType: Type.Type,
  base: Value.Input,
  indices: ReadonlyArray<Value.Input>,
  name?: ByteString.ByteString | Uint8Array | string,
  options: GetElementPtrOptions = {},
): Effect.fn.Return<Value.Value, LlvmError> {
  const plan = yield* gepPlan(self, sourceType, base, indices, options)
  const builder = yield* FunctionBodyState.builder(self)
  const resultType =
    plan.vector === undefined || plan.baseIsVector
      ? plan.pointerType
      : plan.vector.scalable
        ? yield* Type.scalableVector(builder, plan.pointerScalarType, plan.vector.length)
        : yield* Type.vector(builder, plan.pointerScalarType, plan.vector.length)
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.getElementPtr', (draft) =>
    Result.gen(function* () {
      const resultTypeIndex = yield* Handle.resolve(
        draft.builder,
        draft.moduleOwner,
        resultType,
        'Type',
        'FunctionBody.getElementPtr',
      )
      return (yield* FunctionBodyState.appendResult(
        draft,
        resultTypeIndex,
        name,
        (result, finalName) =>
          Object.freeze({
            _tag: 'GetElementPtr',
            sourceType: plan.sourceType,
            base: plan.base,
            indices: plan.indices,
            inbounds: options.inbounds ?? false,
            inrange: options.inrange,
            result,
            name: finalName,
          }),
      )).value
    }),
  )
})

/**
 * Converts numeric aggregate indices to constants and delegates to {@link getElementPtr}.
 *
 * @category instructions
 * @since 0.0.0
 */
export const structuredGetElementPtr = Effect.fn('FunctionBody.structuredGetElementPtr')(function* (
  self: FunctionBody,
  sourceType: Type.Type,
  base: Value.Input,
  fields: ReadonlyArray<number>,
  name?: ByteString.ByteString | Uint8Array | string,
  options: GetElementPtrOptions = {},
): Effect.fn.Return<Value.Value, LlvmError> {
  const builder = yield* FunctionBodyState.builder(self)
  const i32 = yield* Type.integer(builder, 32)
  const exact = [0, ...fields]
  const indices: Array<Constant.Constant> = []
  for (const field of exact) {
    if (!Number.isSafeInteger(field) || field < 0) {
      return yield* Effect.fail(
        invalidInput({
          operation: 'FunctionBody.structuredGetElementPtr',
          message: 'Structured GEP fields must be non-negative integers',
          input: field,
        }),
      )
    }
    indices.push(yield* Constant.integerUnsigned(builder, i32, field))
  }
  return yield* getElementPtr(self, sourceType, base, indices, name, options)
})

/**
 * Builds an aggregate from a poison seed and a complete, exact sequence of element values.
 *
 * @category instructions
 * @since 0.0.0
 */
export const buildAggregate = Effect.fn('FunctionBody.buildAggregate')(function* (
  self: FunctionBody,
  type: Type.Type,
  elements: ReadonlyArray<Value.Input>,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Input, LlvmError> {
  const builder = yield* FunctionBodyState.builder(self)
  const shape = yield* Type.aggregateShape(builder, type)
  const expectedLength = shape.length === undefined ? shape.fields.length : Number(shape.length)
  if (elements.length !== expectedLength) {
    return yield* Effect.fail(
      invalidInput({
        operation: 'FunctionBody.buildAggregate',
        message: 'Aggregate element count does not match its type',
        input: elements,
      }),
    )
  }
  let aggregate: Value.Input = yield* Constant.poison(builder, type)
  for (let index = 0; index < elements.length; index += 1) {
    const element = elements[index]
    if (element === undefined) continue
    aggregate = yield* insertValue(
      self,
      aggregate,
      element,
      [index],
      index === elements.length - 1 ? name : undefined,
    )
  }
  return aggregate
})

/**
 * Extracts one vector lane using a scalar integer index.
 *
 * @category instructions
 * @since 0.0.0
 */
export const extractElement = Effect.fn('FunctionBody.extractElement')(function* (
  self: FunctionBody,
  vector: Value.Input,
  index: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.extractElement',
    (draft, module) =>
      Result.gen(function* () {
        const source = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          vector,
          'FunctionBody.extractElement',
        )
        const sourceType = yield* FunctionBodyState.typeAt(
          module,
          source.type,
          'FunctionBody.extractElement',
        )
        const selected = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          index,
          'FunctionBody.extractElement',
        )
        const selectedType = yield* FunctionBodyState.typeAt(
          module,
          selected.type,
          'FunctionBody.extractElement',
        )
        if (sourceType._tag !== 'Vector' || selectedType._tag !== 'Integer') {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.extractElement',
              message: 'extractelement requires a vector and scalar integer index',
              input: { vector, index },
            }),
          )
        }
        return (yield* FunctionBodyState.appendResult(
          draft,
          sourceType.child,
          name,
          (result, finalName) =>
            Object.freeze({
              _tag: 'ExtractElement',
              vector: source.operand,
              index: selected.operand,
              result,
              name: finalName,
            }),
        )).value
      }),
  )
})

/**
 * Inserts a matching scalar into one vector lane selected by an integer index.
 *
 * @category instructions
 * @since 0.0.0
 */
export const insertElement = Effect.fn('FunctionBody.insertElement')(function* (
  self: FunctionBody,
  vector: Value.Input,
  element: Value.Input,
  index: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.insertElement',
    (draft, module) =>
      Result.gen(function* () {
        const source = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          vector,
          'FunctionBody.insertElement',
        )
        const sourceType = yield* FunctionBodyState.typeAt(
          module,
          source.type,
          'FunctionBody.insertElement',
        )
        const inserted = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          element,
          'FunctionBody.insertElement',
        )
        const selected = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          index,
          'FunctionBody.insertElement',
        )
        const selectedType = yield* FunctionBodyState.typeAt(
          module,
          selected.type,
          'FunctionBody.insertElement',
        )
        if (
          sourceType._tag !== 'Vector' ||
          inserted.type !== sourceType.child ||
          selectedType._tag !== 'Integer'
        ) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.insertElement',
              message: 'insertelement operands do not match the vector shape',
              input: { vector, element, index },
            }),
          )
        }
        return (yield* FunctionBodyState.appendResult(
          draft,
          source.type,
          name,
          (result, finalName) =>
            Object.freeze({
              _tag: 'InsertElement',
              vector: source.operand,
              element: inserted.operand,
              index: selected.operand,
              result,
              name: finalName,
            }),
        )).value
      }),
  )
})

/**
 * Shuffles two same-shaped vectors with a fixed integer mask of compatible length.
 *
 * @category instructions
 * @since 0.0.0
 */
export const shuffleVector = Effect.fn('FunctionBody.shuffleVector')(function* (
  self: FunctionBody,
  left: Value.Input,
  right: Value.Input,
  mask: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  const builder = yield* FunctionBodyState.builder(self)
  const plan = yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.shuffleVector',
    (draft, module) =>
      Result.gen(function* () {
        const values = yield* sameOperands(draft, module, left, right, 'FunctionBody.shuffleVector')
        const vector = yield* FunctionBodyState.typeAt(
          module,
          values.leftValue.type,
          'FunctionBody.shuffleVector',
        )
        const selected = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          mask,
          'FunctionBody.shuffleVector',
        )
        const maskType = yield* FunctionBodyState.typeAt(
          module,
          selected.type,
          'FunctionBody.shuffleVector',
        )
        if (
          vector._tag !== 'Vector' ||
          maskType._tag !== 'Vector' ||
          !(yield* FunctionBodyState.isIntegerType(
            module,
            selected.type,
            'FunctionBody.shuffleVector',
          )) ||
          vector.scalable !== maskType.scalable
        ) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.shuffleVector',
              message: 'shufflevector requires compatible vectors and an integer vector mask',
              input: { left, right, mask },
            }),
          )
        }
        const child = module.typeHandles[vector.child]
        if (child === undefined) {
          return yield* Result.fail(
            invalidState({
              operation: 'FunctionBody.shuffleVector',
              message: 'Vector child type handle is missing',
              state: vector,
            }),
          )
        }
        return {
          left: values.leftValue.operand,
          right: values.rightValue.operand,
          mask: selected.operand,
          child,
          length: maskType.length,
          scalable: maskType.scalable,
        }
      }),
  )
  const resultType = plan.scalable
    ? yield* Type.scalableVector(builder, plan.child, plan.length)
    : yield* Type.vector(builder, plan.child, plan.length)
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.shuffleVector', (draft) =>
    Result.gen(function* () {
      const resultTypeIndex = yield* Handle.resolve(
        draft.builder,
        draft.moduleOwner,
        resultType,
        'Type',
        'FunctionBody.shuffleVector',
      )
      return (yield* FunctionBodyState.appendResult(
        draft,
        resultTypeIndex,
        name,
        (result, finalName) =>
          Object.freeze({
            _tag: 'ShuffleVector',
            left: plan.left,
            right: plan.right,
            mask: plan.mask,
            result,
            name: finalName,
          }),
      )).value
    }),
  )
})

/**
 * Broadcasts a matching scalar across every lane of a fixed or scalable vector.
 *
 * @category instructions
 * @since 0.0.0
 */
export const splatVector = Effect.fn('FunctionBody.splatVector')(function* (
  self: FunctionBody,
  vectorType: Type.Type,
  element: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  const builder = yield* FunctionBodyState.builder(self)
  const shape = yield* Type.aggregateShape(builder, vectorType)
  if (shape.length === undefined || shape.fields.length !== 1) {
    return yield* Effect.fail(
      invalidInput({
        operation: 'FunctionBody.splatVector',
        message: 'splatVector requires a vector result type',
        input: vectorType,
      }),
    )
  }
  const child = shape.fields[0]
  if (child === undefined) {
    return yield* Effect.fail(
      invalidState({
        operation: 'FunctionBody.splatVector',
        message: 'Vector child is missing',
        state: vectorType,
      }),
    )
  }
  const scalarVector = shape.scalable
    ? yield* Type.scalableVector(builder, child, 1)
    : yield* Type.vector(builder, child, 1)
  const i32 = yield* Type.integer(builder, 32)
  const maskType = shape.scalable
    ? yield* Type.scalableVector(builder, i32, Number(shape.length))
    : yield* Type.vector(builder, i32, Number(shape.length))
  const zero = yield* Constant.integerUnsigned(builder, i32, 0)
  const scalarPoison = yield* Constant.poison(builder, scalarVector)
  const mask = yield* Constant.splat(builder, maskType, zero)
  const inserted = yield* insertElement(self, scalarPoison, element, zero)
  return yield* shuffleVector(self, inserted, scalarPoison, mask, name)
})

/**
 * Appends an atomic fence with acquire-or-stronger validated ordering.
 *
 * @category instructions
 * @since 0.0.0
 */
export const fence = Effect.fn('FunctionBody.fence')(function* (
  self: FunctionBody,
  ordering: Exclude<MemoryAccess.AtomicOrdering, 'none' | 'unordered' | 'monotonic'>,
  syncScope: MemoryAccess.SyncScope = 'system',
): Effect.fn.Return<Instruction, LlvmError> {
  yield* MemoryAccess.validateFenceOrdering(ordering)
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.fence', (draft) =>
    FunctionBodyState.appendInstruction(
      draft,
      Object.freeze({
        _tag: 'Fence',
        syncScope,
        ordering,
        result: undefined,
        name: ByteString.empty,
      }),
    ),
  )
})

/**
 * Appends `cmpxchg` and returns LLVM's `{ value, i1 }` result structure.
 *
 * **Gotchas**
 *
 * The failure ordering may not contain release semantics and may not be stronger than the
 * success ordering. Pointer, expected, and replacement types must agree.
 *
 * @category instructions
 * @since 0.0.0
 */
export const compareExchange = Effect.fn('FunctionBody.compareExchange')(function* (
  self: FunctionBody,
  pointer: Value.Input,
  comparison: Value.Input,
  replacement: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
  options: CompareExchangeOptions = { failureOrdering: 'monotonic' },
): Effect.fn.Return<Value.Value, LlvmError> {
  const access = accessInfo({ ...options, ordering: options.ordering ?? 'monotonic' })
  yield* MemoryAccess.validateCompareExchange(access.ordering, options.failureOrdering)
  const builder = yield* FunctionBodyState.builder(self)
  const comparisonType = yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.compareExchange',
    (draft, module) =>
      Result.gen(function* () {
        const value = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          comparison,
          'FunctionBody.compareExchange',
        )
        const handle = module.typeHandles[value.type]
        if (handle === undefined) {
          return yield* Result.fail(
            invalidState({
              operation: 'FunctionBody.compareExchange',
              message: 'Comparison type handle is missing',
              state: comparison,
            }),
          )
        }
        return handle
      }),
  )
  const resultType = yield* Type.structure(builder, [
    comparisonType,
    yield* Type.integer(builder, 1),
  ])
  return yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.compareExchange',
    (draft, module) =>
      Result.gen(function* () {
        const address = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          pointer,
          'FunctionBody.compareExchange',
        )
        const expected = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          comparison,
          'FunctionBody.compareExchange',
        )
        const desired = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          replacement,
          'FunctionBody.compareExchange',
        )
        if (
          !(yield* FunctionBodyState.isPointerType(
            module,
            address.type,
            'FunctionBody.compareExchange',
          )) ||
          expected.type !== desired.type
        ) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.compareExchange',
              message: 'cmpxchg requires a pointer and matching comparison/replacement values',
              input: { pointer, comparison, replacement },
            }),
          )
        }
        const type = yield* Handle.resolve(
          draft.builder,
          draft.moduleOwner,
          resultType,
          'Type',
          'FunctionBody.compareExchange',
        )
        return (yield* FunctionBodyState.appendResult(draft, type, name, (result, finalName) =>
          Object.freeze({
            _tag: 'CompareExchange',
            pointer: address.operand,
            comparison: expected.operand,
            replacement: desired.operand,
            access,
            failureOrdering: options.failureOrdering,
            weak: options.weak ?? false,
            result,
            name: finalName,
          }),
        )).value
      }),
  )
})

/**
 * Appends a type-compatible atomic read-modify-write with at least monotonic ordering.
 *
 * @category instructions
 * @since 0.0.0
 */
export const atomicRmw = Effect.fn('FunctionBody.atomicRmw')(function* (
  self: FunctionBody,
  operation: MemoryAccess.AtomicOperation,
  pointer: Value.Input,
  value: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
  options: MemoryAccess.Input = { ordering: 'monotonic' },
): Effect.fn.Return<Value.Value, LlvmError> {
  const access = accessInfo({ ...options, ordering: options.ordering ?? 'monotonic' })
  yield* MemoryAccess.validateRmwOrdering(access.ordering)
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.atomicRmw', (draft, module) =>
    Result.gen(function* () {
      const address = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        pointer,
        'FunctionBody.atomicRmw',
      )
      const operand = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        value,
        'FunctionBody.atomicRmw',
      )
      if (
        !(yield* FunctionBodyState.isPointerType(module, address.type, 'FunctionBody.atomicRmw'))
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.atomicRmw',
            message: 'atomicrmw requires a pointer operand',
            input: pointer,
          }),
        )
      }
      const floating =
        operation === 'fadd' || operation === 'fsub' || operation === 'fmax' || operation === 'fmin'
      if (
        floating
          ? !(yield* FunctionBodyState.isFloatingType(
              module,
              operand.type,
              'FunctionBody.atomicRmw',
            ))
          : !(yield* FunctionBodyState.isIntegerType(
              module,
              operand.type,
              'FunctionBody.atomicRmw',
            )) && operation !== 'xchg'
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.atomicRmw',
            message: 'atomicrmw operation is incompatible with the value type',
            input: { operation, value },
          }),
        )
      }
      return (yield* FunctionBodyState.appendResult(
        draft,
        operand.type,
        name,
        (result, finalName) =>
          Object.freeze({
            _tag: 'AtomicRmw',
            operation,
            pointer: address.operand,
            value: operand.operand,
            access,
            result,
            name: finalName,
          }),
      )).value
    }),
  )
})

/**
 * Appends `va_arg` from a pointer-like list value and explicit result type.
 *
 * @category instructions
 * @since 0.0.0
 */
export const vaArg = Effect.fn('FunctionBody.vaArg')(function* (
  self: FunctionBody,
  list: Value.Input,
  valueType: Type.Type,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.vaArg', (draft, module) =>
    Result.gen(function* () {
      const source = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        list,
        'FunctionBody.vaArg',
      )
      if (!(yield* FunctionBodyState.isPointerType(module, source.type, 'FunctionBody.vaArg'))) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.vaArg',
            message: 'va_arg requires a pointer list operand',
            input: list,
          }),
        )
      }
      const type = yield* Handle.resolve(
        draft.builder,
        draft.moduleOwner,
        valueType,
        'Type',
        'FunctionBody.vaArg',
      )
      return (yield* FunctionBodyState.appendResult(draft, type, name, (result, finalName) =>
        Object.freeze({
          _tag: 'VaArg',
          list: source.operand,
          valueType: type,
          result,
          name: finalName,
        }),
      )).value
    }),
  )
})

/**
 * Appends `indirectbr` to a non-empty, de-duplicated list of destination blocks.
 *
 * @category instructions
 * @since 0.0.0
 */
export const indirectBranch = Effect.fn('FunctionBody.indirectBranch')(function* (
  self: FunctionBody,
  address: Value.Input,
  destinations: ReadonlyArray<Block.Block>,
): Effect.fn.Return<Instruction, LlvmError> {
  return yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.indirectBranch',
    (draft, module) =>
      Result.gen(function* () {
        const resolved = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          address,
          'FunctionBody.indirectBranch',
        )
        if (
          !(yield* FunctionBodyState.isPointerType(
            module,
            resolved.type,
            'FunctionBody.indirectBranch',
          ))
        ) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.indirectBranch',
              message: 'indirectbr requires a pointer address',
              input: address,
            }),
          )
        }
        if (destinations.length === 0) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.indirectBranch',
              message: 'indirectbr requires at least one destination',
              input: destinations,
            }),
          )
        }
        const mutableBlocks: Array<number> = []
        for (const block of destinations) {
          mutableBlocks.push(
            yield* FunctionBodyState.resolveBlock(draft, block, 'FunctionBody.indirectBranch'),
          )
        }
        const blocks = Object.freeze(mutableBlocks)
        if (new Set(blocks).size !== blocks.length) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.indirectBranch',
              message: 'indirectbr destinations must be unique',
              input: destinations,
            }),
          )
        }
        const predecessor = draft.cursor
        if (predecessor === undefined) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.indirectBranch',
              message: 'Set an insertion block before adding indirectbr',
              input: address,
            }),
          )
        }
        const instruction = yield* FunctionBodyState.appendInstruction(
          draft,
          Object.freeze({
            _tag: 'IndirectBranch',
            address: resolved.operand,
            destinations: blocks,
            result: undefined,
            name: ByteString.empty,
          }),
        )
        for (const block of blocks)
          yield* FunctionBodyState.addPredecessor(draft, block, predecessor)
        return instruction
      }),
  )
})

/**
 * Terminates the insertion block with an unconditional branch and records its predecessor edge.
 *
 * @category instructions
 * @since 0.0.0
 */
export const branch = Effect.fn('FunctionBody.branch')(function* (
  self: FunctionBody,
  destination: Block.Block,
): Effect.fn.Return<Instruction, LlvmError> {
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.branch', (draft) =>
    Result.gen(function* () {
      const block = yield* FunctionBodyState.resolveBlock(draft, destination, 'FunctionBody.branch')
      const predecessor = draft.cursor
      if (predecessor === undefined) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.branch',
            message: 'Set an insertion block before branching',
            input: destination,
          }),
        )
      }
      const instruction = yield* FunctionBodyState.appendInstruction(
        draft,
        Object.freeze({
          _tag: 'Branch',
          destination: block,
          result: undefined,
          name: ByteString.empty,
        }),
      )
      yield* FunctionBodyState.addPredecessor(draft, block, predecessor)
      return instruction
    }),
  )
})

/**
 * Terminates the insertion block with an `i1` conditional branch and records both edges.
 *
 * @category instructions
 * @since 0.0.0
 */
export const conditionalBranch = Effect.fn('FunctionBody.conditionalBranch')(function* (
  self: FunctionBody,
  condition: Value.Input,
  onTrue: Block.Block,
  onFalse: Block.Block,
  weights: 'none' | 'unpredictable' | 'true-likely' | 'false-likely' = 'none',
): Effect.fn.Return<Instruction, LlvmError> {
  const instruction = yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.conditionalBranch',
    (draft, module) =>
      Result.gen(function* () {
        const resolved = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          condition,
          'FunctionBody.conditionalBranch',
        )
        const type = yield* FunctionBodyState.typeAt(
          module,
          resolved.type,
          'FunctionBody.conditionalBranch',
        )
        if (type._tag !== 'Integer' || type.bitWidth !== 1) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.conditionalBranch',
              message: 'Conditional branches require an i1 condition',
              input: condition,
            }),
          )
        }
        const trueBlock = yield* FunctionBodyState.resolveBlock(
          draft,
          onTrue,
          'FunctionBody.conditionalBranch',
        )
        const falseBlock = yield* FunctionBodyState.resolveBlock(
          draft,
          onFalse,
          'FunctionBody.conditionalBranch',
        )
        const predecessor = draft.cursor
        if (predecessor === undefined) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.conditionalBranch',
              message: 'Set an insertion block before branching',
              input: condition,
            }),
          )
        }
        const instruction = yield* FunctionBodyState.appendInstruction(
          draft,
          Object.freeze({
            _tag: 'ConditionalBranch',
            condition: resolved.operand,
            onTrue: trueBlock,
            onFalse: falseBlock,
            weights,
            result: undefined,
            name: ByteString.empty,
          }),
        )
        yield* FunctionBodyState.addPredecessor(draft, trueBlock, predecessor)
        yield* FunctionBodyState.addPredecessor(draft, falseBlock, predecessor)
        return instruction
      }),
  )
  if (weights === 'unpredictable') {
    yield* setUnpredictable(self, instruction)
  } else if (weights === 'true-likely' || weights === 'false-likely') {
    yield* setBranchWeights(self, instruction, weights === 'true-likely' ? [2000, 1] : [1, 2000])
  }
  return instruction
})

/**
 * Replaces one supported metadata attachment on a non-terminator or terminator instruction.
 *
 * @category instructions
 * @since 0.0.0
 */
export const attachMetadata = Effect.fn('FunctionBody.attachMetadata')(function* (
  self: FunctionBody,
  instruction: Instruction,
  kind: 'dbg' | 'prof' | 'unpredictable',
  metadata: Metadata.Optional,
): Effect.fn.Return<void, LlvmError> {
  yield* FunctionBodyState.mutateModule(self, 'FunctionBody.attachMetadata', (draft, module) =>
    Result.gen(function* () {
      if (module.strip || metadata === undefined) return
      const instructionIndex = yield* FunctionBodyState.resolveInstruction(
        draft,
        instruction,
        'FunctionBody.attachMetadata',
      )
      const metadataIndex = yield* Metadata.resolveIndex(
        draft.builder,
        module,
        draft.moduleOwner,
        metadata,
        'FunctionBody.attachMetadata',
      )
      if (metadataIndex === undefined) return
      const attachments = draft.metadata[instructionIndex]
      if (attachments === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.attachMetadata',
            message: 'Instruction metadata table entry is missing',
            state: instruction,
          }),
        )
      }
      const next = attachments.filter((attachment) => attachment.kind !== kind)
      next.push(Object.freeze({ kind, metadata: metadataIndex }))
      draft.metadata[instructionIndex] = next
    }),
  )
})

/**
 * Sets or clears an instruction debug location, doing nothing when metadata stripping is enabled.
 *
 * @category instructions
 * @since 0.0.0
 */
export const setDebugLocation = Effect.fn('FunctionBody.setDebugLocation')(function* (
  self: FunctionBody,
  instruction: Instruction,
  location: Metadata.Optional,
): Effect.fn.Return<void, LlvmError> {
  yield* FunctionBodyState.mutateModule(self, 'FunctionBody.setDebugLocation', (draft, module) =>
    Result.gen(function* () {
      if (module.strip) return
      const instructionIndex = yield* FunctionBodyState.resolveInstruction(
        draft,
        instruction,
        'FunctionBody.setDebugLocation',
      )
      const metadataIndex = yield* Metadata.resolveIndex(
        draft.builder,
        module,
        draft.moduleOwner,
        location,
        'FunctionBody.setDebugLocation',
      )
      if (metadataIndex !== undefined) {
        const entry = module.metadata[metadataIndex]
        if (
          entry?._tag !== 'Forward' &&
          (entry?._tag !== 'Node' || entry.value._tag !== 'Location')
        ) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.setDebugLocation',
              message:
                'Instruction debug location must be a DILocation or a compatible forward reference',
              input: location,
            }),
          )
        }
      }
      draft.debugLocations[instructionIndex] = metadataIndex
    }),
  )
})

/**
 * Attaches non-negative 32-bit branch weights to a conditional branch or switch.
 *
 * @category instructions
 * @since 0.0.0
 */
export const setBranchWeights = Effect.fn('FunctionBody.setBranchWeights')(function* (
  self: FunctionBody,
  instruction: Instruction,
  weights: ReadonlyArray<number>,
): Effect.fn.Return<void, LlvmError> {
  if (
    weights.length < 2 ||
    weights.some((weight) => !Number.isSafeInteger(weight) || weight < 0 || weight > 0xffff_ffff)
  ) {
    return yield* Effect.fail(
      invalidInput({
        operation: 'FunctionBody.setBranchWeights',
        message: 'Branch weights require at least two unsigned 32-bit integers',
        input: weights,
      }),
    )
  }
  const builder = yield* FunctionBodyState.builder(self)
  const i32 = yield* Type.integer(builder, 32)
  const label = yield* Metadata.string(builder, 'branch_weights')
  const values: Array<Metadata.Metadata> = [label]
  for (const weight of weights) {
    const value = yield* Constant.integerUnsigned(builder, i32, weight)
    values.push(yield* Metadata.constant(builder, value))
  }
  const node = yield* Metadata.tuple(builder, values)
  yield* attachMetadata(self, instruction, 'prof', node)
})

/**
 * Marks a conditional branch or switch with LLVM's `unpredictable` metadata.
 *
 * @category instructions
 * @since 0.0.0
 */
export const setUnpredictable = Effect.fn('FunctionBody.setUnpredictable')(function* (
  self: FunctionBody,
  instruction: Instruction,
): Effect.fn.Return<void, LlvmError> {
  const builder = yield* FunctionBodyState.builder(self)
  yield* attachMetadata(self, instruction, 'unpredictable', yield* Metadata.emptyTuple(builder))
})

/**
 * Starts a switch terminator whose cases must be added and then finalized with {@link sealSwitch}.
 *
 * **Details**
 *
 * The switch remains intentionally incomplete so callers can assemble cases incrementally.
 *
 * **Gotchas**
 *
 * The body transaction cannot commit while any switch is unsealed.
 *
 * @category instructions
 * @since 0.0.0
 */
export const switchTerminator = Effect.fn('FunctionBody.switchTerminator')(function* (
  self: FunctionBody,
  value: Value.Input,
  defaultBlock: Block.Block,
  weights: ReadonlyArray<number> = [],
): Effect.fn.Return<Switch, LlvmError> {
  return yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.switchTerminator',
    (draft, module) =>
      Result.gen(function* () {
        const resolved = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          value,
          'FunctionBody.switchTerminator',
        )
        if (
          !(yield* FunctionBodyState.isIntegerType(
            module,
            resolved.type,
            'FunctionBody.switchTerminator',
          ))
        ) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.switchTerminator',
              message: 'switch requires an integer value',
              input: value,
            }),
          )
        }
        const destination = yield* FunctionBodyState.resolveBlock(
          draft,
          defaultBlock,
          'FunctionBody.switchTerminator',
        )
        const predecessor = draft.cursor
        if (predecessor === undefined) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.switchTerminator',
              message: 'Set an insertion block before adding a switch',
              input: value,
            }),
          )
        }
        const instruction = yield* FunctionBodyState.appendInstruction(
          draft,
          Object.freeze({
            _tag: 'Switch',
            value: resolved.operand,
            defaultBlock: destination,
            cases: Object.freeze([]),
            weights: Object.freeze([...weights]),
            sealed: false,
            result: undefined,
            name: ByteString.empty,
          }),
        )
        yield* FunctionBodyState.addPredecessor(draft, destination, predecessor)
        return yield* FunctionBodyState.makeSwitchHandle(draft, instruction)
      }),
  )
})

/**
 * Adds one unique, same-typed integer constant case to an open switch.
 *
 * @category instructions
 * @since 0.0.0
 */
export const addSwitchCase = Effect.fn('FunctionBody.addSwitchCase')(function* (
  self: FunctionBody,
  switchHandle: Switch,
  value: Constant.Constant,
  destination: Block.Block,
): Effect.fn.Return<void, LlvmError> {
  yield* FunctionBodyState.mutateModule(self, 'FunctionBody.addSwitchCase', (draft, module) =>
    Result.gen(function* () {
      const index = yield* FunctionBodyState.resolveSwitch(
        draft,
        switchHandle,
        'FunctionBody.addSwitchCase',
      )
      const instruction = draft.instructions[index]
      if (instruction?._tag !== 'Switch' || instruction.sealed) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.addSwitchCase',
            message: 'Switch is missing or already finalized',
            state: switchHandle,
          }),
        )
      }
      const switchValue =
        instruction.value._tag === 'Constant'
          ? module.constants[instruction.value.constant]
          : draft.values[instruction.value.value]
      const resolved = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        value,
        'FunctionBody.addSwitchCase',
      )
      if (switchValue === undefined || switchValue.type !== resolved.type) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.addSwitchCase',
            message: 'Switch case value must match the switch condition type',
            input: value,
          }),
        )
      }
      const constantIndex =
        resolved.operand._tag === 'Constant' ? resolved.operand.constant : undefined
      if (constantIndex === undefined) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.addSwitchCase',
            message: 'Switch cases must be module constants',
            input: value,
          }),
        )
      }
      if (instruction.cases.some((entry) => entry.value === constantIndex)) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.addSwitchCase',
            message: 'Switch case values must be unique',
            input: value,
          }),
        )
      }
      const block = yield* FunctionBodyState.resolveBlock(
        draft,
        destination,
        'FunctionBody.addSwitchCase',
      )
      const predecessor = draft.blocks.findIndex((candidate) =>
        candidate.instructions.includes(index),
      )
      draft.instructions[index] = Object.freeze({
        ...instruction,
        cases: Object.freeze([
          ...instruction.cases,
          Object.freeze({ value: constantIndex, block }),
        ]),
      })
      if (predecessor >= 0) yield* FunctionBodyState.addPredecessor(draft, block, predecessor)
    }),
  )
})

/**
 * Finalizes a switch exactly once and records predecessor edges for every destination.
 *
 * @category instructions
 * @since 0.0.0
 */
export const sealSwitch = Effect.fn('FunctionBody.sealSwitch')(function* (
  self: FunctionBody,
  switchHandle: Switch,
): Effect.fn.Return<Instruction, LlvmError> {
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.sealSwitch', (draft) =>
    Result.gen(function* () {
      const index = yield* FunctionBodyState.resolveSwitch(
        draft,
        switchHandle,
        'FunctionBody.sealSwitch',
      )
      const instruction = draft.instructions[index]
      if (instruction?._tag !== 'Switch' || instruction.sealed) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.sealSwitch',
            message: 'Switch is missing or already finalized',
            state: switchHandle,
          }),
        )
      }
      if (
        instruction.weights.length > 0 &&
        instruction.weights.length !== instruction.cases.length + 1
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.sealSwitch',
            message: 'Switch weights must include the default and every case',
            input: instruction.weights,
          }),
        )
      }
      draft.instructions[index] = Object.freeze({ ...instruction, sealed: true })
      const handle = draft.instructionHandles[index]
      if (handle === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.sealSwitch',
            message: 'Switch instruction handle is missing',
            state: switchHandle,
          }),
        )
      }
      return handle
    }),
  )
})

/**
 * Terminates the insertion block by returning a value matching the function return type.
 *
 * @category instructions
 * @since 0.0.0
 */
export const returnValue = Effect.fn('FunctionBody.returnValue')(function* (
  self: FunctionBody,
  value: Value.Input,
): Effect.fn.Return<Instruction, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.returnValue', (draft, module) =>
    Result.gen(function* () {
      const resolved = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        value,
        'FunctionBody.returnValue',
      )
      if (resolved.type !== draft.returnType) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.returnValue',
            message: 'Return value does not match the function return type',
            input: value,
          }),
        )
      }
      return yield* FunctionBodyState.appendInstruction(
        draft,
        Object.freeze({
          _tag: 'Return',
          value: resolved.operand,
          result: undefined,
          name: ByteString.empty,
        }),
      )
    }),
  )
})

/**
 * Terminates the insertion block with `ret void` after checking the signature.
 *
 * @category instructions
 * @since 0.0.0
 */
export const returnVoid = Effect.fn('FunctionBody.returnVoid')(function* (
  self: FunctionBody,
): Effect.fn.Return<Instruction, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.returnVoid', (draft, module) =>
    Result.gen(function* () {
      const returnType = yield* FunctionBodyState.typeAt(
        module,
        draft.returnType,
        'FunctionBody.returnVoid',
      )
      if (returnType._tag !== 'Simple' || returnType.tag !== 'Void') {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.returnVoid',
            message: 'returnVoid requires a void function return type',
            input: draft.returnType,
          }),
        )
      }
      return yield* FunctionBodyState.appendInstruction(
        draft,
        Object.freeze({
          _tag: 'ReturnVoid',
          result: undefined,
          name: ByteString.empty,
        }),
      )
    }),
  )
})

/**
 * Terminates the insertion block with LLVM's `unreachable` instruction.
 *
 * @category instructions
 * @since 0.0.0
 */
export const unreachable = Effect.fn('FunctionBody.unreachable')(function* (
  self: FunctionBody,
): Effect.fn.Return<Instruction, LlvmError> {
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.unreachable', (draft) =>
    FunctionBodyState.appendInstruction(
      draft,
      Object.freeze({
        _tag: 'Unreachable',
        result: undefined,
        name: ByteString.empty,
      }),
    ),
  )
})

/**
 * Starts a phi instruction whose incoming edges must be completed with {@link addPhiIncoming}.
 *
 * **Details**
 *
 * Use {@link phiValue} when later instructions need the result before the phi is sealed.
 * The final incoming block set must cover every predecessor exactly once.
 *
 * @category instructions
 * @since 0.0.0
 */
export const phi = Effect.fn('FunctionBody.phi')(function* (
  self: FunctionBody,
  type: Type.Type,
  name?: ByteString.ByteString | Uint8Array | string,
  options: { readonly fastMath?: FastMathInput } = {},
): Effect.fn.Return<Phi, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.phi', (draft, module) =>
    Result.gen(function* () {
      const typeIndex = yield* Handle.resolve(
        draft.builder,
        draft.moduleOwner,
        type,
        'Type',
        'FunctionBody.phi',
      )
      if (
        FastMathActor.toBitcode(fastMath(options.fastMath)) !== 0 &&
        !(yield* FunctionBodyState.isFloatingType(module, typeIndex, 'FunctionBody.phi'))
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.phi',
            message: 'Fast-math phi requires a floating-point type',
            input: type,
          }),
        )
      }
      const appended = yield* FunctionBodyState.appendResult(
        draft,
        typeIndex,
        name,
        (result, finalName) =>
          Object.freeze({
            _tag: 'Phi',
            type: typeIndex,
            incoming: Object.freeze([]),
            fastMath: fastMath(options.fastMath),
            sealed: false,
            result,
            name: finalName,
          }),
      )
      return yield* FunctionBodyState.makePhiHandle(draft, appended.instruction)
    }),
  )
})

/**
 * Returns the SSA result of an open or sealed phi instruction.
 *
 * @category instructions
 * @since 0.0.0
 */
export const phiValue = Effect.fn('FunctionBody.phiValue')(function* (
  self: FunctionBody,
  phiHandle: Phi,
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.phiValue', (draft) =>
    Result.gen(function* () {
      const instruction =
        draft.instructions[
          yield* FunctionBodyState.resolvePhi(draft, phiHandle, 'FunctionBody.phiValue')
        ]
      if (instruction?._tag !== 'Phi') {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.phiValue',
            message: 'Phi instruction is missing',
            state: phiHandle,
          }),
        )
      }
      const value = draft.valueHandles[instruction.result]
      if (value === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.phiValue',
            message: 'Phi result value is missing',
            state: phiHandle,
          }),
        )
      }
      return value
    }),
  )
})

/**
 * Adds one same-typed value from a unique predecessor block to an open phi.
 *
 * @category instructions
 * @since 0.0.0
 */
export const addPhiIncoming = Effect.fn('FunctionBody.addPhiIncoming')(function* (
  self: FunctionBody,
  phiHandle: Phi,
  value: Value.Input,
  block: Block.Block,
): Effect.fn.Return<void, LlvmError> {
  yield* FunctionBodyState.mutateModule(self, 'FunctionBody.addPhiIncoming', (draft, module) =>
    Result.gen(function* () {
      const index = yield* FunctionBodyState.resolvePhi(
        draft,
        phiHandle,
        'FunctionBody.addPhiIncoming',
      )
      const instruction = draft.instructions[index]
      if (instruction?._tag !== 'Phi' || instruction.sealed) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.addPhiIncoming',
            message: 'Phi is missing or already sealed',
            state: phiHandle,
          }),
        )
      }
      const resolved = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        value,
        'FunctionBody.addPhiIncoming',
      )
      if (resolved.type !== instruction.type) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.addPhiIncoming',
            message: 'Phi incoming value has the wrong type',
            input: value,
          }),
        )
      }
      const blockIndex = yield* FunctionBodyState.resolveBlock(
        draft,
        block,
        'FunctionBody.addPhiIncoming',
      )
      if (instruction.incoming.some((entry) => entry.block === blockIndex)) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.addPhiIncoming',
            message: 'Phi already has an incoming value for this block',
            state: block,
          }),
        )
      }
      draft.instructions[index] = Object.freeze({
        ...instruction,
        incoming: Object.freeze([
          ...instruction.incoming,
          Object.freeze({ value: resolved.operand, block: blockIndex }),
        ]),
      })
    }),
  )
})

/**
 * Validates predecessor coverage, finalizes a phi exactly once, and returns its SSA value.
 *
 * @category instructions
 * @since 0.0.0
 */
export const sealPhi = Effect.fn('FunctionBody.sealPhi')(function* (
  self: FunctionBody,
  phiHandle: Phi,
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.sealPhi', (draft) =>
    Result.gen(function* () {
      const index = yield* FunctionBodyState.resolvePhi(draft, phiHandle, 'FunctionBody.sealPhi')
      const instruction = draft.instructions[index]
      if (instruction?._tag !== 'Phi' || instruction.sealed) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.sealPhi',
            message: 'Phi is missing or already sealed',
            state: phiHandle,
          }),
        )
      }
      draft.instructions[index] = Object.freeze({ ...instruction, sealed: true })
      const value = draft.valueHandles[instruction.result]
      if (value === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.sealPhi',
            message: 'Phi result value is missing',
            state: phiHandle,
          }),
        )
      }
      return value
    }),
  )
})

/** @internal */
const callInternal = Effect.fn('FunctionBody.callInternal')(function* (
  self: FunctionBody,
  functionType: Type.Type,
  callee: Value.Input,
  args: ReadonlyArray<Value.Input>,
  name: ByteString.ByteString | Uint8Array | string | undefined,
  options: CallOptions,
): Effect.fn.Return<Value.Value | undefined, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.call', (draft, module) =>
    Result.gen(function* () {
      const functionTypeIndex = yield* Handle.resolve(
        draft.builder,
        draft.moduleOwner,
        functionType,
        'Type',
        'FunctionBody.call',
      )
      const signature = yield* FunctionBodyState.typeAt(
        module,
        functionTypeIndex,
        'FunctionBody.call',
      )
      if (signature._tag !== 'Function') {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.call',
            message: 'Calls require a function type',
            input: functionType,
          }),
        )
      }
      const calleeValue = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        callee,
        'FunctionBody.call',
      )
      const calleeConstant =
        calleeValue.operand._tag === 'Constant'
          ? module.constants[calleeValue.operand.constant]
          : undefined
      const inlineAssembly =
        calleeConstant?._tag === 'Assembly' && calleeValue.type === functionTypeIndex
      if (
        !inlineAssembly &&
        !(yield* FunctionBodyState.isPointerType(module, calleeValue.type, 'FunctionBody.call'))
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.call',
            message: 'Call callee must have pointer type',
            input: callee,
          }),
        )
      }
      if (
        (!signature.variadic && args.length !== signature.parameters.length) ||
        args.length < signature.parameters.length
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.call',
            message: 'Call argument count does not match the function signature',
            input: args,
          }),
        )
      }
      const argumentsResolved: Array<FunctionBodyDescription.Operand> = []
      for (let index = 0; index < args.length; index += 1) {
        const argument = args[index]
        if (argument === undefined) continue
        const resolved = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          argument,
          'FunctionBody.call',
        )
        const expected = signature.parameters[index]
        if (expected !== undefined && expected !== resolved.type) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.call',
              message: 'Call argument type does not match its parameter',
              input: { index, argument },
            }),
          )
        }
        argumentsResolved.push(resolved.operand)
      }
      const callingConvention = options.callingConvention ?? 0
      if (
        !Number.isSafeInteger(callingConvention) ||
        callingConvention < 0 ||
        callingConvention > 1023
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.call',
            message: 'Calling convention must be an unsigned 10-bit integer',
            input: callingConvention,
          }),
        )
      }
      const attributes =
        options.attributes === undefined
          ? undefined
          : yield* Handle.resolve(
              draft.builder,
              draft.moduleOwner,
              options.attributes,
              'FunctionAttributeSet',
              'FunctionBody.call',
            )
      const bundles: Array<FunctionBodyDescription.OperandBundle> = []
      for (const bundle of options.operandBundles ?? []) {
        const operands: Array<FunctionBodyDescription.Operand> = []
        for (const operand of bundle.operands) {
          operands.push(
            (yield* FunctionBodyState.resolveOperand(draft, module, operand, 'FunctionBody.call'))
              .operand,
          )
        }
        bundles.push(
          Object.freeze({
            tag: bytes(bundle.tag),
            operands: Object.freeze(operands),
          }),
        )
      }
      const returnType = yield* FunctionBodyState.typeAt(
        module,
        signature.returnType,
        'FunctionBody.call',
      )
      const callFastMath = fastMath(options.fastMath)
      if (
        FastMathActor.toBitcode(callFastMath) !== 0 &&
        !(yield* FunctionBodyState.isFloatingType(
          module,
          signature.returnType,
          'FunctionBody.call',
        ))
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.call',
            message: 'Fast-math call requires a floating-point return type',
            input: functionType,
          }),
        )
      }
      if (options.tail === 'musttail' && functionTypeIndex !== draft.functionType) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.call',
            message: 'musttail requires the caller and callee to have the same function type',
            input: functionType,
          }),
        )
      }
      if (returnType._tag === 'Simple' && returnType.tag === 'Void') {
        yield* FunctionBodyState.appendInstruction(
          draft,
          Object.freeze({
            _tag: 'Call',
            functionType: functionTypeIndex,
            callee: calleeValue.operand,
            arguments: Object.freeze(argumentsResolved),
            callingConvention,
            attributes,
            tail: options.tail ?? 'none',
            fastMath: callFastMath,
            operandBundles: Object.freeze(bundles),
            result: undefined,
            name: ByteString.empty,
          }),
        )
        return undefined
      }
      return (yield* FunctionBodyState.appendResult(
        draft,
        signature.returnType,
        name,
        (result, finalName) =>
          Object.freeze({
            _tag: 'Call',
            functionType: functionTypeIndex,
            callee: calleeValue.operand,
            arguments: Object.freeze(argumentsResolved),
            callingConvention,
            attributes,
            tail: options.tail ?? 'none',
            fastMath: callFastMath,
            operandBundles: Object.freeze(bundles),
            result,
            name: finalName,
          }),
      )).value
    }),
  )
})

/**
 * Calls a typed callee operand with exact arity, argument types, attributes, and operand bundles.
 *
 * @category instructions
 * @since 0.0.0
 */
export const call = Effect.fn('FunctionBody.call')(function* (
  self: FunctionBody,
  functionType: Type.Type,
  callee: Value.Input,
  args: ReadonlyArray<Value.Input>,
  name?: ByteString.ByteString | Uint8Array | string,
  options: CallOptions = {},
): Effect.fn.Return<Value.Value | undefined, LlvmError> {
  return yield* callInternal(self, functionType, callee, args, name, options)
})

/**
 * Interns inline assembly as a constant and appends a type-checked call to it.
 *
 * @category instructions
 * @since 0.0.0
 */
export const callAssembly = Effect.fn('FunctionBody.callAssembly')(function* (
  self: FunctionBody,
  functionType: Type.Type,
  assembly: ByteString.ByteString | Uint8Array | string,
  constraints: ByteString.ByteString | Uint8Array | string,
  args: ReadonlyArray<Value.Input>,
  name?: ByteString.ByteString | Uint8Array | string,
  assemblyOptions: Constant.AssemblyOptions = {},
  callOptions: CallOptions = {},
): Effect.fn.Return<Value.Value | undefined, LlvmError> {
  const builder = yield* FunctionBodyState.builder(self)
  const callee = yield* Constant.assembly(
    builder,
    functionType,
    assembly,
    constraints,
    assemblyOptions,
  )
  return yield* callInternal(self, functionType, callee, args, name, callOptions)
})

/**
 * Calls a declared function while inheriting its signature, convention, and default attributes.
 *
 * @category instructions
 * @since 0.0.0
 */
export const callDirect = Effect.fn('FunctionBody.callDirect')(function* (
  self: FunctionBody,
  targetFunction: FunctionActor.Function,
  args: ReadonlyArray<Value.Input>,
  name?: ByteString.ByteString | Uint8Array | string,
  options: CallOptions = {},
): Effect.fn.Return<Value.Value | undefined, LlvmError> {
  const builder = yield* FunctionBodyState.builder(self)
  const properties = yield* FunctionActor.properties(builder, targetFunction)
  const global: Global.Global = yield* FunctionActor.global(builder, targetFunction)
  const callee = yield* Constant.fromGlobal(builder, global)
  const attributes = options.attributes ?? properties.attributes
  return yield* callInternal(self, properties.type, callee, args, name, {
    ...options,
    callingConvention: options.callingConvention ?? properties.callingConvention,
    ...(attributes === undefined ? {} : { attributes }),
  })
})

/**
 * Returns an instruction's SSA result, or `undefined` for void and terminator instructions.
 *
 * @category instructions
 * @since 0.0.0
 */
export const instructionResult = Effect.fn('FunctionBody.instructionResult')(function* (
  self: FunctionBody,
  instruction: Instruction,
): Effect.fn.Return<Value.Value | undefined, LlvmError> {
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.instructionResult', (draft) =>
    FunctionBodyState.instructionResult(draft, instruction),
  )
})

/**
 * Returns an instruction's zero-based semantic index in the body.
 *
 * @category instructions
 * @since 0.0.0
 */
export const instructionIndex = Effect.fn('FunctionBody.instructionIndex')(function* (
  self: FunctionBody,
  instruction: Instruction,
): Effect.fn.Return<number, LlvmError> {
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.instructionIndex', (draft) =>
    FunctionBodyState.resolveInstruction(draft, instruction, 'FunctionBody.instructionIndex'),
  )
})

/**
 * Returns the module builder that owns an open body transaction.
 *
 * @category instructions
 * @since 0.0.0
 */
export const builder = Effect.fn('FunctionBody.builder')(function* (
  self: FunctionBody,
): Effect.fn.Return<Builder.Builder, LlvmError> {
  return yield* FunctionBodyState.builder(self)
})

/**
 * Resolves the LLVM type of either a local SSA value or a module constant operand.
 *
 * @category instructions
 * @since 0.0.0
 */
export const inputType = Effect.fn('FunctionBody.inputType')(function* (
  self: FunctionBody,
  input: Value.Input,
): Effect.fn.Return<Type.Type, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.inputType', (draft, module) =>
    Result.gen(function* () {
      const resolved = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        input,
        'FunctionBody.inputType',
      )
      const type = module.typeHandles[resolved.type]
      if (type === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.inputType',
            message: 'Input type handle is missing',
            state: input,
          }),
        )
      }
      return type
    }),
  )
})
