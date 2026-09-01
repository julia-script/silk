import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as ByteString from '../ByteString.js'
import type * as BuilderState from '../internal/BuilderState.js'
import type * as FunctionBodyDescription from '../internal/FunctionBodyDescription.js'
import * as FunctionBodyState from '../internal/FunctionBodyState.js'
import * as Handle from '../internal/Handle.js'
import { invalidInput, type LlvmError } from '../LlvmError.js'
import type * as Type from '../Type.js'
import type * as Value from '../Value.js'
import type { FunctionBody } from './_internal.js'

/**
 * Scalar and vector conversion operations accepted by {@link cast}.
 *
 * @category instructions
 * @since 0.0.0
 */
export type CastKind = FunctionBodyDescription.CastKind

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
export const cast = Effect.fnUntraced(function* (
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
