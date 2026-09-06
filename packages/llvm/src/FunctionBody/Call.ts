import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Block from '../Block.js'
import type * as Attribute from '../Attribute.js'
import * as ByteString from '../ByteString.js'
import * as Constant from '../Constant.js'
import * as FastMathActor from '../FastMath.js'
import * as FunctionActor from '../Function.js'
import type * as Global from '../Global.js'
import type * as FunctionBodyDescription from '../internal/FunctionBodyDescription.js'
import * as FunctionBodyState from '../internal/FunctionBodyState.js'
import * as Handle from '../internal/Handle.js'
import { invalidInput, type LlvmError } from '../LlvmError.js'
import type * as Type from '../Type.js'
import type * as Value from '../Value.js'
import { type FastMathInput, type FunctionBody, fastMath } from './_internal.js'

/**
 * Tail-call marker accepted by call operations.
 *
 * @category instructions
 * @since 0.0.0
 */
export type TailKind = FunctionBodyDescription.TailKind

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

/** @internal */
const callInternal = Effect.fnUntraced(function* (
  self: FunctionBody,
  functionType: Type.Type,
  callee: Value.Input,
  args: ReadonlyArray<Value.Input>,
  name: ByteString.ByteString | Uint8Array | string | undefined,
  options: CallOptions,
  destinations?: { readonly normal: Block.Block; readonly unwind: Block.Block },
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
          ? module.constants.descriptions[calleeValue.operand.constant]
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
          const actualDescription = module.types.descriptions[resolved.type]
          const expectedDescription = module.types.descriptions[expected]
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.call',
              message: `Call${name === undefined ? '' : ` ${ByteString.escapeForIr(ByteString.coerce(name))}`} argument ${index} type does not match its parameter (${actualDescription?._tag ?? resolved.type} !== ${expectedDescription?._tag ?? expected})`,
              input: { index, argument, actualType: resolved.type, expectedType: expected },
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
            tag: ByteString.coerce(bundle.tag),
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
      const normal =
        destinations === undefined
          ? undefined
          : yield* FunctionBodyState.resolveBlock(draft, destinations.normal, 'FunctionBody.invoke')
      const unwind =
        destinations === undefined
          ? undefined
          : yield* FunctionBodyState.resolveBlock(draft, destinations.unwind, 'FunctionBody.invoke')
      if (
        destinations !== undefined &&
        (options.tail !== undefined ||
          inlineAssembly ||
          signature.variadic ||
          FastMathActor.toBitcode(callFastMath) !== 0)
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.invoke',
            message:
              'Invoke requires a fixed signature without tail markers, inline assembly, or fast-math flags',
            input: options,
          }),
        )
      }
      const makeInstruction = (
        result: number | undefined,
        finalName: ByteString.ByteString,
      ): FunctionBodyDescription.Instruction => {
        const fields = {
          functionType: functionTypeIndex,
          callee: calleeValue.operand,
          arguments: Object.freeze(argumentsResolved),
          callingConvention,
          attributes,
          fastMath: callFastMath,
          operandBundles: Object.freeze(bundles),
          result,
          name: finalName,
        }
        return normal === undefined || unwind === undefined
          ? Object.freeze({ ...fields, _tag: 'Call', tail: options.tail ?? 'none' })
          : Object.freeze({ ...fields, _tag: 'Invoke', normal, unwind })
      }
      const predecessor = draft.cursor
      let value: Value.Value | undefined
      if (returnType._tag === 'Simple' && returnType.tag === 'Void') {
        yield* FunctionBodyState.appendInstruction(
          draft,
          makeInstruction(undefined, ByteString.empty),
        )
      } else {
        value = (yield* FunctionBodyState.appendResult(
          draft,
          signature.returnType,
          name,
          makeInstruction,
        )).value
      }
      if (normal !== undefined && unwind !== undefined && predecessor !== undefined) {
        yield* FunctionBodyState.addPredecessor(draft, normal, predecessor)
        yield* FunctionBodyState.addPredecessor(draft, unwind, predecessor)
      }
      return value
    }),
  )
})

/**
 * Calls a typed callee operand with exact arity, argument types, attributes, and operand bundles.
 *
 * @category instructions
 * @since 0.0.0
 */
export const call = Effect.fnUntraced(function* (
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
export const callAssembly = Effect.fnUntraced(function* (
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
export const callDirect = Effect.fnUntraced(function* (
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
 * Calls a fixed-signature callee and terminates the block with normal and unwind successors.
 *
 * The unwind successor must start with a cleanup landing pad and the function must have a
 * personality. The result is available only along the normal edge.
 *
 * @category instructions
 * @since 0.0.0
 */
export const invoke = Effect.fn('FunctionBody.invoke')(function* (
  self: FunctionBody,
  functionType: Type.Type,
  callee: Value.Input,
  args: ReadonlyArray<Value.Input>,
  normal: Block.Block,
  unwind: Block.Block,
  name?: ByteString.ByteString | Uint8Array | string,
  options: Omit<CallOptions, 'tail' | 'fastMath'> = {},
): Effect.fn.Return<Value.Value | undefined, LlvmError> {
  return yield* callInternal(self, functionType, callee, args, name, options, { normal, unwind })
})
