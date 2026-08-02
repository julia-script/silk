import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as AddrSpace from './AddrSpace.js'
import type * as Attribute from './Attribute.js'
import type * as Builder from './Builder.js'
import * as ByteString from './ByteString.js'
import type * as Constant from './Constant.js'
import type * as FunctionBody from './FunctionBody.js'
import type * as Global from './Global.js'
import * as BuilderState from './internal/BuilderState.js'
import * as CanonicalKey from './internal/CanonicalKey.js'
import * as FunctionBodyState from './internal/FunctionBodyState.js'
import type * as GlobalDescription from './internal/GlobalDescription.js'
import * as GlobalState from './internal/GlobalState.js'
import * as Handle from './internal/Handle.js'
import * as Metadata from './Metadata.js'
import { SilkError } from './SilkError.js'
import type * as Type from './Type.js'

/**
 * Opaque builder-owned identity for an LLVM function declaration or definition.
 *
 * @category functions
 * @since 0.0.0
 */
export interface Function extends Handle.Handle<'Function'> {}

/**
 * Common global and function-specific declaration properties.
 *
 * @category functions
 * @since 0.0.0
 */
export interface Options extends Global.Options {
  readonly callingConvention?: number
  readonly attributes?: Attribute.FunctionSet
  readonly garbageCollector?: ByteString.ByteString | Uint8Array | string
  readonly prefix?: Constant.Constant
  readonly prologue?: Constant.Constant
  readonly personality?: Constant.Constant
}

/**
 * Read-only function-specific properties.
 *
 * @category functions
 * @since 0.0.0
 */
export interface Properties {
  readonly type: Type.Type
  readonly callingConvention: number
  readonly attributes: Attribute.FunctionSet | undefined
  readonly garbageCollector: ByteString.ByteString
}

/** @internal */
const bytes = (
  value: ByteString.ByteString | Uint8Array | string | undefined,
): ByteString.ByteString =>
  value === undefined
    ? ByteString.empty
    : typeof value === 'string'
      ? ByteString.fromString(value)
      : value instanceof Uint8Array
        ? ByteString.fromUint8Array(value)
        : value

/** @internal */
const optionalConstant = (
  builder: Builder.Builder,
  owner: BuilderState.State['owner'],
  constant: Constant.Constant | undefined,
  operation: string,
): number | undefined =>
  constant === undefined
    ? undefined
    : Handle.resolve(builder, owner, constant, 'Constant', operation)

/** @internal */
const handleAt = (state: BuilderState.MutableState, index: number, operation: string): Function => {
  const handle = state.functionHandles[index]
  if (handle === undefined) {
    throw new SilkError({ operation, message: 'Function table handle is missing', cause: index })
  }
  return handle
}

/** @internal */
const compatible = (
  description: GlobalDescription.FunctionDescription,
  type: number,
  callingConvention: number,
  attributes: number | undefined,
): boolean =>
  description.type === type &&
  description.callingConvention === callingConvention &&
  description.attributes === attributes

/**
 * Declares a function with a function type and canonical module-global name.
 *
 * **Details**
 *
 * Repeating an exactly compatible named declaration returns the existing identity.
 *
 * **Gotchas**
 *
 * An incompatible collision, non-function type, invalid calling convention, or foreign handle
 * fails transactionally.
 *
 * @category functions
 * @since 0.0.0
 */
export const declare = Effect.fn('Function.declare')(function* (
  builder: Builder.Builder,
  name: ByteString.ByteString | Uint8Array | string,
  type: Type.Type,
  options: Options = {},
): Effect.fn.Return<Function, SilkError> {
  const globalName = bytes(name)
  return yield* BuilderState.mutate(builder, 'Function.declare', (state, owner) => {
    const typeIndex = Handle.resolve(builder, owner, type, 'Type', 'Function.declare')
    if (state.types[typeIndex]?._tag !== 'Function') {
      throw new SilkError({
        operation: 'Function.declare',
        message: 'Function declarations require a function type',
        cause: type,
      })
    }
    const callingConvention = options.callingConvention ?? 0
    if (
      !Number.isSafeInteger(callingConvention) ||
      callingConvention < 0 ||
      callingConvention > 1023
    ) {
      throw new SilkError({
        operation: 'Function.declare',
        message: 'Calling convention must be an unsigned 10-bit integer',
        cause: callingConvention,
      })
    }
    const attributes =
      options.attributes === undefined
        ? undefined
        : Handle.resolve(
            builder,
            owner,
            options.attributes,
            'FunctionAttributeSet',
            'Function.declare',
          )
    if (globalName.bytes.length > 0) {
      const occupied = state.globalNames.get(CanonicalKey.bytes(globalName))
      if (occupied !== undefined) {
        const global = state.globals[GlobalState.resolveIndex(state, occupied, 'Function.declare')]
        if (global?.kind === 'Function') {
          const existing = state.functions[global.actorIndex]
          if (
            existing !== undefined &&
            compatible(existing, typeIndex, callingConvention, attributes)
          ) {
            return handleAt(state, global.actorIndex, 'Function.declare')
          }
        }
        throw new SilkError({
          operation: 'Function.declare',
          message: 'An incompatible global already occupies the function name',
          cause: name,
        })
      }
    }
    const index = state.functions.length
    const allocated = GlobalState.allocate(
      state,
      owner,
      globalName,
      'Function',
      index,
      options,
      'Function.declare',
    )
    const handle = Handle.make('Function', owner, index)
    state.functions.push(
      Object.freeze({
        _tag: 'Function',
        global: allocated.index,
        type: typeIndex,
        callingConvention,
        attributes,
        garbageCollector: bytes(options.garbageCollector),
        prefix: optionalConstant(builder, owner, options.prefix, 'Function.declare'),
        prologue: optionalConstant(builder, owner, options.prologue, 'Function.declare'),
        personality: optionalConstant(builder, owner, options.personality, 'Function.declare'),
        addressSpace: options.addressSpace ?? AddrSpace.defaultAddrSpace,
        body: undefined,
      }),
    )
    state.functionHandles.push(handle)
    return handle
  })
})

/**
 * Adopts an existing generic global as a function, or returns its existing function identity.
 *
 * @category functions
 * @since 0.0.0
 */
export const fromGlobal = Effect.fn('Function.fromGlobal')(function* (
  builder: Builder.Builder,
  global: Global.Global,
  type: Type.Type,
  options: Pick<Options, 'callingConvention' | 'attributes'> = {},
): Effect.fn.Return<Function, SilkError> {
  return yield* BuilderState.mutate(builder, 'Function.fromGlobal', (state, owner) => {
    const resolved = GlobalState.resolve(builder, state, owner, global, 'Function.fromGlobal')
    if (resolved.description.kind === 'Function') {
      return handleAt(state, resolved.description.actorIndex, 'Function.fromGlobal')
    }
    const typeIndex = Handle.resolve(builder, owner, type, 'Type', 'Function.fromGlobal')
    if (state.types[typeIndex]?._tag !== 'Function') {
      throw new SilkError({
        operation: 'Function.fromGlobal',
        message: 'Function conversion requires a function type',
        cause: type,
      })
    }
    const index = state.functions.length
    const handle = Handle.make('Function', owner, index)
    state.functions.push(
      Object.freeze({
        _tag: 'Function',
        global: resolved.index,
        type: typeIndex,
        callingConvention: options.callingConvention ?? 0,
        attributes:
          options.attributes === undefined
            ? undefined
            : Handle.resolve(
                builder,
                owner,
                options.attributes,
                'FunctionAttributeSet',
                'Function.fromGlobal',
              ),
        garbageCollector: ByteString.empty,
        prefix: undefined,
        prologue: undefined,
        personality: undefined,
        addressSpace: resolved.description.addressSpace,
        body: undefined,
      }),
    )
    state.functionHandles.push(handle)
    state.globals[resolved.index] = Object.freeze({
      ...resolved.description,
      kind: 'Function',
      actorIndex: index,
    })
    return handle
  })
})

/**
 * Returns the shared global-symbol handle for a function.
 *
 * @category functions
 * @since 0.0.0
 */
export const global = Effect.fn('Function.global')(function* (
  builder: Builder.Builder,
  self: Function,
): Effect.fn.Return<Global.Global, SilkError> {
  return yield* BuilderState.mutate(builder, 'Function.global', (state, owner) => {
    const index = Handle.resolve(builder, owner, self, 'Function', 'Function.global')
    const description = state.functions[index]
    if (description === undefined) {
      throw new SilkError({
        operation: 'Function.global',
        message: 'Function is missing',
        cause: self,
      })
    }
    return GlobalState.handleAt(state, description.global, 'Function.global')
  })
})

/**
 * Replaces or clears a function's canonical attribute groups.
 *
 * @category functions
 * @since 0.0.0
 */
export const setAttributes = Effect.fn('Function.setAttributes')(function* (
  builder: Builder.Builder,
  self: Function,
  attributes: Attribute.FunctionSet | undefined,
): Effect.fn.Return<void, SilkError> {
  yield* BuilderState.mutate(builder, 'Function.setAttributes', (state, owner) => {
    const index = Handle.resolve(builder, owner, self, 'Function', 'Function.setAttributes')
    const description = state.functions[index]
    if (description === undefined) {
      throw new SilkError({
        operation: 'Function.setAttributes',
        message: 'Function is missing',
        cause: self,
      })
    }
    state.functions[index] = Object.freeze({
      ...description,
      attributes:
        attributes === undefined
          ? undefined
          : Handle.resolve(
              builder,
              owner,
              attributes,
              'FunctionAttributeSet',
              'Function.setAttributes',
            ),
    })
  })
})

/**
 * Replaces the function's `!dbg` subprogram attachment, or does nothing in strip mode.
 *
 * @category functions
 * @since 0.0.0
 */
export const setSubprogram = Effect.fn('Function.setSubprogram')(function* (
  builder: Builder.Builder,
  self: Function,
  subprogram: Metadata.Optional,
): Effect.fn.Return<void, SilkError> {
  yield* BuilderState.mutate(builder, 'Function.setSubprogram', (state, owner) => {
    if (state.strip || subprogram === undefined) return
    const functionIndex = Handle.resolve(builder, owner, self, 'Function', 'Function.setSubprogram')
    const description = state.functions[functionIndex]
    if (description === undefined) {
      throw new SilkError({
        operation: 'Function.setSubprogram',
        message: 'Function is missing',
        cause: self,
      })
    }
    const metadataIndex = Metadata.resolveIndex(
      builder,
      state,
      owner,
      subprogram,
      'Function.setSubprogram',
    )
    if (metadataIndex === undefined) return
    const attachments = state.globalMetadata[description.global] ?? []
    state.globalMetadata[description.global] = Object.freeze([
      ...attachments.filter((attachment) => attachment.kind !== 'dbg'),
      Object.freeze({ kind: 'dbg', metadata: metadataIndex }),
    ])
  })
})

/**
 * Reads an immutable snapshot of the function type, convention, attributes, and GC name.
 *
 * @category functions
 * @since 0.0.0
 */
export const properties = Effect.fn('Function.properties')(function* (
  builder: Builder.Builder,
  self: Function,
): Effect.fn.Return<Properties, SilkError> {
  return yield* BuilderState.mutate(builder, 'Function.properties', (state, owner) => {
    const index = Handle.resolve(builder, owner, self, 'Function', 'Function.properties')
    const description = state.functions[index]
    const type = description === undefined ? undefined : state.typeHandles[description.type]
    const attributes =
      description?.attributes === undefined
        ? undefined
        : state.functionAttributeSetHandles[description.attributes]
    if (
      description === undefined ||
      type === undefined ||
      (description.attributes !== undefined && attributes === undefined)
    ) {
      throw new SilkError({
        operation: 'Function.properties',
        message: 'Function references a missing table entry',
        cause: self,
      })
    }
    return Object.freeze({
      type,
      callingConvention: description.callingConvention,
      attributes,
      garbageCollector: description.garbageCollector,
    })
  })
})

/** @internal */
const releaseBodyBuild = (
  builder: Builder.Builder,
  functionIndex: number,
): Effect.Effect<void, SilkError> =>
  BuilderState.mutate(builder, 'Function.buildBody.release', (state) => {
    state.buildingFunctions.delete(functionIndex)
  })

/**
 * Builds, validates, and atomically commits one function body.
 *
 * **Details**
 *
 * The supplied body is valid only in the current fiber and callback.
 * On success, the effect returns the action's value. It preserves the action's additional typed
 * failures and service requirements while adding {@link SilkError} for body validation and commit.
 *
 * **Gotchas**
 *
 * If the callback fails, a block lacks a terminator, SSA forwards remain unresolved, or phi
 * coverage is incomplete, the draft is discarded and the function remains a declaration that may
 * be retried.
 *
 * **Example** (Committing a function body)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Block from '@silk-effect/llvm/Block'
 * import * as Builder from '@silk-effect/llvm/Builder'
 * import * as FunctionActor from '@silk-effect/llvm/Function'
 * import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
 * import * as Type from '@silk-effect/llvm/Type'
 *
 * await Effect.runPromise(Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const voidType = yield* Type.voidType(builder)
 *   const signature = yield* Type.functionType(builder, voidType, [])
 *   const fn = yield* FunctionActor.declare(builder, 'noop', signature)
 *   yield* FunctionActor.buildBody(builder, fn, Effect.fn('Example.noop')(function* (body) {
 *     yield* Block.make(body, 'entry')
 *     yield* FunctionBody.returnVoid(body)
 *   }))
 * }))
 * ```
 *
 * @category functions
 * @since 0.0.0
 */
export const buildBody = Effect.fn('Function.buildBody')(function* <A, E, R>(
  builder: Builder.Builder,
  self: Function,
  action: (body: FunctionBody.FunctionBody) => Effect.Effect<A, E, R>,
): Effect.fn.Return<A, E | SilkError, R> {
  const fiber = yield* Effect.fiberId
  const acquired = yield* BuilderState.mutate(builder, 'Function.buildBody', (state, owner) => {
    const functionIndex = Handle.resolve(builder, owner, self, 'Function', 'Function.buildBody')
    const description = state.functions[functionIndex]
    const signature = description === undefined ? undefined : state.types[description.type]
    if (description === undefined || signature?._tag !== 'Function') {
      throw new SilkError({
        operation: 'Function.buildBody',
        message: 'Function or function signature is missing',
        cause: self,
      })
    }
    if (description.body !== undefined) {
      throw new SilkError({
        operation: 'Function.buildBody',
        message: 'Function already has a committed body',
        cause: self,
      })
    }
    if (state.buildingFunctions.has(functionIndex)) {
      throw new SilkError({
        operation: 'Function.buildBody',
        message: 'Function body construction is already in progress',
        cause: self,
      })
    }
    state.buildingFunctions.add(functionIndex)
    return { functionIndex, type: description.type, signature, owner }
  })
  const body = FunctionBodyState.create(
    builder,
    acquired.owner,
    acquired.functionIndex,
    acquired.type,
    acquired.signature,
    fiber,
  )
  const actionExit = yield* Effect.exit(action(body))
  if (Exit.isFailure(actionExit)) {
    FunctionBodyState.close(body, 'failed')
    yield* releaseBodyBuild(builder, acquired.functionIndex)
    return yield* Effect.failCause(actionExit.cause)
  }
  const validationExit = yield* Effect.exit(FunctionBodyState.validate(body))
  if (Exit.isFailure(validationExit)) {
    FunctionBodyState.close(body, 'failed')
    yield* releaseBodyBuild(builder, acquired.functionIndex)
    return yield* Effect.failCause(validationExit.cause)
  }
  const commitExit = yield* Effect.exit(
    BuilderState.mutate(builder, 'Function.buildBody.commit', (state) => {
      const description = state.functions[acquired.functionIndex]
      if (description === undefined || description.body !== undefined) {
        throw new SilkError({
          operation: 'Function.buildBody.commit',
          message: 'Function changed before its body could commit',
          cause: self,
        })
      }
      state.functions[acquired.functionIndex] = Object.freeze({
        ...description,
        body: validationExit.value,
      })
      state.buildingFunctions.delete(acquired.functionIndex)
    }),
  )
  if (Exit.isFailure(commitExit)) {
    FunctionBodyState.close(body, 'failed')
    yield* releaseBodyBuild(builder, acquired.functionIndex)
    return yield* Effect.failCause(commitExit.cause)
  }
  FunctionBodyState.close(body, 'committed')
  return actionExit.value
})
