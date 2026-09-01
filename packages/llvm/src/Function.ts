import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as Result from 'effect/Result'
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
import * as ResolveActor from './internal/resolveActor.js'
import { invalidInput, invalidState, type LlvmError } from './LlvmError.js'
import * as Metadata from './Metadata.js'
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
const optionalConstant = (
  builder: Builder.Builder,
  owner: BuilderState.State['owner'],
  constant: Constant.Constant | undefined,
  operation: string,
): Result.Result<number | undefined, LlvmError> =>
  constant === undefined
    ? Result.succeed(undefined)
    : Handle.resolve(builder, owner, constant, 'Constant', operation)

/** @internal */
const handleAt = (
  state: BuilderState.MutableState,
  index: number,
  operation: string,
): Result.Result<Function, LlvmError> => {
  const handle = state.globals.functions.handles[index]
  if (handle === undefined) {
    return Result.fail(
      invalidState({ operation, message: 'Function table handle is missing', state: index }),
    )
  }
  return Result.succeed(handle)
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
export const declare = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  name: ByteString.ByteString | Uint8Array | string,
  type: Type.Type,
  options: Options = {},
): Effect.fn.Return<Function, LlvmError> {
  const globalName = ByteString.coerceOrEmpty(name)
  return yield* BuilderState.mutate(builder, 'Function.declare', (state, owner) =>
    Result.gen(function* () {
      const typeIndex = yield* Handle.resolve(builder, owner, type, 'Type', 'Function.declare')
      if (state.types.descriptions[typeIndex]?._tag !== 'Function') {
        return yield* Result.fail(
          invalidInput({
            operation: 'Function.declare',
            message: 'Function declarations require a function type',
            input: type,
          }),
        )
      }
      const callingConvention = options.callingConvention ?? 0
      if (
        !Number.isSafeInteger(callingConvention) ||
        callingConvention < 0 ||
        callingConvention > 1023
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'Function.declare',
            message: 'Calling convention must be an unsigned 10-bit integer',
            input: callingConvention,
          }),
        )
      }
      const attributes =
        options.attributes === undefined
          ? undefined
          : yield* Handle.resolve(
              builder,
              owner,
              options.attributes,
              'FunctionAttributeSet',
              'Function.declare',
            )
      if (globalName.bytes.length > 0) {
        const occupied = state.globals.entries.keys.get(CanonicalKey.bytes(globalName))
        if (occupied !== undefined) {
          const global =
            state.globals.entries.descriptions[
              yield* GlobalState.resolveIndex(state, occupied, 'Function.declare')
            ]
          if (global?.kind === 'Function') {
            const existing = state.globals.functions.descriptions[global.actorIndex]
            if (
              existing !== undefined &&
              compatible(existing, typeIndex, callingConvention, attributes)
            ) {
              return yield* handleAt(state, global.actorIndex, 'Function.declare')
            }
          }
          return yield* Result.fail(
            invalidInput({
              operation: 'Function.declare',
              message: 'An incompatible global already occupies the function name',
              input: name,
            }),
          )
        }
      }
      const index = state.globals.functions.descriptions.length
      const allocated = yield* GlobalState.allocate(
        state,
        owner,
        globalName,
        'Function',
        index,
        options,
        'Function.declare',
      )
      const handle = Handle.make('Function', owner, index)
      state.globals.functions.descriptions.push(
        Object.freeze({
          _tag: 'Function',
          global: allocated.index,
          type: typeIndex,
          callingConvention,
          attributes,
          garbageCollector: ByteString.coerceOrEmpty(options.garbageCollector),
          prefix: yield* optionalConstant(builder, owner, options.prefix, 'Function.declare'),
          prologue: yield* optionalConstant(builder, owner, options.prologue, 'Function.declare'),
          personality: yield* optionalConstant(
            builder,
            owner,
            options.personality,
            'Function.declare',
          ),
          addressSpace: options.addressSpace ?? AddrSpace.defaultAddrSpace,
          body: undefined,
        }),
      )
      state.globals.functions.handles.push(handle)
      return handle
    }),
  )
})

/**
 * Adopts an existing generic global as a function, or returns its existing function identity.
 *
 * @category functions
 * @since 0.0.0
 */
export const fromGlobal = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  global: Global.Global,
  type: Type.Type,
  options: Pick<Options, 'callingConvention' | 'attributes'> = {},
): Effect.fn.Return<Function, LlvmError> {
  return yield* BuilderState.mutate(builder, 'Function.fromGlobal', (state, owner) =>
    Result.gen(function* () {
      const resolved = yield* GlobalState.resolve(
        builder,
        state,
        owner,
        global,
        'Function.fromGlobal',
      )
      if (resolved.description.kind === 'Function') {
        return yield* handleAt(state, resolved.description.actorIndex, 'Function.fromGlobal')
      }
      const typeIndex = yield* Handle.resolve(builder, owner, type, 'Type', 'Function.fromGlobal')
      if (state.types.descriptions[typeIndex]?._tag !== 'Function') {
        return yield* Result.fail(
          invalidInput({
            operation: 'Function.fromGlobal',
            message: 'Function conversion requires a function type',
            input: type,
          }),
        )
      }
      const index = state.globals.functions.descriptions.length
      const handle = Handle.make('Function', owner, index)
      state.globals.functions.descriptions.push(
        Object.freeze({
          _tag: 'Function',
          global: resolved.index,
          type: typeIndex,
          callingConvention: options.callingConvention ?? 0,
          attributes:
            options.attributes === undefined
              ? undefined
              : yield* Handle.resolve(
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
      state.globals.functions.handles.push(handle)
      state.globals.entries.descriptions[resolved.index] = Object.freeze({
        ...resolved.description,
        kind: 'Function',
        actorIndex: index,
      })
      return handle
    }),
  )
})

/**
 * Returns the shared global-symbol handle for a function.
 *
 * @category functions
 * @since 0.0.0
 */
export const global = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  self: Function,
): Effect.fn.Return<Global.Global, LlvmError> {
  return yield* BuilderState.mutate(builder, 'Function.global', (state, owner) =>
    Result.gen(function* () {
      const { description } = yield* ResolveActor.resolve(
        builder,
        owner,
        self,
        'Function',
        state.globals.functions,
        'Function.global',
      )
      return yield* GlobalState.handleAt(state, description.global, 'Function.global')
    }),
  )
})

/**
 * Replaces or clears a function's canonical attribute groups.
 *
 * @category functions
 * @since 0.0.0
 */
export const setAttributes = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  self: Function,
  attributes: Attribute.FunctionSet | undefined,
): Effect.fn.Return<void, LlvmError> {
  yield* BuilderState.mutate(builder, 'Function.setAttributes', (state, owner) =>
    Result.gen(function* () {
      const index = yield* Handle.resolve(
        builder,
        owner,
        self,
        'Function',
        'Function.setAttributes',
      )
      const description = state.globals.functions.descriptions[index]
      if (description === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'Function.setAttributes',
            message: 'Function is missing',
            state: self,
          }),
        )
      }
      state.globals.functions.descriptions[index] = Object.freeze({
        ...description,
        attributes:
          attributes === undefined
            ? undefined
            : yield* Handle.resolve(
                builder,
                owner,
                attributes,
                'FunctionAttributeSet',
                'Function.setAttributes',
              ),
      })
    }),
  )
})

/**
 * Replaces the function's `!dbg` subprogram attachment, or does nothing in strip mode.
 *
 * @category functions
 * @since 0.0.0
 */
export const setSubprogram = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  self: Function,
  subprogram: Metadata.Optional,
): Effect.fn.Return<void, LlvmError> {
  yield* BuilderState.mutate(builder, 'Function.setSubprogram', (state, owner) =>
    Result.gen(function* () {
      if (state.strip || subprogram === undefined) return
      const functionIndex = yield* Handle.resolve(
        builder,
        owner,
        self,
        'Function',
        'Function.setSubprogram',
      )
      const description = state.globals.functions.descriptions[functionIndex]
      if (description === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'Function.setSubprogram',
            message: 'Function is missing',
            state: self,
          }),
        )
      }
      const metadataIndex = yield* Metadata.resolveIndex(
        builder,
        state,
        owner,
        subprogram,
        'Function.setSubprogram',
      )
      if (metadataIndex === undefined) return
      const attachments = state.globals.attachments[description.global] ?? []
      state.globals.attachments[description.global] = Object.freeze([
        ...attachments.filter((attachment) => attachment.kind !== 'dbg'),
        Object.freeze({ kind: 'dbg', metadata: metadataIndex }),
      ])
    }),
  )
})

/**
 * Reads an immutable snapshot of the function type, convention, attributes, and GC name.
 *
 * @category functions
 * @since 0.0.0
 */
export const properties = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  self: Function,
): Effect.fn.Return<Properties, LlvmError> {
  return yield* BuilderState.mutate(builder, 'Function.properties', (state, owner) =>
    Result.gen(function* () {
      const { description } = yield* ResolveActor.resolve(
        builder,
        owner,
        self,
        'Function',
        state.globals.functions,
        'Function.properties',
      )
      const type = state.types.handles[description.type]
      const attributes =
        description.attributes === undefined
          ? undefined
          : state.functionAttributeSets.handles[description.attributes]
      if (
        type === undefined ||
        (description.attributes !== undefined && attributes === undefined)
      ) {
        return yield* Result.fail(
          invalidState({
            operation: 'Function.properties',
            message: 'Function references a missing table entry',
            state: self,
          }),
        )
      }
      return Object.freeze({
        type,
        callingConvention: description.callingConvention,
        attributes,
        garbageCollector: description.garbageCollector,
      })
    }),
  )
})

/** @internal */
const releaseBodyBuild = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  functionIndex: number,
): Effect.fn.Return<void, LlvmError> {
  yield* BuilderState.mutate(builder, 'Function.buildBody.release', (state) => {
    state.buildingFunctions.delete(functionIndex)
    return Result.succeed(undefined)
  })
})

/**
 * Builds, validates, and atomically commits one function body.
 *
 * **Details**
 *
 * The supplied body is valid only in the current fiber and callback.
 * On success, the effect returns the action's value. It preserves the action's additional typed
 * failures and service requirements while adding {@link LlvmError} for body validation and commit.
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
 * import * as Block from '@silklang/llvm/Block'
 * import * as Builder from '@silklang/llvm/Builder'
 * import * as FunctionActor from '@silklang/llvm/Function'
 * import * as FunctionBody from '@silklang/llvm/FunctionBody'
 * import * as Type from '@silklang/llvm/Type'
 *
 * await Effect.runPromise(Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const voidType = yield* Type.voidType(builder)
 *   const signature = yield* Type.functionType(builder, voidType, [])
 *   const fn = yield* FunctionActor.declare(builder, 'noop', signature)
 *   yield* FunctionActor.buildBody(builder, fn, Effect.fnUntraced(function* (body) {
 *     yield* Block.make(body, 'entry')
 *     yield* FunctionBody.returnVoid(body)
 *   }))
 * }))
 * ```
 *
 * @category functions
 * @since 0.0.0
 */
export const buildBody = Effect.fnUntraced(function* <A, E, R>(
  builder: Builder.Builder,
  self: Function,
  action: (body: FunctionBody.FunctionBody) => Effect.Effect<A, E, R>,
): Effect.fn.Return<A, E | LlvmError, R> {
  const fiber = yield* Effect.fiberId
  return yield* Effect.acquireUseRelease(
    BuilderState.mutate(builder, 'Function.buildBody', (state, owner) =>
      Result.gen(function* () {
        const functionIndex = yield* Handle.resolve(
          builder,
          owner,
          self,
          'Function',
          'Function.buildBody',
        )
        const description = state.globals.functions.descriptions[functionIndex]
        const signature =
          description === undefined ? undefined : state.types.descriptions[description.type]
        if (description === undefined || signature?._tag !== 'Function') {
          return yield* Result.fail(
            invalidState({
              operation: 'Function.buildBody',
              message: 'Function or function signature is missing',
              state: self,
            }),
          )
        }
        if (description.body !== undefined) {
          return yield* Result.fail(
            invalidState({
              operation: 'Function.buildBody',
              message: 'Function already has a committed body',
              state: self,
            }),
          )
        }
        if (state.buildingFunctions.has(functionIndex)) {
          return yield* Result.fail(
            invalidInput({
              operation: 'Function.buildBody',
              message: 'Function body construction is already in progress',
              input: self,
            }),
          )
        }
        state.buildingFunctions.add(functionIndex)
        return { functionIndex, type: description.type, signature, owner }
      }),
    ),
    (acquired) => {
      const body = FunctionBodyState.create(
        builder,
        acquired.owner,
        acquired.functionIndex,
        acquired.type,
        acquired.signature,
        fiber,
      )
      return Effect.onExit(
        Effect.gen(function* () {
          const value = yield* action(body)
          const snapshot = yield* FunctionBodyState.validate(body)
          yield* BuilderState.mutate(builder, 'Function.buildBody.commit', (state) =>
            Result.gen(function* () {
              const description = state.globals.functions.descriptions[acquired.functionIndex]
              if (description === undefined || description.body !== undefined) {
                return yield* Result.fail(
                  invalidInput({
                    operation: 'Function.buildBody.commit',
                    message: 'Function changed before its body could commit',
                    input: self,
                  }),
                )
              }
              state.globals.functions.descriptions[acquired.functionIndex] = Object.freeze({
                ...description,
                body: snapshot,
              })
            }),
          )
          return value
        }),
        (exit) =>
          Effect.sync(() => {
            FunctionBodyState.close(body, Exit.isSuccess(exit) ? 'committed' : 'failed')
          }),
      )
    },
    (acquired) => releaseBodyBuild(builder, acquired.functionIndex),
  )
})
