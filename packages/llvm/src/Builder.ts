import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import * as Semaphore from 'effect/Semaphore'
import * as ByteString from './ByteString.js'
import * as DataLayout from './DataLayout.js'
import * as BuilderState from './internal/BuilderState.js'
import * as OwnedHandle from './internal/OwnedHandle.js'
import { invalidState, type LlvmError } from './LlvmError.js'

/**
 * An opaque, concurrency-safe owner of LLVM IR state.
 *
 * @category builders
 * @since 0.0.0
 */
export interface Builder {
  readonly _tag: 'Builder'
}

/**
 * Immutable module configuration captured when a {@link Builder} is created.
 *
 * @category builders
 * @since 0.0.0
 */
export interface Options {
  readonly strip?: boolean
  readonly moduleName?: ByteString.ByteString | Uint8Array | string
  readonly sourceFilename?: ByteString.ByteString | Uint8Array | string
  readonly targetTriple?: ByteString.ByteString | Uint8Array | string
  readonly dataLayout?: ByteString.ByteString | Uint8Array | string
  readonly moduleAssembly?: ByteString.ByteString | Uint8Array | string
}

const HandleTypeId: unique symbol = Symbol.for('@silk-effect/llvm/Builder/Handle')

/**
 * A lightweight opaque handle used to enforce builder ownership.
 *
 * @category builders
 * @since 0.0.0
 */
export interface Handle {
  readonly _tag: 'Handle'
  readonly [HandleTypeId]: typeof HandleTypeId
}

const handles = new WeakMap<Handle, OwnedHandle.OwnedHandle>()

/** @internal */
const bytes = (
  value: ByteString.ByteString | Uint8Array | string | undefined,
): ByteString.ByteString => {
  if (value === undefined) return ByteString.empty
  if (typeof value === 'string') return ByteString.fromString(value)
  return value instanceof Uint8Array ? ByteString.fromUint8Array(value) : value
}

/**
 * Creates the isolated state owner for one LLVM module.
 *
 * **Details**
 *
 * Every type, global, constant, function, local value, and metadata node created from the builder
 * is owner-checked. Mutations pass through one Effect semaphore, so concurrent fibers cannot lose
 * committed updates. Debug information is stripped by default; pass `strip: false` to retain it.
 *
 * Options set module headers, target layout, initial assembly, and the debug-strip policy.
 *
 * **Gotchas**
 *
 * An invalid data layout fails with {@link LlvmError} before the builder is created.
 *
 * **Example** (Creating a module)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silk-effect/llvm/Builder'
 * import * as IrText from '@silk-effect/llvm/IrText'
 *
 * const text = await Effect.runPromise(
 *   Effect.gen(function* () {
 *     const builder = yield* Builder.make({
 *       sourceFilename: 'answer.ll',
 *       targetTriple: 'wasm32-unknown-unknown',
 *     })
 *     return yield* IrText.render(builder)
 *   }),
 * )
 * ```
 *
 * @category builders
 * @since 0.0.0
 */
export const make = Effect.fn('Builder.make')(function* (
  options: Options = {},
): Effect.fn.Return<Builder, LlvmError> {
  const gate = yield* Semaphore.make(1)
  const dataLayout = bytes(options.dataLayout)
  const layout = yield* DataLayout.parse(dataLayout)
  const self: Builder = Object.freeze({ _tag: 'Builder' })
  BuilderState.register(self, {
    owner: OwnedHandle.makeOwner(),
    gate,
    value: {
      strip: options.strip ?? true,
      moduleName: bytes(options.moduleName),
      sourceFilename: bytes(options.sourceFilename),
      targetTriple: bytes(options.targetTriple),
      dataLayout,
      layout,
      moduleAssembly: options.moduleAssembly === undefined ? [] : [bytes(options.moduleAssembly)],
      nextHandle: 0,
      strings: [],
      stringKeys: new Map(),
      types: [],
      typeHandles: [],
      typeKeys: new Map(),
      namedTypes: new Map(),
      attributes: [],
      attributeHandles: [],
      attributeKeys: new Map(),
      attributeSets: [],
      attributeSetHandles: [],
      attributeSetKeys: new Map(),
      functionAttributeSets: [],
      functionAttributeSetHandles: [],
      functionAttributeSetKeys: new Map(),
      constants: [],
      constantHandles: [],
      constantKeys: new Map(),
      globals: [],
      globalHandles: [],
      globalNames: new Map(),
      nextAnonymousGlobal: 0,
      variables: [],
      variableHandles: [],
      aliases: [],
      aliasHandles: [],
      functions: [],
      functionHandles: [],
      buildingFunctions: new Set(),
      metadata: [],
      metadataHandles: [],
      metadataStringKeys: new Map(),
      metadataNodeKeys: new Map(),
      namedMetadata: [],
      namedMetadataKeys: new Map(),
      globalMetadata: [],
    },
  })
  return self
})

/**
are  * Allocates a lightweight handle whose owner is checked by every consuming operation.
 *
 * **When to use**
 *
 * Use when implementing low-level actor infrastructure. Public LLVM actors allocate their own
 * typed handles.
 *
 * @category builders
 * @since 0.0.0
 */
export const allocateHandle = Effect.fn('Builder.allocateHandle')(function* (
  self: Builder,
): Effect.fn.Return<Handle, LlvmError> {
  return yield* BuilderState.mutate(self, 'Builder.allocateHandle', (state, owner) => {
    const handle: Handle = { _tag: 'Handle', [HandleTypeId]: HandleTypeId }
    Object.freeze(handle)
    handles.set(handle, OwnedHandle.make(owner, state.nextHandle))
    state.nextHandle += 1
    return Result.succeed(handle)
  })
})

/**
 * Validates that a low-level handle belongs to this builder, failing with {@link LlvmError}.
 *
 * @category builders
 * @since 0.0.0
 */
export const validateHandle = Effect.fn('Builder.validateHandle')(function* (
  self: Builder,
  handle: Handle,
): Effect.fn.Return<void, LlvmError> {
  yield* BuilderState.mutate(self, 'Builder.validateHandle', (_state, owner) =>
    Result.gen(function* () {
      const owned = handles.get(handle)
      if (owned === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'Builder.validateHandle',
            message: 'Unknown LLVM builder handle',
            state: handle,
          }),
        )
      }
      yield* OwnedHandle.ensureOwner(owner, owned, 'Builder.validateHandle')
    }),
  )
})

/**
 * Appends one module-level assembly fragment in serialized commit order.
 *
 * @category builders
 * @since 0.0.0
 */
export const appendModuleAssembly = Effect.fn('Builder.appendModuleAssembly')(function* (
  self: Builder,
  assembly: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<void, LlvmError> {
  const value = bytes(assembly)
  yield* BuilderState.mutate(self, 'Builder.appendModuleAssembly', (state) => {
    state.moduleAssembly.push(value)
    return Result.succeed(undefined)
  })
})
