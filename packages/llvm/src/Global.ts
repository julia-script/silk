import * as Effect from 'effect/Effect'
import type * as Builder from './Builder.js'
import * as ByteString from './ByteString.js'
import * as BuilderState from './internal/BuilderState.js'
import * as CanonicalKey from './internal/CanonicalKey.js'
import type * as GlobalDescription from './internal/GlobalDescription.js'
import * as GlobalState from './internal/GlobalState.js'
import type * as Handle from './internal/Handle.js'
import * as Metadata from './Metadata.js'
import { SilkError } from './SilkError.js'

/**
 * Opaque identity shared by variables, aliases, and functions in the module symbol table.
 *
 * @category globals
 * @since 0.0.0
 */
export interface Global extends Handle.Handle<'Global'> {}

/**
 * LLVM linkage accepted by global declarations.
 *
 * @category globals
 * @since 0.0.0
 */
export type Linkage = GlobalDescription.Linkage
/**
 * LLVM symbol visibility accepted by global declarations.
 *
 * @category globals
 * @since 0.0.0
 */
export type Visibility = GlobalDescription.Visibility
/**
 * Runtime preemption behavior for a global symbol.
 *
 * @category globals
 * @since 0.0.0
 */
export type Preemption = GlobalDescription.Preemption
/**
 * Platform DLL import/export storage class.
 *
 * @category globals
 * @since 0.0.0
 */
export type DllStorage = GlobalDescription.DllStorage
/**
 * Whether address significance is local, global, or ordinary.
 *
 * @category globals
 * @since 0.0.0
 */
export type UnnamedAddress = GlobalDescription.UnnamedAddress
/**
 * Common creation and update properties shared by every global actor.
 *
 * @category globals
 * @since 0.0.0
 */
export type Options = GlobalState.CommonOptions

/**
 * Read-only common properties and concrete actor kind of a global symbol.
 *
 * @category globals
 * @since 0.0.0
 */
export interface Properties extends GlobalDescription.Common {
  readonly kind: GlobalDescription.GlobalDescription['kind']
}

/** @internal */
const bytes = (value: ByteString.ByteString | Uint8Array | string): ByteString.ByteString =>
  typeof value === 'string'
    ? ByteString.fromString(value)
    : value instanceof Uint8Array
      ? ByteString.fromUint8Array(value)
      : value

/**
 * Looks up an active global by its exact byte name.
 *
 * **Details**
 *
 * Variables, aliases, and functions share this lookup table. Renamed, removed, and replaced source
 * names are absent. The returned handle is the common global identity; use {@link kind} to inspect
 * its actor category when the declaration type is not already known.
 * JavaScript names are encoded as UTF-8; byte strings preserve an exact lookup key. A missing name
 * returns `undefined`.
 *
 * **Example** (Looking up a global)
 *
 * Find the shared global identity created for a variable.
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silk-effect/llvm/Builder'
 * import * as Global from '@silk-effect/llvm/Global'
 * import * as Type from '@silk-effect/llvm/Type'
 * import * as Variable from '@silk-effect/llvm/Variable'
 *
 * const matches = await Effect.runPromise(Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const i32 = yield* Type.integer(builder, 32)
 *   const variable = yield* Variable.make(builder, 'answer', i32)
 *   const expected = yield* Variable.global(builder, variable)
 *   return (yield* Global.lookup(builder, 'answer')) === expected
 * }))
 * // matches === true
 * ```
 *
 * @category globals
 * @since 0.0.0
 */
export const lookup = Effect.fn('Global.lookup')(function* (
  builder: Builder.Builder,
  name: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Global | undefined, SilkError> {
  const value = bytes(name)
  return yield* BuilderState.mutate(builder, 'Global.lookup', (state) => {
    const index = state.globalNames.get(CanonicalKey.bytes(value))
    if (index === undefined) return undefined
    const resolved = GlobalState.resolveIndex(state, index, 'Global.lookup')
    const description = state.globals[resolved]
    return description === undefined || description.deleted
      ? undefined
      : GlobalState.handleAt(state, resolved, 'Global.lookup')
  })
})

/**
 * Reads a global's current byte-exact name.
 *
 * @category globals
 * @since 0.0.0
 */
export const name = Effect.fn('Global.name')(function* (
  builder: Builder.Builder,
  self: Global,
): Effect.fn.Return<ByteString.ByteString, SilkError> {
  return yield* BuilderState.mutate(
    builder,
    'Global.name',
    (state, owner) =>
      GlobalState.resolve(builder, state, owner, self, 'Global.name').description.name,
  )
})

/**
 * Identifies whether a global currently represents a variable, alias, or function.
 *
 * @category globals
 * @since 0.0.0
 */
export const kind = Effect.fn('Global.kind')(function* (
  builder: Builder.Builder,
  self: Global,
): Effect.fn.Return<GlobalDescription.GlobalDescription['kind'], SilkError> {
  return yield* BuilderState.mutate(
    builder,
    'Global.kind',
    (state, owner) =>
      GlobalState.resolve(builder, state, owner, self, 'Global.kind').description.kind,
  )
})

/**
 * Reads an immutable snapshot of a global's common properties.
 *
 * @category globals
 * @since 0.0.0
 */
export const properties = Effect.fn('Global.properties')(function* (
  builder: Builder.Builder,
  self: Global,
): Effect.fn.Return<Properties, SilkError> {
  return yield* BuilderState.mutate(builder, 'Global.properties', (state, owner) => {
    const description = GlobalState.resolve(
      builder,
      state,
      owner,
      self,
      'Global.properties',
    ).description
    return Object.freeze({
      name: description.name,
      addressSpace: description.addressSpace,
      linkage: description.linkage,
      visibility: description.visibility,
      preemption: description.preemption,
      dllStorage: description.dllStorage,
      unnamedAddress: description.unnamedAddress,
      section: description.section,
      alignment: description.alignment,
      kind: description.kind,
    })
  })
})

/**
 * Atomically moves a global to a new non-empty, unoccupied symbol name.
 *
 * **Gotchas**
 *
 * Empty names, collisions, inactive handles, and cross-builder handles fail with
 * {@link SilkError} without mutating the global.
 *
 * @category globals
 * @since 0.0.0
 */
export const rename = Effect.fn('Global.rename')(function* (
  builder: Builder.Builder,
  self: Global,
  nextName: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<void, SilkError> {
  const requested = bytes(nextName)
  yield* BuilderState.mutate(builder, 'Global.rename', (state, owner) => {
    const { description, index } = GlobalState.resolve(builder, state, owner, self, 'Global.rename')
    if (requested.bytes.length === 0) {
      throw new SilkError({
        operation: 'Global.rename',
        message: 'Renaming requires an explicit non-empty global name',
        cause: nextName,
      })
    }
    const nextKey = CanonicalKey.bytes(requested)
    const occupied = state.globalNames.get(nextKey)
    if (
      occupied !== undefined &&
      GlobalState.resolveIndex(state, occupied, 'Global.rename') !== index
    ) {
      throw new SilkError({
        operation: 'Global.rename',
        message: 'LLVM global name is already occupied',
        cause: nextName,
      })
    }
    state.globalNames.delete(CanonicalKey.bytes(description.name))
    state.globalNames.set(nextKey, index)
    state.globals[index] = Object.freeze({ ...description, name: requested })
  })
})

/**
 * Applies only the supplied common properties, preserving every omitted field.
 *
 * @category globals
 * @since 0.0.0
 */
export const configure = Effect.fn('Global.configure')(function* (
  builder: Builder.Builder,
  self: Global,
  options: Options,
): Effect.fn.Return<void, SilkError> {
  yield* BuilderState.mutate(builder, 'Global.configure', (state, owner) => {
    const { description, index } = GlobalState.resolve(
      builder,
      state,
      owner,
      self,
      'Global.configure',
    )
    state.globals[index] = Object.freeze({
      ...description,
      addressSpace: options.addressSpace ?? description.addressSpace,
      linkage: options.linkage ?? description.linkage,
      visibility: options.visibility ?? description.visibility,
      preemption: options.preemption ?? description.preemption,
      dllStorage: options.dllStorage ?? description.dllStorage,
      unnamedAddress: options.unnamedAddress ?? description.unnamedAddress,
      section: options.section ?? description.section,
      alignment: options.alignment ?? description.alignment,
    })
  })
})

/**
 * Replaces one global metadata attachment kind, or does nothing when debug stripping is enabled.
 *
 * @category globals
 * @since 0.0.0
 */
export const attachMetadata = Effect.fn('Global.attachMetadata')(function* (
  builder: Builder.Builder,
  self: Global,
  kind: 'dbg' | 'prof' | 'unpredictable',
  metadata: Metadata.Optional,
): Effect.fn.Return<void, SilkError> {
  yield* BuilderState.mutate(builder, 'Global.attachMetadata', (state, owner) => {
    if (state.strip || metadata === undefined) return
    const { index } = GlobalState.resolve(builder, state, owner, self, 'Global.attachMetadata')
    const metadataIndex = Metadata.resolveIndex(
      builder,
      state,
      owner,
      metadata,
      'Global.attachMetadata',
    )
    if (metadataIndex === undefined) return
    const current = state.globalMetadata[index] ?? []
    state.globalMetadata[index] = Object.freeze([
      ...current.filter((attachment) => attachment.kind !== kind),
      Object.freeze({ kind, metadata: metadataIndex }),
    ])
  })
})

/**
 * Redirects references from one global identity to another and retires the source symbol.
 *
 * **Details**
 *
 * Replacement preserves handle-resolution chains; replacing a global with itself is a no-op.
 *
 * @category globals
 * @since 0.0.0
 */
export const replace = Effect.fn('Global.replace')(function* (
  builder: Builder.Builder,
  self: Global,
  replacement: Global,
): Effect.fn.Return<void, SilkError> {
  yield* BuilderState.mutate(builder, 'Global.replace', (state, owner) => {
    const source = GlobalState.resolve(builder, state, owner, self, 'Global.replace')
    const target = GlobalState.resolve(builder, state, owner, replacement, 'Global.replace')
    if (source.index === target.index) return
    state.globalNames.delete(CanonicalKey.bytes(source.description.name))
    state.globals[source.index] = Object.freeze({
      ...source.description,
      replacement: target.index,
      deleted: true,
    })
  })
})

/**
 * Removes a global from lookup and serialization and invalidates its active actor handle.
 *
 * @category globals
 * @since 0.0.0
 */
export const remove = Effect.fn('Global.remove')(function* (
  builder: Builder.Builder,
  self: Global,
): Effect.fn.Return<void, SilkError> {
  yield* BuilderState.mutate(builder, 'Global.remove', (state, owner) => {
    const { description, index } = GlobalState.resolve(builder, state, owner, self, 'Global.remove')
    state.globalNames.delete(CanonicalKey.bytes(description.name))
    state.globals[index] = Object.freeze({ ...description, deleted: true })
  })
})
