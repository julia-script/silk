import * as Effect from 'effect/Effect'
import type * as Builder from './Builder.js'
import * as ByteString from './ByteString.js'
import type * as Constant from './Constant.js'
import type * as Global from './Global.js'
import * as BuilderState from './internal/BuilderState.js'
import type * as GlobalDescription from './internal/GlobalDescription.js'
import * as GlobalState from './internal/GlobalState.js'
import * as Handle from './internal/Handle.js'
import { SilkError } from './SilkError.js'
import type * as Type from './Type.js'

/**
 * Opaque builder-owned identity for an LLVM global alias.
 *
 * @category aliases
 * @since 0.0.0
 */
export interface Alias extends Handle.Handle<'Alias'> {}

/**
 * Common global properties accepted when creating an alias.
 *
 * @category aliases
 * @since 0.0.0
 */
export interface Options extends Global.Options {}

/** @internal */
const bytes = (value: ByteString.ByteString | Uint8Array | string): ByteString.ByteString =>
  typeof value === 'string'
    ? ByteString.fromString(value)
    : value instanceof Uint8Array
      ? ByteString.fromUint8Array(value)
      : value

/** @internal */
const aliaseeIndex = (
  builder: Builder.Builder,
  state: BuilderState.MutableState,
  owner: BuilderState.State['owner'],
  aliasee: Constant.Constant,
  operation: string,
): number => {
  const index = Handle.resolve(builder, owner, aliasee, 'Constant', operation)
  const constant = state.constants[index]
  const type = constant === undefined ? undefined : state.types[constant.type]
  if (type?._tag !== 'Pointer') {
    throw new SilkError({
      operation,
      message: 'An LLVM alias aliasee must have pointer type',
      cause: aliasee,
    })
  }
  return index
}

/** @internal */
const handleAt = (state: BuilderState.MutableState, index: number, operation: string): Alias => {
  const handle = state.aliasHandles[index]
  if (handle === undefined) {
    throw new SilkError({ operation, message: 'Alias table handle is missing', cause: index })
  }
  return handle
}

/** @internal */
const resolve = (
  builder: Builder.Builder,
  state: BuilderState.MutableState,
  owner: BuilderState.State['owner'],
  self: Alias,
  operation: string,
): { readonly index: number; readonly description: GlobalDescription.AliasDescription } => {
  const index = Handle.resolve(builder, owner, self, 'Alias', operation)
  const description = state.aliases[index]
  const global = description === undefined ? undefined : state.globals[description.global]
  if (
    description === undefined ||
    global?.kind !== 'Alias' ||
    global.actorIndex !== index ||
    global.deleted
  ) {
    throw new SilkError({ operation, message: 'Alias handle is no longer active', cause: self })
  }
  return { index, description }
}

/**
 * Creates a named LLVM alias to a pointer-typed constant.
 *
 * **Details**
 *
 * The alias occupies the same module-wide symbol table as variables and functions. `valueType`
 * describes the value visible through the alias, while `aliasee` must be a pointer constant owned
 * by the same builder. Creation is atomic: validation failures leave the symbol table unchanged.
 *
 * JavaScript names are encoded as UTF-8; byte strings preserve an exact global symbol name.
 *
 * **Gotchas**
 *
 * Name collisions, cross-builder handles, and non-pointer aliasees fail with {@link SilkError}.
 *
 * **Example** (Creating a global alias)
 *
 * Alias a global variable under a second symbol name.
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Alias from '@silk-effect/llvm/Alias'
 * import * as Builder from '@silk-effect/llvm/Builder'
 * import * as Constant from '@silk-effect/llvm/Constant'
 * import * as IrText from '@silk-effect/llvm/IrText'
 * import * as Type from '@silk-effect/llvm/Type'
 * import * as Variable from '@silk-effect/llvm/Variable'
 *
 * const text = await Effect.runPromise(Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const i32 = yield* Type.integer(builder, 32)
 *   const variable = yield* Variable.make(builder, 'answer', i32)
 *   const pointer = yield* Constant.fromGlobal(builder, yield* Variable.global(builder, variable))
 *   yield* Alias.make(builder, 'answer_alias', i32, pointer)
 *   return yield* IrText.render(builder)
 * }))
 * // text includes `@answer_alias = alias i32, ptr @answer`
 * ```
 *
 * @category aliases
 * @since 0.0.0
 */
export const make = Effect.fn('Alias.make')(function* (
  builder: Builder.Builder,
  name: ByteString.ByteString | Uint8Array | string,
  valueType: Type.Type,
  aliasee: Constant.Constant,
  options: Options = {},
): Effect.fn.Return<Alias, SilkError> {
  return yield* BuilderState.mutate(builder, 'Alias.make', (state, owner) => {
    const typeIndex = Handle.resolve(builder, owner, valueType, 'Type', 'Alias.make')
    const target = aliaseeIndex(builder, state, owner, aliasee, 'Alias.make')
    const index = state.aliases.length
    const allocated = GlobalState.allocate(
      state,
      owner,
      bytes(name),
      'Alias',
      index,
      options,
      'Alias.make',
    )
    const handle = Handle.make('Alias', owner, index)
    state.aliases.push(
      Object.freeze({
        _tag: 'Alias',
        global: allocated.index,
        valueType: typeIndex,
        aliasee: target,
      }),
    )
    state.aliasHandles.push(handle)
    return handle
  })
})

/**
 * Adopts an existing generic global as an alias, or returns its existing alias identity.
 *
 * @category aliases
 * @since 0.0.0
 */
export const fromGlobal = Effect.fn('Alias.fromGlobal')(function* (
  builder: Builder.Builder,
  global: Global.Global,
  valueType: Type.Type,
  aliasee: Constant.Constant,
): Effect.fn.Return<Alias, SilkError> {
  return yield* BuilderState.mutate(builder, 'Alias.fromGlobal', (state, owner) => {
    const resolved = GlobalState.resolve(builder, state, owner, global, 'Alias.fromGlobal')
    if (resolved.description.kind === 'Alias') {
      return handleAt(state, resolved.description.actorIndex, 'Alias.fromGlobal')
    }
    const index = state.aliases.length
    const handle = Handle.make('Alias', owner, index)
    state.aliases.push(
      Object.freeze({
        _tag: 'Alias',
        global: resolved.index,
        valueType: Handle.resolve(builder, owner, valueType, 'Type', 'Alias.fromGlobal'),
        aliasee: aliaseeIndex(builder, state, owner, aliasee, 'Alias.fromGlobal'),
      }),
    )
    state.aliasHandles.push(handle)
    state.globals[resolved.index] = Object.freeze({
      ...resolved.description,
      kind: 'Alias',
      actorIndex: index,
    })
    return handle
  })
})

/**
 * Replaces an alias target after pointer-type and ownership validation.
 *
 * @category aliases
 * @since 0.0.0
 */
export const setAliasee = Effect.fn('Alias.setAliasee')(function* (
  builder: Builder.Builder,
  self: Alias,
  aliasee: Constant.Constant,
): Effect.fn.Return<void, SilkError> {
  yield* BuilderState.mutate(builder, 'Alias.setAliasee', (state, owner) => {
    const { description, index } = resolve(builder, state, owner, self, 'Alias.setAliasee')
    state.aliases[index] = Object.freeze({
      ...description,
      aliasee: aliaseeIndex(builder, state, owner, aliasee, 'Alias.setAliasee'),
    })
  })
})

/**
 * Returns the shared global-symbol handle for an alias.
 *
 * @category aliases
 * @since 0.0.0
 */
export const global = Effect.fn('Alias.global')(function* (
  builder: Builder.Builder,
  self: Alias,
): Effect.fn.Return<Global.Global, SilkError> {
  return yield* BuilderState.mutate(builder, 'Alias.global', (state, owner) => {
    const value = resolve(builder, state, owner, self, 'Alias.global').description
    return GlobalState.handleAt(state, value.global, 'Alias.global')
  })
})

/**
 * Reads the current pointer-typed aliasee constant.
 *
 * @category aliases
 * @since 0.0.0
 */
export const aliasee = Effect.fn('Alias.aliasee')(function* (
  builder: Builder.Builder,
  self: Alias,
): Effect.fn.Return<Constant.Constant, SilkError> {
  return yield* BuilderState.mutate(builder, 'Alias.aliasee', (state, owner) => {
    const description = resolve(builder, state, owner, self, 'Alias.aliasee').description
    const constant = state.constantHandles[description.aliasee]
    if (constant === undefined) {
      throw new SilkError({
        operation: 'Alias.aliasee',
        message: 'Alias target constant is missing',
        cause: self,
      })
    }
    return constant
  })
})
