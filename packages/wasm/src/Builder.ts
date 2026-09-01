import * as Effect from 'effect/Effect'
import * as ModuleState from './internal/ModuleState.js'
import * as OwnedHandle from './internal/OwnedHandle.js'

/**
 * An opaque, concurrency-safe owner of WebAssembly module state.
 *
 * **Details**
 *
 * A `Builder` is an identity, not a container: it carries no inspectable fields, and the module
 * state it owns lives inside the package. Every declaration takes the builder as its first
 * argument and returns an opaque handle, and the handles of one builder are rejected by any
 * other. Nothing is read back except through the actor `name`/`type`/`signature` accessors and
 * the two emitters.
 *
 * @see {@link make} for the constructor.
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
  /** Module name recorded in the binary `name` custom section and text output. */
  readonly moduleName?: string
}

/**
 * Creates the isolated state owner for one WebAssembly module.
 *
 * **When to use**
 *
 * Use once per module you intend to emit. Building two modules concurrently means two builders;
 * they share no state and their handles are not interchangeable.
 *
 * **Details**
 *
 * Every type, import, function, table, memory, global, segment, and export declared from the
 * builder is owner-checked. Mutations pass through one Effect semaphore, so concurrent fibers
 * cannot lose committed declarations. Numeric indices do not exist on the public surface; they
 * are computed from the builder's state when a module is emitted.
 *
 * **Gotchas**
 *
 * Declarations mutate the builder's state in place rather than returning a new builder, so a
 * builder value cannot be forked or reused as a template for a second module. The builder holds
 * no external resource and needs no scope or teardown.
 *
 * **Example** (Creating a module)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silklang/wasm/Builder'
 * import * as WatText from '@silklang/wasm/WatText'
 *
 * const text = Effect.gen(function* () {
 *   const builder = yield* Builder.make({ moduleName: 'demo' })
 *   return yield* WatText.render(builder)
 * })
 * ```
 *
 * @category builders
 * @since 0.0.0
 */
export const make = Effect.fnUntraced(function* (options: Options = {}) {
  const self: Builder = Object.freeze({ _tag: 'Builder' })
  ModuleState.register(self, {
    owner: OwnedHandle.makeOwner(),
    value: {
      moduleName: options.moduleName,
      types: [],
      typeHandles: [],
      typeKeys: new Map(),
      recGroups: [],
      funcs: [],
      funcHandles: [],
      tables: [],
      tableHandles: [],
      memories: [],
      memoryHandles: [],
      globals: [],
      globalHandles: [],
      tags: [],
      tagHandles: [],
      elems: [],
      elemHandles: [],
      datas: [],
      dataHandles: [],
      exports: [],
      exportNameKeys: new Set(),
      start: undefined,
    },
  })
  return self
})
