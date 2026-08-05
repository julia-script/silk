import * as Effect from 'effect/Effect'
import * as Semaphore from 'effect/Semaphore'
import * as ModuleState from './internal/ModuleState.js'
import * as OwnedHandle from './internal/OwnedHandle.js'

/**
 * An opaque, concurrency-safe owner of WebAssembly module state.
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
  /** Module name recorded in the binary `name` custom section and text output. */
  readonly moduleName?: string
}

/**
 * Creates the isolated state owner for one WebAssembly module.
 *
 * **Details**
 *
 * Every type, import, function, table, memory, global, segment, and export declared from the
 * builder is owner-checked. Mutations pass through one Effect semaphore, so concurrent fibers
 * cannot lose committed declarations. Numeric indices do not exist on the public surface; they
 * are computed from the builder's state when a module is emitted.
 *
 * **Example** (Creating a module)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silk-effect/wasm/Builder'
 * import * as WatText from '@silk-effect/wasm/WatText'
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
export const make = Effect.fn('Builder.make')(function* (options: Options = {}) {
  const gate = yield* Semaphore.make(1)
  const self: Builder = Object.freeze({ _tag: 'Builder' })
  ModuleState.register(self, {
    owner: OwnedHandle.makeOwner(),
    gate,
    value: {
      moduleName: options.moduleName,
      types: [],
      typeHandles: [],
      typeKeys: new Map(),
      funcs: [],
      funcHandles: [],
      tables: [],
      tableHandles: [],
      memories: [],
      memoryHandles: [],
      globals: [],
      globalHandles: [],
      elems: [],
      elemHandles: [],
      datas: [],
      dataHandles: [],
      exports: [],
      exportNames: new Set(),
      start: undefined,
    },
  })
  return self
})
