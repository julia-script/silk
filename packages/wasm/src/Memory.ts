import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Builder from './Builder.js'
import * as Handle from './internal/Handle.js'
import * as LimitsCheck from './internal/LimitsCheck.js'
import * as ModuleState from './internal/ModuleState.js'
import * as NameCheck from './internal/NameCheck.js'
import type * as Limits from './Limits.js'
import type { WasmError } from './WasmError.js'

/**
 * Opaque builder-owned identity for an imported or defined memory.
 *
 * @category memories
 * @since 0.0.0
 */
export interface Memory extends Handle.Handle<'Memory'> {}

/**
 * Options accepted by memory declarations.
 *
 * @category memories
 * @since 0.0.0
 */
export interface Options {
  /** Name used for text identifiers and the binary `name` custom section. */
  readonly name?: string
}

/** The maximum number of 64 KiB pages addressable by a 32-bit memory. */
const maxPages = 65536

/**
 * Declares a defined memory sized in 64 KiB pages. A module may declare several memories.
 *
 * @category memories
 * @since 0.0.0
 */
export const make = Effect.fn('Memory.make')(function* (
  builder: Builder.Builder,
  limits: Limits.Limits,
  options: Options = {},
): Effect.fn.Return<Memory, WasmError> {
  return yield* ModuleState.mutate(builder, 'Memory.make', (state, owner) =>
    Result.gen(function* () {
      const checked = yield* LimitsCheck.check(limits, maxPages, 'Memory.make')
      yield* NameCheck.ensureFresh(state.memories, options.name, 'Memory.make')
      const index = state.memories.length
      state.memories.push({
        limits: checked,
        name: options.name,
        importSource: undefined,
      })
      const handle: Memory = Handle.make('Memory', owner, index)
      state.memoryHandles.push(handle)
      return handle
    }),
  )
})
