import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Builder from './Builder.js'
import * as Handle from './internal/Handle.js'
import * as LimitsCheck from './internal/LimitsCheck.js'
import * as ModuleState from './internal/ModuleState.js'
import * as NameCheck from './internal/NameCheck.js'
import type * as Limits from './Limits.js'
import { invalidInput, type WasmError } from './WasmError.js'

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
  /** Marks the memory shared for use with atomic operations. Requires a maximum size. */
  readonly shared?: boolean
  /** Address type used to index the memory. Defaults to `'i32'`. */
  readonly addressType?: 'i32' | 'i64'
}

/**
 * Declares a defined memory sized in 64 KiB pages. A module may declare several memories, and a
 * memory may be shared (for atomics) or 64-bit addressed.
 *
 * **Details**
 *
 * `limits` are page counts, not bytes: `{ min: 1 }` reserves 64 KiB. A 32-bit memory is capped
 * at 65536 pages (4 GiB); `addressType: 'i64'` raises that ceiling and changes the type of every
 * address operand and active data-segment offset for this memory to `i64`.
 *
 * **Gotchas**
 *
 * A shared memory must declare a maximum — it cannot grow past a bound other threads have already
 * observed — and is rejected here without one. Atomic instructions require a shared memory.
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
      const addressType = options.addressType ?? 'i32'
      const checked = yield* LimitsCheck.check(limits, 'memory', addressType, 'Memory.make')
      yield* NameCheck.ensureFresh(state.memories, options.name, 'Memory.make')
      const shared = options.shared ?? false
      if (shared && checked.max === undefined) {
        return yield* Result.fail(
          invalidInput({
            operation: 'Memory.make',
            message: 'A shared memory must declare a maximum size',
            input: limits,
          }),
        )
      }
      const index = state.memories.length
      state.memories.push({
        limits: checked,
        shared,
        addressType,
        name: options.name,
        importSource: undefined,
      })
      const handle: Memory = Handle.make('Memory', owner, index)
      state.memoryHandles.push(handle)
      return handle
    }),
  )
})
