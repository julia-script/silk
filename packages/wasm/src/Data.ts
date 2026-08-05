/**
 * Data segments.
 *
 * Active segments copy their bytes into a memory at instantiation; passive segments feed
 * `memory.init`. Bytes are copied on declaration and preserved exactly in emitted output.
 *
 * @since 0.0.0
 */
import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Builder from './Builder.js'
import type * as ConstExpr from './ConstExpr.js'
import * as ConstExprCheck from './internal/ConstExprCheck.js'
import * as Handle from './internal/Handle.js'
import * as ModuleState from './internal/ModuleState.js'
import * as NameCheck from './internal/NameCheck.js'
import type * as Memory from './Memory.js'
import * as ValType from './ValType.js'
import { invalidState, type WasmError } from './WasmError.js'

/**
 * Opaque builder-owned identity for a data segment.
 *
 * @category data segments
 * @since 0.0.0
 */
export interface Data extends Handle.Handle<'Data'> {}

/**
 * Options accepted by data segment declarations.
 *
 * @category data segments
 * @since 0.0.0
 */
export interface Options {
  /** Name used for text identifiers and the binary `name` custom section. */
  readonly name?: string
}

/**
 * Declares an active data segment copied into a memory at instantiation.
 *
 * **When to use**
 *
 * Use for static initial memory contents — string literals, lookup tables, a preloaded heap.
 *
 * **Details**
 *
 * The offset is a constant expression producing an `i32`, or an `i64` when the memory is 64-bit
 * addressed. Bytes are copied when the segment is declared, so mutating the caller's array
 * afterwards does not affect the module.
 *
 * **Gotchas**
 *
 * The offset is not bounds-checked at declaration: a segment that overruns the memory's size
 * fails at instantiation, in the host, not here.
 *
 * @see {@link passive} for bytes applied later by `memory.init`.
 * @category data segments
 * @since 0.0.0
 */
export const active = Effect.fn('Data.active')(function* (
  builder: Builder.Builder,
  memory: Memory.Memory,
  offset: ConstExpr.ConstExpr,
  bytes: Uint8Array,
  options: Options = {},
): Effect.fn.Return<Data, WasmError> {
  return yield* ModuleState.mutate(builder, 'Data.active', (state, owner) =>
    Result.gen(function* () {
      const memoryIndex = yield* Handle.resolve(owner, memory, 'Memory', 'Data.active')
      const memoryEntry = state.memories[memoryIndex]
      if (memoryEntry === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'Data.active',
            message: 'Memory entry is missing',
            state: memoryIndex,
          }),
        )
      }
      yield* NameCheck.ensureFresh(state.datas, options.name, 'Data.active')
      const offsetType = memoryEntry.addressType === 'i64' ? ValType.i64 : ValType.i32
      yield* ConstExprCheck.check(state, owner, offset, offsetType, 'Data.active')
      const index = state.datas.length
      state.datas.push({
        mode: { _tag: 'Active', memory: memoryIndex, offset: Object.freeze([...offset]) },
        bytes: new Uint8Array(bytes),
        name: options.name,
      })
      const handle: Data = Handle.make('Data', owner, index)
      state.dataHandles.push(handle)
      return handle
    }),
  )
})

/**
 * Declares a passive data segment for later use with `memory.init`.
 *
 * **When to use**
 *
 * Use when bytes are copied into memory at a moment the module chooses rather than at
 * instantiation, or copied more than once, or to a computed destination.
 *
 * **Details**
 *
 * The segment names no memory and no offset; `memory.init` supplies both. It stays available
 * until `memory.drop`, after which `memory.init` from it traps. Using either instruction makes
 * the emitted module carry a data-count section, which the builder adds automatically.
 *
 * @see {@link active} for bytes copied automatically at instantiation.
 * @category data segments
 * @since 0.0.0
 */
export const passive = Effect.fn('Data.passive')(function* (
  builder: Builder.Builder,
  bytes: Uint8Array,
  options: Options = {},
): Effect.fn.Return<Data, WasmError> {
  return yield* ModuleState.mutate(builder, 'Data.passive', (state, owner) =>
    Result.gen(function* () {
      yield* NameCheck.ensureFresh(state.datas, options.name, 'Data.passive')
      const index = state.datas.length
      state.datas.push({
        mode: { _tag: 'Passive' },
        bytes: new Uint8Array(bytes),
        name: options.name,
      })
      const handle: Data = Handle.make('Data', owner, index)
      state.dataHandles.push(handle)
      return handle
    }),
  )
})
