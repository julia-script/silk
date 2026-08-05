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
import type { WasmError } from './WasmError.js'

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
      yield* NameCheck.ensureFresh(state.datas, options.name, 'Data.active')
      yield* ConstExprCheck.check(state, owner, offset, ValType.i32, 'Data.active')
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
