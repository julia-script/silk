/**
 * Module exports.
 *
 * Export names are UTF-8 strings that must be unique across all exports of a module; a duplicate
 * name is rejected when it is declared.
 *
 * @since 0.0.0
 */
import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Builder from './Builder.js'
import type * as FuncActor from './Func.js'
import type * as GlobalActor from './Global.js'
import * as Handle from './internal/Handle.js'
import * as ModuleState from './internal/ModuleState.js'
import type * as MemoryActor from './Memory.js'
import type * as TableActor from './Table.js'
import type * as TagActor from './Tag.js'
import { invalidInput, type WasmError } from './WasmError.js'

/** @internal */
const declare = (
  operation: string,
  kind: ModuleState.ExportKind,
  tag: 'Func' | 'Table' | 'Memory' | 'Global' | 'Tag',
) =>
  Effect.fn(operation)(function* (
    builder: Builder.Builder,
    name: string,
    handle: Handle.Handle<typeof tag>,
  ): Effect.fn.Return<void, WasmError> {
    yield* ModuleState.mutate(builder, operation, (state, owner) =>
      Result.gen(function* () {
        const entryIndex = yield* Handle.resolve(owner, handle, tag, operation)
        if (state.exportNames.has(name)) {
          return yield* Result.fail(
            invalidInput({
              operation,
              message: `The module already exports the name ${JSON.stringify(name)}`,
              input: name,
            }),
          )
        }
        state.exportNames.add(name)
        state.exports.push({ name, kind, entryIndex })
      }),
    )
  })

/**
 * Exports a function under a unique name.
 *
 * @category exports
 * @since 0.0.0
 */
export const func: (
  builder: Builder.Builder,
  name: string,
  handle: FuncActor.Func,
) => Effect.Effect<void, WasmError> = declare('Export.func', 'func', 'Func')

/**
 * Exports a table under a unique name.
 *
 * @category exports
 * @since 0.0.0
 */
export const table: (
  builder: Builder.Builder,
  name: string,
  handle: TableActor.Table,
) => Effect.Effect<void, WasmError> = declare('Export.table', 'table', 'Table')

/**
 * Exports a memory under a unique name.
 *
 * @category exports
 * @since 0.0.0
 */
export const memory: (
  builder: Builder.Builder,
  name: string,
  handle: MemoryActor.Memory,
) => Effect.Effect<void, WasmError> = declare('Export.memory', 'memory', 'Memory')

/**
 * Exports a global under a unique name.
 *
 * @category exports
 * @since 0.0.0
 */
export const global: (
  builder: Builder.Builder,
  name: string,
  handle: GlobalActor.Global,
) => Effect.Effect<void, WasmError> = declare('Export.global', 'global', 'Global')

/**
 * Exports an exception tag under a unique name.
 *
 * @category exports
 * @since 0.0.0
 */
export const tag: (
  builder: Builder.Builder,
  name: string,
  handle: TagActor.Tag,
) => Effect.Effect<void, WasmError> = declare('Export.tag', 'tag', 'Tag')
