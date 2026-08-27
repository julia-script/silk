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
 * **Details**
 *
 * The export name is what the host looks up; it is independent of the function's own
 * `options.name`, and either may be omitted or differ. Exporting also declares the function
 * referenceable, which satisfies `ref.func`'s requirement without a declarative element segment.
 *
 * **Gotchas**
 *
 * Export names must be unique across every kind: a function and a memory cannot share one name.
 * A duplicate is rejected here, at declaration.
 *
 * **Example** (Exporting under a different name)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silklang/wasm/Builder'
 * import * as Export from '@silklang/wasm/Export'
 * import * as Func from '@silklang/wasm/Func'
 * import * as Instr from '@silklang/wasm/Instr'
 * import * as Type from '@silklang/wasm/Type'
 * import * as ValType from '@silklang/wasm/ValType'
 *
 * const program = Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const signature = yield* Type.func(builder, [], [ValType.i32])
 *   const answer = yield* Func.declare(builder, signature, { name: 'answer' })
 *   yield* Func.define(builder, answer, { body: [Instr.i32Const(42)] })
 *   yield* Export.func(builder, 'getAnswer', answer)
 * })
 * ```
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
 * @see {@link func} for how export names relate to entity names and uniqueness.
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
 * @see {@link func} for how export names relate to entity names and uniqueness.
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
 * **Details**
 *
 * A mutable global may be exported; the host sees it as a writable `WebAssembly.Global`.
 *
 * @see {@link func} for how export names relate to entity names and uniqueness.
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
 * **Details**
 *
 * Exporting a tag lets another module import the same tag identity, which is what makes an
 * exception thrown in one module catchable by name in another.
 *
 * @see {@link func} for how export names relate to entity names and uniqueness.
 * @category exports
 * @since 0.0.0
 */
export const tag: (
  builder: Builder.Builder,
  name: string,
  handle: TagActor.Tag,
) => Effect.Effect<void, WasmError> = declare('Export.tag', 'tag', 'Tag')
