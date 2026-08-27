import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Builder from './Builder.js'
import type * as Instr from './Instr.js'
import * as Handle from './internal/Handle.js'
import * as ModuleState from './internal/ModuleState.js'
import * as NameCheck from './internal/NameCheck.js'
import * as Validator from './internal/Validator.js'
import type * as Type from './Type.js'
import type * as ValType from './ValType.js'
import { invalidState, type WasmError } from './WasmError.js'

/**
 * Opaque builder-owned identity for an imported or defined function.
 *
 * @category functions
 * @since 0.0.0
 */
export interface Func extends Handle.Handle<'Func'> {}

/**
 * Options accepted by function declarations.
 *
 * @category functions
 * @since 0.0.0
 */
export interface DeclareOptions {
  /** Name used for text identifiers and the binary `name` custom section. */
  readonly name?: string
}

/**
 * Declares a defined function with an interned function type.
 *
 * **When to use**
 *
 * Use to reserve a function's identity before its body exists. Because the handle is valid
 * immediately, mutually recursive functions need no special ordering: declare both, then define
 * both.
 *
 * **Details**
 *
 * Declaration returns a handle immediately usable in calls, exports, and element segments; the
 * body is supplied later with `Func.define`. Emission fails if a declared function was never
 * defined.
 *
 * **Gotchas**
 *
 * A declared function is not yet a complete function. Nothing rejects the omission until
 * `Binary.encode` or `WatText.render` runs, so an undefined declaration surfaces as a
 * whole-module failure at emission rather than at the point of the mistake.
 *
 * @see {@link define} for supplying the body.
 * @category functions
 * @since 0.0.0
 */
export const declare = Effect.fn('Func.declare')(function* (
  builder: Builder.Builder,
  type: Type.Type,
  options: DeclareOptions = {},
): Effect.fn.Return<Func, WasmError> {
  return yield* ModuleState.mutate(builder, 'Func.declare', (state, owner) =>
    Result.gen(function* () {
      const typeIndex = yield* Handle.resolve(owner, type, 'Type', 'Func.declare')
      if (ModuleState.funcTypeAt(state.types, typeIndex) === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'Func.declare',
            message: 'A function must be declared with a function type',
            state: typeIndex,
          }),
        )
      }
      yield* NameCheck.ensureFresh(state.funcs, options.name, 'Func.declare')
      const index = state.funcs.length
      state.funcs.push({
        typeIndex,
        name: options.name,
        importSource: undefined,
        definition: undefined,
      })
      const handle: Func = Handle.make('Func', owner, index)
      state.funcHandles.push(handle)
      return handle
    }),
  )
})

/**
 * One local variable declaration inside a function definition.
 *
 * @category functions
 * @since 0.0.0
 */
export interface Local {
  readonly type: ValType.ValType
  /** Name used for text identifiers and the binary `name` custom section. */
  readonly name?: string
}

/**
 * The locals and body of a function definition.
 *
 * @category functions
 * @since 0.0.0
 */
export interface Definition {
  readonly locals?: ReadonlyArray<Local>
  readonly body: ReadonlyArray<Instr.Instr>
}

/**
 * Supplies the locals and body of a declared function, validating the body against the
 * WebAssembly specification before anything is committed.
 *
 * **When to use**
 *
 * Use once per declared function. This is where body errors are reported, so it is the point at
 * which a malformed instruction sequence is worth debugging — the emitters assume bodies already
 * passed here and never re-check them.
 *
 * **Details**
 *
 * Validation runs the specification's algorithm — value-stack typing, control-frame tracking,
 * branch arities, entity reference checks, and polymorphic typing after unreachable code. A
 * rejected body fails with `WasmError` and leaves the function undefined, so a corrected
 * definition can be retried without any artifact of the failed attempt.
 *
 * **Gotchas**
 *
 * Defining the same function twice fails rather than replacing the first body, and an imported
 * function cannot be defined at all. Locals must be defaultable, so a non-nullable reference
 * local is rejected; declare it nullable, or keep the value on the stack.
 *
 * **Example** (Defining `add`)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silk-lang/wasm/Builder'
 * import * as Func from '@silk-lang/wasm/Func'
 * import * as Instr from '@silk-lang/wasm/Instr'
 * import * as Type from '@silk-lang/wasm/Type'
 * import * as ValType from '@silk-lang/wasm/ValType'
 *
 * const program = Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const signature = yield* Type.func(builder, [ValType.i32, ValType.i32], [ValType.i32])
 *   const add = yield* Func.declare(builder, signature, { name: 'add' })
 *   yield* Func.define(builder, add, {
 *     body: [Instr.localGet(0), Instr.localGet(1), Instr.op('i32.add')],
 *   })
 * })
 * ```
 *
 * @category functions
 * @since 0.0.0
 */
export const define = Effect.fn('Func.define')(function* (
  builder: Builder.Builder,
  func: Func,
  definition: Definition,
): Effect.fn.Return<void, WasmError> {
  yield* ModuleState.mutate(builder, 'Func.define', (state, owner) =>
    Result.gen(function* () {
      const index = yield* Handle.resolve(owner, func, 'Func', 'Func.define')
      const entry = state.funcs[index]
      const funcType =
        entry === undefined ? undefined : ModuleState.funcTypeAt(state.types, entry.typeIndex)
      if (entry === undefined || funcType === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'Func.define',
            message: 'Function table entry is missing',
            state: index,
          }),
        )
      }
      if (entry.importSource !== undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'Func.define',
            message: 'An imported function cannot be defined',
            state: entry.importSource,
          }),
        )
      }
      if (entry.definition !== undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'Func.define',
            message: 'The function is already defined',
            state: index,
          }),
        )
      }
      const locals = Object.freeze(
        (definition.locals ?? []).map((local) =>
          Object.freeze({ type: local.type, name: local.name }),
        ),
      )
      for (const [localIndex, local] of locals.entries()) {
        yield* NameCheck.ensureFresh(locals.slice(0, localIndex), local.name, 'Func.define')
      }
      const checked = yield* Validator.checkBody(
        state,
        owner,
        funcType,
        locals,
        definition.body,
        'Func.define',
      )
      entry.definition = {
        locals,
        body: Object.freeze([...definition.body]),
        refFuncs: checked.refFuncs,
        usesDataCount: checked.usesDataCount,
      }
    }),
  )
})

/**
 * Reads back the interned function type of a declared or imported function.
 *
 * @category functions
 * @since 0.0.0
 */
export const type = Effect.fn('Func.type')(function* (
  builder: Builder.Builder,
  func: Func,
): Effect.fn.Return<Type.Type, WasmError> {
  return yield* ModuleState.mutate(builder, 'Func.type', (state, owner) =>
    Result.gen(function* () {
      const index = yield* Handle.resolve(owner, func, 'Func', 'Func.type')
      const entry = state.funcs[index]
      const handle = entry === undefined ? undefined : state.typeHandles[entry.typeIndex]
      if (handle === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'Func.type',
            message: 'Function table entry is missing',
            state: index,
          }),
        )
      }
      return handle
    }),
  )
})

/**
 * Reads back the optional name recorded for a function.
 *
 * @category functions
 * @since 0.0.0
 */
export const name = Effect.fn('Func.name')(function* (
  builder: Builder.Builder,
  func: Func,
): Effect.fn.Return<string | undefined, WasmError> {
  return yield* ModuleState.mutate(builder, 'Func.name', (state, owner) =>
    Result.gen(function* () {
      const index = yield* Handle.resolve(owner, func, 'Func', 'Func.name')
      const entry = state.funcs[index]
      if (entry === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'Func.name',
            message: 'Function table entry is missing',
            state: index,
          }),
        )
      }
      return entry.name
    }),
  )
})

/**
 * Designates the module's start function, which must have the type `[] -> []`.
 *
 * **Details**
 *
 * The start function runs during instantiation, after the module's globals and segments are
 * initialized and before any export is callable.
 *
 * **Gotchas**
 *
 * A module has at most one start function: a second call fails rather than replacing the first.
 * Both the arity requirement and the single-designation rule are enforced here, not at emission.
 *
 * @category functions
 * @since 0.0.0
 */
export const start = Effect.fn('Func.start')(function* (
  builder: Builder.Builder,
  func: Func,
): Effect.fn.Return<void, WasmError> {
  yield* ModuleState.mutate(builder, 'Func.start', (state, owner) =>
    Result.gen(function* () {
      const index = yield* Handle.resolve(owner, func, 'Func', 'Func.start')
      const entry = state.funcs[index]
      const funcType =
        entry === undefined ? undefined : ModuleState.funcTypeAt(state.types, entry.typeIndex)
      if (funcType === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'Func.start',
            message: 'Function table entry is missing',
            state: index,
          }),
        )
      }
      if (funcType.params.length > 0 || funcType.results.length > 0) {
        return yield* Result.fail(
          invalidState({
            operation: 'Func.start',
            message: 'The start function must have the type [] -> []',
            state: {
              params: funcType.params.length,
              results: funcType.results.length,
            },
          }),
        )
      }
      if (state.start !== undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'Func.start',
            message: 'The module already has a start function',
            state: state.start,
          }),
        )
      }
      state.start = index
    }),
  )
})
