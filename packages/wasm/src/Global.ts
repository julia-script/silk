import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Builder from './Builder.js'
import type * as ConstExpr from './ConstExpr.js'
import * as ConstExprCheck from './internal/ConstExprCheck.js'
import * as Handle from './internal/Handle.js'
import * as ModuleState from './internal/ModuleState.js'
import * as NameCheck from './internal/NameCheck.js'
import type * as ValType from './ValType.js'
import type { WasmError } from './WasmError.js'

/**
 * Opaque builder-owned identity for an imported or defined global.
 *
 * @category globals
 * @since 0.0.0
 */
export interface Global extends Handle.Handle<'Global'> {}

/**
 * Options accepted by global declarations.
 *
 * @category globals
 * @since 0.0.0
 */
export interface Options {
  /** Name used for text identifiers and the binary `name` custom section. */
  readonly name?: string
}

/**
 * Declares a defined global with a constant initializer expression.
 *
 * **Details**
 *
 * The initializer is validated against the specification's constant expression rules when the
 * global is declared: constants, `ref.null`, `ref.func`, reads of imported immutable globals,
 * and extended integer arithmetic, producing exactly one value of the global's type.
 *
 * **Example** (A mutable counter starting at zero)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silk-effect/wasm/Builder'
 * import * as Global from '@silk-effect/wasm/Global'
 * import * as Instr from '@silk-effect/wasm/Instr'
 * import * as ValType from '@silk-effect/wasm/ValType'
 *
 * const program = Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   return yield* Global.make(builder, ValType.i32, true, [Instr.i32Const(0)])
 * })
 * ```
 *
 * @category globals
 * @since 0.0.0
 */
export const make = Effect.fn('Global.make')(function* (
  builder: Builder.Builder,
  valType: ValType.ValType,
  mutable: boolean,
  init: ConstExpr.ConstExpr,
  options: Options = {},
): Effect.fn.Return<Global, WasmError> {
  return yield* ModuleState.mutate(builder, 'Global.make', (state, owner) =>
    Result.gen(function* () {
      yield* ConstExprCheck.check(state, owner, init, valType, 'Global.make')
      yield* NameCheck.ensureFresh(state.globals, options.name, 'Global.make')
      const index = state.globals.length
      state.globals.push({
        valType,
        mutable,
        name: options.name,
        importSource: undefined,
        init: Object.freeze([...init]),
      })
      const handle: Global = Handle.make('Global', owner, index)
      state.globalHandles.push(handle)
      return handle
    }),
  )
})
