import * as Effect from 'effect/Effect'
import type * as Builder from './Builder.js'
import * as IndexSpace from './internal/IndexSpace.js'
import * as ModuleCheck from './internal/ModuleCheck.js'
import * as ModuleState from './internal/ModuleState.js'
import * as TextRender from './internal/TextRender.js'
import type { WasmError } from './WasmError.js'

/**
 * Renders the builder's committed module state as WebAssembly text format.
 *
 * **When to use**
 *
 * Use when a human or a text-consuming tool needs to read the module — debugging a body the
 * validator accepted but you did not expect, diffing two builds, or producing `.wat` fixtures.
 * For anything that runs the module, encode it instead.
 *
 * **Details**
 *
 * The same module-level validation and index resolution as `Binary.encode` run first, so the
 * text and binary projections always describe the same module: parsing the rendered text with
 * the supported oracle produces bytes identical to `Binary.encode`'s output. Declared names
 * appear as `$`-identifiers when they fit the text format's identifier character set; entities
 * whose names do not fit are referenced by index while their exact names remain in the binary
 * `name` custom section.
 *
 * **Gotchas**
 *
 * Rendering is a projection of committed state, not a record of how it was built: interned types
 * appear once no matter how many times they were requested, and imports are numbered ahead of
 * definitions regardless of declaration order.
 *
 * **Example** (Rendering a module)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silk-lang/wasm/Builder'
 * import * as WatText from '@silk-lang/wasm/WatText'
 *
 * const text = Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   return yield* WatText.render(builder)
 * })
 * ```
 *
 * @category output
 * @since 0.0.0
 */
export const render = Effect.fn('WatText.render')(function* (
  builder: Builder.Builder,
): Effect.fn.Return<string, WasmError> {
  const snapshot = yield* ModuleState.snapshot(builder, 'WatText.render')
  yield* Effect.fromResult(ModuleCheck.check(snapshot, 'WatText.render'))
  const spaces = IndexSpace.resolve(snapshot)
  return yield* Effect.fromResult(TextRender.renderModule(snapshot, spaces, 'WatText.render'))
})
