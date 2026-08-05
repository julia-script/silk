import * as Effect from 'effect/Effect'
import type * as Builder from './Builder.js'
import * as BinaryEncode from './internal/BinaryEncode.js'
import * as IndexSpace from './internal/IndexSpace.js'
import * as ModuleCheck from './internal/ModuleCheck.js'
import * as ModuleState from './internal/ModuleState.js'
import type { WasmError } from './WasmError.js'

/**
 * Encodes the builder's committed module state as a binary `.wasm` module.
 *
 * **Details**
 *
 * Emission first validates module-level constraints that are only decidable over the whole
 * module (every declared function is defined; every `ref.func` inside a body targets a function
 * declared in an element segment, global initializer, or export), then resolves all index
 * spaces — imported entities first, definitions after, both in declaration order — and encodes
 * the canonical section sequence. Declared names are carried in the binary `name` custom
 * section. Output is deterministic: the same committed operation order produces identical bytes.
 *
 * **Example** (Encoding a module)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Binary from '@silk-effect/wasm/Binary'
 * import * as Builder from '@silk-effect/wasm/Builder'
 *
 * const bytes = Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   return yield* Binary.encode(builder)
 * })
 * ```
 *
 * @category output
 * @since 0.0.0
 */
export const encode = Effect.fn('Binary.encode')(function* (
  builder: Builder.Builder,
): Effect.fn.Return<Uint8Array, WasmError> {
  const snapshot = yield* ModuleState.snapshot(builder, 'Binary.encode')
  yield* Effect.fromResult(ModuleCheck.check(snapshot, 'Binary.encode'))
  const spaces = IndexSpace.resolve(snapshot)
  return yield* Effect.fromResult(BinaryEncode.encodeModule(snapshot, spaces, 'Binary.encode'))
})
