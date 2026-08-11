import { createHash } from 'node:crypto'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Hir from '../../dist/Hir.js'
import * as Mir from '../../dist/Mir.js'

const source = `import silk.logging { length, messageByteAt }
effect fn program() -> i32 ! LogError {
  let mut logger = InMemoryLogger.memory()
  let first = run Effect.provideMut(Effect.log("alpha"), &mut logger)
  let second = run Effect.provideMut(Effect.logAt(LogLevel.error(), "beta"), &mut logger)
  if length(&logger) != 2 { return 0 }
  return u8.toI32(messageByteAt(&logger, 1, 0))
}
effect fn recover(error: LogError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catch(program(), recover) }`
const bytes = new TextEncoder().encode(source)
const snapshot = (name, target) => Effect.runPromise(Analysis.ofSourceRealized(name, bytes, target))
const native = await snapshot('logging/determinism-native', 'aarch64-apple-darwin')
const wasm = await snapshot('logging/determinism-wasm', 'wasm32-unknown-unknown')
const nativeArtifact = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))
const wasmArtifact = await Effect.runPromise(Analysis.codegenWasm(wasm, { mode: 'release' }))
const json = (value) =>
  JSON.stringify(value, (_key, candidate) =>
    typeof candidate === 'bigint' ? candidate.toString() : candidate,
  )

process.stdout.write(
  json({
    diagnostics: Analysis.diagnostics(native),
    modules: Analysis.modules(native).map((module) => module.name),
    hir: Hir.encode(Analysis.hirOf(native, 'silk/effects')),
    nativeMir: Mir.encode(Analysis.loweredMir(native)),
    wasmMir: Mir.encode(Analysis.loweredMir(wasm)),
    nativeOutcome: Analysis.evaluate(native),
    wasmOutcome: Analysis.evaluate(wasm),
    native: createHash('sha256').update(nativeArtifact.bitcode).digest('hex'),
    wasm: createHash('sha256').update(wasmArtifact.bytes).digest('hex'),
    hostImports: wasmArtifact.hostImports,
  }),
)
