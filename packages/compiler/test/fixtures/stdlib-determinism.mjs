import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Tir from '../../dist/Tir.js'
import * as MirEncoding from '../../dist/MirEncoding.js'

const source = `import silk.vector { Vector }

pub fn main() -> i32 {
  return 42
}`
const bytes = new TextEncoder().encode(source)
const snapshot = await Effect.runPromise(
  Analysis.ofSourceRealized('fixture/stdlib-determinism', bytes, 'wasm32-unknown-unknown'),
)

process.stdout.write(
  JSON.stringify({
    diagnostics: Analysis.diagnostics(snapshot),
    modules: Analysis.modules(snapshot).map((module) => module.name),
    libraryTir: Tir.encode(snapshot.results.get('silk/vector')?.tir),
    mir: MirEncoding.encode(Analysis.loweredMir(snapshot)),
  }),
)
