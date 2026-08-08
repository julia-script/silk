import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Hir from '../../dist/Hir.js'
import * as Mir from '../../dist/Mir.js'

const source = `import silk.vector { Vector, length }

pub fn main() -> I32 {
  return 42
}`
const bytes = new TextEncoder().encode(source)
const snapshot = await Effect.runPromise(
  Analysis.ofSource('fixture/stdlib-determinism', bytes, 'wasm32-unknown-unknown'),
)

process.stdout.write(
  JSON.stringify({
    diagnostics: Analysis.diagnostics(snapshot),
    modules: Analysis.modules(snapshot).map((module) => module.name),
    libraryHir: Hir.encode(Analysis.hirOf(snapshot, 'silk/vector')),
    mir: Mir.encode(Analysis.loweredMir(snapshot)),
  }),
)
