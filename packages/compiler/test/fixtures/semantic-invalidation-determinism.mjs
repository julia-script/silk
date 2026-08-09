import * as Effect from 'effect/Effect'
import * as ProjectAnalysis from '../../dist/ProjectAnalysis.js'
import * as SourceFile from '../../dist/SourceFile.js'
import * as SourceResolver from '../../dist/SourceResolver.js'

const encoder = new TextEncoder()
const rootSource =
  'import deterministic.Dependency { answer } pub fn main() -> i32 { return answer() }'
const previousSources = new Map([
  ['deterministic/Dependency', encoder.encode('pub fn answer() -> i32 { return 1 }')],
])
const previous = Effect.runSync(
  ProjectAnalysis.make([SourceFile.make('deterministic/Main', encoder.encode(rootSource))]).pipe(
    Effect.provide(SourceResolver.memory(previousSources)),
  ),
)
const currentSources = new Map([
  ['deterministic/Dependency', encoder.encode('pub fn answer(value: i32) -> i32 { return value }')],
])
const current = Effect.runSync(
  ProjectAnalysis.revise(previous, [
    SourceFile.make('deterministic/Main', encoder.encode(rootSource)),
  ]).pipe(Effect.provide(SourceResolver.memory(currentSources))),
)
const report = current.report.map(({ phase, inputs, outputs, diagnostics, counters }) => ({
  phase,
  inputs,
  outputs,
  diagnostics,
  ...(counters === undefined ? {} : { counters }),
}))

process.stdout.write(
  JSON.stringify({
    surfaces: [...current.surfaces],
    invalidation: current.semanticInvalidation,
    report,
  }),
)
