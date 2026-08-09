import * as Effect from 'effect/Effect'
import * as ProjectAnalysis from '../../dist/ProjectAnalysis.js'
import * as SourceFile from '../../dist/SourceFile.js'
import * as SourceResolver from '../../dist/SourceResolver.js'

const encoder = new TextEncoder()
const sources = new Map([
  [
    'surface/Model',
    encoder.encode(`pub struct Pair { pub left: i32 pub right: i32 }
pub const enabled: bool = true`),
  ],
])
const project = Effect.runSync(
  ProjectAnalysis.make([
    SourceFile.make(
      'surface/Main',
      encoder.encode(
        'import surface.Model { Pair, enabled } pub fn answer(value: Pair) -> bool { return enabled }',
      ),
    ),
  ]).pipe(Effect.provide(SourceResolver.memory(sources))),
)

process.stdout.write(
  JSON.stringify([...project.views.values()][0]?.surfaces ?? [], (_key, value) =>
    value instanceof Map ? [...value] : value,
  ),
)
