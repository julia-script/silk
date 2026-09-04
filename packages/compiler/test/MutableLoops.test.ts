import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Projections from './support/projections.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (source: string, target = 'wasm32-unknown-unknown') =>
  Analysis.ofSourceRealized('mutable-loops/main', ascii(source), target)

it.effect('diagnoses immutable writes and transfers outside loops', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub fn main() -> i32 {
  let value = 1
  value = 2
  break
  return value
}`)
    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['SEM0035', 'SEM0038'],
    )
  }),
)

it.effect('keeps assignment and equality tokens distinct beside mutable loops', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      'pub fn main() -> i32 { let mut value = 0 while value == 0 { value = 42 } return value }',
    )
    const kinds = Analysis.rootAnalysis(self).syntax.tokens.map((token) => token.kind)
    assert.strictEqual(kinds.filter((kind) => kind === 'Equals').length, 2)
    assert.strictEqual(kinds.filter((kind) => kind === 'EqualEqual').length, 1)
    assert.deepEqual(Analysis.diagnostics(self), [])
  }),
)

it.effect('recovers damaged mutable statements without losing following declarations', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub fn damaged() -> i32 {
  let mut value = 0
  value = @
  return value
}
pub fn main() -> i32 { return 42 }`)
    const analysis = Analysis.rootAnalysis(self)
    assert.isAbove(analysis.syntax.parserDiagnostics.length, 0)
    assert.strictEqual(analysis.functions.length, 2)
    const name = analysis.functions.at(1)?.declaration.name
    assert.strictEqual(name?._tag, 'Present')
    if (name?._tag === 'Present') assert.strictEqual(name.spelling, 'main')
  }),
)

it.effect('publishes immutable facade facts for writes, loops, transfers, and DAG edges', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub fn main() -> i32 {
  let mut value = 0
  while value < 2 { value = value + 1 if value == 1 { continue } }
  return value
}`)
    const bindings = Projections.bindingsOf(self, 'mutable-loops/main')
    const writes = Projections.writesOf(self, 'mutable-loops/main')
    const loops = Projections.loopsOf(self, 'mutable-loops/main')
    const transfers = Projections.transfersOf(self, 'mutable-loops/main')
    const regions = Projections.controlRegionsOf(self)
    const edges = Projections.controlEdgesOf(self)
    const fixedPoints = Projections.ownershipFixedPointsOf(self, 'mutable-loops/main')

    assert.strictEqual(bindings.at(0)?.mutability, 'Mutable')
    assert.strictEqual(writes.length, 1)
    assert.strictEqual(loops.length, 1)
    assert.strictEqual(transfers.at(0)?._tag, 'ContinueStatement')
    assert.isAbove(regions.length, 0)
    assert.isAbove(edges.length, 0)
    assert.strictEqual(fixedPoints.at(0)?.compatible, true)
    assert.strictEqual(Object.isFrozen(regions), true)
    assert.strictEqual(Object.isFrozen(edges), true)
  }),
)

it.effect('rejects incompatible repeating owner states and overlapping replacement', () =>
  Effect.gen(function* () {
    const incompatible = yield* snapshot(`struct Token { value: i32 }
pub fn main() -> i32 {
  let mut token = Token { value: 1 }
  let mut iteration = 0
  while iteration < 1 {
    if iteration == 0 { let old = move token continue }
    iteration = iteration + 1
  }
  return 42
}`)
    assert.include(
      Analysis.diagnostics(incompatible).map((diagnostic) => diagnostic.code),
      'OWN0005',
    )

    const overlapping = yield* snapshot(`struct Token { value: i32 }
pub fn main() -> i32 {
  let mut token = Token { value: 1 }
  token = move token
  return 42
}`)
    assert.include(
      Analysis.diagnostics(overlapping).map((diagnostic) => diagnostic.code),
      'OWN0004',
    )
  }),
)

it.effect('plans lexical cleanup for continue and break without releasing outer owners', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Token { value: i32 }
pub fn main() -> i32 {
  let outer = Token { value: 42 }
  let mut iteration = 0
  while iteration < 1 {
    let inner = Token { value: 1 }
    if iteration == 0 { continue }
    break
  }
  return outer.value
}`)
    const transfers = Projections.cleanupExitsOf(self, 'mutable-loops/main').filter(
      (exit) => exit.kind === 'Continue' || exit.kind === 'Break',
    )
    assert.deepEqual(
      transfers.map((exit) => ({
        kind: exit.kind,
        releases: exit.releases.map((release) => release.binding.name),
      })),
      [
        { kind: 'Continue', releases: ['inner'] },
        { kind: 'Break', releases: ['inner'] },
      ],
    )
    assert.strictEqual(
      transfers.some((exit) => exit.releases.some((release) => release.binding.name === 'outer')),
      false,
    )
  }),
)
