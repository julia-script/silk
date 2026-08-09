import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (source: string, target = 'wasm32-unknown-unknown') =>
  Analysis.ofSource('mutable-loops/main', ascii(source), target)

const runWasm = Effect.fnUntraced(function* (source: string) {
  const self = yield* snapshot(source)
  const bytes = (yield* Analysis.codegenWasm(self, { mode: 'release' })).bytes.slice()
  const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes), {})
  const main = instance.exports.silk_main
  if (typeof main !== 'function') throw new Error('wasm program lost silk_main')
  return main()
})

it.effect('mutates a scalar through a structured loop DAG', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub fn main() -> I32 {
  let mut count = 0
  while count < 3 { count = count + 1 }
  return count
}`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    const mir = Analysis.loweredMir(self)
    assert.deepEqual(Mir.verify(mir), [])
    assert.strictEqual(
      mir.functions.at(0)?.regions.some((region) => region._tag === 'LoopRegion'),
      true,
    )
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 3)
    assert.strictEqual(outcome.trace.filter((event) => event._tag === 'Iteration').length, 3)
  }),
)

it.effect('updates checked array elements and returns the replacement', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub fn main() -> I32 {
  let mut values = [1, 2, 3]
  let mut index = 0
  while index < 3 {
    values[index] = values[index] + 1
    index = index + 1
  }
  return values[1]
}`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 3)
    assert.strictEqual(outcome.trace.filter((event) => event._tag === 'Replacement').length, 6)
  }),
)

it.effect('emits scalar and checked-array loops directly as structured WebAssembly', () =>
  Effect.gen(function* () {
    const scalar = `pub fn main() -> I32 {
  let mut count = 0
  while count < 3 { count = count + 1 }
  return count
}`
    const array = `pub fn main() -> I32 {
  let mut values = [1, 2, 3]
  let mut index = 0
  while index < 3 {
    values[index] = values[index] + 1
    index = index + 1
  }
  return values[1]
}`
    assert.strictEqual(yield* runWasm(scalar), 3)
    assert.strictEqual(yield* runWasm(array), 3)
    const artifact = yield* Analysis.codegenWasm(yield* snapshot(scalar), { mode: 'release' })
    assert.include(artifact.wat, 'block')
    assert.include(artifact.wat, 'loop')
    assert.notInclude(artifact.wat, 'br_table')
    assert.strictEqual(
      artifact.control.some((entry) => entry.construct === 'WasmLoop'),
      true,
    )
    assert.strictEqual(
      artifact.control.some((entry) => entry.construct === 'WasmBr' && entry.loop?.ordinal === 0),
      true,
    )
  }),
)

it.effect('resolves continue and break to the innermost loop', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub fn main() -> I32 {
  let mut index = 0
  while index < 10 {
    index = index + 1
    if index == 2 { continue }
    if index == 4 { break }
  }
  return index
}`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 4)
    assert.isAbove(outcome.trace.filter((event) => event._tag === 'Repeat').length, 0)
    assert.strictEqual(outcome.trace.filter((event) => event._tag === 'Exit').length, 1)
  }),
)

it.effect('diagnoses immutable writes and transfers outside loops', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub fn main() -> I32 {
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
      'pub fn main() -> I32 { let mut value = 0 while value == 0 { value = 42 } return value }',
    )
    const kinds = Analysis.rootAnalysis(self).syntax.tokens.map((token) => token.kind)
    assert.strictEqual(kinds.filter((kind) => kind === 'Equals').length, 2)
    assert.strictEqual(kinds.filter((kind) => kind === 'EqualEqual').length, 1)
    assert.deepEqual(Analysis.diagnostics(self), [])
  }),
)

it.effect('recovers damaged mutable statements without losing following declarations', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub fn damaged() -> I32 {
  let mut value = 0
  value = @
  return value
}
pub fn main() -> I32 { return 42 }`)
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
    const self = yield* snapshot(`pub fn main() -> I32 {
  let mut value = 0
  while value < 2 { value = value + 1 if value == 1 { continue } }
  return value
}`)
    const bindings = Analysis.bindingsOf(self, 'mutable-loops/main')
    const writes = Analysis.writesOf(self, 'mutable-loops/main')
    const loops = Analysis.loopsOf(self, 'mutable-loops/main')
    const transfers = Analysis.transfersOf(self, 'mutable-loops/main')
    const regions = Analysis.controlRegionsOf(self)
    const edges = Analysis.controlEdgesOf(self)
    const fixedPoints = Analysis.ownershipFixedPointsOf(self, 'mutable-loops/main')

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

it.effect('restores a moved owner with a complete replacement before continue', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Token { value: I32 }
pub fn main() -> I32 {
  let mut token = Token { value: 1 }
  let mut iteration = 0
  while iteration < 1 {
    let old = move token
    token = Token { value: 42 }
    iteration = iteration + 1
    continue
  }
  return token.value
}`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(
      Analysis.ownershipFixedPointsOf(self, 'mutable-loops/main').at(0)?.compatible,
      true,
    )
    const result = Analysis.evaluate(self)
    assert.strictEqual(result._tag, 'Completed')
    if (result._tag === 'Completed') {
      assert.strictEqual(result.result.value, 42)
      assert.strictEqual(
        result.trace.filter((event) => event._tag === 'ReplacementCleanup').length,
        1,
      )
    }
  }),
)

it.effect('rejects incompatible repeating owner states and overlapping replacement', () =>
  Effect.gen(function* () {
    const incompatible = yield* snapshot(`struct Token { value: I32 }
pub fn main() -> I32 {
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

    const overlapping = yield* snapshot(`struct Token { value: I32 }
pub fn main() -> I32 {
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

it.effect('checks an indexed destination before evaluating its right-hand call', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn replacement() -> I32 { return 42 }
pub fn main() -> I32 {
  let mut values = [1, 2]
  let index = 2
  values[index] = replacement()
  return 0
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Blocked')
    assert.strictEqual(
      outcome.trace.some((event) => event._tag === 'Call' && event.target.name === 'replacement'),
      false,
    )
    assert.strictEqual(
      outcome.trace.some((event) => event._tag === 'Replacement'),
      false,
    )
  }),
)

it.effect('plans lexical cleanup for continue and break without releasing outer owners', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Token { value: I32 }
pub fn main() -> I32 {
  let outer = Token { value: 42 }
  let mut iteration = 0
  while iteration < 1 {
    let inner = Token { value: 1 }
    if iteration == 0 { continue }
    break
  }
  return outer.value
}`)
    const transfers = Analysis.cleanupExitsOf(self, 'mutable-loops/main').filter(
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

it.effect('repeats loop DAG encodings, facade facts, traces, and wasm bytes exactly', () =>
  Effect.gen(function* () {
    const source = `pub fn main() -> I32 {
  let mut outer = 0
  let mut total = 0
  while outer < 2 {
    let mut inner = 0
    while inner < 3 { total = total + 7 inner = inner + 1 }
    outer = outer + 1
  }
  return total
}`
    const first = yield* snapshot(source)
    const second = yield* snapshot(source)
    assert.strictEqual(
      Mir.encode(Analysis.loweredMir(first)),
      Mir.encode(Analysis.loweredMir(second)),
    )
    assert.deepEqual(Analysis.controlEdgesOf(first), Analysis.controlEdgesOf(second))
    assert.deepEqual(
      Analysis.ownershipFixedPointsOf(first, 'mutable-loops/main'),
      Analysis.ownershipFixedPointsOf(second, 'mutable-loops/main'),
    )
    assert.deepEqual(Analysis.evaluate(first), Analysis.evaluate(second))
    const firstArtifact = yield* Analysis.codegenWasm(first, { mode: 'release' })
    const secondArtifact = yield* Analysis.codegenWasm(second, { mode: 'release' })
    assert.strictEqual(firstArtifact.wat, secondArtifact.wat)
    assert.deepEqual(firstArtifact.bytes, secondArtifact.bytes)
    assert.deepEqual(
      Analysis.backendControlOf(firstArtifact),
      Analysis.backendControlOf(secondArtifact),
    )
  }),
)
