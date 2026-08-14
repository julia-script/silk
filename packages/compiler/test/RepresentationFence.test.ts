import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const realized = (name: string, source: string) =>
  Analysis.ofSourceRealized(name, ascii(source), 'wasm32-unknown-unknown')

const assertFenced = (snapshot: Analysis.Snapshot, code: 'SEM0103' | 'SEM0107'): void => {
  assert.include(
    Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
    code,
  )
  assert.strictEqual(snapshot.layoutCatalog._tag, 'Unavailable')
  assert.strictEqual(snapshot.layout._tag, 'Unavailable')
  assert.strictEqual(snapshot.mir._tag, 'Unavailable')
}

it.effect('fences exact represented callable storage before layout and MIR', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'representation-fence/callable-exact',
      `struct Parser<F: fn(i32) -> i32> { parse: F }
fn decode(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let parser = Parser { parse: decode }
  return parser.parse(1)
}`,
    )

    assertFenced(snapshot, 'SEM0103')
    assert.include(
      Analysis.diagnostics(snapshot).find((diagnostic) => diagnostic.code === 'SEM0103')?.message ??
        '',
      'retains the static identity',
    )
  }),
)

it.effect('fences an open callable field after reachable exact specialization', () =>
  Effect.gen(function* () {
    const source = `struct Parser<F: fn(i32) -> i32> { parse: F }
fn decode(value: i32) -> i32 { return value }
fn make<F: fn(i32) -> i32>(parse: F) -> Parser<F> {
  return Parser<F> { parse: move parse }
}
pub fn main() -> i32 {
  let parser = make(decode)
  return parser.parse(1)
}`
    const snapshot = yield* realized('representation-fence/callable-open', source)
    const diagnostic = Analysis.diagnostics(snapshot).find(
      (candidate) => candidate.code === 'SEM0103',
    )

    assertFenced(snapshot, 'SEM0103')
    assert.strictEqual(
      diagnostic === undefined
        ? undefined
        : source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      'make(decode)',
    )
  }),
)

it.effect('fences exact represented Effect storage before layout and MIR', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'representation-fence/effect-exact',
      `struct Deferred<F: Effect<i32>> { operation: F }
pub fn main() -> i32 {
  let deferred = Deferred { operation: effect { return 1 } }
  return run deferred.operation
}`,
    )

    assertFenced(snapshot, 'SEM0107')
  }),
)

it.effect('fences an open Effect field after reachable exact specialization', () =>
  Effect.gen(function* () {
    const source = `struct Deferred<F: Effect<i32>> { operation: F }
fn defer<F: Effect<i32>>(operation: F) -> Deferred<F> {
  return Deferred<F> { operation: move operation }
}
pub fn main() -> i32 {
  let deferred = defer(effect { return 1 })
  return run deferred.operation
}`
    const snapshot = yield* realized('representation-fence/effect-open', source)
    const diagnostic = Analysis.diagnostics(snapshot).find(
      (candidate) => candidate.code === 'SEM0107',
    )

    assertFenced(snapshot, 'SEM0107')
    assert.strictEqual(
      diagnostic === undefined
        ? undefined
        : source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      'defer(effect { return 1 })',
    )
  }),
)

it.effect('fences nested repeated represented callable fields through the shared field index', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'representation-fence/callable-nested-repeated',
      `struct Parser<F: fn(i32) -> i32> { parse: F }
struct Pair<F: fn(i32) -> i32> { first: Parser<F> second: Parser<F> }
fn decode(value: i32) -> i32 { return value }
fn pair<F: fn(i32) -> i32>(first: Parser<F>, second: Parser<F>) -> Pair<F> {
  return Pair<F> { first: move first, second: move second }
}
pub fn main() -> i32 {
  let first = Parser { parse: decode }
  let second = Parser { parse: decode }
  let paired = pair(move first, move second)
  return 0
}`,
    )
    const diagnostics = Analysis.diagnostics(snapshot).filter(
      (diagnostic) => diagnostic.code === 'SEM0103',
    )

    assertFenced(snapshot, 'SEM0103')
    assert.strictEqual(diagnostics.length, 3)
    assert.strictEqual(
      diagnostics.some((diagnostic) => diagnostic.message.includes('field first')),
      true,
    )
  }),
)

it.effect('fences nested repeated represented Effect fields through the shared field index', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'representation-fence/effect-nested-repeated',
      `struct Deferred<F: Effect<i32>> { operation: F }
struct Pair<F: Effect<i32>, G: Effect<i32>> { first: Deferred<F> second: Deferred<G> }
fn pair<F: Effect<i32>, G: Effect<i32>>(
  first: Deferred<F>,
  second: Deferred<G>
) -> Pair<F, G> {
  return Pair<F, G> { first: move first, second: move second }
}
pub fn main() -> i32 {
  let first = Deferred { operation: effect { return 1 } }
  let second = Deferred { operation: effect { return 2 } }
  let paired = pair(move first, move second)
  return 0
}`,
    )
    const diagnostics = Analysis.diagnostics(snapshot).filter(
      (diagnostic) => diagnostic.code === 'SEM0107',
    )

    assertFenced(snapshot, 'SEM0107')
    assert.strictEqual(diagnostics.length, 3)
    assert.strictEqual(
      diagnostics.some((diagnostic) => diagnostic.message.includes('field first')),
      true,
    )
  }),
)
