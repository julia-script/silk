import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Instances from '../src/Instances.js'
import * as Type from '../src/Type.js'

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

it.effect('fences a represented callable element nested in a specialized fixed array', () =>
  Effect.gen(function* () {
    const source = `fn decode(value: i32) -> i32 { return value }
fn singleton<F: fn(i32) -> i32>(value: F) -> [F; 1] {
  return [move value]
}
pub fn main() -> i32 {
  let values = singleton(decode)
  return 0
}`
    const snapshot = yield* realized('representation-fence/callable-array', source)
    const diagnostic = Analysis.diagnostics(snapshot).find(
      (candidate) => candidate.code === 'SEM0103',
    )

    assertFenced(snapshot, 'SEM0103')
    assert.strictEqual(
      diagnostic === undefined
        ? undefined
        : source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      'singleton(decode)',
    )
    assert.deepEqual(
      diagnostic?.relatedSpans?.map((related) => [
        related.label,
        source.slice(related.span.start, related.span.end).trim(),
      ]),
      [['constructed here', '[move value]']],
    )
  }),
)

it.effect('fences a represented Effect element nested in a specialized fixed array', () =>
  Effect.gen(function* () {
    const source = `fn singleton<F: Effect<i32>>(value: F) -> [F; 1] {
  return [move value]
}
pub fn main() -> i32 {
  let values = singleton(effect { return 1 })
  return 0
}`
    const snapshot = yield* realized('representation-fence/effect-array', source)
    const diagnostic = Analysis.diagnostics(snapshot).find(
      (candidate) => candidate.code === 'SEM0107',
    )

    assertFenced(snapshot, 'SEM0107')
    assert.strictEqual(
      diagnostic === undefined
        ? undefined
        : source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      'singleton(effect { return 1 })',
    )
    assert.deepEqual(
      diagnostic?.relatedSpans?.map((related) => [
        related.label,
        source.slice(related.span.start, related.span.end).trim(),
      ]),
      [['constructed here', '[move value]']],
    )
  }),
)

it.effect('fences represented callable storage through a struct field and nested arrays', () =>
  Effect.gen(function* () {
    const source = `struct Bucket<F: fn(i32) -> i32> { values: [[F; 1]; 1] }
fn decode(value: i32) -> i32 { return value }
fn bucket<F: fn(i32) -> i32>(value: F) -> Bucket<F> {
  return Bucket<F> { values: [[move value]] }
}
pub fn main() -> i32 {
  let values = bucket(decode)
  return 0
}`
    const snapshot = yield* realized('representation-fence/callable-nested-array-field', source)

    assertFenced(snapshot, 'SEM0103')
    assert.strictEqual(
      Analysis.diagnostics(snapshot)
        .filter((diagnostic) => diagnostic.code === 'SEM0103')
        .some((diagnostic) => diagnostic.message.includes('values')),
      true,
    )
  }),
)

it.effect('fences an array whose elements are represented Effect nominals', () =>
  Effect.gen(function* () {
    const source = `struct Deferred<F: Effect<i32>> { operation: F }
fn singleton<F: Effect<i32>>(operation: F) -> [Deferred<F>; 1] {
  return [Deferred<F> { operation: move operation }]
}
pub fn main() -> i32 {
  let values = singleton(effect { return 1 })
  return 0
}`
    const snapshot = yield* realized('representation-fence/effect-nominal-array', source)

    assertFenced(snapshot, 'SEM0107')
    assert.strictEqual(
      Analysis.diagnostics(snapshot)
        .filter((diagnostic) => diagnostic.code === 'SEM0107')
        .some((diagnostic) => diagnostic.message.includes('operation')),
      true,
    )
  }),
)

it.effect('fences a specialized zero-length represented callable array', () =>
  Effect.gen(function* () {
    const source = `fn decode(value: i32) -> i32 { return value }
fn empty<F: fn(i32) -> i32>(value: F) -> [F; 0] {
  return []
}
pub fn main() -> i32 {
  let values = empty(decode)
  return 0
}`
    const snapshot = yield* realized('representation-fence/callable-empty-array', source)

    assertFenced(snapshot, 'SEM0103')
  }),
)

it.effect('keeps array and slice traversal symmetric without descending through references', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'representation-fence/container-shapes',
      ascii('pub fn main() -> i32 { return 0 }'),
    )
    const callable = Type.callable(['i32'], 'i32')
    const callableArgument = Type.exactRepresentationArgument(
      Type.callableIdentityArgument('container-shapes.decode', {
        _tag: 'Declaration',
        module: 'representation-fence/container-shapes',
        name: 'decode',
      }),
      callable,
    )
    const representedCallable = Type.represented(callable, callable, callableArgument)
    const effect = Type.effect('i32', [])
    const representedEffect = Type.represented(
      effect,
      effect,
      Type.exactRepresentationArgument(
        Type.effectIdentityArgument('container-shapes.effect'),
        effect,
      ),
    )

    assert.strictEqual(
      Instances.storedRepresentation(
        snapshot.index,
        Type.fixedArray(representedCallable, 0),
        'Callable',
      )?.contract._tag,
      'CallableType',
    )
    assert.strictEqual(
      Instances.storedRepresentation(
        snapshot.index,
        Type.fixedArray(Type.fixedArray(representedCallable, 1), 1),
        'Callable',
      )?.contract._tag,
      'CallableType',
    )
    assert.strictEqual(
      Instances.storedRepresentation(
        snapshot.index,
        Type.slice('Shared', representedEffect),
        'Effect',
      )?.contract._tag,
      'EffectType',
    )
    assert.strictEqual(
      Instances.storedRepresentation(
        snapshot.index,
        Type.reference('Shared', representedCallable),
        'Callable',
      ),
      undefined,
    )
  }),
)

it.effect('does not recurse through references to represented executable values', () =>
  Effect.gen(function* () {
    const callable = yield* realized(
      'representation-fence/callable-reference',
      `struct Pointer<F: fn(i32) -> i32> { operation: &F }
fn point<F: fn(i32) -> i32>(operation: &F) -> Pointer<F> {
  return Pointer<F> { operation }
}
pub fn main() -> i32 {
  let operation = i32.add(1)
  let pointer = point(&operation)
  return 0
}`,
    )
    const effect = yield* realized(
      'representation-fence/effect-reference',
      `struct Pointer<F: Effect<i32>> { operation: &F }
fn point<F: Effect<i32>>(operation: &F) -> Pointer<F> {
  return Pointer<F> { operation }
}
pub fn main() -> i32 {
  let operation = effect { return 1 }
  let pointer = point(&operation)
  return 0
}`,
    )

    assert.strictEqual(
      Analysis.diagnostics(callable).some((diagnostic) => diagnostic.code === 'SEM0103'),
      false,
    )
    assert.strictEqual(
      Analysis.diagnostics(effect).some((diagnostic) => diagnostic.code === 'SEM0107'),
      false,
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

it.effect('fences a represented capturing section stored in a nominal field', () =>
  Effect.gen(function* () {
    const source = `struct Adder<F: fn(i32) -> i32> { step: F }
pub fn main() -> i32 {
  let adder = Adder { step: i32.add(1) }
  return adder.step(2)
}`
    const snapshot = yield* realized('representation-fence/callable-section', source)
    const diagnostic = Analysis.diagnostics(snapshot).find(
      (candidate) => candidate.code === 'SEM0103',
    )

    assertFenced(snapshot, 'SEM0103')
    assert.include(diagnostic?.message ?? '', 'retains the static identity')
    assert.include(diagnostic?.message ?? '', 'field step')
  }),
)

it.effect('fences a represented take-once callable field stored in a nominal', () =>
  Effect.gen(function* () {
    const source = `struct Once<F: once fn(i32) -> i32> { step: F }
fn decode(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let taken = Once { step: decode }
  return taken.step(1)
}`
    const snapshot = yield* realized('representation-fence/callable-once', source)

    assertFenced(snapshot, 'SEM0103')
  }),
)
