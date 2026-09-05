import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as InstanceDiagnostics from '../src/InstanceDiagnostics.js'
import * as Lifetime from '../src/Lifetime.js'
import * as MirVerification from '../src/MirVerification.js'
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

const assertRealizedCallable = (snapshot: Analysis.Snapshot): void => {
  assert.notInclude(
    Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
    'SEM0103',
  )
  assert.strictEqual(snapshot.layoutCatalog._tag, 'Available')
  assert.strictEqual(snapshot.layout._tag, 'Available')
  assert.strictEqual(snapshot.mir._tag, 'Available')
  if (snapshot.mir._tag === 'Available')
    assert.deepEqual(MirVerification.verify(snapshot.mir.value), [])
}

const assertRealizedEffect = (snapshot: Analysis.Snapshot): void => {
  assert.notInclude(
    Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
    'SEM0107',
  )
  assert.strictEqual(snapshot.layoutCatalog._tag, 'Available')
  assert.strictEqual(snapshot.layout._tag, 'Available')
  assert.strictEqual(snapshot.mir._tag, 'Available')
  if (snapshot.mir._tag === 'Available')
    assert.deepEqual(MirVerification.verify(snapshot.mir.value), [])
}

it.effect('realizes exact represented callable storage through layout and MIR', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'representation-fence/callable-exact',
      `struct Parser<F: fn<'static>(i32) -> i32> { parse: F }
fn decode(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let parser = Parser { parse: decode }
  return parser.parse(1)
}`,
    )

    assertRealizedCallable(snapshot)
  }),
)

it.effect('realizes an open callable field after reachable exact specialization', () =>
  Effect.gen(function* () {
    const source = `struct Parser<F: fn<'static>(i32) -> i32> { parse: F }
fn decode(value: i32) -> i32 { return value }
fn make<F: fn<'static>(i32) -> i32>(parse: F) -> Parser<F> {
  return Parser<F> { parse: move parse }
}
pub fn main() -> i32 {
  let parser = make(decode)
  return parser.parse(1)
}`
    const snapshot = yield* realized('representation-fence/callable-open', source)
    assertRealizedCallable(snapshot)
  }),
)

it.effect('realizes exact represented Effect storage through layout and MIR', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'representation-fence/effect-exact',
      `struct Deferred<F: Effect<'static; i32>> { operation: F }
pub fn main() -> i32 {
  let deferred = Deferred { operation: effect { return 1 } }
  return run deferred.operation
}`,
    )

    assertRealizedEffect(snapshot)
  }),
)

it.effect('realizes represented Effect storage for every proven run mode', () =>
  Effect.gen(function* () {
    const modes = [
      ['shared', "Effect<'static; i32>"],
      ['exclusive', "mut Effect<'static; i32>"],
      ['consuming', "once Effect<'static; i32>"],
    ] as const

    for (const [name, bound] of modes) {
      const snapshot = yield* realized(
        `representation-fence/effect-${name}`,
        `struct Deferred<F: ${bound}> { operation: F }
pub fn main() -> i32 {
  let deferred = Deferred { operation: effect { return 1 } }
  return 0
}`,
      )

      assertRealizedEffect(snapshot)
    }
  }),
)

it.effect('realizes an open Effect field after reachable exact specialization', () =>
  Effect.gen(function* () {
    const source = `struct Deferred<F: Effect<'static; i32>> { operation: F }
fn defer<F: Effect<'static; i32>>(operation: F) -> Deferred<F> {
  return Deferred<F> { operation: move operation }
}
pub fn main() -> i32 {
  let deferred = defer(effect { return 1 })
  return run deferred.operation
}`
    const snapshot = yield* realized('representation-fence/effect-open', source)
    assertRealizedEffect(snapshot)
  }),
)

it.effect('fences a represented callable element nested in a specialized fixed array', () =>
  Effect.gen(function* () {
    const source = `fn decode(value: i32) -> i32 { return value }
fn singleton<F: fn<'static>(i32) -> i32>(value: F) -> [F; 1] {
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
    const source = `fn singleton<F: Effect<'static; i32>>(value: F) -> [F; 1] {
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

it.effect('fences represented callable storage through nested arrays inside a struct field', () =>
  Effect.gen(function* () {
    const source = `struct Bucket<F: fn<'static>(i32) -> i32> { values: [[F; 1]; 1] }
fn decode(value: i32) -> i32 { return value }
fn bucket<F: fn<'static>(i32) -> i32>(value: F) -> Bucket<F> {
  return Bucket<F> { values: [[move value]] }
}
pub fn main() -> i32 {
  let values = bucket(decode)
  return 0
}`
    const snapshot = yield* realized('representation-fence/callable-nested-array-field', source)

    assertFenced(snapshot, 'SEM0103')
    assert.strictEqual(
      Analysis.diagnostics(snapshot).filter((diagnostic) => diagnostic.code === 'SEM0103').length,
      2,
    )
  }),
)

it.effect('fences an array whose elements are represented Effect nominals', () =>
  Effect.gen(function* () {
    const source = `struct Deferred<F: Effect<'static; i32>> { operation: F }
fn singleton<F: Effect<'static; i32>>(operation: F) -> [Deferred<F>; 1] {
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
fn empty<F: fn<'static>(i32) -> i32>(value: F) -> [F; 0] {
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
    const callable = Type.callable(['i32'], 'i32', {
      environment: Lifetime.staticLifetime,
      lifetimeBinders: [],
    })
    const callableArgument = Type.exactRepresentationArgument(
      Type.callableIdentityArgument('container-shapes.decode', {
        _tag: 'Declaration',
        module: 'representation-fence/container-shapes',
        name: 'decode',
      }),
      callable,
    )
    const representedCallable = Type.represented(callable, callable, callableArgument)
    const effect = Type.effect('i32', [], {
      environment: Lifetime.staticLifetime,
      lifetimeBinders: [],
    })
    const representedEffect = Type.represented(
      effect,
      effect,
      Type.exactRepresentationArgument(
        Type.effectIdentityArgument('container-shapes.effect'),
        effect,
      ),
    )

    assert.strictEqual(
      InstanceDiagnostics.storedRepresentation(
        snapshot.index,
        Type.fixedArray(representedCallable, 0),
        'Callable',
      )?.contract._tag,
      'CallableType',
    )
    assert.strictEqual(
      InstanceDiagnostics.storedRepresentation(
        snapshot.index,
        Type.fixedArray(Type.fixedArray(representedCallable, 1), 1),
        'Callable',
      )?.contract._tag,
      'CallableType',
    )
    assert.strictEqual(
      InstanceDiagnostics.storedRepresentation(
        snapshot.index,
        Type.slice('Shared', representedEffect, Lifetime.staticLifetime),
        'Effect',
      )?.contract._tag,
      'EffectType',
    )
    assert.strictEqual(
      InstanceDiagnostics.storedRepresentation(
        snapshot.index,
        Type.reference('Shared', representedCallable, Lifetime.staticLifetime),
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
      `import silk.i32 as i32
struct Pointer<F: fn<'static>(i32) -> i32> { operation: &F }
fn point<F: fn<'static>(i32) -> i32>(operation: &F) -> Pointer<F> {
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
      `struct Pointer<F: Effect<'static; i32>> { operation: &F }
fn point<F: Effect<'static; i32>>(operation: &F) -> Pointer<F> {
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

it.effect(
  'realizes nested repeated represented callable fields through the shared field index',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* realized(
        'representation-fence/callable-nested-repeated',
        `struct Parser<F: fn<'static>(i32) -> i32> { parse: F }
struct Pair<F: fn<'static>(i32) -> i32> { first: Parser<F> second: Parser<F> }
fn decode(value: i32) -> i32 { return value }
fn pair<F: fn<'static>(i32) -> i32>(first: Parser<F>, second: Parser<F>) -> Pair<F> {
  return Pair<F> { first: move first, second: move second }
}
pub fn main() -> i32 {
  let first = Parser { parse: decode }
  let second = Parser { parse: decode }
  let paired = pair(move first, move second)
  return 0
}`,
      )
      assertRealizedCallable(snapshot)
    }),
)

it.effect('realizes nested repeated represented Effect fields through the shared field index', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'representation-fence/effect-nested-repeated',
      `struct Deferred<F: Effect<'static; i32>> { operation: F }
struct Pair<F: Effect<'static; i32>, G: Effect<'static; i32>> { first: Deferred<F> second: Deferred<G> }
fn pair<F: Effect<'static; i32>, G: Effect<'static; i32>>(
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
    assertRealizedEffect(snapshot)
  }),
)
