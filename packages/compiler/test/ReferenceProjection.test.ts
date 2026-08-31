import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as MirVerification from '../src/MirVerification.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('reads a Copy scalar through an explicit referent projection', () =>
  Effect.gen(function* () {
    const source = `fn read(value: &i32) -> i32 { return value.* }
pub fn main() -> i32 {
  let value = 42
  return read(&value)
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/scalar-referent',
      ascii(source),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const [projection] = Analysis.referentProjectionsOf(
      snapshot,
      'reference-projection/scalar-referent',
    )
    assert.strictEqual(projection?.state._tag, 'Resolved')
    assert.strictEqual(projection?.borrowAccess, 'Shared')
    assert.strictEqual(projection?.type._tag, 'Available')
    if (projection?.type._tag === 'Available') assert.strictEqual(projection.type.type, 'i32')
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('reads a zero-lane Copy referent without inventing runtime state', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/zero-lane-referent',
      ascii(`struct Empty {}
impl Copy for Empty {}
fn read(value: &Empty) -> Empty { return value.* }
pub fn main() -> i32 {
  let value = Empty {}
  let copied = read(&value)
  drop copied
  return 42
}`),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('reads and writes a runtime-indexed element through an explicit referent', () =>
  Effect.gen(function* () {
    const source = `import silk.usize as usize
struct Buffer { values: [i32; 3] }
fn update(buffer: &mut Buffer, index: usize) -> i32 {
  buffer.*.values[index] = 42
  return buffer.*.values[index]
}
pub fn main() -> i32 {
  let mut buffer = Buffer { values: [1, 2, 3] }
  return update(&mut buffer, usize.ONE)
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/runtime-indexed-referent',
      ascii(source),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('retains failed referent facts and rejects affine borrowed reads', () =>
  Effect.gen(function* () {
    const invalid = yield* Analysis.ofSourceRealized(
      'reference-projection/non-reference-referent',
      ascii('fn invalid(value: i32) -> i32 { return value.* }'),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(invalid).map((diagnostic) => diagnostic.code),
      ['SEM0171'],
    )
    assert.strictEqual(
      Analysis.referentProjectionsOf(invalid, 'reference-projection/non-reference-referent').at(0)
        ?.state._tag,
      'Unavailable',
    )

    const affine = yield* Analysis.ofSourceRealized(
      'reference-projection/affine-referent',
      ascii(`struct Token { value: i32 }
fn invalid(value: &Token) -> Token { return value.* }`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(affine).map((diagnostic) => diagnostic.code),
      ['OWN0012'],
    )
  }),
)

it.effect('reborrows value-reference parameters for nested calls and restores the parent', () =>
  Effect.gen(function* () {
    const source = `struct Box { value: i32 }
fn increment(box: &mut Box) -> () { box.value = box.value + 1 }
fn observe(box: &Box) -> i32 { return box.value }
fn read(value: &i32) -> i32 { return value.* }
fn forwarded(value: &i32) -> i32 { return read(&value.*) }
fn twice(box: &mut Box) -> i32 {
  increment(&mut box)
  increment(&mut box)
  return observe(&box) + forwarded(&box.value)
}
pub fn main() -> i32 {
  let mut box = Box { value: 20 }
  return twice(&mut box)
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/value-reborrow',
      ascii(source),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.include(Hir.encode(Analysis.rootAnalysis(snapshot).hir), 'reborrow-value')
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 44n)
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 44)
  }),
)

it.effect('rejects strengthening a shared value-reference reborrow', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/value-reborrow-strengthening',
      ascii(`struct Box { value: i32 }
fn mutate(box: &mut Box) -> () { box.value = 1 }
fn invalid(box: &Box) -> () { mutate(&mut box) }`),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0056'],
    )
  }),
)

it.effect('replaces an exclusive referent with exact-once cleanup', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/referent-replacement',
      ascii(`struct Token { value: i32 }
fn replace(token: &mut Token) -> i32 {
  token.* = Token { value: 42 }
  return token.*.value
}
pub fn main() -> i32 {
  let mut token = Token { value: 1 }
  return replace(&mut token)
}`),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.strictEqual(
      evaluated.trace.filter((event) => event._tag === 'ReplacementCleanup').length,
      1,
    )
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

    const shared = yield* Analysis.ofSourceRealized(
      'reference-projection/shared-referent-replacement',
      ascii(`struct Token { value: i32 }
fn invalid(token: &Token) -> () { token.* = Token { value: 1 } }`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(shared).map((diagnostic) => diagnostic.code),
      ['SEM0036'],
    )
  }),
)

it.effect('reads and writes fields through nominal references on both targets', () =>
  Effect.gen(function* () {
    const source = `struct Counter { value: i32 }

fn bump(self: &mut Counter) -> i32 {
  self.value = self.value + 1
  return self.value
}

fn peek(self: &Counter) -> i32 {
  return self.value
}

pub fn main() -> i32 {
  let mut counter = Counter { value: 40 }
  let bumped = bump(&mut counter)
  let again = bump(&mut counter)
  return again + peek(&counter) - again
}`
    for (const target of ['aarch64-apple-darwin', 'wasm32-unknown-unknown']) {
      const snapshot = yield* Analysis.ofSourceRealized(
        'reference-projection/counter',
        ascii(source),
        target,
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], target)
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', target)
      if (evaluated._tag !== 'Completed') continue
      assert.strictEqual(evaluated.result.value, 42n, target)
    }
    const wasm = yield* Analysis.codegenWasm(
      yield* Analysis.ofSourceRealized(
        'reference-projection/counter',
        ascii(source),
        'wasm32-unknown-unknown',
      ),
      { mode: 'release' },
    )
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('keeps reference projection inside the borrow contract', () =>
  Effect.gen(function* () {
    // Writing through a shared reference is not a writable place.
    const shared = yield* Analysis.ofSourceRealized(
      'reference-projection/shared-write',
      ascii(`struct Counter { value: i32 }
fn bump(self: &Counter) -> i32 {
  self.value = 1
  return self.value
}`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(shared).map((diagnostic) => diagnostic.code),
      ['SEM0036'],
    )

    // Consuming a field through a reference stays a partial move.
    const stolen = yield* Analysis.ofSourceRealized(
      'reference-projection/steal',
      ascii(`struct Token { value: i32 }
struct Holder { token: Token }
fn steal(self: &mut Holder) -> Token {
  return move self.token
}`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(stolen).map((diagnostic) => diagnostic.code),
      ['OWN0002'],
    )
  }),
)

it.effect('returns an exclusive nominal reference through a pipeline', () =>
  Effect.gen(function* () {
    const source = `struct Counter {
  value: i32
}

fn increment(counter: &mut Counter) -> &mut Counter {
  counter.value = counter.value + 1
  return move counter
}

pub fn main() -> i32 {
  let mut counter = Counter { value: 0 }
  let result = &mut counter |> increment
  return result.value
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/returned-pipeline',
      ascii(source),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 1n)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 1)
  }),
)

it.effect('mutates an owned parameter transferred through a pipeline', () =>
  Effect.gen(function* () {
    const source = `struct Counter {
  value: i32
}

fn increment(mut counter: Counter) -> Counter {
  counter.value = counter.value + 1
  return move counter
}

pub fn main() -> i32 {
  let counter = Counter { value: 0 }
  let result = move counter |> increment
  return result.value
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/mutable-owned-parameter',
      ascii(source),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 1n)
  }),
)

it.effect(
  'keeps mutable owned parameters out of sections and generic specialization identity',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'reference-projection/mutable-owned-callable-identity',
        ascii(`struct Counter { value: i32 }
fn adjust<T>(mut value: T, delta: i32) -> T { return move value }
fn increment(mut counter: Counter, delta: i32) -> Counter {
  counter.value = counter.value + delta
  return move counter
}
pub fn main() -> i32 {
  let callback = increment(2)
  let first = Counter { value: 40 }
  let updated = callback(move first)
  let specialized = adjust<Counter>(move updated, 0)
  return specialized.value
}`),
        'wasm32-unknown-unknown',
      )

      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
    }),
)

it.effect('cleans and replaces mutable owned parameter storage exactly once', () =>
  Effect.gen(function* () {
    const source = `struct Token { value: i32 }
fn replace(mut token: Token) -> Token {
  token = Token { value: 42 }
  return move token
}
pub fn main() -> i32 {
  let token = Token { value: 1 }
  let result = replace(move token)
  return result.value
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'reference-projection/mutable-owned-replacement',
      ascii(source),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(snapshot).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.strictEqual(
      evaluated.trace.filter((event) => event._tag === 'ReplacementCleanup').length,
      1,
    )
  }),
)
