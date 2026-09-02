import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Driver from './support/TestDriver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-vector-sort-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

const program = (body: string): string =>
  `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.usize as usize
import silk.vector { Vector }
import silk.option { Option }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
${body}
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 99 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

/** Appends each value in source order through the effectful `append`. */
const fill = (name: string, values: ReadonlyArray<number>): string =>
  values
    .map(
      (value, ordinal) =>
        `  let ${name}${ordinal} = run Vector.append<i32>(&mut ${name}, ${value}) |> Effect.provideMut(&mut allocator)`,
    )
    .join('\n')

/**
 * Folds the whole sequence into one integer, so a single returned value pins every element and its
 * position rather than only the set of elements.
 */
const fingerprint = (name: string, radix: number, into: string): string =>
  `  let mut ${into}Index = usize.ZERO
  let mut ${into} = 0
  while ${into}Index < Vector.length<i32>(&${name}) {
    ${into} = ${into} * ${radix} + Vector.get<i32>(&${name}, ${into}Index)
    ${into}Index = ${into}Index + usize.ONE
  }`

const evaluate = (name: string, source: string) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(name, ascii(source))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(
      outcome._tag,
      'Completed',
      Json.stringify(outcome, (_, value) => (typeof value === 'bigint' ? value.toString() : value)),
    )
    return outcome
  })

const evaluatedValue = (name: string, source: string) =>
  Effect.map(evaluate(name, source), (outcome) =>
    outcome._tag === 'Completed' ? Number(outcome.result.value) : undefined,
  )

// [9, -3, 5, 1, 0, -8, 7, 2] sorts to [-8, -3, 0, 1, 2, 5, 7, 9], which folds to -19473 in radix 3.
const unsorted = [9, -3, 5, 1, 0, -8, 7, 2]
const sortEight = program(`  let mut values = Vector.make<i32>()
${fill('values', unsorted)}
  let sorted = run Vector.sort<i32>(&mut values) |> Effect.provideMut(&mut allocator)
${fingerprint('values', 3, 'folded')}
  return folded`)

it.effect('sorts an empty vector and a one-element vector', () =>
  Effect.gen(function* () {
    const value = yield* evaluatedValue(
      'vector-sort/small',
      program(`  let mut empty = Vector.make<i32>()
  let emptied = run Vector.sort<i32>(&mut empty) |> Effect.provideMut(&mut allocator)
  let mut single = Vector.make<i32>()
${fill('single', [5])}
  let singled = run Vector.sort<i32>(&mut single) |> Effect.provideMut(&mut allocator)
  if Vector.length<i32>(&empty) == usize.ZERO {
    if Vector.length<i32>(&single) == usize.ONE {
      return Vector.get<i32>(&single, usize.ZERO) + 37
    }
  }
  return 0`),
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('orders every element of an unsorted vector', () =>
  Effect.gen(function* () {
    assert.strictEqual(yield* evaluatedValue('vector-sort/eight', sortEight), -19473)
  }),
)

it.effect('gives the same output for two runs over the same input', () =>
  Effect.gen(function* () {
    // Two independent vectors built from the same input, sorted separately in one program: the
    // returned zero is the difference of their fingerprints, so any divergence is non-zero.
    const value = yield* evaluatedValue(
      'vector-sort/repeatable',
      program(`  let mut first = Vector.make<i32>()
${fill('first', unsorted)}
  let sortedFirst = run Vector.sort<i32>(&mut first) |> Effect.provideMut(&mut allocator)
${fingerprint('first', 3, 'firstFold')}
  let mut second = Vector.make<i32>()
${fill('second', unsorted)}
  let sortedSecond = run Vector.sort<i32>(&mut second) |> Effect.provideMut(&mut allocator)
${fingerprint('second', 3, 'secondFold')}
  if firstFold == secondFold { return 42 }
  return firstFold - secondFold`),
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('keeps equal elements in one defined relative position', () =>
  Effect.gen(function* () {
    // [4, 1, 4, 1, 3, 4, 1] sorts to [1, 1, 1, 3, 4, 4, 4]. The fingerprint pins the exact
    // arrangement of the three equal 1s and the three equal 4s, not merely that the result is
    // ordered, so a change in how equal elements are placed fails this test.
    const value = yield* evaluatedValue(
      'vector-sort/equal-elements',
      program(`  let mut values = Vector.make<i32>()
${fill('values', [4, 1, 4, 1, 3, 4, 1])}
  let sorted = run Vector.sort<i32>(&mut values) |> Effect.provideMut(&mut allocator)
${fingerprint('values', 10, 'folded')}
  return folded`),
    )
    assert.strictEqual(value, 1113444)
  }),
)

it.effect('leaves an already-sorted vector with equal elements unchanged', () =>
  Effect.gen(function* () {
    // Sorting twice must reach the same arrangement: an unstable placement of the equal elements
    // could reorder them on the second pass, which this comparison would catch.
    const value = yield* evaluatedValue(
      'vector-sort/idempotent',
      program(`  let mut values = Vector.make<i32>()
${fill('values', [4, 1, 4, 1, 3, 4, 1])}
  let first = run Vector.sort<i32>(&mut values) |> Effect.provideMut(&mut allocator)
${fingerprint('values', 10, 'firstFold')}
  let second = run Vector.sort<i32>(&mut values) |> Effect.provideMut(&mut allocator)
${fingerprint('values', 10, 'secondFold')}
  if firstFold == secondFold { return 42 }
  return 0`),
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('sorts every integer width the standard library witnesses', () =>
  Effect.gen(function* () {
    const value = yield* evaluatedValue(
      'vector-sort/widths',
      `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.usize as usize
import silk.vector { Vector }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut narrow = Vector.make<u8>()
  let n0 = run Vector.append<u8>(&mut narrow, 9) |> Effect.provideMut(&mut allocator)
  let n1 = run Vector.append<u8>(&mut narrow, 2) |> Effect.provideMut(&mut allocator)
  let n2 = run Vector.append<u8>(&mut narrow, 5) |> Effect.provideMut(&mut allocator)
  let narrowed = run Vector.sort<u8>(&mut narrow) |> Effect.provideMut(&mut allocator)
  let mut wide = Vector.make<i64>()
  let w0 = run Vector.append<i64>(&mut wide, 30) |> Effect.provideMut(&mut allocator)
  let w1 = run Vector.append<i64>(&mut wide, 10) |> Effect.provideMut(&mut allocator)
  let w2 = run Vector.append<i64>(&mut wide, 20) |> Effect.provideMut(&mut allocator)
  let widened = run Vector.sort<i64>(&mut wide) |> Effect.provideMut(&mut allocator)
  let mut counts = Vector.make<usize>()
  let c0 = run Vector.append<usize>(&mut counts, 7) |> Effect.provideMut(&mut allocator)
  let c1 = run Vector.append<usize>(&mut counts, 3) |> Effect.provideMut(&mut allocator)
  let counted = run Vector.sort<usize>(&mut counts) |> Effect.provideMut(&mut allocator)
  if Vector.get<u8>(&narrow, usize.ZERO) == 2 {
    if Vector.get<i64>(&wide, usize.ZERO) == 10 {
      if Vector.get<usize>(&counts, usize.ZERO) == 3 {
        return 42
      }
    }
  }
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 99 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('releases every allocation the sort acquires', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluate('vector-sort/allocations', sortEight)
    if (outcome._tag !== 'Completed') return
    // The scratch element buffer and the two index vectors are acquired and released alongside the
    // vector's own growth, so no element and no allocation leaks out of the sort.
    const acquires = outcome.trace.filter((event) => event._tag === 'AllocationAcquire')
    const releases = outcome.trace.filter((event) => event._tag === 'AllocationRelease')
    assert.strictEqual(acquires.length, releases.length)
    assert.isAbove(acquires.length, 0)
  }),
)

it.effect('finds a present element and reports an absent one', () =>
  Effect.gen(function* () {
    // [9, 2, 7, 2, 5] sorts to [2, 2, 5, 7, 9]: 7 sits at index 3, 4 is absent, and the lowest
    // index of the duplicated 2 is 0.
    const value = yield* evaluatedValue(
      'vector-sort/search',
      program(`  let mut values = Vector.make<i32>()
${fill('values', [9, 2, 7, 2, 5])}
  let sorted = run Vector.sort<i32>(&mut values) |> Effect.provideMut(&mut allocator)
  let hit = Vector.binarySearch<i32>(&values, 7)
  let miss = Vector.binarySearch<i32>(&values, 4)
  let duplicate = Vector.binarySearch<i32>(&values, 2)
  let hitCode = match move hit { Option<usize>.Some { value } => usize.toI32(value) _ => 0 - 1 }
  let missCode = match move miss { Option<usize>.Some { value } => usize.toI32(value) _ => 0 - 1 }
  let duplicateCode = match move duplicate { Option<usize>.Some { value } => usize.toI32(value) _ => 0 - 1 }
  return hitCode * 100 + (missCode + 1) * 10 + duplicateCode`),
    )
    assert.strictEqual(value, 300)
  }),
)

it.effect('reports an absent value for every miss around and beyond the elements', () =>
  Effect.gen(function* () {
    const value = yield* evaluatedValue(
      'vector-sort/search-misses',
      program(`  let mut values = Vector.make<i32>()
${fill('values', [5, 1, 9])}
  let sorted = run Vector.sort<i32>(&mut values) |> Effect.provideMut(&mut allocator)
  let below = Vector.binarySearch<i32>(&values, 0)
  let between = Vector.binarySearch<i32>(&values, 4)
  let above = Vector.binarySearch<i32>(&values, 12)
  let mut score = 0
  let belowCode = match move below { Option<usize>.Some { value } => 1 _ => 0 }
  let betweenCode = match move between { Option<usize>.Some { value } => 1 _ => 0 }
  let aboveCode = match move above { Option<usize>.Some { value } => 1 _ => 0 }
  if belowCode + betweenCode + aboveCode == 0 { return 42 }
  return 0`),
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('searches an empty vector without matching', () =>
  Effect.gen(function* () {
    const value = yield* evaluatedValue(
      'vector-sort/search-empty',
      program(`  let mut values = Vector.make<i32>()
  let missing = Vector.binarySearch<i32>(&values, 3)
  return match move missing { Option<usize>.Some { value } => 0 _ => 42 }`),
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect(
  'sorts to the same output on the evaluator, on Wasm, and on LLVM',
  () =>
    Effect.gen(function* () {
      const wasmSnapshot = yield* Analysis.ofSourceRealized(
        'vector-sort/parity',
        ascii(sortEight),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(wasmSnapshot), [])

      const evaluated = Analysis.evaluate(wasmSnapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, -19473n)

      const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      // Wasm returns i32 as an unsigned 32-bit pattern, so compare the same two's complement bits.
      assert.strictEqual((instance.exports.silk_main as () => number)() | 0, -19473)

      const compiled = yield* Driver.compile({
        compilation: { root: SourceFile.make('vector-sort/parity', ascii(sortEight)) },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        profile: 'release',
        destination: join(destinationRoot, 'parity'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled')
      if (compiled._tag !== 'Compiled') return
      // A process exit status is one unsigned byte, so compare the fingerprint the same way.
      const native = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(native.status, -19473 & 0xff, native.stderr)
    }),
  60_000,
)

/**
 * Stability is only observable when two elements can compare equal yet stay distinguishable. Every
 * conforming type used to be a scalar, whose equality is identity, so this program could not be
 * written until a user type could witness `Order`. `Item` compares on `key` alone and carries a
 * `tag` the comparison never reads, so the arrangement of the equal elements is visible in the
 * result.
 */
const stableUserOrder = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.usize as usize
import silk.order { Order }
import silk.vector { Vector }

struct Item {
  key: i32
  tag: i32
}

fn itemLess(left: &Item, right: &Item) -> bool {
  return left.key < right.key
}

impl Order for Item { lessThan: Item.itemLess }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut items = Vector.make<Item>()
  let a = run Vector.append<Item>(&mut items, Item { key: 1, tag: 1 }) |> Effect.provideMut(&mut allocator)
  let b = run Vector.append<Item>(&mut items, Item { key: 0, tag: 2 }) |> Effect.provideMut(&mut allocator)
  let c = run Vector.append<Item>(&mut items, Item { key: 1, tag: 3 }) |> Effect.provideMut(&mut allocator)
  let d = run Vector.append<Item>(&mut items, Item { key: 0, tag: 4 }) |> Effect.provideMut(&mut allocator)
  let ordered = run Vector.sort<Item>(&mut items) |> Effect.provideMut(&mut allocator)
  let view = Vector.asSlice<Item>(&items)
  let mut folded = 0
  let mut index = usize.ZERO
  while index < Vector.length<Item>(&items) {
    folded = folded * 10 + view[index].tag
    index = index + usize.ONE
  }
  return folded
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 99 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('keeps equal elements of a user type in their input order', () =>
  Effect.gen(function* () {
    // Tags 1..4 carry keys [1, 0, 1, 0]. A stable order yields keys [0, 0, 1, 1] with tags 2, 4,
    // 1, 3 — the two equal-key pairs each in input order. Any unstable placement folds otherwise.
    const value = yield* evaluatedValue('vector-sort/stable-user-order', stableUserOrder)
    assert.strictEqual(value, 2413)
  }),
)

/**
 * A sort over an element type that owns an allocation and is therefore never `Copy`. Nothing in
 * the comparison, the index permutation, or the final bulk move may duplicate or lose one, so the
 * acquire and release counts have to agree exactly.
 */
const moveOnlyElements = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.usize as usize
import silk.order { Order }
import silk.vector { Vector }

struct Tracked {
  key: i32
  payload: Vector<i32>
}

fn trackedLess(left: &Tracked, right: &Tracked) -> bool {
  return left.key < right.key
}

impl Order for Tracked { lessThan: Tracked.trackedLess }

effect fn hold(key: i32) -> Tracked ! OutOfMemoryError ? &mut Allocator {
  let mut payload = Vector.make<i32>()
  let filled = run Vector.append<i32>(&mut payload, key)
  return Tracked { key: key, payload: move payload }
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut items = Vector.make<Tracked>()
  let first = run hold(3) |> Effect.provideMut(&mut allocator)
  let a = run Vector.append<Tracked>(&mut items, move first) |> Effect.provideMut(&mut allocator)
  let second = run hold(1) |> Effect.provideMut(&mut allocator)
  let b = run Vector.append<Tracked>(&mut items, move second) |> Effect.provideMut(&mut allocator)
  let third = run hold(2) |> Effect.provideMut(&mut allocator)
  let c = run Vector.append<Tracked>(&mut items, move third) |> Effect.provideMut(&mut allocator)
  let ordered = run Vector.sort<Tracked>(&mut items) |> Effect.provideMut(&mut allocator)
  let view = Vector.asSlice<Tracked>(&items)
  let mut folded = 0
  let mut index = usize.ZERO
  while index < Vector.length<Tracked>(&items) {
    folded = folded * 10 + view[index].key
    index = index + usize.ONE
  }
  return folded
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 99 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('sorts a move-only element type and releases every allocation it acquires', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluate('vector-sort/move-only-elements', moveOnlyElements)
    if (outcome._tag !== 'Completed') return
    // Keys [3, 1, 2] order to [1, 2, 3]: each element travelled without being duplicated or lost.
    assert.strictEqual(outcome.result.value, 123n)
    const acquires = outcome.trace.filter((event) => event._tag === 'AllocationAcquire')
    const releases = outcome.trace.filter((event) => event._tag === 'AllocationRelease')
    // Each element owns one allocation of its own, so a duplicated element would leak one and a
    // lost element would release one twice. The counts agree only when neither happened.
    assert.isAbove(acquires.length, 0)
    assert.strictEqual(releases.length, acquires.length)
  }),
)
