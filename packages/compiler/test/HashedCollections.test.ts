import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

/**
 * `HashMap` and `HashSet` as a program uses them: insert, lookup, removal, the equivalence deciding
 * which entries are one entry, growth that keeps every entry, and a failed growth that keeps the map.
 *
 * Every claim is asserted as a value the program returns, not as a compilation that finished, and
 * the load-bearing ones are asserted on the bootstrap evaluator and the direct WebAssembly backend.
 * Native agreement is proven once by the differential corpus (`support/corpus.ts`), which carries a
 * representative hashed-collection growth program.
 */

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyzed = (name: string, source: string, target?: string) =>
  Analysis.ofSourceRealized(name, ascii(source), target)

const messages = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.message)

/** Evaluated scalars can be BigInt, which plain JSON serialization refuses. */
const describe = (outcome: unknown): string =>
  JSON.stringify(outcome, (_, value) => (typeof value === 'bigint' ? value.toString() : value))

const evaluatedValue = (name: string, source: string) =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(name, source)
    assert.deepEqual(messages(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    return outcome._tag === 'Completed' ? Number(outcome.result.value) : undefined
  })

/** Runs one source on the bootstrap evaluator and the direct WebAssembly backend. */
const twoEngineValue = (name: string, source: string) =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(name, source, 'wasm32-unknown-unknown')
    assert.deepEqual(messages(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
    const bootstrap = evaluated._tag === 'Completed' ? Number(evaluated.result.value) : undefined

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    const direct = (instance.exports.silk_main as () => number)()

    return Object.freeze({ bootstrap, direct })
  })

/** Asserts one value on both engines, so a divergence names the engine that drifted. */
const agrees = (outcome: { bootstrap: unknown; direct: unknown }, expected: number) => {
  assert.strictEqual(outcome.bootstrap, expected, 'bootstrap evaluator')
  assert.strictEqual(outcome.direct, expected, 'direct WebAssembly')
}

const mapImports = `import silk.hash { HashKey, HashSeed, Word }
import silk.hash_map {
  HashMap,
  bucketCount,
  contains,
  get,
  indexOf,
  insert,
  keyAt,
  length,
  make,
  occupiedAt,
  remove,
  valueAt
}
import silk.option { Option, Some, None }`

/** Wraps a body in the allocator the collections require and the recovery an `OutOfMemoryError` needs. */
const program = (imports: string, body: string): string =>
  `${imports}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
${body}
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 99 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('inserts, looks up, and removes on both engines', () =>
  Effect.gen(function* () {
    // The answer is assembled from the values the map returns — 20 removed plus 22 still held —
    // so a map that lost an entry, kept a removed one, or answered the wrong value cannot reach 42.
    const outcome = yield* twoEngineValue(
      'hashed-collections/insert-lookup-remove',
      program(
        mapImports,
        `  let mut map = make<Word, i32>(HashKey.seed(12345))
  let first = run insert<Word, i32>(&mut map, HashKey.word(7), 20) |> Effect.provideMut(&mut allocator)
  let second = run insert<Word, i32>(&mut map, HashKey.word(9), 22) |> Effect.provideMut(&mut allocator)
  drop first
  drop second
  if length<Word, i32>(&map) != 2 { return 1 }
  if !contains<Word, i32>(&map, HashKey.word(7)) { return 2 }
  if contains<Word, i32>(&map, HashKey.word(11)) { return 3 }
  let taken = remove<Word, i32>(&mut map, HashKey.word(7))
  let removed = Option.unwrapOr<i32>(move taken, 0)
  if length<Word, i32>(&map) != 1 { return 4 }
  if contains<Word, i32>(&map, HashKey.word(7)) { return 5 }
  let missing = remove<Word, i32>(&mut map, HashKey.word(7))
  let absent = Option.unwrapOr<i32>(move missing, 0)
  if absent != 0 { return 6 }
  let held = Option.unwrapOr<i32>(get<Word, i32>(&map, HashKey.word(9)), 0)
  return removed + held`,
      ),
    )
    agrees(outcome, 42)
  }),
)

it.effect('reaches one entry from two equivalent keys, and replaces rather than duplicating', () =>
  Effect.gen(function* () {
    // `Word` is equivalent by its integer, so two separately constructed keys of one integer are
    // two values and one key. The map must treat them as one entry in both directions.
    const value = yield* evaluatedValue(
      'hashed-collections/equivalent-keys',
      program(
        mapImports,
        `  let mut map = make<Word, i32>(HashKey.seed(7))
  let first = run insert<Word, i32>(&mut map, HashKey.word(3), 11) |> Effect.provideMut(&mut allocator)
  drop first
  // A second key equivalent to the first finds the entry the first placed.
  if !contains<Word, i32>(&map, HashKey.word(3)) { return 1 }
  let second = run insert<Word, i32>(&mut map, HashKey.word(3), 31) |> Effect.provideMut(&mut allocator)
  let replaced = Option.unwrapOr<i32>(move second, 0)
  if length<Word, i32>(&map) != 1 { return 2 }
  if replaced != 11 { return 3 }
  let held = Option.unwrapOr<i32>(get<Word, i32>(&map, HashKey.word(3)), 0)
  if held != 31 { return 4 }
  return replaced + held`,
      ),
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('keeps every entry across the growth that rehomes them', () =>
  Effect.gen(function* () {
    // Forty entries take the map through three growths. Each is read back at its own key and the
    // values are totalled, so an entry lost to a rehash, rehomed under the wrong hash, or left
    // behind in the old table shows up as a wrong total rather than as a smaller length.
    const outcome = yield* twoEngineValue(
      'hashed-collections/growth',
      program(
        mapImports,
        `  let mut map = make<Word, i32>(HashKey.seed(4242))
  let mut key = 0
  while key < 40 {
    let previous = run insert<Word, i32>(&mut map, HashKey.word(i32.toU64(key)), key * 3)
      |> Effect.provideMut(&mut allocator)
    drop previous
    key = key + 1
  }
  if length<Word, i32>(&map) != 40 { return 1 }
  if bucketCount<Word, i32>(&map) <= 40 { return 2 }
  let mut probe = 0
  let mut total = 0
  while probe < 40 {
    let found = Option.unwrapOr<i32>(get<Word, i32>(&map, HashKey.word(i32.toU64(probe))), -1)
    if found != probe * 3 { return 3 }
    total = total + found
    probe = probe + 1
  }
  // Three times the sum of nought to thirty-nine.
  if total != 2340 { return 4 }
  return 42`,
      ),
    )
    agrees(outcome, 42)
  }),
)

it.effect('leaves the map intact when the growth allocation fails', () =>
  Effect.gen(function* () {
    // A provider with room for exactly the first table's two buffers. The seventh insert is the one
    // that needs growth, and it fails; every entry placed before it must still answer at its key.
    const value = yield* evaluatedValue(
      'hashed-collections/failed-growth',
      `${mapImports}

struct Budget {
  inner: SystemAllocator
  remaining: usize
}

effect fn allocate(self: &mut Budget, layout: Layout) -> Allocation ! OutOfMemoryError {
  if self.remaining == usize.ZERO {
    fail OutOfMemoryError {}
  }
  self.remaining = self.remaining - usize.ONE
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut self.inner)
  return run pending
}

impl Allocator for Budget { allocate: Budget.allocate }

effect fn grow(map: &mut HashMap<Word, i32>) -> i32 ! OutOfMemoryError ? &mut Allocator {
  let previous = run insert<Word, i32>(move map, HashKey.word(6), 106)
  drop previous
  return 0
}

effect fn noRoom(error: OutOfMemoryError) -> i32 { return 1 }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Budget { inner: SystemAllocator.make(), remaining: 2 }
  let mut map = make<Word, i32>(HashKey.seed(5))
  let mut key = 0
  while key < 6 {
    let previous = run insert<Word, i32>(&mut map, HashKey.word(i32.toU64(key)), key + 100)
      |> Effect.provideMut(&mut allocator)
    drop previous
    key = key + 1
  }
  if length<Word, i32>(&map) != 6 { return 2 }
  let refused = run Effect.catchAll(grow(&mut map), noRoom) |> Effect.provideMut(&mut allocator)
  if refused != 1 { return 3 }
  // The map is exactly as it was: the same entries, the same length, the same bucket count.
  if length<Word, i32>(&map) != 6 { return 4 }
  if bucketCount<Word, i32>(&map) != 8 { return 5 }
  if contains<Word, i32>(&map, HashKey.word(6)) { return 6 }
  let mut probe = 0
  let mut total = 0
  while probe < 6 {
    let found = Option.unwrapOr<i32>(get<Word, i32>(&map, HashKey.word(i32.toU64(probe))), -1)
    if found != probe + 100 { return 7 }
    total = total + found
    probe = probe + 1
  }
  // Nought to five, each raised by a hundred.
  if total != 615 { return 8 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 99 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('keeps probing through the marks a removal leaves behind', () =>
  Effect.gen(function* () {
    // The classic failure of a linear probe: a removal that vacates its slot outright cuts the run
    // that carried later keys past it, and they become unreachable while still being held. Six
    // rounds of filling and emptying, with the keys shifted each round so a run has to cross the
    // marks the last round left, and the map is asked for every key it should still hold.
    const value = yield* evaluatedValue(
      'hashed-collections/tombstones',
      program(
        mapImports,
        `  let mut map = make<Word, i32>(HashKey.seed(31))
  let mut round = 0
  while round < 6 {
    let mut key = 0
    while key < 6 {
      let placed = run insert<Word, i32>(&mut map, HashKey.word(i32.toU64(round * 6 + key)), round * 6 + key)
        |> Effect.provideMut(&mut allocator)
      drop placed
      key = key + 1
    }
    // Every key of this round must be present while the previous rounds' marks are still there.
    let mut check = 0
    while check < 6 {
      let found = Option.unwrapOr<i32>(get<Word, i32>(&map, HashKey.word(i32.toU64(round * 6 + check))), -1)
      if found != round * 6 + check { return 1 }
      check = check + 1
    }
    if length<Word, i32>(&map) != 6 { return 2 }
    // Empty it again, leaving six fresh marks for the next round to probe across.
    let mut gone = 0
    while gone < 6 {
      let taken = remove<Word, i32>(&mut map, HashKey.word(i32.toU64(round * 6 + gone)))
      let removed = Option.unwrapOr<i32>(move taken, -1)
      if removed != round * 6 + gone { return 3 }
      gone = gone + 1
    }
    if length<Word, i32>(&map) != usize.ZERO { return 4 }
    round = round + 1
  }
  return 42`,
      ),
    )
    assert.strictEqual(value, 42)
  }),
)

const setImports = `import silk.hash { HashKey, HashSeed, Word }
import silk.hash_set {
  HashSet,
  bucketCount,
  contains,
  elementAt,
  insert,
  length,
  make,
  occupiedAt,
  remove
}
import silk.option { Option, Some, None }`

it.effect(
  'refuses a second equivalent element, and answers membership before and after removal',
  () =>
    Effect.gen(function* () {
      const outcome = yield* twoEngineValue(
        'hashed-collections/set',
        program(
          setImports,
          `  let mut seen = make<Word>(HashKey.seed(99))
  let first = run insert<Word>(&mut seen, HashKey.word(7)) |> Effect.provideMut(&mut allocator)
  let second = run insert<Word>(&mut seen, HashKey.word(9)) |> Effect.provideMut(&mut allocator)
  let again = run insert<Word>(&mut seen, HashKey.word(7)) |> Effect.provideMut(&mut allocator)
  if first { return 1 }
  if second { return 2 }
  // The duplicate reports that the element was already held, and the set does not grow.
  if !again { return 3 }
  if length<Word>(&seen) != 2 { return 4 }
  if !contains<Word>(&seen, HashKey.word(9)) { return 5 }
  let taken = remove<Word>(&mut seen, HashKey.word(9))
  let gone = match move taken {
    Some<Word> { value } => u64.toI32(value.value)
    None {} => 0
  }
  if gone != 9 { return 6 }
  if contains<Word>(&seen, HashKey.word(9)) { return 7 }
  if length<Word>(&seen) != 1 { return 8 }
  if !contains<Word>(&seen, HashKey.word(7)) { return 9 }
  return gone * 4 + 6`,
        ),
      )
      agrees(outcome, 42)
    }),
)

it.effect('refuses a key type that has no HashKey witness', () =>
  Effect.gen(function* () {
    // The bound is what makes a hashed collection safe to instantiate: a key with no witness has no
    // hash and no equivalence, and the instantiation is reported rather than accepted.
    const snapshot = yield* analyzed(
      'hashed-collections/no-witness',
      `${mapImports}

struct Unhashed { tag: i32 }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let mut map = make<Unhashed, i32>(HashKey.seed(1))
  let placed = run insert<Unhashed, i32>(&mut map, Unhashed { tag: 1 }, 2)
    |> Effect.provideMut(&mut allocator)
  drop placed
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 99 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    )
    assert.deepEqual(messages(snapshot), [
      'Invalid conformance: hashed-collections/no-witness.Unhashed does not implement HashKey',
    ])
  }),
)
