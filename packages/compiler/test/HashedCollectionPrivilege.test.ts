import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Intrinsic from '../src/Intrinsic.js'
import type * as Mir from '../src/Mir.js'

/**
 * No engine contains a hash operation.
 *
 * `HashMap` is ordinary library source over `Allocator` and typed storage, and every hash a program
 * computes is a call to a function some witness declared in Silk. That is a constraint on the
 * compiler rather than on the collections, so it is checked where a violation would appear: in the
 * catalogue of operations the compiler provides, and in the MIR of a program that really uses a map.
 *
 * The catalogue check is the wider of the two. An engine can only gain a primitive by an intrinsic
 * being declared, and semantic analysis, the HIR, the MIR, the evaluator, the native backend and the
 * WebAssembly backend all draw from that one catalogue — so a catalogue with no hash in it is six
 * statements at once, and it would fail on the first commit that added one anywhere.
 */

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyzed = (name: string, source: string) => Analysis.ofSourceRealized(name, ascii(source))

const messages = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.message)

/** A program that inserts, grows, looks up and removes, so the whole map is lowered, not a corner. */
const usingAMap = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.i32 as i32
import silk.hash as Hash
import silk.hash { HashKey, HashSeed, Word }
import silk.hash_map { HashMap, contains, get, insert, length, make, remove }
import silk.option { Option, Some, None }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut map = make<Word, i32>(Hash.seed(12345))
  let mut key = 0
  while key < 20 {
    let previous = run insert<Word, i32>(&mut map, Hash.word(i32.toU64(key)), key)
      |> Effect.provideMut(&mut allocator)
    drop previous
    key = key + 1
  }
  let taken = remove<Word, i32>(&mut map, Hash.word(3))
  let removed = Option.unwrapOr<i32>(move taken, -1)
  if removed != 3 { return 1 }
  if !contains<Word, i32>(&map, Hash.word(4)) { return 2 }
  let held = Option.unwrapOr<i32>(get<Word, i32>(&map, Hash.word(4)), -1)
  if held != 4 { return 3 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 99 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

/** Every operation of every region of every lowered function, in one flat sequence. */
const operationsOf = (module: Mir.Module): ReadonlyArray<Mir.Operation> =>
  module.functions.flatMap((lowered) =>
    lowered.regions.flatMap((region) => {
      if (region._tag === 'OperationRegion') return region.operations
      if (region._tag === 'CleanupRegion') return region.releases
      return []
    }),
  )

/** A name that would betray a hash operation whatever it was called. */
const hashing = /hash|digest|fnv|murmur|siphash|checksum|crc/i

it.effect('lowers a HashMap program to a MIR that names no hash operation', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed('hashed-privilege/map', usingAMap)
    assert.deepEqual(messages(snapshot), [])

    // The program really runs, so the MIR examined below is the MIR of a working map rather than of
    // something that happened to lower.
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      JSON.stringify(evaluated, (_, value) =>
        typeof value === 'bigint' ? value.toString() : value,
      ),
    )
    assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 42n)

    const mir = Analysis.mirOf(snapshot)
    assert.strictEqual(mir._tag, 'Available')
    if (mir._tag !== 'Available') return

    const operations = operationsOf(mir.value)
    assert.isAbove(operations.length, 0, 'the map lowered to something')
    const kinds = [...new Set(operations.map((operation) => operation._tag))].sort()
    assert.deepEqual(
      kinds.filter((kind) => hashing.test(kind)),
      [],
      `MIR operations used: ${kinds.join(', ')}`,
    )
  }),
)

it.effect('computes every hash as an ordinary call to a witness’s own Silk function', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed('hashed-privilege/map', usingAMap)
    assert.deepEqual(messages(snapshot), [])
    const mir = Analysis.mirOf(snapshot)
    assert.strictEqual(mir._tag, 'Available')
    if (mir._tag !== 'Available') return

    // The witness is a function of the standard library's own module, lowered like any other Silk
    // function — so it has a body in the MIR rather than being a name the backend recognizes.
    const lowered = mir.value.functions.map((fn) => `${fn.id.module}.${fn.id.name}`)
    assert.include(lowered, 'silk/hash.wordHash', `lowered functions: ${lowered.join(', ')}`)
    assert.include(lowered, 'silk/hash.mix')

    // And it is reached the ordinary way: a `Call` naming that declaration, not an operation of its
    // own. This is the positive half of the constraint — the hash exists, and it is Silk.
    const calls = operationsOf(mir.value).flatMap((operation) =>
      operation._tag === 'Call' ? [`${operation.target.module}.${operation.target.name}`] : [],
    )
    assert.include(calls, 'silk/hash.wordHash')

    // The collection itself is library source too, lowered from `hash_map.silk`.
    assert.isTrue(
      lowered.some((name) => name.startsWith('silk/hash_map.')),
      `no HashMap function was lowered: ${lowered.join(', ')}`,
    )
  }),
)

it('provides no hash operation to any engine', () => {
  // Every intrinsic the compiler declares, by the spelling a program would write. An engine cannot
  // implement an operation the catalogue does not name, so this one assertion covers analysis, the
  // HIR, the MIR, the evaluator, and both backends.
  const spellings = Intrinsic.all().flatMap((actor) =>
    actor.operations.map((operation) => `${actor.spelling}.${operation.spelling}`),
  )
  assert.isAbove(spellings.length, 0, 'the catalogue was read')
  assert.deepEqual(
    spellings.filter((spelling) => hashing.test(spelling)),
    [],
  )
})

it.effect('treats HashKey as an ordinary interface rather than by its spelling', () =>
  Effect.gen(function* () {
    // An interface of the same two operations, declared by a program under a name nothing could
    // recognize, behaves exactly as the standard library's does. If any phase special-cased the
    // spelling `HashKey`, this program and the one above would not agree.
    const snapshot = yield* analyzed(
      'hashed-privilege/renamed',
      `import silk.hash as Hash
import silk.hash { HashKey }
import silk.u64 as u64
import silk.hash as Hash
import silk.hash { HashSeed }

interface Whatever {
  fn equals(left: &Self, right: &Self) -> bool
  fn hash(value: &Self, seed: &HashSeed) -> u64
}

struct Tag { value: u64 }

fn tagEquals(left: &Tag, right: &Tag) -> bool { return left.value == right.value }
fn tagHash(value: &Tag, seed: &HashSeed) -> u64 { return Hash.mix(seed, value.value) }

impl Whatever for Tag { equals: Tag.tagEquals hash: Tag.tagHash }

fn hashOf<T: Whatever>(value: T, seed: HashSeed) -> u64 { return Whatever.hash(&value, &seed) }

pub fn main() -> i32 {
  let seed = Hash.seed(12345)
  let mine = hashOf<Tag>(Tag { value: 20 }, Hash.seed(12345))
  let stock = Hash.mix(&seed, u64.toU64(20))
  if mine != stock { return 1 }
  return 42
}`,
    )
    assert.deepEqual(messages(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 42n)
  }),
)
