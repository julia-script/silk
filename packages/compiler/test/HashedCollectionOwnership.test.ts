import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import { unreachable } from './support/raise.js'

/**
 * A hashed collection releases the move-only keys and values it owns exactly once — on removal, on
 * an overwrite under an equivalent key, and on its own drop while still non-empty.
 *
 * The last two are the ones that get missed. Removal is where a map's ownership is thought about,
 * because something visibly leaves; an overwrite quietly destroys a value nobody asked about, and a
 * map dropped without being emptied destroys everything at once through a path a passing test never
 * takes.
 *
 * The structural ownership assertions prove that every owned key and value receives one cleanup;
 * runtime outcomes are pinned separately in the native corpus.
 *
 * The keys are move-only as well as the values. A `HashMap` is generic over its key, and a symbol
 * table's key is as likely to own storage as its value is.
 */

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyzed = (name: string, source: string, target?: string) =>
  Analysis.ofSourceRealized(name, ascii(source), target)

/**
 * A move-only key and a move-only value each own one heap block, making their cleanup plans explicit.
 *
 * The key's tag is an `i32` widened by its witness rather than a `u64` held directly, because an
 * effect function that returns a struct carrying a 64-bit field currently fails WebAssembly
 * emission — a backend defect with nothing to do with hashing, reported on #34 with a reproduction
 * that names no collection. Widening in the witness sidesteps it without weakening anything here:
 * the key still owns storage, is still move-only, and its hash still folds through the same mixer.
 *
 * The key's hash is folded from its tag through the standard library's own mixer, and its
 * equivalence compares the same tag — so two keys are equivalent exactly when they hash alike, which
 * is what `HashKey` requires of a witness.
 */
const owners = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.i32 as i32
import silk.layout { Layout }
import silk.hash { Hash }
import silk.hash { HashKey, HashSeed }
import silk.hash_map { HashMap }
import silk.option { Option }

struct Handle {
  tag: i32
  storage: Allocation
}

struct Held {
  tag: i32
  storage: Allocation
}

struct Empty {}

struct Filled { payload: Held }

struct Cell { slot: Empty | Filled }

struct Capture { slot: Empty | Filled }

fn handleEquals(left: &Handle, right: &Handle) -> bool { return left.tag == right.tag }

fn handleHash(value: &Handle, seed: &HashSeed) -> u64 {
  return Hash.mix(seed, i32.toU64(value.tag))
}

impl HashKey for Handle { equals: Handle.handleEquals hash: Handle.handleHash }

effect fn handle(tag: i32) -> Handle ! OutOfMemoryError ? &mut Allocator {
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout)
  let block = run recipe
  return Handle { tag: tag, storage: move block }
}

effect fn held(tag: i32) -> Held ! OutOfMemoryError ? &mut Allocator {
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout)
  let block = run recipe
  return Held { tag: tag, storage: move block }
}

fn tagOf(value: Held) -> i32 {
  return match move value {
    Held { tag, storage } => release(tag, move storage)
  }
}

fn release(tag: i32, storage: Allocation) -> i32 {
  drop storage
  return tag
}

fn extractInto(cell: &mut Cell, output: &mut Capture) -> () {
  let previous = Intrinsic.replace(cell.slot, Empty {})
  output.slot = move previous
  return ()
}`

const codesOf = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)

it.effect('rejects a value borrow returned from the mutation callback', () =>
  Effect.gen(function* () {
    const source = `${owners}
import silk.hash { Word }
struct Box { value: i32 }
fn expose(value: &mut Box) -> &mut Box { return move value }
fn escaped(map: &mut HashMap<Word, Box>, key: Word) -> bool {
  return HashMap.withMut(move map, move key, expose)
}
pub fn main() -> i32 { return 0 }`
    const snapshot = yield* analyzed('hashed-ownership/callback-escape', source)
    const diagnostics = Analysis.diagnostics(snapshot)
    assert.deepEqual(codesOf(snapshot), ['SEM0052'])
    const reason = (diagnostics.at(0) ?? unreachable('expected one diagnostic')).reason
    assert.strictEqual(reason._tag, 'TypeArgumentInference')
    if (reason._tag !== 'TypeArgumentInference') return
    assert.strictEqual(reason.target, 'HashMap.withMut')
  }),
)

it.effect('rejects a callback that parks while holding the value borrow', () =>
  Effect.gen(function* () {
    const source = `${owners}
import silk.execution { Execution }
import silk.hash { Word }
struct Guard {}
struct Box { value: i32 }
fn register(wake: Intrinsic.Wake) -> Guard { drop wake return Guard {} }
fn parking(value: &mut Box) -> () {
  let parked = run Execution.park(register)
  value.value = value.value
  return ()
}
pub fn main() -> i32 {
  let mut map = HashMap.make<Word, Box>(Hash.seed(9))
  let attempted = HashMap.withMut(&mut map, Hash.word(1), parking)
  drop attempted
  return 0
}
`
    const snapshot = yield* analyzed('hashed-ownership/callback-parking', source)
    const diagnostics = Analysis.diagnostics(snapshot)
    assert.deepEqual(codesOf(snapshot), ['SEM0139'])
    const reason = (diagnostics.at(0) ?? unreachable('expected one diagnostic')).reason
    assert.strictEqual(reason._tag, 'UnsatisfiedExecutableProperty')
    if (reason._tag !== 'UnsatisfiedExecutableProperty') return
    assert.strictEqual(reason.property, 'Intrinsic.NonParking')
  }),
)
