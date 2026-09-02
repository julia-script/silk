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
 * Both halves of the count are load-bearing, and each fails differently. A value released too few
 * times leaks, which shows as an acquire with no matching release in the evaluator's allocation
 * trace. A value released twice traps rather than completing, so the outcome is `Blocked` — which is
 * why every program here asserts that it *completed* before its counts are read at all.
 *
 * The keys are move-only as well as the values. A `HashMap` is generic over its key, and a symbol
 * table's key is as likely to own storage as its value is.
 */

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyzed = (name: string, source: string, target?: string) =>
  Analysis.ofSourceRealized(name, ascii(source), target)

const messages = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.message)

const describe = (outcome: unknown): string =>
  JSON.stringify(outcome, (_, value) => (typeof value === 'bigint' ? value.toString() : value))

/**
 * A move-only key and a move-only value, each owning one heap block, so every acquisition and every
 * release is one event in the evaluator's allocation trace.
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

const program = (body: string): string =>
  `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
${owners}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
${body}
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 99 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

const codesOf = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)

const allocationEvents = (
  run: ReturnType<typeof Analysis.evaluate>,
): ReadonlyArray<'AllocationAcquire' | 'AllocationRelease'> => {
  if (run._tag !== 'Completed') return []
  return run.trace.flatMap((event) => {
    if (event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease') {
      return [event._tag]
    }
    return []
  })
}

/**
 * Runs one program on the evaluator and Wasm and returns the evaluator's acquire and release counts.
 *
 * The counts are the evaluator's to give — it is the engine that observes every acquisition and
 * every release — while the value the program returns is required of both, so the behaviour the
 * counts describe is the behaviour every engine has.
 */
const owned = (name: string, source: string) =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(name, source, 'wasm32-unknown-unknown')
    assert.deepEqual(messages(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    // A value released a second time traps instead of completing, so this is what fails first if
    // the map ever releases something it already handed away.
    assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
    const bootstrap = evaluated._tag === 'Completed' ? Number(evaluated.result.value) : undefined
    const events = allocationEvents(evaluated)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    const direct = (instance.exports.silk_main as () => number)()

    return Object.freeze({
      bootstrap,
      direct,
      acquired: events.filter((event) => event === 'AllocationAcquire').length,
      released: events.filter((event) => event === 'AllocationRelease').length,
    })
  })

/** Asserts one answer on both engines, and that nothing the evaluator saw acquired went unreleased. */
const balanced = (
  outcome: {
    bootstrap: unknown
    direct: unknown
    acquired: number
    released: number
  },
  expected: number,
  acquired: number,
) => {
  assert.strictEqual(outcome.bootstrap, expected, 'bootstrap evaluator')
  assert.strictEqual(outcome.direct, expected, 'direct WebAssembly')
  // The expected acquire count is stated rather than derived, so an insert path that stopped taking
  // ownership at all — which would balance trivially — fails here instead of passing quietly.
  assert.strictEqual(outcome.acquired, acquired, 'allocations acquired')
  assert.strictEqual(outcome.released, outcome.acquired, 'acquires equal releases')
}

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

it.effect('transfers a move-only value out through callback-scoped mutation', () =>
  Effect.gen(function* () {
    const outcome = yield* owned(
      'hashed-ownership/callback-extraction',
      program(`  let mut map = HashMap.make<Handle, Cell>(Hash.seed(3))
  let key = run handle(7) |> Effect.provideMut(&mut allocator)
  let value = run held(42) |> Effect.provideMut(&mut allocator)
  let cell = Cell { slot: Filled { payload: move value } }
  let inserted = run HashMap.insert<Handle, Cell>(&mut map, move key, move cell)
    |> Effect.provideMut(&mut allocator)
  drop inserted
  let probe = run handle(7) |> Effect.provideMut(&mut allocator)
  let mut captured = Capture { slot: Empty {} }
  let found = HashMap.withMut(&mut map, move probe, extractInto(&mut captured))
  if !found { return 1 }
  let answer = match move captured {
    Capture { slot } => match move slot {
      Empty {} => 0
      Filled { payload } => tagOf(move payload)
    }
  }
  if HashMap.length<Handle, Cell>(&map) != 1 { return 2 }
  return answer`),
    )
    balanced(outcome, 42, 5)
  }),
)

it.effect('releases every owned key and value when a non-empty map is dropped', () =>
  Effect.gen(function* () {
    // Nothing is removed and the map is never emptied: it goes out of scope holding three keys and
    // three values, and its own two buffers. Eight acquisitions, eight releases, and the releases
    // of the keys and values happen because the map's drop walks its occupied slots — not because
    // anything the program wrote released them.
    const outcome = yield* owned(
      'hashed-ownership/drop-non-empty',
      program(`  let mut map = HashMap.make<Handle, Held>(Hash.seed(3))
  let mut index = 0
  while index < 3 {
    let key = run handle(index) |> Effect.provideMut(&mut allocator)
    let value = run held(index + 10) |> Effect.provideMut(&mut allocator)
    let previous = run HashMap.insert<Handle, Held>(&mut map, move key, move value)
      |> Effect.provideMut(&mut allocator)
    drop previous
    index = index + 1
  }
  if HashMap.length<Handle, Held>(&map) != 3 { return 1 }
  return 42`),
    )
    balanced(outcome, 42, 8)
  }),
)

it.effect('releases the replaced value and the replaced key when an overwrite lands', () =>
  Effect.gen(function* () {
    // Two keys of one tag, so the second insert finds the first entry. The value it displaces
    // comes back to the caller, who releases it; the key the map held is released by the map. The
    // map then drops holding the second pair.
    //
    // Two keys, two values, two buffers: six acquisitions. A replaced value the map forgot to hand
    // back and did not release would leave five.
    const outcome = yield* owned(
      'hashed-ownership/overwrite',
      program(`  let mut map = HashMap.make<Handle, Held>(Hash.seed(3))
  let firstKey = run handle(7) |> Effect.provideMut(&mut allocator)
  let firstValue = run held(11) |> Effect.provideMut(&mut allocator)
  let none = run HashMap.insert<Handle, Held>(&mut map, move firstKey, move firstValue)
    |> Effect.provideMut(&mut allocator)
  drop none
  let secondKey = run handle(7) |> Effect.provideMut(&mut allocator)
  let secondValue = run held(31) |> Effect.provideMut(&mut allocator)
  let displaced = run HashMap.insert<Handle, Held>(&mut map, move secondKey, move secondValue)
    |> Effect.provideMut(&mut allocator)
  if HashMap.length<Handle, Held>(&map) != 1 { return 1 }
  let replaced = match move displaced {
    Option<Held>.Some { value } => tagOf(move value)
    Option<Held>.None => 0
  }
  if replaced != 11 { return 2 }
  return 42`),
    )
    balanced(outcome, 42, 6)
  }),
)

it.effect('transfers a removed value out and releases the key the map held', () =>
  Effect.gen(function* () {
    // The probe key is a third owner: `remove` consumes it, so the map releases the key it held
    // and the probe key is released too, while the value travels to the caller intact.
    //
    // Three keys — two stored, one probing — two values, two buffers: seven acquisitions.
    const outcome = yield* owned(
      'hashed-ownership/remove',
      program(`  let mut map = HashMap.make<Handle, Held>(Hash.seed(3))
  let firstKey = run handle(4) |> Effect.provideMut(&mut allocator)
  let firstValue = run held(20) |> Effect.provideMut(&mut allocator)
  let noFirst = run HashMap.insert<Handle, Held>(&mut map, move firstKey, move firstValue)
    |> Effect.provideMut(&mut allocator)
  drop noFirst
  let secondKey = run handle(9) |> Effect.provideMut(&mut allocator)
  let secondValue = run held(22) |> Effect.provideMut(&mut allocator)
  let noSecond = run HashMap.insert<Handle, Held>(&mut map, move secondKey, move secondValue)
    |> Effect.provideMut(&mut allocator)
  drop noSecond
  let probe = run handle(4) |> Effect.provideMut(&mut allocator)
  let taken = HashMap.remove<Handle, Held>(&mut map, move probe)
  if HashMap.length<Handle, Held>(&map) != 1 { return 1 }
  let carried = match move taken {
    Option<Held>.Some { value } => tagOf(move value)
    Option<Held>.None => 0
  }
  // The removed value is the caller's now, and the map still owns the entry it kept.
  if carried != 20 { return 2 }
  return 42`),
    )
    balanced(outcome, 42, 7)
  }),
)

it.effect('releases every owned key and value carried through a growth', () =>
  Effect.gen(function* () {
    // Ten entries take the map past its first table, so every key and value is moved once by the
    // rehash. A rehome that copied instead of moving would double the releases and trap; one that
    // dropped an entry on the floor would leave an acquisition unmatched.
    //
    // Ten keys, ten values, and two buffers for each of the tables of eight and sixteen: twenty-four.
    const outcome = yield* owned(
      'hashed-ownership/growth',
      program(`  let mut map = HashMap.make<Handle, Held>(Hash.seed(3))
  let mut index = 0
  while index < 10 {
    let key = run handle(index) |> Effect.provideMut(&mut allocator)
    let value = run held(index) |> Effect.provideMut(&mut allocator)
    let previous = run HashMap.insert<Handle, Held>(&mut map, move key, move value)
      |> Effect.provideMut(&mut allocator)
    drop previous
    index = index + 1
  }
  if HashMap.length<Handle, Held>(&map) != 10 { return 1 }
  return 42`),
    )
    balanced(outcome, 42, 24)
  }),
)
