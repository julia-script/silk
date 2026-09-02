import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Json from './support/Json.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const llvmOperationNames = (ir: string): ReadonlyArray<string> =>
  ir.split('\n').flatMap((line) => {
    const operation = line
      .trim()
      .match(/^(?:[%@][-a-zA-Z0-9$._]+\s*=\s*)?([a-z][a-z0-9_.-]*)\b/)
      ?.at(1)
    return operation === undefined ? [] : [operation]
  })

const watOperationNames = (wat: string): ReadonlyArray<string> =>
  [...wat.matchAll(/\(\s*([a-z][a-z0-9_.-]*)\b/g)].flatMap((match) => {
    const operation = match.at(1)
    return operation === undefined ? [] : [operation]
  })

/**
 * The first useful owned sequence written entirely in Silk: six appends force two geometric
 * growths (0 -> 4 -> 8) with element migration, then checked reads observe both ends.
 */
const growth = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.vector { Vector }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<i32>()
  let pending0 = Vector.append<i32>(&mut values, 10) |> Effect.provideMut(&mut allocator)
  let appended0 = run pending0
  let pending1 = Vector.append<i32>(&mut values, 11) |> Effect.provideMut(&mut allocator)
  let appended1 = run pending1
  let pending2 = Vector.append<i32>(&mut values, 12) |> Effect.provideMut(&mut allocator)
  let appended2 = run pending2
  let pending3 = Vector.append<i32>(&mut values, 13) |> Effect.provideMut(&mut allocator)
  let appended3 = run pending3
  let pending4 = Vector.append<i32>(&mut values, 14) |> Effect.provideMut(&mut allocator)
  let appended4 = run pending4
  let pending5 = Vector.append<i32>(&mut values, 15) |> Effect.provideMut(&mut allocator)
  let appended5 = run pending5
  if Vector.length<i32>(&values) == 6 {} else { return 0 }
  if Vector.capacity<i32>(&values) == 8 {} else { return 1 }
  let first = Vector.get<i32>(&values, 0)
  let last = Vector.get<i32>(&values, 5)
  return first + last + 17
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'grows, reads, and releases a Silk-written vector on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'vector-acceptance/growth',
        ascii(growth),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', Json.stringify(evaluated))
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)
      // Two growths acquire two buffers; the migration and the final Drop hook release both,
      // and every acquire pairs with exactly one release.
      const acquires = evaluated.trace.filter((event) => event._tag === 'AllocationAcquire')
      const releases = evaluated.trace.filter((event) => event._tag === 'AllocationRelease')
      assert.strictEqual(acquires.length, 2)
      assert.strictEqual(releases.length, 2)
      // The vector's parametric Drop hook ran during cleanup.
      assert.isTrue(
        evaluated.trace.some(
          (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'),
        ),
      )

      // No vector-shaped operation exists in MIR: only the substrate's own vocabulary appears.
      const operations = new Set(
        Analysis.loweredMir(snapshot).functions.flatMap((fn) =>
          MirVerification.operations(fn).map((operation) => operation._tag),
        ),
      )
      assert.isFalse([...operations].some((tag) => tag.toLowerCase().includes('vector')))
      assert.isFalse(evaluated.trace.some((event) => event._tag.toLowerCase().includes('vector')))

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      assert.isFalse(
        watOperationNames(wasm.wat).some((operation) => operation.toLowerCase().includes('vector')),
      )
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

      const nativeSnapshot = yield* Analysis.ofSourceRealized(
        'vector-acceptance/growth',
        ascii(growth),
        'aarch64-apple-darwin',
      )
      assert.deepEqual(Analysis.diagnostics(nativeSnapshot), [])
      const llvm = yield* Analysis.codegen(nativeSnapshot, { mode: 'release' })
      assert.isFalse(
        llvmOperationNames(llvm.ir).some((operation) => operation.toLowerCase().includes('vector')),
      )
    }),
  60_000,
)

const lexicalViews = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.vector { Vector }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<i32>()
  let empty = Vector.asSlice<i32>(&values)
  if empty.length == 0 {} else { return 1 }
  let pending0 = Vector.append<i32>(&mut values, 10) |> Effect.provideMut(&mut allocator)
  let appended0 = run pending0
  let pending1 = Vector.append<i32>(&mut values, 12) |> Effect.provideMut(&mut allocator)
  let appended1 = run pending1
  let mut writable = Vector.asMutSlice<i32>(&mut values)
  writable[1] = 32
  let readable = Vector.asSlice<i32>(&values)
  return readable[0] + readable[1]
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'returns shared and exclusive Vector views on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const wasmSnapshot = yield* Analysis.ofSourceRealized(
        'vector-acceptance/lexical-views',
        ascii(lexicalViews),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(wasmSnapshot), [])
      const evaluated = Analysis.evaluate(wasmSnapshot)
      assert.strictEqual(evaluated._tag, 'Completed', Json.stringify(evaluated))
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)
      assert.isTrue(
        Analysis.loweredMir(wasmSnapshot).functions.some((fn) =>
          MirVerification.operations(fn).some((operation) => operation._tag === 'RawBufferView'),
        ),
      )
      const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    }),
  60_000,
)

const failedGrowth = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.layout { Layout }
import silk.vector { Vector }

struct QuotaAllocator { remaining: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  if self.remaining == 0 { fail OutOfMemoryError {} }
  self.remaining = self.remaining - 1
  let mut inner = Allocator.systemAllocatorProvider()
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
  let block = run pending
  return move block
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

effect fn grow(values: &mut Vector<i32>) -> i32 ! OutOfMemoryError ? &mut Allocator {
  let appended = run Vector.append<i32>(move values, 14)
  return 1
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = QuotaAllocator { remaining: 1 }
  let mut values = Vector.make<i32>()
  let pending0 = Vector.append<i32>(&mut values, 10) |> Effect.provideMut(&mut allocator)
  let appended0 = run pending0
  let pending1 = Vector.append<i32>(&mut values, 11) |> Effect.provideMut(&mut allocator)
  let appended1 = run pending1
  let pending2 = Vector.append<i32>(&mut values, 12) |> Effect.provideMut(&mut allocator)
  let appended2 = run pending2
  let pending3 = Vector.append<i32>(&mut values, 13) |> Effect.provideMut(&mut allocator)
  let appended3 = run pending3
  let marker = run grow(&mut values)
    |> Effect.catchAll(recover)
    |> Effect.provideMut(&mut allocator)
  if marker == 7 {} else { return 0 }
  if Vector.length<i32>(&values) == 4 {} else { return 1 }
  if Vector.capacity<i32>(&values) == 4 {} else { return 2 }
  let first = Vector.get<i32>(&values, 0)
  let last = Vector.get<i32>(&values, 3)
  return first + last + 19
}

effect fn outerRecover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), outerRecover) }`

it.effect(
  'preserves the original vector when replacement allocation fails',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'vector-acceptance/failed-growth',
        ascii(failedGrowth),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', Json.stringify(evaluated))
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)
      const acquires = evaluated.trace.filter((event) => event._tag === 'AllocationAcquire')
      const releases = evaluated.trace.filter((event) => event._tag === 'AllocationRelease')
      assert.strictEqual(acquires.length, 1)
      assert.strictEqual(releases.length, 1)
      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    }),
  60_000,
)

const elementReleaseOrder = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.vector { Vector }

struct Entry {
  value: i32
  marker: Vector<i32>
}

fn record(value: i32) -> () { return () }

impl Drop for Entry {
  fn drop(self: &mut Entry) -> () {
    return record(self.value)
  }
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<Entry>()
  let entry0 = Entry { value: 3, marker: Vector.make<i32>() }
  let pending0 = Vector.append<Entry>(&mut values, move entry0) |> Effect.provideMut(&mut allocator)
  let appended0 = run pending0
  let entry1 = Entry { value: 5, marker: Vector.make<i32>() }
  let pending1 = Vector.append<Entry>(&mut values, move entry1) |> Effect.provideMut(&mut allocator)
  let appended1 = run pending1
  let entry2 = Entry { value: 7, marker: Vector.make<i32>() }
  let pending2 = Vector.append<Entry>(&mut values, move entry2) |> Effect.provideMut(&mut allocator)
  let appended2 = run pending2
  if Vector.capacity<Entry>(&values) == 4 {} else { return 0 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('drops initialized elements in order before releasing vector storage', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'vector-acceptance/element-release-order',
      ascii(elementReleaseOrder),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    const recorded = evaluated.trace.flatMap((event) =>
      event._tag === 'Binding' &&
      event.target.name === 'record' &&
      event.value._tag === 'IntegerValue'
        ? [event.value.value]
        : [],
    )
    // Capacity is four, but only the three initialized slots run Entry.drop, in index order.
    assert.deepEqual(recorded, [3n, 5n, 7n])
    const lastRecord = evaluated.trace.findLastIndex(
      (event) => event._tag === 'Call' && event.target.name === 'record',
    )
    const releases = evaluated.trace
      .map((event, index) => ({ event, index }))
      .filter(({ event }) => event._tag === 'AllocationRelease')
    assert.strictEqual(releases.length, 1)
    assert.isBelow(lastRecord, releases[0]?.index ?? -1)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

const transferredEarlyDrop = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.vector { Vector }

struct Entry {
  value: i32
  marker: Vector<i32>
}

fn record(value: i32) -> () { return () }

impl Drop for Entry {
  fn drop(self: &mut Entry) -> () {
    return record(self.value)
  }
}

fn consume(values: Vector<Entry>) -> i32 {
  drop values
  return 40
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<Entry>()
  let entry0 = Entry { value: 11, marker: Vector.make<i32>() }
  let pending0 = Vector.append<Entry>(&mut values, move entry0) |> Effect.provideMut(&mut allocator)
  let appended0 = run pending0
  let entry1 = Entry { value: 13, marker: Vector.make<i32>() }
  let pending1 = Vector.append<Entry>(&mut values, move entry1) |> Effect.provideMut(&mut allocator)
  let appended1 = run pending1
  let consumed = consume(move values)
  return consumed + 2
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('transfers vector ownership and drops it early on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'vector-acceptance/transferred-early-drop',
      ascii(transferredEarlyDrop),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    const recorded = evaluated.trace.flatMap((event) =>
      event._tag === 'Binding' &&
      event.target.name === 'record' &&
      event.value._tag === 'IntegerValue'
        ? [event.value.value]
        : [],
    )
    assert.deepEqual(recorded, [11n, 13n])
    const releaseIndices = evaluated.trace.flatMap((event, index) =>
      event._tag === 'AllocationRelease' ? [index] : [],
    )
    assert.strictEqual(releaseIndices.length, 1)
    const consumeReturn = evaluated.trace.findIndex(
      (event) => event._tag === 'Return' && event.function.name === 'consume',
    )
    assert.isBelow(releaseIndices[0] ?? -1, consumeReturn)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('reads a zero-sized Copy element through a shared vector on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const source = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.vector { Vector }
struct Marker {}
impl Copy for Marker {}
effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<Marker>()
  let pending = Vector.append<Marker>(&mut values, Marker {}) |> Effect.provideMut(&mut allocator)
  let appended = run pending
  let observed = Vector.get<Marker>(&values, 0)
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`
    const snapshot = yield* Analysis.ofSourceRealized(
      'vector-acceptance/zero-sized-read',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 42n)
    assert.strictEqual(evaluated.trace.filter((event) => event._tag === 'RawBufferRead').length, 1)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect(
  'reads all-Copy structural-union elements through a shared vector on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const source = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.u8 as u8
import silk.vector { Vector }
struct Step { value: i32 }
impl Copy for Step {}
struct Diagnostic {
  marker: u8
  value: i32
}
impl Copy for Diagnostic {}
fn observe(event: Step | Diagnostic) -> i32 {
  return match move event {
    Step { value: stepValue } => stepValue
    Diagnostic { marker, value: diagnosticValue } => u8.toI32(marker) + diagnosticValue
  }
}
effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut events = Vector.make<Step | Diagnostic>()
  let step = Step { value: 7 }
  let diagnostic = Diagnostic { marker: 3, value: 11 }
  let firstAppend = run Vector.append<Step | Diagnostic>(&mut events, move step) |>
    Effect.provideMut(&mut allocator)
  let secondAppend = run Vector.append<Step | Diagnostic>(&mut events, move diagnostic) |>
    Effect.provideMut(&mut allocator)
  let first = Vector.get<Step | Diagnostic>(&events, 0)
  let second = Vector.get<Step | Diagnostic>(&events, 1)
  let firstAgain = Vector.get<Step | Diagnostic>(&events, 0)
  let secondAgain = Vector.get<Step | Diagnostic>(&events, 1)
  return observe(move first) + observe(move second) + observe(move firstAgain) +
    observe(move secondAgain)
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`
      const snapshot = yield* Analysis.ofSourceRealized(
        'vector-acceptance/structural-union-read',
        ascii(source),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 42n)
      assert.strictEqual(
        evaluated.trace.filter((event) => event._tag === 'RawBufferRead').length,
        4,
      )

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    }),
)

it.effect('rejects a shared vector read when one union member is move-only', () =>
  Effect.gen(function* () {
    const source = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.layout { Layout }
import silk.vector { Vector }
struct Guard { storage: Allocation }
struct Marker { value: i32 }
fn guarded(storage: Allocation) -> Guard | Marker { return Guard { storage: move storage } }
effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<[i32; 1]>()
  let allocation = run Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let mut events = Vector.make<Guard | Marker>()
  let event = guarded(move allocation)
  let appended = run Vector.append<Guard | Marker>(&mut events, move event) |>
    Effect.provideMut(&mut allocator)
  let observed = Vector.get<Guard | Marker>(&events, 0)
  drop observed
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`
    const snapshot = yield* Analysis.ofSourceRealized(
      'vector-acceptance/move-only-union-read',
      ascii(source),
    )
    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'SEM0083',
    )
  }),
)

/**
 * Runs one program on the evaluator and Wasm, asserting each engine
 * agrees on the exit value. Returns the evaluator trace so a caller can assert on drop order
 * and allocation pairing.
 */
const acceptOnEngines = (name: string, source: string) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      `vector-acceptance/${name}`,
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', Json.stringify(evaluated))
    if (evaluated._tag !== 'Completed') throw new Error('unreachable')
    assert.strictEqual(evaluated.result.value, 42n)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

    return evaluated
  })

const recordedValues = (
  evaluated: Extract<ReturnType<typeof Analysis.evaluate>, { _tag: 'Completed' }>,
): ReadonlyArray<bigint> =>
  evaluated.trace.flatMap((event) =>
    event._tag === 'Binding' &&
    event.target.name === 'record' &&
    event.value._tag === 'IntegerValue'
      ? [event.value.value]
      : [],
  )

const popShrinks = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.option { Option }
import silk.vector { Vector }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<i32>()
  let pending0 = Vector.append<i32>(&mut values, 10) |> Effect.provideMut(&mut allocator)
  let appended0 = run pending0
  let pending1 = Vector.append<i32>(&mut values, 11) |> Effect.provideMut(&mut allocator)
  let appended1 = run pending1
  let pending2 = Vector.append<i32>(&mut values, 21) |> Effect.provideMut(&mut allocator)
  let appended2 = run pending2
  let taken = Vector.pop<i32>(&mut values)
  let last = match move taken {
    Option<i32>.Some { value } => move value
    Option<i32>.None => 0
  }
  if last == 21 {} else { return 1 }
  // The Vector.length drops by exactly one and the Vector.capacity is untouched.
  if Vector.length<i32>(&values) == 2 {} else { return 2 }
  if Vector.capacity<i32>(&values) == 4 {} else { return 3 }
  if Vector.get<i32>(&values, 1) == 11 {} else { return 4 }
  return last + 21
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'pops the last element and decreases the length by one on the evaluator and Wasm',
  () => acceptOnEngines('pop-shrinks', popShrinks),
  60_000,
)

const popEmpty = `import silk.allocator { OutOfMemoryError }
import silk.effect { Effect }
import silk.option { Option }
import silk.vector { Vector }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut values = Vector.make<i32>()
  // An empty vector never allocated, so Vector.pop must answer from the Empty arm.
  let first = Vector.pop<i32>(&mut values)
  let absent = match move first {
    Option<i32>.Some { value } => 0
    Option<i32>.None => 42
  }
  if Vector.length<i32>(&values) == 0 {} else { return 1 }
  return absent
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'returns an absent value when popping an empty vector on the evaluator and Wasm',
  () => acceptOnEngines('pop-empty', popEmpty),
  60_000,
)

const removeShifts = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.vector { Vector }

effect fn seed(values: &mut Vector<i32>, value: i32) -> () ! OutOfMemoryError ? &mut Allocator {
  let appended = run Vector.append<i32>(move values, value)
  return ()
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<i32>()
  let s0 = run (seed(&mut values, 10) |> Effect.provideMut(&mut allocator))
  let s1 = run (seed(&mut values, 11) |> Effect.provideMut(&mut allocator))
  let s2 = run (seed(&mut values, 12) |> Effect.provideMut(&mut allocator))
  let s3 = run (seed(&mut values, 13) |> Effect.provideMut(&mut allocator))
  let removed = Vector.remove<i32>(&mut values, 1)
  if removed == 11 {} else { return 1 }
  if Vector.length<i32>(&values) == 3 {} else { return 2 }
  // The later elements shift down one slot and keep their relative order.
  if Vector.get<i32>(&values, 0) == 10 {} else { return 3 }
  if Vector.get<i32>(&values, 1) == 12 {} else { return 4 }
  if Vector.get<i32>(&values, 2) == 13 {} else { return 5 }
  return removed + 31
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'removes an element and shifts later elements down on the evaluator and Wasm',
  () => acceptOnEngines('remove-shifts', removeShifts),
  60_000,
)

const clearKeepsCapacity = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.vector { Vector }

effect fn seed(values: &mut Vector<i32>, value: i32) -> () ! OutOfMemoryError ? &mut Allocator {
  let appended = run Vector.append<i32>(move values, value)
  return ()
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<i32>()
  let s0 = run (seed(&mut values, 10) |> Effect.provideMut(&mut allocator))
  let s1 = run (seed(&mut values, 11) |> Effect.provideMut(&mut allocator))
  let s2 = run (seed(&mut values, 12) |> Effect.provideMut(&mut allocator))
  Vector.clear<i32>(&mut values)
  if Vector.length<i32>(&values) == 0 {} else { return 1 }
  // The buffer stays available for reuse, so the Vector.capacity survives the Vector.clear.
  if Vector.capacity<i32>(&values) == 4 {} else { return 2 }
  let s3 = run (seed(&mut values, 9) |> Effect.provideMut(&mut allocator))
  if Vector.length<i32>(&values) == 1 {} else { return 3 }
  if Vector.capacity<i32>(&values) == 4 {} else { return 4 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'clears the length while keeping the capacity on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const evaluated = yield* acceptOnEngines('clear-keeps-capacity', clearKeepsCapacity)
      // Reusing the cleared buffer needs no second allocation.
      const acquires = evaluated.trace.filter((event) => event._tag === 'AllocationAcquire')
      const releases = evaluated.trace.filter((event) => event._tag === 'AllocationRelease')
      assert.strictEqual(acquires.length, 1)
      assert.strictEqual(releases.length, 1)
    }),
  60_000,
)

const setDropsOldElement = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.vector { Vector }

struct Entry {
  value: i32
  marker: Vector<i32>
}

fn record(value: i32) -> () { return () }

impl Drop for Entry {
  fn drop(self: &mut Entry) -> () {
    return record(self.value)
  }
}

effect fn seed(values: &mut Vector<Entry>, value: i32) -> () ! OutOfMemoryError ? &mut Allocator {
  let entry = Entry { value: value, marker: Vector.make<i32>() }
  let appended = run Vector.append<Entry>(move values, move entry)
  return ()
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<Entry>()
  let s0 = run (seed(&mut values, 3) |> Effect.provideMut(&mut allocator))
  let s1 = run (seed(&mut values, 5) |> Effect.provideMut(&mut allocator))
  let replacement = Entry { value: 9, marker: Vector.make<i32>() }
  Vector.set<Entry>(&mut values, 0, move replacement)
  if Vector.length<Entry>(&values) == 2 {} else { return 1 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'drops the overwritten element exactly once on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const evaluated = yield* acceptOnEngines('set-drops-old', setDropsOldElement)
      // The overwritten 3 drops during set; 9 and 5 drop with the vector at scope end.
      assert.deepEqual(recordedValues(evaluated), [3n, 9n, 5n])
    }),
  60_000,
)

const truncateReleasesTail = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.vector { Vector }

struct Entry {
  value: i32
  marker: Vector<i32>
}

fn record(value: i32) -> () { return () }

impl Drop for Entry {
  fn drop(self: &mut Entry) -> () {
    return record(self.value)
  }
}

effect fn seed(values: &mut Vector<Entry>, value: i32) -> () ! OutOfMemoryError ? &mut Allocator {
  let entry = Entry { value: value, marker: Vector.make<i32>() }
  let appended = run Vector.append<Entry>(move values, move entry)
  return ()
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<Entry>()
  let s0 = run (seed(&mut values, 3) |> Effect.provideMut(&mut allocator))
  let s1 = run (seed(&mut values, 5) |> Effect.provideMut(&mut allocator))
  let s2 = run (seed(&mut values, 7) |> Effect.provideMut(&mut allocator))
  Vector.truncate<Entry>(&mut values, 1)
  if Vector.length<Entry>(&values) == 1 {} else { return 1 }
  if Vector.capacity<Entry>(&values) == 4 {} else { return 2 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'releases each truncated element exactly once on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const evaluated = yield* acceptOnEngines('truncate-releases-tail', truncateReleasesTail)
      // The tail drops in index order during truncate; the survivor drops with the vector.
      assert.deepEqual(recordedValues(evaluated), [5n, 7n, 3n])
    }),
  60_000,
)

const moveOnlyRoundTrip = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.option { Option }
import silk.vector { Vector }

struct Entry {
  value: i32
  marker: Vector<i32>
}

fn record(value: i32) -> () { return () }

impl Drop for Entry {
  fn drop(self: &mut Entry) -> () {
    return record(self.value)
  }
}

// Reads the scalar field off the owned element before consuming it, so the exit value every
// engine checks depends on what the vector actually stored.
fn release(entry: Entry) -> i32 {
  let read = entry.value
  drop entry
  return read
}

effect fn seed(values: &mut Vector<Entry>, value: i32) -> () ! OutOfMemoryError ? &mut Allocator {
  let entry = Entry { value: value, marker: Vector.make<i32>() }
  let appended = run Vector.append<Entry>(move values, move entry)
  return ()
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<Entry>()
  let s0 = run (seed(&mut values, 3) |> Effect.provideMut(&mut allocator))
  let s1 = run (seed(&mut values, 5) |> Effect.provideMut(&mut allocator))
  let s2 = run (seed(&mut values, 7) |> Effect.provideMut(&mut allocator))
  let s3 = run (seed(&mut values, 9) |> Effect.provideMut(&mut allocator))
  let taken = Vector.pop<Entry>(&mut values)
  let popped = match move taken {
    Option<Entry>.Some { value } => release(move value)
    Option<Entry>.None => 0
  }
  // The popped element is the last one appended, and the removed one is the first.
  if popped == 9 {} else { return 1 }
  if Vector.length<Entry>(&values) == 3 {} else { return 2 }
  let pulled = Vector.remove<Entry>(&mut values, 0)
  let removed = release(move pulled)
  if removed == 3 {} else { return 3 }
  if Vector.length<Entry>(&values) == 2 {} else { return 4 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'pairs every acquire with one release across append, pop, and drop of move-only elements',
  () =>
    Effect.gen(function* () {
      const evaluated = yield* acceptOnEngines('move-only-round-trip', moveOnlyRoundTrip)
      // Ownership leaves the vector with pop and remove, so each element is released once: the
      // two extracted elements drop at their binding, the two survivors with the vector.
      const acquires = evaluated.trace.filter((event) => event._tag === 'AllocationAcquire')
      const releases = evaluated.trace.filter((event) => event._tag === 'AllocationRelease')
      assert.strictEqual(acquires.length, releases.length)
      assert.deepEqual(recordedValues(evaluated), [9n, 3n, 5n, 7n])
    }),
  60_000,
)

const failedReserve = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.layout { Layout }
import silk.vector { Vector }

struct QuotaAllocator { remaining: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  if self.remaining == 0 { fail OutOfMemoryError {} }
  self.remaining = self.remaining - 1
  let mut inner = Allocator.systemAllocatorProvider()
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
  let block = run pending
  return move block
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

effect fn grow(values: &mut Vector<i32>) -> i32 ! OutOfMemoryError ? &mut Allocator {
  let reserved = run Vector.reserve<i32>(move values, 100)
  return 1
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = QuotaAllocator { remaining: 1 }
  let mut values = Vector.make<i32>()
  let pending0 = Vector.append<i32>(&mut values, 10) |> Effect.provideMut(&mut allocator)
  let appended0 = run pending0
  let pending1 = Vector.append<i32>(&mut values, 11) |> Effect.provideMut(&mut allocator)
  let appended1 = run pending1
  let marker = run grow(&mut values)
    |> Effect.catchAll(recover)
    |> Effect.provideMut(&mut allocator)
  if marker == 7 {} else { return 1 }
  // The failed Vector.reserve committed nothing: Vector.length, Vector.capacity, and every element survive.
  if Vector.length<i32>(&values) == 2 {} else { return 2 }
  if Vector.capacity<i32>(&values) == 4 {} else { return 3 }
  if Vector.get<i32>(&values, 0) == 10 {} else { return 4 }
  if Vector.get<i32>(&values, 1) == 11 {} else { return 5 }
  return 42
}

effect fn outerRecover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), outerRecover) }`

it.effect(
  'leaves the vector unchanged when reserve fails with OutOfMemoryError',
  () =>
    Effect.gen(function* () {
      const evaluated = yield* acceptOnEngines('failed-reserve', failedReserve)
      // Only the original buffer was ever acquired, and it is released exactly once.
      const acquires = evaluated.trace.filter((event) => event._tag === 'AllocationAcquire')
      const releases = evaluated.trace.filter((event) => event._tag === 'AllocationRelease')
      assert.strictEqual(acquires.length, 1)
      assert.strictEqual(releases.length, 1)
    }),
  60_000,
)

// An owner that is live across an effect call and carries a Drop hook needs frame storage for
// that hook, but its address never leaves the frame — the value itself stays in registers. The
// Wasm backend used to reload every frame root after an effect call, which overwrote such an
// owner with frame bytes nothing had written yet, so the element the append went on to store
// was all-zero. Only the appends that grow reach an effect call while holding the element, which
// is why index 0 and the index that triggers the next growth were the ones that read back zero.
const moveOnlyElementZero = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.vector { Vector }

struct Entry {
  value: i32
  marker: Vector<i32>
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<Entry>()
  let entry = Entry { value: 42, marker: Vector.make<i32>() }
  let pending = Vector.append<Entry>(&mut values, move entry) |> Effect.provideMut(&mut allocator)
  let appended = run pending
  let readable = Vector.asSlice<Entry>(&values)
  return readable[0].value
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'reads back element zero of a move-only element on the evaluator and Wasm',
  () => acceptOnEngines('move-only-element-zero', moveOnlyElementZero),
  60_000,
)

const moveOnlyAppendSequence = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.vector { Vector }

struct Entry {
  value: i32
  marker: Vector<i32>
}

effect fn seed(values: &mut Vector<Entry>, value: i32) -> () ! OutOfMemoryError ? &mut Allocator {
  let entry = Entry { value: value, marker: Vector.make<i32>() }
  let appended = run Vector.append<Entry>(move values, move entry)
  return ()
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<Entry>()
  let s0 = run (seed(&mut values, 1) |> Effect.provideMut(&mut allocator))
  let s1 = run (seed(&mut values, 2) |> Effect.provideMut(&mut allocator))
  let s2 = run (seed(&mut values, 3) |> Effect.provideMut(&mut allocator))
  if Vector.length<Entry>(&values) == 3 {} else { return 1 }
  let readable = Vector.asSlice<Entry>(&values)
  if readable[0].value == 1 {} else { return 2 }
  if readable[1].value == 2 {} else { return 3 }
  if readable[2].value == 3 {} else { return 4 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'reads back every element of a move-only append sequence on the evaluator and Wasm',
  () => acceptOnEngines('move-only-append-sequence', moveOnlyAppendSequence),
  60_000,
)

// Five appends cross both growths (0 -> 4 -> 8), so two of them hold the element across the
// allocating effect call rather than one.
const moveOnlyGrowthSequence = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.vector { Vector }

struct Entry {
  value: i32
  marker: Vector<i32>
}

effect fn seed(values: &mut Vector<Entry>, value: i32) -> () ! OutOfMemoryError ? &mut Allocator {
  let entry = Entry { value: value, marker: Vector.make<i32>() }
  let appended = run Vector.append<Entry>(move values, move entry)
  return ()
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<Entry>()
  let s0 = run (seed(&mut values, 1) |> Effect.provideMut(&mut allocator))
  let s1 = run (seed(&mut values, 2) |> Effect.provideMut(&mut allocator))
  let s2 = run (seed(&mut values, 3) |> Effect.provideMut(&mut allocator))
  let s3 = run (seed(&mut values, 4) |> Effect.provideMut(&mut allocator))
  let s4 = run (seed(&mut values, 5) |> Effect.provideMut(&mut allocator))
  if Vector.length<Entry>(&values) == 5 {} else { return 1 }
  if Vector.capacity<Entry>(&values) == 8 {} else { return 2 }
  let readable = Vector.asSlice<Entry>(&values)
  if readable[0].value == 1 {} else { return 3 }
  if readable[1].value == 2 {} else { return 4 }
  if readable[2].value == 3 {} else { return 5 }
  if readable[3].value == 4 {} else { return 6 }
  if readable[4].value == 5 {} else { return 7 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 8 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'reads back every element across both growths of a move-only vector on the evaluator and Wasm',
  () => acceptOnEngines('move-only-growth-sequence', moveOnlyGrowthSequence),
  60_000,
)

// Any move-only field reaches the same path, so the element type is pinned with Bytes rather
// than a nested Vector.
const moveOnlyBytesField = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.vector { Vector }
import silk.bytes { Bytes }

struct Entry {
  marker: Bytes
  value: i32
}

effect fn seed(values: &mut Vector<Entry>, value: i32) -> () ! OutOfMemoryError ? &mut Allocator {
  let entry = Entry { marker: Bytes.make(), value: value }
  let appended = run Vector.append<Entry>(move values, move entry)
  return ()
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<Entry>()
  let s0 = run (seed(&mut values, 20) |> Effect.provideMut(&mut allocator))
  let s1 = run (seed(&mut values, 22) |> Effect.provideMut(&mut allocator))
  if Vector.length<Entry>(&values) == 2 {} else { return 1 }
  let readable = Vector.asSlice<Entry>(&values)
  return readable[0].value + readable[1].value
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'reads back element zero when the move-only field is Bytes on the evaluator and Wasm',
  () => acceptOnEngines('move-only-bytes-field', moveOnlyBytesField),
  60_000,
)
