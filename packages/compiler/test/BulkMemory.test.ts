import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'
import * as Driver from '../src/Driver.js'
import * as Mir from '../src/Mir.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as WasmBackend from '../src/WasmBackend.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-bulk-memory-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

const runWasm = (bytes: Uint8Array): number => {
  const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes.slice()), {})
  return (instance.exports.silk_main as () => number)()
}

const compileNative = (name: string, source: string, backend?: Backend.Backend) =>
  Driver.compile({
    compilation: { root: SourceFile.make(name, ascii(source)) },
    toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
    profile: 'release',
    destination: join(destinationRoot, name.replace(/[^a-z0-9]/gi, '-')),
    ...(backend === undefined ? {} : { backend }),
    cache: false,
  }).pipe(Effect.provide(SourceResolver.empty))

/**
 * Copies an initialized prefix between two raw buffers and reads both back. The elements are
 * `i32`, so the moved-from range stays readable exactly as the byte-level backends leave it.
 */
const copyRange = `import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.raw_buffer as RawBuffer
import silk.slot as Slot
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let sourceLayout = Layout.of<[i32; 4]>()
  let sourceAllocation = run Allocator.allocate(move sourceLayout) |> Effect.provideMut(&mut allocator)
  let targetLayout = Layout.of<[i32; 4]>()
  let targetAllocation = run Allocator.allocate(move targetLayout) |> Effect.provideMut(&mut allocator)
  unsafe {
    let mut source = RawBuffer.from<i32>(move sourceAllocation, 4)
    let mut target = RawBuffer.from<i32>(move targetAllocation, 4)
    let first = Slot.write(RawBuffer.slot(&mut source, 0), 3)
    let second = Slot.write(RawBuffer.slot(&mut source, 1), 5)
    let third = Slot.write(RawBuffer.slot(&mut source, 2), 7)
    let prefix = RawBuffer.view<i32>(&source, 1, 2)
    let copied = RawBuffer.copy<i32>(&mut target, 2, prefix, 2)
    let low = RawBuffer.read<i32>(&target, 2)
    let high = RawBuffer.read<i32>(&target, 3)
    let kept = RawBuffer.read<i32>(&source, 1)
    let clearedFirst = Slot.take(RawBuffer.slot(&mut target, 2))
    let clearedSecond = Slot.take(RawBuffer.slot(&mut target, 3))
    let sourceFirst = Slot.take(RawBuffer.slot(&mut source, 0))
    let sourceSecond = Slot.take(RawBuffer.slot(&mut source, 1))
    let sourceThird = Slot.take(RawBuffer.slot(&mut source, 2))
    drop source
    drop target
    return low * 10 + high * 3 + kept
  }
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

it.effect('copies a raw-storage range identically on the evaluator, LLVM, and Wasm', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'bulk-memory/copy-range',
      ascii(copyRange),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 76n)
    assert.strictEqual(evaluated.trace.filter((event) => event._tag === 'RawBufferCopy').length, 1)
    const encoded = Mir.encode(Analysis.loweredMir(snapshot))
    assert.include(encoded, 'raw-buffer-copy')

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    assert.include(wasm.wat, 'memory.copy')
    assert.strictEqual(runWasm(wasm.bytes), 76)

    const bitcode = yield* Analysis.codegen(snapshot, { mode: 'release' })
    // memmove, not memcpy: overlapping ranges are a defined move rather than undefined behavior.
    assert.include(bitcode.ir, 'llvm.memmove')
    assert.notInclude(bitcode.ir, 'llvm.memcpy')
  }),
)

/** Moves a range of move-only records between buffers and drops them from their new home. */
const moveOnlyCopy = (extra: string) => `import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.raw_buffer as RawBuffer
import silk.slot as Slot
struct Guard { storage: Allocation }

effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let sourceLayout = Layout.of<[Guard; 2]>()
  let sourceAllocation = run Allocator.allocate(move sourceLayout) |> Effect.provideMut(&mut allocator)
  let targetLayout = Layout.of<[Guard; 2]>()
  let targetAllocation = run Allocator.allocate(move targetLayout) |> Effect.provideMut(&mut allocator)
  let firstLayout = Layout.of<[i32; 1]>()
  let firstPayload = run Allocator.allocate(move firstLayout) |> Effect.provideMut(&mut allocator)
  let secondLayout = Layout.of<[i32; 1]>()
  let secondPayload = run Allocator.allocate(move secondLayout) |> Effect.provideMut(&mut allocator)
  unsafe {
    let mut source = RawBuffer.from<Guard>(move sourceAllocation, 2)
    let mut target = RawBuffer.from<Guard>(move targetAllocation, 2)
    let first = Guard { storage: move firstPayload }
    let second = Guard { storage: move secondPayload }
    let firstWritten = Slot.write<Guard>(RawBuffer.slot(&mut source, 0), move first)
    let secondWritten = Slot.write<Guard>(RawBuffer.slot(&mut source, 1), move second)
    let initialized = RawBuffer.view<Guard>(&source, 0, 2)
    let moved = RawBuffer.copy<Guard>(&mut target, 0, initialized, 2)
${extra}
    let takenFirst = Slot.take<Guard>(RawBuffer.slot(&mut target, 0))
    let takenSecond = Slot.take<Guard>(RawBuffer.slot(&mut target, 1))
    drop takenFirst
    drop takenSecond
    drop source
    drop target
    return 42
  }
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

it.effect('moves a range of move-only elements and leaves the source slots empty', () =>
  Effect.gen(function* () {
    const source = moveOnlyCopy('')
    const snapshot = yield* Analysis.ofSourceRealized(
      'bulk-memory/move-only',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    // Both payload allocations release through the moved elements' new home, not the old one.
    assert.strictEqual(
      evaluated.trace.filter((event) => event._tag === 'AllocationRelease').length,
      evaluated.trace.filter((event) => event._tag === 'AllocationAcquire').length,
    )

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    assert.strictEqual(runWasm(wasm.bytes), 42)

    // Taking from the moved-from range traps: the copy gave those slots up. Only the evaluator
    // tracks per-slot initialization, so this is where the emptied source is observable.
    const stealing = yield* Analysis.ofSourceRealized(
      'bulk-memory/move-only-stolen',
      ascii(
        moveOnlyCopy(`    let stolen = Slot.take<Guard>(RawBuffer.slot(&mut source, 0))
    drop stolen`),
      ),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(stealing), [])
    const blocked = Analysis.evaluate(stealing)
    assert.strictEqual(blocked._tag, 'Trap')
    if (blocked._tag !== 'Trap') return
    assert.strictEqual(blocked.reason, 'Slot.take requires live initialized storage')
  }),
)

/** Sets a byte range to one repeated value and reads the touched and untouched bytes back. */
const fillRange = `import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.raw_buffer as RawBuffer
import silk.slot as Slot
import silk.u8 as u8
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[u8; 4]>()
  let allocation = run Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  unsafe {
    let mut buffer = RawBuffer.from<u8>(move allocation, 4)
    let cleared = RawBuffer.fill(&mut buffer, 0, 4, u8.toU8(9))
    let refilled = RawBuffer.fill(&mut buffer, 1, 2, u8.toU8(3))
    let first = RawBuffer.read<u8>(&buffer, 0)
    let second = RawBuffer.read<u8>(&buffer, 1)
    let third = RawBuffer.read<u8>(&buffer, 2)
    let fourth = RawBuffer.read<u8>(&buffer, 3)
    let takenFirst = Slot.take(RawBuffer.slot(&mut buffer, 0))
    let takenSecond = Slot.take(RawBuffer.slot(&mut buffer, 1))
    let takenThird = Slot.take(RawBuffer.slot(&mut buffer, 2))
    let takenFourth = Slot.take(RawBuffer.slot(&mut buffer, 3))
    drop buffer
    return u8.toI32(first) * 8 + u8.toI32(second) * 4 + u8.toI32(third) * 2 + u8.toI32(fourth)
  }
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

it.effect('fills a byte range identically on the evaluator, LLVM, and Wasm', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'bulk-memory/fill-range',
      ascii(fillRange),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 99n)
    assert.strictEqual(evaluated.trace.filter((event) => event._tag === 'RawBufferFill').length, 2)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    assert.include(wasm.wat, 'memory.fill')
    assert.strictEqual(runWasm(wasm.bytes), 99)

    const bitcode = yield* Analysis.codegen(snapshot, { mode: 'release' })
    assert.include(bitcode.ir, 'llvm.memset')
  }),
)

/**
 * A copy whose source range and destination range genuinely overlap. Silk's borrow rules keep a
 * shared source borrow and an exclusive destination borrow of one buffer from coexisting at a
 * call, so the aliasing pair is formed on the lowered MIR the three engines share: the copy keeps
 * its shared view of `buffer[0..3)` and writes through the exclusive borrow of that same buffer
 * at offset 1. A forward element-by-element copy would smear the first element across the range;
 * the defined move produces the as-if-intermediate-buffer result 1, 1, 2, 3.
 */
const overlappingCopy = `import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 4]>()
  let allocation = run Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let spare = Layout.of<[i32; 4]>()
  let spareAllocation = run Allocator.allocate(move spare) |> Effect.provideMut(&mut allocator)
  unsafe {
    let mut buffer = Intrinsic.rawBufferFrom<i32>(move allocation, 4)
    let mut other = Intrinsic.rawBufferFrom<i32>(move spareAllocation, 4)
    let first = Intrinsic.slotWrite<i32>(Intrinsic.rawBufferSlot<i32>(&mut buffer, 0), 1)
    let second = Intrinsic.slotWrite<i32>(Intrinsic.rawBufferSlot<i32>(&mut buffer, 1), 2)
    let third = Intrinsic.slotWrite<i32>(Intrinsic.rawBufferSlot<i32>(&mut buffer, 2), 3)
    let fourth = Intrinsic.slotWrite<i32>(Intrinsic.rawBufferSlot<i32>(&mut buffer, 3), 4)
    let prefix = Intrinsic.rawBufferView<i32>(&buffer, 0, 3)
    let moved = Intrinsic.rawBufferCopy<i32>(&mut other, 1, prefix, 3)
    let a = Intrinsic.rawBufferRead<i32>(&buffer, 0)
    let b = Intrinsic.rawBufferRead<i32>(&buffer, 1)
    let c = Intrinsic.rawBufferRead<i32>(&buffer, 2)
    let d = Intrinsic.rawBufferRead<i32>(&buffer, 3)
    let takenFirst = Intrinsic.slotTake<i32>(Intrinsic.rawBufferSlot<i32>(&mut buffer, 0))
    let takenSecond = Intrinsic.slotTake<i32>(Intrinsic.rawBufferSlot<i32>(&mut buffer, 1))
    let takenThird = Intrinsic.slotTake<i32>(Intrinsic.rawBufferSlot<i32>(&mut buffer, 2))
    let takenFourth = Intrinsic.slotTake<i32>(Intrinsic.rawBufferSlot<i32>(&mut buffer, 3))
    drop buffer
    drop other
    return a * 27 + b * 9 + c * 3 + d
  }
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

/**
 * Rewrites the copy's destination borrow to the exclusive borrow of the buffer its source view
 * already reads, which is the aliasing pair source-level borrow checking rejects.
 */
const aliasDestinationWithSource = (program: Mir.Module): Mir.Module => {
  let rewritten = 0
  const functions = program.functions.map((fn) => {
    const operations = Mir.operations(fn)
    const copy = operations.find((operation) => operation._tag === 'RawBufferCopy')
    if (copy === undefined) return fn
    const view = operations.find(
      (operation): operation is Extract<Mir.Operation, { readonly _tag: 'RawBufferView' }> =>
        operation._tag === 'RawBufferView',
    )
    const sharedRoot = operations.find(
      (operation): operation is Extract<Mir.Operation, { readonly _tag: 'BeginLoan' }> =>
        operation._tag === 'BeginLoan' && operation.destination.ordinal === view?.buffer.ordinal,
    )?.root
    const exclusive = operations.findLast(
      (operation): operation is Extract<Mir.Operation, { readonly _tag: 'BeginLoan' }> =>
        operation._tag === 'BeginLoan' &&
        operation.access === 'Exclusive' &&
        operation.root.ordinal === sharedRoot?.ordinal &&
        operation.destination.ordinal < copy.destination.ordinal,
    )
    if (exclusive === undefined) return fn
    const substitute = (operation: Mir.Operation): Mir.Operation => {
      if (operation._tag !== 'RawBufferCopy') return operation
      rewritten += 1
      return Object.freeze({ ...operation, buffer: exclusive.destination })
    }
    return Object.freeze({
      ...fn,
      regions: fn.regions.map((region) =>
        region._tag === 'OperationRegion'
          ? Object.freeze({ ...region, operations: region.operations.map(substitute) })
          : region,
      ),
    })
  })
  if (rewritten !== 1) {
    throw new RangeError(`Expected exactly one copy to alias, rewrote ${rewritten}`)
  }
  return Object.freeze({ ...program, functions })
}

it.effect('treats an overlapping copy as a defined move on all three engines', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'bulk-memory/overlap',
      ascii(overlappingCopy),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    // Without aliasing the copy reaches the spare buffer and the observed one is untouched.
    const untouched = Analysis.evaluate(snapshot)
    assert.strictEqual(untouched._tag, 'Completed')
    if (untouched._tag !== 'Completed') return
    assert.strictEqual(untouched.result.value, 58n)

    const overlapping = aliasDestinationWithSource(Analysis.loweredMir(snapshot))
    assert.deepEqual(Mir.verify(overlapping), [])

    const evaluated = BootstrapEvaluation.evaluate(Analysis.instancesOf(snapshot), overlapping)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 45n)

    const wasm = yield* Backend.emit(WasmBackend.WasmBackend, overlapping, { mode: 'release' })
    assert.strictEqual(runWasm(wasm.bytes), 45)

    const overlappingBackend: Backend.Backend = Object.freeze({
      ...Backend.LlvmBackend,
      emit: (program: Mir.Module, request: Backend.CodegenRequest) =>
        Backend.LlvmBackend.emit(aliasDestinationWithSource(program), request),
    })
    const compiled = yield* compileNative(
      'bulk-memory/overlap',
      overlappingCopy,
      overlappingBackend,
    )
    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag !== 'Compiled') return
    const executed = spawnSync(compiled.path, [], { encoding: 'utf8' })
    assert.strictEqual(executed.status, 45, executed.stderr)
  }),
)

/** `Bytes.append` copies a borrowed byte sequence in one bulk move rather than byte by byte. */
const bulkBytes = `import silk.bytes { Bytes }
import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effect as Effect
import silk.u8 as u8
import silk.usize as usize
fn octet(value: u8) -> u8 { return value }

effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let recipe = build() |> Effect.provideMut(&mut allocator)
  return run recipe
}

effect fn build() -> i32 ! OutOfMemoryError ? &mut Allocator {
  let mut collected = Bytes.make()
  let first = [octet(1), octet(2), octet(3)]
  let appendedFirst = run Bytes.append(&mut collected, &first)
  let second = [octet(4), octet(5)]
  let appendedSecond = run Bytes.append(&mut collected, &second)
  let view = Bytes.asSlice(&collected)
  let total = u8.toI32(view[0]) + u8.toI32(view[2]) + u8.toI32(view[4])
  return total * 10 + usize.toI32(Bytes.length(&collected))
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

it.effect('appends borrowed bytes through the copy intrinsic on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'bulk-memory/bytes-append',
      ascii(bulkBytes),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const encoded = Mir.encode(Analysis.loweredMir(snapshot))
    assert.include(encoded, 'raw-buffer-copy')

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 95n)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    assert.strictEqual(runWasm(wasm.bytes), 95)
  }),
)

/** Vector growth migrates its initialized elements with one copy instead of one per element. */
const vectorGrowth = `import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effect as Effect
import silk.vector { Vector }
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let recipe = build() |> Effect.provideMut(&mut allocator)
  return run recipe
}

effect fn build() -> i32 ! OutOfMemoryError ? &mut Allocator {
  let mut values = Vector.make<i32>()
  let mut index = 0
  while index < 9 {
    let appended = run Vector.append<i32>(&mut values, index * index)
    index = index + 1
  }
  return Vector.get<i32>(&values, 0) + Vector.get<i32>(&values, 4) + Vector.get<i32>(&values, 8)
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

it.effect('grows a vector through one bulk copy per migration on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'bulk-memory/vector-growth',
      ascii(vectorGrowth),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 80n)
    // Three migrations (4 -> 8 -> 16 elements plus the first allocation) move 12 elements between
    // buffers, and each migration is one copy rather than one Slot.take/Slot.write pair each.
    assert.strictEqual(evaluated.trace.filter((event) => event._tag === 'RawBufferCopy').length, 2)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    assert.strictEqual(runWasm(wasm.bytes), 80)
  }),
)

/** A range that runs past the destination or the source traps identically everywhere. */
const outOfRange = `import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.raw_buffer as RawBuffer
import silk.slot as Slot
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let sourceLayout = Layout.of<[i32; 2]>()
  let sourceAllocation = run Allocator.allocate(move sourceLayout) |> Effect.provideMut(&mut allocator)
  let targetLayout = Layout.of<[i32; 2]>()
  let targetAllocation = run Allocator.allocate(move targetLayout) |> Effect.provideMut(&mut allocator)
  unsafe {
    let mut source = RawBuffer.from<i32>(move sourceAllocation, 2)
    let mut target = RawBuffer.from<i32>(move targetAllocation, 2)
    let first = Slot.write(RawBuffer.slot(&mut source, 0), 10)
    let second = Slot.write(RawBuffer.slot(&mut source, 1), 20)
    let initialized = RawBuffer.view<i32>(&source, 0, 2)
    let copied = RawBuffer.copy<i32>(&mut target, 1, initialized, 2)
    drop source
    drop target
    return 5
  }
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

it.effect('traps when the copied range runs past the destination', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'bulk-memory/out-of-range',
      ascii(outOfRange),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Trap')
    if (evaluated._tag !== 'Trap') return
    assert.strictEqual(evaluated.reason, 'RawBuffer copy range is out of bounds')

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    assert.throws(() => runWasm(wasm.bytes))
  }),
)
