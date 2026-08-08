import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as Mir from '../src/Mir.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

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

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-vector-acceptance-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

/**
 * The first useful owned sequence written entirely in Silk: six appends force two geometric
 * growths (0 -> 4 -> 8) with element migration, then checked reads observe both ends.
 */
const growth = `import silk.vector { Vector, make, append, get, length, capacity }

effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let mut values = make<I32>()
  let pending0 = append<I32>(&mut values, 10) |> Allocator.provide(&mut allocator)
  let appended0 = run pending0
  let pending1 = append<I32>(&mut values, 11) |> Allocator.provide(&mut allocator)
  let appended1 = run pending1
  let pending2 = append<I32>(&mut values, 12) |> Allocator.provide(&mut allocator)
  let appended2 = run pending2
  let pending3 = append<I32>(&mut values, 13) |> Allocator.provide(&mut allocator)
  let appended3 = run pending3
  let pending4 = append<I32>(&mut values, 14) |> Allocator.provide(&mut allocator)
  let appended4 = run pending4
  let pending5 = append<I32>(&mut values, 15) |> Allocator.provide(&mut allocator)
  let appended5 = run pending5
  if length<I32>(&values) == 6 {} else { return 0 }
  if capacity<I32>(&values) == 8 {} else { return 1 }
  let first = get<I32>(&mut values, 0)
  let last = get<I32>(&mut values, 5)
  return first + last + 17
}

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), recover) }`

it.effect('grows, reads, and releases a Silk-written vector on all three engines', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'vector-acceptance/growth',
      ascii(growth),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42)
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
        Mir.operations(fn).map((operation) => operation._tag),
      ),
    )
    assert.isFalse([...operations].some((tag) => tag.toLowerCase().includes('vector')))
    assert.isFalse(evaluated.trace.some((event) => event._tag.toLowerCase().includes('vector')))

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    assert.isFalse(
      watOperationNames(wasm.ir).some((operation) => operation.toLowerCase().includes('vector')),
    )
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

    const nativeSnapshot = yield* Analysis.ofSource(
      'vector-acceptance/growth',
      ascii(growth),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(nativeSnapshot), [])
    const llvm = yield* Analysis.codegen(nativeSnapshot, { mode: 'release' })
    assert.isFalse(
      llvmOperationNames(llvm.ir).some((operation) => operation.toLowerCase().includes('vector')),
    )

    const compiled = yield* Driver.compile({
      compilation: { root: SourceFile.make('vector-acceptance/growth', ascii(growth)) },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
      profile: 'release',
      destination: join(destinationRoot, 'growth'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.executable, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, 42, run.stderr)
  }),
  15_000,
)

const failedGrowth = `import silk.vector { Vector, make, append, get, length, capacity }

struct QuotaAllocator { remaining: I32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  if self.remaining == 0 { fail OutOfMemory {} }
  self.remaining = self.remaining - 1
  let mut inner = SystemAllocator.make()
  let pending = Allocator.allocate(move layout) |> Allocator.provide(&mut inner)
  let block = run pending
  return move block
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

effect fn grow(values: &mut Vector<I32>) -> I32 ! OutOfMemory ? &mut Allocator {
  let appended = run append<I32>(move values, 14)
  return 1
}

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = QuotaAllocator { remaining: 1 }
  let mut values = make<I32>()
  let pending0 = append<I32>(&mut values, 10) |> Allocator.provide(&mut allocator)
  let appended0 = run pending0
  let pending1 = append<I32>(&mut values, 11) |> Allocator.provide(&mut allocator)
  let appended1 = run pending1
  let pending2 = append<I32>(&mut values, 12) |> Allocator.provide(&mut allocator)
  let appended2 = run pending2
  let pending3 = append<I32>(&mut values, 13) |> Allocator.provide(&mut allocator)
  let appended3 = run pending3
  let marker = run Effect.catch<OutOfMemory>(
    grow(&mut values) |> Allocator.provide(&mut allocator),
    recover,
  )
  if marker == 7 {} else { return 0 }
  if length<I32>(&values) == 4 {} else { return 1 }
  if capacity<I32>(&values) == 4 {} else { return 2 }
  let first = get<I32>(&mut values, 0)
  let last = get<I32>(&mut values, 3)
  return first + last + 19
}

effect fn outerRecover(error: OutOfMemory) -> I32 { return 0 }

pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), outerRecover) }`

it.effect('preserves the original vector when replacement allocation fails', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'vector-acceptance/failed-growth',
      ascii(failedGrowth),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42)
    const acquires = evaluated.trace.filter((event) => event._tag === 'AllocationAcquire')
    const releases = evaluated.trace.filter((event) => event._tag === 'AllocationRelease')
    assert.strictEqual(acquires.length, 1)
    assert.strictEqual(releases.length, 1)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

    const compiled = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('vector-acceptance/failed-growth', ascii(failedGrowth)),
      },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
      profile: 'release',
      destination: join(destinationRoot, 'failed-growth'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.executable, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, 42, run.stderr)
  }),
)

const elementReleaseOrder = `import silk.vector { Vector, make, append, capacity }

struct Entry {
  value: I32
  marker: Vector<I32>
}

fn record(value: I32) -> Unit { return Unit.make() }

impl Drop for Entry {
  fn drop(self: &mut Entry) -> Unit {
    return record(self.value)
  }
}

effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let mut values = make<Entry>()
  let entry0 = Entry { value: 3, marker: make<I32>() }
  let pending0 = append<Entry>(&mut values, move entry0) |> Allocator.provide(&mut allocator)
  let appended0 = run pending0
  let entry1 = Entry { value: 5, marker: make<I32>() }
  let pending1 = append<Entry>(&mut values, move entry1) |> Allocator.provide(&mut allocator)
  let appended1 = run pending1
  let entry2 = Entry { value: 7, marker: make<I32>() }
  let pending2 = append<Entry>(&mut values, move entry2) |> Allocator.provide(&mut allocator)
  let appended2 = run pending2
  if capacity<Entry>(&values) == 4 {} else { return 0 }
  return 42
}

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), recover) }`

it.effect('drops initialized elements in order before releasing vector storage', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'vector-acceptance/element-release-order',
      ascii(elementReleaseOrder),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42)
    const recorded = evaluated.trace.flatMap((event) =>
      event._tag === 'Binding' && event.target.name === 'record' && event.value._tag === 'I32Value'
        ? [event.value.value]
        : [],
    )
    // Capacity is four, but only the three initialized slots run Entry.drop, in index order.
    assert.deepEqual(recorded, [3, 5, 7])
    const lastRecord = evaluated.trace.findLastIndex(
      (event) => event._tag === 'Call' && event.target.name === 'record',
    )
    const releases = evaluated.trace
      .map((event, index) => ({ event, index }))
      .filter(({ event }) => event._tag === 'AllocationRelease')
    assert.strictEqual(releases.length, 1)
    assert.isBelow(lastRecord, releases[0]?.index ?? -1)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

    const compiled = yield* Driver.compile({
      compilation: {
        root: SourceFile.make(
          'vector-acceptance/element-release-order',
          ascii(elementReleaseOrder),
        ),
      },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
      profile: 'release',
      destination: join(destinationRoot, 'element-release-order'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.executable, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, 42, run.stderr)
  }),
)

const transferredEarlyDrop = `import silk.vector { Vector, make, append }

struct Entry {
  value: I32
  marker: Vector<I32>
}

fn record(value: I32) -> Unit { return Unit.make() }

impl Drop for Entry {
  fn drop(self: &mut Entry) -> Unit {
    return record(self.value)
  }
}

fn consume(values: Vector<Entry>) -> I32 {
  drop values
  return 40
}

effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let mut values = make<Entry>()
  let entry0 = Entry { value: 11, marker: make<I32>() }
  let pending0 = append<Entry>(&mut values, move entry0) |> Allocator.provide(&mut allocator)
  let appended0 = run pending0
  let entry1 = Entry { value: 13, marker: make<I32>() }
  let pending1 = append<Entry>(&mut values, move entry1) |> Allocator.provide(&mut allocator)
  let appended1 = run pending1
  let consumed = consume(move values)
  return consumed + 2
}

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), recover) }`

it.effect('transfers vector ownership and drops it early on all three engines', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'vector-acceptance/transferred-early-drop',
      ascii(transferredEarlyDrop),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42)
    const recorded = evaluated.trace.flatMap((event) =>
      event._tag === 'Binding' && event.target.name === 'record' && event.value._tag === 'I32Value'
        ? [event.value.value]
        : [],
    )
    assert.deepEqual(recorded, [11, 13])
    const releaseIndices = evaluated.trace.flatMap((event, index) =>
      event._tag === 'AllocationRelease' ? [index] : [],
    )
    assert.strictEqual(releaseIndices.length, 1)
    const consumeReturn = evaluated.trace.findIndex(
      (event) => event._tag === 'Return' && event.function.name === 'consume',
    )
    assert.isBelow(releaseIndices[0] ?? -1, consumeReturn)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

    const compiled = yield* Driver.compile({
      compilation: {
        root: SourceFile.make(
          'vector-acceptance/transferred-early-drop',
          ascii(transferredEarlyDrop),
        ),
      },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
      profile: 'release',
      destination: join(destinationRoot, 'transferred-early-drop'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.executable, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, 42, run.stderr)
  }),
)
