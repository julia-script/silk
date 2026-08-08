import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-drop-hook-execution-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

/** Fallthrough: the guard leaves scope, its hook runs, then its Allocation field releases. */
const fallthrough = `struct Guard {
  tag: I32
  storage: Allocation
}

impl Drop for Guard {
  fn drop(self: &mut Guard) -> Unit { return Unit.make() }
}

effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[I32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Allocator.provide(&mut allocator)
  let allocation = run recipe
  let guard = Guard { tag: 5, storage: move allocation }
  return 42
}

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), recover) }`

/** Early drop: explicit drop releases at that statement and block cleanup does not repeat it. */
const earlyDrop = `struct Guard {
  tag: I32
  storage: Allocation
}

impl Drop for Guard {
  fn drop(self: &mut Guard) -> Unit { return Unit.make() }
}

effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[I32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Allocator.provide(&mut allocator)
  let allocation = run recipe
  let guard = Guard { tag: 5, storage: move allocation }
  drop guard
  return 42
}

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), recover) }`

/** Typed failure: a guard live at a failing run releases through its hook before propagating. */
const failurePropagation = `struct Guard {
  tag: I32
  storage: Allocation
}

impl Drop for Guard {
  fn drop(self: &mut Guard) -> Unit { return Unit.make() }
}

struct ExhaustedAllocator { tag: I32 }

effect fn allocate(self: &mut ExhaustedAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  fail OutOfMemory {}
}

impl Allocator for ExhaustedAllocator { allocate: ExhaustedAllocator.allocate }

effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let mut empty = ExhaustedAllocator { tag: 0 }
  let layout = Layout.of<[I32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Allocator.provide(&mut allocator)
  let allocation = run recipe
  let guard = Guard { tag: 5, storage: move allocation }
  let second = Layout.of<[I32; 2]>()
  let refused = Allocator.allocate(move second) |> Allocator.provide(&mut empty)
  let never = run refused
  drop never
  return 42
}

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), recover) }`

const hookCallsOf = (run: ReturnType<typeof Analysis.evaluate>) =>
  run._tag === 'Completed'
    ? run.trace.filter(
        (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'),
      )
    : []

it.effect('runs Drop hooks before field cleanup exactly once on every structured exit', () =>
  Effect.gen(function* () {
    for (const [name, source, expected, expectedHooks, expectedReleases] of [
      ['fallthrough', fallthrough, 42, 1, 1],
      ['early-drop', earlyDrop, 42, 1, 1],
      ['failure-propagation', failurePropagation, 7, 1, 1],
    ] as const) {
      const snapshot = yield* Analysis.ofSource(
        `drop-hook/${name}`,
        ascii(source),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], name)
      const run = Analysis.evaluate(snapshot)
      assert.strictEqual(run._tag, 'Completed', name)
      if (run._tag !== 'Completed') continue
      assert.strictEqual(run.result.value, expected, name)
      assert.strictEqual(hookCallsOf(run).length, expectedHooks, `${name} hook calls`)
      const releases = run.trace.filter((event) => event._tag === 'AllocationRelease')
      assert.strictEqual(releases.length, expectedReleases, `${name} releases`)
      // The hook runs before its own field cleanup releases the allocation.
      const hookIndex = run.trace.findIndex(
        (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'),
      )
      const releaseIndex = run.trace.findIndex((event) => event._tag === 'AllocationRelease')
      assert.isBelow(hookIndex, releaseIndex, `${name} hook-before-field order`)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), expected, `${name} wasm`)

      const compiled = yield* Driver.compile({
        compilation: { root: SourceFile.make(`drop-hook/${name}`, ascii(source)) },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        profile: 'release',
        destination: join(destinationRoot, name),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled', name)
      if (compiled._tag !== 'Compiled') continue
      const nativeRun = spawnSync(compiled.executable, [], { encoding: 'utf8' })
      assert.strictEqual(nativeRun.status, expected, `${name} native: ${nativeRun.stderr}`)
    }
  }),
)

/** One parametric conformance serves two instantiations, each with its own hook instance. */
const parametric = `struct Guard<T> {
  value: T
  storage: Allocation
}

impl<T> Drop for Guard<T> {
  fn drop(self: &mut Guard<T>) -> Unit { return Unit.make() }
}

effect fn hold<T>(value: T) -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[I32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Allocator.provide(&mut allocator)
  let allocation = run recipe
  let guard = Guard<T> { value: move value, storage: move allocation }
  return 21
}

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

pub fn main() -> I32 {
  let first = run Effect.catch<OutOfMemory>(hold<I32>(1), recover)
  let second = run Effect.catch<OutOfMemory>(hold<Bool>(true), recover)
  return first + second
}`

/** A parametric Drop over a struct that is all-Copy at some instantiation. */
const copyInstantiation = `struct Holder<T> {
  value: T
}

impl<T> Drop for Holder<T> {
  fn drop(self: &mut Holder<T>) -> Unit { return Unit.make() }
}

fn keep<T>(value: T) -> I32 {
  let held = Holder<T> { value: move value }
  return 1
}

pub fn main() -> I32 { return keep<I32>(41) + 1 }`

it.effect('rejects a parametric Drop instantiated at a Copy provider', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'drop-hook/copy-instantiation',
      ascii(copyInstantiation),
      'wasm32-unknown-unknown',
    )
    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.message),
      'Invalid Drop hook: Copy type drop-hook/copy-instantiation.Holder<I32> cannot implement Drop',
    )
  }),
)

it.effect('monomorphizes one parametric Drop conformance per reachable instantiation', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'drop-hook/parametric',
      ascii(parametric),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const instances = Analysis.instancesOf(snapshot).instances.filter((instance) =>
      instance.key.declaration.name.startsWith('drop@impl'),
    )
    assert.strictEqual(instances.length, 2)
    assert.deepEqual(
      instances.map((instance) => instance.key.typeArguments).sort(),
      [['Bool'], ['I32']],
    )

    const run = Analysis.evaluate(snapshot)
    assert.strictEqual(run._tag, 'Completed')
    if (run._tag !== 'Completed') return
    assert.strictEqual(run.result.value, 42)
    assert.strictEqual(hookCallsOf(run).length, 2)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

    const compiled = yield* Driver.compile({
      compilation: { root: SourceFile.make('drop-hook/parametric', ascii(parametric)) },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
      profile: 'release',
      destination: join(destinationRoot, 'parametric'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag !== 'Compiled') return
    const nativeRun = spawnSync(compiled.executable, [], { encoding: 'utf8' })
    assert.strictEqual(nativeRun.status, 42, `parametric native: ${nativeRun.stderr}`)
  }),
)
