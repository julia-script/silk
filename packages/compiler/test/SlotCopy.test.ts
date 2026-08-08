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

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-slot-copy-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

/** Copy reads the same initialized slot twice without consuming it; take still works after. */
const copyRead = `effect fn store() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[I32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Allocator.provide(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<I32>(move allocation, 2)
    let written = Slot.write(RawBuffer.slot(&mut buffer, 0), 21)
    let first = Slot.copy(RawBuffer.slot(&mut buffer, 0))
    let second = Slot.copy(RawBuffer.slot(&mut buffer, 0))
    let taken = Slot.take(RawBuffer.slot(&mut buffer, 0))
    drop buffer
    return first + second - taken + 21
  }
  return 0
}

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(store(), recover) }`

it.effect('copies initialized Copy slots without consuming them on all three engines', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'slot-copy/read',
      ascii(copyRead),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42)
    assert.strictEqual(
      evaluated.trace.filter((event) => event._tag === 'SlotCopy').length,
      2,
    )

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

    const compiled = yield* Driver.compile({
      compilation: { root: SourceFile.make('slot-copy/read', ascii(copyRead)) },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
      profile: 'release',
      destination: join(destinationRoot, 'read'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.executable, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, 42, run.stderr)
  }),
)

/** Copying a non-Copy element is rejected when the concrete instantiation is verified. */
const nonCopy = `struct Guard { storage: Allocation }
effect fn store() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[Guard; 1]>()
  let recipe = Allocator.allocate(move layout) |> Allocator.provide(&mut allocator)
  let allocation = run recipe
  let inner = Layout.of<[I32; 1]>()
  let innerRecipe = Allocator.allocate(move inner) |> Allocator.provide(&mut allocator)
  let payload = run innerRecipe
  unsafe {
    let mut buffer = RawBuffer.from<Guard>(move allocation, 1)
    let element = Guard { storage: move payload }
    let written = Slot.write(RawBuffer.slot(&mut buffer, 0), move element)
    let copied = Slot.copy(RawBuffer.slot(&mut buffer, 0))
    let taken = Slot.take(RawBuffer.slot(&mut buffer, 0))
    drop taken
    drop copied
    drop buffer
    return 42
  }
  return 0
}

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(store(), recover) }`

it.effect('rejects copying a non-Copy element at MIR verification', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource('slot-copy/non-copy', ascii(nonCopy))
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Blocked')
    if (evaluated._tag !== 'Blocked') return
    assert.strictEqual(evaluated.reason._tag, 'InvalidMir')
  }),
)
