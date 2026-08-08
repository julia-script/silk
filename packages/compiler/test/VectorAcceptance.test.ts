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

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

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
)
