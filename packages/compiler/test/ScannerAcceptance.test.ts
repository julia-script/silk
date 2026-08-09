import { spawnSync } from 'node:child_process'
import { mkdtempSync, readFileSync, rmSync } from 'node:fs'
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

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-scanner-acceptance-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

export const scannerSource = readFileSync(
  new URL('./fixtures/scanner-acceptance/Main.silk', import.meta.url),
  'utf8',
)

const quotaScannerSource = (quota: number): string =>
  scannerSource
    .replace(
      'let mut allocator = SystemAllocator.make()',
      `let mut allocator = QuotaAllocator { remaining: ${quota} }`,
    )
    .replace(
      'struct U8 { value: i32 }',
      `struct QuotaAllocator { remaining: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  if self.remaining == 0 { fail OutOfMemory {} }
  self.remaining = self.remaining - 1
  let mut inner = SystemAllocator.make()
  let pending = Allocator.allocate(move layout) |> Allocator.provide(&mut inner)
  let allocation = run pending
  return move allocation
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

struct U8 { value: i32 }`,
    )

it.effect(
  'returns an owned token vector through two reallocations on all three engines',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'scanner-acceptance/main',
        ascii(scannerSource),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42)
      const tokenKinds = evaluated.trace.flatMap((event) =>
        event._tag === 'Binding' &&
        event.target.name === 'observe' &&
        event.value._tag === 'I32Value'
          ? [event.value.value]
          : [],
      )
      assert.deepEqual(tokenKinds, [1, 2, 3, 1, 2, 3, 1, 2, 3, 1])
      assert.strictEqual(
        evaluated.trace.filter((event) => event._tag === 'AllocationAcquire').length,
        3,
      )
      assert.strictEqual(
        evaluated.trace.filter((event) => event._tag === 'AllocationRelease').length,
        3,
      )

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('scanner-acceptance/main', ascii(scannerSource)),
        },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        profile: 'release',
        destination: join(destinationRoot, 'scanner'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled')
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(run.status, 42, run.stderr)
    }),
  15_000,
)

it.effect(
  'rolls back every scanner allocation failure without leaking on all three engines',
  () =>
    Effect.gen(function* () {
      for (const quota of [0, 1, 2, 3]) {
        const label = `q${quota}`
        const source = quotaScannerSource(quota)
        const expected = quota === 3 ? 42 : 7
        const snapshot = yield* Analysis.ofSourceRealized(
          `scanner-acceptance/${label}`,
          ascii(source),
          'wasm32-unknown-unknown',
        )
        assert.deepEqual(Analysis.diagnostics(snapshot), [], label)

        const evaluated = Analysis.evaluate(snapshot)
        assert.strictEqual(evaluated._tag, 'Completed', label)
        if (evaluated._tag !== 'Completed') continue
        assert.strictEqual(evaluated.result.value, expected, label)
        const events = Analysis.allocationTraceEventsOf(evaluated).map((event) => event._tag)
        assert.strictEqual(
          events.filter((event) => event === 'AllocationAcquire').length,
          quota,
          label,
        )
        assert.strictEqual(
          events.filter((event) => event === 'AllocationRelease').length,
          quota,
          label,
        )

        const again = Analysis.evaluate(snapshot)
        assert.strictEqual(again._tag, 'Completed', label)
        if (again._tag === 'Completed') assert.strictEqual(again.result.value, expected, label)

        const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
        const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
        assert.strictEqual((instance.exports.silk_main as () => number)(), expected, label)

        const compiled = yield* Driver.compile({
          compilation: {
            root: SourceFile.make(`scanner-acceptance/${label}`, ascii(source)),
          },
          toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
          profile: 'release',
          destination: join(destinationRoot, label),
        }).pipe(Effect.provide(SourceResolver.empty))
        assert.strictEqual(compiled._tag, 'Compiled', label)
        if (compiled._tag !== 'Compiled') continue
        const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
        assert.strictEqual(run.status, expected, `${label}: ${run.stderr}`)
      }
    }),
  30_000,
)
