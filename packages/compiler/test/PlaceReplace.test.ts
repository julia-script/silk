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

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-place-replace-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

/** Swap a scalar field through an exclusive reference and observe both halves. */
const scalarSwap = `struct Counter {
  value: I32
}
fn bump(self: &mut Counter) -> I32 {
  let old = Place.replace(self.value, 42)
  return old
}
pub fn main() -> I32 {
  let mut counter = Counter { value: 41 }
  let old = bump(&mut counter)
  return old + counter.value - 41
}`

/**
 * Swap a union field through an exclusive reference: the affine payload moves out through the
 * swap while the place stays initialized, so no partial move exists at any point.
 */
const unionSwap = `struct Empty {}
struct Full { value: I32 }
struct Cell {
  state: Empty | Full
}
fn take(self: &mut Cell) -> I32 {
  let old = Place.replace(self.state, Empty {})
  return match move old {
    Empty {} => 0
    Full { value } => value
  }
}
pub fn main() -> I32 {
  let mut cell = Cell { state: Full { value: 42 } }
  let first = take(&mut cell)
  let second = take(&mut cell)
  return first + second
}`

it.effect('swaps places atomically on all three engines', () =>
  Effect.gen(function* () {
    for (const [name, source, expected] of [
      ['scalar', scalarSwap, 42],
      ['union', unionSwap, 42],
    ] as const) {
      const snapshot = yield* Analysis.ofSource(
        `place-replace/${name}`,
        ascii(source),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], name)
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', name)
      if (evaluated._tag !== 'Completed') continue
      assert.strictEqual(evaluated.result.value, expected, name)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), expected, `${name} wasm`)

      const compiled = yield* Driver.compile({
        compilation: { root: SourceFile.make(`place-replace/${name}`, ascii(source)) },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        profile: 'release',
        destination: join(destinationRoot, name),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled', name)
      if (compiled._tag !== 'Compiled') continue
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(run.status, expected, `${name} native: ${run.stderr}`)
    }
  }),
)

it.effect('rejects invalid replace places with assignment diagnostics', () =>
  Effect.gen(function* () {
    const immutable = yield* Analysis.ofSource(
      'place-replace/immutable',
      ascii(`pub fn main() -> I32 {
  let value = 1
  let old = Place.replace(value, 2)
  return old
}`),
    )
    assert.include(
      Analysis.diagnostics(immutable).map((diagnostic) => diagnostic.code),
      'SEM0035',
    )

    const sharedRoot = yield* Analysis.ofSource(
      'place-replace/shared',
      ascii(`struct Counter {
  value: I32
}
fn peek(self: &Counter) -> I32 {
  let old = Place.replace(self.value, 2)
  return old
}
pub fn main() -> I32 { return 0 }`),
    )
    assert.include(
      Analysis.diagnostics(sharedRoot).map((diagnostic) => diagnostic.code),
      'SEM0036',
    )

    const arity = yield* Analysis.ofSource(
      'place-replace/arity',
      ascii(`pub fn main() -> I32 {
  let mut value = 1
  let old = Place.replace(value)
  return old
}`),
    )
    assert.include(
      Analysis.diagnostics(arity).map((diagnostic) => diagnostic.code),
      'SEM0007',
    )
  }),
)
