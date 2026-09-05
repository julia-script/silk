import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Driver from './support/TestDriver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const clang = existsSync('/opt/homebrew/opt/llvm/bin/clang')
  ? '/opt/homebrew/opt/llvm/bin/clang'
  : '/usr/bin/clang'
const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang,
  llvmAr: 'llvm-ar',
  runtimeObjectCache: NativeToolchain.makeRuntimeObjectCache(),
})
const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-nested-effect-row-stabilization-'))

afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

/**
 * SERV-009 / EFF-004: provision applies to one Effect layer. An `effect fn` whose success value is
 * an Effect carrying a requirement row hands that inner Effect out of `run` unprovided; the caller
 * provides it separately. The inner `read()` therefore observes the provider given to it, never the
 * one given to the outer execution.
 */
const counter = `import silk.effect { Effect }
service Counter {
  effect fn get() -> i32 ? &Counter
}
struct Cell { n: i32 }
impl Cell {
  effect fn getImpl(self: &Self) -> i32 { return self.n }
}
impl Counter for Cell { get: Cell.getImpl }
effect fn read() -> i32 ? &Counter { return run Counter.get() }`

/** The outer execution is itself provided; the inner Effect still needs its own provider. */
const provideEachLayer = `${counter}
effect fn outer() -> Effect<'static; i32 ? &Counter> ? &Counter {
  return read()
}
pub fn main() -> i32 {
  let a = Cell { n: 1 }
  let b = Cell { n: 2 }
  let inner = run Effect.provide<Counter>(outer(), &a)
  return run Effect.provide<Counter>(move inner, &b)
}`

/** Providing the outer layer does not close the inner Effect's requirement row. */
const innerNotClosed = `${counter}
effect fn outer() -> Effect<'static; i32 ? &Counter> ? &Counter {
  return read()
}
pub fn main() -> i32 {
  let a = Cell { n: 1 }
  let inner = run Effect.provide<Counter>(outer(), &a)
  return run inner
}`

it.effect('reports SEM0071 when only the outer layer is provided', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'effect-typing/inner-not-closed',
      ascii(innerNotClosed),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0071'],
    )
  }),
)

it.effect(
  'provides each Effect layer separately on native',
  () =>
    Effect.gen(function* () {
      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('effect-typing/provide-each-layer', ascii(provideEachLayer)),
        },
        toolchain,
        profile: 'release',
        artifactKind: 'NativeExecutable',
        destination: join(destinationRoot, 'provide-each-layer'),
      }).pipe(Effect.provide(SourceResolver.empty))

      assert.strictEqual(compiled._tag, 'Compiled')
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(run.signal, null, run.stderr)
      assert.strictEqual(run.status, 2, run.stderr)
    }),
  180_000,
)
