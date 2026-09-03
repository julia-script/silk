import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
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
  shimCache: NativeToolchain.makeShimCache(),
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

/** `run` of an effect fn whose success is a requirement-carrying Effect, provided afterwards. */
const runThenProvide = `${counter}
effect fn outer() -> Effect<i32 ? &Counter> {
  return read()
}
pub fn main() -> i32 {
  let b = Cell { n: 2 }
  let inner = run outer()
  return run Effect.provide<Counter>(move inner, &b)
}`

/** The outer execution is itself provided; the inner Effect still needs its own provider. */
const provideEachLayer = `${counter}
effect fn outer() -> Effect<i32 ? &Counter> ? &Counter {
  return read()
}
pub fn main() -> i32 {
  let a = Cell { n: 1 }
  let b = Cell { n: 2 }
  let inner = run Effect.provide<Counter>(outer(), &a)
  return run Effect.provide<Counter>(move inner, &b)
}`

/** The outer execution uses its own provider before returning the inner Effect. */
const outerUsesItsProvider = `${counter}
effect fn outer() -> Effect<i32 ? &Counter> ? &Counter {
  let here = run Counter.get()
  return read()
}
pub fn main() -> i32 {
  let a = Cell { n: 1 }
  let b = Cell { n: 2 }
  let inner = run Effect.provide<Counter>(outer(), &a)
  return run Effect.provide<Counter>(move inner, &b)
}`

/** `Effect.flatten` joins both layers, so one provider covers the joined requirement row. */
const flattenBothLayers = `${counter}
effect fn outer() -> Effect<i32 ? &Counter> ? &Counter {
  let here = run Counter.get()
  return read()
}
pub fn main() -> i32 {
  let a = Cell { n: 3 }
  return run Effect.provide<Counter>(Effect.flatten(outer()), &a)
}`

/** Providing the outer layer does not close the inner Effect's requirement row. */
const innerNotClosed = `${counter}
effect fn outer() -> Effect<i32 ? &Counter> ? &Counter {
  return read()
}
pub fn main() -> i32 {
  let a = Cell { n: 1 }
  let inner = run Effect.provide<Counter>(outer(), &a)
  return run inner
}`

const evaluatesTo = (name: string, source: string, expected: number) =>
  it.effect(
    `${name} on the evaluator and Wasm`,
    () =>
      Effect.gen(function* () {
        const snapshot = yield* Analysis.ofSourceRealized(
          `effect-typing/${name}`,
          ascii(source),
          'wasm32-unknown-unknown',
        )
        assert.deepEqual(Analysis.diagnostics(snapshot), [])

        const evaluated = Analysis.evaluate(snapshot)
        assert.strictEqual(
          evaluated._tag,
          'Completed',
          Json.stringify(evaluated, (_, value) =>
            typeof value === 'bigint' ? value.toString() : value,
          ),
        )
        if (evaluated._tag !== 'Completed') return
        assert.strictEqual(evaluated.result.value, BigInt(expected))

        const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
        const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
        assert.strictEqual((instance.exports.silk_main as () => number)(), expected)
      }),
    120_000,
  )

evaluatesTo('run-then-provide', runThenProvide, 2)
evaluatesTo('provide-each-layer', provideEachLayer, 2)
evaluatesTo('outer-uses-its-provider', outerUsesItsProvider, 2)
evaluatesTo('flatten-both-layers', flattenBothLayers, 3)

it.effect('reports SEM0071 when only the outer layer is provided', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'effect-typing/inner-not-closed',
      ascii(innerNotClosed),
      'wasm32-unknown-unknown',
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
