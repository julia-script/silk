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

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-bitwise-operator-acceptance-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

const parity = `pub fn checksum(value: u32, mask: u32) -> u32 {
  let masked = value & mask
  let flipped = ~masked
  return flipped ^ mask
}

fn named(value: u32, mask: u32) -> u32 {
  return u32.bitXor(u32.bitNot(u32.bitAnd(value, mask)), mask)
}

fn agrees(a: u32, b: u32) -> u32 {
  if (a & b) == u32.bitAnd(a, b) {} else { return 1 }
  if (a | b) == u32.bitOr(a, b) {} else { return 2 }
  if (a ^ b) == u32.bitXor(a, b) {} else { return 3 }
  if ~a == u32.bitNot(a) {} else { return 4 }
  if checksum(a, b) == named(a, b) {} else { return 5 }
  return 0
}

fn leftAssociative(a: u32, b: u32, c: u32) -> u32 { return a | b & c }

fn doubled(value: u32) -> u32 { return value * 2 }

fn piped(a: u32, b: u32) -> u32 { return a & b |> doubled }

pub fn main() -> i32 {
  if agrees(3988292384, 255) == 0 {} else { return 1 }
  if agrees(0xff, 0x0f) == 0 {} else { return 2 }
  if agrees(0b1010, 0b0110) == 0 {} else { return 3 }
  if agrees(1_0, 3) == 0 {} else { return 4 }
  if leftAssociative(8, 1, 2) == u32.bitAnd(u32.bitOr(8, 1), 2) {} else { return 5 }
  if leftAssociative(8, 1, 2) == 0 {} else { return 6 }
  if piped(12, 10) == 16 {} else { return 7 }
  if checksum(12, 10) == 4294967293 {} else { return 8 }
  return 42
}`

it.effect(
  'compiles the four bitwise operators to their named operations on all three engines',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'bitwise-operator/parity',
        ascii(parity),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        JSON.stringify(evaluated, (_, value) =>
          typeof value === 'bigint' ? value.toString() : value,
        ),
      )
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

      const compiled = yield* Driver.compile({
        compilation: { root: SourceFile.make('bitwise-operator/parity', ascii(parity)) },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        profile: 'release',
        destination: join(destinationRoot, 'parity'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled')
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(run.status, 42, run.stderr)
    }),
  60_000,
)

it.effect('gives `a & b` the value the named operation gives', () =>
  Effect.gen(function* () {
    const source = `fn viaOperator(a: u32, b: u32) -> u32 { return a & b }
fn viaFunction(a: u32, b: u32) -> u32 { return u32.bitAnd(a, b) }

pub fn main() -> i32 {
  if viaOperator(3988292384, 255) == viaFunction(3988292384, 255) {} else { return 1 }
  if viaOperator(3988292384, 255) == 32 {} else { return 2 }
  return 42
}`
    const snapshot = yield* Analysis.ofSourceRealized('bitwise-operator/bit-and', ascii(source))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42)
  }),
)

it.effect('rejects mixed operand types exactly as the named operation rejects them', () =>
  Effect.gen(function* () {
    const viaOperator = yield* Analysis.ofSourceRealized(
      'bitwise-operator/mixed-operator',
      ascii(`fn mixed(a: u32, b: i32) -> u32 { return a & b }
pub fn main() -> i32 { return 0 }`),
    )
    const viaFunction = yield* Analysis.ofSourceRealized(
      'bitwise-operator/mixed-function',
      ascii(`fn mixed(a: u32, b: i32) -> u32 { return u32.bitAnd(a, b) }
pub fn main() -> i32 { return 0 }`),
    )

    const operatorCodes = Analysis.diagnostics(viaOperator).map((diagnostic) => diagnostic.code)
    assert.notStrictEqual(operatorCodes.length, 0)
    assert.deepEqual(
      operatorCodes,
      Analysis.diagnostics(viaFunction).map((diagnostic) => diagnostic.code),
    )
  }),
)

it.effect('binds the bitwise operators below equality', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'bitwise-operator/equality-binding',
      ascii(`fn f(a: u32, b: u32, c: u32) -> u32 { return a & b == c }
pub fn main() -> i32 { return 0 }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0012'],
      '`a & b == c` groups as `a & (b == c)`, so the right operand is a bool',
    )
  }),
)

it.effect('reports a type diagnostic instead of failing on a float bitwise operand', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'bitwise-operator/float',
      ascii(`fn f(a: f64, b: f64) -> f64 { return a & b }
fn g(a: f64) -> f64 { return ~a }
pub fn main() -> i32 { return 0 }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0012', 'SEM0012'],
    )
  }),
)
