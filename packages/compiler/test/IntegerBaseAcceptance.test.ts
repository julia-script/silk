import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const parity = `const upperMask: i32 = 0xff00
const lowBits: i32 = 0b1010
const permission: i32 = 0o644

fn identity(value: i32) -> i32 { return value }

pub fn main() -> i32 {
  let hexadecimal = 0xff
  let upperHexadecimal = 0XFF
  let binary = 0b1010
  let octal = 0o777
  if hexadecimal == 255 {} else { return 1 }
  if upperHexadecimal == hexadecimal {} else { return 2 }
  if binary == 10 {} else { return 3 }
  if octal == 511 {} else { return 4 }
  if upperMask == 65280 {} else { return 5 }
  if lowBits == 10 {} else { return 6 }
  if permission == 420 {} else { return 7 }
  return identity(hexadecimal) - 213
}`

it.effect(
  'reads hexadecimal, binary, and octal literals in their own base on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'integer-base/parity',
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
    }),
  60_000,
)

it.effect('keeps a prefixed literal equal to its decimal spelling in the same program', () =>
  Effect.gen(function* () {
    const source = `pub fn main() -> i32 { return 0xff - 255 }`
    const snapshot = yield* Analysis.ofSourceRealized('integer-base/equality', ascii(source))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 0)
  }),
)

it.effect('reports the existing out-of-range diagnostic for a prefixed literal', () =>
  Effect.gen(function* () {
    const source = `fn accept(value: u8) -> u8 { return value }
pub fn main() -> i32 { let value = accept(0x1ff) return 42 }`
    const snapshot = yield* Analysis.ofSourceRealized('integer-base/out-of-range', ascii(source))
    const outOfRange = Analysis.expressionsOf(snapshot, 'integer-base/out-of-range').filter(
      (expression) => expression._tag === 'Integer' && expression.integer._tag === 'OutOfRange',
    )
    assert.strictEqual(outOfRange.length, 1)
    const reported = Analysis.diagnostics(snapshot).filter(
      (diagnostic) => diagnostic.reason._tag === 'IntegerOutOfRange',
    )
    assert.strictEqual(reported.length, 1)
    assert.deepEqual(
      reported.map((diagnostic) =>
        diagnostic.reason._tag === 'IntegerOutOfRange' ? diagnostic.reason.spelling : undefined,
      ),
      ['0x1ff'],
    )
  }),
)

it.effect('rejects a base prefix without digits before parsing', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'integer-base/missing-digits',
      ascii('pub fn main() -> i32 { return 0x }'),
    )
    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'LEX0004',
    )
  }),
)
