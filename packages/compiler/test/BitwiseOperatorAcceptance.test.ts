import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('rejects mixed operand types exactly as the named operation rejects them', () =>
  Effect.gen(function* () {
    const viaOperator = yield* Analysis.ofSourceRealized(
      'bitwise-operator/mixed-operator',
      ascii(`fn mixed(a: u32, b: i32) -> u32 { return a & b }
pub fn main() -> i32 { return 0 }`),
    )
    const viaFunction = yield* Analysis.ofSourceRealized(
      'bitwise-operator/mixed-function',
      ascii(`import silk.u32 as u32
fn mixed(a: u32, b: i32) -> u32 { return u32.bitAnd(a, b) }
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
