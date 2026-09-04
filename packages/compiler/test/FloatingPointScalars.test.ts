import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as FloatingPoint from '../src/FloatingPoint.js'
import * as Scalar from '../src/Scalar.js'

it('rounds a 64-bit integer to f32 in a single step', () => {
  // 2^60 + 2^36 + 1 sits one above the f32 tie; double rounding through f64 loses the `+ 1`
  // and lands on 2^60, while single rounding must go up to 2^60 + 2^37.
  const value = (1n << 60n) + (1n << 36n) + 1n
  const single = FloatingPoint.fromBigInt(value, 32)
  const expected = FloatingPoint.toNumber(single)
  assert.strictEqual(expected, 2 ** 60 + 2 ** 37)
  assert.notStrictEqual(
    expected,
    FloatingPoint.toNumber(FloatingPoint.fromNumber(Number(value), 32)),
  )
  // Within f64's exact range the two paths agree.
  assert.deepEqual(FloatingPoint.fromBigInt(-12345n, 32), FloatingPoint.fromNumber(-12345, 32))
  assert.deepEqual(FloatingPoint.fromBigInt(value, 64), FloatingPoint.fromNumber(Number(value), 64))
})

it('rounds decimal source directly to canonical IEEE bits', () => {
  assert.deepEqual(FloatingPoint.fromDecimal('1.000000059604644775390625', 32), {
    width: 32,
    bits: 0x3f800000n,
  })
  assert.deepEqual(FloatingPoint.fromDecimal('1.000000059604644775390626', 32), {
    width: 32,
    bits: 0x3f800001n,
  })
  assert.deepEqual(FloatingPoint.fromDecimal('-0.0', 64), {
    width: 64,
    bits: 0x8000000000000000n,
  })
})

it.effect('fails to compile a float literal whose exponent has no digits', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'float/malformed-exponent',
      new TextEncoder().encode('pub fn main() -> f64 {\n  return 1e\n}'),
      'wasm32-unknown-unknown',
    )
    const codes = Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)
    assert.include(codes, 'LEX0006')
    assert.strictEqual(codes.filter((code) => code === 'LEX0006').length, 1)
  }),
)

it('publishes canonical float catalog entries', () => {
  assert.deepEqual(
    Scalar.floats().map((scalar) => scalar.spelling),
    ['f32', 'f64'],
  )
  assert.strictEqual(Scalar.defaultFloat.spelling, 'f64')
})

it.effect('guards native float-to-integer conversion against out-of-range and NaN inputs', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'float/native-convert-guard',
      new TextEncoder().encode(
        'import silk.f64 as f64\n' +
          'fn convert(value: f64) -> i32 { return f64.toI32(value) }\n' +
          'pub fn main() -> i32 { return convert(42.75) }',
      ),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const llvm = yield* Analysis.codegen(snapshot, { mode: 'release' })
    // The bare `fptosi` is poison on out-of-range or NaN input, so lowering must precede it
    // with a range guard that branches to the shared deterministic trap.
    assert.include(llvm.ir, 'fptosi')
    assert.include(llvm.ir, '_below')
    assert.include(llvm.ir, '_above')
    assert.include(llvm.ir, 'trap_site')
    assert.include(llvm.ir, '@silk_trap_report_v1')
  }),
)
