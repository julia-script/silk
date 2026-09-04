import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as FloatingPoint from '../src/FloatingPoint.js'
import { floatMathPrograms } from './support/floatMath.js'

const encode = (value: string): Uint8Array => new TextEncoder().encode(value)

/**
 * Every LLVM math intrinsic the float modules could have reached for, and the libm symbols behind
 * them. `llvm.sqrt` is deliberately absent from this list: IEEE-754 requires a correctly rounded
 * square root, so that one intrinsic is bit-exact on every conforming target. `pow` and `log`
 * stay banned because their results are implementation-defined.
 */
const bannedIntrinsics = Object.freeze([
  'llvm.pow',
  'llvm.log',
  'llvm.exp',
  'llvm.sin',
  'llvm.cos',
  'llvm.fabs',
  'llvm.floor',
  'llvm.ceil',
  'llvm.trunc',
  'llvm.round',
  'llvm.rint',
  'llvm.nearbyint',
  'llvm.copysign',
  'llvm.minnum',
  'llvm.maxnum',
  'llvm.minimum',
  'llvm.maximum',
])

/** The libm entry points, spelled as an LLVM call target so a mangled Silk name cannot match. */
const bannedLibm = Object.freeze([
  '@sqrt(',
  '@sqrtf(',
  '@pow(',
  '@powf(',
  '@log(',
  '@logf(',
  '@fabs(',
  '@fabsf(',
  '@floor(',
  '@floorf(',
  '@ceil(',
  '@ceilf(',
  '@round(',
  '@roundf(',
  '@trunc(',
  '@truncf(',
  '@copysign(',
  '@copysignf(',
  '@fmin(',
  '@fmax(',
])

it.effect(
  'keeps every float math function off the platform libm and off every math intrinsic but sqrt',
  () =>
    Effect.gen(function* () {
      for (const program of floatMathPrograms) {
        const native = yield* Analysis.ofSourceRealized(
          `float-math/${program.name}-ir`,
          encode(program.source),
          'aarch64-apple-darwin',
        )
        const llvm = yield* Analysis.codegen(native, { mode: 'release' })
        for (const banned of bannedIntrinsics)
          assert.notInclude(llvm.ir, banned, `${program.name} reached ${banned}`)
        for (const banned of bannedLibm)
          assert.notInclude(llvm.ir, banned, `${program.name} reached libm ${banned}`)
        // Fast-math would let the backend rewrite these sequences and break bit parity.
        assert.notInclude(llvm.ir, 'fast', program.name)
      }
    }),
  120_000,
)

it.effect('lowers both square-root widths to the exact LLVM intrinsic', () =>
  Effect.gen(function* () {
    const source = `import silk.f32 as f32
import silk.f64 as f64
pub fn main() -> i32 {
  if f64.sqrt(1764.0) != 42.0 { return 1 }
  if f32.sqrt(1764.0) != 42.0 { return 2 }
  return 42
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'float-math/sqrt-lowering',
      encode(source),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const llvm = yield* Analysis.codegen(snapshot, { mode: 'release' })
    assert.include(llvm.ir, 'llvm.sqrt.f64')
    assert.include(llvm.ir, 'llvm.sqrt.f32')
  }),
)

/**
 * The floating-point model's square root must reproduce hardware bit patterns. Expectations were
 * taken from a host square root, which IEEE-754 requires be correctly rounded.
 */
it('rounds square root exactly at both widths', () => {
  const roots64: ReadonlyArray<readonly [bigint, bigint, string]> = [
    [4611686018427387904n, 4609047870845172685n, 'sqrt(2)'],
    [4613937818241073152n, 4610479282544200874n, 'sqrt(3)'],
    [4621819117588971520n, 4614303235046005587n, 'sqrt(10)'],
    [4602678819172646912n, 4604544271217802189n, 'sqrt(0.5)'],
    [4463399334375249557n, 4535287923269689789n, 'sqrt(2.5e-10)'],
    [9094988921128908188n, 6850974717710472879n, 'sqrt(1e300)'],
    [4655472969491939328n, 4631107791820423168n, 'sqrt(1764)'],
    [1n, 2188749418902061056n, 'sqrt of the least subnormal'],
    [2024n, 2213095440444558963n, 'sqrt(1e-320), a subnormal'],
  ]
  for (const [input, expected, label] of roots64)
    assert.strictEqual(FloatingPoint.squareRoot({ width: 64, bits: input }).bits, expected, label)

  const roots32: ReadonlyArray<readonly [bigint, bigint, string]> = [
    [1073741824n, 1068827891n, 'sqrt(2)'],
    [1077936128n, 1071494103n, 'sqrt(3)'],
    [1056964608n, 1060439283n, 'sqrt(0.5)'],
    [1155301376n, 1109917696n, 'sqrt(1764)'],
    [1900671690n, 1482907561n, 'sqrt(1e30)'],
    [1n, 439682291n, 'sqrt of the least subnormal'],
    [1000n, 481485484n, 'sqrt of a subnormal'],
  ]
  for (const [input, expected, label] of roots32)
    assert.strictEqual(FloatingPoint.squareRoot({ width: 32, bits: input }).bits, expected, label)
})

it('gives square root its IEEE special cases', () => {
  const root = (bits: bigint, width: 32 | 64 = 64): bigint =>
    FloatingPoint.squareRoot({ width, bits }).bits
  // Both zeros keep their sign.
  assert.strictEqual(root(0n), 0n)
  assert.strictEqual(root(0x8000000000000000n), 0x8000000000000000n)
  // Positive infinity roots to itself.
  assert.strictEqual(root(0x7ff0000000000000n), 0x7ff0000000000000n)
  // Every invalid or NaN input collapses to the one canonical NaN.
  assert.strictEqual(root(0xfff0000000000000n), 0x7ff8000000000000n)
  assert.strictEqual(root(0xbff0000000000000n), 0x7ff8000000000000n)
  assert.strictEqual(root(0x7ff8000000000000n), 0x7ff8000000000000n)
  // A signalling NaN payload is not propagated either.
  assert.strictEqual(root(0x7ff0000000000001n), 0x7ff8000000000000n)
  assert.strictEqual(root(0x7fc00000n, 32), 0x7fc00000n)
  assert.strictEqual(root(0xbf800000n, 32), 0x7fc00000n)
  assert.strictEqual(root(0x80000000n, 32), 0x80000000n)
})

/**
 * A square root is exact whenever its operand is a perfect square, so re-squaring must return the
 * operand. This sweeps a range no pinned table would cover.
 */
it('returns an exact root for every perfect square it can represent', () => {
  for (let value = 1n; value <= 4096n; value += 1n) {
    const square = value * value
    const encoded = FloatingPoint.fromDecimal(square.toString(), 64)
    assert.isDefined(encoded)
    if (encoded === undefined) continue
    const expected = FloatingPoint.fromDecimal(value.toString(), 64)
    assert.deepEqual(FloatingPoint.squareRoot(encoded), expected, `sqrt(${square})`)
  }
})
