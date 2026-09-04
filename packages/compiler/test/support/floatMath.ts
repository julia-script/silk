/**
 * The float math conformance programs, shared by three consumers so that one source text is what
 * the structural float tests and native corpus run from one source definition.
 *
 * Each program returns 42 only when every one of its assertions holds, and a distinct small code
 * otherwise, so a single exit status both proves the whole set and names the first failure.
 *
 * Assertions compare `toBits` rather than values wherever a sign or a payload is the point, since
 * `-0.0 != 0.0` is false and no comparison against a NaN is ever true.
 *
 * A NaN is obtained by dividing a runtime zero by itself. `f64.NAN` does not exist yet (#109), and
 * SEM0086 bans `0.0 / 0.0` as a constant initializer, but nothing stops a runtime division.
 */
export interface FloatMathProgram {
  readonly name: string
  readonly source: string
}

const f64Exact = `import silk.f64 as f64
pub fn main() -> i32 {
  // Negative zero survives every operation in the exact group.
  if f64.toBits(f64.abs(-0.0)) != f64.toBits(0.0) { return 1 }
  if f64.toBits(f64.floor(-0.0)) != f64.toBits(-0.0) { return 2 }
  if f64.toBits(f64.ceil(-0.0)) != f64.toBits(-0.0) { return 3 }
  if f64.toBits(f64.round(-0.0)) != f64.toBits(-0.0) { return 4 }
  if f64.toBits(f64.trunc(-0.0)) != f64.toBits(-0.0) { return 5 }
  // A magnitude below one collapses to a signed zero, not to positive zero.
  if f64.toBits(f64.trunc(-0.5)) != f64.toBits(-0.0) { return 6 }
  if f64.toBits(f64.ceil(-0.5)) != f64.toBits(-0.0) { return 7 }
  if f64.toBits(f64.round(-0.25)) != f64.toBits(-0.0) { return 8 }
  if f64.toBits(f64.trunc(0.5)) != f64.toBits(0.0) { return 9 }
  // abs clears the sign bit and nothing else.
  if f64.abs(-42.5) != 42.5 { return 10 }
  if f64.abs(42.5) != 42.5 { return 11 }
  if f64.abs(f64.MIN) != f64.MAX { return 12 }
  // Directed rounding across a negative half.
  if f64.floor(-2.5) != -3.0 { return 13 }
  if f64.ceil(-2.5) != -2.0 { return 14 }
  if f64.trunc(-2.5) != -2.0 { return 15 }
  if f64.round(-2.5) != -3.0 { return 16 }
  // ... and across a positive half.
  if f64.floor(2.5) != 2.0 { return 17 }
  if f64.ceil(2.5) != 3.0 { return 18 }
  if f64.trunc(2.5) != 2.0 { return 19 }
  if f64.round(2.5) != 3.0 { return 20 }
  // A half rounds away from zero; the value one unit below a half does not reach it.
  if f64.round(0.5) != 1.0 { return 21 }
  if f64.round(0.49999999999999994) != 0.0 { return 22 }
  if f64.toBits(f64.round(-0.49999999999999994)) != f64.toBits(-0.0) { return 23 }
  // An already integral value passes through, including one too large to hold a fraction.
  if f64.floor(7.0) != 7.0 { return 24 }
  if f64.ceil(7.0) != 7.0 { return 25 }
  if f64.trunc(f64.MAX) != f64.MAX { return 26 }
  if f64.ceil(f64.MIN) != f64.MIN { return 27 }
  if f64.round(4503599627370497.0) != 4503599627370497.0 { return 28 }
  // Either infinity is already integral.
  if f64.floor(f64.INFINITY) != f64.INFINITY { return 29 }
  if f64.round(f64.INFINITY) != f64.INFINITY { return 30 }
  if f64.trunc(f64.negate(f64.INFINITY)) != f64.negate(f64.INFINITY) { return 31 }
  if f64.ceil(f64.negate(f64.INFINITY)) != f64.negate(f64.INFINITY) { return 32 }
  // copysign transfers the sign bit and preserves the magnitude.
  if f64.toBits(f64.copysign(3.0, -1.0)) != f64.toBits(-3.0) { return 33 }
  if f64.toBits(f64.copysign(-3.0, 1.0)) != f64.toBits(3.0) { return 34 }
  if f64.toBits(f64.copysign(3.0, -0.0)) != f64.toBits(-3.0) { return 35 }
  if f64.toBits(f64.copysign(0.0, -1.0)) != f64.toBits(-0.0) { return 36 }
  if f64.toBits(f64.copysign(-0.0, 1.0)) != f64.toBits(0.0) { return 37 }
  return 42
}`

const f32Exact = `import silk.f32 as f32
pub fn main() -> i32 {
  if f32.toBits(f32.abs(-0.0)) != f32.toBits(0.0) { return 1 }
  if f32.toBits(f32.floor(-0.0)) != f32.toBits(-0.0) { return 2 }
  if f32.toBits(f32.ceil(-0.0)) != f32.toBits(-0.0) { return 3 }
  if f32.toBits(f32.round(-0.0)) != f32.toBits(-0.0) { return 4 }
  if f32.toBits(f32.trunc(-0.0)) != f32.toBits(-0.0) { return 5 }
  if f32.toBits(f32.trunc(-0.5)) != f32.toBits(-0.0) { return 6 }
  if f32.toBits(f32.ceil(-0.5)) != f32.toBits(-0.0) { return 7 }
  if f32.abs(-42.5) != 42.5 { return 8 }
  if f32.abs(f32.MIN) != f32.MAX { return 9 }
  if f32.floor(-2.5) != -3.0 { return 10 }
  if f32.ceil(-2.5) != -2.0 { return 11 }
  if f32.trunc(-2.5) != -2.0 { return 12 }
  if f32.round(-2.5) != -3.0 { return 13 }
  if f32.floor(2.5) != 2.0 { return 14 }
  if f32.ceil(2.5) != 3.0 { return 15 }
  if f32.trunc(2.5) != 2.0 { return 16 }
  if f32.round(2.5) != 3.0 { return 17 }
  // The float one unit below a half must not round up.
  if f32.round(0.49999997) != 0.0 { return 18 }
  if f32.trunc(f32.MAX) != f32.MAX { return 19 }
  if f32.floor(f32.INFINITY) != f32.INFINITY { return 20 }
  if f32.trunc(f32.negate(f32.INFINITY)) != f32.negate(f32.INFINITY) { return 21 }
  if f32.toBits(f32.copysign(3.0, -1.0)) != f32.toBits(-3.0) { return 22 }
  if f32.toBits(f32.copysign(-3.0, 1.0)) != f32.toBits(3.0) { return 23 }
  if f32.toBits(f32.copysign(0.0, -1.0)) != f32.toBits(-0.0) { return 24 }
  return 42
}`

const minimumMaximum = `import silk.f32 as f32
import silk.f64 as f64
pub fn main() -> i32 {
  if f64.min(1.0, 2.0) != 1.0 { return 1 }
  if f64.max(1.0, 2.0) != 2.0 { return 2 }
  if f64.min(2.0, 1.0) != 1.0 { return 3 }
  if f64.max(2.0, 1.0) != 2.0 { return 4 }
  if f64.min(-2.0, 1.0) != -2.0 { return 5 }
  if f64.max(-2.0, 1.0) != 1.0 { return 6 }
  // Negative zero orders below positive zero, in either argument position.
  if f64.toBits(f64.min(-0.0, 0.0)) != f64.toBits(-0.0) { return 7 }
  if f64.toBits(f64.min(0.0, -0.0)) != f64.toBits(-0.0) { return 8 }
  if f64.toBits(f64.max(-0.0, 0.0)) != f64.toBits(0.0) { return 9 }
  if f64.toBits(f64.max(0.0, -0.0)) != f64.toBits(0.0) { return 10 }
  if f64.toBits(f64.min(0.0, 0.0)) != f64.toBits(0.0) { return 11 }
  if f64.toBits(f64.max(-0.0, -0.0)) != f64.toBits(-0.0) { return 12 }
  // Infinity is an ordinary bound.
  if f64.min(f64.INFINITY, f64.MAX) != f64.MAX { return 13 }
  if f64.max(f64.INFINITY, f64.MAX) != f64.INFINITY { return 14 }
  if f32.min(1.0, 2.0) != 1.0 { return 15 }
  if f32.max(1.0, 2.0) != 2.0 { return 16 }
  if f32.toBits(f32.min(-0.0, 0.0)) != f32.toBits(-0.0) { return 17 }
  if f32.toBits(f32.max(-0.0, 0.0)) != f32.toBits(0.0) { return 18 }
  return 42
}`

/**
 * Inputs and their exactly rounded roots, pinned as bit patterns. The expectations were derived
 * independently of the compiler, from a host square root, which IEEE-754 requires be correctly
 * rounded and which therefore agrees with any conforming implementation.
 */
const squareRoot = `import silk.f32 as f32
import silk.f64 as f64
pub fn main() -> i32 {
  // sqrt(2), sqrt(3), sqrt(10) — irrational roots that pin the rounding of the last bit.
  if f64.toBits(f64.sqrt(f64.fromBits(4611686018427387904))) != 4609047870845172685 { return 1 }
  if f64.toBits(f64.sqrt(f64.fromBits(4613937818241073152))) != 4610479282544200874 { return 2 }
  if f64.toBits(f64.sqrt(f64.fromBits(4621819117588971520))) != 4614303235046005587 { return 3 }
  // sqrt(0.5) and sqrt(2.5e-10), below one.
  if f64.toBits(f64.sqrt(f64.fromBits(4602678819172646912))) != 4604544271217802189 { return 4 }
  if f64.toBits(f64.sqrt(f64.fromBits(4463399334375249557))) != 4535287923269689789 { return 5 }
  // sqrt(1e300), near the top of the range.
  if f64.toBits(f64.sqrt(f64.fromBits(9094988921128908188))) != 6850974717710472879 { return 6 }
  // A subnormal input still roots into the normal range: the least subnormal, then 1e-320.
  if f64.toBits(f64.sqrt(f64.fromBits(1))) != 2188749418902061056 { return 7 }
  if f64.toBits(f64.sqrt(f64.fromBits(2024))) != 2213095440444558963 { return 8 }
  // Exact roots stay exact.
  if f64.sqrt(1764.0) != 42.0 { return 9 }
  if f64.sqrt(1.0) != 1.0 { return 10 }
  if f64.sqrt(0.25) != 0.5 { return 11 }
  if f64.sqrt(f64.INFINITY) != f64.INFINITY { return 12 }
  // Both zeros keep their sign, as IEEE-754 requires.
  if f64.toBits(f64.sqrt(0.0)) != f64.toBits(0.0) { return 13 }
  if f64.toBits(f64.sqrt(-0.0)) != f64.toBits(-0.0) { return 14 }
  // A negative operand is an invalid operation, whose NaN sign the hosts do not agree on, so the
  // module screens it and reports the canonical NaN instead.
  if f64.toBits(f64.sqrt(-1.0)) != 9221120237041090560 { return 15 }
  if f64.toBits(f64.sqrt(f64.MIN)) != 9221120237041090560 { return 16 }
  if f64.toBits(f64.sqrt(f64.negate(f64.INFINITY))) != 9221120237041090560 { return 17 }
  // The same set at binary32 width.
  if f32.toBits(f32.sqrt(f32.fromBits(1073741824))) != 1068827891 { return 18 }
  if f32.toBits(f32.sqrt(f32.fromBits(1077936128))) != 1071494103 { return 19 }
  if f32.toBits(f32.sqrt(f32.fromBits(1056964608))) != 1060439283 { return 20 }
  if f32.toBits(f32.sqrt(f32.fromBits(1))) != 439682291 { return 21 }
  if f32.toBits(f32.sqrt(f32.fromBits(1900671690))) != 1482907561 { return 22 }
  if f32.sqrt(1764.0) != 42.0 { return 23 }
  if f32.toBits(f32.sqrt(-0.0)) != f32.toBits(-0.0) { return 24 }
  if f32.toBits(f32.sqrt(-1.0)) != 2143289344 { return 25 }
  return 42
}`

/**
 * Requirement 10: every function reports the one canonical NaN encoding for a NaN input, so no
 * engine's payload propagation rule can show through.
 */
const canonicalNaN = `import silk.f32 as f32
import silk.f64 as f64
fn notANumber(zero: f64) -> f64 { return zero / zero }
fn notANumber32(zero: f32) -> f32 { return zero / zero }
pub fn main() -> i32 {
  let missing = notANumber(0.0)
  if !f64.isNaN(missing) { return 1 }
  if f64.toBits(f64.abs(missing)) != 9221120237041090560 { return 2 }
  if f64.toBits(f64.floor(missing)) != 9221120237041090560 { return 3 }
  if f64.toBits(f64.ceil(missing)) != 9221120237041090560 { return 4 }
  if f64.toBits(f64.round(missing)) != 9221120237041090560 { return 5 }
  if f64.toBits(f64.trunc(missing)) != 9221120237041090560 { return 6 }
  if f64.toBits(f64.sqrt(missing)) != 9221120237041090560 { return 7 }
  if f64.toBits(f64.copysign(missing, 1.0)) != 9221120237041090560 { return 8 }
  if f64.toBits(f64.copysign(missing, -1.0)) != 9221120237041090560 { return 9 }
  // A NaN in either operand propagates, which is the IEEE-754-2019 minimum and maximum rule.
  if f64.toBits(f64.min(missing, 1.0)) != 9221120237041090560 { return 10 }
  if f64.toBits(f64.min(1.0, missing)) != 9221120237041090560 { return 11 }
  if f64.toBits(f64.max(missing, 1.0)) != 9221120237041090560 { return 12 }
  if f64.toBits(f64.max(1.0, missing)) != 9221120237041090560 { return 13 }
  // A negative sign operand cannot make copysign hand back a signed NaN.
  if f64.isSignNegative(f64.copysign(missing, -1.0)) { return 14 }

  let missing32 = notANumber32(0.0)
  if !f32.isNaN(missing32) { return 15 }
  if f32.toBits(f32.abs(missing32)) != 2143289344 { return 16 }
  if f32.toBits(f32.floor(missing32)) != 2143289344 { return 17 }
  if f32.toBits(f32.ceil(missing32)) != 2143289344 { return 18 }
  if f32.toBits(f32.round(missing32)) != 2143289344 { return 19 }
  if f32.toBits(f32.trunc(missing32)) != 2143289344 { return 20 }
  if f32.toBits(f32.sqrt(missing32)) != 2143289344 { return 21 }
  if f32.toBits(f32.copysign(missing32, -1.0)) != 2143289344 { return 22 }
  if f32.toBits(f32.min(missing32, 1.0)) != 2143289344 { return 23 }
  if f32.toBits(f32.max(1.0, missing32)) != 2143289344 { return 24 }
  return 42
}`

export const floatMathPrograms: ReadonlyArray<FloatMathProgram> = Object.freeze([
  Object.freeze({ name: 'float-math-f64-exact', source: f64Exact }),
  Object.freeze({ name: 'float-math-f32-exact', source: f32Exact }),
  Object.freeze({ name: 'float-math-min-max', source: minimumMaximum }),
  Object.freeze({ name: 'float-math-sqrt', source: squareRoot }),
  Object.freeze({ name: 'float-math-canonical-nan', source: canonicalNaN }),
])
