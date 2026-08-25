import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as FloatingPoint from '../src/FloatingPoint.js'
import * as Scalar from '../src/Scalar.js'

const source = `import silk.f32 as f32
import silk.f64 as f64
fn add(left: f64, right: f64) -> f64 { return left + right }
fn bits32(value: u32) -> u32 { return f32.toBits(f32.fromBits(value)) }
fn bits64(value: u64) -> u64 { return f64.toBits(f64.fromBits(value)) }
fn classify(value: f64) -> bool { return f64.isNaN(value) }
fn section() -> f64 { let addHalf = f64.add(0.5) return addHalf(12.5) }
pub fn main() -> i32 {
  if add(1.25e1, 0.5) != 13.0 { return 0 }
  if bits32(2143289344) != 2143289344 { return 1 }
  if bits64(9223372036854775808) != 9223372036854775808 { return 2 }
  if !classify(f64.fromBits(9221120237041090560)) { return 3 }
  if !f64.totalOrder(f64.fromBits(9223372036854775808), 0.0) { return 4 }
  if section() != 13.0 { return 5 }
  return f64.toI32(42.75)
}`

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

it.effect('keeps a well-formed exponent compiling to its exact value', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'float/well-formed-exponent',
      new TextEncoder().encode(
        'pub fn main() -> i32 {\n' +
          '  if 1e5 != 100000.0 { return 1 }\n' +
          '  if 1e+5 != 100000.0 { return 2 }\n' +
          '  if 1e-5 != 0.00001 { return 3 }\n' +
          '  if 1.5e10 != 15000000000.0 { return 4 }\n' +
          '  return 42\n}',
      ),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it('publishes canonical float catalog entries', () => {
  assert.deepEqual(
    Scalar.floats().map((scalar) => scalar.spelling),
    ['f32', 'f64'],
  )
  assert.strictEqual(Scalar.defaultFloat.spelling, 'f64')
})

it.effect('keeps evaluator, LLVM, and direct Wasm float semantics aligned', () =>
  Effect.gen(function* () {
    const native = yield* Analysis.ofSourceRealized(
      'float/matrix',
      new TextEncoder().encode(source),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(native), [])
    const outcome = Analysis.evaluate(native)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    const llvm = yield* Analysis.codegen(native, { mode: 'release' })
    assert.isAbove(llvm.bitcode.length, 0)
    assert.notInclude(llvm.ir, 'fast')

    const wasm = yield* Analysis.ofSourceRealized(
      'float/matrix-wasm',
      new TextEncoder().encode(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(wasm), [])
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    assert.isAbove(artifact.bytes.length, 0)
    assert.include(artifact.wat, 'f64.add')
  }),
)

const floatArguments = (scalar: Scalar.FloatScalar, operation: Scalar.Operation): string => {
  switch (operation.code) {
    case 'Subtract':
      return '44.0, 2.0'
    case 'Multiply':
      return '21.0, 2.0'
    case 'Divide':
      return '84.0, 2.0'
    case 'Remainder':
      return '42.0, 100.0'
    case 'Negate':
      return '-42.0'
    case 'Equals':
      return '42.0, 42.0'
    case 'NotEquals':
    case 'LessThan':
      return '41.0, 42.0'
    case 'LessOrEqual':
    case 'GreaterOrEqual':
      return '42.0, 42.0'
    case 'GreaterThan':
      return '43.0, 42.0'
    case 'IsNaN':
      return `${scalar.spelling}.fromBits(${scalar.spelling === 'f32' ? '2143289344' : '9221120237041090560'})`
    case 'IsInfinite':
      return `${scalar.spelling}.fromBits(${scalar.spelling === 'f32' ? '2139095040' : '9218868437227405312'})`
    case 'IsSubnormal':
      return `${scalar.spelling}.fromBits(1)`
    case 'IsSignNegative':
      return '-42.0'
    case 'TotalOrder':
      return `${scalar.spelling}.fromBits(${scalar.spelling === 'f32' ? '2147483648' : '9223372036854775808'}), 0.0`
    case 'Sqrt':
      return '1764.0'
    case 'Sin':
    case 'Cos':
      return '0.0'
    case 'FromBits':
      return scalar.spelling === 'f32' ? '1109917696' : '4631107791820423168'
    default:
      return operation.arity === 1 ? '42.0' : '40.0, 2.0'
  }
}

const floatMatrix = (() => {
  const cases = Scalar.floats().flatMap((scalar) =>
    scalar.operations.map((operation) => ({ scalar, operation })),
  )
  const functions = cases.map(({ scalar, operation }, ordinal) => {
    const invocation = `${scalar.spelling}.${operation.spelling}(${floatArguments(scalar, operation)})`
    if (operation.result === 'Boolean')
      return `fn floatCase${ordinal}() -> i32 { if ${invocation} { return 42 } return 0 }`
    if (operation.code === 'Sin' || operation.code === 'Cos') {
      let expected = '0'
      if (operation.code !== 'Sin') {
        expected = scalar.spelling === 'f32' ? '1065353216' : '4607182418800017408'
      }
      return `fn floatCase${ordinal}() -> i32 { if ${scalar.spelling}.toBits(${invocation}) == ${expected} { return 42 } return 0 }`
    }
    if (operation.code === 'ToBits')
      return `fn floatCase${ordinal}() -> i32 { return ${scalar.spelling}.toI32(${scalar.spelling}.fromBits(${invocation})) }`
    const target = Scalar.find(operation.result === 'Self' ? scalar.spelling : operation.result)
    return `fn floatCase${ordinal}() -> i32 { return ${target?.spelling ?? scalar.spelling}.toI32(${invocation}) }`
  })
  const imports = [...Scalar.floats(), ...Scalar.integers()]
    .map((scalar) => `import silk.${scalar.spelling} as ${scalar.spelling}`)
    .join('\n')
  return `${imports}
${functions.join('\n')}
fn verify(value: i32) -> () { if value != 42 { let boom = 1 / 0 } }
pub fn main() -> i32 {
${cases.map((_, ordinal) => `  let checked${ordinal} = verify(floatCase${ordinal}())`).join('\n')}
  return 42
}`
})()

it.effect(
  'lowers every catalogued float operation through all engines',
  () =>
    Effect.gen(function* () {
      const native = yield* Analysis.ofSourceRealized(
        'float/catalog-matrix',
        new TextEncoder().encode(floatMatrix),
        'aarch64-apple-darwin',
      )
      assert.deepEqual(Analysis.diagnostics(native), [])
      const outcome = Analysis.evaluate(native)
      assert.strictEqual(outcome._tag, 'Completed')
      if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
      assert.isAbove((yield* Analysis.codegen(native, { mode: 'release' })).bitcode.length, 0)

      const wasm = yield* Analysis.ofSourceRealized(
        'float/catalog-matrix-wasm',
        new TextEncoder().encode(floatMatrix),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(wasm), [])
      assert.isAbove((yield* Analysis.codegenWasm(wasm, { mode: 'release' })).bytes.length, 0)
    }),
  90_000,
)
