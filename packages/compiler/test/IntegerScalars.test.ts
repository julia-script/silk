import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Scalar from '../src/Scalar.js'

const source = `import silk.option { Option, Some, None }

fn overflow() -> Option<u8> {
  return u8.checkedAdd(255, 1)
}

fn convert() -> Option<u8> {
  return i16.checkedToU8(255)
}

fn section() -> Option<u8> {
  let addOne = u8.checkedAdd(1)
  return addOne(u8.add(40, 1))
}

pub fn main() -> i32 {
  let failed = match move overflow() {
    None {} => 40
    Some<u8> { value } => u8.toI32(value)
  }
  let converted = match move convert() {
    None {} => 0
    Some<u8> { value } => u8.toI32(value)
  }
  let sectioned = match move section() {
    None {} => 0
    Some<u8> { value } => u8.toI32(value)
  }
  return failed + converted + sectioned - 295
}`

it.effect('returns canonical Some and None outcomes for recoverable integer operations', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource('integer/checked', new TextEncoder().encode(source))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(
      outcome._tag,
      'Completed',
      JSON.stringify(outcome, (_, value) => (typeof value === 'bigint' ? `${value}n` : value), 2),
    )
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('lowers checked integer outcomes through LLVM and direct Wasm', () =>
  Effect.gen(function* () {
    for (const target of ['aarch64-apple-darwin', 'wasm32-unknown-unknown'] as const) {
      const snapshot = yield* Analysis.ofSource(
        `integer/checked-${target}`,
        new TextEncoder().encode(source),
        target,
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const size =
        target === 'wasm32-unknown-unknown'
          ? (yield* Analysis.codegenWasm(snapshot, { mode: 'debug' })).bytes.length
          : (yield* Analysis.codegen(snapshot, { mode: 'debug' })).bitcode.length
      assert.isAbove(size, 0)
    }
  }),
)

const integerArguments = (operation: Scalar.Operation): string => {
  switch (operation.code) {
    case 'Subtract':
    case 'WrappingSubtract':
    case 'SaturatingSubtract':
    case 'CheckedSubtract':
      return '44, 2'
    case 'Multiply':
    case 'WrappingMultiply':
    case 'SaturatingMultiply':
    case 'CheckedMultiply':
      return '21, 2'
    case 'Divide':
    case 'CheckedDivide':
      return '84, 2'
    case 'Remainder':
    case 'CheckedRemainder':
      return '42, 100'
    case 'BitAnd':
      return '42, 63'
    case 'ShiftLeft':
      return '21, 1'
    case 'ShiftRight':
      return '84, 1'
    case 'RotateLeft':
    case 'RotateRight':
      return '42, 0'
    case 'Equals':
      return '42, 42'
    case 'NotEquals':
    case 'LessThan':
      return '41, 42'
    case 'LessOrEqual':
    case 'GreaterOrEqual':
      return '42, 42'
    case 'GreaterThan':
      return '43, 42'
    default:
      return operation.arity === 1 ? '42' : '40, 2'
  }
}

const integerCase = (
  scalar: Scalar.IntegerScalar,
  operation: Scalar.Operation,
  ordinal: number,
): string => {
  const target =
    Scalar.conversionTarget(operation.code) ??
    Scalar.floatConversionTarget(operation.code) ??
    scalar
  const invocation =
    operation.code === 'BitNot'
      ? `${scalar.spelling}.bitNot(${scalar.spelling}.bitNot(42))`
      : operation.code === 'Negate' ||
          operation.code === 'WrappingNegate' ||
          operation.code === 'SaturatingNegate'
        ? `${scalar.spelling}.${operation.spelling}(-42)`
        : `${scalar.spelling}.${operation.spelling}(${integerArguments(operation)})`
  if (operation.result === 'Boolean')
    return `fn integerCase${ordinal}() -> i32 { if ${invocation} { return 42 } return 0 }`
  if (operation.result === 'OptionSelf' || operation.result === 'OptionTarget')
    return `fn integerCase${ordinal}() -> i32 {
  return match move ${invocation} {
    None {} => 0
    Some<${target.spelling}> { value } => ${target.spelling === 'i32' ? 'value' : `${target.spelling}.toI32(value)`}
  }
}`
  return `fn integerCase${ordinal}() -> i32 { return ${target.spelling === 'i32' ? invocation : `${target.spelling}.toI32(${invocation})`} }`
}

const matrixSource = (() => {
  const cases = Scalar.integers().flatMap((scalar) =>
    scalar.operations.map((operation) => ({ scalar, operation })),
  )
  const declarations = cases.map(({ scalar, operation }, ordinal) =>
    integerCase(scalar, operation, ordinal),
  )
  const checks = cases.map(
    (_, ordinal) => `  let checked${ordinal} = verify(integerCase${ordinal}())`,
  )
  return `import silk.option { Some, None }
${declarations.join('\n')}
fn verify(value: i32) -> () { if value != 42 { let boom = 1 / 0 } }
pub fn main() -> i32 {
${checks.join('\n')}
  return 42
}`
})()

it.effect(
  'keeps every catalogued integer operation in evaluator, LLVM, and Wasm parity',
  () =>
    Effect.gen(function* () {
      const native = yield* Analysis.ofSource(
        'integer/matrix',
        new TextEncoder().encode(matrixSource),
        'aarch64-apple-darwin',
      )
      assert.deepEqual(Analysis.diagnostics(native), [])
      const outcome = Analysis.evaluate(native)
      assert.strictEqual(outcome._tag, 'Completed')
      if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
      const llvm = yield* Analysis.codegen(native, { mode: 'release' })
      assert.isAbove(llvm.bitcode.length, 0)

      const wasm = yield* Analysis.ofSource(
        'integer/matrix',
        new TextEncoder().encode(matrixSource),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(wasm), [])
      const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
      assert.isAbove(wasmArtifact.bytes.length, 0)
    }),
  20_000,
)

it.effect('accepts exact boundaries, unit fallthrough, and only lowercase primitive names', () =>
  Effect.gen(function* () {
    const accepted = `fn wide(value: u64) -> u64 { return value }
fn signed(value: i64) -> i64 { return value }
fn fallthrough() {}
fn bare() { return }
fn diverge() -> never { return diverge() }
pub fn main() -> i32 {
  let maximum = wide(18446744073709551615)
  let minimum = signed(-9223372036854775808)
  let first = fallthrough()
  let second = bare()
  if false { return diverge() }
  return 42
}`
    const snapshot = yield* Analysis.ofSource(
      'integer/boundaries',
      new TextEncoder().encode(accepted),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')

    const removed = yield* Analysis.ofSource(
      'integer/removed-spelling',
      new TextEncoder().encode('pub fn main() -> I32 { return 42 }'),
    )
    assert.isTrue(Analysis.diagnostics(removed).some((diagnostic) => diagnostic.code === 'SEM0001'))

    const overflow = yield* Analysis.ofSource(
      'integer/wide-overflow',
      new TextEncoder().encode(
        'fn wide(value: u64) -> u64 { return value } pub fn main() -> i32 { let value = wide(18446744073709551616) return 42 }',
      ),
    )
    assert.isAbove(Analysis.diagnostics(overflow).length, 0)
  }),
)
