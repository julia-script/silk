import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Mir from '../src/Mir.js'
import * as Scalar from '../src/Scalar.js'

const source = `import silk.i16 as i16
import silk.u8 as u8
import silk.option { Option, Some, None }

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
    const snapshot = yield* Analysis.ofSourceRealized(
      'integer/checked',
      new TextEncoder().encode(source),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(
      outcome._tag,
      'Completed',
      JSON.stringify(outcome, (_, value) => (typeof value === 'bigint' ? `${value}n` : value), 2),
    )
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('lowers checked integer outcomes through LLVM and direct Wasm', () =>
  Effect.gen(function* () {
    for (const target of ['aarch64-apple-darwin', 'wasm32-unknown-unknown'] as const) {
      const snapshot = yield* Analysis.ofSourceRealized(
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

const characters = `import silk.u32 as u32
import silk.char { fromU32, toU32 }
import silk.option { Some, None }

fn value(input: u32) -> u32 {
  return match move fromU32(input) {
    Some<char> { value } => toU32(value)
    None {} => u32.toU32(0)
  }
}

pub fn main() -> i32 {
  let ascii = value(u32.toU32(65))
  let emoji = value(u32.toU32(128640))
  let surrogate = value(u32.toU32(55296))
  let above = value(u32.toU32(1114112))
  return u32.toI32(ascii + emoji + surrogate + above - u32.toU32(128663))
}`

it.effect('checks Unicode scalar construction and preserves total char-to-u32 conversion', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'scalar/character-conversion',
      new TextEncoder().encode(characters),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

    const native = yield* Analysis.ofSourceRealized(
      'scalar/character-conversion-native',
      new TextEncoder().encode(characters),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(native), [])
    assert.isAbove((yield* Analysis.codegen(native, { mode: 'debug' })).bitcode.length, 0)
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
    return `import silk.option { None }
import silk.option { Some }
fn integerCase${ordinal}() -> i32 {
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
  const imports = Scalar.integers()
    .map((scalar) => `import silk.${scalar.spelling} as ${scalar.spelling}`)
    .join('\n')
  return `${imports}
import silk.f32 as f32
import silk.f64 as f64
import silk.option { Some, None }
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
      const native = yield* Analysis.ofSourceRealized(
        'integer/matrix',
        new TextEncoder().encode(matrixSource),
        'aarch64-apple-darwin',
      )
      assert.deepEqual(Analysis.diagnostics(native), [])
      const outcome = Analysis.evaluate(native)
      assert.strictEqual(outcome._tag, 'Completed')
      if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
      const llvm = yield* Analysis.codegen(native, { mode: 'release' })
      assert.isAbove(llvm.bitcode.length, 0)

      const wasm = yield* Analysis.ofSourceRealized(
        'integer/matrix',
        new TextEncoder().encode(matrixSource),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(wasm), [])
      const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
      assert.isAbove(wasmArtifact.bytes.length, 0)
    }),
  120_000,
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
    const snapshot = yield* Analysis.ofSourceRealized(
      'integer/boundaries',
      new TextEncoder().encode(accepted),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')

    const removed = yield* Analysis.ofSourceRealized(
      'integer/removed-spelling',
      new TextEncoder().encode('pub fn main() -> I32 { return 42 }'),
    )
    assert.isTrue(Analysis.diagnostics(removed).some((diagnostic) => diagnostic.code === 'SEM0001'))

    const overflow = yield* Analysis.ofSourceRealized(
      'integer/wide-overflow',
      new TextEncoder().encode(
        'fn wide(value: u64) -> u64 { return value } pub fn main() -> i32 { let value = wide(18446744073709551616) return 42 }',
      ),
    )
    assert.isAbove(Analysis.diagnostics(overflow).length, 0)
  }),
)

const contextualCallSource = `import silk.u8 as u8
fn selectByte(
  source: &[u8],
  index: usize,
  first: u8,
  second: u8
) -> u8 {
  if source[index] == first { return second }
  return u8.add(0, 0)
}

fn identity<T>(value: T) -> T { return move value }

fn acceptByte(value: u8) -> u8 { return value }

fn isCarriageReturn(value: u8) -> bool { return value == 13 }

pub fn main() -> i32 {
  let direct = selectByte(b"//", 0, 47, 42)
  let explicit = identity<u8>(42)
  let piped = 42 |> acceptByte
  if !isCarriageReturn(13) { return 0 }
  return u8.toI32(direct) + u8.toI32(explicit) + u8.toI32(piped) - 84
}`

it.effect('uses concrete call and pipeline parameters as exact integer literal contexts', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'integer/contextual-calls',
      new TextEncoder().encode(contextualCallSource),
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const contextualValues = Analysis.expressionsOf(snapshot, 'integer/contextual-calls').flatMap(
      (expression) =>
        expression._tag === 'Integer' &&
        expression.integer._tag === 'Available' &&
        expression.integer.value === 42n
          ? [expression.integer.type]
          : [],
    )
    assert.isAtLeast(contextualValues.filter((type) => type === 'u8').length, 3)
    assert.include(Hir.encode(Analysis.rootAnalysis(snapshot).hir), 'literal 42 : u8')
    assert.include(Hir.encode(Analysis.rootAnalysis(snapshot).hir), 'literal 13 : u8')
    assert.include(Mir.encode(Analysis.loweredMir(snapshot)), 'literal 42 : u8')

    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('rejects contextual overflow and already-typed integer mismatches before MIR', () =>
  Effect.gen(function* () {
    const overflow = yield* Analysis.ofSourceRealized(
      'integer/contextual-overflow',
      new TextEncoder().encode(
        'fn accept(value: u8) -> u8 { return value } pub fn main() -> i32 { let value = accept(256) return 42 }',
      ),
    )
    assert.isAbove(Analysis.diagnostics(overflow).length, 0)
    assert.isTrue(
      Analysis.expressionsOf(overflow, 'integer/contextual-overflow').some(
        (expression) => expression._tag === 'Integer' && expression.integer._tag === 'OutOfRange',
      ),
    )
    assert.notInclude(Hir.encode(Analysis.rootAnalysis(overflow).hir), 'literal 256')

    const mismatch = yield* Analysis.ofSourceRealized(
      'integer/contextual-mismatch',
      new TextEncoder().encode(`import silk.i32 as i32
fn accept(value: u8) -> u8 { return value }
pub fn main() -> i32 {
  let wider = i32.add(40, 2)
  let value = accept(wider)
  return 42
}`),
    )
    assert.include(
      Analysis.diagnostics(mismatch).map((diagnostic) => diagnostic.code),
      'SEM0012',
    )
  }),
)
