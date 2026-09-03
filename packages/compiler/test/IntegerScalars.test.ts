import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as Scalar from '../src/Scalar.js'

const source = `import silk.i16 as i16
import silk.u8 as u8
import silk.option { Option }

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
    Option<u8>.None => 40
    Option<u8>.Some { value } => u8.toI32(value)
  }
  let converted = match move convert() {
    Option<u8>.None => 0
    Option<u8>.Some { value } => u8.toI32(value)
  }
  let sectioned = match move section() {
    Option<u8>.None => 0
    Option<u8>.Some { value } => u8.toI32(value)
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
      Json.stringify(outcome, (_, value) => (typeof value === 'bigint' ? `${value}n` : value), 2),
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

const customCheckedCarrier = `import silk.u8 as u8

union Checked<T> {
  Present { value: T },
  Absent
}

fn present<T>(value: T) -> Checked<T> {
  return Checked<T>.Present { value: move value }
}

fn absent<T>() -> Checked<T> {
  return Checked<T>.Absent
}

fn add(left: u8, right: u8) -> Checked<u8> {
  return Intrinsic.u8CheckedAdd<Checked<u8>>(left, right, present, absent)
}

fn value(self: Checked<u8>) -> i32 {
  return match move self {
    Checked<u8>.Present { value } => u8.toI32(value)
    Checked<u8>.Absent => 0
  }
}

pub fn main() -> i32 {
  return value(add(40, 2)) + value(add(255, 1))
}`

it.effect('lets checked scalar intrinsics choose a generic nominal carrier', () =>
  Effect.gen(function* () {
    const wasm = yield* Analysis.ofSourceRealized(
      'integer/custom-checked-carrier',
      new TextEncoder().encode(customCheckedCarrier),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(wasm), [])

    const evaluated = Analysis.evaluate(wasm)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      Json.stringify(evaluated, (_, value) => (typeof value === 'bigint' ? `${value}n` : value), 2),
    )
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)

    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

    const native = yield* Analysis.ofSourceRealized(
      'integer/custom-checked-carrier-native',
      new TextEncoder().encode(customCheckedCarrier),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(native), [])
    assert.isAbove((yield* Analysis.codegen(native, { mode: 'release' })).bitcode.length, 0)
  }),
)

const affineCheckedCarrier = `import silk.u8 as u8

union Checked<T> {
  Present { value: T },
  Absent
}

struct Token { marker: i32 }

fn present(value: u8, token: Token) -> Checked<u8> {
  let observed = token.marker
  return Checked<u8>.Present { value: value }
}

fn absent() -> Checked<u8> {
  return Checked<u8>.Absent
}

fn presentWith(token: Token) -> some<F: once fn(u8) -> Checked<u8>> F {
  return present(move token)
}

fn value(self: Checked<u8>) -> i32 {
  return match move self {
    Checked<u8>.Present { value } => u8.toI32(value)
    Checked<u8>.Absent => 0
  }
}

pub fn main() -> i32 {
  let firstPresent = Token { marker: 1 }
  let secondPresent = Token { marker: 2 }
  let succeeded = Intrinsic.u8CheckedAdd<Checked<u8>>(
    u8.toU8(40),
    u8.toU8(2),
    presentWith(move firstPresent),
    absent
  )
  let failed = Intrinsic.u8CheckedAdd<Checked<u8>>(
    u8.toU8(255),
    u8.toU8(1),
    presentWith(move secondPresent),
    absent
  )
  return value(move succeeded) + value(move failed)
}`

it.effect('cleans the unused affine carrier and invokes the selected carrier exactly once', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'integer/affine-checked-carrier',
      new TextEncoder().encode(affineCheckedCarrier),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      Json.stringify(evaluated, (_, value) => (typeof value === 'bigint' ? `${value}n` : value), 2),
    )
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
    assert.strictEqual(evaluated.trace.filter((event) => event._tag === 'CallableApply').length, 2)
    assert.strictEqual(
      evaluated.trace.filter((event) => event._tag === 'CallableCleanup').length,
      1,
    )
  }),
)

const checkedScalarNominalUnionCleanup = `import silk.i8 as i8
import silk.u8 as u8

union Checked<T> {
  Present { value: T },
  Absent
}

struct Guard {
  left: i8
  right: i8
}

impl Drop for Guard {
  fn drop(self: &mut Guard) -> () {
    let observed = i8.toI32(self.left) + i8.toI32(self.right)
    if observed != 42 {
      let boom = 1 / 0
    }
    return ()
  }
}

union Choice {
  Small { marker: i8, guard: Guard },
  Wide { value: i64 }
}

fn present(value: u8, choice: Choice) -> Checked<u8> {
  drop choice
  return Checked<u8>.Present { value: value }
}

fn absent() -> Checked<u8> {
  return Checked<u8>.Absent
}

fn presentWith(choice: Choice) -> some<F: once fn(u8) -> Checked<u8>> F {
  return present(move choice)
}

fn value(self: Checked<u8>) -> i32 {
  return match move self {
    Checked<u8>.Present { value } => u8.toI32(value)
    Checked<u8>.Absent => 42
  }
}

pub fn main() -> i32 {
  let choice = Choice.Small {
    marker: i8.toI8(7),
    guard: Guard { left: i8.toI8(19), right: i8.toI8(23) }
  }
  let failed = Intrinsic.u8CheckedAdd<Checked<u8>>(
    u8.toU8(255),
    u8.toU8(1),
    presentWith(move choice),
    absent
  )
  return value(move failed)
}`

it.effect('reserves roots and nominal-union scratch for unused checked-scalar callbacks', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'integer/checked-scalar-nominal-union-cleanup',
      new TextEncoder().encode(checkedScalarNominalUnionCleanup),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)

    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

const characters = `import silk.u32 as u32
import silk.char { fromU32, toU32 }
import silk.option { Option }

fn value(input: u32) -> u32 {
  return match move fromU32(input) {
    Option<char>.Some { value } => toU32(value)
    Option<char>.None => u32.toU32(0)
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

const rotation = `import silk.u8 as u8
import silk.u64 as u64
pub fn main() -> i32 {
  let wrapped = u8.rotateLeft(42, 8)
  let shifted = u8.rotateRight(u8.rotateLeft(42, 9), 65)
  let wide = u64.rotateLeft(42, 64)
  return u8.toI32(wrapped) + u8.toI32(shifted) + u64.toI32(wide) - 84
}`

it.effect('wraps rotate counts modulo the width in the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'integer/rotate-wrap',
      new TextEncoder().encode(rotation),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
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
  let invocation: string
  if (operation.code === 'BitNot') {
    invocation = `${scalar.spelling}.bitNot(${scalar.spelling}.bitNot(42))`
  } else if (
    operation.code === 'Negate' ||
    operation.code === 'WrappingNegate' ||
    operation.code === 'SaturatingNegate'
  ) {
    invocation = `${scalar.spelling}.${operation.spelling}(-42)`
  } else {
    invocation = `${scalar.spelling}.${operation.spelling}(${integerArguments(operation)})`
  }
  if (operation.result === 'Boolean')
    return `fn integerCase${ordinal}() -> i32 { if ${invocation} { return 42 } return 0 }`
  if (operation.result === 'OptionSelf' || operation.result === 'OptionTarget')
    return `import silk.option { Option }
fn integerCase${ordinal}() -> i32 {
  return match move ${invocation} {
    Option<${target.spelling}>.None => 0
    Option<${target.spelling}>.Some { value } => ${target.spelling === 'i32' ? 'value' : `${target.spelling}.toI32(value)`}
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
import silk.option { Option }
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

const durationSource = `pub const timeout: u64 = 01h05m00s

fn accept(value: u64) -> u64 { return value }

pub fn main() -> i32 {
  let nano = 1ns
  let micro = 1us
  let milli = 1ms
  let second = 1s
  let minute = 1m
  let hour = 1h
  let day = 1d
  let week = 1w
  let compound = 1w2d3h4m5s6ms7us8ns
  let aligned = 01h05m00s
  let additive = 1h + 30m + 30s
  let accepted = accept(300ms)
  if nano != 1 { return 1 }
  if micro != 1000 { return 2 }
  if milli != 1000000 { return 3 }
  if second != 1000000000 { return 4 }
  if minute != 60000000000 { return 5 }
  if hour != 3600000000000 { return 6 }
  if day != 86400000000000 { return 7 }
  if week != 604800000000000 { return 8 }
  if compound != 788645006007008 { return 9 }
  if aligned != 3900000000000 { return 10 }
  if additive != 5430000000000 { return 11 }
  if accepted != 300000000 { return 12 }
  if timeout != 3900000000000 { return 13 }
  return 42
}`

it.effect(
  'treats every duration form as one exact fixed-u64 value and reuses scalar lowering',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'integer/durations',
        new TextEncoder().encode(durationSource),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const durations = Analysis.expressionsOf(snapshot, 'integer/durations').filter(
        (expression) => expression._tag === 'Duration',
      )
      assert.isAtLeast(durations.length, 12)
      assert.isTrue(
        durations.every(
          (duration) => duration.type._tag === 'Available' && duration.type.type === 'u64',
        ),
      )

      const hir = Hir.encode(Analysis.rootAnalysis(snapshot).hir)
      const mir = MirEncoding.encode(Analysis.loweredMir(snapshot))
      assert.include(hir, 'literal 1000000000 : u64')
      assert.include(mir, 'literal 1000000000 : u64')
      assert.notInclude(hir, 'Duration')
      assert.notInclude(mir, 'Duration')
      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'debug' })
      assert.isAbove(wasm.bytes.length, 0)

      const outcome = Analysis.evaluate(snapshot)
      assert.strictEqual(outcome._tag, 'Completed')
      if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    }),
)

it.effect(
  'rejects duration range and type mismatches before HIR without duplicate recovery errors',
  () =>
    Effect.gen(function* () {
      const overflow = yield* Analysis.ofSourceRealized(
        'integer/duration-overflow',
        new TextEncoder().encode('pub fn main() -> u64 { return 18446744073709551616ns }'),
      )
      assert.deepEqual(
        Analysis.diagnostics(overflow).map((diagnostic) => diagnostic.code),
        ['SEM0204', 'SEM0170'],
      )
      assert.notInclude(Hir.encode(Analysis.rootAnalysis(overflow).hir), '18446744073709551616')

      const constantOverflow = yield* Analysis.ofSourceRealized(
        'integer/duration-constant-overflow',
        new TextEncoder().encode(
          'pub const timeout: u64 = 18446744073709551616ns pub fn main() -> i32 { return 42 }',
        ),
      )
      assert.deepEqual(
        Analysis.diagnostics(constantOverflow).map((diagnostic) => diagnostic.code),
        ['SEM0170'],
      )

      const mismatch = yield* Analysis.ofSourceRealized(
        'integer/duration-mismatch',
        new TextEncoder().encode(
          'fn accept(value: i32) -> i32 { return value } pub fn main() -> i32 { return accept(1s) }',
        ),
      )
      assert.deepEqual(
        Analysis.diagnostics(mismatch).map((diagnostic) => diagnostic.code),
        ['SEM0012'],
      )
      assert.isTrue(
        Analysis.expressionsOf(mismatch, 'integer/duration-mismatch').some(
          (expression) =>
            expression._tag === 'Duration' &&
            expression.type._tag === 'Available' &&
            expression.type.type === 'u64',
        ),
      )

      const malformed = yield* Analysis.ofSourceRealized(
        'integer/duration-malformed',
        new TextEncoder().encode('pub fn main() -> u64 { return 1h60m }'),
      )
      assert.deepEqual(
        Analysis.diagnostics(malformed).map((diagnostic) => diagnostic.code),
        ['SEM0204', 'LEX0012'],
      )
    }),
)

it.effect('uses ordinary checked u64 arithmetic for duration expressions', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'integer/duration-arithmetic-overflow',
      new TextEncoder().encode(
        'pub fn main() -> i32 { let overflow = 18446744073709551615ns + 1ns return 42 }',
      ),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(Analysis.evaluate(snapshot)._tag, 'Trap')
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
    assert.include(MirEncoding.encode(Analysis.loweredMir(snapshot)), 'literal 42 : u8')

    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('lets a declared scalar operand drive literal-first infix arithmetic', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'integer/literal-first-infix',
      new TextEncoder().encode(`import silk.u16 as u16
fn mixed(value: u16) -> i32 {
  let literalFirst = 5 + value
  let literalLast = value + 5
  let defaulted = 5 + 5
  return u16.toI32(literalFirst) + u16.toI32(literalLast) - defaulted
}
pub fn main() -> i32 { return mixed(21) }`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
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
