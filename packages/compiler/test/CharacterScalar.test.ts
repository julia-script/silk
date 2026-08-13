import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Layout from '../src/Layout.js'
import * as Scalar from '../src/Scalar.js'
import * as Target from '../src/Target.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyze = (name: string, text: string) =>
  Analysis.ofSourceRealized(`char/${name}`, ascii(text), 'wasm32-unknown-unknown')

const codes = (snapshot: Analysis.FrontendSnapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)

const messages = (snapshot: Analysis.FrontendSnapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.message)

it('gives char a fixed 32-bit width and a 4-byte layout on every target', () => {
  const found = Scalar.find('char')
  assert.strictEqual(found, Scalar.character)
  assert.strictEqual(found?.category, 'Character')
  assert.strictEqual(Scalar.bits(Scalar.character, 32), 32)
  assert.strictEqual(Scalar.bits(Scalar.character, 64), 32)
  assert.deepEqual(Scalar.resolveLayout(Scalar.character, 4, 4), { size: 4, alignment: 4 })
  assert.deepEqual(Scalar.resolveLayout(Scalar.character, 8, 8), { size: 4, alignment: 4 })
})

it('gives char a 32-bit unsigned representation on a 64-bit and a 32-bit target', () => {
  for (const target of [Target.aarch64AppleDarwin, Target.wasm32UnknownUnknown]) {
    const entry = Layout.make(target, ['char']).entries.at(0)
    assert.strictEqual(entry?._tag, 'LayoutEntry')
    if (entry?._tag !== 'LayoutEntry') return
    assert.strictEqual(entry.size, 4)
    assert.strictEqual(entry.alignment, 4)
    assert.deepEqual(entry.representation, { _tag: 'UnsignedInteger', bits: 32 })
  }
})

it.effect('accepts equality and ordering over two char operands', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'compare',
      'pub fn equal(left: char, right: char) -> bool { return left == right }\n' +
        'pub fn different(left: char, right: char) -> bool { return left != right }\n' +
        'pub fn below(left: char, right: char) -> bool { return left < right }\n' +
        'pub fn atMost(left: char, right: char) -> bool { return left <= right }\n' +
        'pub fn above(left: char, right: char) -> bool { return left > right }\n' +
        'pub fn atLeast(left: char, right: char) -> bool { return left >= right }\n' +
        'pub fn main() -> i32 { return 0 }',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect('accepts the named equality and ordering operations of the silk/char module', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'named',
      'import silk.char { equals, notEquals, lessThan, lessOrEqual, greaterThan, greaterOrEqual }\n' +
        'pub fn all(left: char, right: char) -> bool {\n' +
        '  if !equals(left, right) { return notEquals(left, right) }\n' +
        '  if lessThan(left, right) { return lessOrEqual(left, right) }\n' +
        '  if greaterThan(left, right) { return greaterOrEqual(left, right) }\n' +
        '  return true\n' +
        '}\n' +
        'pub fn main() -> i32 { return 0 }',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect('keeps char and u32 distinct in both directions', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'distinct',
      'pub fn takesChar(value: char) -> bool { return value == value }\n' +
        'pub fn takesU32(value: u32) -> bool { return value == value }\n' +
        'pub fn passesU32(value: u32) -> bool { return takesChar(value) }\n' +
        'pub fn passesChar(value: char) -> bool { return takesU32(value) }\n' +
        'pub fn main() -> i32 { return 0 }',
    )
    assert.deepEqual(codes(snapshot), ['SEM0012', 'SEM0012'])
    assert.deepEqual(messages(snapshot), [
      'Expected char but received u32',
      'Expected u32 but received char',
    ])
  }),
)

it.effect('rejects a u32 operand against a char operand without an explicit conversion', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'mixed-operands',
      'pub fn compare(left: char, right: u32) -> bool { return left == right }\n' +
        'pub fn main() -> i32 { return 0 }',
    )
    assert.deepEqual(codes(snapshot), ['SEM0012'])
    assert.include(messages(snapshot).at(0), 'Expected char but received u32')
  }),
)

/**
 * The operand-type diagnostic is the whole point of adding `char` to the operator fallback: an
 * arithmetic operator over an actor that declares no such operation used to reach a hard throw
 * while resolving `char.add`, because the compiler operator table has no entry for it.
 */
it.effect('reports an operand-type diagnostic for arithmetic over char operands', () =>
  Effect.gen(function* () {
    for (const [name, operator] of [
      ['add', '+'],
      ['subtract', '-'],
      ['multiply', '*'],
      ['divide', '/'],
      ['remainder', '%'],
      ['bitAnd', '&'],
      ['bitOr', '|'],
      ['bitXor', '^'],
    ] as const) {
      const snapshot = yield* analyze(
        `arithmetic-${name}`,
        `pub fn compute(left: char, right: char) -> char { return left ${operator} right }\n` +
          'pub fn main() -> i32 { return 0 }',
      )
      assert.include(codes(snapshot), 'SEM0012', `${name} produced no operand diagnostic`)
      assert.include(messages(snapshot), 'Expected i32 but received char', name)
    }
  }),
)

it.effect('reports an operand-type diagnostic for prefix operators over a char operand', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'prefix',
      'pub fn flip(value: char) -> char { return -value }\n' +
        'pub fn invert(value: char) -> char { return ~value }\n' +
        'pub fn main() -> i32 { return 0 }',
    )
    assert.deepEqual(codes(snapshot), ['SEM0012', 'SEM0012'])
    assert.deepEqual(messages(snapshot), [
      'Expected i32 but received char',
      'Expected i32 but received char',
    ])
  }),
)

it.effect('declares no arithmetic and no integer conversion on the char module', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'absent-members',
      'pub fn sum(left: char, right: char) -> char { return char.add(left, right) }\n' +
        'pub fn code(value: char) -> u32 { return char.toU32(value) }\n' +
        'pub fn main() -> i32 { return 0 }',
    )
    assert.deepEqual(codes(snapshot), ['SEM0014', 'SEM0014'])
    assert.deepEqual(messages(snapshot), [
      'Module silk/char has no member add',
      'Module silk/char has no member toU32',
    ])
    for (const absent of ['add', 'subtract', 'negate', 'bitAnd', 'toU32', 'checkedToU32'])
      assert.strictEqual(
        Scalar.character.operations.some((operation) => operation.spelling === absent),
        false,
        absent,
      )
  }),
)

it.effect('refuses to give an integer literal the char type', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze('literal', 'const letter: char = 97')
    assert.deepEqual(codes(snapshot), ['SEM0086'])
    assert.include(messages(snapshot).at(0), 'char')
  }),
)

/**
 * Lowering is reachability-driven, so a `char` operation reaches MIR only from a source that
 * builds a `char` value. The character literal is now the one way to build one, and it is what
 * makes the MIR admission, the evaluator value, and the per-engine compare lane testable at all;
 * `CharacterLiteral.test.ts` covers them end to end. This test keeps the reachability property
 * itself pinned: without a value, a `char` function is analyzed and never lowered.
 */
it.effect('lowers a char operation only from a source that builds a char value', () =>
  Effect.gen(function* () {
    const unreachable = yield* analyze(
      'unreachable',
      'pub fn below(left: char, right: char) -> bool { return left < right }\n' +
        'pub fn main() -> i32 { return 0 }',
    )
    assert.deepEqual(Analysis.diagnostics(unreachable), [])
    assert.deepEqual(
      Analysis.loweredMir(unreachable).functions.map((fn) => fn.id.name),
      ['main'],
    )
    const artifact = yield* Analysis.codegenWasm(unreachable, { mode: 'release' })
    assert.isTrue(WebAssembly.validate(artifact.bytes.slice()))

    const reachable = yield* analyze(
      'reachable',
      'pub fn below(left: char, right: char) -> bool { return left < right }\n' +
        "pub fn main() -> i32 { if below('a', 'b') { return 0 }\n  return 1 }",
    )
    assert.deepEqual(Analysis.diagnostics(reachable), [])
    assert.deepEqual(
      Analysis.loweredMir(reachable)
        .functions.map((fn) => fn.id.name)
        .sort(),
      ['below', 'main'],
    )
  }),
)
