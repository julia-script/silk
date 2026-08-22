import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'

const utf8 = (value: string): Uint8Array => new TextEncoder().encode(value)

const analyze = (name: string, text: string) =>
  Analysis.ofSourceRealized(`char-literal/${name}`, utf8(text), 'wasm32-unknown-unknown')

const codes = (snapshot: Analysis.FrontendSnapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)

const messages = (snapshot: Analysis.FrontendSnapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.message)

it.effect('gives a character literal the char type', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'typed',
      "const asciiSpace: char = ' '\n" +
        "const asciiTab: char = '\\t'\n" +
        "const newline: char = '\\n'\n" +
        "const snowman: char = '\\u{2603}'\n" +
        'pub fn isSpace(value: char) -> bool {\n' +
        '  return value == asciiSpace\n' +
        '}\n' +
        "pub fn isTab(value: char) -> bool { return value == '\\t' }\n" +
        'pub fn main() -> i32 { return 0 }',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

/**
 * The literal has one type and never adapts to its context, which is the same rule the `char`
 * scalar states in the other direction: no integer literal becomes a `char` and no character
 * literal becomes an integer.
 */
it.effect('keeps a character literal distinct from every integer and from a string', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'distinct',
      'pub fn takesU32(value: u32) -> u32 { return value }\n' +
        "pub fn passesChar() -> u32 { return takesU32('a') }\n" +
        "const wrong: u32 = 'a'\n" +
        "const alsoWrong: string = 'a'\n" +
        'pub fn main() -> i32 { return 0 }',
    )
    assert.deepEqual(codes(snapshot), ['SEM0012', 'SEM0086', 'SEM0086'])
    assert.include(messages(snapshot).at(0), 'Expected u32 but received char')
  }),
)

it.effect('reports one semantic diagnostic for an escape the shared decoder rejects', () =>
  Effect.gen(function* () {
    for (const [name, spelling, detail] of [
      ['unknown-escape', "'\\q'", 'unknown escape sequence'],
      ['short-hex', "'\\xZ'", '`\\x` escape requires exactly two hexadecimal digits'],
      ['surrogate', "'\\u{d800}'", 'invalid Unicode scalar escape'],
      ['above-range', "'\\u{110000}'", 'invalid Unicode scalar escape'],
      ['non-utf8', "'\\xff'", 'character literal is not valid UTF-8'],
    ] as const) {
      const snapshot = yield* analyze(
        `escape-${name}`,
        `pub fn letter() -> char { return ${spelling} }\npub fn main() -> i32 { return 0 }`,
      )
      assert.deepEqual(codes(snapshot), ['SEM0085'], name)
      assert.include(messages(snapshot).at(0), detail, name)
    }
  }),
)

/** The scalar-count rule is lexical, so the semantic phase adds nothing on top of it. */
it.effect('reports exactly one lexical diagnostic for a body that is not one scalar', () =>
  Effect.gen(function* () {
    for (const [name, spelling] of [
      ['empty', "''"],
      ['two', "'ab'"],
      ['unterminated', "'a"],
    ] as const) {
      const snapshot = yield* analyze(
        `count-${name}`,
        `const letter: char = ${spelling}\npub fn main() -> i32 { return 0 }`,
      )
      assert.deepEqual(
        codes(snapshot).filter((code) => code.startsWith('LEX')),
        [name === 'unterminated' ? 'LEX0003' : 'LEX0007'],
        name,
      )
    }
  }),
)

/**
 * A `char` value is now constructible, so `char` operations reach MIR. The literal is a general
 * `Literal` operation over the `char` type rather than a lane-shaped or backend-specific one.
 */
it.effect('lowers a character literal to one general MIR literal over char', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'lowering',
      'pub fn below(left: char, right: char) -> bool { return left < right }\n' +
        "pub fn main() -> i32 { if below('a', 'b') { return 0 }\n  return 1 }",
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const lowered = Analysis.loweredMir(snapshot)
    assert.deepEqual(lowered.functions.map((fn) => fn.id.name).sort(), ['below', 'main'])
    const operations = lowered.functions.flatMap((fn) => MirVerification.operations(fn))
    assert.deepEqual(
      operations.flatMap((operation) =>
        operation._tag === 'Literal' && operation.type._tag === 'char'
          ? [Number(operation.value)]
          : [],
      ),
      [0x61, 0x62],
    )
    assert.deepEqual(
      operations.flatMap((operation) =>
        operation._tag === 'Binary' && operation.type._tag === 'bool' ? [operation.operator] : [],
      ),
      ['LessThan'],
    )
    assert.deepEqual(MirVerification.verify(lowered), [])
  }),
)

/**
 * Every accepted escape, a multi-byte scalar, the six comparisons, and a `char` constant, run on
 * the evaluator and the Wasm backend; the native backend runs the same program through the corpus
 * entry `character-literal-acceptance` in `DriverNativeAcceptance.test.ts`. The comparisons take
 * parameters rather than literals so no engine can fold the operator away and still report the
 * right answer.
 */
const acceptance = `import silk.char { equals, notEquals, lessThan, lessOrEqual, greaterThan, greaterOrEqual }

const asciiSpace: char = ' '
const asciiTab: char = '\\t'
const snowman: char = '\\u{2603}'

fn eq(left: char, right: char) -> bool { return left == right }
fn ne(left: char, right: char) -> bool { return left != right }
fn lt(left: char, right: char) -> bool { return left < right }
fn le(left: char, right: char) -> bool { return left <= right }
fn gt(left: char, right: char) -> bool { return left > right }
fn ge(left: char, right: char) -> bool { return left >= right }

pub fn main() -> i32 {
  if eq('a', 'a') {} else { return 1 }
  if ne('a', 'b') {} else { return 2 }
  if lt('a', 'b') {} else { return 3 }
  if le('a', 'a') {} else { return 4 }
  if gt('b', 'a') {} else { return 5 }
  if ge('a', 'a') {} else { return 6 }
  if eq('\\n', '\\u{a}') {} else { return 7 }
  if eq('\\r', '\\u{d}') {} else { return 8 }
  if eq('\\t', asciiTab) {} else { return 9 }
  if eq('\\0', '\\u{0}') {} else { return 10 }
  if eq('\\\\', '\\u{5c}') {} else { return 11 }
  if eq('\\'', '\\u{27}') {} else { return 12 }
  if eq('\\"', '"') {} else { return 13 }
  if eq('\\x41', 'A') {} else { return 14 }
  if eq(' ', asciiSpace) {} else { return 15 }
  if eq('é', '\\u{e9}') {} else { return 16 }
  if eq('☃', snowman) {} else { return 17 }
  if lt('é', snowman) {} else { return 18 }
  if gt('😀', snowman) {} else { return 19 }
  if equals('a', 'a') {} else { return 20 }
  if notEquals('a', 'b') {} else { return 21 }
  if lessThan('a', 'b') {} else { return 22 }
  if lessOrEqual('a', 'b') {} else { return 23 }
  if greaterThan('b', 'a') {} else { return 24 }
  if greaterOrEqual('b', 'a') {} else { return 25 }
  return 42
}`

it.effect(
  'runs the same character-literal program identically on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* analyze('acceptance', acceptance)
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
      assert.strictEqual(evaluated.result.value, 42n)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    }),
  120_000,
)

/**
 * The ordering is by Unicode scalar value over the whole range, so it must stay unsigned in every
 * engine: the high scalars sit above the 31-bit boundary of no lane, but the lane is a 32-bit one
 * and a signed comparison would be a silent, engine-specific difference.
 */
it.effect(
  'orders the whole scalar range the same way on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const source = `fn below(left: char, right: char) -> bool { return left < right }

pub fn main() -> i32 {
  if below('\\u{0}', '\\u{10ffff}') {} else { return 1 }
  if below('\\u{10ffff}', '\\u{0}') == false {} else { return 2 }
  if below('\\u{d7ff}', '\\u{e000}') {} else { return 3 }
  if below('\\u{ffff}', '\\u{10000}') {} else { return 4 }
  return 42
}`
      const snapshot = yield* analyze('ordering', source)
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    }),
  120_000,
)
