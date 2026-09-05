import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'

const utf8 = (value: string): Uint8Array => new TextEncoder().encode(value)

const analyze = (name: string, text: string) =>
  Analysis.ofSource(`char-literal/${name}`, utf8(text))

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
        "const alsoWrong: string<'static> = 'a'\n" +
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
      ['unterminated', "'\\n"],
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
    const snapshot = yield* Analysis.ofSourceRealized(
      'char-literal/lowering',
      utf8(
        'pub fn below(left: char, right: char) -> bool { return left < right }\n' +
          "pub fn main() -> i32 { if below('a', 'b') { return 0 }\n  return 1 }",
      ),
      'wasm32-unknown-unknown',
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
