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

it.effect('selects existing immediate integer contexts while retaining the char default', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'contextual',
      `
const accent: u8 = 'é'
struct Header { byte: u8 }
fn identity<T>(value: T) -> T { return move value }
fn byte(value: u8) -> u8 { return value }
fn defaults() -> char { let value = 'A'
return value }
fn signedByte() -> i8 { return '\\u{7f}' }
fn signedShort() -> i16 { return '\\u{7fff}' }
fn unsignedShort() -> u16 { return '\\u{ffff}' }
fn signedWord() -> i32 { return '😀' }
fn unsignedWord() -> u32 { return '\\u{10ffff}' }
fn signedWide() -> i64 { return 'A' }
fn unsignedWide() -> u64 { return 'A' }
fn signedPointer() -> isize { return 'A' }
fn unsignedPointer() -> usize { return 'A' }
fn contexts(value: u8) -> bool {
  let mut selected: u8 = 'A'
  selected = 'B'
  let header = Header { byte: 'C' }
  let array: [u8; 2] = ['D', 'é']
  let generic = identity<u8>('E')
  let piped = 'F' |> byte
  return value == 'A' && 'A' == value && byte('G') == 71
    && header.byte == 67 && array[1] == accent && generic == 69 && piped == 70
    && selected == 66 && 65 == 'A' && 'A' == 'A'
}
`,
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect('rejects existing char values and incompatible literal contexts', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'distinct',
      'pub fn takesU32(value: u32) -> u32 { return value }\n' +
        "pub fn passesChar() -> u32 { let value = 'a'\n return takesU32(value) }\n" +
        "const wrongFloat: f32 = 'a'\n" +
        "const wrongBool: bool = 'a'\n" +
        "const wrongString: string<'static> = 'a'\n" +
        "pub fn mixed() -> bool { return 'A' == 65 }\n" +
        "enum(u8) Letter { A = 65 }\nfn nominal() -> Letter { return 'A' }\n" +
        'pub fn main() -> i32 { return 0 }',
    )
    assert.deepEqual(codes(snapshot), [
      'SEM0012',
      'SEM0086',
      'SEM0086',
      'SEM0086',
      'SEM0012',
      'SEM0129',
    ])
  }),
)

it.effect('reports exact selected ranges at character literal spans', () =>
  Effect.gen(function* () {
    const source = `fn byte() -> u8 { return '☃' }
fn signed() -> i8 { return 'é' }
fn wide() -> u64 { return 18446744073709551616 }
const constant: u8 = '☃'`
    const snapshot = yield* analyze('range', source)
    const diagnostics = Analysis.diagnostics(snapshot)
    assert.deepEqual(codes(snapshot), ['SEM0002', 'SEM0002', 'SEM0002', 'SEM0086'])
    assert.deepEqual(
      diagnostics.slice(0, 3).map((diagnostic) => ({
        reason: diagnostic.reason,
        literal: new TextDecoder().decode(
          utf8(source).slice(diagnostic.span.start, diagnostic.span.end),
        ),
      })),
      [
        {
          reason: {
            _tag: 'IntegerOutOfRange',
            spelling: '9731',
            type: 'u8',
            minimum: '0',
            maximum: '255',
          },
          literal: "'☃'",
        },
        {
          reason: {
            _tag: 'IntegerOutOfRange',
            spelling: '233',
            type: 'i8',
            minimum: '-128',
            maximum: '127',
          },
          literal: "'é'",
        },
        {
          reason: {
            _tag: 'IntegerOutOfRange',
            spelling: '18446744073709551616',
            type: 'u64',
            minimum: '0',
            maximum: '18446744073709551615',
          },
          literal: '18446744073709551616',
        },
      ],
    )
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
        `pub fn letter() -> u32 { return ${spelling} }\npub fn main() -> i32 { return 0 }`,
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
        `const letter: u32 = ${spelling}\npub fn main() -> i32 { return 0 }`,
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
it.effect(
  'lowers default and static contextual character literals to their selected MIR types',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'char-literal/lowering',
        utf8(
          'pub fn below(left: char, right: char) -> bool { return left < right }\n' +
            "static fn accent() -> u8 { return 'é' }\n" +
            "pub fn main() -> i32 { if below('a', 'b') && accent() == 233 { return 0 }\n  return 1 }",
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
        ['Equals', 'LessThan'],
      )
      assert.deepEqual(MirVerification.verify(lowered), [])
      assert.deepEqual(
        operations.flatMap((operation) =>
          operation._tag === 'Literal' && operation.type._tag === 'u8'
            ? [Number(operation.value)]
            : [],
        ),
        [233, 233],
      )
    }),
)
