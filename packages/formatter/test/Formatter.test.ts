import { assert, it } from '@effect/vitest'
import * as FormattedDocument from '@silk-effect/compiler/FormattedDocument'
import * as Lexer from '@silk-effect/compiler/Lexer'
import * as Parser from '@silk-effect/compiler/Parser'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import * as Formatter from '../src/Formatter.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

const parse = (id: string, source: string) =>
  Parser.parse(Lexer.lex(SourceFile.make(id, encoder.encode(source))))

const text = (document: FormattedDocument.FormattedDocument): string =>
  decoder.decode(FormattedDocument.toUint8Array(document))

it.effect('formats declaration and module examples through the canonical policy', () =>
  Effect.gen(function* () {
    const source = `//! Module example.
//! \`\`\`silk ignore
//! pub fn moduleExample()->i32{return unknown()}
//! \`\`\`
/// Declaration example.
/// \`\`\`silk
/// pub fn example()->i32{return 1}
/// \`\`\`
pub fn documented()->i32{return 1}
`
    const formatted = yield* Formatter.format(parse('memory://examples.silk', source))
    assert.strictEqual(
      text(formatted),
      `//! Module example.
//! \`\`\`silk ignore
//! pub fn moduleExample() -> i32 {
//!   return unknown()
//! }
//! \`\`\`
/// Declaration example.
/// \`\`\`silk
/// pub fn example() -> i32 {
///   return 1
/// }
/// \`\`\`
pub fn documented() -> i32 {
  return 1
}
`,
    )
    assert.isTrue(formatted.changed)
  }),
)

it.effect('leaves opaque fence languages alone while formatting active Silk', () =>
  Effect.gen(function* () {
    const source = `/// \`\`\`silk,ignore
/// fragment( )
/// \`\`\`
///
/// \`\`\`SILK
/// upper( )
/// \`\`\`
///
/// \`\`\`typescript
/// const  value=1
/// \`\`\`
///
/// \`\`\`
/// plain  text
/// \`\`\`
///
/// \`\`\`silk
/// pub fn active()->i32{return 1}
/// \`\`\`
pub fn documented() -> i32 {
  return 1
}
`
    const formatted = yield* Formatter.format(parse('memory://opaque.silk', source))
    const value = text(formatted)
    assert.match(value, /fragment\( \)/)
    assert.match(value, /upper\( \)/)
    assert.match(value, /const {2}value=1/)
    assert.match(value, /plain {2}text/)
    assert.match(value, /pub fn active\(\) -> i32 \{\n\/\/\/ {3}return 1/)
  }),
)

it.effect('accepts unchanged active bodies and terminal-space normalization on fence lines', () =>
  Effect.gen(function* () {
    const ordinary = `pub fn main() -> i32 {\n  return 1\n}\n`
    const ordinaryResult = yield* Formatter.format(parse('memory://ordinary.silk', ordinary))
    assert.isFalse(ordinaryResult.changed)
    assert.strictEqual(text(ordinaryResult), ordinary)

    const canonical = `/// \`\`\`silk
/// pub fn example() -> i32 {
///   return 1
/// }
/// \`\`\`
pub fn documented() -> i32 {
  return 1
}
`
    const unchanged = yield* Formatter.format(parse('memory://unchanged.silk', canonical))
    assert.isFalse(unchanged.changed)
    assert.strictEqual(text(unchanged), canonical)

    const spaced = canonical
      .replace('```silk\n', '```silk   \n')
      .replace('/// ```\n', '/// ```   \n')
    const normalized = yield* Formatter.format(parse('memory://spaced.silk', spaced))
    assert.strictEqual(text(normalized), canonical)
  }),
)

it.effect('preserves embedded multiline-literal trailing spaces and formatted blank lines', () =>
  Effect.gen(function* () {
    const trailing = '  '
    const source = `/// \`\`\`silk
/// pub fn main()->i32{
/// let value="""
/// line${trailing}
/// next
/// """
///
/// return 0
/// }
/// \`\`\`
pub fn documented() -> i32 { return 1 }
`
    const formatted = yield* Formatter.format(parse('memory://literal.silk', source))
    const value = text(formatted)
    assert.match(value, new RegExp(`/// line${trailing}\\n`))
    assert.match(value, /\/\/\/\n\/\/\/ {3}return 0/)
    const second = yield* Formatter.format(parse('memory://literal.silk', value))
    assert.strictEqual(text(second), value)
    assert.isFalse(second.changed)
  }),
)

it.effect('formats recursively nested documentation examples', () =>
  Effect.gen(function* () {
    const source = `/// \`\`\`silk
/// /// \`\`\`silk
/// /// pub fn nested()->i32{return 1}
/// /// \`\`\`
/// pub fn outer()->i32{return 1}
/// \`\`\`
pub fn documented() -> i32 { return 1 }
`
    const formatted = yield* Formatter.format(parse('memory://nested.silk', source))
    const value = text(formatted)
    assert.match(value, /\/\/\/ \/\/\/ pub fn nested\(\) -> i32 \{/)
    assert.match(value, /\/\/\/ \/\/\/ {3}return 1/)
    assert.match(value, /\/\/\/ pub fn outer\(\) -> i32 \{/)
    const second = yield* Formatter.format(parse('memory://nested.silk', value))
    assert.strictEqual(text(second), value)
  }),
)

it.effect(
  'locates embedded syntax damage in the original source despite preceding CRLF drift',
  () =>
    Effect.gen(function* () {
      const source = `pub fn before()->i32{return 1}\r
\r
/// Example.\r
/// \`\`\`silk\r
/// @@@\r
/// \`\`\`\r
pub fn documented()->i32{return 1}\r
`
      const syntax = parse('memory://damaged.silk', source)
      const attempted = yield* Effect.result(Formatter.format(syntax))
      assert.isTrue(Result.isFailure(attempted))
      if (Result.isSuccess(attempted)) return
      assert.strictEqual(attempted.failure.reason._tag, 'EmbeddedSyntax')
      if (attempted.failure.reason._tag !== 'EmbeddedSyntax') return
      const range = attempted.failure.reason.fence
      const physical = decoder.decode(
        Uint8Array.from(syntax.source.bytes.slice(range.start, range.end)),
      )
      assert.strictEqual(physical, `\`\`\`silk\r\n/// @@@\r\n/// \`\`\``)
      assert.deepEqual(attempted.failure.reason.nestedPath, [])
    }),
)

it.effect('rejects unclosed and later damaged active fences atomically', () =>
  Effect.gen(function* () {
    const unclosed = `/// \`\`\`silk
/// pub fn main() -> i32 { return 1 }
pub fn documented() -> i32 { return 1 }
`
    const malformed = yield* Effect.result(
      Formatter.format(parse('memory://unclosed.silk', unclosed)),
    )
    assert.isTrue(Result.isFailure(malformed))
    if (Result.isFailure(malformed))
      assert.strictEqual(malformed.failure.reason._tag, 'MalformedActiveFence')

    const mixed = `/// \`\`\`silk
/// pub fn first()->i32{return 1}
/// \`\`\`
///
/// \`\`\`silk
/// @@@
/// \`\`\`
pub fn documented() -> i32 { return 1 }
`
    const damaged = yield* Effect.result(Formatter.format(parse('memory://atomic.silk', mixed)))
    assert.isTrue(Result.isFailure(damaged))
    if (Result.isFailure(damaged)) assert.strictEqual(damaged.failure.reason._tag, 'EmbeddedSyntax')
  }),
)

it.effect('retains a nested relative fence path when recursively embedded Silk is damaged', () =>
  Effect.gen(function* () {
    const source = `/// \`\`\`silk
/// /// \`\`\`silk
/// /// @@@
/// /// \`\`\`
/// pub fn outer() -> i32 { return 1 }
/// \`\`\`
pub fn documented() -> i32 { return 1 }
`
    const attempted = yield* Effect.result(
      Formatter.format(parse('memory://nested-damage.silk', source)),
    )
    assert.isTrue(Result.isFailure(attempted))
    if (Result.isSuccess(attempted) || attempted.failure.reason._tag !== 'EmbeddedSyntax') return
    assert.strictEqual(attempted.failure.reason.fence.sourceId, 'memory://nested-damage.silk')
    assert.strictEqual(attempted.failure.reason.nestedPath.length, 1)
    assert.match(attempted.failure.reason.nestedPath[0]?.sourceId ?? '', /#documentation\/0\/0/)
  }),
)

it.effect('uses original-byte changed detection and remains idempotent', () =>
  Effect.gen(function* () {
    const source = `/// \`\`\`silk
/// pub fn value()->i32{return missing}
/// \`\`\`
pub fn documented()->i32{return 1}
`
    const first = yield* Formatter.format(parse('memory://idempotent.silk', source))
    assert.isTrue(first.changed)
    const canonical = text(first)
    const second = yield* Formatter.format(parse('memory://idempotent.silk', canonical))
    assert.isFalse(second.changed)
    assert.strictEqual(text(second), canonical)
  }),
)
