import { assert, it } from '@effect/vitest'
import * as DocBlock from '@silklang/compiler/DocBlock'
import * as Lexer from '@silklang/compiler/Lexer'
import * as Parser from '@silklang/compiler/Parser'
import * as SourceFile from '@silklang/compiler/SourceFile'
import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import { unreachable } from '../../../test/support/raise.js'
import * as CodeFence from '../src/CodeFence.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

const parse = (id: string, source: string) => {
  const file = SourceFile.make(id, encoder.encode(source))
  return Parser.parse(Lexer.lex(file))
}

const firstBlock = (id: string, source: string) => {
  const syntax = parse(id, source)
  return {
    syntax,
    block: DocBlock.all(syntax).at(0) ?? unreachable('expected documentation block'),
  }
}

it.effect('classifies exact CommonMark language words and records authored closers', () =>
  Effect.gen(function* () {
    const source = `/// \`\`\`silk
/// fn one() -> i32 { return 1 }
/// \`\`\`
///
/// \`\`\`silk ignore
/// fn two() -> i32 { return 2 }
/// \`\`\`
///
/// \`\`\`silk,ignore
/// fragment()
/// \`\`\`
///
/// \`\`\`SILK
/// upper()
/// \`\`\`
///
/// \`\`\`typescript
/// const value = 1
/// \`\`\`
///
/// \`\`\`
/// plain
/// \`\`\`
pub fn documented() -> i32 { return 1 }
`
    const { syntax, block } = firstBlock('memory://classify.silk', source)
    const fences = yield* CodeFence.all(syntax.source, block)
    assert.deepEqual(
      fences.map((fence) => [fence.language, fence.metadata, CodeFence.isActive(fence)]),
      [
        ['silk', undefined, true],
        ['silk', 'ignore', true],
        ['silk,ignore', undefined, false],
        ['SILK', undefined, false],
        ['typescript', undefined, false],
        [undefined, undefined, false],
      ],
    )
    assert.deepEqual(
      fences.map((fence) => fence.rawInfoString),
      ['silk', 'silk ignore', 'silk,ignore', 'SILK', 'typescript', ''],
    )
    assert.isTrue(fences.every((fence) => fence.hasAuthoredCloser))
  }),
)

it.effect('detects an unclosed active fence without making documentation parsing partial', () =>
  Effect.gen(function* () {
    const source = `/// \`\`\`silk
/// fn open() -> i32 { return 1 }
pub fn documented() -> i32 { return 1 }
`
    const { syntax, block } = firstBlock('memory://unclosed.silk', source)
    const fences = yield* CodeFence.all(syntax.source, block)
    assert.strictEqual(fences.length, 1)
    assert.isTrue(CodeFence.isActive(fences[0] ?? unreachable('expected fence')))
    assert.isFalse(fences[0]?.hasAuthoredCloser)
  }),
)

it.effect('ignores indented CommonMark code while inventorying actual fences', () =>
  Effect.gen(function* () {
    const source = `/// Example output:
///
///     not a fenced block
///
/// \`\`\`typescript
/// const value = 1
/// \`\`\`
pub fn documented() -> i32 { return 1 }
`
    const { syntax, block } = firstBlock('memory://indented.silk', source)
    const fences = yield* CodeFence.all(syntax.source, block)
    assert.strictEqual(fences.length, 1)
    assert.strictEqual(fences[0]?.language, 'typescript')
  }),
)

it.effect('rewrites selected raw blocks while preserving markers, fences, Unicode, and CRLF', () =>
  Effect.gen(function* () {
    const source = `/// Prose α.\r
/// \`\`\`silk\r
/// fn old()->i32{return 1}\r
/// \`\`\`\r
/// Between.\r
/// ~~~~silk\r
/// fn second()->i32{return 2}\r
/// ~~~~\r
/// After.\r
pub fn documented() -> i32 { return 1 }\r
`
    const { syntax, block } = firstBlock('memory://rewrite.silk', source)
    const fences = yield* CodeFence.all(syntax.source, block)
    const first = fences.at(0) ?? unreachable('expected first fence')
    const second = fences.at(1) ?? unreachable('expected second fence')
    const rewritten = yield* CodeFence.rewrite(syntax.source, block, [
      { fence: first, body: 'fn first() -> i32 {\n  return 1\n}' },
      { fence: second, body: 'fn second() -> i32 {\n  return 2\n}' },
    ])
    assert.strictEqual(
      decoder.decode(rewritten),
      `/// Prose α.\r
/// \`\`\`silk\r
/// fn first() -> i32 {\r
///   return 1\r
/// }\r
/// \`\`\`\r
/// Between.\r
/// ~~~~silk\r
/// fn second() -> i32 {\r
///   return 2\r
/// }\r
/// ~~~~\r
/// After.`,
    )
    assert.strictEqual(first.delimiter, '`')
    assert.strictEqual(second.delimiter, '~')
    assert.strictEqual(second.delimiterLength, 4)
  }),
)

it.effect('synthesizes container continuation and comment-only blank body lines', () =>
  Effect.gen(function* () {
    const source = `/// > - \`\`\`silk
/// >   fn old()->i32{return 1}
/// >   \`\`\`
pub fn documented() -> i32 { return 1 }
`
    const { syntax, block } = firstBlock('memory://container.silk', source)
    const fence =
      (yield* CodeFence.all(syntax.source, block)).at(0) ?? unreachable('expected fence')
    assert.deepEqual(fence.containers, ['BlockQuote', 'UnorderedList', 'ListItem'])
    const rewritten = yield* CodeFence.rewrite(syntax.source, block, [
      {
        fence,
        body: 'fn main() -> i32 {\n\n  let text = """line with spaces  \nnext"""\n  return 1\n}',
      },
    ])
    const raw = decoder.decode(rewritten)
    assert.strictEqual(
      raw,
      `/// > - \`\`\`silk
/// >   fn main() -> i32 {
/// > ${'  '}
/// >     let text = """line with spaces${'  '}
/// >   next"""
/// >     return 1
/// >   }
/// >   \`\`\``,
    )
    const reparsedSource = `${raw}\npub fn documented() -> i32 { return 1 }\n`
    const reparsed = firstBlock('memory://container-reparsed.silk', reparsedSource)
    const reparsedFence = (yield* CodeFence.all(reparsed.syntax.source, reparsed.block)).at(0)
    assert.deepEqual(reparsedFence?.containers, fence.containers)
  }),
)

it.effect('preserves module markers and reports original physical fence ranges', () =>
  Effect.gen(function* () {
    const source = `//! Before β.
//! \`\`\`silk
//! fn old()->i32{return 1}
//! \`\`\`
pub fn main() -> i32 { return 1 }
`
    const { syntax, block } = firstBlock('memory://module.silk', source)
    const fence =
      (yield* CodeFence.all(syntax.source, block)).at(0) ?? unreachable('expected fence')
    const physical = decoder.decode(
      Uint8Array.from(syntax.source.bytes.slice(fence.source.start, fence.source.end)),
    )
    assert.strictEqual(physical, `\`\`\`silk\n//! fn old()->i32{return 1}\n//! \`\`\``)
    const rewritten = yield* CodeFence.rewrite(syntax.source, block, [
      { fence, body: 'fn main() -> i32 {\n  return 1\n}' },
    ])
    assert.match(decoder.decode(rewritten), /\/\/! {3}return 1/)
  }),
)

it.effect('returns typed source failures instead of throwing across the CommonMark boundary', () =>
  Effect.gen(function* () {
    const source = `/// \`\`\`silk
/// fn main() -> i32 { return 1 }
/// \`\`\`
pub fn documented() -> i32 { return 1 }
`
    const original = firstBlock('memory://original.silk', source)
    const foreign = parse('memory://foreign.silk', source)
    const attempted = yield* Effect.result(CodeFence.all(foreign.source, original.block))
    assert.isTrue(Result.isFailure(attempted))
    if (Result.isFailure(attempted)) {
      assert.strictEqual(attempted.failure._tag, 'CodeFenceError')
      assert.strictEqual(attempted.failure.reason._tag, 'InvalidSource')
    }
  }),
)
