import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FormattedDocument from '../src/FormattedDocument.js'
import * as Formatter from '../src/Formatter.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SyntaxTree from '../src/SyntaxTree.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

const parse = (id: string, source: string) =>
  Parser.parse(Lexer.lex(SourceFile.make(id, encoder.encode(source))))

const descendants = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Element> =>
  node.children.flatMap(
    (child): ReadonlyArray<SyntaxTree.Element> =>
      SyntaxTree.isNode(child) ? [child, ...descendants(child)] : [child],
  )

const formatted = Effect.fnUntraced(function* (source: string) {
  const document = yield* Formatter.format(parse('opaque-result/format', source))
  return decoder.decode(FormattedDocument.toUint8Array(document))
})

it('parses a contextual some binder only in result position', () => {
  const syntax = parse(
    'opaque-result/parse',
    `pub struct Parser<F: fn(i32) -> i32> { parse: F }
pub fn make() -> some<F: fn(i32) -> i32> Parser<F> { return 0 }`,
  )
  assert.deepEqual(syntax.parserDiagnostics, [])
  const binders = descendants(syntax.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'OpaqueResultType',
  )
  assert.strictEqual(binders.length, 1)
  assert.strictEqual(
    SyntaxTree.directNode(binders[0] ?? assert.fail('binder'), 'TypeParameterList') !== undefined,
    true,
  )
})

it('keeps some usable as an ordinary type name outside result position', () => {
  const syntax = parse(
    'opaque-result/contextual',
    `pub struct some<T> { value: T }
pub fn hold(value: some<i32>) -> i32 { return 0 }`,
  )
  assert.deepEqual(syntax.parserDiagnostics, [])
  assert.strictEqual(
    descendants(syntax.root).some(
      (element) => SyntaxTree.isNode(element) && element.kind === 'OpaqueResultType',
    ),
    false,
  )
})

it.effect('formats a contextual some binder over its complete result', () =>
  Effect.gen(function* () {
    const text = yield* formatted(
      `pub struct Parser<F:fn(i32)->i32>{parse:F}
pub fn make()->some<F:fn(i32)->i32>Parser<F>{return 0}`,
    )
    assert.include(text, '-> some<F: fn(i32) -> i32> Parser<F>')
    assert.strictEqual(yield* formatted(text), text)
  }),
)

it('contains a damaged some binder and still parses the next declaration', () => {
  const syntax = parse(
    'opaque-result/recovery',
    `pub fn broken() -> some<F: fn(i32) -> > Parser<F> { return 0 }
pub fn next() -> i32 { return 0 }`,
  )
  assert.strictEqual(
    syntax.root.children.filter(
      (child) => SyntaxTree.isNode(child) && child.kind === 'FunctionDeclaration',
    ).length,
    2,
  )
})
