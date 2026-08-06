import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const descendants = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
  node.children.flatMap(
    (child): ReadonlyArray<SyntaxTree.Node> =>
      SyntaxTree.isNode(child) ? [child, ...descendants(child)] : [],
  )

it('parses nested structural union types losslessly', () => {
  const source = SourceFile.make(
    'union-syntax/main',
    ascii('fn choose(value: Token | (End | Token)) -> End | Token { return value }'),
  )
  const syntax = Parser.parse(Lexer.lex(source))
  const kinds = descendants(syntax.root).map((node) => node.kind)
  assert.strictEqual(kinds.filter((kind) => kind === 'UnionType').length, 3)
  assert.include(kinds, 'ParenthesizedType')
  assert.strictEqual(syntax.tokens.filter((token) => token.kind === 'Pipe').length, 3)
  assert.deepEqual(syntax.parserDiagnostics, [])
})

it('contains a missing union member at the declared-type boundary', () => {
  const source = SourceFile.make(
    'union-syntax/recovery',
    ascii('fn broken(value: Token |) -> Never { return value }\nfn next() -> I32 { return 0 }'),
  )
  const syntax = Parser.parse(Lexer.lex(source))
  const missing = descendants(syntax.root)
    .flatMap((node) => node.children)
    .filter(SyntaxTree.isMissingToken)
  assert.include(
    missing.map((token) => token.expected),
    'Identifier',
  )
  assert.strictEqual(
    syntax.root.children.filter(
      (child) => SyntaxTree.isNode(child) && child.kind === 'FunctionDeclaration',
    ).length,
    2,
  )
})

it.effect('resolves equivalent union spellings to one canonical type', () =>
  Effect.gen(function* () {
    const root = SourceFile.make(
      'union-syntax/main',
      ascii(`import model.Types { Token, End }
fn first(value: Token | (End | Token)) -> End | Token { return value }
pub fn main() -> I32 { return 0 }`),
    )
    const snapshot = yield* Analysis.make({ root }).pipe(
      Effect.provide(
        SourceResolver.memory(
          new Map([['model/Types', ascii('pub struct Token {}\npub struct End {}')]]),
        ),
      ),
    )
    const lookup = Analysis.declarationByName(snapshot, 'union-syntax/main', 'first')
    assert.strictEqual(lookup._tag, 'Resolved')
    if (lookup._tag !== 'Resolved') return
    const parameter = lookup.declaration.parameters.at(0)?.declaredType
    const result = lookup.declaration.returnType
    assert.strictEqual(parameter?._tag, 'Resolved')
    assert.strictEqual(result._tag, 'Resolved')
    if (parameter?._tag !== 'Resolved' || result._tag !== 'Resolved') return
    assert.strictEqual(Type.equals(parameter.type, result.type), true)
    assert.strictEqual(Type.encode(result.type), 'model/Types.End | model/Types.Token')
    assert.strictEqual(parameter.unionSource?.members.length, 2)
  }),
)
