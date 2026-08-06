import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import * as Analysis from '../src/Analysis.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const parse = (source: string) =>
  Parser.parse(Lexer.lex(SourceFile.make('array-syntax/main', ascii(source))))

const descendants = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
  node.children.flatMap(
    (child): ReadonlyArray<SyntaxTree.Node> =>
      SyntaxTree.isNode(child) ? [child, ...descendants(child)] : [],
  )

it('parses recursive array types, complete literals, and mixed postfix chains losslessly', () => {
  const source = `struct Cell { value: I32 }
fn read(matrix: Array<Array<Cell, 4>, 3>, row: I32, column: I32) -> I32 {
  let values = [[Cell { value: 1 }],]
  return matrix[row][column].value
}`
  const syntax = parse(source)
  const kinds = descendants(syntax.root).map((node) => node.kind)

  assert.strictEqual(kinds.filter((kind) => kind === 'FixedArrayType').length, 2)
  assert.strictEqual(kinds.filter((kind) => kind === 'ArrayLiteralExpression').length, 2)
  assert.strictEqual(kinds.filter((kind) => kind === 'IndexProjectionExpression').length, 2)
  assert.include(kinds, 'FieldProjectionExpression')
  assert.deepEqual(syntax.parserDiagnostics, [])
  assert.deepEqual(
    SyntaxTree.tokens(syntax.root)
      .filter((token) => token.kind !== 'EndOfFile')
      .flatMap((token) =>
        Array.from(Option.getOrThrow(SourceFile.slice(syntax.source, token.span))),
      ),
    Array.from(ascii(source)),
  )
})

it('contains missing array length and closing-bracket recovery', () => {
  const syntax = parse(
    `struct Broken { values: Array<I32, > }
pub fn main(values: Array<I32, 2>, index: I32) -> I32 { return values[index }`,
  )
  const missing = descendants(syntax.root)
    .flatMap((node) => node.children)
    .filter(SyntaxTree.isMissingToken)

  assert.deepEqual(
    missing.map((token) => token.expected),
    ['DecimalInteger', 'RightBracket'],
  )
  assert.strictEqual(
    syntax.root.children.filter(
      (child) => SyntaxTree.isNode(child) && child.kind === 'FunctionDeclaration',
    ).length,
    1,
  )
})

it.effect('resolves a namespace-qualified array element to canonical identity', () =>
  Effect.gen(function* () {
    const root = SourceFile.make(
      'app/Main',
      ascii(
        `import model.Token as Model { Token }
pub fn keep(values: Array<Model.Token, 8>) -> Array<Model.Token, 8> { return values }
pub fn main() -> I32 { return 0 }`,
      ),
    )
    const snapshot = yield* Analysis.make({ root }).pipe(
      Effect.provide(
        SourceResolver.memory(
          new Map([['model/Token', ascii('pub struct Token { pub kind: I32 }')]]),
        ),
      ),
    )
    const lookup = Analysis.declarationByName(snapshot, 'app/Main', 'keep')
    assert.strictEqual(lookup._tag, 'Resolved')
    if (lookup._tag !== 'Resolved') return
    const declaration = lookup.declaration
    const parameter = declaration.parameters.at(0)?.declaredType
    const result = declaration.returnType
    assert.strictEqual(parameter?._tag, 'Resolved')
    assert.strictEqual(result._tag, 'Resolved')
    if (parameter?._tag !== 'Resolved' || result._tag !== 'Resolved') return
    assert.strictEqual(Type.encode(parameter.type), 'Array<model/Token.Token, 8>')
    assert.strictEqual(Type.equals(parameter.type, result.type), true)
  }),
)
