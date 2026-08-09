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
  const source = `struct Cell { value: i32 }
fn read(matrix: [[Cell; 4]; 3], row: usize, column: usize) -> i32 {
  let values = [[Cell { value: 1 }],]
  return matrix[row][column].value
}`
  const syntax = parse(source)
  const kinds = descendants(syntax.root).map((node) => node.kind)
  const fixedArrays = descendants(syntax.root).filter((node) => node.kind === 'FixedArrayType')

  assert.strictEqual(fixedArrays.length, 2)
  assert.strictEqual(
    fixedArrays.every(
      (node) =>
        SyntaxTree.directToken(node, 'LeftBracket') !== undefined &&
        SyntaxTree.directToken(node, 'Semicolon') !== undefined &&
        SyntaxTree.directToken(node, 'RightBracket') !== undefined,
    ),
    true,
  )
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
    `struct Broken { values: [i32; ] }
pub fn main(values: [i32; 2], index: usize) -> i32 { return values[index }`,
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

it('parses simple and zero-length fixed-array types', () => {
  const syntax = parse('struct Arrays { values: [i32; 2] empty: [i32; 0] }')
  const fixedArrays = descendants(syntax.root).filter((node) => node.kind === 'FixedArrayType')

  assert.strictEqual(fixedArrays.length, 2)
  assert.deepEqual(
    fixedArrays.map((node) => {
      const token = SyntaxTree.directToken(node, 'DecimalInteger')
      if (token === undefined) return undefined
      return String.fromCharCode(...Option.getOrThrow(SourceFile.slice(syntax.source, token.span)))
    }),
    ['2', '0'],
  )
  assert.deepEqual(syntax.parserDiagnostics, [])
})

it('recovers missing fixed-array separators and closing brackets locally', () => {
  const missingSeparator = parse('struct Broken { values: [i32 2] }')
  const missingBracket = parse('struct Broken { values: [i32; 2 }')

  assert.include(
    descendants(missingSeparator.root)
      .flatMap((node) => node.children)
      .filter(SyntaxTree.isMissingToken)
      .map((token) => token.expected),
    'Semicolon',
  )
  assert.include(
    descendants(missingBracket.root)
      .flatMap((node) => node.children)
      .filter(SyntaxTree.isMissingToken)
      .map((token) => token.expected),
    'RightBracket',
  )
})

it('rejects the former angle-bracketed array source spelling', () => {
  const syntax = parse('struct Old { values: Array<i32, 4> }')

  assert.strictEqual(
    descendants(syntax.root).some((node) => node.kind === 'FixedArrayType'),
    false,
  )
  assert.isAbove(syntax.parserDiagnostics.length, 0)
})

it.effect('resolves a namespace-qualified array element to canonical identity', () =>
  Effect.gen(function* () {
    const root = SourceFile.make(
      'app/Main',
      ascii(
        `import model.Token as Model { Token }
pub fn keep(values: [Model.Token; 8]) -> [Model.Token; 8] { return values }
pub fn main() -> i32 { return 0 }`,
      ),
    )
    const snapshot = yield* Analysis.makeRealized({ root }).pipe(
      Effect.provide(
        SourceResolver.memory(
          new Map([['model/Token', ascii('pub struct Token { pub kind: i32 }')]]),
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
