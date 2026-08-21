import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FormattedDocument from '../src/FormattedDocument.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SyntaxFormatter from '../src/SyntaxFormatter.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import * as Type from '../src/Type.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

const parse = (source: string) =>
  Parser.parse(Lexer.lex(SourceFile.make('slices/Syntax', encoder.encode(source))))

const descendants = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
  node.children.flatMap(
    (child): ReadonlyArray<SyntaxTree.Node> =>
      SyntaxTree.isNode(child) ? [child, ...descendants(child)] : [],
  )

it('parses shared and exclusive slice types and borrow expressions losslessly', () => {
  const syntax = parse(`fn use(shared: &[i32], exclusive: &mut [i32]) -> i32 {
  return consume(&shared, &mut exclusive)
}`)
  const nodes = descendants(syntax.root)

  assert.strictEqual(nodes.filter((node) => node.kind === 'SliceType').length, 2)
  assert.strictEqual(nodes.filter((node) => node.kind === 'BorrowExpression').length, 2)
  assert.deepEqual(syntax.lexicalDiagnostics, [])
  assert.deepEqual(syntax.parserDiagnostics, [])
})

it('recovers a missing slice element and closing bracket at parameter boundaries', () => {
  const syntax = parse('fn broken(left: &[], right: &mut [i32) -> i32 { return 0 }')
  const missing = descendants(syntax.root)
    .flatMap((node) => node.children)
    .filter(SyntaxTree.isMissingToken)
    .map((token) => token.expected)

  assert.include(missing, 'Identifier')
  assert.include(missing, 'RightBracket')
  assert.strictEqual(
    syntax.root.children.filter(
      (child) => SyntaxTree.isNode(child) && child.kind === 'FunctionDeclaration',
    ).length,
    1,
  )
})

it.effect('formats slice syntax and borrow expressions idempotently', () =>
  Effect.gen(function* () {
    const syntax = parse(
      'fn use(values : & mut [ i32 ]) -> i32 { return consume ( & mut values ) }',
    )
    const formatted = yield* SyntaxFormatter.format(syntax)
    const text = decoder.decode(FormattedDocument.toUint8Array(formatted))
    assert.strictEqual(
      text,
      'fn use(values: &mut [i32]) -> i32 {\n  return consume(&mut values)\n}\n',
    )
    const again = yield* SyntaxFormatter.format(parse(text))
    assert.strictEqual(decoder.decode(FormattedDocument.toUint8Array(again)), text)
  }),
)

it('keeps slice identity, substitution, inference, and borrow containment canonical', () => {
  const parameter = Type.parameter({ module: 'slices/Syntax', name: 'scan' }, 0, 'T')
  const shared = Type.slice('Shared', parameter)
  const exclusive = Type.slice('Exclusive', Type.nominal('model/Token', 'Token'))
  const substitution = new Map([[Type.key(parameter), Type.nominal('model/Token', 'Token')]])

  assert.strictEqual(Type.encode(shared), '&[T]')
  assert.notStrictEqual(Type.key(shared), Type.key(exclusive))
  assert.strictEqual(Type.containsBorrow(Type.fixedArray(shared, 1)), true)
  assert.strictEqual(Type.containsBorrow(Type.nominal('model/Box', 'Box', [shared])), true)
  assert.strictEqual(Type.containsBorrow(Type.fixedArray('i32', 2)), false)
  assert.deepEqual(Type.substitute(shared, substitution), Type.slice('Shared', exclusive.element))

  const inferred = new Map<string, Type.Type>()
  assert.strictEqual(Type.infer(shared, Type.slice('Shared', exclusive.element), inferred), true)
  assert.deepEqual(inferred.get(Type.key(parameter)), exclusive.element)
  assert.strictEqual(Type.infer(shared, exclusive, new Map()), false)
})
