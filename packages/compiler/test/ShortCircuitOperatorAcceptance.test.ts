import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Diagnostic from '../src/Diagnostic.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import type * as Token from '../src/Token.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const infixOperatorKinds: ReadonlyArray<Token.TokenKind> = Object.freeze([
  'Star',
  'Slash',
  'Percent',
  'Plus',
  'Minus',
  'Ampersand',
  'Caret',
  'Pipe',
  'Less',
  'LessEqual',
  'Greater',
  'GreaterEqual',
  'EqualEqual',
  'BangEqual',
  'AmpersandAmpersand',
  'PipePipe',
])

const outermostInfix = (element: SyntaxTree.Element): SyntaxTree.Node | undefined => {
  if (!SyntaxTree.isNode(element)) return undefined
  if (element.kind === 'InfixExpression') return element
  for (const child of element.children) {
    const found = outermostInfix(child)
    if (found !== undefined) return found
  }
  return undefined
}

type InfixShape = string | readonly [string, InfixShape, InfixShape]

const shapeOf = (element: SyntaxTree.Element): InfixShape => {
  if (!SyntaxTree.isNode(element) || element.kind !== 'InfixExpression') return 'operand'
  const kind = infixOperatorKinds.find(
    (candidate) => SyntaxTree.directToken(element, candidate) !== undefined,
  )
  const operands = element.children.filter(SyntaxTree.isNode)
  const left = operands.at(0)
  const right = operands.at(-1)
  return [
    kind ?? 'unknown',
    left === undefined ? 'operand' : shapeOf(left),
    right === undefined || operands.length < 2 ? 'operand' : shapeOf(right),
  ]
}

const infixShape = (source: string): InfixShape => {
  const file = Parser.parse(Lexer.lex(SourceFile.make('short-circuit/shape', ascii(source))))
  const outermost = outermostInfix(file.root)
  return outermost === undefined ? 'operand' : shapeOf(outermost)
}

it.effect('lexes `&&` and `||` as one token each', () =>
  Effect.gen(function* () {
    const lexed = Lexer.lex(
      SourceFile.make(
        'short-circuit/tokens',
        ascii('fn both(a: bool, b: bool) -> bool { return a && b || a & b == b }'),
      ),
    )
    const kinds = lexed.tokens
      .filter(
        (token) =>
          token.kind === 'AmpersandAmpersand' ||
          token.kind === 'PipePipe' ||
          token.kind === 'Ampersand' ||
          token.kind === 'Pipe',
      )
      .map((token) => token.kind)
    assert.deepEqual(kinds, ['AmpersandAmpersand', 'PipePipe', 'Ampersand'])
    const compound = lexed.tokens.filter(
      (token) => token.kind === 'AmpersandAmpersand' || token.kind === 'PipePipe',
    )
    for (const token of compound) assert.strictEqual(token.span.end - token.span.start, 2)
    yield* Effect.void
  }),
)

it('binds `&&` tighter than `||` and both looser than equality', () => {
  assert.deepEqual(infixShape('fn f(a: bool, b: bool, c: bool) -> bool { return a && b || c }'), [
    'PipePipe',
    ['AmpersandAmpersand', 'operand', 'operand'],
    'operand',
  ])
  assert.deepEqual(infixShape('fn f(a: bool, b: bool, c: bool) -> bool { return a || b && c }'), [
    'PipePipe',
    'operand',
    ['AmpersandAmpersand', 'operand', 'operand'],
  ])
  assert.deepEqual(infixShape('fn f(a: i32, b: i32) -> bool { return a == b && a != b }'), [
    'AmpersandAmpersand',
    ['EqualEqual', 'operand', 'operand'],
    ['BangEqual', 'operand', 'operand'],
  ])
})

it.effect('rejects a non-bool operand on either side', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'short-circuit/non-bool',
      ascii(`fn left(a: i32, b: bool) -> bool { return a && b }
fn right(a: bool, b: i32) -> bool { return a || b }
pub fn main() -> i32 { return 0 }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [Diagnostic.argumentTypeMismatchCode, Diagnostic.argumentTypeMismatchCode],
    )
  }),
)

it.effect('rejects a use reached after one short-circuit path moves its owner', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'short-circuit/conditional-use-after-move',
      ascii(`struct Flag { value: bool }
fn unwrap(flag: Flag) -> bool { return flag.value }
fn invalid(gate: bool, flag: Flag) -> bool {
  let selected = gate && unwrap(move flag)
  return selected && flag.value
}
pub fn main() -> i32 { return 0 }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [Diagnostic.useAfterMoveCode],
    )
  }),
)
