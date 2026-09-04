import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
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
  const file = Parser.parse(Lexer.lex(SourceFile.make('bitwise-operator/shape', ascii(source))))
  const outermost = outermostInfix(file.root)
  return outermost === undefined ? 'operand' : shapeOf(outermost)
}

it.effect('rejects mixed operand types exactly as the named operation rejects them', () =>
  Effect.gen(function* () {
    const viaOperator = yield* Analysis.ofSourceRealized(
      'bitwise-operator/mixed-operator',
      ascii(`fn mixed(a: u32, b: i32) -> u32 { return a & b }
pub fn main() -> i32 { return 0 }`),
    )
    const viaFunction = yield* Analysis.ofSourceRealized(
      'bitwise-operator/mixed-function',
      ascii(`import silk.u32 as u32
fn mixed(a: u32, b: i32) -> u32 { return u32.bitAnd(a, b) }
pub fn main() -> i32 { return 0 }`),
    )

    const operatorCodes = Analysis.diagnostics(viaOperator).map((diagnostic) => diagnostic.code)
    assert.notStrictEqual(operatorCodes.length, 0)
    assert.deepEqual(
      operatorCodes,
      Analysis.diagnostics(viaFunction).map((diagnostic) => diagnostic.code),
    )
  }),
)

it.effect('reports a type diagnostic instead of failing on a float bitwise operand', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'bitwise-operator/float',
      ascii(`fn f(a: f64, b: f64) -> f64 { return a & b }
fn g(a: f64) -> f64 { return ~a }
pub fn main() -> i32 { return 0 }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0012', 'SEM0012'],
    )
  }),
)

it('binds bitwise operators above comparison', () => {
  assert.deepEqual(infixShape('fn masked(a: u32, b: u32, c: u32) -> bool { return a & b == c }'), [
    'EqualEqual',
    ['Ampersand', 'operand', 'operand'],
    'operand',
  ])
})

it('orders `&` inside `^` inside `|`', () => {
  assert.deepEqual(
    infixShape('fn tiered(a: u32, b: u32, c: u32, d: u32) -> u32 { return a | b ^ c & d }'),
    ['Pipe', 'operand', ['Caret', 'operand', ['Ampersand', 'operand', 'operand']]],
  )
})
