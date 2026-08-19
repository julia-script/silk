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

/** Returns the nested operator shape of the first infix expression one source spells. */
const infixShape = (source: string): InfixShape => {
  const file = Parser.parse(Lexer.lex(SourceFile.make('bitwise-operator/shape', ascii(source))))
  const outermost = outermostInfix(file.root)
  return outermost === undefined ? 'operand' : shapeOf(outermost)
}

const parity = `pub fn checksum(value: u32, mask: u32) -> u32 {
  let masked = value & mask
  let flipped = ~masked
  return flipped ^ mask
}

fn named(value: u32, mask: u32) -> u32 {
  return u32.bitXor(u32.bitNot(u32.bitAnd(value, mask)), mask)
}

fn agrees(a: u32, b: u32) -> u32 {
  if (a & b) == u32.bitAnd(a, b) {} else { return 1 }
  if (a | b) == u32.bitOr(a, b) {} else { return 2 }
  if (a ^ b) == u32.bitXor(a, b) {} else { return 3 }
  if ~a == u32.bitNot(a) {} else { return 4 }
  if checksum(a, b) == named(a, b) {} else { return 5 }
  return 0
}

fn tiered(a: u32, b: u32, c: u32) -> u32 { return a | b & c }

fn tieredThree(a: u32, b: u32, c: u32, d: u32) -> u32 { return a | b ^ c & d }

fn leftAssociative(a: u32, b: u32, c: u32) -> u32 { return a | b | c }

fn aboveComparison(a: u32, b: u32, c: u32) -> bool { return a & b == c }

fn doubled(value: u32) -> u32 { return value * 2 }

fn piped(a: u32, b: u32) -> u32 { return a & b |> doubled }

pub fn main() -> i32 {
  if agrees(3988292384, 255) == 0 {} else { return 1 }
  if agrees(0xff, 0x0f) == 0 {} else { return 2 }
  if agrees(0b1010, 0b0110) == 0 {} else { return 3 }
  if agrees(1_0, 3) == 0 {} else { return 4 }
  if tiered(8, 1, 2) == u32.bitOr(8, u32.bitAnd(1, 2)) {} else { return 5 }
  if tiered(8, 1, 2) == 8 {} else { return 6 }
  if tieredThree(8, 1, 3, 2) == u32.bitOr(8, u32.bitXor(1, u32.bitAnd(3, 2))) {} else { return 7 }
  if tieredThree(8, 1, 3, 2) == 11 {} else { return 8 }
  if leftAssociative(8, 1, 2) == 11 {} else { return 9 }
  if aboveComparison(12, 10, 8) {} else { return 10 }
  if aboveComparison(12, 10, 9) == false {} else { return 11 }
  if piped(12, 10) == 16 {} else { return 12 }
  if checksum(12, 10) == 4294967293 {} else { return 13 }
  return 42
}`

it.effect(
  'compiles the four bitwise operators to their named operations on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'bitwise-operator/parity',
        ascii(parity),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        JSON.stringify(evaluated, (_, value) =>
          typeof value === 'bigint' ? value.toString() : value,
        ),
      )
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    }),
  60_000,
)

it.effect('gives `a & b` the value the named operation gives', () =>
  Effect.gen(function* () {
    const source = `fn viaOperator(a: u32, b: u32) -> u32 { return a & b }
fn viaFunction(a: u32, b: u32) -> u32 { return u32.bitAnd(a, b) }

pub fn main() -> i32 {
  if viaOperator(3988292384, 255) == viaFunction(3988292384, 255) {} else { return 1 }
  if viaOperator(3988292384, 255) == 32 {} else { return 2 }
  return 42
}`
    const snapshot = yield* Analysis.ofSourceRealized('bitwise-operator/bit-and', ascii(source))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
  }),
)

it.effect('rejects mixed operand types exactly as the named operation rejects them', () =>
  Effect.gen(function* () {
    const viaOperator = yield* Analysis.ofSourceRealized(
      'bitwise-operator/mixed-operator',
      ascii(`fn mixed(a: u32, b: i32) -> u32 { return a & b }
pub fn main() -> i32 { return 0 }`),
    )
    const viaFunction = yield* Analysis.ofSourceRealized(
      'bitwise-operator/mixed-function',
      ascii(`fn mixed(a: u32, b: i32) -> u32 { return u32.bitAnd(a, b) }
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

it.effect('binds the bitwise operators above comparison', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'bitwise-operator/comparison-binding',
      ascii(`fn masked(a: u32, b: u32, c: u32) -> bool { return a & b == c }

pub fn main() -> i32 {
  if masked(12, 10, 8) {} else { return 1 }
  if masked(12, 10, 9) == false {} else { return 2 }
  return 42
}`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot),
      [],
      '`a & b == c` groups as `(a & b) == c`, so both comparison operands are `u32`',
    )
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)

    assert.deepEqual(
      infixShape('fn masked(a: u32, b: u32, c: u32) -> bool { return a & b == c }'),
      ['EqualEqual', ['Ampersand', 'operand', 'operand'], 'operand'],
      '`a & b == c` nests the bitwise operator inside equality',
    )
  }),
)

it.effect('orders `&` inside `^` inside `|`', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'bitwise-operator/tier-order',
      ascii(`fn tiered(a: u32, b: u32, c: u32, d: u32) -> u32 { return a | b ^ c & d }
fn grouped(a: u32, b: u32, c: u32, d: u32) -> u32 { return a | (b ^ (c & d)) }

pub fn main() -> i32 {
  if tiered(8, 1, 3, 2) == grouped(8, 1, 3, 2) {} else { return 1 }
  if tiered(8, 1, 3, 2) == 11 {} else { return 2 }
  return 42
}`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)

    assert.deepEqual(
      infixShape('fn tiered(a: u32, b: u32, c: u32, d: u32) -> u32 { return a | b ^ c & d }'),
      ['Pipe', 'operand', ['Caret', 'operand', ['Ampersand', 'operand', 'operand']]],
      '`a | b ^ c & d` nests as `a | (b ^ (c & d))`',
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
