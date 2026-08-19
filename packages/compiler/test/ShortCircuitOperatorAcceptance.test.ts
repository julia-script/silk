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

/** Returns the nested operator shape of the first infix expression one source spells. */
const infixShape = (source: string): InfixShape => {
  const file = Parser.parse(Lexer.lex(SourceFile.make('short-circuit/shape', ascii(source))))
  const outermost = outermostInfix(file.root)
  return outermost === undefined ? 'operand' : shapeOf(outermost)
}

/**
 * Proves the unevaluated operand by counting. `bump` writes through an exclusive borrow, which
 * the pure-right-operand rule allows, and returns the answer its caller asks for. Each counter
 * reading is the exact number of times the right operand actually ran.
 *
 * The gate is a parameter rather than a literal, so no engine can fold the operator away and
 * report the right answer without ever taking the branch.
 */
const counting = `fn bump(counter: &mut [i32], answer: bool) -> bool {
  counter[0] = counter[0] + 1
  return answer
}

fn conjunction(gate: bool) -> i32 {
  let mut counter = [0]
  if gate && bump(&mut counter, true) { return counter[0] }
  return counter[0]
}

fn disjunction(gate: bool) -> i32 {
  let mut counter = [0]
  if gate || bump(&mut counter, true) { return counter[0] }
  return counter[0]
}

pub fn main() -> i32 {
  if conjunction(false) == 0 {} else { return 1 }
  if conjunction(true) == 1 {} else { return 2 }
  if disjunction(true) == 0 {} else { return 3 }
  if disjunction(false) == 1 {} else { return 4 }
  return 42
}`

/**
 * The guard case the decision comment names: eager evaluation of the right operand traps exactly
 * when the left operand is `false`, so a passing run is itself the short-circuit proof.
 */
const guarding = `fn inRange(values: &[i32], index: usize) -> bool {
  return index < values.length && values[index] > 0
}

fn beyond(values: &[i32], index: usize) -> bool {
  return index >= values.length || values[index] > 0
}

pub fn main() -> i32 {
  let values = [1, 0, 3]
  if inRange(&values, 0) {} else { return 1 }
  if inRange(&values, 1) == false {} else { return 2 }
  if inRange(&values, 2) {} else { return 3 }
  if inRange(&values, 3) == false {} else { return 4 }
  if inRange(&values, 4000) == false {} else { return 5 }
  if beyond(&values, 3) {} else { return 6 }
  if beyond(&values, 1) == false {} else { return 7 }
  return 42
}`

it.effect(
  'runs the counting and guarding programs identically on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      for (const [name, source] of [
        ['counting', counting],
        ['guarding', guarding],
      ] as const) {
        const snapshot = yield* Analysis.ofSourceRealized(
          `short-circuit/${name}`,
          ascii(source),
          'wasm32-unknown-unknown',
        )
        assert.deepEqual(Analysis.diagnostics(snapshot), [], name)

        const evaluated = Analysis.evaluate(snapshot)
        assert.strictEqual(
          evaluated._tag,
          'Completed',
          `${name}: ${JSON.stringify(evaluated, (_, value) =>
            typeof value === 'bigint' ? value.toString() : value,
          )}`,
        )
        if (evaluated._tag !== 'Completed') return
        assert.strictEqual(evaluated.result.value, 42n, name)

        const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
        const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
        assert.strictEqual((instance.exports.silk_main as () => number)(), 42, name)
      }
    }),
  120_000,
)

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

it.effect('binds `&&` tighter than `||` and both looser than equality', () =>
  Effect.gen(function* () {
    assert.deepEqual(
      infixShape('fn f(a: bool, b: bool, c: bool) -> bool { return a && b || c }'),
      ['PipePipe', ['AmpersandAmpersand', 'operand', 'operand'], 'operand'],
      '`a && b || c` groups as `(a && b) || c`',
    )
    assert.deepEqual(
      infixShape('fn f(a: bool, b: bool, c: bool) -> bool { return a || b && c }'),
      ['PipePipe', 'operand', ['AmpersandAmpersand', 'operand', 'operand']],
      '`a || b && c` groups as `a || (b && c)`',
    )
    assert.deepEqual(
      infixShape('fn f(a: i32, b: i32) -> bool { return a == b && a != b }'),
      [
        'AmpersandAmpersand',
        ['EqualEqual', 'operand', 'operand'],
        ['BangEqual', 'operand', 'operand'],
      ],
      '`a == b && a != b` nests both equalities inside `&&`',
    )
    assert.deepEqual(
      infixShape('fn f(a: bool, b: bool, c: bool) -> bool { return a && b && c }'),
      ['AmpersandAmpersand', ['AmpersandAmpersand', 'operand', 'operand'], 'operand'],
      '`&&` is left-associative',
    )

    const snapshot = yield* Analysis.ofSourceRealized(
      'short-circuit/precedence',
      ascii(`fn tiered(a: bool, b: bool, c: bool) -> bool { return a && b || c }
fn grouped(a: bool, b: bool, c: bool) -> bool { return (a && b) || c }
fn other(a: bool, b: bool, c: bool) -> bool { return a || b && c }
fn otherGrouped(a: bool, b: bool, c: bool) -> bool { return a || (b && c) }
fn compared(a: i32, b: i32) -> bool { return a == b && a != b }

pub fn main() -> i32 {
  if tiered(true, false, true) == grouped(true, false, true) {} else { return 1 }
  if tiered(true, false, false) == false {} else { return 2 }
  if other(false, true, false) == otherGrouped(false, true, false) {} else { return 3 }
  if other(false, true, false) == false {} else { return 4 }
  if other(false, true, true) {} else { return 5 }
  if compared(1, 1) == false {} else { return 6 }
  return 42
}`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
  }),
)

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

it.effect('rejects an effect site in the right operand and accepts it in the left', () =>
  Effect.gen(function* () {
    const rejected = yield* Analysis.ofSourceRealized(
      'short-circuit/effect-right',
      ascii(`effect fn decide() -> bool { return true }
fn choose(flag: bool) -> bool { return flag && run decide() }
pub fn main() -> i32 { return 0 }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(rejected).map((diagnostic) => diagnostic.code),
      [Diagnostic.impureShortCircuitOperandCode],
    )

    const accepted = yield* Analysis.ofSourceRealized(
      'short-circuit/effect-left',
      ascii(`effect fn decide() -> bool { return true }
fn choose(flag: bool) -> bool { return (run decide()) && flag }
pub fn main() -> i32 {
  if choose(true) {} else { return 1 }
  if choose(false) == false {} else { return 2 }
  return 42
}`),
    )
    assert.deepEqual(
      Analysis.diagnostics(accepted),
      [],
      'the same effect site compiles unrestricted on the left, which always evaluates',
    )
    const evaluated = Analysis.evaluate(accepted)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
  }),
)

it.effect('rejects a move in the right operand and accepts it in the left', () =>
  Effect.gen(function* () {
    const rejected = yield* Analysis.ofSourceRealized(
      'short-circuit/move-right',
      ascii(`struct Flag { value: bool }
fn unwrap(flag: Flag) -> bool { return flag.value }
fn choose(gate: bool, flag: Flag) -> bool { return gate && unwrap(move flag) }
pub fn main() -> i32 { return 0 }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(rejected).map((diagnostic) => diagnostic.code),
      [Diagnostic.impureShortCircuitOperandCode],
    )

    const accepted = yield* Analysis.ofSourceRealized(
      'short-circuit/move-left',
      ascii(`struct Flag { value: bool }
fn unwrap(flag: Flag) -> bool { return flag.value }
fn choose(gate: bool, flag: Flag) -> bool { return unwrap(move flag) && gate }
pub fn main() -> i32 {
  if choose(true, Flag { value: true }) {} else { return 1 }
  if choose(false, Flag { value: true }) == false {} else { return 2 }
  return 42
}`),
    )
    assert.deepEqual(Analysis.diagnostics(accepted), [])
    const evaluated = Analysis.evaluate(accepted)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
  }),
)
