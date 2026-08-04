import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Expression from '../src/Expression.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as Program from '../src/Program.js'

const parse = Effect.fnUntraced(function* (source: string) {
  return yield* Parser.parseExpression(yield* Lexer.tokenize(source))
})

const parseProgram = Effect.fnUntraced(function* (source: string) {
  return yield* Parser.parse(yield* Lexer.tokenize(source))
})

it.effect('gives multiplication higher precedence than addition', () =>
  Effect.gen(function* () {
    assert.strictEqual(Expression.render(yield* parse('1 + 2 * 3')), '(+ 1 (* 2 3))')
  }),
)

it.effect('lets parentheses override precedence', () =>
  Effect.gen(function* () {
    assert.strictEqual(Expression.render(yield* parse('(1 + 2) * 3')), '(* (+ 1 2) 3)')
  }),
)

it.effect('keeps subtraction left associative', () =>
  Effect.gen(function* () {
    assert.strictEqual(Expression.render(yield* parse('10 - 3 - 2')), '(- (- 10 3) 2)')
  }),
)

it.effect('binds unary negation more tightly than multiplication', () =>
  Effect.gen(function* () {
    assert.strictEqual(Expression.render(yield* parse('-2 * 3')), '(* (- 2) 3)')
  }),
)

it.effect('parses the complete score program into one immutable AST', () =>
  Effect.gen(function* () {
    const program = yield* parseProgram(`fn abs(x) = if x < 0 then -x else x
fn score(x, y) = abs(x - y) * 3 + 2
fn main() = score(4, 10)`)

    assert.strictEqual(
      Program.render(program),
      '(program (fn abs (x) (if (< x 0) (- x) x)) (fn score (x y) (+ (* (call abs (- x y)) 3) 2)) (fn main () (call score 4 10)))',
    )
    assert.ok(Object.isFrozen(program))
    assert.ok(Object.isFrozen(program.functions))
  }),
)

it.effect('parses calls and nested conditionals as expressions', () =>
  Effect.gen(function* () {
    assert.strictEqual(
      Expression.render(yield* parse('f(1, g(2 + 3))')),
      '(call f 1 (call g (+ 2 3)))',
    )
    assert.strictEqual(
      Expression.render(yield* parse('if x then if y then 1 else 2 else 3')),
      '(if x (if y 1 2) 3)',
    )
  }),
)

it.effect('reports a missing else at the current source span', () =>
  Effect.gen(function* () {
    const error = yield* Effect.flip(parseProgram('fn main() = if 1 then 2'))
    if (error._tag !== 'ParseError') return assert.fail('expected ParseError')
    assert.strictEqual(error.expected, "'else'")
    assert.strictEqual(error.start, 23)
    assert.strictEqual(error.end, 23)
  }),
)

it.effect('rejects duplicate parameters and trailing syntax', () =>
  Effect.gen(function* () {
    const duplicate = yield* Effect.flip(parseProgram('fn add(x, x) = x'))
    if (duplicate._tag !== 'ParseError') return assert.fail('expected ParseError')
    assert.strictEqual(duplicate.expected, 'a unique parameter name')
    assert.strictEqual(duplicate.found, 'x')

    const trailing = yield* Effect.flip(parseProgram('fn main() = 1 nope'))
    assert.strictEqual(trailing.found, 'Identifier')
    assert.strictEqual(trailing.start, 14)
  }),
)
