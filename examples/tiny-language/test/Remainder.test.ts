import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Compiler from '../src/Compiler.js'
import * as Expression from '../src/Expression.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'

it.effect('tokenizes remainder as its own operator', () =>
  Effect.gen(function* () {
    const tokens = yield* Lexer.tokenize('7 % 4')
    assert.deepStrictEqual(
      tokens.map(({ kind, lexeme }) => ({ kind, lexeme })),
      [
        { kind: 'Integer', lexeme: '7' },
        { kind: 'Percent', lexeme: '%' },
        { kind: 'Integer', lexeme: '4' },
        { kind: 'Eof', lexeme: '' },
      ],
    )
  }),
)

it.effect('groups remainder with multiplication from the left', () =>
  Effect.gen(function* () {
    const expression = yield* Parser.parseExpression(yield* Lexer.tokenize('10 + 7 % 4 * 2'))
    assert.strictEqual(Expression.render(expression), '(+ 10 (* (% 7 4) 2))')
  }),
)

it.effect('lowers signed remainder to srem', () =>
  Effect.gen(function* () {
    const compilation = yield* Compiler.compile('fn isOdd(n) = n % 2 fn main() = isOdd(7)')
    assert.match(compilation.ir, /%remainder_0 = srem i32 %v0, 2/)
    assert.match(compilation.ir, /call i32 @isOdd\(i32 7\)/)
  }),
)
