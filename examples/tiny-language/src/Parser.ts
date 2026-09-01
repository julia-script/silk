import * as Effect from 'effect/Effect'
import * as Inspectable from 'effect/Inspectable'
import * as Diagnostic from './Diagnostic.js'
import * as Expression from './Expression.js'
import * as Program from './Program.js'
import * as Token from './Token.js'

interface State {
  readonly tokens: ReadonlyArray<Token.Token>
  index: number
}

interface BinaryInfo {
  readonly operator: Expression.BinaryOperator
  readonly precedence: number
}

const binaryOperators: ReadonlyMap<Token.Kind, BinaryInfo> = new Map([
  ['Less', { operator: '<', precedence: 5 }],
  ['Greater', { operator: '>', precedence: 5 }],
  ['Plus', { operator: '+', precedence: 10 }],
  ['Minus', { operator: '-', precedence: 10 }],
  ['Star', { operator: '*', precedence: 20 }],
  ['Slash', { operator: '/', precedence: 20 }],
  ['Percent', { operator: '%', precedence: 20 }],
])

const current = (state: State): Token.Token => {
  const token = state.tokens.at(state.index)
  if (token !== undefined) return token
  const end = state.tokens.at(-1)?.end ?? 0
  return Token.make('Eof', '', end, end)
}

const expected = (token: Token.Token, description: string): Diagnostic.ParseError =>
  new Diagnostic.ParseError({
    message: `Expected ${description}, found ${token.kind}`,
    start: token.start,
    end: token.end,
    expected: description,
    found: token.kind,
  })

const advance = (state: State): Token.Token => {
  const token = current(state)
  state.index += 1
  return token
}

const expect = Effect.fnUntraced(function* (
  state: State,
  kind: Token.Kind,
  description: string,
): Effect.fn.Return<Token.Token, Diagnostic.ParseError> {
  const token = current(state)
  if (token.kind !== kind) return yield* expected(token, description)
  return advance(state)
})

const parsePrimary = Effect.fnUntraced(function* (
  state: State,
): Effect.fn.Return<Expression.Expression, Diagnostic.ParseError> {
  const token = current(state)

  if (token.kind === 'Integer') {
    advance(state)
    return Expression.integer(BigInt(token.lexeme), token.start, token.end)
  }

  if (token.kind === 'Identifier') {
    advance(state)
    if (current(state).kind !== 'LeftParen') {
      return Expression.name(token.lexeme, token.start, token.end)
    }

    advance(state)
    const arguments_: Array<Expression.Expression> = []
    if (current(state).kind !== 'RightParen') {
      while (true) {
        arguments_.push(yield* parseExpressionInternal(state))
        if (current(state).kind !== 'Comma') break
        advance(state)
      }
    }
    const rightParen = yield* expect(state, 'RightParen', "')'")
    return Expression.call(token.lexeme, arguments_, token.start, rightParen.end)
  }

  if (token.kind === 'LeftParen') {
    advance(state)
    const expression = yield* parseExpressionInternal(state)
    yield* expect(state, 'RightParen', "')'")
    return expression
  }

  return yield* expected(token, 'an integer, identifier, or parenthesized expression')
})

const parseIf = Effect.fnUntraced(function* (
  state: State,
): Effect.fn.Return<Expression.Expression, Diagnostic.ParseError> {
  const ifToken = yield* expect(state, 'If', "'if'")
  const condition = yield* parseExpressionInternal(state)
  yield* expect(state, 'Then', "'then'")
  const onTrue = yield* parseExpressionInternal(state)
  yield* expect(state, 'Else', "'else'")
  const onFalse = yield* parseExpressionInternal(state)
  return Expression.ifExpression(condition, onTrue, onFalse, ifToken.start, onFalse.end)
})

const parseUnary = Effect.fnUntraced(function* (
  state: State,
): Effect.fn.Return<Expression.Expression, Diagnostic.ParseError> {
  const token = current(state)
  if (token.kind !== 'Minus') return yield* parsePrimary(state)

  advance(state)
  const operand = yield* parseUnary(state)
  return Expression.unary('-', operand, token.start, operand.end)
})

const parseBinary = Effect.fnUntraced(function* (
  state: State,
  minimumPrecedence: number,
): Effect.fn.Return<Expression.Expression, Diagnostic.ParseError> {
  let left = yield* parseUnary(state)

  while (true) {
    const info = binaryOperators.get(current(state).kind)
    if (info === undefined || info.precedence < minimumPrecedence) return left

    advance(state)
    const right = yield* parseBinary(state, info.precedence + 1)
    left = Expression.binary(info.operator, left, right, left.start, right.end)
  }
})

const parseExpressionInternal = Effect.fnUntraced(function* (
  state: State,
): Effect.fn.Return<Expression.Expression, Diagnostic.ParseError> {
  return current(state).kind === 'If' ? yield* parseIf(state) : yield* parseBinary(state, 0)
})

const parseFunction = Effect.fnUntraced(function* (
  state: State,
): Effect.fn.Return<Program.FunctionDefinition, Diagnostic.ParseError> {
  const fnToken = yield* expect(state, 'Fn', "'fn'")
  const nameToken = yield* expect(state, 'Identifier', 'a function name')
  yield* expect(state, 'LeftParen', "'('")

  const parameters: Array<Program.Parameter> = []
  const parameterNames = new Set<string>()
  if (current(state).kind !== 'RightParen') {
    while (true) {
      const parameterToken = yield* expect(state, 'Identifier', 'a parameter name')
      if (parameterNames.has(parameterToken.lexeme)) {
        return yield* new Diagnostic.ParseError({
          message: `Duplicate parameter ${Inspectable.toStringUnknown(parameterToken.lexeme)}`,
          start: parameterToken.start,
          end: parameterToken.end,
          expected: 'a unique parameter name',
          found: parameterToken.lexeme,
        })
      }
      parameterNames.add(parameterToken.lexeme)
      parameters.push(
        Program.parameter(parameterToken.lexeme, parameterToken.start, parameterToken.end),
      )
      if (current(state).kind !== 'Comma') break
      advance(state)
    }
  }

  yield* expect(state, 'RightParen', "')'")
  yield* expect(state, 'Equal', "'='")
  const body = yield* parseExpressionInternal(state)
  return Program.functionDefinition(nameToken.lexeme, parameters, body, fnToken.start, body.end)
})

export const parseExpression = Effect.fn('Parser.parseExpression')(function* (
  tokens: ReadonlyArray<Token.Token>,
): Effect.fn.Return<Expression.Expression, Diagnostic.ParseError> {
  const state: State = { tokens, index: 0 }
  const expression = yield* parseExpressionInternal(state)
  yield* expect(state, 'Eof', 'end of input')
  return expression
})

export const parse = Effect.fn('Parser.parse')(function* (
  tokens: ReadonlyArray<Token.Token>,
): Effect.fn.Return<Program.Program, Diagnostic.ParseError> {
  const state: State = { tokens, index: 0 }
  const functions: Array<Program.FunctionDefinition> = []
  while (current(state).kind === 'Fn') functions.push(yield* parseFunction(state))
  const eof = yield* expect(
    state,
    'Eof',
    "a function definition beginning with 'fn' or end of input",
  )
  return Program.make(functions, functions.at(0)?.start ?? eof.start, eof.end)
})
