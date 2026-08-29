import * as Diagnostic from '../Diagnostic.js'
import type { NodeResult, State } from '../internal/ParseState.js'
import {
  addDiagnostic,
  advance,
  consumeTrivia,
  currentToken,
  expect,
  nextSignificantKind,
  peek,
  syntaxNode,
} from '../internal/ParseState.js'
import * as SyntaxTree from '../SyntaxTree.js'
import type * as Token from '../Token.js'
import {
  isUniversalPatternStart,
  parseExpression,
  parseIdentifierExpression,
  parsePattern,
  parseProjectionChain,
} from './Expression.js'
import { expressionFollowing, expressionStarts } from './Grammar.js'

export const parseReturnStatement = (initial: State): NodeResult => {
  const keyword = expect(initial, 'ReturnKeyword', [...expressionStarts, 'RightBrace'])
  if (nextSignificantKind(keyword.state) === 'RightBrace') {
    const expression = syntaxNode(keyword.state, 'UnitExpression', [])
    return Object.freeze({
      state: keyword.state,
      node: syntaxNode(keyword.state, 'ReturnStatement', [...keyword.elements, expression]),
    })
  }
  const expression = parseExpression(keyword.state, 0, 'Integer')
  return Object.freeze({
    state: expression.state,
    node: syntaxNode(expression.state, 'ReturnStatement', [...keyword.elements, expression.node]),
  })
}

export const parseImplicitUnitReturnStatement = (initial: State): NodeResult => {
  const expression = syntaxNode(initial, 'UnitExpression', [])
  return Object.freeze({
    state: initial,
    node: syntaxNode(initial, 'ReturnStatement', [expression]),
  })
}

export const parseFailStatement = (initial: State): NodeResult => {
  const keyword = expect(initial, 'FailKeyword', ['MoveKeyword', ...expressionStarts])
  const move =
    nextSignificantKind(keyword.state) === 'MoveKeyword'
      ? expect(keyword.state, 'MoveKeyword', [...expressionStarts, ...expressionFollowing])
      : Object.freeze({ state: keyword.state, elements: Object.freeze([]) })
  const expression = parseExpression(move.state, 0, 'Identifier')
  return Object.freeze({
    state: expression.state,
    node: syntaxNode(expression.state, 'FailStatement', [
      ...keyword.elements,
      ...move.elements,
      expression.node,
    ]),
  })
}

export const parseDropStatement = (initial: State): NodeResult => {
  const keyword = expect(initial, 'DropKeyword', expressionStarts)
  const expression = parseExpression(keyword.state, 0, 'Identifier')
  return Object.freeze({
    state: expression.state,
    node: syntaxNode(expression.state, 'DropStatement', [...keyword.elements, expression.node]),
  })
}

export const parseExpressionStatement = (initial: State): NodeResult => {
  const expression = parseExpression(initial, 0, 'Identifier')
  return Object.freeze({
    state: expression.state,
    node: syntaxNode(expression.state, 'ExpressionStatement', [expression.node]),
  })
}

export const parseUnsafeStatement = (initial: State): NodeResult => {
  const keyword = expect(initial, 'UnsafeKeyword', ['LeftBrace', 'RightBrace'])
  const block = parseBlock(keyword.state, false)
  return Object.freeze({
    state: block.state,
    node: syntaxNode(block.state, 'UnsafeStatement', [...keyword.elements, block.node]),
  })
}

export const parseBindingStatement = (initial: State): NodeResult => {
  const keyword = expect(initial, 'LetKeyword', [
    'MutKeyword',
    'Identifier',
    'Equals',
    'ReturnKeyword',
    'UnsafeKeyword',
    'RightBrace',
  ])
  const mut =
    nextSignificantKind(keyword.state) === 'MutKeyword'
      ? expect(keyword.state, 'MutKeyword', ['Identifier', 'Equals', 'RightBrace'])
      : Object.freeze({ state: keyword.state, elements: Object.freeze([]) })
  const name = expect(mut.state, 'Identifier', [
    'Equals',
    'LetKeyword',
    'ReturnKeyword',
    'RightBrace',
  ])
  const equals = expect(name.state, 'Equals', [
    ...expressionStarts,
    'LetKeyword',
    'ReturnKeyword',
    'RightBrace',
  ])
  const expression = parseExpression(equals.state, 0, 'Integer')
  return Object.freeze({
    state: expression.state,
    node: syntaxNode(expression.state, 'BindingStatement', [
      ...keyword.elements,
      ...mut.elements,
      ...name.elements,
      ...equals.elements,
      expression.node,
    ]),
  })
}

export const startsPatternBindingStatement = (initial: State): boolean => {
  if (peek(initial, 1) === 'MutKeyword') return false
  if (peek(initial, 1) === 'Equals') return false
  if (peek(initial, 1) !== 'Identifier') return true
  const keyword = expect(initial, 'LetKeyword', ['Identifier', 'Equals'])
  return isUniversalPatternStart(keyword.state) || peek(initial, 2) !== 'Equals'
}

export const parsePatternBindingStatement = (initial: State): NodeResult => {
  const keyword = expect(initial, 'LetKeyword', ['Identifier', 'LeftBracket', 'Equals'])
  const pattern = parsePattern(keyword.state, ['Equals', 'RightBrace'])
  const equals = expect(pattern.state, 'Equals', [...expressionStarts, 'RightBrace'])
  const expression = parseExpression(equals.state, 0, 'Identifier')
  return Object.freeze({
    state: expression.state,
    node: syntaxNode(expression.state, 'PatternBindingStatement', [
      ...keyword.elements,
      pattern.node,
      ...equals.elements,
      expression.node,
    ]),
  })
}

export const parseAssignmentStatement = (initial: State): NodeResult => {
  const place = parseProjectionChain(parseIdentifierExpression(initial))
  const equals = expect(place.state, 'Equals', [
    ...expressionStarts,
    'LetKeyword',
    'IfKeyword',
    'WhileKeyword',
    'BreakKeyword',
    'ContinueKeyword',
    'ReturnKeyword',
    'FailKeyword',
    'RightBrace',
  ])
  const expression = parseExpression(equals.state, 0, 'Identifier')
  return Object.freeze({
    state: expression.state,
    node: syntaxNode(expression.state, 'AssignmentStatement', [
      place.node,
      ...equals.elements,
      expression.node,
    ]),
  })
}

export const startsAssignmentStatement = (state: State): boolean => {
  const place = parseProjectionChain(parseIdentifierExpression(state))
  return nextSignificantKind(place.state) === 'Equals'
}

export function parseConditionalStatement(initial: State): NodeResult {
  const keyword = expect(initial, 'IfKeyword', [...expressionStarts, 'LeftBrace', 'RightBrace'])
  if (nextSignificantKind(keyword.state) === 'LetKeyword') {
    const letKeyword = expect(keyword.state, 'LetKeyword', ['Identifier', 'LeftBracket', 'Equals'])
    const pattern = parsePattern(letKeyword.state, ['Equals', 'LeftBrace', 'RightBrace'])
    const equals = expect(pattern.state, 'Equals', [...expressionStarts, 'LeftBrace'])
    const subject = parseExpression(equals.state, 0, 'Identifier', false)
    const taken = parseBlock(subject.state, false)
    let state = taken.state
    let children: ReadonlyArray<SyntaxTree.Element> = Object.freeze([
      ...keyword.elements,
      ...letKeyword.elements,
      pattern.node,
      ...equals.elements,
      subject.node,
      taken.node,
    ])
    if (nextSignificantKind(state) === 'ElseKeyword') {
      const elseKeyword = expect(state, 'ElseKeyword', ['LeftBrace', 'IfKeyword'])
      const otherwise =
        nextSignificantKind(elseKeyword.state) === 'IfKeyword'
          ? parseConditionalStatement(elseKeyword.state)
          : parseBlock(elseKeyword.state, false)
      state = otherwise.state
      children = Object.freeze([...children, ...elseKeyword.elements, otherwise.node])
    }
    return Object.freeze({
      state,
      node: syntaxNode(state, 'PatternConditionalStatement', children),
    })
  }
  const condition = parseExpression(keyword.state, 0, 'Identifier', false)
  const taken = parseBlock(condition.state, false)
  let state = taken.state
  let children: ReadonlyArray<SyntaxTree.Element> = Object.freeze([
    ...keyword.elements,
    condition.node,
    taken.node,
  ])

  if (nextSignificantKind(state) === 'ElseKeyword') {
    const elseKeyword = expect(state, 'ElseKeyword', ['LeftBrace', 'IfKeyword'])
    const otherwise =
      nextSignificantKind(elseKeyword.state) === 'IfKeyword'
        ? parseConditionalStatement(elseKeyword.state)
        : parseBlock(elseKeyword.state, false)
    state = otherwise.state
    children = Object.freeze([...children, ...elseKeyword.elements, otherwise.node])
  }

  return Object.freeze({
    state,
    node: syntaxNode(state, 'ConditionalStatement', children),
  })
}

export function parseWhileStatement(initial: State): NodeResult {
  const keyword = expect(initial, 'WhileKeyword', [...expressionStarts, 'LeftBrace', 'RightBrace'])
  const condition = parseExpression(keyword.state, 0, 'Identifier', false)
  const body = parseBlock(condition.state, false)
  return Object.freeze({
    state: body.state,
    node: syntaxNode(body.state, 'WhileStatement', [
      ...keyword.elements,
      condition.node,
      body.node,
    ]),
  })
}

export const parseTransferStatement = (
  initial: State,
  keyword: 'BreakKeyword' | 'ContinueKeyword',
  kind: 'BreakStatement' | 'ContinueStatement',
): NodeResult => {
  const result = expect(initial, keyword, [
    'LetKeyword',
    'IfKeyword',
    'WhileKeyword',
    'BreakKeyword',
    'ContinueKeyword',
    'ReturnKeyword',
    'RightBrace',
  ])
  return Object.freeze({
    state: result.state,
    node: syntaxNode(result.state, kind, result.elements),
  })
}

export const startsBlockStatement = (state: State): boolean => {
  const kind = nextSignificantKind(state)
  if (kind === undefined) return false
  return (
    kind === 'LetKeyword' ||
    kind === 'ReturnKeyword' ||
    kind === 'FailKeyword' ||
    kind === 'DropKeyword' ||
    kind === 'UnsafeKeyword' ||
    kind === 'IfKeyword' ||
    kind === 'WhileKeyword' ||
    kind === 'BreakKeyword' ||
    kind === 'ContinueKeyword' ||
    expressionStarts.includes(kind)
  )
}

export const endsBlock = (state: State): boolean => {
  const kind = nextSignificantKind(state)
  return (
    kind === undefined ||
    kind === 'RightBrace' ||
    kind === 'EndOfFile' ||
    kind === 'ElseKeyword' ||
    kind === 'ImportKeyword' ||
    kind === 'PubKeyword' ||
    kind === 'ConstKeyword' ||
    kind === 'StructKeyword' ||
    kind === 'EnumKeyword' ||
    kind === 'UnionKeyword' ||
    kind === 'FnKeyword' ||
    kind === 'ImplKeyword' ||
    (kind === 'EffectKeyword' && peek(state, 1) === 'FnKeyword')
  )
}

export const parseErrorStatement = (initial: State): NodeResult => {
  const leading = consumeTrivia(initial)
  let state = leading.state
  let unexpected: ReadonlyArray<Token.Token> = Object.freeze([])
  let token = currentToken(state)

  while (token !== undefined) {
    if (unexpected.length > 0 && (startsBlockStatement(state) || endsBlock(state))) break
    unexpected = Object.freeze([...unexpected, token])
    state = advance(state)
    token = currentToken(state)
  }

  const error = syntaxNode(state, 'Error', unexpected)
  state = addDiagnostic(
    state,
    Diagnostic.unexpectedTokens(
      unexpected.map((item) => item.kind),
      'statement',
      [
        '`let`',
        '`return`',
        '`if`',
        '`while`',
        '`fail`',
        '`drop`',
        '`unsafe`',
        'an expression',
        '`}`',
      ],
      error.span,
    ),
  )
  return Object.freeze({
    state,
    node: syntaxNode(state, 'ErrorStatement', [...leading.elements, error]),
  })
}

export const blockTerminatesSyntactically = (
  children: ReadonlyArray<SyntaxTree.Element>,
): boolean => {
  const statementTerminates = (statement: SyntaxTree.Node): boolean => {
    if (statement.kind === 'ReturnStatement' || statement.kind === 'FailStatement') return true
    if (statement.kind === 'UnsafeStatement') {
      const block = SyntaxTree.directNode(statement, 'Block')
      return block !== undefined && blockTerminatesSyntactically(block.children)
    }
    if (statement.kind !== 'ConditionalStatement') return false
    const blocks = SyntaxTree.directNodes(statement, 'Block')
    const taken = blocks.at(0)
    if (taken === undefined || !blockTerminatesSyntactically(taken.children)) return false
    const chained = SyntaxTree.directNode(statement, 'ConditionalStatement')
    if (chained !== undefined) return statementTerminates(chained)
    const otherwise = blocks.at(1)
    return otherwise !== undefined && blockTerminatesSyntactically(otherwise.children)
  }
  for (const child of children) {
    if (SyntaxTree.isNode(child) && statementTerminates(child)) return true
  }
  return false
}

const parseBlockChild = (state: State): NodeResult => {
  const kind = nextSignificantKind(state)
  switch (kind) {
    case 'LetKeyword':
      if (startsPatternBindingStatement(state)) return parsePatternBindingStatement(state)
      return parseBindingStatement(state)
    case 'IfKeyword':
      return parseConditionalStatement(state)
    case 'WhileKeyword':
      return parseWhileStatement(state)
    case 'BreakKeyword':
      return parseTransferStatement(state, 'BreakKeyword', 'BreakStatement')
    case 'ContinueKeyword':
      return parseTransferStatement(state, 'ContinueKeyword', 'ContinueStatement')
    case 'ReturnKeyword':
      return parseReturnStatement(state)
    case 'FailKeyword':
      return parseFailStatement(state)
    case 'DropKeyword':
      return parseDropStatement(state)
    case 'Identifier':
      if (startsAssignmentStatement(state)) return parseAssignmentStatement(state)
      break
    case 'UnsafeKeyword':
      if (peek(state, 1) === 'LeftBrace') return parseUnsafeStatement(state)
      break
  }
  if (startsBlockStatement(state)) return parseExpressionStatement(state)
  return parseErrorStatement(state)
}

export function parseBlock(
  initial: State,
  requireReturn: boolean,
  implicitUnitReturn = false,
): NodeResult {
  const leftBrace = expect(initial, 'LeftBrace', [
    'LetKeyword',
    'IfKeyword',
    'WhileKeyword',
    'BreakKeyword',
    'ContinueKeyword',
    'ReturnKeyword',
    ...expressionStarts,
    'RightBrace',
  ])
  let state = leftBrace.state
  let children: ReadonlyArray<SyntaxTree.Element> = leftBrace.elements

  while (!endsBlock(state)) {
    const statement = parseBlockChild(state)
    children = Object.freeze([...children, statement.node])
    state = statement.state
  }

  if ((requireReturn || implicitUnitReturn) && !blockTerminatesSyntactically(children)) {
    const statement = parseImplicitUnitReturnStatement(state)
    children = Object.freeze([...children, statement.node])
  }

  const rightBrace = expect(state, 'RightBrace', [
    'LetKeyword',
    'IfKeyword',
    'WhileKeyword',
    'BreakKeyword',
    'ContinueKeyword',
    'ReturnKeyword',
    'FailKeyword',
    'UnsafeKeyword',
    'ElseKeyword',
    'PubKeyword',
    'StructKeyword',
    'EnumKeyword',
    'UnionKeyword',
    'FnKeyword',
    'EffectKeyword',
    'ImportKeyword',
  ])
  return Object.freeze({
    state: rightBrace.state,
    node: syntaxNode(rightBrace.state, 'Block', [...children, ...rightBrace.elements]),
  })
}
