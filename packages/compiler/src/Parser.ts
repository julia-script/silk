import * as Option from 'effect/Option'
import * as Diagnostic from './Diagnostic.js'
import type * as Lexer from './Lexer.js'
import * as Operator from './Operator.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'

interface State {
  readonly lexical: Lexer.LexicalResult
  readonly index: number
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

interface ElementsResult {
  readonly state: State
  readonly elements: ReadonlyArray<SyntaxTree.Element>
}

interface NodeResult {
  readonly state: State
  readonly node: SyntaxTree.Node
}

const triviaKinds: ReadonlyArray<Token.TokenKind> = Object.freeze([
  'Whitespace',
  'LineComment',
  'DocComment',
])

const isTrivia = (kind: Token.TokenKind): boolean => triviaKinds.includes(kind)

const currentToken = (state: State): Token.Token | undefined => state.lexical.tokens.at(state.index)

const nextSignificantKind = (state: State): Token.TokenKind | undefined => {
  let index = state.index
  let token = state.lexical.tokens.at(index)

  while (token !== undefined && isTrivia(token.kind)) {
    index += 1
    token = state.lexical.tokens.at(index)
  }

  return token?.kind
}

const advance = (state: State): State =>
  Object.freeze({
    ...state,
    index: state.index + 1,
  })

const addDiagnostic = (state: State, diagnostic: Diagnostic.Diagnostic): State =>
  Object.freeze({
    ...state,
    diagnostics: Object.freeze([...state.diagnostics, diagnostic]),
  })

const insertionOffset = (state: State): number =>
  currentToken(state)?.span.start ?? state.lexical.source.bytes.length

const syntaxNode = (
  state: State,
  kind: SyntaxTree.NodeKind,
  children: ReadonlyArray<SyntaxTree.Element>,
): SyntaxTree.Node =>
  Option.getOrThrowWith(
    SyntaxTree.make(state.lexical.source, kind, children, insertionOffset(state)),
    () => new RangeError(`Parser produced invalid ordered children for ${kind}`),
  )

const missingToken = (state: State, expected: Token.TokenKind): SyntaxTree.MissingToken =>
  Option.getOrThrowWith(
    SyntaxTree.missingToken(state.lexical.source, expected, insertionOffset(state)),
    () => new RangeError(`Parser produced an invalid insertion position for ${expected}`),
  )

const consumeTrivia = (initial: State): ElementsResult => {
  let state = initial
  let elements: ReadonlyArray<SyntaxTree.Element> = Object.freeze([])
  let token = currentToken(state)

  while (token !== undefined && isTrivia(token.kind)) {
    elements = Object.freeze([...elements, token])
    state = advance(state)
    token = currentToken(state)
  }

  return Object.freeze({ state, elements })
}

const isSynchronizationKind = (
  kind: Token.TokenKind,
  expected: Token.TokenKind,
  following: ReadonlyArray<Token.TokenKind>,
): boolean => kind === expected || kind === 'EndOfFile' || following.includes(kind)

const expect = (
  initial: State,
  expected: Token.TokenKind,
  following: ReadonlyArray<Token.TokenKind>,
): ElementsResult => {
  const leading = consumeTrivia(initial)
  let state = leading.state
  let elements = leading.elements
  let token = currentToken(state)

  if (token?.kind === expected) {
    return Object.freeze({
      state: advance(state),
      elements: Object.freeze([...elements, token]),
    })
  }

  let unexpected: ReadonlyArray<Token.Token> = Object.freeze([])
  while (token !== undefined && !isSynchronizationKind(token.kind, expected, following)) {
    unexpected = Object.freeze([...unexpected, token])
    state = advance(state)
    token = currentToken(state)
  }

  if (unexpected.length > 0) {
    const error = syntaxNode(state, 'Error', unexpected)
    state = addDiagnostic(state, Diagnostic.unexpectedTokens(error.span))
    elements = Object.freeze([...elements, error])
  }

  if (token?.kind === expected) {
    return Object.freeze({
      state: advance(state),
      elements: Object.freeze([...elements, token]),
    })
  }

  const missing = missingToken(state, expected)
  return Object.freeze({
    state: addDiagnostic(state, Diagnostic.missingToken(expected, missing.span)),
    elements: Object.freeze([...elements, missing]),
  })
}

const expressionFollowing: ReadonlyArray<Token.TokenKind> = Object.freeze([
  'Comma',
  'RightParenthesis',
  'RightBracket',
  'LeftBrace',
  'RightBrace',
  'Equals',
  'FatArrow',
  'LetKeyword',
  'IfKeyword',
  'WhileKeyword',
  'BreakKeyword',
  'ContinueKeyword',
  'ReturnKeyword',
  'ElseKeyword',
  'PubKeyword',
  'StructKeyword',
  'FnKeyword',
  'ImportKeyword',
  'MatchKeyword',
])

const expressionStarts: ReadonlyArray<Token.TokenKind> = Object.freeze([
  'DecimalInteger',
  'Identifier',
  'LeftBrace',
  'MoveKeyword',
  'TrueKeyword',
  'FalseKeyword',
  'Minus',
  'Bang',
  'LeftParenthesis',
  'LeftBracket',
  'MatchKeyword',
  'Less',
])

const typeStarts: ReadonlyArray<Token.TokenKind> = Object.freeze([
  'Identifier',
  'LeftParenthesis',
  'LeftBracket',
])

const significantKindAfter = (
  state: State,
  significantCount: number,
): Token.TokenKind | undefined => {
  let index = state.index
  let remaining = significantCount
  while (index < state.lexical.tokens.length) {
    const token = state.lexical.tokens.at(index)
    if (token === undefined) return undefined
    index += 1
    if (isTrivia(token.kind)) continue
    if (remaining === 0) return token.kind
    remaining -= 1
  }
  return undefined
}

const reservedTemplateStart = (state: State): boolean => {
  let index = state.index
  let token = state.lexical.tokens.at(index)
  while (token !== undefined && isTrivia(token.kind)) {
    index += 1
    token = state.lexical.tokens.at(index)
  }
  if (token?.kind !== 'Less') return false
  const following = state.lexical.tokens.at(index + 1)
  return following?.kind === 'Identifier' || following?.kind === 'Greater'
}

/** True only for `callee<T, ...>(` with balanced angles; comparisons never enter call parsing. */
const hasCompleteAppliedPostfix = (
  state: State,
  postfix: 'LeftParenthesis' | 'LeftBrace',
): boolean => {
  let index = state.index
  const significant = (): Token.Token | undefined => {
    let token = state.lexical.tokens.at(index)
    while (token !== undefined && isTrivia(token.kind)) {
      index += 1
      token = state.lexical.tokens.at(index)
    }
    return token
  }
  if (significant()?.kind !== 'Identifier') return false
  index += 1
  if (significant()?.kind === 'Dot') {
    index += 1
    if (significant()?.kind !== 'Identifier') return false
    index += 1
  }
  if (significant()?.kind !== 'Less') return false
  let depth = 0
  while (index < state.lexical.tokens.length) {
    const token = significant()
    if (token === undefined) return false
    if (token.kind === 'Less') depth += 1
    else if (token.kind === 'Greater') {
      depth -= 1
      if (depth === 0) {
        index += 1
        return significant()?.kind === postfix
      }
    } else if (
      token.kind !== 'Identifier' &&
      token.kind !== 'LeftBracket' &&
      token.kind !== 'Dot' &&
      token.kind !== 'Comma' &&
      token.kind !== 'LeftParenthesis' &&
      token.kind !== 'RightParenthesis' &&
      token.kind !== 'RightBracket' &&
      token.kind !== 'Semicolon' &&
      token.kind !== 'DecimalInteger' &&
      token.kind !== 'Pipe'
    )
      return false
    index += 1
  }
  return false
}

const parseIntegerLiteralExpression = (initial: State): NodeResult => {
  if (nextSignificantKind(initial) === 'Minus') {
    const minus = expect(initial, 'Minus', ['DecimalInteger', ...expressionFollowing])
    const integer = expect(minus.state, 'DecimalInteger', expressionFollowing)
    return Object.freeze({
      state: integer.state,
      node: syntaxNode(integer.state, 'IntegerLiteralExpression', [
        ...minus.elements,
        ...integer.elements,
      ]),
    })
  }
  const integer = expect(initial, 'DecimalInteger', expressionFollowing)
  return Object.freeze({
    state: integer.state,
    node: syntaxNode(integer.state, 'IntegerLiteralExpression', integer.elements),
  })
}

const parseIdentifierExpression = (initial: State): NodeResult => {
  const identifier = expect(initial, 'Identifier', expressionFollowing)
  return Object.freeze({
    state: identifier.state,
    node: syntaxNode(identifier.state, 'IdentifierExpression', identifier.elements),
  })
}

const parseBooleanLiteralExpression = (initial: State): NodeResult => {
  const keyword = expect(
    initial,
    nextSignificantKind(initial) === 'FalseKeyword' ? 'FalseKeyword' : 'TrueKeyword',
    expressionFollowing,
  )
  return Object.freeze({
    state: keyword.state,
    node: syntaxNode(keyword.state, 'BooleanLiteralExpression', keyword.elements),
  })
}

const primaryKind = (
  state: State,
  recoveryKind: 'Integer' | 'Identifier',
  allowStructLiteral: boolean,
):
  | 'Integer'
  | 'Boolean'
  | 'Identifier'
  | 'Move'
  | 'Call'
  | 'StructLiteral'
  | 'ArrayLiteral'
  | 'Match'
  | 'Grouped'
  | 'Prefix'
  | 'ReservedTemplate' => {
  if (reservedTemplateStart(state)) return 'ReservedTemplate'
  let index = state.index
  let token = state.lexical.tokens.at(index)

  while (token !== undefined) {
    if (token.kind === 'DecimalInteger') return 'Integer'
    if (token.kind === 'Minus')
      return significantKindAfter(state, 1) === 'DecimalInteger' ? 'Integer' : 'Prefix'
    if (token.kind === 'Bang') return 'Prefix'
    if (token.kind === 'TrueKeyword' || token.kind === 'FalseKeyword') return 'Boolean'
    if (token.kind === 'MoveKeyword') return 'Move'
    if (token.kind === 'MatchKeyword') return 'Match'
    if (token.kind === 'LeftBracket') return 'ArrayLiteral'
    if (token.kind === 'Identifier') {
      const following = significantKindAfter(state, 1)
      if (hasCompleteAppliedPostfix(state, 'LeftParenthesis')) return 'Call'
      if (hasCompleteAppliedPostfix(state, 'LeftBrace')) {
        return allowStructLiteral ? 'StructLiteral' : 'Identifier'
      }
      if (following === 'LeftParenthesis') return 'Call'
      if (following === 'LeftBrace') return allowStructLiteral ? 'StructLiteral' : 'Identifier'
      if (following === 'Dot') {
        const member = significantKindAfter(state, 2)
        if (member === 'LeftParenthesis') return 'Call'
        const afterMember = significantKindAfter(state, 3)
        if (afterMember === 'LeftParenthesis') return 'Call'
        if (afterMember === 'LeftBrace') return allowStructLiteral ? 'StructLiteral' : 'Identifier'
      }
      return 'Identifier'
    }
    if (token.kind === 'LeftBrace') return allowStructLiteral ? 'StructLiteral' : recoveryKind
    if (token.kind === 'LeftParenthesis')
      return significantKindAfter(state, 1) === 'RightParenthesis' ? 'Call' : 'Grouped'
    if (
      token.kind === 'Comma' ||
      token.kind === 'RightParenthesis' ||
      token.kind === 'RightBracket' ||
      token.kind === 'RightBrace' ||
      token.kind === 'FatArrow' ||
      token.kind === 'LetKeyword' ||
      token.kind === 'IfKeyword' ||
      token.kind === 'ReturnKeyword' ||
      token.kind === 'PubKeyword' ||
      token.kind === 'StructKeyword' ||
      token.kind === 'FnKeyword' ||
      token.kind === 'ImportKeyword' ||
      token.kind === 'EndOfFile'
    ) {
      return recoveryKind
    }
    index += 1
    token = state.lexical.tokens.at(index)
  }

  return recoveryKind
}

const remainingRightParentheses = (state: State): number => {
  let count = 0
  for (let index = state.index; index < state.lexical.tokens.length; index += 1) {
    const token = state.lexical.tokens.at(index)
    if (token === undefined) break
    if (token.kind === 'RightParenthesis') count += 1
    if (
      token.kind === 'RightBrace' ||
      token.kind === 'PubKeyword' ||
      token.kind === 'StructKeyword' ||
      token.kind === 'FnKeyword' ||
      token.kind === 'ImportKeyword' ||
      token.kind === 'EndOfFile'
    ) {
      break
    }
  }
  return count
}

const expectCallRightParenthesis = (
  initial: State,
  reservedForEnclosingCalls: number,
): ElementsResult => {
  const leading = consumeTrivia(initial)
  const token = currentToken(leading.state)
  if (
    token?.kind === 'RightParenthesis' &&
    remainingRightParentheses(leading.state) <= reservedForEnclosingCalls
  ) {
    const missing = missingToken(leading.state, 'RightParenthesis')
    return Object.freeze({
      state: addDiagnostic(
        leading.state,
        Diagnostic.missingToken('RightParenthesis', missing.span),
      ),
      elements: Object.freeze([...leading.elements, missing]),
    })
  }
  return expect(initial, 'RightParenthesis', [
    'RightBrace',
    'PubKeyword',
    'StructKeyword',
    'FnKeyword',
    'ImportKeyword',
  ])
}

function parseArgumentList(initial: State, reservedForEnclosingCalls: number): NodeResult {
  const leftParenthesis = expect(initial, 'LeftParenthesis', [
    ...expressionStarts,
    'RightParenthesis',
    'RightBrace',
    'PubKeyword',
    'StructKeyword',
    'FnKeyword',
    'ImportKeyword',
  ])
  let state = leftParenthesis.state
  let children: ReadonlyArray<SyntaxTree.Element> = leftParenthesis.elements
  let kind = nextSignificantKind(state)

  while (
    kind !== undefined &&
    kind !== 'RightParenthesis' &&
    kind !== 'RightBrace' &&
    kind !== 'PubKeyword' &&
    kind !== 'StructKeyword' &&
    kind !== 'FnKeyword' &&
    kind !== 'ImportKeyword' &&
    kind !== 'EndOfFile'
  ) {
    const argument = parseExpression(state, reservedForEnclosingCalls + 1, 'Identifier')
    children = Object.freeze([...children, argument.node])
    state = argument.state
    kind = nextSignificantKind(state)

    if (
      kind === 'RightParenthesis' ||
      kind === 'RightBrace' ||
      kind === 'PubKeyword' ||
      kind === 'StructKeyword' ||
      kind === 'FnKeyword' ||
      kind === 'ImportKeyword'
    )
      break

    const comma = expect(state, 'Comma', [
      ...expressionStarts,
      'RightParenthesis',
      'RightBrace',
      'PubKeyword',
      'StructKeyword',
      'FnKeyword',
      'ImportKeyword',
    ])
    children = Object.freeze([...children, ...comma.elements])
    state = comma.state
    kind = nextSignificantKind(state)
  }

  const rightParenthesis = expectCallRightParenthesis(state, reservedForEnclosingCalls)
  return Object.freeze({
    state: rightParenthesis.state,
    node: syntaxNode(rightParenthesis.state, 'ArgumentList', [
      ...children,
      ...rightParenthesis.elements,
    ]),
  })
}

function parseCallExpression(initial: State, reservedForEnclosingCalls: number): NodeResult {
  const callee = expect(initial, 'Identifier', [
    'LeftParenthesis',
    'Dot',
    'RightParenthesis',
    'RightBrace',
    'PubKeyword',
    'StructKeyword',
    'FnKeyword',
    'ImportKeyword',
  ])
  let state = callee.state
  let elements: ReadonlyArray<SyntaxTree.Element> = callee.elements
  if (nextSignificantKind(state) === 'Dot') {
    const dot = expect(state, 'Dot', ['Identifier', 'LeftParenthesis'])
    const operation = expect(dot.state, 'Identifier', ['LeftParenthesis'])
    state = operation.state
    elements = Object.freeze([...elements, ...dot.elements, ...operation.elements])
  }
  if (nextSignificantKind(state) === 'Less') {
    const arguments_ = parseTypeArgumentList(state, 'CallTypeArgumentList', ['LeftParenthesis'])
    state = arguments_.state
    elements = Object.freeze([...elements, arguments_.node])
  }
  const argumentsList = parseArgumentList(state, reservedForEnclosingCalls)
  return Object.freeze({
    state: argumentsList.state,
    node: syntaxNode(argumentsList.state, 'CallExpression', [...elements, argumentsList.node]),
  })
}

function parseStructLiteralExpression(
  initial: State,
  reservedForEnclosingCalls: number,
): NodeResult {
  const first = expect(initial, 'Identifier', ['Dot', 'Less', 'LeftBrace', ...expressionFollowing])
  let state = first.state
  let targetChildren: ReadonlyArray<SyntaxTree.Element> = first.elements
  if (nextSignificantKind(state) === 'Dot') {
    const dot = expect(state, 'Dot', ['Identifier', 'Less', 'LeftBrace'])
    const second = expect(dot.state, 'Identifier', ['Less', 'LeftBrace'])
    state = second.state
    targetChildren = Object.freeze([...targetChildren, ...dot.elements, ...second.elements])
  }
  const path = syntaxNode(state, 'TypePath', targetChildren)
  const applied =
    nextSignificantKind(state) === 'Less'
      ? parseTypeArgumentList(state, 'TypeArgumentList', ['LeftBrace'])
      : undefined
  if (applied !== undefined) state = applied.state
  const target =
    applied === undefined ? path : syntaxNode(applied.state, 'AppliedType', [path, applied.node])
  const leftBrace = expect(state, 'LeftBrace', ['Identifier', 'RightBrace', ...expressionFollowing])
  state = leftBrace.state
  let children: ReadonlyArray<SyntaxTree.Element> = Object.freeze([target, ...leftBrace.elements])
  let kind = nextSignificantKind(state)

  while (
    kind !== undefined &&
    kind !== 'RightBrace' &&
    kind !== 'LetKeyword' &&
    kind !== 'IfKeyword' &&
    kind !== 'ReturnKeyword' &&
    kind !== 'PubKeyword' &&
    kind !== 'StructKeyword' &&
    kind !== 'FnKeyword' &&
    kind !== 'ImportKeyword' &&
    kind !== 'EndOfFile'
  ) {
    const field = expect(state, 'Identifier', ['Colon', ...expressionStarts, 'RightBrace'])
    const colon = expect(field.state, 'Colon', [...expressionStarts, 'Comma', 'RightBrace'])
    const value = parseExpression(colon.state, reservedForEnclosingCalls, 'Identifier')
    const initializer = syntaxNode(value.state, 'StructFieldInitializer', [
      ...field.elements,
      ...colon.elements,
      value.node,
    ])
    children = Object.freeze([...children, initializer])
    state = value.state
    kind = nextSignificantKind(state)
    if (kind === 'RightBrace') break
    const comma = expect(state, 'Comma', [...expressionStarts, 'RightBrace'])
    children = Object.freeze([...children, ...comma.elements])
    state = comma.state
    kind = nextSignificantKind(state)
    if (kind === 'RightBrace') break
  }

  const rightBrace = expect(state, 'RightBrace', expressionFollowing)
  return Object.freeze({
    state: rightBrace.state,
    node: syntaxNode(rightBrace.state, 'StructLiteralExpression', [
      ...children,
      ...rightBrace.elements,
    ]),
  })
}

function parseGroupedExpression(initial: State, reservedForEnclosingCalls: number): NodeResult {
  const left = expect(initial, 'LeftParenthesis', [...expressionStarts, 'RightParenthesis'])
  const expression = parseExpression(left.state, reservedForEnclosingCalls + 1, 'Identifier')
  const right = expectCallRightParenthesis(expression.state, reservedForEnclosingCalls)
  return Object.freeze({
    state: right.state,
    node: syntaxNode(right.state, 'GroupedExpression', [
      ...left.elements,
      expression.node,
      ...right.elements,
    ]),
  })
}

function parseArrayLiteralExpression(
  initial: State,
  reservedForEnclosingCalls: number,
): NodeResult {
  const left = expect(initial, 'LeftBracket', [...expressionStarts, 'RightBracket'])
  let state = left.state
  let children: ReadonlyArray<SyntaxTree.Element> = left.elements
  let first = true
  while (
    nextSignificantKind(state) !== 'RightBracket' &&
    nextSignificantKind(state) !== 'RightBrace' &&
    nextSignificantKind(state) !== 'EndOfFile'
  ) {
    if (!first) {
      const comma = expect(state, 'Comma', [...expressionStarts, 'RightBracket'])
      children = Object.freeze([...children, ...comma.elements])
      state = comma.state
      if (nextSignificantKind(state) === 'RightBracket') break
    }
    const element = parseExpression(state, reservedForEnclosingCalls, 'Identifier')
    children = Object.freeze([...children, element.node])
    state = element.state
    first = false
  }
  const right = expect(state, 'RightBracket', expressionFollowing)
  return Object.freeze({
    state: right.state,
    node: syntaxNode(right.state, 'ArrayLiteralExpression', [...children, ...right.elements]),
  })
}

const significantToken = (state: State): Token.Token | undefined => {
  let index = state.index
  let token = state.lexical.tokens.at(index)
  while (token !== undefined && isTrivia(token.kind)) {
    index += 1
    token = state.lexical.tokens.at(index)
  }
  return token
}

const isUniversalPatternStart = (state: State): boolean => {
  const token = significantToken(state)
  return (
    token?.kind === 'Identifier' &&
    Option.contains(SourceFile.spelling(state.lexical.source, token.span), '_')
  )
}

const isNominalPatternStart = (state: State): boolean => {
  if (nextSignificantKind(state) !== 'Identifier') return false
  if (hasCompleteAppliedPostfix(state, 'LeftBrace')) return true
  const following = significantKindAfter(state, 1)
  if (following === 'LeftBrace') return true
  return following === 'Dot' && significantKindAfter(state, 3) === 'LeftBrace'
}

function parsePattern(initial: State): NodeResult {
  if (isUniversalPatternStart(initial)) {
    const identifier = expect(initial, 'Identifier', ['IfKeyword', 'FatArrow', 'RightBrace'])
    return Object.freeze({
      state: identifier.state,
      node: syntaxNode(identifier.state, 'UniversalPattern', identifier.elements),
    })
  }
  return parseNominalPattern(initial)
}

function parseNominalPattern(initial: State): NodeResult {
  const target = parseTypePrimary(initial, ['LeftBrace', 'IfKeyword', 'FatArrow', 'RightBrace'])
  const left = expect(target.state, 'LeftBrace', [
    'Identifier',
    'DotDot',
    'RightBrace',
    'IfKeyword',
    'FatArrow',
  ])
  let state = left.state
  let children: ReadonlyArray<SyntaxTree.Element> = Object.freeze([target.node, ...left.elements])

  while (
    nextSignificantKind(state) !== 'RightBrace' &&
    nextSignificantKind(state) !== 'IfKeyword' &&
    nextSignificantKind(state) !== 'FatArrow' &&
    nextSignificantKind(state) !== 'EndOfFile'
  ) {
    if (nextSignificantKind(state) === 'DotDot') {
      const rest = expect(state, 'DotDot', ['Comma', 'RightBrace', 'IfKeyword', 'FatArrow'])
      children = Object.freeze([...children, syntaxNode(rest.state, 'RestPattern', rest.elements)])
      state = rest.state
    } else {
      const name = expect(state, 'Identifier', [
        'Colon',
        'Comma',
        'RightBrace',
        'IfKeyword',
        'FatArrow',
      ])
      state = name.state
      let fieldChildren: ReadonlyArray<SyntaxTree.Element> = name.elements
      if (nextSignificantKind(state) === 'Colon' || isNominalPatternStart(state)) {
        const colon = expect(state, 'Colon', [
          'Identifier',
          'Comma',
          'RightBrace',
          'IfKeyword',
          'FatArrow',
        ])
        state = colon.state
        if (isNominalPatternStart(state)) {
          const nested = parseNominalPattern(state)
          fieldChildren = Object.freeze([...fieldChildren, ...colon.elements, nested.node])
          state = nested.state
        } else {
          const binding = expect(state, 'Identifier', [
            'Comma',
            'RightBrace',
            'IfKeyword',
            'FatArrow',
          ])
          fieldChildren = Object.freeze([...fieldChildren, ...colon.elements, ...binding.elements])
          state = binding.state
        }
      }
      children = Object.freeze([...children, syntaxNode(state, 'PatternField', fieldChildren)])
    }

    if (nextSignificantKind(state) === 'RightBrace') break
    const comma = expect(state, 'Comma', [
      'Identifier',
      'DotDot',
      'RightBrace',
      'IfKeyword',
      'FatArrow',
    ])
    children = Object.freeze([...children, ...comma.elements])
    state = comma.state
  }

  const right = expect(state, 'RightBrace', ['IfKeyword', 'FatArrow', 'Identifier', 'RightBrace'])
  return Object.freeze({
    state: right.state,
    node: syntaxNode(right.state, 'NominalPattern', [...children, ...right.elements]),
  })
}

function parseMatchArm(initial: State, reservedForEnclosingCalls: number): NodeResult {
  const pattern = parsePattern(initial)
  let state = pattern.state
  let children: ReadonlyArray<SyntaxTree.Element> = Object.freeze([pattern.node])

  if (nextSignificantKind(state) === 'IfKeyword') {
    const keyword = expect(state, 'IfKeyword', [...expressionStarts, 'FatArrow'])
    const guard = parseExpression(keyword.state, reservedForEnclosingCalls, 'Identifier', false)
    children = Object.freeze([...children, ...keyword.elements, guard.node])
    state = guard.state
  }

  const arrow = expect(state, 'FatArrow', [...expressionStarts, 'Identifier', 'RightBrace'])
  const result = parseExpression(arrow.state, reservedForEnclosingCalls, 'Identifier')
  state = result.state
  children = Object.freeze([...children, ...arrow.elements, result.node])
  return Object.freeze({ state, node: syntaxNode(state, 'MatchArm', children) })
}

function parseMatchExpression(initial: State, reservedForEnclosingCalls: number): NodeResult {
  const keyword = expect(initial, 'MatchKeyword', ['MoveKeyword', 'Ampersand', ...expressionStarts])
  let state = keyword.state
  let accessChildren: ReadonlyArray<SyntaxTree.Element> = Object.freeze([])
  if (nextSignificantKind(state) === 'MoveKeyword') {
    const move = expect(state, 'MoveKeyword', expressionStarts)
    state = move.state
    accessChildren = move.elements
  } else if (nextSignificantKind(state) === 'Ampersand') {
    const ampersand = expect(state, 'Ampersand', ['MutKeyword', ...expressionStarts])
    state = ampersand.state
    accessChildren = ampersand.elements
    if (nextSignificantKind(state) === 'MutKeyword') {
      const mut = expect(state, 'MutKeyword', expressionStarts)
      state = mut.state
      accessChildren = Object.freeze([...accessChildren, ...mut.elements])
    }
  }
  const access = syntaxNode(state, 'MatchAccess', accessChildren)
  const scrutinee = parseExpression(state, reservedForEnclosingCalls, 'Identifier', false)
  const left = expect(scrutinee.state, 'LeftBrace', ['Identifier', 'RightBrace'])
  state = left.state
  let children: ReadonlyArray<SyntaxTree.Element> = Object.freeze([
    ...keyword.elements,
    access,
    scrutinee.node,
    ...left.elements,
  ])

  while (
    nextSignificantKind(state) !== 'RightBrace' &&
    nextSignificantKind(state) !== 'EndOfFile'
  ) {
    const arm = parseMatchArm(state, reservedForEnclosingCalls)
    children = Object.freeze([...children, arm.node])
    state = arm.state
  }

  const right = expect(state, 'RightBrace', expressionFollowing)
  return Object.freeze({
    state: right.state,
    node: syntaxNode(right.state, 'MatchExpression', [...children, ...right.elements]),
  })
}

const reservedTemplateBoundaries: ReadonlyArray<Token.TokenKind> = Object.freeze([
  'Comma',
  'RightParenthesis',
  'RightBracket',
  'RightBrace',
  'LetKeyword',
  'IfKeyword',
  'WhileKeyword',
  'BreakKeyword',
  'ContinueKeyword',
  'ReturnKeyword',
  'ElseKeyword',
  'PubKeyword',
  'StructKeyword',
  'FnKeyword',
  'ImportKeyword',
  'EndOfFile',
])

const parseReservedTemplateExpression = (initial: State): NodeResult => {
  const leading = consumeTrivia(initial)
  let state = leading.state
  let elements: ReadonlyArray<SyntaxTree.Element> = leading.elements
  let token = currentToken(state)
  const start = token?.span.start ?? insertionOffset(state)
  let braceDepth = 0
  let bracketDepth = 0
  let parenthesisDepth = 0

  while (token !== undefined) {
    const nested = braceDepth > 0 || bracketDepth > 0 || parenthesisDepth > 0
    if (elements.length > leading.elements.length && !nested) {
      if (reservedTemplateBoundaries.includes(token.kind)) break
    }

    elements = Object.freeze([...elements, token])
    state = advance(state)

    if (token.kind === 'LeftBrace') braceDepth += 1
    else if (token.kind === 'RightBrace' && braceDepth > 0) braceDepth -= 1
    else if (token.kind === 'LeftBracket') bracketDepth += 1
    else if (token.kind === 'RightBracket' && bracketDepth > 0) bracketDepth -= 1
    else if (token.kind === 'LeftParenthesis') parenthesisDepth += 1
    else if (token.kind === 'RightParenthesis' && parenthesisDepth > 0) parenthesisDepth -= 1

    if (
      token.kind === 'Greater' &&
      braceDepth === 0 &&
      bracketDepth === 0 &&
      parenthesisDepth === 0
    ) {
      break
    }
    token = currentToken(state)
  }

  const node = syntaxNode(state, 'Error', elements)
  const end = elements.filter(SyntaxTree.isToken).at(-1)?.span.end ?? start
  const span = Option.getOrThrowWith(
    SourceSpan.make(state.lexical.source, start, end),
    () => new RangeError(`Parser produced an invalid reserved template span [${start}, ${end})`),
  )
  return Object.freeze({
    state: addDiagnostic(state, Diagnostic.reservedTemplateSyntax(span)),
    node,
  })
}

function parsePrimaryExpression(
  initial: State,
  reservedForEnclosingCalls: number,
  recoveryKind: 'Integer' | 'Identifier',
  allowStructLiteral: boolean,
): NodeResult {
  const kind = primaryKind(initial, recoveryKind, allowStructLiteral)
  if (kind === 'ReservedTemplate') return parseReservedTemplateExpression(initial)
  if (kind === 'Call') return parseCallExpression(initial, reservedForEnclosingCalls)
  if (kind === 'StructLiteral')
    return parseStructLiteralExpression(initial, reservedForEnclosingCalls)
  if (kind === 'ArrayLiteral')
    return parseArrayLiteralExpression(initial, reservedForEnclosingCalls)
  if (kind === 'Match') return parseMatchExpression(initial, reservedForEnclosingCalls)
  if (kind === 'Move') {
    const keyword = expect(initial, 'MoveKeyword', ['Identifier', ...expressionFollowing])
    const subject = parseIdentifierExpression(keyword.state)
    const projected = parseProjectionChain(subject)
    return Object.freeze({
      state: projected.state,
      node: syntaxNode(projected.state, 'MoveExpression', [...keyword.elements, projected.node]),
    })
  }
  if (kind === 'Boolean') return parseBooleanLiteralExpression(initial)
  if (kind === 'Identifier') return parseIdentifierExpression(initial)
  if (kind === 'Grouped') return parseGroupedExpression(initial, reservedForEnclosingCalls)
  return parseIntegerLiteralExpression(initial)
}

function parseProjectionChain(initial: NodeResult): NodeResult {
  let result = initial
  while (
    nextSignificantKind(result.state) === 'Dot' ||
    nextSignificantKind(result.state) === 'LeftBracket'
  ) {
    if (nextSignificantKind(result.state) === 'Dot') {
      const dot = expect(result.state, 'Dot', ['Identifier', ...expressionFollowing])
      const field = expect(dot.state, 'Identifier', expressionFollowing)
      result = Object.freeze({
        state: field.state,
        node: syntaxNode(field.state, 'FieldProjectionExpression', [
          result.node,
          ...dot.elements,
          ...field.elements,
        ]),
      })
      continue
    }
    const left = expect(result.state, 'LeftBracket', [...expressionStarts, 'RightBracket'])
    const index = parseExpression(left.state, 0, 'Identifier')
    const right = expect(index.state, 'RightBracket', [
      'Dot',
      'LeftBracket',
      ...expressionFollowing,
    ])
    result = Object.freeze({
      state: right.state,
      node: syntaxNode(right.state, 'IndexProjectionExpression', [
        result.node,
        ...left.elements,
        index.node,
        ...right.elements,
      ]),
    })
  }
  return result
}

function parsePrefixExpression(
  initial: State,
  reservedForEnclosingCalls: number,
  recoveryKind: 'Integer' | 'Identifier',
  allowStructLiteral: boolean,
): NodeResult {
  const kind = primaryKind(initial, recoveryKind, allowStructLiteral)
  if (kind !== 'Prefix')
    return parseProjectionChain(
      parsePrimaryExpression(initial, reservedForEnclosingCalls, recoveryKind, allowStructLiteral),
    )
  const tokenKind = nextSignificantKind(initial) === 'Bang' ? 'Bang' : 'Minus'
  const operator = expect(initial, tokenKind, [...expressionStarts, ...expressionFollowing])
  const operand = parsePrefixExpression(
    operator.state,
    reservedForEnclosingCalls,
    recoveryKind,
    allowStructLiteral,
  )
  return Object.freeze({
    state: operand.state,
    node: syntaxNode(operand.state, 'PrefixExpression', [...operator.elements, operand.node]),
  })
}

function parseInfixExpression(
  initial: State,
  reservedForEnclosingCalls: number,
  recoveryKind: 'Integer' | 'Identifier',
  minimumPrecedence: number,
  allowStructLiteral: boolean,
): NodeResult {
  let left = parsePrefixExpression(
    initial,
    reservedForEnclosingCalls,
    recoveryKind,
    allowStructLiteral,
  )
  let nonAssociativePrecedence: number | undefined

  for (;;) {
    const kind = nextSignificantKind(left.state)
    if (kind === undefined) break
    const info = Operator.infix(kind)
    if (info === undefined || info.precedence < minimumPrecedence) break
    if (info.associativity === 'None' && nonAssociativePrecedence === info.precedence) break

    const operator = expect(left.state, kind, [...expressionStarts, ...expressionFollowing])
    const right = parseInfixExpression(
      operator.state,
      reservedForEnclosingCalls,
      recoveryKind,
      info.precedence + 1,
      allowStructLiteral,
    )
    left = Object.freeze({
      state: right.state,
      node: syntaxNode(right.state, 'InfixExpression', [
        left.node,
        ...operator.elements,
        right.node,
      ]),
    })
    if (info.associativity === 'None') nonAssociativePrecedence = info.precedence
  }

  return left
}

function parsePipelineTarget(initial: State, reservedForEnclosingCalls: number): NodeResult {
  const qualifier = expect(initial, 'Identifier', [
    'Dot',
    'LeftParenthesis',
    ...expressionFollowing,
  ])
  const dot = expect(qualifier.state, 'Dot', [
    'Identifier',
    'LeftParenthesis',
    ...expressionFollowing,
  ])
  const member = expect(dot.state, 'Identifier', [
    'LeftParenthesis',
    'PipeGreater',
    ...expressionFollowing,
  ])
  let state = member.state
  let children: ReadonlyArray<SyntaxTree.Element> = Object.freeze([
    ...qualifier.elements,
    ...dot.elements,
    ...member.elements,
  ])
  if (nextSignificantKind(state) === 'LeftParenthesis') {
    const argumentsList = parseArgumentList(state, reservedForEnclosingCalls)
    state = argumentsList.state
    children = Object.freeze([...children, argumentsList.node])
  }
  return Object.freeze({ state, node: syntaxNode(state, 'PipelineTarget', children) })
}

function parseExpression(
  initial: State,
  reservedForEnclosingCalls: number,
  recoveryKind: 'Integer' | 'Identifier',
  allowStructLiteral = true,
): NodeResult {
  let left = parseInfixExpression(
    initial,
    reservedForEnclosingCalls,
    recoveryKind,
    0,
    allowStructLiteral,
  )
  while (nextSignificantKind(left.state) === 'PipeGreater') {
    const pipe = expect(left.state, 'PipeGreater', ['Identifier', ...expressionFollowing])
    const target = parsePipelineTarget(pipe.state, reservedForEnclosingCalls)
    left = Object.freeze({
      state: target.state,
      node: syntaxNode(target.state, 'PipelineExpression', [
        left.node,
        ...pipe.elements,
        target.node,
      ]),
    })
  }
  return left
}

const parseReturnStatement = (initial: State): NodeResult => {
  const keyword = expect(initial, 'ReturnKeyword', [...expressionStarts, 'RightBrace'])
  const expression = parseExpression(keyword.state, 0, 'Integer')
  return Object.freeze({
    state: expression.state,
    node: syntaxNode(expression.state, 'ReturnStatement', [...keyword.elements, expression.node]),
  })
}

const parseBindingStatement = (initial: State): NodeResult => {
  const keyword = expect(initial, 'LetKeyword', [
    'MutKeyword',
    'Identifier',
    'Equals',
    'ReturnKeyword',
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

const parseAssignmentStatement = (initial: State): NodeResult => {
  const place = parseProjectionChain(parseIdentifierExpression(initial))
  const equals = expect(place.state, 'Equals', [
    ...expressionStarts,
    'LetKeyword',
    'IfKeyword',
    'WhileKeyword',
    'BreakKeyword',
    'ContinueKeyword',
    'ReturnKeyword',
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

function parseConditionalStatement(initial: State): NodeResult {
  const keyword = expect(initial, 'IfKeyword', [...expressionStarts, 'LeftBrace', 'RightBrace'])
  const condition = parseExpression(keyword.state, 0, 'Identifier', false)
  const taken = parseBlock(condition.state, false)
  let state = taken.state
  let children: ReadonlyArray<SyntaxTree.Element> = Object.freeze([
    ...keyword.elements,
    condition.node,
    taken.node,
  ])

  if (nextSignificantKind(state) === 'ElseKeyword') {
    const elseKeyword = expect(state, 'ElseKeyword', ['LeftBrace'])
    const otherwise = parseBlock(elseKeyword.state, false)
    state = otherwise.state
    children = Object.freeze([...children, ...elseKeyword.elements, otherwise.node])
  }

  return Object.freeze({
    state,
    node: syntaxNode(state, 'ConditionalStatement', children),
  })
}

function parseWhileStatement(initial: State): NodeResult {
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

const parseTransferStatement = (
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

function parseBlock(initial: State, requireReturn: boolean): NodeResult {
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
  let sawReturn = false

  let kind = nextSignificantKind(state)
  while (
    kind === 'LetKeyword' ||
    kind === 'ReturnKeyword' ||
    kind === 'IfKeyword' ||
    kind === 'WhileKeyword' ||
    kind === 'BreakKeyword' ||
    kind === 'ContinueKeyword' ||
    (kind === 'Identifier' && significantKindAfter(state, 1) !== 'Colon')
  ) {
    const statement =
      kind === 'LetKeyword'
        ? parseBindingStatement(state)
        : kind === 'IfKeyword'
          ? parseConditionalStatement(state)
          : kind === 'WhileKeyword'
            ? parseWhileStatement(state)
            : kind === 'BreakKeyword'
              ? parseTransferStatement(state, 'BreakKeyword', 'BreakStatement')
              : kind === 'ContinueKeyword'
                ? parseTransferStatement(state, 'ContinueKeyword', 'ContinueStatement')
                : kind === 'Identifier'
                  ? parseAssignmentStatement(state)
                  : parseReturnStatement(state)
    if (kind === 'ReturnKeyword') sawReturn = true
    children = Object.freeze([...children, statement.node])
    state = statement.state
    kind = nextSignificantKind(state)
  }

  if (requireReturn && !sawReturn) {
    const statement = parseReturnStatement(state)
    children = Object.freeze([...children, statement.node])
    state = statement.state
  }

  const rightBrace = expect(state, 'RightBrace', [
    'LetKeyword',
    'IfKeyword',
    'WhileKeyword',
    'BreakKeyword',
    'ContinueKeyword',
    'ReturnKeyword',
    'ElseKeyword',
    'PubKeyword',
    'StructKeyword',
    'FnKeyword',
    'ImportKeyword',
  ])
  return Object.freeze({
    state: rightBrace.state,
    node: syntaxNode(rightBrace.state, 'Block', [...children, ...rightBrace.elements]),
  })
}

const parseTypePath = (
  initial: State,
  following: ReadonlyArray<Token.TokenKind>,
  preserveFieldStart = false,
): NodeResult => {
  const fieldStartsHere =
    preserveFieldStart &&
    nextSignificantKind(initial) === 'Identifier' &&
    significantKindAfter(initial, 1) === 'Colon'
  const first = fieldStartsHere
    ? (() => {
        const leading = consumeTrivia(initial)
        const missing = missingToken(leading.state, 'Identifier')
        return Object.freeze({
          state: addDiagnostic(leading.state, Diagnostic.missingToken('Identifier', missing.span)),
          elements: Object.freeze([...leading.elements, missing]),
        })
      })()
    : expect(initial, 'Identifier', ['Dot', ...following])
  let state = first.state
  let children: ReadonlyArray<SyntaxTree.Element> = first.elements
  if (!fieldStartsHere && nextSignificantKind(state) === 'Dot') {
    const dot = expect(state, 'Dot', ['Identifier', ...following])
    const member = expect(dot.state, 'Identifier', following)
    state = member.state
    children = Object.freeze([...children, ...dot.elements, ...member.elements])
  }
  return Object.freeze({ state, node: syntaxNode(state, 'TypePath', children) })
}

const parseTypeArgumentList = (
  initial: State,
  kind: 'TypeArgumentList' | 'CallTypeArgumentList',
  following: ReadonlyArray<Token.TokenKind>,
): NodeResult => {
  const left = expect(initial, 'Less', [...typeStarts, 'Greater', ...following])
  let state = left.state
  let children: ReadonlyArray<SyntaxTree.Element> = left.elements
  if (nextSignificantKind(state) === 'Greater') {
    const missing = missingToken(state, 'Identifier')
    state = addDiagnostic(state, Diagnostic.missingToken('Identifier', missing.span))
    children = Object.freeze([...children, missing])
  }
  while (
    nextSignificantKind(state) !== 'Greater' &&
    nextSignificantKind(state) !== 'EndOfFile' &&
    !following.includes(nextSignificantKind(state) ?? 'EndOfFile')
  ) {
    const argument = parseType(state, ['Comma', 'Greater', ...following])
    children = Object.freeze([...children, argument.node])
    state = argument.state
    if (nextSignificantKind(state) !== 'Comma') break
    const comma = expect(state, 'Comma', [...typeStarts, 'Greater', ...following])
    children = Object.freeze([...children, ...comma.elements])
    state = comma.state
  }
  const right = expect(state, 'Greater', following)
  return Object.freeze({
    state: right.state,
    node: syntaxNode(right.state, kind, [...children, ...right.elements]),
  })
}

const parseTypeParameterList = (
  initial: State,
  following: ReadonlyArray<Token.TokenKind>,
): NodeResult => {
  const left = expect(initial, 'Less', ['Identifier', 'Greater', ...following])
  let state = left.state
  let children: ReadonlyArray<SyntaxTree.Element> = left.elements
  if (nextSignificantKind(state) === 'Greater') {
    const missing = missingToken(state, 'Identifier')
    state = addDiagnostic(state, Diagnostic.missingToken('Identifier', missing.span))
    children = Object.freeze([...children, missing])
  }
  while (
    nextSignificantKind(state) !== 'Greater' &&
    nextSignificantKind(state) !== 'EndOfFile' &&
    !following.includes(nextSignificantKind(state) ?? 'EndOfFile')
  ) {
    const name = expect(state, 'Identifier', ['Comma', 'Greater', ...following])
    children = Object.freeze([...children, syntaxNode(name.state, 'TypeParameter', name.elements)])
    state = name.state
    if (nextSignificantKind(state) !== 'Comma') break
    const comma = expect(state, 'Comma', ['Identifier', 'Greater', ...following])
    children = Object.freeze([...children, ...comma.elements])
    state = comma.state
  }
  const right = expect(state, 'Greater', following)
  return Object.freeze({
    state: right.state,
    node: syntaxNode(right.state, 'TypeParameterList', [...children, ...right.elements]),
  })
}

const parseTypePrimary = (
  initial: State,
  following: ReadonlyArray<Token.TokenKind>,
  preserveFieldStart = false,
): NodeResult => {
  if (nextSignificantKind(initial) === 'LeftParenthesis') {
    const left = expect(initial, 'LeftParenthesis', [
      ...typeStarts,
      'RightParenthesis',
      ...following,
    ])
    const type = parseType(left.state, ['RightParenthesis', ...following])
    const right = expect(type.state, 'RightParenthesis', following)
    return Object.freeze({
      state: right.state,
      node: syntaxNode(right.state, 'ParenthesizedType', [
        ...left.elements,
        type.node,
        ...right.elements,
      ]),
    })
  }
  if (nextSignificantKind(initial) !== 'LeftBracket') {
    const path = parseTypePath(initial, ['Less', ...following], preserveFieldStart)
    if (nextSignificantKind(path.state) !== 'Less') return path
    const arguments_ = parseTypeArgumentList(path.state, 'TypeArgumentList', following)
    return Object.freeze({
      state: arguments_.state,
      node: syntaxNode(arguments_.state, 'AppliedType', [path.node, arguments_.node]),
    })
  }
  const left = expect(initial, 'LeftBracket', [...typeStarts, ...following])
  const element = parseType(left.state, ['Semicolon', 'RightBracket', ...following])
  const semicolon = expect(element.state, 'Semicolon', [
    'DecimalInteger',
    'RightBracket',
    ...following,
  ])
  const length = expect(semicolon.state, 'DecimalInteger', ['RightBracket', ...following])
  const right = expect(length.state, 'RightBracket', following)
  return Object.freeze({
    state: right.state,
    node: syntaxNode(right.state, 'FixedArrayType', [
      ...left.elements,
      element.node,
      ...semicolon.elements,
      ...length.elements,
      ...right.elements,
    ]),
  })
}

function parseType(
  initial: State,
  following: ReadonlyArray<Token.TokenKind>,
  preserveFieldStart = false,
): NodeResult {
  const first = parseTypePrimary(initial, ['Pipe', ...following], preserveFieldStart)
  let state = first.state
  let children: ReadonlyArray<SyntaxTree.Element> = Object.freeze([first.node])
  while (nextSignificantKind(state) === 'Pipe') {
    const pipe = expect(state, 'Pipe', [...typeStarts, ...following])
    const member = parseTypePrimary(pipe.state, ['Pipe', ...following], preserveFieldStart)
    children = Object.freeze([...children, ...pipe.elements, member.node])
    state = member.state
  }
  return children.length === 1
    ? first
    : Object.freeze({ state, node: syntaxNode(state, 'UnionType', children) })
}

const parseReturnType = (initial: State): NodeResult => {
  const arrow = expect(initial, 'Arrow', [...typeStarts, 'LeftBrace'])
  const type = parseType(arrow.state, ['LeftBrace'])
  return Object.freeze({
    state: type.state,
    node: syntaxNode(type.state, 'ReturnType', [...arrow.elements, type.node]),
  })
}

const parseParameterList = (initial: State): NodeResult => {
  const leftParenthesis = expect(initial, 'LeftParenthesis', [
    'Identifier',
    'RightParenthesis',
    'Arrow',
  ])
  let state = leftParenthesis.state
  let children: ReadonlyArray<SyntaxTree.Element> = leftParenthesis.elements
  let kind = nextSignificantKind(state)

  while (
    kind !== undefined &&
    kind !== 'RightParenthesis' &&
    kind !== 'Arrow' &&
    kind !== 'PubKeyword' &&
    kind !== 'StructKeyword' &&
    kind !== 'FnKeyword' &&
    kind !== 'ImportKeyword' &&
    kind !== 'EndOfFile'
  ) {
    const name = expect(state, 'Identifier', ['Colon', 'Comma', 'RightParenthesis', 'Arrow'])
    const colon = expect(name.state, 'Colon', [...typeStarts, 'Comma', 'RightParenthesis', 'Arrow'])
    const type = parseType(colon.state, ['Comma', 'RightParenthesis', 'Arrow'])
    const parameter = syntaxNode(type.state, 'ParameterDeclaration', [
      ...name.elements,
      ...colon.elements,
      type.node,
    ])
    children = Object.freeze([...children, parameter])
    state = type.state
    kind = nextSignificantKind(state)

    if (
      kind === 'RightParenthesis' ||
      kind === 'Arrow' ||
      kind === 'PubKeyword' ||
      kind === 'StructKeyword' ||
      kind === 'FnKeyword' ||
      kind === 'ImportKeyword'
    )
      break

    const comma = expect(state, 'Comma', [
      'Identifier',
      'RightParenthesis',
      'Arrow',
      'PubKeyword',
      'StructKeyword',
      'FnKeyword',
      'ImportKeyword',
    ])
    children = Object.freeze([...children, ...comma.elements])
    state = comma.state
    kind = nextSignificantKind(state)
  }

  const rightParenthesis = expect(state, 'RightParenthesis', [
    'Arrow',
    'PubKeyword',
    'StructKeyword',
    'FnKeyword',
    'ImportKeyword',
  ])
  return Object.freeze({
    state: rightParenthesis.state,
    node: syntaxNode(rightParenthesis.state, 'ParameterList', [
      ...children,
      ...rightParenthesis.elements,
    ]),
  })
}

const topLevelFollowing: ReadonlyArray<Token.TokenKind> = Object.freeze([
  'ImportKeyword',
  'PubKeyword',
  'StructKeyword',
  'FnKeyword',
  'EndOfFile',
])

const parseImportAlias = (initial: State): NodeResult => {
  const keyword = expect(initial, 'AsKeyword', ['Identifier', 'LeftBrace', ...topLevelFollowing])
  const name = expect(keyword.state, 'Identifier', ['LeftBrace', ...topLevelFollowing])
  return Object.freeze({
    state: name.state,
    node: syntaxNode(name.state, 'ImportAlias', [...keyword.elements, ...name.elements]),
  })
}

const parseImportPath = (initial: State): NodeResult => {
  const first = expect(initial, 'Identifier', [
    'Dot',
    'AsKeyword',
    'LeftBrace',
    ...topLevelFollowing,
  ])
  let state = first.state
  let children: ReadonlyArray<SyntaxTree.Element> = first.elements
  while (nextSignificantKind(state) === 'Dot') {
    const dot = expect(state, 'Dot', ['Identifier', 'AsKeyword', 'LeftBrace', ...topLevelFollowing])
    const segment = expect(dot.state, 'Identifier', [
      'Dot',
      'AsKeyword',
      'LeftBrace',
      ...topLevelFollowing,
    ])
    children = Object.freeze([...children, ...dot.elements, ...segment.elements])
    state = segment.state
  }
  return Object.freeze({ state, node: syntaxNode(state, 'ImportPath', children) })
}

const parseImportMember = (initial: State): NodeResult => {
  const name = expect(initial, 'Identifier', [
    'AsKeyword',
    'Comma',
    'RightBrace',
    ...topLevelFollowing,
  ])
  let state = name.state
  let children: ReadonlyArray<SyntaxTree.Element> = name.elements
  if (nextSignificantKind(state) === 'AsKeyword') {
    const alias = parseImportAlias(state)
    state = alias.state
    children = Object.freeze([...children, alias.node])
  }
  return Object.freeze({ state, node: syntaxNode(state, 'ImportMember', children) })
}

const parseImportMemberList = (initial: State): NodeResult => {
  const left = expect(initial, 'LeftBrace', ['Identifier', 'RightBrace', ...topLevelFollowing])
  let state = left.state
  let children: ReadonlyArray<SyntaxTree.Element> = left.elements
  let first = true
  while (
    nextSignificantKind(state) !== 'RightBrace' &&
    !topLevelFollowing.includes(nextSignificantKind(state) ?? 'EndOfFile')
  ) {
    if (!first) {
      const comma = expect(state, 'Comma', ['Identifier', 'RightBrace', ...topLevelFollowing])
      children = Object.freeze([...children, ...comma.elements])
      state = comma.state
      if (nextSignificantKind(state) === 'RightBrace') break
    }
    const member = parseImportMember(state)
    children = Object.freeze([...children, member.node])
    state = member.state
    first = false
  }
  if (first) {
    const member = parseImportMember(state)
    children = Object.freeze([...children, member.node])
    state = member.state
  }
  const right = expect(state, 'RightBrace', topLevelFollowing)
  return Object.freeze({
    state: right.state,
    node: syntaxNode(right.state, 'ImportMemberList', [...children, ...right.elements]),
  })
}

const parseImportDeclaration = (initial: State): NodeResult => {
  const keyword = expect(initial, 'ImportKeyword', ['Identifier', ...topLevelFollowing])
  const path = parseImportPath(keyword.state)
  let state = path.state
  let children: ReadonlyArray<SyntaxTree.Element> = Object.freeze([...keyword.elements, path.node])
  if (nextSignificantKind(state) === 'AsKeyword') {
    const alias = parseImportAlias(state)
    state = alias.state
    children = Object.freeze([...children, alias.node])
  }
  if (nextSignificantKind(state) === 'LeftBrace') {
    const members = parseImportMemberList(state)
    state = members.state
    children = Object.freeze([...children, members.node])
  }
  return Object.freeze({ state, node: syntaxNode(state, 'ImportDeclaration', children) })
}

const beginsTopLevelDeclaration = (state: State): boolean => {
  const kind = nextSignificantKind(state)
  if (kind === 'ImportKeyword' || kind === 'FnKeyword' || kind === 'StructKeyword') return true
  if (kind !== 'PubKeyword') return false
  const following = significantKindAfter(state, 1)
  return following === 'FnKeyword' || following === 'StructKeyword'
}

const parseStructField = (initial: State): NodeResult => {
  const hasPublicModifier = nextSignificantKind(initial) === 'PubKeyword'
  const pubKeyword = hasPublicModifier
    ? expect(initial, 'PubKeyword', ['Identifier', 'RightBrace', ...topLevelFollowing])
    : Object.freeze({ state: initial, elements: Object.freeze([]) })
  const name = expect(pubKeyword.state, 'Identifier', ['Colon', 'RightBrace', ...topLevelFollowing])
  const colon = expect(name.state, 'Colon', [...typeStarts, 'RightBrace', ...topLevelFollowing])
  const type = parseType(colon.state, ['PubKeyword', 'RightBrace', ...topLevelFollowing], true)
  return Object.freeze({
    state: type.state,
    node: syntaxNode(type.state, 'StructField', [
      ...pubKeyword.elements,
      ...name.elements,
      ...colon.elements,
      type.node,
    ]),
  })
}

const parseStructDeclaration = (initial: State): NodeResult => {
  const hasPublicModifier = nextSignificantKind(initial) === 'PubKeyword'
  const pubKeyword = hasPublicModifier
    ? expect(initial, 'PubKeyword', ['StructKeyword', 'Identifier', 'LeftBrace'])
    : Object.freeze({ state: initial, elements: Object.freeze([]) })
  const keyword = expect(pubKeyword.state, 'StructKeyword', ['Identifier', 'LeftBrace'])
  const name = expect(keyword.state, 'Identifier', ['Less', 'LeftBrace', ...topLevelFollowing])
  const typeParameters =
    nextSignificantKind(name.state) === 'Less'
      ? parseTypeParameterList(name.state, ['LeftBrace'])
      : undefined
  const afterName = typeParameters?.state ?? name.state
  const left = expect(afterName, 'LeftBrace', [
    'PubKeyword',
    'Identifier',
    'RightBrace',
    ...topLevelFollowing,
  ])
  let state = left.state
  let children: ReadonlyArray<SyntaxTree.Element> = Object.freeze([
    ...pubKeyword.elements,
    ...keyword.elements,
    ...name.elements,
    ...(typeParameters === undefined ? [] : [typeParameters.node]),
    ...left.elements,
  ])

  while (
    !beginsTopLevelDeclaration(state) &&
    (nextSignificantKind(state) === 'PubKeyword' || nextSignificantKind(state) === 'Identifier')
  ) {
    const field = parseStructField(state)
    children = Object.freeze([...children, field.node])
    state = field.state
  }

  const right = expect(state, 'RightBrace', topLevelFollowing)
  return Object.freeze({
    state: right.state,
    node: syntaxNode(right.state, 'StructDeclaration', [...children, ...right.elements]),
  })
}

const parseTopLevelDeclaration = (state: State): NodeResult =>
  nextSignificantKind(state) === 'ImportKeyword'
    ? parseImportDeclaration(state)
    : nextSignificantKind(state) === 'StructKeyword' ||
        (nextSignificantKind(state) === 'PubKeyword' &&
          significantKindAfter(state, 1) === 'StructKeyword')
      ? parseStructDeclaration(state)
      : parseFunctionDeclaration(state)

const parseFunctionDeclaration = (initial: State): NodeResult => {
  let lookahead = initial.index
  let lookaheadToken = initial.lexical.tokens.at(lookahead)
  let hasPublicModifier = false
  while (
    lookaheadToken !== undefined &&
    lookaheadToken.kind !== 'FnKeyword' &&
    lookaheadToken.kind !== 'StructKeyword' &&
    lookaheadToken.kind !== 'EndOfFile'
  ) {
    if (lookaheadToken.kind === 'PubKeyword') {
      hasPublicModifier = true
      break
    }
    lookahead += 1
    lookaheadToken = initial.lexical.tokens.at(lookahead)
  }
  const pubKeyword = hasPublicModifier
    ? expect(initial, 'PubKeyword', ['FnKeyword', 'Identifier', 'LeftParenthesis'])
    : Object.freeze({ state: initial, elements: Object.freeze([]) })
  const fnKeyword = expect(pubKeyword.state, 'FnKeyword', ['Identifier', 'LeftParenthesis'])
  const name = expect(fnKeyword.state, 'Identifier', ['Less', 'LeftParenthesis'])
  const typeParameters =
    nextSignificantKind(name.state) === 'Less'
      ? parseTypeParameterList(name.state, ['LeftParenthesis'])
      : undefined
  const parameters = parseParameterList(typeParameters?.state ?? name.state)
  const returnType = parseReturnType(parameters.state)
  const block = parseBlock(returnType.state, true)

  return Object.freeze({
    state: block.state,
    node: syntaxNode(block.state, 'FunctionDeclaration', [
      ...pubKeyword.elements,
      ...fnKeyword.elements,
      ...name.elements,
      ...(typeParameters === undefined ? [] : [typeParameters.node]),
      parameters.node,
      returnType.node,
      block.node,
    ]),
  })
}

const compareDiagnostics = (left: Diagnostic.Diagnostic, right: Diagnostic.Diagnostic): number =>
  left.span.start - right.span.start ||
  left.span.end - right.span.end ||
  (left.code < right.code ? -1 : left.code > right.code ? 1 : 0)

/** Parses one or more bootstrap functions with lossless local recovery. */
export const parse = (lexical: Lexer.LexicalResult): SyntaxFile.SyntaxFile => {
  const initial: State = Object.freeze({
    lexical,
    index: 0,
    diagnostics: Object.freeze([]),
  })
  const first = parseTopLevelDeclaration(initial)
  let state = first.state
  let declarations: ReadonlyArray<SyntaxTree.Node> = Object.freeze([first.node])
  let significantKind = nextSignificantKind(state)

  while (significantKind !== undefined && significantKind !== 'EndOfFile') {
    const declaration = parseTopLevelDeclaration(state)
    declarations = Object.freeze([...declarations, declaration.node])
    state = declaration.state
    significantKind = nextSignificantKind(state)
  }

  const endOfFile = expect(state, 'EndOfFile', [])
  const root = syntaxNode(endOfFile.state, 'SourceFile', [...declarations, ...endOfFile.elements])

  return SyntaxFile.make(
    lexical.source,
    lexical.tokens,
    root,
    lexical.diagnostics,
    Object.freeze([...endOfFile.state.diagnostics].sort(compareDiagnostics)),
  )
}
