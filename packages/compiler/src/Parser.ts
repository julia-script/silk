import * as Option from 'effect/Option'
import * as Diagnostic from './Diagnostic.js'
import type * as Lexer from './Lexer.js'
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
  'RightBrace',
  'LetKeyword',
  'ReturnKeyword',
])

const parseIntegerLiteralExpression = (initial: State): NodeResult => {
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

const parseMoveExpression = (initial: State): NodeResult => {
  const keyword = expect(initial, 'MoveKeyword', ['Identifier', ...expressionFollowing])
  const name = expect(keyword.state, 'Identifier', expressionFollowing)
  return Object.freeze({
    state: name.state,
    node: syntaxNode(name.state, 'MoveExpression', [...keyword.elements, ...name.elements]),
  })
}

const expressionKind = (
  state: State,
  recoveryKind: 'Integer' | 'Identifier',
): 'Integer' | 'Identifier' | 'Move' | 'Call' => {
  let index = state.index
  let token = state.lexical.tokens.at(index)

  while (token !== undefined) {
    if (token.kind === 'DecimalInteger') return 'Integer'
    if (token.kind === 'MoveKeyword') return 'Move'
    if (token.kind === 'Identifier') {
      index += 1
      token = state.lexical.tokens.at(index)
      while (token !== undefined && isTrivia(token.kind)) {
        index += 1
        token = state.lexical.tokens.at(index)
      }
      return token?.kind === 'LeftParenthesis' ? 'Call' : 'Identifier'
    }
    if (token.kind === 'LeftParenthesis') return 'Call'
    if (
      token.kind === 'Comma' ||
      token.kind === 'RightParenthesis' ||
      token.kind === 'RightBrace' ||
      token.kind === 'LetKeyword' ||
      token.kind === 'ReturnKeyword' ||
      token.kind === 'PubKeyword' ||
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
    if (token.kind === 'RightBrace' || token.kind === 'PubKeyword' || token.kind === 'EndOfFile') {
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
  return expect(initial, 'RightParenthesis', ['RightBrace', 'PubKeyword'])
}

function parseArgumentList(initial: State, reservedForEnclosingCalls: number): NodeResult {
  const leftParenthesis = expect(initial, 'LeftParenthesis', [
    'DecimalInteger',
    'Identifier',
    'RightParenthesis',
    'RightBrace',
    'PubKeyword',
  ])
  let state = leftParenthesis.state
  let children: ReadonlyArray<SyntaxTree.Element> = leftParenthesis.elements
  let kind = nextSignificantKind(state)

  while (
    kind !== undefined &&
    kind !== 'RightParenthesis' &&
    kind !== 'RightBrace' &&
    kind !== 'PubKeyword' &&
    kind !== 'EndOfFile'
  ) {
    const argument = parseExpression(state, reservedForEnclosingCalls + 1, 'Identifier')
    children = Object.freeze([...children, argument.node])
    state = argument.state
    kind = nextSignificantKind(state)

    if (kind === 'RightParenthesis' || kind === 'RightBrace' || kind === 'PubKeyword') break

    const comma = expect(state, 'Comma', [
      'DecimalInteger',
      'Identifier',
      'RightParenthesis',
      'RightBrace',
      'PubKeyword',
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
    'RightParenthesis',
    'RightBrace',
    'PubKeyword',
  ])
  const argumentsList = parseArgumentList(callee.state, reservedForEnclosingCalls)
  return Object.freeze({
    state: argumentsList.state,
    node: syntaxNode(argumentsList.state, 'CallExpression', [
      ...callee.elements,
      argumentsList.node,
    ]),
  })
}

function parseExpression(
  initial: State,
  reservedForEnclosingCalls: number,
  recoveryKind: 'Integer' | 'Identifier',
): NodeResult {
  const kind = expressionKind(initial, recoveryKind)
  if (kind === 'Call') return parseCallExpression(initial, reservedForEnclosingCalls)
  if (kind === 'Move') return parseMoveExpression(initial)
  if (kind === 'Identifier') return parseIdentifierExpression(initial)
  return parseIntegerLiteralExpression(initial)
}

const parseReturnStatement = (initial: State): NodeResult => {
  const keyword = expect(initial, 'ReturnKeyword', [
    'DecimalInteger',
    'Identifier',
    'MoveKeyword',
    'LeftParenthesis',
    'RightBrace',
  ])
  const expression = parseExpression(keyword.state, 0, 'Integer')
  return Object.freeze({
    state: expression.state,
    node: syntaxNode(expression.state, 'ReturnStatement', [...keyword.elements, expression.node]),
  })
}

const parseBindingStatement = (initial: State): NodeResult => {
  const keyword = expect(initial, 'LetKeyword', [
    'Identifier',
    'Equals',
    'ReturnKeyword',
    'RightBrace',
  ])
  const name = expect(keyword.state, 'Identifier', [
    'Equals',
    'LetKeyword',
    'ReturnKeyword',
    'RightBrace',
  ])
  const equals = expect(name.state, 'Equals', [
    'DecimalInteger',
    'Identifier',
    'MoveKeyword',
    'LetKeyword',
    'ReturnKeyword',
    'RightBrace',
  ])
  const expression = parseExpression(equals.state, 0, 'Integer')
  return Object.freeze({
    state: expression.state,
    node: syntaxNode(expression.state, 'BindingStatement', [
      ...keyword.elements,
      ...name.elements,
      ...equals.elements,
      expression.node,
    ]),
  })
}

const parseBlock = (initial: State): NodeResult => {
  const leftBrace = expect(initial, 'LeftBrace', [
    'LetKeyword',
    'ReturnKeyword',
    'DecimalInteger',
    'Identifier',
    'LeftParenthesis',
    'RightBrace',
  ])
  let state = leftBrace.state
  let children: ReadonlyArray<SyntaxTree.Element> = leftBrace.elements
  let sawReturn = false

  let kind = nextSignificantKind(state)
  while (kind === 'LetKeyword' || kind === 'ReturnKeyword') {
    const statement =
      kind === 'LetKeyword' ? parseBindingStatement(state) : parseReturnStatement(state)
    if (kind === 'ReturnKeyword') sawReturn = true
    children = Object.freeze([...children, statement.node])
    state = statement.state
    kind = nextSignificantKind(state)
  }

  if (!sawReturn) {
    const statement = parseReturnStatement(state)
    children = Object.freeze([...children, statement.node])
    state = statement.state
  }

  const rightBrace = expect(state, 'RightBrace', ['PubKeyword'])
  return Object.freeze({
    state: rightBrace.state,
    node: syntaxNode(rightBrace.state, 'Block', [...children, ...rightBrace.elements]),
  })
}

const parseReturnType = (initial: State): NodeResult => {
  const arrow = expect(initial, 'Arrow', ['Identifier', 'LeftBrace'])
  const name = expect(arrow.state, 'Identifier', ['LeftBrace'])
  return Object.freeze({
    state: name.state,
    node: syntaxNode(name.state, 'ReturnType', [...arrow.elements, ...name.elements]),
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
    kind !== 'EndOfFile'
  ) {
    const name = expect(state, 'Identifier', ['Colon', 'Comma', 'RightParenthesis', 'Arrow'])
    const colon = expect(name.state, 'Colon', ['Identifier', 'Comma', 'RightParenthesis', 'Arrow'])
    const type = expect(colon.state, 'Identifier', ['Comma', 'RightParenthesis', 'Arrow'])
    const parameter = syntaxNode(type.state, 'ParameterDeclaration', [
      ...name.elements,
      ...colon.elements,
      ...type.elements,
    ])
    children = Object.freeze([...children, parameter])
    state = type.state
    kind = nextSignificantKind(state)

    if (kind === 'RightParenthesis' || kind === 'Arrow' || kind === 'PubKeyword') break

    const comma = expect(state, 'Comma', ['Identifier', 'RightParenthesis', 'Arrow', 'PubKeyword'])
    children = Object.freeze([...children, ...comma.elements])
    state = comma.state
    kind = nextSignificantKind(state)
  }

  const rightParenthesis = expect(state, 'RightParenthesis', ['Arrow', 'PubKeyword'])
  return Object.freeze({
    state: rightParenthesis.state,
    node: syntaxNode(rightParenthesis.state, 'ParameterList', [
      ...children,
      ...rightParenthesis.elements,
    ]),
  })
}

const parseImportDeclaration = (initial: State): NodeResult => {
  const keyword = expect(initial, 'ImportKeyword', ['Identifier', 'PubKeyword', 'FnKeyword'])
  const name = expect(keyword.state, 'Identifier', ['PubKeyword', 'ImportKeyword', 'FnKeyword'])
  return Object.freeze({
    state: name.state,
    node: syntaxNode(name.state, 'ImportDeclaration', [...keyword.elements, ...name.elements]),
  })
}

const parseTopLevelDeclaration = (state: State): NodeResult =>
  nextSignificantKind(state) === 'ImportKeyword'
    ? parseImportDeclaration(state)
    : parseFunctionDeclaration(state)

const parseFunctionDeclaration = (initial: State): NodeResult => {
  const pubKeyword = expect(initial, 'PubKeyword', ['FnKeyword', 'Identifier', 'LeftParenthesis'])
  const fnKeyword = expect(pubKeyword.state, 'FnKeyword', ['Identifier', 'LeftParenthesis'])
  const name = expect(fnKeyword.state, 'Identifier', ['LeftParenthesis'])
  const parameters = parseParameterList(name.state)
  const returnType = parseReturnType(parameters.state)
  const block = parseBlock(returnType.state)

  return Object.freeze({
    state: block.state,
    node: syntaxNode(block.state, 'FunctionDeclaration', [
      ...pubKeyword.elements,
      ...fnKeyword.elements,
      ...name.elements,
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
