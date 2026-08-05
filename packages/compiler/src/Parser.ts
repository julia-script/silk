import * as Option from 'effect/Option'
import type * as Lexer from './Lexer.js'
import * as ParseDiagnostic from './ParseDiagnostic.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'

/** The deterministic concrete result of parsing one lexical result. */
export interface ParseResult {
  readonly lexical: Lexer.LexicalResult
  readonly root: SyntaxTree.Node
  readonly diagnostics: ReadonlyArray<ParseDiagnostic.ParseDiagnostic>
}

interface State {
  readonly lexical: Lexer.LexicalResult
  readonly index: number
  readonly diagnostics: ReadonlyArray<ParseDiagnostic.ParseDiagnostic>
}

interface ElementsResult {
  readonly state: State
  readonly elements: ReadonlyArray<SyntaxTree.Element>
}

interface NodeResult {
  readonly state: State
  readonly node: SyntaxTree.Node
}

const triviaKinds: ReadonlyArray<Token.TokenKind> = Object.freeze(['Whitespace', 'LineComment'])

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

const addDiagnostic = (state: State, diagnostic: ParseDiagnostic.ParseDiagnostic): State =>
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
    state = addDiagnostic(state, ParseDiagnostic.unexpectedTokens(error.span))
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
    state: addDiagnostic(state, ParseDiagnostic.missingToken(expected, missing.span)),
    elements: Object.freeze([...elements, missing]),
  })
}

const parseIntegerLiteralExpression = (initial: State): NodeResult => {
  const integer = expect(initial, 'DecimalInteger', ['Comma', 'RightParenthesis', 'RightBrace'])
  return Object.freeze({
    state: integer.state,
    node: syntaxNode(integer.state, 'IntegerLiteralExpression', integer.elements),
  })
}

const parseIdentifierExpression = (initial: State): NodeResult => {
  const identifier = expect(initial, 'Identifier', ['Comma', 'RightParenthesis', 'RightBrace'])
  return Object.freeze({
    state: identifier.state,
    node: syntaxNode(identifier.state, 'IdentifierExpression', identifier.elements),
  })
}

const expressionKind = (
  state: State,
  recoveryKind: 'Integer' | 'Identifier',
): 'Integer' | 'Identifier' | 'Call' => {
  let index = state.index
  let token = state.lexical.tokens.at(index)

  while (token !== undefined) {
    if (token.kind === 'DecimalInteger') return 'Integer'
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
        ParseDiagnostic.missingToken('RightParenthesis', missing.span),
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
  if (kind === 'Identifier') return parseIdentifierExpression(initial)
  return parseIntegerLiteralExpression(initial)
}

const parseReturnStatement = (initial: State): NodeResult => {
  const keyword = expect(initial, 'ReturnKeyword', [
    'DecimalInteger',
    'Identifier',
    'LeftParenthesis',
    'RightBrace',
  ])
  const expression = parseExpression(keyword.state, 0, 'Integer')
  return Object.freeze({
    state: expression.state,
    node: syntaxNode(expression.state, 'ReturnStatement', [...keyword.elements, expression.node]),
  })
}

const parseBlock = (initial: State): NodeResult => {
  const leftBrace = expect(initial, 'LeftBrace', [
    'ReturnKeyword',
    'DecimalInteger',
    'Identifier',
    'LeftParenthesis',
    'RightBrace',
  ])
  const statement = parseReturnStatement(leftBrace.state)
  const rightBrace = expect(statement.state, 'RightBrace', ['PubKeyword'])
  return Object.freeze({
    state: rightBrace.state,
    node: syntaxNode(rightBrace.state, 'Block', [
      ...leftBrace.elements,
      statement.node,
      ...rightBrace.elements,
    ]),
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

const compareDiagnostics = (
  left: ParseDiagnostic.ParseDiagnostic,
  right: ParseDiagnostic.ParseDiagnostic,
): number =>
  left.span.start - right.span.start ||
  left.span.end - right.span.end ||
  (left.code < right.code ? -1 : left.code > right.code ? 1 : 0)

/** Parses one or more bootstrap functions with lossless local recovery. */
export const parse = (lexical: Lexer.LexicalResult): ParseResult => {
  const initial: State = Object.freeze({
    lexical,
    index: 0,
    diagnostics: Object.freeze([]),
  })
  const first = parseFunctionDeclaration(initial)
  let state = first.state
  let declarations: ReadonlyArray<SyntaxTree.Node> = Object.freeze([first.node])
  let significantKind = nextSignificantKind(state)

  while (significantKind !== undefined && significantKind !== 'EndOfFile') {
    const declaration = parseFunctionDeclaration(state)
    declarations = Object.freeze([...declarations, declaration.node])
    state = declaration.state
    significantKind = nextSignificantKind(state)
  }

  const endOfFile = expect(state, 'EndOfFile', [])
  const root = syntaxNode(endOfFile.state, 'SourceFile', [...declarations, ...endOfFile.elements])

  return Object.freeze({
    lexical,
    root,
    diagnostics: Object.freeze([...endOfFile.state.diagnostics].sort(compareDiagnostics)),
  })
}
