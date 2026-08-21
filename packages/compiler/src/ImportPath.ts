import type * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'

const reservedSegmentKinds: ReadonlySet<Token.TokenKind> = new Set([
  'PubKeyword',
  'StructKeyword',
  'ServiceKeyword',
  'InterfaceKeyword',
  'RoleKeyword',
  'EffectKeyword',
  'FnKeyword',
  'RunKeyword',
  'FailKeyword',
  'DropKeyword',
  'UnsafeKeyword',
  'ImplKeyword',
  'ForKeyword',
  'ReturnKeyword',
  'ImportKeyword',
  'AsKeyword',
  'LetKeyword',
  'ConstKeyword',
  'MutKeyword',
  'OnceKeyword',
  'MoveKeyword',
  'MatchKeyword',
  'IfKeyword',
  'ElseKeyword',
  'WhileKeyword',
  'BreakKeyword',
  'ContinueKeyword',
  'TrueKeyword',
  'FalseKeyword',
])

/** Tests whether a token kind can name one contextual import-path segment. */
export const isSegmentKind = (kind: Token.TokenKind): boolean =>
  kind === 'Identifier' || reservedSegmentKinds.has(kind)

/** Tests whether a contextual path segment is unavailable as an ordinary source binding. */
export const isReservedSegment = (token: Token.Token): boolean => token.kind !== 'Identifier'

/** Returns the ordered contextual segment tokens retained by an import-path node. */
export const segments = (self: SyntaxTree.Node): ReadonlyArray<Token.Token> =>
  Object.freeze(
    self.children.filter(
      (element): element is Token.Token => element._tag === 'Token' && isSegmentKind(element.kind),
    ),
  )
