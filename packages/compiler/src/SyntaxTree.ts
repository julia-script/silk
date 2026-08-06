import * as Option from 'effect/Option'
import type * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import type * as Token from './Token.js'

/** The complete node vocabulary for the first bootstrap grammar slice. */
export type NodeKind =
  | 'SourceFile'
  | 'ImportDeclaration'
  | 'ImportPath'
  | 'ImportAlias'
  | 'ImportMemberList'
  | 'ImportMember'
  | 'StructDeclaration'
  | 'StructField'
  | 'TypePath'
  | 'FixedArrayType'
  | 'FunctionDeclaration'
  | 'ParameterList'
  | 'ParameterDeclaration'
  | 'ReturnType'
  | 'Block'
  | 'BindingStatement'
  | 'ConditionalStatement'
  | 'ReturnStatement'
  | 'IntegerLiteralExpression'
  | 'BooleanLiteralExpression'
  | 'IdentifierExpression'
  | 'MoveExpression'
  | 'StructLiteralExpression'
  | 'ArrayLiteralExpression'
  | 'StructFieldInitializer'
  | 'FieldProjectionExpression'
  | 'IndexProjectionExpression'
  | 'CallExpression'
  | 'GroupedExpression'
  | 'PrefixExpression'
  | 'InfixExpression'
  | 'PipelineExpression'
  | 'PipelineTarget'
  | 'ArgumentList'
  | 'Error'

/** An absent required token at one exact source position. */
export interface MissingToken {
  readonly _tag: 'MissingToken'
  readonly expected: Token.TokenKind
  readonly span: SourceSpan.SourceSpan
}

/** One immutable concrete syntax node. */
export interface Node {
  readonly _tag: 'SyntaxNode'
  readonly kind: NodeKind
  readonly span: SourceSpan.SourceSpan
  readonly children: ReadonlyArray<Element>
}

/** A concrete node, original lexer token, or expected-but-absent token. */
export type Element = Node | Token.Token | MissingToken

/** Tests whether an element is a concrete syntax node. */
export const isNode = (self: Element): self is Node => self._tag === 'SyntaxNode'

/** Tests whether an element is an original lexer token. */
export const isToken = (self: Element): self is Token.Token => self._tag === 'Token'

/** Tests whether an element is an expected-but-absent token. */
export const isMissingToken = (self: Element): self is MissingToken => self._tag === 'MissingToken'

/** Returns the source-owned span of any syntax element. */
export const span = (self: Element): SourceSpan.SourceSpan => self.span

/** Creates an absent token when the insertion offset belongs to the source. */
export const missingToken = (
  source: SourceFile.SourceFile,
  expected: Token.TokenKind,
  offset: number,
): Option.Option<MissingToken> =>
  Option.map(SourceSpan.make(source, offset, offset), (span) =>
    Object.freeze({
      _tag: 'MissingToken' as const,
      expected,
      span,
    }),
  )

const orderedChildren = (
  source: SourceFile.SourceFile,
  children: ReadonlyArray<Element>,
): boolean => {
  let previousEnd = 0
  for (const [index, child] of children.entries()) {
    if (child.span.sourceId !== source.id) return false
    if (index > 0 && child.span.start < previousEnd) return false
    previousEnd = child.span.end
  }
  return true
}

/**
 * Creates a node whose span is derived from its ordered children, or from `emptyOffset` when it has
 * no children. Foreign or overlapping child spans return `None`.
 */
export const make = (
  source: SourceFile.SourceFile,
  kind: NodeKind,
  children: ReadonlyArray<Element>,
  emptyOffset: number,
): Option.Option<Node> => {
  if (!orderedChildren(source, children)) return Option.none()
  const first = children.at(0)
  const last = children.at(-1)
  const nodeSpan = SourceSpan.make(
    source,
    first?.span.start ?? emptyOffset,
    last?.span.end ?? emptyOffset,
  )
  return Option.map(nodeSpan, (span) =>
    Object.freeze({
      _tag: 'SyntaxNode' as const,
      kind,
      span,
      children: Object.freeze(Array.from(children)),
    }),
  )
}

/** Returns the direct child nodes of one kind in concrete order. */
export const directNodes = (parent: Node, kind: NodeKind): ReadonlyArray<Node> =>
  parent.children.filter((element): element is Node => isNode(element) && element.kind === kind)

/** Returns the first direct child node of one kind, if present. */
export const directNode = (parent: Node, kind: NodeKind): Node | undefined =>
  parent.children.find((element): element is Node => isNode(element) && element.kind === kind)

/** Returns the first direct child token of one kind, if present. */
export const directToken = (parent: Node, kind: Token.TokenKind): Token.Token | undefined =>
  parent.children.find(
    (element): element is Token.Token => isToken(element) && element.kind === kind,
  )

/** Tests whether an element contains no missing tokens or error regions at any depth. */
export const isAvailableSyntax = (element: Element): boolean =>
  !isMissingToken(element) &&
  !(isNode(element) && (element.kind === 'Error' || !element.children.every(isAvailableSyntax)))

/** Returns the missing child expecting one kind, or the parent as the unavailable region. */
export const unavailableChild = (parent: Node, expected: Token.TokenKind): Element =>
  parent.children.find(
    (element): element is MissingToken => isMissingToken(element) && element.expected === expected,
  ) ?? parent

/** Returns the first missing or damaged element, or the fallback node. */
export const unavailableElement = (elements: ReadonlyArray<Element>, fallback: Node): Element =>
  elements.find((element) => isMissingToken(element) || !isAvailableSyntax(element)) ?? fallback

/** Returns every original lexer token below a node in concrete source order. */
export const tokens = (self: Node): ReadonlyArray<Token.Token> =>
  Object.freeze(
    self.children.flatMap((child): ReadonlyArray<Token.Token> => {
      if (isToken(child)) return [child]
      if (isNode(child)) return tokens(child)
      return []
    }),
  )
