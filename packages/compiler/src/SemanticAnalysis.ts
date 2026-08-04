import { dual } from 'effect/Function'
import * as Option from 'effect/Option'
import type * as Parser from './Parser.js'
import * as SemanticDiagnostic from './SemanticDiagnostic.js'
import * as SourceFile from './SourceFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'

/** The only semantic type recognized by the first analysis slice. */
export type SemanticType = 'I32'

/** A deterministic declaration identity local to one analyzed source snapshot. */
export interface DeclarationId {
  readonly _tag: 'DeclarationId'
  readonly sourceId: string
  readonly ordinal: 0
}

/** A declaration name supplied by syntax or explicitly unavailable after recovery. */
export type DeclaredName =
  | {
      readonly _tag: 'Present'
      readonly spelling: string
      readonly token: Token.Token
    }
  | {
      readonly _tag: 'Unavailable'
      readonly syntax: SyntaxTree.Element
    }

/** The resolved, unresolved, or syntax-unavailable declared return type. */
export type ReturnTypeFact =
  | {
      readonly _tag: 'Resolved'
      readonly type: SemanticType
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'Unresolved'
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'Unavailable'
      readonly syntax: SyntaxTree.Element
    }

/** The available, out-of-range, or syntax-unavailable integer-expression fact. */
export type IntegerExpressionFact =
  | {
      readonly _tag: 'Available'
      readonly type: SemanticType
      readonly value: number
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'OutOfRange'
      readonly type: SemanticType
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'Unavailable'
      readonly syntax: SyntaxTree.Element
    }

/** Whether the first returned expression is known to match its declared result type. */
export type ReturnCompatibility = { readonly _tag: 'Compatible' } | { readonly _tag: 'Unavailable' }

/** The first public function declaration and its syntax-owned semantic facts. */
export interface DeclarationFact {
  readonly _tag: 'FunctionDeclaration'
  readonly id: DeclarationId
  readonly visibility: 'Public'
  readonly parameterCount: 0
  readonly name: DeclaredName
  readonly returnType: ReturnTypeFact
  readonly syntax: SyntaxTree.Node
}

/** The complete deterministic semantic result for the first bootstrap grammar slice. */
export interface Result {
  readonly _tag: 'SemanticAnalysis'
  readonly parse: Parser.ParseResult
  readonly declaration: DeclarationFact
  readonly integerExpression: IntegerExpressionFact
  readonly returnCompatibility: ReturnCompatibility
  readonly diagnostics: ReadonlyArray<SemanticDiagnostic.SemanticDiagnostic>
}

const i32Maximum = 2147483647
const compatible: ReturnCompatibility = Object.freeze({ _tag: 'Compatible' })
const unavailableCompatibility: ReturnCompatibility = Object.freeze({ _tag: 'Unavailable' })

const childNode = (parent: SyntaxTree.Node, kind: SyntaxTree.NodeKind): SyntaxTree.Node => {
  const child = parent.children.find(
    (element): element is SyntaxTree.Node => SyntaxTree.isNode(element) && element.kind === kind,
  )
  if (child === undefined) {
    throw new RangeError(`Semantic analysis expected ${kind} below ${parent.kind}`)
  }
  return child
}

const directToken = (parent: SyntaxTree.Node, kind: Token.TokenKind): Token.Token | undefined =>
  parent.children.find(
    (element): element is Token.Token => SyntaxTree.isToken(element) && element.kind === kind,
  )

const unavailableSyntax = (
  parent: SyntaxTree.Node,
  expected: Token.TokenKind,
): SyntaxTree.Element =>
  parent.children.find(
    (element): element is SyntaxTree.MissingToken =>
      SyntaxTree.isMissingToken(element) && element.expected === expected,
  ) ?? parent

const spelling = (source: SourceFile.SourceFile, token: Token.Token): string => {
  const bytes = Option.getOrThrowWith(
    SourceFile.slice(source, token.span),
    () => new RangeError(`Semantic token span does not belong to source ${source.id}`),
  )
  return Array.from(bytes, (byte) => String.fromCharCode(byte)).join('')
}

const presentName = (source: SourceFile.SourceFile, node: SyntaxTree.Node): DeclaredName => {
  const token = directToken(node, 'Identifier')
  return token === undefined
    ? Object.freeze({
        _tag: 'Unavailable',
        syntax: unavailableSyntax(node, 'Identifier'),
      })
    : Object.freeze({
        _tag: 'Present',
        spelling: spelling(source, token),
        token,
      })
}

interface ReturnTypeResult {
  readonly fact: ReturnTypeFact
  readonly diagnostics: ReadonlyArray<SemanticDiagnostic.SemanticDiagnostic>
}

const analyzeReturnType = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): ReturnTypeResult => {
  const token = directToken(node, 'Identifier')
  if (token === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Unavailable',
        syntax: unavailableSyntax(node, 'Identifier'),
      }),
      diagnostics: Object.freeze([]),
    })
  }

  const tokenSpelling = spelling(source, token)
  if (tokenSpelling === 'I32') {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Resolved',
        type: 'I32',
        spelling: tokenSpelling,
        token,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
    })
  }

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Unresolved',
      spelling: tokenSpelling,
      token,
      syntax: node,
    }),
    diagnostics: Object.freeze([SemanticDiagnostic.unknownType(tokenSpelling, token.span)]),
  })
}

const positiveI32Value = (bytes: Uint8Array): Option.Option<number> => {
  let value = 0
  for (const byte of bytes) {
    const digit = byte - 0x30
    if (value > Math.floor((i32Maximum - digit) / 10)) return Option.none()
    value = value * 10 + digit
  }
  return Option.some(value)
}

interface IntegerResult {
  readonly fact: IntegerExpressionFact
  readonly diagnostics: ReadonlyArray<SemanticDiagnostic.SemanticDiagnostic>
}

const analyzeInteger = (source: SourceFile.SourceFile, node: SyntaxTree.Node): IntegerResult => {
  const token = directToken(node, 'DecimalInteger')
  if (token === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Unavailable',
        syntax: unavailableSyntax(node, 'DecimalInteger'),
      }),
      diagnostics: Object.freeze([]),
    })
  }

  const bytes = Option.getOrThrowWith(
    SourceFile.slice(source, token.span),
    () => new RangeError(`Semantic integer span does not belong to source ${source.id}`),
  )
  const value = positiveI32Value(bytes)
  if (Option.isSome(value)) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Available',
        type: 'I32',
        value: value.value,
        token,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
    })
  }

  const tokenSpelling = Array.from(bytes, (byte) => String.fromCharCode(byte)).join('')
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'OutOfRange',
      type: 'I32',
      spelling: tokenSpelling,
      token,
      syntax: node,
    }),
    diagnostics: Object.freeze([SemanticDiagnostic.integerOutOfRange(tokenSpelling, token.span)]),
  })
}

const compareDiagnostics = (
  left: SemanticDiagnostic.SemanticDiagnostic,
  right: SemanticDiagnostic.SemanticDiagnostic,
): number =>
  left.span.start - right.span.start ||
  left.span.end - right.span.end ||
  (left.code < right.code ? -1 : left.code > right.code ? 1 : 0)

/** Analyzes the exact first bootstrap function into immutable semantic facts. */
export const analyze = (parse: Parser.ParseResult): Result => {
  const source = parse.lexical.source
  const functionNode = childNode(parse.root, 'FunctionDeclaration')
  const returnTypeNode = childNode(functionNode, 'ReturnType')
  const blockNode = childNode(functionNode, 'Block')
  const returnStatementNode = childNode(blockNode, 'ReturnStatement')
  const integerNode = childNode(returnStatementNode, 'IntegerLiteralExpression')
  const returnType = analyzeReturnType(source, returnTypeNode)
  const integer = analyzeInteger(source, integerNode)
  const declaration: DeclarationFact = Object.freeze({
    _tag: 'FunctionDeclaration',
    id: Object.freeze({
      _tag: 'DeclarationId',
      sourceId: source.id,
      ordinal: 0,
    }),
    visibility: 'Public',
    parameterCount: 0,
    name: presentName(source, functionNode),
    returnType: returnType.fact,
    syntax: functionNode,
  })
  const returnCompatibility =
    returnType.fact._tag === 'Resolved' && integer.fact._tag === 'Available'
      ? compatible
      : unavailableCompatibility

  return Object.freeze({
    _tag: 'SemanticAnalysis',
    parse,
    declaration,
    integerExpression: integer.fact,
    returnCompatibility,
    diagnostics: Object.freeze(
      [...returnType.diagnostics, ...integer.diagnostics].sort(compareDiagnostics),
    ),
  })
}

/** Looks up the first declaration only when syntax supplied the exact requested name. */
export const declarationByName = dual<
  (name: string) => (self: Result) => Option.Option<DeclarationFact>,
  (self: Result, name: string) => Option.Option<DeclarationFact>
>(2, (self, name) =>
  self.declaration.name._tag === 'Present' && self.declaration.name.spelling === name
    ? Option.some(self.declaration)
    : Option.none(),
)
