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
  readonly ordinal: number
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

/** A call callee resolved against top-level declarations or unavailable after syntax recovery. */
export type CallReferenceFact =
  | {
      readonly _tag: 'Resolved'
      readonly spelling: string
      readonly token: Token.Token
      readonly declaration: DeclarationFact
    }
  | {
      readonly _tag: 'Missing'
      readonly spelling: string
      readonly token: Token.Token
    }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly token: Token.Token
      readonly declarations: ReadonlyArray<DeclarationFact>
    }
  | {
      readonly _tag: 'Unavailable'
      readonly syntax: SyntaxTree.Element
    }

/** The available or unavailable type of one returned expression. */
export type ExpressionTypeFact =
  | { readonly _tag: 'Available'; readonly type: SemanticType }
  | { readonly _tag: 'Unavailable' }

/** One returned expression with exact concrete provenance. */
export type ReturnedExpressionFact =
  | {
      readonly _tag: 'Integer'
      readonly integer: IntegerExpressionFact
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'Identifier'
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'Call'
      readonly reference: CallReferenceFact
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }

/** Whether one returned expression is known to match its declared result type. */
export type ReturnCompatibility = { readonly _tag: 'Compatible' } | { readonly _tag: 'Unavailable' }

/** One public function declaration and its syntax-owned semantic facts. */
export interface DeclarationFact {
  readonly _tag: 'FunctionDeclaration'
  readonly id: DeclarationId
  readonly visibility: 'Public'
  readonly parameterCount: number
  readonly name: DeclaredName
  readonly returnType: ReturnTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One function's declaration, returned expression, and compatibility facts. */
export interface FunctionFact {
  readonly _tag: 'FunctionFact'
  readonly declaration: DeclarationFact
  readonly returnedExpression: ReturnedExpressionFact
  readonly returnCompatibility: ReturnCompatibility
}

/** The closed result of looking up one declaration spelling. */
export type DeclarationLookup =
  | {
      readonly _tag: 'Resolved'
      readonly spelling: string
      readonly declaration: DeclarationFact
    }
  | {
      readonly _tag: 'Missing'
      readonly spelling: string
    }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly declarations: ReadonlyArray<DeclarationFact>
    }

/** The complete deterministic semantic result for all direct bootstrap declarations. */
export interface Result {
  readonly _tag: 'SemanticAnalysis'
  readonly parse: Parser.ParseResult
  readonly functions: ReadonlyArray<FunctionFact>
  readonly diagnostics: ReadonlyArray<SemanticDiagnostic.SemanticDiagnostic>
}

const i32Maximum = 2147483647
const compatible: ReturnCompatibility = Object.freeze({ _tag: 'Compatible' })
const unavailableCompatibility: ReturnCompatibility = Object.freeze({ _tag: 'Unavailable' })
const availableI32ExpressionType: ExpressionTypeFact = Object.freeze({
  _tag: 'Available',
  type: 'I32',
})
const unavailableExpressionType: ExpressionTypeFact = Object.freeze({ _tag: 'Unavailable' })

const childNode = (parent: SyntaxTree.Node, kind: SyntaxTree.NodeKind): SyntaxTree.Node => {
  const child = parent.children.find(
    (element): element is SyntaxTree.Node => SyntaxTree.isNode(element) && element.kind === kind,
  )
  if (child === undefined) {
    throw new RangeError(`Semantic analysis expected ${kind} below ${parent.kind}`)
  }
  return child
}

const directChildNodes = (
  parent: SyntaxTree.Node,
  kind: SyntaxTree.NodeKind,
): ReadonlyArray<SyntaxTree.Node> =>
  parent.children.filter(
    (element): element is SyntaxTree.Node => SyntaxTree.isNode(element) && element.kind === kind,
  )

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

interface ReturnedExpressionResult {
  readonly fact: ReturnedExpressionFact
  readonly diagnostics: ReadonlyArray<SemanticDiagnostic.SemanticDiagnostic>
  readonly type: SemanticType | undefined
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

const analyzeReturnedExpression = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
): ReturnedExpressionResult => {
  if (node.kind === 'IntegerLiteralExpression') {
    const integer = analyzeInteger(source, node)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Integer',
        integer: integer.fact,
        syntax: node,
      }),
      diagnostics: integer.diagnostics,
      type: integer.fact._tag === 'Available' ? integer.fact.type : undefined,
    })
  }

  if (node.kind === 'IdentifierExpression') {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Identifier',
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
      type: undefined,
    })
  }

  const token = directToken(node, 'Identifier')
  if (token === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Call',
        reference: Object.freeze({
          _tag: 'Unavailable',
          syntax: unavailableSyntax(node, 'Identifier'),
        }),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
      type: undefined,
    })
  }

  const tokenSpelling = spelling(source, token)
  const lookup = lookupDeclaration(declarations, tokenSpelling)
  const reference: CallReferenceFact =
    lookup._tag === 'Resolved'
      ? Object.freeze({
          _tag: 'Resolved',
          spelling: tokenSpelling,
          token,
          declaration: lookup.declaration,
        })
      : lookup._tag === 'Ambiguous'
        ? Object.freeze({
            _tag: 'Ambiguous',
            spelling: tokenSpelling,
            token,
            declarations: lookup.declarations,
          })
        : Object.freeze({
            _tag: 'Missing',
            spelling: tokenSpelling,
            token,
          })
  const syntaxAvailable = node.children.every(isAvailableSyntax)
  const expressionType =
    syntaxAvailable &&
    reference._tag === 'Resolved' &&
    reference.declaration.returnType._tag === 'Resolved'
      ? availableI32ExpressionType
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Call',
      reference,
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze(
      reference._tag === 'Missing'
        ? [SemanticDiagnostic.unknownFunction(reference.spelling, reference.token.span)]
        : [],
    ),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

const returnedExpressionNode = (returnStatement: SyntaxTree.Node): SyntaxTree.Node => {
  const expression = returnStatement.children.find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) &&
      (element.kind === 'IntegerLiteralExpression' ||
        element.kind === 'IdentifierExpression' ||
        element.kind === 'CallExpression'),
  )
  if (expression === undefined) {
    throw new RangeError('Semantic analysis expected a returned expression')
  }
  return expression
}

const isAvailableSyntax = (element: SyntaxTree.Element): boolean =>
  !SyntaxTree.isMissingToken(element) &&
  !(
    SyntaxTree.isNode(element) &&
    (element.kind === 'Error' || !element.children.every(isAvailableSyntax))
  )

const compareDiagnostics = (
  left: SemanticDiagnostic.SemanticDiagnostic,
  right: SemanticDiagnostic.SemanticDiagnostic,
): number =>
  left.span.start - right.span.start ||
  left.span.end - right.span.end ||
  (left.code < right.code ? -1 : left.code > right.code ? 1 : 0)

interface FunctionAnalysis {
  readonly fact: FunctionFact
  readonly diagnostics: ReadonlyArray<SemanticDiagnostic.SemanticDiagnostic>
}

interface DeclarationHeader {
  readonly node: SyntaxTree.Node
  readonly declaration: DeclarationFact
  readonly diagnostics: ReadonlyArray<SemanticDiagnostic.SemanticDiagnostic>
}

const analyzeDeclarationHeader = (
  source: SourceFile.SourceFile,
  functionNode: SyntaxTree.Node,
  ordinal: number,
): DeclarationHeader => {
  const returnTypeNode = childNode(functionNode, 'ReturnType')
  const parameterListNode = childNode(functionNode, 'ParameterList')
  const returnType = analyzeReturnType(source, returnTypeNode)
  const declaration: DeclarationFact = Object.freeze({
    _tag: 'FunctionDeclaration',
    id: Object.freeze({
      _tag: 'DeclarationId',
      sourceId: source.id,
      ordinal,
    }),
    visibility: 'Public',
    parameterCount: directChildNodes(parameterListNode, 'ParameterDeclaration').length,
    name: presentName(source, functionNode),
    returnType: returnType.fact,
    syntax: functionNode,
  })

  return Object.freeze({
    node: functionNode,
    declaration,
    diagnostics: returnType.diagnostics,
  })
}

const analyzeFunctionBody = (
  source: SourceFile.SourceFile,
  header: DeclarationHeader,
  declarations: ReadonlyArray<DeclarationFact>,
): FunctionAnalysis => {
  const blockNode = childNode(header.node, 'Block')
  const returnStatementNode = childNode(blockNode, 'ReturnStatement')
  const expressionNode = returnedExpressionNode(returnStatementNode)
  const expression = analyzeReturnedExpression(source, expressionNode, declarations)
  const returnCompatibility =
    header.declaration.returnType._tag === 'Resolved' &&
    expression.type === header.declaration.returnType.type
      ? compatible
      : unavailableCompatibility

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FunctionFact',
      declaration: header.declaration,
      returnedExpression: expression.fact,
      returnCompatibility,
    }),
    diagnostics: Object.freeze([...header.diagnostics, ...expression.diagnostics]),
  })
}

interface PresentNameEntry {
  readonly spelling: string
  readonly token: Token.Token
  readonly declaration: DeclarationFact
}

const presentNameEntries = (
  declarations: ReadonlyArray<DeclarationFact>,
): ReadonlyArray<PresentNameEntry> =>
  Object.freeze(
    declarations.flatMap((declaration): ReadonlyArray<PresentNameEntry> => {
      const name = declaration.name
      return name._tag === 'Present'
        ? [
            Object.freeze({
              spelling: name.spelling,
              token: name.token,
              declaration,
            }),
          ]
        : []
    }),
  )

const duplicateNameDiagnostics = (
  entries: ReadonlyArray<PresentNameEntry>,
): ReadonlyArray<SemanticDiagnostic.SemanticDiagnostic> => {
  const firstBySpelling = new Map<string, PresentNameEntry>()
  let diagnostics: ReadonlyArray<SemanticDiagnostic.SemanticDiagnostic> = Object.freeze([])

  for (const entry of entries) {
    const original = firstBySpelling.get(entry.spelling)
    if (original === undefined) {
      firstBySpelling.set(entry.spelling, entry)
    } else {
      diagnostics = Object.freeze([
        ...diagnostics,
        SemanticDiagnostic.duplicateDeclarationName(
          entry.spelling,
          original.token.span,
          entry.token.span,
        ),
      ])
    }
  }

  return diagnostics
}

const lookupDeclaration = (
  declarations: ReadonlyArray<DeclarationFact>,
  spelling: string,
): DeclarationLookup => {
  const matches = presentNameEntries(declarations)
    .filter((entry) => entry.spelling === spelling)
    .map((entry) => entry.declaration)
  const first = matches.at(0)

  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling })
  if (matches.length === 1) {
    return Object.freeze({ _tag: 'Resolved', spelling, declaration: first })
  }
  return Object.freeze({
    _tag: 'Ambiguous',
    spelling,
    declarations: Object.freeze(matches),
  })
}

/** Collects every declaration before resolving returned expressions into immutable facts. */
export const analyze = (parse: Parser.ParseResult): Result => {
  const source = parse.lexical.source
  const headers = directChildNodes(parse.root, 'FunctionDeclaration').map((node, ordinal) =>
    analyzeDeclarationHeader(source, node, ordinal),
  )
  const declarations = Object.freeze(headers.map((header) => header.declaration))
  const analyzed = headers.map((header) => analyzeFunctionBody(source, header, declarations))
  const functions = Object.freeze(analyzed.map((result) => result.fact))
  const diagnostics = [
    ...analyzed.flatMap((result) => result.diagnostics),
    ...duplicateNameDiagnostics(presentNameEntries(declarations)),
  ].sort(compareDiagnostics)

  return Object.freeze({
    _tag: 'SemanticAnalysis',
    parse,
    functions,
    diagnostics: Object.freeze(diagnostics),
  })
}

/** Looks up every present declaration with the exact requested spelling. */
export const declarationByName = dual<
  (spelling: string) => (self: Result) => DeclarationLookup,
  (self: Result, spelling: string) => DeclarationLookup
>(2, (self, spelling) =>
  lookupDeclaration(
    self.functions.map((fact) => fact.declaration),
    spelling,
  ),
)
