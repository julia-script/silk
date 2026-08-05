import { dual } from 'effect/Function'
import * as Option from 'effect/Option'
import type * as Parser from './Parser.js'
import * as SemanticDiagnostic from './SemanticDiagnostic.js'
import * as SourceFile from './SourceFile.js'
import type * as SourceSpan from './SourceSpan.js'
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

/** A deterministic parameter identity nested under its owning function declaration. */
export interface ParameterId {
  readonly _tag: 'ParameterId'
  readonly function: DeclarationId
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
export type DeclaredTypeFact =
  | {
      readonly _tag: 'Resolved'
      readonly type: SemanticType
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Element
    }
  | {
      readonly _tag: 'Unresolved'
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Element
    }
  | {
      readonly _tag: 'Unavailable'
      readonly syntax: SyntaxTree.Element
    }

/** The declared type fact attached to a function return. */
export type ReturnTypeFact = DeclaredTypeFact

/** One ordered parameter declaration with exact concrete provenance. */
export interface ParameterFact {
  readonly _tag: 'ParameterDeclaration'
  readonly id: ParameterId
  readonly name: DeclaredName
  readonly declaredType: DeclaredTypeFact
  readonly syntax: SyntaxTree.Node
}

/** The closed result of looking up a parameter spelling within one function. */
export type ParameterLookup =
  | {
      readonly _tag: 'Resolved'
      readonly spelling: string
      readonly parameter: ParameterFact
    }
  | {
      readonly _tag: 'Missing'
      readonly spelling: string
    }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly parameters: ReadonlyArray<ParameterFact>
    }

/** A bare identifier resolved against its enclosing function's parameters. */
export type ParameterReferenceFact =
  | {
      readonly _tag: 'Resolved'
      readonly spelling: string
      readonly token: Token.Token
      readonly parameter: ParameterFact
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
      readonly parameters: ReadonlyArray<ParameterFact>
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

/** One bare identifier expression with its local reference and type facts. */
export interface IdentifierExpressionFact {
  readonly _tag: 'Identifier'
  readonly reference: ParameterReferenceFact
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One semantic expression fact at any returned or argument position. */
export type ExpressionFact =
  | {
      readonly _tag: 'Integer'
      readonly integer: IntegerExpressionFact
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }
  | IdentifierExpressionFact
  | {
      readonly _tag: 'Call'
      readonly reference: CallReferenceFact
      readonly arguments: ReadonlyArray<ArgumentFact>
      readonly mappings: ReadonlyArray<ArgumentMappingFact>
      readonly contract: CallContractFact
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }

/** A deterministic argument identity within one caller and concrete call site. */
export interface ArgumentId {
  readonly _tag: 'ArgumentId'
  readonly function: DeclarationId
  readonly callSpan: SourceSpan.SourceSpan
  readonly ordinal: number
}

/** One ordered, syntax-owned call argument. */
export interface ArgumentFact {
  readonly _tag: 'Argument'
  readonly id: ArgumentId
  readonly expression: ExpressionFact
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One positional argument-to-parameter relationship. */
export interface ArgumentMappingFact {
  readonly _tag: 'ArgumentMapping'
  readonly argument: ArgumentFact
  readonly parameter: ParameterFact
}

/** Why a call contract cannot be established. */
export type UnavailableCallContractReason =
  | { readonly _tag: 'UnavailableCallSyntax'; readonly syntax: SyntaxTree.Node }
  | { readonly _tag: 'UnavailableCallTarget'; readonly reference: CallReferenceFact }
  | { readonly _tag: 'UnavailableMappedType'; readonly mapping: ArgumentMappingFact }

/** The complete positional contract outcome for one call. */
export type CallContractFact =
  | {
      readonly _tag: 'Compatible'
      readonly expectedCount: number
      readonly actualCount: number
    }
  | {
      readonly _tag: 'ArityMismatch'
      readonly expectedCount: number
      readonly actualCount: number
    }
  | {
      readonly _tag: 'Unavailable'
      readonly reason: UnavailableCallContractReason
    }

/** Whether one returned expression is known to match its declared result type. */
export type ReturnCompatibility = { readonly _tag: 'Compatible' } | { readonly _tag: 'Unavailable' }

/** One public function declaration and its syntax-owned semantic facts. */
export interface DeclarationFact {
  readonly _tag: 'FunctionDeclaration'
  readonly id: DeclarationId
  readonly visibility: 'Public'
  readonly parameterCount: number
  readonly parameters: ReadonlyArray<ParameterFact>
  readonly name: DeclaredName
  readonly returnType: ReturnTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One function's declaration, returned expression, and compatibility facts. */
export interface FunctionFact {
  readonly _tag: 'FunctionFact'
  readonly declaration: DeclarationFact
  readonly returnedExpression: ExpressionFact
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
  readonly fact: DeclaredTypeFact
  readonly diagnostics: ReadonlyArray<SemanticDiagnostic.SemanticDiagnostic>
}

const analyzeDeclaredType = (
  source: SourceFile.SourceFile,
  token: Token.Token | undefined,
  syntax: SyntaxTree.Element,
): ReturnTypeResult => {
  if (token === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Unavailable',
        syntax,
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
        syntax,
      }),
      diagnostics: Object.freeze([]),
    })
  }

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Unresolved',
      spelling: tokenSpelling,
      token,
      syntax,
    }),
    diagnostics: Object.freeze([SemanticDiagnostic.unknownType(tokenSpelling, token.span)]),
  })
}

const analyzeReturnType = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): ReturnTypeResult =>
  analyzeDeclaredType(
    source,
    directToken(node, 'Identifier'),
    directToken(node, 'Identifier') === undefined ? unavailableSyntax(node, 'Identifier') : node,
  )

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

interface ExpressionResult {
  readonly fact: ExpressionFact
  readonly diagnostics: ReadonlyArray<SemanticDiagnostic.SemanticDiagnostic>
  readonly type: SemanticType | undefined
}

interface IdentifierResult {
  readonly fact: IdentifierExpressionFact
  readonly diagnostics: ReadonlyArray<SemanticDiagnostic.SemanticDiagnostic>
  readonly type: SemanticType | undefined
  readonly syntax: SyntaxTree.Node
}

interface ArgumentsResult {
  readonly facts: ReadonlyArray<ArgumentFact>
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

const analyzeIdentifier = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  parameters: ReadonlyArray<ParameterFact>,
): IdentifierResult => {
  const token = directToken(node, 'Identifier')
  if (token === undefined || !node.children.every(isAvailableSyntax)) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Identifier',
        reference: Object.freeze({
          _tag: 'Unavailable',
          syntax:
            token === undefined
              ? unavailableSyntax(node, 'Identifier')
              : unavailableElement(node.children, node),
        }),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
      type: undefined,
      syntax: node,
    })
  }

  const tokenSpelling = spelling(source, token)
  const lookup = lookupParameter(parameters, tokenSpelling)
  const reference: ParameterReferenceFact =
    lookup._tag === 'Resolved'
      ? Object.freeze({
          _tag: 'Resolved',
          spelling: tokenSpelling,
          token,
          parameter: lookup.parameter,
        })
      : lookup._tag === 'Ambiguous'
        ? Object.freeze({
            _tag: 'Ambiguous',
            spelling: tokenSpelling,
            token,
            parameters: lookup.parameters,
          })
        : Object.freeze({
            _tag: 'Missing',
            spelling: tokenSpelling,
            token,
          })
  const expressionType =
    reference._tag === 'Resolved' && reference.parameter.declaredType._tag === 'Resolved'
      ? availableI32ExpressionType
      : unavailableExpressionType

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Identifier',
      reference,
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze(
      reference._tag === 'Missing'
        ? [SemanticDiagnostic.unknownParameterReference(reference.spelling, reference.token.span)]
        : [],
    ),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
    syntax: node,
  })
}

function analyzeArguments(
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
): ArgumentsResult {
  const argumentList = childNode(call, 'ArgumentList')
  const analyzed = argumentList.children.flatMap((element): ReadonlyArray<ExpressionResult> => {
    if (!SyntaxTree.isNode(element)) return []
    if (element.kind !== 'CallExpression' && !isAvailableSyntax(element)) return []
    const result = analyzeExpression(source, element, declarations, declaration)
    return result === undefined ? [] : [result]
  })
  const facts = analyzed.map((result, ordinal): ArgumentFact => {
    const expression = result.fact
    return Object.freeze({
      _tag: 'Argument',
      id: Object.freeze({
        _tag: 'ArgumentId',
        function: declaration.id,
        callSpan: call.span,
        ordinal,
      }),
      expression,
      type: expression.type,
      syntax: expression.syntax,
    })
  })

  return Object.freeze({
    facts: Object.freeze(facts),
    diagnostics: Object.freeze(analyzed.flatMap((result) => result.diagnostics)),
  })
}

interface CallContractResult {
  readonly mappings: ReadonlyArray<ArgumentMappingFact>
  readonly fact: CallContractFact
  readonly diagnostics: ReadonlyArray<SemanticDiagnostic.SemanticDiagnostic>
}

const hasAvailableCallSyntax = (call: SyntaxTree.Node): boolean => {
  const argumentList = childNode(call, 'ArgumentList')
  const callHeadAvailable = call.children.every(
    (element) =>
      (SyntaxTree.isNode(element) && element.kind === 'ArgumentList') || isAvailableSyntax(element),
  )
  const listStructureAvailable = argumentList.children.every(
    (element) =>
      (SyntaxTree.isNode(element) && element.kind === 'CallExpression') ||
      isAvailableSyntax(element),
  )
  return callHeadAvailable && listStructureAvailable
}

const analyzeCallContract = (
  call: SyntaxTree.Node,
  reference: CallReferenceFact,
  argumentsList: ReadonlyArray<ArgumentFact>,
): CallContractResult => {
  if (!hasAvailableCallSyntax(call)) {
    return Object.freeze({
      mappings: Object.freeze([]),
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
      }),
      diagnostics: Object.freeze([]),
    })
  }
  if (reference._tag !== 'Resolved') {
    return Object.freeze({
      mappings: Object.freeze([]),
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({ _tag: 'UnavailableCallTarget', reference }),
      }),
      diagnostics: Object.freeze([]),
    })
  }

  const parameters = reference.declaration.parameters
  const mappings = Object.freeze(
    argumentsList.flatMap((argument, ordinal): ReadonlyArray<ArgumentMappingFact> => {
      const parameter = parameters.at(ordinal)
      return parameter === undefined
        ? []
        : [Object.freeze({ _tag: 'ArgumentMapping', argument, parameter })]
    }),
  )
  const unavailableMapping = mappings.find(
    (mapping) =>
      mapping.argument.type._tag !== 'Available' ||
      mapping.parameter.declaredType._tag !== 'Resolved',
  )
  if (unavailableMapping !== undefined) {
    return Object.freeze({
      mappings,
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({
          _tag: 'UnavailableMappedType',
          mapping: unavailableMapping,
        }),
      }),
      diagnostics: Object.freeze([]),
    })
  }

  const expectedCount = parameters.length
  const actualCount = argumentsList.length
  if (expectedCount !== actualCount) {
    return Object.freeze({
      mappings,
      fact: Object.freeze({ _tag: 'ArityMismatch', expectedCount, actualCount }),
      diagnostics: Object.freeze([
        SemanticDiagnostic.wrongCallArity(
          reference.declaration.id,
          expectedCount,
          actualCount,
          call.span,
        ),
      ]),
    })
  }

  return Object.freeze({
    mappings,
    fact: Object.freeze({ _tag: 'Compatible', expectedCount, actualCount }),
    diagnostics: Object.freeze([]),
  })
}

function analyzeExpression(
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
): ExpressionResult | undefined {
  if (node.kind === 'IntegerLiteralExpression') {
    const integer = analyzeInteger(source, node)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Integer',
        integer: integer.fact,
        type:
          integer.fact._tag === 'Available'
            ? availableI32ExpressionType
            : unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: integer.diagnostics,
      type: integer.fact._tag === 'Available' ? integer.fact.type : undefined,
    })
  }

  if (node.kind === 'IdentifierExpression') {
    return analyzeIdentifier(source, node, declaration.parameters)
  }

  if (node.kind !== 'CallExpression') return undefined

  const argumentsResult = analyzeArguments(source, node, declarations, declaration)

  const token = directToken(node, 'Identifier')
  if (token === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Call',
        reference: Object.freeze({
          _tag: 'Unavailable',
          syntax: unavailableSyntax(node, 'Identifier'),
        }),
        arguments: argumentsResult.facts,
        mappings: Object.freeze([]),
        contract: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({
            _tag: 'UnavailableCallSyntax',
            syntax: node,
          }),
        }),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: argumentsResult.diagnostics,
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
  const callContract = analyzeCallContract(node, reference, argumentsResult.facts)
  const syntaxAvailable = hasAvailableCallSyntax(node)
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
      arguments: argumentsResult.facts,
      mappings: callContract.mappings,
      contract: callContract.fact,
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      ...(reference._tag === 'Missing'
        ? [SemanticDiagnostic.unknownFunction(reference.spelling, reference.token.span)]
        : []),
      ...argumentsResult.diagnostics,
      ...callContract.diagnostics,
    ]),
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

function isAvailableSyntax(element: SyntaxTree.Element): boolean {
  return (
    !SyntaxTree.isMissingToken(element) &&
    !(
      SyntaxTree.isNode(element) &&
      (element.kind === 'Error' || !element.children.every(isAvailableSyntax))
    )
  )
}

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

interface ParameterResult {
  readonly fact: ParameterFact
  readonly diagnostics: ReadonlyArray<SemanticDiagnostic.SemanticDiagnostic>
}

const isSeparator = (element: SyntaxTree.Element, kind: Token.TokenKind): boolean =>
  (SyntaxTree.isToken(element) && element.kind === kind) ||
  (SyntaxTree.isMissingToken(element) && element.expected === kind)

const identifierToken = (elements: ReadonlyArray<SyntaxTree.Element>): Token.Token | undefined =>
  elements.every(isAvailableSyntax)
    ? elements.find(
        (element): element is Token.Token =>
          SyntaxTree.isToken(element) && element.kind === 'Identifier',
      )
    : undefined

const unavailableElement = (
  elements: ReadonlyArray<SyntaxTree.Element>,
  fallback: SyntaxTree.Node,
): SyntaxTree.Element =>
  elements.find((element) => SyntaxTree.isMissingToken(element) || !isAvailableSyntax(element)) ??
  fallback

const analyzeParameter = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  functionId: DeclarationId,
  ordinal: number,
): ParameterResult => {
  const colonIndex = node.children.findIndex((element) => isSeparator(element, 'Colon'))
  const nameElements = colonIndex < 0 ? node.children : node.children.slice(0, colonIndex)
  const typeElements = colonIndex < 0 ? Object.freeze([]) : node.children.slice(colonIndex + 1)
  const nameToken = identifierToken(nameElements)
  const typeToken = identifierToken(typeElements)
  const name: DeclaredName =
    nameToken === undefined
      ? Object.freeze({
          _tag: 'Unavailable',
          syntax: unavailableElement(nameElements, node),
        })
      : Object.freeze({
          _tag: 'Present',
          spelling: spelling(source, nameToken),
          token: nameToken,
        })
  const declaredType = analyzeDeclaredType(
    source,
    typeToken,
    typeToken ?? unavailableElement(typeElements, node),
  )

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'ParameterDeclaration',
      id: Object.freeze({
        _tag: 'ParameterId',
        function: functionId,
        ordinal,
      }),
      name,
      declaredType: declaredType.fact,
      syntax: node,
    }),
    diagnostics: declaredType.diagnostics,
  })
}

const analyzeDeclarationHeader = (
  source: SourceFile.SourceFile,
  functionNode: SyntaxTree.Node,
  ordinal: number,
): DeclarationHeader => {
  const returnTypeNode = childNode(functionNode, 'ReturnType')
  const parameterListNode = childNode(functionNode, 'ParameterList')
  const returnType = analyzeReturnType(source, returnTypeNode)
  const id: DeclarationId = Object.freeze({
    _tag: 'DeclarationId',
    sourceId: source.id,
    ordinal,
  })
  const analyzedParameters = directChildNodes(parameterListNode, 'ParameterDeclaration').map(
    (node, parameterOrdinal) => analyzeParameter(source, node, id, parameterOrdinal),
  )
  const parameters = Object.freeze(analyzedParameters.map((result) => result.fact))
  const declaration: DeclarationFact = Object.freeze({
    _tag: 'FunctionDeclaration',
    id,
    visibility: 'Public',
    parameterCount: parameters.length,
    parameters,
    name: presentName(source, functionNode),
    returnType: returnType.fact,
    syntax: functionNode,
  })

  return Object.freeze({
    node: functionNode,
    declaration,
    diagnostics: Object.freeze([
      ...analyzedParameters.flatMap((result) => result.diagnostics),
      ...duplicateParameterDiagnostics(parameters),
      ...returnType.diagnostics,
    ]),
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
  const expression = analyzeExpression(source, expressionNode, declarations, header.declaration)
  if (expression === undefined) {
    throw new RangeError(`Semantic analysis cannot analyze ${expressionNode.kind}`)
  }
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

interface PresentParameterNameEntry {
  readonly spelling: string
  readonly token: Token.Token
  readonly parameter: ParameterFact
}

const presentParameterNameEntries = (
  parameters: ReadonlyArray<ParameterFact>,
): ReadonlyArray<PresentParameterNameEntry> =>
  Object.freeze(
    parameters.flatMap((parameter): ReadonlyArray<PresentParameterNameEntry> => {
      const name = parameter.name
      return name._tag === 'Present'
        ? [
            Object.freeze({
              spelling: name.spelling,
              token: name.token,
              parameter,
            }),
          ]
        : []
    }),
  )

const duplicateParameterDiagnostics = (
  parameters: ReadonlyArray<ParameterFact>,
): ReadonlyArray<SemanticDiagnostic.SemanticDiagnostic> => {
  const firstBySpelling = new Map<string, PresentParameterNameEntry>()
  let diagnostics: ReadonlyArray<SemanticDiagnostic.SemanticDiagnostic> = Object.freeze([])

  for (const entry of presentParameterNameEntries(parameters)) {
    const original = firstBySpelling.get(entry.spelling)
    if (original === undefined) {
      firstBySpelling.set(entry.spelling, entry)
    } else {
      diagnostics = Object.freeze([
        ...diagnostics,
        SemanticDiagnostic.duplicateParameterName(
          entry.spelling,
          original.token.span,
          entry.token.span,
        ),
      ])
    }
  }

  return diagnostics
}

const lookupParameter = (
  parameters: ReadonlyArray<ParameterFact>,
  spelling: string,
): ParameterLookup => {
  const matches = presentParameterNameEntries(parameters)
    .filter((entry) => entry.spelling === spelling)
    .map((entry) => entry.parameter)
  const first = matches.at(0)

  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling })
  if (matches.length === 1) {
    return Object.freeze({ _tag: 'Resolved', spelling, parameter: first })
  }
  return Object.freeze({
    _tag: 'Ambiguous',
    spelling,
    parameters: Object.freeze(matches),
  })
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

/** Looks up every present parameter with the exact requested spelling in one function. */
export const parameterByName = dual<
  (spelling: string) => (self: DeclarationFact) => ParameterLookup,
  (self: DeclarationFact, spelling: string) => ParameterLookup
>(2, (self, spelling) => lookupParameter(self.parameters, spelling))
