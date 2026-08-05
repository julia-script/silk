import { dual } from 'effect/Function'
import * as Option from 'effect/Option'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Hir from './Hir.js'
import * as SourceFile from './SourceFile.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'

/** The only semantic type recognized by the first analysis slice. */
export type SemanticType = DeclarationIndex.SemanticType

/** A deterministic declaration identity local to one analyzed source snapshot. */
export type DeclarationId = DeclarationIndex.DeclarationId

/** A deterministic parameter identity nested under its owning function declaration. */
export type ParameterId = DeclarationIndex.ParameterId

/** A declaration name supplied by syntax or explicitly unavailable after recovery. */
export type DeclaredName = DeclarationIndex.DeclaredName

/** The resolved, unresolved, or syntax-unavailable declared return type. */
export type DeclaredTypeFact = DeclarationIndex.DeclaredTypeFact

/** The declared type fact attached to a function return. */
export type ReturnTypeFact = DeclarationIndex.ReturnTypeFact

/** One ordered parameter declaration with exact concrete provenance. */
export type ParameterFact = DeclarationIndex.ParameterFact

/** The closed result of looking up a parameter spelling within one function. */
export type ParameterLookup = DeclarationIndex.ParameterLookup

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
      readonly cause?: Diagnostic.Identity
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
      readonly cause?: Diagnostic.Identity
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
      readonly cause?: Diagnostic.Identity
    }

/** Whether one returned expression is known to match its declared result type. */
export type ReturnCompatibility = { readonly _tag: 'Compatible' } | { readonly _tag: 'Unavailable' }

/** One public function declaration and its syntax-owned semantic facts. */
export type DeclarationFact = DeclarationIndex.DeclarationFact

/** One function's declaration, returned expression, and compatibility facts. */
export interface FunctionFact {
  readonly _tag: 'FunctionFact'
  readonly declaration: DeclarationFact
  readonly returnedExpression: ExpressionFact
  readonly returnCompatibility: ReturnCompatibility
}

/** The closed result of looking up one declaration spelling. */
export type DeclarationLookup = DeclarationIndex.DeclarationLookup

/** The complete deterministic elaboration result for all direct bootstrap declarations. */
export interface Result {
  readonly _tag: 'Elaboration'
  readonly syntax: SyntaxFile.SyntaxFile
  readonly functions: ReadonlyArray<FunctionFact>
  readonly hir: Hir.Module
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
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
  const child = SyntaxTree.directNode(parent, kind)
  if (child === undefined) {
    throw new RangeError(`Semantic analysis expected ${kind} below ${parent.kind}`)
  }
  return child
}

const directToken = SyntaxTree.directToken

const unavailableSyntax = SyntaxTree.unavailableChild

const isAvailableSyntax = SyntaxTree.isAvailableSyntax

const unavailableElement = SyntaxTree.unavailableElement

const lookupParameter = DeclarationIndex.lookupParameter

const lookupDeclaration = DeclarationIndex.lookupDeclaration

const spelling = (source: SourceFile.SourceFile, token: Token.Token): string =>
  Option.getOrThrowWith(
    SourceFile.spelling(source, token.span),
    () => new RangeError(`Semantic token span does not belong to source ${source.id}`),
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
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

interface ExpressionResult {
  readonly fact: ExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly type: SemanticType | undefined
}

interface IdentifierResult {
  readonly fact: IdentifierExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly type: SemanticType | undefined
  readonly syntax: SyntaxTree.Node
}

interface ArgumentsResult {
  readonly facts: ReadonlyArray<ArgumentFact>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
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
    diagnostics: Object.freeze([Diagnostic.integerOutOfRange(tokenSpelling, token.span)]),
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
  const missingDiagnostic =
    lookup._tag === 'Missing'
      ? Diagnostic.unknownParameterReference(tokenSpelling, token.span)
      : undefined
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
            ...(missingDiagnostic === undefined
              ? {}
              : { cause: Diagnostic.identity(missingDiagnostic) }),
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
    diagnostics: Object.freeze(missingDiagnostic === undefined ? [] : [missingDiagnostic]),
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
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
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
    const cause = reference._tag === 'Missing' ? reference.cause : undefined
    return Object.freeze({
      mappings: Object.freeze([]),
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({ _tag: 'UnavailableCallTarget', reference }),
        ...(cause === undefined ? {} : { cause }),
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
        Diagnostic.wrongCallArity(reference.declaration.id, expectedCount, actualCount, call.span),
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
  const missingDiagnostic =
    lookup._tag === 'Missing' ? Diagnostic.unknownFunction(tokenSpelling, token.span) : undefined
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
            ...(missingDiagnostic === undefined
              ? {}
              : { cause: Diagnostic.identity(missingDiagnostic) }),
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
      ...(missingDiagnostic === undefined ? [] : [missingDiagnostic]),
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

const compareDiagnostics = (left: Diagnostic.Diagnostic, right: Diagnostic.Diagnostic): number =>
  left.span.start - right.span.start ||
  left.span.end - right.span.end ||
  (left.code < right.code ? -1 : left.code > right.code ? 1 : 0)

interface FunctionAnalysis {
  readonly fact: FunctionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const analyzeFunctionBody = (
  source: SourceFile.SourceFile,
  declaration: DeclarationFact,
  declarations: ReadonlyArray<DeclarationFact>,
): FunctionAnalysis => {
  const blockNode = childNode(declaration.syntax, 'Block')
  const returnStatementNode = childNode(blockNode, 'ReturnStatement')
  const expressionNode = returnedExpressionNode(returnStatementNode)
  const expression = analyzeExpression(source, expressionNode, declarations, declaration)
  if (expression === undefined) {
    throw new RangeError(`Semantic analysis cannot analyze ${expressionNode.kind}`)
  }
  const returnCompatibility =
    declaration.returnType._tag === 'Resolved' && expression.type === declaration.returnType.type
      ? compatible
      : unavailableCompatibility

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FunctionFact',
      declaration,
      returnedExpression: expression.fact,
      returnCompatibility,
    }),
    diagnostics: expression.diagnostics,
  })
}

const hirExpression = (fact: ExpressionFact): Hir.Expression => {
  if (fact._tag === 'Integer') {
    return fact.integer._tag === 'Available'
      ? Object.freeze({
          _tag: 'IntegerLiteral',
          value: fact.integer.value,
          type: fact.integer.type,
          span: fact.syntax.span,
        })
      : Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
  }
  if (fact._tag === 'Identifier') {
    if (fact.reference._tag === 'Resolved' && fact.type._tag === 'Available') {
      return Object.freeze({
        _tag: 'ParameterReference',
        parameter: fact.reference.parameter.id,
        type: fact.type.type,
        span: fact.syntax.span,
      })
    }
    return Object.freeze({
      _tag: 'Unavailable',
      span: fact.syntax.span,
      ...(fact.reference._tag === 'Missing' && fact.reference.cause !== undefined
        ? { cause: fact.reference.cause }
        : {}),
    })
  }
  if (
    fact.reference._tag === 'Resolved' &&
    fact.reference.declaration.canonical._tag === 'Canonical' &&
    fact.contract._tag === 'Compatible' &&
    fact.type._tag === 'Available'
  ) {
    return Object.freeze({
      _tag: 'Call',
      target: fact.reference.declaration.canonical.id,
      arguments: Object.freeze(
        fact.arguments.map((argument) => hirExpression(argument.expression)),
      ),
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  const cause =
    fact.reference._tag === 'Missing'
      ? fact.reference.cause
      : fact.contract._tag === 'Unavailable'
        ? fact.contract.cause
        : undefined
  return Object.freeze({
    _tag: 'Unavailable',
    span: fact.syntax.span,
    ...(cause === undefined ? {} : { cause }),
  })
}

/** Elaborates every declaration body into immutable facts and the module's HIR. */
export const elaborateModule = (syntax: SyntaxFile.SyntaxFile): Result => {
  const source = syntax.source
  const headers = DeclarationIndex.collectModule(syntax)
  const declarations = headers.declarations
  const analyzed = declarations.map((declaration) =>
    analyzeFunctionBody(source, declaration, declarations),
  )
  const functions = Object.freeze(analyzed.map((result) => result.fact))
  const diagnostics = [
    ...headers.diagnostics,
    ...analyzed.flatMap((result) => result.diagnostics),
  ].sort(compareDiagnostics)
  const hir: Hir.Module = Object.freeze({
    _tag: 'HirModule',
    module: source.id,
    functions: Object.freeze(
      functions.map((fact) =>
        Object.freeze({
          _tag: 'HirFunction' as const,
          declaration: fact.declaration,
          contract: Hir.contractOf(fact.declaration),
          body: hirExpression(fact.returnedExpression),
        }),
      ),
    ),
  })

  return Object.freeze({
    _tag: 'Elaboration',
    syntax,
    functions,
    hir,
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
