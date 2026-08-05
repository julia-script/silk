import { dual } from 'effect/Function'
import * as Option from 'effect/Option'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Hir from './Hir.js'
import * as NameResolution from './NameResolution.js'
import * as Operator from './Operator.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
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

/** One `let` binding declaration with its inferred type and initializer facts. */
export interface BindingDeclarationFact {
  readonly _tag: 'BindingFact'
  readonly id: Hir.BindingId
  readonly name: DeclaredName
  readonly inferredType: ExpressionTypeFact
  readonly initializer: ExpressionFact
  readonly syntax: SyntaxTree.Node
}

/** A bare identifier resolved against enclosing parameters and preceding bindings. */
export type ParameterReferenceFact =
  | {
      readonly _tag: 'Resolved'
      readonly spelling: string
      readonly token: Token.Token
      readonly parameter: ParameterFact
    }
  | {
      readonly _tag: 'ResolvedBinding'
      readonly spelling: string
      readonly token: Token.Token
      readonly binding: BindingDeclarationFact
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
      readonly _tag: 'ResolvedBuiltin'
      readonly spelling: string
      readonly token: Token.Token
      readonly actor: string
      readonly operation: Hir.BuiltinOperation
      readonly parameters: ReadonlyArray<SemanticType>
      readonly result: SemanticType
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
      readonly cause?: Diagnostic.Identity
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

/** One `move <name>` expression with its consuming reference fact. */
export interface MoveExpressionFact {
  readonly _tag: 'Move'
  readonly reference: ParameterReferenceFact
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One `true`/`false` literal expression fact. */
export interface BooleanExpressionFact {
  readonly _tag: 'Boolean'
  readonly value: boolean
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One parenthesized expression retaining its concrete grouping. */
export interface GroupedExpressionFact {
  readonly _tag: 'Grouped'
  readonly expression: ExpressionFact
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One compiler-known operand-to-parameter relationship. */
export interface BuiltinArgumentMappingFact {
  readonly _tag: 'BuiltinArgumentMapping'
  readonly argument: ArgumentFact
  readonly ordinal: number
  readonly expected: SemanticType
}

/** One prefix or infix operator and its canonical builtin resolution. */
export interface OperatorExpressionFact {
  readonly _tag: 'Operator'
  readonly operator: Operator.Prefix | Operator.Infix
  readonly reference: CallReferenceFact
  readonly arguments: ReadonlyArray<ArgumentFact>
  readonly mappings: ReadonlyArray<BuiltinArgumentMappingFact>
  readonly contract: CallContractFact
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One qualified pipeline with its inserted first argument and canonical call resolution. */
export interface PipelineExpressionFact {
  readonly _tag: 'Pipeline'
  readonly input: ExpressionFact
  readonly target: SyntaxTree.Node
  readonly reference: CallReferenceFact
  readonly arguments: ReadonlyArray<ArgumentFact>
  readonly mappings: ReadonlyArray<ArgumentMappingFact | BuiltinArgumentMappingFact>
  readonly contract: CallContractFact
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
  | BooleanExpressionFact
  | IdentifierExpressionFact
  | MoveExpressionFact
  | GroupedExpressionFact
  | OperatorExpressionFact
  | PipelineExpressionFact
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
  | { readonly _tag: 'UnavailableBuiltinArgument'; readonly argument: ArgumentFact }
  | {
      readonly _tag: 'ArgumentTypeMismatch'
      readonly argument: ArgumentFact
      readonly expected: SemanticType
    }

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

/** One analyzed body statement in source order, nesting through conditionals. */
export type StatementFact =
  | { readonly _tag: 'BindStatement'; readonly binding: BindingDeclarationFact }
  | {
      readonly _tag: 'IfStatement'
      readonly condition: ExpressionFact
      readonly taken: ReadonlyArray<StatementFact>
      readonly otherwise: ReadonlyArray<StatementFact>
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'ReturnStatement'
      readonly expression: ExpressionFact
      readonly syntax: SyntaxTree.Node
    }

/** One function's declaration, statements, bindings, and compatibility facts. */
export interface FunctionFact {
  readonly _tag: 'FunctionFact'
  readonly declaration: DeclarationFact
  readonly statements: ReadonlyArray<StatementFact>
  readonly bindings: ReadonlyArray<BindingDeclarationFact>
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
const availableBoolExpressionType: ExpressionTypeFact = Object.freeze({
  _tag: 'Available',
  type: 'Bool',
})
const availableExpressionType = (type: SemanticType): ExpressionTypeFact =>
  type === 'I32' ? availableI32ExpressionType : availableBoolExpressionType
const unavailableExpressionType: ExpressionTypeFact = Object.freeze({ _tag: 'Unavailable' })

const expressionNodeKinds: ReadonlyArray<SyntaxTree.NodeKind> = Object.freeze([
  'IntegerLiteralExpression',
  'BooleanLiteralExpression',
  'IdentifierExpression',
  'MoveExpression',
  'CallExpression',
  'GroupedExpression',
  'PrefixExpression',
  'InfixExpression',
  'PipelineExpression',
])

const isExpressionNode = (element: SyntaxTree.Element): element is SyntaxTree.Node =>
  SyntaxTree.isNode(element) && expressionNodeKinds.includes(element.kind)

const isRecursiveArgumentNode = (element: SyntaxTree.Element): element is SyntaxTree.Node =>
  isExpressionNode(element) &&
  (element.kind === 'CallExpression' ||
    element.kind === 'MoveExpression' ||
    element.kind === 'GroupedExpression' ||
    element.kind === 'PrefixExpression' ||
    element.kind === 'InfixExpression' ||
    element.kind === 'PipelineExpression' ||
    SyntaxTree.isAvailableSyntax(element))

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

const signedI32Value = (bytes: Uint8Array, negative: boolean): Option.Option<number> => {
  const limit = negative ? 2147483648 : i32Maximum
  let value = 0
  for (const byte of bytes) {
    const digit = byte - 0x30
    if (value > Math.floor((limit - digit) / 10)) return Option.none()
    value = value * 10 + digit
  }
  return Option.some(negative ? -value : value)
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

const argumentFact = (
  declaration: DeclarationFact,
  callSpan: SourceSpan.SourceSpan,
  expression: ExpressionFact,
  ordinal: number,
): ArgumentFact =>
  Object.freeze({
    _tag: 'Argument',
    id: Object.freeze({
      _tag: 'ArgumentId',
      function: declaration.id,
      callSpan,
      ordinal,
    }),
    expression,
    type: expression.type,
    syntax: expression.syntax,
  })

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

  const minusToken = directToken(node, 'Minus')
  const negative = minusToken !== undefined
  const literalSpan =
    minusToken === undefined
      ? token.span
      : Option.getOrElse(
          SourceSpan.make(source, minusToken.span.start, token.span.end),
          () => token.span,
        )
  const bytes = Option.getOrThrowWith(
    SourceFile.slice(source, token.span),
    () => new RangeError(`Semantic integer span does not belong to source ${source.id}`),
  )
  const value = signedI32Value(bytes, negative)
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

  const digits = Array.from(bytes, (byte) => String.fromCharCode(byte)).join('')
  const tokenSpelling = negative ? `-${digits}` : digits
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'OutOfRange',
      type: 'I32',
      spelling: tokenSpelling,
      token,
      syntax: node,
    }),
    diagnostics: Object.freeze([Diagnostic.integerOutOfRange(tokenSpelling, literalSpan)]),
  })
}

/** The value names visible at one body position: parameters plus completed bindings. */
interface Scope {
  readonly parameters: ReadonlyArray<ParameterFact>
  readonly bindings: ReadonlyArray<BindingDeclarationFact>
}

interface ValueResolution {
  readonly reference: ParameterReferenceFact
  readonly type: ExpressionTypeFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const resolveValueName = (
  scope: Scope,
  tokenSpelling: string,
  token: Token.Token,
): ValueResolution => {
  const lookup = lookupParameter(scope.parameters, tokenSpelling)
  if (lookup._tag === 'Resolved') {
    return Object.freeze({
      reference: Object.freeze({
        _tag: 'Resolved' as const,
        spelling: tokenSpelling,
        token,
        parameter: lookup.parameter,
      }),
      type:
        lookup.parameter.declaredType._tag === 'Resolved'
          ? availableExpressionType(lookup.parameter.declaredType.type)
          : unavailableExpressionType,
      diagnostics: Object.freeze([]),
    })
  }
  if (lookup._tag === 'Ambiguous') {
    return Object.freeze({
      reference: Object.freeze({
        _tag: 'Ambiguous' as const,
        spelling: tokenSpelling,
        token,
        parameters: lookup.parameters,
      }),
      type: unavailableExpressionType,
      diagnostics: Object.freeze([]),
    })
  }
  const binding = scope.bindings.find(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === tokenSpelling,
  )
  if (binding !== undefined) {
    return Object.freeze({
      reference: Object.freeze({
        _tag: 'ResolvedBinding' as const,
        spelling: tokenSpelling,
        token,
        binding,
      }),
      type: binding.inferredType,
      diagnostics: Object.freeze([]),
    })
  }
  const missingDiagnostic = Diagnostic.unknownParameterReference(tokenSpelling, token.span)
  return Object.freeze({
    reference: Object.freeze({
      _tag: 'Missing' as const,
      spelling: tokenSpelling,
      token,
      cause: Diagnostic.identity(missingDiagnostic),
    }),
    type: unavailableExpressionType,
    diagnostics: Object.freeze([missingDiagnostic]),
  })
}

const analyzeIdentifier = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  scope: Scope,
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

  const resolution = resolveValueName(scope, spelling(source, token), token)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Identifier',
      reference: resolution.reference,
      type: resolution.type,
      syntax: node,
    }),
    diagnostics: resolution.diagnostics,
    type: resolution.type._tag === 'Available' ? resolution.type.type : undefined,
    syntax: node,
  })
}

interface MoveResult {
  readonly fact: MoveExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly type: SemanticType | undefined
}

const analyzeMove = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  scope: Scope,
): MoveResult => {
  const token = directToken(node, 'Identifier')
  if (token === undefined || !node.children.every(isAvailableSyntax)) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Move',
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
    })
  }

  const resolution = resolveValueName(scope, spelling(source, token), token)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Move',
      reference: resolution.reference,
      type: resolution.type,
      syntax: node,
    }),
    diagnostics: resolution.diagnostics,
    type: resolution.type._tag === 'Available' ? resolution.type.type : undefined,
  })
}

const analyzeArgumentNodes = (
  source: SourceFile.SourceFile,
  site: SyntaxTree.Node,
  nodes: ReadonlyArray<SyntaxTree.Node>,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ArgumentsResult => {
  const analyzed = nodes.flatMap((element): ReadonlyArray<ExpressionResult> => {
    const result = analyzeExpression(source, element, declarations, declaration, scope, resolution)
    return result === undefined ? [] : [result]
  })
  const facts = analyzed.map((result, ordinal) =>
    argumentFact(declaration, site.span, result.fact, ordinal),
  )

  return Object.freeze({
    facts: Object.freeze(facts),
    diagnostics: Object.freeze(analyzed.flatMap((result) => result.diagnostics)),
  })
}

function analyzeArguments(
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ArgumentsResult {
  const argumentList = childNode(call, 'ArgumentList')
  return analyzeArgumentNodes(
    source,
    call,
    argumentList.children.filter(isRecursiveArgumentNode),
    declarations,
    declaration,
    scope,
    resolution,
  )
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
    (element) => isRecursiveArgumentNode(element) || isAvailableSyntax(element),
  )
  return callHeadAvailable && listStructureAvailable
}

const analyzeCallContract = (
  call: SyntaxTree.Node,
  reference: CallReferenceFact,
  argumentsList: ReadonlyArray<ArgumentFact>,
  syntaxAvailable = hasAvailableCallSyntax(call),
): CallContractResult => {
  if (!syntaxAvailable) {
    return Object.freeze({
      mappings: Object.freeze([]),
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
      }),
      diagnostics: Object.freeze([]),
    })
  }
  if (reference._tag === 'ResolvedBuiltin') {
    const unavailableArgument = argumentsList.find((argument) => argument.type._tag !== 'Available')
    if (unavailableArgument !== undefined) {
      return Object.freeze({
        mappings: Object.freeze([]),
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({
            _tag: 'UnavailableBuiltinArgument',
            argument: unavailableArgument,
          }),
        }),
        diagnostics: Object.freeze([]),
      })
    }
    for (const [ordinal, argument] of argumentsList.entries()) {
      const expected = reference.parameters.at(ordinal)
      if (
        expected !== undefined &&
        argument.type._tag === 'Available' &&
        argument.type.type !== expected
      ) {
        const mismatch = Diagnostic.argumentTypeMismatch(
          expected,
          argument.type.type,
          argument.syntax.span,
        )
        return Object.freeze({
          mappings: Object.freeze([]),
          fact: Object.freeze({
            _tag: 'Unavailable',
            reason: Object.freeze({ _tag: 'ArgumentTypeMismatch', argument, expected }),
            cause: Diagnostic.identity(mismatch),
          }),
          diagnostics: Object.freeze([mismatch]),
        })
      }
    }
    const expectedCount = reference.parameters.length
    const actualCount = argumentsList.length
    if (expectedCount !== actualCount) {
      return Object.freeze({
        mappings: Object.freeze([]),
        fact: Object.freeze({ _tag: 'ArityMismatch', expectedCount, actualCount }),
        diagnostics: Object.freeze([
          Diagnostic.wrongCallArity(
            Object.freeze({
              _tag: 'BuiltinTarget',
              actor: reference.actor,
              operation: reference.operation,
            }),
            expectedCount,
            actualCount,
            call.span,
          ),
        ]),
      })
    }
    return Object.freeze({
      mappings: Object.freeze([]),
      fact: Object.freeze({ _tag: 'Compatible', expectedCount, actualCount }),
      diagnostics: Object.freeze([]),
    })
  }

  if (reference._tag !== 'Resolved') {
    const cause =
      reference._tag === 'Missing' || reference._tag === 'Ambiguous' ? reference.cause : undefined
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
  for (const mapping of mappings) {
    if (
      mapping.argument.type._tag === 'Available' &&
      mapping.parameter.declaredType._tag === 'Resolved' &&
      mapping.argument.type.type !== mapping.parameter.declaredType.type
    ) {
      const mismatch = Diagnostic.argumentTypeMismatch(
        mapping.parameter.declaredType.type,
        mapping.argument.type.type,
        mapping.argument.syntax.span,
      )
      return Object.freeze({
        mappings,
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({
            _tag: 'ArgumentTypeMismatch',
            argument: mapping.argument,
            expected: mapping.parameter.declaredType.type,
          }),
          cause: Diagnostic.identity(mismatch),
        }),
        diagnostics: Object.freeze([mismatch]),
      })
    }
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

interface BuiltinSignature {
  readonly operation: Hir.BuiltinOperation
  readonly parameters: ReadonlyArray<SemanticType>
  readonly result: SemanticType
}

const binaryI32 = (operation: Hir.BuiltinOperation): BuiltinSignature =>
  Object.freeze({ operation, parameters: Object.freeze(['I32', 'I32'] as const), result: 'I32' })

const comparisonI32 = (operation: Hir.BuiltinOperation): BuiltinSignature =>
  Object.freeze({ operation, parameters: Object.freeze(['I32', 'I32'] as const), result: 'Bool' })

const comparisonBool = (operation: Hir.BuiltinOperation): BuiltinSignature =>
  Object.freeze({ operation, parameters: Object.freeze(['Bool', 'Bool'] as const), result: 'Bool' })

/** The compiler-known built-in actor table. Issue 07's runtime actors extend this shape. */
const builtinActors: Readonly<
  Record<string, Readonly<Record<string, BuiltinSignature>> | undefined>
> = Object.freeze({
  I32: Object.freeze({
    negate: Object.freeze({
      operation: 'Negate' as const,
      parameters: Object.freeze(['I32'] as const),
      result: 'I32' as const,
    }),
    add: binaryI32('Add'),
    subtract: binaryI32('Subtract'),
    multiply: binaryI32('Multiply'),
    divide: binaryI32('Divide'),
    remainder: binaryI32('Remainder'),
    equals: comparisonI32('Equals'),
    notEquals: comparisonI32('NotEquals'),
    lessThan: comparisonI32('LessThan'),
    lessOrEqual: comparisonI32('LessOrEqual'),
    greaterThan: comparisonI32('GreaterThan'),
    greaterOrEqual: comparisonI32('GreaterOrEqual'),
  }),
  Bool: Object.freeze({
    equals: comparisonBool('Equals'),
    notEquals: comparisonBool('NotEquals'),
    not: Object.freeze({
      operation: 'Not' as const,
      parameters: Object.freeze(['Bool'] as const),
      result: 'Bool' as const,
    }),
  }),
})

function analyzeBuiltinCall(
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  argumentsResult: ArgumentsResult,
): ExpressionResult {
  const identifiers = call.children.filter(
    (element): element is Token.Token =>
      SyntaxTree.isToken(element) && element.kind === 'Identifier',
  )
  const actorToken = identifiers.at(0)
  const operationToken = identifiers.at(1)

  if (actorToken === undefined || operationToken === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Call',
        reference: Object.freeze({
          _tag: 'Unavailable',
          syntax: unavailableSyntax(call, 'Identifier'),
        }),
        arguments: argumentsResult.facts,
        mappings: Object.freeze([]),
        contract: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
        }),
        type: unavailableExpressionType,
        syntax: call,
      }),
      diagnostics: argumentsResult.diagnostics,
      type: undefined,
    })
  }

  const actorSpelling = spelling(source, actorToken)
  const operationSpelling = spelling(source, operationToken)
  const actor = builtinActors[actorSpelling]
  const signature = actor?.[operationSpelling]
  const missingDiagnostic =
    actor === undefined
      ? Diagnostic.unknownActor(actorSpelling, actorToken.span)
      : signature === undefined
        ? Diagnostic.unknownActorOperation(actorSpelling, operationSpelling, operationToken.span)
        : undefined
  const reference: CallReferenceFact =
    signature !== undefined
      ? Object.freeze({
          _tag: 'ResolvedBuiltin',
          spelling: `${actorSpelling}.${operationSpelling}`,
          token: operationToken,
          actor: actorSpelling,
          operation: signature.operation,
          parameters: signature.parameters,
          result: signature.result,
        })
      : Object.freeze({
          _tag: 'Missing',
          spelling: `${actorSpelling}.${operationSpelling}`,
          token: actor === undefined ? actorToken : operationToken,
          ...(missingDiagnostic === undefined
            ? {}
            : { cause: Diagnostic.identity(missingDiagnostic) }),
        })
  const callContract = analyzeCallContract(call, reference, argumentsResult.facts)
  const expressionType =
    hasAvailableCallSyntax(call) && reference._tag === 'ResolvedBuiltin'
      ? availableExpressionType(reference.result)
      : unavailableExpressionType

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Call',
      reference,
      arguments: argumentsResult.facts,
      mappings: callContract.mappings,
      contract: callContract.fact,
      type: expressionType,
      syntax: call,
    }),
    diagnostics: Object.freeze([
      ...(missingDiagnostic === undefined ? [] : [missingDiagnostic]),
      ...argumentsResult.diagnostics,
      ...callContract.diagnostics,
    ]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

const builtinArgumentMappings = (
  reference: CallReferenceFact,
  argumentsList: ReadonlyArray<ArgumentFact>,
): ReadonlyArray<BuiltinArgumentMappingFact> =>
  reference._tag !== 'ResolvedBuiltin'
    ? Object.freeze([])
    : Object.freeze(
        reference.parameters.flatMap(
          (expected, ordinal): ReadonlyArray<BuiltinArgumentMappingFact> => {
            const argument = argumentsList.at(ordinal)
            return argument === undefined
              ? []
              : [Object.freeze({ _tag: 'BuiltinArgumentMapping', argument, ordinal, expected })]
          },
        ),
      )

interface QualifiedReferenceResult {
  readonly reference: CallReferenceFact
  readonly diagnostic?: Diagnostic.Diagnostic
}

const resolveQualifiedReference = (
  source: SourceFile.SourceFile,
  qualifierToken: Token.Token,
  memberToken: Token.Token,
  resolution: ResolutionContext,
): QualifiedReferenceResult => {
  const qualifier = spelling(source, qualifierToken)
  const member = spelling(source, memberToken)
  const qualifierLookup = NameResolution.lookup(resolution.scope, qualifier)

  if (qualifierLookup._tag === 'Intrinsic') {
    const signature = builtinActors[qualifier]?.[member]
    const diagnostic =
      signature === undefined
        ? Diagnostic.unknownActorOperation(qualifier, member, memberToken.span)
        : undefined
    return Object.freeze({
      reference:
        signature === undefined
          ? Object.freeze({
              _tag: 'Missing' as const,
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              ...(diagnostic === undefined ? {} : { cause: Diagnostic.identity(diagnostic) }),
            })
          : Object.freeze({
              _tag: 'ResolvedBuiltin' as const,
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              actor: qualifier,
              operation: signature.operation,
              parameters: signature.parameters,
              result: signature.result,
            }),
      ...(diagnostic === undefined ? {} : { diagnostic }),
    })
  }

  if (qualifierLookup._tag === 'Namespace') {
    const memberLookup = DeclarationIndex.lookup(resolution.index, qualifierLookup.module, member)
    const candidate = memberLookup._tag === 'Resolved' ? memberLookup.declaration : undefined
    const diagnostic =
      candidate === undefined
        ? Diagnostic.unknownImportedMember(qualifierLookup.module, member, memberToken.span)
        : candidate.visibility === 'Private'
          ? Diagnostic.inaccessibleImportedMember(qualifierLookup.module, member, memberToken.span)
          : undefined
    return Object.freeze({
      reference:
        candidate !== undefined && candidate.visibility === 'Public'
          ? Object.freeze({
              _tag: 'Resolved' as const,
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              declaration: candidate,
            })
          : Object.freeze({
              _tag: 'Missing' as const,
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              ...(diagnostic === undefined ? {} : { cause: Diagnostic.identity(diagnostic) }),
            }),
      ...(diagnostic === undefined ? {} : { diagnostic }),
    })
  }

  const diagnostic =
    qualifierLookup._tag === 'Missing' || qualifierLookup._tag === 'Resolved'
      ? Diagnostic.unknownActor(qualifier, qualifierToken.span)
      : undefined
  const inheritedCause =
    qualifierLookup._tag === 'Unavailable'
      ? qualifierLookup.cause
      : qualifierLookup._tag === 'Conflict'
        ? qualifierLookup.conflict.cause
        : undefined
  return Object.freeze({
    reference: Object.freeze({
      _tag: 'Missing',
      spelling: `${qualifier}.${member}`,
      token: qualifierToken,
      ...(diagnostic !== undefined
        ? { cause: Diagnostic.identity(diagnostic) }
        : inheritedCause === undefined
          ? {}
          : { cause: inheritedCause }),
    }),
    ...(diagnostic === undefined ? {} : { diagnostic }),
  })
}

const analyzeGroupedExpression = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const child = node.children.find(isExpressionNode)
  const expression =
    child === undefined
      ? undefined
      : analyzeExpression(source, child, declarations, declaration, scope, resolution)
  if (expression === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Grouped',
        expression: Object.freeze({
          _tag: 'Integer',
          integer: Object.freeze({ _tag: 'Unavailable', syntax: node }),
          type: unavailableExpressionType,
          syntax: node,
        }),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
      type: undefined,
    })
  }
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Grouped',
      expression: expression.fact,
      type: expression.fact.type,
      syntax: node,
    }),
    diagnostics: expression.diagnostics,
    type: expression.type,
  })
}

const analyzeOperatorExpression = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const operatorToken = node.children.find(
    (element): element is Token.Token =>
      SyntaxTree.isToken(element) &&
      (node.kind === 'PrefixExpression'
        ? Operator.prefix(element.kind) !== undefined
        : Operator.infix(element.kind) !== undefined),
  )
  const operator =
    operatorToken === undefined
      ? undefined
      : node.kind === 'PrefixExpression'
        ? Operator.prefix(operatorToken.kind)
        : Operator.infix(operatorToken.kind)?.operator
  const argumentsResult = analyzeArgumentNodes(
    source,
    node,
    node.children.filter(isExpressionNode),
    declarations,
    declaration,
    scope,
    resolution,
  )
  if (operator === undefined || operatorToken === undefined) {
    const reference: CallReferenceFact = Object.freeze({
      _tag: 'Unavailable',
      syntax: unavailableSyntax(node, node.kind === 'PrefixExpression' ? 'Minus' : 'Plus'),
    })
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Operator',
        operator: node.kind === 'PrefixExpression' ? 'Negate' : 'Add',
        reference,
        arguments: argumentsResult.facts,
        mappings: Object.freeze([]),
        contract: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: node }),
        }),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: argumentsResult.diagnostics,
      type: undefined,
    })
  }

  const firstType = argumentsResult.facts.at(0)?.type
  const equalityActor =
    firstType?._tag === 'Available' && firstType.type === 'Bool' ? 'Bool' : 'I32'
  const target = Operator.target(operator, equalityActor)
  const signature = builtinActors[target.actor]?.[target.operation]
  if (signature === undefined) throw new RangeError('Compiler operator table is inconsistent')
  const reference: CallReferenceFact = Object.freeze({
    _tag: 'ResolvedBuiltin',
    spelling: `${target.actor}.${target.operation}`,
    token: operatorToken,
    actor: target.actor,
    operation: signature.operation,
    parameters: signature.parameters,
    result: signature.result,
  })
  const contract = analyzeCallContract(
    node,
    reference,
    argumentsResult.facts,
    isAvailableSyntax(node),
  )
  const expressionType =
    contract.fact._tag === 'Compatible'
      ? availableExpressionType(signature.result)
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Operator',
      operator,
      reference,
      arguments: argumentsResult.facts,
      mappings: builtinArgumentMappings(reference, argumentsResult.facts),
      contract: contract.fact,
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([...argumentsResult.diagnostics, ...contract.diagnostics]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

const analyzePipelineExpression = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const inputNode = node.children.find(isExpressionNode)
  const target = SyntaxTree.directNode(node, 'PipelineTarget')
  const input =
    inputNode === undefined
      ? undefined
      : analyzeExpression(source, inputNode, declarations, declaration, scope, resolution)
  const argumentList =
    target === undefined ? undefined : SyntaxTree.directNode(target, 'ArgumentList')
  const explicit =
    argumentList === undefined
      ? Object.freeze({ facts: Object.freeze([]), diagnostics: Object.freeze([]) })
      : analyzeArgumentNodes(
          source,
          node,
          argumentList.children.filter(isRecursiveArgumentNode),
          declarations,
          declaration,
          scope,
          resolution,
        )
  const argumentsList = Object.freeze([
    ...(input === undefined ? [] : [argumentFact(declaration, node.span, input.fact, 0)]),
    ...explicit.facts.map((argument, index) =>
      argumentFact(declaration, node.span, argument.expression, index + 1),
    ),
  ])
  const identifiers = target?.children.filter(
    (element): element is Token.Token =>
      SyntaxTree.isToken(element) && element.kind === 'Identifier',
  )
  const qualifierToken = identifiers?.at(0)
  const memberToken = identifiers?.at(1)
  const resolved: QualifiedReferenceResult =
    qualifierToken === undefined || memberToken === undefined
      ? Object.freeze({
          reference: Object.freeze({
            _tag: 'Unavailable' as const,
            syntax: unavailableSyntax(target ?? node, 'Identifier'),
          }),
        })
      : resolveQualifiedReference(source, qualifierToken, memberToken, resolution)
  const contract = analyzeCallContract(
    node,
    resolved.reference,
    argumentsList,
    isAvailableSyntax(node),
  )
  const expressionType =
    contract.fact._tag === 'Compatible' && resolved.reference._tag === 'ResolvedBuiltin'
      ? availableExpressionType(resolved.reference.result)
      : contract.fact._tag === 'Compatible' &&
          resolved.reference._tag === 'Resolved' &&
          resolved.reference.declaration.returnType._tag === 'Resolved'
        ? availableExpressionType(resolved.reference.declaration.returnType.type)
        : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Pipeline',
      input:
        input?.fact ??
        Object.freeze({
          _tag: 'Integer',
          integer: Object.freeze({ _tag: 'Unavailable', syntax: node }),
          type: unavailableExpressionType,
          syntax: node,
        }),
      target: target ?? node,
      reference: resolved.reference,
      arguments: argumentsList,
      mappings:
        resolved.reference._tag === 'ResolvedBuiltin'
          ? builtinArgumentMappings(resolved.reference, argumentsList)
          : contract.mappings,
      contract: contract.fact,
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      ...(input?.diagnostics ?? []),
      ...explicit.diagnostics,
      ...(resolved.diagnostic === undefined ? [] : [resolved.diagnostic]),
      ...contract.diagnostics,
    ]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

function analyzeExpression(
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult | undefined {
  if (node.kind === 'BooleanLiteralExpression') {
    const token = directToken(node, 'TrueKeyword') ?? directToken(node, 'FalseKeyword')
    const type = token === undefined ? unavailableExpressionType : availableBoolExpressionType
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Boolean',
        value: token?.kind === 'TrueKeyword',
        type,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }

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
    return analyzeIdentifier(source, node, scope)
  }

  if (node.kind === 'MoveExpression') {
    const move = analyzeMove(source, node, scope)
    return Object.freeze({
      fact: move.fact,
      diagnostics: move.diagnostics,
      type: move.type,
    })
  }

  if (node.kind === 'GroupedExpression') {
    return analyzeGroupedExpression(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind === 'PrefixExpression' || node.kind === 'InfixExpression') {
    return analyzeOperatorExpression(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind === 'PipelineExpression') {
    return analyzePipelineExpression(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind !== 'CallExpression') return undefined

  const argumentsResult = analyzeArguments(
    source,
    node,
    declarations,
    declaration,
    scope,
    resolution,
  )

  const dotToken = directToken(node, 'Dot')
  if (dotToken !== undefined) {
    const identifiers = node.children.filter(
      (element): element is Token.Token =>
        SyntaxTree.isToken(element) && element.kind === 'Identifier',
    )
    const qualifierToken = identifiers.at(0)
    const memberToken = identifiers.at(1)
    if (qualifierToken === undefined || memberToken === undefined)
      return analyzeBuiltinCall(source, node, argumentsResult)
    const qualifier = spelling(source, qualifierToken)
    const member = spelling(source, memberToken)
    const qualifierLookup = NameResolution.lookup(resolution.scope, qualifier)
    if (qualifierLookup._tag === 'Intrinsic')
      return analyzeBuiltinCall(source, node, argumentsResult)
    if (qualifierLookup._tag === 'Namespace') {
      const memberLookup = DeclarationIndex.lookup(resolution.index, qualifierLookup.module, member)
      const candidate = memberLookup._tag === 'Resolved' ? memberLookup.declaration : undefined
      const diagnostic =
        candidate === undefined
          ? Diagnostic.unknownImportedMember(qualifierLookup.module, member, memberToken.span)
          : candidate.visibility === 'Private'
            ? Diagnostic.inaccessibleImportedMember(
                qualifierLookup.module,
                member,
                memberToken.span,
              )
            : undefined
      const reference: CallReferenceFact =
        candidate !== undefined && candidate.visibility === 'Public'
          ? Object.freeze({
              _tag: 'Resolved',
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              declaration: candidate,
            })
          : Object.freeze({
              _tag: 'Missing',
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              ...(diagnostic === undefined ? {} : { cause: Diagnostic.identity(diagnostic) }),
            })
      return finishDeclarationCall(node, reference, argumentsResult, diagnostic)
    }
    const diagnostic =
      qualifierLookup._tag === 'Missing' || qualifierLookup._tag === 'Resolved'
        ? Diagnostic.unknownActor(qualifier, qualifierToken.span)
        : undefined
    const inheritedCause =
      qualifierLookup._tag === 'Unavailable'
        ? qualifierLookup.cause
        : qualifierLookup._tag === 'Conflict'
          ? qualifierLookup.conflict.cause
          : undefined
    const reference: CallReferenceFact = Object.freeze({
      _tag: 'Missing',
      spelling: `${qualifier}.${member}`,
      token: qualifierToken,
      ...(diagnostic !== undefined
        ? { cause: Diagnostic.identity(diagnostic) }
        : inheritedCause === undefined
          ? {}
          : { cause: inheritedCause }),
    })
    return finishDeclarationCall(node, reference, argumentsResult, diagnostic)
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
  const resolvedLookup = NameResolution.lookup(resolution.scope, tokenSpelling)
  const localLookup = lookupDeclaration(declarations, tokenSpelling)
  const lookup: DeclarationIndex.DeclarationLookup =
    resolvedLookup._tag === 'Conflict'
      ? Object.freeze({
          _tag: 'Ambiguous',
          spelling: tokenSpelling,
          declarations: Object.freeze(
            resolvedLookup.conflict.bindings.flatMap((binding) =>
              binding._tag === 'LocalDeclaration' || binding._tag === 'ImportedMember'
                ? [binding.declaration]
                : [],
            ),
          ),
        })
      : localLookup._tag === 'Ambiguous'
        ? localLookup
        : resolvedLookup._tag === 'Resolved'
          ? Object.freeze({
              _tag: 'Resolved',
              spelling: tokenSpelling,
              declaration: resolvedLookup.declaration,
            })
          : resolvedLookup._tag === 'Missing'
            ? localLookup
            : Object.freeze({ _tag: 'Missing', spelling: tokenSpelling })
  const missingDiagnostic =
    lookup._tag === 'Missing' && resolvedLookup._tag !== 'Unavailable'
      ? Diagnostic.unknownFunction(tokenSpelling, token.span)
      : undefined
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
            ...(resolvedLookup._tag === 'Conflict' ? { cause: resolvedLookup.conflict.cause } : {}),
          })
        : Object.freeze({
            _tag: 'Missing',
            spelling: tokenSpelling,
            token,
            ...(missingDiagnostic !== undefined
              ? { cause: Diagnostic.identity(missingDiagnostic) }
              : resolvedLookup._tag === 'Unavailable' && resolvedLookup.cause !== undefined
                ? { cause: resolvedLookup.cause }
                : {}),
          })
  const callContract = analyzeCallContract(node, reference, argumentsResult.facts)
  const syntaxAvailable = hasAvailableCallSyntax(node)
  const expressionType =
    syntaxAvailable &&
    reference._tag === 'Resolved' &&
    reference.declaration.returnType._tag === 'Resolved'
      ? availableExpressionType(reference.declaration.returnType.type)
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

const finishDeclarationCall = (
  node: SyntaxTree.Node,
  reference: CallReferenceFact,
  argumentsResult: ArgumentsResult,
  diagnostic: Diagnostic.Diagnostic | undefined,
): ExpressionResult => {
  const callContract = analyzeCallContract(node, reference, argumentsResult.facts)
  const expressionType =
    hasAvailableCallSyntax(node) &&
    reference._tag === 'Resolved' &&
    reference.declaration.returnType._tag === 'Resolved'
      ? availableExpressionType(reference.declaration.returnType.type)
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
      ...(diagnostic === undefined ? [] : [diagnostic]),
      ...argumentsResult.diagnostics,
      ...callContract.diagnostics,
    ]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

const statementExpressionNode = (statement: SyntaxTree.Node): SyntaxTree.Node => {
  const expression = statement.children.find((element): element is SyntaxTree.Node =>
    isExpressionNode(element),
  )
  if (expression === undefined) {
    throw new RangeError('Semantic analysis expected a statement expression')
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

const bindingName = (
  source: SourceFile.SourceFile,
  statement: SyntaxTree.Node,
): DeclarationIndex.DeclaredName => {
  const token = directToken(statement, 'Identifier')
  return token === undefined
    ? Object.freeze({
        _tag: 'Unavailable' as const,
        syntax: unavailableSyntax(statement, 'Identifier'),
      })
    : Object.freeze({ _tag: 'Present' as const, spelling: spelling(source, token), token })
}

const scopeSpanFor = (scope: Scope, spellingText: string): SourceSpan.SourceSpan | undefined => {
  for (const parameter of scope.parameters) {
    if (parameter.name._tag === 'Present' && parameter.name.spelling === spellingText) {
      return parameter.name.token.span
    }
  }
  for (const binding of scope.bindings) {
    if (binding.name._tag === 'Present' && binding.name.spelling === spellingText) {
      return binding.name.token.span
    }
  }
  return undefined
}

interface BodyContext {
  readonly source: SourceFile.SourceFile
  readonly declaration: DeclarationFact
  readonly declarations: ReadonlyArray<DeclarationFact>
  readonly bindings: Array<BindingDeclarationFact>
  readonly diagnostics: Array<Diagnostic.Diagnostic>
  readonly resolution: ResolutionContext
}

interface ResolutionContext {
  readonly scope: NameResolution.ModuleScope
  readonly index: DeclarationIndex.Index
}

const analyzeStatements = (
  context: BodyContext,
  blockNode: SyntaxTree.Node,
  initialScope: Scope,
): ReadonlyArray<StatementFact> => {
  const facts: Array<StatementFact> = []
  let scope = initialScope

  for (const element of blockNode.children) {
    if (!SyntaxTree.isNode(element)) continue

    if (element.kind === 'BindingStatement') {
      const initializerNode = statementExpressionNode(element)
      const initializer = analyzeExpression(
        context.source,
        initializerNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (initializer === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${initializerNode.kind}`)
      }
      context.diagnostics.push(...initializer.diagnostics)

      const name = bindingName(context.source, element)
      const binding: BindingDeclarationFact = Object.freeze({
        _tag: 'BindingFact',
        id: Object.freeze({
          _tag: 'HirBinding',
          function: context.declaration.id,
          ordinal: context.bindings.length,
        }),
        name,
        inferredType: initializer.fact.type,
        initializer: initializer.fact,
        syntax: element,
      })
      context.bindings.push(binding)
      facts.push(Object.freeze({ _tag: 'BindStatement', binding }))

      if (name._tag === 'Present') {
        const originalSpan = scopeSpanFor(scope, name.spelling)
        if (originalSpan === undefined) {
          scope = Object.freeze({
            parameters: scope.parameters,
            bindings: Object.freeze([...scope.bindings, binding]),
          })
        } else {
          context.diagnostics.push(
            Diagnostic.rebindingName(name.spelling, originalSpan, name.token.span),
          )
        }
      }
      continue
    }

    if (element.kind === 'ConditionalStatement') {
      const conditionNode = statementExpressionNode(element)
      const condition = analyzeExpression(
        context.source,
        conditionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (condition === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${conditionNode.kind}`)
      }
      context.diagnostics.push(...condition.diagnostics)
      if (condition.fact.type._tag === 'Available' && condition.fact.type.type !== 'Bool') {
        context.diagnostics.push(
          Diagnostic.conditionNotBool(condition.fact.type.type, condition.fact.syntax.span),
        )
      }

      const arms = SyntaxTree.directNodes(element, 'Block')
      const taken =
        arms.at(0) === undefined
          ? []
          : analyzeStatements(context, arms[0] as SyntaxTree.Node, scope)
      const otherwiseArm = arms.at(1)
      const otherwise =
        otherwiseArm === undefined ? [] : analyzeStatements(context, otherwiseArm, scope)
      facts.push(
        Object.freeze({
          _tag: 'IfStatement',
          condition: condition.fact,
          taken: Object.freeze([...taken]),
          otherwise: Object.freeze([...otherwise]),
          syntax: element,
        }),
      )
      continue
    }

    if (element.kind === 'ReturnStatement') {
      const expressionNode = statementExpressionNode(element)
      const expression = analyzeExpression(
        context.source,
        expressionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (expression === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${expressionNode.kind}`)
      }
      context.diagnostics.push(...expression.diagnostics)
      facts.push(
        Object.freeze({ _tag: 'ReturnStatement', expression: expression.fact, syntax: element }),
      )
      break
    }
  }

  return Object.freeze(facts)
}

const analyzeFunctionBody = (
  source: SourceFile.SourceFile,
  declaration: DeclarationFact,
  declarations: ReadonlyArray<DeclarationFact>,
  resolution: ResolutionContext,
): FunctionAnalysis => {
  const blockNode = childNode(declaration.syntax, 'Block')
  const context: BodyContext = {
    source,
    declaration,
    declarations,
    bindings: [],
    diagnostics: [],
    resolution,
  }
  const statements = analyzeStatements(
    context,
    blockNode,
    Object.freeze({ parameters: declaration.parameters, bindings: [] }),
  )

  const trailing = [...statements]
    .reverse()
    .find(
      (statement): statement is Extract<StatementFact, { _tag: 'ReturnStatement' }> =>
        statement._tag === 'ReturnStatement',
    )
  if (trailing === undefined) {
    throw new RangeError('Semantic analysis expected a trailing return statement')
  }
  const expression = trailing.expression
  const expressionType = expression.type._tag === 'Available' ? expression.type.type : undefined

  const returnCompatibility =
    declaration.returnType._tag === 'Resolved' && expressionType === declaration.returnType.type
      ? compatible
      : unavailableCompatibility

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FunctionFact',
      declaration,
      statements,
      bindings: Object.freeze([...context.bindings]),
      returnedExpression: expression,
      returnCompatibility,
    }),
    diagnostics: Object.freeze([...context.diagnostics]),
  })
}

const hirReference = (
  reference: ParameterReferenceFact,
  type: ExpressionTypeFact,
  span: SourceSpan.SourceSpan,
): Hir.Expression => {
  if (reference._tag === 'Resolved' && type._tag === 'Available') {
    return Object.freeze({
      _tag: 'ParameterReference',
      parameter: reference.parameter.id,
      type: type.type,
      span,
    })
  }
  if (reference._tag === 'ResolvedBinding' && type._tag === 'Available') {
    return Object.freeze({
      _tag: 'BindingReference',
      binding: reference.binding.id,
      type: type.type,
      span,
    })
  }
  return Object.freeze({
    _tag: 'Unavailable',
    span,
    ...(reference._tag === 'Missing' && reference.cause !== undefined
      ? { cause: reference.cause }
      : {}),
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
  if (fact._tag === 'Boolean') {
    return fact.type._tag === 'Available'
      ? Object.freeze({
          _tag: 'BooleanLiteral',
          value: fact.value,
          type: fact.type.type,
          span: fact.syntax.span,
        })
      : Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
  }
  if (fact._tag === 'Identifier') {
    return hirReference(fact.reference, fact.type, fact.syntax.span)
  }
  if (fact._tag === 'Move') {
    const subject = hirReference(fact.reference, fact.type, fact.syntax.span)
    if (subject._tag === 'Unavailable' || fact.type._tag !== 'Available') {
      return subject._tag === 'Unavailable'
        ? subject
        : Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    }
    return Object.freeze({
      _tag: 'Move',
      subject,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'Grouped') return hirExpression(fact.expression)
  if (
    fact.reference._tag === 'ResolvedBuiltin' &&
    fact.contract._tag === 'Compatible' &&
    fact.type._tag === 'Available'
  ) {
    return Object.freeze({
      _tag: 'BuiltinCall',
      operation: fact.reference.operation,
      arguments: Object.freeze(
        fact.arguments.map((argument) => hirExpression(argument.expression)),
      ),
      type: fact.type.type,
      span: fact.syntax.span,
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
    fact.reference._tag === 'Missing' || fact.reference._tag === 'Ambiguous'
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
export interface Input {
  readonly syntax: SyntaxFile.SyntaxFile
  readonly headers: DeclarationIndex.ModuleHeaders
  readonly scope: NameResolution.ModuleScope
  readonly index: DeclarationIndex.Index
}

export const elaborateModule = (input: Input): Result => {
  const { syntax, headers, scope, index } = input
  const source = syntax.source
  const declarations = headers.declarations
  const analyzed = declarations.map((declaration) =>
    analyzeFunctionBody(source, declaration, declarations, Object.freeze({ scope, index })),
  )
  const functions = Object.freeze(analyzed.map((result) => result.fact))
  const diagnostics = [
    ...headers.diagnostics,
    ...analyzed.flatMap((result) => result.diagnostics),
  ].sort(compareDiagnostics)
  const hirStatements = (facts: ReadonlyArray<StatementFact>): ReadonlyArray<Hir.Statement> =>
    Object.freeze(
      facts.map((statement): Hir.Statement => {
        if (statement._tag === 'BindStatement') {
          return Object.freeze({
            _tag: 'Bind' as const,
            binding: statement.binding.id,
            name:
              statement.binding.name._tag === 'Present'
                ? statement.binding.name.spelling
                : undefined,
            initializer: hirExpression(statement.binding.initializer),
            span: statement.binding.syntax.span,
          })
        }
        if (statement._tag === 'IfStatement') {
          return Object.freeze({
            _tag: 'If' as const,
            condition: hirExpression(statement.condition),
            taken: hirStatements(statement.taken),
            otherwise: hirStatements(statement.otherwise),
            span: statement.syntax.span,
          })
        }
        return Object.freeze({
          _tag: 'Return' as const,
          expression: hirExpression(statement.expression),
          span: statement.expression.syntax.span,
        })
      }),
    )
  const hir: Hir.Module = Object.freeze({
    _tag: 'HirModule',
    module: source.id,
    functions: Object.freeze(
      functions.map((fact) =>
        Object.freeze({
          _tag: 'HirFunction' as const,
          declaration: fact.declaration,
          contract: Hir.contractOf(fact.declaration),
          statements: hirStatements(fact.statements),
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
