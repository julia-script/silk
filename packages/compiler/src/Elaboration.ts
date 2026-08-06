import { dual } from 'effect/Function'
import * as Option from 'effect/Option'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Hir from './Hir.js'
import * as Match from './Match.js'
import * as NameResolution from './NameResolution.js'
import * as Operator from './Operator.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'
import * as TypeCompatibility from './TypeCompatibility.js'

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
  readonly mutability: 'Immutable' | 'Mutable'
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
      readonly _tag: 'ResolvedPattern'
      readonly spelling: string
      readonly token: Token.Token
      readonly binding: PatternBindingFact
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

/** One `move <place>` expression with its consuming subject fact. */
export interface MoveExpressionFact {
  readonly _tag: 'Move'
  readonly subject: ExpressionFact
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One flattened leaf binding introduced by a nominal pattern. */
export interface PatternBindingFact {
  readonly _tag: 'PatternBinding'
  readonly id: Match.BindingId
  readonly name: DeclaredName
  readonly field: DeclarationIndex.FieldFact
  readonly path: ReadonlyArray<DeclarationIndex.FieldId>
  readonly type: ExpressionTypeFact
  readonly access: Match.Access
  readonly syntax: SyntaxTree.Node
}

export type PatternFieldState =
  | { readonly _tag: 'Resolved'; readonly field: DeclarationIndex.FieldFact }
  | { readonly _tag: 'Unknown'; readonly cause: Diagnostic.Identity }
  | {
      readonly _tag: 'Duplicate'
      readonly field: DeclarationIndex.FieldFact
      readonly cause: Diagnostic.Identity
    }
  | { readonly _tag: 'Unavailable' }

export interface PatternFieldFact {
  readonly _tag: 'PatternField'
  readonly name: string | undefined
  readonly state: PatternFieldState
  readonly binding?: PatternBindingFact
  readonly nested?: PatternFact
  readonly syntax: SyntaxTree.Node
}

export type PatternFact =
  | {
      readonly _tag: 'NominalPattern'
      readonly id: Match.PatternId
      readonly target: StructTargetFact
      readonly member?: Type.Nominal
      readonly fields: ReadonlyArray<PatternFieldFact>
      readonly bindings: ReadonlyArray<PatternBindingFact>
      readonly omitted: ReadonlyArray<ReadonlyArray<DeclarationIndex.FieldId>>
      readonly rest: boolean
      readonly complete: boolean
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'UniversalPattern'
      readonly id: Match.PatternId
      readonly bindings: ReadonlyArray<PatternBindingFact>
      readonly omitted: ReadonlyArray<ReadonlyArray<DeclarationIndex.FieldId>>
      readonly syntax: SyntaxTree.Node
    }

export interface MatchArmFact {
  readonly _tag: 'MatchArm'
  readonly id: Match.ArmId
  readonly pattern: PatternFact
  readonly bindings: ReadonlyArray<PatternBindingFact>
  readonly guard?: ExpressionFact
  readonly result: ExpressionFact
  readonly before: ReadonlyArray<Type.Nominal>
  readonly after: ReadonlyArray<Type.Nominal>
  readonly reachable: boolean
  readonly syntax: SyntaxTree.Node
}

export interface MatchExpressionFact {
  readonly _tag: 'Match'
  readonly id: Match.MatchId
  readonly access: Match.Access
  readonly scrutinee: ExpressionFact
  readonly members: ReadonlyArray<Type.Nominal>
  readonly arms: ReadonlyArray<MatchArmFact>
  readonly exhaustive: boolean
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

export type StructTargetFact =
  | {
      readonly _tag: 'Resolved'
      readonly struct: DeclarationIndex.StructFact
      readonly type: Type.Nominal
    }
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity }

export type StructInitializerState =
  | { readonly _tag: 'Resolved'; readonly field: DeclarationIndex.FieldFact }
  | { readonly _tag: 'Unknown'; readonly cause: Diagnostic.Identity }
  | {
      readonly _tag: 'Duplicate'
      readonly field: DeclarationIndex.FieldFact
      readonly cause: Diagnostic.Identity
    }
  | {
      readonly _tag: 'TypeMismatch'
      readonly field: DeclarationIndex.FieldFact
      readonly cause: Diagnostic.Identity
    }
  | { readonly _tag: 'Unavailable' }

export interface StructInitializerFact {
  readonly _tag: 'StructInitializer'
  readonly name: string | undefined
  readonly expression: ExpressionFact
  readonly state: StructInitializerState
  readonly syntax: SyntaxTree.Node
}

export interface StructLiteralExpressionFact {
  readonly _tag: 'StructLiteral'
  readonly target: StructTargetFact
  readonly authorized: boolean
  readonly initializers: ReadonlyArray<StructInitializerFact>
  readonly fields: ReadonlyArray<{
    readonly field: DeclarationIndex.FieldFact
    readonly initializer: StructInitializerFact
  }>
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

export type ProjectionState =
  | { readonly _tag: 'Resolved'; readonly field: DeclarationIndex.FieldFact }
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity }

export interface FieldProjectionExpressionFact {
  readonly _tag: 'FieldProjection'
  readonly subject: ExpressionFact
  readonly nominal?: Type.Nominal
  readonly fieldName: string | undefined
  readonly state: ProjectionState
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One source-ordered array element with contextual compatibility retained independently. */
export interface ArrayElementFact {
  readonly _tag: 'ArrayElement'
  readonly ordinal: number
  readonly expression: ExpressionFact
  readonly expected?: SemanticType
  readonly compatibility:
    | { readonly _tag: 'Compatible' }
    | {
        readonly _tag: 'TypeMismatch'
        readonly expected: SemanticType
        readonly actual: SemanticType
      }
    | { readonly _tag: 'Unavailable' }
  readonly syntax: SyntaxTree.Node
}

export type ArrayLiteralState =
  | { readonly _tag: 'Complete'; readonly type: Type.FixedArray }
  | { readonly _tag: 'MissingContext' }
  | { readonly _tag: 'LengthMismatch'; readonly expected: number; readonly actual: number }
  | { readonly _tag: 'IncompatibleElements' }
  | { readonly _tag: 'Unavailable' }

/** One complete-or-unavailable fixed-array literal and every retained element fact. */
export interface ArrayLiteralExpressionFact {
  readonly _tag: 'ArrayLiteral'
  readonly elements: ReadonlyArray<ArrayElementFact>
  readonly expected?: Type.FixedArray
  readonly elementType?: SemanticType
  readonly length: number
  readonly state: ArrayLiteralState
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

export type BoundsFact =
  | { readonly _tag: 'Proven'; readonly index: number; readonly length: number }
  | {
      readonly _tag: 'Invalid'
      readonly index: number
      readonly length: number
      readonly cause: Diagnostic.Identity
    }
  | { readonly _tag: 'Runtime'; readonly length: number }
  | { readonly _tag: 'Unavailable' }

/** One typed checked array-place projection. */
export interface IndexProjectionExpressionFact {
  readonly _tag: 'IndexProjection'
  readonly subject: ExpressionFact
  readonly index: ExpressionFact
  readonly array?: Type.FixedArray
  readonly elementType?: SemanticType
  readonly access: 'CopyRead' | 'ConsumeRequested'
  readonly bounds: BoundsFact
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
  | MatchExpressionFact
  | StructLiteralExpressionFact
  | ArrayLiteralExpressionFact
  | FieldProjectionExpressionFact
  | IndexProjectionExpressionFact
  | GroupedExpressionFact
  | OperatorExpressionFact
  | PipelineExpressionFact
  | {
      readonly _tag: 'Call'
      readonly reference: CallReferenceFact
      readonly typeArguments: ReadonlyArray<TypeArgumentFact>
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

/** One source-owned explicit call type argument, resolved canonically when available. */
export interface TypeArgumentFact {
  readonly _tag: 'TypeArgument'
  readonly ordinal: number
  readonly syntax: SyntaxTree.Node
  readonly declared: DeclaredTypeFact
  readonly type?: SemanticType
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
      readonly typeArguments: ReadonlyArray<SemanticType>
      readonly substitution: ReadonlyMap<string, SemanticType>
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
  | {
      readonly _tag: 'BindStatement'
      readonly binding: BindingDeclarationFact
      readonly region: Hir.RegionId
    }
  | {
      readonly _tag: 'IfStatement'
      readonly condition: ExpressionFact
      readonly taken: ReadonlyArray<StatementFact>
      readonly otherwise: ReadonlyArray<StatementFact>
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'WriteStatement'
      readonly destination: ExpressionFact
      readonly root?: BindingDeclarationFact
      readonly value: ExpressionFact
      readonly compatible: boolean
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'WhileStatement'
      readonly loop: Hir.LoopId
      readonly parent?: Hir.LoopId
      readonly condition: ExpressionFact
      readonly body: ReadonlyArray<StatementFact>
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'BreakStatement'
      readonly target?: Hir.LoopId
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'ContinueStatement'
      readonly target?: Hir.LoopId
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'ReturnStatement'
      readonly expression: ExpressionFact
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }

/** One function's declaration, statements, bindings, and compatibility facts. */
export interface FunctionFact {
  readonly _tag: 'FunctionFact'
  readonly declaration: DeclarationFact
  readonly statements: ReadonlyArray<StatementFact>
  readonly bindings: ReadonlyArray<BindingDeclarationFact>
  readonly regionOrder: ReadonlyArray<Hir.RegionId>
  readonly returnedExpression: ExpressionFact
  readonly returnCompatibility: ReturnCompatibility
}

/** The closed result of looking up one declaration spelling. */
export type DeclarationLookup = DeclarationIndex.DeclarationLookup

/** The complete deterministic elaboration result for all direct bootstrap declarations. */
export interface Result {
  readonly _tag: 'Elaboration'
  readonly syntax: SyntaxFile.SyntaxFile
  readonly index: DeclarationIndex.Index
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
  type === 'I32'
    ? availableI32ExpressionType
    : type === 'Bool'
      ? availableBoolExpressionType
      : Object.freeze({ _tag: 'Available', type })
const unavailableExpressionType: ExpressionTypeFact = Object.freeze({ _tag: 'Unavailable' })

const typesCompatible = (source: SemanticType, target: SemanticType): boolean =>
  TypeCompatibility.isCompatible(TypeCompatibility.check(source, target))

const unionConversionDiagnostic = (
  source: SemanticType,
  target: SemanticType,
  span: SourceSpan.SourceSpan,
): Diagnostic.Diagnostic | undefined => {
  const compatibility = TypeCompatibility.check(source, target)
  return compatibility._tag === 'Incompatible' &&
    (Type.isUnion(source) || Type.isNever(source) || Type.isUnion(target) || Type.isNever(target))
    ? Diagnostic.incompatibleUnionConversion(
        Type.encode(source),
        Type.encode(target),
        compatibility.missing.map(Type.encode),
        span,
      )
    : undefined
}

const expressionNodeKinds: ReadonlyArray<SyntaxTree.NodeKind> = Object.freeze([
  'IntegerLiteralExpression',
  'BooleanLiteralExpression',
  'IdentifierExpression',
  'MoveExpression',
  'MatchExpression',
  'StructLiteralExpression',
  'ArrayLiteralExpression',
  'FieldProjectionExpression',
  'IndexProjectionExpression',
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
    element.kind === 'MatchExpression' ||
    element.kind === 'StructLiteralExpression' ||
    element.kind === 'ArrayLiteralExpression' ||
    element.kind === 'FieldProjectionExpression' ||
    element.kind === 'IndexProjectionExpression' ||
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
  readonly patternBindings: ReadonlyArray<PatternBindingFact>
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
  const patternBinding = scope.patternBindings.find(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === tokenSpelling,
  )
  if (patternBinding !== undefined) {
    return Object.freeze({
      reference: Object.freeze({
        _tag: 'ResolvedPattern' as const,
        spelling: tokenSpelling,
        token,
        binding: patternBinding,
      }),
      type: patternBinding.type,
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
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): MoveResult => {
  const subjectNode = node.children.find(isExpressionNode)
  const subject =
    subjectNode === undefined
      ? undefined
      : analyzeExpression(source, subjectNode, declarations, declaration, scope, resolution)
  if (subject === undefined) throw new RangeError('Move expression requires a subject expression')
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Move',
      subject: subject.fact,
      type: subject.fact.type,
      syntax: node,
    }),
    diagnostics: subject.diagnostics,
    type: subject.type,
  })
}

interface StructTargetResult {
  readonly fact: StructTargetFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const resolveStructTarget = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  resolution: ResolutionContext,
  caller?: DeclarationFact,
): StructTargetResult => {
  const environment = new Map(
    (caller?.typeParameters ?? []).flatMap((parameter) =>
      parameter.name._tag === 'Present' ? [[parameter.name.spelling, parameter.type] as const] : [],
    ),
  )
  const analyzed = DeclarationIndex.analyzeDeclaredType(source, syntax, environment)
  const nameResolution: NameResolution.Resolution = Object.freeze({
    _tag: 'NameResolution',
    modules: Object.freeze([resolution.scope]),
    diagnostics: Object.freeze([]),
  })
  const resolved = DeclarationIndex.resolveTypeFact(
    resolution.index,
    source.id,
    analyzed.fact,
    (module, path) => NameResolution.resolveType(nameResolution, resolution.index, module, path),
  )
  if (resolved.fact._tag === 'Resolved' && Type.isNominal(resolved.fact.type)) {
    const declaration = DeclarationIndex.byCanonical(resolution.index, {
      _tag: 'CanonicalDeclarationId',
      module: resolved.fact.type.module,
      name: resolved.fact.type.name,
    })
    if (declaration?._tag === 'StructDeclaration') {
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Resolved', struct: declaration, type: resolved.fact.type }),
        diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics),
      })
    }
  }
  const token = SyntaxTree.tokens(syntax).find((candidate) => candidate.kind === 'Identifier')
  const diagnostic = Diagnostic.expectedType(
    resolved.fact._tag === 'Resolved' ? Type.encode(resolved.fact.type) : 'unavailable struct',
    token?.span ?? syntax.span,
  )
  return Object.freeze({
    fact: Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) }),
    diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics, [diagnostic]),
  })
}

interface PatternCounters {
  pattern: number
  binding: number
  invalid: boolean
}

interface PatternResult {
  readonly fact: PatternFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const patternDeclaredName = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  token: Token.Token | undefined,
): DeclaredName =>
  token === undefined
    ? Object.freeze({ _tag: 'Unavailable', syntax })
    : Object.freeze({ _tag: 'Present', spelling: spelling(source, token), token })

const analyzePattern = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  arm: Match.ArmId,
  access: Match.Access,
  scope: Scope,
  resolution: ResolutionContext,
  declaration: DeclarationFact,
  counters: PatternCounters,
  prefix: ReadonlyArray<DeclarationIndex.FieldId> = Object.freeze([]),
  localNames = new Map<string, SourceSpan.SourceSpan>(),
): PatternResult => {
  const id: Match.PatternId = Object.freeze({
    _tag: 'PatternId',
    arm,
    ordinal: counters.pattern,
  })
  counters.pattern += 1
  if (node.kind === 'UniversalPattern') {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'UniversalPattern',
        id,
        bindings: Object.freeze([]),
        omitted: Object.freeze(access === 'Move' ? [Object.freeze([])] : []),
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
    })
  }

  const targetSyntax = SyntaxTree.directNode(node, 'AppliedType') ?? childNode(node, 'TypePath')
  const target = resolveStructTarget(source, targetSyntax, resolution, declaration)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...target.diagnostics]
  const struct = target.fact._tag === 'Resolved' ? target.fact.struct : undefined
  const nominal = target.fact._tag === 'Resolved' ? target.fact.type : undefined
  const structSubstitution =
    struct === undefined || nominal === undefined
      ? new Map<string, SemanticType>()
      : (Type.substitution(
          struct.typeParameters.map((parameter) => parameter.type),
          nominal.arguments,
        ) ?? new Map())
  const label = nominal === undefined ? 'unknown struct' : Type.encode(nominal)
  const seen = new Map<string, PatternFieldFact>()
  const bindings: Array<PatternBindingFact> = []
  const fields = SyntaxTree.directNodes(node, 'PatternField').map((fieldNode): PatternFieldFact => {
    const identifiers = fieldNode.children.filter(
      (element): element is Token.Token =>
        SyntaxTree.isToken(element) && element.kind === 'Identifier',
    )
    const nameToken = identifiers.at(0)
    const name = nameToken === undefined ? undefined : spelling(source, nameToken)
    const lookup =
      struct === undefined || name === undefined
        ? undefined
        : DeclarationIndex.lookupField(struct.fields, name)
    let state: PatternFieldState = Object.freeze({ _tag: 'Unavailable' })
    let resolvedField: DeclarationIndex.FieldFact | undefined
    if (lookup?._tag === 'Resolved') {
      const original = seen.get(name ?? '')
      if (original === undefined) {
        resolvedField = lookup.field
        state = Object.freeze({ _tag: 'Resolved', field: lookup.field })
      } else {
        const diagnostic = Diagnostic.duplicatePatternField(
          name ?? '',
          original.syntax.span,
          nameToken?.span ?? fieldNode.span,
        )
        diagnostics.push(diagnostic)
        state = Object.freeze({
          _tag: 'Duplicate',
          field: lookup.field,
          cause: Diagnostic.identity(diagnostic),
        })
      }
    } else if (name !== undefined) {
      const diagnostic = Diagnostic.unknownStructField(
        label,
        name,
        nameToken?.span ?? fieldNode.span,
      )
      diagnostics.push(diagnostic)
      state = Object.freeze({ _tag: 'Unknown', cause: Diagnostic.identity(diagnostic) })
    }

    const nestedNode = SyntaxTree.directNode(fieldNode, 'NominalPattern')
    let nested: PatternFact | undefined
    let binding: PatternBindingFact | undefined
    if (nestedNode !== undefined) {
      const nestedResult = analyzePattern(
        source,
        nestedNode,
        arm,
        access,
        scope,
        resolution,
        declaration,
        counters,
        resolvedField === undefined ? prefix : Object.freeze([...prefix, resolvedField.id]),
        localNames,
      )
      diagnostics.push(...nestedResult.diagnostics)
      nested = nestedResult.fact
      const expected =
        resolvedField?.declaredType._tag === 'Resolved'
          ? Type.substitute(resolvedField.declaredType.type, structSubstitution)
          : undefined
      if (
        expected !== undefined &&
        nested._tag === 'NominalPattern' &&
        nested.member !== undefined &&
        !Type.equals(expected, nested.member)
      ) {
        counters.invalid = true
        diagnostics.push(
          Diagnostic.matchMemberNotInScrutinee(
            Type.encode(nested.member),
            Type.encode(expected),
            nestedNode.span,
          ),
        )
      }
    } else if (resolvedField !== undefined) {
      const bindingToken = identifiers.at(1) ?? nameToken
      const declaredName = patternDeclaredName(source, fieldNode, bindingToken)
      const bindingType =
        resolvedField.declaredType._tag === 'Resolved'
          ? availableExpressionType(
              Type.substitute(resolvedField.declaredType.type, structSubstitution),
            )
          : unavailableExpressionType
      binding = Object.freeze({
        _tag: 'PatternBinding',
        id: Object.freeze({
          _tag: 'PatternBindingId',
          arm,
          ordinal: counters.binding,
        }),
        name: declaredName,
        field: resolvedField,
        path: Object.freeze([...prefix, resolvedField.id]),
        type: bindingType,
        access,
        syntax: fieldNode,
      })
      counters.binding += 1
      bindings.push(binding)
      if (declaredName._tag === 'Present') {
        const original =
          scopeSpanFor(scope, declaredName.spelling) ?? localNames.get(declaredName.spelling)
        if (original === undefined) localNames.set(declaredName.spelling, declaredName.token.span)
        else {
          counters.invalid = true
          diagnostics.push(
            Diagnostic.patternBindingConflict(
              declaredName.spelling,
              original,
              declaredName.token.span,
            ),
          )
        }
      }
    }
    if (nested?.bindings !== undefined) bindings.push(...nested.bindings)
    const fact: PatternFieldFact = Object.freeze({
      _tag: 'PatternField',
      name,
      state,
      ...(binding === undefined ? {} : { binding }),
      ...(nested === undefined ? {} : { nested }),
      syntax: fieldNode,
    })
    if (name !== undefined && !seen.has(name)) seen.set(name, fact)
    return fact
  })

  const rest = SyntaxTree.directNode(node, 'RestPattern') !== undefined
  const omitted: Array<ReadonlyArray<DeclarationIndex.FieldId>> = fields.flatMap(
    (field) => field.nested?.omitted ?? [],
  )
  if (struct !== undefined && !rest) {
    for (const field of struct.fields) {
      if (field.name._tag !== 'Present' || seen.has(field.name.spelling)) continue
      diagnostics.push(Diagnostic.missingPatternField(label, field.name.spelling, node.span))
    }
  } else if (struct !== undefined && rest) {
    for (const field of struct.fields) {
      if (field.name._tag === 'Present' && seen.has(field.name.spelling)) continue
      omitted.push(Object.freeze([...prefix, field.id]))
    }
  }
  const complete =
    target.fact._tag === 'Resolved' &&
    !counters.invalid &&
    isAvailableSyntax(node) &&
    fields.every(
      (field) =>
        field.state._tag === 'Resolved' &&
        (field.nested === undefined ||
          (field.nested._tag === 'NominalPattern' && field.nested.complete)),
    ) &&
    (rest ||
      struct?.fields.every(
        (field) => field.name._tag !== 'Present' || seen.has(field.name.spelling),
      ) === true)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'NominalPattern',
      id,
      target: target.fact,
      ...(nominal === undefined ? {} : { member: nominal }),
      fields: Object.freeze(fields),
      bindings: Object.freeze(bindings),
      omitted: Object.freeze(omitted),
      rest,
      complete,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
  })
}

const unavailableExpression = (syntax: SyntaxTree.Node): ExpressionFact =>
  Object.freeze({
    _tag: 'Identifier',
    reference: Object.freeze({ _tag: 'Unavailable', syntax }),
    type: unavailableExpressionType,
    syntax,
  })

const matchAccess = (node: SyntaxTree.Node): Match.Access => {
  const access = SyntaxTree.directNode(node, 'MatchAccess')
  if (access === undefined) return 'Copy'
  if (directToken(access, 'MoveKeyword') !== undefined) return 'Move'
  if (directToken(access, 'Ampersand') === undefined) return 'Copy'
  return directToken(access, 'MutKeyword') === undefined ? 'Shared' : 'Exclusive'
}

const analyzeMatch = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
): ExpressionResult => {
  const id: Match.MatchId = Object.freeze({
    _tag: 'MatchId',
    function: declaration.id,
    span: node.span,
  })
  const access = matchAccess(node)
  const expressionNodes = node.children.filter(isExpressionNode)
  const scrutineeNode = expressionNodes.at(0)
  const scrutinee =
    scrutineeNode === undefined
      ? undefined
      : analyzeExpression(source, scrutineeNode, declarations, declaration, scope, resolution)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...(scrutinee?.diagnostics ?? [])]
  const members = scrutinee?.type === undefined ? undefined : Match.membersOf(scrutinee.type)
  if (scrutinee?.type !== undefined && members === undefined) {
    diagnostics.push(Diagnostic.matchScrutineeNotNominal(Type.encode(scrutinee.type), node.span))
  }

  const preliminary = SyntaxTree.directNodes(node, 'MatchArm').map((armNode, ordinal) => {
    const armId: Match.ArmId = Object.freeze({ _tag: 'MatchArmId', match: id, ordinal })
    const patternNode =
      SyntaxTree.directNode(armNode, 'NominalPattern') ??
      SyntaxTree.directNode(armNode, 'UniversalPattern')
    if (patternNode === undefined) throw new RangeError('Match arm requires a pattern')
    const pattern = analyzePattern(
      source,
      patternNode,
      armId,
      access,
      scope,
      resolution,
      declaration,
      {
        pattern: 0,
        binding: 0,
        invalid: false,
      },
    )
    diagnostics.push(...pattern.diagnostics)
    return Object.freeze({ armNode, armId, pattern: pattern.fact })
  })
  const coverage = Match.cover(
    members ?? Object.freeze([]),
    preliminary.map(({ armNode, pattern }) =>
      Object.freeze({
        ...(pattern._tag === 'NominalPattern' && pattern.member !== undefined
          ? { member: pattern.member }
          : {}),
        universal: pattern._tag === 'UniversalPattern',
        guarded: directToken(armNode, 'IfKeyword') !== undefined,
      }),
    ),
  )
  const arms = preliminary.map(({ armNode, armId, pattern }, ordinal): MatchArmFact => {
    const transition = coverage.transitions.at(ordinal)
    if (transition === undefined) throw new RangeError('Match coverage lost an arm')
    if (
      pattern._tag === 'NominalPattern' &&
      pattern.member !== undefined &&
      members !== undefined &&
      !members.some((member) => Type.equals(member, pattern.member ?? member))
    ) {
      diagnostics.push(
        Diagnostic.matchMemberNotInScrutinee(
          Type.encode(pattern.member),
          scrutinee?.type === undefined ? 'unknown' : Type.encode(scrutinee.type),
          pattern.syntax.span,
        ),
      )
    } else if (!transition.reachable && (members?.length ?? 0) > 0) {
      diagnostics.push(
        Diagnostic.unreachableMatchArm(
          pattern._tag === 'UniversalPattern'
            ? '_'
            : pattern.member === undefined
              ? 'unknown'
              : Type.encode(pattern.member),
          armNode.span,
        ),
      )
    }
    const armExpressions = armNode.children.filter(isExpressionNode)
    const guarded = directToken(armNode, 'IfKeyword') !== undefined
    const guardNode = guarded ? armExpressions.at(0) : undefined
    const resultNode = armExpressions.at(guarded ? 1 : 0)
    const armScope: Scope = Object.freeze({
      parameters: scope.parameters,
      bindings: scope.bindings,
      patternBindings: Object.freeze([...scope.patternBindings, ...pattern.bindings]),
    })
    const guard =
      guardNode === undefined
        ? undefined
        : analyzeExpression(source, guardNode, declarations, declaration, armScope, resolution)
    if (guard !== undefined) {
      diagnostics.push(...guard.diagnostics)
      if (guard.type !== undefined && guard.type !== 'Bool') {
        diagnostics.push(
          Diagnostic.matchGuardNotBool(Type.encode(guard.type), guardNode?.span ?? armNode.span),
        )
      }
    }
    const result =
      resultNode === undefined
        ? undefined
        : analyzeExpression(
            source,
            resultNode,
            declarations,
            declaration,
            armScope,
            resolution,
            expected,
          )
    if (result !== undefined) diagnostics.push(...result.diagnostics)
    return Object.freeze({
      _tag: 'MatchArm',
      id: armId,
      pattern,
      bindings: pattern.bindings,
      ...(guard === undefined ? {} : { guard: guard.fact }),
      result: result?.fact ?? unavailableExpression(resultNode ?? armNode),
      before: transition.before,
      after: transition.after,
      reachable: transition.reachable,
      syntax: armNode,
    })
  })
  if (members !== undefined && !coverage.exhaustive) {
    diagnostics.push(Diagnostic.incompleteMatch(coverage.missing.map(Type.encode), node.span))
  }
  const reachableTypes = arms.flatMap((arm) =>
    arm.reachable && arm.result.type._tag === 'Available' ? [arm.result.type.type] : [],
  )
  const unavailableReachableResult = arms.some(
    (arm) => arm.reachable && arm.result.type._tag !== 'Available',
  )
  const joined = Match.join(reachableTypes)
  if (joined._tag === 'Incompatible') {
    diagnostics.push(Diagnostic.incompatibleMatchResults(joined.types.map(Type.encode), node.span))
  }
  const hasInvalidGuard = arms.some(
    (arm) =>
      arm.guard !== undefined &&
      arm.guard.type._tag === 'Available' &&
      arm.guard.type.type !== 'Bool',
  )
  const type =
    members !== undefined &&
    coverage.exhaustive &&
    arms.every(
      (arm) => arm.reachable && (arm.pattern._tag !== 'NominalPattern' || arm.pattern.complete),
    ) &&
    !unavailableReachableResult &&
    !hasInvalidGuard &&
    joined._tag === 'Joined'
      ? availableExpressionType(joined.type)
      : unavailableExpressionType
  const fact: MatchExpressionFact = Object.freeze({
    _tag: 'Match',
    id,
    access,
    scrutinee: scrutinee?.fact ?? unavailableExpression(scrutineeNode ?? node),
    members: Object.freeze([...(members ?? [])]),
    arms: Object.freeze(arms),
    exhaustive: coverage.exhaustive,
    type,
    syntax: node,
  })
  return Object.freeze({
    fact,
    diagnostics: Object.freeze(diagnostics),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

const analyzeStructLiteral = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const targetSyntax = SyntaxTree.directNode(node, 'AppliedType') ?? childNode(node, 'TypePath')
  const target = resolveStructTarget(source, targetSyntax, resolution, declaration)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...target.diagnostics]
  const struct = target.fact._tag === 'Resolved' ? target.fact.struct : undefined
  const nominal = target.fact._tag === 'Resolved' ? target.fact.type : undefined
  const nominalLabel = nominal === undefined ? 'unknown struct' : Type.encode(nominal)
  const structSubstitution =
    struct === undefined || nominal === undefined
      ? new Map<string, SemanticType>()
      : (Type.substitution(
          struct.typeParameters.map((parameter) => parameter.type),
          nominal.arguments,
        ) ?? new Map())
  const authorized = nominal !== undefined && nominal.module === source.id
  if (nominal !== undefined && !authorized) {
    diagnostics.push(Diagnostic.externalRawStructLiteral(Type.encode(nominal), node.span))
  }

  const seen = new Map<string, StructInitializerFact>()
  const initializers = SyntaxTree.directNodes(node, 'StructFieldInitializer').map(
    (initializer): StructInitializerFact => {
      const nameToken = directToken(initializer, 'Identifier')
      const name = nameToken === undefined ? undefined : spelling(source, nameToken)
      const fieldLookup =
        struct === undefined || name === undefined
          ? undefined
          : DeclarationIndex.lookupField(struct.fields, name)
      const expected =
        fieldLookup?._tag === 'Resolved' && fieldLookup.field.declaredType._tag === 'Resolved'
          ? Type.substitute(fieldLookup.field.declaredType.type, structSubstitution)
          : undefined
      const expressionNode = initializer.children.find(isExpressionNode)
      if (expressionNode === undefined) {
        throw new RangeError('Struct initializer requires an expression node')
      }
      const expression = analyzeExpression(
        source,
        expressionNode,
        declarations,
        declaration,
        scope,
        resolution,
        expected,
      )
      if (expression === undefined) {
        throw new RangeError(`Cannot analyze struct initializer ${expressionNode.kind}`)
      }
      diagnostics.push(...expression.diagnostics)
      let state: StructInitializerState = Object.freeze({ _tag: 'Unavailable' })
      if (name !== undefined && nameToken !== undefined && struct !== undefined) {
        const previous = seen.get(name)
        if (fieldLookup?._tag !== 'Resolved') {
          const diagnostic = Diagnostic.unknownStructField(nominalLabel, name, nameToken.span)
          diagnostics.push(diagnostic)
          state = Object.freeze({ _tag: 'Unknown', cause: Diagnostic.identity(diagnostic) })
        } else if (previous !== undefined) {
          const diagnostic = Diagnostic.duplicateStructInitializer(
            name,
            previous.syntax.span,
            nameToken.span,
          )
          diagnostics.push(diagnostic)
          state = Object.freeze({
            _tag: 'Duplicate',
            field: fieldLookup.field,
            cause: Diagnostic.identity(diagnostic),
          })
        } else if (
          fieldLookup.field.declaredType._tag === 'Resolved' &&
          expression.type !== undefined &&
          !typesCompatible(
            expression.type,
            Type.substitute(fieldLookup.field.declaredType.type, structSubstitution),
          )
        ) {
          const expectedType = Type.substitute(
            fieldLookup.field.declaredType.type,
            structSubstitution,
          )
          const diagnostic =
            unionConversionDiagnostic(expression.type, expectedType, expressionNode.span) ??
            Diagnostic.structFieldTypeMismatch(
              name,
              Type.encode(expectedType),
              Type.encode(expression.type),
              expressionNode.span,
            )
          diagnostics.push(diagnostic)
          state = Object.freeze({
            _tag: 'TypeMismatch',
            field: fieldLookup.field,
            cause: Diagnostic.identity(diagnostic),
          })
        } else if (
          fieldLookup.field.declaredType._tag === 'Resolved' &&
          expression.type !== undefined
        ) {
          state = Object.freeze({ _tag: 'Resolved', field: fieldLookup.field })
        }
      }
      const fact: StructInitializerFact = Object.freeze({
        _tag: 'StructInitializer',
        name,
        expression: expression.fact,
        state,
        syntax: initializer,
      })
      if (name !== undefined && !seen.has(name)) seen.set(name, fact)
      return fact
    },
  )

  if (struct !== undefined && nominal !== undefined) {
    for (const field of struct.fields) {
      if (field.name._tag !== 'Present' || seen.has(field.name.spelling)) continue
      diagnostics.push(
        Diagnostic.missingStructInitializer(Type.encode(nominal), field.name.spelling, node.span),
      )
    }
  }

  const fields =
    struct === undefined
      ? []
      : struct.fields.flatMap((field) => {
          if (field.name._tag !== 'Present') return []
          const fieldName = field.name.spelling
          const initializer = initializers.find(
            (candidate) => candidate.name === fieldName && candidate.state._tag === 'Resolved',
          )
          return initializer === undefined ? [] : [{ field, initializer }]
        })
  const complete =
    struct !== undefined &&
    nominal !== undefined &&
    authorized &&
    SyntaxTree.isAvailableSyntax(node) &&
    fields.length === struct.fields.length &&
    initializers.length === struct.fields.length &&
    initializers.every((initializer) => initializer.state._tag === 'Resolved')
  const type =
    complete && nominal !== undefined ? availableExpressionType(nominal) : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'StructLiteral',
      target: target.fact,
      authorized,
      initializers: Object.freeze(initializers),
      fields: Object.freeze(fields),
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: complete ? nominal : undefined,
  })
}

const analyzeArrayLiteral = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
): ExpressionResult => {
  const expectedArray = expected !== undefined && Type.isFixedArray(expected) ? expected : undefined
  const elementNodes = node.children.filter(isExpressionNode)
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  let elementType = expectedArray?.element
  const elements = elementNodes.map((elementNode, ordinal): ArrayElementFact => {
    const element = analyzeExpression(
      source,
      elementNode,
      declarations,
      declaration,
      scope,
      resolution,
      elementType,
    )
    if (element === undefined)
      throw new RangeError(`Cannot analyze array element ${elementNode.kind}`)
    diagnostics.push(...element.diagnostics)
    if (elementType === undefined && element.type !== undefined) elementType = element.type
    let compatibility: ArrayElementFact['compatibility']
    if (element.type === undefined || elementType === undefined) {
      compatibility = Object.freeze({ _tag: 'Unavailable' })
    } else if (!typesCompatible(element.type, elementType)) {
      const diagnostic =
        unionConversionDiagnostic(element.type, elementType, elementNode.span) ??
        Diagnostic.arrayElementTypeMismatch(
          Type.encode(elementType),
          Type.encode(element.type),
          ordinal,
          elementNode.span,
        )
      diagnostics.push(diagnostic)
      compatibility = Object.freeze({
        _tag: 'TypeMismatch',
        expected: elementType,
        actual: element.type,
      })
    } else {
      compatibility = Object.freeze({ _tag: 'Compatible' })
    }
    return Object.freeze({
      _tag: 'ArrayElement',
      ordinal,
      expression: element.fact,
      ...(elementType === undefined ? {} : { expected: elementType }),
      compatibility,
      syntax: elementNode,
    })
  })

  const actualLength = elements.length
  let state: ArrayLiteralState
  if (elementType === undefined && actualLength === 0) {
    const diagnostic = Diagnostic.emptyArrayNeedsContext(node.span)
    diagnostics.push(diagnostic)
    state = Object.freeze({ _tag: 'MissingContext' })
  } else if (expectedArray !== undefined && expectedArray.length !== actualLength) {
    const diagnostic = Diagnostic.arrayLengthMismatch(expectedArray.length, actualLength, node.span)
    diagnostics.push(diagnostic)
    state = Object.freeze({
      _tag: 'LengthMismatch',
      expected: expectedArray.length,
      actual: actualLength,
    })
  } else if (elements.some((element) => element.compatibility._tag === 'TypeMismatch')) {
    state = Object.freeze({ _tag: 'IncompatibleElements' })
  } else if (
    elementType === undefined ||
    elements.some((element) => element.compatibility._tag === 'Unavailable') ||
    !SyntaxTree.isAvailableSyntax(node)
  ) {
    state = Object.freeze({ _tag: 'Unavailable' })
  } else {
    state = Object.freeze({
      _tag: 'Complete',
      type: expectedArray ?? Type.fixedArray(elementType, actualLength),
    })
  }
  const type =
    state._tag === 'Complete' ? availableExpressionType(state.type) : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'ArrayLiteral',
      elements: Object.freeze(elements),
      ...(expectedArray === undefined ? {} : { expected: expectedArray }),
      ...(elementType === undefined ? {} : { elementType }),
      length: actualLength,
      state,
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

const analyzeIndexProjection = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const expressions = node.children.filter(isExpressionNode)
  const subjectNode = expressions.at(0)
  const indexNode = expressions.at(1)
  if (subjectNode === undefined || indexNode === undefined) {
    throw new RangeError('Index projection requires subject and index expressions')
  }
  const subject = analyzeExpression(
    source,
    subjectNode,
    declarations,
    declaration,
    scope,
    resolution,
  )
  const index = analyzeExpression(
    source,
    indexNode,
    declarations,
    declaration,
    scope,
    resolution,
    'I32',
  )
  if (subject === undefined || index === undefined) {
    throw new RangeError('Cannot analyze index projection operands')
  }
  const diagnostics: Array<Diagnostic.Diagnostic> = [...subject.diagnostics, ...index.diagnostics]
  const array =
    subject.type !== undefined && Type.isFixedArray(subject.type) ? subject.type : undefined
  if (subject.type !== undefined && array === undefined) {
    diagnostics.push(Diagnostic.indexOnNonArray(Type.encode(subject.type), subjectNode.span))
  }
  if (index.type !== undefined && index.type !== 'I32') {
    diagnostics.push(Diagnostic.indexNotI32(Type.encode(index.type), indexNode.span))
  }
  let bounds: BoundsFact = Object.freeze({ _tag: 'Unavailable' })
  if (array !== undefined && index.type === 'I32') {
    const literal =
      index.fact._tag === 'Integer' && index.fact.integer._tag === 'Available'
        ? index.fact.integer.value
        : undefined
    if (literal === undefined) bounds = Object.freeze({ _tag: 'Runtime', length: array.length })
    else if (literal < 0 || literal >= array.length) {
      const diagnostic = Diagnostic.indexOutOfBounds(literal, array.length, indexNode.span)
      diagnostics.push(diagnostic)
      bounds = Object.freeze({
        _tag: 'Invalid',
        index: literal,
        length: array.length,
        cause: Diagnostic.identity(diagnostic),
      })
    } else bounds = Object.freeze({ _tag: 'Proven', index: literal, length: array.length })
  }
  const available =
    array !== undefined &&
    index.type === 'I32' &&
    bounds._tag !== 'Invalid' &&
    bounds._tag !== 'Unavailable' &&
    SyntaxTree.isAvailableSyntax(node)
  const type = available ? availableExpressionType(array.element) : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'IndexProjection',
      subject: subject.fact,
      index: index.fact,
      ...(array === undefined ? {} : { array, elementType: array.element }),
      access: 'CopyRead',
      bounds,
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

const analyzeProjection = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const subjectNode = node.children.find(isExpressionNode)
  if (subjectNode === undefined) throw new RangeError('Projection requires a subject expression')
  const subject = analyzeExpression(
    source,
    subjectNode,
    declarations,
    declaration,
    scope,
    resolution,
  )
  if (subject === undefined) throw new RangeError(`Cannot analyze projection ${subjectNode.kind}`)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...subject.diagnostics]
  const fieldToken = directToken(node, 'Identifier')
  const fieldName = fieldToken === undefined ? undefined : spelling(source, fieldToken)
  const nominal =
    subject.type !== undefined && Type.isNominal(subject.type) ? subject.type : undefined
  let state: ProjectionState = Object.freeze({ _tag: 'Unavailable' })
  let type: SemanticType | undefined
  if (subject.type !== undefined && nominal === undefined && fieldToken !== undefined) {
    const diagnostic = Diagnostic.projectionOnNonStruct(Type.encode(subject.type), fieldToken.span)
    diagnostics.push(diagnostic)
    state = Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) })
  } else if (nominal !== undefined && fieldName !== undefined && fieldToken !== undefined) {
    const member = DeclarationIndex.byCanonical(resolution.index, {
      _tag: 'CanonicalDeclarationId',
      module: nominal.module,
      name: nominal.name,
    })
    const struct = member?._tag === 'StructDeclaration' ? member : undefined
    const lookup =
      struct === undefined ? undefined : DeclarationIndex.lookupField(struct.fields, fieldName)
    if (lookup?._tag !== 'Resolved') {
      const diagnostic = Diagnostic.unknownProjectedField(
        Type.encode(nominal),
        fieldName,
        fieldToken.span,
      )
      diagnostics.push(diagnostic)
      state = Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) })
    } else if (lookup.field.visibility === 'Private' && nominal.module !== source.id) {
      const diagnostic = Diagnostic.inaccessibleProjectedField(
        Type.encode(nominal),
        fieldName,
        fieldToken.span,
      )
      diagnostics.push(diagnostic)
      state = Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) })
    } else if (lookup.field.declaredType._tag === 'Resolved') {
      state = Object.freeze({ _tag: 'Resolved', field: lookup.field })
      const substitution =
        struct === undefined
          ? new Map<string, SemanticType>()
          : (Type.substitution(
              struct.typeParameters.map((parameter) => parameter.type),
              nominal.arguments,
            ) ?? new Map())
      type = Type.substitute(lookup.field.declaredType.type, substitution)
    }
  }
  const typeFact = type === undefined ? unavailableExpressionType : availableExpressionType(type)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FieldProjection',
      subject: subject.fact,
      ...(nominal === undefined ? {} : { nominal }),
      fieldName,
      state,
      type: typeFact,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type,
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
  expectedTypes: ReadonlyArray<SemanticType | undefined> = Object.freeze([]),
): ArgumentsResult => {
  const analyzed = nodes.flatMap((element, ordinal): ReadonlyArray<ExpressionResult> => {
    const result = analyzeExpression(
      source,
      element,
      declarations,
      declaration,
      scope,
      resolution,
      expectedTypes.at(ordinal),
    )
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
  callTypeArguments?: CallTypeArgumentsResult,
): ArgumentsResult {
  const argumentList = childNode(call, 'ArgumentList')
  const identifiers = call.children.filter(
    (element): element is Token.Token =>
      SyntaxTree.isToken(element) && element.kind === 'Identifier',
  )
  const first = identifiers.at(0)
  const second = identifiers.at(1)
  let target: DeclarationFact | undefined
  if (first !== undefined && second === undefined) {
    const name = spelling(source, first)
    const resolved = NameResolution.lookup(resolution.scope, resolution.index, name)
    const local = lookupDeclaration(declarations, name)
    target =
      resolved._tag === 'Resolved' && resolved.declaration._tag === 'FunctionDeclaration'
        ? resolved.declaration
        : local._tag === 'Resolved'
          ? local.declaration
          : undefined
  } else if (first !== undefined && second !== undefined) {
    const qualifier = NameResolution.lookup(
      resolution.scope,
      resolution.index,
      spelling(source, first),
    )
    if (qualifier._tag === 'Namespace') {
      const member = DeclarationIndex.lookup(
        resolution.index,
        qualifier.module,
        spelling(source, second),
      )
      target = member._tag === 'Resolved' ? member.declaration : undefined
    }
  }
  const declaredTypeParameters =
    target?.typeParameters.map((parameter) => parameter.type) ?? Object.freeze([])
  const explicitTypes = callTypeArguments?.types
  const substitution =
    callTypeArguments?.explicit === true &&
    explicitTypes !== undefined &&
    explicitTypes.length === declaredTypeParameters.length
      ? Type.substitution(declaredTypeParameters, explicitTypes)
      : undefined
  const expectedTypes = Object.freeze(
    (target?.parameters ?? []).map((parameter) =>
      parameter.declaredType._tag === 'Resolved'
        ? Type.substitute(parameter.declaredType.type, substitution ?? new Map())
        : undefined,
    ),
  )
  return analyzeArgumentNodes(
    source,
    call,
    argumentList.children.filter(isRecursiveArgumentNode),
    declarations,
    declaration,
    scope,
    resolution,
    expectedTypes,
  )
}

interface CallContractResult {
  readonly mappings: ReadonlyArray<ArgumentMappingFact>
  readonly fact: CallContractFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

interface CallTypeArgumentsResult {
  readonly explicit: boolean
  readonly facts: ReadonlyArray<TypeArgumentFact>
  readonly types?: ReadonlyArray<SemanticType>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const analyzeCallTypeArguments = (
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  caller: DeclarationFact,
  resolution: ResolutionContext,
): CallTypeArgumentsResult => {
  const list = SyntaxTree.directNode(call, 'CallTypeArgumentList')
  if (list === undefined) {
    return Object.freeze({
      explicit: false,
      facts: Object.freeze([]),
      diagnostics: Object.freeze([]),
    })
  }
  const environment = new Map(
    caller.typeParameters.flatMap((parameter) =>
      parameter.name._tag === 'Present' ? [[parameter.name.spelling, parameter.type] as const] : [],
    ),
  )
  const nameResolution: NameResolution.Resolution = Object.freeze({
    _tag: 'NameResolution',
    modules: Object.freeze([resolution.scope]),
    diagnostics: Object.freeze([]),
  })
  const nodes = list.children.filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) &&
      (element.kind === 'TypePath' ||
        element.kind === 'AppliedType' ||
        element.kind === 'FixedArrayType' ||
        element.kind === 'ParenthesizedType' ||
        element.kind === 'UnionType'),
  )
  const analyzed = nodes.map((node, ordinal) => {
    const raw = DeclarationIndex.analyzeDeclaredType(source, node, environment)
    const resolved = DeclarationIndex.resolveTypeFact(
      resolution.index,
      source.id,
      raw.fact,
      (module, path) => NameResolution.resolveType(nameResolution, resolution.index, module, path),
    )
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'TypeArgument' as const,
        ordinal,
        syntax: node,
        declared: resolved.fact,
        ...(resolved.fact._tag === 'Resolved' ? { type: resolved.fact.type } : {}),
      }),
      diagnostics: Diagnostic.merge(raw.diagnostics, resolved.diagnostics),
    })
  })
  const facts = Object.freeze(analyzed.map((entry) => entry.fact))
  const available = facts.map((fact) => fact.type)
  return Object.freeze({
    explicit: true,
    facts,
    ...(available.every((type) => type !== undefined)
      ? {
          types: Object.freeze(
            available.filter((type): type is SemanticType => type !== undefined),
          ),
        }
      : {}),
    diagnostics: Diagnostic.merge(...analyzed.map((entry) => entry.diagnostics)),
  })
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
  callTypeArguments?: CallTypeArgumentsResult,
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
        !typesCompatible(argument.type.type, expected)
      ) {
        const mismatch =
          unionConversionDiagnostic(argument.type.type, expected, argument.syntax.span) ??
          Diagnostic.argumentTypeMismatch(
            Type.encode(expected),
            Type.encode(argument.type.type),
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
      fact: Object.freeze({
        _tag: 'Compatible',
        expectedCount,
        actualCount,
        typeArguments: Object.freeze([]),
        substitution: new Map(),
      }),
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
  const declaredTypeParameters = reference.declaration.typeParameters.map(
    (parameter) => parameter.type,
  )
  let substitution: ReadonlyMap<string, SemanticType>
  let typeArguments: ReadonlyArray<SemanticType>
  if (callTypeArguments?.explicit === true) {
    if (callTypeArguments.facts.length !== declaredTypeParameters.length) {
      const diagnostic = Diagnostic.typeArgumentArity(
        reference.spelling,
        declaredTypeParameters.length,
        callTypeArguments.facts.length,
        call.span,
      )
      return Object.freeze({
        mappings,
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
          cause: Diagnostic.identity(diagnostic),
        }),
        diagnostics: Object.freeze([diagnostic]),
      })
    }
    if (callTypeArguments.types === undefined) {
      const unavailable = callTypeArguments.facts.find((fact) => fact.type === undefined)
      const cause =
        unavailable !== undefined && 'cause' in unavailable.declared
          ? unavailable.declared.cause
          : undefined
      return Object.freeze({
        mappings,
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
          ...(cause === undefined ? {} : { cause }),
        }),
        diagnostics: Object.freeze([]),
      })
    }
    typeArguments = Object.freeze(Array.from(callTypeArguments.types))
    const explicitSubstitution = Type.substitution(declaredTypeParameters, typeArguments)
    if (explicitSubstitution === undefined) {
      throw new RangeError('validated call type arguments lost their declaration arity')
    }
    substitution = explicitSubstitution
  } else if (declaredTypeParameters.length === 0) {
    typeArguments = Object.freeze([])
    substitution = new Map()
  } else {
    const inferred = new Map<string, SemanticType>()
    const compatible = mappings.every(
      (mapping) =>
        mapping.argument.type._tag === 'Available' &&
        mapping.parameter.declaredType._tag === 'Resolved' &&
        Type.infer(mapping.parameter.declaredType.type, mapping.argument.type.type, inferred),
    )
    typeArguments = Object.freeze(
      declaredTypeParameters.flatMap((parameter) => {
        const inferredType = inferred.get(Type.key(parameter))
        return inferredType === undefined ? [] : [inferredType]
      }),
    )
    if (!compatible || typeArguments.length !== declaredTypeParameters.length) {
      const diagnostic = Diagnostic.typeArgumentInference(reference.spelling, call.span)
      return Object.freeze({
        mappings,
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
          cause: Diagnostic.identity(diagnostic),
        }),
        diagnostics: Object.freeze([diagnostic]),
      })
    }
    substitution = inferred
  }
  for (const mapping of mappings) {
    if (
      mapping.argument.type._tag !== 'Available' ||
      mapping.parameter.declaredType._tag !== 'Resolved'
    ) {
      continue
    }
    const expected = Type.substitute(mapping.parameter.declaredType.type, substitution)
    if (!typesCompatible(mapping.argument.type.type, expected)) {
      const mismatch =
        unionConversionDiagnostic(
          mapping.argument.type.type,
          expected,
          mapping.argument.syntax.span,
        ) ??
        Diagnostic.argumentTypeMismatch(
          Type.encode(expected),
          Type.encode(mapping.argument.type.type),
          mapping.argument.syntax.span,
        )
      return Object.freeze({
        mappings,
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({
            _tag: 'ArgumentTypeMismatch',
            argument: mapping.argument,
            expected,
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
    fact: Object.freeze({
      _tag: 'Compatible',
      expectedCount,
      actualCount,
      typeArguments,
      substitution,
    }),
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
  typeArguments: CallTypeArgumentsResult,
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
        typeArguments: typeArguments.facts,
        arguments: argumentsResult.facts,
        mappings: Object.freeze([]),
        contract: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
        }),
        type: unavailableExpressionType,
        syntax: call,
      }),
      diagnostics: Object.freeze([...argumentsResult.diagnostics, ...typeArguments.diagnostics]),
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
  const specializationDiagnostic = typeArguments.explicit
    ? Diagnostic.typeArgumentArity(
        `${actorSpelling}.${operationSpelling}`,
        0,
        typeArguments.types?.length ?? 0,
        call.span,
      )
    : undefined
  const expressionType =
    hasAvailableCallSyntax(call) &&
    reference._tag === 'ResolvedBuiltin' &&
    specializationDiagnostic === undefined
      ? availableExpressionType(reference.result)
      : unavailableExpressionType

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Call',
      reference,
      typeArguments: typeArguments.facts,
      arguments: argumentsResult.facts,
      mappings: callContract.mappings,
      contract: callContract.fact,
      type: expressionType,
      syntax: call,
    }),
    diagnostics: Object.freeze([
      ...(missingDiagnostic === undefined ? [] : [missingDiagnostic]),
      ...(specializationDiagnostic === undefined ? [] : [specializationDiagnostic]),
      ...argumentsResult.diagnostics,
      ...typeArguments.diagnostics,
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
  const qualifierLookup = NameResolution.lookup(resolution.scope, resolution.index, qualifier)

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
  expected?: SemanticType,
): ExpressionResult => {
  const child = node.children.find(isExpressionNode)
  const expression =
    child === undefined
      ? undefined
      : analyzeExpression(source, child, declarations, declaration, scope, resolution, expected)
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
  expected?: SemanticType,
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
    const move = analyzeMove(source, node, declarations, declaration, scope, resolution)
    return Object.freeze({
      fact: move.fact,
      diagnostics: move.diagnostics,
      type: move.type,
    })
  }

  if (node.kind === 'MatchExpression') {
    return analyzeMatch(source, node, declarations, declaration, scope, resolution, expected)
  }

  if (node.kind === 'StructLiteralExpression') {
    return analyzeStructLiteral(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind === 'ArrayLiteralExpression') {
    return analyzeArrayLiteral(source, node, declarations, declaration, scope, resolution, expected)
  }

  if (node.kind === 'FieldProjectionExpression') {
    return analyzeProjection(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind === 'IndexProjectionExpression') {
    return analyzeIndexProjection(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind === 'GroupedExpression') {
    return analyzeGroupedExpression(
      source,
      node,
      declarations,
      declaration,
      scope,
      resolution,
      expected,
    )
  }

  if (node.kind === 'PrefixExpression' || node.kind === 'InfixExpression') {
    return analyzeOperatorExpression(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind === 'PipelineExpression') {
    return analyzePipelineExpression(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind !== 'CallExpression') return undefined

  const callTypeArguments = analyzeCallTypeArguments(source, node, declaration, resolution)
  const argumentsResult = analyzeArguments(
    source,
    node,
    declarations,
    declaration,
    scope,
    resolution,
    callTypeArguments,
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
      return analyzeBuiltinCall(source, node, argumentsResult, callTypeArguments)
    const qualifier = spelling(source, qualifierToken)
    const member = spelling(source, memberToken)
    const qualifierLookup = NameResolution.lookup(resolution.scope, resolution.index, qualifier)
    if (qualifierLookup._tag === 'Intrinsic')
      return analyzeBuiltinCall(source, node, argumentsResult, callTypeArguments)
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
      return finishDeclarationCall(node, reference, argumentsResult, callTypeArguments, diagnostic)
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
    return finishDeclarationCall(node, reference, argumentsResult, callTypeArguments, diagnostic)
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
        typeArguments: callTypeArguments.facts,
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
  const resolvedLookup = NameResolution.lookup(resolution.scope, resolution.index, tokenSpelling)
  const localLookup = lookupDeclaration(declarations, tokenSpelling)
  const lookup: DeclarationIndex.DeclarationLookup =
    resolvedLookup._tag === 'Conflict'
      ? Object.freeze({
          _tag: 'Ambiguous',
          spelling: tokenSpelling,
          declarations: Object.freeze(
            resolvedLookup.conflict.bindings.flatMap((binding) => {
              if (binding._tag !== 'LocalDeclaration' && binding._tag !== 'ImportedMember')
                return []
              const declaration = DeclarationIndex.byCanonical(
                resolution.index,
                binding.declaration,
              )
              return declaration?._tag === 'FunctionDeclaration' ? [declaration] : []
            }),
          ),
        })
      : localLookup._tag === 'Ambiguous'
        ? localLookup
        : resolvedLookup._tag === 'Resolved' &&
            resolvedLookup.declaration._tag === 'FunctionDeclaration'
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
  const callContract = analyzeCallContract(
    node,
    reference,
    argumentsResult.facts,
    hasAvailableCallSyntax(node),
    callTypeArguments,
  )
  const syntaxAvailable = hasAvailableCallSyntax(node)
  const expressionType =
    syntaxAvailable &&
    reference._tag === 'Resolved' &&
    reference.declaration.returnType._tag === 'Resolved'
      ? availableExpressionType(
          callContract.fact._tag === 'Compatible'
            ? Type.substitute(reference.declaration.returnType.type, callContract.fact.substitution)
            : reference.declaration.returnType.type,
        )
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Call',
      reference,
      typeArguments: callTypeArguments.facts,
      arguments: argumentsResult.facts,
      mappings: callContract.mappings,
      contract: callContract.fact,
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      ...(missingDiagnostic === undefined ? [] : [missingDiagnostic]),
      ...argumentsResult.diagnostics,
      ...callTypeArguments.diagnostics,
      ...callContract.diagnostics,
    ]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

const finishDeclarationCall = (
  node: SyntaxTree.Node,
  reference: CallReferenceFact,
  argumentsResult: ArgumentsResult,
  callTypeArguments: CallTypeArgumentsResult,
  diagnostic: Diagnostic.Diagnostic | undefined,
): ExpressionResult => {
  const callContract = analyzeCallContract(
    node,
    reference,
    argumentsResult.facts,
    hasAvailableCallSyntax(node),
    callTypeArguments,
  )
  const expressionType =
    hasAvailableCallSyntax(node) &&
    reference._tag === 'Resolved' &&
    reference.declaration.returnType._tag === 'Resolved'
      ? availableExpressionType(
          callContract.fact._tag === 'Compatible'
            ? Type.substitute(reference.declaration.returnType.type, callContract.fact.substitution)
            : reference.declaration.returnType.type,
        )
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Call',
      reference,
      typeArguments: callTypeArguments.facts,
      arguments: argumentsResult.facts,
      mappings: callContract.mappings,
      contract: callContract.fact,
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      ...(diagnostic === undefined ? [] : [diagnostic]),
      ...argumentsResult.diagnostics,
      ...callTypeArguments.diagnostics,
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
  for (const binding of scope.patternBindings) {
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
  readonly regions: Array<Hir.RegionId>
  readonly loops: Array<Hir.LoopId>
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
  loopStack: ReadonlyArray<Hir.LoopId> = Object.freeze([]),
): ReadonlyArray<StatementFact> => {
  const facts: Array<StatementFact> = []
  let scope = initialScope

  const nextRegion = (): Hir.RegionId => {
    const region = Object.freeze({
      _tag: 'HirRegion' as const,
      function: context.declaration.id,
      ordinal: context.regions.length,
    })
    context.regions.push(region)
    return region
  }

  const assignmentRoot = (fact: ExpressionFact): BindingDeclarationFact | undefined => {
    if (fact._tag === 'Identifier') {
      return fact.reference._tag === 'ResolvedBinding' ? fact.reference.binding : undefined
    }
    if (fact._tag === 'FieldProjection' || fact._tag === 'IndexProjection') {
      return assignmentRoot(fact.subject)
    }
    if (fact._tag === 'Grouped') return assignmentRoot(fact.expression)
    return undefined
  }

  for (const element of blockNode.children) {
    if (!SyntaxTree.isNode(element)) continue

    if (element.kind === 'BindingStatement') {
      const region = nextRegion()
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
        mutability:
          SyntaxTree.directToken(element, 'MutKeyword') === undefined ? 'Immutable' : 'Mutable',
        inferredType: initializer.fact.type,
        initializer: initializer.fact,
        syntax: element,
      })
      context.bindings.push(binding)
      facts.push(Object.freeze({ _tag: 'BindStatement', binding, region }))

      if (name._tag === 'Present') {
        const originalSpan = scopeSpanFor(scope, name.spelling)
        if (originalSpan === undefined) {
          scope = Object.freeze({
            parameters: scope.parameters,
            bindings: Object.freeze([...scope.bindings, binding]),
            patternBindings: scope.patternBindings,
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
      const region = nextRegion()
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
          Diagnostic.conditionNotBool(
            Type.encode(condition.fact.type.type),
            condition.fact.syntax.span,
          ),
        )
      }

      const arms = SyntaxTree.directNodes(element, 'Block')
      const taken =
        arms.at(0) === undefined
          ? []
          : analyzeStatements(context, arms[0] as SyntaxTree.Node, scope, loopStack)
      const otherwiseArm = arms.at(1)
      const otherwise =
        otherwiseArm === undefined ? [] : analyzeStatements(context, otherwiseArm, scope, loopStack)
      facts.push(
        Object.freeze({
          _tag: 'IfStatement',
          condition: condition.fact,
          taken: Object.freeze([...taken]),
          otherwise: Object.freeze([...otherwise]),
          region,
          syntax: element,
        }),
      )
      continue
    }

    if (element.kind === 'AssignmentStatement') {
      const region = nextRegion()
      const nodes = element.children.filter(
        (child): child is SyntaxTree.Node => SyntaxTree.isNode(child) && isExpressionNode(child),
      )
      const destinationNode = nodes.at(0)
      const valueNode = nodes.at(1)
      if (destinationNode === undefined || valueNode === undefined) {
        context.diagnostics.push(Diagnostic.invalidAssignmentPlace(element.span))
        continue
      }
      const destination = analyzeExpression(
        context.source,
        destinationNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (destination === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${destinationNode.kind}`)
      }
      context.diagnostics.push(...destination.diagnostics)
      const value = analyzeExpression(
        context.source,
        valueNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
        destination.type,
      )
      if (value === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${valueNode.kind}`)
      }
      context.diagnostics.push(...value.diagnostics)
      const root = assignmentRoot(destination.fact)
      if (root === undefined) {
        context.diagnostics.push(Diagnostic.invalidAssignmentPlace(destinationNode.span))
      } else if (root.mutability === 'Immutable') {
        context.diagnostics.push(
          Diagnostic.immutableAssignment(
            root.name._tag === 'Present' ? root.name.spelling : '?',
            destinationNode.span,
          ),
        )
      }
      const compatible =
        destination.type !== undefined &&
        value.type !== undefined &&
        typesCompatible(value.type, destination.type)
      if (destination.type !== undefined && value.type !== undefined && !compatible) {
        context.diagnostics.push(
          unionConversionDiagnostic(value.type, destination.type, valueNode.span) ??
            Diagnostic.assignmentTypeMismatch(
              Type.encode(destination.type),
              Type.encode(value.type),
              valueNode.span,
            ),
        )
      }
      facts.push(
        Object.freeze({
          _tag: 'WriteStatement',
          destination: destination.fact,
          ...(root === undefined ? {} : { root }),
          value: value.fact,
          compatible,
          region,
          syntax: element,
        }),
      )
      continue
    }

    if (element.kind === 'WhileStatement') {
      const region = nextRegion()
      const loop = Object.freeze({
        _tag: 'HirLoop' as const,
        function: context.declaration.id,
        ordinal: context.loops.length,
      })
      context.loops.push(loop)
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
          Diagnostic.conditionNotBool(Type.encode(condition.fact.type.type), conditionNode.span),
        )
      }
      const bodyNode = SyntaxTree.directNode(element, 'Block')
      const body =
        bodyNode === undefined
          ? []
          : analyzeStatements(context, bodyNode, scope, Object.freeze([...loopStack, loop]))
      const parent = loopStack.at(-1)
      facts.push(
        Object.freeze({
          _tag: 'WhileStatement',
          loop,
          ...(parent === undefined ? {} : { parent }),
          condition: condition.fact,
          body: Object.freeze([...body]),
          region,
          syntax: element,
        }),
      )
      continue
    }

    if (element.kind === 'BreakStatement' || element.kind === 'ContinueStatement') {
      const region = nextRegion()
      const target = loopStack.at(-1)
      if (target === undefined) {
        context.diagnostics.push(
          Diagnostic.transferOutsideLoop(
            element.kind === 'BreakStatement' ? 'break' : 'continue',
            element.span,
          ),
        )
      }
      facts.push(
        Object.freeze({
          _tag: element.kind,
          ...(target === undefined ? {} : { target }),
          region,
          syntax: element,
        }),
      )
      continue
    }

    if (element.kind === 'ReturnStatement') {
      const region = nextRegion()
      const expressionNode = statementExpressionNode(element)
      const expression = analyzeExpression(
        context.source,
        expressionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
        context.declaration.returnType._tag === 'Resolved'
          ? context.declaration.returnType.type
          : undefined,
      )
      if (expression === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${expressionNode.kind}`)
      }
      context.diagnostics.push(...expression.diagnostics)
      if (
        context.declaration.returnType._tag === 'Resolved' &&
        expression.type !== undefined &&
        !typesCompatible(expression.type, context.declaration.returnType.type)
      ) {
        const diagnostic = unionConversionDiagnostic(
          expression.type,
          context.declaration.returnType.type,
          expressionNode.span,
        )
        if (diagnostic !== undefined) context.diagnostics.push(diagnostic)
      }
      facts.push(
        Object.freeze({
          _tag: 'ReturnStatement',
          expression: expression.fact,
          region,
          syntax: element,
        }),
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
    regions: [],
    loops: [],
    resolution,
  }
  const statements = analyzeStatements(
    context,
    blockNode,
    Object.freeze({ parameters: declaration.parameters, bindings: [], patternBindings: [] }),
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
    declaration.returnType._tag === 'Resolved' &&
    expressionType !== undefined &&
    typesCompatible(expressionType, declaration.returnType.type)
      ? compatible
      : unavailableCompatibility

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FunctionFact',
      declaration,
      statements,
      bindings: Object.freeze([...context.bindings]),
      regionOrder: Object.freeze([...context.regions]),
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
  if (reference._tag === 'ResolvedPattern' && type._tag === 'Available') {
    return Object.freeze({
      _tag: 'PatternBindingReference',
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
    const subject = hirExpression(fact.subject)
    if (subject._tag === 'Unavailable' || fact.type._tag !== 'Available') {
      return subject._tag === 'Unavailable'
        ? subject
        : Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    }
    return Object.freeze({
      _tag: 'Move',
      subject:
        subject._tag === 'Project' || subject._tag === 'IndexPlace'
          ? Object.freeze({ ...subject, access: 'ConsumeRequested' as const })
          : subject,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'Match') {
    const scrutinee = hirExpression(fact.scrutinee)
    if (scrutinee._tag === 'Unavailable' || fact.type._tag !== 'Available') {
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    }
    const target = fact.type.type
    return Object.freeze({
      _tag: 'Match',
      id: fact.id,
      access: fact.access,
      scrutinee,
      members: fact.members,
      arms: Object.freeze(
        fact.arms.map((arm) => {
          const member = arm.pattern._tag === 'NominalPattern' ? arm.pattern.member : undefined
          return Object.freeze({
            id: arm.id,
            ...(member === undefined ? {} : { member }),
            universal: arm.pattern._tag === 'UniversalPattern',
            bindings: Object.freeze(
              arm.bindings.flatMap((binding) =>
                binding.type._tag === 'Available'
                  ? [
                      Object.freeze({
                        id: binding.id,
                        ...(binding.name._tag === 'Present' ? { name: binding.name.spelling } : {}),
                        field: binding.field.id,
                        path: binding.path,
                        type: binding.type.type,
                        access: binding.access,
                        span: binding.syntax.span,
                      }),
                    ]
                  : [],
              ),
            ),
            cleanup: arm.pattern.omitted,
            ...(arm.guard === undefined ? {} : { guard: hirExpression(arm.guard) }),
            result: Type.isUnion(target)
              ? hirExpectedExpression(arm.result, target, 'MatchArm', arm.syntax.span)
              : hirExpression(arm.result),
            before: arm.before,
            after: arm.after,
            reachable: arm.reachable,
            span: arm.syntax.span,
          })
        }),
      ),
      type: target,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'StructLiteral') {
    if (
      fact.target._tag !== 'Resolved' ||
      fact.type._tag !== 'Available' ||
      fact.fields.length !== fact.target.struct.fields.length
    ) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: fact.syntax.span,
        ...(fact.target._tag === 'Unavailable' && fact.target.cause !== undefined
          ? { cause: fact.target.cause }
          : {}),
      })
    }
    const substitution =
      Type.substitution(
        fact.target.struct.typeParameters.map((parameter) => parameter.type),
        fact.target.type.arguments,
      ) ?? new Map()
    return Object.freeze({
      _tag: 'Construct',
      nominal: fact.target.type,
      evaluationOrder: Object.freeze(
        fact.initializers.flatMap((initializer) =>
          initializer.state._tag === 'Resolved' ? [initializer.state.field.id] : [],
        ),
      ),
      fields: Object.freeze(
        fact.fields.map(({ field, initializer }) => {
          const value =
            field.declaredType._tag === 'Resolved'
              ? hirExpectedExpression(
                  initializer.expression,
                  Type.substitute(field.declaredType.type, substitution),
                  'StructField',
                  field.syntax.span,
                )
              : hirExpression(initializer.expression)
          return Object.freeze({ field: field.id, value })
        }),
      ),
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'ArrayLiteral') {
    if (fact.state._tag !== 'Complete' || fact.type._tag !== 'Available') {
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    }
    return Object.freeze({
      _tag: 'ArrayConstruct',
      elements: Object.freeze(
        fact.elements.map((element) =>
          element.expected === undefined
            ? hirExpression(element.expression)
            : hirExpectedExpression(
                element.expression,
                element.expected,
                'ArrayElement',
                element.syntax.span,
              ),
        ),
      ),
      type: fact.state.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'FieldProjection') {
    if (
      fact.nominal === undefined ||
      fact.state._tag !== 'Resolved' ||
      fact.type._tag !== 'Available'
    ) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: fact.syntax.span,
        ...(fact.state._tag === 'Unavailable' && fact.state.cause !== undefined
          ? { cause: fact.state.cause }
          : {}),
      })
    }
    return Object.freeze({
      _tag: 'Project',
      subject: hirExpression(fact.subject),
      nominal: fact.nominal,
      field: fact.state.field.id,
      access: 'CopyRead',
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'IndexProjection') {
    if (
      fact.array === undefined ||
      fact.type._tag !== 'Available' ||
      (fact.bounds._tag !== 'Proven' && fact.bounds._tag !== 'Runtime')
    ) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: fact.syntax.span,
        ...(fact.bounds._tag === 'Invalid' ? { cause: fact.bounds.cause } : {}),
      })
    }
    const subject = hirExpression(fact.subject)
    const index = hirExpression(fact.index)
    if (subject._tag === 'Unavailable' || index._tag === 'Unavailable') {
      return subject._tag === 'Unavailable' ? subject : index
    }
    return Object.freeze({
      _tag: 'IndexPlace',
      subject,
      index,
      array: fact.array,
      access: fact.access,
      bounds: fact.bounds,
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
    const target = fact.reference.declaration
    const substitution = fact.contract.substitution
    return Object.freeze({
      _tag: 'Call',
      target: fact.reference.declaration.canonical.id,
      typeArguments: fact.contract.typeArguments,
      arguments: Object.freeze(
        fact.arguments.map((argument, ordinal) => {
          const parameter = target.parameters.at(ordinal)
          return parameter?.declaredType._tag === 'Resolved'
            ? hirExpectedExpression(
                argument.expression,
                Type.substitute(parameter.declaredType.type, substitution),
                'Argument',
                parameter.syntax.span,
              )
            : hirExpression(argument.expression)
        }),
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

const hirExpectedExpression = (
  fact: ExpressionFact,
  target: SemanticType,
  context: Extract<Hir.Expression, { readonly _tag: 'UnionConvert' }>['context'],
  expectedAt: SourceSpan.SourceSpan,
): Hir.Expression => {
  const source = hirExpression(fact)
  if (source._tag === 'Unavailable') return source
  const compatibility = TypeCompatibility.check(source.type, target)
  if (compatibility._tag === 'Exact') return source
  if (compatibility._tag === 'Incompatible') {
    return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
  }
  return Object.freeze({
    _tag: 'UnionConvert',
    source,
    sourceType: compatibility.source,
    target: compatibility.target,
    conversion: compatibility._tag,
    mappings: compatibility.mappings,
    access: 'Owned',
    context,
    expectedAt,
    type: compatibility.target,
    span: fact.syntax.span,
  })
}

const hirWritePlace = (
  fact: ExpressionFact,
  root: BindingDeclarationFact,
): Hir.WritePlace | undefined => {
  const selectors: Array<Hir.WriteSelector> = []
  const walk = (current: ExpressionFact): boolean => {
    if (current._tag === 'Grouped') return walk(current.expression)
    if (current._tag === 'Identifier') {
      return (
        current.reference._tag === 'ResolvedBinding' &&
        current.reference.binding.id.ordinal === root.id.ordinal
      )
    }
    if (current._tag === 'FieldProjection') {
      if (
        !walk(current.subject) ||
        current.state._tag !== 'Resolved' ||
        current.type._tag !== 'Available'
      ) {
        return false
      }
      selectors.push(
        Object.freeze({
          _tag: 'Field',
          field: current.state.field.id,
          type: current.type.type,
          span: current.syntax.span,
        }),
      )
      return true
    }
    if (current._tag === 'IndexProjection') {
      if (
        !walk(current.subject) ||
        current.array === undefined ||
        current.type._tag !== 'Available' ||
        (current.bounds._tag !== 'Proven' && current.bounds._tag !== 'Runtime')
      ) {
        return false
      }
      const index = hirExpression(current.index)
      if (index._tag === 'Unavailable') return false
      selectors.push(
        Object.freeze({
          _tag: 'Index',
          index,
          array: current.array,
          bounds: current.bounds,
          type: current.type.type,
          span: current.syntax.span,
        }),
      )
      return true
    }
    return false
  }
  if (!walk(fact) || fact.type._tag !== 'Available') return undefined
  return Object.freeze({
    _tag: 'WritePlace',
    root: root.id,
    selectors: Object.freeze(selectors),
    type: fact.type.type,
    span: fact.syntax.span,
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
  const hirStatements = (
    facts: ReadonlyArray<StatementFact>,
    resultType?: SemanticType,
  ): ReadonlyArray<Hir.Statement> =>
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
            mutability: statement.binding.mutability,
            initializer: hirExpression(statement.binding.initializer),
            region: statement.region,
            span: statement.binding.syntax.span,
          })
        }
        if (statement._tag === 'IfStatement') {
          return Object.freeze({
            _tag: 'If' as const,
            condition: hirExpression(statement.condition),
            taken: hirStatements(statement.taken, resultType),
            otherwise: hirStatements(statement.otherwise, resultType),
            region: statement.region,
            span: statement.syntax.span,
          })
        }
        if (statement._tag === 'WriteStatement') {
          const place =
            statement.root === undefined
              ? undefined
              : hirWritePlace(statement.destination, statement.root)
          if (
            place === undefined ||
            statement.root?.mutability !== 'Mutable' ||
            !statement.compatible
          ) {
            return Object.freeze({
              _tag: 'UnavailableStatement' as const,
              region: statement.region,
              span: statement.syntax.span,
            })
          }
          return Object.freeze({
            _tag: 'Write' as const,
            place,
            value: hirExpectedExpression(statement.value, place.type, 'Assignment', place.span),
            region: statement.region,
            span: statement.syntax.span,
          })
        }
        if (statement._tag === 'WhileStatement') {
          return Object.freeze({
            _tag: 'While' as const,
            loop: statement.loop,
            ...(statement.parent === undefined ? {} : { parent: statement.parent }),
            condition: hirExpression(statement.condition),
            body: hirStatements(statement.body, resultType),
            region: statement.region,
            span: statement.syntax.span,
          })
        }
        if (statement._tag === 'BreakStatement' || statement._tag === 'ContinueStatement') {
          if (statement.target === undefined) {
            return Object.freeze({
              _tag: 'UnavailableStatement' as const,
              region: statement.region,
              span: statement.syntax.span,
            })
          }
          return Object.freeze({
            _tag: statement._tag === 'BreakStatement' ? ('Break' as const) : ('Continue' as const),
            target: statement.target,
            region: statement.region,
            span: statement.syntax.span,
          })
        }
        if (statement._tag === 'ReturnStatement')
          return Object.freeze({
            _tag: 'Return' as const,
            expression:
              resultType === undefined
                ? hirExpression(statement.expression)
                : hirExpectedExpression(
                    statement.expression,
                    resultType,
                    'Return',
                    statement.syntax.span,
                  ),
            region: statement.region,
            span: statement.expression.syntax.span,
          })
        throw new RangeError('Unknown statement fact')
      }),
    )
  const hir: Hir.Module = Object.freeze({
    _tag: 'HirModule',
    module: source.id,
    functions: Object.freeze(
      functions.map((fact) =>
        (() => {
          const entryRegion =
            fact.regionOrder.at(0) ??
            Object.freeze({
              _tag: 'HirRegion' as const,
              function: fact.declaration.id,
              ordinal: 0,
            })
          return Object.freeze({
            _tag: 'HirFunction' as const,
            declaration: fact.declaration,
            contract: Hir.contractOf(fact.declaration),
            entryRegion,
            regionOrder: fact.regionOrder,
            statements: hirStatements(
              fact.statements,
              fact.declaration.returnType._tag === 'Resolved'
                ? fact.declaration.returnType.type
                : undefined,
            ),
          })
        })(),
      ),
    ),
  })

  return Object.freeze({
    _tag: 'Elaboration',
    syntax,
    index,
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
