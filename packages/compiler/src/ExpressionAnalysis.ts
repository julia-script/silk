import * as Option from 'effect/Option'
import * as AggregateIdentity from './AggregateIdentity.js'
import * as CallableContract from './CallableContract.js'
import * as ConformanceProof from './ConformanceProof.js'
import * as Constraint from './Constraint.js'
import * as DeclarationCollection from './DeclarationCollection.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as DeclarationResolution from './DeclarationResolution.js'
import * as Diagnostic from './Diagnostic.js'
import type {
  ArgumentFact,
  ArgumentsResult,
  ReferencePathFact,
  ArrayElementFact,
  ArrayLiteralState,
  AnonymousCaptureFact,
  BindingDeclarationFact,
  BorrowRootFact,
  BoundsFact,
  BuiltinArgumentMappingFact,
  CallableApplyExpressionFact,
  CallableSectionExpressionFact,
  CallReferenceFact,
  ConstantExpressionFact,
  DeclarationFact,
  DeclarationId,
  DeclaredName,
  DurationExpressionFact,
  EffectCaptureFact,
  EffectRequirementBindingFact,
  ExpressionFact,
  ExpressionResult,
  ExpressionTypeFact,
  FieldProjectionExpressionFact,
  FloatingExpressionFact,
  FunctionFact,
  IdentifierExpressionFact,
  IdentifierResult,
  IntegerResult,
  InterfaceOperationFact,
  IntrinsicReferenceFact,
  MatchArmFact,
  MatchExpressionFact,
  MoveExpressionFact,
  ParameterFact,
  ParameterReferenceFact,
  PatternBindingFact,
  PatternFact,
  PatternFieldFact,
  PatternFieldState,
  ProjectionState,
  ReferentProjectionState,
  SemanticType,
  StatementFact,
  StaticIterationFact,
  StructInitializerFact,
  StructInitializerState,
  StructTargetFact,
  StructTypeArgumentFact,
  UnionVariantTargetFact,
} from './Elaboration.js'
import {
  argumentFact,
  assignmentRoot,
  assignmentRootAccess,
  availableBoolExpressionType,
  availableExpressionType,
  callCallee,
  callReferenceTokens,
  childNode,
  contextualIntegerCompatible,
  directToken,
  isAvailableSyntax,
  isExpressionNode,
  isRecursiveArgumentNode,
  lookupDeclaration,
  lookupParameter,
  pipelineCallable,
  pipelineInput,
  referencePath,
  representationJoinDiagnostic,
  spelling,
  typesCompatible,
  unavailableElement,
  unavailableExpressionType,
  unavailableSyntax,
  unionConversionDiagnostic,
} from './Elaboration.js'
import * as FloatingPoint from './FloatingPoint.js'
import * as Hir from './Hir.js'
import * as Intrinsic from './Intrinsic.js'
import * as DigitSeparator from './internal/DigitSeparator.js'
import * as DurationLiteral from './internal/DurationLiteral.js'
import * as IntegerLiteral from './internal/IntegerLiteral.js'
import * as TypeInference from './internal/TypeInference.js'
import * as LiteralForm from './LiteralForm.js'
import * as Match from './Match.js'
import * as NameResolution from './NameResolution.js'
import * as Operator from './Operator.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as Scalar from './Scalar.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import {
  analyzeFunctionBody,
  analyzeStatements,
  reachableCallableWrites,
  returnFlowOf,
  unsafeCallDiagnostic,
} from './StatementAnalysis.js'
import * as StaticEvaluation from './StaticEvaluation.js'
import * as StaticText from './StaticText.js'
import * as StaticValue from './StaticValue.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'

const integerLiteralSpan = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  token: Token.Token,
): SourceSpan.SourceSpan => {
  const minusToken = directToken(node, 'Minus')
  return minusToken === undefined
    ? token.span
    : Option.getOrElse(
        SourceSpan.make(source, minusToken.span.start, token.span.end),
        () => token.span,
      )
}

export const analyzeInteger = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  expected?: SemanticType,
): IntegerResult => {
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
  const literalSpan = integerLiteralSpan(source, node, token)
  const bytes = Option.getOrThrowWith(
    SourceFile.slice(source, token.span),
    () => new RangeError(`Semantic integer span does not belong to source ${source.id}`),
  )
  const selected =
    typeof expected === 'string' && Scalar.isIntegerSpelling(expected)
      ? Scalar.find(expected)
      : Scalar.defaultInteger
  if (selected === undefined || selected.category !== 'Integer')
    throw new RangeError('The scalar catalog lost its default integer')
  const magnitude = IntegerLiteral.magnitude(bytes)
  const value = negative ? -magnitude : magnitude
  // Target-width integers are retained against the widest admitted target here. The concrete
  // target validates its selected 32- or 64-bit range before MIR is committed.
  const range = Scalar.range(selected, 64)
  if (value >= range.minimum && value <= range.maximum) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Available',
        type: selected.spelling,
        value,
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
      type: selected.spelling,
      spelling: tokenSpelling,
      token,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      selected.spelling === 'usize' && negative
        ? Diagnostic.usizeNegative(tokenSpelling, literalSpan)
        : Diagnostic.integerOutOfRange(tokenSpelling, literalSpan),
    ]),
  })
}

export const analyzeFloating = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  expected?: SemanticType,
): {
  readonly fact: FloatingExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const unavailable = (
    diagnostics: ReadonlyArray<Diagnostic.Diagnostic>,
  ): { readonly fact: FloatingExpressionFact; readonly diagnostics: typeof diagnostics } =>
    Object.freeze({ fact: Object.freeze({ _tag: 'Unavailable', syntax: node }), diagnostics })
  const token = directToken(node, 'DecimalFloat')
  if (token === undefined) return unavailable(Object.freeze([]))
  const bytes = Option.getOrThrowWith(
    SourceFile.slice(source, token.span),
    () => new RangeError(`Semantic float span does not belong to source ${source.id}`),
  )
  const unsigned = DigitSeparator.strip(bytes)
  const spelling = directToken(node, 'Minus') === undefined ? unsigned : `-${unsigned}`
  const selected = Scalar.isFloatSpelling(expected) ? expected : Scalar.defaultFloat.spelling
  const encoded = FloatingPoint.fromDecimal(spelling, selected === 'f32' ? 32 : 64)
  // A spelling the lexer accepted but no float can represent must never pass silently.
  if (encoded === undefined) {
    return unavailable(Object.freeze([Diagnostic.invalidFloatLiteral(spelling, node.span)]))
  }
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Available',
      type: selected,
      bits: encoded.bits,
      spelling,
      token,
      syntax: node,
    }),
    diagnostics: Object.freeze([]),
  })
}

export const analyzeDuration = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): {
  readonly fact: DurationExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const token = directToken(node, 'DurationLiteral')
  if (token === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Duration',
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
    })
  }
  const bytes = Option.getOrThrowWith(
    SourceFile.slice(source, token.span),
    () => new RangeError(`Semantic duration span does not belong to source ${source.id}`),
  )
  const parsed = DurationLiteral.parse(bytes)
  if (parsed._tag === 'Invalid') {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Duration',
        token,
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
    })
  }
  const spelling = Array.from(bytes, (byte) => String.fromCharCode(byte)).join('')
  if (parsed.nanoseconds > 18_446_744_073_709_551_615n) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Duration',
        spelling,
        token,
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([Diagnostic.durationOutOfRange(spelling, token.span)]),
    })
  }
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Duration',
      value: parsed.nanoseconds,
      spelling,
      token,
      type: availableExpressionType('u64'),
      syntax: node,
    }),
    diagnostics: Object.freeze([]),
  })
}

export const analyzeConstant = (
  declaration: DeclarationFacts.ConstantFact,
  token: Token.Token,
  syntax: SyntaxTree.Node,
  reportDiagnostic: boolean,
): ExpressionResult => {
  const declared = declaration.declaredType
  const literal = declaration.literal
  let value: ConstantExpressionFact['value']
  let type: SemanticType | undefined
  let detail: string | undefined

  if (
    declared._tag !== 'Resolved' ||
    typeof declared.type !== 'string' ||
    !(
      declared.type === 'bool' ||
      declared.type === 'char' ||
      Type.isString(declared.type) ||
      Scalar.isIntegerSpelling(declared.type) ||
      Scalar.isFloatSpelling(declared.type)
    )
  ) {
    detail = 'the declared type must be one primitive scalar or string'
  } else if (literal._tag === 'Malformed') {
    detail = literal.detail
  } else if (literal._tag === 'Unavailable') {
    type = declared.type
  } else if (Type.isString(declared.type) && literal._tag === 'StringLiteral') {
    if (literal.data.kind !== 'Text') detail = 'a byte-string literal does not produce a string'
    else {
      type = Type.string
      value = Object.freeze({ _tag: 'String', data: literal.data })
    }
  } else if (declared.type === 'bool' && literal._tag === 'BooleanLiteral') {
    type = 'bool'
    value = Object.freeze({ _tag: 'Boolean', value: literal.value })
  } else if (declared.type === 'char' && literal._tag === 'CharacterLiteral') {
    type = 'char'
    value = Object.freeze({ _tag: 'Character', value: literal.value })
  } else if (Scalar.isIntegerSpelling(declared.type) && literal._tag === 'IntegerLiteral') {
    const scalar = Scalar.find(declared.type)
    if (scalar === undefined || scalar.category !== 'Integer') {
      detail = `unknown integer type ${declared.type}`
    } else {
      const range = Scalar.range(scalar, 64)
      if (literal.value < range.minimum || literal.value > range.maximum) {
        detail = `${literal.spelling} does not fit ${declared.type}`
      } else {
        type = declared.type
        value = Object.freeze({ _tag: 'Integer', value: literal.value, type })
      }
    }
  } else if (declared.type === 'u64' && literal._tag === 'DurationLiteral') {
    if (literal.value > 18_446_744_073_709_551_615n) {
      detail = `${literal.spelling} does not fit u64`
    } else {
      type = 'u64'
      value = Object.freeze({ _tag: 'Integer', value: literal.value, type })
    }
  } else if (Scalar.isFloatSpelling(declared.type) && literal._tag === 'FloatingLiteral') {
    const selected = declared.type
    const encoded = FloatingPoint.fromDecimal(literal.spelling, selected === 'f32' ? 32 : 64)
    if (encoded === undefined) detail = `${literal.spelling} is not a valid ${selected} literal`
    else {
      type = selected
      value = Object.freeze({
        _tag: 'Floating',
        bits: encoded.bits,
        spelling: literal.spelling,
        type: selected,
      })
    }
  } else {
    detail = `the literal kind does not match ${declared._tag === 'Resolved' ? Type.display(declared.type) : 'the declared type'}`
  }

  let diagnostic: Diagnostic.Diagnostic | undefined
  if (detail !== undefined) {
    diagnostic =
      literal._tag === 'DurationLiteral' && literal.value > 18_446_744_073_709_551_615n
        ? Diagnostic.durationOutOfRange(literal.spelling, literal.token.span)
        : Diagnostic.invalidConstant(detail, declaration.initializer.span)
  }
  const expressionType =
    type === undefined ? unavailableExpressionType : availableExpressionType(type)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Constant',
      declaration,
      token,
      ...(value === undefined ? {} : { value }),
      type: expressionType,
      syntax,
    }),
    diagnostics: Object.freeze(reportDiagnostic && diagnostic !== undefined ? [diagnostic] : []),
    type,
  })
}

/** The value names visible at one body position: parameters plus completed bindings. */
export interface Scope {
  readonly parameters: ReadonlyArray<ParameterFact>
  readonly bindings: ReadonlyArray<BindingDeclarationFact>
  readonly patternBindings: ReadonlyArray<PatternBindingFact>
}

export interface ValueResolution {
  readonly reference: ParameterReferenceFact
  readonly type: ExpressionTypeFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export const resolveValueName = (
  scope: Scope,
  tokenSpelling: string,
  token: Token.Token,
): ValueResolution => {
  const binding = scope.bindings.findLast(
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
  const patternBinding = scope.patternBindings.findLast(
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
  const missingDiagnostic = Diagnostic.unknownValueReference(tokenSpelling, token.span)
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

export const analyzeIdentifier = (
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

export const enumFactByType = (
  index: DeclarationIndex.Index,
  type: SemanticType,
): DeclarationFacts.EnumFact | undefined =>
  Type.isNominal(type)
    ? index.modules
        .find((module) => module.module === type.module)
        ?.enums.find(
          (declaration) =>
            declaration.canonical._tag === 'Canonical' &&
            declaration.canonical.id.name === type.name,
        )
    : undefined

export const unionFactByType = (
  index: DeclarationIndex.Index,
  type: SemanticType,
): DeclarationFacts.UnionFact | undefined => {
  if (!Type.isNominal(type)) return undefined
  const declaration = DeclarationFacts.byCanonical(index, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  return declaration?._tag === 'UnionDeclaration' ? declaration : undefined
}

const nominalUnionCoverage = (
  index: DeclarationIndex.Index,
  type: Type.Nominal,
  root: Type.Type,
): ReadonlyArray<Match.CoverageIdentity> => {
  const union = unionFactByType(index, type)
  return Object.freeze(
    union?.variants.flatMap((variant) =>
      variant.canonical._tag === 'Canonical'
        ? [Match.nominalUnionVariant(root, type, variant.canonical.id, variant.id.ordinal)]
        : [],
    ) ?? [],
  )
}

export const coverageMembersOf = (
  index: DeclarationIndex.Index,
  type: Type.Type,
): ReadonlyArray<Match.CoverageIdentity> => {
  if (Type.isUnion(type))
    return Object.freeze(
      type.members.flatMap((member) =>
        Type.isNominal(member) && unionFactByType(index, member) !== undefined
          ? nominalUnionCoverage(index, member, member)
          : [Match.structuralMember(member)],
      ),
    )
  if (Type.isNominal(type) && unionFactByType(index, type) !== undefined)
    return nominalUnionCoverage(index, type, type)
  return Match.membersOf(type)
}

export const analyzeEnumMember = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  resolution: ResolutionContext,
  expected?: SemanticType,
): ExpressionResult | undefined => {
  const path = SyntaxTree.directNode(node, 'TypePath')
  const pathIdentifiers =
    path === undefined ? [] : SyntaxTree.tokens(path).filter((token) => token.kind === 'Identifier')
  const identifiers =
    pathIdentifiers.length === 2
      ? pathIdentifiers
      : SyntaxTree.tokens(node).filter((token) => token.kind === 'Identifier')
  const qualifierToken = identifiers.at(0)
  const memberToken = identifiers.at(1)
  if (qualifierToken === undefined || memberToken === undefined || identifiers.length !== 2)
    return undefined
  const qualifier = spelling(source, qualifierToken)
  const enumLookup = NameResolution.lookup(resolution.scope, resolution.index, qualifier)
  if (enumLookup._tag !== 'Resolved' || enumLookup.declaration._tag !== 'EnumDeclaration')
    return undefined
  const enum_ = enumLookup.declaration
  const memberName = spelling(source, memberToken)
  const memberLookup = DeclarationFacts.lookupEnumMember(enum_.members, memberName)
  const member = memberLookup._tag === 'Resolved' ? memberLookup.member : undefined
  // An enum owns inherent members beside its members; `Status.describe` is a function item, not a
  // misspelled member, when an impl declares it.
  if (
    member === undefined &&
    NameResolution.lookupAssociated(resolution.index, enum_, memberName, resolution.scope.module)
      ._tag !== 'Missing'
  )
    return undefined
  const nominal =
    enum_.canonical._tag === 'Canonical'
      ? Type.nominal(enum_.canonical.id.module, enum_.canonical.id.name)
      : undefined
  const expectedEnum =
    expected === undefined ? undefined : enumFactByType(resolution.index, expected)
  const memberPathSpan = Option.getOrElse(
    SourceSpan.make(source, qualifierToken.span.start, memberToken.span.end),
    () => node.span,
  )
  let wrongEnum: Diagnostic.Diagnostic | undefined
  if (
    nominal !== undefined &&
    expected !== undefined &&
    expectedEnum?.canonical._tag === 'Canonical' &&
    !Type.equals(nominal, expected)
  ) {
    wrongEnum = Diagnostic.wrongEnumMember(
      Type.display(expected),
      Type.display(nominal),
      memberPathSpan,
    )
  } else if (
    nominal !== undefined &&
    typeof expected === 'string' &&
    Scalar.isIntegerSpelling(expected)
  ) {
    wrongEnum = Diagnostic.enumIntegerMismatch(
      Type.display(nominal),
      expected,
      'EnumToInteger',
      memberPathSpan,
    )
  } else {
    wrongEnum = undefined
  }
  const unknown =
    member === undefined
      ? Diagnostic.unknownEnumMember(qualifier, memberName, memberToken.span)
      : undefined
  const invalidMember =
    member !== undefined &&
    (member.canonical._tag !== 'Canonical' || member.discriminant._tag !== 'Available')
  const diagnostic = unknown ?? wrongEnum
  const type =
    nominal !== undefined && member !== undefined && !invalidMember && diagnostic === undefined
      ? availableExpressionType(nominal)
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'EnumMember',
      enum: enum_,
      ...(member === undefined ? {} : { member }),
      ...(diagnostic === undefined ? {} : { cause: Diagnostic.identity(diagnostic) }),
      qualifierToken,
      memberToken,
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostic === undefined ? [] : [diagnostic]),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

const analyzeEnumValueCall = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  argumentsResult: ArgumentsResult,
  resolution: ResolutionContext,
): ExpressionResult | undefined => {
  const identifiers = callReferenceTokens(node)
  const qualifierToken = identifiers.at(0)
  const operationToken = identifiers.at(1)
  if (qualifierToken === undefined || operationToken === undefined || identifiers.length !== 2)
    return undefined
  const qualifier = spelling(source, qualifierToken)
  const operationName = spelling(source, operationToken)
  const qualifierLookup = NameResolution.lookup(resolution.scope, resolution.index, qualifier)
  if (qualifierLookup._tag !== 'Resolved' || qualifierLookup.declaration._tag !== 'EnumDeclaration')
    return undefined
  const operation = qualifierLookup.declaration.associatedOperations.find(
    (candidate) => candidate.name === operationName,
  )
  if (operation === undefined) return undefined
  const argument = argumentsResult.facts.at(0)
  const actual = argument?.type._tag === 'Available' ? argument.type.type : undefined
  let mismatch: Diagnostic.Diagnostic | undefined
  if (actual === undefined || Type.equals(actual, operation.parameter)) {
    mismatch = undefined
  } else if (enumFactByType(resolution.index, actual) !== undefined) {
    mismatch = Diagnostic.wrongEnumMember(
      Type.display(operation.parameter),
      Type.display(actual),
      argument?.syntax.span ?? node.span,
    )
  } else if (typeof actual === 'string' && Scalar.isIntegerSpelling(actual)) {
    mismatch = Diagnostic.enumIntegerMismatch(
      Type.display(operation.parameter),
      actual,
      'IntegerToEnum',
      argument?.syntax.span ?? node.span,
    )
  } else {
    mismatch = Diagnostic.argumentTypeMismatch(
      Type.display(operation.parameter),
      Type.display(actual),
      argument?.syntax.span ?? node.span,
    )
  }
  const arity =
    argumentsResult.facts.length === 1
      ? undefined
      : Diagnostic.wrongCallArity(
          Object.freeze({
            _tag: 'BuiltinTarget',
            actor: qualifier,
            operation: 'value',
          }),
          1,
          argumentsResult.facts.length,
          node.span,
        )
  const valid = argument !== undefined && mismatch === undefined && arity === undefined
  const type = valid
    ? availableExpressionType(operation.result.spelling)
    : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'EnumValue',
      operation,
      argument: argument?.expression ?? unavailableExpression(node),
      qualifierToken,
      operationToken,
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      ...argumentsResult.diagnostics,
      ...(mismatch === undefined ? [] : [mismatch]),
      ...(arity === undefined ? [] : [arity]),
    ]),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

export const analyzeConstantReference = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  resolution: ResolutionContext,
): ExpressionResult | undefined => {
  const identifiers = SyntaxTree.tokens(node).filter((token) => token.kind === 'Identifier')
  const first = identifiers.at(0)
  const second = identifiers.at(1)
  if (first === undefined || identifiers.length > 2) return undefined
  const lookup =
    second === undefined
      ? NameResolution.lookup(resolution.scope, resolution.index, spelling(source, first))
      : NameResolution.lookupQualified(
          resolution.scope,
          resolution.index,
          spelling(source, first),
          spelling(source, second),
          second,
        )
  if (lookup._tag !== 'Resolved' || lookup.declaration._tag !== 'ConstantDeclaration')
    return undefined
  const result = analyzeConstant(lookup.declaration, second ?? first, node, false)
  if (
    lookup.declaration.literal._tag !== 'Unavailable' ||
    resolution.staticContext?.constant === undefined
  )
    return result
  const selected = resolution.staticContext.constant(
    lookup.declaration,
    node.span,
    resolution.staticContext.trace,
  )
  if (selected._tag === 'Failed') return result
  const value: ConstantExpressionFact['value'] = (() => {
    const candidate = selected.value
    if (candidate._tag === 'BooleanValue')
      return Object.freeze({ _tag: 'Boolean' as const, value: candidate.value })
    if (candidate._tag === 'CharacterValue')
      return Object.freeze({ _tag: 'Character' as const, value: candidate.value })
    if (candidate._tag === 'IntegerValue')
      return Object.freeze({
        _tag: 'Integer' as const,
        value: candidate.value,
        type: candidate.type,
      })
    if (candidate._tag === 'FloatValue')
      return Object.freeze({
        _tag: 'Floating' as const,
        bits: candidate.bits,
        spelling: StaticValue.encode(candidate),
        type: candidate.type,
      })
    if (candidate._tag === 'TextValue') {
      const bytes = Object.freeze([...candidate.bytes])
      return Object.freeze({
        _tag: 'String' as const,
        data: Object.freeze({
          _tag: 'StaticData' as const,
          id: `text:${bytes.map((byte) => byte.toString(16).padStart(2, '0')).join('')}`,
          kind: 'Text' as const,
          bytes,
          utf8: true,
        }),
      })
    }
    return undefined
  })()
  return value === undefined || result.fact._tag !== 'Constant'
    ? result
    : Object.freeze({
        ...result,
        fact: Object.freeze({ ...result.fact, value }),
      })
}

export interface MoveResult {
  readonly fact: MoveExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly type: SemanticType | undefined
}

export const analyzeMove = (
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
  const invalidMove =
    subject.fact._tag === 'Constant'
      ? Diagnostic.invalidConstant('constants are immediate values and cannot be moved', node.span)
      : undefined
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Move',
      subject: subject.fact,
      type: subject.fact.type,
      syntax: node,
    }),
    diagnostics: Object.freeze(
      invalidMove === undefined ? subject.diagnostics : [...subject.diagnostics, invalidMove],
    ),
    type: invalidMove === undefined ? subject.type : undefined,
  })
}

const borrowsConstant = (subject: ExpressionFact): boolean =>
  subject._tag === 'Grouped' ? borrowsConstant(subject.expression) : subject._tag === 'Constant'

export const borrowRoot = (subject: ExpressionFact): BorrowRootFact | undefined => {
  if (subject._tag === 'Grouped') return borrowRoot(subject.expression)
  if (subject._tag === 'ReferentProjection' && subject.state._tag === 'Resolved') {
    return borrowRoot(subject.subject)
  }
  if (subject._tag === 'FieldProjection' && subject.state._tag === 'Resolved') {
    const root = borrowRoot(subject.subject)
    return root === undefined
      ? undefined
      : Object.freeze({
          ...root,
          path: Object.freeze([
            ...root.path,
            Object.freeze({
              _tag: 'Field' as const,
              field: subject.state.field.id,
              span: subject.syntax.span,
            }),
          ]),
        })
  }
  if (
    subject._tag === 'IndexProjection' &&
    subject.array !== undefined &&
    (subject.bounds._tag === 'Proven' || subject.bounds._tag === 'Runtime')
  ) {
    const root = borrowRoot(subject.subject)
    return root === undefined
      ? undefined
      : Object.freeze({
          ...root,
          path: Object.freeze([
            ...root.path,
            Object.freeze({
              _tag: 'Index' as const,
              index: subject.index,
              array: subject.array,
              bounds: subject.bounds,
              span: subject.syntax.span,
            }),
          ]),
        })
  }
  if (
    subject._tag === 'IndexProjection' &&
    subject.slice !== undefined &&
    subject.bounds._tag === 'RuntimeSlice'
  ) {
    const root = borrowRoot(subject.subject)
    return root === undefined
      ? undefined
      : Object.freeze({
          ...root,
          path: Object.freeze([
            ...root.path,
            Object.freeze({
              _tag: 'SliceIndex' as const,
              index: subject.index,
              slice: subject.slice,
              span: subject.syntax.span,
            }),
          ]),
        })
  }
  if (subject._tag !== 'Identifier') return undefined
  if (subject.reference._tag === 'ResolvedBinding') {
    return Object.freeze({
      _tag: 'BindingRoot',
      binding: subject.reference.binding,
      path: Object.freeze([]),
    })
  }
  if (subject.reference._tag === 'Resolved') {
    return Object.freeze({
      _tag: 'ParameterRoot',
      parameter: subject.reference.parameter,
      path: Object.freeze([]),
    })
  }
  if (subject.reference._tag === 'ResolvedPattern') {
    return Object.freeze({
      _tag: 'PatternRoot',
      binding: subject.reference.binding,
      path: Object.freeze([]),
    })
  }
  return undefined
}

export const exclusiveBorrowRoot = (root: BorrowRootFact): boolean =>
  root._tag === 'TemporaryRoot' ||
  (root._tag === 'BindingRoot' && root.binding.mutability === 'Mutable') ||
  (root._tag === 'PatternRoot' && root.binding.access === 'Exclusive') ||
  (root._tag === 'ParameterRoot' &&
    ((root.parameter.bindingMutability === 'Mutable' &&
      root.parameter.declaredType._tag === 'Resolved' &&
      !Type.isReference(root.parameter.declaredType.type) &&
      !Type.isSlice(root.parameter.declaredType.type)) ||
      (root.path.length > 0 &&
        root.parameter.declaredType._tag === 'Resolved' &&
        Type.isReference(root.parameter.declaredType.type) &&
        root.parameter.declaredType.type.access === 'Exclusive')))

export const unavailableBorrow = (
  node: SyntaxTree.Node,
  access: Type.BorrowAccess,
  subject: ExpressionFact,
  diagnostics: ReadonlyArray<Diagnostic.Diagnostic>,
  cause?: Diagnostic.Diagnostic,
): ExpressionResult =>
  Object.freeze({
    fact: Object.freeze({
      _tag: 'Borrow',
      access,
      subject,
      formation: Object.freeze({
        _tag: 'Unavailable',
        ...(cause === undefined ? {} : { cause: Diagnostic.identity(cause) }),
      }),
      type: unavailableExpressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([...diagnostics, ...(cause === undefined ? [] : [cause])]),
    type: undefined,
  })

export const analyzeBorrow = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected: SemanticType | undefined,
  borrowAllowed: boolean,
): ExpressionResult => {
  const access: Type.BorrowAccess =
    directToken(node, 'MutKeyword') === undefined ? 'Shared' : 'Exclusive'
  const subjectNode = node.children.find(isExpressionNode)
  const subjectResult =
    subjectNode === undefined
      ? undefined
      : analyzeExpression(source, subjectNode, declarations, declaration, scope, resolution)
  const subject = subjectResult?.fact ?? unavailableExpression(node)
  const diagnostics = subjectResult?.diagnostics ?? Object.freeze([])
  if (!borrowAllowed) {
    return unavailableBorrow(
      node,
      access,
      subject,
      diagnostics,
      Diagnostic.invalidBorrowPosition(node.span),
    )
  }
  return borrowSubject(node, subjectNode, subjectResult, access, expected, declaration)
}

/**
 * Forms one borrow of an already analyzed subject: the written `&`/`&mut` form after its
 * position check, and a receiver synthesized for a method call whose parameter zero is a reference.
 */
export const borrowSubject = (
  node: SyntaxTree.Node,
  subjectNode: SyntaxTree.Node | undefined,
  subjectResult: ExpressionResult | undefined,
  access: Type.BorrowAccess,
  expected: SemanticType | undefined,
  declaration: DeclarationFact,
): ExpressionResult => {
  const subject = subjectResult?.fact ?? unavailableExpression(node)
  const diagnostics = subjectResult?.diagnostics ?? Object.freeze([])
  if (borrowsConstant(subject))
    return unavailableBorrow(
      node,
      access,
      subject,
      diagnostics,
      Diagnostic.invalidConstant(
        'constants are immediate values and cannot be borrowed',
        node.span,
      ),
    )
  const sourceType = subjectResult?.type
  const root =
    borrowRoot(subject) ??
    (sourceType === undefined
      ? undefined
      : Object.freeze({
          _tag: 'TemporaryRoot' as const,
          owner: Object.freeze({
            _tag: 'TemporaryOwnerId' as const,
            function: declaration.id,
            span: subject.syntax.span,
            ordinal: 0,
          }),
          value: subject,
          path: Object.freeze([]),
        }))
  if (root === undefined || sourceType === undefined) {
    return unavailableBorrow(
      node,
      access,
      subject,
      diagnostics,
      Diagnostic.invalidBorrowOperand(subjectNode?.span ?? node.span),
    )
  }
  const projectedReference =
    subject._tag === 'ReferentProjection' && subject.state._tag === 'Resolved'
      ? subject.state.reference
      : undefined
  const parentReference = Type.isReference(sourceType) ? sourceType : projectedReference
  if (
    parentReference !== undefined &&
    (projectedReference !== undefined || (expected !== undefined && Type.isReference(expected)))
  ) {
    const target = parentReference.target
    if (
      (expected !== undefined &&
        (!Type.isReference(expected) ||
          !TypeInference.infer(expected.target, target, new Map()) ||
          expected.access !== access)) ||
      (parentReference.access === 'Shared' && access === 'Exclusive')
    ) {
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.invalidBorrowOperand(subjectNode?.span ?? node.span),
      )
    }
    const type = Type.reference(access, target)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Borrow',
        access,
        subject,
        formation: Object.freeze({
          _tag: 'ValueReborrow',
          root,
          parent: parentReference,
          suspendsParent: parentReference.access === 'Exclusive',
        }),
        type: availableExpressionType(type),
        syntax: node,
      }),
      diagnostics,
      type,
    })
  }
  if (
    (expected === undefined || (!Type.isSlice(expected) && !Type.isReference(expected))) &&
    !Type.isFixedArray(sourceType) &&
    !Type.isSlice(sourceType)
  ) {
    if (access === 'Exclusive' && !exclusiveBorrowRoot(root)) {
      const name =
        subject._tag === 'Identifier' && 'spelling' in subject.reference
          ? subject.reference.spelling
          : '?'
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.exclusiveBorrowRequiresMutable(name, subjectNode?.span ?? node.span),
      )
    }
    const type = Type.reference(access, sourceType)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Borrow',
        access,
        subject,
        formation: Object.freeze({ _tag: 'ValueBorrow', root, source: sourceType }),
        type: availableExpressionType(type),
        syntax: node,
      }),
      diagnostics,
      type,
    })
  }
  if (expected !== undefined && Type.isReference(expected)) {
    if (!TypeInference.infer(expected.target, sourceType, new Map())) {
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.invalidBorrowOperand(subjectNode?.span ?? node.span),
      )
    }
    if (access === 'Exclusive' && !exclusiveBorrowRoot(root)) {
      const name =
        subject._tag === 'Identifier' && 'spelling' in subject.reference
          ? subject.reference.spelling
          : '?'
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.exclusiveBorrowRequiresMutable(name, subjectNode?.span ?? node.span),
      )
    }
    if (access !== expected.access) {
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.invalidBorrowOperand(subjectNode?.span ?? node.span),
      )
    }
    const type = Type.reference(access, sourceType)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Borrow',
        access,
        subject,
        formation: Object.freeze({ _tag: 'ValueBorrow', root, source: sourceType }),
        type: availableExpressionType(type),
        syntax: node,
      }),
      diagnostics,
      type,
    })
  }
  if (Type.isFixedArray(sourceType)) {
    if (access === 'Exclusive' && !exclusiveBorrowRoot(root)) {
      const name =
        subject._tag === 'Identifier' && 'spelling' in subject.reference
          ? subject.reference.spelling
          : '?'
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.exclusiveBorrowRequiresMutable(name, subjectNode?.span ?? node.span),
      )
    }
    const type = Type.slice(access, sourceType.element)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Borrow',
        access,
        subject,
        formation: Object.freeze({ _tag: 'FixedArrayBorrow', root, array: sourceType }),
        type: availableExpressionType(type),
        syntax: node,
      }),
      diagnostics,
      type,
    })
  }
  if (Type.isSlice(sourceType)) {
    if (sourceType.access === 'Shared' && access === 'Exclusive') {
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.invalidSliceReborrow(sourceType.access, access, node.span),
      )
    }
    const type = Type.slice(access, sourceType.element)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Borrow',
        access,
        subject,
        formation: Object.freeze({
          _tag: 'SliceReborrow',
          root,
          parent: sourceType,
          suspendsParent: sourceType.access === 'Exclusive',
        }),
        type: availableExpressionType(type),
        syntax: node,
      }),
      diagnostics,
      type,
    })
  }
  return unavailableBorrow(
    node,
    access,
    subject,
    diagnostics,
    Diagnostic.invalidBorrowOperand(subjectNode?.span ?? node.span),
  )
}

export interface StructTargetResult {
  readonly fact: StructTargetFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export interface UnionVariantTargetResult {
  readonly fact: UnionVariantTargetFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export const intrinsicStruct = (
  type: Type.Nominal,
  syntax: SyntaxTree.Node,
  token: Token.Token,
): DeclarationFacts.StructFact => {
  const ordinal = Type.intrinsicNominalOrdinal(type)
  const id: DeclarationFacts.DeclarationId = Object.freeze({
    _tag: 'DeclarationId',
    sourceId: type.module,
    ordinal,
  })
  let fieldTypes: ReadonlyArray<readonly [string, Type.Type]>
  if (Type.equals(type, Type.layout)) {
    fieldTypes = Object.freeze([
      Object.freeze(['bytes', 'usize'] as const),
      Object.freeze(['alignment', 'usize'] as const),
    ])
  } else if (Type.equals(type, Type.invalidAlignment)) {
    fieldTypes = Object.freeze([Object.freeze(['alignment', 'usize'] as const)])
  } else {
    fieldTypes = Object.freeze([])
  }
  return Object.freeze({
    _tag: 'StructDeclaration',
    id,
    canonical: Object.freeze({
      _tag: 'Canonical',
      id: Object.freeze({
        _tag: 'CanonicalDeclarationId',
        module: type.module,
        name: type.name,
      }),
    }),
    visibility:
      Type.equals(type, Type.layout) || Type.equals(type, Type.invalidAlignment)
        ? 'Public'
        : 'Private',
    typeParameters: Object.freeze([]),
    name: Object.freeze({ _tag: 'Present', spelling: type.name, token }),
    aggregateKind: 'Named',
    fields: Object.freeze(
      fieldTypes.map(([name, fieldType], fieldOrdinal) =>
        Object.freeze({
          _tag: 'AggregateField' as const,
          id: Object.freeze({
            _tag: 'FieldId' as const,
            owner: Object.freeze({ _tag: 'StructFieldOwnerId' as const, declaration: id }),
            ordinal: fieldOrdinal,
          }),
          member: AggregateIdentity.labeled(name),
          state: Object.freeze({
            _tag: 'Unique' as const,
            id: Object.freeze({
              _tag: 'FieldId' as const,
              owner: Object.freeze({ _tag: 'StructFieldOwnerId' as const, declaration: id }),
              ordinal: fieldOrdinal,
            }),
          }),
          visibility: 'Public' as const,
          name: Object.freeze({ _tag: 'Present' as const, spelling: name, token }),
          declaredType: Object.freeze({
            _tag: 'Resolved' as const,
            type: fieldType,
            spelling: Type.encode(fieldType),
            token,
            syntax,
          }),
          syntax,
        }),
      ),
    ),
    dependency: Object.freeze({ _tag: 'Available', types: Object.freeze([]) }),
    syntax,
  })
}

export const resolveStructTarget = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  resolution: ResolutionContext,
  caller?: DeclarationFact,
  inferConstructionArguments = false,
): StructTargetResult => {
  const environment = new Map(
    (caller?.typeParameters ?? []).flatMap((parameter) =>
      parameter.name._tag === 'Present' ? [[parameter.name.spelling, parameter.type] as const] : [],
    ),
  )
  const analyzed = DeclarationCollection.analyzeDeclaredType(source, syntax, environment)
  const nameResolution: NameResolution.Resolution = Object.freeze({
    _tag: 'NameResolution',
    modules: Object.freeze([resolution.scope]),
    diagnostics: Object.freeze([]),
  })
  if (inferConstructionArguments) {
    const applied = analyzed.fact._tag === 'Applied' ? analyzed.fact : undefined
    const targetFact = applied?.target ?? analyzed.fact
    const path = targetFact._tag === 'Unresolved' ? targetFact.path : undefined
    let base: Type.Nominal | undefined
    if (path === undefined) {
      if (targetFact._tag === 'Resolved' && Type.isNominal(targetFact.type)) base = targetFact.type
    } else {
      const candidate = NameResolution.resolveType(
        nameResolution,
        resolution.index,
        source.id,
        path,
      ).fact
      if (candidate._tag === 'Resolved' && Type.isNominal(candidate.type)) base = candidate.type
    }
    const candidate =
      base === undefined
        ? undefined
        : DeclarationFacts.byCanonical(resolution.index, {
            _tag: 'CanonicalDeclarationId',
            module: base.module,
            name: base.name,
          })
    // A base that already carries arguments came through an alias; its arguments are final, so
    // the ordinary resolution below keeps them instead of re-inferring from the literal.
    if (
      base !== undefined &&
      base.arguments.length === 0 &&
      candidate?._tag === 'StructDeclaration'
    ) {
      const supplied = applied?.arguments ?? []
      const sourceParameters = candidate.typeParameters.filter(
        (parameter) =>
          parameter.type.kind !== 'CallableRepresentation' &&
          parameter.type.kind !== 'EffectRepresentation',
      )
      if (supplied.length <= sourceParameters.length) {
        const resolvedArguments = supplied.map((argument) =>
          DeclarationResolution.resolveTypeFact(
            resolution.index,
            source.id,
            argument,
            (module, argumentPath) =>
              NameResolution.resolveType(nameResolution, resolution.index, module, argumentPath),
          ),
        )
        let suppliedOrdinal = 0
        const arguments_ = candidate.typeParameters.flatMap(
          (parameter): ReadonlyArray<Type.GenericArgument> => {
            if (
              parameter.type.kind === 'CallableRepresentation' ||
              parameter.type.kind === 'EffectRepresentation'
            )
              return [Type.representationParameterArgument(parameter.type)]
            const resolved = resolvedArguments.at(suppliedOrdinal)
            suppliedOrdinal += 1
            if (resolved === undefined) return [Type.parameterArgument(parameter.type)]
            if (resolved?.fact._tag !== 'Resolved') return []
            if (parameter.type.kind === 'Value')
              return Type.isTypeArgument(resolved.fact.type) ? [resolved.fact.type] : []
            if (
              parameter.type.kind === 'RequirementRow' &&
              Type.isParameter(resolved.fact.type) &&
              resolved.fact.type.kind === 'RequirementRow'
            )
              return [Type.requirementRowArgument([], [resolved.fact.type])]
            return []
          },
        )
        if (arguments_.length === candidate.typeParameters.length) {
          const parameters = candidate.typeParameters.map((parameter) => parameter.type)
          if (TypeInference.prefixSubstitution(parameters, arguments_) !== undefined) {
            const token = SyntaxTree.tokens(syntax).find(
              (candidateToken) => candidateToken.kind === 'Identifier',
            )
            if (token !== undefined)
              return Object.freeze({
                fact: Object.freeze({
                  _tag: 'Resolved',
                  struct: candidate,
                  type: Type.specializeNominal(base, arguments_),
                  token,
                }),
                diagnostics: Diagnostic.merge(
                  analyzed.diagnostics,
                  ...resolvedArguments.map((argument) => argument.diagnostics),
                ),
              })
          }
        }
      }
    }
  }
  const resolved = DeclarationResolution.resolveTypeFact(
    resolution.index,
    source.id,
    analyzed.fact,
    (module, path) => NameResolution.resolveType(nameResolution, resolution.index, module, path),
  )
  if (resolved.fact._tag === 'Resolved' && Type.isNominal(resolved.fact.type)) {
    if (Type.isIntrinsicNominal(resolved.fact.type) && !Type.isSharedCore(resolved.fact.type)) {
      const token = SyntaxTree.tokens(syntax).find((candidate) => candidate.kind === 'Identifier')
      if (token !== undefined)
        return Object.freeze({
          fact: Object.freeze({
            _tag: 'Resolved',
            struct: intrinsicStruct(resolved.fact.type, syntax, token),
            type: resolved.fact.type,
            token,
          }),
          diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics),
        })
    }
    const declaration = DeclarationFacts.byCanonical(resolution.index, {
      _tag: 'CanonicalDeclarationId',
      module: resolved.fact.type.module,
      name: resolved.fact.type.name,
    })
    if (declaration?._tag === 'StructDeclaration') {
      const token = SyntaxTree.tokens(syntax).find((candidate) => candidate.kind === 'Identifier')
      if (token === undefined)
        return Object.freeze({
          fact: Object.freeze({ _tag: 'Unavailable' }),
          diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics),
        })
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          struct: declaration,
          type: resolved.fact.type,
          token,
        }),
        diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics),
      })
    }
  }
  const token = SyntaxTree.tokens(syntax).find((candidate) => candidate.kind === 'Identifier')
  const diagnostic = Diagnostic.expectedType(
    resolved.fact._tag === 'Resolved' ? Type.display(resolved.fact.type) : 'unavailable struct',
    token?.span ?? syntax.span,
  )
  return Object.freeze({
    fact: Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) }),
    diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics, [diagnostic]),
  })
}

const unavailableUnionVariantTarget = (
  diagnostic: Diagnostic.Diagnostic,
  diagnostics: ReadonlyArray<Diagnostic.Diagnostic> = [],
): UnionVariantTargetResult =>
  Object.freeze({
    fact: Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) }),
    diagnostics: Diagnostic.merge(diagnostics, [diagnostic]),
  })

const selectedUnionVariant = (
  union: DeclarationFacts.UnionFact,
  type: Type.Nominal,
  variantName: string,
  token: Token.Token,
  diagnostics: ReadonlyArray<Diagnostic.Diagnostic>,
): UnionVariantTargetResult => {
  const variant = DeclarationFacts.lookupUnionVariant(union.variants, variantName)
  if (variant._tag !== 'Resolved')
    return unavailableUnionVariantTarget(
      Diagnostic.unknownUnionVariant(Type.display(type), variantName, token.span),
      diagnostics,
    )
  if (union.validity._tag !== 'Valid')
    return unavailableUnionVariantTarget(
      Diagnostic.invalidNominalUnionConstruction(Type.display(type), token.span),
      diagnostics,
    )
  return Object.freeze({
    fact: Object.freeze({ _tag: 'Resolved', union, variant: variant.variant, type, token }),
    diagnostics: Object.freeze(diagnostics),
  })
}

export const resolveUnionVariantTarget = (
  source: SourceFile.SourceFile,
  selector: SyntaxTree.Node,
  resolution: ResolutionContext,
  caller?: DeclarationFact,
): UnionVariantTargetResult => {
  const parentSyntax =
    SyntaxTree.directNode(selector, 'AppliedType') ?? childNode(selector, 'TypePath')
  const variantToken = SyntaxTree.tokens(selector)
    .filter((token) => token.kind === 'Identifier')
    .at(-1)
  if (parentSyntax === undefined || variantToken === undefined) {
    const diagnostic = Diagnostic.expectedNominalUnion('unavailable selector', selector.span)
    return unavailableUnionVariantTarget(diagnostic)
  }
  const environment = new Map(
    (caller?.typeParameters ?? []).flatMap((parameter) =>
      parameter.name._tag === 'Present' ? [[parameter.name.spelling, parameter.type] as const] : [],
    ),
  )
  const analyzed = DeclarationCollection.analyzeDeclaredType(source, parentSyntax, environment)
  const nameResolution: NameResolution.Resolution = Object.freeze({
    _tag: 'NameResolution',
    modules: Object.freeze([resolution.scope]),
    diagnostics: Object.freeze([]),
  })
  const applied = analyzed.fact._tag === 'Applied' ? analyzed.fact : undefined
  const targetFact = applied?.target ?? analyzed.fact
  const path = targetFact._tag === 'Unresolved' ? targetFact.path : undefined
  let base: Type.Nominal | undefined
  if (path === undefined) {
    if (targetFact._tag === 'Resolved' && Type.isNominal(targetFact.type)) base = targetFact.type
  } else {
    const candidate = NameResolution.resolveType(
      nameResolution,
      resolution.index,
      source.id,
      path,
    ).fact
    if (candidate._tag === 'Resolved' && Type.isNominal(candidate.type)) base = candidate.type
  }
  const declaration =
    base === undefined
      ? undefined
      : DeclarationFacts.byCanonical(resolution.index, {
          _tag: 'CanonicalDeclarationId',
          module: base.module,
          name: base.name,
        })
  if (base === undefined || declaration?._tag !== 'UnionDeclaration') {
    const diagnostic = Diagnostic.expectedNominalUnion(
      base === undefined ? 'unavailable type' : Type.display(base),
      parentSyntax.span,
    )
    return unavailableUnionVariantTarget(diagnostic, analyzed.diagnostics)
  }
  const fullyResolved = DeclarationResolution.resolveTypeFact(
    resolution.index,
    source.id,
    analyzed.fact,
    (module, argumentPath) =>
      NameResolution.resolveType(nameResolution, resolution.index, module, argumentPath),
  )
  if (
    fullyResolved.fact._tag === 'Resolved' &&
    Type.isNominal(fullyResolved.fact.type) &&
    fullyResolved.fact.type.module === base.module &&
    fullyResolved.fact.type.name === base.name
  )
    return selectedUnionVariant(
      declaration,
      fullyResolved.fact.type,
      spelling(source, variantToken),
      variantToken,
      Diagnostic.merge(analyzed.diagnostics, fullyResolved.diagnostics),
    )
  const supplied = applied?.arguments ?? []
  const sourceParameters = declaration.typeParameters.filter(
    (parameter) =>
      parameter.type.kind !== 'CallableRepresentation' &&
      parameter.type.kind !== 'EffectRepresentation',
  )
  if (supplied.length > sourceParameters.length) {
    const diagnostic = Diagnostic.expectedNominalUnion(Type.display(base), parentSyntax.span)
    return unavailableUnionVariantTarget(diagnostic, analyzed.diagnostics)
  }
  const resolvedArguments = supplied.map((argument) =>
    DeclarationResolution.resolveTypeFact(
      resolution.index,
      source.id,
      argument,
      (module, argumentPath) =>
        NameResolution.resolveType(nameResolution, resolution.index, module, argumentPath),
    ),
  )
  let suppliedOrdinal = 0
  const arguments_ = declaration.typeParameters.flatMap(
    (parameter): ReadonlyArray<Type.GenericArgument> => {
      if (
        parameter.type.kind === 'CallableRepresentation' ||
        parameter.type.kind === 'EffectRepresentation'
      )
        return [Type.representationParameterArgument(parameter.type)]
      const resolved = resolvedArguments.at(suppliedOrdinal)
      suppliedOrdinal += 1
      if (resolved === undefined) return [Type.parameterArgument(parameter.type)]
      if (resolved.fact._tag !== 'Resolved') return []
      if (parameter.type.kind === 'Value')
        return Type.isTypeArgument(resolved.fact.type) ? [resolved.fact.type] : []
      if (
        parameter.type.kind === 'RequirementRow' &&
        Type.isParameter(resolved.fact.type) &&
        resolved.fact.type.kind === 'RequirementRow'
      )
        return [Type.requirementRowArgument([], [resolved.fact.type])]
      return []
    },
  )
  if (
    arguments_.length !== declaration.typeParameters.length ||
    TypeInference.prefixSubstitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      arguments_,
    ) === undefined
  ) {
    const diagnostic = Diagnostic.expectedNominalUnion(Type.display(base), parentSyntax.span)
    return unavailableUnionVariantTarget(
      diagnostic,
      Diagnostic.merge(
        analyzed.diagnostics,
        ...resolvedArguments.map((argument) => argument.diagnostics),
      ),
    )
  }
  return selectedUnionVariant(
    declaration,
    Type.specializeNominal(base, arguments_),
    spelling(source, variantToken),
    variantToken,
    Diagnostic.merge(
      analyzed.diagnostics,
      ...resolvedArguments.map((argument) => argument.diagnostics),
    ),
  )
}

const resolveBareUnionVariantTarget = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  resolution: ResolutionContext,
): UnionVariantTargetResult | undefined => {
  const tokens = SyntaxTree.tokens(node)
  const initializerStart = tokens.findIndex((token) => token.kind === 'LeftBrace')
  const selectorTokens = initializerStart === -1 ? tokens : tokens.slice(0, initializerStart)
  const identifiers = selectorTokens.filter((token) => token.kind === 'Identifier')
  const parentToken = identifiers.at(0)
  const variantToken = identifiers.at(1)
  if (identifiers.length !== 2 || parentToken === undefined || variantToken === undefined)
    return undefined
  const lookup = NameResolution.lookup(
    resolution.scope,
    resolution.index,
    spelling(source, parentToken),
  )
  if (lookup._tag !== 'Resolved' || lookup.declaration._tag !== 'UnionDeclaration') return undefined
  const union = lookup.declaration
  // `Option.some` names an inherent member, not a misspelled variant.
  const variantName = spelling(source, variantToken)
  if (
    !union.variants.some(
      (variant) => variant.name._tag === 'Present' && variant.name.spelling === variantName,
    ) &&
    NameResolution.lookupAssociated(resolution.index, union, variantName, resolution.scope.module)
      ._tag !== 'Missing'
  )
    return undefined
  const type =
    union.canonical._tag === 'Canonical'
      ? Type.nominal(
          union.canonical.id.module,
          union.canonical.id.name,
          union.typeParameters.map((parameter) => Type.parameterArgument(parameter.type)),
        )
      : undefined
  if (type === undefined) {
    const diagnostic = Diagnostic.invalidNominalUnionConstruction(
      spelling(source, parentToken),
      parentToken.span,
    )
    return unavailableUnionVariantTarget(diagnostic)
  }
  return selectedUnionVariant(union, type, spelling(source, variantToken), variantToken, [])
}

export interface PatternCounters {
  pattern: number
  binding: number
  invalid: boolean
}

export interface PatternResult {
  readonly fact: PatternFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export interface PatternTypeResult {
  readonly type?: Type.Type
  readonly declared: DeclarationFacts.DeclaredTypeFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export const resolvePatternType = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  resolution: ResolutionContext,
  declaration: DeclarationFact,
): PatternTypeResult => {
  const environment = new Map(
    declaration.typeParameters.flatMap((parameter) =>
      parameter.name._tag === 'Present' ? [[parameter.name.spelling, parameter.type] as const] : [],
    ),
  )
  const analyzed = DeclarationCollection.analyzeDeclaredType(source, syntax, environment)
  const nameResolution: NameResolution.Resolution = Object.freeze({
    _tag: 'NameResolution',
    modules: Object.freeze([resolution.scope]),
    diagnostics: Object.freeze([]),
  })
  const resolved = DeclarationResolution.resolveTypeFact(
    resolution.index,
    source.id,
    analyzed.fact,
    (module, path) => NameResolution.resolveType(nameResolution, resolution.index, module, path),
  )
  return Object.freeze({
    ...(resolved.fact._tag === 'Resolved' ? { type: resolved.fact.type } : {}),
    declared: resolved.fact,
    diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics),
  })
}

export const patternDeclaredName = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  token: Token.Token | undefined,
): DeclaredName =>
  token === undefined
    ? Object.freeze({ _tag: 'Unavailable', syntax })
    : Object.freeze({ _tag: 'Present', spelling: spelling(source, token), token })

export const analyzePattern = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  arm: Match.ArmId,
  access: Match.Access,
  scope: Scope,
  resolution: ResolutionContext,
  declaration: DeclarationFact,
  counters: PatternCounters,
  expected?: SemanticType,
  prefix: ReadonlyArray<DeclarationFacts.FieldId> = Object.freeze([]),
  localNames = new Map<string, SourceSpan.SourceSpan>(),
): PatternResult => {
  const id: Match.PatternId = Object.freeze({
    _tag: 'PatternId',
    arm,
    ordinal: counters.pattern,
  })
  counters.pattern += 1
  if (node.kind === 'ErrorPattern') {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'UnavailablePattern',
        id,
        bindings: Object.freeze([]),
        omitted: Object.freeze([]),
        complete: false,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
    })
  }
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

  if (node.kind === 'IntegerPattern') {
    const integer = analyzeInteger(source, node)
    const expectedEnum =
      expected === undefined ? undefined : enumFactByType(resolution.index, expected)
    const value = integer.fact._tag === 'Available' ? integer.fact.value : undefined
    const integerToken = directToken(node, 'DecimalInteger')
    const minusToken = directToken(node, 'Minus')
    const patternSpan =
      integerToken === undefined
        ? node.span
        : Option.getOrElse(
            SourceSpan.make(
              source,
              minusToken?.span.start ?? integerToken.span.start,
              integerToken.span.end,
            ),
            () => node.span,
          )
    const diagnostic =
      expectedEnum?.canonical._tag === 'Canonical' && value !== undefined
        ? Diagnostic.integerPatternAgainstEnum(
            Type.encode(
              Type.nominal(expectedEnum.canonical.id.module, expectedEnum.canonical.id.name),
            ),
            value,
            patternSpan,
          )
        : undefined
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'IntegerPattern',
        id,
        ...(value === undefined ? {} : { value }),
        span: patternSpan,
        bindings: Object.freeze([]),
        omitted: Object.freeze([]),
        complete: false,
        syntax: node,
      }),
      diagnostics: Object.freeze([
        ...integer.diagnostics,
        ...(diagnostic === undefined ? [] : [diagnostic]),
      ]),
    })
  }

  const bareUnionPatternTarget =
    node.kind === 'EnumMemberPattern' || node.kind === 'NominalPattern'
      ? resolveBareUnionVariantTarget(source, node, resolution)
      : undefined

  if (node.kind === 'EnumMemberPattern' && bareUnionPatternTarget === undefined) {
    const identifiers = SyntaxTree.tokens(node).filter((token) => token.kind === 'Identifier')
    const qualifierToken = identifiers.at(0)
    const memberToken = identifiers.at(1)
    const qualifier = qualifierToken === undefined ? undefined : spelling(source, qualifierToken)
    const enumLookup =
      qualifier === undefined
        ? undefined
        : NameResolution.lookup(resolution.scope, resolution.index, qualifier)
    const enum_ =
      enumLookup?._tag === 'Resolved' && enumLookup.declaration._tag === 'EnumDeclaration'
        ? enumLookup.declaration
        : undefined
    const pathSpan =
      qualifierToken === undefined || memberToken === undefined
        ? node.span
        : Option.getOrElse(
            SourceSpan.make(source, qualifierToken.span.start, memberToken.span.end),
            () => node.span,
          )
    const memberName = memberToken === undefined ? undefined : spelling(source, memberToken)
    const memberLookup =
      enum_ === undefined || memberName === undefined
        ? undefined
        : DeclarationFacts.lookupEnumMember(enum_.members, memberName)
    const member = memberLookup?._tag === 'Resolved' ? memberLookup.member : undefined
    const coverage =
      enum_?.canonical._tag === 'Canonical' && member?.canonical._tag === 'Canonical'
        ? Match.enumMember(enum_.canonical.id, member.canonical.id)
        : undefined
    const expectedEnum =
      expected === undefined ? undefined : enumFactByType(resolution.index, expected)
    const unknown =
      enum_ !== undefined && member === undefined && memberName !== undefined
        ? Diagnostic.unknownEnumMember(
            qualifier ?? '<unavailable>',
            memberName,
            memberToken?.span ?? node.span,
          )
        : undefined
    const foreign =
      coverage?._tag === 'EnumMember' &&
      expectedEnum?.canonical._tag === 'Canonical' &&
      (coverage.enum.module !== expectedEnum.canonical.id.module ||
        coverage.enum.name !== expectedEnum.canonical.id.name)
        ? Diagnostic.foreignEnumPattern(
            Type.encode(
              Type.nominal(expectedEnum.canonical.id.module, expectedEnum.canonical.id.name),
            ),
            Type.encode(coverage.type),
            pathSpan,
          )
        : undefined
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'EnumMemberPattern',
        id,
        ...(enum_ === undefined ? {} : { enum: enum_ }),
        ...(member === undefined ? {} : { member }),
        ...(coverage === undefined ? {} : { coverage }),
        ...(qualifierToken === undefined ? {} : { qualifierToken }),
        ...(memberToken === undefined ? {} : { memberToken }),
        span: pathSpan,
        bindings: Object.freeze([]),
        omitted: Object.freeze([]),
        complete: coverage !== undefined && unknown === undefined && foreign === undefined,
        syntax: node,
      }),
      diagnostics: Object.freeze([
        ...(unknown === undefined ? [] : [unknown]),
        ...(foreign === undefined ? [] : [foreign]),
      ]),
    })
  }

  if (node.kind === 'BindingPattern') {
    // `Member name` binds the entire member payload: no field destructuring, nothing omitted.
    const bindingTargetSyntax =
      SyntaxTree.directNode(node, 'AppliedType') ??
      SyntaxTree.directNode(node, 'FixedArrayType') ??
      childNode(node, 'TypePath')
    const bindingTarget = resolvePatternType(source, bindingTargetSyntax, resolution, declaration)
    const bindingDiagnostics: Array<Diagnostic.Diagnostic> = [...bindingTarget.diagnostics]
    const member = bindingTarget.type
    const bindingToken = node.children.find(
      (element): element is Token.Token =>
        SyntaxTree.isToken(element) && element.kind === 'Identifier',
    )
    const declaredName: DeclaredName =
      bindingToken === undefined
        ? Object.freeze({ _tag: 'Unavailable' as const, syntax: node })
        : Object.freeze({
            _tag: 'Present' as const,
            spelling: spelling(source, bindingToken),
            token: bindingToken,
          })
    if (declaredName._tag === 'Present') {
      const original =
        scopeSpanFor(scope, declaredName.spelling) ?? localNames.get(declaredName.spelling)
      if (original === undefined) localNames.set(declaredName.spelling, declaredName.token.span)
      else {
        counters.invalid = true
        bindingDiagnostics.push(
          Diagnostic.patternBindingConflict(
            declaredName.spelling,
            original,
            declaredName.token.span,
          ),
        )
      }
    }
    const wholeBinding: PatternBindingFact = Object.freeze({
      _tag: 'PatternBinding',
      id: Object.freeze({ _tag: 'PatternBindingId' as const, arm, ordinal: counters.binding }),
      name: declaredName,
      path: prefix,
      type: member === undefined ? unavailableExpressionType : availableExpressionType(member),
      access,
      syntax: node,
    })
    counters.binding += 1
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'TypePattern',
        id,
        ...(member === undefined ? {} : { member }),
        declared: bindingTarget.declared,
        bindings: Object.freeze([wholeBinding]),
        omitted: Object.freeze([]),
        complete: member !== undefined && !counters.invalid && isAvailableSyntax(node),
        syntax: node,
      }),
      diagnostics: Object.freeze(bindingDiagnostics),
    })
  }

  const variantSelector = SyntaxTree.directNode(node, 'AppliedMemberSelector')
  const unionTarget =
    variantSelector === undefined
      ? bareUnionPatternTarget
      : resolveUnionVariantTarget(source, variantSelector, resolution, declaration)
  let targetSyntax: SyntaxTree.Node
  if (variantSelector === undefined && unionTarget !== undefined) {
    targetSyntax = node
  } else if (variantSelector === undefined) {
    targetSyntax = SyntaxTree.directNode(node, 'AppliedType') ?? childNode(node, 'TypePath')
  } else {
    targetSyntax =
      SyntaxTree.directNode(variantSelector, 'AppliedType') ??
      childNode(variantSelector, 'TypePath')
  }
  const structTarget =
    unionTarget === undefined
      ? resolveStructTarget(source, targetSyntax, resolution, declaration)
      : undefined
  const target = unionTarget ?? structTarget
  if (target === undefined) throw new RangeError('Pattern target resolution is unavailable')
  const diagnostics: Array<Diagnostic.Diagnostic> = [...target.diagnostics]
  const struct =
    target.fact._tag === 'Resolved' && 'struct' in target.fact ? target.fact.struct : undefined
  const union =
    target.fact._tag === 'Resolved' && 'union' in target.fact ? target.fact.union : undefined
  const variant =
    target.fact._tag === 'Resolved' && 'variant' in target.fact ? target.fact.variant : undefined
  const aggregate = struct ?? union
  const aggregateFields = struct?.fields ?? variant?.fields ?? Object.freeze([])
  const nominal = target.fact._tag === 'Resolved' ? target.fact.type : undefined
  const structSubstitution =
    aggregate === undefined || nominal === undefined
      ? new Map<string, SemanticType>()
      : (TypeInference.substitution(
          aggregate.typeParameters.map((parameter) => parameter.type),
          nominal.arguments,
        ) ?? new Map())
  const unresolvedParameters =
    aggregate === undefined || nominal === undefined
      ? []
      : aggregate.typeParameters.filter((parameter, ordinal) => {
          const argument = nominal.arguments.at(ordinal)
          return argument === undefined || isOwnStructArgument(parameter.type, argument)
        })
  for (const parameter of unresolvedParameters) {
    diagnostics.push(
      Diagnostic.uninferredTypeParameter(
        nominal === undefined ? 'unknown aggregate' : Type.display(nominal),
        parameter.type.name,
        parameter.syntax.span,
      ),
    )
  }
  const label = nominal === undefined ? 'unknown aggregate' : Type.display(nominal)
  const outsideDefiningModule = nominal !== undefined && nominal.module !== source.id
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
      aggregate === undefined || name === undefined
        ? undefined
        : DeclarationFacts.lookupField(aggregateFields, name)
    let state: PatternFieldState = Object.freeze({ _tag: 'Unavailable' })
    let resolvedField: DeclarationFacts.FieldFact | undefined
    if (
      lookup?._tag === 'Resolved' &&
      lookup.field.visibility === 'Private' &&
      outsideDefiningModule
    ) {
      const diagnostic = Diagnostic.inaccessibleProjectedField(
        label,
        name ?? '',
        nameToken?.span ?? fieldNode.span,
      )
      diagnostics.push(diagnostic)
      counters.invalid = true
      state = Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) })
    } else if (lookup?._tag === 'Resolved') {
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

    const nestedNode =
      SyntaxTree.directNode(fieldNode, 'UnionVariantPattern') ??
      SyntaxTree.directNode(fieldNode, 'NominalPattern') ??
      SyntaxTree.directNode(fieldNode, 'BindingPattern')
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
        undefined,
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
        (nested._tag === 'NominalPattern' ||
          nested._tag === 'UnionVariantPattern' ||
          nested._tag === 'TypePattern') &&
        nested.member !== undefined &&
        !Type.equals(expected, nested.member)
      ) {
        counters.invalid = true
        diagnostics.push(
          Diagnostic.matchMemberNotInScrutinee(
            Type.display(nested.member),
            Type.display(expected),
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
      ...(nameToken === undefined ? {} : { token: nameToken }),
      state,
      ...(binding === undefined ? {} : { binding }),
      ...(nested === undefined ? {} : { nested }),
      syntax: fieldNode,
    })
    if (name !== undefined && !seen.has(name)) seen.set(name, fact)
    return fact
  })

  const rest = SyntaxTree.directNode(node, 'RestPattern') !== undefined
  const omitted: Array<ReadonlyArray<DeclarationFacts.FieldId>> = fields.flatMap(
    (field) => field.nested?.omitted ?? [],
  )
  if (aggregate !== undefined && !rest) {
    let omittedInaccessible = false
    for (const field of aggregateFields) {
      if (field.name._tag !== 'Present' || seen.has(field.name.spelling)) continue
      if (field.visibility === 'Private' && outsideDefiningModule) {
        omittedInaccessible = true
        continue
      }
      diagnostics.push(Diagnostic.missingPatternField(label, field.name.spelling, node.span))
    }
    if (omittedInaccessible)
      diagnostics.push(Diagnostic.inaccessiblePatternFields(label, node.span))
  } else if (aggregate !== undefined && rest) {
    for (const field of aggregateFields) {
      if (field.name._tag === 'Present' && seen.has(field.name.spelling)) continue
      omitted.push(Object.freeze([...prefix, field.id]))
    }
  }
  const complete =
    target.fact._tag === 'Resolved' &&
    unresolvedParameters.length === 0 &&
    !counters.invalid &&
    isAvailableSyntax(node) &&
    fields.every(
      (field) =>
        field.state._tag === 'Resolved' &&
        (field.nested === undefined ||
          ((field.nested._tag === 'NominalPattern' ||
            field.nested._tag === 'UnionVariantPattern' ||
            field.nested._tag === 'TypePattern') &&
            field.nested.complete)),
    ) &&
    (rest ||
      aggregateFields.every(
        (field) => field.name._tag !== 'Present' || seen.has(field.name.spelling),
      ) === true)
  if (unionTarget !== undefined) {
    const coverage =
      nominal !== undefined && variant?.canonical._tag === 'Canonical'
        ? Match.nominalUnionVariant(nominal, nominal, variant.canonical.id, variant.id.ordinal)
        : undefined
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'UnionVariantPattern',
        id,
        target: unionTarget.fact,
        ...(nominal === undefined ? {} : { member: nominal }),
        ...(coverage === undefined ? {} : { coverage }),
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
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'NominalPattern',
      id,
      target: structTarget?.fact ?? Object.freeze({ _tag: 'Unavailable' }),
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

export const unavailableExpression = (syntax: SyntaxTree.Node): ExpressionFact =>
  Object.freeze({
    _tag: 'Identifier',
    reference: Object.freeze({ _tag: 'Unavailable', syntax }),
    type: unavailableExpressionType,
    syntax,
  })

export const matchAccess = (node: SyntaxTree.Node): Match.Access => {
  const access = SyntaxTree.directNode(node, 'MatchAccess')
  if (access === undefined) return 'Copy'
  if (directToken(access, 'MoveKeyword') !== undefined) return 'Move'
  if (directToken(access, 'Ampersand') === undefined) return 'Copy'
  return directToken(access, 'MutKeyword') === undefined ? 'Shared' : 'Exclusive'
}

export const analyzeMatch = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
  borrowAllowed = false,
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
  const enumScrutinee =
    scrutinee?.type === undefined ? undefined : enumFactByType(resolution.index, scrutinee.type)
  let members: readonly Match.CoverageIdentity[] | undefined
  if (scrutinee?.type === undefined) {
    members = undefined
  } else if (enumScrutinee === undefined) {
    members = coverageMembersOf(resolution.index, scrutinee.type)
  } else {
    members = Match.enumMembersOf(enumScrutinee)
  }

  const preliminary = SyntaxTree.directNodes(node, 'MatchArm').map((armNode, ordinal) => {
    const armId: Match.ArmId = Object.freeze({ _tag: 'MatchArmId', match: id, ordinal })
    const patternNode =
      SyntaxTree.directNode(armNode, 'ErrorPattern') ??
      SyntaxTree.directNode(armNode, 'EnumMemberPattern') ??
      SyntaxTree.directNode(armNode, 'IntegerPattern') ??
      SyntaxTree.directNode(armNode, 'UnionVariantPattern') ??
      SyntaxTree.directNode(armNode, 'NominalPattern') ??
      SyntaxTree.directNode(armNode, 'BindingPattern') ??
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
      scrutinee?.type,
    )
    diagnostics.push(...pattern.diagnostics)
    return Object.freeze({ armNode, armId, pattern: pattern.fact })
  })
  const coverageIdentity = (pattern: PatternFact): Match.CoverageIdentity | undefined => {
    if (pattern._tag === 'EnumMemberPattern') return pattern.coverage
    if (pattern._tag === 'UnionVariantPattern') return pattern.coverage
    return (pattern._tag === 'NominalPattern' || pattern._tag === 'TypePattern') &&
      pattern.member !== undefined
      ? Match.structuralMember(pattern.member)
      : undefined
  }
  const coverage = Match.cover(
    members ?? Object.freeze([]),
    preliminary.map(({ armNode, pattern }) => {
      const member = coverageIdentity(pattern)
      return Object.freeze({
        ...(member === undefined ? {} : { member }),
        universal: pattern._tag === 'UniversalPattern',
        guarded: directToken(armNode, 'IfKeyword') !== undefined,
      })
    }),
  )
  const semanticPatternSpan = (pattern: PatternFact): SourceSpan.SourceSpan => {
    if (pattern._tag === 'EnumMemberPattern' || pattern._tag === 'IntegerPattern')
      return pattern.span
    const first = SyntaxTree.tokens(pattern.syntax).find(
      (token) =>
        token.kind !== 'Whitespace' &&
        token.kind !== 'LineComment' &&
        token.kind !== 'DocComment' &&
        token.kind !== 'ModuleDocComment',
    )
    return first === undefined
      ? pattern.syntax.span
      : Option.getOrElse(
          SourceSpan.make(source, first.span.start, pattern.syntax.span.end),
          () => pattern.syntax.span,
        )
  }
  const semanticArmSpan = (pattern: PatternFact, armNode: SyntaxTree.Node): SourceSpan.SourceSpan =>
    Option.getOrElse(
      SourceSpan.make(source, semanticPatternSpan(pattern).start, armNode.span.end),
      () => armNode.span,
    )
  const firstCoveringArm = new Map<string, SourceSpan.SourceSpan>()
  let wildcardArm: SourceSpan.SourceSpan | undefined
  const arms = preliminary.map(({ armNode, armId, pattern }, ordinal): MatchArmFact => {
    const transition = coverage.transitions.at(ordinal)
    if (transition === undefined) throw new RangeError('Match coverage lost an arm')
    const member = coverageIdentity(pattern)
    const guarded = directToken(armNode, 'IfKeyword') !== undefined
    const memberInDomain =
      member !== undefined && members?.some((candidate) => Match.selects(member, candidate))
    if (
      enumScrutinee === undefined &&
      member !== undefined &&
      members !== undefined &&
      !memberInDomain
    ) {
      diagnostics.push(
        Diagnostic.matchMemberNotInScrutinee(
          Type.display(member.type),
          scrutinee?.type === undefined ? 'unknown' : Type.display(scrutinee.type),
          pattern.syntax.span,
        ),
      )
    } else if (
      enumScrutinee !== undefined &&
      pattern._tag !== 'UnavailablePattern' &&
      !transition.reachable &&
      (members?.length ?? 0) > 0
    ) {
      if (wildcardArm !== undefined) {
        diagnostics.push(
          Diagnostic.enumMatchArmAfterWildcard(wildcardArm, semanticArmSpan(pattern, armNode)),
        )
      } else if (member !== undefined && memberInDomain) {
        const original = firstCoveringArm.get(Match.encodeIdentity(member))
        if (original !== undefined)
          diagnostics.push(
            Diagnostic.duplicateEnumMatchArm(
              Match.encodeIdentity(member),
              original,
              semanticPatternSpan(pattern),
            ),
          )
      }
    } else if (
      enumScrutinee === undefined &&
      pattern._tag !== 'UnavailablePattern' &&
      !transition.reachable &&
      (members?.length ?? 0) > 0
    ) {
      let identity = 'unknown'
      if (pattern._tag === 'UniversalPattern') identity = '_'
      else if (member !== undefined) identity = Match.encodeIdentity(member)
      diagnostics.push(Diagnostic.unreachableMatchArm(identity, armNode.span))
    }
    if (!guarded) {
      if (pattern._tag === 'UniversalPattern' && wildcardArm === undefined)
        wildcardArm = semanticArmSpan(pattern, armNode)
      else if (member !== undefined && memberInDomain && transition.reachable)
        firstCoveringArm.set(Match.encodeIdentity(member), semanticPatternSpan(pattern))
    }
    const armExpressions = armNode.children.filter(isExpressionNode)
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
      if (guard.type !== undefined && guard.type !== 'bool') {
        diagnostics.push(
          Diagnostic.matchGuardNotBool(Type.display(guard.type), guardNode?.span ?? armNode.span),
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
            borrowAllowed,
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
  if (
    members !== undefined &&
    !coverage.exhaustive &&
    !preliminary.some(({ pattern }) => pattern._tag === 'UnavailablePattern')
  ) {
    diagnostics.push(
      enumScrutinee?.canonical._tag === 'Canonical'
        ? Diagnostic.incompleteEnumMatch(
            Type.encode(
              Type.nominal(enumScrutinee.canonical.id.module, enumScrutinee.canonical.id.name),
            ),
            coverage.missing.map(Match.encodeIdentity),
            Option.getOrElse(
              SourceSpan.make(
                source,
                directToken(node, 'MatchKeyword')?.span.start ?? node.span.start,
                node.span.end,
              ),
              () => node.span,
            ),
          )
        : Diagnostic.incompleteMatch(coverage.missing.map(Match.encodeIdentity), node.span),
    )
  }
  const reachableTypes = arms.flatMap((arm) =>
    arm.reachable && arm.result.type._tag === 'Available' ? [arm.result.type.type] : [],
  )
  const unavailableReachableResult = arms.some(
    (arm) => arm.reachable && arm.result.type._tag !== 'Available',
  )
  const anonymousTypes = reachableTypes.filter((type) => {
    if (!Type.isNominal(type)) return false
    const aggregate = aggregateByNominal(resolution, type)
    return (
      aggregate?.aggregateKind === 'AnonymousNamed' ||
      aggregate?.aggregateKind === 'AnonymousPositional'
    )
  })
  const anonymousDisagreement = new Set(anonymousTypes.map(Type.key)).size > 1
  const joined: Match.Join = anonymousDisagreement
    ? Object.freeze({ _tag: 'Incompatible', types: Object.freeze(reachableTypes) })
    : Match.join(reachableTypes)
  if (joined._tag === 'Incompatible') {
    let divergentRepresentations:
      | {
          readonly divergence: Type.RepresentationDivergence
          readonly spans: readonly [SourceSpan.SourceSpan, SourceSpan.SourceSpan]
        }
      | undefined
    const reachableResults = arms.flatMap((arm) =>
      arm.reachable && arm.result.type._tag === 'Available'
        ? [Object.freeze({ type: arm.result.type.type, span: arm.result.syntax.span })]
        : [],
    )
    for (const [leftOrdinal, left] of reachableResults.entries()) {
      for (const right of reachableResults.slice(leftOrdinal + 1)) {
        const divergence = Type.firstRepresentationDivergence(left.type, right.type)
        if (divergence !== undefined) {
          divergentRepresentations = Object.freeze({
            divergence,
            spans: Object.freeze([left.span, right.span] as const),
          })
          break
        }
      }
      if (divergentRepresentations !== undefined) break
    }
    let diagnostic: Diagnostic.Diagnostic
    if (anonymousDisagreement) {
      diagnostic = Diagnostic.anonymousAggregateJoinMismatch(
        joined.types.map(Type.display),
        node.span,
      )
    } else if (divergentRepresentations === undefined) {
      diagnostic = Diagnostic.incompatibleMatchResults(joined.types.map(Type.encode), node.span)
    } else {
      diagnostic = Diagnostic.divergentRepresentationJoin(
        Type.encodeGenericArgument(divergentRepresentations.divergence.left),
        Type.encodeGenericArgument(divergentRepresentations.divergence.right),
        divergentRepresentations.spans,
        node.span,
      )
    }
    diagnostics.push(diagnostic)
  }
  const hasInvalidGuard = arms.some(
    (arm) =>
      arm.guard !== undefined &&
      arm.guard.type._tag === 'Available' &&
      arm.guard.type.type !== 'bool',
  )
  let joinedEffect: Type.Effect | undefined
  if (joined._tag === 'Joined') {
    if (Type.isRepresented(joined.type)) {
      if (Type.isEffect(joined.type.contract)) {
        joinedEffect = joined.type.contract
      } else {
        joinedEffect = undefined
      }
    } else if (Type.isEffect(joined.type)) {
      joinedEffect = joined.type
    } else {
      joinedEffect = undefined
    }
  } else {
    joinedEffect = undefined
  }
  let effectAlternatives: readonly Type.ExactRepresentationArgument[]
  if (joinedEffect === undefined) {
    effectAlternatives = Object.freeze([])
  } else {
    effectAlternatives = Object.freeze(
      arms.flatMap((arm) => {
        if (!arm.reachable) return []
        const representation = representationOfExpression(arm.result)
        return representation !== undefined &&
          Type.isExactRepresentationArgument(representation) &&
          Type.isEffectIdentityArgument(representation.identity) &&
          Type.isEffect(representation.contract)
          ? [representation]
          : []
      }),
    )
  }
  const reachableEffectArms =
    joinedEffect === undefined
      ? 0
      : arms.filter(
          (arm) =>
            arm.reachable &&
            // Mirrors Match.join: diverging (never-typed) arms do not contribute to the join,
            // so they are not required to carry an exact static Effect representation.
            !(arm.result.type._tag === 'Available' && Type.isNever(arm.result.type.type)),
        ).length
  const unavailableEffectComposite =
    joinedEffect !== undefined && effectAlternatives.length !== reachableEffectArms
  if (unavailableEffectComposite)
    diagnostics.push(
      Diagnostic.nonFiniteEffectJoin(
        'every reachable alternative must retain one exact static Effect representation',
        node.span,
      ),
    )
  const callableSites = arms.flatMap((arm) =>
    arm.reachable && arm.result._tag === 'CallableSection'
      ? [Hir.executableSiteKey(arm.result.site)]
      : [],
  )
  // Every reachable arm must construct the same exact callable: a structural callable type names
  // a contract, not a universal representation the arms could be erased into.
  const callableIdentities =
    joined._tag === 'Joined' && Type.isCallable(joined.type)
      ? arms.flatMap((arm) => {
          const representation = arm.reachable ? representationOfExpression(arm.result) : undefined
          return representation !== undefined &&
            Type.isExactRepresentationArgument(representation) &&
            Type.isCallableIdentityArgument(representation.identity)
            ? [Type.genericArgumentKey(representation.identity)]
            : []
        })
      : []
  const erasesCallableIdentity =
    new Set(callableSites).size > 1 || new Set(callableIdentities).size > 1
  if (erasesCallableIdentity) diagnostics.push(Diagnostic.callableIdentityErasure(node.span))
  let type: ExpressionTypeFact
  if (
    members !== undefined &&
    coverage.exhaustive &&
    arms.every(
      (arm) =>
        arm.reachable &&
        (arm.pattern._tag === 'UniversalPattern' ||
          (arm.pattern._tag === 'EnumMemberPattern' && arm.pattern.complete) ||
          ((arm.pattern._tag === 'NominalPattern' ||
            arm.pattern._tag === 'UnionVariantPattern' ||
            arm.pattern._tag === 'TypePattern') &&
            arm.pattern.complete)),
    ) &&
    !unavailableReachableResult &&
    !hasInvalidGuard &&
    !unavailableEffectComposite &&
    !erasesCallableIdentity &&
    joined._tag === 'Joined'
  ) {
    type = availableExpressionType(
      joinedEffect === undefined
        ? joined.type
        : Type.represented(
            joinedEffect,
            joinedEffect,
            effectAlternatives.length === 1
              ? (effectAlternatives.at(0) ??
                  Type.compositeEffectRepresentationArgument(joinedEffect, effectAlternatives))
              : Type.compositeEffectRepresentationArgument(joinedEffect, effectAlternatives),
          ),
    )
  } else {
    type = unavailableExpressionType
  }
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

export const callableRepresentationTarget = (
  reference: CallReferenceFact,
): Type.CallableIdentityArgument['target'] | undefined => {
  if (reference._tag === 'Resolved') {
    const canonical = reference.declaration.canonical
    return canonical._tag === 'Canonical'
      ? Object.freeze({
          _tag: 'Declaration',
          module: canonical.id.module,
          name: canonical.id.name,
        })
      : undefined
  }
  return reference._tag === 'ResolvedBuiltin'
    ? Object.freeze({
        _tag: 'Builtin',
        actor: reference.actor,
        operation: reference.operation,
        intrinsic: Object.freeze({
          actor: reference.intrinsic.actor,
          name: reference.intrinsic.name,
        }),
      })
    : undefined
}

export const exactCallableRepresentation = (
  reference: CallReferenceFact,
  contract: Type.Callable,
  typeArguments: ReadonlyArray<Type.GenericArgument> = Object.freeze([]),
  environment?: Type.CallableEnvironmentIdentity,
): Type.ExactRepresentationArgument | undefined => {
  const target = callableRepresentationTarget(reference)
  if (target === undefined) return undefined
  const identity =
    target._tag === 'Declaration'
      ? `declaration:${target.module}:${target.name}`
      : `builtin:${target.actor}:${target.operation}`
  return Type.exactRepresentationArgument(
    Type.callableIdentityArgument(identity, target, typeArguments, environment),
    contract,
  )
}

export const exactEffectDeclarationRepresentation = (
  declaration: DeclarationFact,
  contract: Type.Effect,
  typeArguments: ReadonlyArray<Type.GenericArgument>,
): Type.ExactRepresentationArgument | undefined => {
  if (declaration.functionKind !== 'Effect' || declaration.canonical._tag !== 'Canonical')
    return undefined
  const owner = Object.freeze({
    declaration: Object.freeze({
      module: declaration.canonical.id.module,
      name: declaration.canonical.id.name,
    }),
    typeArguments,
  })
  const site: Hir.EffectSiteId = Object.freeze({
    _tag: 'EffectSiteId',
    function: declaration.id,
    owner: declaration.canonical.id,
    ordinal: -1,
    span: declaration.syntax.span,
  })
  return Type.exactRepresentationArgument(
    Type.effectIdentityArgument(Hir.effectRepresentationIdentity(site), owner),
    contract,
  )
}

export const exactEffectIdentityOfExpression = (
  expression: ExpressionFact,
): Type.EffectIdentityArgument | undefined => {
  const representation = representationOfExpression(expression)
  return representation?._tag === 'ExactRepresentationArgument' &&
    Type.isEffectIdentityArgument(representation.identity)
    ? representation.identity
    : undefined
}

export const hiddenEffectArguments = (
  declaration: DeclarationFact,
  substitution: Type.Substitution,
  argumentAt: (ordinal: number) => ExpressionFact | undefined,
): ReadonlyArray<Type.EffectIdentityArgument> =>
  Object.freeze(
    declaration.parameters.flatMap((parameter, ordinal) => {
      const declared = parameter.declaredType
      if (declared._tag !== 'Resolved') return []
      const specialized = Type.substitute(declared.type, substitution)
      const contract = Type.isRepresented(specialized) ? specialized.contract : specialized
      if (!Type.isEffect(contract)) return []
      const argument = argumentAt(ordinal)
      const identity =
        argument === undefined ? undefined : exactEffectIdentityOfExpression(argument)
      return identity === undefined ? [] : [identity]
    }),
  )

export const exactEffectApplicationContract = (
  _declaration: DeclarationFact,
  _substitution: Type.Substitution,
  contract: Type.Effect,
): Type.Effect => contract

export const effectCallableApplicationRepresentation = (
  expression: CallableApplyExpressionFact,
  contract: Type.Effect,
): Type.ExactRepresentationArgument | undefined => {
  const callee = expression.callee
  if (callee._tag !== 'CallableSection' || callee.reference._tag !== 'Resolved') return undefined
  const substitution = new Map(callee.substitution)
  for (const [parameter, argument] of expression.substitution) substitution.set(parameter, argument)
  const declaredArguments = Object.freeze(
    callee.reference.declaration.typeParameters.map(
      (parameter) =>
        substitution.get(Type.key(parameter.type)) ??
        Type.substituteGenericArgument(parameter.type, substitution),
    ),
  )
  const applicationArgument = (ordinal: number): ExpressionFact | undefined => {
    const captured = callee.captures.find((capture) => capture.parameterOrdinal === ordinal)
    if (captured !== undefined) return captured.expression
    const argumentOrdinal = callee.remainingParameters.indexOf(ordinal)
    return argumentOrdinal < 0 ? undefined : expression.arguments.at(argumentOrdinal)?.expression
  }
  return exactEffectDeclarationRepresentation(
    callee.reference.declaration,
    exactEffectApplicationContract(callee.reference.declaration, substitution, contract),
    Object.freeze([
      ...declaredArguments,
      ...hiddenEffectArguments(callee.reference.declaration, substitution, applicationArgument),
    ]),
  )
}

/**
 * Recovers a compile-time representation from semantic expression structure. This is deliberately
 * frontend-owned: later phases consume the retained argument and never reconstruct it from syntax.
 */
export function representationOfExpression(
  expression: ExpressionFact,
): Type.RepresentationArgument | undefined {
  if (expression.type._tag === 'Available' && Type.isRepresented(expression.type.type))
    return expression.type.type.representation.argument
  if (expression._tag === 'FunctionItem' && expression.type._tag === 'Available') {
    const contract = expression.type.type
    return Type.isCallable(contract)
      ? exactCallableRepresentation(expression.reference, contract)
      : undefined
  }
  if (expression._tag === 'CallableSection' && expression.type._tag === 'Available') {
    const contract = expression.type.type
    if (!Type.isCallable(contract)) return undefined
    if (expression.captures.length === 0)
      return exactCallableRepresentation(expression.reference, contract, expression.typeArguments)
    if (expression.environmentOwner === undefined) return undefined
    const environment = Hir.callableEnvironmentIdentity(
      expression.site,
      expression.environmentOwner,
    )
    return exactCallableRepresentation(
      expression.reference,
      contract,
      expression.typeArguments,
      environment,
    )
  }
  if (expression._tag === 'EffectBlock' && expression.type._tag === 'Available') {
    const contract = expression.type.type
    if (!Type.isEffect(contract)) return undefined
    const site = expression.site
    return Type.exactRepresentationArgument(
      Type.effectIdentityArgument(
        Hir.effectRepresentationIdentity(site),
        expression.representationOwner,
      ),
      contract,
    )
  }
  if (
    expression._tag === 'Call' &&
    expression.reference._tag === 'Resolved' &&
    expression.contract._tag === 'Compatible' &&
    expression.type._tag === 'Available' &&
    Type.isEffect(expression.type.type)
  ) {
    return exactEffectDeclarationRepresentation(
      expression.reference.declaration,
      expression.type.type,
      Object.freeze([
        ...expression.contract.typeArguments,
        ...hiddenEffectArguments(
          expression.reference.declaration,
          expression.contract.substitution,
          (ordinal) => expression.arguments.at(ordinal)?.expression,
        ),
      ]),
    )
  }
  if (
    expression._tag === 'CallableApply' &&
    expression.type._tag === 'Available' &&
    Type.isEffect(expression.type.type)
  ) {
    return effectCallableApplicationRepresentation(expression, expression.type.type)
  }
  if (expression._tag === 'Identifier' && expression.reference._tag === 'ResolvedBinding')
    return representationOfExpression(expression.reference.binding.initializer)
  if (expression._tag === 'Move') return representationOfExpression(expression.subject)
  if (expression._tag === 'Grouped') return representationOfExpression(expression.expression)
  return undefined
}

export interface InferredStructArgument {
  readonly argument: Type.GenericArgument
  readonly span: SourceSpan.SourceSpan
}

export const isOwnStructArgument = (
  parameter: Type.Parameter,
  argument: Type.GenericArgument,
): boolean => Type.equalsGenericArgument(Type.parameterArgument(parameter), argument)

export const analyzeAggregateLiteral = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  contextualTarget?: StructTargetResult,
): ExpressionResult => {
  const selector = SyntaxTree.directNode(node, 'AppliedMemberSelector')
  const unionTarget =
    selector === undefined
      ? resolveBareUnionVariantTarget(source, node, resolution)
      : resolveUnionVariantTarget(source, selector, resolution, declaration)
  let targetSyntax: SyntaxTree.Node
  if (contextualTarget !== undefined) {
    targetSyntax = node
  } else if (selector !== undefined) {
    targetSyntax = SyntaxTree.directNode(selector, 'AppliedType') ?? childNode(selector, 'TypePath')
  } else if (unionTarget !== undefined && node.kind === 'FieldProjectionExpression') {
    targetSyntax = node
  } else {
    targetSyntax = SyntaxTree.directNode(node, 'AppliedType') ?? childNode(node, 'TypePath')
  }
  const target =
    contextualTarget ??
    unionTarget ??
    resolveStructTarget(source, targetSyntax, resolution, declaration, true)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...target.diagnostics]
  let aggregate: DeclarationFacts.StructFact | DeclarationFacts.UnionFact | undefined
  let aggregateFields: ReadonlyArray<DeclarationFacts.FieldFact> = Object.freeze([])
  if (target.fact._tag === 'Resolved') {
    if ('union' in target.fact) {
      aggregate = target.fact.union
      aggregateFields = target.fact.variant.fields
    } else {
      aggregate = target.fact.struct
      aggregateFields = target.fact.struct.fields
    }
  }
  const nominal = target.fact._tag === 'Resolved' ? target.fact.type : undefined
  const nominalLabel = nominal === undefined ? 'unknown aggregate' : Type.display(nominal)
  const forbiddenPositionalFields =
    aggregate?._tag === 'StructDeclaration' &&
    aggregate.aggregateKind === 'Positional' &&
    node.kind === 'StructLiteralExpression'
  if (forbiddenPositionalFields)
    diagnostics.push(Diagnostic.positionalFieldConstruction(nominalLabel, node.span))
  const inferredArguments = new Map<string, InferredStructArgument>()
  const argumentOrigins = new Map<string, ReadonlyArray<SourceSpan.SourceSpan>>()
  const explicitArguments = new Set<string>()
  if (aggregate !== undefined && nominal !== undefined) {
    for (const [ordinal, parameter] of aggregate.typeParameters.entries()) {
      const argument = nominal.arguments.at(ordinal)
      if (
        argument === undefined ||
        isOwnStructArgument(parameter.type, argument) ||
        (contextualTarget !== undefined &&
          Type.isTypeArgument(argument) &&
          Type.isParameter(argument))
      )
        continue
      const parameterKey = Type.key(parameter.type)
      inferredArguments.set(parameterKey, Object.freeze({ argument, span: targetSyntax.span }))
      argumentOrigins.set(parameterKey, Object.freeze([targetSyntax.span]))
      explicitArguments.add(parameterKey)
    }
  }
  const structSubstitution = new Map<string, Type.GenericArgument>()
  for (const [parameterKey, inferred] of inferredArguments)
    structSubstitution.set(parameterKey, inferred.argument)
  const definingModule = nominal?.module
  const authorized =
    definingModule !== undefined &&
    aggregate !== undefined &&
    (aggregate.visibility === 'Public' || definingModule === source.id) &&
    aggregateFields.every((field) => field.visibility === 'Public' || definingModule === source.id)
  const accessDiagnostic =
    nominal !== undefined && !authorized
      ? Diagnostic.inaccessibleStructConstruction(Type.display(nominal), node.span)
      : undefined
  if (accessDiagnostic !== undefined) diagnostics.push(accessDiagnostic)

  const seen = new Map<string, StructInitializerFact>()
  const initializers = SyntaxTree.directNodes(node, 'StructFieldInitializer').map(
    (initializer): StructInitializerFact => {
      const nameToken = directToken(initializer, 'Identifier')
      const name = nameToken === undefined ? undefined : spelling(source, nameToken)
      const fieldLookup =
        aggregate === undefined || name === undefined
          ? undefined
          : DeclarationFacts.lookupField(aggregateFields, name)
      const contextualFieldType =
        fieldLookup?._tag === 'Resolved' && fieldLookup.field.declaredType._tag === 'Resolved'
          ? Type.substitute(fieldLookup.field.declaredType.type, structSubstitution)
          : undefined
      const contextualExpected =
        contextualFieldType !== undefined && Type.isRepresented(contextualFieldType)
          ? contextualFieldType.contract
          : contextualFieldType
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
        contextualExpected,
      )
      if (expression === undefined) {
        throw new RangeError(`Cannot analyze struct initializer ${expressionNode.kind}`)
      }
      diagnostics.push(...expression.diagnostics)
      let state: StructInitializerState = Object.freeze({ _tag: 'Unavailable' })
      if (name !== undefined && nameToken !== undefined && aggregate !== undefined) {
        const previous = seen.get(name)
        if (forbiddenPositionalFields) {
          state = Object.freeze({ _tag: 'Unavailable' })
        } else if (fieldLookup?._tag !== 'Resolved') {
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
          fieldLookup.field.visibility === 'Private' &&
          nominal !== undefined &&
          nominal.module !== source.id &&
          accessDiagnostic !== undefined
        ) {
          state = Object.freeze({
            _tag: 'Inaccessible',
            field: fieldLookup.field,
            cause: Diagnostic.identity(accessDiagnostic),
          })
        } else if (
          fieldLookup.field.declaredType._tag === 'Resolved' &&
          expression.type !== undefined
        ) {
          const expectedType = fieldLookup.field.declaredType.type
          const expectedValue = Type.isRepresented(expectedType)
            ? expectedType.contract
            : expectedType
          const actualValue = Type.isRepresented(expression.type)
            ? expression.type.contract
            : expression.type
          let representationDiagnostic: Diagnostic.Diagnostic | undefined
          if (Type.isRepresented(expectedType)) {
            const currentSubstitution = new Map(structSubstitution)
            for (const [parameterKey, inferred] of inferredArguments)
              currentSubstitution.set(parameterKey, inferred.argument)
            const candidateSubstitution = new Map(currentSubstitution)
            if (TypeInference.infer(expectedType.contract, actualValue, candidateSubstitution)) {
              const siteSubstitution = new Map<string, Type.GenericArgument>()
              TypeInference.infer(expectedType.contract, actualValue, siteSubstitution)
              for (const parameter of aggregate.typeParameters) {
                if (
                  parameter.type.kind === 'CallableRepresentation' ||
                  parameter.type.kind === 'EffectRepresentation'
                )
                  continue
                const parameterKey = Type.key(parameter.type)
                const inferred = siteSubstitution.get(parameterKey)
                if (inferred === undefined || isOwnStructArgument(parameter.type, inferred))
                  continue
                if (inferredArguments.get(parameterKey) === undefined)
                  inferredArguments.set(
                    parameterKey,
                    Object.freeze({ argument: inferred, span: expressionNode.span }),
                  )
                argumentOrigins.set(
                  parameterKey,
                  Object.freeze([
                    ...(argumentOrigins.get(parameterKey) ?? []),
                    expressionNode.span,
                  ]),
                )
              }
            }
            const representationSubstitution = new Map(structSubstitution)
            for (const [parameterKey, inferred] of inferredArguments)
              representationSubstitution.set(parameterKey, inferred.argument)
            const specialized = Type.substitute(expectedType, representationSubstitution)
            if (!Type.isRepresented(specialized))
              throw new RangeError('represented struct field lost its representation contract')
            const specializedExpectedType = specialized
            const actualRepresentation = representationOfExpression(expression.fact)
            const requiredArgument = expectedType.representation.argument
            if (actualRepresentation === undefined) {
              representationDiagnostic = Diagnostic.structFieldTypeMismatch(
                name,
                Type.display(specializedExpectedType),
                Type.display(expression.type),
                expressionNode.span,
              )
            } else if (requiredArgument._tag === 'RepresentationParameterArgument') {
              const parameter = requiredArgument.parameter
              const parameterKey = Type.key(parameter)
              const previousRepresentation = inferredArguments.get(parameterKey)
              if (
                previousRepresentation !== undefined &&
                !Type.equalsGenericArgument(previousRepresentation.argument, actualRepresentation)
              ) {
                representationDiagnostic = Diagnostic.conflictingInitializerRepresentation(
                  parameter.name,
                  Type.encodeGenericArgument(previousRepresentation.argument),
                  Type.encodeGenericArgument(actualRepresentation),
                  previousRepresentation.span,
                  expressionNode.span,
                )
              } else {
                const represented = Type.represented(
                  Type.isCallable(actualValue) || Type.isEffect(actualValue)
                    ? actualValue
                    : specializedExpectedType.contract,
                  specializedExpectedType.representation.requiredBound,
                  actualRepresentation,
                )
                if (represented.representation.admissibility._tag === 'Unavailable') {
                  const requiredParameter = aggregate.typeParameters.find(
                    (candidate) => Type.key(candidate.type) === Type.key(parameter),
                  )
                  const actualParameter =
                    actualRepresentation._tag === 'RepresentationParameterArgument'
                      ? declaration.typeParameters.find(
                          (candidate) =>
                            Type.key(candidate.type) === Type.key(actualRepresentation.parameter),
                        )
                      : undefined
                  representationDiagnostic = Diagnostic.incompatibleRepresentationBound(
                    parameter.name,
                    Type.display(specializedExpectedType.representation.requiredBound),
                    Type.display(represented.contract),
                    expressionNode.span,
                    {
                      ...(requiredParameter === undefined
                        ? {}
                        : { requiredDeclarationSpan: requiredParameter.syntax.span }),
                      ...(actualParameter === undefined
                        ? {}
                        : { actualDeclarationSpan: actualParameter.syntax.span }),
                    },
                  )
                  if (previousRepresentation === undefined)
                    inferredArguments.set(
                      parameterKey,
                      Object.freeze({ argument: actualRepresentation, span: expressionNode.span }),
                    )
                } else if (previousRepresentation === undefined) {
                  inferredArguments.set(
                    parameterKey,
                    Object.freeze({ argument: actualRepresentation, span: expressionNode.span }),
                  )
                }
                if (
                  previousRepresentation === undefined ||
                  Type.equalsGenericArgument(previousRepresentation.argument, actualRepresentation)
                )
                  argumentOrigins.set(
                    parameterKey,
                    Object.freeze([
                      ...(argumentOrigins.get(parameterKey) ?? []),
                      expressionNode.span,
                    ]),
                  )
              }
            } else if (!Type.equalsGenericArgument(requiredArgument, actualRepresentation)) {
              representationDiagnostic = Diagnostic.structFieldTypeMismatch(
                name,
                Type.encodeGenericArgument(requiredArgument),
                Type.encodeGenericArgument(actualRepresentation),
                expressionNode.span,
              )
            }
          } else {
            const currentSubstitution = new Map(structSubstitution)
            for (const [parameterKey, inferred] of inferredArguments)
              currentSubstitution.set(parameterKey, inferred.argument)
            const candidateSubstitution = new Map(currentSubstitution)
            if (
              !TypeInference.infer(expectedType, expression.type, candidateSubstitution) &&
              !typesCompatible(actualValue, Type.substitute(expectedType, currentSubstitution))
            ) {
              const impliedSubstitution = new Map<string, Type.GenericArgument>()
              if (TypeInference.infer(expectedType, expression.type, impliedSubstitution)) {
                for (const parameter of aggregate.typeParameters) {
                  if (parameter.type.kind !== 'Value') continue
                  const parameterKey = Type.key(parameter.type)
                  const previous = inferredArguments.get(parameterKey)
                  const implied = impliedSubstitution.get(parameterKey)
                  if (
                    previous === undefined ||
                    implied === undefined ||
                    Type.equalsGenericArgument(previous.argument, implied)
                  )
                    continue
                  representationDiagnostic = Diagnostic.typeArgumentConflict(
                    nominalLabel,
                    parameter.type.name,
                    Type.encodeGenericArgument(previous.argument),
                    Type.encodeGenericArgument(implied),
                    expressionNode.span,
                    previous.span,
                  )
                  break
                }
              }
              const specializedExpected = Type.substitute(expectedType, currentSubstitution)
              const divergence = Type.firstRepresentationDivergence(
                specializedExpected,
                expression.type,
              )
              if (representationDiagnostic === undefined && divergence !== undefined) {
                const parameter = aggregate.typeParameters.find((candidate) => {
                  const inferred = inferredArguments.get(Type.key(candidate.type))
                  return (
                    inferred !== undefined &&
                    Type.equalsGenericArgument(inferred.argument, divergence.left)
                  )
                })
                const original =
                  parameter === undefined
                    ? undefined
                    : inferredArguments.get(Type.key(parameter.type))
                if (parameter !== undefined && original !== undefined)
                  representationDiagnostic = Diagnostic.conflictingInitializerRepresentation(
                    parameter.type.name,
                    Type.encodeGenericArgument(divergence.left),
                    Type.encodeGenericArgument(divergence.right),
                    original.span,
                    expressionNode.span,
                  )
              }
            } else {
              const siteSubstitution = new Map<string, Type.GenericArgument>()
              TypeInference.infer(expectedType, expression.type, siteSubstitution)
              for (const parameter of aggregate.typeParameters) {
                const parameterKey = Type.key(parameter.type)
                const inferred = siteSubstitution.get(parameterKey)
                if (
                  inferredArguments.get(parameterKey) === undefined &&
                  inferred !== undefined &&
                  !isOwnStructArgument(parameter.type, inferred)
                ) {
                  inferredArguments.set(
                    parameterKey,
                    Object.freeze({ argument: inferred, span: expressionNode.span }),
                  )
                  argumentOrigins.set(parameterKey, Object.freeze([expressionNode.span]))
                } else if (
                  inferred !== undefined &&
                  !isOwnStructArgument(parameter.type, inferred) &&
                  Type.equalsGenericArgument(
                    inferredArguments.get(parameterKey)?.argument ?? inferred,
                    inferred,
                  )
                ) {
                  argumentOrigins.set(
                    parameterKey,
                    Object.freeze([
                      ...(argumentOrigins.get(parameterKey) ?? []),
                      expressionNode.span,
                    ]),
                  )
                }
              }
            }
          }
          const compatibilitySubstitution = new Map(structSubstitution)
          for (const [parameterKey, inferred] of inferredArguments)
            compatibilitySubstitution.set(parameterKey, inferred.argument)
          const compatibleExpected = Type.substitute(expectedValue, compatibilitySubstitution)
          const compatibleValue = typesCompatible(actualValue, compatibleExpected)
          if (representationDiagnostic !== undefined || !compatibleValue) {
            const diagnostic =
              representationDiagnostic ??
              unionConversionDiagnostic(actualValue, compatibleExpected, expressionNode.span) ??
              Diagnostic.structFieldTypeMismatch(
                name,
                Type.display(compatibleExpected),
                Type.display(actualValue),
                expressionNode.span,
              )
            diagnostics.push(diagnostic)
            state = Object.freeze({
              _tag: 'TypeMismatch',
              field: fieldLookup.field,
              cause: Diagnostic.identity(diagnostic),
            })
          } else {
            state = Object.freeze({ _tag: 'Resolved', field: fieldLookup.field })
          }
        }
      }
      const fact: StructInitializerFact = Object.freeze({
        _tag: 'StructInitializer',
        name,
        ...(nameToken === undefined ? {} : { token: nameToken }),
        expression: expression.fact,
        state,
        syntax: initializer,
      })
      if (name !== undefined && !seen.has(name)) seen.set(name, fact)
      return fact
    },
  )

  const completedArguments =
    aggregate === undefined || nominal === undefined
      ? undefined
      : nominal.arguments.map((argument, ordinal): Type.GenericArgument => {
          const parameter = aggregate.typeParameters.at(ordinal)?.type
          if (parameter === undefined) return argument
          return inferredArguments.get(Type.key(parameter))?.argument ?? argument
        })
  let unresolvedParameters: DeclarationFacts.TypeParameterFact[]
  if (aggregate === undefined || completedArguments === undefined) {
    unresolvedParameters = []
  } else {
    unresolvedParameters = aggregate.typeParameters.flatMap((parameter, ordinal) => {
      const argument = completedArguments.at(ordinal)
      return argument !== undefined && isOwnStructArgument(parameter.type, argument)
        ? [parameter]
        : []
    })
  }
  for (const parameter of unresolvedParameters) {
    diagnostics.push(
      Diagnostic.uninferredTypeParameter(nominalLabel, parameter.type.name, parameter.syntax.span),
    )
  }
  const completedNominal =
    nominal === undefined ||
    completedArguments === undefined ||
    unresolvedParameters.length > 0 ||
    (aggregate !== undefined &&
      TypeInference.substitution(
        aggregate.typeParameters.map((parameter) => parameter.type),
        completedArguments,
      ) === undefined)
      ? undefined
      : Type.nominal(nominal.module, nominal.name, completedArguments)
  const typeArguments: ReadonlyArray<StructTypeArgumentFact> = Object.freeze(
    aggregate?.typeParameters.map((parameter, ordinal) => {
      const parameterKey = Type.key(parameter.type)
      const argument = completedArguments?.at(ordinal)
      const origins = argumentOrigins.get(parameterKey) ?? Object.freeze([])
      const unavailable = argument === undefined || isOwnStructArgument(parameter.type, argument)
      let source: StructTypeArgumentFact['source']
      if (unavailable) source = 'Unavailable'
      else if (explicitArguments.has(parameterKey)) source = 'Explicit'
      else source = 'Inferred'
      return Object.freeze({
        parameter: parameter.type,
        ...(unavailable ? {} : { argument }),
        source,
        origins,
      })
    }) ?? [],
  )
  if (aggregate !== undefined && completedNominal !== undefined) {
    for (const field of aggregateFields) {
      if (field.name._tag !== 'Present' || seen.has(field.name.spelling)) continue
      if (field.visibility === 'Private' && completedNominal.module !== source.id) continue
      diagnostics.push(
        Diagnostic.missingStructInitializer(
          Type.display(completedNominal),
          field.name.spelling,
          node.span,
        ),
      )
    }
  }

  let fields: { field: DeclarationFacts.FieldFact; initializer: StructInitializerFact }[]
  if (aggregate === undefined) {
    fields = []
  } else {
    fields = aggregateFields.flatMap((field) => {
      if (field.name._tag !== 'Present') return []
      const fieldName = field.name.spelling
      const initializer = initializers.find(
        (candidate) => candidate.name === fieldName && candidate.state._tag === 'Resolved',
      )
      return initializer === undefined ? [] : [{ field, initializer }]
    })
  }
  const complete =
    aggregate !== undefined &&
    completedNominal !== undefined &&
    authorized &&
    !forbiddenPositionalFields &&
    SyntaxTree.isAvailableSyntax(node) &&
    fields.length === aggregateFields.length &&
    initializers.length === aggregateFields.length &&
    initializers.every((initializer) => initializer.state._tag === 'Resolved')
  const type =
    complete && completedNominal !== undefined
      ? availableExpressionType(completedNominal)
      : unavailableExpressionType
  let fact: ExpressionFact
  if (unionTarget === undefined) {
    let structTarget: StructTargetFact
    if (target.fact._tag === 'Resolved' && 'struct' in target.fact) {
      structTarget =
        completedNominal === undefined
          ? target.fact
          : Object.freeze({ ...target.fact, type: completedNominal })
    } else {
      structTarget = Object.freeze({
        _tag: 'Unavailable',
        ...(target.fact._tag === 'Unavailable' && target.fact.cause !== undefined
          ? { cause: target.fact.cause }
          : {}),
      })
    }
    fact = Object.freeze({
      _tag: 'StructLiteral',
      target: structTarget,
      authorized,
      typeArguments,
      initializers: Object.freeze(initializers),
      fields: Object.freeze(fields),
      type,
      syntax: node,
    })
  } else {
    const variantTarget: UnionVariantTargetFact =
      completedNominal !== undefined && unionTarget.fact._tag === 'Resolved'
        ? Object.freeze({ ...unionTarget.fact, type: completedNominal })
        : unionTarget.fact
    fact = Object.freeze({
      _tag: 'UnionVariant',
      target: variantTarget,
      authorized,
      typeArguments,
      initializers: Object.freeze(initializers),
      fields: Object.freeze(fields),
      type,
      syntax: node,
    })
  }
  return Object.freeze({
    fact,
    diagnostics: Object.freeze(diagnostics),
    type: complete ? completedNominal : undefined,
  })
}

export const analyzeArrayLiteral = (
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
  let elementOrigin = expectedArray === undefined ? undefined : node.span
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
    if (elementType === undefined && element.type !== undefined) {
      elementType = element.type
      elementOrigin = elementNode.span
    }
    let compatibility: ArrayElementFact['compatibility']
    if (element.type === undefined || elementType === undefined) {
      compatibility = Object.freeze({ _tag: 'Unavailable' })
    } else if (!typesCompatible(element.type, elementType)) {
      const diagnostic =
        representationJoinDiagnostic(
          elementType,
          element.type,
          elementOrigin ?? node.span,
          elementNode.span,
          elementNode.span,
        ) ??
        unionConversionDiagnostic(element.type, elementType, elementNode.span) ??
        Diagnostic.arrayElementTypeMismatch(
          Type.display(elementType),
          Type.display(element.type),
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

export const analyzeIndexProjection = (
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
    'usize',
  )
  if (subject === undefined || index === undefined) {
    throw new RangeError('Cannot analyze index projection operands')
  }
  const diagnostics: Array<Diagnostic.Diagnostic> = [...subject.diagnostics, ...index.diagnostics]
  const array =
    subject.type !== undefined && Type.isFixedArray(subject.type) ? subject.type : undefined
  const slice = subject.type !== undefined && Type.isSlice(subject.type) ? subject.type : undefined
  if (subject.type !== undefined && array === undefined && slice === undefined) {
    diagnostics.push(Diagnostic.indexOnNonArray(Type.display(subject.type), subjectNode.span))
  }
  if (index.type !== undefined && index.type !== 'usize') {
    diagnostics.push(Diagnostic.indexNotUsize(Type.display(index.type), indexNode.span))
  }
  let bounds: BoundsFact = Object.freeze({ _tag: 'Unavailable' })
  if (array !== undefined && index.type === 'usize') {
    let literal: number | undefined
    if (index.fact._tag === 'Integer' && index.fact.integer._tag === 'Available') {
      if (index.fact.integer.value <= BigInt(Number.MAX_SAFE_INTEGER)) {
        literal = Number(index.fact.integer.value)
      } else {
        literal = Number.POSITIVE_INFINITY
      }
    } else {
      literal = undefined
    }
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
  } else if (slice !== undefined && index.type === 'usize') {
    bounds = Object.freeze({ _tag: 'RuntimeSlice' })
  }
  const available =
    (array !== undefined || slice !== undefined) &&
    index.type === 'usize' &&
    bounds._tag !== 'Invalid' &&
    bounds._tag !== 'Unavailable' &&
    SyntaxTree.isAvailableSyntax(node)
  const element = array?.element ?? slice?.element
  const type =
    available && element !== undefined
      ? availableExpressionType(element)
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'IndexProjection',
      subject: subject.fact,
      index: index.fact,
      ...(array === undefined ? {} : { array, elementType: array.element }),
      ...(slice === undefined
        ? {}
        : { slice, elementType: slice.element, borrowAccess: slice.access }),
      access: 'CopyRead',
      bounds,
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

export const analyzeProjection = (
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
  const ordinalToken = directToken(node, 'DecimalInteger')
  const ordinal =
    ordinalToken === undefined ? undefined : Number.parseInt(spelling(source, ordinalToken), 10)
  const labeledToken = directToken(node, 'Identifier')
  const fieldToken = ordinalToken ?? labeledToken
  let fieldName: string | undefined
  if (ordinal !== undefined) fieldName = `${ordinal}`
  else if (labeledToken !== undefined) fieldName = spelling(source, labeledToken)
  // A reference projects the fields of its target: the read happens through the borrow, so
  // the projected value is typed by the target while consumption stays a partial-move error.
  const reference =
    subject.type !== undefined &&
    Type.isReference(subject.type) &&
    Type.isNominal(subject.type.target)
      ? subject.type
      : undefined
  let nominal: Type.Nominal | undefined
  if (subject.type !== undefined && Type.isNominal(subject.type)) {
    nominal = subject.type
  } else if (reference !== undefined && Type.isNominal(reference.target)) {
    nominal = reference.target
  } else {
    nominal = undefined
  }
  const slice = subject.type !== undefined && Type.isSlice(subject.type) ? subject.type : undefined
  const borrowAccess =
    subject.fact._tag === 'IndexProjection' || subject.fact._tag === 'FieldProjection'
      ? subject.fact.borrowAccess
      : undefined
  let state: ProjectionState = Object.freeze({ _tag: 'Unavailable' })
  let type: SemanticType | undefined
  if (slice !== undefined && fieldName === 'length') {
    state = Object.freeze({ _tag: 'SliceLength' })
    type = 'usize'
  } else if (slice !== undefined && fieldName !== undefined && fieldToken !== undefined) {
    const diagnostic = Diagnostic.unknownProjectedField(
      Type.display(slice),
      fieldName,
      fieldToken.span,
    )
    diagnostics.push(diagnostic)
    state = Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) })
  } else if (subject.type !== undefined && nominal === undefined && fieldToken !== undefined) {
    const diagnostic = Diagnostic.projectionOnNonStruct(Type.display(subject.type), fieldToken.span)
    diagnostics.push(diagnostic)
    state = Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) })
  } else if (nominal !== undefined && fieldName !== undefined && fieldToken !== undefined) {
    const struct = aggregateByNominal(resolution, nominal)
    const lookup =
      struct === undefined
        ? undefined
        : DeclarationFacts.lookupAggregateMember(
            struct.fields,
            ordinal === undefined
              ? AggregateIdentity.labeled(fieldName)
              : AggregateIdentity.ordinal(ordinal),
          )
    if (lookup?._tag !== 'Resolved') {
      // A receiver method named through a value binds that value as parameter zero, unless a
      // borrowed receiver is a temporary the section would outlive.
      const candidate = resolveMethodCandidate(
        nominal,
        fieldName,
        fieldToken,
        declaration,
        resolution,
      )
      const temporaryReceiver =
        candidate._tag === 'Inherent' && borrowsTemporary(candidate.declaration, subject)
      if (candidate._tag === 'Inherent' && !temporaryReceiver)
        return finishBoundMethod(
          source,
          node,
          subjectNode,
          subject,
          candidate,
          fieldName,
          fieldToken,
          declaration,
          resolution,
        )
      let diagnostic: Diagnostic.Diagnostic
      if (temporaryReceiver) {
        diagnostic = Diagnostic.invalidBorrowOperand(subjectNode.span)
      } else if (candidate._tag === 'NoReceiver') {
        diagnostic = Diagnostic.associatedFunctionOnValue(
          candidate.ownerSpelling,
          fieldName,
          fieldToken.span,
        )
      } else if (candidate._tag === 'Unavailable' && candidate.diagnostic !== undefined) {
        diagnostic = candidate.diagnostic
      } else if (candidate._tag === 'Conformance' || candidate._tag === 'AmbiguousConformance') {
        // A supplied operation is a real member that this spelling cannot bind: a first-class value
        // would have to carry its witness, which the qualified call does not need either.
        diagnostic = Diagnostic.suppliedOperationValue(
          Type.display(nominal),
          fieldName,
          fieldToken.span,
        )
      } else {
        diagnostic = Diagnostic.unknownProjectedField(
          Type.display(nominal),
          fieldName,
          fieldToken.span,
        )
      }
      diagnostics.push(diagnostic)
      state = Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) })
    } else if (lookup.field.visibility === 'Private' && nominal.module !== source.id) {
      const diagnostic = Diagnostic.inaccessibleProjectedField(
        Type.display(nominal),
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
          : (TypeInference.substitution(
              struct.typeParameters.map((parameter) => parameter.type),
              nominal.arguments,
            ) ?? new Map())
      type = Type.substitute(lookup.field.declaredType.type, substitution)
    }
  }
  const typeFact = type === undefined ? unavailableExpressionType : availableExpressionType(type)
  const projectionAccess = borrowAccess ?? reference?.access
  const projection: FieldProjectionExpressionFact = Object.freeze({
    _tag: 'FieldProjection',
    subject: subject.fact,
    ...(nominal === undefined ? {} : { nominal }),
    ...(projectionAccess === undefined ? {} : { borrowAccess: projectionAccess }),
    fieldName,
    ...(fieldToken === undefined ? {} : { fieldToken }),
    state,
    type: typeFact,
    syntax: node,
  })
  const evaluated =
    resolution.staticContext === undefined
      ? undefined
      : StaticEvaluation.evaluateFact(projection, resolution.staticContext)
  const fact: FieldProjectionExpressionFact =
    evaluated?._tag === 'Complete'
      ? Object.freeze({ ...projection, staticValue: evaluated.value })
      : projection
  return Object.freeze({
    fact,
    diagnostics: Object.freeze(diagnostics),
    type,
  })
}

export const analyzeReferentProjection = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const subjectNode = node.children.find(isExpressionNode)
  if (subjectNode === undefined)
    throw new RangeError('Referent projection requires a subject expression')
  const subject = analyzeExpression(
    source,
    subjectNode,
    declarations,
    declaration,
    scope,
    resolution,
  )
  if (subject === undefined)
    throw new RangeError(`Cannot analyze referent projection ${subjectNode.kind}`)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...subject.diagnostics]
  const reference =
    subject.type !== undefined && Type.isReference(subject.type) ? subject.type : undefined
  let state: ReferentProjectionState
  if (reference === undefined && subject.type !== undefined) {
    const star = directToken(node, 'Star')
    const diagnostic = Diagnostic.invalidReferentProjection(
      Type.encode(subject.type),
      star?.span ?? node.span,
    )
    diagnostics.push(diagnostic)
    state = Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) })
  } else if (reference === undefined) {
    state = Object.freeze({ _tag: 'Unavailable' })
  } else {
    state = Object.freeze({ _tag: 'Resolved', reference })
  }
  const type =
    reference === undefined ? unavailableExpressionType : availableExpressionType(reference.target)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'ReferentProjection',
      subject: subject.fact,
      ...(reference === undefined ? {} : { reference, borrowAccess: reference.access }),
      state,
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

import type { CallTypeArgumentsResult } from './CallResolution.js'
import {
  analyzeArgumentNodes,
  analyzeArguments,
  analyzeCallContract,
  analyzeCallTypeArguments,
  appliedOwnerTypeArgumentNodes,
  analyzeFunctionItem,
  boundOperationReference,
  builtinSignature,
  callArityDiagnostic,
  captureAccess,
  copyAssumptionsOf,
  executableSite,
  executableSpecializationOwner,
  finishCallableApplication,
  finishCallableSection,
  genericArgumentOfTypeArgument,
  hasAvailableCallSyntax,
  interfaceConstraintDiagnostics,
  interfaceOperationContract,
  isSectionArity,
  ownedProviderCaptureAccess,
  serviceOperation,
  sourceCallable,
  unavailableIdentifierFact,
} from './CallResolution.js'
export function analyzePlaceReplace(
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult {
  const argumentList = SyntaxTree.directNode(call, 'ArgumentList')
  const nodes =
    argumentList === undefined ? [] : argumentList.children.filter(isRecursiveArgumentNode)
  const destinationNode = nodes.at(0)
  const valueNode = nodes.at(1)
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  if (destinationNode === undefined || valueNode === undefined || nodes.length !== 2) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'PlaceReplace' as const,
        reference: intrinsicReference(source, call),
        destination: unavailableIdentifierFact(call),
        value: unavailableIdentifierFact(call),
        compatible: false,
        type: unavailableExpressionType,
        syntax: call,
      }),
      diagnostics: Object.freeze([
        Diagnostic.wrongCallArity(
          Object.freeze({ _tag: 'BuiltinTarget', actor: 'Place', operation: 'replace' }),
          2,
          nodes.length,
          call.span,
        ),
      ]),
      type: undefined,
    })
  }
  const destination = analyzeExpression(
    source,
    destinationNode,
    declarations,
    declaration,
    scope,
    resolution,
  )
  if (destination === undefined) {
    throw new RangeError(`Semantic analysis cannot analyze ${destinationNode.kind}`)
  }
  diagnostics.push(...destination.diagnostics)
  const value = analyzeExpression(
    source,
    valueNode,
    declarations,
    declaration,
    scope,
    resolution,
    destination.type,
  )
  if (value === undefined) {
    throw new RangeError(`Semantic analysis cannot analyze ${valueNode.kind}`)
  }
  diagnostics.push(...value.diagnostics)
  const root = assignmentRoot(destination.fact)
  if (root === undefined) {
    if (SyntaxTree.isAvailableSyntax(destinationNode) && destination.diagnostics.length === 0) {
      diagnostics.push(Diagnostic.invalidAssignmentPlace(destinationNode.span))
    }
  } else if (assignmentRootAccess(root) === 'ImmutableOwned') {
    diagnostics.push(
      Diagnostic.immutableAssignment(
        root.name._tag === 'Present' ? root.name.spelling : '?',
        destinationNode.span,
      ),
    )
  } else if (
    assignmentRootAccess(root) === 'SharedBorrowed' ||
    (assignmentRootAccess(root) === 'ExclusiveBorrowed' &&
      destination.fact._tag !== 'IndexProjection' &&
      destination.fact._tag !== 'ReferentProjection' &&
      destination.fact._tag !== 'FieldProjection')
  ) {
    diagnostics.push(Diagnostic.invalidAssignmentPlace(destinationNode.span))
  }
  const compatible =
    destination.type !== undefined &&
    value.type !== undefined &&
    typesCompatible(value.type, destination.type)
  if (destination.type !== undefined && value.type !== undefined && !compatible) {
    const expectedOrigin =
      root?._tag === 'BindingFact' ? root.initializer.syntax.span : destinationNode.span
    diagnostics.push(
      representationJoinDiagnostic(
        destination.type,
        value.type,
        expectedOrigin,
        valueNode.span,
        valueNode.span,
      ) ??
        unionConversionDiagnostic(value.type, destination.type, valueNode.span) ??
        Diagnostic.assignmentTypeMismatch(
          Type.display(destination.type),
          Type.display(value.type),
          valueNode.span,
        ),
    )
  }
  if (
    root?._tag === 'BindingFact' &&
    root.inferredType._tag === 'Available' &&
    Type.isCallable(root.inferredType.type)
  ) {
    resolution.writtenCallableBindings?.add(root.id.ordinal)
  }
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'PlaceReplace' as const,
      reference: intrinsicReference(source, call),
      destination: destination.fact,
      ...(root === undefined ? {} : { root }),
      value: value.fact,
      compatible,
      type:
        destination.type === undefined
          ? unavailableExpressionType
          : Object.freeze({ _tag: 'Available' as const, type: destination.type }),
      syntax: call,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: destination.type,
  })
}

export const directProviderReference = (
  expression: ExpressionFact,
): BindingDeclarationFact | ParameterFact | undefined => {
  if (expression._tag === 'Identifier') {
    if (expression.reference._tag === 'ResolvedBinding') return expression.reference.binding
    if (expression.reference._tag === 'Resolved') return expression.reference.parameter
    return undefined
  }
  if (expression._tag === 'Borrow' || expression._tag === 'Move')
    return directProviderReference(expression.subject)
  if (expression._tag === 'Grouped') return directProviderReference(expression.expression)
  return undefined
}

export const selectedRequirementShape = (
  row: Type.RequirementsRow,
): { readonly capability: Type.Nominal | Type.Parameter; readonly role: string } | undefined => {
  const concrete = RowAlgebra.concretize(Type.requirementRowPolicy(), row)
  if (concrete._tag === 'Concrete') {
    const selected = concrete.row.members.at(0)
    return concrete.row.members.length === 1 && selected !== undefined
      ? Object.freeze({ capability: selected.capability, role: selected.role })
      : undefined
  }
  return row.expression._tag === 'Singleton'
    ? Object.freeze({
        capability: row.expression.member.capability,
        role: row.expression.member.role,
      })
    : undefined
}

export const intrinsicContractReference = (
  operation: Intrinsic.Operation,
  operationToken: Token.Token,
): Extract<CallReferenceFact, { readonly _tag: 'ResolvedIntrinsicContract' }> => {
  if (
    operation.rule._tag !== 'ContractRule' &&
    operation.rule._tag !== 'StaticOnlyRule' &&
    operation.rule._tag !== 'MixedFieldProjectionRule'
  )
    throw new RangeError('intrinsic contract reference requires a contract operation')
  const contract =
    operation.rule.contract.unsafe === operation.unsafe
      ? operation.rule.contract
      : CallableContract.make({
          functionKind: operation.rule.contract.functionKind,
          unsafe: operation.unsafe,
          binders: operation.rule.contract.binders,
          parameters: operation.rule.contract.parameters,
          result: operation.rule.contract.result,
          constraints: operation.rule.contract.constraints,
          captures: operation.rule.contract.captures,
        })
  return Object.freeze({
    _tag: 'ResolvedIntrinsicContract',
    spelling: `Intrinsic.${operation.spelling}`,
    token: operationToken,
    intrinsic: operation,
    contract,
  })
}

export const effectBindingProvider = (
  operation: Intrinsic.Operation,
  substitution: Type.Substitution,
  evidence: ReadonlyArray<Constraint.ConstraintEvidence>,
  provider: ExpressionFact,
  span: SourceSpan.SourceSpan,
  index?: DeclarationIndex.Index,
): EffectRequirementBindingFact | undefined => {
  if (
    operation.rule._tag !== 'ContractRule' ||
    operation.rule.post !== 'BindRequirement' ||
    operation.rule.providerMode === undefined
  )
    return undefined
  const wanted = operation.rule.contract.constraints
    .map((constraint) => Constraint.substitute(constraint, substitution))
    .find(
      (constraint): constraint is Constraint.ProviderSelection =>
        constraint._tag === 'ProviderSelectionConstraint',
    )
  if (wanted === undefined) return undefined
  const providerReference = directProviderReference(provider)
  if (
    providerReference === undefined ||
    !(Type.isNominal(wanted.provider) || Type.isParameter(wanted.provider))
  )
    return undefined
  const wantedKey = Constraint.key(wanted)
  const proof = evidence.find(
    (candidate) =>
      (candidate._tag === 'Assumed' || candidate._tag === 'RequirementSelection') &&
      candidate.wantedKey === wantedKey,
  )
  if (proof === undefined) return undefined
  const selected = selectedRequirementShape(wanted.selected)
  return Object.freeze({
    _tag: 'EffectRequirementBinding',
    reference: providerReference,
    selected: wanted.selected,
    evidence,
    ...(selected === undefined ? {} : { capability: selected.capability }),
    providerType: wanted.provider,
    ...(selected === undefined ? {} : { role: selected.role }),
    selectionAccess: operation.rule.providerMode,
    captureAccess:
      provider._tag === 'Move' &&
      provider.subject.type._tag === 'Available' &&
      index !== undefined &&
      ConformanceProof.copyType(index, provider.subject.type.type)
        ? 'Copy'
        : captureAccess(provider, index),
    span,
  })
}

export const sectionIntrinsicReference = (
  section: CallableSectionExpressionFact,
): IntrinsicReferenceFact => {
  if (section.reference._tag !== 'ResolvedIntrinsicContract')
    return Object.freeze({ _tag: 'UnavailableIntrinsicReference', syntax: section.syntax })
  const actor = Intrinsic.findActor('Intrinsic')
  const actorToken = section.path._tag === 'ReferencePath' ? section.path.qualifier : undefined
  if (actor === undefined || actorToken === undefined)
    return Object.freeze({ _tag: 'UnavailableIntrinsicReference', syntax: section.syntax })
  return Object.freeze({
    _tag: 'ResolvedIntrinsicReference',
    actor,
    operation: section.reference.intrinsic,
    actorToken,
    operationToken: section.reference.token,
  })
}

const isDeferredStaticExpression = (expression: ExpressionFact): boolean => {
  if (expression._tag === 'Grouped') return isDeferredStaticExpression(expression.expression)
  if (expression._tag === 'Move') return isDeferredStaticExpression(expression.subject)
  if (expression._tag === 'Constant') return true
  if (expression._tag === 'Identifier') {
    if (expression.reference._tag === 'Resolved')
      return expression.reference.parameter.phase === 'Static'
    if (expression.reference._tag === 'ResolvedBinding')
      return expression.reference.binding.phase === 'Static'
    return false
  }
  if (expression._tag !== 'Call') return false
  if (expression.staticValue !== undefined) return true
  if (expression.reference._tag === 'Resolved')
    return expression.reference.declaration.phase === 'Static'
  return (
    expression.reference._tag === 'ResolvedIntrinsicContract' &&
    expression.reference.intrinsic.phase === 'StaticOnly'
  )
}

const reflectedFieldName = (descriptor: StaticValue.FieldDescriptorValue): string =>
  descriptor.member._tag === 'LabeledField'
    ? descriptor.member.label
    : `${descriptor.member.ordinal}`

const sameReflectedMember = (
  descriptor: StaticValue.FieldDescriptorValue,
  field: DeclarationFacts.FieldFact,
): boolean =>
  descriptor.member._tag === 'LabeledField'
    ? field.member._tag === 'LabeledAggregateMember' &&
      field.member.label === descriptor.member.label
    : field.member._tag === 'OrdinalAggregateMember' &&
      field.member.ordinal === descriptor.member.ordinal

const appendFieldBorrowRoot = (
  root: BorrowRootFact,
  field: DeclarationFacts.FieldFact,
  span: SourceSpan.SourceSpan,
): BorrowRootFact =>
  Object.freeze({
    ...root,
    path: Object.freeze([
      ...root.path,
      Object.freeze({ _tag: 'Field' as const, field: field.id, span }),
    ]),
  })

const mixedFieldProjection = (
  call: SyntaxTree.Node,
  operation: Intrinsic.Operation,
  reference: Extract<CallReferenceFact, { readonly _tag: 'ResolvedIntrinsicContract' }>,
  argumentsResult: ArgumentsResult,
  typeArguments: CallTypeArgumentsResult,
  analyzed: ReturnType<typeof analyzeCallContract>,
  resolution: ResolutionContext,
  caller: DeclarationFact,
): ExpressionResult => {
  if (operation.rule._tag !== 'MixedFieldProjectionRule')
    throw new RangeError('mixed field projection requires its sealed intrinsic rule')
  const commonDiagnostics = [
    ...argumentsResult.diagnostics,
    ...typeArguments.diagnostics,
    ...analyzed.diagnostics,
  ]
  const substitution =
    analyzed.fact._tag === 'Compatible'
      ? analyzed.fact.substitution
      : new Map<string, Type.GenericArgument>()
  const result = Type.substitute(operation.rule.contract.result, substitution)
  const ownerArgument = argumentsResult.facts.at(operation.rule.runtimeOwnerParameter)
  const descriptorArgument = argumentsResult.facts.at(operation.rule.staticDescriptorParameter)
  const ownerType = ownerArgument?.type._tag === 'Available' ? ownerArgument.type.type : undefined
  const ownerParameter = operation.rule.contract.parameters.at(operation.rule.runtimeOwnerParameter)
  const requiredOwnerType =
    ownerParameter === undefined ? undefined : Type.substitute(ownerParameter.type, substitution)
  const sharedOwnerAvailable =
    ownerType !== undefined && Type.isReference(ownerType) && ownerType.access === 'Shared'
  const baseAvailable =
    analyzed.fact._tag === 'Compatible' &&
    ownerArgument !== undefined &&
    descriptorArgument !== undefined &&
    sharedOwnerAvailable &&
    Type.isReference(result) &&
    result.access === 'Shared'
  const callFact = (type: ExpressionTypeFact): ExpressionFact =>
    Object.freeze({
      _tag: 'Call',
      reference,
      path: referencePath(call),
      typeArguments: typeArguments.facts,
      arguments: argumentsResult.facts,
      mappings: analyzed.mappings,
      contract: analyzed.fact,
      type,
      syntax: call,
    })
  if (caller.phase === 'Static') {
    const diagnostic = Diagnostic.staticPhaseViolation(
      'Intrinsic.borrowField',
      resolution.staticContext?.environment.target ?? 'unselected-target',
      Object.freeze([]),
      call.span,
    )
    return Object.freeze({
      fact: callFact(unavailableExpressionType),
      diagnostics: Object.freeze([...commonDiagnostics, diagnostic]),
      type: undefined,
    })
  }
  if (
    analyzed.fact._tag === 'Compatible' &&
    ownerArgument !== undefined &&
    descriptorArgument !== undefined &&
    !sharedOwnerAvailable
  ) {
    const diagnostic = Diagnostic.argumentTypeMismatch(
      requiredOwnerType === undefined ? '&Owner' : Type.display(requiredOwnerType),
      ownerType === undefined ? '<unavailable>' : Type.display(ownerType),
      ownerArgument.syntax.span,
    )
    return Object.freeze({
      fact: callFact(unavailableExpressionType),
      diagnostics: Object.freeze([...commonDiagnostics, diagnostic]),
      type: undefined,
    })
  }
  if (!baseAvailable || ownerArgument === undefined || descriptorArgument === undefined) {
    return Object.freeze({
      fact: callFact(unavailableExpressionType),
      diagnostics: Object.freeze(commonDiagnostics),
      type: undefined,
    })
  }
  if (resolution.staticContext === undefined) {
    const deferred = isDeferredStaticExpression(descriptorArgument.expression)
    const diagnostic = deferred
      ? undefined
      : Diagnostic.staticPhaseViolation(
          'Intrinsic.borrowField descriptor',
          'unselected-target',
          Object.freeze([]),
          descriptorArgument.syntax.span,
        )
    const type =
      diagnostic === undefined ? availableExpressionType(result) : unavailableExpressionType
    return Object.freeze({
      fact: callFact(type),
      diagnostics: Object.freeze([
        ...commonDiagnostics,
        ...(diagnostic === undefined ? [] : [diagnostic]),
      ]),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }
  const evaluated = StaticEvaluation.evaluateFact(
    descriptorArgument.expression,
    resolution.staticContext,
  )
  if (evaluated._tag === 'Failed' || evaluated.value._tag !== 'FieldDescriptorValue') {
    const diagnostic =
      evaluated._tag === 'Failed'
        ? StaticEvaluation.diagnostic(
            evaluated.failure,
            resolution.staticContext.environment.target,
          )
        : Diagnostic.staticPhaseViolation(
            'Intrinsic.borrowField descriptor',
            resolution.staticContext.environment.target,
            Object.freeze([]),
            descriptorArgument.syntax.span,
          )
    return Object.freeze({
      fact: callFact(unavailableExpressionType),
      diagnostics: Object.freeze([...commonDiagnostics, diagnostic]),
      type: undefined,
    })
  }
  const descriptor = evaluated.value
  const ownerBinder = operation.rule.contract.binders.at(0)
  const valueBinder = operation.rule.contract.binders.at(1)
  const expectedOwner =
    ownerBinder === undefined ? undefined : substitution.get(Type.key(ownerBinder))
  const expectedValue =
    valueBinder === undefined ? undefined : substitution.get(Type.key(valueBinder))
  if (
    expectedOwner === undefined ||
    !Type.isTypeArgument(expectedOwner) ||
    expectedValue === undefined ||
    !Type.isTypeArgument(expectedValue) ||
    !Type.equals(expectedOwner, descriptor.owner.owner) ||
    !Type.equals(expectedValue, descriptor.valueType)
  ) {
    const diagnostic = Diagnostic.argumentTypeMismatch(
      expectedOwner !== undefined &&
        Type.isTypeArgument(expectedOwner) &&
        expectedValue !== undefined &&
        Type.isTypeArgument(expectedValue)
        ? Type.display(Type.fieldDescriptor(expectedOwner, expectedValue))
        : 'Field<Owner, Value>',
      Type.display(StaticValue.fieldDescriptorType(descriptor)),
      descriptorArgument.syntax.span,
    )
    return Object.freeze({
      fact: callFact(unavailableExpressionType),
      diagnostics: Object.freeze([...commonDiagnostics, diagnostic]),
      type: undefined,
    })
  }
  const owner = descriptor.owner.owner
  const aggregate = Type.isNominal(owner) ? aggregateByNominal(resolution, owner) : undefined
  const field = aggregate?.fields.find(
    (candidate) => candidate.id.ordinal === descriptor.declarationOrdinal,
  )
  const name = reflectedFieldName(descriptor)
  if (
    aggregate === undefined ||
    field === undefined ||
    aggregate.aggregateKind !== descriptor.owner.kind ||
    !sameReflectedMember(descriptor, field) ||
    field.declaredType._tag !== 'Resolved'
  ) {
    const diagnostic = Diagnostic.unknownProjectedField(
      Type.display(owner),
      name,
      descriptorArgument.syntax.span,
    )
    return Object.freeze({
      fact: callFact(unavailableExpressionType),
      diagnostics: Object.freeze([...commonDiagnostics, diagnostic]),
      type: undefined,
    })
  }
  const aggregateSubstitution =
    TypeInference.substitution(
      aggregate.typeParameters.map((parameter) => parameter.type),
      owner.arguments,
    ) ?? new Map<string, Type.GenericArgument>()
  const declaredValue = Type.substitute(field.declaredType.type, aggregateSubstitution)
  const authorization = DeclarationFacts.byCanonical(resolution.index, descriptor.authorization)
  const aggregateIdentity =
    aggregate.canonical._tag === 'Canonical' ? aggregate.canonical.id : undefined
  const authorized =
    authorization?.canonical._tag === 'Canonical' &&
    aggregateIdentity !== undefined &&
    (field.visibility !== 'Private' || descriptor.authorization.module === aggregateIdentity.module)
  if (!authorized) {
    const diagnostic = Diagnostic.inaccessibleProjectedField(
      Type.display(owner),
      name,
      descriptorArgument.syntax.span,
    )
    return Object.freeze({
      fact: callFact(unavailableExpressionType),
      diagnostics: Object.freeze([...commonDiagnostics, diagnostic]),
      type: undefined,
    })
  }
  if (!Type.equals(declaredValue, descriptor.valueType)) {
    const diagnostic = Diagnostic.argumentTypeMismatch(
      Type.display(Type.fieldDescriptor(owner, declaredValue)),
      Type.display(StaticValue.fieldDescriptorType(descriptor)),
      descriptorArgument.syntax.span,
    )
    return Object.freeze({
      fact: callFact(unavailableExpressionType),
      diagnostics: Object.freeze([...commonDiagnostics, diagnostic]),
      type: undefined,
    })
  }
  const projection: FieldProjectionExpressionFact = Object.freeze({
    _tag: 'FieldProjection',
    subject: ownerArgument.expression,
    nominal: owner,
    borrowAccess: 'Shared',
    fieldName: name,
    state: Object.freeze({ _tag: 'Resolved', field }),
    type: availableExpressionType(descriptor.valueType),
    syntax: call,
  })
  const directRoot = borrowRoot(projection)
  const borrowedRoot =
    ownerArgument.expression._tag === 'Borrow' &&
    ownerArgument.expression.formation._tag !== 'Unavailable'
      ? appendFieldBorrowRoot(ownerArgument.expression.formation.root, field, call.span)
      : undefined
  const root = directRoot ?? borrowedRoot
  if (root === undefined) {
    const diagnostic = Diagnostic.invalidBorrowOperand(ownerArgument.syntax.span)
    return Object.freeze({
      fact: callFact(unavailableExpressionType),
      diagnostics: Object.freeze([...commonDiagnostics, diagnostic]),
      type: undefined,
    })
  }
  const type = Type.reference('Shared', descriptor.valueType)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Borrow',
      access: 'Shared',
      subject: projection,
      formation: Object.freeze({
        _tag: 'ValueBorrow',
        root,
        source: descriptor.valueType,
      }),
      type: availableExpressionType(type),
      syntax: call,
    }),
    diagnostics: Object.freeze(commonDiagnostics),
    type,
  })
}

export const finishIntrinsicContractCall = (
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  operation: Intrinsic.Operation,
  operationToken: Token.Token,
  argumentsResult: ArgumentsResult,
  typeArguments: CallTypeArgumentsResult,
  resolution: ResolutionContext,
  caller: DeclarationFact,
): ExpressionResult => {
  if (
    operation.rule._tag !== 'ContractRule' &&
    operation.rule._tag !== 'StaticOnlyRule' &&
    operation.rule._tag !== 'MixedFieldProjectionRule'
  )
    throw new RangeError('intrinsic contract finisher received a non-contract operation')
  const reference = intrinsicContractReference(operation, operationToken)
  const analyzed = analyzeCallContract(
    call,
    reference,
    argumentsResult.facts,
    hasAvailableCallSyntax(call),
    typeArguments,
    resolution,
    caller,
  )
  const unsafeDiagnostic = unsafeCallDiagnostic(
    operation.unsafe,
    reference.spelling,
    call,
    resolution,
  )
  const substitution =
    analyzed.fact._tag === 'Compatible'
      ? analyzed.fact.substitution
      : new Map<string, Type.GenericArgument>()
  if (operation.rule._tag === 'MixedFieldProjectionRule')
    return mixedFieldProjection(
      call,
      operation,
      reference,
      argumentsResult,
      typeArguments,
      analyzed,
      resolution,
      caller,
    )
  if (operation.rule._tag === 'StaticOnlyRule') {
    const phaseDiagnostic =
      caller.phase === 'Static' || resolution.staticContext !== undefined
        ? undefined
        : Diagnostic.staticPhaseViolation(
            `Intrinsic.${operation.spelling}`,
            'unselected-target',
            Object.freeze([]),
            call.span,
          )
    const result = Type.substitute(operation.rule.contract.result, substitution)
    const type =
      analyzed.fact._tag === 'Compatible' && phaseDiagnostic === undefined
        ? availableExpressionType(result)
        : unavailableExpressionType
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Call',
        reference,
        path: referencePath(call),
        typeArguments: typeArguments.facts,
        arguments: argumentsResult.facts,
        mappings: analyzed.mappings,
        contract: analyzed.fact,
        type,
        syntax: call,
      }),
      diagnostics: Object.freeze([
        ...argumentsResult.diagnostics,
        ...typeArguments.diagnostics,
        ...analyzed.diagnostics,
        ...(phaseDiagnostic === undefined ? [] : [phaseDiagnostic]),
      ]),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }
  const substitutedResult = Type.substitute(operation.rule.contract.result, substitution)
  const type =
    analyzed.fact._tag === 'Compatible' &&
    unsafeDiagnostic === undefined &&
    Type.isEffect(substitutedResult)
      ? availableExpressionType(
          Type.effectWithRows(
            substitutedResult.success,
            substitutedResult.failureRow,
            intrinsicEffectCaptureAccess(
              operation,
              argumentsResult.facts,
              resolution.index,
              copyAssumptionsOf(caller),
            ),
            substitutedResult.requirementRow,
          ),
        )
      : unavailableExpressionType
  const protected_ = argumentsResult.facts.at(0)
  const evidence = analyzed.fact._tag === 'Compatible' ? analyzed.fact.evidence : Object.freeze([])
  if (operation.rule.post === 'CatchFailure') {
    const handler = argumentsResult.facts.at(1)
    const wanted = operation.rule.contract.constraints
      .map((constraint) => Constraint.substitute(constraint, substitution))
      .find(
        (constraint): constraint is Constraint.FailureSubset =>
          constraint._tag === 'FailureSubsetConstraint',
      )
    const wantedKey = wanted === undefined ? undefined : Constraint.key(wanted)
    const proved =
      wantedKey !== undefined &&
      evidence.some(
        (candidate) =>
          (candidate._tag === 'Assumed' && candidate.wantedKey === wantedKey) ||
          (candidate._tag === 'FailureSubset' &&
            wanted !== undefined &&
            RowAlgebra.equals(Type.failureRowPolicy(), candidate.selected, wanted.selected) &&
            RowAlgebra.equals(Type.failureRowPolicy(), candidate.source, wanted.source)),
      )
    const handlerType = handler?.type._tag === 'Available' ? handler.type.type : undefined
    const handlerEffect =
      handlerType !== undefined && Type.isCallable(handlerType) && Type.isEffect(handlerType.result)
        ? handlerType.result
        : undefined
    const catchAvailable =
      type._tag === 'Available' &&
      protected_ !== undefined &&
      handler !== undefined &&
      wanted !== undefined &&
      proved
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'EffectCatch',
        reference: intrinsicReference(source, call),
        protected: protected_?.expression ?? unavailableExpression(call),
        handler: handler?.expression ?? unavailableExpression(call),
        ...(wanted === undefined ? {} : { selected: Type.failureType(wanted.selected) }),
        protectedRow: wanted?.source ?? RowAlgebra.concrete(Type.failureRowPolicy(), []),
        handlerRow: handlerEffect?.failureRow ?? RowAlgebra.concrete(Type.failureRowPolicy(), []),
        residualRow:
          wanted === undefined
            ? RowAlgebra.concrete(Type.failureRowPolicy(), [])
            : RowAlgebra.without(Type.failureRowPolicy(), wanted.source, wanted.selected),
        evidence,
        type: catchAvailable ? type : unavailableExpressionType,
        syntax: call,
      }),
      diagnostics: Object.freeze([
        ...argumentsResult.diagnostics,
        ...typeArguments.diagnostics,
        ...analyzed.diagnostics,
        ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
      ]),
      type: catchAvailable && type._tag === 'Available' ? type.type : undefined,
    })
  }
  const provider = argumentsResult.facts.at(1)
  const binding =
    provider === undefined
      ? undefined
      : effectBindingProvider(
          operation,
          substitution,
          evidence,
          provider.expression,
          provider.syntax.span,
          resolution.index,
        )
  const bindingAvailable =
    type._tag === 'Available' && protected_ !== undefined && binding !== undefined
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'EffectBindRequirement',
      reference: intrinsicReference(source, call),
      protected: protected_?.expression ?? unavailableExpression(call),
      ...(bindingAvailable
        ? {
            provider: binding,
          }
        : {}),
      type,
      syntax: call,
    }),
    diagnostics: Object.freeze([
      ...argumentsResult.diagnostics,
      ...typeArguments.diagnostics,
      ...analyzed.diagnostics,
      ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
    ]),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

export function analyzeBuiltinCall(
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  argumentsResult: ArgumentsResult,
  typeArguments: CallTypeArgumentsResult,
  resolution: ResolutionContext,
  caller: DeclarationFact,
): ExpressionResult {
  const identifiers = callReferenceTokens(call)
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
        path: referencePath(call),
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
  const actor = Intrinsic.findActor(actorSpelling)
  const operation = Intrinsic.findOperation(actorSpelling, operationSpelling)
  if (
    (operation?.rule._tag === 'ContractRule' ||
      operation?.rule._tag === 'StaticOnlyRule' ||
      operation?.rule._tag === 'MixedFieldProjectionRule') &&
    isSectionArity(operation.rule.contract.parameters.length, argumentsResult.facts.length)
  )
    return finishCallableSection(
      call,
      intrinsicContractReference(operation, operationToken),
      argumentsResult,
      typeArguments,
      resolution,
      caller,
    )
  if (
    operation?.rule._tag === 'ContractRule' ||
    operation?.rule._tag === 'StaticOnlyRule' ||
    operation?.rule._tag === 'MixedFieldProjectionRule'
  )
    return finishIntrinsicContractCall(
      source,
      call,
      operation,
      operationToken,
      argumentsResult,
      typeArguments,
      resolution,
      caller,
    )
  const signature = builtinSignature(actorSpelling, operationSpelling)
  const declaredTypeParameters = signature?.typeParameters ?? Object.freeze([])
  const specializationDiagnostic =
    typeArguments.explicit &&
    (typeArguments.types === undefined ||
      typeArguments.types.length !== declaredTypeParameters.length)
      ? Diagnostic.typeArgumentArity(
          `${actorSpelling}.${operationSpelling}`,
          declaredTypeParameters.length,
          typeArguments.types?.length ?? 0,
          call.span,
        )
      : undefined
  const substitution = new Map<string, Type.GenericArgument>()
  if (
    typeArguments.explicit &&
    typeArguments.types !== undefined &&
    typeArguments.types.length === declaredTypeParameters.length
  ) {
    for (const [ordinal, parameter] of declaredTypeParameters.entries()) {
      const fact = typeArguments.facts.at(ordinal)
      const argument =
        fact === undefined ? undefined : genericArgumentOfTypeArgument(parameter, fact)
      if (argument !== undefined) substitution.set(Type.key(parameter), argument)
    }
  } else if (!typeArguments.explicit && signature !== undefined) {
    for (const [ordinal, parameter] of signature.parameters.entries()) {
      const argument = argumentsResult.facts.at(ordinal)
      if (argument?.type._tag === 'Available')
        TypeInference.infer(parameter, argument.type.type, substitution)
    }
  }
  const missingInference = declaredTypeParameters.find(
    (parameter) => substitution.get(Type.key(parameter)) === undefined,
  )
  const inferenceDiagnostic =
    specializationDiagnostic === undefined && missingInference !== undefined
      ? Diagnostic.typeArgumentArity(
          `${actorSpelling}.${operationSpelling}`,
          declaredTypeParameters.length,
          0,
          call.span,
        )
      : undefined
  const instantiatedParameters =
    signature === undefined
      ? Object.freeze([])
      : Object.freeze(
          signature.parameters.map((parameter) => Type.substitute(parameter, substitution)),
        )
  const instantiatedResult =
    signature === undefined ? undefined : Type.substitute(signature.result, substitution)
  const unsafeDiagnostic =
    signature === undefined
      ? undefined
      : unsafeCallDiagnostic(
          signature.unsafe === true,
          `${actorSpelling}.${operationSpelling}`,
          call,
          resolution,
        )
  let missingDiagnostic: Diagnostic.Diagnostic | undefined
  if (actor === undefined) {
    missingDiagnostic = Diagnostic.unknownActor(actorSpelling, actorToken.span)
  } else if (signature === undefined) {
    missingDiagnostic = Diagnostic.unknownActorOperation(
      actorSpelling,
      operationSpelling,
      operationToken.span,
    )
  } else {
    missingDiagnostic = undefined
  }
  let reference: CallReferenceFact
  if (signature !== undefined) {
    reference = Object.freeze({
      _tag: 'ResolvedBuiltin',
      spelling: `${actorSpelling}.${operationSpelling}`,
      token: operationToken,
      actor: actorSpelling,
      operation: signature.operation,
      intrinsic: signature.id,
      parameters: instantiatedParameters,
      result: instantiatedResult ?? signature.result,
      unsafe: signature.unsafe === true,
      ...(signature.returnedBorrowParameter === undefined
        ? {}
        : { returnedBorrowParameter: signature.returnedBorrowParameter }),
    })
  } else {
    reference = Object.freeze({
      _tag: 'Missing',
      spelling: `${actorSpelling}.${operationSpelling}`,
      token: actor === undefined ? actorToken : operationToken,
      ...(missingDiagnostic === undefined ? {} : { cause: Diagnostic.identity(missingDiagnostic) }),
    })
  }
  if (
    reference._tag === 'ResolvedBuiltin' &&
    declaredTypeParameters.length === 0 &&
    isSectionArity(reference.parameters.length, argumentsResult.facts.length)
  ) {
    return finishCallableSection(
      call,
      reference,
      argumentsResult,
      typeArguments,
      resolution,
      caller,
    )
  }
  const callContract = analyzeCallContract(call, reference, argumentsResult.facts)
  const expressionType =
    hasAvailableCallSyntax(call) &&
    reference._tag === 'ResolvedBuiltin' &&
    specializationDiagnostic === undefined &&
    inferenceDiagnostic === undefined &&
    unsafeDiagnostic === undefined
      ? availableExpressionType(reference.result)
      : unavailableExpressionType

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Call',
      reference,
      path: referencePath(call),
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
      ...(inferenceDiagnostic === undefined ? [] : [inferenceDiagnostic]),
      ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
      ...argumentsResult.diagnostics,
      ...typeArguments.diagnostics,
      ...callContract.diagnostics,
    ]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

export const builtinArgumentMappings = (
  reference: CallReferenceFact,
  argumentsList: ReadonlyArray<ArgumentFact>,
): ReadonlyArray<BuiltinArgumentMappingFact> => {
  if (reference._tag !== 'ResolvedBuiltin') {
    return Object.freeze([])
  }
  return Object.freeze(
    reference.parameters.flatMap((expected, ordinal): ReadonlyArray<BuiltinArgumentMappingFact> => {
      const argument = argumentsList.at(ordinal)
      return argument === undefined
        ? []
        : [Object.freeze({ _tag: 'BuiltinArgumentMapping', argument, ordinal, expected })]
    }),
  )
}

interface AggregateElementAnalysis {
  readonly expression: ExpressionFact
  readonly type?: SemanticType
  readonly syntax: SyntaxTree.Node
  readonly label?: string
  readonly token?: Token.Token
}

const generatedAggregate = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  kind: 'AnonymousNamed' | 'AnonymousPositional',
  elements: ReadonlyArray<AggregateElementAnalysis & { readonly type: SemanticType }>,
  resolution: ResolutionContext,
): { readonly struct: DeclarationFacts.StructFact; readonly type: Type.Nominal } => {
  const identity = AggregateIdentity.anonymous(source.id, syntax.span, kind)
  const type = AggregateIdentity.nominal(identity)
  const id: DeclarationFacts.DeclarationId = Object.freeze({
    _tag: 'DeclarationId',
    sourceId: source.id,
    ordinal: -1 - syntax.span.start * 2 - (kind === 'AnonymousPositional' ? 1 : 0),
  })
  const fields = elements.map((element, ordinal): DeclarationFacts.FieldFact => {
    const fieldId: DeclarationFacts.FieldId = Object.freeze({
      _tag: 'FieldId',
      owner: Object.freeze({ _tag: 'StructFieldOwnerId', declaration: id }),
      ordinal,
    })
    const token =
      element.token ?? SyntaxTree.tokens(element.syntax).at(0) ?? SyntaxTree.tokens(syntax).at(0)
    if (token === undefined)
      throw new RangeError('Generated aggregate member requires source syntax')
    const labeled = kind === 'AnonymousNamed'
    const label = element.label
    return Object.freeze({
      _tag: 'AggregateField',
      id: fieldId,
      member:
        labeled && label !== undefined
          ? AggregateIdentity.labeled(label)
          : AggregateIdentity.ordinal(ordinal),
      state: Object.freeze({ _tag: 'Unique', id: fieldId }),
      visibility: 'Public',
      name:
        labeled && label !== undefined
          ? Object.freeze({ _tag: 'Present', spelling: label, token })
          : Object.freeze({ _tag: 'Unavailable', syntax: element.syntax }),
      declaredType: Object.freeze({
        _tag: 'Resolved',
        type: element.type,
        spelling: Type.encode(element.type),
        token,
        syntax: element.syntax,
      }),
      syntax: element.syntax,
    })
  })
  const dependencies = new Map<string, Type.Nominal>()
  for (const element of elements)
    for (const dependency of Type.nominals(element.type))
      dependencies.set(Type.key(dependency), dependency)
  const struct: DeclarationFacts.StructFact = Object.freeze({
    _tag: 'StructDeclaration',
    id,
    canonical: Object.freeze({
      _tag: 'Canonical',
      id: Object.freeze({
        _tag: 'CanonicalDeclarationId',
        module: identity.module,
        name: AggregateIdentity.internalName(identity),
      }),
    }),
    visibility: 'Private',
    typeParameters: Object.freeze([]),
    name: Object.freeze({ _tag: 'Unavailable', syntax }),
    identity,
    aggregateKind: kind,
    fields: Object.freeze(fields),
    dependency: Object.freeze({
      _tag: 'Available',
      types: Object.freeze([...dependencies.values()].sort(Type.compare)),
    }),
    syntax,
  })
  resolution.generatedAggregates?.set(aggregateKey(type), struct)
  return Object.freeze({ struct, type })
}

const analyzeAggregateElements = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  elementSyntax: ReadonlyArray<{
    readonly expression: SyntaxTree.Node
    readonly label?: string
    readonly token?: Token.Token
    readonly initializerSyntax?: SyntaxTree.Node
  }>,
  struct: DeclarationFacts.StructFact,
  nominal: Type.Nominal,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  preanalyzed: ReadonlyArray<ExpressionResult> = Object.freeze([]),
): ExpressionResult => {
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const substitution =
    TypeInference.substitution(
      struct.typeParameters.map((parameter) => parameter.type),
      nominal.arguments,
    ) ?? new Map()
  const positional =
    struct.aggregateKind === 'Positional' || struct.aggregateKind === 'AnonymousPositional'
  if (positional && elementSyntax.length !== struct.fields.length)
    diagnostics.push(
      Diagnostic.tupleArityMismatch(
        Type.display(nominal),
        struct.fields.length,
        elementSyntax.length,
        syntax.span,
      ),
    )
  const seen = new Map<string, SourceSpan.SourceSpan>()
  const initializers: Array<StructInitializerFact> = []
  const mapped: Array<{ field: DeclarationFacts.FieldFact; initializer: StructInitializerFact }> =
    []
  for (const [ordinal, element] of elementSyntax.entries()) {
    let field: DeclarationFacts.FieldFact | undefined
    if (positional) field = struct.fields.at(ordinal)
    else if (element.label !== undefined) {
      const lookup = DeclarationFacts.lookupField(struct.fields, element.label)
      if (lookup._tag === 'Resolved') field = lookup.field
    }
    const expected =
      field?.declaredType._tag === 'Resolved'
        ? Type.substitute(field.declaredType.type, substitution)
        : undefined
    const analyzed =
      preanalyzed.at(ordinal) ??
      analyzeExpression(
        source,
        element.expression,
        declarations,
        declaration,
        scope,
        resolution,
        expected,
      )
    if (analyzed === undefined)
      throw new RangeError(`Cannot analyze aggregate member ${element.expression.kind}`)
    diagnostics.push(...analyzed.diagnostics)
    let state: StructInitializerState = Object.freeze({ _tag: 'Unavailable' })
    if (!positional && element.label !== undefined) {
      const previous = seen.get(element.label)
      if (previous !== undefined && field !== undefined) {
        const diagnostic = Diagnostic.duplicateStructInitializer(
          element.label,
          previous,
          element.token?.span ?? element.expression.span,
        )
        diagnostics.push(diagnostic)
        state = Object.freeze({
          _tag: 'Duplicate',
          field,
          cause: Diagnostic.identity(diagnostic),
        })
      } else if (field === undefined) {
        const diagnostic = Diagnostic.unknownStructField(
          Type.display(nominal),
          element.label,
          element.token?.span ?? element.expression.span,
        )
        diagnostics.push(diagnostic)
        state = Object.freeze({ _tag: 'Unknown', cause: Diagnostic.identity(diagnostic) })
      }
      seen.set(element.label, element.token?.span ?? element.expression.span)
    }
    if (field !== undefined && state._tag === 'Unavailable') {
      if (field.visibility === 'Private' && nominal.module !== source.id) {
        const diagnostic = Diagnostic.inaccessibleProjectedField(
          Type.display(nominal),
          element.label ?? `${ordinal}`,
          element.token?.span ?? element.expression.span,
        )
        diagnostics.push(diagnostic)
        state = Object.freeze({
          _tag: 'Inaccessible',
          field,
          cause: Diagnostic.identity(diagnostic),
        })
      } else if (
        expected !== undefined &&
        analyzed.type !== undefined &&
        !typesCompatible(analyzed.type, expected) &&
        !contextualIntegerCompatible(analyzed.fact, expected)
      ) {
        const diagnostic = Diagnostic.structFieldTypeMismatch(
          element.label ?? `${ordinal}`,
          Type.display(expected),
          Type.display(analyzed.type),
          element.expression.span,
        )
        diagnostics.push(diagnostic)
        state = Object.freeze({
          _tag: 'TypeMismatch',
          field,
          cause: Diagnostic.identity(diagnostic),
        })
      } else if (analyzed.type !== undefined) {
        state = Object.freeze({ _tag: 'Resolved', field })
      }
    }
    const initializer: StructInitializerFact = Object.freeze({
      _tag: 'StructInitializer',
      name: positional ? undefined : element.label,
      ...(element.token === undefined ? {} : { token: element.token }),
      expression: analyzed.fact,
      state,
      syntax: element.initializerSyntax ?? element.expression,
    })
    initializers.push(initializer)
    if (state._tag === 'Resolved') mapped.push(Object.freeze({ field: state.field, initializer }))
  }
  if (!positional) {
    for (const field of struct.fields) {
      if (field.name._tag !== 'Present' || seen.has(field.name.spelling)) continue
      diagnostics.push(
        Diagnostic.missingStructInitializer(nominal.name, field.name.spelling, syntax.span),
      )
    }
  }
  const authorized =
    (struct.visibility === 'Public' || nominal.module === source.id) &&
    struct.fields.every((field) => field.visibility === 'Public' || nominal.module === source.id)
  const complete =
    SyntaxTree.isAvailableSyntax(syntax) &&
    authorized &&
    initializers.length === struct.fields.length &&
    mapped.length === struct.fields.length &&
    initializers.every((initializer) => initializer.state._tag === 'Resolved')
  const type = complete ? availableExpressionType(nominal) : unavailableExpressionType
  const token = SyntaxTree.tokens(syntax).find((candidate) => candidate.kind === 'Identifier')
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'StructLiteral',
      target: Object.freeze({
        _tag: 'Resolved',
        struct,
        type: nominal,
        ...(token === undefined || struct.aggregateKind.startsWith('Anonymous') ? {} : { token }),
      }),
      authorized,
      typeArguments: Object.freeze([]),
      initializers: Object.freeze(initializers),
      fields: Object.freeze(
        struct.fields.flatMap((field) => {
          const pair = mapped.find((candidate) =>
            DeclarationFacts.sameFieldId(candidate.field.id, field.id),
          )
          return pair === undefined ? [] : [pair]
        }),
      ),
      type,
      syntax,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: complete ? nominal : undefined,
  })
}

const positionalElements = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> => {
  if (node.kind === 'TupleLiteralExpression') return node.children.filter(isExpressionNode)
  const argumentsList = SyntaxTree.directNode(node, 'ArgumentList')
  return argumentsList?.children.filter(isRecursiveArgumentNode) ?? Object.freeze([])
}

const contextualRecordElements = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): ReadonlyArray<{
  readonly expression: SyntaxTree.Node
  readonly label?: string
  readonly token?: Token.Token
  readonly initializerSyntax?: SyntaxTree.Node
}> =>
  Object.freeze(
    SyntaxTree.directNodes(node, 'StructFieldInitializer').flatMap((initializer) => {
      const expression = initializer.children.find(isExpressionNode)
      const token = directToken(initializer, 'Identifier')
      return expression === undefined
        ? []
        : [
            Object.freeze({
              expression,
              ...(token === undefined ? {} : { label: spelling(source, token), token }),
              initializerSyntax: initializer,
            }),
          ]
    }),
  )

const expectedAggregate = (
  expected: SemanticType | undefined,
  resolution: ResolutionContext,
): { readonly struct: DeclarationFacts.StructFact; readonly type: Type.Nominal } | undefined => {
  if (expected === undefined || !Type.isNominal(expected)) return undefined
  const struct = aggregateByNominal(resolution, expected)
  return struct === undefined || struct.aggregateKind.startsWith('Anonymous')
    ? undefined
    : Object.freeze({ struct, type: expected })
}

const analyzeTupleLiteral = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
): ExpressionResult => {
  const elements = positionalElements(node)
  const contextual = expectedAggregate(expected, resolution)
  if (contextual !== undefined && contextual.struct.aggregateKind === 'Positional') {
    const substitution = new Map<string, Type.GenericArgument>()
    for (const [ordinal, parameter] of contextual.struct.typeParameters.entries()) {
      const argument = contextual.type.arguments.at(ordinal)
      if (argument !== undefined && !(Type.isTypeArgument(argument) && Type.isParameter(argument)))
        substitution.set(Type.key(parameter.type), argument)
    }
    const preanalyzed = elements.map((element, ordinal): ExpressionResult => {
      const field = contextual.struct.fields.at(ordinal)
      const specialized =
        field?.declaredType._tag === 'Resolved'
          ? Type.substitute(field.declaredType.type, substitution)
          : undefined
      const result = analyzeExpression(
        source,
        element,
        declarations,
        declaration,
        scope,
        resolution,
        specialized !== undefined && Type.isConcrete(specialized) ? specialized : undefined,
      )
      if (result === undefined)
        throw new RangeError(`Cannot analyze contextual tuple element ${element.kind}`)
      if (field?.declaredType._tag === 'Resolved' && result.type !== undefined) {
        const candidate = new Map(substitution)
        if (TypeInference.infer(field.declaredType.type, result.type, candidate)) {
          for (const [key, argument] of candidate) substitution.set(key, argument)
        }
      }
      return result
    })
    const arguments_ = contextual.struct.typeParameters.map(
      (parameter, ordinal) =>
        substitution.get(Type.key(parameter.type)) ??
        contextual.type.arguments.at(ordinal) ??
        Type.parameterArgument(parameter.type),
    )
    const inferredType = Type.nominal(
      contextual.type.module,
      contextual.type.name,
      Object.freeze(arguments_),
    )
    return analyzeAggregateElements(
      source,
      node,
      elements.map((expression) => Object.freeze({ expression })),
      contextual.struct,
      inferredType,
      declarations,
      declaration,
      scope,
      resolution,
      preanalyzed,
    )
  }
  if (contextual !== undefined) {
    const diagnostic = Diagnostic.contextualAggregateKindMismatch(
      'tuple',
      Type.display(contextual.type),
      node.span,
    )
    return Object.freeze({
      fact: unavailableExpression(node),
      diagnostics: Object.freeze([diagnostic]),
      type: undefined,
    })
  }
  const analyzed = elements.map((expression): ExpressionResult => {
    const result = analyzeExpression(
      source,
      expression,
      declarations,
      declaration,
      scope,
      resolution,
    )
    if (result === undefined)
      throw new RangeError(`Cannot analyze tuple element ${expression.kind}`)
    return result
  })
  const diagnostics = analyzed.flatMap((element) => element.diagnostics)
  if (analyzed.some((element) => element.type === undefined))
    return Object.freeze({
      fact: unavailableExpression(node),
      diagnostics: Object.freeze(diagnostics),
      type: undefined,
    })
  const generated = generatedAggregate(
    source,
    node,
    'AnonymousPositional',
    analyzed.map((element, ordinal) =>
      Object.freeze({
        expression: element.fact,
        type: element.type ?? Type.unit,
        syntax: elements.at(ordinal) ?? node,
      }),
    ),
    resolution,
  )
  return analyzeAggregateElements(
    source,
    node,
    elements.map((expression) => Object.freeze({ expression })),
    generated.struct,
    generated.type,
    declarations,
    declaration,
    scope,
    resolution,
    analyzed,
  )
}

const analyzeContextualRecordLiteral = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
): ExpressionResult => {
  const elements = contextualRecordElements(source, node)
  const contextual = expectedAggregate(expected, resolution)
  if (contextual !== undefined && contextual.struct.aggregateKind === 'Named')
    return analyzeAggregateLiteral(
      source,
      node,
      declarations,
      declaration,
      scope,
      resolution,
      Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          struct: contextual.struct,
          type: contextual.type,
        }),
        diagnostics: Object.freeze([]),
      }),
    )
  if (contextual !== undefined) {
    const diagnostic = Diagnostic.contextualAggregateKindMismatch(
      'record',
      Type.display(contextual.type),
      node.span,
    )
    return Object.freeze({
      fact: unavailableExpression(node),
      diagnostics: Object.freeze([diagnostic]),
      type: undefined,
    })
  }
  const analyzed = elements.map((element): ExpressionResult => {
    const result = analyzeExpression(
      source,
      element.expression,
      declarations,
      declaration,
      scope,
      resolution,
    )
    if (result === undefined)
      throw new RangeError(`Cannot analyze record member ${element.expression.kind}`)
    return result
  })
  if (analyzed.some((element) => element.type === undefined))
    return Object.freeze({
      fact: unavailableExpression(node),
      diagnostics: Object.freeze(analyzed.flatMap((element) => element.diagnostics)),
      type: undefined,
    })
  const labels = new Map<string, SourceSpan.SourceSpan>()
  const duplicateDiagnostics: Array<Diagnostic.Diagnostic> = []
  for (const element of elements) {
    if (element.label === undefined) continue
    const span = element.token?.span ?? element.expression.span
    const previous = labels.get(element.label)
    if (previous !== undefined)
      duplicateDiagnostics.push(
        Diagnostic.duplicateStructInitializer(element.label, previous, span),
      )
    else labels.set(element.label, span)
  }
  if (duplicateDiagnostics.length > 0)
    return Object.freeze({
      fact: unavailableExpression(node),
      diagnostics: Diagnostic.merge(
        analyzed.flatMap((element) => element.diagnostics),
        duplicateDiagnostics,
      ),
      type: undefined,
    })
  const generated = generatedAggregate(
    source,
    node,
    'AnonymousNamed',
    analyzed.map((element, ordinal) => {
      const syntaxElement = elements.at(ordinal)
      return Object.freeze({
        expression: element.fact,
        type: element.type ?? Type.unit,
        syntax: syntaxElement?.expression ?? node,
        ...(syntaxElement?.label === undefined ? {} : { label: syntaxElement.label }),
        ...(syntaxElement?.token === undefined ? {} : { token: syntaxElement.token }),
      })
    }),
    resolution,
  )
  return analyzeAggregateElements(
    source,
    node,
    elements,
    generated.struct,
    generated.type,
    declarations,
    declaration,
    scope,
    resolution,
    analyzed,
  )
}

const tupleConstructor = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult | undefined => {
  const tokens = callReferenceTokens(node)
  const first = tokens.at(0)
  const second = tokens.at(1)
  let candidate: DeclarationFacts.MemberFact | undefined
  if (first !== undefined && second === undefined) {
    if (scopeSpanFor(scope, spelling(source, first)) !== undefined) return undefined
    const lookup = NameResolution.lookup(
      resolution.scope,
      resolution.index,
      spelling(source, first),
    )
    if (lookup._tag === 'Resolved') candidate = lookup.declaration
  } else if (first !== undefined && second !== undefined) {
    if (scopeSpanFor(scope, spelling(source, first)) !== undefined) return undefined
    const qualifier = NameResolution.lookup(
      resolution.scope,
      resolution.index,
      spelling(source, first),
    )
    if (qualifier._tag === 'Namespace') {
      const lookup = DeclarationFacts.member(
        resolution.index,
        qualifier.module,
        spelling(source, second),
      )
      if (lookup._tag === 'Resolved') candidate = lookup.declaration
    }
  }
  if (candidate?._tag !== 'StructDeclaration' || candidate.aggregateKind !== 'Positional')
    return undefined
  if (candidate.canonical._tag !== 'Canonical') return undefined

  const elements = positionalElements(node)
  const typeArguments = analyzeCallTypeArguments(source, node, declaration, resolution)
  const parameters = candidate.typeParameters.map((parameter) => parameter.type)
  const substitution = new Map<string, Type.GenericArgument>()
  if (typeArguments.explicit && typeArguments.types !== undefined) {
    for (const [ordinal, supplied] of typeArguments.types.entries()) {
      const parameter = parameters.at(ordinal)
      if (parameter === undefined || parameter.kind !== 'Value' || !Type.isTypeArgument(supplied))
        continue
      substitution.set(Type.key(parameter), supplied)
    }
  }
  const preanalyzed = elements.map((element, ordinal): ExpressionResult => {
    const field = candidate.fields.at(ordinal)
    const expected =
      field?.declaredType._tag === 'Resolved'
        ? Type.substitute(field.declaredType.type, substitution)
        : undefined
    const result = analyzeExpression(
      source,
      element,
      declarations,
      declaration,
      scope,
      resolution,
      expected,
    )
    if (result === undefined)
      throw new RangeError(`Cannot analyze tuple constructor argument ${element.kind}`)
    if (field?.declaredType._tag === 'Resolved' && result.type !== undefined)
      TypeInference.infer(field.declaredType.type, result.type, substitution)
    return result
  })
  const diagnostics: Array<Diagnostic.Diagnostic> = [...typeArguments.diagnostics]
  const arguments_: Array<Type.GenericArgument> = []
  for (const parameter of candidate.typeParameters) {
    const argument = substitution.get(Type.key(parameter.type))
    if (argument === undefined) {
      diagnostics.push(
        Diagnostic.uninferredTypeParameter(
          candidate.canonical.id.name,
          parameter.type.name,
          parameter.syntax.span,
        ),
      )
      arguments_.push(Type.parameterArgument(parameter.type))
    } else arguments_.push(argument)
  }
  const nominal = Type.nominal(
    candidate.canonical.id.module,
    candidate.canonical.id.name,
    Object.freeze(arguments_),
  )
  const analyzed = analyzeAggregateElements(
    source,
    node,
    elements.map((expression) => Object.freeze({ expression })),
    candidate,
    nominal,
    declarations,
    declaration,
    scope,
    resolution,
    preanalyzed,
  )
  return Object.freeze({
    fact: analyzed.fact,
    diagnostics: Diagnostic.merge(diagnostics, analyzed.diagnostics),
    type: analyzed.type,
  })
}

export const analyzeGroupedExpression = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
  borrowAllowed = false,
): ExpressionResult => {
  const child = node.children.find(isExpressionNode)
  const expression =
    child === undefined
      ? undefined
      : analyzeExpression(
          source,
          child,
          declarations,
          declaration,
          scope,
          resolution,
          expected,
          borrowAllowed,
        )
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

/**
 * Analyzes `&&` or `||`. Both operands must be `bool` and the result is `bool`. HIR retains the
 * right operand as a conditional region, so its ordinary effects, moves, loans, and cleanup stay
 * on the path that executes it.
 */
export const analyzeShortCircuitExpression = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  operator: Operator.ShortCircuit,
  operandNodes: ReadonlyArray<SyntaxTree.Node>,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const boolean: SemanticType = Scalar.boolean.spelling
  const argumentsResult = analyzeArgumentNodes(
    source,
    node,
    operandNodes,
    declarations,
    declaration,
    scope,
    resolution,
    Object.freeze(operandNodes.map(() => boolean)),
  )
  const operandDiagnostics = argumentsResult.facts.flatMap((argument) =>
    argument.type._tag === 'Available' && !Type.equals(argument.type.type, boolean)
      ? [
          Diagnostic.argumentTypeMismatch(
            Type.display(boolean),
            Type.display(argument.type.type),
            argument.syntax.span,
          ),
        ]
      : [],
  )
  const rejected =
    operandDiagnostics.length > 0 ||
    argumentsResult.facts.length !== 2 ||
    argumentsResult.facts.some((argument) => argument.type._tag !== 'Available')
  const type = rejected ? unavailableExpressionType : availableExpressionType(boolean)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'ShortCircuit',
      operator,
      arguments: argumentsResult.facts,
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze([...argumentsResult.diagnostics, ...operandDiagnostics]),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

export interface OperatorContractSelection extends InterfaceOperationFact {
  readonly declaration: DeclarationFacts.ServiceOperationFact
  readonly parameters: ReadonlyArray<SemanticType>
  readonly result: SemanticType
  readonly label: string
}

export const operatorContractSelection = (
  capability: Type.Nominal,
  provider: Type.Type,
  operation: DeclarationFacts.InterfaceOperationApplicationFact,
): OperatorContractSelection | undefined => {
  const contract = interfaceOperationContract(operation)
  const name = operation.declaration.name
  if (contract === undefined || name._tag !== 'Present') return undefined
  return Object.freeze({
    capability,
    provider,
    operation: name.spelling,
    contract: operation,
    declaration: contract.declaration,
    parameters: contract.parameters,
    result: contract.result,
    label: `${Type.encode(capability)}.${name.spelling}`,
  })
}

export const boundOperatorSelections = (
  declaration: DeclarationFact,
  operator: Operator.Eligible,
): ReadonlyArray<OperatorContractSelection> =>
  Object.freeze(
    declaration.typeParameters.flatMap((parameter) =>
      parameter.bounds.flatMap((bound) => {
        if (bound._tag !== 'ResolvedBound') {
          return []
        }
        return bound.application.operations.flatMap((operation) => {
          if (operation.declaration.operator?.operator !== operator) return []
          const selected = operatorContractSelection(
            bound.application.capability,
            parameter.type,
            operation,
          )
          return selected === undefined ? [] : [selected]
        })
      }),
    ),
  )

export const concreteOperatorSelections = (
  index: DeclarationIndex.Index,
  module: string,
  operator: Operator.Eligible,
): ReadonlyArray<OperatorContractSelection> => {
  const interfaces = index.modules.flatMap((headers) => headers.interfaces)
  const selections = index.modules.flatMap((headers) =>
    headers.conformances.flatMap((conformance) => {
      if (
        conformance.validity._tag !== 'ValidConformance' ||
        conformance.coherence._tag !== 'Coherent' ||
        conformance.termination._tag !== 'Terminating' ||
        conformance.capability._tag !== 'Resolved' ||
        conformance.provider._tag !== 'Resolved' ||
        !Type.isNominal(conformance.capability.type)
      )
        return []
      const capability = conformance.capability.type
      const provider = conformance.provider.type
      const interface_ = interfaces.find(
        (candidate) =>
          candidate.canonical._tag === 'Canonical' &&
          candidate.canonical.id.module === capability.module &&
          candidate.canonical.id.name === capability.name &&
          (candidate.visibility === 'Public' || candidate.canonical.id.module === module),
      )
      if (interface_ === undefined) return []
      const proof = ConformanceProof.prove(index, provider, capability)
      if (
        proof._tag !== 'Proved' ||
        proof.selection._tag !== 'SourceSelection' ||
        proof.selection.module !== conformance.module ||
        proof.selection.ordinal !== conformance.ordinal
      )
        return []
      const application = DeclarationFacts.interfaceApplication(interface_, capability, provider)
      if (application?.available !== true) return []
      return application.operations.flatMap((operation) => {
        if (operation.declaration.operator?.operator !== operator) return []
        const selected = operatorContractSelection(capability, provider, operation)
        return selected === undefined ? [] : [selected]
      })
    }),
  )
  const unique = new Map<string, OperatorContractSelection>()
  for (const selection of selections)
    unique.set(
      `${Type.key(selection.capability)}\u0000${Type.key(selection.provider)}\u0000${selection.operation}`,
      selection,
    )
  return Object.freeze([...unique.values()])
}

export const operatorSelectionMatches = (
  selection: OperatorContractSelection,
  arguments_: ReadonlyArray<ArgumentFact>,
): boolean =>
  selection.parameters.length === arguments_.length &&
  arguments_.every((argument, ordinal) => {
    const expected = selection.parameters.at(ordinal)
    return (
      expected !== undefined &&
      argument.type._tag === 'Available' &&
      (typesCompatible(argument.type.type, expected) ||
        contextualIntegerCompatible(argument.expression, expected))
    )
  })

export const finishInterfaceOperator = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  operator: Operator.Eligible,
  operatorToken: Token.Token,
  operandNodes: ReadonlyArray<SyntaxTree.Node>,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  selection: OperatorContractSelection,
): ExpressionResult => {
  const argumentsResult = analyzeArgumentNodes(
    source,
    node,
    operandNodes,
    declarations,
    declaration,
    scope,
    resolution,
    selection.parameters,
  )
  const arguments_ = Object.freeze(
    argumentsResult.facts.map((argument) => {
      let expression = argument.expression
      while (expression._tag === 'Grouped') expression = expression.expression
      return expression === argument.expression
        ? argument
        : Object.freeze({ ...argument, expression, syntax: expression.syntax })
    }),
  )
  const reference: CallReferenceFact = Object.freeze({
    _tag: 'ResolvedInterfaceOperation',
    spelling: selection.label,
    token: operatorToken,
    capability: selection.capability,
    provider: selection.provider,
    operation: selection.operation,
    declaration: selection.declaration,
    interfaceContract: selection.contract,
    parameters: selection.parameters,
    result: selection.result,
  })
  const contract = analyzeCallContract(node, reference, arguments_, true)
  const type =
    contract.fact._tag === 'Compatible'
      ? availableExpressionType(selection.result)
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Operator',
      operator,
      reference,
      arguments: arguments_,
      mappings: Object.freeze(
        selection.parameters.flatMap((expected, ordinal) => {
          const argument = arguments_.at(ordinal)
          return argument === undefined
            ? []
            : [
                Object.freeze({
                  _tag: 'BuiltinArgumentMapping' as const,
                  argument,
                  ordinal,
                  expected,
                }),
              ]
        }),
      ),
      contract: contract.fact,
      interfaceOperation: selection,
      ...(selection.contract.functionKind === 'Effect'
        ? { witnessEffectSite: executableSite('EffectSiteId', resolution, node) }
        : {}),
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze([...argumentsResult.diagnostics, ...contract.diagnostics]),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

export const analyzeOperatorExpression = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
): ExpressionResult => {
  const operatorToken = node.children.find(
    (element): element is Token.Token =>
      SyntaxTree.isToken(element) &&
      (node.kind === 'PrefixExpression'
        ? Operator.prefix(element.kind) !== undefined
        : Operator.infix(element.kind) !== undefined),
  )
  let operator: Operator.Prefix | Operator.Infix | undefined
  if (operatorToken === undefined) {
    operator = undefined
  } else if (node.kind === 'PrefixExpression') {
    operator = Operator.prefix(operatorToken.kind)
  } else {
    operator = Operator.infix(operatorToken.kind)?.operator
  }
  const operandNodes = node.children.filter(isExpressionNode)
  if (operator !== undefined && Operator.isShortCircuit(operator)) {
    return analyzeShortCircuitExpression(
      source,
      node,
      operator,
      operandNodes,
      declarations,
      declaration,
      scope,
      resolution,
    )
  }
  const initialExpected =
    typeof expected === 'string' &&
    Scalar.isSpelling(expected) &&
    node.kind === 'InfixExpression' &&
    operator !== undefined &&
    !Operator.isPredicate(operator)
      ? Object.freeze(operandNodes.map(() => expected))
      : Object.freeze([])
  let argumentsResult = analyzeArgumentNodes(
    source,
    node,
    operandNodes,
    declarations,
    declaration,
    scope,
    resolution,
    initialExpected,
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

  // A bare numeric literal's scalar type is only a default (Scalar.defaultInteger), so it must
  // not drive the operand retry when another operand carries a declared scalar type: `5 + x`
  // must type like `x + 5`. When every operand is a literal, the first (defaulted) one drives.
  const drivingOrdinal = operandNodes.findIndex((operand, ordinal) => {
    if (operand.kind === 'IntegerLiteralExpression' || operand.kind === 'FloatingLiteralExpression')
      return false
    const type = argumentsResult.facts.at(ordinal)?.type
    return (
      type?._tag === 'Available' && typeof type.type === 'string' && Scalar.isSpelling(type.type)
    )
  })
  const firstType = argumentsResult.facts.at(drivingOrdinal === -1 ? 0 : drivingOrdinal)?.type
  if (
    firstType?._tag === 'Available' &&
    typeof firstType.type === 'string' &&
    Scalar.isSpelling(firstType.type) &&
    (initialExpected.length === 0 || firstType.type !== expected) &&
    operandNodes.length > 1
  ) {
    argumentsResult = analyzeArgumentNodes(
      source,
      node,
      operandNodes,
      declarations,
      declaration,
      scope,
      resolution,
      Object.freeze(operandNodes.map(() => firstType.type)),
    )
  }
  const selectedFirstType = argumentsResult.facts.at(0)?.type
  const selectedSecondType = argumentsResult.facts.at(1)?.type
  const firstEnum =
    selectedFirstType?._tag === 'Available'
      ? enumFactByType(resolution.index, selectedFirstType.type)
      : undefined
  const secondEnum =
    selectedSecondType?._tag === 'Available'
      ? enumFactByType(resolution.index, selectedSecondType.type)
      : undefined
  if (firstEnum !== undefined || secondEnum !== undefined) {
    const isEquality = operator === 'Equals' || operator === 'NotEquals'
    const isOrdering =
      operator === 'LessThan' ||
      operator === 'LessOrEqual' ||
      operator === 'GreaterThan' ||
      operator === 'GreaterOrEqual'
    const firstTypeText =
      selectedFirstType?._tag === 'Available' ? Type.encode(selectedFirstType.type) : '?'
    const secondTypeText =
      selectedSecondType?._tag === 'Available' ? Type.encode(selectedSecondType.type) : '?'
    const sameEnum =
      firstEnum?.canonical._tag === 'Canonical' &&
      secondEnum?.canonical._tag === 'Canonical' &&
      firstEnum.canonical.id.module === secondEnum.canonical.id.module &&
      firstEnum.canonical.id.name === secondEnum.canonical.id.name
    if (
      isEquality &&
      sameEnum &&
      firstEnum !== undefined &&
      firstEnum.canonical._tag === 'Canonical'
    ) {
      const equalityOperator = operator === 'Equals' ? 'Equals' : 'NotEquals'
      const reference: CallReferenceFact = Object.freeze({
        _tag: 'ResolvedEnumEquality',
        spelling: spelling(source, operatorToken),
        token: operatorToken,
        enum: firstEnum.canonical.id,
        operator: equalityOperator,
      })
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Operator',
          operator,
          reference,
          arguments: argumentsResult.facts,
          mappings: Object.freeze([]),
          contract: Object.freeze({
            _tag: 'Compatible',
            expectedCount: 2,
            actualCount: argumentsResult.facts.length,
            typeArguments: Object.freeze([]),
            substitution: new Map(),
            evidence: Object.freeze([]),
            inferredProviderSelectors: Object.freeze([]),
          }),
          type: availableBoolExpressionType,
          syntax: node,
        }),
        diagnostics: argumentsResult.diagnostics,
        type: 'bool',
      })
    }
    let diagnostic: Diagnostic.Diagnostic | undefined
    if (isOrdering) {
      diagnostic = Diagnostic.enumOrdering(
        firstEnum === undefined ? secondTypeText : firstTypeText,
        spelling(source, operatorToken),
        operatorToken.span,
      )
    } else if (isEquality && firstEnum !== undefined && secondEnum !== undefined) {
      diagnostic = Diagnostic.crossEnumEquality(firstTypeText, secondTypeText, operatorToken.span)
    } else if (isEquality) {
      diagnostic = Diagnostic.enumIntegerMismatch(
        firstEnum === undefined ? secondTypeText : firstTypeText,
        firstEnum === undefined ? firstTypeText : secondTypeText,
        firstEnum === undefined ? 'IntegerToEnum' : 'EnumToInteger',
        operatorToken.span,
      )
    } else {
      diagnostic = undefined
    }
    if (diagnostic !== undefined) {
      const reference: CallReferenceFact = Object.freeze({
        _tag: 'Missing',
        spelling: spelling(source, operatorToken),
        token: operatorToken,
        cause: Diagnostic.identity(diagnostic),
      })
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Operator',
          operator,
          reference,
          arguments: argumentsResult.facts,
          mappings: Object.freeze([]),
          contract: Object.freeze({
            _tag: 'Unavailable',
            reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: node }),
            cause: Diagnostic.identity(diagnostic),
          }),
          type: unavailableExpressionType,
          syntax: node,
        }),
        diagnostics: Object.freeze([...argumentsResult.diagnostics, diagnostic]),
        type: undefined,
      })
    }
  }
  const builtinOperand =
    selectedFirstType?._tag === 'Available' &&
    (Type.isString(selectedFirstType.type) || Scalar.isSpelling(selectedFirstType.type))
  if (!builtinOperand) {
    const candidates = [
      ...boundOperatorSelections(declaration, operator),
      ...concreteOperatorSelections(resolution.index, source.id, operator),
    ].filter((candidate) => operatorSelectionMatches(candidate, argumentsResult.facts))
    if (candidates.length === 1) {
      const candidate = candidates.at(0)
      if (candidate !== undefined)
        return finishInterfaceOperator(
          source,
          node,
          operator,
          operatorToken,
          operandNodes,
          declarations,
          declaration,
          scope,
          resolution,
          candidate,
        )
    }
    const operatorSpelling = spelling(source, operatorToken)
    const operandTypes = argumentsResult.facts.flatMap((argument) =>
      argument.type._tag === 'Available' ? [Type.display(argument.type.type)] : [],
    )
    const diagnostic =
      candidates.length > 1
        ? Diagnostic.ambiguousOperator(
            operatorSpelling,
            candidates.map((candidate) => candidate.label),
            operatorToken.span,
          )
        : Diagnostic.operatorNotApplicable(operatorSpelling, operandTypes, operatorToken.span)
    const reference: CallReferenceFact = Object.freeze({
      _tag: 'Missing',
      spelling: operatorSpelling,
      token: operatorToken,
      cause: Diagnostic.identity(diagnostic),
    })
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Operator',
        operator,
        reference,
        arguments: argumentsResult.facts,
        mappings: Object.freeze([]),
        contract: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: node }),
          cause: Diagnostic.identity(diagnostic),
        }),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([...argumentsResult.diagnostics, diagnostic]),
      type: undefined,
    })
  }
  let selectedActor: Operator.Actor
  if (selectedFirstType?._tag === 'Available' && Type.isString(selectedFirstType.type)) {
    selectedActor = 'string'
  } else if (selectedFirstType?._tag === 'Available' && Scalar.isSpelling(selectedFirstType.type)) {
    selectedActor = selectedFirstType.type
  } else {
    selectedActor = Scalar.defaultInteger.spelling
  }
  const target = Operator.target(operator, selectedActor)
  const signature = builtinSignature(target.actor, target.operation, 'Primitive')
  if (signature === undefined) throw new RangeError('Compiler operator table is inconsistent')
  const operatorParameters = signature.parameters
  const operatorResult = signature.result
  const reference: CallReferenceFact = Object.freeze({
    _tag: 'ResolvedBuiltin',
    spelling: `${target.actor}.${target.operation}`,
    token: operatorToken,
    actor: target.actor,
    operation: signature.operation,
    intrinsic: signature.id,
    parameters: operatorParameters,
    result: operatorResult,
    unsafe: signature.unsafe === true,
  })
  const contract = analyzeCallContract(
    node,
    reference,
    argumentsResult.facts,
    isAvailableSyntax(node),
  )
  const expressionType =
    contract.fact._tag === 'Compatible'
      ? availableExpressionType(operatorResult)
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

type AppliedInterfaceOperationTarget =
  | {
      readonly _tag: 'Resolved'
      readonly interface: DeclarationFacts.InterfaceFact
      readonly capability: Type.Nominal
      readonly application: DeclarationFacts.DeclaredTypeFact
      readonly reference: Extract<
        CallReferenceFact,
        { readonly _tag: 'ResolvedInterfaceOperation' }
      >
      readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
    }
  | {
      readonly _tag: 'Invalid'
      readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
    }

const appliedMemberExpression = (node: SyntaxTree.Node): SyntaxTree.Node | undefined => {
  let callee = node.kind === 'CallExpression' ? callCallee(node) : node
  while (callee.kind === 'GroupedExpression') {
    const grouped = callee.children.find(isExpressionNode)
    if (grouped === undefined) return undefined
    callee = grouped
  }
  return callee.kind === 'AppliedMemberExpression' ? callee : undefined
}

const appliedMemberParts = (
  node: SyntaxTree.Node,
):
  | {
      readonly owner: SyntaxTree.Node
      readonly member: Token.Token
    }
  | undefined => {
  const expression = appliedMemberExpression(node)
  if (expression === undefined) return undefined
  const selector = SyntaxTree.directNode(expression, 'AppliedMemberSelector')
  const owner = selector === undefined ? undefined : SyntaxTree.directNode(selector, 'AppliedType')
  const member = selector === undefined ? undefined : directToken(selector, 'Identifier')
  return selector === undefined || owner === undefined || member === undefined
    ? undefined
    : Object.freeze({ owner, member })
}

/** One inherent receiver method of a nominal owner, when the owner declares it. */
type MethodCandidate =
  | {
      readonly _tag: 'Inherent'
      readonly declaration: DeclarationFact
      readonly ownerSpelling: string
    }
  | {
      readonly _tag: 'NoReceiver'
      readonly declaration: DeclarationFact
      readonly ownerSpelling: string
    }
  | {
      readonly _tag: 'Unavailable'
      readonly reference: Extract<CallReferenceFact, { readonly _tag: 'Missing' }>
      readonly diagnostic?: Diagnostic.Diagnostic
    }
  | {
      readonly _tag: 'Bound'
      readonly reference: Extract<
        CallReferenceFact,
        { readonly _tag: 'ResolvedInterfaceOperation' }
      >
    }
  | {
      readonly _tag: 'AmbiguousBound'
      readonly parameter: string
      readonly interfaces: ReadonlyArray<string>
    }
  | {
      readonly _tag: 'Conformance'
      readonly reference: Extract<
        CallReferenceFact,
        { readonly _tag: 'ResolvedInterfaceOperation' }
      >
    }
  | {
      readonly _tag: 'AmbiguousConformance'
      readonly receiver: string
      readonly interfaces: ReadonlyArray<string>
    }
  | { readonly _tag: 'Missing' }

/**
 * The receiver operations of one name that a concrete receiver's proved conformances supply.
 *
 * `implementedContracts` is the shared authority: it admits an application only when the ordinary
 * proof selects that exact source conformance and the interface itself is visible to the calling
 * module, so an interface the caller cannot name never contributes a member. Selecting the operation
 * through the applied interface is the same pair the qualified spelling uses once it knows `Self`;
 * because the provider is already concrete here, no `Self` inference is needed. An operation
 * declaring its own binders has no contract, and one with no operand of the provider's type is not
 * a receiver operation; neither supplies a candidate through any spelling.
 */
const conformanceOperationCandidates = (
  receiverType: Type.Nominal,
  member: string,
  resolution: ResolutionContext,
): ReadonlyArray<{
  readonly capability: Type.Nominal
  readonly operation: DeclarationFacts.InterfaceOperationApplicationFact
}> =>
  Object.freeze(
    ConformanceProof.implementedContracts(
      resolution.index,
      resolution.scope.module,
      receiverType,
    ).flatMap((capability) => {
      const contract = ConformanceProof.contractByCapability(resolution.index, capability)
      if (contract === undefined || contract.dependencyEligible) return []
      const application = DeclarationFacts.interfaceApplication(contract, capability, receiverType)
      const operation = application?.operations.find(
        (candidate) =>
          candidate.declaration.name._tag === 'Present' &&
          candidate.declaration.name.spelling === member &&
          candidate.receiverAccess !== 'Unavailable',
      )
      return application?.available === true && operation !== undefined
        ? [Object.freeze({ capability, operation })]
        : []
    }),
  )

/**
 * Resolves a bare qualified call `Contract.operation(...)` whose caller has no bound for the
 * contract: the receiver operand's own type selects the conformance. A parameter-typed receiver
 * has no bound to select through and is reported as such; a nominal receiver selects its unique
 * proved conformance of the contract, and two applications are reported as ambiguous. A call with
 * no receiver operand, or none the contract can read, resolves nothing here.
 */
const concreteContractOperationReference = (
  contract: DeclarationFacts.ContractFact,
  member: string,
  memberToken: Token.Token,
  argumentsResult: ArgumentsResult,
  resolution: ResolutionContext,
):
  | {
      readonly _tag: 'Resolved'
      readonly reference: Extract<
        CallReferenceFact,
        { readonly _tag: 'ResolvedInterfaceOperation' }
      >
    }
  | { readonly _tag: 'Rejected'; readonly diagnostic: Diagnostic.Diagnostic }
  | undefined => {
  if (contract.canonical._tag !== 'Canonical') return undefined
  const source = contract.operationContracts.find(
    (candidate) =>
      candidate.declaration.name._tag === 'Present' &&
      candidate.declaration.name.spelling === member,
  )
  // An ambient service operation takes its provider from the Effect environment, never from an
  // argument, so the synthesized receiver operand selects nothing here.
  if (source === undefined || source.operands.some((operand) => operand.parameter.id.ordinal < 0))
    return undefined
  const receiverOrdinal = source.operands.findIndex((operand) => {
    if (operand.type._tag !== 'Resolved') return false
    const type = operand.type.type
    return Type.equals(Type.isReference(type) ? type.target : type, contract.self)
  })
  const receiver = receiverOrdinal < 0 ? undefined : argumentsResult.facts.at(receiverOrdinal)
  if (receiver === undefined || receiver.type._tag !== 'Available') return undefined
  const receiverType = Type.isReference(receiver.type.type)
    ? receiver.type.type.target
    : receiver.type.type
  const contractName = contract.canonical.id.name
  if (Type.isParameter(receiverType))
    return Object.freeze({
      _tag: 'Rejected',
      diagnostic: Diagnostic.invalidConformance(
        `${Type.encode(receiverType)} does not implement ${contractName}`,
        memberToken.span,
      ),
    })
  if (!Type.isNominal(receiverType)) return undefined
  const canonical = contract.canonical.id
  const supplied = ConformanceProof.implementedContracts(
    resolution.index,
    resolution.scope.module,
    receiverType,
  ).flatMap((capability) => {
    if (capability.module !== canonical.module || capability.name !== canonical.name) return []
    const application = DeclarationFacts.interfaceApplication(contract, capability, receiverType)
    const operation = application?.operations.find(
      (candidate) =>
        candidate.declaration.name._tag === 'Present' &&
        candidate.declaration.name.spelling === member,
    )
    return application?.available === true && operation !== undefined
      ? [Object.freeze({ capability, operation })]
      : []
  })
  if (supplied.length === 0)
    return Object.freeze({
      _tag: 'Rejected',
      diagnostic: Diagnostic.invalidConformance(
        `${Type.encode(receiverType)} does not implement ${contractName}`,
        memberToken.span,
      ),
    })
  if (supplied.length > 1)
    return Object.freeze({
      _tag: 'Rejected',
      diagnostic: Diagnostic.ambiguousSuppliedOperation(
        Type.encode(receiverType),
        member,
        supplied.map(({ capability }) => Type.encode(capability)),
        memberToken.span,
      ),
    })
  const selected = supplied.at(0)
  const selectedContract =
    selected === undefined ? undefined : interfaceOperationContract(selected.operation)
  if (selected === undefined || selectedContract === undefined) return undefined
  return Object.freeze({
    _tag: 'Resolved',
    reference: Object.freeze({
      _tag: 'ResolvedInterfaceOperation' as const,
      spelling: `${Type.encode(selected.capability)}.${member}`,
      token: memberToken,
      capability: selected.capability,
      provider: receiverType,
      operation: member,
      declaration: selectedContract.declaration,
      interfaceContract: selectedContract.contract,
      parameters: selectedContract.parameters,
      result: selectedContract.result,
    }),
  })
}

/**
 * Resolves the member a value-side spelling `receiver.member(...)` names, mirroring the type-side
 * `lookupAssociated`: an inherent member of a nominal receiver, the unique receiver operation among
 * a type parameter's declared bounds, or — only where the receiver's own type has no such member at
 * all — the unique receiver operation its visible proved conformances supply.
 */
export const resolveMethodCandidate = (
  subjectType: SemanticType,
  member: string,
  memberToken: Token.Token,
  caller: DeclarationFact,
  resolution: ResolutionContext,
): MethodCandidate => {
  if (Type.isNominal(subjectType)) {
    const owner = DeclarationFacts.byCanonical(resolution.index, {
      _tag: 'CanonicalDeclarationId',
      module: subjectType.module,
      name: subjectType.name,
    })
    if (owner === undefined) return Object.freeze({ _tag: 'Missing' })
    const associated = NameResolution.lookupAssociated(
      resolution.index,
      owner,
      member,
      resolution.scope.module,
    )
    if (associated._tag === 'Inherent') {
      const fact =
        associated.declaration._tag === 'FunctionDeclaration'
          ? associated.declaration.associatedMember
          : undefined
      if (fact === undefined) return Object.freeze({ _tag: 'Missing' })
      return fact.receiver
        ? Object.freeze({
            _tag: 'Inherent',
            declaration: associated.declaration,
            ownerSpelling: fact.ownerSpelling,
          })
        : Object.freeze({
            _tag: 'NoReceiver',
            declaration: associated.declaration,
            ownerSpelling: fact.ownerSpelling,
          })
    }
    if (associated._tag === 'Inaccessible') {
      const diagnostic = Diagnostic.inaccessibleImportedMember(
        associated.declaration.canonical._tag === 'Canonical'
          ? associated.declaration.canonical.id.module
          : subjectType.name,
        member,
        memberToken.span,
      )
      return Object.freeze({
        _tag: 'Unavailable',
        reference: Object.freeze({
          _tag: 'Missing',
          spelling: `${subjectType.name}.${member}`,
          token: memberToken,
          cause: Diagnostic.identity(diagnostic),
        }),
        diagnostic,
      })
    }
    if (associated._tag === 'Duplicate')
      return Object.freeze({
        _tag: 'Unavailable',
        reference: Object.freeze({
          _tag: 'Missing',
          spelling: `${subjectType.name}.${member}`,
          token: memberToken,
          cause: associated.cause,
        }),
      })
    // Only a name the receiver's own type does not claim at all falls through to its conformances.
    // An inaccessible or duplicate inherent member has already answered above, so a conformance
    // never rescues a name an inherent declaration claimed and failed.
    const supplied = conformanceOperationCandidates(subjectType, member, resolution)
    if (supplied.length === 0) return Object.freeze({ _tag: 'Missing' })
    if (supplied.length > 1)
      return Object.freeze({
        _tag: 'AmbiguousConformance',
        receiver: Type.encode(subjectType),
        interfaces: Object.freeze(supplied.map(({ capability }) => Type.encode(capability))),
      })
    const selectedConformance = supplied.at(0)
    if (selectedConformance === undefined) return Object.freeze({ _tag: 'Missing' })
    const suppliedContract = interfaceOperationContract(selectedConformance.operation)
    if (suppliedContract === undefined) return Object.freeze({ _tag: 'Missing' })
    return Object.freeze({
      _tag: 'Conformance',
      reference: Object.freeze({
        _tag: 'ResolvedInterfaceOperation' as const,
        spelling: `${Type.encode(selectedConformance.capability)}.${member}`,
        token: memberToken,
        capability: selectedConformance.capability,
        provider: subjectType,
        operation: member,
        declaration: suppliedContract.declaration,
        interfaceContract: suppliedContract.contract,
        parameters: suppliedContract.parameters,
        result: suppliedContract.result,
      }),
    })
  }
  if (!Type.isParameter(subjectType)) return Object.freeze({ _tag: 'Missing' })
  const parameter = caller.typeParameters.find((candidate) =>
    Type.equals(candidate.type, subjectType),
  )
  if (parameter === undefined) return Object.freeze({ _tag: 'Missing' })
  const candidates = parameter.bounds.flatMap((bound) =>
    bound._tag === 'ResolvedBound'
      ? bound.application.operations.flatMap((operation) =>
          operation.declaration.name._tag === 'Present' &&
          operation.declaration.name.spelling === member &&
          operation.receiverAccess !== 'Unavailable'
            ? [Object.freeze({ bound, operation })]
            : [],
        )
      : [],
  )
  const parameterName =
    parameter.name._tag === 'Present' ? parameter.name.spelling : Type.encode(parameter.type)
  if (candidates.length === 0) return Object.freeze({ _tag: 'Missing' })
  if (candidates.length > 1)
    return Object.freeze({
      _tag: 'AmbiguousBound',
      parameter: parameterName,
      interfaces: Object.freeze(candidates.map(({ bound }) => bound.spelling)),
    })
  const selected = candidates.at(0)
  if (selected === undefined) return Object.freeze({ _tag: 'Missing' })
  const contract = interfaceOperationContract(selected.operation)
  if (contract === undefined) return Object.freeze({ _tag: 'Missing' })
  return Object.freeze({
    _tag: 'Bound',
    reference: Object.freeze({
      _tag: 'ResolvedInterfaceOperation' as const,
      spelling: `${parameterName}.${member}`,
      token: memberToken,
      capability: selected.bound.application.capability,
      provider: parameter.type,
      operation: member,
      declaration: contract.declaration,
      interfaceContract: contract.contract,
      parameters: contract.parameters,
      result: contract.result,
    }),
  })
}

/**
 * Prepends the owner binders a receiver's static type fixes to an explicit type-argument list, so
 * `value.map<i64>(f)` binds `i64` to the member's own `U` while `T` comes from `value: Option<i32>`.
 * Owner binders the receiver leaves open are not prepended, and the list then binds as written.
 */
const withReceiverOwnerArguments = (
  declaration: DeclarationFact,
  receiverType: SemanticType | undefined,
  explicit: CallTypeArgumentsResult,
  memberToken: Token.Token,
  node: SyntaxTree.Node,
): CallTypeArgumentsResult => {
  const parameterZero = declaration.parameters.at(0)
  if (receiverType === undefined || parameterZero?.declaredType._tag !== 'Resolved') return explicit
  const pattern = parameterZero.declaredType.type
  const declared = Type.isReference(pattern) ? pattern.target : pattern
  const actual = Type.isReference(receiverType) ? receiverType.target : receiverType
  const inferred = new Map<string, Type.GenericArgument>()
  if (!TypeInference.infer(declared, actual, inferred)) return explicit
  const leading: Array<SemanticType> = []
  for (const parameter of declaration.typeParameters) {
    const bound = inferred.get(Type.key(parameter.type))
    if (bound === undefined || !Type.isTypeArgument(bound)) break
    leading.push(bound)
  }
  if (leading.length === 0) return explicit
  const facts = [
    ...leading.map((type, ordinal) =>
      Object.freeze({
        _tag: 'TypeArgument' as const,
        ordinal,
        syntax: node,
        declared: Object.freeze({
          _tag: 'Resolved' as const,
          type,
          spelling: Type.display(type),
          token: memberToken,
          syntax: node,
        }),
        type,
      }),
    ),
    ...explicit.facts.map((fact, ordinal) =>
      Object.freeze({ ...fact, ordinal: leading.length + ordinal }),
    ),
  ]
  const types = facts.map((fact) => fact.type)
  return Object.freeze({
    explicit: true,
    facts: Object.freeze(facts),
    ...(types.every((type) => type !== undefined)
      ? { types: Object.freeze(types.filter((type): type is SemanticType => type !== undefined)) }
      : {}),
    diagnostics: explicit.diagnostics,
  })
}

/**
 * Adapts the receiver to parameter zero: a reference parameter borrows a place (or passes a
 * reference through), and a by-value parameter consumes a place or takes an rvalue as it is. The
 * declaration decides the ownership, so nothing is written in front of the receiver.
 */
const synthesizeReceiver = (
  subjectNode: SyntaxTree.Node,
  subjectResult: ExpressionResult,
  parameterType: SemanticType | undefined,
  declaration: DeclarationFact,
): ExpressionResult => {
  const subjectType = subjectResult.type
  if (parameterType !== undefined && Type.isReference(parameterType)) {
    // A reference receiver of the declared access passes through; any other reference reborrows
    // through the ordinary rules, so `&mut` reaches a `&Self` method and `&` never reaches `&mut`.
    if (
      subjectType !== undefined &&
      Type.isReference(subjectType) &&
      subjectType.access === parameterType.access
    )
      return subjectResult
    return borrowSubject(
      subjectNode,
      subjectNode,
      subjectResult,
      parameterType.access,
      parameterType,
      declaration,
    )
  }
  if (borrowRoot(subjectResult.fact) === undefined) return subjectResult
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Move',
      subject: subjectResult.fact,
      type: subjectResult.fact.type,
      syntax: subjectNode,
    }),
    diagnostics: subjectResult.diagnostics,
    type: subjectResult.type,
  })
}

const declaredParameterType = (parameter: ParameterFact | undefined): SemanticType | undefined =>
  parameter?.declaredType._tag === 'Resolved' ? parameter.declaredType.type : undefined

/**
 * Whether binding `member` to `subject` would loan a temporary: a section outlives the statement
 * that built it, so a borrowed receiver must be a place or an existing reference.
 */
const borrowsTemporary = (member: DeclarationFact, subject: ExpressionResult): boolean => {
  const parameterType = declaredParameterType(member.parameters.at(0))
  return (
    parameterType !== undefined &&
    Type.isReference(parameterType) &&
    borrowRoot(subject.fact) === undefined &&
    (subject.type === undefined || !Type.isReference(subject.type))
  )
}

/**
 * Binds `subject.member` as the section of an inherent receiver method: the receiver is captured
 * as parameter zero under the declared mode and the remaining parameters stay open, so the value
 * lowers and executes exactly as a trailing section of `Owner.member` does.
 */
const finishBoundMethod = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  subjectNode: SyntaxTree.Node,
  subject: ExpressionResult,
  candidate: Extract<MethodCandidate, { readonly _tag: 'Inherent' }>,
  member: string,
  memberToken: Token.Token,
  declaration: DeclarationFact,
  resolution: ResolutionContext,
): ExpressionResult => {
  const receiver = synthesizeReceiver(
    subjectNode,
    subject,
    declaredParameterType(candidate.declaration.parameters.at(0)),
    declaration,
  )
  return finishCallableSection(
    node,
    Object.freeze({
      _tag: 'Resolved',
      spelling: `${candidate.ownerSpelling}.${member}`,
      token: memberToken,
      declaration: candidate.declaration,
    }),
    Object.freeze({
      facts: Object.freeze([argumentFact(declaration, node.span, receiver.fact, 0)]),
      diagnostics: receiver.diagnostics,
    }),
    analyzeCallTypeArguments(source, node, declaration, resolution),
    resolution,
    declaration,
    Object.freeze([0]),
    Object.freeze({ _tag: 'ReferencePath', member: memberToken }),
  )
}

/**
 * Analyzes `receiver.member(args)` as the statically selected member the explicit forms name,
 * or returns nothing so the ordinary callee path runs: a type qualifier, a callable field, or an
 * unknown member all keep their existing analysis.
 */
const analyzeMethodCall = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult | undefined => {
  const calleeNode = callCallee(node)
  if (calleeNode.kind !== 'FieldProjectionExpression') return undefined
  const subjectNode = calleeNode.children.find(isExpressionNode)
  const memberToken = directToken(calleeNode, 'Identifier')
  if (subjectNode === undefined || memberToken === undefined) return undefined
  if (subjectNode.kind === 'IdentifierExpression') {
    // Only a value of the enclosing scope is a receiver; a declaration or namespace qualifier
    // keeps the type-side path, and a local that shadows a type name is a value.
    const value = analyzeIdentifier(source, subjectNode, scope)
    if (
      value.fact._tag !== 'Identifier' ||
      (value.fact.reference._tag !== 'Resolved' &&
        value.fact.reference._tag !== 'ResolvedBinding' &&
        value.fact.reference._tag !== 'ResolvedPattern')
    )
      return undefined
  }
  const member = spelling(source, memberToken)
  // The receiver is argument zero, so a written borrow is as valid here as in an argument list.
  const subjectResult = analyzeExpression(
    source,
    subjectNode,
    declarations,
    declaration,
    scope,
    resolution,
    undefined,
    true,
  )
  if (subjectResult === undefined) return undefined
  const argumentList = childNode(node, 'ArgumentList')
  const argumentNodes = argumentList.children.filter(isRecursiveArgumentNode)
  const callTypeArguments = analyzeCallTypeArguments(source, node, declaration, resolution)
  const path: ReferencePathFact = Object.freeze({ _tag: 'ReferencePath', member: memberToken })
  const withSubjectDiagnostics = (finished: ExpressionResult): ExpressionResult =>
    Object.freeze({
      ...finished,
      diagnostics: Object.freeze([...subjectResult.diagnostics, ...finished.diagnostics]),
    })
  const rejected = (
    reference: Extract<CallReferenceFact, { readonly _tag: 'Missing' }>,
    diagnostic: Diagnostic.Diagnostic | undefined,
  ): ExpressionResult => {
    const written = analyzeArgumentNodes(
      source,
      node,
      argumentNodes,
      declarations,
      declaration,
      scope,
      resolution,
    )
    return withSubjectDiagnostics(
      finishDeclarationCall(
        node,
        reference,
        Object.freeze({
          facts: Object.freeze([
            argumentFact(declaration, node.span, subjectResult.fact, 0),
            ...written.facts.map((argument, ordinal) =>
              argumentFact(declaration, node.span, argument.expression, ordinal + 1),
            ),
          ]),
          diagnostics: written.diagnostics,
        }),
        callTypeArguments,
        diagnostic,
        declaration,
        resolution,
        path,
      ),
    )
  }
  const subjectType = subjectResult.type
  if (subjectType === undefined) {
    // The receiver already failed: report it once and give the call no target.
    const cause = subjectResult.diagnostics.at(0)
    if (cause === undefined) return undefined
    return rejected(
      Object.freeze({
        _tag: 'Missing',
        spelling: member,
        token: memberToken,
        cause: Diagnostic.identity(cause),
      }),
      undefined,
    )
  }
  const target = Type.isReference(subjectType) ? subjectType.target : subjectType
  // A callable field wins over every member, and an unknown name stays the field diagnostic: both
  // are the ordinary application of the projected callee, never a type-side lookup of the value.
  const applyField = (): ExpressionResult => {
    const written = analyzeArgumentNodes(
      source,
      node,
      argumentNodes,
      declarations,
      declaration,
      scope,
      resolution,
    )
    return finishCallableApplication(
      node,
      analyzeProjection(source, calleeNode, declarations, declaration, scope, resolution),
      written,
      callTypeArguments,
      undefined,
      resolution,
      declaration,
    )
  }
  if (Type.isNominal(target)) {
    const aggregate = aggregateByNominal(resolution, target)
    const field =
      aggregate === undefined
        ? undefined
        : DeclarationFacts.lookupAggregateMember(
            aggregate.fields,
            AggregateIdentity.labeled(member),
          )
    if (field?._tag === 'Resolved') return applyField()
  }
  const candidate = resolveMethodCandidate(target, member, memberToken, declaration, resolution)
  if (candidate._tag === 'Missing') return applyField()
  if (candidate._tag === 'Unavailable') return rejected(candidate.reference, candidate.diagnostic)
  if (candidate._tag === 'NoReceiver') {
    const diagnostic = Diagnostic.associatedFunctionOnValue(
      candidate.ownerSpelling,
      member,
      memberToken.span,
    )
    return rejected(
      Object.freeze({
        _tag: 'Missing',
        spelling: `${candidate.ownerSpelling}.${member}`,
        token: memberToken,
        cause: Diagnostic.identity(diagnostic),
      }),
      diagnostic,
    )
  }
  if (candidate._tag === 'AmbiguousBound') {
    const diagnostic = Diagnostic.ambiguousReceiverOperation(
      candidate.parameter,
      member,
      candidate.interfaces,
      memberToken.span,
    )
    return rejected(
      Object.freeze({
        _tag: 'Missing',
        spelling: `${candidate.parameter}.${member}`,
        token: memberToken,
        cause: Diagnostic.identity(diagnostic),
      }),
      diagnostic,
    )
  }
  // Two supplying applications are reported before the arguments are analyzed against either, so
  // no written argument can select the operation the call names.
  if (candidate._tag === 'AmbiguousConformance') {
    const diagnostic = Diagnostic.ambiguousSuppliedOperation(
      candidate.receiver,
      member,
      candidate.interfaces,
      memberToken.span,
    )
    return rejected(
      Object.freeze({
        _tag: 'Missing',
        spelling: `${candidate.receiver}.${member}`,
        token: memberToken,
        cause: Diagnostic.identity(diagnostic),
      }),
      diagnostic,
    )
  }
  const parameterTypes: ReadonlyArray<SemanticType | undefined> =
    candidate._tag === 'Inherent'
      ? candidate.declaration.parameters.map(declaredParameterType)
      : candidate.reference.parameters
  const receiver = synthesizeReceiver(subjectNode, subjectResult, parameterTypes.at(0), declaration)
  // An explicit list binds the member's own binders: the receiver already fixes the owner's, so
  // they are prepended from the receiver's type exactly as an applied qualifier would supply them.
  const typeArguments =
    candidate._tag === 'Inherent' && callTypeArguments.explicit
      ? withReceiverOwnerArguments(
          candidate.declaration,
          subjectResult.type,
          callTypeArguments,
          memberToken,
          node,
        )
      : callTypeArguments
  const written = analyzeArgumentNodes(
    source,
    node,
    argumentNodes,
    declarations,
    declaration,
    scope,
    resolution,
    parameterTypes.slice(1),
  )
  const argumentsResult: ArgumentsResult = Object.freeze({
    facts: Object.freeze([
      argumentFact(declaration, node.span, receiver.fact, 0),
      ...written.facts.map((argument, ordinal) =>
        argumentFact(declaration, node.span, argument.expression, ordinal + 1),
      ),
    ]),
    diagnostics: Object.freeze([...receiver.diagnostics, ...written.diagnostics]),
  })
  if (candidate._tag === 'Bound' || candidate._tag === 'Conformance')
    return finishInterfaceOperationCall(
      node,
      candidate.reference,
      argumentsResult,
      typeArguments,
      resolution,
      node,
      undefined,
      path,
    )
  return finishDeclarationCall(
    node,
    Object.freeze({
      _tag: 'Resolved',
      spelling: `${candidate.ownerSpelling}.${member}`,
      token: memberToken,
      declaration: candidate.declaration,
    }),
    argumentsResult,
    typeArguments,
    undefined,
    declaration,
    resolution,
    path,
  )
}

/** Whether a call's applied qualifier names an inherent member of a nominal owner. */
const appliedInherentMember = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  resolution: ResolutionContext,
): boolean => {
  const parts = appliedMemberParts(node)
  if (parts === undefined) return false
  const path = SyntaxTree.directNode(parts.owner, 'TypePath')
  const tokens =
    path === undefined ? [] : SyntaxTree.tokens(path).filter((item) => item.kind === 'Identifier')
  const token = tokens.at(0)
  if (token === undefined || tokens.length !== 1) return false
  const owner = NameResolution.lookup(resolution.scope, resolution.index, spelling(source, token))
  return (
    owner._tag === 'Resolved' &&
    NameResolution.lookupAssociated(
      resolution.index,
      owner.declaration,
      spelling(source, parts.member),
      resolution.scope.module,
    )._tag !== 'Missing'
  )
}

const appliedInterfaceOwnerDeclaration = (
  source: SourceFile.SourceFile,
  owner: SyntaxTree.Node,
  resolution: ResolutionContext,
): DeclarationFacts.ContractFact | undefined => {
  const path = SyntaxTree.directNode(owner, 'TypePath')
  const token =
    path === undefined
      ? undefined
      : SyntaxTree.tokens(path)
          .filter((item) => item.kind === 'Identifier')
          .at(-1)
  if (token === undefined) return undefined
  const lookup = NameResolution.lookup(resolution.scope, resolution.index, spelling(source, token))
  return lookup._tag === 'Resolved' &&
    (lookup.declaration._tag === 'InterfaceDeclaration' ||
      lookup.declaration._tag === 'ServiceDeclaration')
    ? lookup.declaration
    : undefined
}

const isInterfaceFact = (
  declaration: DeclarationFacts.ContractFact,
): declaration is DeclarationFacts.InterfaceFact =>
  declaration._tag === 'InterfaceDeclaration' && declaration.dependencyEligible === false

const resolveAppliedInterfaceOperationTarget = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  caller: DeclarationFact,
  resolution: ResolutionContext,
): AppliedInterfaceOperationTarget | undefined => {
  const parts = appliedMemberParts(node)
  if (parts === undefined) return undefined
  const ownerResult = resolvePatternType(source, parts.owner, resolution, caller)
  const ownerType = ownerResult.type
  const resolvedDeclaration =
    ownerType !== undefined && Type.isNominal(ownerType)
      ? DeclarationFacts.byCanonical(resolution.index, {
          _tag: 'CanonicalDeclarationId',
          module: ownerType.module,
          name: ownerType.name,
        })
      : undefined
  const ownerDeclaration =
    resolvedDeclaration?._tag === 'InterfaceDeclaration' ||
    resolvedDeclaration?._tag === 'ServiceDeclaration'
      ? resolvedDeclaration
      : appliedInterfaceOwnerDeclaration(source, parts.owner, resolution)
  if (ownerDeclaration === undefined) return undefined
  const member = spelling(source, parts.member)
  if (ownerDeclaration._tag === 'ServiceDeclaration') {
    return Object.freeze({
      _tag: 'Invalid',
      diagnostics: Diagnostic.merge(ownerResult.diagnostics, [
        Diagnostic.unknownActorOperation(
          Type.encode(ownerType ?? ownerDeclaration.self),
          member,
          parts.member.span,
        ),
      ]),
    })
  }
  if (!isInterfaceFact(ownerDeclaration)) return undefined
  if (ownerType === undefined || !Type.isNominal(ownerType)) {
    return Object.freeze({
      _tag: 'Invalid',
      diagnostics:
        ownerResult.diagnostics.length > 0
          ? ownerResult.diagnostics
          : Object.freeze([
              Diagnostic.typeArgumentInference(
                ownerDeclaration.canonical._tag === 'Canonical'
                  ? ownerDeclaration.canonical.id.name
                  : 'interface',
                parts.owner.span,
              ),
            ]),
    })
  }
  const application = DeclarationFacts.interfaceApplication(
    ownerDeclaration,
    ownerType,
    ownerDeclaration.self,
  )
  const operation = application?.operations.find(
    (candidate) =>
      candidate.declaration.name._tag === 'Present' &&
      candidate.declaration.name.spelling === member,
  )
  const contract = operation === undefined ? undefined : interfaceOperationContract(operation)
  if (application === undefined || operation === undefined || contract === undefined) {
    return Object.freeze({
      _tag: 'Invalid',
      diagnostics: Diagnostic.merge(ownerResult.diagnostics, [
        Diagnostic.unknownActorOperation(Type.encode(ownerType), member, parts.member.span),
      ]),
    })
  }
  return Object.freeze({
    _tag: 'Resolved',
    interface: ownerDeclaration,
    capability: ownerType,
    application: ownerResult.declared,
    reference: Object.freeze({
      _tag: 'ResolvedInterfaceOperation' as const,
      spelling: `${Type.encode(ownerType)}.${member}`,
      token: parts.member,
      capability: ownerType,
      provider: ownerDeclaration.self,
      operation: member,
      declaration: contract.declaration,
      interfaceContract: contract.contract,
      parameters: contract.parameters,
      result: contract.result,
    }),
    diagnostics: ownerResult.diagnostics,
  })
}

const fallbackAppliedInterfaceProviders = (
  caller: DeclarationFact,
  capability: Type.Nominal,
  operation: string,
): ReadonlyArray<Type.Type> => {
  const unique = new Map<string, Type.Type>()
  for (const parameter of caller.typeParameters) {
    for (const bound of parameter.bounds) {
      if (
        bound._tag !== 'ResolvedBound' ||
        !Type.equals(bound.application.capability, capability) ||
        !bound.application.operations.some(
          (candidate) =>
            candidate.declaration.name._tag === 'Present' &&
            candidate.declaration.name.spelling === operation,
        )
      )
        continue
      unique.set(Type.key(parameter.type), parameter.type)
    }
  }
  return Object.freeze([...unique.values()])
}

const hasAppliedInterfaceBound = (
  caller: DeclarationFact,
  provider: Type.Type,
  capability: Type.Nominal,
): boolean =>
  caller.typeParameters.some(
    (parameter) =>
      Type.equals(parameter.type, provider) &&
      parameter.bounds.some(
        (bound) =>
          bound._tag === 'ResolvedBound' && Type.equals(bound.application.capability, capability),
      ),
  )

const resolveAppliedInterfaceProvider = (
  node: SyntaxTree.Node,
  target: Extract<AppliedInterfaceOperationTarget, { readonly _tag: 'Resolved' }>,
  argumentsResult: ArgumentsResult,
  caller: DeclarationFact,
  resolution: ResolutionContext,
):
  | {
      readonly reference: Extract<
        CallReferenceFact,
        { readonly _tag: 'ResolvedInterfaceOperation' }
      >
    }
  | { readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic> } => {
  if (target.reference.parameters.length !== argumentsResult.facts.length) {
    return Object.freeze({
      diagnostics: Object.freeze([
        callArityDiagnostic(
          target.reference,
          target.reference.parameters.length,
          argumentsResult.facts.length,
          node.span,
        ),
      ]),
    })
  }
  const substitution = new Map<string, Type.GenericArgument>()
  const inferenceDiagnostics: Array<Diagnostic.Diagnostic> = []
  let providerOrigin: SourceSpan.SourceSpan | undefined
  for (const [ordinal, argument] of argumentsResult.facts.entries()) {
    const expected = target.reference.parameters.at(ordinal)
    if (expected === undefined || argument.type._tag !== 'Available') continue
    const providerKey = Type.key(target.interface.self)
    const previousProvider = substitution.get(providerKey)
    const inference = TypeInference.inferOpenGenericArguments(
      expected,
      argument.type.type,
      substitution,
    )
    if (!inference.matches) {
      const providerConflict = inference.conflicts.find((conflict) =>
        Type.equals(conflict.parameter, target.interface.self),
      )
      inferenceDiagnostics.push(
        providerConflict === undefined
          ? Diagnostic.argumentTypeMismatch(
              Type.encode(expected),
              Type.encode(argument.type.type),
              argument.syntax.span,
            )
          : Diagnostic.typeArgumentConflict(
              target.reference.spelling,
              'Self',
              Type.encodeGenericArgument(providerConflict.previous),
              Type.encodeGenericArgument(providerConflict.conflicting),
              argument.syntax.span,
              providerOrigin,
            ),
      )
    } else if (previousProvider === undefined && substitution.has(providerKey)) {
      providerOrigin = argument.syntax.span
    }
  }
  if (inferenceDiagnostics.length > 0)
    return Object.freeze({ diagnostics: Object.freeze(inferenceDiagnostics) })
  const inferredProvider = substitution.get(Type.key(target.interface.self))
  let provider: Type.Type | undefined =
    inferredProvider !== undefined && Type.isTypeArgument(inferredProvider)
      ? inferredProvider
      : undefined
  if (provider === undefined) {
    const fallback = fallbackAppliedInterfaceProviders(
      caller,
      target.capability,
      target.reference.operation,
    )
    if (fallback.length > 1) {
      return Object.freeze({
        diagnostics: Object.freeze([
          Diagnostic.ambiguousBoundOperation(
            target.reference.spelling,
            fallback.map(Type.encode),
            target.reference.token.span,
          ),
        ]),
      })
    }
    provider = fallback.at(0)
  }
  if (provider === undefined) {
    return Object.freeze({
      diagnostics: Object.freeze([
        Diagnostic.uninferredTypeParameter(
          target.reference.spelling,
          'Self',
          target.reference.token.span,
        ),
      ]),
    })
  }
  const proof = ConformanceProof.prove(resolution.index, provider, target.capability)
  if (!hasAppliedInterfaceBound(caller, provider, target.capability) && proof._tag !== 'Proved') {
    return Object.freeze({
      diagnostics: Object.freeze([
        Diagnostic.unprovenConformance(
          `${Type.encode(provider)}: ${Type.encode(target.capability)}`,
          'no coherent conformance satisfies the applied interface operation',
          Object.freeze([]),
          target.reference.token.span,
        ),
      ]),
    })
  }
  const application = DeclarationFacts.interfaceApplication(
    target.interface,
    target.capability,
    provider,
  )
  const operation = application?.operations.find(
    (candidate) =>
      candidate.declaration.name._tag === 'Present' &&
      candidate.declaration.name.spelling === target.reference.operation,
  )
  const contract = operation === undefined ? undefined : interfaceOperationContract(operation)
  if (application?.available !== true || operation === undefined || contract === undefined) {
    return Object.freeze({
      diagnostics: Object.freeze([
        Diagnostic.unprovenConformance(
          `${Type.encode(provider)}: ${Type.encode(target.capability)}`,
          'the selected interface application has no complete operation contract',
          Object.freeze([]),
          target.reference.token.span,
        ),
      ]),
    })
  }
  return Object.freeze({
    reference: Object.freeze({
      ...target.reference,
      provider,
      declaration: contract.declaration,
      interfaceContract: contract.contract,
      parameters: contract.parameters,
      result: contract.result,
    }),
  })
}

const finishAppliedInterfaceOperation = (
  node: SyntaxTree.Node,
  target: AppliedInterfaceOperationTarget,
  argumentsResult: ArgumentsResult,
  callTypeArguments: CallTypeArgumentsResult,
  caller: DeclarationFact,
  resolution: ResolutionContext,
): ExpressionResult => {
  if (target._tag === 'Invalid')
    return Object.freeze({
      fact: unavailableExpression(node),
      diagnostics: Diagnostic.merge(target.diagnostics, argumentsResult.diagnostics),
      type: undefined,
    })
  const provider = resolveAppliedInterfaceProvider(
    node,
    target,
    argumentsResult,
    caller,
    resolution,
  )
  if ('diagnostics' in provider)
    return Object.freeze({
      fact: unavailableExpression(node),
      diagnostics: Diagnostic.merge(
        target.diagnostics,
        argumentsResult.diagnostics,
        callTypeArguments.diagnostics,
        provider.diagnostics,
      ),
      type: undefined,
    })
  const completed = finishInterfaceOperationCall(
    node,
    provider.reference,
    argumentsResult,
    callTypeArguments,
    resolution,
    node,
    target.application,
  )
  return Object.freeze({
    ...completed,
    diagnostics: Diagnostic.merge(target.diagnostics, completed.diagnostics),
  })
}

export const analyzePipelineExpression = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const inputNode = pipelineInput(node)
  const target = pipelineCallable(node)
  const appliedTarget =
    target === undefined
      ? undefined
      : resolveAppliedInterfaceOperationTarget(source, target, declaration, resolution)
  if (appliedTarget !== undefined) {
    const input =
      inputNode === undefined
        ? undefined
        : analyzeExpression(
            source,
            inputNode,
            declarations,
            declaration,
            scope,
            resolution,
            appliedTarget._tag === 'Resolved'
              ? appliedTarget.reference.parameters.at(0)
              : undefined,
            true,
          )
    const inputFact = input?.fact ?? unavailableExpression(inputNode ?? node)
    return finishAppliedInterfaceOperation(
      node,
      appliedTarget,
      Object.freeze({
        facts: Object.freeze([argumentFact(declaration, node.span, inputFact, 0)]),
        diagnostics: input?.diagnostics ?? Object.freeze([]),
      }),
      Object.freeze({ explicit: false, facts: Object.freeze([]), diagnostics: Object.freeze([]) }),
      declaration,
      resolution,
    )
  }
  const callable =
    target === undefined
      ? undefined
      : analyzeExpression(source, target, declarations, declaration, scope, resolution)
  const expectedInput =
    callable?.type !== undefined && Type.isCallable(callable.type)
      ? callable.type.parameters.at(0)
      : undefined
  const input =
    inputNode === undefined
      ? undefined
      : analyzeExpression(
          source,
          inputNode,
          declarations,
          declaration,
          scope,
          resolution,
          expectedInput,
          true,
        )
  const inputFact = input?.fact ?? unavailableExpression(inputNode ?? node)
  const callableResult =
    callable ??
    Object.freeze({
      fact: unavailableExpression(target ?? node),
      diagnostics: Object.freeze([]),
      type: undefined,
    })
  return finishCallableApplication(
    node,
    callableResult,
    Object.freeze({
      facts: Object.freeze([argumentFact(declaration, node.span, inputFact, 0)]),
      diagnostics: input?.diagnostics ?? Object.freeze([]),
    }),
    Object.freeze({
      explicit: false,
      facts: Object.freeze([]),
      diagnostics: Object.freeze([]),
    }),
    Object.freeze({
      _tag: 'PipelineCallableApplication',
      left: inputFact,
      callable: callableResult.fact,
      evaluation: 'LeftThenCallable',
    }),
    resolution,
    declaration,
  )
}

export const effectExpressionAccess = (
  expression: ExpressionFact,
  index: DeclarationIndex.Index | undefined,
  assumptions: ReadonlySet<string> = new Set(),
): Type.Effect['access'] => {
  if (expression._tag === 'Move') {
    if (expression.subject.type._tag === 'Available' && Type.isEffect(expression.subject.type.type))
      return expression.subject.type.type.access
    if (
      expression.subject.type._tag === 'Available' &&
      Type.isCallable(expression.subject.type.type)
    )
      return expression.subject.type.type.mode
    if (
      expression.subject.type._tag === 'Available' &&
      index !== undefined &&
      ConformanceProof.copyType(index, expression.subject.type.type, assumptions)
    )
      return 'Shared'
    return 'Take'
  }
  if (expression._tag === 'Borrow')
    return expression.access === 'Exclusive' ? 'Exclusive' : 'Shared'
  if (expression._tag === 'Grouped')
    return effectExpressionAccess(expression.expression, index, assumptions)
  if (expression._tag === 'CallableSection') return expression.mode
  if (expression.type._tag === 'Available' && Type.isEffect(expression.type.type))
    return expression.type.type.access
  if (expression.type._tag === 'Available' && Type.isCallable(expression.type.type))
    return expression.type.type.mode
  // An owned affine value (a fresh temporary or a call result) enters the environment by
  // ownership whether or not the source spelled `move`, so running consumes it.
  if (
    expression.type._tag === 'Available' &&
    index !== undefined &&
    !Type.isReference(expression.type.type) &&
    !Type.isSlice(expression.type.type) &&
    !ConformanceProof.copyType(index, expression.type.type, assumptions)
  )
    return 'Take'
  return 'Shared'
}

export const effectCaptureAccess = (
  arguments_: ReadonlyArray<ArgumentFact>,
  index: DeclarationIndex.Index | undefined,
  assumptions: ReadonlySet<string> = new Set(),
): Type.Effect['access'] => {
  const accesses = arguments_.map((argument) =>
    effectExpressionAccess(argument.expression, index, assumptions),
  )
  return strongestEffectAccess(...accesses)
}

export const intrinsicEffectCaptureAccess = (
  operation: Intrinsic.Operation,
  arguments_: ReadonlyArray<ArgumentFact>,
  index: DeclarationIndex.Index,
  assumptions: ReadonlySet<string> = new Set(),
): Type.Effect['access'] => {
  if (
    operation.rule._tag !== 'ContractRule' ||
    operation.rule.post !== 'BindRequirement' ||
    operation.rule.providerMode !== 'Take'
  )
    return effectCaptureAccess(arguments_, index, assumptions)
  const accesses = arguments_.map((argument, ordinal) =>
    ordinal === 1
      ? ownedProviderCaptureAccess(argument.expression, index, assumptions)
      : effectExpressionAccess(argument.expression, index, assumptions),
  )
  return strongestEffectAccess(...accesses.flatMap((access) => (access === 'Copy' ? [] : [access])))
}

export const strongestEffectAccess = (
  ...accesses: ReadonlyArray<Type.Effect['access']>
): Type.Effect['access'] => {
  if (accesses.includes('Take')) {
    return 'Take'
  }
  if (accesses.includes('Exclusive')) {
    return 'Exclusive'
  }
  return 'Shared'
}

export const intrinsicOperationTarget = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): Intrinsic.Operation | undefined => {
  const identifiers = callReferenceTokens(node)
  const qualifier = identifiers.at(0)
  const member = identifiers.at(1)
  return qualifier === undefined || member === undefined
    ? undefined
    : Intrinsic.findOperation(spelling(source, qualifier), spelling(source, member))
}

export const intrinsicReference = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): IntrinsicReferenceFact => {
  const identifiers = callReferenceTokens(node)
  const actorToken = identifiers.at(0)
  const operationToken = identifiers.at(1)
  const actor =
    actorToken === undefined ? undefined : Intrinsic.findActor(spelling(source, actorToken))
  const operation =
    actor === undefined || operationToken === undefined
      ? undefined
      : Intrinsic.findOperation(actor.spelling, spelling(source, operationToken))
  return actorToken === undefined ||
    operationToken === undefined ||
    actor === undefined ||
    operation === undefined
    ? Object.freeze({ _tag: 'UnavailableIntrinsicReference', syntax: node })
    : Object.freeze({
        _tag: 'ResolvedIntrinsicReference',
        actor,
        operation,
        actorToken,
        operationToken,
      })
}

/** Finalizes ordinary lexical captures for one source Effect body. */
export const effectCaptureFacts = (
  statements: ReadonlyArray<StatementFact>,
  firstLocalBinding: number,
  index?: DeclarationIndex.Index,
  assumptions: ReadonlySet<string> = new Set(),
  options: {
    readonly localFunction?: DeclarationId
    readonly order?: 'Reference' | 'FirstUse'
    readonly onPattern?: (
      reference: PatternBindingFact,
      requested: EffectCaptureFact['access'],
      span: SourceSpan.SourceSpan,
      copy: boolean,
      expression: IdentifierExpressionFact,
    ) => void
  } = {},
): ReadonlyArray<EffectCaptureFact> => {
  const captures = new Map<string, EffectCaptureFact>()
  const rank = (access: EffectCaptureFact['access']): number => {
    if (access === 'Take') {
      return 3
    }
    if (access === 'Exclusive') {
      return 2
    }
    if (access === 'Shared') {
      return 1
    }
    return 0
  }
  const recordReference = (
    reference: BindingDeclarationFact | ParameterFact | undefined,
    requested: EffectCaptureFact['access'],
    span: SourceSpan.SourceSpan,
    copy: boolean,
    expression?: IdentifierExpressionFact,
  ): void => {
    if (reference === undefined) return
    if (reference.phase === 'Static') return
    if (
      reference._tag === 'BindingFact' &&
      reference.id.ordinal >= firstLocalBinding &&
      (options.localFunction === undefined ||
        (reference.id.function.sourceId === options.localFunction.sourceId &&
          reference.id.function.ordinal === options.localFunction.ordinal))
    )
      return
    if (
      reference._tag === 'ParameterDeclaration' &&
      options.localFunction !== undefined &&
      reference.id.function.sourceId === options.localFunction.sourceId &&
      reference.id.function.ordinal === options.localFunction.ordinal
    )
      return
    const key = `${reference._tag}:${reference.id.ordinal}`
    const access = requested !== 'Exclusive' && copy ? 'Copy' : requested
    const prior = captures.get(key)
    if (prior === undefined)
      captures.set(
        key,
        Object.freeze({
          _tag: 'EffectCapture',
          reference,
          access,
          span,
          ...(expression === undefined ? {} : { expression }),
        }),
      )
    else if (rank(access) > rank(prior.access))
      captures.set(
        key,
        Object.freeze({
          ...prior,
          access,
          ...(options.order === 'FirstUse' ? {} : { span }),
        }),
      )
  }
  const record = (fact: IdentifierExpressionFact, requested: EffectCaptureFact['access']): void => {
    let reference: DeclarationFacts.ParameterFact | BindingDeclarationFact | undefined
    if (fact.reference._tag === 'ResolvedBinding') {
      reference = fact.reference.binding
    } else if (fact.reference._tag === 'Resolved') {
      reference = fact.reference.parameter
    } else {
      reference = undefined
    }
    // A shared callable or Effect value duplicates freely (CALLABLE-002 / EFFECT-OWN-002), so a
    // body reading one captures a copy rather than a loan on the outer binding.
    const copy =
      fact.type._tag === 'Available' &&
      !Type.containsViewBorrow(fact.type.type) &&
      ((Type.isCallable(fact.type.type) && fact.type.type.mode === 'Shared') ||
        (Type.isEffect(fact.type.type) && fact.type.type.access === 'Shared') ||
        (index === undefined
          ? typeof fact.type.type === 'string'
          : ConformanceProof.copyType(index, fact.type.type, assumptions)))
    if (fact.reference._tag === 'ResolvedPattern') {
      const owner = fact.reference.binding.id.arm.match.function
      if (
        options.localFunction === undefined ||
        owner.sourceId !== options.localFunction.sourceId ||
        owner.ordinal !== options.localFunction.ordinal
      )
        options.onPattern?.(fact.reference.binding, requested, fact.syntax.span, copy, fact)
      return
    }
    recordReference(reference, requested, fact.syntax.span, copy, fact)
  }
  const expression = (
    fact: ExpressionFact,
    requested: EffectCaptureFact['access'] = 'Shared',
  ): void => {
    switch (fact._tag) {
      case 'Identifier':
        record(fact, requested)
        return
      case 'Move':
        expression(fact.subject, 'Take')
        return
      case 'Borrow':
        expression(fact.subject, fact.access === 'Exclusive' ? 'Exclusive' : 'Shared')
        return
      case 'Grouped':
        expression(fact.expression, requested)
        return
      case 'FieldProjection':
        if (fact.staticValue === undefined) expression(fact.subject, requested)
        return
      case 'IndexProjection':
        expression(fact.subject, requested)
        expression(fact.index)
        return
      case 'StructLiteral':
      case 'UnionVariant':
        for (const item of fact.initializers) expression(item.expression)
        return
      case 'ArrayLiteral':
        for (const item of fact.elements) expression(item.expression)
        return
      case 'Match':
        expression(fact.scrutinee, requested)
        for (const arm of fact.arms) {
          if (arm.guard !== undefined) expression(arm.guard)
          expression(arm.result)
        }
        return
      case 'Operator':
      case 'ShortCircuit':
      case 'Call':
        for (const argument of fact.arguments) expression(argument.expression)
        return
      case 'FunctionItem':
        return
      case 'CallableSection':
        for (const capture of fact.captures) expression(capture.expression)
        return
      case 'CallableApply':
        expression(fact.callee)
        for (const argument of fact.arguments) expression(argument.expression)
        return
      case 'PlaceReplace':
        expression(fact.destination, 'Exclusive')
        expression(fact.value)
        return
      case 'Run':
        expression(fact.subject)
        return
      case 'EffectCatch':
        expression(fact.protected)
        expression(fact.handler)
        return
      case 'EffectBindRequirement':
        expression(fact.protected)
        recordReference(
          fact.provider?.reference,
          fact.provider?.captureAccess ?? 'Shared',
          fact.syntax.span,
          false,
        )
        return
      case 'EffectBlock':
        // Constructing a nested deferred Effect still reads the environment needed to create
        // that Effect value. Bubble those dependencies into the enclosing Effect runner so a
        // parameter used only by the nested body remains available when the child is formed.
        for (const capture of fact.captures)
          recordReference(capture.reference, capture.access, capture.span, false)
        return
      case 'EnumValue':
        expression(fact.argument)
        return
      case 'CompileError':
        expression(fact.message)
        return
      case 'ReferentProjection':
        expression(fact.subject)
        return
      case 'Integer':
      case 'Duration':
      case 'Floating':
      case 'Boolean':
      case 'Character':
      case 'Constant':
      case 'StaticText':
      case 'Unit':
      case 'EnumMember':
        return
      default:
        // Exhaustive so a new expression fact kind cannot silently skip capture registration.
        fact satisfies never
        return
    }
  }
  const visit = (items: ReadonlyArray<StatementFact>): void => {
    for (const statement of items) {
      switch (statement._tag) {
        case 'UnsafeStatement':
          visit(statement.statements)
          break
        case 'BindStatement':
          expression(statement.binding.initializer)
          break
        case 'PatternBindStatement':
          expression(statement.selection.source)
          break
        case 'ExpressionStatement':
          expression(statement.expression)
          break
        case 'IfStatement':
          expression(statement.condition)
          visit(statement.taken)
          visit(statement.otherwise)
          break
        case 'IfLetStatement':
          expression(statement.selection.source)
          visit(statement.taken)
          visit(statement.otherwise)
          break
        case 'WriteStatement':
          expression(statement.destination, 'Exclusive')
          expression(statement.value)
          break
        case 'WhileStatement':
          expression(statement.condition)
          visit(statement.body)
          break
        case 'ReturnStatement':
          // A returned owned value leaves with the outcome; a returned borrow is only read.
          expression(
            statement.expression,
            statement.expression.type._tag === 'Available' &&
              Type.containsViewBorrow(statement.expression.type.type)
              ? 'Shared'
              : 'Take',
          )
          break
        case 'FailStatement':
        case 'DropStatement':
          expression(statement.expression, 'Take')
          break
        case 'BreakStatement':
        case 'ContinueStatement':
          break
      }
    }
  }
  visit(statements)
  const values = [...captures.values()]
  return Object.freeze(
    options.order === 'FirstUse'
      ? values
      : values.sort(
          (left, right) =>
            left.reference.id.ordinal - right.reference.id.ordinal ||
            left.span.start - right.span.start,
        ),
  )
}

const anonymousCaptureMode = (captures: ReadonlyArray<AnonymousCaptureFact>): Type.CallableMode => {
  if (captures.some((capture) => capture.access === 'Take')) return 'Take'
  if (captures.some((capture) => capture.access === 'Exclusive')) return 'Exclusive'
  return 'Shared'
}

const anonymousCapturedType = (capture: AnonymousCaptureFact): Type.Type | undefined => {
  if (capture.reference._tag === 'BindingFact')
    return capture.reference.inferredType._tag === 'Available'
      ? capture.reference.inferredType.type
      : undefined
  if (capture.reference._tag === 'PatternBinding')
    return capture.reference.type._tag === 'Available' ? capture.reference.type.type : undefined
  return capture.reference.declaredType._tag === 'Resolved'
    ? capture.reference.declaredType.type
    : undefined
}

const anonymousOuterScope = (authored: ReadonlyArray<ParameterFact>, outer: Scope): Scope => {
  const shadowed = new Set(
    authored.flatMap((parameter) =>
      parameter.name._tag === 'Present' ? [parameter.name.spelling] : [],
    ),
  )
  const visible = <A extends { readonly name: DeclaredName }>(values: ReadonlyArray<A>) =>
    values.filter((value) => value.name._tag !== 'Present' || !shadowed.has(value.name.spelling))
  return Object.freeze({
    parameters: Object.freeze([...visible(outer.parameters), ...authored]),
    bindings: Object.freeze(visible(outer.bindings)),
    patternBindings: Object.freeze(visible(outer.patternBindings)),
  })
}

const analyzeAnonymousCallable = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  if ((resolution.anonymousDepth ?? 0) > 0) {
    const diagnostic = Diagnostic.nestedAnonymousCallable(node.span)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'CallableSection',
        site: executableSite('CallableSiteId', resolution, node),
        reference: Object.freeze({
          _tag: 'Unavailable',
          syntax: unavailableSyntax(node, 'Identifier'),
        }),
        path: referencePath(node),
        remainingParameters: Object.freeze([]),
        captures: Object.freeze([]),
        retainedDependencies: Object.freeze([]),
        typeArguments: Object.freeze([]),
        substitution: new Map(),
        mode: 'Shared',
        type: unavailableExpressionType,
        syntax: node,
        anonymous: Object.freeze({
          functionKind:
            SyntaxTree.directToken(node, 'EffectKeyword') === undefined ? 'Ordinary' : 'Effect',
          captures: Object.freeze([]),
        }),
      }),
      diagnostics: Object.freeze([diagnostic]),
      type: undefined,
    })
  }
  const owner = resolution.executableOwner
  const function_ = resolution.executableFunction
  const site = executableSite('CallableSiteId', resolution, node)
  if (owner === undefined || function_ === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'CallableSection',
        site,
        reference: Object.freeze({
          _tag: 'Unavailable',
          syntax: unavailableSyntax(node, 'Identifier'),
        }),
        path: referencePath(node),
        remainingParameters: Object.freeze([]),
        captures: Object.freeze([]),
        retainedDependencies: Object.freeze([]),
        typeArguments: Object.freeze([]),
        substitution: new Map(),
        mode: 'Shared',
        type: unavailableExpressionType,
        syntax: node,
        anonymous: Object.freeze({
          functionKind:
            SyntaxTree.directToken(node, 'EffectKeyword') === undefined ? 'Ordinary' : 'Effect',
          captures: Object.freeze([]),
        }),
      }),
      diagnostics: Object.freeze([]),
      type: undefined,
    })
  }
  const hiddenId: DeclarationId = Object.freeze({
    _tag: 'DeclarationId',
    sourceId: source.id,
    ordinal: 0x70000000 + node.span.start,
  })
  const canonical = Hir.anonymousCallableId(owner, site)
  const collected = DeclarationCollection.collectAnonymousCallableDeclaration(
    source,
    node,
    hiddenId,
    canonical,
    declaration.typeParameters,
  )
  const nameResolution: NameResolution.Resolution = Object.freeze({
    _tag: 'NameResolution',
    modules: Object.freeze([resolution.scope]),
    diagnostics: Object.freeze([]),
  })
  const resolvers = NameResolution.makeResolvers(nameResolution, resolution.index)
  const resolveType = (fact: DeclarationFacts.DeclaredTypeFact) =>
    DeclarationResolution.resolveTypeFact(resolution.index, source.id, fact, resolvers.type)
  const typeDiagnostics: Array<Diagnostic.Diagnostic> = []
  const authoredParameters = Object.freeze(
    collected.fact.parameters.map((parameter) => {
      const resolved = resolveType(parameter.declaredType)
      typeDiagnostics.push(...resolved.diagnostics)
      return Object.freeze({ ...parameter, declaredType: resolved.fact })
    }),
  )
  const returnType = resolveType(collected.fact.returnType)
  typeDiagnostics.push(...returnType.diagnostics)
  const failureRow = DeclarationResolution.resolveFailureRow(
    source.id,
    collected.fact.failureRow,
    resolvers,
    resolution.index.modules,
  )
  const requirementRow = DeclarationResolution.resolveRequirementRow(
    source.id,
    collected.fact.requirementRow,
    resolvers,
    resolution.index.modules,
  )
  typeDiagnostics.push(...failureRow.diagnostics, ...requirementRow.diagnostics)
  const preliminaryDeclaration: DeclarationFact = Object.freeze({
    ...collected.fact,
    parameters: authoredParameters,
    parameterCount: authoredParameters.length,
    returnType: returnType.fact,
    failureRow: failureRow.fact,
    requirementRow: requirementRow.fact,
  })
  const { hiddenFunctions: _hiddenFunctions, ...preliminaryResolution } = resolution
  const preliminary = analyzeFunctionBody(
    source,
    preliminaryDeclaration,
    declarations,
    Object.freeze({ ...preliminaryResolution, anonymousDepth: 1 }),
    undefined,
    anonymousOuterScope(authoredParameters, scope),
  )
  const firstLocalBinding = 0
  const patternCaptures = new Map<string, AnonymousCaptureFact>()
  const captureRank = (access: AnonymousCaptureFact['access']): number => {
    if (access === 'Take') return 3
    if (access === 'Exclusive') return 2
    if (access === 'Shared') return 1
    return 0
  }
  const ordinaryCaptures = effectCaptureFacts(
    preliminary.fact.statements,
    firstLocalBinding,
    resolution.index,
    copyAssumptionsOf(declaration),
    Object.freeze({
      localFunction: hiddenId,
      order: 'FirstUse',
      onPattern: (reference, requested, span, copy, expression) => {
        const access = requested !== 'Exclusive' && copy ? 'Copy' : requested
        const key = `${reference.id.arm.match.function.sourceId}:${reference.id.arm.match.function.ordinal}:${reference.id.arm.match.span.start}:${reference.id.arm.ordinal}:${reference.id.ordinal}`
        const prior = patternCaptures.get(key)
        if (prior === undefined)
          patternCaptures.set(
            key,
            Object.freeze({
              _tag: 'AnonymousCapture',
              reference,
              access,
              span,
              expression,
            }),
          )
        else if (captureRank(access) > captureRank(prior.access))
          patternCaptures.set(key, Object.freeze({ ...prior, access }))
      },
    }),
  )
  const captures = Object.freeze(
    [
      ...ordinaryCaptures.flatMap((capture): ReadonlyArray<AnonymousCaptureFact> =>
        capture.expression === undefined
          ? []
          : [
              Object.freeze({
                _tag: 'AnonymousCapture',
                reference: capture.reference,
                access: capture.access,
                span: capture.span,
                expression: capture.expression,
              }),
            ],
      ),
      ...patternCaptures.values(),
    ]
      .filter(
        (capture) =>
          anonymousCapturedType(capture) !== undefined &&
          capture.reference.name._tag === 'Present' &&
          directToken(capture.expression.syntax, 'Identifier') !== undefined,
      )
      .sort((left, right) => left.span.start - right.span.start),
  )
  const captureParameters = Object.freeze(
    captures.map((capture, ordinal): ParameterFact => {
      const type = anonymousCapturedType(capture)
      if (type === undefined) throw new RangeError('Anonymous capture lost its resolved type')
      const token = directToken(capture.expression.syntax, 'Identifier')
      if (token === undefined) throw new RangeError('Anonymous capture lost its identifier token')
      const name: DeclaredName = Object.freeze({
        _tag: 'Present',
        spelling: spelling(source, token),
        token,
      })
      return Object.freeze({
        _tag: 'ParameterDeclaration',
        id: Object.freeze({
          _tag: 'ParameterId',
          function: hiddenId,
          ordinal: authoredParameters.length + ordinal,
        }),
        name,
        phase: 'Runtime',
        bindingMutability: capture.access === 'Exclusive' ? 'Mutable' : 'Immutable',
        declaredType: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token,
          syntax: capture.reference.syntax,
        }),
        syntax: capture.reference.syntax,
      })
    }),
  )
  const hiddenDeclaration: DeclarationFact = Object.freeze({
    ...preliminaryDeclaration,
    parameters: Object.freeze([...authoredParameters, ...captureParameters]),
    parameterCount: authoredParameters.length + captureParameters.length,
  })
  const hidden = analyzeFunctionBody(
    source,
    hiddenDeclaration,
    declarations,
    Object.freeze({ ...preliminaryResolution, anonymousDepth: 1 }),
    undefined,
    anonymousOuterScope(hiddenDeclaration.parameters, scope),
  )
  resolution.hiddenFunctions?.push(hidden.fact)
  const mode = anonymousCaptureMode(captures)
  let result: Type.Type | undefined =
    hiddenDeclaration.returnType._tag === 'Resolved' ? hiddenDeclaration.returnType.type : undefined
  if (result !== undefined && hiddenDeclaration.functionKind === 'Effect') {
    const effectCaptures = effectCaptureFacts(
      hidden.fact.statements,
      0,
      resolution.index,
      copyAssumptionsOf(hiddenDeclaration),
    )
    result = Type.effectWithRows(
      result,
      hiddenDeclaration.failureRow.row,
      strongestEffectAccess(
        ...effectCaptures.flatMap((capture) => (capture.access === 'Copy' ? [] : [capture.access])),
      ),
      hiddenDeclaration.requirementRow.row,
    )
  }
  const parameterTypes = authoredParameters.flatMap((parameter) =>
    parameter.declaredType._tag === 'Resolved' ? [parameter.declaredType.type] : [],
  )
  const complete = result !== undefined && parameterTypes.length === authoredParameters.length
  const callable =
    complete && result !== undefined ? Type.callable(parameterTypes, result, mode) : undefined
  const token = directToken(node, 'FnKeyword')
  const reference: CallReferenceFact =
    token === undefined
      ? Object.freeze({ _tag: 'Unavailable', syntax: unavailableSyntax(node, 'Identifier') })
      : Object.freeze({
          _tag: 'Resolved',
          spelling: canonical.name,
          token,
          declaration: hiddenDeclaration,
        })
  const environmentOwner = executableSpecializationOwner(resolution)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'CallableSection',
      site,
      reference,
      path:
        token === undefined
          ? referencePath(node)
          : Object.freeze({ _tag: 'ReferencePath', member: token }),
      remainingParameters: Object.freeze(authoredParameters.map((_, ordinal) => ordinal)),
      captures: Object.freeze(
        captures.map((capture, ordinal) => {
          const expression: ExpressionFact =
            capture.access === 'Take'
              ? Object.freeze({
                  _tag: 'Move',
                  subject: capture.expression,
                  type: capture.expression.type,
                  syntax: capture.expression.syntax,
                })
              : capture.expression
          return Object.freeze({
            _tag: 'CallableCapture',
            ordinal,
            parameterOrdinal: authoredParameters.length + ordinal,
            expression,
            access: capture.access,
          })
        }),
      ),
      retainedDependencies: Object.freeze([]),
      typeArguments: Object.freeze(declaration.typeParameters.map((parameter) => parameter.type)),
      ...(environmentOwner === undefined ? {} : { environmentOwner }),
      // The hidden body is generic over exactly the owner's parameters; naming each as itself
      // lets an owner instance's substitution specialize the section like any other call site.
      substitution: new Map(
        declaration.typeParameters.map((parameter) => [Type.key(parameter.type), parameter.type]),
      ),
      mode,
      type: callable === undefined ? unavailableExpressionType : availableExpressionType(callable),
      syntax: node,
      anonymous: Object.freeze({
        functionKind: hiddenDeclaration.functionKind,
        captures: Object.freeze(captures),
      }),
    }),
    diagnostics: Object.freeze([
      ...collected.diagnostics,
      ...typeDiagnostics,
      ...hidden.diagnostics,
    ]),
    type: callable,
  })
}

export function analyzeExpression(
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
  borrowAllowed = false,
): ExpressionResult | undefined {
  if (node.kind === 'AnonymousCallableExpression')
    return analyzeAnonymousCallable(source, node, declarations, declaration, scope, resolution)
  if (node.kind === 'UnsafeExpression') {
    const call = SyntaxTree.directNode(node, 'CallExpression')
    if (call === undefined) return undefined
    const analyzed = analyzeExpression(
      source,
      call,
      declarations,
      declaration,
      scope,
      Object.freeze({
        ...resolution,
        unsafeCallSpans: Object.freeze([...(resolution.unsafeCallSpans ?? []), call.span]),
      }),
      expected,
      borrowAllowed,
    )
    if (analyzed === undefined) return undefined
    const invokesUnsafe = (() => {
      const fact = analyzed.fact
      // The resolved contract is the one the call was checked against: a bound-typed callee
      // (`F: unsafe fn(i32) -> i32`) keeps its bound's qualifier even when a specialization later
      // selects a safe implementation.
      if (fact._tag === 'CallableApply') return fact.contract?.unsafe === true
      if (fact._tag !== 'Call') return false
      switch (fact.reference._tag) {
        case 'Resolved':
          return fact.reference.declaration.unsafe
        case 'ResolvedBuiltin':
          return fact.reference.unsafe
        case 'ResolvedIntrinsicContract':
          return fact.reference.intrinsic.unsafe
        case 'ResolvedServiceOperation':
          return fact.reference.operation.unsafe
        case 'ResolvedInterfaceOperation':
          return fact.reference.interfaceContract.unsafe
        default:
          return false
      }
    })()
    const diagnostic = invokesUnsafe
      ? undefined
      : Diagnostic.misplacedUnsafeAcknowledgement(node.span)
    return Object.freeze({
      fact: analyzed.fact,
      diagnostics: Object.freeze([
        ...analyzed.diagnostics,
        ...(diagnostic === undefined ? [] : [diagnostic]),
      ]),
      type: diagnostic === undefined ? analyzed.type : undefined,
    })
  }
  if (node.kind === 'EffectExpression') {
    const representationOwner = executableSpecializationOwner(resolution)
    const block = SyntaxTree.directNode(node, 'Block')
    if (block === undefined)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'EffectBlock',
          site: executableSite('EffectSiteId', resolution, node),
          ...(representationOwner === undefined ? {} : { representationOwner }),
          statements: Object.freeze([]),
          captures: Object.freeze([]),
          bindings: Object.freeze([]),
          regions: Object.freeze([]),
          type: unavailableExpressionType,
          syntax: node,
        }),
        diagnostics: Object.freeze([]),
        type: undefined,
      })
    const firstLocalBinding = resolution.nextBindingOrdinal?.value ?? 0
    const inheritedCallableWrites = new Set(resolution.writtenCallableBindings)
    const effectCallableWrites = new Set(inheritedCallableWrites)
    const nested: BodyContext = {
      source,
      declaration,
      declarations,
      bindings: [],
      diagnostics: [],
      regions: [],
      loops: [],
      staticIterations: [],
      resolution: Object.freeze({
        ...resolution,
        writtenCallableBindings: effectCallableWrites,
      }),
      nextBindingOrdinal: resolution.nextBindingOrdinal ?? { value: 0 },
      regionBase: 1_000_000 + node.span.start * 100,
      effectBlock: true,
    }
    const statements = analyzeStatements(nested, block, scope)
    const returned: Array<ExpressionFact> = []
    // The fail statement's analysis already validated the failure type, so every recorded
    // failure — nominal or a value-kind type parameter — belongs in the block's failure row.
    const failures: Array<Type.Type> = []
    const collectTerminals = (items: ReadonlyArray<StatementFact>): void => {
      for (const statement of items) {
        if (statement._tag === 'ReturnStatement') returned.push(statement.expression)
        else if (statement._tag === 'FailStatement' && statement.failure !== undefined)
          failures.push(statement.failure)
        else if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
          collectTerminals(statement.taken)
          collectTerminals(statement.otherwise)
        } else if (statement._tag === 'WhileStatement') collectTerminals(statement.body)
        else if (statement._tag === 'UnsafeStatement') collectTerminals(statement.statements)
      }
    }
    collectTerminals(statements)
    // Every return site contributes to the success type through the one canonical join rule;
    // disagreeing sites are diagnosed instead of silently adopting the last return's type.
    const returnedTypes = returned.flatMap((expression) =>
      expression.type._tag === 'Available' ? [expression.type.type] : [],
    )
    let success: Type.Type | undefined
    // A block whose every path ends in `fail` never produces a value: its success type is `never`
    // (EFF-007), and the recorded failures still form its failure channel.
    if (returned.length === 0 && !returnFlowOf(statements).fallsThrough) success = 'never'
    else if (returned.length > 0 && returnedTypes.length === returned.length) {
      const joined = Match.join(returnedTypes)
      if (joined._tag === 'Joined') success = joined.type
      else {
        const first = returnedTypes.at(0)
        const offender =
          first === undefined
            ? undefined
            : returned.find(
                (expression) =>
                  expression.type._tag === 'Available' && !Type.equals(expression.type.type, first),
              )
        nested.diagnostics.push(
          Diagnostic.effectBlockReturnMismatch(
            joined.types.map(Type.encode),
            offender?.syntax.span ?? node.span,
          ),
        )
      }
    }
    const captures = effectCaptureFacts(
      statements,
      firstLocalBinding,
      resolution.index,
      copyAssumptionsOf(declaration),
    )
    const reachableWrites = reachableCallableWrites(statements)
    for (const capture of captures) {
      if (
        capture.reference._tag === 'BindingFact' &&
        !inheritedCallableWrites.has(capture.reference.id.ordinal) &&
        reachableWrites.has(capture.reference.id.ordinal) &&
        capture.reference.inferredType._tag === 'Available' &&
        Type.isCallable(capture.reference.inferredType.type)
      ) {
        nested.diagnostics.push(
          Diagnostic.deferredCallableMutation(
            capture.reference.name._tag === 'Present' ? capture.reference.name.spelling : '?',
            capture.span,
          ),
        )
      }
    }
    const access = strongestEffectAccess(
      ...captures.flatMap((capture) => (capture.access === 'Copy' ? [] : [capture.access])),
    )
    const type =
      success !== undefined
        ? availableExpressionType(Type.effect(success, failures, access))
        : unavailableExpressionType
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'EffectBlock',
        site: executableSite('EffectSiteId', resolution, node),
        ...(representationOwner === undefined ? {} : { representationOwner }),
        statements,
        captures,
        bindings: Object.freeze(nested.bindings),
        regions: Object.freeze(nested.regions),
        type,
        syntax: node,
      }),
      diagnostics: Object.freeze(nested.diagnostics),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }
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
    const expectedEnum =
      expected === undefined ? undefined : enumFactByType(resolution.index, expected)
    const integer = analyzeInteger(source, node, expectedEnum === undefined ? expected : undefined)
    const mismatch =
      expectedEnum?.canonical._tag === 'Canonical' && integer.fact._tag === 'Available'
        ? Diagnostic.enumIntegerMismatch(
            Type.encode(
              Type.nominal(expectedEnum.canonical.id.module, expectedEnum.canonical.id.name),
            ),
            Type.encode(integer.fact.type),
            'IntegerToEnum',
            integerLiteralSpan(source, node, integer.fact.token),
          )
        : undefined
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Integer',
        integer: integer.fact,
        type:
          integer.fact._tag === 'Available' && mismatch === undefined
            ? availableExpressionType(integer.fact.type)
            : unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([
        ...integer.diagnostics,
        ...(mismatch === undefined ? [] : [mismatch]),
      ]),
      type:
        integer.fact._tag === 'Available' && mismatch === undefined ? integer.fact.type : undefined,
    })
  }

  if (node.kind === 'DurationLiteralExpression') {
    const duration = analyzeDuration(source, node)
    return Object.freeze({
      fact: duration.fact,
      diagnostics: duration.diagnostics,
      type: duration.fact.type._tag === 'Available' ? duration.fact.type.type : undefined,
    })
  }

  if (node.kind === 'FloatingLiteralExpression') {
    const floating = analyzeFloating(source, node, expected)
    const fact = floating.fact
    const type =
      fact._tag === 'Available' ? availableExpressionType(fact.type) : unavailableExpressionType
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Floating', floating: fact, type, syntax: node }),
      diagnostics: floating.diagnostics,
      type: fact._tag === 'Available' ? fact.type : undefined,
    })
  }

  if (node.kind === 'StaticTextLiteralExpression') {
    const token = directToken(node, 'TextLiteral') ?? directToken(node, 'ByteStringLiteral')
    const bytes =
      token === undefined ? undefined : Option.getOrUndefined(SourceFile.slice(source, token.span))
    const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
    const result =
      bytes === undefined || form === undefined
        ? undefined
        : StaticText.decode(Array.from(bytes), form)
    const diagnostic =
      result?._tag === 'Invalid'
        ? Diagnostic.invalidStaticLiteral(result.detail, node.span)
        : undefined
    const data = result?._tag === 'Decoded' ? result.data : undefined
    let type: ExpressionTypeFact
    if (data === undefined) {
      type = unavailableExpressionType
    } else {
      type = availableExpressionType(
        data.kind === 'Text' ? Type.string : Type.slice('Shared', 'u8'),
      )
    }
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'StaticText',
        ...(data === undefined ? {} : { data }),
        ...(token === undefined ? {} : { token }),
        type,
        syntax: node,
      }),
      diagnostics: Object.freeze(diagnostic === undefined ? [] : [diagnostic]),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }

  if (node.kind === 'CharacterLiteralExpression') {
    const token = directToken(node, 'CharLiteral')
    const bytes =
      token === undefined ? undefined : Option.getOrUndefined(SourceFile.slice(source, token.span))
    const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
    const result =
      bytes === undefined || form === undefined
        ? undefined
        : StaticText.decodeScalar(Array.from(bytes), form)
    const diagnostic =
      result?._tag === 'Invalid'
        ? Diagnostic.invalidStaticLiteral(result.detail, node.span)
        : undefined
    const scalar = result?._tag === 'Scalar' ? result.value : undefined
    const type = scalar === undefined ? unavailableExpressionType : availableExpressionType('char')
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Character',
        ...(scalar === undefined ? {} : { value: scalar }),
        type,
        syntax: node,
      }),
      diagnostics: Object.freeze(diagnostic === undefined ? [] : [diagnostic]),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }

  if (node.kind === 'UnitExpression') {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Unit',
        type: availableExpressionType(Type.unit),
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
      type: Type.unit,
    })
  }

  if (node.kind === 'CompileErrorExpression') {
    const messageNode = node.children.find(isExpressionNode)
    if (messageNode === undefined) return undefined
    const message = analyzeExpression(
      source,
      messageNode,
      declarations,
      declaration,
      scope,
      resolution,
      Type.string,
    )
    if (message === undefined) return undefined
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'CompileError',
        message: message.fact,
        type: availableExpressionType('never'),
        syntax: node,
      }),
      diagnostics: message.diagnostics,
      type: 'never',
    })
  }

  if (node.kind === 'IdentifierExpression') {
    const value = analyzeIdentifier(source, node, scope)
    if (
      value.fact._tag === 'Identifier' &&
      (value.fact.reference._tag === 'Resolved' ||
        value.fact.reference._tag === 'ResolvedBinding' ||
        value.fact.reference._tag === 'ResolvedPattern')
    )
      return value
    return (
      analyzeConstantReference(source, node, resolution) ??
      analyzeFunctionItem(source, node, declarations, resolution, expected) ??
      value
    )
  }

  if (node.kind === 'RunExpression') {
    const subjectNode = node.children.find(isExpressionNode)
    const subject =
      subjectNode === undefined
        ? undefined
        : analyzeExpression(source, subjectNode, declarations, declaration, scope, resolution)
    if (subject === undefined) throw new RangeError('Run expression requires one effect subject')
    let effect: Type.Effect | undefined
    if (subject.type !== undefined && Type.isEffect(subject.type)) {
      effect = subject.type
    } else if (
      subject.type !== undefined &&
      Type.isRepresented(subject.type) &&
      Type.isEffect(subject.type.contract)
    ) {
      effect = subject.type.contract
    } else {
      effect = undefined
    }
    const type =
      effect !== undefined ? availableExpressionType(effect.success) : unavailableExpressionType
    const allowed =
      declaration.functionKind === 'Effect' ? declaration.failureRow.failures : Object.freeze([])
    const unhandled =
      (effect === undefined ? [] : Type.failureMembers(effect)).filter(
        (failure) => !allowed.some((candidate) => Type.equals(candidate, failure)),
      ) ?? []
    const symbolicFailuresUnhandled =
      effect !== undefined &&
      RowAlgebra.concretize(Type.failureRowPolicy(), effect.failureRow)._tag === 'Residual' &&
      !RowAlgebra.isKnownSubset(
        Type.failureRowPolicy(),
        effect.failureRow,
        declaration.functionKind === 'Effect'
          ? declaration.failureRow.row
          : RowAlgebra.concrete(Type.failureRowPolicy(), []),
      )
    const allowedRequirements =
      declaration.functionKind === 'Effect'
        ? declaration.requirementRow.requirements
        : Object.freeze<Type.Requirement[]>([])
    const unsatisfiedRequirements =
      (effect === undefined ? [] : Type.requirementMembers(effect)).filter(
        (requirement) =>
          !allowedRequirements.some(
            (allowed) =>
              Type.equals(allowed.capability, requirement.capability) &&
              allowed.role === requirement.role &&
              (allowed.access === 'Exclusive' || allowed.access === requirement.access),
          ),
      ) ?? []
    const symbolicRequirementsUnsatisfied =
      effect !== undefined &&
      RowAlgebra.concretize(Type.requirementRowPolicy(), effect.requirementRow)._tag ===
        'Residual' &&
      !RowAlgebra.isKnownSubset(
        Type.requirementRowPolicy(),
        effect.requirementRow,
        declaration.functionKind === 'Effect'
          ? declaration.requirementRow.row
          : RowAlgebra.concrete(Type.requirementRowPolicy(), []),
      )
    const diagnostics = [...subject.diagnostics]
    if (effect === undefined && subject.type !== undefined)
      diagnostics.push(Diagnostic.runNonEffect(Type.display(subject.type), node.span))
    if (unhandled.length > 0 || symbolicFailuresUnhandled)
      diagnostics.push(
        Diagnostic.unhandledEffectFailures(
          unhandled.length > 0
            ? unhandled.map(Type.encode)
            : [
                RowAlgebra.encode(
                  Type.failureRowPolicy(),
                  effect?.failureRow ?? RowAlgebra.concrete(Type.failureRowPolicy(), []),
                  Type.encode,
                  Type.encode,
                  (member) => member.parameter.name,
                ),
              ],
          node.span,
        ),
      )
    if (unsatisfiedRequirements.length > 0 || symbolicRequirementsUnsatisfied)
      diagnostics.push(
        Diagnostic.unhandledEffectRequirements(
          unsatisfiedRequirements.length > 0
            ? unsatisfiedRequirements.map(
                (requirement) =>
                  `${requirement.access === 'Exclusive' ? '&mut ' : '&'}${Type.encode(requirement.capability)}${requirement.role === 'DefaultRole' ? '' : `@${requirement.role}`}`,
              )
            : [
                RowAlgebra.encode(
                  Type.requirementRowPolicy(),
                  effect?.requirementRow ?? RowAlgebra.concrete(Type.requirementRowPolicy(), []),
                  (requirement) =>
                    `${requirement.access === 'Exclusive' ? '&mut ' : '&'}${Type.encode(requirement.capability)}${requirement.role === 'DefaultRole' ? '' : `@${requirement.role}`}`,
                  Type.encode,
                  (member) =>
                    `${member.access === 'Exclusive' ? '&mut ' : '&'}${member.capability.name}${member.role === 'DefaultRole' ? '' : `@${member.role}`}`,
                ),
              ],
          node.span,
        ),
      )
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Run', subject: subject.fact, type, syntax: node }),
      diagnostics: Object.freeze(diagnostics),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }

  if (node.kind === 'MoveExpression') {
    const move = analyzeMove(source, node, declarations, declaration, scope, resolution)
    return Object.freeze({
      fact: move.fact,
      diagnostics: move.diagnostics,
      type: move.type,
    })
  }

  if (node.kind === 'BorrowExpression') {
    return analyzeBorrow(
      source,
      node,
      declarations,
      declaration,
      scope,
      resolution,
      expected,
      borrowAllowed,
    )
  }

  if (node.kind === 'MatchExpression') {
    return analyzeMatch(
      source,
      node,
      declarations,
      declaration,
      scope,
      resolution,
      expected,
      borrowAllowed,
    )
  }

  if (node.kind === 'AppliedMemberExpression') {
    const applied = resolveAppliedInterfaceOperationTarget(source, node, declaration, resolution)
    if (applied !== undefined) {
      const diagnostic =
        applied._tag === 'Resolved'
          ? Diagnostic.uninferredTypeParameter(
              applied.reference.spelling,
              'Self',
              applied.reference.token.span,
            )
          : undefined
      return Object.freeze({
        fact: unavailableExpression(node),
        diagnostics: Diagnostic.merge(
          applied.diagnostics,
          diagnostic === undefined ? [] : [diagnostic],
        ),
        type: undefined,
      })
    }
    return analyzeAggregateLiteral(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind === 'StructLiteralExpression') {
    return analyzeAggregateLiteral(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind === 'TupleLiteralExpression') {
    return analyzeTupleLiteral(source, node, declarations, declaration, scope, resolution, expected)
  }

  if (node.kind === 'ContextualRecordLiteralExpression') {
    return analyzeContextualRecordLiteral(
      source,
      node,
      declarations,
      declaration,
      scope,
      resolution,
      expected,
    )
  }

  if (node.kind === 'ArrayLiteralExpression') {
    return analyzeArrayLiteral(source, node, declarations, declaration, scope, resolution, expected)
  }

  if (node.kind === 'OrdinalProjectionExpression') {
    return analyzeProjection(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind === 'FieldProjectionExpression') {
    if (resolveBareUnionVariantTarget(source, node, resolution) !== undefined)
      return analyzeAggregateLiteral(source, node, declarations, declaration, scope, resolution)
    return (
      analyzeEnumMember(source, node, resolution, expected) ??
      analyzeConstantReference(source, node, resolution) ??
      analyzeFunctionItem(source, node, declarations, resolution, expected) ??
      analyzeProjection(source, node, declarations, declaration, scope, resolution)
    )
  }

  if (node.kind === 'ReferentProjectionExpression') {
    return analyzeReferentProjection(source, node, declarations, declaration, scope, resolution)
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
      borrowAllowed,
    )
  }

  if (node.kind === 'PrefixExpression' || node.kind === 'InfixExpression') {
    return analyzeOperatorExpression(
      source,
      node,
      declarations,
      declaration,
      scope,
      resolution,
      expected,
    )
  }

  if (node.kind === 'PipelineExpression' || node.kind === 'CallExpression') {
    if (node.kind === 'PipelineExpression')
      return analyzePipelineExpression(source, node, declarations, declaration, scope, resolution)
    const constructed = tupleConstructor(source, node, declarations, declaration, scope, resolution)
    if (constructed !== undefined) return constructed
  }

  if (node.kind !== 'CallExpression') return undefined

  // `receiver.member(...)` is the value-side spelling of one statically selected member; it is
  // recognized before any argument pass so the receiver and the arguments are analyzed once.
  const methodCall = analyzeMethodCall(source, node, declarations, declaration, scope, resolution)
  if (methodCall !== undefined) return methodCall

  // `Owner<Args>.member(...)` on an inherent member is the bare form with the owner arguments
  // prepended to the explicit generic prefix; it never reaches the interface-operation path.
  const appliedInherent = appliedInherentMember(source, node, resolution)
  const callTypeArguments = analyzeCallTypeArguments(
    source,
    node,
    declaration,
    resolution,
    appliedInherent ? appliedOwnerTypeArgumentNodes(node) : Object.freeze([]),
  )
  const appliedTarget = appliedInherent
    ? undefined
    : resolveAppliedInterfaceOperationTarget(source, node, declaration, resolution)
  if (appliedTarget !== undefined) {
    const argumentList = childNode(node, 'ArgumentList')
    const argumentNodes = argumentList.children.filter(isRecursiveArgumentNode)
    const argumentsResult = analyzeArgumentNodes(
      source,
      node,
      argumentNodes,
      declarations,
      declaration,
      scope,
      resolution,
      appliedTarget._tag === 'Resolved' ? appliedTarget.reference.parameters : Object.freeze([]),
    )
    return finishAppliedInterfaceOperation(
      node,
      appliedTarget,
      argumentsResult,
      callTypeArguments,
      declaration,
      resolution,
    )
  }
  const argumentsResult = analyzeArguments(
    source,
    node,
    declarations,
    declaration,
    scope,
    resolution,
    callTypeArguments,
  )

  const enumValue = analyzeEnumValueCall(source, node, argumentsResult, resolution)
  if (enumValue !== undefined) return enumValue

  const calleeNode = callCallee(node)
  const calleeResult = analyzeExpression(
    source,
    calleeNode,
    declarations,
    declaration,
    scope,
    resolution,
  )
  const resolvedValueCallee =
    calleeResult?.fact._tag === 'Identifier' &&
    (calleeResult.fact.reference._tag === 'Resolved' ||
      calleeResult.fact.reference._tag === 'ResolvedBinding' ||
      calleeResult.fact.reference._tag === 'ResolvedPattern')
  if (
    calleeResult !== undefined &&
    calleeResult.fact._tag !== 'FunctionItem' &&
    ((calleeResult.type !== undefined &&
      (Type.isCallable(calleeResult.type) ||
        (Type.isRepresented(calleeResult.type) && Type.isCallable(calleeResult.type.contract)))) ||
      (calleeResult.type !== undefined && calleeNode.kind !== 'IdentifierExpression') ||
      resolvedValueCallee ||
      calleeResult.fact._tag === 'Constant')
  ) {
    return finishCallableApplication(
      node,
      calleeResult,
      argumentsResult,
      callTypeArguments,
      undefined,
      resolution,
      declaration,
    )
  }

  const identifiers = callReferenceTokens(node)
  if (identifiers.length === 2) {
    const qualifierToken = identifiers.at(0)
    const memberToken = identifiers.at(1)
    if (qualifierToken === undefined || memberToken === undefined)
      return analyzeBuiltinCall(
        source,
        node,
        argumentsResult,
        callTypeArguments,
        resolution,
        declaration,
      )
    const qualifier = spelling(source, qualifierToken)
    const member = spelling(source, memberToken)
    if (intrinsicOperationTarget(source, node)?.rule._tag === 'PlaceRule') {
      return analyzePlaceReplace(source, node, declarations, declaration, scope, resolution)
    }
    const qualifierLookup = NameResolution.lookup(resolution.scope, resolution.index, qualifier)
    if (qualifierLookup._tag === 'Intrinsic') {
      return analyzeBuiltinCall(
        source,
        node,
        argumentsResult,
        callTypeArguments,
        resolution,
        declaration,
      )
    }
    // A declared inherent member of the qualifier wins over every other projection of that
    // spelling: contract operations, the legacy module projection, and enum members are consulted
    // only when the owner declares no such member.
    if (qualifierLookup._tag === 'Resolved') {
      const associated = NameResolution.lookupAssociated(
        resolution.index,
        qualifierLookup.declaration,
        member,
        resolution.scope.module,
      )
      if (associated._tag !== 'Missing') {
        let diagnostic: Diagnostic.Diagnostic | undefined
        let reference: CallReferenceFact
        if (associated._tag === 'Inherent') {
          diagnostic = undefined
          reference = Object.freeze({
            _tag: 'Resolved',
            spelling: `${qualifier}.${member}`,
            token: memberToken,
            declaration: associated.declaration,
          })
        } else if (associated._tag === 'Inaccessible') {
          diagnostic = Diagnostic.inaccessibleImportedMember(
            associated.declaration.canonical._tag === 'Canonical'
              ? associated.declaration.canonical.id.module
              : qualifier,
            member,
            memberToken.span,
          )
          reference = Object.freeze({
            _tag: 'Missing',
            spelling: `${qualifier}.${member}`,
            token: memberToken,
            cause: Diagnostic.identity(diagnostic),
          })
        } else {
          diagnostic = undefined
          reference = Object.freeze({
            _tag: 'Missing',
            spelling: `${qualifier}.${member}`,
            token: memberToken,
            cause: associated.cause,
          })
        }
        return finishDeclarationCall(
          node,
          reference,
          argumentsResult,
          callTypeArguments,
          diagnostic,
          declaration,
          resolution,
        )
      }
    }
    // A bound on the caller wins for interfaces and services alike (SERV-003): only an operation
    // no bound supplies reaches the ambient service path.
    if (
      qualifierLookup._tag === 'Resolved' &&
      (qualifierLookup.declaration._tag === 'InterfaceDeclaration' ||
        qualifierLookup.declaration._tag === 'ServiceDeclaration')
    ) {
      const bound = boundOperationReference(
        declaration,
        qualifierLookup.declaration,
        qualifier,
        member,
        memberToken,
      )
      if (bound?._tag === 'AmbiguousBound') {
        const ambiguous = Diagnostic.ambiguousBoundOperation(
          `${qualifier}.${member}`,
          bound.parameters,
          memberToken.span,
        )
        return finishDeclarationCall(
          node,
          Object.freeze({
            _tag: 'Missing',
            spelling: `${qualifier}.${member}`,
            token: memberToken,
            cause: Diagnostic.identity(ambiguous),
          }),
          argumentsResult,
          callTypeArguments,
          ambiguous,
          declaration,
          resolution,
        )
      }
      if (bound !== undefined)
        return finishInterfaceOperationCall(
          node,
          bound.reference,
          argumentsResult,
          callTypeArguments,
          resolution,
        )
      const concrete = concreteContractOperationReference(
        qualifierLookup.declaration,
        member,
        memberToken,
        argumentsResult,
        resolution,
      )
      if (concrete?._tag === 'Resolved')
        return finishInterfaceOperationCall(
          node,
          concrete.reference,
          argumentsResult,
          callTypeArguments,
          resolution,
        )
      if (concrete?._tag === 'Rejected')
        return finishDeclarationCall(
          node,
          Object.freeze({
            _tag: 'Missing',
            spelling: `${qualifier}.${member}`,
            token: memberToken,
            cause: Diagnostic.identity(concrete.diagnostic),
          }),
          argumentsResult,
          callTypeArguments,
          concrete.diagnostic,
          declaration,
          resolution,
        )
    }
    if (
      qualifierLookup._tag === 'Resolved' &&
      qualifierLookup.declaration._tag === 'ServiceDeclaration'
    ) {
      const operation = serviceOperation(qualifierLookup.declaration, member)
      const diagnostic =
        operation === undefined
          ? Diagnostic.unknownActorOperation(qualifier, member, memberToken.span)
          : undefined
      let reference: CallReferenceFact
      if (operation === undefined) {
        reference = Object.freeze({
          _tag: 'Missing',
          spelling: `${qualifier}.${member}`,
          token: memberToken,
          ...(diagnostic === undefined ? {} : { cause: Diagnostic.identity(diagnostic) }),
        })
      } else {
        reference = Object.freeze({
          _tag: 'ResolvedServiceOperation',
          spelling: `${qualifier}.${member}`,
          token: memberToken,
          service: qualifierLookup.declaration,
          operation,
        })
      }
      return finishDeclarationCall(
        node,
        reference,
        argumentsResult,
        callTypeArguments,
        diagnostic,
        declaration,
        resolution,
      )
    }
    if (
      qualifierLookup._tag === 'Resolved' &&
      NameResolution.isNominalOwner(qualifierLookup.declaration)
    ) {
      // The owner declares no such associated item: contract operations and inherent members are
      // the only members a nominal qualifier exposes, never its module's root declarations.
      const diagnostic = Diagnostic.unknownActorOperation(qualifier, member, memberToken.span)
      return finishDeclarationCall(
        node,
        Object.freeze({
          _tag: 'Missing',
          spelling: `${qualifier}.${member}`,
          token: memberToken,
          cause: Diagnostic.identity(diagnostic),
        }),
        argumentsResult,
        callTypeArguments,
        diagnostic,
        declaration,
        resolution,
      )
    }
    if (qualifierLookup._tag === 'Namespace') {
      const memberLookup = DeclarationFacts.lookup(resolution.index, qualifierLookup.module, member)
      const candidate = memberLookup._tag === 'Resolved' ? memberLookup.declaration : undefined
      let diagnostic: Diagnostic.Diagnostic | undefined
      if (candidate === undefined) {
        diagnostic = Diagnostic.unknownImportedMember(
          qualifierLookup.module,
          member,
          memberToken.span,
        )
      } else if (candidate.visibility === 'Private') {
        diagnostic = Diagnostic.inaccessibleImportedMember(
          qualifierLookup.module,
          member,
          memberToken.span,
        )
      } else {
        diagnostic = undefined
      }
      let reference: CallReferenceFact
      if (candidate !== undefined && candidate.visibility === 'Public') {
        reference = Object.freeze({
          _tag: 'Resolved',
          spelling: `${qualifier}.${member}`,
          token: memberToken,
          declaration: candidate,
        })
      } else {
        reference = Object.freeze({
          _tag: 'Missing',
          spelling: `${qualifier}.${member}`,
          token: memberToken,
          ...(diagnostic === undefined ? {} : { cause: Diagnostic.identity(diagnostic) }),
        })
      }
      return finishDeclarationCall(
        node,
        reference,
        argumentsResult,
        callTypeArguments,
        diagnostic,
        declaration,
        resolution,
      )
    }
    const diagnostic =
      qualifierLookup._tag === 'Missing' || qualifierLookup._tag === 'Resolved'
        ? Diagnostic.unknownActor(qualifier, qualifierToken.span)
        : undefined
    let inheritedCause: Diagnostic.Identity | undefined
    if (qualifierLookup._tag === 'Unavailable') {
      inheritedCause = qualifierLookup.cause
    } else if (qualifierLookup._tag === 'Conflict') {
      inheritedCause = qualifierLookup.conflict.cause
    } else {
      inheritedCause = undefined
    }
    const cause = diagnostic !== undefined ? Diagnostic.identity(diagnostic) : inheritedCause
    const reference: CallReferenceFact = Object.freeze({
      _tag: 'Missing',
      spelling: `${qualifier}.${member}`,
      token: qualifierToken,
      ...(cause === undefined ? {} : { cause }),
    })
    return finishDeclarationCall(
      node,
      reference,
      argumentsResult,
      callTypeArguments,
      diagnostic,
      declaration,
      resolution,
    )
  }

  const token = identifiers.at(0)
  if (token === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Call',
        reference: Object.freeze({
          _tag: 'Unavailable',
          syntax: unavailableSyntax(callCallee(node), 'Identifier'),
        }),
        path: referencePath(node),
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
  let lookup: DeclarationFacts.DeclarationLookup
  if (resolvedLookup._tag === 'Conflict') {
    lookup = Object.freeze({
      _tag: 'Ambiguous',
      spelling: tokenSpelling,
      declarations: Object.freeze(
        resolvedLookup.conflict.bindings.flatMap((binding) => {
          if (binding._tag !== 'LocalDeclaration' && binding._tag !== 'ImportedMember') return []
          const declaration = DeclarationFacts.byCanonical(resolution.index, binding.declaration)
          return declaration?._tag === 'FunctionDeclaration' ? [declaration] : []
        }),
      ),
    })
  } else if (localLookup._tag === 'Ambiguous') {
    lookup = localLookup
  } else if (
    resolvedLookup._tag === 'Resolved' &&
    resolvedLookup.declaration._tag === 'FunctionDeclaration'
  ) {
    lookup = Object.freeze({
      _tag: 'Resolved',
      spelling: tokenSpelling,
      declaration: resolvedLookup.declaration,
    })
  } else if (resolvedLookup._tag === 'Missing') {
    lookup = localLookup
  } else {
    lookup = Object.freeze({ _tag: 'Missing', spelling: tokenSpelling })
  }
  const missingDiagnostic =
    lookup._tag === 'Missing' && resolvedLookup._tag !== 'Unavailable'
      ? Diagnostic.unknownFunction(tokenSpelling, token.span)
      : undefined
  let reference: CallReferenceFact
  if (lookup._tag === 'Resolved') {
    reference = Object.freeze({
      _tag: 'Resolved',
      spelling: tokenSpelling,
      token,
      declaration: lookup.declaration,
    })
  } else if (lookup._tag === 'Ambiguous') {
    reference = Object.freeze({
      _tag: 'Ambiguous',
      spelling: tokenSpelling,
      token,
      declarations: lookup.declarations,
      ...(resolvedLookup._tag === 'Conflict' ? { cause: resolvedLookup.conflict.cause } : {}),
    })
  } else {
    let cause: ReturnType<typeof Diagnostic.identity> | undefined
    if (missingDiagnostic !== undefined) cause = Diagnostic.identity(missingDiagnostic)
    else if (resolvedLookup._tag === 'Unavailable') cause = resolvedLookup.cause
    reference = Object.freeze({
      _tag: 'Missing',
      spelling: tokenSpelling,
      token,
      ...(cause === undefined ? {} : { cause }),
    })
  }
  return finishDeclarationCall(
    node,
    reference,
    argumentsResult,
    callTypeArguments,
    missingDiagnostic,
    declaration,
    resolution,
  )
}

export const finishDeclarationCall = (
  node: SyntaxTree.Node,
  reference: CallReferenceFact,
  argumentsResult: ArgumentsResult,
  callTypeArguments: CallTypeArgumentsResult,
  diagnostic: Diagnostic.Diagnostic | undefined,
  caller: DeclarationFact,
  resolution: ResolutionContext,
  path: ReferencePathFact = referencePath(node),
): ExpressionResult => {
  if (
    reference._tag === 'Resolved' &&
    isSectionArity(reference.declaration.parameters.length, argumentsResult.facts.length)
  ) {
    const section = finishCallableSection(
      node,
      reference,
      argumentsResult,
      callTypeArguments,
      resolution,
      caller,
    )
    return diagnostic === undefined
      ? section
      : Object.freeze({
          ...section,
          diagnostics: Object.freeze([diagnostic, ...section.diagnostics]),
        })
  }
  const callContract = analyzeCallContract(
    node,
    reference,
    argumentsResult.facts,
    hasAvailableCallSyntax(node),
    callTypeArguments,
    resolution,
    caller,
  )
  const constraintDiagnostics = interfaceConstraintDiagnostics(
    reference,
    callContract,
    resolution.index,
    caller,
    node.span,
  )
  const callable = sourceCallable(reference)
  const unsafeDiagnostic = unsafeCallDiagnostic(
    callable?.unsafe === true,
    'spelling' in reference ? reference.spelling : 'callable',
    node,
    resolution,
  )
  const phaseDiagnostics =
    reference._tag !== 'Resolved'
      ? Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([])
      : Object.freeze(
          reference.declaration.parameters.flatMap((parameter) => {
            if (parameter.phase !== 'Runtime') return []
            const argument = argumentsResult.facts.at(parameter.id.ordinal)
            return argument?.type._tag === 'Available' &&
              Type.containsStaticPhaseOnly(argument.type.type)
              ? [
                  Diagnostic.staticPhaseViolation(
                    'runtime call argument with a phase-only type',
                    resolution.staticContext?.environment.target ?? 'unselected-target',
                    Object.freeze([]),
                    argument.syntax.span,
                  ),
                ]
              : []
          }),
        )
  const staticArguments = (() => {
    if (
      reference._tag !== 'Resolved' ||
      resolution.staticContext === undefined ||
      resolution.deferStaticCalls === true
    )
      return Object.freeze({
        values: Object.freeze<
          ReadonlyArray<{ readonly parameter: ParameterFact; readonly value: StaticValue.Value }>
        >([]),
        diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
      })
    const values: Array<{ readonly parameter: ParameterFact; readonly value: StaticValue.Value }> =
      []
    const diagnostics: Array<Diagnostic.Diagnostic> = []
    for (const parameter of reference.declaration.parameters) {
      if (parameter.phase !== 'Static') continue
      const argument = argumentsResult.facts.at(parameter.id.ordinal)
      if (argument === undefined) continue
      const evaluated = StaticEvaluation.evaluateFact(argument.expression, resolution.staticContext)
      if (evaluated._tag === 'Complete') {
        const textOrigin =
          StaticEvaluation.staticTextOrigin(argument.expression, resolution.staticContext) ??
          (evaluated.value._tag === 'TextValue' ? evaluated.value.origin : undefined)
        values.push(
          Object.freeze({
            parameter,
            value: evaluated.value,
            ...(textOrigin === undefined ? {} : { textOrigin }),
          }),
        )
      } else {
        diagnostics.push(
          StaticEvaluation.diagnostic(
            evaluated.failure,
            resolution.staticContext.environment.target,
          ),
        )
      }
    }
    return Object.freeze({ values: Object.freeze(values), diagnostics: Object.freeze(diagnostics) })
  })()
  const expressionType =
    hasAvailableCallSyntax(node) &&
    callable !== undefined &&
    callable.returnType._tag === 'Resolved' &&
    callContract.fact._tag === 'Compatible' &&
    constraintDiagnostics.length === 0 &&
    unsafeDiagnostic === undefined
      ? availableExpressionType(
          (() => {
            const substitution =
              callContract.fact._tag === 'Compatible'
                ? callContract.fact.substitution
                : new Map<string, Type.GenericArgument>()
            const success = Type.substitute(callable.returnType.type, substitution)
            // An ordinary function's declared result is its contract; the Effect it returns
            // carries the run access the declaration spelled, not one derived from the call.
            if (callable.functionKind !== 'Effect') return success
            return Type.effectWithRows(
              success,
              Type.substituteFailureRow(callable.failureRow.row, substitution),
              effectCaptureAccess(
                argumentsResult.facts,
                resolution.index,
                copyAssumptionsOf(caller),
              ),
              Type.substituteRequirementsRow(callable.requirementRow.row, substitution),
            )
          })(),
        )
      : unavailableExpressionType
  const fact: Extract<ExpressionFact, { readonly _tag: 'Call' }> = Object.freeze({
    _tag: 'Call',
    reference,
    path,
    typeArguments: callTypeArguments.facts,
    arguments: argumentsResult.facts,
    staticArguments: staticArguments.values,
    mappings: callContract.mappings,
    contract: callContract.fact,
    type: expressionType,
    syntax: node,
  })
  const staticContext = resolution.staticContext
  const staticResult =
    reference._tag === 'Resolved' &&
    reference.declaration.phase === 'Static' &&
    staticContext !== undefined &&
    resolution.deferStaticCalls !== true &&
    expressionType._tag === 'Available'
      ? StaticEvaluation.evaluateFact(fact, staticContext)
      : undefined
  const staticDiagnostics =
    staticResult?._tag === 'Failed' && staticContext !== undefined
      ? [StaticEvaluation.diagnostic(staticResult.failure, staticContext.environment.target)]
      : []
  let resolvedFact = fact
  if (staticResult?._tag === 'Complete') {
    const staticTextSpan = staticContext?.expressionSpans.get(fact)
    const staticTextOrigin = staticContext?.expressionOrigins.get(fact)
    resolvedFact = Object.freeze({
      ...fact,
      staticValue: staticResult.value,
      ...(staticTextSpan === undefined ? {} : { staticTextSpan }),
      ...(staticTextOrigin === undefined ? {} : { staticTextOrigin }),
    })
  } else if (staticResult?._tag === 'Failed')
    resolvedFact = Object.freeze({ ...fact, staticFailure: staticResult.failure })
  return Object.freeze({
    fact: resolvedFact,
    diagnostics: Object.freeze([
      ...(diagnostic === undefined ? [] : [diagnostic]),
      ...argumentsResult.diagnostics,
      ...callTypeArguments.diagnostics,
      ...callContract.diagnostics,
      ...constraintDiagnostics,
      ...phaseDiagnostics,
      ...staticArguments.diagnostics,
      ...staticDiagnostics,
      ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
    ]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

/** Finishes one statically selected interface operation call. */
export const finishInterfaceOperationCall = (
  node: SyntaxTree.Node,
  reference: Extract<CallReferenceFact, { readonly _tag: 'ResolvedInterfaceOperation' }>,
  argumentsResult: ArgumentsResult,
  callTypeArguments: CallTypeArgumentsResult,
  resolution: ResolutionContext,
  effectSiteNode: SyntaxTree.Node = node,
  interfaceApplication?: DeclarationFacts.DeclaredTypeFact,
  path: ReferencePathFact = referencePath(node),
): ExpressionResult => {
  const syntaxAvailable =
    node.kind === 'PipelineExpression' ? isAvailableSyntax(node) : hasAvailableCallSyntax(node)
  const typeArgumentDiagnostic =
    callTypeArguments.explicit && callTypeArguments.facts.length > 0
      ? Diagnostic.typeArgumentArity(
          reference.spelling,
          0,
          callTypeArguments.facts.length,
          node.span,
        )
      : undefined
  const callContract = analyzeCallContract(node, reference, argumentsResult.facts, syntaxAvailable)
  const unsafeDiagnostic = unsafeCallDiagnostic(
    reference.interfaceContract.unsafe,
    reference.spelling,
    node,
    resolution,
  )
  const expressionType =
    syntaxAvailable &&
    typeArgumentDiagnostic === undefined &&
    callContract.fact._tag === 'Compatible' &&
    unsafeDiagnostic === undefined
      ? availableExpressionType(reference.result)
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Call',
      reference,
      path,
      ...(interfaceApplication === undefined ? {} : { interfaceApplication }),
      typeArguments: callTypeArguments.facts,
      arguments: argumentsResult.facts,
      mappings: callContract.mappings,
      contract: callContract.fact,
      ...(expressionType._tag === 'Available' &&
      reference.interfaceContract.functionKind === 'Effect'
        ? { witnessEffectSite: executableSite('EffectSiteId', resolution, effectSiteNode) }
        : {}),
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      ...(typeArgumentDiagnostic === undefined ? [] : [typeArgumentDiagnostic]),
      ...argumentsResult.diagnostics,
      ...callTypeArguments.diagnostics,
      ...callContract.diagnostics,
      ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
    ]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

export const statementExpressionNode = (statement: SyntaxTree.Node): SyntaxTree.Node => {
  const expression = statement.children.find((element): element is SyntaxTree.Node =>
    isExpressionNode(element),
  )
  if (expression === undefined) {
    throw new RangeError('Semantic analysis expected a statement expression')
  }
  return expression
}

export const compareDiagnostics = (
  left: Diagnostic.Diagnostic,
  right: Diagnostic.Diagnostic,
): number => {
  const spanOrder = left.span.start - right.span.start || left.span.end - right.span.end
  if (spanOrder !== 0) return spanOrder
  if (left.code < right.code) return -1
  if (left.code > right.code) return 1
  return 0
}

export interface FunctionAnalysis {
  readonly fact: FunctionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export const bindingName = (
  source: SourceFile.SourceFile,
  statement: SyntaxTree.Node,
): DeclarationFacts.DeclaredName => {
  const token = directToken(statement, 'Identifier')
  return token === undefined
    ? Object.freeze({
        _tag: 'Unavailable' as const,
        syntax: unavailableSyntax(statement, 'Identifier'),
      })
    : Object.freeze({ _tag: 'Present' as const, spelling: spelling(source, token), token })
}

export const scopeSpanFor = (
  scope: Scope,
  spellingText: string,
): SourceSpan.SourceSpan | undefined => {
  const binding = scope.bindings.findLast(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === spellingText,
  )
  if (binding?.name._tag === 'Present') return binding.name.token.span
  const patternBinding = scope.patternBindings.findLast(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === spellingText,
  )
  if (patternBinding?.name._tag === 'Present') return patternBinding.name.token.span
  const parameter = scope.parameters.find(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === spellingText,
  )
  return parameter?.name._tag === 'Present' ? parameter.name.token.span : undefined
}

export interface StaticAnalysisContext {
  readonly environment: StaticEvaluation.TargetEnvironment
  readonly typeSubstitution?: Type.Substitution
  readonly values: Map<string, StaticValue.Value>
  readonly valueSpans: Map<string, SourceSpan.SourceSpan>
  readonly valueOrigins: Map<string, StaticEvaluation.TextOrigin>
  readonly expressionSpans: Map<ExpressionFact, SourceSpan.SourceSpan>
  readonly expressionOrigins: Map<ExpressionFact, StaticEvaluation.TextOrigin>
  readonly returnedTextSpan?: { value: SourceSpan.SourceSpan | undefined }
  readonly trace: StaticEvaluation.Trace
  readonly call: StaticEvaluation.FactEvaluationContext['call']
  readonly reflect: StaticEvaluation.FactEvaluationContext['reflect']
  readonly constant?: NonNullable<StaticEvaluation.FactEvaluationContext['constant']>
  /** Charges one fully analyzed iteration before any of its residual facts are published. */
  readonly chargeStaticIteration?: (
    trace: StaticEvaluation.Trace,
    residualNodes: number,
  ) => StaticEvaluation.StaticFailure | undefined
  /** Residual nodes already charged incrementally by successful static iterations. */
  readonly chargedStaticIterationNodes?: { value: number }
}

export interface BodyContext {
  readonly source: SourceFile.SourceFile
  readonly declaration: DeclarationFact
  readonly declarations: ReadonlyArray<DeclarationFact>
  readonly bindings: Array<BindingDeclarationFact>
  readonly diagnostics: Array<Diagnostic.Diagnostic>
  readonly regions: Array<Hir.RegionId>
  readonly loops: Array<Hir.LoopId>
  readonly staticIterations: Array<StaticIterationFact>
  readonly resolution: ResolutionContext
  readonly nextBindingOrdinal: { value: number }
  readonly regionBase?: number
  readonly effectBlock?: true
  readonly staticContext?: StaticAnalysisContext
  readonly returnType?: Type.Type
}

export interface ResolutionContext {
  readonly scope: NameResolution.ModuleScope
  readonly index: DeclarationIndex.Index
  /** Occurrence-generated aggregates are semantic facts, deliberately outside lexical lookup. */
  readonly generatedAggregates?: Map<string, DeclarationFacts.StructFact>
  readonly unsafeSpans?: ReadonlyArray<SourceSpan.SourceSpan>
  /** Exact direct-call spans acknowledged by the expression form `unsafe call(...)`. */
  readonly unsafeCallSpans?: ReadonlyArray<SourceSpan.SourceSpan>
  readonly nextBindingOrdinal?: { value: number }
  readonly executableFunction?: DeclarationId
  readonly executableOwner?: DeclarationFacts.CanonicalId
  readonly executableSites?: ReadonlyMap<SyntaxTree.Node, number>
  /** Mutable callable bindings whose authored initializer is no longer their exact runtime value. */
  readonly writtenCallableBindings?: Set<number>
  readonly staticContext?: StaticAnalysisContext
  /** Static calls under ordinary control in a static function execute only after branch selection. */
  readonly deferStaticCalls?: true
  /** Hidden anonymous bodies discovered while analyzing the current source declaration. */
  readonly hiddenFunctions?: Array<FunctionFact>
  /** Anonymous bodies are parsed recursively but only the outermost body is admitted in slice one. */
  readonly anonymousDepth?: number
}

const aggregateKey = (nominal: Type.Nominal): string => `${nominal.module}:${nominal.name}`

export const aggregateByNominal = (
  resolution: ResolutionContext,
  nominal: Type.Nominal,
): DeclarationFacts.StructFact | undefined => {
  const generated = resolution.generatedAggregates?.get(aggregateKey(nominal))
  if (generated !== undefined) return generated
  const member = DeclarationFacts.byCanonical(resolution.index, {
    _tag: 'CanonicalDeclarationId',
    module: nominal.module,
    name: nominal.name,
  })
  return member?._tag === 'StructDeclaration' ? member : undefined
}

export const unsafeCallAuthorized = (
  resolution: ResolutionContext | undefined,
  call: SyntaxTree.Node,
): boolean =>
  resolution !== undefined &&
  ((resolution.unsafeSpans ?? []).some(
    (span) =>
      span.sourceId === call.span.sourceId &&
      span.start <= call.span.start &&
      span.end >= call.span.end,
  ) ||
    (resolution.unsafeCallSpans ?? []).some(
      (span) =>
        span.sourceId === call.span.sourceId &&
        span.start === call.span.start &&
        span.end === call.span.end,
    ))
