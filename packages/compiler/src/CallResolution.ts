import * as BodyLifetime from './BodyLifetime.js'
import * as Lifetime from './Lifetime.js'
import * as CallableContract from './CallableContract.js'
import * as ConformanceGoal from './ConformanceGoal.js'
import * as ConformanceProof from './ConformanceProof.js'
import * as Constraint from './Constraint.js'
import * as DeclarationCollection from './DeclarationCollection.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import * as DeclarationLifetime from './DeclarationLifetime.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as DeclarationResolution from './DeclarationResolution.js'
import * as Diagnostic from './Diagnostic.js'
import type {
  ArgumentFact,
  ArgumentMappingFact,
  ArgumentsResult,
  CallableApplyExpressionFact,
  CallableCaptureFact,
  CallableSectionExpressionFact,
  CallContractFact,
  CallReferenceFact,
  DeclarationFact,
  ExpressionFact,
  ExpressionResult,
  InferredProviderSelector,
  ReferencePathFact,
  SemanticType,
  TypeArgumentFact,
} from './Elaboration.js'
import {
  argumentFact,
  availableExpressionType,
  callCallee,
  callReferenceTokens,
  childNode,
  contextualIntegerCompatible,
  isAvailableSyntax,
  isExpressionNode,
  isRecursiveArgumentNode,
  lookupDeclaration,
  pipelineCallable,
  referencePath,
  spelling,
  typesCompatible,
  unavailableExpressionType,
  unavailableSyntax,
  unionConversionDiagnostic,
} from './Elaboration.js'
import type { ResolutionContext, Scope } from './ExpressionAnalysis.js'
import {
  analyzeExpression,
  effectBindingProvider,
  effectCaptureAccess,
  effectExpressionAccess,
  representationOfExpression,
  sectionIntrinsicReference,
  strongestEffectAccess,
  unavailableExpression,
} from './ExpressionAnalysis.js'
import type * as Hir from './Hir.js'
import * as Intrinsic from './Intrinsic.js'
import * as TypeInference from './internal/TypeInference.js'
import * as NameResolution from './NameResolution.js'
import * as ProviderSelection from './ProviderSelection.js'
import * as ResolutionWork from './ResolutionWork.js'
import * as RequirementRow from './RequirementRow.js'
import * as RowAlgebra from './RowAlgebra.js'
import type * as SourceFile from './SourceFile.js'
import type * as SourceSpan from './SourceSpan.js'
import { unsafeCallDiagnostic } from './StatementAnalysis.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'
import * as TypeCompatibility from './TypeCompatibility.js'

export const analyzeArgumentNodes = (
  source: SourceFile.SourceFile,
  site: SyntaxTree.Node,
  nodes: ReadonlyArray<SyntaxTree.Node>,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expectedTypes: ReadonlyArray<SemanticType | undefined> = Object.freeze([]),
): ArgumentsResult => {
  const inferred = new Map<string, Type.GenericArgument>()
  const lifetimes = selectedCallLifetimes(site, [], resolution)
  const analyzed = nodes.flatMap((element, ordinal): ReadonlyArray<ExpressionResult> => {
    const pattern = expectedTypes.at(ordinal)
    const expected = pattern === undefined ? undefined : Type.substitute(pattern, inferred)
    const result = analyzeExpression(
      source,
      element,
      declarations,
      declaration,
      scope,
      resolution,
      expected,
    )
    if (pattern !== undefined && result?.type !== undefined) {
      const attempt = new Map(inferred)
      if (TypeInference.infer(pattern, result.type, attempt, lifetimes.inference))
        commitSpecialization(inferred, attempt)
    }
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

export function analyzeArguments(
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  callTypeArguments?: CallTypeArgumentsResult,
): ArgumentsResult {
  const argumentList = childNode(call, 'ArgumentList')
  const argumentNodes = argumentList.children.filter(isRecursiveArgumentNode)
  const identifiers = callReferenceTokens(call)
  const first = identifiers.at(0)
  const second = identifiers.at(1)
  let target: SourceCallable | undefined
  let builtinParameters: ReadonlyArray<SemanticType> = Object.freeze([])
  let builtinTypeParameters: ReadonlyArray<Type.Parameter> = Object.freeze([])
  let builtinLifetimes: ReadonlyArray<Lifetime.Bound> = Object.freeze([])
  let boundParameters: ReadonlyArray<SemanticType> = Object.freeze([])
  if (first !== undefined && second === undefined) {
    const name = spelling(source, first)
    const resolved = NameResolution.lookup(resolution.scope, resolution.index, name)
    const local = lookupDeclaration(declarations, name)
    if (resolved._tag === 'Resolved' && resolved.declaration._tag === 'FunctionDeclaration') {
      target = resolved.declaration
    } else if (local._tag === 'Resolved') {
      target = local.declaration
    }
  } else if (first !== undefined && second !== undefined) {
    const qualifierSpelling = spelling(source, first)
    const memberSpelling = spelling(source, second)
    const qualifier = NameResolution.lookup(resolution.scope, resolution.index, qualifierSpelling)
    const associated =
      qualifier._tag === 'Resolved'
        ? NameResolution.lookupAssociated(
            resolution.index,
            qualifier.declaration,
            memberSpelling,
            resolution.scope.module,
          )
        : undefined
    if (associated?._tag === 'Inherent') {
      target = associated.declaration
    } else if (qualifier._tag === 'Intrinsic') {
      const builtin = builtinSignature(qualifierSpelling, memberSpelling)
      const intrinsic = Intrinsic.findOperation(qualifierSpelling, memberSpelling)
      const contract = intrinsic?.rule._tag === 'ContractRule' ? intrinsic.rule.contract : undefined
      builtinParameters =
        builtin?.parameters ?? contract?.parameters.map((parameter) => parameter.type) ?? []
      builtinTypeParameters = builtin?.typeParameters ?? contract?.binders ?? []
      const result = builtin?.result ?? contract?.result
      builtinLifetimes = freeLifetimeBinders([
        ...builtinParameters,
        ...(result === undefined ? [] : [result]),
      ])
    } else if (qualifier._tag === 'Namespace') {
      const member = DeclarationFacts.lookup(resolution.index, qualifier.module, memberSpelling)
      target =
        member._tag === 'Resolved' && member.declaration._tag === 'FunctionDeclaration'
          ? member.declaration
          : undefined
    } else if (
      qualifier._tag === 'Resolved' &&
      qualifier.declaration._tag === 'ServiceDeclaration'
    ) {
      target = serviceOperation(qualifier.declaration, memberSpelling)
    } else if (
      qualifier._tag === 'Resolved' &&
      qualifier.declaration._tag === 'InterfaceDeclaration'
    ) {
      const memberToken = second
      const bound = boundOperationReference(
        declaration,
        qualifier.declaration,
        qualifierSpelling,
        memberSpelling,
        memberToken,
      )
      if (bound?._tag === 'BoundOperation')
        boundParameters = instantiateInterfaceReference(
          bound.reference,
          call,
          resolution,
        ).parameters
    }
  }
  const declaredTypeParameters =
    target?.typeParameters.map((parameter) => parameter.type) ?? Object.freeze([])
  const explicitTypes = callTypeArguments?.types
  const explicitBuiltinSubstitution =
    callTypeArguments?.explicit === true &&
    explicitTypes !== undefined &&
    explicitTypes.length <= builtinTypeParameters.length
      ? TypeInference.prefixSubstitution(builtinTypeParameters, explicitTypes)
      : undefined
  const builtinSubstitution = selectedCallLifetimes(
    call,
    builtinLifetimes,
    resolution,
    explicitBuiltinSubstitution,
  ).substitution
  // An explicit prefix is context for the value arguments just as a complete list is: the
  // parameters it binds become concrete expected types, and the ones it leaves open stay symbolic
  // exactly as they are when nothing was written.
  const explicitSubstitution =
    callTypeArguments?.explicit === true && explicitTypes !== undefined
      ? TypeInference.prefixSubstitution(declaredTypeParameters, explicitTypes)
      : undefined
  const substitution =
    target === undefined
      ? explicitSubstitution
      : selectedCallLifetimes(
          call,
          DeclarationFacts.executableLifetimes(target).lifetimeBinders,
          resolution,
          explicitSubstitution,
        ).substitution
  let selectedParameters: ReadonlyArray<SemanticType | undefined>
  if (boundParameters.length > 0) selectedParameters = boundParameters
  else if (builtinParameters.length > 0) {
    const offset = isSectionArity(builtinParameters.length, argumentNodes.length)
      ? builtinParameters.length - argumentNodes.length
      : 0
    selectedParameters = builtinParameters
      .slice(offset)
      .map((parameter) => Type.substitute(parameter, builtinSubstitution ?? new Map()))
  } else {
    const offset =
      target !== undefined && isSectionArity(target.parameters.length, argumentNodes.length)
        ? target.parameters.length - argumentNodes.length
        : 0
    selectedParameters = (target?.parameters ?? [])
      .slice(offset)
      .map((parameter) =>
        parameter.declaredType._tag === 'Resolved'
          ? Type.substitute(parameter.declaredType.type, substitution ?? new Map())
          : undefined,
      )
  }
  const expectedTypes = Object.freeze(selectedParameters)
  return analyzeArgumentNodes(
    source,
    call,
    argumentNodes,
    declarations,
    declaration,
    scope,
    resolution,
    expectedTypes,
  )
}

export interface CallContractResult {
  readonly mappings: ReadonlyArray<ArgumentMappingFact>
  readonly fact: CallContractFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export interface CallTypeArgumentsResult {
  readonly explicit: boolean
  readonly facts: ReadonlyArray<TypeArgumentFact>
  readonly types?: ReadonlyArray<Type.GenericArgument>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const isTypeArgumentNode = (element: SyntaxTree.Element): element is SyntaxTree.Node =>
  SyntaxTree.isNode(element) &&
  (element.kind === 'LifetimeType' ||
    element.kind === 'RequirementSelector' ||
    element.kind === 'TypePath' ||
    element.kind === 'AppliedType' ||
    element.kind === 'FixedArrayType' ||
    element.kind === 'SliceType' ||
    element.kind === 'ReferenceType' ||
    element.kind === 'PointerType' ||
    element.kind === 'CallableType' ||
    element.kind === 'ForeignFunctionType' ||
    element.kind === 'ParenthesizedType' ||
    element.kind === 'UnionType')

/**
 * The type arguments an applied qualifier supplies ahead of the call's own list: for an inherent
 * member `Option<i32>.map<i64>(...)` the owner's `<i32>` binds the owner binders, so the complete
 * explicit prefix reads as `Option.map<i32, i64>`.
 */
export const appliedOwnerTypeArgumentNodes = (
  call: SyntaxTree.Node,
): ReadonlyArray<SyntaxTree.Node> => {
  const callee = callCallee(call)
  if (callee.kind !== 'AppliedMemberExpression') return Object.freeze([])
  const selector = SyntaxTree.directNode(callee, 'AppliedMemberSelector')
  const owner = selector === undefined ? undefined : SyntaxTree.directNode(selector, 'AppliedType')
  const list = owner === undefined ? undefined : SyntaxTree.directNode(owner, 'TypeArgumentList')
  return list === undefined
    ? Object.freeze([])
    : Object.freeze(list.children.filter(isTypeArgumentNode))
}

export const analyzeCallTypeArguments = (
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  caller: DeclarationFact,
  resolution: ResolutionContext,
  leading: ReadonlyArray<SyntaxTree.Node> = Object.freeze([]),
): CallTypeArgumentsResult => {
  const list = SyntaxTree.directNode(call, 'CallTypeArgumentList')
  if (list === undefined && leading.length === 0) {
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
  const nodes = [
    ...leading,
    ...(list === undefined ? [] : list.children.filter(isTypeArgumentNode)),
  ]
  const analyzed = nodes.map((node, ordinal) => {
    const selectorNodes =
      node.kind === 'RequirementSelector'
        ? node.children.filter(SyntaxTree.isNode)
        : Object.freeze<ReadonlyArray<SyntaxTree.Node>>([])
    const argumentNode = selectorNodes.at(0) ?? node
    const roleNode = selectorNodes.at(1)
    const directToken =
      argumentNode.kind === 'TypePath'
        ? SyntaxTree.tokens(argumentNode).find((token) => token.kind === 'Identifier')
        : undefined
    const directParameter =
      directToken === undefined ? undefined : environment.get(spelling(source, directToken))
    if (
      directToken !== undefined &&
      directParameter !== undefined &&
      directParameter.kind !== 'Value'
    )
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'TypeArgument' as const,
          ordinal,
          syntax: node,
          declared: Object.freeze({
            _tag: 'Resolved' as const,
            type: directParameter,
            spelling: directParameter.name,
            token: directToken,
            syntax: node,
          }),
          type: directParameter,
        }),
        diagnostics: Object.freeze([]),
      })
    const roleSegments =
      roleNode?.kind === 'TypePath'
        ? SyntaxTree.tokens(roleNode)
            .filter((token) => token.kind === 'Identifier')
            .map((token) => Object.freeze({ spelling: spelling(source, token), token }))
        : []
    const rolePath =
      roleNode?.kind === 'TypePath' && roleSegments.length > 0
        ? Object.freeze({
            _tag: 'TypePath' as const,
            spelling: roleSegments.map((segment) => segment.spelling).join('.'),
            segments: Object.freeze(roleSegments),
            syntax: roleNode,
          })
        : undefined
    const roleResolution =
      rolePath === undefined
        ? undefined
        : NameResolution.resolveItem(nameResolution, resolution.index, source.id, rolePath)
    const roleDeclaration =
      roleResolution?._tag === 'Resolved' && roleResolution.declaration._tag === 'RoleDeclaration'
        ? roleResolution.declaration
        : undefined
    const requirementRole =
      roleDeclaration?.canonical._tag === 'Canonical'
        ? RequirementRow.declaredRole(
            roleDeclaration.canonical.id.module,
            roleDeclaration.canonical.id.name,
          )
        : undefined
    const roleDiagnostics =
      rolePath === undefined || requirementRole !== undefined
        ? Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([])
        : Object.freeze([
            Diagnostic.invalidRequirementType(`role ${rolePath.spelling}`, rolePath.syntax.span),
          ])
    const body = resolution.bodyLifetimes
    const lifetimeContext =
      body === undefined
        ? undefined
        : DeclarationLifetime.forHeader(
            source,
            body.owner,
            argumentNode,
            environment,
            body,
            (path) => {
              const rawPath = DeclarationCollection.analyzeDeclaredType(
                source,
                path,
                environment,
                true,
              ).fact
              if (rawPath._tag !== 'Unresolved' || rawPath.path === undefined) return undefined
              const target = NameResolution.resolveType(
                nameResolution,
                resolution.index,
                source.id,
                rawPath.path,
              ).fact
              return target._tag === 'Resolved' && Type.isNominal(target.type)
                ? DeclarationResolution.memberByNominal(
                    resolution.index.modules,
                    target.type,
                  )?.typeParameters.map((parameter) => parameter.type)
                : undefined
            },
          )
    const raw = DeclarationCollection.analyzeDeclaredType(
      source,
      argumentNode,
      environment,
      true,
      lifetimeContext,
    )
    const resolved = DeclarationResolution.resolveTypeFact(
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
        ...(requirementRole === undefined ? {} : { requirementRole }),
        ...((resolved.fact._tag === 'Resolved' || resolved.fact._tag === 'Lifetime') &&
        roleDiagnostics.length === 0
          ? {
              type: resolved.fact._tag === 'Lifetime' ? resolved.fact.lifetime : resolved.fact.type,
            }
          : {}),
      }),
      diagnostics: Diagnostic.merge(raw.diagnostics, resolved.diagnostics, roleDiagnostics),
    })
  })
  const facts = Object.freeze(analyzed.map((entry) => entry.fact))
  const available = facts.map((fact) => fact.type)
  return Object.freeze({
    explicit: true,
    facts,
    ...(available.every((type) => type !== undefined)
      ? {
          types: Object.freeze(available.filter((type) => type !== undefined)),
        }
      : {}),
    diagnostics: Diagnostic.merge(...analyzed.map((entry) => entry.diagnostics)),
  })
}

export const hasAvailableCallSyntax = (call: SyntaxTree.Node): boolean => {
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

export const isSectionArity = (expectedCount: number, actualCount: number): boolean =>
  actualCount > 0 && actualCount < expectedCount

/** The parameter ordinals a trailing section of `count` parameters captures for its arguments. */
export const trailingCaptures = (count: number, argumentCount: number): ReadonlyArray<number> =>
  Object.freeze(
    Array.from({ length: argumentCount }, (_, ordinal) => count - argumentCount + ordinal),
  )

const parameterAt = <P>(
  parameters: ReadonlyArray<P>,
  captured: ReadonlyArray<number>,
  ordinal: number,
): P | undefined => {
  const parameter = captured.at(ordinal)
  return parameter === undefined ? undefined : parameters.at(parameter)
}

const remainingOf = <P>(
  parameters: ReadonlyArray<P>,
  captured: ReadonlyArray<number>,
): ReadonlyArray<P> => parameters.filter((_, ordinal) => !captured.includes(ordinal))

export type SourceCallable = DeclarationFact | DeclarationFacts.ServiceOperationFact

export const sourceCallable = (reference: CallReferenceFact): SourceCallable | undefined => {
  if (reference._tag === 'Resolved') {
    return reference.declaration
  }
  if (reference._tag === 'ResolvedServiceOperation') {
    return reference.operation
  }
  return undefined
}

export const resolvedCallableContract = (
  reference: CallReferenceFact,
): CallableContract.CallableContract | undefined => {
  if (reference._tag === 'ResolvedIntrinsicContract') return reference.contract
  const callable = sourceCallable(reference)
  if (callable === undefined) {
    return undefined
  }
  return DeclarationFacts.callableContract(
    callable,
    reference._tag === 'ResolvedServiceOperation'
      ? reference.service.typeParameters
      : Object.freeze([]),
  )
}

export const callArityDiagnostic = (
  reference: Extract<
    CallReferenceFact,
    {
      readonly _tag:
        | 'Resolved'
        | 'ResolvedBuiltin'
        | 'ResolvedIntrinsicContract'
        | 'ResolvedServiceOperation'
        | 'ResolvedInterfaceOperation'
    }
  >,
  expectedCount: number,
  actualCount: number,
  span: SourceSpan.SourceSpan,
): Diagnostic.Diagnostic => {
  if (expectedCount === 1 && actualCount === 0)
    return Diagnostic.redundantUnaryEmptyCall(reference.spelling, span)
  let target: Parameters<typeof Diagnostic.wrongCallArity>[0]
  if (reference._tag === 'ResolvedBuiltin') {
    target = Object.freeze({
      _tag: 'BuiltinTarget',
      actor: reference.actor,
      operation: reference.operation,
    })
  } else if (reference._tag === 'ResolvedIntrinsicContract') {
    target = Object.freeze({
      _tag: 'BuiltinTarget',
      actor: 'Intrinsic',
      operation: reference.intrinsic.spelling,
    })
  } else if (reference._tag === 'ResolvedInterfaceOperation') {
    target = Object.freeze({
      _tag: 'BuiltinTarget',
      actor: reference.capability.name,
      operation: reference.operation,
    })
  } else if (reference._tag === 'Resolved') target = reference.declaration.id
  else target = reference.operation.id
  return Diagnostic.wrongCallArity(target, expectedCount, actualCount, span)
}

/** One value argument paired with the parameter type it must determine. */
export interface SpecializationSite {
  /** Position of the argument in the call, so a caller can keep one mistake to one report. */
  readonly ordinal: number
  readonly pattern: SemanticType
  readonly actual: SemanticType
  readonly expression: ExpressionFact
}

/** One written type argument the value arguments contradict, reported at what was written. */
export interface SpecializationConflict {
  readonly diagnostic: Diagnostic.Diagnostic
  /** The argument that implied the other type, absent when no value argument is involved. */
  readonly ordinal?: number
}

export interface SeededSpecialization {
  readonly substitution: Type.Substitution
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly conflicts: ReadonlyArray<SpecializationConflict>
  /**
   * A parameter no explicit argument wrote and no value argument determines. It waits for the
   * ordinary argument checks, because an argument the call got wrong is the better first report.
   */
  readonly unresolved?: Diagnostic.Diagnostic
}

/** Converts one written source type argument to the generic kind its declaration binder owns. */
export const genericArgumentOfTypeArgument = (
  parameter: Type.Parameter,
  fact: TypeArgumentFact,
): Type.GenericArgument | undefined => {
  const writtenType = fact.type
  if (writtenType === undefined) return undefined
  if (parameter.kind === 'Lifetime')
    return Lifetime.isLifetime(writtenType) ? writtenType : undefined
  if (Lifetime.isLifetime(writtenType)) return undefined
  if (parameter.kind === 'Value') return Type.isTypeArgument(writtenType) ? writtenType : undefined
  if (parameter.kind === 'CallableRepresentation' || parameter.kind === 'EffectRepresentation') {
    if (
      Type.isParameter(writtenType) &&
      writtenType.kind === parameter.kind &&
      writtenType.representationBound !== undefined
    )
      return Type.representationParameterArgument(writtenType)
    if (
      Type.isRepresented(writtenType) &&
      Type.representationArgumentKind(writtenType.representation.argument) === parameter.kind
    )
      return writtenType.representation.argument
    return undefined
  }
  if (Type.isParameter(writtenType) && writtenType.kind === 'RequirementRow')
    return Type.requirementRowArgument([], [writtenType])
  if (
    !Type.isNominal(writtenType) &&
    !(Type.isParameter(writtenType) && writtenType.kind === 'Value')
  )
    return undefined
  return Type.isParameter(writtenType)
    ? Type.requirementRowArgumentFromRow(
        RowAlgebra.singleton(
          Type.requirementRowPolicy(),
          Type.requirementMemberShape(
            writtenType,
            'Shared',
            fact.requirementRole ?? RequirementRow.defaultRole,
          ),
          fact.syntax.span,
        ),
      )
    : Type.requirementRowArgument([
        Object.freeze({
          capability: writtenType,
          role: fact.requirementRole ?? RequirementRow.defaultRole,
          access: 'Shared',
        }),
      ])
}

interface SelectedCallLifetimes {
  readonly substitution: Type.Substitution
  readonly inference?: TypeInference.LifetimeInference
  readonly compatibility?: TypeCompatibility.Context | undefined
}

/** Instantiates only the already selected contract's binders, at one declaration-local call site. */
const selectedCallLifetimes = (
  call: SyntaxTree.Node,
  binders: ReadonlyArray<Lifetime.Bound>,
  resolution: ResolutionContext | undefined,
  initial: Type.Substitution = new Map(),
): SelectedCallLifetimes => {
  const substitution = new Map(initial)
  const body = resolution?.bodyLifetimes
  if (body !== undefined) {
    for (const [ordinal, binder] of binders.entries()) {
      if (substitution.has(Lifetime.key(binder))) continue
      const region = BodyLifetime.region(body, call, 'Call', ordinal)
      if (region !== undefined) substitution.set(Lifetime.key(binder), region)
    }
  }
  const compatibility = resolution?.lifetimeCompatibility
  return {
    substitution,
    compatibility,
    ...(compatibility === undefined
      ? {}
      : {
          inference: {
            compatibility,
            typeOutlives: (type, lifetime) =>
              TypeCompatibility.typeOutlives(compatibility, { type, lifetime }),
            accepts: (source: Lifetime.Lifetime, target: Lifetime.Lifetime, invariant: boolean) =>
              typesCompatible(Type.string(source), Type.string(target), compatibility) &&
              (!invariant ||
                typesCompatible(Type.string(target), Type.string(source), compatibility)),
          },
        }),
  }
}

/** Discharges written call preconditions, retaining local obligations for the body's solver. */
const selectedLifetimeBoundDiagnostics = (
  bounds: ReadonlyArray<Lifetime.Outlives>,
  substitution: Type.Substitution,
  compatibility: TypeCompatibility.Context | undefined,
  span: SourceSpan.SourceSpan,
  typeBounds: ReadonlyArray<Type.TypeOutlives> = [],
): ReadonlyArray<Diagnostic.Diagnostic> => [
  ...bounds.flatMap((bound) => {
    const longer = Type.substituteLifetime(bound.longer, substitution)
    const shorter = Type.substituteLifetime(bound.shorter, substitution)
    return typesCompatible(Type.string(longer), Type.string(shorter), compatibility)
      ? []
      : [
          Diagnostic.unsatisfiedLifetimeBound(
            Lifetime.display(longer),
            Lifetime.display(shorter),
            span,
          ),
        ]
  }),
  ...typeBounds.flatMap((bound) => {
    const type = Type.substitute(bound.type, substitution)
    const lifetime = Type.substituteLifetime(bound.lifetime, substitution)
    return TypeCompatibility.typeOutlives(compatibility ?? TypeCompatibility.context(), {
      type,
      lifetime,
    })
      ? []
      : [Diagnostic.unsatisfiedTypeOutlives(Type.display(type), Lifetime.display(lifetime), span)]
  }),
]

/**
 * Specializes a call from an explicit prefix of its type arguments plus its value arguments. The
 * prefix seeds the substitution and the parameters past it are inferred exactly as they are when
 * nothing was written, so a call annotates only the parameters inference cannot reach.
 *
 * A prefix that names every parameter binds everything and leaves inference nothing to do, which
 * is the same substitution a complete explicit list has always produced.
 *
 * `deferred` names the parameters allowed to stay open because something other than these
 * arguments determines them, which is how a callable section keeps its captured parameter generic.
 */
export const seededSpecialization = (
  target: string,
  declared: ReadonlyArray<Type.Parameter>,
  explicit: ReadonlyArray<TypeArgumentFact>,
  sites: ReadonlyArray<SpecializationSite>,
  span: SourceSpan.SourceSpan,
  deferred: ReadonlySet<string> = new Set(),
  enclosingSubstitution: Type.Substitution = new Map(),
  lifetimes?: SelectedCallLifetimes,
): SeededSpecialization => {
  const written = new Map<string, TypeArgumentFact>()
  const selectedParameters = new Map<TypeArgumentFact, Type.Parameter>()
  const seeded = new Map<string, Type.GenericArgument>(lifetimes?.substitution)
  const conflicts: Array<SpecializationConflict> = []
  let lifetimeOrdinal = 0
  let ordinaryOrdinal = 0
  const lifetimeParameters = declared.filter((parameter) => parameter.kind === 'Lifetime')
  const ordinaryParameters = declared.filter((parameter) => parameter.kind !== 'Lifetime')
  for (const fact of explicit) {
    const parameter =
      fact.type !== undefined && Lifetime.isLifetime(fact.type)
        ? lifetimeParameters.at(lifetimeOrdinal++)
        : ordinaryParameters.at(ordinaryOrdinal++)
    const writtenType = fact.type
    if (writtenType === undefined) continue
    if (parameter === undefined) {
      const lifetime = Lifetime.isLifetime(writtenType)
      conflicts.push({
        diagnostic: Diagnostic.typeArgumentArity(
          target,
          lifetime ? lifetimeParameters.length : ordinaryParameters.length,
          explicit.filter(
            (candidate) =>
              candidate.type !== undefined && Lifetime.isLifetime(candidate.type) === lifetime,
          ).length,
          span,
        ),
      })
      continue
    }
    const rawArgument = genericArgumentOfTypeArgument(parameter, fact)
    const argument =
      rawArgument === undefined
        ? undefined
        : Type.substituteGenericArgument(rawArgument, enclosingSubstitution)
    if (argument === undefined) {
      let suppliedKind: Type.ParameterKind = 'Value'
      if (Lifetime.isLifetime(writtenType)) suppliedKind = 'Lifetime'
      else if (Type.isNominal(writtenType)) suppliedKind = 'RequirementRow'
      conflicts.push(
        Object.freeze({
          diagnostic: Diagnostic.genericParameterKindMismatch(
            parameter.name,
            parameter.kind,
            suppliedKind,
            fact.syntax.span,
          ),
        }),
      )
      continue
    }
    seeded.set(Type.key(parameter), argument)
    written.set(Type.key(parameter), fact)
    selectedParameters.set(fact, parameter)
  }
  const inferred = new Map(seeded)
  let rowFailure: Type.RowInferenceFailure | undefined
  for (const site of sites) {
    const attempt = new Map(inferred)
    if (TypeInference.infer(site.pattern, site.actual, attempt, lifetimes?.inference)) {
      commitSpecialization(inferred, attempt)
      continue
    }
    // Inference under the prefix failed. When the argument still satisfies what the prefix says
    // this parameter is, the written type simply wins — that is how a widened literal keeps
    // working under `take<u8>(1)`.
    const expected = Type.substitute(site.pattern, inferred)
    if (
      typesCompatible(site.actual, expected, lifetimes?.compatibility) ||
      contextualIntegerCompatible(site.expression, expected)
    )
      continue
    rowFailure ??= TypeInference.rowInferenceFailure(site.pattern, site.actual)
    const implied = new Map<string, Type.GenericArgument>()
    // Only what this argument alone implies can contradict the prefix; an argument that does not
    // unify at all is an ordinary argument mismatch and belongs to the argument pass.
    if (!TypeInference.infer(site.pattern, site.actual, implied)) continue
    for (const [identity, fact] of written) {
      const suppliedArgument = implied.get(identity)
      const explicitArgument = seeded.get(identity)
      if (suppliedArgument === undefined || explicitArgument === undefined) continue
      if (Type.genericArgumentKey(suppliedArgument) === Type.genericArgumentKey(explicitArgument))
        continue
      conflicts.push(
        Object.freeze({
          ordinal: site.ordinal,
          diagnostic: Diagnostic.typeArgumentConflict(
            target,
            selectedParameters.get(fact)?.name ?? fact.ordinal.toString(),
            Type.encodeGenericArgument(explicitArgument),
            Type.encodeGenericArgument(suppliedArgument),
            fact.syntax.span,
          ),
        }),
      )
    }
  }
  const open = declared.find(
    (parameter) => !inferred.has(Type.key(parameter)) && !deferred.has(Type.key(parameter)),
  )
  const typeArguments = Object.freeze(
    declared.flatMap((parameter) => {
      const argument = inferred.get(Type.key(parameter))
      return argument === undefined ? [] : [argument]
    }),
  )
  return Object.freeze({
    substitution: inferred,
    typeArguments,
    conflicts: Object.freeze(conflicts),
    ...(open === undefined || conflicts.length > 0
      ? {}
      : {
          unresolved:
            rowFailure === undefined
              ? Diagnostic.uninferredTypeParameter(target, open.name, span)
              : Diagnostic.contractRowInference(rowFailure, span),
        }),
  })
}

export const commitSpecialization = (
  target: Map<string, Type.GenericArgument>,
  source: ReadonlyMap<string, Type.GenericArgument>,
): void => {
  target.clear()
  for (const [identity, argument] of source) target.set(identity, argument)
}

export const contractSpecializationSites = (
  arguments_: ReadonlyArray<ArgumentFact>,
  contract: CallableContract.CallableContract,
  enclosingSubstitution: Type.Substitution = new Map(),
): ReadonlyArray<SpecializationSite> =>
  Object.freeze(
    arguments_.flatMap((argument, ordinal): ReadonlyArray<SpecializationSite> => {
      const parameter = contract.parameters.at(ordinal)
      return argument.type._tag === 'Available' && parameter !== undefined
        ? [
            Object.freeze({
              ordinal,
              pattern: parameter.type,
              actual: Type.substitute(argument.type.type, enclosingSubstitution),
              expression: argument.expression,
            }),
          ]
        : []
    }),
  )

export interface ConstraintSolveResult {
  readonly substitution: Type.Substitution
  readonly evidence: ReadonlyArray<Constraint.ConstraintEvidence>
  readonly inferredProviderSelectors: ReadonlyArray<InferredProviderSelector>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export const constraintOrigins = (
  callable: SourceCallable | undefined,
): ReadonlyArray<SourceSpan.SourceSpan> =>
  Object.freeze(callable?.constraints.map((constraint) => constraint.syntax.span) ?? [])

/** Solves provider relations only after arguments have independently established their operands. */
export const solveCallableConstraints = (
  constraints: ReadonlyArray<Constraint.Constraint>,
  origins: ReadonlyArray<SourceSpan.SourceSpan>,
  initial: Type.Substitution,
  caller: DeclarationFact | undefined,
  resolution: ResolutionContext,
  span: SourceSpan.SourceSpan,
): ConstraintSolveResult => {
  const substitution = new Map(initial)
  const evidence: Array<Constraint.ConstraintEvidence> = []
  const inferredProviderSelectors: Array<InferredProviderSelector> = []
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const givens = caller?.constraintContracts ?? Object.freeze([])
  const checked = constraints.flatMap((constraint, ordinal) =>
    constraint._tag === 'ProviderSelectionConstraint'
      ? []
      : [Object.freeze({ constraint, ordinal })],
  )
  const providers = constraints.flatMap((constraint, ordinal) =>
    constraint._tag === 'ProviderSelectionConstraint'
      ? [Object.freeze({ constraint, ordinal })]
      : [],
  )
  const grouped = new Map<string, ReadonlyArray<(typeof providers)[number]>>()
  for (const provider of providers) {
    const selected = provider.constraint.selected.expression
    const groupKey =
      selected._tag === 'RowParameter'
        ? Type.key(selected.parameter)
        : Constraint.key(provider.constraint)
    grouped.set(groupKey, Object.freeze([...(grouped.get(groupKey) ?? []), provider]))
  }
  for (const [selectedKey, group] of grouped) {
    const wanted = group.map(({ constraint }) => Constraint.substitute(constraint, substitution))
    const assumed = wanted.every((constraint) =>
      givens.some((given) => Constraint.key(given) === Constraint.key(constraint)),
    )
    if (assumed) {
      for (const constraint of wanted) evidence.push(Constraint.assumed(constraint, substitution))
      continue
    }
    const selectedArgument = substitution.get(selectedKey)
    const selected =
      selectedArgument !== undefined && Type.isRequirementRowArgument(selectedArgument)
        ? selectedArgument.row
        : undefined
    const relations = wanted.flatMap((constraint, ordinal) =>
      constraint._tag === 'ProviderSelectionConstraint'
        ? [
            Object.freeze<ProviderSelection.Relation>({
              wanted: constraint,
              origins: [origins.at(group.at(ordinal)?.ordinal ?? 0) ?? span],
            }),
          ]
        : [],
    )
    const solved = ProviderSelection.solve({
      relations,
      ...(selected === undefined ? {} : { selected }),
      responsible: span,
      oracle: Object.freeze<ProviderSelection.ConformanceOracle>({
        observation: {
          work: ResolutionWork.ofIndex(resolution.index),
          initiator: { kind: 'CallConstraint', key: selectedKey, span },
        },
        match: (provider: Type.Type, capability: Type.Nominal) =>
          ConformanceProof.providerMatch(resolution.index, provider, capability),
      }),
    })
    if (solved._tag === 'Rejected') {
      diagnostics.push(...solved.diagnostics.map(Diagnostic.providerSelection))
      continue
    }
    if (selectedArgument === undefined) {
      const parameter = group.at(0)?.constraint.selected.expression
      if (parameter?._tag === 'RowParameter') {
        inferredProviderSelectors.push(
          Object.freeze({ parameter: parameter.parameter, selected: solved.member }),
        )
        substitution.set(
          Type.key(parameter.parameter),
          Type.requirementRowArgument([solved.member]),
        )
      }
    }
    for (const selectedEvidence of solved.evidence) {
      const solvedWanted = wanted.find(
        (candidate) => Constraint.key(candidate) === selectedEvidence.wantedKey,
      )
      const specialized =
        solvedWanted === undefined ? undefined : Constraint.substitute(solvedWanted, substitution)
      if (specialized?._tag === 'ProviderSelectionConstraint')
        evidence.push(
          Constraint.requirementSelectionEvidence(
            specialized,
            solved.member,
            selectedEvidence.providerMatch,
          ),
        )
    }
  }
  // Checked constraints run after provider selection so rows bound by inferred
  // provider selectors are visible to structural proofs.
  for (const entry of checked) {
    const wanted = Constraint.substitute(entry.constraint, substitution)
    if (givens.some((given) => Constraint.key(given) === Constraint.key(wanted))) {
      evidence.push(Constraint.assumed(wanted, substitution))
      continue
    }
    if (wanted._tag === 'ProviderSelectionConstraint')
      throw new RangeError('substitution changed a checked constraint into a provider selection')
    const proof = Constraint.proveStructural(wanted)
    if (proof !== undefined) {
      evidence.push(proof)
      continue
    }
    diagnostics.push(
      wanted._tag === 'RequirementSubsetConstraint'
        ? Diagnostic.invalidEffectProvision(
            'selected requirement row is not an exact subset of the source row',
            span,
          )
        : Diagnostic.invalidEffectHandler(
            wanted._tag === 'NominalMemberConstraint'
              ? 'selected failure is absent or remains underconstrained'
              : 'selected failure type is not an exact subset of the source failure type',
            span,
          ),
    )
  }
  return Object.freeze({
    substitution,
    evidence: Object.freeze(evidence),
    inferredProviderSelectors: Object.freeze(inferredProviderSelectors),
    diagnostics: Object.freeze(diagnostics),
  })
}

export const analyzeCallContract = (
  call: SyntaxTree.Node,
  reference: CallReferenceFact,
  argumentsList: ReadonlyArray<ArgumentFact>,
  syntaxAvailable = hasAvailableCallSyntax(call),
  callTypeArguments?: CallTypeArgumentsResult,
  resolution?: ResolutionContext,
  caller?: DeclarationFact,
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
  // A static interface operation's contract is a fixed parameter and result list over its provider,
  // exactly like a compiler-known operation's, so both are checked the same way.
  if (reference._tag === 'ResolvedBuiltin' || reference._tag === 'ResolvedInterfaceOperation') {
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
        type: undefined,
      })
    }
    for (const [ordinal, argument] of argumentsList.entries()) {
      const expected = reference.parameters.at(ordinal)
      if (
        expected !== undefined &&
        argument.type._tag === 'Available' &&
        !typesCompatible(argument.type.type, expected, resolution?.lifetimeCompatibility)
      ) {
        let mismatch: Diagnostic.Diagnostic
        if (Type.isForeignFunction(expected) && !Type.isForeignFunction(argument.type.type))
          mismatch = Diagnostic.invalidForeignCallback(
            Type.encode(argument.type.type),
            'capturing and anonymous Silk callables do not have an exported C address',
            argument.syntax.span,
          )
        else if (Type.isCallable(expected) && Type.isCallable(argument.type.type))
          mismatch = Diagnostic.incompatibleCallableSignature(
            Type.encode(expected),
            Type.encode(argument.type.type),
            argument.syntax.span,
          )
        else
          mismatch =
            unionConversionDiagnostic(
              argument.type.type,
              expected,
              argument.syntax.span,
              resolution?.lifetimeCompatibility,
            ) ??
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
          callArityDiagnostic(reference, expectedCount, actualCount, call.span),
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
        evidence: Object.freeze([]),
        inferredProviderSelectors: Object.freeze([]),
      }),
      diagnostics: Object.freeze([]),
    })
  }

  if (
    reference._tag !== 'Resolved' &&
    reference._tag !== 'ResolvedServiceOperation' &&
    reference._tag !== 'ResolvedIntrinsicContract'
  ) {
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

  const callable = sourceCallable(reference)
  const contract = resolvedCallableContract(reference)
  if (contract === undefined) throw new RangeError('resolved call lost its callable contract')
  const parameters = callable?.parameters ?? Object.freeze([])
  const mappings = Object.freeze(
    argumentsList.flatMap((argument, ordinal): ReadonlyArray<ArgumentMappingFact> => {
      const parameter = parameters.at(ordinal)
      return parameter === undefined
        ? []
        : [Object.freeze({ _tag: 'ArgumentMapping', argument, parameter })]
    }),
  )
  const unavailableArgument = argumentsList.find((argument) => argument.type._tag !== 'Available')
  const unavailableMapping = mappings.find(
    (mapping) => mapping.parameter.declaredType._tag !== 'Resolved',
  )
  if (unavailableArgument !== undefined) {
    return Object.freeze({
      mappings,
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({
          _tag: 'UnavailableBuiltinArgument' as const,
          argument: unavailableArgument,
        }),
      }),
      diagnostics: Object.freeze([]),
    })
  }
  if (unavailableMapping !== undefined)
    return Object.freeze({
      mappings,
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({
          _tag: 'UnavailableMappedType' as const,
          mapping: unavailableMapping,
        }),
      }),
      diagnostics: Object.freeze([]),
    })
  const sites = contractSpecializationSites(
    argumentsList,
    contract,
    resolution?.staticContext?.typeSubstitution,
  )
  const implicitDecay = sites.find(
    (site) => Type.isFixedArray(site.actual) && Type.isSlice(site.pattern),
  )
  if (implicitDecay !== undefined && Type.isSlice(implicitDecay.pattern)) {
    const expected = implicitDecay.pattern
    const argument = argumentsList.at(implicitDecay.ordinal)
    if (argument === undefined) throw new RangeError('specialization site lost its argument')
    const diagnostic = Diagnostic.implicitSliceDecay(Type.encode(expected), argument.syntax.span)
    return Object.freeze({
      mappings,
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({
          _tag: 'ArgumentTypeMismatch',
          argument,
          expected,
        }),
        cause: Diagnostic.identity(diagnostic),
      }),
      diagnostics: Object.freeze([diagnostic]),
    })
  }
  const callLifetimes = selectedCallLifetimes(call, contract.lifetimeBinders, resolution)
  const declaredTypeParameters = contract.binders
  const constraintDeferred = new Set(
    contract.constraints.flatMap((constraint) =>
      constraint._tag === 'ProviderSelectionConstraint' &&
      constraint.selected.expression._tag === 'RowParameter'
        ? [Type.key(constraint.selected.expression.parameter)]
        : [],
    ),
  )
  let substitution: Type.Substitution
  let typeArguments: ReadonlyArray<Type.GenericArgument>
  let unresolvedSpecialization: Diagnostic.Diagnostic | undefined
  if (callTypeArguments?.explicit === true) {
    // More type arguments than the callable declares is the arity error that remains: fewer is a
    // prefix, and the parameters it leaves open are inferred from the value arguments below.
    if (callTypeArguments.facts.length > declaredTypeParameters.length) {
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
        type: undefined,
      })
    }
    const seeded = seededSpecialization(
      reference.spelling,
      declaredTypeParameters,
      callTypeArguments.facts,
      sites,
      call.span,
      constraintDeferred,
      resolution?.staticContext?.typeSubstitution,
      callLifetimes,
    )
    const conflict = seeded.conflicts.at(0)
    if (conflict !== undefined) {
      return Object.freeze({
        mappings,
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
          cause: Diagnostic.identity(conflict.diagnostic),
        }),
        diagnostics: Object.freeze([conflict.diagnostic]),
      })
    }
    typeArguments = seeded.typeArguments
    substitution = seeded.substitution
    unresolvedSpecialization = seeded.unresolved
  } else if (declaredTypeParameters.length === 0) {
    typeArguments = Object.freeze([])
    substitution = callLifetimes.substitution
  } else {
    const inferred = new Map<string, Type.GenericArgument>(callLifetimes.substitution)
    let compatible = true
    let rowFailure: Type.RowInferenceFailure | undefined
    let pending = [...sites]
    while (pending.length > 0) {
      const deferred: Array<SpecializationSite> = []
      let progressed = false
      for (const site of pending) {
        const pattern = site.pattern
        const supplied = site.actual
        const argument = argumentsList.at(site.ordinal)
        if (argument === undefined) {
          compatible = false
          break
        }
        const representedSupplied =
          Type.isRepresented(pattern) &&
          !Type.isRepresented(supplied) &&
          (Type.isCallable(supplied) || Type.isEffect(supplied))
            ? (() => {
                const representation = representationOfExpression(argument.expression)
                return representation === undefined
                  ? undefined
                  : Type.represented(supplied, pattern.representation.requiredBound, representation)
              })()
            : supplied
        if (representedSupplied === undefined) {
          compatible = false
          rowFailure = TypeInference.rowInferenceFailure(pattern, supplied)
          break
        }
        const attempt = new Map(inferred)
        if (TypeInference.infer(pattern, representedSupplied, attempt, callLifetimes.inference)) {
          commitSpecialization(inferred, attempt)
          progressed = true
        } else {
          deferred.push(site)
        }
      }
      if (!compatible) break
      if (deferred.length === 0) break
      if (!progressed) {
        const failed = deferred.at(0)
        rowFailure =
          failed === undefined
            ? undefined
            : TypeInference.rowInferenceFailure(failed.pattern, failed.actual)
        compatible = false
        break
      }
      pending = deferred
    }
    typeArguments = Object.freeze(
      declaredTypeParameters.flatMap((parameter) => {
        const inferredType = inferred.get(Type.key(parameter))
        return inferredType === undefined ? [] : [inferredType]
      }),
    )
    const missingFromArguments = declaredTypeParameters.find(
      (parameter) =>
        !inferred.has(Type.key(parameter)) && !constraintDeferred.has(Type.key(parameter)),
    )
    if (!compatible || missingFromArguments !== undefined) {
      const diagnostic =
        rowFailure === undefined
          ? Diagnostic.typeArgumentInference(reference.spelling, call.span)
          : Diagnostic.contractRowInference(rowFailure, call.span)
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
  let evidence: ReadonlyArray<Constraint.ConstraintEvidence> = Object.freeze([])
  let inferredProviderSelectors: ReadonlyArray<InferredProviderSelector> = Object.freeze([])
  if (resolution !== undefined && contract.constraints.length > 0) {
    const solved = solveCallableConstraints(
      contract.constraints,
      constraintOrigins(callable),
      substitution,
      caller,
      resolution,
      call.span,
    )
    substitution = solved.substitution
    evidence = solved.evidence
    inferredProviderSelectors = solved.inferredProviderSelectors
    const firstConstraintDiagnostic = solved.diagnostics.at(0)
    if (firstConstraintDiagnostic !== undefined)
      return Object.freeze({
        mappings,
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
          cause: Diagnostic.identity(firstConstraintDiagnostic),
        }),
        diagnostics: solved.diagnostics,
      })
    typeArguments = Object.freeze(
      declaredTypeParameters.flatMap((parameter) => {
        const argument = substitution.get(Type.key(parameter))
        return argument === undefined ? [] : [argument]
      }),
    )
  }
  const remainingOpen = declaredTypeParameters.find(
    (parameter) => substitution.get(Type.key(parameter)) === undefined,
  )
  if (remainingOpen !== undefined)
    unresolvedSpecialization ??= Diagnostic.uninferredTypeParameter(
      reference.spelling,
      remainingOpen.name,
      call.span,
    )
  for (const site of sites) {
    const argument = argumentsList.at(site.ordinal)
    if (argument === undefined) continue
    const expected = Type.substitute(site.pattern, substitution)
    const expectedValue = Type.isRepresented(expected) ? expected.contract : expected
    const suppliedValue = Type.isRepresented(site.actual) ? site.actual.contract : site.actual
    if (
      !typesCompatible(suppliedValue, expectedValue, resolution?.lifetimeCompatibility) &&
      !contextualIntegerCompatible(argument.expression, expectedValue)
    ) {
      let mismatch: Diagnostic.Diagnostic
      if (Type.isForeignFunction(expectedValue) && !Type.isForeignFunction(suppliedValue)) {
        mismatch = Diagnostic.invalidForeignCallback(
          Type.encode(suppliedValue),
          'capturing and anonymous Silk callables do not have an exported C address',
          argument.syntax.span,
        )
      } else if (Type.isCallable(expectedValue) && Type.isCallable(suppliedValue)) {
        mismatch = Diagnostic.incompatibleCallableSignature(
          Type.encode(expectedValue),
          Type.encode(suppliedValue),
          argument.syntax.span,
        )
      } else if (Type.isSlice(expectedValue) && Type.isFixedArray(suppliedValue)) {
        mismatch = Diagnostic.implicitSliceDecay(Type.encode(expectedValue), argument.syntax.span)
      } else {
        mismatch =
          unionConversionDiagnostic(
            suppliedValue,
            expectedValue,
            argument.syntax.span,
            resolution?.lifetimeCompatibility,
          ) ??
          Diagnostic.argumentTypeMismatch(
            Type.encode(expectedValue),
            Type.encode(suppliedValue),
            argument.syntax.span,
          )
      }
      return Object.freeze({
        mappings,
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({
            _tag: 'ArgumentTypeMismatch',
            argument,
            expected,
          }),
          cause: Diagnostic.identity(mismatch),
        }),
        diagnostics: Object.freeze([mismatch]),
      })
    }
  }

  const expectedCount = contract.parameters.length
  const actualCount = argumentsList.length
  if (expectedCount !== actualCount) {
    return Object.freeze({
      mappings,
      fact: Object.freeze({ _tag: 'ArityMismatch', expectedCount, actualCount }),
      diagnostics: Object.freeze([
        callArityDiagnostic(reference, expectedCount, actualCount, call.span),
      ]),
    })
  }
  // Every argument the call did supply is sound, so what remains open is genuinely undetermined
  // rather than a consequence of an argument the author already needs to fix.
  if (unresolvedSpecialization !== undefined) {
    return Object.freeze({
      mappings,
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
        cause: Diagnostic.identity(unresolvedSpecialization),
      }),
      diagnostics: Object.freeze([unresolvedSpecialization]),
    })
  }

  const lifetimeDiagnostics = selectedLifetimeBoundDiagnostics(
    contract.lifetimeBounds,
    substitution,
    resolution?.lifetimeCompatibility,
    call.span,
    contract.typeOutlives,
  )
  const lifetimeFailure = lifetimeDiagnostics.at(0)
  if (lifetimeFailure !== undefined)
    return Object.freeze({
      mappings,
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
        cause: Diagnostic.identity(lifetimeFailure),
      }),
      diagnostics: lifetimeDiagnostics,
    })
  return Object.freeze({
    mappings,
    fact: Object.freeze({
      _tag: 'Compatible',
      expectedCount,
      actualCount,
      typeArguments,
      substitution,
      evidence,
      inferredProviderSelectors,
    }),
    diagnostics: Object.freeze([]),
  })
}

/** Retains the ordinary operation selection so later semantic passes never rediscover witnesses. */
export const interfaceEvidence = (
  reference: CallReferenceFact,
  index: DeclarationIndex.Index,
): ReadonlyArray<ConformanceGoal.Proof> => {
  if (
    reference._tag !== 'ResolvedInterfaceOperation' ||
    !Type.isRuntimeConcrete(reference.provider)
  )
    return []
  const proof = ConformanceProof.prove(index, reference.provider, reference.capability)
  return proof._tag === 'Proved' ? [proof] : []
}

export const interfaceConstraints = (
  reference: CallReferenceFact,
  substitution: Type.Substitution | undefined,
  index: DeclarationIndex.Index,
  caller: DeclarationFact,
  span: SourceSpan.SourceSpan,
): {
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly proofs: ReadonlyArray<ConformanceGoal.Proof>
} => {
  const proofs: Array<ConformanceGoal.Proof> = []
  if (reference._tag !== 'Resolved' || substitution === undefined) {
    proofs.push(...interfaceEvidence(reference, index))
    return { diagnostics: [], proofs }
  }
  const diagnostics = Object.freeze(
    reference.declaration.typeParameters.flatMap((parameter) => {
      const provider = substitution.get(Type.key(parameter.type))
      if (provider === undefined || !Type.isTypeArgument(provider)) return []
      return parameter.bounds.flatMap((bound): ReadonlyArray<Diagnostic.Diagnostic> => {
        // An unresolved bound was reported at its declaration.
        if (bound._tag !== 'ResolvedBound') return []
        const substitutedCapability = Type.substitute(bound.application.capability, substitution)
        if (!Type.isNominal(substitutedCapability))
          return [
            Diagnostic.invalidConformance(
              `unknown interface constraint ${bound.spelling}`,
              parameter.syntax.span,
            ),
          ]
        const capability = substitutedCapability
        const assumedByCaller =
          boundAssumedBy(caller, provider, capability) ||
          (Type.equals(capability, Type.copyCapability) &&
            ConformanceProof.copyType(index, provider, copyAssumptionsOf(caller)))
        if (assumedByCaller) return []
        if (!bound.application.providerMatches)
          return [
            Diagnostic.invalidConformance(
              `${bound.spelling} cannot bind Self to ${Type.encode(provider)}`,
              parameter.syntax.span,
            ),
          ]
        // Selection excludes rejected declarations, but a partial declaration still carries the most
        // useful source error: name the exact operation it failed to map before reporting the broader
        // missing-witness result.
        const unmapped = ConformanceProof.unmappedInterfaceOperations(index, provider, capability)
        if (unmapped.length > 0)
          return unmapped.map((operation) =>
            Diagnostic.invalidConformance(
              `${Type.encode(provider)} does not implement ${bound.spelling}.${operation}`,
              span,
            ),
          )
        if (!ConformanceProof.conforms(index, provider, capability)) {
          // A conditional header that covers this provider but whose own requirements failed has a
          // more useful answer than "does not implement": the chain says which requirement is
          // missing and which wrapper asked for it.
          const proof = ConformanceProof.prove(index, provider, capability)
          if (
            proof._tag === 'Unproved' &&
            ConformanceGoal.key(proof.goal) !==
              ConformanceGoal.key(ConformanceGoal.make(capability, provider))
          )
            return [
              Diagnostic.unprovenConformance(
                ConformanceGoal.encode(ConformanceGoal.make(capability, provider)),
                ConformanceGoal.describe(proof.failure),
                ConformanceGoal.traceLines(proof),
                span,
              ),
            ]
          return [
            Diagnostic.invalidConformance(
              `${Type.encode(provider)} does not implement ${bound.spelling}`,
              span,
            ),
          ]
        }
        const proof = ConformanceProof.prove(index, provider, capability)
        if (proof._tag === 'Proved') proofs.push(proof)
        return []
      })
    }),
  )
  return { diagnostics, proofs }
}

/**
 * Reports whether one caller's own explicit bounds already promise `capability` for a
 * parameter-typed provider, so the promise is evidence rather than something to prove again.
 */
export const boundAssumedBy = (
  caller: DeclarationFact,
  provider: Type.Type,
  capability: Type.Nominal,
): boolean =>
  Type.isParameter(provider) &&
  caller.typeParameters.some(
    (parameter) =>
      Type.equals(parameter.type, provider) &&
      parameter.bounds.some(
        (bound) =>
          bound._tag === 'ResolvedBound' && Type.equals(bound.application.capability, capability),
      ),
  )

export const copyAssumptionsOf = (declaration: DeclarationFact): ReadonlySet<string> =>
  new Set(
    declaration.typeParameters.flatMap((parameter) =>
      parameter.bounds.some(
        (candidate) =>
          candidate._tag === 'ResolvedBound' &&
          Type.equals(candidate.application.capability, Type.copyCapability),
      )
        ? [Type.key(parameter.type)]
        : [],
    ),
  )

export interface BuiltinSignature {
  readonly id: Intrinsic.OperationId
  readonly operation: Hir.BuiltinOperation
  readonly typeParameters?: ReadonlyArray<Type.Parameter>
  readonly parameters: ReadonlyArray<SemanticType>
  readonly result: SemanticType
  readonly unsafe?: boolean
}

export const builtinSignature = (
  actor: string,
  operation: string,
  parameterKind: 'Call' | 'Primitive' = 'Call',
): BuiltinSignature | undefined => {
  const catalog = Intrinsic.findOperation(actor, operation)
  if (catalog === undefined || !Intrinsic.isBuiltinOperation(catalog)) return undefined
  return Object.freeze({
    id: catalog.id,
    operation: catalog.rule.operation,
    typeParameters: catalog.rule.typeParameters,
    parameters: parameterKind === 'Call' ? catalog.callParameters : catalog.rule.parameters,
    result: catalog.rule.result,
    unsafe: catalog.unsafe,
  })
}

/** Allocates validity variables only after intrinsic selection; intrinsic templates never escape into caller bodies. */
const freeLifetimeBinders = (types: ReadonlyArray<Type.Type>): ReadonlyArray<Lifetime.Bound> => [
  ...new Map(
    types
      .flatMap(Type.freeLifetimes)
      .flatMap((lifetime) =>
        lifetime._tag === 'BoundLifetime' ? [[Lifetime.key(lifetime), lifetime] as const] : [],
      ),
  ).values(),
]

/** Opens an intrinsic's free template lifetimes in the selected caller's finite region domain. */
export const instantiateBuiltinSignature = (
  signature: BuiltinSignature,
  call: SyntaxTree.Node,
  resolution: ResolutionContext,
): BuiltinSignature => {
  const binders = freeLifetimeBinders([...signature.parameters, signature.result])
  const selected = selectedCallLifetimes(call, binders, resolution)
  return {
    ...signature,
    parameters: signature.parameters.map((parameter) =>
      Type.substitute(parameter, selected.substitution),
    ),
    result: Type.substitute(signature.result, selected.substitution),
  }
}

export const callableResultType = (declaration: SourceCallable): SemanticType | undefined => {
  if (declaration.returnType._tag !== 'Resolved') return undefined
  if (declaration.functionKind === 'Ordinary') return declaration.returnType.type
  return Type.effectWithRows(
    declaration.returnType.type,
    declaration.failureRow.row,
    { ...DeclarationFacts.executableLifetimes(declaration), lifetimeBinders: [] },
    'Shared',
    declaration.requirementRow.row,
  )
}

export const callableTypeOfReference = (
  reference: CallReferenceFact,
): Type.Callable | undefined => {
  if (reference._tag === 'ResolvedBuiltin')
    return Type.callable(
      reference.parameters,
      reference.result,
      {
        environment: Lifetime.staticLifetime,
        lifetimeBinders: [
          ...new Map(
            reference.parameters
              .flatMap(Type.freeLifetimes)
              .filter((lifetime) => lifetime._tag === 'BoundLifetime')
              .map((lifetime) => [Lifetime.key(lifetime), lifetime]),
          ).values(),
        ],
      },
      'Shared',
      undefined,
      reference.unsafe,
    )
  const callable = sourceCallable(reference)
  if (callable === undefined) return undefined
  const parameters = callable.parameters.flatMap((parameter) =>
    parameter.declaredType._tag === 'Resolved' ? [parameter.declaredType.type] : [],
  )
  const result = callableResultType(callable)
  if (parameters.length !== callable.parameters.length || result === undefined) return undefined
  const contract = resolvedCallableContract(reference)
  return Type.callable(
    parameters,
    result,
    { ...DeclarationFacts.executableLifetimes(callable), environment: Lifetime.staticLifetime },
    'Shared',
    contract === undefined || (contract.constraints.length === 0 && contract.binders.length === 0)
      ? undefined
      : Object.freeze({
          ...(reference._tag === 'Resolved' && reference.declaration.canonical._tag === 'Canonical'
            ? { source: reference.declaration.canonical.id }
            : {}),
          contract,
          binders: contract.binders,
          constraints: contract.constraints,
          evidence: Object.freeze([]),
          substitution: new Map(),
          contractKey: CallableContract.key(contract),
          constraintKeys: Object.freeze(contract.constraints.map(Constraint.key)),
          evidenceKeys: Object.freeze([]),
          origins: constraintOrigins(callable),
        }),
    callable.unsafe,
  )
}

export const serviceOperation = (
  service: DeclarationFacts.ContractFact,
  spelling_: string,
): DeclarationFacts.ServiceOperationFact | undefined =>
  service.operations.find(
    (operation) =>
      operation.state._tag === 'Unique' &&
      operation.name._tag === 'Present' &&
      operation.name.spelling === spelling_,
  )

/**
 * The contract one interface operation declares over a bounded parameter.
 *
 * The interface writes its contract over its own type parameter; a bound applies that interface to
 * one parameter of the bounded declaration, so the operation's contract over that parameter is the
 * declared one with the interface's parameter substituted. It is the same contract the conformance
 * check already holds every witness to, which is what lets the body be checked once, over the
 * canonical parameter, before any concrete argument exists.
 */
export const interfaceOperationContract = (
  operation: DeclarationFacts.InterfaceOperationApplicationFact,
):
  | {
      readonly declaration: DeclarationFacts.ServiceOperationFact
      readonly contract: DeclarationFacts.InterfaceOperationApplicationFact
      readonly parameters: ReadonlyArray<SemanticType>
      readonly result: SemanticType
    }
  | undefined => {
  if (
    operation.declaration.typeParameters.some((parameter) => parameter.type.kind !== 'Lifetime') ||
    operation.success._tag !== 'Resolved'
  )
    return undefined
  const parameters = operation.operands.flatMap((operand) =>
    operand.type._tag === 'Resolved' ? [operand.type.type] : [],
  )
  if (parameters.length !== operation.operands.length) return undefined
  const result =
    operation.functionKind === 'Ordinary'
      ? operation.success.type
      : Type.effectWithRows(
          operation.success.type,
          operation.failureRow.row,
          { ...operation.lifetimes, lifetimeBinders: [] },
          'Shared',
          operation.requirementRow.row,
        )
  return Object.freeze({
    declaration: operation.declaration,
    contract: operation,
    parameters: Object.freeze(parameters),
    result,
  })
}

/** Opens only the selected interface operation's invocation lifetime binders at this call. */
export const instantiateInterfaceReference = (
  reference: Extract<CallReferenceFact, { readonly _tag: 'ResolvedInterfaceOperation' }>,
  call: SyntaxTree.Node,
  resolution: ResolutionContext,
): typeof reference => {
  const declared = DeclarationFacts.executableLifetimes(reference.declaration)
  const selected = selectedCallLifetimes(call, declared.lifetimeBinders, resolution)
  return Object.freeze({
    ...reference,
    parameters: reference.parameters.map((parameter) =>
      Type.substitute(parameter, selected.substitution),
    ),
    result: Type.substitute(reference.result, selected.substitution),
    interfaceContract: DeclarationFacts.instantiateInterfaceOperation(
      reference.interfaceContract,
      selected.substitution,
    ),
  })
}

/**
 * Resolves one `Bound.operation(...)` receiver against the bounds of the declaration being
 * elaborated.
 *
 * A bound's operation is spelled through the bound's own name, so inside a body bounded by an
 * interface that name selects the bound's operation rather than a same-named public function of the
 * module declaring the interface. The preference is deliberately narrow: only a name the bound's
 * recorded contract actually declares is taken, so every other member of that module keeps
 * resolving exactly where it resolved before, and a body with no such bound is untouched.
 *
 * One declaration may bound two of its parameters by one interface. The receiver then names no
 * single parameter, and the call is reported rather than resolved to either.
 */
export const boundOperationReference = (
  declaration: DeclarationFact,
  interface_: DeclarationFacts.ContractFact,
  qualifier: string,
  member: string,
  memberToken: Token.Token,
):
  | {
      readonly _tag: 'BoundOperation'
      readonly reference: Extract<
        CallReferenceFact,
        { readonly _tag: 'ResolvedInterfaceOperation' }
      >
    }
  | { readonly _tag: 'AmbiguousBound'; readonly parameters: ReadonlyArray<string> }
  | undefined => {
  if (interface_.canonical._tag !== 'Canonical') return undefined
  const capability = interface_.canonical.id
  const bounded = declaration.typeParameters.flatMap((parameter) =>
    parameter.bounds.flatMap((bound) =>
      bound._tag === 'ResolvedBound' &&
      bound.application.declaration.module === capability.module &&
      bound.application.declaration.name === capability.name &&
      bound.application.operations.some(
        (operation) =>
          operation.declaration.name._tag === 'Present' &&
          operation.declaration.name.spelling === member,
      )
        ? [Object.freeze({ parameter, bound })]
        : [],
    ),
  )
  if (bounded.length === 0) return undefined
  if (bounded.length > 1)
    return Object.freeze({
      _tag: 'AmbiguousBound',
      parameters: Object.freeze(
        bounded.map(({ parameter }) =>
          parameter.name._tag === 'Present' ? parameter.name.spelling : Type.encode(parameter.type),
        ),
      ),
    })
  const selected = bounded.at(0)
  if (selected === undefined) return undefined
  const { parameter, bound } = selected
  const operation = bound.application.operations.find(
    (candidate) =>
      candidate.declaration.name._tag === 'Present' &&
      candidate.declaration.name.spelling === member,
  )
  if (operation === undefined) return undefined
  const contract = interfaceOperationContract(operation)
  if (contract === undefined) return undefined
  return Object.freeze({
    _tag: 'BoundOperation',
    reference: Object.freeze({
      _tag: 'ResolvedInterfaceOperation' as const,
      spelling: `${qualifier}.${member}`,
      token: memberToken,
      capability: bound.application.capability,
      provider: parameter.type,
      operation: member,
      declaration: contract.declaration,
      interfaceContract: contract.contract,
      parameters: contract.parameters,
      result: contract.result,
    }),
  })
}

export const resolvedFunctionReference = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  resolution: ResolutionContext,
): CallReferenceFact | undefined => {
  const identifiers = callReferenceTokens(node)
  const first = identifiers.at(0)
  const second = identifiers.at(1)
  if (first === undefined) return undefined
  if (second === undefined) {
    const name = spelling(source, first)
    const resolved = NameResolution.lookup(resolution.scope, resolution.index, name)
    const local = lookupDeclaration(declarations, name)
    let declaration: DeclarationFacts.DeclarationFact | undefined
    if (resolved._tag === 'Resolved' && resolved.declaration._tag === 'FunctionDeclaration') {
      declaration = resolved.declaration
    } else if (local._tag === 'Resolved') {
      declaration = local.declaration
    } else {
      declaration = undefined
    }
    return declaration === undefined
      ? undefined
      : Object.freeze({
          _tag: 'Resolved',
          spelling: name,
          token: first,
          declaration,
        })
  }
  const qualifier = spelling(source, first)
  const member = spelling(source, second)
  const qualifierLookup = NameResolution.lookup(resolution.scope, resolution.index, qualifier)
  if (qualifierLookup._tag === 'Intrinsic') {
    const signature = builtinSignature(qualifier, member)
    if (signature === undefined) {
      return undefined
    }
    return Object.freeze({
      _tag: 'ResolvedBuiltin',
      spelling: `${qualifier}.${member}`,
      token: second,
      actor: qualifier,
      operation: signature.operation,
      intrinsic: signature.id,
      parameters: signature.parameters,
      result: signature.result,
      unsafe: signature.unsafe === true,
    })
  }
  if (qualifierLookup._tag === 'Resolved') {
    const associated = NameResolution.lookupAssociated(
      resolution.index,
      qualifierLookup.declaration,
      member,
      resolution.scope.module,
    )
    return associated._tag === 'Inherent'
      ? Object.freeze({
          _tag: 'Resolved',
          spelling: `${qualifier}.${member}`,
          token: second,
          declaration: associated.declaration,
        })
      : undefined
  }
  if (qualifierLookup._tag !== 'Namespace') return undefined
  const memberLookup = DeclarationFacts.lookup(resolution.index, qualifierLookup.module, member)
  if (
    memberLookup._tag !== 'Resolved' ||
    memberLookup.declaration._tag !== 'FunctionDeclaration' ||
    memberLookup.declaration.visibility !== 'Public'
  )
    return undefined
  return Object.freeze({
    _tag: 'Resolved',
    spelling: `${qualifier}.${member}`,
    token: second,
    declaration: memberLookup.declaration,
  })
}

export const analyzeFunctionItem = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  resolution: ResolutionContext,
  caller: DeclarationFact,
  expected?: SemanticType,
): ExpressionResult | undefined => {
  const reference = resolvedFunctionReference(source, node, declarations, resolution)
  if (reference === undefined) {
    const identifiers = callReferenceTokens(node)
    const qualifierToken = identifiers.at(0)
    const memberToken = identifiers.at(1)
    if (qualifierToken === undefined || memberToken === undefined) return undefined
    const qualifier = spelling(source, qualifierToken)
    const member = spelling(source, memberToken)
    const qualifierLookup = NameResolution.lookup(resolution.scope, resolution.index, qualifier)
    if (qualifierLookup._tag !== 'Namespace') return undefined
    const memberLookup = DeclarationFacts.lookup(resolution.index, qualifierLookup.module, member)
    let diagnostic: Diagnostic.Diagnostic | undefined
    if (memberLookup._tag !== 'Resolved') {
      diagnostic = Diagnostic.unknownImportedMember(
        qualifierLookup.module,
        member,
        memberToken.span,
      )
    } else if (memberLookup.declaration.visibility !== 'Public') {
      diagnostic = Diagnostic.inaccessibleImportedMember(
        qualifierLookup.module,
        member,
        memberToken.span,
      )
    } else {
      diagnostic = undefined
    }
    if (diagnostic === undefined) return undefined
    const missing: CallReferenceFact = Object.freeze({
      _tag: 'Missing',
      spelling: `${qualifier}.${member}`,
      token: memberToken,
      cause: Diagnostic.identity(diagnostic),
    })
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'FunctionItem',
        reference: missing,
        path: referencePath(node),
        typeArguments: Object.freeze([]),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([diagnostic]),
      type: undefined,
    })
  }
  const unresolvedCallable = callableTypeOfReference(reference)
  if (expected !== undefined && Type.isForeignFunction(expected)) {
    const declaration = reference._tag === 'Resolved' ? reference.declaration : undefined
    let detail: string | undefined
    if (declaration === undefined) {
      detail = 'only a named exported Silk function has a stable native address'
    } else if (declaration.foreignExport === undefined) {
      detail = 'the function must be declared with export "C"'
    } else if (declaration.typeParameters.length > 0) {
      detail = 'generic functions do not have one monomorphic C address'
    } else if (declaration.functionKind !== 'Ordinary') {
      detail = 'effect functions cannot cross the synchronous C callback boundary'
    } else if (
      unresolvedCallable === undefined ||
      unresolvedCallable.parameters.length !== expected.parameters.length ||
      !unresolvedCallable.parameters.every((parameter, ordinal) => {
        const expectedParameter = expected.parameters.at(ordinal)
        return expectedParameter !== undefined && Type.equals(parameter, expectedParameter)
      }) ||
      !Type.equals(unresolvedCallable.result, expected.result)
    ) {
      detail = `the exported signature must exactly match ${Type.encode(expected)}`
    }
    if (detail !== undefined) {
      const name = reference._tag === 'Unavailable' ? '<expression>' : reference.spelling
      const diagnostic = Diagnostic.invalidForeignCallback(name, detail, node.span)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'FunctionItem',
          reference,
          path: referencePath(node),
          typeArguments: Object.freeze([]),
          type: unavailableExpressionType,
          syntax: node,
        }),
        diagnostics: Object.freeze([diagnostic]),
        type: undefined,
      })
    }
    const symbol = declaration?.foreignExport?.symbol
    if (symbol === undefined) throw new RangeError('validated C callback lost its exported symbol')
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'FunctionItem',
        reference,
        path: referencePath(node),
        typeArguments: Object.freeze([]),
        foreignAddress: Object.freeze({ symbol }),
        type: availableExpressionType(expected),
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
      type: expected,
    })
  }
  const contract = resolvedCallableContract(reference)
  const expectedValue =
    expected !== undefined && Type.isRepresented(expected) ? expected.contract : expected
  const expectedCallable =
    expectedValue !== undefined && Type.isCallable(expectedValue) ? expectedValue : undefined
  const opened =
    expectedCallable?.lifetimeBinders.length === 0
      ? (unresolvedCallable?.lifetimeBinders ?? [])
      : []
  const callLifetimes = selectedCallLifetimes(node, opened, resolution)
  // Here the inference pattern is the offered generic item, while the contextual type is the
  // required use. Argument inference has the opposite orientation; retain that distinction for
  // lifetime obligations while still inferring the item's own type parameters.
  const itemInference: TypeInference.LifetimeInference | undefined =
    callLifetimes.inference === undefined
      ? undefined
      : {
          ...callLifetimes.inference,
          accepts: (source, target, invariant) =>
            callLifetimes.inference?.accepts(target, source, invariant) ?? false,
        }
  const contextual = new Map<string, Type.GenericArgument>(callLifetimes.substitution)
  const pattern =
    unresolvedCallable === undefined || expectedCallable === undefined
      ? undefined
      : Type.callable(
          unresolvedCallable.parameters,
          unresolvedCallable.result,
          {
            ...unresolvedCallable,
            environment: expectedCallable.environment,
            lifetimeBinders:
              expectedCallable.lifetimeBinders.length === 0
                ? []
                : unresolvedCallable.lifetimeBinders,
          },
          expectedCallable.mode,
          unresolvedCallable.schema,
          unresolvedCallable.unsafe,
        )
  const substitutedPattern =
    pattern === undefined ? undefined : Type.substitute(pattern, contextual)
  const contextualPattern =
    substitutedPattern !== undefined && Type.isCallable(substitutedPattern)
      ? substitutedPattern
      : undefined
  let specialized =
    contextualPattern !== undefined &&
    expectedCallable !== undefined &&
    TypeInference.infer(contextualPattern, expectedCallable, contextual, itemInference)
  if (!specialized && contextualPattern !== undefined && expectedCallable !== undefined) {
    const partial = new Map<string, Type.GenericArgument>(callLifetimes.substitution)
    const attempt = (): boolean => {
      const parametersCompatible =
        contextualPattern.parameters.length === expectedCallable.parameters.length &&
        contextualPattern.parameters.every((parameter, ordinal) => {
          const expectedParameter = expectedCallable.parameters.at(ordinal)
          return (
            expectedParameter !== undefined &&
            TypeInference.infer(parameter, expectedParameter, partial, itemInference)
          )
        })
      const patternResult = contextualPattern.result
      const expectedResult = expectedCallable.result
      const resultCompatible =
        Type.isEffect(patternResult) && Type.isEffect(expectedResult)
          ? TypeInference.infer(
              patternResult.success,
              expectedResult.success,
              partial,
              itemInference,
            )
          : TypeInference.infer(patternResult, expectedResult, partial, itemInference)
      const allBindersDetermined = (contract?.binders ?? []).every((parameter) =>
        partial.has(Type.key(parameter)),
      )
      return parametersCompatible && resultCompatible && allBindersDetermined
    }
    const accepted =
      callLifetimes.compatibility === undefined
        ? attempt()
        : TypeCompatibility.commitWhen(callLifetimes.compatibility, attempt, (result) => result)
    if (accepted) {
      contextual.clear()
      for (const [key, argument] of partial) contextual.set(key, argument)
      specialized = true
    }
  }
  let callable = unresolvedCallable
  if (callable !== undefined && specialized) {
    const instantiated = Type.callable(
      callable.parameters,
      callable.result,
      {
        ...callable,
        lifetimeBinders: callable.lifetimeBinders.filter(
          (binder) =>
            expectedCallable?.lifetimeBinders.length !== 0 || !contextual.has(Lifetime.key(binder)),
        ),
      },
      callable.mode,
      callable.schema,
      callable.unsafe,
    )
    const contextualCallable = Type.substitute(instantiated, contextual)
    callable = Type.isCallable(contextualCallable) ? contextualCallable : undefined
  }
  const typeArguments = Object.freeze(
    specialized
      ? (contract?.binders ?? []).flatMap((parameter) => {
          const argument = contextual.get(Type.key(parameter))
          return argument === undefined ? [] : [argument]
        })
      : [],
  )
  // A foreign function is callable only; the call path discards this item and resolves the
  // declaration directly, so the diagnostic survives exactly at first-class uses.
  // A static function has no runtime function item either (STATIC-001).
  const firstClass =
    foreignFirstClassDiagnostic(reference, node) ??
    staticFirstClassDiagnostic(reference, node, resolution)
  const constraints = interfaceConstraints(
    reference,
    contextual,
    resolution.index,
    caller,
    node.span,
  )
  // Specialization can turn invocation predicates into free formation facts. Prove them before
  // publishing the value: structural callable comparison may thereafter assume those facts.
  const formation =
    callable === undefined ? undefined : Type.executableFormationRequirements(callable)
  const lifetimeDiagnostics =
    formation === undefined
      ? []
      : selectedLifetimeBoundDiagnostics(
          formation.lifetimeBounds,
          new Map(),
          resolution.lifetimeCompatibility,
          node.span,
          formation.typeOutlives,
        )
  const available = firstClass === undefined && lifetimeDiagnostics.length === 0
  const type =
    callable === undefined || !available
      ? unavailableExpressionType
      : availableExpressionType(callable)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FunctionItem',
      selectedConformances: constraints.proofs,
      reference,
      path: referencePath(node),
      typeArguments,
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      ...constraints.diagnostics,
      ...lifetimeDiagnostics,
      ...(firstClass === undefined ? [] : [firstClass]),
    ]),
    type: available ? callable : undefined,
  })
}

const staticFirstClassDiagnostic = (
  reference: CallReferenceFact,
  node: SyntaxTree.Node,
  resolution: ResolutionContext,
): Diagnostic.Diagnostic | undefined =>
  reference._tag === 'Resolved' && reference.declaration.phase === 'Static'
    ? Diagnostic.staticPhaseViolation(
        `static function ${reference.spelling} as a runtime callable`,
        resolution.staticContext?.environment.target ?? 'unselected-target',
        Object.freeze([]),
        node.span,
      )
    : undefined

const foreignFirstClassDiagnostic = (
  reference: CallReferenceFact,
  node: SyntaxTree.Node,
): Diagnostic.Diagnostic | undefined =>
  reference._tag === 'Resolved' && reference.declaration.foreign !== undefined
    ? Diagnostic.foreignFunctionNotFirstClass(reference.spelling, node.span)
    : undefined

export interface SectionContractResult {
  readonly substitution: Type.Substitution
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly valid: boolean
}

/** A section binds its written arguments to the captured parameter ordinals, in order. */
export const sectionSpecializationSites = (
  contract: CallableContract.CallableContract,
  arguments_: ReadonlyArray<ArgumentFact>,
  captured: ReadonlyArray<number>,
): ReadonlyArray<SpecializationSite> =>
  Object.freeze(
    arguments_.flatMap((argument, ordinal): ReadonlyArray<SpecializationSite> => {
      const parameter = parameterAt(contract.parameters, captured, ordinal)
      return argument.type._tag === 'Available' && parameter !== undefined
        ? [
            Object.freeze({
              ordinal,
              pattern: parameter.type,
              actual: argument.type.type,
              expression: argument.expression,
            }),
          ]
        : []
    }),
  )

export const analyzeSectionContract = (
  call: SyntaxTree.Node,
  reference: Extract<
    CallReferenceFact,
    { readonly _tag: 'Resolved' | 'ResolvedBuiltin' | 'ResolvedIntrinsicContract' }
  >,
  arguments_: ReadonlyArray<ArgumentFact>,
  callTypeArguments: CallTypeArgumentsResult,
  captured: ReadonlyArray<number>,
  resolution?: ResolutionContext,
): SectionContractResult => {
  if (reference._tag === 'ResolvedBuiltin') {
    const diagnostics = arguments_.flatMap((argument, ordinal) => {
      if (argument.type._tag !== 'Available') return []
      const expected = parameterAt(reference.parameters, captured, ordinal)
      if (
        expected === undefined ||
        typesCompatible(argument.type.type, expected, resolution?.lifetimeCompatibility)
      )
        return []
      return [
        Diagnostic.argumentTypeMismatch(
          Type.encode(expected),
          Type.encode(argument.type.type),
          argument.syntax.span,
        ),
      ]
    })
    if (callTypeArguments.explicit)
      diagnostics.push(
        Diagnostic.typeArgumentArity(
          reference.spelling,
          0,
          callTypeArguments.facts.length,
          call.span,
        ),
      )
    return Object.freeze({
      substitution: new Map(),
      typeArguments: Object.freeze([]),
      diagnostics: Object.freeze(diagnostics),
      valid:
        diagnostics.length === 0 &&
        arguments_.every((argument) => argument.type._tag === 'Available'),
    })
  }

  const callable = resolvedCallableContract(reference)
  if (callable === undefined) throw new RangeError('section lost its callable contract')
  const capturedLifetimes = new Set(
    captured.flatMap((ordinal) => {
      const parameter = callable.parameters.at(ordinal)
      return parameter === undefined ? [] : Type.storageLifetimes(parameter.type).map(Lifetime.key)
    }),
  )
  const capturedBinders = callable.lifetimeBinders.filter((binder) =>
    capturedLifetimes.has(Lifetime.key(binder)),
  )
  const remainingBinders = callable.lifetimeBinders.filter(
    (binder) => !capturedLifetimes.has(Lifetime.key(binder)),
  )
  const remainingLifetimeKeys = new Set(remainingBinders.map(Lifetime.key))
  const callLifetimes = selectedCallLifetimes(call, capturedBinders, resolution)
  const declaredParameters = callable.binders
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const contradicted = new Set<number>()
  let substitution = new Map<string, Type.GenericArgument>(callLifetimes.substitution)
  if (callTypeArguments.explicit) {
    if (
      callTypeArguments.types === undefined ||
      callTypeArguments.facts.length > declaredParameters.length
    ) {
      diagnostics.push(
        Diagnostic.typeArgumentArity(
          reference.spelling,
          declaredParameters.length,
          callTypeArguments.facts.length,
          call.span,
        ),
      )
    } else {
      // Written and inferred type arguments specialize the captured parameters while every
      // remaining parameter stays available to later application.
      const remaining = remainingOf(callable.parameters, captured)
      const constraintDeferred = callable.constraints.flatMap((constraint) =>
        constraint._tag === 'ProviderSelectionConstraint' &&
        constraint.selected.expression._tag === 'RowParameter'
          ? [Type.key(constraint.selected.expression.parameter)]
          : [],
      )
      const seeded = seededSpecialization(
        reference.spelling,
        declaredParameters,
        callTypeArguments.facts,
        sectionSpecializationSites(callable, arguments_, captured),
        call.span,
        new Set([
          ...remainingLifetimeKeys,
          ...remaining.flatMap((parameter) => Type.parameters(parameter.type).map(Type.key)),
          ...constraintDeferred,
        ]),
        resolution?.staticContext?.typeSubstitution,
        callLifetimes,
      )
      substitution = new Map(seeded.substitution)
      for (const conflict of seeded.conflicts) {
        diagnostics.push(conflict.diagnostic)
        if (conflict.ordinal !== undefined) contradicted.add(conflict.ordinal)
      }
      if (seeded.unresolved !== undefined) diagnostics.push(seeded.unresolved)
    }
  } else {
    for (const [ordinal, argument] of arguments_.entries()) {
      const parameter = parameterAt(callable.parameters, captured, ordinal)
      if (
        argument.type._tag === 'Available' &&
        parameter !== undefined &&
        !TypeInference.infer(
          parameter.type,
          argument.type.type,
          substitution,
          callLifetimes.inference,
        )
      ) {
        const rowFailure = TypeInference.rowInferenceFailure(parameter.type, argument.type.type)
        diagnostics.push(
          rowFailure === undefined
            ? Diagnostic.typeArgumentInference(reference.spelling, call.span)
            : Diagnostic.contractRowInference(rowFailure, call.span),
        )
        break
      }
    }
    const remaining = remainingOf(callable.parameters, captured)
    const deferred = new Set([
      ...remainingLifetimeKeys,
      ...remaining.flatMap((parameter) => Type.parameters(parameter.type).map(Type.key)),
      ...callable.constraints.flatMap((constraint) =>
        constraint._tag === 'ProviderSelectionConstraint' &&
        constraint.selected.expression._tag === 'RowParameter'
          ? [Type.key(constraint.selected.expression.parameter)]
          : [],
      ),
    ])
    if (
      declaredParameters.some(
        (parameter) => !substitution.has(Type.key(parameter)) && !deferred.has(Type.key(parameter)),
      )
    ) {
      diagnostics.push(Diagnostic.typeArgumentInference(reference.spelling, call.span))
    }
  }
  for (const [ordinal, argument] of arguments_.entries()) {
    const parameter = parameterAt(callable.parameters, captured, ordinal)
    if (argument.type._tag !== 'Available' || parameter === undefined) continue
    // An argument already named as contradicting a written type argument is one mistake, and it
    // was reported where the author wrote the type.
    if (contradicted.has(ordinal)) continue
    const expected = Type.substitute(parameter.type, substitution)
    if (
      !Type.isConcrete(expected) ||
      typesCompatible(argument.type.type, expected, resolution?.lifetimeCompatibility)
    )
      continue
    diagnostics.push(
      Diagnostic.argumentTypeMismatch(
        Type.encode(expected),
        Type.encode(argument.type.type),
        argument.syntax.span,
      ),
    )
  }
  diagnostics.push(
    ...selectedLifetimeBoundDiagnostics(
      callable.lifetimeBounds.filter(
        (bound) =>
          !remainingLifetimeKeys.has(Lifetime.key(bound.longer)) &&
          !remainingLifetimeKeys.has(Lifetime.key(bound.shorter)),
      ),
      substitution,
      resolution?.lifetimeCompatibility,
      call.span,
      callable.typeOutlives.filter(
        (bound) => !remainingLifetimeKeys.has(Lifetime.key(bound.lifetime)),
      ),
    ),
  )
  const typeArguments = Object.freeze(
    declaredParameters.flatMap((parameter) => {
      const inferred = substitution.get(Type.key(parameter))
      return inferred === undefined ? [] : [inferred]
    }),
  )
  return Object.freeze({
    substitution,
    typeArguments,
    diagnostics: Object.freeze(diagnostics),
    valid:
      diagnostics.length === 0 &&
      arguments_.every((argument) => argument.type._tag === 'Available'),
  })
}

export const captureAccess = (
  expression: ExpressionFact,
  index: DeclarationIndex.Index | undefined,
  assumptions: ReadonlySet<string> = new Set(),
): CallableCaptureFact['access'] => {
  if (expression._tag === 'Move')
    return expression.subject.type._tag === 'Available' &&
      index !== undefined &&
      ConformanceProof.copyType(index, expression.subject.type.type, assumptions)
      ? 'Copy'
      : 'Take'
  if (expression._tag === 'Borrow')
    return expression.access === 'Exclusive' ? 'Exclusive' : 'Shared'
  if (expression._tag === 'Grouped') return captureAccess(expression.expression, index, assumptions)
  if (expression.type._tag === 'Available' && Type.isCallable(expression.type.type))
    return expression.type.type.mode === 'Shared' ? 'Copy' : expression.type.type.mode
  if (expression.type._tag === 'Available' && Type.isEffect(expression.type.type))
    return expression.type.type.access === 'Shared' ? 'Copy' : expression.type.type.access
  // An owned affine value (a fresh temporary or a call result) is captured by ownership whether
  // or not the source spelled `move`; the environment then cleans it exactly once.
  if (
    expression.type._tag === 'Available' &&
    index !== undefined &&
    !Type.isReference(expression.type.type) &&
    !Type.isSlice(expression.type.type) &&
    !ConformanceProof.copyType(index, expression.type.type, assumptions)
  )
    return 'Take'
  return 'Copy'
}

export const ownedProviderCaptureAccess = (
  expression: ExpressionFact,
  index: DeclarationIndex.Index,
  assumptions: ReadonlySet<string> = new Set(),
): CallableCaptureFact['access'] =>
  expression._tag === 'Move' &&
  expression.subject.type._tag === 'Available' &&
  ConformanceProof.copyType(index, expression.subject.type.type, assumptions)
    ? 'Copy'
    : captureAccess(expression, index, assumptions)

type ExactCallableFact = Extract<
  ExpressionFact,
  { readonly _tag: 'FunctionItem' | 'CallableSection' }
>

export const exactCallableOf = (
  expression: ExpressionFact,
  writtenBindings: ReadonlySet<number> = new Set(),
): ExactCallableFact | undefined => {
  if (expression._tag === 'Grouped' || expression._tag === 'Move') {
    return exactCallableOf(
      expression._tag === 'Grouped' ? expression.expression : expression.subject,
      writtenBindings,
    )
  }
  if (expression._tag === 'FunctionItem' || expression._tag === 'CallableSection') return expression
  if (expression._tag === 'Identifier' && expression.reference._tag === 'ResolvedBinding') {
    if (writtenBindings.has(expression.reference.binding.id.ordinal)) return undefined
    return expression.reference.binding.exactCallable
  }
  return undefined
}

export const concreteCallableIdentity = (
  expression: ExpressionFact,
  writtenBindings: ReadonlySet<number> = new Set(),
): boolean => {
  if (exactCallableOf(expression, writtenBindings) !== undefined) return true
  if (expression._tag === 'Grouped' || expression._tag === 'Move') {
    return concreteCallableIdentity(
      expression._tag === 'Grouped' ? expression.expression : expression.subject,
      writtenBindings,
    )
  }
  if (expression._tag === 'Identifier' && expression.reference._tag === 'ResolvedBinding') {
    if (writtenBindings.has(expression.reference.binding.id.ordinal)) return false
    return expression.reference.binding.concreteCallableIdentity === true
  }
  return expression._tag === 'Call' && expression.reference._tag === 'Resolved'
}

export const callableMode = (captures: ReadonlyArray<CallableCaptureFact>): Type.CallableMode =>
  strongestEffectAccess(
    ...captures.flatMap((capture) => (capture.access === 'Copy' ? [] : [capture.access])),
  )

export const sectionCallableType = (
  reference: Extract<
    CallReferenceFact,
    { readonly _tag: 'Resolved' | 'ResolvedBuiltin' | 'ResolvedIntrinsicContract' }
  >,
  substitution: Type.Substitution,
  mode: Type.CallableMode,
  captured: ReadonlyArray<number>,
  lifetimes: Type.ExecutableLifetimes | undefined,
): Type.Callable | undefined => {
  if (lifetimes === undefined) return undefined
  if (reference._tag === 'ResolvedBuiltin')
    return Type.callable(
      remainingOf(reference.parameters, captured).map((parameter) =>
        Type.substitute(parameter, substitution),
      ),
      Type.substitute(reference.result, substitution),
      lifetimes,
      mode,
      undefined,
      reference.unsafe,
    )
  const contract = resolvedCallableContract(reference)
  const result = contract?.result
  if (contract === undefined || result === undefined) return undefined
  const remaining = remainingOf(contract.parameters, captured)
  return Type.callable(
    remaining.map((parameter) => Type.substitute(parameter.type, substitution)),
    Type.substitute(result, substitution),
    {
      ...lifetimes,
      lifetimeBinders: contract.lifetimeBinders.filter(
        (binder) => !substitution.has(Lifetime.key(binder)),
      ),
      lifetimeBounds: [
        ...(lifetimes.lifetimeBounds ?? []),
        ...contract.lifetimeBounds.map((bound) => ({
          longer: Type.substituteLifetime(bound.longer, substitution),
          shorter: Type.substituteLifetime(bound.shorter, substitution),
        })),
      ],
      typeOutlives: [
        ...(lifetimes.typeOutlives ?? []),
        ...contract.typeOutlives.map((bound) => ({
          type: Type.substitute(bound.type, substitution),
          lifetime: Type.substituteLifetime(bound.lifetime, substitution),
        })),
      ],
    },
    mode,
    contract.constraints.length === 0 && contract.binders.length === 0
      ? undefined
      : Object.freeze({
          ...(reference._tag === 'Resolved' && reference.declaration.canonical._tag === 'Canonical'
            ? { source: reference.declaration.canonical.id }
            : {}),
          contract,
          binders: contract.binders,
          constraints: contract.constraints,
          evidence: Object.freeze([]),
          substitution,
          contractKey: CallableContract.key(contract),
          constraintKeys: Object.freeze(contract.constraints.map(Constraint.key)),
          evidenceKeys: Object.freeze([]),
          origins: constraintOrigins(sourceCallable(reference)),
        }),
    contract.unsafe,
  )
}

export const callableSectionOf = (
  expression: ExpressionFact,
  writtenBindings: ReadonlySet<number> = new Set(),
): CallableSectionExpressionFact | undefined => {
  const exact = exactCallableOf(expression, writtenBindings)
  return exact?._tag === 'CallableSection' ? exact : undefined
}

export function executableSite(
  tag: 'CallableSiteId',
  resolution: ResolutionContext,
  node: SyntaxTree.Node,
): Hir.CallableSiteId
export function executableSite(
  tag: 'EffectSiteId',
  resolution: ResolutionContext,
  node: SyntaxTree.Node,
): Hir.EffectSiteId
export function executableSite(
  tag: 'CallableSiteId' | 'EffectSiteId',
  resolution: ResolutionContext,
  node: SyntaxTree.Node,
): Hir.CallableSiteId | Hir.EffectSiteId {
  const ordinal = resolution.executableSites?.get(node) ?? 0
  return Object.freeze({
    _tag: tag,
    function:
      resolution.executableFunction ??
      Object.freeze({ _tag: 'DeclarationId', sourceId: node.span.sourceId, ordinal: 0 }),
    ...(resolution.executableOwner === undefined ? {} : { owner: resolution.executableOwner }),
    ordinal,
    span: node.span,
  })
}

export const executableSites = (root: SyntaxTree.Node): ReadonlyMap<SyntaxTree.Node, number> => {
  const sites = new Map<SyntaxTree.Node, number>()
  const isAppliedInterfacePipeline = (node: SyntaxTree.Node): boolean => {
    if (node.kind !== 'PipelineExpression') return false
    let target = pipelineCallable(node)
    while (target?.kind === 'GroupedExpression') target = target.children.find(isExpressionNode)
    return target?.kind === 'AppliedMemberExpression'
  }
  const visit = (node: SyntaxTree.Node): void => {
    if (
      node.kind === 'CallExpression' ||
      node.kind === 'EffectExpression' ||
      node.kind === 'AnonymousCallableExpression' ||
      isAppliedInterfacePipeline(node)
    )
      sites.set(node, sites.size)
    for (const child of node.children) if (SyntaxTree.isNode(child)) visit(child)
  }
  visit(root)
  // A bound method value is a section at a projection. Those sites follow every call site so the
  // ordinals of existing sites never move.
  const visitProjections = (node: SyntaxTree.Node): void => {
    if (node.kind === 'FieldProjectionExpression') sites.set(node, sites.size)
    for (const child of node.children) if (SyntaxTree.isNode(child)) visitProjections(child)
  }
  visitProjections(root)
  return sites
}

export const executableSpecializationOwner = (
  resolution: ResolutionContext,
): Type.ExecutableSpecializationOwner | undefined => {
  const owner = resolution.executableOwner
  if (owner === undefined) return undefined
  const declaration = DeclarationFacts.byCanonical(resolution.index, owner)
  return declaration === undefined
    ? undefined
    : Object.freeze({
        declaration: Object.freeze({ module: owner.module, name: owner.name }),
        typeArguments: Object.freeze(
          declaration.typeParameters.map((parameter) => Type.parameterArgument(parameter.type)),
        ),
      })
}

export const finishCallableSection = (
  node: SyntaxTree.Node,
  reference: Extract<
    CallReferenceFact,
    { readonly _tag: 'Resolved' | 'ResolvedBuiltin' | 'ResolvedIntrinsicContract' }
  >,
  argumentsResult: ArgumentsResult,
  callTypeArguments: CallTypeArgumentsResult,
  resolution: ResolutionContext,
  caller: DeclarationFact,
  captured?: ReadonlyArray<number>,
  path: ReferencePathFact = referencePath(node),
): ExpressionResult => {
  const parameterCount =
    reference._tag === 'ResolvedBuiltin'
      ? reference.parameters.length
      : (resolvedCallableContract(reference)?.parameters.length ?? 0)
  const capturedParameters =
    captured ?? trailingCaptures(parameterCount, argumentsResult.facts.length)
  const contract = analyzeSectionContract(
    node,
    reference,
    argumentsResult.facts,
    callTypeArguments,
    capturedParameters,
    resolution,
  )
  const constraints = interfaceConstraints(
    reference,
    contract.substitution,
    resolution.index,
    caller,
    node.span,
  )
  const captures = Object.freeze(
    capturedParameters.map((parameterOrdinal, ordinal) => {
      const argument = argumentsResult.facts.at(ordinal)
      if (argument === undefined) throw new RangeError('section capture lost its argument')
      return Object.freeze({
        _tag: 'CallableCapture' as const,
        ordinal,
        parameterOrdinal,
        expression: argument.expression,
        access:
          ordinal === 0 &&
          reference._tag === 'ResolvedIntrinsicContract' &&
          reference.intrinsic.rule._tag === 'ContractRule' &&
          reference.intrinsic.rule.post === 'BindRequirement' &&
          reference.intrinsic.rule.providerMode === 'Take'
            ? ownedProviderCaptureAccess(
                argument.expression,
                resolution.index,
                copyAssumptionsOf(caller),
              )
            : captureAccess(argument.expression, resolution.index, copyAssumptionsOf(caller)),
      })
    }),
  )
  const mode = callableMode(captures)
  const callable = sectionCallableType(
    reference,
    contract.substitution,
    mode,
    capturedParameters,
    BodyLifetime.environment(
      resolution.bodyLifetimes,
      node,
      captures.flatMap((capture) =>
        capture.expression.type._tag === 'Available' ? [capture.expression.type.type] : [],
      ),
      captures
        .filter((capture) => capture.access === 'Shared' || capture.access === 'Exclusive')
        .map((capture) => capture.expression.syntax),
    ),
  )
  const foreign = foreignFirstClassDiagnostic(reference, node)
  const type =
    contract.valid && callable !== undefined && foreign === undefined
      ? availableExpressionType(callable)
      : unavailableExpressionType
  const environmentOwner = executableSpecializationOwner(resolution)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'CallableSection',
      selectedConformances: constraints.proofs,
      site: executableSite('CallableSiteId', resolution, node),
      reference,
      path,
      remainingParameters: remainingOf(
        Array.from({ length: parameterCount }, (_, ordinal) => ordinal),
        capturedParameters,
      ),
      captures,
      retainedDependencies: Object.freeze(
        captures.flatMap((capture) =>
          capture.access === 'Shared' || capture.access === 'Exclusive'
            ? [capture.parameterOrdinal]
            : [],
        ),
      ),
      typeArguments: contract.typeArguments,
      ...(environmentOwner === undefined ? {} : { environmentOwner }),
      substitution: contract.substitution,
      mode,
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      ...argumentsResult.diagnostics,
      ...callTypeArguments.diagnostics,
      ...contract.diagnostics,
      ...constraints.diagnostics,
      ...(foreign === undefined ? [] : [foreign]),
    ]),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

export const finishCallableApplication = (
  node: SyntaxTree.Node,
  callee: ExpressionResult,
  argumentsResult: ArgumentsResult,
  callTypeArguments: CallTypeArgumentsResult,
  provenance?: CallableApplyExpressionFact['provenance'],
  resolution?: ResolutionContext,
  caller?: DeclarationFact,
): ExpressionResult => {
  let callable: Type.Callable | undefined
  if (callee.type !== undefined && Type.isCallable(callee.type)) {
    callable = callee.type
  } else if (
    callee.type !== undefined &&
    Type.isRepresented(callee.type) &&
    Type.isCallable(callee.type.contract)
  ) {
    callable = callee.type.contract
  } else {
    callable = undefined
  }
  const diagnostics: Array<Diagnostic.Diagnostic> = [
    ...callee.diagnostics,
    ...argumentsResult.diagnostics,
    ...callTypeArguments.diagnostics,
  ]
  const writtenBindings = resolution?.writtenCallableBindings ?? new Set<number>()
  const exactCallable = exactCallableOf(callee.fact, writtenBindings)
  const section = exactCallable?._tag === 'CallableSection' ? exactCallable : undefined
  const directSection = callee.fact._tag === 'CallableSection' ? callee.fact : undefined
  const stagedSection =
    directSection !== undefined &&
    callable !== undefined &&
    argumentsResult.facts.length > 0 &&
    argumentsResult.facts.length < callable.parameters.length &&
    resolution !== undefined &&
    caller !== undefined
      ? directSection
      : undefined
  // A proper trailing suffix supplied to a callable value stages a further section over that
  // value (CALLABLE-002); the value's own environment is spliced ahead of these captures.
  const stagedValue =
    directSection === undefined &&
    callable !== undefined &&
    argumentsResult.facts.length > 0 &&
    argumentsResult.facts.length < callable.parameters.length &&
    resolution !== undefined &&
    caller !== undefined
      ? Object.freeze({
          site: executableSite('CallableSiteId', resolution, node),
          captures: Object.freeze(
            argumentsResult.facts.map((argument, ordinal) =>
              Object.freeze({
                _tag: 'CallableCapture' as const,
                ordinal,
                parameterOrdinal:
                  callable.parameters.length - argumentsResult.facts.length + ordinal,
                expression: argument.expression,
                access: captureAccess(
                  argument.expression,
                  resolution.index,
                  copyAssumptionsOf(caller),
                ),
              }),
            ),
          ),
        })
      : undefined
  const schema = callable?.schema
  const callLifetimes = selectedCallLifetimes(
    node,
    callable?.lifetimeBinders ?? [],
    resolution,
    schema?.substitution ?? section?.substitution,
  )
  const inferred = new Map<string, Type.GenericArgument>(callLifetimes.substitution)
  let evidence: ReadonlyArray<Constraint.ConstraintEvidence> = Object.freeze([])
  let inferredProviderSelectors: ReadonlyArray<InferredProviderSelector> = Object.freeze([])
  let valid =
    callable !== undefined &&
    (node.kind === 'PipelineExpression' ? isAvailableSyntax(node) : hasAvailableCallSyntax(node))
  if (callable === undefined && callee.type !== undefined) {
    diagnostics.push(
      Diagnostic.nonCallableApplication(Type.encode(callee.type), callee.fact.syntax.span),
    )
  }

  if (
    callable?.mode === 'Exclusive' &&
    callee.fact._tag === 'Identifier' &&
    callee.fact.reference._tag === 'ResolvedBinding' &&
    callee.fact.reference.binding.mutability !== 'Mutable'
  ) {
    diagnostics.push(
      Diagnostic.invalidCallableInvocationAccess('Exclusive', callee.fact.syntax.span),
    )
    valid = false
  }
  if (
    schema !== undefined &&
    schema.source === undefined &&
    !concreteCallableIdentity(callee.fact, writtenBindings)
  ) {
    diagnostics.push(Diagnostic.nonConcreteSpecialization('constrained callable', node.span))
    valid = false
  }
  if (callTypeArguments.explicit) {
    diagnostics.push(
      Diagnostic.typeArgumentArity('callable value', 0, callTypeArguments.facts.length, node.span),
    )
    valid = false
  }
  if (
    callable !== undefined &&
    callable.parameters.length !== argumentsResult.facts.length &&
    stagedSection === undefined &&
    stagedValue === undefined
  ) {
    diagnostics.push(
      Diagnostic.wrongCallArity(
        Object.freeze({ _tag: 'BuiltinTarget', actor: 'Callable', operation: 'Apply' }),
        callable.parameters.length,
        argumentsResult.facts.length,
        node.span,
      ),
    )
    valid = false
  }
  const completeUnsafeInvocation =
    callable !== undefined &&
    callable.unsafe === true &&
    stagedSection === undefined &&
    callable.parameters.length === argumentsResult.facts.length
  if (completeUnsafeInvocation && callable !== undefined) {
    const diagnostic = unsafeCallDiagnostic(true, Type.encode(callable), node, resolution)
    if (diagnostic !== undefined) {
      diagnostics.push(diagnostic)
      valid = false
    }
  }
  if (callable !== undefined) {
    const parameterOffset =
      stagedSection === undefined && stagedValue === undefined
        ? 0
        : callable.parameters.length - argumentsResult.facts.length
    for (const [ordinal, argument] of argumentsResult.facts.entries()) {
      const expected = callable.parameters.at(parameterOffset + ordinal)
      if (expected === undefined || argument.type._tag !== 'Available') {
        valid = false
        continue
      }
      if (!TypeInference.infer(expected, argument.type.type, inferred, callLifetimes.inference)) {
        const rowFailure = TypeInference.rowInferenceFailure(expected, argument.type.type)
        if (Type.isForeignFunction(expected) && !Type.isForeignFunction(argument.type.type)) {
          diagnostics.push(
            Diagnostic.invalidForeignCallback(
              Type.encode(argument.type.type),
              'capturing and anonymous Silk callables do not have an exported C address',
              argument.syntax.span,
            ),
          )
        } else if (rowFailure !== undefined) {
          diagnostics.push(Diagnostic.contractRowInference(rowFailure, argument.syntax.span))
        } else if (Type.isCallable(expected) && Type.isCallable(argument.type.type)) {
          diagnostics.push(
            Diagnostic.incompatibleCallableSignature(
              Type.encode(expected),
              Type.encode(argument.type.type),
              argument.syntax.span,
            ),
          )
        } else {
          diagnostics.push(
            Diagnostic.argumentTypeMismatch(
              Type.encode(expected),
              Type.encode(argument.type.type),
              argument.syntax.span,
            ),
          )
        }
        valid = false
        continue
      }
      const specialized = Type.substitute(expected, inferred)
      if (
        Type.isConcrete(specialized) &&
        !typesCompatible(argument.type.type, specialized, callLifetimes.compatibility)
      ) {
        let mismatch: Diagnostic.Diagnostic
        if (Type.isForeignFunction(specialized) && !Type.isForeignFunction(argument.type.type))
          mismatch = Diagnostic.invalidForeignCallback(
            Type.encode(argument.type.type),
            'capturing and anonymous Silk callables do not have an exported C address',
            argument.syntax.span,
          )
        else if (Type.isCallable(specialized) && Type.isCallable(argument.type.type))
          mismatch = Diagnostic.incompatibleCallableSignature(
            Type.encode(specialized),
            Type.encode(argument.type.type),
            argument.syntax.span,
          )
        else
          mismatch = Diagnostic.argumentTypeMismatch(
            Type.encode(specialized),
            Type.encode(argument.type.type),
            argument.syntax.span,
          )
        diagnostics.push(mismatch)
        valid = false
      }
    }
  }
  if (callable !== undefined) {
    const lifetimeDiagnostics = selectedLifetimeBoundDiagnostics(
      callable.lifetimeBounds,
      inferred,
      callLifetimes.compatibility,
      node.span,
      callable.typeOutlives,
    )
    diagnostics.push(...lifetimeDiagnostics)
    if (lifetimeDiagnostics.length !== 0) valid = false
  }
  const stagedCaptures =
    stagedSection === undefined || resolution === undefined || caller === undefined
      ? undefined
      : Object.freeze([
          ...stagedSection.captures,
          ...argumentsResult.facts.map((argument, ordinal) => {
            const remainingOffset =
              stagedSection.remainingParameters.length - argumentsResult.facts.length
            const parameterOrdinal = stagedSection.remainingParameters.at(remainingOffset + ordinal)
            if (parameterOrdinal === undefined)
              throw new RangeError('staged callable section lost a remaining parameter')
            return Object.freeze({
              _tag: 'CallableCapture' as const,
              ordinal: stagedSection.captures.length + ordinal,
              parameterOrdinal,
              expression: argument.expression,
              access: captureAccess(
                argument.expression,
                resolution.index,
                copyAssumptionsOf(caller),
              ),
            })
          }),
        ])
  if (
    valid &&
    (schema !== undefined || section !== undefined) &&
    resolution !== undefined &&
    (schema?.constraints.length ??
      (section === undefined
        ? 0
        : resolvedCallableContract(section.reference)?.constraints.length) ??
      0) > 0
  ) {
    const sectionContract =
      schema === undefined && section !== undefined
        ? resolvedCallableContract(section.reference)
        : undefined
    const constraints = schema?.constraints ?? sectionContract?.constraints
    if (constraints === undefined) throw new RangeError('section lost its callable contract')
    const solved = solveCallableConstraints(
      constraints,
      schema?.origins ??
        (section === undefined
          ? Object.freeze([])
          : constraintOrigins(sourceCallable(section.reference))),
      inferred,
      caller,
      resolution,
      node.span,
    )
    inferred.clear()
    for (const [identity, argument] of solved.substitution) inferred.set(identity, argument)
    evidence = Object.freeze([...(schema?.evidence ?? []), ...solved.evidence])
    inferredProviderSelectors = solved.inferredProviderSelectors
    diagnostics.push(...solved.diagnostics)
    if (solved.diagnostics.length > 0) valid = false
  }
  const schemaDeclaration =
    schema?.source === undefined || resolution === undefined
      ? undefined
      : DeclarationFacts.byCanonical(resolution.index, {
          _tag: 'CanonicalDeclarationId',
          ...schema.source,
        })
  let sourceTarget: Extract<CallReferenceFact, { readonly _tag: 'Resolved' }> | undefined
  if (exactCallable?.reference._tag === 'Resolved') sourceTarget = exactCallable.reference
  else if (
    schemaDeclaration?._tag === 'FunctionDeclaration' &&
    schemaDeclaration.name._tag === 'Present'
  )
    sourceTarget = {
      _tag: 'Resolved',
      spelling: schemaDeclaration.name.spelling,
      token: schemaDeclaration.name.token,
      declaration: schemaDeclaration,
    }
  const selectedConformances =
    sourceTarget === undefined || resolution === undefined || caller === undefined
      ? { diagnostics: [], proofs: [] }
      : interfaceConstraints(sourceTarget, inferred, resolution.index, caller, node.span)
  diagnostics.push(...selectedConformances.diagnostics)
  if (selectedConformances.diagnostics.length > 0) valid = false
  const type = (() => {
    if (!valid || callable === undefined) return unavailableExpressionType
    if (stagedSection !== undefined && stagedCaptures !== undefined) {
      const reference = stagedSection.reference
      if (
        reference._tag !== 'Resolved' &&
        reference._tag !== 'ResolvedBuiltin' &&
        reference._tag !== 'ResolvedIntrinsicContract'
      )
        return unavailableExpressionType
      const sectionType = sectionCallableType(
        reference,
        inferred,
        callableMode(stagedCaptures),
        stagedCaptures.map((capture) => capture.parameterOrdinal),
        BodyLifetime.environment(
          resolution?.bodyLifetimes,
          node,
          stagedCaptures.flatMap((capture) =>
            capture.expression.type._tag === 'Available' ? [capture.expression.type.type] : [],
          ),
          stagedCaptures
            .filter((capture) => capture.access === 'Shared' || capture.access === 'Exclusive')
            .map((capture) => capture.expression.syntax),
        ),
      )
      return sectionType === undefined
        ? unavailableExpressionType
        : availableExpressionType(sectionType)
    }
    const result = Type.substitute(callable.result, inferred)
    if (stagedValue !== undefined) {
      const lifetimes = BodyLifetime.environment(
        resolution?.bodyLifetimes,
        node,
        [
          callable,
          ...stagedValue.captures.flatMap((capture) =>
            capture.expression.type._tag === 'Available' ? [capture.expression.type.type] : [],
          ),
        ],
        stagedValue.captures
          .filter((capture) => capture.access === 'Shared' || capture.access === 'Exclusive')
          .map((capture) => capture.expression.syntax),
      )
      if (lifetimes === undefined) return unavailableExpressionType
      return availableExpressionType(
        Type.callable(
          callable.parameters
            .slice(0, callable.parameters.length - argumentsResult.facts.length)
            .map((parameter) => Type.substitute(parameter, inferred)),
          result,
          lifetimes,
          strongestEffectAccess(callable.mode, callableMode(stagedValue.captures)),
          callable.schema,
          callable.unsafe,
        ),
      )
    }
    return availableExpressionType(
      Type.isEffect(result)
        ? Type.effectWithRows(
            result.success,
            result.failureRow,
            result,
            strongestEffectAccess(
              result.access,
              callable.mode,
              effectExpressionAccess(
                callee.fact,
                resolution?.index,
                caller === undefined ? new Set() : copyAssumptionsOf(caller),
              ),
              effectCaptureAccess(
                argumentsResult.facts,
                resolution?.index,
                caller === undefined ? new Set() : copyAssumptionsOf(caller),
              ),
            ),
            result.requirementRow,
          )
        : result,
    )
  })()
  if (stagedSection !== undefined && stagedCaptures !== undefined && resolution !== undefined) {
    const remainingCount = stagedSection.remainingParameters.length - argumentsResult.facts.length
    const environmentOwner = executableSpecializationOwner(resolution)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'CallableSection',
        selectedConformances: selectedConformances.proofs,
        site: executableSite('CallableSiteId', resolution, node),
        reference: stagedSection.reference,
        path: stagedSection.path,
        remainingParameters: Object.freeze(
          stagedSection.remainingParameters.slice(0, remainingCount),
        ),
        captures: stagedCaptures,
        retainedDependencies: Object.freeze(
          stagedCaptures.flatMap((capture) =>
            capture.access === 'Shared' || capture.access === 'Exclusive'
              ? [capture.parameterOrdinal]
              : [],
          ),
        ),
        typeArguments: stagedSection.typeArguments,
        ...(environmentOwner === undefined ? {} : { environmentOwner }),
        substitution: inferred,
        mode: callableMode(stagedCaptures),
        type,
        syntax: node,
      }),
      diagnostics: Object.freeze(diagnostics),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }
  if (section?.reference._tag === 'ResolvedIntrinsicContract' && stagedValue === undefined) {
    const protected_ = argumentsResult.facts.at(0)
    if (
      section.reference.intrinsic.rule._tag === 'ContractRule' &&
      section.reference.intrinsic.rule.post === 'CatchFailure'
    ) {
      const handlerCapture = section.captures.find((capture) => capture.parameterOrdinal === 1)
      const handler = handlerCapture?.expression
      const wanted = section.reference.contract.constraints
        .map((constraint) => Constraint.substitute(constraint, inferred))
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
        handlerType !== undefined &&
        Type.isCallable(handlerType) &&
        Type.isEffect(handlerType.result)
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
          reference: sectionIntrinsicReference(section),
          protected: protected_?.expression ?? unavailableExpression(node),
          handler: handler ?? unavailableExpression(node),
          ...(wanted === undefined ? {} : { selected: Type.failureType(wanted.selected) }),
          protectedRow: wanted?.source ?? RowAlgebra.concrete(Type.failureRowPolicy(), []),
          handlerRow: handlerEffect?.failureRow ?? RowAlgebra.concrete(Type.failureRowPolicy(), []),
          residualRow:
            wanted === undefined
              ? RowAlgebra.concrete(Type.failureRowPolicy(), [])
              : RowAlgebra.without(Type.failureRowPolicy(), wanted.source, wanted.selected),
          evidence,
          type: catchAvailable ? type : unavailableExpressionType,
          syntax: node,
        }),
        diagnostics: Object.freeze(diagnostics),
        type: catchAvailable && type._tag === 'Available' ? type.type : undefined,
      })
    }
    const providerCapture = section.captures.find((capture) => capture.parameterOrdinal === 1)
    const provider =
      providerCapture === undefined
        ? undefined
        : effectBindingProvider(
            section.reference.intrinsic,
            inferred,
            evidence,
            providerCapture.expression,
            providerCapture.expression.syntax.span,
            resolution?.index,
          )
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'EffectBindRequirement',
        reference: sectionIntrinsicReference(section),
        protected: protected_?.expression ?? unavailableExpression(node),
        ...(type._tag === 'Available' && provider !== undefined ? { provider } : {}),
        type,
        syntax: node,
      }),
      diagnostics: Object.freeze(diagnostics),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }
  // Staging copies a shared environment and consumes any other, exactly as capturing the
  // callable value itself would.
  let mode: Type.CallableMode = callable?.mode ?? 'Shared'
  if (stagedValue !== undefined && mode !== 'Shared') mode = 'Take'
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'CallableApply',
      ...(sourceTarget === undefined ? {} : { sourceTarget }),
      selectedConformances: selectedConformances.proofs,
      callee: callee.fact,
      arguments: argumentsResult.facts,
      mode,
      ...(callable === undefined ? {} : { contract: callable }),
      substitution: inferred,
      inferredProviderSelectors,
      ...(stagedValue === undefined ? {} : { staged: stagedValue }),
      provenance: provenance ?? Object.freeze({ _tag: 'DirectCallableApplication' as const }),
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

/**
 * `Place.replace(place, value)`: the first argument resolves as a writable place under the same
 * rules as assignment, the second as a value of the place's type, and the whole expression
 * yields the place's previous value. The place stays initialized, so affine owners can leave a
 * struct field behind a reference without a partial move.
 */
export const unavailableIdentifierFact = (node: SyntaxTree.Node): ExpressionFact =>
  Object.freeze({
    _tag: 'Identifier',
    reference: Object.freeze({
      _tag: 'Unavailable' as const,
      syntax: unavailableSyntax(node, 'Identifier'),
    }),
    type: unavailableExpressionType,
    syntax: node,
  })
