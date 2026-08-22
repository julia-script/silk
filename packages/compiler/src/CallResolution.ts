import * as CallableContract from './CallableContract.js'
import * as ConformanceGoal from './ConformanceGoal.js'
import * as ConformanceProof from './ConformanceProof.js'
import * as Constraint from './Constraint.js'
import * as DeclarationCollection from './DeclarationCollection.js'
import * as DeclarationFacts from './DeclarationFacts.js'
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
  SemanticType,
  TypeArgumentFact,
} from './Elaboration.js'
import {
  argumentFact,
  availableExpressionType,
  callReferenceTokens,
  childNode,
  contextualIntegerCompatible,
  isAvailableSyntax,
  isRecursiveArgumentNode,
  lookupDeclaration,
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
import * as RequirementRow from './RequirementRow.js'
import * as RowAlgebra from './RowAlgebra.js'
import type * as SourceFile from './SourceFile.js'
import type * as SourceSpan from './SourceSpan.js'
import { unsafeCallDiagnostic } from './StatementAnalysis.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'

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
  const analyzed = nodes.flatMap((element, ordinal): ReadonlyArray<ExpressionResult> => {
    const result = analyzeExpression(
      source,
      element,
      declarations,
      declaration,
      scope,
      resolution,
      expectedTypes.at(ordinal),
      true,
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
  let boundParameters: ReadonlyArray<SemanticType> = Object.freeze([])
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
    const qualifierSpelling = spelling(source, first)
    const memberSpelling = spelling(source, second)
    const qualifier = NameResolution.lookup(resolution.scope, resolution.index, qualifierSpelling)
    if (qualifier._tag === 'Intrinsic') {
      const library =
        qualifierSpelling === 'Effect'
          ? DeclarationFacts.lookup(resolution.index, 'silk/effect', memberSpelling)
          : undefined
      if (
        library?._tag === 'Resolved' &&
        library.declaration._tag === 'FunctionDeclaration' &&
        library.declaration.visibility === 'Public'
      ) {
        target = library.declaration
      } else {
        const builtin = builtinSignature(qualifierSpelling, memberSpelling)
        const intrinsic = Intrinsic.findOperation(qualifierSpelling, memberSpelling)
        const contract =
          intrinsic?.rule._tag === 'ContractRule' ? intrinsic.rule.contract : undefined
        builtinParameters =
          builtin?.parameters ?? contract?.parameters.map((parameter) => parameter.type) ?? []
        builtinTypeParameters = builtin?.typeParameters ?? contract?.binders ?? []
      }
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
      if (bound?._tag === 'BoundOperation') boundParameters = bound.reference.parameters
      else if (qualifier.declaration.canonical._tag === 'Canonical') {
        const member = DeclarationFacts.lookup(
          resolution.index,
          qualifier.declaration.canonical.id.module,
          memberSpelling,
        )
        target =
          member._tag === 'Resolved' &&
          member.declaration._tag === 'FunctionDeclaration' &&
          member.declaration.visibility === 'Public'
            ? member.declaration
            : undefined
      }
    } else if (
      qualifier._tag === 'Resolved' &&
      (qualifier.declaration._tag === 'StructDeclaration' ||
        qualifier.declaration._tag === 'InterfaceDeclaration') &&
      qualifier.declaration.canonical._tag === 'Canonical'
    ) {
      // A nominal type doubles as an actor: `Vector.length(...)` names a public function of the
      // module that declares `Vector`. The call itself already resolves that way, but arguments are
      // analyzed first, and without the same lookup they get no expected types — which reads to a
      // borrow argument as "no borrow is wanted here" and rejects it as an invalid borrow position.
      const member = DeclarationFacts.lookup(
        resolution.index,
        qualifier.declaration.canonical.id.module,
        memberSpelling,
      )
      target =
        member._tag === 'Resolved' &&
        member.declaration._tag === 'FunctionDeclaration' &&
        member.declaration.visibility === 'Public'
          ? member.declaration
          : undefined
    }
  }
  const declaredTypeParameters =
    target?.typeParameters.map((parameter) => parameter.type) ?? Object.freeze([])
  const explicitTypes = callTypeArguments?.types
  const builtinSubstitution =
    callTypeArguments?.explicit === true &&
    explicitTypes !== undefined &&
    explicitTypes.length <= builtinTypeParameters.length
      ? TypeInference.prefixSubstitution(builtinTypeParameters, explicitTypes)
      : undefined
  // An explicit prefix is context for the value arguments just as a complete list is: the
  // parameters it binds become concrete expected types, and the ones it leaves open stay symbolic
  // exactly as they are when nothing was written.
  const substitution =
    callTypeArguments?.explicit === true && explicitTypes !== undefined
      ? TypeInference.prefixSubstitution(declaredTypeParameters, explicitTypes)
      : undefined
  const expectedTypes = Object.freeze(
    boundParameters.length > 0
      ? boundParameters
      : builtinParameters.length > 0
        ? builtinParameters
            .slice(
              isSectionArity(builtinParameters.length, argumentNodes.length)
                ? builtinParameters.length - argumentNodes.length
                : 0,
            )
            .map((parameter) => Type.substitute(parameter, builtinSubstitution ?? new Map()))
        : (target?.parameters ?? [])
            .slice(
              target !== undefined && isSectionArity(target.parameters.length, argumentNodes.length)
                ? target.parameters.length - argumentNodes.length
                : 0,
            )
            .map((parameter) =>
              parameter.declaredType._tag === 'Resolved'
                ? Type.substitute(parameter.declaredType.type, substitution ?? new Map())
                : undefined,
            ),
  )
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
  readonly types?: ReadonlyArray<SemanticType>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export const analyzeCallTypeArguments = (
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
      (element.kind === 'RequirementSelector' ||
        element.kind === 'TypePath' ||
        element.kind === 'AppliedType' ||
        element.kind === 'FixedArrayType' ||
        element.kind === 'SliceType' ||
        element.kind === 'ReferenceType' ||
        element.kind === 'CallableType' ||
        element.kind === 'ParenthesizedType' ||
        element.kind === 'UnionType'),
  )
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
    const raw = DeclarationCollection.analyzeDeclaredType(source, argumentNode, environment)
    const resolved = DeclarationResolution.resolveTypeFact(
      resolution.index,
      source.id,
      raw.fact,
      (module, path) => NameResolution.resolveType(nameResolution, resolution.index, module, path),
    )
    const invalidBorrow =
      resolved.fact._tag === 'Resolved' &&
      (Type.isReference(resolved.fact.type)
        ? Type.containsPositionRestrictedBorrow(resolved.fact.type.target)
        : Type.containsPositionRestrictedBorrow(resolved.fact.type))
        ? Diagnostic.sliceTypePosition('type argument', node.span)
        : undefined
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'TypeArgument' as const,
        ordinal,
        syntax: node,
        declared: resolved.fact,
        ...(requirementRole === undefined ? {} : { requirementRole }),
        ...(resolved.fact._tag === 'Resolved' &&
        invalidBorrow === undefined &&
        roleDiagnostics.length === 0
          ? { type: resolved.fact.type }
          : {}),
      }),
      diagnostics: Diagnostic.merge(
        raw.diagnostics,
        resolved.diagnostics,
        roleDiagnostics,
        ...(invalidBorrow === undefined ? [] : [[invalidBorrow]]),
      ),
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

export type SourceCallable = DeclarationFact | DeclarationFacts.ServiceOperationFact

export const sourceCallable = (reference: CallReferenceFact): SourceCallable | undefined =>
  reference._tag === 'Resolved'
    ? reference.declaration
    : reference._tag === 'ResolvedServiceOperation'
      ? reference.operation
      : undefined

export const resolvedCallableContract = (
  reference: CallReferenceFact,
): CallableContract.CallableContract | undefined => {
  if (reference._tag === 'ResolvedIntrinsicContract') return reference.contract
  const callable = sourceCallable(reference)
  return callable === undefined
    ? undefined
    : DeclarationFacts.callableContract(
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
        | 'ResolvedBoundOperation'
    }
  >,
  expectedCount: number,
  actualCount: number,
  span: SourceSpan.SourceSpan,
): Diagnostic.Diagnostic => {
  if (expectedCount === 1 && actualCount === 0)
    return Diagnostic.redundantUnaryEmptyCall(reference.spelling, span)
  return Diagnostic.wrongCallArity(
    reference._tag === 'ResolvedBuiltin'
      ? Object.freeze({
          _tag: 'BuiltinTarget',
          actor: reference.actor,
          operation: reference.operation,
        })
      : reference._tag === 'ResolvedIntrinsicContract'
        ? Object.freeze({
            _tag: 'BuiltinTarget',
            actor: 'Intrinsic',
            operation: reference.intrinsic.spelling,
          })
        : reference._tag === 'ResolvedBoundOperation'
          ? Object.freeze({
              _tag: 'BuiltinTarget',
              actor: reference.capability.name,
              operation: reference.operation,
            })
          : reference._tag === 'Resolved'
            ? reference.declaration.id
            : reference.operation.id,
    expectedCount,
    actualCount,
    span,
  )
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
): SeededSpecialization => {
  const written = new Map<string, TypeArgumentFact>()
  const seeded = new Map<string, Type.GenericArgument>()
  const conflicts: Array<SpecializationConflict> = []
  for (const fact of explicit) {
    const parameter = declared.at(fact.ordinal)
    const writtenType = fact.type
    if (parameter === undefined || writtenType === undefined) continue
    const argument: Type.GenericArgument | undefined =
      parameter.kind === 'Value' && Type.isTypeArgument(writtenType)
        ? writtenType
        : parameter.kind === 'RequirementRow'
          ? Type.isParameter(writtenType) && writtenType.kind === 'RequirementRow'
            ? Type.requirementRowArgument([], [writtenType])
            : Type.isNominal(writtenType) ||
                (Type.isParameter(writtenType) && writtenType.kind === 'Value')
              ? Type.isParameter(writtenType)
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
              : undefined
          : undefined
    if (argument === undefined) {
      conflicts.push(
        Object.freeze({
          diagnostic: Diagnostic.genericParameterKindMismatch(
            parameter.name,
            parameter.kind,
            Type.isNominal(writtenType) ? 'RequirementRow' : 'Value',
            fact.syntax.span,
          ),
        }),
      )
      continue
    }
    seeded.set(Type.key(parameter), argument)
    written.set(Type.key(parameter), fact)
  }
  const inferred = new Map(seeded)
  let rowFailure: Type.RowInferenceFailure | undefined
  for (const site of sites) {
    const attempt = new Map(inferred)
    if (TypeInference.infer(site.pattern, site.actual, attempt)) {
      commitSpecialization(inferred, attempt)
      continue
    }
    // Inference under the prefix failed. When the argument still satisfies what the prefix says
    // this parameter is, the written type simply wins — that is how a widened literal keeps
    // working under `take<u8>(1)`.
    const expected = Type.substitute(site.pattern, inferred)
    if (
      typesCompatible(site.actual, expected) ||
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
            declared.at(fact.ordinal)?.name ?? fact.ordinal.toString(),
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
): ReadonlyArray<SpecializationSite> =>
  Object.freeze(
    arguments_.flatMap((argument, ordinal): ReadonlyArray<SpecializationSite> => {
      const parameter = contract.parameters.at(ordinal)
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

export interface ConstraintSolveResult {
  readonly substitution: Type.Substitution
  readonly evidence: ReadonlyArray<Constraint.ConstraintEvidence>
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
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const givens = caller?.constraintContracts ?? Object.freeze([])
  const checked = constraints.flatMap((constraint, ordinal) =>
    constraint._tag === 'ProviderSelectionConstraint'
      ? []
      : [Object.freeze({ constraint, ordinal })],
  )
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
      oracle: Object.freeze({
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
      if (parameter?._tag === 'RowParameter')
        substitution.set(
          Type.key(parameter.parameter),
          Type.requirementRowArgument([solved.member]),
        )
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
  return Object.freeze({
    substitution,
    evidence: Object.freeze(evidence),
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
  // A bound operation's contract is a fixed parameter and result list over the bounded parameter,
  // exactly like a compiler-known operation's, so both are checked the same way.
  if (reference._tag === 'ResolvedBuiltin' || reference._tag === 'ResolvedBoundOperation') {
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
        !typesCompatible(argument.type.type, expected)
      ) {
        const mismatch =
          Type.isCallable(expected) && Type.isCallable(argument.type.type)
            ? Diagnostic.incompatibleCallableSignature(
                Type.encode(expected),
                Type.encode(argument.type.type),
                argument.syntax.span,
              )
            : (unionConversionDiagnostic(argument.type.type, expected, argument.syntax.span) ??
              Diagnostic.argumentTypeMismatch(
                Type.encode(expected),
                Type.encode(argument.type.type),
                argument.syntax.span,
              ))
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
  const sites = contractSpecializationSites(argumentsList, contract)
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
    substitution = new Map()
  } else {
    const inferred = new Map<string, Type.GenericArgument>()
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
        if (TypeInference.infer(pattern, representedSupplied, attempt)) {
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
      !typesCompatible(suppliedValue, expectedValue) &&
      !contextualIntegerCompatible(argument.expression, expectedValue)
    ) {
      const mismatch =
        Type.isCallable(expectedValue) && Type.isCallable(suppliedValue)
          ? Diagnostic.incompatibleCallableSignature(
              Type.encode(expectedValue),
              Type.encode(suppliedValue),
              argument.syntax.span,
            )
          : Type.isSlice(expectedValue) && Type.isFixedArray(suppliedValue)
            ? Diagnostic.implicitSliceDecay(Type.encode(expectedValue), argument.syntax.span)
            : (unionConversionDiagnostic(suppliedValue, expectedValue, argument.syntax.span) ??
              Diagnostic.argumentTypeMismatch(
                Type.encode(expectedValue),
                Type.encode(suppliedValue),
                argument.syntax.span,
              ))
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

  return Object.freeze({
    mappings,
    fact: Object.freeze({
      _tag: 'Compatible',
      expectedCount,
      actualCount,
      typeArguments,
      substitution,
      evidence,
    }),
    diagnostics: Object.freeze([]),
  })
}

export const interfaceConstraintDiagnostics = (
  reference: CallReferenceFact,
  contract: CallContractResult,
  index: DeclarationIndex.Index,
  caller: DeclarationFact,
  span: SourceSpan.SourceSpan,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  if (reference._tag !== 'Resolved' || contract.fact._tag !== 'Compatible') return Object.freeze([])
  const substitution = contract.fact.substitution
  return Object.freeze(
    reference.declaration.typeParameters.flatMap((parameter) => {
      const provider = substitution.get(Type.key(parameter.type))
      if (provider === undefined || !Type.isTypeArgument(provider)) return []
      return parameter.bounds.flatMap((bound): ReadonlyArray<Diagnostic.Diagnostic> => {
        if (bound._tag !== 'ResolvedBound')
          return [
            Diagnostic.invalidConformance(
              `unknown interface constraint ${bound.spelling}`,
              parameter.syntax.span,
            ),
          ]
        const substitutedCapability = Type.substitute(bound.application.capability, substitution)
        if (!Type.isNominal(substitutedCapability))
          return [
            Diagnostic.invalidConformance(
              `unknown interface constraint ${bound.spelling}`,
              parameter.syntax.span,
            ),
          ]
        const capability = substitutedCapability
        const callerCopyAssumptions = copyAssumptionsOf(caller)
        const assumedByCaller =
          Type.equals(capability, Type.copyCapability) &&
          ConformanceProof.copyType(index, provider, callerCopyAssumptions)
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
        if (!assumedByCaller && !ConformanceProof.conforms(index, provider, capability)) {
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
        return []
      })
    }),
  )
}

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
  readonly returnedBorrowParameter?: number
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
    ...(catalog.returnedBorrowParameter === undefined
      ? {}
      : { returnedBorrowParameter: catalog.returnedBorrowParameter }),
  })
}

export const callableResultType = (declaration: SourceCallable): SemanticType | undefined => {
  if (declaration.returnType._tag !== 'Resolved') return undefined
  if (declaration.functionKind === 'Ordinary') return declaration.returnType.type
  return Type.effectWithRows(
    declaration.returnType.type,
    declaration.failureRow.row,
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
    'Shared',
    contract === undefined || contract.constraints.length === 0
      ? undefined
      : Object.freeze({
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
  service: DeclarationFacts.ServiceFact,
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
  if (operation.declaration.typeParameters.length > 0 || operation.success._tag !== 'Resolved')
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
  interface_: DeclarationFacts.InterfaceFact,
  qualifier: string,
  member: string,
  memberToken: Token.Token,
):
  | {
      readonly _tag: 'BoundOperation'
      readonly reference: Extract<CallReferenceFact, { readonly _tag: 'ResolvedBoundOperation' }>
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
      _tag: 'ResolvedBoundOperation' as const,
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
    const declaration =
      resolved._tag === 'Resolved' && resolved.declaration._tag === 'FunctionDeclaration'
        ? resolved.declaration
        : local._tag === 'Resolved'
          ? local.declaration
          : undefined
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
    if (qualifier === 'Effect') {
      const library = DeclarationFacts.lookup(resolution.index, 'silk/effect', member)
      if (
        library._tag === 'Resolved' &&
        library.declaration._tag === 'FunctionDeclaration' &&
        library.declaration.visibility === 'Public'
      )
        return Object.freeze({
          _tag: 'Resolved',
          spelling: `${qualifier}.${member}`,
          token: second,
          declaration: library.declaration,
        })
    }
    const signature = builtinSignature(qualifier, member)
    return signature === undefined
      ? undefined
      : Object.freeze({
          _tag: 'ResolvedBuiltin',
          spelling: `${qualifier}.${member}`,
          token: second,
          actor: qualifier,
          operation: signature.operation,
          intrinsic: signature.id,
          parameters: signature.parameters,
          result: signature.result,
          unsafe: signature.unsafe === true,
          ...(signature.returnedBorrowParameter === undefined
            ? {}
            : { returnedBorrowParameter: signature.returnedBorrowParameter }),
        })
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
    const diagnostic =
      memberLookup._tag !== 'Resolved'
        ? Diagnostic.unknownImportedMember(qualifierLookup.module, member, memberToken.span)
        : memberLookup.declaration.visibility !== 'Public'
          ? Diagnostic.inaccessibleImportedMember(qualifierLookup.module, member, memberToken.span)
          : undefined
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
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([diagnostic]),
      type: undefined,
    })
  }
  const callable = callableTypeOfReference(reference)
  const type =
    callable === undefined ? unavailableExpressionType : availableExpressionType(callable)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FunctionItem',
      reference,
      path: referencePath(node),
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze([]),
    type: callable,
  })
}

export interface SectionContractResult {
  readonly substitution: Type.Substitution
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly valid: boolean
}

/** A section binds its written arguments to the callable's trailing parameter suffix. */
export const sectionSpecializationSites = (
  contract: CallableContract.CallableContract,
  arguments_: ReadonlyArray<ArgumentFact>,
): ReadonlyArray<SpecializationSite> =>
  Object.freeze(
    arguments_.flatMap((argument, ordinal): ReadonlyArray<SpecializationSite> => {
      const parameter = contract.parameters.at(
        contract.parameters.length - arguments_.length + ordinal,
      )
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
): SectionContractResult => {
  if (reference._tag === 'ResolvedBuiltin') {
    const captureStart = reference.parameters.length - arguments_.length
    const diagnostics = arguments_.flatMap((argument, ordinal) => {
      if (argument.type._tag !== 'Available') return []
      const expected = reference.parameters.at(captureStart + ordinal)
      if (expected === undefined || typesCompatible(argument.type.type, expected)) return []
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
  const declaredParameters = callable.binders
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const contradicted = new Set<number>()
  let substitution = new Map<string, Type.GenericArgument>()
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
      // A section binds one trailing suffix. Written and inferred type arguments specialize those
      // captures while every remaining leading parameter stays available to later application.
      const remaining = callable.parameters.slice(0, callable.parameters.length - arguments_.length)
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
        sectionSpecializationSites(callable, arguments_),
        call.span,
        new Set([
          ...remaining.flatMap((parameter) => Type.parameters(parameter.type).map(Type.key)),
          ...constraintDeferred,
        ]),
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
      const parameter = callable.parameters.at(
        callable.parameters.length - arguments_.length + ordinal,
      )
      if (
        argument.type._tag === 'Available' &&
        parameter !== undefined &&
        !TypeInference.infer(parameter.type, argument.type.type, substitution)
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
    const remaining = callable.parameters.slice(0, callable.parameters.length - arguments_.length)
    const deferred = new Set([
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
    const parameter = callable.parameters.at(
      callable.parameters.length - arguments_.length + ordinal,
    )
    if (argument.type._tag !== 'Available' || parameter === undefined) continue
    // An argument already named as contradicting a written type argument is one mistake, and it
    // was reported where the author wrote the type.
    if (contradicted.has(ordinal)) continue
    const expected = Type.substitute(parameter.type, substitution)
    if (!Type.isConcrete(expected) || typesCompatible(argument.type.type, expected)) continue
    diagnostics.push(
      Diagnostic.argumentTypeMismatch(
        Type.encode(expected),
        Type.encode(argument.type.type),
        argument.syntax.span,
      ),
    )
  }
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

export const concreteCallableIdentity = (expression: ExpressionFact): boolean => {
  if (expression._tag === 'Grouped' || expression._tag === 'Move') {
    return concreteCallableIdentity(
      expression._tag === 'Grouped' ? expression.expression : expression.subject,
    )
  }
  if (expression._tag === 'FunctionItem' || expression._tag === 'CallableSection') return true
  if (expression._tag === 'Identifier' && expression.reference._tag === 'ResolvedBinding') {
    return concreteCallableIdentity(expression.reference.binding.initializer)
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
  argumentCount: number,
): Type.Callable | undefined => {
  if (reference._tag === 'ResolvedBuiltin') {
    const remaining = reference.parameters.slice(0, reference.parameters.length - argumentCount)
    return remaining.length === 0
      ? undefined
      : Type.callable(remaining, reference.result, mode, undefined, reference.unsafe)
  }
  const contract = resolvedCallableContract(reference)
  const result = contract?.result
  if (contract === undefined || result === undefined) return undefined
  const remaining = contract.parameters.slice(0, contract.parameters.length - argumentCount)
  if (remaining.length === 0) return undefined
  return Type.callable(
    remaining.map((parameter) => Type.substitute(parameter.type, substitution)),
    Type.substitute(result, substitution),
    mode,
    contract.constraints.length === 0
      ? undefined
      : Object.freeze({
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
): CallableSectionExpressionFact | undefined => {
  if (expression._tag === 'CallableSection') return expression
  if (expression._tag === 'Identifier' && expression.reference._tag === 'ResolvedBinding')
    return callableSectionOf(expression.reference.binding.initializer)
  if (expression._tag === 'Move') return callableSectionOf(expression.subject)
  if (expression._tag === 'Grouped') return callableSectionOf(expression.expression)
  return undefined
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
  const visit = (node: SyntaxTree.Node): void => {
    if (node.kind === 'CallExpression' || node.kind === 'EffectExpression')
      sites.set(node, sites.size)
    for (const child of node.children) if (SyntaxTree.isNode(child)) visit(child)
  }
  visit(root)
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
        typeArguments: Object.freeze(declaration.typeParameters.map((parameter) => parameter.type)),
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
): ExpressionResult => {
  const contract = analyzeSectionContract(node, reference, argumentsResult.facts, callTypeArguments)
  const parameterCount =
    reference._tag === 'ResolvedBuiltin'
      ? reference.parameters.length
      : (resolvedCallableContract(reference)?.parameters.length ?? 0)
  const captureStart = parameterCount - argumentsResult.facts.length
  const captures = Object.freeze(
    argumentsResult.facts.map((argument, ordinal) =>
      Object.freeze({
        _tag: 'CallableCapture' as const,
        ordinal,
        parameterOrdinal: captureStart + ordinal,
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
      }),
    ),
  )
  const mode = callableMode(captures)
  const callable = sectionCallableType(
    reference,
    contract.substitution,
    mode,
    argumentsResult.facts.length,
  )
  const type =
    contract.valid && callable !== undefined
      ? availableExpressionType(callable)
      : unavailableExpressionType
  const environmentOwner = executableSpecializationOwner(resolution)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'CallableSection',
      site: executableSite('CallableSiteId', resolution, node),
      reference,
      path: referencePath(node),
      remainingParameters: Object.freeze(
        Array.from({ length: captureStart }, (_, ordinal) => ordinal),
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
    ]),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

export const finishCallableApplication = (
  node: SyntaxTree.Node,
  callee: ExpressionResult,
  argumentsResult: ArgumentsResult,
  callTypeArguments: CallTypeArgumentsResult,
  provenance: CallableApplyExpressionFact['provenance'] | undefined = undefined,
  resolution?: ResolutionContext,
  caller?: DeclarationFact,
): ExpressionResult => {
  const callable =
    callee.type !== undefined && Type.isCallable(callee.type)
      ? callee.type
      : callee.type !== undefined &&
          Type.isRepresented(callee.type) &&
          Type.isCallable(callee.type.contract)
        ? callee.type.contract
        : undefined
  const diagnostics: Array<Diagnostic.Diagnostic> = [
    ...callee.diagnostics,
    ...argumentsResult.diagnostics,
    ...callTypeArguments.diagnostics,
  ]
  const section = callableSectionOf(callee.fact)
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
  const schema = callable?.schema
  const inferred = new Map<string, Type.GenericArgument>(
    schema?.substitution ?? section?.substitution ?? [],
  )
  let evidence: ReadonlyArray<Constraint.ConstraintEvidence> = Object.freeze([])
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
  if (schema !== undefined && !concreteCallableIdentity(callee.fact)) {
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
    stagedSection === undefined
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
    callable?.unsafe === true &&
    stagedSection === undefined &&
    callable.parameters.length === argumentsResult.facts.length
  if (completeUnsafeInvocation) {
    const diagnostic = unsafeCallDiagnostic(true, Type.encode(callable), node, resolution)
    if (diagnostic !== undefined) {
      diagnostics.push(diagnostic)
      valid = false
    }
  }
  if (callable !== undefined) {
    const parameterOffset =
      stagedSection === undefined ? 0 : callable.parameters.length - argumentsResult.facts.length
    for (const [ordinal, argument] of argumentsResult.facts.entries()) {
      const expected = callable.parameters.at(parameterOffset + ordinal)
      if (expected === undefined || argument.type._tag !== 'Available') {
        valid = false
        continue
      }
      if (!TypeInference.infer(expected, argument.type.type, inferred)) {
        const rowFailure = TypeInference.rowInferenceFailure(expected, argument.type.type)
        diagnostics.push(
          rowFailure !== undefined
            ? Diagnostic.contractRowInference(rowFailure, argument.syntax.span)
            : Type.isCallable(expected) && Type.isCallable(argument.type.type)
              ? Diagnostic.incompatibleCallableSignature(
                  Type.encode(expected),
                  Type.encode(argument.type.type),
                  argument.syntax.span,
                )
              : Diagnostic.argumentTypeMismatch(
                  Type.encode(expected),
                  Type.encode(argument.type.type),
                  argument.syntax.span,
                ),
        )
        valid = false
        continue
      }
      const specialized = Type.substitute(expected, inferred)
      if (Type.isConcrete(specialized) && !typesCompatible(argument.type.type, specialized)) {
        diagnostics.push(
          Type.isCallable(specialized) && Type.isCallable(argument.type.type)
            ? Diagnostic.incompatibleCallableSignature(
                Type.encode(specialized),
                Type.encode(argument.type.type),
                argument.syntax.span,
              )
            : Diagnostic.argumentTypeMismatch(
                Type.encode(specialized),
                Type.encode(argument.type.type),
                argument.syntax.span,
              ),
        )
        valid = false
      }
    }
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
    diagnostics.push(...solved.diagnostics)
    if (solved.diagnostics.length > 0) valid = false
  }
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
        stagedCaptures.length,
      )
      return sectionType === undefined
        ? unavailableExpressionType
        : availableExpressionType(sectionType)
    }
    const result = Type.substitute(callable.result, inferred)
    return availableExpressionType(
      Type.isEffect(result)
        ? Type.effectWithRows(
            result.success,
            result.failureRow,
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
  if (section?.reference._tag === 'ResolvedIntrinsicContract') {
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
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'CallableApply',
      callee: callee.fact,
      arguments: argumentsResult.facts,
      mode: callable?.mode ?? 'Shared',
      ...(callable === undefined ? {} : { contract: callable }),
      substitution: inferred,
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
