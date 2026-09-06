import type * as ArtifactComposition from './ArtifactComposition.js'
import * as ConfigurationError from './ConfigurationError.js'
import * as ConfigurationOrigin from './ConfigurationOrigin.js'
import type * as ProfileBootstrap from './ProfileBootstrap.js'
import type * as CAbi from './CAbi.js'
import * as CleanupPlan from './CleanupPlan.js'
import * as ConformanceProof from './ConformanceProof.js'
import * as Constraint from './Constraint.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as ExecutableOrigin from './ExecutableOrigin.js'
import * as Hir from './Hir.js'
import type * as Intrinsic from './Intrinsic.js'
import * as TypeInference from './internal/TypeInference.js'
import type * as NameResolution from './NameResolution.js'
import * as Ownership from './Ownership.js'
import * as ProviderSelection from './ProviderSelection.js'
import * as Residualization from './Residualization.js'
import * as ResidualOwnership from './ResidualOwnership.js'
import * as RowAlgebra from './RowAlgebra.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Specialization from './Specialization.js'
import * as StaticEvaluation from './StaticEvaluation.js'
import * as StaticValue from './StaticValue.js'
import * as SuspensionMode from './SuspensionMode.js'
import type * as Target from './Target.js'
import * as Type from './Type.js'
import type * as TypeCompatibility from './TypeCompatibility.js'

/**
 * Instance discovery: which concrete runtime instances are reachable from the user entry. Keys
 * are canonical declaration identities plus normalized type and contract-row arguments — both
 * empty in the frozen slice. The worklist records an instance before following it, so ordinary
 * recursion terminates.
 */

/** One normalized concrete instance key. */
export interface InstanceKey {
  readonly _tag: 'InstanceKey'
  readonly declaration: DeclarationFacts.CanonicalId
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly contractRow: ReadonlyArray<string>
  /** Canonical selected-evidence encodings in declaration order. */
  readonly evidence: ReadonlyArray<string>
  readonly staticArguments: ReadonlyArray<StaticValue.Value>
}

/** One discovered instance with its elaborated HIR function. */
export interface Instance {
  readonly _tag: 'Instance'
  readonly key: InstanceKey
  readonly function: Hir.HirFunction
  readonly substitution: Type.Substitution
  readonly specialization: ConcreteSpecialization
  readonly ownership: Ownership.FunctionOwnership
  readonly resultCallable?: Type.CallableIdentityArgument
  readonly resultEffect?: string
  readonly effectSuccesses?: ReadonlyArray<{
    readonly site: Hir.EffectSiteId
    readonly identity: string
  }>
}

const concreteSpecializationBrand: unique symbol = Symbol('ConcreteSpecialization')

export type ConcreteEvidence = Exclude<Constraint.ConstraintEvidence, { readonly _tag: 'Assumed' }>

/**
 * The single post-generic frontier consumed by instance-dependent phases. Rows and evidence in
 * this bundle have already been substituted, validated, and reduced to finite concrete values.
 */
export interface ConcreteSpecialization {
  readonly _tag: 'ConcreteSpecialization'
  readonly [concreteSpecializationBrand]: true
  readonly compatibility?: TypeCompatibility.Context
  readonly parameters: ReadonlyArray<Type.Type>
  readonly result: Type.Type
  readonly failureRow?: Type.FailureRow
  readonly requirementRow?: Type.RequirementsRow
  readonly constraints: ReadonlyArray<Constraint.Constraint>
  readonly evidence: ReadonlyArray<ConcreteEvidence>
}

/** One concrete hidden callable-section construction reachable from an instance. */
export interface CallableInstance {
  readonly _tag: 'CallableInstance'
  readonly owner: InstanceKey
  readonly site: Hir.CallableSiteId
  readonly target: Hir.CallableTarget
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly substitution: Type.Substitution
  readonly captureTypes: ReadonlyArray<Type.Type>
  readonly captures: ReadonlyArray<{
    readonly ordinal: number
    readonly parameterOrdinal: number
    readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
    readonly type: Type.Type
    /** Hidden identity of a callable value captured as environment payload rather than data. */
    readonly callableIdentity?: Type.CallableIdentityArgument
  }>
  readonly type: Type.Callable
  readonly mode: Type.CallableMode
}

/** One specialized source Effect construction before any target layout is selected. */
export interface EffectInstance {
  readonly _tag: 'EffectInstance'
  readonly representationIdentity: string
  readonly identity: string
  readonly owner: InstanceKey
  readonly site: Hir.EffectSiteId
  readonly runner: DeclarationFacts.CanonicalId
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly captures: ReadonlyArray<{
    readonly ordinal: number
    readonly source: 'Parameter' | 'Binding' | 'Pattern'
    readonly sourceOrdinal: number
    readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
    readonly type: Type.Type
    readonly effectIdentity?: string
    readonly callableIdentity?: Type.CallableIdentityArgument
    /** Exact selected demand represented by this provider capture, when it binds a requirement. */
    readonly providedRequirement?: {
      readonly capability: Type.Nominal
      readonly role: string
      readonly requirementAccess: Type.Requirement['access']
      readonly providerAccess: 'Shared' | 'Exclusive' | 'Take'
    }
  }>
  readonly type: Type.Effect
  readonly suspension: SuspensionMode.Summary
}

/** One deterministic semantic-inspection entry for an executable node. */
export interface SuspensionFact {
  readonly _tag: 'SuspensionFact'
  readonly subject:
    | { readonly _tag: 'Instance'; readonly key: InstanceKey }
    | { readonly _tag: 'Execution'; readonly key: InstanceKey }
    | { readonly _tag: 'Effect'; readonly identity: string }
  readonly summary: SuspensionMode.Summary
}

/** A lexical provider needed to select one concrete call at a shared source span. */
export interface CallProvider {
  readonly capability: Type.Nominal
  readonly providerType: Type.Nominal
  readonly role: string
}

/** One monomorphic ordinary/effect constructor call with hidden Effect identities resolved. */
export interface CallInstance {
  readonly _tag: 'CallInstance'
  readonly owner: InstanceKey
  readonly span: Hir.Expression['span']
  readonly target: InstanceKey
  /** Caller-authored metadata aligned with target static arguments, outside instance identity. */
  readonly staticArgumentOrigins?: ReadonlyArray<StaticEvaluation.TextOrigin | undefined>
  readonly resultEffect?: string
  /** Lexical selections used to resolve the hidden argument identities at this call. */
  readonly providers?: ReadonlyArray<CallProvider>
}

/** Tests whether a call's provider-dependent identities belong to the current lexical context. */
export const callMatchesProviders = (
  call: CallInstance,
  providers: ReadonlyArray<CallProvider>,
): boolean =>
  (call.providers ?? []).every((expected) => {
    const actual = providers.findLast(
      (candidate) =>
        expected.role === candidate.role && Type.equals(expected.capability, candidate.capability),
    )
    return actual !== undefined && Type.equals(expected.providerType, actual.providerType)
  })

/** One exact sealed intrinsic call retained by executable instance closure. */
export interface IntrinsicCall {
  readonly _tag: 'ReachableIntrinsicCall'
  readonly operation: Intrinsic.OperationId
  readonly span: Hir.Expression['span']
}

/** One reachable foreign (`extern "C"`) declaration, classified for the selected target. */
export interface ForeignCall {
  readonly _tag: 'ReachableForeignCall'
  readonly symbol: string
  readonly signature: CAbi.CAbiSignature
  readonly declaration: DeclarationFacts.CanonicalId
  readonly declarationSpan: SourceSpan.SourceSpan
  /** The first reachable call in canonical order; availability diagnostics point here. */
  readonly callSpan: SourceSpan.SourceSpan
}

/** One `export "C"` function discovered as a native root, with the instance it selects. */
export interface ForeignExport {
  readonly _tag: 'ForeignExport'
  readonly symbol: string
  /** Exact source-level C function-pointer type, including pointer pointees. */
  readonly type: Type.ForeignFunction
  readonly signature: CAbi.CAbiSignature
  readonly key: InstanceKey
  readonly declaration: DeclarationFacts.CanonicalId
  readonly declarationSpan: SourceSpan.SourceSpan
}

/** One normalized owned failure retained by an effectful user entry. */
export interface EntryFailure {
  readonly type: Type.Type
  readonly identity: string
}

/** One target-selected primitive constant value with no runtime storage. */
export interface SelectedConstant {
  readonly _tag: 'SelectedConstant'
  readonly declaration: DeclarationFacts.CanonicalId
  readonly value: StaticValue.Value
}

/** The resolved or explicitly unavailable user entry. */
export type Entry =
  | {
      readonly _tag: 'Resolved'
      readonly kind: 'Ordinary'
      readonly result: 'Unit' | 'Status'
      readonly key: InstanceKey
    }
  | {
      readonly _tag: 'Resolved'
      readonly kind: 'Effect'
      readonly key: InstanceKey
      readonly failures: ReadonlyArray<EntryFailure>
      readonly requirements: ReadonlyArray<Type.Requirement>
    }
  | {
      readonly _tag: 'None'
    }
  | {
      readonly _tag: 'Unavailable'
      readonly reason:
        | 'MissingEntry'
        | 'AmbiguousEntry'
        | 'GenericEntry'
        | 'StaticEntry'
        | 'ParameterizedEntry'
        | 'PrivateEntry'
        | 'UntypedEntry'
        | 'InvalidOrdinaryEntryResult'
        | 'InvalidEffectEntryResult'
        | 'EffectEntryRequirements'
        | 'UnavailableEntryBody'
        | 'InvalidSource'
      readonly requirements?: ReadonlyArray<Type.Requirement>
    }

/** The deterministic discovery result. */
export interface Counters {
  readonly _tag: 'InstanceDiscoveryCounters'
  readonly residualBodies: Residualization.Counters
  readonly residualOwnership: ResidualOwnership.Counters
}

/** The deterministic discovery result and work actually performed to obtain it. */
export interface Discovery {
  readonly retention: ReadonlyArray<InstanceKey>
  readonly _tag: 'InstanceDiscovery'
  readonly rootModule: string
  readonly entry: Entry
  readonly instances: ReadonlyArray<Instance>
  /** Demanded residual specializations rejected before executable reachability. */
  readonly unavailableOwnership: ReadonlyArray<UnavailableResidualOwnership>
  readonly callables: ReadonlyArray<CallableInstance>
  readonly effects: ReadonlyArray<EffectInstance>
  readonly calls: ReadonlyArray<CallInstance>
  readonly intrinsics: ReadonlyArray<IntrinsicCall>
  /** Reachable foreign declarations in canonical order; each execution surface admits them. */
  readonly foreignCalls: ReadonlyArray<ForeignCall>
  /** Every closure export in canonical module then declaration order; roots only on native. */
  readonly foreignExports: ReadonlyArray<ForeignExport>
  readonly constants: ReadonlyArray<SelectedConstant>
  /** Exact direct/nested/external-park summaries in canonical subject order. */
  readonly suspension: ReadonlyArray<SuspensionFact>
  /** Target-relative diagnostics produced while selecting and residualizing static work. */
  readonly residualizationDiagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly specializationFailures: ReadonlyArray<NonConcreteSpecialization>
  readonly violations: ReadonlyArray<PolymorphicRecursion>
  readonly counters: Counters
  readonly residualBodies: ReadonlyArray<Residualization.Observation>
  readonly residualOwnership: ReadonlyArray<ResidualOwnership.Observation>
}

/** One specialization-keyed unavailable ownership result retained for semantic inspection. */
export interface UnavailableResidualOwnership {
  readonly _tag: 'UnavailableResidualOwnership'
  readonly key: InstanceKey
  readonly ownership: Ownership.FunctionOwnership
}

/** A recursive generic edge that changes an ancestor declaration's concrete arguments. */
export interface PolymorphicRecursion {
  readonly _tag: 'PolymorphicRecursion'
  readonly caller: InstanceKey
  readonly target: InstanceKey
}

export interface NonConcreteSpecialization {
  readonly _tag: 'NonConcreteSpecialization'
  readonly key: InstanceKey
  readonly span: Hir.HirFunction['declaration']['syntax']['span']
}

export const requirementBindings = (
  fn: Hir.HirFunction,
): ReadonlyArray<Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>> =>
  fn.statements.flatMap((statement) =>
    Hir.statementExpressions(statement).flatMap((expression) =>
      Hir.expressionTree(expression).flatMap((candidate) =>
        candidate._tag === 'EffectBindRequirement' ? [candidate] : [],
      ),
    ),
  )

const selectedRequirement = (
  binding: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>,
  substitution: Type.Substitution,
): Type.Requirement | undefined => {
  return Hir.selectedRequirement(binding.provider, substitution)
}

const requirementBindingWitness = (
  binding: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>,
  substitution: Type.Substitution,
  index: DeclarationIndex.Index,
): DeclarationFacts.ConformanceWitness | undefined => {
  const capability = selectedRequirement(binding, substitution)?.capability
  const provider = Type.substitute(binding.provider.providerType, substitution)
  return capability !== undefined && Type.isNominal(capability) && Type.isNominal(provider)
    ? (binding.provider.witness ?? ConformanceProof.witness(index, provider, capability))
    : undefined
}

const forwardedRequirementBinding = (
  fn: Hir.HirFunction,
): Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }> | undefined => {
  const returned = fn.statements.at(-1)
  if (fn.statements.length !== 1 || returned?._tag !== 'Return') return undefined
  const block = returned.expression
  if (block._tag !== 'EffectBlock' || block.statements.length !== 2) return undefined
  const binding = block.statements.at(0)
  const completed = block.statements.at(1)
  if (
    binding?._tag !== 'Bind' ||
    binding.initializer._tag !== 'EffectBindRequirement' ||
    binding.initializer.protected._tag !== 'Move' ||
    binding.initializer.protected.subject._tag !== 'ParameterReference' ||
    binding.initializer.protected.subject.parameter.ordinal !== 0 ||
    binding.initializer.provider.parameter?.ordinal !== 1 ||
    completed?._tag !== 'Return' ||
    completed.expression._tag !== 'Run' ||
    completed.expression.subject._tag !== 'BindingReference' ||
    completed.expression.subject.binding.ordinal !== binding.binding.ordinal
  )
    return undefined
  return binding.initializer
}

/** Retains an explicit unavailable entry when frontend errors prevent discovery. */
export const invalid = (rootModule: string): Discovery =>
  Object.freeze({
    _tag: 'InstanceDiscovery',
    retention: Object.freeze([]),
    rootModule,
    entry: Object.freeze({ _tag: 'Unavailable', reason: 'InvalidSource' }),
    instances: Object.freeze([]),
    unavailableOwnership: Object.freeze([]),
    callables: Object.freeze([]),
    effects: Object.freeze([]),
    calls: Object.freeze([]),
    intrinsics: Object.freeze([]),
    foreignCalls: Object.freeze([]),
    foreignExports: Object.freeze([]),
    constants: Object.freeze([]),
    suspension: Object.freeze([]),
    residualizationDiagnostics: Object.freeze([]),
    specializationFailures: Object.freeze([]),
    violations: Object.freeze([]),
    counters: Object.freeze({
      _tag: 'InstanceDiscoveryCounters',
      residualBodies: Residualization.noWork,
      residualOwnership: ResidualOwnership.counters(ResidualOwnership.make()),
    }),
    residualBodies: Object.freeze([]),
    residualOwnership: Object.freeze([]),
  })

/**
 * Closes a partial section's binder-owned channels before the type becomes instance identity.
 *
 * A constrained partial section is deliberately open in its target contract's own unapplied
 * binders: its Effect channels close only at application, so its surface mentions binder-owned
 * success/failure types and requirement rows no carrying call can resolve.
 * Elaboration's constrained
 * callable escape gate proves such a value only ever reaches a whole-value relay, an application,
 * or a drop — every other escape is rejected there with its own diagnostic — and the callable
 * itself is erased onto its hidden identity argument. The instance identity therefore closes the
 * schema's unapplied value binders to `never` and requirement binders to empty rows, exactly
 * the shape the erased relay needs, so a proven relay is not re-rejected as unresolved.
 */
const carriedSectionArgument = (argument: Type.GenericArgument): Type.GenericArgument => {
  if (!Type.isTypeArgument(argument)) return argument
  if (!Type.isCallable(argument) || argument.schema === undefined) return argument
  if (Type.isRuntimeConcrete(argument)) return argument
  const closure = new Map<string, Type.GenericArgument>()
  for (const binder of argument.schema.binders) {
    const selected = argument.schema.substitution.get(Type.key(binder))
    if (
      selected !== undefined &&
      !Type.equalsGenericArgument(selected, Type.parameterArgument(binder))
    )
      continue
    if (binder.kind === 'RequirementRow')
      closure.set(Type.key(binder), Type.requirementRowArgument([]))
    else if (binder.kind === 'Value') closure.set(Type.key(binder), 'never')
  }
  if (closure.size === 0) return argument
  const closed = Type.substitute(argument, closure)
  return Type.isRuntimeConcrete(closed) ? closed : argument
}

const keyOf = (
  declaration: DeclarationFacts.CanonicalId,
  contract: Hir.ContractFact,
  typeParameters: ReadonlyArray<Type.Parameter> = [],
  rawTypeArguments: ReadonlyArray<Type.GenericArgument> = [],
  staticArguments: ReadonlyArray<StaticValue.Value> = [],
  evidence: ReadonlyArray<string> = [],
): InstanceKey =>
  (() => {
    const typeArguments = rawTypeArguments.map(carriedSectionArgument)
    const selected = TypeInference.selectedSubstitution(
      typeParameters,
      typeArguments.filter((argument) => !Type.isHiddenExecutableArgument(argument)),
    )
    if (selected === undefined) {
      throw new RangeError('Instance key type arguments do not match declaration parameters')
    }
    const { substitution, compatibility } = selected
    return Object.freeze({
      _tag: 'InstanceKey',
      declaration,
      typeArguments: Object.freeze(Array.from(typeArguments)),
      evidence: Object.freeze([...evidence]),
      staticArguments: Object.freeze([...staticArguments]),
      contractRow:
        contract._tag === 'Contract'
          ? Object.freeze([
              ...contract.parameters.map((type) =>
                Type.runtimeKey(Type.substitute(type, substitution, compatibility)),
              ),
              `result:${Type.runtimeKey(Type.substitute(contract.result, substitution))}`,
              ...(contract.failureRow === undefined
                ? []
                : [
                    `failures:${Type.runtimeFailureRowKey(Type.substituteFailureRow(contract.failureRow, substitution))}`,
                  ]),
              ...(contract.requirementRow === undefined
                ? []
                : [
                    `requirements:${Type.runtimeRequirementsRowKey(Type.substituteRequirementsRow(contract.requirementRow, substitution))}`,
                  ]),
              ...contract.constraints.map(
                (constraint) =>
                  `constraint:${Type.runtimeConstraintKey(Constraint.substitute(constraint, substitution))}`,
              ),
            ])
          : Object.freeze([]),
    })
  })()

const keyTextCache = new WeakMap<InstanceKey, string>()

export const keyText = (key: InstanceKey): string => {
  let cached = keyTextCache.get(key)
  if (cached === undefined) {
    cached = `${key.declaration.module}\u0000${key.declaration.name}\u0000${Type.runtimeArgumentKeys(
      key.typeArguments,
    ).join('\u0000')}${key.evidence.length === 0 ? '' : `\u0004${key.evidence.join('\u0000')}`}${
      key.staticArguments.length === 0
        ? ''
        : `\u0001${key.staticArguments.map(StaticValue.key).join('\u0000')}`
    }\u0002${key.contractRow.join('\u0000')}`
    keyTextCache.set(key, cached)
  }
  return cached
}

const concreteConstraintEvidence = (
  wanted: Constraint.Constraint,
  origin: SourceSpan.SourceSpan,
  index: DeclarationIndex.Index,
): ReadonlyArray<ConcreteEvidence> | undefined => {
  if (wanted._tag !== 'ProviderSelectionConstraint') {
    const proof = Constraint.proveStructural(wanted)
    return proof === undefined ? undefined : Object.freeze([proof])
  }
  const selected = RowAlgebra.concretize(Type.requirementRowPolicy(), wanted.selected)
  const source = RowAlgebra.concretize(Type.requirementRowPolicy(), wanted.source)
  if (
    !Type.isRuntimeConcrete(wanted.provider) ||
    selected._tag !== 'Concrete' ||
    source._tag !== 'Concrete' ||
    selected.row.members.some((requirement) => !Type.isRuntimeConcrete(requirement.capability)) ||
    source.row.members.some((requirement) => !Type.isRuntimeConcrete(requirement.capability))
  )
    return undefined
  const solved = ProviderSelection.solve({
    relations: Object.freeze([
      Object.freeze<ProviderSelection.Relation>({ wanted, origins: [origin] }),
    ]),
    selected: wanted.selected,
    responsible: origin,
    oracle: Object.freeze({
      match: (provider: Type.Type, capability: Type.Nominal) =>
        ConformanceProof.providerMatch(index, provider, capability),
    }),
  })
  return solved._tag === 'Selected' ? solved.evidence : undefined
}

const specializeEvidence = (
  evidence: Constraint.ConstraintEvidence,
  substitution: Type.Substitution,
  origin: SourceSpan.SourceSpan,
  index: DeclarationIndex.Index,
): ReadonlyArray<ConcreteEvidence> | undefined => {
  if (evidence._tag === 'Assumed') {
    const assumed = Constraint.substitute(evidence.wanted, evidence.substitution)
    return concreteConstraintEvidence(Constraint.substitute(assumed, substitution), origin, index)
  }
  if (evidence._tag === 'Member') {
    const selected = Type.substitute(evidence.selected, substitution)
    const source = Type.substituteFailureRow(evidence.source, substitution)
    return concreteConstraintEvidence(Constraint.nominalMember(selected, source), origin, index)
  }
  if (evidence._tag === 'FailureSubset') {
    const selected = Type.substituteFailureRow(evidence.selected, substitution)
    const source = Type.substituteFailureRow(evidence.source, substitution)
    const selectedConcrete = RowAlgebra.concretize(Type.failureRowPolicy(), selected)
    const sourceConcrete = RowAlgebra.concretize(Type.failureRowPolicy(), source)
    if (
      selectedConcrete._tag !== 'Concrete' ||
      sourceConcrete._tag !== 'Concrete' ||
      selectedConcrete.row.members.some((member) => !Type.isRuntimeConcrete(member)) ||
      sourceConcrete.row.members.some((member) => !Type.isRuntimeConcrete(member)) ||
      !RowAlgebra.isKnownSubset(Type.failureRowPolicy(), selected, source)
    )
      return undefined
    return Object.freeze([
      Object.freeze<ConcreteEvidence>({ _tag: 'FailureSubset', selected, source }),
    ])
  }
  if (evidence._tag === 'RequirementSubset')
    return concreteConstraintEvidence(
      Constraint.requirementSubset(
        Type.substituteRequirementsRow(evidence.selected, substitution),
        Type.substituteRequirementsRow(evidence.source, substitution),
      ),
      origin,
      index,
    )
  return concreteConstraintEvidence(
    Constraint.substitute(evidence.wanted, substitution),
    origin,
    index,
  )
}

const hirEvidence = (
  fn: Hir.HirFunction,
): ReadonlyArray<{
  readonly evidence: Constraint.ConstraintEvidence
  readonly origin: SourceSpan.SourceSpan
}> =>
  Object.freeze(
    fn.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
      .flatMap((expression) => {
        let evidence: ReadonlyArray<Constraint.ConstraintEvidence> = Object.freeze([])
        if (expression._tag === 'EffectBindRequirement') {
          evidence = expression.provider.evidence
        } else if (expression._tag === 'EffectCatch') {
          evidence = expression.evidence
        }
        return evidence.map((proof) => Object.freeze({ evidence: proof, origin: expression.span }))
      }),
  )

export const specialize = (
  fn: Hir.HirFunction,
  substitution: Type.Substitution,
  index: DeclarationIndex.Index,
  compatibility?: TypeCompatibility.Context,
): ConcreteSpecialization | undefined => {
  if (fn.contract._tag !== 'Contract') return undefined
  const parameters = fn.contract.parameters.map((parameter) =>
    Type.substitute(parameter, substitution, compatibility),
  )
  const result = Type.substitute(fn.contract.result, substitution, compatibility)
  if (
    parameters.some((parameter) => !Type.isRuntimeConcrete(parameter)) ||
    !Type.isRuntimeConcrete(result)
  )
    return undefined

  const failureRow =
    fn.contract.failureRow === undefined
      ? undefined
      : Type.substituteFailureRow(fn.contract.failureRow, substitution, compatibility)
  const requirementRow =
    fn.contract.requirementRow === undefined
      ? undefined
      : Type.substituteRequirementsRow(fn.contract.requirementRow, substitution, compatibility)
  if (
    (failureRow !== undefined &&
      (RowAlgebra.concretize(Type.failureRowPolicy(), failureRow)._tag !== 'Concrete' ||
        RowAlgebra.concreteMembers(Type.failureRowPolicy(), failureRow).some(
          (member) => !Type.isRuntimeConcrete(member),
        ))) ||
    (requirementRow !== undefined &&
      (RowAlgebra.concretize(Type.requirementRowPolicy(), requirementRow)._tag !== 'Concrete' ||
        RowAlgebra.concreteMembers(Type.requirementRowPolicy(), requirementRow).some(
          (requirement) => !Type.isRuntimeConcrete(requirement.capability),
        )))
  )
    return undefined

  const origin = fn.declaration.syntax.span
  const constraints = fn.contract.constraints.map((constraint) =>
    Constraint.substitute(constraint, substitution),
  )
  const concreteEvidence: Array<ConcreteEvidence> = []
  for (const constraint of constraints) {
    const solved = concreteConstraintEvidence(constraint, origin, index)
    if (solved === undefined) return undefined
    concreteEvidence.push(...solved)
  }
  for (const occurrence of hirEvidence(fn)) {
    const solved = specializeEvidence(occurrence.evidence, substitution, occurrence.origin, index)
    if (solved === undefined) return undefined
    concreteEvidence.push(...solved)
  }
  const evidence = Object.freeze(
    [
      ...new Map(concreteEvidence.map((proof) => [Constraint.evidenceKey(proof), proof])).values(),
    ].sort((left, right) => {
      const leftKey = Constraint.evidenceKey(left)
      const rightKey = Constraint.evidenceKey(right)
      if (leftKey < rightKey) return -1
      if (leftKey > rightKey) return 1
      return 0
    }),
  )
  return Object.freeze({
    _tag: 'ConcreteSpecialization',
    ...(compatibility === undefined ? {} : { compatibility }),
    [concreteSpecializationBrand]: true as const,
    parameters: Object.freeze(parameters),
    result,
    ...(failureRow === undefined ? {} : { failureRow }),
    ...(requirementRow === undefined ? {} : { requirementRow }),
    constraints: Object.freeze(constraints),
    evidence,
  })
}

/** Returns the exact branded provider proof attached to one specialized HIR binding. */
export const requirementSelection = (
  instance: Instance,
  provider: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>['provider'],
): Extract<ConcreteEvidence, { readonly _tag: 'RequirementSelection' }> | undefined => {
  const wantedKeys = new Set(
    provider.evidence.flatMap((proof) => {
      let wanted: Constraint.Constraint | undefined
      if (proof._tag === 'Assumed') {
        wanted = Constraint.substitute(
          Constraint.substitute(proof.wanted, proof.substitution),
          instance.substitution,
        )
      } else if (proof._tag === 'RequirementSelection') {
        wanted = Constraint.substitute(proof.wanted, instance.substitution)
      }
      return wanted?._tag === 'ProviderSelectionConstraint' ? [Constraint.key(wanted)] : []
    }),
  )
  return instance.specialization.evidence.find(
    (proof): proof is Extract<ConcreteEvidence, { readonly _tag: 'RequirementSelection' }> =>
      proof._tag === 'RequirementSelection' && wantedKeys.has(proof.wantedKey),
  )
}

/** Returns every discovered instance with the exact declaration and kinded arguments. */
export const matchingSpecialization = (
  self: Discovery,
  specialization: Specialization.Specialization,
): ReadonlyArray<Instance> => {
  const identity = Specialization.runtimeKey(specialization)
  return self.instances.filter((candidate) => Specialization.runtimeKey(candidate.key) === identity)
}

export const effectIdentity = (owner: InstanceKey, site: Hir.EffectSiteId): string =>
  `${keyText(owner)}\u0004${Hir.executableSiteKey(site)}`

const resolveEntry = (root: Elaboration.Result, name: string): Entry => {
  const lookup = Elaboration.declarationByName(root, name)
  if (lookup._tag === 'Missing')
    return Object.freeze({ _tag: 'Unavailable', reason: 'MissingEntry' })
  if (lookup._tag === 'Ambiguous') {
    return Object.freeze({ _tag: 'Unavailable', reason: 'AmbiguousEntry' })
  }
  const declaration = lookup.declaration
  if (declaration.phase === 'Static') {
    return Object.freeze({ _tag: 'Unavailable', reason: 'StaticEntry' })
  }
  if (declaration.typeParameters.length > 0) {
    return Object.freeze({ _tag: 'Unavailable', reason: 'GenericEntry' })
  }
  if (declaration.parameterCount > 0) {
    return Object.freeze({ _tag: 'Unavailable', reason: 'ParameterizedEntry' })
  }
  if (declaration.visibility !== 'Public') {
    return Object.freeze({ _tag: 'Unavailable', reason: 'PrivateEntry' })
  }
  if (
    declaration.returnType._tag !== 'Resolved' ||
    declaration.canonical._tag !== 'Canonical' ||
    !declaration.failureRow.available ||
    !declaration.requirementRow.available
  ) {
    return Object.freeze({ _tag: 'Unavailable', reason: 'UntypedEntry' })
  }
  if (declaration.functionKind === 'Ordinary') {
    if (
      declaration.failureRow.failures.length !== 0 ||
      declaration.requirementRow.requirements.length !== 0 ||
      (!Type.equals(declaration.returnType.type, Type.unit) &&
        declaration.returnType.type !== 'i32')
    ) {
      return Object.freeze({ _tag: 'Unavailable', reason: 'InvalidOrdinaryEntryResult' })
    }
    return Object.freeze({
      _tag: 'Resolved',
      kind: 'Ordinary',
      result: Type.equals(declaration.returnType.type, Type.unit) ? 'Unit' : 'Status',
      key: keyOf(declaration.canonical.id, Hir.contractOf(declaration)),
    })
  }
  if (!Type.equals(declaration.returnType.type, Type.unit)) {
    return Object.freeze({ _tag: 'Unavailable', reason: 'InvalidEffectEntryResult' })
  }
  if (declaration.requirementRow.requirements.length > 0) {
    return Object.freeze({
      _tag: 'Unavailable',
      reason: 'EffectEntryRequirements',
      requirements: Object.freeze(declaration.requirementRow.requirements),
    })
  }
  return Object.freeze({
    _tag: 'Resolved',
    kind: 'Effect',
    key: keyOf(declaration.canonical.id, Hir.contractOf(declaration)),
    requirements: Object.freeze(declaration.requirementRow.requirements),
    failures: Object.freeze(
      declaration.failureRow.failures.map((failure) =>
        Object.freeze({ type: failure, identity: Type.encode(failure) }),
      ),
    ),
  })
}

const instanceSubstitution = (
  fn: Hir.HirFunction,
  key: InstanceKey,
): Type.Substitution | undefined =>
  TypeInference.selectedSubstitution(
    fn.declaration.typeParameters.map((parameter) => parameter.type),
    key.typeArguments.filter((argument) => !Type.isHiddenExecutableArgument(argument)),
  )?.substitution

const effectParameterOrdinals = (
  fn: Hir.HirFunction,
  substitution: Type.Substitution,
): ReadonlyArray<number> =>
  fn.contract._tag === 'Contract'
    ? fn.contract.parameters.flatMap((parameter, ordinal) =>
        (() => {
          const specialized = Type.substitute(parameter, substitution)
          const contract = Type.isRepresented(specialized) ? specialized.contract : specialized
          return Type.isEffect(contract) ? [ordinal] : []
        })(),
      )
    : Object.freeze([])

export const parameterEffectRepresentationArgument = (
  fn: Hir.HirFunction,
  key: InstanceKey,
  ordinal: number,
): Type.EffectIdentityArgument | Type.CompositeEffectRepresentationArgument | undefined => {
  const substitution = instanceSubstitution(fn, key)
  if (substitution === undefined) return undefined
  const position = effectParameterOrdinals(fn, substitution).indexOf(ordinal)
  if (position < 0) return undefined
  return key.typeArguments
    .filter(
      (
        argument,
      ): argument is Type.EffectIdentityArgument | Type.CompositeEffectRepresentationArgument =>
        Type.isEffectIdentityArgument(argument) ||
        Type.isCompositeEffectRepresentationArgument(argument),
    )
    .at(position)
}

export const parameterEffectIdentityArgument = (
  fn: Hir.HirFunction,
  key: InstanceKey,
  ordinal: number,
): Type.EffectIdentityArgument | undefined => {
  const argument = parameterEffectRepresentationArgument(fn, key, ordinal)
  return argument !== undefined && Type.isEffectIdentityArgument(argument) ? argument : undefined
}

export const parameterEffectIdentity = (
  fn: Hir.HirFunction,
  key: InstanceKey,
  ordinal: number,
): string | undefined => parameterEffectIdentityArgument(fn, key, ordinal)?.identity

/** Replaces an owner-scoped represented Effect parameter with its concrete hidden identity. */
export const concreteEffectRepresentationArgument = (
  fn: Hir.HirFunction,
  key: InstanceKey,
  argument: Type.GenericArgument,
): Type.GenericArgument => {
  if (
    !Type.isExactRepresentationArgument(argument) ||
    !Type.isEffect(argument.contract) ||
    !Type.isEffectIdentityArgument(argument.identity) ||
    fn.contract._tag !== 'Contract'
  )
    return argument
  const identities = fn.contract.parameters.flatMap((parameter, ordinal) => {
    const substitution = instanceSubstitution(fn, key)
    if (substitution === undefined) return []
    const specialized = Type.substitute(parameter, substitution)
    if (
      !Type.isRepresented(specialized) ||
      !Type.isEffect(specialized.contract) ||
      !Type.isExactRepresentationArgument(specialized.representation.argument) ||
      !Type.equalsGenericArgument(specialized.representation.argument, argument)
    )
      return []
    const identity = parameterEffectIdentityArgument(fn, key, ordinal)
    return identity === undefined ? [] : [identity]
  })
  const identity = identities.length === 1 ? identities.at(0) : undefined
  return identity === undefined
    ? argument
    : Type.exactRepresentationArgument(identity, argument.contract)
}

const callableParameterOrdinals = (
  fn: Hir.HirFunction,
  substitution: Type.Substitution,
): ReadonlyArray<number> =>
  fn.contract._tag === 'Contract'
    ? fn.contract.parameters.flatMap((parameter, ordinal) =>
        Type.isCallable(Type.substitute(parameter, substitution)) ? [ordinal] : [],
      )
    : Object.freeze([])

export const parameterCallableIdentity = (
  fn: Hir.HirFunction,
  key: InstanceKey,
  ordinal: number,
): Type.CallableIdentityArgument | undefined => {
  const substitution = instanceSubstitution(fn, key)
  if (substitution === undefined) return undefined
  const position = callableParameterOrdinals(fn, substitution).indexOf(ordinal)
  if (position < 0) return undefined
  return key.typeArguments.filter(Type.isCallableIdentityArgument).at(position)
}

export const callableIdentity = (self: CallableInstance): string =>
  `${keyText(self.owner)}\u0001${Hir.executableSiteKey(self.site)}\u0001${Type.runtimeArgumentKeys(self.typeArguments).join('\u0000')}`

/** Returns the canonical specialized identity of one discovered callable environment. */
export const callableEnvironmentIdentity = (
  self: CallableInstance,
): Type.CallableEnvironmentIdentity =>
  Hir.callableEnvironmentIdentity(self.site, {
    declaration: Object.freeze({
      module: self.owner.declaration.module,
      name: self.owner.declaration.name,
    }),
    typeArguments: self.owner.typeArguments,
  })

const {
  functionByKey,
  instanceNode,
  effectNode,
  hookCalls,
  bodyCallTargets,
  interfaceWitnessTargets,
  requirementBindingCallTargets,
  forwardedRequirementCallTargets,
  slotDropHookTargets,
  directCallInstances,
  callableCallTargets,
  forwardedRequirementTargets,
  resultCallableIdentity,
  resultEffectIdentity,
  effectSuccesses,
  concreteCallables,
  concreteEffects,
  suspensionGraph,
} = ExecutableOrigin.make({
  specializeInstanceType: (type, owner, substitutions) =>
    Specialization.specializeType(owner, type, substitutions),
  keyOf,
  keyText,
  requirementBindings,
  selectedRequirement,
  requirementBindingWitness,
  forwardedRequirementBinding,
  instanceSubstitution,
  effectParameterOrdinals,
  callableParameterOrdinals,
  parameterEffectIdentity,
  parameterEffectRepresentationArgument,
  parameterCallableIdentity,
  effectIdentity,
  callableIdentity,
  callableEnvironmentIdentity,
})

type CallTarget = ExecutableOrigin.CallTarget

const compareInstanceKeys = (left: InstanceKey, right: InstanceKey): number => {
  const leftText = keyText(left)
  const rightText = keyText(right)
  if (leftText < rightText) return -1
  if (leftText > rightText) return 1
  return 0
}

const suspensionFact = (
  self: Discovery,
  predicate: (subject: SuspensionFact['subject']) => boolean,
): SuspensionMode.Summary =>
  self.suspension.find((fact) => predicate(fact.subject))?.summary ?? SuspensionMode.direct

/** Returns the complete summary of a function plus any lazy Effect it returns. */
export const suspensionOf = (self: Discovery, key: InstanceKey): SuspensionMode.Summary =>
  suspensionFact(
    self,
    (subject) => subject._tag === 'Instance' && keyText(subject.key) === keyText(key),
  )

/** Returns the summary of executing one function body, excluding its lazy result. */
export const executionSuspensionOf = (self: Discovery, key: InstanceKey): SuspensionMode.Summary =>
  suspensionFact(
    self,
    (subject) => subject._tag === 'Execution' && keyText(subject.key) === keyText(key),
  )

/** Returns the summary of one exact hidden Effect runner. */
export const effectSuspensionOf = (self: Discovery, identity: string): SuspensionMode.Summary =>
  suspensionFact(self, (subject) => subject._tag === 'Effect' && subject.identity === identity)

const sameVisibleTypeArguments = (
  left: ReadonlyArray<Type.GenericArgument>,
  right: ReadonlyArray<Type.GenericArgument>,
): boolean => {
  const leftVisible = left.filter((argument) => !Type.isHiddenExecutableArgument(argument))
  const rightVisible = right.filter((argument) => !Type.isHiddenExecutableArgument(argument))
  return (
    leftVisible.length === rightVisible.length &&
    leftVisible.every((argument, ordinal) => {
      const expected = rightVisible.at(ordinal)
      return expected !== undefined && Type.equalsGenericArgument(argument, expected)
    })
  )
}

const sameExactOwner = (left: InstanceKey, right: Type.ExecutableSpecializationOwner): boolean =>
  left.declaration.module === right.declaration.module &&
  left.declaration.name === right.declaration.name &&
  left.typeArguments.length === right.typeArguments.length &&
  left.typeArguments.every((argument, ordinal) => {
    const expected = right.typeArguments.at(ordinal)
    return expected !== undefined && Type.equalsGenericArgument(argument, expected)
  })

/** Resolves an owner-scoped source representation identity to its concrete hidden Effect. */
export const representedEffectOf = (
  self: Discovery,
  identity: Type.EffectIdentityArgument,
): EffectInstance | undefined => {
  const concrete = self.effects.filter((effect) => effect.identity === identity.identity)
  if (concrete.length === 1) return concrete.at(0)
  const represented = self.effects.filter(
    (effect) => effect.representationIdentity === identity.identity,
  )
  const owner = identity.owner
  if (owner === undefined) return represented.length === 1 ? represented.at(0) : undefined
  const exact = represented.filter((effect) => sameExactOwner(effect.owner, owner))
  if (exact.length === 1) return exact.at(0)
  const visible = represented.filter(
    (effect) =>
      effect.owner.declaration.module === owner.declaration.module &&
      effect.owner.declaration.name === owner.declaration.name &&
      sameVisibleTypeArguments(effect.owner.typeArguments, owner.typeArguments),
  )
  return visible.length === 1 ? visible.at(0) : undefined
}

/** Resolves the suspension summary of one owner-scoped represented Effect. */
export const representedEffectSuspensionOf = (
  self: Discovery,
  identity: Type.EffectIdentityArgument,
): SuspensionMode.Summary => {
  const selected = representedEffectOf(self, identity)
  return selected?.suspension ?? effectSuspensionOf(self, identity.identity)
}

/**
 * Every admitted `export "C"` header in the loaded closure as a monomorphic root record, in
 * canonical module then declaration order. A rejected header (unresolved result) is skipped.
 */
const exportRoots = (
  index: DeclarationIndex.Index,
  target: Target.Target,
): ReadonlyArray<ForeignExport> =>
  Object.freeze(
    [...index.modules]
      .sort((left, right) => {
        if (left.module < right.module) return -1
        if (left.module > right.module) return 1
        return 0
      })
      .flatMap((module) =>
        module.declarations.flatMap((fact): ReadonlyArray<ForeignExport> => {
          if (
            fact.foreignExport === undefined ||
            fact.canonical._tag !== 'Canonical' ||
            fact.name._tag !== 'Present' ||
            fact.returnType._tag !== 'Resolved'
          )
            return []
          const parameters = fact.parameters.flatMap((parameter) =>
            parameter.declaredType._tag === 'Resolved' ? [parameter.declaredType.type] : [],
          )
          if (parameters.length !== fact.parameters.length) return []
          return [
            Object.freeze({
              _tag: 'ForeignExport',
              symbol: fact.foreignExport.symbol,
              type: Type.foreignFunction(
                parameters,
                fact.returnType.type,
                fact.foreignExport.contract,
                DeclarationFacts.executableLifetimes(fact),
              ),
              signature: ExecutableOrigin.foreignSignature(fact, target),
              key: keyOf(
                fact.canonical.id,
                Hir.contractOf(fact),
                fact.typeParameters.map((parameter) => parameter.type),
                fact.typeParameters.map((parameter) => Type.parameterArgument(parameter.type)),
              ),
              declaration: fact.canonical.id,
              declarationSpan: fact.name.token.span,
            }),
          ]
        }),
      ),
  )

/**
 * Discovers the reachable instances from the root module's entry. The worklist records an
 * instance before following its calls, so directly and mutually recursive programs terminate.
 */
export const discover = (
  rootModule: string,
  results: ReadonlyMap<string, Elaboration.Result>,
  index: DeclarationIndex.Index,
  completion: ProfileBootstrap.Completion,
  resolution: NameResolution.Resolution,
  composition: ArtifactComposition.Resolved,
): Discovery => {
  const target = completion.profile.target
  const root = results.get(rootModule)
  if (root === undefined) {
    throw new RangeError(`Instance discovery lost its root module ${rootModule}`)
  }
  const foreignExports = exportRoots(index, target)
  const invoked =
    composition.invocation === undefined ? undefined : results.get(composition.invocation.module)
  let entry: Entry = Object.freeze({ _tag: 'None' })
  if (composition.invocation !== undefined)
    entry =
      invoked === undefined
        ? Object.freeze({ _tag: 'Unavailable', reason: 'MissingEntry' })
        : resolveEntry(invoked, composition.invocation.declaration)
  const retention: Array<InstanceKey> = []
  const rootDiagnostics: Array<Diagnostic.Diagnostic> = []
  for (const selector of composition.retention) {
    const module = results.get(selector.module)
    const lookup =
      module === undefined ? undefined : Elaboration.declarationByName(module, selector.declaration)
    const declaration = lookup?._tag === 'Resolved' ? lookup.declaration : undefined
    if (
      declaration === undefined ||
      declaration.canonical._tag !== 'Canonical' ||
      declaration.phase === 'Static' ||
      declaration.typeParameters.length > 0 ||
      declaration.foreign !== undefined ||
      declaration.returnType._tag !== 'Resolved' ||
      !declaration.failureRow.available ||
      !declaration.requirementRow.available
    ) {
      let related: ReadonlyArray<DeclarationFacts.DeclarationFact> =
        declaration === undefined ? [] : [declaration]
      if (lookup?._tag === 'Ambiguous') related = lookup.declarations
      const origins = [
        selector.origin,
        ...related.map((candidate) =>
          ConfigurationOrigin.snapshot({
            source: selector.module,
            provenance: 'literal',
            span: candidate.syntax.span,
          }),
        ),
      ]
      rootDiagnostics.push(
        Diagnostic.invalidConfiguration(
          ConfigurationError.make(
            'ArtifactComposition.retention',
            'InvalidInput',
            'retention root must name one monomorphic runtime definition',
            origins,
          ),
          selector.origin.span ?? related[0]?.syntax.span ?? root.syntax.root.span,
        ),
      )
    } else retention.push(keyOf(declaration.canonical.id, Hir.contractOf(declaration)))
  }
  if (rootDiagnostics.length > 0)
    return Object.freeze({
      ...invalid(rootModule),
      foreignExports,
      residualizationDiagnostics: Object.freeze(rootDiagnostics),
    })
  if (entry._tag === 'Unavailable') {
    return Object.freeze({ ...invalid(rootModule), entry, foreignExports })
  }

  const residualization = Residualization.make(
    completion.profile,
    results,
    resolution,
    index,
    undefined,
    completion.values,
  )
  const residualOwnership = ResidualOwnership.make()
  const accessBoundaryPlan = Ownership.localSharedAccessBoundaryPlan(results)
  interface PreparedInstance {
    readonly instance: Omit<Instance, 'ownership'>
    readonly fact: Elaboration.FunctionFact
  }
  interface PreparedUnavailableOwnership {
    readonly key: InstanceKey
    readonly function: Hir.HirFunction
    readonly fact: Elaboration.FunctionFact
    readonly diagnostic: Diagnostic.Diagnostic
  }
  const prepared = new Map<string, PreparedInstance>()
  const preparedUnavailableOwnership = new Map<string, PreparedUnavailableOwnership>()
  const residualizationDiagnostics = new Map<string, Diagnostic.Diagnostic>()
  const selectedConstants: Array<SelectedConstant> = []
  for (const module of index.modules) {
    const moduleDiagnostics = results.get(module.module)?.diagnostics ?? Object.freeze([])
    for (const declaration of module.constants) {
      const declarationHasError = moduleDiagnostics.some(
        (diagnostic) =>
          diagnostic.severity === 'error' &&
          diagnostic.span.sourceId === declaration.syntax.span.sourceId &&
          declaration.syntax.span.start <= diagnostic.span.start &&
          diagnostic.span.end <= declaration.syntax.span.end,
      )
      if (declarationHasError) continue
      const selected = Residualization.evaluateConstant(residualization, declaration)
      if (selected._tag === 'Failed') {
        const diagnostic = StaticEvaluation.diagnostic(selected.failure, target.id)
        residualizationDiagnostics.set(
          `${diagnostic.code}:${diagnostic.span.sourceId}:${diagnostic.span.start}:${diagnostic.span.end}`,
          diagnostic,
        )
      } else if (declaration.canonical._tag === 'Canonical') {
        selectedConstants.push(
          Object.freeze({
            _tag: 'SelectedConstant',
            declaration: declaration.canonical.id,
            value: selected.value,
          }),
        )
      }
    }
  }
  const recordedCallables = new Map<string, CallableInstance>()
  /** Finds the already-discovered environment a hidden callable identity names. */
  const resolveRecordedCallable = (
    identity: Type.CallableIdentityArgument,
  ): CallableInstance | undefined => {
    const environment = identity.environment
    if (environment === undefined) return undefined
    for (const candidate of recordedCallables.values()) {
      if (
        Type.runtimeCallableEnvironmentIdentityKey(environment) ===
          Type.runtimeCallableEnvironmentIdentityKey(callableEnvironmentIdentity(candidate)) &&
        Hir.matchesCallableTargetIdentity(candidate.target, identity.target) &&
        candidate.typeArguments.length === identity.typeArguments.length &&
        candidate.typeArguments.every((argument, ordinal) => {
          const expected = identity.typeArguments.at(ordinal)
          return (
            expected !== undefined &&
            Type.runtimeGenericArgumentKey(argument) === Type.runtimeGenericArgumentKey(expected)
          )
        })
      )
        return candidate
    }
    return undefined
  }
  const recordedCalls = new Map<string, CallInstance>()
  const providerCalls = new Map<string, CallInstance>()
  const scannedContexts = new Set<string>()
  interface Ancestor {
    readonly key: InstanceKey
    readonly structuralProvider?: Type.Type
  }
  interface WorkItem {
    readonly key: InstanceKey
    readonly staticArgumentOrigins?: ReadonlyArray<StaticEvaluation.TextOrigin | undefined>
    readonly ancestors: ReadonlyMap<string, Ancestor>
    readonly cleanupReachable: boolean
  }
  const declarationText = (key: InstanceKey): string =>
    `${key.declaration.module}\u0000${key.declaration.name}`
  const sameArguments = (left: InstanceKey, right: InstanceKey): boolean =>
    left.typeArguments.length === right.typeArguments.length &&
    left.typeArguments.every((argument, index) => {
      const candidate = right.typeArguments.at(index)
      return (
        candidate !== undefined &&
        Type.runtimeGenericArgumentKey(argument) === Type.runtimeGenericArgumentKey(candidate)
      )
    })
  const sameVisibleArguments = (left: InstanceKey, right: InstanceKey): boolean => {
    const leftVisible = left.typeArguments.filter(
      (argument) => !Type.isHiddenExecutableArgument(argument),
    )
    const rightVisible = right.typeArguments.filter(
      (argument) => !Type.isHiddenExecutableArgument(argument),
    )
    return (
      leftVisible.length === rightVisible.length &&
      leftVisible.every((argument, index) => {
        const candidate = rightVisible.at(index)
        return (
          candidate !== undefined &&
          Type.runtimeGenericArgumentKey(argument) === Type.runtimeGenericArgumentKey(candidate)
        )
      })
    )
  }
  const rootItem = (key: InstanceKey): WorkItem =>
    Object.freeze({
      key,
      ancestors: new Map([[declarationText(key), Object.freeze({ key })]]),
      cleanupReachable: false,
    })
  const pending: Array<WorkItem> = [
    ...(entry._tag === 'Resolved' ? [rootItem(entry.key)] : []),
    ...retention.map(rootItem),
  ]
  // The export inventory is recorded for every target so planning can reject it off native;
  // only a native target seeds the executable worklist with its implementation body.
  if (target.kind === 'Native')
    for (const record of foreignExports) pending.push(rootItem(record.key))
  const violations: Array<PolymorphicRecursion> = []
  const violationKeys = new Set<string>()
  const specializationFailures = new Map<string, NonConcreteSpecialization>()
  const recordedContexts = new Map<string, Map<string, WorkItem>>()
  const contextText = (item: WorkItem): string =>
    `${item.cleanupReachable ? 'cleanup' : 'ordinary'}\u0001${keyText(item.key)}\u0001${[
      ...item.ancestors.entries(),
    ]
      .sort(([left], [right]) => {
        if (left < right) return -1
        if (left > right) return 1
        return 0
      })
      .map(
        ([declaration, ancestor]) =>
          `${declaration}\u0002${keyText(ancestor.key)}\u0002${ancestor.structuralProvider === undefined ? '' : Type.key(ancestor.structuralProvider)}`,
      )
      .join('\u0003')}`
  const cleanupPrepassTargets = (
    fn: Hir.HirFunction,
    fact: Elaboration.FunctionFact,
    substitution: Type.Substitution,
  ): ReadonlyArray<CallTarget> => {
    const types = new Map<string, Type.Type>()
    for (const parameter of fn.declaration.parameters) {
      if (parameter.phase !== 'Runtime' || parameter.declaredType._tag !== 'Resolved') continue
      const type = Type.substitute(parameter.declaredType.type, substitution)
      types.set(Type.key(type), type)
    }
    Elaboration.visitStatementFacts(fact.statements, {
      expression: (expression) => {
        if (expression.type._tag !== 'Available') return
        const type = Type.substitute(expression.type.type, substitution)
        types.set(Type.key(type), type)
      },
    })
    return Object.freeze(
      [...types.values()].flatMap((type) => hookCalls(CleanupPlan.cleanupPlan(index, type), index)),
    )
  }
  let graph: ExecutableOrigin.SuspensionGraph | undefined
  while (true) {
    while (pending.length > 0) {
      const item = pending.shift()
      if (item === undefined) continue
      const context = contextText(item)
      if (scannedContexts.has(context)) continue
      scannedContexts.add(context)
      const key = item.key
      const ownerContexts = recordedContexts.get(keyText(key)) ?? new Map<string, WorkItem>()
      ownerContexts.set(context, item)
      recordedContexts.set(keyText(key), ownerContexts)
      const template = functionByKey(results, key)
      if (template === undefined) continue
      const application = Object.freeze({
        declaration: key.declaration,
        typeArguments: key.typeArguments,
        evidence: key.evidence,
        contractRow: key.contractRow,
        staticArguments: key.staticArguments,
        ...(item.staticArgumentOrigins === undefined
          ? {}
          : { staticArgumentOrigins: item.staticArgumentOrigins }),
      })
      const residual = Residualization.residualize(residualization, application)
      if (residual._tag === 'StaticFailure') {
        const diagnostic = StaticEvaluation.diagnostic(residual.failure, target.id)
        residualizationDiagnostics.set(
          `${diagnostic.code}:${diagnostic.span.sourceId}:${diagnostic.span.start}:${diagnostic.span.end}`,
          diagnostic,
        )
        continue
      }
      const selectedCompileError = residual.diagnostics.findIndex(
        (diagnostic) => diagnostic.code === Diagnostic.selectedCompileErrorCode,
      )
      const residualDiagnostics =
        selectedCompileError < 0
          ? residual.diagnostics
          : residual.diagnostics.slice(0, selectedCompileError + 1)
      for (const diagnostic of residualDiagnostics)
        residualizationDiagnostics.set(
          `${diagnostic.code}:${diagnostic.span.sourceId}:${diagnostic.span.start}:${diagnostic.span.end}`,
          diagnostic,
        )
      const residualError = residualDiagnostics.find(
        (diagnostic) => diagnostic.severity === 'error',
      )
      if (residualError !== undefined) {
        preparedUnavailableOwnership.set(
          keyText(key),
          Object.freeze({
            key,
            function: residual.function,
            fact: residual.fact,
            diagnostic: residualError,
          }),
        )
        continue
      }
      const fn = residual.function
      const parameters = template.declaration.typeParameters.map((parameter) => parameter.type)
      const selected = TypeInference.selectedSubstitution(
        parameters,
        key.typeArguments.filter((argument) => !Type.isHiddenExecutableArgument(argument)),
      )
      const substitution = selected?.substitution
      // A key whose arguments no longer fit the declaration's binders is as unreachable as one
      // that cannot be made concrete; both are reported rather than silently dropped.
      const specialization =
        substitution === undefined
          ? undefined
          : specialize(fn, substitution, index, selected?.compatibility)
      if (substitution === undefined || specialization === undefined) {
        specializationFailures.set(
          keyText(key),
          Object.freeze({
            _tag: 'NonConcreteSpecialization',
            key,
            span: fn.declaration.syntax.span,
          }),
        )
        continue
      }
      if (!prepared.has(keyText(key))) {
        const resultCallable = resultCallableIdentity(fn, key, results, index)
        const resultEffect = resultEffectIdentity(fn, key, results, index)
        prepared.set(
          keyText(key),
          Object.freeze({
            fact: residual.fact,
            instance: Object.freeze({
              _tag: 'Instance',
              key,
              function: fn,
              substitution,
              specialization,
              ...(resultCallable === undefined ? {} : { resultCallable }),
              ...(resultEffect === undefined ? {} : { resultEffect }),
            }),
          }),
        )
      }
      for (const callable of concreteCallables(
        fn,
        key,
        substitution,
        results,
        index,
        resolveRecordedCallable,
      )) {
        recordedCallables.set(callableIdentity(callable), callable)
      }
      const cleanupHooks = cleanupPrepassTargets(fn, residual.fact, substitution)
      const calls = new Map<string, CallTarget>()
      const directCalls = directCallInstances(fn, key, substitution, results, index)
      const callableTargets = callableCallTargets(fn, key, substitution, results, index)
      for (const call of directCalls) {
        recordedCalls.set(
          `${keyText(call.owner)}\u0005${call.span.sourceId}:${call.span.start}:${call.span.end}`,
          call,
        )
      }
      const cleanupTargets = [
        ...slotDropHookTargets(fn, index, substitution),
        ...cleanupHooks,
        ...(entry._tag === 'Resolved' &&
        entry.kind === 'Effect' &&
        keyText(key) === keyText(entry.key)
          ? entry.failures.flatMap((failure) =>
              hookCalls(CleanupPlan.cleanupPlan(index, failure.type), index),
            )
          : []),
      ]
      const identityOfCall = Specialization.key
      const cleanupIdentities = new Set(cleanupTargets.map(identityOfCall))
      const reachableCalls: ReadonlyArray<CallTarget> = [
        ...bodyCallTargets(fn, index, substitution),
        ...interfaceWitnessTargets(fn, index, substitution),
        ...requirementBindingCallTargets(fn, substitution, index),
        ...directCalls.map((call) => ({
          declaration: call.target.declaration,
          typeArguments: call.target.typeArguments,
          evidence: call.target.evidence,
          staticArguments: call.target.staticArguments,
          ...(call.staticArgumentOrigins === undefined
            ? {}
            : { staticArgumentOrigins: call.staticArgumentOrigins }),
        })),
        ...forwardedRequirementCallTargets(directCalls, results, index),
        ...callableTargets,
        ...forwardedRequirementTargets(callableTargets, results, index),
        ...cleanupTargets,
      ]
      for (const call of reachableCalls) {
        const identity = identityOfCall(call)
        const existing = calls.get(identity)
        // An ordinary edge must keep the recursion guard even when the same target is also reached
        // through a proved dependency or conditional witness root. Conflicting provider evidence is
        // equally unsafe: descent is granted only where this path has one unambiguous measure.
        if (existing === undefined) {
          calls.set(identity, call)
          continue
        }
        const existingOrdinary = existing.structuralProvider === undefined
        const callOrdinary = call.structuralProvider === undefined
        if (existingOrdinary) continue
        if (callOrdinary) {
          calls.set(
            identity,
            Object.freeze({
              declaration: call.declaration,
              typeArguments: call.typeArguments,
              ...(call.evidence === undefined ? {} : { evidence: call.evidence }),
              ...(call.staticArguments === undefined
                ? {}
                : { staticArguments: call.staticArguments }),
              ...(call.staticArgumentOrigins === undefined
                ? {}
                : { staticArgumentOrigins: call.staticArgumentOrigins }),
            }),
          )
          continue
        }
        if (
          existing.structuralProvider !== undefined &&
          call.structuralProvider !== undefined &&
          !Type.equals(existing.structuralProvider, call.structuralProvider)
        )
          calls.set(
            identity,
            Object.freeze({
              declaration: call.declaration,
              typeArguments: call.typeArguments,
              ...(call.evidence === undefined ? {} : { evidence: call.evidence }),
              ...(call.staticArguments === undefined
                ? {}
                : { staticArguments: call.staticArguments }),
              ...(call.staticArgumentOrigins === undefined
                ? {}
                : { staticArgumentOrigins: call.staticArgumentOrigins }),
            }),
          )
      }
      for (const call of calls.values()) {
        const target = call.declaration
        const targetFunction = results
          .get(target.module)
          ?.hir.functions.find(
            (candidate) =>
              candidate.declaration.canonical._tag === 'Canonical' &&
              candidate.declaration.canonical.id.name === target.name,
          )
        if (targetFunction === undefined) continue
        const targetArguments = call.typeArguments.map((argument) =>
          Type.substituteGenericArgument(argument, substitution),
        )
        const targetKey = keyOf(
          target,
          targetFunction.contract,
          targetFunction.declaration.typeParameters.map((parameter) => parameter.type),
          targetArguments,
          call.staticArguments ?? Object.freeze([]),
          call.evidence ?? Object.freeze([]),
        )
        const ancestor = item.ancestors.get(declarationText(targetKey))
        const structurallyDescending =
          call.structuralProvider !== undefined &&
          ancestor?.structuralProvider !== undefined &&
          Type.isStrictStructuralSubterm(call.structuralProvider, ancestor.structuralProvider)
        const terminalCallableSpecialization =
          ancestor !== undefined &&
          sameVisibleArguments(ancestor.key, targetKey) &&
          targetKey.typeArguments.some(Type.isCallableIdentityArgument) &&
          targetKey.typeArguments
            .filter(Type.isHiddenIdentityArgument)
            .every(
              (argument) =>
                Type.isCallableIdentityArgument(argument) && argument.environment === undefined,
            )
        if (
          ancestor !== undefined &&
          !sameArguments(ancestor.key, targetKey) &&
          !structurallyDescending &&
          !terminalCallableSpecialization &&
          !(
            prepared.has(keyText(targetKey)) &&
            (item.cleanupReachable || cleanupIdentities.has(identityOfCall(call)))
          )
        ) {
          const violationKey = `${keyText(key)}\u0000${keyText(targetKey)}`
          if (!violationKeys.has(violationKey)) {
            violationKeys.add(violationKey)
            violations.push(
              Object.freeze({ _tag: 'PolymorphicRecursion', caller: key, target: targetKey }),
            )
          }
          continue
        }
        pending.push(
          Object.freeze({
            key: targetKey,
            ...(call.staticArgumentOrigins === undefined
              ? {}
              : { staticArgumentOrigins: call.staticArgumentOrigins }),
            ancestors: new Map(item.ancestors).set(
              declarationText(targetKey),
              Object.freeze({
                key: targetKey,
                ...(call.structuralProvider === undefined
                  ? {}
                  : { structuralProvider: call.structuralProvider }),
              }),
            ),
            cleanupReachable: item.cleanupReachable || cleanupIdentities.has(identityOfCall(call)),
          }),
        )
      }
    }

    const currentInstances = Object.freeze(
      [...prepared.values()].map((candidate) => candidate.instance),
    )
    const currentGraph = suspensionGraph(currentInstances, results, index)
    providerCalls.clear()
    for (const provided of currentGraph.providedTargets) {
      const target = functionByKey(results, provided.target)
      const resultEffect =
        target === undefined
          ? undefined
          : resultEffectIdentity(target, provided.target, results, index)
      providerCalls.set(
        `${keyText(provided.owner)}\u0005${provided.span.sourceId}:${provided.span.start}:${provided.span.end}\u0005${keyText(provided.target)}`,
        Object.freeze({
          _tag: 'CallInstance',
          owner: provided.owner,
          span: provided.span,
          target: provided.target,
          ...(provided.providers === undefined ? {} : { providers: provided.providers }),
          ...(provided.staticArgumentOrigins === undefined
            ? {}
            : { staticArgumentOrigins: provided.staticArgumentOrigins }),
          ...(resultEffect === undefined ? {} : { resultEffect }),
        }),
      )
    }
    let scheduledProvided = false
    for (const provided of currentGraph.providedTargets) {
      for (const ownerContext of recordedContexts.get(keyText(provided.owner))?.values() ?? []) {
        const ancestors = new Map(ownerContext.ancestors)
        const declaration = declarationText(provided.target)
        const ancestor = ancestors.get(declaration)
        if (ancestor !== undefined && !sameArguments(ancestor.key, provided.target)) {
          const violationKey = `${keyText(provided.owner)}\u0000${keyText(provided.target)}`
          if (!violationKeys.has(violationKey)) {
            violationKeys.add(violationKey)
            violations.push(
              Object.freeze({
                _tag: 'PolymorphicRecursion',
                caller: provided.owner,
                target: provided.target,
              }),
            )
          }
          continue
        }
        ancestors.set(declaration, Object.freeze({ key: provided.target }))
        const item = Object.freeze({
          key: provided.target,
          ancestors,
          cleanupReachable: ownerContext.cleanupReachable,
        })
        if (scannedContexts.has(contextText(item))) continue
        pending.push(item)
        scheduledProvided = true
      }
    }
    if (!scheduledProvided) {
      graph = currentGraph
      break
    }
  }

  // Success identities may resolve through another instance's block, so they are traced only once
  // every instance is prepared.
  const preparedInstances = [...prepared.values()].map((candidate) => candidate.instance)
  const instances = Object.freeze(
    [...prepared.values()].map(({ instance, fact }) => {
      const checked = ResidualOwnership.check(
        residualOwnership,
        Ownership.input(instance.function, fact, index, accessBoundaryPlan),
        Residualization.selectionReason(residualization, instance.key) === undefined
          ? 'UnchangedBody'
          : 'SelectedStaticBody',
      )
      for (const diagnostic of checked.diagnostics)
        residualizationDiagnostics.set(
          `${diagnostic.code}:${diagnostic.span.sourceId}:${diagnostic.span.start}:${diagnostic.span.end}`,
          diagnostic,
        )
      return Object.freeze({
        ...instance,
        effectSuccesses: effectSuccesses(
          instance.function,
          instance.key,
          instance.substitution,
          results,
          index,
          preparedInstances,
        ),
        ownership: checked.ownership,
      })
    }),
  )
  const unavailableOwnership = Object.freeze(
    [...preparedUnavailableOwnership.values()].map((candidate) => {
      const checked = ResidualOwnership.check(
        residualOwnership,
        Ownership.input(candidate.function, candidate.fact, index, accessBoundaryPlan),
        Residualization.selectionReason(residualization, candidate.key) === undefined
          ? 'UnchangedBody'
          : 'SelectedStaticBody',
      )
      for (const diagnostic of checked.diagnostics)
        residualizationDiagnostics.set(
          `${diagnostic.code}:${diagnostic.span.sourceId}:${diagnostic.span.start}:${diagnostic.span.end}`,
          diagnostic,
        )
      return Object.freeze({
        _tag: 'UnavailableResidualOwnership' as const,
        key: candidate.key,
        ownership: Object.freeze({
          ...checked.ownership,
          verdict: Object.freeze({
            _tag: 'Unavailable' as const,
            cause: Diagnostic.identity(candidate.diagnostic),
          }),
        }),
      })
    }),
  )
  const finalGraph = graph ?? suspensionGraph(instances, results, index)
  const summaries = ExecutableOrigin.suspensionSummaries(finalGraph)
  const summaryOfNode = (node: string): SuspensionMode.Summary =>
    summaries.get(node) ?? SuspensionMode.direct
  const callInstances = Object.freeze([...recordedCalls.values(), ...providerCalls.values()])
  return Object.freeze({
    _tag: 'InstanceDiscovery',
    retention: Object.freeze(retention),
    rootModule,
    entry,
    instances,
    unavailableOwnership,
    callables: Object.freeze([...recordedCallables.values()]),
    effects: concreteEffects(
      instances,
      summaries,
      results,
      index,
      Object.freeze([...recordedCallables.values()]),
    ),
    calls: callInstances,
    intrinsics: ExecutableOrigin.reachableIntrinsics(instances, index),
    foreignCalls: ExecutableOrigin.reachableForeignCalls(instances, index, target),
    foreignExports,
    constants: Object.freeze(selectedConstants),
    suspension: Object.freeze([
      ...instances
        .slice()
        .sort((left, right) => compareInstanceKeys(left.key, right.key))
        .flatMap((instance): ReadonlyArray<SuspensionFact> => {
          const execution = summaryOfNode(instanceNode(instance.key))
          const result =
            instance.resultEffect === undefined
              ? SuspensionMode.direct
              : summaryOfNode(effectNode(instance.resultEffect))
          return Object.freeze([
            Object.freeze({
              _tag: 'SuspensionFact',
              subject: Object.freeze({ _tag: 'Instance', key: instance.key }),
              summary: SuspensionMode.join([execution, result]),
            }),
            Object.freeze({
              _tag: 'SuspensionFact',
              subject: Object.freeze({ _tag: 'Execution', key: instance.key }),
              summary: execution,
            }),
          ])
        }),
      ...[...finalGraph.effectIdentities].sort().map((identity): SuspensionFact =>
        Object.freeze({
          _tag: 'SuspensionFact',
          subject: Object.freeze({ _tag: 'Effect', identity }),
          summary: summaryOfNode(effectNode(identity)),
        }),
      ),
    ]),
    residualizationDiagnostics: Object.freeze([...residualizationDiagnostics.values()]),
    specializationFailures: Object.freeze([...specializationFailures.values()]),
    violations: Object.freeze(violations),
    counters: Object.freeze({
      _tag: 'InstanceDiscoveryCounters',
      residualBodies: Residualization.counters(residualization),
      residualOwnership: ResidualOwnership.counters(residualOwnership),
    }),
    residualBodies: Residualization.observations(residualization),
    residualOwnership: ResidualOwnership.observations(residualOwnership),
  })
}
