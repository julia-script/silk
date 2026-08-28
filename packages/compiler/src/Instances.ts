import * as CleanupPlan from './CleanupPlan.js'
import * as ConformanceProof from './ConformanceProof.js'
import * as Constraint from './Constraint.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Elaboration from './Elaboration.js'
import * as ExecutableOrigin from './ExecutableOrigin.js'
import * as Hir from './Hir.js'
import type * as Intrinsic from './Intrinsic.js'
import * as TypeInference from './internal/TypeInference.js'
import * as Ownership from './Ownership.js'
import * as ProviderSelection from './ProviderSelection.js'
import * as RowAlgebra from './RowAlgebra.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Specialization from './Specialization.js'
import * as SuspensionMode from './SuspensionMode.js'
import * as Type from './Type.js'

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
}

/** One discovered instance with its elaborated HIR function. */
export interface Instance {
  readonly _tag: 'Instance'
  readonly key: InstanceKey
  readonly function: Hir.HirFunction
  readonly substitution: Type.Substitution
  readonly specialization: ConcreteSpecialization
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
    readonly source: 'Parameter' | 'Binding'
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

/** One monomorphic ordinary/effect constructor call with hidden Effect identities resolved. */
export interface CallInstance {
  readonly _tag: 'CallInstance'
  readonly owner: InstanceKey
  readonly span: Hir.Expression['span']
  readonly target: InstanceKey
  readonly resultEffect?: string
}

/** One exact sealed intrinsic call retained by executable instance closure. */
export interface IntrinsicCall {
  readonly _tag: 'ReachableIntrinsicCall'
  readonly operation: Intrinsic.OperationId
  readonly span: Hir.Expression['span']
}

/** One normalized owned failure retained by an effectful user entry. */
export interface EntryFailure {
  readonly type: Type.Type
  readonly identity: string
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
      readonly _tag: 'Unavailable'
      readonly reason:
        | 'MissingEntry'
        | 'AmbiguousEntry'
        | 'GenericEntry'
        | 'ParameterizedEntry'
        | 'PrivateEntry'
        | 'UntypedEntry'
        | 'InvalidOrdinaryEntryResult'
        | 'InvalidEffectEntryResult'
        | 'EffectEntryRequirements'
        | 'InvalidSource'
      readonly requirements?: ReadonlyArray<Type.Requirement>
    }

/** The deterministic discovery result. */
export interface Discovery {
  readonly _tag: 'InstanceDiscovery'
  readonly rootModule: string
  readonly entry: Entry
  readonly instances: ReadonlyArray<Instance>
  readonly callables: ReadonlyArray<CallableInstance>
  readonly effects: ReadonlyArray<EffectInstance>
  readonly calls: ReadonlyArray<CallInstance>
  readonly intrinsics: ReadonlyArray<IntrinsicCall>
  /** Exact direct/nested/external-park summaries in canonical subject order. */
  readonly suspension: ReadonlyArray<SuspensionFact>
  readonly specializationFailures: ReadonlyArray<NonConcreteSpecialization>
  readonly violations: ReadonlyArray<PolymorphicRecursion>
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
    rootModule,
    entry: Object.freeze({ _tag: 'Unavailable', reason: 'InvalidSource' }),
    instances: Object.freeze([]),
    callables: Object.freeze([]),
    effects: Object.freeze([]),
    calls: Object.freeze([]),
    intrinsics: Object.freeze([]),
    suspension: Object.freeze([]),
    specializationFailures: Object.freeze([]),
    violations: Object.freeze([]),
  })

/**
 * Closes a partial section's binder-owned channels before the type becomes instance identity.
 *
 * A constrained partial section is deliberately open in its target contract's own unapplied
 * binders: its Effect channels close only at application, so its surface mentions binder-owned
 * failure types and requirement rows that no substitution at a carrying call can ever resolve.
 * Elaboration's constrained
 * callable escape gate proves such a value only ever reaches a whole-value relay, an application,
 * or a drop — every other escape is rejected there with its own diagnostic — and the callable
 * itself is erased onto its hidden identity argument. The instance identity therefore closes the
 * schema's own failure-channel binders to `never` and requirement binders to empty rows, exactly
 * the shape the erased relay needs, so a proven relay is not re-rejected as unresolved.
 */
const carriedSectionArgument = (argument: Type.GenericArgument): Type.GenericArgument => {
  if (!Type.isTypeArgument(argument)) return argument
  if (!Type.isCallable(argument) || argument.schema === undefined) return argument
  if (Type.isRuntimeConcrete(argument)) return argument
  const closure = new Map<string, Type.GenericArgument>()
  const failureBinders = new Set<string>()
  Type.visit(argument, (type) => {
    if (!Type.isEffect(type)) return
    for (const parameter of Type.failureMemberParameters(type))
      failureBinders.add(Type.key(parameter))
  })
  for (const binder of argument.schema.binders) {
    if (binder.kind === 'RequirementRow')
      closure.set(Type.key(binder), Type.requirementRowArgument([]))
    else if (binder.kind === 'Value' && failureBinders.has(Type.key(binder)))
      closure.set(Type.key(binder), 'never')
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
): InstanceKey =>
  (() => {
    const typeArguments = rawTypeArguments.map(carriedSectionArgument)
    const substitution = TypeInference.substitution(
      typeParameters,
      typeArguments.filter((argument) => !Type.isHiddenExecutableArgument(argument)),
    )
    if (substitution === undefined) {
      throw new RangeError('Instance key type arguments do not match declaration parameters')
    }
    return Object.freeze({
      _tag: 'InstanceKey',
      declaration,
      typeArguments: Object.freeze(Array.from(typeArguments)),
      contractRow:
        contract._tag === 'Contract'
          ? Object.freeze([
              ...contract.parameters.map((type) => Type.key(Type.substitute(type, substitution))),
              `result:${Type.key(Type.substitute(contract.result, substitution))}`,
              ...(contract.failureRow === undefined
                ? []
                : [
                    `failures:${RowAlgebra.key(
                      Type.failureRowPolicy(),
                      Type.substituteFailureRow(contract.failureRow, substitution),
                    )}`,
                  ]),
              ...(contract.requirementRow === undefined
                ? []
                : [
                    `requirements:${RowAlgebra.key(
                      Type.requirementRowPolicy(),
                      Type.substituteRequirementsRow(contract.requirementRow, substitution),
                    )}`,
                  ]),
              ...contract.constraints.map(
                (constraint) =>
                  `constraint:${Constraint.key(Constraint.substitute(constraint, substitution))}`,
              ),
            ])
          : Object.freeze([]),
    })
  })()

export const keyText = (key: InstanceKey): string =>
  `${key.declaration.module}\u0000${key.declaration.name}\u0000${key.typeArguments
    .map(Type.genericArgumentKey)
    .join('\u0000')}\u0002${key.contractRow.join('\u0000')}`

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
): ConcreteSpecialization | undefined => {
  if (fn.contract._tag !== 'Contract') return undefined
  const parameters = fn.contract.parameters.map((parameter) =>
    Type.substitute(parameter, substitution),
  )
  const result = Type.substitute(fn.contract.result, substitution)
  if (
    parameters.some((parameter) => !Type.isRuntimeConcrete(parameter)) ||
    !Type.isRuntimeConcrete(result)
  )
    return undefined

  const failureRow =
    fn.contract.failureRow === undefined
      ? undefined
      : Type.substituteFailureRow(fn.contract.failureRow, substitution)
  const requirementRow =
    fn.contract.requirementRow === undefined
      ? undefined
      : Type.substituteRequirementsRow(fn.contract.requirementRow, substitution)
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
  const identity = Specialization.key(specialization)
  return self.instances.filter((candidate) => Specialization.key(candidate.key) === identity)
}

export const effectIdentity = (owner: InstanceKey, site: Hir.EffectSiteId): string =>
  `${keyText(owner)}\u0004${Hir.executableSiteKey(site)}`

const resolveEntry = (root: Elaboration.Result): Entry => {
  const lookup = Elaboration.declarationByName(root, 'main')
  if (lookup._tag === 'Missing')
    return Object.freeze({ _tag: 'Unavailable', reason: 'MissingEntry' })
  if (lookup._tag === 'Ambiguous') {
    return Object.freeze({ _tag: 'Unavailable', reason: 'AmbiguousEntry' })
  }
  const declaration = lookup.declaration
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
  TypeInference.substitution(
    fn.declaration.typeParameters.map((parameter) => parameter.type),
    key.typeArguments.filter((argument) => !Type.isHiddenExecutableArgument(argument)),
  )

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
  `${keyText(self.owner)}\u0001${Hir.executableSiteKey(self.site)}\u0001${self.typeArguments.map(Type.genericArgumentKey).join('\u0000')}`

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
 * Discovers the reachable instances from the root module's entry. The worklist records an
 * instance before following its calls, so directly and mutually recursive programs terminate.
 */
export const discover = (
  rootModule: string,
  results: ReadonlyMap<string, Elaboration.Result>,
  ownership: ReadonlyMap<string, Ownership.ModuleOwnership>,
  index: DeclarationIndex.Index,
): Discovery => {
  const root = results.get(rootModule)
  if (root === undefined) {
    throw new RangeError(`Instance discovery lost its root module ${rootModule}`)
  }
  const entry = resolveEntry(root)
  if (entry._tag !== 'Resolved') {
    return Object.freeze({
      _tag: 'InstanceDiscovery',
      rootModule,
      entry,
      instances: Object.freeze([]),
      callables: Object.freeze([]),
      effects: Object.freeze([]),
      calls: Object.freeze([]),
      intrinsics: Object.freeze([]),
      suspension: Object.freeze([]),
      specializationFailures: Object.freeze([]),
      violations: Object.freeze([]),
    })
  }

  const recorded = new Map<string, Instance>()
  const recordedCallables = new Map<string, CallableInstance>()
  const recordedCalls = new Map<string, CallInstance>()
  const providerCalls = new Map<string, CallInstance>()
  const scannedContexts = new Set<string>()
  interface Ancestor {
    readonly key: InstanceKey
    readonly structuralProvider?: Type.Type
  }
  interface WorkItem {
    readonly key: InstanceKey
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
        Type.genericArgumentKey(argument) === Type.genericArgumentKey(candidate)
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
          Type.genericArgumentKey(argument) === Type.genericArgumentKey(candidate)
        )
      })
    )
  }
  const pending: Array<WorkItem> = [
    Object.freeze({
      key: entry.key,
      ancestors: new Map([[declarationText(entry.key), Object.freeze({ key: entry.key })]]),
      cleanupReachable: false,
    }),
  ]
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
      const fn = functionByKey(results, key)
      if (fn === undefined) continue
      const parameters = fn.declaration.typeParameters.map((parameter) => parameter.type)
      const substitution = TypeInference.substitution(
        parameters,
        key.typeArguments.filter((argument) => !Type.isHiddenExecutableArgument(argument)),
      )
      if (substitution === undefined) continue
      const specialization = specialize(fn, substitution, index)
      if (specialization === undefined) {
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
      if (!recorded.has(keyText(key))) {
        const resultCallable = resultCallableIdentity(fn, key, results, index)
        const resultEffect = resultEffectIdentity(fn, key, results, index)
        recorded.set(
          keyText(key),
          Object.freeze({
            _tag: 'Instance',
            key,
            function: fn,
            substitution,
            specialization,
            effectSuccesses: effectSuccesses(fn, key, substitution, results, index),
            ...(resultCallable === undefined ? {} : { resultCallable }),
            ...(resultEffect === undefined ? {} : { resultEffect }),
          }),
        )
      }
      for (const callable of concreteCallables(fn, key, substitution, results, index)) {
        recordedCallables.set(callableIdentity(callable), callable)
      }
      const functionOwnership = ownership
        ?.get(key.declaration.module)
        ?.functions.find(
          (candidate) => candidate.declaration.id.ordinal === fn.declaration.id.ordinal,
        )
      // Deferred effect-body bindings publish only through exit releases, so the joined binding
      // facts and the exit releases both feed hook reachability.
      const cleanupHooks = [
        ...Ownership.allBindings(functionOwnership).map((binding) => binding.cleanup),
        ...(functionOwnership?.exits.flatMap((exit) =>
          exit.releases.map((release) => release.cleanup),
        ) ?? []),
      ]
        .map((cleanup) =>
          CleanupPlan.specializeCleanup(cleanup, substitution, (type) =>
            CleanupPlan.cleanupPlan(index, type),
          ),
        )
        .flatMap((cleanup) => hookCalls(cleanup, index))
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
        ...(entry.kind === 'Effect' && keyText(key) === keyText(entry.key)
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
            Object.freeze({ declaration: call.declaration, typeArguments: call.typeArguments }),
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
            Object.freeze({ declaration: call.declaration, typeArguments: call.typeArguments }),
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
            recorded.has(keyText(targetKey)) &&
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

    const currentInstances = Object.freeze([...recorded.values()])
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

  const instances = Object.freeze([...recorded.values()])
  const finalGraph = graph ?? suspensionGraph(instances, results, index)
  const summaries = ExecutableOrigin.suspensionSummaries(finalGraph)
  const summaryOfNode = (node: string): SuspensionMode.Summary =>
    summaries.get(node) ?? SuspensionMode.direct
  const callInstances = Object.freeze([...recordedCalls.values(), ...providerCalls.values()])
  return Object.freeze({
    _tag: 'InstanceDiscovery',
    rootModule,
    entry,
    instances,
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
      ...[...finalGraph.effectIdentities].sort().map(
        (identity): SuspensionFact =>
          Object.freeze({
            _tag: 'SuspensionFact',
            subject: Object.freeze({ _tag: 'Effect', identity }),
            summary: summaryOfNode(effectNode(identity)),
          }),
      ),
    ]),
    specializationFailures: Object.freeze([...specializationFailures.values()]),
    violations: Object.freeze(violations),
  })
}
