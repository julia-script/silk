import * as ConformanceGoal from './ConformanceGoal.js'
import * as ResolutionWork from './ResolutionWork.js'
import type * as Constraint from './Constraint.js'
import type {
  CanonicalId,
  CanonicalState,
  ConformanceFact,
  ConformanceWitness,
  ContractFact,
  FieldFact,
  UnionVariantFact,
} from './DeclarationFacts.js'
import { byCanonical } from './DeclarationFacts.js'
import type { Index } from './DeclarationIndex.js'
import { declaredRequirements, memberByNominal } from './DeclarationResolution.js'
import * as Intrinsic from './Intrinsic.js'
import * as TypeInference from './internal/TypeInference.js'
import * as Specialization from './Specialization.js'
import * as Type from './Type.js'

/** The deterministic result of asking the compiler's one `Copy` authority. */
export type CopyProof =
  | { readonly _tag: 'Copy' }
  | { readonly _tag: 'NotCopy'; readonly reason: string }
  | { readonly _tag: 'UnavailableCopy'; readonly reason: string }

/** Tests whether every value of one concrete type copies freely. */
export const copyType = (
  self: Index,
  type: Type.Type,
  assumptions: ReadonlySet<string> = new Set(),
): boolean => copyProof(self, type, assumptions)._tag === 'Copy'

export const contractByCapability = (
  self: Index,
  capability: Type.Nominal,
): ContractFact | undefined => {
  const declaration = memberByNominal(self.modules, capability)
  return declaration?._tag === 'InterfaceDeclaration' || declaration?._tag === 'ServiceDeclaration'
    ? declaration
    : undefined
}

/** One conformance whose head covers a concrete goal, with the arguments matching it bound. */
interface ConformanceCandidate {
  readonly module: string
  readonly conformance: ConformanceFact
  readonly substitution: Type.Substitution
}

/** Returns every admitted conformance whose head covers one concrete goal. */
export const conformanceCandidates = (
  self: Index,
  goal: ConformanceGoal.ConformanceGoal,
  admission: 'Selectable' | 'Declared' = 'Selectable',
): ReadonlyArray<ConformanceCandidate> => {
  const work = ResolutionWork.begin(
    ResolutionWork.ofIndex(self),
    {
      kind: 'ConformanceGoal',
      key: `${admission}:${ConformanceGoal.key(goal)}`,
    },
    'ConformanceDiscovery',
  )
  return Object.freeze(
    self.modules.flatMap((module) =>
      module.conformances.flatMap((conformance): ReadonlyArray<ConformanceCandidate> => {
        ResolutionWork.visit(work)
        if (
          conformance.capability._tag !== 'Resolved' ||
          !Type.isNominal(conformance.capability.type) ||
          conformance.provider._tag !== 'Resolved' ||
          (admission === 'Selectable' && conformance.validity._tag !== 'ValidConformance') ||
          conformance.coherence._tag !== 'Coherent' ||
          conformance.termination._tag !== 'Terminating'
        )
          return []
        const inferred = new Map<string, Type.GenericArgument>()
        if (!TypeInference.infer(conformance.provider.type, goal.provider, inferred)) return []
        if (!TypeInference.infer(conformance.capability.type, goal.capability, inferred)) return []
        ResolutionWork.accept(work)
        return Object.freeze([
          Object.freeze({ module: module.module, conformance, substitution: inferred }),
        ])
      }),
    ),
  )
}

const endpointVisible = (
  declaration: { readonly visibility: 'Public' | 'Private'; readonly canonical: CanonicalState },
  requestingModule: string,
): boolean =>
  declaration.visibility === 'Public' ||
  (declaration.canonical._tag === 'Canonical' &&
    declaration.canonical.id.module === requestingModule)

/**
 * Returns the proved, endpoint-visible contracts implemented by one concrete nominal provider.
 *
 * This is the shared authority over the same conformance evidence semantic analysis uses. Merely
 * matching a declared header is insufficient: conditional, invalid, incoherent, and ambiguous
 * conformances are admitted only when the ordinary proof selects that exact source declaration.
 *
 * Receiver-call resolution selects a concrete receiver's interface operations through this query,
 * and completion offers the same set, so the two cannot drift into offering a member the resolver
 * then rejects. Changing this filter therefore changes what compiles, not only what an editor
 * shows. Endpoint visibility is applied to the interface declaration rather than the conformance,
 * because coherence is a whole-program property and a conformance carries no module visibility.
 */
export const implementedContracts = (
  self: Index,
  requestingModule: string,
  provider: Type.Type,
): ReadonlyArray<Type.Nominal> => {
  if (!Type.isNominal(provider) || !Type.isRuntimeConcrete(provider)) return Object.freeze([])
  const providerDeclaration = memberByNominal(self.modules, provider)
  if (providerDeclaration === undefined || !endpointVisible(providerDeclaration, requestingModule))
    return Object.freeze([])

  const implemented = new Map<string, Type.Nominal>()
  for (const headers of self.modules)
    for (const conformance of headers.conformances) {
      if (
        conformance.validity._tag !== 'ValidConformance' ||
        conformance.coherence._tag !== 'Coherent' ||
        conformance.termination._tag !== 'Terminating' ||
        conformance.capability._tag !== 'Resolved' ||
        !Type.isNominal(conformance.capability.type) ||
        conformance.provider._tag !== 'Resolved'
      )
        continue
      const substitution = new Map<string, Type.GenericArgument>()
      if (!TypeInference.infer(conformance.provider.type, provider, substitution)) continue
      const specialized = Type.substitute(conformance.capability.type, substitution)
      if (
        !Type.isNominal(specialized) ||
        !Type.isRuntimeConcrete(specialized) ||
        Type.equals(specialized, Type.copyCapability) ||
        Type.equals(specialized, Type.dropCapability)
      )
        continue
      const contract = contractByCapability(self, specialized)
      if (contract === undefined || !endpointVisible(contract, requestingModule)) continue
      const proof = prove(self, provider, specialized)
      if (
        proof._tag !== 'Proved' ||
        proof.selection._tag !== 'SourceSelection' ||
        proof.selection.module !== conformance.module ||
        proof.selection.ordinal !== conformance.ordinal
      )
        continue
      implemented.set(Type.key(specialized), specialized)
    }
  return Object.freeze(
    [...implemented.entries()]
      .sort(([left], [right]) => {
        if (left < right) {
          return -1
        }
        if (left > right) {
          return 1
        }
        return 0
      })
      .map(([, capability]) => capability),
  )
}

const proofMemos = new WeakMap<Index, Map<string, ConformanceGoal.Proof>>()

const provedCopy: CopyProof = Object.freeze({ _tag: 'Copy' })

export const copyAssumptions = (conformance: ConformanceFact): ReadonlySet<string> =>
  new Set(
    conformance.requirements.flatMap((requirement) =>
      requirement.capability._tag === 'Resolved' &&
      Type.equals(requirement.capability.type, Type.copyCapability)
        ? [Type.key(requirement.parameter)]
        : [],
    ),
  )

const hasDropConformance = (self: Index, provider: Type.Type): boolean =>
  conformanceCandidates(self, ConformanceGoal.make(Type.dropCapability, provider)).length > 0

/** Reports whether one concrete nominal has exactly one admitted empty `Copy` declaration. */
export const hasCopyDeclaration = (self: Index, provider: Type.Type): boolean =>
  conformanceCandidates(self, ConformanceGoal.make(Type.copyCapability, provider)).length === 1

/**
 * Proves whether one semantic type duplicates without user code or cleanup.
 *
 * Nominal fields never imply the answer on their own: an admitted empty `impl Copy` opens the
 * proof, and every reachable field must then close it. Parameters close only through an explicit
 * `Copy` bound. Cycles and damaged executable representations remain unavailable instead of being
 * guessed affine or Copy.
 */
export const copyProof = (
  self: Index,
  type: Type.Type,
  assumptions: ReadonlySet<string> = new Set(),
  active: ReadonlySet<string> = new Set(),
): CopyProof => {
  if (
    Type.isBuiltin(type) ||
    Type.isString(type) ||
    Type.isNever(type) ||
    Type.equals(type, Type.unit) ||
    Type.isPointer(type) ||
    Type.isForeignFunction(type)
  )
    return provedCopy
  if (Type.isReference(type) || Type.isSlice(type))
    return type.access === 'Shared'
      ? provedCopy
      : Object.freeze({ _tag: 'NotCopy', reason: 'exclusive borrows are affine' })
  if (Type.isParameter(type))
    return assumptions.has(Type.key(type))
      ? provedCopy
      : Object.freeze({ _tag: 'NotCopy', reason: `${type.name} has no Copy bound` })
  if (Type.isFixedArray(type)) return copyProof(self, type.element, assumptions, active)
  if (Type.isUnion(type)) {
    for (const member of type.members) {
      const proof = copyProof(self, member, assumptions, active)
      if (proof._tag !== 'Copy') return proof
    }
    return provedCopy
  }
  if (Type.isRepresented(type)) {
    const argument = type.representation.argument
    if (Type.isExactRepresentationArgument(argument)) {
      if (Type.isCallable(argument.contract))
        return argument.contract.mode === 'Shared'
          ? provedCopy
          : Object.freeze({
              _tag: 'NotCopy',
              reason: `${argument.contract.mode.toLowerCase()} callable captures are affine`,
            })
      if (Type.isEffect(argument.contract))
        return argument.contract.access === 'Shared'
          ? provedCopy
          : Object.freeze({
              _tag: 'NotCopy',
              reason: `${argument.contract.access.toLowerCase()} Effect captures are affine`,
            })
      return Object.freeze({
        _tag: 'UnavailableCopy',
        reason: 'the executable representation contract is damaged',
      })
    }
    if (Type.isCompositeEffectRepresentationArgument(argument)) {
      for (const alternative of argument.alternatives) {
        if (!Type.isEffect(alternative.contract) || alternative.contract.access !== 'Shared')
          return Object.freeze({
            _tag: 'NotCopy',
            reason: 'a selected Effect alternative has affine captures',
          })
      }
      return provedCopy
    }
    return Object.freeze({
      _tag: 'UnavailableCopy',
      reason: 'executable Copy depends on its concrete realized captures',
    })
  }
  if (Type.isCallable(type) || Type.isEffect(type))
    return Object.freeze({
      _tag: 'UnavailableCopy',
      reason: 'an open executable contract does not identify its captures',
    })
  if (!Type.isNominal(type) || Type.isIntrinsicNominal(type))
    return Object.freeze({ _tag: 'NotCopy', reason: `${Type.encode(type)} is compiler-affine` })

  const declaration = byCanonical(self, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (declaration?._tag === 'EnumDeclaration')
    return declaration.validity._tag === 'Valid'
      ? provedCopy
      : Object.freeze({
          _tag: 'UnavailableCopy',
          reason: `scalar enum ${Type.encode(type)} is invalid`,
        })

  const key = Type.key(type)
  if (active.has(key))
    return Object.freeze({
      _tag: 'UnavailableCopy',
      reason: `recursive Copy proof for ${Type.encode(type)}`,
    })
  const candidates = conformanceCandidates(self, ConformanceGoal.make(Type.copyCapability, type))
  const selected = candidates.at(0)
  if (candidates.length !== 1 || selected === undefined)
    return Object.freeze({
      _tag: candidates.length === 0 ? 'NotCopy' : 'UnavailableCopy',
      reason:
        candidates.length === 0
          ? `${Type.encode(type)} has no valid Copy impl`
          : `${Type.encode(type)} has conflicting Copy evidence`,
    })
  if (hasDropConformance(self, type))
    return Object.freeze({
      _tag: 'NotCopy',
      reason: `${Type.encode(type)} also implements Drop`,
    })
  if (declaration?._tag !== 'StructDeclaration' && declaration?._tag !== 'UnionDeclaration')
    return Object.freeze({ _tag: 'NotCopy', reason: `${Type.encode(type)} is not an aggregate` })
  if (declaration.dependency._tag === 'Unavailable')
    return Object.freeze({
      _tag: 'UnavailableCopy',
      reason: `stored fields of ${Type.encode(type)} are unavailable`,
    })
  const substitution =
    TypeInference.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      type.arguments,
    ) ?? new Map()
  const nestedAssumptions = new Set([...assumptions, ...copyAssumptions(selected.conformance)])
  const nestedActive = new Set(active).add(key)
  const fields: ReadonlyArray<{
    readonly field: FieldFact
    readonly variant?: UnionVariantFact
  }> =
    declaration._tag === 'StructDeclaration'
      ? declaration.fields.map((field) => Object.freeze({ field }))
      : declaration.variants.flatMap((variant) =>
          variant.fields.map((field) => Object.freeze({ field, variant })),
        )
  for (const { field, variant } of fields) {
    if (field.declaredType._tag !== 'Resolved')
      return Object.freeze({
        _tag: 'UnavailableCopy',
        reason: `a stored field of ${Type.encode(type)} is unresolved`,
      })
    const fieldType = Type.substitute(field.declaredType.type, substitution)
    const proof = copyProof(self, fieldType, nestedAssumptions, nestedActive)
    if (proof._tag !== 'Copy')
      return Object.freeze({
        ...proof,
        reason: `${variant === undefined ? '' : `variant ${variant.name._tag === 'Present' ? variant.name.spelling : `#${variant.id.ordinal}`} `}field ${field.name._tag === 'Present' ? field.name.spelling : `#${field.id.ordinal}`} (${Type.encode(fieldType)}): ${proof.reason}`,
      })
  }
  return provedCopy
}

const provedGoal = (
  goal: ConformanceGoal.ConformanceGoal,
  selection: ConformanceGoal.Selection,
  typeArguments: ReadonlyArray<Type.GenericArgument>,
  requirements: ReadonlyArray<ConformanceGoal.Proof>,
): ConformanceGoal.Proof =>
  Object.freeze({
    _tag: 'Proved' as const,
    goal,
    selection,
    typeArguments: Object.freeze([...typeArguments]),
    requirements: Object.freeze([...requirements]),
  })

const unprovedGoal = (
  goal: ConformanceGoal.ConformanceGoal,
  failure: ConformanceGoal.Failure,
  trace: ReadonlyArray<ConformanceGoal.ConformanceGoal>,
): ConformanceGoal.Proof =>
  Object.freeze({
    _tag: 'Unproved' as const,
    goal,
    failure,
    trace: Object.freeze([...trace]),
  })

const proveGoal = (
  self: Index,
  goal: ConformanceGoal.ConformanceGoal,
  memo: Map<string, ConformanceGoal.Proof>,
  active: ReadonlyArray<ConformanceGoal.ConformanceGoal>,
): ConformanceGoal.Proof => {
  const goalKey = ConformanceGoal.key(goal)
  const completed = memo.get(goalKey)
  if (completed !== undefined) return completed
  // An in-progress goal cannot satisfy itself. Declaration-time descent already proves the search
  // finite, so reaching this means a fact was damaged; the answer recovers the path rather than
  // admitting a coinductive proof, and is deliberately not remembered.
  if (active.some((entry) => ConformanceGoal.key(entry) === goalKey))
    return unprovedGoal(goal, Object.freeze({ _tag: 'ActiveCycle' as const }), active)
  const proof = ((): ConformanceGoal.Proof => {
    if (Type.equals(goal.capability, Type.copyCapability)) {
      const copy = copyProof(self, goal.provider)
      if (copy._tag !== 'Copy')
        return unprovedGoal(
          goal,
          Object.freeze({ _tag: 'UnavailableWitness' as const, reason: copy.reason }),
          active,
        )
      const candidate = conformanceCandidates(self, goal).at(0)
      return candidate === undefined
        ? provedGoal(goal, Object.freeze({ _tag: 'IntrinsicSelection' as const }), [], [])
        : provedGoal(
            goal,
            Object.freeze({
              _tag: 'SourceSelection' as const,
              module: candidate.module,
              ordinal: candidate.conformance.ordinal,
            }),
            candidate.conformance.typeParameters
              .filter((parameter) => parameter.duplicateOf === undefined)
              .map(
                (parameter) =>
                  candidate.substitution.get(Type.key(parameter.type)) ?? parameter.type,
              ),
            [],
          )
    }
    if (Type.isNominal(goal.provider)) {
      if (Type.equals(goal.provider, goal.capability))
        return provedGoal(goal, Object.freeze({ _tag: 'IdentitySelection' as const }), [], [])
    }
    const matching = conformanceCandidates(self, goal)
    const selected = matching.at(0)
    if (matching.length === 0 || selected === undefined)
      return unprovedGoal(goal, Object.freeze({ _tag: 'MissingWitness' as const }), active)
    if (matching.length > 1)
      return unprovedGoal(
        goal,
        Object.freeze({ _tag: 'AmbiguousWitness' as const, candidates: matching.length }),
        active,
      )
    const nested = Object.freeze([...active, goal])
    const requirements: Array<ConformanceGoal.Proof> = []
    for (const requirement of declaredRequirements(self.modules, selected.conformance)) {
      const capability = Type.substitute(requirement.capability, selected.substitution)
      const provider = Type.substitute(requirement.provider, selected.substitution)
      if (!Type.isNominal(capability))
        return unprovedGoal(
          goal,
          Object.freeze({
            _tag: 'UnavailableWitness' as const,
            reason: 'a declared requirement did not resolve to an interface',
          }),
          active,
        )
      const proved = proveGoal(self, ConformanceGoal.make(capability, provider), memo, nested)
      // A failed requirement is reported as itself: the goal that has no witness is the useful
      // one, and the chain that asked for it travels with it.
      if (proved._tag === 'Unproved') return proved
      requirements.push(proved)
    }
    return provedGoal(
      goal,
      Object.freeze({
        _tag: 'SourceSelection' as const,
        module: selected.module,
        ordinal: selected.conformance.ordinal,
      }),
      selected.conformance.typeParameters
        .filter((parameter) => parameter.duplicateOf === undefined)
        .map((parameter) => selected.substitution.get(Type.key(parameter.type)) ?? parameter.type),
      requirements,
    )
  })()
  if (proof._tag === 'Proved') memo.set(goalKey, proof)
  return proof
}

/**
 * Proves one concrete conformance goal, following every declared requirement to a base witness.
 *
 * The search is finite without a fuel budget or a depth limit. Declaration-time validation proved
 * that each requirement names a strictly smaller provider, so the chain of goals reachable from one
 * concrete provider is bounded by that provider's own term size.
 */
export const prove = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
): ConformanceGoal.Proof => {
  const remembered = proofMemos.get(self)
  const memo = remembered ?? new Map<string, ConformanceGoal.Proof>()
  if (remembered === undefined) proofMemos.set(self, memo)
  return proveGoal(self, ConformanceGoal.make(capability, provider), memo, Object.freeze([]))
}

const interfaceConformance = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
): ConformanceFact | undefined => {
  const proof = prove(self, provider, capability)
  if (proof._tag !== 'Proved' || proof.selection._tag !== 'SourceSelection') return undefined
  return selectedConformance(self, proof.selection)
}

const selectedConformance = (
  self: Index,
  selection: Extract<ConformanceGoal.Selection, { readonly _tag: 'SourceSelection' }>,
): ConformanceFact | undefined =>
  self.modules
    .find((module) => module.module === selection.module)
    ?.conformances.find((conformance) => conformance.ordinal === selection.ordinal)

/** Tests whether one nominal provider has a compiler-shipped or source-declared witness. */
export const conforms = (self: Index, provider: Type.Type, capability: Type.Nominal): boolean => {
  if (Type.equals(capability, Type.copyCapability)) {
    return copyType(self, provider)
  }
  if (contractByCapability(self, capability) !== undefined) {
    return prove(self, provider, capability)._tag === 'Proved'
  }
  return witness(self, provider, capability) !== undefined
}

/**
 * Returns, in declaration order, the operations one interface declares that the provider's selected
 * declared conformance leaves unmapped. Rejected declarations remain visible here only to preserve
 * the most specific diagnostic after selection excludes them. An empty result means the declaration
 * covers the whole contract. A capability that is not an interface, or a provider with no single
 * coherent, terminating declaration, has no partial witness to report and returns nothing.
 */
export const unmappedInterfaceOperations = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
): ReadonlyArray<string> => {
  const interface_ = contractByCapability(self, capability)
  const declared = conformanceCandidates(
    self,
    ConformanceGoal.make(capability, provider),
    'Declared',
  )
  const conformance = declared.length === 1 ? declared.at(0)?.conformance : undefined
  if (interface_ === undefined || conformance === undefined) return Object.freeze([])
  const mapped = new Set(
    conformance.operations.flatMap((mapping) =>
      mapping.name._tag === 'Present' ? [mapping.name.spelling] : [],
    ),
  )
  return Object.freeze(
    interface_.operations.flatMap((operation) =>
      operation.name._tag === 'Present' && !mapped.has(operation.name.spelling)
        ? [operation.name.spelling]
        : [],
    ),
  )
}

/**
 * Selects the compiler-known operation one provider's interface conformance maps an operation to.
 *
 * An operator on a bound-typed operand needs no witness: its lowering is width-neutral, and the
 * specialized operand type alone selects the concrete instruction. An operation no operator spells
 * has no such lowering, so its call reads the witness the specialization selected — two providers
 * of one interface may map one operation to two unrelated instructions, and only the conformance
 * says which. A provider with no single conformance, or a mapping that is not a two-segment
 * `Intrinsic` path, selects nothing.
 */
export const interfaceOperationIntrinsic = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
  operation: string,
): Intrinsic.Operation | undefined => {
  const mapping = interfaceConformance(self, provider, capability)?.operations.find(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === operation,
  )
  const target = mapping?.target
  if (
    target?._tag !== 'TypePath' ||
    target.segments.length !== 2 ||
    target.segments.at(0)?.spelling !== 'Intrinsic'
  )
    return undefined
  return Intrinsic.findOperation('Intrinsic', target.segments.at(1)?.spelling ?? '')
}

/**
 * Selects the canonical source function that an interface conformance maps one operation to.
 *
 * Nothing is returned when the mapping names a sealed intrinsic instead. Inline implementations
 * are selected by their conformance identity rather than by their synthesized source spelling, so
 * scalar and nominal providers share the same stable lookup.
 */
export const interfaceWitnessImplementation = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
  operation: string,
): CanonicalId | undefined => {
  const conformance = interfaceConformance(self, provider, capability)
  return conformance === undefined
    ? undefined
    : witnessImplementation(self, provider, conformance, operation)
}

const witnessImplementation = (
  self: Index,
  provider: Type.Type,
  conformance: ConformanceFact,
  operation: string,
): CanonicalId | undefined => {
  const mapping = conformance.operations.find(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === operation,
  )
  if (mapping?.form === 'Inline') {
    const declaration = self.modules
      .find((module) => module.module === conformance.module)
      ?.declarations.find(
        (candidate) =>
          candidate.conformanceImplementation?.ordinal === conformance.ordinal &&
          candidate.conformanceImplementation.operation === operation,
      )
    return declaration?.canonical._tag === 'Canonical' ? declaration.canonical.id : undefined
  }
  if (!Type.isNominal(provider)) return undefined
  const target = mapping?.target
  if (
    target?._tag !== 'TypePath' ||
    target.segments.length !== 2 ||
    target.segments.at(0)?.spelling !== provider.name
  )
    return undefined
  const targetName = target.segments.at(1)?.spelling
  const declaration = self.modules
    .find((module) => module.module === provider.module)
    ?.declarations.find(
      (candidate) =>
        targetName !== undefined &&
        candidate.name._tag === 'Present' &&
        candidate.name.spelling === targetName,
    )
  return declaration?.canonical._tag === 'Canonical' ? declaration.canonical.id : undefined
}

/** One source witness together with the arguments its own specialization needs. */
export interface InterfaceWitnessTarget {
  readonly implementation: CanonicalId
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  /** The concrete provider whose terminating conditional proof selected this target. */
  readonly structuralProvider?: Type.Type
}

const inferredTargetArguments = (
  conformance: ConformanceFact,
  mapping: ConformanceFact['operations'][number],
  proof: Extract<ConformanceGoal.Proof, { readonly _tag: 'Proved' }>,
): ReadonlyArray<Type.GenericArgument> | undefined => {
  if (mapping.targetArguments === undefined) return undefined
  const headerParameters = conformance.typeParameters
    .filter((parameter) => parameter.duplicateOf === undefined)
    .map((parameter) => parameter.type)
  const headerSubstitution = TypeInference.substitution(headerParameters, proof.typeArguments)
  if (headerSubstitution === undefined) return undefined
  const arguments_ = Object.freeze(
    mapping.targetArguments.map((argument) =>
      Type.substituteGenericArgument(argument, headerSubstitution),
    ),
  )
  return arguments_.every(Type.isRuntimeConcreteGenericArgument) ? arguments_ : undefined
}

/**
 * Selects the provider's own function one interface operation maps to, with its type arguments.
 *
 * A conditional conformance's witness is generic in the header's binders, so naming the function is
 * not enough to reach code: the call needs the arguments this specialization bound. They come from
 * the proof rather than from the call site, because the proof is what decided which header covers
 * this provider.
 */
export const interfaceWitnessTarget = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
  operation: string,
): InterfaceWitnessTarget | undefined => {
  const proof = prove(self, provider, capability)
  if (proof._tag !== 'Proved' || proof.selection._tag !== 'SourceSelection') return undefined
  const conformance = selectedConformance(self, proof.selection)
  if (conformance === undefined) return undefined
  const mapping = conformance.operations.find(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === operation,
  )
  if (mapping === undefined) return undefined
  const implementation = witnessImplementation(self, provider, conformance, operation)
  const typeArguments = inferredTargetArguments(conformance, mapping, proof)
  if (implementation === undefined || typeArguments === undefined) return undefined
  return Object.freeze({
    implementation,
    typeArguments,
    ...(proof.requirements.length > 0 ? { structuralProvider: provider } : {}),
  })
}

/**
 * Selects every source witness implementation a proved conditional witness rests on.
 *
 * The result is innermost first, deduplicated by concrete implementation specialization, and does
 * not include the root witness. Discovery consumes it independently of the selected operation's
 * body: a declared requirement is part of admitting the witness even when that operation never
 * invokes the required interface itself.
 */
export const witnessDependencyTargets = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
): ReadonlyArray<InterfaceWitnessTarget> => {
  const proof = prove(self, provider, capability)
  if (proof._tag !== 'Proved') return Object.freeze([])
  const found = new Map<string, InterfaceWitnessTarget>()
  const visit = (dependency: ConformanceGoal.Proof): void => {
    if (dependency._tag !== 'Proved') return
    for (const requirement of dependency.requirements) visit(requirement)
    if (dependency.selection._tag !== 'SourceSelection') return
    const conformance = selectedConformance(self, dependency.selection)
    if (conformance === undefined) return
    for (const mapping of conformance.operations) {
      if (mapping.name._tag !== 'Present') continue
      const implementation = witnessImplementation(
        self,
        dependency.goal.provider,
        conformance,
        mapping.name.spelling,
      )
      const typeArguments = inferredTargetArguments(conformance, mapping, dependency)
      if (implementation === undefined || typeArguments === undefined) continue
      const identity = Specialization.key({
        declaration: implementation,
        typeArguments,
      })
      if (!found.has(identity))
        found.set(
          identity,
          Object.freeze({
            implementation,
            typeArguments,
            structuralProvider: dependency.goal.provider,
          }),
        )
    }
  }
  for (const requirement of proof.requirements) visit(requirement)
  return Object.freeze([...found.values()])
}

/** Selects the unique compiler-shipped or source-declared witness for one provider. */
export const witness = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
): ConformanceWitness | undefined => {
  if (!Type.isNominal(provider)) return undefined
  if (Type.equals(provider, capability)) {
    return Object.freeze({ _tag: 'IdentityConformanceWitness', capability, provider })
  }
  // Proof selection is the single authority for matching both the provider and capability heads.
  // Repeating only provider inference here would lose capability binders and could select a header
  // whose requirements failed.
  const proof = prove(self, provider, capability)
  if (proof._tag !== 'Proved' || proof.selection._tag !== 'SourceSelection') return undefined
  const conformance = selectedConformance(self, proof.selection)
  if (conformance === undefined) return undefined
  const member = memberByNominal(self.modules, capability)
  const contract =
    member?._tag === 'InterfaceDeclaration' || member?._tag === 'ServiceDeclaration'
      ? member
      : undefined
  const mappedNames = new Set(
    conformance.operations.flatMap((mapping) =>
      mapping.name._tag === 'Present' ? [mapping.name.spelling] : [],
    ),
  )
  const completeContract =
    contract !== undefined &&
    conformance.hook === undefined &&
    mappedNames.size === conformance.operations.length &&
    contract.operations.every(
      (operation) => operation.name._tag === 'Present' && mappedNames.has(operation.name.spelling),
    ) &&
    conformance.operations.length === contract.operations.length
  let operations: readonly Readonly<{ name: string; implementation: CanonicalId }>[]
  if (contract === undefined) {
    operations = Object.freeze([])
  } else {
    operations = Object.freeze(
      contract.operations.flatMap((operation) => {
        const name = operation.name._tag === 'Present' ? operation.name.spelling : undefined
        const implementation =
          name === undefined ? undefined : witnessImplementation(self, provider, conformance, name)
        return name === undefined || implementation === undefined
          ? []
          : [
              Object.freeze({
                name,
                implementation,
              }),
            ]
      }),
    )
  }
  const completeOperationSet =
    contract === undefined || operations.length === contract.operations.length
  return Type.equals(capability, Type.dropCapability) || (completeContract && completeOperationSet)
    ? Object.freeze({
        _tag: 'SourceConformanceWitness',
        module: conformance.module,
        ordinal: conformance.ordinal,
        capability,
        provider,
        operations,
        typeArguments: proof.typeArguments,
      })
    : undefined
}

/** Adapts declaration-owned witnesses into the neutral constraint-solver result domain. */
export const providerMatch = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
): Constraint.ConformanceOutcome => {
  const proof = prove(self, provider, capability)
  if (proof._tag === 'Unproved') {
    if (proof.failure._tag === 'MissingWitness') return Object.freeze({ _tag: 'NoMatch' })
    if (proof.failure._tag === 'AmbiguousWitness') {
      const goal = ConformanceGoal.make(capability, provider)
      return Object.freeze({
        _tag: 'Ambiguous',
        witnesses: Object.freeze(
          conformanceCandidates(self, goal).map((candidate) =>
            Object.freeze({
              origin: Object.freeze({
                _tag: 'SourceWitness' as const,
                declaration: Object.freeze({
                  module: candidate.module,
                  name: `conformance#${candidate.conformance.ordinal}`,
                }),
              }),
              typeArguments: Object.freeze(
                candidate.conformance.typeParameters
                  .filter((parameter) => parameter.duplicateOf === undefined)
                  .map(
                    (parameter) =>
                      candidate.substitution.get(Type.key(parameter.type)) ?? parameter.type,
                  ),
              ),
            }),
          ),
        ),
      })
    }
    return Object.freeze({
      _tag: 'Invalid',
      reason:
        proof.failure._tag === 'UnavailableWitness'
          ? proof.failure.reason
          : 'conformance selection reached an active cycle',
    })
  }
  const selected = witness(self, provider, capability)
  if (selected === undefined)
    return Object.freeze({
      _tag: 'Invalid',
      reason: 'the selected conformance does not provide a complete service implementation',
    })
  if (selected._tag === 'IdentityConformanceWitness')
    return Object.freeze({
      _tag: 'Unique',
      match: Object.freeze({ _tag: 'Identity' }),
    })
  if (selected._tag === 'IntrinsicConformanceWitness')
    return Object.freeze({
      _tag: 'Unique',
      match: Object.freeze({
        _tag: 'Conformance',
        witness: Object.freeze({
          origin: Object.freeze({
            _tag: 'IntrinsicWitness',
            operation: `${Type.key(selected.provider)}=>${Type.key(selected.capability)}`,
          }),
          typeArguments: Object.freeze([]),
        }),
      }),
    })
  return Object.freeze({
    _tag: 'Unique',
    match: Object.freeze({
      _tag: 'Conformance',
      witness: Object.freeze({
        origin: Object.freeze({
          _tag: 'SourceWitness',
          declaration: Object.freeze({
            module: selected.module,
            name: `conformance#${selected.ordinal}`,
          }),
        }),
        typeArguments: selected.typeArguments,
      }),
    }),
  })
}

/** Selects one mapped source implementation from a declaration-shaped witness. */
export const witnessOperation = (
  self: Extract<ConformanceWitness, { readonly _tag: 'SourceConformanceWitness' }>,
  name: string,
): CanonicalId | undefined =>
  self.operations.find((operation) => operation.name === name)?.implementation
