/**
 * The alpha-normalized head of one conformance declaration, together with the two declaration-time
 * questions a conditional conformance has to answer before any proof is attempted: whether two
 * heads may ever name one provider, and whether following a declaration's requirements can only
 * descend.
 *
 * A head is the pair the source writes as `impl<...> Capability<...> for Provider`. Its identity is
 * alpha-invariant: binder spelling and binder declaration order are provenance, so the normalized
 * form renumbers every parameter by first occurrence in the canonical term. Two declarations that
 * differ only in how they spell or order their binders therefore share one key.
 *
 * Overlap is decided here without consulting a single bound. A head that mentions `S: Decoder<S>`
 * and one that mentions `S: Encoder<S>` cover the same providers as far as coherence is concerned,
 * because whether a bound is satisfiable is a property of the whole program and may change as
 * declarations are added. Deciding coherence on shapes alone keeps it stable, and the answer is
 * deliberately biased toward reporting overlap: a shape this module cannot decompose is treated as
 * overlapping, so a false rejection is preferred to a coherence hole.
 *
 * Termination is likewise structural. Every requirement must name a provider that is a strict
 * structural subterm of the head's provider, may not multiply any binder's occurrences, and may not
 * rewrite a ground argument the interface already fixed. Those three conditions make the provider
 * term a well-founded measure, so proof search needs no fuel and no depth limit.
 */
import * as Type from './Type.js'

/** The canonical owner every normalized head parameter is renumbered under. */
const normalOwner = Object.freeze({ module: '', name: 'impl' })

/** The distinct owner the right side of an overlap question is renumbered under. */
const opposingOwner = Object.freeze({ module: '', name: 'impl~' })

/** One interface application a conditional conformance must prove before it admits a witness. */
export interface Requirement {
  readonly capability: Type.Nominal
  readonly provider: Type.Type
}

/**
 * One conformance head reduced to its alpha-invariant canonical form.
 *
 * The requirements are renumbered under the same substitution as the head, so a requirement's
 * provider is a term in the head's own vocabulary and the descent check can compare the two
 * directly. Identity ignores them: `key` reads the head alone, because coherence never consults a
 * bound.
 */
export interface ConformanceHead {
  readonly _tag: 'ConformanceHead'
  readonly capability: Type.Nominal
  readonly provider: Type.Type
  readonly parameters: ReadonlyArray<Type.Parameter>
  readonly requirements: ReadonlyArray<Requirement>
}

/** Why following one declared requirement could fail to descend toward a base witness. */
export type TerminationFailure =
  | {
      readonly _tag: 'ProviderNotStrictSubterm'
      readonly requirement: string
      readonly required: string
      readonly provider: string
    }
  | {
      readonly _tag: 'IncreasingVariableOccurrences'
      readonly requirement: string
      readonly variable: string
      readonly declared: number
      readonly required: number
    }
  | {
      readonly _tag: 'ChangedGroundArgument'
      readonly requirement: string
      readonly ordinal: number
      readonly declared: string
      readonly required: string
    }

/**
 * Returns every parameter reachable from the supplied terms in first-occurrence order.
 *
 * `Type.parameters` answers the same question as a canonically sorted set, which is the right
 * answer for identity comparisons but the wrong one for renumbering: normalizing by sorted order
 * would make the normal form depend on parameter spelling, which is exactly the provenance the
 * normal form exists to discard.
 */
const occurringParameters = (terms: ReadonlyArray<Type.Type>): ReadonlyArray<Type.Parameter> => {
  const found = new Map<string, Type.Parameter>()
  const remember = (parameter: Type.Parameter): void => {
    const key = Type.key(parameter)
    if (!found.has(key)) found.set(key, parameter)
  }
  const visitArgument = (argument: Type.GenericArgument): void => {
    if (Type.isTypeArgument(argument)) visit(argument)
    else if (Type.isRepresentationParameterArgument(argument)) remember(argument.parameter)
    else if (Type.isExactRepresentationArgument(argument)) {
      visit(argument.contract)
      visitArgument(argument.identity)
    } else if (Type.isCallableIdentityArgument(argument))
      for (const typeArgument of argument.typeArguments) visitArgument(typeArgument)
    else if (Type.isFailureRowArgument(argument)) {
      for (const failure of argument.failures) visit(failure)
      for (const parameter of argument.parameters) remember(parameter)
    } else if (Type.isRequirementRowArgument(argument)) {
      for (const requirement of argument.requirements) visit(requirement.capability)
      for (const parameter of argument.parameters) remember(parameter)
    }
  }
  const visit = (type: Type.Type): void => {
    if (Type.isParameter(type)) {
      remember(type)
      return
    }
    if (Type.isFailureProjection(type)) {
      remember(type.parameter)
      return
    }
    if (Type.isNominal(type)) {
      for (const argument of type.arguments) visitArgument(argument)
      return
    }
    if (Type.isFixedArray(type) || Type.isSlice(type)) visit(type.element)
    else if (Type.isReference(type)) visit(type.target)
    else if (Type.isCallable(type)) {
      for (const parameter of type.parameters) visit(parameter)
      visit(type.result)
    } else if (Type.isEffect(type)) {
      visit(type.success)
      for (const failure of type.failures) visit(failure)
      for (const parameter of type.failureParameters) visit(parameter)
      for (const requirement of type.requirements) visit(requirement.capability)
      for (const parameter of type.requirementParameters) visit(parameter)
    } else if (Type.isRepresented(type)) {
      visit(type.contract)
      visitArgument(type.representation.argument)
    } else if (Type.isUnion(type)) for (const member of type.members) visit(member)
  }
  for (const term of terms) visit(term)
  return Object.freeze([...found.values()])
}

/** Builds the renumbering that maps each occurring parameter onto its canonical position. */
const normalization = (
  parameters: ReadonlyArray<Type.Parameter>,
  owner: { readonly module: string; readonly name: string },
): {
  readonly substitution: Type.Substitution
  readonly parameters: ReadonlyArray<Type.Parameter>
} => {
  const normalized = parameters.map((parameter, position) =>
    Type.parameter(owner, position, `%${position}`, parameter.kind, parameter.representationBound),
  )
  return Object.freeze({
    substitution: new Map(
      parameters.map((parameter, position) => {
        const replacement = normalized.at(position)
        if (replacement === undefined) throw new RangeError('Head normalization lost a parameter')
        return [Type.key(parameter), replacement] as const
      }),
    ),
    parameters: Object.freeze(normalized),
  })
}

/** Renumbers one head's binders by first occurrence so alpha-equivalent heads share one identity. */
export const make = (
  capability: Type.Nominal,
  provider: Type.Type,
  requirements: ReadonlyArray<Requirement> = [],
): ConformanceHead => makeUnder(capability, provider, requirements, normalOwner)

const makeUnder = (
  capability: Type.Nominal,
  provider: Type.Type,
  requirements: ReadonlyArray<Requirement>,
  owner: { readonly module: string; readonly name: string },
): ConformanceHead => {
  // The head's own terms are walked first, so a parameter that only a requirement mentions cannot
  // shift the numbering of one the head names — which is what keeps identity independent of bounds.
  const renumbered = normalization(
    occurringParameters([
      capability,
      provider,
      ...requirements.flatMap((requirement) => [requirement.capability, requirement.provider]),
    ]),
    owner,
  )
  const normalizedCapability = Type.substitute(capability, renumbered.substitution)
  if (!Type.isNominal(normalizedCapability))
    throw new RangeError('Head normalization changed the capability kind')
  return Object.freeze({
    _tag: 'ConformanceHead' as const,
    capability: normalizedCapability,
    provider: Type.substitute(provider, renumbered.substitution),
    parameters: renumbered.parameters,
    requirements: Object.freeze(
      requirements.flatMap((requirement): ReadonlyArray<Requirement> => {
        const normalized = Type.substitute(requirement.capability, renumbered.substitution)
        if (!Type.isNominal(normalized)) return []
        return Object.freeze([
          Object.freeze({
            capability: normalized,
            provider: Type.substitute(requirement.provider, renumbered.substitution),
          }),
        ])
      }),
    ),
  })
}

/** The canonical alpha-invariant identity of one head. */
export const key = (self: ConformanceHead): string =>
  `${Type.key(self.capability)} ${Type.key(self.provider)}`

/** Renders one head the way a diagnostic spells it. */
export const encode = (self: ConformanceHead): string =>
  `${Type.encode(self.capability)} for ${Type.encode(self.provider)}`

const resolve = (type: Type.Type, bindings: ReadonlyMap<string, Type.Type>): Type.Type => {
  let current = type
  const seen = new Set<string>()
  while (Type.isParameter(current)) {
    const parameterKey = Type.key(current)
    if (seen.has(parameterKey)) return current
    seen.add(parameterKey)
    const bound = bindings.get(parameterKey)
    if (bound === undefined) return current
    current = bound
  }
  return current
}

const occurs = (parameter: Type.Parameter, type: Type.Type): boolean =>
  Type.parameters(type).some((candidate) => Type.key(candidate) === Type.key(parameter))

/**
 * Binds one open parameter, refusing a binding that would only be satisfied by an infinite term.
 *
 * The occurs check is what makes `P<A>` and `A` disjoint rather than accidentally unifiable, which
 * is the difference between rejecting a wrapper that overlaps its own argument and admitting one.
 */
const bind = (
  parameter: Type.Parameter,
  type: Type.Type,
  bindings: Map<string, Type.Type>,
): boolean => {
  if (occurs(parameter, type)) return false
  if (parameter.kind === 'CallableRepresentation' || parameter.kind === 'EffectRepresentation')
    return true
  bindings.set(Type.key(parameter), type)
  return true
}

/**
 * Reports whether two representation arguments could name one concrete representation.
 *
 * A representation parameter admits every representation its bound admits, so two parameters
 * overlap whenever their bounds intersect and a parameter overlaps an exact argument whenever that
 * argument's contract is admissible at the parameter's bound. `#187` owns both answers.
 */
const representationsMayOverlap = (
  left: Type.RepresentationArgument,
  right: Type.RepresentationArgument,
): boolean => {
  if (Type.representationArgumentKind(left) !== Type.representationArgumentKind(right)) return false
  if (Type.isExactRepresentationArgument(left) && Type.isExactRepresentationArgument(right))
    return Type.genericArgumentKey(left) === Type.genericArgumentKey(right)
  if (
    Type.isRepresentationParameterArgument(left) &&
    Type.isRepresentationParameterArgument(right)
  ) {
    const leftBound = left.parameter.representationBound
    const rightBound = right.parameter.representationBound
    if (leftBound === undefined || rightBound === undefined) return true
    return Type.intersectRepresentationBounds(leftBound, rightBound) !== undefined
  }
  const open = Type.isRepresentationParameterArgument(left) ? left : right
  const exact = Type.isExactRepresentationArgument(left) ? left : right
  if (!Type.isRepresentationParameterArgument(open) || !Type.isExactRepresentationArgument(exact))
    return true
  const bound = open.parameter.representationBound
  if (bound === undefined) return true
  return Type.representationAdmissibility(exact.contract, bound)._tag !== 'Unavailable'
}

const unifyArgument = (
  left: Type.GenericArgument,
  right: Type.GenericArgument,
  bindings: Map<string, Type.Type>,
): boolean => {
  // A damaged argument records that a fact was withheld, never that two heads are disjoint.
  if (Type.isUnavailableGenericArgument(left) || Type.isUnavailableGenericArgument(right))
    return true
  if (Type.isTypeArgument(left) && Type.isTypeArgument(right)) return unify(left, right, bindings)
  if (Type.isRepresentationArgument(left) && Type.isRepresentationArgument(right))
    return representationsMayOverlap(left, right)
  if (Type.isFailureRowArgument(left) && Type.isFailureRowArgument(right)) {
    // An open row stands for every extension of itself, so only two closed rows can be disjoint.
    if (left.parameters.length > 0 || right.parameters.length > 0) return true
    return Type.genericArgumentKey(left) === Type.genericArgumentKey(right)
  }
  if (Type.isRequirementRowArgument(left) && Type.isRequirementRowArgument(right)) {
    if (left.parameters.length > 0 || right.parameters.length > 0) return true
    return Type.genericArgumentKey(left) === Type.genericArgumentKey(right)
  }
  if (Type.isHiddenIdentityArgument(left) && Type.isHiddenIdentityArgument(right))
    return Type.genericArgumentKey(left) === Type.genericArgumentKey(right)
  return false
}

const unify = (
  leftTerm: Type.Type,
  rightTerm: Type.Type,
  bindings: Map<string, Type.Type>,
): boolean => {
  const left = resolve(leftTerm, bindings)
  const right = resolve(rightTerm, bindings)
  if (Type.equals(left, right)) return true
  if (Type.isParameter(left) && Type.isParameter(right)) {
    if (left.kind !== right.kind) return false
    return bind(left, right, bindings)
  }
  if (Type.isParameter(left)) return left.kind === 'Value' && bind(left, right, bindings)
  if (Type.isParameter(right)) return right.kind === 'Value' && bind(right, left, bindings)
  if (Type.isNominal(left) && Type.isNominal(right)) {
    if (
      left.module !== right.module ||
      left.name !== right.name ||
      left.arguments.length !== right.arguments.length
    )
      return false
    return left.arguments.every((argument, ordinal) => {
      const opposing = right.arguments.at(ordinal)
      return opposing !== undefined && unifyArgument(argument, opposing, bindings)
    })
  }
  if (Type.isFixedArray(left) && Type.isFixedArray(right))
    return left.length === right.length && unify(left.element, right.element, bindings)
  if (Type.isSlice(left) && Type.isSlice(right))
    return left.access === right.access && unify(left.element, right.element, bindings)
  if (Type.isReference(left) && Type.isReference(right))
    return left.access === right.access && unify(left.target, right.target, bindings)
  if (Type.isCallable(left) && Type.isCallable(right))
    return (
      left.mode === right.mode &&
      left.parameters.length === right.parameters.length &&
      left.parameters.every((parameter, ordinal) => {
        const opposing = right.parameters.at(ordinal)
        return opposing !== undefined && unify(parameter, opposing, bindings)
      }) &&
      unify(left.result, right.result, bindings)
    )
  if (Type.isRepresented(left) && Type.isRepresented(right))
    return representationsMayOverlap(left.representation.argument, right.representation.argument)
  // An Effect carries two open rows and a union has no canonical decomposition into cases, so
  // neither shape can be shown disjoint here. Both report overlap rather than a coherence hole.
  if (Type.isEffect(left) && Type.isEffect(right)) return true
  if (Type.isUnion(left) || Type.isUnion(right)) return true
  return false
}

/**
 * Reports whether two heads may ever name one provider under one interface.
 *
 * Bounds are deliberately not consulted. Two heads whose requirements are mutually exclusive today
 * may both become provable when a later declaration adds a conformance, so a coherence answer that
 * read them would not be stable across an evolving program.
 */
export const mayOverlap = (left: ConformanceHead, right: ConformanceHead): boolean => {
  const opposing = makeUnder(right.capability, right.provider, [], opposingOwner)
  const bindings = new Map<string, Type.Type>()
  return (
    unify(left.capability, opposing.capability, bindings) &&
    unify(left.provider, opposing.provider, bindings)
  )
}

/** Counts every occurrence of each parameter, with multiplicity, across the supplied terms. */
const occurrences = (terms: ReadonlyArray<Type.Type>): ReadonlyMap<string, number> => {
  const counts = new Map<string, number>()
  for (const term of terms)
    for (const parameter of occurringParameters([term])) {
      const parameterKey = Type.key(parameter)
      counts.set(parameterKey, (counts.get(parameterKey) ?? 0) + countIn(parameter, term))
    }
  return counts
}

const countIn = (parameter: Type.Parameter, term: Type.Type): number => {
  const target = Type.key(parameter)
  const countArgument = (argument: Type.GenericArgument): number => {
    if (Type.isTypeArgument(argument)) return count(argument)
    if (Type.isRepresentationParameterArgument(argument))
      return Type.key(argument.parameter) === target ? 1 : 0
    if (Type.isExactRepresentationArgument(argument))
      return count(argument.contract) + countArgument(argument.identity)
    if (Type.isCallableIdentityArgument(argument))
      return argument.typeArguments.reduce((total, entry) => total + countArgument(entry), 0)
    if (Type.isFailureRowArgument(argument))
      return (
        argument.failures.reduce((total, failure) => total + count(failure), 0) +
        argument.parameters.filter((entry) => Type.key(entry) === target).length
      )
    if (Type.isRequirementRowArgument(argument))
      return (
        argument.requirements.reduce((total, entry) => total + count(entry.capability), 0) +
        argument.parameters.filter((entry) => Type.key(entry) === target).length
      )
    return 0
  }
  const count = (type: Type.Type): number => {
    if (Type.isParameter(type)) return Type.key(type) === target ? 1 : 0
    if (Type.isFailureProjection(type)) return Type.key(type.parameter) === target ? 1 : 0
    if (Type.isNominal(type))
      return type.arguments.reduce((total, argument) => total + countArgument(argument), 0)
    if (Type.isFixedArray(type) || Type.isSlice(type)) return count(type.element)
    if (Type.isReference(type)) return count(type.target)
    if (Type.isCallable(type))
      return type.parameters.reduce((total, entry) => total + count(entry), 0) + count(type.result)
    if (Type.isEffect(type))
      return (
        count(type.success) +
        type.failures.reduce((total, entry) => total + count(entry), 0) +
        type.failureParameters.reduce((total, entry) => total + count(entry), 0) +
        type.requirements.reduce((total, entry) => total + count(entry.capability), 0) +
        type.requirementParameters.reduce((total, entry) => total + count(entry), 0)
      )
    if (Type.isRepresented(type))
      return count(type.contract) + countArgument(type.representation.argument)
    if (Type.isUnion(type)) return type.members.reduce((total, member) => total + count(member), 0)
    return 0
  }
  return count(term)
}

/** Reports whether one term occurs strictly inside another. */
export const isStrictSubterm = (candidate: Type.Type, whole: Type.Type): boolean => {
  if (Type.equals(candidate, whole)) return false
  return containsSubterm(candidate, whole)
}

const containsSubterm = (candidate: Type.Type, whole: Type.Type): boolean => {
  if (Type.equals(candidate, whole)) return true
  const inArgument = (argument: Type.GenericArgument): boolean =>
    Type.isTypeArgument(argument)
      ? containsSubterm(candidate, argument)
      : Type.isExactRepresentationArgument(argument)
        ? containsSubterm(candidate, argument.contract)
        : Type.isFailureRowArgument(argument)
          ? argument.failures.some((failure) => containsSubterm(candidate, failure))
          : Type.isRequirementRowArgument(argument)
            ? argument.requirements.some((entry) => containsSubterm(candidate, entry.capability))
            : false
  if (Type.isNominal(whole)) return whole.arguments.some(inArgument)
  if (Type.isFixedArray(whole) || Type.isSlice(whole))
    return containsSubterm(candidate, whole.element)
  if (Type.isReference(whole)) return containsSubterm(candidate, whole.target)
  if (Type.isCallable(whole))
    return (
      whole.parameters.some((parameter) => containsSubterm(candidate, parameter)) ||
      containsSubterm(candidate, whole.result)
    )
  if (Type.isEffect(whole))
    return (
      containsSubterm(candidate, whole.success) ||
      whole.failures.some((failure) => containsSubterm(candidate, failure)) ||
      whole.requirements.some((entry) => containsSubterm(candidate, entry.capability))
    )
  if (Type.isRepresented(whole)) return containsSubterm(candidate, whole.contract)
  if (Type.isUnion(whole)) return whole.members.some((member) => containsSubterm(candidate, member))
  return false
}

/**
 * Reports every way one declaration's requirements could fail to descend toward a base witness.
 *
 * The three conditions together make the head's provider term a well-founded measure. Strict
 * subterm descent bounds the depth of any proof by the size of the concrete provider it starts
 * from; non-increasing occurrences stop a requirement from trading depth for width; and holding
 * ground interface arguments fixed stops a requirement from walking an interface's non-provider
 * arguments while the provider stands still. An empty result is the termination proof itself, so no
 * fuel, depth limit, or configurable budget is needed anywhere downstream.
 */
export const terminationFailures = (self: ConformanceHead): ReadonlyArray<TerminationFailure> => {
  const declaredOccurrences = occurrences([self.capability, self.provider])
  return Object.freeze(
    self.requirements.flatMap((requirement): ReadonlyArray<TerminationFailure> => {
      const spelling = `${Type.encode(requirement.capability)} for ${Type.encode(requirement.provider)}`
      const failures: Array<TerminationFailure> = []
      if (!isStrictSubterm(requirement.provider, self.provider))
        failures.push(
          Object.freeze({
            _tag: 'ProviderNotStrictSubterm' as const,
            requirement: spelling,
            required: Type.encode(requirement.provider),
            provider: Type.encode(self.provider),
          }),
        )
      const requiredOccurrences = occurrences([requirement.capability, requirement.provider])
      for (const [parameter, required] of requiredOccurrences) {
        const declared = declaredOccurrences.get(parameter) ?? 0
        if (required > declared)
          failures.push(
            Object.freeze({
              _tag: 'IncreasingVariableOccurrences' as const,
              requirement: spelling,
              variable: parameter,
              declared,
              required,
            }),
          )
      }
      // Only the same interface has positional correspondence to compare. A requirement naming a
      // different interface still descends, because its provider is a strict subterm.
      if (
        requirement.capability.module === self.capability.module &&
        requirement.capability.name === self.capability.name &&
        requirement.capability.arguments.length === self.capability.arguments.length
      )
        for (const [ordinal, declared] of self.capability.arguments.entries()) {
          if (ordinal === providerOrdinal) continue
          const required = requirement.capability.arguments.at(ordinal)
          if (required === undefined) continue
          if (!Type.isConcreteGenericArgument(declared)) continue
          if (Type.genericArgumentKey(declared) === Type.genericArgumentKey(required)) continue
          failures.push(
            Object.freeze({
              _tag: 'ChangedGroundArgument' as const,
              requirement: spelling,
              ordinal,
              declared: Type.encodeGenericArgument(declared),
              required: Type.encodeGenericArgument(required),
            }),
          )
        }
      return Object.freeze(failures)
    }),
  )
}

/** The interface argument position every conformance spells its own provider in. */
export const providerOrdinal = 0

/** Renders one termination failure as the sentence a diagnostic reports. */
export const describeTermination = (self: TerminationFailure): string => {
  switch (self._tag) {
    case 'ProviderNotStrictSubterm':
      return `requirement ${self.requirement} does not descend: ${self.required} is not a strict subterm of ${self.provider}`
    case 'IncreasingVariableOccurrences':
      return `requirement ${self.requirement} repeats ${self.variable} ${self.required} times where the header uses it ${self.declared}`
    case 'ChangedGroundArgument':
      return `requirement ${self.requirement} rewrites ground argument ${self.ordinal} from ${self.declared} to ${self.required}`
  }
}
