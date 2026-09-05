import * as Lifetime from '../Lifetime.js'
import * as TypeCompatibility from '../TypeCompatibility.js'
import * as RowAlgebra from '../RowAlgebra.js'
import type {
  Callable,
  Effect,
  FailureRow,
  GenericArgument,
  Parameter,
  RepresentationArgument,
  RepresentationBound,
  RequirementRowArgument,
  RequirementsRow,
  RowInferenceFailure,
  SealedStaticProperty,
  Substitution,
  TypeOutlives,
  Type,
} from '../Type.js'
import {
  callable,
  compareAccess,
  effectWithRows,
  executableFormationRequirements,
  lifetimes,
  encode,
  equals,
  failureMemberParameters,
  failureMembers,
  failureRowPolicy,
  genericArgumentKey,
  isCallable,
  isEffect,
  isFixedArray,
  isNever,
  isNominal,
  isParameter,
  isPointer,
  isReference,
  isRepresentationArgument,
  isRepresentationParameterArgument,
  isRepresented,
  isRequirementRowArgument,
  isSlice,
  isString,
  isTypeArgument,
  key,
  representationAdmissibility,
  representationArgumentKind,
  requirementMembers,
  requirementRowArgument,
  requirementRowArgumentFromRow,
  requirementRowParameters,
  requirementRowPolicy,
  requirementSatisfies,
  someSubterm,
  satisfiesOutlives,
  substitute,
  substituteFailureRow,
  substituteLifetime,
  substituteRequirementsRow,
  union,
} from '../Type.js'

const representationArgumentContract = (
  self: RepresentationArgument,
): RepresentationBound | undefined =>
  self._tag === 'RepresentationParameterArgument'
    ? self.parameter.representationBound
    : self.contract

export interface GenericArgumentConflict {
  readonly parameter: Parameter
  readonly previous: GenericArgument
  readonly conflicting: GenericArgument
}

export interface OpenGenericInference {
  readonly matches: boolean
  readonly conflicts: ReadonlyArray<GenericArgumentConflict>
}

export interface LifetimeInference {
  readonly compatibility?: TypeCompatibility.Context
  /** Declaration-owned lifetime variables still open at this inference boundary. */
  readonly inferable?: ReadonlySet<string>
  readonly typeOutlives?: (type: Type, lifetime: Lifetime.Lifetime) => boolean
  /** Checks one selected source-to-expected region relation; invariant positions require both directions. */
  readonly accepts: (
    source: Lifetime.Lifetime,
    target: Lifetime.Lifetime,
    invariant: boolean,
  ) => boolean
}

interface InferenceContext {
  readonly lifetimes?: LifetimeInference | undefined
  readonly invariant?: boolean
  readonly contravariant?: boolean
  readonly allowOpenGenericArguments: boolean
  readonly conflicts?: Array<GenericArgumentConflict>
}

const commitTrial = <A>(
  context: InferenceContext,
  evaluate: () => A,
  accepted: (result: A) => boolean,
): A =>
  context.lifetimes?.compatibility === undefined
    ? evaluate()
    : TypeCompatibility.commitWhen(context.lifetimes.compatibility, evaluate, accepted)

/** Adds structural constraints from one declared type pattern to one supplied concrete type. */
const bindGenericArgument = (
  parameter_: Parameter,
  actual: GenericArgument,
  inferred: Map<string, GenericArgument>,
  context: InferenceContext,
): boolean => {
  const identity = key(parameter_)
  const existing = inferred.get(identity)
  if (existing === undefined) {
    inferred.set(identity, actual)
    return true
  }
  if (genericArgumentKey(existing) === genericArgumentKey(actual)) return true
  context.conflicts?.push(
    Object.freeze({ parameter: parameter_, previous: existing, conflicting: actual }),
  )
  return false
}

const commitInference = (
  target: Map<string, GenericArgument>,
  source: ReadonlyMap<string, GenericArgument>,
): void => {
  target.clear()
  for (const [identity, argument] of source) target.set(identity, argument)
}

/** Finds the first deterministic complete matching of normalized row members. */
const inferRowMembers = <Member>(
  pattern: ReadonlyArray<Member>,
  actual: ReadonlyArray<Member>,
  inferred: ReadonlyMap<string, GenericArgument>,
  matches: (pattern: Member, actual: Member, inferred: Map<string, GenericArgument>) => boolean,
  complete: (remaining: ReadonlyArray<Member>, inferred: Map<string, GenericArgument>) => boolean,
  context: InferenceContext,
): ReadonlyMap<string, GenericArgument> | undefined => {
  const search = (
    position: number,
    remaining: ReadonlyArray<Member>,
    current: ReadonlyMap<string, GenericArgument>,
  ): ReadonlyMap<string, GenericArgument> | undefined => {
    const member = pattern.at(position)
    if (member === undefined) {
      const completed = new Map(current)
      return complete(remaining, completed) ? completed : undefined
    }
    for (const [candidatePosition, candidate] of remaining.entries()) {
      const found = commitTrial(
        context,
        () => {
          const trial = new Map(current)
          if (!matches(member, candidate, trial)) return undefined
          return search(
            position + 1,
            remaining.filter((_, index) => index !== candidatePosition),
            trial,
          )
        },
        (result) => result !== undefined,
      )
      if (found !== undefined) return found
    }
    return undefined
  }
  return search(0, actual, inferred)
}

/** Infers one normalized internal failure expression through ordinary type parameters. */
const inferFailureRow = (
  pattern: FailureRow,
  actual: FailureRow,
  inferred: Map<string, GenericArgument>,
  allowOpenActual: boolean,
  context: InferenceContext,
): boolean => {
  if (!allowOpenActual && RowAlgebra.concretize(failureRowPolicy(), actual)._tag !== 'Concrete')
    return false
  if (RowAlgebra.key(failureRowPolicy(), pattern) === RowAlgebra.key(failureRowPolicy(), actual))
    return true
  const substitutedPattern = substituteFailureRow(pattern, inferred)
  if (
    RowAlgebra.key(failureRowPolicy(), substitutedPattern) ===
    RowAlgebra.key(failureRowPolicy(), actual)
  )
    return true
  if (pattern.expression._tag === 'Singleton') {
    if (actual.expression._tag === 'Singleton')
      return bindGenericArgument(
        pattern.expression.member.parameter,
        actual.expression.member.parameter,
        inferred,
        context,
      )
    const concrete = RowAlgebra.concretize(failureRowPolicy(), actual)
    if (concrete._tag !== 'Concrete') return false
    const normalized = union(concrete.row.members)
    if (normalized._tag !== 'Normalized') return false
    return bindGenericArgument(
      pattern.expression.member.parameter,
      normalized.type,
      inferred,
      context,
    )
  }
  if (
    pattern.expression._tag === 'Without' ||
    (pattern.expression._tag === 'Union' &&
      pattern.expression.operands.some(
        (operand) => operand._tag === 'Without' || operand._tag === 'Singleton',
      ))
  )
    return false
  const memberContext = Object.freeze({ ...context, allowOpenGenericArguments: false })
  const matched = inferRowMembers(
    failureMembers(pattern),
    failureMembers(actual),
    inferred,
    (failure, supplied, trial) => inferType(failure, supplied, trial, memberContext),
    (remaining, trial) => {
      void trial
      return remaining.length === 0
    },
    context,
  )
  if (matched === undefined) return false
  const trial = new Map(matched)
  commitInference(inferred, trial)
  return true
}

/** Infers one normalized requirement-row argument, assigning at most one open remainder. */
const inferRequirementRowArgument = (
  pattern: RequirementRowArgument,
  actual: RequirementRowArgument,
  inferred: Map<string, GenericArgument>,
  allowOpenActual: boolean,
  context: InferenceContext,
): boolean => {
  if (
    !allowOpenActual &&
    RowAlgebra.concretize(requirementRowPolicy(), actual.row)._tag !== 'Concrete'
  )
    return false
  if (genericArgumentKey(pattern) === genericArgumentKey(actual)) return true
  const substitutedPattern = requirementRowArgumentFromRow(
    substituteRequirementsRow(pattern.row, inferred),
  )
  if (genericArgumentKey(substitutedPattern) === genericArgumentKey(actual)) return true
  if (pattern.row.expression._tag === 'RowParameter') {
    // Occurs check: R may never bind to a row that still mentions R.
    if (
      RowAlgebra.containsRowParameter(
        requirementRowPolicy(),
        actual.row,
        pattern.row.expression.parameter,
      )
    )
      return false
    return bindGenericArgument(pattern.row.expression.parameter, actual, inferred, context)
  }
  if (substitutedPattern.row.expression._tag === 'Union') {
    const rowParameters = substitutedPattern.row.expression.operands.filter(
      (operand): operand is Extract<typeof operand, { readonly _tag: 'RowParameter' }> =>
        operand._tag === 'RowParameter',
    )
    const fixed = substitutedPattern.row.expression.operands.filter(
      (operand) => operand._tag !== 'RowParameter',
    )
    const actualOperands =
      actual.row.expression._tag === 'Union'
        ? [...actual.row.expression.operands]
        : [actual.row.expression]
    if (rowParameters.length === 1) {
      const remaining = [...actualOperands]
      let matched = true
      for (const operand of fixed) {
        const operandKey = RowAlgebra.key(requirementRowPolicy(), {
          expression: operand,
          memberWellFormed: Object.freeze([]),
        })
        const index = remaining.findIndex(
          (candidate) =>
            RowAlgebra.key(requirementRowPolicy(), {
              expression: candidate,
              memberWellFormed: Object.freeze([]),
            }) === operandKey,
        )
        if (index < 0) {
          matched = false
          break
        }
        remaining.splice(index, 1)
      }
      const parameter_ = rowParameters.at(0)?.parameter
      if (matched && parameter_ !== undefined) {
        const remainder = remaining.reduce<RequirementsRow>(
          (row, expression) =>
            RowAlgebra.union(requirementRowPolicy(), row, {
              expression,
              memberWellFormed: Object.freeze([]),
            }),
          RowAlgebra.concrete(requirementRowPolicy(), []),
        )
        // Occurs check: the open remainder may not itself mention the parameter being bound.
        if (RowAlgebra.containsRowParameter(requirementRowPolicy(), remainder, parameter_))
          return false
        return bindGenericArgument(
          parameter_,
          requirementRowArgumentFromRow(remainder),
          inferred,
          context,
        )
      }
    }
  }
  if (
    pattern.row.expression._tag === 'Without' ||
    pattern.row.expression._tag === 'Singleton' ||
    (pattern.row.expression._tag === 'Union' &&
      pattern.row.expression.operands.some(
        (operand) => operand._tag === 'Without' || operand._tag === 'Singleton',
      ))
  )
    return false
  const memberContext = Object.freeze({ ...context, allowOpenGenericArguments: false })
  const matched = inferRowMembers(
    requirementMembers(pattern),
    requirementMembers(actual),
    inferred,
    (requirement, supplied, trial) => {
      if (!requirementSatisfies(requirement, supplied) || requirement.role !== supplied.role)
        return false
      return inferType(requirement.capability, supplied.capability, trial, memberContext)
    },
    (remaining, trial) => {
      if (requirementRowParameters(pattern).length === 0)
        return remaining.length === 0 && requirementRowParameters(actual).length === 0
      const parameter_ = requirementRowParameters(pattern).at(0)
      return (
        requirementRowParameters(pattern).length === 1 &&
        parameter_ !== undefined &&
        bindGenericArgument(
          parameter_,
          requirementRowArgument(remaining, requirementRowParameters(actual)),
          trial,
          context,
        )
      )
    },
    context,
  )
  if (matched === undefined) return false
  const trial = new Map(matched)
  commitInference(inferred, trial)
  return true
}

/** Infers declaration-bound or body-local regions without inventing a longer validity. */
const inferLifetime = (
  pattern: Lifetime.Lifetime,
  actual: Lifetime.Lifetime,
  inferred: Map<string, GenericArgument>,
  context: InferenceContext,
): boolean => {
  const identity = Lifetime.key(pattern)
  const previous = inferred.get(identity)
  if (previous === undefined && context.lifetimes?.inferable?.has(identity)) {
    inferred.set(identity, actual)
    return true
  }
  if (Lifetime.equals(pattern, actual) && previous === undefined) {
    if (
      context.lifetimes === undefined &&
      (pattern._tag === 'BoundLifetime' || pattern._tag === 'LocalLifetime')
    )
      inferred.set(identity, actual)
    return true
  }
  if (context.lifetimes !== undefined) {
    const expected = previous === undefined ? pattern : previous
    return (
      Lifetime.isLifetime(expected) &&
      context.lifetimes.accepts(
        context.contravariant === true ? expected : actual,
        context.contravariant === true ? actual : expected,
        context.invariant ?? false,
      )
    )
  }
  if (pattern._tag !== 'BoundLifetime' && pattern._tag !== 'LocalLifetime') return false
  if (previous !== undefined)
    return Lifetime.isLifetime(previous) && Lifetime.equals(previous, actual)
  inferred.set(identity, actual)
  return true
}

const inferGenericArgument = (
  pattern: GenericArgument,
  actual: GenericArgument,
  inferred: Map<string, GenericArgument>,
  context: InferenceContext,
): boolean => {
  if (Lifetime.isLifetime(pattern) || Lifetime.isLifetime(actual))
    return (
      Lifetime.isLifetime(pattern) &&
      Lifetime.isLifetime(actual) &&
      inferLifetime(pattern, actual, inferred, context)
    )
  if (isRepresentationParameterArgument(pattern))
    return (
      isRepresentationArgument(actual) &&
      bindGenericArgument(pattern.parameter, actual, inferred, context)
    )
  if (isRequirementRowArgument(pattern) && isRequirementRowArgument(actual))
    return inferRequirementRowArgument(
      pattern,
      actual,
      inferred,
      context.allowOpenGenericArguments,
      context,
    )
  if (isTypeArgument(pattern) && isTypeArgument(actual))
    return inferType(pattern, actual, inferred, context)
  return genericArgumentKey(pattern) === genericArgumentKey(actual)
}

const inferFailureRows = (
  pattern: Effect,
  actual: Effect,
  inferred: Map<string, GenericArgument>,
  context: InferenceContext,
): boolean => inferFailureRow(pattern.failureRow, actual.failureRow, inferred, true, context)

const inferRequirementRows = (
  pattern: Effect,
  actual: Effect,
  inferred: Map<string, GenericArgument>,
  context: InferenceContext,
): boolean =>
  inferRequirementRowArgument(
    requirementRowArgumentFromRow(pattern.requirementRow),
    requirementRowArgumentFromRow(actual.requirementRow),
    inferred,
    true,
    context,
  )

/** Explains a failed Effect-row decomposition without replacing ordinary type diagnostics. */
export const rowInferenceFailure = (
  pattern: Type,
  actual: Type,
): RowInferenceFailure | undefined => {
  if (isNominal(pattern) && isNominal(actual)) {
    if (
      pattern.module !== actual.module ||
      pattern.name !== actual.name ||
      pattern.arguments.length !== actual.arguments.length
    )
      return undefined
    for (const [index, argument] of pattern.arguments.entries()) {
      const supplied = actual.arguments.at(index)
      if (supplied === undefined) continue
      if (!isTypeArgument(argument) || !isTypeArgument(supplied)) continue
      const failure = rowInferenceFailure(argument, supplied)
      if (failure !== undefined) return failure
    }
    return undefined
  }
  if (isFixedArray(pattern) && isFixedArray(actual))
    return rowInferenceFailure(pattern.element, actual.element)
  if (isSlice(pattern) && isSlice(actual))
    return rowInferenceFailure(pattern.element, actual.element)
  if (isReference(pattern) && isReference(actual))
    return rowInferenceFailure(pattern.target, actual.target)
  if (isPointer(pattern) && isPointer(actual))
    return rowInferenceFailure(pattern.pointee, actual.pointee)
  if (isCallable(pattern) && isCallable(actual)) {
    for (const [index, parameter_] of pattern.parameters.entries()) {
      const supplied = actual.parameters.at(index)
      if (supplied === undefined) continue
      const failure = rowInferenceFailure(parameter_, supplied)
      if (failure !== undefined) return failure
    }
    return rowInferenceFailure(pattern.result, actual.result)
  }
  if (!isEffect(pattern) || !isEffect(actual)) return undefined
  if (requirementRowParameters(actual).length !== 0)
    return Object.freeze({ _tag: 'NonFiniteRequirementRow' })
  for (const failure of [...failureMembers(pattern), ...failureMemberParameters(pattern)]) {
    if (
      ![...failureMembers(actual), ...failureMemberParameters(actual)].some((supplied) =>
        infer(failure, supplied, new Map()),
      )
    )
      return Object.freeze({ _tag: 'AbsentFailureMember', member: encode(failure) })
  }
  if (requirementRowParameters(pattern).length > 1)
    return Object.freeze({
      _tag: 'AmbiguousRequirementRemainder',
      parameters: Object.freeze(
        requirementRowParameters(pattern).map((parameter_) => parameter_.name),
      ),
    })
  for (const requirement of requirementMembers(pattern)) {
    const capabilityMatches = requirementMembers(actual).filter((supplied) =>
      infer(requirement.capability, supplied.capability, new Map()),
    )
    if (capabilityMatches.length === 0)
      return Object.freeze({
        _tag: 'AbsentRequirementMember',
        capability: encode(requirement.capability),
        role: requirement.role,
        access: requirement.access,
      })
    const roleMatches = capabilityMatches.filter((supplied) => supplied.role === requirement.role)
    if (roleMatches.length === 0)
      return Object.freeze({
        _tag: 'IncompatibleRequirementRole',
        capability: encode(requirement.capability),
        expected: requirement.role,
        actual: Object.freeze(
          [...new Set(capabilityMatches.map((supplied) => supplied.role))].sort(),
        ),
      })
    if (!roleMatches.some((supplied) => requirementSatisfies(requirement, supplied)))
      return Object.freeze({
        _tag: 'IncompatibleRequirementAccess',
        capability: encode(requirement.capability),
        role: requirement.role,
        expected: requirement.access,
        actual: Object.freeze([...new Set(roleMatches.map((supplied) => supplied.access))].sort()),
      })
  }
  return undefined
}

/** Includes hidden representation identities when checking whether a rigid region escaped. */
const argumentLifetimes = (argument: GenericArgument): ReadonlyArray<Lifetime.Lifetime> => {
  if (Lifetime.isLifetime(argument)) return [argument]
  if (
    (typeof argument !== 'string' && argument._tag === 'TypeParameter') ||
    isTypeArgument(argument)
  )
    return lifetimes(argument)
  switch (argument._tag) {
    case 'RequirementRowArgument': {
      const parameters_ = RowAlgebra.parameters(requirementRowPolicy(), argument.row)
      return [
        ...RowAlgebra.concreteMembers(requirementRowPolicy(), argument.row).flatMap((member) =>
          lifetimes(member.capability),
        ),
        ...[...parameters_.rows, ...parameters_.members].flatMap(lifetimes),
      ]
    }
    case 'UnavailableGenericArgument':
      return []
    case 'RepresentedType':
      return lifetimes(argument)
    case 'RepresentationParameterArgument':
      return lifetimes(argument.parameter)
    case 'OpaqueRepresentationArgument':
      return [...lifetimes(argument.contract), ...argument.arguments.flatMap(argumentLifetimes)]
    case 'ExactRepresentationArgument':
      return [...lifetimes(argument.contract), ...argumentLifetimes(argument.identity)]
    case 'CompositeEffectRepresentationArgument':
      return [...lifetimes(argument.contract), ...argument.alternatives.flatMap(argumentLifetimes)]
    case 'EffectIdentityArgument':
      return argument.owner?.typeArguments.flatMap(argumentLifetimes) ?? []
    case 'CallableIdentityArgument':
      return [
        ...argument.typeArguments.flatMap(argumentLifetimes),
        ...(argument.environment?.owner.typeArguments.flatMap(argumentLifetimes) ?? []),
      ]
  }
}

/** Opens one finite outer binder in a rigid universe and commits only nonescaping inference. */
const inferQuantifiedExecutable = (
  pattern: Callable | Effect,
  actual: Callable | Effect,
  inferred: Map<string, GenericArgument>,
  context: InferenceContext,
): boolean => {
  if (pattern.lifetimeBinders.length !== actual.lifetimeBinders.length) return false
  const nested = (type: Type): boolean =>
    someSubterm(
      type,
      (part) => (isCallable(part) || isEffect(part)) && part.lifetimeBinders.length > 0,
    )
  if (
    !isCallable(pattern) ||
    !isCallable(actual) ||
    [...pattern.parameters, pattern.result, ...actual.parameters, actual.result].some(nested)
  )
    return false
  const patternSubstitution = new Map<string, GenericArgument>()
  const actualSubstitution = new Map<string, GenericArgument>()
  const universe = `inference:${key(pattern)}:${key(actual)}`
  for (const [ordinal, binder] of pattern.lifetimeBinders.entries()) {
    const supplied = actual.lifetimeBinders.at(ordinal)
    if (supplied === undefined) return false
    const rigid = Lifetime.placeholder(binder, universe)
    patternSubstitution.set(Lifetime.key(binder), rigid)
    actualSubstitution.set(Lifetime.key(supplied), rigid)
  }
  const open = (self: Callable | Effect, substitution: Substitution): Callable | Effect => {
    const metadata = {
      environment: self.environment,
      lifetimeBinders: [],
      typeOutlives: self.typeOutlives.map((bound) => ({
        type: substitute(bound.type, substitution),
        lifetime: substituteLifetime(bound.lifetime, substitution),
      })),
      lifetimeBounds: self.lifetimeBounds.map((bound) => ({
        longer: substituteLifetime(bound.longer, substitution),
        shorter: substituteLifetime(bound.shorter, substitution),
      })),
    }
    return isCallable(self)
      ? callable(
          self.parameters.map((parameter_) => substitute(parameter_, substitution)),
          substitute(self.result, substitution),
          metadata,
          self.mode,
          self.schema,
          self.unsafe,
        )
      : effectWithRows(
          substitute(self.success, substitution),
          substituteFailureRow(self.failureRow, substitution),
          metadata,
          self.access,
          substituteRequirementsRow(self.requirementRow, substitution),
        )
  }
  const trial = new Map(inferred)
  if (
    !inferType(open(pattern, patternSubstitution), open(actual, actualSubstitution), trial, context)
  )
    return false
  for (const [identity, argument] of trial) {
    if (inferred.has(identity)) continue
    const regions = argumentLifetimes(argument)
    if (
      regions.some(
        (region) => region._tag === 'PlaceholderLifetime' && region.universe === universe,
      )
    )
      return false
  }
  commitInference(inferred, trial)
  return true
}

/** Invocation preconditions need implication; independent formation facts travel with the value. */
const inferExecutableBounds = (
  pattern: Callable | Effect,
  actual: Callable | Effect,
  inferred: Substitution,
  context: InferenceContext,
): boolean => {
  const formation = executableFormationRequirements(actual)
  const expected = Lifetime.assumptions(
    [...pattern.lifetimeBounds, ...formation.lifetimeBounds].map((bound) => ({
      longer: substituteLifetime(bound.longer, inferred),
      shorter: substituteLifetime(bound.shorter, inferred),
    })),
  )
  const expectedTypes = [...pattern.typeOutlives, ...formation.typeOutlives].map((bound) => ({
    type: substitute(bound.type, inferred),
    lifetime: substituteLifetime(bound.lifetime, inferred),
  }))
  const proves = (longer: Lifetime.Lifetime, shorter: Lifetime.Lifetime): boolean =>
    Lifetime.outlives(expected, longer, shorter) ||
    (context.lifetimes?.accepts(longer, shorter, false) ?? false)
  return (
    actual.typeOutlives.every((bound) => {
      const type = substitute(bound.type, inferred)
      const lifetime = substituteLifetime(bound.lifetime, inferred)
      return (
        satisfiesOutlives(type, lifetime, expectedTypes, proves) ||
        (context.lifetimes?.typeOutlives?.(type, lifetime) ?? false)
      )
    }) &&
    actual.lifetimeBounds.every((bound) => {
      const longer = substituteLifetime(bound.longer, inferred)
      const shorter = substituteLifetime(bound.shorter, inferred)
      return (
        Lifetime.outlives(expected, longer, shorter) ||
        (context.lifetimes?.accepts(longer, shorter, false) ?? false)
      )
    })
  )
}

const inferType = (
  pattern: Type,
  actual: Type,
  inferred: Map<string, GenericArgument>,
  context: InferenceContext,
): boolean =>
  commitTrial(
    context,
    () => inferSelectedType(pattern, actual, inferred, context),
    (matched) => matched,
  )

const inferSelectedType = (
  pattern: Type,
  actual: Type,
  inferred: Map<string, GenericArgument>,
  context: InferenceContext,
): boolean => {
  // A diverging expression satisfies every expected result without replacing a generic already
  // inferred from a value argument. When it is the only evidence (including an explicit `never`
  // type argument), retain it so exact specialization keys remain complete.
  if (isNever(actual)) {
    if (!isParameter(pattern)) return true
    return inferred.has(key(pattern))
      ? true
      : bindGenericArgument(pattern, actual, inferred, context)
  }
  if (isParameter(pattern)) {
    const fixed = inferred.get(key(pattern))
    if (
      pattern.kind === 'Value' &&
      fixed !== undefined &&
      isTypeArgument(fixed) &&
      context.lifetimes !== undefined
    ) {
      const comparison =
        context.lifetimes.compatibility ??
        TypeCompatibility.context({
          outlives: (source, target) =>
            context.lifetimes?.accepts(source, target, context.invariant ?? false) ?? false,
          typeOutlives: context.lifetimes.typeOutlives,
        })
      const source = context.contravariant ? fixed : actual
      const target = context.contravariant ? actual : fixed
      if (
        TypeCompatibility.isCompatible(TypeCompatibility.check(source, target, comparison)) &&
        (!context.invariant ||
          TypeCompatibility.isCompatible(TypeCompatibility.check(target, source, comparison)))
      )
        return true
    }
    return pattern.kind === 'Value' && bindGenericArgument(pattern, actual, inferred, context)
  }
  if (isNominal(pattern) && isNominal(actual)) {
    if (
      pattern.module !== actual.module ||
      pattern.name !== actual.name ||
      pattern.arguments.length !== actual.arguments.length
    )
      return false
    return pattern.arguments.every((argument, index) => {
      const supplied = actual.arguments.at(index)
      return supplied !== undefined && inferGenericArgument(argument, supplied, inferred, context)
    })
  }
  if (isString(pattern) && isString(actual))
    return inferLifetime(pattern.lifetime, actual.lifetime, inferred, context)
  if (isFixedArray(pattern) && isFixedArray(actual)) {
    return (
      pattern.length === actual.length &&
      inferType(pattern.element, actual.element, inferred, context)
    )
  }
  if (isSlice(pattern) && isSlice(actual)) {
    return (
      pattern.access === actual.access &&
      inferLifetime(pattern.lifetime, actual.lifetime, inferred, context) &&
      inferType(
        pattern.element,
        actual.element,
        inferred,
        pattern.access === 'Exclusive' ? { ...context, invariant: true } : context,
      ) &&
      (context.lifetimes !== undefined ||
        pattern.access !== 'Exclusive' ||
        equals(substitute(pattern.element, inferred), actual.element))
    )
  }
  if (isReference(pattern) && isReference(actual)) {
    return (
      compareAccess(actual.access, pattern.access) &&
      inferLifetime(pattern.lifetime, actual.lifetime, inferred, context) &&
      inferType(
        pattern.target,
        actual.target,
        inferred,
        pattern.access === 'Exclusive' ? { ...context, invariant: true } : context,
      ) &&
      (context.lifetimes !== undefined ||
        pattern.access !== 'Exclusive' ||
        equals(substitute(pattern.target, inferred), actual.target))
    )
  }
  if (isPointer(pattern) && isPointer(actual)) {
    // `*mut T` satisfies a `*const T` pattern; the reverse does not.
    return (
      (!pattern.mutable || actual.mutable) &&
      inferType(pattern.pointee, actual.pointee, inferred, { ...context, invariant: true })
    )
  }
  if (isCallable(pattern) && isCallable(actual)) {
    if (pattern.lifetimeBinders.length !== 0 || actual.lifetimeBinders.length !== 0)
      return inferQuantifiedExecutable(pattern, actual, inferred, context)
    return (
      inferLifetime(pattern.environment, actual.environment, inferred, context) &&
      (!actual.unsafe || pattern.unsafe) &&
      compareAccess(pattern.mode, actual.mode) &&
      pattern.parameters.length === actual.parameters.length &&
      pattern.parameters.every((parameter_, index) => {
        const supplied = actual.parameters.at(index)
        return (
          supplied !== undefined &&
          inferType(parameter_, supplied, inferred, {
            ...context,
            contravariant: !context.contravariant,
          })
        )
      }) &&
      inferType(pattern.result, actual.result, inferred, context) &&
      inferExecutableBounds(pattern, actual, inferred, context)
    )
  }
  if (isEffect(pattern) && isEffect(actual)) {
    if (pattern.lifetimeBinders.length !== 0 || actual.lifetimeBinders.length !== 0)
      return inferQuantifiedExecutable(pattern, actual, inferred, context)
    return (
      inferLifetime(pattern.environment, actual.environment, inferred, context) &&
      compareAccess(pattern.access, actual.access) &&
      inferType(pattern.success, actual.success, inferred, context) &&
      inferFailureRows(pattern, actual, inferred, context) &&
      inferRequirementRows(pattern, actual, inferred, context) &&
      inferExecutableBounds(pattern, actual, inferred, context)
    )
  }
  if (isRepresented(pattern) && isRepresented(actual)) {
    if (!inferType(pattern.contract, actual.contract, inferred, context)) return false
    return inferGenericArgument(
      pattern.representation.argument,
      actual.representation.argument,
      inferred,
      context,
    )
  }
  return equals(pattern, actual)
}

export const infer = (
  pattern: Type,
  actual: Type,
  inferred: Map<string, GenericArgument>,
  lifetimes?: LifetimeInference,
): boolean =>
  inferType(
    pattern,
    actual,
    inferred,
    Object.freeze({ allowOpenGenericArguments: false, lifetimes }),
  )

/** Infers through generic arguments that remain open over an enclosing declaration. */
export const inferOpenGenericArguments = (
  pattern: Type,
  actual: Type,
  inferred: Map<string, GenericArgument>,
): OpenGenericInference => {
  const conflicts: Array<GenericArgumentConflict> = []
  const matches = inferType(
    pattern,
    actual,
    inferred,
    Object.freeze({ allowOpenGenericArguments: true, conflicts }),
  )
  return Object.freeze({ matches, conflicts: Object.freeze(conflicts) })
}

/**
 * Binds the supplied prefix independently in the lifetime and ordinary generic namespaces.
 * Omitted lifetime arguments remain open while an ordinary prefix such as `<A>` binds the first
 * ordinary parameter. Kind mismatches and excess arguments in either namespace are rejected.
 */
export const prefixSubstitution = (
  declared: ReadonlyArray<Parameter>,
  arguments_: ReadonlyArray<GenericArgument>,
): Substitution | undefined => bindPrefix(declared, arguments_)

const bindPrefix = (
  declared: ReadonlyArray<Parameter>,
  arguments_: ReadonlyArray<GenericArgument>,
  representationContext?: TypeCompatibility.Context,
): Substitution | undefined => {
  if (arguments_.length > declared.length) return undefined
  const result = new Map<string, GenericArgument>()
  const lifetimeParameters = declared.filter((parameter) => parameter.kind === 'Lifetime')
  const ordinaryParameters = declared.filter((parameter) => parameter.kind !== 'Lifetime')
  let lifetimeOrdinal = 0
  let ordinaryOrdinal = 0
  const supplied: Array<{ readonly parameter: Parameter; readonly argument: GenericArgument }> = []
  for (const argument of arguments_) {
    const parameter = Lifetime.isLifetime(argument)
      ? lifetimeParameters.at(lifetimeOrdinal++)
      : ordinaryParameters.at(ordinaryOrdinal++)
    if (parameter === undefined) return undefined
    supplied.push({ parameter, argument })
    result.set(key(parameter), argument)
  }
  for (const { parameter: parameter_, argument } of supplied) {
    const rawRepresentationContract = isRepresentationArgument(argument)
      ? representationArgumentContract(argument)
      : undefined
    const substitutedRepresentationContract =
      rawRepresentationContract === undefined
        ? undefined
        : substitute(rawRepresentationContract, result)
    const representationContract =
      substitutedRepresentationContract !== undefined &&
      (isCallable(substitutedRepresentationContract) || isEffect(substitutedRepresentationContract))
        ? substitutedRepresentationContract
        : undefined
    const substitutedRepresentationBound =
      parameter_.representationBound === undefined
        ? undefined
        : substitute(parameter_.representationBound, result)
    const requiredRepresentationBound =
      substitutedRepresentationBound !== undefined &&
      (isCallable(substitutedRepresentationBound) || isEffect(substitutedRepresentationBound))
        ? substitutedRepresentationBound
        : undefined
    let suppliedStaticProperties: ReadonlyArray<SealedStaticProperty> | undefined
    if (isRepresentationArgument(argument) && argument._tag === 'RepresentationParameterArgument') {
      suppliedStaticProperties = argument.parameter.staticProperties
    } else if (isTypeArgument(argument) && isParameter(argument)) {
      suppliedStaticProperties = argument.staticProperties
    }
    const preservesStaticProperties =
      suppliedStaticProperties === undefined ||
      parameter_.staticProperties.every((property) => suppliedStaticProperties.includes(property))
    if (
      (parameter_.kind === 'Lifetime' && !Lifetime.isLifetime(argument)) ||
      (parameter_.kind === 'Value' && !isTypeArgument(argument)) ||
      (parameter_.kind === 'RequirementRow' && !isRequirementRowArgument(argument)) ||
      ((parameter_.kind === 'CallableRepresentation' ||
        parameter_.kind === 'EffectRepresentation') &&
        (!isRepresentationArgument(argument) ||
          representationArgumentKind(argument) !== parameter_.kind ||
          requiredRepresentationBound === undefined ||
          representationContract === undefined ||
          !preservesStaticProperties ||
          representationAdmissibility(
            representationContract,
            requiredRepresentationBound,
            representationContext,
          )._tag !== 'Admitted'))
    )
      return undefined
  }
  return result
}

/** Builds a substitution from ordered parameters and arguments when their arities match. */
export const substitution = (
  declared: ReadonlyArray<Parameter>,
  arguments_: ReadonlyArray<GenericArgument>,
): Substitution | undefined =>
  declared.length !== arguments_.length ? undefined : prefixSubstitution(declared, arguments_)

/** Exact semantic arguments and the selected call's proven representation lifetime relations. */
export interface SelectedSubstitution {
  readonly substitution: Substitution
  readonly compatibility: TypeCompatibility.Context
}

/**
 * Rebinds a complete invocation selected by semantic checking. Only Instances and ExecutableOrigin
 * may use this boundary: the caller must supply already checked HIR arguments. Kind, access and
 * representation structure are verified again; only their accepted lifetime relations are carried
 * forward as explicit assumptions. Detached and NonParking obligations remain independently checked.
 */
export const selectedSubstitution = (
  declared: ReadonlyArray<Parameter>,
  arguments_: ReadonlyArray<GenericArgument>,
): SelectedSubstitution | undefined => {
  if (declared.length !== arguments_.length) return undefined
  const bounds: Array<Lifetime.Outlives> = []
  const typeBounds: Array<TypeOutlives> = []
  const collect = TypeCompatibility.context({
    outlives: () => true,
    commitOutlives: (longer, shorter) => {
      bounds.push({ longer, shorter })
    },
    typeOutlives: () => true,
    commitTypeOutlives: (type, lifetime) => {
      typeBounds.push({ type, lifetime })
    },
  })
  const substitution = bindPrefix(declared, arguments_, collect)
  return substitution === undefined
    ? undefined
    : Object.freeze({
        substitution,
        compatibility: TypeCompatibility.context({
          assumptions: Lifetime.assumptions(bounds),
          typeBounds,
        }),
      })
}
