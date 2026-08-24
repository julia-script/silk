import * as RowAlgebra from '../RowAlgebra.js'
import type {
  Effect,
  FailureRow,
  GenericArgument,
  Parameter,
  RepresentationArgument,
  RepresentationBound,
  RequirementRowArgument,
  RequirementsRow,
  RowInferenceFailure,
  Substitution,
  Type,
} from '../Type.js'
import {
  compareAccess,
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
  isReference,
  isRepresentationArgument,
  isRepresentationParameterArgument,
  isRepresented,
  isRequirementRowArgument,
  isSlice,
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
  substitute,
  substituteFailureRow,
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

interface InferenceContext {
  readonly allowOpenGenericArguments: boolean
  readonly conflicts?: Array<GenericArgumentConflict>
}

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
      const trial = new Map(current)
      if (!matches(member, candidate, trial)) continue
      const found = search(
        position + 1,
        remaining.filter((_, index) => index !== candidatePosition),
        trial,
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
  )
  if (matched === undefined) return false
  const trial = new Map(matched)
  commitInference(inferred, trial)
  return true
}

const inferGenericArgument = (
  pattern: GenericArgument,
  actual: GenericArgument,
  inferred: Map<string, GenericArgument>,
  context: InferenceContext,
): boolean => {
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

const inferType = (
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
  if (isFixedArray(pattern) && isFixedArray(actual)) {
    return (
      pattern.length === actual.length &&
      inferType(pattern.element, actual.element, inferred, context)
    )
  }
  if (isSlice(pattern) && isSlice(actual)) {
    return (
      pattern.access === actual.access &&
      inferType(pattern.element, actual.element, inferred, context)
    )
  }
  if (isReference(pattern) && isReference(actual)) {
    return (
      compareAccess(actual.access, pattern.access) &&
      inferType(pattern.target, actual.target, inferred, context)
    )
  }
  if (isCallable(pattern) && isCallable(actual)) {
    return (
      (!actual.unsafe || pattern.unsafe) &&
      compareAccess(pattern.mode, actual.mode) &&
      pattern.parameters.length === actual.parameters.length &&
      pattern.parameters.every((parameter_, index) => {
        const supplied = actual.parameters.at(index)
        return supplied !== undefined && inferType(parameter_, supplied, inferred, context)
      }) &&
      inferType(pattern.result, actual.result, inferred, context)
    )
  }
  if (isEffect(pattern) && isEffect(actual)) {
    return (
      compareAccess(pattern.access, actual.access) &&
      inferType(pattern.success, actual.success, inferred, context) &&
      inferFailureRows(pattern, actual, inferred, context) &&
      inferRequirementRows(pattern, actual, inferred, context)
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
): boolean =>
  inferType(pattern, actual, inferred, Object.freeze({ allowOpenGenericArguments: false }))

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
 * Builds a substitution from a leading run of parameters, binding only the parameters an argument
 * was supplied for. The parameters past the prefix stay open, so inference can determine them
 * afterwards; the result is undefined when an argument's kind does not match its parameter, or
 * when more arguments were supplied than the declaration has parameters.
 */
export const prefixSubstitution = (
  declared: ReadonlyArray<Parameter>,
  arguments_: ReadonlyArray<GenericArgument>,
): Substitution | undefined => {
  if (arguments_.length > declared.length) return undefined
  const result = new Map<string, GenericArgument>()
  for (const [index, argument] of arguments_.entries()) {
    const parameter_ = declared.at(index)
    if (parameter_ === undefined) return undefined
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
    const suppliedStaticProperties =
      isRepresentationArgument(argument) && argument._tag === 'RepresentationParameterArgument'
        ? argument.parameter.staticProperties
        : isTypeArgument(argument) && isParameter(argument)
          ? argument.staticProperties
          : undefined
    const preservesStaticProperties =
      suppliedStaticProperties === undefined ||
      parameter_.staticProperties.every((property) => suppliedStaticProperties.includes(property))
    if (
      (parameter_.kind === 'Value' && !isTypeArgument(argument)) ||
      (parameter_.kind === 'RequirementRow' && !isRequirementRowArgument(argument)) ||
      ((parameter_.kind === 'CallableRepresentation' ||
        parameter_.kind === 'EffectRepresentation') &&
        (!isRepresentationArgument(argument) ||
          representationArgumentKind(argument) !== parameter_.kind ||
          requiredRepresentationBound === undefined ||
          representationContract === undefined ||
          !preservesStaticProperties ||
          representationAdmissibility(representationContract, requiredRepresentationBound)._tag !==
            'Admitted'))
    )
      return undefined
    result.set(key(parameter_), argument)
  }
  return result
}

/** Builds a substitution from ordered parameters and arguments when their arities match. */
export const substitution = (
  declared: ReadonlyArray<Parameter>,
  arguments_: ReadonlyArray<GenericArgument>,
): Substitution | undefined =>
  declared.length !== arguments_.length ? undefined : prefixSubstitution(declared, arguments_)
