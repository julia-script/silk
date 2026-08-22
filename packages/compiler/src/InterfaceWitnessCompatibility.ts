import * as Type from './Type.js'

export type Access = 'Shared' | 'Exclusive' | 'Take'

export interface Operand {
  readonly name: string
  readonly type: Type.Type
  readonly receiver: boolean
}

export interface Contract {
  readonly functionKind: 'Ordinary' | 'Effect'
  readonly unsafe: boolean
  readonly operands: ReadonlyArray<Operand>
  readonly success: Type.Type
  readonly failures: ReadonlyArray<Type.Type>
  readonly requirements: ReadonlyArray<Type.Requirement>
  readonly requirementParameters: ReadonlyArray<Type.Parameter>
}

export interface Witness extends Contract {}

export type Problem =
  | { readonly _tag: 'StrongerSafety' }
  | {
      readonly _tag: 'StrongerFlow'
      readonly promised: Contract['functionKind']
      readonly required: Witness['functionKind']
    }
  | { readonly _tag: 'OperandArity'; readonly promised: number; readonly required: number }
  | {
      readonly _tag: 'StrongerOperandAccess'
      readonly ordinal: number
      readonly name: string
      readonly receiver: boolean
      readonly promised: Access
      readonly required: Access
    }
  | {
      readonly _tag: 'OperandOwnership'
      readonly ordinal: number
      readonly name: string
      readonly promised: Access
      readonly required: Access
    }
  | {
      readonly _tag: 'OperandType'
      readonly ordinal: number
      readonly name: string
      readonly promised: Type.Type
      readonly required: Type.Type
    }
  | { readonly _tag: 'Success'; readonly promised: Type.Type; readonly actual: Type.Type }
  | { readonly _tag: 'Failure'; readonly failure: Type.Type }
  | {
      readonly _tag: 'StrongerRequirementAccess'
      readonly requirement: Type.Requirement
      readonly promised: ReadonlyArray<Type.Requirement['access']>
    }
  | { readonly _tag: 'Requirement'; readonly requirement: Type.Requirement }
  | { readonly _tag: 'RequirementParameter'; readonly parameter: Type.Parameter }

export type Compatibility =
  | { readonly _tag: 'Compatible' }
  | { readonly _tag: 'Incompatible'; readonly problem: Problem }

type OperandShape =
  | { readonly _tag: 'Value'; readonly access: 'Take'; readonly type: Type.Type }
  | {
      readonly _tag: 'Reference'
      readonly access: Type.Reference['access']
      readonly type: Type.Type
    }
  | { readonly _tag: 'Slice'; readonly access: Type.Slice['access']; readonly type: Type.Type }

const operandShape = (type: Type.Type): OperandShape => {
  if (Type.isReference(type))
    return Object.freeze({ _tag: 'Reference', access: type.access, type: type.target })
  if (Type.isSlice(type))
    return Object.freeze({ _tag: 'Slice', access: type.access, type: type.element })
  return Object.freeze({ _tag: 'Value', access: 'Take', type })
}

const incompatible = (problem: Problem): Compatibility =>
  Object.freeze({ _tag: 'Incompatible', problem })

const operandProblem = (
  contract: Operand,
  witness: Operand,
  ordinal: number,
): Problem | undefined => {
  const promised = operandShape(contract.type)
  const required = operandShape(witness.type)
  if (!Type.compareAccess(promised.access, required.access))
    return Object.freeze({
      _tag: 'StrongerOperandAccess',
      ordinal,
      name: contract.name,
      receiver: contract.receiver,
      promised: promised.access,
      required: required.access,
    })
  if (!Type.equals(promised.type, required.type))
    return Object.freeze({
      _tag: 'OperandType',
      ordinal,
      name: contract.name,
      promised: contract.type,
      required: witness.type,
    })
  if (promised._tag !== required._tag && promised._tag !== 'Value' && required._tag !== 'Value')
    return Object.freeze({
      _tag: 'OperandOwnership',
      ordinal,
      name: contract.name,
      promised: promised.access,
      required: required.access,
    })
  return undefined
}

/** Checks one substituted interface contract without narrowing or rewriting that caller contract. */
export const check = (contract: Contract, witness: Witness): Compatibility => {
  if (!contract.unsafe && witness.unsafe)
    return incompatible(Object.freeze({ _tag: 'StrongerSafety' }))
  if (contract.functionKind === 'Ordinary' && witness.functionKind === 'Effect')
    return incompatible(
      Object.freeze({
        _tag: 'StrongerFlow',
        promised: contract.functionKind,
        required: witness.functionKind,
      }),
    )
  if (contract.operands.length !== witness.operands.length)
    return incompatible(
      Object.freeze({
        _tag: 'OperandArity',
        promised: contract.operands.length,
        required: witness.operands.length,
      }),
    )
  for (const [ordinal, operand] of contract.operands.entries()) {
    const implementation = witness.operands.at(ordinal)
    if (implementation === undefined)
      return incompatible(
        Object.freeze({
          _tag: 'OperandArity',
          promised: contract.operands.length,
          required: witness.operands.length,
        }),
      )
    const problem = operandProblem(operand, implementation, ordinal)
    if (problem !== undefined) return incompatible(problem)
  }
  if (!Type.equals(witness.success, contract.success))
    return incompatible(
      Object.freeze({ _tag: 'Success', promised: contract.success, actual: witness.success }),
    )
  for (const failure of witness.failures)
    if (!contract.failures.some((allowed) => Type.equals(failure, allowed)))
      return incompatible(Object.freeze({ _tag: 'Failure', failure }))
  for (const requirement of witness.requirements) {
    const matching = contract.requirements.filter(
      (allowed) =>
        Type.equals(requirement.capability, allowed.capability) &&
        requirement.role === allowed.role,
    )
    if (matching.some((allowed) => Type.requirementSatisfies(allowed, requirement))) continue
    if (matching.length > 0)
      return incompatible(
        Object.freeze({
          _tag: 'StrongerRequirementAccess',
          requirement,
          promised: Object.freeze(matching.map((allowed) => allowed.access)),
        }),
      )
    return incompatible(Object.freeze({ _tag: 'Requirement', requirement }))
  }
  for (const parameter of witness.requirementParameters)
    if (!contract.requirementParameters.some((allowed) => Type.equals(parameter, allowed)))
      return incompatible(Object.freeze({ _tag: 'RequirementParameter', parameter }))
  return Object.freeze({ _tag: 'Compatible' })
}

const accessText = (access: Access): string => access.toLowerCase()

/** Renders the stable first-demand explanation for a conformance diagnostic. */
export const describe = (self: Compatibility): string | undefined => {
  if (self._tag === 'Compatible') return undefined
  const problem = self.problem
  switch (problem._tag) {
    case 'StrongerSafety':
      return 'unsafe witness cannot satisfy a safe operation contract'
    case 'StrongerFlow':
      return `witness flow ${problem.required.toLowerCase()} is stronger than promised ${problem.promised.toLowerCase()} flow`
    case 'OperandArity':
      return `witness requires ${problem.required} operands but the interface promises ${problem.promised}`
    case 'StrongerOperandAccess':
      return `${problem.receiver ? 'receiver' : 'parameter'} ${problem.name} requires ${accessText(problem.required)} access but the interface promises ${accessText(problem.promised)} access`
    case 'OperandOwnership':
      return `parameter ${problem.name} requires ${accessText(problem.required)} ownership but the interface promises ${accessText(problem.promised)} ownership`
    case 'OperandType':
      return `parameter ${problem.name} requires ${Type.encode(problem.required)} but the interface promises ${Type.encode(problem.promised)}`
    case 'Success':
      return `witness returns ${Type.encode(problem.actual)} but the interface promises ${Type.encode(problem.promised)}`
    case 'Failure':
      return `witness adds failure ${Type.encode(problem.failure)}`
    case 'StrongerRequirementAccess':
      return `witness requires ${problem.requirement.access.toLowerCase()} access to ${Type.encode(problem.requirement.capability)} but the interface promises ${problem.promised.map((access) => access.toLowerCase()).join(' or ')} access`
    case 'Requirement':
      return `witness adds requirement ${Type.encode(problem.requirement.capability)} in role ${problem.requirement.role}`
    case 'RequirementParameter':
      return `witness adds open requirement row ${problem.parameter.name}`
  }
}
