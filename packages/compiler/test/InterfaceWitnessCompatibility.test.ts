import { assert, it } from '@effect/vitest'
import * as InterfaceWitnessCompatibility from '../src/InterfaceWitnessCompatibility.js'
import * as Lifetime from '../src/Lifetime.js'
import * as Type from '../src/Type.js'

const schema = Type.nominal('test', 'Schema')
const decodeError = Type.nominal('test', 'DecodeError')
const extraError = Type.nominal('test', 'ExtraError')
const clock = Type.nominal('test', 'Clock')
const logger = Type.nominal('test', 'Logger')
const failureParameter = Type.parameter({ module: 'test', name: 'Contract' }, 0, 'E')
const requirementParameter = Type.parameter(
  { module: 'test', name: 'Contract' },
  1,
  'R',
  'RequirementRow',
)

const operand = (
  name: string,
  type: Type.Type,
  receiver = false,
): InterfaceWitnessCompatibility.Operand => Object.freeze({ name, type, receiver })

const contract = Object.freeze({
  functionKind: 'Effect' as const,
  unsafe: false,
  operands: Object.freeze([
    operand('self', Type.reference('Exclusive', schema, Lifetime.staticLifetime), true),
    operand('encoded', 'i32'),
  ]),
  success: 'bool' as const,
  failures: Object.freeze([decodeError, extraError]),
  requirements: Object.freeze([
    Object.freeze({ capability: clock, role: 'DefaultRole', access: 'Exclusive' as const }),
    Object.freeze({ capability: logger, role: 'DefaultRole', access: 'Shared' as const }),
  ]),
  requirementParameters: Object.freeze([]),
})

it('admits a pure witness with smaller rows and weaker receiver and requirement access', () => {
  const witness = Object.freeze({
    functionKind: 'Ordinary' as const,
    unsafe: false,
    operands: Object.freeze([
      operand('self', Type.reference('Shared', schema, Lifetime.staticLifetime), true),
      operand('encoded', 'i32'),
    ]),
    success: 'bool' as const,
    failures: Object.freeze([decodeError]),
    requirements: Object.freeze([
      Object.freeze({ capability: clock, role: 'DefaultRole', access: 'Shared' as const }),
    ]),
    requirementParameters: Object.freeze([]),
  })

  assert.deepEqual(InterfaceWitnessCompatibility.check(contract, witness), {
    _tag: 'Compatible',
  })
  assert.deepEqual(contract.failures, [decodeError, extraError])
  assert.deepEqual(
    contract.requirements.map((requirement) => requirement.capability),
    [clock, logger],
  )
})

it('reports the first stronger receiver demand before later row demands', () => {
  const receiverContract = Object.freeze({
    ...contract,
    operands: Object.freeze([
      operand('self', Type.reference('Shared', schema, Lifetime.staticLifetime), true),
      operand('encoded', 'i32'),
    ]),
  })
  const witness = Object.freeze({
    functionKind: 'Effect' as const,
    unsafe: false,
    operands: Object.freeze([
      operand('self', Type.reference('Exclusive', schema, Lifetime.staticLifetime), true),
      operand('encoded', 'i32'),
    ]),
    success: 'bool' as const,
    failures: Object.freeze([Type.nominal('test', 'Unpromised')]),
    requirements: Object.freeze([]),
    requirementParameters: Object.freeze([]),
  })

  const compatibility = InterfaceWitnessCompatibility.check(receiverContract, witness)
  assert.strictEqual(compatibility._tag, 'Incompatible')
  if (compatibility._tag !== 'Incompatible') return
  assert.deepEqual(compatibility.problem, {
    _tag: 'StrongerOperandAccess',
    ordinal: 0,
    name: 'self',
    receiver: true,
    promised: 'Shared',
    required: 'Exclusive',
  })
  assert.strictEqual(
    InterfaceWitnessCompatibility.describe(compatibility),
    'receiver self requires exclusive access but the interface promises shared access',
  )
})

it('rejects stronger parameter, failure, and requirement demands deterministically', () => {
  const strongerParameter = Object.freeze({
    ...contract,
    operands: Object.freeze([
      operand('self', Type.reference('Shared', schema, Lifetime.staticLifetime), true),
      operand('encoded', Type.reference('Exclusive', 'i32', Lifetime.staticLifetime)),
    ]),
    failures: Object.freeze([]),
    requirements: Object.freeze([]),
  })
  const borrowedContract = Object.freeze({
    ...contract,
    operands: Object.freeze([
      operand('self', Type.reference('Shared', schema, Lifetime.staticLifetime), true),
      operand('encoded', Type.reference('Shared', 'i32', Lifetime.staticLifetime)),
    ]),
    failures: Object.freeze([]),
    requirements: Object.freeze([]),
  })
  const parameterCompatibility = InterfaceWitnessCompatibility.check(
    borrowedContract,
    strongerParameter,
  )
  assert.strictEqual(parameterCompatibility._tag, 'Incompatible')
  if (parameterCompatibility._tag !== 'Incompatible') return
  assert.deepEqual(parameterCompatibility.problem, {
    _tag: 'StrongerOperandAccess',
    ordinal: 1,
    name: 'encoded',
    receiver: false,
    promised: 'Shared',
    required: 'Exclusive',
  })

  const strongerRows = Object.freeze({
    ...contract,
    failures: Object.freeze([Type.nominal('test', 'Unpromised')]),
    requirements: Object.freeze([
      Object.freeze({ capability: logger, role: 'DefaultRole', access: 'Exclusive' as const }),
    ]),
  })
  const failureCompatibility = InterfaceWitnessCompatibility.check(contract, strongerRows)
  assert.strictEqual(failureCompatibility._tag, 'Incompatible')
  if (failureCompatibility._tag !== 'Incompatible') return
  assert.deepEqual(failureCompatibility.problem, {
    _tag: 'Failure',
    failure: Type.nominal('test', 'Unpromised'),
  })

  const strongerRequirement = Object.freeze({
    ...contract,
    failures: Object.freeze([]),
    requirements: Object.freeze([
      Object.freeze({ capability: logger, role: 'DefaultRole', access: 'Exclusive' as const }),
    ]),
  })
  const requirementCompatibility = InterfaceWitnessCompatibility.check(
    contract,
    strongerRequirement,
  )
  assert.strictEqual(requirementCompatibility._tag, 'Incompatible')
  if (requirementCompatibility._tag !== 'Incompatible') return
  assert.deepEqual(requirementCompatibility.problem, {
    _tag: 'StrongerRequirementAccess',
    requirement: { capability: logger, role: 'DefaultRole', access: 'Exclusive' },
    promised: ['Shared'],
  })
})

it('rejects stronger flow and non-exact operand or success types', () => {
  const ordinaryContract = Object.freeze({ ...contract, functionKind: 'Ordinary' as const })
  const strongerFlow = InterfaceWitnessCompatibility.check(ordinaryContract, contract)
  assert.strictEqual(strongerFlow._tag, 'Incompatible')
  if (strongerFlow._tag !== 'Incompatible') return
  assert.deepEqual(strongerFlow.problem, {
    _tag: 'StrongerFlow',
    promised: 'Ordinary',
    required: 'Effect',
  })
  assert.strictEqual(
    InterfaceWitnessCompatibility.describe(strongerFlow),
    'witness flow effect is stronger than promised ordinary flow',
  )

  const union = Type.union([decodeError, extraError])
  assert.strictEqual(union._tag, 'Normalized')
  if (union._tag !== 'Normalized') return
  const widerOperand = Object.freeze({
    ...contract,
    operands: Object.freeze([
      operand('self', Type.reference('Exclusive', schema, Lifetime.staticLifetime), true),
      operand('encoded', union.type),
    ]),
  })
  const operandCompatibility = InterfaceWitnessCompatibility.check(contract, widerOperand)
  assert.strictEqual(operandCompatibility._tag, 'Incompatible')
  if (operandCompatibility._tag !== 'Incompatible') return
  assert.strictEqual(operandCompatibility.problem._tag, 'OperandType')

  const unionContract = Object.freeze({ ...contract, success: union.type })
  const narrowerSuccess = Object.freeze({ ...contract, success: decodeError })
  const successCompatibility = InterfaceWitnessCompatibility.check(unionContract, narrowerSuccess)
  assert.strictEqual(successCompatibility._tag, 'Incompatible')
  if (successCompatibility._tag !== 'Incompatible') return
  assert.strictEqual(successCompatibility.problem._tag, 'Success')
})

it('admits safer witnesses and rejects unsafe witnesses for safe contracts', () => {
  const unsafeContract = Object.freeze({ ...contract, unsafe: true })
  const safeWitness = Object.freeze({ ...contract, unsafe: false })
  const unsafeWitness = Object.freeze({ ...contract, unsafe: true })

  assert.deepEqual(InterfaceWitnessCompatibility.check(unsafeContract, safeWitness), {
    _tag: 'Compatible',
  })
  const compatibility = InterfaceWitnessCompatibility.check(contract, unsafeWitness)
  assert.strictEqual(compatibility._tag, 'Incompatible')
  if (compatibility._tag !== 'Incompatible') return
  assert.deepEqual(compatibility.problem, { _tag: 'StrongerSafety' })
  assert.strictEqual(
    InterfaceWitnessCompatibility.describe(compatibility),
    'unsafe witness cannot satisfy a safe operation contract',
  )
})

it('subsumes generic failure types only when the witness forwards types promised by the contract', () => {
  const openContract = Object.freeze({
    ...contract,
    failures: Object.freeze([failureParameter]),
    requirementParameters: Object.freeze([requirementParameter]),
  })
  const matchingWitness = Object.freeze({
    ...contract,
    failures: Object.freeze([failureParameter]),
    requirements: Object.freeze([]),
    requirementParameters: Object.freeze([requirementParameter]),
  })
  assert.deepEqual(InterfaceWitnessCompatibility.check(openContract, matchingWitness), {
    _tag: 'Compatible',
  })

  const unknownFailure = Type.parameter({ module: 'test', name: 'Witness' }, 0, 'F')
  const failureCompatibility = InterfaceWitnessCompatibility.check(
    openContract,
    Object.freeze({
      ...matchingWitness,
      failures: Object.freeze([unknownFailure]),
    }),
  )
  assert.strictEqual(failureCompatibility._tag, 'Incompatible')
  if (failureCompatibility._tag !== 'Incompatible') return
  assert.deepEqual(failureCompatibility.problem, {
    _tag: 'Failure',
    failure: unknownFailure,
  })

  const unknownRequirement = Type.parameter(
    { module: 'test', name: 'Witness' },
    1,
    'S',
    'RequirementRow',
  )
  const requirementCompatibility = InterfaceWitnessCompatibility.check(
    openContract,
    Object.freeze({
      ...matchingWitness,
      requirementParameters: Object.freeze([unknownRequirement]),
    }),
  )
  assert.strictEqual(requirementCompatibility._tag, 'Incompatible')
  if (requirementCompatibility._tag !== 'Incompatible') return
  assert.deepEqual(requirementCompatibility.problem, {
    _tag: 'RequirementParameter',
    parameter: unknownRequirement,
  })
})

it('rejects a requirement capability or role absent from the promised row', () => {
  const missing = Object.freeze({
    ...contract,
    failures: Object.freeze([]),
    requirements: Object.freeze([
      Object.freeze({ capability: clock, role: 'OtherRole', access: 'Shared' as const }),
    ]),
  })
  const compatibility = InterfaceWitnessCompatibility.check(contract, missing)
  assert.strictEqual(compatibility._tag, 'Incompatible')
  if (compatibility._tag !== 'Incompatible') return
  assert.deepEqual(compatibility.problem, {
    _tag: 'Requirement',
    requirement: { capability: clock, role: 'OtherRole', access: 'Shared' },
  })
})

it('allows a take-capable value promise to satisfy a shared-borrow witness operand', () => {
  const valueContract = Object.freeze({
    ...contract,
    functionKind: 'Ordinary' as const,
    operands: Object.freeze([operand('value', schema)]),
    failures: Object.freeze([]),
    requirements: Object.freeze([]),
  })
  const borrowedWitness = Object.freeze({
    ...valueContract,
    operands: Object.freeze([
      operand('value', Type.reference('Shared', schema, Lifetime.staticLifetime)),
    ]),
  })

  assert.strictEqual(
    InterfaceWitnessCompatibility.check(valueContract, borrowedWitness)._tag,
    'Compatible',
  )
})

it('applies the Shared < Exclusive < Take order to every witness operand pair', () => {
  const accesses = ['Shared', 'Exclusive', 'Take'] as const
  const expected = [
    [true, true, true],
    [false, true, true],
    [false, false, true],
  ] as const
  const operandType = (access: (typeof accesses)[number]): Type.Type =>
    access === 'Take' ? schema : Type.reference(access, schema, Lifetime.staticLifetime)

  for (const [requiredOrdinal, required] of accesses.entries()) {
    for (const [suppliedOrdinal, supplied] of accesses.entries()) {
      const promised = Object.freeze({
        functionKind: 'Ordinary' as const,
        unsafe: false,
        operands: Object.freeze([operand('value', operandType(supplied))]),
        success: 'bool' as const,
        failures: Object.freeze([]),
        requirements: Object.freeze([]),
        requirementParameters: Object.freeze([]),
      })
      const implementation = Object.freeze({
        ...promised,
        operands: Object.freeze([operand('value', operandType(required))]),
      })
      assert.strictEqual(
        InterfaceWitnessCompatibility.check(promised, implementation)._tag === 'Compatible',
        expected.at(requiredOrdinal)?.at(suppliedOrdinal) ?? false,
      )
    }
  }
})
