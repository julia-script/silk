import { assert, it } from '@effect/vitest'
import * as InterfaceWitnessCompatibility from '../src/InterfaceWitnessCompatibility.js'
import * as Type from '../src/Type.js'

const schema = Type.nominal('test', 'Schema')
const decodeError = Type.nominal('test', 'DecodeError')
const extraError = Type.nominal('test', 'ExtraError')
const clock = Type.nominal('test', 'Clock')
const logger = Type.nominal('test', 'Logger')

const operand = (
  name: string,
  type: Type.Type,
  receiver = false,
): InterfaceWitnessCompatibility.Operand => Object.freeze({ name, type, receiver })

const contract = Object.freeze({
  functionKind: 'Effect' as const,
  operands: Object.freeze([
    operand('self', Type.reference('Exclusive', schema), true),
    operand('encoded', 'i32'),
  ]),
  success: 'bool' as const,
  failures: Object.freeze([decodeError, extraError]),
  failureParameters: Object.freeze([]),
  requirements: Object.freeze([
    Object.freeze({ capability: clock, role: 'DefaultRole', access: 'Exclusive' as const }),
    Object.freeze({ capability: logger, role: 'DefaultRole', access: 'Shared' as const }),
  ]),
  requirementParameters: Object.freeze([]),
})

it('admits a pure witness with smaller rows and weaker receiver and requirement access', () => {
  const witness = Object.freeze({
    functionKind: 'Ordinary' as const,
    operands: Object.freeze([
      operand('self', Type.reference('Shared', schema), true),
      operand('encoded', 'i32'),
    ]),
    success: 'bool' as const,
    failures: Object.freeze([decodeError]),
    failureParameters: Object.freeze([]),
    requirements: Object.freeze([
      Object.freeze({ capability: clock, role: 'DefaultRole', access: 'Shared' as const }),
    ]),
    requirementParameters: Object.freeze([]),
  })

  assert.deepEqual(InterfaceWitnessCompatibility.check(contract, witness, 'Literal'), {
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
      operand('self', Type.reference('Shared', schema), true),
      operand('encoded', 'i32'),
    ]),
  })
  const witness = Object.freeze({
    functionKind: 'Effect' as const,
    operands: Object.freeze([
      operand('self', Type.reference('Exclusive', schema), true),
      operand('encoded', 'i32'),
    ]),
    success: 'bool' as const,
    failures: Object.freeze([Type.nominal('test', 'Unpromised')]),
    failureParameters: Object.freeze([]),
    requirements: Object.freeze([]),
    requirementParameters: Object.freeze([]),
  })

  const compatibility = InterfaceWitnessCompatibility.check(receiverContract, witness, 'Literal')
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
})

it('rejects stronger parameter, failure, and requirement demands deterministically', () => {
  const strongerParameter = Object.freeze({
    ...contract,
    operands: Object.freeze([
      operand('self', Type.reference('Shared', schema), true),
      operand('encoded', Type.reference('Exclusive', 'i32')),
    ]),
    failures: Object.freeze([]),
    requirements: Object.freeze([]),
  })
  const borrowedContract = Object.freeze({
    ...contract,
    operands: Object.freeze([
      operand('self', Type.reference('Shared', schema), true),
      operand('encoded', Type.reference('Shared', 'i32')),
    ]),
    failures: Object.freeze([]),
    requirements: Object.freeze([]),
  })
  const parameterCompatibility = InterfaceWitnessCompatibility.check(
    borrowedContract,
    strongerParameter,
    'Literal',
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
  const failureCompatibility = InterfaceWitnessCompatibility.check(
    contract,
    strongerRows,
    'Literal',
  )
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
    'Literal',
  )
  assert.strictEqual(requirementCompatibility._tag, 'Incompatible')
  if (requirementCompatibility._tag !== 'Incompatible') return
  assert.deepEqual(requirementCompatibility.problem, {
    _tag: 'StrongerRequirementAccess',
    requirement: { capability: logger, role: 'DefaultRole', access: 'Exclusive' },
    promised: ['Shared'],
  })
})

it('keeps the old value-to-shared-borrow convention explicit and removable', () => {
  const valueContract = Object.freeze({
    ...contract,
    functionKind: 'Ordinary' as const,
    operands: Object.freeze([operand('value', schema)]),
    failures: Object.freeze([]),
    requirements: Object.freeze([]),
  })
  const borrowedWitness = Object.freeze({
    ...valueContract,
    operands: Object.freeze([operand('value', Type.reference('Shared', schema))]),
  })

  assert.deepEqual(
    InterfaceWitnessCompatibility.check(valueContract, borrowedWitness, 'LegacySharedBorrow'),
    { _tag: 'Compatible' },
  )
  assert.strictEqual(
    InterfaceWitnessCompatibility.check(valueContract, borrowedWitness, 'Literal')._tag,
    'Incompatible',
  )
})
