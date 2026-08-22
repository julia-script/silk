import type { AggregateValue, Value } from './BootstrapValue.js'
import type * as CleanupPlan from './CleanupPlan.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Type from './Type.js'

/** Lists the semantic owners visited by one concrete cleanup execution. */
export const cleanupMembers = (
  cleanup: CleanupPlan.CleanupPlan,
  owner: Value,
): ReadonlyArray<Type.Type> => {
  if (cleanup._tag === 'NoCleanup' || cleanup._tag === 'ParameterCleanup') return Object.freeze([])
  if (cleanup._tag === 'AllocationCleanup') return Object.freeze([Type.allocation])
  if (cleanup._tag === 'UnionCleanup') {
    if (owner._tag !== 'UnionValue') return Object.freeze([])
    const active = cleanup.cases.find((candidate) => Type.equals(candidate.member, owner.member))
    return Object.freeze([
      owner.member,
      ...(active === undefined ? [] : cleanupMembers(active.cleanup, owner.payload)),
    ])
  }
  if (cleanup._tag === 'ArrayCleanup')
    return owner._tag === 'ArrayValue'
      ? Object.freeze(owner.elements.flatMap((element) => cleanupMembers(cleanup.element, element)))
      : Object.freeze([])
  if (cleanup._tag === 'CallableCleanup') {
    if (owner._tag !== 'CallableValue') return Object.freeze([])
    return Object.freeze(
      cleanup.slots.flatMap((slot) => {
        const capture = owner.captures.find((candidate) => candidate.ordinal === slot.ordinal)
        return capture === undefined ? [] : cleanupMembers(slot.cleanup, capture.value)
      }),
    )
  }
  if (cleanup._tag === 'EffectCleanup') {
    if (owner._tag !== 'EffectValue') return Object.freeze([])
    return Object.freeze(
      cleanup.slots.flatMap((slot) => {
        const capture = owner.captures.at(slot.ordinal)
        return capture === undefined ? [] : cleanupMembers(slot.cleanup, capture)
      }),
    )
  }
  if (cleanup._tag === 'EffectCompositeCleanup') {
    if (owner._tag !== 'EffectCompositeValue') return Object.freeze([])
    const selected = cleanup.alternatives.at(owner.alternative)
    return selected === undefined ? Object.freeze([]) : cleanupMembers(selected, owner.effect)
  }
  if (cleanup._tag === 'RawBufferCleanup') return Object.freeze([cleanup.type])
  if (cleanup._tag === 'HookCleanup') return cleanupMembers(cleanup.inner, owner)
  if (cleanup._tag === 'RepresentedCallableCleanup' || cleanup._tag === 'RepresentedEffectCleanup')
    return Object.freeze([])
  if (owner._tag !== 'AggregateValue') return Object.freeze([])
  return Object.freeze(
    cleanup.fields.flatMap((field) => {
      const value = owner.fields.find(
        (candidate) => candidate.field.ordinal === field.field.ordinal,
      )
      return value === undefined ? [] : cleanupMembers(field.cleanup, value.value)
    }),
  )
}

/** Selects a declaration field path from one checked aggregate value. */
export const selectFieldPath = (
  root: Value,
  path: ReadonlyArray<DeclarationIndex.FieldId>,
): Value => {
  let selected: Value = root
  for (const selector of path) {
    if (selected._tag !== 'AggregateValue')
      throw new RangeError('MIR verifier allowed a match field below a non-struct value')
    const field: AggregateValue['fields'][number] | undefined = selected.fields.find(
      (candidate) =>
        candidate.field.ordinal === selector.ordinal &&
        candidate.field.struct.sourceId === selector.struct.sourceId &&
        candidate.field.struct.ordinal === selector.struct.ordinal,
    )
    if (field === undefined) throw new RangeError('MIR verifier allowed a missing match field')
    selected = field.value
  }
  return selected
}
