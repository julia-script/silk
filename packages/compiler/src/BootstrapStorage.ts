import type { AggregateValue, Value } from './BootstrapValue.js'
import type * as CleanupPlan from './CleanupPlan.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as ExecutionPackage from './ExecutionPackage.js'
import * as Type from './Type.js'
import * as WakeCell from './WakeCell.js'

export interface Allocation {
  active: boolean
  readonly values: Map<string, Value>
  shared?: {
    readonly element: Type.Type
    readonly provenance: string
    value: Value
    strong: bigint
    access: 'Available' | 'Active'
  }
  execution?: {
    readonly provenance: string
    state:
      | 'Initial'
      | 'InitialReady'
      | 'Running'
      | 'Dormant'
      | 'Notifying'
      | 'Eligible'
      | 'Completed'
      | 'Destroyed'
    readonly body: Value
    readonly endpoint: Value
    readonly callback: Value
    readonly bodyCleanup: CleanupPlan.CleanupPlan
    readonly endpointCleanup: CleanupPlan.CleanupPlan
    readonly callbackCleanup: CleanupPlan.CleanupPlan
    wake?: WakeCell.State
    guard?: {
      readonly value: Value
      readonly cleanup: CleanupPlan.CleanupPlan
    }
    cleanupPending?: boolean
    /** Stable root depth restored for every activation of this package. */
    logicalDepth?: number
  }
}

export type Allocations = Map<number, Allocation>
export type ExecutionState = NonNullable<Allocation['execution']>

/** Acquires one empty evaluator allocation ticket. */
export const allocate = (allocations: Allocations, ticket: number): void => {
  allocations.set(ticket, { active: true, values: new Map() })
}

/** Atomically installs one initialized local-shared core into a live empty allocation. */
export const initializeShared = (
  allocations: Allocations,
  ticket: number,
  element: Type.Type,
  provenance: string,
  value: Value,
): boolean => {
  const allocation = allocations.get(ticket)
  if (allocation === undefined || !allocation.active || allocation.shared !== undefined)
    return false
  allocation.shared = { element, provenance, value, strong: 1n, access: 'Available' }
  return true
}

/** Atomically installs every exact package owner without running the body. */
export const initializeExecution = (
  allocations: Allocations,
  ticket: number,
  plan: ExecutionPackage.Plan,
  body: Value,
  endpoint: Value,
  callback: Value,
  cleanup: {
    readonly body: CleanupPlan.CleanupPlan
    readonly endpoint: CleanupPlan.CleanupPlan
    readonly callback: CleanupPlan.CleanupPlan
  },
): boolean => {
  const allocation = allocations.get(ticket)
  if (
    allocation === undefined ||
    !allocation.active ||
    allocation.execution !== undefined ||
    allocation.shared !== undefined ||
    allocation.values.size > 0
  )
    return false
  allocation.execution = {
    provenance: plan.provenance,
    state: 'Initial',
    body,
    endpoint,
    callback,
    bodyCleanup: cleanup.body,
    endpointCleanup: cleanup.endpoint,
    callbackCleanup: cleanup.callback,
    ...(plan.readinessStorage ? { wake: WakeCell.initial() } : {}),
  }
  return true
}

export const execution = (allocations: Allocations, ticket: number): Allocation['execution'] =>
  allocations.get(ticket)?.execution

/** Inspects evaluator-owned opaque state without publishing storage lanes to Silk source. */
export const shared = (allocations: Allocations, ticket: number): Allocation['shared'] =>
  allocations.get(ticket)?.shared

/** Applies a successful strong clone without exposing the count to source. */
export const cloneShared = (allocations: Allocations, ticket: number, maximum: bigint): boolean => {
  const state = shared(allocations, ticket)
  if (state === undefined || state.strong >= maximum) return false
  state.strong += 1n
  return true
}

/** Releases one live allocation and optionally clears its initialized slots. */
export const release = (allocations: Allocations, ticket: number, clear: boolean): boolean => {
  const allocation = allocations.get(ticket)
  if (allocation === undefined || !allocation.active) return false
  allocation.active = false
  if (clear) allocation.values.clear()
  return true
}

/** Reads one initialized slot from live raw storage. */
export const read = (
  allocations: Allocations,
  ticket: number,
  index: bigint,
): Value | undefined => {
  const allocation = allocations.get(ticket)
  return allocation === undefined || !allocation.active
    ? undefined
    : allocation.values.get(index.toString())
}

/** Writes one value only when the raw slot is live and uninitialized. */
export const write = (
  allocations: Allocations,
  ticket: number,
  index: bigint,
  value: Value,
): boolean => {
  const allocation = allocations.get(ticket)
  const key = index.toString()
  if (allocation === undefined || !allocation.active || allocation.values.has(key)) return false
  allocation.values.set(key, value)
  return true
}

/** Takes one initialized raw slot, leaving it uninitialized. */
export const take = (
  allocations: Allocations,
  ticket: number,
  index: bigint,
): Value | undefined => {
  const allocation = allocations.get(ticket)
  const key = index.toString()
  const selected = allocation?.values.get(key)
  if (allocation === undefined || !allocation.active || selected === undefined) return undefined
  allocation.values.delete(key)
  return selected
}

/** Removes one initialized slot after its value has been released. */
export const drop = (allocations: Allocations, ticket: number, index: bigint): boolean => {
  const allocation = allocations.get(ticket)
  if (allocation === undefined || !allocation.active) return false
  return allocation.values.delete(index.toString())
}

/** Initializes a byte range of one live raw allocation. */
export const fill = (
  allocations: Allocations,
  ticket: number,
  offset: bigint,
  length: number,
  value: Value,
): boolean => {
  const allocation = allocations.get(ticket)
  if (allocation === undefined || !allocation.active) return false
  for (let index = 0; index < length; index += 1)
    allocation.values.set(String(offset + BigInt(index)), value)
  return true
}

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
  if (cleanup._tag === 'LocalSharedCoreCleanup')
    return Object.freeze([cleanup.type, cleanup.element])
  if (cleanup._tag === 'ExecutionCleanup') return Object.freeze([cleanup.type])
  if (cleanup._tag === 'WakeCleanup') return Object.freeze([cleanup.type])
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
  path: ReadonlyArray<DeclarationFacts.FieldId>,
): Value => {
  let selected: Value = root
  for (const selector of path) {
    if (selected._tag !== 'AggregateValue')
      throw new RangeError('MIR verifier allowed a match field below a non-struct value')
    const field: AggregateValue['fields'][number] | undefined = selected.fields.find((candidate) =>
      DeclarationFacts.sameFieldId(candidate.field, selector),
    )
    if (field === undefined) throw new RangeError('MIR verifier allowed a missing match field')
    selected = field.value
  }
  return selected
}
