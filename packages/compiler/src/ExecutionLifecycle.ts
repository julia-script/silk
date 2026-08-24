import type * as DeclarationIndex from './DeclarationIndex.js'
import * as ExecutionAffinity from './ExecutionAffinity.js'
import * as Type from './Type.js'

/** Target-neutral logical states; backends remain free to fuse their physical tags. */
export type State =
  | 'Initial'
  | 'Running'
  | 'Dormant'
  | 'Notifying'
  | 'Eligible'
  | 'Completed'
  | 'Destroyed'

export type Event =
  | 'Drive'
  | 'Park'
  | 'BeginNotification'
  | 'FinishNotification'
  | 'Complete'
  | 'Drop'

export type Transition =
  | { readonly _tag: 'Transition'; readonly state: State }
  | { readonly _tag: 'FatalIntrinsicStateTrap'; readonly state: State; readonly event: Event }
  | { readonly _tag: 'OwnershipRejected'; readonly state: State; readonly event: Event }

export interface Fact {
  readonly _tag: 'ExecutionSemanticFact'
  readonly identity: 'Intrinsic.Execution'
  readonly result: Type.Type
  readonly affine: true
  readonly copy: false
  readonly threadTransfer: false
  readonly affinity: ExecutionAffinity.ExecutionAffinity
  readonly initial: 'Initial'
  readonly states: ReadonlyArray<State>
  readonly loans: {
    readonly externalConstruction: 'Rejected'
    readonly internalStable: 'MayCrossParking'
    readonly cleanup: 'LoanBeforeReferent'
    readonly completionBorrow: 'Rejected'
  }
  readonly localShared: {
    readonly ownedStrongHandle: 'PreservedAcrossParking'
    readonly activeAccess: 'RejectParking'
  }
}

export type FactResult =
  | { readonly _tag: 'Available'; readonly fact: Fact }
  | { readonly _tag: 'Unavailable'; readonly reason: 'NotExecution' | 'UnavailableResult' }

export const states: ReadonlyArray<State> = Object.freeze([
  'Initial',
  'Running',
  'Dormant',
  'Notifying',
  'Eligible',
  'Completed',
  'Destroyed',
])

/** Publishes representation-free ownership and lifecycle semantics for one sealed specialization. */
export const ofType = (index: DeclarationIndex.Index, type: Type.Type): FactResult => {
  if (!Type.isExecution(type)) return Object.freeze({ _tag: 'Unavailable', reason: 'NotExecution' })
  const result = type.arguments.at(0)
  if (result === undefined || !Type.isTypeArgument(result) || !Type.runtimeAvailable(result))
    return Object.freeze({ _tag: 'Unavailable', reason: 'UnavailableResult' })
  return Object.freeze({
    _tag: 'Available',
    fact: Object.freeze({
      _tag: 'ExecutionSemanticFact',
      identity: 'Intrinsic.Execution',
      result,
      affine: true,
      copy: false,
      threadTransfer: false,
      affinity: ExecutionAffinity.ofType(index, type),
      initial: 'Initial',
      states,
      loans: Object.freeze({
        externalConstruction: 'Rejected',
        internalStable: 'MayCrossParking',
        cleanup: 'LoanBeforeReferent',
        completionBorrow: 'Rejected',
      }),
      localShared: Object.freeze({
        ownedStrongHandle: 'PreservedAcrossParking',
        activeAccess: 'RejectParking',
      }),
    }),
  })
}

/** Applies the owner-neutral logical transition contract without selecting storage. */
export const transition = (state: State, event: Event): Transition => {
  if (event === 'Drive') {
    if (state === 'Initial' || state === 'Eligible')
      return Object.freeze({ _tag: 'Transition', state: 'Running' })
    if (state === 'Dormant' || state === 'Notifying')
      return Object.freeze({ _tag: 'FatalIntrinsicStateTrap', state, event })
    return Object.freeze({ _tag: 'OwnershipRejected', state, event })
  }
  if (event === 'Park' && state === 'Running')
    return Object.freeze({ _tag: 'Transition', state: 'Dormant' })
  if (event === 'BeginNotification' && state === 'Dormant')
    return Object.freeze({ _tag: 'Transition', state: 'Notifying' })
  if (event === 'FinishNotification' && state === 'Notifying')
    return Object.freeze({ _tag: 'Transition', state: 'Eligible' })
  if (event === 'Complete' && state === 'Running')
    return Object.freeze({ _tag: 'Transition', state: 'Completed' })
  if (event === 'Drop' && (state === 'Initial' || state === 'Dormant' || state === 'Eligible'))
    return Object.freeze({ _tag: 'Transition', state: 'Destroyed' })
  return Object.freeze({ _tag: 'OwnershipRejected', state, event })
}

export const encode = (self: Fact): string =>
  `${self.identity}<${Type.encode(self.result)}> affine=yes copy=no transfer=no affinity=${ExecutionAffinity.encode(self.affinity)} initial=${self.initial} states=${self.states.join(',')} loans=${self.loans.externalConstruction}/${self.loans.internalStable}/${self.loans.cleanup}/${self.loans.completionBorrow} shared=${self.localShared.ownedStrongHandle}/${self.localShared.activeAccess}`
