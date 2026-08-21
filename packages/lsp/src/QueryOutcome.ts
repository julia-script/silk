import type * as IncidentId from './IncidentId.js'

/** Method-independent terminal result of one exact-revision semantic query. */
export type QueryOutcome<A> =
  | Ready<A>
  | Superseded
  | Canceled
  | DeadlineExceeded
  | AnalysisFailed
  | Unavailable
  | Closed

export interface Ready<A> {
  readonly _tag: 'Ready'
  readonly value: A
}

export interface Superseded {
  readonly _tag: 'Superseded'
}

export interface Canceled {
  readonly _tag: 'Canceled'
}

export interface DeadlineExceeded {
  readonly _tag: 'DeadlineExceeded'
}

export interface AnalysisFailed {
  readonly _tag: 'AnalysisFailed'
  readonly incident: IncidentId.IncidentId
}

export interface Unavailable {
  readonly _tag: 'Unavailable'
}

export interface Closed {
  readonly _tag: 'Closed'
}

export const ready = <A>(value: A): Ready<A> => Object.freeze({ _tag: 'Ready', value })
export const superseded: Superseded = Object.freeze({ _tag: 'Superseded' })
export const canceled: Canceled = Object.freeze({ _tag: 'Canceled' })
export const deadlineExceeded: DeadlineExceeded = Object.freeze({ _tag: 'DeadlineExceeded' })
export const analysisFailed = (incident: IncidentId.IncidentId): AnalysisFailed =>
  Object.freeze({ _tag: 'AnalysisFailed', incident })
export const unavailable: Unavailable = Object.freeze({ _tag: 'Unavailable' })
export const closed: Closed = Object.freeze({ _tag: 'Closed' })
