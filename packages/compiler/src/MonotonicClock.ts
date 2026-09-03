/** Exact evaluator host values and deterministic providers for monotonic time. */

/** One canonical split-second monotonic mark. */
export interface Instant {
  readonly seconds: bigint
  readonly nanoseconds: bigint
}

/** One explicit host-boundary failure. */
export interface BoundaryFailure {
  readonly _tag: 'BoundaryFailure'
  readonly message: string
}

/** The result of reading the monotonic clock. */
export type ReadResult = { readonly _tag: 'Read'; readonly instant: Instant } | BoundaryFailure

/** The result of querying the clock's whole-nanosecond resolution. */
export type ResolutionResult =
  | { readonly _tag: 'Resolution'; readonly nanoseconds: bigint }
  | BoundaryFailure

/** One completed deterministic absolute wait. */
export interface WaitEvent {
  readonly _tag: 'WaitUntil'
  readonly deadline: Instant
  readonly before: Instant
  readonly after: Instant
}

/** The result of completing an absolute wait. */
export type WaitResult = { readonly _tag: 'Waited' } | BoundaryFailure

/** The explicit monotonic-clock boundary injected into evaluation. */
export interface Provider {
  readonly now: () => ReadResult
  readonly resolution: () => ResolutionResult
  readonly waitUntil: (deadline: Instant) => WaitResult
}

/** A validated scripted monotonic provider with immutable observation snapshots. */
export interface Scripted {
  readonly _tag: 'MonotonicClockScripted'
  readonly provider: Provider
  readonly current: () => Instant
  readonly waits: () => ReadonlyArray<WaitEvent>
}

/** Why a scripted monotonic host could not be constructed. */
export type ConstructionFailureReason =
  | { readonly _tag: 'SecondsOutOfRange'; readonly value: bigint }
  | { readonly _tag: 'NanosecondsOutOfRange'; readonly value: bigint }
  | { readonly _tag: 'ResolutionOutOfRange'; readonly value: bigint }
  | { readonly _tag: 'EmptyScript' }
  | {
      readonly _tag: 'DecreasingScript'
      readonly index: number
      readonly previous: Instant
      readonly current: Instant
    }

/** A construction result that returns invalid timeline data explicitly. */
export type Construction =
  | { readonly _tag: 'Constructed'; readonly value: Scripted }
  | { readonly _tag: 'ConstructionFailure'; readonly reason: ConstructionFailureReason }

const compare = (left: Instant, right: Instant): number => {
  if (left.seconds < right.seconds) return -1
  if (left.seconds > right.seconds) return 1
  if (left.nanoseconds < right.nanoseconds) return -1
  if (left.nanoseconds > right.nanoseconds) return 1
  return 0
}

const instant = (value: Instant): Instant =>
  Object.freeze({ seconds: value.seconds, nanoseconds: value.nanoseconds })

const invalidInstant = (value: Instant): ConstructionFailureReason | undefined => {
  if (value.seconds < -(1n << 63n) || value.seconds > (1n << 63n) - 1n) {
    return Object.freeze({ _tag: 'SecondsOutOfRange', value: value.seconds })
  }
  if (value.nanoseconds < 0n || value.nanoseconds >= 1_000_000_000n) {
    return Object.freeze({ _tag: 'NanosecondsOutOfRange', value: value.nanoseconds })
  }
  return undefined
}

/** Tests whether a value fits Silk's canonical monotonic-mark scalar ranges. */
export const isInstant = (value: Instant): boolean => invalidInstant(value) === undefined

/** Tests whether a value fits Silk's positive whole-nanosecond resolution range. */
export const isResolution = (value: bigint): boolean => value >= 1n && value <= (1n << 64n) - 1n

/** Builds a deterministic provider from a non-decreasing sequence of exact marks. */
export const scripted = (values: ReadonlyArray<Instant>, resolution: bigint): Construction => {
  if (values.length === 0) {
    return Object.freeze({
      _tag: 'ConstructionFailure',
      reason: Object.freeze({ _tag: 'EmptyScript' }),
    })
  }
  const marks: Array<Instant> = []
  for (const [index, value] of values.entries()) {
    const failure = invalidInstant(value)
    if (failure !== undefined) {
      return Object.freeze({ _tag: 'ConstructionFailure', reason: failure })
    }
    const copied = instant(value)
    const previous = marks.at(-1)
    if (previous !== undefined && compare(copied, previous) < 0) {
      return Object.freeze({
        _tag: 'ConstructionFailure',
        reason: Object.freeze({
          _tag: 'DecreasingScript',
          index,
          previous,
          current: copied,
        }),
      })
    }
    marks.push(copied)
  }
  if (!isResolution(resolution)) {
    return Object.freeze({
      _tag: 'ConstructionFailure',
      reason: Object.freeze({ _tag: 'ResolutionOutOfRange', value: resolution }),
    })
  }

  const first = marks.at(0)
  if (first === undefined) {
    return Object.freeze({
      _tag: 'ConstructionFailure',
      reason: Object.freeze({ _tag: 'EmptyScript' }),
    })
  }
  let selected: Instant = first
  let index = 0
  const recorded: Array<WaitEvent> = []
  const provider: Provider = Object.freeze({
    now: () => {
      const next = marks.at(index)
      if (next !== undefined) {
        index += 1
        if (compare(next, selected) > 0) selected = next
      }
      return Object.freeze({ _tag: 'Read', instant: selected })
    },
    resolution: () => Object.freeze({ _tag: 'Resolution', nanoseconds: resolution }),
    waitUntil: (deadline: Instant) => {
      const before = selected
      const canonicalDeadline = instant(deadline)
      if (compare(canonicalDeadline, selected) > 0) selected = canonicalDeadline
      recorded.push(
        Object.freeze({
          _tag: 'WaitUntil',
          deadline: canonicalDeadline,
          before,
          after: selected,
        }),
      )
      return Object.freeze({ _tag: 'Waited' })
    },
  })
  const value: Scripted = Object.freeze({
    _tag: 'MonotonicClockScripted',
    provider,
    current: () => instant(selected),
    waits: () => Object.freeze(recorded.map((event) => event)),
  })
  return Object.freeze({ _tag: 'Constructed', value })
}

/** Builds a provider that reports an explicit boundary failure for every operation. */
export const failing = (message = 'monotonic clock host failed'): Provider => {
  const failure: BoundaryFailure = Object.freeze({
    _tag: 'BoundaryFailure',
    message,
  })
  return Object.freeze({
    now: () => failure,
    resolution: () => failure,
    waitUntil: () => failure,
  })
}
