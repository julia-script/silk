/** Exact evaluator host values and deterministic providers for Unix-epoch time. */

const minimumI64 = -(1n << 63n)
const maximumI64 = (1n << 63n) - 1n
const maximumU64 = (1n << 64n) - 1n
const nanosecondsPerSecond = 1_000_000_000n

/** One canonical split-second clock value. */
export interface Instant {
  readonly seconds: bigint
  readonly nanoseconds: bigint
}

/** One explicit host-boundary failure. */
export interface BoundaryFailure {
  readonly _tag: 'BoundaryFailure'
  readonly message: string
}

/** The result of reading a system clock. */
export type ReadResult = { readonly _tag: 'Read'; readonly instant: Instant } | BoundaryFailure

/** The result of querying a clock's whole-nanosecond resolution. */
export type ResolutionResult =
  | { readonly _tag: 'Resolution'; readonly nanoseconds: bigint }
  | BoundaryFailure

/** The explicit system-clock boundary injected into evaluation. */
export interface Provider {
  readonly now: () => ReadResult
  readonly resolution: () => ResolutionResult
}

/** A validated fixed system-clock provider. */
export interface Fixed {
  readonly _tag: 'SystemClockFixed'
  readonly provider: Provider
}

/** Why a deterministic host could not be constructed. */
export type ConstructionFailureReason =
  | { readonly _tag: 'SecondsOutOfRange'; readonly value: bigint }
  | { readonly _tag: 'NanosecondsOutOfRange'; readonly value: bigint }
  | { readonly _tag: 'ResolutionOutOfRange'; readonly value: bigint }

/** A construction result that never truncates, wraps, or throws for invalid clock data. */
export type Construction<A> =
  | { readonly _tag: 'Constructed'; readonly value: A }
  | { readonly _tag: 'ConstructionFailure'; readonly reason: ConstructionFailureReason }

const freezeInstant = (value: Instant): Instant =>
  Object.freeze({ seconds: value.seconds, nanoseconds: value.nanoseconds })

const instantFailure = (value: Instant): ConstructionFailureReason | undefined => {
  if (value.seconds < minimumI64 || value.seconds > maximumI64) {
    return Object.freeze({ _tag: 'SecondsOutOfRange', value: value.seconds })
  }
  if (value.nanoseconds < 0n || value.nanoseconds >= nanosecondsPerSecond) {
    return Object.freeze({ _tag: 'NanosecondsOutOfRange', value: value.nanoseconds })
  }
  return undefined
}

const resolutionFailure = (value: bigint): ConstructionFailureReason | undefined =>
  value < 1n || value > maximumU64
    ? Object.freeze({ _tag: 'ResolutionOutOfRange', value })
    : undefined

/** Tests whether a value fits Silk's canonical `Instant` scalar ranges. */
export const isInstant = (value: Instant): boolean => instantFailure(value) === undefined

/** Tests whether a value fits Silk's positive whole-nanosecond resolution range. */
export const isResolution = (value: bigint): boolean => resolutionFailure(value) === undefined

/** Builds a deterministic fixed provider after exact range validation. */
export const fixed = (value: Instant, resolution: bigint): Construction<Fixed> => {
  const invalidInstant = instantFailure(value)
  if (invalidInstant !== undefined) {
    return Object.freeze({ _tag: 'ConstructionFailure', reason: invalidInstant })
  }
  const invalidResolution = resolutionFailure(resolution)
  if (invalidResolution !== undefined) {
    return Object.freeze({ _tag: 'ConstructionFailure', reason: invalidResolution })
  }
  const instant = freezeInstant(value)
  const provider: Provider = Object.freeze({
    now: () => Object.freeze({ _tag: 'Read', instant }),
    resolution: () => Object.freeze({ _tag: 'Resolution', nanoseconds: resolution }),
  })
  return Object.freeze({
    _tag: 'Constructed',
    value: Object.freeze({ _tag: 'SystemClockFixed', provider }),
  })
}

/** Builds a provider that reports an explicit boundary failure for every operation. */
export const failing = (message = 'system clock host failed'): Provider => {
  const failure: BoundaryFailure = Object.freeze({ _tag: 'BoundaryFailure', message })
  return Object.freeze({ now: () => failure, resolution: () => failure })
}
