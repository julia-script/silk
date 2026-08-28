/** Explicit evaluator host values and deterministic providers for secure random bytes. */

/** Closed, non-sensitive reasons a random host call could not complete. */
export type FailureCategory =
  | 'ExplicitFailure'
  | 'Exhausted'
  | 'Underfill'
  | 'Overfill'
  | 'InvalidByte'
  | 'HostThrew'

const failureCategorySet: ReadonlySet<unknown> = new Set<FailureCategory>([
  'ExplicitFailure',
  'Exhausted',
  'Underfill',
  'Overfill',
  'InvalidByte',
  'HostThrew',
])

/** Tests an arbitrary host value against the complete non-sensitive failure vocabulary. */
export const isFailureCategory = (value: unknown): value is FailureCategory =>
  failureCategorySet.has(value)

/** One normalized host-boundary failure. No arbitrary payload crosses this boundary. */
export interface BoundaryFailure {
  readonly _tag: 'BoundaryFailure'
  readonly category: FailureCategory
}

/** One candidate byte sequence returned by an injected host. */
export interface Filled {
  readonly _tag: 'Filled'
  readonly bytes: ReadonlyArray<number>
}

/** The result of one exact random-fill request. */
export type FillResult = Filled | BoundaryFailure

/** The explicit random boundary injected into bootstrap evaluation. */
export interface Provider {
  readonly fill: (length: number) => FillResult
}

/** A validated per-call deterministic provider for evaluator tests. */
export interface Scripted {
  readonly _tag: 'RandomHostScripted'
  readonly provider: Provider
  readonly remaining: () => number
}

/** Why a scripted host could not be constructed. */
export interface ConstructionFailureReason {
  readonly _tag: 'InvalidByte'
  readonly chunk: number
  readonly offset: number
}

/** A construction result that never truncates, wraps, or throws for invalid bytes. */
export type Construction =
  | { readonly _tag: 'Constructed'; readonly value: Scripted }
  | { readonly _tag: 'ConstructionFailure'; readonly reason: ConstructionFailureReason }

const failure = (category: FailureCategory): BoundaryFailure =>
  Object.freeze({ _tag: 'BoundaryFailure', category })

/** Builds a deterministic provider with one immutable chunk per fill call. */
export const scripted = (chunks: ReadonlyArray<ReadonlyArray<number>>): Construction => {
  const copied: Array<ReadonlyArray<number>> = []
  for (const [chunk, bytes] of chunks.entries()) {
    for (const [offset, byte] of bytes.entries()) {
      if (!Number.isInteger(byte) || byte < 0 || byte > 255) {
        return Object.freeze({
          _tag: 'ConstructionFailure',
          reason: Object.freeze({ _tag: 'InvalidByte', chunk, offset }),
        })
      }
    }
    copied.push(Object.freeze(Array.from(bytes)))
  }
  let index = 0
  const provider: Provider = Object.freeze({
    fill: (length: number) => {
      const bytes = copied.at(index)
      if (bytes === undefined) return failure('Exhausted')
      index += 1
      if (bytes.length < length) return failure('Underfill')
      if (bytes.length > length) return failure('Overfill')
      return Object.freeze({ _tag: 'Filled', bytes })
    },
  })
  return Object.freeze({
    _tag: 'Constructed',
    value: Object.freeze({
      _tag: 'RandomHostScripted',
      provider,
      remaining: () => copied.length - index,
    }),
  })
}

/** Builds a provider that reports a normalized explicit failure for every request. */
export const failing = (): Provider => Object.freeze({ fill: () => failure('ExplicitFailure') })
