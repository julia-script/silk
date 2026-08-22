/** The source-tree observations accumulated while waiting for a writer to finish. */
export interface SourceSettlement {
  readonly fingerprint: string
  readonly stableSamples: number
}

export type Observation =
  | { readonly _tag: 'Pending'; readonly settlement: SourceSettlement }
  | { readonly _tag: 'Settled'; readonly fingerprint: string }

const confirmationsRequired = 2

export const make = (fingerprint: string): SourceSettlement => ({
  fingerprint,
  stableSamples: 0,
})

/** Settles only after two later observations confirm the same source-tree fingerprint. */
export const observe = (self: SourceSettlement, fingerprint: string): Observation => {
  if (fingerprint !== self.fingerprint) {
    return { _tag: 'Pending', settlement: make(fingerprint) }
  }
  const stableSamples = self.stableSamples + 1
  return stableSamples < confirmationsRequired
    ? { _tag: 'Pending', settlement: { fingerprint, stableSamples } }
    : { _tag: 'Settled', fingerprint }
}
