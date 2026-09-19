import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'

/** A sibling's authored key, independent of source position and selected profile. */
export interface Key {
  readonly kind: string
  readonly name?: string
  readonly role?: string
}

/** Occurrences distinguish only siblings with the same complete authored key. */
export interface Segment extends Key {
  readonly _tag: 'OwnerSegment'
  readonly occurrence: number
}

/** Full logical identity remains authoritative even when content digests agree. */
export interface Identity {
  readonly _tag: 'AuthoredIdentity'
  readonly namespace: string
  readonly module: string
  readonly path: readonly Segment[]
}

/** A position in an owner's local authored structure, without source coordinates. */
export interface LocalSegment {
  readonly _tag: 'LocalSegment'
  readonly role: string
  readonly occurrence: number
}

export interface Anchor {
  readonly _tag: 'AuthoredAnchor'
  readonly owner: Identity
  readonly path: readonly LocalSegment[]
}

export class AuthoredIdentityError extends Data.TaggedError('AuthoredIdentityError')<{
  readonly operation: 'AuthoredIdentity.anchor'
  readonly reason: {
    readonly _tag: 'InvalidOwnerOccurrence' | 'InvalidLocalOccurrence'
    readonly index: number
    readonly occurrence: number
  }
}> {}

const segment = (key: Key, occurrence: number): Segment =>
  Object.freeze({
    _tag: 'OwnerSegment',
    kind: key.kind,
    ...(key.name === undefined ? {} : { name: key.name }),
    ...(key.role === undefined ? {} : { role: key.role }),
    occurrence,
  })

const copy = (self: Identity): Identity =>
  Object.freeze({
    _tag: 'AuthoredIdentity',
    namespace: self.namespace,
    module: self.module,
    path: Object.freeze(self.path.map((part) => segment(part, part.occurrence))),
  })

/** Create a module owner from a logical namespace and resolver-canonical module name. */
export const module = (namespace: string, module: string): Identity =>
  Object.freeze({ _tag: 'AuthoredIdentity', namespace, module, path: Object.freeze([]) })

/**
 * Assign sibling identities in authored order. Inserting or moving a differently named
 * sibling cannot renumber another key; indistinguishable siblings remain conservative.
 * This allocation does not decide source-language namespace legality.
 */
export const children = (parent: Identity, keys: readonly Key[]): readonly Identity[] => {
  const owner = copy(parent)
  const occurrences = new Map<string, number>()
  return Object.freeze(
    keys.map((key): Identity => {
      const encoded = JSON.stringify([key.kind, key.name ?? null, key.role ?? null])
      const occurrence = occurrences.get(encoded) ?? 0
      occurrences.set(encoded, occurrence + 1)
      return Object.freeze({
        _tag: 'AuthoredIdentity',
        namespace: owner.namespace,
        module: owner.module,
        path: Object.freeze([...owner.path, segment(key, occurrence)]),
      })
    }),
  )
}

/** Compare complete owner identity, without relying on a digest or object identity. */
export const equals = (self: Identity, other: Identity): boolean =>
  self.namespace === other.namespace &&
  self.module === other.module &&
  self.path.length === other.path.length &&
  self.path.every((part, index) => {
    const candidate = other.path[index]
    return (
      candidate !== undefined &&
      part.kind === candidate.kind &&
      part.name === candidate.name &&
      part.role === candidate.role &&
      part.occurrence === candidate.occurrence
    )
  })

/** A string key for one owner identity: equal exactly when `equals` holds, usable in maps. */
export const key = (self: Identity): string =>
  `${self.namespace}:${self.module}${self.path
    .map(
      (part) =>
        `/${part.kind}${part.name === undefined ? '' : `=${part.name}`}${part.role === undefined ? '' : `@${part.role}`}#${part.occurrence}`,
    )
    .join('')}`

/** A string key for one owner-local anchor, usable in maps keyed by authored position. */
export const anchorKey = (self: Anchor): string =>
  `${key(self.owner)}${self.path.map((part) => `|${part.role}#${part.occurrence}`).join('')}`

/** Copy and freeze a local anchor, rejecting invalid occurrence numbers as typed failures. */
export const anchor = Effect.fn('AuthoredIdentity.anchor')(function* (
  owner: Identity,
  path: readonly { readonly role: string; readonly occurrence: number }[],
): Effect.fn.Return<Anchor, AuthoredIdentityError> {
  for (const [index, part] of owner.path.entries()) {
    if (!Number.isSafeInteger(part.occurrence) || part.occurrence < 0) {
      return yield* new AuthoredIdentityError({
        operation: 'AuthoredIdentity.anchor',
        reason: { _tag: 'InvalidOwnerOccurrence', index, occurrence: part.occurrence },
      })
    }
  }
  for (const [index, part] of path.entries()) {
    if (!Number.isSafeInteger(part.occurrence) || part.occurrence < 0) {
      return yield* new AuthoredIdentityError({
        operation: 'AuthoredIdentity.anchor',
        reason: { _tag: 'InvalidLocalOccurrence', index, occurrence: part.occurrence },
      })
    }
  }
  return Object.freeze({
    _tag: 'AuthoredAnchor',
    owner: copy(owner),
    path: Object.freeze(
      path.map((part): LocalSegment =>
        Object.freeze({ _tag: 'LocalSegment', role: part.role, occurrence: part.occurrence }),
      ),
    ),
  })
})
