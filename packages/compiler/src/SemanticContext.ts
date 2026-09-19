import * as AuthoredIdentity from './AuthoredIdentity.js'
import type * as AuthoredHir from './AuthoredHir.js'
import type * as AuthoredLowering from './AuthoredLowering.js'
import type * as AuthoredPool from './AuthoredPool.js'
import * as AuthoredPresentation from './AuthoredPresentation.js'
import * as SourceSpan from './SourceSpan.js'

/**
 * The explicit context semantic consumers receive instead of source or syntax: the authored module,
 * pool-resolved text and the current revision's presentation for spans and evaluation order.
 * Nothing here can reconstruct meaning from source bytes; spans exist for diagnostics and navigation.
 */
export interface SemanticContext {
  readonly _tag: 'SemanticContext'
  readonly module: AuthoredHir.Module
  readonly presentation: AuthoredPresentation.Presentation
  /** Current-revision span of an authored position; total, falling back to the nearest presented ancestor. */
  readonly spanOf: (anchor: AuthoredIdentity.Anchor) => SourceSpan.SourceSpan
  /** Authored text behind a pool reference. */
  readonly textOf: (reference: AuthoredPool.TextRef) => string
  /** Authored bytes behind a pool reference. */
  readonly bytesOf: (reference: AuthoredPool.BytesRef) => Uint8Array
  /**
   * Document-order ordinal of an authored position within this module, for consumers that compare
   * evaluation order. Anchors without a presented entry take their nearest presented ancestor's ordinal.
   */
  readonly orderOf: (anchor: AuthoredIdentity.Anchor) => number
}

const contexts = new WeakMap<AuthoredLowering.Lowered, SemanticContext>()

const parentOwner = (owner: AuthoredIdentity.Identity): AuthoredIdentity.Identity | undefined =>
  owner.path.length === 0
    ? undefined
    : {
        _tag: 'AuthoredIdentity',
        namespace: owner.namespace,
        module: owner.module,
        path: owner.path.slice(0, -1),
      }

/** Local prefixes of an anchor, from the anchor itself to the owner root. */
const prefixes = (anchor: AuthoredIdentity.Anchor): ReadonlyArray<AuthoredIdentity.Anchor> => {
  const found: AuthoredIdentity.Anchor[] = []
  for (let length = anchor.path.length; length >= 0; length -= 1) {
    found.push({ _tag: 'AuthoredAnchor', owner: anchor.owner, path: anchor.path.slice(0, length) })
  }
  return found
}

/** One context per lowered module; the anchor index and document order are built once. */
export const make = (lowered: AuthoredLowering.Lowered): SemanticContext => {
  const cached = contexts.get(lowered)
  if (cached !== undefined) return cached
  const index = AuthoredPresentation.index(lowered.presentation)
  const orders = new Map<string, number>()
  // The first presented entry of each owner (its header) anchors everything the owner fails to present.
  const ownerEntries = new Map<string, AuthoredPresentation.Entry>()
  for (const [ordinal, entry] of lowered.presentation.entries.entries()) {
    const key = AuthoredIdentity.anchorKey(entry.anchor)
    if (!orders.has(key)) orders.set(key, ordinal)
    const owner = AuthoredIdentity.key(entry.anchor.owner)
    if (!ownerEntries.has(owner)) ownerEntries.set(owner, entry)
  }
  const moduleSpan =
    SourceSpan.fromOffsets(lowered.presentation.sourceId, 0, 0) ??
    (() => {
      throw new RangeError('Empty span rejected for a module')
    })()
  const resolve = (anchor: AuthoredIdentity.Anchor): AuthoredPresentation.Entry | undefined => {
    for (const candidate of prefixes(anchor)) {
      const entry = index.entry(candidate)
      if (entry !== undefined) return entry
    }
    for (
      let owner: AuthoredIdentity.Identity | undefined = anchor.owner;
      owner !== undefined;
      owner = parentOwner(owner)
    ) {
      const entry = ownerEntries.get(AuthoredIdentity.key(owner))
      if (entry !== undefined) return entry
    }
    return undefined
  }
  const pool = lowered.module.pool
  const built: SemanticContext = {
    _tag: 'SemanticContext',
    module: lowered.module,
    presentation: lowered.presentation,
    spanOf: (anchor) => {
      const entry = resolve(anchor)
      return entry === undefined
        ? moduleSpan
        : (SourceSpan.fromOffsets(
            lowered.presentation.sourceId,
            entry.span.start,
            entry.span.end,
          ) ?? moduleSpan)
    },
    textOf: (reference) => {
      const text = pool.texts[reference.index]
      if (text === undefined)
        throw new RangeError(`Text reference ${reference.index} is outside the module pool`)
      return text.value
    },
    bytesOf: (reference) => {
      const bytes = pool.bytes[reference.index]
      if (bytes === undefined)
        throw new RangeError(`Byte reference ${reference.index} is outside the module pool`)
      return Uint8Array.from(bytes.value)
    },
    orderOf: (anchor) => {
      const entry = resolve(anchor)
      return entry === undefined ? -1 : (orders.get(AuthoredIdentity.anchorKey(entry.anchor)) ?? -1)
    },
  }
  contexts.set(lowered, built)
  return built
}

/** The current-revision span of one authored node. */
export const spanOf = (self: SemanticContext, node: AuthoredHir.Node): SourceSpan.SourceSpan =>
  self.spanOf(node.anchor)

/** The spelling of an authored name, or `undefined` when the name is missing. */
export const nameText = (self: SemanticContext, name: AuthoredHir.Name): string | undefined => {
  if (name._tag === 'Name') return self.textOf(name.text)
  if (name._tag === 'InvalidName') return self.textOf(name.spelling)
  return undefined
}
