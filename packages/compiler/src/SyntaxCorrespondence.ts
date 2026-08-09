import * as Option from 'effect/Option'
import * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'

/** One previous/current canonical identity pair established by exact structural evidence. */
export interface IdentityPair {
  readonly previous: SyntaxFile.SyntaxId
  readonly current: SyntaxFile.SyntaxId
}

/** Immutable evidence about the amount of exact syntax correspondence found. */
export interface Counts {
  readonly previousElements: number
  readonly currentElements: number
  readonly correspondingElements: number
}

/** A conservative bidirectional relation between adjacent syntax artifacts of one logical source. */
export interface SyntaxCorrespondence {
  readonly _tag: 'SyntaxCorrespondence'
  readonly previous: SyntaxFile.SyntaxFile
  readonly current: SyntaxFile.SyntaxFile
  readonly identities: ReadonlyArray<IdentityPair>
  readonly counts: Counts
}

interface Indexes {
  readonly previousToCurrent: ReadonlyMap<SyntaxTree.Element, SyntaxTree.Element>
  readonly currentToPrevious: ReadonlyMap<SyntaxTree.Element, SyntaxTree.Element>
}

const indexes = new WeakMap<SyntaxCorrespondence, Indexes>()

const hashByte = (hash: number, byte: number): number => Math.imul(hash ^ byte, 16777619) >>> 0

const hashText = (hash: number, value: string): number => {
  let result = hash
  for (const character of value) {
    const point = character.codePointAt(0) ?? 0
    result = hashByte(result, point & 0xff)
    result = hashByte(result, (point >>> 8) & 0xff)
  }
  return result
}

const fingerprint = (
  syntax: SyntaxFile.SyntaxFile,
  element: SyntaxTree.Element,
  cache: Map<SyntaxTree.Element, string>,
): string => {
  const cached = cache.get(element)
  if (cached !== undefined) return cached
  let hash = 2166136261
  let key: string
  if (SyntaxTree.isToken(element)) {
    hash = hashText(hashText(hash, 'token'), element.kind)
    for (let offset = element.span.start; offset < element.span.end; offset += 1) {
      hash = hashByte(hash, syntax.source.bytes[offset] ?? 0)
    }
    key = `token:${element.kind}:${element.span.end - element.span.start}:${hash}`
  } else if (SyntaxTree.isMissingToken(element)) {
    hash = hashText(hashText(hash, 'missing'), element.expected)
    key = `missing:${element.expected}:${hash}`
  } else {
    hash = hashText(hashText(hash, 'node'), element.kind)
    for (const child of element.children) hash = hashText(hash, fingerprint(syntax, child, cache))
    key = `node:${element.kind}:${element.children.length}:${hash}`
  }
  cache.set(element, key)
  return key
}

const sameTokenBytes = (
  previous: SyntaxFile.SyntaxFile,
  previousElement: Extract<SyntaxTree.Element, { readonly _tag: 'Token' }>,
  current: SyntaxFile.SyntaxFile,
  currentElement: Extract<SyntaxTree.Element, { readonly _tag: 'Token' }>,
): boolean => {
  const previousLength = previousElement.span.end - previousElement.span.start
  if (previousLength !== currentElement.span.end - currentElement.span.start) return false
  for (let index = 0; index < previousLength; index += 1) {
    if (
      previous.source.bytes[previousElement.span.start + index] !==
      current.source.bytes[currentElement.span.start + index]
    )
      return false
  }
  return true
}

const sameStructure = (
  previous: SyntaxFile.SyntaxFile,
  previousElement: SyntaxTree.Element,
  current: SyntaxFile.SyntaxFile,
  currentElement: SyntaxTree.Element,
): boolean => {
  if (previousElement._tag !== currentElement._tag) return false
  if (SyntaxTree.isToken(previousElement) && SyntaxTree.isToken(currentElement))
    return (
      previousElement.kind === currentElement.kind &&
      sameTokenBytes(previous, previousElement, current, currentElement)
    )
  if (SyntaxTree.isMissingToken(previousElement) && SyntaxTree.isMissingToken(currentElement))
    return previousElement.expected === currentElement.expected
  if (!SyntaxTree.isNode(previousElement) || !SyntaxTree.isNode(currentElement)) return false
  return (
    previousElement.kind === currentElement.kind &&
    previousElement.children.length === currentElement.children.length &&
    previousElement.children.every((child, index) => {
      const candidate = currentElement.children.at(index)
      return candidate !== undefined && sameStructure(previous, child, current, candidate)
    })
  )
}

const elementCount = (element: SyntaxTree.Element): number =>
  SyntaxTree.isNode(element)
    ? 1 + element.children.reduce((total, child) => total + elementCount(child), 0)
    : 1

const pairExact = (
  previous: SyntaxTree.Element,
  current: SyntaxTree.Element,
  previousToCurrent: Map<SyntaxTree.Element, SyntaxTree.Element>,
  currentToPrevious: Map<SyntaxTree.Element, SyntaxTree.Element>,
): void => {
  previousToCurrent.set(previous, current)
  currentToPrevious.set(current, previous)
  if (!SyntaxTree.isNode(previous) || !SyntaxTree.isNode(current)) return
  for (const [index, previousChild] of previous.children.entries()) {
    const currentChild = current.children.at(index)
    if (currentChild !== undefined)
      pairExact(previousChild, currentChild, previousToCurrent, currentToPrevious)
  }
}

const uniqueByFingerprint = (
  syntax: SyntaxFile.SyntaxFile,
  elements: ReadonlyArray<SyntaxTree.Element>,
  cache: Map<SyntaxTree.Element, string>,
): ReadonlyMap<string, ReadonlyArray<SyntaxTree.Element>> => {
  const groups = new Map<string, Array<SyntaxTree.Element>>()
  for (const element of elements) {
    const key = fingerprint(syntax, element, cache)
    const group = groups.get(key)
    if (group === undefined) groups.set(key, [element])
    else group.push(element)
  }
  return groups
}

/**
 * Builds exact adjacent-revision correspondence, or `None` when the artifacts have different
 * logical source identities.
 */
export const between = (
  previous: SyntaxFile.SyntaxFile,
  current: SyntaxFile.SyntaxFile,
): Option.Option<SyntaxCorrespondence> => {
  if (previous.source.id !== current.source.id) return Option.none()
  const previousToCurrent = new Map<SyntaxTree.Element, SyntaxTree.Element>()
  const currentToPrevious = new Map<SyntaxTree.Element, SyntaxTree.Element>()
  if (sameStructure(previous, previous.root, current, current.root)) {
    pairExact(previous.root, current.root, previousToCurrent, currentToPrevious)
  } else {
    const previousFingerprints = new Map<SyntaxTree.Element, string>()
    const currentFingerprints = new Map<SyntaxTree.Element, string>()
    const previousGroups = uniqueByFingerprint(
      previous,
      previous.root.children,
      previousFingerprints,
    )
    const currentGroups = uniqueByFingerprint(current, current.root.children, currentFingerprints)
    for (const [key, previousCandidates] of previousGroups) {
      const currentCandidates = currentGroups.get(key)
      if (previousCandidates.length !== 1 || currentCandidates?.length !== 1) continue
      const previousElement = previousCandidates.at(0)
      const currentElement = currentCandidates.at(0)
      if (
        previousElement !== undefined &&
        currentElement !== undefined &&
        sameStructure(previous, previousElement, current, currentElement)
      )
        pairExact(previousElement, currentElement, previousToCurrent, currentToPrevious)
    }
  }
  const identities = Object.freeze(
    [...previousToCurrent.entries()]
      .map(
        ([previousElement, currentElement]): IdentityPair =>
          Object.freeze({
            previous: Option.getOrThrow(SyntaxFile.idOf(previous, previousElement)),
            current: Option.getOrThrow(SyntaxFile.idOf(current, currentElement)),
          }),
      )
      .sort((left, right) => left.previous.ordinal - right.previous.ordinal),
  )
  const correspondence: SyntaxCorrespondence = Object.freeze({
    _tag: 'SyntaxCorrespondence',
    previous,
    current,
    identities,
    counts: Object.freeze({
      previousElements: elementCount(previous.root),
      currentElements: elementCount(current.root),
      correspondingElements: previousToCurrent.size,
    }),
  })
  indexes.set(correspondence, { previousToCurrent, currentToPrevious })
  return Option.some(correspondence)
}

/** Returns the exact current element corresponding to one previous element. */
export const currentOf = (
  self: SyntaxCorrespondence,
  previous: SyntaxTree.Element,
): Option.Option<SyntaxTree.Element> =>
  Option.fromUndefinedOr(indexes.get(self)?.previousToCurrent.get(previous))

/** Returns the exact previous element corresponding to one current element. */
export const previousOf = (
  self: SyntaxCorrespondence,
  current: SyntaxTree.Element,
): Option.Option<SyntaxTree.Element> =>
  Option.fromUndefinedOr(indexes.get(self)?.currentToPrevious.get(current))
