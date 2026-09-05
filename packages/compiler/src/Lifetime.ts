import * as Canonical from './internal/Canonical.js'

/** Stable declaration identity shared by a lifetime binder and its semantic consumers. */
export interface Owner {
  readonly module: string
  readonly name: string
}

/** Validity independent of every finite caller region. It does not keep an owner alive. */
export interface Static {
  readonly _tag: 'StaticLifetime'
}

/** One universal declaration parameter; its name is presentation, not semantic identity. */
export interface Bound {
  readonly _tag: 'BoundLifetime'
  readonly owner: Owner
  readonly binder: ReadonlyArray<number>
  readonly ordinal: number
  readonly name: string
}

/** One inference variable scoped to a body and its already selected semantic context. */
export interface Local {
  readonly _tag: 'LocalLifetime'
  readonly owner: Owner
  readonly context: string
  readonly ordinal: number
}

/** A rigid invocation parameter which must not escape its comparison universe. */
export interface Placeholder {
  readonly _tag: 'PlaceholderLifetime'
  readonly parameter: Bound
  readonly universe: string
}

/** Static proof information; concrete loan identity and access belong to ownership analysis. */
export type Lifetime = Static | Bound | Local | Placeholder

export const staticLifetime: Static = Object.freeze({ _tag: 'StaticLifetime' })

export const bound = (
  owner: Owner,
  ordinal: number,
  name: string,
  binder: ReadonlyArray<number> = [],
): Bound =>
  Object.freeze({
    _tag: 'BoundLifetime',
    owner: Object.freeze({ ...owner }),
    binder: Object.freeze([...binder]),
    ordinal,
    name: name.startsWith("'") ? name.slice(1) : name,
  })

export const local = (owner: Owner, context: string, ordinal: number): Local =>
  Object.freeze({
    _tag: 'LocalLifetime',
    owner: Object.freeze({ ...owner }),
    context,
    ordinal,
  })

export const placeholder = (parameter: Bound, universe: string): Placeholder =>
  Object.freeze({ _tag: 'PlaceholderLifetime', parameter, universe })

/** Recognizes a region in an already typed discriminated semantic-argument union. */
export const isLifetime = (self: string | { readonly _tag: string }): self is Lifetime =>
  typeof self !== 'string' &&
  (self._tag === 'StaticLifetime' ||
    self._tag === 'BoundLifetime' ||
    self._tag === 'LocalLifetime' ||
    self._tag === 'PlaceholderLifetime')

const ownerKey = (self: Owner): string => Canonical.record('Declaration', [self.module, self.name])

/** Encodes proof identity without parameter spelling, source offsets or concrete referents. */
export const key = (self: Lifetime): string => {
  switch (self._tag) {
    case 'StaticLifetime':
      return 'static'
    case 'BoundLifetime':
      return Canonical.record('BoundLifetime', [
        ownerKey(self.owner),
        Canonical.array(self.binder.map(String)),
        String(self.ordinal),
      ])
    case 'LocalLifetime':
      return Canonical.record('LocalLifetime', [
        ownerKey(self.owner),
        self.context,
        String(self.ordinal),
      ])
    case 'PlaceholderLifetime':
      return Canonical.record('PlaceholderLifetime', [self.universe, key(self.parameter)])
  }
}

export const equals = (self: Lifetime, other: Lifetime): boolean => key(self) === key(other)

/** Renders a source-facing lifetime; unresolved locals never masquerade as static validity. */
export const display = (self: Lifetime): string => {
  switch (self._tag) {
    case 'StaticLifetime':
      return "'static"
    case 'BoundLifetime':
      return `'${self.name}`
    case 'LocalLifetime':
      return `'_local${self.ordinal}`
    case 'PlaceholderLifetime':
      return display(self.parameter)
  }
}

export const substitute = (
  self: Lifetime,
  substitution: ReadonlyMap<string, Lifetime>,
): Lifetime => (self._tag === 'StaticLifetime' ? self : (substitution.get(key(self)) ?? self))

/** The longer region must contain every use required by the shorter region. */
export interface Outlives {
  readonly longer: Lifetime
  readonly shorter: Lifetime
}

export interface Assumptions {
  readonly bounds: ReadonlyArray<Outlives>
  readonly key: string
}

const outlivesKey = (self: Outlives): string =>
  Canonical.record('Outlives', [key(self.longer), key(self.shorter)])

/** Canonicalizes declared and implied assumptions before contextual comparison memoization. */
export const assumptions = (bounds: ReadonlyArray<Outlives>): Assumptions => {
  const entries = new Map(bounds.map((entry) => [outlivesKey(entry), Object.freeze({ ...entry })]))
  const ordered = [...entries].sort(([left], [right]) => {
    if (left < right) return -1
    if (left > right) return 1
    return 0
  })
  return Object.freeze({
    bounds: Object.freeze(ordered.map(([, entry]) => entry)),
    key: Canonical.array(ordered.map(([identity]) => identity)),
  })
}

/** Proves a declared relationship using only reflexivity, static validity and known bounds. */
export const outlives = (self: Assumptions, longer: Lifetime, shorter: Lifetime): boolean => {
  if (longer._tag === 'StaticLifetime' || equals(longer, shorter)) return true
  const destination = key(shorter)
  const edges = new Map<string, Array<string>>()
  for (const bound of self.bounds) {
    const from = key(bound.longer)
    const next = edges.get(from) ?? []
    next.push(key(bound.shorter))
    edges.set(from, next)
  }
  const pending = [key(longer)]
  const visited = new Set(pending)
  for (let index = 0; index < pending.length; index += 1) {
    const current = pending.at(index)
    if (current === undefined) continue
    for (const next of edges.get(current) ?? []) {
      if (next === destination || next === key(staticLifetime)) return true
      if (visited.has(next)) continue
      visited.add(next)
      pending.push(next)
    }
  }
  return false
}

/** A finite local region's permitted points and the uses which demand its validity. */
export interface Region {
  readonly lifetime: Lifetime
  readonly available: ReadonlySet<number>
  readonly required: ReadonlySet<number>
}

export interface Input {
  readonly pointCount: number
  readonly regions: ReadonlyArray<Region>
  readonly constraints: ReadonlyArray<Outlives>
}

export interface Work {
  readonly regions: number
  readonly constraints: number
  readonly requiredPoints: number
  readonly propagatedPoints: number
  readonly edgeVisits: number
}

export type Solution =
  | {
      readonly _tag: 'Solved'
      readonly required: ReadonlyMap<string, ReadonlySet<number>>
      readonly violations: ReadonlyArray<{
        readonly lifetime: Lifetime
        readonly point: number
      }>
      readonly work: Work
    }
  | {
      readonly _tag: 'InvalidDomain'
      readonly dimension: 'PointCount' | 'Point' | 'DuplicateRegion' | 'MissingRegion'
      readonly lifetime?: Lifetime
    }

/**
 * Computes the least required regions in an explicit finite body domain.
 *
 * Constraints only propagate existing program points. Missing inputs are rejected rather than
 * treated as unconstrained validity; the solver has no declaration or implementation resolver.
 */
export const solve = (input: Input): Solution => {
  if (!Number.isSafeInteger(input.pointCount) || input.pointCount < 0)
    return Object.freeze({ _tag: 'InvalidDomain', dimension: 'PointCount' })
  const regions = new Map<string, Region>()
  const required = new Map<string, Set<number>>()
  const edges = new Map<string, Set<string>>()
  const pending: Array<readonly [string, number]> = []
  for (const region of input.regions) {
    const identity = key(region.lifetime)
    if (regions.has(identity))
      return Object.freeze({
        _tag: 'InvalidDomain',
        dimension: 'DuplicateRegion',
        lifetime: region.lifetime,
      })
    for (const point of [...region.available, ...region.required]) {
      if (!Number.isSafeInteger(point) || point < 0 || point >= input.pointCount)
        return Object.freeze({
          _tag: 'InvalidDomain',
          dimension: 'Point',
          lifetime: region.lifetime,
        })
    }
    regions.set(identity, region)
    required.set(identity, new Set(region.required))
    for (const point of [...region.required].sort((left, right) => left - right))
      pending.push([identity, point])
  }
  for (const constraint of input.constraints) {
    const longer = key(constraint.longer)
    const shorter = key(constraint.shorter)
    let absent: Lifetime | undefined
    if (!regions.has(longer)) absent = constraint.longer
    else if (!regions.has(shorter)) absent = constraint.shorter
    if (absent !== undefined)
      return Object.freeze({
        _tag: 'InvalidDomain',
        dimension: 'MissingRegion',
        lifetime: absent,
      })
    const parents = edges.get(shorter) ?? new Set<string>()
    parents.add(longer)
    edges.set(shorter, parents)
  }
  const requiredPoints = pending.length
  let edgeVisits = 0
  for (let cursor = 0; cursor < pending.length; cursor += 1) {
    const fact = pending.at(cursor)
    if (fact === undefined) continue
    const [shorter, point] = fact
    for (const longer of edges.get(shorter) ?? []) {
      edgeVisits += 1
      const points = required.get(longer)
      if (points === undefined || points.has(point)) continue
      points.add(point)
      pending.push([longer, point])
    }
  }
  const violations: Array<{ readonly lifetime: Lifetime; readonly point: number }> = []
  for (const [identity, region] of regions) {
    if (region.lifetime._tag === 'StaticLifetime') continue
    for (const point of [...(required.get(identity) ?? [])].sort((left, right) => left - right)) {
      if (!region.available.has(point))
        violations.push(Object.freeze({ lifetime: region.lifetime, point }))
    }
  }
  return Object.freeze({
    _tag: 'Solved',
    required,
    violations: Object.freeze(violations),
    work: Object.freeze({
      regions: regions.size,
      constraints: [...edges.values()].reduce((count, values) => count + values.size, 0),
      requiredPoints,
      propagatedPoints: pending.length - requiredPoints,
      edgeVisits,
    }),
  })
}
