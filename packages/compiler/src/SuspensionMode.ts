/** Static suspension capabilities retained for one exact executable specialization. */
export type Mode = 'NestedTransfer' | 'ExternalPark'

/** Why one mode is reachable, from the inspected executable to the sealed origin. */
export interface Cause {
  readonly mode: Mode
  readonly path: ReadonlyArray<string>
}

/**
 * The normalized target-neutral suspension summary.
 *
 * Direct execution is represented by an empty `modes` array. `Open` summaries retain every mode
 * permitted by an unresolved static contract; `Unavailable` summaries keep the causal node rather
 * than silently becoming direct.
 */
export interface Summary {
  readonly _tag: 'SuspensionModeSummary'
  readonly availability: 'Complete' | 'Open' | 'Unavailable'
  readonly modes: ReadonlyArray<Mode>
  readonly causes: ReadonlyArray<Cause>
}

/** One reachability graph whose roots are classified by suspension policy. */
export interface Graph {
  readonly roots: ReadonlyMap<Mode, ReadonlySet<string>>
  readonly dependencies: ReadonlyMap<string, ReadonlySet<string>>
  readonly permitted: ReadonlyMap<string, ReadonlySet<Mode>>
  readonly unavailable: ReadonlySet<string>
}

export const order: ReadonlyArray<Mode> = Object.freeze(['NestedTransfer', 'ExternalPark'])

const compareText = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0

const normalizeModes = (modes: Iterable<Mode>): ReadonlyArray<Mode> => {
  const retained = new Set(modes)
  return Object.freeze(order.filter((mode) => retained.has(mode)))
}

const comparePath = (left: ReadonlyArray<string>, right: ReadonlyArray<string>): number => {
  if (left.length !== right.length) return left.length - right.length
  return compareText(left.join('\0'), right.join('\0'))
}

const pathTo = (
  graph: Graph,
  origin: string,
  roots: ReadonlySet<string>,
): ReadonlyArray<string> | undefined => {
  if (roots.has(origin)) return Object.freeze([origin])
  const pending: Array<ReadonlyArray<string>> = [Object.freeze([origin])]
  const visited = new Set([origin])
  while (pending.length > 0) {
    const path = pending.shift()
    const tail = path?.at(-1)
    if (path === undefined || tail === undefined) continue
    const targets = [...(graph.dependencies.get(tail) ?? [])].sort(compareText)
    for (const target of targets) {
      if (visited.has(target)) continue
      visited.add(target)
      const next = Object.freeze([...path, target])
      if (roots.has(target)) return next
      pending.push(next)
    }
    pending.sort(comparePath)
  }
  return undefined
}

/** Computes one deterministic summary for every graph node. */
export const summarize = (graph: Graph): ReadonlyMap<string, Summary> => {
  const nodes = new Set<string>([
    ...graph.dependencies.keys(),
    ...[...graph.dependencies.values()].flatMap((targets) => [...targets]),
    ...[...graph.roots.values()].flatMap((roots) => [...roots]),
    ...graph.permitted.keys(),
    ...graph.unavailable,
  ])
  const summaries = new Map<string, Summary>()
  for (const node of [...nodes].sort(compareText)) {
    const causes: Array<Cause> = []
    for (const mode of order) {
      const roots = graph.roots.get(mode) ?? new Set<string>()
      const path = pathTo(graph, node, roots)
      if (path !== undefined) causes.push(Object.freeze({ mode, path }))
    }
    const permitted = graph.permitted.get(node) ?? new Set<Mode>()
    const modes = normalizeModes([...causes.map((cause) => cause.mode), ...permitted])
    summaries.set(
      node,
      Object.freeze({
        _tag: 'SuspensionModeSummary',
        availability: graph.unavailable.has(node)
          ? 'Unavailable'
          : permitted.size > 0
            ? 'Open'
            : 'Complete',
        modes,
        causes: Object.freeze(causes),
      }),
    )
  }
  return summaries
}

export const direct: Summary = Object.freeze({
  _tag: 'SuspensionModeSummary',
  availability: 'Complete',
  modes: Object.freeze([]),
  causes: Object.freeze([]),
})

/** Conservatively summarizes one unresolved executable contract from its sealed obligations. */
export const openExecutable = (
  staticProperties: ReadonlyArray<'Intrinsic.Detached' | 'Intrinsic.NonParking'>,
): Summary =>
  Object.freeze({
    _tag: 'SuspensionModeSummary',
    availability: 'Open',
    modes: Object.freeze<ReadonlyArray<Mode>>(
      staticProperties.includes('Intrinsic.NonParking')
        ? ['NestedTransfer']
        : ['NestedTransfer', 'ExternalPark'],
    ),
    causes: Object.freeze([]),
  })

export const has = (self: Summary, mode: Mode): boolean => self.modes.includes(mode)

/** Joins summaries while retaining deterministic modes and causal paths. */
export const join = (inputs: ReadonlyArray<Summary>): Summary => {
  const availability = inputs.some((input) => input.availability === 'Unavailable')
    ? 'Unavailable'
    : inputs.some((input) => input.availability === 'Open')
      ? 'Open'
      : 'Complete'
  const causes = [
    ...new Map(
      inputs
        .flatMap((input) => input.causes)
        .map((cause) => [`${cause.mode}\0${cause.path.join('\0')}`, cause] as const),
    ).values(),
  ].sort(
    (left, right) =>
      order.indexOf(left.mode) - order.indexOf(right.mode) || comparePath(left.path, right.path),
  )
  return Object.freeze({
    _tag: 'SuspensionModeSummary',
    availability,
    modes: normalizeModes(inputs.flatMap((input) => input.modes)),
    causes: Object.freeze(causes),
  })
}

/** Canonical inspection encoding; graph node identities retain the complete causal path. */
export const encode = (self: Summary): string =>
  `${self.availability}[${self.modes.join(',') || 'Direct'}]${
    self.causes.length === 0
      ? ''
      : `{${self.causes.map((cause) => `${cause.mode}:${cause.path.join(' -> ')}`).join(';')}}`
  }`
