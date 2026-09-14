/** One canonical set of complete ancestor assignments in a discovery-local catalog. */
export interface History {
  readonly id: number
  readonly variable?: string
  readonly branches: ReadonlyMap<string | undefined, History>
}

/** Interned decision nodes; an omitted declaration always means an absent ancestor. */
export interface AncestorHistory {
  readonly empty: History
  readonly initial: History
  readonly nodes: Map<string, History>
  readonly unions: Map<string, History>
}

export const make = (): AncestorHistory => ({
  empty: Object.freeze({ id: 0, branches: new Map() }),
  initial: Object.freeze({ id: 1, branches: new Map() }),
  nodes: new Map(),
  unions: new Map(),
})

const node = (
  self: AncestorHistory,
  variable: string,
  branches: ReadonlyMap<string | undefined, History>,
): History => {
  const entries = [...branches].filter(([, child]) => child !== self.empty)
  if (entries.length === 0) return self.empty
  if (entries.length === 1 && entries[0]?.[0] === undefined) return entries[0]?.[1] ?? self.empty
  entries.sort(([a], [b]) => {
    if (a === b) return 0
    if (a === undefined) return -1
    if (b === undefined) return 1
    return a < b ? -1 : 1
  })
  const key = JSON.stringify([variable, entries.map(([value, child]) => [value ?? null, child.id])])
  const prior = self.nodes.get(key)
  if (prior !== undefined) return prior
  const result = Object.freeze({ id: self.nodes.size + 2, variable, branches: new Map(entries) })
  self.nodes.set(key, result)
  return result
}

/** Unites exact assignments without mixing values from different histories. */
export const union = (self: AncestorHistory, left: History, right: History): History => {
  if (left === right || right === self.empty) return left
  if (left === self.empty) return right
  const key = left.id < right.id ? `${left.id}:${right.id}` : `${right.id}:${left.id}`
  const cached = self.unions.get(key)
  if (cached !== undefined) return cached
  let variable = left.variable
  if (variable === undefined || (right.variable !== undefined && right.variable < variable))
    variable = right.variable
  if (variable === undefined) return self.initial
  const a: ReadonlyMap<string | undefined, History> =
    left.variable === variable ? left.branches : new Map([[undefined, left]])
  const b: ReadonlyMap<string | undefined, History> =
    right.variable === variable ? right.branches : new Map([[undefined, right]])
  const branches = new Map<string | undefined, History>()
  for (const value of new Set([...a.keys(), ...b.keys()]))
    branches.set(value, union(self, a.get(value) ?? self.empty, b.get(value) ?? self.empty))
  const result = node(self, variable, branches)
  self.unions.set(key, result)
  return result
}

/** Overwrites one declaration in every assignment, retaining all other correlations. */
export const set = (
  self: AncestorHistory,
  history: History,
  variable: string,
  value: string,
): History => {
  const memo = new Map<number, History>()
  const visit = (current: History): History => {
    if (current === self.empty) return current
    const prior = memo.get(current.id)
    if (prior !== undefined) return prior
    let result: History
    if (current.variable === undefined || current.variable > variable)
      result = node(self, variable, new Map([[value, current]]))
    else if (current.variable === variable) {
      let suffix = self.empty
      for (const child of current.branches.values()) suffix = union(self, suffix, child)
      result = node(self, variable, new Map([[value, suffix]]))
    } else {
      result = node(
        self,
        current.variable,
        new Map([...current.branches].map(([branch, child]) => [branch, visit(child)])),
      )
    }
    memo.set(current.id, result)
    return result
  }
  return visit(history)
}

/** Partitions by one ancestor while preserving the complete correlated assignments. */
export const partition = (
  self: AncestorHistory,
  history: History,
  variable: string,
): ReadonlyMap<string | undefined, History> => {
  const memo = new Map<number, ReadonlyMap<string | undefined, History>>()
  const visit = (current: History): ReadonlyMap<string | undefined, History> => {
    if (current === self.empty) return new Map()
    const prior = memo.get(current.id)
    if (prior !== undefined) return prior
    const result = new Map<string | undefined, History>()
    if (current.variable === undefined || current.variable > variable)
      result.set(undefined, current)
    else if (current.variable === variable) {
      for (const [value, child] of current.branches)
        result.set(value, node(self, variable, new Map([[value, child]])))
    } else {
      const groups = new Map<string | undefined, Map<string | undefined, History>>()
      for (const [branch, child] of current.branches) {
        for (const [value, suffix] of visit(child)) {
          const branches = groups.get(value) ?? new Map<string | undefined, History>()
          branches.set(branch, suffix)
          groups.set(value, branches)
        }
      }
      for (const [value, branches] of groups)
        result.set(value, node(self, current.variable, branches))
    }
    memo.set(current.id, result)
    return result
  }
  return visit(history)
}
