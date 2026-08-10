export type Operation =
  | { readonly _tag: 'Constant'; readonly destination: number; readonly value: number }
  | {
      readonly _tag: 'Add'
      readonly destination: number
      readonly left: number
      readonly right: number
    }
  | { readonly _tag: 'BindResult'; readonly destination: number; readonly source: number }

export type Outcome =
  | { readonly _tag: 'Forward'; readonly target: number }
  | { readonly _tag: 'Return'; readonly value: number }
  | { readonly _tag: 'Repeat' }
  | { readonly _tag: 'Exit' }

export type Region =
  | {
      readonly _tag: 'OperationRegion'
      readonly id: number
      readonly operations: ReadonlyArray<Operation>
      readonly outcome: Outcome
    }
  | { readonly _tag: 'ConditionalRegion'; readonly id: number }
  | { readonly _tag: 'LoopRegion'; readonly id: number }
  | { readonly _tag: 'CleanupRegion'; readonly id: number }

export interface Graph {
  readonly entry: number
  readonly locals: ReadonlyArray<number>
  readonly regions: ReadonlyArray<Region>
}

export type Rejection =
  | 'UnknownLocal'
  | 'UnknownRegion'
  | 'ConditionalOrLoopRegion'
  | 'CleanupRegion'
  | 'LexicalLoopExit'
  | 'Cycle'
  | 'MultipleReturns'

export type Result =
  | {
      readonly _tag: 'Accepted'
      readonly graph: Graph
      readonly localMap: ReadonlyMap<number, number>
      readonly regionMap: ReadonlyMap<number, number>
    }
  | { readonly _tag: 'Rejected'; readonly reason: Rejection }

const reject = (reason: Rejection): Result => Object.freeze({ _tag: 'Rejected', reason })

const operationLocals = (operation: Operation): ReadonlyArray<number> => {
  switch (operation._tag) {
    case 'Constant':
      return [operation.destination]
    case 'Add':
      return [operation.destination, operation.left, operation.right]
    case 'BindResult':
      return [operation.destination, operation.source]
  }
}

const hasCycle = (
  regions: ReadonlyArray<Extract<Region, { readonly _tag: 'OperationRegion' }>>,
) => {
  const edges = new Map(
    regions.map((region) => [
      region.id,
      region.outcome._tag === 'Forward' ? [region.outcome.target] : [],
    ]),
  )
  const active = new Set<number>()
  const complete = new Set<number>()
  const visit = (id: number): boolean => {
    if (active.has(id)) return true
    if (complete.has(id)) return false
    active.add(id)
    const cyclic = (edges.get(id) ?? []).some(visit)
    active.delete(id)
    complete.add(id)
    return cyclic
  }
  return regions.some((region) => visit(region.id))
}

export const clone = (
  graph: Graph,
  options: {
    readonly firstLocal: number
    readonly firstRegion: number
    readonly continuation: number
    readonly result: number
  },
): Result => {
  const knownLocals = new Set(graph.locals)
  const knownRegions = new Set(graph.regions.map((region) => region.id))
  if (!knownRegions.has(graph.entry)) return reject('UnknownRegion')

  const operationRegions: Array<Extract<Region, { readonly _tag: 'OperationRegion' }>> = []
  for (const region of [...graph.regions].sort((left, right) => left.id - right.id)) {
    if (region._tag === 'CleanupRegion') return reject('CleanupRegion')
    if (region._tag !== 'OperationRegion') return reject('ConditionalOrLoopRegion')
    if (region.operations.flatMap(operationLocals).some((local) => !knownLocals.has(local))) {
      return reject('UnknownLocal')
    }
    if (region.outcome._tag === 'Return' && !knownLocals.has(region.outcome.value)) {
      return reject('UnknownLocal')
    }
    if (region.outcome._tag === 'Forward' && !knownRegions.has(region.outcome.target)) {
      return reject('UnknownRegion')
    }
    if (region.outcome._tag === 'Repeat' || region.outcome._tag === 'Exit') {
      return reject('LexicalLoopExit')
    }
    operationRegions.push(region)
  }

  if (hasCycle(operationRegions)) return reject('Cycle')
  if (operationRegions.filter((region) => region.outcome._tag === 'Return').length !== 1) {
    return reject('MultipleReturns')
  }

  const localMap: ReadonlyMap<number, number> = new Map(
    [...graph.locals]
      .sort((left, right) => left - right)
      .map((local, ordinal) => [local, options.firstLocal + ordinal]),
  )
  const regionMap: ReadonlyMap<number, number> = new Map(
    operationRegions.map((region, ordinal) => [region.id, options.firstRegion + ordinal]),
  )
  const mappedLocal = (local: number): number => localMap.get(local) ?? local
  const mappedRegion = (region: number): number => regionMap.get(region) ?? region
  const operations = (values: ReadonlyArray<Operation>): ReadonlyArray<Operation> =>
    Object.freeze(
      values.map((operation) => {
        switch (operation._tag) {
          case 'Constant':
            return Object.freeze({ ...operation, destination: mappedLocal(operation.destination) })
          case 'Add':
            return Object.freeze({
              ...operation,
              destination: mappedLocal(operation.destination),
              left: mappedLocal(operation.left),
              right: mappedLocal(operation.right),
            })
          case 'BindResult':
            return Object.freeze({
              ...operation,
              destination: mappedLocal(operation.destination),
              source: mappedLocal(operation.source),
            })
          default:
            return operation
        }
      }),
    )

  return Object.freeze({
    _tag: 'Accepted',
    graph: Object.freeze({
      entry: mappedRegion(graph.entry),
      locals: Object.freeze([...localMap.values()]),
      regions: Object.freeze(
        operationRegions.map((region) => {
          const cloned = operations(region.operations)
          return Object.freeze({
            _tag: 'OperationRegion' as const,
            id: mappedRegion(region.id),
            operations:
              region.outcome._tag === 'Return'
                ? Object.freeze([
                    ...cloned,
                    Object.freeze({
                      _tag: 'BindResult' as const,
                      destination: options.result,
                      source: mappedLocal(region.outcome.value),
                    }),
                  ])
                : cloned,
            outcome: Object.freeze(
              region.outcome._tag === 'Forward'
                ? { _tag: 'Forward' as const, target: mappedRegion(region.outcome.target) }
                : { _tag: 'Forward' as const, target: options.continuation },
            ),
          })
        }),
      ),
    }),
    localMap,
    regionMap,
  })
}
