import type * as Mir from './Mir.js'
import { destinationOf, type LinearBlock, opensRuntimeContinuation } from './MirLinearization.js'

export interface MutableRoots {
  readonly mutable: ReadonlySet<number>
  readonly address: ReadonlySet<number>
}

/** Finds locals that need stable stack storage across mutation, calls, and suspension control. */
export const mutableRoots = (
  fn: Mir.MirFunction,
  blocks: ReadonlyArray<LinearBlock>,
): MutableRoots => {
  const assignments = new Map<number, number>()
  for (const operation of blocks.flatMap((block) => block.operations)) {
    const destination = destinationOf(operation)
    if (destination !== undefined)
      assignments.set(destination.ordinal, (assignments.get(destination.ordinal) ?? 0) + 1)
  }
  const continuationLocals = blocks.flatMap((block) => {
    if (
      block.terminator._tag === 'Return' ||
      block.terminator._tag === 'Trap' ||
      block.terminator._tag === 'PropagateEffectFailure'
    )
      return []
    let afterRuntimeContinuation = false
    const ordinals: Array<number> = []
    for (const operation of block.operations) {
      if (opensRuntimeContinuation(operation)) afterRuntimeContinuation = true
      if (!afterRuntimeContinuation) continue
      const destination = destinationOf(operation)
      if (destination !== undefined) ordinals.push(destination.ordinal)
    }
    return ordinals
  })
  const runtimeContinuationDestinations = blocks.flatMap((block) =>
    block.operations.flatMap((operation) => {
      if (!opensRuntimeContinuation(operation) || operation._tag === 'Binary') return []
      const destination = destinationOf(operation)
      return destination === undefined ? [] : [destination.ordinal]
    }),
  )
  const borrowedCaptureRoots = new Set(
    blocks.flatMap((block) =>
      block.operations.flatMap((operation) =>
        operation._tag === 'MakeEffect' || operation._tag === 'MakeCallable'
          ? operation.captures.flatMap((capture, ordinal) =>
              (
                operation._tag === 'MakeEffect'
                  ? operation.type.environment.fields.at(ordinal)?.representation === 'Borrow'
                  : capture.access === 'Shared' || capture.access === 'Exclusive'
              )
                ? [capture.source.ordinal]
                : [],
            )
          : [],
      ),
    ),
  )
  const mutable = new Set([
    ...blocks.flatMap((block) =>
      block.operations.flatMap((operation) =>
        operation._tag === 'WritePlace' &&
        fn.localTypes.at(operation.root.ordinal)?._tag !== 'Slice'
          ? [operation.root.ordinal]
          : [],
      ),
    ),
    ...[...assignments].flatMap(([ordinal, count]) => (count > 1 ? [ordinal] : [])),
    ...continuationLocals,
    ...runtimeContinuationDestinations,
    ...blocks.flatMap((block) =>
      block.operations.flatMap((operation) =>
        operation._tag === 'RunEffectComposite'
          ? [operation.outcome.ordinal, operation.destination.ordinal]
          : [],
      ),
    ),
    ...(fn.suspension?.regions ?? []).flatMap((region) =>
      region._tag === 'RunSuspendableEffectRegion' && region.relay.state !== undefined
        ? [
            region.operation.destination.ordinal,
            region.operation.outcome.ordinal,
            ...region.relay.state.slots.map((slot) => slot.local.ordinal),
          ]
        : [],
    ),
    ...borrowedCaptureRoots,
  ])
  const address = new Set([
    ...blocks.flatMap((block) =>
      block.operations.flatMap((operation) =>
        operation._tag === 'BeginLoan' &&
        operation.sourceType._tag !== 'Slice' &&
        fn.localTypes.at(operation.root.ordinal)?._tag !== 'Reference'
          ? [operation.root.ordinal]
          : [],
      ),
    ),
    ...borrowedCaptureRoots,
  ])
  for (const root of address) mutable.add(root)
  return Object.freeze({ mutable, address })
}
