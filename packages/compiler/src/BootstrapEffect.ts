import type { TraceEvent } from './BootstrapTrace.js'
import type * as Termination from './Termination.js'
import * as Type from './Type.js'

export const isPhysicalEntryAdapter = (name: string): boolean =>
  name === '$effect-entry' || name === '$unit-entry'

export const failureIdentity = (type: Type.Effect | Type.FailureRow, tag: number): string => {
  const failure = Type.failureMembers(type).at(tag - 1)
  if (failure === undefined) throw new RangeError(`Effect failure tag ${tag} has no type identity`)
  return Type.encode(failure)
}

export const logicalPathAt = (
  trace: ReadonlyArray<TraceEvent>,
  through: number,
): ReadonlyArray<Termination.LogicalFrame> => {
  const active = new Map<
    number,
    { readonly depth: number; readonly value: Termination.LogicalFrame }
  >()
  for (let index = 0; index <= through; index += 1) {
    const event = trace.at(index)
    if (event?._tag === 'Entry' && !isPhysicalEntryAdapter(event.function.name))
      active.set(
        event.frame,
        Object.freeze({
          depth: event.depth,
          value: Object.freeze({ function: event.function, provenance: event.span }),
        }),
      )
    else if (event?._tag === 'Return') active.delete(event.frame)
  }
  return Object.freeze(
    [...active.values()].sort((left, right) => left.depth - right.depth).map(({ value }) => value),
  )
}

export const causalHistory = (
  trace: ReadonlyArray<TraceEvent>,
  terminal: 'Success' | 'TypedFailure' | 'Trap',
  terminalIdentity?: string,
): ReadonlyArray<Termination.CausalFailure> => {
  const failures = trace.flatMap((event, index) =>
    event._tag === 'EffectFailure' && event.phase === 'Produced'
      ? [
          Object.freeze({
            event,
            index,
            key: `${event.function.module}\u0000${event.function.name}\u0000${event.tag}\u0000${event.span.start}\u0000${event.span.end}`,
          }),
        ]
      : [],
  )
  const distinct = failures.filter(
    (failure, index) => failures.findIndex((candidate) => candidate.key === failure.key) === index,
  )
  return Object.freeze(
    distinct.map(({ event, index }, ordinal) => {
      const identity =
        event.identity ?? (ordinal === distinct.length - 1 ? terminalIdentity : undefined)
      return Object.freeze({
        tag: event.tag,
        ...(identity === undefined ? {} : { identity }),
        provenance: event.span,
        logicalPath: logicalPathAt(trace, index),
        recovered: terminal !== 'TypedFailure' || ordinal !== distinct.length - 1,
      })
    }),
  )
}

export const longestCausalPath = (
  history: ReadonlyArray<Termination.CausalFailure>,
  fallback: ReadonlyArray<Termination.LogicalFrame>,
): ReadonlyArray<Termination.LogicalFrame> =>
  history.reduce(
    (selected, candidate) =>
      candidate.logicalPath.length > selected.length ? candidate.logicalPath : selected,
    fallback,
  )
