import * as Instances from './Instances.js'
import type * as Mir from './Mir.js'
import type * as Ownership from './Ownership.js'

/** One frame-producing relay in the exact inner-to-outer order observed for one transfer. */
export interface Relay {
  readonly _tag: 'ContinuationRelay'
  readonly function: Instances.InstanceKey
  readonly descriptor: Mir.ContinuationDescriptor
  readonly layout: Mir.ContinuationTargetLayout
}

export type Event =
  | {
      readonly _tag: 'Request'
      readonly ordinal: number
      readonly point: Mir.SuspensionPointId
      readonly bytes: number
      readonly alignment: number
    }
  | {
      readonly _tag: 'Reject'
      readonly ordinal: number
      readonly point: Mir.SuspensionPointId
    }
  | {
      readonly _tag: 'Acquire'
      readonly ordinal: number
      readonly owner: number
      readonly point: Mir.SuspensionPointId
    }
  | {
      readonly _tag: 'EndAllocatorLoan' | 'Initialize' | 'Publish' | 'ChildReborrow'
      readonly ordinal: number
      readonly owner: number
      readonly point: Mir.SuspensionPointId
    }
  | {
      readonly _tag: 'FrameDrop' | 'SourceCleanup'
      readonly ordinal: number
      readonly point: Mir.SuspensionPointId
      readonly local: Mir.LocalId
      readonly cleanup: Ownership.CleanupPlan
    }
  | {
      readonly _tag: 'Reclaim'
      readonly ordinal: number
      readonly owner: number
      readonly point: Mir.SuspensionPointId
    }
  | { readonly _tag: 'ChildStart'; readonly frameCount: number }

export interface Prepared {
  readonly _tag: 'Prepared'
  readonly relays: ReadonlyArray<Relay>
  readonly events: ReadonlyArray<Event>
}

export interface Refused<OutOfMemory> {
  readonly _tag: 'Refused'
  readonly ordinal: number
  /** The exact failure supplied by the selected allocator; it is never reconstructed. */
  readonly failure: OutOfMemory
  readonly events: ReadonlyArray<Event>
}

export type Result<OutOfMemory> = Prepared | Refused<OutOfMemory>

const samePoint = (left: Mir.SuspensionPointId, right: Mir.SuspensionPointId): boolean =>
  left.sourceId === right.sourceId &&
  left.spanStart === right.spanStart &&
  left.spanEnd === right.spanEnd &&
  left.ordinal === right.ordinal &&
  Instances.keyText(left.owner) === Instances.keyText(right.owner)

/** Resolves one logical descriptor to its one validated physical target layout. */
export const relay = (
  program: Mir.Module,
  function_: Mir.MirFunction,
  descriptor: Mir.ContinuationDescriptor,
): Relay | undefined => {
  const layout = program.continuations?.entries.find((entry) =>
    samePoint(entry.point, descriptor.point),
  )
  return layout === undefined
    ? undefined
    : Object.freeze({
        _tag: 'ContinuationRelay',
        function: function_.instance,
        descriptor,
        layout,
      })
}

const cleanupEvents = (
  tag: 'FrameDrop' | 'SourceCleanup',
  ordinal: number,
  point: Mir.SuspensionPointId,
  releases: ReadonlyArray<Mir.ContinuationRelease>,
): ReadonlyArray<Event> =>
  Object.freeze(
    releases.map((release) =>
      Object.freeze({
        _tag: tag,
        ordinal,
        point,
        local: release.local,
        cleanup: release.cleanup,
      }),
    ),
  )

/**
 * Prepares an unpublished continuation chain transactionally.
 *
 * `relays` is the runtime inner-to-outer observation order. Each accepted request owns and fully
 * initializes one private frame, but nothing publishes until the entire chain is ready. Refusal
 * therefore rolls the rejecting activation back in-place and destroys earlier unpublished frames
 * in reverse order. The selected allocator's failure value is returned by identity.
 */
export const prepare = <OutOfMemory>(
  relays: ReadonlyArray<Relay>,
  failure: OutOfMemory,
  refusalOrdinal?: number,
): Result<OutOfMemory> => {
  if (
    refusalOrdinal !== undefined &&
    (!Number.isSafeInteger(refusalOrdinal) || refusalOrdinal < 1 || refusalOrdinal > relays.length)
  )
    throw new RangeError('refusalOrdinal must name one frame-producing relay')

  const events: Array<Event> = []
  const acquired: Array<{
    readonly ordinal: number
    readonly owner: number
    readonly relay: Relay
  }> = []
  for (let index = 0; index < relays.length; index += 1) {
    const selected = relays.at(index)
    if (selected === undefined) continue
    const ordinal = index + 1
    events.push(
      Object.freeze({
        _tag: 'Request',
        ordinal,
        point: selected.descriptor.point,
        bytes: selected.layout.acquisition.request.bytes,
        alignment: selected.layout.acquisition.request.alignment,
      }),
    )
    if (ordinal === refusalOrdinal) {
      events.push(
        Object.freeze({ _tag: 'Reject', ordinal, point: selected.descriptor.point }),
        ...cleanupEvents(
          'SourceCleanup',
          ordinal,
          selected.descriptor.point,
          selected.descriptor.allocationRefusal.releases,
        ),
      )
      for (const earlier of [...acquired].reverse()) {
        const full = earlier.relay.descriptor.layout.prefixRollbacks.at(-1)
        if (full === undefined)
          throw new RangeError('continuation descriptor has no complete initialization rollback')
        events.push(
          ...cleanupEvents(
            'FrameDrop',
            earlier.ordinal,
            earlier.relay.descriptor.point,
            full.frameDrops,
          ),
          ...cleanupEvents(
            'SourceCleanup',
            earlier.ordinal,
            earlier.relay.descriptor.point,
            full.sourceReleases,
          ),
          Object.freeze({
            _tag: 'Reclaim',
            ordinal: earlier.ordinal,
            owner: earlier.owner,
            point: earlier.relay.descriptor.point,
          }),
        )
      }
      return Object.freeze({
        _tag: 'Refused',
        ordinal,
        failure,
        events: Object.freeze(events),
      })
    }
    const owner = ordinal
    acquired.push(Object.freeze({ ordinal, owner, relay: selected }))
    events.push(
      Object.freeze({
        _tag: 'Acquire',
        ordinal,
        owner,
        point: selected.descriptor.point,
      }),
      Object.freeze({
        _tag: 'EndAllocatorLoan',
        ordinal,
        owner,
        point: selected.descriptor.point,
      }),
      Object.freeze({
        _tag: 'Initialize',
        ordinal,
        owner,
        point: selected.descriptor.point,
      }),
    )
  }
  for (const accepted of acquired)
    events.push(
      Object.freeze({
        _tag: 'Publish',
        ordinal: accepted.ordinal,
        owner: accepted.owner,
        point: accepted.relay.descriptor.point,
      }),
      Object.freeze({
        _tag: 'ChildReborrow',
        ordinal: accepted.ordinal,
        owner: accepted.owner,
        point: accepted.relay.descriptor.point,
      }),
    )
  events.push(Object.freeze({ _tag: 'ChildStart', frameCount: acquired.length }))
  return Object.freeze({ _tag: 'Prepared', relays, events: Object.freeze(events) })
}
