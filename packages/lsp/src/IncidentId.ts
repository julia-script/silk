import * as Schema from 'effect/Schema'

/** Stable identity of one analysis or worker-health incident. */
export const schema = Schema.TaggedStruct('IncidentId', { value: Schema.Natural })

export type IncidentId = typeof schema.Type

export const initial: IncidentId = Object.freeze({ _tag: 'IncidentId', value: 0 })

/** Allocates the next incident identity in one workspace-engine lifetime. */
export const next = (self: IncidentId): IncidentId =>
  Object.freeze({ _tag: 'IncidentId', value: self.value + 1 })
