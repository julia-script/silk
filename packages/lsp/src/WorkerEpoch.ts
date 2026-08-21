import * as Schema from 'effect/Schema'

/** Identity of one replaceable project-worker lifetime. */
export const schema = Schema.Struct({
  _tag: Schema.Literal('WorkerEpoch'),
  value: Schema.Natural,
})

export type WorkerEpoch = typeof schema.Type

export const initial: WorkerEpoch = Object.freeze({ _tag: 'WorkerEpoch', value: 0 })

/** Advances to the identity of a replacement worker. */
export const next = (self: WorkerEpoch): WorkerEpoch =>
  Object.freeze({ _tag: 'WorkerEpoch', value: self.value + 1 })
