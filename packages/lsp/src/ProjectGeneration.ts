import * as Schema from 'effect/Schema'

/** Monotonic desired-state generation within one project association. */
export const schema = Schema.Struct({
  _tag: Schema.Literal('ProjectGeneration'),
  value: Schema.Natural,
})

export type ProjectGeneration = typeof schema.Type

export const initial: ProjectGeneration = Object.freeze({ _tag: 'ProjectGeneration', value: 0 })

/** Advances to the next project generation. */
export const next = (self: ProjectGeneration): ProjectGeneration =>
  Object.freeze({ _tag: 'ProjectGeneration', value: self.value + 1 })
