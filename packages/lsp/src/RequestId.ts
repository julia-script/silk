import * as Schema from 'effect/Schema'

/** Identity of one cancellable semantic query. */
export const schema = Schema.TaggedStruct('RequestId', { value: Schema.Natural })

export type RequestId = typeof schema.Type

export const initial: RequestId = Object.freeze({ _tag: 'RequestId', value: 0 })

/** Allocates the next request identity in one workspace-engine lifetime. */
export const next = (self: RequestId): RequestId =>
  Object.freeze({ _tag: 'RequestId', value: self.value + 1 })
