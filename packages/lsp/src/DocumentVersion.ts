import * as Schema from 'effect/Schema'

/** Monotonic editor-owned version of one synchronized document. */
export const schema = Schema.TaggedStruct('DocumentVersion', { value: Schema.Natural })

export type DocumentVersion = typeof schema.Type

/** Creates a document version after its protocol boundary has validated the number. */
export const make = (value: number): DocumentVersion =>
  Object.freeze({ _tag: 'DocumentVersion', value })

/** Whether one document version is strictly newer than another. */
export const isAfter = (self: DocumentVersion, other: DocumentVersion): boolean =>
  self.value > other.value
