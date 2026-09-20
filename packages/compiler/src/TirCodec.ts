import type * as DeclarationFacts from './DeclarationFacts.js'
import * as Elaboration from './Elaboration.js'
import type * as SemanticContext from './SemanticContext.js'
import * as LifetimeFlow from './LifetimeFlow.js'
import * as SourceSpan from './SourceSpan.js'
import * as Tir from './Tir.js'

/**
 * The portable form of a checked body.
 *
 * A body is revision-free content: nodes, tables and authored origins. Two things in memory are
 * not content and never reach the encoding. A position is presentation, stamped again from the
 * origin beside it when the body is decoded for a revision. The owning declaration is this
 * revision's header object, named by id and resolved again by the reader.
 */
type Encoded =
  | null
  | boolean
  | number
  | string
  | ReadonlyArray<Encoded>
  | { readonly [key: string]: Encoded }

const tag = '$'

const encodeValue = (input: unknown): Encoded => {
  if (input === null || typeof input === 'boolean' || typeof input === 'string') return input
  if (typeof input === 'number') {
    if (!Number.isFinite(input)) throw new RangeError('A checked body holds a non-finite number')
    return input
  }
  if (typeof input === 'bigint') return { [tag]: 'bigint', value: input.toString() }
  if (typeof input !== 'object') throw new RangeError(`A checked body holds a ${typeof input}`)
  if (SourceSpan.isSourceSpan(input)) return { [tag]: 'span', source: input.sourceId }
  if (Array.isArray(input)) return input.map(encodeValue)
  if (input instanceof Map)
    return {
      [tag]: 'map',
      entries: [...input].map(([key, value]) => [encodeValue(key), encodeValue(value)]),
    }
  if (input instanceof Set) return { [tag]: 'set', values: [...input].map(encodeValue) }
  if (input instanceof Uint8Array) return { [tag]: 'bytes', values: [...input] }
  const result: Record<string, Encoded> = {}
  // Keys are written in one order so equal bodies encode to equal text.
  for (const key of Object.keys(input).sort()) {
    const value = (input as Readonly<Record<string, unknown>>)[key]
    if (value !== undefined) result[key] = encodeValue(value)
  }
  return result
}

const decodeValue = (input: Encoded): unknown => {
  if (input === null || typeof input !== 'object') return input
  if (Array.isArray(input)) return Object.freeze(input.map(decodeValue))
  const record = input as { readonly [key: string]: Encoded }
  switch (record[tag]) {
    case 'bigint':
      return BigInt(record['value'] as string)
    case 'span':
      // A placeholder: the reader stamps every position from the origin beside it.
      return SourceSpan.fromOffsets(record['source'] as string, 0, 0)
    case 'map':
      return new Map(
        (record['entries'] as ReadonlyArray<readonly [Encoded, Encoded]>).map(([key, value]) => [
          decodeValue(key),
          decodeValue(value),
        ]),
      )
    case 'set':
      return new Set((record['values'] as ReadonlyArray<Encoded>).map(decodeValue))
    case 'bytes':
      return Uint8Array.from(record['values'] as ReadonlyArray<number>)
  }
  const result: Record<string, unknown> = {}
  for (const key of Object.keys(record)) result[key] = decodeValue(record[key] ?? null)
  return Object.freeze(result)
}

/** Writes one checked body as canonical text. Its declaration is written as its id alone. */
export const encode = (self: Elaboration.CheckedBody): string =>
  JSON.stringify(
    encodeValue({
      artifact: self.artifact,
      declaration: self.hidden ? self.declaration : self.declaration.id,
      hidden: self.hidden,
      function: { ...self.function, declaration: undefined },
      results:
        self.results.lifetimes === undefined
          ? self.results
          : { ...self.results, lifetimes: LifetimeFlow.content(self.results.lifetimes) },
    }),
  )

/** Reads a checked body back for one revision: its header is resolved and its positions stamped. */
export const decode = (
  text: string,
  declarationOf: (id: DeclarationFacts.DeclarationId) => DeclarationFacts.DeclarationFact,
  context: SemanticContext.SemanticContext,
): Elaboration.CheckedBody => {
  const decoded = decodeValue(JSON.parse(text) as Encoded) as {
    readonly artifact: Tir.ArtifactId
    readonly declaration: DeclarationFacts.DeclarationFact | DeclarationFacts.DeclarationId
    readonly hidden: boolean
    readonly function: Omit<Tir.TirFunction, 'declaration'>
    readonly results: Elaboration.BodyResults
  }
  const declaration =
    decoded.declaration._tag === 'DeclarationId'
      ? declarationOf(decoded.declaration)
      : Tir.stamp(decoded.declaration, context.spanOf)
  return Elaboration.presentBody(
    {
      artifact: decoded.artifact,
      declaration,
      hidden: decoded.hidden,
      function: { ...decoded.function, declaration },
      results: decoded.results,
    },
    context,
  )
}
