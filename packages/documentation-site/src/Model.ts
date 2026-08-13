/**
 * The renderer's own view of the documentation JSON.
 *
 * These shapes are declared here rather than imported from the emitter, and the package declares no
 * workspace dependency at all, so there is no compiler package installed under it to import even by
 * accident. That is the whole boundary: `silk-documentation-json` requires the emitted value to be
 * readable "without importing compiler internals or reparsing source comments", and a consumer that
 * could reach for those internals is not evidence that the requirement holds.
 *
 * Decoding is strict at the top and lenient below it. The top level is the difference between
 * documentation JSON and some other file, so a missing schema marker is reported. Everything below
 * is a schema chartered as experimental and free to change without migration, so a field that is
 * absent falls back rather than failing a whole site over one unfamiliar shape.
 */

export interface SourceRange {
  readonly sourceId: string
  readonly start: number
  readonly end: number
}

/**
 * A documentation document, with its block tree left as `unknown`.
 *
 * Prose rendering walks that tree defensively rather than against a closed union, because a block
 * tag added to the schema later must degrade to text instead of throwing.
 */
export interface Document {
  readonly markdown: string
  readonly blocks: ReadonlyArray<unknown>
  readonly fallback: boolean
}

export interface Item {
  readonly id: string
  readonly kind: string
  readonly name: string
  readonly visibility: string
  readonly signature: string
  readonly source?: SourceRange
  readonly documentation?: Document
  readonly children: ReadonlyArray<Item>
}

export interface Module {
  readonly name: string
  readonly sourceId: string
  readonly documentation?: Document
  readonly items: ReadonlyArray<Item>
}

export interface Documentation {
  readonly schema: string
  readonly experimental: boolean
  readonly modules: ReadonlyArray<Module>
}

/** The marker the emitter writes so a consumer can tell this file from any other JSON. */
export const schemaName = 'silk-documentation'

export type Decoded =
  | { readonly _tag: 'Decoded'; readonly documentation: Documentation }
  | { readonly _tag: 'NotDocumentation'; readonly message: string }

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === 'object' && value !== null && !Array.isArray(value)

const stringAt = (value: Record<string, unknown>, key: string, fallback: string): string =>
  typeof value[key] === 'string' ? value[key] : fallback

const arrayAt = (value: Record<string, unknown>, key: string): ReadonlyArray<unknown> =>
  Array.isArray(value[key]) ? value[key] : []

const rangeOf = (value: unknown): SourceRange | undefined => {
  if (!isRecord(value)) return undefined
  const { sourceId, start, end } = value
  return typeof sourceId === 'string' && typeof start === 'number' && typeof end === 'number'
    ? Object.freeze({ sourceId, start, end })
    : undefined
}

const documentOf = (value: unknown): Document | undefined => {
  if (!isRecord(value)) return undefined
  return Object.freeze({
    markdown: stringAt(value, 'markdown', ''),
    blocks: Object.freeze(arrayAt(value, 'blocks')),
    fallback: value.fallback === true,
  })
}

/** The emitter nests a signature as `{ text }`; a bare string is accepted for the same reason. */
const signatureOf = (value: unknown): string => {
  if (typeof value === 'string') return value
  return isRecord(value) ? stringAt(value, 'text', '') : ''
}

const itemOf = (value: unknown): Item | undefined => {
  if (!isRecord(value)) return undefined
  const documentation = documentOf(value.documentation)
  const source = rangeOf(value.source)
  return Object.freeze({
    id: stringAt(value, 'id', ''),
    kind: stringAt(value, 'kind', 'Unknown'),
    name: stringAt(value, 'name', ''),
    visibility: stringAt(value, 'visibility', 'Inherited'),
    signature: signatureOf(value.signature),
    ...(source === undefined ? {} : { source }),
    ...(documentation === undefined ? {} : { documentation }),
    children: Object.freeze(arrayAt(value, 'children').flatMap((child) => itemOf(child) ?? [])),
  })
}

const moduleOf = (value: unknown): Module | undefined => {
  if (!isRecord(value)) return undefined
  const name = stringAt(value, 'name', '')
  if (name === '') return undefined
  const documentation = documentOf(value.documentation)
  return Object.freeze({
    name,
    sourceId: stringAt(value, 'sourceId', name),
    ...(documentation === undefined ? {} : { documentation }),
    items: Object.freeze(arrayAt(value, 'items').flatMap((item) => itemOf(item) ?? [])),
  })
}

/**
 * Reads a parsed JSON value as documentation.
 *
 * Reports rather than throws, because the command reading a file the user named has to say which
 * file was wrong instead of dying partway through writing a site.
 */
export const decode = (value: unknown): Decoded => {
  if (!isRecord(value))
    return Object.freeze({
      _tag: 'NotDocumentation',
      message: 'expected a documentation JSON object',
    })
  const schema = value.schema
  if (schema !== schemaName)
    return Object.freeze({
      _tag: 'NotDocumentation',
      message: `expected a "${schemaName}" schema marker, found ${JSON.stringify(schema) ?? 'nothing'}`,
    })
  if (!Array.isArray(value.modules))
    return Object.freeze({
      _tag: 'NotDocumentation',
      message: 'expected a module list',
    })
  return Object.freeze({
    _tag: 'Decoded',
    documentation: Object.freeze({
      schema,
      experimental: value.experimental === true,
      modules: Object.freeze(arrayAt(value, 'modules').flatMap((module) => moduleOf(module) ?? [])),
    }),
  })
}

/** The first paragraph of a document, collapsed to one line. Used for listings and search. */
export const summary = (self: Document | undefined): string =>
  self === undefined
    ? ''
    : (self.markdown.trim().split('\n\n').at(0) ?? '').replace(/\s+/g, ' ').trim()
