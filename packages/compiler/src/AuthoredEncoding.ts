import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as AuthoredHir from './AuthoredHir.js'
import type * as AuthoredPool from './AuthoredPool.js'

/** Invalid authored structure is distinct from failure of the platform digest primitive. */
export class AuthoredEncodingError extends Data.TaggedError('AuthoredEncodingError')<{
  readonly reason:
    | { readonly _tag: 'InvalidArtifact'; readonly message: string }
    | { readonly _tag: 'DigestFailure'; readonly cause: unknown }
}> {}

/** Version of the canonical authored-content encoding, not a persistent storage format. */
export const version = 1

const encoder = new TextEncoder()

const identityFields: ReadonlyArray<readonly [string, ReadonlyArray<string>]> = [
  ['AuthoredIdentity', ['namespace', 'module', 'path']],
  ['OwnerSegment', ['kind', 'name', 'role', 'occurrence']],
  ['AuthoredAnchor', ['owner', 'path']],
  ['LocalSegment', ['role', 'occurrence']],
  ['AuthoredPool', ['texts', 'bytes']],
  ['PoolText', ['value', 'byteLength']],
  ['PoolBytes', ['value', 'byteLength']],
  ['TextRef', ['index']],
  ['BytesRef', ['index']],
]

const fields: ReadonlyMap<string, ReadonlyArray<string>> = new Map([
  ...identityFields,
  ...Object.entries(AuthoredHir.fields),
])

const optionalFields: ReadonlyMap<string, ReadonlyArray<string>> = new Map<
  string,
  readonly string[]
>([['OwnerSegment', ['name', 'role']], ...Object.entries(AuthoredHir.optionalFields)])

const invalid = (message: string): AuthoredEncodingError =>
  new AuthoredEncodingError({ reason: { _tag: 'InvalidArtifact', message } })

/** Reads data properties only: artifacts cannot hide source access behind getters. */
const property = (value: object, key: string): unknown =>
  Object.getOwnPropertyDescriptor(value, key)?.value

const identityKey = (value: unknown): string | undefined => {
  if (value === null || typeof value !== 'object') return undefined
  const path = property(value, 'path')
  if (!Array.isArray(path)) return undefined
  const parts: unknown[] = []
  for (const part of path) {
    if (part === null || typeof part !== 'object') return undefined
    parts.push([
      property(part, 'kind'),
      property(part, 'name'),
      property(part, 'role'),
      property(part, 'occurrence'),
    ])
  }
  return JSON.stringify([property(value, 'namespace'), property(value, 'module'), parts])
}

const localKey = (value: unknown): string | undefined => {
  if (value === null || typeof value !== 'object') return undefined
  const owner = identityKey(property(value, 'owner'))
  const path = property(value, 'path')
  if (owner === undefined || !Array.isArray(path)) return undefined
  const parts: unknown[] = []
  for (const part of path) {
    if (part === null || typeof part !== 'object') return undefined
    parts.push([property(part, 'role'), property(part, 'occurrence')])
  }
  return JSON.stringify([owner, parts])
}

const binderTags = new Set([
  'Parameter',
  'TypeParameter',
  'RowParameter',
  'LifetimeParameter',
  'BindingPattern',
  'UniversalPattern',
  'BindingStatement',
  'StaticForStatement',
])

const validateOwnership = Effect.fnUntraced(function* (
  self: AuthoredHir.Module,
): Effect.fn.Return<void, AuthoredEncodingError> {
  if (self.owner.path.length !== 0)
    return yield* invalid('Module owner must have an empty owner path')
  const declarations = new Set<string>()
  const binders = new Set<string>()
  const references: string[] = []
  const visited = new Set<object>()
  const pending: unknown[] = [self]
  while (pending.length > 0) {
    const value = pending.pop()
    if (value === null || typeof value !== 'object') continue
    const tag = property(value, '_tag')
    if (tag === 'Declaration') {
      const key = identityKey(property(value, 'owner'))
      if (key === undefined || declarations.has(key)) {
        return yield* invalid('Declaration owners must be distinct within a module')
      }
      declarations.add(key)
    }
    if (visited.has(value)) continue
    visited.add(value)
    if (
      tag === 'AuthoredIdentity' &&
      (property(value, 'namespace') !== self.owner.namespace ||
        property(value, 'module') !== self.owner.module)
    )
      return yield* invalid('Authored identities and local references must belong to their module')
    if (typeof tag === 'string' && binderTags.has(tag)) {
      const key = localKey(property(value, 'anchor'))
      if (key === undefined || binders.has(key))
        return yield* invalid('Binder anchors must be distinct')
      binders.add(key)
    }
    if (tag === 'LexicalReference') {
      const key = localKey(value)
      if (key === undefined) return yield* invalid('Invalid lexical reference')
      references.push(key)
    }
    for (const child of Object.values(value)) pending.push(child)
  }
  if (references.some((key) => !binders.has(key))) {
    return yield* invalid('Lexical reference has no authored binder in its module')
  }
})

type Work =
  | { readonly _tag: 'Value'; readonly value: unknown }
  | { readonly _tag: 'End'; readonly value: object; readonly lengthOffset: number }

/**
 * One iterative walk supplies both structural validation and canonical framing. No recursion depth
 * or mutable traversal state survives publication. This is a closed record vocabulary, not a
 * general object serializer: unknown tags, fields, prototypes, accessors and cycles are rejected.
 */
const encode = Effect.fnUntraced(function* (
  pool: AuthoredPool.Pool,
  root: unknown,
  domain: 'header' | 'body' | 'artifact',
): Effect.fn.Return<ReadonlyArray<number>, AuthoredEncodingError> {
  const output: number[] = []
  const active = new Set<object>()
  const work: Work[] = [{ _tag: 'Value', value: root }]
  const length = (value: number) => {
    output.push((value >>> 24) & 255, (value >>> 16) & 255, (value >>> 8) & 255, value & 255)
  }
  const frame = (tag: number, payload: ReadonlyArray<number> | Uint8Array) => {
    output.push(tag)
    length(payload.length)
    for (const byte of payload) output.push(byte)
  }
  frame(1, encoder.encode(`silk.authored.${domain}`))
  frame(2, encoder.encode(String(version)))
  while (work.length > 0) {
    const next = work.pop()
    if (next === undefined) break
    if (next._tag === 'End') {
      const size = output.length - next.lengthOffset - 4
      if (size > 0xffffffff) return yield* invalid('Canonical frame exceeds u32 length')
      output[next.lengthOffset] = (size >>> 24) & 255
      output[next.lengthOffset + 1] = (size >>> 16) & 255
      output[next.lengthOffset + 2] = (size >>> 8) & 255
      output[next.lengthOffset + 3] = size & 255
      active.delete(next.value)
      continue
    }
    const value = next.value
    if (value === undefined) {
      frame(3, [])
    } else if (typeof value === 'string') {
      if (/[\uD800-\uDFFF]/u.test(value))
        return yield* invalid('Text contains an unpaired surrogate')
      frame(4, encoder.encode(value))
    } else if (typeof value === 'bigint') {
      frame(5, encoder.encode(value.toString()))
    } else if (typeof value === 'number') {
      if (!Number.isSafeInteger(value) || Object.is(value, -0)) {
        return yield* invalid('Numbers must be exact safe integers; exact literals use bigint')
      }
      frame(6, encoder.encode(String(value)))
    } else if (typeof value === 'boolean') {
      frame(7, [value ? 1 : 0])
    } else if (value === null || typeof value !== 'object') {
      return yield* invalid('Unsupported authored value')
    } else {
      if (active.has(value)) return yield* invalid('Authored artifact contains a cycle')
      const isArray = Array.isArray(value)
      if (!isArray && Object.getPrototypeOf(value) !== Object.prototype) {
        return yield* invalid('Authored records must be plain data')
      }
      if (isArray) {
        const items: readonly unknown[] = value
        if (Reflect.ownKeys(value).length !== items.length + 1) {
          return yield* invalid('Authored sequences cannot contain holes or extra fields')
        }
        active.add(value)
        output.push(8)
        const lengthOffset = output.length
        length(0)
        work.push({ _tag: 'End', value, lengthOffset })
        for (let index = items.length - 1; index >= 0; index -= 1) {
          const descriptor = Object.getOwnPropertyDescriptor(value, String(index))
          if (descriptor === undefined || !('value' in descriptor) || !descriptor.enumerable) {
            return yield* invalid('Authored sequences cannot contain accessors')
          }
          work.push({ _tag: 'Value', value: items[index] })
        }
        continue
      }
      const tag = property(value, '_tag')
      const keys = typeof tag === 'string' ? fields.get(tag) : undefined
      if (typeof tag !== 'string' || keys === undefined) {
        return yield* invalid('Unknown authored record tag')
      }
      for (const key of Reflect.ownKeys(value)) {
        if (typeof key !== 'string' || (key !== '_tag' && !keys.includes(key))) {
          return yield* invalid(`Unexpected field in ${tag}`)
        }
        const descriptor = Object.getOwnPropertyDescriptor(value, key)
        if (descriptor === undefined || !('value' in descriptor) || !descriptor.enumerable) {
          return yield* invalid(`Accessor or non-enumerable field in ${tag}`)
        }
      }
      const optional = optionalFields.get(tag) ?? []
      for (const key of keys) {
        if (property(value, key) === undefined && !optional.includes(key)) {
          return yield* invalid(`Missing ${tag}.${key}`)
        }
      }
      if (tag === 'OwnerSegment' || tag === 'LocalSegment' || tag === 'Synthetic') {
        const occurrence = property(value, 'occurrence')
        if (typeof occurrence !== 'number' || !Number.isSafeInteger(occurrence) || occurrence < 0) {
          return yield* invalid('Owner-local occurrences must be nonnegative safe integers')
        }
      }
      if (tag === 'AuthoredIdentity') {
        const module = property(value, 'module')
        const namespace = property(value, 'namespace')
        if (
          typeof module !== 'string' ||
          typeof namespace !== 'string' ||
          module.includes('\\') ||
          module.includes(':') ||
          namespace.startsWith('/') ||
          module.split('/').some((part) => part === '' || part === '.' || part === '..')
        ) {
          return yield* invalid('Owner identity must use a logical namespace and canonical module')
        }
      }
      if (tag === 'PoolText') {
        const text = property(value, 'value')
        if (
          typeof text !== 'string' ||
          /[\uD800-\uDFFF]/u.test(text) ||
          property(value, 'byteLength') !== encoder.encode(text).length
        ) {
          return yield* invalid('Text pool entry has invalid text or UTF-8 length')
        }
      }
      if (tag === 'PoolBytes') {
        const bytes: unknown = property(value, 'value')
        if (!Array.isArray(bytes) || property(value, 'byteLength') !== bytes.length) {
          return yield* invalid('Byte pool entry has an invalid length')
        }
        for (let index = 0; index < bytes.length; index += 1) {
          const byte = property(bytes, String(index))
          if (typeof byte !== 'number' || !Number.isInteger(byte) || byte < 0 || byte > 255) {
            return yield* invalid('Byte pool entry contains a non-byte')
          }
        }
      }
      if (tag === 'CharacterLiteral') {
        const scalar = property(value, 'scalar')
        if (
          typeof scalar !== 'number' ||
          !Number.isInteger(scalar) ||
          scalar < 0 ||
          scalar > 0x10ffff ||
          (scalar >= 0xd800 && scalar <= 0xdfff)
        ) {
          return yield* invalid('Character literal must contain a Unicode scalar')
        }
      }
      if (tag === 'FloatingLiteral' || tag === 'DurationComponent') {
        const magnitude = property(value, tag === 'FloatingLiteral' ? 'coefficient' : 'magnitude')
        if (typeof magnitude !== 'bigint' || magnitude < 0n) {
          return yield* invalid('Exact literal magnitude must be a nonnegative bigint')
        }
      }
      if (tag.startsWith('Missing') || tag.startsWith('Invalid')) {
        const causes = property(value, 'causes')
        if (!Array.isArray(causes) || causes.length === 0) {
          return yield* invalid('Recovered records must retain at least one cause')
        }
      }
      if (tag === 'TextRef' || tag === 'BytesRef') {
        const index = property(value, 'index')
        if (typeof index !== 'number' || !Number.isSafeInteger(index) || index < 0) {
          return yield* invalid('Invalid pool reference')
        }
        if (domain !== 'artifact') {
          if (tag === 'TextRef') {
            const entry = pool.texts[index]
            if (entry === undefined)
              return yield* invalid('Text reference is outside its module pool')
            frame(10, encoder.encode(entry.value))
          } else {
            const entry = pool.bytes[index]
            if (entry === undefined)
              return yield* invalid('Byte reference is outside its module pool')
            frame(11, entry.value)
          }
          continue
        }
        if ((tag === 'TextRef' ? pool.texts[index] : pool.bytes[index]) === undefined) {
          return yield* invalid('Reference is outside its module pool')
        }
      }
      active.add(value)
      output.push(9)
      const lengthOffset = output.length
      length(0)
      frame(4, encoder.encode(tag))
      work.push({ _tag: 'End', value, lengthOffset })
      // Owner identity belongs to the identity channel. Content anchors are owner-relative.
      const contentKeys =
        domain === 'artifact'
          ? keys
          : keys.filter(
              (key) => !((tag === 'AuthoredAnchor' || tag === 'Declaration') && key === 'owner'),
            )
      for (let index = contentKeys.length - 1; index >= 0; index -= 1) {
        const key = contentKeys[index]
        if (key !== undefined) work.push({ _tag: 'Value', value: property(value, key) })
      }
    }
  }
  return Object.freeze(output)
})

/** Encodes only the declared header, dereferencing reachable module pool contents. */
export const header = Effect.fn('AuthoredEncoding.header')(function* (
  pool: AuthoredPool.Pool,
  declaration: AuthoredHir.Declaration,
): Effect.fn.Return<ReadonlyArray<number>, AuthoredEncodingError> {
  yield* encode(pool, pool, 'artifact')
  return yield* encode(pool, declaration.header, 'header')
})

/** Encodes only the authored body in a domain distinct from its header. */
export const body = Effect.fn('AuthoredEncoding.body')(function* (
  pool: AuthoredPool.Pool,
  declaration: AuthoredHir.Declaration,
): Effect.fn.Return<ReadonlyArray<number>, AuthoredEncodingError> {
  yield* encode(pool, pool, 'artifact')
  return yield* encode(pool, declaration.body, 'body')
})

/** Validates the closed artifact structure and every reachable pool reference. */
export const validate = Effect.fn('AuthoredEncoding.validate')(function* (
  self: AuthoredHir.Module,
): Effect.fn.Return<void, AuthoredEncodingError> {
  const descriptor = Object.getOwnPropertyDescriptor(self, 'pool')
  if (descriptor === undefined || !('value' in descriptor)) {
    return yield* invalid('Module pool must be an owned data property')
  }
  yield* encode(self.pool, self, 'artifact')
  yield* validateOwnership(self)
})

/** SHA-256 of canonical bytes; equality is never a semantic-reuse or owner-identity proof. */
export const digest = Effect.fn('AuthoredEncoding.digest')(function* (
  bytes: ReadonlyArray<number>,
): Effect.fn.Return<string, AuthoredEncodingError> {
  const owned: number[] = []
  for (const byte of bytes) {
    if (!Number.isInteger(byte) || byte < 0 || byte > 255) {
      return yield* invalid('Digest input must contain bytes')
    }
    owned.push(byte)
  }
  const result = yield* Effect.tryPromise({
    try: () => globalThis.crypto.subtle.digest('SHA-256', Uint8Array.from(owned)),
    catch: (cause) => new AuthoredEncodingError({ reason: { _tag: 'DigestFailure', cause } }),
  })
  return Array.from(new Uint8Array(result), (byte) => byte.toString(16).padStart(2, '0')).join('')
})
