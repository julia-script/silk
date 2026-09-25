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

interface Shape {
  readonly known: ReadonlySet<string>
  readonly required: ReadonlyArray<string>
}

/** Field membership per tag, precomputed once: validation visits every record of every module. */
const shapes: ReadonlyMap<string, Shape> = new Map(
  [...fields].map(([tag, keys]) => {
    const optional = optionalFields.get(tag) ?? []
    return [tag, { known: new Set(keys), required: keys.filter((key) => !optional.includes(key)) }]
  }),
)

const invalid = (message: string): AuthoredEncodingError =>
  new AuthoredEncodingError({ reason: { _tag: 'InvalidArtifact', message } })

/** Reads data properties only: artifacts cannot hide source access behind getters. */
const property = (value: object, key: string): unknown =>
  Object.getOwnPropertyDescriptor(value, key)?.value

/** Direct reads for structure that `encode` has already proven to be plain data fields. */
const read = (value: object, key: string): unknown =>
  (value as Readonly<Record<string, unknown>>)[key]

const identityKeys = new WeakMap<object, string | undefined>()

const identityKey = (value: unknown): string | undefined => {
  if (value === null || typeof value !== 'object') return undefined
  if (identityKeys.has(value)) return identityKeys.get(value)
  const key = computeIdentityKey(value)
  identityKeys.set(value, key)
  return key
}

const computeIdentityKey = (value: object): string | undefined => {
  const path = read(value, 'path')
  if (!Array.isArray(path)) return undefined
  const parts: unknown[] = []
  for (const part of path) {
    if (part === null || typeof part !== 'object') return undefined
    parts.push([
      read(part, 'kind'),
      read(part, 'name'),
      read(part, 'role'),
      read(part, 'occurrence'),
    ])
  }
  return JSON.stringify([read(value, 'namespace'), read(value, 'module'), parts])
}

const localKey = (value: unknown): string | undefined => {
  if (value === null || typeof value !== 'object') return undefined
  const owner = identityKey(read(value, 'owner'))
  const path = read(value, 'path')
  if (owner === undefined || !Array.isArray(path)) return undefined
  const parts: unknown[] = []
  for (const part of path) {
    if (part === null || typeof part !== 'object') return undefined
    parts.push([read(part, 'role'), read(part, 'occurrence')])
  }
  return JSON.stringify([owner, parts])
}

const isWithinOwner = (value: unknown, parent: unknown): boolean => {
  // Lowering shares one identity object per owner, so most anchors resolve without comparing paths.
  if (value === parent) return true
  if (
    value === null ||
    typeof value !== 'object' ||
    parent === null ||
    typeof parent !== 'object'
  ) {
    return false
  }
  if (
    read(value, 'namespace') !== read(parent, 'namespace') ||
    read(value, 'module') !== read(parent, 'module')
  )
    return false
  const path = read(value, 'path')
  const parentPath = read(parent, 'path')
  if (!Array.isArray(path) || !Array.isArray(parentPath) || path.length < parentPath.length)
    return false
  const segmentKey = (part: unknown): string | undefined => {
    if (part === null || typeof part !== 'object') return undefined
    return JSON.stringify([
      read(part, 'kind'),
      read(part, 'name'),
      read(part, 'role'),
      read(part, 'occurrence'),
    ])
  }
  return parentPath.every((part, index) => segmentKey(part) === segmentKey(path[index]))
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
  const pending: Array<{ readonly value: unknown; readonly owner: unknown }> = [
    { value: self, owner: self.owner },
  ]
  while (pending.length > 0) {
    const item = pending.pop()
    if (item === undefined) break
    const value = item.value
    if (value === null || typeof value !== 'object') continue
    const tag = read(value, '_tag')
    let owner = item.owner
    if (tag === 'Declaration') {
      owner = read(value, 'owner')
      const key = identityKey(owner)
      if (
        key === undefined ||
        declarations.has(key) ||
        key === identityKey(item.owner) ||
        !isWithinOwner(owner, item.owner)
      ) {
        return yield* invalid('Declaration owners must be distinct within a module')
      }
      declarations.add(key)
    }
    const anchor = read(value, 'anchor')
    if (anchor !== undefined && anchor !== null && typeof anchor === 'object') {
      const anchorOwner = read(anchor, 'owner')
      if (tag === 'CallableExpression') {
        if (identityKey(anchorOwner) === identityKey(owner) || !isWithinOwner(anchorOwner, owner)) {
          return yield* invalid('Anonymous callable must introduce a nested authored owner')
        }
        owner = anchorOwner
      } else if (tag === 'Synthetic') {
        if (!isWithinOwner(owner, anchorOwner))
          return yield* invalid('Synthetic origin must be local or enclosing')
      } else if (identityKey(anchorOwner) !== identityKey(owner)) {
        return yield* invalid('Node anchor must belong to its containing authored owner')
      }
    }
    if (tag === 'LexicalReference' && !isWithinOwner(owner, read(value, 'owner'))) {
      return yield* invalid('Lexical captures must target the current or an enclosing owner')
    }
    if (visited.has(value)) continue
    visited.add(value)
    if (
      tag === 'AuthoredIdentity' &&
      (read(value, 'namespace') !== self.owner.namespace ||
        read(value, 'module') !== self.owner.module)
    )
      return yield* invalid('Authored identities and local references must belong to their module')
    if (typeof tag === 'string' && binderTags.has(tag)) {
      const key = localKey(read(value, 'anchor'))
      if (key === undefined || binders.has(key))
        return yield* invalid('Binder anchors must be distinct')
      binders.add(key)
    }
    if (tag === 'LexicalReference') {
      const key = localKey(value)
      if (key === undefined) return yield* invalid('Invalid lexical reference')
      references.push(key)
    }
    for (const child of Object.values(value)) pending.push({ value: child, owner })
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
  /** Validation walks the same structure without materializing bytes. */
  emit = true,
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
  if (emit) {
    frame(1, encoder.encode(`silk.authored.${domain}`))
    frame(2, encoder.encode(String(version)))
  }
  while (work.length > 0) {
    const next = work.pop()
    if (next === undefined) break
    if (next._tag === 'End') {
      active.delete(next.value)
      if (!emit) continue
      const size = output.length - next.lengthOffset - 4
      if (size > 0xffffffff) return yield* invalid('Canonical frame exceeds u32 length')
      output[next.lengthOffset] = (size >>> 24) & 255
      output[next.lengthOffset + 1] = (size >>> 16) & 255
      output[next.lengthOffset + 2] = (size >>> 8) & 255
      output[next.lengthOffset + 3] = size & 255
      continue
    }
    const value = next.value
    if (value === undefined) {
      if (emit) frame(3, [])
    } else if (typeof value === 'string') {
      if (/[\uD800-\uDFFF]/u.test(value))
        return yield* invalid('Text contains an unpaired surrogate')
      if (emit) frame(4, encoder.encode(value))
    } else if (typeof value === 'bigint') {
      if (emit) frame(5, encoder.encode(value.toString()))
    } else if (typeof value === 'number') {
      if (!Number.isSafeInteger(value) || Object.is(value, -0)) {
        return yield* invalid('Numbers must be exact safe integers; exact literals use bigint')
      }
      if (emit) frame(6, encoder.encode(String(value)))
    } else if (typeof value === 'boolean') {
      if (emit) frame(7, [value ? 1 : 0])
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
        // Holes and extra enumerable fields change the key count; non-enumerable and symbol keys
        // never reach the published clone, so they need no rejection here.
        if (Object.keys(value).length !== items.length) {
          return yield* invalid('Authored sequences cannot contain holes or extra fields')
        }
        active.add(value)
        const lengthOffset = output.length + 1
        if (emit) {
          output.push(8)
          length(0)
        }
        work.push({ _tag: 'End', value, lengthOffset })
        for (let index = items.length - 1; index >= 0; index -= 1) {
          work.push({ _tag: 'Value', value: items[index] })
        }
        continue
      }
      const tag = property(value, '_tag')
      const shape = typeof tag === 'string' ? shapes.get(tag) : undefined
      const keys = typeof tag === 'string' ? fields.get(tag) : undefined
      if (typeof tag !== 'string' || shape === undefined || keys === undefined) {
        return yield* invalid('Unknown authored record tag')
      }
      let enumerable = 0
      for (const key in value) {
        enumerable += 1
        if (key !== '_tag' && !shape.known.has(key)) {
          return yield* invalid(`Unexpected field in ${tag}`)
        }
        const descriptor = Object.getOwnPropertyDescriptor(value, key)
        if (descriptor === undefined || !('value' in descriptor)) {
          return yield* invalid(`Accessor field in ${tag}`)
        }
      }
      // Non-enumerable and symbol fields would silently vanish from the published clone.
      if (
        Object.getOwnPropertyNames(value).length !== enumerable ||
        Object.getOwnPropertySymbols(value).length !== 0
      ) {
        return yield* invalid(`Non-enumerable field in ${tag}`)
      }
      // Every own property is now a plain data field, so direct reads cannot run source access.
      const record = value as Readonly<Record<string, unknown>>
      for (const key of shape.required) {
        if (record[key] === undefined) return yield* invalid(`Missing ${tag}.${key}`)
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
            if (emit) frame(10, encoder.encode(entry.value))
          } else {
            const entry = pool.bytes[index]
            if (entry === undefined)
              return yield* invalid('Byte reference is outside its module pool')
            if (emit) frame(11, entry.value)
          }
          continue
        }
        if ((tag === 'TextRef' ? pool.texts[index] : pool.bytes[index]) === undefined) {
          return yield* invalid('Reference is outside its module pool')
        }
      }
      active.add(value)
      const lengthOffset = output.length + 1
      if (emit) {
        output.push(9)
        length(0)
        frame(4, encoder.encode(tag))
      }
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
  return output
})

/** Encodes only the declared header, dereferencing reachable module pool contents. */
export const header = Effect.fn('AuthoredEncoding.header')(function* (
  pool: AuthoredPool.Pool,
  declaration: AuthoredHir.Declaration,
): Effect.fn.Return<ReadonlyArray<number>, AuthoredEncodingError> {
  yield* validatePool(pool)
  return yield* encode(pool, declaration.header, 'header')
})

/** Encodes only the authored body in a domain distinct from its header. */
export const body = Effect.fn('AuthoredEncoding.body')(function* (
  pool: AuthoredPool.Pool,
  declaration: AuthoredHir.Declaration,
): Effect.fn.Return<ReadonlyArray<number>, AuthoredEncodingError> {
  yield* validatePool(pool)
  return yield* encode(pool, declaration.body, 'body')
})

// Published pools are never edited, and every declaration of a module shares its pool: validate
// each pool object once instead of once per encoded header or body.
const validatedPools = new WeakSet<AuthoredPool.Pool>()

const validatePool = Effect.fnUntraced(function* (
  pool: AuthoredPool.Pool,
): Effect.fn.Return<void, AuthoredEncodingError> {
  if (validatedPools.has(pool)) return
  yield* encode(pool, pool, 'artifact', false)
  validatedPools.add(pool)
})

/** Validates the closed artifact structure and every reachable pool reference. */
export const validate = Effect.fn('AuthoredEncoding.validate')(function* (
  self: AuthoredHir.Module,
): Effect.fn.Return<void, AuthoredEncodingError> {
  const descriptor = Object.getOwnPropertyDescriptor(self, 'pool')
  if (descriptor === undefined || !('value' in descriptor)) {
    return yield* invalid('Module pool must be an owned data property')
  }
  yield* encode(self.pool, self, 'artifact', false)
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
