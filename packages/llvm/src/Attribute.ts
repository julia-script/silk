import * as Effect from 'effect/Effect'
import type * as Builder from './Builder.js'
import * as ByteString from './ByteString.js'
import type * as AttributeDescription from './internal/AttributeDescription.js'
import * as BuilderState from './internal/BuilderState.js'
import * as CanonicalKey from './internal/CanonicalKey.js'
import * as Handle from './internal/Handle.js'
import { SilkError } from './SilkError.js'
import type * as Type from './Type.js'

/**
 * Opaque identity for one structurally interned LLVM attribute.
 *
 * @category attributes
 * @since 0.0.0
 */
export interface Attribute extends Handle.Handle<'Attribute'> {}

/**
 * Opaque identity for a canonical, conflict-free set of attributes.
 *
 * @category attributes
 * @since 0.0.0
 */
export interface Set extends Handle.Handle<'AttributeSet'> {}

/**
 * Opaque identity for function, return, and parameter attribute groups.
 *
 * @category attributes
 * @since 0.0.0
 */
export interface FunctionSet extends Handle.Handle<'FunctionAttributeSet'> {}

/**
 * Attribute groups accepted by {@link functionSet}; omitted groups become empty sets.
 *
 * @category attributes
 * @since 0.0.0
 */
export interface FunctionSetInput {
  readonly functionAttributes?: Set
  readonly returnAttributes?: Set
  readonly parameterAttributes?: ReadonlyArray<Set>
}

/**
 * Materialized groups returned by {@link functionSetEntries}.
 *
 * @category attributes
 * @since 0.0.0
 */
export interface FunctionSetEntries {
  readonly functionAttributes: Set
  readonly returnAttributes: Set
  readonly parameterAttributes: ReadonlyArray<Set>
}

/** @internal */
const bytes = (value: ByteString.ByteString | Uint8Array | string): ByteString.ByteString =>
  typeof value === 'string'
    ? ByteString.fromString(value)
    : value instanceof Uint8Array
      ? ByteString.fromUint8Array(value)
      : value

/** @internal */
const descriptionKey = (description: AttributeDescription.Description): string => {
  const name = CanonicalKey.bytes(description.name)
  switch (description._tag) {
    case 'Flag':
      return CanonicalKey.tagged('flag', [name])
    case 'Integer':
      return CanonicalKey.tagged('integer', [name, CanonicalKey.integer(description.value)])
    case 'Type':
      return CanonicalKey.tagged('type', [name, CanonicalKey.integer(description.type)])
    case 'String':
      return CanonicalKey.tagged('string', [name, CanonicalKey.bytes(description.value)])
    case 'IntegerList':
      return CanonicalKey.tagged('integer-list', [
        name,
        CanonicalKey.sequence(description.values.map(CanonicalKey.integer)),
      ])
  }
}

/** @internal */
const handleAt = (
  handles: ReadonlyArray<Attribute>,
  index: number,
  operation: string,
): Attribute => {
  const handle = handles[index]
  if (handle === undefined) {
    throw new SilkError({ operation, message: 'Attribute table handle is missing', cause: index })
  }
  return handle
}

/** @internal */
const setHandleAt = (handles: ReadonlyArray<Set>, index: number, operation: string): Set => {
  const handle = handles[index]
  if (handle === undefined) {
    throw new SilkError({ operation, message: 'Attribute-set handle is missing', cause: index })
  }
  return handle
}

/** @internal */
const intern = Effect.fn('Attribute.intern')(function* (
  builder: Builder.Builder,
  description: AttributeDescription.Description,
) {
  return yield* BuilderState.mutate(builder, 'Attribute.intern', (state, owner) => {
    const key = descriptionKey(description)
    const found = state.attributeKeys.get(key)
    if (found !== undefined) return handleAt(state.attributeHandles, found, 'Attribute.intern')
    const index = state.attributes.length
    const handle = Handle.make('Attribute', owner, index)
    state.attributes.push(description)
    state.attributeHandles.push(handle)
    state.attributeKeys.set(key, index)
    return handle
  })
})

/** @internal */
const validateName = (
  name: ByteString.ByteString | Uint8Array | string,
  operation: string,
): Effect.Effect<ByteString.ByteString, SilkError> => {
  const value = bytes(name)
  return ByteString.isEmpty(value)
    ? Effect.fail(
        new SilkError({ operation, message: 'An LLVM attribute requires a name', cause: name }),
      )
    : Effect.succeed(value)
}

/**
 * Creates or reuses a valueless attribute such as `nounwind`.
 *
 * @category attributes
 * @since 0.0.0
 */
export const flag = Effect.fn('Attribute.flag')(function* (
  builder: Builder.Builder,
  name: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Attribute, SilkError> {
  const value = yield* validateName(name, 'Attribute.flag')
  return yield* intern(builder, Object.freeze({ _tag: 'Flag', name: value }))
})

/**
 * Creates or reuses an attribute with an unsigned 64-bit integer payload.
 *
 * @category attributes
 * @since 0.0.0
 */
export const integer = Effect.fn('Attribute.integer')(function* (
  builder: Builder.Builder,
  name: ByteString.ByteString | Uint8Array | string,
  value: number | bigint,
): Effect.fn.Return<Attribute, SilkError> {
  const attributeName = yield* validateName(name, 'Attribute.integer')
  const exact = typeof value === 'bigint' ? value : BigInt(value)
  if (exact < 0n || exact > 0xffff_ffff_ffff_ffffn) {
    return yield* Effect.fail(
      new SilkError({
        operation: 'Attribute.integer',
        message: 'Integer attribute payload must be an unsigned 64-bit integer',
        cause: value,
      }),
    )
  }
  return yield* intern(
    builder,
    Object.freeze({ _tag: 'Integer', name: attributeName, value: exact }),
  )
})

/**
 * Creates or reuses an attribute whose payload is a builder-owned LLVM type.
 *
 * @category attributes
 * @since 0.0.0
 */
export const typeAttribute = Effect.fn('Attribute.typeAttribute')(function* (
  builder: Builder.Builder,
  name: ByteString.ByteString | Uint8Array | string,
  type: Type.Type,
): Effect.fn.Return<Attribute, SilkError> {
  const attributeName = yield* validateName(name, 'Attribute.typeAttribute')
  const typeIndex = yield* BuilderState.mutate(
    builder,
    'Attribute.typeAttribute',
    (_state, owner) => Handle.resolve(builder, owner, type, 'Type', 'Attribute.typeAttribute'),
  )
  return yield* intern(
    builder,
    Object.freeze({ _tag: 'Type', name: attributeName, type: typeIndex }),
  )
})

/**
 * Creates or reuses a named string attribute, including an explicitly empty value.
 *
 * @category attributes
 * @since 0.0.0
 */
export const string = Effect.fn('Attribute.string')(function* (
  builder: Builder.Builder,
  name: ByteString.ByteString | Uint8Array | string,
  value: ByteString.ByteString | Uint8Array | string = ByteString.empty,
): Effect.fn.Return<Attribute, SilkError> {
  const attributeName = yield* validateName(name, 'Attribute.string')
  return yield* intern(
    builder,
    Object.freeze({ _tag: 'String', name: attributeName, value: bytes(value) }),
  )
})

/**
 * Creates or reuses a named list of unsigned 64-bit integer payloads.
 *
 * @category attributes
 * @since 0.0.0
 */
export const integerList = Effect.fn('Attribute.integerList')(function* (
  builder: Builder.Builder,
  name: ByteString.ByteString | Uint8Array | string,
  values: ReadonlyArray<number | bigint>,
): Effect.fn.Return<Attribute, SilkError> {
  const attributeName = yield* validateName(name, 'Attribute.integerList')
  const exact = Object.freeze(values.map((value) => BigInt(value)))
  if (exact.some((value) => value < 0n || value > 0xffff_ffff_ffff_ffffn)) {
    return yield* Effect.fail(
      new SilkError({
        operation: 'Attribute.integerList',
        message: 'Attribute list values must be unsigned 64-bit integers',
        cause: values,
      }),
    )
  }
  return yield* intern(
    builder,
    Object.freeze({ _tag: 'IntegerList', name: attributeName, values: exact }),
  )
})

/** @internal */
const internSet = Effect.fn('Attribute.internSet')(function* (
  builder: Builder.Builder,
  attributeIndices: ReadonlyArray<number>,
): Effect.fn.Return<Set, SilkError> {
  return yield* BuilderState.mutate(builder, 'Attribute.set', (state, owner) => {
    const ordered = [...new Set(attributeIndices)].sort((left, right) => {
      const leftDescription = state.attributes[left]
      const rightDescription = state.attributes[right]
      if (leftDescription === undefined || rightDescription === undefined) return left - right
      return descriptionKey(leftDescription).localeCompare(descriptionKey(rightDescription))
    })
    const names = new Map<string, number>()
    for (const index of ordered) {
      const description = state.attributes[index]
      if (description === undefined) {
        throw new SilkError({
          operation: 'Attribute.set',
          message: 'Attribute table entry is missing',
          cause: index,
        })
      }
      const name = CanonicalKey.bytes(description.name)
      const previous = names.get(name)
      if (previous !== undefined && previous !== index) {
        throw new SilkError({
          operation: 'Attribute.set',
          message: 'An attribute set cannot contain conflicting values for one name',
          cause: { previous, index },
        })
      }
      names.set(name, index)
    }
    const values = Object.freeze(ordered)
    const key = CanonicalKey.sequence(values.map(CanonicalKey.integer))
    const found = state.attributeSetKeys.get(key)
    if (found !== undefined) {
      return setHandleAt(state.attributeSetHandles, found, 'Attribute.set')
    }
    const index = state.attributeSets.length
    const handle = Handle.make('AttributeSet', owner, index)
    state.attributeSets.push(values)
    state.attributeSetHandles.push(handle)
    state.attributeSetKeys.set(key, index)
    return handle
  })
})

/**
 * Canonicalizes attributes into a deterministic set.
 *
 * **Details**
 *
 * Duplicates collapse, and ordering is structural rather than insertion-based.
 *
 * **Gotchas**
 *
 * Two different values for the same attribute name fail instead of producing ambiguous LLVM IR.
 *
 * **Example** (Canonicalizing an attribute set)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Attribute from '@silk-effect/llvm/Attribute'
 * import * as Builder from '@silk-effect/llvm/Builder'
 *
 * const attributes = await Effect.runPromise(
 *   Effect.gen(function* () {
 *     const builder = yield* Builder.make()
 *     const nounwind = yield* Attribute.flag(builder, 'nounwind')
 *     return yield* Attribute.set(builder, [nounwind])
 *   }),
 * )
 * ```
 *
 * @category attributes
 * @since 0.0.0
 */
export const set = Effect.fn('Attribute.set')(function* (
  builder: Builder.Builder,
  attributes: ReadonlyArray<Attribute>,
): Effect.fn.Return<Set, SilkError> {
  const values = yield* BuilderState.mutate(builder, 'Attribute.set', (_state, owner) =>
    Object.freeze(
      attributes.map((attribute) =>
        Handle.resolve(builder, owner, attribute, 'Attribute', 'Attribute.set'),
      ),
    ),
  )
  return yield* internSet(builder, values)
})

/**
 * Returns the canonical set produced by adding one attribute.
 *
 * @category attributes
 * @since 0.0.0
 */
export const add = Effect.fn('Attribute.add')(function* (
  builder: Builder.Builder,
  self: Set,
  attribute: Attribute,
): Effect.fn.Return<Set, SilkError> {
  const values = yield* BuilderState.mutate(builder, 'Attribute.add', (state, owner) => {
    const setIndex = Handle.resolve(builder, owner, self, 'AttributeSet', 'Attribute.add')
    const attributeIndex = Handle.resolve(builder, owner, attribute, 'Attribute', 'Attribute.add')
    const existing = state.attributeSets[setIndex]
    if (existing === undefined) {
      throw new SilkError({
        operation: 'Attribute.add',
        message: 'Attribute set is missing',
        cause: self,
      })
    }
    return Object.freeze([...existing, attributeIndex])
  })
  return yield* internSet(builder, values)
})

/**
 * Returns the canonical set produced by removing every occurrence of one attribute.
 *
 * @category attributes
 * @since 0.0.0
 */
export const remove = Effect.fn('Attribute.remove')(function* (
  builder: Builder.Builder,
  self: Set,
  attribute: Attribute,
): Effect.fn.Return<Set, SilkError> {
  const values = yield* BuilderState.mutate(builder, 'Attribute.remove', (state, owner) => {
    const setIndex = Handle.resolve(builder, owner, self, 'AttributeSet', 'Attribute.remove')
    const attributeIndex = Handle.resolve(
      builder,
      owner,
      attribute,
      'Attribute',
      'Attribute.remove',
    )
    const existing = state.attributeSets[setIndex]
    if (existing === undefined) {
      throw new SilkError({
        operation: 'Attribute.remove',
        message: 'Attribute set is missing',
        cause: self,
      })
    }
    return Object.freeze(existing.filter((index) => index !== attributeIndex))
  })
  return yield* internSet(builder, values)
})

/**
 * Expands a canonical set into its deterministic attribute order.
 *
 * @category attributes
 * @since 0.0.0
 */
export const entries = Effect.fn('Attribute.entries')(function* (
  builder: Builder.Builder,
  self: Set,
): Effect.fn.Return<ReadonlyArray<Attribute>, SilkError> {
  return yield* BuilderState.mutate(builder, 'Attribute.entries', (state, owner) => {
    const index = Handle.resolve(builder, owner, self, 'AttributeSet', 'Attribute.entries')
    const values = state.attributeSets[index]
    if (values === undefined) {
      throw new SilkError({
        operation: 'Attribute.entries',
        message: 'Attribute set is missing',
        cause: self,
      })
    }
    return Object.freeze(
      values.map((value) => handleAt(state.attributeHandles, value, 'Attribute.entries')),
    )
  })
})

/**
 * Interns function, return, and positional parameter groups as one function attribute set.
 *
 * @category attributes
 * @since 0.0.0
 */
export const functionSet = Effect.fn('Attribute.functionSet')(function* (
  builder: Builder.Builder,
  input: FunctionSetInput = {},
): Effect.fn.Return<FunctionSet, SilkError> {
  const emptySet = yield* set(builder, [])
  const functionAttributes = input.functionAttributes ?? emptySet
  const returnAttributes = input.returnAttributes ?? emptySet
  const parameterAttributes = input.parameterAttributes ?? []
  return yield* BuilderState.mutate(builder, 'Attribute.functionSet', (state, owner) => {
    const functionIndex = Handle.resolve(
      builder,
      owner,
      functionAttributes,
      'AttributeSet',
      'Attribute.functionSet',
    )
    const returnIndex = Handle.resolve(
      builder,
      owner,
      returnAttributes,
      'AttributeSet',
      'Attribute.functionSet',
    )
    const parameterIndices = Object.freeze(
      parameterAttributes.map((attributes) =>
        Handle.resolve(builder, owner, attributes, 'AttributeSet', 'Attribute.functionSet'),
      ),
    )
    const description = Object.freeze({
      functionAttributes: functionIndex,
      returnAttributes: returnIndex,
      parameterAttributes: parameterIndices,
    })
    const key = CanonicalKey.tagged('function-attributes', [
      CanonicalKey.integer(functionIndex),
      CanonicalKey.integer(returnIndex),
      CanonicalKey.sequence(parameterIndices.map(CanonicalKey.integer)),
    ])
    const found = state.functionAttributeSetKeys.get(key)
    if (found !== undefined) {
      const handle = state.functionAttributeSetHandles[found]
      if (handle !== undefined) return handle
    }
    const index = state.functionAttributeSets.length
    const handle = Handle.make('FunctionAttributeSet', owner, index)
    state.functionAttributeSets.push(description)
    state.functionAttributeSetHandles.push(handle)
    state.functionAttributeSetKeys.set(key, index)
    return handle
  })
})

/**
 * Expands a function attribute set into its function, return, and parameter groups.
 *
 * @category attributes
 * @since 0.0.0
 */
export const functionSetEntries = Effect.fn('Attribute.functionSetEntries')(function* (
  builder: Builder.Builder,
  self: FunctionSet,
): Effect.fn.Return<FunctionSetEntries, SilkError> {
  return yield* BuilderState.mutate(builder, 'Attribute.functionSetEntries', (state, owner) => {
    const index = Handle.resolve(
      builder,
      owner,
      self,
      'FunctionAttributeSet',
      'Attribute.functionSetEntries',
    )
    const description = state.functionAttributeSets[index]
    const functionAttributes =
      description === undefined
        ? undefined
        : state.attributeSetHandles[description.functionAttributes]
    const returnAttributes =
      description === undefined
        ? undefined
        : state.attributeSetHandles[description.returnAttributes]
    const parameterAttributes =
      description === undefined
        ? []
        : description.parameterAttributes.map((setIndex) => state.attributeSetHandles[setIndex])
    if (
      description === undefined ||
      functionAttributes === undefined ||
      returnAttributes === undefined ||
      parameterAttributes.some((set) => set === undefined)
    ) {
      throw new SilkError({
        operation: 'Attribute.functionSetEntries',
        message: 'Function attribute set references a missing table entry',
        cause: self,
      })
    }
    return Object.freeze({
      functionAttributes,
      returnAttributes,
      parameterAttributes: Object.freeze(
        parameterAttributes.flatMap((set) => (set === undefined ? [] : [set])),
      ),
    })
  })
})
