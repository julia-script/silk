import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Builder from './Builder.js'
import * as ByteString from './ByteString.js'
import type * as AttributeDescription from './internal/AttributeDescription.js'
import * as BuilderState from './internal/BuilderState.js'
import * as CanonicalKey from './internal/CanonicalKey.js'
import * as Handle from './internal/Handle.js'
import * as IntegerInput from './internal/IntegerInput.js'
import * as Table from './internal/Table.js'
import { invalidInput, invalidState, type LlvmError } from './LlvmError.js'
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
const intern = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  description: AttributeDescription.Description,
) {
  return yield* BuilderState.mutate(builder, 'Attribute.intern', (state, owner) =>
    Result.gen(function* () {
      const key = descriptionKey(description)
      const interned = yield* Table.intern(
        state.attributes,
        'Attribute.intern',
        'Attribute',
        key,
        description,
        (index) => Handle.make('Attribute', owner, index),
      )
      return interned.handle
    }),
  )
})

/** @internal */
const validateName = Effect.fnUntraced(function* (
  name: ByteString.ByteString | Uint8Array | string,
  operation: string,
): Effect.fn.Return<ByteString.ByteString, LlvmError> {
  const value = ByteString.coerce(name)
  if (ByteString.isEmpty(value)) {
    return yield* invalidInput({
      operation,
      message: 'An LLVM attribute requires a name',
      input: name,
    })
  }
  return value
})

/**
 * Creates or reuses a valueless attribute such as `nounwind`.
 *
 * @category attributes
 * @since 0.0.0
 */
export const flag = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  name: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Attribute, LlvmError> {
  const value = yield* validateName(name, 'Attribute.flag')
  return yield* intern(builder, Object.freeze({ _tag: 'Flag', name: value }))
})

/**
 * Creates or reuses an attribute with an unsigned 64-bit integer payload.
 *
 * @category attributes
 * @since 0.0.0
 */
export const integer = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  name: ByteString.ByteString | Uint8Array | string,
  value: number | bigint,
): Effect.fn.Return<Attribute, LlvmError> {
  const attributeName = yield* validateName(name, 'Attribute.integer')
  const exact = yield* Effect.fromResult(
    IntegerInput.normalize(value, {
      operation: 'Attribute.integer',
      message: 'Integer attribute payload must be an unsigned 64-bit integer',
      minimum: 0n,
      maximum: 0xffff_ffff_ffff_ffffn,
    }),
  )
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
export const typeAttribute = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  name: ByteString.ByteString | Uint8Array | string,
  type: Type.Type,
): Effect.fn.Return<Attribute, LlvmError> {
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
export const string = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  name: ByteString.ByteString | Uint8Array | string,
  value: ByteString.ByteString | Uint8Array | string = ByteString.empty,
): Effect.fn.Return<Attribute, LlvmError> {
  const attributeName = yield* validateName(name, 'Attribute.string')
  return yield* intern(
    builder,
    Object.freeze({ _tag: 'String', name: attributeName, value: ByteString.coerce(value) }),
  )
})

/**
 * Creates or reuses a named list of unsigned 64-bit integer payloads.
 *
 * @category attributes
 * @since 0.0.0
 */
export const integerList = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  name: ByteString.ByteString | Uint8Array | string,
  values: ReadonlyArray<number | bigint>,
): Effect.fn.Return<Attribute, LlvmError> {
  const attributeName = yield* validateName(name, 'Attribute.integerList')
  const exact = yield* Effect.fromResult(
    IntegerInput.normalizeAll(values, {
      operation: 'Attribute.integerList',
      message: 'Attribute list values must be unsigned 64-bit integers',
      minimum: 0n,
      maximum: 0xffff_ffff_ffff_ffffn,
    }),
  )
  return yield* intern(
    builder,
    Object.freeze({ _tag: 'IntegerList', name: attributeName, values: exact }),
  )
})

/** @internal */
const internSet = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  attributeIndices: ReadonlyArray<number>,
): Effect.fn.Return<Set, LlvmError> {
  return yield* BuilderState.mutate(builder, 'Attribute.set', (state, owner) =>
    Result.gen(function* () {
      const ordered = [...new Set(attributeIndices)].sort((left, right) => {
        const leftDescription = state.attributes.descriptions[left]
        const rightDescription = state.attributes.descriptions[right]
        if (leftDescription === undefined || rightDescription === undefined) return left - right
        return descriptionKey(leftDescription).localeCompare(descriptionKey(rightDescription))
      })
      const names = new Map<string, number>()
      for (const index of ordered) {
        const description = state.attributes.descriptions[index]
        if (description === undefined) {
          return yield* Result.fail(
            invalidState({
              operation: 'Attribute.set',
              message: 'Attribute table entry is missing',
              state: index,
            }),
          )
        }
        const name = CanonicalKey.bytes(description.name)
        const previous = names.get(name)
        if (previous !== undefined && previous !== index) {
          return yield* Result.fail(
            invalidInput({
              operation: 'Attribute.set',
              message: 'An attribute set cannot contain conflicting values for one name',
              input: { previous, index },
            }),
          )
        }
        names.set(name, index)
      }
      const values = Object.freeze(ordered)
      const key = CanonicalKey.sequence(values.map(CanonicalKey.integer))
      const interned = yield* Table.intern(
        state.attributeSets,
        'Attribute.set',
        'AttributeSet',
        key,
        values,
        (index) => Handle.make('AttributeSet', owner, index),
      )
      return interned.handle
    }),
  )
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
 * import * as Attribute from '@silklang/llvm/Attribute'
 * import * as Builder from '@silklang/llvm/Builder'
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
export const set = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  attributes: ReadonlyArray<Attribute>,
): Effect.fn.Return<Set, LlvmError> {
  const values = yield* BuilderState.mutate(builder, 'Attribute.set', (_state, owner) =>
    Result.gen(function* () {
      const indices: Array<number> = []
      for (const attribute of attributes) {
        indices.push(yield* Handle.resolve(builder, owner, attribute, 'Attribute', 'Attribute.set'))
      }
      return Object.freeze(indices)
    }),
  )
  return yield* internSet(builder, values)
})

/**
 * Returns the canonical set produced by adding one attribute.
 *
 * @category attributes
 * @since 0.0.0
 */
export const add = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  self: Set,
  attribute: Attribute,
): Effect.fn.Return<Set, LlvmError> {
  const values = yield* BuilderState.mutate(builder, 'Attribute.add', (state, owner) =>
    Result.gen(function* () {
      const setIndex = yield* Handle.resolve(builder, owner, self, 'AttributeSet', 'Attribute.add')
      const attributeIndex = yield* Handle.resolve(
        builder,
        owner,
        attribute,
        'Attribute',
        'Attribute.add',
      )
      const existing = state.attributeSets.descriptions[setIndex]
      if (existing === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'Attribute.add',
            message: 'Attribute set is missing',
            state: self,
          }),
        )
      }
      return Object.freeze([...existing, attributeIndex])
    }),
  )
  return yield* internSet(builder, values)
})

/**
 * Returns the canonical set produced by removing every occurrence of one attribute.
 *
 * @category attributes
 * @since 0.0.0
 */
export const remove = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  self: Set,
  attribute: Attribute,
): Effect.fn.Return<Set, LlvmError> {
  const values = yield* BuilderState.mutate(builder, 'Attribute.remove', (state, owner) =>
    Result.gen(function* () {
      const setIndex = yield* Handle.resolve(
        builder,
        owner,
        self,
        'AttributeSet',
        'Attribute.remove',
      )
      const attributeIndex = yield* Handle.resolve(
        builder,
        owner,
        attribute,
        'Attribute',
        'Attribute.remove',
      )
      const existing = state.attributeSets.descriptions[setIndex]
      if (existing === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'Attribute.remove',
            message: 'Attribute set is missing',
            state: self,
          }),
        )
      }
      return Object.freeze(existing.filter((index) => index !== attributeIndex))
    }),
  )
  return yield* internSet(builder, values)
})

/**
 * Expands a canonical set into its deterministic attribute order.
 *
 * @category attributes
 * @since 0.0.0
 */
export const entries = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  self: Set,
): Effect.fn.Return<ReadonlyArray<Attribute>, LlvmError> {
  return yield* BuilderState.mutate(builder, 'Attribute.entries', (state, owner) =>
    Result.gen(function* () {
      const index = yield* Handle.resolve(builder, owner, self, 'AttributeSet', 'Attribute.entries')
      const values = state.attributeSets.descriptions[index]
      if (values === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'Attribute.entries',
            message: 'Attribute set is missing',
            state: self,
          }),
        )
      }
      const entries: Array<Attribute> = []
      for (const value of values) {
        entries.push(
          yield* Table.handleAt(state.attributes, value, 'Attribute.entries', 'Attribute'),
        )
      }
      return Object.freeze(entries)
    }),
  )
})

/**
 * Interns function, return, and positional parameter groups as one function attribute set.
 *
 * @category attributes
 * @since 0.0.0
 */
export const functionSet = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  input: FunctionSetInput = {},
): Effect.fn.Return<FunctionSet, LlvmError> {
  const emptySet = yield* set(builder, [])
  const functionAttributes = input.functionAttributes ?? emptySet
  const returnAttributes = input.returnAttributes ?? emptySet
  const parameterAttributes = input.parameterAttributes ?? []
  return yield* BuilderState.mutate(builder, 'Attribute.functionSet', (state, owner) =>
    Result.gen(function* () {
      const functionIndex = yield* Handle.resolve(
        builder,
        owner,
        functionAttributes,
        'AttributeSet',
        'Attribute.functionSet',
      )
      const returnIndex = yield* Handle.resolve(
        builder,
        owner,
        returnAttributes,
        'AttributeSet',
        'Attribute.functionSet',
      )
      const mutableParameterIndices: Array<number> = []
      for (const attributes of parameterAttributes) {
        mutableParameterIndices.push(
          yield* Handle.resolve(
            builder,
            owner,
            attributes,
            'AttributeSet',
            'Attribute.functionSet',
          ),
        )
      }
      const parameterIndices = Object.freeze(mutableParameterIndices)
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
      const interned = yield* Table.intern(
        state.functionAttributeSets,
        'Attribute.functionSet',
        'FunctionAttributeSet',
        key,
        description,
        (index) => Handle.make('FunctionAttributeSet', owner, index),
      )
      return interned.handle
    }),
  )
})

/**
 * Expands a function attribute set into its function, return, and parameter groups.
 *
 * @category attributes
 * @since 0.0.0
 */
export const functionSetEntries = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  self: FunctionSet,
): Effect.fn.Return<FunctionSetEntries, LlvmError> {
  return yield* BuilderState.mutate(builder, 'Attribute.functionSetEntries', (state, owner) =>
    Result.gen(function* () {
      const index = yield* Handle.resolve(
        builder,
        owner,
        self,
        'FunctionAttributeSet',
        'Attribute.functionSetEntries',
      )
      const description = state.functionAttributeSets.descriptions[index]
      const functionAttributes =
        description === undefined
          ? undefined
          : state.attributeSets.handles[description.functionAttributes]
      const returnAttributes =
        description === undefined
          ? undefined
          : state.attributeSets.handles[description.returnAttributes]
      const parameterAttributes =
        description === undefined
          ? []
          : description.parameterAttributes.map((setIndex) => state.attributeSets.handles[setIndex])
      if (
        description === undefined ||
        functionAttributes === undefined ||
        returnAttributes === undefined ||
        parameterAttributes.some((set) => set === undefined)
      ) {
        return yield* Result.fail(
          invalidState({
            operation: 'Attribute.functionSetEntries',
            message: 'Function attribute set references a missing table entry',
            state: self,
          }),
        )
      }
      return Object.freeze({
        functionAttributes,
        returnAttributes,
        parameterAttributes: Object.freeze(
          parameterAttributes.flatMap((set) => (set === undefined ? [] : [set])),
        ),
      })
    }),
  )
})
