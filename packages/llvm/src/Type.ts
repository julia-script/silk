import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import * as AddrSpace from './AddrSpace.js'
import type * as Alignment from './Alignment.js'
import type * as Builder from './Builder.js'
import * as ByteString from './ByteString.js'
import * as DataLayout from './DataLayout.js'
import * as BuilderState from './internal/BuilderState.js'
import * as CanonicalKey from './internal/CanonicalKey.js'
import * as Handle from './internal/Handle.js'
import * as IntegerInput from './internal/IntegerInput.js'
import * as ResolveActor from './internal/resolveActor.js'
import * as Table from './internal/Table.js'
import type * as TypeDescription from './internal/TypeDescription.js'
import { invalidInput, type LlvmError, wrappedFailure } from './LlvmError.js'

/**
 * Opaque builder-owned identity for a structurally interned LLVM type.
 *
 * @category types
 * @since 0.0.0
 */
export interface Type extends Handle.Handle<'Type'> {}

/**
 * Runtime classification returned by {@link tag}.
 *
 * @category types
 * @since 0.0.0
 */
export type Tag = TypeDescription.Description['_tag'] | TypeDescription.SimpleTag

/**
 * Materialized return, parameter, and variadic fields of a function type.
 *
 * @category types
 * @since 0.0.0
 */
export interface FunctionSignature {
  readonly returnType: Type
  readonly parameters: ReadonlyArray<Type>
  readonly variadic: boolean
}

/**
 * Uniform structural view of arrays, vectors, literal structures, and complete named structures.
 *
 * @category types
 * @since 0.0.0
 */
export interface AggregateShape {
  readonly fields: ReadonlyArray<Type>
  readonly packed: boolean
  readonly scalable: boolean
  readonly length: bigint | undefined
}

/** @internal */
const keyForDescription = (description: TypeDescription.Description): string => {
  switch (description._tag) {
    case 'Simple':
      return CanonicalKey.tagged('simple', [description.tag])
    case 'Integer':
      return CanonicalKey.tagged('integer', [CanonicalKey.integer(description.bitWidth)])
    case 'Pointer':
      return CanonicalKey.tagged('pointer', [CanonicalKey.integer(description.addressSpace.value)])
    case 'Function':
      return CanonicalKey.tagged('function', [
        CanonicalKey.integer(description.returnType),
        CanonicalKey.sequence(description.parameters.map(CanonicalKey.integer)),
        description.variadic ? '1' : '0',
      ])
    case 'Vector':
      return CanonicalKey.tagged('vector', [
        CanonicalKey.integer(description.child),
        CanonicalKey.integer(description.length),
        description.scalable ? '1' : '0',
      ])
    case 'Array':
      return CanonicalKey.tagged('array', [
        CanonicalKey.integer(description.child),
        CanonicalKey.integer(description.length),
      ])
    case 'Structure':
      return CanonicalKey.tagged('structure', [
        description.packed ? '1' : '0',
        CanonicalKey.sequence(description.fields.map(CanonicalKey.integer)),
      ])
    case 'NamedStructure':
      return CanonicalKey.tagged('named-structure', [CanonicalKey.bytes(description.name)])
    case 'TargetExtension':
      return CanonicalKey.tagged('target-extension', [
        CanonicalKey.bytes(description.name),
        CanonicalKey.sequence(description.types.map(CanonicalKey.integer)),
        CanonicalKey.sequence(description.integers.map(CanonicalKey.integer)),
      ])
  }
}

/** @internal */
const intern = Effect.fn('Type.intern')(function* (
  builder: Builder.Builder,
  description: TypeDescription.Description,
) {
  return yield* BuilderState.mutate(builder, 'Type.intern', (state, owner) =>
    Result.gen(function* () {
      const key = keyForDescription(description)
      const interned = yield* Table.intern(
        state.types,
        'Type.intern',
        'Type',
        key,
        description,
        (index) => Handle.make('Type', owner, index),
      )
      return interned.handle
    }),
  )
})

/** @internal */
const simple = (builder: Builder.Builder, tag: TypeDescription.SimpleTag) =>
  intern(builder, Object.freeze({ _tag: 'Simple', tag }))

/**
 * Returns the builder's canonical LLVM `void` type.
 *
 * @category types
 * @since 0.0.0
 */
export const voidType = Effect.fn('Type.voidType')(function* (builder: Builder.Builder) {
  return yield* simple(builder, 'Void')
})

/**
 * Returns the canonical IEEE 16-bit `half` type.
 *
 * @category types
 * @since 0.0.0
 */
export const half = Effect.fn('Type.half')(function* (builder: Builder.Builder) {
  return yield* simple(builder, 'Half')
})

/**
 * Returns the canonical 16-bit `bfloat` type.
 *
 * @category types
 * @since 0.0.0
 */
export const bfloat = Effect.fn('Type.bfloat')(function* (builder: Builder.Builder) {
  return yield* simple(builder, 'BFloat')
})

/**
 * Returns the canonical IEEE 32-bit `float` type.
 *
 * @category types
 * @since 0.0.0
 */
export const float = Effect.fn('Type.float')(function* (builder: Builder.Builder) {
  return yield* simple(builder, 'Float')
})

/**
 * Returns the canonical IEEE 64-bit `double` type.
 *
 * @category types
 * @since 0.0.0
 */
export const double = Effect.fn('Type.double')(function* (builder: Builder.Builder) {
  return yield* simple(builder, 'Double')
})

/**
 * Returns the canonical x87 80-bit floating-point type.
 *
 * @category types
 * @since 0.0.0
 */
export const x86Fp80 = Effect.fn('Type.x86Fp80')(function* (builder: Builder.Builder) {
  return yield* simple(builder, 'X86Fp80')
})

/**
 * Returns the canonical IEEE quad-precision `fp128` type.
 *
 * @category types
 * @since 0.0.0
 */
export const fp128 = Effect.fn('Type.fp128')(function* (builder: Builder.Builder) {
  return yield* simple(builder, 'Fp128')
})

/**
 * Returns the canonical PowerPC double-double `ppc_fp128` type.
 *
 * @category types
 * @since 0.0.0
 */
export const ppcFp128 = Effect.fn('Type.ppcFp128')(function* (builder: Builder.Builder) {
  return yield* simple(builder, 'PpcFp128')
})

/**
 * Returns the canonical basic-block `label` type.
 *
 * @category types
 * @since 0.0.0
 */
export const label = Effect.fn('Type.label')(function* (builder: Builder.Builder) {
  return yield* simple(builder, 'Label')
})

/**
 * Returns the canonical first-class `metadata` type.
 *
 * @category types
 * @since 0.0.0
 */
export const metadata = Effect.fn('Type.metadata')(function* (builder: Builder.Builder) {
  return yield* simple(builder, 'Metadata')
})

/**
 * Returns the canonical x86 AMX tile type.
 *
 * @category types
 * @since 0.0.0
 */
export const x86Amx = Effect.fn('Type.x86Amx')(function* (builder: Builder.Builder) {
  return yield* simple(builder, 'X86Amx')
})

/**
 * Returns the canonical LLVM `token` type.
 *
 * @category types
 * @since 0.0.0
 */
export const token = Effect.fn('Type.token')(function* (builder: Builder.Builder) {
  return yield* simple(builder, 'Token')
})

/**
 * Returns the canonical arbitrary-width integer type for this builder.
 *
 * **Details**
 *
 * The width may range from `1` through LLVM's 24-bit maximum, `16_777_215`.
 *
 * **Gotchas**
 *
 * Values outside that range, including non-integers, fail with {@link LlvmError}.
 *
 * **Example** (Creating a wide integer type)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silk-lang/llvm/Builder'
 * import * as Type from '@silk-lang/llvm/Type'
 *
 * const width = await Effect.runPromise(Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const i129 = yield* Type.integer(builder, 129)
 *   return yield* Type.integerBitWidth(builder, i129)
 * }))
 * // width === 129
 * ```
 *
 * @category types
 * @since 0.0.0
 */
export const integer = Effect.fn('Type.integer')(function* (
  builder: Builder.Builder,
  bitWidth: number,
): Effect.fn.Return<Type, LlvmError> {
  if (!Number.isSafeInteger(bitWidth) || bitWidth < 1 || bitWidth > 0xff_ffff) {
    return yield* Effect.fail(
      invalidInput({
        operation: 'Type.integer',
        message: 'LLVM integer width must be from 1 through 16777215 bits',
        input: bitWidth,
      }),
    )
  }
  return yield* intern(builder, Object.freeze({ _tag: 'Integer', bitWidth }))
})

/**
 * Returns the canonical opaque pointer type for an address space.
 *
 * @category types
 * @since 0.0.0
 */
export const pointer = Effect.fn('Type.pointer')(function* (
  builder: Builder.Builder,
  addressSpace: AddrSpace.AddrSpace = AddrSpace.defaultAddrSpace,
) {
  return yield* intern(builder, Object.freeze({ _tag: 'Pointer', addressSpace }))
})

/** @internal */
const indices = (
  builder: Builder.Builder,
  owner: BuilderState.Snapshot['owner'],
  types: ReadonlyArray<Type>,
  operation: string,
): Result.Result<ReadonlyArray<number>, LlvmError> =>
  Result.gen(function* () {
    const values: Array<number> = []
    for (const type of types) {
      values.push(yield* Handle.resolve(builder, owner, type, 'Type', operation))
    }
    return Object.freeze(values)
  })

/**
 * Interns an LLVM function signature from a return type, positional parameters, and variadic flag.
 *
 * @category types
 * @since 0.0.0
 */
export const functionType = Effect.fn('Type.functionType')(function* (
  builder: Builder.Builder,
  returnType: Type,
  parameters: ReadonlyArray<Type>,
  options: { readonly variadic?: boolean } = {},
): Effect.fn.Return<Type, LlvmError> {
  const description = yield* BuilderState.mutate(builder, 'Type.functionType', (_state, owner) =>
    Result.gen(function* () {
      return Object.freeze({
        _tag: 'Function' as const,
        returnType: yield* Handle.resolve(builder, owner, returnType, 'Type', 'Type.functionType'),
        parameters: yield* indices(builder, owner, parameters, 'Type.functionType'),
        variadic: options.variadic ?? false,
      })
    }),
  )
  return yield* intern(builder, description)
})

/** @internal */
const vectorOf = Effect.fn('Type.vectorOf')(function* (
  builder: Builder.Builder,
  child: Type,
  length: number,
  scalable: boolean,
): Effect.fn.Return<Type, LlvmError> {
  if (!Number.isSafeInteger(length) || length < 1 || length > 0xffff_ffff) {
    return yield* Effect.fail(
      invalidInput({
        operation: 'Type.vector',
        message: 'LLVM vector length must be a positive 32-bit integer',
        input: length,
      }),
    )
  }
  const childIndex = yield* BuilderState.mutate(builder, 'Type.vector', (_state, owner) =>
    Handle.resolve(builder, owner, child, 'Type', 'Type.vector'),
  )
  return yield* intern(
    builder,
    Object.freeze({ _tag: 'Vector', child: childIndex, length, scalable }),
  )
})

/**
 * Interns a fixed vector with a positive unsigned 32-bit element count.
 *
 * @category types
 * @since 0.0.0
 */
export const vector = Effect.fn('Type.vector')(function* (
  builder: Builder.Builder,
  child: Type,
  length: number,
) {
  return yield* vectorOf(builder, child, length, false)
})

/**
 * Interns a scalable vector whose runtime length is a positive multiple of `minimumLength`.
 *
 * @category types
 * @since 0.0.0
 */
export const scalableVector = Effect.fn('Type.scalableVector')(function* (
  builder: Builder.Builder,
  child: Type,
  minimumLength: number,
) {
  return yield* vectorOf(builder, child, minimumLength, true)
})

/**
 * Interns an array with an unsigned 64-bit length; unlike vectors, zero-length arrays are valid.
 *
 * @category types
 * @since 0.0.0
 */
export const array = Effect.fn('Type.array')(function* (
  builder: Builder.Builder,
  child: Type,
  length: number | bigint,
): Effect.fn.Return<Type, LlvmError> {
  const exactLength = yield* Effect.fromResult(
    IntegerInput.normalize(length, {
      operation: 'Type.array',
      message: 'LLVM array length must be an unsigned 64-bit integer',
      minimum: 0n,
      maximum: 0xffff_ffff_ffff_ffffn,
    }),
  )
  const childIndex = yield* BuilderState.mutate(builder, 'Type.array', (_state, owner) =>
    Handle.resolve(builder, owner, child, 'Type', 'Type.array'),
  )
  return yield* intern(
    builder,
    Object.freeze({ _tag: 'Array', child: childIndex, length: exactLength }),
  )
})

/**
 * Interns an anonymous literal structure, optionally using packed field layout.
 *
 * @category types
 * @since 0.0.0
 */
export const structure = Effect.fn('Type.structure')(function* (
  builder: Builder.Builder,
  fields: ReadonlyArray<Type>,
  options: { readonly packed?: boolean } = {},
) {
  const description = yield* BuilderState.mutate(builder, 'Type.structure', (_state, owner) =>
    Result.gen(function* () {
      return Object.freeze({
        _tag: 'Structure' as const,
        fields: yield* indices(builder, owner, fields, 'Type.structure'),
        packed: options.packed ?? false,
      })
    }),
  )
  return yield* intern(builder, description)
})

/**
 * Declares or retrieves a named structure with a stable identity and initially opaque body.
 *
 * **When to use**
 *
 * Use when a recursive type needs a stable identity before its fields can be declared. Complete the
 * type exactly once with {@link setNamedBody}.
 *
 * @category types
 * @since 0.0.0
 */
export const namedStructure = Effect.fn('Type.namedStructure')(function* (
  builder: Builder.Builder,
  name: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Type, LlvmError> {
  const value = ByteString.coerce(name)
  if (ByteString.isEmpty(value)) {
    return yield* Effect.fail(
      invalidInput({
        operation: 'Type.namedStructure',
        message: 'A named LLVM structure requires a non-empty name',
        input: name,
      }),
    )
  }
  return yield* BuilderState.mutate(builder, 'Type.namedStructure', (state, owner) =>
    Result.gen(function* () {
      const key = CanonicalKey.bytes(value)
      const found = state.namedTypes.get(key)
      if (found !== undefined) {
        return yield* Table.handleAt(state.types, found, 'Type.namedStructure', 'Type')
      }
      const index = state.types.descriptions.length
      const handle = Handle.make('Type', owner, index)
      const description: TypeDescription.Description = Object.freeze({
        _tag: 'NamedStructure',
        name: value,
        body: undefined,
      })
      state.types.descriptions.push(description)
      state.types.handles.push(handle)
      state.types.keys.set(keyForDescription(description), index)
      state.namedTypes.set(key, index)
      return handle
    }),
  )
})

/**
 * Completes a named structure exactly once, allowing its fields to refer back to its identity.
 *
 * @category types
 * @since 0.0.0
 */
export const setNamedBody = Effect.fn('Type.setNamedBody')(function* (
  builder: Builder.Builder,
  self: Type,
  fields: ReadonlyArray<Type>,
  options: { readonly packed?: boolean } = {},
): Effect.fn.Return<void, LlvmError> {
  yield* BuilderState.mutate(builder, 'Type.setNamedBody', (state, owner) =>
    Result.gen(function* () {
      const index = yield* Handle.resolve(builder, owner, self, 'Type', 'Type.setNamedBody')
      const description = state.types.descriptions[index]
      if (description?._tag !== 'NamedStructure') {
        return yield* Result.fail(
          invalidInput({
            operation: 'Type.setNamedBody',
            message: 'Only a named structure can receive a body',
            input: self,
          }),
        )
      }
      if (description.body !== undefined) {
        return yield* Result.fail(
          invalidInput({
            operation: 'Type.setNamedBody',
            message: 'A named structure body can only be assigned once',
            input: self,
          }),
        )
      }
      state.types.descriptions[index] = Object.freeze({
        ...description,
        body: Object.freeze({
          fields: yield* indices(builder, owner, fields, 'Type.setNamedBody'),
          packed: options.packed ?? false,
        }),
      })
    }),
  )
})

/**
 * Interns a non-empty named target-extension type with type and integer parameters.
 *
 * @category types
 * @since 0.0.0
 */
export const targetExtension = Effect.fn('Type.targetExtension')(function* (
  builder: Builder.Builder,
  name: ByteString.ByteString | Uint8Array | string,
  types: ReadonlyArray<Type> = [],
  integerParameters: ReadonlyArray<number | bigint> = [],
): Effect.fn.Return<Type, LlvmError> {
  const value = ByteString.coerce(name)
  if (ByteString.isEmpty(value)) {
    return yield* Effect.fail(
      invalidInput({
        operation: 'Type.targetExtension',
        message: 'A target extension type requires a non-empty name',
        input: name,
      }),
    )
  }
  const integers = yield* Effect.fromResult(
    IntegerInput.normalizeAll(integerParameters, {
      operation: 'Type.targetExtension',
      message: 'Target extension integer parameters must be finite safe integers or bigints',
    }),
  )
  const description = yield* BuilderState.mutate(builder, 'Type.targetExtension', (_state, owner) =>
    Result.gen(function* () {
      return Object.freeze({
        _tag: 'TargetExtension' as const,
        name: value,
        types: yield* indices(builder, owner, types, 'Type.targetExtension'),
        integers,
      })
    }),
  )
  return yield* intern(builder, description)
})

/** @internal */
const inspect = <A>(
  builder: Builder.Builder,
  self: Type,
  operation: string,
  f: (
    description: TypeDescription.Description,
    state: BuilderState.MutableState,
    index: number,
  ) => Result.Result<A, LlvmError>,
) =>
  BuilderState.mutate(builder, operation, (state, owner) =>
    Result.gen(function* () {
      const { index, description } = yield* ResolveActor.resolve(
        builder,
        owner,
        self,
        'Type',
        state.types,
        operation,
      )
      return yield* f(description, state, index)
    }),
  )

/**
 * Returns a stable runtime tag for a builder-owned type.
 *
 * @category types
 * @since 0.0.0
 */
export const tag = Effect.fn('Type.tag')(function* (builder: Builder.Builder, self: Type) {
  return yield* inspect(builder, self, 'Type.tag', (description) =>
    Result.succeed(description._tag === 'Simple' ? description.tag : description._tag),
  )
})

/**
 * Reads an integer type's bit width, failing with {@link LlvmError} for other type families.
 *
 * @category types
 * @since 0.0.0
 */
export const integerBitWidth = Effect.fn('Type.integerBitWidth')(function* (
  builder: Builder.Builder,
  self: Type,
): Effect.fn.Return<number, LlvmError> {
  return yield* inspect(builder, self, 'Type.integerBitWidth', (description) =>
    Result.gen(function* () {
      if (description._tag !== 'Integer') {
        return yield* Result.fail(
          invalidInput({
            operation: 'Type.integerBitWidth',
            message: 'Expected an integer type',
            input: self,
          }),
        )
      }
      return description.bitWidth
    }),
  )
})

/**
 * Reads the repeated child type of an array or vector.
 *
 * @category types
 * @since 0.0.0
 */
export const childType = Effect.fn('Type.childType')(function* (
  builder: Builder.Builder,
  self: Type,
): Effect.fn.Return<Type, LlvmError> {
  return yield* inspect(builder, self, 'Type.childType', (description, state) =>
    Result.gen(function* () {
      if (description._tag !== 'Array' && description._tag !== 'Vector') {
        return yield* Result.fail(
          invalidInput({
            operation: 'Type.childType',
            message: 'Expected an array or vector type',
            input: self,
          }),
        )
      }
      return yield* Table.handleAt(state.types, description.child, 'Type.childType', 'Type')
    }),
  )
})

/**
 * Expands a function type into builder-owned return and parameter handles.
 *
 * @category types
 * @since 0.0.0
 */
export const functionSignature = Effect.fn('Type.functionSignature')(function* (
  builder: Builder.Builder,
  self: Type,
): Effect.fn.Return<FunctionSignature, LlvmError> {
  return yield* inspect(builder, self, 'Type.functionSignature', (description, state) =>
    Result.gen(function* () {
      if (description._tag !== 'Function') {
        return yield* Result.fail(
          invalidInput({
            operation: 'Type.functionSignature',
            message: 'Expected a function type',
            input: self,
          }),
        )
      }
      const parameters: Array<Type> = []
      for (const index of description.parameters) {
        parameters.push(yield* Table.handleAt(state.types, index, 'Type.functionSignature', 'Type'))
      }
      return Object.freeze({
        returnType: yield* Table.handleAt(
          state.types,
          description.returnType,
          'Type.functionSignature',
          'Type',
        ),
        parameters: Object.freeze(parameters),
        variadic: description.variadic,
      })
    }),
  )
})

/**
 * Expands a complete aggregate into a uniform shape.
 *
 * **Details**
 *
 * Arrays and vectors expose one repeated field plus `length`; structures expose each field
 * and leave `length` undefined.
 *
 * **Gotchas**
 *
 * Opaque named structures fail until completed.
 *
 * @category types
 * @since 0.0.0
 */
export const aggregateShape = Effect.fn('Type.aggregateShape')(function* (
  builder: Builder.Builder,
  self: Type,
): Effect.fn.Return<AggregateShape, LlvmError> {
  return yield* inspect(builder, self, 'Type.aggregateShape', (description, state) =>
    Result.gen(function* () {
      if (description._tag === 'Array' || description._tag === 'Vector') {
        return Object.freeze({
          fields: Object.freeze([
            yield* Table.handleAt(state.types, description.child, 'Type.aggregateShape', 'Type'),
          ]),
          packed: false,
          scalable: description._tag === 'Vector' && description.scalable,
          length: description._tag === 'Array' ? description.length : BigInt(description.length),
        })
      }
      let body: { readonly fields: ReadonlyArray<number>; readonly packed: boolean } | undefined
      if (description._tag === 'Structure') body = description
      else if (description._tag === 'NamedStructure') body = description.body
      if (body === undefined) {
        return yield* Result.fail(
          invalidInput({
            operation: 'Type.aggregateShape',
            message: 'Expected a complete aggregate type',
            input: self,
          }),
        )
      }
      const fields: Array<Type> = []
      for (const index of body.fields) {
        fields.push(yield* Table.handleAt(state.types, index, 'Type.aggregateShape', 'Type'))
      }
      return Object.freeze({
        fields: Object.freeze(fields),
        packed: body.packed,
        scalable: false,
        length: undefined,
      })
    }),
  )
})

/** @internal */
const simpleBitWidth = (tag: TypeDescription.SimpleTag): number | undefined => {
  switch (tag) {
    case 'Half':
    case 'BFloat':
      return 16
    case 'Float':
      return 32
    case 'Double':
      return 64
    case 'X86Fp80':
      return 80
    case 'Fp128':
    case 'PpcFp128':
      return 128
    case 'X86Amx':
      return 8192
    case 'Void':
    case 'Label':
    case 'Metadata':
    case 'Token':
      return undefined
  }
}

/** @internal */
const layoutOf = (
  index: number,
  state: BuilderState.MutableState,
  visiting: Set<number>,
): { readonly size: bigint; readonly alignment: Alignment.Alignment } => {
  if (visiting.has(index)) throw new Error('recursive type has no finite inline size')
  visiting.add(index)
  const description = state.types.descriptions[index]
  if (description === undefined) throw new Error('missing type table entry')
  try {
    if (description._tag === 'Integer') {
      const spec = DataLayout.integerSpec(state.layout, description.bitWidth)
      const size = BigInt(Math.ceil(description.bitWidth / 8))
      return {
        size,
        alignment: spec?.abiAlignment ?? { _tag: 'Alignment', byteUnits: size === 0n ? 1n : size },
      }
    }
    if (description._tag === 'Simple') {
      const bits = simpleBitWidth(description.tag)
      if (bits === undefined) throw new Error(`${description.tag} has no allocatable size`)
      const size = BigInt(Math.ceil(bits / 8))
      const spec = DataLayout.floatSpec(state.layout, bits)
      return { size, alignment: spec?.abiAlignment ?? { _tag: 'Alignment', byteUnits: size } }
    }
    if (description._tag === 'Pointer') {
      const spec = DataLayout.pointerSpec(state.layout, description.addressSpace)
      if (spec === undefined) throw new Error('data layout has no pointer specification')
      return { size: BigInt(Math.ceil(spec.bitWidth / 8)), alignment: spec.abiAlignment }
    }
    if (description._tag === 'Array' || description._tag === 'Vector') {
      if (description._tag === 'Vector' && description.scalable) {
        throw new Error('scalable vector has no fixed allocation size')
      }
      const child = layoutOf(description.child, state, visiting)
      const length = description._tag === 'Array' ? description.length : BigInt(description.length)
      return { size: child.size * length, alignment: child.alignment }
    }
    let body: { readonly fields: ReadonlyArray<number>; readonly packed: boolean } | undefined
    if (description._tag === 'Structure') body = description
    else if (description._tag === 'NamedStructure') body = description.body
    if (body !== undefined) {
      let offset = 0n
      let maximumAlignment = 1n
      for (const field of body.fields) {
        const fieldLayout = layoutOf(field, state, visiting)
        const fieldAlignment = body.packed ? 1n : (fieldLayout.alignment.byteUnits ?? 1n)
        maximumAlignment = fieldAlignment > maximumAlignment ? fieldAlignment : maximumAlignment
        const remainder = offset % fieldAlignment
        if (remainder !== 0n) offset += fieldAlignment - remainder
        offset += fieldLayout.size
      }
      const remainder = offset % maximumAlignment
      if (remainder !== 0n) offset += maximumAlignment - remainder
      return {
        size: offset,
        alignment: { _tag: 'Alignment', byteUnits: maximumAlignment },
      }
    }
    throw new Error(`${description._tag} has no allocatable size`)
  } finally {
    visiting.delete(index)
  }
}

/**
 * Computes fixed allocation size in bytes from the builder's parsed data layout.
 *
 * **Gotchas**
 *
 * Recursive inline types, scalable vectors, unsized types, target extensions, and pointers without
 * a layout specification fail with {@link LlvmError}.
 *
 * @category types
 * @since 0.0.0
 */
export const sizeOf = Effect.fn('Type.sizeOf')(function* (
  builder: Builder.Builder,
  self: Type,
): Effect.fn.Return<bigint, LlvmError> {
  return yield* inspect(builder, self, 'Type.sizeOf', (_description, state, index) =>
    Result.try({
      try: () => layoutOf(index, state, new Set()).size,
      catch: (cause) =>
        wrappedFailure({
          operation: 'Type.sizeOf',
          message: 'Type has no fixed size',
          cause: cause,
        }),
    }),
  )
})

/**
 * Computes ABI alignment from the builder's parsed data layout, with the same sized-type constraints as {@link sizeOf}.
 *
 * @category types
 * @since 0.0.0
 */
export const alignmentOf = Effect.fn('Type.alignmentOf')(function* (
  builder: Builder.Builder,
  self: Type,
): Effect.fn.Return<Alignment.Alignment, LlvmError> {
  return yield* inspect(builder, self, 'Type.alignmentOf', (_description, state, index) =>
    Result.try({
      try: () => layoutOf(index, state, new Set()).alignment,
      catch: (cause) =>
        wrappedFailure({
          operation: 'Type.alignmentOf',
          message: 'Type has no alignment',
          cause: cause,
        }),
    }),
  )
})
