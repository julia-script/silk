import * as Effect from 'effect/Effect'
import type * as Builder from './Builder.js'
import * as ByteString from './ByteString.js'
import * as FunctionActor from './Function.js'
import type * as Global from './Global.js'
import * as BuilderState from './internal/BuilderState.js'
import * as CanonicalKey from './internal/CanonicalKey.js'
import type * as ConstantDescription from './internal/ConstantDescription.js'
import * as GlobalState from './internal/GlobalState.js'
import * as Handle from './internal/Handle.js'
import type * as TypeDescription from './internal/TypeDescription.js'
import { SilkError } from './SilkError.js'
import * as Type from './Type.js'

/**
 * Opaque builder-owned identity for a structurally interned LLVM constant or constant expression.
 *
 * @category constants
 * @since 0.0.0
 */
export interface Constant extends Handle.Handle<'Constant'> {}

/**
 * Exact LLVM floating-point storage formats accepted by {@link floatingRaw}.
 *
 * @category constants
 * @since 0.0.0
 */
export type FloatFormat = ConstantDescription.FloatFormat
/**
 * Cast operations supported by constant expressions.
 *
 * @category constants
 * @since 0.0.0
 */
export type CastKind = ConstantDescription.CastKind
/**
 * Integer binary operations supported by constant expressions.
 *
 * @category constants
 * @since 0.0.0
 */
export type BinaryKind = ConstantDescription.BinaryKind

/**
 * Semantic flags carried by an inline-assembly constant.
 *
 * @category constants
 * @since 0.0.0
 */
export interface AssemblyOptions {
  readonly sideEffect?: boolean
  readonly alignStack?: boolean
  readonly intelDialect?: boolean
  readonly canThrow?: boolean
}

/** @internal */
const bytes = (value: ByteString.ByteString | Uint8Array | string): ByteString.ByteString =>
  typeof value === 'string'
    ? ByteString.fromString(value)
    : value instanceof Uint8Array
      ? ByteString.fromUint8Array(value)
      : value

/** @internal */
const descriptionKey = (description: ConstantDescription.Description): string => {
  const type = CanonicalKey.integer(description.type)
  switch (description._tag) {
    case 'Global':
      return CanonicalKey.tagged('global', [type, CanonicalKey.integer(description.global)])
    case 'Integer':
      return CanonicalKey.tagged('integer', [
        type,
        CanonicalKey.integer(description.bitPattern),
        description.signed ? '1' : '0',
      ])
    case 'Float':
      return CanonicalKey.tagged('float', [
        type,
        description.format,
        CanonicalKey.bytes(description.bits),
      ])
    case 'Special':
      return CanonicalKey.tagged('special', [type, description.kind])
    case 'Aggregate':
      return CanonicalKey.tagged('aggregate', [
        type,
        description.kind,
        CanonicalKey.sequence(description.elements.map(CanonicalKey.integer)),
      ])
    case 'String':
      return CanonicalKey.tagged('string', [type, CanonicalKey.bytes(description.bytes)])
    case 'Splat':
      return CanonicalKey.tagged('splat', [type, CanonicalKey.integer(description.value)])
    case 'BlockAddress':
      return CanonicalKey.tagged('blockaddress', [
        type,
        CanonicalKey.integer(description.function),
        CanonicalKey.integer(description.block),
      ])
    case 'FunctionReference':
      return CanonicalKey.tagged(description.kind, [
        type,
        CanonicalKey.integer(description.function),
      ])
    case 'Cast':
      return CanonicalKey.tagged('cast', [
        type,
        description.kind,
        CanonicalKey.integer(description.value),
      ])
    case 'Binary':
      return CanonicalKey.tagged('binary', [
        type,
        description.kind,
        CanonicalKey.integer(description.left),
        CanonicalKey.integer(description.right),
      ])
    case 'GetElementPtr':
      return CanonicalKey.tagged('getelementptr', [
        type,
        CanonicalKey.integer(description.sourceType),
        CanonicalKey.integer(description.base),
        CanonicalKey.sequence(description.indices.map(CanonicalKey.integer)),
        description.inbounds ? '1' : '0',
        description.inrange === undefined ? 'none' : CanonicalKey.integer(description.inrange),
      ])
    case 'Assembly':
      return CanonicalKey.tagged('assembly', [
        type,
        CanonicalKey.bytes(description.assembly),
        CanonicalKey.bytes(description.constraints),
        description.sideEffect ? '1' : '0',
        description.alignStack ? '1' : '0',
        description.intelDialect ? '1' : '0',
        description.canThrow ? '1' : '0',
      ])
  }
}

/** @internal */
const handleAt = (handles: ReadonlyArray<Constant>, index: number, operation: string): Constant => {
  const handle = handles[index]
  if (handle === undefined) {
    throw new SilkError({ operation, message: 'Constant table handle is missing', cause: index })
  }
  return handle
}

/** @internal */
const typeHandleAt = (
  handles: ReadonlyArray<Type.Type>,
  index: number,
  operation: string,
): Type.Type => {
  const handle = handles[index]
  if (handle === undefined) {
    throw new SilkError({ operation, message: 'Type table handle is missing', cause: index })
  }
  return handle
}

/** @internal */
const intern = Effect.fn('Constant.intern')(function* (
  builder: Builder.Builder,
  description: ConstantDescription.Description,
) {
  return yield* BuilderState.mutate(builder, 'Constant.intern', (state, owner) => {
    const key = descriptionKey(description)
    const found = state.constantKeys.get(key)
    if (found !== undefined) return handleAt(state.constantHandles, found, 'Constant.intern')
    const index = state.constants.length
    const handle = Handle.make('Constant', owner, index)
    state.constants.push(description)
    state.constantHandles.push(handle)
    state.constantKeys.set(key, index)
    return handle
  })
})

/** @internal */
const typeDescription = (
  state: BuilderState.MutableState,
  index: number,
  operation: string,
): TypeDescription.Description => {
  const description = state.types[index]
  if (description === undefined) {
    throw new SilkError({ operation, message: 'Type table entry is missing', cause: index })
  }
  return description
}

/** @internal */
const constantDescription = (
  state: BuilderState.MutableState,
  index: number,
  operation: string,
): ConstantDescription.Description => {
  const description = state.constants[index]
  if (description === undefined) {
    throw new SilkError({ operation, message: 'Constant table entry is missing', cause: index })
  }
  return description
}

/** @internal */
const integerOf = Effect.fn('Constant.integerOf')(function* (
  builder: Builder.Builder,
  type: Type.Type,
  value: bigint,
  signed: boolean,
): Effect.fn.Return<Constant, SilkError> {
  const description = yield* BuilderState.mutate(builder, 'Constant.integer', (state, owner) => {
    const typeIndex = Handle.resolve(builder, owner, type, 'Type', 'Constant.integer')
    const typeValue = typeDescription(state, typeIndex, 'Constant.integer')
    if (typeValue._tag !== 'Integer') {
      throw new SilkError({
        operation: 'Constant.integer',
        message: 'Integer constants require an integer type',
        cause: type,
      })
    }
    const modulus = 1n << BigInt(typeValue.bitWidth)
    const minimum = signed ? -(1n << BigInt(typeValue.bitWidth - 1)) : 0n
    const maximum = signed ? (1n << BigInt(typeValue.bitWidth - 1)) - 1n : modulus - 1n
    if (value < minimum || value > maximum) {
      throw new SilkError({
        operation: 'Constant.integer',
        message: `Integer value does not fit i${typeValue.bitWidth}`,
        cause: value,
      })
    }
    return Object.freeze({
      _tag: 'Integer' as const,
      type: typeIndex,
      bitPattern: value < 0n ? modulus + value : value,
      signed,
    })
  })
  return yield* intern(builder, description)
})

/**
 * Interns a non-negative arbitrary-width integer after checking it fits the declared integer type.
 *
 * **Gotchas**
 *
 * Use `bigint` for values outside JavaScript's safe integer range; no precision is discarded.
 *
 * **Example** (Interning a wide integer)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silk-effect/llvm/Builder'
 * import * as Constant from '@silk-effect/llvm/Constant'
 * import * as Type from '@silk-effect/llvm/Type'
 *
 * const bits = await Effect.runPromise(Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const i129 = yield* Type.integer(builder, 129)
 *   const value = yield* Constant.integerUnsigned(builder, i129, (1n << 128n) + 1n)
 *   return yield* Constant.integerBitPattern(builder, value)
 * }))
 * ```
 *
 * @category constants
 * @since 0.0.0
 */
export const integerUnsigned = Effect.fn('Constant.integerUnsigned')(function* (
  builder: Builder.Builder,
  type: Type.Type,
  value: number | bigint,
): Effect.fn.Return<Constant, SilkError> {
  return yield* integerOf(builder, type, typeof value === 'bigint' ? value : BigInt(value), false)
})

/**
 * Interns the opaque pointer constant that refers to a module global.
 *
 * @category constants
 * @since 0.0.0
 */
export const fromGlobal = Effect.fn('Constant.fromGlobal')(function* (
  builder: Builder.Builder,
  global: Global.Global,
): Effect.fn.Return<Constant, SilkError> {
  const resolved = yield* BuilderState.mutate(builder, 'Constant.fromGlobal', (state, owner) =>
    GlobalState.resolve(builder, state, owner, global, 'Constant.fromGlobal'),
  )
  const pointer = yield* Type.pointer(builder, resolved.description.addressSpace)
  const description = yield* BuilderState.mutate(builder, 'Constant.fromGlobal', (_state, owner) =>
    Object.freeze({
      _tag: 'Global' as const,
      type: Handle.resolve(builder, owner, pointer, 'Type', 'Constant.fromGlobal'),
      global: resolved.index,
    }),
  )
  return yield* intern(builder, description)
})

/**
 * Interns a signed arbitrary-width integer and stores its exact two's-complement bit pattern.
 *
 * @category constants
 * @since 0.0.0
 */
export const integerSigned = Effect.fn('Constant.integerSigned')(function* (
  builder: Builder.Builder,
  type: Type.Type,
  value: number | bigint,
): Effect.fn.Return<Constant, SilkError> {
  return yield* integerOf(builder, type, typeof value === 'bigint' ? value : BigInt(value), true)
})

const formatTypeTag: Record<FloatFormat, TypeDescription.SimpleTag> = {
  half: 'Half',
  bfloat: 'BFloat',
  float: 'Float',
  double: 'Double',
  x86_fp80: 'X86Fp80',
  fp128: 'Fp128',
  ppc_fp128: 'PpcFp128',
}

const formatBytes: Record<FloatFormat, number> = {
  half: 2,
  bfloat: 2,
  float: 4,
  double: 8,
  x86_fp80: 10,
  fp128: 16,
  ppc_fp128: 16,
}

/**
 * Interns byte-exact floating-point bits without JavaScript number conversion.
 *
 * **When to use**
 *
 * Use when you need NaN payloads, infinities with specific encodings, or formats JavaScript cannot
 * represent.
 *
 * **Gotchas**
 *
 * The format, type, and exact byte length must agree.
 *
 * **Example** (Preserving a NaN payload)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silk-effect/llvm/Builder'
 * import * as Constant from '@silk-effect/llvm/Constant'
 * import * as Type from '@silk-effect/llvm/Type'
 *
 * const nan = await Effect.runPromise(Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const type = yield* Type.double(builder)
 *   return yield* Constant.floatingRaw(
 *     builder,
 *     type,
 *     'double',
 *     Uint8Array.of(1, 0, 0, 0, 0, 0, 0xF8, 0x7F),
 *   )
 * }))
 * ```
 *
 * @category constants
 * @since 0.0.0
 */
export const floatingRaw = Effect.fn('Constant.floatingRaw')(function* (
  builder: Builder.Builder,
  type: Type.Type,
  format: FloatFormat,
  bits: ByteString.ByteString | Uint8Array,
): Effect.fn.Return<Constant, SilkError> {
  const value = bits instanceof Uint8Array ? ByteString.fromUint8Array(bits) : bits
  const description = yield* BuilderState.mutate(
    builder,
    'Constant.floatingRaw',
    (state, owner) => {
      const typeIndex = Handle.resolve(builder, owner, type, 'Type', 'Constant.floatingRaw')
      const typeValue = typeDescription(state, typeIndex, 'Constant.floatingRaw')
      if (typeValue._tag !== 'Simple' || typeValue.tag !== formatTypeTag[format]) {
        throw new SilkError({
          operation: 'Constant.floatingRaw',
          message: `Raw ${format} bits require the matching LLVM floating type`,
          cause: type,
        })
      }
      if (value.bytes.length !== formatBytes[format]) {
        throw new SilkError({
          operation: 'Constant.floatingRaw',
          message: `${format} requires exactly ${formatBytes[format]} bytes`,
          cause: bits,
        })
      }
      return Object.freeze({ _tag: 'Float' as const, type: typeIndex, format, bits: value })
    },
  )
  return yield* intern(builder, description)
})

/**
 * Convenience constructor for a raw 16-bit IEEE `half` payload.
 *
 * @category constants
 * @since 0.0.0
 */
export const halfBits = Effect.fn('Constant.halfBits')(function* (
  builder: Builder.Builder,
  type: Type.Type,
  bits: number,
) {
  return yield* floatingRaw(builder, type, 'half', Uint8Array.of(bits & 0xff, (bits >>> 8) & 0xff))
})

/**
 * Convenience constructor for a raw 16-bit `bfloat` payload.
 *
 * @category constants
 * @since 0.0.0
 */
export const bfloatBits = Effect.fn('Constant.bfloatBits')(function* (
  builder: Builder.Builder,
  type: Type.Type,
  bits: number,
) {
  return yield* floatingRaw(
    builder,
    type,
    'bfloat',
    Uint8Array.of(bits & 0xff, (bits >>> 8) & 0xff),
  )
})

/** @internal */
const numberBytes = (value: number, byteLength: 4 | 8): Uint8Array => {
  const result = new Uint8Array(byteLength)
  const view = new DataView(result.buffer)
  if (byteLength === 4) view.setFloat32(0, value, true)
  else view.setFloat64(0, value, true)
  return result
}

/**
 * Converts a JavaScript number to IEEE little-endian float bits and interns the constant.
 *
 * @category constants
 * @since 0.0.0
 */
export const floatFromNumber = Effect.fn('Constant.floatFromNumber')(function* (
  builder: Builder.Builder,
  type: Type.Type,
  value: number,
) {
  return yield* floatingRaw(builder, type, 'float', numberBytes(value, 4))
})

/**
 * Converts a JavaScript number to IEEE little-endian double bits and interns the constant.
 *
 * @category constants
 * @since 0.0.0
 */
export const doubleFromNumber = Effect.fn('Constant.doubleFromNumber')(function* (
  builder: Builder.Builder,
  type: Type.Type,
  value: number,
) {
  return yield* floatingRaw(builder, type, 'double', numberBytes(value, 8))
})

/** @internal */
const special = Effect.fn('Constant.special')(function* (
  builder: Builder.Builder,
  type: Type.Type,
  kind: Extract<ConstantDescription.Description, { readonly _tag: 'Special' }>['kind'],
) {
  const typeIndex = yield* BuilderState.mutate(builder, `Constant.${kind}`, (_state, owner) =>
    Handle.resolve(builder, owner, type, 'Type', `Constant.${kind}`),
  )
  return yield* intern(builder, Object.freeze({ _tag: 'Special', type: typeIndex, kind }))
})

/**
 * Returns the canonical `null` constant for a type.
 *
 * @category constants
 * @since 0.0.0
 */
export const nullValue = Effect.fn('Constant.nullValue')(function* (
  builder: Builder.Builder,
  type: Type.Type,
) {
  return yield* special(builder, type, 'null')
})

/**
 * Returns the canonical LLVM `none` token for a type.
 *
 * @category constants
 * @since 0.0.0
 */
export const none = Effect.fn('Constant.none')(function* (
  builder: Builder.Builder,
  type: Type.Type,
) {
  return yield* special(builder, type, 'none')
})

/**
 * Returns the canonical `zeroinitializer` for a type.
 *
 * @category constants
 * @since 0.0.0
 */
export const zero = Effect.fn('Constant.zero')(function* (
  builder: Builder.Builder,
  type: Type.Type,
) {
  return yield* special(builder, type, 'zeroinitializer')
})

/**
 * Returns the canonical deprecated-but-supported `undef` value for a type.
 *
 * @category constants
 * @since 0.0.0
 */
export const undef = Effect.fn('Constant.undef')(function* (
  builder: Builder.Builder,
  type: Type.Type,
) {
  return yield* special(builder, type, 'undef')
})

/**
 * Returns the canonical `poison` value for a type.
 *
 * @category constants
 * @since 0.0.0
 */
export const poison = Effect.fn('Constant.poison')(function* (
  builder: Builder.Builder,
  type: Type.Type,
) {
  return yield* special(builder, type, 'poison')
})

/**
 * Interns byte-exact contents as an `[N x i8]` string constant, optionally appending one NUL byte.
 *
 * @category constants
 * @since 0.0.0
 */
export const string = Effect.fn('Constant.string')(function* (
  builder: Builder.Builder,
  value: ByteString.ByteString | Uint8Array | string,
  options: { readonly nullTerminated?: boolean } = {},
): Effect.fn.Return<Constant, SilkError> {
  const input = bytes(value)
  const contents = options.nullTerminated
    ? ByteString.concat([input, ByteString.fromUint8Array(Uint8Array.of(0))])
    : input
  const i8 = yield* Type.integer(builder, 8)
  const type = yield* Type.array(builder, i8, contents.bytes.length)
  const typeIndex = yield* BuilderState.mutate(builder, 'Constant.string', (_state, owner) =>
    Handle.resolve(builder, owner, type, 'Type', 'Constant.string'),
  )
  return yield* intern(builder, Object.freeze({ _tag: 'String', type: typeIndex, bytes: contents }))
})

/**
 * Interns an array, vector, or complete structure after exact element-count and type validation.
 *
 * @category constants
 * @since 0.0.0
 */
export const aggregate = Effect.fn('Constant.aggregate')(function* (
  builder: Builder.Builder,
  type: Type.Type,
  elements: ReadonlyArray<Constant>,
): Effect.fn.Return<Constant, SilkError> {
  const description = yield* BuilderState.mutate(builder, 'Constant.aggregate', (state, owner) => {
    const typeIndex = Handle.resolve(builder, owner, type, 'Type', 'Constant.aggregate')
    const typeValue = typeDescription(state, typeIndex, 'Constant.aggregate')
    const elementIndices = Object.freeze(
      elements.map((element) =>
        Handle.resolve(builder, owner, element, 'Constant', 'Constant.aggregate'),
      ),
    )
    let expected: ReadonlyArray<number>
    let kind: Extract<ConstantDescription.Description, { readonly _tag: 'Aggregate' }>['kind']
    if (typeValue._tag === 'Array' || typeValue._tag === 'Vector') {
      const length = typeValue._tag === 'Array' ? typeValue.length : BigInt(typeValue.length)
      if (length !== BigInt(elementIndices.length)) {
        throw new SilkError({
          operation: 'Constant.aggregate',
          message: 'Aggregate element count does not match its type',
          cause: { type, elements },
        })
      }
      expected = Object.freeze(elementIndices.map(() => typeValue.child))
      kind = typeValue._tag === 'Array' ? 'array' : 'vector'
    } else {
      const body =
        typeValue._tag === 'Structure'
          ? typeValue
          : typeValue._tag === 'NamedStructure'
            ? typeValue.body
            : undefined
      if (body === undefined || body.fields.length !== elementIndices.length) {
        throw new SilkError({
          operation: 'Constant.aggregate',
          message: 'Aggregate elements require a complete matching aggregate type',
          cause: { type, elements },
        })
      }
      expected = body.fields
      kind = body.packed ? 'packed-structure' : 'structure'
    }
    for (let index = 0; index < elementIndices.length; index += 1) {
      const element = constantDescription(state, elementIndices[index] ?? -1, 'Constant.aggregate')
      if (element.type !== expected[index]) {
        throw new SilkError({
          operation: 'Constant.aggregate',
          message: 'Aggregate element type does not match its position',
          cause: { index, type, elements },
        })
      }
    }
    return Object.freeze({
      _tag: 'Aggregate' as const,
      type: typeIndex,
      kind,
      elements: elementIndices,
    })
  })
  return yield* intern(builder, description)
})

/**
 * Interns a vector splat whose scalar value exactly matches the vector child type.
 *
 * @category constants
 * @since 0.0.0
 */
export const splat = Effect.fn('Constant.splat')(function* (
  builder: Builder.Builder,
  type: Type.Type,
  value: Constant,
): Effect.fn.Return<Constant, SilkError> {
  const description = yield* BuilderState.mutate(builder, 'Constant.splat', (state, owner) => {
    const typeIndex = Handle.resolve(builder, owner, type, 'Type', 'Constant.splat')
    const valueIndex = Handle.resolve(builder, owner, value, 'Constant', 'Constant.splat')
    const typeValue = typeDescription(state, typeIndex, 'Constant.splat')
    const constantValue = constantDescription(state, valueIndex, 'Constant.splat')
    if (typeValue._tag !== 'Vector' || constantValue.type !== typeValue.child) {
      throw new SilkError({
        operation: 'Constant.splat',
        message: 'A splat value must match the vector child type',
        cause: { type, value },
      })
    }
    return Object.freeze({ _tag: 'Splat' as const, type: typeIndex, value: valueIndex })
  })
  return yield* intern(builder, description)
})

/** @internal */
const functionConstant = Effect.fn('Constant.functionConstant')(function* (
  builder: Builder.Builder,
  fn: FunctionActor.Function,
): Effect.fn.Return<Constant, SilkError> {
  return yield* fromGlobal(builder, yield* FunctionActor.global(builder, fn))
})

/**
 * Interns a `blockaddress` constant for a non-negative block index in a function.
 *
 * @category constants
 * @since 0.0.0
 */
export const blockAddress = Effect.fn('Constant.blockAddress')(function* (
  builder: Builder.Builder,
  fn: FunctionActor.Function,
  block: number,
): Effect.fn.Return<Constant, SilkError> {
  if (!Number.isSafeInteger(block) || block < 0) {
    return yield* Effect.fail(
      new SilkError({
        operation: 'Constant.blockAddress',
        message: 'A block address requires a non-negative block index',
        cause: block,
      }),
    )
  }
  const reference = yield* functionConstant(builder, fn)
  const description = yield* BuilderState.mutate(
    builder,
    'Constant.blockAddress',
    (state, owner) => {
      const functionIndex = Handle.resolve(builder, owner, fn, 'Function', 'Constant.blockAddress')
      const body = state.functions[functionIndex]?.body
      if (body !== undefined && block >= body.blocks.length) {
        throw new SilkError({
          operation: 'Constant.blockAddress',
          message: 'Block index is outside the function body',
          cause: block,
        })
      }
      const referenceIndex = Handle.resolve(
        builder,
        owner,
        reference,
        'Constant',
        'Constant.blockAddress',
      )
      const type = constantDescription(state, referenceIndex, 'Constant.blockAddress').type
      return Object.freeze({
        _tag: 'BlockAddress' as const,
        type,
        function: referenceIndex,
        block,
      })
    },
  )
  return yield* intern(builder, description)
})

/** @internal */
const functionReference = Effect.fn('Constant.functionReference')(function* (
  builder: Builder.Builder,
  fn: FunctionActor.Function,
  kind: 'dso_local_equivalent' | 'no_cfi',
): Effect.fn.Return<Constant, SilkError> {
  const reference = yield* functionConstant(builder, fn)
  const description = yield* BuilderState.mutate(builder, `Constant.${kind}`, (state, owner) => {
    const referenceIndex = Handle.resolve(builder, owner, reference, 'Constant', `Constant.${kind}`)
    return Object.freeze({
      _tag: 'FunctionReference' as const,
      kind,
      function: referenceIndex,
      type: constantDescription(state, referenceIndex, `Constant.${kind}`).type,
    })
  })
  return yield* intern(builder, description)
})

/**
 * Interns LLVM's `dso_local_equivalent` function reference constant.
 *
 * @category constants
 * @since 0.0.0
 */
export const dsoLocalEquivalent = Effect.fn('Constant.dsoLocalEquivalent')(function* (
  builder: Builder.Builder,
  fn: FunctionActor.Function,
): Effect.fn.Return<Constant, SilkError> {
  return yield* functionReference(builder, fn, 'dso_local_equivalent')
})

/**
 * Interns LLVM's `no_cfi` function reference constant.
 *
 * @category constants
 * @since 0.0.0
 */
export const noCfi = Effect.fn('Constant.noCfi')(function* (
  builder: Builder.Builder,
  fn: FunctionActor.Function,
): Effect.fn.Return<Constant, SilkError> {
  return yield* functionReference(builder, fn, 'no_cfi')
})

/** @internal */
const castScalar = (
  state: BuilderState.MutableState,
  index: number,
): TypeDescription.Description => {
  const description = typeDescription(state, index, 'Constant.cast')
  return description._tag === 'Vector'
    ? typeDescription(state, description.child, 'Constant.cast')
    : description
}

/** @internal */
const castWidth = (description: TypeDescription.Description): number | undefined => {
  if (description._tag === 'Integer') return description.bitWidth
  if (description._tag !== 'Simple') return undefined
  switch (description.tag) {
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
    default:
      return undefined
  }
}

/**
 * Interns a type-checked integer, pointer, address-space, or equal-width bitcast expression.
 *
 * @category constants
 * @since 0.0.0
 */
export const cast = Effect.fn('Constant.cast')(function* (
  builder: Builder.Builder,
  kind: CastKind,
  value: Constant,
  type: Type.Type,
): Effect.fn.Return<Constant, SilkError> {
  const description = yield* BuilderState.mutate(builder, 'Constant.cast', (state, owner) => {
    const valueIndex = Handle.resolve(builder, owner, value, 'Constant', 'Constant.cast')
    const destinationIndex = Handle.resolve(builder, owner, type, 'Type', 'Constant.cast')
    const sourceIndex = constantDescription(state, valueIndex, 'Constant.cast').type
    const sourceType = typeDescription(state, sourceIndex, 'Constant.cast')
    const destinationType = typeDescription(state, destinationIndex, 'Constant.cast')
    const sourceScalar = castScalar(state, sourceIndex)
    const destinationScalar = castScalar(state, destinationIndex)
    const sameVectorShape =
      sourceType._tag === 'Vector' || destinationType._tag === 'Vector'
        ? sourceType._tag === 'Vector' &&
          destinationType._tag === 'Vector' &&
          sourceType.length === destinationType.length &&
          sourceType.scalable === destinationType.scalable
        : true
    const valid =
      sameVectorShape &&
      ((kind === 'trunc' &&
        sourceScalar._tag === 'Integer' &&
        destinationScalar._tag === 'Integer' &&
        sourceScalar.bitWidth > destinationScalar.bitWidth) ||
        (kind === 'ptrtoint' &&
          sourceScalar._tag === 'Pointer' &&
          destinationScalar._tag === 'Integer') ||
        (kind === 'inttoptr' &&
          sourceScalar._tag === 'Integer' &&
          destinationScalar._tag === 'Pointer') ||
        (kind === 'bitcast' &&
          ((sourceScalar._tag === 'Pointer' && destinationScalar._tag === 'Pointer') ||
            castWidth(sourceScalar) === castWidth(destinationScalar))) ||
        (kind === 'addrspacecast' &&
          sourceScalar._tag === 'Pointer' &&
          destinationScalar._tag === 'Pointer'))
    if (!valid) {
      throw new SilkError({
        operation: 'Constant.cast',
        message: `${kind} is invalid for the constant source and destination types`,
        cause: { value, type },
      })
    }
    return Object.freeze({
      _tag: 'Cast' as const,
      kind,
      value: valueIndex,
      type: destinationIndex,
    })
  })
  return yield* intern(builder, description)
})

/**
 * Interns an integer binary constant expression whose operands have exactly one type.
 *
 * @category constants
 * @since 0.0.0
 */
export const binary = Effect.fn('Constant.binary')(function* (
  builder: Builder.Builder,
  kind: BinaryKind,
  left: Constant,
  right: Constant,
): Effect.fn.Return<Constant, SilkError> {
  const description = yield* BuilderState.mutate(builder, 'Constant.binary', (state, owner) => {
    const leftIndex = Handle.resolve(builder, owner, left, 'Constant', 'Constant.binary')
    const rightIndex = Handle.resolve(builder, owner, right, 'Constant', 'Constant.binary')
    const leftValue = constantDescription(state, leftIndex, 'Constant.binary')
    const rightValue = constantDescription(state, rightIndex, 'Constant.binary')
    if (leftValue.type !== rightValue.type) {
      throw new SilkError({
        operation: 'Constant.binary',
        message: 'Binary constant operands must have one type',
        cause: { left, right },
      })
    }
    if (castScalar(state, leftValue.type)._tag !== 'Integer') {
      throw new SilkError({
        operation: 'Constant.binary',
        message: 'Pinned constant binary expressions require integer operands',
        cause: { left, right },
      })
    }
    return Object.freeze({
      _tag: 'Binary' as const,
      kind,
      type: leftValue.type,
      left: leftIndex,
      right: rightIndex,
    })
  })
  return yield* intern(builder, description)
})

/**
 * Interns a validated scalar or vector `getelementptr` constant expression.
 *
 * **Gotchas**
 *
 * Structure indices must be integer constants selecting an existing field. Vector inputs
 * must share one shape, `inrange` must name an index, and the result pointer shape is checked.
 *
 * @category constants
 * @since 0.0.0
 */
export const getElementPtr = Effect.fn('Constant.getElementPtr')(function* (
  builder: Builder.Builder,
  sourceType: Type.Type,
  resultType: Type.Type,
  base: Constant,
  indices: ReadonlyArray<Constant>,
  options: { readonly inbounds?: boolean; readonly inrange?: number } = {},
): Effect.fn.Return<Constant, SilkError> {
  const description = yield* BuilderState.mutate(
    builder,
    'Constant.getElementPtr',
    (state, owner) => {
      const sourceTypeIndex = Handle.resolve(
        builder,
        owner,
        sourceType,
        'Type',
        'Constant.getElementPtr',
      )
      const resultTypeIndex = Handle.resolve(
        builder,
        owner,
        resultType,
        'Type',
        'Constant.getElementPtr',
      )
      const baseIndex = Handle.resolve(builder, owner, base, 'Constant', 'Constant.getElementPtr')
      const baseValue = constantDescription(state, baseIndex, 'Constant.getElementPtr')
      const baseType = typeDescription(state, baseValue.type, 'Constant.getElementPtr')
      const basePointer =
        baseType._tag === 'Vector'
          ? typeDescription(state, baseType.child, 'Constant.getElementPtr')
          : baseType
      if (basePointer._tag !== 'Pointer') {
        throw new SilkError({
          operation: 'Constant.getElementPtr',
          message: 'Constant GEP base must be a pointer or vector of pointers',
          cause: base,
        })
      }
      if (
        options.inrange !== undefined &&
        (!Number.isSafeInteger(options.inrange) ||
          options.inrange < 0 ||
          options.inrange >= indices.length)
      ) {
        throw new SilkError({
          operation: 'Constant.getElementPtr',
          message: 'inrange must identify an existing GEP index',
          cause: options.inrange,
        })
      }
      const indexValues = Object.freeze(
        indices.map((index) =>
          Handle.resolve(builder, owner, index, 'Constant', 'Constant.getElementPtr'),
        ),
      )
      let current = sourceTypeIndex
      let vector =
        baseType._tag === 'Vector'
          ? { length: baseType.length, scalable: baseType.scalable }
          : undefined
      for (let position = 0; position < indexValues.length; position += 1) {
        const index = indexValues[position]
        const indexValue = constantDescription(state, index ?? -1, 'Constant.getElementPtr')
        const indexType = typeDescription(state, indexValue.type, 'Constant.getElementPtr')
        const scalar =
          indexType._tag === 'Vector'
            ? typeDescription(state, indexType.child, 'Constant.getElementPtr')
            : indexType
        if (scalar._tag !== 'Integer') {
          throw new SilkError({
            operation: 'Constant.getElementPtr',
            message: 'getelementptr indices must be integer scalars or vectors',
            cause: indices,
          })
        }
        if (indexType._tag === 'Vector') {
          const shape = { length: indexType.length, scalable: indexType.scalable }
          if (
            vector !== undefined &&
            (vector.length !== shape.length || vector.scalable !== shape.scalable)
          ) {
            throw new SilkError({
              operation: 'Constant.getElementPtr',
              message: 'Vector GEP operands must have one vector shape',
              cause: indices,
            })
          }
          vector = shape
        }
        if (position === 0) continue
        const aggregate = typeDescription(state, current, 'Constant.getElementPtr')
        if (aggregate._tag === 'Array' || aggregate._tag === 'Vector') {
          current = aggregate.child
          continue
        }
        const body =
          aggregate._tag === 'Structure'
            ? aggregate
            : aggregate._tag === 'NamedStructure'
              ? aggregate.body
              : undefined
        const field =
          body !== undefined &&
          indexValue._tag === 'Integer' &&
          indexValue.bitPattern <= BigInt(Number.MAX_SAFE_INTEGER)
            ? body.fields[Number(indexValue.bitPattern)]
            : undefined
        if (field === undefined) {
          throw new SilkError({
            operation: 'Constant.getElementPtr',
            message: 'Constant GEP path cannot select the requested aggregate child',
            cause: indices,
          })
        }
        current = field
      }
      const result = typeDescription(state, resultTypeIndex, 'Constant.getElementPtr')
      const expectedPointerIndex = baseType._tag === 'Vector' ? baseType.child : baseValue.type
      const validResult =
        vector === undefined
          ? resultTypeIndex === expectedPointerIndex
          : result._tag === 'Vector' &&
            result.child === expectedPointerIndex &&
            result.length === vector.length &&
            result.scalable === vector.scalable
      if (!validResult) {
        throw new SilkError({
          operation: 'Constant.getElementPtr',
          message: 'Constant GEP result type does not match its pointer/vector shape',
          cause: resultType,
        })
      }
      return Object.freeze({
        _tag: 'GetElementPtr' as const,
        sourceType: sourceTypeIndex,
        type: resultTypeIndex,
        base: baseIndex,
        indices: indexValues,
        inbounds: options.inbounds ?? false,
        inrange: options.inrange,
      })
    },
  )
  return yield* intern(builder, description)
})

/**
 * Interns byte-preserving inline assembly and its constraints for a function type.
 *
 * @category constants
 * @since 0.0.0
 */
export const assembly = Effect.fn('Constant.assembly')(function* (
  builder: Builder.Builder,
  type: Type.Type,
  assembly: ByteString.ByteString | Uint8Array | string,
  constraints: ByteString.ByteString | Uint8Array | string,
  options: AssemblyOptions = {},
): Effect.fn.Return<Constant, SilkError> {
  const typeIndex = yield* BuilderState.mutate(builder, 'Constant.assembly', (_state, owner) =>
    Handle.resolve(builder, owner, type, 'Type', 'Constant.assembly'),
  )
  return yield* intern(
    builder,
    Object.freeze({
      _tag: 'Assembly',
      type: typeIndex,
      assembly: bytes(assembly),
      constraints: bytes(constraints),
      sideEffect: options.sideEffect ?? false,
      alignStack: options.alignStack ?? false,
      intelDialect: options.intelDialect ?? false,
      canThrow: options.canThrow ?? false,
    }),
  )
})

/**
 * Returns the builder-owned LLVM type of a constant.
 *
 * @category constants
 * @since 0.0.0
 */
export const typeOf = Effect.fn('Constant.typeOf')(function* (
  builder: Builder.Builder,
  self: Constant,
): Effect.fn.Return<Type.Type, SilkError> {
  return yield* BuilderState.mutate(builder, 'Constant.typeOf', (state, owner) => {
    const index = Handle.resolve(builder, owner, self, 'Constant', 'Constant.typeOf')
    const description = constantDescription(state, index, 'Constant.typeOf')
    return typeHandleAt(state.typeHandles, description.type, 'Constant.typeOf')
  })
})

/**
 * Returns the constant's stable runtime representation tag.
 *
 * @category constants
 * @since 0.0.0
 */
export const tag = Effect.fn('Constant.tag')(function* (
  builder: Builder.Builder,
  self: Constant,
): Effect.fn.Return<ConstantDescription.Description['_tag'], SilkError> {
  return yield* BuilderState.mutate(builder, 'Constant.tag', (state, owner) => {
    const index = Handle.resolve(builder, owner, self, 'Constant', 'Constant.tag')
    return constantDescription(state, index, 'Constant.tag')._tag
  })
})

/**
 * Reads an integer constant's normalized unsigned bit pattern as `bigint`.
 *
 * @category constants
 * @since 0.0.0
 */
export const integerBitPattern = Effect.fn('Constant.integerBitPattern')(function* (
  builder: Builder.Builder,
  self: Constant,
): Effect.fn.Return<bigint, SilkError> {
  return yield* BuilderState.mutate(builder, 'Constant.integerBitPattern', (state, owner) => {
    const index = Handle.resolve(builder, owner, self, 'Constant', 'Constant.integerBitPattern')
    const description = constantDescription(state, index, 'Constant.integerBitPattern')
    if (description._tag !== 'Integer') {
      throw new SilkError({
        operation: 'Constant.integerBitPattern',
        message: 'Expected an integer constant',
        cause: self,
      })
    }
    return description.bitPattern
  })
})

/**
 * Reads a floating constant's byte-exact immutable payload.
 *
 * @category constants
 * @since 0.0.0
 */
export const floatingBits = Effect.fn('Constant.floatingBits')(function* (
  builder: Builder.Builder,
  self: Constant,
): Effect.fn.Return<ByteString.ByteString, SilkError> {
  return yield* BuilderState.mutate(builder, 'Constant.floatingBits', (state, owner) => {
    const index = Handle.resolve(builder, owner, self, 'Constant', 'Constant.floatingBits')
    const description = constantDescription(state, index, 'Constant.floatingBits')
    if (description._tag !== 'Float') {
      throw new SilkError({
        operation: 'Constant.floatingBits',
        message: 'Expected a floating-point constant',
        cause: self,
      })
    }
    return description.bits
  })
})
