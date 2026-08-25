import * as Effect from 'effect/Effect'
import * as AddrSpace from './AddrSpace.js'
import * as Alignment from './Alignment.js'
import * as ByteString from './ByteString.js'
import { type LlvmError, wrappedFailure } from './LlvmError.js'

/**
 * Alignment rules for one integer, floating-point, or vector bit width.
 *
 * @category data layouts
 * @since 0.0.0
 */
export interface PrimitiveSpec {
  readonly bitWidth: number
  readonly abiAlignment: Alignment.Alignment
  readonly preferredAlignment: Alignment.Alignment
}

/**
 * Pointer and index widths for one LLVM address space.
 *
 * @category data layouts
 * @since 0.0.0
 */
export interface PointerSpec {
  readonly addressSpace: AddrSpace.AddrSpace
  readonly bitWidth: number
  readonly abiAlignment: Alignment.Alignment
  readonly preferredAlignment: Alignment.Alignment
  readonly indexBitWidth: number
}

/**
 * Parsed, immutable view of an LLVM target data-layout string.
 *
 * @category data layouts
 * @since 0.0.0
 */
export interface DataLayout {
  readonly _tag: 'DataLayout'
  readonly original: ByteString.ByteString
  readonly endian: 'little' | 'big' | undefined
  readonly integers: ReadonlyArray<PrimitiveSpec>
  readonly floats: ReadonlyArray<PrimitiveSpec>
  readonly vectors: ReadonlyArray<PrimitiveSpec>
  readonly pointers: ReadonlyArray<PointerSpec>
  readonly nativeIntegerWidths: ReadonlyArray<number>
  readonly stackAlignment: Alignment.Alignment
  readonly nonIntegralAddressSpaces: ReadonlyArray<AddrSpace.AddrSpace>
  readonly mangling: string | undefined
  readonly programAddressSpace: AddrSpace.AddrSpace
  readonly globalAddressSpace: AddrSpace.AddrSpace
  readonly allocaAddressSpace: AddrSpace.AddrSpace
  readonly functionPointerAlignment: string | undefined
}

/**
 * A data layout with no target-specific rules.
 *
 * @category data layouts
 * @since 0.0.0
 */
export const empty: DataLayout = Object.freeze({
  _tag: 'DataLayout',
  original: ByteString.empty,
  endian: undefined,
  integers: Object.freeze([]),
  floats: Object.freeze([]),
  vectors: Object.freeze([]),
  pointers: Object.freeze([]),
  nativeIntegerWidths: Object.freeze([]),
  stackAlignment: Alignment.defaultAlignment,
  nonIntegralAddressSpaces: Object.freeze([]),
  mangling: undefined,
  programAddressSpace: AddrSpace.defaultAddrSpace,
  globalAddressSpace: AddrSpace.defaultAddrSpace,
  allocaAddressSpace: AddrSpace.defaultAddrSpace,
  functionPointerAlignment: undefined,
})

/** @internal */
const ascii = (value: ByteString.ByteString): string => {
  let output = ''
  for (const byte of value.bytes) {
    if (byte > 0x7f) throw new Error('data layouts must contain ASCII bytes')
    output += String.fromCharCode(byte)
  }
  return output
}

/** @internal */
const natural = (value: string, component: string): number => {
  if (!/^\d+$/.test(value)) throw new Error(`invalid number in data-layout component ${component}`)
  const parsed = Number(value)
  if (!Number.isSafeInteger(parsed)) throw new Error(`number is too large in ${component}`)
  return parsed
}

/** @internal */
const addressSpace = (value: string, component: string): AddrSpace.AddrSpace => {
  const parsed = natural(value, component)
  if (parsed > 0xff_ffff) throw new Error(`address space is too large in ${component}`)
  return Object.freeze({ _tag: 'AddrSpace', value: parsed })
}

/** @internal */
const bitAlignment = (value: string, component: string): Alignment.Alignment => {
  const bits = natural(value, component)
  if (bits < 8 || bits % 8 !== 0) throw new Error(`invalid bit alignment in ${component}`)
  const bytes = BigInt(bits / 8)
  if ((bytes & (bytes - 1n)) !== 0n)
    throw new Error(`alignment is not a power of two in ${component}`)
  return Object.freeze({ _tag: 'Alignment', byteUnits: bytes })
}

/** @internal */
const primitive = (component: string): PrimitiveSpec => {
  const values = component.slice(1).split(':')
  if (values.length < 2 || values.length > 3) throw new Error(`malformed ${component}`)
  const bitWidth = natural(values[0] ?? '', component)
  const abiAlignment = bitAlignment(values[1] ?? '', component)
  const preferredAlignment = bitAlignment(values[2] ?? values[1] ?? '', component)
  return Object.freeze({ bitWidth, abiAlignment, preferredAlignment })
}

/** @internal */
const pointer = (component: string): PointerSpec => {
  const values = component.slice(1).split(':')
  if (values.length < 3 || values.length > 5) throw new Error(`malformed ${component}`)
  const address =
    values[0] === '' ? AddrSpace.defaultAddrSpace : addressSpace(values[0] ?? '', component)
  const bitWidth = natural(values[1] ?? '', component)
  const abiAlignment = bitAlignment(values[2] ?? '', component)
  const preferredAlignment = bitAlignment(values[3] ?? values[2] ?? '', component)
  const indexBitWidth = natural(values[4] ?? values[1] ?? '', component)
  return Object.freeze({
    addressSpace: address,
    bitWidth,
    abiAlignment,
    preferredAlignment,
    indexBitWidth,
  })
}

/** @internal */
const sorted = <A>(values: ReadonlyArray<A>, key: (value: A) => number): ReadonlyArray<A> =>
  Object.freeze([...values].sort((left, right) => key(left) - key(right)))

/** @internal */
const parseUnsafe = (original: ByteString.ByteString): DataLayout => {
  if (ByteString.isEmpty(original)) return empty
  let endian: DataLayout['endian']
  let mangling: string | undefined
  let stackAlignment = Alignment.defaultAlignment
  let programAddressSpace = AddrSpace.defaultAddrSpace
  let globalAddressSpace = AddrSpace.defaultAddrSpace
  let allocaAddressSpace = AddrSpace.defaultAddrSpace
  let functionPointerAlignment: string | undefined
  const integers: Array<PrimitiveSpec> = []
  const floats: Array<PrimitiveSpec> = []
  const vectors: Array<PrimitiveSpec> = []
  const pointers: Array<PointerSpec> = []
  const nativeIntegerWidths: Array<number> = []
  const nonIntegralAddressSpaces: Array<AddrSpace.AddrSpace> = []

  for (const component of ascii(original).split('-')) {
    if (component === 'e' || component === 'E') {
      endian = component === 'e' ? 'little' : 'big'
    } else if (/^i\d+:/.test(component)) {
      integers.push(primitive(component))
    } else if (/^f\d+:/.test(component)) {
      floats.push(primitive(component))
    } else if (/^v\d+:/.test(component)) {
      vectors.push(primitive(component))
    } else if (/^p\d*:/.test(component)) {
      pointers.push(pointer(component))
    } else if (/^n\d+(?::\d+)*$/.test(component)) {
      nativeIntegerWidths.push(
        ...component
          .slice(1)
          .split(':')
          .map((value) => natural(value, component)),
      )
    } else if (/^S\d+$/.test(component)) {
      stackAlignment = bitAlignment(component.slice(1), component)
    } else if (/^ni:\d+(?::\d+)*$/.test(component)) {
      nonIntegralAddressSpaces.push(
        ...component
          .slice(3)
          .split(':')
          .map((value) => addressSpace(value, component)),
      )
    } else if (/^m:[A-Za-z]$/.test(component)) {
      mangling = component.slice(2)
    } else if (/^P\d+$/.test(component)) {
      programAddressSpace = addressSpace(component.slice(1), component)
    } else if (/^G\d+$/.test(component)) {
      globalAddressSpace = addressSpace(component.slice(1), component)
    } else if (/^A\d+$/.test(component)) {
      allocaAddressSpace = addressSpace(component.slice(1), component)
    } else if (/^F[ni]\d+$/.test(component)) {
      functionPointerAlignment = component
    } else if (/^a:\d+(?::\d+)?$/.test(component)) {
      // Aggregate alignment affects layout queries only when no field-specific rule applies.
      primitive(`a${component.slice(1)}`)
    } else {
      throw new Error(`unsupported data-layout component ${component}`)
    }
  }

  return Object.freeze({
    _tag: 'DataLayout',
    original,
    endian,
    integers: sorted(integers, (value) => value.bitWidth),
    floats: sorted(floats, (value) => value.bitWidth),
    vectors: sorted(vectors, (value) => value.bitWidth),
    pointers: sorted(pointers, (value) => value.addressSpace.value),
    nativeIntegerWidths: Object.freeze(
      [...nativeIntegerWidths].sort((left, right) => left - right),
    ),
    stackAlignment,
    nonIntegralAddressSpaces: sorted(nonIntegralAddressSpaces, (value) => value.value),
    mangling,
    programAddressSpace,
    globalAddressSpace,
    allocaAddressSpace,
    functionPointerAlignment,
  })
}

/**
 * Parses the LLVM data-layout subset used by builder layout queries and preserves the original bytes.
 *
 * **Details**
 *
 * Entries are sorted for deterministic lookup. Input may be byte-exact ASCII or a JavaScript
 * string convenience value.
 *
 * **Gotchas**
 *
 * Unsupported or malformed components fail with {@link LlvmError} instead of being ignored,
 * preventing target-dependent silent guesses.
 *
 * **Example** (Parsing a target layout)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as DataLayout from '@silk-effect/llvm/DataLayout'
 *
 * const width = await Effect.runPromise(
 *   Effect.gen(function* () {
 *     const layout = yield* DataLayout.parse('e-p:64:64-i32:32')
 *     return DataLayout.pointerSpec(layout)?.bitWidth
 *   }),
 * )
 * // width === 64
 * ```
 *
 * @category data layouts
 * @since 0.0.0
 */
export const parse = Effect.fn('DataLayout.parse')(function* (
  input: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<DataLayout, LlvmError> {
  const original = ByteString.coerce(input)
  return yield* Effect.try({
    try: () => parseUnsafe(original),
    catch: (cause) =>
      wrappedFailure({
        operation: 'DataLayout.parse',
        message: cause instanceof Error ? cause.message : 'Invalid LLVM data layout',
        cause: cause,
      }),
  })
})

/**
 * Finds the exact integer-width rule, without synthesizing a fallback.
 *
 * @category data layouts
 * @since 0.0.0
 */
export const integerSpec = (self: DataLayout, bitWidth: number): PrimitiveSpec | undefined =>
  self.integers.find((spec) => spec.bitWidth === bitWidth)

/**
 * Finds the exact floating-point-width rule, without synthesizing a fallback.
 *
 * @category data layouts
 * @since 0.0.0
 */
export const floatSpec = (self: DataLayout, bitWidth: number): PrimitiveSpec | undefined =>
  self.floats.find((spec) => spec.bitWidth === bitWidth)

/**
 * Finds the exact vector-width rule, without synthesizing a fallback.
 *
 * @category data layouts
 * @since 0.0.0
 */
export const vectorSpec = (self: DataLayout, bitWidth: number): PrimitiveSpec | undefined =>
  self.vectors.find((spec) => spec.bitWidth === bitWidth)

/**
 * Finds a pointer rule for an address space, falling back to address space zero when present.
 *
 * @category data layouts
 * @since 0.0.0
 */
export const pointerSpec = (
  self: DataLayout,
  address: AddrSpace.AddrSpace = AddrSpace.defaultAddrSpace,
): PointerSpec | undefined =>
  self.pointers.find((spec) => spec.addressSpace.value === address.value) ??
  self.pointers.find((spec) => spec.addressSpace.value === 0)

/**
 * Returns the original, byte-exact data-layout spelling.
 *
 * @category data layouts
 * @since 0.0.0
 */
export const render = (self: DataLayout): ByteString.ByteString => self.original
