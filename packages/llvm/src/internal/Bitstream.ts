import { SilkError } from '../SilkError.js'

export interface Writer {
  readonly words: Array<number>
  bitBuffer: bigint
  bitCount: number
}

export type Scalar = number | bigint

export type AbbrevOp =
  | { readonly _tag: 'Literal'; readonly value: bigint }
  | { readonly _tag: 'Fixed'; readonly width: number }
  | { readonly _tag: 'Vbr'; readonly width: number }
  | { readonly _tag: 'Array' }
  | { readonly _tag: 'Char6' }
  | { readonly _tag: 'Blob' }

export interface Abbreviation {
  readonly name: string
  readonly ops: ReadonlyArray<AbbrevOp>
}

export interface Block {
  readonly id: number
  readonly abbreviations: ReadonlyArray<Abbreviation>
}

export interface BlockWriter {
  readonly writer: Writer
  readonly block: Block
  readonly abbrevWidth: number
  readonly sizeWordIndex: number
}

export type RecordValue = Scalar | ReadonlyArray<Scalar>

/** @internal */
const failure = (operation: string, message: string, cause: unknown): SilkError =>
  new SilkError({ operation, message, cause })

/** @internal */
const integer = (value: Scalar, operation: string): bigint => {
  if (typeof value === 'bigint') {
    if (value < 0n) throw failure(operation, 'Expected an unsigned integer', value)
    return value
  }
  if (!Number.isSafeInteger(value) || value < 0) {
    throw failure(operation, 'Use bigint for integers outside the safe number range', value)
  }
  return BigInt(value)
}

/** @internal */
const width = (value: number, operation: string, minimum: number): void => {
  if (!Number.isInteger(value) || value < minimum || value > 32) {
    throw failure(operation, `Expected a bit width from ${minimum} through 32`, value)
  }
}

/** @internal */
export const make = (): Writer => ({ words: [], bitBuffer: 0n, bitCount: 0 })

/**
 * The bit writer is deliberately imperative: `pnpm parity:bench` measures this per-bit loop and
 * the large-blob path. Allocating an Effect node per bit would scale allocation with encoded bit
 * count, so the loop remains contained behind Bitcode.encode's typed Effect boundary.
 *
 * @internal
 */
export const writeBits = (self: Writer, input: Scalar, bits: number): void => {
  if (!Number.isInteger(bits) || bits < 0) {
    throw failure('Bitstream.writeBits', 'Expected a non-negative bit count', bits)
  }
  let value = integer(input, 'Bitstream.writeBits')
  let remaining = bits

  while (remaining > 0) {
    const available = 32 - self.bitCount
    const taken = Math.min(available, remaining)
    const mask = (1n << BigInt(taken)) - 1n
    self.bitBuffer |= (value & mask) << BigInt(self.bitCount)
    self.bitCount += taken
    remaining -= taken
    value >>= BigInt(taken)

    if (self.bitCount === 32) {
      self.words.push(Number(self.bitBuffer & 0xffff_ffffn) >>> 0)
      self.bitBuffer = 0n
      self.bitCount = 0
    }
  }
}

/** @internal */
export const alignTo32 = (self: Writer): void => {
  if (self.bitCount === 0) return
  self.words.push(Number(self.bitBuffer & 0xffff_ffffn) >>> 0)
  self.bitBuffer = 0n
  self.bitCount = 0
}

/** @internal */
export const writeVbr = (self: Writer, input: Scalar, bits: number): void => {
  width(bits, 'Bitstream.writeVbr', 2)
  let value = integer(input, 'Bitstream.writeVbr')
  const payloadBits = BigInt(bits - 1)
  const payloadMask = (1n << payloadBits) - 1n
  const continuation = 1n << payloadBits

  do {
    const payload = value & payloadMask
    value >>= payloadBits
    writeBits(self, value === 0n ? payload : payload | continuation, bits)
  } while (value !== 0n)
}

/** @internal */
export const bitsVbr = (input: Scalar, bits: number): number => {
  width(bits, 'Bitstream.bitsVbr', 2)
  let value = integer(input, 'Bitstream.bitsVbr')
  const payloadBits = BigInt(bits - 1)
  let total = bits
  value >>= payloadBits
  while (value !== 0n) {
    total += bits
    value >>= payloadBits
  }
  return total
}

/** @internal */
export const char6 = (value: string): number => {
  if (value.length !== 1) {
    throw failure('Bitstream.char6', 'Expected exactly one character', value)
  }
  const code = value.charCodeAt(0)
  if (code >= 0x61 && code <= 0x7a) return code - 0x61
  if (code >= 0x41 && code <= 0x5a) return code - 0x41 + 26
  if (code >= 0x30 && code <= 0x39) return code - 0x30 + 52
  if (value === '.') return 62
  if (value === '_') return 63
  throw failure('Bitstream.char6', 'Character is not representable by LLVM char6', value)
}

/** @internal */
export const write6BitChar = (self: Writer, value: string): void => {
  writeBits(self, char6(value), 6)
}

/** @internal */
export const writeBlob = (self: Writer, bytes: ReadonlyArray<Scalar>): void => {
  // `parity:bench` exercises a one-megabyte blob; this measured byte-copy loop remains imperative
  // and is contained by the same Bitcode.encode Effect boundary as writeBits.
  alignTo32(self)
  const paddedLength = Math.ceil(bytes.length / 4) * 4
  const padded = new Uint8Array(paddedLength)
  for (let index = 0; index < bytes.length; index += 1) {
    const byte = integer(bytes[index] ?? 0, 'Bitstream.writeBlob')
    if (byte > 0xffn) {
      throw failure('Bitstream.writeBlob', 'Blob elements must be bytes', byte)
    }
    padded[index] = Number(byte)
  }
  const view = new DataView(padded.buffer)
  for (let offset = 0; offset < paddedLength; offset += 4) {
    self.words.push(view.getUint32(offset, true))
  }
}

/** @internal */
const abbreviationWidth = (count: number): number => Math.ceil(Math.log2(4 + count))

/** @internal */
export const enterBlock = (
  self: Writer,
  block: Block,
  parentAbbrevWidth = 2,
  defineAbbreviations = true,
): BlockWriter => {
  writeBits(self, 1, parentAbbrevWidth)
  writeVbr(self, block.id, 8)
  const abbrevWidth = abbreviationWidth(block.abbreviations.length)
  writeVbr(self, abbrevWidth, 4)
  alignTo32(self)
  const sizeWordIndex = self.words.length
  writeBits(self, 0, 32)
  const result = { writer: self, block, abbrevWidth, sizeWordIndex }
  if (defineAbbreviations) {
    for (const abbreviation of block.abbreviations) defineAbbreviation(result, abbreviation)
  }
  return result
}

/** @internal */
export const endBlock = (self: BlockWriter): void => {
  writeBits(self.writer, 0, self.abbrevWidth)
  alignTo32(self.writer)
  self.writer.words[self.sizeWordIndex] = self.writer.words.length - self.sizeWordIndex - 1
}

/** @internal */
const encoding = (op: Exclude<AbbrevOp, { readonly _tag: 'Literal' }>): number => {
  switch (op._tag) {
    case 'Fixed':
      return 1
    case 'Vbr':
      return 2
    case 'Array':
      return 3
    case 'Char6':
      return 4
    case 'Blob':
      return 5
  }
}

/** @internal */
export const defineAbbreviation = (self: BlockWriter, abbreviation: Abbreviation): void => {
  writeBits(self.writer, 2, self.abbrevWidth)
  writeVbr(self.writer, abbreviation.ops.length, 5)
  for (const op of abbreviation.ops) {
    if (op._tag === 'Literal') {
      writeBits(self.writer, 1, 1)
      writeVbr(self.writer, op.value, 8)
      continue
    }
    writeBits(self.writer, 0, 1)
    writeBits(self.writer, encoding(op), 3)
    if (op._tag === 'Fixed' || op._tag === 'Vbr') writeVbr(self.writer, op.width, 5)
  }
}

/** @internal */
const scalar = (value: RecordValue | undefined, operation: string): Scalar => {
  if (typeof value === 'number' || typeof value === 'bigint') return value
  throw failure(operation, 'Expected a scalar record operand', value)
}

/** @internal */
const array = (value: RecordValue | undefined, operation: string): ReadonlyArray<Scalar> => {
  if (value !== undefined && typeof value !== 'number' && typeof value !== 'bigint') return value
  throw failure(operation, 'Expected an array record operand', value)
}

/** @internal */
const writeOperand = (self: Writer, op: AbbrevOp, value: Scalar): void => {
  switch (op._tag) {
    case 'Fixed':
      width(op.width, 'Bitstream.writeRecord', 1)
      writeBits(self, value, op.width)
      return
    case 'Vbr':
      writeVbr(self, value, op.width)
      return
    case 'Char6': {
      const code = integer(value, 'Bitstream.writeRecord')
      if (code > 0x7fn) throw failure('Bitstream.writeRecord', 'Expected an ASCII code', code)
      write6BitChar(self, String.fromCharCode(Number(code)))
      return
    }
    case 'Literal':
    case 'Array':
    case 'Blob':
      throw failure('Bitstream.writeRecord', 'Invalid scalar abbreviation operand', op)
  }
}

/** @internal */
export const writeRecord = (
  self: BlockWriter,
  abbreviation: Abbreviation,
  values: ReadonlyArray<RecordValue>,
): void => {
  const abbreviationIndex = self.block.abbreviations.indexOf(abbreviation)
  if (abbreviationIndex < 0) {
    throw failure(
      'Bitstream.writeRecord',
      'Abbreviation is not defined by this block',
      abbreviation,
    )
  }
  writeBits(self.writer, 4 + abbreviationIndex, self.abbrevWidth)
  let valueIndex = 0

  for (let opIndex = 0; opIndex < abbreviation.ops.length; opIndex += 1) {
    const op = abbreviation.ops[opIndex]
    if (op === undefined) continue
    if (op._tag === 'Literal') continue
    if (op._tag === 'Array') {
      const elements = array(values[valueIndex], 'Bitstream.writeRecord')
      const elementOp = abbreviation.ops[opIndex + 1]
      if (
        elementOp === undefined ||
        elementOp._tag === 'Literal' ||
        elementOp._tag === 'Array' ||
        elementOp._tag === 'Blob'
      ) {
        throw failure(
          'Bitstream.writeRecord',
          'Array must be followed by a scalar encoding',
          elementOp,
        )
      }
      writeVbr(self.writer, elements.length, 6)
      for (const element of elements) writeOperand(self.writer, elementOp, element)
      valueIndex += 1
      opIndex += 1
      continue
    }
    if (op._tag === 'Blob') {
      const blob = array(values[valueIndex], 'Bitstream.writeRecord')
      writeVbr(self.writer, blob.length, 6)
      writeBlob(self.writer, blob)
      valueIndex += 1
      continue
    }
    writeOperand(self.writer, op, scalar(values[valueIndex], 'Bitstream.writeRecord'))
    valueIndex += 1
  }

  if (valueIndex !== values.length) {
    throw failure('Bitstream.writeRecord', 'Record has unused operands', values)
  }
}

/** @internal */
export const writeUnabbreviatedRecord = (
  self: BlockWriter,
  code: Scalar,
  values: ReadonlyArray<Scalar>,
): void => {
  writeBits(self.writer, 3, self.abbrevWidth)
  writeVbr(self.writer, code, 6)
  writeVbr(self.writer, values.length, 6)
  for (const value of values) writeVbr(self.writer, value, 6)
}

/** @internal */
export const toUint8Array = (self: Writer): Uint8Array => {
  if (self.bitCount !== 0) {
    throw failure(
      'Bitstream.toUint8Array',
      'Bitstream must be aligned before emission',
      self.bitCount,
    )
  }
  const bytes = new Uint8Array(self.words.length * 4)
  const view = new DataView(bytes.buffer)
  for (let index = 0; index < self.words.length; index += 1) {
    view.setUint32(index * 4, self.words[index] ?? 0, true)
  }
  return bytes
}
