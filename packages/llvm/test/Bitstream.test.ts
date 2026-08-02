import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import { expect, test } from 'vitest'
import * as Bitstream from '../src/internal/Bitstream.js'

const TestRuntime = ManagedRuntime.make(Layer.empty)

test(
  'packs crossing bit writes into little-endian words',
  Effect.fnUntraced(function* () {
    yield* Effect.succeed(undefined)
    const writer = Bitstream.make()
    Bitstream.writeBits(writer, 0x3fff_ffff, 30)
    Bitstream.writeBits(writer, 0b10101, 5)
    Bitstream.alignTo32(writer)

    expect(Bitstream.toUint8Array(writer)).toEqual(
      Uint8Array.of(0xff, 0xff, 0xff, 0x7f, 0x05, 0x00, 0x00, 0x00),
    )
  }, TestRuntime.runPromise),
)

test(
  'writes VBR values with exact bigint semantics',
  Effect.fnUntraced(function* () {
    yield* Effect.succeed(undefined)
    const writer = Bitstream.make()
    Bitstream.writeVbr(writer, 63n, 6)
    Bitstream.writeVbr(writer, 0xffff_ffff_ffff_ffffn, 8)
    Bitstream.alignTo32(writer)

    expect(Bitstream.bitsVbr(63n, 6)).toBe(12)
    expect(Bitstream.toUint8Array(writer)).toEqual(
      Uint8Array.of(0x7f, 0xf0, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x1f, 0x00),
    )
  }, TestRuntime.runPromise),
)

test(
  'rejects characters outside LLVM char6',
  Effect.fnUntraced(function* () {
    yield* Effect.succeed(undefined)
    expect(() => Bitstream.char6('-')).toThrow('Character is not representable')
    expect(Bitstream.char6('a')).toBe(0)
    expect(Bitstream.char6('_')).toBe(63)
  }, TestRuntime.runPromise),
)

test(
  'aligns and pads blobs to little-endian words',
  Effect.fnUntraced(function* () {
    yield* Effect.succeed(undefined)
    const writer = Bitstream.make()
    Bitstream.writeBits(writer, 1, 1)
    Bitstream.writeBlob(writer, [0x11, 0x22, 0x33, 0x44, 0x55])

    expect(Bitstream.toUint8Array(writer)).toEqual(
      Uint8Array.of(0x01, 0x00, 0x00, 0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x00, 0x00, 0x00),
    )
  }, TestRuntime.runPromise),
)

test(
  'encodes a blob beyond the 16-bit length boundary without truncation',
  Effect.fnUntraced(function* () {
    yield* Effect.succeed(undefined)
    const writer = Bitstream.make()
    const blob = Uint8Array.from({ length: 65_537 }, (_, index) => index & 0xff)
    Bitstream.writeBlob(writer, blob)
    const bytes = Bitstream.toUint8Array(writer)

    expect(bytes).toHaveLength(65_540)
    expect(bytes.slice(0, 4)).toEqual(Uint8Array.of(0, 1, 2, 3))
    expect(bytes.slice(65_536)).toEqual(Uint8Array.of(0, 0, 0, 0))
  }, TestRuntime.runPromise),
)

test(
  'defines abbreviations, writes records, and backpatches block lengths',
  Effect.fnUntraced(function* () {
    yield* Effect.succeed(undefined)
    const record: Bitstream.Abbreviation = {
      name: 'TEST_RECORD',
      ops: [
        { _tag: 'Literal', value: 7n },
        { _tag: 'Vbr', width: 6 },
        { _tag: 'Array' },
        { _tag: 'Fixed', width: 8 },
      ],
    }
    const block: Bitstream.Block = { id: 8, abbreviations: [record] }
    const writer = Bitstream.make()
    const entered = Bitstream.enterBlock(writer, block)
    Bitstream.writeRecord(entered, record, [42n, [0x61, 0x62]])
    Bitstream.writeUnabbreviatedRecord(entered, 9, [1, 2, 3])
    Bitstream.endBlock(entered)

    const bytes = Bitstream.toUint8Array(writer)
    expect(bytes.slice(0, 8)).toEqual(Uint8Array.of(0x21, 0x0c, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00))
    expect(bytes.length % 4).toBe(0)
  }, TestRuntime.runPromise),
)
