import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import { expect, test } from 'vitest'
import * as ByteString from '../src/ByteString.js'

const TestRuntime = ManagedRuntime.make(Layer.empty)

test(
  'copies input bytes and compares by byte content',
  Effect.fnUntraced(function* () {
    yield* Effect.succeed(undefined)
    const input = Uint8Array.of(0x61, 0x62)
    const value = ByteString.fromUint8Array(input)
    input[0] = 0xff

    expect(ByteString.toUint8Array(value)).toEqual(Uint8Array.of(0x61, 0x62))
    expect(ByteString.equals(value, ByteString.fromString('ab'))).toBe(true)
    expect(ByteString.hash(value)).toBe(ByteString.hash(ByteString.fromString('ab')))
  }, TestRuntime.runPromise),
)

test(
  'encodes UTF-8 and applies LLVM byte escaping',
  Effect.fnUntraced(function* () {
    yield* Effect.succeed(undefined)
    const value = ByteString.fromString('a"\\\nλ')
    expect(ByteString.toUint8Array(value)).toEqual(
      Uint8Array.of(0x61, 0x22, 0x5c, 0x0a, 0xce, 0xbb),
    )
    expect(ByteString.escapeForIr(value)).toBe('a\\22\\5C\\0A\\CE\\BB')
  }, TestRuntime.runPromise),
)
