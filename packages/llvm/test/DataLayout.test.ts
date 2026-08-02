import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import { expect, test } from 'vitest'
import * as AddrSpace from '../src/AddrSpace.js'
import * as Alignment from '../src/Alignment.js'
import * as Builder from '../src/Builder.js'
import * as DataLayout from '../src/DataLayout.js'

const TestRuntime = ManagedRuntime.make(Layer.empty)

test(
  'parses explicit primitive, pointer, native-width, stack, and non-integral fields',
  Effect.fnUntraced(function* () {
    const layout = yield* DataLayout.parse(
      'e-m:e-p:64:64:128:32-p1:32:32-i32:32:64-f64:64-v128:128-n8:16:32:64-S128-ni:1:3',
    )
    const addressOne = yield* AddrSpace.make(1)

    expect(layout.endian).toBe('little')
    expect(DataLayout.integerSpec(layout, 32)?.preferredAlignment.byteUnits).toBe(8n)
    expect(DataLayout.floatSpec(layout, 64)?.abiAlignment.byteUnits).toBe(8n)
    expect(DataLayout.vectorSpec(layout, 128)?.abiAlignment.byteUnits).toBe(16n)
    expect(DataLayout.pointerSpec(layout, addressOne)?.indexBitWidth).toBe(32)
    expect(layout.nativeIntegerWidths).toEqual([8, 16, 32, 64])
    expect(layout.stackAlignment.byteUnits).toBe(16n)
    expect(layout.nonIntegralAddressSpaces.map((address) => address.value)).toEqual([1, 3])
  }, TestRuntime.runPromise),
)

test(
  'rejects malformed layouts through SilkError, including Builder.make',
  Effect.fnUntraced(function* () {
    const parseError = yield* Effect.flip(DataLayout.parse('e-i32:24'))
    const builderError = yield* Effect.flip(Builder.make({ dataLayout: 'e-unknown' }))

    expect(parseError._tag).toBe('SilkError')
    expect(parseError.operation).toBe('DataLayout.parse')
    expect(builderError.operation).toBe('DataLayout.parse')
  }, TestRuntime.runPromise),
)

test(
  'models alignments and address spaces without lossy numbers',
  Effect.fnUntraced(function* () {
    const alignment = yield* Alignment.fromByteUnits(16n)
    const address = yield* AddrSpace.make(42)
    expect(Alignment.toByteUnits(alignment)).toBe(16n)
    expect(Alignment.compare(Alignment.defaultAlignment, alignment)).toBe(-1)
    expect(AddrSpace.render(address)).toBe('addrspace(42)')
  }, TestRuntime.runPromise),
)
