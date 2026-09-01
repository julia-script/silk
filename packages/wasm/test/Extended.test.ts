import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Binary from '../src/Binary.js'
import * as Builder from '../src/Builder.js'
import * as DataActor from '../src/Data.js'
import * as ExportActor from '../src/Export.js'
import * as Func from '../src/Func.js'
import * as Import from '../src/Import.js'
import * as Instr from '../src/Instr.js'
import * as Memory from '../src/Memory.js'
import * as Table from '../src/Table.js'
import * as Type from '../src/Type.js'
import * as ValType from '../src/ValType.js'
import type { WasmError } from '../src/WasmError.js'
import { mixedMemoryCopy, requiring } from './support/hostCapability.js'

const failure = <A>(effect: Effect.Effect<A, WasmError>) =>
  effect.pipe(
    Effect.as(undefined),
    Effect.catchTag('WasmError', (error) => Effect.succeed(error)),
  )

const bodyOf = (
  build: (
    builder: Builder.Builder,
    memory: Memory.Memory,
  ) => Effect.Effect<ReadonlyArray<Instr.Instr>, WasmError>,
) =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const memory = yield* Memory.make(builder, { min: 1 })
    const type = yield* Type.func(builder, [], [])
    const func = yield* Func.declare(builder, type)
    const body = yield* build(builder, memory)
    yield* Func.define(builder, func, { body })
    return builder
  })

it.effect('validates and runs SIMD lane operations', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [ValType.i32], [ValType.i32])
    const func = yield* Func.declare(builder, type, { name: 'sumOfSplat' })
    yield* Func.define(builder, func, {
      body: [
        Instr.localGet(0),
        Instr.op('i32x4.splat'),
        Instr.localGet(0),
        Instr.simdLane('i32x4.replace_lane', 3),
        Instr.op('i16x8.abs'),
        Instr.simdLane('i32x4.extract_lane', 0),
      ],
    })
    yield* ExportActor.func(builder, 'sumOfSplat', func)
    const bytes = yield* Binary.encode(builder)
    assert.isTrue(WebAssembly.validate(bytes))
    const instance = yield* Effect.promise(() =>
      WebAssembly.instantiate(bytes.buffer as ArrayBuffer, {}),
    )
    const exports = instance.instance.exports as { sumOfSplat: (value: number) => number }
    assert.strictEqual(exports.sumOfSplat(7), 7)
  }),
)

it.effect('rejects out-of-range lane and shuffle immediates', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [], [])
    const func = yield* Func.declare(builder, type)
    const badLane = yield* failure(
      Func.define(builder, func, {
        body: [
          Instr.v128Const(new Uint8Array(16)),
          Instr.simdLane('f32x4.extract_lane', 4),
          Instr.op('drop'),
        ],
      }),
    )
    assert.strictEqual(badLane?.reason._tag, 'ValidationFailed')
    const badShuffle = yield* failure(
      Func.define(builder, func, {
        body: [
          Instr.v128Const(new Uint8Array(16)),
          Instr.v128Const(new Uint8Array(16)),
          Instr.i8x16Shuffle([...Array.from({ length: 15 }, () => 0), 32]),
          Instr.op('drop'),
        ],
      }),
    )
    assert.strictEqual(badShuffle?.reason._tag, 'ValidationFailed')
    const badConst = yield* failure(
      Func.define(builder, func, { body: [Instr.v128Const(new Uint8Array(15)), Instr.op('drop')] }),
    )
    assert.strictEqual(badConst?.reason._tag, 'ValidationFailed')
  }),
)

it.effect('requires exact natural alignment for atomic accesses', () =>
  Effect.gen(function* () {
    const under = yield* failure(
      bodyOf((_, memory) =>
        Effect.succeed([
          Instr.i32Const(0),
          Instr.atomicAccess('i32.atomic.load', memory, { align: 1 }),
          Instr.op('drop'),
        ]),
      ).pipe(Effect.asVoid),
    )
    assert.strictEqual(under?.reason._tag, 'ValidationFailed')
    // Atomics on an unshared memory are valid.
    yield* bodyOf((_, memory) =>
      Effect.succeed([
        Instr.i32Const(0),
        Instr.i32Const(1),
        Instr.atomicAccess('i32.atomic.rmw.add', memory),
        Instr.op('drop'),
        Instr.op('atomic.fence'),
      ]),
    )
  }),
)

it.effect('types atomic wait and notify', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const memory = yield* Memory.make(builder, { min: 1, max: 1 }, { shared: true })
    const type = yield* Type.func(builder, [], [ValType.i32])
    const func = yield* Func.declare(builder, type)
    yield* Func.define(builder, func, {
      body: [
        Instr.i32Const(0),
        Instr.i32Const(1),
        Instr.i64Const(-1n),
        Instr.atomicAccess('memory.atomic.wait32', memory),
        Instr.i32Const(0),
        Instr.i32Const(1),
        Instr.atomicAccess('memory.atomic.notify', memory),
        Instr.op('i32.add'),
      ],
    })
    const bytes = yield* Binary.encode(builder)
    assert.isTrue(WebAssembly.validate(bytes))
  }),
)

it.effect('rejects a shared memory without a maximum', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const error = yield* failure(Memory.make(builder, { min: 1 }, { shared: true }))
    assert.strictEqual(error?.reason._tag, 'InvalidInput')
    const importError = yield* failure(
      Import.memory(builder, 'env', 'm', { min: 1 }, { shared: true }),
    )
    assert.strictEqual(importError?.reason._tag, 'InvalidInput')
  }),
)

/**
 * An i64-address memory beside an i32-address one, an i32 index the i64 memory must reject, and a
 * body that copies between the two memories, loads above 2^32, and reads the wide memory's size.
 *
 * Encoding it is one test and asking the host to validate it is another, because only the second
 * depends on the host implementing mixed-address `memory.copy`.
 */
const addressThreadedModule = Effect.fnUntraced(function* () {
  const builder = yield* Builder.make()
  const wide = yield* Memory.make(builder, { min: 1 }, { addressType: 'i64', name: 'wide' })
  const narrow = yield* Memory.make(builder, { min: 1 }, { name: 'narrow' })
  const type = yield* Type.func(builder, [], [ValType.i64])
  const func = yield* Func.declare(builder, type, { name: 'probe' })
  const wrongAddress = yield* failure(
    Func.define(builder, func, {
      body: [Instr.i32Const(0), Instr.memoryAccess('i64.load', wide)],
    }),
  )
  yield* Func.define(builder, func, {
    body: [
      Instr.i64Const(8n),
      Instr.i32Const(0),
      Instr.i32Const(4),
      // Mixed 64/32-bit copy takes an i32 count.
      Instr.memoryCopy(wide, narrow),
      Instr.i64Const(0n),
      Instr.memoryAccess('i64.load', wide, { offset: 2n ** 32n }),
      Instr.memorySize(wide),
      Instr.op('i64.add'),
    ],
  })
  return { wrongAddress, bytes: yield* Binary.encode(builder) }
})

const hostValidatesAddressThreaded = requiring(
  mixedMemoryCopy,
  'the host validates a module threading address types through memory instructions',
)

it.effect('threads address types through memory instructions', () =>
  Effect.gen(function* () {
    const { wrongAddress, bytes } = yield* addressThreadedModule()
    assert.strictEqual(wrongAddress?.reason._tag, 'ValidationFailed')
    assert.isDefined(bytes)
  }),
)

it.effect.skipIf(hostValidatesAddressThreaded.skip)(hostValidatesAddressThreaded.name, () =>
  Effect.gen(function* () {
    if (hostValidatesAddressThreaded.failure !== undefined)
      return assert.fail(hostValidatesAddressThreaded.failure)
    const { bytes } = yield* addressThreadedModule()
    assert.isTrue(WebAssembly.validate(bytes))
  }),
)

it.effect('validates 64-bit tables and segment offsets', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const table = yield* Table.make(builder, ValType.funcref, { min: 1 }, { addressType: 'i64' })
    const memory = yield* Memory.make(builder, { min: 1 }, { addressType: 'i64' })
    yield* DataActor.active(builder, memory, [Instr.i64Const(8n)], new Uint8Array([1]))
    const wrongOffset = yield* failure(
      DataActor.active(builder, memory, [Instr.i32Const(8)], new Uint8Array([1])),
    )
    assert.strictEqual(wrongOffset?.reason._tag, 'ValidationFailed')
    const type = yield* Type.func(builder, [], [ValType.funcref])
    const func = yield* Func.declare(builder, type)
    yield* Func.define(builder, func, {
      body: [Instr.i64Const(0n), Instr.tableGet(table)],
    })
    const bytes = yield* Binary.encode(builder)
    assert.isDefined(bytes)
  }),
)

it.effect('rejects oversized 64-bit limits and accepts large ones', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const big = yield* Memory.make(builder, { min: 2n ** 20n }, { addressType: 'i64' })
    assert.isDefined(big)
    const tooBig = yield* failure(
      Memory.make(builder, { min: 2n ** 48n + 1n }, { addressType: 'i64' }),
    )
    assert.strictEqual(tooBig?.reason._tag, 'InvalidInput')
    const narrowTooBig = yield* failure(Memory.make(builder, { min: 65537 }))
    assert.strictEqual(narrowTooBig?.reason._tag, 'InvalidInput')
  }),
)
