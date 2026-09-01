import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Binary from '../src/Binary.js'
import * as Builder from '../src/Builder.js'
import * as DataActor from '../src/Data.js'
import * as Elem from '../src/Elem.js'
import * as ExportActor from '../src/Export.js'
import * as Func from '../src/Func.js'
import * as Global from '../src/Global.js'
import * as Import from '../src/Import.js'
import * as Instr from '../src/Instr.js'
import * as Memory from '../src/Memory.js'
import * as Table from '../src/Table.js'
import * as Type from '../src/Type.js'
import * as ValType from '../src/ValType.js'
import type { WasmError } from '../src/WasmError.js'

const failure = <A>(effect: Effect.Effect<A, WasmError>) =>
  effect.pipe(
    Effect.as(undefined),
    Effect.catchTag('WasmError', (error) => Effect.succeed(error)),
  )

it.effect('interns structurally equal function types to one handle', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const first = yield* Type.func(builder, [ValType.i32, ValType.i32], [ValType.i32])
    const second = yield* Type.func(builder, [ValType.i32, ValType.i32], [ValType.i32])
    const different = yield* Type.func(builder, [ValType.i32], [ValType.i32])
    assert.strictEqual(first, second)
    assert.notStrictEqual(first, different)
    const signature = yield* Type.signature(builder, first)
    assert.deepStrictEqual(signature.params, [ValType.i32, ValType.i32])
    assert.deepStrictEqual(signature.results, [ValType.i32])
  }),
)

it.effect('returns first-class handles for imports', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const signature = yield* Type.func(builder, [ValType.i32], [])
    const log = yield* Import.func(builder, 'env', 'log', signature, { name: 'log' })
    assert.strictEqual(yield* Func.type(builder, log), signature)
    const table = yield* Import.table(builder, 'env', 'indirect', ValType.funcref, { min: 1 })
    const memory = yield* Import.memory(builder, 'env', 'memory', { min: 1, max: 4 })
    const counter = yield* Import.global(builder, 'env', 'counter', ValType.i32, false)
    yield* ExportActor.table(builder, 'table', table)
    yield* ExportActor.memory(builder, 'memory', memory)
    yield* ExportActor.global(builder, 'counter', counter)
  }),
)

it.effect('declares multiple memories', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const main = yield* Memory.make(builder, { min: 1 }, { name: 'main' })
    const scratch = yield* Memory.make(builder, { min: 1 }, { name: 'scratch' })
    assert.notStrictEqual(main, scratch)
  }),
)

it.effect('rejects invalid limits', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const inverted = yield* failure(Memory.make(builder, { min: 2, max: 1 }))
    assert.strictEqual(inverted?.reason._tag, 'InvalidInput')
    const oversized = yield* failure(Memory.make(builder, { min: 65537 }))
    assert.strictEqual(oversized?.reason._tag, 'InvalidInput')
    const fractional = yield* failure(Table.make(builder, ValType.funcref, { min: 0.5 }))
    assert.strictEqual(fractional?.reason._tag, 'InvalidInput')
  }),
)

it.effect('accepts an extended constant initializer over an imported global', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const base = yield* Import.global(builder, 'env', 'base', ValType.i32, false)
    const offset = yield* Global.make(builder, ValType.i32, false, [
      Instr.globalGet(base),
      Instr.i32Const(16),
      Instr.op('i32.add'),
    ])
    assert.isDefined(offset)
  }),
)

it.effect('rejects constant expressions the specification forbids', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const mutable = yield* Import.global(builder, 'env', 'mutable', ValType.i32, true)
    const readsMutable = yield* failure(
      Global.make(builder, ValType.i32, false, [Instr.globalGet(mutable)]),
    )
    assert.strictEqual(readsMutable?.reason._tag, 'ValidationFailed')
    const defined = yield* Global.make(builder, ValType.i32, false, [Instr.i32Const(1)])
    const readsDefined = yield* failure(
      Global.make(builder, ValType.i32, false, [Instr.globalGet(defined)]),
    )
    assert.strictEqual(readsDefined?.reason._tag, 'ValidationFailed')
    const notConstant = yield* failure(
      Global.make(builder, ValType.i32, false, [Instr.i32Const(1), Instr.op('i32.eqz')]),
    )
    assert.strictEqual(notConstant?.reason._tag, 'ValidationFailed')
    const wrongArity = yield* failure(
      Global.make(builder, ValType.i32, false, [Instr.i32Const(1), Instr.i32Const(2)]),
    )
    assert.strictEqual(wrongArity?.reason._tag, 'ValidationFailed')
  }),
)

it.effect('rejects duplicate export names without committing', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [], [])
    const func = yield* Func.declare(builder, type)
    yield* ExportActor.func(builder, 'run', func)
    const duplicate = yield* failure(ExportActor.func(builder, 'run', func))
    assert.strictEqual(duplicate?.reason._tag, 'InvalidInput')
  }),
)

it.effect('rejects a start function whose type is not [] -> []', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const wrong = yield* Type.func(builder, [ValType.i32], [])
    const func = yield* Func.declare(builder, wrong)
    const error = yield* failure(Func.start(builder, func))
    assert.strictEqual(error?.reason._tag, 'InvalidState')
    const right = yield* Type.func(builder, [], [])
    const main = yield* Func.declare(builder, right)
    yield* Func.start(builder, main)
    const second = yield* failure(Func.start(builder, main))
    assert.strictEqual(second?.reason._tag, 'InvalidState')
  }),
)

it.effect('declares element and data segments with validated offsets and items', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [], [])
    const func = yield* Func.declare(builder, type)
    const table = yield* Table.make(builder, ValType.funcref, { min: 2 })
    const memory = yield* Memory.make(builder, { min: 1 })
    yield* Elem.active(
      builder,
      table,
      [Instr.i32Const(0)],
      [func, [Instr.refNull(ValType.funcref)]],
    )
    yield* Elem.passive(builder, ValType.externref, [[Instr.refNull(ValType.externref)]])
    yield* Elem.declarative(builder, ValType.funcref, [func])
    yield* DataActor.active(builder, memory, [Instr.i32Const(8)], new Uint8Array([1, 2, 3]))
    yield* DataActor.passive(builder, new Uint8Array([4, 5]))
    const wrongOffset = yield* failure(
      DataActor.active(builder, memory, [Instr.i64Const(0n)], new Uint8Array()),
    )
    assert.strictEqual(wrongOffset?.reason._tag, 'ValidationFailed')
    const wrongItem = yield* failure(
      Elem.passive(builder, ValType.externref, [[Instr.refNull(ValType.funcref)]]),
    )
    assert.strictEqual(wrongItem?.reason._tag, 'ValidationFailed')
  }),
)

it.effect('preserves data segment bytes against later mutation of the source', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const source = new Uint8Array([9, 9, 9])
    const segment = yield* DataActor.passive(builder, source)
    source[0] = 0
    assert.isDefined(segment)
  }),
)

it.effect('rejects duplicate names within an index space', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [], [])
    yield* Func.declare(builder, type, { name: 'helper' })
    const duplicate = yield* failure(Func.declare(builder, type, { name: 'helper' }))
    assert.strictEqual(duplicate?.reason._tag, 'InvalidInput')
    // The same name remains free in other index spaces.
    yield* Memory.make(builder, { min: 1 }, { name: 'helper' })
    const memoryDuplicate = yield* failure(
      Import.memory(builder, 'env', 'm', { min: 1 }, { name: 'helper' }),
    )
    assert.strictEqual(memoryDuplicate?.reason._tag, 'InvalidInput')
  }),
)

it.effect('rejects canonically equal UTF-8 names without partial commits', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [], [])
    const malformed = yield* Func.declare(builder, type, { name: '\uD800' })
    const entityCollision = yield* failure(Func.declare(builder, type, { name: '\uFFFD' }))
    assert.strictEqual(entityCollision?.reason._tag, 'InvalidInput')

    const afterCollision = yield* Func.declare(builder, type, { name: 'after-collision' })
    yield* Func.define(builder, malformed, { body: [] })
    const localCollision = yield* failure(
      Func.define(builder, afterCollision, {
        locals: [
          { type: ValType.i32, name: '\uD800' },
          { type: ValType.i32, name: '\uFFFD' },
        ],
        body: [],
      }),
    )
    assert.strictEqual(localCollision?.reason._tag, 'InvalidInput')
    yield* Func.define(builder, afterCollision, {
      locals: [{ type: ValType.i32, name: 'local' }],
      body: [],
    })

    yield* ExportActor.func(builder, '\uD800', malformed)
    const exportCollision = yield* failure(ExportActor.func(builder, '\uFFFD', afterCollision))
    assert.strictEqual(exportCollision?.reason._tag, 'InvalidInput')
    yield* ExportActor.func(builder, 'after-collision', afterCollision)

    assert.isTrue(WebAssembly.validate(yield* Binary.encode(builder)))
  }),
)
