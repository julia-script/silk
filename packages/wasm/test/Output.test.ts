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
import * as WatText from '../src/WatText.js'

const failure = <A>(effect: Effect.Effect<A, WasmError>) =>
  effect.pipe(
    Effect.map(() => undefined),
    Effect.catchTag('WasmError', (error) => Effect.succeed(error)),
  )

/** Builds a representative module exercising most baseline features. */
const representative = Effect.gen(function* () {
  const builder = yield* Builder.make({ moduleName: 'representative' })
  const logType = yield* Type.func(builder, [ValType.i32], [])
  const log = yield* Import.func(builder, 'env', 'log', logType, { name: 'log' })
  const base = yield* Import.global(builder, 'env', 'base', ValType.i32, false, { name: 'base' })
  const addType = yield* Type.func(builder, [ValType.i32, ValType.i32], [ValType.i32])
  const add = yield* Func.declare(builder, addType, { name: 'add' })
  yield* Func.define(builder, add, {
    body: [Instr.localGet(0), Instr.localGet(1), Instr.op('i32.add')],
  })
  const mainType = yield* Type.func(builder, [], [])
  const main = yield* Func.declare(builder, mainType, { name: 'main' })
  const counter = yield* Global.make(
    builder,
    ValType.i32,
    true,
    [Instr.globalGet(base), Instr.i32Const(1), Instr.op('i32.add')],
    { name: 'counter' },
  )
  const memory = yield* Memory.make(builder, { min: 1, max: 4 }, { name: 'memory' })
  const table = yield* Table.make(builder, ValType.funcref, { min: 2 }, { name: 'indirect' })
  yield* Func.define(builder, main, {
    locals: [{ type: ValType.i32, name: 'sum' }],
    body: [
      Instr.i32Const(20),
      Instr.i32Const(22),
      Instr.call(add),
      Instr.localTee(0),
      Instr.globalSet(counter),
      Instr.block(Instr.emptyBlockType, [
        Instr.localGet(0),
        Instr.i32Const(42),
        Instr.op('i32.eq'),
        Instr.brIf(0),
        Instr.i32Const(0),
        Instr.call(log),
      ]),
      Instr.i32Const(1),
      Instr.i32Const(2),
      Instr.i32Const(0),
      Instr.callIndirect(table, addType),
      Instr.op('drop'),
    ],
  })
  yield* Elem.active(builder, table, [Instr.i32Const(0)], [add, add])
  yield* DataActor.active(builder, memory, [Instr.i32Const(8)], new Uint8Array([1, 2, 3, 4]))
  yield* ExportActor.func(builder, 'add', add)
  yield* ExportActor.func(builder, 'main', main)
  yield* ExportActor.memory(builder, 'memory', memory)
  yield* ExportActor.global(builder, 'counter', counter)
  return builder
})

it.effect('encodes a module Node’s own WebAssembly runtime accepts', () =>
  Effect.gen(function* () {
    const builder = yield* representative
    const bytes = yield* Binary.encode(builder)
    assert.deepStrictEqual([...bytes.slice(0, 8)], [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00])
    const valid = WebAssembly.validate(bytes)
    assert.isTrue(valid)
    const module = yield* Effect.promise(() => WebAssembly.compile(bytes.buffer as ArrayBuffer))
    const logged: Array<number> = []
    const instance = yield* Effect.promise(() =>
      WebAssembly.instantiate(module, {
        env: {
          log: (value: number) => logged.push(value),
          base: new WebAssembly.Global({ value: 'i32' }, 41),
        },
      }),
    )
    const exports = instance.exports as {
      add: (left: number, right: number) => number
      main: () => void
      counter: WebAssembly.Global
    }
    assert.strictEqual(exports.add(20, 22), 42)
    exports.main()
    assert.strictEqual(exports.counter.value, 42)
    assert.deepStrictEqual(logged, [])
  }),
)

it.effect('renders text alongside the binary', () =>
  Effect.gen(function* () {
    const builder = yield* representative
    const text = yield* WatText.render(builder)
    assert.isTrue(text.startsWith('(module $representative\n'))
    assert.include(text, '(func $add (type 1)')
    assert.include(text, '(local $sum i32)')
    assert.include(text, '(export "add" (func $add))')
    assert.include(text, '(import "env" "log" (func $log (type 0)))')
    assert.isTrue(text.endsWith(')\n'))
  }),
)

it.effect('produces identical bytes and text for identical operation order', () =>
  Effect.gen(function* () {
    const first = yield* representative
    const second = yield* representative
    const firstBytes = yield* Binary.encode(first)
    const secondBytes = yield* Binary.encode(second)
    assert.deepStrictEqual([...firstBytes], [...secondBytes])
    const firstText = yield* WatText.render(first)
    const secondText = yield* WatText.render(second)
    assert.strictEqual(firstText, secondText)
    // Repeat emission over the same builder is also stable.
    const again = yield* Binary.encode(first)
    assert.deepStrictEqual([...firstBytes], [...again])
  }),
)

it.effect('declaration order of imports does not disturb committed references', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [], [ValType.i32])
    const callee = yield* Func.declare(builder, type, { name: 'callee' })
    yield* Func.define(builder, callee, { body: [Instr.i32Const(7)] })
    const caller = yield* Func.declare(builder, type, { name: 'caller' })
    yield* Func.define(builder, caller, { body: [Instr.call(callee)] })
    // The import is declared after both definitions but claims function index 0.
    yield* Import.func(builder, 'env', 'late', yield* Type.func(builder, [], []))
    yield* ExportActor.func(builder, 'caller', caller)
    const bytes = yield* Binary.encode(builder)
    const instance = yield* Effect.promise(() =>
      WebAssembly.instantiate(bytes.buffer as ArrayBuffer, { env: { late: () => undefined } }),
    )
    const exports = instance.instance.exports as { caller: () => number }
    assert.strictEqual(exports.caller(), 7)
  }),
)

it.effect('emits the name custom section only when names exist', () =>
  Effect.gen(function* () {
    const named = yield* Builder.make({ moduleName: 'named' })
    const namedBytes = yield* Binary.encode(named)
    const anonymous = yield* Builder.make()
    const anonymousBytes = yield* Binary.encode(anonymous)
    assert.isTrue(namedBytes.length > anonymousBytes.length)
    // An empty module is exactly the 8-byte header.
    assert.strictEqual(anonymousBytes.length, 8)
  }),
)

it.effect('never emits a module with an undefined declared function', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [], [])
    yield* Func.declare(builder, type, { name: 'ghost' })
    const binaryError = yield* failure(Binary.encode(builder))
    assert.strictEqual(binaryError?.reason._tag, 'ValidationFailed')
    const textError = yield* failure(WatText.render(builder))
    assert.strictEqual(textError?.reason._tag, 'ValidationFailed')
  }),
)

it.effect('never emits a module whose ref.func target is undeclared', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [], [])
    const target = yield* Func.declare(builder, type)
    yield* Func.define(builder, target, { body: [] })
    const user = yield* Func.declare(builder, type)
    yield* Func.define(builder, user, { body: [Instr.refFunc(target), Instr.op('drop')] })
    yield* ExportActor.func(builder, 'user', user)
    const error = yield* failure(Binary.encode(builder))
    assert.strictEqual(error?.reason._tag, 'ValidationFailed')
    // Declaring the target in a declarative segment makes the module emittable.
    yield* Elem.declarative(builder, ValType.funcref, [target])
    const bytes = yield* Binary.encode(builder)
    assert.isTrue(WebAssembly.validate(bytes))
  }),
)

it.effect('round-trips passive segments and bulk operations through a runtime', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const memory = yield* Memory.make(builder, { min: 1 })
    const segment = yield* DataActor.passive(builder, new Uint8Array([104, 105]))
    const type = yield* Type.func(builder, [], [ValType.i32])
    const func = yield* Func.declare(builder, type, { name: 'copyAndRead' })
    yield* Func.define(builder, func, {
      body: [
        Instr.i32Const(16),
        Instr.i32Const(0),
        Instr.i32Const(2),
        Instr.memoryInit(segment, memory),
        Instr.dataDrop(segment),
        Instr.i32Const(16),
        Instr.memoryAccess('i32.load8_u', memory),
      ],
    })
    yield* ExportActor.func(builder, 'copyAndRead', func)
    const bytes = yield* Binary.encode(builder)
    const instance = yield* Effect.promise(() =>
      WebAssembly.instantiate(bytes.buffer as ArrayBuffer, {}),
    )
    const exports = instance.instance.exports as { copyAndRead: () => number }
    assert.strictEqual(exports.copyAndRead(), 104)
  }),
)

it.effect('renders non-identifier names as quoted identifiers', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [], [])
    const func = yield* Func.declare(builder, type, { name: 'weird name!' })
    yield* Func.define(builder, func, { body: [] })
    yield* ExportActor.func(builder, 'f', func)
    const text = yield* WatText.render(builder)
    assert.include(text, '(func $"weird name!" (type 0)')
    const bytes = yield* Binary.encode(builder)
    assert.isTrue(WebAssembly.validate(bytes))
  }),
)
