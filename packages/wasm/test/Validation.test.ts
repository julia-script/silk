import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Builder from '../src/Builder.js'
import * as Elem from '../src/Elem.js'
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

const defineWith = (
  params: ReadonlyArray<ValType.ValType>,
  results: ReadonlyArray<ValType.ValType>,
  definition: Func.Definition,
) =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, params, results)
    const func = yield* Func.declare(builder, type)
    yield* Func.define(builder, func, definition)
    return builder
  })

const rejects = (
  params: ReadonlyArray<ValType.ValType>,
  results: ReadonlyArray<ValType.ValType>,
  definition: Func.Definition,
) =>
  Effect.gen(function* () {
    const error = yield* failure(defineWith(params, results, definition).pipe(Effect.asVoid))
    assert.isDefined(error)
    assert.strictEqual(error.reason._tag, 'ValidationFailed')
    return error
  })

it.effect('accepts a well-typed add function', () =>
  defineWith([ValType.i32, ValType.i32], [ValType.i32], {
    body: [Instr.localGet(0), Instr.localGet(1), Instr.op('i32.add')],
  }).pipe(Effect.asVoid),
)

it.effect('rejects stack underflow', () =>
  rejects([], [ValType.i32], { body: [Instr.i32Const(1), Instr.op('i32.add')] }).pipe(
    Effect.asVoid,
  ),
)

it.effect('rejects operand type mismatches', () =>
  rejects([], [ValType.i32], {
    body: [Instr.i32Const(1), Instr.i64Const(2n), Instr.op('i32.add')],
  }).pipe(Effect.asVoid),
)

it.effect('rejects missing results', () =>
  rejects([], [ValType.i32], { body: [] }).pipe(Effect.asVoid),
)

it.effect('rejects extra values left on the stack', () =>
  rejects([], [], { body: [Instr.i32Const(1)] }).pipe(Effect.asVoid),
)

it.effect('rejects out-of-range locals', () =>
  rejects([ValType.i32], [], { body: [Instr.localGet(3), Instr.op('drop')] }).pipe(Effect.asVoid),
)

it.effect('accepts declared locals after parameters', () =>
  defineWith([ValType.i32], [ValType.f64], {
    locals: [{ type: ValType.f64, name: 'accumulator' }],
    body: [Instr.localGet(1)],
  }).pipe(Effect.asVoid),
)

it.effect('accepts polymorphic typing after unreachable', () =>
  defineWith([], [ValType.i32], {
    body: [Instr.op('unreachable'), Instr.op('i32.add')],
  }).pipe(Effect.asVoid),
)

it.effect('rejects type errors even after unreachable', () =>
  rejects([], [ValType.i32], {
    body: [Instr.op('unreachable'), Instr.i64Const(1n), Instr.op('i32.add')],
  }).pipe(Effect.asVoid),
)

it.effect('checks structured control flow and branch depths', () =>
  Effect.gen(function* () {
    // A loop nested in a block; br 1 targets the block, br 0 the loop.
    yield* defineWith([ValType.i32], [ValType.i32], {
      body: [
        Instr.block(Instr.valueBlockType(ValType.i32), [
          Instr.loop(Instr.emptyBlockType, [
            Instr.localGet(0),
            Instr.brIf(0),
            Instr.i32Const(7),
            Instr.br(1),
          ]),
          Instr.i32Const(1),
        ]),
      ],
    })
    const tooDeep = yield* rejects([], [], { body: [Instr.br(1)] })
    assert.isDefined(tooDeep)
  }),
)

it.effect('rejects an if whose implicit else cannot produce the result', () =>
  rejects([ValType.i32], [ValType.i32], {
    body: [Instr.localGet(0), Instr.ifElse(Instr.valueBlockType(ValType.i32), [Instr.i32Const(1)])],
  }).pipe(Effect.asVoid),
)

it.effect('checks br_table label arity agreement', () =>
  rejects([ValType.i32], [ValType.i32], {
    body: [
      Instr.block(Instr.valueBlockType(ValType.i32), [
        Instr.block(Instr.emptyBlockType, [
          Instr.i32Const(0),
          Instr.localGet(0),
          Instr.brTable([0], 1),
        ]),
        Instr.i32Const(2),
      ]),
    ],
  }).pipe(Effect.asVoid),
)

it.effect('checks calls against their function types', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const calleeType = yield* Type.func(builder, [ValType.i32], [ValType.i64])
    const callee = yield* Func.declare(builder, calleeType)
    const callerType = yield* Type.func(builder, [], [ValType.i64])
    const caller = yield* Func.declare(builder, callerType)
    const missingArgument = yield* failure(
      Func.define(builder, caller, { body: [Instr.call(callee)] }),
    )
    assert.strictEqual(missingArgument?.reason._tag, 'ValidationFailed')
    yield* Func.define(builder, caller, { body: [Instr.i32Const(1), Instr.call(callee)] })
  }),
)

it.effect('requires tail calls to match the caller result types', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const calleeType = yield* Type.func(builder, [], [ValType.i64])
    const callee = yield* Func.declare(builder, calleeType)
    const callerType = yield* Type.func(builder, [], [ValType.i32])
    const caller = yield* Func.declare(builder, callerType)
    const error = yield* failure(Func.define(builder, caller, { body: [Instr.returnCall(callee)] }))
    assert.strictEqual(error?.reason._tag, 'ValidationFailed')
  }),
)

it.effect('checks call_indirect table and stack types', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const externTable = yield* Table.make(builder, ValType.externref, { min: 1 })
    const signature = yield* Type.func(builder, [], [])
    const type = yield* Type.func(builder, [], [])
    const func = yield* Func.declare(builder, type)
    const error = yield* failure(
      Func.define(builder, func, {
        body: [Instr.i32Const(0), Instr.callIndirect(externTable, signature)],
      }),
    )
    assert.strictEqual(error?.reason._tag, 'ValidationFailed')
  }),
)

it.effect('rejects writes to immutable globals', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const constant = yield* Global.make(builder, ValType.i32, false, [Instr.i32Const(1)])
    const type = yield* Type.func(builder, [], [])
    const func = yield* Func.declare(builder, type)
    const error = yield* failure(
      Func.define(builder, func, { body: [Instr.i32Const(2), Instr.globalSet(constant)] }),
    )
    assert.strictEqual(error?.reason._tag, 'ValidationFailed')
  }),
)

it.effect('rejects over-aligned memory accesses', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const memory = yield* Memory.make(builder, { min: 1 })
    const type = yield* Type.func(builder, [], [ValType.i32])
    const func = yield* Func.declare(builder, type)
    const error = yield* failure(
      Func.define(builder, func, {
        body: [Instr.i32Const(0), Instr.memoryAccess('i32.load', memory, { align: 3 })],
      }),
    )
    assert.strictEqual(error?.reason._tag, 'ValidationFailed')
  }),
)

it.effect('rejects untyped select over references', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [], [])
    const func = yield* Func.declare(builder, type)
    const error = yield* failure(
      Func.define(builder, func, {
        body: [
          Instr.refNull(ValType.funcref),
          Instr.refNull(ValType.funcref),
          Instr.i32Const(1),
          Instr.op('select'),
          Instr.op('drop'),
        ],
      }),
    )
    assert.strictEqual(error?.reason._tag, 'ValidationFailed')
  }),
)

it.effect('checks table instruction reference types', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const funcTable = yield* Table.make(builder, ValType.funcref, { min: 1 })
    const externTable = yield* Table.make(builder, ValType.externref, { min: 1 })
    const type = yield* Type.func(builder, [], [])
    const func = yield* Func.declare(builder, type)
    const error = yield* failure(
      Func.define(builder, func, {
        body: [
          Instr.i32Const(0),
          Instr.i32Const(0),
          Instr.i32Const(1),
          Instr.tableCopy(funcTable, externTable),
        ],
      }),
    )
    assert.strictEqual(error?.reason._tag, 'ValidationFailed')
  }),
)

it.effect('leaves no state behind and allows retry after a failed definition', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [], [ValType.i32])
    const func = yield* Func.declare(builder, type)
    const error = yield* failure(Func.define(builder, func, { body: [] }))
    assert.strictEqual(error?.reason._tag, 'ValidationFailed')
    yield* Func.define(builder, func, { body: [Instr.i32Const(42)] })
    const again = yield* failure(Func.define(builder, func, { body: [Instr.i32Const(1)] }))
    assert.strictEqual(again?.reason._tag, 'InvalidState')
  }),
)

it.effect('records ref.func targets for emit-time declaration checking', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [], [])
    const target = yield* Func.declare(builder, type)
    const user = yield* Func.declare(builder, type)
    yield* Func.define(builder, user, {
      body: [Instr.refFunc(target), Instr.op('drop')],
    })
    yield* Elem.declarative(builder, ValType.funcref, [target])
    yield* Func.define(builder, target, { body: [] })
  }),
)

it.effect('rejects defining an imported function', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [], [])
    const imported = yield* Import.func(builder, 'env', 'noop', type)
    const error = yield* failure(Func.define(builder, imported, { body: [] }))
    assert.strictEqual(error?.reason._tag, 'InvalidState')
  }),
)
