import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Builder from '../src/Builder.js'
import * as Func from '../src/Func.js'
import * as Global from '../src/Global.js'
import * as Instr from '../src/Instr.js'
import * as Type from '../src/Type.js'
import * as ValType from '../src/ValType.js'
import type { WasmError } from '../src/WasmError.js'

const failure = <A>(effect: Effect.Effect<A, WasmError>) =>
  effect.pipe(
    Effect.map(() => undefined),
    Effect.catchTag('WasmError', (error) => Effect.succeed(error)),
  )

it.effect('creates a builder and returns owned handles', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make({ moduleName: 'demo' })
    const type = yield* Type.func(builder, [ValType.i32], [ValType.i32])
    const func = yield* Func.declare(builder, type, { name: 'identity' })
    assert.strictEqual(yield* Func.name(builder, func), 'identity')
    assert.strictEqual(yield* Func.type(builder, func), type)
  }),
)

it.effect('rejects a handle from a different builder without modifying state', () =>
  Effect.gen(function* () {
    const first = yield* Builder.make()
    const second = yield* Builder.make()
    const foreign = yield* Type.func(first, [], [])
    const error = yield* failure(Func.declare(second, foreign))
    assert.isDefined(error)
    assert.strictEqual(error.reason._tag, 'InvalidState')
    // The rejected declaration must not have committed an entry.
    const local = yield* Type.func(second, [], [])
    const func = yield* Func.declare(second, local)
    assert.strictEqual(yield* Func.name(second, func), undefined)
  }),
)

it.effect('keeps names only on named entities', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [], [])
    const named = yield* Func.declare(builder, type, { name: 'main' })
    const anonymous = yield* Func.declare(builder, type)
    assert.strictEqual(yield* Func.name(builder, named), 'main')
    assert.strictEqual(yield* Func.name(builder, anonymous), undefined)
  }),
)

it.effect('commits concurrent independent declarations exactly once', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [ValType.i64], [])
    const handles = yield* Effect.all(
      Array.from({ length: 16 }, (_, index) => Func.declare(builder, type, { name: `f${index}` })),
      { concurrency: 'unbounded' },
    )
    const names = yield* Effect.all(handles.map((handle) => Func.name(builder, handle)))
    assert.strictEqual(new Set(handles).size, 16)
    assert.deepStrictEqual(
      [...names].sort(),
      Array.from({ length: 16 }, (_, index) => `f${index}`).sort(),
    )
  }),
)

it.effect('fails with InvalidInput for rejected caller input', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const error = yield* failure(
      Global.make(builder, ValType.i32, false, [Instr.i64Const(1n)]).pipe(Effect.asVoid),
    )
    assert.isDefined(error)
    assert.strictEqual(error.reason._tag, 'ValidationFailed')
    assert.strictEqual(error.operation, 'Global.make')
  }),
)
