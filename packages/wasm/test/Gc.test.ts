import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Binary from '../src/Binary.js'
import * as Builder from '../src/Builder.js'
import * as ExportActor from '../src/Export.js'
import * as Func from '../src/Func.js'
import * as Instr from '../src/Instr.js'
import * as Type from '../src/Type.js'
import * as ValType from '../src/ValType.js'
import type { WasmError } from '../src/WasmError.js'

const failure = <A>(effect: Effect.Effect<A, WasmError>) =>
  effect.pipe(
    Effect.map(() => undefined),
    Effect.catchTag('WasmError', (error) => Effect.succeed(error)),
  )

it.effect('interns struct and array types with canonical rec groups', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const point = yield* Type.struct(builder, [
      { storage: ValType.f64, mutable: true },
      { storage: ValType.f64, mutable: true },
    ])
    const samePoint = yield* Type.struct(builder, [
      { storage: ValType.f64, mutable: true },
      { storage: ValType.f64, mutable: true },
    ])
    assert.strictEqual(point, samePoint)
    const bytes = yield* Type.array(builder, { storage: 'i8', mutable: true })
    assert.notStrictEqual(point, bytes)
    // Equivalent recursive groups dedupe to the same handles.
    const defineNode = (self: Type.Type): Type.Definition => ({
      kind: 'struct',
      fields: [{ storage: ValType.i32 }, { storage: ValType.refNull(self), mutable: true }],
    })
    const [nodeA] = yield* Type.rec(builder, 1, ([self]) => [defineNode(self ?? point)])
    const [nodeB] = yield* Type.rec(builder, 1, ([self]) => [defineNode(self ?? point)])
    assert.strictEqual(nodeA, nodeB)
  }),
)

it.effect('validates declared supertypes structurally', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const base = yield* Type.struct(builder, [{ storage: ValType.i32 }], { final: false })
    const extended = yield* Type.struct(
      builder,
      [{ storage: ValType.i32 }, { storage: ValType.f64 }],
      { supertype: base },
    )
    assert.isDefined(extended)
    const incompatible = yield* failure(
      Type.struct(builder, [{ storage: ValType.f32 }], { supertype: base }),
    )
    assert.strictEqual(incompatible?.reason._tag, 'InvalidInput')
    const finalParent = yield* Type.struct(builder, [{ storage: ValType.i64 }])
    const belowFinal = yield* failure(
      Type.struct(builder, [{ storage: ValType.i64 }], { supertype: finalParent }),
    )
    assert.strictEqual(belowFinal?.reason._tag, 'InvalidInput')
  }),
)

it.effect('allocates, mutates, and reads structs and arrays at runtime', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const box = yield* Type.struct(builder, [{ storage: ValType.i32, mutable: true }], {
      name: 'box',
    })
    const run = yield* Func.declare(
      builder,
      yield* Type.func(builder, [ValType.i32], [ValType.i32]),
      { name: 'run' },
    )
    yield* Func.define(builder, run, {
      locals: [{ type: ValType.refNull(box), name: 'cell' }],
      body: [
        Instr.localGet(0),
        Instr.structNew(box),
        Instr.localTee(1),
        Instr.localGet(0),
        Instr.i32Const(1),
        Instr.op('i32.add'),
        Instr.structSet(box, 0),
        Instr.localGet(1),
        Instr.structGet(box, 0),
      ],
    })
    yield* ExportActor.func(builder, 'run', run)
    const bytes = yield* Binary.encode(builder)
    assert.isTrue(WebAssembly.validate(bytes))
    const instance = yield* Effect.promise(() =>
      WebAssembly.instantiate(bytes.buffer as ArrayBuffer, {}),
    )
    const exports = instance.instance.exports as { run: (value: number) => number }
    assert.strictEqual(exports.run(41), 42)
  }),
)

it.effect('rejects packed access without signedness and immutable writes', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const packed = yield* Type.struct(builder, [{ storage: 'i8' }])
    const type = yield* Type.func(builder, [], [ValType.i32])
    const func = yield* Func.declare(builder, type)
    const unsigned = yield* failure(
      Func.define(builder, func, {
        body: [Instr.structNew(packed, { defaults: true }), Instr.structGet(packed, 0)],
      }),
    )
    assert.strictEqual(unsigned?.reason._tag, 'ValidationFailed')
    const immutable = yield* failure(
      Func.define(builder, func, {
        body: [
          Instr.structNew(packed, { defaults: true }),
          Instr.i32Const(1),
          Instr.structSet(packed, 0),
          Instr.i32Const(0),
        ],
      }),
    )
    assert.strictEqual(immutable?.reason._tag, 'ValidationFailed')
    yield* Func.define(builder, func, {
      body: [Instr.structNew(packed, { defaults: true }), Instr.structGet(packed, 0, 'u')],
    })
  }),
)

it.effect('widens typed function references through subtyping and calls them', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const answerType = yield* Type.func(builder, [], [ValType.i32])
    const answer = yield* Func.declare(builder, answerType, { name: 'answer' })
    yield* Func.define(builder, answer, { body: [Instr.i32Const(42)] })
    const caller = yield* Func.declare(builder, answerType, { name: 'caller' })
    yield* Func.define(builder, caller, {
      locals: [{ type: ValType.funcref, name: 'wide' }],
      body: [
        // A precise (ref $answerType) widens into a funcref local...
        Instr.refFunc(answer),
        Instr.localSet(0),
        // ...and call_ref goes through the precise reference.
        Instr.refFunc(answer),
        Instr.callRef(answerType),
      ],
    })
    yield* ExportActor.func(builder, 'caller', caller)
    yield* ExportActor.func(builder, 'answer', answer)
    const bytes = yield* Binary.encode(builder)
    assert.isTrue(WebAssembly.validate(bytes))
    const instance = yield* Effect.promise(() =>
      WebAssembly.instantiate(bytes.buffer as ArrayBuffer, {}),
    )
    const exports = instance.instance.exports as { caller: () => number }
    assert.strictEqual(exports.caller(), 42)
  }),
)

it.effect('type-checks casts within a hierarchy and rejects cross-hierarchy casts', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const box = yield* Type.struct(builder, [{ storage: ValType.i32 }])
    const type = yield* Type.func(builder, [], [ValType.i32])
    const func = yield* Func.declare(builder, type)
    yield* Func.define(builder, func, {
      body: [
        Instr.structNew(box, { defaults: true }),
        Instr.refCast(ValType.ref(box)),
        Instr.structGet(box, 0),
        Instr.block(Instr.emptyBlockType, [
          Instr.refNull('any'),
          Instr.refTest(ValType.ref('struct')),
          Instr.op('drop'),
        ]),
      ],
    })
    const crossHierarchy = yield* failure(
      Func.define(builder, yield* Func.declare(builder, type), {
        body: [
          Instr.refNull('extern'),
          Instr.refCast(ValType.ref(box)),
          Instr.op('drop'),
          Instr.i32Const(0),
        ],
      }),
    )
    assert.strictEqual(crossHierarchy?.reason._tag, 'ValidationFailed')
  }),
)

it.effect('types br_on_cast against the target label', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const box = yield* Type.struct(builder, [{ storage: ValType.i32 }], { name: 'gcBox' })
    const type = yield* Type.func(builder, [], [ValType.i32])
    const classify = yield* Func.declare(builder, type, { name: 'classify' })
    yield* Func.define(builder, classify, {
      body: [
        Instr.block(Instr.valueBlockType(ValType.refNull(box)), [
          Instr.i32Const(5),
          Instr.op('ref.i31'),
          Instr.brOnCast(0, ValType.anyref, ValType.refNull(box)),
          Instr.op('drop'),
          Instr.refNull(box),
        ]),
        Instr.op('ref.is_null'),
      ],
    })
    yield* ExportActor.func(builder, 'classify', classify)
    const bytes = yield* Binary.encode(builder)
    assert.isTrue(WebAssembly.validate(bytes))
    const badLabel = yield* failure(
      Func.define(builder, yield* Func.declare(builder, type), {
        body: [
          Instr.block(Instr.emptyBlockType, [
            Instr.refNull('any'),
            Instr.brOnCast(0, ValType.anyref, ValType.refNull(box)),
            Instr.op('drop'),
          ]),
          Instr.i32Const(0),
        ],
      }),
    )
    assert.strictEqual(badLabel?.reason._tag, 'ValidationFailed')
  }),
)

it.effect('handles i31 and null-aware branches at runtime', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [ValType.i32], [ValType.i32])
    const roundtrip = yield* Func.declare(builder, type, { name: 'roundtrip' })
    yield* Func.define(builder, roundtrip, {
      body: [
        Instr.block(Instr.emptyBlockType, [
          Instr.localGet(0),
          Instr.op('ref.i31'),
          Instr.brOnNull(0),
          Instr.op('i31.get_s'),
          Instr.op('return'),
        ]),
        Instr.i32Const(-1),
      ],
    })
    yield* ExportActor.func(builder, 'roundtrip', roundtrip)
    const bytes = yield* Binary.encode(builder)
    const instance = yield* Effect.promise(() =>
      WebAssembly.instantiate(bytes.buffer as ArrayBuffer, {}),
    )
    const exports = instance.instance.exports as { roundtrip: (value: number) => number }
    assert.strictEqual(exports.roundtrip(7), 7)
  }),
)

it.effect('rejects non-defaultable locals as documented', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [], [])
    const func = yield* Func.declare(builder, type)
    const error = yield* failure(
      Func.define(builder, func, {
        locals: [{ type: ValType.ref('any') }],
        body: [],
      }),
    )
    assert.strictEqual(error?.reason._tag, 'ValidationFailed')
  }),
)

it.effect('supports supertype-below where a supertype value is rejected', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const box = yield* Type.struct(builder, [{ storage: ValType.i32 }])
    const type = yield* Type.func(builder, [], [])
    const func = yield* Func.declare(builder, type)
    const tooWide = yield* failure(
      Func.define(builder, func, {
        body: [Instr.refNull('any'), Instr.structGet(box, 0), Instr.op('drop')],
      }),
    )
    assert.strictEqual(tooWide?.reason._tag, 'ValidationFailed')
  }),
)
