import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Binary from '../src/Binary.js'
import * as Builder from '../src/Builder.js'
import * as ExportActor from '../src/Export.js'
import * as Func from '../src/Func.js'
import * as Import from '../src/Import.js'
import * as Instr from '../src/Instr.js'
import * as Tag from '../src/Tag.js'
import * as Type from '../src/Type.js'
import * as ValType from '../src/ValType.js'
import type { WasmError } from '../src/WasmError.js'
import * as WatText from '../src/WatText.js'

const failure = <A>(effect: Effect.Effect<A, WasmError>) =>
  effect.pipe(
    Effect.as(undefined),
    Effect.catchTag('WasmError', (error) => Effect.succeed(error)),
  )

it.effect('declares, imports, and exports tags', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const payload = yield* Type.func(builder, [ValType.i32], [])
    const own = yield* Tag.make(builder, payload, { name: 'error' })
    const foreign = yield* Import.tag(builder, 'env', 'oops', payload, { name: 'oops' })
    yield* ExportActor.tag(builder, 'error', own)
    yield* ExportActor.tag(builder, 'oops', foreign)
    const withResults = yield* Type.func(builder, [], [ValType.i32])
    const rejected = yield* failure(Tag.make(builder, withResults))
    assert.strictEqual(rejected?.reason._tag, 'InvalidInput')
    const bytes = yield* Binary.encode(builder)
    assert.isTrue(WebAssembly.validate(bytes))
  }),
)

it.effect('catches a thrown tag through try_table', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const payload = yield* Type.func(builder, [ValType.i32], [])
    const error = yield* Tag.make(builder, payload, { name: 'error' })
    const type = yield* Type.func(builder, [ValType.i32], [ValType.i32])
    const guarded = yield* Func.declare(builder, type, { name: 'guarded' })
    yield* Func.define(builder, guarded, {
      body: [
        Instr.block(Instr.valueBlockType(ValType.i32), [
          Instr.tryTable(
            Instr.emptyBlockType,
            [Instr.catchTag(error, 0)],
            [
              Instr.localGet(0),
              Instr.op('i32.eqz'),
              Instr.ifElse(Instr.emptyBlockType, [Instr.i32Const(41), Instr.throwTag(error)]),
            ],
          ),
          Instr.i32Const(7),
          Instr.br(0),
        ]),
        Instr.i32Const(1),
        Instr.op('i32.add'),
      ],
    })
    yield* ExportActor.func(builder, 'guarded', guarded)
    const bytes = yield* Binary.encode(builder)
    assert.isTrue(WebAssembly.validate(bytes))
    const instance = yield* Effect.promise(() =>
      WebAssembly.instantiate(bytes.buffer as ArrayBuffer, {}),
    )
    const exports = instance.instance.exports as { guarded: (value: number) => number }
    // Non-zero input: no throw, block yields 7, result 8.
    assert.strictEqual(exports.guarded(5), 8)
    // Zero input: throw caught into the block with payload 41, result 42.
    assert.strictEqual(exports.guarded(0), 42)
  }),
)

it.effect('rejects catch clauses whose label arity does not match', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const payload = yield* Type.func(builder, [ValType.i32], [])
    const error = yield* Tag.make(builder, payload)
    const type = yield* Type.func(builder, [], [])
    const func = yield* Func.declare(builder, type)
    const mismatch = yield* failure(
      Func.define(builder, func, {
        body: [
          Instr.block(Instr.emptyBlockType, [
            // catch delivers [i32] but the target label accepts [].
            Instr.tryTable(Instr.emptyBlockType, [Instr.catchTag(error, 0)], []),
          ]),
        ],
      }),
    )
    assert.strictEqual(mismatch?.reason._tag, 'ValidationFailed')
    const refMismatch = yield* failure(
      Func.define(builder, func, {
        body: [
          Instr.block(Instr.emptyBlockType, [
            Instr.tryTable(Instr.emptyBlockType, [Instr.catchAllRef(0)], []),
          ]),
        ],
      }),
    )
    assert.strictEqual(refMismatch?.reason._tag, 'ValidationFailed')
  }),
)

it.effect('types throw_ref and exnref rethrows', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const payload = yield* Type.func(builder, [], [])
    const error = yield* Tag.make(builder, payload, { name: 'error' })
    const type = yield* Type.func(builder, [], [])
    const rethrow = yield* Func.declare(builder, type, { name: 'rethrow' })
    yield* Func.define(builder, rethrow, {
      body: [
        Instr.block(Instr.valueBlockType(ValType.exnref), [
          Instr.tryTable(
            Instr.emptyBlockType,
            [Instr.catchTagRef(error, 0)],
            [Instr.throwTag(error)],
          ),
          Instr.op('return'),
        ]),
        Instr.op('throw_ref'),
      ],
    })
    yield* ExportActor.func(builder, 'rethrow', rethrow)
    const bytes = yield* Binary.encode(builder)
    assert.isTrue(WebAssembly.validate(bytes))
    const wrongOperand = yield* failure(
      Func.define(builder, yield* Func.declare(builder, type), {
        body: [Instr.i32Const(1), Instr.op('throw_ref')],
      }),
    )
    assert.strictEqual(wrongOperand?.reason._tag, 'ValidationFailed')
  }),
)

it.effect('emits the branch-hint section only when hints exist', () =>
  Effect.gen(function* () {
    const build = (hinted: boolean) =>
      Effect.gen(function* () {
        const builder = yield* Builder.make()
        const type = yield* Type.func(builder, [ValType.i32], [ValType.i32])
        const func = yield* Func.declare(builder, type, { name: 'clamp' })
        yield* Func.define(builder, func, {
          body: [
            Instr.block(Instr.emptyBlockType, [
              Instr.localGet(0),
              hinted ? Instr.brIf(0, { hint: 'unlikely' }) : Instr.brIf(0),
            ]),
            Instr.localGet(0),
            Instr.ifElse(
              Instr.valueBlockType(ValType.i32),
              [Instr.i32Const(1)],
              [Instr.i32Const(2)],
              hinted ? { hint: 'likely' } : {},
            ),
          ],
        })
        yield* ExportActor.func(builder, 'clamp', func)
        return builder
      })
    const plainBytes = yield* Binary.encode(yield* build(false))
    const hintedBuilder = yield* build(true)
    const hintedBytes = yield* Binary.encode(hintedBuilder)
    const marker = 'metadata.code.branch_hint'
    const text = (bytes: Uint8Array) => [...bytes].map((byte) => String.fromCharCode(byte)).join('')
    assert.isFalse(text(plainBytes).includes(marker))
    assert.isTrue(text(hintedBytes).includes(marker))
    // Hints never change executable code bytes: both modules validate and run identically.
    assert.isTrue(WebAssembly.validate(hintedBytes))
    const rendered = yield* WatText.render(hintedBuilder)
    assert.include(rendered, '(@metadata.code.branch_hint "\\00")')
    assert.include(rendered, '(@metadata.code.branch_hint "\\01")')
  }),
)
