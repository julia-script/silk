import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Alias from '../src/Alias.js'
import * as Attribute from '../src/Attribute.js'
import * as Builder from '../src/Builder.js'
import * as Constant from '../src/Constant.js'
import * as FunctionActor from '../src/Function.js'
import * as Global from '../src/Global.js'
import * as Type from '../src/Type.js'
import * as Variable from '../src/Variable.js'

it.effect('creates, looks up, renames, configures, and removes ordered globals', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const i32 = yield* Type.integer(builder, 32)
    const initializer = yield* Constant.integerUnsigned(builder, i32, 7)
    const variable = yield* Variable.make(builder, 'answer', i32, {
      initializer,
      constant: true,
      linkage: 'internal',
    })
    const global = yield* Variable.global(builder, variable)

    assert.strictEqual(yield* Global.lookup(builder, 'answer'), global)
    assert.strictEqual((yield* Variable.properties(builder, variable)).initializer, initializer)
    assert.strictEqual((yield* Global.properties(builder, global)).linkage, 'internal')
    yield* Global.rename(builder, global, 'renamed')
    assert.isUndefined(yield* Global.lookup(builder, 'answer'))
    assert.strictEqual(yield* Global.lookup(builder, 'renamed'), global)
    yield* Global.remove(builder, global)
    assert.isUndefined(yield* Global.lookup(builder, 'renamed'))
  }),
)

it.effect('rejects duplicate declarations without disturbing the original', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const i32 = yield* Type.integer(builder, 32)
    const original = yield* Variable.make(builder, 'occupied', i32)
    const duplicate = yield* Effect.flip(Variable.make(builder, 'occupied', i32))

    assert.include(duplicate.message, 'already occupied')
    assert.strictEqual(
      yield* Global.lookup(builder, 'occupied'),
      yield* Variable.global(builder, original),
    )
  }),
)

it.effect('creates aliases and atomically converts an existing global category', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const i32 = yield* Type.integer(builder, 32)
    const target = yield* Variable.make(builder, 'target', i32)
    const targetPointer = yield* Constant.fromGlobal(
      builder,
      yield* Variable.global(builder, target),
    )
    const source = yield* Variable.make(builder, 'source', i32)
    const sourceGlobal = yield* Variable.global(builder, source)
    const alias = yield* Alias.fromGlobal(builder, sourceGlobal, i32, targetPointer)
    const staleVariable = yield* Effect.flip(Variable.properties(builder, source))

    assert.strictEqual(yield* Alias.aliasee(builder, alias), targetPointer)
    assert.strictEqual(yield* Global.kind(builder, sourceGlobal), 'Alias')
    assert.include(staleVariable.message, 'no longer active')
  }),
)

it.effect('canonicalizes compatible function declarations and round-trips properties', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const i32 = yield* Type.integer(builder, 32)
    const type = yield* Type.functionType(builder, i32, [i32])
    const nounwind = yield* Attribute.flag(builder, 'nounwind')
    const attributes = yield* Attribute.functionSet(builder, {
      functionAttributes: yield* Attribute.set(builder, [nounwind]),
    })
    const first = yield* FunctionActor.declare(builder, 'compute', type, {
      callingConvention: 8,
      attributes,
      linkage: 'external',
    })
    const second = yield* FunctionActor.declare(builder, 'compute', type, {
      callingConvention: 8,
      attributes,
    })
    const incompatible = yield* Effect.flip(
      FunctionActor.declare(builder, 'compute', type, { callingConvention: 9 }),
    )

    assert.strictEqual(first, second)
    assert.strictEqual((yield* FunctionActor.properties(builder, first)).attributes, attributes)
    assert.include(incompatible.message, 'incompatible global')
  }),
)

it.effect('replaces globals while preserving stale global indirection', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const i8 = yield* Type.integer(builder, 8)
    const first = yield* Variable.global(builder, yield* Variable.make(builder, 'first', i8))
    const second = yield* Variable.global(builder, yield* Variable.make(builder, 'second', i8))
    yield* Global.replace(builder, first, second)

    assert.deepEqual(yield* Global.name(builder, first), yield* Global.name(builder, second))
    assert.isUndefined(yield* Global.lookup(builder, 'first'))
  }),
)
