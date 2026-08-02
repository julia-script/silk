import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import { expect, test } from 'vitest'
import * as Alias from '../src/Alias.js'
import * as Attribute from '../src/Attribute.js'
import * as Builder from '../src/Builder.js'
import * as Constant from '../src/Constant.js'
import * as FunctionActor from '../src/Function.js'
import * as Global from '../src/Global.js'
import * as Type from '../src/Type.js'
import * as Variable from '../src/Variable.js'

const TestRuntime = ManagedRuntime.make(Layer.empty)

test(
  'creates, looks up, renames, configures, and removes ordered globals',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const i32 = yield* Type.integer(builder, 32)
    const initializer = yield* Constant.integerUnsigned(builder, i32, 7)
    const variable = yield* Variable.make(builder, 'answer', i32, {
      initializer,
      constant: true,
      linkage: 'internal',
    })
    const global = yield* Variable.global(builder, variable)

    expect(yield* Global.lookup(builder, 'answer')).toBe(global)
    expect((yield* Variable.properties(builder, variable)).initializer).toBe(initializer)
    expect((yield* Global.properties(builder, global)).linkage).toBe('internal')
    yield* Global.rename(builder, global, 'renamed')
    expect(yield* Global.lookup(builder, 'answer')).toBeUndefined()
    expect(yield* Global.lookup(builder, 'renamed')).toBe(global)
    yield* Global.remove(builder, global)
    expect(yield* Global.lookup(builder, 'renamed')).toBeUndefined()
  }, TestRuntime.runPromise),
)

test(
  'rejects duplicate declarations without disturbing the original',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const i32 = yield* Type.integer(builder, 32)
    const original = yield* Variable.make(builder, 'occupied', i32)
    const duplicate = yield* Effect.flip(Variable.make(builder, 'occupied', i32))

    expect(duplicate.message).toContain('already occupied')
    expect(yield* Global.lookup(builder, 'occupied')).toBe(
      yield* Variable.global(builder, original),
    )
  }, TestRuntime.runPromise),
)

test(
  'creates aliases and atomically converts an existing global category',
  Effect.fnUntraced(function* () {
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

    expect(yield* Alias.aliasee(builder, alias)).toBe(targetPointer)
    expect(yield* Global.kind(builder, sourceGlobal)).toBe('Alias')
    expect(staleVariable.message).toContain('no longer active')
  }, TestRuntime.runPromise),
)

test(
  'canonicalizes compatible function declarations and round-trips properties',
  Effect.fnUntraced(function* () {
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

    expect(first).toBe(second)
    expect((yield* FunctionActor.properties(builder, first)).attributes).toBe(attributes)
    expect(incompatible.message).toContain('incompatible global')
  }, TestRuntime.runPromise),
)

test(
  'replaces globals while preserving stale global indirection',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const i8 = yield* Type.integer(builder, 8)
    const first = yield* Variable.global(builder, yield* Variable.make(builder, 'first', i8))
    const second = yield* Variable.global(builder, yield* Variable.make(builder, 'second', i8))
    yield* Global.replace(builder, first, second)

    expect(yield* Global.name(builder, first)).toEqual(yield* Global.name(builder, second))
    expect(yield* Global.lookup(builder, 'first')).toBeUndefined()
  }, TestRuntime.runPromise),
)
