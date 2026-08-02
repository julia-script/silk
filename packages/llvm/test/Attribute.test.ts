import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import { expect, test } from 'vitest'
import * as Attribute from '../src/Attribute.js'
import * as Builder from '../src/Builder.js'
import * as Type from '../src/Type.js'

const TestRuntime = ManagedRuntime.make(Layer.empty)

test(
  'interns every attribute storage shape and canonicalizes set ordering',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const i8 = yield* Type.integer(builder, 8)
    const noAlias = yield* Attribute.flag(builder, 'noalias')
    const dereferenceable = yield* Attribute.integer(builder, 'dereferenceable', 16n)
    const byValue = yield* Attribute.typeAttribute(builder, 'byval', i8)
    const targetCpu = yield* Attribute.string(builder, 'target-cpu', 'generic')
    const initializes = yield* Attribute.integerList(builder, 'initializes', [0n, 8n])
    const first = yield* Attribute.set(builder, [
      targetCpu,
      noAlias,
      byValue,
      dereferenceable,
      initializes,
    ])
    const second = yield* Attribute.set(builder, [
      initializes,
      dereferenceable,
      byValue,
      noAlias,
      targetCpu,
    ])

    expect(first).toBe(second)
    expect(yield* Attribute.entries(builder, first)).toHaveLength(5)
  }, TestRuntime.runPromise),
)

test(
  'supports immutable editing and canonical function attribute positions',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const cold = yield* Attribute.flag(builder, 'cold')
    const noUnwind = yield* Attribute.flag(builder, 'nounwind')
    const empty = yield* Attribute.set(builder, [])
    const one = yield* Attribute.add(builder, empty, cold)
    const two = yield* Attribute.add(builder, one, noUnwind)
    const backToOne = yield* Attribute.remove(builder, two, noUnwind)
    const first = yield* Attribute.functionSet(builder, {
      functionAttributes: two,
      returnAttributes: empty,
      parameterAttributes: [one],
    })
    const second = yield* Attribute.functionSet(builder, {
      parameterAttributes: [backToOne],
      returnAttributes: empty,
      functionAttributes: yield* Attribute.set(builder, [noUnwind, cold]),
    })

    expect(backToOne).toBe(one)
    expect(first).toBe(second)
  }, TestRuntime.runPromise),
)

test(
  'rejects conflicting and cross-builder attribute sets',
  Effect.fnUntraced(function* () {
    const first = yield* Builder.make()
    const second = yield* Builder.make()
    const left = yield* Attribute.integer(first, 'align', 8)
    const right = yield* Attribute.integer(first, 'align', 16)
    const conflict = yield* Effect.flip(Attribute.set(first, [left, right]))
    const set = yield* Attribute.set(first, [left])
    const ownerError = yield* Effect.flip(Attribute.entries(second, set))

    expect(conflict.message).toContain('conflicting values')
    expect(ownerError.message).toContain('different LLVM builder')
  }, TestRuntime.runPromise),
)
