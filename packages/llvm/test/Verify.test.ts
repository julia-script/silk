import { raise } from './support/raise.js'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Block from '../src/Block.js'
import * as Builder from '../src/Builder.js'
import * as Constant from '../src/Constant.js'
import * as FunctionActor from '../src/Function.js'
import * as FunctionBody from '../src/FunctionBody.js'
import * as Type from '../src/Type.js'
import * as Value from '../src/Value.js'
import * as Verify from '../src/Verify.js'

/**
 * A diamond over one argument. `leak` decides whether the value the taken arm computes is read in
 * the join — the shape #130 emitted, where a cleanup arm's reloads escaped into the arm's join.
 */
const buildDiamond = Effect.fnUntraced(function* (name: string, leak: boolean) {
  const builder = yield* Builder.make()
  const i32 = yield* Type.integer(builder, 32)
  const type = yield* Type.functionType(builder, i32, [i32])
  const fn = yield* FunctionActor.declare(builder, name, type)
  yield* FunctionActor.buildBody(
    builder,
    fn,
    Effect.fnUntraced(function* (body) {
      const entry = yield* Block.make(body, 'entry')
      const taken = yield* Block.make(body, 'taken')
      const otherwise = yield* Block.make(body, 'otherwise')
      const join = yield* Block.make(body, 'join')
      const argument = yield* Value.argument(body, 0)
      const zero = yield* Constant.integerSigned(builder, i32, 0n)

      yield* Block.setInsertionPoint(body, entry)
      const condition = yield* FunctionBody.integerCompare(body, 'ne', argument, zero, 'cond')
      yield* FunctionBody.conditionalBranch(body, condition, taken, otherwise)

      yield* Block.setInsertionPoint(body, taken)
      const armLocal = yield* FunctionBody.binary(body, 'add', argument, argument, 'armLocal')
      yield* FunctionBody.branch(body, join)

      yield* Block.setInsertionPoint(body, otherwise)
      yield* FunctionBody.branch(body, join)

      yield* Block.setInsertionPoint(body, join)
      const result = yield* FunctionBody.binary(
        body,
        'add',
        leak ? armLocal : argument,
        argument,
        'result',
      )
      yield* FunctionBody.returnValue(body, result)
    }),
  )
  return builder
})

it.effect('accepts a well-formed diamond', () =>
  Effect.gen(function* () {
    const builder = yield* buildDiamond('dominates', false)
    assert.deepEqual(yield* Verify.verify(builder), [])
  }),
)

it.effect('reports a value that escapes its arm into the join, naming the function', () =>
  Effect.gen(function* () {
    const builder = yield* buildDiamond('escapes', true)
    const violations = yield* Verify.verify(builder)
    assert.strictEqual(violations.length, 1)
    const violation = violations.at(0)
    assert.strictEqual(violation?.function, '@escapes')
    assert.strictEqual(violation?.message, 'Instruction does not dominate all uses!')
    assert.deepEqual(violation?.detail, [
      '  %armLocal defined in %taken',
      '  used by %result = add in %join',
    ])
    assert.include(Verify.format(violations), '@escapes')
  }),
)

it.effect('accepts a phi that joins both arms', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const i32 = yield* Type.integer(builder, 32)
    const type = yield* Type.functionType(builder, i32, [i32])
    const fn = yield* FunctionActor.declare(builder, 'joined', type)
    yield* FunctionActor.buildBody(
      builder,
      fn,
      Effect.fnUntraced(function* (body) {
        const entry = yield* Block.make(body, 'entry')
        const taken = yield* Block.make(body, 'taken')
        const otherwise = yield* Block.make(body, 'otherwise')
        const join = yield* Block.make(body, 'join')
        const argument = yield* Value.argument(body, 0)
        const zero = yield* Constant.integerSigned(builder, i32, 0n)

        yield* Block.setInsertionPoint(body, entry)
        const condition = yield* FunctionBody.integerCompare(body, 'ne', argument, zero, 'cond')
        yield* FunctionBody.conditionalBranch(body, condition, taken, otherwise)

        yield* Block.setInsertionPoint(body, taken)
        const doubled = yield* FunctionBody.binary(body, 'add', argument, argument, 'doubled')
        yield* FunctionBody.branch(body, join)

        yield* Block.setInsertionPoint(body, otherwise)
        yield* FunctionBody.branch(body, join)

        yield* Block.setInsertionPoint(body, join)
        const merged = yield* FunctionBody.phi(body, i32, 'merged')
        yield* FunctionBody.addPhiIncoming(body, merged, doubled, taken)
        yield* FunctionBody.addPhiIncoming(body, merged, zero, otherwise)
        const value = yield* FunctionBody.sealPhi(body, merged)
        yield* FunctionBody.returnValue(body, value)
      }),
    )
    assert.deepEqual(yield* Verify.verify(builder), [])
  }),
)

it.effect('reports a phi reading an arm across the wrong edge', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const i32 = yield* Type.integer(builder, 32)
    const type = yield* Type.functionType(builder, i32, [i32])
    const fn = yield* FunctionActor.declare(builder, 'crossed', type)
    yield* FunctionActor.buildBody(
      builder,
      fn,
      Effect.fnUntraced(function* (body) {
        const entry = yield* Block.make(body, 'entry')
        const taken = yield* Block.make(body, 'taken')
        const otherwise = yield* Block.make(body, 'otherwise')
        const join = yield* Block.make(body, 'join')
        const argument = yield* Value.argument(body, 0)
        const zero = yield* Constant.integerSigned(builder, i32, 0n)

        yield* Block.setInsertionPoint(body, entry)
        const condition = yield* FunctionBody.integerCompare(body, 'ne', argument, zero, 'cond')
        yield* FunctionBody.conditionalBranch(body, condition, taken, otherwise)

        yield* Block.setInsertionPoint(body, taken)
        const doubled = yield* FunctionBody.binary(body, 'add', argument, argument, 'doubled')
        yield* FunctionBody.branch(body, join)

        yield* Block.setInsertionPoint(body, otherwise)
        yield* FunctionBody.branch(body, join)

        yield* Block.setInsertionPoint(body, join)
        const merged = yield* FunctionBody.phi(body, i32, 'merged')
        yield* FunctionBody.addPhiIncoming(body, merged, zero, taken)
        // The value the taken arm defines, read as if it arrived along the other edge.
        yield* FunctionBody.addPhiIncoming(body, merged, doubled, otherwise)
        const value = yield* FunctionBody.sealPhi(body, merged)
        yield* FunctionBody.returnValue(body, value)
      }),
    )
    const violations = yield* Verify.verify(builder)
    assert.strictEqual(violations.length, 1)
    assert.strictEqual(violations.at(0)?.function, '@crossed')
    assert.strictEqual(violations.at(0)?.message, 'Instruction does not dominate all uses!')
    assert.deepEqual(violations.at(0)?.detail, [
      '  %doubled defined in %taken',
      '  used by %merged = Phi in %join, incoming from %otherwise',
    ])
  }),
)

it.effect('says nothing about a module with no function bodies', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const i32 = yield* Type.integer(builder, 32)
    const type = yield* Type.functionType(builder, i32, [])
    yield* FunctionActor.declare(builder, 'external', type)
    assert.deepEqual(yield* Verify.verify(builder), [])
  }),
)

it.effect('rejects use of an invoke result on the unwind edge and missing personalities', () =>
  Effect.gen(function* () {
    for (const fault of ['result-on-unwind', 'missing-personality', 'normal-bypass'] as const) {
      const builder = yield* Builder.make()
      const i32 = yield* Type.integer(builder, 32)
      const signature = yield* Type.functionType(builder, i32, [])
      const callee = yield* FunctionActor.declare(builder, 'foreign', signature)
      const address = yield* Constant.fromGlobal(
        builder,
        yield* FunctionActor.global(builder, callee),
      )
      const fn = yield* FunctionActor.declare(
        builder,
        fault,
        yield* Type.functionType(builder, i32, [yield* Type.integer(builder, 1)]),
        fault === 'missing-personality' ? {} : { personality: address },
      )
      yield* FunctionActor.buildBody(
        builder,
        fn,
        Effect.fnUntraced(function* (body) {
          const entry = yield* Block.make(body, 'entry')
          const normal = yield* Block.make(body, 'normal')
          const unwind = yield* Block.make(body, 'unwind')
          yield* Block.setInsertionPoint(body, entry)
          if (fault === 'normal-bypass') {
            const invoking = yield* Block.make(body, 'invoking')
            yield* FunctionBody.conditionalBranch(
              body,
              yield* Value.argument(body, 0),
              invoking,
              normal,
            )
            yield* Block.setInsertionPoint(body, invoking)
          }
          const result =
            (yield* FunctionBody.invoke(body, signature, address, [], normal, unwind)) ??
            raise('expected invoke result')
          yield* Block.setInsertionPoint(body, normal)
          yield* FunctionBody.returnValue(body, result)
          yield* Block.setInsertionPoint(body, unwind)
          yield* FunctionBody.cleanupLandingPad(body)
          if (fault === 'result-on-unwind') yield* FunctionBody.returnValue(body, result)
          else yield* FunctionBody.unreachable(body)
        }),
      )
      assert.deepEqual(
        (yield* Verify.verify(builder)).map((violation) => violation.message),
        [
          fault === 'missing-personality'
            ? 'Landing pad requires a personality function!'
            : 'Instruction does not dominate all uses!',
        ],
      )
    }
  }),
)
