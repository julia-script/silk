import * as Effect from 'effect/Effect'
import * as Fiber from 'effect/Fiber'
import * as Layer from 'effect/Layer'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import { expect, test } from 'vitest'
import * as Attribute from '../src/Attribute.js'
import * as Block from '../src/Block.js'
import * as Builder from '../src/Builder.js'
import * as Constant from '../src/Constant.js'
import * as FunctionActor from '../src/Function.js'
import * as FunctionBody from '../src/FunctionBody.js'
import * as IrText from '../src/IrText.js'
import { SilkError } from '../src/SilkError.js'
import * as Type from '../src/Type.js'
import * as Value from '../src/Value.js'

const TestRuntime = ManagedRuntime.make(Layer.empty)

const buildAdd = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  fn: FunctionActor.Function,
) {
  return yield* FunctionActor.buildBody(
    builder,
    fn,
    Effect.fnUntraced(function* (body) {
      yield* Block.make(body, 'entry')
      const left = yield* Value.argument(body, 0)
      const right = yield* Value.argument(body, 1)
      const sum = yield* FunctionBody.binary(body, 'add', left, right, 'sum')
      yield* FunctionBody.returnValue(body, sum)
      return body
    }),
  )
})

test(
  'commits a valid body once and closes its scoped draft',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const i32 = yield* Type.integer(builder, 32)
    const type = yield* Type.functionType(builder, i32, [i32, i32])
    const fn = yield* FunctionActor.declare(builder, 'add', type)
    const closed = yield* buildAdd(builder, fn)

    const reuse = yield* Effect.flip(Block.make(closed, 'late'))
    expect(reuse).toBeInstanceOf(SilkError)
    const duplicate = yield* Effect.flip(buildAdd(builder, fn))
    expect(duplicate.message).toContain('already has a committed body')
  }, TestRuntime.runPromise),
)

test(
  'rolls back callback and validation failures without exposing partial bodies',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const voidType = yield* Type.voidType(builder)
    const type = yield* Type.functionType(builder, voidType, [])
    const fn = yield* FunctionActor.declare(builder, 'retry', type)

    const callbackFailure = yield* Effect.flip(
      FunctionActor.buildBody(
        builder,
        fn,
        Effect.fnUntraced(function* (body) {
          yield* Block.make(body, 'discarded')
          return yield* Effect.fail(
            new SilkError({ operation: 'test', message: 'discard this body', cause: fn }),
          )
        }),
      ),
    )
    expect(callbackFailure.message).toBe('discard this body')

    const validationFailure = yield* Effect.flip(
      FunctionActor.buildBody(
        builder,
        fn,
        Effect.fnUntraced(function* (body) {
          yield* Block.make(body, 'unterminated')
        }),
      ),
    )
    expect(validationFailure.message).toContain('terminator')

    const phiFailure = yield* Effect.flip(
      FunctionActor.buildBody(
        builder,
        fn,
        Effect.fnUntraced(function* (body) {
          const entry = yield* Block.make(body, 'entry')
          const merge = yield* Block.make(body, 'merge')
          yield* Block.setInsertionPoint(body, entry)
          yield* FunctionBody.branch(body, merge)
          yield* Block.setInsertionPoint(body, merge)
          const phi = yield* FunctionBody.phi(body, voidType, 'malformed')
          yield* FunctionBody.sealPhi(body, phi)
          yield* FunctionBody.returnVoid(body)
        }),
      ),
    )
    expect(phiFailure.message).toContain('cover every predecessor')

    yield* FunctionActor.buildBody(
      builder,
      fn,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        yield* FunctionBody.returnVoid(body)
      }),
    )
    expect(yield* IrText.render(builder)).toContain('define void @retry()')
  }, TestRuntime.runPromise),
)

test(
  'rejects values and blocks owned by another function body',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const i32 = yield* Type.integer(builder, 32)
    const type = yield* Type.functionType(builder, i32, [i32])
    const first = yield* FunctionActor.declare(builder, 'first', type)
    const second = yield* FunctionActor.declare(builder, 'second', type)
    const foreign = yield* FunctionActor.buildBody(
      builder,
      first,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const argument = yield* Value.argument(body, 0)
        yield* FunctionBody.returnValue(body, argument)
        return argument
      }),
    )

    const error = yield* Effect.flip(
      FunctionActor.buildBody(
        builder,
        second,
        Effect.fnUntraced(function* (body) {
          yield* Block.make(body, 'entry')
          yield* FunctionBody.returnValue(body, foreign)
        }),
      ),
    )
    expect(error.message).toContain('different function body')
  }, TestRuntime.runPromise),
)

test(
  'rejects use of a draft from a child fiber',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const voidType = yield* Type.voidType(builder)
    const type = yield* Type.functionType(builder, voidType, [])
    const fn = yield* FunctionActor.declare(builder, 'fiber_owned', type)

    yield* FunctionActor.buildBody(
      builder,
      fn,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const child = yield* Effect.forkChild(Block.make(body, 'foreign'))
        const error = yield* Effect.flip(Fiber.join(child))
        expect(error.message).toContain('another fiber')
        yield* FunctionBody.returnVoid(body)
      }),
    )
  }, TestRuntime.runPromise),
)

test(
  'builds arithmetic, comparisons, casts, selects, and aggregate operations',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const i1 = yield* Type.integer(builder, 1)
    const i32 = yield* Type.integer(builder, 32)
    const i64 = yield* Type.integer(builder, 64)
    const pair = yield* Type.structure(builder, [i32, i64])
    const type = yield* Type.functionType(builder, i64, [i32, i32, i1])
    const fn = yield* FunctionActor.declare(builder, 'core_ops', type)

    yield* FunctionActor.buildBody(
      builder,
      fn,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const left = yield* Value.argument(body, 0)
        const right = yield* Value.argument(body, 1)
        const condition = yield* Value.argument(body, 2)
        const sum = yield* FunctionBody.binary(body, 'add', left, right, 'sum', {
          noSignedWrap: true,
        })
        const different = yield* FunctionBody.integerCompare(body, 'ne', left, right, 'different')
        const chosen = yield* FunctionBody.select(body, different, sum, left, 'chosen')
        const widened = yield* FunctionBody.cast(body, 'zext', chosen, i64, 'widened')
        const empty = yield* Constant.poison(builder, pair)
        const withLeft = yield* FunctionBody.insertValue(body, empty, chosen, [0], 'with_left')
        const withBoth = yield* FunctionBody.insertValue(body, withLeft, widened, [1], 'with_both')
        const extracted = yield* FunctionBody.extractValue(body, withBoth, [1], 'extracted')
        const selected = yield* FunctionBody.select(body, condition, extracted, widened, 'selected')
        yield* FunctionBody.returnValue(body, selected)
      }),
    )

    const text = yield* IrText.render(builder)
    expect(text).toContain('%sum = add nsw i32 %v0, %v1')
    expect(text).toContain('%different = icmp ne i32 %v0, %v1')
    expect(text).toContain('%widened = zext i32 %chosen to i64')
    expect(text).toContain('insertvalue')
    expect(text).toContain('extractvalue')
  }, TestRuntime.runPromise),
)

test(
  'builds a diamond with branches, a forward-aware phi, and a direct call',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make({ sourceFilename: 'core.ll' })
    const i32 = yield* Type.integer(builder, 32)
    const binaryType = yield* Type.functionType(builder, i32, [i32, i32])
    const callee = yield* FunctionActor.declare(builder, 'callee', binaryType)
    yield* buildAdd(builder, callee)
    const fn = yield* FunctionActor.declare(builder, 'diamond', binaryType)

    yield* FunctionActor.buildBody(
      builder,
      fn,
      Effect.fnUntraced(function* (body) {
        const entry = yield* Block.make(body, 'entry')
        const onTrue = yield* Block.make(body, 'on_true')
        const onFalse = yield* Block.make(body, 'on_false')
        const merge = yield* Block.make(body, 'merge')
        yield* Block.setInsertionPoint(body, entry)
        const left = yield* Value.argument(body, 0)
        const right = yield* Value.argument(body, 1)
        const condition = yield* FunctionBody.integerCompare(body, 'sgt', left, right, 'condition')
        yield* FunctionBody.conditionalBranch(body, condition, onTrue, onFalse, 'true-likely')

        yield* Block.setInsertionPoint(body, onTrue)
        const added = yield* FunctionBody.callDirect(body, callee, [left, right], 'added')
        if (added === undefined) {
          return yield* Effect.fail(
            new SilkError({ operation: 'test', message: 'expected call result', cause: callee }),
          )
        }
        yield* FunctionBody.branch(body, merge)

        yield* Block.setInsertionPoint(body, onFalse)
        const subtracted = yield* FunctionBody.binary(body, 'sub', left, right, 'subtracted')
        yield* FunctionBody.branch(body, merge)

        yield* Block.setInsertionPoint(body, merge)
        const phi = yield* FunctionBody.phi(body, i32, 'result')
        yield* FunctionBody.addPhiIncoming(body, phi, added, onTrue)
        yield* FunctionBody.addPhiIncoming(body, phi, subtracted, onFalse)
        yield* FunctionBody.returnValue(body, yield* FunctionBody.sealPhi(body, phi))
      }),
    )

    const text = yield* IrText.render(builder)
    expect(text).toContain('define i32 @diamond(i32 %v0, i32 %v1)')
    expect(text).toContain('br i1 %condition, label %on_true, label %on_false')
    expect(text).toContain('%added = call i32 @callee(i32 %v0, i32 %v1)')
    expect(text).toContain('%result = phi i32 [ %added, %on_true ], [ %subtracted, %on_false ]')
  }, TestRuntime.runPromise),
)

test(
  'builds and finalizes switches with unique cases',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const i32 = yield* Type.integer(builder, 32)
    const type = yield* Type.functionType(builder, i32, [i32])
    const fn = yield* FunctionActor.declare(builder, 'choose', type)
    const zero = yield* Constant.integerUnsigned(builder, i32, 0)
    const one = yield* Constant.integerUnsigned(builder, i32, 1)

    yield* FunctionActor.buildBody(
      builder,
      fn,
      Effect.fnUntraced(function* (body) {
        const entry = yield* Block.make(body, 'entry')
        const fallback = yield* Block.make(body, 'fallback')
        const selected = yield* Block.make(body, 'selected')
        yield* Block.setInsertionPoint(body, entry)
        const argument = yield* Value.argument(body, 0)
        const switchHandle = yield* FunctionBody.switchTerminator(body, argument, fallback)
        yield* FunctionBody.addSwitchCase(body, switchHandle, zero, selected)
        const duplicate = yield* Effect.flip(
          FunctionBody.addSwitchCase(body, switchHandle, zero, fallback),
        )
        expect(duplicate.message).toContain('unique')
        yield* FunctionBody.sealSwitch(body, switchHandle)
        yield* Block.setInsertionPoint(body, fallback)
        yield* FunctionBody.returnValue(body, one)
        yield* Block.setInsertionPoint(body, selected)
        yield* FunctionBody.returnValue(body, zero)
      }),
    )

    expect(yield* IrText.render(builder)).toContain('switch i32 %v0, label %fallback')
  }, TestRuntime.runPromise),
)

test(
  'resolves a loop-carried forward value and validates complete phi coverage',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const i32 = yield* Type.integer(builder, 32)
    const type = yield* Type.functionType(builder, i32, [i32])
    const fn = yield* FunctionActor.declare(builder, 'loop', type)
    const one = yield* Constant.integerUnsigned(builder, i32, 1)
    const ten = yield* Constant.integerUnsigned(builder, i32, 10)

    yield* FunctionActor.buildBody(
      builder,
      fn,
      Effect.fnUntraced(function* (body) {
        const entry = yield* Block.make(body, 'entry')
        const loop = yield* Block.make(body, 'loop')
        const exit = yield* Block.make(body, 'exit')
        yield* Block.setInsertionPoint(body, entry)
        const initial = yield* Value.argument(body, 0)
        yield* FunctionBody.branch(body, loop)

        yield* Block.setInsertionPoint(body, loop)
        const nextForward = yield* Value.forward(body, i32, 'next_forward')
        const phi = yield* FunctionBody.phi(body, i32, 'current')
        yield* FunctionBody.addPhiIncoming(body, phi, initial, entry)
        yield* FunctionBody.addPhiIncoming(body, phi, nextForward, loop)
        const current = yield* FunctionBody.phiValue(body, phi)
        const next = yield* FunctionBody.binary(body, 'add', current, one, 'next')
        yield* Value.resolveForward(body, nextForward, next)
        const keepGoing = yield* FunctionBody.integerCompare(body, 'slt', next, ten, 'keep_going')
        yield* FunctionBody.conditionalBranch(body, keepGoing, loop, exit)
        yield* FunctionBody.sealPhi(body, phi)

        yield* Block.setInsertionPoint(body, exit)
        yield* FunctionBody.returnValue(body, next)
      }),
    )

    const text = yield* IrText.render(builder)
    expect(text).toContain('%current = phi i32 [ %v0, %entry ], [ %next, %loop ]')
  }, TestRuntime.runPromise),
)

test(
  'validates vararg calls and preserves call settings',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const i32 = yield* Type.integer(builder, 32)
    const calleeType = yield* Type.functionType(builder, i32, [i32], { variadic: true })
    const nounwind = yield* Attribute.flag(builder, 'nounwind')
    const attributes = yield* Attribute.functionSet(builder, {
      functionAttributes: yield* Attribute.set(builder, [nounwind]),
    })
    const callee = yield* FunctionActor.declare(builder, 'variadic', calleeType, { attributes })
    const callerType = yield* Type.functionType(builder, i32, [i32])
    const caller = yield* FunctionActor.declare(builder, 'call_variadic', callerType)
    const one = yield* Constant.integerUnsigned(builder, i32, 1)

    yield* FunctionActor.buildBody(
      builder,
      caller,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const argument = yield* Value.argument(body, 0)
        const invalid = yield* Effect.flip(FunctionBody.callDirect(body, callee, []))
        expect(invalid.message).toContain('argument count')
        const invalidConvention = yield* Effect.flip(
          FunctionBody.callDirect(body, callee, [argument], undefined, {
            callingConvention: -1,
          }),
        )
        expect(invalidConvention.message).toContain('unsigned 10-bit')
        const result = yield* FunctionBody.callDirect(body, callee, [argument, one], 'result', {
          tail: 'tail',
        })
        if (result === undefined) {
          return yield* Effect.fail(
            new SilkError({ operation: 'test', message: 'expected call result', cause: callee }),
          )
        }
        yield* FunctionBody.returnValue(body, result)
      }),
    )

    expect(yield* IrText.render(builder)).toContain(
      '%result = tail call i32 @variadic(i32 %v0, i32 1) nounwind',
    )
  }, TestRuntime.runPromise),
)
