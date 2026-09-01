import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'
import * as AddrSpace from '../dist/AddrSpace.js'
import * as Attribute from '../dist/Attribute.js'
import * as Bitcode from '../dist/Bitcode.js'
import * as Block from '../dist/Block.js'
import * as Builder from '../dist/Builder.js'
import * as Constant from '../dist/Constant.js'
import * as FunctionActor from '../dist/Function.js'
import * as FunctionBody from '../dist/FunctionBody.js'
import * as IrText from '../dist/IrText.js'
import * as Type from '../dist/Type.js'
import * as Value from '../dist/Value.js'

const stopAfter = Effect.runSync(Config.string('CORE_STOP_AFTER').pipe(Config.withDefault('')))
const floatBinaryCount = Effect.runSync(
  Config.integer('CORE_FLOAT_BINARY_COUNT').pipe(Config.withDefault(5)),
)
const floatCompareCount = Effect.runSync(
  Config.integer('CORE_FLOAT_COMPARE_COUNT').pipe(Config.withDefault(15)),
)

const output = await Effect.runPromise(
  Effect.gen(function* () {
    const builder = yield* Builder.make({
      sourceFilename: 'core.ll',
      dataLayout: 'e-p:64:64-i32:32',
      targetTriple: 'aarch64-unknown-linux',
    })
    const finish = Effect.fnUntraced(function* () {
      return process.argv[2] === 'bitcode'
        ? Buffer.from(yield* Bitcode.encode(builder))
        : Buffer.from(yield* IrText.render(builder))
    })
    const i32 = yield* Type.integer(builder, 32)
    const type = yield* Type.functionType(builder, i32, [i32, i32])
    const fn = yield* FunctionActor.declare(builder, 'max_plus_one', type)
    yield* FunctionActor.buildBody(
      builder,
      fn,
      Effect.fnUntraced(function* (body) {
        const entry = yield* Block.make(body, 'entry')
        const greater = yield* Block.make(body, 'greater')
        const lesser = yield* Block.make(body, 'lesser')
        const merge = yield* Block.make(body, 'merge')
        yield* Block.setInsertionPoint(body, entry)
        const left = yield* Value.argument(body, 0)
        const right = yield* Value.argument(body, 1)
        const condition = yield* FunctionBody.integerCompare(body, 'sgt', left, right, 'condition')
        yield* FunctionBody.conditionalBranch(body, condition, greater, lesser)
        yield* Block.setInsertionPoint(body, greater)
        const increasedLeft = yield* FunctionBody.binary(body, 'add', left, right, 'increased_left')
        yield* FunctionBody.branch(body, merge)
        yield* Block.setInsertionPoint(body, lesser)
        const increasedRight = yield* FunctionBody.binary(
          body,
          'sub',
          right,
          left,
          'increased_right',
        )
        yield* FunctionBody.branch(body, merge)
        yield* Block.setInsertionPoint(body, merge)
        const phi = yield* FunctionBody.phi(body, i32, 'result')
        yield* FunctionBody.addPhiIncoming(body, phi, increasedLeft, greater)
        yield* FunctionBody.addPhiIncoming(body, phi, increasedRight, lesser)
        yield* FunctionBody.returnValue(body, yield* FunctionBody.sealPhi(body, phi))
      }),
    )
    const chooseType = yield* Type.functionType(builder, i32, [i32])
    const choose = yield* FunctionActor.declare(builder, 'choose', chooseType)
    const zero = yield* Constant.integerUnsigned(builder, i32, 0)
    const one = yield* Constant.integerUnsigned(builder, i32, 1)
    yield* FunctionActor.buildBody(
      builder,
      choose,
      Effect.fnUntraced(function* (body) {
        const entry = yield* Block.make(body, 'entry')
        const fallback = yield* Block.make(body, 'fallback')
        const selected = yield* Block.make(body, 'selected')
        yield* Block.setInsertionPoint(body, entry)
        const argument = yield* Value.argument(body, 0)
        const switchHandle = yield* FunctionBody.switchTerminator(body, argument, fallback)
        yield* FunctionBody.addSwitchCase(body, switchHandle, zero, selected)
        yield* FunctionBody.sealSwitch(body, switchHandle)
        yield* Block.setInsertionPoint(body, fallback)
        yield* FunctionBody.returnValue(body, one)
        yield* Block.setInsertionPoint(body, selected)
        yield* FunctionBody.returnValue(body, zero)
      }),
    )
    const loop = yield* FunctionActor.declare(builder, 'count_up', chooseType)
    const ten = yield* Constant.integerUnsigned(builder, i32, 10)
    yield* FunctionActor.buildBody(
      builder,
      loop,
      Effect.fnUntraced(function* (body) {
        const entry = yield* Block.make(body, 'entry')
        const repeat = yield* Block.make(body, 'repeat')
        const exit = yield* Block.make(body, 'exit')
        yield* Block.setInsertionPoint(body, entry)
        const initial = yield* Value.argument(body, 0)
        yield* FunctionBody.branch(body, repeat)
        yield* Block.setInsertionPoint(body, repeat)
        const nextForward = yield* Value.forward(body, i32, 'next_forward')
        const phi = yield* FunctionBody.phi(body, i32, 'current')
        yield* FunctionBody.addPhiIncoming(body, phi, initial, entry)
        yield* FunctionBody.addPhiIncoming(body, phi, nextForward, repeat)
        const current = yield* FunctionBody.phiValue(body, phi)
        const next = yield* FunctionBody.binary(body, 'add', current, one, 'next')
        yield* Value.resolveForward(body, nextForward, next)
        const condition = yield* FunctionBody.integerCompare(body, 'slt', next, ten, 'condition')
        yield* FunctionBody.conditionalBranch(body, condition, repeat, exit)
        yield* FunctionBody.sealPhi(body, phi)
        yield* Block.setInsertionPoint(body, exit)
        yield* FunctionBody.returnValue(body, next)
      }),
    )
    if (stopAfter === 'cfg') return yield* finish()
    const integerOps = yield* FunctionActor.declare(builder, 'integer_ops', type)
    yield* FunctionActor.buildBody(
      builder,
      integerOps,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const left = yield* Value.argument(body, 0)
        const right = yield* Value.argument(body, 1)
        let result = left
        for (const kind of [
          'add',
          'sub',
          'mul',
          'udiv',
          'sdiv',
          'urem',
          'srem',
          'shl',
          'lshr',
          'ashr',
          'and',
          'or',
          'xor',
        ]) {
          result = yield* FunctionBody.binary(body, kind, result, right, kind)
        }
        result = yield* FunctionBody.negate(body, result, 'negated')
        result = yield* FunctionBody.bitwiseNot(body, result, 'inverted')
        let condition = yield* FunctionBody.integerCompare(body, 'eq', left, right, 'eq')
        for (const predicate of ['ne', 'ugt', 'uge', 'ult', 'ule', 'sgt', 'sge', 'slt', 'sle']) {
          condition = yield* FunctionBody.integerCompare(body, predicate, left, right, predicate)
        }
        result = yield* FunctionBody.select(body, condition, result, left, 'selected')
        yield* FunctionBody.returnValue(body, result)
      }),
    )
    if (stopAfter === 'integer') return yield* finish()
    const float = yield* Type.float(builder)
    const floatType = yield* Type.functionType(builder, float, [float, float])
    const floatingOps = yield* FunctionActor.declare(builder, 'floating_ops', floatType)
    yield* FunctionActor.buildBody(
      builder,
      floatingOps,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const left = yield* Value.argument(body, 0)
        const right = yield* Value.argument(body, 1)
        let result = yield* FunctionBody.unary(body, 'fneg', left, 'negated', { fastMath: true })
        for (const kind of ['fadd', 'fsub', 'fmul', 'fdiv', 'frem'].slice(0, floatBinaryCount)) {
          result = yield* FunctionBody.binary(body, kind, result, right, kind, { fastMath: true })
        }
        let condition = yield* FunctionBody.floatingCompare(body, 'false', left, right, 'false')
        for (const predicate of [
          'oeq',
          'ogt',
          'oge',
          'olt',
          'ole',
          'one',
          'ord',
          'ueq',
          'ugt',
          'uge',
          'ult',
          'ule',
          'une',
          'uno',
          'true',
        ].slice(0, floatCompareCount)) {
          condition = yield* FunctionBody.floatingCompare(body, predicate, left, right, predicate, {
            fastMath: true,
          })
        }
        result = yield* FunctionBody.select(body, condition, result, left, 'selected', {
          fastMath: true,
        })
        yield* FunctionBody.returnValue(body, result)
      }),
    )
    if (stopAfter === 'floating') return yield* finish()
    const i16 = yield* Type.integer(builder, 16)
    const i64 = yield* Type.integer(builder, 64)
    const double = yield* Type.double(builder)
    const pointer = yield* Type.pointer(builder)
    const pointerOne = yield* Type.pointer(builder, yield* AddrSpace.make(1))
    const castType = yield* Type.functionType(builder, i32, [i32, i16, i64, float, double, pointer])
    const casts = yield* FunctionActor.declare(builder, 'casts', castType)
    yield* FunctionActor.buildBody(
      builder,
      casts,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const int32 = yield* Value.argument(body, 0)
        const int16 = yield* Value.argument(body, 1)
        const int64 = yield* Value.argument(body, 2)
        const floatValue = yield* Value.argument(body, 3)
        const doubleValue = yield* Value.argument(body, 4)
        const pointerValue = yield* Value.argument(body, 5)
        yield* FunctionBody.cast(body, 'trunc', int32, i16, 'truncated')
        yield* FunctionBody.cast(body, 'zext', int16, i32, 'zero_extended')
        yield* FunctionBody.cast(body, 'sext', int16, i32, 'sign_extended')
        yield* FunctionBody.cast(body, 'fptoui', floatValue, i32, 'float_unsigned')
        yield* FunctionBody.cast(body, 'fptosi', floatValue, i32, 'float_signed')
        yield* FunctionBody.cast(body, 'uitofp', int32, float, 'unsigned_float')
        yield* FunctionBody.cast(body, 'sitofp', int32, float, 'signed_float')
        yield* FunctionBody.cast(body, 'fptrunc', doubleValue, float, 'float_truncated')
        yield* FunctionBody.cast(body, 'fpext', floatValue, double, 'float_extended')
        yield* FunctionBody.cast(body, 'ptrtoint', pointerValue, i64, 'pointer_integer')
        yield* FunctionBody.cast(body, 'inttoptr', int64, pointer, 'integer_pointer')
        yield* FunctionBody.cast(body, 'bitcast', int32, float, 'bit_cast')
        yield* FunctionBody.cast(body, 'addrspacecast', pointerValue, pointerOne, 'address_cast')
        yield* FunctionBody.returnValue(body, int32)
      }),
    )
    if (stopAfter === 'casts') return yield* finish()
    const pair = yield* Type.structure(builder, [i32, i64])
    const aggregateType = yield* Type.functionType(builder, i64, [i32, i64])
    const aggregateOps = yield* FunctionActor.declare(builder, 'aggregate_ops', aggregateType)
    const emptyPair = yield* Constant.poison(builder, pair)
    yield* FunctionActor.buildBody(
      builder,
      aggregateOps,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const first = yield* Value.argument(body, 0)
        const second = yield* Value.argument(body, 1)
        const withFirst = yield* FunctionBody.insertValue(body, emptyPair, first, [0], 'with_first')
        const complete = yield* FunctionBody.insertValue(body, withFirst, second, [1], 'complete')
        const extracted = yield* FunctionBody.extractValue(body, complete, [1], 'extracted')
        yield* FunctionBody.returnValue(body, extracted)
      }),
    )
    if (stopAfter === 'aggregate') return yield* finish()
    const voidType = yield* Type.voidType(builder)
    const voidFunctionType = yield* Type.functionType(builder, voidType, [])
    const noop = yield* FunctionActor.declare(builder, 'noop', voidFunctionType)
    yield* FunctionActor.buildBody(
      builder,
      noop,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        yield* FunctionBody.returnVoid(body)
      }),
    )
    const trap = yield* FunctionActor.declare(builder, 'trap', voidFunctionType)
    yield* FunctionActor.buildBody(
      builder,
      trap,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        yield* FunctionBody.unreachable(body)
      }),
    )
    const caller = yield* FunctionActor.declare(builder, 'call_max', type)
    yield* FunctionActor.buildBody(
      builder,
      caller,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const left = yield* Value.argument(body, 0)
        const right = yield* Value.argument(body, 1)
        const result = yield* FunctionBody.callDirect(body, fn, [left, right], 'result', {
          operandBundles: [{ tag: 'cold', operands: [] }],
        })
        if (result === undefined) return yield* Effect.die('expected a call result')
        yield* FunctionBody.returnValue(body, result)
      }),
    )
    for (const tail of ['tail', 'musttail', 'notail']) {
      const tailCaller = yield* FunctionActor.declare(builder, `${tail}_call`, type)
      yield* FunctionActor.buildBody(
        builder,
        tailCaller,
        Effect.fnUntraced(function* (body) {
          yield* Block.make(body, 'entry')
          const left = yield* Value.argument(body, 0)
          const right = yield* Value.argument(body, 1)
          const result = yield* FunctionBody.callDirect(body, fn, [left, right], 'result', { tail })
          if (result === undefined) return yield* Effect.die('expected a call result')
          yield* FunctionBody.returnValue(body, result)
        }),
      )
    }
    const fastCaller = yield* FunctionActor.declare(builder, 'fast_call', floatType)
    yield* FunctionActor.buildBody(
      builder,
      fastCaller,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const left = yield* Value.argument(body, 0)
        const right = yield* Value.argument(body, 1)
        const result = yield* FunctionBody.callDirect(body, floatingOps, [left, right], 'result', {
          fastMath: true,
        })
        if (result === undefined) return yield* Effect.die('expected a call result')
        yield* FunctionBody.returnValue(body, result)
      }),
    )
    const indirectType = yield* Type.functionType(builder, i32, [pointer, i32, i32])
    const indirect = yield* FunctionActor.declare(builder, 'indirect_call', indirectType)
    yield* FunctionActor.buildBody(
      builder,
      indirect,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const callee = yield* Value.argument(body, 0)
        const left = yield* Value.argument(body, 1)
        const right = yield* Value.argument(body, 2)
        const result = yield* FunctionBody.call(body, type, callee, [left, right], 'result')
        if (result === undefined) return yield* Effect.die('expected a call result')
        yield* FunctionBody.returnValue(body, result)
      }),
    )
    const variadicType = yield* Type.functionType(builder, i32, [i32], { variadic: true })
    const nounwind = yield* Attribute.flag(builder, 'nounwind')
    const variadicAttributes = yield* Attribute.functionSet(builder, {
      functionAttributes: yield* Attribute.set(builder, [nounwind]),
    })
    const variadic = yield* FunctionActor.declare(builder, 'variadic', variadicType, {
      attributes: variadicAttributes,
    })
    const variadicCaller = yield* FunctionActor.declare(builder, 'variadic_call', chooseType)
    yield* FunctionActor.buildBody(
      builder,
      variadicCaller,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const value = yield* Value.argument(body, 0)
        const result = yield* FunctionBody.callDirect(body, variadic, [value, one], 'result')
        if (result === undefined) return yield* Effect.die('expected a call result')
        yield* FunctionBody.returnValue(body, result)
      }),
    )
    return yield* finish()
  }),
)

process.stdout.write(output)
