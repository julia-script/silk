import * as LlvmBlock from '@silk-effect/llvm/Block'
import * as Constant from '@silk-effect/llvm/Constant'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import * as Intrinsic from '@silk-effect/llvm/Intrinsic'
import * as LlvmType from '@silk-effect/llvm/Type'
import * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'
import * as Mir from './Mir.js'
import type { LinearOperation } from './MirLinearization.js'
import * as NativeArith from './NativeArith.js'
import * as NativeDebug from './NativeDebug.js'
import type { Context } from './NativeOperationContext.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeTranscendental from './NativeTranscendental.js'
import * as NativeType from './NativeType.js'
import * as Scalar from './Scalar.js'
import * as SilkType from './Type.js'

type Operation = Extract<
  LinearOperation,
  {
    readonly _tag:
      | 'ConvertInteger'
      | 'ConvertScalar'
      | 'ReinterpretScalar'
      | 'FloatUnary'
      | 'FloatTranscendental'
      | 'CheckedScalar'
      | 'Binary'
  }
>

export const emit = Effect.fnUntraced(function* (context: Context, operation: Operation) {
  const {
    body,
    builder,
    arith,
    debug,
    entry,
    f32,
    f64,
    i32,
    integerTypes,
    program,
    signedOverflowSignatures,
    storage: nativeStorage,
    types,
    unsignedOverflowSignatures,
  } = context
  const initialTrapBlock = context.state.trapBlock
  let trapBlock = initialTrapBlock
  let checkOrdinal = context.state.checkOrdinal
  switch (operation._tag) {
    case 'ConvertInteger': {
      const result = yield* NativeArith.emitIntegerConversion(
        arith,
        NativeStorage.readScalar(nativeStorage, operation.source),
        operation.sourceType,
        operation.type,
        `convert${operation.destination.ordinal}`,
      )
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
      break
    }
    case 'ConvertScalar': {
      const source = Scalar.find(operation.sourceType._tag)
      const target = Scalar.find(operation.type._tag)
      if (
        source === undefined ||
        target === undefined ||
        source.category === 'Boolean' ||
        target.category === 'Boolean'
      )
        throw new RangeError('LLVM scalar conversion lost its types')
      const sourceValue = NativeStorage.readScalar(nativeStorage, operation.source)
      if (source.category === 'Character' && target.spelling === 'u32') {
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([sourceValue]))
        break
      }
      let destinationType: LlvmType.Type
      if (target.category === 'Floating') {
        if (target.spelling === 'f32') {
          destinationType = f32
        } else {
          destinationType = f64
        }
      } else {
        destinationType =
          integerTypes.get(
            Scalar.bits(target, program.layout.target.pointerSize === 4 ? 32 : 64),
          ) ?? i32
      }
      let kind: FunctionBody.CastKind
      if (source.category === 'Floating' && target.category === 'Floating') {
        if (source.spelling === 'f64') {
          kind = 'fptrunc'
        } else {
          kind = 'fpext'
        }
      } else if (source.category === 'Floating' && target.category === 'Integer') {
        if (target.signedness === 'Signed') {
          kind = 'fptosi'
        } else {
          kind = 'fptoui'
        }
      } else if (source.category === 'Integer' && target.category === 'Floating') {
        if (source.signedness === 'Signed') {
          kind = 'sitofp'
        } else {
          kind = 'uitofp'
        }
      } else {
        throw new RangeError('LLVM scalar conversion was not numeric')
      }
      const result = yield* FunctionBody.cast(
        body,
        kind,
        sourceValue,
        destinationType,
        `convert${operation.destination.ordinal}`,
      )
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
      break
    }
    case 'ReinterpretScalar': {
      const targetLane = NativeType.lanesFor(types, operation.type).at(0)
      if (targetLane === undefined) throw new RangeError('LLVM reinterpretation lost its lane')
      const result = yield* FunctionBody.cast(
        body,
        'bitcast',
        NativeStorage.readScalar(nativeStorage, operation.source),
        NativeType.laneType(types, targetLane),
        `reinterpret${operation.destination.ordinal}`,
      )
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
      break
    }
    case 'FloatUnary': {
      const source = Scalar.find(operation.sourceType._tag)
      if (source?.category !== 'Floating')
        throw new RangeError('LLVM float unary lost its source type')
      const subject = NativeStorage.readScalar(nativeStorage, operation.source)
      if (operation.operation === 'Negate') {
        const result = yield* FunctionBody.unary(
          body,
          'fneg',
          subject,
          `fneg${operation.destination.ordinal}`,
        )
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
        break
      }
      if (operation.operation === 'Sqrt') {
        // IEEE-754 mandates a correctly rounded square root, so `llvm.sqrt` is
        // bit-exact on every conforming target and matches the evaluator exactly.
        const floatType = source.spelling === 'f32' ? f32 : f64
        const signature = Object.freeze({
          returnType: floatType,
          parameters: Object.freeze([floatType]),
        })
        const result = yield* Intrinsic.call(
          body,
          'sqrt',
          [floatType],
          [subject],
          `sqrt${operation.destination.ordinal}`,
          { signature },
        )
        if (result === undefined) throw new RangeError('LLVM square root produced no value')
        yield* NativeDebug.locate(
          debug,
          operation.provenance.span,
          yield* Value.instruction(body, result),
        )
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
        break
      }
      const width = source.spelling === 'f32' ? 32 : 64
      const integerType = integerTypes.get(width) ?? i32
      const raw = yield* FunctionBody.cast(
        body,
        'bitcast',
        subject,
        integerType,
        `floatbits${operation.destination.ordinal}`,
      )
      const fractionBits = source.spelling === 'f32' ? 23 : 52
      const exponentBits = source.spelling === 'f32' ? 8 : 11
      const exponentMask = ((1n << BigInt(exponentBits)) - 1n) << BigInt(fractionBits)
      const fractionMask = (1n << BigInt(fractionBits)) - 1n
      const zero = yield* Constant.integerUnsigned(builder, integerType, 0n)
      const exponentMaskValue = yield* Constant.integerUnsigned(builder, integerType, exponentMask)
      const fractionMaskValue = yield* Constant.integerUnsigned(builder, integerType, fractionMask)
      const exponent = yield* FunctionBody.binary(
        body,
        'and',
        raw,
        exponentMaskValue,
        `fclass_exp${operation.destination.ordinal}`,
      )
      const fraction = yield* FunctionBody.binary(
        body,
        'and',
        raw,
        fractionMaskValue,
        `fclass_frac${operation.destination.ordinal}`,
      )
      const exponentZero = yield* FunctionBody.integerCompare(
        body,
        'eq',
        exponent,
        zero,
        `fclass_exp_zero${operation.destination.ordinal}`,
      )
      const exponentAll = yield* FunctionBody.integerCompare(
        body,
        'eq',
        exponent,
        exponentMaskValue,
        `fclass_exp_all${operation.destination.ordinal}`,
      )
      const fractionZero = yield* FunctionBody.integerCompare(
        body,
        'eq',
        fraction,
        zero,
        `fclass_frac_zero${operation.destination.ordinal}`,
      )
      let flag: Value.Input
      if (operation.operation === 'IsSignNegative') {
        flag = yield* FunctionBody.integerCompare(
          body,
          'slt',
          raw,
          zero,
          `fclass_sign${operation.destination.ordinal}`,
        )
      } else if (operation.operation === 'IsNaN') {
        const fractionNonzero = yield* FunctionBody.integerCompare(
          body,
          'ne',
          fraction,
          zero,
          `fclass_frac_nonzero${operation.destination.ordinal}`,
        )
        flag = yield* FunctionBody.binary(
          body,
          'and',
          exponentAll,
          fractionNonzero,
          `fclass_nan${operation.destination.ordinal}`,
        )
      } else if (operation.operation === 'IsInfinite') {
        flag = yield* FunctionBody.binary(
          body,
          'and',
          exponentAll,
          fractionZero,
          `fclass_inf${operation.destination.ordinal}`,
        )
      } else if (operation.operation === 'IsFinite') {
        flag = yield* FunctionBody.integerCompare(
          body,
          'ne',
          exponent,
          exponentMaskValue,
          `fclass_finite${operation.destination.ordinal}`,
        )
      } else if (operation.operation === 'IsNormal') {
        const nonzero = yield* FunctionBody.integerCompare(
          body,
          'ne',
          exponent,
          zero,
          `fclass_nonzero${operation.destination.ordinal}`,
        )
        const finite = yield* FunctionBody.integerCompare(
          body,
          'ne',
          exponent,
          exponentMaskValue,
          `fclass_notall${operation.destination.ordinal}`,
        )
        flag = yield* FunctionBody.binary(
          body,
          'and',
          nonzero,
          finite,
          `fclass_normal${operation.destination.ordinal}`,
        )
      } else {
        const fractionNonzero = yield* FunctionBody.integerCompare(
          body,
          'ne',
          fraction,
          zero,
          `fclass_sub_frac${operation.destination.ordinal}`,
        )
        flag = yield* FunctionBody.binary(
          body,
          'and',
          exponentZero,
          fractionNonzero,
          `fclass_sub${operation.destination.ordinal}`,
        )
      }
      const result = yield* FunctionBody.cast(
        body,
        'zext',
        flag,
        i32,
        `fclass${operation.destination.ordinal}`,
      )
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
      break
    }
    case 'FloatTranscendental': {
      const i64Type = integerTypes.get(64)
      const result = yield* NativeTranscendental.emit(
        { builder, i32, ...(i64Type === undefined ? {} : { i64: i64Type }), f32, f64 },
        body,
        operation,
        NativeStorage.readScalar(nativeStorage, operation.source),
      )
      yield* NativeDebug.locate(
        debug,
        operation.provenance.span,
        yield* Value.instruction(body, result),
      )
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
      break
    }
    case 'CheckedScalar': {
      const leftLocal = operation.operands.at(0)
      const rightLocal = operation.operands.at(1)
      const source = Scalar.find(operation.sourceType._tag)
      const target = Scalar.find(operation.valueType._tag)
      const characterConversion =
        operation.operation === 'CheckedConvertToChar' &&
        source?.spelling === 'u32' &&
        target?.category === 'Character'
      if (
        leftLocal === undefined ||
        source?.category !== 'Integer' ||
        (target?.category !== 'Integer' && !characterConversion)
      )
        throw new RangeError('LLVM checked scalar operation lost its scalar types')
      const left = NativeStorage.readScalar(nativeStorage, leftLocal)
      const right =
        rightLocal === undefined ? undefined : NativeStorage.readScalar(nativeStorage, rightLocal)
      const pointerBits = program.layout.target.pointerSize === 4 ? 32 : 64
      const sourceBits = Scalar.bits(source, pointerBits)
      const targetBits = Scalar.bits(target, pointerBits)
      const sourcePhysical = integerTypes.get(sourceBits) ?? i32
      const targetPhysical = integerTypes.get(targetBits) ?? i32
      const name = `checked${operation.destination.ordinal}`
      let result: Value.Input
      let invalid: Value.Input
      if (characterConversion) {
        const maximum = yield* Constant.integerUnsigned(builder, sourcePhysical, 0x10ffffn)
        const surrogateMinimum = yield* Constant.integerUnsigned(builder, sourcePhysical, 0xd800n)
        const surrogateMaximum = yield* Constant.integerUnsigned(builder, sourcePhysical, 0xdfffn)
        const aboveMaximum = yield* FunctionBody.integerCompare(
          body,
          'ugt',
          left,
          maximum,
          `${name}_above`,
        )
        const atLeastSurrogate = yield* FunctionBody.integerCompare(
          body,
          'uge',
          left,
          surrogateMinimum,
          `${name}_surrogate_minimum`,
        )
        const atMostSurrogate = yield* FunctionBody.integerCompare(
          body,
          'ule',
          left,
          surrogateMaximum,
          `${name}_surrogate_maximum`,
        )
        const surrogate = yield* FunctionBody.binary(
          body,
          'and',
          atLeastSurrogate,
          atMostSurrogate,
          `${name}_surrogate`,
        )
        invalid = yield* FunctionBody.binary(body, 'or', aboveMaximum, surrogate, `${name}_invalid`)
        result = left
      } else if (operation.operation.startsWith('CheckedConvertTo')) {
        if (target.category !== 'Integer')
          throw new RangeError('LLVM checked conversion lost its integer target')
        const sourceRange = Scalar.range(source, pointerBits)
        const targetRange = Scalar.range(target, pointerBits)
        const checks: Array<Value.Input> = []
        if (targetRange.minimum > sourceRange.minimum)
          checks.push(
            yield* FunctionBody.integerCompare(
              body,
              source.signedness === 'Signed' ? 'slt' : 'ult',
              left,
              source.signedness === 'Signed'
                ? yield* Constant.integerSigned(builder, sourcePhysical, targetRange.minimum)
                : yield* Constant.integerUnsigned(builder, sourcePhysical, targetRange.minimum),
              `${name}_below`,
            ),
          )
        if (targetRange.maximum < sourceRange.maximum)
          checks.push(
            yield* FunctionBody.integerCompare(
              body,
              source.signedness === 'Signed' ? 'sgt' : 'ugt',
              left,
              source.signedness === 'Signed'
                ? yield* Constant.integerSigned(builder, sourcePhysical, targetRange.maximum)
                : yield* Constant.integerUnsigned(builder, sourcePhysical, targetRange.maximum),
              `${name}_above`,
            ),
          )
        invalid =
          checks.at(0) ??
          (yield* Constant.integerUnsigned(builder, yield* LlvmType.integer(builder, 1), 0n))
        for (const [ordinal, check] of checks.slice(1).entries())
          invalid = yield* FunctionBody.binary(
            body,
            'or',
            invalid,
            check,
            `${name}_invalid${ordinal}`,
          )
        if (sourceBits === targetBits) {
          result = left
        } else {
          const extension = source.signedness === 'Signed' ? 'sext' : 'zext'
          result = yield* FunctionBody.cast(
            body,
            sourceBits < targetBits ? extension : 'trunc',
            left,
            targetPhysical,
            `${name}_value`,
          )
        }
      } else if (
        operation.operation === 'CheckedAdd' ||
        operation.operation === 'CheckedSubtract' ||
        operation.operation === 'CheckedMultiply'
      ) {
        if (target.category !== 'Integer')
          throw new RangeError('LLVM checked arithmetic lost its integer target')
        if (right === undefined)
          throw new RangeError('LLVM checked arithmetic lost its right operand')
        const signatures =
          target.signedness === 'Unsigned' ? unsignedOverflowSignatures : signedOverflowSignatures
        let signature = signatures.get(targetBits)
        if (signature === undefined) {
          const i1 = yield* LlvmType.integer(builder, 1)
          signature = Object.freeze({
            returnType: yield* LlvmType.structure(builder, [targetPhysical, i1]),
            parameters: Object.freeze([targetPhysical, targetPhysical]),
          })
          signatures.set(targetBits, signature)
        }
        let stem: 'add' | 'sub' | 'mul'
        switch (operation.operation) {
          case 'CheckedAdd':
            stem = 'add'
            break
          case 'CheckedSubtract':
            stem = 'sub'
            break
          case 'CheckedMultiply':
            stem = 'mul'
            break
        }
        const pair = yield* Intrinsic.call(
          body,
          `${target.signedness === 'Unsigned' ? 'u' : 's'}${stem}.with.overflow`,
          [targetPhysical],
          [left, right],
          `${name}_pair`,
          { signature },
        )
        if (pair === undefined) throw new RangeError('LLVM checked arithmetic produced no outcome')
        result = yield* FunctionBody.extractValue(body, pair, [0], `${name}_value`)
        invalid = yield* FunctionBody.extractValue(body, pair, [1], `${name}_invalid`)
      } else if (target.category === 'Integer') {
        if (right === undefined)
          throw new RangeError('LLVM checked division lost its right operand')
        const zero = yield* Constant.integerUnsigned(builder, targetPhysical, 0n)
        invalid = yield* FunctionBody.integerCompare(body, 'eq', right, zero, `${name}_zero`)
        if (target.signedness === 'Signed' && operation.operation === 'CheckedDivide') {
          const range = Scalar.range(target, pointerBits)
          const minimum = yield* Constant.integerSigned(builder, targetPhysical, range.minimum)
          const negativeOne = yield* Constant.integerSigned(builder, targetPhysical, -1n)
          const minimumDividend = yield* FunctionBody.integerCompare(
            body,
            'eq',
            left,
            minimum,
            `${name}_minimum`,
          )
          const negativeDivisor = yield* FunctionBody.integerCompare(
            body,
            'eq',
            right,
            negativeOne,
            `${name}_negative_one`,
          )
          const overflow = yield* FunctionBody.binary(
            body,
            'and',
            minimumDividend,
            negativeDivisor,
            `${name}_overflow`,
          )
          invalid = yield* FunctionBody.binary(body, 'or', invalid, overflow, `${name}_invalid`)
        }
        const one = yield* Constant.integerUnsigned(builder, targetPhysical, 1n)
        const safeRight = yield* FunctionBody.select(body, invalid, one, right, `${name}_divisor`)
        let opcode: 'udiv' | 'sdiv' | 'urem' | 'srem'
        if (operation.operation === 'CheckedDivide')
          opcode = target.signedness === 'Unsigned' ? 'udiv' : 'sdiv'
        else opcode = target.signedness === 'Unsigned' ? 'urem' : 'srem'
        result = yield* FunctionBody.binary(body, opcode, left, safeRight, `${name}_value`)
      } else {
        throw new RangeError('LLVM checked division lost its integer target')
      }
      const successOrdinal = operation.type.type.members.findIndex((member) =>
        SilkType.equals(member, operation.success),
      )
      const failureOrdinal = operation.type.type.members.findIndex((member) =>
        SilkType.equals(member, operation.failure),
      )
      if (successOrdinal < 0 || failureOrdinal < 0)
        throw new RangeError('LLVM checked scalar operation lost its Option members')
      const successTag = yield* Constant.integerSigned(builder, i32, BigInt(successOrdinal))
      const failureTag = yield* Constant.integerSigned(builder, i32, BigInt(failureOrdinal))
      const tag = yield* FunctionBody.select(body, invalid, failureTag, successTag, `${name}_tag`)
      const valueLane = NativeType.lanesFor(types, operation.valueType).at(0)
      const payloadLane = NativeType.lanesFor(types, operation.type).at(1)
      if (valueLane === undefined || payloadLane === undefined)
        throw new RangeError('LLVM checked scalar operation lost its payload lane')
      const payload = yield* NativeArith.coerceLane(
        arith.lane,
        result,
        valueLane,
        payloadLane,
        `${name}_payload`,
      )
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([tag, payload]))
      break
    }
    case 'Binary': {
      const left = NativeStorage.readScalar(nativeStorage, operation.left)
      const right = NativeStorage.readScalar(nativeStorage, operation.right)
      const leftType = entry.fn.localTypes.at(operation.left.ordinal)
      const leftLane =
        leftType === undefined ? undefined : NativeType.valueLanesFor(types, leftType).at(0)
      if (leftType === undefined || leftLane === undefined) {
        throw new RangeError('LLVM binary operation lost its operand type')
      }
      const semanticOperand = Mir.semanticType(leftType)
      const scalar = typeof semanticOperand === 'string' ? Scalar.find(semanticOperand) : undefined
      const unsigned = scalar?.signedness === 'Unsigned'
      const operandType = NativeType.laneType(types, leftLane)
      const ordinal = checkOrdinal
      checkOrdinal += 1
      if (scalar?.category === 'Floating') {
        if (operation.operator === 'TotalOrder') {
          const width = scalar.spelling === 'f32' ? 32 : 64
          const integerType = integerTypes.get(width) ?? i32
          const leftBits = yield* FunctionBody.cast(
            body,
            'bitcast',
            left,
            integerType,
            `total${ordinal}_left_bits`,
          )
          const rightBits = yield* FunctionBody.cast(
            body,
            'bitcast',
            right,
            integerType,
            `total${ordinal}_right_bits`,
          )
          const zero = yield* Constant.integerUnsigned(builder, integerType, 0n)
          const all = yield* Constant.integerUnsigned(
            builder,
            integerType,
            (1n << BigInt(width)) - 1n,
          )
          const sign = yield* Constant.integerUnsigned(
            builder,
            integerType,
            1n << BigInt(width - 1),
          )
          const key = Effect.fnUntraced(function* (bits: Value.Input, side: string) {
            const negative = yield* FunctionBody.integerCompare(
              body,
              'slt',
              bits,
              zero,
              `total${ordinal}_${side}_negative`,
            )
            const mask = yield* FunctionBody.select(
              body,
              negative,
              all,
              sign,
              `total${ordinal}_${side}_mask`,
            )
            return yield* FunctionBody.binary(
              body,
              'xor',
              bits,
              mask,
              `total${ordinal}_${side}_key`,
            )
          })
          const leftKey = yield* key(leftBits, 'left')
          const rightKey = yield* key(rightBits, 'right')
          const flag = yield* FunctionBody.integerCompare(
            body,
            'ule',
            leftKey,
            rightKey,
            `total${ordinal}_flag`,
          )
          const result = yield* FunctionBody.cast(body, 'zext', flag, i32, `total${ordinal}`)
          nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
          break
        }
        let predicate: FunctionBody.FloatingPredicate | undefined
        switch (operation.operator) {
          case 'Equals':
            predicate = 'oeq'
            break
          case 'NotEquals':
            predicate = 'une'
            break
          case 'LessThan':
            predicate = 'olt'
            break
          case 'LessOrEqual':
            predicate = 'ole'
            break
          case 'GreaterThan':
            predicate = 'ogt'
            break
          case 'GreaterOrEqual':
            predicate = 'oge'
            break
          default:
            predicate = undefined
            break
        }
        if (predicate !== undefined) {
          const flag = yield* FunctionBody.floatingCompare(
            body,
            predicate,
            left,
            right,
            `fcmp${ordinal}_flag`,
          )
          const result = yield* FunctionBody.cast(body, 'zext', flag, i32, `fcmp${ordinal}`)
          nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
          break
        }
        let mnemonic: FunctionBody.FloatingBinaryKind | undefined
        switch (operation.operator) {
          case 'Add':
            mnemonic = 'fadd'
            break
          case 'Subtract':
            mnemonic = 'fsub'
            break
          case 'Multiply':
            mnemonic = 'fmul'
            break
          case 'Divide':
            mnemonic = 'fdiv'
            break
          case 'Remainder':
            mnemonic = 'frem'
            break
          default:
            mnemonic = undefined
            break
        }
        if (mnemonic === undefined)
          throw new RangeError(`LLVM float operation ${operation.operator} is unavailable`)
        const result = yield* FunctionBody.binary(body, mnemonic, left, right, `float${ordinal}`)
        yield* NativeDebug.locate(
          debug,
          operation.provenance.span,
          yield* Value.instruction(body, result),
        )
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
        break
      }
      const predicate = NativeArith.comparisonPredicate(operation.operator, unsigned)
      if (predicate !== undefined) {
        const flag = yield* FunctionBody.integerCompare(
          body,
          predicate,
          left,
          right,
          `cmp${ordinal}_flag`,
        )
        const widened = yield* FunctionBody.cast(body, 'zext', flag, i32, `cmp${ordinal}`)
        const instruction = yield* Value.instruction(body, flag)
        yield* NativeDebug.locate(debug, operation.provenance.span, instruction)
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([widened]))
        break
      }
      if (
        operation.operator === 'BitAnd' ||
        operation.operator === 'BitOr' ||
        operation.operator === 'BitXor' ||
        operation.operator === 'WrappingAdd' ||
        operation.operator === 'WrappingSubtract' ||
        operation.operator === 'WrappingMultiply'
      ) {
        let mnemonic: FunctionBody.BinaryKind
        switch (operation.operator) {
          case 'BitAnd':
            mnemonic = 'and'
            break
          case 'BitOr':
            mnemonic = 'or'
            break
          case 'BitXor':
            mnemonic = 'xor'
            break
          case 'WrappingAdd':
            mnemonic = 'add'
            break
          case 'WrappingSubtract':
            mnemonic = 'sub'
            break
          case 'WrappingMultiply':
            mnemonic = 'mul'
            break
        }
        const result = yield* FunctionBody.binary(body, mnemonic, left, right, `integer${ordinal}`)
        yield* NativeDebug.locate(
          debug,
          operation.provenance.span,
          yield* Value.instruction(body, result),
        )
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
        break
      }
      if (operation.operator === 'ShiftLeft' || operation.operator === 'ShiftRight') {
        if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'arith_trap')
        let width: number
        if (scalar === undefined) {
          width = 32
        } else {
          width = Scalar.bits(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
        }
        const limit = yield* Constant.integerUnsigned(builder, operandType, BigInt(width))
        const invalid = yield* FunctionBody.integerCompare(
          body,
          'uge',
          right,
          limit,
          `shift${ordinal}_invalid`,
        )
        const continueBlock = yield* LlvmBlock.make(body, `shift${ordinal}_ok`)
        yield* FunctionBody.conditionalBranch(body, invalid, trapBlock, continueBlock)
        yield* LlvmBlock.setInsertionPoint(body, continueBlock)
        let opcode: 'shl' | 'lshr' | 'ashr'
        if (operation.operator === 'ShiftLeft') opcode = 'shl'
        else opcode = unsigned ? 'lshr' : 'ashr'
        const result = yield* FunctionBody.binary(body, opcode, left, right, `shift${ordinal}`)
        yield* NativeDebug.locate(
          debug,
          operation.provenance.span,
          yield* Value.instruction(body, result),
        )
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
        break
      }
      if (operation.operator === 'RotateLeft' || operation.operator === 'RotateRight') {
        const signature = Object.freeze({
          returnType: operandType,
          parameters: Object.freeze([operandType, operandType, operandType]),
        })
        const result = yield* Intrinsic.call(
          body,
          operation.operator === 'RotateLeft' ? 'fshl' : 'fshr',
          [operandType],
          [left, left, right],
          `rotate${ordinal}`,
          { signature },
        )
        if (result === undefined) throw new RangeError('LLVM rotate produced no value')
        yield* NativeDebug.locate(
          debug,
          operation.provenance.span,
          yield* Value.instruction(body, result),
        )
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
        break
      }
      if (operation.operator === 'SaturatingAdd' || operation.operator === 'SaturatingSubtract') {
        const signature = Object.freeze({
          returnType: operandType,
          parameters: Object.freeze([operandType, operandType]),
        })
        let intrinsic: Intrinsic.Id
        switch (operation.operator) {
          case 'SaturatingAdd':
            intrinsic = unsigned ? 'uadd.sat' : 'sadd.sat'
            break
          case 'SaturatingSubtract':
            intrinsic = unsigned ? 'usub.sat' : 'ssub.sat'
            break
        }
        const result = yield* Intrinsic.call(
          body,
          intrinsic,
          [operandType],
          [left, right],
          `saturating${ordinal}`,
          { signature },
        )
        if (result === undefined)
          throw new RangeError('LLVM saturating arithmetic produced no value')
        yield* NativeDebug.locate(
          debug,
          operation.provenance.span,
          yield* Value.instruction(body, result),
        )
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
        break
      }
      if (operation.operator === 'SaturatingMultiply') {
        let bits: number
        if (scalar === undefined) {
          bits = 32
        } else {
          bits = Scalar.bits(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
        }
        const signatures = unsigned ? unsignedOverflowSignatures : signedOverflowSignatures
        let signature = signatures.get(bits)
        if (signature === undefined) {
          const i1 = yield* LlvmType.integer(builder, 1)
          signature = Object.freeze({
            returnType: yield* LlvmType.structure(builder, [operandType, i1]),
            parameters: Object.freeze([operandType, operandType]),
          })
          signatures.set(bits, signature)
        }
        const pair = yield* Intrinsic.call(
          body,
          unsigned ? 'umul.with.overflow' : 'smul.with.overflow',
          [operandType],
          [left, right],
          `saturating${ordinal}_pair`,
          { signature },
        )
        if (pair === undefined) throw new RangeError('LLVM saturating multiply produced no value')
        const wrapped = yield* FunctionBody.extractValue(
          body,
          pair,
          [0],
          `saturating${ordinal}_wrapped`,
        )
        const overflowed = yield* FunctionBody.extractValue(
          body,
          pair,
          [1],
          `saturating${ordinal}_overflow`,
        )
        let range: { readonly minimum: bigint; readonly maximum: bigint }
        if (scalar?.category === 'Integer') {
          range = Scalar.range(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
        } else {
          range = { minimum: -2147483648n, maximum: 2147483647n }
        }
        const maximum = unsigned
          ? yield* Constant.integerUnsigned(builder, operandType, range.maximum)
          : yield* Constant.integerSigned(builder, operandType, range.maximum)
        let boundary: Value.Input = maximum
        if (!unsigned) {
          const zero = yield* Constant.integerSigned(builder, operandType, 0n)
          const minimum = yield* Constant.integerSigned(builder, operandType, range.minimum)
          const signs = yield* FunctionBody.binary(
            body,
            'xor',
            left,
            right,
            `saturating${ordinal}_signs`,
          )
          const negative = yield* FunctionBody.integerCompare(
            body,
            'slt',
            signs,
            zero,
            `saturating${ordinal}_negative`,
          )
          boundary = yield* FunctionBody.select(
            body,
            negative,
            minimum,
            maximum,
            `saturating${ordinal}_boundary`,
          )
        }
        const result = yield* FunctionBody.select(
          body,
          overflowed,
          boundary,
          wrapped,
          `saturating${ordinal}`,
        )
        yield* NativeDebug.locate(
          debug,
          operation.provenance.span,
          yield* Value.instruction(body, result),
        )
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
        break
      }
      let result: Value.Value
      if (trapBlock === undefined) {
        trapBlock = yield* LlvmBlock.make(body, 'arith_trap')
      }
      if (
        operation.operator === 'Add' ||
        operation.operator === 'Subtract' ||
        operation.operator === 'Multiply'
      ) {
        let intrinsicId: Intrinsic.Id
        switch (operation.operator) {
          case 'Add':
            intrinsicId = unsigned ? 'uadd.with.overflow' : 'sadd.with.overflow'
            break
          case 'Subtract':
            intrinsicId = unsigned ? 'usub.with.overflow' : 'ssub.with.overflow'
            break
          case 'Multiply':
            intrinsicId = unsigned ? 'umul.with.overflow' : 'smul.with.overflow'
            break
        }
        let bits: number
        if (scalar === undefined) {
          bits = 32
        } else {
          bits = Scalar.bits(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
        }
        const signatures = unsigned ? unsignedOverflowSignatures : signedOverflowSignatures
        let overflowSignature = signatures.get(bits)
        if (overflowSignature === undefined) {
          const i1 = yield* LlvmType.integer(builder, 1)
          overflowSignature = Object.freeze({
            returnType: yield* LlvmType.structure(builder, [operandType, i1]),
            parameters: Object.freeze([operandType, operandType]),
          })
          signatures.set(bits, overflowSignature)
        }
        const pair = yield* Intrinsic.call(
          body,
          intrinsicId,
          [operandType],
          [left, right],
          `arith${ordinal}_pair`,
          { signature: overflowSignature },
        )
        if (pair === undefined) {
          throw new RangeError('Backend overflow intrinsic produced no value')
        }
        const valuePart = yield* FunctionBody.extractValue(body, pair, [0], `arith${ordinal}`)
        const overflowed = yield* FunctionBody.extractValue(body, pair, [1], `arith${ordinal}_flag`)
        const continueBlock = yield* LlvmBlock.make(body, `arith${ordinal}_ok`)
        yield* FunctionBody.conditionalBranch(body, overflowed, trapBlock, continueBlock)
        yield* LlvmBlock.setInsertionPoint(body, continueBlock)
        result = valuePart
      } else {
        const zero = yield* Constant.integerUnsigned(builder, operandType, 0n)
        const zeroDivisor = yield* FunctionBody.integerCompare(
          body,
          'eq',
          right,
          zero,
          `div${ordinal}_zero`,
        )
        let trapping: Value.Value = zeroDivisor
        if (!unsigned) {
          const minimum = yield* Constant.integerSigned(
            builder,
            operandType,
            scalar?.category === 'Integer'
              ? Scalar.range(scalar, program.layout.target.pointerSize === 4 ? 32 : 64).minimum
              : -2147483648n,
          )
          const negativeOne = yield* Constant.integerSigned(builder, operandType, -1n)
          const minimumDividend = yield* FunctionBody.integerCompare(
            body,
            'eq',
            left,
            minimum,
            `div${ordinal}_min`,
          )
          const negativeOneDivisor = yield* FunctionBody.integerCompare(
            body,
            'eq',
            right,
            negativeOne,
            `div${ordinal}_negone`,
          )
          const overflowCase = yield* FunctionBody.binary(
            body,
            'and',
            minimumDividend,
            negativeOneDivisor,
            `div${ordinal}_overflow`,
          )
          trapping = yield* FunctionBody.binary(
            body,
            'or',
            zeroDivisor,
            overflowCase,
            `div${ordinal}_trapping`,
          )
        }
        const continueBlock = yield* LlvmBlock.make(body, `div${ordinal}_ok`)
        yield* FunctionBody.conditionalBranch(body, trapping, trapBlock, continueBlock)
        yield* LlvmBlock.setInsertionPoint(body, continueBlock)
        let opcode: 'udiv' | 'sdiv' | 'urem' | 'srem'
        if (operation.operator === 'Divide') opcode = unsigned ? 'udiv' : 'sdiv'
        else opcode = unsigned ? 'urem' : 'srem'
        result = yield* FunctionBody.binary(body, opcode, left, right, `arith${ordinal}`)
      }
      const instruction = yield* Value.instruction(body, result)
      yield* NativeDebug.locate(debug, operation.provenance.span, instruction)
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
      break
    }
  }
  if (trapBlock !== initialTrapBlock) context.state.trapBlock = trapBlock
  context.state.checkOrdinal = checkOrdinal
})
