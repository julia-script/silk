import * as Constant from '@silk-effect/llvm/Constant'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'
import * as CleanupPlan from './CleanupPlan.js'
import * as Layout from './Layout.js'
import * as LayoutVerify from './LayoutVerify.js'
import * as Mir from './Mir.js'
import type { LinearOperation } from './MirLinearization.js'
import * as NativeArith from './NativeArith.js'
import * as NativeCall from './NativeCall.js'
import * as Scalar from './Scalar.js'

/** Whether one MIR operation requires the native allocation ABI. */
export const needsAllocation = (operation: Mir.Operation): boolean =>
  operation._tag === 'Allocate' ||
  operation._tag === 'RawBufferFrom' ||
  operation._tag === 'RawBufferCount' ||
  operation._tag === 'RawBufferSlot' ||
  operation._tag === 'RawBufferRead' ||
  operation._tag === 'RawBufferView' ||
  operation._tag === 'RawBufferCopy' ||
  operation._tag === 'RawBufferFill' ||
  operation._tag === 'SlotWrite' ||
  operation._tag === 'SlotTake' ||
  operation._tag === 'SlotCopy' ||
  operation._tag === 'SlotDrop' ||
  (operation._tag === 'CloseEffectEntry' &&
    operation.failures.some((failure) => CleanupPlan.reclaims(failure.cleanup))) ||
  (operation._tag === 'Drop' && CleanupPlan.reclaims(operation.cleanup))

import * as NativeFunction from './NativeFunction.js'
import type { LoweringContext } from './NativeOperation.js'

type Operation = Extract<LinearOperation, { readonly _tag: 'ApplyCallable' | 'Call' }>

export const emit = Effect.fnUntraced(function* (context: LoweringContext, operation: Operation) {
  const {
    addressRoots,
    body,
    builder,
    call,
    constantBytePointer,
    declared,
    arith,
    entry,
    f32,
    f64,
    i32,
    integerTypes,
    laneType,
    locals,
    locate,
    program,
    reloadAddressRoot,
    valueLanesFor,
  } = context
  const initialTrapBlock = context.state.trapBlock
  const trapBlock = initialTrapBlock
  const checkOrdinal = context.state.checkOrdinal
  switch (operation._tag) {
    case 'ApplyCallable': {
      const sourceType =
        operation.callable === undefined
          ? undefined
          : entry.fn.localTypes.at(operation.callable.ordinal)
      const target =
        operation.target ?? (sourceType?._tag === 'CallableValue' ? sourceType.target : undefined)
      if (target === undefined)
        throw new RangeError('Backend callable application lost its hidden identity')
      const captureGroups: Array<{
        readonly parameterOrdinal: number
        readonly values: ReadonlyArray<Value.Input>
      }> = []
      if (operation.callable !== undefined) {
        if (sourceType?._tag !== 'CallableValue')
          throw new RangeError('Stored callable application lost its identity')
        const environmentValues = NativeFunction.readLocal(locals, operation.callable)
        let cursor = 0
        for (const field of sourceType.environment?.fields ?? []) {
          const shape = Layout.callingShape(program.layout, field.type)
          if (shape === undefined)
            throw new RangeError('Callable capture lost its semantic calling shape')
          if (field.representation === 'Value') {
            captureGroups.push(
              Object.freeze({
                parameterOrdinal: field.parameterOrdinal,
                values: Object.freeze(environmentValues.slice(cursor, cursor + shape.laneCount)),
              }),
            )
            cursor += shape.laneCount
            continue
          }
          const base = environmentValues.at(cursor)
          if (base === undefined)
            throw new RangeError('Callable borrowed environment lost its pointer')
          cursor += 1
          const values: Array<Value.Input> = []
          for (const [laneOrdinal, lane] of shape.lanes.entries()) {
            const offset = LayoutVerify.laneOffset(program.layout, field.type, lane.path)
            if (offset === undefined)
              throw new RangeError('Callable borrowed capture lost its lane offset')
            values.push(
              yield* FunctionBody.load(
                body,
                laneType(lane),
                yield* constantBytePointer(
                  base,
                  offset,
                  `callable${operation.destination.ordinal}_capture${field.ordinal}_${laneOrdinal}_ptr`,
                ),
                `callable${operation.destination.ordinal}_capture${field.ordinal}_${laneOrdinal}`,
              ),
            )
          }
          captureGroups.push(
            Object.freeze({
              parameterOrdinal: field.parameterOrdinal,
              values: Object.freeze(values),
            }),
          )
        }
      } else {
        for (const capture of operation.captures) {
          captureGroups.push(
            Object.freeze({
              parameterOrdinal: capture.parameterOrdinal,
              values: NativeFunction.readLocal(locals, capture.source),
            }),
          )
        }
      }
      const captureValues = [...captureGroups]
        .sort((left, right) => left.parameterOrdinal - right.parameterOrdinal)
        .flatMap((capture) => [...capture.values])
      if (target._tag === 'BuiltinCallableTarget') {
        const supplied = Object.freeze([
          ...operation.arguments.flatMap((argument) => [
            ...NativeFunction.readLocal(locals, argument),
          ]),
          ...captureValues,
        ])
        const first = supplied.at(0)
        const firstLocal = operation.arguments.at(0)
        const firstType =
          firstLocal === undefined ? undefined : entry.fn.localTypes.at(firstLocal.ordinal)
        if (first === undefined || firstType === undefined)
          throw new RangeError('LLVM callable builtin lost its first operand')
        const conversionTarget = Scalar.conversionTarget(target.operation)
        if (conversionTarget !== undefined) {
          const sourceScalar = Scalar.find(firstType._tag)
          if (sourceScalar?.category === 'Floating') {
            const destination =
              integerTypes.get(
                Scalar.bits(conversionTarget, program.layout.target.pointerSize === 4 ? 32 : 64),
              ) ?? i32
            const result = yield* FunctionBody.cast(
              body,
              conversionTarget.signedness === 'Signed' ? 'fptosi' : 'fptoui',
              first,
              destination,
              `callable_convert${operation.destination.ordinal}`,
            )
            locals.set(operation.destination.ordinal, Object.freeze([result]))
            break
          }
          if (sourceScalar?.category !== 'Integer')
            throw new RangeError('LLVM callable conversion lost its source type')
          const result = yield* NativeArith.emitIntegerConversion(
            arith,
            first,
            Object.freeze({ _tag: sourceScalar.spelling }),
            Object.freeze({ _tag: conversionTarget.spelling }),
            `callable_convert${operation.destination.ordinal}`,
          )
          locals.set(operation.destination.ordinal, Object.freeze([result]))
          break
        }
        const floatTarget = Scalar.floatConversionTarget(target.operation)
        if (floatTarget !== undefined) {
          const source = Scalar.find(firstType._tag)
          if (source === undefined || source.category === 'Boolean')
            throw new RangeError('LLVM callable float conversion lost its source type')
          const destination = floatTarget.spelling === 'f32' ? f32 : f64
          const result =
            source.category === 'Floating'
              ? source.spelling === floatTarget.spelling
                ? first
                : yield* FunctionBody.cast(
                    body,
                    source.spelling === 'f64' ? 'fptrunc' : 'fpext',
                    first,
                    destination,
                    `callable_convert${operation.destination.ordinal}`,
                  )
              : yield* FunctionBody.cast(
                  body,
                  source.signedness === 'Signed' ? 'sitofp' : 'uitofp',
                  first,
                  destination,
                  `callable_convert${operation.destination.ordinal}`,
                )
          locals.set(operation.destination.ordinal, Object.freeze([result]))
          break
        }
        if (target.operation === 'Negate' && Scalar.find(firstType._tag)?.category === 'Floating') {
          const result = yield* FunctionBody.unary(
            body,
            'fneg',
            first,
            `callable_fneg${operation.destination.ordinal}`,
          )
          locals.set(operation.destination.ordinal, Object.freeze([result]))
          break
        }
        if (
          target.operation === 'Not' ||
          target.operation === 'Negate' ||
          target.operation === 'WrappingNegate' ||
          target.operation === 'SaturatingNegate' ||
          target.operation === 'BitNot'
        ) {
          const firstLane = valueLanesFor(firstType).at(0)
          if (firstLane === undefined)
            throw new RangeError('LLVM callable unary operation lost its lane')
          const operandType = laneType(firstLane)
          const zero = yield* Constant.integerSigned(builder, operandType, 0n)
          if (target.operation !== 'Not') {
            const unaryOperator =
              target.operation === 'Negate'
                ? 'Subtract'
                : target.operation === 'WrappingNegate'
                  ? 'WrappingSubtract'
                  : target.operation === 'SaturatingNegate'
                    ? 'SaturatingSubtract'
                    : 'BitXor'
            const right =
              target.operation === 'BitNot'
                ? yield* Constant.integerSigned(builder, operandType, -1n)
                : first
            const values = Object.freeze([
              yield* NativeArith.emitCallableBinary(
                arith,
                unaryOperator,
                target.operation === 'BitNot' ? first : zero,
                right,
                firstType,
                operation.provenance.span,
                operation.destination.ordinal,
              ),
            ])
            locals.set(operation.destination.ordinal, values)
            break
          }
          const boolZero = yield* Constant.integerSigned(builder, i32, 0n)
          const flag = yield* FunctionBody.integerCompare(
            body,
            'eq',
            first,
            boolZero,
            `callable_not${operation.destination.ordinal}_flag`,
          )
          const values = Object.freeze([
            yield* FunctionBody.cast(
              body,
              'zext',
              flag,
              i32,
              `callable_not${operation.destination.ordinal}`,
            ),
          ])
          locals.set(operation.destination.ordinal, values)
          break
        }
        const second = supplied.at(1)
        if (
          second === undefined ||
          target.operation === 'StorageAcquire' ||
          !Mir.isBinaryOperator(target.operation)
        ) {
          throw new RangeError(
            `LLVM callable builtin ${target.actor}.${target.operation} is unavailable`,
          )
        }
        const values = Object.freeze([
          yield* NativeArith.emitCallableBinary(
            arith,
            target.operation,
            first,
            second,
            firstType,
            operation.provenance.span,
            operation.destination.ordinal,
          ),
        ])
        locals.set(operation.destination.ordinal, values)
        break
      }
      const callableTarget = declared.find((candidate) =>
        Mir.matchesInstance(candidate.fn, target.declaration, operation.typeArguments),
      )
      if (callableTarget === undefined)
        throw new RangeError('Backend cannot resolve callable target')
      const result = yield* NativeCall.callValues(
        call,
        callableTarget,
        Object.freeze([
          ...operation.arguments.flatMap((argument) => [
            ...NativeFunction.readLocal(locals, argument),
          ]),
          ...captureValues,
        ]),
        `callable${operation.destination.ordinal}`,
      )
      locals.set(operation.destination.ordinal, result)
      break
    }
    case 'Call': {
      const target = declared.find((candidate) =>
        Mir.matchesInstance(candidate.fn, operation.target, operation.typeArguments),
      )
      if (target === undefined) {
        throw new RangeError(`Backend cannot resolve call target ${operation.target.name}`)
      }
      const result = yield* FunctionBody.callDirect(
        body,
        target.handle,
        operation.arguments.flatMap((argument) => [...NativeFunction.readLocal(locals, argument)]),
        `t${operation.destination.ordinal}`,
      )
      for (const root of [...addressRoots].sort((left, right) => left - right)) {
        yield* reloadAddressRoot(root)
      }
      if (target.resultLaneCount === 0) {
        locals.set(operation.destination.ordinal, Object.freeze([]))
        break
      }
      if (result === undefined) {
        throw new RangeError('Backend call produced no value')
      }
      const instruction = yield* Value.instruction(body, result)
      yield* locate(operation.provenance.span, instruction)
      if (target.resultLaneCount === 1) {
        locals.set(operation.destination.ordinal, Object.freeze([result]))
        break
      }
      const values: Array<Value.Input> = []
      for (let lane = 0; lane < target.resultLaneCount; lane += 1) {
        values.push(
          yield* FunctionBody.extractValue(
            body,
            result,
            [lane],
            `t${operation.destination.ordinal}_${lane}`,
          ),
        )
      }
      locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
  }
  if (trapBlock !== initialTrapBlock) context.state.trapBlock = trapBlock
  context.state.checkOrdinal = checkOrdinal
})
