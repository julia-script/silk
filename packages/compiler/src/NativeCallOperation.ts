import * as NativeResult from './NativeResult.js'
import * as NativeCallable from './NativeCallable.js'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as Hir from './Hir.js'
import * as Mir from './Mir.js'
import * as FunctionIndex from './internal/FunctionIndex.js'
import type { LinearOperation } from './MirLinearization.js'
import * as NativeArith from './NativeArith.js'
import * as NativeCall from './NativeCall.js'
import * as NativeDiagnosticOutcome from './NativeDiagnosticOutcome.js'
import * as NativeDebug from './NativeDebug.js'
import type { Context } from './NativeOperationContext.js'
import * as NativeScalarOperation from './NativeScalarOperation.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeType from './NativeType.js'
import * as Scalar from './Scalar.js'
import * as SilkType from './Type.js'

type Operation = Extract<LinearOperation, { readonly _tag: 'ApplyCallable' | 'Call' }>

export const emit = Effect.fnUntraced(function* (context: Context, operation: Operation) {
  const {
    body,
    builder,
    call,
    declared,
    arith,
    debug,
    entry,
    f32,
    f64,
    i32,
    integerTypes,
    program,
    storage: nativeStorage,
    types,
  } = context
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
      const targetUsesEnvironmentBorrows =
        target._tag === 'DeclarationCallableTarget' && Hir.isAnonymousCallableId(target.declaration)
      const captureGroups: Array<{
        readonly parameterOrdinal: number
        readonly values: ReadonlyArray<Value.Input>
      }> = []
      if (operation.callable !== undefined) {
        if (sourceType?._tag !== 'CallableValue')
          throw new RangeError('Stored callable application lost its identity')
        captureGroups.push(
          ...(yield* NativeCallable.capturedArguments(
            context,
            sourceType,
            yield* NativeStorage.materialize(nativeStorage, operation.callable),
            `callable${operation.destination.ordinal}`,
          )),
        )
      } else {
        for (const capture of operation.captures) {
          let values: ReadonlyArray<Value.Input>
          if (
            targetUsesEnvironmentBorrows &&
            (capture.access === 'Shared' || capture.access === 'Exclusive')
          ) {
            yield* NativeStorage.ensureAddressRoot(nativeStorage, capture.source)
            const base = yield* NativeStorage.addressOf(nativeStorage, capture.source)
            values = Object.freeze([base])
          } else {
            values = yield* NativeStorage.materialize(nativeStorage, capture.source)
          }
          captureGroups.push(
            Object.freeze({
              parameterOrdinal: capture.parameterOrdinal,
              values,
            }),
          )
        }
      }
      const operands = Mir.applyOperands(
        captureGroups.map((capture) =>
          Object.freeze({ parameterOrdinal: capture.parameterOrdinal, items: capture.values }),
        ),
        yield* NativeStorage.materializeArguments(nativeStorage, operation.arguments),
      )
      if (target._tag === 'BuiltinCallableTarget') {
        const supplied = operands
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
            yield* NativeScalarOperation.emitFloatToIntegerGuard(
              context,
              first,
              sourceScalar,
              conversionTarget,
              `callable_convert${operation.destination.ordinal}`,
              operation.provenance.span,
            )
            const result = yield* FunctionBody.cast(
              body,
              conversionTarget.signedness === 'Signed' ? 'fptosi' : 'fptoui',
              first,
              destination,
              `callable_convert${operation.destination.ordinal}`,
            )
            yield* NativeStorage.writeLocal(
              nativeStorage,
              operation.destination.ordinal,
              Object.freeze([result]),
            )
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
            operation.provenance.span,
          )
          yield* NativeStorage.writeLocal(
            nativeStorage,
            operation.destination.ordinal,
            Object.freeze([result]),
          )
          break
        }
        const floatTarget = Scalar.floatConversionTarget(target.operation)
        if (floatTarget !== undefined) {
          const source = Scalar.find(firstType._tag)
          if (source === undefined || source.category === 'Boolean')
            throw new RangeError('LLVM callable float conversion lost its source type')
          const destination = floatTarget.spelling === 'f32' ? f32 : f64
          let result: Value.Input
          if (source.category === 'Floating') {
            if (source.spelling === floatTarget.spelling) {
              result = first
            } else {
              result = yield* FunctionBody.cast(
                body,
                source.spelling === 'f64' ? 'fptrunc' : 'fpext',
                first,
                destination,
                `callable_convert${operation.destination.ordinal}`,
              )
            }
          } else {
            result = yield* FunctionBody.cast(
              body,
              source.signedness === 'Signed' ? 'sitofp' : 'uitofp',
              first,
              destination,
              `callable_convert${operation.destination.ordinal}`,
            )
          }
          yield* NativeStorage.writeLocal(
            nativeStorage,
            operation.destination.ordinal,
            Object.freeze([result]),
          )
          break
        }
        if (target.operation === 'Negate' && Scalar.find(firstType._tag)?.category === 'Floating') {
          const result = yield* FunctionBody.unary(
            body,
            'fneg',
            first,
            `callable_fneg${operation.destination.ordinal}`,
          )
          yield* NativeStorage.writeLocal(
            nativeStorage,
            operation.destination.ordinal,
            Object.freeze([result]),
          )
          break
        }
        if (
          target.operation === 'Not' ||
          target.operation === 'Negate' ||
          target.operation === 'WrappingNegate' ||
          target.operation === 'SaturatingNegate' ||
          target.operation === 'BitNot'
        ) {
          const firstLane = NativeType.valueLanesFor(types, firstType).at(0)
          if (firstLane === undefined)
            throw new RangeError('LLVM callable unary operation lost its lane')
          const operandType = NativeType.laneType(types, firstLane)
          const zero = yield* Constant.integerSigned(builder, operandType, 0n)
          if (target.operation !== 'Not') {
            let unaryOperator: Mir.BinaryOperator
            switch (target.operation) {
              case 'Negate':
                unaryOperator = 'Subtract'
                break
              case 'WrappingNegate':
                unaryOperator = 'WrappingSubtract'
                break
              case 'SaturatingNegate':
                unaryOperator = 'SaturatingSubtract'
                break
              case 'BitNot':
                unaryOperator = 'BitXor'
                break
            }
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
            yield* NativeStorage.writeLocal(nativeStorage, operation.destination.ordinal, values)
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
          yield* NativeStorage.writeLocal(nativeStorage, operation.destination.ordinal, values)
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
        yield* NativeStorage.writeLocal(nativeStorage, operation.destination.ordinal, values)
        break
      }
      const callableTarget = FunctionIndex.nativeCandidates(declared, target.declaration).find(
        (candidate) =>
          Mir.matchesInstance(candidate.fn, target.declaration, operation.typeArguments),
      )
      if (callableTarget === undefined)
        throw new RangeError(
          `Backend cannot resolve callable target ${target.declaration.module}.${target.declaration.name}<${operation.typeArguments.map(SilkType.encodeGenericArgument).join(', ')}>`,
        )
      // Callable application has the same completion boundary as an ordinary source call.
      const handle = callableTarget.suspendable ? callableTarget.driver : callableTarget.handle
      if (handle === undefined)
        throw new RangeError('Backend callable application lost its completion driver')
      const called = yield* FunctionBody.callDirect(
        body,
        handle,
        yield* NativeCall.argumentsFor(call.synchronous, callableTarget, operands),
        `callable${operation.destination.ordinal}`,
      )
      // A never-returning callback may inhabit a wider join result type. It produces no
      // payload to store; the enclosing MIR control flow owns its unreachable terminator.
      if (callableTarget.fn.result._tag === 'Bottom') break
      const result = yield* NativeResult.unpack(
        body,
        {
          resultLaneCount: callableTarget.resultLaneCount,
          diagnosticResult: callableTarget.diagnosticResult !== undefined,
        },
        called,
        `callable${operation.destination.ordinal}`,
      )
      for (const root of [...nativeStorage.addressRoots].sort((left, right) => left - right)) {
        yield* NativeStorage.reloadAddressRoot(nativeStorage, root)
      }
      yield* NativeStorage.writeLocal(
        nativeStorage,
        operation.destination.ordinal,
        yield* NativeDiagnosticOutcome.accept(
          call.synchronous.diagnostic,
          operation.destination,
          result,
        ),
      )
      break
    }
    case 'Call': {
      const target = FunctionIndex.nativeCandidates(declared, operation.target).find((candidate) =>
        Mir.matchesInstance(
          candidate.fn,
          operation.target,
          operation.typeArguments,
          operation.staticArguments,
        ),
      )
      if (target === undefined) {
        throw new RangeError(`Backend cannot resolve call target ${operation.target.name}`)
      }
      // An ordinary call has no suspension control: a suspendable target is driven to
      // completion here, the way the machine root drives the entry.
      const handle = target.suspendable ? target.driver : target.handle
      if (handle === undefined)
        throw new RangeError(
          `Backend cannot drive suspendable call target ${operation.target.name}`,
        )
      const result = yield* FunctionBody.callDirect(
        body,
        handle,
        yield* NativeCall.argumentsFor(
          call.synchronous,
          target,
          (yield* NativeStorage.materializeArguments(nativeStorage, operation.arguments)).flat(),
        ),
        `t${operation.destination.ordinal}`,
      )
      if (target.fn.result._tag === 'Bottom') break
      for (const root of [...nativeStorage.addressRoots].sort((left, right) => left - right)) {
        yield* NativeStorage.reloadAddressRoot(nativeStorage, root)
      }
      if (target.resultLaneCount === 0) {
        yield* NativeStorage.writeLocal(
          nativeStorage,
          operation.destination.ordinal,
          Object.freeze([]),
        )
        break
      }
      if (result === undefined) {
        throw new RangeError('Backend call produced no value')
      }
      const instruction = yield* Value.instruction(body, result)
      yield* NativeDebug.locate(debug, operation.provenance.span, instruction)
      const unpacked = yield* NativeResult.unpack(
        body,
        {
          resultLaneCount: target.resultLaneCount,
          diagnosticResult: target.diagnosticResult !== undefined,
        },
        result,
        `t${operation.destination.ordinal}`,
      )
      yield* NativeStorage.writeLocal(
        nativeStorage,
        operation.destination.ordinal,
        yield* NativeDiagnosticOutcome.accept(
          call.synchronous.diagnostic,
          operation.destination,
          unpacked,
        ),
      )
      break
    }
  }
  context.state.checkOrdinal = checkOrdinal
})
