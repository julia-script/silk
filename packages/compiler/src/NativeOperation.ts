import * as Alignment from '@silk-effect/llvm/Alignment'
import * as LlvmBlock from '@silk-effect/llvm/Block'
import type * as Builder from '@silk-effect/llvm/Builder'
import * as Constant from '@silk-effect/llvm/Constant'
import type * as FunctionActor from '@silk-effect/llvm/Function'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import * as Intrinsic from '@silk-effect/llvm/Intrinsic'
import type * as LlvmError from '@silk-effect/llvm/LlvmError'
import * as LlvmType from '@silk-effect/llvm/Type'
import * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'
import * as CleanupPlan from './CleanupPlan.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as FloatingPoint from './FloatingPoint.js'
import * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import type { LinearOperation } from './MirLinearization.js'
import * as NativeArith from './NativeArith.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import type * as NativeLoweringContext from './NativeLoweringContext.js'
import * as NativeTranscendental from './NativeTranscendental.js'
import * as Scalar from './Scalar.js'
import type * as SourceSpan from './SourceSpan.js'
import * as SilkType from './Type.js'

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

interface OverflowSignature {
  readonly returnType: LlvmType.Type
  readonly parameters: ReadonlyArray<LlvmType.Type>
}

type EffectOperation = Extract<
  Mir.Operation,
  { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' }
>

export interface LoweringContext {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly program: Mir.Module
  readonly entry: NativeLoweringContext.DeclaredFunction
  readonly declared: ReadonlyArray<NativeLoweringContext.DeclaredFunction>
  readonly locals: Map<number, ReadonlyArray<Value.Input>>
  readonly staticPointers: ReadonlyMap<string, Constant.Constant>
  readonly i32: LlvmType.Type
  readonly f32: LlvmType.Type
  readonly f64: LlvmType.Type
  readonly pointer: LlvmType.Type
  readonly usizeType?: LlvmType.Type
  readonly integerTypes: Map<number, LlvmType.Type>
  readonly signedOverflowSignatures: Map<number, OverflowSignature>
  readonly unsignedOverflowSignatures: Map<number, OverflowSignature>
  readonly malloc?: FunctionActor.Function
  readonly free?: FunctionActor.Function
  readonly memcmp?: FunctionActor.Function
  readonly standardWrite?: FunctionActor.Function
  readonly osRuntimes: ReadonlyMap<
    string,
    {
      readonly handle: FunctionActor.Function
      readonly abi: 'Direct' | 'OpenOut'
      readonly resultLaneCount: number
      readonly symbol: string
    }
  >
  readonly lanePointers: NativeLanePointer.Context
  readonly addressRoots: ReadonlySet<number>
  readonly addressStorage: Map<number, Value.Input>
  readonly mutableStorage: ReadonlyMap<number, ReadonlyArray<Value.Input>>
  readonly suspensionRegions: ReadonlyMap<Mir.Operation, Mir.SuspensionRegion>
  readonly lanesFor: (type: Mir.Type) => ReadonlyArray<Layout.CallingLane>
  readonly valueLanesFor: (type: Mir.Type) => ReadonlyArray<Layout.CallingLane>
  readonly laneType: (lane: Layout.CallingLane) => LlvmType.Type
  readonly readLocal: (local: Mir.LocalId) => ReadonlyArray<Value.Input>
  readonly readScalar: (local: Mir.LocalId) => Value.Input
  readonly coerceLane: (
    input: Value.Input,
    source: Layout.CallingLane,
    target: Layout.CallingLane,
    name: string,
  ) => Effect.Effect<Value.Input, LlvmError.LlvmError>
  readonly locate: (
    span: SourceSpan.SourceSpan,
    instruction: FunctionBody.Instruction | undefined,
  ) => Effect.Effect<void, LlvmError.LlvmError>
  readonly constantBytePointer: (
    base: Value.Input,
    offset: number,
    name: string,
  ) => Effect.Effect<Value.Value, LlvmError.LlvmError>
  readonly aggregateFieldOffset: (type: SilkType.Type, name: string) => number
  readonly emitHostFailure: (
    operation: Extract<Mir.Operation, { readonly _tag: 'Allocate' | 'HostWrite' }>,
  ) => Effect.Effect<void, LlvmError.LlvmError>
  readonly materializeAddressRoot: (root: Mir.LocalId) => Effect.Effect<void, LlvmError.LlvmError>
  readonly ensureAddressRoot: (root: Mir.LocalId) => Effect.Effect<void, LlvmError.LlvmError>
  readonly reloadAddressRoot: (root: number) => Effect.Effect<void, LlvmError.LlvmError>
  readonly reloadMutableRoots: (tag: string) => Effect.Effect<void, LlvmError.LlvmError>
  readonly storeMutable: (
    root: Mir.LocalId,
    values: ReadonlyArray<Value.Input>,
  ) => Effect.Effect<void, LlvmError.LlvmError>
  readonly dropThroughPlan: (
    plan: CleanupPlan.CleanupPlan,
    values: ReadonlyArray<Value.Input>,
    tag: string,
  ) => Effect.Effect<void, LlvmError.LlvmError>
  readonly failurePayload: (
    source: ReadonlyArray<Value.Input>,
    sourceType: DeclarationIndex.SemanticType,
    sourceTag: Value.Input | undefined,
    targetType: SilkType.Effect,
    mappings: ReadonlyArray<{ readonly source: number; readonly target: number }>,
    label: string,
  ) => Effect.Effect<ReadonlyArray<Value.Input>, LlvmError.LlvmError>
  readonly emitCallableBinary: (
    operator: Mir.BinaryOperator,
    left: Value.Input,
    right: Value.Input,
    operandMirType: Mir.Type,
    span: SourceSpan.SourceSpan,
    nameOrdinal: number,
  ) => Effect.Effect<Value.Input, LlvmError.LlvmError>
  readonly emitIntegerConversion: (
    input: Value.Input,
    sourceType: Mir.ScalarType,
    targetType: Mir.ScalarType,
    name: string,
  ) => Effect.Effect<Value.Input, LlvmError.LlvmError>
  readonly callValues: (
    target: NativeLoweringContext.DeclaredFunction,
    arguments_: ReadonlyArray<Value.Input>,
    name: string,
    suspension?: Mir.RunSuspendableEffectRegion,
  ) => Effect.Effect<ReadonlyArray<Value.Input>, LlvmError.LlvmError>
  readonly emitOrigin: (
    operation: EffectOperation,
    arguments_: ReadonlyArray<Value.Input>,
    name: string,
  ) => Effect.Effect<boolean, LlvmError.LlvmError>
  readonly joinSuspensionOutcome: (
    operation: EffectOperation,
    completedValues: ReadonlyArray<Value.Input>,
    name: string,
  ) => Effect.Effect<ReadonlyArray<Value.Input>, LlvmError.LlvmError>
  readonly returnStep: (
    status: bigint,
    values: ReadonlyArray<Value.Input>,
    tag: string,
  ) => Effect.Effect<void, LlvmError.LlvmError>
  readonly getTrapBlock: () => LlvmBlock.Block | undefined
  readonly setTrapBlock: (block: LlvmBlock.Block | undefined) => void
  readonly getCheckOrdinal: () => number
  readonly setCheckOrdinal: (ordinal: number) => void
}

export const emit = Effect.fnUntraced(function* (
  context: LoweringContext,
  operation: LinearOperation,
) {
  const {
    addressRoots,
    addressStorage,
    aggregateFieldOffset,
    body,
    builder,
    callValues,
    coerceLane,
    constantBytePointer,
    declared,
    dropThroughPlan,
    emitCallableBinary,
    emitHostFailure,
    emitIntegerConversion,
    emitOrigin,
    ensureAddressRoot,
    entry,
    f32,
    f64,
    failurePayload,
    free,
    i32,
    integerTypes,
    joinSuspensionOutcome,
    lanePointers,
    laneType,
    lanesFor,
    locals,
    locate,
    malloc,
    materializeAddressRoot,
    memcmp,
    mutableStorage,
    osRuntimes,
    pointer,
    program,
    readLocal,
    readScalar,
    reloadAddressRoot,
    reloadMutableRoots,
    returnStep,
    signedOverflowSignatures,
    standardWrite,
    staticPointers,
    storeMutable,
    suspensionRegions,
    unsignedOverflowSignatures,
    usizeType,
    valueLanesFor,
  } = context
  const initialTrapBlock = context.getTrapBlock()
  let trapBlock = initialTrapBlock
  let checkOrdinal = context.getCheckOrdinal()
  switch (operation._tag) {
    case 'BindMatch': {
      const physical = Layout.memberFieldSlots(
        operation.shape,
        operation.member,
        operation.binding.path,
      )
      if (physical === undefined) {
        throw new RangeError('LLVM match lost a pattern payload path')
      }
      const source = readLocal(operation.scrutinee)
      const sourceLanes = operation.shape.lanes
      const targetLanes = lanesFor(operation.binding.type)
      const selected: Array<Value.Input> = []
      for (const [targetOrdinal, ordinal] of physical.entries()) {
        const value = source.at(ordinal)
        const sourceLane = sourceLanes.at(ordinal)
        const targetLane = targetLanes.at(targetOrdinal)
        if (value === undefined || sourceLane === undefined || targetLane === undefined) {
          continue
        }
        selected.push(
          yield* coerceLane(
            value,
            sourceLane,
            targetLane,
            `match${operation.binding.destination.ordinal}_${targetOrdinal}_lane`,
          ),
        )
      }
      if (selected.length !== targetLanes.length) {
        throw new RangeError('LLVM match binding disagrees with its payload lanes')
      }
      locals.set(operation.binding.destination.ordinal, Object.freeze(selected))
      break
    }
    case 'Literal': {
      const lane = lanesFor(operation.type).at(0)
      if (lane === undefined) throw new RangeError('LLVM literal lost its lane')
      const physicalType = laneType(lane)
      const semantic = Mir.semanticType(operation.type)
      const floating = typeof semantic === 'string' ? Scalar.find(semantic) : undefined
      if (floating?.category === 'Floating') {
        locals.set(
          operation.destination.ordinal,
          Object.freeze([
            yield* Constant.floatingRaw(
              builder,
              physicalType,
              floating.spelling === 'f32' ? 'float' : 'double',
              FloatingPoint.littleEndianBytes({
                width: floating.spelling === 'f32' ? 32 : 64,
                bits: BigInt(operation.value),
              }),
            ),
          ]),
        )
        break
      }
      const unsigned =
        typeof semantic === 'string' && Scalar.find(semantic)?.signedness === 'Unsigned'
      locals.set(
        operation.destination.ordinal,
        Object.freeze([
          unsigned
            ? yield* Constant.integerUnsigned(builder, physicalType, BigInt(operation.value))
            : yield* Constant.integerSigned(builder, physicalType, BigInt(operation.value)),
        ]),
      )
      break
    }
    case 'StaticView': {
      const address = staticPointers.get(operation.data)
      if (address === undefined || usizeType === undefined) {
        throw new RangeError('LLVM static view lost its data placement or usize type')
      }
      locals.set(
        operation.destination.ordinal,
        Object.freeze([
          address,
          yield* Constant.integerUnsigned(builder, usizeType, operation.length),
        ]),
      )
      break
    }
    case 'StaticString': {
      const address = staticPointers.get(operation.data)
      if (address === undefined || usizeType === undefined) {
        throw new RangeError('LLVM static string lost its data placement or usize type')
      }
      locals.set(
        operation.destination.ordinal,
        Object.freeze([
          address,
          yield* Constant.integerUnsigned(builder, usizeType, operation.byteLength),
        ]),
      )
      break
    }
    case 'StringFromUtf8Unchecked': {
      locals.set(operation.destination.ordinal, readLocal(operation.bytes))
      break
    }
    case 'StringUtf8Bytes': {
      locals.set(operation.destination.ordinal, readLocal(operation.string))
      break
    }
    case 'StringByteLength': {
      const length = readLocal(operation.string).at(1)
      if (length === undefined) {
        throw new RangeError('LLVM string lost its byte-length lane')
      }
      locals.set(operation.destination.ordinal, Object.freeze([length]))
      break
    }
    case 'StringEqualsExact': {
      const [leftAddress, leftLength] = readLocal(operation.left)
      const [rightAddress, rightLength] = readLocal(operation.right)
      if (
        leftAddress === undefined ||
        leftLength === undefined ||
        rightAddress === undefined ||
        rightLength === undefined ||
        memcmp === undefined ||
        usizeType === undefined
      ) {
        throw new RangeError('LLVM string equality lost its lanes or runtime helper')
      }
      const lengthsEqual = yield* FunctionBody.integerCompare(
        body,
        'eq',
        leftLength,
        rightLength,
        `string${operation.destination.ordinal}_lengths_equal`,
      )
      const zeroLength = yield* Constant.integerUnsigned(builder, usizeType, 0n)
      const comparedLength = yield* FunctionBody.select(
        body,
        lengthsEqual,
        leftLength,
        zeroLength,
        `string${operation.destination.ordinal}_compared_length`,
      )
      const compared = yield* FunctionBody.callDirect(
        body,
        memcmp,
        [leftAddress, rightAddress, comparedLength],
        `string${operation.destination.ordinal}_memcmp`,
      )
      if (compared === undefined) {
        throw new RangeError('LLVM string equality produced no comparison result')
      }
      const zero = yield* Constant.integerSigned(builder, i32, 0n)
      const bytesEqual = yield* FunctionBody.integerCompare(
        body,
        'eq',
        compared,
        zero,
        `string${operation.destination.ordinal}_bytes_equal`,
      )
      const exact = yield* FunctionBody.binary(
        body,
        'and',
        lengthsEqual,
        bytesEqual,
        `string${operation.destination.ordinal}_exact`,
      )
      const selected = operation.negated
        ? yield* FunctionBody.integerCompare(
            body,
            'eq',
            exact,
            yield* Constant.integerUnsigned(builder, yield* LlvmType.integer(builder, 1), 0n),
            `string${operation.destination.ordinal}_negated`,
          )
        : exact
      locals.set(
        operation.destination.ordinal,
        Object.freeze([
          yield* FunctionBody.cast(
            body,
            'zext',
            selected,
            i32,
            `string${operation.destination.ordinal}_result`,
          ),
        ]),
      )
      break
    }
    case 'Allocate': {
      const [bytes, alignment] = readLocal(operation.layout)
      if (
        bytes === undefined ||
        alignment === undefined ||
        usizeType === undefined ||
        malloc === undefined
      ) {
        throw new RangeError('LLVM allocation lost its platform boundary')
      }
      const one = yield* Constant.integerUnsigned(builder, usizeType, 1n)
      const zero = yield* Constant.integerUnsigned(builder, usizeType, 0n)
      const padding = yield* FunctionBody.binary(
        body,
        'sub',
        alignment,
        one,
        `allocation${operation.destination.ordinal}_padding`,
      )
      const usizeBits = program.layout.target.pointerSize * 8
      let unsignedOverflowSignature = unsignedOverflowSignatures.get(usizeBits)
      if (unsignedOverflowSignature === undefined) {
        const i1 = yield* LlvmType.integer(builder, 1)
        unsignedOverflowSignature = Object.freeze({
          returnType: yield* LlvmType.structure(builder, [usizeType, i1]),
          parameters: Object.freeze([usizeType, usizeType]),
        })
        unsignedOverflowSignatures.set(usizeBits, unsignedOverflowSignature)
      }
      const requestPair = yield* Intrinsic.call(
        body,
        'uadd.with.overflow',
        [usizeType],
        [bytes, padding],
        `allocation${operation.destination.ordinal}_request_pair`,
        { signature: unsignedOverflowSignature },
      )
      if (requestPair === undefined) {
        throw new RangeError('LLVM allocation size calculation produced no value')
      }
      const requested = yield* FunctionBody.extractValue(
        body,
        requestPair,
        [0],
        `allocation${operation.destination.ordinal}_requested`,
      )
      const overflowed = yield* FunctionBody.extractValue(
        body,
        requestPair,
        [1],
        `allocation${operation.destination.ordinal}_overflowed`,
      )
      const empty = yield* FunctionBody.integerCompare(
        body,
        'eq',
        requested,
        zero,
        `allocation${operation.destination.ordinal}_empty`,
      )
      const physicalSize = yield* FunctionBody.select(
        body,
        empty,
        one,
        requested,
        `allocation${operation.destination.ordinal}_physical_size`,
      )
      const raw = yield* FunctionBody.callDirect(
        body,
        malloc,
        [physicalSize],
        `allocation${operation.destination.ordinal}_raw`,
      )
      if (raw === undefined) throw new RangeError('LLVM malloc returned no value')
      const rawAddress = yield* FunctionBody.cast(
        body,
        'ptrtoint',
        raw,
        usizeType,
        `allocation${operation.destination.ordinal}_context`,
      )
      const missing = yield* FunctionBody.integerCompare(
        body,
        'eq',
        rawAddress,
        zero,
        `allocation${operation.destination.ordinal}_missing`,
      )
      const rejected = yield* FunctionBody.binary(
        body,
        'or',
        overflowed,
        missing,
        `allocation${operation.destination.ordinal}_rejected`,
      )
      const failed = yield* LlvmBlock.make(
        body,
        `allocation${operation.destination.ordinal}_failure`,
      )
      const acquired = yield* LlvmBlock.make(
        body,
        `allocation${operation.destination.ordinal}_success`,
      )
      yield* FunctionBody.conditionalBranch(body, rejected, failed, acquired)
      yield* LlvmBlock.setInsertionPoint(body, failed)
      if (free === undefined) throw new RangeError('LLVM allocation lost release shim')
      yield* FunctionBody.callDirect(body, free, [raw])
      yield* emitHostFailure(operation)
      yield* LlvmBlock.setInsertionPoint(body, acquired)
      const advanced = yield* FunctionBody.binary(
        body,
        'add',
        rawAddress,
        padding,
        `allocation${operation.destination.ordinal}_advanced`,
      )
      const mask = yield* FunctionBody.binary(
        body,
        'sub',
        zero,
        alignment,
        `allocation${operation.destination.ordinal}_mask`,
      )
      const base = yield* FunctionBody.binary(
        body,
        'and',
        advanced,
        mask,
        `allocation${operation.destination.ordinal}_base`,
      )
      locals.set(
        operation.destination.ordinal,
        Object.freeze([base, bytes, alignment, one, rawAddress, one]),
      )
      break
    }
    case 'HostWrite': {
      const stream = readLocal(operation.stream).at(0)
      const [address, length] = readLocal(operation.bytes)
      if (
        stream === undefined ||
        address === undefined ||
        length === undefined ||
        standardWrite === undefined
      ) {
        throw new RangeError('LLVM standard-stream write lost its host boundary lanes')
      }
      const status = yield* FunctionBody.callDirect(
        body,
        standardWrite,
        [stream, address, length],
        `standard_stream${operation.destination.ordinal}_status`,
      )
      if (status === undefined) {
        throw new RangeError('LLVM standard-stream host returned no status')
      }
      const failedStatus = yield* FunctionBody.integerCompare(
        body,
        'ne',
        status,
        yield* Constant.integerUnsigned(builder, i32, 0n),
        `standard_stream${operation.destination.ordinal}_failed`,
      )
      const failed = yield* LlvmBlock.make(
        body,
        `standard_stream${operation.destination.ordinal}_failure`,
      )
      const written = yield* LlvmBlock.make(
        body,
        `standard_stream${operation.destination.ordinal}_success`,
      )
      yield* FunctionBody.conditionalBranch(body, failedStatus, failed, written)
      yield* LlvmBlock.setInsertionPoint(body, failed)
      yield* emitHostFailure(operation)
      yield* LlvmBlock.setInsertionPoint(body, written)
      locals.set(operation.destination.ordinal, Object.freeze([]))
      break
    }
    case 'OsCall': {
      const runtime = osRuntimes.get(operation.operation.name)
      if (runtime === undefined) {
        throw new RangeError(`LLVM OS runtime ${operation.operation.name} is unavailable`)
      }
      const arguments_ = operation.arguments.flatMap((argument) => [...readLocal(argument)])
      const resultLanes = lanesFor(operation.type)
      const openOutputs =
        runtime.abi === 'OpenOut'
          ? yield* Effect.forEach(resultLanes.slice(1), (lane, ordinal) =>
              FunctionBody.alloca(
                body,
                laneType(lane),
                `os${operation.destination.ordinal}_out${ordinal}`,
              ),
            )
          : Object.freeze([])
      const result = yield* FunctionBody.callDirect(
        body,
        runtime.handle,
        [...arguments_, ...openOutputs],
        `os${operation.destination.ordinal}`,
      )
      for (const root of [...addressRoots].sort((left, right) => left - right)) {
        yield* reloadAddressRoot(root)
      }
      if (runtime.resultLaneCount === 0) {
        locals.set(operation.destination.ordinal, Object.freeze([]))
        break
      }
      if (result === undefined) throw new RangeError('LLVM OS runtime returned no value')
      if (runtime.abi === 'OpenOut') {
        const values: Array<Value.Input> = [result]
        for (const [ordinal, output] of openOutputs.entries()) {
          const lane = resultLanes.at(ordinal + 1)
          if (lane === undefined) throw new RangeError('LLVM OS open runtime lost an output lane')
          values.push(
            yield* FunctionBody.load(
              body,
              laneType(lane),
              output,
              `os${operation.destination.ordinal}_out${ordinal}_value`,
            ),
          )
        }
        locals.set(operation.destination.ordinal, Object.freeze(values))
        break
      }
      if (runtime.resultLaneCount === 1) {
        locals.set(operation.destination.ordinal, Object.freeze([result]))
        break
      }
      const values: Array<Value.Input> = []
      for (let lane = 0; lane < runtime.resultLaneCount; lane += 1) {
        values.push(
          yield* FunctionBody.extractValue(
            body,
            result,
            [lane],
            `os${operation.destination.ordinal}_${lane}`,
          ),
        )
      }
      locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'RawBufferFrom': {
      const allocation = readLocal(operation.allocation)
      const count = readLocal(operation.count).at(0)
      const bytes = allocation.at(1)
      const alignment = allocation.at(2)
      if (
        count === undefined ||
        bytes === undefined ||
        alignment === undefined ||
        usizeType === undefined
      ) {
        throw new RangeError('LLVM RawBuffer construction lost its lanes')
      }
      const expected = yield* FunctionBody.binary(
        body,
        'mul',
        count,
        yield* Constant.integerUnsigned(builder, usizeType, BigInt(operation.stride)),
        `raw_buffer${operation.destination.ordinal}_bytes`,
      )
      const bytesMismatch = yield* FunctionBody.integerCompare(
        body,
        'ne',
        expected,
        bytes,
        `raw_buffer${operation.destination.ordinal}_bytes_mismatch`,
      )
      const alignmentMismatch = yield* FunctionBody.integerCompare(
        body,
        'ne',
        alignment,
        yield* Constant.integerUnsigned(builder, usizeType, BigInt(operation.elementAlignment)),
        `raw_buffer${operation.destination.ordinal}_alignment_mismatch`,
      )
      const invalid = yield* FunctionBody.binary(
        body,
        'or',
        bytesMismatch,
        alignmentMismatch,
        `raw_buffer${operation.destination.ordinal}_invalid`,
      )
      if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'raw_trap')
      const accepted = yield* LlvmBlock.make(
        body,
        `raw_buffer${operation.destination.ordinal}_accepted`,
      )
      yield* FunctionBody.conditionalBranch(body, invalid, trapBlock, accepted)
      yield* LlvmBlock.setInsertionPoint(body, accepted)
      locals.set(operation.destination.ordinal, Object.freeze([...allocation, count]))
      break
    }
    case 'RawBufferCount': {
      const address = readLocal(operation.buffer).at(0)
      const referenceType = entry.fn.localTypes.at(operation.buffer.ordinal)
      if (
        address === undefined ||
        referenceType?._tag !== 'Reference' ||
        !SilkType.isRawBuffer(referenceType.type.target) ||
        usizeType === undefined
      ) {
        throw new RangeError('LLVM RawBuffer.count lost its referenced buffer')
      }
      const value = yield* FunctionBody.load(
        body,
        usizeType,
        yield* constantBytePointer(
          address,
          aggregateFieldOffset(referenceType.type.target, 'count'),
          `raw_buffer_count${operation.destination.ordinal}_ptr`,
        ),
        `raw_buffer_count${operation.destination.ordinal}`,
      )
      locals.set(operation.destination.ordinal, Object.freeze([value]))
      break
    }
    case 'RawBufferSlot': {
      const address = readLocal(operation.buffer).at(0)
      const index = readLocal(operation.index).at(0)
      const element = Layout.entry(program.layout, operation.element)
      if (
        address === undefined ||
        index === undefined ||
        element === undefined ||
        usizeType === undefined
      ) {
        throw new RangeError('LLVM RawBuffer.slot lost its storage provenance')
      }
      const bufferType = SilkType.rawBuffer(operation.element)
      const count = yield* FunctionBody.load(
        body,
        usizeType,
        yield* constantBytePointer(
          address,
          aggregateFieldOffset(bufferType, 'count'),
          `raw_slot${operation.destination.ordinal}_count_ptr`,
        ),
        `raw_slot${operation.destination.ordinal}_count`,
      )
      const outOfBounds = yield* FunctionBody.integerCompare(
        body,
        'uge',
        index,
        count,
        `raw_slot${operation.destination.ordinal}_bounds`,
      )
      if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'raw_trap')
      const accepted = yield* LlvmBlock.make(
        body,
        `raw_slot${operation.destination.ordinal}_accepted`,
      )
      yield* FunctionBody.conditionalBranch(body, outOfBounds, trapBlock, accepted)
      yield* LlvmBlock.setInsertionPoint(body, accepted)
      const allocationOffset = aggregateFieldOffset(bufferType, '$allocation')
      const baseAddress = yield* FunctionBody.load(
        body,
        usizeType,
        yield* constantBytePointer(
          address,
          allocationOffset + aggregateFieldOffset(SilkType.allocation, '$base'),
          `raw_slot${operation.destination.ordinal}_base_ptr`,
        ),
        `raw_slot${operation.destination.ordinal}_base`,
      )
      const stride = Math.ceil(element.size / element.alignment) * element.alignment
      const offset = yield* FunctionBody.binary(
        body,
        'mul',
        index,
        yield* Constant.integerUnsigned(builder, usizeType, BigInt(stride)),
        `raw_slot${operation.destination.ordinal}_offset`,
      )
      const selected = yield* FunctionBody.binary(
        body,
        'add',
        baseAddress,
        offset,
        `raw_slot${operation.destination.ordinal}_address`,
      )
      locals.set(operation.destination.ordinal, Object.freeze([selected]))
      break
    }
    case 'RawBufferRead': {
      const address = readLocal(operation.buffer).at(0)
      const index = readLocal(operation.index).at(0)
      const element = Layout.entry(program.layout, operation.element)
      if (
        address === undefined ||
        index === undefined ||
        element === undefined ||
        usizeType === undefined
      ) {
        throw new RangeError('LLVM RawBuffer.read lost its storage provenance')
      }
      const bufferType = SilkType.rawBuffer(operation.element)
      const count = yield* FunctionBody.load(
        body,
        usizeType,
        yield* constantBytePointer(
          address,
          aggregateFieldOffset(bufferType, 'count'),
          `raw_read${operation.destination.ordinal}_count_ptr`,
        ),
        `raw_read${operation.destination.ordinal}_count`,
      )
      const outOfBounds = yield* FunctionBody.integerCompare(
        body,
        'uge',
        index,
        count,
        `raw_read${operation.destination.ordinal}_bounds`,
      )
      if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'raw_trap')
      const accepted = yield* LlvmBlock.make(
        body,
        `raw_read${operation.destination.ordinal}_accepted`,
      )
      yield* FunctionBody.conditionalBranch(body, outOfBounds, trapBlock, accepted)
      yield* LlvmBlock.setInsertionPoint(body, accepted)
      const allocationOffset = aggregateFieldOffset(bufferType, '$allocation')
      const baseAddress = yield* FunctionBody.load(
        body,
        usizeType,
        yield* constantBytePointer(
          address,
          allocationOffset + aggregateFieldOffset(SilkType.allocation, '$base'),
          `raw_read${operation.destination.ordinal}_base_ptr`,
        ),
        `raw_read${operation.destination.ordinal}_base`,
      )
      const stride = Math.ceil(element.size / element.alignment) * element.alignment
      const offset = yield* FunctionBody.binary(
        body,
        'mul',
        index,
        yield* Constant.integerUnsigned(builder, usizeType, BigInt(stride)),
        `raw_read${operation.destination.ordinal}_offset`,
      )
      const selected = yield* FunctionBody.binary(
        body,
        'add',
        baseAddress,
        offset,
        `raw_read${operation.destination.ordinal}_address`,
      )
      const base = yield* FunctionBody.cast(
        body,
        'inttoptr',
        selected,
        pointer,
        `raw_read${operation.destination.ordinal}_element_ptr`,
      )
      const lanes = Layout.callingShape(program.layout, operation.element)?.lanes
      if (lanes === undefined) throw new RangeError('LLVM RawBuffer.read lost its shape')
      const values: Array<Value.Input> = []
      for (const [ordinal, lane] of lanes.entries()) {
        const laneOffset = Layout.laneOffset(program.layout, operation.element, lane.path)
        if (laneOffset === undefined) {
          throw new RangeError('LLVM RawBuffer.read lost an element lane')
        }
        values.push(
          yield* FunctionBody.load(
            body,
            laneType(lane),
            yield* constantBytePointer(
              base,
              laneOffset,
              `raw_read${operation.destination.ordinal}_${ordinal}_ptr`,
            ),
            `raw_read${operation.destination.ordinal}_${ordinal}`,
          ),
        )
      }
      locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'RawBufferView': {
      const address = readLocal(operation.buffer).at(0)
      const offset = readLocal(operation.offset).at(0)
      const length = readLocal(operation.length).at(0)
      if (
        address === undefined ||
        offset === undefined ||
        length === undefined ||
        usizeType === undefined
      ) {
        throw new RangeError('LLVM RawBuffer.view lost its storage provenance')
      }
      const bufferType = SilkType.rawBuffer(operation.element)
      const count = yield* FunctionBody.load(
        body,
        usizeType,
        yield* constantBytePointer(
          address,
          aggregateFieldOffset(bufferType, 'count'),
          `raw_view${operation.destination.ordinal}_count_ptr`,
        ),
        `raw_view${operation.destination.ordinal}_count`,
      )
      const offsetOutOfBounds = yield* FunctionBody.integerCompare(
        body,
        'ugt',
        offset,
        count,
        `raw_view${operation.destination.ordinal}_offset_bounds`,
      )
      const remaining = yield* FunctionBody.binary(
        body,
        'sub',
        count,
        offset,
        `raw_view${operation.destination.ordinal}_remaining`,
      )
      const lengthOutOfBounds = yield* FunctionBody.integerCompare(
        body,
        'ugt',
        length,
        remaining,
        `raw_view${operation.destination.ordinal}_length_bounds`,
      )
      const invalid = yield* FunctionBody.binary(
        body,
        'or',
        offsetOutOfBounds,
        lengthOutOfBounds,
        `raw_view${operation.destination.ordinal}_invalid`,
      )
      if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'raw_trap')
      const accepted = yield* LlvmBlock.make(
        body,
        `raw_view${operation.destination.ordinal}_accepted`,
      )
      yield* FunctionBody.conditionalBranch(body, invalid, trapBlock, accepted)
      yield* LlvmBlock.setInsertionPoint(body, accepted)
      const allocationOffset = aggregateFieldOffset(bufferType, '$allocation')
      const baseAddress = yield* FunctionBody.load(
        body,
        usizeType,
        yield* constantBytePointer(
          address,
          allocationOffset + aggregateFieldOffset(SilkType.allocation, '$base'),
          `raw_view${operation.destination.ordinal}_base_ptr`,
        ),
        `raw_view${operation.destination.ordinal}_base`,
      )
      const byteOffset = yield* FunctionBody.binary(
        body,
        'mul',
        offset,
        yield* Constant.integerUnsigned(builder, usizeType, BigInt(operation.stride)),
        `raw_view${operation.destination.ordinal}_byte_offset`,
      )
      const selected = yield* FunctionBody.binary(
        body,
        'add',
        baseAddress,
        byteOffset,
        `raw_view${operation.destination.ordinal}_address`,
      )
      const base = yield* FunctionBody.cast(
        body,
        'inttoptr',
        selected,
        pointer,
        `raw_view${operation.destination.ordinal}_ptr`,
      )
      locals.set(operation.destination.ordinal, Object.freeze([base, length]))
      break
    }
    case 'RawBufferCopy': {
      const address = readLocal(operation.buffer).at(0)
      const offset = readLocal(operation.offset).at(0)
      const sourceLanes = readLocal(operation.source)
      const sourceAddress = sourceLanes.at(0)
      const sourceLength = sourceLanes.at(1)
      const length = readLocal(operation.length).at(0)
      const element = Layout.entry(program.layout, operation.element)
      if (
        address === undefined ||
        offset === undefined ||
        sourceAddress === undefined ||
        sourceLength === undefined ||
        length === undefined ||
        element === undefined ||
        usizeType === undefined
      ) {
        throw new RangeError('LLVM RawBuffer.copy lost its storage provenance')
      }
      const bufferType = SilkType.rawBuffer(operation.element)
      const count = yield* FunctionBody.load(
        body,
        usizeType,
        yield* constantBytePointer(
          address,
          aggregateFieldOffset(bufferType, 'count'),
          `raw_copy${operation.destination.ordinal}_count_ptr`,
        ),
        `raw_copy${operation.destination.ordinal}_count`,
      )
      const offsetOutOfBounds = yield* FunctionBody.integerCompare(
        body,
        'ugt',
        offset,
        count,
        `raw_copy${operation.destination.ordinal}_offset_bounds`,
      )
      const remaining = yield* FunctionBody.binary(
        body,
        'sub',
        count,
        offset,
        `raw_copy${operation.destination.ordinal}_remaining`,
      )
      const lengthOutOfBounds = yield* FunctionBody.integerCompare(
        body,
        'ugt',
        length,
        remaining,
        `raw_copy${operation.destination.ordinal}_length_bounds`,
      )
      const sourceOutOfBounds = yield* FunctionBody.integerCompare(
        body,
        'ugt',
        length,
        sourceLength,
        `raw_copy${operation.destination.ordinal}_source_bounds`,
      )
      const invalidRange = yield* FunctionBody.binary(
        body,
        'or',
        offsetOutOfBounds,
        lengthOutOfBounds,
        `raw_copy${operation.destination.ordinal}_range`,
      )
      const invalid = yield* FunctionBody.binary(
        body,
        'or',
        invalidRange,
        sourceOutOfBounds,
        `raw_copy${operation.destination.ordinal}_invalid`,
      )
      if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'raw_trap')
      const accepted = yield* LlvmBlock.make(
        body,
        `raw_copy${operation.destination.ordinal}_accepted`,
      )
      yield* FunctionBody.conditionalBranch(body, invalid, trapBlock, accepted)
      yield* LlvmBlock.setInsertionPoint(body, accepted)
      const allocationOffset = aggregateFieldOffset(bufferType, '$allocation')
      const baseAddress = yield* FunctionBody.load(
        body,
        usizeType,
        yield* constantBytePointer(
          address,
          allocationOffset + aggregateFieldOffset(SilkType.allocation, '$base'),
          `raw_copy${operation.destination.ordinal}_base_ptr`,
        ),
        `raw_copy${operation.destination.ordinal}_base`,
      )
      const stride = yield* Constant.integerUnsigned(builder, usizeType, BigInt(operation.stride))
      const byteOffset = yield* FunctionBody.binary(
        body,
        'mul',
        offset,
        stride,
        `raw_copy${operation.destination.ordinal}_byte_offset`,
      )
      const selected = yield* FunctionBody.binary(
        body,
        'add',
        baseAddress,
        byteOffset,
        `raw_copy${operation.destination.ordinal}_address`,
      )
      const target = yield* FunctionBody.cast(
        body,
        'inttoptr',
        selected,
        pointer,
        `raw_copy${operation.destination.ordinal}_ptr`,
      )
      const byteLength = yield* FunctionBody.binary(
        body,
        'mul',
        length,
        stride,
        `raw_copy${operation.destination.ordinal}_bytes`,
      )
      // memmove, not memcpy: an overlapping source and destination is a defined move.
      yield* Intrinsic.memmove(body, target, sourceAddress, byteLength, {
        destinationAlignment: yield* Alignment.fromByteUnits(element.alignment),
        sourceAlignment: yield* Alignment.fromByteUnits(element.alignment),
      })
      locals.set(operation.destination.ordinal, Object.freeze([]))
      break
    }
    case 'RawBufferFill': {
      const address = readLocal(operation.buffer).at(0)
      const offset = readLocal(operation.offset).at(0)
      const length = readLocal(operation.length).at(0)
      const value = readLocal(operation.value).at(0)
      if (
        address === undefined ||
        offset === undefined ||
        length === undefined ||
        value === undefined ||
        usizeType === undefined
      ) {
        throw new RangeError('LLVM RawBuffer.fill lost its storage provenance')
      }
      const bufferType = SilkType.rawBuffer('u8')
      const count = yield* FunctionBody.load(
        body,
        usizeType,
        yield* constantBytePointer(
          address,
          aggregateFieldOffset(bufferType, 'count'),
          `raw_fill${operation.destination.ordinal}_count_ptr`,
        ),
        `raw_fill${operation.destination.ordinal}_count`,
      )
      const offsetOutOfBounds = yield* FunctionBody.integerCompare(
        body,
        'ugt',
        offset,
        count,
        `raw_fill${operation.destination.ordinal}_offset_bounds`,
      )
      const remaining = yield* FunctionBody.binary(
        body,
        'sub',
        count,
        offset,
        `raw_fill${operation.destination.ordinal}_remaining`,
      )
      const lengthOutOfBounds = yield* FunctionBody.integerCompare(
        body,
        'ugt',
        length,
        remaining,
        `raw_fill${operation.destination.ordinal}_length_bounds`,
      )
      const invalid = yield* FunctionBody.binary(
        body,
        'or',
        offsetOutOfBounds,
        lengthOutOfBounds,
        `raw_fill${operation.destination.ordinal}_invalid`,
      )
      if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'raw_trap')
      const accepted = yield* LlvmBlock.make(
        body,
        `raw_fill${operation.destination.ordinal}_accepted`,
      )
      yield* FunctionBody.conditionalBranch(body, invalid, trapBlock, accepted)
      yield* LlvmBlock.setInsertionPoint(body, accepted)
      const allocationOffset = aggregateFieldOffset(bufferType, '$allocation')
      const baseAddress = yield* FunctionBody.load(
        body,
        usizeType,
        yield* constantBytePointer(
          address,
          allocationOffset + aggregateFieldOffset(SilkType.allocation, '$base'),
          `raw_fill${operation.destination.ordinal}_base_ptr`,
        ),
        `raw_fill${operation.destination.ordinal}_base`,
      )
      const selected = yield* FunctionBody.binary(
        body,
        'add',
        baseAddress,
        offset,
        `raw_fill${operation.destination.ordinal}_address`,
      )
      const target = yield* FunctionBody.cast(
        body,
        'inttoptr',
        selected,
        pointer,
        `raw_fill${operation.destination.ordinal}_ptr`,
      )
      yield* Intrinsic.memset(body, target, value, length)
      locals.set(operation.destination.ordinal, Object.freeze([]))
      break
    }
    case 'SlotWrite': {
      const address = readLocal(operation.slot).at(0)
      if (address === undefined || usizeType === undefined) {
        throw new RangeError('LLVM Slot.write lost its address')
      }
      const base = yield* FunctionBody.cast(
        body,
        'inttoptr',
        address,
        pointer,
        `slot_write${operation.destination.ordinal}_base`,
      )
      const values = readLocal(operation.value)
      const lanes = Layout.callingShape(program.layout, operation.element)?.lanes
      if (lanes === undefined) throw new RangeError('LLVM Slot.write lost its shape')
      for (const [ordinal, lane] of lanes.entries()) {
        const value = values.at(ordinal)
        const offset = Layout.laneOffset(program.layout, operation.element, lane.path)
        if (value === undefined || offset === undefined) {
          throw new RangeError('LLVM Slot.write lost an element lane')
        }
        yield* FunctionBody.store(
          body,
          value,
          yield* constantBytePointer(
            base,
            offset,
            `slot_write${operation.destination.ordinal}_${ordinal}_ptr`,
          ),
        )
      }
      locals.set(operation.destination.ordinal, Object.freeze([]))
      break
    }
    case 'ValidateLayout': {
      const bytes = readLocal(operation.bytes).at(0)
      const alignment = readLocal(operation.alignment).at(0)
      if (bytes === undefined || alignment === undefined || usizeType === undefined) {
        throw new RangeError('LLVM layout validation lost its operands')
      }
      const name = `validate${operation.destination.ordinal}`
      const zero = yield* Constant.integerUnsigned(builder, usizeType, 0n)
      const one = yield* Constant.integerUnsigned(builder, usizeType, 1n)
      const nonZero = yield* FunctionBody.integerCompare(
        body,
        'ne',
        alignment,
        zero,
        `${name}_nonzero`,
      )
      const decremented = yield* FunctionBody.binary(
        body,
        'sub',
        alignment,
        one,
        `${name}_decrement`,
      )
      const masked = yield* FunctionBody.binary(body, 'and', alignment, decremented, `${name}_mask`)
      const powerOfTwo = yield* FunctionBody.integerCompare(
        body,
        'eq',
        masked,
        zero,
        `${name}_pow2`,
      )
      const valid = yield* FunctionBody.binary(body, 'and', nonZero, powerOfTwo, `${name}_valid`)
      const members = operation.type.type.members
      const layoutOrdinal = members.findIndex((member) => SilkType.equals(member, SilkType.layout))
      const invalidOrdinal = members.findIndex((member) =>
        SilkType.equals(member, SilkType.invalidAlignment),
      )
      if (layoutOrdinal < 0 || invalidOrdinal < 0) {
        throw new RangeError('LLVM layout validation lost its union members')
      }
      const tag = yield* FunctionBody.select(
        body,
        valid,
        yield* Constant.integerSigned(builder, i32, BigInt(layoutOrdinal)),
        yield* Constant.integerSigned(builder, i32, BigInt(invalidOrdinal)),
        `${name}_tag`,
      )
      // Layout packs {bytes, alignment}; InvalidAlignment packs {alignment} at slot 0.
      const first = yield* FunctionBody.select(body, valid, bytes, alignment, `${name}_slot0`)
      const second = yield* FunctionBody.select(body, valid, alignment, zero, `${name}_slot1`)
      const lanes = lanesFor(operation.type)
      const values: Array<Value.Input> = [tag, first, second]
      while (values.length < lanes.length) {
        const lane = lanes.at(values.length)
        if (lane === undefined) break
        values.push(yield* Constant.nullValue(builder, laneType(lane)))
      }
      locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'RepeatLayout': {
      const layoutValues = readLocal(operation.layout)
      const bytes = layoutValues.at(0)
      const alignment = layoutValues.at(1)
      const count = readLocal(operation.count).at(0)
      if (
        bytes === undefined ||
        alignment === undefined ||
        count === undefined ||
        usizeType === undefined
      ) {
        throw new RangeError('LLVM repeated layout lost its operands')
      }
      const name = `repeat${operation.destination.ordinal}`
      const zero = yield* Constant.integerUnsigned(builder, usizeType, 0n)
      const one = yield* Constant.integerUnsigned(builder, usizeType, 1n)
      const maximum = yield* Constant.integerUnsigned(
        builder,
        usizeType,
        program.layout.target.pointerSize === 4 ? 4294967295n : 18446744073709551615n,
      )
      const alignmentZero = yield* FunctionBody.integerCompare(
        body,
        'eq',
        alignment,
        zero,
        `${name}_alignment_zero`,
      )
      const safeAlignment = yield* FunctionBody.select(
        body,
        alignmentZero,
        one,
        alignment,
        `${name}_safe_alignment`,
      )
      const summed = yield* FunctionBody.binary(
        body,
        'add',
        bytes,
        yield* FunctionBody.binary(body, 'sub', safeAlignment, one, `${name}_pad`),
        `${name}_summed`,
      )
      const quotient = yield* FunctionBody.binary(
        body,
        'udiv',
        summed,
        safeAlignment,
        `${name}_quotient`,
      )
      const rounded = yield* FunctionBody.binary(
        body,
        'mul',
        quotient,
        safeAlignment,
        `${name}_rounded`,
      )
      const stride = yield* FunctionBody.select(
        body,
        alignmentZero,
        zero,
        rounded,
        `${name}_stride`,
      )
      const countZero = yield* FunctionBody.integerCompare(
        body,
        'eq',
        count,
        zero,
        `${name}_count_zero`,
      )
      const safeCount = yield* FunctionBody.select(
        body,
        countZero,
        one,
        count,
        `${name}_safe_count`,
      )
      const budget = yield* FunctionBody.binary(body, 'udiv', maximum, safeCount, `${name}_budget`)
      const exceeds = yield* FunctionBody.integerCompare(
        body,
        'ugt',
        stride,
        budget,
        `${name}_exceeds`,
      )
      const countPositive = yield* FunctionBody.integerCompare(
        body,
        'ne',
        count,
        zero,
        `${name}_count_positive`,
      )
      // Rounding up can wrap the integer itself; classify that as overflow directly.
      const headroom = yield* FunctionBody.binary(
        body,
        'sub',
        maximum,
        yield* FunctionBody.binary(body, 'sub', safeAlignment, one, `${name}_pad2`),
        `${name}_headroom`,
      )
      const huge = yield* FunctionBody.integerCompare(body, 'ugt', bytes, headroom, `${name}_huge`)
      const exceedsOrHuge = yield* FunctionBody.binary(
        body,
        'or',
        exceeds,
        huge,
        `${name}_exceeds_or_huge`,
      )
      const overflow = yield* FunctionBody.binary(
        body,
        'and',
        countPositive,
        exceedsOrHuge,
        `${name}_overflow`,
      )
      const total = yield* FunctionBody.binary(body, 'mul', stride, count, `${name}_total`)
      const members = operation.type.type.members
      const layoutOrdinal = members.findIndex((member) => SilkType.equals(member, SilkType.layout))
      const overflowOrdinal = members.findIndex((member) =>
        SilkType.equals(member, SilkType.layoutOverflow),
      )
      if (layoutOrdinal < 0 || overflowOrdinal < 0) {
        throw new RangeError('LLVM repeated layout lost its union members')
      }
      const tag = yield* FunctionBody.select(
        body,
        overflow,
        yield* Constant.integerSigned(builder, i32, BigInt(overflowOrdinal)),
        yield* Constant.integerSigned(builder, i32, BigInt(layoutOrdinal)),
        `${name}_tag`,
      )
      const totalOut = yield* FunctionBody.select(body, overflow, zero, total, `${name}_bytes`)
      const lanes = lanesFor(operation.type)
      const values: Array<Value.Input> = [tag, totalOut, alignment]
      while (values.length < lanes.length) {
        const lane = lanes.at(values.length)
        if (lane === undefined) break
        values.push(yield* Constant.nullValue(builder, laneType(lane)))
      }
      locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'SlotTake':
    case 'SlotCopy': {
      const address = readLocal(operation.slot).at(0)
      if (address === undefined || usizeType === undefined) {
        throw new RangeError('LLVM Slot.take lost its address')
      }
      const base = yield* FunctionBody.cast(
        body,
        'inttoptr',
        address,
        pointer,
        `slot_take${operation.destination.ordinal}_base`,
      )
      const lanes = Layout.callingShape(program.layout, operation.element)?.lanes
      if (lanes === undefined) throw new RangeError('LLVM Slot.take lost its shape')
      const values: Array<Value.Input> = []
      for (const [ordinal, lane] of lanes.entries()) {
        const offset = Layout.laneOffset(program.layout, operation.element, lane.path)
        if (offset === undefined) throw new RangeError('LLVM Slot.take lost a lane')
        values.push(
          yield* FunctionBody.load(
            body,
            laneType(lane),
            yield* constantBytePointer(
              base,
              offset,
              `slot_take${operation.destination.ordinal}_${ordinal}_ptr`,
            ),
            `slot_take${operation.destination.ordinal}_${ordinal}`,
          ),
        )
      }
      locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'SlotDrop': {
      const address = readLocal(operation.slot).at(0)
      if (address === undefined || usizeType === undefined) {
        throw new RangeError('LLVM Slot.drop lost its address')
      }
      const base = yield* FunctionBody.cast(
        body,
        'inttoptr',
        address,
        pointer,
        `slot_drop${operation.destination.ordinal}_base`,
      )
      const lanes = Layout.callingShape(program.layout, operation.element)?.lanes
      if (lanes === undefined) throw new RangeError('LLVM Slot.drop lost its shape')
      const values: Array<Value.Input> = []
      for (const [ordinal, lane] of lanes.entries()) {
        const offset = Layout.laneOffset(program.layout, operation.element, lane.path)
        if (offset === undefined) throw new RangeError('LLVM Slot.drop lost a lane')
        values.push(
          yield* FunctionBody.load(
            body,
            laneType(lane),
            yield* constantBytePointer(
              base,
              offset,
              `slot_drop${operation.destination.ordinal}_${ordinal}_ptr`,
            ),
            `slot_drop${operation.destination.ordinal}_${ordinal}`,
          ),
        )
      }
      yield* dropThroughPlan(
        operation.cleanup,
        Object.freeze(values),
        `slot_drop${operation.destination.ordinal}`,
      )
      locals.set(operation.destination.ordinal, Object.freeze([]))
      break
    }
    case 'Move': {
      const sourceType = entry.fn.localTypes.at(operation.source.ordinal)
      if (sourceType?._tag === 'Bottom') {
        const destinationType = entry.fn.localTypes.at(operation.destination.ordinal)
        if (destinationType === undefined)
          throw new RangeError('Bottom move lost its destination type')
        const placeholders: Array<Value.Input> = []
        for (const lane of lanesFor(destinationType)) {
          placeholders.push(yield* Constant.nullValue(builder, laneType(lane)))
        }
        locals.set(operation.destination.ordinal, Object.freeze(placeholders))
        break
      }
      locals.set(operation.destination.ordinal, readLocal(operation.source))
      break
    }
    case 'BeginLoan': {
      if (operation.sourceType._tag === 'Slice') {
        locals.set(operation.destination.ordinal, readLocal(operation.root))
        break
      }
      const rootType = entry.fn.localTypes.at(operation.root.ordinal)
      const rootSemantic = rootType === undefined ? undefined : Mir.semanticType(rootType)
      if (rootSemantic === undefined)
        throw new RangeError('LLVM borrow formation lost its root type')
      if (SilkType.isSlice(rootSemantic)) {
        const [selector, ...suffixSelectors] = operation.selectors
        const [base, length] = readLocal(operation.root)
        if (
          selector?._tag !== 'SliceElementSelector' ||
          base === undefined ||
          length === undefined ||
          operation.type._tag !== 'Reference'
        ) {
          throw new RangeError('LLVM slice borrow lost its canonical lanes')
        }
        if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
        const index = readScalar(selector.index)
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          index,
          length,
          `borrow${checkOrdinal}_in_bounds`,
        )
        yield* locate(selector.provenance.span, yield* Value.instruction(body, inBounds))
        const continuation = yield* LlvmBlock.make(body, `borrow${checkOrdinal}_ok`)
        yield* FunctionBody.conditionalBranch(body, inBounds, continuation, trapBlock)
        yield* LlvmBlock.setInsertionPoint(body, continuation)
        const sliceLayout = Layout.entry(program.layout, rootSemantic)
        if (sliceLayout?.representation._tag !== 'Slice') {
          throw new RangeError('LLVM slice borrow lost its compiler layout')
        }
        const elementOffset = yield* FunctionBody.binary(
          body,
          'mul',
          index,
          yield* Constant.integerUnsigned(
            builder,
            usizeType ?? i32,
            BigInt(sliceLayout.representation.stride),
          ),
          `borrow${operation.destination.ordinal}_element_offset`,
        )
        const staticSelectors: Array<Layout.Selector> = []
        for (const candidate of suffixSelectors) {
          if (candidate._tag === 'FieldSelector') {
            staticSelectors.push(candidate.field)
          } else if (candidate._tag === 'ElementSelector' && candidate.index._tag === 'Proven') {
            staticSelectors.push(
              Object.freeze({
                _tag: 'ElementSelector',
                index: candidate.index.value,
              }),
            )
          } else {
            throw new RangeError('LLVM nested runtime slice borrow is not canonical')
          }
        }
        const staticOffset = Layout.laneOffset(
          program.layout,
          rootSemantic.element,
          staticSelectors,
        )
        if (staticOffset === undefined) {
          throw new RangeError('LLVM slice borrow lost its selected layout')
        }
        const offset =
          staticOffset === 0
            ? elementOffset
            : yield* FunctionBody.binary(
                body,
                'add',
                elementOffset,
                yield* Constant.integerUnsigned(builder, usizeType ?? i32, BigInt(staticOffset)),
                `borrow${operation.destination.ordinal}_static_offset`,
              )
        const projected = yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          base,
          offset,
          `borrow${operation.destination.ordinal}_projected`,
        )
        locals.set(operation.destination.ordinal, Object.freeze([projected]))
        checkOrdinal += 1
        break
      }
      let selected = SilkType.isReference(rootSemantic) ? rootSemantic.target : rootSemantic
      let staticOffset = 0
      const dynamicOffsets: Array<{
        readonly local: Mir.LocalId
        readonly stride: number
        readonly length: number
        readonly span: SourceSpan.SourceSpan
      }> = []
      for (const selector of operation.selectors) {
        const selectedLayout = Layout.entry(program.layout, selected)
        if (selector._tag === 'FieldSelector') {
          if (selectedLayout?.representation._tag !== 'Aggregate')
            throw new RangeError('LLVM borrow field lost its aggregate layout')
          const field = selectedLayout.representation.fields.find(
            (candidate) =>
              candidate.id.ordinal === selector.field.ordinal &&
              candidate.id.struct.sourceId === selector.field.struct.sourceId &&
              candidate.id.struct.ordinal === selector.field.struct.ordinal,
          )
          if (field === undefined) throw new RangeError('LLVM borrow field lost its field layout')
          staticOffset += field.offset
          selected = field.type
          continue
        }
        if (
          selector._tag !== 'ElementSelector' ||
          selectedLayout?.representation._tag !== 'Repeated'
        )
          throw new RangeError('LLVM borrow element lost its repeated layout')
        if (selector.index._tag === 'Proven') {
          staticOffset += selector.index.value * selectedLayout.representation.stride
        } else {
          dynamicOffsets.push(
            Object.freeze({
              local: selector.index.local,
              stride: selectedLayout.representation.stride,
              length: selector.length,
              span: selector.provenance.span,
            }),
          )
        }
        selected = selectedLayout.representation.element
      }
      let dynamicOffset: Value.Input | undefined
      for (const [ordinal, offset] of dynamicOffsets.entries()) {
        const index = readScalar(offset.local)
        const length = yield* Constant.integerUnsigned(
          builder,
          usizeType ?? i32,
          BigInt(offset.length),
        )
        if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          index,
          length,
          `borrow${checkOrdinal}_${ordinal}_in_bounds`,
        )
        yield* locate(offset.span, yield* Value.instruction(body, inBounds))
        const continuation = yield* LlvmBlock.make(body, `borrow${checkOrdinal}_${ordinal}_ok`)
        yield* FunctionBody.conditionalBranch(body, inBounds, continuation, trapBlock)
        yield* LlvmBlock.setInsertionPoint(body, continuation)
        const scaled = yield* FunctionBody.binary(
          body,
          'mul',
          index,
          yield* Constant.integerUnsigned(builder, usizeType ?? i32, BigInt(offset.stride)),
          `borrow${operation.destination.ordinal}_${ordinal}_scaled`,
        )
        dynamicOffset =
          dynamicOffset === undefined
            ? scaled
            : yield* FunctionBody.binary(
                body,
                'add',
                dynamicOffset,
                scaled,
                `borrow${operation.destination.ordinal}_${ordinal}_offset`,
              )
      }
      if (staticOffset !== 0) {
        const constant = yield* Constant.integerUnsigned(
          builder,
          usizeType ?? i32,
          BigInt(staticOffset),
        )
        dynamicOffset =
          dynamicOffset === undefined
            ? constant
            : yield* FunctionBody.binary(
                body,
                'add',
                dynamicOffset,
                constant,
                `borrow${operation.destination.ordinal}_static_offset`,
              )
      }
      let rootBase: Value.Input | undefined
      if (SilkType.isReference(rootSemantic)) {
        const address = readLocal(operation.root).at(0)
        if (address === undefined)
          throw new RangeError('LLVM projected borrow lost its reference address')
        rootBase = yield* FunctionBody.cast(
          body,
          'inttoptr',
          address,
          pointer,
          `borrow${operation.destination.ordinal}_base`,
        )
      } else {
        yield* materializeAddressRoot(operation.root)
        rootBase = addressStorage.get(operation.root.ordinal)
      }
      if (rootBase === undefined) throw new RangeError('LLVM borrow formation lost its root')
      const projected =
        dynamicOffset === undefined
          ? rootBase
          : yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              rootBase,
              dynamicOffset,
              `borrow${operation.destination.ordinal}_projected`,
            )
      if (operation.type._tag === 'Reference') {
        locals.set(operation.destination.ordinal, Object.freeze([projected]))
        break
      }
      if (operation.sourceType._tag !== 'FixedArray') {
        throw new RangeError('LLVM slice formation requires an array root')
      }
      locals.set(
        operation.destination.ordinal,
        Object.freeze([
          projected,
          yield* Constant.integerUnsigned(
            builder,
            usizeType ?? i32,
            BigInt(operation.sourceType.type.length),
          ),
        ]),
      )
      break
    }
    case 'EndLoan':
      break
    case 'SliceLength': {
      const length = readLocal(operation.slice).at(1)
      if (length === undefined) throw new RangeError('LLVM slice lost its length lane')
      locals.set(operation.destination.ordinal, Object.freeze([length]))
      break
    }
    case 'ConvertUnion': {
      const source = readLocal(operation.source)
      const targetWidth = operation.targetShape.laneCount
      const zero = yield* Constant.integerSigned(builder, i32, 0n)
      const sourceLanes = operation.sourceShape.lanes
      const targetLanes = operation.targetShape.lanes
      if (operation.conversion === 'Inject') {
        const mapping = operation.mappings.at(0)
        if (mapping === undefined) {
          throw new RangeError('LLVM union injection has no member map')
        }
        const tag = yield* Constant.integerSigned(builder, i32, BigInt(mapping.targetOrdinal))
        const payload: Array<Value.Input> = []
        for (let ordinal = 0; ordinal < Math.max(0, targetWidth - 1); ordinal += 1) {
          const targetLane = targetLanes.at(ordinal + 1)
          if (targetLane === undefined) {
            throw new RangeError('LLVM union injection lost a target payload lane')
          }
          const input = source.at(ordinal)
          const sourceLane = sourceLanes.at(ordinal)
          payload.push(
            input === undefined || sourceLane === undefined
              ? yield* Constant.nullValue(builder, laneType(targetLane))
              : yield* coerceLane(
                  input,
                  sourceLane,
                  targetLane,
                  `union${operation.destination.ordinal}_${ordinal}_inject`,
                ),
          )
        }
        locals.set(operation.destination.ordinal, Object.freeze([tag, ...payload]))
        break
      }
      const sourceTag = source.at(0)
      if (sourceTag === undefined) {
        throw new RangeError('LLVM union widening has no source tag')
      }
      let tag: Value.Input = zero
      for (const [ordinal, mapping] of operation.mappings.entries()) {
        const sourceOrdinal = yield* Constant.integerSigned(
          builder,
          i32,
          BigInt(mapping.sourceOrdinal),
        )
        const matches = yield* FunctionBody.integerCompare(
          body,
          'eq',
          sourceTag,
          sourceOrdinal,
          `union${operation.destination.ordinal}_${ordinal}_matches`,
        )
        const targetOrdinal = yield* Constant.integerSigned(
          builder,
          i32,
          BigInt(mapping.targetOrdinal),
        )
        tag = yield* FunctionBody.select(
          body,
          matches,
          targetOrdinal,
          tag,
          `union${operation.destination.ordinal}_${ordinal}_tag`,
        )
      }
      const payload: Array<Value.Input> = []
      for (let ordinal = 0; ordinal < Math.max(0, targetWidth - 1); ordinal += 1) {
        const targetLane = targetLanes.at(ordinal + 1)
        if (targetLane === undefined) {
          throw new RangeError('LLVM union widening lost a target payload lane')
        }
        const input = source.at(ordinal + 1)
        const sourceLane = sourceLanes.at(ordinal + 1)
        payload.push(
          input === undefined || sourceLane === undefined
            ? yield* Constant.nullValue(builder, laneType(targetLane))
            : yield* coerceLane(
                input,
                sourceLane,
                targetLane,
                `union${operation.destination.ordinal}_${ordinal}_widen`,
              ),
        )
      }
      locals.set(operation.destination.ordinal, Object.freeze([tag, ...payload]))
      break
    }
    case 'Construct':
      locals.set(
        operation.destination.ordinal,
        Object.freeze(operation.fields.flatMap((field) => [...readLocal(field.value)])),
      )
      break
    case 'ConstructArray':
      locals.set(
        operation.destination.ordinal,
        Object.freeze(operation.elements.flatMap((element) => [...readLocal(element)])),
      )
      break
    case 'Project': {
      const sourceType = entry.fn.localTypes.at(operation.source.ordinal)
      if (sourceType === undefined) {
        throw new RangeError('Backend projection lost its source type')
      }
      const sourceLanes = lanesFor(sourceType)
      const sourceValues = readLocal(operation.source)
      const projected = sourceLanes.flatMap((lane, index) => {
        const first = lane.path.at(0)
        const selected = sourceValues.at(index)
        return first !== undefined &&
          first._tag === 'FieldId' &&
          selected !== undefined &&
          first.ordinal === operation.field.ordinal &&
          first.struct.sourceId === operation.field.struct.sourceId &&
          first.struct.ordinal === operation.field.struct.ordinal
          ? [selected]
          : []
      })
      locals.set(operation.destination.ordinal, Object.freeze(projected))
      break
    }
    case 'ReadPlace': {
      const sourceType = entry.fn.localTypes.at(operation.root.ordinal)
      if (sourceType === undefined) {
        throw new RangeError('Backend place read lost its root type')
      }
      const sourceSemantic = Mir.semanticType(sourceType)
      if (SilkType.isReference(sourceSemantic)) {
        // The place lives on the referenced target: static field offsets off the
        // borrow's address, one load per lane of the projected value.
        const address = readLocal(operation.root).at(0)
        if (address === undefined) throw new RangeError('LLVM reference read lost its address')
        const base = yield* FunctionBody.cast(
          body,
          'inttoptr',
          address,
          pointer,
          `reference_read${operation.destination.ordinal}_base`,
        )
        const staticSelectors: Array<Layout.Selector> = []
        for (const candidate of operation.selectors) {
          if (candidate._tag !== 'FieldSelector')
            throw new RangeError('LLVM reference place supports only field selectors')
          staticSelectors.push(candidate.field)
        }
        const target = sourceSemantic.target
        const values: Array<Value.Input> = []
        for (const [ordinal, lane] of lanesFor(operation.type).entries()) {
          const offset = Layout.laneOffset(program.layout, target, [
            ...staticSelectors,
            ...lane.path,
          ])
          if (offset === undefined) throw new RangeError('LLVM reference read lost a lane offset')
          values.push(
            yield* FunctionBody.load(
              body,
              laneType(lane),
              yield* constantBytePointer(
                base,
                offset,
                `reference_read${operation.destination.ordinal}_${ordinal}_ptr`,
              ),
              `reference_read${operation.destination.ordinal}_${ordinal}`,
            ),
          )
        }
        locals.set(operation.destination.ordinal, Object.freeze(values))
        break
      }
      if (SilkType.isSlice(sourceSemantic)) {
        const [selector, ...suffixSelectors] = operation.selectors
        if (selector?._tag !== 'SliceElementSelector') {
          throw new RangeError('LLVM slice read lost its runtime element selector')
        }
        const [base, length] = readLocal(operation.root)
        if (base === undefined || length === undefined) {
          throw new RangeError('LLVM slice read lost its address or length lane')
        }
        if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
        const index = readScalar(selector.index)
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          index,
          length,
          `slice${checkOrdinal}_in_bounds`,
        )
        yield* locate(selector.provenance.span, yield* Value.instruction(body, inBounds))
        const continueBlock = yield* LlvmBlock.make(body, `slice${checkOrdinal}_ok`)
        yield* FunctionBody.conditionalBranch(body, inBounds, continueBlock, trapBlock)
        yield* LlvmBlock.setInsertionPoint(body, continueBlock)
        const sliceLayout = Layout.entry(program.layout, sourceSemantic)
        if (sliceLayout?.representation._tag !== 'Slice') {
          throw new RangeError('LLVM slice read lost its compiler layout')
        }
        const stride = yield* Constant.integerUnsigned(
          builder,
          usizeType ?? i32,
          BigInt(sliceLayout.representation.stride),
        )
        const elementOffset = yield* FunctionBody.binary(
          body,
          'mul',
          index,
          stride,
          `slice${checkOrdinal}_element_offset`,
        )
        const staticSelectors: Array<Layout.Selector> = []
        for (const candidate of suffixSelectors) {
          if (candidate._tag === 'FieldSelector') {
            staticSelectors.push(candidate.field)
          } else if (candidate._tag === 'ElementSelector' && candidate.index._tag === 'Proven') {
            staticSelectors.push(
              Object.freeze({
                _tag: 'ElementSelector',
                index: candidate.index.value,
              }),
            )
          } else {
            throw new RangeError('LLVM nested runtime slice place is not canonical')
          }
        }
        const selectedValues: Array<Value.Input> = []
        for (const [laneOrdinal, lane] of lanesFor(operation.type).entries()) {
          const staticOffset = Layout.laneOffset(
            program.layout,
            sourceSemantic.element,
            Object.freeze([...staticSelectors, ...lane.path]),
          )
          if (staticOffset === undefined) {
            throw new RangeError(`LLVM slice read lost lane ${laneOrdinal}`)
          }
          const offset =
            staticOffset === 0
              ? elementOffset
              : yield* FunctionBody.binary(
                  body,
                  'add',
                  elementOffset,
                  yield* Constant.integerUnsigned(builder, usizeType ?? i32, BigInt(staticOffset)),
                  `slice${checkOrdinal}_${laneOrdinal}_offset`,
                )
          const address = yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            base,
            offset,
            `slice${checkOrdinal}_${laneOrdinal}_ptr`,
          )
          selectedValues.push(
            yield* FunctionBody.load(
              body,
              laneType(lane),
              address,
              `slice${checkOrdinal}_${laneOrdinal}`,
            ),
          )
        }
        locals.set(operation.destination.ordinal, Object.freeze(selectedValues))
        checkOrdinal += 1
        break
      }
      const sourceLanes = lanesFor(sourceType)
      const sourceValues = readLocal(operation.root)
      const destinationLanes = lanesFor(operation.type)
      const runtimeSelectors = operation.selectors.flatMap((selector, ordinal) =>
        selector._tag === 'ElementSelector' && selector.index._tag === 'Runtime'
          ? [
              Object.freeze({
                local: selector.index.local,
                length: selector.length,
                span: selector.provenance.span,
                ordinal,
              }),
            ]
          : [],
      )
      for (const [runtimeOrdinal, selector] of runtimeSelectors.entries()) {
        if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
        const limit = yield* Constant.integerUnsigned(
          builder,
          usizeType ?? i32,
          BigInt(selector.length),
        )
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          readScalar(selector.local),
          limit,
          `index${checkOrdinal}_${runtimeOrdinal}_in_bounds`,
        )
        const instruction = yield* Value.instruction(body, inBounds)
        yield* locate(selector.span, instruction)
        const continueBlock = yield* LlvmBlock.make(
          body,
          `index${checkOrdinal}_${runtimeOrdinal}_ok`,
        )
        yield* FunctionBody.conditionalBranch(body, inBounds, continueBlock, trapBlock)
        yield* LlvmBlock.setInsertionPoint(body, continueBlock)
      }

      const selectedValues: Array<Value.Input> = []
      for (const [destinationOrdinal, destinationLane] of destinationLanes.entries()) {
        const candidates = sourceLanes.flatMap((sourceLane, sourceOrdinal) => {
          if (sourceLane.path.length !== operation.selectors.length + destinationLane.path.length) {
            return []
          }
          const runtimeElements: Array<{
            readonly local: Mir.LocalId
            readonly element: number
          }> = []
          for (const [selectorOrdinal, selector] of operation.selectors.entries()) {
            const physical = sourceLane.path.at(selectorOrdinal)
            if (physical === undefined) return []
            if (selector._tag === 'FieldSelector') {
              if (
                physical._tag !== 'FieldId' ||
                physical.ordinal !== selector.field.ordinal ||
                physical.struct.sourceId !== selector.field.struct.sourceId ||
                physical.struct.ordinal !== selector.field.struct.ordinal
              ) {
                return []
              }
            } else {
              if (physical._tag !== 'ElementSelector') return []
              if (selector.index._tag === 'Proven' && physical.index !== selector.index.value) {
                return []
              }
              if (selector.index._tag === 'Runtime') {
                runtimeElements.push(
                  Object.freeze({
                    local: selector.index.local,
                    element: physical.index,
                  }),
                )
              }
            }
          }
          const suffix = sourceLane.path.slice(operation.selectors.length)
          const sameSuffix = suffix.every((physical, ordinal) => {
            const expected = destinationLane.path.at(ordinal)
            return expected !== undefined && Layout.selectorEquals(physical, expected)
          })
          const selected = sourceValues.at(sourceOrdinal)
          return sameSuffix && selected !== undefined
            ? [Object.freeze({ value: selected, runtimeElements })]
            : []
        })
        const first = candidates.at(0)
        if (
          first === undefined &&
          operation.selectors.some(
            (selector) => selector._tag === 'ElementSelector' && selector.length === 0,
          )
        ) {
          selectedValues.push(yield* Constant.integerSigned(builder, i32, 0n))
          continue
        }
        if (first === undefined) {
          throw new RangeError(`Backend could not realize place-read lane ${destinationOrdinal}`)
        }
        let selected = first.value
        for (const [candidateOrdinal, candidate] of candidates.slice(1).entries()) {
          let condition: Value.Input | undefined
          for (const [elementOrdinal, element] of candidate.runtimeElements.entries()) {
            const expected = yield* Constant.integerUnsigned(
              builder,
              usizeType ?? i32,
              BigInt(element.element),
            )
            const equal = yield* FunctionBody.integerCompare(
              body,
              'eq',
              readScalar(element.local),
              expected,
              `index${checkOrdinal}_${destinationOrdinal}_${candidateOrdinal}_${elementOrdinal}`,
            )
            condition =
              condition === undefined
                ? equal
                : yield* FunctionBody.binary(
                    body,
                    'and',
                    condition,
                    equal,
                    `index${checkOrdinal}_${destinationOrdinal}_${candidateOrdinal}_${elementOrdinal}_all`,
                  )
          }
          if (condition !== undefined) {
            selected = yield* FunctionBody.select(
              body,
              condition,
              candidate.value,
              selected,
              `index${checkOrdinal}_${destinationOrdinal}_${candidateOrdinal}_value`,
            )
          }
        }
        selectedValues.push(selected)
      }
      checkOrdinal += 1
      locals.set(operation.destination.ordinal, Object.freeze(selectedValues))
      break
    }
    case 'CheckPlace': {
      const rootType = entry.fn.localTypes.at(operation.root.ordinal)
      if (rootType?._tag === 'Slice') {
        const selector = operation.selectors.at(0)
        const length = readLocal(operation.root).at(1)
        if (selector?._tag !== 'SliceElementSelector' || length === undefined) {
          throw new RangeError('LLVM slice write check lost its canonical lanes')
        }
        if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          readScalar(selector.index),
          length,
          `write_slice${checkOrdinal}_in_bounds`,
        )
        yield* locate(selector.provenance.span, yield* Value.instruction(body, inBounds))
        const continueBlock = yield* LlvmBlock.make(body, `write_slice${checkOrdinal}_ok`)
        yield* FunctionBody.conditionalBranch(body, inBounds, continueBlock, trapBlock)
        yield* LlvmBlock.setInsertionPoint(body, continueBlock)
        checkOrdinal += 1
        break
      }
      const runtimeSelectors = operation.selectors.flatMap((selector, ordinal) =>
        selector._tag === 'ElementSelector' && selector.index._tag === 'Runtime'
          ? [
              Object.freeze({
                local: selector.index.local,
                length: selector.length,
                span: selector.provenance.span,
                ordinal,
              }),
            ]
          : [],
      )
      for (const [runtimeOrdinal, selector] of runtimeSelectors.entries()) {
        if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
        const limit = yield* Constant.integerUnsigned(
          builder,
          usizeType ?? i32,
          BigInt(selector.length),
        )
        const inBounds = yield* FunctionBody.integerCompare(
          body,
          'ult',
          readScalar(selector.local),
          limit,
          `write_index${checkOrdinal}_${runtimeOrdinal}_in_bounds`,
        )
        const instruction = yield* Value.instruction(body, inBounds)
        yield* locate(selector.span, instruction)
        const continueBlock = yield* LlvmBlock.make(
          body,
          `write_index${checkOrdinal}_${runtimeOrdinal}_ok`,
        )
        yield* FunctionBody.conditionalBranch(body, inBounds, continueBlock, trapBlock)
        yield* LlvmBlock.setInsertionPoint(body, continueBlock)
      }
      checkOrdinal += 1
      break
    }
    case 'WritePlace': {
      if (operation.rootType._tag === 'Reference') {
        // Writing through the borrow stores each value lane at its target offset.
        const address = readLocal(operation.root).at(0)
        if (address === undefined) throw new RangeError('LLVM reference write lost its address')
        const base = yield* FunctionBody.cast(
          body,
          'inttoptr',
          address,
          pointer,
          `reference_write${operation.source.ordinal}_base`,
        )
        const staticSelectors: Array<Layout.Selector> = []
        for (const candidate of operation.selectors) {
          if (candidate._tag !== 'FieldSelector')
            throw new RangeError('LLVM reference place supports only field selectors')
          staticSelectors.push(candidate.field)
        }
        const target = operation.rootType.type.target
        const values = readLocal(operation.source)
        for (const [ordinal, lane] of lanesFor(operation.type).entries()) {
          const value = values.at(ordinal)
          const offset = Layout.laneOffset(program.layout, target, [
            ...staticSelectors,
            ...lane.path,
          ])
          if (value === undefined || offset === undefined)
            throw new RangeError('LLVM reference write lost a lane offset')
          yield* FunctionBody.store(
            body,
            value,
            yield* constantBytePointer(
              base,
              offset,
              `reference_write${operation.source.ordinal}_${ordinal}_ptr`,
            ),
          )
        }
        break
      }
      if (operation.rootType._tag === 'Slice') {
        const [selector, ...suffixSelectors] = operation.selectors
        const [base] = readLocal(operation.root)
        if (selector?._tag !== 'SliceElementSelector' || base === undefined) {
          throw new RangeError('LLVM slice write lost its canonical address lane')
        }
        const sliceLayout = Layout.entry(program.layout, operation.rootType.type)
        if (sliceLayout?.representation._tag !== 'Slice') {
          throw new RangeError('LLVM slice write lost its compiler layout')
        }
        const stride = yield* Constant.integerUnsigned(
          builder,
          usizeType ?? i32,
          BigInt(sliceLayout.representation.stride),
        )
        const elementOffset = yield* FunctionBody.binary(
          body,
          'mul',
          readScalar(selector.index),
          stride,
          `write_slice${checkOrdinal}_element_offset`,
        )
        const staticSelectors: Array<Layout.Selector> = []
        for (const candidate of suffixSelectors) {
          if (candidate._tag === 'FieldSelector') {
            staticSelectors.push(candidate.field)
          } else if (candidate._tag === 'ElementSelector' && candidate.index._tag === 'Proven') {
            staticSelectors.push(
              Object.freeze({
                _tag: 'ElementSelector',
                index: candidate.index.value,
              }),
            )
          } else {
            throw new RangeError('LLVM nested runtime slice write is not canonical')
          }
        }
        const sourceValues = readLocal(operation.source)
        for (const [laneOrdinal, lane] of lanesFor(operation.type).entries()) {
          const staticOffset = Layout.laneOffset(
            program.layout,
            operation.rootType.type.element,
            Object.freeze([...staticSelectors, ...lane.path]),
          )
          const stored = sourceValues.at(laneOrdinal)
          if (staticOffset === undefined || stored === undefined) {
            throw new RangeError(`LLVM slice write lost lane ${laneOrdinal}`)
          }
          const offset =
            staticOffset === 0
              ? elementOffset
              : yield* FunctionBody.binary(
                  body,
                  'add',
                  elementOffset,
                  yield* Constant.integerUnsigned(builder, usizeType ?? i32, BigInt(staticOffset)),
                  `write_slice${checkOrdinal}_${laneOrdinal}_offset`,
                )
          yield* FunctionBody.store(
            body,
            stored,
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              base,
              offset,
              `write_slice${checkOrdinal}_${laneOrdinal}_ptr`,
            ),
          )
        }
        checkOrdinal += 1
        break
      }
      const rootLanes = lanesFor(operation.rootType)
      const rootValues = readLocal(operation.root)
      const sourceLanes = lanesFor(operation.type)
      const sourceValues = readLocal(operation.source)
      if (operation.selectors.length === 0) {
        locals.set(operation.root.ordinal, sourceValues)
        yield* storeMutable(operation.root, sourceValues)
        break
      }
      const updated: Array<Value.Input> = []
      for (const [rootOrdinal, rootLane] of rootLanes.entries()) {
        const previous = rootValues.at(rootOrdinal)
        if (previous === undefined) throw new RangeError('Mutable root lost a lane')
        const runtimeElements: Array<{
          readonly local: Mir.LocalId
          readonly element: number
        }> = []
        let matches = true
        for (const [selectorOrdinal, selector] of operation.selectors.entries()) {
          const physical = rootLane.path.at(selectorOrdinal)
          if (physical === undefined) {
            matches = false
            break
          }
          if (selector._tag === 'FieldSelector') {
            if (
              physical._tag !== 'FieldId' ||
              physical.ordinal !== selector.field.ordinal ||
              physical.struct.sourceId !== selector.field.struct.sourceId ||
              physical.struct.ordinal !== selector.field.struct.ordinal
            ) {
              matches = false
              break
            }
          } else if (selector._tag === 'SliceElementSelector') {
            matches = false
            break
          } else if (physical._tag !== 'ElementSelector') {
            matches = false
            break
          } else if (selector.index._tag === 'Proven') {
            if (physical.index !== selector.index.value) {
              matches = false
              break
            }
          } else {
            runtimeElements.push(
              Object.freeze({
                local: selector.index.local,
                element: physical.index,
              }),
            )
          }
        }
        if (!matches) {
          updated.push(previous)
          continue
        }
        const suffix = rootLane.path.slice(operation.selectors.length)
        const sourceOrdinal = sourceLanes.findIndex(
          (lane) =>
            lane.path.length === suffix.length &&
            lane.path.every((physical, ordinal) => {
              const expected = suffix.at(ordinal)
              return expected !== undefined && Layout.selectorEquals(physical, expected)
            }),
        )
        const replacement = sourceValues.at(sourceOrdinal)
        if (replacement === undefined) {
          throw new RangeError(`Backend could not realize place-write lane ${rootOrdinal}`)
        }
        let condition: Value.Input | undefined
        for (const [elementOrdinal, element] of runtimeElements.entries()) {
          const expected = yield* Constant.integerUnsigned(
            builder,
            usizeType ?? i32,
            BigInt(element.element),
          )
          const equal = yield* FunctionBody.integerCompare(
            body,
            'eq',
            readScalar(element.local),
            expected,
            `write_index${checkOrdinal}_${rootOrdinal}_${elementOrdinal}`,
          )
          condition =
            condition === undefined
              ? equal
              : yield* FunctionBody.binary(
                  body,
                  'and',
                  condition,
                  equal,
                  `write_index${checkOrdinal}_${rootOrdinal}_${elementOrdinal}_all`,
                )
        }
        updated.push(
          condition === undefined
            ? replacement
            : yield* FunctionBody.select(
                body,
                condition,
                replacement,
                previous,
                `write_index${checkOrdinal}_${rootOrdinal}_value`,
              ),
        )
      }
      checkOrdinal += 1
      const frozen = Object.freeze(updated)
      locals.set(operation.root.ordinal, frozen)
      yield* storeMutable(operation.root, frozen)
      break
    }
    case 'ConvertInteger': {
      const result = yield* emitIntegerConversion(
        readScalar(operation.source),
        operation.sourceType,
        operation.type,
        `convert${operation.destination.ordinal}`,
      )
      locals.set(operation.destination.ordinal, Object.freeze([result]))
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
      const sourceValue = readScalar(operation.source)
      if (source.category === 'Character' && target.spelling === 'u32') {
        locals.set(operation.destination.ordinal, Object.freeze([sourceValue]))
        break
      }
      const destinationType =
        target.category === 'Floating'
          ? target.spelling === 'f32'
            ? f32
            : f64
          : (integerTypes.get(
              Scalar.bits(target, program.layout.target.pointerSize === 4 ? 32 : 64),
            ) ?? i32)
      const kind: FunctionBody.CastKind =
        source.category === 'Floating' && target.category === 'Floating'
          ? source.spelling === 'f64'
            ? 'fptrunc'
            : 'fpext'
          : source.category === 'Floating' && target.category === 'Integer'
            ? target.signedness === 'Signed'
              ? 'fptosi'
              : 'fptoui'
            : source.category === 'Integer' && target.category === 'Floating'
              ? source.signedness === 'Signed'
                ? 'sitofp'
                : 'uitofp'
              : (() => {
                  throw new RangeError('LLVM scalar conversion was not numeric')
                })()
      const result = yield* FunctionBody.cast(
        body,
        kind,
        sourceValue,
        destinationType,
        `convert${operation.destination.ordinal}`,
      )
      locals.set(operation.destination.ordinal, Object.freeze([result]))
      break
    }
    case 'ReinterpretScalar': {
      const targetLane = lanesFor(operation.type).at(0)
      if (targetLane === undefined) throw new RangeError('LLVM reinterpretation lost its lane')
      const result = yield* FunctionBody.cast(
        body,
        'bitcast',
        readScalar(operation.source),
        laneType(targetLane),
        `reinterpret${operation.destination.ordinal}`,
      )
      locals.set(operation.destination.ordinal, Object.freeze([result]))
      break
    }
    case 'FloatUnary': {
      const source = Scalar.find(operation.sourceType._tag)
      if (source?.category !== 'Floating')
        throw new RangeError('LLVM float unary lost its source type')
      const subject = readScalar(operation.source)
      if (operation.operation === 'Negate') {
        const result = yield* FunctionBody.unary(
          body,
          'fneg',
          subject,
          `fneg${operation.destination.ordinal}`,
        )
        locals.set(operation.destination.ordinal, Object.freeze([result]))
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
        yield* locate(operation.provenance.span, yield* Value.instruction(body, result))
        locals.set(operation.destination.ordinal, Object.freeze([result]))
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
      locals.set(operation.destination.ordinal, Object.freeze([result]))
      break
    }
    case 'FloatTranscendental': {
      const i64Type = integerTypes.get(64)
      const result = yield* NativeTranscendental.emit(
        { builder, i32, ...(i64Type === undefined ? {} : { i64: i64Type }), f32, f64 },
        body,
        operation,
        readScalar(operation.source),
      )
      yield* locate(operation.provenance.span, yield* Value.instruction(body, result))
      locals.set(operation.destination.ordinal, Object.freeze([result]))
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
      const left = readScalar(leftLocal)
      const right = rightLocal === undefined ? undefined : readScalar(rightLocal)
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
        result =
          sourceBits === targetBits
            ? left
            : yield* FunctionBody.cast(
                body,
                sourceBits < targetBits
                  ? source.signedness === 'Signed'
                    ? 'sext'
                    : 'zext'
                  : 'trunc',
                left,
                targetPhysical,
                `${name}_value`,
              )
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
        const stem =
          operation.operation === 'CheckedAdd'
            ? 'add'
            : operation.operation === 'CheckedSubtract'
              ? 'sub'
              : 'mul'
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
      } else {
        if (target.category !== 'Integer')
          throw new RangeError('LLVM checked division lost its integer target')
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
        result = yield* FunctionBody.binary(
          body,
          operation.operation === 'CheckedDivide'
            ? target.signedness === 'Unsigned'
              ? 'udiv'
              : 'sdiv'
            : target.signedness === 'Unsigned'
              ? 'urem'
              : 'srem',
          left,
          safeRight,
          `${name}_value`,
        )
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
      const valueLane = lanesFor(operation.valueType).at(0)
      const payloadLane = lanesFor(operation.type).at(1)
      if (valueLane === undefined || payloadLane === undefined)
        throw new RangeError('LLVM checked scalar operation lost its payload lane')
      const payload = yield* coerceLane(result, valueLane, payloadLane, `${name}_payload`)
      locals.set(operation.destination.ordinal, Object.freeze([tag, payload]))
      break
    }
    case 'Binary': {
      const left = readScalar(operation.left)
      const right = readScalar(operation.right)
      const leftType = entry.fn.localTypes.at(operation.left.ordinal)
      const leftLane = leftType === undefined ? undefined : valueLanesFor(leftType).at(0)
      if (leftType === undefined || leftLane === undefined) {
        throw new RangeError('LLVM binary operation lost its operand type')
      }
      const semanticOperand = Mir.semanticType(leftType)
      const scalar = typeof semanticOperand === 'string' ? Scalar.find(semanticOperand) : undefined
      const unsigned = scalar?.signedness === 'Unsigned'
      const operandType = laneType(leftLane)
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
          locals.set(operation.destination.ordinal, Object.freeze([result]))
          break
        }
        const predicate: FunctionBody.FloatingPredicate | undefined =
          operation.operator === 'Equals'
            ? 'oeq'
            : operation.operator === 'NotEquals'
              ? 'une'
              : operation.operator === 'LessThan'
                ? 'olt'
                : operation.operator === 'LessOrEqual'
                  ? 'ole'
                  : operation.operator === 'GreaterThan'
                    ? 'ogt'
                    : operation.operator === 'GreaterOrEqual'
                      ? 'oge'
                      : undefined
        if (predicate !== undefined) {
          const flag = yield* FunctionBody.floatingCompare(
            body,
            predicate,
            left,
            right,
            `fcmp${ordinal}_flag`,
          )
          const result = yield* FunctionBody.cast(body, 'zext', flag, i32, `fcmp${ordinal}`)
          locals.set(operation.destination.ordinal, Object.freeze([result]))
          break
        }
        const mnemonic: FunctionBody.FloatingBinaryKind | undefined =
          operation.operator === 'Add'
            ? 'fadd'
            : operation.operator === 'Subtract'
              ? 'fsub'
              : operation.operator === 'Multiply'
                ? 'fmul'
                : operation.operator === 'Divide'
                  ? 'fdiv'
                  : operation.operator === 'Remainder'
                    ? 'frem'
                    : undefined
        if (mnemonic === undefined)
          throw new RangeError(`LLVM float operation ${operation.operator} is unavailable`)
        const result = yield* FunctionBody.binary(body, mnemonic, left, right, `float${ordinal}`)
        yield* locate(operation.provenance.span, yield* Value.instruction(body, result))
        locals.set(operation.destination.ordinal, Object.freeze([result]))
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
        yield* locate(operation.provenance.span, instruction)
        locals.set(operation.destination.ordinal, Object.freeze([widened]))
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
        const mnemonic =
          operation.operator === 'BitAnd'
            ? 'and'
            : operation.operator === 'BitOr'
              ? 'or'
              : operation.operator === 'BitXor'
                ? 'xor'
                : operation.operator === 'WrappingAdd'
                  ? 'add'
                  : operation.operator === 'WrappingSubtract'
                    ? 'sub'
                    : 'mul'
        const result = yield* FunctionBody.binary(body, mnemonic, left, right, `integer${ordinal}`)
        yield* locate(operation.provenance.span, yield* Value.instruction(body, result))
        locals.set(operation.destination.ordinal, Object.freeze([result]))
        break
      }
      if (operation.operator === 'ShiftLeft' || operation.operator === 'ShiftRight') {
        if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'arith_trap')
        const width =
          scalar === undefined
            ? 32
            : Scalar.bits(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
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
        const result = yield* FunctionBody.binary(
          body,
          operation.operator === 'ShiftLeft' ? 'shl' : unsigned ? 'lshr' : 'ashr',
          left,
          right,
          `shift${ordinal}`,
        )
        yield* locate(operation.provenance.span, yield* Value.instruction(body, result))
        locals.set(operation.destination.ordinal, Object.freeze([result]))
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
        yield* locate(operation.provenance.span, yield* Value.instruction(body, result))
        locals.set(operation.destination.ordinal, Object.freeze([result]))
        break
      }
      if (operation.operator === 'SaturatingAdd' || operation.operator === 'SaturatingSubtract') {
        const signature = Object.freeze({
          returnType: operandType,
          parameters: Object.freeze([operandType, operandType]),
        })
        const intrinsic =
          operation.operator === 'SaturatingAdd'
            ? unsigned
              ? 'uadd.sat'
              : 'sadd.sat'
            : unsigned
              ? 'usub.sat'
              : 'ssub.sat'
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
        yield* locate(operation.provenance.span, yield* Value.instruction(body, result))
        locals.set(operation.destination.ordinal, Object.freeze([result]))
        break
      }
      if (operation.operator === 'SaturatingMultiply') {
        const bits =
          scalar === undefined
            ? 32
            : Scalar.bits(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
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
        const range =
          scalar?.category === 'Integer'
            ? Scalar.range(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
            : { minimum: -2147483648n, maximum: 2147483647n }
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
        yield* locate(operation.provenance.span, yield* Value.instruction(body, result))
        locals.set(operation.destination.ordinal, Object.freeze([result]))
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
        const intrinsicId =
          operation.operator === 'Add'
            ? unsigned
              ? ('uadd.with.overflow' as const)
              : ('sadd.with.overflow' as const)
            : operation.operator === 'Subtract'
              ? unsigned
                ? ('usub.with.overflow' as const)
                : ('ssub.with.overflow' as const)
              : unsigned
                ? ('umul.with.overflow' as const)
                : ('smul.with.overflow' as const)
        const bits =
          scalar === undefined
            ? 32
            : Scalar.bits(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
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
        result = yield* FunctionBody.binary(
          body,
          operation.operator === 'Divide'
            ? unsigned
              ? 'udiv'
              : 'sdiv'
            : unsigned
              ? 'urem'
              : 'srem',
          left,
          right,
          `arith${ordinal}`,
        )
      }
      const instruction = yield* Value.instruction(body, result)
      yield* locate(operation.provenance.span, instruction)
      locals.set(operation.destination.ordinal, Object.freeze([result]))
      break
    }
    case 'Drop': {
      if (CleanupPlan.hasEffect(operation.cleanup)) {
        yield* dropThroughPlan(
          operation.cleanup,
          readLocal(operation.local),
          `drop${operation.local.ordinal}`,
        )
      }
      break
    }
    case 'MakeEffect':
    case 'MakeCallable': {
      const captured: Array<Value.Input> = []
      const fields =
        operation._tag === 'MakeEffect'
          ? operation.type.environment.fields
          : (operation.type.environment?.fields ?? Object.freeze([]))
      for (const [ordinal, capture] of operation.captures.entries()) {
        const field = fields.at(ordinal)
        if (field === undefined) throw new RangeError('Effect capture lost its environment field')
        if (field.representation !== 'Borrow') {
          captured.push(...readLocal(capture.source))
          continue
        }
        yield* ensureAddressRoot(capture.source)
        const base = addressStorage.get(capture.source.ordinal)
        if (base === undefined) throw new RangeError('Effect borrowed capture lost its storage')
        captured.push(base)
      }
      if (captured.length !== lanesFor(operation.type).length)
        throw new RangeError('Effect environment capture lanes do not match its plan')
      locals.set(operation.destination.ordinal, Object.freeze(captured))
      break
    }
    case 'PackEffectComposite': {
      const source = [...readLocal(operation.source)]
      const sourceType = entry.fn.localTypes.at(operation.source.ordinal)
      if (sourceType?._tag !== 'EffectValue')
        throw new RangeError('LLVM Effect composite lost its selected alternative')
      const sourceLanes = lanesFor(sourceType)
      const targetLanes = lanesFor(operation.type)
      const values: Array<Value.Input> = [
        yield* Constant.integerSigned(builder, i32, BigInt(operation.alternative)),
      ]
      for (const [ordinal, targetLane] of targetLanes.slice(1).entries()) {
        const input = source.at(ordinal)
        const sourceLane = sourceLanes.at(ordinal)
        values.push(
          input === undefined || sourceLane === undefined
            ? yield* Constant.nullValue(builder, laneType(targetLane))
            : yield* coerceLane(
                input,
                sourceLane,
                targetLane,
                `effect_composite${operation.destination.ordinal}_${ordinal}`,
              ),
        )
      }
      locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'PackEffectOutcome': {
      const source = [...readLocal(operation.source)]
      const sourceType = entry.fn.localTypes.at(operation.source.ordinal)
      if (sourceType === undefined) throw new RangeError('LLVM effect outcome lost its source type')
      const sourceLanes = valueLanesFor(sourceType)
      const targetLanes = lanesFor(operation.type)
      const values: Array<Value.Input> = [
        yield* Constant.integerSigned(builder, i32, BigInt(operation.tag)),
      ]
      for (const [ordinal, targetLane] of targetLanes.slice(1).entries()) {
        const input = source.at(ordinal)
        const sourceLane = sourceLanes.at(ordinal)
        values.push(
          input === undefined || sourceLane === undefined
            ? yield* Constant.nullValue(builder, laneType(targetLane))
            : yield* coerceLane(
                input,
                sourceLane,
                targetLane,
                `effect_outcome${operation.destination.ordinal}_${ordinal}_payload`,
              ),
        )
      }
      locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'PackEffectFailureUnion': {
      const source = readLocal(operation.source)
      const sourceTag = source.at(0)
      if (sourceTag === undefined) throw new RangeError('Effect failure union lost its tag lane')
      let mappedTag: Value.Input = yield* Constant.integerSigned(builder, i32, -1n)
      for (const [ordinal, mapping] of operation.mappings.entries()) {
        const matches = yield* FunctionBody.integerCompare(
          body,
          'eq',
          sourceTag,
          yield* Constant.integerSigned(builder, i32, BigInt(mapping.source)),
          `effect_failure_union${operation.destination.ordinal}_${ordinal}`,
        )
        mappedTag = yield* FunctionBody.select(
          body,
          matches,
          yield* Constant.integerSigned(builder, i32, BigInt(mapping.target)),
          mappedTag,
          `effect_failure_union${operation.destination.ordinal}_${ordinal}_tag`,
        )
      }
      const values: Array<Value.Input> = [
        mappedTag,
        ...(yield* failurePayload(
          source,
          operation.sourceType.type,
          sourceTag,
          operation.type.type,
          operation.mappings,
          `effect_failure_union${operation.destination.ordinal}_payload`,
        )),
      ]
      locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'UnpackEffectSuccess': {
      const count = lanesFor(operation.type).length
      locals.set(
        operation.destination.ordinal,
        Object.freeze(readLocal(operation.source).slice(1, 1 + count)),
      )
      break
    }
    case 'RunEffect': {
      const target = declared.find((candidate) =>
        Mir.matchesInstance(candidate.fn, operation.target, operation.typeArguments),
      )
      if (target === undefined)
        throw new RangeError('Backend cannot resolve propagated effect target')
      const runArguments = operation.arguments.flatMap((argument) => [...readLocal(argument)])
      if (yield* emitOrigin(operation, runArguments, `effect_run${operation.destination.ordinal}`))
        break
      const suspension = suspensionRegions.get(operation)
      const outcomeValues = yield* joinSuspensionOutcome(
        operation,
        yield* callValues(
          target,
          runArguments,
          `effect_run${operation.destination.ordinal}`,
          suspension?._tag === 'RunSuspendableEffectRegion' ? suspension : undefined,
        ),
        `effect_run${operation.destination.ordinal}`,
      )
      locals.set(operation.outcome.ordinal, outcomeValues)
      const tag = outcomeValues.at(0)
      if (tag === undefined) throw new RangeError('Effect outcome lost its tag')
      const zero = yield* Constant.integerSigned(builder, i32, 0n)
      const succeeded = yield* FunctionBody.integerCompare(
        body,
        'eq',
        tag,
        zero,
        `effect_run_success${operation.destination.ordinal}`,
      )
      const successBlock = yield* LlvmBlock.make(
        body,
        `effect_run${operation.destination.ordinal}_success`,
      )
      const failureBlock = yield* LlvmBlock.make(
        body,
        `effect_run${operation.destination.ordinal}_failure`,
      )
      const followingBlock = yield* LlvmBlock.make(
        body,
        `effect_run${operation.destination.ordinal}_following`,
      )
      yield* FunctionBody.conditionalBranch(body, succeeded, successBlock, failureBlock)
      const resultLaneCount = lanesFor(operation.type).length
      yield* LlvmBlock.setInsertionPoint(body, successBlock)
      yield* storeMutable(
        operation.destination,
        Object.freeze(outcomeValues.slice(1, 1 + resultLaneCount)),
      )
      yield* FunctionBody.branch(body, followingBlock)
      yield* LlvmBlock.setInsertionPoint(body, failureBlock)
      let mappedTag: Value.Input = yield* Constant.integerSigned(builder, i32, -1n)
      for (const [ordinal, mapping] of operation.tagMappings.entries()) {
        const source = yield* Constant.integerSigned(builder, i32, BigInt(mapping.source))
        const matches = yield* FunctionBody.integerCompare(
          body,
          'eq',
          tag,
          source,
          `effect_tag${operation.destination.ordinal}_${ordinal}`,
        )
        mappedTag = yield* FunctionBody.select(
          body,
          matches,
          yield* Constant.integerSigned(builder, i32, BigInt(mapping.target)),
          mappedTag,
          `effect_mapped_tag${operation.destination.ordinal}_${ordinal}`,
        )
      }
      // Owners still live at this site release before the failure leaves the function
      // through their complete cleanup plans, matching the Drop lowering.
      for (const release of operation.releases ?? []) {
        if (!CleanupPlan.hasEffect(release.cleanup)) continue
        yield* dropThroughPlan(
          release.cleanup,
          readLocal(release.local),
          `propagation_release${release.local.ordinal}`,
        )
      }
      const returned: Array<Value.Input> = [
        mappedTag,
        ...(yield* failurePayload(
          outcomeValues,
          operation.outcomeType.type,
          tag,
          operation.propagationType.type,
          operation.tagMappings,
          `effect_run${operation.destination.ordinal}_payload`,
        )),
      ]
      if (entry.suspendable) {
        yield* returnStep(0n, Object.freeze(returned), 'propagated_effect_step')
      } else if (returned.length === 1) {
        const single = returned.at(0)
        if (single === undefined) throw new RangeError('Effect propagation lost its tag')
        yield* FunctionBody.returnValue(body, single)
      } else {
        yield* FunctionBody.returnValue(
          body,
          yield* FunctionBody.buildAggregate(
            body,
            entry.resultType,
            Object.freeze(returned.slice(0, operation.propagationLaneCount)),
            'propagated_effect',
          ),
        )
      }
      yield* LlvmBlock.setInsertionPoint(body, followingBlock)
      // Both arms of this outcome dispatch reach here, so neither arm's cached
      // values are readable in the join. Reloading re-roots them at this block.
      yield* reloadMutableRoots(`effect_run${operation.destination.ordinal}_following`)
      const storage = mutableStorage.get(operation.destination.ordinal)
      if (storage === undefined) throw new RangeError('Effect run destination is not materialized')
      const loaded: Array<Value.Input> = []
      for (const [lane, pointer] of storage.entries()) {
        const callingLane = lanesFor(operation.type).at(lane)
        if (callingLane === undefined) throw new RangeError('Effect run destination lost a lane')
        loaded.push(
          yield* FunctionBody.load(
            body,
            laneType(callingLane),
            pointer,
            `effect_run${operation.destination.ordinal}_${lane}`,
          ),
        )
      }
      locals.set(operation.destination.ordinal, Object.freeze(loaded))
      break
    }
    case 'RunEffectComposite': {
      const compositeValues = readLocal(operation.effect)
      const choice = compositeValues.at(0)
      const compositeType = entry.fn.localTypes.at(operation.effect.ordinal)
      if (choice === undefined || compositeType?._tag !== 'EffectComposite')
        throw new RangeError('LLVM Effect composite lost its tag or representation')
      const compositeLanes = lanesFor(compositeType)
      const joinedOutcomeLanes = lanesFor(operation.outcomeType)
      const following = yield* LlvmBlock.make(
        body,
        `effect_composite${operation.destination.ordinal}_following`,
      )
      for (const [alternativeOrdinal, alternative] of operation.alternatives.entries()) {
        const selected = yield* LlvmBlock.make(
          body,
          `effect_composite${operation.destination.ordinal}_alternative${alternativeOrdinal}`,
        )
        const otherwise = yield* LlvmBlock.make(
          body,
          `effect_composite${operation.destination.ordinal}_otherwise${alternativeOrdinal}`,
        )
        const selectedTag = yield* Constant.integerSigned(builder, i32, BigInt(alternativeOrdinal))
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            choice,
            selectedTag,
            `effect_composite${operation.destination.ordinal}_is${alternativeOrdinal}`,
          ),
          selected,
          otherwise,
        )
        yield* LlvmBlock.setInsertionPoint(body, selected)
        const target = declared.find((candidate) =>
          Mir.matchesInstance(candidate.fn, alternative.runner, alternative.runnerTypeArguments),
        )
        if (target === undefined)
          throw new RangeError(
            `Backend cannot resolve Effect composite runner ${alternative.runner.module}.${alternative.runner.name}`,
          )
        const captureLanes = lanesFor(alternative.type)
        const effectArguments: Array<Value.Input> = []
        for (const [ordinal, targetLane] of captureLanes.entries()) {
          const input = compositeValues.at(ordinal + 1)
          const sourceLane = compositeLanes.at(ordinal + 1)
          if (input === undefined || sourceLane === undefined)
            throw new RangeError('LLVM Effect composite lost a capture lane')
          effectArguments.push(
            yield* coerceLane(
              input,
              sourceLane,
              targetLane,
              `effect_composite${operation.destination.ordinal}_${alternativeOrdinal}_capture${ordinal}`,
            ),
          )
        }
        effectArguments.push(
          ...alternative.arguments.flatMap((argument) => [...readLocal(argument)]),
        )
        const called = yield* callValues(
          target,
          effectArguments,
          `effect_composite${operation.destination.ordinal}_${alternativeOrdinal}`,
        )
        const sourceOutcomeType: Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> =
          Object.freeze({ _tag: 'EffectOutcome', type: alternative.type.type })
        const sourceOutcomeLanes = lanesFor(sourceOutcomeType)
        const sourceTag = called.at(0)
        if (sourceTag === undefined)
          throw new RangeError('LLVM Effect composite runner lost its outcome tag')
        let mappedTag: Value.Input = sourceTag
        for (const [mappingOrdinal, mapping] of alternative.tagMappings.entries()) {
          const matches = yield* FunctionBody.integerCompare(
            body,
            'eq',
            sourceTag,
            yield* Constant.integerSigned(builder, i32, BigInt(mapping.source)),
            `effect_composite${operation.destination.ordinal}_${alternativeOrdinal}_tag${mappingOrdinal}`,
          )
          mappedTag = yield* FunctionBody.select(
            body,
            matches,
            yield* Constant.integerSigned(builder, i32, BigInt(mapping.target)),
            mappedTag,
            `effect_composite${operation.destination.ordinal}_${alternativeOrdinal}_mapped${mappingOrdinal}`,
          )
        }
        const joined: Array<Value.Input> = [mappedTag]
        for (const [ordinal, targetLane] of joinedOutcomeLanes.slice(1).entries()) {
          const input = called.at(ordinal + 1)
          const sourceLane = sourceOutcomeLanes.at(ordinal + 1)
          joined.push(
            input === undefined || sourceLane === undefined
              ? yield* Constant.nullValue(builder, laneType(targetLane))
              : yield* coerceLane(
                  input,
                  sourceLane,
                  targetLane,
                  `effect_composite${operation.destination.ordinal}_${alternativeOrdinal}_outcome${ordinal}`,
                ),
          )
        }
        yield* storeMutable(operation.outcome, Object.freeze(joined))
        yield* FunctionBody.branch(body, following)
        yield* LlvmBlock.setInsertionPoint(body, otherwise)
      }
      if (trapBlock === undefined)
        trapBlock = yield* LlvmBlock.make(body, 'effect_composite_invalid_tag')
      yield* FunctionBody.branch(body, trapBlock)
      yield* LlvmBlock.setInsertionPoint(body, following)
      yield* reloadMutableRoots(`effect_composite${operation.destination.ordinal}_following`)
      const outcomeStorage = mutableStorage.get(operation.outcome.ordinal)
      if (outcomeStorage === undefined)
        throw new RangeError('Effect composite outcome is not materialized')
      const outcomeValues: Array<Value.Input> = []
      for (const [ordinal, pointer] of outcomeStorage.entries()) {
        const lane = joinedOutcomeLanes.at(ordinal)
        if (lane === undefined) throw new RangeError('Effect composite outcome lost a lane')
        outcomeValues.push(
          yield* FunctionBody.load(
            body,
            laneType(lane),
            pointer,
            `effect_composite${operation.destination.ordinal}_outcome${ordinal}`,
          ),
        )
      }
      locals.set(operation.outcome.ordinal, Object.freeze(outcomeValues))
      const resultLaneCount = lanesFor(operation.type).length
      if (operation.propagationType === undefined) {
        locals.set(
          operation.destination.ordinal,
          Object.freeze(outcomeValues.slice(1, 1 + resultLaneCount)),
        )
        break
      }
      const tag = outcomeValues.at(0)
      if (tag === undefined) throw new RangeError('Effect composite outcome lost its tag')
      const succeeded = yield* FunctionBody.integerCompare(
        body,
        'eq',
        tag,
        yield* Constant.integerSigned(builder, i32, 0n),
        `effect_composite_success${operation.destination.ordinal}`,
      )
      const successBlock = yield* LlvmBlock.make(
        body,
        `effect_composite${operation.destination.ordinal}_success`,
      )
      const failureBlock = yield* LlvmBlock.make(
        body,
        `effect_composite${operation.destination.ordinal}_failure`,
      )
      const completed = yield* LlvmBlock.make(
        body,
        `effect_composite${operation.destination.ordinal}_completed`,
      )
      yield* FunctionBody.conditionalBranch(body, succeeded, successBlock, failureBlock)
      yield* LlvmBlock.setInsertionPoint(body, successBlock)
      yield* storeMutable(
        operation.destination,
        Object.freeze(outcomeValues.slice(1, 1 + resultLaneCount)),
      )
      yield* FunctionBody.branch(body, completed)
      yield* LlvmBlock.setInsertionPoint(body, failureBlock)
      let propagatedTag: Value.Input = yield* Constant.integerSigned(builder, i32, -1n)
      for (const [ordinal, mapping] of operation.tagMappings.entries()) {
        const matches = yield* FunctionBody.integerCompare(
          body,
          'eq',
          tag,
          yield* Constant.integerSigned(builder, i32, BigInt(mapping.source)),
          `effect_composite_propagation_tag${operation.destination.ordinal}_${ordinal}`,
        )
        propagatedTag = yield* FunctionBody.select(
          body,
          matches,
          yield* Constant.integerSigned(builder, i32, BigInt(mapping.target)),
          propagatedTag,
          `effect_composite_propagated_tag${operation.destination.ordinal}_${ordinal}`,
        )
      }
      for (const release of operation.releases ?? []) {
        if (!CleanupPlan.hasEffect(release.cleanup)) continue
        yield* dropThroughPlan(
          release.cleanup,
          readLocal(release.local),
          `effect_composite_release${release.local.ordinal}`,
        )
      }
      const returned: Array<Value.Input> = [
        propagatedTag,
        ...(yield* failurePayload(
          outcomeValues,
          operation.outcomeType.type,
          tag,
          operation.propagationType.type,
          operation.tagMappings,
          `effect_composite${operation.destination.ordinal}_payload`,
        )),
      ]
      if (entry.suspendable) {
        yield* returnStep(0n, Object.freeze(returned), 'propagated_effect_composite_step')
      } else {
        yield* FunctionBody.returnValue(
          body,
          returned.length === 1
            ? (returned.at(0) ?? propagatedTag)
            : yield* FunctionBody.buildAggregate(
                body,
                entry.resultType,
                Object.freeze(returned.slice(0, operation.propagationLaneCount)),
                'propagated_effect_composite',
              ),
        )
      }
      yield* LlvmBlock.setInsertionPoint(body, completed)
      yield* reloadMutableRoots(`effect_composite${operation.destination.ordinal}_completed`)
      const destinationStorage = mutableStorage.get(operation.destination.ordinal)
      if (destinationStorage === undefined)
        throw new RangeError('Effect composite destination is not materialized')
      const loaded: Array<Value.Input> = []
      for (const [ordinal, pointer] of destinationStorage.entries()) {
        const lane = lanesFor(operation.type).at(ordinal)
        if (lane === undefined) throw new RangeError('Effect composite destination lost a lane')
        loaded.push(
          yield* FunctionBody.load(
            body,
            laneType(lane),
            pointer,
            `effect_composite${operation.destination.ordinal}_${ordinal}`,
          ),
        )
      }
      locals.set(operation.destination.ordinal, Object.freeze(loaded))
      break
    }
    case 'RunEffectValue':
    case 'RunStaticEffect': {
      const logicalInputs =
        operation._tag === 'RunStaticEffect'
          ? [...operation.captures.map((capture) => capture.source), ...operation.arguments]
          : undefined
      const target = declared.find(
        (candidate) =>
          Mir.matchesInstance(candidate.fn, operation.runner, operation.runnerTypeArguments) &&
          (operation._tag !== 'RunStaticEffect' ||
            (logicalInputs !== undefined &&
              candidate.fn.result._tag === 'EffectOutcome' &&
              SilkType.equals(candidate.fn.result.type, operation.outcomeType.type) &&
              candidate.fn.parameterCount === logicalInputs.length &&
              logicalInputs.every((input, ordinal) => {
                const actual = entry.fn.localTypes.at(input.ordinal)
                const expected = candidate.fn.localTypes.at(ordinal)
                return (
                  actual !== undefined &&
                  expected !== undefined &&
                  SilkType.equals(Mir.semanticType(actual), Mir.semanticType(expected))
                )
              }))),
      )
      if (target === undefined)
        throw new RangeError(
          `Backend cannot resolve Effect value runner ${operation.runner.module}.${operation.runner.name}<${operation.runnerTypeArguments.map(SilkType.encodeGenericArgument).join(', ')}>`,
        )
      const effectArguments = [
        ...(operation._tag === 'RunEffectValue'
          ? readLocal(operation.effect)
          : operation.captures.flatMap((capture) => [...readLocal(capture.source)])),
        ...operation.arguments.flatMap((argument) => [...readLocal(argument)]),
      ]
      if (operation._tag !== 'RunStaticEffect') {
        if (
          yield* emitOrigin(
            operation,
            effectArguments,
            `effect_value_run${operation.destination.ordinal}`,
          )
        )
          break
      }
      const suspension =
        operation._tag === 'RunStaticEffect' ? undefined : suspensionRegions.get(operation)
      const called = yield* callValues(
        target,
        effectArguments,
        `effect_value_run${operation.destination.ordinal}`,
        suspension?._tag === 'RunSuspendableEffectRegion' ? suspension : undefined,
      )
      const outcomeValues =
        operation._tag === 'RunStaticEffect'
          ? called
          : yield* joinSuspensionOutcome(
              operation,
              called,
              `effect_value_run${operation.destination.ordinal}`,
            )
      locals.set(operation.outcome.ordinal, outcomeValues)
      const resultLaneCount = lanesFor(operation.type).length
      if (operation.propagationType === undefined) {
        locals.set(
          operation.destination.ordinal,
          Object.freeze(outcomeValues.slice(1, 1 + resultLaneCount)),
        )
        break
      }
      const tag = outcomeValues.at(0)
      if (tag === undefined) throw new RangeError('Effect outcome lost its tag')
      const zero = yield* Constant.integerSigned(builder, i32, 0n)
      const succeeded = yield* FunctionBody.integerCompare(
        body,
        'eq',
        tag,
        zero,
        `effect_value_success${operation.destination.ordinal}`,
      )
      const successBlock = yield* LlvmBlock.make(
        body,
        `effect_value${operation.destination.ordinal}_success`,
      )
      const failureBlock = yield* LlvmBlock.make(
        body,
        `effect_value${operation.destination.ordinal}_failure`,
      )
      const followingBlock = yield* LlvmBlock.make(
        body,
        `effect_value${operation.destination.ordinal}_following`,
      )
      yield* FunctionBody.conditionalBranch(body, succeeded, successBlock, failureBlock)
      yield* LlvmBlock.setInsertionPoint(body, successBlock)
      yield* storeMutable(
        operation.destination,
        Object.freeze(outcomeValues.slice(1, 1 + resultLaneCount)),
      )
      yield* FunctionBody.branch(body, followingBlock)
      yield* LlvmBlock.setInsertionPoint(body, failureBlock)
      let mappedTag: Value.Input = yield* Constant.integerSigned(builder, i32, -1n)
      for (const [ordinal, mapping] of operation.tagMappings.entries()) {
        const source = yield* Constant.integerSigned(builder, i32, BigInt(mapping.source))
        const matches = yield* FunctionBody.integerCompare(
          body,
          'eq',
          tag,
          source,
          `effect_value_tag${operation.destination.ordinal}_${ordinal}`,
        )
        mappedTag = yield* FunctionBody.select(
          body,
          matches,
          yield* Constant.integerSigned(builder, i32, BigInt(mapping.target)),
          mappedTag,
          `effect_value_mapped_tag${operation.destination.ordinal}_${ordinal}`,
        )
      }
      // Owners still live at this site release before the failure leaves the function
      // through their complete cleanup plans, matching the Drop lowering.
      for (const release of operation.releases ?? []) {
        if (!CleanupPlan.hasEffect(release.cleanup)) continue
        yield* dropThroughPlan(
          release.cleanup,
          readLocal(release.local),
          `propagation_release${release.local.ordinal}`,
        )
      }
      const returned: Array<Value.Input> = [
        mappedTag,
        ...(yield* failurePayload(
          outcomeValues,
          operation.outcomeType.type,
          tag,
          operation.propagationType.type,
          operation.tagMappings,
          `effect_value${operation.destination.ordinal}_payload`,
        )),
      ]
      if (entry.suspendable) {
        yield* returnStep(0n, Object.freeze(returned), 'propagated_effect_value_step')
      } else {
        yield* FunctionBody.returnValue(
          body,
          returned.length === 1
            ? (returned.at(0) ?? mappedTag)
            : yield* FunctionBody.buildAggregate(
                body,
                entry.resultType,
                Object.freeze(returned.slice(0, operation.propagationLaneCount)),
                'propagated_effect_value',
              ),
        )
      }
      yield* LlvmBlock.setInsertionPoint(body, followingBlock)
      // Both arms of this outcome dispatch reach here, so neither arm's cached
      // values are readable in the join. Reloading re-roots them at this block.
      yield* reloadMutableRoots(`effect_value${operation.destination.ordinal}_following`)
      const storage = mutableStorage.get(operation.destination.ordinal)
      if (storage === undefined)
        throw new RangeError('Effect value run destination is not materialized')
      const loaded: Array<Value.Input> = []
      for (const [lane, pointer] of storage.entries()) {
        const callingLane = lanesFor(operation.type).at(lane)
        if (callingLane === undefined)
          throw new RangeError('Effect value run destination lost a lane')
        loaded.push(
          yield* FunctionBody.load(
            body,
            laneType(callingLane),
            pointer,
            `effect_value${operation.destination.ordinal}_${lane}`,
          ),
        )
      }
      locals.set(operation.destination.ordinal, Object.freeze(loaded))
      break
    }
    case 'ReifyEffect': {
      const target = declared.find((candidate) =>
        Mir.matchesInstance(candidate.fn, operation.runner, operation.runnerTypeArguments),
      )
      if (target === undefined) throw new RangeError('Backend cannot resolve Effect result runner')
      const reifyArguments = [
        ...readLocal(operation.effect),
        ...operation.arguments.flatMap((argument) => [...readLocal(argument)]),
      ]
      if (
        yield* emitOrigin(
          operation,
          reifyArguments,
          `effect_result_run${operation.destination.ordinal}`,
        )
      )
        break
      const suspension = suspensionRegions.get(operation)
      const outcomeValues = yield* joinSuspensionOutcome(
        operation,
        yield* callValues(
          target,
          reifyArguments,
          `effect_result_run${operation.destination.ordinal}`,
          suspension?._tag === 'RunSuspendableEffectRegion' ? suspension : undefined,
        ),
        `effect_result_run${operation.destination.ordinal}`,
      )
      locals.set(operation.outcome.ordinal, outcomeValues)
      const tag = outcomeValues.at(0)
      if (tag === undefined) throw new RangeError('Effect result lost its outcome tag')
      const zero = yield* Constant.integerSigned(builder, i32, 0n)
      const succeeded = yield* FunctionBody.integerCompare(
        body,
        'eq',
        tag,
        zero,
        `effect_result_success${operation.destination.ordinal}`,
      )
      const successBlock = yield* LlvmBlock.make(
        body,
        `effect_result${operation.destination.ordinal}_success`,
      )
      const failureBlock = yield* LlvmBlock.make(
        body,
        `effect_result${operation.destination.ordinal}_failure`,
      )
      const followingBlock = yield* LlvmBlock.make(
        body,
        `effect_result${operation.destination.ordinal}_following`,
      )
      yield* FunctionBody.conditionalBranch(body, succeeded, successBlock, failureBlock)
      const destinationLanes = operation.resultShape.lanes
      const destinationPayloadLanes = destinationLanes.slice(1)
      const outcomeLanes = operation.outcomeShape.lanes
      const writeBranch = Effect.fnUntraced(function* (
        outerTag: number,
        values: ReadonlyArray<Value.Input>,
        lanes: ReadonlyArray<Layout.CallingLane>,
        label: string,
      ) {
        const branch: Array<Value.Input> = [
          yield* Constant.integerSigned(builder, i32, BigInt(outerTag)),
        ]
        for (const [ordinal, targetLane] of destinationPayloadLanes.entries()) {
          const input = values.at(ordinal)
          const sourceLane = lanes.at(ordinal)
          branch.push(
            input === undefined || sourceLane === undefined
              ? yield* Constant.nullValue(builder, laneType(targetLane))
              : yield* coerceLane(input, sourceLane, targetLane, `${label}_${ordinal}`),
          )
        }
        yield* storeMutable(operation.destination, Object.freeze(branch))
      })
      yield* LlvmBlock.setInsertionPoint(body, successBlock)
      const successLaneCount =
        operation.outcomeShape.tree._tag === 'OutcomeShape'
          ? operation.outcomeShape.tree.success.laneCount
          : 0
      yield* writeBranch(
        operation.successTag,
        Object.freeze(outcomeValues.slice(1, 1 + successLaneCount)),
        Object.freeze(outcomeLanes.slice(1, 1 + successLaneCount)),
        `effect_result${operation.destination.ordinal}_success`,
      )
      yield* FunctionBody.branch(body, followingBlock)
      yield* LlvmBlock.setInsertionPoint(body, failureBlock)
      if (SilkType.failureMembers(operation.outcomeType.type).length === 0) {
        if (trapBlock === undefined)
          trapBlock = yield* LlvmBlock.make(body, 'effect_result_invalid_tag')
        yield* FunctionBody.branch(body, trapBlock)
      } else {
        const failureValues: Array<Value.Input> = []
        if (SilkType.isUnion(operation.failureValueType)) {
          failureValues.push(
            yield* FunctionBody.binary(
              body,
              'sub',
              tag,
              yield* Constant.integerSigned(builder, i32, 1n),
              `effect_result${operation.destination.ordinal}_failure_tag`,
            ),
          )
        }
        failureValues.push(...outcomeValues.slice(1))
        const failureLanes: Array<Layout.CallingLane> = []
        if (SilkType.isUnion(operation.failureValueType)) {
          const failureTagLane = operation.failureValueShape.lanes.at(0)
          if (failureTagLane === undefined)
            throw new RangeError('Effect result lost its failure-union tag lane')
          failureLanes.push(failureTagLane)
        }
        failureLanes.push(...outcomeLanes.slice(1))
        yield* writeBranch(
          operation.failureTag,
          Object.freeze(failureValues),
          Object.freeze(failureLanes),
          `effect_result${operation.destination.ordinal}_failure`,
        )
        yield* FunctionBody.branch(body, followingBlock)
      }
      yield* LlvmBlock.setInsertionPoint(body, followingBlock)
      // Both arms of this outcome dispatch reach here, so neither arm's cached
      // values are readable in the join. Reloading re-roots them at this block.
      yield* reloadMutableRoots(`effect_result${operation.destination.ordinal}_following`)
      const storage = mutableStorage.get(operation.destination.ordinal)
      if (storage === undefined)
        throw new RangeError('Effect result destination is not materialized')
      const loaded: Array<Value.Input> = []
      for (const [ordinal, pointer] of storage.entries()) {
        const lane = destinationLanes.at(ordinal)
        if (lane === undefined) throw new RangeError('Effect result destination lost a lane')
        loaded.push(
          yield* FunctionBody.load(
            body,
            laneType(lane),
            pointer,
            `effect_result${operation.destination.ordinal}_${ordinal}`,
          ),
        )
      }
      locals.set(operation.destination.ordinal, Object.freeze(loaded))
      break
    }
    case 'CloseEffectEntry': {
      const target = declared.find((candidate) =>
        Mir.matchesInstance(candidate.fn, operation.target, operation.typeArguments),
      )
      const runner = declared.find((candidate) =>
        Mir.matchesInstance(candidate.fn, operation.runner, operation.typeArguments),
      )
      if (target === undefined || runner === undefined)
        throw new RangeError('Backend cannot resolve effect entry constructor or runner')
      const effectValues = yield* callValues(target, [], 'effect_entry_make')
      locals.set(operation.effect.ordinal, effectValues)
      const outcomeValues = yield* callValues(runner, effectValues, 'effect_entry_run')
      locals.set(operation.outcome.ordinal, outcomeValues)
      const tag = outcomeValues.at(0)
      if (tag === undefined) throw new RangeError('Effect entry outcome lost its tag')
      const following = yield* LlvmBlock.make(body, 'effect_entry_following')
      const success = yield* LlvmBlock.make(body, 'effect_entry_success')
      const failureDispatch = yield* LlvmBlock.make(body, 'effect_entry_failure')
      yield* FunctionBody.conditionalBranch(
        body,
        yield* FunctionBody.integerCompare(
          body,
          'eq',
          tag,
          yield* Constant.integerSigned(builder, i32, 0n),
          'effect_entry_succeeded',
        ),
        success,
        failureDispatch,
      )
      yield* LlvmBlock.setInsertionPoint(body, success)
      yield* storeMutable(
        operation.destination,
        Object.freeze([yield* Constant.integerSigned(builder, i32, 0n)]),
      )
      yield* FunctionBody.branch(body, following)
      yield* LlvmBlock.setInsertionPoint(body, failureDispatch)
      for (const [ordinal, failure] of operation.failures.entries()) {
        const selected = yield* LlvmBlock.make(body, `effect_entry_tag${failure.tag}`)
        const otherwise = yield* LlvmBlock.make(body, `effect_entry_tag${failure.tag}_otherwise`)
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            tag,
            yield* Constant.integerSigned(builder, i32, BigInt(failure.tag)),
            `effect_entry_is_tag${failure.tag}`,
          ),
          selected,
          otherwise,
        )
        yield* LlvmBlock.setInsertionPoint(body, selected)
        const payloadType = entry.fn.localTypes.at(failure.payload.ordinal)
        if (payloadType === undefined)
          throw new RangeError('Effect entry failure lost its payload type')
        const payloadLaneCount = lanesFor(payloadType).length
        const payload = outcomeValues.slice(1, 1 + payloadLaneCount)
        if (payload.length !== payloadLaneCount) {
          throw new RangeError('Effect entry failure lost its typed payload lanes')
        }
        locals.set(failure.payload.ordinal, Object.freeze(payload))
        if (CleanupPlan.hasEffect(failure.cleanup)) {
          yield* dropThroughPlan(
            failure.cleanup,
            Object.freeze(payload),
            `effect_entry_cleanup${failure.tag}`,
          )
        }
        yield* storeMutable(
          operation.destination,
          Object.freeze([yield* Constant.integerSigned(builder, i32, 1n)]),
        )
        yield* FunctionBody.branch(body, following)
        yield* LlvmBlock.setInsertionPoint(body, otherwise)
        if (ordinal === operation.failures.length - 1) {
          if (trapBlock === undefined)
            trapBlock = yield* LlvmBlock.make(body, 'effect_entry_invalid_tag')
          yield* FunctionBody.branch(body, trapBlock)
        }
      }
      if (operation.failures.length === 0) {
        if (trapBlock === undefined)
          trapBlock = yield* LlvmBlock.make(body, 'effect_entry_invalid_tag')
        yield* FunctionBody.branch(body, trapBlock)
      }
      yield* LlvmBlock.setInsertionPoint(body, following)
      // The success arm and every failure-tag arm reach here, so no arm's cached
      // values are readable in the join — and the failure arms run cleanup, which
      // reloads. Reloading re-roots the cache at this block.
      yield* reloadMutableRoots(`effect_entry${operation.destination.ordinal}_following`)
      const storage = mutableStorage.get(operation.destination.ordinal)
      const pointer = storage?.at(0)
      if (pointer === undefined) throw new RangeError('Effect entry status is not materialized')
      locals.set(
        operation.destination.ordinal,
        Object.freeze([yield* FunctionBody.load(body, i32, pointer, 'effect_entry_status')]),
      )
      break
    }
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
        const environmentValues = readLocal(operation.callable)
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
            const offset = Layout.laneOffset(program.layout, field.type, lane.path)
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
              values: readLocal(capture.source),
            }),
          )
        }
      }
      const captureValues = [...captureGroups]
        .sort((left, right) => left.parameterOrdinal - right.parameterOrdinal)
        .flatMap((capture) => [...capture.values])
      if (target._tag === 'BuiltinCallableTarget') {
        const supplied = Object.freeze([
          ...operation.arguments.flatMap((argument) => [...readLocal(argument)]),
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
          const result = yield* emitIntegerConversion(
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
              yield* emitCallableBinary(
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
          yield* emitCallableBinary(
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
      const result = yield* callValues(
        callableTarget,
        Object.freeze([
          ...operation.arguments.flatMap((argument) => [...readLocal(argument)]),
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
        operation.arguments.flatMap((argument) => [...readLocal(argument)]),
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
  if (trapBlock !== initialTrapBlock) context.setTrapBlock(trapBlock)
  context.setCheckOrdinal(checkOrdinal)
})
