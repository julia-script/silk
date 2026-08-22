import * as Alignment from '@silk-effect/llvm/Alignment'
import * as LlvmBlock from '@silk-effect/llvm/Block'
import * as Constant from '@silk-effect/llvm/Constant'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import * as Intrinsic from '@silk-effect/llvm/Intrinsic'
import * as LlvmType from '@silk-effect/llvm/Type'
import type * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'
import * as Layout from './Layout.js'
import * as LayoutVerify from './LayoutVerify.js'
import type { LinearOperation } from './MirLinearization.js'
import * as NativeAggregate from './NativeAggregate.js'
import * as NativeFunction from './NativeFunction.js'
import type { LoweringContext } from './NativeOperation.js'
import * as SilkType from './Type.js'

type Operation = Extract<
  LinearOperation,
  {
    readonly _tag:
      | 'Allocate'
      | 'HostWrite'
      | 'OsCall'
      | 'RawBufferFrom'
      | 'RawBufferCount'
      | 'RawBufferSlot'
      | 'RawBufferRead'
      | 'RawBufferView'
      | 'RawBufferCopy'
      | 'RawBufferFill'
      | 'SlotWrite'
      | 'ValidateLayout'
      | 'RepeatLayout'
      | 'SlotTake'
      | 'SlotCopy'
      | 'SlotDrop'
  }
>

export const emit = Effect.fnUntraced(function* (context: LoweringContext, operation: Operation) {
  const {
    addressRoots,
    aggregateFieldOffset,
    body,
    builder,
    constantBytePointer,
    cleanup,
    emitHostFailure,
    entry,
    free,
    i32,
    laneType,
    lanesFor,
    locals,
    malloc,
    osRuntimes,
    pointer,
    program,
    reloadAddressRoot,
    standardWrite,
    unsignedOverflowSignatures,
    usizeType,
  } = context
  const initialTrapBlock = context.state.trapBlock
  let trapBlock = initialTrapBlock
  const checkOrdinal = context.state.checkOrdinal
  switch (operation._tag) {
    case 'Allocate': {
      const [bytes, alignment] = NativeFunction.readLocal(locals, operation.layout)
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
      const stream = NativeFunction.readLocal(locals, operation.stream).at(0)
      const [address, length] = NativeFunction.readLocal(locals, operation.bytes)
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
      const arguments_ = operation.arguments.flatMap((argument) => [
        ...NativeFunction.readLocal(locals, argument),
      ])
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
      const allocation = NativeFunction.readLocal(locals, operation.allocation)
      const count = NativeFunction.readLocal(locals, operation.count).at(0)
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
      const address = NativeFunction.readLocal(locals, operation.buffer).at(0)
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
      const address = NativeFunction.readLocal(locals, operation.buffer).at(0)
      const index = NativeFunction.readLocal(locals, operation.index).at(0)
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
      const address = NativeFunction.readLocal(locals, operation.buffer).at(0)
      const index = NativeFunction.readLocal(locals, operation.index).at(0)
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
        const laneOffset = LayoutVerify.laneOffset(program.layout, operation.element, lane.path)
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
      const address = NativeFunction.readLocal(locals, operation.buffer).at(0)
      const offset = NativeFunction.readLocal(locals, operation.offset).at(0)
      const length = NativeFunction.readLocal(locals, operation.length).at(0)
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
      const address = NativeFunction.readLocal(locals, operation.buffer).at(0)
      const offset = NativeFunction.readLocal(locals, operation.offset).at(0)
      const sourceLanes = NativeFunction.readLocal(locals, operation.source)
      const sourceAddress = sourceLanes.at(0)
      const sourceLength = sourceLanes.at(1)
      const length = NativeFunction.readLocal(locals, operation.length).at(0)
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
      const address = NativeFunction.readLocal(locals, operation.buffer).at(0)
      const offset = NativeFunction.readLocal(locals, operation.offset).at(0)
      const length = NativeFunction.readLocal(locals, operation.length).at(0)
      const value = NativeFunction.readLocal(locals, operation.value).at(0)
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
      const address = NativeFunction.readLocal(locals, operation.slot).at(0)
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
      const values = NativeFunction.readLocal(locals, operation.value)
      const lanes = Layout.callingShape(program.layout, operation.element)?.lanes
      if (lanes === undefined) throw new RangeError('LLVM Slot.write lost its shape')
      for (const [ordinal, lane] of lanes.entries()) {
        const value = values.at(ordinal)
        const offset = LayoutVerify.laneOffset(program.layout, operation.element, lane.path)
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
      const bytes = NativeFunction.readLocal(locals, operation.bytes).at(0)
      const alignment = NativeFunction.readLocal(locals, operation.alignment).at(0)
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
      const layoutValues = NativeFunction.readLocal(locals, operation.layout)
      const bytes = layoutValues.at(0)
      const alignment = layoutValues.at(1)
      const count = NativeFunction.readLocal(locals, operation.count).at(0)
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
      const address = NativeFunction.readLocal(locals, operation.slot).at(0)
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
        const offset = LayoutVerify.laneOffset(program.layout, operation.element, lane.path)
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
      const address = NativeFunction.readLocal(locals, operation.slot).at(0)
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
        const offset = LayoutVerify.laneOffset(program.layout, operation.element, lane.path)
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
      yield* NativeAggregate.dropThroughPlan(
        cleanup,
        operation.cleanup,
        Object.freeze(values),
        `slot_drop${operation.destination.ordinal}`,
      )
      locals.set(operation.destination.ordinal, Object.freeze([]))
      break
    }
  }
  if (trapBlock !== initialTrapBlock) context.state.trapBlock = trapBlock
  context.state.checkOrdinal = checkOrdinal
})
