import * as Alignment from '@silklang/llvm/Alignment'
import * as LlvmBlock from '@silklang/llvm/Block'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as Intrinsic from '@silklang/llvm/Intrinsic'
import * as LlvmType from '@silklang/llvm/Type'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as Layout from './Layout.js'
import * as LayoutVerify from './LayoutVerify.js'
import type { LinearOperation } from './MirLinearization.js'
import * as NativeAggregate from './NativeAggregate.js'
import * as NativeHostFailure from './NativeHostFailure.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import type { Context } from './NativeOperationContext.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeType from './NativeType.js'
import * as SilkType from './Type.js'

type Operation = Extract<
  LinearOperation,
  {
    readonly _tag:
      | 'Allocate'
      | 'HostWrite'
      | 'OsCall'
      | 'OsOpenOutcome'
      | 'RawBufferFrom'
      | 'SharedFromAllocation'
      | 'SharedClone'
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

export const emit = Effect.fnUntraced(function* (context: Context, operation: Operation) {
  const {
    body,
    builder,
    cleanup,
    entry,
    free,
    hostFailure,
    i32,
    lanePointers,
    malloc,
    osRuntimes,
    pointer,
    program,
    standardWrite,
    storage: nativeStorage,
    types,
    unsignedOverflowSignatures,
    usizeType,
  } = context
  const initialTrapBlock = context.state.trapBlock
  let trapBlock = initialTrapBlock
  const checkOrdinal = context.state.checkOrdinal
  switch (operation._tag) {
    case 'Allocate': {
      const [bytes, alignment] = NativeStorage.readLocal(nativeStorage, operation.layout)
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
      yield* NativeHostFailure.emit(hostFailure, operation)
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
      nativeStorage.locals.set(
        operation.destination.ordinal,
        Object.freeze([base, bytes, alignment, one, rawAddress, one]),
      )
      break
    }
    case 'HostWrite': {
      const stream = NativeStorage.readLocal(nativeStorage, operation.stream).at(0)
      const [address, length] = NativeStorage.readLocal(nativeStorage, operation.bytes)
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
      yield* NativeHostFailure.emit(hostFailure, operation)
      yield* LlvmBlock.setInsertionPoint(body, written)
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([]))
      break
    }
    case 'OsOpenOutcome': {
      const runtime = osRuntimes.get(operation.operation.name)
      if (runtime === undefined || runtime.abi !== 'OpenOut')
        throw new RangeError(`LLVM OS open runtime ${operation.operation.name} is unavailable`)
      const arguments_ = operation.arguments.flatMap((argument) => [
        ...NativeStorage.readLocal(nativeStorage, argument),
      ])
      const handleLanes = NativeType.lanesFor(types, operation.handleType)
      const outputs = yield* Effect.forEach(handleLanes, (lane, ordinal) =>
        Effect.gen(function* () {
          const type = NativeType.laneType(types, lane)
          const output = yield* FunctionBody.alloca(
            body,
            type,
            `os${operation.valid.ordinal}_out${ordinal}`,
          )
          yield* FunctionBody.store(body, yield* Constant.zero(builder, type), output)
          return output
        }),
      )
      const result = yield* FunctionBody.callDirect(
        body,
        runtime.handle,
        [...arguments_, ...outputs],
        `os${operation.valid.ordinal}`,
      )
      for (const root of [...nativeStorage.addressRoots].sort((left, right) => left - right)) {
        yield* NativeStorage.reloadAddressRoot(nativeStorage, root)
      }
      if (result === undefined) throw new RangeError('LLVM OS open runtime returned no status')
      const handle: Array<Value.Input> = []
      for (const [ordinal, output] of outputs.entries()) {
        const lane = handleLanes.at(ordinal)
        if (lane === undefined) throw new RangeError('LLVM OS open runtime lost an output lane')
        handle.push(
          yield* FunctionBody.load(
            body,
            NativeType.laneType(types, lane),
            output,
            `os${operation.valid.ordinal}_out${ordinal}_value`,
          ),
        )
      }
      nativeStorage.locals.set(operation.valid.ordinal, Object.freeze([result]))
      nativeStorage.locals.set(operation.handle.ordinal, Object.freeze(handle))
      break
    }
    case 'OsCall': {
      const runtime = osRuntimes.get(operation.operation.name)
      if (runtime === undefined) {
        throw new RangeError(`LLVM OS runtime ${operation.operation.name} is unavailable`)
      }
      const arguments_ = operation.arguments.flatMap((argument) => [
        ...NativeStorage.readLocal(nativeStorage, argument),
      ])
      const result = yield* FunctionBody.callDirect(
        body,
        runtime.handle,
        arguments_,
        `os${operation.destination.ordinal}`,
      )
      for (const root of [...nativeStorage.addressRoots].sort((left, right) => left - right)) {
        yield* NativeStorage.reloadAddressRoot(nativeStorage, root)
      }
      if (runtime.resultLaneCount === 0) {
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([]))
        break
      }
      if (result === undefined) throw new RangeError('LLVM OS runtime returned no value')
      if (runtime.resultLaneCount === 1) {
        nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([result]))
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
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'RawBufferFrom': {
      const allocation = NativeStorage.readLocal(nativeStorage, operation.allocation)
      const count = NativeStorage.readLocal(nativeStorage, operation.count).at(0)
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
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([...allocation, count]))
      break
    }
    case 'SharedFromAllocation': {
      const allocation = NativeStorage.readLocal(nativeStorage, operation.allocation)
      const baseAddress = allocation.at(0)
      const bytes = allocation.at(1)
      const alignment = allocation.at(2)
      if (
        baseAddress === undefined ||
        bytes === undefined ||
        alignment === undefined ||
        usizeType === undefined
      )
        throw new RangeError('LLVM local-shared initialization lost its allocation lanes')
      const bytesMismatch = yield* FunctionBody.integerCompare(
        body,
        'ne',
        bytes,
        yield* Constant.integerUnsigned(builder, usizeType, BigInt(operation.block.size)),
        `shared${operation.destination.ordinal}_bytes_mismatch`,
      )
      const alignmentMismatch = yield* FunctionBody.integerCompare(
        body,
        'ne',
        alignment,
        yield* Constant.integerUnsigned(builder, usizeType, BigInt(operation.block.alignment)),
        `shared${operation.destination.ordinal}_alignment_mismatch`,
      )
      const invalid = yield* FunctionBody.binary(
        body,
        'or',
        bytesMismatch,
        alignmentMismatch,
        `shared${operation.destination.ordinal}_invalid`,
      )
      if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'shared_trap')
      const accepted = yield* LlvmBlock.make(
        body,
        `shared${operation.destination.ordinal}_accepted`,
      )
      yield* FunctionBody.conditionalBranch(body, invalid, trapBlock, accepted)
      yield* LlvmBlock.setInsertionPoint(body, accepted)
      const base = yield* FunctionBody.cast(
        body,
        'inttoptr',
        baseAddress,
        pointer,
        `shared${operation.destination.ordinal}_base`,
      )
      const storeWord = Effect.fnUntraced(function* (offset: number, value: Value.Input) {
        yield* FunctionBody.store(
          body,
          value,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            base,
            offset,
            `shared${operation.destination.ordinal}_${offset}_ptr`,
          ),
        )
      })
      yield* storeWord(
        operation.block.strongOffset,
        yield* Constant.integerUnsigned(builder, usizeType, 1n),
      )
      yield* storeWord(
        operation.block.accessOffset,
        yield* Constant.integerUnsigned(builder, usizeType, 0n),
      )
      const allocationLanes = Layout.callingShape(program.layout, SilkType.allocation)?.lanes
      const valueLanes = Layout.callingShape(program.layout, operation.element)?.lanes
      const payload = NativeStorage.readLocal(nativeStorage, operation.value)
      if (allocationLanes === undefined || valueLanes === undefined)
        throw new RangeError('LLVM local-shared initialization lost its calling shapes')
      for (const [ordinal, lane] of allocationLanes.entries()) {
        const value = allocation.at(ordinal)
        const offset = LayoutVerify.laneOffset(program.layout, SilkType.allocation, lane.path)
        if (value === undefined || offset === undefined)
          throw new RangeError('LLVM local-shared initialization lost reclaim provenance')
        yield* storeWord(operation.block.allocationOffset + offset, value)
      }
      for (const [ordinal, lane] of valueLanes.entries()) {
        const value = payload.at(ordinal)
        const offset = LayoutVerify.laneOffset(program.layout, operation.element, lane.path)
        if (value === undefined || offset === undefined)
          throw new RangeError('LLVM local-shared initialization lost its payload')
        yield* storeWord(operation.block.valueOffset + offset, value)
      }
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([baseAddress]))
      break
    }
    case 'SharedClone': {
      const self = NativeStorage.readLocal(nativeStorage, operation.self).at(0)
      if (self === undefined || usizeType === undefined)
        throw new RangeError('LLVM local-shared clone lost its borrowed handle')
      const baseAddress = yield* FunctionBody.load(
        body,
        usizeType,
        self,
        `shared${operation.destination.ordinal}_base_address`,
      )
      const base = yield* FunctionBody.cast(
        body,
        'inttoptr',
        baseAddress,
        pointer,
        `shared${operation.destination.ordinal}_base`,
      )
      const countPointer = yield* NativeLanePointer.lanePointer(
        lanePointers,
        body,
        base,
        operation.block.strongOffset,
        `shared${operation.destination.ordinal}_strong_ptr`,
      )
      const count = yield* FunctionBody.load(
        body,
        usizeType,
        countPointer,
        `shared${operation.destination.ordinal}_strong`,
      )
      const overflow = yield* FunctionBody.integerCompare(
        body,
        'eq',
        count,
        yield* Constant.integerUnsigned(builder, usizeType, operation.block.strongMaximum),
        `shared${operation.destination.ordinal}_overflow`,
      )
      if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'shared_clone_trap')
      const accepted = yield* LlvmBlock.make(
        body,
        `shared${operation.destination.ordinal}_clone_accepted`,
      )
      yield* FunctionBody.conditionalBranch(body, overflow, trapBlock, accepted)
      yield* LlvmBlock.setInsertionPoint(body, accepted)
      const incremented = yield* FunctionBody.binary(
        body,
        'add',
        count,
        yield* Constant.integerUnsigned(builder, usizeType, 1n),
        `shared${operation.destination.ordinal}_incremented`,
      )
      yield* FunctionBody.store(body, incremented, countPointer)
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([baseAddress]))
      break
    }
    case 'RawBufferCount': {
      const address = NativeStorage.readLocal(nativeStorage, operation.buffer).at(0)
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
        yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          address,
          NativeAggregate.fieldOffset(program.layout, referenceType.type.target, 'count'),
          `raw_buffer_count${operation.destination.ordinal}_ptr`,
        ),
        `raw_buffer_count${operation.destination.ordinal}`,
      )
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([value]))
      break
    }
    case 'RawBufferSlot': {
      const address = NativeStorage.readLocal(nativeStorage, operation.buffer).at(0)
      const index = NativeStorage.readLocal(nativeStorage, operation.index).at(0)
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
        yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          address,
          NativeAggregate.fieldOffset(program.layout, bufferType, 'count'),
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
      const allocationOffset = NativeAggregate.fieldOffset(
        program.layout,
        bufferType,
        '$allocation',
      )
      const baseAddress = yield* FunctionBody.load(
        body,
        usizeType,
        yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          address,
          allocationOffset +
            NativeAggregate.fieldOffset(program.layout, SilkType.allocation, '$base'),
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
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([selected]))
      break
    }
    case 'RawBufferRead': {
      const address = NativeStorage.readLocal(nativeStorage, operation.buffer).at(0)
      const index = NativeStorage.readLocal(nativeStorage, operation.index).at(0)
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
        yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          address,
          NativeAggregate.fieldOffset(program.layout, bufferType, 'count'),
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
      const allocationOffset = NativeAggregate.fieldOffset(
        program.layout,
        bufferType,
        '$allocation',
      )
      const baseAddress = yield* FunctionBody.load(
        body,
        usizeType,
        yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          address,
          allocationOffset +
            NativeAggregate.fieldOffset(program.layout, SilkType.allocation, '$base'),
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
            NativeType.laneType(types, lane),
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              base,
              laneOffset,
              `raw_read${operation.destination.ordinal}_${ordinal}_ptr`,
            ),
            `raw_read${operation.destination.ordinal}_${ordinal}`,
          ),
        )
      }
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'RawBufferView': {
      const address = NativeStorage.readLocal(nativeStorage, operation.buffer).at(0)
      const offset = NativeStorage.readLocal(nativeStorage, operation.offset).at(0)
      const length = NativeStorage.readLocal(nativeStorage, operation.length).at(0)
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
        yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          address,
          NativeAggregate.fieldOffset(program.layout, bufferType, 'count'),
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
      const allocationOffset = NativeAggregate.fieldOffset(
        program.layout,
        bufferType,
        '$allocation',
      )
      const baseAddress = yield* FunctionBody.load(
        body,
        usizeType,
        yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          address,
          allocationOffset +
            NativeAggregate.fieldOffset(program.layout, SilkType.allocation, '$base'),
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
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([base, length]))
      break
    }
    case 'RawBufferCopy': {
      const address = NativeStorage.readLocal(nativeStorage, operation.buffer).at(0)
      const offset = NativeStorage.readLocal(nativeStorage, operation.offset).at(0)
      const sourceLanes = NativeStorage.readLocal(nativeStorage, operation.source)
      const sourceAddress = sourceLanes.at(0)
      const sourceLength = sourceLanes.at(1)
      const length = NativeStorage.readLocal(nativeStorage, operation.length).at(0)
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
        yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          address,
          NativeAggregate.fieldOffset(program.layout, bufferType, 'count'),
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
      const allocationOffset = NativeAggregate.fieldOffset(
        program.layout,
        bufferType,
        '$allocation',
      )
      const baseAddress = yield* FunctionBody.load(
        body,
        usizeType,
        yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          address,
          allocationOffset +
            NativeAggregate.fieldOffset(program.layout, SilkType.allocation, '$base'),
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
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([]))
      break
    }
    case 'RawBufferFill': {
      const address = NativeStorage.readLocal(nativeStorage, operation.buffer).at(0)
      const offset = NativeStorage.readLocal(nativeStorage, operation.offset).at(0)
      const length = NativeStorage.readLocal(nativeStorage, operation.length).at(0)
      const value = NativeStorage.readLocal(nativeStorage, operation.value).at(0)
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
        yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          address,
          NativeAggregate.fieldOffset(program.layout, bufferType, 'count'),
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
      const allocationOffset = NativeAggregate.fieldOffset(
        program.layout,
        bufferType,
        '$allocation',
      )
      const baseAddress = yield* FunctionBody.load(
        body,
        usizeType,
        yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          address,
          allocationOffset +
            NativeAggregate.fieldOffset(program.layout, SilkType.allocation, '$base'),
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
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([]))
      break
    }
    case 'SlotWrite': {
      const address = NativeStorage.readLocal(nativeStorage, operation.slot).at(0)
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
      const values = NativeStorage.readLocal(nativeStorage, operation.value)
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
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            base,
            offset,
            `slot_write${operation.destination.ordinal}_${ordinal}_ptr`,
          ),
        )
      }
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([]))
      break
    }
    case 'ValidateLayout': {
      const bytes = NativeStorage.readLocal(nativeStorage, operation.bytes).at(0)
      const alignment = NativeStorage.readLocal(nativeStorage, operation.alignment).at(0)
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
      const lanes = NativeType.lanesFor(types, operation.type)
      const values: Array<Value.Input> = [tag, first, second]
      while (values.length < lanes.length) {
        const lane = lanes.at(values.length)
        if (lane === undefined) break
        values.push(yield* Constant.nullValue(builder, NativeType.laneType(types, lane)))
      }
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'RepeatLayout': {
      const layoutValues = NativeStorage.readLocal(nativeStorage, operation.layout)
      const bytes = layoutValues.at(0)
      const alignment = layoutValues.at(1)
      const count = NativeStorage.readLocal(nativeStorage, operation.count).at(0)
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
      const lanes = NativeType.lanesFor(types, operation.type)
      const values: Array<Value.Input> = [tag, totalOut, alignment]
      while (values.length < lanes.length) {
        const lane = lanes.at(values.length)
        if (lane === undefined) break
        values.push(yield* Constant.nullValue(builder, NativeType.laneType(types, lane)))
      }
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'SlotTake':
    case 'SlotCopy': {
      const address = NativeStorage.readLocal(nativeStorage, operation.slot).at(0)
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
            NativeType.laneType(types, lane),
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              base,
              offset,
              `slot_take${operation.destination.ordinal}_${ordinal}_ptr`,
            ),
            `slot_take${operation.destination.ordinal}_${ordinal}`,
          ),
        )
      }
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'SlotDrop': {
      const address = NativeStorage.readLocal(nativeStorage, operation.slot).at(0)
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
            NativeType.laneType(types, lane),
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
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
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([]))
      break
    }
  }
  if (trapBlock !== initialTrapBlock) context.state.trapBlock = trapBlock
  context.state.checkOrdinal = checkOrdinal
})
