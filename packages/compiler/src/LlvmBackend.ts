import * as Alignment from '@silk-effect/llvm/Alignment'
import * as Bitcode from '@silk-effect/llvm/Bitcode'
import * as LlvmBlock from '@silk-effect/llvm/Block'
import * as Builder from '@silk-effect/llvm/Builder'
import * as Constant from '@silk-effect/llvm/Constant'
import * as DISPFlags from '@silk-effect/llvm/DISPFlags'
import * as FunctionActor from '@silk-effect/llvm/Function'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import * as Intrinsic from '@silk-effect/llvm/Intrinsic'
import * as IrText from '@silk-effect/llvm/IrText'
import type * as LlvmError from '@silk-effect/llvm/LlvmError'
import * as LlvmMetadata from '@silk-effect/llvm/Metadata'
import * as LlvmType from '@silk-effect/llvm/Type'
import * as Value from '@silk-effect/llvm/Value'
import * as Variable from '@silk-effect/llvm/Variable'
import * as Verify from '@silk-effect/llvm/Verify'
import * as Effect from 'effect/Effect'
import type { Backend, CodegenRequest, LlvmBitcodeArtifact, SymbolEntry } from './Backend.js'
import {
  BackendError,
  formatModuleViolations,
  lineTable,
  positionOf,
  sanitize,
  suspensionPointKey,
  symbolFor,
  terminationOf,
} from './Backend.js'
import * as CleanupPlan from './CleanupPlan.js'
import * as CoroutineFrame from './CoroutineFrame.js'
import * as CoroutineRuntime from './CoroutineRuntime.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as FloatingPoint from './FloatingPoint.js'
import * as Instances from './Instances.js'
import { alignUp } from './internal/Align.js'
import * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import * as NativeAggregate from './NativeAggregate.js'
import * as NativeArith from './NativeArith.js'
import * as NativeCall from './NativeCall.js'
import * as NativeControl from './NativeControl.js'
import * as NativeDebug from './NativeDebug.js'
import * as NativeDeclare from './NativeDeclare.js'
import * as NativeFunction from './NativeFunction.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import * as NativeOperation from './NativeOperation.js'
import * as NativeSuspension from './NativeSuspension.js'
import * as NativeTranscendental from './NativeTranscendental.js'
import * as NativeType from './NativeType.js'
import * as Scalar from './Scalar.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Target from './Target.js'
import * as SilkType from './Type.js'

/**
 * Field paths to every reclaim context inside one plan, or undefined when the plan holds
 * shapes (hooks, arrays, nested unions, callables) that conditional union cleanup cannot lower.
 */
/**
 * Code generation as a nominal `Backend` service: one operation consuming the whole
 * monomorphized MIR program plus its compiler-owned target/layout plan and codegen request,
 * producing one program artifact. The bootstrap `LlvmBackend` lowers MIR through the Silk LLVM builder and
 * emits deterministic bitcode directly — no `libLLVM`, no LLVM C API, no compiler-private
 * native FFI. Textual LLVM IR is an implementation-specific inspection artifact.
 */

import type { LinearBlock } from './MirLinearization.js'
import { destinationOf, linearize, llvmControl } from './MirLinearization.js'

const emitProgram = Effect.fnUntraced(function* (
  program: Mir.Module,
  request: CodegenRequest,
): Effect.fn.Return<
  {
    readonly symbols: ReadonlyArray<SymbolEntry>
    readonly nativeRuntimeSymbols: ReadonlyArray<string>
    readonly ir: string
    readonly bitcode: Uint8Array
  },
  BackendError | LlvmError.LlvmError
> {
  const suspensionEnabled = program.functions.some((fn) => (fn.suspension?.regions.length ?? 0) > 0)
  const i32Layout = Layout.entry(program.layout, 'i32')
  if (i32Layout === undefined || i32Layout.representation._tag !== 'SignedInteger') {
    return yield* new BackendError({
      operation: 'Backend.emit',
      backend: 'LLVM',
      message: 'LLVM requires the planned i32 representation',
      reason: { _tag: 'InvalidMir', violations: Mir.verify(program) },
    })
  }
  const scalarBits = i32Layout.representation.bits
  const builder = yield* Builder.make({
    sourceFilename: program.module,
    targetTriple: program.layout.target.id,
    strip: request.mode !== 'debug',
  })
  const i32 = yield* LlvmType.integer(builder, scalarBits)
  const usesScalar = (spelling: Scalar.Spelling): boolean =>
    program.layout.callingShapes.some((shape) => shape.lanes.some((lane) => lane.type === spelling))
  // LLVM assigns type-table identities in creation order, so preserve byte-for-byte output for
  // programs that do not use floating-point values by creating these types only when required.
  const f32 = usesScalar('f32') ? yield* LlvmType.float(builder) : i32
  const f64 = usesScalar('f64') ? yield* LlvmType.double(builder) : i32
  const usizeLayout = Layout.entry(program.layout, 'usize')
  const usizeType =
    usizeLayout?.representation._tag === 'UnsignedInteger'
      ? yield* LlvmType.integer(builder, usizeLayout.representation.bits)
      : suspensionEnabled
        ? yield* LlvmType.integer(builder, program.layout.target.pointerSize * 8)
        : undefined
  const integerTypes = new Map<number, LlvmType.Type>([[32, i32]])
  if (usizeLayout?.representation._tag === 'UnsignedInteger' && usizeType !== undefined) {
    integerTypes.set(usizeLayout.representation.bits, usizeType)
  }
  for (const bits of [8, 16, 64] as const) {
    if (!integerTypes.has(bits)) integerTypes.set(bits, yield* LlvmType.integer(builder, bits))
  }
  const hasAddressLane =
    suspensionEnabled ||
    (program.staticData?.length ?? 0) > 0 ||
    program.functions.some((fn) =>
      Mir.operations(fn).some(
        (operation) =>
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
          operation._tag === 'SlotDrop',
      ),
    ) ||
    program.layout.callingShapes.some((shape) =>
      shape.lanes.some((lane) => typeof lane.type !== 'string'),
    ) ||
    program.layout.effectEnvironments.some(
      (environment) =>
        environment._tag === 'EffectEnvironment' &&
        environment.fields.some((field) => field.representation === 'Borrow'),
    ) ||
    program.layout.callableEnvironments.some(
      (environment) =>
        environment._tag === 'CallableEnvironment' &&
        environment.fields.some((field) => field.representation === 'Borrow'),
    )
  const i8 = hasAddressLane ? yield* LlvmType.integer(builder, 8) : i32
  const pointer = hasAddressLane ? yield* LlvmType.pointer(builder) : i32
  const lanePointers: NativeLanePointer.Context = Object.freeze({
    builder,
    byteType: i8,
    offsetType: i32,
  })
  const staticPointers = new Map<string, Constant.Constant>()
  for (const [ordinal, data] of (program.staticData ?? []).entries()) {
    const storageType = yield* LlvmType.array(builder, i8, data.bytes.length)
    const initializer = yield* Constant.string(builder, Uint8Array.from(data.bytes))
    const variable = yield* Variable.make(builder, `silk.static.${ordinal}`, storageType, {
      initializer,
      constant: true,
      linkage: 'internal',
      unnamedAddress: 'unnamed_addr',
    })
    staticPointers.set(
      data.id,
      yield* Constant.fromGlobal(builder, yield* Variable.global(builder, variable)),
    )
  }
  let voidType: LlvmType.Type | undefined
  const lanesFor = (type: Mir.Type): ReadonlyArray<Layout.CallingLane> => {
    if (type._tag === 'EffectComposite') {
      const payloadTypes = type.alternatives.flatMap((alternative) =>
        lanesFor(alternative).map((lane) => lane.type),
      )
      return Object.freeze([
        Object.freeze({
          _tag: 'CallingLane' as const,
          path: Object.freeze([Object.freeze({ _tag: 'UnionTagSelector' as const })]),
          type: 'i32' as const,
        }),
        ...payloadTypes.map((laneType, slot) =>
          Object.freeze({
            _tag: 'CallingLane' as const,
            path: Object.freeze([Object.freeze({ _tag: 'UnionPayloadSelector' as const, slot })]),
            type: laneType,
          }),
        ),
      ])
    }
    if (type._tag === 'EffectBorrow')
      return Object.freeze([
        Object.freeze({
          _tag: 'CallingLane' as const,
          path: Object.freeze([]),
          type: Object.freeze({
            _tag: 'Address' as const,
            element: type.type,
            bits: program.layout.target.pointerSize === 4 ? 32 : 64,
          }),
        }),
      ])
    if (type._tag === 'EffectValue' && type.storage !== undefined) {
      const shape = Layout.callingShape(program.layout, type.storage.type)
      if (shape === undefined)
        throw new RangeError('LLVM backend lost a stored Effect calling shape')
      return shape.lanes
    }
    if (type._tag === 'EffectValue')
      return Layout.effectEnvironmentLanes(program.layout, type.environment)
    if (type._tag === 'CallableValue' && type.storage !== undefined) {
      const shape = Layout.callingShape(program.layout, type.storage.type)
      if (shape === undefined)
        throw new RangeError('LLVM backend lost a stored callable calling shape')
      return shape.lanes
    }
    if (type._tag === 'CallableValue')
      return type.environment === undefined
        ? Object.freeze([])
        : Layout.callableEnvironmentLanes(program.layout, type.environment)
    const shape = Layout.callingShape(program.layout, Mir.semanticType(type))
    if (shape === undefined) {
      throw new RangeError(`LLVM backend lost calling shape for ${Mir.semanticType(type)}`)
    }
    return shape.lanes
  }
  const valueLanesFor = (type: Mir.Type): ReadonlyArray<Layout.CallingLane> => {
    if (type._tag !== 'EffectBorrow') return lanesFor(type)
    const shape = Layout.callingShape(program.layout, type.type)
    if (shape === undefined) {
      throw new RangeError(
        `LLVM backend lost borrowed calling shape for ${SilkType.encode(type.type)}`,
      )
    }
    return shape.lanes
  }
  const laneType = (lane: Layout.CallingLane): LlvmType.Type => {
    if (typeof lane.type !== 'string') return pointer
    const scalar = Scalar.find(lane.type)
    if (scalar === undefined) return i32
    if (scalar.category === 'Floating') return scalar.spelling === 'f32' ? f32 : f64
    const bits = Scalar.bits(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
    return integerTypes.get(bits) ?? i32
  }
  const transferHeaderSize = program.layout.target.pointerSize * 3
  const originArgumentLanes = program.functions.flatMap((fn) =>
    (fn.suspension?.regions ?? []).flatMap((region) =>
      region._tag === 'SuspendEffectRegion'
        ? [
            NativeSuspension.logicalLanes(
              fn,
              NativeCall.operationInputs(region.operation),
              lanesFor,
            ),
          ]
        : [],
    ),
  )
  const transferArgumentSize = originArgumentLanes.reduce(
    (maximum, lanes) => Math.max(maximum, NativeType.packLanes(program.layout.target, lanes).end),
    0,
  )
  const transferResultOffset = alignUp(
    transferHeaderSize + transferArgumentSize,
    program.layout.target.pointerAlignment,
  )
  const transferResultSize = program.functions.reduce(
    (maximum, fn) =>
      Math.max(maximum, NativeType.packLanes(program.layout.target, lanesFor(fn.result)).end),
    0,
  )
  const transferStorageSize = alignUp(
    transferResultOffset + transferResultSize,
    program.layout.target.pointerAlignment,
  )
  type OverflowSignature = {
    readonly returnType: LlvmType.Type
    readonly parameters: ReadonlyArray<LlvmType.Type>
  }
  const signedOverflowSignatures = new Map<number, OverflowSignature>()
  const unsignedOverflowSignatures = new Map<number, OverflowSignature>()
  const needsAllocation = program.functions.some((fn) =>
    Mir.operations(fn).some(NativeOperation.needsAllocation),
  )
  const malloc =
    needsAllocation && usizeType !== undefined
      ? yield* FunctionActor.declare(
          builder,
          'malloc',
          yield* LlvmType.functionType(builder, pointer, [usizeType]),
        )
      : undefined
  const free = needsAllocation
    ? yield* FunctionActor.declare(
        builder,
        'free',
        yield* LlvmType.functionType(builder, voidType ?? (yield* LlvmType.voidType(builder)), [
          pointer,
        ]),
      )
    : undefined
  const coroutineFramePush =
    suspensionEnabled && usizeType !== undefined
      ? yield* FunctionActor.declare(
          builder,
          CoroutineRuntime.pushSymbol,
          yield* LlvmType.functionType(builder, pointer, [usizeType, usizeType]),
        )
      : undefined
  const coroutineFramePop = suspensionEnabled
    ? yield* FunctionActor.declare(
        builder,
        CoroutineRuntime.popSymbol,
        yield* LlvmType.functionType(builder, voidType ?? (yield* LlvmType.voidType(builder)), [
          pointer,
        ]),
      )
    : undefined
  const needsStringEquality = program.functions.some((fn) =>
    Mir.operations(fn).some((operation) => operation._tag === 'StringEqualsExact'),
  )
  const memcmp =
    needsStringEquality && usizeType !== undefined
      ? yield* FunctionActor.declare(
          builder,
          'memcmp',
          yield* LlvmType.functionType(builder, i32, [pointer, pointer, usizeType]),
        )
      : undefined
  const needsHostWrite = program.functions.some((fn) =>
    Mir.operations(fn).some((operation) => operation._tag === 'HostWrite'),
  )
  const standardWrite =
    needsHostWrite && usizeType !== undefined
      ? yield* FunctionActor.declare(
          builder,
          'silk_standard_stream_write_v1',
          yield* LlvmType.functionType(builder, i32, [i32, pointer, usizeType]),
        )
      : undefined

  const osRuntimes = new Map<
    string,
    {
      readonly handle: FunctionActor.Function
      readonly abi: 'Direct' | 'OpenOut'
      readonly resultLaneCount: number
      readonly symbol: string
    }
  >()
  for (const operation of program.functions.flatMap((fn) => Mir.operations(fn))) {
    if (operation._tag !== 'OsCall' || osRuntimes.has(operation.operation.name)) continue
    const resultLanes = lanesFor(operation.type)
    const abi =
      operation.operation.name === 'osFileOpen' || operation.operation.name === 'osDirectoryOpen'
        ? 'OpenOut'
        : 'Direct'
    const singleResultLane = resultLanes.at(0)
    const resultType =
      abi === 'OpenOut'
        ? i32
        : resultLanes.length === 0
          ? (voidType ?? (yield* LlvmType.voidType(builder)))
          : resultLanes.length === 1 && singleResultLane !== undefined
            ? laneType(singleResultLane)
            : yield* LlvmType.structure(builder, resultLanes.map(laneType))
    const parameters = operation.arguments.flatMap((argument) => {
      const type = program.functions
        .find((fn) => Mir.operations(fn).includes(operation))
        ?.localTypes.at(argument.ordinal)
      return type === undefined ? [] : lanesFor(type).map(laneType)
    })
    osRuntimes.set(
      operation.operation.name,
      Object.freeze({
        abi,
        symbol: NativeDeclare.osRuntimeSymbol(operation.operation.name),
        handle: yield* FunctionActor.declare(
          builder,
          NativeDeclare.osRuntimeSymbol(operation.operation.name),
          yield* LlvmType.functionType(
            builder,
            resultType,
            abi === 'OpenOut' ? [...parameters, pointer, pointer, pointer] : parameters,
          ),
        ),
        resultLaneCount: resultLanes.length,
      }),
    )
  }

  const declared: Array<{
    readonly fn: Mir.MirFunction
    readonly symbol: string
    readonly publicSymbol: string
    readonly handle: FunctionActor.Function
    readonly resultType: LlvmType.Type
    readonly emittedResultType: LlvmType.Type
    readonly resultLaneCount: number
    readonly suspendable: boolean
    readonly parameterTypes: ReadonlyArray<LlvmType.Type>
    readonly linear: ReadonlyArray<LinearBlock>
  }> = []
  for (const fn of program.functions) {
    const resultLaneCount = lanesFor(fn.result).length
    let resultType: LlvmType.Type
    if (resultLaneCount === 0) {
      const selected = voidType ?? (yield* LlvmType.voidType(builder))
      voidType = selected
      resultType = selected
    } else if (resultLaneCount === 1) {
      const lane = lanesFor(fn.result).at(0)
      if (lane === undefined) throw new RangeError('LLVM result lost its scalar lane')
      resultType = laneType(lane)
    } else {
      resultType = yield* LlvmType.structure(builder, lanesFor(fn.result).map(laneType))
    }
    const parameters =
      fn.regions.length === 0
        ? []
        : fn.localTypes.slice(0, fn.parameterCount).flatMap((type) => lanesFor(type).map(laneType))
    const suspendable =
      fn.suspension !== undefined && fn.suspension.classification !== 'Synchronous'
    const publicSymbol = symbolFor(fn, Mir.machineEntry(program))
    const emittedResultType = suspendable
      ? yield* LlvmType.structure(builder, [i32, ...lanesFor(fn.result).map(laneType)])
      : resultType
    const parameterTypes = suspendable
      ? Object.freeze([...parameters, pointer, pointer, i32])
      : Object.freeze(parameters)
    const signature = yield* LlvmType.functionType(builder, emittedResultType, parameterTypes)
    declared.push({
      fn,
      symbol: suspendable ? `${publicSymbol}$suspend_step` : publicSymbol,
      publicSymbol,
      handle: yield* FunctionActor.declare(
        builder,
        suspendable ? `${publicSymbol}$suspend_step` : publicSymbol,
        signature,
      ),
      resultType,
      emittedResultType,
      resultLaneCount,
      suspendable,
      parameterTypes,
      linear: linearize(fn),
    })
  }
  const childThunkType = suspensionEnabled
    ? yield* LlvmType.functionType(builder, i32, [pointer])
    : undefined
  const resumeThunkType = suspensionEnabled
    ? yield* LlvmType.functionType(builder, i32, [pointer, pointer])
    : undefined
  const originThunks = new Map<
    string,
    {
      readonly handle: FunctionActor.Function
      readonly region: Mir.SuspendEffectRegion
      readonly owner: (typeof declared)[number]
    }
  >()
  const resumeThunks = new Map<
    string,
    {
      readonly handle: FunctionActor.Function
      readonly region: Mir.RunSuspendableEffectRegion
      readonly owner: (typeof declared)[number]
      readonly frame: Mir.CoroutineFrameTargetLayout
      readonly layout: Mir.CoroutineFrameTargetStateLayout
    }
  >()
  for (const owner of declared) {
    for (const region of owner.fn.suspension?.regions ?? []) {
      const key = suspensionPointKey(region.point)
      const suffix = `${sanitize(Instances.keyText(region.point.owner))}_${sanitize(region.point.sourceId)}_${region.point.spanStart}_${region.point.ordinal}`
      if (region._tag === 'SuspendEffectRegion') {
        if (childThunkType === undefined) throw new RangeError('LLVM origin lost thunk type')
        originThunks.set(
          key,
          Object.freeze({
            owner,
            region,
            handle: yield* FunctionActor.declare(
              builder,
              `silk_suspend_child_${suffix}`,
              childThunkType,
            ),
          }),
        )
        continue
      }
      const descriptor = region.relay.state
      if (descriptor === undefined) continue
      const frame = program.coroutineFrames?.entries.find(
        (candidate) =>
          Instances.keyText(candidate.function) === Instances.keyText(owner.fn.instance),
      )
      const layout = CoroutineFrame.stateLayout(program, region.point)
      if (frame === undefined || layout === undefined || resumeThunkType === undefined)
        throw new RangeError('LLVM coroutine frame lost its physical layout or thunk type')
      resumeThunks.set(
        key,
        Object.freeze({
          owner,
          region,
          frame,
          layout,
          handle: yield* FunctionActor.declare(
            builder,
            `silk_suspend_resume_${suffix}`,
            resumeThunkType,
          ),
        }),
      )
    }
  }
  const machine = declared.find((entry) =>
    Mir.matchesInstanceKey(entry.fn, Mir.machineEntry(program)),
  )
  const driver =
    machine?.suspendable === true
      ? yield* FunctionActor.declare(
          builder,
          machine.publicSymbol,
          yield* LlvmType.functionType(
            builder,
            machine.resultType,
            machine.parameterTypes.slice(0, -3),
          ),
        )
      : undefined

  const debug = request.mode === 'debug'
  let compileUnit: LlvmMetadata.Optional
  let file: LlvmMetadata.Optional
  const table = lineTable(request.sources?.get(program.module))
  if (debug) {
    const fileName = yield* LlvmMetadata.string(builder, program.module)
    file = yield* LlvmMetadata.file(builder, fileName)
    const producer = yield* LlvmMetadata.string(builder, 'silk-effect bootstrap')
    compileUnit = yield* LlvmMetadata.compileUnit(builder, file, producer, {})
    if (compileUnit !== undefined) {
      yield* LlvmMetadata.named(builder, 'llvm.dbg.cu', [compileUnit])
    }
  }

  const debugTypes = new Map<string, LlvmMetadata.Optional>()
  const debugTypeOf = Effect.fnUntraced(function* (
    type: Mir.Type,
  ): Effect.fn.Return<LlvmMetadata.Optional, LlvmError.LlvmError> {
    if (!debug) return undefined
    const semantic = Mir.semanticType(type)
    const key = SilkType.key(semantic)
    if (debugTypes.has(key)) return debugTypes.get(key)
    const selected = Layout.entry(program.layout, semantic)
    if (selected === undefined) {
      debugTypes.set(key, undefined)
      return undefined
    }
    let metadata: LlvmMetadata.Optional
    if (SilkType.isString(semantic)) {
      metadata = yield* LlvmMetadata.stringType(builder, {
        name: yield* LlvmMetadata.string(builder, 'string'),
        sizeInBits: selected.size * 8,
        alignInBits: selected.alignment * 8,
        encoding: 'utf',
      })
    } else if (SilkType.isSlice(semantic) && SilkType.equals(semantic.element, 'u8')) {
      metadata = yield* LlvmMetadata.structureType(builder, {
        name: yield* LlvmMetadata.string(builder, SilkType.encode(semantic)),
        file,
        sizeInBits: selected.size * 8,
        alignInBits: selected.alignment * 8,
      })
    }
    debugTypes.set(key, metadata)
    return metadata
  })

  for (const entry of declared) {
    let subprogram: LlvmMetadata.Optional
    if (debug && file !== undefined && compileUnit !== undefined) {
      const startLine = positionOf(table, NativeDebug.functionStart(entry.fn)).line
      const symbolName = yield* LlvmMetadata.string(builder, entry.symbol)
      const signatureTypes = yield* LlvmMetadata.tuple(builder, [
        yield* debugTypeOf(entry.fn.result),
        ...(yield* Effect.forEach(
          entry.fn.localTypes.slice(0, entry.fn.parameterCount),
          debugTypeOf,
        )),
      ])
      const signature = yield* LlvmMetadata.subroutineType(builder, signatureTypes)
      subprogram = yield* LlvmMetadata.subprogram(builder, file, symbolName, {
        line: startLine,
        scopeLine: startLine,
        type: signature,
        spFlags: DISPFlags.make({ definition: true }),
        compileUnit,
      })
      if (subprogram !== undefined) {
        yield* FunctionActor.setSubprogram(builder, entry.handle, subprogram)
      }
    }
    const scope = subprogram

    yield* FunctionActor.buildBody(
      builder,
      entry.handle,
      Effect.fnUntraced(function* (body) {
        const suspensionRegions = new Map(
          (entry.fn.suspension?.regions ?? []).map((region) => [region.operation, region] as const),
        )
        const resumeControls = [...resumeThunks.values()]
          .filter((resume) => resume.owner === entry)
          .sort((left, right) =>
            suspensionPointKey(left.region.point).localeCompare(
              suspensionPointKey(right.region.point),
            ),
          )
          .map((resume, ordinal) => Object.freeze({ ...resume, ordinal: ordinal + 1 }))
        const coroutineFrame = program.coroutineFrames?.entries.find(
          (candidate) =>
            Instances.keyText(candidate.function) === Instances.keyText(entry.fn.instance),
        )
        const dispatchBlock = entry.suspendable
          ? yield* LlvmBlock.make(body, 'suspend_dispatch')
          : undefined
        const frameReuseBlock =
          entry.suspendable && coroutineFrame !== undefined
            ? yield* LlvmBlock.make(body, 'suspend_invocation_frame_reuse')
            : undefined
        const frameAllocateBlock =
          entry.suspendable && coroutineFrame !== undefined
            ? yield* LlvmBlock.make(body, 'suspend_invocation_frame_push')
            : undefined
        const frameAcquiredBlock =
          entry.suspendable && coroutineFrame !== undefined
            ? yield* LlvmBlock.make(body, 'suspend_invocation_frame_acquired')
            : undefined
        const frameTrapBlock =
          entry.suspendable && coroutineFrame !== undefined
            ? yield* LlvmBlock.make(body, 'suspend_invocation_frame_trap')
            : undefined
        const framePushedBlock =
          entry.suspendable && coroutineFrame !== undefined
            ? yield* LlvmBlock.make(body, 'suspend_invocation_frame_pushed')
            : undefined
        const blocks = new Map<number, LlvmBlock.Block>()
        for (const block of entry.linear) {
          blocks.set(
            block.id.ordinal,
            yield* LlvmBlock.make(
              body,
              `bb${block.id.ordinal}${block.kind === 'Cleanup' ? '_cleanup' : ''}`,
            ),
          )
        }
        const resumeBlocks = new Map(
          yield* Effect.forEach(resumeControls, (resume) =>
            Effect.map(
              LlvmBlock.make(body, `suspend_resume_${resume.ordinal}`),
              (block) => [suspensionPointKey(resume.region.point), block] as const,
            ),
          ),
        )
        const invocationFrameStorage =
          entry.suspendable && coroutineFrame !== undefined
            ? yield* FunctionBody.alloca(body, pointer, 'suspend_invocation_frame_slot')
            : undefined
        let trapBlock: LlvmBlock.Block | undefined
        let checkOrdinal = 0
        const locals = new Map<number, ReadonlyArray<Value.Input>>()
        const roots = NativeFunction.mutableRoots(entry.fn, entry.linear)
        const mutableRoots = roots.mutable
        const addressRoots = roots.address
        const mutableStorage = new Map<number, ReadonlyArray<Value.Input>>()
        for (const root of [...mutableRoots].sort((left, right) => left - right)) {
          const logicalType = entry.fn.localTypes.at(root)
          if (logicalType === undefined) throw new RangeError(`Backend lost mutable root %${root}`)
          if (logicalType._tag === 'EffectBorrow') continue
          const storage: Array<Value.Input> = []
          for (const [lane, callingLane] of valueLanesFor(logicalType).entries()) {
            storage.push(
              yield* FunctionBody.alloca(body, laneType(callingLane), `mut${root}_${lane}`),
            )
          }
          mutableStorage.set(root, Object.freeze(storage))
        }
        const addressStorage = new Map<number, Value.Input>()
        for (const root of [...addressRoots].sort((left, right) => left - right)) {
          const logicalType = entry.fn.localTypes.at(root)
          const layout =
            logicalType === undefined
              ? undefined
              : Layout.entry(program.layout, Mir.semanticType(logicalType))
          if (logicalType === undefined || layout === undefined) {
            throw new RangeError(`Backend lost address-taken value %${root}`)
          }
          const count = yield* Constant.integerUnsigned(builder, i32, BigInt(layout.size))
          addressStorage.set(
            root,
            yield* FunctionBody.alloca(body, i8, `addr${root}`, {
              count,
              alignment: yield* Alignment.fromByteUnits(layout.alignment),
            }),
          )
        }
        const stableAddressFields = new Map<number, Mir.CoroutineFramePayloadField>()
        if (coroutineFrame !== undefined) {
          for (const state of coroutineFrame.states) {
            for (const field of state.payload) {
              if (!addressRoots.has(field.local.ordinal)) continue
              const existing = stableAddressFields.get(field.local.ordinal)
              if (existing !== undefined && existing.offset !== field.offset) {
                throw new RangeError(
                  `LLVM coroutine frame moved borrowed root %${field.local.ordinal} between states`,
                )
              }
              stableAddressFields.set(field.local.ordinal, field)
            }
          }
        }

        /**
         * Re-reads every memory-backed root from its storage into the local value cache.
         *
         * The cache holds SSA values, and an SSA value is only readable in blocks the block
         * defining it dominates. Running this on entry to a block whose predecessors diverge
         * re-establishes that: nothing cached from one predecessor survives into the join, and
         * what replaces it is a load placed in the join itself. Storage is authoritative
         * because every write to one of these roots stores through it, so the reloaded value
         * is also the current one — a cleanup arm that mutated the root is observed, not
         * discarded.
         */
        const reloadMutableRoots = Effect.fnUntraced(function* (tag: string) {
          for (const root of [...mutableRoots].sort((left, right) => left - right)) {
            const storage = mutableStorage.get(root)
            if (storage === undefined) continue
            const loaded: Array<Value.Input> = []
            const logicalType = entry.fn.localTypes.at(root)
            if (logicalType === undefined) throw new RangeError('Mutable root lost its type')
            for (const [lane, pointer] of storage.entries()) {
              const callingLane = valueLanesFor(logicalType).at(lane)
              if (callingLane === undefined) throw new RangeError('Mutable root lost a lane')
              loaded.push(
                yield* FunctionBody.load(
                  body,
                  laneType(callingLane),
                  pointer,
                  `mut${root}_${lane}_load_${tag}`,
                ),
              )
            }
            locals.set(root, Object.freeze(loaded))
          }
        })

        const storeAddressRootValues = Effect.fnUntraced(function* (
          root: number,
          values: ReadonlyArray<Value.Input>,
          name: string,
        ) {
          const base = addressStorage.get(root)
          const logicalType = entry.fn.localTypes.at(root)
          if (base === undefined || logicalType === undefined) {
            throw new RangeError(`Backend lost address storage for %${root}`)
          }
          for (const [ordinal, lane] of valueLanesFor(logicalType).entries()) {
            const offset = Layout.laneOffset(
              program.layout,
              Mir.semanticType(logicalType),
              lane.path,
            )
            const stored = values.at(ordinal)
            if (offset === undefined || stored === undefined) {
              throw new RangeError(`Backend lost address lane ${ordinal} for %${root}`)
            }
            yield* FunctionBody.store(
              body,
              stored,
              yield* FunctionBody.getElementPtr(
                body,
                i8,
                base,
                [yield* Constant.integerUnsigned(builder, i32, BigInt(offset))],
                `${name}_${ordinal}_ptr`,
              ),
            )
          }
        })
        for (const root of [...addressRoots].sort((left, right) => left - right)) {
          const logicalType = entry.fn.localTypes.at(root)
          if (logicalType === undefined) throw new RangeError(`Backend lost address root %${root}`)
          yield* storeAddressRootValues(
            root,
            Object.freeze(
              yield* Effect.forEach(valueLanesFor(logicalType), (lane) =>
                Constant.nullValue(builder, laneType(lane)),
              ),
            ),
            `addr${root}_zero`,
          )
        }
        let physicalParameter = 0
        for (let ordinal = 0; ordinal < entry.fn.parameterCount; ordinal += 1) {
          const logicalType = entry.fn.localTypes.at(ordinal)
          if (logicalType === undefined) {
            throw new RangeError(`Backend lost parameter type %${ordinal}`)
          }
          const values: Array<Value.Input> = []
          for (let lane = 0; lane < lanesFor(logicalType).length; lane += 1) {
            values.push(yield* Value.argument(body, physicalParameter))
            physicalParameter += 1
          }
          if (logicalType._tag === 'EffectBorrow') {
            const base = values.at(0)
            if (base === undefined) throw new RangeError(`Backend lost Effect borrow %${ordinal}`)
            const storage: Array<Value.Input> = []
            const loaded: Array<Value.Input> = []
            for (const [lane, callingLane] of valueLanesFor(logicalType).entries()) {
              const offset = Layout.laneOffset(program.layout, logicalType.type, callingLane.path)
              if (offset === undefined)
                throw new RangeError(`Backend lost Effect borrow lane ${lane}`)
              const pointer = yield* FunctionBody.getElementPtr(
                body,
                i8,
                base,
                [yield* Constant.integerUnsigned(builder, i32, BigInt(offset))],
                `borrow${ordinal}_${lane}_ptr`,
              )
              storage.push(pointer)
              loaded.push(
                yield* FunctionBody.load(
                  body,
                  laneType(callingLane),
                  pointer,
                  `borrow${ordinal}_${lane}`,
                ),
              )
            }
            mutableStorage.set(ordinal, Object.freeze(storage))
            locals.set(ordinal, Object.freeze(loaded))
            continue
          }
          locals.set(ordinal, Object.freeze(values))
          const storage = mutableStorage.get(ordinal)
          if (storage !== undefined) {
            for (const [lane, pointer] of storage.entries()) {
              const stored = values.at(lane)
              if (stored !== undefined) yield* FunctionBody.store(body, stored, pointer)
            }
          }
          if (addressRoots.has(ordinal)) {
            yield* storeAddressRootValues(ordinal, Object.freeze(values), `addr${ordinal}_param`)
          }
        }
        const transferPointer = entry.suspendable
          ? yield* Value.argument(body, physicalParameter)
          : undefined
        const resumeFrame = entry.suspendable
          ? yield* Value.argument(body, physicalParameter + 1)
          : undefined
        const resumePath = entry.suspendable
          ? yield* Value.argument(body, physicalParameter + 2)
          : undefined
        if (entry.suspendable) {
          const entryBlock = blocks.get(entry.fn.entry.ordinal)
          if (
            dispatchBlock === undefined ||
            resumeFrame === undefined ||
            resumePath === undefined ||
            entryBlock === undefined
          )
            throw new RangeError('LLVM suspension dispatch lost its entry state')
          if (coroutineFrame !== undefined) {
            if (
              invocationFrameStorage === undefined ||
              coroutineFramePush === undefined ||
              usizeType === undefined ||
              frameReuseBlock === undefined ||
              frameAllocateBlock === undefined ||
              frameAcquiredBlock === undefined ||
              frameTrapBlock === undefined ||
              framePushedBlock === undefined
            )
              throw new RangeError('LLVM coroutine-frame stack lost private storage support')
            const resumeAddress = yield* FunctionBody.cast(
              body,
              'ptrtoint',
              resumeFrame,
              usizeType,
              'suspend_resume_frame_address',
            )
            yield* FunctionBody.conditionalBranch(
              body,
              yield* FunctionBody.integerCompare(
                body,
                'ne',
                resumeAddress,
                yield* Constant.integerUnsigned(builder, usizeType, 0n),
                'suspend_has_reusable_frame',
              ),
              frameReuseBlock,
              frameAllocateBlock,
            )
            yield* LlvmBlock.setInsertionPoint(body, frameReuseBlock)
            yield* FunctionBody.store(body, resumeFrame, invocationFrameStorage)
            yield* FunctionBody.branch(body, frameAcquiredBlock)
            yield* LlvmBlock.setInsertionPoint(body, frameAllocateBlock)
            const allocatedFrame = yield* FunctionBody.callDirect(
              body,
              coroutineFramePush,
              [
                yield* Constant.integerUnsigned(
                  builder,
                  usizeType,
                  BigInt(Math.max(coroutineFrame.size, 1)),
                ),
                yield* Constant.integerUnsigned(
                  builder,
                  usizeType,
                  BigInt(Math.max(coroutineFrame.alignment, 1)),
                ),
              ],
              'suspend_invocation_frame',
            )
            if (allocatedFrame === undefined)
              throw new RangeError('LLVM private frame allocation returned no pointer')
            const exhausted = yield* FunctionBody.integerCompare(
              body,
              'eq',
              yield* FunctionBody.cast(
                body,
                'ptrtoint',
                allocatedFrame,
                usizeType,
                'suspend_invocation_frame_address',
              ),
              yield* Constant.integerUnsigned(builder, usizeType, 0n),
              'suspend_invocation_frame_exhausted',
            )
            yield* FunctionBody.conditionalBranch(body, exhausted, frameTrapBlock, framePushedBlock)
            yield* LlvmBlock.setInsertionPoint(body, frameTrapBlock)
            yield* Intrinsic.call(body, 'trap', [], [])
            yield* FunctionBody.unreachable(body)
            yield* LlvmBlock.setInsertionPoint(body, framePushedBlock)
            yield* FunctionBody.store(body, allocatedFrame, invocationFrameStorage)
            for (const [root, field] of stableAddressFields) {
              if (root >= entry.fn.parameterCount) continue
              const logicalType = entry.fn.localTypes.at(root)
              const values = locals.get(root)
              if (logicalType === undefined || values === undefined)
                throw new RangeError(`LLVM coroutine frame lost parameter root %${root}`)
              const base = yield* FunctionBody.getElementPtr(
                body,
                i8,
                allocatedFrame,
                [yield* Constant.integerUnsigned(builder, i32, BigInt(field.offset))],
                `suspend_parameter_root${root}`,
              )
              for (const [ordinal, lane] of valueLanesFor(logicalType).entries()) {
                const offset = Layout.laneOffset(
                  program.layout,
                  Mir.semanticType(logicalType),
                  lane.path,
                )
                const value = values.at(ordinal)
                if (offset === undefined || value === undefined)
                  throw new RangeError(`LLVM coroutine parameter root %${root} lost a lane`)
                yield* FunctionBody.store(
                  body,
                  value,
                  yield* FunctionBody.getElementPtr(
                    body,
                    i8,
                    base,
                    [yield* Constant.integerUnsigned(builder, i32, BigInt(offset))],
                    `suspend_parameter_root${root}_${ordinal}`,
                  ),
                )
              }
            }
            yield* FunctionBody.branch(body, frameAcquiredBlock)
            yield* LlvmBlock.setInsertionPoint(body, frameAcquiredBlock)
            const invocationFrame = yield* FunctionBody.load(
              body,
              pointer,
              invocationFrameStorage,
              'suspend_selected_invocation_frame',
            )
            for (const [root, field] of stableAddressFields) {
              addressStorage.set(
                root,
                yield* FunctionBody.getElementPtr(
                  body,
                  i8,
                  invocationFrame,
                  [yield* Constant.integerUnsigned(builder, i32, BigInt(field.offset))],
                  `suspend_stable_root${root}`,
                ),
              )
            }
          }
          const dispatch = yield* FunctionBody.switchTerminator(body, resumePath, entryBlock)
          for (const resume of resumeControls) {
            const target = resumeBlocks.get(suspensionPointKey(resume.region.point))
            if (target === undefined) throw new RangeError('LLVM suspension lost resume block')
            yield* FunctionBody.addSwitchCase(
              body,
              dispatch,
              yield* Constant.integerUnsigned(builder, i32, BigInt(resume.ordinal)),
              target,
            )
          }
          yield* FunctionBody.sealSwitch(body, dispatch)
        }
        const readLocal = (local: Mir.LocalId): ReadonlyArray<Value.Input> => {
          const found = locals.get(local.ordinal)
          if (found === undefined) {
            throw new RangeError(`Backend read undefined local %${local.ordinal}`)
          }
          return found
        }
        const readScalar = (local: Mir.LocalId): Value.Input => {
          const values = readLocal(local)
          const first = values.at(0)
          if (values.length !== 1 || first === undefined) {
            throw new RangeError(`Backend expected scalar local %${local.ordinal}`)
          }
          return first
        }
        const locate = Effect.fnUntraced(function* (
          span: SourceSpan.SourceSpan,
          instruction: FunctionBody.Instruction | undefined,
        ) {
          if (!debug || scope === undefined || instruction === undefined) return
          const position = positionOf(table, span.start)
          const location = yield* LlvmMetadata.location(
            builder,
            position.line,
            position.column,
            scope,
          )
          yield* FunctionBody.setDebugLocation(body, instruction, location)
        })

        const coerceLane = Effect.fnUntraced(function* (
          input: Value.Input,
          source: Layout.CallingLane,
          target: Layout.CallingLane,
          name: string,
        ) {
          const sourceIsAddress = typeof source.type !== 'string'
          const targetIsAddress = typeof target.type !== 'string'
          if (sourceIsAddress && targetIsAddress) return input
          const pointerBits = program.layout.target.pointerSize === 4 ? 32 : 64
          const scalarBits = (lane: Layout.CallingLane): number =>
            typeof lane.type !== 'string'
              ? pointerBits
              : (() => {
                  const scalar = Scalar.find(lane.type) ?? Scalar.defaultInteger
                  return scalar.category === 'Boolean' ? 32 : Scalar.bits(scalar, pointerBits)
                })()
          const sourceBits = scalarBits(source)
          const targetBits = scalarBits(target)
          const sourceScalar =
            typeof source.type === 'string' ? Scalar.find(source.type) : undefined
          const targetScalar =
            typeof target.type === 'string' ? Scalar.find(target.type) : undefined
          const sourceFloating = sourceScalar?.category === 'Floating'
          const targetFloating = targetScalar?.category === 'Floating'
          const sourceIntegerType = integerTypes.get(sourceBits) ?? i32
          const targetIntegerType = integerTypes.get(targetBits) ?? i32
          if (
            !sourceIsAddress &&
            !targetIsAddress &&
            sourceBits === targetBits &&
            sourceFloating === targetFloating
          )
            return input
          let bits = sourceIsAddress
            ? yield* FunctionBody.cast(body, 'ptrtoint', input, sourceIntegerType, `${name}_bits`)
            : sourceFloating
              ? yield* FunctionBody.cast(body, 'bitcast', input, sourceIntegerType, `${name}_bits`)
              : input
          if (sourceBits !== targetBits) {
            bits = yield* FunctionBody.cast(
              body,
              targetBits > sourceBits ? 'zext' : 'trunc',
              bits,
              targetIntegerType,
              `${name}_width`,
            )
          }
          if (targetIsAddress)
            return yield* FunctionBody.cast(body, 'inttoptr', bits, laneType(target), name)
          return targetFloating
            ? yield* FunctionBody.cast(body, 'bitcast', bits, laneType(target), name)
            : bits
        })

        const failurePayload = Effect.fnUntraced(function* (
          source: ReadonlyArray<Value.Input>,
          sourceType: DeclarationIndex.SemanticType,
          sourceTag: Value.Input | undefined,
          targetType: SilkType.Effect,
          mappings: ReadonlyArray<{ readonly source: number; readonly target: number }>,
          label: string,
        ) {
          const targetShape = Layout.callingShape(program.layout, targetType)
          if (targetShape?.tree._tag !== 'OutcomeShape')
            throw new RangeError('LLVM failure propagation lost its target calling shape')
          const payload: Array<Value.Input> = []
          for (const [targetOrdinal, targetLane] of targetShape.lanes.slice(1).entries()) {
            let selected: Value.Input = yield* Constant.nullValue(builder, laneType(targetLane))
            for (const [mappingOrdinal, mapping] of [...mappings].reverse().entries()) {
              const repacking = Layout.failurePayloadRepacking(
                program.layout,
                sourceType,
                mapping.source,
                targetType,
                mapping.target,
              )
              if (repacking === undefined)
                throw new RangeError('LLVM failure propagation has an invalid member mapping')
              const lane = repacking.lanes.find(
                (candidate) => candidate.targetOrdinal === targetOrdinal,
              )
              const sourceValue = lane === undefined ? undefined : source.at(lane.sourceOrdinal)
              let candidate: Value.Input = yield* Constant.nullValue(builder, laneType(targetLane))
              if (lane !== undefined && sourceValue !== undefined) {
                const member = yield* coerceLane(
                  sourceValue,
                  lane.source,
                  lane.member,
                  `${label}_${targetOrdinal}_${mappingOrdinal}_member`,
                )
                candidate = yield* coerceLane(
                  member,
                  lane.member,
                  lane.target,
                  `${label}_${targetOrdinal}_${mappingOrdinal}_carrier`,
                )
              }
              if (sourceTag === undefined) {
                selected = candidate
                continue
              }
              const matches = yield* FunctionBody.integerCompare(
                body,
                'eq',
                sourceTag,
                yield* Constant.integerSigned(builder, i32, BigInt(mapping.source)),
                `${label}_${targetOrdinal}_${mappingOrdinal}_matches`,
              )
              selected = yield* FunctionBody.select(
                body,
                matches,
                candidate,
                selected,
                `${label}_${targetOrdinal}_${mappingOrdinal}_select`,
              )
            }
            payload.push(selected)
          }
          return Object.freeze(payload)
        })

        const returnStep = Effect.fnUntraced(function* (
          status: bigint,
          values: ReadonlyArray<Value.Input>,
          tag: string,
        ) {
          if (!entry.suspendable)
            throw new RangeError('LLVM synchronous function cannot return a suspension step')
          if (status === 0n && invocationFrameStorage !== undefined) {
            if (coroutineFramePop === undefined)
              throw new RangeError('LLVM suspension step lost private frame release')
            yield* FunctionBody.callDirect(
              body,
              coroutineFramePop,
              [
                yield* FunctionBody.load(
                  body,
                  pointer,
                  invocationFrameStorage,
                  `${tag}_invocation_frame`,
                ),
              ],
              `${tag}_release_invocation_frame`,
            )
          }
          const padded = [...values]
          const resultLanes = lanesFor(entry.fn.result)
          while (padded.length < resultLanes.length) {
            const lane = resultLanes.at(padded.length)
            if (lane === undefined) break
            padded.push(yield* Constant.nullValue(builder, laneType(lane)))
          }
          yield* FunctionBody.returnValue(
            body,
            yield* FunctionBody.buildAggregate(
              body,
              entry.emittedResultType,
              Object.freeze([
                yield* Constant.integerUnsigned(builder, i32, status),
                ...padded.slice(0, resultLanes.length),
              ]),
              tag,
            ),
          )
        })
        const callSynchronousValues = Effect.fnUntraced(function* (
          target: (typeof declared)[number],
          arguments_: ReadonlyArray<Value.Input>,
          name: string,
        ) {
          if (target.suspendable)
            throw new RangeError('LLVM synchronous helper selected a suspendable target')
          const result = yield* FunctionBody.callDirect(body, target.handle, arguments_, name)
          for (const root of [...addressRoots].sort((left, right) => left - right)) {
            yield* reloadAddressRoot(root)
          }
          if (target.resultLaneCount === 0) return Object.freeze([])
          if (result === undefined) throw new RangeError('Backend call produced no value')
          if (target.resultLaneCount === 1) return Object.freeze([result])
          const values: Array<Value.Input> = []
          for (let lane = 0; lane < target.resultLaneCount; lane += 1) {
            values.push(yield* FunctionBody.extractValue(body, result, [lane], `${name}_${lane}`))
          }
          return Object.freeze(values)
        })
        const callValues = Effect.fnUntraced(function* (
          target: (typeof declared)[number],
          arguments_: ReadonlyArray<Value.Input>,
          name: string,
          suspension?: Mir.RunSuspendableEffectRegion,
        ) {
          if (!target.suspendable) return yield* callSynchronousValues(target, arguments_, name)
          if (transferPointer === undefined || suspension === undefined)
            throw new RangeError(
              `LLVM suspension-aware call from ${entry.fn.id.module}.${entry.fn.id.name} to ${target.fn.id.module}.${target.fn.id.name} lost transfer control`,
            )
          const nullPointer = yield* Constant.nullValue(builder, pointer)
          const result = yield* FunctionBody.callDirect(
            body,
            target.handle,
            [
              ...arguments_,
              transferPointer,
              nullPointer,
              yield* Constant.integerUnsigned(builder, i32, 0n),
            ],
            name,
          )
          if (result === undefined) throw new RangeError('LLVM suspension step produced no value')
          const status = yield* FunctionBody.extractValue(body, result, [0], `${name}_status`)
          const completed = yield* LlvmBlock.make(body, `${name}_complete`)
          const transferred = yield* LlvmBlock.make(body, `${name}_transfer`)
          yield* FunctionBody.conditionalBranch(
            body,
            yield* FunctionBody.integerCompare(
              body,
              'eq',
              status,
              yield* Constant.integerUnsigned(builder, i32, 0n),
              `${name}_is_complete`,
            ),
            completed,
            transferred,
          )
          yield* LlvmBlock.setInsertionPoint(body, transferred)
          const continuation = suspension.relay.state
          if (continuation !== undefined) {
            const generated = resumeThunks.get(suspensionPointKey(suspension.point))
            if (generated === undefined)
              throw new RangeError('LLVM coroutine relay lost its native frame plan')
            if (invocationFrameStorage === undefined)
              throw new RangeError('LLVM coroutine relay lost its invocation frame')
            const frame = yield* FunctionBody.load(
              body,
              pointer,
              invocationFrameStorage,
              `${name}_invocation_frame`,
            )
            const appendPointerPointer = yield* FunctionBody.getElementPtr(
              body,
              i8,
              transferPointer,
              [
                yield* Constant.integerUnsigned(
                  builder,
                  i32,
                  BigInt(program.layout.target.pointerSize * 2),
                ),
              ],
              `${name}_append_ptr_ptr`,
            )
            const appendPointer = yield* FunctionBody.load(
              body,
              pointer,
              appendPointerPointer,
              `${name}_append_ptr`,
            )
            const next = yield* FunctionBody.load(body, pointer, appendPointer, `${name}_next`)
            yield* FunctionBody.store(
              body,
              next,
              yield* NativeLanePointer.lanePointer(
                lanePointers,
                body,
                frame,
                0,
                `${name}_store_parent`,
              ),
            )
            yield* FunctionBody.store(
              body,
              yield* Constant.fromGlobal(
                builder,
                yield* FunctionActor.global(builder, generated.handle),
              ),
              yield* NativeLanePointer.lanePointer(
                lanePointers,
                body,
                frame,
                program.layout.target.pointerSize,
                `${name}_store_resume`,
              ),
            )
            for (const field of generated.layout.payload) {
              const values = readLocal(field.local)
              const type = entry.fn.localTypes.at(field.local.ordinal)
              if (type === undefined) throw new RangeError('LLVM frame payload lost its type')
              const packed = NativeType.packLanes(
                program.layout.target,
                lanesFor(type),
                field.offset,
              )
              for (const [ordinal, lane] of packed.entries.entries()) {
                const value = values.at(ordinal)
                if (value === undefined) throw new RangeError('LLVM frame payload lost a lane')
                yield* FunctionBody.store(
                  body,
                  value,
                  yield* NativeLanePointer.lanePointer(
                    lanePointers,
                    body,
                    frame,
                    lane.offset,
                    `${name}_payload${field.slot}_${ordinal}`,
                  ),
                )
              }
            }
            yield* FunctionBody.store(body, frame, appendPointer)
            yield* FunctionBody.store(
              body,
              yield* NativeLanePointer.lanePointer(
                lanePointers,
                body,
                frame,
                0,
                `${name}_next_append_ptr`,
              ),
              appendPointerPointer,
            )
          }
          yield* returnStep(1n, Object.freeze([]), `${name}_relayed`)
          yield* LlvmBlock.setInsertionPoint(body, completed)
          for (const root of [...addressRoots].sort((left, right) => left - right)) {
            yield* reloadAddressRoot(root)
          }
          const values: Array<Value.Input> = []
          for (let lane = 0; lane < target.resultLaneCount; lane += 1) {
            values.push(
              yield* FunctionBody.extractValue(body, result, [lane + 1], `${name}_${lane}`),
            )
          }
          return Object.freeze(values)
        })

        const originateTransfer = Effect.fnUntraced(function* (
          region: Mir.SuspendEffectRegion,
          arguments_: ReadonlyArray<Value.Input>,
          name: string,
        ) {
          if (transferPointer === undefined)
            throw new RangeError('LLVM suspension origin lost transfer storage')
          const generated = originThunks.get(suspensionPointKey(region.point))
          if (generated === undefined) throw new RangeError('LLVM suspension origin lost thunk')
          yield* FunctionBody.store(
            body,
            yield* Constant.fromGlobal(
              builder,
              yield* FunctionActor.global(builder, generated.handle),
            ),
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              transferPointer,
              0,
              `${name}_child`,
            ),
          )
          yield* FunctionBody.store(
            body,
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              transferPointer,
              program.layout.target.pointerSize,
              `${name}_head`,
            ),
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              transferPointer,
              program.layout.target.pointerSize * 2,
              `${name}_append_ptr`,
            ),
          )
          const packed = NativeType.packLanes(
            program.layout.target,
            NativeSuspension.logicalLanes(
              entry.fn,
              NativeCall.operationInputs(region.operation),
              lanesFor,
            ),
          )
          if (packed.entries.length !== arguments_.length)
            throw new RangeError('LLVM suspension origin argument shape disagrees with its thunk')
          for (const [ordinal, lane] of packed.entries.entries()) {
            const value = arguments_.at(ordinal)
            if (value === undefined) throw new RangeError('LLVM suspension origin lost argument')
            yield* FunctionBody.store(
              body,
              value,
              yield* NativeLanePointer.lanePointer(
                lanePointers,
                body,
                transferPointer,
                transferHeaderSize + lane.offset,
                `${name}_argument${ordinal}`,
              ),
            )
          }
          yield* returnStep(1n, Object.freeze([]), `${name}_originated`)
        })

        const emitOrigin = Effect.fnUntraced(function* (
          operation: Extract<
            Mir.Operation,
            { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' }
          >,
          arguments_: ReadonlyArray<Value.Input>,
          name: string,
        ) {
          const suspension = suspensionRegions.get(operation)
          if (suspension?._tag !== 'SuspendEffectRegion') return false
          yield* originateTransfer(suspension, arguments_, name)
          yield* LlvmBlock.setInsertionPoint(
            body,
            yield* LlvmBlock.make(body, `${name}_unreachable_continuation`),
          )
          const outcomeValues = Object.freeze(
            yield* Effect.forEach(lanesFor(operation.outcomeType), (lane) =>
              Constant.nullValue(builder, laneType(lane)),
            ),
          )
          const destinationValues = Object.freeze(
            yield* Effect.forEach(lanesFor(operation.type), (lane) =>
              Constant.nullValue(builder, laneType(lane)),
            ),
          )
          locals.set(operation.outcome.ordinal, outcomeValues)
          locals.set(operation.destination.ordinal, destinationValues)
          return true
        })

        const joinSuspensionOutcome = Effect.fnUntraced(function* (
          operation: Extract<
            Mir.Operation,
            { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' }
          >,
          completedValues: ReadonlyArray<Value.Input>,
          name: string,
        ) {
          const suspension = suspensionRegions.get(operation)
          const descriptor =
            suspension?._tag === 'RunSuspendableEffectRegion' ? suspension.relay.state : undefined
          if (descriptor === undefined) return completedValues
          if (transferPointer === undefined || resumeFrame === undefined)
            throw new RangeError('LLVM coroutine resume lost private arguments')
          const generated = resumeThunks.get(suspensionPointKey(descriptor.point))
          const resumeBlock = resumeBlocks.get(suspensionPointKey(descriptor.point))
          if (generated === undefined || resumeBlock === undefined)
            throw new RangeError('LLVM coroutine resume lost generated control')
          yield* storeMutable(operation.outcome, completedValues)
          const following = yield* LlvmBlock.make(body, `${name}_joined`)
          yield* FunctionBody.branch(body, following)
          yield* LlvmBlock.setInsertionPoint(body, resumeBlock)
          for (const field of generated.layout.payload) {
            const type = entry.fn.localTypes.at(field.local.ordinal)
            const storage = mutableStorage.get(field.local.ordinal)
            if (type === undefined || storage === undefined)
              throw new RangeError('LLVM coroutine payload has no mutable restore storage')
            const packed = NativeType.packLanes(program.layout.target, lanesFor(type), field.offset)
            for (const [ordinal, lane] of packed.entries.entries()) {
              const target = storage.at(ordinal)
              if (target === undefined) throw new RangeError('LLVM restore lost payload lane')
              yield* FunctionBody.store(
                body,
                yield* FunctionBody.load(
                  body,
                  laneType(lane.lane),
                  yield* NativeLanePointer.lanePointer(
                    lanePointers,
                    body,
                    resumeFrame,
                    lane.offset,
                    `${name}_restore${field.slot}_${ordinal}`,
                  ),
                  `${name}_restored${field.slot}_${ordinal}`,
                ),
                target,
              )
            }
          }
          const outcomeStorage = mutableStorage.get(operation.outcome.ordinal)
          if (outcomeStorage === undefined)
            throw new RangeError('LLVM coroutine outcome has no restore storage')
          const outcomePacked = NativeType.packLanes(
            program.layout.target,
            lanesFor(operation.outcomeType),
            transferResultOffset,
          )
          for (const [ordinal, lane] of outcomePacked.entries.entries()) {
            const target = outcomeStorage.at(ordinal)
            if (target === undefined) throw new RangeError('LLVM resume lost outcome lane')
            yield* FunctionBody.store(
              body,
              yield* FunctionBody.load(
                body,
                laneType(lane.lane),
                yield* FunctionBody.getElementPtr(
                  body,
                  i8,
                  transferPointer,
                  [yield* Constant.integerUnsigned(builder, i32, BigInt(lane.offset))],
                  `${name}_resume_outcome${ordinal}_ptr`,
                ),
                `${name}_resume_outcome${ordinal}`,
              ),
              target,
            )
          }
          yield* FunctionBody.branch(body, following)
          yield* LlvmBlock.setInsertionPoint(body, following)
          for (const field of generated.layout.payload) {
            const type = entry.fn.localTypes.at(field.local.ordinal)
            const storage = mutableStorage.get(field.local.ordinal)
            if (type === undefined || storage === undefined)
              throw new RangeError('LLVM joined continuation lost payload storage')
            const values: Array<Value.Input> = []
            for (const [ordinal, lane] of lanesFor(type).entries()) {
              const source = storage.at(ordinal)
              if (source === undefined) throw new RangeError('LLVM joined payload lost lane')
              values.push(
                yield* FunctionBody.load(
                  body,
                  laneType(lane),
                  source,
                  `${name}_joined_payload${field.slot}_${ordinal}`,
                ),
              )
            }
            locals.set(field.local.ordinal, Object.freeze(values))
          }
          const joined: Array<Value.Input> = []
          for (const [ordinal, lane] of lanesFor(operation.outcomeType).entries()) {
            const source = outcomeStorage.at(ordinal)
            if (source === undefined) throw new RangeError('LLVM joined outcome lost storage')
            joined.push(
              yield* FunctionBody.load(body, laneType(lane), source, `${name}_joined${ordinal}`),
            )
          }
          return Object.freeze(joined)
        })

        const emitCallableBinary = Effect.fnUntraced(function* (
          operator: Mir.BinaryOperator,
          left: Value.Input,
          right: Value.Input,
          operandMirType: Mir.Type,
          span: SourceSpan.SourceSpan,
          nameOrdinal: number,
        ) {
          const leftLane = valueLanesFor(operandMirType).at(0)
          if (leftLane === undefined)
            throw new RangeError('LLVM callable binary operation lost its operand type')
          const semanticOperand = Mir.semanticType(operandMirType)
          const scalar =
            typeof semanticOperand === 'string' ? Scalar.find(semanticOperand) : undefined
          const unsigned = scalar?.signedness === 'Unsigned'
          const operandType = laneType(leftLane)
          if (scalar?.category === 'Floating') {
            const predicate: FunctionBody.FloatingPredicate | undefined =
              operator === 'Equals'
                ? 'oeq'
                : operator === 'NotEquals'
                  ? 'une'
                  : operator === 'LessThan'
                    ? 'olt'
                    : operator === 'LessOrEqual'
                      ? 'ole'
                      : operator === 'GreaterThan'
                        ? 'ogt'
                        : operator === 'GreaterOrEqual'
                          ? 'oge'
                          : undefined
            if (predicate !== undefined) {
              const flag = yield* FunctionBody.floatingCompare(
                body,
                predicate,
                left,
                right,
                `callable_fcmp${nameOrdinal}_flag`,
              )
              return yield* FunctionBody.cast(
                body,
                'zext',
                flag,
                i32,
                `callable_fcmp${nameOrdinal}`,
              )
            }
            const mnemonic: FunctionBody.FloatingBinaryKind | undefined =
              operator === 'Add'
                ? 'fadd'
                : operator === 'Subtract'
                  ? 'fsub'
                  : operator === 'Multiply'
                    ? 'fmul'
                    : operator === 'Divide'
                      ? 'fdiv'
                      : operator === 'Remainder'
                        ? 'frem'
                        : undefined
            if (mnemonic === undefined)
              throw new RangeError(`LLVM callable float ${operator} is unavailable`)
            const result = yield* FunctionBody.binary(
              body,
              mnemonic,
              left,
              right,
              `callable_float${nameOrdinal}`,
            )
            yield* locate(span, yield* Value.instruction(body, result))
            return result
          }
          const predicate = NativeArith.comparisonPredicate(operator, unsigned)
          if (predicate !== undefined) {
            const flag = yield* FunctionBody.integerCompare(
              body,
              predicate,
              left,
              right,
              `callable_cmp${nameOrdinal}_flag`,
            )
            const widened = yield* FunctionBody.cast(
              body,
              'zext',
              flag,
              i32,
              `callable_cmp${nameOrdinal}`,
            )
            yield* locate(span, yield* Value.instruction(body, flag))
            return widened
          }
          if (
            operator === 'BitAnd' ||
            operator === 'BitOr' ||
            operator === 'BitXor' ||
            operator === 'WrappingAdd' ||
            operator === 'WrappingSubtract' ||
            operator === 'WrappingMultiply'
          ) {
            const result = yield* FunctionBody.binary(
              body,
              operator === 'BitAnd'
                ? 'and'
                : operator === 'BitOr'
                  ? 'or'
                  : operator === 'BitXor'
                    ? 'xor'
                    : operator === 'WrappingAdd'
                      ? 'add'
                      : operator === 'WrappingSubtract'
                        ? 'sub'
                        : 'mul',
              left,
              right,
              `callable_integer${nameOrdinal}`,
            )
            yield* locate(span, yield* Value.instruction(body, result))
            return result
          }
          if (operator === 'ShiftLeft' || operator === 'ShiftRight') {
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
              `callable_shift${nameOrdinal}_invalid`,
            )
            const continueBlock = yield* LlvmBlock.make(body, `callable_shift${nameOrdinal}_ok`)
            yield* FunctionBody.conditionalBranch(body, invalid, trapBlock, continueBlock)
            yield* LlvmBlock.setInsertionPoint(body, continueBlock)
            const result = yield* FunctionBody.binary(
              body,
              operator === 'ShiftLeft' ? 'shl' : unsigned ? 'lshr' : 'ashr',
              left,
              right,
              `callable_shift${nameOrdinal}`,
            )
            yield* locate(span, yield* Value.instruction(body, result))
            return result
          }
          if (operator === 'RotateLeft' || operator === 'RotateRight') {
            const signature = Object.freeze({
              returnType: operandType,
              parameters: Object.freeze([operandType, operandType, operandType]),
            })
            const result = yield* Intrinsic.call(
              body,
              operator === 'RotateLeft' ? 'fshl' : 'fshr',
              [operandType],
              [left, left, right],
              `callable_rotate${nameOrdinal}`,
              { signature },
            )
            if (result === undefined) throw new RangeError('LLVM callable rotate produced no value')
            yield* locate(span, yield* Value.instruction(body, result))
            return result
          }
          if (operator === 'SaturatingAdd' || operator === 'SaturatingSubtract') {
            const signature = Object.freeze({
              returnType: operandType,
              parameters: Object.freeze([operandType, operandType]),
            })
            const intrinsic =
              operator === 'SaturatingAdd'
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
              `callable_saturating${nameOrdinal}`,
              { signature },
            )
            if (result === undefined)
              throw new RangeError('LLVM callable saturating arithmetic produced no value')
            yield* locate(span, yield* Value.instruction(body, result))
            return result
          }
          if (operator === 'SaturatingMultiply') {
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
              `callable_saturating${nameOrdinal}_pair`,
              { signature },
            )
            if (pair === undefined)
              throw new RangeError('LLVM callable saturating multiply produced no value')
            const wrapped = yield* FunctionBody.extractValue(
              body,
              pair,
              [0],
              `callable_saturating${nameOrdinal}_wrapped`,
            )
            const overflowed = yield* FunctionBody.extractValue(
              body,
              pair,
              [1],
              `callable_saturating${nameOrdinal}_overflow`,
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
                `callable_saturating${nameOrdinal}_signs`,
              )
              const negative = yield* FunctionBody.integerCompare(
                body,
                'slt',
                signs,
                zero,
                `callable_saturating${nameOrdinal}_negative`,
              )
              boundary = yield* FunctionBody.select(
                body,
                negative,
                minimum,
                maximum,
                `callable_saturating${nameOrdinal}_boundary`,
              )
            }
            const result = yield* FunctionBody.select(
              body,
              overflowed,
              boundary,
              wrapped,
              `callable_saturating${nameOrdinal}`,
            )
            yield* locate(span, yield* Value.instruction(body, result))
            return result
          }
          if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'arith_trap')
          let result: Value.Value
          if (operator === 'Add' || operator === 'Subtract' || operator === 'Multiply') {
            const intrinsicId =
              operator === 'Add'
                ? unsigned
                  ? ('uadd.with.overflow' as const)
                  : ('sadd.with.overflow' as const)
                : operator === 'Subtract'
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
              `callable_arith${nameOrdinal}_pair`,
              { signature: overflowSignature },
            )
            if (pair === undefined)
              throw new RangeError('Backend callable overflow intrinsic produced no value')
            result = yield* FunctionBody.extractValue(
              body,
              pair,
              [0],
              `callable_arith${nameOrdinal}`,
            )
            const overflowed = yield* FunctionBody.extractValue(
              body,
              pair,
              [1],
              `callable_arith${nameOrdinal}_flag`,
            )
            const continueBlock = yield* LlvmBlock.make(body, `callable_arith${nameOrdinal}_ok`)
            yield* FunctionBody.conditionalBranch(body, overflowed, trapBlock, continueBlock)
            yield* LlvmBlock.setInsertionPoint(body, continueBlock)
          } else {
            const zero = yield* Constant.integerUnsigned(builder, operandType, 0n)
            const zeroDivisor = yield* FunctionBody.integerCompare(
              body,
              'eq',
              right,
              zero,
              `callable_div${nameOrdinal}_zero`,
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
                `callable_div${nameOrdinal}_min`,
              )
              const negativeOneDivisor = yield* FunctionBody.integerCompare(
                body,
                'eq',
                right,
                negativeOne,
                `callable_div${nameOrdinal}_negone`,
              )
              const overflowCase = yield* FunctionBody.binary(
                body,
                'and',
                minimumDividend,
                negativeOneDivisor,
                `callable_div${nameOrdinal}_overflow`,
              )
              trapping = yield* FunctionBody.binary(
                body,
                'or',
                zeroDivisor,
                overflowCase,
                `callable_div${nameOrdinal}_trapping`,
              )
            }
            const continueBlock = yield* LlvmBlock.make(body, `callable_div${nameOrdinal}_ok`)
            yield* FunctionBody.conditionalBranch(body, trapping, trapBlock, continueBlock)
            yield* LlvmBlock.setInsertionPoint(body, continueBlock)
            result = yield* FunctionBody.binary(
              body,
              operator === 'Divide' ? (unsigned ? 'udiv' : 'sdiv') : unsigned ? 'urem' : 'srem',
              left,
              right,
              `callable_arith${nameOrdinal}`,
            )
          }
          yield* locate(span, yield* Value.instruction(body, result))
          return result
        })

        const emitIntegerConversion = Effect.fnUntraced(function* (
          input: Value.Input,
          sourceType: Mir.ScalarType,
          targetType: Mir.ScalarType,
          name: string,
        ) {
          const source = Scalar.find(sourceType._tag)
          const target = Scalar.find(targetType._tag)
          if (
            source === undefined ||
            source.category !== 'Integer' ||
            target === undefined ||
            target.category !== 'Integer'
          )
            throw new RangeError('LLVM integer conversion lost its scalar types')
          const pointerBits = program.layout.target.pointerSize === 4 ? 32 : 64
          const sourceBits = Scalar.bits(source, pointerBits)
          const targetBits = Scalar.bits(target, pointerBits)
          const sourceRange = Scalar.range(source, pointerBits)
          const targetRange = Scalar.range(target, pointerBits)
          const physicalSource = integerTypes.get(sourceBits) ?? i32
          const physicalTarget = integerTypes.get(targetBits) ?? i32
          const checks: Array<Value.Input> = []
          if (targetRange.minimum > sourceRange.minimum) {
            checks.push(
              yield* FunctionBody.integerCompare(
                body,
                source.signedness === 'Signed' ? 'slt' : 'ult',
                input,
                source.signedness === 'Signed'
                  ? yield* Constant.integerSigned(builder, physicalSource, targetRange.minimum)
                  : yield* Constant.integerUnsigned(builder, physicalSource, targetRange.minimum),
                `${name}_below`,
              ),
            )
          }
          if (targetRange.maximum < sourceRange.maximum) {
            checks.push(
              yield* FunctionBody.integerCompare(
                body,
                source.signedness === 'Signed' ? 'sgt' : 'ugt',
                input,
                source.signedness === 'Signed'
                  ? yield* Constant.integerSigned(builder, physicalSource, targetRange.maximum)
                  : yield* Constant.integerUnsigned(builder, physicalSource, targetRange.maximum),
                `${name}_above`,
              ),
            )
          }
          let invalid = checks.at(0)
          for (const [ordinal, check] of checks.slice(1).entries())
            invalid = yield* FunctionBody.binary(
              body,
              'or',
              invalid ?? check,
              check,
              `${name}_invalid${ordinal}`,
            )
          if (invalid !== undefined) {
            if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'arith_trap')
            const following = yield* LlvmBlock.make(body, `${name}_ok`)
            yield* FunctionBody.conditionalBranch(body, invalid, trapBlock, following)
            yield* LlvmBlock.setInsertionPoint(body, following)
          }
          if (sourceBits === targetBits) return input
          return yield* FunctionBody.cast(
            body,
            sourceBits < targetBits ? (source.signedness === 'Signed' ? 'sext' : 'zext') : 'trunc',
            input,
            physicalTarget,
            name,
          )
        })

        const storeMutable = Effect.fnUntraced(function* (
          root: Mir.LocalId,
          values: ReadonlyArray<Value.Input>,
        ) {
          const storage = mutableStorage.get(root.ordinal)
          if (storage === undefined) return
          for (const [lane, pointer] of storage.entries()) {
            const stored = values.at(lane)
            if (stored === undefined) throw new RangeError('Mutable root lost a physical lane')
            yield* FunctionBody.store(body, stored, pointer)
          }
        })

        const constantBytePointer = Effect.fnUntraced(function* (
          base: Value.Input,
          offset: number,
          name: string,
        ) {
          return yield* NativeLanePointer.lanePointer(lanePointers, body, base, offset, name)
        })
        const aggregateFieldOffset = (type: SilkType.Type, name: string): number => {
          const planned = Layout.entry(program.layout, type)
          if (planned?.representation._tag !== 'Aggregate') {
            throw new RangeError(`LLVM raw storage lost aggregate ${SilkType.encode(type)}`)
          }
          const field = planned.representation.fields.find((candidate) => candidate.name === name)
          if (field === undefined) throw new RangeError(`LLVM raw storage lost field ${name}`)
          return field.offset
        }
        const emitHostFailure = Effect.fnUntraced(function* (
          operation: Extract<Mir.Operation, { readonly _tag: 'Allocate' | 'HostWrite' }>,
        ) {
          const lanes = lanesFor(operation.propagationType)
          const values: Array<Value.Input> = []
          for (const [ordinal, lane] of lanes.entries()) {
            values.push(
              yield* Constant.integerUnsigned(
                builder,
                laneType(lane),
                ordinal === 0 ? BigInt(operation.failureTag) : 0n,
              ),
            )
          }
          if (entry.suspendable) {
            yield* returnStep(
              0n,
              Object.freeze(values),
              `host_failure${operation.destination.ordinal}`,
            )
            return
          }
          if (values.length === 0) {
            yield* FunctionBody.returnVoid(body)
            return
          }
          const single = values.at(0)
          if (values.length === 1 && single !== undefined) {
            yield* FunctionBody.returnValue(body, single)
            return
          }
          yield* FunctionBody.returnValue(
            body,
            yield* FunctionBody.buildAggregate(
              body,
              entry.resultType,
              Object.freeze(values),
              `host_failure${operation.destination.ordinal}`,
            ),
          )
        })
        let materializeSequence = 0
        const materializeAddressRoot = Effect.fnUntraced(function* (root: Mir.LocalId) {
          const materializeId = materializeSequence++
          const base = addressStorage.get(root.ordinal)
          const logicalType = entry.fn.localTypes.at(root.ordinal)
          if (base === undefined || logicalType === undefined) {
            throw new RangeError(`Backend lost address storage for %${root.ordinal}`)
          }
          yield* storeAddressRootValues(
            root.ordinal,
            readLocal(root),
            `addr${root.ordinal}_${materializeId}`,
          )
        })
        const ensureAddressRoot = Effect.fnUntraced(function* (root: Mir.LocalId) {
          if (!addressStorage.has(root.ordinal)) {
            const logicalType = entry.fn.localTypes.at(root.ordinal)
            const layout =
              logicalType === undefined
                ? undefined
                : Layout.entry(program.layout, Mir.semanticType(logicalType))
            if (logicalType === undefined || layout === undefined)
              throw new RangeError(`Backend cannot materialize callable capture %${root.ordinal}`)
            addressStorage.set(
              root.ordinal,
              yield* FunctionBody.alloca(body, i8, `callable_addr${root.ordinal}`, {
                count: yield* Constant.integerUnsigned(builder, i32, BigInt(layout.size)),
                alignment: yield* Alignment.fromByteUnits(layout.alignment),
              }),
            )
          }
          yield* materializeAddressRoot(root)
        })
        let reloadSequence = 0
        const reloadAddressRoot = Effect.fnUntraced(function* (root: number) {
          const reloadId = reloadSequence++
          const base = addressStorage.get(root)
          const logicalType = entry.fn.localTypes.at(root)
          if (base === undefined || logicalType === undefined) {
            throw new RangeError(`Backend lost address storage for %${root}`)
          }
          const values: Array<Value.Input> = []
          for (const [ordinal, lane] of valueLanesFor(logicalType).entries()) {
            const offset = Layout.laneOffset(
              program.layout,
              Mir.semanticType(logicalType),
              lane.path,
            )
            if (offset === undefined) throw new RangeError(`Backend lost address lane ${ordinal}`)
            values.push(
              yield* FunctionBody.load(
                body,
                laneType(lane),
                yield* constantBytePointer(
                  base,
                  offset,
                  `reload${root}_${ordinal}_${reloadId}_ptr`,
                ),
                `reload${root}_${ordinal}_${reloadId}`,
              ),
            )
          }
          const frozen = Object.freeze(values)
          locals.set(root, frozen)
          yield* storeMutable(Object.freeze({ _tag: 'Local', ordinal: root }), frozen)
        })

        const semanticLanesOf = (type: SilkType.Type): ReadonlyArray<Layout.CallingLane> => {
          const shape = Layout.callingShape(program.layout, type)
          if (shape === undefined)
            throw new RangeError(`LLVM cleanup lost calling shape for ${SilkType.encode(type)}`)
          return shape.lanes
        }

        /**
         * Releases one owned value's lanes through its complete cleanup plan: hooks run
         * against a stack materialization before their inner cleanup sees the (possibly
         * mutated) lanes, struct fields release in declaration order, and every ticket-backed
         * lane calls the release shim exactly once.
         */
        const dropThroughPlan: (
          plan: CleanupPlan.CleanupPlan,
          values: ReadonlyArray<Value.Input>,
          tag: string,
        ) => Effect.Effect<void, LlvmError.LlvmError> = Effect.fnUntraced(function* (
          plan: CleanupPlan.CleanupPlan,
          values: ReadonlyArray<Value.Input>,
          tag: string,
        ) {
          switch (plan._tag) {
            case 'NoCleanup':
            case 'ParameterCleanup':
              return
            case 'CallableCleanup': {
              if (plan.environment._tag !== 'CallableEnvironmentIdentity')
                throw new RangeError('LLVM callable cleanup lost its specialized environment')
              for (const slot of plan.slots) {
                const range = Layout.callableCaptureRange(
                  program.layout,
                  plan.environment.identity,
                  slot.ordinal,
                )
                if (range === undefined)
                  throw new RangeError('LLVM callable cleanup lost an owned capture lane')
                yield* dropThroughPlan(
                  slot.cleanup,
                  Object.freeze(values.slice(range.laneOffset, range.laneOffset + range.laneCount)),
                  `${tag}_callable${slot.ordinal}`,
                )
              }
              return
            }
            case 'EffectCleanup':
              for (const slot of plan.slots) {
                if (!CleanupPlan.hasEffect(slot.cleanup)) continue
                yield* dropThroughPlan(
                  slot.cleanup,
                  Object.freeze(values.slice(slot.laneOffset, slot.laneOffset + slot.laneCount)),
                  `${tag}_effect${slot.ordinal}`,
                )
              }
              return
            case 'EffectCompositeCleanup': {
              const choice = values.at(0)
              if (choice === undefined)
                throw new RangeError('LLVM Effect composite cleanup lost its tag')
              const following = yield* LlvmBlock.make(body, `${tag}_effect_composite_following`)
              for (const [ordinal, alternative] of plan.alternatives.entries()) {
                const selected = yield* LlvmBlock.make(body, `${tag}_effect_composite_${ordinal}`)
                const otherwise = yield* LlvmBlock.make(
                  body,
                  `${tag}_effect_composite_${ordinal}_otherwise`,
                )
                yield* FunctionBody.conditionalBranch(
                  body,
                  yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    choice,
                    yield* Constant.integerSigned(builder, i32, BigInt(ordinal)),
                    `${tag}_effect_composite_is_${ordinal}`,
                  ),
                  selected,
                  otherwise,
                )
                yield* LlvmBlock.setInsertionPoint(body, selected)
                yield* dropThroughPlan(
                  alternative,
                  Object.freeze(values.slice(1)),
                  `${tag}_${ordinal}`,
                )
                yield* FunctionBody.branch(body, following)
                yield* LlvmBlock.setInsertionPoint(body, otherwise)
              }
              yield* FunctionBody.branch(body, following)
              yield* LlvmBlock.setInsertionPoint(body, following)
              return
            }
            case 'AllocationCleanup':
            case 'RawBufferCleanup': {
              const context = values.at(4)
              if (context === undefined || free === undefined)
                throw new RangeError('LLVM allocation cleanup lost its reclaim context')
              yield* FunctionBody.callDirect(body, free, [
                yield* FunctionBody.cast(body, 'inttoptr', context, pointer, `${tag}_context`),
              ])
              return
            }
            case 'HookCleanup': {
              const target = declared.find((candidate) =>
                Mir.matchesInstance(candidate.fn, plan.hook, plan.typeArguments),
              )
              if (target === undefined)
                throw new RangeError('LLVM cleanup cannot resolve its Drop hook instance')
              const layoutEntry = Layout.entry(program.layout, plan.type)
              if (layoutEntry === undefined)
                throw new RangeError(
                  `LLVM hook cleanup lost the layout for ${SilkType.encode(plan.type)}`,
                )
              const base = yield* FunctionBody.alloca(body, i8, `${tag}_hook_storage`, {
                count: yield* Constant.integerUnsigned(builder, i32, BigInt(layoutEntry.size)),
                alignment: yield* Alignment.fromByteUnits(layoutEntry.alignment),
              })
              const lanes = semanticLanesOf(plan.type)
              for (const [ordinal, lane] of lanes.entries()) {
                const offset = Layout.laneOffset(program.layout, plan.type, lane.path)
                const stored = values.at(ordinal)
                if (offset === undefined || stored === undefined)
                  throw new RangeError('LLVM hook cleanup lost a lane')
                yield* FunctionBody.store(
                  body,
                  stored,
                  yield* constantBytePointer(base, offset, `${tag}_store${ordinal}`),
                )
              }
              yield* callValues(target, [base], `${tag}_hook`)
              const reloaded: Array<Value.Input> = []
              for (const [ordinal, lane] of lanes.entries()) {
                const offset = Layout.laneOffset(program.layout, plan.type, lane.path)
                if (offset === undefined)
                  throw new RangeError('LLVM hook cleanup lost a lane offset')
                reloaded.push(
                  yield* FunctionBody.load(
                    body,
                    laneType(lane),
                    yield* constantBytePointer(base, offset, `${tag}_reload${ordinal}_ptr`),
                    `${tag}_reload${ordinal}`,
                  ),
                )
              }
              yield* dropThroughPlan(plan.inner, Object.freeze(reloaded), `${tag}_inner`)
              return
            }
            case 'StructCleanup': {
              const lanes = semanticLanesOf(plan.type)
              for (const [fieldOrdinal, field] of plan.fields.entries()) {
                if (!CleanupPlan.hasEffect(field.cleanup)) continue
                const fieldValues = lanes.flatMap((lane, index) => {
                  const first = lane.path.at(0)
                  const value = values.at(index)
                  return first !== undefined &&
                    first._tag === 'FieldId' &&
                    value !== undefined &&
                    first.ordinal === field.field.ordinal &&
                    first.struct.ordinal === field.field.struct.ordinal &&
                    first.struct.sourceId === field.field.struct.sourceId
                    ? [value]
                    : []
                })
                yield* dropThroughPlan(
                  field.cleanup,
                  Object.freeze(fieldValues),
                  `${tag}_f${fieldOrdinal}`,
                )
              }
              return
            }
            case 'ArrayCleanup': {
              if (!CleanupPlan.hasEffect(plan.element)) return
              const lanes = semanticLanesOf(plan.type)
              for (let index = 0; index < plan.length; index += 1) {
                const elementValues = lanes.flatMap((lane, ordinal) => {
                  const first = lane.path.at(0)
                  const value = values.at(ordinal)
                  return first !== undefined &&
                    first._tag === 'ElementSelector' &&
                    first.index === index &&
                    value !== undefined
                    ? [value]
                    : []
                })
                yield* dropThroughPlan(
                  plan.element,
                  Object.freeze(elementValues),
                  `${tag}_e${index}`,
                )
              }
              return
            }
            case 'UnionCleanup': {
              if (plan.cases.every((entry) => !CleanupPlan.hasEffect(entry.cleanup))) return
              // Hook-bearing or structurally unsupported cases branch on the live tag and lower
              // the complete plan. Plain reclaim paths select a null context for inactive cases,
              // which libc free ignores.
              const shape = Layout.callingShape(program.layout, plan.type)
              const tagValue = values.at(0)
              if (shape === undefined || tagValue === undefined) {
                throw new RangeError('LLVM union cleanup lost its shape')
              }
              for (const caseEntry of plan.cases) {
                const paths = NativeAggregate.reclaimContextPaths(caseEntry.cleanup)
                if (paths === undefined) {
                  const matches = yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    tagValue,
                    yield* Constant.integerSigned(builder, i32, BigInt(caseEntry.ordinal)),
                    `${tag}_u${caseEntry.ordinal}_is`,
                  )
                  const selectedBlock = yield* LlvmBlock.make(
                    body,
                    `${tag}_u${caseEntry.ordinal}_drop`,
                  )
                  const followingBlock = yield* LlvmBlock.make(
                    body,
                    `${tag}_u${caseEntry.ordinal}_next`,
                  )
                  yield* FunctionBody.conditionalBranch(
                    body,
                    matches,
                    selectedBlock,
                    followingBlock,
                  )
                  yield* LlvmBlock.setInsertionPoint(body, selectedBlock)
                  const physical = Layout.memberFieldSlots(shape, caseEntry.member, [])
                  const targetLanes = semanticLanesOf(caseEntry.member)
                  const selected: Array<Value.Input> = []
                  for (const [targetOrdinal, ordinal] of physical?.entries() ?? []) {
                    const value = values.at(ordinal)
                    const sourceLane = shape.lanes.at(ordinal)
                    const targetLane = targetLanes.at(targetOrdinal)
                    if (value === undefined || sourceLane === undefined || targetLane === undefined)
                      continue
                    selected.push(
                      yield* coerceLane(
                        value,
                        sourceLane,
                        targetLane,
                        `${tag}_u${caseEntry.ordinal}_${targetOrdinal}_lane`,
                      ),
                    )
                  }
                  if (selected.length !== targetLanes.length) {
                    throw new RangeError('LLVM union cleanup lost a member payload lane')
                  }
                  yield* dropThroughPlan(
                    caseEntry.cleanup,
                    Object.freeze(selected),
                    `${tag}_u${caseEntry.ordinal}`,
                  )
                  yield* FunctionBody.branch(body, followingBlock)
                  yield* LlvmBlock.setInsertionPoint(body, followingBlock)
                  // The arm just emitted is only one of this join's two predecessors, so
                  // anything it left in the value cache is unreadable here — a Drop hook
                  // reloads its receiver after calling out, and those loads live in the arm.
                  // Reloading re-roots the cache in this block, which is both valid SSA and
                  // the value the arm may have just mutated.
                  yield* reloadMutableRoots(`${tag}_u${caseEntry.ordinal}_next`)
                  continue
                }
                if (paths.length === 0) continue
                if (free === undefined || usizeType === undefined) {
                  throw new RangeError('LLVM union reclaim cleanup lost its release helper')
                }
                const zero = yield* Constant.integerUnsigned(builder, usizeType, 0n)
                for (const [pathOrdinal, path] of paths.entries()) {
                  const slots = Layout.memberFieldSlots(shape, caseEntry.member, path)
                  const contextSlot = slots?.at(4)
                  const context = contextSlot === undefined ? undefined : values.at(contextSlot)
                  if (context === undefined) {
                    throw new RangeError('LLVM union cleanup lost a reclaim lane')
                  }
                  const matches = yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    tagValue,
                    yield* Constant.integerSigned(builder, i32, BigInt(caseEntry.ordinal)),
                    `${tag}_u${caseEntry.ordinal}_${pathOrdinal}_is`,
                  )
                  const guarded = yield* FunctionBody.select(
                    body,
                    matches,
                    context,
                    zero,
                    `${tag}_u${caseEntry.ordinal}_${pathOrdinal}_context`,
                  )
                  yield* FunctionBody.callDirect(body, free, [
                    yield* FunctionBody.cast(
                      body,
                      'inttoptr',
                      guarded,
                      pointer,
                      `${tag}_u${caseEntry.ordinal}_${pathOrdinal}_pointer`,
                    ),
                  ])
                }
              }
              return
            }
          }
        })

        for (const [blockOrdinal, block] of entry.linear.entries()) {
          const blockHandle = blocks.get(block.id.ordinal)
          if (blockHandle === undefined) continue
          yield* LlvmBlock.setInsertionPoint(body, blockHandle)
          if (blockOrdinal > 0) yield* reloadMutableRoots(`b${block.id.ordinal}`)
          for (const operation of block.operations) {
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
                      ? yield* Constant.integerUnsigned(
                          builder,
                          physicalType,
                          BigInt(operation.value),
                        )
                      : yield* Constant.integerSigned(
                          builder,
                          physicalType,
                          BigInt(operation.value),
                        ),
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
                      yield* Constant.integerUnsigned(
                        builder,
                        yield* LlvmType.integer(builder, 1),
                        0n,
                      ),
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
                const arguments_ = operation.arguments.flatMap((argument) => [
                  ...readLocal(argument),
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
                    if (lane === undefined)
                      throw new RangeError('LLVM OS open runtime lost an output lane')
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
                  yield* Constant.integerUnsigned(
                    builder,
                    usizeType,
                    BigInt(operation.elementAlignment),
                  ),
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
                const stride = yield* Constant.integerUnsigned(
                  builder,
                  usizeType,
                  BigInt(operation.stride),
                )
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
                const masked = yield* FunctionBody.binary(
                  body,
                  'and',
                  alignment,
                  decremented,
                  `${name}_mask`,
                )
                const powerOfTwo = yield* FunctionBody.integerCompare(
                  body,
                  'eq',
                  masked,
                  zero,
                  `${name}_pow2`,
                )
                const valid = yield* FunctionBody.binary(
                  body,
                  'and',
                  nonZero,
                  powerOfTwo,
                  `${name}_valid`,
                )
                const members = operation.type.type.members
                const layoutOrdinal = members.findIndex((member) =>
                  SilkType.equals(member, SilkType.layout),
                )
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
                const first = yield* FunctionBody.select(
                  body,
                  valid,
                  bytes,
                  alignment,
                  `${name}_slot0`,
                )
                const second = yield* FunctionBody.select(
                  body,
                  valid,
                  alignment,
                  zero,
                  `${name}_slot1`,
                )
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
                const budget = yield* FunctionBody.binary(
                  body,
                  'udiv',
                  maximum,
                  safeCount,
                  `${name}_budget`,
                )
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
                const huge = yield* FunctionBody.integerCompare(
                  body,
                  'ugt',
                  bytes,
                  headroom,
                  `${name}_huge`,
                )
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
                const total = yield* FunctionBody.binary(
                  body,
                  'mul',
                  stride,
                  count,
                  `${name}_total`,
                )
                const members = operation.type.type.members
                const layoutOrdinal = members.findIndex((member) =>
                  SilkType.equals(member, SilkType.layout),
                )
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
                const totalOut = yield* FunctionBody.select(
                  body,
                  overflow,
                  zero,
                  total,
                  `${name}_bytes`,
                )
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
                    } else if (
                      candidate._tag === 'ElementSelector' &&
                      candidate.index._tag === 'Proven'
                    ) {
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
                          yield* Constant.integerUnsigned(
                            builder,
                            usizeType ?? i32,
                            BigInt(staticOffset),
                          ),
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
                let selected = SilkType.isReference(rootSemantic)
                  ? rootSemantic.target
                  : rootSemantic
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
                    if (field === undefined)
                      throw new RangeError('LLVM borrow field lost its field layout')
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
                  const continuation = yield* LlvmBlock.make(
                    body,
                    `borrow${checkOrdinal}_${ordinal}_ok`,
                  )
                  yield* FunctionBody.conditionalBranch(body, inBounds, continuation, trapBlock)
                  yield* LlvmBlock.setInsertionPoint(body, continuation)
                  const scaled = yield* FunctionBody.binary(
                    body,
                    'mul',
                    index,
                    yield* Constant.integerUnsigned(
                      builder,
                      usizeType ?? i32,
                      BigInt(offset.stride),
                    ),
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
                if (rootBase === undefined)
                  throw new RangeError('LLVM borrow formation lost its root')
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
                  const tag = yield* Constant.integerSigned(
                    builder,
                    i32,
                    BigInt(mapping.targetOrdinal),
                  )
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
                  if (address === undefined)
                    throw new RangeError('LLVM reference read lost its address')
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
                    if (offset === undefined)
                      throw new RangeError('LLVM reference read lost a lane offset')
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
                    } else if (
                      candidate._tag === 'ElementSelector' &&
                      candidate.index._tag === 'Proven'
                    ) {
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
                            yield* Constant.integerUnsigned(
                              builder,
                              usizeType ?? i32,
                              BigInt(staticOffset),
                            ),
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
                    if (
                      sourceLane.path.length !==
                      operation.selectors.length + destinationLane.path.length
                    ) {
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
                        if (
                          selector.index._tag === 'Proven' &&
                          physical.index !== selector.index.value
                        ) {
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
                    throw new RangeError(
                      `Backend could not realize place-read lane ${destinationOrdinal}`,
                    )
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
                  if (address === undefined)
                    throw new RangeError('LLVM reference write lost its address')
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
                    } else if (
                      candidate._tag === 'ElementSelector' &&
                      candidate.index._tag === 'Proven'
                    ) {
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
                            yield* Constant.integerUnsigned(
                              builder,
                              usizeType ?? i32,
                              BigInt(staticOffset),
                            ),
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
                    throw new RangeError(
                      `Backend could not realize place-write lane ${rootOrdinal}`,
                    )
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
                if (targetLane === undefined)
                  throw new RangeError('LLVM reinterpretation lost its lane')
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
                  if (result === undefined)
                    throw new RangeError('LLVM square root produced no value')
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
                const exponentMaskValue = yield* Constant.integerUnsigned(
                  builder,
                  integerType,
                  exponentMask,
                )
                const fractionMaskValue = yield* Constant.integerUnsigned(
                  builder,
                  integerType,
                  fractionMask,
                )
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
                  const maximum = yield* Constant.integerUnsigned(
                    builder,
                    sourcePhysical,
                    0x10ffffn,
                  )
                  const surrogateMinimum = yield* Constant.integerUnsigned(
                    builder,
                    sourcePhysical,
                    0xd800n,
                  )
                  const surrogateMaximum = yield* Constant.integerUnsigned(
                    builder,
                    sourcePhysical,
                    0xdfffn,
                  )
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
                  invalid = yield* FunctionBody.binary(
                    body,
                    'or',
                    aboveMaximum,
                    surrogate,
                    `${name}_invalid`,
                  )
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
                          ? yield* Constant.integerSigned(
                              builder,
                              sourcePhysical,
                              targetRange.minimum,
                            )
                          : yield* Constant.integerUnsigned(
                              builder,
                              sourcePhysical,
                              targetRange.minimum,
                            ),
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
                          ? yield* Constant.integerSigned(
                              builder,
                              sourcePhysical,
                              targetRange.maximum,
                            )
                          : yield* Constant.integerUnsigned(
                              builder,
                              sourcePhysical,
                              targetRange.maximum,
                            ),
                        `${name}_above`,
                      ),
                    )
                  invalid =
                    checks.at(0) ??
                    (yield* Constant.integerUnsigned(
                      builder,
                      yield* LlvmType.integer(builder, 1),
                      0n,
                    ))
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
                    target.signedness === 'Unsigned'
                      ? unsignedOverflowSignatures
                      : signedOverflowSignatures
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
                  if (pair === undefined)
                    throw new RangeError('LLVM checked arithmetic produced no outcome')
                  result = yield* FunctionBody.extractValue(body, pair, [0], `${name}_value`)
                  invalid = yield* FunctionBody.extractValue(body, pair, [1], `${name}_invalid`)
                } else {
                  if (target.category !== 'Integer')
                    throw new RangeError('LLVM checked division lost its integer target')
                  if (right === undefined)
                    throw new RangeError('LLVM checked division lost its right operand')
                  const zero = yield* Constant.integerUnsigned(builder, targetPhysical, 0n)
                  invalid = yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    right,
                    zero,
                    `${name}_zero`,
                  )
                  if (target.signedness === 'Signed' && operation.operation === 'CheckedDivide') {
                    const range = Scalar.range(target, pointerBits)
                    const minimum = yield* Constant.integerSigned(
                      builder,
                      targetPhysical,
                      range.minimum,
                    )
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
                    invalid = yield* FunctionBody.binary(
                      body,
                      'or',
                      invalid,
                      overflow,
                      `${name}_invalid`,
                    )
                  }
                  const one = yield* Constant.integerUnsigned(builder, targetPhysical, 1n)
                  const safeRight = yield* FunctionBody.select(
                    body,
                    invalid,
                    one,
                    right,
                    `${name}_divisor`,
                  )
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
                const successTag = yield* Constant.integerSigned(
                  builder,
                  i32,
                  BigInt(successOrdinal),
                )
                const failureTag = yield* Constant.integerSigned(
                  builder,
                  i32,
                  BigInt(failureOrdinal),
                )
                const tag = yield* FunctionBody.select(
                  body,
                  invalid,
                  failureTag,
                  successTag,
                  `${name}_tag`,
                )
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
                const scalar =
                  typeof semanticOperand === 'string' ? Scalar.find(semanticOperand) : undefined
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
                    const result = yield* FunctionBody.cast(
                      body,
                      'zext',
                      flag,
                      i32,
                      `total${ordinal}`,
                    )
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
                    const result = yield* FunctionBody.cast(
                      body,
                      'zext',
                      flag,
                      i32,
                      `fcmp${ordinal}`,
                    )
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
                    throw new RangeError(
                      `LLVM float operation ${operation.operator} is unavailable`,
                    )
                  const result = yield* FunctionBody.binary(
                    body,
                    mnemonic,
                    left,
                    right,
                    `float${ordinal}`,
                  )
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
                  const result = yield* FunctionBody.binary(
                    body,
                    mnemonic,
                    left,
                    right,
                    `integer${ordinal}`,
                  )
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
                if (
                  operation.operator === 'SaturatingAdd' ||
                  operation.operator === 'SaturatingSubtract'
                ) {
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
                  const signatures = unsigned
                    ? unsignedOverflowSignatures
                    : signedOverflowSignatures
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
                  if (pair === undefined)
                    throw new RangeError('LLVM saturating multiply produced no value')
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
                    const minimum = yield* Constant.integerSigned(
                      builder,
                      operandType,
                      range.minimum,
                    )
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
                  const signatures = unsigned
                    ? unsignedOverflowSignatures
                    : signedOverflowSignatures
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
                  const valuePart = yield* FunctionBody.extractValue(
                    body,
                    pair,
                    [0],
                    `arith${ordinal}`,
                  )
                  const overflowed = yield* FunctionBody.extractValue(
                    body,
                    pair,
                    [1],
                    `arith${ordinal}_flag`,
                  )
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
                        ? Scalar.range(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
                            .minimum
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
                  if (field === undefined)
                    throw new RangeError('Effect capture lost its environment field')
                  if (field.representation !== 'Borrow') {
                    captured.push(...readLocal(capture.source))
                    continue
                  }
                  yield* ensureAddressRoot(capture.source)
                  const base = addressStorage.get(capture.source.ordinal)
                  if (base === undefined)
                    throw new RangeError('Effect borrowed capture lost its storage')
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
                if (sourceType === undefined)
                  throw new RangeError('LLVM effect outcome lost its source type')
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
                if (sourceTag === undefined)
                  throw new RangeError('Effect failure union lost its tag lane')
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
                const runArguments = operation.arguments.flatMap((argument) => [
                  ...readLocal(argument),
                ])
                if (
                  yield* emitOrigin(
                    operation,
                    runArguments,
                    `effect_run${operation.destination.ordinal}`,
                  )
                )
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
                if (storage === undefined)
                  throw new RangeError('Effect run destination is not materialized')
                const loaded: Array<Value.Input> = []
                for (const [lane, pointer] of storage.entries()) {
                  const callingLane = lanesFor(operation.type).at(lane)
                  if (callingLane === undefined)
                    throw new RangeError('Effect run destination lost a lane')
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
                  const selectedTag = yield* Constant.integerSigned(
                    builder,
                    i32,
                    BigInt(alternativeOrdinal),
                  )
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
                    Mir.matchesInstance(
                      candidate.fn,
                      alternative.runner,
                      alternative.runnerTypeArguments,
                    ),
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
                yield* reloadMutableRoots(
                  `effect_composite${operation.destination.ordinal}_following`,
                )
                const outcomeStorage = mutableStorage.get(operation.outcome.ordinal)
                if (outcomeStorage === undefined)
                  throw new RangeError('Effect composite outcome is not materialized')
                const outcomeValues: Array<Value.Input> = []
                for (const [ordinal, pointer] of outcomeStorage.entries()) {
                  const lane = joinedOutcomeLanes.at(ordinal)
                  if (lane === undefined)
                    throw new RangeError('Effect composite outcome lost a lane')
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
                yield* reloadMutableRoots(
                  `effect_composite${operation.destination.ordinal}_completed`,
                )
                const destinationStorage = mutableStorage.get(operation.destination.ordinal)
                if (destinationStorage === undefined)
                  throw new RangeError('Effect composite destination is not materialized')
                const loaded: Array<Value.Input> = []
                for (const [ordinal, pointer] of destinationStorage.entries()) {
                  const lane = lanesFor(operation.type).at(ordinal)
                  if (lane === undefined)
                    throw new RangeError('Effect composite destination lost a lane')
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
                    ? [
                        ...operation.captures.map((capture) => capture.source),
                        ...operation.arguments,
                      ]
                    : undefined
                const target = declared.find(
                  (candidate) =>
                    Mir.matchesInstance(
                      candidate.fn,
                      operation.runner,
                      operation.runnerTypeArguments,
                    ) &&
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
                  operation._tag === 'RunStaticEffect'
                    ? undefined
                    : suspensionRegions.get(operation)
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
                  Mir.matchesInstance(
                    candidate.fn,
                    operation.runner,
                    operation.runnerTypeArguments,
                  ),
                )
                if (target === undefined)
                  throw new RangeError('Backend cannot resolve Effect result runner')
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
                  if (lane === undefined)
                    throw new RangeError('Effect result destination lost a lane')
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
                  const otherwise = yield* LlvmBlock.make(
                    body,
                    `effect_entry_tag${failure.tag}_otherwise`,
                  )
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
                if (pointer === undefined)
                  throw new RangeError('Effect entry status is not materialized')
                locals.set(
                  operation.destination.ordinal,
                  Object.freeze([
                    yield* FunctionBody.load(body, i32, pointer, 'effect_entry_status'),
                  ]),
                )
                break
              }
              case 'ApplyCallable': {
                const sourceType =
                  operation.callable === undefined
                    ? undefined
                    : entry.fn.localTypes.at(operation.callable.ordinal)
                const target =
                  operation.target ??
                  (sourceType?._tag === 'CallableValue' ? sourceType.target : undefined)
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
                          values: Object.freeze(
                            environmentValues.slice(cursor, cursor + shape.laneCount),
                          ),
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
                    firstLocal === undefined
                      ? undefined
                      : entry.fn.localTypes.at(firstLocal.ordinal)
                  if (first === undefined || firstType === undefined)
                    throw new RangeError('LLVM callable builtin lost its first operand')
                  const conversionTarget = Scalar.conversionTarget(target.operation)
                  if (conversionTarget !== undefined) {
                    const sourceScalar = Scalar.find(firstType._tag)
                    if (sourceScalar?.category === 'Floating') {
                      const destination =
                        integerTypes.get(
                          Scalar.bits(
                            conversionTarget,
                            program.layout.target.pointerSize === 4 ? 32 : 64,
                          ),
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
                  if (
                    target.operation === 'Negate' &&
                    Scalar.find(firstType._tag)?.category === 'Floating'
                  ) {
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
                  throw new RangeError(
                    `Backend cannot resolve call target ${operation.target.name}`,
                  )
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
            const destination = destinationOf(operation)
            if (destination !== undefined && mutableRoots.has(destination.ordinal)) {
              yield* storeMutable(destination, readLocal(destination))
            }
            if (destination !== undefined && addressRoots.has(destination.ordinal)) {
              yield* storeAddressRootValues(
                destination.ordinal,
                readLocal(destination),
                `addr${destination.ordinal}_defined`,
              )
            }
          }
          const terminator = block.terminator
          switch (terminator._tag) {
            case 'PropagateEffectFailure': {
              const source = readLocal(terminator.source)
              const sourceTag = terminator.sourceType._tag === 'Union' ? source.at(0) : undefined
              let mappedTag: Value.Input
              if (terminator.sourceType._tag === 'Nominal') {
                mappedTag = yield* Constant.integerSigned(
                  builder,
                  i32,
                  BigInt(terminator.tagMappings.at(0)?.target ?? -1),
                )
              } else {
                if (sourceTag === undefined)
                  throw new RangeError('Effect failure propagation lost its tag lane')
                mappedTag = yield* Constant.integerSigned(builder, i32, -1n)
                for (const [ordinal, mapping] of terminator.tagMappings.entries()) {
                  const matches = yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    sourceTag,
                    yield* Constant.integerSigned(builder, i32, BigInt(mapping.source)),
                    `effect_failure_propagation${terminator.source.ordinal}_${ordinal}`,
                  )
                  mappedTag = yield* FunctionBody.select(
                    body,
                    matches,
                    yield* Constant.integerSigned(builder, i32, BigInt(mapping.target)),
                    mappedTag,
                    `effect_failure_propagation${terminator.source.ordinal}_${ordinal}_tag`,
                  )
                }
              }
              for (const release of terminator.releases ?? []) {
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
                  source,
                  Mir.semanticType(terminator.sourceType),
                  sourceTag,
                  terminator.propagationType.type,
                  terminator.tagMappings,
                  `effect_failure_propagation${terminator.source.ordinal}_payload`,
                )),
              ]
              if (entry.suspendable) {
                yield* returnStep(0n, Object.freeze(returned), 'propagated_selective_failure_step')
              } else {
                yield* FunctionBody.returnValue(
                  body,
                  returned.length === 1
                    ? (returned.at(0) ?? mappedTag)
                    : yield* FunctionBody.buildAggregate(
                        body,
                        entry.resultType,
                        Object.freeze(returned.slice(0, terminator.propagationLaneCount)),
                        'propagated_selective_failure',
                      ),
                )
              }
              break
            }
            case 'Return': {
              const returned = readLocal(terminator.value)
              if (entry.suspendable) {
                yield* returnStep(0n, returned, `complete_value_b${block.id.ordinal}`)
                break
              }
              const instruction =
                returned.length === 0
                  ? yield* FunctionBody.returnVoid(body)
                  : returned.length === 1
                    ? yield* FunctionBody.returnValue(body, readScalar(terminator.value))
                    : yield* FunctionBody.returnValue(
                        body,
                        yield* FunctionBody.buildAggregate(
                          body,
                          entry.resultType,
                          returned,
                          `return_value_b${block.id.ordinal}`,
                        ),
                      )
              yield* locate(terminator.provenance.span, instruction)
              break
            }
            case 'Jump': {
              const target = NativeControl.targetBlock(blocks, terminator.target, 'Backend jump')
              yield* FunctionBody.branch(body, target)
              break
            }
            case 'Branch': {
              const zero = yield* Constant.integerSigned(builder, i32, 0n)
              const condition = yield* FunctionBody.integerCompare(
                body,
                'ne',
                readScalar(terminator.condition),
                zero,
                `c${blockOrdinal}`,
              )
              const taken = NativeControl.targetBlock(blocks, terminator.taken, 'Backend branch')
              const otherwise = NativeControl.targetBlock(
                blocks,
                terminator.otherwise,
                'Backend branch',
              )
              yield* FunctionBody.conditionalBranch(body, condition, taken, otherwise)
              break
            }
            case 'MatchBranch': {
              const tag = readLocal(terminator.scrutinee).at(0)
              if (tag === undefined) throw new RangeError('LLVM union match has no tag lane')
              const expected = yield* Constant.integerSigned(
                builder,
                i32,
                BigInt(terminator.memberOrdinal),
              )
              const condition = yield* FunctionBody.integerCompare(
                body,
                'eq',
                tag,
                expected,
                `match${block.id.ordinal}_member`,
              )
              const taken = NativeControl.targetBlock(blocks, terminator.taken, 'LLVM match branch')
              const otherwise = NativeControl.targetBlock(
                blocks,
                terminator.otherwise,
                'LLVM match branch',
              )
              yield* FunctionBody.conditionalBranch(body, condition, taken, otherwise)
              break
            }
            case 'Trap': {
              yield* Intrinsic.call(body, 'trap', [], [])
              const instruction = yield* FunctionBody.unreachable(body)
              yield* locate(terminator.provenance.span, instruction)
              break
            }
          }
        }

        if (trapBlock !== undefined) {
          yield* LlvmBlock.setInsertionPoint(body, trapBlock)
          yield* Intrinsic.call(body, 'trap', [], [])
          yield* FunctionBody.unreachable(body)
        }
      }),
    )
  }

  for (const origin of originThunks.values()) {
    yield* FunctionActor.buildBody(
      builder,
      origin.handle,
      Effect.fnUntraced(function* (body) {
        yield* LlvmBlock.make(body, 'entry')
        const transfer = yield* Value.argument(body, 0)
        const target = declared.find((candidate) =>
          origin.region.deferred.instance !== undefined
            ? Mir.matchesInstanceKey(candidate.fn, origin.region.deferred.instance)
            : origin.region.deferred.declaration !== undefined &&
              Mir.matchesInstance(
                candidate.fn,
                origin.region.deferred.declaration,
                origin.region.deferred.typeArguments,
              ),
        )
        if (target === undefined) throw new RangeError('LLVM child thunk lost deferred runner')
        const argumentLanes = NativeSuspension.logicalLanes(
          origin.owner.fn,
          NativeCall.operationInputs(origin.region.operation),
          lanesFor,
        )
        const packed = NativeType.packLanes(program.layout.target, argumentLanes)
        const arguments_: Array<Value.Input> = []
        for (const [ordinal, lane] of packed.entries.entries()) {
          arguments_.push(
            yield* FunctionBody.load(
              body,
              laneType(lane.lane),
              yield* NativeLanePointer.lanePointer(
                lanePointers,
                body,
                transfer,
                transferHeaderSize + lane.offset,
                `child_argument${ordinal}_ptr`,
              ),
              `child_argument${ordinal}`,
            ),
          )
        }
        const result = yield* FunctionBody.callDirect(
          body,
          target.handle,
          target.suspendable
            ? [
                ...arguments_,
                transfer,
                yield* Constant.nullValue(builder, pointer),
                yield* Constant.integerUnsigned(builder, i32, 0n),
              ]
            : arguments_,
          'child_step',
        )
        if (target.resultLaneCount > 0 && result === undefined)
          throw new RangeError('LLVM child thunk lost result')
        const status = target.suspendable
          ? result === undefined
            ? undefined
            : yield* FunctionBody.extractValue(body, result, [0], 'child_status')
          : yield* Constant.integerUnsigned(builder, i32, 0n)
        if (status === undefined) throw new RangeError('LLVM child thunk lost status')
        const resultLanes = lanesFor(target.fn.result)
        const resultPacked = NativeType.packLanes(
          program.layout.target,
          resultLanes,
          transferResultOffset,
        )
        for (const [ordinal, lane] of resultPacked.entries.entries()) {
          const value =
            resultLanes.length === 1 && !target.suspendable
              ? result
              : result === undefined
                ? undefined
                : yield* FunctionBody.extractValue(
                    body,
                    result,
                    [target.suspendable ? ordinal + 1 : ordinal],
                    `child_result${ordinal}`,
                  )
          if (value === undefined) throw new RangeError('LLVM child thunk lost result lane')
          yield* FunctionBody.store(
            body,
            yield* FunctionBody.freeze(body, value, `child_result${ordinal}_stable`),
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              transfer,
              lane.offset,
              `child_result${ordinal}_ptr`,
            ),
          )
        }
        yield* FunctionBody.returnValue(body, status)
      }),
    )
  }

  for (const resume of resumeThunks.values()) {
    yield* FunctionActor.buildBody(
      builder,
      resume.handle,
      Effect.fnUntraced(function* (body) {
        yield* LlvmBlock.make(body, 'entry')
        const transfer = yield* Value.argument(body, 0)
        const frame = yield* Value.argument(body, 1)
        const ordinal = [...resumeThunks.values()]
          .filter((candidate) => candidate.owner === resume.owner)
          .sort((left, right) =>
            suspensionPointKey(left.region.point).localeCompare(
              suspensionPointKey(right.region.point),
            ),
          )
          .indexOf(resume)
        if (ordinal < 0) throw new RangeError('LLVM resume thunk lost dispatch identity')
        const parameters = resume.owner.fn.localTypes
          .slice(0, resume.owner.fn.parameterCount)
          .flatMap((type) => lanesFor(type))
        const result = yield* FunctionBody.callDirect(
          body,
          resume.owner.handle,
          [
            ...(yield* Effect.forEach(parameters, (lane) =>
              Constant.nullValue(builder, laneType(lane)),
            )),
            transfer,
            frame,
            yield* Constant.integerUnsigned(builder, i32, BigInt(ordinal + 1)),
          ],
          'resume_step',
        )
        if (result === undefined) throw new RangeError('LLVM resume thunk lost step result')
        const status = yield* FunctionBody.extractValue(body, result, [0], 'resume_status')
        const resultPacked = NativeType.packLanes(
          program.layout.target,
          lanesFor(resume.owner.fn.result),
          transferResultOffset,
        )
        for (const [laneOrdinal, lane] of resultPacked.entries.entries()) {
          const value = yield* FunctionBody.extractValue(
            body,
            result,
            [laneOrdinal + 1],
            `resume_result${laneOrdinal}`,
          )
          yield* FunctionBody.store(
            body,
            yield* FunctionBody.freeze(body, value, `resume_result${laneOrdinal}_stable`),
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              transfer,
              lane.offset,
              `resume_result${laneOrdinal}_ptr`,
            ),
          )
        }
        yield* FunctionBody.returnValue(body, status)
      }),
    )
  }

  if (driver !== undefined && machine !== undefined) {
    yield* FunctionActor.buildBody(
      builder,
      driver,
      Effect.fnUntraced(function* (body) {
        yield* LlvmBlock.make(body, 'entry')
        const arguments_: Array<Value.Input> = []
        for (let ordinal = 0; ordinal < machine.parameterTypes.length - 3; ordinal += 1)
          arguments_.push(yield* Value.argument(body, ordinal))
        const transfer = yield* FunctionBody.alloca(body, i8, 'suspend_transfer', {
          count: yield* Constant.integerUnsigned(
            builder,
            i32,
            BigInt(Math.max(transferStorageSize, 1)),
          ),
          alignment: yield* Alignment.fromByteUnits(program.layout.target.pointerAlignment),
        })
        const nullPointer = yield* Constant.nullValue(builder, pointer)
        yield* FunctionBody.store(
          body,
          nullPointer,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            transfer,
            program.layout.target.pointerSize,
            'suspend_initial_head_ptr',
          ),
        )
        const initial = yield* FunctionBody.callDirect(
          body,
          machine.handle,
          [...arguments_, transfer, nullPointer, yield* Constant.integerUnsigned(builder, i32, 0n)],
          'suspend_initial',
        )
        if (initial === undefined) throw new RangeError('LLVM suspension driver lost initial step')
        const initialStatus = yield* FunctionBody.extractValue(
          body,
          initial,
          [0],
          'suspend_initial_status',
        )
        const initialComplete = yield* LlvmBlock.make(body, 'suspend_initial_complete')
        const drive = yield* LlvmBlock.make(body, 'suspend_drive')
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            initialStatus,
            yield* Constant.integerUnsigned(builder, i32, 0n),
            'suspend_initial_done',
          ),
          initialComplete,
          drive,
        )
        const returnMachineResult = Effect.fnUntraced(function* (
          values: ReadonlyArray<Value.Input>,
          tag: string,
        ) {
          if (machine.resultLaneCount === 0) return yield* FunctionBody.returnVoid(body)
          if (machine.resultLaneCount === 1) {
            const value = values.at(0)
            if (value === undefined) throw new RangeError('LLVM driver lost scalar result')
            return yield* FunctionBody.returnValue(body, value)
          }
          return yield* FunctionBody.returnValue(
            body,
            yield* FunctionBody.buildAggregate(body, machine.resultType, values, tag),
          )
        })
        yield* LlvmBlock.setInsertionPoint(body, initialComplete)
        const initialValues: Array<Value.Input> = []
        for (let ordinal = 0; ordinal < machine.resultLaneCount; ordinal += 1)
          initialValues.push(
            yield* FunctionBody.extractValue(
              body,
              initial,
              [ordinal + 1],
              `suspend_initial_result${ordinal}`,
            ),
          )
        yield* returnMachineResult(Object.freeze(initialValues), 'suspend_initial_result')
        yield* LlvmBlock.setInsertionPoint(body, drive)
        const child = yield* FunctionBody.load(
          body,
          pointer,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            transfer,
            0,
            'suspend_child_ptr',
          ),
          'suspend_child',
        )
        if (childThunkType === undefined || resumeThunkType === undefined)
          throw new RangeError('LLVM driver lost private thunk types')
        const childStatus = yield* FunctionBody.call(
          body,
          childThunkType,
          child,
          [transfer],
          'suspend_child_status',
        )
        if (childStatus === undefined) throw new RangeError('LLVM driver child returned void')
        const childTransferred = yield* LlvmBlock.make(body, 'suspend_child_transferred')
        const childCompleted = yield* LlvmBlock.make(body, 'suspend_child_completed')
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            childStatus,
            yield* Constant.integerUnsigned(builder, i32, 0n),
            'suspend_child_done',
          ),
          childCompleted,
          childTransferred,
        )
        yield* LlvmBlock.setInsertionPoint(body, childTransferred)
        yield* FunctionBody.branch(body, drive)
        yield* LlvmBlock.setInsertionPoint(body, childCompleted)
        const parentPointer = yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          transfer,
          program.layout.target.pointerSize,
          'suspend_parent_ptr',
        )
        const parent = yield* FunctionBody.load(body, pointer, parentPointer, 'suspend_parent')
        const finish = yield* LlvmBlock.make(body, 'suspend_finish')
        const resumeParent = yield* LlvmBlock.make(body, 'suspend_resume_parent')
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            yield* FunctionBody.cast(
              body,
              'ptrtoint',
              parent,
              usizeType ?? i32,
              'suspend_parent_addr',
            ),
            yield* Constant.integerUnsigned(builder, usizeType ?? i32, 0n),
            'suspend_has_no_parent',
          ),
          finish,
          resumeParent,
        )
        yield* LlvmBlock.setInsertionPoint(body, resumeParent)
        const nextParent = yield* FunctionBody.load(
          body,
          pointer,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            parent,
            0,
            'suspend_next_parent_ptr',
          ),
          'suspend_next_parent',
        )
        yield* FunctionBody.store(body, nextParent, parentPointer)
        const appendPointerPointer = yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          transfer,
          program.layout.target.pointerSize * 2,
          'suspend_append_ptr_ptr',
        )
        yield* FunctionBody.store(body, parentPointer, appendPointerPointer)
        const resumeFunction = yield* FunctionBody.load(
          body,
          pointer,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            parent,
            program.layout.target.pointerSize,
            'suspend_resume_ptr',
          ),
          'suspend_resume',
        )
        const resumedStatus = yield* FunctionBody.call(
          body,
          resumeThunkType,
          resumeFunction,
          [transfer, parent],
          'suspend_resumed_status',
        )
        if (resumedStatus === undefined) throw new RangeError('LLVM resume returned void')
        const resumeTransferred = yield* LlvmBlock.make(body, 'suspend_resume_transferred')
        const resumeCompleted = yield* LlvmBlock.make(body, 'suspend_resume_completed')
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            resumedStatus,
            yield* Constant.integerUnsigned(builder, i32, 0n),
            'suspend_resume_done',
          ),
          resumeCompleted,
          resumeTransferred,
        )
        yield* LlvmBlock.setInsertionPoint(body, resumeTransferred)
        yield* FunctionBody.branch(body, drive)
        yield* LlvmBlock.setInsertionPoint(body, resumeCompleted)
        yield* FunctionBody.branch(body, childCompleted)
        yield* LlvmBlock.setInsertionPoint(body, finish)
        const finalValues: Array<Value.Input> = []
        const finalPacked = NativeType.packLanes(
          program.layout.target,
          lanesFor(machine.fn.result),
          transferResultOffset,
        )
        for (const [ordinal, lane] of finalPacked.entries.entries())
          finalValues.push(
            yield* FunctionBody.load(
              body,
              laneType(lane.lane),
              yield* NativeLanePointer.lanePointer(
                lanePointers,
                body,
                transfer,
                lane.offset,
                `suspend_final_result${ordinal}_ptr`,
              ),
              `suspend_final_result${ordinal}`,
            ),
          )
        yield* returnMachineResult(Object.freeze(finalValues), 'suspend_final_result')
      }),
    )
  }

  // The module is verified before it is encoded: what reaches Clang has already been checked
  // for the SSA invariants Clang itself will not check on `-x ir` input.
  const violations = yield* Verify.verify(builder)
  if (violations.length > 0) {
    return yield* new BackendError({
      operation: 'Backend.emit',
      backend: 'LLVM',
      message: `LLVM emitted an invalid module for ${program.module} (${violations.length} violation(s)):\n${formatModuleViolations(violations)}`,
      reason: { _tag: 'InvalidModule', violations },
    })
  }

  return {
    symbols: declared.map((entry) =>
      Object.freeze({
        declaration: entry.fn.id,
        instance: entry.fn.instance,
        symbol: entry.publicSymbol,
      }),
    ),
    nativeRuntimeSymbols: Object.freeze([
      ...[...osRuntimes.values()].map((runtime) => runtime.symbol),
      ...(needsHostWrite ? ['silk_standard_stream_write_v1'] : []),
      ...(suspensionEnabled ? CoroutineRuntime.symbols : []),
    ]),
    ir: yield* IrText.render(builder),
    bitcode: yield* Bitcode.encode(builder),
  }
})

/** The bootstrap LLVM backend over the Silk LLVM builder. */
export const LlvmBackend: Backend<LlvmBitcodeArtifact> = Object.freeze({
  _tag: 'Backend',
  id: 'llvm',
  name: 'LLVM',
  targets: Object.freeze([
    ...Target.native.map((target) => target.id),
    Target.wasm32UnknownUnknown.id,
  ]),
  emit: Effect.fn('Backend.LLVM.emit')(function* (
    program: Mir.Module,
    request: CodegenRequest,
  ): Effect.fn.Return<LlvmBitcodeArtifact, BackendError> {
    const output = yield* emitProgram(program, request).pipe(
      Effect.catchTag('LlvmError', (cause) =>
        Effect.fail(
          new BackendError({
            operation: 'Backend.emit',
            backend: 'LLVM',
            message: `LLVM emission failed for ${program.module}`,
            reason: { _tag: 'WrappedFailure', cause },
          }),
        ),
      ),
    )
    return Object.freeze({
      _tag: 'LlvmBitcodeArtifact',
      backend: 'llvm',
      module: program.module,
      target: program.layout.target,
      symbols: Object.freeze(output.symbols),
      termination: terminationOf(program),
      nativeRuntimeSymbols: output.nativeRuntimeSymbols,
      control: llvmControl(program),
      bitcode: output.bitcode,
      ir: output.ir,
    })
  }),
})
