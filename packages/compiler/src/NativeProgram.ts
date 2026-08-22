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
import type { CodegenRequest, SymbolEntry } from './Backend.js'
import {
  BackendError,
  formatModuleViolations,
  lineTable,
  positionOf,
  sanitize,
  suspensionPointKey,
} from './Backend.js'
import * as CleanupPlan from './CleanupPlan.js'
import * as CoroutineFrame from './CoroutineFrame.js'
import * as CoroutineRuntime from './CoroutineRuntime.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
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
import type * as NativeLoweringContext from './NativeLoweringContext.js'
import * as NativeOperation from './NativeOperation.js'
import * as NativeSuspension from './NativeSuspension.js'
import * as NativeType from './NativeType.js'
import * as Scalar from './Scalar.js'
import type * as SourceSpan from './SourceSpan.js'
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

import { destinationOf } from './MirLinearization.js'

export const emit = Effect.fn('NativeProgram.emit')(function* (
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
  const typeContext: NativeType.LoweringContext = Object.freeze({
    program,
    i32,
    f32,
    f64,
    pointer,
    integerTypes,
  })
  const lanesFor = (type: Mir.Type): ReadonlyArray<Layout.CallingLane> =>
    NativeType.lanesFor(typeContext, type)
  const valueLanesFor = (type: Mir.Type): ReadonlyArray<Layout.CallingLane> =>
    NativeType.valueLanesFor(typeContext, type)
  const laneType = (lane: Layout.CallingLane): LlvmType.Type =>
    NativeType.laneType(typeContext, lane)
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

  const functionDeclarations = yield* NativeDeclare.functions(
    Object.freeze({ builder, program, i32, pointer, lanesFor, laneType }),
  )
  const declared = functionDeclarations.declared
  if (functionDeclarations.voidType !== undefined) voidType = functionDeclarations.voidType
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
  const debugContext: NativeDebug.LoweringContext = Object.freeze({
    builder,
    program,
    enabled: debug,
    file,
    types: debugTypes,
  })
  const debugTypeOf = (type: Mir.Type) => NativeDebug.typeOf(debugContext, type)

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
        const loweringContext: NativeLoweringContext.LoweringContext = Object.freeze({
          builder,
          body,
          program,
          request,
          layout: program.layout,
          types: Object.freeze({ i8, i32, f32, f64, pointer, integers: integerTypes }),
          lanesFor,
          valueLanesFor,
          laneType,
          packedLanes: (lanes: ReadonlyArray<Layout.CallingLane>, start?: number) =>
            NativeType.packLanes(program.layout.target, lanes, start),
          declared,
          entry,
          mutableStorage,
        })
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

        const storageContext: NativeFunction.StorageContext = Object.freeze({
          builder: loweringContext.builder,
          body: loweringContext.body,
          byteType: i8,
          offsetType: i32,
          fn: loweringContext.entry.fn,
          layout: loweringContext.layout,
          mutableRoots,
          mutableStorage: loweringContext.mutableStorage,
          addressStorage,
          locals,
          valueLanesFor: loweringContext.valueLanesFor,
          laneType: loweringContext.laneType,
        })
        const reloadMutableRoots = (tag: string) =>
          NativeFunction.reloadMutableRoots(storageContext, tag)
        const storeAddressRootValues = (
          root: number,
          values: ReadonlyArray<Value.Input>,
          name: string,
        ) => NativeFunction.storeAddressRootValues(storageContext, root, values, name)
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

        const laneContext: NativeArith.LaneContext = Object.freeze({
          body: loweringContext.body,
          pointerBits: loweringContext.layout.target.pointerSize === 4 ? 32 : 64,
          i32: loweringContext.types.i32,
          integerTypes: loweringContext.types.integers,
          laneType: loweringContext.laneType,
        })
        const coerceLane = (
          input: Value.Input,
          source: Layout.CallingLane,
          target: Layout.CallingLane,
          name: string,
        ) => NativeArith.coerceLane(laneContext, input, source, target, name)

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

        const suspensionReturnContext: NativeSuspension.ReturnContext = Object.freeze({
          builder,
          body,
          i32,
          pointer,
          entry,
          ...(invocationFrameStorage === undefined ? {} : { invocationFrameStorage }),
          ...(coroutineFramePop === undefined ? {} : { coroutineFramePop }),
          lanesFor,
          laneType,
        })
        const returnStep = (status: bigint, values: ReadonlyArray<Value.Input>, tag: string) =>
          NativeSuspension.returnStep(suspensionReturnContext, status, values, tag)
        const synchronousCallContext: NativeCall.SynchronousContext = Object.freeze({
          body,
          addressRoots,
          reloadAddressRoot: (root: number) => reloadAddressRoot(root),
        })
        const callSynchronousValues = (
          target: (typeof declared)[number],
          arguments_: ReadonlyArray<Value.Input>,
          name: string,
        ) => NativeCall.callSynchronous(synchronousCallContext, target, arguments_, name)
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

        const operationContext: NativeOperation.LoweringContext = Object.freeze({
          builder,
          body,
          program,
          entry,
          declared,
          locals,
          staticPointers,
          i32,
          f32,
          f64,
          pointer,
          ...(usizeType === undefined ? {} : { usizeType }),
          integerTypes,
          signedOverflowSignatures,
          unsignedOverflowSignatures,
          ...(malloc === undefined ? {} : { malloc }),
          ...(free === undefined ? {} : { free }),
          ...(memcmp === undefined ? {} : { memcmp }),
          ...(standardWrite === undefined ? {} : { standardWrite }),
          osRuntimes,
          lanePointers,
          addressRoots,
          addressStorage,
          mutableStorage,
          suspensionRegions,
          lanesFor,
          valueLanesFor,
          laneType,
          readLocal,
          readScalar,
          coerceLane,
          locate,
          constantBytePointer,
          aggregateFieldOffset,
          emitHostFailure,
          materializeAddressRoot,
          ensureAddressRoot,
          reloadAddressRoot,
          reloadMutableRoots,
          storeMutable,
          dropThroughPlan,
          failurePayload,
          emitCallableBinary,
          emitIntegerConversion,
          callValues,
          emitOrigin,
          joinSuspensionOutcome,
          returnStep,
          getTrapBlock: () => trapBlock,
          setTrapBlock: (block: LlvmBlock.Block | undefined) => {
            trapBlock = block
          },
          getCheckOrdinal: () => checkOrdinal,
          setCheckOrdinal: (ordinal: number) => {
            checkOrdinal = ordinal
          },
        })
        for (const [blockOrdinal, block] of entry.linear.entries()) {
          const blockHandle = blocks.get(block.id.ordinal)
          if (blockHandle === undefined) continue
          yield* LlvmBlock.setInsertionPoint(body, blockHandle)
          if (blockOrdinal > 0) yield* reloadMutableRoots(`b${block.id.ordinal}`)
          for (const operation of block.operations) {
            yield* NativeOperation.emit(operationContext, operation)
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
          const controlContext: NativeControl.Context = Object.freeze({
            builder,
            body,
            i32,
            blocks,
            readScalar,
            readLocal,
          })
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
              yield* NativeControl.jump(controlContext, terminator)
              break
            }
            case 'Branch': {
              yield* NativeControl.branch(controlContext, terminator, blockOrdinal)
              break
            }
            case 'MatchBranch': {
              yield* NativeControl.matchBranch(controlContext, terminator, block.id.ordinal)
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
