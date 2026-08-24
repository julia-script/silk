import * as Bitcode from '@silk-effect/llvm/Bitcode'
import * as Builder from '@silk-effect/llvm/Builder'
import * as Constant from '@silk-effect/llvm/Constant'
import * as FunctionActor from '@silk-effect/llvm/Function'
import * as IrText from '@silk-effect/llvm/IrText'
import type * as LlvmError from '@silk-effect/llvm/LlvmError'
import * as LlvmMetadata from '@silk-effect/llvm/Metadata'
import * as LlvmType from '@silk-effect/llvm/Type'
import * as Variable from '@silk-effect/llvm/Variable'
import * as Verify from '@silk-effect/llvm/Verify'
import * as Effect from 'effect/Effect'
import type { CodegenRequest, SymbolEntry } from './Backend.js'
import {
  BackendError,
  formatModuleViolations,
  lineTable,
  sanitize,
  suspensionPointKey,
} from './Backend.js'
import * as CoroutineFrame from './CoroutineFrame.js'
import * as CoroutineRuntime from './CoroutineRuntime.js'
import * as Instances from './Instances.js'
import { alignUp } from './internal/Align.js'
import * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import * as MirVerification from './MirVerification.js'
import * as NativeCall from './NativeCall.js'
import type * as NativeDebug from './NativeDebug.js'
import * as NativeDeclare from './NativeDeclare.js'
import * as NativeFunction from './NativeFunction.js'
import type * as NativeLanePointer from './NativeLanePointer.js'
import * as NativeOperation from './NativeOperation.js'
import * as NativeSuspension from './NativeSuspension.js'
import * as NativeType from './NativeType.js'
import type * as Scalar from './Scalar.js'

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
      reason: { _tag: 'InvalidMir', violations: MirVerification.verify(program) },
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
      MirVerification.operations(fn).some(
        (operation) =>
          operation._tag === 'Allocate' ||
          operation._tag === 'RawBufferFrom' ||
          operation._tag === 'SharedFromAllocation' ||
          operation._tag === 'SharedClone' ||
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
  // The fourth private word identifies the independently driven Execution owner.
  const transferHeaderSize = program.layout.target.pointerSize * 4
  const originArgumentLanes = program.functions.flatMap((fn) =>
    (fn.suspension?.regions ?? []).flatMap((region) =>
      region._tag === 'SuspendEffectRegion'
        ? [
            NativeSuspension.logicalLanes(
              fn,
              NativeCall.operationInputs(region.operation),
              typeContext,
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
    MirVerification.operations(fn).some(NativeOperation.needsAllocation),
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
    MirVerification.operations(fn).some((operation) => operation._tag === 'StringEqualsExact'),
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
    MirVerification.operations(fn).some((operation) => operation._tag === 'HostWrite'),
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
  for (const operation of program.functions.flatMap((fn) => MirVerification.operations(fn))) {
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
        .find((fn) => MirVerification.operations(fn).includes(operation))
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
  yield* NativeFunction.emitBodies(
    Object.freeze({
      builder,
      program,
      request,
      i8,
      i32,
      f32,
      f64,
      pointer,
      ...(usizeType === undefined ? {} : { usizeType }),
      integerTypes,
      lanePointers,
      staticPointers,
      lanesFor,
      valueLanesFor,
      laneType,
      transferHeaderSize,
      transferResultOffset,
      transferStorageSize,
      ...(childThunkType === undefined ? {} : { childThunkType }),
      ...(resumeThunkType === undefined ? {} : { resumeThunkType }),
      signedOverflowSignatures,
      unsignedOverflowSignatures,
      ...(malloc === undefined ? {} : { malloc }),
      ...(free === undefined ? {} : { free }),
      ...(coroutineFramePush === undefined ? {} : { coroutineFramePush }),
      ...(coroutineFramePop === undefined ? {} : { coroutineFramePop }),
      ...(memcmp === undefined ? {} : { memcmp }),
      ...(standardWrite === undefined ? {} : { standardWrite }),
      osRuntimes,
      declared,
      originThunks,
      resumeThunks,
      debug,
      compileUnit,
      file,
      table,
      debugContext,
    }),
  )

  yield* NativeSuspension.emitThunks(
    Object.freeze({
      builder,
      program,
      i8,
      i32,
      pointer,
      ...(usizeType === undefined ? {} : { usizeType }),
      lanePointers,
      declared,
      originThunks,
      resumeThunks,
      types: typeContext,
      transferHeaderSize,
      transferResultOffset,
      transferStorageSize,
      ...(driver === undefined ? {} : { driver }),
      ...(machine === undefined ? {} : { machine }),
      ...(childThunkType === undefined ? {} : { childThunkType }),
      ...(resumeThunkType === undefined ? {} : { resumeThunkType }),
    }),
  )

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
