import * as ByteString from '@silklang/llvm/ByteString'
import * as NativeForeignGuard from './NativeForeignGuard.js'
import * as NativeCAbi from './NativeCAbi.js'
import * as Bitcode from '@silklang/llvm/Bitcode'
import * as Builder from '@silklang/llvm/Builder'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionActor from '@silklang/llvm/Function'
import * as IrText from '@silklang/llvm/IrText'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import * as LlvmMetadata from '@silklang/llvm/Metadata'
import * as LlvmType from '@silklang/llvm/Type'
import * as Variable from '@silklang/llvm/Variable'
import * as Verify from '@silklang/llvm/Verify'
import * as Effect from 'effect/Effect'
import type {
  CodegenRequest,
  ForeignExport,
  ForeignImport,
  ForeignStatic,
  RuntimeFeature,
  SymbolEntry,
} from './Backend.js'
import {
  BackendError,
  formatModuleViolations,
  lineTable,
  sanitize,
  suspensionPointKey,
} from './Backend.js'
import * as CAbi from './CAbi.js'
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
import * as NativeExecutionOperation from './NativeExecutionOperation.js'
import type * as NativeForeignOperation from './NativeForeignOperation.js'
import * as NativeFunction from './NativeFunction.js'
import type * as NativeLanePointer from './NativeLanePointer.js'
import * as NativeOperation from './NativeOperation.js'
import * as NativeSuspension from './NativeSuspension.js'
import * as NativeTermination from './NativeTermination.js'
import * as NativeType from './NativeType.js'
import type * as Scalar from './Scalar.js'
import type * as Termination from './Termination.js'

export const emit = Effect.fn('NativeProgram.emit')(function* (
  program: Mir.Module,
  request: CodegenRequest,
): Effect.fn.Return<
  {
    /** Static host-report tables for the standalone adapter; empty for freestanding targets. */
    readonly report: Termination.Report
    readonly symbols: ReadonlyArray<SymbolEntry>
    readonly nativeRuntimeSymbols: ReadonlyArray<string>
    readonly runtimeFeatures: ReadonlyArray<RuntimeFeature>
    readonly foreignImports: ReadonlyArray<ForeignImport>
    readonly foreignExports: ReadonlyArray<ForeignExport>
    readonly foreignStatics: ReadonlyArray<ForeignStatic>
    /** Renders the textual IR on demand; most compiles never read it. */
    readonly renderIr: () => string
    readonly bitcode: Uint8Array
  },
  BackendError | LlvmError.LlvmError
> {
  const suspensionEnabled = program.functions.some((fn) => (fn.suspension?.regions.length ?? 0) > 0)
  // A retained execution can be destroyed without ever running its body. Its package still owns
  // the continuation chain and therefore needs frame cleanup even when no relay was emitted.
  const needsFrameCleanup = program.layout.executionPackages.plans.some(
    (plan) => plan.initialContinuationSegment,
  )
  const frameRuntimeEnabled = suspensionEnabled || needsFrameCleanup
  const runtimeFeatures = new Set<RuntimeFeature>()
  const builder = yield* Builder.make({
    sourceFilename: program.module,
    targetTriple: program.layout.target.id,
    strip: request.mode !== 'debug',
  })
  // Internal control/ABI values use i32 even when the selected source has no i32 declarations.
  const i32 = yield* LlvmType.integer(builder, 32)
  const usesScalar = (spelling: Scalar.Spelling): boolean =>
    program.layout.callingShapes.some((shape) => shape.lanes.some((lane) => lane.type === spelling))
  // LLVM assigns type-table identities in creation order, so preserve byte-for-byte output for
  // programs that do not use floating-point values by creating these types only when required.
  const f32 = usesScalar('f32') ? yield* LlvmType.float(builder) : i32
  const f64 = usesScalar('f64') ? yield* LlvmType.double(builder) : i32
  const usizeLayout = Layout.entry(program.layout, 'usize')
  let usizeType: LlvmType.Type | undefined
  if (usizeLayout?.representation._tag === 'UnsignedInteger') {
    usizeType = yield* LlvmType.integer(builder, usizeLayout.representation.bits)
  } else if (frameRuntimeEnabled) {
    usizeType = yield* LlvmType.integer(builder, program.layout.target.pointerSize * 8)
  } else {
    usizeType = undefined
  }
  const integerTypes = new Map<number, LlvmType.Type>([[32, i32]])
  if (usizeLayout?.representation._tag === 'UnsignedInteger' && usizeType !== undefined) {
    integerTypes.set(usizeLayout.representation.bits, usizeType)
  }
  for (const bits of [8, 16, 64] as const) {
    if (!integerTypes.has(bits)) integerTypes.set(bits, yield* LlvmType.integer(builder, bits))
  }
  const hasAddressLane =
    frameRuntimeEnabled ||
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
  const coroutineFramePop = frameRuntimeEnabled
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
    if (
      (operation._tag !== 'OsCall' && operation._tag !== 'OsOpen') ||
      osRuntimes.has(operation.operation.name)
    )
      continue
    const resultLanes = lanesFor(
      operation._tag === 'OsOpen' ? operation.handleType : operation.type,
    )
    const abi = operation._tag === 'OsOpen' ? 'OpenOut' : 'Direct'
    const singleResultLane = resultLanes.at(0)
    let resultType: LlvmType.Type
    if (abi === 'OpenOut') {
      resultType = i32
    } else if (resultLanes.length === 0) {
      resultType = voidType ?? (yield* LlvmType.voidType(builder))
    } else if (resultLanes.length === 1 && singleResultLane !== undefined) {
      resultType = laneType(singleResultLane)
    } else {
      resultType = yield* LlvmType.structure(builder, resultLanes.map(laneType))
    }
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
            abi === 'OpenOut' ? [...parameters, ...resultLanes.map(() => pointer)] : parameters,
          ),
        ),
        resultLaneCount: resultLanes.length,
      }),
    )
  }

  // Foreign symbols are declared once each under the default (C) calling convention with the
  // LLVM types the classified C signature selects; agreeing redeclarations share one entry.
  const foreignFunctions = new Map<string, NativeForeignOperation.Declaration>()
  const foreignStatics = new Map<string, NativeForeignOperation.StaticDeclaration>()
  const cType = (type: CAbi.CAbiType): LlvmType.Type | undefined => {
    switch (type._tag) {
      case 'Void':
        return undefined
      case 'Float':
        return type.bits === 32 ? f32 : f64
      case 'Integer':
        return integerTypes.get(type.bits)
      case 'Pointer':
      case 'FunctionPointer':
        return pointer
    }
  }
  const foreignGuard =
    program.foreignCalls.length === 0 ? undefined : yield* NativeForeignGuard.make(builder)
  for (const call of program.foreignCalls) {
    if (foreignFunctions.has(call.symbol)) continue
    const parameters = call.signature.parameters.map(cType)
    if (parameters.some((type) => type === undefined))
      throw new RangeError(`LLVM foreign function ${call.symbol} has a void parameter`)
    const attributes = yield* NativeCAbi.attributes(builder, call.signature)
    const handle = yield* FunctionActor.declare(
      builder,
      call.symbol,
      yield* LlvmType.functionType(
        builder,
        cType(call.signature.result) ?? voidType ?? (yield* LlvmType.voidType(builder)),
        parameters.flatMap((type) => (type === undefined ? [] : [type])),
      ),
      attributes === undefined ? {} : { attributes },
    ).pipe(
      Effect.mapError(
        (cause) =>
          new BackendError({
            operation: 'Backend.emit',
            backend: 'LLVM',
            message: `foreign function ${call.symbol} conflicts with the native runtime's own declaration of that symbol: ${cause.message}`,
            reason: { _tag: 'ForeignSymbolConflict', symbol: call.symbol },
          }),
      ),
    )
    if (foreignGuard === undefined) throw new RangeError('LLVM foreign guard was not initialized')
    const guarded = yield* NativeForeignGuard.wrap(
      foreignGuard,
      builder,
      handle,
      foreignFunctions.size,
      call.signature.parameters.length,
    )
    foreignFunctions.set(call.symbol, Object.freeze({ handle: guarded, signature: call.signature }))
  }
  for (const record of program.foreignStatics) {
    const classified = CAbi.classify(record.type, program.layout.target, 'Parameter')
    const valueType = cType(classified)
    if (valueType === undefined)
      throw new RangeError(`LLVM foreign static ${record.symbol} has a void type`)
    let initializer: Constant.Constant | undefined
    if (record.direction === 'Export' && record.literal?._tag === 'IntegerLiteral') {
      initializer =
        classified._tag === 'Integer' && classified.signed
          ? yield* Constant.integerSigned(builder, valueType, record.literal.value)
          : yield* Constant.integerUnsigned(builder, valueType, record.literal.value)
    } else if (record.direction === 'Export' && record.literal?._tag === 'FloatingLiteral') {
      initializer =
        classified._tag === 'Float' && classified.bits === 32
          ? yield* Constant.floatFromNumber(builder, valueType, Number(record.literal.spelling))
          : yield* Constant.doubleFromNumber(builder, valueType, Number(record.literal.spelling))
    }
    const variable = yield* Variable.make(builder, record.symbol, valueType, {
      ...(initializer === undefined ? {} : { initializer }),
      constant: record.direction === 'Export',
      linkage: 'external',
      externallyInitialized: record.direction === 'Import',
    }).pipe(
      Effect.mapError(
        (cause) =>
          new BackendError({
            operation: 'Backend.emit',
            backend: 'LLVM',
            message: `foreign static ${record.symbol} conflicts with another native symbol: ${cause.message}`,
            reason: { _tag: 'ForeignSymbolConflict', symbol: record.symbol },
          }),
      ),
    )
    foreignStatics.set(
      record.symbol,
      Object.freeze({
        address: yield* Constant.fromGlobal(builder, yield* Variable.global(builder, variable)),
        valueType,
      }),
    )
  }

  const functionDeclarations = yield* NativeDeclare.functions(
    Object.freeze({ builder, program, i32, pointer, lanesFor, laneType }),
  )
  const declared = functionDeclarations.declared
  const retained: Array<Constant.Constant> = []
  for (const root of program.retainedRoots ?? []) {
    const declaration = declared.find((candidate) => Mir.matchesInstanceKey(candidate.fn, root))
    if (declaration === undefined)
      return yield* new BackendError({
        operation: 'Backend.emit',
        backend: 'LLVM',
        message: 'Retained root has no emitted definition',
        reason: { _tag: 'InvalidMir', violations: MirVerification.verify(program) },
      })
    retained.push(
      yield* Constant.fromGlobal(
        builder,
        yield* FunctionActor.global(builder, declaration.driver ?? declaration.handle),
      ),
    )
  }
  if (retained.length > 0) {
    const array = yield* LlvmType.array(builder, yield* LlvmType.pointer(builder), retained.length)
    yield* Variable.make(builder, 'llvm.used', array, {
      initializer: yield* Constant.aggregate(builder, array, retained),
      linkage: 'appending',
      section: ByteString.fromString('llvm.metadata'),
    })
  }

  if (functionDeclarations.voidType !== undefined) voidType = functionDeclarations.voidType
  const executionRelease = yield* NativeExecutionOperation.declareReleaseHelper(
    builder,
    program,
    pointer,
    voidType,
  )
  const exportThunks = yield* NativeDeclare.exportThunks(
    Object.freeze({ builder, program, declared, cType }),
  )
  const foreignCallbacks = new Map<string, Constant.Constant>()
  for (const [symbol, thunk] of exportThunks)
    foreignCallbacks.set(
      symbol,
      yield* Constant.fromGlobal(builder, yield* FunctionActor.global(builder, thunk)),
    )
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
              { visibility: 'hidden' },
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
            { visibility: 'hidden' },
          ),
        }),
      )
    }
  }
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
  const termination = NativeTermination.make(builder, program, request, i32)
  yield* NativeFunction.emitBodies(
    Object.freeze({
      termination,
      runtimeFeatures,
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
      ...(executionRelease === undefined ? {} : { executionRelease: executionRelease.handle }),
      ...(memcmp === undefined ? {} : { memcmp }),
      ...(standardWrite === undefined ? {} : { standardWrite }),
      osRuntimes,
      foreignFunctions,
      foreignStatics,
      foreignCallbacks,
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

  if (executionRelease !== undefined)
    yield* NativeExecutionOperation.emitReleaseHelper(
      Object.freeze({
        builder,
        program,
        i8,
        i32,
        pointer,
        ...(usizeType === undefined ? {} : { usizeType }),
        ...(free === undefined ? {} : { free }),
        ...(coroutineFramePop === undefined ? {} : { coroutineFramePop }),
        resumeThunks,
        declared,
        types: typeContext,
        lanePointers,
        helper: executionRelease,
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
      ...(childThunkType === undefined ? {} : { childThunkType }),
      ...(resumeThunkType === undefined ? {} : { resumeThunkType }),
    }),
  )
  if (needsFrameCleanup || originThunks.size > 0 || resumeThunks.size > 0)
    runtimeFeatures.add('NestedSuspensionRuntime')

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
  const context = yield* Effect.context<never>()

  return {
    report: NativeTermination.report(termination),
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
      ...(coroutineFramePush === undefined ? [] : [CoroutineRuntime.pushSymbol]),
      ...(coroutineFramePop === undefined ? [] : [CoroutineRuntime.popSymbol]),
    ]),
    runtimeFeatures: Object.freeze([...runtimeFeatures].sort()),
    foreignImports: Object.freeze(
      [...foreignFunctions]
        .sort(([left], [right]) => left.localeCompare(right, 'en'))
        .map(([symbol, foreign]) =>
          Object.freeze({
            symbol,
            parameters: Object.freeze(foreign.signature.parameters.map(CAbi.typeText)),
            result: CAbi.typeText(foreign.signature.result),
            contract: foreign.signature.contract,
          }),
        ),
    ),
    foreignExports: Object.freeze(
      [...program.foreignExports]
        .sort((left, right) => left.symbol.localeCompare(right.symbol, 'en'))
        .map((record) =>
          Object.freeze({
            symbol: record.symbol,
            parameters: Object.freeze(record.signature.parameters.map(CAbi.typeText)),
            result: CAbi.typeText(record.signature.result),
            contract: record.signature.contract,
          }),
        ),
    ),
    foreignStatics: Object.freeze(
      [...program.foreignStatics]
        .sort(
          (left, right) =>
            left.symbol.localeCompare(right.symbol, 'en') ||
            left.direction.localeCompare(right.direction, 'en'),
        )
        .map((record) =>
          Object.freeze({
            symbol: record.symbol,
            type: CAbi.typeText(CAbi.classify(record.type, program.layout.target, 'Parameter')),
            direction: record.direction,
          }),
        ),
    ),
    renderIr: () => Effect.runSyncWith(context)(IrText.render(builder)),
    bitcode: yield* Bitcode.encode(builder),
  }
})
