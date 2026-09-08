import * as NativeDiagnosticOutcome from './NativeDiagnosticOutcome.js'
import * as NativeExecutionStorage from './NativeExecutionStorage.js'
import * as NativeAssemblyOperation from './NativeAssemblyOperation.js'
import * as Alignment from '@silklang/llvm/Alignment'
import * as LlvmBlock from '@silklang/llvm/Block'
import type * as Builder from '@silklang/llvm/Builder'
import * as Constant from '@silklang/llvm/Constant'
import * as DISPFlags from '@silklang/llvm/DISPFlags'
import * as FunctionActor from '@silklang/llvm/Function'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as Intrinsic from '@silklang/llvm/Intrinsic'
import * as LlvmMetadata from '@silklang/llvm/Metadata'
import type * as LlvmType from '@silklang/llvm/Type'
import * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import type * as Backend from './Backend.js'
import { type lineTable, positionOf, suspensionPointKey } from './Backend.js'
import * as Instances from './Instances.js'
import * as Layout from './Layout.js'
import * as LayoutVerify from './LayoutVerify.js'
import * as Mir from './Mir.js'
import { destinationOf, type LinearBlock, opensRuntimeContinuation } from './MirLinearization.js'
import type * as NativeAggregate from './NativeAggregate.js'
import type * as NativeArith from './NativeArith.js'
import type * as NativeCall from './NativeCall.js'
import * as NativeControl from './NativeControl.js'
import * as NativeDebug from './NativeDebug.js'
import type * as NativeForeignOperation from './NativeForeignOperation.js'
import type * as NativeHostFailure from './NativeHostFailure.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import type * as NativeLoweringContext from './NativeLoweringContext.js'
import * as NativeOperation from './NativeOperation.js'
import type * as NativeOperationContext from './NativeOperationContext.js'
import * as NativeStorage from './NativeStorage.js'
import type * as NativeSuspension from './NativeSuspension.js'
import * as NativeTermination from './NativeTermination.js'
import * as NativeType from './NativeType.js'
import * as NativeDiagnosticContext from './NativeDiagnosticContext.js'
import * as NativeDiagnosticScope from './NativeDiagnosticScope.js'

export interface MutableRoots {
  readonly mutable: ReadonlySet<number>
  readonly address: ReadonlySet<number>
}

/** Finds locals that need stable stack storage across mutation, calls, and suspension control. */
export const discoverRoots = (
  fn: Mir.MirFunction,
  blocks: ReadonlyArray<LinearBlock>,
): MutableRoots => {
  const assignments = new Map<number, number>()
  for (const operation of blocks.flatMap((block) => block.operations)) {
    const destination = destinationOf(operation)
    if (destination !== undefined)
      assignments.set(destination.ordinal, (assignments.get(destination.ordinal) ?? 0) + 1)
  }
  const continuationLocals = blocks.flatMap((block) => {
    if (
      block.terminator._tag === 'Return' ||
      block.terminator._tag === 'Trap' ||
      block.terminator._tag === 'PropagateEffectFailure'
    )
      return []
    let afterRuntimeContinuation = false
    const ordinals: Array<number> = []
    for (const operation of block.operations) {
      if (opensRuntimeContinuation(operation)) afterRuntimeContinuation = true
      if (!afterRuntimeContinuation) continue
      const destination = destinationOf(operation)
      if (destination !== undefined) ordinals.push(destination.ordinal)
    }
    return ordinals
  })
  const runtimeContinuationDestinations = blocks.flatMap((block) =>
    block.operations.flatMap((operation) => {
      if (!opensRuntimeContinuation(operation) || operation._tag === 'Binary') return []
      if (operation._tag === 'CatchEffect')
        return [
          operation.destination.ordinal,
          operation.successValue.ordinal,
          operation.failureValue.ordinal,
        ]
      const destination = destinationOf(operation)
      return destination === undefined ? [] : [destination.ordinal]
    }),
  )
  const borrowedCaptureRoots = new Set(
    blocks.flatMap((block) =>
      block.operations.flatMap((operation) =>
        operation._tag === 'MakeEffect' || operation._tag === 'MakeCallable'
          ? operation.captures.flatMap((capture, ordinal) =>
              (
                operation._tag === 'MakeEffect'
                  ? operation.type.environment.fields.at(ordinal)?.representation === 'Borrow'
                  : capture.access === 'Shared' || capture.access === 'Exclusive'
              )
                ? [capture.source.ordinal]
                : [],
            )
          : [],
      ),
    ),
  )
  const mutable = new Set([
    ...blocks.flatMap((block) =>
      block.operations.flatMap((operation) =>
        operation._tag === 'SetInitialized' ? [operation.flag.ordinal] : [],
      ),
    ),
    ...blocks.flatMap((block) =>
      block.operations.flatMap((operation) =>
        operation._tag === 'WritePlace' &&
        fn.localTypes.at(operation.root.ordinal)?._tag !== 'Slice'
          ? [operation.root.ordinal]
          : [],
      ),
    ),
    ...blocks.flatMap((block) =>
      block.operations.flatMap((operation) =>
        operation._tag === 'BindMatch' ? [operation.destination.ordinal] : [],
      ),
    ),
    ...[...assignments].flatMap(([ordinal, count]) => (count > 1 ? [ordinal] : [])),
    ...continuationLocals,
    ...runtimeContinuationDestinations,
    ...blocks.flatMap((block) =>
      block.operations.flatMap((operation) =>
        operation._tag === 'RunEffectComposite'
          ? [operation.outcome.ordinal, operation.destination.ordinal]
          : [],
      ),
    ),
    ...(fn.suspension?.regions ?? []).flatMap((region) =>
      region._tag === 'RunSuspendableEffectRegion' && region.relay.state !== undefined
        ? [
            region.operation.destination.ordinal,
            ...(region.operation._tag === 'ExecutionPark'
              ? []
              : [region.operation.outcome.ordinal]),
            ...region.relay.state.slots.map((slot) => slot.local.ordinal),
          ]
        : [],
    ),
    ...borrowedCaptureRoots,
  ])
  const address = new Set([
    ...blocks.flatMap((block) =>
      block.operations.flatMap((operation) =>
        operation._tag === 'EnterDiagnosticScope'
          ? [operation.state.ordinal, operation.observer.ordinal]
          : [],
      ),
    ),
    ...blocks.flatMap((block) =>
      block.operations.flatMap((operation) =>
        operation._tag === 'BeginLoan' &&
        (Mir.borrowsDescriptor(operation) ||
          (operation.sourceType._tag !== 'Slice' &&
            fn.localTypes.at(operation.root.ordinal)?._tag !== 'Reference'))
          ? [operation.root.ordinal]
          : [],
      ),
    ),
    ...borrowedCaptureRoots,
  ])
  for (const root of address) mutable.add(root)
  return Object.freeze({ mutable, address })
}

export interface EmissionContext {
  readonly runtimeFeatures: Set<Backend.RuntimeFeature>
  readonly builder: Builder.Builder
  readonly program: Mir.Module
  readonly request: Backend.CodegenRequest
  readonly i8: LlvmType.Type
  readonly i32: LlvmType.Type
  readonly f32: LlvmType.Type
  readonly f64: LlvmType.Type
  readonly pointer: LlvmType.Type
  readonly usizeType?: LlvmType.Type
  readonly integerTypes: Map<number, LlvmType.Type>
  readonly lanePointers: NativeLanePointer.Context
  readonly staticPointers: ReadonlyMap<string, Constant.Constant>
  readonly lanesFor: (type: Mir.Type) => ReadonlyArray<Layout.CallingLane>
  readonly valueLanesFor: (type: Mir.Type) => ReadonlyArray<Layout.CallingLane>
  readonly laneType: (lane: Layout.CallingLane) => LlvmType.Type
  readonly transferHeaderSize: number
  readonly transferResultOffset: number
  readonly transferStorageSize: number
  readonly childThunkType?: LlvmType.Type
  readonly resumeThunkType?: LlvmType.Type
  readonly signedOverflowSignatures: Map<
    number,
    { readonly returnType: LlvmType.Type; readonly parameters: ReadonlyArray<LlvmType.Type> }
  >
  readonly unsignedOverflowSignatures: Map<
    number,
    { readonly returnType: LlvmType.Type; readonly parameters: ReadonlyArray<LlvmType.Type> }
  >
  readonly malloc?: FunctionActor.Function
  readonly free?: FunctionActor.Function
  readonly executionStorage?: NativeExecutionStorage.NativeExecutionStorage
  readonly executionRelease?: FunctionActor.Function
  readonly memcmp?: FunctionActor.Function
  readonly foreignIndirects: ReadonlyMap<string, NativeForeignOperation.Declaration>
  readonly foreignFunctions: ReadonlyMap<string, NativeForeignOperation.Declaration>
  readonly foreignStatics: ReadonlyMap<string, NativeForeignOperation.StaticDeclaration>
  readonly foreignCallbacks: ReadonlyMap<string, Constant.Constant>
  readonly declared: ReadonlyArray<NativeLoweringContext.DeclaredFunction>
  readonly originThunks: ReadonlyMap<
    string,
    {
      readonly handle: FunctionActor.Function
      readonly region: Mir.SuspendEffectRegion
      readonly owner: NativeLoweringContext.DeclaredFunction
    }
  >
  readonly resumeThunks: ReadonlyMap<
    string,
    {
      readonly handle: FunctionActor.Function
      readonly region: Mir.RunSuspendableEffectRegion
      readonly owner: NativeLoweringContext.DeclaredFunction
      readonly frame: Mir.CoroutineFrameTargetLayout
      readonly layout: Mir.CoroutineFrameTargetStateLayout
    }
  >
  readonly debug: boolean
  readonly compileUnit: LlvmMetadata.Optional
  readonly file: LlvmMetadata.Optional
  readonly table: ReturnType<typeof lineTable>
  readonly debugContext: NativeDebug.LoweringContext
  readonly termination: NativeTermination.ModuleContext
}

/** Emits every declared native function body from explicit program lowering state. */
export const emitBodies = Effect.fnUntraced(function* (context: EmissionContext) {
  const {
    runtimeFeatures,
    builder,
    program,
    request,
    i8,
    i32,
    f32,
    f64,
    pointer,
    usizeType,
    integerTypes,
    lanePointers,
    staticPointers,
    lanesFor,
    valueLanesFor,
    laneType,
    transferHeaderSize,
    transferResultOffset,
    transferStorageSize,
    childThunkType,
    resumeThunkType,
    signedOverflowSignatures,
    unsignedOverflowSignatures,
    malloc,
    free,
    executionStorage,
    executionRelease,
    memcmp,
    foreignIndirects,
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
    termination,
  } = context
  for (const entry of declared) {
    let subprogram: LlvmMetadata.Optional
    if (debug && file !== undefined && compileUnit !== undefined) {
      const startLine = positionOf(table, NativeDebug.functionStart(entry.fn)).line
      const symbolName = yield* LlvmMetadata.string(builder, entry.symbol)
      const signatureTypes = yield* LlvmMetadata.tuple(builder, [
        yield* NativeDebug.typeOf(debugContext, entry.fn.result),
        ...(yield* Effect.forEach(entry.fn.localTypes.slice(0, entry.fn.parameterCount), (type) =>
          NativeDebug.typeOf(debugContext, type),
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
        if (entry.fn.machine !== undefined)
          return yield* NativeAssemblyOperation.emitNaked(
            builder,
            body,
            entry.fn,
            program.layout.target,
          )
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
        yield* LlvmBlock.make(body, 'entry')
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
        const operationState: NativeOperation.State = { trapBlocks: [], checkOrdinal: 0 }
        const diagnostic =
          entry.diagnosticParameter === undefined
            ? undefined
            : yield* NativeDiagnosticContext.make(
                builder,
                body,
                pointer,
                i8,
                integerTypes.get(program.layout.target.pointerSize * 8) ?? i32,
                yield* Value.argument(body, entry.diagnosticParameter),
                yield* Value.argument(body, entry.diagnosticParameter + 1),
              )
        // A transfer-only runner can use the suspension ABI without retaining a
        // resumable frame. Its outcome slots belong to this native invocation;
        // child results escape through the separate transfer result record.
        if (diagnostic !== undefined && invocationFrameStorage === undefined) {
          for (const outcome of Mir.diagnosticOutcomeLocals(program, entry.fn)) {
            const slot = Object.freeze({
              storage: yield* FunctionBody.alloca(
                body,
                diagnostic.causeType,
                `diagnostic_outcome${outcome.ordinal}`,
              ),
            })
            yield* NativeDiagnosticOutcome.initialize(slot, diagnostic)
            diagnostic.outcomes.set(outcome.ordinal, slot)
          }
        }
        const terminationContext: NativeTermination.FunctionContext = Object.freeze({
          module: termination,
          body,
          fn: entry.fn,
          state: operationState,
          ...(diagnostic === undefined ? {} : { diagnostic }),
        })
        const locals = new Map<number, ReadonlyArray<Value.Input>>()
        const roots = discoverRoots(entry.fn, entry.linear)
        const mutableRoots = roots.mutable
        const addressRoots = roots.address
        const mutableStorage = new Map<number, ReadonlyArray<Value.Input>>()
        for (const root of [...mutableRoots].sort((left, right) => left - right)) {
          const logicalType = entry.fn.localTypes.at(root)
          if (logicalType === undefined) throw new RangeError(`Backend lost mutable root %${root}`)
          if (logicalType._tag === 'EnvironmentBorrow') continue
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
        const nativeTypes: NativeType.LoweringContext = Object.freeze({
          program,
          i32,
          f32,
          f64,
          pointer,
          integerTypes,
        })
        const addressStorage = new Map<number, Value.Input>()
        for (const root of [...addressRoots].sort((left, right) => left - right)) {
          const logicalType = entry.fn.localTypes.at(root)
          const layout =
            logicalType === undefined
              ? undefined
              : NativeType.addressLayout(program.layout, logicalType)
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

        const storageContext: NativeStorage.Context = Object.freeze({
          builder: loweringContext.builder,
          body: loweringContext.body,
          byteType: i8,
          offsetType: i32,
          fn: loweringContext.entry.fn,
          layout: loweringContext.layout,
          mutableRoots,
          mutableStorage: loweringContext.mutableStorage,
          addressRoots,
          addressStorage,
          locals,
          types: nativeTypes,
          lanePointers,
          sequences: { materialize: 0, reload: 0 },
        })
        for (const root of [...addressRoots].sort((left, right) => left - right)) {
          const logicalType = entry.fn.localTypes.at(root)
          if (logicalType === undefined) throw new RangeError(`Backend lost address root %${root}`)
          yield* NativeStorage.storeAddressValues(
            storageContext,
            root,
            Object.freeze(
              yield* Effect.forEach(valueLanesFor(logicalType), (lane) =>
                Constant.nullValue(builder, laneType(lane)),
              ),
            ),
            `addr${root}_zero`,
          )
        }
        const diagnosticScopes =
          diagnostic === undefined
            ? new Map<number, NativeDiagnosticScope.NativeDiagnosticScope>()
            : yield* NativeDiagnosticScope.prepare({
                builder,
                body,
                entry,
                declared,
                diagnostic,
                program,
                types: nativeTypes,
                lanePointers,
              })
        if (entry.suspendable && diagnosticScopes.size > 0 && coroutineFrame === undefined)
          throw new RangeError('Suspended diagnostic scope lost its persistent frame')
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
          if (logicalType._tag === 'EnvironmentBorrow') {
            const base = values.at(0)
            if (base === undefined)
              throw new RangeError(`Backend lost environment borrow %${ordinal}`)
            addressStorage.set(ordinal, base)
            const storage: Array<Value.Input> = []
            const loaded: Array<Value.Input> = []
            for (const [lane, callingLane] of valueLanesFor(logicalType).entries()) {
              const offset = LayoutVerify.laneOffset(
                program.layout,
                logicalType.type,
                callingLane.path,
              )
              if (offset === undefined)
                throw new RangeError(`Backend lost environment borrow lane ${lane}`)
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
            yield* NativeStorage.storeAddressValues(
              storageContext,
              ordinal,
              Object.freeze(values),
              `addr${ordinal}_param`,
            )
          }
        }
        if (entry.diagnosticParameter !== undefined) physicalParameter += 2
        const transferPointer = entry.suspendable
          ? yield* Value.argument(body, physicalParameter)
          : undefined
        const resumeFrame = entry.suspendable
          ? yield* Value.argument(body, physicalParameter + 1)
          : undefined
        const resumePath = entry.suspendable
          ? yield* Value.argument(body, physicalParameter + 2)
          : undefined
        const initialBlock = blocks.get(entry.fn.entry.ordinal)
        if (initialBlock === undefined) throw new RangeError('Native function has no entry block')
        yield* FunctionBody.branch(body, dispatchBlock ?? initialBlock)
        if (entry.suspendable) {
          const entryBlock = blocks.get(entry.fn.entry.ordinal)
          if (
            dispatchBlock === undefined ||
            resumeFrame === undefined ||
            resumePath === undefined ||
            entryBlock === undefined
          )
            throw new RangeError('LLVM suspension dispatch lost its entry state')
          yield* LlvmBlock.setInsertionPoint(body, dispatchBlock)
          if (coroutineFrame !== undefined) {
            if (
              invocationFrameStorage === undefined ||
              executionStorage === undefined ||
              transferPointer === undefined ||
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
            const state = yield* NativeExecutionStorage.ensure(
              { builder, body, pointer, usizeType, storage: executionStorage },
              yield* NativeLanePointer.lanePointer(
                lanePointers,
                body,
                transferPointer,
                NativeExecutionStorage.stateOffset(program.layout.target.pointerSize),
                'suspend_storage_slot',
              ),
              'suspend_storage',
            )
            const allocatedFrame = yield* NativeExecutionStorage.invoke(
              { builder, body, pointer, storage: executionStorage },
              'acquire',
              [
                state,
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
            yield* FunctionBody.store(body, allocatedFrame, invocationFrameStorage)
            yield* FunctionBody.conditionalBranch(body, exhausted, frameTrapBlock, framePushedBlock)
            yield* LlvmBlock.setInsertionPoint(body, frameTrapBlock)
            yield* Intrinsic.call(body, 'trap', [], [])
            yield* FunctionBody.unreachable(body)
            yield* LlvmBlock.setInsertionPoint(body, framePushedBlock)
            const pushedFrame = yield* FunctionBody.load(
              body,
              pointer,
              invocationFrameStorage,
              'suspend_pushed_frame',
            )
            if (diagnostic !== undefined) {
              for (const field of coroutineFrame.diagnosticOutcomes) {
                yield* NativeDiagnosticOutcome.initialize(
                  {
                    storage: yield* NativeLanePointer.lanePointer(
                      lanePointers,
                      body,
                      pushedFrame,
                      field.offset,
                      `suspend_initialize_outcome${field.outcome.ordinal}`,
                    ),
                  },
                  diagnostic,
                )
              }
            }
            for (const [root, field] of stableAddressFields) {
              if (root >= entry.fn.parameterCount) continue
              const logicalType = entry.fn.localTypes.at(root)
              const values = locals.get(root)
              if (logicalType === undefined || values === undefined)
                throw new RangeError(`LLVM coroutine frame lost parameter root %${root}`)
              const base = yield* FunctionBody.getElementPtr(
                body,
                i8,
                pushedFrame,
                [yield* Constant.integerUnsigned(builder, i32, BigInt(field.offset))],
                `suspend_parameter_root${root}`,
              )
              for (const [ordinal, lane] of valueLanesFor(logicalType).entries()) {
                const offset = NativeType.addressLaneOffset(
                  program.layout,
                  logicalType,
                  lane,
                  ordinal,
                )
                const value = values.at(ordinal)
                if (offset === undefined || value === undefined)
                  throw new RangeError(
                    `LLVM coroutine parameter root %${root} lost lane ${ordinal} in ${entry.fn.id.module}.${entry.fn.id.name}: offset=${offset}, values=${values.length}, type=${logicalType._tag}`,
                  )
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
            if (diagnostic !== undefined) {
              for (const field of coroutineFrame.diagnosticOutcomes) {
                diagnostic.outcomes.set(
                  field.outcome.ordinal,
                  Object.freeze({
                    storage: yield* NativeLanePointer.lanePointer(
                      lanePointers,
                      body,
                      invocationFrame,
                      field.offset,
                      `suspend_diagnostic_outcome${field.outcome.ordinal}`,
                    ),
                  }),
                )
              }
            }
            for (const field of coroutineFrame.diagnosticScopes) {
              const scope = diagnosticScopes.get(field.scope.ordinal)
              if (scope === undefined)
                throw new RangeError('Persistent diagnostic descriptor lost its scope')
              diagnosticScopes.set(
                field.scope.ordinal,
                Object.freeze({
                  ...scope,
                  record: yield* NativeLanePointer.lanePointer(
                    lanePointers,
                    body,
                    invocationFrame,
                    field.offset,
                    `suspend_diagnostic_scope${field.scope.ordinal}`,
                  ),
                }),
              )
            }
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
        const debugLocation: NativeDebug.LocationContext = Object.freeze({
          builder,
          body,
          enabled: debug,
          scope,
          table,
        })
        const laneContext: NativeArith.LaneContext = Object.freeze({
          body: loweringContext.body,
          pointerBits: loweringContext.layout.target.pointerSize === 4 ? 32 : 64,
          i32: loweringContext.types.i32,
          integerTypes: loweringContext.types.integers,
          types: nativeTypes,
        })
        const failureContext: NativeAggregate.FailureContext = Object.freeze({
          builder,
          body,
          program,
          i32,
          types: nativeTypes,
          arith: laneContext,
        })

        const suspensionReturnContext: NativeSuspension.ReturnContext = Object.freeze({
          builder,
          body,
          i32,
          pointer,
          entry,
          lanePointers,
          ...(transferPointer === undefined ? {} : { transferPointer }),
          ...(invocationFrameStorage === undefined ? {} : { invocationFrameStorage }),
          ...(executionStorage === undefined ? {} : { executionStorage }),
          types: nativeTypes,
          ...(diagnostic === undefined ? {} : { diagnostic }),
        })
        const hostFailureContext: NativeHostFailure.Context = Object.freeze({
          builder,
          body,
          entry,
          types: nativeTypes,
          suspension: suspensionReturnContext,
          termination: terminationContext,
        })
        const synchronousCallContext: NativeCall.SynchronousContext = Object.freeze({
          body,
          storage: storageContext,
          ...(diagnostic === undefined ? {} : { diagnostic }),
        })
        const arithContext: NativeArith.OperationContext = Object.freeze({
          builder,
          body,
          program,
          i32,
          integerTypes,
          signedOverflowSignatures,
          unsignedOverflowSignatures,
          lane: laneContext,
          types: nativeTypes,
          debug: debugLocation,
          termination: terminationContext,
        })
        const suspensionContext: NativeSuspension.OperationContext = Object.freeze({
          ...(diagnostic === undefined ? {} : { diagnostic }),
          builder,
          body,
          program,
          i8,
          i32,
          pointer,
          entry,
          lanePointers,
          types: nativeTypes,
          transferHeaderSize,
          transferResultOffset,
          ...(transferPointer === undefined ? {} : { transferPointer }),
          ...(resumeFrame === undefined ? {} : { resumeFrame }),
          originThunks,
          resumeThunks,
          suspensionRegions,
          resumeBlocks,
          storage: storageContext,
          returns: suspensionReturnContext,
        })
        const callContext: NativeCall.Context = Object.freeze({
          builder,
          body,
          program,
          i8,
          i32,
          pointer,
          entry,
          ...(transferPointer === undefined ? {} : { transferPointer }),
          ...(invocationFrameStorage === undefined ? {} : { invocationFrameStorage }),
          resumeThunks,
          lanePointers,
          types: nativeTypes,
          storage: storageContext,
          synchronous: synchronousCallContext,
          returns: suspensionReturnContext,
        })

        const cleanupContext: NativeAggregate.Context = Object.freeze({
          builder,
          body,
          program,
          i8,
          i32,
          pointer,
          ...(usizeType === undefined ? {} : { usizeType }),
          ...(free === undefined ? {} : { free }),
          ...(executionStorage === undefined ? {} : { executionStorage }),
          ...(executionRelease === undefined ? {} : { executionRelease }),
          declared,
          resumeThunks,
          types: nativeTypes,
          lanePointers,
          call: callContext,
          arith: laneContext,
          storage: storageContext,
        })

        const actorContext: NativeOperationContext.Context = Object.freeze({
          diagnosticScopes,
          runtimeFeatures,
          builder,
          body,
          program,
          entry,
          declared,
          staticPointers,
          i32,
          f32,
          f64,
          pointer,
          transferStorageSize,
          ...(childThunkType === undefined ? {} : { childThunkType }),
          ...(resumeThunkType === undefined ? {} : { resumeThunkType }),
          ...(usizeType === undefined ? {} : { usizeType }),
          integerTypes,
          signedOverflowSignatures,
          unsignedOverflowSignatures,
          ...(malloc === undefined ? {} : { malloc }),
          ...(free === undefined ? {} : { free }),
          ...(memcmp === undefined ? {} : { memcmp }),
          foreignIndirects,
          foreignFunctions,
          foreignStatics,
          foreignCallbacks,
          lanePointers,
          suspensionRegions,
          types: nativeTypes,
          storage: storageContext,
          debug: debugLocation,
          hostFailure: hostFailureContext,
          cleanup: cleanupContext,
          failure: failureContext,
          arith: arithContext,
          call: callContext,
          suspension: suspensionContext,
          termination: terminationContext,
          state: operationState,
        })
        const operationContext: NativeOperation.LoweringContext = Object.freeze({
          value: actorContext,
          memory: actorContext,
          place: actorContext,
          scalar: actorContext,
          effect: actorContext,
          execution: actorContext,
          call: actorContext,
        })
        for (const [blockOrdinal, block] of entry.linear.entries()) {
          const blockHandle = blocks.get(block.id.ordinal)
          if (blockHandle === undefined) continue
          yield* LlvmBlock.setInsertionPoint(body, blockHandle)
          if (diagnostic !== undefined) {
            const recovered = block.recoveryOutcomes?.at(-1)
            const slot =
              recovered === undefined ? undefined : diagnostic.outcomes.get(recovered.ordinal)
            if (recovered !== undefined && slot === undefined)
              throw new RangeError('Selected recovery lost its diagnostic outcome storage')
            let cause =
              block.recoveryBoundary === undefined
                ? diagnostic.incomingCause
                : yield* Constant.nullValue(builder, diagnostic.causeType)
            if (slot !== undefined) cause = yield* NativeDiagnosticOutcome.borrow(slot, diagnostic)
            yield* FunctionBody.store(body, cause, diagnostic.cause)
          }
          if (blockOrdinal > 0)
            yield* NativeStorage.reloadRoots(storageContext, `b${block.id.ordinal}`)
          for (const operation of block.operations) {
            yield* NativeOperation.emit(operationContext, operation)
            const destination = destinationOf(operation)
            if (destination !== undefined && mutableRoots.has(destination.ordinal)) {
              yield* NativeStorage.storeMutable(
                storageContext,
                destination,
                NativeStorage.readLocal(storageContext, destination),
              )
            }
            if (destination !== undefined && addressRoots.has(destination.ordinal)) {
              yield* NativeStorage.storeAddressValues(
                storageContext,
                destination.ordinal,
                NativeStorage.readLocal(storageContext, destination),
                `addr${destination.ordinal}_defined`,
              )
            }
            if (diagnostic?.sourceState.dirty === true) {
              yield* NativeStorage.reloadAddressRoots(storageContext)
              diagnostic.sourceState.dirty = false
            }
          }
          const terminalAssembly = block.operations.at(-1)
          if (terminalAssembly?._tag === 'NativeAssembly' && terminalAssembly.assembly.noReturn) {
            yield* FunctionBody.unreachable(body)
            continue
          }
          yield* NativeControl.emit(
            Object.freeze({
              builder,
              body,
              i32,
              types: nativeTypes,
              blocks,
              locals,
              entry,
              cleanup: cleanupContext,
              failure: failureContext,
              suspension: suspensionReturnContext,
              debug: debugLocation,
              termination: terminationContext,
            }),
            block.terminator,
            blockOrdinal,
            block.id,
          )
        }

        yield* NativeTermination.emitTrapBlocks(terminationContext)
      }),
    )
  }
})
