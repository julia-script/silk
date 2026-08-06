import * as Bitcode from '@silk-effect/llvm/Bitcode'
import * as LlvmBlock from '@silk-effect/llvm/Block'
import * as Builder from '@silk-effect/llvm/Builder'
import * as Constant from '@silk-effect/llvm/Constant'
import * as DISPFlags from '@silk-effect/llvm/DISPFlags'
import * as FunctionActor from '@silk-effect/llvm/Function'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import * as Intrinsic from '@silk-effect/llvm/Intrinsic'
import * as IrText from '@silk-effect/llvm/IrText'
import * as LlvmMetadata from '@silk-effect/llvm/Metadata'
import * as LlvmType from '@silk-effect/llvm/Type'
import * as Value from '@silk-effect/llvm/Value'
import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Target from './Target.js'

/**
 * Code generation as a nominal `Backend` service: one operation consuming the whole
 * monomorphized MIR program plus its compiler-owned target/layout plan and codegen request,
 * producing one program artifact. The bootstrap `LlvmBackend` lowers MIR through the Silk LLVM builder and
 * emits deterministic bitcode directly — no `libLLVM`, no LLVM C API, no compiler-private
 * native FFI. Textual LLVM IR is an implementation-specific inspection artifact.
 */

/** One codegen request. Debug builds emit native LLVM debug metadata. */
export interface CodegenRequest {
  readonly mode: 'debug' | 'release'
  readonly sources?: ReadonlyMap<string, Uint8Array>
}

/** One instance's deterministic native symbol. */
export interface SymbolEntry {
  readonly declaration: DeclarationIndex.CanonicalId
  readonly symbol: string
}

/** One backend-local control construct traced back to its canonical MIR region and source span. */
export interface ControlProvenance {
  readonly _tag: 'BackendControlProvenance'
  readonly backend: 'LLVM' | 'WebAssembly'
  readonly function: DeclarationIndex.CanonicalId
  readonly region: Mir.RegionId
  readonly construct:
    | 'LlvmJump'
    | 'LlvmBranch'
    | 'LlvmReturn'
    | 'LlvmTrap'
    | 'WasmIf'
    | 'WasmLoop'
    | 'WasmBr'
    | 'WasmReturn'
    | 'WasmTrap'
  readonly targets: ReadonlyArray<Mir.RegionId>
  readonly loop?: Mir.LoopId
  readonly span: SourceSpan.SourceSpan
}

/** The backend's program artifact: deterministic bitcode plus the IR inspection text. */
export interface Artifact {
  readonly _tag: 'BackendArtifact'
  readonly module: string
  readonly target: Target.Target
  readonly symbols: ReadonlyArray<SymbolEntry>
  readonly control: ReadonlyArray<ControlProvenance>
  readonly bitcode: Uint8Array
  readonly ir: string
}

/** An expected failure at the backend boundary. */
export class BackendError extends Data.TaggedError('BackendError')<{
  readonly operation: 'Backend.emit'
  readonly backend: string
  readonly message: string
  readonly reason:
    | { readonly _tag: 'InvalidMir'; readonly violations: ReadonlyArray<Mir.Violation> }
    | { readonly _tag: 'UnsupportedMir'; readonly detail: string }
    | { readonly _tag: 'UnsupportedTarget'; readonly target: Target.Id }
    | { readonly _tag: 'WrappedFailure'; readonly cause: unknown }
}> {}

/** The nominal backend service contract. */
export interface Backend {
  readonly _tag: 'Backend'
  readonly name: string
  readonly targets: ReadonlyArray<Target.Id>
  readonly emit: (
    program: Mir.Module,
    request: CodegenRequest,
  ) => Effect.Effect<Artifact, BackendError>
}

/** Validates shared MIR/target invariants before dispatching to one backend implementation. */
export const emit = Effect.fn('Backend.emit')(function* (
  self: Backend,
  program: Mir.Module,
  request: CodegenRequest,
): Effect.fn.Return<Artifact, BackendError> {
  const violations = Mir.verify(program)
  if (violations.length > 0) {
    return yield* new BackendError({
      operation: 'Backend.emit',
      backend: self.name,
      message: `${self.name} cannot emit invalid MIR`,
      reason: { _tag: 'InvalidMir', violations },
    })
  }
  if (!self.targets.includes(program.layout.target.id)) {
    return yield* new BackendError({
      operation: 'Backend.emit',
      backend: self.name,
      message: `${self.name} does not support target ${program.layout.target.id}`,
      reason: { _tag: 'UnsupportedTarget', target: program.layout.target.id },
    })
  }
  return yield* self.emit(program, request)
})

const sanitize = (name: string): string => name.replace(/[^A-Za-z0-9_]/g, '_')

/** The entry instance is always `silk_main`; later instances key on their discovery ordinal. */
export const symbolFor = (fn: Mir.MirFunction, ordinal: number): string =>
  ordinal === 0 ? 'silk_main' : `silk_${ordinal}_${sanitize(fn.id.name)}`

interface LineTable {
  readonly lineStarts: ReadonlyArray<number>
}

const lineTable = (bytes: Uint8Array | undefined): LineTable => {
  const lineStarts = [0]
  if (bytes !== undefined) {
    for (let index = 0; index < bytes.length; index += 1) {
      if (bytes[index] === 0x0a) lineStarts.push(index + 1)
    }
  }
  return { lineStarts }
}

const positionOf = (table: LineTable, offset: number): { line: number; column: number } => {
  let line = 0
  while (line + 1 < table.lineStarts.length && (table.lineStarts[line + 1] ?? 0) <= offset) {
    line += 1
  }
  return { line: line + 1, column: offset - (table.lineStarts[line] ?? 0) + 1 }
}

type LinearTerminator =
  | { readonly _tag: 'Return'; readonly value: Mir.LocalId; readonly provenance: Mir.Provenance }
  | { readonly _tag: 'Jump'; readonly target: Mir.RegionId; readonly provenance: Mir.Provenance }
  | {
      readonly _tag: 'Branch'
      readonly condition: Mir.LocalId
      readonly taken: Mir.RegionId
      readonly otherwise: Mir.RegionId
      readonly provenance: Mir.Provenance
    }
  | { readonly _tag: 'Trap'; readonly reason: string; readonly provenance: Mir.Provenance }

interface LinearBlock {
  readonly id: Mir.RegionId
  readonly kind: 'Normal' | 'Cleanup'
  readonly operations: ReadonlyArray<Mir.Operation>
  readonly terminator: LinearTerminator
}

const destinationOf = (operation: Mir.Operation): Mir.LocalId | undefined => {
  switch (operation._tag) {
    case 'Literal':
    case 'Binary':
    case 'Move':
    case 'ConvertUnion':
    case 'Call':
    case 'Construct':
    case 'ConstructArray':
    case 'Project':
    case 'ReadPlace':
      return operation.destination
    case 'CheckPlace':
    case 'WritePlace':
    case 'Drop':
      return undefined
  }
}

/** LLVM-private flattening of the compiler-owned DAG. Repeat is the only source of a back-edge. */
const linearize = (fn: Mir.MirFunction): ReadonlyArray<LinearBlock> => {
  const loops = new Map(
    fn.regions.flatMap((region) =>
      region._tag === 'LoopRegion' ? [[region.loop.ordinal, region] as const] : [],
    ),
  )
  const conditionOwners = new Map(
    fn.regions.flatMap((region) =>
      region._tag === 'LoopRegion' ? [[region.condition.ordinal, region] as const] : [],
    ),
  )
  const outcome = (region: Mir.OperationRegion | Mir.CleanupRegion): LinearTerminator => {
    const value = region.outcome
    switch (value._tag) {
      case 'Forward':
      case 'Return':
      case 'Trap':
        return value._tag === 'Forward'
          ? Object.freeze({ _tag: 'Jump', target: value.target, provenance: value.provenance })
          : value
      case 'Repeat': {
        const loop = loops.get(value.loop.ordinal)
        if (loop === undefined) throw new RangeError('LLVM linearizer lost repeat loop')
        return Object.freeze({ _tag: 'Jump', target: loop.id, provenance: value.provenance })
      }
      case 'Exit': {
        const loop = loops.get(value.loop.ordinal)
        if (loop === undefined) throw new RangeError('LLVM linearizer lost exit loop')
        return Object.freeze({ _tag: 'Jump', target: loop.following, provenance: value.provenance })
      }
      case 'Yield': {
        const loop = conditionOwners.get(region.id.ordinal)
        if (loop === undefined) throw new RangeError('LLVM linearizer found unowned yield')
        return Object.freeze({
          _tag: 'Branch',
          condition: loop.conditionValue,
          taken: loop.body,
          otherwise: loop.following,
          provenance: value.provenance,
        })
      }
    }
  }
  const raw = Mir.topologicalRegions(fn).map((region): LinearBlock => {
    if (region._tag === 'ConditionalRegion') {
      return Object.freeze({
        id: region.id,
        kind: 'Normal',
        operations: Object.freeze([]),
        terminator: Object.freeze({
          _tag: 'Branch',
          condition: region.condition,
          taken: region.taken,
          otherwise: region.otherwise,
          provenance: region.provenance,
        }),
      })
    }
    if (region._tag === 'LoopRegion') {
      return Object.freeze({
        id: region.id,
        kind: 'Normal',
        operations: Object.freeze([]),
        terminator: Object.freeze({
          _tag: 'Jump',
          target: region.condition,
          provenance: region.provenance,
        }),
      })
    }
    return Object.freeze({
      id: region.id,
      kind: region._tag === 'CleanupRegion' ? 'Cleanup' : 'Normal',
      operations: region._tag === 'CleanupRegion' ? region.releases : region.operations,
      terminator: outcome(region),
    })
  })
  const incoming = new Map<number, number>()
  for (const edge of Mir.controlEdges(fn)) {
    incoming.set(edge.to.ordinal, (incoming.get(edge.to.ordinal) ?? 0) + 1)
  }
  const byId = new Map(raw.map((block) => [block.id.ordinal, block] as const))
  const regionsById = new Map(fn.regions.map((region) => [region.id.ordinal, region] as const))
  const inlined = new Set<number>()
  const blocks = raw.map((block): LinearBlock => {
    let operations = [...block.operations]
    let terminator = block.terminator
    const seen = new Set<number>()
    while (terminator._tag === 'Jump' && !seen.has(terminator.target.ordinal)) {
      const target = byId.get(terminator.target.ordinal)
      const targetRegion = regionsById.get(terminator.target.ordinal)
      const inlineable =
        target !== undefined &&
        incoming.get(target.id.ordinal) === 1 &&
        (target.kind === 'Cleanup' || targetRegion?._tag === 'ConditionalRegion')
      if (!inlineable) break
      seen.add(target.id.ordinal)
      inlined.add(target.id.ordinal)
      operations = [...operations, ...target.operations]
      terminator = target.terminator
    }
    return Object.freeze({ ...block, operations: Object.freeze(operations), terminator })
  })
  return Object.freeze(blocks.filter((block) => !inlined.has(block.id.ordinal)))
}

const llvmControl = (program: Mir.Module): ReadonlyArray<ControlProvenance> =>
  Object.freeze(
    program.functions.flatMap((fn) =>
      linearize(fn).map((block): ControlProvenance => {
        const terminator = block.terminator
        return Object.freeze({
          _tag: 'BackendControlProvenance',
          backend: 'LLVM',
          function: fn.id,
          region: block.id,
          construct:
            terminator._tag === 'Jump'
              ? 'LlvmJump'
              : terminator._tag === 'Branch'
                ? 'LlvmBranch'
                : terminator._tag === 'Return'
                  ? 'LlvmReturn'
                  : 'LlvmTrap',
          targets:
            terminator._tag === 'Jump'
              ? Object.freeze([terminator.target])
              : terminator._tag === 'Branch'
                ? Object.freeze([terminator.taken, terminator.otherwise])
                : Object.freeze([]),
          span: terminator.provenance.span,
        })
      }),
    ),
  )

const functionStart = (fn: Mir.MirFunction): number => {
  const region = fn.regions.find((candidate) => candidate.id.ordinal === fn.entry.ordinal)
  if (region === undefined) return 0
  if (region._tag === 'ConditionalRegion' || region._tag === 'LoopRegion') {
    return region.provenance.span.start
  }
  return (
    (region._tag === 'OperationRegion' ? region.operations : region.releases).at(0)?.provenance.span
      .start ?? region.outcome.provenance.span.start
  )
}

const emitProgram = (program: Mir.Module, request: CodegenRequest) =>
  Effect.gen(function* () {
    const i32Layout = Layout.entry(program.layout, 'I32')
    if (i32Layout === undefined || i32Layout.representation._tag !== 'SignedInteger') {
      return yield* new BackendError({
        operation: 'Backend.emit',
        backend: 'LLVM',
        message: 'LLVM requires the planned I32 representation',
        reason: { _tag: 'InvalidMir', violations: Mir.verify(program) },
      })
    }
    const scalarBits = i32Layout.representation.bits
    if (
      program.layout.entries.some(
        (entry) =>
          (entry.representation._tag === 'SignedInteger' ||
            entry.representation._tag === 'Boolean') &&
          entry.representation.bits !== scalarBits,
      )
    ) {
      return yield* new BackendError({
        operation: 'Backend.emit',
        backend: 'LLVM',
        message: 'LLVM requires compatible planned scalar widths',
        reason: { _tag: 'InvalidMir', violations: Mir.verify(program) },
      })
    }
    const builder = yield* Builder.make({
      sourceFilename: program.module,
      targetTriple: program.layout.target.triple,
      strip: request.mode !== 'debug',
    })
    const i32 = yield* LlvmType.integer(builder, scalarBits)
    let voidType: LlvmType.Type | undefined
    const lanesFor = (type: Mir.Type): ReadonlyArray<Layout.CallingLane> => {
      const shape = Layout.callingShape(program.layout, Mir.semanticType(type))
      if (shape === undefined) {
        throw new RangeError(`LLVM backend lost calling shape for ${Mir.semanticType(type)}`)
      }
      return shape.lanes
    }
    let overflowSignature:
      | { readonly returnType: LlvmType.Type; readonly parameters: ReadonlyArray<LlvmType.Type> }
      | undefined

    const declared: Array<{
      readonly fn: Mir.MirFunction
      readonly symbol: string
      readonly handle: FunctionActor.Function
      readonly resultType: LlvmType.Type
      readonly resultLaneCount: number
      readonly linear: ReadonlyArray<LinearBlock>
    }> = []
    for (const [ordinal, fn] of program.functions.entries()) {
      const resultLaneCount = lanesFor(fn.result).length
      let resultType: LlvmType.Type
      if (resultLaneCount === 0) {
        const selected = voidType ?? (yield* LlvmType.voidType(builder))
        voidType = selected
        resultType = selected
      } else if (resultLaneCount === 1) {
        resultType = i32
      } else {
        resultType = yield* LlvmType.structure(
          builder,
          Array.from({ length: resultLaneCount }, () => i32),
        )
      }
      const parameters =
        fn.regions.length === 0
          ? []
          : fn.localTypes
              .slice(0, fn.parameterCount)
              .flatMap((type) => Array.from({ length: lanesFor(type).length }, () => i32))
      const signature = yield* LlvmType.functionType(builder, resultType, parameters)
      declared.push({
        fn,
        symbol: symbolFor(fn, ordinal),
        handle: yield* FunctionActor.declare(builder, symbolFor(fn, ordinal), signature),
        resultType,
        resultLaneCount,
        linear: linearize(fn),
      })
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

    for (const entry of declared) {
      let subprogram: LlvmMetadata.Optional
      if (debug && file !== undefined && compileUnit !== undefined) {
        const startLine = positionOf(table, functionStart(entry.fn)).line
        const symbolName = yield* LlvmMetadata.string(builder, entry.symbol)
        subprogram = yield* LlvmMetadata.subprogram(builder, file, symbolName, {
          line: startLine,
          scopeLine: startLine,
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
          let trapBlock: LlvmBlock.Block | undefined
          let checkOrdinal = 0
          const locals = new Map<number, ReadonlyArray<Value.Input>>()
          const mutableRoots = new Set(
            entry.linear.flatMap((block) =>
              block.operations.flatMap((operation) =>
                operation._tag === 'WritePlace' ? [operation.root.ordinal] : [],
              ),
            ),
          )
          const mutableStorage = new Map<number, ReadonlyArray<Value.Input>>()
          for (const root of [...mutableRoots].sort((left, right) => left - right)) {
            const logicalType = entry.fn.localTypes.at(root)
            if (logicalType === undefined)
              throw new RangeError(`Backend lost mutable root %${root}`)
            const storage: Array<Value.Input> = []
            for (let lane = 0; lane < lanesFor(logicalType).length; lane += 1) {
              storage.push(yield* FunctionBody.alloca(body, i32, `mut${root}_${lane}`))
            }
            mutableStorage.set(root, Object.freeze(storage))
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
            locals.set(ordinal, Object.freeze(values))
            const storage = mutableStorage.get(ordinal)
            if (storage !== undefined) {
              for (const [lane, pointer] of storage.entries()) {
                const stored = values.at(lane)
                if (stored !== undefined) yield* FunctionBody.store(body, stored, pointer)
              }
            }
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
          const locate = (span: SourceSpan.SourceSpan, instruction: unknown) =>
            Effect.gen(function* () {
              if (!debug || scope === undefined || instruction === undefined) return
              const position = positionOf(table, span.start)
              const location = yield* LlvmMetadata.location(
                builder,
                position.line,
                position.column,
                scope,
              )
              yield* FunctionBody.setDebugLocation(
                body,
                instruction as Parameters<typeof FunctionBody.setDebugLocation>[1],
                location,
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

          for (const [blockOrdinal, block] of entry.linear.entries()) {
            const blockHandle = blocks.get(block.id.ordinal)
            if (blockHandle === undefined) continue
            if (blockOrdinal > 0) yield* LlvmBlock.setInsertionPoint(body, blockHandle)
            if (blockOrdinal > 0) {
              for (const root of [...mutableRoots].sort((left, right) => left - right)) {
                const storage = mutableStorage.get(root)
                if (storage === undefined) continue
                const loaded: Array<Value.Input> = []
                for (const [lane, pointer] of storage.entries()) {
                  loaded.push(
                    yield* FunctionBody.load(body, i32, pointer, `mut${root}_${lane}_load`),
                  )
                }
                locals.set(root, Object.freeze(loaded))
              }
            }
            for (const operation of block.operations) {
              switch (operation._tag) {
                case 'Literal':
                  locals.set(
                    operation.destination.ordinal,
                    Object.freeze([
                      yield* Constant.integerSigned(builder, i32, BigInt(operation.value)),
                    ]),
                  )
                  break
                case 'Move':
                  locals.set(operation.destination.ordinal, readLocal(operation.source))
                  break
                case 'ConvertUnion': {
                  const source = readLocal(operation.source)
                  const targetWidth = operation.targetShape.laneCount
                  const zero = yield* Constant.integerSigned(builder, i32, 0n)
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
                    locals.set(
                      operation.destination.ordinal,
                      Object.freeze([
                        tag,
                        ...Array.from(
                          { length: Math.max(0, targetWidth - 1) },
                          (_, ordinal) => source.at(ordinal) ?? zero,
                        ),
                      ]),
                    )
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
                  locals.set(
                    operation.destination.ordinal,
                    Object.freeze([
                      tag,
                      ...Array.from(
                        { length: Math.max(0, targetWidth - 1) },
                        (_, ordinal) => source.at(ordinal + 1) ?? zero,
                      ),
                    ]),
                  )
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
                    const limit = yield* Constant.integerSigned(
                      builder,
                      i32,
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
                        const expected = yield* Constant.integerSigned(
                          builder,
                          i32,
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
                    const limit = yield* Constant.integerSigned(
                      builder,
                      i32,
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
                      const expected = yield* Constant.integerSigned(
                        builder,
                        i32,
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
                case 'Binary': {
                  const left = readScalar(operation.left)
                  const right = readScalar(operation.right)
                  const ordinal = checkOrdinal
                  checkOrdinal += 1
                  const comparisonPredicates: Readonly<
                    Partial<Record<Mir.BinaryOperator, 'eq' | 'ne' | 'slt' | 'sle' | 'sgt' | 'sge'>>
                  > = {
                    Equals: 'eq',
                    NotEquals: 'ne',
                    LessThan: 'slt',
                    LessOrEqual: 'sle',
                    GreaterThan: 'sgt',
                    GreaterOrEqual: 'sge',
                  }
                  const predicate = comparisonPredicates[operation.operator]
                  if (predicate !== undefined) {
                    const flag = yield* FunctionBody.integerCompare(
                      body,
                      predicate,
                      left,
                      right,
                      `cmp${ordinal}_flag`,
                    )
                    const widened = yield* FunctionBody.cast(
                      body,
                      'zext',
                      flag,
                      i32,
                      `cmp${ordinal}`,
                    )
                    const instruction = yield* Value.instruction(body, flag)
                    yield* locate(operation.provenance.span, instruction)
                    locals.set(operation.destination.ordinal, Object.freeze([widened]))
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
                        ? ('sadd.with.overflow' as const)
                        : operation.operator === 'Subtract'
                          ? ('ssub.with.overflow' as const)
                          : ('smul.with.overflow' as const)
                    if (overflowSignature === undefined) {
                      const i1 = yield* LlvmType.integer(builder, 1)
                      overflowSignature = Object.freeze({
                        returnType: yield* LlvmType.structure(builder, [i32, i1]),
                        parameters: Object.freeze([i32, i32]),
                      })
                    }
                    const pair = yield* Intrinsic.call(
                      body,
                      intrinsicId,
                      [i32],
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
                    yield* FunctionBody.conditionalBranch(
                      body,
                      overflowed,
                      trapBlock,
                      continueBlock,
                    )
                    yield* LlvmBlock.setInsertionPoint(body, continueBlock)
                    result = valuePart
                  } else {
                    const zero = yield* Constant.integerSigned(builder, i32, 0n)
                    const minimum = yield* Constant.integerSigned(builder, i32, -2147483648n)
                    const negativeOne = yield* Constant.integerSigned(builder, i32, -1n)
                    const zeroDivisor = yield* FunctionBody.integerCompare(
                      body,
                      'eq',
                      right,
                      zero,
                      `div${ordinal}_zero`,
                    )
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
                    const trapping = yield* FunctionBody.binary(
                      body,
                      'or',
                      zeroDivisor,
                      overflowCase,
                      `div${ordinal}_trapping`,
                    )
                    const continueBlock = yield* LlvmBlock.make(body, `div${ordinal}_ok`)
                    yield* FunctionBody.conditionalBranch(body, trapping, trapBlock, continueBlock)
                    yield* LlvmBlock.setInsertionPoint(body, continueBlock)
                    result = yield* FunctionBody.binary(
                      body,
                      operation.operator === 'Divide' ? 'sdiv' : 'srem',
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
                case 'Drop':
                  break
                case 'Call': {
                  const target = declared.find(
                    (candidate) =>
                      candidate.fn.id.module === operation.target.module &&
                      candidate.fn.id.name === operation.target.name,
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
            }
            const terminator = block.terminator
            switch (terminator._tag) {
              case 'Return': {
                const returned = readLocal(terminator.value)
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
                            'return_value',
                          ),
                        )
                yield* locate(terminator.provenance.span, instruction)
                break
              }
              case 'Jump': {
                const target = blocks.get(terminator.target.ordinal)
                if (target === undefined) throw new RangeError('Backend jump to missing block')
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
                const taken = blocks.get(terminator.taken.ordinal)
                const otherwise = blocks.get(terminator.otherwise.ordinal)
                if (taken === undefined || otherwise === undefined) {
                  throw new RangeError('Backend branch to missing block')
                }
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

    return {
      symbols: declared.map((entry) =>
        Object.freeze({ declaration: entry.fn.id, symbol: entry.symbol }),
      ),
      ir: yield* IrText.render(builder),
      bitcode: yield* Bitcode.encode(builder),
    }
  })

/** The bootstrap LLVM backend over the Silk LLVM builder. */
export const LlvmBackend: Backend = Object.freeze({
  _tag: 'Backend',
  name: 'LLVM',
  targets: Object.freeze(Target.native.map((target) => target.id)),
  emit: Effect.fn('Backend.LLVM.emit')(function* (
    program: Mir.Module,
    request: CodegenRequest,
  ): Effect.fn.Return<Artifact, BackendError> {
    const output = yield* emitProgram(program, request).pipe(
      Effect.mapError((cause) =>
        cause._tag === 'BackendError'
          ? cause
          : new BackendError({
              operation: 'Backend.emit',
              backend: 'LLVM',
              message: `LLVM emission failed for ${program.module}`,
              reason: { _tag: 'WrappedFailure', cause },
            }),
      ),
    )
    return Object.freeze({
      _tag: 'BackendArtifact',
      module: program.module,
      target: program.layout.target,
      symbols: Object.freeze(output.symbols),
      control: llvmControl(program),
      bitcode: output.bitcode,
      ir: output.ir,
    })
  }),
})
