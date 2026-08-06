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

/** The backend's program artifact: deterministic bitcode plus the IR inspection text. */
export interface Artifact {
  readonly _tag: 'BackendArtifact'
  readonly module: string
  readonly target: Target.Target
  readonly symbols: ReadonlyArray<SymbolEntry>
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
          entry.representation._tag === 'Aggregate' || entry.representation.bits !== scalarBits,
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
    let overflowSignature:
      | { readonly returnType: LlvmType.Type; readonly parameters: ReadonlyArray<LlvmType.Type> }
      | undefined

    const declared: Array<{
      readonly fn: Mir.MirFunction
      readonly symbol: string
      readonly handle: FunctionActor.Function
    }> = []
    for (const [ordinal, fn] of program.functions.entries()) {
      const signature = yield* LlvmType.functionType(
        builder,
        i32,
        fn.blocks.length === 0 ? [] : Array.from({ length: fn.parameterCount }, () => i32),
      )
      declared.push({
        fn,
        symbol: symbolFor(fn, ordinal),
        handle: yield* FunctionActor.declare(builder, symbolFor(fn, ordinal), signature),
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
        const startLine = positionOf(
          table,
          entry.fn.blocks.at(0)?.terminator.provenance.span.start ?? 0,
        ).line
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
          const blocks = []
          for (const block of entry.fn.blocks) {
            blocks.push(
              yield* LlvmBlock.make(
                body,
                `bb${block.id.ordinal}${block.kind === 'Cleanup' ? '_cleanup' : ''}`,
              ),
            )
          }
          let trapBlock: LlvmBlock.Block | undefined
          let checkOrdinal = 0
          const locals = new Map<number, Value.Input>()
          for (let ordinal = 0; ordinal < entry.fn.parameterCount; ordinal += 1) {
            locals.set(ordinal, yield* Value.argument(body, ordinal))
          }
          const readLocal = (local: Mir.LocalId): Value.Input => {
            const found = locals.get(local.ordinal)
            if (found === undefined) {
              throw new RangeError(`Backend read undefined local %${local.ordinal}`)
            }
            return found
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

          for (const [blockOrdinal, block] of entry.fn.blocks.entries()) {
            const blockHandle = blocks[blockOrdinal]
            if (blockHandle === undefined) continue
            if (blockOrdinal > 0) yield* LlvmBlock.setInsertionPoint(body, blockHandle)
            for (const operation of block.operations) {
              switch (operation._tag) {
                case 'Literal':
                  locals.set(
                    operation.destination.ordinal,
                    yield* Constant.integerSigned(builder, i32, BigInt(operation.value)),
                  )
                  break
                case 'Move':
                  locals.set(operation.destination.ordinal, readLocal(operation.source))
                  break
                case 'Binary': {
                  const left = readLocal(operation.left)
                  const right = readLocal(operation.right)
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
                    locals.set(operation.destination.ordinal, widened)
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
                  locals.set(operation.destination.ordinal, result)
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
                    operation.arguments.map(readLocal),
                    `t${operation.destination.ordinal}`,
                  )
                  if (result === undefined) {
                    throw new RangeError('Backend call produced no value')
                  }
                  const instruction = yield* Value.instruction(body, result)
                  yield* locate(operation.provenance.span, instruction)
                  locals.set(operation.destination.ordinal, result)
                  break
                }
              }
            }
            const terminator = block.terminator
            switch (terminator._tag) {
              case 'Return': {
                const instruction = yield* FunctionBody.returnValue(
                  body,
                  readLocal(terminator.value),
                )
                yield* locate(terminator.provenance.span, instruction)
                break
              }
              case 'Jump': {
                const target = blocks[terminator.target.ordinal]
                if (target === undefined) throw new RangeError('Backend jump to missing block')
                yield* FunctionBody.branch(body, target)
                break
              }
              case 'Branch': {
                const zero = yield* Constant.integerSigned(builder, i32, 0n)
                const condition = yield* FunctionBody.integerCompare(
                  body,
                  'ne',
                  readLocal(terminator.condition),
                  zero,
                  `c${blockOrdinal}`,
                )
                const taken = blocks[terminator.taken.ordinal]
                const otherwise = blocks[terminator.otherwise.ordinal]
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
      bitcode: output.bitcode,
      ir: output.ir,
    })
  }),
})
