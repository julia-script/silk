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
import * as Effect from 'effect/Effect'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Mir from './Mir.js'
import type * as SourceSpan from './SourceSpan.js'

/**
 * Code generation as a nominal `Backend` service: one operation consuming the whole
 * monomorphized MIR program plus the explicit target layout and codegen request, producing one
 * program artifact. The bootstrap `LlvmBackend` lowers MIR through the Silk LLVM builder and
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
  readonly symbols: ReadonlyArray<SymbolEntry>
  readonly bitcode: Uint8Array
  readonly ir: string
}

/** The nominal backend service contract. Consumers never inspect backend identity. */
export interface Backend {
  readonly emit: (
    program: Mir.Module,
    layout: Mir.TargetLayout,
    request: CodegenRequest,
  ) => Artifact
}

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

const emitProgram = (program: Mir.Module, layout: Mir.TargetLayout, request: CodegenRequest) =>
  Effect.gen(function* () {
    const builder = yield* Builder.make({
      sourceFilename: program.module,
      targetTriple: layout.triple,
      strip: request.mode !== 'debug',
    })
    const i32 = yield* LlvmType.integer(builder, 32)

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
  emit: (program: Mir.Module, layout: Mir.TargetLayout, request: CodegenRequest): Artifact => {
    const output = Effect.runSync(emitProgram(program, layout, request))
    return Object.freeze({
      _tag: 'BackendArtifact',
      module: program.module,
      symbols: Object.freeze(output.symbols),
      bitcode: output.bitcode,
      ir: output.ir,
    })
  },
})
