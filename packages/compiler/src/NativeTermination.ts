import * as NativeDiagnosticText from './NativeDiagnosticText.js'
import * as NativeDiagnosticOutcome from './NativeDiagnosticOutcome.js'
import * as LlvmBlock from '@silklang/llvm/Block'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as Intrinsic from '@silklang/llvm/Intrinsic'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import * as Effect from 'effect/Effect'
import { type CodegenRequest, type LineTable, symbolFor } from './Backend.js'
import type * as Mir from './Mir.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'
import * as NativeDiagnosticContext from './NativeDiagnosticContext.js'

/** Source line lookup and semantic trap sites used while emitting lexical diagnostics. */
export interface ModuleContext {
  readonly trapSites: Array<{ readonly reason: string; readonly origin: string }>
  readonly tables: Map<string, LineTable>
  readonly sources: ReadonlyMap<string, Uint8Array> | undefined
}

/** One lazily filled trap block: the site it reports before trapping. */
export interface TrapState {
  readonly trapBlocks: Array<{ readonly block: LlvmBlock.Block; readonly site: number }>
}

/** Per-function view over the module tables. */
export interface FunctionContext {
  readonly module: ModuleContext
  readonly body: FunctionBody.FunctionBody
  readonly fn: Mir.MirFunction
  readonly state: TrapState
  readonly diagnostic?: NativeDiagnosticContext.NativeDiagnosticContext
}

export const make = (request: CodegenRequest): ModuleContext => ({
  trapSites: [],
  tables: new Map(),
  sources: request.sources,
})

/** Canonical identity of one failure member of an Effect outcome type. */
export const identityOf = (type: Type.Effect | Type.FailureRow, tag: number): string => {
  const failure = Type.failureMembers(type).at(tag - 1)
  if (failure === undefined) throw new RangeError(`Effect failure tag ${tag} has no type identity`)
  return Type.encode(failure)
}

/** A failure propagated out of this function: append its frame to the path. */
export const storePropagated = Effect.fnUntraced(function* (
  context: FunctionContext,
  outcome: Mir.LocalId,
  span: SourceSpan.SourceSpan,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  if (context.diagnostic !== undefined) {
    const slot = context.diagnostic.outcomes.get(outcome.ordinal)
    if (slot === undefined) throw new RangeError('Failure propagation lost its diagnostic outcome')
    yield* NativeDiagnosticOutcome.propagate(
      slot,
      context.diagnostic,
      yield* NativeDiagnosticText.literal(
        context.diagnostic,
        NativeDiagnosticText.origin(context.module, context.fn, span),
        `${symbolFor(context.fn)}.propagate${outcome.ordinal}.${span.start}.frame`,
      ),
    )
  }
})

/** Creates the block a checked operation branches to when its trap condition holds. */
export const trapBlock = Effect.fnUntraced(function* (
  context: FunctionContext,
  reason: string,
  span: SourceSpan.SourceSpan,
): Effect.fn.Return<LlvmBlock.Block, LlvmError.LlvmError> {
  let site = 0
  if (context.diagnostic !== undefined) {
    context.module.trapSites.push({
      reason,
      origin: NativeDiagnosticText.origin(context.module, context.fn, span),
    })
    site = context.module.trapSites.length
  }
  const block = yield* LlvmBlock.make(context.body, `trap_site${site}`)
  context.state.trapBlocks.push({ block, site })
  return block
})

/** Delivers the trap site to the lexical source observer before the machine trap. */
const reportTrap = Effect.fnUntraced(function* (
  context: FunctionContext,
  site: number,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  if (site === 0) return
  const diagnostic = context.diagnostic
  if (diagnostic !== undefined) {
    const entry = context.module.trapSites.at(site - 1)
    if (entry === undefined) throw new RangeError('Diagnostic trap lost its semantic site')
    yield* NativeDiagnosticContext.fatal(
      diagnostic,
      yield* NativeDiagnosticText.literal(
        diagnostic,
        entry.reason,
        `silk.diagnostic.trap${site}.reason`,
      ),
      yield* NativeDiagnosticText.literal(
        diagnostic,
        entry.origin,
        `silk.diagnostic.trap${site}.origin`,
      ),
    )
    return
  }
})

/** Fills every trap block registered while lowering the function body. */
export const emitTrapBlocks = Effect.fnUntraced(function* (
  context: FunctionContext,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  for (const { block, site } of context.state.trapBlocks) {
    yield* LlvmBlock.setInsertionPoint(context.body, block)
    yield* reportTrap(context, site)
    yield* Intrinsic.call(context.body, 'trap', [], [])
    yield* FunctionBody.unreachable(context.body)
  }
})

/** Emits a report call followed by the trap for a terminator that traps unconditionally. */
export const emitTrap = Effect.fnUntraced(function* (
  context: FunctionContext,
  reason: string,
  span: SourceSpan.SourceSpan,
): Effect.fn.Return<FunctionBody.Instruction, LlvmError.LlvmError> {
  if (context.diagnostic !== undefined) {
    context.module.trapSites.push({
      reason,
      origin: NativeDiagnosticText.origin(context.module, context.fn, span),
    })
    yield* reportTrap(context, context.module.trapSites.length)
  }
  yield* Intrinsic.call(context.body, 'trap', [], [])
  return yield* FunctionBody.unreachable(context.body)
})
