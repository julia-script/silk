import * as LlvmBlock from '@silklang/llvm/Block'
import type * as Builder from '@silklang/llvm/Builder'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionActor from '@silklang/llvm/Function'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as Intrinsic from '@silklang/llvm/Intrinsic'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import * as LlvmType from '@silklang/llvm/Type'
import type * as Value from '@silklang/llvm/Value'
import * as Variable from '@silklang/llvm/Variable'
import * as Effect from 'effect/Effect'
import {
  type CodegenRequest,
  type LineTable,
  lineTable,
  logicalFrameEntries,
  positionOf,
} from './Backend.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as Mir from './Mir.js'
import * as MirVerification from './MirVerification.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Termination from './Termination.js'
import * as Type from './Type.js'

/**
 * Static host-report tables and the runtime state the native adapter reads after `silk_main`
 * returns: which entry failure tag closed, where the active failure was produced, the logical
 * frames it propagated through, and the failure a failing recovery handler was handling.
 *
 * Every store is emitted only on failure or trap paths, and every global exists only when a
 * program has a site that writes it, so a program that cannot fail pays nothing.
 */

/** Frames retained per runtime failure path; deeper propagation keeps the first frames. */
// ponytail: fixed capacity, grow to a heap-backed path if deep recursive propagation matters.
export const pathCapacity = 32

export const trapReportSymbol = 'silk_trap_report_v1'
export const failureTagSymbol = 'silk_failure_tag_v1'
export const failureSiteSymbol = 'silk_failure_site_v1'
export const failureDepthSymbol = 'silk_failure_depth_v1'
export const failurePathSymbol = 'silk_failure_path_v1'
export const causeSiteSymbol = 'silk_cause_site_v1'
export const causeDepthSymbol = 'silk_cause_depth_v1'
export const causePathSymbol = 'silk_cause_path_v1'

interface FailureGlobals {
  readonly site: Constant.Constant
  readonly depth: Constant.Constant
  readonly path: Constant.Constant
  readonly causeSite: Constant.Constant
  readonly causeDepth: Constant.Constant
  readonly causePath: Constant.Constant
  readonly pathType: LlvmType.Type
}

/** Module-wide report tables shared by every lowered function. */
export interface ModuleContext {
  readonly builder: Builder.Builder
  readonly program: Mir.Module
  readonly i32: LlvmType.Type
  /** Native standalone targets own a host report; direct Wasm keeps the bare trap. */
  readonly enabled: boolean
  readonly frames: ReadonlyArray<{
    readonly fn: Mir.MirFunction
    readonly frame: Termination.LogicalFrame
  }>
  readonly frameLabels: ReadonlyArray<string>
  readonly failureSites: Array<{ readonly identity: string; readonly origin: string }>
  readonly trapSites: Array<{ readonly reason: string; readonly origin: string }>
  readonly tables: Map<string, LineTable>
  readonly sources: ReadonlyMap<string, Uint8Array> | undefined
  state: {
    failureTag: Constant.Constant | undefined
    failure: FailureGlobals | undefined
    trapReport: FunctionActor.Function | undefined
  }
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
}

const labelOf = (
  context: Pick<ModuleContext, 'tables' | 'sources'>,
  id: DeclarationFacts.CanonicalId,
  span: SourceSpan.SourceSpan,
): string => {
  // A declared effect fn runs through its `$effect$-1` body runner; the report names the function.
  const name = `${id.module}.${id.name.replace(/\$effect\$-1$/, '')}`
  const bytes = context.sources?.get(span.sourceId)
  if (bytes === undefined) return `${name} (${span.sourceId})`
  let table = context.tables.get(span.sourceId)
  if (table === undefined) {
    table = lineTable(bytes)
    context.tables.set(span.sourceId, table)
  }
  const position = positionOf(table, span.start)
  return `${name} (${span.sourceId}:${position.line}:${position.column})`
}

export const make = (
  builder: Builder.Builder,
  program: Mir.Module,
  request: CodegenRequest,
  i32: LlvmType.Type,
): ModuleContext => {
  const frames = logicalFrameEntries(program)
  const tables = new Map<string, LineTable>()
  const partial = { tables, sources: request.sources }
  return {
    builder,
    program,
    i32,
    enabled: program.layout.target.kind === 'Native',
    frames,
    frameLabels: frames.map(({ frame }) => labelOf(partial, frame.function, frame.provenance)),
    failureSites: [],
    trapSites: [],
    tables,
    sources: request.sources,
    state: { failureTag: undefined, failure: undefined, trapReport: undefined },
  }
}

export const report = (context: ModuleContext): Termination.Report =>
  Object.freeze({
    frames: Object.freeze([...context.frameLabels]),
    failureSites: Object.freeze(context.failureSites.map((site) => Object.freeze(site))),
    trapSites: Object.freeze(context.trapSites.map((site) => Object.freeze(site))),
  })

/** Canonical identity of one failure member of an Effect outcome type. */
export const identityOf = (type: Type.Effect | Type.FailureRow, tag: number): string => {
  const failure = Type.failureMembers(type).at(tag - 1)
  if (failure === undefined) throw new RangeError(`Effect failure tag ${tag} has no type identity`)
  return Type.encode(failure)
}

const frameOrdinal = (context: FunctionContext): number =>
  Math.max(
    0,
    context.module.frames.findIndex((entry) => entry.fn === context.fn),
  )

const hasCatch = (fn: Mir.MirFunction): boolean =>
  MirVerification.operations(fn).some((operation) => operation._tag === 'CatchEffect')

const scalarGlobal = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  name: string,
  type: LlvmType.Type,
): Effect.fn.Return<Constant.Constant, LlvmError.LlvmError> {
  const variable = yield* Variable.make(builder, name, type, {
    initializer: yield* Constant.zero(builder, type),
  })
  return yield* Constant.fromGlobal(builder, yield* Variable.global(builder, variable))
})

const failureGlobals = Effect.fnUntraced(function* (
  context: ModuleContext,
): Effect.fn.Return<FailureGlobals, LlvmError.LlvmError> {
  if (context.state.failure !== undefined) return context.state.failure
  const { builder, i32 } = context
  const pathType = yield* LlvmType.array(builder, i32, pathCapacity)
  const globals: FailureGlobals = Object.freeze({
    site: yield* scalarGlobal(builder, failureSiteSymbol, i32),
    depth: yield* scalarGlobal(builder, failureDepthSymbol, i32),
    path: yield* scalarGlobal(builder, failurePathSymbol, pathType),
    causeSite: yield* scalarGlobal(builder, causeSiteSymbol, i32),
    causeDepth: yield* scalarGlobal(builder, causeDepthSymbol, i32),
    causePath: yield* scalarGlobal(builder, causePathSymbol, pathType),
    pathType,
  })
  context.state.failure = globals
  return globals
})

/** Records which entry failure tag closed, so the adapter names the active union member. */
export const storeFailureTag = Effect.fnUntraced(function* (
  context: FunctionContext,
  tag: number,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  if (!context.module.enabled) return
  const { builder, i32 } = context.module
  context.module.state.failureTag ??= yield* scalarGlobal(builder, failureTagSymbol, i32)
  yield* FunctionBody.store(
    context.body,
    yield* Constant.integerSigned(builder, i32, BigInt(tag)),
    context.module.state.failureTag,
  )
})

/** Registers one static failure origin; returns its 1-based site ordinal (0 when disabled). */
export const registerFailureSite = (
  context: FunctionContext,
  identity: string,
  span: SourceSpan.SourceSpan,
): number => {
  if (!context.module.enabled) return 0
  context.module.failureSites.push({
    identity,
    origin: labelOf(context.module, context.fn.id, span),
  })
  return context.module.failureSites.length
}

/** A failure was produced here: the path restarts at this function. */
export const storeProduced = Effect.fnUntraced(function* (
  context: FunctionContext,
  site: Value.Input,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  if (!context.module.enabled) return
  const { builder, i32 } = context.module
  const globals = yield* failureGlobals(context.module)
  const zero = yield* Constant.integerSigned(builder, i32, 0n)
  yield* FunctionBody.store(context.body, site, globals.site)
  yield* FunctionBody.store(
    context.body,
    yield* Constant.integerSigned(builder, i32, 1n),
    globals.depth,
  )
  yield* FunctionBody.store(
    context.body,
    yield* Constant.integerSigned(builder, i32, BigInt(frameOrdinal(context))),
    yield* FunctionBody.getElementPtr(
      context.body,
      globals.pathType,
      globals.path,
      [zero, zero],
      'failure_path_origin',
    ),
  )
})

/** A failure propagated out of this function: append its frame to the path. */
export const storePropagated = Effect.fnUntraced(function* (
  context: FunctionContext,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  if (!context.module.enabled) return
  const { builder, i32 } = context.module
  const { body } = context
  const globals = yield* failureGlobals(context.module)
  const depth = yield* FunctionBody.load(body, i32, globals.depth, 'failure_depth')
  const capacity = yield* Constant.integerSigned(builder, i32, BigInt(pathCapacity))
  const inRange = yield* FunctionBody.integerCompare(
    body,
    'ult',
    depth,
    capacity,
    'failure_in_range',
  )
  const index = yield* FunctionBody.select(
    body,
    inRange,
    depth,
    yield* Constant.integerSigned(builder, i32, BigInt(pathCapacity - 1)),
    'failure_path_index',
  )
  yield* FunctionBody.store(
    body,
    yield* Constant.integerSigned(builder, i32, BigInt(frameOrdinal(context))),
    yield* FunctionBody.getElementPtr(
      body,
      globals.pathType,
      globals.path,
      [yield* Constant.integerSigned(builder, i32, 0n), index],
      'failure_path_slot',
    ),
  )
  yield* FunctionBody.store(
    body,
    yield* FunctionBody.binary(
      body,
      'add',
      depth,
      yield* Constant.integerSigned(builder, i32, 1n),
      'failure_depth_next',
    ),
    globals.depth,
  )
})

/** A recovery handler is about to run: retain the caught failure as the causal context. */
export const storeCaught = Effect.fnUntraced(function* (
  context: FunctionContext,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  if (!context.module.enabled) return
  const { builder, i32 } = context.module
  const { body } = context
  const globals = yield* failureGlobals(context.module)
  yield* FunctionBody.store(
    body,
    yield* FunctionBody.load(body, i32, globals.site, 'caught_site'),
    globals.causeSite,
  )
  yield* FunctionBody.store(
    body,
    yield* FunctionBody.load(body, i32, globals.depth, 'caught_depth'),
    globals.causeDepth,
  )
  yield* Intrinsic.memcpy(
    body,
    globals.causePath,
    globals.path,
    yield* Constant.integerSigned(builder, i32, BigInt(pathCapacity * 4)),
  )
})

/** The enclosing catch runner completed successfully: the handled failure leaves the outcome. */
export const clearCause = Effect.fnUntraced(function* (
  context: FunctionContext,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  if (!context.module.enabled || !hasCatch(context.fn)) return
  const { builder, i32 } = context.module
  const globals = yield* failureGlobals(context.module)
  yield* FunctionBody.store(
    context.body,
    yield* Constant.integerSigned(builder, i32, 0n),
    globals.causeSite,
  )
})

/** Creates the block a checked operation branches to when its trap condition holds. */
export const trapBlock = Effect.fnUntraced(function* (
  context: FunctionContext,
  reason: string,
  span: SourceSpan.SourceSpan,
): Effect.fn.Return<LlvmBlock.Block, LlvmError.LlvmError> {
  let site = 0
  if (context.module.enabled) {
    context.module.trapSites.push({ reason, origin: labelOf(context.module, context.fn.id, span) })
    site = context.module.trapSites.length
  }
  const block = yield* LlvmBlock.make(context.body, `trap_site${site}`)
  context.state.trapBlocks.push({ block, site })
  return block
})

/** Reports the trap site through the host adapter, then traps. Emits nothing outside a report. */
const reportTrap = Effect.fnUntraced(function* (
  context: FunctionContext,
  site: number,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  if (site === 0) return
  const { builder, i32 } = context.module
  context.module.state.trapReport ??= yield* FunctionActor.declare(
    builder,
    trapReportSymbol,
    yield* LlvmType.functionType(builder, yield* LlvmType.voidType(builder), [i32]),
  )
  yield* FunctionBody.callDirect(context.body, context.module.state.trapReport, [
    yield* Constant.integerSigned(builder, i32, BigInt(site)),
  ])
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
  if (context.module.enabled) {
    context.module.trapSites.push({ reason, origin: labelOf(context.module, context.fn.id, span) })
    yield* reportTrap(context, context.module.trapSites.length)
  }
  yield* Intrinsic.call(context.body, 'trap', [], [])
  return yield* FunctionBody.unreachable(context.body)
})
