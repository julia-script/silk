import * as LlvmBlock from '@silklang/llvm/Block'
import type * as Builder from '@silklang/llvm/Builder'
import * as FunctionActor from '@silklang/llvm/Function'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import * as LlvmType from '@silklang/llvm/Type'
import * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import { BackendError, symbolFor } from './Backend.js'
import type * as CAbi from './CAbi.js'
import type * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import { linearize } from './MirLinearization.js'
import type * as NativeLoweringContext from './NativeLoweringContext.js'

export interface DeclarationContext {
  readonly builder: Builder.Builder
  readonly program: Mir.Module
  readonly i32: LlvmType.Type
  readonly pointer: LlvmType.Type
  readonly lanesFor: (type: Mir.Type) => ReadonlyArray<Layout.CallingLane>
  readonly laneType: (lane: Layout.CallingLane) => LlvmType.Type
}

/** Declares every MIR function with its complete synchronous or suspension-aware ABI. */
export const functions = Effect.fn('NativeDeclare.functions')(function* (
  context: DeclarationContext,
): Effect.fn.Return<
  {
    readonly declared: ReadonlyArray<NativeLoweringContext.DeclaredFunction>
    readonly voidType?: LlvmType.Type
  },
  LlvmError.LlvmError
> {
  let voidType: LlvmType.Type | undefined
  const declared: Array<NativeLoweringContext.DeclaredFunction> = []
  const machineEntry =
    context.program.entry._tag === 'LibraryEntry' ? undefined : Mir.machineEntry(context.program)
  for (const fn of context.program.functions) {
    const resultLanes = context.lanesFor(fn.result)
    const resultLaneCount = resultLanes.length
    let resultType: LlvmType.Type
    if (resultLaneCount === 0) {
      const selected = voidType ?? (yield* LlvmType.voidType(context.builder))
      voidType = selected
      resultType = selected
    } else if (resultLaneCount === 1) {
      const lane = resultLanes.at(0)
      if (lane === undefined) throw new RangeError('LLVM result lost its scalar lane')
      resultType = context.laneType(lane)
    } else {
      resultType = yield* LlvmType.structure(context.builder, resultLanes.map(context.laneType))
    }
    const parameters =
      fn.regions.length === 0
        ? []
        : fn.localTypes
            .slice(0, fn.parameterCount)
            .flatMap((type) => context.lanesFor(type).map(context.laneType))
    const suspendable =
      fn.suspension !== undefined && fn.suspension.classification !== 'Synchronous'
    const publicSymbol = symbolFor(fn, machineEntry)
    const emittedResultType = suspendable
      ? yield* LlvmType.structure(context.builder, [
          context.i32,
          ...resultLanes.map(context.laneType),
        ])
      : resultType
    const parameterTypes = suspendable
      ? Object.freeze([...parameters, context.pointer, context.pointer, context.i32])
      : Object.freeze(parameters)
    const signature = yield* LlvmType.functionType(
      context.builder,
      emittedResultType,
      parameterTypes,
    )
    const symbol = suspendable ? `${publicSymbol}$suspend_step` : publicSymbol
    const isMachine = machineEntry !== undefined && Mir.matchesInstanceKey(fn, machineEntry)
    const driver = suspendable
      ? yield* FunctionActor.declare(
          context.builder,
          isMachine ? publicSymbol : `${publicSymbol}$drive`,
          yield* LlvmType.functionType(context.builder, resultType, parameters),
          { visibility: 'hidden' },
        )
      : undefined
    declared.push(
      Object.freeze({
        fn,
        symbol,
        publicSymbol,
        handle: yield* FunctionActor.declare(context.builder, symbol, signature, {
          visibility: 'hidden',
        }),
        resultType,
        emittedResultType,
        resultLaneCount,
        suspendable,
        ...(driver === undefined ? {} : { driver }),
        parameterTypes,
        linear: linearize(fn),
      }),
    )
  }
  return Object.freeze({
    declared: Object.freeze(declared),
    ...(voidType === undefined ? {} : { voidType }),
  })
})

export interface ExportContext {
  readonly builder: Builder.Builder
  readonly program: Mir.Module
  readonly declared: ReadonlyArray<NativeLoweringContext.DeclaredFunction>
  /** LLVM type for one classified C type; `undefined` for `void`. */
  readonly cType: (type: CAbi.CAbiType) => LlvmType.Type | undefined
}

/**
 * Defines one external C-convention thunk per export that forwards its scalar arguments to the
 * private implementation and returns its result, so the internal ABI never becomes the public one.
 */
export const exportThunks = Effect.fn('NativeDeclare.exportThunks')(function* (
  context: ExportContext,
): Effect.fn.Return<
  ReadonlyMap<string, FunctionActor.Function>,
  BackendError | LlvmError.LlvmError
> {
  const thunks = new Map<string, FunctionActor.Function>()
  for (const record of context.program.foreignExports) {
    const implementation = context.declared.find((entry) =>
      Mir.matchesInstanceKey(entry.fn, record.key),
    )
    if (implementation === undefined)
      throw new RangeError(`LLVM export ${record.symbol} lost its implementation`)
    if (implementation.suspendable)
      throw new RangeError(`LLVM export ${record.symbol} forwards to a suspendable implementation`)
    const parameters = record.signature.parameters.map(context.cType)
    if (parameters.some((type) => type === undefined))
      throw new RangeError(`LLVM export ${record.symbol} has a void parameter`)
    const resultType =
      context.cType(record.signature.result) ?? (yield* LlvmType.voidType(context.builder))
    const thunk = yield* FunctionActor.declare(
      context.builder,
      record.symbol,
      yield* LlvmType.functionType(
        context.builder,
        resultType,
        parameters.flatMap((type) => (type === undefined ? [] : [type])),
      ),
    ).pipe(
      Effect.mapError(
        (cause) =>
          new BackendError({
            operation: 'Backend.emit',
            backend: 'LLVM',
            message: `exported function ${record.symbol} conflicts with another declaration of that symbol: ${cause.message}`,
            reason: { _tag: 'ForeignSymbolConflict', symbol: record.symbol },
          }),
      ),
    )
    yield* FunctionActor.buildBody(
      context.builder,
      thunk,
      Effect.fnUntraced(function* (body) {
        yield* LlvmBlock.make(body, 'entry')
        const arguments_: Array<Value.Input> = []
        for (let ordinal = 0; ordinal < parameters.length; ordinal += 1)
          arguments_.push(yield* Value.argument(body, ordinal))
        const result = yield* FunctionBody.callDirect(
          body,
          implementation.handle,
          arguments_,
          'forward',
        )
        if (record.signature.result._tag === 'Void') return yield* FunctionBody.returnVoid(body)
        if (result === undefined) throw new RangeError('LLVM export thunk lost its result')
        return yield* FunctionBody.returnValue(body, result)
      }),
    )
    thunks.set(record.symbol, thunk)
  }
  return thunks
})

/** Stable C ABI symbol for one sealed OS intrinsic. */
export const osRuntimeSymbol = (name: string): string => {
  const words = name
    .replace(/^os/, '')
    .replaceAll(/([a-z])([A-Z])/g, '$1_$2')
    .toLowerCase()
  return `silk_os_${words}_v1`
}
