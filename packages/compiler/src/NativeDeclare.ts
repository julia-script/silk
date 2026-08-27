import type * as Builder from '@silk-lang/llvm/Builder'
import * as FunctionActor from '@silk-lang/llvm/Function'
import type * as LlvmError from '@silk-lang/llvm/LlvmError'
import * as LlvmType from '@silk-lang/llvm/Type'
import * as Effect from 'effect/Effect'
import { symbolFor } from './Backend.js'
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
    const publicSymbol = symbolFor(fn, Mir.machineEntry(context.program))
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
    declared.push(
      Object.freeze({
        fn,
        symbol,
        publicSymbol,
        handle: yield* FunctionActor.declare(context.builder, symbol, signature),
        resultType,
        emittedResultType,
        resultLaneCount,
        suspendable,
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

/** Stable C ABI symbol for one sealed OS intrinsic. */
export const osRuntimeSymbol = (name: string): string => {
  const words = name
    .replace(/^os/, '')
    .replaceAll(/([a-z])([A-Z])/g, '$1_$2')
    .toLowerCase()
  return `silk_os_${words}_v1`
}
