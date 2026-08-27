import type * as Builder from '@silklang/llvm/Builder'
import type * as FunctionActor from '@silklang/llvm/Function'
import type * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as LlvmType from '@silklang/llvm/Type'
import type * as Value from '@silklang/llvm/Value'
import type * as Backend from './Backend.js'
import type * as Layout from './Layout.js'
import type * as Mir from './Mir.js'
import type * as MirLinearization from './MirLinearization.js'

/** One declared MIR function and its fixed native ABI ownership. */
export interface DeclaredFunction {
  readonly fn: Mir.MirFunction
  readonly symbol: string
  readonly publicSymbol: string
  readonly handle: FunctionActor.Function
  readonly resultType: LlvmType.Type
  readonly emittedResultType: LlvmType.Type
  readonly resultLaneCount: number
  readonly suspendable: boolean
  readonly parameterTypes: ReadonlyArray<LlvmType.Type>
  readonly linear: ReadonlyArray<MirLinearization.LinearBlock>
}

/** Explicit native lowering state shared by the per-function lowering actors. */
export interface LoweringContext {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly program: Mir.Module
  readonly request: Backend.CodegenRequest
  readonly layout: Layout.Plan
  readonly types: {
    readonly i8: LlvmType.Type
    readonly i32: LlvmType.Type
    readonly f32: LlvmType.Type
    readonly f64: LlvmType.Type
    readonly pointer: LlvmType.Type
    readonly integers: ReadonlyMap<number, LlvmType.Type>
  }
  readonly lanesFor: (type: Mir.Type) => ReadonlyArray<Layout.CallingLane>
  readonly valueLanesFor: (type: Mir.Type) => ReadonlyArray<Layout.CallingLane>
  readonly laneType: (lane: Layout.CallingLane) => LlvmType.Type
  readonly packedLanes: (
    lanes: ReadonlyArray<Layout.CallingLane>,
    start?: number,
  ) => {
    readonly entries: ReadonlyArray<{ readonly lane: Layout.CallingLane; readonly offset: number }>
    readonly end: number
    readonly alignment: number
  }
  readonly declared: ReadonlyArray<DeclaredFunction>
  readonly entry: DeclaredFunction
  readonly mutableStorage: ReadonlyMap<number, ReadonlyArray<Value.Input>>
}
