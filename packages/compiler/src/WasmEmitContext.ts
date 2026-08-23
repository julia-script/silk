import type * as FuncActor from '@silk-effect/wasm/Func'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as LayoutPlan from './Layout.js'
import type * as Mir from './Mir.js'
import type * as SilkType from './Type.js'
import type * as WasmMemory from './WasmMemory.js'

/** Explicit state consumed by sibling WebAssembly operation emitters. */
export interface WasmEmitContext<FunctionLayout, SuspensionRuntime> {
  readonly fn: Mir.MirFunction
  readonly layout: FunctionLayout
  readonly plan: LayoutPlan.Plan
  readonly resolve: (
    target: DeclarationFacts.CanonicalId,
    typeArguments: ReadonlyArray<SilkType.GenericArgument>,
  ) => FuncActor.Func
  readonly resolveIndependent: (
    target: DeclarationFacts.CanonicalId,
    typeArguments: ReadonlyArray<SilkType.GenericArgument>,
  ) => FuncActor.Func
  readonly memory: WasmMemory.MemoryContext | undefined
  readonly suspensionRuntime?: SuspensionRuntime
}
