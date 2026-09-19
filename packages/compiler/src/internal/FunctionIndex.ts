import type * as DeclarationFacts from '../DeclarationFacts.js'
import type * as Tir from '../Tir.js'
import type * as NativeLoweringContext from '../NativeLoweringContext.js'

/** Source-ordered candidates, indexed without changing declaration or specialization identity. */
export interface FunctionIndex<A> {
  readonly names: ReadonlyMap<string, ReadonlyArray<A>>
  readonly modules: ReadonlyMap<string, ReadonlyMap<string, ReadonlyArray<A>>>
}

/** Builds one index for a completed function collection; unresolved declarations are omitted. */
export const make = <A>(
  values: ReadonlyArray<A>,
  declarationOf: (value: A) => DeclarationFacts.CanonicalId | undefined,
): FunctionIndex<A> => {
  const names = new Map<string, Array<A>>()
  const modules = new Map<string, Map<string, Array<A>>>()
  // The cold scalar sweep measured quadratic module-wide .find comparisons. Index each
  // collection once, retaining source order within buckets so first-match behavior is exact.
  for (const value of values) {
    const declaration = declarationOf(value)
    if (declaration === undefined) continue
    const named = names.get(declaration.name) ?? []
    named.push(value)
    names.set(declaration.name, named)
    const module = modules.get(declaration.module) ?? new Map<string, Array<A>>()
    const candidates = module.get(declaration.name) ?? []
    candidates.push(value)
    module.set(declaration.name, candidates)
    modules.set(declaration.module, module)
  }
  for (const values of names.values()) Object.freeze(values)
  for (const module of modules.values()) for (const values of module.values()) Object.freeze(values)
  return Object.freeze({ names, modules })
}

/** Returns only this declaration's candidates, in the original collection order. */
export const candidates = <A>(
  self: FunctionIndex<A>,
  declaration: DeclarationFacts.CanonicalId,
): ReadonlyArray<A> => self.modules.get(declaration.module)?.get(declaration.name) ?? []

const tirIndexes = new WeakMap<Tir.Module, FunctionIndex<Tir.TirFunction>>()

const tirIndex = (module: Tir.Module): FunctionIndex<Tir.TirFunction> => {
  const cached = tirIndexes.get(module)
  if (cached !== undefined) return cached
  const index = make(module.functions, (fn) =>
    fn.declaration.canonical._tag === 'Canonical' ? fn.declaration.canonical.id : undefined,
  )
  // TIR module snapshots are immutable. A revised module owns a different index, and weak
  // ownership lets discarded analysis snapshots (including their functions) be collected.
  tirIndexes.set(module, index)
  return index
}

/** Resolves the first canonical function by name in an already-selected TIR module. */
export const tirByName = (
  module: Tir.Module | undefined,
  name: string,
): Tir.TirFunction | undefined =>
  module === undefined ? undefined : tirIndex(module).names.get(name)?.at(0)

/** Resolves the first function with the exact canonical module and declaration name. */
export const tirByCanonical = (
  module: Tir.Module | undefined,
  declaration: DeclarationFacts.CanonicalId,
): Tir.TirFunction | undefined =>
  module === undefined ? undefined : candidates(tirIndex(module), declaration).at(0)

const nativeIndexes = new WeakMap<
  ReadonlyArray<NativeLoweringContext.DeclaredFunction>,
  FunctionIndex<NativeLoweringContext.DeclaredFunction>
>()

/** Narrows native lookup by declaration; callers still apply the exact MIR instance matcher. */
export const nativeCandidates = (
  declared: ReadonlyArray<NativeLoweringContext.DeclaredFunction>,
  declaration: DeclarationFacts.CanonicalId,
): ReadonlyArray<NativeLoweringContext.DeclaredFunction> => {
  let index = nativeIndexes.get(declared)
  if (index === undefined) {
    // Native emission starts after declaration is complete; this collection is not appended
    // to while calls are emitted. Never key by symbol spelling or erase static arguments.
    index = make(declared, (entry) => entry.fn.id)
    nativeIndexes.set(declared, index)
  }
  return candidates(index, declaration)
}
