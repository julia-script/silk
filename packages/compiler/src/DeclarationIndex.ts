import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as Diagnostic from './Diagnostic.js'

/** The immutable declaration index assembled from one loaded module closure. */
export interface Index {
  readonly _tag: 'DeclarationIndex'
  readonly stage: 'Collected' | 'Complete'
  readonly modules: ReadonlyArray<DeclarationFacts.ModuleHeaders>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

/** Constructs one immutable declaration index at a coordinator-owned phase boundary. */
export const make = (
  stage: Index['stage'],
  modules: ReadonlyArray<DeclarationFacts.ModuleHeaders>,
  diagnostics: ReadonlyArray<Diagnostic.Diagnostic>,
): Index =>
  Object.freeze({
    _tag: 'DeclarationIndex',
    stage,
    modules: Object.freeze(modules),
    diagnostics: Object.freeze(diagnostics),
  })
