import * as Effect from 'effect/Effect'
import type * as Analysis from './Analysis.js'
import * as FrontendTooling from './FrontendTooling.js'
import * as ModuleClosure from './ModuleClosure.js'
import type * as PhaseReport from './PhaseReport.js'
import * as Pipeline from './Pipeline.js'
import type * as SourceFile from './SourceFile.js'
import type * as SourceResolver from './SourceResolver.js'

/** One immutable multi-root compiler frontend and its structurally shared root views. */
export interface ProjectAnalysis {
  readonly _tag: 'ProjectAnalysis'
  readonly roots: ReadonlyArray<string>
  readonly closure: ModuleClosure.ProjectClosure
  readonly views: ReadonlyMap<string, Analysis.FrontendSnapshot>
  readonly report: ReadonlyArray<PhaseReport.PhaseReport>
}

/** Constructs one frontend analysis for the union closure of all supplied roots. */
export const make = Effect.fn('ProjectAnalysis.make')(function* (
  roots: ReadonlyArray<SourceFile.SourceFile>,
): Effect.fn.Return<ProjectAnalysis, never, SourceResolver.SourceResolver> {
  const frontend = yield* Pipeline.frontendProject({ roots })
  const tooling = FrontendTooling.make(frontend)
  const views = new Map<string, Analysis.FrontendSnapshot>()
  for (const rootModule of frontend.closure.rootModules) {
    const closure = ModuleClosure.view(frontend.closure, rootModule)
    if (closure === undefined)
      throw new RangeError(`Project analysis lost requested root ${rootModule}`)
    views.set(
      rootModule,
      Object.freeze({
        _tag: 'AnalysisSnapshot',
        ...frontend,
        ...tooling,
        closure,
      }),
    )
  }
  return Object.freeze({
    _tag: 'ProjectAnalysis',
    roots: frontend.closure.rootModules,
    closure: frontend.closure,
    views,
    report: tooling.report,
  })
})

/** Returns the requested root view, or `undefined` when it was not part of the project request. */
export const view = (
  self: ProjectAnalysis,
  rootModule: string,
): Analysis.FrontendSnapshot | undefined => self.views.get(rootModule)

/** Returns the one phase sequence that produced every root view. */
export const phases = (self: ProjectAnalysis): ReadonlyArray<PhaseReport.PhaseReport> => self.report
