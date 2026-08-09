import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import type * as Analysis from './Analysis.js'
import * as FrontendTooling from './FrontendTooling.js'
import * as ModuleClosure from './ModuleClosure.js'
import type * as ModuleSurface from './ModuleSurface.js'
import * as PhaseReport from './PhaseReport.js'
import * as Pipeline from './Pipeline.js'
import * as SemanticInvalidation from './SemanticInvalidation.js'
import type * as SourceFile from './SourceFile.js'
import type * as SourceResolver from './SourceResolver.js'
import * as SyntaxCorrespondence from './SyntaxCorrespondence.js'
import type * as SyntaxFile from './SyntaxFile.js'

/** One current module's syntax relationship to the optional previous project revision. */
export type SyntaxRevision =
  | {
      readonly _tag: 'Fresh'
      readonly module: string
      readonly current: SyntaxFile.SyntaxFile
    }
  | {
      readonly _tag: 'Reused'
      readonly module: string
      readonly previous: SyntaxFile.SyntaxFile
      readonly current: SyntaxFile.SyntaxFile
    }
  | {
      readonly _tag: 'Changed'
      readonly module: string
      readonly previous: SyntaxFile.SyntaxFile
      readonly current: SyntaxFile.SyntaxFile
      readonly correspondence: SyntaxCorrespondence.SyntaxCorrespondence
    }

/** One immutable multi-root compiler frontend and its structurally shared root views. */
export interface ProjectAnalysis {
  readonly _tag: 'ProjectAnalysis'
  readonly roots: ReadonlyArray<string>
  readonly closure: ModuleClosure.ProjectClosure
  readonly views: ReadonlyMap<string, Analysis.FrontendSnapshot>
  readonly syntaxRevisions: ReadonlyMap<string, SyntaxRevision>
  readonly surfaces: ReadonlyMap<string, ModuleSurface.ModuleSurface>
  readonly semanticEnvironment: string
  readonly semanticInvalidation: SemanticInvalidation.SemanticInvalidation
  readonly report: ReadonlyArray<PhaseReport.PhaseReport>
}

const analyze = Effect.fnUntraced(function* (
  roots: ReadonlyArray<SourceFile.SourceFile>,
  previous?: ProjectAnalysis,
): Effect.fn.Return<ProjectAnalysis, never, SourceResolver.SourceResolver> {
  const frontend = yield* Pipeline.frontendProject({
    roots,
    ...(previous === undefined ? {} : { previous: previous.closure }),
  })
  const tooling = FrontendTooling.make(frontend)
  const previousModules = new Map(
    previous?.closure.modules.map((module) => [module.name, module.syntax]),
  )
  const syntaxRevisions = new Map<string, SyntaxRevision>()
  for (const module of frontend.closure.modules) {
    const previousSyntax = previousModules.get(module.name)
    if (previousSyntax === undefined) {
      syntaxRevisions.set(
        module.name,
        Object.freeze({ _tag: 'Fresh', module: module.name, current: module.syntax }),
      )
      continue
    }
    if (previousSyntax === module.syntax) {
      syntaxRevisions.set(
        module.name,
        Object.freeze({
          _tag: 'Reused',
          module: module.name,
          previous: previousSyntax,
          current: module.syntax,
        }),
      )
      continue
    }
    syntaxRevisions.set(
      module.name,
      Object.freeze({
        _tag: 'Changed',
        module: module.name,
        previous: previousSyntax,
        current: module.syntax,
        correspondence: Option.getOrThrow(
          SyntaxCorrespondence.between(previousSyntax, module.syntax),
        ),
      }),
    )
  }
  const semanticProject: SemanticInvalidation.Project = Object.freeze({
    closure: frontend.closure,
    surfaces: frontend.surfaces,
    environment: SemanticInvalidation.environment,
  })
  const previousSemanticProject: SemanticInvalidation.Project | undefined =
    previous === undefined
      ? undefined
      : Object.freeze({
          closure: previous.closure,
          surfaces: previous.surfaces,
          environment: previous.semanticEnvironment,
        })
  const measuredInvalidation = PhaseReport.measure(
    'semantic-invalidation',
    frontend.closure.modules.length,
    () =>
      SemanticInvalidation.make({
        current: semanticProject,
        revisions: syntaxRevisions,
        ...(previousSemanticProject === undefined ? {} : { previous: previousSemanticProject }),
      }),
    (value) => value.totals.recomputed,
  )
  const totals = measuredInvalidation.value.totals
  const invalidationReport = PhaseReport.make({
    ...measuredInvalidation.report,
    counters: Object.freeze({
      _tag: 'SemanticInvalidationCounters',
      reusable: totals.reusable,
      recomputed: totals.recomputed,
      fresh: totals.reasons.Fresh,
      localChange: totals.reasons.LocalChange,
      dependencySurfaceChange: totals.reasons.DependencySurfaceChange,
      cyclicPeerChange: totals.reasons.CyclicPeerChange,
      environmentChange: totals.reasons.EnvironmentChange,
      surfaceChange: totals.reasons.SurfaceChange,
    }),
  })
  const report = Object.freeze([...tooling.report, invalidationReport])
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
        semanticInvalidation: measuredInvalidation.value,
        report,
      }),
    )
  }
  return Object.freeze({
    _tag: 'ProjectAnalysis',
    roots: frontend.closure.rootModules,
    closure: frontend.closure,
    views,
    syntaxRevisions,
    surfaces: frontend.surfaces,
    semanticEnvironment: SemanticInvalidation.environment,
    semanticInvalidation: measuredInvalidation.value,
    report,
  })
})

/** Constructs one history-independent frontend analysis for the union closure of all roots. */
export const make = Effect.fn('ProjectAnalysis.make')(function* (
  roots: ReadonlyArray<SourceFile.SourceFile>,
): Effect.fn.Return<ProjectAnalysis, never, SourceResolver.SourceResolver> {
  return yield* analyze(roots)
})

/** Constructs a new coherent project while reusing safe syntax from one completed prior project. */
export const revise = Effect.fn('ProjectAnalysis.revise')(function* (
  previous: ProjectAnalysis,
  roots: ReadonlyArray<SourceFile.SourceFile>,
): Effect.fn.Return<ProjectAnalysis, never, SourceResolver.SourceResolver> {
  return yield* analyze(roots, previous)
})

/** Returns the requested root view, or `undefined` when it was not part of the project request. */
export const view = (
  self: ProjectAnalysis,
  rootModule: string,
): Analysis.FrontendSnapshot | undefined => self.views.get(rootModule)

/** Returns the one phase sequence that produced every root view. */
export const phases = (self: ProjectAnalysis): ReadonlyArray<PhaseReport.PhaseReport> => self.report
