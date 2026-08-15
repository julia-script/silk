import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import type * as Analysis from './Analysis.js'
import * as FrontendTooling from './FrontendTooling.js'
import * as ModuleClosure from './ModuleClosure.js'
import type * as ModuleSemantics from './ModuleSemantics.js'
import type * as ModuleSurface from './ModuleSurface.js'
import type * as ModuleTooling from './ModuleTooling.js'
import * as OpaqueRealization from './OpaqueRealization.js'
import type * as PhaseReport from './PhaseReport.js'
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

/** One frontend-query-compatible root view that cannot be passed to runtime realization. */
export interface View extends Analysis.FrontendSnapshot {
  readonly _tag: 'ProjectAnalysisView'
  readonly realization: 'ProjectView'
}

/** One immutable multi-root compiler frontend and its structurally shared root views. */
export interface ProjectAnalysis {
  readonly _tag: 'ProjectAnalysis'
  readonly roots: ReadonlyArray<string>
  readonly closure: ModuleClosure.ProjectClosure
  readonly views: ReadonlyMap<string, View>
  readonly syntaxRevisions: ReadonlyMap<string, SyntaxRevision>
  readonly surfaces: ReadonlyMap<string, ModuleSurface.ModuleSurface>
  readonly semantics: ReadonlyMap<string, ModuleSemantics.ModuleSemantics>
  readonly [OpaqueRealization.catalogSymbol]: OpaqueRealization.Catalog
  readonly toolingModules: ReadonlyMap<string, ModuleTooling.ModuleTooling>
  readonly semanticEnvironment: string
  readonly semanticInvalidation: SemanticInvalidation.SemanticInvalidation
  readonly report: ReadonlyArray<PhaseReport.PhaseReport>
}

const analyze = Effect.fnUntraced(function* (
  roots: ReadonlyArray<SourceFile.SourceFile>,
  previous?: ProjectAnalysis,
): Effect.fn.Return<ProjectAnalysis, never, SourceResolver.SourceResolver> {
  const frontend = yield* Pipeline.frontendProject(
    {
      roots,
      ...(previous === undefined ? {} : { previous: previous.closure }),
    },
    {},
    previous === undefined
      ? undefined
      : {
          closure: previous.closure,
          surfaces: previous.surfaces,
          semantics: previous.semantics,
          [OpaqueRealization.catalogSymbol]: OpaqueRealization.catalogOf(previous),
          environment: previous.semanticEnvironment,
        },
  )
  const tooling = FrontendTooling.make(frontend, previous?.toolingModules)
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
  const report = tooling.report
  const views = new Map<string, View>()
  for (const rootModule of frontend.closure.rootModules) {
    const closure = ModuleClosure.view(frontend.closure, rootModule)
    if (closure === undefined)
      throw new RangeError(`Project analysis lost requested root ${rootModule}`)
    views.set(
      rootModule,
      Object.freeze({
        _tag: 'ProjectAnalysisView',
        realization: 'ProjectView',
        ...frontend,
        ...tooling,
        closure,
        semanticInvalidation: frontend.semanticInvalidation,
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
    semantics: frontend.semantics,
    [OpaqueRealization.catalogSymbol]: OpaqueRealization.catalogOf(frontend),
    toolingModules: tooling.toolingModules,
    semanticEnvironment: SemanticInvalidation.environment,
    semanticInvalidation: frontend.semanticInvalidation,
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
export const view = (self: ProjectAnalysis, rootModule: string): View | undefined =>
  self.views.get(rootModule)

/** Returns the one phase sequence that produced every root view. */
export const phases = (self: ProjectAnalysis): ReadonlyArray<PhaseReport.PhaseReport> => self.report
