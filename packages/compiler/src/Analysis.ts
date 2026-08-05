import * as BootstrapEvaluation from './BootstrapEvaluation.js'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import type * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as Lower from './Lower.js'
import type * as Mir from './Mir.js'
import * as ModuleClosure from './ModuleClosure.js'
import * as Ownership from './Ownership.js'
import type * as SyntaxFile from './SyntaxFile.js'

/**
 * The supported analysis facade. Tooling consumes compiler phases exclusively through this
 * module: build a snapshot from one compilation request, then query immutable facts. The
 * data-model vocabularies (syntax elements, diagnostics, fact types) are part of the facade's
 * answers; running phase modules directly is not a supported consumer surface.
 */

/** One immutable analysis snapshot of one compilation request. */
export interface Snapshot {
  readonly _tag: 'AnalysisSnapshot'
  readonly closure: ModuleClosure.Closure
  readonly index: DeclarationIndex.Index
  readonly results: ReadonlyMap<string, Elaboration.Result>
  readonly ownership: ReadonlyMap<string, Ownership.ModuleOwnership>
  readonly instances: Instances.Discovery
  readonly mir: Mir.Module
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

/** Builds the snapshot of one compilation request. */
export const make = (request: ModuleClosure.CompilationRequest): Snapshot => {
  const closure = ModuleClosure.load(request)
  const index = DeclarationIndex.collect(closure)
  const results = new Map(
    closure.modules.map((module) => [module.name, Elaboration.elaborateModule(module.syntax)]),
  )
  const ownership = new Map(
    [...results.entries()].map(([name, result]) => [name, Ownership.checkModule(result)]),
  )
  const instances = Instances.discover(request.rootModule, results)
  const mir = Lower.lowerProgram(instances, ownership)
  const diagnostics = Diagnostic.merge(
    ...closure.modules.map((module) => module.syntax.lexicalDiagnostics),
    ...closure.modules.map((module) => module.syntax.parserDiagnostics),
    closure.diagnostics,
    ...[...results.values()].map((result) => result.diagnostics),
  )
  return Object.freeze({
    _tag: 'AnalysisSnapshot',
    closure,
    index,
    results,
    ownership,
    instances,
    mir,
    diagnostics,
  })
}

/** Builds the snapshot of one single-module source. */
export const ofSource = (sourceId: string, bytes: Uint8Array): Snapshot =>
  make({ rootModule: sourceId, sources: new Map([[sourceId, bytes]]) })

/** Returns every loaded module of the snapshot in canonical identity order. */
export const modules = (self: Snapshot): ReadonlyArray<ModuleClosure.Module> => self.closure.modules

/** Returns the snapshot's import cycle facts in canonical order. */
export const cycles = (self: Snapshot): ReadonlyArray<ReadonlyArray<string>> => self.closure.cycles

/** Returns the closure's declaration index. */
export const declarationIndex = (self: Snapshot): DeclarationIndex.Index => self.index

/** Returns one module's syntax artifact, or `undefined` for an unknown identity. */
export const syntaxOf = (self: Snapshot, module: string): SyntaxFile.SyntaxFile | undefined =>
  self.results.get(module)?.syntax

/** Returns one module's elaborated analysis, or `undefined` for an unknown identity. */
export const moduleAnalysis = (self: Snapshot, module: string): Elaboration.Result | undefined =>
  self.results.get(module)

/** Returns the root module's elaborated analysis. The root is always loaded. */
export const rootAnalysis = (self: Snapshot): Elaboration.Result => {
  const result = self.results.get(self.closure.rootModule)
  if (result === undefined) {
    throw new RangeError(`Snapshot lost its root module ${self.closure.rootModule}`)
  }
  return result
}

/** Returns one module's HIR, or `undefined` for an unknown identity. */
export const hirOf = (self: Snapshot, module: string): Hir.Module | undefined =>
  self.results.get(module)?.hir

/** Returns one module's ownership facts and cleanup plans, or `undefined` for an unknown identity. */
export const ownershipOf = (
  self: Snapshot,
  module: string,
): Ownership.ModuleOwnership | undefined => self.ownership.get(module)

/** Returns the snapshot's instance discovery: entry state and ordered instances. */
export const instancesOf = (self: Snapshot): Instances.Discovery => self.instances

/** Returns the snapshot's lowered MIR program module. */
export const loweredMir = (self: Snapshot): Mir.Module => self.mir

/** Looks up one declaration name within one module. */
export const declarationByName = (
  self: Snapshot,
  module: string,
  spelling: string,
): DeclarationIndex.DeclarationLookup => DeclarationIndex.lookup(self.index, module, spelling)

/** Looks up one declaration name within one module's elaborated analysis. */
export const declarationLookup = (
  result: Elaboration.Result,
  spelling: string,
): DeclarationIndex.DeclarationLookup => Elaboration.declarationByName(result, spelling)

/** Looks up one parameter name within one declaration's collected parameters. */
export const parameterLookup = (
  declaration: DeclarationIndex.DeclarationFact,
  spelling: string,
): DeclarationIndex.ParameterLookup => Elaboration.parameterByName(declaration, spelling)

/** The compilation's complete diagnostic sequence in deterministic driver order. */
export const diagnostics = (self: Snapshot): ReadonlyArray<Diagnostic.Diagnostic> =>
  self.diagnostics

/** Evaluates the snapshot's root module through the closed bootstrap evaluator. */
export const evaluate = (self: Snapshot): BootstrapEvaluation.Outcome =>
  BootstrapEvaluation.evaluate(rootAnalysis(self))
