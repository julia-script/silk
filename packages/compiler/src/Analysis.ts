import * as Effect from 'effect/Effect'
import * as Backend from './Backend.js'
import * as BackendRegistry from './BackendRegistry.js'
import * as BootstrapEvaluation from './BootstrapEvaluation.js'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import type * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as Layout from './Layout.js'
import * as Lower from './Lower.js'
import type * as Mir from './Mir.js'
import * as ModuleClosure from './ModuleClosure.js'
import * as NameResolution from './NameResolution.js'
import * as Ownership from './Ownership.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as Target from './Target.js'
import type * as Token from './Token.js'
import type * as Type from './Type.js'
import * as WasmBackend from './WasmBackend.js'

/**
 * The supported analysis facade. Tooling consumes compiler phases exclusively through this
 * module: build a snapshot from one compilation request, then query immutable facts. The
 * data-model vocabularies (syntax elements, diagnostics, fact types) are part of the facade's
 * answers; running phase modules directly is not a supported consumer surface.
 */

/** An available phase artifact or the target failure that prevented its construction. */
export type Targeted<A> =
  | { readonly _tag: 'Available'; readonly value: A }
  | { readonly _tag: 'Unavailable'; readonly error: Target.TargetError }

/** One immutable analysis snapshot of one compilation request. */
export interface Snapshot {
  readonly _tag: 'AnalysisSnapshot'
  readonly closure: ModuleClosure.Closure
  readonly index: DeclarationIndex.Index
  readonly resolution: NameResolution.Resolution
  readonly results: ReadonlyMap<string, Elaboration.Result>
  readonly ownership: ReadonlyMap<string, Ownership.ModuleOwnership>
  readonly instances: Instances.Discovery
  readonly target: Target.Selection
  readonly layoutCatalog: Targeted<Layout.Catalog>
  readonly layout: Targeted<Layout.Plan>
  readonly mir: Targeted<Mir.Module>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

/** Builds the snapshot of one compilation request. */
export const make = (request: ModuleClosure.CompilationRequest): Snapshot => {
  const closure = ModuleClosure.load(request)
  const declarations = NameResolution.analyze(closure)
  const index = declarations.index
  const resolution = declarations.resolution
  const results = new Map(
    closure.modules.map((module) => {
      const headers = index.modules.find((candidate) => candidate.module === module.name)
      const scope = NameResolution.scopeOf(resolution, module.name)
      if (headers === undefined || scope === undefined)
        throw new RangeError(`Analysis lost module facts for ${module.name}`)
      return [
        module.name,
        Elaboration.elaborateModule({ syntax: module.syntax, headers, scope, index }),
      ]
    }),
  )
  const ownership = new Map(
    [...results.entries()].map(([name, result]) => [name, Ownership.checkModule(result)]),
  )
  const instances = Instances.discover(request.rootModule, results)
  const target = Target.select(request.target)
  const layoutCatalog: Targeted<Layout.Catalog> =
    target._tag === 'Resolved'
      ? Object.freeze({ _tag: 'Available', value: Layout.catalog(target.target, index) })
      : Object.freeze({ _tag: 'Unavailable', error: target.error })
  const layout: Targeted<Layout.Plan> =
    layoutCatalog._tag === 'Available'
      ? Object.freeze({ _tag: 'Available', value: Layout.plan(layoutCatalog.value, instances) })
      : layoutCatalog
  const mir: Targeted<Mir.Module> =
    layout._tag === 'Available'
      ? Object.freeze({
          _tag: 'Available',
          value: Lower.lowerProgram(instances, ownership, layout.value),
        })
      : layout
  const diagnostics = Diagnostic.merge(
    ...closure.modules.map((module) => module.syntax.lexicalDiagnostics),
    ...closure.modules.map((module) => module.syntax.parserDiagnostics),
    closure.diagnostics,
    resolution.diagnostics,
    ...[...results.values()].map((result) => result.diagnostics),
    ...[...ownership.values()].map((facts) => facts.diagnostics),
  )
  return Object.freeze({
    _tag: 'AnalysisSnapshot',
    closure,
    index,
    resolution,
    results,
    ownership,
    instances,
    target,
    layoutCatalog,
    layout,
    mir,
    diagnostics,
  })
}

/** Builds the snapshot of one single-module source. */
export const ofSource = (sourceId: string, bytes: Uint8Array, target?: string): Snapshot =>
  target === undefined
    ? make({ rootModule: sourceId, sources: new Map([[sourceId, bytes]]) })
    : make({ rootModule: sourceId, sources: new Map([[sourceId, bytes]]), target })

/** Returns every loaded module of the snapshot in canonical identity order. */
export const modules = (self: Snapshot): ReadonlyArray<ModuleClosure.Module> => self.closure.modules

/** Returns the snapshot's import cycle facts in canonical order. */
export const cycles = (self: Snapshot): ReadonlyArray<ReadonlyArray<string>> => self.closure.cycles

/** Returns the closure's declaration index. */
export const declarationIndex = (self: Snapshot): DeclarationIndex.Index => self.index

export const nameResolution = (self: Snapshot): NameResolution.Resolution => self.resolution
export const moduleScope = (
  self: Snapshot,
  module: string,
): NameResolution.ModuleScope | undefined => NameResolution.scopeOf(self.resolution, module)
export const lookupName = (
  self: Snapshot,
  module: string,
  spelling: string,
): NameResolution.Lookup => {
  const scope = moduleScope(self, module)
  return scope === undefined
    ? Object.freeze({ _tag: 'Missing', spelling })
    : NameResolution.lookup(scope, self.index, spelling)
}
export const lookupQualifiedName = (
  self: Snapshot,
  module: string,
  namespace: string,
  member: string,
  token: Token.Token,
): NameResolution.Lookup => {
  const scope = moduleScope(self, module)
  return scope === undefined
    ? Object.freeze({ _tag: 'Missing', spelling: `${namespace}.${member}` })
    : NameResolution.lookupQualified(scope, self.index, namespace, member, token)
}

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

/** Returns the snapshot's resolved or unavailable target selection. */
export const targetOf = (self: Snapshot): Target.Selection => self.target

/** Returns the target-selected declaration-wide nominal layout catalog. */
export const layoutCatalogOf = (self: Snapshot): Targeted<Layout.Catalog> => self.layoutCatalog

/** Looks up one nominal declaration's available or unavailable target-selected layout. */
export const nominalLayout = (
  self: Snapshot,
  type: Type.Nominal,
): Layout.CatalogEntry | undefined =>
  self.layoutCatalog._tag === 'Available'
    ? Layout.catalogEntry(self.layoutCatalog.value, type)
    : undefined

/** Returns the snapshot's available or explicitly unavailable layout plan. */
export const layoutOf = (self: Snapshot): Targeted<Layout.Plan> => self.layout

/** Returns the snapshot's available or explicitly unavailable lowered MIR state. */
export const mirOf = (self: Snapshot): Targeted<Mir.Module> => self.mir

/** Returns the snapshot's lowered MIR program for callers that already established availability. */
export const loweredMir = (self: Snapshot): Mir.Module => {
  if (self.mir._tag === 'Available') return self.mir.value
  throw new RangeError(self.mir.error.message)
}

/** Looks up one declaration name within one module. */
export const declarationByName = (
  self: Snapshot,
  module: string,
  spelling: string,
): DeclarationIndex.DeclarationLookup => DeclarationIndex.lookup(self.index, module, spelling)

/** Looks up a function or struct in the shared module-level namespace. */
export const memberByName = (
  self: Snapshot,
  module: string,
  spelling: string,
): DeclarationIndex.MemberLookup => DeclarationIndex.member(self.index, module, spelling)

/** Looks up one nominal struct declaration. */
export const structByName = (
  self: Snapshot,
  module: string,
  spelling: string,
): DeclarationIndex.StructLookup => DeclarationIndex.struct(self.index, module, spelling)

/** Looks up one declaration-ordered field from a resolved nominal struct. */
export const fieldByName = (
  declaration: DeclarationIndex.StructFact,
  spelling: string,
): DeclarationIndex.FieldLookup => DeclarationIndex.lookupField(declaration.fields, spelling)

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

/** Emits the snapshot's lowered program through the nominal backend service. */
/**
 * Emits the snapshot's lowered program.
 *
 * The backend follows from the snapshot's target unless one is named explicitly, so the ordinary
 * call site picks a target and is done: pairing the two by hand is what lets them disagree.
 */
export const codegen = Effect.fn('Analysis.codegen')(function* (
  self: Snapshot,
  request: Backend.CodegenRequest,
  backend?: Backend.Backend,
): Effect.fn.Return<Backend.Artifact, Backend.BackendError | Target.TargetError> {
  if (self.mir._tag === 'Unavailable') return yield* self.mir.error
  const target = self.mir.value.layout.target
  const selected = backend ?? BackendRegistry.forTarget(target)
  if (selected === undefined) {
    return yield* new Backend.BackendError({
      operation: 'Backend.emit',
      backend: 'Analysis.codegen',
      message: `no backend supports target ${target.id}`,
      reason: { _tag: 'UnsupportedTarget', target: target.id },
    })
  }
  return yield* Backend.emit(selected, self.mir.value, {
    ...request,
    sources:
      request.sources ??
      new Map(
        self.closure.modules.map((module) => [
          module.name,
          Uint8Array.from(module.syntax.source.bytes),
        ]),
      ),
  })
})

/**
 * Emits the snapshot's lowered program as WebAssembly. The artifact's `ir` carries the WAT
 * inspection text and its `bitcode` carries the instantiable wasm binary, mirroring how
 * {@link codegen} pairs LLVM IR text with bitcode.
 *
 * Prefer {@link codegen} on a snapshot built for a WebAssembly target: the backend follows from
 * the target, so naming both is redundant and lets them disagree. This forces the WebAssembly
 * backend regardless of target, which fails on a snapshot lowered for a native one.
 */
export const codegenWasm = Effect.fn('Analysis.codegenWasm')(function* (
  self: Snapshot,
  request: Backend.CodegenRequest,
): Effect.fn.Return<Backend.Artifact, Backend.BackendError | Target.TargetError> {
  return yield* codegen(self, request, WasmBackend.WasmBackend)
})

/** Executes the snapshot's lowered MIR program through the closed bootstrap interpreter. */
export const evaluate = (self: Snapshot): BootstrapEvaluation.Outcome =>
  BootstrapEvaluation.evaluate(self.instances, loweredMir(self))
