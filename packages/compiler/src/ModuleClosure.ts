import type * as ConfigurationOrigin from './ConfigurationOrigin.js'
import type * as ArtifactComposition from './ArtifactComposition.js'
import type * as CompilationProfile from './CompilationProfile.js'
import type * as PackageConfiguration from './PackageConfiguration.js'
import * as Effect from 'effect/Effect'
import * as Data from 'effect/Data'
import * as Option from 'effect/Option'
import * as Result from 'effect/Result'
import * as AuthoredLowering from './AuthoredLowering.js'
import * as Diagnostic from './Diagnostic.js'
import * as ImportPath from './ImportPath.js'
import * as Graph from './internal/Graph.js'
import * as Lexer from './Lexer.js'
import * as Parser from './Parser.js'
import * as SourceFile from './SourceFile.js'
import * as SourceResolver from './SourceResolver.js'
import * as Stdlib from './Stdlib.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'

/** One compilation request: a canonical root identity plus optional target selection. */
export interface CompilationRequest {
  readonly root: string
  readonly target?: string
  readonly configuration?: {
    readonly composition?: ArtifactComposition.Input
    readonly compositionOrigin?: ConfigurationOrigin.ConfigurationOrigin
    readonly package?: string
    readonly profile: CompilationProfile.Input
    readonly bindings?: ReadonlyArray<PackageConfiguration.Binding>
    readonly modules?: ReadonlyArray<Omit<PackageConfiguration.Module, 'bytes'>>
  }
}

/** One project frontend request with one or more independently queryable roots. */
export interface ProjectRequest {
  readonly application?: string
  readonly configuration?: CompilationRequest['configuration']
  readonly roots: ReadonlyArray<string>
  /** Composition roots retain recoverable absence/failure facts. */
  readonly additionalRoots?: ReadonlyArray<string>
  /** Prior loaded facts whose unchanged modules keep their syntax and authored artifacts. */
  readonly previous?: Facts
  /** Completed condition decisions for this discovery pass; absent decisions admit neither arm. */
  readonly selection?: ReadonlyMap<string, ReadonlyMap<number, boolean>>
}

/** The resolved, diagnosed, or syntax-unavailable target of one import declaration. */
export type ImportTarget =
  | {
      readonly _tag: 'Resolved'
      readonly module: string
      readonly token: Token.Token
    }
  | {
      readonly _tag: 'Unknown'
      readonly module: string
      readonly token: Token.Token
      readonly cause: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Self'
      readonly module: string
      readonly token: Token.Token
      readonly cause: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Failed'
      readonly module: string
      readonly token: Token.Token
      readonly error: SourceResolver.SourceResolverError
    }
  | {
      readonly _tag: 'Unavailable'
      readonly syntax: SyntaxTree.Element
    }

/** One import declaration of a loaded module with its exact concrete provenance. */
export interface ImportFact {
  readonly _tag: 'Import'
  readonly syntax: SyntaxTree.Node
  readonly path: SyntaxTree.Node
  readonly sourceSpelling?: string
  readonly canonicalTarget?: string
  readonly target: ImportTarget
}

/** One loaded module of the closure. */
export interface Module {
  readonly _tag: 'Module'
  readonly name: string
  readonly syntax: SyntaxFile.SyntaxFile
  /** The source-free authored module and its current presentation, lowered once per parse. */
  readonly authored: AuthoredLowering.Lowered
  readonly imports: ReadonlyArray<ImportFact>
  /** Selected module declarations, retaining their identities in the complete parsed syntax. */
  readonly declarations: ReadonlyArray<SyntaxTree.Node>
}

/** Immutable facts shared by single-root and multi-root module closures. */
export interface Facts {
  readonly modules: ReadonlyArray<Module>
  readonly cycles: ReadonlyArray<ReadonlyArray<string>>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly sources: ReadonlyMap<string, SourceFile.SourceFile>
  readonly resolutionFailures: ReadonlyArray<SourceResolver.SourceResolverError>
  readonly missingRoots: ReadonlyArray<string>
}

/** The deterministic closure admitted by one declaration-selection discovery pass. */
export interface Closure extends Facts {
  readonly _tag: 'ModuleClosure'
  readonly rootModule: string
}

/** The deterministic union closure admitted by one project discovery pass. */
export interface ProjectClosure extends Facts {
  readonly _tag: 'ProjectModuleClosure'
  readonly rootModules: ReadonlyArray<string>
}

/** A requested root cannot establish a complete analysis snapshot. */
export class ModuleClosureError extends Data.TaggedError('ModuleClosureError')<{
  readonly operation: 'ModuleClosure.loadProject'
  readonly message: string
  readonly reason:
    | { readonly _tag: 'EmptyRoots' }
    | { readonly _tag: 'InvalidRoot'; readonly module: string }
    | { readonly _tag: 'MissingRoot'; readonly module: string }
    | {
        readonly _tag: 'RootResolutionFailed'
        readonly module: string
        readonly error: SourceResolver.SourceResolverError
      }
}> {}

const compareText = (left: string, right: string): number => {
  if (left < right) return -1
  if (left > right) return 1
  return 0
}

/** Validates the entire request before any source is accessed. */
export const validateRoots = Effect.fn('ModuleClosure.validateRoots')(function* (
  roots: ReadonlyArray<string>,
  additionalRoots: ReadonlyArray<string> = [],
): Effect.fn.Return<ReadonlyArray<string>, ModuleClosureError> {
  if (roots.length === 0)
    return yield* new ModuleClosureError({
      operation: 'ModuleClosure.loadProject',
      message: 'Project analysis requires at least one root module',
      reason: { _tag: 'EmptyRoots' },
    })
  for (const module of [...new Set([...roots, ...additionalRoots])].sort())
    if (!SourceResolver.isCanonicalModule(module))
      return yield* new ModuleClosureError({
        operation: 'ModuleClosure.loadProject',
        message: `Root module identity ${module} is not canonical`,
        reason: { _tag: 'InvalidRoot', module },
      })
  return Object.freeze([...new Set(roots)].sort())
})

const unavailableSyntax = (parent: SyntaxTree.Node): SyntaxTree.Element =>
  SyntaxTree.unavailableElement(parent.children, parent)

interface ParsedModule {
  readonly name: string
  readonly syntax: SyntaxFile.SyntaxFile
  readonly authored: AuthoredLowering.Lowered
  readonly declarations: ReadonlyArray<SyntaxTree.Node>
  readonly imports: ReadonlyArray<{
    readonly syntax: SyntaxTree.Node
    readonly path: SyntaxTree.Node
    readonly sourceSpelling?: string
    readonly canonicalTarget?: string
    readonly token?: Token.Token
  }>
}

interface ModuleAnalysis {
  readonly module: Module
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const parseModule = Effect.fnUntraced(function* (
  name: string,
  source: SourceResolver.ResolvedSource,
  previous?: Module,
  selection: ReadonlyMap<number, boolean> = new Map(),
  application?: string,
): Effect.fn.Return<ParsedModule> {
  const currentSource = SourceFile.make(name, source.bytes, source.origin)
  const reused = previous !== undefined && SourceFile.equals(previous.syntax.source, currentSource)
  const syntax = reused ? previous.syntax : Parser.parse(Lexer.lex(currentSource))
  // Lowering a parser artifact cannot fail for source mistakes; a rejection is a compiler defect.
  const authored = reused
    ? previous.authored
    : yield* Effect.orDie(
        AuthoredLowering.lower(syntax, AuthoredLowering.moduleOwner(name, currentSource.origin)),
      )
  const declarations = selectedDeclarations(syntax.root, selection)
  const imports = declarations.flatMap((element): ParsedModule['imports'] => {
    if (!SyntaxTree.isNode(element) || element.kind !== 'ImportDeclaration') return []
    const path = SyntaxTree.directNode(element, 'ImportPath')
    if (path === undefined || !SyntaxTree.isAvailableSyntax(path)) {
      return [Object.freeze({ syntax: element, path: path ?? element })]
    }
    const sourceSpelling = ImportPath.spelling(syntax.source, path)
    const canonicalTarget =
      sourceSpelling === 'Intrinsic.application' && application !== undefined
        ? application
        : ImportPath.canonicalTarget(syntax.source, path)
    if (sourceSpelling === undefined || canonicalTarget === undefined) {
      return [Object.freeze({ syntax: element, path })]
    }
    const tokens = ImportPath.segments(path)
    const token = tokens.at(0)
    if (token === undefined) throw new RangeError('Available import path lost its first segment')
    return [Object.freeze({ syntax: element, path, sourceSpelling, canonicalTarget, token })]
  })
  return Object.freeze({ name, syntax, authored, declarations, imports: Object.freeze(imports) })
})

/** Flattens only decided declaration groups without changing the lossless syntax artifact. */
export const selectedDeclarations = (
  node: SyntaxTree.Node,
  selection: ReadonlyMap<number, boolean>,
): ReadonlyArray<SyntaxTree.Node> => {
  if (node.kind === 'StaticConditionalDeclaration') {
    const selected = selection.get(node.span.start)
    if (selected === undefined) return Object.freeze([])
    const children = node.children.filter(SyntaxTree.isNode)
    const arm = children[selected ? 1 : 2]
    return arm === undefined ? Object.freeze([]) : selectedDeclarations(arm, selection)
  }
  if (node.kind !== 'SourceFile' && node.kind !== 'DeclarationGroup') return Object.freeze([node])
  return Object.freeze(
    node.children
      .filter(SyntaxTree.isNode)
      .flatMap((child) => selectedDeclarations(child, selection)),
  )
}

type Resolution =
  | { readonly _tag: 'Found'; readonly source: SourceResolver.ResolvedSource }
  | { readonly _tag: 'Absent' }
  | { readonly _tag: 'Failed'; readonly error: SourceResolver.SourceResolverError }

const analyzeModule = Effect.fnUntraced(function* (
  parsed: ParsedModule,
  resolve: (module: string) => Effect.Effect<Resolution, never, SourceResolver.SourceResolver>,
): Effect.fn.Return<ModuleAnalysis, never, SourceResolver.SourceResolver> {
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const imports: Array<ImportFact> = []
  for (const imported of parsed.imports) {
    if (imported.canonicalTarget === undefined || imported.token === undefined) {
      imports.push(
        Object.freeze({
          _tag: 'Import',
          syntax: imported.syntax,
          path: imported.path,
          target: Object.freeze({
            _tag: 'Unavailable',
            syntax: unavailableSyntax(imported.path),
          }),
        }),
      )
      continue
    }
    const module = imported.canonicalTarget
    const sourceSpelling = imported.sourceSpelling
    if (sourceSpelling === undefined)
      throw new RangeError('Available import path lost its source spelling')
    if (module === parsed.name) {
      const diagnostic = Diagnostic.selfImport(module, imported.path.span)
      diagnostics.push(diagnostic)
      imports.push(
        Object.freeze({
          _tag: 'Import',
          syntax: imported.syntax,
          path: imported.path,
          sourceSpelling,
          canonicalTarget: module,
          target: Object.freeze({
            _tag: 'Self',
            module,
            token: imported.token,
            cause: Diagnostic.identity(diagnostic),
          }),
        }),
      )
      continue
    }
    const resolution = yield* resolve(module)
    if (resolution._tag === 'Absent') {
      const diagnostic = Diagnostic.unknownModule(module, imported.path.span)
      diagnostics.push(diagnostic)
      imports.push(
        Object.freeze({
          _tag: 'Import',
          syntax: imported.syntax,
          path: imported.path,
          sourceSpelling,
          canonicalTarget: module,
          target: Object.freeze({
            _tag: 'Unknown',
            module,
            token: imported.token,
            cause: Diagnostic.identity(diagnostic),
          }),
        }),
      )
      continue
    }
    imports.push(
      Object.freeze({
        _tag: 'Import',
        syntax: imported.syntax,
        path: imported.path,
        sourceSpelling,
        canonicalTarget: module,
        target:
          resolution._tag === 'Found'
            ? Object.freeze({ _tag: 'Resolved' as const, module, token: imported.token })
            : Object.freeze({
                _tag: 'Failed' as const,
                module,
                token: imported.token,
                error: resolution.error,
              }),
      }),
    )
  }

  return Object.freeze({
    module: Object.freeze({
      _tag: 'Module',
      name: parsed.name,
      syntax: parsed.syntax,
      authored: parsed.authored,
      declarations: parsed.declarations,
      imports: Object.freeze(imports),
    }),
    diagnostics: Object.freeze(diagnostics),
  })
})

const resolvedTargets = (module: Module): ReadonlyArray<string> =>
  Object.freeze(
    [
      ...new Set(
        module.imports.flatMap((fact) =>
          fact.target._tag === 'Resolved' ? [fact.target.module] : [],
        ),
      ),
    ].sort(),
  )

/** Computes strongly connected components of size > 1 over resolved imports, deterministically. */
const cycleFacts = (modules: ReadonlyArray<Module>): ReadonlyArray<ReadonlyArray<string>> => {
  const names = modules.map((module) => module.name)
  const edges = new Map(modules.map((module) => [module.name, resolvedTargets(module)]))
  return Object.freeze(
    Graph.stronglyConnected(names, (name) => edges.get(name) ?? [])
      .filter((component) => component.length > 1)
      .sort((left, right) => (left.at(0) ?? '').localeCompare(right.at(0) ?? '')),
  )
}

/**
 * Discovers imports admitted by the supplied declaration decisions. Undecided groups admit neither
 * arm; Frontend coordinates profile completion and subsequent selection passes. Roots and the final
 * module order are canonically sorted, so neither supply order nor traversal order affects the
 * result.
 */
export const loadProject = Effect.fn('ModuleClosure.loadProject')(function* (
  request: ProjectRequest,
): Effect.fn.Return<ProjectClosure, ModuleClosureError, SourceResolver.SourceResolver> {
  yield* Effect.annotateCurrentSpan({
    'request.application': request.application,
    'request.roots': request.roots,
    'request.previous': request.previous?.modules.map((module) => module.name),
  })
  const roots = yield* validateRoots(request.roots, request.additionalRoots)
  const previousModules = new Map(request.previous?.modules.map((module) => [module.name, module]))
  const loaded = new Map<string, Module>()
  const diagnostics: Array<ReadonlyArray<Diagnostic.Diagnostic>> = []
  const resolutions = new Map<string, Resolution>()
  const rootModules: Array<string> = [...roots]
  const missingRoots: Array<string> = []

  const resolve = Effect.fn('ModuleClosure.resolve')(function* (
    module: string,
  ): Effect.fn.Return<Resolution, never, SourceResolver.SourceResolver> {
    const cached = resolutions.get(module)
    if (cached !== undefined) return cached
    // Standard-library identities resolve from the compiler-shipped sources exclusively; a
    // user resolver is never consulted inside the reserved namespace.
    const attempted = yield* Effect.result(
      Stdlib.isReserved(module)
        ? SourceResolver.resolveStandardLibrary(module)
        : SourceResolver.resolve(module),
    )
    const resolution: Resolution = Result.isFailure(attempted)
      ? Object.freeze({ _tag: 'Failed', error: attempted.failure })
      : Option.match(attempted.success, {
          onNone: () => Object.freeze({ _tag: 'Absent' as const }),
          onSome: (source) => Object.freeze({ _tag: 'Found' as const, source }),
        })
    resolutions.set(module, resolution)
    return resolution
  })

  for (const module of roots) {
    const outcome = yield* resolve(module)
    if (outcome._tag === 'Absent')
      return yield* new ModuleClosureError({
        operation: 'ModuleClosure.loadProject',
        message: `Root module ${module} is missing`,
        reason: { _tag: 'MissingRoot', module },
      })
    if (outcome._tag === 'Failed')
      return yield* new ModuleClosureError({
        operation: 'ModuleClosure.loadProject',
        message: `Cannot resolve root module ${module}: ${outcome.error.message}`,
        reason: { _tag: 'RootResolutionFailed', module, error: outcome.error },
      })
  }
  for (const module of [...new Set(request.additionalRoots ?? [])].sort()) {
    if (roots.includes(module)) continue
    const outcome = yield* resolve(module)
    if (outcome._tag === 'Found') rootModules.push(module)
    else if (outcome._tag === 'Absent') missingRoots.push(module)
  }
  rootModules.sort()
  const pending: Array<string> = [...rootModules]

  while (pending.length > 0) {
    pending.sort()
    const name = pending.shift()
    if (name === undefined || loaded.has(name)) continue
    const resolution = resolutions.get(name) ?? (yield* resolve(name))
    if (resolution?._tag !== 'Found') continue
    const analysis = yield* analyzeModule(
      yield* parseModule(
        name,
        resolution.source,
        previousModules.get(name),
        request.selection?.get(name),
        request.application,
      ),
      resolve,
    )
    loaded.set(name, analysis.module)
    diagnostics.push(analysis.diagnostics)
    for (const target of resolvedTargets(analysis.module)) {
      if (!loaded.has(target) && !pending.includes(target)) pending.push(target)
    }
  }

  const modules = Object.freeze(
    [...loaded.values()].sort((left, right) => {
      return compareText(left.name, right.name)
    }),
  )

  return Object.freeze({
    _tag: 'ProjectModuleClosure',
    rootModules: Object.freeze(rootModules),
    missingRoots: Object.freeze(missingRoots),
    modules,
    cycles: cycleFacts(modules),
    diagnostics: Diagnostic.merge(...diagnostics),
    sources: new Map(modules.map((module) => [module.name, module.syntax.source])),
    resolutionFailures: Object.freeze(
      [...resolutions.entries()]
        .sort(([left], [right]) => {
          return compareText(left, right)
        })
        .flatMap(([, resolution]) => (resolution._tag === 'Failed' ? [resolution.error] : [])),
    ),
  })
})

/** Selects one root from a project closure without copying project-owned module facts. */
export const view = (self: ProjectClosure, rootModule: string): Closure | undefined =>
  self.rootModules.includes(rootModule)
    ? Object.freeze({
        _tag: 'ModuleClosure',
        rootModule,
        modules: self.modules,
        cycles: self.cycles,
        diagnostics: self.diagnostics,
        sources: self.sources,
        resolutionFailures: self.resolutionFailures,
        missingRoots: self.missingRoots,
      })
    : undefined

/** Discovers the unconditional bootstrap closure of one compilation request. Use Analysis.make for profile-selected frontend facts. */
export const load = Effect.fn('ModuleClosure.load')(function* (
  request: CompilationRequest,
  additionalRoots: ReadonlyArray<string> = [],
  previous?: Facts,
): Effect.fn.Return<Closure, ModuleClosureError, SourceResolver.SourceResolver> {
  const project = yield* loadProject({
    roots: [request.root],
    additionalRoots,
    application: request.root,
    ...(previous === undefined ? {} : { previous }),
  })
  const closure = view(project, request.root)
  if (closure === undefined) throw new RangeError(`Project closure lost root ${request.root}`)
  return closure
})
