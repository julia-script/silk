import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import * as Result from 'effect/Result'
import * as Diagnostic from './Diagnostic.js'
import * as ImportPath from './ImportPath.js'
import * as Graph from './internal/Graph.js'
import * as Lexer from './Lexer.js'
import * as Parser from './Parser.js'
import * as SourceFile from './SourceFile.js'
import * as SourceResolver from './SourceResolver.js'
import * as SourceSpan from './SourceSpan.js'
import * as Stdlib from './Stdlib.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'

/** One compilation request: an explicit root source plus optional target selection. */
export interface CompilationRequest {
  readonly root: SourceFile.SourceFile
  readonly target?: string
}

/** One project frontend request with one or more independently queryable roots. */
export interface ProjectRequest {
  readonly roots: ReadonlyArray<SourceFile.SourceFile>
  readonly previous?: ProjectClosure
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
  readonly imports: ReadonlyArray<ImportFact>
}

/** Immutable facts shared by single-root and multi-root module closures. */
export interface Facts {
  readonly modules: ReadonlyArray<Module>
  readonly cycles: ReadonlyArray<ReadonlyArray<string>>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly sources: ReadonlyMap<string, SourceFile.SourceFile>
  readonly resolutionFailures: ReadonlyArray<SourceResolver.SourceResolverError>
}

/** The complete deterministic closure of one compilation request. */
export interface Closure extends Facts {
  readonly _tag: 'ModuleClosure'
  readonly rootModule: string
}

/** The complete deterministic union closure of one project frontend request. */
export interface ProjectClosure extends Facts {
  readonly _tag: 'ProjectModuleClosure'
  readonly rootModules: ReadonlyArray<string>
}

const validateRequest = (request: CompilationRequest): void => {
  if (!SourceResolver.isCanonicalModule(request.root.id))
    throw new RangeError(`Compilation request module identity ${request.root.id} is not canonical`)
}

const compareText = (left: string, right: string): number => {
  if (left < right) return -1
  if (left > right) return 1
  return 0
}

const sameBytes = (left: SourceFile.SourceFile, right: SourceFile.SourceFile): boolean => {
  const leftBytes = SourceFile.toUint8Array(left)
  const rightBytes = SourceFile.toUint8Array(right)
  if (leftBytes.length !== rightBytes.length) return false
  for (let index = 0; index < leftBytes.length; index += 1) {
    if (leftBytes[index] !== rightBytes[index]) return false
  }
  return true
}

const canonicalRoots = (
  roots: ReadonlyArray<SourceFile.SourceFile>,
): ReadonlyArray<SourceFile.SourceFile> => {
  if (roots.length === 0) throw new RangeError('Project analysis requires at least one root source')
  const byModule = new Map<string, SourceFile.SourceFile>()
  for (const root of roots) {
    validateRequest({ root })
    const existing = byModule.get(root.id)
    if (existing !== undefined && !sameBytes(existing, root))
      throw new RangeError(`Project analysis received conflicting roots for ${root.id}`)
    if (existing === undefined) byModule.set(root.id, root)
  }
  return Object.freeze(
    [...byModule.values()].sort((left, right) => {
      return compareText(left.id, right.id)
    }),
  )
}

const unavailableSyntax = (parent: SyntaxTree.Node): SyntaxTree.Element =>
  SyntaxTree.unavailableElement(parent.children, parent)

interface ParsedModule {
  readonly name: string
  readonly syntax: SyntaxFile.SyntaxFile
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

const parseModule = (
  name: string,
  source: SourceResolver.ResolvedSource,
  previous?: Module,
): ParsedModule => {
  const currentSource = SourceFile.make(name, source.bytes, source.origin)
  const syntax =
    previous !== undefined && SourceFile.equals(previous.syntax.source, currentSource)
      ? previous.syntax
      : Parser.parse(Lexer.lex(currentSource))
  const imports = syntax.root.children.flatMap((element): ParsedModule['imports'] => {
    if (!SyntaxTree.isNode(element) || element.kind !== 'ImportDeclaration') return []
    const path = SyntaxTree.directNode(element, 'ImportPath')
    if (path === undefined || !SyntaxTree.isAvailableSyntax(path)) {
      return [Object.freeze({ syntax: element, path: path ?? element })]
    }
    const sourceSpelling = ImportPath.spelling(syntax.source, path)
    const canonicalTarget = ImportPath.canonicalTarget(syntax.source, path)
    if (sourceSpelling === undefined || canonicalTarget === undefined) {
      return [Object.freeze({ syntax: element, path })]
    }
    const tokens = ImportPath.segments(path)
    const token = tokens.at(0)
    if (token === undefined) throw new RangeError('Available import path lost its first segment')
    return [Object.freeze({ syntax: element, path, sourceSpelling, canonicalTarget, token })]
  })
  return Object.freeze({ name, syntax, imports: Object.freeze(imports) })
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
 * Loads the union reachable closure of one project request. Roots, the frontier, and the final
 * module order are canonically sorted, so neither supply order nor traversal order affects the
 * result.
 */
export const loadProject = Effect.fn('ModuleClosure.loadProject')(function* (
  request: ProjectRequest,
): Effect.fn.Return<ProjectClosure, never, SourceResolver.SourceResolver> {
  const roots = canonicalRoots(request.roots)
  const rootModules = Object.freeze(roots.map((root) => root.id))
  const previousModules = new Map(request.previous?.modules.map((module) => [module.name, module]))
  const loaded = new Map<string, Module>()
  const diagnostics: Array<ReadonlyArray<Diagnostic.Diagnostic>> = []
  for (const root of roots) {
    if (!Stdlib.isReserved(root.id)) continue
    const span = Option.getOrThrow(SourceSpan.make(root, 0, 0))
    diagnostics.push(Object.freeze([Diagnostic.reservedModuleIdentity(root.id, span)]))
  }
  const resolutions = new Map<string, Resolution>(
    roots.map((root) => [
      root.id,
      Object.freeze({
        _tag: 'Found' as const,
        source: SourceResolver.resolved(SourceFile.toUint8Array(root), root.origin),
      }),
    ]),
  )
  const pending: Array<string> = [...rootModules]

  const resolve = Effect.fnUntraced(function* (
    module: string,
  ): Effect.fn.Return<Resolution, never, SourceResolver.SourceResolver> {
    const cached = resolutions.get(module)
    if (cached !== undefined) return cached
    // Standard-library identities resolve from the compiler-shipped sources exclusively; a
    // user resolver is never consulted inside the reserved namespace.
    if (Stdlib.isReserved(module)) {
      const attempted = yield* Effect.result(SourceResolver.resolveStandardLibrary(module))
      const resolution: Resolution = Result.isFailure(attempted)
        ? Object.freeze({ _tag: 'Failed', error: attempted.failure })
        : Option.match(attempted.success, {
            onNone: () => Object.freeze({ _tag: 'Absent' as const }),
            onSome: (source) => Object.freeze({ _tag: 'Found' as const, source }),
          })
      resolutions.set(module, resolution)
      return resolution
    }
    const attempted = yield* Effect.result(SourceResolver.resolve(module))
    const resolution: Resolution = Result.isFailure(attempted)
      ? Object.freeze({ _tag: 'Failed', error: attempted.failure })
      : Option.match(attempted.success, {
          onNone: () => Object.freeze({ _tag: 'Absent' as const }),
          onSome: (source) => Object.freeze({ _tag: 'Found' as const, source }),
        })
    resolutions.set(module, resolution)
    return resolution
  })

  while (pending.length > 0) {
    pending.sort()
    const name = pending.shift()
    if (name === undefined || loaded.has(name)) continue
    const resolution = resolutions.get(name) ?? (yield* resolve(name))
    if (resolution?._tag !== 'Found') continue
    const analysis = yield* analyzeModule(
      parseModule(name, resolution.source, previousModules.get(name)),
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
    rootModules,
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
      })
    : undefined

/** Loads the complete reachable closure of one compilation request. */
export const load = Effect.fn('ModuleClosure.load')(function* (
  request: CompilationRequest,
): Effect.fn.Return<Closure, never, SourceResolver.SourceResolver> {
  const project = yield* loadProject({ roots: [request.root] })
  const closure = view(project, request.root.id)
  if (closure === undefined) throw new RangeError(`Project closure lost root ${request.root.id}`)
  return closure
})
