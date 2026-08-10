import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import * as Result from 'effect/Result'
import * as Diagnostic from './Diagnostic.js'
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
    [...byModule.values()].sort((left, right) =>
      left.id < right.id ? -1 : left.id > right.id ? 1 : 0,
    ),
  )
}

const spelling = (source: SourceFile.SourceFile, token: Token.Token): string => {
  const bytes = Option.getOrThrowWith(
    SourceFile.slice(source, token.span),
    () => new RangeError(`Import token span does not belong to source ${source.id}`),
  )
  return Array.from(bytes, (byte) => String.fromCharCode(byte)).join('')
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
    const tokens =
      path?.children.filter(
        (child): child is Token.Token => SyntaxTree.isToken(child) && child.kind === 'Identifier',
      ) ?? []
    if (path === undefined || tokens.length === 0 || !SyntaxTree.isAvailableSyntax(path)) {
      return [Object.freeze({ syntax: element, path: path ?? element })]
    }
    const sourceSpelling = tokens.map((token) => spelling(syntax.source, token)).join('.')
    const canonicalTarget = tokens.map((token) => spelling(syntax.source, token)).join('/')
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

const needsEffectLibrary = (source: SourceFile.SourceFile): boolean => {
  const text = Array.from(SourceFile.toUint8Array(source), (byte) =>
    String.fromCharCode(byte),
  ).join('')
  return /\bEffect\./.test(text)
}

/** Computes strongly connected components of size > 1 over resolved imports, deterministically. */
const cycleFacts = (modules: ReadonlyArray<Module>): ReadonlyArray<ReadonlyArray<string>> => {
  const names = modules.map((module) => module.name)
  const edges = new Map(modules.map((module) => [module.name, resolvedTargets(module)]))
  const index = new Map<string, number>()
  const lowLink = new Map<string, number>()
  const onStack = new Set<string>()
  const stack: Array<string> = []
  const components: Array<ReadonlyArray<string>> = []
  let counter = 0

  const connect = (name: string): void => {
    index.set(name, counter)
    lowLink.set(name, counter)
    counter += 1
    stack.push(name)
    onStack.add(name)

    for (const target of edges.get(name) ?? []) {
      if (!edges.has(target)) continue
      if (!index.has(target)) {
        connect(target)
        lowLink.set(name, Math.min(lowLink.get(name) ?? 0, lowLink.get(target) ?? 0))
      } else if (onStack.has(target)) {
        lowLink.set(name, Math.min(lowLink.get(name) ?? 0, index.get(target) ?? 0))
      }
    }

    if (lowLink.get(name) === index.get(name)) {
      const component: Array<string> = []
      let member = stack.pop()
      while (member !== undefined) {
        onStack.delete(member)
        component.push(member)
        if (member === name) break
        member = stack.pop()
      }
      if (component.length > 1) components.push(Object.freeze(component.sort()))
    }
  }

  for (const name of names) {
    if (!index.has(name)) connect(name)
  }

  return Object.freeze(
    components.sort((left, right) =>
      (left.at(0) ?? '') < (right.at(0) ?? '')
        ? -1
        : (left.at(0) ?? '') > (right.at(0) ?? '')
          ? 1
          : 0,
    ),
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
    if (
      !Stdlib.isReserved(name) &&
      needsEffectLibrary(analysis.module.syntax.source) &&
      !loaded.has('silk/effects') &&
      !pending.includes('silk/effects')
    )
      pending.push('silk/effects')
    for (const target of resolvedTargets(analysis.module)) {
      if (!loaded.has(target) && !pending.includes(target)) pending.push(target)
    }
  }

  const modules = Object.freeze(
    [...loaded.values()].sort((left, right) =>
      left.name < right.name ? -1 : left.name > right.name ? 1 : 0,
    ),
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
        .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
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
