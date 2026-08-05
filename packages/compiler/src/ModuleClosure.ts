import * as Option from 'effect/Option'
import * as Diagnostic from './Diagnostic.js'
import * as Lexer from './Lexer.js'
import * as Parser from './Parser.js'
import * as SourceFile from './SourceFile.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'

/** One compilation request: a root module and the available sources by logical identity. */
export interface CompilationRequest {
  readonly rootModule: string
  readonly sources: ReadonlyMap<string, Uint8Array>
  readonly target?: string
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
      readonly _tag: 'Unavailable'
      readonly syntax: SyntaxTree.Element
    }

/** One import declaration of a loaded module with its exact concrete provenance. */
export interface ImportFact {
  readonly _tag: 'Import'
  readonly syntax: SyntaxTree.Node
  readonly target: ImportTarget
}

/** One loaded module of the closure. */
export interface Module {
  readonly _tag: 'Module'
  readonly name: string
  readonly syntax: SyntaxFile.SyntaxFile
  readonly imports: ReadonlyArray<ImportFact>
}

/** The complete deterministic closure of one compilation request. */
export interface Closure {
  readonly _tag: 'ModuleClosure'
  readonly rootModule: string
  readonly modules: ReadonlyArray<Module>
  readonly cycles: ReadonlyArray<ReadonlyArray<string>>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const spelling = (source: SourceFile.SourceFile, token: Token.Token): string => {
  const bytes = Option.getOrThrowWith(
    SourceFile.slice(source, token.span),
    () => new RangeError(`Import token span does not belong to source ${source.id}`),
  )
  return Array.from(bytes, (byte) => String.fromCharCode(byte)).join('')
}

const directToken = (parent: SyntaxTree.Node, kind: Token.TokenKind): Token.Token | undefined =>
  parent.children.find(
    (element): element is Token.Token => SyntaxTree.isToken(element) && element.kind === kind,
  )

const unavailableSyntax = (parent: SyntaxTree.Node): SyntaxTree.Element =>
  parent.children.find((element) => SyntaxTree.isMissingToken(element)) ?? parent

interface ModuleAnalysis {
  readonly module: Module
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const analyzeModule = (
  name: string,
  bytes: Uint8Array,
  sources: ReadonlyMap<string, Uint8Array>,
): ModuleAnalysis => {
  const syntax = Parser.parse(Lexer.lex(SourceFile.make(name, bytes)))
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const imports = syntax.root.children.flatMap((element): ReadonlyArray<ImportFact> => {
    if (!SyntaxTree.isNode(element) || element.kind !== 'ImportDeclaration') return []
    const token = directToken(element, 'Identifier')
    if (token === undefined) {
      return [
        Object.freeze({
          _tag: 'Import' as const,
          syntax: element,
          target: Object.freeze({
            _tag: 'Unavailable' as const,
            syntax: unavailableSyntax(element),
          }),
        }),
      ]
    }
    const module = spelling(syntax.source, token)
    if (module === name) {
      const diagnostic = Diagnostic.selfImport(module, token.span)
      diagnostics.push(diagnostic)
      return [
        Object.freeze({
          _tag: 'Import' as const,
          syntax: element,
          target: Object.freeze({
            _tag: 'Self' as const,
            module,
            token,
            cause: Diagnostic.identity(diagnostic),
          }),
        }),
      ]
    }
    if (!sources.has(module)) {
      const diagnostic = Diagnostic.unknownModule(module, token.span)
      diagnostics.push(diagnostic)
      return [
        Object.freeze({
          _tag: 'Import' as const,
          syntax: element,
          target: Object.freeze({
            _tag: 'Unknown' as const,
            module,
            token,
            cause: Diagnostic.identity(diagnostic),
          }),
        }),
      ]
    }
    return [
      Object.freeze({
        _tag: 'Import' as const,
        syntax: element,
        target: Object.freeze({ _tag: 'Resolved' as const, module, token }),
      }),
    ]
  })

  return Object.freeze({
    module: Object.freeze({
      _tag: 'Module' as const,
      name,
      syntax,
      imports: Object.freeze(imports),
    }),
    diagnostics: Object.freeze(diagnostics),
  })
}

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
 * Loads the complete reachable closure of one compilation request. The frontier and the final
 * module order are both canonically sorted, so neither supply order nor traversal order affects
 * the result. A missing root module violates the caller contract and is rejected.
 */
export const load = (request: CompilationRequest): Closure => {
  const rootBytes = request.sources.get(request.rootModule)
  if (rootBytes === undefined) {
    throw new RangeError(
      `Compilation request root module ${request.rootModule} is not among the supplied sources`,
    )
  }

  const loaded = new Map<string, Module>()
  const diagnostics: Array<ReadonlyArray<Diagnostic.Diagnostic>> = []
  const pending: Array<string> = [request.rootModule]

  while (pending.length > 0) {
    pending.sort()
    const name = pending.shift()
    if (name === undefined || loaded.has(name)) continue
    const bytes = request.sources.get(name)
    if (bytes === undefined) continue
    const analysis = analyzeModule(name, bytes, request.sources)
    loaded.set(name, analysis.module)
    diagnostics.push(analysis.diagnostics)
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
    _tag: 'ModuleClosure',
    rootModule: request.rootModule,
    modules,
    cycles: cycleFacts(modules),
    diagnostics: Diagnostic.merge(...diagnostics),
  })
}
