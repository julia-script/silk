import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import * as Result from 'effect/Result'
import type * as CompilationProfile from './CompilationProfile.js'
import * as ConfigurationError from './ConfigurationError.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as Graph from './internal/Graph.js'
import * as Canonical from './internal/Canonical.js'
import * as ImportPath from './ImportPath.js'
import * as ModuleClosure from './ModuleClosure.js'
import * as NameResolution from './NameResolution.js'
import type * as ProfileBootstrap from './ProfileBootstrap.js'
import * as Residualization from './Residualization.js'
import * as SourceFile from './SourceFile.js'
import * as SourceResolver from './SourceResolver.js'
import type * as SourceSpan from './SourceSpan.js'
import * as StaticEvaluation from './StaticEvaluation.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'

/** One completed profile's declaration choices and their full source provenance. */
export interface ModuleSelection {
  readonly conditions: ReadonlyMap<string, ReadonlyArray<Elaboration.ExpressionFact>>
  readonly profile: CompilationProfile.CompilationProfile
  readonly decisions: ReadonlyMap<string, ReadonlyMap<number, boolean>>
  readonly inactiveRanges: ReadonlyMap<string, ReadonlyArray<SourceSpan.SourceSpan>>
  readonly dependencies: string
}

/** Visits only reachable conditions; a pending parent never admits either nested arm. */
const pending = (
  node: SyntaxTree.Node,
  decisions: ReadonlyMap<number, boolean>,
): ReadonlyArray<SyntaxTree.Node> => {
  if (node.kind === 'StaticConditionalDeclaration') {
    const decision = decisions.get(node.span.start)
    if (decision === undefined) return [node]
    const arm = node.children.filter(SyntaxTree.isNode)[decision ? 1 : 2]
    return arm === undefined ? [] : pending(arm, decisions)
  }
  if (node.kind !== 'SourceFile' && node.kind !== 'DeclarationGroup') return []
  return node.children.filter(SyntaxTree.isNode).flatMap((child) => pending(child, decisions))
}

const inactiveRanges = (
  node: SyntaxTree.Node,
  decisions: ReadonlyMap<number, boolean>,
): ReadonlyArray<SourceSpan.SourceSpan> => {
  if (node.kind === 'StaticConditionalDeclaration') {
    const decision = decisions.get(node.span.start)
    const children = node.children.filter(SyntaxTree.isNode)
    if (decision === undefined) return children.slice(1).map((arm) => arm.span)
    const active = children[decision ? 1 : 2]
    const inactive = children[decision ? 2 : 1]
    return [
      ...(inactive === undefined ? [] : [inactive.span]),
      ...(active === undefined ? [] : inactiveRanges(active, decisions)),
    ]
  }
  if (node.kind !== 'SourceFile' && node.kind !== 'DeclarationGroup') return []
  return node.children
    .filter(SyntaxTree.isNode)
    .flatMap((child) => inactiveRanges(child, decisions))
}

/** True when loaded syntax contains a module-level condition. */
export const required = (closure: ModuleClosure.Facts): boolean =>
  closure.modules.some((module) => pending(module.syntax.root, new Map()).length > 0)

/** Finds controlling groups for a failed bootstrap dependency, including selective-import aliases. */
export const availabilityOrigins = (
  closure: ModuleClosure.Facts,
  span: SourceSpan.SourceSpan,
): ReadonlyArray<SourceSpan.SourceSpan> => {
  const source = closure.sources.get(span.sourceId)
  if (source === undefined) return []
  const name = Option.getOrElse(SourceFile.spelling(source, span), () => '')
  const names = new Set(availabilityNames(closure, span.sourceId, name))
  return closure.modules.flatMap((module) => {
    const visit = (
      node: SyntaxTree.Node,
      parents: ReadonlyArray<SourceSpan.SourceSpan>,
    ): ReadonlyArray<SourceSpan.SourceSpan> => {
      if (node.kind === 'StaticConditionalDeclaration')
        return node.children
          .filter(SyntaxTree.isNode)
          .slice(1)
          .flatMap((arm) => visit(arm, [...parents, node.span]))
      if (node.kind === 'SourceFile' || node.kind === 'DeclarationGroup')
        return node.children.filter(SyntaxTree.isNode).flatMap((child) => visit(child, parents))
      const token = SyntaxTree.directToken(node, 'Identifier')
      if (parents.length === 0 || token === undefined) return []
      const spelling = Option.getOrElse(
        SourceFile.spelling(module.syntax.source, token.span),
        () => '',
      )
      return names.has(`${module.name}\u0000${spelling}`) ? [...parents, token.span] : []
    }
    return visit(module.syntax.root, [])
  })
}

/** Builds static evaluation inputs without elaborating unrelated executable bodies. */
const coordinator = (
  closure: ModuleClosure.Facts,
  completion: ProfileBootstrap.Completion,
): Residualization.Coordinator => {
  const { index, resolution } = NameResolution.analyze(closure)
  const results = new Map<string, Elaboration.Result>()
  for (const module of closure.modules) {
    const headers = index.modules.find((candidate) => candidate.module === module.name)
    const scope = NameResolution.scopeOf(resolution, module.name)
    if (headers === undefined || scope === undefined)
      throw new RangeError(`Module selection lost headers for ${module.name}`)
    results.set(
      module.name,
      Elaboration.elaborateModule({
        syntax: module.syntax,
        headers: { ...headers, declarations: [], constants: [] },
        scope,
        index,
      }),
    )
  }
  return Residualization.make(
    completion.profile,
    results,
    resolution,
    index,
    undefined,
    completion.values,
  )
}

const conditionKey = (node: SyntaxTree.Node): string => `${node.span.sourceId}:${node.span.start}`

/** Follows explicit import aliases without loading unavailable modules or admitting declarations. */
const availabilityNames = (
  closure: ModuleClosure.Facts,
  moduleName: string,
  spelling: string,
  visited: ReadonlySet<string> = new Set(),
): ReadonlyArray<string> => {
  const key = `${moduleName}\u0000${spelling}`
  if (visited.has(key)) return []
  const next = new Set([...visited, key])
  const module = closure.modules.find((candidate) => candidate.name === moduleName)
  const names = [key]
  for (const imported of module?.imports ?? []) {
    if (imported.canonicalTarget === undefined || module === undefined) continue
    const list = SyntaxTree.directNode(imported.syntax, 'ImportMemberList')
    for (const member of list === undefined ? [] : SyntaxTree.directNodes(list, 'ImportMember')) {
      const original = SyntaxTree.directToken(member, 'Identifier')
      const alias = SyntaxTree.directNode(member, 'ImportAlias')
      const local = alias === undefined ? original : SyntaxTree.directToken(alias, 'Identifier')
      if (original === undefined || local === undefined) continue
      if (
        Option.getOrElse(SourceFile.spelling(module.syntax.source, local.span), () => '') !==
        spelling
      )
        continue
      const originalName = Option.getOrElse(
        SourceFile.spelling(module.syntax.source, original.span),
        () => '',
      )
      names.push(...availabilityNames(closure, imported.canonicalTarget, originalName, next))
    }
  }
  return names
}

/** Maps unavailable names to the pending conditions that control their declaration's availability. */
const availabilityCycles = (
  closure: ModuleClosure.Facts,
  failures: ReadonlyArray<{
    readonly condition: SyntaxTree.Node
    readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  }>,
  target: string,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  const owners = new Map<string, Array<string>>()
  for (const { condition } of failures) {
    const source = closure.sources.get(condition.span.sourceId)
    if (source === undefined) continue
    const addName = (name: Token.Token): void => {
      const spelling = Option.getOrElse(SourceFile.spelling(source, name.span), () => '')
      const key = `${source.id}\u0000${spelling}`
      owners.set(key, [...(owners.get(key) ?? []), conditionKey(condition)])
    }
    const visit = (node: SyntaxTree.Node): void => {
      if (node.kind === 'DeclarationGroup' || node.kind === 'StaticConditionalDeclaration') {
        for (const child of node.children.filter(SyntaxTree.isNode)) visit(child)
        return
      }
      if (!node.kind.endsWith('Declaration')) return
      if (node.kind === 'ImportDeclaration') {
        const list = SyntaxTree.directNode(node, 'ImportMemberList')
        const alias = SyntaxTree.directNode(node, 'ImportAlias')
        const path = SyntaxTree.directNode(node, 'ImportPath')
        let namespace: Token.Token | undefined
        if (alias !== undefined) namespace = SyntaxTree.directToken(alias, 'Identifier')
        else if (list === undefined && path !== undefined)
          namespace = ImportPath.segments(path).at(-1)
        if (namespace !== undefined) addName(namespace)
        for (const member of list === undefined
          ? []
          : SyntaxTree.directNodes(list, 'ImportMember')) {
          const alias = SyntaxTree.directNode(member, 'ImportAlias')
          const name = SyntaxTree.directToken(alias ?? member, 'Identifier')
          if (name !== undefined) addName(name)
        }
        return
      }
      const name = SyntaxTree.directToken(node, 'Identifier')
      if (name === undefined) return
      addName(name)
    }
    for (const arm of condition.children.filter(SyntaxTree.isNode).slice(1)) visit(arm)
  }
  const edges = new Map<string, ReadonlyArray<string>>()
  for (const failure of failures) {
    const targets = failure.diagnostics.flatMap((diagnostic) => {
      const reason = diagnostic.reason
      if (!('spelling' in reason)) return []
      const module =
        reason._tag === 'UnknownImportedMember' ? reason.module : diagnostic.span.sourceId
      return availabilityNames(closure, module, reason.spelling).flatMap(
        (name) => owners.get(name) ?? [],
      )
    })
    edges.set(conditionKey(failure.condition), targets)
  }
  return Graph.stronglyConnected([...edges.keys()], (key) => edges.get(key) ?? []).flatMap(
    (component) => {
      if (
        component.length === 1 &&
        !(edges.get(component[0] ?? '') ?? []).includes(component[0] ?? '')
      )
        return []
      const conditions = failures.filter((failure) =>
        component.includes(conditionKey(failure.condition)),
      )
      return conditions.map(({ condition }) =>
        Diagnostic.staticEvaluationCycle(
          'module declaration availability',
          target,
          conditions.map((dependency) => ({
            kind: 'Call',
            label: 'condition dependency',
            arguments: [],
            span: dependency.condition.span,
          })),
          condition.span,
        ),
      )
    },
  )
}

/** Resolves one selection transaction against a stable, memoized source supply. */
export const select = Effect.fn('ModuleSelection.select')(function* (
  request: ModuleClosure.ProjectRequest,
  initial: ModuleClosure.ProjectClosure,
  completion: ProfileBootstrap.Completion,
): Effect.fn.Return<
  {
    readonly closure: ModuleClosure.ProjectClosure
    readonly selection: ModuleSelection
  },
  never,
  SourceResolver.SourceResolver
> {
  const resolver = yield* SourceResolver.SourceResolver
  const resolved = new Map<
    string,
    Result.Result<Option.Option<SourceResolver.ResolvedSource>, SourceResolver.SourceResolverError>
  >()
  for (const module of initial.modules) {
    resolved.set(
      module.name,
      Result.succeed(
        Option.some(
          SourceResolver.resolved(
            SourceFile.toUint8Array(module.syntax.source),
            module.syntax.source.origin,
          ),
        ),
      ),
    )
    for (const imported of module.imports) {
      if (imported.target._tag === 'Unknown')
        resolved.set(imported.target.module, Result.succeed(Option.none()))
      if (imported.target._tag === 'Failed')
        resolved.set(imported.target.module, Result.fail(imported.target.error))
    }
  }
  const memoized = (resolve: typeof resolver.resolve) =>
    Effect.fnUntraced(function* (module: string) {
      let result = resolved.get(module)
      if (result === undefined) {
        result = yield* Effect.result(resolve(module))
        resolved.set(module, result)
      }
      if (Result.isFailure(result)) return yield* result.failure
      return result.success
    })
  const supply = SourceResolver.SourceResolver.of({
    ...resolver,
    resolve: memoized(resolver.resolve),
    resolveStandardLibrary: memoized(resolver.resolveStandardLibrary),
  })
  const decisions = new Map<string, Map<number, boolean>>()
  const bootstrapModules = new Set(initial.modules.map((module) => module.name))
  const dependencies: Array<string> = []
  const expressions = new Map<string, Map<number, Elaboration.ExpressionFact>>()
  let closure = initial
  let diagnostics: ReadonlyArray<Diagnostic.Diagnostic> = []
  while (true) {
    const conditions = closure.modules.flatMap((module) =>
      pending(module.syntax.root, decisions.get(module.name) ?? new Map()),
    )
    if (conditions.length === 0) break
    const evaluation = coordinator(closure, completion)
    const failures: Array<Diagnostic.Diagnostic> = []
    const failedConditions: Array<{
      readonly condition: SyntaxTree.Node
      readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
    }> = []
    let progressed = false
    for (const condition of conditions) {
      const result = yield* Residualization.evaluateModuleCondition(evaluation, condition)
      if (result.expression !== undefined) {
        let module = expressions.get(condition.span.sourceId)
        if (module === undefined) {
          module = new Map()
          expressions.set(condition.span.sourceId, module)
        }
        module.set(condition.span.start, result.expression)
      }
      if (result.outcome._tag === 'Complete' && result.outcome.value._tag === 'BooleanValue') {
        let module = decisions.get(condition.span.sourceId)
        if (module === undefined) {
          module = new Map()
          decisions.set(condition.span.sourceId, module)
        }
        module.set(condition.span.start, result.outcome.value.value)
        progressed = true
      } else {
        failedConditions.push({ condition, diagnostics: result.diagnostics })
        failures.push(...result.diagnostics)
        if (result.outcome._tag === 'Failed')
          failures.push(
            StaticEvaluation.diagnostic(result.outcome.failure, completion.profile.target.id),
          )
      }
    }
    dependencies.push(Residualization.dependencies(evaluation))
    if (!progressed) {
      diagnostics = Diagnostic.merge(
        failures,
        availabilityCycles(closure, failedConditions, completion.profile.target.id),
      )
      break
    }
    closure = yield* ModuleClosure.loadProject({
      ...request,
      previous: closure,
      selection: decisions,
    }).pipe(Effect.provideService(SourceResolver.SourceResolver, supply))
    const conditionalSchemas = closure.modules.flatMap((module) =>
      bootstrapModules.has(module.name)
        ? []
        : module.declarations.filter(
            (declaration) => declaration.kind === 'PackageParameterDeclaration',
          ),
    )
    if (conditionalSchemas.length > 0) {
      diagnostics = conditionalSchemas.map((schema) =>
        Diagnostic.invalidConfiguration(
          ConfigurationError.make(
            'ModuleSelection.select',
            'DependencyCycle',
            'conditionally available package schema',
            [{ source: schema.span.sourceId, provenance: 'literal', span: schema.span }],
          ),
          schema.span,
        ),
      )
      break
    }
  }
  return Object.freeze({
    closure: Object.freeze({
      ...closure,
      diagnostics: Diagnostic.merge(closure.diagnostics, diagnostics),
    }),
    selection: Object.freeze({
      conditions: new Map(
        [...expressions].map(([module, values]) => [
          module,
          Object.freeze(
            [...values.entries()].sort(([a], [b]) => a - b).map(([, expression]) => expression),
          ),
        ]),
      ),
      profile: completion.profile,
      decisions: new Map([...decisions].map(([name, values]) => [name, new Map(values)])),
      inactiveRanges: new Map(
        closure.modules.map((module) => [
          module.name,
          Object.freeze(
            inactiveRanges(module.syntax.root, decisions.get(module.name) ?? new Map()),
          ),
        ]),
      ),
      dependencies: Canonical.array(dependencies),
    }),
  })
})
