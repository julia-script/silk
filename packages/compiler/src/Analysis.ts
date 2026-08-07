import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as Backend from './Backend.js'
import * as BackendRegistry from './BackendRegistry.js'
import * as BootstrapEvaluation from './BootstrapEvaluation.js'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as Layout from './Layout.js'
import * as Lower from './Lower.js'
import * as Mir from './Mir.js'
import * as ModuleClosure from './ModuleClosure.js'
import * as NameResolution from './NameResolution.js'
import * as Ownership from './Ownership.js'
import * as SemanticTarget from './SemanticTarget.js'
import * as SourceFile from './SourceFile.js'
import * as SourceResolver from './SourceResolver.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as Target from './Target.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'
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
  | {
      readonly _tag: 'Unavailable'
      readonly error: Target.TargetError | AnalysisUnavailable
    }

/** A valid target phase intentionally withheld because source specialization is invalid. */
export class AnalysisUnavailable extends Data.TaggedError('AnalysisUnavailable')<{
  readonly operation: 'Analysis.make'
  readonly message: string
}> {}

/** One immutable analysis snapshot of one compilation request. */
export interface Snapshot {
  readonly _tag: 'AnalysisSnapshot'
  readonly closure: ModuleClosure.Closure
  readonly index: DeclarationIndex.Index
  readonly resolution: NameResolution.Resolution
  readonly results: ReadonlyMap<string, Elaboration.Result>
  readonly semanticTargets: SemanticTarget.Index
  readonly ownership: ReadonlyMap<string, Ownership.ModuleOwnership>
  readonly instances: Instances.Discovery
  readonly target: Target.Selection
  readonly layoutCatalog: Targeted<Layout.Catalog>
  readonly layout: Targeted<Layout.Plan>
  readonly mir: Targeted<Mir.Module>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

/** Source facts that make backend emission unavailable while keeping analysis queryable. */
export class CodegenUnavailable extends Data.TaggedError('CodegenUnavailable')<{
  readonly operation: 'Analysis.codegen'
  readonly message: string
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly resolutionFailures: ReadonlyArray<SourceResolver.SourceResolverError>
}> {}

const hasInvalidGenericBody = (
  index: DeclarationIndex.Index,
  diagnostics: ReadonlyArray<Diagnostic.Diagnostic>,
): boolean =>
  index.modules.some((module) =>
    module.members.some(
      (member) =>
        member.typeParameters.length > 0 &&
        diagnostics.some(
          (diagnostic) =>
            diagnostic.span.sourceId === member.syntax.span.sourceId &&
            diagnostic.span.start >= member.syntax.span.start &&
            diagnostic.span.end <= member.syntax.span.end,
        ),
    ),
  )

/** Builds the snapshot of one compilation request. */
export const make = Effect.fn('Analysis.make')(function* (
  request: ModuleClosure.CompilationRequest,
): Effect.fn.Return<Snapshot, never, SourceResolver.SourceResolver> {
  const closure = yield* ModuleClosure.load(request)
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
  const semanticTargets = SemanticTarget.make(results, index, resolution)
  const ownership = new Map(
    [...results.entries()].map(([name, result]) => [name, Ownership.checkModule(result)]),
  )
  const frontendDiagnostics = Diagnostic.merge(
    ...closure.modules.map((module) => module.syntax.lexicalDiagnostics),
    ...closure.modules.map((module) => module.syntax.parserDiagnostics),
    closure.diagnostics,
    resolution.diagnostics,
    ...[...results.values()].map((result) => result.diagnostics),
    ...[...ownership.values()].map((facts) => facts.diagnostics),
  )
  const frontendSpecializationInvalid =
    Diagnostic.hasGenericSpecializationErrors(frontendDiagnostics) ||
    hasInvalidGenericBody(index, frontendDiagnostics)
  const instances = frontendSpecializationInvalid
    ? Instances.invalid(request.root.id)
    : Instances.discover(request.root.id, results)
  const baseDiagnostics = Diagnostic.merge(
    frontendDiagnostics,
    Instances.violationDiagnostics(instances),
  )
  const target = Target.select(request.target)
  const specializationError =
    frontendSpecializationInvalid || Diagnostic.hasGenericSpecializationErrors(baseDiagnostics)
      ? new AnalysisUnavailable({
          operation: 'Analysis.make',
          message: 'Target-dependent phases are unavailable for invalid source specialization',
        })
      : undefined
  const layoutCatalog: Targeted<Layout.Catalog> =
    specializationError !== undefined
      ? Object.freeze({ _tag: 'Unavailable', error: specializationError })
      : target._tag === 'Resolved'
        ? Object.freeze({
            _tag: 'Available',
            value: Layout.catalog(target.target, index, instances),
          })
        : Object.freeze({ _tag: 'Unavailable', error: target.error })
  const layout: Targeted<Layout.Plan> =
    layoutCatalog._tag === 'Available'
      ? Object.freeze({ _tag: 'Available', value: Layout.plan(layoutCatalog.value, instances) })
      : layoutCatalog
  const diagnostics = Diagnostic.merge(
    baseDiagnostics,
    ...(layout._tag === 'Available' ? [layout.value.diagnostics] : []),
  )
  const targetLiteralError =
    layout._tag === 'Available' && Diagnostic.hasErrors(layout.value.diagnostics)
      ? new AnalysisUnavailable({
          operation: 'Analysis.make',
          message: 'MIR is unavailable because a Usize literal exceeds the selected target',
        })
      : undefined
  const mir: Targeted<Mir.Module> =
    targetLiteralError !== undefined
      ? Object.freeze({ _tag: 'Unavailable', error: targetLiteralError })
      : layout._tag === 'Available'
        ? Object.freeze({
            _tag: 'Available',
            value: Lower.lowerProgram(instances, ownership, layout.value),
          })
        : layout
  return Object.freeze({
    _tag: 'AnalysisSnapshot',
    closure,
    index,
    resolution,
    results,
    semanticTargets,
    ownership,
    instances,
    target,
    layoutCatalog,
    layout,
    mir,
    diagnostics,
  })
})

/** Builds the snapshot of one single-module source. */
export const ofSource = (
  sourceId: string,
  bytes: Uint8Array,
  target?: string,
): Effect.Effect<Snapshot> =>
  Effect.provide(
    target === undefined
      ? make({ root: SourceFile.make(sourceId, bytes) })
      : make({ root: SourceFile.make(sourceId, bytes), target }),
    SourceResolver.empty,
  )

/** Returns every loaded module of the snapshot in canonical identity order. */
export const modules = (self: Snapshot): ReadonlyArray<ModuleClosure.Module> => self.closure.modules

/** Returns the snapshot's import cycle facts in canonical order. */
export const cycles = (self: Snapshot): ReadonlyArray<ReadonlyArray<string>> => self.closure.cycles

/** Returns exact immutable source snapshots for every successfully loaded module. */
export const sources = (self: Snapshot): ReadonlyMap<string, SourceFile.SourceFile> =>
  self.closure.sources

/** Returns the smallest reference-bearing semantic target containing one byte offset. */
export const semanticTargetAt = (
  self: Snapshot,
  module: string,
  byteOffset: number,
): SemanticTarget.SemanticTarget | undefined =>
  SemanticTarget.at(self.semanticTargets, module, byteOffset)

/** Resolves one semantic identity to its exact declaration and name spans. */
export const declarationLocation = (
  self: Snapshot,
  identity: SemanticTarget.Identity,
): SemanticTarget.DeclarationLocation | undefined =>
  SemanticTarget.declarationLocation(self.semanticTargets, identity)

/** Returns operational source-resolution failures in canonical module order. */
export const resolutionFailures = (
  self: Snapshot,
): ReadonlyArray<SourceResolver.SourceResolverError> => self.closure.resolutionFailures

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

const nestedExpressionFacts = (
  expression: Elaboration.ExpressionFact,
): ReadonlyArray<Elaboration.ExpressionFact> => {
  const nested: ReadonlyArray<Elaboration.ExpressionFact> = (() => {
    switch (expression._tag) {
      case 'Move':
      case 'FieldProjection':
        return [expression.subject]
      case 'IndexProjection':
        return [expression.subject, expression.index]
      case 'ArrayLiteral':
        return expression.elements.map((element) => element.expression)
      case 'StructLiteral':
        return expression.initializers.map((initializer) => initializer.expression)
      case 'Grouped':
        return [expression.expression]
      case 'Run':
        return [expression.subject]
      case 'EffectCatch':
        return [expression.protected]
      case 'EffectRetry':
        return [expression.protected, expression.retries]
      case 'EffectProvide':
        return [expression.protected]
      case 'EffectProvideWith':
        return [expression.protected, expression.acquisition]
      case 'Operator':
      case 'Call':
        return expression.arguments.map((argument) => argument.expression)
      case 'Pipeline':
        return [expression.input, ...expression.arguments.map((argument) => argument.expression)]
      case 'Match':
        return [
          expression.scrutinee,
          ...expression.arms.flatMap((arm) => [
            ...(arm.guard === undefined ? [] : [arm.guard]),
            arm.result,
          ]),
        ]
      default:
        return []
    }
  })()
  return Object.freeze([
    expression,
    ...nested.flatMap((candidate) => nestedExpressionFacts(candidate)),
  ])
}

const statementExpressionFacts = (
  statement: Elaboration.StatementFact,
): ReadonlyArray<Elaboration.ExpressionFact> => {
  switch (statement._tag) {
    case 'BindStatement':
      return nestedExpressionFacts(statement.binding.initializer)
    case 'ReturnStatement':
      return nestedExpressionFacts(statement.expression)
    case 'FailStatement':
    case 'DropStatement':
      return nestedExpressionFacts(statement.expression)
    case 'IfStatement':
      return Object.freeze([
        ...nestedExpressionFacts(statement.condition),
        ...statement.taken.flatMap(statementExpressionFacts),
        ...statement.otherwise.flatMap(statementExpressionFacts),
      ])
    case 'WriteStatement':
      return Object.freeze([
        ...nestedExpressionFacts(statement.destination),
        ...nestedExpressionFacts(statement.value),
      ])
    case 'WhileStatement':
      return Object.freeze([
        ...nestedExpressionFacts(statement.condition),
        ...statement.body.flatMap(statementExpressionFacts),
      ])
    case 'BreakStatement':
    case 'ContinueStatement':
      return Object.freeze([])
  }
}

const nestedStatementFacts = (
  statement: Elaboration.StatementFact,
): ReadonlyArray<Elaboration.StatementFact> => {
  switch (statement._tag) {
    case 'IfStatement':
      return Object.freeze([
        statement,
        ...statement.taken.flatMap(nestedStatementFacts),
        ...statement.otherwise.flatMap(nestedStatementFacts),
      ])
    case 'WhileStatement':
      return Object.freeze([statement, ...statement.body.flatMap(nestedStatementFacts)])
    default:
      return Object.freeze([statement])
  }
}

/** Returns every semantic statement fact in deterministic source nesting order. */
export const statementsOf = (
  self: Snapshot,
  module: string,
): ReadonlyArray<Elaboration.StatementFact> =>
  Object.freeze(
    self.results
      .get(module)
      ?.functions.flatMap((fn) => fn.statements.flatMap(nestedStatementFacts)) ?? [],
  )

/** Returns every binding with its canonical identity and immutable/mutable classification. */
export const bindingsOf = (
  self: Snapshot,
  module: string,
): ReadonlyArray<Elaboration.BindingDeclarationFact> =>
  Object.freeze(self.results.get(module)?.functions.flatMap((fn) => fn.bindings) ?? [])

/** Returns every complete or unavailable assignment fact in source order. */
export const writesOf = (
  self: Snapshot,
  module: string,
): ReadonlyArray<Extract<Elaboration.StatementFact, { readonly _tag: 'WriteStatement' }>> =>
  Object.freeze(
    statementsOf(self, module).filter(
      (
        statement,
      ): statement is Extract<Elaboration.StatementFact, { readonly _tag: 'WriteStatement' }> =>
        statement._tag === 'WriteStatement',
    ),
  )

/** Returns canonical loop identities, lexical parents, conditions, and ordered bodies. */
export const loopsOf = (
  self: Snapshot,
  module: string,
): ReadonlyArray<Extract<Elaboration.StatementFact, { readonly _tag: 'WhileStatement' }>> =>
  Object.freeze(
    statementsOf(self, module).filter(
      (
        statement,
      ): statement is Extract<Elaboration.StatementFact, { readonly _tag: 'WhileStatement' }> =>
        statement._tag === 'WhileStatement',
    ),
  )

/** Returns every lexical loop transfer, including explicitly unresolved invalid transfers. */
export const transfersOf = (
  self: Snapshot,
  module: string,
): ReadonlyArray<
  Extract<Elaboration.StatementFact, { readonly _tag: 'BreakStatement' | 'ContinueStatement' }>
> =>
  Object.freeze(
    statementsOf(self, module).filter(
      (
        statement,
      ): statement is Extract<
        Elaboration.StatementFact,
        { readonly _tag: 'BreakStatement' | 'ContinueStatement' }
      > => statement._tag === 'BreakStatement' || statement._tag === 'ContinueStatement',
    ),
  )

/** Returns every semantic expression fact in deterministic source nesting order. */
export const expressionsOf = (
  self: Snapshot,
  module: string,
): ReadonlyArray<Elaboration.ExpressionFact> =>
  Object.freeze(
    self.results
      .get(module)
      ?.functions.flatMap((fn) => fn.statements.flatMap(statementExpressionFacts)) ?? [],
  )

/** Returns every retained semantic match with source patterns and canonical coverage facts. */
export const matchesOf = (
  self: Snapshot,
  module: string,
): ReadonlyArray<Extract<Elaboration.ExpressionFact, { readonly _tag: 'Match' }>> =>
  Object.freeze(
    expressionsOf(self, module).filter(
      (expression): expression is Extract<Elaboration.ExpressionFact, { readonly _tag: 'Match' }> =>
        expression._tag === 'Match',
    ),
  )

/** Returns every retained struct literal fact without reconstructing field mappings. */
export const structLiteralsOf = (
  self: Snapshot,
  module: string,
): ReadonlyArray<Elaboration.StructLiteralExpressionFact> =>
  Object.freeze(
    expressionsOf(self, module).filter(
      (expression): expression is Elaboration.StructLiteralExpressionFact =>
        expression._tag === 'StructLiteral',
    ),
  )

/** Returns every canonical or explicitly unavailable field-projection step. */
export const fieldProjectionsOf = (
  self: Snapshot,
  module: string,
): ReadonlyArray<Elaboration.FieldProjectionExpressionFact> =>
  Object.freeze(
    expressionsOf(self, module).filter(
      (expression): expression is Elaboration.FieldProjectionExpressionFact =>
        expression._tag === 'FieldProjection',
    ),
  )

/** Returns every retained array literal with its ordered elements and completeness state. */
export const arrayLiteralsOf = (
  self: Snapshot,
  module: string,
): ReadonlyArray<Elaboration.ArrayLiteralExpressionFact> =>
  Object.freeze(
    expressionsOf(self, module).filter(
      (expression): expression is Elaboration.ArrayLiteralExpressionFact =>
        expression._tag === 'ArrayLiteral',
    ),
  )

/** Returns every retained indexed-place step with its canonical bounds mode. */
export const indexProjectionsOf = (
  self: Snapshot,
  module: string,
): ReadonlyArray<Elaboration.IndexProjectionExpressionFact> =>
  Object.freeze(
    expressionsOf(self, module).filter(
      (expression): expression is Elaboration.IndexProjectionExpressionFact =>
        expression._tag === 'IndexProjection',
    ),
  )

/** Returns canonical fixed-array types used by one module's contracts and expressions. */
export const fixedArrayTypesOf = (
  self: Snapshot,
  module: string,
): ReadonlyArray<Type.FixedArray> => {
  const found = new Map<string, Type.FixedArray>()
  const add = (type: DeclarationIndex.SemanticType): void => {
    if (!Type.isFixedArray(type)) return
    found.set(Type.key(type), type)
    add(type.element)
  }
  const headers = self.index.modules.find((candidate) => candidate.module === module)
  for (const member of headers?.members ?? []) {
    if (member._tag === 'FunctionDeclaration') {
      for (const parameter of member.parameters) {
        if (parameter.declaredType._tag === 'Resolved') add(parameter.declaredType.type)
      }
      if (member.returnType._tag === 'Resolved') add(member.returnType.type)
    } else {
      for (const field of member.fields) {
        if (field.declaredType._tag === 'Resolved') add(field.declaredType.type)
      }
    }
  }
  for (const expression of expressionsOf(self, module)) {
    if (expression.type._tag === 'Available') add(expression.type.type)
  }
  return Object.freeze([...found.values()])
}

/** Returns one module's HIR, or `undefined` for an unknown identity. */
export const hirOf = (self: Snapshot, module: string): Hir.Module | undefined =>
  self.results.get(module)?.hir

/** Returns one module's ownership facts and cleanup plans, or `undefined` for an unknown identity. */
export const ownershipOf = (
  self: Snapshot,
  module: string,
): Ownership.ModuleOwnership | undefined => self.ownership.get(module)

/** Returns deterministic loop-header ownership fixed points for one module. */
export const ownershipFixedPointsOf = (
  self: Snapshot,
  module: string,
): ReadonlyArray<Ownership.LoopFixedPoint> =>
  Object.freeze(self.ownership.get(module)?.functions.flatMap((fn) => fn.fixedPoints) ?? [])

/** Returns every lexical cleanup exit, including loop fallthrough and transfers. */
export const cleanupExitsOf = (self: Snapshot, module: string): ReadonlyArray<Ownership.ExitPlan> =>
  Object.freeze(self.ownership.get(module)?.functions.flatMap((fn) => fn.exits) ?? [])

/** Returns the snapshot's instance discovery: entry state and ordered instances. */
export const instancesOf = (self: Snapshot): Instances.Discovery => self.instances

/** Returns declarations that own canonical type parameters in module/source order. */
export const genericDeclarationsOf = (self: Snapshot): ReadonlyArray<DeclarationIndex.MemberFact> =>
  Object.freeze(
    self.index.modules.flatMap((module) =>
      module.members.filter((member) => member.typeParameters.length > 0),
    ),
  )

/** Returns every typed generic call in deterministic HIR preorder. */
export const genericCallsOf = (
  self: Snapshot,
): ReadonlyArray<Extract<Hir.Expression, { readonly _tag: 'Call' }>> =>
  Object.freeze(
    [...self.results.values()].flatMap((result) =>
      result.hir.functions.flatMap((fn) =>
        fn.statements
          .flatMap(Hir.statementExpressions)
          .flatMap(Hir.expressionTree)
          .flatMap((expression) =>
            expression._tag === 'Call' && expression.typeArguments.length > 0 ? [expression] : [],
          ),
      ),
    ),
  )

export interface CallInstanceLink {
  readonly call: Extract<Hir.Expression, { readonly _tag: 'Call' }>
  readonly caller: Instances.Instance
  readonly target: Instances.Instance
}

/** Resolves one source HIR call in every reached caller specialization. */
export const instancesOfCall = (
  self: Snapshot,
  call: Extract<Hir.Expression, { readonly _tag: 'Call' }>,
): ReadonlyArray<CallInstanceLink> =>
  Object.freeze(
    self.instances.instances.flatMap((caller): ReadonlyArray<CallInstanceLink> => {
      const ownsCall = caller.function.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .some((expression) => expression === call)
      if (!ownsCall) return []
      const arguments_ = call.typeArguments.map((argument) =>
        Type.substitute(argument, caller.substitution),
      )
      const target = self.instances.instances.find(
        (candidate) =>
          candidate.key.declaration.module === call.target.module &&
          candidate.key.declaration.name === call.target.name &&
          candidate.key.typeArguments.length === arguments_.length &&
          candidate.key.typeArguments.every((argument, index) => {
            const callArgument = arguments_.at(index)
            return callArgument !== undefined && Type.equals(argument, callArgument)
          }),
      )
      return target === undefined ? [] : [Object.freeze({ call, caller, target })]
    }),
  )

/** Returns concrete reached applications while excluding open generic declarations. */
export const appliedLayoutsOf = (self: Snapshot): ReadonlyArray<Layout.Entry> =>
  self.layout._tag === 'Available'
    ? Object.freeze(
        self.layout.value.entries.filter(
          (entry) => Type.isNominal(entry.type) && entry.type.arguments.length > 0,
        ),
      )
    : Object.freeze([])

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

/** Returns every reachable repeated layout without asking tooling to reconstruct arrays. */
export const repeatedLayoutsOf = (self: Snapshot): ReadonlyArray<Layout.Entry> =>
  self.layout._tag === 'Available'
    ? Object.freeze(
        self.layout.value.entries.filter((entry) => entry.representation._tag === 'Repeated'),
      )
    : Object.freeze([])

/** Returns every reachable compiler-owned structural-union layout. */
export const unionLayoutsOf = (self: Snapshot): ReadonlyArray<Layout.Entry> =>
  self.layout._tag === 'Available'
    ? Object.freeze(
        self.layout.value.entries.filter((entry) => entry.representation._tag === 'Union'),
      )
    : Object.freeze([])

/** Returns every reachable array calling shape and its canonical physical paths. */
export const arrayCallingShapesOf = (self: Snapshot): ReadonlyArray<Layout.CallingShape> =>
  self.layout._tag === 'Available'
    ? Object.freeze(
        self.layout.value.callingShapes.filter((shape) => Type.isFixedArray(shape.type)),
      )
    : Object.freeze([])

/** Returns every reachable structural-union sum calling shape. */
export const unionCallingShapesOf = (self: Snapshot): ReadonlyArray<Layout.CallingShape> =>
  self.layout._tag === 'Available'
    ? Object.freeze(self.layout.value.callingShapes.filter((shape) => Type.isUnion(shape.type)))
    : Object.freeze([])

/** Returns every explicit HIR union conversion in source semantic order. */
export const hirUnionConversionsOf = (
  self: Snapshot,
  module: string,
): ReadonlyArray<Extract<Hir.Expression, { readonly _tag: 'UnionConvert' }>> =>
  Object.freeze(
    (self.results.get(module)?.hir.functions ?? []).flatMap((fn) =>
      fn.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .flatMap((expression) => (expression._tag === 'UnionConvert' ? [expression] : [])),
    ),
  )

/** Returns every typed structured HIR match in deterministic expression preorder. */
export const hirMatchesOf = (
  self: Snapshot,
  module: string,
): ReadonlyArray<Extract<Hir.Expression, { readonly _tag: 'Match' }>> =>
  Object.freeze(
    (self.results.get(module)?.hir.functions ?? []).flatMap((fn) =>
      fn.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .flatMap((expression) => (expression._tag === 'Match' ? [expression] : [])),
    ),
  )

/** Returns every match-local ownership and cleanup plan in function/source order. */
export const ownershipMatchesOf = (
  self: Snapshot,
  module: string,
): ReadonlyArray<Ownership.MatchOwnership> =>
  Object.freeze(self.ownership.get(module)?.functions.flatMap((fn) => fn.matches) ?? [])

/** Returns every verified MIR union conversion in canonical topological order. */
export const mirUnionConversionsOf = (
  self: Snapshot,
): ReadonlyArray<Extract<Mir.Operation, { readonly _tag: 'ConvertUnion' }>> =>
  self.mir._tag === 'Unavailable'
    ? Object.freeze([])
    : Object.freeze(
        self.mir.value.functions.flatMap((fn) =>
          Mir.operations(fn).flatMap((operation) =>
            operation._tag === 'ConvertUnion' ? [operation] : [],
          ),
        ),
      )

/** Returns every structured MIR match, including nested expression matches, in DAG preorder. */
export const mirMatchesOf = (
  self: Snapshot,
): ReadonlyArray<Extract<Mir.Operation, { readonly _tag: 'Match' }>> =>
  self.mir._tag === 'Unavailable'
    ? Object.freeze([])
    : Object.freeze(
        self.mir.value.functions.flatMap((fn) =>
          Mir.operations(fn).flatMap((operation) =>
            operation._tag === 'Match' ? [operation] : [],
          ),
        ),
      )

/** Looks up one compiler-owned aggregate calling shape from the completed runtime plan. */
export const callingShapeOf = (self: Snapshot, type: Type.Type): Layout.CallingShape | undefined =>
  self.layout._tag === 'Available' ? Layout.callingShape(self.layout.value, type) : undefined

/** Returns the snapshot's available or explicitly unavailable lowered MIR state. */
export const mirOf = (self: Snapshot): Targeted<Mir.Module> => self.mir

export interface ControlRegionFact {
  readonly function: DeclarationIndex.CanonicalId
  readonly region: Mir.Region
}

/** Returns every MIR region in canonical per-function topological order. */
export const controlRegionsOf = (self: Snapshot): ReadonlyArray<ControlRegionFact> =>
  self.mir._tag === 'Unavailable'
    ? Object.freeze([])
    : Object.freeze(
        self.mir.value.functions.flatMap((fn) =>
          Mir.topologicalRegions(fn).map((region) => Object.freeze({ function: fn.id, region })),
        ),
      )

export interface ControlEdgeFact {
  readonly function: DeclarationIndex.CanonicalId
  readonly edge: Mir.ControlEdge
}

/** Returns compiler-owned structural DAG edges; lexical repeat/exit remain outcomes, not edges. */
export const controlEdgesOf = (self: Snapshot): ReadonlyArray<ControlEdgeFact> =>
  self.mir._tag === 'Unavailable'
    ? Object.freeze([])
    : Object.freeze(
        self.mir.value.functions.flatMap((fn) =>
          Mir.controlEdges(fn).map((edge) => Object.freeze({ function: fn.id, edge })),
        ),
      )

/** Returns one execution's immutable, source-linked trace for tooling projections. */
export const traceOf = (
  outcome: BootstrapEvaluation.Outcome,
): ReadonlyArray<BootstrapEvaluation.TraceEvent> => outcome.trace

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
): Effect.fn.Return<
  Backend.Artifact,
  Backend.BackendError | Target.TargetError | AnalysisUnavailable | CodegenUnavailable
> {
  if (Diagnostic.hasErrors(self.diagnostics) || self.closure.resolutionFailures.length > 0) {
    return yield* new CodegenUnavailable({
      operation: 'Analysis.codegen',
      message: 'Backend emission is unavailable for an invalid analysis snapshot',
      diagnostics: self.diagnostics,
      resolutionFailures: self.closure.resolutionFailures,
    })
  }
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
): Effect.fn.Return<
  Backend.Artifact,
  Backend.BackendError | Target.TargetError | AnalysisUnavailable | CodegenUnavailable
> {
  return yield* codegen(self, request, WasmBackend.WasmBackend)
})

/** Returns backend-local control constructs with canonical region and source provenance. */
export const backendControlOf = (
  artifact: Backend.Artifact,
): ReadonlyArray<Backend.ControlProvenance> => artifact.control

/** Executes the snapshot's lowered MIR program through the closed bootstrap interpreter. */
export const evaluate = (self: Snapshot): BootstrapEvaluation.Outcome =>
  BootstrapEvaluation.evaluate(self.instances, loweredMir(self))

/** Returns the compact array-specific events from an explicit evaluation outcome. */
export const arrayTraceEventsOf = (
  outcome: BootstrapEvaluation.Outcome,
): ReadonlyArray<
  BootstrapEvaluation.ArrayConstructTraceEvent | BootstrapEvaluation.PlaceReadTraceEvent
> =>
  Object.freeze(
    outcome.trace.filter(
      (
        event,
      ): event is
        | BootstrapEvaluation.ArrayConstructTraceEvent
        | BootstrapEvaluation.PlaceReadTraceEvent =>
        event._tag === 'ArrayConstruct' || event._tag === 'PlaceRead',
    ),
  )

/** Returns logical injection and widening events without exposing physical backend tags. */
export const unionTraceEventsOf = (
  outcome: BootstrapEvaluation.Outcome,
): ReadonlyArray<BootstrapEvaluation.UnionConversionTraceEvent> =>
  Object.freeze(
    outcome.trace.filter(
      (event): event is BootstrapEvaluation.UnionConversionTraceEvent =>
        event._tag === 'UnionConversion',
    ),
  )
