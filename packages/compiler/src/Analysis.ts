import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'

export { AnalysisUnavailable } from './AnalysisUnavailable.js'

import type { AnalysisUnavailable } from './AnalysisUnavailable.js'
import * as Backend from './Backend.js'
import * as BootstrapEvaluation from './BootstrapEvaluation.js'
import * as Completion from './Completion.js'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as DocBlock from './DocBlock.js'
import * as Elaboration from './Elaboration.js'
import * as FrontendTooling from './FrontendTooling.js'
import * as Hir from './Hir.js'
import type * as Instances from './Instances.js'
import * as Intrinsic from './Intrinsic.js'
import * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import type * as ModuleClosure from './ModuleClosure.js'
import type * as ModuleSemantics from './ModuleSemantics.js'
import type * as ModuleSurface from './ModuleSurface.js'
import * as NameResolution from './NameResolution.js'
import type * as Ownership from './Ownership.js'
import type * as PhaseReport from './PhaseReport.js'
import * as Pipeline from './Pipeline.js'
import * as Presentation from './Presentation.js'
import type * as SemanticInvalidation from './SemanticInvalidation.js'
import * as SemanticOccurrence from './SemanticOccurrence.js'
import * as SourceFile from './SourceFile.js'
import * as SourceResolver from './SourceResolver.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as SyntaxFile from './SyntaxFile.js'
import type * as SyntaxTree from './SyntaxTree.js'
import type * as Target from './Target.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'
import * as TypeHint from './TypeHint.js'
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

/** One immutable frontend analysis snapshot of one compilation request. */
export interface FrontendSnapshot {
  readonly _tag: 'AnalysisSnapshot' | 'ProjectAnalysisView'
  readonly realization: 'SingleRoot' | 'ProjectView'
  readonly closure: ModuleClosure.Closure
  readonly index: DeclarationIndex.Index
  readonly resolution: NameResolution.Resolution
  readonly surfaces: ReadonlyMap<string, ModuleSurface.ModuleSurface>
  readonly semantics: ReadonlyMap<string, ModuleSemantics.ModuleSemantics>
  readonly results: ReadonlyMap<string, Elaboration.Result>
  readonly semanticOccurrences: SemanticOccurrence.Index
  readonly anonymousExpressions: ReadonlyMap<string, ReadonlyArray<AnonymousExpression>>
  readonly ownership: ReadonlyMap<string, Ownership.ModuleOwnership>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<PhaseReport.PhaseReport>
  readonly semanticInvalidation?: SemanticInvalidation.SemanticInvalidation
  readonly requestedTarget?: string
}

/** One true single-root frontend accepted by explicit runtime realization. */
export interface SingleRootFrontendSnapshot extends FrontendSnapshot {
  readonly _tag: 'AnalysisSnapshot'
  readonly realization: 'SingleRoot'
}

/** One immutable runtime realization derived from a completed frontend snapshot. */
export interface Snapshot extends SingleRootFrontendSnapshot {
  readonly instances: Instances.Discovery
  readonly target: Target.Selection
  readonly layoutCatalog: Targeted<Layout.Catalog>
  readonly layout: Targeted<Layout.Plan>
  readonly mir: Targeted<Mir.Module>
}

/** One available anonymous expression type cached for position fallback. */
export interface AnonymousExpression {
  readonly span: FrontendTooling.AnonymousExpression['span']
  readonly type: FrontendTooling.AnonymousExpression['type']
}

/** The occurrence-first semantic subject selected for a hover request. */
export type HoverSubject =
  | {
      readonly _tag: 'OccurrenceHoverSubject'
      readonly occurrence: SemanticOccurrence.SemanticOccurrence
      readonly presentation: Presentation.Presentation
    }
  | {
      readonly _tag: 'ExpressionHoverSubject'
      readonly expression: AnonymousExpression
      readonly presentation: Presentation.Presentation
    }

/** Source facts that make backend emission unavailable while keeping analysis queryable. */
export class CodegenUnavailable extends Data.TaggedError('CodegenUnavailable')<{
  readonly operation: 'Analysis.codegen'
  readonly message: string
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly resolutionFailures: ReadonlyArray<SourceResolver.SourceResolverError>
}> {}

/** Builds the frontend snapshot of one compilation request. */
export const make = Effect.fn('Analysis.make')(function* (
  request: ModuleClosure.CompilationRequest,
): Effect.fn.Return<SingleRootFrontendSnapshot, never, SourceResolver.SourceResolver> {
  const frontend = yield* Pipeline.frontend(request)
  const tooling = FrontendTooling.make(frontend)
  return Object.freeze({
    _tag: 'AnalysisSnapshot',
    realization: 'SingleRoot',
    ...frontend,
    ...tooling,
  })
})

/** Explicitly derives one immutable runtime snapshot from completed frontend facts. */
export const realize = (
  self: SingleRootFrontendSnapshot,
  target: string | undefined = self.requestedTarget,
): Snapshot => {
  const realization = Pipeline.realize(self, target)
  return Object.freeze({
    ...self,
    ...realization,
    _tag: 'AnalysisSnapshot',
    realization: 'SingleRoot',
  })
}

/** Builds and explicitly realizes one compilation request in a single effect. */
export const makeRealized = Effect.fn('Analysis.makeRealized')(function* (
  request: ModuleClosure.CompilationRequest,
): Effect.fn.Return<Snapshot, never, SourceResolver.SourceResolver> {
  return realize(yield* make(request))
})

/** Builds the snapshot of one single-module source. */
export const ofSource = (
  sourceId: string,
  bytes: Uint8Array,
  target?: string,
): Effect.Effect<SingleRootFrontendSnapshot> =>
  Effect.provide(
    target === undefined
      ? make({ root: SourceFile.make(sourceId, bytes) })
      : make({ root: SourceFile.make(sourceId, bytes), target }),
    SourceResolver.empty,
  )

/** Builds and explicitly realizes one single-module source. */
export const ofSourceRealized = (
  sourceId: string,
  bytes: Uint8Array,
  target?: string,
): Effect.Effect<Snapshot> => Effect.map(ofSource(sourceId, bytes, target), realize)

/** Returns every loaded module of the snapshot in canonical identity order. */
export const modules = (self: FrontendSnapshot): ReadonlyArray<ModuleClosure.Module> =>
  self.closure.modules

/** Returns the snapshot's import cycle facts in canonical order. */
export const cycles = (self: FrontendSnapshot): ReadonlyArray<ReadonlyArray<string>> =>
  self.closure.cycles

/** Returns exact immutable source snapshots for every successfully loaded module. */
export const sources = (self: FrontendSnapshot): ReadonlyMap<string, SourceFile.SourceFile> =>
  self.closure.sources

/** Returns the smallest exact-token semantic occurrence containing one byte offset. */
export const semanticOccurrenceAt = (
  self: FrontendSnapshot,
  module: string,
  byteOffset: number,
): SemanticOccurrence.SemanticOccurrence | undefined =>
  SemanticOccurrence.at(self.semanticOccurrences, module, byteOffset)

/** Returns exact-token semantic occurrences overlapping one source range. */
export const semanticOccurrencesInRange = (
  self: FrontendSnapshot,
  module: string,
  range: SourceSpan.SourceSpan,
): ReadonlyArray<SemanticOccurrence.SemanticOccurrence> =>
  SemanticOccurrence.inRange(self.semanticOccurrences, module, range)

/** Returns operational source-resolution failures in canonical module order. */
export const resolutionFailures = (
  self: FrontendSnapshot,
): ReadonlyArray<SourceResolver.SourceResolverError> => self.closure.resolutionFailures

/** Returns the closure's declaration index. */
export const declarationIndex = (self: FrontendSnapshot): DeclarationIndex.Index => self.index

export const nameResolution = (self: FrontendSnapshot): NameResolution.Resolution => self.resolution
export const moduleScope = (
  self: FrontendSnapshot,
  module: string,
): NameResolution.ModuleScope | undefined => NameResolution.scopeOf(self.resolution, module)
export const lookupName = (
  self: FrontendSnapshot,
  module: string,
  spelling: string,
): NameResolution.Lookup => {
  const scope = moduleScope(self, module)
  return scope === undefined
    ? Object.freeze({ _tag: 'Missing', spelling })
    : NameResolution.lookup(scope, self.index, spelling)
}
export const lookupQualifiedName = (
  self: FrontendSnapshot,
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
export const syntaxOf = (
  self: FrontendSnapshot,
  module: string,
): SyntaxFile.SyntaxFile | undefined => self.results.get(module)?.syntax

/** Returns one module's raw leading `//!` documentation without interpreting Markdown. */
export const moduleDocumentation = (
  self: FrontendSnapshot,
  module: string,
): DocBlock.DocBlock | undefined => {
  const syntax = syntaxOf(self, module)
  return syntax === undefined ? undefined : DocBlock.ofModule(syntax)
}

/** Returns raw documentation for a source-owned declaration-like syntax node. */
export const documentationOfSyntax = (
  self: FrontendSnapshot,
  module: string,
  node: SyntaxTree.Node,
): DocBlock.DocBlock | undefined => {
  const syntax = syntaxOf(self, module)
  return syntax === undefined ? undefined : DocBlock.ofNode(syntax, node)
}

/** Returns one module's elaborated analysis, or `undefined` for an unknown identity. */
export const moduleAnalysis = (
  self: FrontendSnapshot,
  module: string,
): Elaboration.Result | undefined => self.results.get(module)

/** Returns the root module's elaborated analysis. The root is always loaded. */
export const rootAnalysis = (self: FrontendSnapshot): Elaboration.Result => {
  const result = self.results.get(self.closure.rootModule)
  if (result === undefined) {
    throw new RangeError(`Snapshot lost its root module ${self.closure.rootModule}`)
  }
  return result
}

const declarationForIdentity = (
  self: FrontendSnapshot,
  identity: Extract<SemanticOccurrence.Identity, { readonly _tag: 'DeclarationIdentity' }>,
): DeclarationIndex.MemberFact | undefined => {
  if (identity.id._tag === 'CanonicalDeclarationId')
    return DeclarationIndex.byCanonical(self.index, identity.id)
  const local = identity.id
  return self.index.modules
    .flatMap((module) => module.members)
    .find((member) => member.id.sourceId === local.sourceId && member.id.ordinal === local.ordinal)
}

const syntaxForIdentity = (
  self: FrontendSnapshot,
  identity: SemanticOccurrence.Identity,
): SyntaxTree.Node | undefined => {
  if (identity._tag === 'DeclarationIdentity') return declarationForIdentity(self, identity)?.syntax
  if (identity._tag === 'TypeParameterIdentity') {
    for (const headers of self.index.modules)
      for (const member of headers.members) {
        const parameter = member.typeParameters.find((candidate) =>
          Type.equals(candidate.type, identity.id),
        )
        if (parameter !== undefined) return parameter.syntax
      }
    return undefined
  }
  if (identity._tag === 'ParameterIdentity') {
    for (const headers of self.index.modules)
      for (const declaration of headers.declarations) {
        const parameter = declaration.parameters.find(
          (candidate) =>
            candidate.id.function.sourceId === identity.id.function.sourceId &&
            candidate.id.function.ordinal === identity.id.function.ordinal &&
            candidate.id.ordinal === identity.id.ordinal,
        )
        if (parameter !== undefined) return parameter.syntax
      }
    return undefined
  }
  if (identity._tag === 'FieldIdentity') {
    for (const headers of self.index.modules)
      for (const declaration of headers.structs) {
        const field = declaration.fields.find(
          (candidate) =>
            candidate.id.struct.sourceId === identity.id.struct.sourceId &&
            candidate.id.struct.ordinal === identity.id.struct.ordinal &&
            candidate.id.ordinal === identity.id.ordinal,
        )
        if (field !== undefined) return field.syntax
      }
  }
  return undefined
}

/** Returns raw documentation for one source-backed semantic identity. */
export const documentationOfIdentity = (
  self: FrontendSnapshot,
  identity: SemanticOccurrence.Identity,
): DocBlock.DocBlock | undefined => {
  const node = syntaxForIdentity(self, identity)
  return node === undefined ? undefined : documentationOfSyntax(self, node.span.sourceId, node)
}

/** Resolves one source position and returns the selected declaration's raw documentation. */
export const documentationAt = (
  self: FrontendSnapshot,
  module: string,
  byteOffset: number,
): DocBlock.DocBlock | undefined => {
  const occurrence = semanticOccurrenceAt(self, module, byteOffset)
  return occurrence?.resolution._tag === 'Available'
    ? documentationOfIdentity(self, occurrence.resolution.identity)
    : undefined
}

const presentationOfIdentity = (
  self: FrontendSnapshot,
  module: string,
  identity: SemanticOccurrence.Identity,
): Presentation.Presentation | undefined => {
  const scope = NameResolution.scopeOf(self.resolution, module)
  if (identity._tag === 'DeclarationIdentity') {
    const declaration = declarationForIdentity(self, identity)
    return declaration?._tag === 'FunctionDeclaration'
      ? Presentation.functionDeclaration(declaration)
      : declaration?._tag === 'StructDeclaration'
        ? Presentation.structDeclaration(declaration)
        : declaration?._tag === 'ConstantDeclaration'
          ? Presentation.constantDeclaration(declaration)
          : undefined
  }
  if (identity._tag === 'TypeParameterIdentity') {
    for (const headers of self.index.modules)
      for (const member of headers.members) {
        const parameter = member.typeParameters.find((candidate) =>
          Type.equals(candidate.type, identity.id),
        )
        if (parameter !== undefined) return Presentation.typeParameter(parameter)
      }
    return undefined
  }
  if (identity._tag === 'ParameterIdentity') {
    for (const headers of self.index.modules)
      for (const declaration of headers.declarations) {
        const parameter = declaration.parameters.find(
          (candidate) =>
            candidate.id.function.sourceId === identity.id.function.sourceId &&
            candidate.id.function.ordinal === identity.id.function.ordinal &&
            candidate.id.ordinal === identity.id.ordinal,
        )
        if (parameter !== undefined) return Presentation.parameter(parameter)
      }
    return undefined
  }
  if (identity._tag === 'FieldIdentity') {
    for (const headers of self.index.modules)
      for (const declaration of headers.structs) {
        const field = declaration.fields.find(
          (candidate) =>
            candidate.id.struct.sourceId === identity.id.struct.sourceId &&
            candidate.id.struct.ordinal === identity.id.struct.ordinal &&
            candidate.id.ordinal === identity.id.ordinal,
        )
        if (field !== undefined) return Presentation.field(field)
      }
    return undefined
  }
  if (identity._tag === 'BindingIdentity') {
    for (const result of self.results.values())
      for (const fn of result.functions) {
        const binding = fn.bindings.find(
          (candidate) =>
            candidate.id.function.sourceId === identity.id.function.sourceId &&
            candidate.id.function.ordinal === identity.id.function.ordinal &&
            candidate.id.ordinal === identity.id.ordinal,
        )
        if (binding !== undefined) return Presentation.binding(binding, module, scope)
      }
    return undefined
  }
  if (identity._tag === 'PatternBindingIdentity') {
    const key = SemanticOccurrence.identityKey(identity)
    for (const result of self.results.values())
      for (const fn of result.functions)
        for (const statement of fn.statements)
          for (const expression of FrontendTooling.statementExpressions(statement))
            if (expression._tag === 'Match')
              for (const arm of expression.arms)
                for (const binding of arm.bindings)
                  if (
                    SemanticOccurrence.identityKey(
                      Object.freeze({ _tag: 'PatternBindingIdentity', id: binding.id }),
                    ) === key
                  )
                    return Presentation.patternBinding(binding, module, scope)
    return undefined
  }
  if (identity._tag === 'ImportNamespaceIdentity')
    return Presentation.importBinding(identity.spelling, identity.module)
  if (identity._tag === 'IntrinsicActorIdentity') {
    const intrinsic = Intrinsic.findActor(identity.id.name)
    return intrinsic === undefined ? undefined : Presentation.intrinsicActor(intrinsic)
  }
  const intrinsic = Intrinsic.findOperation(identity.id.actor, identity.id.name)
  return intrinsic === undefined ? undefined : Presentation.intrinsicOperation(intrinsic)
}

/** Lazily presents one available occurrence through declaration and scope facts. */
export const occurrencePresentation = (
  self: FrontendSnapshot,
  module: string,
  occurrence: SemanticOccurrence.SemanticOccurrence,
): Presentation.Presentation | undefined =>
  occurrence.resolution._tag === 'Available'
    ? presentationOfIdentity(self, module, occurrence.resolution.identity)
    : undefined

/** Returns the smallest cached available anonymous expression containing one byte offset. */
export const anonymousExpressionAt = (
  self: FrontendSnapshot,
  module: string,
  offset: number,
): AnonymousExpression | undefined =>
  (self.anonymousExpressions.get(module) ?? [])
    .filter((candidate) => candidate.span.start <= offset && offset < candidate.span.end)
    .reduce<AnonymousExpression | undefined>((selected, candidate) => {
      if (selected === undefined) return candidate
      return candidate.span.end - candidate.span.start < selected.span.end - selected.span.start
        ? candidate
        : selected
    }, undefined)

/** Selects an occurrence presentation first, with anonymous expression type as a strict fallback. */
export const hoverSubjectAt = (
  self: FrontendSnapshot,
  module: string,
  offset: number,
): HoverSubject | undefined => {
  const occurrence = semanticOccurrenceAt(self, module, offset)
  if (occurrence !== undefined) {
    const presentation = occurrencePresentation(self, module, occurrence)
    return presentation === undefined
      ? undefined
      : Object.freeze({ _tag: 'OccurrenceHoverSubject', occurrence, presentation })
  }
  const expression = anonymousExpressionAt(self, module, offset)
  if (expression === undefined) return undefined
  return Object.freeze({
    _tag: 'ExpressionHoverSubject',
    expression,
    presentation: Presentation.expressionType(
      expression.type,
      module,
      NameResolution.scopeOf(self.resolution, module),
    ),
  })
}

/** Returns source-ordered inferred local type hints clipped to one byte range. */
export const typeHints = (
  self: FrontendSnapshot,
  module: string,
  start: number,
  end: number,
): ReadonlyArray<TypeHint.TypeHint> =>
  TypeHint.make(
    bindingsOf(self, module),
    module,
    NameResolution.scopeOf(self.resolution, module),
    start,
    end,
  )

/** Returns deterministic recovery-aware completion for one module byte offset. */
export const completionAt = (
  self: FrontendSnapshot,
  module: string,
  offset: number,
): Completion.Result | undefined => {
  const source = self.closure.sources.get(module)
  const result = self.results.get(module)
  return source === undefined || result === undefined
    ? undefined
    : Completion.complete({
        source,
        module,
        offset,
        index: self.index,
        resolution: self.resolution,
        result,
      })
}

const nestedStatementFacts = (
  statement: Elaboration.StatementFact,
): ReadonlyArray<Elaboration.StatementFact> => {
  switch (statement._tag) {
    case 'UnsafeStatement':
      return Object.freeze([statement, ...statement.statements.flatMap(nestedStatementFacts)])
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
  self: FrontendSnapshot,
  module: string,
): ReadonlyArray<Elaboration.StatementFact> =>
  Object.freeze(
    self.results
      .get(module)
      ?.functions.flatMap((fn) => fn.statements.flatMap(nestedStatementFacts)) ?? [],
  )

/** Returns every binding with its canonical identity and immutable/mutable classification. */
export const bindingsOf = (
  self: FrontendSnapshot,
  module: string,
): ReadonlyArray<Elaboration.BindingDeclarationFact> =>
  Object.freeze(self.results.get(module)?.functions.flatMap((fn) => fn.bindings) ?? [])

/** Returns every complete or unavailable assignment fact in source order. */
export const writesOf = (
  self: FrontendSnapshot,
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
  self: FrontendSnapshot,
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
  self: FrontendSnapshot,
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
  self: FrontendSnapshot,
  module: string,
): ReadonlyArray<Elaboration.ExpressionFact> =>
  Object.freeze(
    self.results
      .get(module)
      ?.functions.flatMap((fn) => fn.statements.flatMap(FrontendTooling.statementExpressions)) ??
      [],
  )

/** Returns every retained semantic match with source patterns and canonical coverage facts. */
export const matchesOf = (
  self: FrontendSnapshot,
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
  self: FrontendSnapshot,
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
  self: FrontendSnapshot,
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
  self: FrontendSnapshot,
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
  self: FrontendSnapshot,
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
  self: FrontendSnapshot,
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
    } else if (member._tag === 'StructDeclaration') {
      for (const field of member.fields) {
        if (field.declaredType._tag === 'Resolved') add(field.declaredType.type)
      }
    } else if (member.declaredType._tag === 'Resolved') {
      add(member.declaredType.type)
    }
  }
  for (const expression of expressionsOf(self, module)) {
    if (expression.type._tag === 'Available') add(expression.type.type)
  }
  return Object.freeze([...found.values()])
}

/** Returns one module's HIR, or `undefined` for an unknown identity. */
export const hirOf = (self: FrontendSnapshot, module: string): Hir.Module | undefined =>
  self.results.get(module)?.hir

/** Returns one module's ownership facts and cleanup plans, or `undefined` for an unknown identity. */
export const ownershipOf = (
  self: FrontendSnapshot,
  module: string,
): Ownership.ModuleOwnership | undefined => self.ownership.get(module)

/** Returns deterministic loop-header ownership fixed points for one module. */
export const ownershipFixedPointsOf = (
  self: FrontendSnapshot,
  module: string,
): ReadonlyArray<Ownership.LoopFixedPoint> =>
  Object.freeze(self.ownership.get(module)?.functions.flatMap((fn) => fn.fixedPoints) ?? [])

/** Returns every lexical cleanup exit, including loop fallthrough and transfers. */
export const cleanupExitsOf = (
  self: FrontendSnapshot,
  module: string,
): ReadonlyArray<Ownership.ExitPlan> =>
  Object.freeze(self.ownership.get(module)?.functions.flatMap((fn) => fn.exits) ?? [])

/** Returns the snapshot's instance discovery: entry state and ordered instances. */
export const instancesOf = (self: Snapshot): Instances.Discovery => self.instances

/** Returns declarations that own canonical type parameters in module/source order. */
export const genericDeclarationsOf = (
  self: FrontendSnapshot,
): ReadonlyArray<DeclarationIndex.MemberFact> =>
  Object.freeze(
    self.index.modules.flatMap((module) =>
      module.members.filter((member) => member.typeParameters.length > 0),
    ),
  )

/** Returns every typed generic call in deterministic HIR preorder. */
export const genericCallsOf = (
  self: FrontendSnapshot,
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
  self: FrontendSnapshot,
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
  self: FrontendSnapshot,
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
  self: FrontendSnapshot,
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
  self: FrontendSnapshot,
  module: string,
  spelling: string,
): DeclarationIndex.DeclarationLookup => DeclarationIndex.lookup(self.index, module, spelling)

/** Looks up a function or struct in the shared module-level namespace. */
export const memberByName = (
  self: FrontendSnapshot,
  module: string,
  spelling: string,
): DeclarationIndex.MemberLookup => DeclarationIndex.member(self.index, module, spelling)

/** Looks up one nominal struct declaration. */
export const structByName = (
  self: FrontendSnapshot,
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
export const diagnostics = (self: FrontendSnapshot): ReadonlyArray<Diagnostic.Diagnostic> =>
  self.diagnostics

/** Returns immutable operational observations for exactly the phases that produced this snapshot. */
export const phases = (self: FrontendSnapshot): ReadonlyArray<PhaseReport.PhaseReport> =>
  self.report

/** Emits the snapshot's lowered program through the nominal backend service. */
/**
 * Emits the snapshot's lowered program.
 *
 * LLVM is the default backend independently of the snapshot target. Passing a backend explicitly
 * preserves that selection and compatibility is validated at the backend boundary.
 */
export const codegen = Effect.fn('Analysis.codegen')(function* <
  A extends Backend.Artifact = Backend.LlvmBitcodeArtifact,
>(
  self: Snapshot,
  request: Backend.CodegenRequest,
  backend?: Backend.Backend<A>,
): Effect.fn.Return<
  A,
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
  // The cast closes the generic default/override variance gap: omitted selection is LLVM, while
  // an explicit backend determines A at the call site.
  const selected = backend ?? (Backend.LlvmBackend as Backend.Backend<A>)
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
 * Emits the snapshot through the direct WebAssembly backend. The final-module artifact's `wat`
 * carries inspection text and `bytes` carries the instantiable module. A native-target snapshot
 * is rejected by compatibility validation.
 */
export const codegenWasm = Effect.fn('Analysis.codegenWasm')(function* (
  self: Snapshot,
  request: Backend.CodegenRequest,
): Effect.fn.Return<
  Backend.WebAssemblyModuleArtifact,
  Backend.BackendError | Target.TargetError | AnalysisUnavailable | CodegenUnavailable
> {
  const artifact = yield* codegen(self, request, WasmBackend.WasmBackend)
  if (artifact._tag === 'WebAssemblyModuleArtifact') return artifact
  return yield* new Backend.BackendError({
    operation: 'Backend.emit',
    backend: WasmBackend.WasmBackend.name,
    message: 'WebAssembly backend returned a non-WebAssembly artifact',
    reason: { _tag: 'UnsupportedMir', detail: 'backend artifact kind mismatch' },
  })
})

/** Returns backend-local control constructs with canonical region and source provenance. */
export const backendControlOf = (
  artifact: Backend.Artifact,
): ReadonlyArray<Backend.ControlProvenance> => artifact.control

/** Executes the snapshot's lowered MIR program through the closed bootstrap interpreter. */
export const evaluate = (
  self: Snapshot,
  options: BootstrapEvaluation.Options = {},
): BootstrapEvaluation.Outcome =>
  BootstrapEvaluation.evaluate(self.instances, loweredMir(self), options)

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

/** Returns logical allocation and typed-storage events without exposing host addresses. */
export const allocationTraceEventsOf = (
  outcome: BootstrapEvaluation.Outcome,
): ReadonlyArray<BootstrapEvaluation.AllocationTraceEvent> =>
  Object.freeze(
    outcome.trace.filter(
      (event): event is BootstrapEvaluation.AllocationTraceEvent =>
        event._tag === 'AllocationAcquire' ||
        event._tag === 'RawBufferForm' ||
        event._tag === 'SlotProject' ||
        event._tag === 'SlotWrite' ||
        event._tag === 'SlotTake' ||
        event._tag === 'SlotCopy' ||
        event._tag === 'SlotDrop' ||
        event._tag === 'AllocationRelease',
    ),
  )
