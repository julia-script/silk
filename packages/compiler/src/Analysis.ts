import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'

export { AnalysisUnavailable } from './AnalysisUnavailable.js'

import type { AnalysisUnavailable } from './AnalysisUnavailable.js'
import * as AutoImport from './AutoImport.js'
import * as Backend from './Backend.js'
import * as BootstrapEvaluation from './BootstrapEvaluation.js'
import * as Completion from './Completion.js'
import * as ConformanceProof from './ConformanceProof.js'
import * as Diagnostic from './Diagnostic.js'
import * as DocBlock from './DocBlock.js'
import type * as Elaboration from './Elaboration.js'
import * as ExecutableProperty from './ExecutableProperty.js'
import * as Frontend from './Frontend.js'
import * as FrontendTooling from './FrontendTooling.js'
import * as Instances from './Instances.js'
import * as Intrinsic from './Intrinsic.js'
import * as IntrinsicAvailability from './IntrinsicAvailability.js'
import * as Layout from './Layout.js'
import * as LlvmBackend from './LlvmBackend.js'
import type * as Mir from './Mir.js'
import * as MirVerification from './MirVerification.js'
import type * as ModuleClosure from './ModuleClosure.js'
import type * as ModuleSemantics from './ModuleSemantics.js'
import type * as ModuleSurface from './ModuleSurface.js'
import * as ModuleTooling from './ModuleTooling.js'
import * as NameResolution from './NameResolution.js'
import * as OpaqueRealization from './OpaqueRealization.js'
import type * as Ownership from './Ownership.js'
import type * as PhaseReport from './PhaseReport.js'
import * as Presentation from './Presentation.js'
import * as Realization from './Realization.js'
import type * as SemanticInvalidation from './SemanticInvalidation.js'
import * as SemanticOccurrence from './SemanticOccurrence.js'
import * as SourceFile from './SourceFile.js'
import * as SourceResolver from './SourceResolver.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as SyntaxTree from './SyntaxTree.js'
import * as Target from './Target.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'
import * as TypeHint from './TypeHint.js'
import * as WasmBackend from './WasmBackend.js'
import type * as WorkspaceInventory from './WorkspaceInventory.js'

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
  readonly toolingModules: ReadonlyMap<string, ModuleTooling.ModuleTooling>
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
  readonly span: ModuleTooling.AnonymousExpression['span']
  readonly type: ModuleTooling.AnonymousExpression['type']
}

/** The occurrence-first semantic subject selected for a hover request. */
export type HoverSubject =
  | {
      readonly _tag: 'OccurrenceHoverSubject'
      readonly occurrence: SemanticOccurrence.SemanticOccurrence
      readonly presentation: Presentation.Presentation
      readonly implementedContracts: ReadonlyArray<Presentation.Presentation>
    }
  | {
      readonly _tag: 'ExpressionHoverSubject'
      readonly expression: AnonymousExpression
      readonly presentation: Presentation.Presentation
      readonly implementedContracts: ReadonlyArray<Presentation.Presentation>
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
  const frontend = yield* Frontend.frontend(request)
  yield* Effect.yieldNow
  const tooling = yield* FrontendTooling.make(frontend)
  return OpaqueRealization.withCatalog(
    Object.freeze({
      _tag: 'AnalysisSnapshot',
      realization: 'SingleRoot',
      ...frontend,
      ...tooling,
    }),
    OpaqueRealization.catalogOf(frontend),
  )
})

/** Explicitly derives one immutable runtime snapshot from completed frontend facts. */
export const realize = (
  self: SingleRootFrontendSnapshot,
  target: string | undefined = self.requestedTarget ?? Target.x8664UnknownLinuxGnu.id,
  options: Frontend.Options = {},
): Snapshot => {
  const realization = Realization.realize(self, target, options)
  return OpaqueRealization.withCatalog(
    Object.freeze({
      ...self,
      ...realization,
      _tag: 'AnalysisSnapshot',
      realization: 'SingleRoot',
    }),
    OpaqueRealization.catalogOf(self),
  )
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
  options: Frontend.Options = {},
): Effect.Effect<Snapshot> =>
  Effect.map(ofSource(sourceId, bytes, target), (self) => realize(self, target, options))

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

/** Discovers compiler-owned auto-import actions for one unresolved source occurrence. */
export const autoImportsAt = (
  self: FrontendSnapshot,
  inventory: WorkspaceInventory.WorkspaceInventory,
  module: string,
  byteOffset: number,
): ReadonlyArray<AutoImport.Action> =>
  AutoImport.discover({ snapshot: self, inventory, module, byteOffset })

/** Revalidates and resolves one auto-import candidate against this exact snapshot. */
export const resolveAutoImport = (
  self: FrontendSnapshot,
  inventory: WorkspaceInventory.WorkspaceInventory,
  module: string,
  byteOffset: number,
  candidate: AutoImport.CandidateKey,
) => AutoImport.resolve({ snapshot: self, inventory, module, byteOffset, candidate })

/** Returns one module's raw leading `//!` documentation without interpreting Markdown. */
export const moduleDocumentation = (
  self: FrontendSnapshot,
  module: string,
): DocBlock.DocBlock | undefined => {
  const syntax = self.results.get(module)?.syntax
  return syntax === undefined ? undefined : DocBlock.ofModule(syntax)
}

/** Returns raw documentation for a source-owned declaration-like syntax node. */
export const documentationOfSyntax = (
  self: FrontendSnapshot,
  module: string,
  node: SyntaxTree.Node,
): DocBlock.DocBlock | undefined => {
  const syntax = self.results.get(module)?.syntax
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

/** Resolves the declaration one declaration identity names, canonical or module-local. */
export const declarationForIdentity = (
  self: FrontendSnapshot,
  identity: Extract<SemanticOccurrence.Identity, { readonly _tag: 'DeclarationIdentity' }>,
): DeclarationFacts.MemberFact | undefined => {
  if (identity.id._tag === 'CanonicalDeclarationId')
    return DeclarationFacts.byCanonical(self.index, identity.id)
  const local = identity.id
  return self.index.modules
    .flatMap((module) => module.members)
    .find((member) => member.id.sourceId === local.sourceId && member.id.ordinal === local.ordinal)
}

const serviceOperationForIdentity = (
  self: FrontendSnapshot,
  identity: Extract<SemanticOccurrence.Identity, { readonly _tag: 'ServiceOperationIdentity' }>,
): DeclarationFacts.ServiceOperationFact | undefined =>
  self.index.modules
    .flatMap((module) => module.services)
    .find(
      (service) =>
        service.id.sourceId === identity.id.service.sourceId &&
        service.id.ordinal === identity.id.service.ordinal,
    )
    ?.operations.find(
      (operation) =>
        operation.name._tag === 'Present' && operation.name.spelling === identity.id.name,
    )

const unionVariantForIdentity = (
  self: FrontendSnapshot,
  identity: Extract<SemanticOccurrence.Identity, { readonly _tag: 'UnionVariantIdentity' }>,
): readonly [DeclarationFacts.UnionFact, DeclarationFacts.UnionVariantFact] | undefined => {
  const union = DeclarationFacts.byCanonical(self.index, identity.id.union)
  if (union?._tag !== 'UnionDeclaration') return undefined
  const variant = union.variants.find(
    (candidate) =>
      candidate.canonical._tag === 'Canonical' && candidate.canonical.id.name === identity.id.name,
  )
  return variant === undefined ? undefined : Object.freeze([union, variant] as const)
}

const syntaxForIdentity = (
  self: FrontendSnapshot,
  identity: SemanticOccurrence.Identity,
): SyntaxTree.Node | undefined => {
  if (identity._tag === 'DeclarationIdentity') return declarationForIdentity(self, identity)?.syntax
  if (identity._tag === 'ServiceOperationIdentity')
    return serviceOperationForIdentity(self, identity)?.syntax
  if (identity._tag === 'EnumMemberIdentity')
    return self.index.modules
      .flatMap((module) => module.enums)
      .find(
        (enum_) =>
          enum_.canonical._tag === 'Canonical' &&
          enum_.canonical.id.module === identity.id.enum.module &&
          enum_.canonical.id.name === identity.id.enum.name,
      )
      ?.members.find(
        (member) =>
          member.canonical._tag === 'Canonical' && member.canonical.id.name === identity.id.name,
      )?.syntax
  if (identity._tag === 'UnionVariantIdentity')
    return unionVariantForIdentity(self, identity)?.[1].syntax
  if (identity._tag === 'EnumAssociatedOperationIdentity')
    return DeclarationFacts.byCanonical(self.index, identity.id.enum)?.syntax
  if (identity._tag === 'TypeParameterIdentity') {
    for (const headers of self.index.modules)
      for (const member of headers.members) {
        const parameter = member.typeParameters.find((candidate) =>
          Type.equals(candidate.type, identity.id),
        )
        if (parameter !== undefined) return parameter.syntax
        if (member._tag === 'ServiceDeclaration')
          for (const operation of member.operations) {
            const operationParameter = operation.typeParameters.find((candidate) =>
              Type.equals(candidate.type, identity.id),
            )
            if (operationParameter !== undefined) return operationParameter.syntax
          }
      }
    return undefined
  }
  if (identity._tag === 'ParameterIdentity') {
    for (const headers of self.index.modules)
      for (const declaration of [
        ...headers.declarations,
        ...headers.services.flatMap((service) => service.operations),
      ]) {
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
    for (const headers of self.index.modules) {
      const fields = [
        ...headers.structs.flatMap((declaration) => declaration.fields),
        ...headers.unions.flatMap((declaration) =>
          declaration.variants.flatMap((variant) => variant.fields),
        ),
      ]
      for (const field of fields) {
        if (DeclarationFacts.sameFieldId(field.id, identity.id)) return field.syntax
      }
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

interface HoverPresentation {
  readonly presentation: Presentation.Presentation
  readonly type?: Type.Type
}

const hoverPresentation = (
  presentation: Presentation.Presentation | undefined,
  type?: Type.Type,
): HoverPresentation | undefined => {
  if (presentation === undefined) {
    return undefined
  }
  return Object.freeze({ presentation, ...(type === undefined ? {} : { type }) })
}

const nominalDeclarationType = (
  declaration:
    | DeclarationFacts.StructFact
    | DeclarationFacts.UnionFact
    | DeclarationFacts.EnumFact
    | DeclarationFacts.ContractFact,
): Type.Nominal | undefined =>
  declaration.canonical._tag === 'Canonical'
    ? Type.nominal(
        declaration.canonical.id.module,
        declaration.canonical.id.name,
        declaration.typeParameters.map((parameter) => Type.parameterArgument(parameter.type)),
      )
    : undefined

const declaredType = (fact: DeclarationFacts.DeclaredTypeFact): Type.Type | undefined =>
  fact._tag === 'Resolved' ? fact.type : undefined

const presentationOfIdentity = (
  self: FrontendSnapshot,
  module: string,
  identity: SemanticOccurrence.Identity,
): HoverPresentation | undefined => {
  const scope = NameResolution.scopeOf(self.resolution, module)
  if (identity._tag === 'DeclarationIdentity') {
    const declaration = declarationForIdentity(self, identity)
    if (declaration?._tag === 'FunctionDeclaration')
      return hoverPresentation(
        Presentation.functionDeclaration(declaration),
        declaredType(declaration.returnType),
      )
    if (declaration?._tag === 'StructDeclaration')
      return hoverPresentation(
        Presentation.structDeclaration(declaration),
        nominalDeclarationType(declaration),
      )
    if (declaration?._tag === 'EnumDeclaration')
      return hoverPresentation(
        Presentation.enumDeclaration(declaration),
        nominalDeclarationType(declaration),
      )
    if (declaration?._tag === 'UnionDeclaration')
      return hoverPresentation(
        Presentation.unionDeclaration(declaration),
        nominalDeclarationType(declaration),
      )
    if (declaration?._tag === 'ServiceDeclaration')
      return hoverPresentation(
        Presentation.serviceDeclaration(declaration),
        nominalDeclarationType(declaration),
      )
    if (declaration?._tag === 'InterfaceDeclaration')
      return hoverPresentation(
        Presentation.serviceDeclaration(declaration),
        nominalDeclarationType(declaration),
      )
    return declaration?._tag === 'ConstantDeclaration'
      ? hoverPresentation(
          Presentation.constantDeclaration(declaration),
          declaredType(declaration.declaredType),
        )
      : undefined
  }
  if (identity._tag === 'TypeParameterIdentity') {
    for (const headers of self.index.modules)
      for (const member of headers.members) {
        const parameter = member.typeParameters.find((candidate) =>
          Type.equals(candidate.type, identity.id),
        )
        if (parameter !== undefined) return hoverPresentation(Presentation.typeParameter(parameter))
        if (member._tag === 'ServiceDeclaration')
          for (const operation of member.operations) {
            const operationParameter = operation.typeParameters.find((candidate) =>
              Type.equals(candidate.type, identity.id),
            )
            if (operationParameter !== undefined)
              return hoverPresentation(Presentation.typeParameter(operationParameter))
          }
      }
    return undefined
  }
  if (identity._tag === 'ParameterIdentity') {
    for (const headers of self.index.modules)
      for (const declaration of [
        ...headers.declarations,
        ...headers.services.flatMap((service) => service.operations),
      ]) {
        const parameter = declaration.parameters.find(
          (candidate) =>
            candidate.id.function.sourceId === identity.id.function.sourceId &&
            candidate.id.function.ordinal === identity.id.function.ordinal &&
            candidate.id.ordinal === identity.id.ordinal,
        )
        if (parameter !== undefined)
          return hoverPresentation(
            Presentation.parameter(parameter),
            declaredType(parameter.declaredType),
          )
      }
    return undefined
  }
  if (identity._tag === 'FieldIdentity') {
    for (const headers of self.index.modules) {
      const fields = [
        ...headers.structs.flatMap((declaration) => declaration.fields),
        ...headers.unions.flatMap((declaration) =>
          declaration.variants.flatMap((variant) => variant.fields),
        ),
      ]
      for (const field of fields)
        if (DeclarationFacts.sameFieldId(field.id, identity.id))
          return hoverPresentation(Presentation.field(field), declaredType(field.declaredType))
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
        if (binding !== undefined)
          return hoverPresentation(
            Presentation.binding(binding, module, scope),
            binding.inferredType._tag === 'Available' ? binding.inferredType.type : undefined,
          )
      }
    return undefined
  }
  if (identity._tag === 'PatternBindingIdentity') {
    const key = SemanticOccurrence.identityKey(identity)
    const findStatementBinding = (
      statements: ReadonlyArray<Elaboration.StatementFact>,
    ): Elaboration.PatternBindingFact | undefined => {
      for (const statement of statements) {
        if (statement._tag === 'PatternBindStatement' || statement._tag === 'IfLetStatement')
          for (const binding of statement.selection.bindings)
            if (
              SemanticOccurrence.identityKey(
                Object.freeze({ _tag: 'PatternBindingIdentity', id: binding.id }),
              ) === key
            )
              return binding
        let nested: readonly Elaboration.StatementFact[]
        if (statement._tag === 'UnsafeStatement') {
          nested = statement.statements
        } else if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
          nested = [...statement.taken, ...statement.otherwise]
        } else if (statement._tag === 'WhileStatement') {
          nested = statement.body
        } else {
          nested = []
        }
        const found = findStatementBinding(nested)
        if (found !== undefined) return found
      }
      return undefined
    }
    for (const result of self.results.values())
      for (const fn of result.functions) {
        const statementBinding = findStatementBinding(fn.statements)
        if (statementBinding !== undefined)
          return hoverPresentation(
            Presentation.patternBinding(statementBinding, module, scope),
            statementBinding.type._tag === 'Available' ? statementBinding.type.type : undefined,
          )
      }
    for (const result of self.results.values())
      for (const fn of result.functions)
        for (const statement of fn.statements)
          for (const expression of ModuleTooling.statementExpressions(statement))
            if (expression._tag === 'Match')
              for (const arm of expression.arms)
                for (const binding of arm.bindings)
                  if (
                    SemanticOccurrence.identityKey(
                      Object.freeze({ _tag: 'PatternBindingIdentity', id: binding.id }),
                    ) === key
                  )
                    return hoverPresentation(
                      Presentation.patternBinding(binding, module, scope),
                      binding.type._tag === 'Available' ? binding.type.type : undefined,
                    )
    return undefined
  }
  if (identity._tag === 'ImportNamespaceIdentity')
    return hoverPresentation(Presentation.importBinding(identity.spelling, identity.module))
  if (identity._tag === 'ServiceOperationIdentity') {
    const operation = serviceOperationForIdentity(self, identity)
    return operation === undefined
      ? undefined
      : hoverPresentation(
          Presentation.serviceOperation(operation),
          declaredType(operation.returnType),
        )
  }
  if (identity._tag === 'EnumMemberIdentity') {
    const enum_ = DeclarationFacts.byCanonical(self.index, identity.id.enum)
    if (enum_?._tag !== 'EnumDeclaration') return undefined
    const member = enum_.members.find(
      (candidate) =>
        candidate.canonical._tag === 'Canonical' &&
        candidate.canonical.id.name === identity.id.name,
    )
    return member === undefined
      ? undefined
      : hoverPresentation(Presentation.enumMember(enum_, member), nominalDeclarationType(enum_))
  }
  if (identity._tag === 'UnionVariantIdentity') {
    const selected = unionVariantForIdentity(self, identity)
    return selected === undefined
      ? undefined
      : hoverPresentation(
          Presentation.unionVariant(selected[0], selected[1]),
          nominalDeclarationType(selected[0]),
        )
  }
  if (identity._tag === 'EnumAssociatedOperationIdentity') {
    const enum_ = DeclarationFacts.byCanonical(self.index, identity.id.enum)
    const operation =
      enum_?._tag === 'EnumDeclaration'
        ? enum_.associatedOperations.find((candidate) => candidate.id.name === identity.id.name)
        : undefined
    return operation === undefined
      ? undefined
      : hoverPresentation(
          Presentation.enumAssociatedOperation(operation),
          operation.result.spelling,
        )
  }
  if (identity._tag === 'IntrinsicActorIdentity') {
    const intrinsic = Intrinsic.findActor(identity.id.name)
    return intrinsic === undefined
      ? undefined
      : hoverPresentation(Presentation.intrinsicActor(intrinsic))
  }
  const intrinsic = Intrinsic.findOperation(identity.id.actor, identity.id.name)
  return intrinsic === undefined
    ? undefined
    : hoverPresentation(Presentation.intrinsicOperation(intrinsic))
}

/** Lazily presents one available occurrence through declaration and scope facts. */
export const occurrencePresentation = (
  self: FrontendSnapshot,
  module: string,
  occurrence: SemanticOccurrence.SemanticOccurrence,
): Presentation.Presentation | undefined =>
  occurrence.resolution._tag === 'Available'
    ? presentationOfIdentity(self, module, occurrence.resolution.identity)?.presentation
    : undefined

const implementedContractPresentations = (
  self: FrontendSnapshot,
  module: string,
  type: Type.Type | undefined,
): ReadonlyArray<Presentation.Presentation> => {
  if (type === undefined) return Object.freeze([])
  const scope = NameResolution.scopeOf(self.resolution, module)
  return Object.freeze(
    ConformanceProof.implementedContracts(self.index, module, type).map((contract) =>
      Presentation.scopedNominal(contract, module, scope),
    ),
  )
}

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
    const answer =
      occurrence.resolution._tag === 'Available'
        ? presentationOfIdentity(self, module, occurrence.resolution.identity)
        : undefined
    return answer === undefined
      ? undefined
      : Object.freeze({
          _tag: 'OccurrenceHoverSubject',
          occurrence,
          presentation: answer.presentation,
          implementedContracts: implementedContractPresentations(self, module, answer.type),
        })
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
    implementedContracts: implementedContractPresentations(self, module, expression.type),
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
    self.results.get(module)?.functions ?? Object.freeze([]),
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

/** Returns every semantic expression fact in deterministic source nesting order. */
export const expressionsOf = (
  self: FrontendSnapshot,
  module: string,
): ReadonlyArray<Elaboration.ExpressionFact> =>
  Object.freeze(
    self.results
      .get(module)
      ?.functions.flatMap((fn) => fn.statements.flatMap(ModuleTooling.statementExpressions)) ?? [],
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

/** Returns every canonical or explicitly unavailable postfix referent-projection step. */
export const referentProjectionsOf = (
  self: FrontendSnapshot,
  module: string,
): ReadonlyArray<Elaboration.ReferentProjectionExpressionFact> =>
  Object.freeze(
    expressionsOf(self, module).filter(
      (expression): expression is Elaboration.ReferentProjectionExpressionFact =>
        expression._tag === 'ReferentProjection',
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
  const add = (type: DeclarationFacts.SemanticType): void => {
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
    } else if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration') {
      for (const operation of member.operations) {
        for (const parameter of operation.parameters)
          if (parameter.declaredType._tag === 'Resolved') add(parameter.declaredType.type)
        if (operation.returnType._tag === 'Resolved') add(operation.returnType.type)
      }
    } else if (member._tag === 'ConstantDeclaration' && member.declaredType._tag === 'Resolved') {
      add(member.declaredType.type)
    }
  }
  for (const expression of expressionsOf(self, module)) {
    if (expression.type._tag === 'Available') add(expression.type.type)
  }
  return Object.freeze([...found.values()])
}

/** Returns one module's ownership facts and cleanup plans, or `undefined` for an unknown identity. */
export const ownershipOf = (
  self: FrontendSnapshot,
  module: string,
): Ownership.ModuleOwnership | undefined => self.ownership.get(module)

/** Returns the snapshot's instance discovery: entry state and ordered instances. */
export const instancesOf = (self: Snapshot): Instances.Discovery => self.instances

/** Returns normalized environment and suspension properties for every realized executable. */
export const executablePropertiesOf = (self: Snapshot): ReadonlyArray<ExecutableProperty.Fact> =>
  ExecutableProperty.derive(self.instances, self.index, Instances.callableIdentity)

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

/** Returns every reachable array calling shape and its canonical physical paths. */
export const arrayCallingShapesOf = (self: Snapshot): ReadonlyArray<Layout.CallingShape> =>
  self.layout._tag === 'Available'
    ? Object.freeze(
        self.layout.value.callingShapes.filter((shape) => Type.isFixedArray(shape.type)),
      )
    : Object.freeze([])

/** Returns every match-local ownership and cleanup plan in function/source order. */
export const ownershipMatchesOf = (
  self: FrontendSnapshot,
  module: string,
): ReadonlyArray<Ownership.MatchOwnership> =>
  Object.freeze(self.ownership.get(module)?.functions.flatMap((fn) => fn.matches) ?? [])

/** Returns every structured MIR match, including nested expression matches, in DAG preorder. */
export const mirMatchesOf = (
  self: Snapshot,
): ReadonlyArray<Extract<Mir.Operation, { readonly _tag: 'Match' }>> => {
  if (self.mir._tag === 'Unavailable') {
    return Object.freeze([])
  }
  return Object.freeze(
    self.mir.value.functions.flatMap((fn) =>
      MirVerification.operations(fn).flatMap((operation) =>
        operation._tag === 'Match' ? [operation] : [],
      ),
    ),
  )
}

/** Returns the snapshot's available or explicitly unavailable lowered MIR state. */
export const mirOf = (self: Snapshot): Targeted<Mir.Module> => self.mir

/** Returns the snapshot's lowered MIR program for callers that already established availability. */
export const loweredMir = (self: Snapshot): Mir.Module => {
  if (self.mir._tag === 'Available') return self.mir.value
  throw new RangeError(self.mir.error.message)
}

/** Returns deterministic shared-MIR Effect normalization decisions for tooling and cost gates. */
export const effectNormalizationOf = (self: Snapshot): ReadonlyArray<Mir.NormalizationVerdict> =>
  self.mir._tag === 'Available'
    ? (self.mir.value.normalization ?? Object.freeze([]))
    : Object.freeze([])

/** Looks up one declaration name within one module. */
export const declarationByName = (
  self: FrontendSnapshot,
  module: string,
  spelling: string,
): DeclarationFacts.DeclarationLookup => DeclarationFacts.lookup(self.index, module, spelling)

/** Looks up one declaration in the shared module-level namespace. */
export const memberByName = (
  self: FrontendSnapshot,
  module: string,
  spelling: string,
): DeclarationFacts.MemberLookup => DeclarationFacts.member(self.index, module, spelling)

/** Looks up one nominal scalar enum declaration. */
export const enumByName = (
  self: FrontendSnapshot,
  module: string,
  spelling: string,
): DeclarationFacts.EnumLookup => DeclarationFacts.enumByName(self.index, module, spelling)

/** Looks up one declaration-ordered member from a resolved scalar enum. */
export const enumMemberByName = (
  declaration: DeclarationFacts.EnumFact,
  spelling: string,
): DeclarationFacts.EnumMemberLookup =>
  DeclarationFacts.lookupEnumMember(declaration.members, spelling)

/** Looks up one nominal struct declaration. */
export const structByName = (
  self: FrontendSnapshot,
  module: string,
  spelling: string,
): DeclarationFacts.StructLookup => DeclarationFacts.struct(self.index, module, spelling)

/** Looks up one nominal tagged-union declaration. */
export const unionByName = (
  self: FrontendSnapshot,
  module: string,
  spelling: string,
): DeclarationFacts.UnionLookup => DeclarationFacts.unionByName(self.index, module, spelling)

/** Looks up one declaration-ordered variant from a resolved nominal union. */
export const unionVariantByName = (
  declaration: DeclarationFacts.UnionFact,
  spelling: string,
): DeclarationFacts.UnionVariantLookup =>
  DeclarationFacts.lookupUnionVariant(declaration.variants, spelling)

/** Looks up one declaration-ordered field within a resolved nominal-union variant. */
export const unionVariantFieldByName = (
  variant: DeclarationFacts.UnionVariantFact,
  spelling: string,
): DeclarationFacts.FieldLookup => DeclarationFacts.lookupField(variant.fields, spelling)

/** Looks up one declaration-ordered field from a resolved nominal struct. */
export const fieldByName = (
  declaration: DeclarationFacts.StructFact,
  spelling: string,
): DeclarationFacts.FieldLookup => DeclarationFacts.lookupField(declaration.fields, spelling)

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
  // The cast closes the generic default/override variance gap: omitted selection is LLVM, while
  // an explicit backend determines A at the call site.
  const selected = backend ?? (LlvmBackend.LlvmBackend as Backend.Backend<A>)
  const availability = IntrinsicAvailability.select(
    self.instances.intrinsics,
    IntrinsicAvailability.backendTarget(selected.id),
  )
  if (availability._tag === 'Unavailable') {
    return yield* new CodegenUnavailable({
      operation: 'Analysis.codegen',
      message: `${selected.name} cannot emit a program with unavailable intrinsics`,
      diagnostics: Diagnostic.merge(self.diagnostics, availability.diagnostics),
      resolutionFailures: self.closure.resolutionFailures,
    })
  }
  if (self.mir._tag === 'Unavailable') return yield* self.mir.error
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

/** Executes the snapshot's lowered MIR program through the closed bootstrap interpreter. */
export const evaluate = (
  self: Snapshot,
  options: BootstrapEvaluation.Options = {},
): BootstrapEvaluation.Outcome =>
  BootstrapEvaluation.evaluate(self.instances, loweredMir(self), options)
