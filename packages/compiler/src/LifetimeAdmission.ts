import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as Lifetime from './Lifetime.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'
import * as TypeOutlives from './TypeOutlives.js'
import * as TypeInference from './internal/TypeInference.js'

/** A gated storage use retained until a selected generic application supplies its data types. */
export interface Obligation {
  readonly type: Type.Type
  readonly span: SourceSpan.SourceSpan
}

type Feature = 'EffectOutcome'

/** Declaration-owned storage facts used by the current lifetime feature admission boundary. */
export interface Context {
  readonly index: DeclarationIndex.Index
  readonly nominals: ReadonlyMap<string, DeclarationFacts.StructFact | DeclarationFacts.UnionFact>
  readonly lifetimes: TypeOutlives.Context
  readonly cache: Map<string, ReadonlyArray<Feature>>
}

const contexts = new WeakMap<DeclarationIndex.Index, Context>()
const nominalKey = (type: Type.Nominal): string => Type.key(Type.specializeNominal(type, []))

/** Builds one reusable index without recognizing any standard-library declaration by name. */
export const context = (index: DeclarationIndex.Index): Context => {
  const cached = contexts.get(index)
  if (cached !== undefined) return cached
  const nominals = new Map<string, DeclarationFacts.StructFact | DeclarationFacts.UnionFact>()
  for (const declaration of [
    ...index.modules.flatMap((module) => [...module.structs, ...module.unions]),
    ...index.generatedAggregates.values(),
  ]) {
    if (declaration.canonical._tag !== 'Canonical') continue
    nominals.set(
      nominalKey(Type.nominal(declaration.canonical.id.module, declaration.canonical.id.name)),
      declaration,
    )
  }
  const result: Context = {
    index,
    nominals,
    lifetimes: TypeOutlives.context(index.modules),
    cache: new Map(),
  }
  contexts.set(index, result)
  return result
}

/** Adds occurrence-generated field declarations without rebuilding the module lifetime index. */
export const withAggregates = (
  self: Context,
  declarations: Iterable<DeclarationFacts.StructFact>,
): Context => {
  const generated = [...declarations]
  if (generated.length === 0) return self
  const nominals = new Map(self.nominals)
  for (const declaration of generated)
    if (declaration.canonical._tag === 'Canonical')
      nominals.set(
        nominalKey(Type.nominal(declaration.canonical.id.module, declaration.canonical.id.name)),
        declaration,
      )
  return { ...self, nominals, cache: new Map() }
}

const dependent = (self: Context, type: Type.Type): boolean =>
  Type.storageLifetimes(type).some(
    (region) => !Lifetime.outlives(self.lifetimes.assumptions, region, Lifetime.staticLifetime),
  )

const features = (self: Context, type: Type.Type): ReadonlyArray<Feature> => {
  const identity = Type.key(type)
  const cached = self.cache.get(identity)
  if (cached !== undefined) return cached
  const found = new Set<Feature>()
  const visit = (type: Type.Type, seen: ReadonlySet<string>): void => {
    if (Type.isReference(type) || Type.isSlice(type)) {
      visit(Type.isReference(type) ? type.target : type.element, seen)
    } else if (Type.isFixedArray(type)) visit(type.element, seen)
    else if (Type.isUnion(type)) for (const member of type.members) visit(member, seen)
    else if (Type.isPointer(type)) visit(type.pointee, seen)
    else if (Type.isRepresented(type)) visit(type.contract, seen)
    else if (Type.isParameter(type)) {
      if (type.representationBound !== undefined) visit(type.representationBound, seen)
    } else if (Type.isCallable(type) || Type.isForeignFunction(type)) {
      for (const parameter of type.parameters) visit(parameter, seen)
      visit(type.result, seen)
    } else if (Type.isEffect(type)) {
      for (const outcome of [type.success, ...Type.failureMembers(type)]) {
        if (dependent(self, outcome)) found.add('EffectOutcome')
        visit(outcome, seen)
      }
    } else if (Type.isNominal(type)) {
      const declarationKey = nominalKey(type)
      const declaration = self.nominals.get(declarationKey)
      if (declaration === undefined || seen.has(declarationKey)) {
        for (const argument of type.arguments)
          if (Type.isTypeArgument(argument)) visit(argument, seen)
        return
      }
      const substitution = TypeInference.substitution(
        declaration.typeParameters.map((parameter) => parameter.type),
        type.arguments,
      )
      if (substitution === undefined) return
      const fields =
        declaration._tag === 'StructDeclaration'
          ? declaration.fields
          : declaration.variants.flatMap((variant) => variant.fields)
      const next = new Set(seen).add(declarationKey)
      for (const field of fields)
        if (field.declaredType._tag === 'Resolved')
          visit(Type.substitute(field.declaredType.type, substitution), next)
    }
  }
  visit(type, new Set())
  const result = Object.freeze([...found])
  self.cache.set(identity, result)
  return result
}

/** Checks concrete and universally named lifetime uses, leaving unknown data parameters deferred. */
export const check = (
  self: Context,
  type: Type.Type,
  span: SourceSpan.SourceSpan,
): ReadonlyArray<Diagnostic.Diagnostic> =>
  Object.freeze(
    features(self, type).map((feature) => Diagnostic.unsupportedLifetimeFeature(feature, span)),
  )

/** Discharges retained body uses at an already selected source-level generic application. */
export const instantiate = (
  self: Context,
  obligations: ReadonlyArray<Obligation>,
  substitution: Type.Substitution,
  span: SourceSpan.SourceSpan,
): ReadonlyArray<Diagnostic.Diagnostic> =>
  Diagnostic.merge(
    ...obligations.map((obligation) =>
      check(self, Type.substitute(obligation.type, substitution), span),
    ),
  )

/** Checks authored headers after aliases, generic arguments, and conformance heads are resolved. */
export const moduleDiagnostics = (
  self: Context,
  module: DeclarationFacts.ModuleHeaders,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const inspect = (fact: DeclarationFacts.DeclaredTypeFact): void => {
    if (fact._tag === 'Resolved') diagnostics.push(...check(self, fact.type, fact.syntax.span))
  }
  const operation = (
    member: DeclarationFacts.DeclarationFact | DeclarationFacts.ServiceOperationFact,
  ): void => {
    for (const parameter of member.parameters) inspect(parameter.declaredType)
    inspect(member.returnType)
    for (const failure of member.failureRow.members) inspect(failure)
    if (member.functionKind === 'Effect') {
      for (const outcome of [member.returnType, ...member.failureRow.members])
        if (outcome._tag === 'Resolved' && dependent(self, outcome.type))
          diagnostics.push(
            Diagnostic.unsupportedLifetimeFeature('EffectOutcome', outcome.syntax.span),
          )
    }
  }
  for (const member of module.members) {
    if (member._tag === 'FunctionDeclaration') operation(member)
    else if (member._tag === 'ConstantDeclaration') inspect(member.declaredType)
    else if (member._tag === 'AliasDeclaration') inspect(member.target)
    else if (member._tag === 'StructDeclaration' || member._tag === 'UnionDeclaration') {
      const fields =
        member._tag === 'StructDeclaration'
          ? member.fields
          : member.variants.flatMap((variant) => variant.fields)
      for (const field of fields) inspect(field.declaredType)
      if (member.canonical._tag === 'Canonical')
        diagnostics.push(
          ...check(
            self,
            Type.nominal(
              member.canonical.id.module,
              member.canonical.id.name,
              member.typeParameters.map((parameter) => Type.parameterArgument(parameter.type)),
            ),
            member.syntax.span,
          ),
        )
    } else if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration')
      for (const memberOperation of member.operations) operation(memberOperation)
  }
  return Diagnostic.merge(diagnostics)
}

/** Records only abstract uses that can change admission when a caller supplies generic arguments. */
export const body = (
  self: Context,
  statements: ReadonlyArray<Elaboration.StatementFact>,
): {
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly obligations: ReadonlyArray<Obligation>
} => {
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const obligations = new Map<string, Obligation>()
  Elaboration.visitStatementFacts(statements, {
    expression: (expression) => {
      if (expression.type._tag !== 'Available') return
      const type = expression.type.type
      diagnostics.push(...check(self, type, expression.syntax.span))
      if (Type.parameters(type).length > 0)
        obligations.set(Type.key(type), {
          type,
          span: expression.syntax.span,
        })
    },
  })
  return Object.freeze({
    diagnostics: Diagnostic.merge(diagnostics),
    obligations: Object.freeze([...obligations.values()]),
  })
}
