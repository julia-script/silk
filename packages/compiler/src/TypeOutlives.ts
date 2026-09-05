import * as DeclarationFacts from './DeclarationFacts.js'
import * as Diagnostic from './Diagnostic.js'
import * as Lifetime from './Lifetime.js'
import * as Type from './Type.js'

/** Declared universal storage obligations; absence of a parameter bound is not static validity. */
export interface Context {
  readonly assumptions: Lifetime.Assumptions
  readonly parameters: ReadonlyMap<string, DeclarationFacts.TypeParameterFact>
  readonly parameterBounds: ReadonlyMap<string, ReadonlyArray<Lifetime.Lifetime>>
  readonly nominals: ReadonlyMap<string, ReadonlyArray<DeclarationFacts.TypeParameterFact>>
  readonly work: {
    readonly declarations: number
    readonly headers: number
    readonly iterations: number
    readonly lifetimeObligations: number
    readonly typeObligations: number
  }
}

export type Prove = (longer: Lifetime.Lifetime, shorter: Lifetime.Lifetime) => boolean

const contexts = new WeakMap<ReadonlyArray<DeclarationFacts.ModuleHeaders>, Context>()
const nominalKey = (type: Type.Nominal): string => Type.key(Type.specializeNominal(type, []))

/** Indexes immutable declaration assumptions by canonical owner, never by binder spelling. */
export const context = (modules: ReadonlyArray<DeclarationFacts.ModuleHeaders>): Context => {
  const cached = contexts.get(modules)
  if (cached !== undefined) return cached
  const work = {
    declarations: 0,
    headers: 0,
    iterations: 0,
    lifetimeObligations: 0,
    typeObligations: 0,
  }
  const parameters = new Map<string, DeclarationFacts.TypeParameterFact>()
  const nominals = new Map<string, ReadonlyArray<DeclarationFacts.TypeParameterFact>>()
  const storedHeaders: Array<Type.Type> = []
  const executableHeaders: Array<Type.ExecutableLifetimes> = []
  const headerType = (fact: DeclarationFacts.DeclaredTypeFact): Type.Type | undefined => {
    if (fact._tag === 'Resolved') return fact.type
    if (fact._tag === 'Reference') {
      const target = headerType(fact.target)
      return target === undefined ? undefined : Type.reference(fact.access, target, fact.lifetime)
    }
    if (fact._tag === 'Slice') {
      const element = headerType(fact.element)
      return element === undefined ? undefined : Type.slice(fact.access, element, fact.lifetime)
    }
    if (fact._tag === 'FixedArray' && fact.length._tag === 'Available') {
      const element = headerType(fact.element)
      return element === undefined ? undefined : Type.fixedArray(element, fact.length.value)
    }
    return undefined
  }
  const registerInput = (fact: DeclarationFacts.DeclaredTypeFact): void => {
    const type = headerType(fact)
    if (type !== undefined) {
      storedHeaders.push(type)
      work.headers += 1
    }
  }
  const register = (facts: ReadonlyArray<DeclarationFacts.TypeParameterFact>): void => {
    for (const parameter of facts) parameters.set(Type.key(parameter.type), parameter)
  }
  for (const module of modules) {
    for (const member of module.members) {
      work.declarations += 1
      if ('typeParameters' in member) register(member.typeParameters)
      if (member._tag === 'FunctionDeclaration') {
        executableHeaders.push(DeclarationFacts.executableLifetimes(member))
        for (const parameter of member.parameters) registerInput(parameter.declaredType)
        for (const entry of member.requirementRow.entries) registerInput(entry.capability)
      }
      if (member._tag === 'StructDeclaration')
        for (const field of member.fields) registerInput(field.declaredType)
      if (member._tag === 'UnionDeclaration')
        for (const variant of member.variants)
          for (const field of variant.fields) registerInput(field.declaredType)
      if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration')
        for (const operation of member.operations) {
          register(operation.typeParameters)
          executableHeaders.push(DeclarationFacts.executableLifetimes(operation))
          for (const parameter of operation.parameters) registerInput(parameter.declaredType)
          for (const entry of operation.requirementRow.entries) registerInput(entry.capability)
        }
    }
    for (const declaration of [...module.conformances, ...module.inherentImpls])
      register(declaration.typeParameters)
    for (const declaration of module.conformances) {
      registerInput(declaration.provider)
      registerInput(declaration.capability)
      for (const requirement of declaration.requirements) registerInput(requirement.capability)
    }
    for (const declaration of module.inherentImpls) registerInput(declaration.owner)
    for (const declaration of [
      ...module.structs,
      ...module.unions,
      ...module.services,
      ...module.interfaces,
    ]) {
      if (declaration.canonical._tag !== 'Canonical') continue
      nominals.set(
        nominalKey(Type.nominal(declaration.canonical.id.module, declaration.canonical.id.name)),
        declaration.typeParameters,
      )
    }
  }
  const bounds: Array<Lifetime.Outlives> = []
  const boundKeys = new Set<string>()
  const parameterBounds = new Map<string, ReadonlyArray<Lifetime.Lifetime>>()
  const add = (argument: Type.GenericArgument, shorter: Lifetime.Lifetime): boolean => {
    if (Lifetime.isLifetime(argument)) {
      const identity = Lifetime.assumptions([{ longer: argument, shorter }]).key
      if (boundKeys.has(identity)) return false
      boundKeys.add(identity)
      bounds.push({ longer: argument, shorter })
      work.lifetimeObligations += 1
      return true
    }
    if (!Type.isTypeArgument(argument)) return false
    let changed = false
    for (const lifetime of Type.storageLifetimes(argument))
      changed = add(lifetime, shorter) || changed
    for (const parameter of Type.storageParameters(argument)) {
      const identity = Type.key(parameter)
      const existing = parameterBounds.get(identity) ?? []
      if (existing.some((bound) => Lifetime.equals(bound, shorter))) continue
      parameterBounds.set(identity, Object.freeze([...existing, shorter]))
      work.typeObligations += 1
      changed = true
    }
    return changed
  }
  for (const parameter of parameters.values())
    for (const shorter of parameter.lifetimeBounds ?? [])
      add(Type.parameterArgument(parameter.type), shorter)
  for (const header of executableHeaders) {
    for (const bound of header.lifetimeBounds ?? []) add(bound.longer, bound.shorter)
    for (const bound of header.typeOutlives ?? []) add(bound.type, bound.lifetime)
  }
  for (const header of storedHeaders)
    for (const type of Type.storageTypes(header)) {
      if (Type.isReference(type)) add(type.target, type.lifetime)
      else if (Type.isSlice(type)) add(type.element, type.lifetime)
    }
  // Source headers and their region identities are finite; propagation adds only missing edges.
  let changed = true
  while (changed) {
    work.iterations += 1
    changed = false
    for (const header of storedHeaders)
      for (const nominal of Type.nominals(header)) {
        const declared = nominals.get(nominalKey(nominal)) ?? []
        const substitution = new Map<string, Type.GenericArgument>()
        for (const [ordinal, parameter] of declared.entries()) {
          const argument = nominal.arguments.at(ordinal)
          if (argument !== undefined) substitution.set(Type.key(parameter.type), argument)
        }
        for (const [ordinal, parameter] of declared.entries()) {
          const argument = nominal.arguments.at(ordinal)
          if (argument === undefined) continue
          const implied =
            parameter.type.kind === 'Lifetime'
              ? bounds
                  .filter((bound) => Lifetime.key(bound.longer) === Type.key(parameter.type))
                  .map((bound) => bound.shorter)
              : (parameterBounds.get(Type.key(parameter.type)) ?? [])
          for (const shorter of implied)
            changed = add(argument, Type.substituteLifetime(shorter, substitution)) || changed
        }
      }
  }
  const result = Object.freeze({
    assumptions: Lifetime.assumptions(bounds),
    parameters,
    parameterBounds,
    nominals,
    work: Object.freeze(work),
  })
  contexts.set(modules, result)
  return result
}

/** Proves structural retained-data validity using explicit bounds for every unknown type. */
export const check = (
  self: Type.Type,
  lifetime: Lifetime.Lifetime,
  scope: Context,
  prove: Prove = (longer, shorter) => Lifetime.outlives(scope.assumptions, longer, shorter),
): boolean => {
  if (!Type.storageLifetimes(self).every((retained) => prove(retained, lifetime))) return false
  return Type.storageParameters(self).every((parameter) => {
    if (parameter.staticProperties.includes('Intrinsic.Detached')) return true
    if (parameter.representationBound !== undefined)
      return prove(parameter.representationBound.environment, lifetime)
    return (scope.parameterBounds.get(Type.key(parameter)) ?? []).some((bound) =>
      prove(bound, lifetime),
    )
  })
}

export interface Failure {
  readonly argument: Type.GenericArgument
  readonly required: Lifetime.Lifetime
  readonly ordinal: number
}

/** Checks one already selected nominal application; this operation performs no lookup search. */
export const application = (
  self: Type.Nominal,
  scope: Context,
  prove: Prove = (longer, shorter) => Lifetime.outlives(scope.assumptions, longer, shorter),
): ReadonlyArray<Failure> => {
  const parameters = scope.nominals.get(nominalKey(self)) ?? []
  const substitution = new Map<string, Type.GenericArgument>()
  for (const [ordinal, parameter] of parameters.entries()) {
    const argument = self.arguments.at(ordinal)
    if (argument !== undefined) substitution.set(Type.key(parameter.type), argument)
  }
  return Object.freeze(
    parameters.flatMap((parameter, ordinal) => {
      const argument = self.arguments.at(ordinal)
      if (argument === undefined) return []
      const requiredBounds =
        parameter.type.kind === 'Lifetime'
          ? scope.assumptions.bounds
              .filter((bound) => Lifetime.key(bound.longer) === Type.key(parameter.type))
              .map((bound) => bound.shorter)
          : (scope.parameterBounds.get(Type.key(parameter.type)) ?? [])
      return requiredBounds.flatMap((bound) => {
        const required = Type.substituteLifetime(bound, substitution)
        let valid = false
        if (Lifetime.isLifetime(argument)) valid = prove(argument, required)
        else if (Type.isTypeArgument(argument)) valid = check(argument, required, scope, prove)
        else if (Type.isRepresentationArgument(argument)) {
          const represented = Type.representedType(argument)
          valid = represented !== undefined && check(represented, required, scope, prove)
        }
        return valid ? [] : [{ argument, required, ordinal }]
      })
    }),
  )
}

/** Validates resolved headers after all parameter-implied assumptions are available. */
export const moduleDiagnostics = (
  self: DeclarationFacts.ModuleHeaders,
  scope: Context,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  const diagnostics = new Map<string, Diagnostic.Diagnostic>()
  const inspect = (fact: DeclarationFacts.DeclaredTypeFact): void => {
    if (fact._tag !== 'Resolved') return
    for (const nominal of Type.nominals(fact.type))
      for (const failure of application(nominal, scope)) {
        const diagnostic = Diagnostic.unsatisfiedLifetimeBound(
          Type.encodeGenericArgument(failure.argument),
          Lifetime.display(failure.required),
          fact.syntax.span,
        )
        diagnostics.set(
          `${Type.key(nominal)}:${failure.ordinal}:${Lifetime.key(failure.required)}:${fact.syntax.span.start}`,
          diagnostic,
        )
      }
  }
  const inspectOperation = (
    operation: DeclarationFacts.DeclarationFact | DeclarationFacts.ServiceOperationFact,
  ): void => {
    for (const parameter of operation.parameters) inspect(parameter.declaredType)
    inspect(operation.returnType)
    for (const member of operation.failureRow.members) inspect(member)
    for (const entry of operation.requirementRow.entries) inspect(entry.capability)
  }
  for (const member of self.members) {
    if (member._tag === 'FunctionDeclaration') inspectOperation(member)
    else if (member._tag === 'ConstantDeclaration') inspect(member.declaredType)
    else if (member._tag === 'StructDeclaration')
      for (const field of member.fields) inspect(field.declaredType)
    else if (member._tag === 'UnionDeclaration')
      for (const variant of member.variants)
        for (const field of variant.fields) inspect(field.declaredType)
    else if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration')
      for (const operation of member.operations) inspectOperation(operation)
  }
  for (const declaration of self.conformances) {
    inspect(declaration.provider)
    inspect(declaration.capability)
    for (const requirement of declaration.requirements) inspect(requirement.capability)
    if (declaration.hook !== undefined) {
      inspect(declaration.hook.parameterType)
      inspect(declaration.hook.returnType)
      for (const member of declaration.hook.failureRow.members) inspect(member)
      for (const entry of declaration.hook.requirementRow.entries) inspect(entry.capability)
    }
  }
  for (const declaration of self.inherentImpls) inspect(declaration.owner)
  return Object.freeze([...diagnostics.values()])
}
