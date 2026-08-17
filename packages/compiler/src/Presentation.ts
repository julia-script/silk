import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Elaboration from './Elaboration.js'
import type * as Intrinsic from './Intrinsic.js'
import * as IntrinsicCatalog from './Intrinsic.js'
import type * as NameResolution from './NameResolution.js'
import * as Type from './Type.js'

interface Base {
  readonly text: string
}

export type Presentation =
  | (Base & {
      readonly _tag: 'FunctionPresentation'
      readonly name: string
      readonly functionKind: DeclarationIndex.DeclarationFact['functionKind']
    })
  | (Base & { readonly _tag: 'StructPresentation'; readonly name: string })
  | (Base & { readonly _tag: 'ServicePresentation'; readonly name: string })
  | (Base & { readonly _tag: 'ServiceOperationPresentation'; readonly name: string })
  | (Base & { readonly _tag: 'ConstantPresentation'; readonly name: string })
  | (Base & { readonly _tag: 'ParameterPresentation'; readonly name: string })
  | (Base & { readonly _tag: 'TypeParameterPresentation'; readonly name: string })
  | (Base & { readonly _tag: 'FieldPresentation'; readonly name: string })
  | (Base & {
      readonly _tag: 'BindingPresentation'
      readonly name: string
      readonly mutability: Elaboration.BindingDeclarationFact['mutability']
    })
  | (Base & { readonly _tag: 'ImportPresentation'; readonly name: string })
  | (Base & { readonly _tag: 'IntrinsicActorPresentation'; readonly name: string })
  | (Base & {
      readonly _tag: 'IntrinsicOperationPresentation'
      readonly actor: string
      readonly name: string
    })
  | (Base & { readonly _tag: 'ExpressionTypePresentation' })

const declaredType = (fact: DeclarationIndex.DeclaredTypeFact): string =>
  fact._tag === 'Unavailable' ? '_' : fact.spelling

const failureRow = (fact: DeclarationIndex.FailureRowFact): string =>
  fact.members.length === 0 && fact.parameters.length === 0
    ? ''
    : ` ! ${[
        ...fact.members.map(declaredType),
        ...fact.parameters.map((parameter) => parameter.name),
      ].join(' | ')}`

const requirementRow = (fact: DeclarationIndex.RequirementRowFact): string =>
  fact.entries.length === 0 && fact.parameters.length === 0
    ? ''
    : ` ? ${[
        ...fact.entries.map(
          (entry) =>
            `${entry.access === 'Exclusive' ? '&mut ' : '&'}${declaredType(entry.capability)}${entry.role === 'DefaultRole' ? '' : `@${entry.role}`}`,
        ),
        ...fact.parameters.map((parameter) => parameter.name),
      ].join(' | ')}`

const typeParameterName = (parameter: DeclarationIndex.TypeParameterFact): string => {
  const name = parameter.name._tag === 'Present' ? parameter.name.spelling : '_'
  return `${parameter.type.kind === 'FailureRow' ? '!' : parameter.type.kind === 'RequirementRow' ? '?' : ''}${name}`
}

/** Renders a declaration in its source-level callable form. */
export const functionDeclaration = (self: DeclarationIndex.DeclarationFact): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : '_'
  const visibility = self.visibility === 'Public' ? 'pub ' : ''
  const kind = self.functionKind === 'Effect' ? 'effect fn' : 'fn'
  const typeParameters =
    self.typeParameters.length === 0
      ? ''
      : `<${self.typeParameters.map(typeParameterName).join(', ')}>`
  const parameters = self.parameters
    .map((parameter) => {
      const parameterName = parameter.name._tag === 'Present' ? parameter.name.spelling : '_'
      return `${parameterName}: ${declaredType(parameter.declaredType)}`
    })
    .join(', ')
  return Object.freeze({
    _tag: 'FunctionPresentation',
    name,
    functionKind: self.functionKind,
    text: `${visibility}${kind} ${name}${typeParameters}(${parameters}) -> ${declaredType(self.returnType)}${failureRow(self.failureRow)}${requirementRow(self.requirementRow)}`,
  })
}

/** Renders a nominal type declaration without expanding its body. */
export const structDeclaration = (self: DeclarationIndex.StructFact): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : '_'
  const visibility = self.visibility === 'Public' ? 'pub ' : ''
  const typeParameters =
    self.typeParameters.length === 0
      ? ''
      : `<${self.typeParameters.map(typeParameterName).join(', ')}>`
  return Object.freeze({
    _tag: 'StructPresentation',
    name,
    text: `${visibility}struct ${name}${typeParameters}`,
  })
}

/** Renders one nominal service contract without expanding its operation list. */
export const serviceDeclaration = (
  self: DeclarationIndex.ServiceFact | DeclarationIndex.InterfaceFact,
): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : '_'
  const visibility = self.visibility === 'Public' ? 'pub ' : ''
  const typeParameters =
    self.typeParameters.length === 0
      ? ''
      : `<${self.typeParameters.map(typeParameterName).join(', ')}>`
  return Object.freeze({
    _tag: 'ServicePresentation',
    name,
    text: `${visibility}${self._tag === 'ServiceDeclaration' ? 'service' : 'interface'} ${name}${typeParameters}`,
  })
}

/** Renders a complete operation contract nested beneath a service. */
export const serviceOperation = (self: DeclarationIndex.ServiceOperationFact): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : '_'
  const kind = self.functionKind === 'Effect' ? 'effect fn' : 'fn'
  const typeParameters =
    self.typeParameters.length === 0
      ? ''
      : `<${self.typeParameters.map(typeParameterName).join(', ')}>`
  const parameters = self.parameters
    .map((parameter) => {
      const parameterName = parameter.name._tag === 'Present' ? parameter.name.spelling : '_'
      return `${parameterName}: ${declaredType(parameter.declaredType)}`
    })
    .join(', ')
  return Object.freeze({
    _tag: 'ServiceOperationPresentation',
    name,
    text: `${kind} ${name}${typeParameters}(${parameters}) -> ${declaredType(self.returnType)}${failureRow(self.failureRow)}${requirementRow(self.requirementRow)}`,
  })
}

/** Renders one typed compile-time scalar declaration. */
export const constantDeclaration = (self: DeclarationIndex.ConstantFact): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : '_'
  const visibility = self.visibility === 'Public' ? 'pub ' : ''
  return Object.freeze({
    _tag: 'ConstantPresentation',
    name,
    text: `${visibility}const ${name}: ${declaredType(self.declaredType)}`,
  })
}

export const parameter = (self: DeclarationIndex.ParameterFact): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : '_'
  return Object.freeze({
    _tag: 'ParameterPresentation',
    name,
    text: `${name}: ${declaredType(self.declaredType)}`,
  })
}

export const typeParameter = (self: DeclarationIndex.TypeParameterFact): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : self.type.name
  const kind =
    self.type.kind === 'FailureRow'
      ? 'failure row'
      : self.type.kind === 'RequirementRow'
        ? 'requirement row'
        : 'type'
  return Object.freeze({ _tag: 'TypeParameterPresentation', name, text: `${kind} ${name}` })
}

export const field = (self: DeclarationIndex.FieldFact): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : '_'
  return Object.freeze({
    _tag: 'FieldPresentation',
    name,
    text: `${self.visibility === 'Public' ? 'pub ' : ''}${name}: ${declaredType(self.declaredType)}`,
  })
}

const importedMemberSpelling = (
  self: Type.Nominal,
  scope: NameResolution.ModuleScope | undefined,
): string | undefined =>
  scope?.bindings.find(
    (binding) =>
      binding._tag === 'ImportedMember' &&
      binding.declaration.module === self.module &&
      binding.declaration.name === self.name &&
      !scope.conflicts.some((conflict) => conflict.spelling === binding.spelling),
  )?.spelling

const namespaceSpelling = (
  module: string,
  scope: NameResolution.ModuleScope | undefined,
): string | undefined =>
  scope?.bindings.find(
    (binding) =>
      binding._tag === 'ModuleNamespace' &&
      binding.module === module &&
      !scope.conflicts.some((conflict) => conflict.spelling === binding.spelling),
  )?.spelling

/** Renders an inferred type with the shortest unambiguous spelling available in one module. */
export const type = (
  self: Type.Type,
  module: string,
  scope?: NameResolution.ModuleScope,
): string => {
  if (Type.equals(self, Type.unit)) return '()'
  if (typeof self === 'string') return self
  if (Type.isNominal(self)) {
    const base =
      self.module === module || self.module === 'silk/core'
        ? self.name
        : (importedMemberSpelling(self, scope) ??
          (() => {
            const namespace = namespaceSpelling(self.module, scope)
            return namespace === undefined
              ? `${self.module}.${self.name}`
              : `${namespace}.${self.name}`
          })())
    return self.arguments.length === 0
      ? base
      : `${base}<${self.arguments.map((argument) => genericArgument(argument, module, scope)).join(', ')}>`
  }
  if (Type.isParameter(self)) return self.name
  if (Type.isFailureProjection(self)) return `Row<! ${self.parameter.name}>`
  if (Type.isFixedArray(self)) return `Array<${type(self.element, module, scope)}, ${self.length}>`
  if (Type.isSlice(self))
    return `${self.access === 'Exclusive' ? '&mut ' : '&'}[${type(self.element, module, scope)}]`
  if (Type.isReference(self))
    return `${self.access === 'Exclusive' ? '&mut ' : '&'}${type(self.target, module, scope)}`
  if (Type.isCallable(self)) {
    const mode = self.mode === 'Exclusive' ? 'mut ' : self.mode === 'Take' ? 'once ' : ''
    return `${mode}fn(${self.parameters.map((entry) => type(entry, module, scope)).join(', ')}) -> ${type(self.result, module, scope)}`
  }
  if (Type.isEffect(self)) {
    const failureMembers = [
      ...Type.failureMembers(self).map((failure) => type(failure, module, scope)),
      ...Type.failureRowParameters(self).map((parameter_) => parameter_.name),
    ]
    const failures = failureMembers.length === 0 ? '' : ` ! ${failureMembers.join(' | ')}`
    const requirementMembers = [
      ...Type.requirementMembers(self).map(
        (requirement) =>
          `${requirement.access === 'Exclusive' ? '&mut ' : '&'}${type(requirement.capability, module, scope)}${requirement.role === 'DefaultRole' ? '' : `@${requirement.role}`}`,
      ),
      ...Type.requirementRowParameters(self).map((parameter_) => parameter_.name),
    ]
    const requirements =
      requirementMembers.length === 0 ? '' : ` ? ${requirementMembers.join(' | ')}`
    return `Effect<${type(self.success, module, scope)}${failures}${requirements}>`
  }
  if (Type.isRepresented(self)) return type(self.contract, module, scope)
  return self.members.map((member) => type(member, module, scope)).join(' | ')
}

/** Renders one erased generic argument for tooling without exposing a runtime descriptor. */
export const genericArgument = (
  self: Type.GenericArgument,
  module: string,
  scope?: NameResolution.ModuleScope,
): string =>
  Type.isUnavailableGenericArgument(self)
    ? Type.encodeGenericArgument(self)
    : Type.isRepresentationParameterArgument(self)
      ? self.parameter.name
      : Type.isOpaqueRepresentationArgument(self)
        ? Type.encodeGenericArgument(self)
        : Type.isExactRepresentationArgument(self)
          ? Type.encodeGenericArgument(self)
          : Type.isEffectIdentityArgument(self)
            ? `effect@${self.identity}`
            : Type.isCallableIdentityArgument(self)
              ? `callable@${self.identity}`
              : Type.isFailureRowArgument(self)
                ? `! ${
                    Type.failureMembers(self)
                      .map((failure) => type(failure, module, scope))
                      .join(' | ') || 'never'
                  }`
                : Type.isRequirementRowArgument(self)
                  ? `? ${Type.requirementMembers(self)
                      .map(
                        (requirement) =>
                          `${requirement.access === 'Exclusive' ? '&mut ' : '&'}${type(requirement.capability, module, scope)}${requirement.role === 'DefaultRole' ? '' : `@${requirement.role}`}`,
                      )
                      .join(' | ')}`
                  : type(self, module, scope)

export const binding = (
  self: Elaboration.BindingDeclarationFact,
  module: string,
  scope?: NameResolution.ModuleScope,
): Presentation | undefined => {
  if (self.name._tag !== 'Present' || self.inferredType._tag !== 'Available') return undefined
  return Object.freeze({
    _tag: 'BindingPresentation',
    name: self.name.spelling,
    mutability: self.mutability,
    text: `let ${self.mutability === 'Mutable' ? 'mut ' : ''}${self.name.spelling}: ${type(self.inferredType.type, module, scope)}`,
  })
}

export const patternBinding = (
  self: Elaboration.PatternBindingFact,
  module: string,
  scope?: NameResolution.ModuleScope,
): Presentation | undefined => {
  if (self.name._tag !== 'Present' || self.type._tag !== 'Available') return undefined
  return Object.freeze({
    _tag: 'BindingPresentation',
    name: self.name.spelling,
    mutability: 'Immutable',
    text: `let ${self.name.spelling}: ${type(self.type.type, module, scope)}`,
  })
}

export const importBinding = (name: string, target: string): Presentation =>
  Object.freeze({ _tag: 'ImportPresentation', name, text: `import ${target} as ${name}` })

export const intrinsicActor = (self: Intrinsic.Actor): Presentation =>
  Object.freeze({
    _tag: 'IntrinsicActorPresentation',
    name: self.spelling,
    text: `${self.kind === 'Type' ? 'intrinsic type' : 'intrinsic namespace'} ${self.spelling}`,
  })

export const intrinsicOperation = (self: Intrinsic.Operation): Presentation =>
  Object.freeze({
    _tag: 'IntrinsicOperationPresentation',
    actor: self.id.actor,
    name: self.spelling,
    text: IntrinsicCatalog.signature(self),
  })

export const expressionType = (
  self: Type.Type,
  module: string,
  scope?: NameResolution.ModuleScope,
): Presentation =>
  Object.freeze({ _tag: 'ExpressionTypePresentation', text: type(self, module, scope) })
