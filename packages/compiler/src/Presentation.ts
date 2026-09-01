import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as Elaboration from './Elaboration.js'
import type * as Intrinsic from './Intrinsic.js'
import * as IntrinsicCatalog from './Intrinsic.js'
import type * as NameResolution from './NameResolution.js'
import * as Operator from './Operator.js'
import * as RequirementRow from './RequirementRow.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as Type from './Type.js'

interface Base {
  readonly text: string
}

export type Presentation =
  | (Base & {
      readonly _tag: 'FunctionPresentation'
      readonly name: string
      readonly functionKind: DeclarationFacts.DeclarationFact['functionKind']
    })
  | (Base & { readonly _tag: 'StructPresentation'; readonly name: string })
  | (Base & { readonly _tag: 'UnionPresentation'; readonly name: string })
  | (Base & { readonly _tag: 'UnionVariantPresentation'; readonly name: string })
  | (Base & { readonly _tag: 'EnumPresentation'; readonly name: string })
  | (Base & { readonly _tag: 'EnumMemberPresentation'; readonly name: string })
  | (Base & { readonly _tag: 'EnumOperationPresentation'; readonly name: string })
  | (Base & { readonly _tag: 'ServicePresentation'; readonly name: string })
  | (Base & { readonly _tag: 'RolePresentation'; readonly name: string })
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

const declaredType = (fact: DeclarationFacts.DeclaredTypeFact): string =>
  fact._tag === 'Unavailable' ? '_' : fact.spelling

const requirementRole = (fact: DeclarationFacts.RequirementRoleFact): string => {
  switch (fact._tag) {
    case 'DefaultRole':
      return ''
    case 'UnresolvedRole':
    case 'ResolvedRole':
      return ` at ${fact.path.spelling}`
  }
}

const rowExpression = (fact: DeclarationFacts.RowExpressionFact): string => {
  switch (fact._tag) {
    case 'EmptyRowExpression':
      return ''
    case 'RowParameterExpression':
      return fact.parameter.name
    case 'FailureMemberExpression':
      return declaredType(fact.member)
    case 'RequirementMemberExpression':
      return `${fact.access === 'Exclusive' ? '&mut ' : '&'}${declaredType(fact.capability)}${requirementRole(fact.role)}`
    case 'UnionRowExpression':
      return fact.operands.map(rowExpression).join(' | ')
    case 'WithoutRowExpression':
      return `Without<${rowExpression(fact.source)}, ${rowExpression(fact.selected)}>`
    case 'UnavailableRowExpression':
      return '_'
  }
}

const failureRow = (fact: DeclarationFacts.FailureRowFact): string => {
  const row = rowExpression(fact.expression)
  return row.length === 0 ? '' : ` ! ${row}`
}

const requirementRow = (fact: DeclarationFacts.RequirementRowFact): string => {
  const row = rowExpression(fact.expression)
  return row.length === 0 ? '' : ` ? ${row}`
}

const constraint = (fact: DeclarationFacts.ConstraintFact): string => {
  if (fact._tag === 'MembershipConstraint') {
    return `${rowExpression(fact.selected)} in ${rowExpression(fact.source)}`
  }
  let prefix = ''
  if (fact.mode === 'Exclusive') prefix = '&mut '
  else if (fact.mode === 'Shared') prefix = '&'
  return `${prefix}${declaredType(fact.provider)} provides ${rowExpression(fact.selected)} from ${rowExpression(fact.source)}`
}

const constraints = (facts: ReadonlyArray<DeclarationFacts.ConstraintFact>): string =>
  facts.length === 0 ? '' : ` where ${facts.map(constraint).join(', ')}`

const typeParameterName = (parameter: DeclarationFacts.TypeParameterFact): string => {
  const name = parameter.name._tag === 'Present' ? parameter.name.spelling : '_'
  return `${parameter.type.kind === 'RequirementRow' ? '?' : ''}${name}`
}

/** Renders a declaration in its source-level callable form. */
export const functionDeclaration = (self: DeclarationFacts.DeclarationFact): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : '_'
  const visibility = self.visibility === 'Public' ? 'pub ' : ''
  const phase = self.phase === 'Static' ? 'static ' : ''
  const kind = `${phase}${self.unsafe ? 'unsafe ' : ''}${self.functionKind === 'Effect' ? 'effect fn' : 'fn'}`
  const typeParameters =
    self.typeParameters.length === 0
      ? ''
      : `<${self.typeParameters.map(typeParameterName).join(', ')}>`
  const parameters = self.parameters
    .map((parameter) => {
      const parameterName = parameter.name._tag === 'Present' ? parameter.name.spelling : '_'
      const phase = parameter.phase === 'Static' ? 'static ' : ''
      const mutability = parameter.bindingMutability === 'Mutable' ? 'mut ' : ''
      return `${phase}${mutability}${parameterName}: ${declaredType(parameter.declaredType)}`
    })
    .join(', ')
  return Object.freeze({
    _tag: 'FunctionPresentation',
    name,
    functionKind: self.functionKind,
    text: `${visibility}${kind} ${name}${typeParameters}(${parameters}) -> ${declaredType(self.returnType)}${failureRow(self.failureRow)}${requirementRow(self.requirementRow)}${constraints(self.constraints)}`,
  })
}

/** Renders a scalar enum declaration without expanding its members. */
export const enumDeclaration = (self: DeclarationFacts.EnumFact): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : '_'
  const visibility = self.visibility === 'Public' ? 'pub ' : ''
  let representation: string
  if (self.representation.explicit) {
    representation = `(${self.representation._tag === 'Available' ? self.representation.scalar.spelling : (self.representation.spelling ?? '_')})`
  } else {
    representation = ''
  }
  return Object.freeze({
    _tag: 'EnumPresentation',
    name,
    text: `${visibility}enum${representation} ${name}`,
  })
}

export const enumMember = (
  enum_: DeclarationFacts.EnumFact,
  member: DeclarationFacts.EnumMemberFact,
): Presentation => {
  const enumName = enum_.name._tag === 'Present' ? enum_.name.spelling : '_'
  const name = member.name._tag === 'Present' ? member.name.spelling : '_'
  return Object.freeze({
    _tag: 'EnumMemberPresentation',
    name,
    text: `${enumName}.${name}: ${enumName}`,
  })
}

export const enumAssociatedOperation = (
  self: DeclarationFacts.EnumAssociatedOperationFact,
): Presentation =>
  Object.freeze({
    _tag: 'EnumOperationPresentation',
    name: self.name,
    text: `fn ${self.name}(value: ${self.parameter.name}) -> ${self.result.spelling}`,
  })

/** Renders a nominal type declaration without expanding its body. */
export const structDeclaration = (self: DeclarationFacts.StructFact): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : '_'
  const visibility = self.visibility === 'Public' ? 'pub ' : ''
  const typeParameters =
    self.typeParameters.length === 0
      ? ''
      : `<${self.typeParameters.map(typeParameterName).join(', ')}>`
  return Object.freeze({
    _tag: 'StructPresentation',
    name,
    text: `${visibility}${self.aggregateKind === 'Positional' ? 'tuple' : 'struct'} ${name}${typeParameters}`,
  })
}

/** Renders a nominal tagged-union declaration without expanding its variants. */
export const unionDeclaration = (self: DeclarationFacts.UnionFact): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : '_'
  const visibility = self.visibility === 'Public' ? 'pub ' : ''
  const typeParameters =
    self.typeParameters.length === 0
      ? ''
      : `<${self.typeParameters.map(typeParameterName).join(', ')}>`
  return Object.freeze({
    _tag: 'UnionPresentation',
    name,
    text: `${visibility}union ${name}${typeParameters}`,
  })
}

/** Renders one variant as a constructor of its complete nominal union parent. */
export const unionVariant = (
  union: DeclarationFacts.UnionFact,
  variant: DeclarationFacts.UnionVariantFact,
): Presentation => {
  const unionName = union.name._tag === 'Present' ? union.name.spelling : '_'
  const variantName = variant.name._tag === 'Present' ? variant.name.spelling : '_'
  const typeParameters =
    union.typeParameters.length === 0
      ? ''
      : `<${union.typeParameters.map(typeParameterName).join(', ')}>`
  const fields =
    variant.kind === 'Unit'
      ? ''
      : ` { ${variant.fields
          .map((field) =>
            field.name._tag === 'Present'
              ? `${field.name.spelling}: ${declaredType(field.declaredType)}`
              : `_: ${declaredType(field.declaredType)}`,
          )
          .join(', ')} }`
  return Object.freeze({
    _tag: 'UnionVariantPresentation',
    name: variantName,
    text: `${unionName}${typeParameters}.${variantName}${fields}: ${unionName}${typeParameters}`,
  })
}

/** Renders one nominal service contract without expanding its operation list. */
export const serviceDeclaration = (
  self: DeclarationFacts.ServiceFact | DeclarationFacts.InterfaceFact,
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

/** Renders one nominal dependency role declaration. */
export const roleDeclaration = (self: DeclarationFacts.RoleFact): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : '_'
  const visibility = self.visibility === 'Public' ? 'pub ' : ''
  return Object.freeze({
    _tag: 'RolePresentation',
    name,
    text: `${visibility}role ${name}`,
  })
}

/** Renders a complete operation contract nested beneath a service. */
export const serviceOperation = (self: DeclarationFacts.ServiceOperationFact): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : '_'
  const kind = `${self.unsafe ? 'unsafe ' : ''}${self.functionKind === 'Effect' ? 'effect fn' : 'fn'}`
  const operator =
    self.operator === undefined ? '' : `operator ${Operator.spelling(self.operator.operator)} `
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
    text: `${operator}${kind} ${name}${typeParameters}(${parameters}) -> ${declaredType(self.returnType)}${failureRow(self.failureRow)}${requirementRow(self.requirementRow)}${constraints(self.constraints)}`,
  })
}

/** Renders one typed compile-time scalar declaration. */
export const constantDeclaration = (self: DeclarationFacts.ConstantFact): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : '_'
  const visibility = self.visibility === 'Public' ? 'pub ' : ''
  return Object.freeze({
    _tag: 'ConstantPresentation',
    name,
    text: `${visibility}const ${name}: ${declaredType(self.declaredType)}`,
  })
}

export const parameter = (self: DeclarationFacts.ParameterFact): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : '_'
  const phase = self.phase === 'Static' ? 'static ' : ''
  const mutability = self.bindingMutability === 'Mutable' ? 'mut ' : ''
  return Object.freeze({
    _tag: 'ParameterPresentation',
    name,
    text: `${phase}${mutability}${name}: ${declaredType(self.declaredType)}`,
  })
}

export const typeParameter = (self: DeclarationFacts.TypeParameterFact): Presentation => {
  const name = self.name._tag === 'Present' ? self.name.spelling : self.type.name
  const kind = self.type.kind === 'RequirementRow' ? 'requirement row' : 'type'
  return Object.freeze({ _tag: 'TypeParameterPresentation', name, text: `${kind} ${name}` })
}

export const field = (self: DeclarationFacts.FieldFact): Presentation => {
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
    const anonymous = Type.anonymousAggregateDisplay(self)
    if (anonymous !== undefined) return anonymous
    let base: string
    if (self.module === module || self.module === 'silk/core') {
      base = self.name
    } else {
      const imported = importedMemberSpelling(self, scope)
      if (imported !== undefined) base = imported
      else {
        const namespace = namespaceSpelling(self.module, scope)
        base = namespace === undefined ? `${self.module}.${self.name}` : `${namespace}.${self.name}`
      }
    }
    return self.arguments.length === 0
      ? base
      : `${base}<${self.arguments.map((argument) => genericArgument(argument, module, scope)).join(', ')}>`
  }
  if (Type.isParameter(self)) return self.name
  if (Type.isFixedArray(self)) return `Array<${type(self.element, module, scope)}, ${self.length}>`
  if (Type.isSlice(self))
    return `${self.access === 'Exclusive' ? '&mut ' : '&'}[${type(self.element, module, scope)}]`
  if (Type.isReference(self))
    return `${self.access === 'Exclusive' ? '&mut ' : '&'}${type(self.target, module, scope)}`
  if (Type.isCallable(self)) {
    let mode: string
    if (self.mode === 'Exclusive') {
      mode = 'mut '
    } else if (self.mode === 'Take') {
      mode = 'once '
    } else {
      mode = ''
    }
    return `${self.unsafe ? 'unsafe ' : ''}${mode}fn(${self.parameters.map((entry) => type(entry, module, scope)).join(', ')}) -> ${type(self.result, module, scope)}`
  }
  if (Type.isEffect(self)) {
    const failureText = RowAlgebra.encode(
      Type.failureRowPolicy(),
      self.failureRow,
      (failure) => type(failure, module, scope),
      (parameter_) => parameter_.name,
      (member) => member.parameter.name,
    )
    const failures = failureText.length === 0 ? '' : ` ! ${failureText}`
    const requirementText = RowAlgebra.encode(
      Type.requirementRowPolicy(),
      self.requirementRow,
      (requirement) =>
        Type.encodeRequirement(requirement, (capability) => type(capability, module, scope)),
      (parameter_) => parameter_.name,
      (member) =>
        `${member.access === 'Exclusive' ? '&mut ' : '&'}${member.capability.name}${member.role === RequirementRow.defaultRole ? '' : ` at ${RequirementRow.roleName(member.role)}`}`,
    )
    const requirements = requirementText.length === 0 ? '' : ` ? ${requirementText}`
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
): string => {
  if (Type.isUnavailableGenericArgument(self)) {
    return Type.encodeGenericArgument(self)
  }
  if (Type.isRepresentationParameterArgument(self)) {
    return self.parameter.name
  }
  if (Type.isOpaqueRepresentationArgument(self)) {
    return Type.encodeGenericArgument(self)
  }
  if (Type.isExactRepresentationArgument(self)) {
    return Type.encodeGenericArgument(self)
  }
  if (Type.isCompositeEffectRepresentationArgument(self)) {
    return Type.encodeGenericArgument(self)
  }
  if (Type.isEffectIdentityArgument(self)) {
    return `effect@${self.identity}`
  }
  if (Type.isCallableIdentityArgument(self)) {
    return `callable@${self.identity}`
  }
  if (Type.isRequirementRowArgument(self)) {
    return `? ${RowAlgebra.encode(
      Type.requirementRowPolicy(),
      self.row,
      (requirement) =>
        Type.encodeRequirement(requirement, (capability) => type(capability, module, scope)),
      (parameter_) => parameter_.name,
      (member) =>
        `${member.access === 'Exclusive' ? '&mut ' : '&'}${member.capability.name}${member.role === RequirementRow.defaultRole ? '' : ` at ${RequirementRow.roleName(member.role)}`}`,
    )}`
  }
  return type(self, module, scope)
}

const scopedNominalBase = (
  self: Type.Nominal,
  module: string,
  scope: NameResolution.ModuleScope | undefined,
): string => {
  const anonymous = Type.anonymousAggregateDisplay(self)
  if (anonymous !== undefined) return anonymous
  if (self.module === module) return self.name
  const imported = importedMemberSpelling(self, scope)
  if (imported !== undefined) return imported
  const namespace = namespaceSpelling(self.module, scope)
  return namespace === undefined ? `${self.module}.${self.name}` : `${namespace}.${self.name}`
}

function scopedGenericArgumentText(
  self: Type.GenericArgument,
  module: string,
  scope: NameResolution.ModuleScope | undefined,
): string {
  if (Type.isUnavailableGenericArgument(self)) return Type.encodeGenericArgument(self)
  if (Type.isRepresentationParameterArgument(self)) return self.parameter.name
  if (
    Type.isOpaqueRepresentationArgument(self) ||
    Type.isExactRepresentationArgument(self) ||
    Type.isCompositeEffectRepresentationArgument(self)
  )
    return Type.encodeGenericArgument(self)
  if (Type.isEffectIdentityArgument(self)) return `effect@${self.identity}`
  if (Type.isCallableIdentityArgument(self)) return `callable@${self.identity}`
  if (Type.isRequirementRowArgument(self))
    return `? ${RowAlgebra.encode(
      Type.requirementRowPolicy(),
      self.row,
      (requirement) =>
        Type.encodeRequirement(requirement, (capability) =>
          scopedTypeText(capability, module, scope),
        ),
      (parameter_) => parameter_.name,
      (member) =>
        `${member.access === 'Exclusive' ? '&mut ' : '&'}${member.capability.name}${member.role === RequirementRow.defaultRole ? '' : ` at ${RequirementRow.roleName(member.role)}`}`,
    )}`
  return scopedTypeText(self, module, scope)
}

function scopedTypeText(
  self: Type.Type,
  module: string,
  scope: NameResolution.ModuleScope | undefined,
): string {
  if (!Type.isNominal(self)) return type(self, module, scope)
  const base = scopedNominalBase(self, module, scope)
  return self.arguments.length === 0
    ? base
    : `${base}<${self.arguments
        .map((argument) => scopedGenericArgumentText(argument, module, scope))
        .join(', ')}>`
}

/** Renders a nominal through source-valid imports without the inferred-type core shortcut. */
export const scopedNominal = (
  self: Type.Nominal,
  module: string,
  scope?: NameResolution.ModuleScope,
): Presentation =>
  Object.freeze({ _tag: 'ExpressionTypePresentation', text: scopedTypeText(self, module, scope) })

/** Renders the selector syntax for one successfully inferred service requirement. */
export const providerSelector = (
  self: Pick<Type.Requirement, 'role'> & { readonly capability: Type.Nominal },
  module: string,
  scope?: NameResolution.ModuleScope,
): Presentation =>
  Object.freeze({
    _tag: 'ExpressionTypePresentation',
    text: `${scopedTypeText(self.capability, module, scope)}${
      self.role === RequirementRow.defaultRole ? '' : ` at ${RequirementRow.roleName(self.role)}`
    }`,
  })

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
