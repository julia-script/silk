import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Diagnostic from './Diagnostic.js'
import type * as Elaboration from './Elaboration.js'
import type * as Hir from './Hir.js'
import * as Intrinsic from './Intrinsic.js'
import type * as Match from './Match.js'
import * as NameResolution from './NameResolution.js'
import type * as SourceSpan from './SourceSpan.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'

/** The exact source location and name selection of one source-backed declaration. */
export interface DeclarationLocation {
  readonly module: string
  readonly span: SourceSpan.SourceSpan
  readonly selectionSpan: SourceSpan.SourceSpan
}

/** Compiler-owned identity selected by one semantic token occurrence. */
export type Identity =
  | {
      readonly _tag: 'DeclarationIdentity'
      readonly id: DeclarationFacts.CanonicalId | DeclarationFacts.DeclarationId
    }
  | { readonly _tag: 'TypeParameterIdentity'; readonly id: Type.Parameter }
  | { readonly _tag: 'ParameterIdentity'; readonly id: DeclarationFacts.ParameterId }
  | { readonly _tag: 'BindingIdentity'; readonly id: Hir.BindingId }
  | { readonly _tag: 'PatternBindingIdentity'; readonly id: Match.BindingId }
  | { readonly _tag: 'FieldIdentity'; readonly id: DeclarationFacts.FieldId }
  | {
      readonly _tag: 'UnionVariantIdentity'
      readonly id: DeclarationFacts.CanonicalUnionVariantId
    }
  | { readonly _tag: 'EnumMemberIdentity'; readonly id: DeclarationFacts.CanonicalEnumMemberId }
  | {
      readonly _tag: 'EnumAssociatedOperationIdentity'
      readonly id: DeclarationFacts.EnumAssociatedOperationId
    }
  | {
      readonly _tag: 'ServiceOperationIdentity'
      readonly id: DeclarationFacts.ServiceOperationId
    }
  | { readonly _tag: 'ImportNamespaceIdentity'; readonly module: string; readonly spelling: string }
  | { readonly _tag: 'IntrinsicActorIdentity'; readonly id: Intrinsic.ActorId }
  | { readonly _tag: 'IntrinsicOperationIdentity'; readonly id: Intrinsic.OperationId }

export type Role = 'Declaration' | 'Value' | 'Type' | 'Field' | 'Actor' | 'Operation' | 'Import'

export type Resolution =
  | { readonly _tag: 'Available'; readonly identity: Identity }
  | { readonly _tag: 'Missing'; readonly cause?: Diagnostic.Identity }
  | { readonly _tag: 'Inaccessible'; readonly cause?: Diagnostic.Identity }
  | { readonly _tag: 'Ambiguous'; readonly cause?: Diagnostic.Identity }
  | { readonly _tag: 'Conflicting'; readonly cause?: Diagnostic.Identity }
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity }

/** One immutable exact-token semantic query answer. */
export interface SemanticOccurrence {
  readonly _tag: 'SemanticOccurrence'
  readonly span: SourceSpan.SourceSpan
  readonly role: Role
  readonly resolution: Resolution
  readonly declaration?: DeclarationLocation
  readonly ordinal: number
}

/** Start-sorted occurrences and their compact prefix maximum-end lookup index. */
export interface ModuleIndex {
  readonly occurrences: ReadonlyArray<SemanticOccurrence>
  readonly prefixMaximumEnd: ReadonlyArray<number>
  readonly declarationLocations: ReadonlyMap<string, DeclarationLocation>
}

/** Deterministic semantic occurrences grouped by canonical source module. */
export interface Index {
  readonly _tag: 'SemanticOccurrenceIndex'
  readonly modules: ReadonlyMap<string, ModuleIndex>
  readonly declarationLocations: ReadonlyMap<string, DeclarationLocation>
}

const identityOfDeclaration = (declaration: DeclarationFacts.MemberFact): Identity =>
  Object.freeze({
    _tag: 'DeclarationIdentity',
    id: declaration.canonical._tag === 'Canonical' ? declaration.canonical.id : declaration.id,
  })

const location = (
  module: string,
  span: SourceSpan.SourceSpan,
  selectionSpan: SourceSpan.SourceSpan,
): DeclarationLocation => Object.freeze({ module, span, selectionSpan })

const currentDeclaration = (
  index: DeclarationIndex.Index,
  declaration: DeclarationFacts.MemberFact,
): DeclarationFacts.MemberFact =>
  declaration.canonical._tag === 'Canonical'
    ? (DeclarationFacts.byCanonical(index, declaration.canonical.id) ?? declaration)
    : (index.modules
        .find((module) => module.module === declaration.id.sourceId)
        ?.members.find(
          (candidate) =>
            candidate.id.sourceId === declaration.id.sourceId &&
            candidate.id.ordinal === declaration.id.ordinal,
        ) ?? declaration)

const locationOfDeclaration = (
  index: DeclarationIndex.Index,
  declaration: DeclarationFacts.MemberFact,
): DeclarationLocation | undefined => {
  const current = currentDeclaration(index, declaration)
  return current.name._tag === 'Present'
    ? location(current.name.token.span.sourceId, current.syntax.span, current.name.token.span)
    : undefined
}

const locationOfParameter = (
  parameter: DeclarationFacts.ParameterFact,
): DeclarationLocation | undefined =>
  parameter.name._tag === 'Present'
    ? location(parameter.name.token.span.sourceId, parameter.syntax.span, parameter.name.token.span)
    : undefined

const locationOfServiceOperation = (
  operation: DeclarationFacts.ServiceOperationFact,
): DeclarationLocation | undefined =>
  operation.name._tag === 'Present'
    ? location(operation.name.token.span.sourceId, operation.syntax.span, operation.name.token.span)
    : undefined

const locationOfTypeParameter = (
  parameter: DeclarationFacts.TypeParameterFact,
): DeclarationLocation | undefined =>
  parameter.name._tag === 'Present'
    ? location(parameter.name.token.span.sourceId, parameter.syntax.span, parameter.name.token.span)
    : undefined

const locationOfBinding = (
  binding: Elaboration.BindingDeclarationFact | Elaboration.PatternBindingFact,
): DeclarationLocation | undefined =>
  binding.name._tag === 'Present'
    ? location(binding.name.token.span.sourceId, binding.syntax.span, binding.name.token.span)
    : undefined

const locationOfEnumMember = (
  member: DeclarationFacts.EnumMemberFact,
): DeclarationLocation | undefined =>
  member.name._tag === 'Present'
    ? location(member.name.token.span.sourceId, member.syntax.span, member.name.token.span)
    : undefined

const locationOfUnionVariant = (
  variant: DeclarationFacts.UnionVariantFact,
): DeclarationLocation | undefined =>
  variant.name._tag === 'Present'
    ? location(variant.name.token.span.sourceId, variant.syntax.span, variant.name.token.span)
    : undefined

const locationOfField = (
  index: DeclarationIndex.Index,
  field: DeclarationFacts.FieldFact,
): DeclarationLocation | undefined => {
  const declarationId = DeclarationFacts.fieldDeclaration(field.id)
  const module = index.modules.find((candidate) => candidate.module === declarationId.sourceId)
  const owner = field.id.owner
  const current =
    (owner._tag === 'StructFieldOwnerId'
      ? module?.structs
          .find((struct) => struct.id.ordinal === declarationId.ordinal)
          ?.fields.find((candidate) => DeclarationFacts.sameFieldId(candidate.id, field.id))
      : module?.unions
          .find((union) => union.id.ordinal === declarationId.ordinal)
          ?.variants.find((variant) => variant.id.ordinal === owner.variant.ordinal)
          ?.fields.find((candidate) => DeclarationFacts.sameFieldId(candidate.id, field.id))) ??
    field
  return current.name._tag === 'Present'
    ? location(current.name.token.span.sourceId, current.syntax.span, current.name.token.span)
    : undefined
}

const available = (identity: Identity): Resolution => Object.freeze({ _tag: 'Available', identity })

interface Pending {
  readonly occurrence: Omit<SemanticOccurrence, 'ordinal'>
  readonly ordinal: number
}

const push = (
  pending: Array<Pending>,
  span: SourceSpan.SourceSpan | undefined,
  role: Role,
  resolution: Resolution,
  declaration?: DeclarationLocation,
): void => {
  if (span === undefined || span.start === span.end) return
  pending.push(
    Object.freeze({
      occurrence: Object.freeze({
        _tag: 'SemanticOccurrence',
        span,
        role,
        resolution,
        ...(declaration === undefined ? {} : { declaration }),
      }),
      ordinal: pending.length,
    }),
  )
}

const isNominalDeclaration = (
  declaration: DeclarationFacts.MemberFact,
): declaration is
  | DeclarationFacts.StructFact
  | DeclarationFacts.EnumFact
  | DeclarationFacts.ServiceFact
  | DeclarationFacts.InterfaceFact =>
  declaration._tag === 'StructDeclaration' ||
  declaration._tag === 'EnumDeclaration' ||
  declaration._tag === 'ServiceDeclaration' ||
  declaration._tag === 'InterfaceDeclaration'

const declarationByNominal = (
  index: DeclarationIndex.Index,
  nominal: Type.Nominal,
):
  | DeclarationFacts.StructFact
  | DeclarationFacts.EnumFact
  | DeclarationFacts.ServiceFact
  | DeclarationFacts.InterfaceFact
  | undefined =>
  index.modules
    .find((module) => module.module === nominal.module)
    ?.members.find(
      (
        declaration,
      ): declaration is
        | DeclarationFacts.StructFact
        | DeclarationFacts.EnumFact
        | DeclarationFacts.ServiceFact
        | DeclarationFacts.InterfaceFact =>
        isNominalDeclaration(declaration) &&
        declaration.canonical._tag === 'Canonical' &&
        declaration.canonical.id.name === nominal.name,
    )

const typeParameterFact = (
  index: DeclarationIndex.Index,
  type: Type.Parameter,
): DeclarationFacts.TypeParameterFact | undefined => {
  for (const module of index.modules)
    for (const member of module.members) {
      const parameter = member.typeParameters.find((candidate) => Type.equals(candidate.type, type))
      if (parameter !== undefined) return parameter
      if (
        (member._tag === 'FunctionDeclaration' &&
          member.opaqueResult !== undefined &&
          Type.equals(member.opaqueResult.binder.type, type)) ||
        ((member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration') &&
          member.operations.some(
            (operation) =>
              operation.opaqueResult !== undefined &&
              Type.equals(operation.opaqueResult.binder.type, type),
          ))
      ) {
        if (member._tag === 'FunctionDeclaration') return member.opaqueResult?.binder
        return member.operations.find(
          (operation) =>
            operation.opaqueResult !== undefined &&
            Type.equals(operation.opaqueResult.binder.type, type),
        )?.opaqueResult?.binder
      }
    }
  return undefined
}

const collectQualifier = (
  token: Token.Token,
  spelling: string,
  scope: NameResolution.ModuleScope | undefined,
  index: DeclarationIndex.Index,
  pending: Array<Pending>,
): void => {
  if (scope === undefined) {
    push(pending, token.span, 'Actor', Object.freeze({ _tag: 'Unavailable' }))
    return
  }
  const lookup = NameResolution.lookup(scope, index, spelling)
  if (lookup._tag === 'Resolved') {
    push(
      pending,
      token.span,
      'Actor',
      available(identityOfDeclaration(lookup.declaration)),
      locationOfDeclaration(index, lookup.declaration),
    )
    return
  }
  if (lookup._tag === 'Intrinsic') {
    const actor = Intrinsic.findActor(lookup.actor)
    push(
      pending,
      token.span,
      'Actor',
      actor === undefined
        ? Object.freeze({ _tag: 'Unavailable' })
        : available(Object.freeze({ _tag: 'IntrinsicActorIdentity', id: actor.id })),
    )
    return
  }
  if (lookup._tag === 'Namespace') {
    const binding = scope.bindings.find(
      (candidate) => candidate._tag === 'ModuleNamespace' && candidate.spelling === lookup.spelling,
    )
    const declaration =
      binding?._tag === 'ModuleNamespace'
        ? location(binding.token.span.sourceId, binding.syntax.span, binding.token.span)
        : undefined
    push(
      pending,
      token.span,
      'Actor',
      available(
        Object.freeze({
          _tag: 'ImportNamespaceIdentity',
          module: lookup.module,
          spelling: lookup.spelling,
        }),
      ),
      declaration,
    )
    return
  }
  if (lookup._tag === 'Conflict') {
    push(
      pending,
      token.span,
      'Actor',
      Object.freeze({ _tag: 'Conflicting', cause: lookup.conflict.cause }),
    )
    return
  }
  push(pending, token.span, 'Actor', Object.freeze({ _tag: 'Unavailable' }))
}

const collectResolvedType = (
  fact: Extract<DeclarationFacts.DeclaredTypeFact, { readonly _tag: 'Resolved' }>,
  index: DeclarationIndex.Index,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  if (fact.exactItem !== undefined) {
    const qualifier =
      fact.exactItem.path.segments.length > 1 ? fact.exactItem.path.segments.at(0) : undefined
    if (qualifier !== undefined)
      collectQualifier(qualifier.token, qualifier.spelling, scope, index, pending)
    const selected = fact.exactItem.path.segments.at(-1)
    const declaration = DeclarationFacts.byCanonical(index, fact.exactItem.declaration)
    push(
      pending,
      selected?.token.span,
      'Value',
      declaration === undefined
        ? Object.freeze({ _tag: 'Unavailable' })
        : available(identityOfDeclaration(declaration)),
      declaration === undefined ? undefined : locationOfDeclaration(index, declaration),
    )
  }
  if (fact.components !== undefined) {
    for (const component of fact.components) collectDeclaredType(component, index, scope, pending)
    return
  }
  if (fact.unionSource !== undefined) {
    for (const member of fact.unionSource.members)
      collectDeclaredType(member, index, scope, pending)
    return
  }
  const tokens = fact.path?.segments.map((segment) => segment.token) ?? Object.freeze([fact.token])
  const token = tokens.at(-1) ?? fact.token
  const qualifier = tokens.length > 1 ? tokens.at(0) : undefined
  if (qualifier !== undefined) {
    collectQualifier(
      qualifier,
      fact.spelling.split('.').at(0) ?? fact.spelling,
      scope,
      index,
      pending,
    )
  }
  if (
    Type.isRepresented(fact.type) &&
    Type.isRepresentationParameterArgument(fact.type.representation.argument)
  ) {
    const parameter = fact.type.representation.argument.parameter
    const declaration = typeParameterFact(index, parameter)
    push(
      pending,
      token.span,
      'Type',
      available(Object.freeze({ _tag: 'TypeParameterIdentity', id: parameter })),
      declaration === undefined ? undefined : locationOfTypeParameter(declaration),
    )
    return
  }
  if (Type.isParameter(fact.type)) {
    const declaration = typeParameterFact(index, fact.type)
    push(
      pending,
      token.span,
      'Type',
      available(Object.freeze({ _tag: 'TypeParameterIdentity', id: fact.type })),
      declaration === undefined ? undefined : locationOfTypeParameter(declaration),
    )
    return
  }
  if (Type.isNominal(fact.type)) {
    const declaration = declarationByNominal(index, fact.type)
    if (declaration !== undefined) {
      push(
        pending,
        token.span,
        'Type',
        available(identityOfDeclaration(declaration)),
        locationOfDeclaration(index, declaration),
      )
      return
    }
    const actor = Intrinsic.findActor(fact.type.name)
    push(
      pending,
      token.span,
      'Type',
      actor === undefined
        ? Object.freeze({ _tag: 'Unavailable' })
        : available(Object.freeze({ _tag: 'IntrinsicActorIdentity', id: actor.id })),
    )
    return
  }
  if (typeof fact.type === 'string' && fact.type !== 'never') {
    const actor = Intrinsic.findActor(fact.type)
    push(
      pending,
      token.span,
      'Type',
      actor === undefined
        ? Object.freeze({ _tag: 'Unavailable' })
        : available(Object.freeze({ _tag: 'IntrinsicActorIdentity', id: actor.id })),
    )
  }
}

const collectDeclaredType = (
  fact: DeclarationFacts.DeclaredTypeFact,
  index: DeclarationIndex.Index,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  if (fact._tag === 'Resolved') {
    collectResolvedType(fact, index, scope, pending)
    return
  }
  if (fact._tag === 'Unresolved') {
    const qualifier = fact.path.segments.length > 1 ? fact.path.segments.at(0) : undefined
    if (qualifier !== undefined)
      collectQualifier(qualifier.token, qualifier.spelling, scope, index, pending)
    const selected = fact.path.segments.at(-1)
    let declarationLocation: DeclarationLocation | undefined
    if (fact.candidate !== undefined) {
      const declaration = declarationByNominal(index, fact.candidate)
      if (declaration !== undefined) declarationLocation = locationOfDeclaration(index, declaration)
    }
    push(
      pending,
      selected?.token.span,
      'Type',
      Object.freeze({
        _tag: fact.candidate === undefined ? 'Missing' : 'Inaccessible',
        ...(fact.cause === undefined ? {} : { cause: fact.cause }),
      }),
      declarationLocation,
    )
    return
  }
  if (fact._tag === 'FixedArray' || fact._tag === 'Slice') {
    collectDeclaredType(fact.element, index, scope, pending)
    return
  }
  if (fact._tag === 'Reference') {
    collectDeclaredType(fact.target, index, scope, pending)
    return
  }
  if (fact._tag === 'Callable') {
    for (const parameter of fact.parameters) collectDeclaredType(parameter, index, scope, pending)
    collectDeclaredType(fact.result, index, scope, pending)
    return
  }
  if (fact._tag === 'Applied') {
    collectDeclaredType(fact.target, index, scope, pending)
    for (const argument of fact.arguments) collectDeclaredType(argument, index, scope, pending)
    return
  }
  if (fact._tag === 'Effect') {
    collectDeclaredType(fact.success, index, scope, pending)
    for (const failure of fact.failures) collectDeclaredType(failure, index, scope, pending)
    for (const requirement of fact.requirements)
      collectDeclaredType(requirement.capability, index, scope, pending)
    return
  }
  if (fact._tag === 'ExactRepresentation') {
    const qualifier = fact.item.segments.length > 1 ? fact.item.segments.at(0) : undefined
    if (qualifier !== undefined)
      collectQualifier(qualifier.token, qualifier.spelling, scope, index, pending)
    const selected = fact.item.segments.at(-1)
    const declaration =
      fact.itemCandidate === undefined
        ? undefined
        : DeclarationFacts.byCanonical(index, fact.itemCandidate)
    push(
      pending,
      selected?.token.span,
      'Value',
      Object.freeze({
        _tag: 'Unavailable',
        ...(fact.cause === undefined ? {} : { cause: fact.cause }),
      }),
      declaration === undefined ? undefined : locationOfDeclaration(index, declaration),
    )
    for (const argument of fact.arguments) collectDeclaredType(argument, index, scope, pending)
    return
  }
  if (fact._tag === 'RepresentationParameter') {
    const declaration = typeParameterFact(index, fact.parameter)
    push(
      pending,
      fact.token.span,
      'Type',
      available(Object.freeze({ _tag: 'TypeParameterIdentity', id: fact.parameter })),
      declaration === undefined ? undefined : locationOfTypeParameter(declaration),
    )
    return
  }
  if (fact._tag === 'Union')
    for (const member of fact.members) collectDeclaredType(member, index, scope, pending)
}

const collectRowExpression = (
  fact: DeclarationFacts.RowExpressionFact,
  index: DeclarationIndex.Index,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  switch (fact._tag) {
    case 'EmptyRowExpression':
    case 'UnavailableRowExpression':
      return
    case 'RowParameterExpression': {
      const declaration = typeParameterFact(index, fact.parameter)
      const token = SyntaxTree.tokens(fact.syntax).find(
        (candidate) => candidate.kind === 'Identifier',
      )
      push(
        pending,
        token?.span,
        'Type',
        available(Object.freeze({ _tag: 'TypeParameterIdentity', id: fact.parameter })),
        declaration === undefined ? undefined : locationOfTypeParameter(declaration),
      )
      return
    }
    case 'FailureMemberExpression':
      collectDeclaredType(fact.member, index, scope, pending)
      return
    case 'RequirementMemberExpression':
      collectDeclaredType(fact.capability, index, scope, pending)
      return
    case 'UnionRowExpression':
      for (const operand of fact.operands) collectRowExpression(operand, index, scope, pending)
      return
    case 'WithoutRowExpression':
      collectRowExpression(fact.source, index, scope, pending)
      collectRowExpression(fact.selected, index, scope, pending)
      return
  }
}

const collectConstraint = (
  fact: DeclarationFacts.ConstraintFact,
  index: DeclarationIndex.Index,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  if (fact._tag === 'ProviderConstraint') collectDeclaredType(fact.provider, index, scope, pending)
  collectRowExpression(fact.selected, index, scope, pending)
  collectRowExpression(fact.source, index, scope, pending)
}

const parameterResolution = (
  reference: Elaboration.ParameterReferenceFact,
): { readonly resolution: Resolution; readonly declaration?: DeclarationLocation } => {
  if (reference._tag === 'Resolved') {
    const declaration = locationOfParameter(reference.parameter)
    return Object.freeze({
      resolution: available(
        Object.freeze({ _tag: 'ParameterIdentity', id: reference.parameter.id }),
      ),
      ...(declaration === undefined ? {} : { declaration }),
    })
  }
  if (reference._tag === 'ResolvedBinding') {
    const declaration = locationOfBinding(reference.binding)
    return Object.freeze({
      resolution: available(Object.freeze({ _tag: 'BindingIdentity', id: reference.binding.id })),
      ...(declaration === undefined ? {} : { declaration }),
    })
  }
  if (reference._tag === 'ResolvedPattern') {
    const declaration = locationOfBinding(reference.binding)
    return Object.freeze({
      resolution: available(
        Object.freeze({ _tag: 'PatternBindingIdentity', id: reference.binding.id }),
      ),
      ...(declaration === undefined ? {} : { declaration }),
    })
  }
  if (reference._tag === 'Missing')
    return Object.freeze({
      resolution: Object.freeze({
        _tag: 'Missing',
        ...(reference.cause === undefined ? {} : { cause: reference.cause }),
      }),
    })
  return Object.freeze({
    resolution: Object.freeze({
      _tag: reference._tag === 'Ambiguous' ? 'Ambiguous' : 'Unavailable',
    }),
  })
}

const callResolution = (
  reference: Elaboration.CallReferenceFact,
  index: DeclarationIndex.Index,
): { readonly resolution: Resolution; readonly declaration?: DeclarationLocation } => {
  if (reference._tag === 'Resolved') {
    const declaration = locationOfDeclaration(index, reference.declaration)
    return Object.freeze({
      resolution: available(identityOfDeclaration(reference.declaration)),
      ...(declaration === undefined ? {} : { declaration }),
    })
  }
  if (reference._tag === 'ResolvedBuiltin') {
    const operation = Intrinsic.findOperation(
      reference.actor,
      reference.spelling.split('.').at(-1) ?? reference.spelling,
    )
    return Object.freeze({
      resolution:
        operation === undefined
          ? Object.freeze({ _tag: 'Unavailable' })
          : available(Object.freeze({ _tag: 'IntrinsicOperationIdentity', id: operation.id })),
    })
  }
  if (reference._tag === 'ResolvedBoundOperation') {
    // A bound operation is declared once, by the interface, and answered per specialization by a
    // witness. The declaration is what a reader navigates to, so that is what the occurrence names.
    const declaration = locationOfServiceOperation(reference.declaration)
    return Object.freeze({
      resolution:
        reference.declaration.state._tag === 'Unique'
          ? available(
              Object.freeze({
                _tag: 'ServiceOperationIdentity',
                id: reference.declaration.state.id,
              }),
            )
          : Object.freeze({ _tag: 'Unavailable' }),
      ...(declaration === undefined ? {} : { declaration }),
    })
  }
  if (reference._tag === 'ResolvedServiceOperation') {
    const declaration = locationOfServiceOperation(reference.operation)
    return Object.freeze({
      resolution:
        reference.operation.state._tag === 'Unique'
          ? available(
              Object.freeze({
                _tag: 'ServiceOperationIdentity',
                id: reference.operation.state.id,
              }),
            )
          : Object.freeze({ _tag: 'Unavailable' }),
      ...(declaration === undefined ? {} : { declaration }),
    })
  }
  if (reference._tag === 'Missing')
    return Object.freeze({
      resolution: Object.freeze({
        _tag: 'Missing',
        ...(reference.cause === undefined ? {} : { cause: reference.cause }),
      }),
    })
  if (reference._tag === 'Ambiguous')
    return Object.freeze({
      resolution: Object.freeze({
        _tag: 'Ambiguous',
        ...(reference.cause === undefined ? {} : { cause: reference.cause }),
      }),
    })
  return Object.freeze({ resolution: Object.freeze({ _tag: 'Unavailable' }) })
}

const collectCallReference = (
  reference: Elaboration.CallReferenceFact,
  path: Elaboration.ReferencePathFact | undefined,
  index: DeclarationIndex.Index,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  let tokens: ReadonlyArray<Token.Token>
  if (path?._tag === 'ReferencePath') {
    tokens = Object.freeze([...(path.qualifier === undefined ? [] : [path.qualifier]), path.member])
  } else if ('token' in reference) {
    tokens = Object.freeze([reference.token])
  } else {
    tokens = Object.freeze([])
  }
  const qualifier = tokens.length > 1 ? tokens.at(0) : undefined
  if (qualifier !== undefined) {
    const qualifierName = 'spelling' in reference ? reference.spelling.split('.').at(0) : undefined
    if (qualifierName !== undefined)
      collectQualifier(qualifier, qualifierName, scope, index, pending)
  }
  const selected = 'token' in reference ? reference.token : tokens.at(-1)
  const resolved = callResolution(reference, index)
  push(
    pending,
    selected?.span,
    reference._tag === 'ResolvedBuiltin' ||
      reference._tag === 'ResolvedServiceOperation' ||
      reference._tag === 'ResolvedBoundOperation'
      ? 'Operation'
      : 'Value',
    resolved.resolution,
    resolved.declaration,
  )
}

const collectIntrinsicReference = (
  reference: Elaboration.IntrinsicReferenceFact,
  index: DeclarationIndex.Index,
  pending: Array<Pending>,
): void => {
  if (reference._tag === 'UnavailableIntrinsicReference') return
  const actorResolution =
    reference.actor._tag === 'IntrinsicActor'
      ? available(Object.freeze({ _tag: 'IntrinsicActorIdentity', id: reference.actor.id }))
      : available(identityOfDeclaration(reference.actor))
  const actorDeclaration =
    reference.actor._tag === 'IntrinsicActor'
      ? undefined
      : locationOfDeclaration(index, reference.actor)
  push(pending, reference.actorToken.span, 'Actor', actorResolution, actorDeclaration)
  push(
    pending,
    reference.operationToken.span,
    'Operation',
    available(Object.freeze({ _tag: 'IntrinsicOperationIdentity', id: reference.operation.id })),
  )
}

const collectPattern = (
  pattern: Elaboration.PatternFact,
  index: DeclarationIndex.Index,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  // A shorthand field pattern uses one token for both the selected field and the new local.
  // Prefer the declaration occurrence for exact-position queries while retaining the field
  // occurrence in range queries.
  for (const binding of pattern.bindings)
    if (binding.name._tag === 'Present')
      push(
        pending,
        binding.name.token.span,
        'Declaration',
        available(Object.freeze({ _tag: 'PatternBindingIdentity', id: binding.id })),
        locationOfBinding(binding),
      )
  if (pattern._tag === 'EnumMemberPattern') {
    if (pattern.enum !== undefined)
      push(
        pending,
        pattern.qualifierToken?.span,
        'Type',
        available(identityOfDeclaration(pattern.enum)),
        locationOfDeclaration(index, pattern.enum),
      )
    if (pattern.member?.canonical._tag === 'Canonical')
      push(
        pending,
        pattern.memberToken?.span,
        'Value',
        available(Object.freeze({ _tag: 'EnumMemberIdentity', id: pattern.member.canonical.id })),
      )
  } else if (pattern._tag === 'NominalPattern') {
    const token = pattern.target._tag === 'Resolved' ? pattern.target.token : undefined
    if (pattern.target._tag === 'Resolved')
      push(
        pending,
        token?.span,
        'Type',
        available(identityOfDeclaration(pattern.target.struct)),
        locationOfDeclaration(index, pattern.target.struct),
      )
    for (const field of pattern.fields) {
      const fieldToken = field.token
      if (field.state._tag === 'Resolved')
        push(
          pending,
          fieldToken?.span,
          'Field',
          available(Object.freeze({ _tag: 'FieldIdentity', id: field.state.field.id })),
          locationOfField(index, field.state.field),
        )
      if (field.nested !== undefined) collectPattern(field.nested, index, scope, pending)
    }
  } else if (pattern._tag === 'UnionVariantPattern') {
    if (pattern.target._tag === 'Resolved') {
      const parentToken = SyntaxTree.tokens(pattern.syntax).find(
        (token) => token.kind === 'Identifier',
      )
      push(
        pending,
        parentToken?.span,
        'Type',
        available(identityOfDeclaration(pattern.target.union)),
        locationOfDeclaration(index, pattern.target.union),
      )
      if (pattern.target.variant.canonical._tag === 'Canonical')
        push(
          pending,
          pattern.target.token.span,
          'Value',
          available(
            Object.freeze({
              _tag: 'UnionVariantIdentity',
              id: pattern.target.variant.canonical.id,
            }),
          ),
          locationOfUnionVariant(pattern.target.variant),
        )
    }
    for (const field of pattern.fields) {
      if (field.state._tag === 'Resolved')
        push(
          pending,
          field.token?.span,
          'Field',
          available(Object.freeze({ _tag: 'FieldIdentity', id: field.state.field.id })),
          locationOfField(index, field.state.field),
        )
      if (field.nested !== undefined) collectPattern(field.nested, index, scope, pending)
    }
  } else if (pattern._tag === 'TypePattern') {
    collectDeclaredType(pattern.declared, index, scope, pending)
  }
}

const collectExpression = (
  expression: Elaboration.ExpressionFact,
  index: DeclarationIndex.Index,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  switch (expression._tag) {
    case 'EnumMember':
      push(
        pending,
        expression.qualifierToken.span,
        'Type',
        available(identityOfDeclaration(expression.enum)),
        locationOfDeclaration(index, expression.enum),
      )
      push(
        pending,
        expression.memberToken.span,
        'Value',
        expression.member?.canonical._tag === 'Canonical'
          ? available(
              Object.freeze({
                _tag: 'EnumMemberIdentity',
                id: expression.member.canonical.id,
              }),
            )
          : Object.freeze({
              _tag: 'Unavailable',
              ...(expression.cause === undefined ? {} : { cause: expression.cause }),
            }),
      )
      return
    case 'EnumValue':
      push(
        pending,
        expression.qualifierToken.span,
        'Type',
        available(
          Object.freeze({
            _tag: 'DeclarationIdentity',
            id: expression.operation.enum,
          }),
        ),
      )
      push(
        pending,
        expression.operationToken.span,
        'Operation',
        available(
          Object.freeze({
            _tag: 'EnumAssociatedOperationIdentity',
            id: expression.operation.id,
          }),
        ),
      )
      collectExpression(expression.argument, index, scope, pending)
      return
    case 'Constant':
      push(
        pending,
        expression.token.span,
        'Value',
        available(identityOfDeclaration(expression.declaration)),
        locationOfDeclaration(index, expression.declaration),
      )
      return
    case 'Identifier': {
      const resolved = parameterResolution(expression.reference)
      push(
        pending,
        'token' in expression.reference ? expression.reference.token.span : undefined,
        'Value',
        resolved.resolution,
        resolved.declaration,
      )
      return
    }
    case 'Call':
      collectCallReference(expression.reference, expression.path, index, scope, pending)
      for (const typeArgument of expression.typeArguments)
        collectDeclaredType(typeArgument.declared, index, scope, pending)
      for (const argument of expression.arguments)
        collectExpression(argument.expression, index, scope, pending)
      return
    case 'Operator':
      collectCallReference(expression.reference, undefined, index, scope, pending)
      for (const argument of expression.arguments)
        collectExpression(argument.expression, index, scope, pending)
      return
    case 'ShortCircuit':
      for (const argument of expression.arguments)
        collectExpression(argument.expression, index, scope, pending)
      return
    case 'FunctionItem':
      collectCallReference(expression.reference, expression.path, index, scope, pending)
      return
    case 'CallableSection':
      collectCallReference(expression.reference, expression.path, index, scope, pending)
      for (const capture of expression.captures)
        collectExpression(capture.expression, index, scope, pending)
      return
    case 'CallableApply':
      collectExpression(expression.callee, index, scope, pending)
      for (const argument of expression.arguments)
        collectExpression(argument.expression, index, scope, pending)
      return
    case 'FieldProjection': {
      collectExpression(expression.subject, index, scope, pending)
      const token = expression.fieldToken
      if (expression.state._tag === 'Resolved')
        push(
          pending,
          token?.span,
          'Field',
          available(Object.freeze({ _tag: 'FieldIdentity', id: expression.state.field.id })),
          locationOfField(index, expression.state.field),
        )
      else
        push(
          pending,
          token?.span,
          'Field',
          Object.freeze({
            _tag: 'Unavailable',
            ...(expression.state._tag === 'Unavailable' && expression.state.cause !== undefined
              ? { cause: expression.state.cause }
              : {}),
          }),
        )
      return
    }
    case 'StructLiteral': {
      const token = expression.target._tag === 'Resolved' ? expression.target.token : undefined
      if (expression.target._tag === 'Resolved')
        push(
          pending,
          token?.span,
          'Type',
          available(identityOfDeclaration(expression.target.struct)),
          locationOfDeclaration(index, expression.target.struct),
        )
      for (const initializer of expression.initializers) {
        const fieldToken = initializer.token
        if (initializer.state._tag === 'Resolved' || initializer.state._tag === 'Inaccessible')
          push(
            pending,
            fieldToken?.span,
            'Field',
            available(Object.freeze({ _tag: 'FieldIdentity', id: initializer.state.field.id })),
            locationOfField(index, initializer.state.field),
          )
        collectExpression(initializer.expression, index, scope, pending)
      }
      return
    }
    case 'UnionVariant': {
      const token = expression.target._tag === 'Resolved' ? expression.target.token : undefined
      if (expression.target._tag === 'Resolved')
        push(
          pending,
          token?.span,
          'Value',
          expression.target.variant.canonical._tag === 'Canonical'
            ? available(
                Object.freeze({
                  _tag: 'UnionVariantIdentity',
                  id: expression.target.variant.canonical.id,
                }),
              )
            : Object.freeze({ _tag: 'Unavailable' }),
          locationOfUnionVariant(expression.target.variant),
        )
      for (const initializer of expression.initializers) {
        const fieldToken = initializer.token
        if (initializer.state._tag === 'Resolved' || initializer.state._tag === 'Inaccessible')
          push(
            pending,
            fieldToken?.span,
            'Field',
            available(Object.freeze({ _tag: 'FieldIdentity', id: initializer.state.field.id })),
            locationOfField(index, initializer.state.field),
          )
        collectExpression(initializer.expression, index, scope, pending)
      }
      return
    }
    case 'Move':
    case 'Borrow':
    case 'Run':
      collectExpression(expression.subject, index, scope, pending)
      return
    case 'PlaceReplace':
      collectIntrinsicReference(expression.reference, index, pending)
      collectExpression(expression.destination, index, scope, pending)
      collectExpression(expression.value, index, scope, pending)
      return
    case 'IndexProjection':
      collectExpression(expression.subject, index, scope, pending)
      collectExpression(expression.index, index, scope, pending)
      return
    case 'ArrayLiteral':
      for (const element of expression.elements)
        collectExpression(element.expression, index, scope, pending)
      return
    case 'Grouped':
      collectExpression(expression.expression, index, scope, pending)
      return
    case 'Match':
      collectExpression(expression.scrutinee, index, scope, pending)
      for (const arm of expression.arms) {
        collectPattern(arm.pattern, index, scope, pending)
        if (arm.guard !== undefined) collectExpression(arm.guard, index, scope, pending)
        collectExpression(arm.result, index, scope, pending)
      }
      return
    case 'EffectBlock':
      for (const statement of expression.statements)
        collectStatement(statement, index, scope, pending)
      return
    case 'EffectCatch':
      collectIntrinsicReference(expression.reference, index, pending)
      collectExpression(expression.protected, index, scope, pending)
      collectExpression(expression.handler, index, scope, pending)
      return
    case 'EffectBindRequirement':
      collectIntrinsicReference(expression.reference, index, pending)
      collectExpression(expression.protected, index, scope, pending)
      return
    case 'Integer':
    case 'Boolean':
      return
  }
}

const collectStatement = (
  statement: Elaboration.StatementFact,
  index: DeclarationIndex.Index,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  switch (statement._tag) {
    case 'UnsafeStatement':
      for (const nested of statement.statements) collectStatement(nested, index, scope, pending)
      return
    case 'BindStatement':
      if (statement.binding.name._tag === 'Present')
        push(
          pending,
          statement.binding.name.token.span,
          'Declaration',
          available(Object.freeze({ _tag: 'BindingIdentity', id: statement.binding.id })),
          locationOfBinding(statement.binding),
        )
      collectExpression(statement.binding.initializer, index, scope, pending)
      return
    case 'PatternBindStatement':
      collectPattern(statement.selection.pattern, index, scope, pending)
      collectExpression(statement.selection.source, index, scope, pending)
      return
    case 'ExpressionStatement':
      collectExpression(statement.expression, index, scope, pending)
      return
    case 'ReturnStatement':
    case 'FailStatement':
    case 'DropStatement':
      collectExpression(statement.expression, index, scope, pending)
      return
    case 'IfStatement':
      collectExpression(statement.condition, index, scope, pending)
      for (const nested of statement.taken) collectStatement(nested, index, scope, pending)
      for (const nested of statement.otherwise) collectStatement(nested, index, scope, pending)
      return
    case 'IfLetStatement':
      collectPattern(statement.selection.pattern, index, scope, pending)
      collectExpression(statement.selection.source, index, scope, pending)
      for (const nested of statement.taken) collectStatement(nested, index, scope, pending)
      for (const nested of statement.otherwise) collectStatement(nested, index, scope, pending)
      return
    case 'WriteStatement':
      collectExpression(statement.destination, index, scope, pending)
      collectExpression(statement.value, index, scope, pending)
      return
    case 'WhileStatement':
      collectExpression(statement.condition, index, scope, pending)
      for (const nested of statement.body) collectStatement(nested, index, scope, pending)
      return
    case 'BreakStatement':
    case 'ContinueStatement':
      return
  }
}

const collectMember = (
  member: DeclarationFacts.MemberFact,
  index: DeclarationIndex.Index,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  const declaration = locationOfDeclaration(index, member)
  if (member.name._tag === 'Present')
    push(
      pending,
      member.name.token.span,
      'Declaration',
      available(identityOfDeclaration(member)),
      declaration,
    )
  for (const typeParameter of member.typeParameters) {
    const parameterLocation = locationOfTypeParameter(typeParameter)
    if (typeParameter.name._tag === 'Present')
      push(
        pending,
        typeParameter.name.token.span,
        'Declaration',
        available(Object.freeze({ _tag: 'TypeParameterIdentity', id: typeParameter.type })),
        parameterLocation,
      )
  }
  if (member._tag === 'FunctionDeclaration') {
    const opaqueBinder = member.opaqueResult?.binder
    if (opaqueBinder?.name._tag === 'Present')
      push(
        pending,
        opaqueBinder.name.token.span,
        'Declaration',
        available(Object.freeze({ _tag: 'TypeParameterIdentity', id: opaqueBinder.type })),
        locationOfTypeParameter(opaqueBinder),
      )
    for (const parameter of member.parameters) {
      if (parameter.name._tag === 'Present')
        push(
          pending,
          parameter.name.token.span,
          'Declaration',
          available(Object.freeze({ _tag: 'ParameterIdentity', id: parameter.id })),
          locationOfParameter(parameter),
        )
      collectDeclaredType(parameter.declaredType, index, scope, pending)
    }
    collectDeclaredType(member.returnType, index, scope, pending)
    collectRowExpression(member.failureRow.expression, index, scope, pending)
    collectRowExpression(member.requirementRow.expression, index, scope, pending)
    for (const constraint of member.constraints)
      collectConstraint(constraint, index, scope, pending)
    return
  }
  if (member._tag === 'ConstantDeclaration') {
    collectDeclaredType(member.declaredType, index, scope, pending)
    return
  }
  if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration') {
    for (const operation of member.operations) {
      const opaqueBinder = operation.opaqueResult?.binder
      if (opaqueBinder?.name._tag === 'Present')
        push(
          pending,
          opaqueBinder.name.token.span,
          'Declaration',
          available(Object.freeze({ _tag: 'TypeParameterIdentity', id: opaqueBinder.type })),
          locationOfTypeParameter(opaqueBinder),
        )
      for (const typeParameter of operation.typeParameters) {
        const parameterLocation = locationOfTypeParameter(typeParameter)
        if (typeParameter.name._tag === 'Present')
          push(
            pending,
            typeParameter.name.token.span,
            'Declaration',
            available(Object.freeze({ _tag: 'TypeParameterIdentity', id: typeParameter.type })),
            parameterLocation,
          )
      }
      const operationLocation = locationOfServiceOperation(operation)
      if (operation.name._tag === 'Present' && operation.state._tag === 'Unique')
        push(
          pending,
          operation.name.token.span,
          'Declaration',
          available(Object.freeze({ _tag: 'ServiceOperationIdentity', id: operation.state.id })),
          operationLocation,
        )
      for (const parameter of operation.parameters) {
        if (parameter.name._tag === 'Present')
          push(
            pending,
            parameter.name.token.span,
            'Declaration',
            available(Object.freeze({ _tag: 'ParameterIdentity', id: parameter.id })),
            locationOfParameter(parameter),
          )
        collectDeclaredType(parameter.declaredType, index, scope, pending)
      }
      collectDeclaredType(operation.returnType, index, scope, pending)
      collectRowExpression(operation.failureRow.expression, index, scope, pending)
      collectRowExpression(operation.requirementRow.expression, index, scope, pending)
      for (const constraint of operation.constraints)
        collectConstraint(constraint, index, scope, pending)
    }
    return
  }
  if (member._tag === 'EnumDeclaration') {
    for (const enumMember of member.members)
      if (enumMember.name._tag === 'Present' && enumMember.canonical._tag === 'Canonical')
        push(
          pending,
          enumMember.name.token.span,
          'Declaration',
          available(Object.freeze({ _tag: 'EnumMemberIdentity', id: enumMember.canonical.id })),
          locationOfEnumMember(enumMember),
        )
    return
  }
  if (member._tag === 'RoleDeclaration') return
  if (member._tag === 'UnionDeclaration') {
    for (const variant of member.variants) {
      if (variant.name._tag === 'Present' && variant.canonical._tag === 'Canonical')
        push(
          pending,
          variant.name.token.span,
          'Declaration',
          available(Object.freeze({ _tag: 'UnionVariantIdentity', id: variant.canonical.id })),
          locationOfUnionVariant(variant),
        )
      for (const field of variant.fields) {
        if (field.name._tag === 'Present')
          push(
            pending,
            field.name.token.span,
            'Declaration',
            available(Object.freeze({ _tag: 'FieldIdentity', id: field.id })),
            locationOfField(index, field),
          )
        collectDeclaredType(field.declaredType, index, scope, pending)
      }
    }
    return
  }
  const fields = member.fields
  for (const field of fields) {
    if (field.name._tag === 'Present')
      push(
        pending,
        field.name.token.span,
        'Declaration',
        available(Object.freeze({ _tag: 'FieldIdentity', id: field.id })),
        locationOfField(index, field),
      )
    collectDeclaredType(field.declaredType, index, scope, pending)
  }
}

const collectImports = (
  scope: NameResolution.ModuleScope | undefined,
  index: DeclarationIndex.Index,
  pending: Array<Pending>,
): void => {
  for (const imported of scope?.imports ?? []) {
    if (imported._tag !== 'Available') continue
    for (const binding of imported.bindings) {
      if (binding._tag === 'LocalDeclaration' || binding._tag === 'IntrinsicActor') continue
      if (binding._tag === 'ModuleNamespace') {
        const selection = binding.token.span
        const declaration = location(selection.sourceId, binding.syntax.span, selection)
        push(
          pending,
          selection,
          'Import',
          available(
            Object.freeze({
              _tag: 'ImportNamespaceIdentity',
              module: binding.module,
              spelling: binding.spelling,
            }),
          ),
          declaration,
        )
        continue
      }
      if (binding._tag === 'ImportedMember') {
        const declarationFact = DeclarationFacts.byCanonical(index, binding.declaration)
        const identity =
          declarationFact === undefined ? undefined : identityOfDeclaration(declarationFact)
        const tokens =
          binding.sourceToken.span.start === binding.localToken.span.start &&
          binding.sourceToken.span.end === binding.localToken.span.end
            ? Object.freeze([binding.sourceToken])
            : Object.freeze([binding.sourceToken, binding.localToken])
        for (const token of tokens)
          push(
            pending,
            token.span,
            'Import',
            identity === undefined ? Object.freeze({ _tag: 'Unavailable' }) : available(identity),
            declarationFact === undefined
              ? undefined
              : locationOfDeclaration(index, declarationFact),
          )
        continue
      }
      if (binding._tag === 'Unavailable')
        for (const token of binding.tokens)
          push(
            pending,
            token.span,
            'Import',
            Object.freeze({
              _tag: 'Unavailable',
              ...(binding.cause === undefined ? {} : { cause: binding.cause }),
            }),
          )
    }
  }
}

/** Builds one module's immutable exact-token occurrence index from recovered compiler facts. */
export const makeModule = (
  module: string,
  result: Elaboration.Result,
  index: DeclarationIndex.Index,
  resolution: NameResolution.Resolution,
): ModuleIndex => {
  const pending: Array<Pending> = []
  const scope = NameResolution.scopeOf(resolution, module)
  const headers = index.modules.find((candidate) => candidate.module === module)
  for (const member of headers?.members ?? []) collectMember(member, index, scope, pending)
  for (const fn of result.functions)
    for (const statement of fn.statements) collectStatement(statement, index, scope, pending)
  collectImports(scope, index, pending)
  pending.sort(
    (left, right) =>
      left.occurrence.span.start - right.occurrence.span.start ||
      left.occurrence.span.end -
        left.occurrence.span.start -
        (right.occurrence.span.end - right.occurrence.span.start) ||
      left.ordinal - right.ordinal,
  )
  const occurrences = Object.freeze(
    pending.map((entry) => Object.freeze({ ...entry.occurrence, ordinal: entry.ordinal })),
  )
  let maximumEnd = 0
  const prefixMaximumEnd = Object.freeze(
    occurrences.map((occurrence) => {
      maximumEnd = Math.max(maximumEnd, occurrence.span.end)
      return maximumEnd
    }),
  )
  const declarationLocations = new Map<string, DeclarationLocation>()
  for (const occurrence of occurrences) {
    if (occurrence.resolution._tag !== 'Available') continue
    const declaration = occurrence.declaration
    if (
      declaration === undefined ||
      declaration.module !== module ||
      occurrence.span.start !== declaration.selectionSpan.start ||
      occurrence.span.end !== declaration.selectionSpan.end
    )
      continue
    declarationLocations.set(identityKey(occurrence.resolution.identity), declaration)
  }
  return Object.freeze({ occurrences, prefixMaximumEnd, declarationLocations })
}

/** Shallowly composes current module indexes and their current declaration locations. */
export const compose = (modules: ReadonlyMap<string, ModuleIndex>): Index => {
  const declarationLocations = new Map<string, DeclarationLocation>()
  for (const moduleIndex of modules.values())
    for (const [identity, declaration] of moduleIndex.declarationLocations)
      declarationLocations.set(identity, declaration)
  return Object.freeze({ _tag: 'SemanticOccurrenceIndex', modules, declarationLocations })
}

/** Builds the immutable exact-token occurrence index from recovered compiler facts. */
export const make = (
  results: ReadonlyMap<string, Elaboration.Result>,
  index: DeclarationIndex.Index,
  resolution: NameResolution.Resolution,
): Index =>
  compose(
    new Map(
      [...results].map(([module, result]) => [
        module,
        makeModule(module, result, index, resolution),
      ]),
    ),
  )

const withCurrentDeclaration = (
  self: Index,
  occurrence: SemanticOccurrence,
): SemanticOccurrence => {
  // Unavailable occurrences can still retain an exact rejected candidate (for example an open
  // `typeof` item). That location was built from the current declaration index and is the useful
  // navigation answer even though no available semantic identity can be rebased through the map.
  if (occurrence.resolution._tag !== 'Available') return occurrence
  const current = self.declarationLocations.get(identityKey(occurrence.resolution.identity))
  if (current === occurrence.declaration) return occurrence
  const { declaration: _previous, ...withoutDeclaration } = occurrence
  return Object.freeze({
    ...withoutDeclaration,
    ...(current === undefined ? {} : { declaration: current }),
  })
}

const lastStartAtOrBefore = (
  occurrences: ReadonlyArray<SemanticOccurrence>,
  offset: number,
): number => {
  let low = 0
  let high = occurrences.length - 1
  let answer = -1
  while (low <= high) {
    const middle = Math.floor((low + high) / 2)
    const candidate = occurrences.at(middle)
    if (candidate !== undefined && candidate.span.start <= offset) {
      answer = middle
      low = middle + 1
    } else high = middle - 1
  }
  return answer
}

/** Returns the smallest deterministic half-open occurrence containing one byte offset. */
export const at = (self: Index, module: string, offset: number): SemanticOccurrence | undefined => {
  const moduleIndex = self.modules.get(module)
  if (moduleIndex === undefined) return undefined
  let cursor = lastStartAtOrBefore(moduleIndex.occurrences, offset)
  let selected: SemanticOccurrence | undefined
  while (cursor >= 0 && (moduleIndex.prefixMaximumEnd.at(cursor) ?? 0) > offset) {
    const candidate = moduleIndex.occurrences.at(cursor)
    if (
      candidate !== undefined &&
      candidate.span.start <= offset &&
      offset < candidate.span.end &&
      (selected === undefined ||
        candidate.span.end - candidate.span.start < selected.span.end - selected.span.start ||
        (candidate.span.end - candidate.span.start === selected.span.end - selected.span.start &&
          candidate.ordinal < selected.ordinal))
    )
      selected = candidate
    cursor -= 1
  }
  return selected === undefined ? undefined : withCurrentDeclaration(self, selected)
}

/** Returns occurrences whose exact token spans overlap one half-open byte range. */
export const inRange = (
  self: Index,
  module: string,
  range: SourceSpan.SourceSpan,
): ReadonlyArray<SemanticOccurrence> =>
  Object.freeze(
    (self.modules.get(module)?.occurrences ?? [])
      .filter(
        (occurrence) => occurrence.span.start < range.end && range.start < occurrence.span.end,
      )
      .map((occurrence) => withCurrentDeclaration(self, occurrence)),
  )

/** Returns a stable structural key for identity deduplication and lookup. */
export const identityKey = (identity: Identity): string => {
  switch (identity._tag) {
    case 'DeclarationIdentity':
      return identity.id._tag === 'CanonicalDeclarationId'
        ? `declaration:${identity.id.module}.${identity.id.name}`
        : `declaration:${identity.id.sourceId}:${identity.id.ordinal}`
    case 'TypeParameterIdentity':
      return `type-parameter:${Type.key(identity.id)}`
    case 'ParameterIdentity':
      return `parameter:${identity.id.function.sourceId}:${identity.id.function.ordinal}:${identity.id.ordinal}`
    case 'BindingIdentity':
      return `binding:${identity.id.function.sourceId}:${identity.id.function.ordinal}:${identity.id.ordinal}`
    case 'PatternBindingIdentity':
      return `pattern:${JSON.stringify(identity.id)}`
    case 'FieldIdentity':
      return `field:${DeclarationFacts.fieldIdKey(identity.id)}`
    case 'UnionVariantIdentity':
      return `union-variant:${identity.id.union.module}.${identity.id.union.name}.${identity.id.name}`
    case 'EnumMemberIdentity':
      return `enum-member:${identity.id.enum.module}.${identity.id.enum.name}.${identity.id.name}`
    case 'EnumAssociatedOperationIdentity':
      return `enum-operation:${identity.id.enum.module}.${identity.id.enum.name}.${identity.id.name}`
    case 'ServiceOperationIdentity':
      return `service-operation:${identity.id.service.sourceId}:${identity.id.service.ordinal}:${identity.id.name}`
    case 'ImportNamespaceIdentity':
      return `namespace:${identity.module}:${identity.spelling}`
    case 'IntrinsicActorIdentity':
      return `intrinsic-actor:${identity.id.name}`
    case 'IntrinsicOperationIdentity':
      return `intrinsic-operation:${identity.id.actor}.${identity.id.name}`
  }
}
