import * as Option from 'effect/Option'
import * as SourceFile from './SourceFile.js'
import type * as SyntaxFile from './SyntaxFile.js'
import type * as Token from './Token.js'
import type * as AuthoredHir from './AuthoredHir.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import type * as Tir from './Tir.js'
import * as Intrinsic from './Intrinsic.js'
import type * as Match from './Match.js'
import * as NameResolution from './NameResolution.js'
import type * as SourceSpan from './SourceSpan.js'
import * as SemanticContext from './SemanticContext.js'
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
  | { readonly _tag: 'BindingIdentity'; readonly id: Tir.BindingId }
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

export type Role =
  | 'Declaration'
  | 'Value'
  | 'Type'
  | 'Field'
  | 'Actor'
  | 'Operation'
  | 'Method'
  | 'Import'

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
  /** The exact authored import name that supplied this unqualified occurrence, when applicable. */
  readonly importBinding?: SourceSpan.SourceSpan
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

/**
 * The alias a retained type path names, if any. An alias is erased from the resolved type, so
 * the path spelling is the only trace of it and is re-looked-up through the same module scope.
 */
const aliasAtPath = (
  path: DeclarationFacts.TypePathFact | undefined,
  scope: NameResolution.ModuleScope | undefined,
  index: DeclarationIndex.Index,
  spans: SemanticContext.Registry,
): DeclarationFacts.AliasFact | undefined => {
  if (path === undefined || scope === undefined) return undefined
  const result = NameResolution.lookupPath(scope, index, path, {
    kind: 'ToolingPath',
    key: path.spelling,
    span: spans.spanOf(path.anchor),
  })
  return result._tag === 'Resolved' && result.declaration._tag === 'AliasDeclaration'
    ? result.declaration
    : undefined
}

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
  spans: SemanticContext.Registry,
  declaration: DeclarationFacts.MemberFact,
): DeclarationLocation | undefined => {
  const current = currentDeclaration(index, declaration)
  return current.name._tag === 'Present'
    ? location(
        current.name.anchor.owner.module,
        spans.spanOf(current.anchor),
        spans.spanOf(current.name.anchor),
      )
    : undefined
}

const locationOfParameter = (
  spans: SemanticContext.Registry,
  parameter: DeclarationFacts.ParameterFact,
): DeclarationLocation | undefined =>
  parameter.name._tag === 'Present'
    ? location(
        parameter.name.anchor.owner.module,
        spans.spanOf(parameter.anchor),
        spans.spanOf(parameter.name.anchor),
      )
    : undefined

const locationOfServiceOperation = (
  spans: SemanticContext.Registry,
  operation: DeclarationFacts.ServiceOperationFact,
): DeclarationLocation | undefined =>
  operation.name._tag === 'Present'
    ? location(
        operation.name.anchor.owner.module,
        spans.spanOf(operation.anchor),
        spans.spanOf(operation.name.anchor),
      )
    : undefined

const locationOfTypeParameter = (
  spans: SemanticContext.Registry,
  parameter: DeclarationFacts.TypeParameterFact,
): DeclarationLocation | undefined =>
  parameter.implicitLifetime !== true && parameter.name._tag === 'Present'
    ? location(
        parameter.name.anchor.owner.module,
        spans.spanOf(parameter.anchor),
        spans.spanOf(parameter.name.anchor),
      )
    : undefined

const locationOfBinding = (
  spans: SemanticContext.Registry,
  binding: Elaboration.BindingDeclarationFact | Elaboration.PatternBindingFact,
): DeclarationLocation | undefined =>
  binding.name._tag === 'Present'
    ? location(
        binding.name.anchor.owner.module,
        spans.spanOf(binding.anchor),
        spans.spanOf(binding.name.anchor),
      )
    : undefined

const locationOfEnumMember = (
  spans: SemanticContext.Registry,
  member: DeclarationFacts.EnumMemberFact,
): DeclarationLocation | undefined =>
  member.name._tag === 'Present'
    ? location(
        member.name.anchor.owner.module,
        spans.spanOf(member.anchor),
        spans.spanOf(member.name.anchor),
      )
    : undefined

const locationOfUnionVariant = (
  spans: SemanticContext.Registry,
  variant: DeclarationFacts.UnionVariantFact,
): DeclarationLocation | undefined =>
  variant.name._tag === 'Present'
    ? location(
        variant.name.anchor.owner.module,
        spans.spanOf(variant.anchor),
        spans.spanOf(variant.name.anchor),
      )
    : undefined

const locationOfField = (
  index: DeclarationIndex.Index,
  spans: SemanticContext.Registry,
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
    ? location(
        current.name.anchor.owner.module,
        spans.spanOf(current.anchor),
        spans.spanOf(current.name.anchor),
      )
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
  | DeclarationFacts.UnionFact
  | DeclarationFacts.ServiceFact
  | DeclarationFacts.InterfaceFact =>
  declaration._tag === 'StructDeclaration' ||
  declaration._tag === 'EnumDeclaration' ||
  declaration._tag === 'UnionDeclaration' ||
  declaration._tag === 'ServiceDeclaration' ||
  declaration._tag === 'InterfaceDeclaration'

const declarationByNominal = (
  index: DeclarationIndex.Index,
  nominal: Type.Nominal,
):
  | DeclarationFacts.StructFact
  | DeclarationFacts.EnumFact
  | DeclarationFacts.UnionFact
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
        | DeclarationFacts.UnionFact
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
  anchor: AuthoredHir.Anchor,
  spelling: string,
  scope: NameResolution.ModuleScope | undefined,
  index: DeclarationIndex.Index,
  spans: SemanticContext.Registry,
  pending: Array<Pending>,
): void => {
  if (scope === undefined) {
    push(pending, spans.spanOf(anchor), 'Actor', Object.freeze({ _tag: 'Unavailable' }))
    return
  }
  const lookup = NameResolution.lookup(scope, index, spelling, {
    kind: 'ToolingName',
    key: spelling,
    span: spans.spanOf(anchor),
  })
  if (lookup._tag === 'Resolved') {
    push(
      pending,
      spans.spanOf(anchor),
      'Actor',
      available(identityOfDeclaration(lookup.declaration)),
      locationOfDeclaration(index, spans, lookup.declaration),
    )
    return
  }
  if (lookup._tag === 'Intrinsic') {
    const actor = Intrinsic.findActor(lookup.actor)
    push(
      pending,
      spans.spanOf(anchor),
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
        ? location(
            spans.spanOf(binding.anchor).sourceId,
            spans.spanOf(binding.anchor),
            spans.spanOf(binding.anchor),
          )
        : undefined
    push(
      pending,
      spans.spanOf(anchor),
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
      spans.spanOf(anchor),
      'Actor',
      Object.freeze({ _tag: 'Conflicting', cause: lookup.conflict.cause }),
    )
    return
  }
  push(pending, spans.spanOf(anchor), 'Actor', Object.freeze({ _tag: 'Unavailable' }))
}

const collectResolvedType = (
  fact: Extract<DeclarationFacts.DeclaredTypeFact, { readonly _tag: 'Resolved' }>,
  index: DeclarationIndex.Index,
  spans: SemanticContext.Registry,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  if (fact.exactItem !== undefined) {
    const qualifier =
      fact.exactItem.path.segments.length > 1 ? fact.exactItem.path.segments.at(0) : undefined
    if (qualifier !== undefined)
      collectQualifier(qualifier.anchor, qualifier.spelling, scope, index, spans, pending)
    const selected = fact.exactItem.path.segments.at(-1)
    const declaration = DeclarationFacts.byCanonical(index, fact.exactItem.declaration)
    push(
      pending,
      selected === undefined ? undefined : spans.spanOf(selected.anchor),
      'Value',
      declaration === undefined
        ? Object.freeze({ _tag: 'Unavailable' })
        : available(identityOfDeclaration(declaration)),
      declaration === undefined ? undefined : locationOfDeclaration(index, spans, declaration),
    )
  }
  if (fact.components !== undefined) {
    for (const component of fact.components)
      collectDeclaredType(component, index, spans, scope, pending)
    // The lifetime argument supplements the named string type rather than replacing its token.
    if (!Type.isString(fact.type)) return
  }
  if (fact.unionSource !== undefined) {
    for (const member of fact.unionSource.members)
      collectDeclaredType(member, index, spans, scope, pending)
    return
  }
  const anchors =
    fact.path?.segments.map((segment) => segment.anchor) ?? Object.freeze([fact.anchor])
  const token = anchors.at(-1) ?? fact.anchor
  const qualifier = anchors.length > 1 ? anchors.at(0) : undefined
  if (qualifier !== undefined) {
    collectQualifier(
      qualifier,
      fact.spelling.split('.').at(0) ?? fact.spelling,
      scope,
      index,
      spans,
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
      spans.spanOf(token),
      'Type',
      available(Object.freeze({ _tag: 'TypeParameterIdentity', id: parameter })),
      declaration === undefined ? undefined : locationOfTypeParameter(spans, declaration),
    )
    return
  }
  if (Type.isParameter(fact.type)) {
    const declaration = typeParameterFact(index, fact.type)
    push(
      pending,
      spans.spanOf(token),
      'Type',
      available(Object.freeze({ _tag: 'TypeParameterIdentity', id: fact.type })),
      declaration === undefined ? undefined : locationOfTypeParameter(spans, declaration),
    )
    return
  }
  const alias = aliasAtPath(fact.path, scope, index, spans)
  if (alias !== undefined) {
    push(
      pending,
      spans.spanOf(token),
      'Type',
      available(identityOfDeclaration(alias)),
      locationOfDeclaration(index, spans, alias),
    )
    return
  }
  if (Type.isNominal(fact.type)) {
    const declaration = declarationByNominal(index, fact.type)
    if (declaration !== undefined) {
      push(
        pending,
        spans.spanOf(token),
        'Type',
        available(identityOfDeclaration(declaration)),
        locationOfDeclaration(index, spans, declaration),
      )
      return
    }
    const actor = Intrinsic.findActor(fact.type.name)
    push(
      pending,
      spans.spanOf(token),
      'Type',
      actor === undefined
        ? Object.freeze({ _tag: 'Unavailable' })
        : available(Object.freeze({ _tag: 'IntrinsicActorIdentity', id: actor.id })),
    )
    return
  }
  if (Type.isString(fact.type) || (typeof fact.type === 'string' && fact.type !== 'never')) {
    const actor = Intrinsic.findActor(Type.isString(fact.type) ? 'string' : fact.type)
    push(
      pending,
      spans.spanOf(token),
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
  spans: SemanticContext.Registry,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  if (fact._tag === 'Resolved') {
    collectResolvedType(fact, index, spans, scope, pending)
    return
  }
  if (fact._tag === 'Unresolved') {
    const qualifier = fact.path.segments.length > 1 ? fact.path.segments.at(0) : undefined
    if (qualifier !== undefined)
      collectQualifier(qualifier.anchor, qualifier.spelling, scope, index, spans, pending)
    const selected = fact.path.segments.at(-1)
    let declarationLocation: DeclarationLocation | undefined
    if (fact.candidate !== undefined) {
      const declaration = declarationByNominal(index, fact.candidate)
      if (declaration !== undefined)
        declarationLocation = locationOfDeclaration(index, spans, declaration)
    }
    push(
      pending,
      selected === undefined ? undefined : spans.spanOf(selected.anchor),
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
    collectDeclaredType(fact.element, index, spans, scope, pending)
    return
  }
  if (fact._tag === 'Reference') {
    collectDeclaredType(fact.target, index, spans, scope, pending)
    return
  }
  if (fact._tag === 'Pointer') {
    collectDeclaredType(fact.pointee, index, spans, scope, pending)
    return
  }
  if (fact._tag === 'Callable' || fact._tag === 'ForeignFunction') {
    for (const parameter of fact.parameters)
      collectDeclaredType(parameter, index, spans, scope, pending)
    collectDeclaredType(fact.result, index, spans, scope, pending)
    return
  }
  if (fact._tag === 'Applied') {
    collectDeclaredType(fact.target, index, spans, scope, pending)
    for (const argument of fact.arguments)
      collectDeclaredType(argument, index, spans, scope, pending)
    return
  }
  if (fact._tag === 'Effect') {
    collectDeclaredType(fact.success, index, spans, scope, pending)
    for (const failure of fact.failures) collectDeclaredType(failure, index, spans, scope, pending)
    for (const requirement of fact.requirements)
      collectDeclaredType(requirement.capability, index, spans, scope, pending)
    return
  }
  if (fact._tag === 'ExactRepresentation') {
    const qualifier = fact.item.segments.length > 1 ? fact.item.segments.at(0) : undefined
    if (qualifier !== undefined)
      collectQualifier(qualifier.anchor, qualifier.spelling, scope, index, spans, pending)
    const selected = fact.item.segments.at(-1)
    const declaration =
      fact.itemCandidate === undefined
        ? undefined
        : DeclarationFacts.byCanonical(index, fact.itemCandidate)
    push(
      pending,
      selected === undefined ? undefined : spans.spanOf(selected.anchor),
      'Value',
      Object.freeze({
        _tag: 'Unavailable',
        ...(fact.cause === undefined ? {} : { cause: fact.cause }),
      }),
      declaration === undefined ? undefined : locationOfDeclaration(index, spans, declaration),
    )
    for (const argument of fact.arguments)
      collectDeclaredType(argument, index, spans, scope, pending)
    return
  }
  if (fact._tag === 'RepresentationParameter') {
    const declaration = typeParameterFact(index, fact.parameter)
    push(
      pending,
      spans.spanOf(fact.anchor),
      'Type',
      available(Object.freeze({ _tag: 'TypeParameterIdentity', id: fact.parameter })),
      declaration === undefined ? undefined : locationOfTypeParameter(spans, declaration),
    )
    return
  }
  if (fact._tag === 'Union')
    for (const member of fact.members) collectDeclaredType(member, index, spans, scope, pending)
}

const collectRowExpression = (
  fact: DeclarationFacts.RowExpressionFact,
  index: DeclarationIndex.Index,
  spans: SemanticContext.Registry,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  switch (fact._tag) {
    case 'EmptyRowExpression':
    case 'UnavailableRowExpression':
      return
    case 'RowParameterExpression': {
      const declaration = typeParameterFact(index, fact.parameter)
      push(
        pending,
        spans.spanOf(fact.anchor),
        'Type',
        available(Object.freeze({ _tag: 'TypeParameterIdentity', id: fact.parameter })),
        declaration === undefined ? undefined : locationOfTypeParameter(spans, declaration),
      )
      return
    }
    case 'FailureMemberExpression':
      collectDeclaredType(fact.member, index, spans, scope, pending)
      return
    case 'RequirementMemberExpression':
      collectDeclaredType(fact.capability, index, spans, scope, pending)
      return
    case 'UnionRowExpression':
      for (const operand of fact.operands)
        collectRowExpression(operand, index, spans, scope, pending)
      return
    case 'WithoutRowExpression':
      collectRowExpression(fact.source, index, spans, scope, pending)
      collectRowExpression(fact.selected, index, spans, scope, pending)
      return
  }
}

const collectConstraint = (
  fact: DeclarationFacts.ConstraintFact,
  index: DeclarationIndex.Index,
  spans: SemanticContext.Registry,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  if (fact._tag === 'ProviderConstraint')
    collectDeclaredType(fact.provider, index, spans, scope, pending)
  collectRowExpression(fact.selected, index, spans, scope, pending)
  collectRowExpression(fact.source, index, spans, scope, pending)
}

const parameterResolution = (
  reference: Elaboration.ParameterReferenceFact,
  spans: SemanticContext.Registry,
): { readonly resolution: Resolution; readonly declaration?: DeclarationLocation } => {
  if (reference._tag === 'Resolved') {
    const declaration = locationOfParameter(spans, reference.parameter)
    return Object.freeze({
      resolution: available(
        Object.freeze({ _tag: 'ParameterIdentity', id: reference.parameter.id }),
      ),
      ...(declaration === undefined ? {} : { declaration }),
    })
  }
  if (reference._tag === 'ResolvedBinding') {
    const declaration = locationOfBinding(spans, reference.binding)
    return Object.freeze({
      resolution: available(Object.freeze({ _tag: 'BindingIdentity', id: reference.binding.id })),
      ...(declaration === undefined ? {} : { declaration }),
    })
  }
  if (reference._tag === 'ResolvedPattern') {
    const declaration = locationOfBinding(spans, reference.binding)
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
  spans: SemanticContext.Registry,
): { readonly resolution: Resolution; readonly declaration?: DeclarationLocation } => {
  if (reference._tag === 'Resolved') {
    const declaration = locationOfDeclaration(index, spans, reference.declaration)
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
  if (reference._tag === 'ResolvedInterfaceOperation') {
    // An interface operation is declared once and answered per specialization by a witness. The
    // declaration is what a reader navigates to, so that is what the occurrence names.
    const declaration = locationOfServiceOperation(spans, reference.declaration)
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
    const declaration = locationOfServiceOperation(spans, reference.operation)
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
  spans: SemanticContext.Registry,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
  qualifierOwnedByTypeApplication = false,
): void => {
  let anchors: ReadonlyArray<AuthoredHir.Anchor>
  if (path?._tag === 'ReferencePath') {
    anchors = Object.freeze([
      ...(path.qualifierAnchor === undefined ? [] : [path.qualifierAnchor]),
      path.memberAnchor,
    ])
  } else if ('anchor' in reference) {
    anchors = Object.freeze([reference.anchor])
  } else {
    anchors = Object.freeze([])
  }
  const qualifier = anchors.length > 1 ? anchors.at(0) : undefined
  if (qualifier !== undefined && !qualifierOwnedByTypeApplication) {
    const qualifierName = 'spelling' in reference ? reference.spelling.split('.').at(0) : undefined
    if (qualifierName !== undefined)
      collectQualifier(qualifier, qualifierName, scope, index, spans, pending)
  }
  const selected = 'anchor' in reference ? reference.anchor : anchors.at(-1)
  const resolved = callResolution(reference, index, spans)
  // A receiver-syntax call names its member without a qualifier token; the member keeps its
  // identity and gains the method role, so hover presents the receiver-bound contract.
  const receiverSyntax =
    path?._tag === 'ReferencePath' &&
    path.qualifierAnchor === undefined &&
    reference._tag === 'Resolved' &&
    reference.declaration._tag === 'FunctionDeclaration' &&
    reference.declaration.associatedMember?.receiver === true
  let role: Role
  if (
    reference._tag === 'ResolvedBuiltin' ||
    reference._tag === 'ResolvedServiceOperation' ||
    reference._tag === 'ResolvedInterfaceOperation'
  ) {
    role = 'Operation'
  } else if (receiverSyntax) {
    role = 'Method'
  } else {
    role = 'Value'
  }
  push(
    pending,
    selected === undefined ? undefined : spans.spanOf(selected),
    role,
    resolved.resolution,
    resolved.declaration,
  )
}

const collectIntrinsicReference = (
  reference: Elaboration.IntrinsicReferenceFact,
  index: DeclarationIndex.Index,
  spans: SemanticContext.Registry,
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
      : locationOfDeclaration(index, spans, reference.actor)
  push(pending, spans.spanOf(reference.actorAnchor), 'Actor', actorResolution, actorDeclaration)
  push(
    pending,
    spans.spanOf(reference.operationAnchor),
    'Operation',
    available(Object.freeze({ _tag: 'IntrinsicOperationIdentity', id: reference.operation.id })),
  )
}

const collectPattern = (
  pattern: Elaboration.PatternFact,
  index: DeclarationIndex.Index,
  spans: SemanticContext.Registry,
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
        spans.spanOf(binding.name.anchor),
        'Declaration',
        available(Object.freeze({ _tag: 'PatternBindingIdentity', id: binding.id })),
        locationOfBinding(spans, binding),
      )
  if (pattern._tag === 'EnumMemberPattern') {
    if (pattern.enum !== undefined)
      push(
        pending,
        pattern.qualifierAnchor === undefined ? undefined : spans.spanOf(pattern.qualifierAnchor),
        'Type',
        available(identityOfDeclaration(pattern.enum)),
        locationOfDeclaration(index, spans, pattern.enum),
      )
    if (pattern.member?.canonical._tag === 'Canonical')
      push(
        pending,
        pattern.memberAnchor === undefined ? undefined : spans.spanOf(pattern.memberAnchor),
        'Value',
        available(Object.freeze({ _tag: 'EnumMemberIdentity', id: pattern.member.canonical.id })),
      )
  } else if (pattern._tag === 'NominalPattern') {
    if (pattern.target._tag === 'Resolved')
      push(
        pending,
        pattern.target.anchor === undefined ? undefined : spans.spanOf(pattern.target.anchor),
        'Type',
        available(identityOfDeclaration(pattern.target.struct)),
        locationOfDeclaration(index, spans, pattern.target.struct),
      )
    for (const field of pattern.fields) {
      if (field.state._tag === 'Resolved')
        push(
          pending,
          spans.spanOf(field.anchor),
          'Field',
          available(Object.freeze({ _tag: 'FieldIdentity', id: field.state.field.id })),
          locationOfField(index, spans, field.state.field),
        )
      if (field.nested !== undefined) collectPattern(field.nested, index, spans, scope, pending)
    }
  } else if (pattern._tag === 'UnionVariantPattern') {
    if (pattern.target._tag === 'Resolved') {
      push(
        pending,
        spans.spanOf(pattern.anchor),
        'Type',
        available(identityOfDeclaration(pattern.target.union)),
        locationOfDeclaration(index, spans, pattern.target.union),
      )
      if (pattern.target.variant.canonical._tag === 'Canonical')
        push(
          pending,
          spans.spanOf(pattern.target.anchor),
          'Value',
          available(
            Object.freeze({
              _tag: 'UnionVariantIdentity',
              id: pattern.target.variant.canonical.id,
            }),
          ),
          locationOfUnionVariant(spans, pattern.target.variant),
        )
    }
    for (const field of pattern.fields) {
      if (field.state._tag === 'Resolved')
        push(
          pending,
          spans.spanOf(field.anchor),
          'Field',
          available(Object.freeze({ _tag: 'FieldIdentity', id: field.state.field.id })),
          locationOfField(index, spans, field.state.field),
        )
      if (field.nested !== undefined) collectPattern(field.nested, index, spans, scope, pending)
    }
  } else if (pattern._tag === 'TypePattern') {
    collectDeclaredType(pattern.declared, index, spans, scope, pending)
  }
}

const collectExpression = (
  expression: Elaboration.ExpressionFact,
  index: DeclarationIndex.Index,
  spans: SemanticContext.Registry,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  switch (expression._tag) {
    case 'EnumMember':
      push(
        pending,
        spans.spanOf(expression.qualifierAnchor),
        'Type',
        available(identityOfDeclaration(expression.enum)),
        locationOfDeclaration(index, spans, expression.enum),
      )
      push(
        pending,
        spans.spanOf(expression.memberAnchor),
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
        spans.spanOf(expression.qualifierAnchor),
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
        spans.spanOf(expression.operationAnchor),
        'Operation',
        available(
          Object.freeze({
            _tag: 'EnumAssociatedOperationIdentity',
            id: expression.operation.id,
          }),
        ),
      )
      collectExpression(expression.argument, index, spans, scope, pending)
      return
    case 'Constant':
      push(
        pending,
        spans.spanOf(expression.anchor),
        'Value',
        available(identityOfDeclaration(expression.declaration)),
        locationOfDeclaration(index, spans, expression.declaration),
      )
      return
    case 'Identifier': {
      const resolved = parameterResolution(expression.reference, spans)
      push(
        pending,
        spans.spanOf(expression.reference.anchor),
        'Value',
        resolved.resolution,
        resolved.declaration,
      )
      return
    }
    case 'Call':
      collectCallReference(
        expression.reference,
        expression.path,
        index,
        spans,
        scope,
        pending,
        expression.interfaceApplication !== undefined,
      )
      if (expression.interfaceApplication !== undefined)
        collectDeclaredType(expression.interfaceApplication, index, spans, scope, pending)
      for (const typeArgument of expression.typeArguments)
        collectDeclaredType(typeArgument.declared, index, spans, scope, pending)
      for (const argument of expression.arguments)
        collectExpression(argument.expression, index, spans, scope, pending)
      return
    case 'Operator':
      collectCallReference(expression.reference, undefined, index, spans, scope, pending)
      for (const argument of expression.arguments)
        collectExpression(argument.expression, index, spans, scope, pending)
      return
    case 'ShortCircuit':
      for (const argument of expression.arguments)
        collectExpression(argument.expression, index, spans, scope, pending)
      return
    case 'FunctionItem':
      collectCallReference(expression.reference, expression.path, index, spans, scope, pending)
      return
    case 'CallableSection':
      if (expression.anonymous === undefined) {
        collectCallReference(expression.reference, expression.path, index, spans, scope, pending)
      } else if (expression.reference._tag === 'Resolved') {
        const declaration = expression.reference.declaration
        for (const parameter of declaration.parameters.slice(
          0,
          expression.remainingParameters.length,
        )) {
          if (parameter.name._tag === 'Present')
            push(
              pending,
              spans.spanOf(parameter.name.anchor),
              'Declaration',
              available(Object.freeze({ _tag: 'ParameterIdentity', id: parameter.id })),
              locationOfParameter(spans, parameter),
            )
          collectDeclaredType(parameter.declaredType, index, spans, scope, pending)
        }
        collectDeclaredType(declaration.returnType, index, spans, scope, pending)
        collectRowExpression(declaration.failureRow.expression, index, spans, scope, pending)
        collectRowExpression(declaration.requirementRow.expression, index, spans, scope, pending)
      }
      for (const capture of expression.captures)
        collectExpression(capture.expression, index, spans, scope, pending)
      return
    case 'ForeignApply':
    case 'CallableApply':
      collectExpression(expression.callee, index, spans, scope, pending)
      for (const argument of expression.arguments)
        collectExpression(argument.expression, index, spans, scope, pending)
      return
    case 'FieldProjection': {
      collectExpression(expression.subject, index, spans, scope, pending)
      const token = expression.anchor
      if (expression.state._tag === 'Resolved')
        push(
          pending,
          token === undefined ? undefined : spans.spanOf(token),
          'Field',
          available(Object.freeze({ _tag: 'FieldIdentity', id: expression.state.field.id })),
          locationOfField(index, spans, expression.state.field),
        )
      else
        push(
          pending,
          token === undefined ? undefined : spans.spanOf(token),
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
      const token = expression.target._tag === 'Resolved' ? expression.target.anchor : undefined
      if (expression.target._tag === 'Resolved')
        push(
          pending,
          token === undefined ? undefined : spans.spanOf(token),
          'Type',
          available(identityOfDeclaration(expression.target.struct)),
          locationOfDeclaration(index, spans, expression.target.struct),
        )
      for (const initializer of expression.initializers) {
        const fieldToken = initializer.anchor
        if (initializer.state._tag === 'Resolved' || initializer.state._tag === 'Inaccessible')
          push(
            pending,
            fieldToken === undefined ? undefined : spans.spanOf(fieldToken),
            'Field',
            available(Object.freeze({ _tag: 'FieldIdentity', id: initializer.state.field.id })),
            locationOfField(index, spans, initializer.state.field),
          )
        collectExpression(initializer.expression, index, spans, scope, pending)
      }
      return
    }
    case 'UnionVariant': {
      const token = expression.target._tag === 'Resolved' ? expression.target.anchor : undefined
      if (expression.target._tag === 'Resolved')
        push(
          pending,
          token === undefined ? undefined : spans.spanOf(token),
          'Value',
          expression.target.variant.canonical._tag === 'Canonical'
            ? available(
                Object.freeze({
                  _tag: 'UnionVariantIdentity',
                  id: expression.target.variant.canonical.id,
                }),
              )
            : Object.freeze({ _tag: 'Unavailable' }),
          locationOfUnionVariant(spans, expression.target.variant),
        )
      for (const initializer of expression.initializers) {
        const fieldToken = initializer.anchor
        if (initializer.state._tag === 'Resolved' || initializer.state._tag === 'Inaccessible')
          push(
            pending,
            fieldToken === undefined ? undefined : spans.spanOf(fieldToken),
            'Field',
            available(Object.freeze({ _tag: 'FieldIdentity', id: initializer.state.field.id })),
            locationOfField(index, spans, initializer.state.field),
          )
        collectExpression(initializer.expression, index, spans, scope, pending)
      }
      return
    }
    case 'Move':
    case 'Borrow':
    case 'Run':
      collectExpression(expression.subject, index, spans, scope, pending)
      return
    case 'PlaceReplace':
      collectIntrinsicReference(expression.reference, index, spans, pending)
      collectExpression(expression.destination, index, spans, scope, pending)
      collectExpression(expression.value, index, spans, scope, pending)
      return
    case 'IndexProjection':
      collectExpression(expression.subject, index, spans, scope, pending)
      collectExpression(expression.index, index, spans, scope, pending)
      return
    case 'ArrayLiteral':
      for (const element of expression.elements)
        collectExpression(element.expression, index, spans, scope, pending)
      return
    case 'Match':
      collectExpression(expression.scrutinee, index, spans, scope, pending)
      for (const arm of expression.arms) {
        collectPattern(arm.pattern, index, spans, scope, pending)
        if (arm.guard !== undefined) collectExpression(arm.guard, index, spans, scope, pending)
        if (arm.body._tag === 'Expression')
          collectExpression(arm.body.expression, index, spans, scope, pending)
        else
          for (const statement of arm.body.statements)
            collectStatement(statement, index, spans, scope, pending)
      }
      return
    case 'EffectBlock':
      for (const statement of expression.statements)
        collectStatement(statement, index, spans, scope, pending)
      return
    case 'EffectCatch':
      collectIntrinsicReference(expression.reference, index, spans, pending)
      collectExpression(expression.protected, index, spans, scope, pending)
      collectExpression(expression.handler, index, spans, scope, pending)
      return
    case 'EffectBindRequirement':
      collectIntrinsicReference(expression.reference, index, spans, pending)
      collectExpression(expression.protected, index, spans, scope, pending)
      return
    case 'Integer':
    case 'Boolean':
      return
  }
}

const collectStatement = (
  statement: Elaboration.StatementFact,
  index: DeclarationIndex.Index,
  spans: SemanticContext.Registry,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  switch (statement._tag) {
    case 'UnsafeStatement':
      for (const nested of statement.statements)
        collectStatement(nested, index, spans, scope, pending)
      return
    case 'BindStatement':
      if (statement.binding.name._tag === 'Present')
        push(
          pending,
          spans.spanOf(statement.binding.name.anchor),
          'Declaration',
          available(Object.freeze({ _tag: 'BindingIdentity', id: statement.binding.id })),
          locationOfBinding(spans, statement.binding),
        )
      if (statement.binding.declaredType !== undefined)
        collectDeclaredType(statement.binding.declaredType, index, spans, scope, pending)
      collectExpression(statement.binding.initializer, index, spans, scope, pending)
      return
    case 'PatternBindStatement':
      collectPattern(statement.selection.pattern, index, spans, scope, pending)
      collectExpression(statement.selection.source, index, spans, scope, pending)
      return
    case 'ExpressionStatement':
      collectExpression(statement.expression, index, spans, scope, pending)
      return
    case 'ReturnStatement':
    case 'FailStatement':
    case 'DropStatement':
      collectExpression(statement.expression, index, spans, scope, pending)
      return
    case 'IfStatement':
      collectExpression(statement.condition, index, spans, scope, pending)
      for (const nested of statement.taken) collectStatement(nested, index, spans, scope, pending)
      for (const nested of statement.otherwise)
        collectStatement(nested, index, spans, scope, pending)
      return
    case 'IfLetStatement':
      collectPattern(statement.selection.pattern, index, spans, scope, pending)
      collectExpression(statement.selection.source, index, spans, scope, pending)
      for (const nested of statement.taken) collectStatement(nested, index, spans, scope, pending)
      for (const nested of statement.otherwise)
        collectStatement(nested, index, spans, scope, pending)
      return
    case 'WriteStatement':
      collectExpression(statement.destination, index, spans, scope, pending)
      collectExpression(statement.value, index, spans, scope, pending)
      return
    case 'WhileStatement':
      collectExpression(statement.condition, index, spans, scope, pending)
      for (const nested of statement.body) collectStatement(nested, index, spans, scope, pending)
      return
    case 'BreakStatement':
    case 'ContinueStatement':
      return
  }
}

const collectMember = (
  member: DeclarationFacts.MemberFact,
  index: DeclarationIndex.Index,
  spans: SemanticContext.Registry,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  const declaration = locationOfDeclaration(index, spans, member)
  if (member.name._tag === 'Present')
    push(
      pending,
      spans.spanOf(member.name.anchor),
      'Declaration',
      available(identityOfDeclaration(member)),
      declaration,
    )
  for (const typeParameter of member.typeParameters) {
    // Elision anchors a semantic binder to an annotation; it does not declare a source name there.
    if (typeParameter.implicitLifetime === true) continue
    // An inherent member carries its impl head's binders ahead of its own; the head declares
    // those once (see `collectInherentImpl`), so only binders spelled inside the member count.
    if (spans.spanOf(typeParameter.anchor).start < spans.spanOf(member.anchor).start) continue
    const parameterLocation = locationOfTypeParameter(spans, typeParameter)
    if (typeParameter.name._tag === 'Present')
      push(
        pending,
        spans.spanOf(typeParameter.name.anchor),
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
        spans.spanOf(opaqueBinder.name.anchor),
        'Declaration',
        available(Object.freeze({ _tag: 'TypeParameterIdentity', id: opaqueBinder.type })),
        locationOfTypeParameter(spans, opaqueBinder),
      )
    for (const parameter of member.parameters) {
      if (parameter.name._tag === 'Present')
        push(
          pending,
          spans.spanOf(parameter.name.anchor),
          'Declaration',
          available(Object.freeze({ _tag: 'ParameterIdentity', id: parameter.id })),
          locationOfParameter(spans, parameter),
        )
      collectDeclaredType(parameter.declaredType, index, spans, scope, pending)
    }
    collectDeclaredType(member.returnType, index, spans, scope, pending)
    collectRowExpression(member.failureRow.expression, index, spans, scope, pending)
    collectRowExpression(member.requirementRow.expression, index, spans, scope, pending)
    for (const constraint of member.constraints)
      collectConstraint(constraint, index, spans, scope, pending)
    return
  }
  if (
    member._tag === 'ConstantDeclaration' ||
    member._tag === 'PackageParameterDeclaration' ||
    member._tag === 'ForeignStaticDeclaration'
  ) {
    collectDeclaredType(member.declaredType, index, spans, scope, pending)
    return
  }
  if (member._tag === 'AliasDeclaration') {
    collectDeclaredType(member.target, index, spans, scope, pending)
    return
  }
  if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration') {
    for (const operation of member.operations) {
      const opaqueBinder = operation.opaqueResult?.binder
      if (opaqueBinder?.name._tag === 'Present')
        push(
          pending,
          spans.spanOf(opaqueBinder.name.anchor),
          'Declaration',
          available(Object.freeze({ _tag: 'TypeParameterIdentity', id: opaqueBinder.type })),
          locationOfTypeParameter(spans, opaqueBinder),
        )
      for (const typeParameter of operation.typeParameters) {
        const parameterLocation = locationOfTypeParameter(spans, typeParameter)
        if (typeParameter.name._tag === 'Present')
          push(
            pending,
            spans.spanOf(typeParameter.name.anchor),
            'Declaration',
            available(Object.freeze({ _tag: 'TypeParameterIdentity', id: typeParameter.type })),
            parameterLocation,
          )
      }
      const operationLocation = locationOfServiceOperation(spans, operation)
      if (operation.name._tag === 'Present' && operation.state._tag === 'Unique')
        push(
          pending,
          spans.spanOf(operation.name.anchor),
          'Declaration',
          available(Object.freeze({ _tag: 'ServiceOperationIdentity', id: operation.state.id })),
          operationLocation,
        )
      for (const parameter of operation.parameters) {
        if (parameter.name._tag === 'Present')
          push(
            pending,
            spans.spanOf(parameter.name.anchor),
            'Declaration',
            available(Object.freeze({ _tag: 'ParameterIdentity', id: parameter.id })),
            locationOfParameter(spans, parameter),
          )
        collectDeclaredType(parameter.declaredType, index, spans, scope, pending)
      }
      collectDeclaredType(operation.returnType, index, spans, scope, pending)
      collectRowExpression(operation.failureRow.expression, index, spans, scope, pending)
      collectRowExpression(operation.requirementRow.expression, index, spans, scope, pending)
      for (const constraint of operation.constraints)
        collectConstraint(constraint, index, spans, scope, pending)
    }
    return
  }
  if (member._tag === 'EnumDeclaration') {
    for (const enumMember of member.members)
      if (enumMember.name._tag === 'Present' && enumMember.canonical._tag === 'Canonical')
        push(
          pending,
          spans.spanOf(enumMember.name.anchor),
          'Declaration',
          available(Object.freeze({ _tag: 'EnumMemberIdentity', id: enumMember.canonical.id })),
          locationOfEnumMember(spans, enumMember),
        )
    return
  }
  if (member._tag === 'RoleDeclaration') return
  if (member._tag === 'UnionDeclaration') {
    for (const variant of member.variants) {
      if (variant.name._tag === 'Present' && variant.canonical._tag === 'Canonical')
        push(
          pending,
          spans.spanOf(variant.name.anchor),
          'Declaration',
          available(Object.freeze({ _tag: 'UnionVariantIdentity', id: variant.canonical.id })),
          locationOfUnionVariant(spans, variant),
        )
      for (const field of variant.fields) {
        if (field.name._tag === 'Present')
          push(
            pending,
            spans.spanOf(field.name.anchor),
            'Declaration',
            available(Object.freeze({ _tag: 'FieldIdentity', id: field.id })),
            locationOfField(index, spans, field),
          )
        collectDeclaredType(field.declaredType, index, spans, scope, pending)
      }
    }
    return
  }
  const fields = member.fields
  for (const field of fields) {
    if (field.name._tag === 'Present')
      push(
        pending,
        spans.spanOf(field.name.anchor),
        'Declaration',
        available(Object.freeze({ _tag: 'FieldIdentity', id: field.id })),
        locationOfField(index, spans, field),
      )
    collectDeclaredType(field.declaredType, index, spans, scope, pending)
  }
}

/** The head `impl<Binders> Owner<Binders>` declares the binders once and references the owner. */
const collectInherentImpl = (
  head: DeclarationFacts.InherentImplFact,
  index: DeclarationIndex.Index,
  spans: SemanticContext.Registry,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  for (const typeParameter of head.typeParameters)
    if (typeParameter.name._tag === 'Present')
      push(
        pending,
        spans.spanOf(typeParameter.name.anchor),
        'Declaration',
        available(Object.freeze({ _tag: 'TypeParameterIdentity', id: typeParameter.type })),
        locationOfTypeParameter(spans, typeParameter),
      )
  collectDeclaredType(head.owner, index, spans, scope, pending)
}

const unavailableLookupResolution = (
  lookup: Exclude<NameResolution.Lookup, { readonly _tag: 'Resolved' }>,
): Resolution => {
  switch (lookup._tag) {
    case 'Missing':
      return Object.freeze({ _tag: 'Missing' })
    case 'Inaccessible':
      return Object.freeze({ _tag: 'Inaccessible', cause: lookup.cause })
    case 'Conflict':
      return Object.freeze({ _tag: 'Conflicting', cause: lookup.conflict.cause })
    case 'Unavailable':
      return Object.freeze({
        _tag: 'Unavailable',
        ...(lookup.cause === undefined ? {} : { cause: lookup.cause }),
      })
    default:
      return Object.freeze({ _tag: 'Unavailable' })
  }
}

const collectTypePath = (
  path: DeclarationFacts.TypePathFact,
  index: DeclarationIndex.Index,
  spans: SemanticContext.Registry,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  const qualifier = path.segments.length > 1 ? path.segments.at(0) : undefined
  if (qualifier !== undefined)
    collectQualifier(qualifier.anchor, qualifier.spelling, scope, index, spans, pending)
  const selected = path.segments.at(-1)
  if (selected === undefined || scope === undefined) return
  const lookup = NameResolution.lookupPath(scope, index, path, {
    kind: 'ToolingPath',
    key: path.spelling,
    span: spans.spanOf(path.anchor),
  })
  if (lookup._tag === 'Resolved') {
    push(
      pending,
      spans.spanOf(selected.anchor),
      'Value',
      available(identityOfDeclaration(lookup.declaration)),
      locationOfDeclaration(index, spans, lookup.declaration),
    )
    return
  }
  push(
    pending,
    spans.spanOf(selected.anchor),
    'Value',
    unavailableLookupResolution(lookup),
    'declaration' in lookup && lookup.declaration !== undefined
      ? locationOfDeclaration(index, spans, lookup.declaration)
      : undefined,
  )
}

const collectConformance = (
  conformance: DeclarationFacts.ConformanceFact,
  index: DeclarationIndex.Index,
  spans: SemanticContext.Registry,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  for (const typeParameter of conformance.typeParameters)
    if (typeParameter.name._tag === 'Present')
      push(
        pending,
        spans.spanOf(typeParameter.name.anchor),
        'Declaration',
        available(Object.freeze({ _tag: 'TypeParameterIdentity', id: typeParameter.type })),
        locationOfTypeParameter(spans, typeParameter),
      )
  for (const requirement of conformance.requirements)
    collectDeclaredType(requirement.capability, index, spans, scope, pending)
  collectDeclaredType(conformance.capability, index, spans, scope, pending)
  collectDeclaredType(conformance.provider, index, spans, scope, pending)
  for (const operation of conformance.operations)
    if (operation.target._tag === 'TypePath')
      collectTypePath(operation.target, index, spans, scope, pending)
  if (conformance.hook !== undefined) {
    collectDeclaredType(conformance.hook.parameterType, index, spans, scope, pending)
    collectDeclaredType(conformance.hook.returnType, index, spans, scope, pending)
    collectRowExpression(conformance.hook.failureRow.expression, index, spans, scope, pending)
    collectRowExpression(conformance.hook.requirementRow.expression, index, spans, scope, pending)
  }
}

const collectImports = (
  scope: NameResolution.ModuleScope | undefined,
  index: DeclarationIndex.Index,
  spans: SemanticContext.Registry,
  pending: Array<Pending>,
): void => {
  for (const imported of scope?.imports ?? []) {
    if (imported._tag !== 'Available') continue
    for (const binding of imported.bindings) {
      if (binding._tag === 'LocalDeclaration' || binding._tag === 'IntrinsicActor') continue
      if (binding._tag === 'ModuleNamespace') {
        const selection = spans.spanOf(binding.anchor)
        const declaration = location(selection.sourceId, spans.spanOf(binding.anchor), selection)
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
        const sourceSpan = spans.spanOf(binding.sourceAnchor)
        const localSpan = spans.spanOf(binding.localAnchor)
        const selected =
          sourceSpan.start === localSpan.start && sourceSpan.end === localSpan.end
            ? Object.freeze([sourceSpan])
            : Object.freeze([sourceSpan, localSpan])
        for (const span of selected)
          push(
            pending,
            span,
            'Import',
            identity === undefined ? Object.freeze({ _tag: 'Unavailable' }) : available(identity),
            declarationFact === undefined
              ? undefined
              : locationOfDeclaration(index, spans, declarationFact),
          )
        continue
      }
      if (binding._tag === 'Unavailable')
        for (const anchor of binding.anchors)
          push(
            pending,
            spans.spanOf(anchor),
            'Import',
            Object.freeze({
              _tag: 'Unavailable',
              ...(binding.cause === undefined ? {} : { cause: binding.cause }),
            }),
          )
    }
  }
}

/** Starts of identifiers written after a `.`: members reached through a qualifier. */
const qualifiedOccurrenceStarts = (tokens: ReadonlyArray<Token.Token>): ReadonlySet<number> => {
  const starts = new Set<number>()
  let previous: Token.Token | undefined
  for (const token of tokens) {
    if (token.kind === 'Whitespace' || token.kind.endsWith('Comment')) continue
    if (token.kind === 'Identifier' && previous?.kind === 'Dot') starts.add(token.span.start)
    previous = token
  }
  return starts
}

/**
 * The authored import name that supplied one unqualified occurrence.
 *
 * Which import a use spends is a fact about what was written: two aliases of one declaration are
 * told apart only by the spelling at the use, and `geo.area` spends `geo`, not a direct `area`
 * selector beside it. Both come from the module's syntax, which this tooling index is built with.
 */
const importBindingFor = (
  occurrence: Omit<SemanticOccurrence, 'ordinal'>,
  spans: SemanticContext.Registry,
  bindings: ReadonlyMap<string, NameResolution.Binding>,
  syntax: SyntaxFile.SyntaxFile,
  qualifiedStarts: ReadonlySet<number>,
): SourceSpan.SourceSpan | undefined => {
  if (occurrence.role === 'Import' || occurrence.resolution._tag !== 'Available') return undefined
  const spelling = Option.getOrUndefined(SourceFile.spelling(syntax.source, occurrence.span))
  const binding = spelling === undefined ? undefined : bindings.get(spelling)
  const identity = occurrence.resolution.identity
  if (binding?._tag === 'ModuleNamespace')
    return identity._tag === 'ImportNamespaceIdentity' &&
      identity.module === binding.module &&
      identity.spelling === binding.spelling
      ? spans.spanOf(binding.anchor)
      : undefined
  if (binding?._tag !== 'ImportedMember' || qualifiedStarts.has(occurrence.span.start))
    return undefined
  return identity._tag === 'DeclarationIdentity' &&
    identityKey(identity) ===
      identityKey(Object.freeze({ _tag: 'DeclarationIdentity', id: binding.declaration }))
    ? spans.spanOf(binding.localAnchor)
    : undefined
}

/** Builds one module's immutable exact-token occurrence index from recovered compiler facts. */
export const makeModule = (
  module: string,
  result: Elaboration.Result,
  index: DeclarationIndex.Index,
  spans: SemanticContext.Registry,
  resolution: NameResolution.Resolution,
  conditions: ReadonlyArray<Elaboration.ExpressionFact> = [],
  syntax?: SyntaxFile.SyntaxFile,
): ModuleIndex => {
  const pending: Array<Pending> = []
  const scope = NameResolution.scopeOf(resolution, module)
  const headers = index.modules.find((candidate) => candidate.module === module)
  for (const member of headers?.members ?? []) collectMember(member, index, spans, scope, pending)
  for (const head of headers?.inherentImpls ?? [])
    collectInherentImpl(head, index, spans, scope, pending)
  for (const conformance of headers?.conformances ?? [])
    collectConformance(conformance, index, spans, scope, pending)
  for (const fn of Elaboration.executableFunctions(result))
    for (const statement of fn.statements) collectStatement(statement, index, spans, scope, pending)
  for (const condition of conditions) collectExpression(condition, index, spans, scope, pending)
  collectImports(scope, index, spans, pending)
  // The first binding of a spelling is the effective one; later ones are conflicts.
  const bindings = new Map<string, NameResolution.Binding>()
  for (const binding of scope?.bindings ?? [])
    if (!bindings.has(binding.spelling)) bindings.set(binding.spelling, binding)
  const qualifiedStarts =
    syntax === undefined ? new Set<number>() : qualifiedOccurrenceStarts(syntax.tokens)
  const attributed = pending.map((entry): Pending => {
    const importBinding =
      syntax === undefined
        ? undefined
        : importBindingFor(entry.occurrence, spans, bindings, syntax, qualifiedStarts)
    return importBinding === undefined
      ? entry
      : Object.freeze({
          ...entry,
          occurrence: Object.freeze({ ...entry.occurrence, importBinding }),
        })
  })
  attributed.sort(
    (left, right) =>
      left.occurrence.span.start - right.occurrence.span.start ||
      left.occurrence.span.end -
        left.occurrence.span.start -
        (right.occurrence.span.end - right.occurrence.span.start) ||
      left.ordinal - right.ordinal,
  )
  const occurrences = Object.freeze(
    attributed.map((entry) => Object.freeze({ ...entry.occurrence, ordinal: entry.ordinal })),
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
): Index => {
  const spans = SemanticContext.fromModules([...results.values()])
  return compose(
    new Map(
      [...results].map(([module, result]) => [
        module,
        makeModule(module, result, index, spans, resolution),
      ]),
    ),
  )
}

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
