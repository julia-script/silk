import * as DeclarationIndex from './DeclarationIndex.js'
import type * as Diagnostic from './Diagnostic.js'
import type * as Elaboration from './Elaboration.js'
import type * as Hir from './Hir.js'
import * as Intrinsic from './Intrinsic.js'
import type * as Match from './Match.js'
import * as NameResolution from './NameResolution.js'
import type * as SourceSpan from './SourceSpan.js'
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
      readonly id: DeclarationIndex.CanonicalId | DeclarationIndex.DeclarationId
    }
  | { readonly _tag: 'TypeParameterIdentity'; readonly id: Type.Parameter }
  | { readonly _tag: 'ParameterIdentity'; readonly id: DeclarationIndex.ParameterId }
  | { readonly _tag: 'BindingIdentity'; readonly id: Hir.BindingId }
  | { readonly _tag: 'PatternBindingIdentity'; readonly id: Match.BindingId }
  | { readonly _tag: 'FieldIdentity'; readonly id: DeclarationIndex.FieldId }
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
}

/** Deterministic semantic occurrences grouped by canonical source module. */
export interface Index {
  readonly _tag: 'SemanticOccurrenceIndex'
  readonly modules: ReadonlyMap<string, ModuleIndex>
}

const identityOfDeclaration = (declaration: DeclarationIndex.MemberFact): Identity =>
  Object.freeze({
    _tag: 'DeclarationIdentity',
    id: declaration.canonical._tag === 'Canonical' ? declaration.canonical.id : declaration.id,
  })

const location = (
  module: string,
  span: SourceSpan.SourceSpan,
  selectionSpan: SourceSpan.SourceSpan,
): DeclarationLocation => Object.freeze({ module, span, selectionSpan })

const locationOfDeclaration = (
  declaration: DeclarationIndex.MemberFact,
): DeclarationLocation | undefined =>
  declaration.name._tag === 'Present'
    ? location(
        declaration.name.token.span.sourceId,
        declaration.syntax.span,
        declaration.name.token.span,
      )
    : undefined

const locationOfParameter = (
  parameter: DeclarationIndex.ParameterFact,
): DeclarationLocation | undefined =>
  parameter.name._tag === 'Present'
    ? location(parameter.name.token.span.sourceId, parameter.syntax.span, parameter.name.token.span)
    : undefined

const locationOfTypeParameter = (
  parameter: DeclarationIndex.TypeParameterFact,
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

const locationOfField = (field: DeclarationIndex.FieldFact): DeclarationLocation | undefined =>
  field.name._tag === 'Present'
    ? location(field.name.token.span.sourceId, field.syntax.span, field.name.token.span)
    : undefined

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

const declarationByNominal = (
  index: DeclarationIndex.Index,
  nominal: Type.Nominal,
): DeclarationIndex.StructFact | undefined =>
  index.modules
    .find((module) => module.module === nominal.module)
    ?.structs.find(
      (declaration) =>
        declaration.canonical._tag === 'Canonical' &&
        declaration.canonical.id.name === nominal.name,
    )

const typeParameterFact = (
  index: DeclarationIndex.Index,
  type: Type.Parameter,
): DeclarationIndex.TypeParameterFact | undefined => {
  for (const module of index.modules)
    for (const member of module.members) {
      const parameter = member.typeParameters.find((candidate) => Type.equals(candidate.type, type))
      if (parameter !== undefined) return parameter
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
  fact: Extract<DeclarationIndex.DeclaredTypeFact, { readonly _tag: 'Resolved' }>,
  index: DeclarationIndex.Index,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
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
        locationOfDeclaration(declaration),
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
  fact: DeclarationIndex.DeclaredTypeFact,
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
    push(
      pending,
      selected?.token.span,
      'Type',
      Object.freeze({
        _tag: fact.candidate === undefined ? 'Missing' : 'Inaccessible',
        ...(fact.cause === undefined ? {} : { cause: fact.cause }),
      }),
      fact.candidate === undefined
        ? undefined
        : (() => {
            const declaration = declarationByNominal(index, fact.candidate)
            return declaration === undefined ? undefined : locationOfDeclaration(declaration)
          })(),
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
  if (fact._tag === 'Union')
    for (const member of fact.members) collectDeclaredType(member, index, scope, pending)
}

const parameterResolution = (
  reference: Elaboration.ParameterReferenceFact,
): { readonly resolution: Resolution; readonly declaration?: DeclarationLocation } => {
  if (reference._tag === 'Resolved')
    return (() => {
      const declaration = locationOfParameter(reference.parameter)
      return Object.freeze({
        resolution: available(
          Object.freeze({ _tag: 'ParameterIdentity', id: reference.parameter.id }),
        ),
        ...(declaration === undefined ? {} : { declaration }),
      })
    })()
  if (reference._tag === 'ResolvedBinding')
    return (() => {
      const declaration = locationOfBinding(reference.binding)
      return Object.freeze({
        resolution: available(Object.freeze({ _tag: 'BindingIdentity', id: reference.binding.id })),
        ...(declaration === undefined ? {} : { declaration }),
      })
    })()
  if (reference._tag === 'ResolvedPattern')
    return (() => {
      const declaration = locationOfBinding(reference.binding)
      return Object.freeze({
        resolution: available(
          Object.freeze({ _tag: 'PatternBindingIdentity', id: reference.binding.id }),
        ),
        ...(declaration === undefined ? {} : { declaration }),
      })
    })()
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
): { readonly resolution: Resolution; readonly declaration?: DeclarationLocation } => {
  if (reference._tag === 'Resolved') {
    const declaration = locationOfDeclaration(reference.declaration)
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
  const tokens =
    path?._tag === 'ReferencePath'
      ? Object.freeze([...(path.qualifier === undefined ? [] : [path.qualifier]), path.member])
      : 'token' in reference
        ? Object.freeze([reference.token])
        : Object.freeze([])
  const qualifier = tokens.length > 1 ? tokens.at(0) : undefined
  if (qualifier !== undefined) {
    const qualifierName = 'spelling' in reference ? reference.spelling.split('.').at(0) : undefined
    if (qualifierName !== undefined)
      collectQualifier(qualifier, qualifierName, scope, index, pending)
  }
  const selected = 'token' in reference ? reference.token : tokens.at(-1)
  const resolved = callResolution(reference)
  push(
    pending,
    selected?.span,
    reference._tag === 'ResolvedBuiltin' ? 'Operation' : 'Value',
    resolved.resolution,
    resolved.declaration,
  )
}

const collectIntrinsicReference = (
  reference: Elaboration.IntrinsicReferenceFact,
  pending: Array<Pending>,
): void => {
  if (reference._tag === 'UnavailableIntrinsicReference') return
  const actorResolution =
    reference.actor._tag === 'IntrinsicActor'
      ? available(Object.freeze({ _tag: 'IntrinsicActorIdentity', id: reference.actor.id }))
      : available(identityOfDeclaration(reference.actor))
  const actorDeclaration =
    reference.actor._tag === 'IntrinsicActor' ? undefined : locationOfDeclaration(reference.actor)
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
  if (pattern._tag === 'NominalPattern') {
    const token = pattern.target._tag === 'Resolved' ? pattern.target.token : undefined
    if (pattern.target._tag === 'Resolved')
      push(
        pending,
        token?.span,
        'Type',
        available(identityOfDeclaration(pattern.target.struct)),
        locationOfDeclaration(pattern.target.struct),
      )
    for (const field of pattern.fields) {
      const fieldToken = field.token
      if (field.state._tag === 'Resolved')
        push(
          pending,
          fieldToken?.span,
          'Field',
          available(Object.freeze({ _tag: 'FieldIdentity', id: field.state.field.id })),
          locationOfField(field.state.field),
        )
      if (field.nested !== undefined) collectPattern(field.nested, index, scope, pending)
    }
  }
  for (const binding of pattern.bindings)
    if (binding.name._tag === 'Present')
      push(
        pending,
        binding.name.token.span,
        'Declaration',
        available(Object.freeze({ _tag: 'PatternBindingIdentity', id: binding.id })),
        locationOfBinding(binding),
      )
}

const collectExpression = (
  expression: Elaboration.ExpressionFact,
  index: DeclarationIndex.Index,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  switch (expression._tag) {
    case 'Constant':
      push(
        pending,
        expression.token.span,
        'Value',
        available(identityOfDeclaration(expression.declaration)),
        locationOfDeclaration(expression.declaration),
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
          locationOfField(expression.state.field),
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
          locationOfDeclaration(expression.target.struct),
        )
      for (const initializer of expression.initializers) {
        const fieldToken = initializer.token
        if (initializer.state._tag === 'Resolved')
          push(
            pending,
            fieldToken?.span,
            'Field',
            available(Object.freeze({ _tag: 'FieldIdentity', id: initializer.state.field.id })),
            locationOfField(initializer.state.field),
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
      collectIntrinsicReference(expression.reference, pending)
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
      collectIntrinsicReference(expression.reference, pending)
      for (const typeArgument of expression.typeArguments)
        collectDeclaredType(typeArgument.declared, index, scope, pending)
      collectExpression(expression.protected, index, scope, pending)
      collectExpression(expression.handler, index, scope, pending)
      return
    case 'EffectRetry':
      collectIntrinsicReference(expression.reference, pending)
      collectExpression(expression.protected, index, scope, pending)
      collectExpression(expression.retries, index, scope, pending)
      return
    case 'EffectProvide':
      collectIntrinsicReference(expression.reference, pending)
      collectExpression(expression.protected, index, scope, pending)
      return
    case 'EffectProvideWith':
      collectIntrinsicReference(expression.reference, pending)
      collectExpression(expression.protected, index, scope, pending)
      collectExpression(expression.acquisition, index, scope, pending)
      return
    case 'EffectTransform':
      collectIntrinsicReference(expression.reference, pending)
      collectExpression(expression.protected, index, scope, pending)
      collectExpression(expression.callback, index, scope, pending)
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
  member: DeclarationIndex.MemberFact,
  index: DeclarationIndex.Index,
  scope: NameResolution.ModuleScope | undefined,
  pending: Array<Pending>,
): void => {
  const declaration = locationOfDeclaration(member)
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
    for (const failure of member.failureRow.members)
      collectDeclaredType(failure, index, scope, pending)
    for (const requirement of member.requirementRow.entries)
      collectDeclaredType(requirement.capability, index, scope, pending)
    return
  }
  if (member._tag === 'ConstantDeclaration') {
    collectDeclaredType(member.declaredType, index, scope, pending)
    return
  }
  for (const field of member.fields) {
    if (field.name._tag === 'Present')
      push(
        pending,
        field.name.token.span,
        'Declaration',
        available(Object.freeze({ _tag: 'FieldIdentity', id: field.id })),
        locationOfField(field),
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
        const declarationFact = DeclarationIndex.byCanonical(index, binding.declaration)
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
            declarationFact === undefined ? undefined : locationOfDeclaration(declarationFact),
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

/** Builds the immutable exact-token occurrence index from recovered compiler facts. */
export const make = (
  results: ReadonlyMap<string, Elaboration.Result>,
  index: DeclarationIndex.Index,
  resolution: NameResolution.Resolution,
): Index => {
  const modules = new Map<string, ModuleIndex>()
  for (const [module, result] of results) {
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
    modules.set(module, Object.freeze({ occurrences, prefixMaximumEnd }))
  }
  return Object.freeze({ _tag: 'SemanticOccurrenceIndex', modules })
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
  return selected
}

/** Returns occurrences whose exact token spans overlap one half-open byte range. */
export const inRange = (
  self: Index,
  module: string,
  range: SourceSpan.SourceSpan,
): ReadonlyArray<SemanticOccurrence> =>
  Object.freeze(
    (self.modules.get(module)?.occurrences ?? []).filter(
      (occurrence) => occurrence.span.start < range.end && range.start < occurrence.span.end,
    ),
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
      return `field:${identity.id.struct.sourceId}:${identity.id.struct.ordinal}:${identity.id.ordinal}`
    case 'ImportNamespaceIdentity':
      return `namespace:${identity.module}:${identity.spelling}`
    case 'IntrinsicActorIdentity':
      return `intrinsic-actor:${identity.id.name}`
    case 'IntrinsicOperationIdentity':
      return `intrinsic-operation:${identity.id.actor}.${identity.id.name}`
  }
}
