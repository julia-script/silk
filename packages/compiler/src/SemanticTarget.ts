import * as DeclarationIndex from './DeclarationIndex.js'
import type * as Diagnostic from './Diagnostic.js'
import type * as Elaboration from './Elaboration.js'
import type * as Hir from './Hir.js'
import type * as Match from './Match.js'
import type * as NameResolution from './NameResolution.js'
import type * as SourceSpan from './SourceSpan.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'

/** The exact source location of a declaration and its declared name. */
export interface DeclarationLocation {
  readonly module: string
  readonly span: SourceSpan.SourceSpan
  readonly selectionSpan: SourceSpan.SourceSpan
}

/** A compiler-owned identity selected by one reference-bearing semantic fact. */
export type Identity =
  | {
      readonly _tag: 'DeclarationTarget'
      readonly id: DeclarationIndex.CanonicalId | DeclarationIndex.DeclarationId
    }
  | { readonly _tag: 'ParameterTarget'; readonly id: DeclarationIndex.ParameterId }
  | { readonly _tag: 'BindingTarget'; readonly id: Hir.BindingId }
  | { readonly _tag: 'PatternBindingTarget'; readonly id: Match.BindingId }
  | { readonly _tag: 'FieldTarget'; readonly id: DeclarationIndex.FieldId }

/** The available or explicitly recovered outcome selected at one source reference. */
export type Resolution =
  | {
      readonly _tag: 'Available'
      readonly target: Identity
      readonly declaration: DeclarationLocation
    }
  | { readonly _tag: 'Missing'; readonly cause?: Diagnostic.Identity }
  | { readonly _tag: 'Inaccessible'; readonly cause?: Diagnostic.Identity }
  | { readonly _tag: 'Ambiguous'; readonly cause?: Diagnostic.Identity }
  | { readonly _tag: 'Conflicting'; readonly cause?: Diagnostic.Identity }
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity }

/** One immutable position-query answer. */
export interface SemanticTarget {
  readonly _tag: 'SemanticTarget'
  readonly origin: SourceSpan.SourceSpan
  readonly resolution: Resolution
}

/** Deterministic semantic targets grouped by their source module. */
export interface Index {
  readonly _tag: 'SemanticTargetIndex'
  readonly modules: ReadonlyMap<string, ReadonlyArray<SemanticTarget>>
}

const identityOfDeclaration = (declaration: DeclarationIndex.MemberFact): Identity =>
  Object.freeze({
    _tag: 'DeclarationTarget',
    id:
      declaration.canonical._tag === 'Canonical'
        ? declaration.canonical.id
        : declaration.id,
  })

const locationOfDeclaration = (
  declaration: DeclarationIndex.MemberFact,
): DeclarationLocation | undefined =>
  declaration.name._tag === 'Present'
    ? Object.freeze({
        module: declaration.name.token.span.sourceId,
        span: declaration.syntax.span,
        selectionSpan: declaration.name.token.span,
      })
    : undefined

const locationOfParameter = (
  parameter: DeclarationIndex.ParameterFact,
): DeclarationLocation | undefined =>
  parameter.name._tag === 'Present'
    ? Object.freeze({
        module: parameter.name.token.span.sourceId,
        span: parameter.syntax.span,
        selectionSpan: parameter.name.token.span,
      })
    : undefined

const locationOfBinding = (
  binding: Elaboration.BindingDeclarationFact | Elaboration.PatternBindingFact,
): DeclarationLocation | undefined =>
  binding.name._tag === 'Present'
    ? Object.freeze({
        module: binding.name.token.span.sourceId,
        span: binding.syntax.span,
        selectionSpan: binding.name.token.span,
      })
    : undefined

const locationOfField = (field: DeclarationIndex.FieldFact): DeclarationLocation | undefined =>
  field.name._tag === 'Present'
    ? Object.freeze({
        module: field.name.token.span.sourceId,
        span: field.syntax.span,
        selectionSpan: field.name.token.span,
      })
    : undefined

const available = (
  target: Identity,
  declaration: DeclarationLocation | undefined,
): Resolution =>
  declaration === undefined
    ? Object.freeze({ _tag: 'Unavailable' })
    : Object.freeze({ _tag: 'Available', target, declaration })

const parameterResolution = (reference: Elaboration.ParameterReferenceFact): Resolution => {
  switch (reference._tag) {
    case 'Resolved':
      return available(
        Object.freeze({ _tag: 'ParameterTarget', id: reference.parameter.id }),
        locationOfParameter(reference.parameter),
      )
    case 'ResolvedBinding':
      return available(
        Object.freeze({ _tag: 'BindingTarget', id: reference.binding.id }),
        locationOfBinding(reference.binding),
      )
    case 'ResolvedPattern':
      return available(
        Object.freeze({ _tag: 'PatternBindingTarget', id: reference.binding.id }),
        locationOfBinding(reference.binding),
      )
    case 'Missing':
      return Object.freeze({
        _tag: 'Missing',
        ...(reference.cause === undefined ? {} : { cause: reference.cause }),
      })
    case 'Ambiguous':
      return Object.freeze({ _tag: 'Ambiguous' })
    case 'Unavailable':
      return Object.freeze({ _tag: 'Unavailable' })
  }
}

const callResolution = (reference: Elaboration.CallReferenceFact): Resolution => {
  switch (reference._tag) {
    case 'Resolved':
      return available(identityOfDeclaration(reference.declaration), locationOfDeclaration(reference.declaration))
    case 'Missing':
      return Object.freeze({
        _tag: 'Missing',
        ...(reference.cause === undefined ? {} : { cause: reference.cause }),
      })
    case 'Ambiguous':
      return Object.freeze({
        _tag: 'Ambiguous',
        ...(reference.cause === undefined ? {} : { cause: reference.cause }),
      })
    case 'ResolvedBuiltin':
    case 'Unavailable':
      return Object.freeze({ _tag: 'Unavailable' })
  }
}

const referenceSpan = (
  reference: Elaboration.ParameterReferenceFact | Elaboration.CallReferenceFact,
): SourceSpan.SourceSpan | undefined => ('token' in reference ? reference.token.span : undefined)

interface PendingTarget {
  readonly target: SemanticTarget
  readonly ordinal: number
}

const push = (
  pending: Array<PendingTarget>,
  origin: SourceSpan.SourceSpan | undefined,
  resolution: Resolution,
): void => {
  if (origin === undefined || origin.start === origin.end) return
  pending.push(
    Object.freeze({
      target: Object.freeze({ _tag: 'SemanticTarget', origin, resolution }),
      ordinal: pending.length,
    }),
  )
}

const directIdentifier = (syntax: SyntaxTree.Node): Token.Token | undefined =>
  SyntaxTree.tokens(syntax).find((token) => token.kind === 'Identifier')

const lastIdentifier = (syntax: SyntaxTree.Node): Token.Token | undefined =>
  SyntaxTree.tokens(syntax)
    .filter((token) => token.kind === 'Identifier')
    .at(-1)

const identifierTokens = (syntax: SyntaxTree.Element): ReadonlyArray<Token.Token> =>
  SyntaxTree.isToken(syntax)
    ? syntax.kind === 'Identifier'
      ? [syntax]
      : []
    : SyntaxTree.isNode(syntax)
      ? SyntaxTree.tokens(syntax).filter((token) => token.kind === 'Identifier')
      : []

const collectExpression = (
  expression: Elaboration.ExpressionFact,
  pending: Array<PendingTarget>,
): void => {
  switch (expression._tag) {
    case 'Identifier':
      push(pending, referenceSpan(expression.reference), parameterResolution(expression.reference))
      return
    case 'Call':
    case 'Operator':
      push(pending, referenceSpan(expression.reference), callResolution(expression.reference))
      for (const argument of expression.arguments) collectExpression(argument.expression, pending)
      return
    case 'Pipeline':
      collectExpression(expression.input, pending)
      push(pending, referenceSpan(expression.reference), callResolution(expression.reference))
      for (const argument of expression.arguments) collectExpression(argument.expression, pending)
      return
    case 'FieldProjection': {
      collectExpression(expression.subject, pending)
      const token = lastIdentifier(expression.syntax)
      const resolution: Resolution =
        expression.state._tag === 'Resolved'
          ? available(
              Object.freeze({ _tag: 'FieldTarget', id: expression.state.field.id }),
              locationOfField(expression.state.field),
            )
          : Object.freeze({
              _tag: 'Unavailable',
              ...(expression.state._tag === 'Unavailable' && expression.state.cause !== undefined
                ? { cause: expression.state.cause }
                : {}),
            })
      push(pending, token?.span, resolution)
      return
    }
    case 'StructLiteral': {
      const token = directIdentifier(expression.syntax)
      const resolution: Resolution =
        expression.target._tag === 'Resolved'
          ? available(
              identityOfDeclaration(expression.target.struct),
              locationOfDeclaration(expression.target.struct),
            )
          : Object.freeze({ _tag: 'Unavailable' })
      push(pending, token?.span, resolution)
      for (const initializer of expression.initializers) {
        const fieldToken = directIdentifier(initializer.syntax)
        const fieldResolution: Resolution =
          initializer.state._tag === 'Resolved'
            ? available(
                Object.freeze({ _tag: 'FieldTarget', id: initializer.state.field.id }),
                locationOfField(initializer.state.field),
              )
            : Object.freeze({ _tag: 'Unavailable' })
        push(pending, fieldToken?.span, fieldResolution)
        collectExpression(initializer.expression, pending)
      }
      return
    }
    case 'Move':
    case 'Borrow':
    case 'Run':
      collectExpression(expression.subject, pending)
      return
    case 'IndexProjection':
      collectExpression(expression.subject, pending)
      collectExpression(expression.index, pending)
      return
    case 'ArrayLiteral':
      for (const element of expression.elements) collectExpression(element.expression, pending)
      return
    case 'Grouped':
      collectExpression(expression.expression, pending)
      return
    case 'Match':
      collectExpression(expression.scrutinee, pending)
      for (const arm of expression.arms) {
        if (arm.guard !== undefined) collectExpression(arm.guard, pending)
        collectExpression(arm.result, pending)
      }
      return
    case 'EffectBlock':
      for (const statement of expression.statements) collectStatement(statement, pending)
      return
    case 'EffectCatch':
      collectExpression(expression.protected, pending)
      return
    case 'EffectRetry':
      collectExpression(expression.protected, pending)
      collectExpression(expression.retries, pending)
      return
    case 'EffectProvide':
      collectExpression(expression.protected, pending)
      return
    case 'EffectProvideWith':
      collectExpression(expression.protected, pending)
      collectExpression(expression.acquisition, pending)
      return
    case 'Integer':
    case 'Boolean':
      return
  }
}

const collectStatement = (
  statement: Elaboration.StatementFact,
  pending: Array<PendingTarget>,
): void => {
  switch (statement._tag) {
    case 'BindStatement':
      collectExpression(statement.binding.initializer, pending)
      return
    case 'ReturnStatement':
    case 'FailStatement':
    case 'DropStatement':
      collectExpression(statement.expression, pending)
      return
    case 'IfStatement':
      collectExpression(statement.condition, pending)
      for (const nested of statement.taken) collectStatement(nested, pending)
      for (const nested of statement.otherwise) collectStatement(nested, pending)
      return
    case 'WriteStatement':
      collectExpression(statement.destination, pending)
      collectExpression(statement.value, pending)
      return
    case 'WhileStatement':
      collectExpression(statement.condition, pending)
      for (const nested of statement.body) collectStatement(nested, pending)
      return
    case 'BreakStatement':
    case 'ContinueStatement':
      return
  }
}

const importResolution = (
  binding: NameResolution.Binding,
  index: DeclarationIndex.Index,
): Resolution => {
  if (binding._tag === 'ImportedMember') {
    const declaration = DeclarationIndex.byCanonical(index, binding.declaration)
    return declaration === undefined
      ? Object.freeze({ _tag: 'Unavailable' })
      : available(identityOfDeclaration(declaration), locationOfDeclaration(declaration))
  }
  if (binding._tag === 'Unavailable') {
    return Object.freeze({
      _tag: 'Unavailable',
      ...(binding.cause === undefined ? {} : { cause: binding.cause }),
    })
  }
  return Object.freeze({ _tag: 'Unavailable' })
}

/** Collects immutable position-query entries from recovered analysis facts. */
export const make = (
  results: ReadonlyMap<string, Elaboration.Result>,
  index: DeclarationIndex.Index,
  resolution: NameResolution.Resolution,
): Index => {
  const modules = new Map<string, ReadonlyArray<SemanticTarget>>()
  for (const [module, result] of results) {
    const pending: Array<PendingTarget> = []
    for (const fn of result.functions)
      for (const statement of fn.statements) collectStatement(statement, pending)
    const scope = resolution.modules.find((candidate) => candidate.module === module)
    for (const imported of scope?.imports ?? []) {
      if (imported._tag !== 'Available') continue
      for (const binding of imported.bindings) {
        if (binding._tag !== 'ImportedMember' && binding._tag !== 'Unavailable') continue
        for (const token of identifierTokens(binding.syntax)) {
          push(pending, token.span, importResolution(binding, index))
        }
      }
    }
    pending.sort(
      (left, right) =>
        left.target.origin.start - right.target.origin.start ||
        left.target.origin.end -
          left.target.origin.start -
          (right.target.origin.end - right.target.origin.start) ||
        left.ordinal - right.ordinal,
    )
    modules.set(
      module,
      Object.freeze(pending.map((entry) => entry.target)),
    )
  }
  return Object.freeze({ _tag: 'SemanticTargetIndex', modules })
}

/** Returns the deterministic smallest half-open target containing one byte offset. */
export const at = (
  self: Index,
  module: string,
  offset: number,
): SemanticTarget | undefined => {
  const candidates = (self.modules.get(module) ?? []).filter(
    (candidate) => candidate.origin.start <= offset && offset < candidate.origin.end,
  )
  return candidates.reduce<SemanticTarget | undefined>((best, candidate) => {
    if (best === undefined) return candidate
    const bestWidth = best.origin.end - best.origin.start
    const candidateWidth = candidate.origin.end - candidate.origin.start
    return candidateWidth < bestWidth ? candidate : best
  }, undefined)
}

/** Resolves one compiler identity to its declaration location without source spelling lookup. */
export const declarationLocation = (
  self: Index,
  identity: Identity,
): DeclarationLocation | undefined => {
  for (const targets of self.modules.values()) {
    for (const target of targets) {
      if (target.resolution._tag !== 'Available') continue
      if (target.resolution.target === identity) return target.resolution.declaration
      if (
        target.resolution.target._tag === identity._tag &&
        JSON.stringify(target.resolution.target.id) === JSON.stringify(identity.id)
      )
        return target.resolution.declaration
    }
  }
  return undefined
}
