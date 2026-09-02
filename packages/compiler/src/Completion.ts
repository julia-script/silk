import * as Option from 'effect/Option'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Elaboration from './Elaboration.js'
import * as Intrinsic from './Intrinsic.js'
import * as NameResolution from './NameResolution.js'
import type * as Presentation from './Presentation.js'
import * as PresentationRenderer from './Presentation.js'
import * as Scalar from './Scalar.js'
import type * as SemanticOccurrence from './SemanticOccurrence.js'
import * as SourceFile from './SourceFile.js'
import type * as SourceSpan from './SourceSpan.js'
import * as SourceSpanFactory from './SourceSpan.js'
import * as Type from './Type.js'

export type Context =
  | { readonly _tag: 'ExpressionContext' }
  | { readonly _tag: 'DeclaredTypeContext' }
  | { readonly _tag: 'TypeArgumentContext' }
  | { readonly _tag: 'ActorMemberContext'; readonly actor: string }
  | {
      readonly _tag: 'ValueMemberContext'
      readonly state: 'Available' | 'Missing' | 'Ambiguous' | 'Unavailable'
    }
  | { readonly _tag: 'UnavailableContext' }

export type Kind =
  | 'Binding'
  | 'Parameter'
  | 'Function'
  | 'Method'
  | 'AssociatedFunction'
  | 'Constant'
  | 'Constructor'
  | 'Type'
  | 'Field'
  | 'Actor'
  | 'Operation'
  | 'Keyword'

export type CandidateIdentity =
  | { readonly _tag: 'SemanticCandidate'; readonly identity: SemanticOccurrence.Identity }
  | { readonly _tag: 'SyntaxCandidate'; readonly spelling: string }

export interface Candidate {
  readonly _tag: 'CompletionCandidate'
  readonly identity: CandidateIdentity
  readonly kind: Kind
  readonly label: string
  readonly insertText: string
  readonly detail?: Presentation.Presentation
  readonly sortGroup: number
}

/** One deterministic completion answer with an exact byte replacement span. */
export interface Result {
  readonly _tag: 'CompletionResult'
  readonly context: Context
  readonly replacement: SourceSpan.SourceSpan
  readonly candidates: ReadonlyArray<Candidate>
}

const decoder = new TextDecoder()

const isIdentifierByte = (byte: number | undefined): boolean =>
  byte !== undefined &&
  ((byte >= 0x41 && byte <= 0x5a) ||
    (byte >= 0x61 && byte <= 0x7a) ||
    (byte >= 0x30 && byte <= 0x39) ||
    byte === 0x5f)

const replacementSpan = (
  source: SourceFile.SourceFile,
  offset: number,
): { readonly span: SourceSpan.SourceSpan; readonly start: number } => {
  const bytes = SourceFile.toUint8Array(source)
  const end = Math.max(0, Math.min(offset, bytes.length))
  let start = end
  while (start > 0 && isIdentifierByte(bytes.at(start - 1))) start -= 1
  return {
    span: Option.getOrThrowWith(
      SourceSpanFactory.make(source, start, end),
      () => new RangeError('Completion replacement escaped its source snapshot'),
    ),
    start,
  }
}

const declarationIdentity = (
  declaration: DeclarationFacts.MemberFact,
): SemanticOccurrence.Identity =>
  Object.freeze({
    _tag: 'DeclarationIdentity',
    id: declaration.canonical._tag === 'Canonical' ? declaration.canonical.id : declaration.id,
  })

const declarationKind = (
  declaration: DeclarationFacts.MemberFact,
  aggregateKind: 'Type' | 'Constructor',
): Kind => {
  switch (declaration._tag) {
    case 'FunctionDeclaration':
      return 'Function'
    case 'ConstantDeclaration':
      return 'Constant'
    case 'ServiceDeclaration':
    case 'InterfaceDeclaration':
    case 'RoleDeclaration':
    case 'EnumDeclaration':
    case 'UnionDeclaration':
    case 'AliasDeclaration':
      return 'Type'
    case 'StructDeclaration':
      return aggregateKind
  }
}

const declarationDetail = (declaration: DeclarationFacts.MemberFact): Presentation.Presentation => {
  switch (declaration._tag) {
    case 'FunctionDeclaration':
      return PresentationRenderer.functionDeclaration(declaration)
    case 'ConstantDeclaration':
      return PresentationRenderer.constantDeclaration(declaration)
    case 'ServiceDeclaration':
    case 'InterfaceDeclaration':
      return PresentationRenderer.serviceDeclaration(declaration)
    case 'RoleDeclaration':
      return PresentationRenderer.roleDeclaration(declaration)
    case 'EnumDeclaration':
      return PresentationRenderer.enumDeclaration(declaration)
    case 'StructDeclaration':
      return PresentationRenderer.structDeclaration(declaration)
    case 'UnionDeclaration':
      return PresentationRenderer.unionDeclaration(declaration)
    case 'AliasDeclaration':
      return PresentationRenderer.aliasDeclaration(declaration)
  }
}

const compareText = (left: string, right: string): number => {
  if (left < right) return -1
  if (left > right) return 1
  return 0
}

const semantic = (identity: SemanticOccurrence.Identity): CandidateIdentity =>
  Object.freeze({ _tag: 'SemanticCandidate', identity })

const syntax = (spelling: string): CandidateIdentity =>
  Object.freeze({ _tag: 'SyntaxCandidate', spelling })

const candidate = (options: {
  readonly identity: CandidateIdentity
  readonly kind: Kind
  readonly label: string
  readonly detail?: Presentation.Presentation
  readonly sortGroup: number
}): Candidate =>
  Object.freeze({
    _tag: 'CompletionCandidate',
    identity: options.identity,
    kind: options.kind,
    label: options.label,
    insertText: options.label,
    ...(options.detail === undefined ? {} : { detail: options.detail }),
    sortGroup: options.sortGroup,
  })

const enclosingFunction = (
  result: Elaboration.Result,
  offset: number,
): Elaboration.FunctionFact | undefined =>
  result.functions.find(
    (fn) => fn.declaration.syntax.span.start <= offset && offset <= fn.declaration.syntax.span.end,
  )

const sameDeclaration = (
  left: Elaboration.DeclarationId,
  right: Elaboration.DeclarationId,
): boolean => left.sourceId === right.sourceId && left.ordinal === right.ordinal

const scopeChain = (
  result: Elaboration.Result,
  fn: Elaboration.FunctionFact | undefined,
  offset: number,
): ReadonlyArray<Elaboration.LexicalScopeFact> => {
  if (fn === undefined) return Object.freeze([])
  const candidates = result.lexicalScopes
    .filter(
      (scope) =>
        sameDeclaration(scope.id.function, fn.declaration.id) &&
        scope.span.start <= offset &&
        offset <= scope.span.end,
    )
    .sort(
      (left, right) =>
        left.span.end - left.span.start - (right.span.end - right.span.start) ||
        right.id.ordinal - left.id.ordinal,
    )
  const byOrdinal = new Map(
    result.lexicalScopes
      .filter((scope) => sameDeclaration(scope.id.function, fn.declaration.id))
      .map((scope) => [scope.id.ordinal, scope] as const),
  )
  const chain: Array<Elaboration.LexicalScopeFact> = []
  let current = candidates.at(0)
  while (current !== undefined) {
    chain.push(current)
    current = current.parent === undefined ? undefined : byOrdinal.get(current.parent.ordinal)
  }
  return Object.freeze(chain)
}

const visibleBindings = (
  result: Elaboration.Result,
  fn: Elaboration.FunctionFact | undefined,
  offset: number,
): ReadonlyArray<Elaboration.BindingDeclarationFact> => {
  const selected = new Map<string, Elaboration.BindingDeclarationFact>()
  for (const scope of scopeChain(result, fn, offset))
    for (const binding of scope.bindings.toReversed())
      if (
        binding.name._tag === 'Present' &&
        binding.name.token.span.start < offset &&
        !selected.has(binding.name.spelling)
      )
        selected.set(binding.name.spelling, binding)
  return Object.freeze(
    [...selected.values()].sort((left, right) => left.syntax.span.start - right.syntax.span.start),
  )
}

const visiblePatternBindings = (
  result: Elaboration.Result,
  fn: Elaboration.FunctionFact | undefined,
  offset: number,
): ReadonlyArray<Elaboration.PatternBindingFact> => {
  const selected = new Map<string, Elaboration.PatternBindingFact>()
  for (const scope of scopeChain(result, fn, offset))
    for (const binding of scope.patternBindings.toReversed())
      if (
        binding.name._tag === 'Present' &&
        binding.name.token.span.start <= offset &&
        !selected.has(binding.name.spelling)
      )
        selected.set(binding.name.spelling, binding)
  return Object.freeze([...selected.values()])
}

const actorCandidates = (actor: Intrinsic.Actor): ReadonlyArray<Candidate> =>
  actor.operations.map((operation) =>
    candidate({
      identity: semantic(Object.freeze({ _tag: 'IntrinsicOperationIdentity', id: operation.id })),
      kind: 'Operation',
      label: operation.spelling,
      detail: PresentationRenderer.intrinsicOperation(operation),
      sortGroup: 0,
    }),
  )

const enumCandidates = (enum_: DeclarationFacts.EnumFact): ReadonlyArray<Candidate> =>
  Object.freeze([
    ...enum_.members.flatMap((member): ReadonlyArray<Candidate> =>
      member.name._tag !== 'Present' || member.canonical._tag !== 'Canonical'
        ? []
        : [
            candidate({
              identity: semantic(
                Object.freeze({ _tag: 'EnumMemberIdentity', id: member.canonical.id }),
              ),
              kind: 'Constant',
              label: member.name.spelling,
              detail: PresentationRenderer.enumMember(enum_, member),
              sortGroup: 0,
            }),
          ],
    ),
    ...enum_.associatedOperations.map((operation) =>
      candidate({
        identity: semantic(
          Object.freeze({ _tag: 'EnumAssociatedOperationIdentity', id: operation.id }),
        ),
        kind: 'Operation',
        label: operation.name,
        detail: PresentationRenderer.enumAssociatedOperation(operation),
        sortGroup: 1,
      }),
    ),
  ])

const unionCandidates = (union: DeclarationFacts.UnionFact): ReadonlyArray<Candidate> =>
  Object.freeze(
    union.variants.flatMap((variant): ReadonlyArray<Candidate> =>
      variant.name._tag !== 'Present' || variant.canonical._tag !== 'Canonical'
        ? []
        : [
            candidate({
              identity: semantic(
                Object.freeze({ _tag: 'UnionVariantIdentity', id: variant.canonical.id }),
              ),
              kind: 'Constructor',
              label: variant.name.spelling,
              detail: PresentationRenderer.unionVariant(union, variant),
              sortGroup: 0,
            }),
          ],
    ),
  )

const serviceCandidates = (service: DeclarationFacts.ServiceFact): ReadonlyArray<Candidate> =>
  Object.freeze(
    service.operations.flatMap((operation): ReadonlyArray<Candidate> =>
      operation.name._tag !== 'Present' || operation.state._tag !== 'Unique'
        ? []
        : [
            candidate({
              identity: semantic(
                Object.freeze({ _tag: 'ServiceOperationIdentity', id: operation.state.id }),
              ),
              kind: 'Operation',
              label: operation.name.spelling,
              detail: PresentationRenderer.serviceOperation(operation),
              sortGroup: 0,
            }),
          ],
    ),
  )

/**
 * The owner's accessible inherent members, labeled `Method` when parameter zero is the receiver
 * and `AssociatedFunction` otherwise. Visibility and duplicate rules are the resolver's.
 */
const inherentCandidates = (
  index: DeclarationIndex.Index,
  owner: DeclarationFacts.MemberFact,
  module: string,
): ReadonlyArray<Candidate> => {
  if (owner.canonical._tag !== 'Canonical') return Object.freeze([])
  const ownerId = owner.canonical.id
  const names = new Set<string>()
  for (const member of index.modules.find((headers) => headers.module === ownerId.module)
    ?.members ?? []) {
    const associated = member._tag === 'FunctionDeclaration' ? member.associatedMember : undefined
    if (associated === undefined) continue
    if ((associated.owner?.name ?? associated.ownerSpelling) === ownerId.name)
      names.add(associated.name)
  }
  return Object.freeze(
    [...names].flatMap((name): ReadonlyArray<Candidate> => {
      const lookup = NameResolution.lookupAssociated(index, owner, name, module)
      if (lookup._tag !== 'Inherent' || lookup.declaration._tag !== 'FunctionDeclaration') return []
      return [
        candidate({
          identity: semantic(declarationIdentity(lookup.declaration)),
          kind: lookup.declaration.associatedMember?.receiver ? 'Method' : 'AssociatedFunction',
          label: name,
          detail: PresentationRenderer.functionDeclaration(lookup.declaration),
          sortGroup: 1,
        }),
      ]
    }),
  )
}

const namespaceCandidates = (
  index: DeclarationIndex.Index,
  module: string,
  sortGroup = 0,
): ReadonlyArray<Candidate> =>
  (index.modules.find((headers) => headers.module === module)?.members ?? []).flatMap(
    (declaration) => {
      if (
        declaration.visibility !== 'Public' ||
        declaration.name._tag !== 'Present' ||
        (declaration._tag === 'FunctionDeclaration' && declaration.associatedMember !== undefined)
      ) {
        return []
      }
      return [
        candidate({
          identity: semantic(declarationIdentity(declaration)),
          kind: declarationKind(declaration, 'Type'),
          label: declaration.name.spelling,
          detail: declarationDetail(declaration),
          sortGroup,
        }),
      ]
    },
  )

interface ValueLookup {
  readonly found: boolean
  readonly type?: Type.Type
}

const valueLookup = (
  spelling: string,
  result: Elaboration.Result,
  fn: Elaboration.FunctionFact | undefined,
  offset: number,
): ValueLookup => {
  const binding = visibleBindings(result, fn, offset).find(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === spelling,
  )
  if (binding !== undefined)
    return Object.freeze({
      found: true,
      ...(binding.inferredType._tag === 'Available' ? { type: binding.inferredType.type } : {}),
    })
  const patternBinding = visiblePatternBindings(result, fn, offset).find(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === spelling,
  )
  if (patternBinding !== undefined)
    return Object.freeze({
      found: true,
      ...(patternBinding.type._tag === 'Available' ? { type: patternBinding.type.type } : {}),
    })
  const parameter = fn?.declaration.parameters.find(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === spelling,
  )
  if (parameter !== undefined)
    return Object.freeze({
      found: true,
      ...(parameter.declaredType._tag === 'Resolved' ? { type: parameter.declaredType.type } : {}),
    })
  return Object.freeze({ found: false })
}

const nominalSubject = (type: Type.Type | undefined): Type.Nominal | undefined => {
  if (type === undefined) return undefined
  if (Type.isNominal(type)) return type
  if (Type.isReference(type)) return nominalSubject(type.target)
  return undefined
}

const fieldCandidates = (
  index: DeclarationIndex.Index,
  type: Type.Nominal | undefined,
  module: string,
): ReadonlyArray<Candidate> => {
  if (type === undefined) return Object.freeze([])
  const resolved = DeclarationFacts.byCanonical(
    index,
    Object.freeze({
      _tag: 'CanonicalDeclarationId',
      module: type.module,
      name: type.name,
    }),
  )
  const declaration = resolved?._tag === 'StructDeclaration' ? resolved : undefined
  return Object.freeze(
    (declaration?.fields ?? []).flatMap((field) =>
      field.name._tag !== 'Present' || (field.visibility === 'Private' && type.module !== module)
        ? []
        : [
            candidate({
              identity: semantic(Object.freeze({ _tag: 'FieldIdentity', id: field.id })),
              kind: 'Field',
              label: field.name.spelling,
              detail: PresentationRenderer.field(field),
              sortGroup: 0,
            }),
          ],
    ),
  )
}

const typeCandidates = (
  module: string,
  index: DeclarationIndex.Index,
  scope: NameResolution.ModuleScope | undefined,
  fn: Elaboration.FunctionFact | undefined,
): ReadonlyArray<Candidate> => {
  const candidates: Array<Candidate> = [
    candidate({
      identity: syntax('Effect'),
      kind: 'Type',
      label: 'Effect',
      detail: Object.freeze({
        _tag: 'IntrinsicActorPresentation',
        name: 'Effect',
        text: 'builtin type Effect<A ! E ? R>',
      }),
      sortGroup: 1,
    }),
  ]
  for (const scalar of Scalar.all())
    candidates.push(
      candidate({
        identity: syntax(scalar.spelling),
        kind: 'Type',
        label: scalar.spelling,
        detail: Object.freeze({
          _tag: 'IntrinsicActorPresentation',
          name: scalar.spelling,
          text: `builtin type ${scalar.spelling}`,
        }),
        sortGroup: 1,
      }),
    )
  for (const actor of Intrinsic.all())
    if (actor.kind === 'Type')
      candidates.push(
        candidate({
          identity: semantic(Object.freeze({ _tag: 'IntrinsicActorIdentity', id: actor.id })),
          kind: 'Type',
          label: actor.spelling,
          detail: PresentationRenderer.intrinsicActor(actor),
          sortGroup: 1,
        }),
      )
  for (const declaration of [
    ...(index.modules.find((headers) => headers.module === module)?.structs ?? []),
    ...(index.modules.find((headers) => headers.module === module)?.unions ?? []),
    ...(index.modules.find((headers) => headers.module === module)?.enums ?? []),
    ...(index.modules.find((headers) => headers.module === module)?.services ?? []),
    ...(index.modules.find((headers) => headers.module === module)?.interfaces ?? []),
    ...(index.modules
      .find((headers) => headers.module === module)
      ?.members.filter(
        (member): member is DeclarationFacts.AliasFact => member._tag === 'AliasDeclaration',
      ) ?? []),
  ])
    if (declaration.name._tag === 'Present')
      candidates.push(
        candidate({
          identity: semantic(declarationIdentity(declaration)),
          kind: 'Type',
          label: declaration.name.spelling,
          detail: declarationDetail(declaration),
          sortGroup: 0,
        }),
      )
  for (const parameter of fn?.declaration.typeParameters ?? [])
    if (parameter.name._tag === 'Present')
      candidates.push(
        candidate({
          identity: semantic(Object.freeze({ _tag: 'TypeParameterIdentity', id: parameter.type })),
          kind: 'Type',
          label: parameter.name.spelling,
          detail: PresentationRenderer.typeParameter(parameter),
          sortGroup: 0,
        }),
      )
  for (const binding of scope?.bindings ?? []) {
    if (binding._tag !== 'ImportedMember') continue
    const declaration = DeclarationFacts.byCanonical(index, binding.declaration)
    if (
      declaration?._tag !== 'StructDeclaration' &&
      declaration?._tag !== 'UnionDeclaration' &&
      declaration?._tag !== 'EnumDeclaration' &&
      declaration?._tag !== 'ServiceDeclaration' &&
      declaration?._tag !== 'InterfaceDeclaration' &&
      declaration?._tag !== 'AliasDeclaration'
    )
      continue
    candidates.push(
      candidate({
        identity: semantic(declarationIdentity(declaration)),
        kind: 'Type',
        label: binding.spelling,
        detail: declarationDetail(declaration),
        sortGroup: 2,
      }),
    )
  }
  return Object.freeze(candidates)
}

const expressionCandidates = (
  module: string,
  index: DeclarationIndex.Index,
  scope: NameResolution.ModuleScope | undefined,
  result: Elaboration.Result,
  fn: Elaboration.FunctionFact | undefined,
  offset: number,
): ReadonlyArray<Candidate> => {
  const candidates: Array<Candidate> = []
  for (const binding of visibleBindings(result, fn, offset))
    if (binding.name._tag === 'Present')
      candidates.push(
        candidate(
          (() => {
            const detail = PresentationRenderer.binding(binding, module, scope)
            return {
              identity: semantic(Object.freeze({ _tag: 'BindingIdentity', id: binding.id })),
              kind: 'Binding',
              label: binding.name.spelling,
              ...(detail === undefined ? {} : { detail }),
              sortGroup: 0,
            }
          })(),
        ),
      )
  for (const binding of visiblePatternBindings(result, fn, offset))
    if (binding.name._tag === 'Present') {
      const detail = PresentationRenderer.patternBinding(binding, module, scope)
      candidates.push(
        candidate({
          identity: semantic(Object.freeze({ _tag: 'PatternBindingIdentity', id: binding.id })),
          kind: 'Binding',
          label: binding.name.spelling,
          ...(detail === undefined ? {} : { detail }),
          sortGroup: 0,
        }),
      )
    }
  for (const parameter of fn?.declaration.parameters ?? [])
    if (parameter.name._tag === 'Present')
      candidates.push(
        candidate({
          identity: semantic(Object.freeze({ _tag: 'ParameterIdentity', id: parameter.id })),
          kind: 'Parameter',
          label: parameter.name.spelling,
          detail: PresentationRenderer.parameter(parameter),
          sortGroup: 1,
        }),
      )
  for (const declaration of index.modules.find((headers) => headers.module === module)?.members ??
    [])
    if (declaration.name._tag === 'Present')
      candidates.push(
        candidate({
          identity: semantic(declarationIdentity(declaration)),
          kind: declarationKind(declaration, 'Constructor'),
          label: declaration.name.spelling,
          detail: declarationDetail(declaration),
          sortGroup: 2,
        }),
      )
  for (const binding of scope?.bindings ?? []) {
    if (binding._tag === 'ImportedMember') {
      const declaration = DeclarationFacts.byCanonical(index, binding.declaration)
      if (declaration === undefined) continue
      candidates.push(
        candidate({
          identity: semantic(declarationIdentity(declaration)),
          kind: declarationKind(declaration, 'Constructor'),
          label: binding.spelling,
          detail: declarationDetail(declaration),
          sortGroup: 3,
        }),
      )
    } else if (binding._tag === 'ModuleNamespace')
      candidates.push(
        candidate({
          identity: semantic(
            Object.freeze({
              _tag: 'ImportNamespaceIdentity',
              module: binding.module,
              spelling: binding.spelling,
            }),
          ),
          kind: 'Actor',
          label: binding.spelling,
          detail: PresentationRenderer.importBinding(binding.spelling, binding.module),
          sortGroup: 3,
        }),
      )
  }
  for (const actor of Intrinsic.all())
    candidates.push(
      candidate({
        identity: semantic(Object.freeze({ _tag: 'IntrinsicActorIdentity', id: actor.id })),
        kind: actor.kind === 'Type' ? 'Constructor' : 'Actor',
        label: actor.spelling,
        detail: PresentationRenderer.intrinsicActor(actor),
        sortGroup: 4,
      }),
    )
  for (const keyword of ['true', 'false', 'effect', 'move', 'run', 'match'])
    candidates.push(
      candidate({ identity: syntax(keyword), kind: 'Keyword', label: keyword, sortGroup: 5 }),
    )
  return Object.freeze(candidates)
}

const candidateKey = (self: Candidate): string =>
  self.identity._tag === 'SyntaxCandidate'
    ? `syntax:${self.identity.spelling}`
    : (() => {
        const identity = self.identity.identity
        if (identity._tag === 'DeclarationIdentity')
          return identity.id._tag === 'CanonicalDeclarationId'
            ? `declaration:${identity.id.module}.${identity.id.name}`
            : `declaration:${identity.id.sourceId}:${identity.id.ordinal}`
        if (identity._tag === 'IntrinsicActorIdentity') return `actor:${identity.id.name}`
        if (identity._tag === 'IntrinsicOperationIdentity')
          return `operation:${identity.id.actor}.${identity.id.name}`
        return `${identity._tag}:${self.label}`
      })()

const stable = (inputs: ReadonlyArray<Candidate>): ReadonlyArray<Candidate> => {
  const unique = new Map<string, Candidate>()
  for (const input of inputs)
    if (!unique.has(candidateKey(input))) unique.set(candidateKey(input), input)
  return Object.freeze(
    [...unique.values()].sort(
      (left, right) =>
        left.sortGroup - right.sortGroup ||
        compareText(left.kind, right.kind) ||
        compareText(left.label, right.label),
    ),
  )
}

/** Computes recovery-aware semantic completion from one exact source snapshot and byte offset. */
export const complete = (options: {
  readonly source: SourceFile.SourceFile
  readonly module: string
  readonly offset: number
  readonly index: DeclarationIndex.Index
  readonly resolution: NameResolution.Resolution
  readonly result: Elaboration.Result
}): Result => {
  const replacement = replacementSpan(options.source, options.offset)
  const bytes = SourceFile.toUint8Array(options.source)
  const before = decoder.decode(bytes.subarray(0, replacement.start))
  const scope = NameResolution.scopeOf(options.resolution, options.module)
  const fn = enclosingFunction(options.result, options.offset)
  const memberMatch = before.match(/([A-Za-z_][A-Za-z0-9_]*)\.\s*$/)
  if (memberMatch !== null) {
    const qualifier = memberMatch[1]
    const value =
      qualifier === undefined
        ? Object.freeze({ found: false })
        : valueLookup(qualifier, options.result, fn, options.offset)
    if (value.found) {
      const subject = nominalSubject(value.type)
      return Object.freeze({
        _tag: 'CompletionResult',
        context: Object.freeze({
          _tag: 'ValueMemberContext',
          state: subject === undefined ? 'Unavailable' : 'Available',
        }),
        replacement: replacement.span,
        candidates: stable(fieldCandidates(options.index, subject, options.module)),
      })
    }
    const lookup =
      qualifier === undefined || scope === undefined
        ? undefined
        : NameResolution.lookup(scope, options.index, qualifier)
    if (lookup?._tag === 'Intrinsic') {
      const intrinsic = Intrinsic.findActor(lookup.actor)
      if (intrinsic !== undefined)
        return Object.freeze({
          _tag: 'CompletionResult',
          context: Object.freeze({ _tag: 'ActorMemberContext', actor: intrinsic.spelling }),
          replacement: replacement.span,
          candidates: stable([...actorCandidates(intrinsic)]),
        })
    }
    if (lookup?._tag === 'Namespace')
      return Object.freeze({
        _tag: 'CompletionResult',
        context: Object.freeze({ _tag: 'ActorMemberContext', actor: lookup.module }),
        replacement: replacement.span,
        candidates: stable(namespaceCandidates(options.index, lookup.module)),
      })
    // Declared inherent members are the only associated candidates of a nominal qualifier.
    const inherent =
      lookup?._tag === 'Resolved'
        ? inherentCandidates(
            options.index,
            NameResolution.erasedOwner(options.index, lookup.declaration),
            options.module,
          )
        : Object.freeze([])
    // A transparent alias qualifier offers the aliased owner's members and nothing of its own.
    if (
      lookup?._tag === 'Resolved' &&
      lookup.declaration._tag === 'AliasDeclaration' &&
      inherent.length > 0
    )
      return Object.freeze({
        _tag: 'CompletionResult',
        context: Object.freeze({ _tag: 'ActorMemberContext', actor: lookup.spelling }),
        replacement: replacement.span,
        candidates: stable(inherent),
      })
    if (lookup?._tag === 'Resolved' && lookup.declaration._tag === 'EnumDeclaration')
      return Object.freeze({
        _tag: 'CompletionResult',
        context: Object.freeze({
          _tag: 'ActorMemberContext',
          actor:
            lookup.declaration.name._tag === 'Present'
              ? lookup.declaration.name.spelling
              : (qualifier ?? 'enum'),
        }),
        replacement: replacement.span,
        candidates: stable([...enumCandidates(lookup.declaration), ...inherent]),
      })
    if (lookup?._tag === 'Resolved' && lookup.declaration._tag === 'UnionDeclaration') {
      return Object.freeze({
        _tag: 'CompletionResult',
        context: Object.freeze({
          _tag: 'ActorMemberContext',
          actor:
            lookup.declaration.name._tag === 'Present'
              ? lookup.declaration.name.spelling
              : (qualifier ?? 'union'),
        }),
        replacement: replacement.span,
        candidates: stable([...unionCandidates(lookup.declaration), ...inherent]),
      })
    }
    if (
      lookup?._tag === 'Resolved' &&
      (lookup.declaration._tag === 'StructDeclaration' ||
        lookup.declaration._tag === 'InterfaceDeclaration') &&
      lookup.declaration.canonical._tag === 'Canonical'
    ) {
      return Object.freeze({
        _tag: 'CompletionResult',
        context: Object.freeze({
          _tag: 'ActorMemberContext',
          actor: lookup.declaration.canonical.id.name,
        }),
        replacement: replacement.span,
        candidates: stable(inherent),
      })
    }
    if (lookup?._tag === 'Resolved' && lookup.declaration._tag === 'ServiceDeclaration') {
      return Object.freeze({
        _tag: 'CompletionResult',
        context: Object.freeze({
          _tag: 'ActorMemberContext',
          actor:
            lookup.declaration.name._tag === 'Present'
              ? lookup.declaration.name.spelling
              : (qualifier ?? 'service'),
        }),
        replacement: replacement.span,
        candidates: stable([...serviceCandidates(lookup.declaration), ...inherent]),
      })
    }
    let state: 'Ambiguous' | 'Missing' | 'Unavailable'
    if (lookup?._tag === 'Conflict') state = 'Ambiguous'
    else if (lookup?._tag === 'Missing' || lookup === undefined) state = 'Missing'
    else state = 'Unavailable'
    return Object.freeze({
      _tag: 'CompletionResult',
      context: Object.freeze({
        _tag: 'ValueMemberContext',
        state,
      }),
      replacement: replacement.span,
      candidates: Object.freeze([]),
    })
  }
  const tail = before.slice(Math.max(0, before.lastIndexOf('\n')))
  const typeArgument = /<[^>]*$/.test(tail)
  const declaredType = /(?::|->)\s*[A-Za-z0-9_.]*$/.test(tail)
  if (typeArgument || declaredType)
    return Object.freeze({
      _tag: 'CompletionResult',
      context: Object.freeze({
        _tag: typeArgument ? 'TypeArgumentContext' : 'DeclaredTypeContext',
      }),
      replacement: replacement.span,
      candidates: stable(typeCandidates(options.module, options.index, scope, fn)),
    })
  return Object.freeze({
    _tag: 'CompletionResult',
    context: Object.freeze({ _tag: 'ExpressionContext' }),
    replacement: replacement.span,
    candidates: stable(
      expressionCandidates(
        options.module,
        options.index,
        scope,
        options.result,
        fn,
        options.offset,
      ),
    ),
  })
}
