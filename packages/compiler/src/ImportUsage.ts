import * as Option from 'effect/Option'
import type * as NameResolution from './NameResolution.js'
import * as SemanticOccurrence from './SemanticOccurrence.js'
import * as SourceAction from './SourceAction.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'

export interface UnusedBinding {
  readonly _tag: 'UnusedImportBinding'
  readonly spelling: string
  readonly span: SourceSpan.SourceSpan
  readonly declarationSpan: SourceSpan.SourceSpan
  readonly change?: SourceAction.ChangePlan
}

const text = (source: SourceFile.SourceFile, span: SourceSpan.SourceSpan): string | undefined =>
  Option.getOrUndefined(SourceFile.spelling(source, span))

const identityKey = (binding: NameResolution.Binding): string | undefined => {
  if (binding._tag === 'ModuleNamespace')
    return SemanticOccurrence.identityKey({
      _tag: 'ImportNamespaceIdentity',
      module: binding.module,
      spelling: binding.spelling,
    })
  if (binding._tag === 'ImportedMember')
    return SemanticOccurrence.identityKey({ _tag: 'DeclarationIdentity', id: binding.declaration })
  return undefined
}

const hasComment = (syntax: SyntaxFile.SyntaxFile, start: number, end: number): boolean =>
  syntax.tokens.some(
    (token) =>
      token.span.start >= start &&
      token.span.end <= end &&
      (token.kind === 'LineComment' ||
        token.kind === 'DocComment' ||
        token.kind === 'ModuleDocComment'),
  )

const lineSpan = (
  source: SourceFile.SourceFile,
  declaration: SyntaxTree.Node,
): SourceSpan.SourceSpan => {
  let start = declaration.span.start
  while (start > 0 && source.bytes[start - 1] !== 0x0a && source.bytes[start - 1] !== 0x0d)
    start -= 1
  let end = declaration.span.end
  while (
    end < source.bytes.length &&
    (source.bytes[end] === 0x20 || source.bytes[end] === 0x09)
  )
    end += 1
  if (source.bytes[end] === 0x0d) end += 1
  if (source.bytes[end] === 0x0a) end += 1
  return Option.getOrElse(SourceSpan.make(source, start, end), () => declaration.span)
}

const removal = (
  syntax: SyntaxFile.SyntaxFile,
  declaration: SyntaxTree.Node,
  member: SyntaxTree.Node | undefined,
): SourceAction.ChangePlan | undefined => {
  const members = SyntaxTree.directNodes(
    SyntaxTree.directNode(declaration, 'ImportMemberList') ?? declaration,
    'ImportMember',
  )
  let span: SourceSpan.SourceSpan
  if (member === undefined || members.length === 1) span = lineSpan(syntax.source, declaration)
  else {
    const ordinal = members.indexOf(member)
    if (ordinal < 0) return undefined
    const next = members.at(ordinal + 1)
    const previous = members.at(ordinal - 1)
    const start = next === undefined ? previous?.span.end : member.span.start
    const end = next?.span.start ?? member.span.end
    if (start === undefined) return undefined
    span = Option.getOrUndefined(SourceSpan.make(syntax.source, start, end)) ?? member.span
  }
  if (hasComment(syntax, span.start, span.end)) return undefined
  return Option.getOrUndefined(
    SourceAction.changePlan({
      preconditions: [SourceAction.precondition(syntax.source)],
      changes: [[syntax.source.id, [SourceAction.edit(span, '')]]],
    }),
  )
}

/** Classifies valid authored import bindings independently of their canonical declaration. */
export const unused = (
  syntax: SyntaxFile.SyntaxFile,
  scope: NameResolution.ModuleScope | undefined,
  occurrences: SemanticOccurrence.ModuleIndex | undefined,
): ReadonlyArray<UnusedBinding> => {
  if (scope === undefined || occurrences === undefined) return []
  const conflicted = new Set(scope.conflicts.flatMap((conflict) => conflict.bindings))
  const result: Array<UnusedBinding> = []
  for (const imported of scope.imports) {
    if (imported._tag !== 'Available') continue
    for (const binding of imported.bindings) {
      if (conflicted.has(binding)) continue
      const key = identityKey(binding)
      if (
        key === undefined ||
        (binding._tag !== 'ModuleNamespace' && binding._tag !== 'ImportedMember')
      )
        continue
      const used = occurrences.occurrences.some(
        (occurrence) =>
          occurrence.role !== 'Import' &&
          occurrence.resolution._tag === 'Available' &&
          SemanticOccurrence.identityKey(occurrence.resolution.identity) === key &&
          text(syntax.source, occurrence.span) === binding.spelling,
      )
      if (used) continue
      const declaration =
        binding._tag === 'ModuleNamespace' ? binding.syntax : imported.import.syntax
      const member = binding._tag === 'ImportedMember' ? binding.syntax : undefined
      const change = removal(syntax, declaration, member)
      result.push(
        Object.freeze({
          _tag: 'UnusedImportBinding',
          spelling: binding.spelling,
          span: binding._tag === 'ModuleNamespace' ? binding.token.span : binding.localToken.span,
          declarationSpan: declaration.span,
          ...(change === undefined ? {} : { change }),
        }),
      )
    }
  }
  return Object.freeze(result.sort((left, right) => left.span.start - right.span.start))
}
