import * as Option from 'effect/Option'
import type * as DeclarationLifetime from './DeclarationLifetime.js'
import * as Lifetime from './Lifetime.js'
import * as SourceAction from './SourceAction.js'
import * as SourceSpan from './SourceSpan.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'

/** A complete header expansion planned against one immutable analyzed source. */
export interface LifetimeElision {
  readonly descriptor: SourceAction.Descriptor
  readonly plan: SourceAction.ChangePlan
}

const significant = (token: Token.Token): boolean =>
  !['Whitespace', 'LineComment', 'DocComment', 'ModuleDocComment', 'EndOfFile'].includes(token.kind)

/** Names omitted regions without changing bodies, comments, or authored lifetime choices. */
export const makeExplicit = (
  self: DeclarationLifetime.Context,
  executable?: Type.ExecutableLifetimes,
): Option.Option<LifetimeElision> => {
  if (self.diagnostics.length > 0) return Option.none()
  const insertions = new Map<number, string>()
  const insert = (offset: number, text: string, prepend = false): void => {
    const existing = insertions.get(offset) ?? ''
    insertions.set(offset, prepend ? `${text}${existing}` : `${existing}${text}`)
  }
  const listBinders = (node: SyntaxTree.Node, names: ReadonlyArray<string>): boolean => {
    if (names.length === 0) return true
    const list = SyntaxTree.directNode(node, 'TypeParameterList')
    if (list !== undefined) {
      const close = SyntaxTree.directToken(list, 'Greater')
      if (close === undefined) return false
      insert(
        close.span.start,
        `${list.children.some(SyntaxTree.isNode) ? ', ' : ''}${names.join(', ')}`,
      )
      return true
    }
    const name = SyntaxTree.directToken(node, 'Identifier')
    if (name === undefined) return false
    insert(name.span.end, `<${names.join(', ')}>`)
    return true
  }
  const effectKeyword = SyntaxTree.directToken(self.syntax, 'EffectKeyword')
  if (
    executable !== undefined &&
    effectKeyword !== undefined &&
    self.explicitEnvironment === undefined
  )
    insert(effectKeyword.span.end, `<${Lifetime.display(executable.environment)}>`)
  const requiredBounds = new Map<string, Array<Lifetime.Lifetime>>()
  for (const bound of executable?.lifetimeBounds ?? []) {
    const key = Lifetime.key(bound.longer)
    requiredBounds.set(key, [...(requiredBounds.get(key) ?? []), bound.shorter])
  }
  for (const bound of executable?.typeOutlives ?? []) {
    const key = Type.key(bound.type)
    requiredBounds.set(key, [...(requiredBounds.get(key) ?? []), bound.lifetime])
  }
  const boundNames = (key: string): ReadonlyArray<string> => [
    ...new Set((requiredBounds.get(key) ?? []).map(Lifetime.display)),
  ]
  const parameterName = (parameter: Type.Parameter): string => {
    const bounds = boundNames(Type.key(parameter))
    return `${parameter.name}${bounds.length === 0 ? '' : `: ${bounds.join(' + ')}`}`
  }
  const declaredKeys = new Set([
    ...[...self.parameters.values()].map(Type.key),
    ...self.implicit.map((binder) => Type.key(binder.parameter)),
  ])
  const generated = (executable?.lifetimeBinders ?? []).filter(
    (binder) => !declaredKeys.has(Lifetime.key(binder)),
  )
  if (
    !listBinders(self.syntax, [
      ...self.implicit.map((binder) => parameterName(binder.parameter)),
      ...generated.map((binder) => {
        const bounds = boundNames(Lifetime.key(binder))
        return `${Lifetime.display(binder)}${bounds.length === 0 ? '' : `: ${bounds.join(' + ')}`}`
      }),
    ])
  )
    return Option.none()
  const parameterList = SyntaxTree.directNode(self.syntax, 'TypeParameterList')
  for (const node of parameterList?.children ?? []) {
    if (!SyntaxTree.isNode(node) || !['TypeParameter', 'LifetimeParameter'].includes(node.kind))
      continue
    const tokens = SyntaxTree.tokens(node).filter(significant)
    const name = tokens.at(0)
    const last = tokens.at(-1)
    if (name === undefined || last === undefined) return Option.none()
    const spelling = String.fromCharCode(...self.source.bytes.slice(name.span.start, name.span.end))
    const parameter = self.parameters.get(spelling)
    if (parameter === undefined) continue
    const authored = new Set(
      tokens
        .slice(1)
        .filter((token) => token.kind === 'Lifetime')
        .map((token) =>
          String.fromCharCode(...self.source.bytes.slice(token.span.start, token.span.end)),
        ),
    )
    const missing = boundNames(Type.key(parameter)).filter((name) => !authored.has(name))
    if (missing.length > 0)
      insert(
        last.span.end,
        `${tokens.some((token) => token.kind === 'Colon') ? ' + ' : ': '}${missing.join(' + ')}`,
        true,
      )
  }
  for (const [node, lifetime] of self.regions) {
    if (node.kind === 'ReferenceType' || node.kind === 'SliceType') {
      if (SyntaxTree.directToken(node, 'Lifetime') !== undefined) continue
      const ampersand = SyntaxTree.directToken(node, 'Ampersand')
      if (ampersand === undefined) return Option.none()
      const next = self.source.bytes.at(ampersand.span.end)
      insert(
        ampersand.span.end,
        `${Lifetime.display(lifetime)}${next === 32 || next === 9 || next === 10 || next === 13 ? '' : ' '}`,
      )
    } else if (node.kind === 'TypePath' && !self.nominalArguments.has(node)) {
      const token = SyntaxTree.directToken(node, 'Identifier')
      if (
        token !== undefined &&
        String.fromCharCode(...self.source.bytes.slice(token.span.start, token.span.end)) ===
          'string'
      )
        insert(token.span.end, `<${Lifetime.display(lifetime)}>`)
    } else if (node.kind === 'AppliedType' && !self.nominalArguments.has(node)) {
      const list = SyntaxTree.directNode(node, 'TypeArgumentList')
      if (list === undefined || SyntaxTree.directNode(list, 'EffectEnvironment') !== undefined)
        continue
      const path = SyntaxTree.directNode(node, 'TypePath')
      const token = path === undefined ? undefined : SyntaxTree.directToken(path, 'Identifier')
      if (
        token === undefined ||
        String.fromCharCode(...self.source.bytes.slice(token.span.start, token.span.end)) !==
          'Effect'
      )
        continue
      const open = SyntaxTree.directToken(list, 'Less')
      if (open === undefined) return Option.none()
      insert(open.span.end, `${Lifetime.display(lifetime)}; `)
    }
  }
  for (const [node, lifetimes] of self.nominalArguments) {
    const argumentsText = lifetimes.map(Lifetime.display).join(', ')
    const list = SyntaxTree.directNode(node, 'TypeArgumentList')
    if (list !== undefined) {
      const open = SyntaxTree.directToken(list, 'Less')
      if (open === undefined) return Option.none()
      insert(open.span.end, `${argumentsText}${list.children.some(SyntaxTree.isNode) ? ', ' : ''}`)
    } else {
      const token = SyntaxTree.tokens(node).filter(significant).at(-1)
      if (token === undefined) return Option.none()
      insert(token.span.end, `<${argumentsText}>`)
    }
  }
  for (const [node, contract] of self.callables) {
    const fn = SyntaxTree.directToken(node, 'FnKeyword')
    if (fn === undefined) return Option.none()
    if (SyntaxTree.directNode(node, 'CallableEnvironment') === undefined)
      insert(fn.span.end, `<${Lifetime.display(contract.environment)}>`)
    const list = SyntaxTree.directNode(node, 'LifetimeBinderList')
    const authoredCount =
      list === undefined ? 0 : SyntaxTree.directNodes(list, 'LifetimeParameter').length
    const omitted = contract.lifetimeBinders.slice(authoredCount).map(Lifetime.display)
    if (omitted.length === 0) continue
    if (list === undefined) {
      const first = SyntaxTree.tokens(node).find(significant)
      if (first === undefined) return Option.none()
      insert(first.span.start, `for<${omitted.join(', ')}> `)
    } else {
      const close = SyntaxTree.directToken(list, 'Greater')
      if (close === undefined) return Option.none()
      insert(close.span.start, `${authoredCount === 0 ? '' : ', '}${omitted.join(', ')}`)
    }
  }
  if (insertions.size === 0) return Option.none()
  const edits: Array<SourceAction.Edit> = []
  for (const [offset, replacement] of insertions) {
    const span = SourceSpan.fromOffsets(self.source.id, offset, offset)
    if (span === undefined) return Option.none()
    edits.push(SourceAction.edit(span, replacement))
  }
  const plan = SourceAction.changePlan({
    preconditions: [SourceAction.precondition(self.source)],
    changes: [[self.source.id, edits]],
  })
  return Option.map(plan, (plan) =>
    Object.freeze({
      descriptor: SourceAction.descriptor({
        key: 'make-lifetimes-explicit',
        title: 'Make lifetimes explicit',
        kind: 'RefactorRewrite',
        target: self.syntax.span,
      }),
      plan,
    }),
  )
}
