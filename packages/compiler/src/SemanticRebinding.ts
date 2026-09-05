import * as SourceSpan from './SourceSpan.js'
import * as SyntaxTree from './SyntaxTree.js'

/** An adjacent-revision correspondence for immutable syntax and semantic fact graphs. */
export interface SemanticRebinding {
  readonly replacements: WeakMap<object, object>
  readonly spans: Map<string, SourceSpan.SourceSpan>
  readonly positions: Map<string, number>
}

export const make = (): SemanticRebinding => ({
  replacements: new WeakMap(),
  spans: new Map(),
  positions: new Map(),
})

const record = (value: unknown): value is Readonly<Record<string, unknown>> =>
  typeof value === 'object' && value !== null && !Array.isArray(value)

const span = (value: unknown): value is SourceSpan.SourceSpan =>
  record(value) &&
  typeof value.sourceId === 'string' &&
  typeof value.start === 'number' &&
  typeof value.end === 'number' &&
  Symbol.for('@silklang/compiler/SourceSpan') in value

const spanKey = (value: SourceSpan.SourceSpan): string =>
  `${value.sourceId}:${value.start}:${value.end}`

/** Records corresponding nodes in two already matched declaration or syntax graphs. */
export const pair = (self: SemanticRebinding, previous: unknown, current: unknown): void => {
  const visited = new WeakSet<object>()
  const visit = (left: unknown, right: unknown): void => {
    if (typeof left !== 'object' || left === null || typeof right !== 'object' || right === null)
      return
    if (self.replacements.get(left) === right) return
    if (visited.has(left)) return
    visited.add(left)
    self.replacements.set(left, right)
    if (span(left) && span(right)) {
      self.spans.set(spanKey(left), right)
      self.positions.set(`${left.sourceId}:${left.start}`, right.start)
      self.positions.set(`${left.sourceId}:${left.end}`, right.end)
    }
    if (Array.isArray(left) && Array.isArray(right)) {
      if (left.length === right.length)
        for (const [ordinal, value] of left.entries()) visit(value, right[ordinal])
    } else if (record(left) && record(right)) {
      for (const key of Object.keys(left)) visit(left[key], right[key])
    }
  }
  visit(previous, current)
}

const significant = (value: SyntaxTree.Element): boolean =>
  !SyntaxTree.isToken(value) ||
  !['Whitespace', 'LineComment', 'DocComment', 'ModuleDocComment'].includes(value.kind)

/** Matches source nodes by grammar position after trivia and binder spelling changes. */
export const syntax = (
  self: SemanticRebinding,
  previous: SyntaxTree.Element,
  current: SyntaxTree.Element,
): void => {
  self.replacements.set(previous, current)
  pair(self, previous.span, current.span)
  if (!SyntaxTree.isNode(previous) || !SyntaxTree.isNode(current)) return
  const left = previous.children.filter(significant)
  const right = current.children.filter(significant)
  if (left.length !== right.length) return
  for (const [ordinal, child] of left.entries()) {
    const next = right[ordinal]
    if (next !== undefined) syntax(self, child, next)
  }
}

/** Rebinds immutable facts without rerunning semantic analysis, preserving graph sharing. */
export const rebind = <A>(self: SemanticRebinding, value: A): A => {
  const copies = new WeakMap<object, object>()
  const visit = (input: unknown): unknown => {
    if (typeof input !== 'object' || input === null) return input
    const replacement = self.replacements.get(input) ?? copies.get(input)
    if (replacement !== undefined) return replacement
    if (span(input)) {
      const exact = self.spans.get(spanKey(input))
      if (exact !== undefined) return exact
      const start = self.positions.get(`${input.sourceId}:${input.start}`)
      const end = self.positions.get(`${input.sourceId}:${input.end}`)
      return start === undefined || end === undefined
        ? input
        : (SourceSpan.fromOffsets(input.sourceId, start, end) ?? input)
    }
    if (input instanceof Map) {
      const result = new Map<unknown, unknown>()
      copies.set(input, result)
      for (const [key, child] of input) result.set(visit(key), visit(child))
      return result
    }
    if (input instanceof Set) {
      const result = new Set<unknown>()
      copies.set(input, result)
      for (const child of input) result.add(visit(child))
      return result
    }
    if (Array.isArray(input)) {
      const result: Array<unknown> = []
      copies.set(input, result)
      for (const child of input) result.push(visit(child))
      return Object.freeze(result)
    }
    const result: Record<PropertyKey, unknown> = {}
    copies.set(input, result)
    for (const key of Reflect.ownKeys(input)) {
      const descriptor = Object.getOwnPropertyDescriptor(input, key)
      if (descriptor !== undefined && 'value' in descriptor)
        Object.defineProperty(result, key, { ...descriptor, value: visit(descriptor.value) })
    }
    return Object.freeze(result)
  }
  // Rebinding preserves every property's type and changes only matched identities. TypeScript
  // cannot express that a recursive graph reconstruction preserves an arbitrary input shape.
  return visit(value) as A
}

/** Visits each object in an immutable fact graph once, stopping at accepted boundary objects. */
export const visit = (value: unknown, inspect: (value: object) => boolean): void => {
  const visited = new WeakSet<object>()
  const walk = (input: unknown): void => {
    if (typeof input !== 'object' || input === null || visited.has(input)) return
    visited.add(input)
    if (!inspect(input)) return
    if (input instanceof Map) {
      for (const [key, child] of input) {
        walk(key)
        walk(child)
      }
    } else if (input instanceof Set || Array.isArray(input)) {
      for (const child of input) walk(child)
    } else if (record(input)) {
      for (const child of Object.values(input)) walk(child)
    }
  }
  walk(value)
}
