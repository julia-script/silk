import type * as Constraint from './Constraint.js'
import * as AuthoredIdentity from './AuthoredIdentity.js'
import type * as Diagnostic from './Diagnostic.js'
import type * as Location from './Location.js'
import * as SourceSpan from './SourceSpan.js'
import type * as Tir from './Tir.js'

/** Private, single-use construction state for one checked artifact. */
export interface BodyBuilder {
  readonly artifact: Tir.ArtifactId
  readonly nodes: Array<Tir.PublishedNode>
  readonly locals: Array<Tir.Local>
  readonly evidence: Array<ReadonlyArray<Constraint.ConstraintEvidence>>
  readonly causes: Array<Diagnostic.Identity<Location.Location>>
  readonly localIds: Map<string, Tir.LocalId>
  readonly expressions: WeakMap<object, Tir.Expression>
}

export const make = (artifact: Tir.ArtifactId): BodyBuilder => ({
  artifact,
  nodes: [],
  locals: [],
  evidence: [],
  causes: [],
  localIds: new Map(),
  expressions: new WeakMap(),
})

const semanticLocalKey = (input: unknown): string | undefined => {
  if (typeof input !== 'object' || input === null) return undefined
  const id = input as Readonly<Record<string, unknown>>
  const tag = id['_tag']
  const ordinal = id['ordinal']
  if (typeof ordinal !== 'number') return undefined
  if (tag === 'ParameterId' || tag === 'TirBinding') {
    const owner = id['function'] as Readonly<Record<string, unknown>> | undefined
    return owner === undefined
      ? undefined
      : `${String(tag)}:${String(owner['sourceId'])}:${String(owner['ordinal'])}:${ordinal}`
  }
  if (tag === 'PatternBindingId') {
    const arm = id['arm'] as Readonly<Record<string, unknown>> | undefined
    const match = arm?.['match'] as Readonly<Record<string, unknown>> | undefined
    const owner = match?.['function'] as Readonly<Record<string, unknown>> | undefined
    const at = match?.['at'] as AuthoredIdentity.Anchor | undefined
    return owner === undefined || arm === undefined || at === undefined
      ? undefined
      : `PatternBindingId:${String(owner['sourceId'])}:${String(owner['ordinal'])}:${AuthoredIdentity.anchorKey(at)}:${String(arm['ordinal'])}:${ordinal}`
  }
  return undefined
}

/** Publishes one node and assigns the next dense artifact-local identity. */
export const node = <A extends { readonly origin: Tir.Origin }>(
  self: BodyBuilder,
  value: A,
): Readonly<A & Tir.Node> => {
  const result = Object.freeze({
    ...value,
    id: Object.freeze({ _tag: 'TirNode' as const, ordinal: self.nodes.length }),
  })
  self.nodes.push(result)
  return result
}

/** Returns an unambiguous reference to a node already published by this builder. */
export const reference = (self: BodyBuilder, value: Tir.PublishedNode): Tir.NodeRef =>
  Object.freeze({ artifact: self.artifact, node: value.id })

/** Adds one local to the body's unified dense namespace. */
export const local = (self: BodyBuilder, value: Omit<Tir.Local, 'id'>): Tir.Local => {
  const result = Object.freeze({
    ...value,
    id: Object.freeze({ _tag: 'TirLocal' as const, ordinal: self.locals.length }),
  })
  self.locals.push(result)
  return result
}

/** Registers or retrieves one semantic local in the body's unified namespace. */
export const semanticLocal = (
  self: BodyBuilder,
  semantic: unknown,
  value: Omit<Tir.Local, 'id'>,
): Tir.LocalId => {
  const key = semanticLocalKey(semantic)
  if (key === undefined) throw new RangeError('TIR local has no semantic construction identity')
  const known = self.localIds.get(key)
  if (known !== undefined) return known
  const registered = local(self, value)
  self.localIds.set(key, registered.id)
  return registered.id
}

/** Resolves a semantic construction identity after its local has been registered. */
export const localId = (self: BodyBuilder, semantic: unknown): Tir.LocalId => {
  const key = semanticLocalKey(semantic)
  const known = key === undefined ? undefined : self.localIds.get(key)
  if (known === undefined) throw new RangeError('TIR referenced an unregistered local')
  return known
}

/** Stores selected conformance evidence once and returns its dense table reference. */
export const selectedEvidence = (
  self: BodyBuilder,
  value: ReadonlyArray<Constraint.ConstraintEvidence>,
): Tir.EvidenceRef => {
  const ref = Object.freeze({ _tag: 'TirEvidence' as const, ordinal: self.evidence.length })
  self.evidence.push(Object.freeze(Array.from(value)))
  return ref
}

/** Stores one revision-free unavailable cause and returns its dense table reference. */
export const cause = (
  self: BodyBuilder,
  value: Diagnostic.Identity<Location.Location>,
): Tir.CauseRef => {
  const ref = Object.freeze({ _tag: 'TirCause' as const, ordinal: self.causes.length })
  self.causes.push(value)
  return ref
}

/**
 * Numbers an already typed body while the direct-construction migration is in progress.
 *
 * This walks TIR itself, not a second executable schema. Direct construction replaces this final
 * numbering walk once every analysis constructor owns the builder.
 */
export const index = (self: BodyBuilder, fn: Tir.TirFunction): Tir.TirFunction => {
  // Directly emitted expressions are already numbered. Until statements are emitted directly too,
  // the completed tree is renumbered once as a unit so the published namespace has no gaps.
  self.nodes.length = 0
  for (const parameter of fn.declaration.parameters) {
    semanticLocal(self, parameter.id, {
      kind: 'Parameter',
      ...(parameter.name._tag === 'Present' ? { name: parameter.name.spelling } : {}),
      type: parameter.declaredType._tag === 'Resolved' ? parameter.declaredType.type : 'never',
      mutability: parameter.bindingMutability,
    })
  }
  const discover = (input: unknown, seen: WeakSet<object>): void => {
    if (typeof input !== 'object' || input === null || seen.has(input)) return
    if (input instanceof Map || input instanceof Set || SourceSpan.isSourceSpan(input)) return
    seen.add(input)
    if (Array.isArray(input)) {
      for (const item of input) discover(item, seen)
      return
    }
    const value = input as Readonly<Record<string, unknown>>
    const tag = value['_tag']
    if (
      (tag === 'Call' || tag === 'EffectConstruct' || tag === 'EffectCatch') &&
      Array.isArray(value['evidence'])
    )
      selectedEvidence(self, value['evidence'] as ReadonlyArray<Constraint.ConstraintEvidence>)
    if (tag === 'EffectBindRequirement') {
      const provider = value['provider'] as Readonly<Record<string, unknown>> | undefined
      if (Array.isArray(provider?.['evidence']))
        selectedEvidence(self, provider['evidence'] as ReadonlyArray<Constraint.ConstraintEvidence>)
    }
    if (tag === 'Unavailable') {
      const unavailable = value['causeAt'] as Diagnostic.Identity<Location.Location> | undefined
      if (unavailable !== undefined) cause(self, unavailable)
    }
    if (value['_tag'] === 'Bind') {
      const initializer = value['initializer'] as Readonly<Record<string, unknown>> | undefined
      semanticLocal(self, value['binding'], {
        kind: 'Binding',
        ...(typeof value['name'] === 'string' ? { name: value['name'] } : {}),
        type: (initializer?.['type'] as import('./Type.js').Type | undefined) ?? 'never',
        mutability: value['mutability'] === 'Mutable' ? 'Mutable' : 'Immutable',
      })
    }
    const patternId = value['id']
    const patternKey = semanticLocalKey(patternId)
    if (patternKey?.startsWith('PatternBindingId:') === true && Array.isArray(value['path']))
      semanticLocal(self, patternId, {
        kind: 'Pattern',
        ...(typeof value['name'] === 'string' ? { name: value['name'] } : {}),
        type: (value['type'] as import('./Type.js').Type | undefined) ?? 'never',
        mutability: value['access'] === 'Place' ? 'Mutable' : 'Immutable',
      })
    for (const child of Object.values(value)) discover(child, seen)
  }
  discover(fn.statements, new WeakSet())
  const copies = new WeakMap<object, unknown>()
  const visit = (input: unknown): unknown => {
    if (typeof input !== 'object' || input === null) return input
    if (input instanceof Map || input instanceof Set || SourceSpan.isSourceSpan(input)) return input
    const known = copies.get(input)
    if (known !== undefined) return known
    if (Array.isArray(input)) {
      const items: Array<unknown> = []
      copies.set(input, items)
      for (const item of input) items.push(visit(item))
      return Object.freeze(items)
    }
    const source = input as Readonly<Record<string, unknown>>
    const result: Record<string, unknown> = {}
    copies.set(input, result)
    const origin = source['origin'] as Tir.Origin | undefined
    const isNode = origin !== undefined && typeof source['_tag'] === 'string'
    if (isNode) {
      result['id'] = Object.freeze({ _tag: 'TirNode' as const, ordinal: self.nodes.length })
      self.nodes.push(result as unknown as Tir.PublishedNode)
    }
    for (const key of Object.keys(source)) {
      if (!isNode || key !== 'id') result[key] = visit(source[key])
    }
    return Object.freeze(result)
  }
  const indexed = visit(fn) as Tir.TirFunction
  return Object.freeze({ ...indexed, locals: Object.freeze(Array.from(self.locals)) })
}
