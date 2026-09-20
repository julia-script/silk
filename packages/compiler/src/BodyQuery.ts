import type * as AuthoredHir from './AuthoredHir.js'
import * as AuthoredIdentity from './AuthoredIdentity.js'
import * as AuthoredLowering from './AuthoredLowering.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Elaboration from './Elaboration.js'
import * as ModuleSurface from './ModuleSurface.js'
import type * as NameResolution from './NameResolution.js'
import type * as Ownership from './Ownership.js'
import * as SourceSpan from './SourceSpan.js'
import * as Tir from './Tir.js'
import type * as SemanticContext from './SemanticContext.js'

/** Actual source-body query work, independent of module invalidation observations. */
export interface Counters {
  readonly _tag: 'BodyQueryCounters'
  readonly checked: number
  readonly reused: number
  /** Reused bodies whose source moved, so their positions were presented again. */
  readonly presented: number
  readonly validatedDependencies: number
  readonly dependencyCacheHits: number
  readonly ownershipChecked: number
  readonly ownershipReused: number
  readonly recursiveComponents: number
}

/**
 * One input a body consumed while it was built, with the answer it got. A lookup that found
 * nothing is an observation too, so adding the member repairs the body that missed it.
 */
interface Observation {
  readonly key: string
  readonly signature: string | undefined
  readonly implementation?: string
}

/**
 * What must still hold for a cached unit to stand. It is content, never bytes or positions: the
 * owner's semantic signature, its canonical authored body, the names its body can see, and every
 * observation asked again. A candidate is found by its owner alone; this decides whether it is used.
 */
interface Validity {
  readonly header: string
  readonly body: string
  readonly scope: string
  readonly resolution: string
  readonly observed: ReadonlyArray<Observation>
}

interface Entry {
  readonly declaration: DeclarationFacts.DeclarationFact
  /**
   * The authored declaration behind this body. Declaration facts are rebuilt every revision, so
   * only the authored node — which the closure shares whenever a module's source is unchanged —
   * can witness that a body's own input is untouched and its cached positions still hold.
   */
  readonly authoredDeclaration: AuthoredHir.Declaration
  readonly context: SemanticContext.SemanticContext
  readonly index: DeclarationIndex.Index
  readonly validity: Validity
  /** The checked unit as this revision presents it. Working records are never stored. */
  readonly unit: Elaboration.CheckedUnit
  readonly calls: ReadonlyArray<string>
  readonly ownership: Map<
    string,
    {
      readonly input: Ownership.CheckInput
      readonly checked: Ownership.CheckedFunction
    }
  >
}

/** One revision's declaration cache, shared by module elaboration coordinators. */
export interface BodyQuery {
  readonly index: DeclarationIndex.Index
  readonly resolution: string
  readonly members: ReadonlyMap<string, DeclarationFacts.MemberFact>
  readonly signatures: ReadonlyMap<string, string>
  readonly implementations: ReadonlyMap<string, string>
  readonly comparisons: Map<string, boolean>
  readonly owners: WeakMap<object, string>
  readonly previous: ReadonlyMap<string, Entry>
  readonly previousModules: ReadonlyMap<string, Elaboration.Result>
  /**
   * Whether each module of this revision shares its authored lowering with the previous revision.
   * A shared lowering is the only evidence that a module's declarations kept their authored
   * positions, because every declaration fact is rebuilt each revision. Resolved for the whole
   * closure up front, so a body may consult a module whose own bodies are not yet checked.
   */
  readonly sharedModules: ReadonlyMap<string, boolean>
  readonly entries: Map<string, Entry>
  readonly reuse: WeakMap<object, { readonly prior: Entry; readonly moved: boolean }>
  readonly parents: WeakMap<object, string>
  readonly work: {
    checked: number
    reused: number
    presented: number
    validatedDependencies: number
    dependencyCacheHits: number
    ownershipChecked: number
    ownershipReused: number
  }
}

const artifacts = new WeakMap<Elaboration.Result, ReadonlyArray<Entry>>()
const records = (value: unknown): value is Readonly<Record<string, unknown>> =>
  typeof value === 'object' && value !== null && !Array.isArray(value)

/** Visits each object of an immutable graph once, stopping below objects the inspector declines. */
const visit = (value: unknown, inspect: (value: object) => boolean): void => {
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
    } else if (records(input)) {
      for (const child of Object.values(input)) walk(child)
    }
  }
  walk(value)
}

const hiddenOrdinals = Tir.hiddenDeclarationOrdinal(0, 0)
const declarationKey = (sourceId: string, ordinal: number): string => `${sourceId}\u0000${ordinal}`

/**
 * Where each declaration of an earlier revision sits now.
 *
 * ponytail: a declaration id is its position in its module, so inserting a declaration renumbers
 * the ones after it, and a reused body must follow. Ids keyed by authored identity would make this
 * table, and the copy that applies it, unnecessary.
 */
const renumberings = new WeakMap<
  DeclarationIndex.Index,
  WeakMap<DeclarationIndex.Index, ReadonlyMap<string, number>>
>()
const renumberingOf = (
  previous: DeclarationIndex.Index,
  self: BodyQuery,
): ReadonlyMap<string, number> => {
  let known = renumberings.get(previous)
  if (known === undefined) {
    known = new WeakMap()
    renumberings.set(previous, known)
  }
  const cached = known.get(self.index)
  if (cached !== undefined) return cached
  const result = new Map<string, number>()
  for (const [key, before] of membersOf(previous)) {
    const after = self.members.get(key)
    if (after === undefined || !('id' in before) || !('id' in after)) continue
    if (before.id._tag !== 'DeclarationId' || after.id._tag !== 'DeclarationId') continue
    if (before.id.ordinal !== after.id.ordinal)
      result.set(declarationKey(before.id.sourceId, before.id.ordinal), after.id.ordinal)
  }
  known.set(self.index, result)
  return result
}

/** The same value naming every declaration by its current id. Positions are left alone. */
const renumber = <A>(value: A, moved: ReadonlyMap<string, number>): A => {
  if (moved.size === 0) return value
  const copies = new WeakMap<object, unknown>()
  const copy = (input: unknown): unknown => {
    if (typeof input !== 'object' || input === null || SourceSpan.isSourceSpan(input)) return input
    const known = copies.get(input)
    if (known !== undefined) return known
    if (input instanceof Map) {
      const result = new Map<unknown, unknown>()
      copies.set(input, result)
      for (const [key, child] of input) result.set(copy(key), copy(child))
      return result
    }
    if (input instanceof Set) {
      const result = new Set<unknown>()
      copies.set(input, result)
      for (const child of input) result.add(copy(child))
      return result
    }
    if (Array.isArray(input)) {
      const result: Array<unknown> = []
      copies.set(input, result)
      for (const child of input) result.push(copy(child))
      return Object.freeze(result)
    }
    if (!records(input)) return input
    if (
      input._tag === 'DeclarationId' &&
      typeof input.sourceId === 'string' &&
      typeof input.ordinal === 'number'
    ) {
      // A compiler-made body is numbered from the declaration that encloses it.
      const made = input.ordinal >= hiddenOrdinals
      const site = made ? (input.ordinal - hiddenOrdinals) % 65536 : 0
      const enclosing = made ? (input.ordinal - hiddenOrdinals - site) / 65536 : input.ordinal
      const current = moved.get(declarationKey(input.sourceId, enclosing))
      if (current === undefined) return input
      return Object.freeze({
        ...input,
        ordinal: made ? Tir.hiddenDeclarationOrdinal(current, site) : current,
      })
    }
    const result: Record<string, unknown> = {}
    copies.set(input, result)
    for (const key of Object.keys(input)) result[key] = copy(input[key])
    return Object.freeze(result)
  }
  return copy(value) as A
}

const memberKey = (value: DeclarationFacts.MemberFact): string =>
  'canonical' in value && value.canonical._tag === 'Canonical'
    ? `${value.canonical.id.module}/${value.canonical.id.name}`
    : `${value._tag}:${AuthoredIdentity.anchorKey(value.anchor)}`

const memberCatalogs = new WeakMap<
  DeclarationIndex.Index,
  ReadonlyMap<string, DeclarationFacts.MemberFact>
>()
const membersOf = (
  index: DeclarationIndex.Index,
): ReadonlyMap<string, DeclarationFacts.MemberFact> => {
  const cached = memberCatalogs.get(index)
  if (cached !== undefined) return cached
  const result = new Map(
    index.modules.flatMap((module) =>
      [...module.members, ...module.declarations].map(
        (member) => [memberKey(member), member] as const,
      ),
    ),
  )
  memberCatalogs.set(index, result)
  return result
}

/** Creates a query context from the prior revision's completed module artifacts. */
export const make = (
  index: DeclarationIndex.Index,
  previous: Iterable<Elaboration.Result> = [],
  current: Iterable<AuthoredLowering.Lowered> = [],
): BodyQuery => {
  const previousResults = [...previous]
  const previousLowerings = new Map(
    previousResults.map((result) => [result.authored.module.owner.module, result.authored]),
  )
  const members = membersOf(index)
  const owners = new WeakMap<object, string>()
  for (const [key, member] of members)
    visit(member, (value) => {
      if (records(value) && (value._tag === 'SyntaxNode' || value._tag === 'Token')) return false
      if (
        value === member ||
        (records(value) &&
          typeof value._tag === 'string' &&
          (value._tag.endsWith('Declaration') ||
            value._tag.endsWith('Id') ||
            value._tag === 'AggregateField' ||
            value._tag === 'UnionVariant'))
      )
        owners.set(value, key)
      return true
    })
  return {
    index,
    resolution: ModuleSurface.resolutionSignature(index),
    members,
    signatures: new Map(
      [...members].map(([key, member]) => [key, ModuleSurface.memberSignature(member)]),
    ),
    implementations: new Map(
      [...members].map(([key, member]) => [key, ModuleSurface.memberImplementation(member)]),
    ),
    comparisons: new Map(),
    owners,
    previous: new Map(
      previousResults.flatMap((result) =>
        (artifacts.get(result) ?? []).map(
          (entry) => [memberKey(entry.declaration), entry] as const,
        ),
      ),
    ),
    previousModules: new Map(
      previousResults.map((result) => [result.authored.module.owner.module, result]),
    ),
    sharedModules: new Map(
      [...current].map((lowered) => {
        const module = lowered.module.owner.module
        return [module, previousLowerings.get(module) === lowered] as const
      }),
    ),
    entries: new Map(),
    reuse: new WeakMap(),
    parents: new WeakMap(),
    work: {
      checked: 0,
      reused: 0,
      presented: 0,
      validatedDependencies: 0,
      dependencyCacheHits: 0,
      ownershipChecked: 0,
      ownershipReused: 0,
    },
  }
}

/** The authored declaration behind one indexed declaration; the pipeline lowers every module. */
const authoredDeclaration = (
  authored: AuthoredLowering.Lowered,
  declaration: DeclarationFacts.DeclarationFact,
) =>
  AuthoredLowering.declarationOf(authored, declaration.owner) ??
  (() => {
    throw new RangeError(`Authored module lost declaration ${memberKey(declaration)}`)
  })()

/** The canonical authored body up to lifetime alpha-renaming: the syntax-free implementation key. */
const implementation = (
  authored: AuthoredLowering.Lowered,
  declaration: DeclarationFacts.DeclarationFact,
): string => AuthoredLowering.canonicalBody(authored, authoredDeclaration(authored, declaration))

const scopeSignature = (
  authored: AuthoredLowering.Lowered,
  declaration: DeclarationFacts.DeclarationFact,
  scope: NameResolution.ModuleScope,
): string => {
  const names = AuthoredLowering.bodyNames(authored, authoredDeclaration(authored, declaration))
  return JSON.stringify(
    scope.bindings
      .filter((binding) => names.has(binding.spelling))
      .map((binding) => {
        if (binding._tag === 'LocalDeclaration' || binding._tag === 'ImportedMember')
          return [
            binding._tag,
            binding.spelling,
            binding.declaration.module,
            binding.declaration.name,
          ]
        if (binding._tag === 'ModuleNamespace')
          return [binding._tag, binding.spelling, binding.module]
        return [binding._tag, binding.spelling]
      }),
  )
}

const nominalDependencies = new WeakMap<DeclarationFacts.MemberFact, ReadonlyArray<string>>()

const nominalDependenciesOf = (member: DeclarationFacts.MemberFact): ReadonlyArray<string> => {
  const cached = nominalDependencies.get(member)
  if (cached !== undefined) return cached
  const selected = new Set<string>()
  // Documentation checks thousands of bodies against the same immutable declarations. Walk each
  // declaration shape once; callers still close over these edges in their own dependency set.
  visit(member, (value) => {
    if (
      records(value) &&
      value._tag === 'NominalType' &&
      typeof value.module === 'string' &&
      typeof value.name === 'string'
    )
      selected.add(`${value.module}/${value.name}`)
    return !(records(value) && (value._tag === 'SyntaxNode' || value._tag === 'Token'))
  })
  const result = Object.freeze([...selected])
  nominalDependencies.set(member, result)
  return result
}

const dependencies = (self: BodyQuery, built: Built): ReadonlyArray<Observation> => {
  const selected = new Set<string>()
  visit(built.records, (value) => {
    const owner = self.owners.get(value)
    if (owner !== undefined) {
      selected.add(owner)
      return false
    }
    if (
      records(value) &&
      value._tag === 'NominalType' &&
      typeof value.module === 'string' &&
      typeof value.name === 'string'
    )
      selected.add(`${value.module}/${value.name}`)
    return !(records(value) && (value._tag === 'SyntaxNode' || value._tag === 'Token'))
  })
  const result = new Map<string, Observation>()
  const add = (key: string): void => {
    if (result.has(key)) return
    const member = self.members.get(key)
    if (member === undefined) return
    result.set(key, {
      key,
      signature: self.signatures.get(key) ?? '',
      ...(member._tag === 'FunctionDeclaration' && member.bodyTemplate !== undefined
        ? { implementation: self.implementations.get(key) ?? '' }
        : {}),
    })
    // Resolved nominal shapes carry variance, cleanup and nested lifetime requirements. Traverse
    // only those selected shapes; a visited declaration bounds recursive components finitely.
    for (const dependency of nominalDependenciesOf(member)) add(dependency)
  }
  for (const key of [...selected].sort()) add(key)
  // A failed lookup consumed the absence of this exact member. Retain that input so adding
  // the member repairs cached diagnostics without invalidating users of unrelated names.
  for (const diagnostic of built.unit.diagnostics) {
    if (diagnostic.reason._tag !== 'UnknownImportedMember') continue
    const key = `${diagnostic.reason.module}/${diagnostic.reason.spelling}`
    if (!result.has(key)) result.set(key, { key, signature: self.signatures.get(key) })
  }
  return [...result.values()].sort((left, right) => left.key.localeCompare(right.key))
}

const callsOf = (self: BodyQuery, built: Built): ReadonlyArray<string> => {
  const calls = new Set<string>()
  visit(built.records, (value) => {
    if (
      records(value) &&
      (value._tag === 'Call' || value._tag === 'CallableSection') &&
      records(value.reference) &&
      records(value.reference.declaration)
    ) {
      const key = self.owners.get(value.reference.declaration)
      if (key !== undefined) calls.add(key)
    }
    return (
      self.owners.get(value) === undefined &&
      !(records(value) && (value._tag === 'SyntaxNode' || value._tag === 'Token'))
    )
  })
  return [...calls].sort()
}

/** The authored module owning one dependency key's member fact, absent when the key is unresolved. */
const dependencyModule = (self: BodyQuery, key: string): string | undefined =>
  self.members.get(key)?.anchor.owner.module

const validateDependencies = (
  self: BodyQuery,
  dependencies: ReadonlyArray<Observation>,
  visited = new Set<string>(),
): boolean =>
  dependencies.every((dependency) => {
    const comparison = JSON.stringify([
      dependency.key,
      dependency.signature,
      dependency.implementation,
    ])
    let matches = self.comparisons.get(comparison)
    if (matches === undefined) {
      self.work.validatedDependencies += 1
      matches =
        self.signatures.get(dependency.key) === dependency.signature &&
        (dependency.implementation === undefined ||
          self.implementations.get(dependency.key) === dependency.implementation)
      self.comparisons.set(comparison, matches)
    } else self.work.dependencyCacheHits += 1
    if (!matches) return false
    if (dependency.implementation === undefined) return true
    if (visited.has(dependency.key)) return true
    visited.add(dependency.key)
    const body = self.previous.get(dependency.key)
    return body === undefined || validateDependencies(self, body.validity.observed, visited)
  })

/** What construction hands back for one declaration. */
export interface Built {
  readonly unit: Elaboration.CheckedUnit
  /** The working records of this build, read once for the inputs they consumed and then dropped. */
  readonly records: unknown
}

const present = (
  previous: Elaboration.CheckedUnit,
  moved: ReadonlyMap<string, number>,
  context: SemanticContext.SemanticContext,
  declaration: DeclarationFacts.DeclarationFact,
): Elaboration.CheckedUnit =>
  Object.freeze({
    diagnostics: renumber(previous.diagnostics, moved),
    bodies: Object.freeze(
      previous.bodies.map((body) =>
        // A source body belongs to this revision's header object, which is never copied; a
        // compiler-made body owns its declaration, which is renumbered with the rest.
        Elaboration.presentBody(
          {
            artifact: body.artifact,
            declaration: body.hidden ? renumber(body.declaration, moved) : declaration,
            hidden: body.hidden,
            ...(body.function === undefined
              ? {}
              : {
                  function: {
                    ...renumber({ ...body.function, declaration: undefined }, moved),
                    declaration,
                  },
                }),
            results: renumber(body.results, moved),
          },
          context,
        ),
      ),
    ),
  })

/** Runs the body checker only when its own implementation or a consumed input changed. */
export const check = (
  self: BodyQuery,
  context: SemanticContext.SemanticContext,
  authored: AuthoredLowering.Lowered,
  scope: NameResolution.ModuleScope,
  declaration: DeclarationFacts.DeclarationFact,
  compute: () => Built,
): Elaboration.CheckedUnit => {
  const key = memberKey(declaration)
  const prior = self.previous.get(key)
  const signature = self.signatures.get(key) ?? ModuleSurface.memberSignature(declaration)
  const declared = authoredDeclaration(authored, declaration)
  const bodyKey = implementation(authored, declaration)
  const scopeKey = scopeSignature(authored, declaration, scope)
  const valid =
    prior !== undefined &&
    AuthoredIdentity.equals(prior.context.module.owner, context.module.owner) &&
    prior.validity.header === signature &&
    prior.validity.body === bodyKey &&
    prior.validity.scope === scopeKey &&
    prior.validity.resolution === self.resolution &&
    validateDependencies(self, prior.validity.observed)
  let unit: Elaboration.CheckedUnit
  let consumed: ReadonlyArray<Observation>
  let calls: ReadonlyArray<string>
  let ownership: Entry['ownership']
  if (valid && prior !== undefined) {
    self.work.reused += 1
    // Declaration and member facts are rebuilt every revision, so their object identity never
    // survives one; only the authored lowering is shared, and only for a byte-identical source.
    // A shared authored declaration therefore witnesses that this body kept its own positions, and
    // a shared lowering behind every consumed member witnesses the same for its inputs. Otherwise
    // the body is the same and only its presentation is stale.
    const unchanged =
      prior.authoredDeclaration === declared &&
      prior.validity.observed.every((dependency) => {
        const module = dependencyModule(self, dependency.key)
        return module !== undefined && self.sharedModules.get(module) === true
      })
    unit = unchanged
      ? prior.unit
      : present(prior.unit, renumberingOf(prior.index, self), context, declaration)
    if (!unchanged) self.work.presented += 1
    for (const body of unit.bodies)
      self.reuse.set(body.results.lifetimes ?? body.results, { prior, moved: !unchanged })
    consumed = prior.validity.observed
    calls = prior.calls
    ownership = new Map(prior.ownership)
  } else {
    self.work.checked += 1
    const built = compute()
    unit = built.unit
    consumed = dependencies(self, built)
    calls = callsOf(self, built)
    ownership = new Map()
  }
  self.entries.set(key, {
    declaration,
    authoredDeclaration: declared,
    context,
    index: self.index,
    validity: {
      header: signature,
      body: bodyKey,
      scope: scopeKey,
      resolution: self.resolution,
      observed: consumed,
    },
    unit,
    calls,
    ownership,
  })
  for (const body of unit.bodies) self.parents.set(body.results.lifetimes ?? body.results, key)
  return unit
}

const spanKey = (span: SourceSpan.SourceSpan): string => `${span.start}:${span.end}`

/** Where each position of a body moved, read off the same body presented in two revisions. */
const movesOf = (previous: Entry, current: Entry): ReadonlyMap<string, SourceSpan.SourceSpan> => {
  const moves = new Map<string, SourceSpan.SourceSpan>()
  const visited = new WeakSet<object>()
  const walk = (before: unknown, after: unknown): void => {
    if (typeof before !== 'object' || before === null || visited.has(before)) return
    if (typeof after !== 'object' || after === null) return
    visited.add(before)
    if (SourceSpan.isSourceSpan(before)) {
      if (SourceSpan.isSourceSpan(after)) moves.set(spanKey(before), after)
      return
    }
    if (before instanceof Map || before instanceof Set) return
    if (Array.isArray(before)) {
      if (Array.isArray(after)) for (const [at, item] of before.entries()) walk(item, after[at])
      return
    }
    if (!records(before) || !records(after)) return
    for (const key of Object.keys(before)) walk(before[key], after[key])
  }
  for (const [at, body] of previous.unit.bodies.entries()) {
    walk(body.function, current.unit.bodies[at]?.function)
    // Ownership also reports at the header: the declaration and each parameter it binds.
    for (const anchor of [
      body.declaration.anchor,
      ...body.declaration.parameters.map((parameter) => parameter.anchor),
    ])
      moves.set(spanKey(previous.context.spanOf(anchor)), current.context.spanOf(anchor))
  }
  return moves
}

/** A proof about a moved body, with each position carried along; absent when one is unknown. */
const moved = <A>(value: A, moves: ReadonlyMap<string, SourceSpan.SourceSpan>): A | undefined => {
  let complete = true
  const copies = new WeakMap<object, unknown>()
  const copy = (input: unknown): unknown => {
    if (typeof input !== 'object' || input === null) return input
    if (SourceSpan.isSourceSpan(input)) {
      const next = moves.get(spanKey(input))
      if (next === undefined) complete = false
      return next ?? input
    }
    const known = copies.get(input)
    if (known !== undefined) return known
    if (input instanceof Map) {
      const result = new Map<unknown, unknown>()
      copies.set(input, result)
      for (const [key, child] of input) result.set(copy(key), copy(child))
      return result
    }
    if (input instanceof Set) {
      const result = new Set<unknown>()
      copies.set(input, result)
      for (const child of input) result.add(copy(child))
      return result
    }
    if (Array.isArray(input)) {
      const result: Array<unknown> = []
      copies.set(input, result)
      for (const child of input) result.push(copy(child))
      return Object.freeze(result)
    }
    if (!records(input)) return input
    const result: Record<string, unknown> = {}
    copies.set(input, result)
    for (const key of Object.keys(input)) result[key] = copy(input[key])
    return Object.freeze(result)
  }
  const result = copy(value) as A
  return complete ? result : undefined
}

/** Reuses source ownership only beside a reused semantic body and unchanged access boundaries. */
export const ownership = (
  self: BodyQuery,
  input: Ownership.CheckInput,
  compute: () => Ownership.CheckedFunction,
): Ownership.CheckedFunction => {
  // A body is known here by the region proof it published, which is one object per checked body.
  const { lifetimes: proof, boundaries } = input
  const key = proof === undefined ? undefined : memberKey(input.function.declaration)
  const parent = proof === undefined ? undefined : self.parents.get(proof)
  const current = parent === undefined ? undefined : self.entries.get(parent)
  const reuse = proof === undefined ? undefined : self.reuse.get(proof)
  const prior = key === undefined ? undefined : reuse?.prior.ownership.get(key)
  const retained =
    prior === undefined ? undefined : { boundaries: prior.input.boundaries, checked: prior.checked }
  // Ownership reports in source coordinates. Its proof holds for a body that only moved, so each
  // position follows the body; a position the body does not account for means checking again.
  const carried =
    retained === undefined || reuse?.moved !== true || current === undefined
      ? retained
      : moved(retained, movesOf(reuse.prior, current))
  let checked: Ownership.CheckedFunction
  if (carried !== undefined && JSON.stringify(carried.boundaries) === JSON.stringify(boundaries)) {
    checked = carried.checked
    self.work.ownershipReused += 1
  } else {
    checked = compute()
    self.work.ownershipChecked += 1
  }
  if (current !== undefined && key !== undefined) current.ownership.set(key, { input, checked })
  return checked
}

/** Attaches current query artifacts to their immutable elaboration boundary for the next revision. */
export const publish = (self: BodyQuery, result: Elaboration.Result): Elaboration.Result => {
  const previous = self.previousModules.get(result.authored.module.owner.module)
  const published =
    previous !== undefined &&
    previous.authored === result.authored &&
    previous.bodies.length === result.bodies.length &&
    result.bodies.every((body, ordinal) => body === previous.bodies[ordinal])
      ? previous
      : result
  artifacts.set(
    published,
    result.bodies.flatMap((body) => {
      const entry = body.hidden ? undefined : self.entries.get(memberKey(body.declaration))
      return entry === undefined ? [] : [entry]
    }),
  )
  return published
}

/** Returns deterministic counters recorded at actual query execution and reuse branches. */
export const counters = (self: BodyQuery): Counters => ({
  _tag: 'BodyQueryCounters',
  ...self.work,
  recursiveComponents: components(self).length,
})

/** Finds actual recursive components in the resolved source-call graph, excluding nominal edges. */
export const components = (self: BodyQuery): ReadonlyArray<ReadonlyArray<string>> => {
  const indices = new Map<string, number>()
  const low = new Map<string, number>()
  const stack: Array<string> = []
  const active = new Set<string>()
  const result: Array<ReadonlyArray<string>> = []
  const visit = (key: string): void => {
    const index = indices.size
    indices.set(key, index)
    low.set(key, index)
    stack.push(key)
    active.add(key)
    for (const dependency of self.entries.get(key)?.calls ?? []) {
      if (!self.entries.has(dependency)) continue
      if (!indices.has(dependency)) {
        visit(dependency)
        low.set(key, Math.min(low.get(key) ?? index, low.get(dependency) ?? index))
      } else if (active.has(dependency))
        low.set(key, Math.min(low.get(key) ?? index, indices.get(dependency) ?? index))
    }
    if (low.get(key) !== index) return
    const component: Array<string> = []
    let member = stack.pop()
    while (member !== undefined) {
      active.delete(member)
      component.push(member)
      if (member === key) break
      member = stack.pop()
    }
    if (component.length > 1 || self.entries.get(key)?.calls.includes(key))
      result.push(component.sort())
  }
  for (const key of [...self.entries.keys()].sort()) if (!indices.has(key)) visit(key)
  return result.sort((left, right) => (left[0] ?? '').localeCompare(right[0] ?? ''))
}
