import * as AuthoredIdentity from './AuthoredIdentity.js'
import * as AuthoredLowering from './AuthoredLowering.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Elaboration from './Elaboration.js'
import type * as ExpressionAnalysis from './ExpressionAnalysis.js'
import * as ModuleSurface from './ModuleSurface.js'
import type * as NameResolution from './NameResolution.js'
import type * as Ownership from './Ownership.js'
import * as Tir from './Tir.js'
import * as SemanticRebinding from './SemanticRebinding.js'
import type * as SemanticContext from './SemanticContext.js'

/** Actual source-body query work, independent of module invalidation observations. */
export interface Counters {
  readonly _tag: 'BodyQueryCounters'
  readonly checked: number
  readonly reused: number
  readonly rebound: number
  readonly validatedDependencies: number
  readonly dependencyCacheHits: number
  readonly ownershipChecked: number
  readonly ownershipReused: number
  readonly recursiveComponents: number
}

interface Dependency {
  readonly key: string
  readonly signature: string | undefined
  readonly implementation?: string
}

interface Entry {
  readonly declaration: DeclarationFacts.DeclarationFact
  readonly context: SemanticContext.SemanticContext
  readonly index: DeclarationIndex.Index
  readonly implementation: string
  readonly signature: string
  readonly scope: string
  readonly resolution: string
  readonly dependencies: ReadonlyArray<Dependency>
  readonly analysis: ExpressionAnalysis.FunctionAnalysis
  readonly hidden: ReadonlyArray<Elaboration.FunctionFact>
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
  readonly rebindings: WeakMap<DeclarationIndex.Index, SemanticRebinding.SemanticRebinding>
  readonly owners: WeakMap<object, string>
  readonly previous: ReadonlyMap<string, Entry>
  readonly previousModules: ReadonlyMap<string, Elaboration.Result>
  readonly entries: Map<string, Entry>
  readonly reuse: WeakMap<
    Elaboration.FunctionFact,
    { readonly prior: Entry; readonly rebinding?: SemanticRebinding.SemanticRebinding }
  >
  readonly parents: WeakMap<Elaboration.FunctionFact, string>
  readonly work: {
    checked: number
    reused: number
    rebound: number
    validatedDependencies: number
    dependencyCacheHits: number
    ownershipChecked: number
    ownershipReused: number
  }
}

const artifacts = new WeakMap<Elaboration.Result, ReadonlyArray<Entry>>()
const records = (value: unknown): value is Readonly<Record<string, unknown>> =>
  typeof value === 'object' && value !== null && !Array.isArray(value)

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
): BodyQuery => {
  const previousResults = [...previous]
  const members = membersOf(index)
  const owners = new WeakMap<object, string>()
  for (const [key, member] of members)
    SemanticRebinding.visit(member, (value) => {
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
    rebindings: new WeakMap(),
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
    entries: new Map(),
    reuse: new WeakMap(),
    parents: new WeakMap(),
    work: {
      checked: 0,
      reused: 0,
      rebound: 0,
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
  SemanticRebinding.visit(member, (value) => {
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

const dependencies = (
  self: BodyQuery,
  analysis: ExpressionAnalysis.FunctionAnalysis,
  hidden: ReadonlyArray<Elaboration.FunctionFact>,
): ReadonlyArray<Dependency> => {
  const selected = new Set<string>()
  SemanticRebinding.visit([analysis, hidden], (value) => {
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
  const result = new Map<string, Dependency>()
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
  for (const diagnostic of analysis.diagnostics) {
    if (diagnostic.reason._tag !== 'UnknownImportedMember') continue
    const key = `${diagnostic.reason.module}/${diagnostic.reason.spelling}`
    if (!result.has(key)) result.set(key, { key, signature: self.signatures.get(key) })
  }
  return [...result.values()].sort((left, right) => left.key.localeCompare(right.key))
}

const callsOf = (
  self: BodyQuery,
  analysis: ExpressionAnalysis.FunctionAnalysis,
  hidden: ReadonlyArray<Elaboration.FunctionFact>,
): ReadonlyArray<string> => {
  const calls = new Set<string>()
  SemanticRebinding.visit([analysis, hidden], (value) => {
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

const correspondence = (previous: Entry, self: BodyQuery): SemanticRebinding.SemanticRebinding => {
  const cached = self.rebindings.get(previous.index)
  if (cached !== undefined) return cached
  const result = SemanticRebinding.make()
  self.rebindings.set(previous.index, result)
  for (const [key, oldMember] of membersOf(previous.index)) {
    const current = self.members.get(key)
    if (current === undefined) continue
    SemanticRebinding.pair(result, oldMember, current)
  }
  return result
}

const validateDependencies = (
  self: BodyQuery,
  dependencies: ReadonlyArray<Dependency>,
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
    return body === undefined || validateDependencies(self, body.dependencies, visited)
  })

/** Runs the body checker only when its own implementation or a consumed input changed. */
export const check = (
  self: BodyQuery,
  context: SemanticContext.SemanticContext,
  authored: AuthoredLowering.Lowered,
  scope: NameResolution.ModuleScope,
  declaration: DeclarationFacts.DeclarationFact,
  hiddenFunctions: Array<Elaboration.FunctionFact>,
  compute: () => ExpressionAnalysis.FunctionAnalysis,
): ExpressionAnalysis.FunctionAnalysis => {
  const key = memberKey(declaration)
  const prior = self.previous.get(key)
  const signature = self.signatures.get(key) ?? ModuleSurface.memberSignature(declaration)
  const bodyKey = implementation(authored, declaration)
  const scopeKey = scopeSignature(authored, declaration, scope)
  const valid =
    prior !== undefined &&
    AuthoredIdentity.equals(prior.context.module.owner, context.module.owner) &&
    prior.signature === signature &&
    prior.implementation === bodyKey &&
    prior.scope === scopeKey &&
    prior.resolution === self.resolution &&
    validateDependencies(self, prior.dependencies)
  let analysis: ExpressionAnalysis.FunctionAnalysis
  let hidden: ReadonlyArray<Elaboration.FunctionFact>
  if (valid && prior !== undefined) {
    self.work.reused += 1
    const previousMembers = membersOf(prior.index)
    // An anchor is stable across revisions, so identity of the fact object decides reuse.
    const unchanged =
      prior.declaration === declaration &&
      prior.dependencies.every(
        (dependency) => previousMembers.get(dependency.key) === self.members.get(dependency.key),
      )
    const rebinding = unchanged ? undefined : correspondence(prior, self)
    if (rebinding !== undefined) {
      SemanticRebinding.pairPresentations(rebinding, prior.context, context)
      for (const hidden of prior.hidden) {
        // The hidden body keeps its site; only its enclosing declaration's ordinal can move.
        const site =
          hidden.declaration.canonical._tag === 'Canonical'
            ? Tir.anonymousCallableSite(hidden.declaration.canonical.id)
            : undefined
        if (site === undefined) throw new RangeError('Hidden body lost its anonymous callable site')
        SemanticRebinding.pair(rebinding, hidden.declaration.id, {
          ...hidden.declaration.id,
          ordinal: Tir.hiddenDeclarationOrdinal(declaration.id.ordinal, site),
        })
      }
    }
    const rebound =
      rebinding === undefined
        ? { analysis: prior.analysis, hidden: prior.hidden }
        : SemanticRebinding.rebind(rebinding, { analysis: prior.analysis, hidden: prior.hidden })
    analysis = rebound.analysis
    hidden = rebound.hidden
    hiddenFunctions.push(...hidden)
    if (rebinding !== undefined) self.work.rebound += 1
    for (const fact of [analysis.fact, ...hidden])
      self.reuse.set(fact, { prior, ...(rebinding === undefined ? {} : { rebinding }) })
  } else {
    self.work.checked += 1
    const firstHidden = hiddenFunctions.length
    analysis = compute()
    hidden = hiddenFunctions.slice(firstHidden)
  }
  self.entries.set(key, {
    declaration,
    context,
    index: self.index,
    implementation: bodyKey,
    signature,
    scope: scopeKey,
    resolution: self.resolution,
    dependencies:
      valid && prior !== undefined ? prior.dependencies : dependencies(self, analysis, hidden),
    analysis,
    hidden,
    calls: valid && prior !== undefined ? prior.calls : callsOf(self, analysis, hidden),
    ownership: valid && prior !== undefined ? new Map(prior.ownership) : new Map(),
  })
  for (const fact of [analysis.fact, ...hidden]) self.parents.set(fact, key)
  return analysis
}

/** Reuses source ownership only beside a reused semantic body and unchanged access boundaries. */
export const ownership = (
  self: BodyQuery,
  input: Ownership.CheckInput,
  compute: () => Ownership.CheckedFunction,
): Ownership.CheckedFunction => {
  const { semantic: fact, boundaries } = input
  const key = fact === undefined ? undefined : memberKey(fact.declaration)
  const parent = fact === undefined ? undefined : self.parents.get(fact)
  const current = parent === undefined ? undefined : self.entries.get(parent)
  const reuse = fact === undefined ? undefined : self.reuse.get(fact)
  const prior = key === undefined ? undefined : reuse?.prior.ownership.get(key)
  // Rebind only the checked result and spans; the input retains its original index authority.
  const retained =
    prior === undefined ? undefined : { boundaries: prior.input.boundaries, checked: prior.checked }
  const rebound =
    retained === undefined || reuse?.rebinding === undefined
      ? retained
      : SemanticRebinding.rebind(reuse.rebinding, retained)
  let checked: Ownership.CheckedFunction
  if (rebound !== undefined && JSON.stringify(rebound.boundaries) === JSON.stringify(boundaries)) {
    checked = rebound.checked
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
    previous.functions.length === result.functions.length &&
    previous.hiddenFunctions.length === result.hiddenFunctions.length &&
    result.functions.every((fact, ordinal) => fact === previous.functions[ordinal]) &&
    result.hiddenFunctions.every((fact, ordinal) => fact === previous.hiddenFunctions[ordinal])
      ? previous
      : result
  artifacts.set(
    published,
    result.functions.flatMap((fn) => {
      const entry = self.entries.get(memberKey(fn.declaration))
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
