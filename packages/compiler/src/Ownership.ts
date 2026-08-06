import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import type * as Elaboration from './Elaboration.js'
import * as Hir from './Hir.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

/**
 * The ownership and scope phase over typed HIR. It runs once per declaration and is a producer:
 * ownership facts plus the target-neutral cleanup plan MIR lowering consumes to insert drops.
 * Bindings cover parameters and `let` statements; an explicit `move` consumes its binding even
 * for copyable types, and later uses are `OWN0001` violations.
 */

/** The ownership category of one binding. Nominal structs are whole-value move-only owners. */
export type OwnershipCategory =
  | { readonly _tag: 'Copyable' }
  | { readonly _tag: 'MoveOnly'; readonly type: DeclarationIndex.SemanticType }

/** Where one binding was introduced: a parameter or a `let` statement. */
export type BindingSite =
  | { readonly _tag: 'Parameter'; readonly parameter: DeclarationIndex.ParameterId }
  | { readonly _tag: 'Let'; readonly binding: Hir.BindingId }

/** One binding's ownership fact: site, category, live range, and consuming move if any. */
export interface BindingFact {
  readonly _tag: 'Binding'
  readonly site: BindingSite
  readonly name: string | undefined
  readonly mutability: 'Immutable' | 'Mutable'
  readonly category: OwnershipCategory
  readonly type?: DeclarationIndex.SemanticType
  readonly cleanup: CleanupPlan
  readonly liveFrom: SourceSpan.SourceSpan
  readonly liveTo: SourceSpan.SourceSpan
  readonly movedAt?: SourceSpan.SourceSpan
}

/** One ordered release of an owned binding at a structured exit. */
export interface Release {
  readonly _tag: 'Release'
  readonly binding: BindingFact
  readonly fields: ReadonlyArray<DeclarationIndex.FieldId>
  readonly cleanup: CleanupPlan
}

/** The symbolic recursive cleanup of one complete logical owner. */
export type CleanupPlan =
  | { readonly _tag: 'NoCleanup'; readonly type: DeclarationIndex.SemanticType }
  | {
      readonly _tag: 'StructCleanup'
      readonly type: Type.Nominal
      readonly fields: ReadonlyArray<{
        readonly field: DeclarationIndex.FieldId
        readonly cleanup: CleanupPlan
      }>
    }
  | {
      readonly _tag: 'ArrayCleanup'
      readonly type: Type.FixedArray
      readonly length: number
      readonly element: CleanupPlan
    }

/** One structured exit path with its ordered (last-acquired, first-released) releases. */
export interface ExitPlan {
  readonly _tag: 'Exit'
  readonly kind: 'Return' | 'ArmEnd' | 'LoopFallthrough' | 'Break' | 'Continue'
  readonly span: SourceSpan.SourceSpan
  readonly arm?: 'Taken' | 'Otherwise'
  readonly target?: Hir.LoopId
  readonly releases: ReadonlyArray<Release>
}

/** The finite owner-liveness states used to establish one deterministic loop header. */
export interface LoopFixedPoint {
  readonly _tag: 'LoopFixedPoint'
  readonly loop: Hir.LoopId
  readonly span: SourceSpan.SourceSpan
  readonly incoming: ReadonlyArray<BindingSite>
  readonly repeating: ReadonlyArray<ReadonlyArray<BindingSite>>
  readonly following: ReadonlyArray<BindingSite>
  readonly compatible: boolean
  readonly iterations: number
}

/** The closed outcome of checking one function. */
export type Verdict =
  | { readonly _tag: 'Satisfied' }
  | { readonly _tag: 'Violation'; readonly cause: Diagnostic.Identity }
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity }

/** One function's ownership facts and its target-neutral cleanup plan. */
export interface FunctionOwnership {
  readonly _tag: 'FunctionOwnership'
  readonly declaration: DeclarationIndex.DeclarationFact
  readonly bindings: ReadonlyArray<BindingFact>
  readonly exits: ReadonlyArray<ExitPlan>
  readonly fixedPoints: ReadonlyArray<LoopFixedPoint>
  readonly verdict: Verdict
}

/** One module's ownership fact table and its phase diagnostics. */
export interface ModuleOwnership {
  readonly _tag: 'OwnershipFacts'
  readonly module: string
  readonly functions: ReadonlyArray<FunctionOwnership>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const satisfied: Verdict = Object.freeze({ _tag: 'Satisfied' })

const copyable: OwnershipCategory = Object.freeze({ _tag: 'Copyable' })

const categoryOf = (type: DeclarationIndex.SemanticType | undefined): OwnershipCategory =>
  type === undefined || Type.isBuiltin(type)
    ? copyable
    : Type.isFixedArray(type)
      ? categoryOf(type.element)._tag === 'Copyable'
        ? copyable
        : Object.freeze({ _tag: 'MoveOnly', type })
      : Object.freeze({ _tag: 'MoveOnly', type })

const siteKey = (site: BindingSite): string =>
  site._tag === 'Parameter' ? `p${site.parameter.ordinal}` : `b${site.binding.ordinal}`

interface MutableBinding {
  readonly site: BindingSite
  readonly name: string | undefined
  readonly mutability: 'Immutable' | 'Mutable'
  readonly liveFrom: SourceSpan.SourceSpan
  readonly category: OwnershipCategory
  readonly type?: DeclarationIndex.SemanticType
  liveTo: SourceSpan.SourceSpan
  movedAt?: SourceSpan.SourceSpan
}

interface CheckState {
  readonly bindings: Map<string, MutableBinding>
  readonly order: Array<MutableBinding>
  readonly diagnostics: Array<Diagnostic.Diagnostic>
}

const useSite = (expression: Hir.Expression): BindingSite | undefined => {
  switch (expression._tag) {
    case 'ParameterReference':
      return Object.freeze({ _tag: 'Parameter', parameter: expression.parameter })
    case 'BindingReference':
      return Object.freeze({ _tag: 'Let', binding: expression.binding })
    default:
      return undefined
  }
}

const checkUse = (
  state: CheckState,
  live: Set<string>,
  site: BindingSite,
  span: SourceSpan.SourceSpan,
  consuming: boolean,
): void => {
  const key = siteKey(site)
  const binding = state.bindings.get(key)
  if (binding === undefined) return
  if (!live.has(key)) {
    state.diagnostics.push(
      Diagnostic.useAfterMove(binding.name ?? '?', binding.movedAt ?? binding.liveTo, span),
    )
    return
  }
  if (consuming) {
    binding.movedAt ??= span
    binding.liveTo = span
    live.delete(key)
  }
}

const checkExpression = (
  state: CheckState,
  live: Set<string>,
  expression: Hir.Expression,
  consuming: boolean,
): void => {
  switch (expression._tag) {
    case 'ParameterReference':
    case 'BindingReference': {
      const site = useSite(expression)
      if (site === undefined) return
      const binding = state.bindings.get(siteKey(site))
      if (consuming && binding?.category._tag === 'MoveOnly') {
        state.diagnostics.push(
          Diagnostic.explicitMoveRequired(binding.name ?? '?', expression.span),
        )
      }
      checkUse(state, live, site, expression.span, false)
      return
    }
    case 'Move': {
      if (expression.subject._tag === 'Project' || expression.subject._tag === 'IndexPlace') {
        checkExpression(state, live, expression.subject, false)
        state.diagnostics.push(Diagnostic.partialMove(expression.span))
        return
      }
      const site = useSite(expression.subject)
      if (site !== undefined) checkUse(state, live, site, expression.span, true)
      else checkExpression(state, live, expression.subject, true)
      return
    }
    case 'Construct': {
      const fields = new Map(
        expression.fields.map((field) => [field.field.ordinal, field.value] as const),
      )
      for (const field of expression.evaluationOrder) {
        const value = fields.get(field.ordinal)
        if (value !== undefined) checkExpression(state, live, value, true)
      }
      return
    }
    case 'ArrayConstruct': {
      for (const element of expression.elements) checkExpression(state, live, element, true)
      return
    }
    case 'Project': {
      checkExpression(state, live, expression.subject, false)
      if (consuming && categoryOf(expression.type)._tag === 'MoveOnly') {
        state.diagnostics.push(Diagnostic.partialMove(expression.span))
      }
      return
    }
    case 'IndexPlace': {
      checkExpression(state, live, expression.subject, false)
      checkExpression(state, live, expression.index, false)
      if (consuming && categoryOf(expression.type)._tag === 'MoveOnly') {
        state.diagnostics.push(Diagnostic.partialMove(expression.span))
      }
      return
    }
    case 'BuiltinCall': {
      for (const argument of expression.arguments) checkExpression(state, live, argument, false)
      return
    }
    case 'Call': {
      for (const argument of expression.arguments) checkExpression(state, live, argument, true)
      return
    }
    default:
      return
  }
}

interface CheckedFunction {
  readonly ownership: FunctionOwnership
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

interface ExitDescriptor {
  readonly kind: ExitPlan['kind']
  readonly span: SourceSpan.SourceSpan
  readonly arm?: 'Taken' | 'Otherwise'
  readonly target?: Hir.LoopId
  readonly sites: ReadonlyArray<string>
}

const cleanupFields = (
  index: DeclarationIndex.Index,
  type: Type.Nominal,
  seen = new Set<string>(),
): ReadonlyArray<DeclarationIndex.FieldId> => {
  const key = Type.key(type)
  if (seen.has(key)) return Object.freeze([])
  const nextSeen = new Set(seen).add(key)
  const declaration = DeclarationIndex.byCanonical(index, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (declaration?._tag !== 'StructDeclaration') return Object.freeze([])
  return Object.freeze(
    declaration.fields.flatMap((field) => {
      if (field.declaredType._tag !== 'Resolved') return [field.id]
      const nested = field.declaredType.type
      return Type.isNominal(nested)
        ? [field.id, ...cleanupFields(index, nested, nextSeen)]
        : [field.id]
    }),
  )
}

const cleanupPlan = (
  index: DeclarationIndex.Index,
  type: DeclarationIndex.SemanticType,
  seen = new Set<string>(),
): CleanupPlan => {
  if (Type.isBuiltin(type)) return Object.freeze({ _tag: 'NoCleanup', type })
  if (Type.isFixedArray(type)) {
    return Object.freeze({
      _tag: 'ArrayCleanup',
      type,
      length: type.length,
      element: cleanupPlan(index, type.element, seen),
    })
  }
  const key = Type.key(type)
  if (seen.has(key)) return Object.freeze({ _tag: 'NoCleanup', type })
  const declaration = DeclarationIndex.byCanonical(index, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (declaration?._tag !== 'StructDeclaration') {
    return Object.freeze({ _tag: 'NoCleanup', type })
  }
  const nextSeen = new Set(seen).add(key)
  return Object.freeze({
    _tag: 'StructCleanup',
    type,
    fields: Object.freeze(
      declaration.fields.map((field) =>
        Object.freeze({
          field: field.id,
          cleanup:
            field.declaredType._tag === 'Resolved'
              ? cleanupPlan(index, field.declaredType.type, nextSeen)
              : Object.freeze({ _tag: 'NoCleanup' as const, type: 'I32' as const }),
        }),
      ),
    ),
  })
}

const checkFunction = (fn: Hir.HirFunction, index: DeclarationIndex.Index): CheckedFunction => {
  const declaration = fn.declaration
  const state: CheckState = {
    bindings: new Map(),
    order: [],
    diagnostics: [],
  }

  const initialLive = new Set<string>()
  for (const parameter of declaration.parameters) {
    const type =
      parameter.declaredType._tag === 'Resolved' ? parameter.declaredType.type : undefined
    const binding: MutableBinding = {
      site: Object.freeze({ _tag: 'Parameter', parameter: parameter.id }),
      name: parameter.name._tag === 'Present' ? parameter.name.spelling : undefined,
      mutability: 'Immutable',
      liveFrom: parameter.syntax.span,
      liveTo: declaration.syntax.span,
      category: categoryOf(type),
      ...(type === undefined ? {} : { type }),
    }
    const key = siteKey(binding.site)
    state.bindings.set(key, binding)
    state.order.push(binding)
    initialLive.add(key)
  }

  const exits: Array<ExitDescriptor> = []
  const continueStates = new Map<number, Array<Set<string>>>()
  const breakStates = new Map<number, Array<Set<string>>>()
  const fixedPoints: Array<{
    readonly loop: Hir.LoopId
    readonly span: SourceSpan.SourceSpan
    readonly incoming: Set<string>
    readonly repeating: ReadonlyArray<Set<string>>
    readonly following: Set<string>
    readonly compatible: boolean
    readonly iterations: number
  }> = []
  const appendLoopState = (
    states: Map<number, Array<Set<string>>>,
    loop: Hir.LoopId,
    live: Set<string>,
  ): void => {
    const existing = states.get(loop.ordinal)
    if (existing === undefined) states.set(loop.ordinal, [new Set(live)])
    else existing.push(new Set(live))
  }
  const sameLive = (left: ReadonlySet<string>, right: ReadonlySet<string>): boolean =>
    left.size === right.size && [...left].every((site) => right.has(site))
  const intersection = (states: ReadonlyArray<ReadonlySet<string>>): Set<string> => {
    const [first, ...rest] = states
    return new Set([...(first ?? [])].filter((site) => rest.every((state) => state.has(site))))
  }
  const frameSitesInnerFirst = (
    frames: ReadonlyArray<ReadonlyArray<string>>,
    live: ReadonlySet<string>,
  ): ReadonlyArray<string> =>
    [...frames].reverse().flatMap((frame) => [...frame].reverse().filter((site) => live.has(site)))

  const walkStatements = (
    statements: ReadonlyArray<Hir.Statement>,
    enclosingSpan: SourceSpan.SourceSpan,
    initial: Set<string>,
    frames: Array<Array<string>>,
    loopScopes: ReadonlyArray<{ readonly loop: Hir.LoopId; readonly frame: number }> = [],
  ): { readonly returned: boolean; readonly live: Set<string> } => {
    let live = initial
    for (const statement of statements) {
      if (statement._tag === 'Bind') {
        checkExpression(state, live, statement.initializer, true)
        const type =
          statement.initializer._tag === 'Unavailable' ? undefined : statement.initializer.type
        const binding: MutableBinding = {
          site: Object.freeze({ _tag: 'Let', binding: statement.binding }),
          name: statement.name,
          mutability: statement.mutability,
          liveFrom: statement.span,
          liveTo: enclosingSpan,
          category: categoryOf(type),
          ...(type === undefined ? {} : { type }),
        }
        const key = siteKey(binding.site)
        state.bindings.set(key, binding)
        state.order.push(binding)
        frames.at(-1)?.push(key)
        live.add(key)
        continue
      }
      if (statement._tag === 'If') {
        checkExpression(state, live, statement.condition, false)
        const continuing: Array<Set<string>> = []
        for (const [arm, body] of [
          ['Taken', statement.taken],
          ['Otherwise', statement.otherwise],
        ] as const) {
          const armFrames = [...frames.map((frame) => [...frame]), []]
          const result = walkStatements(body, statement.span, new Set(live), armFrames, loopScopes)
          const frame = armFrames.at(-1) ?? []
          if (!result.returned && frame.length > 0) {
            exits.push(
              Object.freeze({
                kind: 'ArmEnd' as const,
                span: statement.span,
                arm,
                sites: Object.freeze([...frame].reverse().filter((site) => result.live.has(site))),
              }),
            )
          }
          if (!result.returned) {
            for (const site of frame) result.live.delete(site)
            continuing.push(result.live)
          }
        }
        if (continuing.length === 0) return Object.freeze({ returned: true, live })
        live = new Set(
          [...(continuing.at(0) ?? [])].filter((site) =>
            continuing.every((candidate) => candidate.has(site)),
          ),
        )
        continue
      }
      if (statement._tag === 'Write') {
        for (const selector of statement.place.selectors) {
          if (selector._tag === 'Index') checkExpression(state, live, selector.index, false)
        }
        const rootSite: BindingSite = Object.freeze({
          _tag: 'Let',
          binding: statement.place.root,
        })
        const rootKey = siteKey(rootSite)
        const root = state.bindings.get(rootKey)
        const wasLive = live.has(rootKey)
        if (!wasLive && statement.place.selectors.length > 0 && root !== undefined) {
          checkUse(state, live, rootSite, statement.place.span, false)
        }
        checkExpression(state, live, statement.value, true)
        if (wasLive && !live.has(rootKey)) {
          state.diagnostics.push(
            Diagnostic.overlappingAssignment(root?.name ?? '?', statement.span),
          )
        } else if (statement.place.selectors.length === 0) {
          live.add(rootKey)
        }
        continue
      }
      if (statement._tag === 'While') {
        checkExpression(state, live, statement.condition, false)
        const incoming = new Set(live)
        const previousContinues = continueStates.get(statement.loop.ordinal)?.length ?? 0
        const previousBreaks = breakStates.get(statement.loop.ordinal)?.length ?? 0
        const loopFrames = [...frames.map((frame) => [...frame]), []]
        const loopResult = walkStatements(
          statement.body,
          statement.span,
          new Set(live),
          loopFrames,
          [...loopScopes, { loop: statement.loop, frame: loopFrames.length - 1 }],
        )
        const loopFrame = loopFrames.at(-1) ?? []
        const repeating: Array<Set<string>> = [
          ...(continueStates.get(statement.loop.ordinal)?.slice(previousContinues) ?? []),
        ]
        if (!loopResult.returned) {
          exits.push(
            Object.freeze({
              kind: 'LoopFallthrough' as const,
              span: statement.span,
              target: statement.loop,
              sites: Object.freeze(
                [...loopFrame].reverse().filter((site) => loopResult.live.has(site)),
              ),
            }),
          )
          repeating.push(new Set(loopResult.live))
        }
        for (const candidate of repeating) {
          for (const site of loopFrame) candidate.delete(site)
        }
        const compatible = repeating.every((candidate) => sameLive(candidate, incoming))
        if (!compatible) {
          state.diagnostics.push(
            Diagnostic.incompatibleLoopHeader(statement.loop.ordinal, statement.span),
          )
        }
        const exitsFromLoop = breakStates.get(statement.loop.ordinal)?.slice(previousBreaks) ?? []
        for (const candidate of exitsFromLoop) {
          for (const site of loopFrame) candidate.delete(site)
        }
        live = intersection([incoming, ...exitsFromLoop])
        fixedPoints.push({
          loop: statement.loop,
          span: statement.span,
          incoming,
          repeating: Object.freeze(repeating.map((candidate) => new Set(candidate))),
          following: new Set(live),
          compatible,
          iterations: repeating.length === 0 ? 1 : 2,
        })
        continue
      }
      if (statement._tag === 'Break' || statement._tag === 'Continue') {
        const targetScope = [...loopScopes]
          .reverse()
          .find((scope) => scope.loop.ordinal === statement.target.ordinal)
        const transferFrames =
          targetScope === undefined ? [frames.at(-1) ?? []] : frames.slice(targetScope.frame)
        const transferSites = [...transferFrames].reverse().flatMap((frame) => [...frame].reverse())
        const sites = Object.freeze(transferSites.filter((site) => live.has(site)))
        exits.push(
          Object.freeze({
            kind: statement._tag,
            span: statement.span,
            target: statement.target,
            sites,
          }),
        )
        const next = new Set(live)
        for (const site of transferSites) next.delete(site)
        appendLoopState(
          statement._tag === 'Break' ? breakStates : continueStates,
          statement.target,
          next,
        )
        return Object.freeze({ returned: true, live })
      }
      if (statement._tag === 'UnavailableStatement') {
        continue
      }
      checkExpression(state, live, statement.expression, true)
      exits.push(
        Object.freeze({
          kind: 'Return' as const,
          span: statement.span,
          sites: frameSitesInnerFirst(frames, live),
        }),
      )
      return Object.freeze({ returned: true, live })
    }
    return Object.freeze({ returned: false, live })
  }

  const rootFrame = state.order
    .filter((binding) => binding.category._tag === 'MoveOnly')
    .map((binding) => siteKey(binding.site))
  const result = walkStatements(fn.statements, declaration.syntax.span, initialLive, [rootFrame])
  if (!result.returned) {
    exits.push(
      Object.freeze({
        kind: 'Return' as const,
        span: fn.statements.at(-1)?.span ?? declaration.syntax.span,
        sites: frameSitesInnerFirst([rootFrame], result.live),
      }),
    )
  }

  const bindings = Object.freeze(
    state.order.map(
      (binding): BindingFact =>
        Object.freeze({
          _tag: 'Binding',
          site: binding.site,
          name: binding.name,
          mutability: binding.mutability,
          category: binding.category,
          ...(binding.type === undefined ? {} : { type: binding.type }),
          cleanup:
            binding.type === undefined
              ? Object.freeze({ _tag: 'NoCleanup' as const, type: 'I32' as const })
              : cleanupPlan(index, binding.type),
          liveFrom: binding.liveFrom,
          liveTo: binding.liveTo,
          ...(binding.movedAt === undefined ? {} : { movedAt: binding.movedAt }),
        }),
    ),
  )
  const bindingBySite = new Map(bindings.map((binding) => [siteKey(binding.site), binding]))

  const exitPlans = Object.freeze(
    exits.map(
      (exit): ExitPlan =>
        Object.freeze({
          _tag: 'Exit' as const,
          kind: exit.kind,
          span: exit.span,
          ...(exit.arm === undefined ? {} : { arm: exit.arm }),
          ...(exit.target === undefined ? {} : { target: exit.target }),
          releases: Object.freeze(
            exit.sites.flatMap((site): ReadonlyArray<Release> => {
              const fact = bindingBySite.get(site)
              if (fact === undefined) return []
              return [
                Object.freeze({
                  _tag: 'Release' as const,
                  binding: fact,
                  fields:
                    fact.category._tag === 'MoveOnly' && Type.isNominal(fact.category.type)
                      ? cleanupFields(index, fact.category.type)
                      : Object.freeze([]),
                  cleanup: fact.cleanup,
                }),
              ]
            }),
          ),
        }),
    ),
  )

  const firstUnavailable = Hir.firstUnavailable(fn)
  const violation = state.diagnostics.at(0)
  const verdict: Verdict =
    fn.contract._tag === 'Unavailable'
      ? Object.freeze({
          _tag: 'Unavailable',
          ...(fn.contract.cause === undefined ? {} : { cause: fn.contract.cause }),
        })
      : firstUnavailable !== undefined
        ? Object.freeze({
            _tag: 'Unavailable',
            ...(firstUnavailable.cause === undefined ? {} : { cause: firstUnavailable.cause }),
          })
        : violation !== undefined
          ? Object.freeze({ _tag: 'Violation', cause: Diagnostic.identity(violation) })
          : satisfied

  return Object.freeze({
    ownership: Object.freeze({
      _tag: 'FunctionOwnership' as const,
      declaration,
      bindings,
      exits: exitPlans,
      fixedPoints: Object.freeze(
        fixedPoints.map((point) => {
          const sites = (keys: ReadonlySet<string>): ReadonlyArray<BindingSite> =>
            Object.freeze(
              [...keys].flatMap((key): ReadonlyArray<BindingSite> => {
                const binding = bindingBySite.get(key)
                return binding === undefined ? [] : [binding.site]
              }),
            )
          return Object.freeze({
            _tag: 'LoopFixedPoint' as const,
            loop: point.loop,
            span: point.span,
            incoming: sites(point.incoming),
            repeating: Object.freeze(point.repeating.map(sites)),
            following: sites(point.following),
            compatible: point.compatible,
            iterations: point.iterations,
          })
        }),
      ),
      verdict,
    }),
    diagnostics: Object.freeze([...state.diagnostics]),
  })
}

/** Checks every declaration of one elaborated module once, producing its ownership facts. */
export const checkModule = (result: Elaboration.Result): ModuleOwnership => {
  const checked = result.hir.functions.map((fn) => checkFunction(fn, result.index))
  return Object.freeze({
    _tag: 'OwnershipFacts',
    module: result.syntax.source.id,
    functions: Object.freeze(checked.map((entry) => entry.ownership)),
    diagnostics: Object.freeze(
      checked.flatMap((entry) => entry.diagnostics).sort(Diagnostic.compare),
    ),
  })
}

const spanText = (span: SourceSpan.SourceSpan): string => `[${span.start}, ${span.end})`

const identityLabel = (declaration: DeclarationIndex.DeclarationFact): string => {
  switch (declaration.canonical._tag) {
    case 'Canonical':
      return `${declaration.canonical.id.module}.${declaration.canonical.id.name}`
    case 'Duplicate':
      return `duplicate:${declaration.canonical.original.module}.${declaration.canonical.original.name}#${declaration.id.ordinal}`
    case 'Unidentified':
      return `unidentified#${declaration.id.ordinal}`
  }
}

const verdictText = (verdict: Verdict): string => {
  switch (verdict._tag) {
    case 'Satisfied':
      return 'satisfied'
    case 'Violation':
      return 'violation'
    case 'Unavailable':
      return 'unavailable'
  }
}

const siteText = (site: BindingSite): string =>
  site._tag === 'Parameter' ? `p${site.parameter.ordinal}` : `b${site.binding.ordinal}`

const cleanupText = (cleanup: CleanupPlan): string => {
  if (cleanup._tag === 'NoCleanup') return `none:${Type.encode(cleanup.type)}`
  if (cleanup._tag === 'ArrayCleanup') {
    return `array:${Type.encode(cleanup.type)} length=${cleanup.length} element=(${cleanupText(cleanup.element)})`
  }
  return `struct:${Type.encode(cleanup.type)} fields=${cleanup.fields
    .map((field) => `#${field.field.ordinal}(${cleanupText(field.cleanup)})`)
    .join(',')}`
}

/**
 * Deterministic textual encoding of one module's ownership facts and cleanup plans for
 * debugging, inspection, and golden tests. No compatibility promise attaches to this format.
 */
export const encode = (self: ModuleOwnership): string =>
  [
    `ownership-module ${self.module}`,
    ...self.functions.flatMap((fn) => [
      `fn ${identityLabel(fn.declaration)} ${verdictText(fn.verdict)}`,
      ...fn.bindings.map((binding) => {
        const category =
          binding.category._tag === 'Copyable'
            ? 'copyable'
            : `move-only ${Type.encode(binding.category.type)}`
        return `  binding ${siteText(binding.site)} ${binding.name ?? '?'} ${category} live ${spanText(binding.liveFrom)}..${spanText(binding.liveTo)}${binding.movedAt === undefined ? '' : ` moved ${spanText(binding.movedAt)}`}`
      }),
      ...fn.exits.map((exit) => {
        const label = (() => {
          switch (exit.kind) {
            case 'Return':
              return 'return'
            case 'ArmEnd':
              return `arm-end ${exit.arm === 'Otherwise' ? 'otherwise' : 'taken'}`
            case 'LoopFallthrough':
              return `loop${exit.target?.ordinal ?? '?'} fallthrough`
            case 'Break':
              return `break loop${exit.target?.ordinal ?? '?'}`
            case 'Continue':
              return `continue loop${exit.target?.ordinal ?? '?'}`
          }
        })()
        return exit.releases.length === 0
          ? `  exit ${label} ${spanText(exit.span)} releases none`
          : [
              `  exit ${label} ${spanText(exit.span)}`,
              ...exit.releases.map(
                (release) =>
                  `    release ${siteText(release.binding.site)}${release.fields.length === 0 ? '' : ` fields ${release.fields.map((field) => `#${field.ordinal}`).join(',')}`}${release.cleanup._tag === 'ArrayCleanup' ? ` cleanup ${cleanupText(release.cleanup)}` : ''}`,
              ),
            ].join('\n')
      }),
      ...fn.fixedPoints.map(
        (point) =>
          `  loop${point.loop.ordinal} fixed-point ${point.compatible ? 'compatible' : 'incompatible'} iterations=${point.iterations} incoming=${point.incoming.map(siteText).join(',') || 'none'} repeating=${point.repeating.map((state) => `[${state.map(siteText).join(',')}]`).join(',') || 'none'} following=${point.following.map(siteText).join(',') || 'none'}`,
      ),
    ]),
    '',
  ].join('\n')
