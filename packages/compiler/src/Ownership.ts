import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import type * as Elaboration from './Elaboration.js'
import * as Hir from './Hir.js'
import type * as SourceSpan from './SourceSpan.js'

/**
 * The ownership and scope phase over typed HIR. It runs once per declaration and is a producer:
 * ownership facts plus the target-neutral cleanup plan MIR lowering consumes to insert drops.
 * Bindings cover parameters and `let` statements; an explicit `move` consumes its binding even
 * for copyable types, and later uses are `OWN0001` violations.
 */

/** The ownership category of one binding. The bootstrap slice knows only copyable values. */
export type OwnershipCategory = { readonly _tag: 'Copyable' }

/** Where one binding was introduced: a parameter or a `let` statement. */
export type BindingSite =
  | { readonly _tag: 'Parameter'; readonly parameter: DeclarationIndex.ParameterId }
  | { readonly _tag: 'Let'; readonly binding: Hir.BindingId }

/** One binding's ownership fact: site, category, live range, and consuming move if any. */
export interface BindingFact {
  readonly _tag: 'Binding'
  readonly site: BindingSite
  readonly name: string | undefined
  readonly category: OwnershipCategory
  readonly liveFrom: SourceSpan.SourceSpan
  readonly liveTo: SourceSpan.SourceSpan
  readonly movedAt?: SourceSpan.SourceSpan
}

/** One ordered release of an owned binding at a structured exit. */
export interface Release {
  readonly _tag: 'Release'
  readonly binding: BindingFact
}

/** One structured exit path with its ordered (last-acquired, first-released) releases. */
export interface ExitPlan {
  readonly _tag: 'Exit'
  readonly kind: 'Return'
  readonly span: SourceSpan.SourceSpan
  readonly releases: ReadonlyArray<Release>
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

const siteKey = (site: BindingSite): string =>
  site._tag === 'Parameter' ? `p${site.parameter.ordinal}` : `b${site.binding.ordinal}`

interface MutableBinding {
  readonly site: BindingSite
  readonly name: string | undefined
  readonly liveFrom: SourceSpan.SourceSpan
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
  site: BindingSite,
  span: SourceSpan.SourceSpan,
  consuming: boolean,
): void => {
  const binding = state.bindings.get(siteKey(site))
  if (binding === undefined) return
  if (binding.movedAt !== undefined) {
    state.diagnostics.push(Diagnostic.useAfterMove(binding.name ?? '?', binding.movedAt, span))
    return
  }
  if (consuming) {
    binding.movedAt = span
    binding.liveTo = span
  }
}

const checkExpression = (state: CheckState, expression: Hir.Expression): void => {
  switch (expression._tag) {
    case 'ParameterReference':
    case 'BindingReference': {
      const site = useSite(expression)
      if (site !== undefined) checkUse(state, site, expression.span, false)
      return
    }
    case 'Move': {
      const site = useSite(expression.subject)
      if (site !== undefined) checkUse(state, site, expression.span, true)
      return
    }
    case 'Call':
    case 'BuiltinCall': {
      for (const argument of expression.arguments) checkExpression(state, argument)
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

const checkFunction = (fn: Hir.HirFunction): CheckedFunction => {
  const declaration = fn.declaration
  const state: CheckState = {
    bindings: new Map(),
    order: [],
    diagnostics: [],
  }

  for (const parameter of declaration.parameters) {
    const binding: MutableBinding = {
      site: Object.freeze({ _tag: 'Parameter', parameter: parameter.id }),
      name: parameter.name._tag === 'Present' ? parameter.name.spelling : undefined,
      liveFrom: parameter.syntax.span,
      liveTo: declaration.syntax.span,
    }
    state.bindings.set(siteKey(binding.site), binding)
    state.order.push(binding)
  }

  const letBindings: Array<MutableBinding> = []
  for (const statement of fn.statements) {
    if (statement._tag === 'Bind') {
      checkExpression(state, statement.initializer)
      const binding: MutableBinding = {
        site: Object.freeze({ _tag: 'Let', binding: statement.binding }),
        name: statement.name,
        liveFrom: statement.span,
        liveTo: declaration.syntax.span,
      }
      state.bindings.set(siteKey(binding.site), binding)
      state.order.push(binding)
      letBindings.push(binding)
    } else {
      checkExpression(state, statement.expression)
    }
  }

  const bindings = Object.freeze(
    state.order.map(
      (binding): BindingFact =>
        Object.freeze({
          _tag: 'Binding',
          site: binding.site,
          name: binding.name,
          category: copyable,
          liveFrom: binding.liveFrom,
          liveTo: binding.liveTo,
          ...(binding.movedAt === undefined ? {} : { movedAt: binding.movedAt }),
        }),
    ),
  )
  const bindingBySite = new Map(bindings.map((binding) => [siteKey(binding.site), binding]))

  const returnSpan = fn.statements.at(-1)?.span ?? declaration.syntax.span
  const releases = Object.freeze(
    [...letBindings]
      .reverse()
      .filter((binding) => binding.movedAt === undefined)
      .flatMap((binding): ReadonlyArray<Release> => {
        const fact = bindingBySite.get(siteKey(binding.site))
        return fact === undefined
          ? []
          : [Object.freeze({ _tag: 'Release' as const, binding: fact })]
      }),
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
      exits: Object.freeze([
        Object.freeze({
          _tag: 'Exit' as const,
          kind: 'Return' as const,
          span: returnSpan,
          releases,
        }),
      ]),
      verdict,
    }),
    diagnostics: Object.freeze([...state.diagnostics]),
  })
}

/** Checks every declaration of one elaborated module once, producing its ownership facts. */
export const checkModule = (result: Elaboration.Result): ModuleOwnership => {
  const checked = result.hir.functions.map(checkFunction)
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

/**
 * Deterministic textual encoding of one module's ownership facts and cleanup plans for
 * debugging, inspection, and golden tests. No compatibility promise attaches to this format.
 */
export const encode = (self: ModuleOwnership): string =>
  [
    `ownership-module ${self.module}`,
    ...self.functions.flatMap((fn) => [
      `fn ${identityLabel(fn.declaration)} ${verdictText(fn.verdict)}`,
      ...fn.bindings.map(
        (binding) =>
          `  binding ${siteText(binding.site)} ${binding.name ?? '?'} copyable live ${spanText(binding.liveFrom)}..${spanText(binding.liveTo)}${binding.movedAt === undefined ? '' : ` moved ${spanText(binding.movedAt)}`}`,
      ),
      ...fn.exits.map((exit) =>
        exit.releases.length === 0
          ? `  exit return ${spanText(exit.span)} releases none`
          : [
              `  exit return ${spanText(exit.span)}`,
              ...exit.releases.map((release) => `    release ${siteText(release.binding.site)}`),
            ].join('\n'),
      ),
    ]),
    '',
  ].join('\n')
