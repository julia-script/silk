import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Diagnostic from './Diagnostic.js'
import type * as Elaboration from './Elaboration.js'
import type * as SourceSpan from './SourceSpan.js'

/**
 * The ownership and scope phase over typed HIR. It runs once per declaration and is a producer:
 * ownership facts plus the target-neutral cleanup plan MIR lowering consumes to insert drops.
 * On the frozen slice every value is a copyable `I32`, so verdicts are satisfied and every exit
 * releases nothing — the phase boundary, fact table, and artifact are what this module pins.
 */

/** The ownership category of one binding. The frozen slice knows only copyable values. */
export type OwnershipCategory = { readonly _tag: 'Copyable' }

/** One binding's ownership fact: identity, category, and live range over source spans. */
export interface BindingFact {
  readonly _tag: 'Binding'
  readonly parameter: DeclarationIndex.ParameterId
  readonly name: string | undefined
  readonly category: OwnershipCategory
  readonly liveFrom: SourceSpan.SourceSpan
  readonly liveTo: SourceSpan.SourceSpan
}

/** One ordered release of an owned binding at a structured exit. Empty in the frozen slice. */
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
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity }

/** One function's ownership facts and its target-neutral cleanup plan. */
export interface FunctionOwnership {
  readonly _tag: 'FunctionOwnership'
  readonly declaration: DeclarationIndex.DeclarationFact
  readonly bindings: ReadonlyArray<BindingFact>
  readonly exits: ReadonlyArray<ExitPlan>
  readonly verdict: Verdict
}

/** One module's ownership fact table. */
export interface ModuleOwnership {
  readonly _tag: 'OwnershipFacts'
  readonly module: string
  readonly functions: ReadonlyArray<FunctionOwnership>
}

const satisfied: Verdict = Object.freeze({ _tag: 'Satisfied' })

const checkFunction = (fn: Elaboration.Result['hir']['functions'][number]): FunctionOwnership => {
  const declaration = fn.declaration
  const bindings = Object.freeze(
    declaration.parameters.map(
      (parameter): BindingFact =>
        Object.freeze({
          _tag: 'Binding',
          parameter: parameter.id,
          name: parameter.name._tag === 'Present' ? parameter.name.spelling : undefined,
          category: Object.freeze({ _tag: 'Copyable' }),
          liveFrom: parameter.syntax.span,
          liveTo: declaration.syntax.span,
        }),
    ),
  )
  const verdict: Verdict =
    fn.contract._tag === 'Unavailable'
      ? Object.freeze({
          _tag: 'Unavailable',
          ...(fn.contract.cause === undefined ? {} : { cause: fn.contract.cause }),
        })
      : fn.body._tag === 'Unavailable'
        ? Object.freeze({
            _tag: 'Unavailable',
            ...(fn.body.cause === undefined ? {} : { cause: fn.body.cause }),
          })
        : satisfied

  return Object.freeze({
    _tag: 'FunctionOwnership',
    declaration,
    bindings,
    exits: Object.freeze([
      Object.freeze({
        _tag: 'Exit' as const,
        kind: 'Return' as const,
        span: fn.body.span,
        releases: Object.freeze([]),
      }),
    ]),
    verdict,
  })
}

/** Checks every declaration of one elaborated module once, producing its ownership facts. */
export const checkModule = (result: Elaboration.Result): ModuleOwnership =>
  Object.freeze({
    _tag: 'OwnershipFacts',
    module: result.syntax.source.id,
    functions: Object.freeze(result.hir.functions.map(checkFunction)),
  })

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

const verdictText = (verdict: Verdict): string =>
  verdict._tag === 'Satisfied' ? 'satisfied' : 'unavailable'

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
          `  binding p${binding.parameter.ordinal} ${binding.name ?? '?'} copyable live ${spanText(binding.liveFrom)}..${spanText(binding.liveTo)}`,
      ),
      ...fn.exits.map((exit) =>
        exit.releases.length === 0
          ? `  exit return ${spanText(exit.span)} releases none`
          : [
              `  exit return ${spanText(exit.span)}`,
              ...exit.releases.map(
                (release) => `    release p${release.binding.parameter.ordinal}`,
              ),
            ].join('\n'),
      ),
    ]),
    '',
  ].join('\n')
