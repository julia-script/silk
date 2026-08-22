import type * as CleanupPlan from './CleanupPlan.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import * as ExecutionAffinity from './ExecutionAffinity.js'
import * as Hir from './Hir.js'
import * as LocalSharedOwnership from './LocalSharedOwnership.js'
import type { BindingSite, ModuleOwnership, Verdict } from './Ownership.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

const spanText = (span: SourceSpan.SourceSpan): string => `[${span.start}, ${span.end})`

const identityLabel = (declaration: DeclarationFacts.DeclarationFact): string => {
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
  site._tag === 'Parameter'
    ? `p${site.parameter.ordinal}`
    : site._tag === 'Let'
      ? `b${site.binding.ordinal}`
      : site._tag === 'Pattern'
        ? `m${site.binding.arm.match.span.start}.a${site.binding.arm.ordinal}.p${site.binding.ordinal}`
        : `t${site.owner.span.start}.${site.owner.ordinal}`

const cleanupText = (cleanup: CleanupPlan.CleanupPlan): string => {
  if (cleanup._tag === 'NoCleanup') return `none:${Type.encode(cleanup.type)}`
  if (cleanup._tag === 'ParameterCleanup') return `parameter:${Type.key(cleanup.type)}`
  if (cleanup._tag === 'AllocationCleanup')
    return `allocation:${Type.encode(cleanup.type)} ticket=${cleanup.ticket}`
  if (cleanup._tag === 'RawBufferCleanup')
    return `raw-buffer:${Type.encode(cleanup.type)} owner=(${cleanupText(cleanup.allocation)})`
  if (cleanup._tag === 'ArrayCleanup') {
    return `array:${Type.encode(cleanup.type)} length=${cleanup.length} element=(${cleanupText(cleanup.element)})`
  }
  if (cleanup._tag === 'UnionCleanup') {
    return `union:${Type.encode(cleanup.type)} cases=${cleanup.cases
      .map(
        (member) =>
          `${member.ordinal}:${Type.encode(member.member)}(${cleanupText(member.cleanup)})`,
      )
      .join(',')}`
  }
  if (cleanup._tag === 'CallableCleanup') {
    const environment =
      cleanup.environment._tag === 'CallableEnvironmentSite'
        ? Hir.executableSiteLabel(cleanup.environment.site)
        : Type.callableEnvironmentKey(cleanup.environment.identity)
    return `callable:${Type.encode(cleanup.type)} environment=${environment} slots=${cleanup.slots.map((slot) => `#${slot.ordinal}(${cleanupText(slot.cleanup)})`).join(',') || 'none'}`
  }
  if (cleanup._tag === 'EffectCleanup') {
    return `effect:${Type.encode(cleanup.type)} site=${Hir.executableSiteLabel(cleanup.site)} slots=${cleanup.slots.map((slot) => `#${slot.ordinal}(${cleanupText(slot.cleanup)})`).join(',') || 'none'}`
  }
  if (cleanup._tag === 'EffectCompositeCleanup') {
    return `effect-composite:${Type.encode(cleanup.type)} alternatives=${cleanup.alternatives.map((alternative, ordinal) => `${ordinal}(${cleanupText(alternative)})`).join(',')}`
  }
  if (cleanup._tag === 'RepresentedCallableCleanup') {
    return `represented-callable:${Type.encode(cleanup.contract)}`
  }
  if (cleanup._tag === 'RepresentedEffectCleanup') {
    return `represented-effect:${Type.encode(cleanup.contract)}`
  }
  if (cleanup._tag === 'HookCleanup') {
    return `hook:${Type.encode(cleanup.type)} target=${cleanup.hook.module}.${cleanup.hook.name}${
      cleanup.typeArguments.length === 0
        ? ''
        : `<${cleanup.typeArguments.map(Type.encodeGenericArgument).join(',')}>`
    } inner=(${cleanupText(cleanup.inner)})`
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
            : binding.category._tag === 'Unavailable'
              ? 'unavailable'
              : `move-only ${Type.encode(binding.category.type)}`
        return `  binding ${siteText(binding.site)} ${binding.name ?? '?'} ${category} affinity=${ExecutionAffinity.encode(binding.executionAffinity)} obligations=${LocalSharedOwnership.encode(binding.localSharedObligations)} live ${spanText(binding.liveFrom)}..${spanText(binding.liveTo)}${binding.movedAt === undefined ? '' : ` moved ${spanText(binding.movedAt)}`}`
      }),
      ...fn.loans.map(
        (loan) =>
          `  loan l${loan.id.ordinal} ${loan.access.toLowerCase()} ${siteText(loan.root)} ${loan.origin === 'SliceReborrow' ? `reborrow parent=${loan.parent === undefined ? '?' : siteText(loan.parent)} suspended=${loan.suspendsParent}` : loan.origin === 'ReturnedView' ? 'returned-view' : 'array'} region=${loan.startRegion.ordinal}->${loan.endRegion.ordinal} ${spanText(loan.startSpan)}..${spanText(loan.endSpan)}`,
      ),
      ...fn.borrowedReplacements.map(
        (replacement) =>
          `  borrowed-replace p${replacement.root.ordinal} region=${replacement.region.ordinal} type=${Type.encode(replacement.type)} cleanup=${cleanupText(replacement.displacedCleanup)} ${spanText(replacement.span)}`,
      ),
      ...fn.callables.map(
        (callable) =>
          `  callable ${Hir.executableSiteLabel(callable.site)} ${callable.mode.toLowerCase()} affinity=${ExecutionAffinity.encode(callable.executionAffinity)} obligations=${LocalSharedOwnership.encode(callable.localSharedObligations)} slots=${callable.slots.map((slot) => `#${slot.ordinal}:p${slot.parameterOrdinal}:${slot.access.toLowerCase()}:${slot.type === undefined ? '?' : Type.encode(slot.type)}:${ExecutionAffinity.encode(slot.executionAffinity)}:${LocalSharedOwnership.encode(slot.localSharedObligations)}:${cleanupText(slot.cleanup)}`).join(',') || 'none'} retained=${callable.retainedDependencies.join(',') || 'none'} drop=${callable.dropOrder.map((ordinal) => `#${ordinal}`).join(',') || 'none'}`,
      ),
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
            case 'Propagation':
              return 'propagation'
          }
        })()
        const loanEnds = exit.loanEnds.map((loan) => `l${loan.ordinal}`).join(',') || 'none'
        return exit.releases.length === 0
          ? `  exit ${label} ${spanText(exit.span)} loan-ends ${loanEnds} releases none`
          : [
              `  exit ${label} ${spanText(exit.span)} loan-ends ${loanEnds}`,
              ...exit.releases.map(
                (release) =>
                  `    release ${siteText(release.binding.site)}${release.fields.length === 0 ? '' : ` fields ${release.fields.map((field) => `#${field.ordinal}`).join(',')}`}${release.cleanup._tag === 'ArrayCleanup' || release.cleanup._tag === 'CallableCleanup' ? ` cleanup ${cleanupText(release.cleanup)}` : ''}`,
              ),
            ].join('\n')
      }),
      ...fn.fixedPoints.map(
        (point) =>
          `  loop${point.loop.ordinal} fixed-point ${point.compatible ? 'compatible' : 'incompatible'} iterations=${point.iterations} incoming=${point.incoming.map(siteText).join(',') || 'none'} repeating=${point.repeating.map((state) => `[${state.map(siteText).join(',')}]`).join(',') || 'none'} following=${point.following.map(siteText).join(',') || 'none'}`,
      ),
      ...fn.matches.map((match) =>
        [
          `  match ${match.access.toLowerCase()} ${spanText(match.span)}`,
          ...match.arms.map(
            (arm) =>
              `    arm #${arm.id.ordinal} ${arm.universal ? '_' : arm.member === undefined ? 'unknown' : Type.encode(arm.member)} guard=${arm.provisionalGuard} bindings=${arm.bindings.map(siteText).join(',') || 'none'} cleanup=${arm.cleanup.map((entry) => `${entry.path.map((field) => `#${field.ordinal}`).join('.') || 'payload'}(${cleanupText(entry.cleanup)})`).join(',') || 'none'}`,
          ),
        ].join('\n'),
      ),
    ]),
    '',
  ].join('\n')
