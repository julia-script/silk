'use client'

/**
 * Module, name, ownership, lowering and backend phases as rows.
 *
 * The split from `project-syntax` is by phase rather than by size: these are the phases that can
 * be *absent* — a target that did not resolve leaves no MIR, a program that did not elaborate
 * leaves no backend output. Each projection here states that absence as a row rather than
 * returning nothing, which is what keeps a broken pipeline readable.
 */

import { Mir, Type } from '@silk-effect/compiler'
import type {
  BootstrapEvaluation,
  Backend,
  DeclarationIndex,
  Elaboration,
  Instances,
  Layout,
  ModuleClosure,
  NameResolution,
  Ownership,
} from '@silk-effect/compiler'
import type { RowModel, Span } from './row'

const typeText = (type: Type.Type): string =>
  typeof type === 'string'
    ? type
    : type._tag === 'NominalType'
      ? `${type.module}.${type.name}${
          type.arguments.length === 0 ? '' : `<${type.arguments.map(typeText).join(', ')}>`
        }`
      : type._tag === 'TypeParameter'
        ? type.name
      : type._tag === 'FixedArrayType'
        ? `Array<${typeText(type.element)}, ${type.length}>`
      : type._tag === 'SliceType'
        ? `${type.access === 'Exclusive' ? '&mut ' : '&'}[${typeText(type.element)}]`
        : type._tag === 'EffectType'
          ? `Effect<${typeText(type.success)}${
              type.failures.length === 0
                ? ''
                : ` ! ${type.failures.map(typeText).join(' | ')}`
            }> ${type.access.toLowerCase()}`
          : type._tag === 'CallableType'
            ? `(${type.parameters.map(typeText).join(', ')}) -> ${typeText(type.result)} ${type.mode.toLowerCase()}`
            : type._tag === 'ReferenceType'
              ? `${type.access === 'Exclusive' ? '&mut ' : '&'}${typeText(type.target)}`
              : type._tag === 'FailureProjectionType'
                ? `Row<!${type.parameter.name}>`
                : type.members.map(typeText).join(' | ')

const asSpan = (span: { readonly start: number; readonly end: number }): Span => ({
  start: span.start,
  end: span.end,
})

export const closureRows = (
  closure: ModuleClosure.Closure,
  onPick: (span: Span) => void,
): ReadonlyArray<RowModel> => {
  const rows: Array<RowModel> = []

  for (const module of closure.modules) {
    const isRoot = module.name === closure.rootModule
    const span = asSpan(module.syntax.root.span)
    rows.push({
      key: `mod-${module.name}`,
      dot: isRoot ? 'symbol' : 'node',
      label: module.name,
      detail: `${isRoot ? 'root · ' : ''}${module.syntax.source.bytes.length} B`,
      span,
      head: true,
      ...(isRoot ? { tone: 'symbol' as const } : {}),
      onActivate: () => onPick(span),
    })
    rows.push({
      key: `mod-${module.name}-imports`,
      depth: 1,
      label: 'imports',
      detail:
        module.imports.length === 0
          ? 'none'
          : module.imports
              .map((entry) => entry.canonicalTarget ?? entry.sourceSpelling ?? 'unavailable')
              .join(', '),
    })
  }

  rows.push({
    key: 'closure-summary',
    label: 'closure',
    detail: `${closure.modules.length} module${closure.modules.length === 1 ? '' : 's'} · ${
      closure.cycles.length
    } cycle${closure.cycles.length === 1 ? '' : 's'}${closure.cycles.length === 0 ? ' · acyclic' : ''}`,
    head: true,
    ...(closure.cycles.length === 0 ? {} : { tone: 'warning' as const }),
  })

  for (const [index, cycle] of closure.cycles.entries()) {
    rows.push({
      key: `cycle-${index}`,
      depth: 1,
      dot: 'warning',
      label: 'cycle',
      detail: cycle.join(' → '),
      tone: 'warning',
    })
  }

  return rows
}

const declaredTypeText = (fact: DeclarationIndex.DeclaredTypeFact): string =>
  fact._tag === 'Resolved'
    ? typeText(fact.type)
    : fact._tag === 'Unresolved'
      ? fact.spelling
      : 'unavailable'

const memberSignature = (member: DeclarationIndex.MemberFact): string => {
  const parameters =
    member.typeParameters.length === 0
      ? ''
      : `<${member.typeParameters.map((parameter) => typeText(parameter.type)).join(', ')}>`
  if (member._tag === 'StructDeclaration')
    return `struct${parameters} · ${member.fields.length} field${member.fields.length === 1 ? '' : 's'}`
  if (member._tag === 'ConstantDeclaration')
    return `${member.visibility === 'Public' ? 'pub ' : ''}const · ${declaredTypeText(member.declaredType)}`
  if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration')
    return `${member.visibility === 'Public' ? 'pub ' : ''}${member._tag === 'ServiceDeclaration' ? 'service' : 'interface'}${parameters} · ${member.operations.length} operation${member.operations.length === 1 ? '' : 's'}`
  const values = member.parameters
    .map(
      (parameter) =>
        `${parameter.name._tag === 'Present' ? parameter.name.spelling : '∅'}: ${declaredTypeText(
          parameter.declaredType,
        )}`,
    )
    .join(', ')
  const failures =
    member.functionKind === 'Effect'
      ? ` ! ${member.failureRow.failures.map(typeText).join(' | ') || 'empty'}`
      : ''
  return `${member.visibility === 'Public' ? 'pub ' : ''}${member.functionKind === 'Effect' ? 'effect ' : ''}fn${parameters} · (${values}) -> ${declaredTypeText(
    member.returnType,
  )}${failures}`
}

const memberName = (member: DeclarationIndex.MemberFact): string =>
  member.name._tag === 'Present' ? member.name.spelling : 'unavailable name'

const declaredName = (name: DeclarationIndex.DeclaredName): string =>
  name._tag === 'Present' ? name.spelling : 'unavailable name'

const conformanceLabel = (conformance: DeclarationIndex.ConformanceFact): string => {
  const parameters =
    conformance.typeParameters.length === 0
      ? ''
      : `<${conformance.typeParameters.map((parameter) => declaredName(parameter.name)).join(', ')}>`
  return `impl${parameters} ${declaredTypeText(conformance.capability)} for ${declaredTypeText(conformance.provider)}`
}

export const indexRows = (
  index: DeclarationIndex.Index,
  onPick: (span: Span) => void,
): ReadonlyArray<RowModel> => {
  const rows: Array<RowModel> = []

  for (const module of index.modules) {
    // A name collision is recorded on the declaration itself, as a non-canonical identity —
    // there is no separate conflict list to read.
    const duplicates = module.members.filter(
      (member) => member.canonical._tag === 'Duplicate',
    ).length
    rows.push({
      key: `idx-${module.module}`,
      label: module.module,
      detail: `${module.members.length} declaration${
        module.members.length === 1 ? '' : 's'
      } · ${module.conformances.length} conformance${
        module.conformances.length === 1 ? '' : 's'
      } · ${duplicates} conflict${duplicates === 1 ? '' : 's'}`,
      head: true,
      ...(duplicates === 0 ? {} : { tone: 'warning' as const }),
    })

    for (const member of module.members) {
      const span = asSpan(member.syntax.span)
      const duplicate = member.canonical._tag === 'Duplicate'
      rows.push({
        key: `idx-${module.module}-${member.id.ordinal}`,
        depth: 1,
        ...(duplicate ? { dot: 'warning' as const, tone: 'warning' as const } : {}),
        label: memberName(member),
        detail: `${memberSignature(member)}${duplicate ? ' · duplicate' : ''}`,
        span,
        onActivate: () => onPick(span),
      })
    }

    for (const conformance of module.conformances) {
      const span = asSpan(conformance.syntax.span)
      rows.push({
        key: `idx-${module.module}-conformance-${conformance.ordinal}`,
        depth: 1,
        dot: 'symbol',
        label: conformanceLabel(conformance),
        detail: `${conformance.operations.length} operation${
          conformance.operations.length === 1 ? '' : 's'
        }${conformance.hook === undefined ? '' : ' · drop hook'}`,
        span,
        onActivate: () => onPick(span),
      })
      for (const [ordinal, operation] of conformance.operations.entries()) {
        const operationSpan = asSpan(operation.syntax.span)
        rows.push({
          key: `idx-${module.module}-conformance-${conformance.ordinal}-operation-${ordinal}`,
          depth: 2,
          label: declaredName(operation.name),
          detail:
            operation.target._tag === 'Unavailable'
              ? 'unavailable target'
              : operation.target.spelling,
          span: operationSpan,
          onActivate: () => onPick(operationSpan),
        })
      }
      if (conformance.hook !== undefined) {
        const hookSpan = asSpan(conformance.hook.syntax.span)
        rows.push({
          key: `idx-${module.module}-conformance-${conformance.ordinal}-drop-hook`,
          depth: 2,
          label: declaredName(conformance.hook.name),
          detail: `${conformance.hook.functionKind.toLowerCase()} drop hook · ${declaredTypeText(conformance.hook.parameterType)}`,
          span: hookSpan,
          onActivate: () => onPick(hookSpan),
        })
      }
    }
  }

  return rows
}

const bindingDetail = (binding: NameResolution.Binding): string => {
  switch (binding._tag) {
    case 'LocalDeclaration':
      return `LocalDeclaration · ${binding.declaration.module}.${binding.declaration.name}`
    case 'IntrinsicActor':
      return 'IntrinsicActor · language intrinsic'
    case 'StdlibNamespace':
      return `StdlibNamespace · ${binding.module}`
    case 'ModuleNamespace':
      return `ModuleNamespace · ${binding.module}`
    case 'ImportedMember':
      return `ImportedMember · ${binding.declaration.module}.${binding.declaration.name}`
    case 'Unavailable':
      return 'Unavailable · unresolved'
  }
}

const bindingSpan = (binding: NameResolution.Binding): Span | undefined => {
  if (binding._tag === 'ModuleNamespace' || binding._tag === 'ImportedMember')
    return asSpan(binding.syntax.span)
  if (binding._tag === 'Unavailable') return asSpan(binding.syntax.span)
  return undefined
}

export const resolutionRows = (
  resolution: NameResolution.Resolution,
  onPick: (span: Span) => void,
): ReadonlyArray<RowModel> => {
  const rows: Array<RowModel> = []

  for (const scope of resolution.modules) {
    rows.push({
      key: `res-${scope.module}`,
      label: scope.module,
      detail: `${scope.bindings.length} binding${scope.bindings.length === 1 ? '' : 's'} · ${
        scope.conflicts.length
      } conflict${scope.conflicts.length === 1 ? '' : 's'}`,
      head: true,
      ...(scope.conflicts.length === 0 ? {} : { tone: 'warning' as const }),
    })

    for (const [ordinal, binding] of scope.bindings.entries()) {
      const span = bindingSpan(binding)
      rows.push({
        key: `res-${scope.module}-${ordinal}`,
        depth: 1,
        label: binding.spelling,
        detail: bindingDetail(binding),
        ...(span === undefined ? {} : { span, onActivate: () => onPick(span) }),
        ...(binding._tag === 'Unavailable' ? { tone: 'warning' as const, dot: 'warning' as const } : {}),
      })
    }

    for (const [ordinal, outcome] of scope.imports.entries()) {
      const unavailable = outcome._tag === 'Unavailable'
      const span = asSpan(outcome.import.syntax.span)
      rows.push({
        key: `res-${scope.module}-import-${ordinal}`,
        depth: 1,
        dot: unavailable ? 'warning' : undefined,
        label: `import ${outcome.import.canonicalTarget ?? outcome.import.sourceSpelling ?? '∅'}`,
        detail: unavailable
          ? 'unavailable'
          : `${outcome.bindings.length} binding${outcome.bindings.length === 1 ? '' : 's'}`,
        span,
        ...(unavailable ? { tone: 'warning' as const } : {}),
        onActivate: () => onPick(span),
      })
    }
  }

  return rows
}

const bindingSiteText = (fact: Ownership.BindingFact): string =>
  fact.site._tag === 'Parameter'
    ? `parameter #${fact.site.parameter.ordinal}`
    : `let b${fact.site.binding.ordinal}`

const loanSiteText = (site: Ownership.BindingSite): string =>
  site._tag === 'Parameter'
    ? `parameter #${site.parameter.ordinal}`
    : site._tag === 'Let'
      ? `let b${site.binding.ordinal}`
      : `pattern b${site.binding.ordinal}`

const cleanupText = (cleanup: Ownership.CleanupPlan): string => {
  switch (cleanup._tag) {
    case 'NoCleanup':
      return 'no cleanup'
    case 'ParameterCleanup':
      return `${typeText(cleanup.type)} · symbolic cleanup`
    case 'AllocationCleanup':
      return `${typeText(cleanup.type)} · active reclaim ticket`
    case 'RawBufferCleanup':
      return `${typeText(cleanup.type)} · ${cleanupText(cleanup.allocation)}`
    case 'HookCleanup':
      return `${typeText(cleanup.type)} · drop hook ${cleanup.hook.module}.${cleanup.hook.name} · ${cleanupText(cleanup.inner)}`
    case 'StructCleanup':
      return `${typeText(cleanup.type)} ${cleanup.fields
        .map(({ field }) => `#${field.ordinal}`)
        .join(' → ')}`
    case 'ArrayCleanup':
      return `${typeText(cleanup.type)} elements in reverse order · ${cleanupText(cleanup.element)}`
    case 'UnionCleanup':
      return `${typeText(cleanup.type)} active case · ${cleanup.cases
        .map((member) => `${member.ordinal}:${typeText(member.member)}`)
        .join(', ')}`
    case 'CallableCleanup':
      return `${typeText(cleanup.type)} captures ${cleanup.slots
        .map(({ ordinal, cleanup: slot }) => `#${ordinal}:${cleanupText(slot)}`)
        .join(' → ')}`
    case 'EffectCleanup':
      return `${typeText(cleanup.type)} captures ${cleanup.slots
        .map(({ ordinal, cleanup: slot }) => `#${ordinal}:${cleanupText(slot)}`)
        .join(' → ')}`
  }
}

export const ownershipRows = (
  facts: Ownership.ModuleOwnership,
  onPick: (span: Span) => void,
): ReadonlyArray<RowModel> => {
  const rows: Array<RowModel> = []

  for (const fn of facts.functions) {
    const span = asSpan(fn.declaration.syntax.span)
    const moves = fn.bindings.filter((binding) => binding.movedAt !== undefined).length
    const cleanups = fn.exits.reduce((total, exit) => total + exit.releases.length, 0)
    rows.push({
      key: `own-${fn.declaration.id.ordinal}`,
      label: memberName(fn.declaration),
      detail: `${fn.bindings.length} binding${fn.bindings.length === 1 ? '' : 's'} · ${moves} move${
        moves === 1 ? '' : 's'
      } · ${cleanups} cleanup${cleanups === 1 ? '' : 's'} · ${fn.verdict._tag}`,
      span,
      head: true,
      ...(fn.verdict._tag === 'Satisfied' ? {} : { tone: 'warning' as const }),
      onActivate: () => onPick(span),
    })

    for (const [ordinal, binding] of fn.bindings.entries()) {
      const bindSpan = asSpan(binding.liveFrom)
      rows.push({
        key: `own-${fn.declaration.id.ordinal}-b${ordinal}`,
        depth: 1,
        label: binding.name ?? '∅',
        detail: `${bindingSiteText(binding)} · ${binding.mutability.toLowerCase()} · ${binding.category._tag.toLowerCase()} · live [${binding.liveFrom.start}, ${binding.liveTo.end})${binding.movedAt === undefined ? '' : ' · moved'}`,
        span: bindSpan,
        onActivate: () => onPick(bindSpan),
      })
    }

    for (const [ordinal, exit] of fn.exits.entries()) {
      const exitSpan = asSpan(exit.span)
      rows.push({
        key: `own-${fn.declaration.id.ordinal}-exit${ordinal}`,
        depth: 1,
        label: `exit ${exit.kind.toLowerCase()}`,
        detail: `${
          exit.loanEnds.length === 0
            ? 'no loan endings'
            : `end ${exit.loanEnds.map((loan) => `#${loan.ordinal}`).join(', ')}`
        } · ${
          exit.releases.length === 0
            ? 'no releases'
            : exit.releases
                .map(
                  (release) =>
                    `${release.binding.name ?? '∅'} (${cleanupText(release.cleanup)})`,
                )
                .join(', ')
        }`,
        span: exitSpan,
        onActivate: () => onPick(exitSpan),
      })
    }

    for (const loan of fn.loans) {
      const loanSpan = asSpan(loan.startSpan)
      rows.push({
        key: `own-${fn.declaration.id.ordinal}-loan-${loan.id.callSpan.start}-${loan.id.ordinal}`,
        depth: 1,
        dot: 'symbol',
        tone: 'symbol',
        label: `loan #${loan.id.ordinal} · ${loan.access.toLowerCase()}`,
        detail: `${loanSiteText(loan.root)} · ${loan.origin}${loan.parent === undefined ? '' : ` · parent ${loanSiteText(loan.parent)} suspended`} · r${loan.startRegion.ordinal} → r${loan.endRegion.ordinal}`,
        span: loanSpan,
        onActivate: () => onPick(loanSpan),
      })
    }

    for (const callable of fn.callables) {
      const callableSpan = asSpan(callable.span)
      rows.push({
        key: `own-${fn.declaration.id.ordinal}-callable-${callable.site.span.start}`,
        depth: 1,
        dot: 'symbol',
        tone: 'symbol',
        label: `callable · ${callable.mode.toLowerCase()}`,
        detail: `${callable.slots.length} slot${callable.slots.length === 1 ? '' : 's'} · retained ${callable.retainedDependencies.join(', ') || 'none'} · drop ${callable.dropOrder.map((ordinal) => `#${ordinal}`).join(' → ') || 'none'}`,
        span: callableSpan,
        onActivate: () => onPick(callableSpan),
      })
      for (const slot of callable.slots) {
        rows.push({
          key: `own-${fn.declaration.id.ordinal}-callable-${callable.site.span.start}-slot${slot.ordinal}`,
          depth: 2,
          label: `slot #${slot.ordinal} → parameter #${slot.parameterOrdinal}`,
          detail: `${slot.access.toLowerCase()} · ${slot.type === undefined ? 'unavailable type' : typeText(slot.type)} · ${cleanupText(slot.cleanup)}`,
          span: callableSpan,
          onActivate: () => onPick(callableSpan),
        })
      }
    }

    for (const replacement of fn.borrowedReplacements) {
      const replacementSpan = asSpan(replacement.span)
      rows.push({
        key: `own-${fn.declaration.id.ordinal}-borrowed-replacement-${replacement.span.start}`,
        depth: 1,
        label: `replace through parameter #${replacement.root.ordinal}`,
        detail: `${typeText(replacement.type)} · ${cleanupText(replacement.displacedCleanup)} · r${replacement.region.ordinal}`,
        span: replacementSpan,
        onActivate: () => onPick(replacementSpan),
      })
    }

    for (const point of fn.fixedPoints) {
      const loopSpan = asSpan(point.span)
      rows.push({
        key: `own-${fn.declaration.id.ordinal}-loop${point.loop.ordinal}`,
        depth: 1,
        dot: point.compatible ? 'ok' : 'warning',
        label: `loop${point.loop.ordinal} fixed point`,
        detail: `${point.compatible ? 'compatible' : 'incompatible'} · ${point.iterations} step${point.iterations === 1 ? '' : 's'} · ${point.repeating.length} repeating path${point.repeating.length === 1 ? '' : 's'}`,
        span: loopSpan,
        ...(point.compatible ? {} : { tone: 'warning' as const }),
        onActivate: () => onPick(loopSpan),
      })
    }

    for (const match of fn.matches) {
      const matchSpan = asSpan(match.span)
      rows.push({
        key: `own-${fn.declaration.id.ordinal}-match-${match.span.start}`,
        depth: 1,
        dot: 'symbol',
        label: `match ${match.access.toLowerCase()}`,
        detail: `${match.arms.length} arm${match.arms.length === 1 ? '' : 's'} · arm-local lifetime`,
        span: matchSpan,
        onActivate: () => onPick(matchSpan),
      })
      for (const arm of match.arms) {
        const cleanup = arm.cleanup
          .map((entry) => entry.path.map((field) => `#${field.ordinal}`).join('.') || 'payload')
          .join(', ')
        rows.push({
          key: `own-${fn.declaration.id.ordinal}-match-${match.span.start}-arm${arm.id.ordinal}`,
          depth: 2,
          label: `arm #${arm.id.ordinal} ${arm.universal ? '_' : arm.member === undefined ? 'unknown' : typeText(arm.member)}`,
          detail: `${arm.provisionalGuard ? 'provisional guard' : 'direct selection'} · ${arm.bindings.length} binding${arm.bindings.length === 1 ? '' : 's'} · cleanup ${cleanup || 'none'}`,
          span: matchSpan,
          onActivate: () => onPick(matchSpan),
        })
      }
    }
  }

  return rows
}

export const instanceRows = (
  discovery: Instances.Discovery,
  onPick: (span: Span) => void,
): ReadonlyArray<RowModel> => {
  const rows: Array<RowModel> = [
    {
      key: 'entry',
      label: 'entry',
      detail:
        discovery.entry._tag === 'Resolved'
          ? `${discovery.entry.key.declaration.module}.${discovery.entry.key.declaration.name}`
          : `unavailable · ${discovery.entry.reason}`,
      head: true,
      ...(discovery.entry._tag === 'Resolved' ? {} : { tone: 'warning' as const }),
    },
    {
      key: 'reachable',
      label: 'reachable',
      detail: `${discovery.instances.length} instance${discovery.instances.length === 1 ? '' : 's'}`,
      head: true,
    },
  ]

  for (const [ordinal, instance] of discovery.instances.entries()) {
    const span = asSpan(instance.function.declaration.syntax.span)
    rows.push({
      key: `inst-${ordinal}`,
      depth: 1,
      dot: 'symbol',
      label: `${instance.key.declaration.module}.${instance.key.declaration.name}${
        instance.key.typeArguments.length === 0
          ? ''
          : `<${instance.key.typeArguments.map(Type.encodeGenericArgument).join(', ')}>`
      }`,
      detail: `${instance.substitution.size} substitution${instance.substitution.size === 1 ? '' : 's'} · instantiated once`,
      span,
      onActivate: () => onPick(span),
    })
  }

  for (const [ordinal, callable] of discovery.callables.entries()) {
    const span = asSpan(callable.site.span)
    rows.push({
      key: `callable-instance-${ordinal}`,
      depth: 1,
      dot: 'symbol',
      tone: 'symbol',
      label: `callable ${callable.target._tag === 'DeclarationCallableTarget' ? callable.target.declaration.name : `${callable.target.actor}.${callable.target.operation}`}`,
      detail: `${typeText(callable.type)} · ${callable.mode.toLowerCase()} · ${callable.captures.map((capture) => `#${capture.ordinal}:${capture.access.toLowerCase()} ${typeText(capture.type)}`).join(', ') || 'no captures'}`,
      span,
      onActivate: () => onPick(span),
    })
  }

  for (const [ordinal, violation] of discovery.violations.entries()) {
    const caller = discovery.instances.find((instance) => instance.key === violation.caller)
    const span = asSpan(caller?.function.declaration.syntax.span ?? { start: 0, end: 0 })
    rows.push({
      key: `inst-violation-${ordinal}`,
      depth: 1,
      dot: 'warning',
      tone: 'warning',
      label: 'polymorphic recursion',
      detail: `${violation.target.declaration.name}<${violation.target.typeArguments.map(Type.encodeGenericArgument).join(', ')}> changes an ancestor specialization`,
      span,
      onActivate: () => onPick(span),
    })
  }

  return rows
}

export const layoutRows = (
  catalog: Layout.Catalog | undefined,
  plan: Layout.Plan | undefined,
  unavailable: string | undefined,
): ReadonlyArray<RowModel> => {
  if (plan === undefined && catalog === undefined) {
    return [
      {
        key: 'layout-unavailable',
        dot: 'warning',
        label: 'layout unavailable',
        detail: unavailable ?? 'the target did not resolve',
        tone: 'warning',
      },
    ]
  }

  const rows: Array<RowModel> = []

  if (plan !== undefined) {
    rows.push({
      key: 'plan',
      label: 'reachable runtime layout',
      detail: `${plan.entries.length} entr${plan.entries.length === 1 ? 'y' : 'ies'}`,
      head: true,
    })
    for (const entry of plan.entries) {
      rows.push({
        key: `plan-${typeText(entry.type)}`,
        depth: 1,
        label: typeText(entry.type),
        detail: `${entry.size} bytes · align ${entry.alignment} · ${
          entry.representation._tag === 'Aggregate'
            ? 'aggregate'
            : entry.representation._tag === 'Repeated'
              ? `${entry.representation.length} × ${typeText(entry.representation.element)} · stride ${entry.representation.stride}`
              : entry.representation._tag === 'Slice'
                ? `address i${entry.representation.address.bits} + length i32 · stride ${entry.representation.stride}`
              : entry.representation._tag === 'Union'
                ? `sum · tag i${entry.representation.tag.bits} · payload +${entry.representation.payloadOffset}/${entry.representation.payloadSize}`
              : entry.representation._tag === 'Reference'
                ? `reference · address i${entry.representation.address.bits}`
                : `i${entry.representation.bits}`
        }`,
      })
    }
    for (const [ordinal, environment] of plan.callableEnvironments.entries()) {
      const span = asSpan(environment.callable.site.span)
      const available = environment._tag === 'CallableEnvironment'
      rows.push({
        key: `plan-callable-${ordinal}`,
        depth: 1,
        dot: available ? 'symbol' : 'warning',
        ...(available ? { tone: 'symbol' as const } : { tone: 'warning' as const }),
        label: `callable environment · ${environment.callable.mode.toLowerCase()}`,
        detail: available
          ? `${environment.size} bytes · align ${environment.alignment} · ${environment.fields.map((field) => `#${field.ordinal}@${field.offset} ${field.representation.toLowerCase()}`).join(', ') || 'empty'} · view ${environment.view.pointerBits}-bit`
          : `unavailable · ${environment.reason}`,
        span,
      })
    }
    if (plan.literalVerdicts.length > 0) {
      rows.push({
        key: 'plan-usize-literals',
        depth: 1,
        label: 'usize literal verdicts',
        detail: `${plan.literalVerdicts.length} target-checked`,
        head: true,
      })
      for (const [ordinal, verdict] of plan.literalVerdicts.entries()) {
        const available = verdict._tag === 'AvailableUsizeLiteral'
        rows.push({
          key: `plan-usize-literal-${ordinal}`,
          depth: 2,
          dot: available ? 'symbol' : 'warning',
          ...(available ? {} : { tone: 'warning' as const }),
          label: verdict.value.toString(),
          detail: `${verdict.bits}-bit · ${available ? 'available' : 'out of range'}`,
          span: asSpan(verdict.span),
        })
      }
    }
  }

  if (catalog !== undefined) {
    rows.push({
      key: 'catalog',
      label: 'nominal catalog',
      detail: `${catalog.entries.length} declaration${catalog.entries.length === 1 ? '' : 's'}`,
      head: true,
    })
    for (const entry of catalog.entries) {
      const unavailableEntry = entry._tag === 'UnavailableLayoutEntry'
      rows.push({
        key: `catalog-${typeText(entry.type)}`,
        depth: 1,
        ...(unavailableEntry ? { dot: 'warning' as const, tone: 'warning' as const } : {}),
        label: typeText(entry.type),
        detail: unavailableEntry
          ? 'unavailable'
          : `${entry.size} bytes · align ${entry.alignment} · ${entry.representation._tag.toLowerCase()}`,
      })
    }
  }

  return rows
}

const localText = (local: Mir.LocalId): string => `_${local.ordinal}`

const placeText = (root: Mir.LocalId, selectors: ReadonlyArray<Mir.PlaceSelector>): string =>
  `${localText(root)}${selectors
    .map((selector) =>
      selector._tag === 'FieldSelector'
        ? `.#${selector.field.ordinal}`
        : selector._tag === 'SliceElementSelector'
          ? `[${localText(selector.index)} · ${selector.access.toLowerCase()} slice]`
          : `[${
            selector.index._tag === 'Proven'
              ? selector.index.value
              : localText(selector.index.local)
          }/${selector.length}]`,
    )
    .join('')}`

const operationLabel = (operation: Mir.Operation): string => {
  switch (operation._tag) {
    case 'Literal':
      return `${localText(operation.destination)} = const ${operation.value}`
    case 'StaticView':
      return `${localText(operation.destination)} = static ${operation.data} · ${operation.length} bytes`
    case 'Binary':
      return `${localText(operation.destination)} = ${operation.operator.toLowerCase()} ${localText(
        operation.left,
      )}, ${localText(operation.right)}`
    case 'ConvertInteger':
      return `${localText(operation.destination)} = convert ${localText(operation.source)} → ${operation.type._tag}`
    case 'ConvertScalar':
      return `${localText(operation.destination)} = convert ${localText(operation.source)} → ${operation.type._tag}`
    case 'ReinterpretScalar':
      return `${localText(operation.destination)} = reinterpret ${localText(operation.source)} → ${operation.type._tag}`
    case 'FloatUnary':
      return `${localText(operation.destination)} = ${operation.operation.toLowerCase()} ${localText(operation.source)}`
    case 'FloatTranscendental':
      return `${localText(operation.destination)} = ${operation.operation.toLowerCase()} ${localText(operation.source)}`
    case 'CheckedInteger':
      return `${localText(operation.destination)} = ${operation.operation.toLowerCase()} ${operation.operands.map(localText).join(', ')}`
    case 'ValidateLayout':
      return `${localText(operation.destination)} = layout ${localText(operation.bytes)} bytes · align ${localText(operation.alignment)}`
    case 'RepeatLayout':
      return `${localText(operation.destination)} = repeat ${localText(operation.layout)} × ${localText(operation.count)}`
    case 'Allocate':
      return `${localText(operation.destination)} = allocate ${localText(operation.layout)}`
    case 'Move':
      return `${localText(operation.destination)} = move ${localText(operation.source)}`
    case 'BeginLoan':
      return `${localText(operation.destination)} = ${operation.reborrow ? 'reborrow' : 'borrow'} ${operation.access.toLowerCase()} ${localText(operation.root)} · loan #${operation.borrow.ordinal}`
    case 'EndLoan':
      return `end loan #${operation.borrow.ordinal} ${localText(operation.slice)}`
    case 'SliceLength':
      return `${localText(operation.destination)} = length ${localText(operation.slice)}`
    case 'ConvertUnion':
      return `${localText(operation.destination)} = ${operation.conversion.toLowerCase()} ${localText(operation.source)} → ${typeText(operation.targetType.type)}`
    case 'Call':
      return `${localText(operation.destination)} = call ${operation.target.name}(${operation.arguments
        .map(localText)
        .join(', ')})`
    case 'MakeEffect':
      return `${localText(operation.destination)} = effect ${operation.runner.name} captures ${operation.captures.map((capture) => `${capture.access.toLowerCase()} ${localText(capture.source)}`).join(', ') || 'none'}`
    case 'MakeCallable':
      return `${localText(operation.destination)} = callable ${operation.target._tag === 'DeclarationCallableTarget' ? operation.target.declaration.name : `${operation.target.actor}.${operation.target.operation}`} captures ${operation.captures.map((capture) => `${capture.access.toLowerCase()} ${localText(capture.source)}`).join(', ') || 'none'}`
    case 'ApplyCallable':
      return `${localText(operation.destination)} = apply ${operation.callable === undefined ? 'erased section' : localText(operation.callable)}(${operation.arguments.map(localText).join(', ')}) · ${operation.access.toLowerCase()}`
    case 'PackEffectOutcome':
      return `${localText(operation.destination)} = effect outcome tag ${operation.tag} payload ${localText(operation.source)}`
    case 'PackEffectFailureUnion':
      return `${localText(operation.destination)} = effect failure union ${localText(operation.source)} · ${operation.mappings.map((mapping) => `${mapping.source}→${mapping.target}`).join(', ')}`
    case 'UnpackEffectSuccess':
      return `${localText(operation.destination)} = effect success ${localText(operation.source)}`
    case 'RunEffect':
      return `${localText(operation.destination)} = run ${operation.target.name} · propagate ${operation.tagMappings.map((mapping) => `${mapping.source}→${mapping.target}`).join(', ') || 'none'}`
    case 'RunEffectValue':
      return `${localText(operation.destination)} = run ${localText(operation.effect)} with ${operation.runner.name}`
    case 'RunStaticEffect':
      return `${localText(operation.destination)} = run static ${operation.runner.name} with ${operation.captures.map((capture) => localText(capture.source)).join(', ') || 'no captures'}`
    case 'ReifyEffect':
      return `${localText(operation.destination)} = result ${localText(operation.effect)} with ${operation.runner.name}`
    case 'CloseEffectEntry':
      return `${localText(operation.destination)} = close ${operation.target.name} with ${operation.runner.name}`
    case 'Construct':
      return `${localText(operation.destination)} = construct ${typeText(operation.type.type)} { ${operation.fields
        .map(({ field, value }) => `#${field.ordinal}: ${localText(value)}`)
        .join(', ')} }`
    case 'ConstructArray':
      return `${localText(operation.destination)} = array [${operation.elements
        .map(localText)
        .join(', ')}]`
    case 'Project':
      return `${localText(operation.destination)} = project ${localText(operation.source)}.#${operation.field.ordinal}`
    case 'ReadPlace':
      return `${localText(operation.destination)} = read ${placeText(operation.root, operation.selectors)}`
    case 'CheckPlace':
      return `check ${placeText(operation.root, operation.selectors)}`
    case 'WritePlace':
      return `write ${placeText(operation.root, operation.selectors)} = ${localText(operation.source)}`
    case 'Drop':
      return `drop ${localText(operation.local)}`
    case 'Match':
      return `${localText(operation.destination)} = match ${operation.access.toLowerCase()} ${localText(operation.scrutinee)}`
    case 'ValidateLayout':
      return `${localText(operation.destination)} = validate layout ${localText(operation.bytes)} bytes, align ${localText(operation.alignment)}`
    case 'RepeatLayout':
      return `${localText(operation.destination)} = repeat layout ${localText(operation.layout)} × ${localText(operation.count)}`
    case 'Allocate':
      return `${localText(operation.destination)} = allocate ${localText(operation.layout)} ! ${operation.failure.name}`
    case 'HostWrite':
      return `${localText(operation.destination)} = write all ${localText(operation.bytes)} to stream ${localText(operation.stream)} ! ${operation.failure.name}`
    case 'OsCall':
      return `${localText(operation.destination)} = ${operation.operation}(${operation.arguments.map(localText).join(', ')})`
    case 'RawBufferFrom':
      return `${localText(operation.destination)} = raw buffer from ${localText(operation.allocation)} × ${localText(operation.count)} · stride ${operation.stride}`
    case 'RawBufferCount':
      return `${localText(operation.destination)} = count ${localText(operation.buffer)}`
    case 'RawBufferSlot':
      return `${localText(operation.destination)} = slot ${localText(operation.buffer)}[${localText(operation.index)}]`
    case 'RawBufferRead':
      return `${localText(operation.destination)} = read ${localText(operation.buffer)}[${localText(operation.index)}]`
    case 'RawBufferView':
      return `${localText(operation.destination)} = ${operation.access.toLowerCase()} view ${localText(operation.buffer)}[${localText(operation.offset)}..+${localText(operation.length)}]`
    case 'SlotWrite':
      return `${localText(operation.destination)} = write ${localText(operation.slot)} = ${localText(operation.value)}`
    case 'SlotTake':
      return `${localText(operation.destination)} = take ${localText(operation.slot)}`
    case 'SlotCopy':
      return `${localText(operation.destination)} = copy ${localText(operation.slot)}`
    case 'SlotDrop':
      return `${localText(operation.destination)} = drop in place ${localText(operation.slot)} · ${cleanupText(operation.cleanup)}`
  }
}

const outcomeLabel = (outcome: Mir.Outcome): string => {
  switch (outcome._tag) {
    case 'Return':
      return `return ${localText(outcome.value)}`
    case 'Forward':
      return `forward r${outcome.target.ordinal}`
    case 'Trap':
      return `trap "${outcome.reason}"`
    case 'Repeat':
      return `repeat loop${outcome.loop.ordinal}`
    case 'Exit':
      return `exit loop${outcome.loop.ordinal}`
    case 'Yield':
      return 'yield condition'
  }
}

const regionOperations = (region: Mir.Region): ReadonlyArray<Mir.Operation> =>
  region._tag === 'OperationRegion'
    ? region.operations
    : region._tag === 'CleanupRegion'
      ? region.releases
      : []

const regionDetail = (region: Mir.Region): string => {
  const owner = region.ownerLoop === undefined ? '' : ` · owner loop${region.ownerLoop.ordinal}`
  switch (region._tag) {
    case 'OperationRegion':
      return `operations${owner}`
    case 'CleanupRegion':
      return `cleanup${owner}`
    case 'ConditionalRegion':
      return `if ${localText(region.condition)} · r${region.taken.ordinal} / r${region.otherwise.ordinal}${owner}`
    case 'LoopRegion':
      return `loop${region.loop.ordinal} · condition r${region.condition.ordinal} · body r${region.body.ordinal} · following r${region.following.ordinal}${owner}`
  }
}

export const mirRows = (
  module: Mir.Module,
  onPick: (span: Span) => void,
): ReadonlyArray<RowModel> => {
  const rows: Array<RowModel> = []

  for (const fn of module.functions) {
    // The module and concrete instance are both identity: different modules may declare the same
    // function name, while one declaration may lower more than one specialization or contract row.
    const instance =
      fn.instance.typeArguments.length === 0
        ? ''
        : `<${fn.instance.typeArguments.map(Type.encodeGenericArgument).join(', ')}>`
    const fnKey = `mir-${JSON.stringify([
      fn.instance.declaration.module,
      fn.instance.declaration.name,
      fn.instance.typeArguments.map(Type.genericArgumentKey),
      fn.instance.contractRow,
    ])}`
    const operationCount = Mir.operations(fn).length
    rows.push({
      key: fnKey,
      dot: 'symbol',
      label: `${fn.id.name}${instance}`,
      detail: `fn · entry r${fn.entry.ordinal} · ${fn.regions.length} region${fn.regions.length === 1 ? '' : 's'} · ${operationCount} op${operationCount === 1 ? '' : 's'}`,
      head: true,
      tone: 'symbol',
    })

    for (const region of Mir.topologicalRegions(fn)) {
      rows.push({
        key: `${fnKey}-r${region.id.ordinal}`,
        depth: 1,
        label: `r${region.id.ordinal} · ${region._tag.replace('Region', '').toLowerCase()}`,
        detail: regionDetail(region),
      })

      for (const [ordinal, operation] of regionOperations(region).entries()) {
        const span = asSpan(operation.provenance.span)
        rows.push({
          key: `${fnKey}-r${region.id.ordinal}-${ordinal}`,
          depth: 2,
          label: operationLabel(operation),
          detail: operation.provenance.generated ? 'generated' : operation._tag.toLowerCase(),
          span,
          onActivate: () => onPick(span),
        })
        if (operation._tag === 'Match') {
          for (const decision of operation.decisions) {
            rows.push({
              key: `${fnKey}-r${region.id.ordinal}-${ordinal}-decision-${typeText(decision.member)}`,
              depth: 3,
              label: `decision ${typeText(decision.member)}`,
              detail: decision.candidates.map((candidate) => `arm #${candidate.ordinal}`).join(' → '),
            })
          }
          for (const arm of operation.arms) {
            const armSpan = asSpan(arm.provenance.span)
            rows.push({
              key: `${fnKey}-r${region.id.ordinal}-${ordinal}-arm${arm.id.ordinal}`,
              depth: 3,
              label: `arm #${arm.id.ordinal} ${arm.universal ? '_' : arm.member === undefined ? 'unknown' : typeText(arm.member)}`,
              detail: `${arm.guard === undefined ? 'selected' : `guard ${localText(arm.guard.result)}`} · result ${localText(arm.selected.result)} → ${localText(operation.destination)} · cleanup ${arm.selected.cleanup.length}`,
              span: armSpan,
              onActivate: () => onPick(armSpan),
            })
            for (const binding of arm.bindings) {
              const bindingSpan = asSpan(binding.provenance.span)
              rows.push({
                key: `${fnKey}-r${region.id.ordinal}-${ordinal}-arm${arm.id.ordinal}-binding${binding.id.ordinal}`,
                depth: 4,
                label: `${localText(binding.destination)} = payload ${binding.path.map((field) => `#${field.ordinal}`).join('.')}`,
                detail: `${binding.access.toLowerCase()} · ${typeText(Mir.semanticType(binding.type))}`,
                span: bindingSpan,
                onActivate: () => onPick(bindingSpan),
              })
            }
          }
        }
      }

      if (region._tag === 'OperationRegion' || region._tag === 'CleanupRegion') {
        const span = asSpan(region.outcome.provenance.span)
        rows.push({
          key: `${fnKey}-r${region.id.ordinal}-outcome`,
          depth: 2,
          label: outcomeLabel(region.outcome),
          detail: 'outcome',
          span,
          onActivate: () => onPick(span),
        })
      } else {
        const span = asSpan(region.provenance.span)
        rows.push({
          key: `${fnKey}-r${region.id.ordinal}-control`,
          depth: 2,
          label:
            region._tag === 'ConditionalRegion'
              ? `taken r${region.taken.ordinal} · otherwise r${region.otherwise.ordinal}${region.following === undefined ? '' : ` · following r${region.following.ordinal}`}`
              : `condition r${region.condition.ordinal} · body r${region.body.ordinal} · following r${region.following.ordinal}`,
          detail: 'structural edges',
          span,
          onActivate: () => onPick(span),
        })
      }
    }
  }

  return rows
}

/**
 * Emitted backend text, one row per line.
 *
 * IR is already line-oriented, so it keeps the lead column as a line number and puts the whole
 * line in the label — `white-space: pre` in the row grammar is what preserves its indentation.
 */
export const backendTextRows = (ir: string): ReadonlyArray<RowModel> =>
  ir.split('\n').map((line, index) => ({
    key: `ir-${index}`,
    lead: index + 1,
    label: line,
  }))

/** Backend-local jumps/branches with canonical region and source provenance kept visible. */
export const backendControlRows = (
  control: ReadonlyArray<Backend.ControlProvenance>,
  onPick: (span: Span) => void,
): ReadonlyArray<RowModel> => [
  {
    key: 'backend-control',
    label: 'control conversion',
    detail: `${control.length} emitted construct${control.length === 1 ? '' : 's'}`,
    head: true,
  },
  ...control.map((entry, ordinal) => {
    const span = asSpan(entry.span)
    return {
      key: `backend-control-${entry.function.name}-${entry.region.ordinal}-${ordinal}`,
      depth: 1,
      label: `${entry.construct} · r${entry.region.ordinal}`,
      detail: `${entry.targets.map((target) => `r${target.ordinal}`).join(', ') || 'terminal'}${entry.loop === undefined ? '' : ` · loop${entry.loop.ordinal}`}`,
      span,
      onActivate: () => onPick(span),
    }
  }),
]

export const symbolRows = (
  symbols: ReadonlyArray<{
    readonly symbol: string
    readonly declaration: { readonly module: string; readonly name: string }
  }>,
): ReadonlyArray<RowModel> =>
  symbols.map((entry) => ({
    key: `sym-${entry.symbol}`,
    dot: 'symbol',
    label: entry.symbol,
    detail: `${entry.declaration.module}.${entry.declaration.name}`,
    tone: 'symbol',
  }))

/**
 * A traced value is no longer always a scalar: constructing a struct binds an aggregate. An
 * aggregate renders as its type plus lane values, recursively, so `Pair { 1, 2 }` stays legible
 * in one trace cell.
 */
const valueText = (value: BootstrapEvaluation.Value): string =>
  value._tag === 'I32Value'
    ? String(value.value)
    : value._tag === 'UsizeValue'
      ? `${value.value.toString()}usize`
      : value._tag === 'ScalarIntegerValue'
        ? `${value.value.toString()}${value.type}`
        : value._tag === 'FloatValue'
          ? `${value.type}(bits=0x${value.bits.toString(16)})`
    : value._tag === 'ArrayValue'
      ? `${typeText(value.type)} [${value.elements.map(valueText).join(', ')}]`
      : value._tag === 'SliceValue'
        ? `slice cell f${value.frame}.c${value.cell} [${value.base}..${value.base + value.length})`
        : value._tag === 'StaticViewValue'
          ? `static ${value.data} · ${value.length} bytes`
      : value._tag === 'UnionValue'
        ? `${typeText(value.type)} <${typeText(value.member)} ${valueText(value.payload)}>`
      : value._tag === 'EffectOutcomeValue'
        ? `${typeText(value.type)} tag=${value.tag} payload=${valueText(value.payload)}`
        : value._tag === 'EffectBorrowValue'
          ? `${value.access.toLowerCase()} borrow f${value.frame}.c${value.cell}`
          : value._tag === 'EffectValue'
            ? `${typeText(value.type)} recipe ${value.runner.name}`
            : value._tag === 'CallableBorrowValue'
              ? `${value.access.toLowerCase()} callable borrow f${value.frame}.c${value.cell}`
              : value._tag === 'CallableValue'
                ? `${typeText(value.type)} callable #${value.ticket} · ${value.captures.length} capture${value.captures.length === 1 ? '' : 's'}`
            : value._tag === 'AllocationValue'
              ? `${typeText(value.type)} ticket=${value.ticket} · ${value.bytes.toString()} bytes · align ${value.alignment.toString()}`
            : value._tag === 'RawBufferValue'
              ? `${typeText(value.type)} ticket=${value.ticket} · ${value.count.toString()} × ${typeText(value.element)} · stride ${value.stride}`
            : value._tag === 'SlotValue'
              ? `${typeText(value.type)} ticket=${value.ticket}[${value.index.toString()}] · ${typeText(value.element)}`
            : value._tag === 'ReferenceValue'
              ? `borrow f${value.frame}.c${value.cell}`
              : `${typeText(value.type)} { ${value.fields.map((entry) => valueText(entry.value)).join(', ')} }`

const traceLabel = (event: BootstrapEvaluation.TraceEvent): string => {
  switch (event._tag) {
    case 'Entry':
      return `enter ${event.function.module}.${event.function.name} · frame ${event.frame} · depth ${event.depth}`
    case 'Call':
      return `call ${event.target.module}.${event.target.name} · frame ${event.frame} · depth ${event.depth}`
    case 'Binding':
      return `bind p${event.parameterOrdinal} = ${valueText(event.value)} · frame ${event.frame} · depth ${event.depth}`
    case 'Return':
      return `return ${valueText(event.value)} · frame ${event.frame} · depth ${event.depth}`
    case 'Construct':
      return `construct ${typeText(event.type)} · ${event.fieldCount} field${
        event.fieldCount === 1 ? '' : 's'
      }`
    case 'ArrayConstruct':
      return `construct ${typeText(event.type)} · ${event.elementCount} element${
        event.elementCount === 1 ? '' : 's'
      }`
    case 'UnionConversion':
      return `${event.conversion.toLowerCase()} ${typeText(event.member)} → ${typeText(event.target)}`
    case 'Project':
      return `project ${typeText(event.type)}.#${event.field.ordinal}`
    case 'PlaceRead':
      return `read ${event.selectors
        .map((selector) =>
          selector._tag === 'Field'
            ? `#${selector.field.ordinal}`
            : selector._tag === 'StaticElement'
              ? `${selector.data}[${selector.index}] ${selector.bounds.toLowerCase()}`
              : selector._tag === 'RawBufferElement'
                ? `allocation #${selector.ticket}[${selector.index}] ${selector.bounds.toLowerCase()}`
                : `${typeText(selector.array)}[${selector.index}] ${selector.bounds.toLowerCase()}`,
        )
        .join(' → ')} = ${valueText(event.value)}`
    case 'Cleanup':
      return `cleanup _${event.local}${event.members === undefined ? '' : ` · active ${event.members.map(typeText).join(', ')}`} · frame ${event.frame} · depth ${event.depth}`
    case 'MatchDispatch':
      return `match ${event.access.toLowerCase()} · active ${typeText(event.member)}`
    case 'MatchCandidate':
      return event.binding === undefined
        ? `candidate arm #${event.arm ?? '?'}`
        : `bind pattern #${event.binding} = ${event.value === undefined ? '?' : valueText(event.value)}`
    case 'MatchSelected':
      return `select arm #${event.arm ?? '?'}`
    case 'MatchCleanup':
      return `cleanup arm #${event.arm ?? '?'} · ${event.path?.map((field) => `#${field.ordinal}`).join('.') || 'payload'}`
    case 'MatchBorrowEnd':
      return `end ${event.access.toLowerCase()} arm view`
    case 'RegionEntry':
      return `enter r${event.region}`
    case 'Condition':
      return `condition r${event.region}${event.loop === undefined ? '' : ` · loop${event.loop}`}`
    case 'Iteration':
      return `iterate loop${event.loop ?? '?'} · body r${event.region}`
    case 'WriteCheck':
      return `check write in r${event.region}`
    case 'ReplacementCleanup':
      return `cleanup replaced owner in r${event.region}${event.members === undefined ? '' : ` · active ${event.members.map(typeText).join(', ')}`}`
    case 'Replacement':
      return `commit replacement in r${event.region}`
    case 'Repeat':
      return `repeat loop${event.loop ?? '?'}`
    case 'Exit':
      return `exit loop${event.loop ?? '?'}`
    case 'Transfer':
      return `transfer in r${event.region}${event.loop === undefined ? '' : ` · loop${event.loop}`}`
    case 'EffectSuccess':
      return `effect success · tag ${event.tag}`
    case 'EffectFailure':
      return `effect failure · tag ${event.tag}`
    case 'CallableConstruct':
      return `construct callable #${event.ticket} · ${event.mode.toLowerCase()}`
    case 'CallableApply':
      return `apply callable #${event.ticket} · ${event.mode.toLowerCase()}`
    case 'CallableCleanup':
      return `cleanup callable #${event.ticket}`
    case 'CallableRejected':
      return `reject callable #${event.ticket} · ${event.mode.toLowerCase()}`
    case 'AllocationAcquire':
      return `acquire allocation #${event.ticket}`
    case 'RawBufferForm':
      return `form raw buffer #${event.ticket} × ${event.count?.toString() ?? '?'}`
    case 'SlotProject':
      return `project slot #${event.ticket}[${event.index?.toString() ?? '?'}]`
    case 'SlotWrite':
      return `write slot #${event.ticket}[${event.index?.toString() ?? '?'}]`
    case 'SlotTake':
      return `take slot #${event.ticket}[${event.index?.toString() ?? '?'}]`
    case 'SlotCopy':
      return `copy slot #${event.ticket}[${event.index?.toString() ?? '?'}]`
    case 'RawBufferRead':
      return `read raw buffer #${event.ticket}[${event.index?.toString() ?? '?'}]`
    case 'SlotDrop':
      return `drop slot #${event.ticket}[${event.index?.toString() ?? '?'}]`
    case 'AllocationRelease':
      return `release allocation #${event.ticket}`
    case 'HostWrite':
      return `${event.destination.toLowerCase()} write ${event.bytes.length} bytes · ${event.outcome.toLowerCase()}`
  }
}

const traceDepth = (event: BootstrapEvaluation.TraceEvent): number => {
  switch (event._tag) {
    case 'Entry':
      return event.depth - 1
    case 'Call':
      return event.depth
    case 'Binding':
      return event.depth
    case 'Return':
      return event.depth - 1
    case 'Cleanup':
      return event.depth
    case 'Construct':
    case 'ArrayConstruct':
    case 'UnionConversion':
    case 'Project':
    case 'PlaceRead':
    case 'MatchDispatch':
    case 'MatchCandidate':
    case 'MatchSelected':
    case 'MatchCleanup':
    case 'MatchBorrowEnd':
    case 'RegionEntry':
    case 'Condition':
    case 'Iteration':
    case 'WriteCheck':
    case 'ReplacementCleanup':
    case 'Replacement':
    case 'Repeat':
    case 'Exit':
    case 'Transfer':
    case 'EffectSuccess':
    case 'EffectFailure':
    case 'CallableConstruct':
    case 'CallableApply':
    case 'CallableCleanup':
    case 'CallableRejected':
    case 'AllocationAcquire':
    case 'RawBufferForm':
    case 'SlotProject':
    case 'SlotWrite':
    case 'SlotTake':
    case 'SlotCopy':
    case 'RawBufferRead':
    case 'SlotDrop':
    case 'AllocationRelease':
    case 'HostWrite':
      return 2
  }
}

const blockedReasonText = (reason: BootstrapEvaluation.BlockedReason): string => {
  switch (reason._tag) {
    case 'InvalidMir':
      return `invalid MIR · ${reason.violations.length} violation${
        reason.violations.length === 1 ? '' : 's'
      }`
    case 'UnavailableEntry':
      return `unavailable entry · ${reason.reason}`
    case 'Trap':
      return `trap · ${reason.reason}`
    case 'MissingFunction':
      return `missing function · ${reason.target.module}.${reason.target.name}`
    case 'EvaluationLimit':
      return `${reason.kind === 'Steps' ? 'step' : 'call-depth'} limit · ${reason.count}/${reason.limit} · stopped in ${reason.function.module}.${reason.function.name} · active ${reason.activeFrames.map((frame) => `f${frame.frame}:d${frame.depth} ${frame.function.name}`).join(' → ')}`
    case 'InvalidCallableReuse':
      return `invalid callable reuse · #${reason.ticket} is ${reason.state.toLowerCase()}`
    case 'MissingStandardStreams':
      return 'missing StandardStreams host provider'
    case 'MissingOsFileSystemHost':
      return 'missing OS filesystem host provider'
    case 'IntrinsicTargetUnavailable':
      return reason.diagnostics.map((diagnostic) => diagnostic.message).join(' · ')
  }
}

export const evaluationRows = (
  outcome: BootstrapEvaluation.Outcome | undefined,
  onPick: (span: Span) => void,
): ReadonlyArray<RowModel> => {
  if (outcome === undefined) {
    return [
      {
        key: 'not-run',
        label: 'not evaluated',
        detail: 'evaluation is an explicit action — run it to see the trace',
      },
    ]
  }

  const rows: Array<RowModel> = outcome.trace.map((event, index) => {
    const span = asSpan(event.span)
    return {
      key: `trace-${index}`,
      lead: index + 1,
      depth: traceDepth(event),
      label: traceLabel(event),
      detail: event._tag.toLowerCase(),
      span,
      onActivate: () => onPick(span),
    }
  })

  rows.push(
    outcome._tag === 'Completed'
      ? {
          key: 'outcome',
          dot: 'ok',
          label: 'Completed',
          detail: `${outcome.entry.module}.${outcome.entry.name}() → ${outcome.result.value}`,
          head: true,
          tone: 'ok',
        }
      : outcome._tag === 'UnhandledFailure'
        ? {
            key: 'outcome',
            dot: 'warning',
            label: 'Unhandled failure',
            detail: `${outcome.report} · tag ${outcome.tag}`,
            head: true,
            tone: 'warning',
          }
        : {
            key: 'outcome',
            dot: 'warning',
            label: 'Blocked',
            detail: blockedReasonText(outcome.reason),
            head: true,
            tone: 'warning',
          },
  )

  return rows
}

/**
 * Canonical struct-value facts: literals, projections, calling shapes, and — after a run —
 * the aggregate evaluation events.
 *
 * The literal's two orders are the point: the source wrote `{ right: 2, left: 1 }` but the
 * canonical struct order is `left, right`, and the compiler owns that reordering. Showing both
 * on one row pair is what makes the reordering inspectable rather than folklore.
 */
export const structValueRows = (
  literals: ReadonlyArray<Elaboration.StructLiteralExpressionFact>,
  projections: ReadonlyArray<Elaboration.FieldProjectionExpressionFact>,
  shapes: ReadonlyArray<Layout.CallingShape>,
  evaluation: BootstrapEvaluation.Outcome | undefined,
  onPick: (span: Span) => void,
): ReadonlyArray<RowModel> => {
  const rows: Array<RowModel> = []

  rows.push({
    key: 'literals',
    label: 'struct construction',
    detail: literals.length === 0 ? 'no struct literals' : `${literals.length}`,
    head: true,
  })
  for (const literal of literals) {
    const span = asSpan(literal.syntax.span)
    const key = `lit-${span.start}-${span.end}`
    rows.push({
      key,
      depth: 1,
      dot: literal.target._tag === 'Resolved' ? 'symbol' : 'warning',
      label: literal.target._tag === 'Resolved' ? typeText(literal.target.type) : 'unavailable target',
      detail: literal.authorized ? 'module-owned' : 'not authorized',
      span,
      ...(literal.target._tag === 'Resolved' && literal.authorized
        ? {}
        : { tone: 'warning' as const }),
      onActivate: () => onPick(span),
    })
    rows.push({
      key: `${key}-source`,
      depth: 2,
      label: 'source order',
      detail:
        literal.initializers.map((initializer) => initializer.name ?? '?').join(', ') || 'empty',
    })
    rows.push({
      key: `${key}-canonical`,
      depth: 2,
      label: 'canonical order',
      detail:
        literal.fields
          .map(({ field }) => (field.name._tag === 'Present' ? field.name.spelling : '?'))
          .join(', ') || 'empty',
    })
  }

  rows.push({
    key: 'projections',
    label: 'field projection chain',
    detail: projections.length === 0 ? 'none' : `${projections.length}`,
    head: true,
  })
  for (const projection of projections) {
    const span = asSpan(projection.syntax.span)
    const unresolved = projection.state._tag !== 'Resolved'
    rows.push({
      key: `proj-${span.start}-${span.end}`,
      depth: 1,
      ...(unresolved ? { dot: 'warning' as const, tone: 'warning' as const } : {}),
      label: `${projection.nominal === undefined ? '?' : typeText(projection.nominal)}.${
        projection.fieldName ?? '?'
      }`,
      detail: projection.state._tag.toLowerCase(),
      span,
      onActivate: () => onPick(span),
    })
  }

  rows.push({
    key: 'shapes',
    label: 'compiler-owned calling shapes',
    detail: shapes.length === 0 ? 'none reachable' : `${shapes.length}`,
    head: true,
  })
  for (const shape of shapes) {
    rows.push({
      key: `shape-${typeText(shape.type)}`,
      depth: 1,
      label: typeText(shape.type),
      detail:
        shape.lanes
          .map((lane) => `${lane.type}:${selectorPathText(lane.path)}`)
          .join(', ') || 'zero runtime lanes',
    })
  }

  const aggregateEvents =
    evaluation?.trace.filter(
      (event) =>
        event._tag === 'Construct' || event._tag === 'Project' || event._tag === 'Cleanup',
    ) ?? []
  rows.push({
    key: 'events',
    label: 'evaluation events',
    detail:
      evaluation === undefined
        ? 'run evaluation to inspect construction, projection, and cleanup'
        : `${aggregateEvents.length}`,
    head: true,
  })
  for (const [index, event] of aggregateEvents.entries()) {
    const span = asSpan(event.span)
    rows.push({
      key: `event-${index}`,
      depth: 1,
      dot: 'ok',
      label: event._tag.toLowerCase(),
      detail:
        event._tag === 'Construct'
          ? `${typeText(event.type)} · ${event.fieldCount} field${event.fieldCount === 1 ? '' : 's'}`
          : event._tag === 'Project'
            ? `${typeText(event.type)}.#${event.field.ordinal}`
            : `local _${event.local}`,
      span,
      onActivate: () => onPick(span),
    })
  }

  return rows
}

const selectorPathText = (path: ReadonlyArray<Layout.Selector>): string =>
  path
    .map((selector) =>
      selector._tag === 'ElementSelector'
        ? `[${selector.index}]`
        : selector._tag === 'FieldId'
          ? `#${selector.ordinal}`
          : selector._tag === 'UnionTagSelector'
            ? 'tag'
            : selector._tag === 'UnionPayloadSelector'
              ? `payload[${selector.slot}]`
              : selector._tag === 'SliceAddressSelector'
                ? 'address'
                : 'length',
    )
    .join('.')

/** Canonical array facts from syntax through evaluation and backend-neutral ABI paths. */
export const arrayValueRows = (
  types: ReadonlyArray<Type.FixedArray>,
  literals: ReadonlyArray<Elaboration.ArrayLiteralExpressionFact>,
  projections: ReadonlyArray<Elaboration.IndexProjectionExpressionFact>,
  layouts: ReadonlyArray<Layout.Entry>,
  shapes: ReadonlyArray<Layout.CallingShape>,
  evaluation: BootstrapEvaluation.Outcome | undefined,
  onPick: (span: Span) => void,
): ReadonlyArray<RowModel> => {
  const rows: Array<RowModel> = [
    {
      key: 'array-types',
      label: 'canonical array types',
      detail: types.length === 0 ? 'none' : `${types.length}`,
      head: true,
    },
    ...types.map(
      (type, ordinal): RowModel => ({
        key: `array-type-${ordinal}`,
        depth: 1,
        dot: 'symbol',
        label: typeText(type),
        detail: `length ${type.length} · element ${typeText(type.element)}`,
        tone: 'symbol',
      }),
    ),
    {
      key: 'array-literals',
      label: 'literal elements',
      detail: literals.length === 0 ? 'none' : `${literals.length}`,
      head: true,
    },
  ]

  for (const [literalOrdinal, literal] of literals.entries()) {
    const span = asSpan(literal.syntax.span)
    const key = `array-literal-${literalOrdinal}`
    rows.push({
      key,
      depth: 1,
      dot: literal.state._tag === 'Complete' ? 'ok' : 'warning',
      label:
        literal.state._tag === 'Complete'
          ? typeText(literal.state.type)
          : literal.expected === undefined
            ? 'unavailable array'
            : typeText(literal.expected),
      detail: `${literal.length} element${literal.length === 1 ? '' : 's'} · ${literal.state._tag}`,
      span,
      ...(literal.state._tag === 'Complete' ? {} : { tone: 'warning' as const }),
      onActivate: () => onPick(span),
    })
    for (const element of literal.elements) {
      const elementSpan = asSpan(element.syntax.span)
      rows.push({
        key: `${key}-element-${element.ordinal}`,
        depth: 2,
        label: `[${element.ordinal}]`,
        detail: element.compatibility._tag,
        span: elementSpan,
        ...(element.compatibility._tag === 'Compatible'
          ? {}
          : { dot: 'warning' as const, tone: 'warning' as const }),
        onActivate: () => onPick(elementSpan),
      })
    }
  }

  rows.push({
    key: 'array-indexes',
    label: 'checked place chains',
    detail: projections.length === 0 ? 'none' : `${projections.length}`,
    head: true,
  })
  for (const [ordinal, projection] of projections.entries()) {
    const span = asSpan(projection.syntax.span)
    rows.push({
      key: `array-index-${ordinal}`,
      depth: 1,
      dot: projection.bounds._tag === 'Invalid' ? 'warning' : 'ok',
      label: projection.array === undefined ? '?[index]' : `${typeText(projection.array)}[index]`,
      detail: `${projection.access} · ${
        projection.bounds._tag === 'Proven'
          ? `proven ${projection.bounds.index}/${projection.bounds.length}`
          : projection.bounds._tag === 'Runtime'
            ? `runtime check < ${projection.bounds.length}`
            : projection.bounds._tag === 'Invalid'
              ? `invalid ${projection.bounds.index}/${projection.bounds.length}`
              : 'bounds unavailable'
      }`,
      span,
      ...(projection.bounds._tag === 'Invalid' ? { tone: 'warning' as const } : {}),
      onActivate: () => onPick(span),
    })
  }

  rows.push({
    key: 'array-layouts',
    label: 'repeated layouts + calling paths',
    detail: `${layouts.length} layout${layouts.length === 1 ? '' : 's'} · ${shapes.length} shape${shapes.length === 1 ? '' : 's'}`,
    head: true,
  })
  for (const entry of layouts) {
    if (entry.representation._tag !== 'Repeated') continue
    rows.push({
      key: `array-layout-${typeText(entry.type)}`,
      depth: 1,
      label: typeText(entry.type),
      detail: `${entry.size} B · align ${entry.alignment} · stride ${entry.representation.stride}`,
    })
  }
  for (const shape of shapes) {
    rows.push({
      key: `array-shape-${typeText(shape.type)}`,
      depth: 1,
      label: `${typeText(shape.type)} lanes`,
      detail:
        shape.lanes.map((lane) => `${lane.type}:${selectorPathText(lane.path)}`).join(', ') ||
        'zero runtime lanes',
    })
  }

  const events =
    evaluation?.trace.filter(
      (event) => event._tag === 'ArrayConstruct' || event._tag === 'PlaceRead',
    ) ?? []
  rows.push({
    key: 'array-events',
    label: 'evaluation events',
    detail: evaluation === undefined ? 'run evaluation to inspect values and checks' : `${events.length}`,
    head: true,
  })
  for (const [ordinal, event] of events.entries()) {
    const span = asSpan(event.span)
    rows.push({
      key: `array-event-${ordinal}`,
      depth: 1,
      dot: 'ok',
      label: traceLabel(event),
      detail: event._tag,
      span,
      onActivate: () => onPick(span),
    })
  }

  return rows
}

/**
 * The whole pipeline as one row per phase.
 *
 * Every other view answers "what did this phase produce"; this one answers "how far did the
 * program get". It is the only view that is about the pipeline rather than about a phase, which
 * is why it survived the consolidation of the standalone labs.
 */
export const pipelineRows = (
  phases: ReadonlyArray<{
    readonly phase: string
    readonly outputs: string
    readonly diagnostics: number
  }>,
): ReadonlyArray<RowModel> =>
  phases.map((entry) => ({
    key: `phase-${entry.phase}`,
    dot: entry.diagnostics === 0 ? 'ok' : 'error',
    label: entry.phase,
    detail: `${entry.outputs} · ${
      entry.diagnostics === 0 ? 'no diagnostics' : `${entry.diagnostics} diagnostics`
    }`,
    ...(entry.diagnostics === 0 ? {} : { tone: 'error' as const }),
  }))

/** 16 bytes per row, offset in the lead column — the shape a hex dump has always had. */
export const hexRows = (bytes: Uint8Array): ReadonlyArray<RowModel> => {
  const rows: Array<RowModel> = []
  for (let offset = 0; offset < bytes.length; offset += 16) {
    rows.push({
      key: `hex-${offset}`,
      lead: offset.toString(16).padStart(6, '0'),
      label: Array.from(bytes.slice(offset, offset + 16), (byte) =>
        byte.toString(16).padStart(2, '0'),
      ).join(' '),
    })
  }
  return rows
}

export const toolchainRows = (
  commands: ReadonlyArray<readonly [string, string]>,
): ReadonlyArray<RowModel> => [
  {
    key: 'commands',
    label: 'planned commands',
    detail: String(commands.length),
    head: true,
  },
  ...commands.map(([name, text]) => ({
    key: `cmd-${name}`,
    depth: 1,
    label: name,
    detail: text,
  })),
  {
    key: 'scope',
    label: 'build scope',
    detail: 'owned intermediates',
    head: true,
  },
  {
    key: 'scope-1',
    depth: 1,
    label: '1',
    detail: 'scope opens: a named temporary directory owns every intermediate',
  },
  {
    key: 'scope-2',
    depth: 1,
    label: '2',
    detail: 'program.bc and program.o are written as path-backed scope artifacts',
  },
  {
    key: 'scope-3',
    depth: 1,
    label: '3',
    detail: 'only an explicit save-temps promotion copies an artifact out durably',
  },
  {
    key: 'scope-4',
    depth: 1,
    label: '4',
    detail: 'scope exit removes the directory after success or failure alike',
  },
]
