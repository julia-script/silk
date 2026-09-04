import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
/**
 * Module, name, ownership, lowering and backend phases as rows.
 *
 * The split from `project-syntax` is by phase rather than by size: these are the phases that can
 * be *absent* — a target that did not resolve leaves no MIR, a program that did not elaborate
 * leaves no backend output. Each projection here states that absence as a row rather than
 * returning nothing, which is what keeps a broken pipeline readable.
 */

import type * as Backend from './Backend.js'
import type * as CleanupPlan from './CleanupPlan.js'
import type * as Elaboration from './Elaboration.js'
import type { RowModel, Span } from './InspectorRow.js'
import { spanOf as asSpan } from './InspectorRow.js'
import type * as Instances from './Instances.js'
import * as Intrinsic from './Intrinsic.js'
import type * as Layout from './Layout.js'
import * as Match from './Match.js'
import * as Mir from './Mir.js'
import * as MirVerification from './MirVerification.js'
import type * as ModuleClosure from './ModuleClosure.js'
import type * as NameResolution from './NameResolution.js'
import type * as Ownership from './Ownership.js'
import * as Type from './Type.js'

const typeText = (type: Type.Type): string => {
  if (typeof type === 'string') return type

  switch (type._tag) {
    case 'NominalType': {
      const argumentsText =
        type.arguments.length === 0
          ? ''
          : `<${type.arguments.map(Type.encodeGenericArgument).join(', ')}>`
      return `${type.module}.${type.name}${argumentsText}`
    }
    case 'TypeParameter':
      return type.name
    case 'FixedArrayType':
      return `Array<${typeText(type.element)}, ${type.length}>`
    case 'SliceType':
      return `${type.access === 'Exclusive' ? '&mut ' : '&'}[${typeText(type.element)}]`
    case 'EffectType': {
      const failureType = Type.failureType(type)
      const failureText = failureType === 'never' ? '' : ` ! ${typeText(failureType)}`
      return `Effect<${typeText(type.success)}${failureText}> ${type.access.toLowerCase()}`
    }
    case 'CallableType':
      return `(${type.parameters.map(typeText).join(', ')}) -> ${typeText(type.result)} ${type.mode.toLowerCase()}`
    case 'ForeignFunctionType':
      return Type.encode(type)
    case 'ReferenceType':
      return `${type.access === 'Exclusive' ? '&mut ' : '&'}${typeText(type.target)}`
    case 'PointerType':
      return `${type.mutable ? '*mut ' : '*const '}${typeText(type.pointee)}`
    case 'RepresentedType':
      return Type.encode(type)
    case 'StructuralUnionType':
      return type.members.map(typeText).join(' | ')
  }
}

const callingScalarText = (scalar: Layout.CallingScalar): string =>
  typeof scalar === 'string' ? scalar : `Address<${Type.encode(scalar.element)},i${scalar.bits}>`

export const closureRows = (closure: ModuleClosure.Closure): ReadonlyArray<RowModel> => {
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

const declaredTypeText = (fact: DeclarationFacts.DeclaredTypeFact): string => {
  switch (fact._tag) {
    case 'Resolved':
      return typeText(fact.type)
    case 'Unresolved':
      return fact.spelling
    default:
      return 'unavailable'
  }
}

const memberSignature = (member: DeclarationFacts.MemberFact): string => {
  if (member._tag === 'RoleDeclaration')
    return `${member.visibility === 'Public' ? 'pub ' : ''}role`
  const parameters =
    member.typeParameters.length === 0
      ? ''
      : `<${member.typeParameters.map((parameter) => typeText(parameter.type)).join(', ')}>`
  if (member._tag === 'StructDeclaration')
    return `struct${parameters} · ${member.fields.length} field${member.fields.length === 1 ? '' : 's'}`
  if (member._tag === 'UnionDeclaration')
    return `union${parameters} · ${member.variants.length} variant${member.variants.length === 1 ? '' : 's'}`
  if (member._tag === 'EnumDeclaration') {
    const representation =
      member.representation._tag === 'Available'
        ? member.representation.scalar.spelling
        : (member.representation.spelling ?? 'unavailable')
    return `enum(${representation}) · ${member.members.length} member${member.members.length === 1 ? '' : 's'} · ${member.validity._tag.toLowerCase()}`
  }
  if (member._tag === 'ConstantDeclaration')
    return `${member.visibility === 'Public' ? 'pub ' : ''}const · ${declaredTypeText(member.declaredType)}`
  if (member._tag === 'ForeignStaticDeclaration')
    return `${member.direction === 'Import' ? 'extern' : 'export'} static · ${declaredTypeText(member.declaredType)} · ${member.foreign.symbol}`
  if (member._tag === 'AliasDeclaration')
    return `${member.visibility === 'Public' ? 'pub ' : ''}type · ${declaredTypeText(member.target)}`
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

const memberName = (member: DeclarationFacts.MemberFact): string =>
  member.name._tag === 'Present' ? member.name.spelling : 'unavailable name'

const declaredName = (name: DeclarationFacts.DeclaredName): string =>
  name._tag === 'Present' ? name.spelling : 'unavailable name'

const conformanceLabel = (conformance: DeclarationFacts.ConformanceFact): string => {
  const parameters =
    conformance.typeParameters.length === 0
      ? ''
      : `<${conformance.typeParameters.map((parameter) => declaredName(parameter.name)).join(', ')}>`
  return `impl${parameters} ${declaredTypeText(conformance.capability)} for ${declaredTypeText(conformance.provider)}`
}

export const indexRows = (index: DeclarationIndex.Index): ReadonlyArray<RowModel> => {
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
      })
      if (member._tag === 'EnumDeclaration')
        for (const enumMember of member.members) {
          const memberSpan = asSpan(enumMember.syntax.span)
          rows.push({
            key: `idx-${module.module}-${member.id.ordinal}-enum-member-${enumMember.id.ordinal}`,
            depth: 2,
            label: declaredName(enumMember.name),
            detail:
              enumMember.discriminant._tag === 'Available'
                ? `discriminant ${enumMember.discriminant.value}`
                : 'unavailable discriminant',
            span: memberSpan,
          })
        }
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

export const resolutionRows = (resolution: NameResolution.Resolution): ReadonlyArray<RowModel> => {
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
        ...(span === undefined ? {} : { span }),
        ...(binding._tag === 'Unavailable'
          ? { tone: 'warning' as const, dot: 'warning' as const }
          : {}),
      })
    }

    for (const [ordinal, outcome] of scope.imports.entries()) {
      const unavailable = outcome._tag === 'Unavailable'
      const span = asSpan(outcome.import.syntax.span)
      let detail = 'unavailable'
      if (!unavailable) {
        detail = `${outcome.bindings.length} binding${outcome.bindings.length === 1 ? '' : 's'}`
      }
      rows.push({
        key: `res-${scope.module}-import-${ordinal}`,
        depth: 1,
        dot: unavailable ? 'warning' : undefined,
        label: `import ${outcome.import.canonicalTarget ?? outcome.import.sourceSpelling ?? '∅'}`,
        detail,
        span,
        ...(unavailable ? { tone: 'warning' as const } : {}),
      })
    }
  }

  return rows
}

const bindingSiteText = (fact: Ownership.BindingFact): string => {
  switch (fact.site._tag) {
    case 'Parameter':
      return `parameter #${fact.site.parameter.ordinal}`
    case 'Temporary':
      return `temporary @${fact.site.owner.span.start}`
    case 'Let':
    case 'Pattern':
      return `let b${fact.site.binding.ordinal}`
  }
}

const loanSiteText = (site: Ownership.BindingSite): string => {
  switch (site._tag) {
    case 'Parameter':
      return `parameter #${site.parameter.ordinal}`
    case 'Let':
      return `let b${site.binding.ordinal}`
    case 'Pattern':
      return `pattern b${site.binding.ordinal}`
    case 'Temporary':
      return `temporary @${site.owner.span.start}`
  }
}

const cleanupText = (cleanup: CleanupPlan.CleanupPlan): string => {
  switch (cleanup._tag) {
    case 'NoCleanup':
      return 'no cleanup'
    case 'ParameterCleanup':
      return `${typeText(cleanup.type)} · symbolic cleanup`
    case 'AllocationCleanup':
      return `${typeText(cleanup.type)} · active reclaim ticket`
    case 'RawBufferCleanup':
      return `${typeText(cleanup.type)} · ${cleanupText(cleanup.allocation)}`
    case 'LocalSharedCoreCleanup':
      return `${typeText(cleanup.type)} · opaque decrement or last ${typeText(cleanup.element)} cleanup`
    case 'ExecutionCleanup':
      return `${typeText(cleanup.type)} · opaque package cleanup · ${cleanupText(cleanup.allocation)}`
    case 'WakeCleanup':
      return `${typeText(cleanup.type)} · generation readiness cleanup · ${cleanupText(cleanup.allocation)}`
    case 'HookCleanup':
      return `${typeText(cleanup.type)} · drop hook ${cleanup.hook.module}.${cleanup.hook.name} · ${cleanupText(cleanup.inner)}`
    case 'StructCleanup':
      return `${typeText(cleanup.type)} ${cleanup.fields
        .map(({ field }) => `#${field.ordinal}`)
        .join(' → ')}`
    case 'NominalUnionCleanup':
      return `${typeText(cleanup.type)} active variant · ${cleanup.variants
        .map((variant) => `${variant.ordinal}:${variant.variant.name}`)
        .join(', ')}`
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
    case 'RepresentedEffectCleanup':
      return `${typeText(cleanup.type)} stored ${typeText(cleanup.contract)} · lanes resolved at the complete instance`
    case 'EffectCleanup':
      return `${typeText(cleanup.type)} captures ${cleanup.slots
        .map(({ ordinal, cleanup: slot }) => `#${ordinal}:${cleanupText(slot)}`)
        .join(' → ')}`
    case 'EffectCompositeCleanup':
      return `${typeText(cleanup.type)} selected Effect alternative · ${cleanup.alternatives
        .map((alternative, ordinal) => `${ordinal}:${cleanupText(alternative)}`)
        .join(', ')}`
    case 'RepresentedCallableCleanup':
      return `${typeText(cleanup.type)} stored ${typeText(cleanup.contract)} · lanes resolved at the complete instance`
  }
}

export const ownershipRows = (facts: Ownership.ModuleOwnership): ReadonlyArray<RowModel> => {
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
    })

    for (const [ordinal, binding] of fn.bindings.entries()) {
      const bindSpan = asSpan(binding.liveFrom)
      rows.push({
        key: `own-${fn.declaration.id.ordinal}-b${ordinal}`,
        depth: 1,
        label: binding.name ?? '∅',
        detail: `${bindingSiteText(binding)} · ${binding.mutability.toLowerCase()} · ${binding.category._tag.toLowerCase()} · live [${binding.liveFrom.start}, ${binding.liveTo.end})${binding.movedAt === undefined ? '' : ' · moved'}`,
        span: bindSpan,
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
                  (release) => `${release.binding.name ?? '∅'} (${cleanupText(release.cleanup)})`,
                )
                .join(', ')
        }`,
        span: exitSpan,
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
      })
      for (const slot of callable.slots) {
        rows.push({
          key: `own-${fn.declaration.id.ordinal}-callable-${callable.site.span.start}-slot${slot.ordinal}`,
          depth: 2,
          label: `slot #${slot.ordinal} → parameter #${slot.parameterOrdinal}`,
          detail: `${slot.access.toLowerCase()} · ${slot.type === undefined ? 'unavailable type' : typeText(slot.type)} · ${cleanupText(slot.cleanup)}`,
          span: callableSpan,
        })
      }
    }

    for (const replacement of fn.replacements) {
      const replacementSpan = asSpan(replacement.span)
      rows.push({
        key: `own-${fn.declaration.id.ordinal}-replacement-${replacement.span.start}`,
        depth: 1,
        label: 'replace live value',
        detail: `${typeText(replacement.type)} · ${cleanupText(replacement.cleanup)} · r${replacement.region.ordinal}`,
        span: replacementSpan,
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
      })
      for (const arm of match.arms) {
        const cleanup = arm.cleanup
          .map((entry) => entry.path.map((field) => `#${field.ordinal}`).join('.') || 'payload')
          .join(', ')
        let selection = 'unknown'
        if (arm.universal) selection = '_'
        else if (arm.member !== undefined) selection = Match.encodeIdentity(arm.member)
        rows.push({
          key: `own-${fn.declaration.id.ordinal}-match-${match.span.start}-arm${arm.id.ordinal}`,
          depth: 2,
          label: `arm #${arm.id.ordinal} ${selection}`,
          detail: `${arm.provisionalGuard ? 'provisional guard' : 'direct selection'} · ${arm.bindings.length} binding${arm.bindings.length === 1 ? '' : 's'} · cleanup ${cleanup || 'none'}`,
          span: matchSpan,
        })
      }
    }
  }

  return rows
}

export const instanceRows = (discovery: Instances.Discovery): ReadonlyArray<RowModel> => {
  let entryDetail: string
  if (discovery.entry._tag === 'Resolved')
    entryDetail = `${discovery.entry.key.declaration.module}.${discovery.entry.key.declaration.name}`
  else if (discovery.entry._tag === 'Library')
    entryDetail = `library · ${discovery.foreignExports.length} exports`
  else entryDetail = `unavailable · ${discovery.entry.reason}`
  const rows: Array<RowModel> = [
    {
      key: 'entry',
      label: 'entry',
      detail: entryDetail,
      head: true,
      ...(discovery.entry._tag === 'Unavailable' ? { tone: 'warning' as const } : {}),
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
    })
  }

  for (const [ordinal, violation] of discovery.violations.entries()) {
    const caller = discovery.instances.find((instance) => instance.key === violation.caller)
    const span = caller === undefined ? undefined : asSpan(caller.function.declaration.syntax.span)
    rows.push({
      key: `inst-violation-${ordinal}`,
      depth: 1,
      dot: 'warning',
      tone: 'warning',
      label: 'polymorphic recursion',
      detail: `${violation.target.declaration.name}<${violation.target.typeArguments.map(Type.encodeGenericArgument).join(', ')}> changes an ancestor specialization`,
      ...(span === undefined ? {} : { span }),
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
      let representationText: string
      switch (entry.representation._tag) {
        case 'Aggregate':
          representationText = 'aggregate'
          break
        case 'CallableEnvironment':
          representationText = `stored callable · ${entry.representation.fields.length} capture${entry.representation.fields.length === 1 ? '' : 's'}`
          break
        case 'StoredEffectEnvironment':
          representationText = `stored Effect · ${entry.representation.fields.length} capture${entry.representation.fields.length === 1 ? '' : 's'}`
          break
        case 'Repeated':
          representationText = `${entry.representation.length} × ${typeText(entry.representation.element)} · stride ${entry.representation.stride}`
          break
        case 'Slice':
          representationText = `address i${entry.representation.address.bits} + length i32 · stride ${entry.representation.stride}`
          break
        case 'String':
          representationText = `UTF-8 address i${entry.representation.storage.bits} + byte length i${entry.representation.byteLength.size * 8}`
          break
        case 'Union':
          representationText = `sum · tag i${entry.representation.tag.bits} · payload +${entry.representation.payloadOffset}/${entry.representation.payloadSize}`
          break
        case 'NominalUnion':
          representationText = `nominal union · ${entry.representation.variants.length} variants · tag i${entry.representation.tag.bits} · payload +${entry.representation.payloadOffset}/${entry.representation.payloadSize}`
          break
        case 'Reference':
          representationText = `reference · address i${entry.representation.address.bits}`
          break
        case 'SignedInteger':
        case 'UnsignedInteger':
        case 'ScalarEnum':
        case 'Floating':
        case 'Boolean':
          representationText = `i${entry.representation.bits}`
          break
      }
      rows.push({
        key: `plan-${typeText(entry.type)}`,
        depth: 1,
        label: typeText(entry.type),
        detail: `${entry.size} bytes · align ${entry.alignment} · ${representationText}`,
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
        key: 'plan-word-literals',
        depth: 1,
        label: 'target word literal verdicts',
        detail: `${plan.literalVerdicts.length} target-checked`,
        head: true,
      })
      for (const [ordinal, verdict] of plan.literalVerdicts.entries()) {
        const available = verdict._tag === 'AvailableWordLiteral'
        rows.push({
          key: `plan-word-literal-${ordinal}`,
          depth: 2,
          dot: available ? 'symbol' : 'warning',
          ...(available ? {} : { tone: 'warning' as const }),
          label: `${verdict.type} ${verdict.value.toString()}`,
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
    .map((selector) => {
      switch (selector._tag) {
        case 'FieldSelector':
          return `.#${selector.field.ordinal}`
        case 'SliceElementSelector':
          return `[${localText(selector.index)} · ${selector.access.toLowerCase()} slice]`
        case 'ElementSelector': {
          const index =
            selector.index._tag === 'Proven'
              ? selector.index.value
              : localText(selector.index.local)
          return `[${index}/${selector.length}]`
        }
        default:
          return ''
      }
    })
    .join('')}`

const operationLabel = (operation: Mir.Operation): string => {
  switch (operation._tag) {
    case 'ForeignStaticLoad':
      return `${localText(operation.destination)} = foreign static ${operation.symbol}`
    case 'ForeignFunctionAddress':
      return `${localText(operation.destination)} = foreign address ${operation.symbol}`
    case 'Literal':
      return `${localText(operation.destination)} = const ${operation.value}`
    case 'EnumConstant':
      return `${localText(operation.destination)} = enum ${operation.enum.name}.${operation.member.name} · ${operation.discriminant}`
    case 'EnumValue':
      return `${localText(operation.destination)} = enum value ${localText(operation.source)} → ${operation.representation.scalar}`
    case 'EnumEquality':
      return `${localText(operation.destination)} = enum ${operation.negated ? 'not equals' : 'equals'} ${localText(operation.left)}, ${localText(operation.right)} · ${operation.enum.name}`
    case 'StaticView':
      return `${localText(operation.destination)} = static ${operation.data} · ${operation.length} bytes`
    case 'StaticString':
      return `${localText(operation.destination)} = static string ${operation.data} · ${operation.byteLength} bytes`
    case 'StringFromUtf8Unchecked':
      return `${localText(operation.destination)} = string from UTF-8 ${localText(operation.bytes)} · unsafe`
    case 'StringUtf8Bytes':
      return `${localText(operation.destination)} = UTF-8 bytes ${localText(operation.string)}`
    case 'StringByteLength':
      return `${localText(operation.destination)} = byte length ${localText(operation.string)}`
    case 'StringEqualsExact':
      return `${localText(operation.destination)} = exact string ${operation.negated ? 'not equals' : 'equals'} ${localText(operation.left)}, ${localText(operation.right)}`
    case 'PackEffectComposite':
      return `${localText(operation.destination)} = effect choice #${operation.alternative} ${localText(operation.source)}`
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
    case 'CheckedScalar':
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
    case 'PropagateEffectFailure':
      return `propagate effect failure ${localText(operation.source)} · ${operation.tagMappings.map((mapping) => `${mapping.source}→${mapping.target}`).join(', ')}`
    case 'UnpackEffectSuccess':
      return `${localText(operation.destination)} = effect success ${localText(operation.source)}`
    case 'RunEffect':
      return `${localText(operation.destination)} = run ${operation.target.name} · propagate ${operation.tagMappings.map((mapping) => `${mapping.source}→${mapping.target}`).join(', ') || 'none'}`
    case 'RunEffectValue':
      return `${localText(operation.destination)} = run ${localText(operation.effect)} with ${operation.runner.name}`
    case 'RunEffectComposite':
      return `${localText(operation.destination)} = run effect choice ${localText(operation.effect)}`
    case 'RunStaticEffect':
      return `${localText(operation.destination)} = run static ${operation.runner.name} with ${operation.captures.map((capture) => localText(capture.source)).join(', ') || 'no captures'}`
    case 'CatchEffect':
      return `${localText(operation.destination)} = result ${localText(operation.effect)} with ${operation.runner.name}`
    case 'CloseEffectEntry':
      return `${localText(operation.destination)} = close ${operation.target.name} with ${operation.runner.name}`
    case 'Construct':
      return `${localText(operation.destination)} = construct ${typeText(operation.type.type)} { ${operation.fields
        .map(({ field, value }) => `#${field.ordinal}: ${localText(value)}`)
        .join(', ')} }`
    case 'ConstructUnionVariant':
      return `${localText(operation.destination)} = construct ${typeText(operation.type.type)}.${operation.variant.name}#${operation.variantOrdinal} { ${operation.fields
        .map(({ field, value }) => `${DeclarationFacts.fieldIdKey(field)}: ${localText(value)}`)
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
    case 'Conditional':
      return `${localText(operation.destination)} = if ${localText(operation.condition)}`
    case 'ShortCircuit':
      return `${localText(operation.destination)} = ${operation.operator === 'And' ? '&&' : '||'} ${localText(operation.left)}`
    case 'HostWrite':
      return `${localText(operation.destination)} = write all ${localText(operation.bytes)} to stream ${localText(operation.stream)} ! ${operation.failure.name}`
    case 'OsOpen':
      return `${localText(operation.destination)} = ${Intrinsic.operationText(operation.operation)}(${operation.arguments.map(localText).join(', ')}) via ${localText(operation.success)}/${localText(operation.failure)}`
    case 'OsCall':
      return `${localText(operation.destination)} = ${Intrinsic.operationText(operation.operation)}(${operation.arguments.map(localText).join(', ')})`
    case 'ForeignCall':
      return `${localText(operation.destination)} = extern "C" ${operation.symbol}(${operation.arguments.map(localText).join(', ')})`
    case 'SharedFromAllocation':
      return `${localText(operation.destination)} = shared core from ${localText(operation.allocation)} with ${localText(operation.value)}`
    case 'ExecutionFromAllocation':
      return `${localText(operation.destination)} = execution from ${localText(operation.allocation)} with ${localText(operation.body)}`
    case 'ExecutionDrive':
      return `${localText(operation.destination)} = drive ${localText(operation.execution)} with ${localText(operation.branch)}`
    case 'ExecutionNotifyInitial':
      return `${localText(operation.destination)} = notify initial ${localText(operation.execution)}`
    case 'ExecutionWake':
      return `${localText(operation.destination)} = wake ${localText(operation.wake)} · take`
    case 'ExecutionPark':
      return `${localText(operation.destination)} = park with ${localText(operation.register)} · guard ${localText(operation.guard)}`
    case 'SharedClone':
      return `${localText(operation.destination)} = clone shared core ${localText(operation.self)}`
    case 'SharedWithMut':
      return `${localText(operation.destination)} = access shared core ${localText(operation.self)} with ${localText(operation.use)} or ${localText(operation.onConflict)}`
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
    case 'RawBufferCopy':
      return `${localText(operation.destination)} = copy ${localText(operation.source)} into ${localText(operation.buffer)}[${localText(operation.offset)}..+${localText(operation.length)}]`
    case 'RawBufferFill':
      return `${localText(operation.destination)} = fill ${localText(operation.buffer)}[${localText(operation.offset)}..+${localText(operation.length)}] = ${localText(operation.value)}`
    case 'PointerNull':
      return `${localText(operation.destination)} = null pointer`
    case 'PointerIsNull':
      return `${localText(operation.destination)} = is null ${localText(operation.pointer)}`
    case 'PointerFromReference':
      return `${localText(operation.destination)} = pointer from ${localText(operation.source)}`
    case 'PointerOffset':
      return `${localText(operation.destination)} = offset ${localText(operation.pointer)} by ${localText(operation.count)}`
    case 'PointerRead':
      return `${localText(operation.destination)} = read pointer ${localText(operation.pointer)}`
    case 'PointerWrite':
      return `${localText(operation.destination)} = write pointer ${localText(operation.pointer)} = ${localText(operation.value)}`
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

const regionOperations = (region: Mir.Region): ReadonlyArray<Mir.Operation> => {
  switch (region._tag) {
    case 'OperationRegion':
      return region.operations
    case 'CleanupRegion':
      return region.releases
    case 'ConditionalRegion':
    case 'LoopRegion':
      return []
  }
}

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

export const mirRows = (module: Mir.Module): ReadonlyArray<RowModel> => {
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
    const operationCount = MirVerification.operations(fn).length
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
        })
        if (operation._tag === 'Match') {
          for (const decision of operation.decisions) {
            rows.push({
              key: `${fnKey}-r${region.id.ordinal}-${ordinal}-decision-${Match.encodeIdentity(decision.member)}`,
              depth: 3,
              label: `decision ${Match.encodeIdentity(decision.member)}`,
              detail: decision.candidates
                .map((candidate) => `arm #${candidate.ordinal}`)
                .join(' → '),
            })
          }
          for (const arm of operation.arms) {
            const armSpan = asSpan(arm.provenance.span)
            let selection = 'unknown'
            if (arm.universal) selection = '_'
            else if (arm.member !== undefined) selection = Match.encodeIdentity(arm.member)
            rows.push({
              key: `${fnKey}-r${region.id.ordinal}-${ordinal}-arm${arm.id.ordinal}`,
              depth: 3,
              label: `arm #${arm.id.ordinal} ${selection}`,
              detail: `${arm.guard === undefined ? 'selected' : `guard ${localText(arm.guard.result)}`} · result ${localText(arm.selected.result)} → ${localText(operation.destination)} · cleanup ${arm.selected.cleanup.length}`,
              span: armSpan,
            })
            for (const binding of arm.bindings) {
              const bindingSpan = asSpan(binding.provenance.span)
              rows.push({
                key: `${fnKey}-r${region.id.ordinal}-${ordinal}-arm${arm.id.ordinal}-binding${binding.id.ordinal}`,
                depth: 4,
                label: `${localText(binding.destination)} = payload ${binding.path.map((field) => `#${field.ordinal}`).join('.')}`,
                detail: `${binding.access.toLowerCase()} · ${typeText(Mir.semanticType(binding.type))}`,
                span: bindingSpan,
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
        })
      } else {
        const span = asSpan(region.provenance.span)
        let label: string
        if (region._tag === 'ConditionalRegion') {
          const following =
            region.following === undefined ? '' : ` · following r${region.following.ordinal}`
          label = `taken r${region.taken.ordinal} · otherwise r${region.otherwise.ordinal}${following}`
        } else {
          label = `condition r${region.condition.ordinal} · body r${region.body.ordinal} · following r${region.following.ordinal}`
        }
        rows.push({
          key: `${fnKey}-r${region.id.ordinal}-control`,
          depth: 2,
          label,
          detail: 'structural edges',
          span,
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
 * Canonical struct-value facts: literals, projections, calling shapes.
 *
 * The literal's two orders are the point: the source wrote `{ right: 2, left: 1 }` but the
 * canonical struct order is `left, right`, and the compiler owns that reordering. Showing both
 * on one row pair is what makes the reordering inspectable rather than folklore.
 */
export const structValueRows = (
  literals: ReadonlyArray<Elaboration.StructLiteralExpressionFact>,
  projections: ReadonlyArray<Elaboration.FieldProjectionExpressionFact>,
  shapes: ReadonlyArray<Layout.CallingShape>,
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
      label:
        literal.target._tag === 'Resolved' ? typeText(literal.target.type) : 'unavailable target',
      detail: literal.authorized ? 'fields visible' : 'field access denied',
      span,
      ...(literal.target._tag === 'Resolved' && literal.authorized
        ? {}
        : { tone: 'warning' as const }),
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
    if (literal.typeArguments.length > 0)
      rows.push({
        key: `${key}-arguments`,
        depth: 2,
        label: 'type arguments',
        detail: literal.typeArguments
          .map(
            (argument) =>
              `${argument.parameter.name}=${argument.argument === undefined ? '?' : Type.encodeGenericArgument(argument.argument)} (${argument.source.toLowerCase()})`,
          )
          .join(', '),
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
          .map((lane) => `${callingScalarText(lane.type)}:${selectorPathText(lane.path)}`)
          .join(', ') || 'zero runtime lanes',
    })
  }

  return rows
}

const selectorPathText = (path: ReadonlyArray<Layout.Selector>): string =>
  path
    .map((selector) => {
      switch (selector._tag) {
        case 'ElementSelector':
          return `[${selector.index}]`
        case 'FieldId':
          return `#${selector.ordinal}`
        case 'UnionTagSelector':
          return 'tag'
        case 'UnionPayloadSelector':
          return `payload[${selector.slot}]`
        case 'NominalUnionTagSelector':
          return 'nominal-tag'
        case 'NominalUnionPayloadSelector':
          return `nominal-payload[${selector.slot}]`
        case 'SliceAddressSelector':
          return 'address'
        default:
          return 'length'
      }
    })
    .join('.')

/** Canonical array facts from syntax through backend-neutral ABI paths. */
export const arrayValueRows = (
  types: ReadonlyArray<Type.FixedArray>,
  literals: ReadonlyArray<Elaboration.ArrayLiteralExpressionFact>,
  projections: ReadonlyArray<Elaboration.IndexProjectionExpressionFact>,
  layouts: ReadonlyArray<Layout.Entry>,
  shapes: ReadonlyArray<Layout.CallingShape>,
): ReadonlyArray<RowModel> => {
  const rows: Array<RowModel> = [
    {
      key: 'array-types',
      label: 'canonical array types',
      detail: types.length === 0 ? 'none' : `${types.length}`,
      head: true,
    },
    ...types.map((type, ordinal): RowModel => ({
      key: `array-type-${ordinal}`,
      depth: 1,
      dot: 'symbol',
      label: typeText(type),
      detail: `length ${type.length} · element ${typeText(type.element)}`,
      tone: 'symbol',
    })),
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
    let label: string
    if (literal.state._tag === 'Complete') label = typeText(literal.state.type)
    else if (literal.expected === undefined) label = 'unavailable array'
    else label = typeText(literal.expected)
    rows.push({
      key,
      depth: 1,
      dot: literal.state._tag === 'Complete' ? 'ok' : 'warning',
      label,
      detail: `${literal.length} element${literal.length === 1 ? '' : 's'} · ${literal.state._tag}`,
      span,
      ...(literal.state._tag === 'Complete' ? {} : { tone: 'warning' as const }),
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
    let boundsDetail = 'bounds unavailable'
    switch (projection.bounds._tag) {
      case 'Proven':
        boundsDetail = `proven ${projection.bounds.index}/${projection.bounds.length}`
        break
      case 'Runtime':
        boundsDetail = `runtime check < ${projection.bounds.length}`
        break
      case 'Invalid':
        boundsDetail = `invalid ${projection.bounds.index}/${projection.bounds.length}`
        break
      default:
        break
    }
    rows.push({
      key: `array-index-${ordinal}`,
      depth: 1,
      dot: projection.bounds._tag === 'Invalid' ? 'warning' : 'ok',
      label: projection.array === undefined ? '?[index]' : `${typeText(projection.array)}[index]`,
      detail: `${projection.access} · ${boundsDetail}`,
      span,
      ...(projection.bounds._tag === 'Invalid' ? { tone: 'warning' as const } : {}),
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
        shape.lanes
          .map((lane) => `${callingScalarText(lane.type)}:${selectorPathText(lane.path)}`)
          .join(', ') || 'zero runtime lanes',
    })
  }

  return rows
}

/**
 * The whole pipeline as one row per phase.
 *
 * Every other view answers "what did this phase produce"; this one answers "how far did the
 * program get". It is the only view that is about the pipeline rather than about a phase, which
 * is why it remains part of the static project inspector.
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
