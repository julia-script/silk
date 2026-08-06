'use client'

/**
 * Module, name, ownership, lowering and backend phases as rows.
 *
 * The split from `project-syntax` is by phase rather than by size: these are the phases that can
 * be *absent* — a target that did not resolve leaves no MIR, a program that did not elaborate
 * leaves no backend output. Each projection here states that absence as a row rather than
 * returning nothing, which is what keeps a broken pipeline readable.
 */

import { Mir } from '@silk-effect/compiler'
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
  Type,
} from '@silk-effect/compiler'
import type { RowModel, Span } from './row'

const typeText = (type: Type.Type): string =>
  typeof type === 'string'
    ? type
    : type._tag === 'NominalType'
      ? `${type.module}.${type.name}`
      : type._tag === 'FixedArrayType'
        ? `Array<${typeText(type.element)}, ${type.length}>`
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
  if (member._tag === 'StructDeclaration')
    return `struct · ${member.fields.length} field${member.fields.length === 1 ? '' : 's'}`
  const parameters = member.parameters
    .map(
      (parameter) =>
        `${parameter.name._tag === 'Present' ? parameter.name.spelling : '∅'}: ${declaredTypeText(
          parameter.declaredType,
        )}`,
    )
    .join(', ')
  return `${member.visibility === 'Public' ? 'pub ' : ''}fn · (${parameters}) -> ${declaredTypeText(
    member.returnType,
  )}`
}

const memberName = (member: DeclarationIndex.MemberFact): string =>
  member.name._tag === 'Present' ? member.name.spelling : 'unavailable name'

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

const cleanupText = (cleanup: Ownership.CleanupPlan): string => {
  switch (cleanup._tag) {
    case 'NoCleanup':
      return 'no cleanup'
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
        detail:
          exit.releases.length === 0
            ? 'no releases'
            : exit.releases
                .map(
                  (release) =>
                    `${release.binding.name ?? '∅'} (${cleanupText(release.cleanup)})`,
                )
                .join(', '),
        span: exitSpan,
        onActivate: () => onPick(exitSpan),
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
      label: `${instance.key.declaration.module}.${instance.key.declaration.name}`,
      detail: 'instantiated once',
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
              : entry.representation._tag === 'Union'
                ? `sum · tag i${entry.representation.tag.bits} · payload +${entry.representation.payloadOffset}/${entry.representation.payloadSize}`
                : `i${entry.representation.bits}`
        }`,
      })
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
    case 'Binary':
      return `${localText(operation.destination)} = ${operation.operator.toLowerCase()} ${localText(
        operation.left,
      )}, ${localText(operation.right)}`
    case 'Move':
      return `${localText(operation.destination)} = move ${localText(operation.source)}`
    case 'ConvertUnion':
      return `${localText(operation.destination)} = ${operation.conversion.toLowerCase()} ${localText(operation.source)} → ${typeText(operation.targetType.type)}`
    case 'Call':
      return `${localText(operation.destination)} = call ${operation.target.name}(${operation.arguments
        .map(localText)
        .join(', ')})`
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
    const operationCount = Mir.operations(fn).length
    rows.push({
      key: `mir-${fn.id.name}`,
      dot: 'symbol',
      label: fn.id.name,
      detail: `fn · entry r${fn.entry.ordinal} · ${fn.regions.length} region${fn.regions.length === 1 ? '' : 's'} · ${operationCount} op${operationCount === 1 ? '' : 's'}`,
      head: true,
      tone: 'symbol',
    })

    for (const region of Mir.topologicalRegions(fn)) {
      rows.push({
        key: `mir-${fn.id.name}-r${region.id.ordinal}`,
        depth: 1,
        label: `r${region.id.ordinal} · ${region._tag.replace('Region', '').toLowerCase()}`,
        detail: regionDetail(region),
      })

      for (const [ordinal, operation] of regionOperations(region).entries()) {
        const span = asSpan(operation.provenance.span)
        rows.push({
          key: `mir-${fn.id.name}-r${region.id.ordinal}-${ordinal}`,
          depth: 2,
          label: operationLabel(operation),
          detail: operation.provenance.generated ? 'generated' : operation._tag.toLowerCase(),
          span,
          onActivate: () => onPick(span),
        })
      }

      if (region._tag === 'OperationRegion' || region._tag === 'CleanupRegion') {
        const span = asSpan(region.outcome.provenance.span)
        rows.push({
          key: `mir-${fn.id.name}-r${region.id.ordinal}-outcome`,
          depth: 2,
          label: outcomeLabel(region.outcome),
          detail: 'outcome',
          span,
          onActivate: () => onPick(span),
        })
      } else {
        const span = asSpan(region.provenance.span)
        rows.push({
          key: `mir-${fn.id.name}-r${region.id.ordinal}-control`,
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
    : value._tag === 'ArrayValue'
      ? `${typeText(value.type)} [${value.elements.map(valueText).join(', ')}]`
      : value._tag === 'UnionValue'
        ? `${typeText(value.type)} <${typeText(value.member)} ${valueText(value.payload)}>`
        : `${typeText(value.type)} { ${value.fields.map((entry) => valueText(entry.value)).join(', ')} }`

const traceLabel = (event: BootstrapEvaluation.TraceEvent): string => {
  switch (event._tag) {
    case 'Entry':
      return `enter ${event.function.module}.${event.function.name}`
    case 'Call':
      return `call ${event.target.module}.${event.target.name}`
    case 'Binding':
      return `bind p${event.parameterOrdinal} = ${valueText(event.value)}`
    case 'Return':
      return `return ${valueText(event.value)}`
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
            : `${typeText(selector.array)}[${selector.index}] ${selector.bounds.toLowerCase()}`,
        )
        .join(' → ')} = ${valueText(event.value)}`
    case 'Cleanup':
      return `cleanup _${event.local}${event.members === undefined ? '' : ` · active ${event.members.map(typeText).join(', ')}`}`
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
  }
}

const traceDepth = (event: BootstrapEvaluation.TraceEvent): number => {
  switch (event._tag) {
    case 'Entry':
      return 0
    case 'Call':
      return 1
    case 'Binding':
      return 2
    case 'Return':
      return 1
    case 'Construct':
    case 'ArrayConstruct':
    case 'UnionConversion':
    case 'Project':
    case 'PlaceRead':
    case 'Cleanup':
    case 'RegionEntry':
    case 'Condition':
    case 'Iteration':
    case 'WriteCheck':
    case 'ReplacementCleanup':
    case 'Replacement':
    case 'Repeat':
    case 'Exit':
    case 'Transfer':
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
    case 'RecursiveCycle':
      return `recursive cycle · ${reason.cycle.map((id) => id.name).join(' → ')}`
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
            : `payload[${selector.slot}]`,
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
