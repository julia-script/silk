'use client'

/**
 * Module, name, ownership, lowering and backend phases as rows.
 *
 * The split from `project-syntax` is by phase rather than by size: these are the phases that can
 * be *absent* — a target that did not resolve leaves no MIR, a program that did not elaborate
 * leaves no backend output. Each projection here states that absence as a row rather than
 * returning nothing, which is what keeps a broken pipeline readable.
 */

import type {
  BootstrapEvaluation,
  DeclarationIndex,
  Instances,
  Layout,
  Mir,
  ModuleClosure,
  NameResolution,
  Ownership,
  Type,
} from '@silk-effect/compiler'
import type { RowModel, Span } from './row'

const typeText = (type: Type.Type): string =>
  typeof type === 'string' ? type : `${type.module}.${type.name}`

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
        detail: `${bindingSiteText(binding)} · ${binding.category._tag.toLowerCase()} · live [${
          binding.liveFrom.start
        }, ${binding.liveTo.end})${binding.movedAt === undefined ? '' : ' · moved'}`,
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
            : exit.releases.map((release) => release.binding.name ?? '∅').join(', '),
        span: exitSpan,
        onActivate: () => onPick(exitSpan),
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
    case 'Call':
      return `${localText(operation.destination)} = call ${operation.target.name}(${operation.arguments
        .map(localText)
        .join(', ')})`
    case 'Drop':
      return `drop ${localText(operation.local)}`
  }
}

const terminatorLabel = (terminator: Mir.Terminator): string => {
  switch (terminator._tag) {
    case 'Return':
      return `return ${localText(terminator.value)}`
    case 'Jump':
      return `jump bb${terminator.target.ordinal}`
    case 'Branch':
      return `branch ${localText(terminator.condition)} ? bb${terminator.taken.ordinal} : bb${
        terminator.otherwise.ordinal
      }`
    case 'Trap':
      return `trap "${terminator.reason}"`
  }
}

export const mirRows = (
  module: Mir.Module,
  onPick: (span: Span) => void,
): ReadonlyArray<RowModel> => {
  const rows: Array<RowModel> = []

  for (const fn of module.functions) {
    const localCount = fn.blocks.reduce((total, block) => total + block.operations.length, 0)
    rows.push({
      key: `mir-${fn.id.name}`,
      dot: 'symbol',
      label: fn.id.name,
      detail: `fn · ${fn.blocks.length} bb · ${localCount} op${localCount === 1 ? '' : 's'}`,
      head: true,
      tone: 'symbol',
    })

    for (const block of fn.blocks) {
      rows.push({
        key: `mir-${fn.id.name}-bb${block.id.ordinal}`,
        depth: 1,
        label: `bb${block.id.ordinal}`,
        detail: block.kind === 'Cleanup' ? 'cleanup' : 'entry',
      })

      for (const [ordinal, operation] of block.operations.entries()) {
        const span = asSpan(operation.provenance.span)
        rows.push({
          key: `mir-${fn.id.name}-bb${block.id.ordinal}-${ordinal}`,
          depth: 2,
          label: operationLabel(operation),
          detail: operation.provenance.generated ? 'generated' : operation._tag.toLowerCase(),
          span,
          onActivate: () => onPick(span),
        })
      }

      const span = asSpan(block.terminator.provenance.span)
      rows.push({
        key: `mir-${fn.id.name}-bb${block.id.ordinal}-term`,
        depth: 2,
        label: terminatorLabel(block.terminator),
        detail: 'terminator',
        span,
        onActivate: () => onPick(span),
      })
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

const traceLabel = (event: BootstrapEvaluation.TraceEvent): string => {
  switch (event._tag) {
    case 'Entry':
      return `enter ${event.function.module}.${event.function.name}`
    case 'Call':
      return `call ${event.target.module}.${event.target.name}`
    case 'Binding':
      return `bind p${event.parameterOrdinal} = ${event.value.value}`
    case 'Return':
      return `return ${event.value.value}`
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
