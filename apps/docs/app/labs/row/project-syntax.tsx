'use client'

/**
 * Syntax and semantics phases, projected into the shared row grammar.
 *
 * These are pure `snapshot → rows` functions rather than components, which is what lets a pane
 * stay a thin shell: the pane owns scrolling and the cursor, the projection owns what a phase
 * has to say. It also makes each phase testable without rendering anything.
 */

import { Diagnostic, SyntaxTree } from '@silk-effect/compiler'
import type { Elaboration, Hir, SyntaxFile, Type } from '@silk-effect/compiler'
import type { FlowModel } from './flow-model'
import type { RowModel, RowTone, Span } from './row'

const decoder = new TextDecoder()

/** One diagnostic paired with the identity other diagnostics cite it by. */
export interface DiagnosticEntry {
  readonly diagnostic: Diagnostic.Diagnostic
  readonly identity: Diagnostic.Identity
}

/**
 * Every phase's diagnostics in one ordered list.
 *
 * Identity matters because a diagnostic can name another as its cause, and the citation has to
 * survive being merged with diagnostics from other phases — hence identify-then-sort rather than
 * a plain concatenation.
 */
export const diagnosticEntries = (
  ...collections: ReadonlyArray<ReadonlyArray<Diagnostic.Diagnostic>>
): ReadonlyArray<DiagnosticEntry> =>
  collections
    .flatMap((diagnostics) => {
      const identities = Diagnostic.identify(diagnostics)
      return diagnostics.map((diagnostic, index) => ({
        diagnostic,
        identity: identities[index] ?? Diagnostic.identity(diagnostic),
      }))
    })
    .sort((left, right) => Diagnostic.compare(left.diagnostic, right.diagnostic))

/** Trivia is most of a lossless tree's rows and almost never what you are looking at. */
const TRIVIA = new Set(['Whitespace', 'Newline', 'Comment'])

export const isTrivia = (kind: string): boolean => TRIVIA.has(kind)

const sliceOf = (source: SyntaxFile.SyntaxFile['source'], span: Span): string =>
  decoder.decode(Uint8Array.from(source.bytes.slice(span.start, span.end)))

const spanOf = (element: { readonly span: Span }): Span => ({
  start: element.span.start,
  end: element.span.end,
})

export const tokenRows = (
  syntax: SyntaxFile.SyntaxFile,
  onPick: (span: Span) => void,
  showTrivia: boolean,
  filter: string,
): ReadonlyArray<RowModel> => {
  const needle = filter.trim().toLowerCase()
  return (
    syntax.tokens
      // The index is kept from the *unfiltered* stream so a row's number is its real position in
      // the token stream. Numbering after the filter would renumber 1..n on every keystroke and
      // quietly disagree with every other phase's idea of which token this is.
      .map((token, index) => ({ token, index }))
      .filter(({ token }) => (showTrivia ? true : !isTrivia(token.kind)))
      .filter(({ token }) => needle === '' || token.kind.toLowerCase().includes(needle))
      .map(({ token, index }) => {
        const span = spanOf(token)
        return {
          key: `${token.kind}-${span.start}-${index}`,
          lead: index + 1,
          dot: 'token' as const,
          label: token.kind,
          detail: JSON.stringify(sliceOf(syntax.source, span)),
          span,
          onActivate: () => onPick(span),
        }
      })
  )
}

/**
 * The lossless concrete tree, flattened to rows with depth as indentation.
 *
 * Missing elements are rendered as real rows rather than being skipped: recovery is the point of
 * the tree being lossless, so a node the parser inserted has to be visible as one — amber, with
 * the zero-width span it was inserted at.
 */
export const treeRows = (
  syntax: SyntaxFile.SyntaxFile,
  onPick: (span: Span) => void,
  showTrivia: boolean,
  filter: string,
): ReadonlyArray<RowModel> => {
  const rows: Array<RowModel> = []
  const needle = filter.trim().toLowerCase()

  const walk = (element: SyntaxTree.Element, depth: number, path: string): void => {
    if (SyntaxTree.isNode(element)) {
      const span = spanOf(element)
      rows.push({
        key: `${path}-${element.kind}-${span.start}`,
        depth,
        caret: '▾',
        dot: 'node',
        label: element.kind,
        span,
        head: depth === 0,
        onActivate: () => onPick(span),
      })
      element.children.forEach((child, index) => walk(child, depth + 1, `${path}.${index}`))
      return
    }

    if (SyntaxTree.isToken(element)) {
      if (!showTrivia && isTrivia(element.kind)) return
      const span = spanOf(element)
      rows.push({
        key: `${path}-${element.kind}-${span.start}`,
        depth,
        dot: 'token',
        label: element.kind,
        detail: JSON.stringify(sliceOf(syntax.source, span)),
        span,
        onActivate: () => onPick(span),
      })
      return
    }

    const span = spanOf(element)
    rows.push({
      key: `${path}-missing-${span.start}`,
      depth,
      dot: 'missing',
      label: element.expected,
      detail: 'missing — inserted by recovery',
      span,
      tone: 'warning',
      onActivate: () => onPick(span),
    })
  }

  walk(syntax.root, 0, 'r')

  // Filtering a tree keeps ancestors of a match, so a hit never appears without the context that
  // explains where it is.
  if (needle === '') return rows
  const kept = new Set<number>()
  const stack: Array<number> = []
  rows.forEach((row, index) => {
    const depth = row.depth ?? 0
    stack.length = depth
    stack[depth] = index
    if (row.label.toLowerCase().includes(needle)) {
      for (const ancestor of stack) if (ancestor !== undefined) kept.add(ancestor)
    }
  })
  return rows.filter((_, index) => kept.has(index))
}

const hirSpan = (span: { readonly start: number; readonly end: number }): Span => ({
  start: span.start,
  end: span.end,
})

/** Contract types are `Type.Type`, so a struct parameter or result is an object, not a string. */
export const hirContract = (contract: Hir.ContractFact): string =>
  contract._tag === 'Contract'
    ? `${contract.functionKind === 'Effect' ? 'effect ' : ''}(${contract.parameters.map(hirTypeText).join(', ')}) -> ${hirTypeText(contract.result)}${
        contract.functionKind === 'Effect'
          ? ` ! ${contract.failures?.map(hirTypeText).join(' | ') || 'empty'}`
          : ''
      }`
    : 'contract unavailable'

const hirIdentity = (declaration: Hir.HirFunction['declaration']): string => {
  switch (declaration.canonical._tag) {
    case 'Canonical':
      return declaration.canonical.id.name
    case 'Duplicate':
      return `duplicate of ${declaration.canonical.original.name}`
    case 'Unidentified':
      return 'unidentified'
  }
}

const hirTypeText = (type: Type.Type): string =>
  typeof type === 'string'
    ? type
    : type._tag === 'NominalType'
      ? `${type.module}.${type.name}${
          type.arguments.length === 0
            ? ''
            : `<${type.arguments.map(hirTypeText).join(', ')}>`
        }`
      : type._tag === 'TypeParameter'
        ? type.name
      : type._tag === 'FixedArrayType'
        ? `Array<${hirTypeText(type.element)}, ${type.length}>`
      : type._tag === 'SliceType'
        ? `${type.access === 'Exclusive' ? '&mut ' : '&'}[${hirTypeText(type.element)}]`
        : type._tag === 'EffectType'
          ? `Effect<${hirTypeText(type.success)}${
              type.failures.length === 0
                ? ''
                : ` ! ${type.failures.map(hirTypeText).join(' | ')}`
            }> ${type.access.toLowerCase()}`
          : type._tag === 'CallableType'
            ? `(${type.parameters.map(hirTypeText).join(', ')}) -> ${hirTypeText(type.result)} ${type.mode.toLowerCase()}`
            : type._tag === 'ReferenceType'
              ? `${type.access === 'Exclusive' ? '&mut ' : '&'}${hirTypeText(type.target)}`
              : type.members.map(hirTypeText).join(' | ')

const hirExpressionLabel = (expression: Hir.Expression): string => {
  switch (expression._tag) {
    case 'IntegerLiteral':
    case 'BooleanLiteral':
      return `const ${expression.value}`
    case 'ParameterReference':
      return `param fn${expression.parameter.function.ordinal}.p${expression.parameter.ordinal}`
    case 'BindingReference':
      return `binding fn${expression.binding.function.ordinal}.b${expression.binding.ordinal}`
    case 'PatternBindingReference':
      return `pattern b${expression.binding.ordinal}`
    case 'Move':
      return 'move'
    case 'UnionConvert':
      return `${expression.conversion.toLowerCase()} → ${hirTypeText(expression.target)}`
    case 'Match':
      return `match ${expression.access.toLowerCase()} · ${expression.members.map(hirTypeText).join(' | ')}`
    case 'Construct':
      return `construct ${hirTypeText(expression.nominal)}`
    case 'ArrayConstruct':
      return `array ${hirTypeText(expression.type)} · ${expression.elements.length} elements`
    case 'Project':
      return `project ${hirTypeText(expression.nominal)}.#${expression.field.ordinal}`
    case 'IndexPlace':
      return `index ${hirTypeText(expression.array)} · ${expression.bounds._tag.toLowerCase()}`
    case 'SliceBorrow':
      return `${expression.reborrow ? 'reborrow' : 'borrow'} ${expression.access.toLowerCase()} · loan #${expression.borrow.ordinal}`
    case 'SliceLength':
      return 'slice length'
    case 'SliceIndexPlace':
      return `slice index ${expression.access.toLowerCase()}`
    case 'Call':
      return `call ${expression.target.name}${
        expression.typeArguments.length === 0
          ? ''
          : `<${expression.typeArguments.map(hirTypeText).join(', ')}>`
      } · loan ends ${expression.loanEnds.map((loan) => `#${loan.ordinal}`).join(', ') || 'none'}`
    case 'FunctionItem':
      return `function ${expression.target._tag === 'DeclarationCallableTarget' ? expression.target.declaration.name : `${expression.target.actor}.${expression.target.operation}`}`
    case 'CallableSection':
      return `section ${expression.target._tag === 'DeclarationCallableTarget' ? expression.target.declaration.name : `${expression.target.actor}.${expression.target.operation}`} · ${expression.mode.toLowerCase()} · ${expression.captures.length} capture${expression.captures.length === 1 ? '' : 's'}`
    case 'CallableApply':
      return `apply callable · ${expression.access.toLowerCase()} · ${expression.evaluation.toLowerCase()}`
    case 'EffectConstruct':
      return `effect recipe ${expression.target.name}${
        expression.typeArguments.length === 0
          ? ''
          : `<${expression.typeArguments.map(hirTypeText).join(', ')}>`
      } · ${expression.type.access.toLowerCase()}`
    case 'EffectBlock':
      return `effect block · ${expression.type.access.toLowerCase()}`
    case 'Run':
      return 'run recipe'
    case 'EffectCatch':
      return `catch ${hirTypeText(expression.handled)} with ${hirExpressionLabel(expression.handler)}`
    case 'EffectRetry':
      return 'retry effect'
    case 'EffectTransform':
      return `${expression.operation.toLowerCase()} effect with ${hirExpressionLabel(expression.callback)}`
    case 'EffectProvide':
      return `provide ${hirTypeText(expression.provider.capability)}@${expression.provider.role}`
    case 'EffectProvideWith':
      return `provideWith ${hirTypeText(expression.capability)}@${expression.role}`
    case 'BuiltinCall':
      return `builtin i32.${expression.operation}`
    default:
      return 'unavailable'
  }
}

export const hirRows = (
  hir: Hir.Module,
  onPick: (span: Span) => void,
): ReadonlyArray<RowModel> => {
  const rows: Array<RowModel> = []

  const expression = (node: Hir.Expression, depth: number, path: string): void => {
    const span = hirSpan(node.span)
    rows.push({
      key: `${path}-${node._tag}-${span.start}`,
      depth,
      dot: node._tag === 'Unavailable' ? 'warning' : undefined,
      label: hirExpressionLabel(node),
      detail: node._tag === 'Unavailable' ? 'unavailable' : `: ${hirTypeText(node.type)}`,
      span,
      ...(node._tag === 'Unavailable' ? { tone: 'warning' as const } : {}),
      onActivate: () => onPick(span),
    })
    if (node._tag === 'Call' || node._tag === 'EffectConstruct' || node._tag === 'BuiltinCall') {
      node.arguments.forEach((argument, index) =>
        expression(argument, depth + 1, `${path}.${index}`),
      )
    }
    if (node._tag === 'Move' || node._tag === 'Project' || node._tag === 'Run') {
      expression(node.subject, depth + 1, `${path}.s`)
    }
    if (node._tag === 'CallableSection') {
      node.captures.forEach((capture) =>
        expression(capture.value, depth + 1, `${path}.capture${capture.ordinal}`),
      )
    }
    if (node._tag === 'CallableApply') {
      expression(node.callee, depth + 1, `${path}.callee`)
      node.arguments.forEach((argument, index) =>
        expression(argument, depth + 1, `${path}.argument${index}`),
      )
    }
    if (node._tag === 'EffectCatch') {
      expression(node.protected, depth + 1, `${path}.protected`)
      expression(node.handler, depth + 1, `${path}.handler`)
    }
    if (node._tag === 'EffectRetry') {
      expression(node.protected, depth + 1, `${path}.protected`)
      expression(node.retries, depth + 1, `${path}.retries`)
    }
    if (node._tag === 'EffectTransform') {
      expression(node.protected, depth + 1, `${path}.protected`)
      expression(node.callback, depth + 1, `${path}.callback`)
    }
    if (node._tag === 'EffectProvide') {
      expression(node.protected, depth + 1, `${path}.protected`)
    }
    if (node._tag === 'EffectProvideWith') {
      expression(node.protected, depth + 1, `${path}.protected`)
      expression(node.acquisition, depth + 1, `${path}.acquisition`)
    }
    if (node._tag === 'SliceLength') expression(node.slice, depth + 1, `${path}.s`)
    if (node._tag === 'SliceIndexPlace') {
      expression(node.slice, depth + 1, `${path}.s`)
      expression(node.index, depth + 1, `${path}.i`)
    }
    if (node._tag === 'UnionConvert') {
      expression(node.source, depth + 1, `${path}.u`)
    }
    if (node._tag === 'IndexPlace') {
      expression(node.subject, depth + 1, `${path}.s`)
      expression(node.index, depth + 1, `${path}.i`)
    }
    if (node._tag === 'Construct') {
      // Canonical storage order, one child per field — the reordering from source order is the
      // struct-values pane's story; here the construct is just a typed expression tree.
      node.fields.forEach(({ field, value }) =>
        expression(value, depth + 1, `${path}.f${field.ordinal}`),
      )
    }
    if (node._tag === 'ArrayConstruct') {
      node.elements.forEach((element, index) =>
        expression(element, depth + 1, `${path}.e${index}`),
      )
    }
    if (node._tag === 'Match') {
      expression(node.scrutinee, depth + 1, `${path}.scrutinee`)
      node.arms.forEach((arm) => {
        const armSpan = hirSpan(arm.span)
        rows.push({
          key: `${path}.arm${arm.id.ordinal}`,
          depth: depth + 1,
          label: `arm #${arm.id.ordinal} ${arm.universal ? '_' : arm.member === undefined ? 'unknown' : hirTypeText(arm.member)}`,
          detail: `${arm.guard === undefined ? 'selected directly' : 'guarded'} · ${arm.bindings.length} binding${arm.bindings.length === 1 ? '' : 's'} · ${arm.before.map(hirTypeText).join(' | ') || 'empty'} → ${arm.after.map(hirTypeText).join(' | ') || 'empty'}`,
          span: armSpan,
          onActivate: () => onPick(armSpan),
        })
        if (arm.guard !== undefined) {
          expression(arm.guard, depth + 2, `${path}.arm${arm.id.ordinal}.guard`)
        }
        expression(arm.result, depth + 2, `${path}.arm${arm.id.ordinal}.result`)
      })
    }
  }

  const statement = (node: Hir.Statement, depth: number, path: string): void => {
    const span = hirSpan(node.span)
    if (node._tag === 'Bind') {
      rows.push({
        key: `${path}-bind-${span.start}`,
        depth,
        label: `bind ${node.mutability.toLowerCase()} b${node.binding.ordinal} ${node.name ?? '∅'}`,
        detail: `r${node.region.ordinal}`,
        span,
        onActivate: () => onPick(span),
      })
      expression(node.initializer, depth + 1, `${path}.i`)
      return
    }
    if (node._tag === 'If') {
      rows.push({
        key: `${path}-if-${span.start}`,
        depth,
        label: 'if',
        span,
        onActivate: () => onPick(span),
      })
      expression(node.condition, depth + 1, `${path}.c`)
      node.taken.forEach((inner, index) => statement(inner, depth + 1, `${path}.t${index}`))
      node.otherwise.forEach((inner, index) => statement(inner, depth + 1, `${path}.e${index}`))
      return
    }
    if (node._tag === 'Write') {
      rows.push({
        key: `${path}-write-${span.start}`,
        depth,
        label: `write b${node.place.root.ordinal}${node.place.selectors
          .map((selector) =>
            selector._tag === 'Field' ? `.#${selector.field.ordinal}` : '[index]',
          )
          .join('')}`,
        detail: `r${node.region.ordinal}`,
        span,
        onActivate: () => onPick(span),
      })
      for (const [index, selector] of node.place.selectors.entries()) {
        if (selector._tag === 'Index') expression(selector.index, depth + 1, `${path}.s${index}`)
      }
      expression(node.value, depth + 1, `${path}.v`)
      return
    }
    if (node._tag === 'While') {
      rows.push({
        key: `${path}-while-${span.start}`,
        depth,
        label: `while loop${node.loop.ordinal}`,
        detail: `r${node.region.ordinal}${node.parent === undefined ? '' : ` · parent loop${node.parent.ordinal}`}`,
        span,
        onActivate: () => onPick(span),
      })
      expression(node.condition, depth + 1, `${path}.c`)
      node.body.forEach((inner, index) => statement(inner, depth + 1, `${path}.b${index}`))
      return
    }
    if (node._tag === 'Break' || node._tag === 'Continue') {
      rows.push({
        key: `${path}-${node._tag.toLowerCase()}-${span.start}`,
        depth,
        label: `${node._tag.toLowerCase()} loop${node.target.ordinal}`,
        detail: `r${node.region.ordinal}`,
        span,
        onActivate: () => onPick(span),
      })
      return
    }
    if (node._tag === 'Fail') {
      rows.push({
        key: `${path}-fail-${span.start}`,
        depth,
        label: `fail ${hirTypeText(node.failure)}`,
        detail: `r${node.region.ordinal}`,
        span,
        tone: 'warning',
        onActivate: () => onPick(span),
      })
      expression(node.expression, depth + 1, `${path}.f`)
      return
    }
    if (node._tag === 'Unsafe') {
      rows.push({
        key: `${path}-unsafe-${span.start}`,
        depth,
        label: 'unsafe',
        detail: `r${node.region.ordinal}`,
        span,
        onActivate: () => onPick(span),
      })
      node.statements.forEach((inner, index) => statement(inner, depth + 1, `${path}.u${index}`))
      return
    }
    if (node._tag === 'UnavailableStatement') {
      rows.push({
        key: `${path}-unavailable-${span.start}`,
        depth,
        label: 'unavailable statement',
        detail: `r${node.region.ordinal}`,
        span,
        tone: 'warning',
        onActivate: () => onPick(span),
      })
      return
    }
    rows.push({
      key: `${path}-return-${span.start}`,
      depth,
      label: 'return',
      detail: `r${node.region.ordinal}`,
      span,
      onActivate: () => onPick(span),
    })
    expression(node.expression, depth + 1, `${path}.r`)
  }

  hir.functions.forEach((fn, index) => {
    const span = hirSpan(fn.declaration.syntax.span)
    rows.push({
      key: `fn-${index}`,
      label: `fn#${fn.declaration.id.ordinal} ${hirIdentity(fn.declaration)}`,
      detail: hirContract(fn.contract),
      span,
      head: true,
      onActivate: () => onPick(span),
    })
    fn.statements.forEach((node, statementIndex) => statement(node, 1, `fn${index}.${statementIndex}`))
  })

  return rows
}

/**
 * Every diagnostic this compiler publishes is an error — `Diagnostic.severity` is the literal
 * `'error'`. The warning tone stays in the row grammar for recovered tokens, which are the tree's
 * own amber state rather than a diagnostic.
 */
const severityTone = (severity: Diagnostic.Diagnostic['severity']): RowTone =>
  severity === 'error' ? 'error' : 'warning'

/**
 * The value-flow model as rows: each call group, then its nodes and the edges between them.
 *
 * This is the one view that is about *relationships* rather than about a tree, so the edges are
 * rendered as their own rows (`argument #0 → parameter value`) rather than being folded into the
 * node they start from — an edge is a fact the elaborator produced, and it can be unavailable on
 * its own.
 */
export const flowRows = (
  flow: FlowModel,
  onPick: (span: Span) => void,
): ReadonlyArray<RowModel> => {
  if (flow.status === 'Empty') return []

  const stateTone = (state: string): RowTone | undefined =>
    state === 'Unavailable' ? 'warning' : undefined

  return flow.groups.flatMap((group) => {
    const groupSpan: Span = { start: group.span.start, end: group.span.end }
    const rows: Array<RowModel> = [
      {
        key: `flow-${group.id}`,
        depth: Math.min(group.depth, 3),
        label: group.label,
        detail: `${group.state} · ${group.detail}${
          group.evaluation === undefined ? '' : ` · evaluated #${group.evaluation.order}`
        }`,
        span: groupSpan,
        head: true,
        ...(stateTone(group.state) === undefined
          ? {}
          : { tone: stateTone(group.state) as RowTone }),
        onActivate: () => onPick(groupSpan),
      },
    ]

    for (const node of flow.nodes.filter((candidate) => candidate.groupId === group.id)) {
      const span: Span = { start: node.span.start, end: node.span.end }
      rows.push({
        key: `flow-${node.id}`,
        depth: Math.min(group.depth, 3) + 1,
        dot: node.evaluation === undefined ? undefined : 'ok',
        label: node.label,
        detail: `${node.kind} · ${node.detail}${
          node.evaluation?.value === undefined ? '' : ` · value ${node.evaluation.value}`
        }`,
        span,
        ...(stateTone(node.state) === undefined ? {} : { tone: stateTone(node.state) as RowTone }),
        onActivate: () => onPick(span),
      })
    }

    for (const edge of flow.edges.filter((candidate) => candidate.groupId === group.id)) {
      const span: Span = { start: edge.span.start, end: edge.span.end }
      rows.push({
        key: `flow-${edge.id}`,
        depth: Math.min(group.depth, 3) + 1,
        label: `→ ${edge.label}`,
        detail: edge.state,
        span,
        ...(stateTone(edge.state) === undefined ? {} : { tone: stateTone(edge.state) as RowTone }),
        onActivate: () => onPick(span),
      })
    }

    return rows
  })
}

/**
 * A diagnostic is two rows, not a card: the message, then its recovery or cause underneath.
 *
 * Severity is carried by the row's left rule rather than by a badge, so a pane full of
 * diagnostics reads as a column of coloured rules — countable at a glance.
 */
export const diagnosticRows = (
  entries: ReadonlyArray<DiagnosticEntry>,
  onPick: (span: Span) => void,
): ReadonlyArray<RowModel> => {
  if (entries.length === 0) {
    return [
      {
        key: 'clean',
        dot: 'ok',
        label: 'no diagnostics',
        detail: 'the program compiles clean for this target and profile',
        tone: 'ok',
      },
    ]
  }

  return entries.flatMap((entry, index) => {
    const diagnostic = entry.diagnostic
    const span: Span = { start: diagnostic.span.start, end: diagnostic.span.end }
    const tone = severityTone(diagnostic.severity)
    const rows: Array<RowModel> = [
      {
        key: `dx-${index}`,
        dot: tone,
        label: diagnostic.code,
        detail: diagnostic.message,
        span,
        tone,
        onActivate: () => onPick(span),
      },
    ]

    const cause = diagnostic.cause
    if (cause !== undefined) {
      const origin = entries.find((candidate) =>
        Diagnostic.identityEquals(candidate.identity, cause),
      )
      rows.push({
        key: `dx-${index}-cause`,
        depth: 1,
        label: `caused by ${cause.code}`,
        detail: origin?.diagnostic.message ?? diagnostic.phase,
        span: { start: cause.span.start, end: cause.span.end },
        tone,
        onActivate: () => onPick({ start: cause.span.start, end: cause.span.end }),
      })
    }

    return rows
  })
}

/** Counts for the app bar and status bar. */
export const diagnosticCounts = (
  entries: ReadonlyArray<DiagnosticEntry>,
): { readonly errors: number; readonly warnings: number } => ({
  errors: entries.filter((entry) => entry.diagnostic.severity === 'error').length,
  warnings: entries.filter((entry) => entry.diagnostic.severity !== 'error').length,
})

export const elaborationSummary = (analysis: Elaboration.Result): string =>
  `${analysis.functions.length} fn`
