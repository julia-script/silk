import type { BootstrapEvaluation, Elaboration, SourceSpan, Type } from '@silk-effect/compiler'

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

export type FlowItemState = 'Connected' | 'Stopped' | 'Branched' | 'Unmatched'
export type FlowLayer = 'Semantic' | 'Evaluated'

export type FlowNodeKind =
  | 'Argument'
  | 'Parameter'
  | 'Reference'
  | 'ReturnedValue'
  | 'CallResult'
  | 'FunctionReturn'
  | 'Terminal'

export interface FlowEvidence {
  readonly order: number
  readonly value?: number | bigint
}

export interface FlowGroup {
  readonly _tag: 'FlowGroup'
  readonly id: string
  readonly label: string
  readonly detail: string
  readonly depth: number
  readonly ordinal: number
  readonly parentId: string | undefined
  readonly state: FlowItemState
  readonly span: SourceSpan.SourceSpan
  readonly nodeIds: ReadonlyArray<string>
  readonly edgeIds: ReadonlyArray<string>
  readonly evaluation?: FlowEvidence
}

export interface FlowNode {
  readonly _tag: 'FlowNode'
  readonly id: string
  readonly groupId: string
  readonly kind: FlowNodeKind
  readonly label: string
  readonly detail: string
  readonly depth: number
  readonly ordinal: number | undefined
  readonly layer: FlowLayer
  readonly state: FlowItemState
  readonly span: SourceSpan.SourceSpan
  readonly evaluation?: FlowEvidence
}

export interface FlowEdge {
  readonly _tag: 'FlowEdge'
  readonly id: string
  readonly groupId: string
  readonly from: string
  readonly to: string
  readonly label: string
  readonly depth: number
  readonly layer: FlowLayer
  readonly state: FlowItemState
  readonly span: SourceSpan.SourceSpan
  readonly evaluation?: FlowEvidence
}

export interface FlowModel {
  readonly _tag: 'FlowModel'
  readonly mode: 'Semantic' | 'Evaluated'
  readonly status: 'Complete' | 'Incomplete' | 'Empty'
  readonly summary: string
  readonly groups: ReadonlyArray<FlowGroup>
  readonly nodes: ReadonlyArray<FlowNode>
  readonly edges: ReadonlyArray<FlowEdge>
}

type CallFact = Extract<Elaboration.ExpressionFact, { readonly _tag: 'Call' }>

interface GroupDraft {
  readonly id: string
  readonly label: string
  readonly detail: string
  readonly depth: number
  readonly ordinal: number
  readonly parentId: string | undefined
  readonly state: FlowItemState
  readonly span: SourceSpan.SourceSpan
  readonly nodeIds: Array<string>
  readonly edgeIds: Array<string>
  readonly target: Elaboration.DeclarationFact | undefined
}

interface ProjectionDraft {
  readonly groups: Array<GroupDraft>
  readonly nodes: Array<FlowNode>
  readonly edges: Array<FlowEdge>
}

interface CallProjection {
  readonly groupId: string
  readonly resultId: string | undefined
  readonly complete: boolean
}

const declarationName = (declaration: Elaboration.DeclarationFact): string =>
  declaration.name._tag === 'Present'
    ? declaration.name.spelling
    : `function #${declaration.id.ordinal}`

const parameterName = (parameter: Elaboration.ParameterFact): string =>
  parameter.name._tag === 'Present'
    ? parameter.name.spelling
    : `parameter #${parameter.id.ordinal}`

const callName = (call: CallFact): string =>
  call.reference._tag === 'Unavailable' ? 'unavailable call' : call.reference.spelling

const directReference = (
  expression: Elaboration.ExpressionFact,
): Elaboration.IdentifierExpressionFact['reference'] | undefined => {
  switch (expression._tag) {
    case 'Identifier':
      return expression.reference
    case 'Move':
      return directReference(expression.subject)
    case 'Grouped':
      return directReference(expression.expression)
    case 'Borrow':
      return directReference(expression.subject)
    default:
      return undefined
  }
}

const argumentLabel = (argument: Elaboration.ArgumentFact): string => {
  const expression = argument.expression
  if (expression._tag === 'Identifier') {
    return expression.reference._tag === 'Unavailable'
      ? 'unavailable reference'
      : expression.reference.spelling
  }
  if (expression._tag === 'Call') return `${callName(expression)}(…)`
  if (expression._tag === 'Move') {
    return `move ${argumentLabel({ ...argument, expression: expression.subject })}`
  }
  if (expression._tag === 'Boolean') return String(expression.value)
  if (expression._tag === 'Grouped')
    return `(${argumentLabel({ ...argument, expression: expression.expression })})`
  if (expression._tag === 'Operator') return `${expression.operator} expression`
  if (expression._tag === 'CallableApply') return 'callable result'
  if (expression._tag === 'StructLiteral')
    return expression.target._tag === 'Resolved'
      ? `${typeText(expression.target.type)} {…}`
      : 'unavailable struct literal'
  if (expression._tag === 'FieldProjection')
    return `${argumentLabel({ ...argument, expression: expression.subject })}.${expression.fieldName ?? '?'}`
  if (expression._tag === 'ArrayLiteral') return `[${expression.elements.length} elements]`
  if (expression._tag === 'IndexProjection')
    return `${argumentLabel({ ...argument, expression: expression.subject })}[index]`
  if (expression._tag === 'Match') return 'match result'
  if (expression._tag === 'Borrow') {
    return `${expression.access === 'Exclusive' ? '&mut ' : '&'}${argumentLabel({ ...argument, expression: expression.subject })}`
  }
  if (expression._tag === 'Run') return 'run result'
  if (expression._tag !== 'Integer') return 'unavailable expression'
  if (expression.integer._tag === 'Available') return String(expression.integer.value)
  if (expression.integer._tag === 'OutOfRange') return expression.integer.spelling
  return 'unavailable integer'
}

const callId = (call: CallFact): string =>
  `call-${call.syntax.span.start}-${call.syntax.span.end}`

const sameSpan = (left: SourceSpan.SourceSpan, right: SourceSpan.SourceSpan): boolean =>
  left.sourceId === right.sourceId && left.start === right.start && left.end === right.end

const sameDeclaration = (
  left: Elaboration.DeclarationFact,
  right: Elaboration.DeclarationFact,
): boolean => left.id.sourceId === right.id.sourceId && left.id.ordinal === right.id.ordinal

const groupState = (call: CallFact): FlowItemState =>
  call.reference._tag === 'Ambiguous'
    ? 'Branched'
    : (call.reference._tag !== 'Resolved' && call.reference._tag !== 'ResolvedBuiltin') ||
        call.contract._tag !== 'Compatible'
      ? 'Stopped'
      : 'Connected'

const addNode = (draft: ProjectionDraft, group: GroupDraft, value: FlowNode): void => {
  draft.nodes.push(Object.freeze(value))
  group.nodeIds.push(value.id)
}

const addEdge = (draft: ProjectionDraft, group: GroupDraft, value: FlowEdge): void => {
  draft.edges.push(Object.freeze(value))
  group.edgeIds.push(value.id)
}

const semanticNode = (
  group: GroupDraft,
  id: string,
  kind: FlowNodeKind,
  label: string,
  detail: string,
  state: FlowItemState,
  span: SourceSpan.SourceSpan,
  ordinal?: number,
): FlowNode =>
  Object.freeze({
    _tag: 'FlowNode',
    id,
    groupId: group.id,
    kind,
    label,
    detail,
    depth: group.depth,
    ordinal,
    layer: 'Semantic',
    state,
    span,
  })

const semanticEdge = (
  group: GroupDraft,
  id: string,
  from: string,
  to: string,
  label: string,
  state: FlowItemState,
  span: SourceSpan.SourceSpan,
): FlowEdge =>
  Object.freeze({
    _tag: 'FlowEdge',
    id,
    groupId: group.id,
    from,
    to,
    label,
    depth: group.depth,
    layer: 'Semantic',
    state,
    span,
  })

const functionFor = (
  analysis: Elaboration.Result,
  declaration: Elaboration.DeclarationFact,
): Elaboration.FunctionFact | undefined =>
  analysis.functions.find((fact) => sameDeclaration(fact.declaration, declaration))

const projectCall = (
  analysis: Elaboration.Result,
  draft: ProjectionDraft,
  caller: Elaboration.DeclarationFact,
  call: CallFact,
  parentId: string | undefined,
  depth: number,
  ordinal: number,
): CallProjection => {
  const id = callId(call)
  const groupTarget = call.reference._tag === 'Resolved' ? call.reference.declaration : undefined
  const group: GroupDraft = {
    id,
    label: `${callName(call)} call site`,
    detail: `${declarationName(caller)} call at [${call.syntax.span.start}, ${call.syntax.span.end})`,
    depth,
    ordinal,
    parentId,
    state: groupState(call),
    span: call.syntax.span,
    nodeIds: [],
    edgeIds: [],
    target: groupTarget,
  }
  draft.groups.push(group)

  const nestedResults = new Map<number, string>()
  const nestedCompleteness = new Map<number, boolean>()
  for (const argument of call.arguments) {
    if (argument.expression._tag !== 'Call') continue
    const nested = projectCall(
      analysis,
      draft,
      caller,
      argument.expression,
      id,
      depth + 1,
      argument.id.ordinal,
    )
    if (nested.resultId !== undefined) nestedResults.set(argument.id.ordinal, nested.resultId)
    nestedCompleteness.set(argument.id.ordinal, nested.complete)
  }

  for (const argument of call.arguments) {
    const argumentId = `${id}-argument-${argument.id.ordinal}`
    const nestedComplete = nestedCompleteness.get(argument.id.ordinal)
    addNode(
      draft,
      group,
      semanticNode(
        group,
        argumentId,
        'Argument',
        `Argument #${argument.id.ordinal}: ${argumentLabel(argument)}`,
        argument.type._tag === 'Available' ? typeText(argument.type.type) : 'Unavailable type',
        call.contract._tag === 'Compatible' && nestedComplete !== false ? 'Connected' : 'Unmatched',
        argument.syntax.span,
        argument.id.ordinal,
      ),
    )
    const nestedResult = nestedResults.get(argument.id.ordinal)
    if (nestedResult !== undefined) {
      addEdge(
        draft,
        group,
        semanticEdge(
          group,
          `${id}-nested-result-${argument.id.ordinal}`,
          nestedResult,
          argumentId,
          'supplies nested result to',
          'Connected',
          argument.syntax.span,
        ),
      )
    }
  }

  if (call.reference._tag !== 'Resolved') {
    const referenceSpan =
      call.reference._tag === 'Unavailable'
        ? call.reference.syntax.span
        : call.reference.token.span
    const referenceId = `${id}-reference`
    addNode(
      draft,
      group,
      semanticNode(
        group,
        referenceId,
        'Reference',
        call.reference._tag === 'Unavailable'
          ? 'Unavailable call target'
          : `Call target: ${call.reference.spelling}`,
        call.reference._tag,
        call.reference._tag === 'Ambiguous' ? 'Branched' : 'Stopped',
        referenceSpan,
      ),
    )
    if (call.reference._tag === 'Ambiguous') {
      for (const declaration of call.reference.declarations) {
        const candidateId = `${id}-candidate-${declaration.id.ordinal}`
        addNode(
          draft,
          group,
          semanticNode(
            group,
            candidateId,
            'Terminal',
            `Candidate ${declarationName(declaration)}`,
            `function #${declaration.id.ordinal}`,
            'Branched',
            declaration.syntax.span,
          ),
        )
        addEdge(
          draft,
          group,
          semanticEdge(
            group,
            `${referenceId}-${candidateId}`,
            referenceId,
            candidateId,
            'could target',
            'Branched',
            referenceSpan,
          ),
        )
      }
    } else {
      const terminalId = `${id}-target-stop`
      addNode(
        draft,
        group,
        semanticNode(
          group,
          terminalId,
          'Terminal',
          'Data flow stops: no unique target',
          call.contract._tag === 'Unavailable' ? call.contract.reason._tag : call.reference._tag,
          'Stopped',
          call.syntax.span,
        ),
      )
      addEdge(
        draft,
        group,
        semanticEdge(
          group,
          `${referenceId}-stop`,
          referenceId,
          terminalId,
          'stops at',
          'Stopped',
          call.syntax.span,
        ),
      )
    }
    return Object.freeze({ groupId: id, resultId: undefined, complete: false })
  }

  const target = call.reference.declaration

  for (const parameter of target.parameters) {
    addNode(
      draft,
      group,
      semanticNode(
        group,
        `${id}-parameter-${parameter.id.ordinal}`,
        'Parameter',
        `${declarationName(target)}.${parameterName(parameter)}`,
        `parameter #${parameter.id.ordinal}`,
        call.mappings.some((mapping) => mapping.parameter === parameter)
          ? 'Connected'
          : 'Unmatched',
        parameter.syntax.span,
        parameter.id.ordinal,
      ),
    )
  }
  for (const mapping of call.mappings) {
    addEdge(
      draft,
      group,
      semanticEdge(
        group,
        `${id}-mapping-${mapping.argument.id.ordinal}`,
        `${id}-argument-${mapping.argument.id.ordinal}`,
        `${id}-parameter-${mapping.parameter.id.ordinal}`,
        'binds positionally to',
        'Connected',
        mapping.argument.syntax.span,
      ),
    )
  }

  if (call.contract._tag !== 'Compatible') {
    const terminalId = `${id}-contract-stop`
    addNode(
      draft,
      group,
      semanticNode(
        group,
        terminalId,
        'Terminal',
        `Data flow stops: ${call.contract._tag}`,
        call.contract._tag === 'ArityMismatch'
          ? `${call.contract.actualCount} actual / ${call.contract.expectedCount} expected`
          : call.contract.reason._tag,
        'Stopped',
        call.syntax.span,
      ),
    )
    const origins =
      call.mappings.length === 0
        ? call.arguments.map((argument) => `${id}-argument-${argument.id.ordinal}`)
        : call.mappings.map((mapping) => `${id}-parameter-${mapping.parameter.id.ordinal}`)
    for (const origin of origins) {
      addEdge(
        draft,
        group,
        semanticEdge(
          group,
          `${origin}-contract-stop`,
          origin,
          terminalId,
          'contract stops at',
          'Stopped',
          call.syntax.span,
        ),
      )
    }
    return Object.freeze({ groupId: id, resultId: undefined, complete: false })
  }

  if (Array.from(nestedCompleteness.values()).some((complete) => !complete)) {
    const terminalId = `${id}-nested-stop`
    addNode(
      draft,
      group,
      semanticNode(
        group,
        terminalId,
        'Terminal',
        'Data flow stops: nested argument has no result',
        'The enclosing semantic result is not drawn.',
        'Stopped',
        call.syntax.span,
      ),
    )
    return Object.freeze({ groupId: id, resultId: undefined, complete: false })
  }

  const targetFact = functionFor(analysis, target)
  const returned = targetFact?.returnedExpression
  const returnedReference = returned === undefined ? undefined : directReference(returned)
  if (
    returned === undefined ||
    (returned._tag !== 'Integer' && returnedReference === undefined)
  ) {
    const terminalId = `${id}-return-stop`
    addNode(
      draft,
      group,
      semanticNode(
        group,
        terminalId,
        'Terminal',
        'Data flow stops: target return path is not directly available',
        returned === undefined ? 'No function fact' : `${returned._tag} target return`,
        'Stopped',
        target.syntax.span,
      ),
    )
    return Object.freeze({ groupId: id, resultId: undefined, complete: false })
  }

  const returnedId = `${id}-target-returned`
  if (returned._tag === 'Integer') {
    addNode(
      draft,
      group,
      semanticNode(
        group,
        returnedId,
        'ReturnedValue',
        returned.integer._tag === 'Available'
          ? `Returned literal: ${returned.integer.value}`
          : 'Unavailable returned literal',
        returned.integer._tag,
        returned.integer._tag === 'Available' ? 'Connected' : 'Stopped',
        returned.syntax.span,
      ),
    )
    if (returned.integer._tag !== 'Available') {
      return Object.freeze({ groupId: id, resultId: undefined, complete: false })
    }
  } else {
    if (returnedReference === undefined) {
      return Object.freeze({ groupId: id, resultId: undefined, complete: false })
    }
    const referenceLabel =
      returnedReference._tag === 'Unavailable'
        ? 'Unavailable reference'
        : returnedReference.spelling
    addNode(
      draft,
      group,
      semanticNode(
        group,
        returnedId,
        'Reference',
        `Returned reference: ${referenceLabel}`,
        returnedReference._tag,
        returnedReference._tag === 'Resolved'
          ? 'Connected'
          : returnedReference._tag === 'Ambiguous'
            ? 'Branched'
            : 'Stopped',
        returned.syntax.span,
      ),
    )
    if (returnedReference._tag === 'Ambiguous') {
      for (const parameter of returnedReference.parameters) {
        addEdge(
          draft,
          group,
          semanticEdge(
            group,
            `${id}-parameter-${parameter.id.ordinal}-branch`,
            `${id}-parameter-${parameter.id.ordinal}`,
            returnedId,
            'could be read by',
            'Branched',
            returned.syntax.span,
          ),
        )
      }
    } else if (returnedReference._tag === 'Resolved') {
      addEdge(
        draft,
        group,
        semanticEdge(
          group,
          `${id}-parameter-reference`,
          `${id}-parameter-${returnedReference.parameter.id.ordinal}`,
          returnedId,
          'is read by',
          'Connected',
          returned.syntax.span,
        ),
      )
    }
    if (returnedReference._tag !== 'Resolved') {
      return Object.freeze({ groupId: id, resultId: undefined, complete: false })
    }
  }

  const resultId = `${id}-result`
  addNode(
    draft,
    group,
    semanticNode(
      group,
      resultId,
      'CallResult',
      `${declarationName(target)} call result`,
      call.type._tag === 'Available' ? typeText(call.type.type) : 'Unavailable type',
      'Connected',
      call.syntax.span,
    ),
  )
  addEdge(
    draft,
    group,
    semanticEdge(
      group,
      `${id}-returned-result`,
      returnedId,
      resultId,
      'produces',
      'Connected',
      returned.syntax.span,
    ),
  )
  return Object.freeze({ groupId: id, resultId, complete: true })
}

const blockedLabel = (reason: BootstrapEvaluation.BlockedReason): string => {
  switch (reason._tag) {
    case 'InvalidMir':
      return `InvalidMir: ${reason.violations.length} violations`
    case 'EvaluationLimit':
      return `EvaluationLimit(${reason.kind}): ${reason.count}/${reason.limit} in ${reason.function.name}; active ${reason.activeFrames.map((frame) => `f${frame.frame}:d${frame.depth} ${frame.function.name}`).join(' → ')}`
    case 'Trap':
      return `Trap: ${reason.reason}`
    case 'UnavailableEntry':
      return `UnavailableEntry: ${reason.reason}`
    case 'MissingFunction':
      return `MissingFunction: ${reason.target.name}`
    case 'InvalidCallableReuse':
      return `InvalidCallableReuse: callable #${reason.ticket} is ${reason.state.toLowerCase()}`
    case 'MissingStandardStreams':
      return 'MissingStandardStreams: no host provider was supplied'
    case 'MissingStandardInput':
      return 'MissingStandardInput: no host provider was supplied'
    case 'MissingHostInput':
      return 'MissingHostInput: no host provider was supplied'
    case 'MissingOsFileSystemHost':
      return 'MissingOsFileSystemHost: no OS filesystem host provider was supplied'
    case 'IntrinsicTargetUnavailable':
      return `IntrinsicTargetUnavailable: ${reason.diagnostics.map((diagnostic) => diagnostic.message).join('; ')}`
  }
}

const blockedSpan = (
  reason: BootstrapEvaluation.BlockedReason,
): SourceSpan.SourceSpan | undefined => {
  switch (reason._tag) {
    case 'InvalidMir':
      return undefined
    case 'UnavailableEntry':
      return undefined
    case 'Trap':
      return reason.span
    case 'MissingFunction':
      return reason.span
    case 'InvalidCallableReuse':
      return reason.span
    case 'MissingStandardStreams':
      return undefined
    case 'MissingStandardInput':
      return undefined
    case 'MissingHostInput':
      return undefined
    case 'MissingOsFileSystemHost':
      return undefined
    case 'EvaluationLimit':
      return reason.span
    case 'IntrinsicTargetUnavailable':
      return reason.diagnostics.at(0)?.span
  }
}

interface Overlay {
  readonly groups: Map<string, FlowEvidence>
  readonly items: Map<string, FlowEvidence>
}

const traceOverlay = (draft: ProjectionDraft, outcome: BootstrapEvaluation.Outcome): Overlay => {
  const groups = new Map<string, FlowEvidence>()
  const items = new Map<string, FlowEvidence>()
  const stack: Array<GroupDraft> = []

  const targetMatches = (
    target: Elaboration.DeclarationFact,
    id: { readonly module: string; readonly name: string },
  ): boolean =>
    target.canonical._tag === 'Canonical' &&
    target.canonical.id.module === id.module &&
    target.canonical.id.name === id.name

  for (const [index, event] of outcome.trace.entries()) {
    const eventValue =
      event._tag === 'Binding' || event._tag === 'Return' ? event.value : undefined
    const evidence = Object.freeze({
      order: index + 1,
      ...(eventValue?._tag === 'I32Value' || eventValue?._tag === 'UsizeValue'
        ? { value: eventValue.value }
        : {}),
    })
    if (event._tag === 'Call') {
      const group = draft.groups.find((candidate) => sameSpan(candidate.span, event.span))
      if (group !== undefined) {
        groups.set(group.id, evidence)
        stack.push(group)
      }
      continue
    }
    if (event._tag === 'Binding') {
      const group = draft.groups.find((candidate) => sameSpan(candidate.span, event.callSpan))
      if (group !== undefined) {
        items.set(`${group.id}-argument-${event.argumentOrdinal}`, evidence)
        items.set(`${group.id}-parameter-${event.parameterOrdinal}`, evidence)
        items.set(`${group.id}-mapping-${event.argumentOrdinal}`, evidence)
        if (event.fromCall) {
          items.set(`${group.id}-nested-result-${event.argumentOrdinal}`, evidence)
        }
      }
      continue
    }
    if (event._tag === 'Return') {
      const group = stack.at(-1)
      if (
        group !== undefined &&
        group.target !== undefined &&
        targetMatches(group.target, event.function)
      ) {
        items.set(`${group.id}-target-returned`, evidence)
        items.set(`${group.id}-result`, evidence)
        items.set(`${group.id}-returned-result`, evidence)
        stack.pop()
      }
    }
  }
  return Object.freeze({ groups, items })
}

const emptyModel = (evaluated: boolean): FlowModel =>
  Object.freeze({
    _tag: 'FlowModel',
    mode: evaluated ? 'Evaluated' : 'Semantic',
    status: 'Empty',
    summary: 'No call expression is available for data-flow projection.',
    groups: Object.freeze([]),
    nodes: Object.freeze([]),
    edges: Object.freeze([]),
  })

/** Projects semantic relationships and an optional trace-backed evaluation overlay for the inspector. */
export const projectDataFlow = (
  analysis: Elaboration.Result,
  outcome?: BootstrapEvaluation.Outcome,
): FlowModel => {
  const caller = analysis.functions.find((fact) => fact.returnedExpression._tag === 'Call')
  if (caller === undefined || caller.returnedExpression._tag !== 'Call') {
    return emptyModel(outcome !== undefined)
  }

  const draft: ProjectionDraft = { groups: [], nodes: [], edges: [] }
  const root = projectCall(
    analysis,
    draft,
    caller.declaration,
    caller.returnedExpression,
    undefined,
    0,
    0,
  )
  if (root.resultId !== undefined) {
    const group = draft.groups.find((candidate) => candidate.id === root.groupId)
    if (group !== undefined) {
      const returnId = `${group.id}-caller-return`
      addNode(
        draft,
        group,
        semanticNode(
          group,
          returnId,
          'FunctionReturn',
          `${declarationName(caller.declaration)} return`,
          caller.returnCompatibility._tag,
          'Connected',
          caller.returnedExpression.syntax.span,
        ),
      )
      addEdge(
        draft,
        group,
        semanticEdge(
          group,
          `${group.id}-result-return`,
          root.resultId,
          returnId,
          'is returned by',
          'Connected',
          caller.returnedExpression.syntax.span,
        ),
      )
    }
  }

  const overlay = outcome === undefined ? undefined : traceOverlay(draft, outcome)
  if (outcome?._tag === 'Completed') {
    const order = outcome.trace.length
    const evidence = Object.freeze({ order, value: outcome.result.value })
    overlay?.items.set(`${root.groupId}-caller-return`, evidence)
    overlay?.items.set(`${root.groupId}-result-return`, evidence)
  }

  if (outcome?._tag === 'Blocked') {
    const span = blockedSpan(outcome.reason)
    const group =
      (span === undefined
        ? undefined
        : draft.groups.find((candidate) => sameSpan(candidate.span, span))) ?? draft.groups.at(0)
    if (group !== undefined && span !== undefined) {
      const terminalId = `${group.id}-evaluated-stop`
      const evidence = Object.freeze({ order: outcome.trace.length + 1 })
      draft.nodes.push(
        Object.freeze({
          _tag: 'FlowNode',
          id: terminalId,
          groupId: group.id,
          kind: 'Terminal',
          label: `Evaluation stops: ${outcome.reason._tag}`,
          detail: blockedLabel(outcome.reason),
          depth: group.depth,
          ordinal: undefined,
          layer: 'Evaluated',
          state: 'Stopped',
          span,
          evaluation: evidence,
        }),
      )
      group.nodeIds.push(terminalId)
    }
  }

  const groups = draft.groups.map((group): FlowGroup =>
    Object.freeze({
      _tag: 'FlowGroup',
      id: group.id,
      label: group.label,
      detail: group.detail,
      depth: group.depth,
      ordinal: group.ordinal,
      parentId: group.parentId,
      state: group.state,
      span: group.span,
      nodeIds: Object.freeze([...group.nodeIds]),
      edgeIds: Object.freeze([...group.edgeIds]),
      ...(overlay?.groups.get(group.id) === undefined
        ? {}
        : { evaluation: overlay.groups.get(group.id) }),
    }),
  )
  const nodes = draft.nodes.map((item): FlowNode => {
    const evaluation = item.evaluation ?? overlay?.items.get(item.id)
    return Object.freeze({ ...item, ...(evaluation === undefined ? {} : { evaluation }) })
  })
  const edges = draft.edges.map((item): FlowEdge => {
    const evaluation = item.evaluation ?? overlay?.items.get(item.id)
    return Object.freeze({ ...item, ...(evaluation === undefined ? {} : { evaluation }) })
  })
  const nestedCount = Math.max(0, groups.length - 1)
  const modeSummary =
    outcome === undefined
      ? 'Semantic relationships only; evaluate explicitly to add reachable order and exact values.'
      : outcome._tag === 'Completed'
        ? `Evaluation completed with ${outcome.result.value}; trace-backed order and values are overlaid.`
        : outcome._tag === 'UnhandledFailure'
          ? `Evaluation terminated with ${outcome.report} (tag ${outcome.tag}); its completed trace is overlaid.`
          : `Evaluation stopped at ${outcome.reason._tag}; only its completed trace prefix is overlaid.`
  return Object.freeze({
    _tag: 'FlowModel',
    mode: outcome === undefined ? 'Semantic' : 'Evaluated',
    status: root.complete ? 'Complete' : 'Incomplete',
    summary: `${nestedCount === 0 ? 'One call site' : `${nestedCount + 1} nested call sites`} projected. ${modeSummary}`,
    groups: Object.freeze(groups),
    nodes: Object.freeze(nodes),
    edges: Object.freeze(edges),
  })
}
