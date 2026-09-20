import * as AuthoredIdentity from './AuthoredIdentity.js'
import * as Elaboration from './Elaboration.js'
import * as SemanticContext from './SemanticContext.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as Tir from './Tir.js'
import * as Type from './Type.js'

const typeText = (type: Type.Type): string => Type.encode(type)

export type FlowItemState = 'Connected' | 'Stopped' | 'Branched' | 'Unmatched'

export type FlowNodeKind =
  | 'Argument'
  | 'Parameter'
  | 'Reference'
  | 'ReturnedValue'
  | 'CallResult'
  | 'FunctionReturn'
  | 'Terminal'

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
  readonly state: FlowItemState
  readonly span: SourceSpan.SourceSpan
}

export interface FlowEdge {
  readonly _tag: 'FlowEdge'
  readonly id: string
  readonly groupId: string
  readonly from: string
  readonly to: string
  readonly label: string
  readonly depth: number
  readonly state: FlowItemState
  readonly span: SourceSpan.SourceSpan
}

export interface FlowModel {
  readonly _tag: 'FlowModel'
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
  parameter.name._tag === 'Present' ? parameter.name.spelling : `parameter #${parameter.id.ordinal}`

const callName = (call: CallFact): string =>
  call.reference._tag === 'Unavailable' ? 'unavailable call' : call.reference.spelling

const directReference = (
  expression: Elaboration.ExpressionFact | Tir.Expression,
): Elaboration.IdentifierExpressionFact['reference'] | undefined => {
  if ('origin' in expression)
    return expression._tag === 'Move' ? directReference(expression.subject) : undefined
  switch (expression._tag) {
    case 'Identifier':
      return expression.reference
    case 'Move':
      return directReference(expression.subject)
    case 'Borrow':
      return directReference(expression.subject)
    default:
      return undefined
  }
}

const argumentLabel = (expression: Tir.Expression): string => {
  if (expression._tag === 'ParameterReference') return `parameter #${expression.parameter.ordinal}`
  if (expression._tag === 'BindingReference') return `binding #${expression.binding.ordinal}`
  if (expression._tag === 'PatternBindingReference')
    return `pattern binding #${expression.binding.ordinal}`
  if (expression._tag === 'Call') return `${expression.target.name}(…)`
  if (expression._tag === 'Move') {
    return `move ${argumentLabel(expression.subject)}`
  }
  if (expression._tag === 'BooleanLiteral') return String(expression.value)
  if (expression._tag === 'BuiltinCall') return `${expression.operation} expression`
  if (expression._tag === 'CallableApply') return 'callable result'
  if (expression._tag === 'Construct') return `${typeText(expression.type)} {…}`
  if (expression._tag === 'Project')
    return `${argumentLabel(expression.subject)}.${expression.field.ordinal}`
  if (expression._tag === 'ArrayConstruct') return `[${expression.elements.length} elements]`
  if (expression._tag === 'IndexPlace') return `${argumentLabel(expression.subject)}[index]`
  if (expression._tag === 'Match') return 'match result'
  if (expression._tag === 'ValueBorrow' || expression._tag === 'SliceBorrow')
    return `${expression.access === 'Exclusive' ? '&mut ' : '&'}borrowed value`
  if (expression._tag === 'Run') return 'run result'
  if (expression._tag === 'IntegerLiteral') return String(expression.value)
  return 'unavailable expression'
}

const callId = (call: CallFact): string => `call-${AuthoredIdentity.anchorKey(call.anchor)}`

const sameDeclaration = (
  left: Elaboration.DeclarationFact,
  right: Elaboration.DeclarationFact,
): boolean => left.id.sourceId === right.id.sourceId && left.id.ordinal === right.id.ordinal

const groupState = (call: CallFact): FlowItemState => {
  if (call.reference._tag === 'Ambiguous') return 'Branched'
  if (
    (call.reference._tag !== 'Resolved' && call.reference._tag !== 'ResolvedBuiltin') ||
    call.contract._tag !== 'Compatible'
  ) {
    return 'Stopped'
  }
  return 'Connected'
}

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
    state,
    span,
  })

const functionFor = (
  analysis: Elaboration.Result,
  declaration: Elaboration.DeclarationFact,
): Elaboration.FunctionFact | undefined =>
  Elaboration.records(analysis).functions.find((fact) =>
    sameDeclaration(fact.declaration, declaration),
  )

const projectCall = (
  analysis: Elaboration.Result,
  context: SemanticContext.SemanticContext,
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
    detail: `${declarationName(caller)} call at [${context.spanOf(call.anchor).start}, ${context.spanOf(call.anchor).end})`,
    depth,
    ordinal,
    parentId,
    state: groupState(call),
    span: context.spanOf(call.anchor),
    nodeIds: [],
    edgeIds: [],
    target: groupTarget,
  }
  draft.groups.push(group)

  const nestedResults = new Map<number, string>()
  const nestedCompleteness = new Map<number, boolean>()
  // Nested published calls are projected from TIR once this model no longer depends on records.

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
        `Argument #${argument.id.ordinal}: ${argumentLabel(argument.expression)}`,
        argument.type._tag === 'Available' ? typeText(argument.type.type) : 'Unavailable type',
        call.contract._tag === 'Compatible' && nestedComplete !== false ? 'Connected' : 'Unmatched',
        context.spanOf(argument.anchor),
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
          context.spanOf(argument.anchor),
        ),
      )
    }
  }

  if (call.reference._tag !== 'Resolved') {
    const referenceSpan = context.spanOf(call.reference.anchor)
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
            context.spanOf(declaration.anchor),
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
          context.spanOf(call.anchor),
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
          context.spanOf(call.anchor),
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
        context.spanOf(parameter.anchor),
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
        context.spanOf(mapping.argument.anchor),
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
        context.spanOf(call.anchor),
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
          context.spanOf(call.anchor),
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
        context.spanOf(call.anchor),
      ),
    )
    return Object.freeze({ groupId: id, resultId: undefined, complete: false })
  }

  const targetFact = functionFor(analysis, target)
  const returned = targetFact?.returnedExpression
  const returnedReference = returned === undefined ? undefined : directReference(returned)
  if (returned === undefined || (returned._tag !== 'Integer' && returnedReference === undefined)) {
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
        context.spanOf(target.anchor),
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
        context.spanOf(returned.anchor),
      ),
    )
    if (returned.integer._tag !== 'Available') {
      return Object.freeze({ groupId: id, resultId: undefined, complete: false })
    }
  } else if (returnedReference === undefined) {
    return Object.freeze({ groupId: id, resultId: undefined, complete: false })
  } else {
    const referenceLabel =
      returnedReference._tag === 'Unavailable'
        ? 'Unavailable reference'
        : returnedReference.spelling
    let returnedState: FlowItemState = 'Stopped'
    if (returnedReference._tag === 'Resolved') returnedState = 'Connected'
    else if (returnedReference._tag === 'Ambiguous') returnedState = 'Branched'
    addNode(
      draft,
      group,
      semanticNode(
        group,
        returnedId,
        'Reference',
        `Returned reference: ${referenceLabel}`,
        returnedReference._tag,
        returnedState,
        context.spanOf(returned.anchor),
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
            context.spanOf(returned.anchor),
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
          context.spanOf(returned.anchor),
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
      context.spanOf(call.anchor),
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
      context.spanOf(returned.anchor),
    ),
  )
  return Object.freeze({ groupId: id, resultId, complete: true })
}

const emptyModel = (): FlowModel =>
  Object.freeze({
    _tag: 'FlowModel',
    status: 'Empty',
    summary: 'No call expression is available for data-flow projection.',
    groups: Object.freeze([]),
    nodes: Object.freeze([]),
    edges: Object.freeze([]),
  })

/** Projects semantic relationships for the inspector. */
export const projectDataFlow = (analysis: Elaboration.Result): FlowModel => {
  const context = SemanticContext.make(analysis.authored)
  const caller = Elaboration.records(analysis).functions.find(
    (fact) => fact.returnedExpression._tag === 'Call',
  )
  if (caller === undefined || caller.returnedExpression._tag !== 'Call') {
    return emptyModel()
  }

  const draft: ProjectionDraft = { groups: [], nodes: [], edges: [] }
  const root = projectCall(
    analysis,
    context,
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
          context.spanOf(caller.returnedExpression.anchor),
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
          context.spanOf(caller.returnedExpression.anchor),
        ),
      )
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
    }),
  )
  const nodes = draft.nodes.map((item): FlowNode => Object.freeze(item))
  const edges = draft.edges.map((item): FlowEdge => Object.freeze(item))
  const nestedCount = Math.max(0, groups.length - 1)
  const modeSummary = 'Semantic relationships only.'
  return Object.freeze({
    _tag: 'FlowModel',
    status: root.complete ? 'Complete' : 'Incomplete',
    summary: `${nestedCount === 0 ? 'One call site' : `${nestedCount + 1} nested call sites`} projected. ${modeSummary}`,
    groups: Object.freeze(groups),
    nodes: Object.freeze(nodes),
    edges: Object.freeze(edges),
  })
}
