import type { SemanticAnalysis, SourceSpan } from '@silk-effect/compiler'

export type FlowItemState = 'Connected' | 'Stopped' | 'Branched' | 'Unmatched'

export type FlowNodeKind =
  | 'Argument'
  | 'Parameter'
  | 'Reference'
  | 'CallResult'
  | 'FunctionReturn'
  | 'Terminal'

export interface FlowNode {
  readonly _tag: 'FlowNode'
  readonly id: string
  readonly kind: FlowNodeKind
  readonly label: string
  readonly detail: string
  readonly state: FlowItemState
  readonly span: SourceSpan.SourceSpan
}

export interface FlowEdge {
  readonly _tag: 'FlowEdge'
  readonly id: string
  readonly from: string
  readonly to: string
  readonly label: string
  readonly state: FlowItemState
  readonly span: SourceSpan.SourceSpan
}

export interface FlowModel {
  readonly _tag: 'FlowModel'
  readonly status: 'Complete' | 'Incomplete' | 'Empty'
  readonly summary: string
  readonly nodes: ReadonlyArray<FlowNode>
  readonly edges: ReadonlyArray<FlowEdge>
}

const node = (
  id: string,
  kind: FlowNodeKind,
  label: string,
  detail: string,
  state: FlowItemState,
  span: SourceSpan.SourceSpan,
): FlowNode => Object.freeze({ _tag: 'FlowNode', id, kind, label, detail, state, span })

const edge = (
  id: string,
  from: string,
  to: string,
  label: string,
  state: FlowItemState,
  span: SourceSpan.SourceSpan,
): FlowEdge => Object.freeze({ _tag: 'FlowEdge', id, from, to, label, state, span })

const declarationName = (declaration: SemanticAnalysis.DeclarationFact): string =>
  declaration.name._tag === 'Present'
    ? declaration.name.spelling
    : `function #${declaration.id.ordinal}`

const parameterName = (parameter: SemanticAnalysis.ParameterFact): string =>
  parameter.name._tag === 'Present'
    ? parameter.name.spelling
    : `parameter #${parameter.id.ordinal}`

const argumentLabel = (argument: SemanticAnalysis.ArgumentFact): string => {
  const expression = argument.expression
  if (expression._tag === 'Identifier') {
    return expression.reference._tag === 'Unavailable'
      ? 'unavailable reference'
      : expression.reference.spelling
  }
  if (expression._tag === 'Call') {
    return expression.reference._tag === 'Unavailable'
      ? 'unavailable call'
      : `${expression.reference.spelling}(…)`
  }
  if (expression.integer._tag === 'Available') return String(expression.integer.value)
  if (expression.integer._tag === 'OutOfRange') return expression.integer.spelling
  return 'unavailable integer'
}

const emptyModel: FlowModel = Object.freeze({
  _tag: 'FlowModel',
  status: 'Empty',
  summary: 'No call expression is available for data-flow projection.',
  nodes: Object.freeze([]),
  edges: Object.freeze([]),
})

/** Projects existing semantic relationships into a small inspector-only flow model. */
export const projectDataFlow = (analysis: SemanticAnalysis.Result): FlowModel => {
  const caller = analysis.functions.find((fact) => fact.returnedExpression._tag === 'Call')
  if (caller === undefined || caller.returnedExpression._tag !== 'Call') return emptyModel

  const call = caller.returnedExpression
  const callerName = declarationName(caller.declaration)
  const nodes: Array<FlowNode> = call.arguments.map((argument) =>
    node(
      `argument-${argument.id.ordinal}`,
      'Argument',
      `Argument #${argument.id.ordinal}: ${argumentLabel(argument)}`,
      argument.type._tag === 'Available' ? argument.type.type : 'Unavailable type',
      call.contract._tag === 'Compatible' ? 'Connected' : 'Unmatched',
      argument.syntax.span,
    ),
  )
  const edges: Array<FlowEdge> = []

  if (call.reference._tag !== 'Resolved') {
    const referenceSpan =
      call.reference._tag === 'Unavailable' ? call.reference.syntax.span : call.reference.token.span
    const referenceId = 'call-reference'
    nodes.push(
      node(
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
        const candidateId = `call-candidate-${declaration.id.ordinal}`
        nodes.push(
          node(
            candidateId,
            'Terminal',
            `Candidate ${declarationName(declaration)}`,
            `function #${declaration.id.ordinal}`,
            'Branched',
            declaration.syntax.span,
          ),
        )
        edges.push(
          edge(
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
      const terminalId = 'unavailable-call-target'
      nodes.push(
        node(
          terminalId,
          'Terminal',
          'Flow stops: no unique target',
          call.contract._tag === 'Unavailable' ? call.contract.reason._tag : call.reference._tag,
          'Stopped',
          call.syntax.span,
        ),
      )
      edges.push(edge(`${referenceId}-stop`, referenceId, terminalId, 'stops at', 'Stopped', call.syntax.span))
    }
    return Object.freeze({
      _tag: 'FlowModel',
      status: 'Incomplete',
      summary: 'The call has no unique target, so no successful argument binding is shown.',
      nodes: Object.freeze(nodes),
      edges: Object.freeze(edges),
    })
  }

  const target = call.reference.declaration
  const targetName = declarationName(target)
  for (const parameter of target.parameters) {
    nodes.push(
      node(
        `parameter-${parameter.id.ordinal}`,
        'Parameter',
        `${targetName}.${parameterName(parameter)}`,
        `parameter #${parameter.id.ordinal}`,
        call.mappings.some((mapping) => mapping.parameter === parameter)
          ? 'Connected'
          : 'Unmatched',
        parameter.syntax.span,
      ),
    )
  }
  for (const mapping of call.mappings) {
    edges.push(
      edge(
        `mapping-${mapping.argument.id.ordinal}`,
        `argument-${mapping.argument.id.ordinal}`,
        `parameter-${mapping.parameter.id.ordinal}`,
        'binds positionally to',
        'Connected',
        mapping.argument.syntax.span,
      ),
    )
  }

  if (call.contract._tag !== 'Compatible') {
    const terminalId = 'call-contract-stop'
    nodes.push(
      node(
        terminalId,
        'Terminal',
        `Flow stops: ${call.contract._tag}`,
        call.contract._tag === 'ArityMismatch'
          ? `${call.contract.actualCount} actual / ${call.contract.expectedCount} expected`
          : call.contract.reason._tag,
        'Stopped',
        call.syntax.span,
      ),
    )
    const origins = call.mappings.map((mapping) => `parameter-${mapping.parameter.id.ordinal}`)
    for (const origin of origins.length === 0 ? call.arguments.map((argument) => `argument-${argument.id.ordinal}`) : origins) {
      edges.push(edge(`${origin}-contract-stop`, origin, terminalId, 'contract stops at', 'Stopped', call.syntax.span))
    }
    return Object.freeze({
      _tag: 'FlowModel',
      status: 'Incomplete',
      summary: 'Known positional pairs remain visible, but the call contract stops the successful path.',
      nodes: Object.freeze(nodes),
      edges: Object.freeze(edges),
    })
  }

  const targetFact = analysis.functions.find((fact) => fact.declaration === target)
  const returned = targetFact?.returnedExpression
  if (returned === undefined || returned._tag !== 'Identifier') {
    const terminalId = 'target-return-stop'
    nodes.push(node(terminalId, 'Terminal', 'Flow stops: target does not return a parameter reference', 'No recorded reference edge', 'Stopped', target.syntax.span))
    return Object.freeze({ _tag: 'FlowModel', status: 'Incomplete', summary: 'The target has no parameter-reference return path.', nodes: Object.freeze(nodes), edges: Object.freeze(edges) })
  }

  const referenceId = 'target-reference'
  const referenceLabel =
    returned.reference._tag === 'Unavailable' ? 'Unavailable reference' : returned.reference.spelling
  nodes.push(
    node(
      referenceId,
      'Reference',
      `Returned reference: ${referenceLabel}`,
      returned.reference._tag,
      returned.reference._tag === 'Resolved'
        ? 'Connected'
        : returned.reference._tag === 'Ambiguous'
          ? 'Branched'
          : 'Stopped',
      returned.syntax.span,
    ),
  )

  if (returned.reference._tag === 'Ambiguous') {
    for (const parameter of returned.reference.parameters) {
      edges.push(edge(`parameter-${parameter.id.ordinal}-branch`, `parameter-${parameter.id.ordinal}`, referenceId, 'could be read by', 'Branched', returned.syntax.span))
    }
  } else if (returned.reference._tag === 'Resolved') {
    edges.push(edge('parameter-reference', `parameter-${returned.reference.parameter.id.ordinal}`, referenceId, 'is read by', 'Connected', returned.syntax.span))
  } else {
    const terminalId = 'reference-stop'
    nodes.push(node(terminalId, 'Terminal', 'Flow stops: unresolved returned reference', returned.reference._tag, 'Stopped', returned.syntax.span))
    edges.push(edge('reference-stop-edge', referenceId, terminalId, 'stops at', 'Stopped', returned.syntax.span))
  }

  if (returned.reference._tag !== 'Resolved') {
    return Object.freeze({ _tag: 'FlowModel', status: 'Incomplete', summary: 'The target return reference does not resolve uniquely.', nodes: Object.freeze(nodes), edges: Object.freeze(edges) })
  }

  const resultId = 'call-result'
  const callerReturnId = 'caller-return'
  nodes.push(
    node(resultId, 'CallResult', `${targetName} call result`, call.type._tag === 'Available' ? call.type.type : 'Unavailable type', 'Connected', call.syntax.span),
    node(callerReturnId, 'FunctionReturn', `${callerName} return`, caller.returnCompatibility._tag, 'Connected', call.syntax.span),
  )
  edges.push(
    edge('reference-result', referenceId, resultId, 'produces', 'Connected', returned.syntax.span),
    edge('result-return', resultId, callerReturnId, 'is returned by', 'Connected', call.syntax.span),
  )

  const firstArgument = call.arguments.at(0)
  return Object.freeze({
    _tag: 'FlowModel',
    status: 'Complete',
    summary:
      firstArgument === undefined
        ? `${targetName} produces the value returned by ${callerName}.`
        : `${argumentLabel(firstArgument)} flows through ${targetName} into ${callerName}.`,
    nodes: Object.freeze(nodes),
    edges: Object.freeze(edges),
  })
}
