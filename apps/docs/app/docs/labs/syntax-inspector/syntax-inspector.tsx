'use client'

import {
  BootstrapEvaluation,
  Lexer,
  Parser,
  SemanticAnalysis,
  SourceFile,
  SyntaxTree,
} from '@silk-effect/compiler'
import { useMemo, useState } from 'react'
import { projectDataFlow, type FlowEdge, type FlowNode } from './flow-model'
import styles from './syntax-inspector.module.css'

const sourceId = 'memory://docs/syntax-inspector.silk'
const acceptedSource = 'pub fn main() -> I32 { return 42 }'

const examples = [
  { label: 'Literal result', source: acceptedSource },
  {
    label: 'Two functions',
    source: `pub fn answer() -> I32 { return 42 }
pub fn main() -> I32 { return 0 }`,
  },
  {
    label: 'Three functions',
    source: `pub fn one() -> I32 { return 1 }
pub fn two() -> I32 { return 2 }
pub fn three() -> I32 { return 3 }`,
  },
  {
    label: 'Resolved backward',
    source: `pub fn answer() -> I32 { return 42 }
pub fn main() -> I32 { return answer() }`,
  },
  {
    label: 'Resolved forward',
    source: `pub fn main() -> I32 { return answer() }
pub fn answer() -> I32 { return 42 }`,
  },
  { label: 'Direct cycle', source: 'pub fn main() -> I32 { return main() }' },
  { label: 'Unknown call', source: 'pub fn main() -> I32 { return missing() }' },
  {
    label: 'Ambiguous call',
    source: `pub fn same() -> I32 { return 1 }
pub fn same() -> I32 { return 2 }
pub fn main() -> I32 { return same() }`,
  },
  { label: 'Missing callee', source: 'pub fn main() -> I32 { return () }' },
  {
    label: 'Missing call )',
    source: `pub fn answer() -> I32 { return 42 }
pub fn main() -> I32 { return answer( }`,
  },
  {
    label: 'Resolved parameter',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(42) }`,
  },
  {
    label: 'Identity result',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(42) }`,
  },
  {
    label: 'Flow unknown reference',
    source: `pub fn identity(value: I32) -> I32 { return missing }
pub fn main() -> I32 { return identity(42) }`,
  },
  {
    label: 'Flow ambiguous reference',
    source: `pub fn choose(value: I32, value: I32) -> I32 { return value }
pub fn main() -> I32 { return choose(1, 2) }`,
  },
  {
    label: 'Flow damaged syntax',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(@) }`,
  },
  {
    label: 'Wrong arity',
    source: `pub fn choose(left: I32, right: I32) -> I32 { return left }
pub fn main() -> I32 { return choose(1) }`,
  },
  {
    label: 'Too many arguments',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(1, 2) }`,
  },
  {
    label: 'Unavailable evaluation',
    source: `pub fn identity(value: Mystery) -> I32 { return 0 }
pub fn main() -> I32 { return identity(42) }`,
  },
  {
    label: 'Second argument result',
    source: `pub fn second(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return second(10, 42) }`,
  },
  {
    label: 'Missing entry',
    source: 'pub fn answer() -> I32 { return 42 }',
  },
  {
    label: 'Mutual cycle',
    source: `pub fn main() -> I32 { return other() }
pub fn other() -> I32 { return main() }`,
  },
  {
    label: 'Unresolved contract call',
    source: 'pub fn main() -> I32 { return missing(42) }',
  },
  {
    label: 'Unknown parameter',
    source: 'pub fn main() -> I32 { return missing }',
  },
  {
    label: 'Duplicate parameter',
    source: 'pub fn choose(value: I32, value: I32) -> I32 { return value }',
  },
  {
    label: 'Cross-function parameter',
    source: `pub fn owner(value: I32) -> I32 { return value }
pub fn other() -> I32 { return value }`,
  },
  {
    label: 'Recovered reference',
    source: 'pub fn identity(value: I32) -> I32 { return @ value }',
  },
  {
    label: 'Two parameters',
    source: 'pub fn choose(left: I32, right: I32) -> I32 { return left }',
  },
  {
    label: 'Identifier argument',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn forward(value: I32) -> I32 { return identity(value) }`,
  },
  {
    label: 'Nested call · syntax only',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`,
  },
  {
    label: 'Damaged nested call',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42) }`,
  },
  {
    label: 'Missing parameter type',
    source: 'pub fn identity(value:) -> I32 { return value }',
  },
  {
    label: 'Missing parameter comma',
    source: 'pub fn choose(left: I32 right: I32) -> I32 { return left }',
  },
  {
    label: 'Malformed argument',
    source: 'pub fn main(value: I32) -> I32 { return missing(@, value) }',
  },
  {
    label: 'Missing name',
    source: `pub fn answer() -> I32 { return 42 }
pub fn () -> I32 { return 0 }`,
  },
  {
    label: 'Duplicate names',
    source: `pub fn same() -> I32 { return 1 }
pub fn same() -> I32 { return 2 }`,
  },
  {
    label: 'Mixed damage',
    source: `pub fn main() -> I32 { return 42 }
pub fn damaged() -> Mystery { return 2147483648 }`,
  },
  {
    label: 'Missing first }',
    source: `pub fn answer() -> I32 { return 42
pub fn main() -> I32 { return 0 }`,
  },
  { label: 'Missing }', source: 'pub fn main() -> I32 { return 42' },
  { label: 'Unexpected @', source: 'pub fn @ main() -> I32 { return 42 }' },
  { label: 'Unknown type', source: 'pub fn main() -> Mystery { return 42 }' },
  { label: 'I32 overflow', source: 'pub fn main() -> I32 { return 2147483648 }' },
  { label: 'UTF-8', source: 'pub fn café() -> I32 { return 42 }' },
] as const

const encoder = new TextEncoder()
const decoder = new TextDecoder()

const spanLabel = (element: SyntaxTree.Element): string =>
  `[${element.span.start}, ${element.span.end})`

const sliceLabel = (source: SourceFile.SourceFile, element: SyntaxTree.Element): string => {
  const bytes = Uint8Array.from(source.bytes.slice(element.span.start, element.span.end))
  return JSON.stringify(decoder.decode(bytes))
}

function TreeElement({
  element,
  source,
}: {
  readonly element: SyntaxTree.Element
  readonly source: SourceFile.SourceFile
}) {
  if (SyntaxTree.isNode(element)) {
    return (
      <li className={styles.treeItem}>
        <details open>
          <summary className={styles.nodeSummary}>
            <span className={styles.nodeKind}>{element.kind}</span>
            <span className={styles.span}>{spanLabel(element)}</span>
          </summary>
          <ol className={styles.treeGroup} aria-label={`${element.kind} children`}>
            {element.children.map((child, index) => (
              <TreeElement
                key={`${child._tag}-${child.span.start}-${child.span.end}-${index}`}
                element={child}
                source={source}
              />
            ))}
          </ol>
        </details>
      </li>
    )
  }

  if (SyntaxTree.isToken(element)) {
    return (
      <li className={styles.leaf}>
        <span className={styles.tokenDot} aria-hidden="true" />
        <span className={styles.tokenKind}>{element.kind}</span>
        <code className={styles.slice}>{sliceLabel(source, element)}</code>
        <span className={styles.span}>{spanLabel(element)}</span>
      </li>
    )
  }

  return (
    <li className={`${styles.leaf} ${styles.missingLeaf}`}>
      <span className={styles.missingDot} aria-hidden="true" />
      <span className={styles.missingKind}>Missing {element.expected}</span>
      <code className={styles.slice}>expected</code>
      <span className={styles.span}>{spanLabel(element)}</span>
    </li>
  )
}

function DiagnosticList({
  title,
  diagnostics,
}: {
  readonly title: string
  readonly diagnostics: ReadonlyArray<{
    readonly code: string
    readonly message: string
    readonly span: { readonly start: number; readonly end: number }
  }>
}) {
  return (
    <section className={styles.diagnosticGroup} aria-labelledby={`${title}-diagnostics`}>
      <div className={styles.diagnosticHeading}>
        <h3 id={`${title}-diagnostics`}>{title}</h3>
        <span>{diagnostics.length}</span>
      </div>
      {diagnostics.length === 0 ? (
        <p className={styles.emptyState}>No diagnostics</p>
      ) : (
        <ul className={styles.diagnosticList}>
          {diagnostics.map((diagnostic, index) => (
            <li key={`${diagnostic.code}-${diagnostic.span.start}-${index}`}>
              <div>
                <code>{diagnostic.code}</code>
                <span>
                  [{diagnostic.span.start}, {diagnostic.span.end})
                </span>
              </div>
              <p>{diagnostic.message}</p>
            </li>
          ))}
        </ul>
      )}
    </section>
  )
}

type CallReturnedExpression = Extract<
  SemanticAnalysis.ReturnedExpressionFact,
  { readonly _tag: 'Call' }
>

type IdentifierExpression = SemanticAnalysis.IdentifierExpressionFact

const declarationLabel = (declaration: SemanticAnalysis.DeclarationFact): string =>
  declaration.name._tag === 'Present' ? declaration.name.spelling : 'Unavailable name'

const parameterLabel = (parameter: SemanticAnalysis.ParameterFact): string =>
  parameter.name._tag === 'Present' ? parameter.name.spelling : 'Unavailable name'

function ParameterRelationship({ expression }: { readonly expression: IdentifierExpression }) {
  const reference = expression.reference
  const referenceName = reference._tag === 'Unavailable' ? 'Unavailable reference' : reference.spelling
  const targetName =
    reference._tag === 'Resolved' ? parameterLabel(reference.parameter) : referenceName

  return (
    <section
      className={styles.parameterRelationship}
      aria-label={`Parameter relationship from ${referenceName} to ${targetName}`}
    >
      <div className={styles.relationshipHeading}>
        <span>Local parameter resolution</span>
        <strong>
          {referenceName} <i aria-hidden="true">→</i> {targetName}
        </strong>
        <code>{reference._tag}</code>
      </div>
      <dl className={styles.relationshipFacts}>
        <div>
          <dt>Reference</dt>
          <dd>
            <code>{referenceName}</code>
            <span>
              {reference._tag === 'Unavailable'
                ? spanLabel(reference.syntax)
                : spanLabel(reference.token)}
            </span>
          </dd>
        </div>
        <div>
          <dt>Expression type</dt>
          <dd>
            <code>{expression.type._tag === 'Available' ? expression.type.type : 'Unavailable'}</code>
            <span>{expression.type._tag}</span>
          </dd>
        </div>
        <div>
          <dt>Scope</dt>
          <dd>
            <code>Function-local</code>
            <span>Parameters only</span>
          </dd>
        </div>
        <div>
          <dt>Matches</dt>
          <dd>
            <code>
              {reference._tag === 'Resolved'
                ? 1
                : reference._tag === 'Ambiguous'
                  ? reference.parameters.length
                  : 0}
            </code>
            <span>{reference._tag}</span>
          </dd>
        </div>
      </dl>
      {reference._tag === 'Resolved' ? (
        <div className={styles.relationshipTargets}>
          <span>Parameter declaration</span>
          <code>
            function #{reference.parameter.id.function.ordinal} · parameter #
            {reference.parameter.id.ordinal} · {spanLabel(reference.parameter.syntax)}
          </code>
        </div>
      ) : reference._tag === 'Ambiguous' ? (
        <div className={styles.relationshipTargets}>
          <span>{reference.parameters.length} matching parameters</span>
          <ul>
            {reference.parameters.map((parameter) => (
              <li
                key={`${parameter.id.function.sourceId}-${parameter.id.function.ordinal}-${parameter.id.ordinal}`}
              >
                <code>
                  parameter #{parameter.id.ordinal} · {spanLabel(parameter.syntax)}
                </code>
              </li>
            ))}
          </ul>
        </div>
      ) : (
        <p className={styles.relationshipNotice}>
          {reference._tag === 'Missing'
            ? 'No parameter in this function matches the reference.'
            : 'Parser recovery did not provide a usable local reference.'}
        </p>
      )}
    </section>
  )
}

function ParameterFacts({ declaration }: { readonly declaration: SemanticAnalysis.DeclarationFact }) {
  if (declaration.parameters.length === 0) {
    return <p className={styles.noParameters}>No parameters</p>
  }

  return (
    <section
      className={styles.parameterFacts}
      aria-label={`Parameters for function ${declaration.id.ordinal}`}
    >
      <div className={styles.parameterFactsHeading}>
        <span>Local parameter declarations</span>
        <code>{declaration.parameters.length}</code>
      </div>
      <div className={styles.parameterList}>
        {declaration.parameters.map((parameter) => {
          const name = parameter.name
          const type = parameter.declaredType
          const lookup =
            name._tag === 'Present'
              ? SemanticAnalysis.parameterByName(declaration, name.spelling)
              : undefined
          return (
            <article
              className={styles.parameterCard}
              key={`${parameter.id.function.sourceId}-${parameter.id.function.ordinal}-${parameter.id.ordinal}`}
              aria-label={`Parameter ${parameter.id.ordinal}: ${parameterLabel(parameter)}`}
            >
              <div>
                <span>Parameter #{parameter.id.ordinal}</span>
                <strong>{parameterLabel(parameter)}</strong>
              </div>
              <dl>
                <div>
                  <dt>Identity</dt>
                  <dd>
                    <code>
                      function #{parameter.id.function.ordinal} · parameter #{parameter.id.ordinal}
                    </code>
                  </dd>
                </div>
                <div>
                  <dt>Declared type</dt>
                  <dd>
                    <code>
                      {type._tag === 'Resolved'
                        ? type.type
                        : type._tag === 'Unresolved'
                          ? type.spelling
                          : 'Unavailable'}
                    </code>
                    <span>{type._tag}</span>
                  </dd>
                </div>
                <div>
                  <dt>Lookup</dt>
                  <dd>
                    <code>{lookup?._tag ?? 'Unavailable'}</code>
                  </dd>
                </div>
                <div>
                  <dt>Provenance</dt>
                  <dd>
                    <code>{spanLabel(parameter.syntax)}</code>
                    <span>
                      {name._tag === 'Present'
                        ? `name ${spanLabel(name.token)}`
                        : `name ${spanLabel(name.syntax)}`}
                    </span>
                    <span>type {spanLabel(type.syntax)}</span>
                  </dd>
                </div>
              </dl>
            </article>
          )
        })}
      </div>
    </section>
  )
}

function CallRelationship({
  caller,
  returned,
}: {
  readonly caller: SemanticAnalysis.DeclarationFact
  readonly returned: CallReturnedExpression
}) {
  const reference = returned.reference
  const callerName = declarationLabel(caller)
  const calleeName = reference._tag === 'Unavailable' ? 'Unavailable callee' : reference.spelling
  const targetName =
    reference._tag === 'Resolved' ? declarationLabel(reference.declaration) : calleeName
  const expectedCount = reference._tag === 'Resolved' ? reference.declaration.parameters.length : undefined
  const contractReason =
    returned.contract._tag !== 'Unavailable'
      ? undefined
      : returned.contract.reason._tag === 'UnavailableCallSyntax'
        ? 'Call syntax is incomplete.'
        : returned.contract.reason._tag === 'UnavailableCallTarget'
          ? 'A unique call target is unavailable.'
          : returned.contract.reason._tag === 'UnavailableNestedArgument'
            ? 'A nested call argument is preserved but not semantically analyzed yet.'
            : 'A mapped argument or parameter type is unavailable.'

  return (
    <section
      className={styles.callRelationship}
      aria-label={`Call relationship from ${callerName} to ${targetName}`}
    >
      <div className={styles.relationshipHeading}>
        <span>Semantic resolution</span>
        <strong>
          {callerName} <i aria-hidden="true">→</i> {targetName}
        </strong>
        <code>{reference._tag}</code>
      </div>
      <dl className={styles.relationshipFacts}>
        <div>
          <dt>Caller</dt>
          <dd>
            <code>#{caller.id.ordinal}</code>
            <span>{spanLabel(caller.syntax)}</span>
          </dd>
        </div>
        <div>
          <dt>Call site</dt>
          <dd>
            <code>{calleeName}</code>
            <span>
              {reference._tag === 'Unavailable'
                ? spanLabel(reference.syntax)
                : spanLabel(reference.token)}
            </span>
          </dd>
        </div>
        <div>
          <dt>Call type</dt>
          <dd>
            <code>{returned.type._tag === 'Available' ? returned.type.type : 'Unavailable'}</code>
            <span>{returned.type._tag}</span>
          </dd>
        </div>
        <div>
          <dt>Arguments</dt>
          <dd>
            <code>
              {returned.arguments.length} actual · {expectedCount ?? '—'} expected
            </code>
            <span>{returned.mappings.length} positional mappings</span>
          </dd>
        </div>
      </dl>
      <section
        className={styles.callContract}
        aria-label={`Call contract ${returned.contract._tag} for ${callerName} to ${targetName}`}
      >
        <div className={styles.callContractHeading}>
          <span>Positional call contract</span>
          <strong>{returned.contract._tag}</strong>
          <code>
            {returned.arguments.length} / {expectedCount ?? '—'}
          </code>
        </div>
        {returned.arguments.length === 0 ? (
          <p className={styles.relationshipNotice}>No semantic arguments were collected.</p>
        ) : (
          <ol className={styles.argumentList} aria-label="Ordered call arguments">
            {returned.arguments.map((argument) => {
              const mapping = returned.mappings.find((candidate) => candidate.argument === argument)
              const expression = argument.expression
              const expressionLabel =
                expression._tag === 'Integer'
                  ? expression.integer._tag === 'Available'
                    ? String(expression.integer.value)
                    : expression.integer._tag
                  : expression._tag === 'UnavailableNestedCall'
                    ? 'Nested call · not analyzed'
                    : expression.reference._tag === 'Unavailable'
                      ? 'Unavailable reference'
                      : expression.reference.spelling
              return (
                <li key={`${argument.id.callSpan.start}-${argument.id.ordinal}`}>
                  <div>
                    <span>Argument #{argument.id.ordinal}</span>
                    <strong>{expressionLabel}</strong>
                    <code>{argument.type._tag === 'Available' ? argument.type.type : 'Unavailable'}</code>
                  </div>
                  <div>
                    <span>{spanLabel(argument.syntax)}</span>
                    {mapping === undefined ? (
                      <code>Unmatched</code>
                    ) : (
                      <code>
                        → parameter #{mapping.parameter.id.ordinal} ·{' '}
                        {parameterLabel(mapping.parameter)} ·{' '}
                        {mapping.parameter.declaredType._tag === 'Resolved'
                          ? mapping.parameter.declaredType.type
                          : 'Unavailable'}
                      </code>
                    )}
                  </div>
                </li>
              )
            })}
          </ol>
        )}
        {reference._tag === 'Resolved' &&
        returned.mappings.length < reference.declaration.parameters.length ? (
          <p className={styles.unmatchedParameters}>
            Unmatched target parameters:{' '}
            {reference.declaration.parameters
              .slice(returned.mappings.length)
              .map((parameter) => `#${parameter.id.ordinal} ${parameterLabel(parameter)}`)
              .join(', ')}
          </p>
        ) : null}
        {contractReason === undefined ? null : (
          <p className={styles.relationshipNotice}>{contractReason}</p>
        )}
      </section>
      {reference._tag === 'Resolved' ? (
        <div className={styles.relationshipTargets}>
          <span>Target declaration</span>
          <code>
            #{reference.declaration.id.ordinal} · {spanLabel(reference.declaration.syntax)}
          </code>
        </div>
      ) : reference._tag === 'Ambiguous' ? (
        <div className={styles.relationshipTargets}>
          <span>{reference.declarations.length} matching declarations</span>
          <ul>
            {reference.declarations.map((declaration) => (
              <li key={`${declaration.id.sourceId}-${declaration.id.ordinal}`}>
                <code>
                  #{declaration.id.ordinal} · {spanLabel(declaration.syntax)}
                </code>
              </li>
            ))}
          </ul>
        </div>
      ) : (
        <p className={styles.relationshipNotice}>
          {reference._tag === 'Missing'
            ? 'No top-level declaration matches this call.'
            : 'Parser recovery did not provide a usable callee.'}
        </p>
      )}
    </section>
  )
}

const flowSpanLabel = (item: FlowNode | FlowEdge): string =>
  `${item.span.sourceId}[${item.span.start}, ${item.span.end})`

export function DataFlow({
  analysis,
  selectedId,
  onSelect,
}: {
  readonly analysis: SemanticAnalysis.Result
  readonly selectedId: string | undefined
  readonly onSelect: (id: string | undefined) => void
}) {
  const flow = useMemo(() => projectDataFlow(analysis), [analysis])
  const items: ReadonlyArray<FlowNode | FlowEdge> = [...flow.nodes, ...flow.edges]
  const selected = items.find((item) => item.id === selectedId)
  const source = analysis.parse.lexical.source
  const selectedSlice =
    selected === undefined
      ? undefined
      : decoder.decode(
          Uint8Array.from(source.bytes.slice(selected.span.start, selected.span.end)),
        )

  return (
    <section className={styles.dataFlow} aria-labelledby="data-flow-heading">
      <div className={styles.dataFlowHeading}>
        <div>
          <span className={styles.eyebrow}>Semantic path</span>
          <h3 id="data-flow-heading">Value flow</h3>
        </div>
        <code data-state={flow.status}>{flow.status}</code>
      </div>
      <p className={styles.dataFlowSummary}>{flow.summary}</p>

      {flow.status === 'Empty' ? (
        <p className={styles.emptyState}>Choose a call preset to inspect a value path.</p>
      ) : (
        <>
          <div className={styles.flowLane} role="group" aria-label="Navigable value-flow nodes">
            {flow.nodes.map((flowNode, index) => (
              <div className={styles.flowLaneItem} key={flowNode.id}>
                {index === 0 ? null : (
                  <span className={styles.flowConnector} aria-hidden="true">
                    →
                  </span>
                )}
                <button
                  type="button"
                  className={styles.flowNode}
                  data-state={flowNode.state}
                  aria-pressed={selectedId === flowNode.id}
                  onClick={() => onSelect(selectedId === flowNode.id ? undefined : flowNode.id)}
                >
                  <span>{flowNode.kind}</span>
                  <strong>{flowNode.label}</strong>
                  <small>{flowNode.state}</small>
                  <code>[{flowNode.span.start}, {flowNode.span.end})</code>
                </button>
              </div>
            ))}
          </div>

          <ol className={styles.flowRelationships} aria-label="Ordered value-flow relationships">
            {flow.edges.map((flowEdge) => {
              const from = flow.nodes.find((flowNode) => flowNode.id === flowEdge.from)
              const to = flow.nodes.find((flowNode) => flowNode.id === flowEdge.to)
              return (
                <li key={flowEdge.id}>
                  <button
                    type="button"
                    data-state={flowEdge.state}
                    aria-pressed={selectedId === flowEdge.id}
                    onClick={() => onSelect(selectedId === flowEdge.id ? undefined : flowEdge.id)}
                  >
                    <strong>
                      {from?.label ?? flowEdge.from} {flowEdge.label} {to?.label ?? flowEdge.to}
                    </strong>
                    <span>{flowEdge.state}</span>
                    <code>{flowSpanLabel(flowEdge)}</code>
                  </button>
                </li>
              )
            })}
          </ol>
        </>
      )}

      {selected === undefined ? (
        <p className={styles.flowSelection}>
          Activate any node or relationship to inspect its source.
        </p>
      ) : (
        <aside className={styles.flowSelection} aria-live="polite" aria-label="Selected flow source">
          <div>
            <strong>Selected {selected._tag === 'FlowNode' ? selected.kind : 'relationship'}</strong>
            <code>{flowSpanLabel(selected)}</code>
          </div>
          <p>{selected._tag === 'FlowNode' ? selected.detail : selected.label}</p>
          <pre>
            <code>
              {decoder.decode(Uint8Array.from(source.bytes.slice(0, selected.span.start)))}
              <mark>{selectedSlice}</mark>
              {decoder.decode(Uint8Array.from(source.bytes.slice(selected.span.end)))}
            </code>
          </pre>
        </aside>
      )}
    </section>
  )
}

const blockedSummary = (reason: BootstrapEvaluation.BlockedReason): string => {
  switch (reason._tag) {
    case 'MissingEntry':
      return 'No top-level declaration named main is available.'
    case 'AmbiguousEntry':
      return `${reason.lookup.declarations.length} declarations named main are available; none was selected.`
    case 'ParameterizedEntry':
      return `main has ${reason.actualCount} parameters; bootstrap entry requires exactly zero.`
    case 'UnavailableEntryType':
      return `main does not have an available resolved I32 return type (${reason.returnType._tag}).`
    case 'UnavailableInteger':
      return `An integer on the reachable path is ${reason.integer._tag}.`
    case 'MissingParameterReference':
      return `No local parameter matches ${reason.reference.spelling}.`
    case 'AmbiguousParameterReference':
      return `${reason.reference.parameters.length} local parameters match ${reason.reference.spelling}.`
    case 'UnavailableParameterReference':
      return 'Parser recovery did not provide a usable parameter reference.'
    case 'UnboundParameter':
      return `Parameter #${reason.parameter.id.ordinal} has no value in the current call frame.`
    case 'MissingCallTarget':
      return 'The reachable call target is missing.'
    case 'AmbiguousCallTarget':
      return 'The reachable call target is ambiguous.'
    case 'UnavailableCallTarget':
      return 'Parser recovery did not provide a usable call target.'
    case 'ArityMismatch':
      return `The reachable call has ${reason.actualCount} arguments but requires ${reason.expectedCount}.`
    case 'UnavailableCallContract':
      return `The reachable call contract is unavailable: ${reason.reason._tag}.`
    case 'UnavailableFunction':
      return `No body fact is available for ${declarationLabel(reason.declaration)}.`
    case 'RecursiveCycle':
      return `Recursive cycle: ${reason.cycle.map(declarationLabel).join(' → ')}.`
  }
}

const traceLabel = (event: BootstrapEvaluation.TraceEvent): string => {
  switch (event._tag) {
    case 'Entry':
      return `Enter ${declarationLabel(event.declaration)}`
    case 'Call':
      return `${declarationLabel(event.caller)} calls ${declarationLabel(event.target)}`
    case 'Binding':
      return `Argument #${event.argument.id.ordinal} binds ${event.value.value} to ${declarationLabel(event.target)}.${parameterLabel(event.parameter)}`
    case 'ParameterRead':
      return `${declarationLabel(event.declaration)} reads ${parameterLabel(event.parameter)} as ${event.value.value}`
    case 'Return':
      return `${declarationLabel(event.declaration)} returns ${event.value.value}`
  }
}

export function EvaluationPanel({
  outcome,
  onEvaluate,
}: {
  readonly outcome: BootstrapEvaluation.Outcome | undefined
  readonly onEvaluate: () => void
}) {
  return (
    <section className={styles.evaluation} aria-labelledby="evaluation-heading">
      <div className={styles.evaluationHeading}>
        <div>
          <span className={styles.eyebrow}>Direct interpretation</span>
          <h3 id="evaluation-heading">Bootstrap evaluation</h3>
        </div>
        <button type="button" onClick={onEvaluate}>
          Evaluate current source
        </button>
      </div>

      {outcome === undefined ? (
        <p className={styles.evaluationIdle}>
          Evaluation is explicit and local. Static analysis above does not execute the program.
        </p>
      ) : (
        <div aria-live="polite">
          <div className={styles.evaluationOutcome} data-state={outcome._tag}>
            <span>{outcome._tag}</span>
            {outcome._tag === 'Completed' ? (
              <strong>
                {outcome.result.value} <code>I32</code>
              </strong>
            ) : (
              <>
                <strong>{outcome.reason._tag}</strong>
                <p>{blockedSummary(outcome.reason)}</p>
              </>
            )}
          </div>

          <ol className={styles.evaluationTrace} aria-label="Ordered bootstrap evaluation trace">
            {outcome.trace.map((event, index) => (
              <li key={`${event._tag}-${event.span.start}-${index}`}>
                <span>{index + 1}</span>
                <div>
                  <strong>{traceLabel(event)}</strong>
                  <code>
                    {event.span.sourceId}[{event.span.start}, {event.span.end})
                  </code>
                </div>
                <small>{event._tag}</small>
              </li>
            ))}
          </ol>
          {outcome.trace.length === 0 ? (
            <p className={styles.evaluationIdle}>No reachable evaluation events occurred.</p>
          ) : null}
        </div>
      )}
    </section>
  )
}

function SemanticFacts({
  analysis,
  selectedFlowId,
  onSelectFlow,
  evaluation,
  onEvaluate,
}: {
  readonly analysis: SemanticAnalysis.Result
  readonly selectedFlowId: string | undefined
  readonly onSelectFlow: (id: string | undefined) => void
  readonly evaluation: BootstrapEvaluation.Outcome | undefined
  readonly onEvaluate: () => void
}) {
  const presentNames = Array.from(
    new Set(
      analysis.functions.flatMap((fact) =>
        fact.declaration.name._tag === 'Present' ? [fact.declaration.name.spelling] : [],
      ),
    ),
  )
  const lookups = presentNames.map((spelling) =>
    SemanticAnalysis.declarationByName(analysis, spelling),
  )

  return (
    <section className={styles.semanticFacts} aria-labelledby="semantic-facts-heading">
      <div className={styles.semanticHeading}>
        <div>
          <span className={styles.eyebrow}>Analysis</span>
          <h3 id="semantic-facts-heading">Semantic facts</h3>
        </div>
        <span className={styles.phaseBoundary}>
          {analysis.functions.length} {analysis.functions.length === 1 ? 'function' : 'functions'}
        </span>
      </div>

      <section className={styles.lookupSummary} aria-labelledby="lookup-summary-heading">
        <strong id="lookup-summary-heading">Declaration lookup</strong>
        {lookups.length === 0 ? (
          <span>No present names</span>
        ) : (
          <ul>
            {lookups.map((lookup) => (
              <li key={lookup.spelling}>
                <code>{lookup.spelling}</code>
                <span>
                  {lookup._tag}
                  {lookup._tag === 'Ambiguous' ? ` · ${lookup.declarations.length} matches` : ''}
                </span>
              </li>
            ))}
          </ul>
        )}
      </section>

      <DataFlow analysis={analysis} selectedId={selectedFlowId} onSelect={onSelectFlow} />
      <EvaluationPanel outcome={evaluation} onEvaluate={onEvaluate} />

      <div className={styles.functionList} aria-label="Collected function facts">
        {analysis.functions.map((fact) => {
          const declaration = fact.declaration
          const name = declaration.name
          const returnType = declaration.returnType
          const returned = fact.returnedExpression
          const nameLabel = name._tag === 'Present' ? name.spelling : 'Unavailable name'
          const identifierExpressions: ReadonlyArray<IdentifierExpression> =
            returned._tag === 'Identifier'
              ? [returned]
              : returned._tag === 'Call'
                ? returned.arguments
                    .map((argument) => argument.expression)
                    .filter(
                    (argument): argument is IdentifierExpression => argument._tag === 'Identifier',
                  )
                : []

          return (
            <article
              className={styles.functionCard}
              key={`${declaration.id.sourceId}-${declaration.id.ordinal}`}
              aria-label={`Function ${declaration.id.ordinal}: ${nameLabel}`}
            >
              <div className={styles.functionCardHeading}>
                <div>
                  <span>Function #{declaration.id.ordinal}</span>
                  <strong>{nameLabel}</strong>
                </div>
                <code>{spanLabel(declaration.syntax)}</code>
              </div>

              <ParameterFacts declaration={declaration} />

              {identifierExpressions.map((expression, index) => (
                <ParameterRelationship
                  key={`${expression.syntax.span.start}-${expression.syntax.span.end}-${index}`}
                  expression={expression}
                />
              ))}

              {returned._tag === 'Call' ? (
                <CallRelationship caller={declaration} returned={returned} />
              ) : null}

              <dl className={styles.factGrid}>
                <div>
                  <dt>Declaration</dt>
                  <dd>
                    <strong>{nameLabel}</strong>
                    <span>
                      public · {declaration.parameterCount}{' '}
                      {declaration.parameterCount === 1 ? 'parameter' : 'parameters'}
                    </span>
                    <code>
                      {declaration.id.sourceId}#{declaration.id.ordinal}
                    </code>
                    <small>
                      {name._tag === 'Present'
                        ? `name ${spanLabel(name.token)}`
                        : `missing ${spanLabel(name.syntax)}`}
                    </small>
                  </dd>
                </div>
                <div>
                  <dt>Declared type</dt>
                  <dd>
                    <strong>
                      {returnType._tag === 'Resolved'
                        ? returnType.type
                        : returnType._tag === 'Unresolved'
                          ? returnType.spelling
                          : 'Unavailable'}
                    </strong>
                    <span>{returnType._tag}</span>
                    <small>{spanLabel(returnType.syntax)}</small>
                  </dd>
                </div>
                <div>
                  <dt>Returned expression</dt>
                  {returned._tag === 'Integer' ? (
                    <dd>
                      <strong>
                        {returned.integer._tag === 'Available'
                          ? returned.integer.value
                          : 'Unavailable'}
                      </strong>
                      <span>
                        Integer ·{' '}
                        {returned.integer._tag === 'Unavailable'
                          ? returned.integer._tag
                          : `${returned.integer.type} · ${returned.integer._tag}`}
                      </span>
                      <small>{spanLabel(returned.syntax)}</small>
                    </dd>
                  ) : returned._tag === 'Identifier' ? (
                    <dd>
                      <strong>
                        {returned.reference._tag === 'Unavailable'
                          ? 'Unavailable'
                          : returned.reference.spelling}
                      </strong>
                      <span>
                        Identifier · {returned.reference._tag} ·{' '}
                        {returned.type._tag === 'Available' ? returned.type.type : 'Unavailable type'}
                      </span>
                      <small>{spanLabel(returned.syntax)}</small>
                    </dd>
                  ) : (
                    <dd>
                      <strong>
                        {returned.reference._tag !== 'Unavailable'
                          ? returned.reference.spelling
                          : 'Unavailable call'}
                      </strong>
                      <span>
                        Call · {returned.reference._tag} ·{' '}
                        {returned.type._tag === 'Available' ? returned.type.type : 'Unavailable type'}
                      </span>
                      <small>
                        call {spanLabel(returned.syntax)}
                        {returned.reference._tag !== 'Unavailable'
                          ? ` · callee ${spanLabel(returned.reference.token)}`
                          : ` · missing ${spanLabel(returned.reference.syntax)}`}
                      </small>
                    </dd>
                  )}
                </div>
                <div>
                  <dt>Return</dt>
                  <dd>
                    <strong>{fact.returnCompatibility._tag}</strong>
                    <span>I32 range 0…2147483647</span>
                  </dd>
                </div>
              </dl>
            </article>
          )
        })}
      </div>

      <p className={styles.boundaryNote}>
        Parameters now have function-local identities, declared types, closed lookup, and exact
        reference links. Calls now expose ordered argument identities, positional mappings, and a
        contract outcome independently of their return compatibility. These arrows record semantic
        relationships, not execution order. Conversions, general scope graphs, semantic AST, HIR,
        and code generation do not exist yet.
      </p>
    </section>
  )
}

export function SyntaxInspector() {
  const [text, setText] = useState(acceptedSource)
  const [selectedFlowId, setSelectedFlowId] = useState<string>()
  const [evaluation, setEvaluation] = useState<BootstrapEvaluation.Outcome>()
  const analysis = useMemo(() => {
    const source = SourceFile.make(sourceId, encoder.encode(text))
    return SemanticAnalysis.analyze(Parser.parse(Lexer.lex(source)))
  }, [text])
  const result = analysis.parse

  return (
    <div className={styles.inspector}>
      <section className={styles.editorPanel} aria-labelledby="source-heading">
        <div className={styles.panelHeading}>
          <div>
            <span className={styles.eyebrow}>Input</span>
            <h2 id="source-heading">Source</h2>
          </div>
          <span className={styles.sourceId}>{sourceId}</span>
        </div>

        <label className="sr-only" htmlFor="syntax-source">
          Silk source code
        </label>
        <textarea
          id="syntax-source"
          className={styles.editor}
          value={text}
          onChange={(event) => {
            setText(event.target.value)
            setSelectedFlowId(undefined)
            setEvaluation(undefined)
          }}
          spellCheck={false}
          autoCapitalize="off"
          autoCorrect="off"
        />

        <div className={styles.exampleBar} aria-label="Source examples">
          {examples.map((example) => (
            <button
              key={example.label}
              type="button"
              onClick={() => {
                setText(example.source)
                setSelectedFlowId(undefined)
                setEvaluation(undefined)
              }}
            >
              {example.label}
            </button>
          ))}
        </div>

        <dl className={styles.metrics}>
          <div>
            <dt>UTF-8 bytes</dt>
            <dd>{result.lexical.source.bytes.length}</dd>
          </div>
          <div>
            <dt>Tokens</dt>
            <dd>{result.lexical.tokens.length}</dd>
          </div>
          <div>
            <dt>Lexer</dt>
            <dd>{result.lexical.diagnostics.length}</dd>
          </div>
          <div>
            <dt>Parser</dt>
            <dd>{result.diagnostics.length}</dd>
          </div>
          <div>
            <dt>Semantic</dt>
            <dd>{analysis.diagnostics.length}</dd>
          </div>
        </dl>

        <SemanticFacts
          analysis={analysis}
          selectedFlowId={selectedFlowId}
          onSelectFlow={setSelectedFlowId}
          evaluation={evaluation}
          onEvaluate={() => setEvaluation(BootstrapEvaluation.evaluate(analysis))}
        />

        <div className={styles.diagnostics}>
          <DiagnosticList title="Lexer" diagnostics={result.lexical.diagnostics} />
          <DiagnosticList title="Parser" diagnostics={result.diagnostics} />
          <DiagnosticList title="Semantic" diagnostics={analysis.diagnostics} />
        </div>
      </section>

      <section className={styles.treePanel} aria-labelledby="tree-heading">
        <div className={styles.panelHeading}>
          <div>
            <span className={styles.eyebrow}>Output</span>
            <h2 id="tree-heading">Concrete tree</h2>
          </div>
          <div className={styles.legend} aria-label="Tree legend">
            <span>
              <i className={styles.tokenDot} /> token
            </span>
            <span>
              <i className={styles.missingDot} /> missing
            </span>
          </div>
        </div>
        <div className={styles.treeScroll}>
          <ol className={styles.treeRoot} aria-label="Concrete syntax tree">
            <TreeElement element={result.root} source={result.lexical.source} />
          </ol>
        </div>
      </section>
    </div>
  )
}
