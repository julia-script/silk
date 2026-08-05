'use client'

import { Analysis, Diagnostic, SyntaxTree } from '@silk-effect/compiler'
import type { BootstrapEvaluation, Elaboration, SourceFile } from '@silk-effect/compiler'
import type { Hir, SourceSpan, SyntaxFile } from '@silk-effect/compiler'
import { useMemo, useState } from 'react'
import {
  projectDataFlow,
  type FlowEdge,
  type FlowGroup,
  type FlowNode,
} from './flow-model'
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
    label: 'Nested flow · complete',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`,
  },
  {
    label: 'Nested flow · siblings',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return choose(identity(1), identity(2)) }`,
  },
  {
    label: 'Nested flow · unavailable',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn uncertain(value: Mystery) -> I32 { return 0 }
pub fn main() -> I32 { return identity(uncertain(42)) }`,
  },
  {
    label: 'Nested flow · wrong arity',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity()) }`,
  },
  {
    label: 'Damaged nested call',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(@)) }`,
  },
  {
    label: 'Nested evaluation · completed',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`,
  },
  {
    label: 'Nested flow · inner blocked',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return choose(identity(1), missing(2)) }`,
  },
  {
    label: 'Nested flow · cycle',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(main()) }`,
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

export function TreeElement({
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

export interface DiagnosticEntry {
  readonly diagnostic: Diagnostic.Diagnostic
  readonly identity: Diagnostic.Identity
}

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

const hirSpanLabel = (span: {
  readonly start: number
  readonly end: number
}): string => `[${span.start}, ${span.end})`

const hirIdentityLabel = (declaration: Hir.HirFunction['declaration']): string => {
  switch (declaration.canonical._tag) {
    case 'Canonical':
      return declaration.canonical.id.name
    case 'Duplicate':
      return `duplicate of ${declaration.canonical.original.name}`
    case 'Unidentified':
      return 'unidentified'
  }
}

const hirContractLabel = (contract: Hir.ContractFact): string =>
  contract._tag === 'Contract'
    ? `(${contract.parameters.join(', ')}) -> ${contract.result}`
    : 'contract unavailable'

function HirExpressionRows({
  expression,
  depth,
}: {
  readonly expression: Hir.Expression
  readonly depth: number
}) {
  const reveal =
    expression._tag === 'Unavailable'
      ? `unavailable ${hirSpanLabel(expression.span)}`
      : `${expression.type} ${hirSpanLabel(expression.span)}`
  const label =
    expression._tag === 'IntegerLiteral'
      ? `literal ${expression.value}`
      : expression._tag === 'ParameterReference'
        ? `param fn${expression.parameter.function.ordinal}.p${expression.parameter.ordinal}`
        : expression._tag === 'BindingReference'
          ? `binding fn${expression.binding.function.ordinal}.b${expression.binding.ordinal}`
          : expression._tag === 'Move'
            ? 'move'
            : expression._tag === 'Call'
              ? `call ${expression.target.name}`
              : expression._tag === 'BuiltinCall'
                ? `builtin I32.${expression.operation}`
                : 'unavailable'
  return (
    <>
      <li style={{ paddingLeft: `${depth * 16}px` }}>
        <div>
          <code tabIndex={0} title={reveal} aria-label={`${label} : ${reveal}`}>
            {label}
          </code>
        </div>
      </li>
      {expression._tag === 'Call' || expression._tag === 'BuiltinCall'
        ? expression.arguments.map((argument, index) => (
            <HirExpressionRows
              key={`${argument._tag}-${argument.span.start}-${index}`}
              expression={argument}
              depth={depth + 1}
            />
          ))
        : null}
      {expression._tag === 'Move' ? (
        <HirExpressionRows expression={expression.subject} depth={depth + 1} />
      ) : null}
    </>
  )
}

function HirStatementRows({ statement }: { readonly statement: Hir.Statement }) {
  if (statement._tag === 'Bind') {
    return (
      <>
        <li>
          <div>
            <code>
              bind b{statement.binding.ordinal} {statement.name ?? '∅'}
            </code>
            <span>{hirSpanLabel(statement.span)}</span>
          </div>
        </li>
        <HirExpressionRows expression={statement.initializer} depth={1} />
      </>
    )
  }
  if (statement._tag === 'If') {
    return (
      <>
        <li>
          <div>
            <code>if</code>
            <span>{hirSpanLabel(statement.span)}</span>
          </div>
        </li>
        <HirExpressionRows expression={statement.condition} depth={1} />
        <li>
          <div>
            <code>then</code>
          </div>
        </li>
        {statement.taken.map((inner, index) => (
          <HirStatementRows key={`taken-${inner.span.start}-${index}`} statement={inner} />
        ))}
        {statement.otherwise.length === 0 ? null : (
          <>
            <li>
              <div>
                <code>else</code>
              </div>
            </li>
            {statement.otherwise.map((inner, index) => (
              <HirStatementRows key={`otherwise-${inner.span.start}-${index}`} statement={inner} />
            ))}
          </>
        )}
      </>
    )
  }
  return (
    <>
      <li>
        <div>
          <code>return</code>
          <span>{hirSpanLabel(statement.span)}</span>
        </div>
      </li>
      <HirExpressionRows expression={statement.expression} depth={1} />
    </>
  )
}

export function HirPanel({ hir }: { readonly hir: Hir.Module }) {
  return (
    <section className={styles.diagnosticGroup} aria-labelledby="hir-view">
      <div className={styles.diagnosticHeading}>
        <h3 id="hir-view">HIR</h3>
        <span>{hir.functions.length}</span>
      </div>
      {hir.functions.length === 0 ? (
        <p className={styles.emptyState}>No functions</p>
      ) : (
        <ul className={styles.diagnosticList} aria-label="Elaborated HIR functions">
          {hir.functions.map((fn) => (
            <li key={`hir-${fn.declaration.id.ordinal}`}>
              <div>
                <code>{hirIdentityLabel(fn.declaration)}</code>
                <span>{hirContractLabel(fn.contract)}</span>
              </div>
              <ul className={styles.diagnosticList} aria-label={`HIR body of function ${fn.declaration.id.ordinal}`}>
                {fn.statements.map((statement, index) => (
                  <HirStatementRows
                    key={`statement-${statement.span.start}-${index}`}
                    statement={statement}
                  />
                ))}
              </ul>
            </li>
          ))}
        </ul>
      )}
    </section>
  )
}

/**
 * The lossless concrete syntax tree, with the legend that tells a real token from a missing one
 * the parser recovered over. Recovery is the point of the tree being lossless, so the legend
 * travels with it rather than living in a panel heading somewhere else.
 */
export function ConcreteTree({ syntax }: { readonly syntax: SyntaxFile.SyntaxFile }) {
  return (
    <section aria-labelledby="concrete-tree">
      <div className={styles.diagnosticHeading}>
        <h3 id="concrete-tree">Concrete tree</h3>
        <div className={styles.legend} aria-label="Tree legend">
          <span>
            <i className={styles.tokenDot} /> token
          </span>
          <span>
            <i className={styles.missingDot} /> missing
          </span>
        </div>
      </div>
      <ol className={styles.treeRoot} aria-label="Concrete syntax tree">
        <TreeElement element={syntax.root} source={syntax.source} />
      </ol>
    </section>
  )
}

export function TokenStream({ syntax }: { readonly syntax: SyntaxFile.SyntaxFile }) {
  return (
    <section className={styles.diagnosticGroup} aria-labelledby="token-stream">
      <div className={styles.diagnosticHeading}>
        <h3 id="token-stream">Token stream</h3>
        <span>{syntax.tokens.length}</span>
      </div>
      <ul className={styles.diagnosticList} aria-label="SyntaxFile token stream">
        {syntax.tokens.map((token, index) => (
          <li key={`${token.kind}-${token.span.start}-${index}`}>
            <div>
              <code>{token.kind}</code>
              <span>
                [{token.span.start}, {token.span.end})
              </span>
            </div>
          </li>
        ))}
      </ul>
    </section>
  )
}

export function DiagnosticPanel({
  entries,
  selected,
  onSelect,
}: {
  readonly entries: ReadonlyArray<DiagnosticEntry>
  readonly selected: number | undefined
  readonly onSelect: (index: number | undefined) => void
}) {
  return (
    <section className={styles.diagnosticGroup} aria-labelledby="unified-diagnostics">
      <div className={styles.diagnosticHeading}>
        <h3 id="unified-diagnostics">Diagnostics</h3>
        <span>{entries.length}</span>
      </div>
      {entries.length === 0 ? (
        <p className={styles.emptyState}>No diagnostics</p>
      ) : (
        <ul className={styles.diagnosticList}>
          {entries.map((entry, index) => {
            const diagnostic = entry.diagnostic
            const cause = diagnostic.cause
            const origin =
              cause === undefined
                ? undefined
                : entries.find((candidate) => Diagnostic.identityEquals(candidate.identity, cause))
            return (
              <li
                key={`${diagnostic.phase}-${diagnostic.code}-${diagnostic.span.start}-${index}`}
              >
                <div>
                  <code>{diagnostic.code}</code>
                  <span>{diagnostic.phase}</span>
                  <span>{diagnostic.severity}</span>
                  <span>
                    [{diagnostic.span.start}, {diagnostic.span.end})
                  </span>
                </div>
                <p>{diagnostic.message}</p>
                {cause === undefined ? null : (
                  <button
                    type="button"
                    onClick={() => onSelect(selected === index ? undefined : index)}
                  >
                    {selected === index ? 'Hide cause' : 'Show cause'}
                  </button>
                )}
                {selected === index && cause !== undefined ? (
                  <p>
                    Caused by <code>{cause.code}</code> at [{cause.span.start}, {cause.span.end})
                    {origin === undefined ? '' : `: ${origin.diagnostic.message}`}
                  </p>
                ) : null}
              </li>
            )
          })}
        </ul>
      )}
    </section>
  )
}

type CallReturnedExpression = Extract<
  Elaboration.ExpressionFact,
  { readonly _tag: 'Call' }
>

type IdentifierExpression = Elaboration.IdentifierExpressionFact

const declarationLabel = (declaration: Elaboration.DeclarationFact): string =>
  declaration.name._tag === 'Present' ? declaration.name.spelling : 'Unavailable name'

const parameterLabel = (parameter: Elaboration.ParameterFact): string =>
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

function ParameterFacts({ declaration }: { readonly declaration: Elaboration.DeclarationFact }) {
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
              ? Analysis.parameterLookup(declaration, name.spelling)
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
  nested = false,
}: {
  readonly caller: Elaboration.DeclarationFact
  readonly returned: CallReturnedExpression
  readonly nested?: boolean
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
          : 'A mapped argument or parameter type is unavailable.'

  return (
    <section
      className={styles.callRelationship}
      aria-label={`Call relationship from ${callerName} to ${targetName}`}
    >
      <div className={styles.relationshipHeading}>
        <span>{nested ? 'Nested resolution' : 'Semantic resolution'}</span>
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
                  : expression._tag === 'Boolean'
                    ? String(expression.value)
                    : expression._tag === 'Call'
                      ? expression.reference._tag === 'Unavailable'
                        ? 'Unavailable nested call'
                        : `${expression.reference.spelling}(…)`
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
                  {expression._tag === 'Call' ? (
                    <CallRelationship caller={caller} returned={expression} nested />
                  ) : null}
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

type FlowItem = FlowGroup | FlowNode | FlowEdge

const flowSpanLabel = (item: FlowItem): string =>
  `${item.span.sourceId}[${item.span.start}, ${item.span.end})`

const evaluationLabel = (item: FlowItem): string | undefined => {
  const evidence = item.evaluation
  if (evidence === undefined) return undefined
  return evidence.value === undefined
    ? `evaluated #${evidence.order}`
    : `evaluated #${evidence.order} · value ${evidence.value}`
}

export function DataFlow({
  analysis,
  outcome,
  selectedId,
  onSelect,
}: {
  readonly analysis: Elaboration.Result
  readonly outcome?: BootstrapEvaluation.Outcome
  readonly selectedId: string | undefined
  readonly onSelect: (id: string | undefined) => void
}) {
  const flow = useMemo(() => projectDataFlow(analysis, outcome), [analysis, outcome])
  const items: ReadonlyArray<FlowItem> = [...flow.groups, ...flow.nodes, ...flow.edges]
  const selected = items.find((item) => item.id === selectedId)
  const source = analysis.syntax.source
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
      <div className={styles.flowLegend} aria-label="Value-flow legend">
        <span data-layer="Semantic">Semantic relationship</span>
        <span data-layer="Evaluated">
          {flow.mode === 'Evaluated' ? 'Evaluated order + exact value' : 'Evaluated overlay not run'}
        </span>
      </div>

      {flow.status === 'Empty' ? (
        <p className={styles.emptyState}>Choose a call preset to inspect a value path.</p>
      ) : (
        <>
          <ol className={styles.flowGroups} aria-label="Nested value-flow call groups">
            {flow.groups.map((group) => {
              const groupNodes = flow.nodes.filter((item) => item.groupId === group.id)
              const groupEdges = flow.edges.filter((item) => item.groupId === group.id)
              return (
                <li key={group.id} data-depth={Math.min(group.depth, 3)}>
                  <button
                    type="button"
                    className={styles.flowGroupHeading}
                    data-state={group.state}
                    data-layer={group.evaluation === undefined ? 'Semantic' : 'Evaluated'}
                    aria-pressed={selectedId === group.id}
                    onClick={() => onSelect(selectedId === group.id ? undefined : group.id)}
                  >
                    <span>
                      depth {group.depth} · argument ordinal {group.ordinal}
                    </span>
                    <strong>{group.label}</strong>
                    <small>{evaluationLabel(group) ?? 'semantic only'}</small>
                    <code>{flowSpanLabel(group)}</code>
                  </button>

                  <div className={styles.flowLane} role="group" aria-label="Navigable value-flow nodes">
                    {groupNodes.map((flowNode, index) => (
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
                          data-layer={flowNode.evaluation === undefined ? flowNode.layer : 'Evaluated'}
                          aria-pressed={selectedId === flowNode.id}
                          onClick={() =>
                            onSelect(selectedId === flowNode.id ? undefined : flowNode.id)
                          }
                        >
                          <span>
                            {flowNode.kind} · depth {flowNode.depth}
                            {flowNode.ordinal === undefined ? '' : ` · ordinal ${flowNode.ordinal}`}
                          </span>
                          <strong>{flowNode.label}</strong>
                          <small>
                            {flowNode.state} · {evaluationLabel(flowNode) ?? 'semantic only'}
                          </small>
                          <code>[{flowNode.span.start}, {flowNode.span.end})</code>
                        </button>
                      </div>
                    ))}
                  </div>

                  <ol
                    className={styles.flowRelationships}
                    aria-label="Ordered value-flow relationships"
                  >
                    {groupEdges.map((flowEdge) => {
                      const from = flow.nodes.find((flowNode) => flowNode.id === flowEdge.from)
                      const to = flow.nodes.find((flowNode) => flowNode.id === flowEdge.to)
                      return (
                        <li key={flowEdge.id}>
                          <button
                            type="button"
                            data-state={flowEdge.state}
                            data-layer={flowEdge.evaluation === undefined ? flowEdge.layer : 'Evaluated'}
                            aria-pressed={selectedId === flowEdge.id}
                            onClick={() =>
                              onSelect(selectedId === flowEdge.id ? undefined : flowEdge.id)
                            }
                          >
                            <strong>
                              {from?.label ?? flowEdge.from} {flowEdge.label}{' '}
                              {to?.label ?? flowEdge.to}
                            </strong>
                            <span>
                              depth {flowEdge.depth} · {flowEdge.state} ·{' '}
                              {evaluationLabel(flowEdge) ?? 'semantic only'}
                            </span>
                            <code>{flowSpanLabel(flowEdge)}</code>
                          </button>
                        </li>
                      )
                    })}
                  </ol>
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
            <strong>
              Selected{' '}
              {selected._tag === 'FlowGroup'
                ? 'call group'
                : selected._tag === 'FlowNode'
                  ? selected.kind
                  : 'relationship'}
            </strong>
            <code>{flowSpanLabel(selected)}</code>
          </div>
          <p>{selected._tag === 'FlowEdge' ? selected.label : selected.detail}</p>
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
    case 'InvalidMir':
      return `MIR verification found ${reason.violations.length} violations.`
    case 'UnavailableEntry':
      switch (reason.reason) {
        case 'MissingEntry':
          return 'No top-level declaration named main is available.'
        case 'AmbiguousEntry':
          return 'Multiple declarations named main are available; none was selected.'
        case 'ParameterizedEntry':
          return 'main has parameters; bootstrap entry requires exactly zero.'
        case 'UntypedEntry':
          return 'main does not have an available resolved I32 return type.'
      }
      return 'The entry is unavailable.'
    case 'Trap':
      return `${reason.function.name} trapped: ${reason.reason}.`
    case 'MissingFunction':
      return `No lowered function is available for ${reason.target.name}.`
    case 'RecursiveCycle':
      return `Recursive cycle: ${reason.cycle.map((id) => id.name).join(' → ')}.`
  }
}

const blockedSpan = (
  reason: BootstrapEvaluation.BlockedReason,
): SourceSpan.SourceSpan | undefined => {
  switch (reason._tag) {
    case 'UnavailableEntry':
      return undefined
    case 'Trap':
      return reason.span
    case 'MissingFunction':
      return reason.span
    case 'RecursiveCycle':
      return reason.closingCallSpan
  }
}

interface TraceRow {
  readonly event: BootstrapEvaluation.TraceEvent
  readonly depth: number
}

const traceRows = (
  trace: ReadonlyArray<BootstrapEvaluation.TraceEvent>,
): ReadonlyArray<TraceRow> => {
  const openCalls: Array<string> = []
  return trace.map((event) => {
    const row = Object.freeze({ event, depth: openCalls.length })
    if (event._tag === 'Call') {
      openCalls.push(`${event.target.module}.${event.target.name}`)
    } else if (event._tag === 'Return') {
      const current = openCalls.at(-1)
      if (current !== undefined && current === `${event.function.module}.${event.function.name}`) {
        openCalls.pop()
      }
    }
    return row
  })
}

const traceLabel = (event: BootstrapEvaluation.TraceEvent): string => {
  switch (event._tag) {
    case 'Entry':
      return `Enter ${event.function.name}`
    case 'Call':
      return `${event.caller.name} calls ${event.target.name}`
    case 'Binding':
      return event.fromCall
        ? `Nested result ${event.value.value} from [${event.callSpan.start}, ${event.callSpan.end}) binds to ${event.target.name} parameter #${event.parameterOrdinal}`
        : `Argument #${event.argumentOrdinal} binds ${event.value.value} to ${event.target.name} parameter #${event.parameterOrdinal}`
    case 'Return':
      return `${event.function.name} returns ${event.value.value}`
  }
}

export function EvaluationPanel({
  outcome,
  onEvaluate,
}: {
  readonly outcome: BootstrapEvaluation.Outcome | undefined
  readonly onEvaluate: () => void
}) {
  const rows = outcome === undefined ? [] : traceRows(outcome.trace)
  const endpoint = outcome?._tag === 'Blocked' ? blockedSpan(outcome.reason) : undefined
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
            {rows.map(({ event, depth }, index) => (
              <li key={`${event._tag}-${event.span.start}-${index}`} data-depth={Math.min(depth, 3)}>
                <span>{index + 1}</span>
                <div>
                  <strong>{traceLabel(event)}</strong>
                  <code>
                    {event.span.sourceId}[{event.span.start}, {event.span.end})
                  </code>
                </div>
                <small>
                  depth {depth} · {event._tag}
                </small>
              </li>
            ))}
          </ol>
          {outcome._tag === 'Blocked' ? (
            <aside className={styles.evaluationEndpoint} aria-label="Blocked evaluation endpoint">
              <span>Stopped at</span>
              <strong>{outcome.reason._tag}</strong>
              {endpoint === undefined ? null : (
                <code>
                  {endpoint.sourceId}[{endpoint.start}, {endpoint.end})
                </code>
              )}
            </aside>
          ) : null}
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
  readonly analysis: Elaboration.Result
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
    Analysis.declarationLookup(analysis, spelling),
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

      <DataFlow
        analysis={analysis}
        outcome={evaluation}
        selectedId={selectedFlowId}
        onSelect={onSelectFlow}
      />
      <EvaluationPanel outcome={evaluation} onEvaluate={onEvaluate} />

      <div className={styles.functionList} aria-label="Collected function facts">
        {analysis.functions.map((fact) => {
          const declaration = fact.declaration
          const name = declaration.name
          const returnType = declaration.returnType
          const returned = fact.returnedExpression
          const nameLabel = name._tag === 'Present' ? name.spelling : 'Unavailable name'
          const collectIdentifiers = (
            expression: Elaboration.ExpressionFact,
          ): ReadonlyArray<IdentifierExpression> =>
            expression._tag === 'Identifier'
              ? [expression]
              : expression._tag === 'Call'
                ? expression.arguments.flatMap((argument) =>
                    collectIdentifiers(argument.expression),
                  )
                : []
          const identifierExpressions = collectIdentifiers(returned)

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
                  ) : returned._tag === 'Boolean' ? (
                    <dd>
                      <strong>{String(returned.value)}</strong>
                      <span>
                        Boolean ·{' '}
                        {returned.type._tag === 'Available' ? returned.type.type : 'Unavailable type'}
                      </span>
                      <small>{spanLabel(returned.syntax)}</small>
                    </dd>
                  ) : returned._tag === 'Identifier' || returned._tag === 'Move' ? (
                    <dd>
                      <strong>
                        {returned.reference._tag === 'Unavailable'
                          ? 'Unavailable'
                          : returned.reference.spelling}
                      </strong>
                      <span>
                        {returned._tag} · {returned.reference._tag} ·{' '}
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
        Parameters have function-local identities, declared types, closed lookup, and exact
        reference links. Calls now expose recursive expression facts, ordered argument identities,
        positional mappings, contracts, and exact provenance at every parsed depth. Evaluation now
        follows those recursive facts left to right, producing nested trace groups and exact blocked
        endpoints from the authoritative event sequence. Nested lanes always record semantic
        relationships; the violet evaluated overlay appears only after explicit evaluation and only
        for order, values, bindings, returns, and endpoints present in that trace. Stopped and
        branched lanes preserve their exact source ranges without claiming an enclosing result.
        Every call group, node, and relationship is keyboard-selectable and described in the same
        ordered structure. Conversions, general scope graphs, semantic AST, HIR, and code generation
        do not exist yet.
      </p>
    </section>
  )
}

export function SyntaxInspector() {
  const [text, setText] = useState(acceptedSource)
  const [selectedFlowId, setSelectedFlowId] = useState<string>()
  const [selectedDiagnostic, setSelectedDiagnostic] = useState<number>()
  const [evaluation, setEvaluation] = useState<BootstrapEvaluation.Outcome>()
  const snapshot = useMemo(() => Analysis.ofSource(sourceId, encoder.encode(text)), [text])
  const analysis = Analysis.rootAnalysis(snapshot)
  const result = analysis.syntax

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
            setSelectedDiagnostic(undefined)
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
                setSelectedDiagnostic(undefined)
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
            <dd>{result.source.bytes.length}</dd>
          </div>
          <div>
            <dt>Tokens</dt>
            <dd>{result.tokens.length}</dd>
          </div>
          <div>
            <dt>Lexer</dt>
            <dd>{result.lexicalDiagnostics.length}</dd>
          </div>
          <div>
            <dt>Parser</dt>
            <dd>{result.parserDiagnostics.length}</dd>
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
          onEvaluate={() => setEvaluation(Analysis.evaluate(snapshot))}
        />

        <div className={styles.diagnostics}>
          <DiagnosticPanel
            entries={diagnosticEntries(
              result.lexicalDiagnostics,
              result.parserDiagnostics,
              analysis.diagnostics,
            )}
            selected={selectedDiagnostic}
            onSelect={setSelectedDiagnostic}
          />
          <TokenStream syntax={result} />
          <HirPanel hir={analysis.hir} />
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
            <TreeElement element={result.root} source={result.source} />
          </ol>
        </div>
      </section>
    </div>
  )
}
