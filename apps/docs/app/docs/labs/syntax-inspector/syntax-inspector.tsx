'use client'

import { Lexer, Parser, SemanticAnalysis, SourceFile, SyntaxTree } from '@silk-effect/compiler'
import { useMemo, useState } from 'react'
import styles from './syntax-inspector.module.css'

const sourceId = 'memory://docs/syntax-inspector.silk'
const acceptedSource = 'pub fn main() -> I32 { return 42 }'

const examples = [
  { label: 'Valid', source: acceptedSource },
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
  { label: 'Self call', source: 'pub fn main() -> I32 { return main() }' },
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
    label: 'Identity syntax',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(42) }`,
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

const declarationLabel = (declaration: SemanticAnalysis.DeclarationFact): string =>
  declaration.name._tag === 'Present' ? declaration.name.spelling : 'Unavailable name'

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
  const argumentsList = returned.syntax.children.find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'ArgumentList',
  )
  const argumentCount =
    argumentsList?.children.filter(
      (element) =>
        SyntaxTree.isNode(element) &&
        (element.kind === 'IntegerLiteralExpression' || element.kind === 'IdentifierExpression'),
    ).length ?? 0

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
            <code>{argumentCount}</code>
            <span>Preserved · unchecked</span>
          </dd>
        </div>
      </dl>
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

function SemanticFacts({ analysis }: { readonly analysis: SemanticAnalysis.Result }) {
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

      <div className={styles.functionList} aria-label="Collected function facts">
        {analysis.functions.map((fact) => {
          const declaration = fact.declaration
          const name = declaration.name
          const returnType = declaration.returnType
          const returned = fact.returnedExpression
          const nameLabel = name._tag === 'Present' ? name.spelling : 'Unavailable name'

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
                      <strong>Unavailable</strong>
                      <span>Identifier · resolution deferred</span>
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
        Parameters and arguments are concrete syntax in this slice: counts are available, while
        local identifier resolution, argument binding, and argument checking are deliberately
        deferred. These arrows record top-level call-name resolution, not execution order. Scope
        graphs, semantic AST, HIR, and code generation do not exist yet.
      </p>
    </section>
  )
}

export function SyntaxInspector() {
  const [text, setText] = useState(acceptedSource)
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
          onChange={(event) => setText(event.target.value)}
          spellCheck={false}
          autoCapitalize="off"
          autoCorrect="off"
        />

        <div className={styles.exampleBar} aria-label="Source examples">
          {examples.map((example) => (
            <button key={example.label} type="button" onClick={() => setText(example.source)}>
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

        <SemanticFacts analysis={analysis} />

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
