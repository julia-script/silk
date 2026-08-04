'use client'

import { Lexer, Parser, SourceFile, SyntaxTree } from '@silk-effect/compiler'
import { useMemo, useState } from 'react'
import styles from './syntax-inspector.module.css'

const sourceId = 'memory://docs/syntax-inspector.silk'
const acceptedSource = 'pub fn main() -> I32 { return 42 }'

const examples = [
  { label: 'Valid', source: acceptedSource },
  { label: 'Missing }', source: 'pub fn main() -> I32 { return 42' },
  { label: 'Unexpected @', source: 'pub fn @ main() -> I32 { return 42 }' },
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

export function SyntaxInspector() {
  const [text, setText] = useState(acceptedSource)
  const result = useMemo(() => {
    const source = SourceFile.make(sourceId, encoder.encode(text))
    return Parser.parse(Lexer.lex(source))
  }, [text])

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
        </dl>

        <div className={styles.diagnostics}>
          <DiagnosticList title="Lexer" diagnostics={result.lexical.diagnostics} />
          <DiagnosticList title="Parser" diagnostics={result.diagnostics} />
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
