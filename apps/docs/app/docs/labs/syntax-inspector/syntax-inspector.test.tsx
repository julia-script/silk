import {
  BootstrapEvaluation,
  Lexer,
  Parser,
  SemanticAnalysis,
  SourceFile,
} from '@silk-effect/compiler'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it } from 'vitest'
import { DataFlow, EvaluationPanel, SyntaxInspector } from './syntax-inspector'

const encoder = new TextEncoder()

const analyze = (text: string): SemanticAnalysis.Result =>
  SemanticAnalysis.analyze(
    Parser.parse(Lexer.lex(SourceFile.make('memory://component-flow.silk', encoder.encode(text)))),
  )

const completeSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(42) }`

const renderFlow = (source: string, selectedId?: string): string =>
  renderToStaticMarkup(
    <DataFlow analysis={analyze(source)} selectedId={selectedId} onSelect={() => undefined} />,
  )

describe('DataFlow', () => {
  it('renders the complete flow as accessible nodes and ordered relationships', () => {
    const markup = renderFlow(completeSource)

    expect(markup).toContain('aria-label="Navigable value-flow nodes"')
    expect(markup).toContain('aria-label="Ordered value-flow relationships"')
    expect(markup).toContain('Argument #0: 42')
    expect(markup).toContain('identity.value')
    expect(markup).toContain('binds positionally to')
    expect(markup).toContain('is returned by')
  })

  it('renders a selected item with its exact marked source slice', () => {
    const markup = renderFlow(completeSource, 'argument-0')

    expect(markup).toContain('aria-pressed="true"')
    expect(markup).toContain('aria-label="Selected flow source"')
    expect(markup).toContain('<mark>42</mark>')
  })

  it('recomputes incomplete input without inventing a successful result edge', () => {
    const incomplete = renderFlow('pub fn main() -> I32 { return missing(42) }')

    expect(incomplete).toContain('Incomplete')
    expect(incomplete).toContain('Flow stops: no unique target')
    expect(incomplete).not.toContain('binds positionally to')
    expect(incomplete).not.toContain('is returned by')
  })
})

describe('EvaluationPanel', () => {
  it('renders an exact completed result and ordered accessible trace', () => {
    const outcome = BootstrapEvaluation.evaluate(analyze(completeSource))
    const markup = renderToStaticMarkup(
      <EvaluationPanel outcome={outcome} onEvaluate={() => undefined} />,
    )

    expect(markup).toContain('Completed')
    expect(markup).toContain('42 <code>I32</code>')
    expect(markup).toContain('aria-label="Ordered bootstrap evaluation trace"')
    expect(markup).toContain('main calls identity')
    expect(markup).toContain('identity returns 42')
  })

  it('renders a bounded recursive cycle with its partial trace', () => {
    const outcome = BootstrapEvaluation.evaluate(
      analyze('pub fn main() -> I32 { return main() }'),
    )
    const markup = renderToStaticMarkup(
      <EvaluationPanel outcome={outcome} onEvaluate={() => undefined} />,
    )

    expect(markup).toContain('Blocked')
    expect(markup).toContain('RecursiveCycle')
    expect(markup).toContain('Recursive cycle: main → main.')
    expect(markup).toContain('main calls main')
  })
})

describe('SyntaxInspector', () => {
  it('offers valid and damaged nested-call presets without advertising semantic completion', () => {
    const markup = renderToStaticMarkup(<SyntaxInspector />)

    expect(markup).toContain('Nested call · syntax only')
    expect(markup).toContain('Damaged nested call')
    expect(markup).toContain('semantic AST, HIR, and code generation do not exist yet')
  })
})
