import { Analysis, Diagnostic } from '@silk-effect/compiler'
import type { Elaboration } from '@silk-effect/compiler'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it } from 'vitest'
import { projectDataFlow } from './flow-model'
import {
  DataFlow,
  DiagnosticPanel,
  diagnosticEntries,
  EvaluationPanel,
  HirPanel,
  SyntaxInspector,
  TokenStream,
} from './syntax-inspector'

const encoder = new TextEncoder()

const snap = (text: string): Analysis.Snapshot =>
  Analysis.ofSource('memory://component-flow.silk', encoder.encode(text))

const analyze = (text: string): Elaboration.Result => Analysis.rootAnalysis(snap(text))

const completeSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(42) }`

const renderFlow = (
  source: string,
  selectedId?: string,
  evaluate = false,
): string => {
  const snapshot = snap(source)
  const analysis = Analysis.rootAnalysis(snapshot)
  return renderToStaticMarkup(
    <DataFlow
      analysis={analysis}
      outcome={evaluate ? Analysis.evaluate(snapshot) : undefined}
      selectedId={selectedId}
      onSelect={() => undefined}
    />,
  )
}

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
    const analysis = analyze(completeSource)
    const selectedId = projectDataFlow(analysis).nodes.find((item) => item.kind === 'Argument')?.id
    const markup = renderToStaticMarkup(
      <DataFlow analysis={analysis} selectedId={selectedId} onSelect={() => undefined} />,
    )

    expect(markup).toContain('aria-pressed="true"')
    expect(markup).toContain('aria-label="Selected flow source"')
    expect(markup).toContain('<mark>42</mark>')
  })

  it('renders nested groups and a trace-backed overlay from the same ordered structure', () => {
    const markup = renderFlow(
      `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`,
      undefined,
      true,
    )

    expect(markup).toContain('aria-label="Nested value-flow call groups"')
    expect(markup).toContain('depth 1 · argument ordinal 0')
    expect(markup).toContain('supplies nested result to')
    expect(markup).toContain('Evaluated order + exact value')
    expect(markup).toContain('value 42')
  })

  it('makes call groups, edges, and evaluated terminals source-selectable', () => {
    const snapshot = snap('pub fn main() -> I32 { return missing(42) }')
    const analysis = Analysis.rootAnalysis(snapshot)
    const outcome = Analysis.evaluate(snapshot)
    const flow = projectDataFlow(analysis, outcome)
    const selectableIds = [flow.groups.at(0)?.id, flow.edges.at(0)?.id, flow.nodes.at(-1)?.id]

    for (const selectedId of selectableIds) {
      const markup = renderToStaticMarkup(
        <DataFlow
          analysis={analysis}
          outcome={outcome}
          selectedId={selectedId}
          onSelect={() => undefined}
        />,
      )
      expect(markup).toContain('aria-pressed="true"')
      expect(markup).toContain('aria-label="Selected flow source"')
      expect(markup).toContain('<mark> missing(42)</mark>')
    }
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
    const outcome = Analysis.evaluate(snap(completeSource))
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
    const outcome = Analysis.evaluate(snap('pub fn main() -> I32 { return main() }'),
    )
    const markup = renderToStaticMarkup(
      <EvaluationPanel outcome={outcome} onEvaluate={() => undefined} />,
    )

    expect(markup).toContain('Blocked')
    expect(markup).toContain('RecursiveCycle')
    expect(markup).toContain('Recursive cycle: main → main.')
    expect(markup).toContain('main calls main')
  })

  it('renders completed nested evaluation with call depth and result binding', () => {
    const outcome = Analysis.evaluate(snap(`pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`),
    )
    const markup = renderToStaticMarkup(
      <EvaluationPanel outcome={outcome} onEvaluate={() => undefined} />,
    )

    expect(markup).toContain('Completed')
    expect(markup).toContain('Nested result 42 from')
    expect(markup).toContain('depth 1 · Binding')
    expect(markup).toContain('main calls identity')
  })

  it('renders an inner blocked endpoint without an enclosing nested-result binding', () => {
    const outcome = Analysis.evaluate(snap(`pub fn identity(value: I32) -> I32 { return value }
pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return choose(identity(1), missing(2)) }`),
    )
    const markup = renderToStaticMarkup(
      <EvaluationPanel outcome={outcome} onEvaluate={() => undefined} />,
    )

    expect(markup).toContain('main trapped: unavailable body.')
    expect(markup).toContain('aria-label="Blocked evaluation endpoint"')
    expect(markup).not.toContain('Nested result')
  })
})

describe('DiagnosticPanel', () => {
  const crossPhaseSource = '@ pub fn main( -> Mystery { return 42 }'

  const crossPhaseEntries = () => {
    const analysis = analyze(crossPhaseSource)
    return diagnosticEntries(
      analysis.syntax.lexicalDiagnostics,
      analysis.syntax.parserDiagnostics,
      analysis.diagnostics,
    )
  }

  it('lists every phase in one deterministic driver-ordered stream', () => {
    const entries = crossPhaseEntries()

    expect(
      entries.map((entry) => ({ phase: entry.diagnostic.phase, code: entry.diagnostic.code })),
    ).toEqual([
      { phase: 'lexical', code: 'LEX0001' },
      { phase: 'parser', code: 'PAR0002' },
      { phase: 'parser', code: 'PAR0001' },
      { phase: 'semantic', code: 'SEM0001' },
    ])
    expect(entries).toEqual(crossPhaseEntries())
  })

  it('labels each entry with phase, code, severity, and primary span', () => {
    const entries = crossPhaseEntries()
    const markup = renderToStaticMarkup(
      <DiagnosticPanel entries={entries} selected={undefined} onSelect={() => undefined} />,
    )

    for (const { diagnostic } of entries) {
      expect(markup).toContain(diagnostic.code)
      expect(markup).toContain(`<span>${diagnostic.phase}</span>`)
      expect(markup).toContain(`[${diagnostic.span.start}, ${diagnostic.span.end})`)
    }
    expect(markup).toContain('<span>error</span>')
    expect(markup).not.toContain('Show cause')
  })

  it('reveals the originating diagnostic and span for a selected caused entry', () => {
    const analysis = analyze('pub fn main() -> I32 { return missing() }')
    const origin =
      analysis.diagnostics[0] ?? (() => {
        throw new Error('expected an unknown-function diagnostic')
      })()
    const caused: Diagnostic.Diagnostic = Object.freeze({
      ...Diagnostic.unknownType('Dependent', origin.span),
      cause: Diagnostic.identity(origin),
    })
    const entries = diagnosticEntries(analysis.diagnostics, [caused])

    const unselected = renderToStaticMarkup(
      <DiagnosticPanel entries={entries} selected={undefined} onSelect={() => undefined} />,
    )
    expect(unselected).toContain('Show cause')
    expect(unselected).not.toContain('Caused by')

    const selectedIndex = entries.findIndex((entry) => entry.diagnostic.cause !== undefined)
    const selected = renderToStaticMarkup(
      <DiagnosticPanel entries={entries} selected={selectedIndex} onSelect={() => undefined} />,
    )
    expect(selected).toContain('Hide cause')
    expect(selected).toContain(
      `Caused by <code>${origin.code}</code> at [${origin.span.start}, ${origin.span.end})`,
    )
    expect(selected).toContain(origin.message)
  })
})

describe('TokenStream', () => {
  it('lists every artifact token in source order including trivia', () => {
    const analysis = analyze('// note\npub fn main() -> I32 { return 42 }')
    const markup = renderToStaticMarkup(<TokenStream syntax={analysis.syntax} />)

    expect(markup).toContain('aria-label="SyntaxFile token stream"')
    expect(markup).toContain('LineComment')
    expect(markup).toContain('Whitespace')
    expect(markup).toContain('PubKeyword')
    expect(markup).toContain('EndOfFile')
    expect(markup).toContain(`<span>${analysis.syntax.tokens.length}</span>`)
  })

  it('keeps invalid tokens visible in the stream', () => {
    const analysis = analyze('pub fn main() -> I32 { return @ 42 }')
    const markup = renderToStaticMarkup(<TokenStream syntax={analysis.syntax} />)

    expect(markup).toContain('Invalid')
  })
})

describe('HirPanel', () => {
  it('lists typed functions with contracts and canonical call targets', () => {
    const analysis = analyze(completeSource)
    const markup = renderToStaticMarkup(<HirPanel hir={analysis.hir} />)

    expect(markup).toContain('aria-label="Elaborated HIR functions"')
    expect(markup).toContain('(I32) -&gt; I32')
    expect(markup).toContain('call identity')
    expect(markup).toContain('literal 42')
  })

  it('reveals resolved type and span on hover and focus', () => {
    const analysis = analyze(completeSource)
    const markup = renderToStaticMarkup(<HirPanel hir={analysis.hir} />)

    expect(markup).toMatch(/title="I32 \[\d+, \d+\)"/)
    expect(markup).toMatch(/aria-label="literal 42 : I32 \[\d+, \d+\)"/)
    expect(markup).toContain('tabindex="0"')
  })

  it('marks unavailable HIR explicitly for unknown call targets', () => {
    const analysis = analyze('pub fn main() -> I32 { return missing() }')
    const markup = renderToStaticMarkup(<HirPanel hir={analysis.hir} />)

    expect(markup).toContain('>unavailable</code>')
    expect(markup).not.toContain('call missing')
  })
})

describe('SyntaxInspector', () => {
  it('offers recursive semantic presets without advertising later compiler phases', () => {
    const markup = renderToStaticMarkup(<SyntaxInspector />)

    expect(markup).toContain('Nested flow · complete')
    expect(markup).toContain('Nested flow · siblings')
    expect(markup).toContain('Nested flow · unavailable')
    expect(markup).toContain('Nested flow · wrong arity')
    expect(markup).toContain('Damaged nested call')
    expect(markup).toContain('Nested evaluation · completed')
    expect(markup).toContain('Nested flow · inner blocked')
    expect(markup).toContain('Nested flow · cycle')
    expect(markup).toContain('semantic AST, HIR, and code generation do not exist yet')
  })
})
