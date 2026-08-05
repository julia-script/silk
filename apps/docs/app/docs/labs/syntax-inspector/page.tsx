import type { Metadata } from 'next'
import { DocsPage } from 'fumadocs-ui/layouts/docs/page'
import { SyntaxInspector } from './syntax-inspector'

export const metadata: Metadata = {
  title: 'Syntax Inspector',
  description:
    'An internal lab for inspecting Silk concrete trees, semantic facts, data flow, and closed bootstrap evaluation.',
  robots: {
    index: false,
    follow: false,
  },
}

export default function SyntaxInspectorPage() {
  return (
    <DocsPage
      full
      breadcrumb={{ enabled: false }}
      footer={{ enabled: false }}
      tableOfContent={{ enabled: false }}
      tableOfContentPopover={{ enabled: false }}
      className="max-w-[1500px] gap-0 px-4 py-10 sm:px-7 lg:px-10"
    >
      <header className="mb-8 max-w-3xl">
        <p className="mb-3 font-mono text-xs font-semibold uppercase tracking-[0.18em] text-fd-muted-foreground">
          Internal compiler lab
        </p>
        <h1 className="mb-3 text-3xl font-semibold tracking-tight text-fd-foreground sm:text-4xl">
          Syntax Inspector
        </h1>
        <p className="text-base leading-7 text-fd-muted-foreground">
          Edit one or more bootstrap functions and inspect their lossless concrete syntax tree
          alongside ordered semantic facts, function-local parameter lookup, and resolved reference
          relationships. Navigable nested lanes group every call site in argument order and connect
          inner results to their owning outer arguments, mapped parameters, returned references,
          call results, and the caller return. The green semantic layer never implies execution;
          only the explicit Evaluate action adds the violet trace-backed order, exact values, and
          blocked endpoint. Missing, ambiguous, incompatible, unavailable, and cyclic branches end
          visibly without a fabricated enclosing binding or return. Every group, node, edge,
          terminal, declaration, reference, argument, separator, and token stays keyboard-selectable
          and attached to its original UTF-8 byte span, with the same depth, ordinal, state, value,
          identity, and range available in reading order. The browser-local evaluator is still only
          the closed bootstrap slice—not native compilation, a runtime, conversions, a general scope
          graph, semantic AST, HIR, or a code-generation pipeline.
        </p>
      </header>
      <SyntaxInspector />
    </DocsPage>
  )
}
