import type { Metadata } from 'next'
import { DocsPage } from 'fumadocs-ui/layouts/docs/page'
import { SyntaxInspector } from './syntax-inspector'

export const metadata: Metadata = {
  title: 'Syntax Inspector',
  description:
    'An internal lab for inspecting Silk concrete trees, function-local parameter references, call resolution, and ordered semantic facts.',
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
          relationships. Every declaration, reference, argument, separator, and token stays attached
          to its original UTF-8 byte span; recovery appears as explicit error nodes and missing
          leaves. Calls show ordered arguments, positional parameter mappings, and a contract state
          separately from their return compatibility. These relationships are not execution,
          conversions, a general scope graph, semantic AST, HIR, or a code-generation pipeline.
        </p>
      </header>
      <SyntaxInspector />
    </DocsPage>
  )
}
