import { DocsPage } from 'fumadocs-ui/layouts/docs/page'
import type { Metadata } from 'next'
import { LlvmIrLab } from './llvm-ir'

export const metadata: Metadata = {
  title: 'LLVM IR',
  description:
    'An internal lab showing the backend emission: LLVM IR text, symbol table, and the lowered MIR it was emitted from.',
  robots: {
    index: false,
    follow: false,
  },
}

export default function LlvmIrPage() {
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
          LLVM IR
        </h1>
        <p className="text-base leading-7 text-fd-muted-foreground">
          Edit a program and inspect the nominal backend&apos;s emission: the deterministic symbol
          table with <code>silk_main</code> for the entry, the textual LLVM IR rendered from the
          same builder state as the bitcode (an inspection artifact, not an interchange format),
          optional native debug metadata, and the lowered MIR blocks the emission came from.
          Everything stays in browser memory.
        </p>
      </header>
      <LlvmIrLab />
    </DocsPage>
  )
}
