import { DocsPage } from 'fumadocs-ui/layouts/docs/page'
import type { Metadata } from 'next'
import { PipelineLab } from './pipeline'

export const metadata: Metadata = {
  title: 'Pipeline',
  description:
    'An internal lab presenting the full compiler pipeline for the edited program: every phase with status, artifact counts, diagnostics, elapsed time, and links to each phase lab.',
  robots: {
    index: false,
    follow: false,
  },
}

export default function PipelinePage() {
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
          Pipeline
        </h1>
        <p className="text-base leading-7 text-fd-muted-foreground">
          The whole pipeline in one place: edit a program and see every phase in pinned order —
          syntax, module closure, declaration index, HIR elaboration, ownership, instance
          discovery, MIR lowering, backend emission, and the planned native stages — each with its
          artifact counts, per-phase diagnostics, the snapshot build time, and a link to the
          phase&apos;s dedicated lab. Every step of the realigned spine is visualizable from here.
        </p>
      </header>
      <PipelineLab />
    </DocsPage>
  )
}
