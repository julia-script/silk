import { DocsPage } from 'fumadocs-ui/layouts/docs/page'
import type { Metadata } from 'next'
import { OwnershipLab } from './ownership'

export const metadata: Metadata = {
  title: 'Ownership',
  description:
    'An internal lab for inspecting ownership facts and target-neutral cleanup plans: binding timelines, verdicts, and per-exit release orders.',
  robots: {
    index: false,
    follow: false,
  },
}

export default function OwnershipPage() {
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
          Ownership
        </h1>
        <p className="text-base leading-7 text-fd-muted-foreground">
          Inspect the ownership phase over typed HIR: each function&apos;s bindings with their
          ownership category and live range over exact source spans, the closed verdict with
          unavailable states explicit, and the target-neutral cleanup plan as an ordered release
          list per structured exit. On the frozen slice every value is a copyable I32, so exits
          release nothing — the phase, its fact table, and the plan artifact are what this lab
          shows. Everything stays in browser memory.
        </p>
      </header>
      <OwnershipLab />
    </DocsPage>
  )
}
