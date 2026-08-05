import { DocsPage } from 'fumadocs-ui/layouts/docs/page'
import type { Metadata } from 'next'
import { InstancesLab } from './instances'

export const metadata: Metadata = {
  title: 'Instance Discovery',
  description:
    'An internal lab for inspecting instance discovery: the entry state and the recorded worklist of reachable instances with canonical keys.',
  robots: {
    index: false,
    follow: false,
  },
}

export default function InstancesPage() {
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
          Instance Discovery
        </h1>
        <p className="text-base leading-7 text-fd-muted-foreground">
          Edit a program and inspect which concrete runtime instances are reachable from the user
          entry: the resolved or explicitly unavailable entry state, and the recorded worklist in
          discovery order, each instance keyed by its canonical declaration identity with (empty)
          normalized type and contract-row arguments. Record-before-follow makes recursion
          terminate. Everything stays in browser memory.
        </p>
      </header>
      <InstancesLab />
    </DocsPage>
  )
}
