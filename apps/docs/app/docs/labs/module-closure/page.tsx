import { DocsPage } from 'fumadocs-ui/layouts/docs/page'
import type { Metadata } from 'next'
import { ModuleClosureLab } from './module-closure'

export const metadata: Metadata = {
  title: 'Module Closure',
  description:
    'An internal lab for inspecting the loaded module closure of a compilation request: imports, canonical order, cycles, and module diagnostics.',
  robots: {
    index: false,
    follow: false,
  },
}

export default function ModuleClosurePage() {
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
          Module Closure
        </h1>
        <p className="text-base leading-7 text-fd-muted-foreground">
          Edit a small set of module sources and inspect the loaded closure of one compilation
          request: every reachable module in canonical identity order, each import resolved to its
          target or diagnosed, import cycles marked on their participating modules, and the
          module-phase diagnostics in driver order. The provisional <code>import</code> spelling is
          owned by the syntax-prototype issue; everything here stays in browser memory.
        </p>
      </header>
      <ModuleClosureLab />
    </DocsPage>
  )
}
