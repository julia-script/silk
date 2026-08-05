import { DocsPage } from 'fumadocs-ui/layouts/docs/page'
import type { Metadata } from 'next'
import { DeclarationIndexLab } from './declaration-index'

export const metadata: Metadata = {
  title: 'Declaration Index',
  description:
    'An internal lab for inspecting the collected declaration headers of a loaded closure: canonical identities, resolved signatures, and header diagnostics.',
  robots: {
    index: false,
    follow: false,
  },
}

export default function DeclarationIndexPage() {
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
          Declaration Index
        </h1>
        <p className="text-base leading-7 text-fd-muted-foreground">
          Edit module sources and inspect the declaration index collected across the loaded
          closure before any body resolves: every header with its module, canonical identity
          (or explicit duplicate and unidentified states), and resolved signature, plus the
          header-level diagnostics in driver order. Everything stays in browser memory.
        </p>
      </header>
      <DeclarationIndexLab />
    </DocsPage>
  )
}
