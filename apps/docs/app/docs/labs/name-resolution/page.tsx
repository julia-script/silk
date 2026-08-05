import { DocsPage } from 'fumadocs-ui/layouts/docs/page'
import type { Metadata } from 'next'
import { NameResolutionLab } from './name-resolution'

export const metadata: Metadata = {
  title: 'Name Resolution',
  description: 'Inspect closure-wide module scopes and canonical import bindings.',
  robots: { index: false, follow: false },
}

export default function NameResolutionPage() {
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
          Name Resolution
        </h1>
        <p className="text-base leading-7 text-fd-muted-foreground">
          Inspect the immutable flat scope built from private and public declarations, intrinsic
          actors, namespace imports, selected members, aliases, and conflicts. Every target is the
          same canonical declaration used by HIR and instance discovery.
        </p>
      </header>
      <NameResolutionLab />
    </DocsPage>
  )
}
