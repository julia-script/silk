import { DocsPage } from 'fumadocs-ui/layouts/docs/page'
import type { Metadata } from 'next'
import { MirCfgLab } from './mir-cfg'

export const metadata: Metadata = {
  title: 'MIR CFG',
  description:
    'An internal lab rendering hand-built MIR samples: blocks, edges, per-operation provenance, and the deterministic textual encoding.',
  robots: {
    index: false,
    follow: false,
  },
}

export default function MirCfgPage() {
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
          MIR CFG
        </h1>
        <p className="text-base leading-7 text-fd-muted-foreground">
          Inspect the monomorphic, backend-neutral MIR control-flow graph on hand-built samples:
          every block with its kind, ordered operations, and terminator; control-flow edges;
          per-operation provenance with generated markers revealed on hover; and the deterministic
          textual encoding gated by golden tests. The samples retire when lowering from HIR lands.
        </p>
      </header>
      <MirCfgLab />
    </DocsPage>
  )
}
