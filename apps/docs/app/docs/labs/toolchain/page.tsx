import { DocsPage } from 'fumadocs-ui/layouts/docs/page'
import type { Metadata } from 'next'
import { ToolchainLab } from './toolchain'

export const metadata: Metadata = {
  title: 'Toolchain',
  description:
    'An internal lab showing native toolchain provenance: planned Clang commands per optimization profile, artifact sizes, the runtime shim, and build-scope lifetimes.',
  robots: {
    index: false,
    follow: false,
  },
}

export default function ToolchainPage() {
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
          Toolchain
        </h1>
        <p className="text-base leading-7 text-fd-muted-foreground">
          Inspect the pinned native toolchain&apos;s orchestration for the edited program: the
          exact structured commands it issues (object emission under a fixed optimization profile,
          shim compilation, and the link driver invocation — never a shell string), the bitcode
          artifact size, the private runtime shim, and the build-scope lifecycle that owns every
          intermediate. The browser issues no processes; these are the same planned commands the
          orchestration runs.
        </p>
      </header>
      <ToolchainLab />
    </DocsPage>
  )
}
