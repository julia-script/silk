import { DocsPage } from 'fumadocs-ui/layouts/docs/page'
import type { Metadata } from 'next'
import { WasmLab } from './wasm'

export const metadata: Metadata = {
  title: 'WebAssembly',
  description:
    'An internal lab showing the WebAssembly backend emission: WAT text, the binary, and the result of executing it in the browser.',
  robots: {
    index: false,
    follow: false,
  },
}

export default function WasmPage() {
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
          WebAssembly
        </h1>
        <p className="text-base leading-7 text-fd-muted-foreground">
          The same MIR the LLVM lab emits from, sent through a second backend that builds a
          WebAssembly module instead. Edit a program and inspect the WAT text, the binary bytes,
          and the value returned by actually running <code>silk_main</code> in your browser&apos;s
          WebAssembly engine — which independently validates the module before it executes. The
          bootstrap interpreter&apos;s result is shown beside it, so the two must agree. MIR keeps
          the structure the source had — branch diamonds with explicit join blocks — so each
          backend consumes that shape the way its target wants: LLVM takes the blocks as-is, and
          this backend recovers <code>if</code>/<code>else</code> from the same diamonds.
          Everything stays in browser memory.
        </p>
      </header>
      <WasmLab />
    </DocsPage>
  )
}
