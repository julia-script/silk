import type { Metadata } from 'next'
import { SyntaxInspector } from './syntax-inspector'

export const metadata: Metadata = {
  title: 'Syntax Inspector',
  description: 'An internal lab for inspecting the first Silk concrete syntax tree.',
  robots: {
    index: false,
    follow: false,
  },
}

export default function SyntaxInspectorPage() {
  return (
    <main className="mx-auto w-full max-w-[1500px] px-4 py-10 sm:px-7 lg:px-10">
      <header className="mb-8 max-w-3xl">
        <p className="mb-3 font-mono text-xs font-semibold uppercase tracking-[0.18em] text-fd-muted-foreground">
          Internal compiler lab
        </p>
        <h1 className="mb-3 text-3xl font-semibold tracking-tight text-fd-foreground sm:text-4xl">
          Syntax Inspector
        </h1>
        <p className="text-base leading-7 text-fd-muted-foreground">
          Edit the first bootstrap function and inspect its lossless concrete syntax tree. Every
          token stays attached to its original UTF-8 byte span; recovery appears as explicit error
          nodes and missing leaves.
        </p>
      </header>
      <SyntaxInspector />
    </main>
  )
}
