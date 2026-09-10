import type { Metadata } from 'next'
import { JetBrains_Mono } from 'next/font/google'
import { ShareWorkbench } from './ShareWorkbench'
import * as SourceCode from './SourceCode'

const mono = JetBrains_Mono({
  subsets: ['latin'],
  weight: ['400', '500', '600'],
  style: ['normal'],
  variable: '--share-mono',
})

const defaultSource = `import silk.allocator { Allocator, SystemAllocator }

pub fn main() -> i32 {
  let mut allocator = Allocator.systemAllocatorProvider()
  return missing()
}
`

export const metadata: Metadata = {
  title: 'Silk Snapshot',
  description: 'Turn Silk source into a syntax-highlighted PNG or SVG.',
}

type SearchParams = Promise<Record<string, string | ReadonlyArray<string> | undefined>>

const first = (value: string | ReadonlyArray<string> | undefined): string | undefined =>
  typeof value === 'string' ? value : value?.[0]

const enabled = (value: string | undefined): boolean => value !== '0'

const integer = (
  value: string | undefined,
  minimum: number,
  maximum: number,
  fallback: number,
): number => {
  const parsed = value === undefined ? Number.NaN : Number.parseInt(value, 10)
  return Number.isFinite(parsed) ? Math.min(maximum, Math.max(minimum, parsed)) : fallback
}

const optionalInteger = (
  value: string | undefined,
  minimum: number,
  maximum: number,
): number | undefined => {
  if (value === undefined || value === '') return undefined
  const parsed = Number.parseInt(value, 10)
  return Number.isFinite(parsed) ? Math.min(maximum, Math.max(minimum, parsed)) : undefined
}

const filename = (value: string | undefined): string => {
  const normalized = value
    ?.replace(/[\r\n]/g, '')
    .trim()
    .slice(0, 120)
  return normalized === undefined || normalized === '' ? 'untitled.silk' : normalized
}

export default async function SharePage({ searchParams }: { readonly searchParams: SearchParams }) {
  const parameters = await searchParams
  const encoded = first(parameters.code)
  const source =
    encoded === undefined ? defaultSource : (SourceCode.decode(encoded, 50_000) ?? defaultSource)
  return (
    <ShareWorkbench
      initialOptions={{
        backdrop: enabled(first(parameters.backdrop)),
        diagnostics: enabled(first(parameters.diagnostics)),
        diagnosticTooltip: enabled(first(parameters.tooltip)),
        filename: filename(first(parameters.filename)),
        height: optionalInteger(first(parameters.height), 200, 1600),
        inlayHints: enabled(first(parameters.hints)),
        padding: integer(first(parameters.padding), 0, 160, 64),
        width: integer(first(parameters.width), 320, 1600, 960),
      }}
      initialSource={source}
      monoClassName={mono.variable}
    />
  )
}
