import { Analytics } from '@vercel/analytics/next'
import { RootProvider } from 'fumadocs-ui/provider/next'
import './global.css'
import type { Metadata } from 'next'
import type { ReactNode } from 'react'

const description =
  'A low-level language where typed failures, borrowed service requirements, ownership, explicit allocation, and structured concurrency compose together.'

export const metadata: Metadata = {
  metadataBase: new URL('https://silklang.org'),
  title: {
    default: 'Silk — What If Effect Were a Low-Level Language?',
    template: '%s · Silk',
  },
  description,
  alternates: {
    // resolves per-page against metadataBase, so / gets https://silklang.org/
    canonical: './',
  },
  openGraph: {
    type: 'website',
    siteName: 'Silk',
    url: 'https://silklang.org/',
    title: 'Silk — What If Effect Were a Low-Level Language?',
    description,
  },
  twitter: {
    card: 'summary_large_image',
    title: 'Silk — What If Effect Were a Low-Level Language?',
    description,
  },
}

export default function Layout({ children }: { children: ReactNode }) {
  return (
    <html lang="en" suppressHydrationWarning>
      <body className="flex flex-col min-h-screen">
        <RootProvider>{children}</RootProvider>
        <Analytics />
      </body>
    </html>
  )
}
