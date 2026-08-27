import { readFileSync } from 'node:fs'
import { join } from 'node:path'
import type { Metadata } from 'next'
import { JetBrains_Mono } from 'next/font/google'
import Link from 'next/link'
import { RegisterSnippets } from './_introduction/RegisterSnippets'
import './_introduction/introduction.css'

const mono = JetBrains_Mono({
  subsets: ['latin'],
  weight: ['400', '500', '600'],
  style: ['normal', 'italic'],
  variable: '--intro-mono',
})

export const metadata: Metadata = {
  title: 'What If Effect Were a Low-Level Language?',
  description:
    'An introduction to Silk — a low-level language with typed effects, ownership, and structured concurrency — with live compiler-checked examples.',
}

export default function Home() {
  // The essay is authored directly as HTML — see the conventions comment at the top of the
  // file. Read per render, not at module scope: the file is not an import, so the dev server
  // never invalidates the module for it, and a module-scope read would pin the first version
  // until restart. Production renders this page once at build time either way.
  const html = readFileSync(
    join(process.cwd(), 'app/_introduction/introduction.html'),
    'utf8',
  )
  return (
    <div className={`silk-intro ${mono.variable}`}>
      <header className="bar">
        <span className="title">silk</span>
        <span className="spacer" />
        <Link href="/docs/language">docs</Link>
      </header>
      {/* Repo-authored content, not user-supplied. */}
      {/* biome-ignore lint/security/noDangerouslySetInnerHtml: repo-authored page content */}
      <article dangerouslySetInnerHTML={{ __html: html }} />
      <RegisterSnippets />
    </div>
  )
}
