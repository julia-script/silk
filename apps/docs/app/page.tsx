import { readFileSync } from 'node:fs'
import { join } from 'node:path'
import { JetBrains_Mono } from 'next/font/google'
import Link from 'next/link'
import { render } from './_introduction/render'
import { RegisterSnippets } from './_introduction/RegisterSnippets'
import './_introduction/introduction.css'

const mono = JetBrains_Mono({
  subsets: ['latin'],
  weight: ['400', '500', '600'],
  style: ['normal', 'italic'],
  variable: '--intro-mono',
})

export const metadata = {
  title: 'What If Effect Were a Low-Level Language?',
  description:
    'An introduction to Silk — a low-level language with typed effects, ownership, and structured concurrency — with live compiler-checked examples.',
}

// The essay is rendered once at build time; every live example was compiled by the real
// compiler during that render, so the page can never show a false diagnostic.
const html = render(
  readFileSync(join(process.cwd(), 'app/_introduction/silk-introduction.md'), 'utf8'),
)

export default function Home() {
  return (
    <div className={`silk-intro ${mono.variable}`}>
      <header className="bar">
        <span className="title">silk</span>
        <span>introduction</span>
        <span className="spacer" />
        <Link href="/docs/language">docs</Link>
        <Link href="/labs">labs</Link>
      </header>
      {/* Rendered from the repo's own Markdown by render.ts; not user-supplied content. */}
      {/* biome-ignore lint/security/noDangerouslySetInnerHtml: build-time output of our renderer */}
      <article dangerouslySetInnerHTML={{ __html: html }} />
      <RegisterSnippets />
    </div>
  )
}
