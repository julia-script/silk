# `@silk-lang/docs`

The live documentation site for the Silk language and compiler. Next.js + Fumadocs + Tailwind.

```sh
pnpm dev
```

Run this from the repository root so Turbo also watches the local packages consumed by the app.

## Content

This app owns the documentation content directly:

- `content/language` contains tutorials, focused guides, and generated standard-library and
  diagnostic lookup pages, exposed at `/docs/language/**`.
- `content/reference` contains the prescriptive language reference, exposed at
  `/docs/reference/**`.

Compiler documentation generators write their checked output into `content/language`; compiler
tests compile examples from both sections.

The standalone `@silk-lang/llvm` package has its own Markdown documentation, indexed from
[`packages/llvm/README.md`](../../packages/llvm/README.md). It is deliberately not published on
this site because it documents using the LLVM library independently of the Silk language.

These docs are plain Markdown with no frontmatter. Every section uses `index.md` as its entrypoint,
which Fumadocs maps to the section route instead of sorting as an ordinary sidebar page.
[`lib/source.ts`](lib/source.ts) derives each page title from its leading `# H1` (falling back to the
file name), so the source stays useful both on the site and on GitHub.

The app pins TypeScript 5.9 because Next.js does not yet support the compiler API exposed by the
repository's TypeScript 7 toolchain.
