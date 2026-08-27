# `@silk-lang/docs`

The live documentation site for the Silk language and compiler. Next.js + Fumadocs + Tailwind.

```sh
pnpm dev
```

Run this from the repository root so Turbo also watches the local packages consumed by the app.

## Content

The language and compiler documentation is read in place from `packages/language/docs`; nothing is
copied into this app. The `content/language` symlink keeps the package directory as the single
source of truth while exposing it to Fumadocs at `/docs/language/**`.

The standalone `@silk-lang/llvm` package has its own Markdown documentation, indexed from
[`packages/llvm/README.md`](../../packages/llvm/README.md). It is deliberately not published on
this site because it documents using the LLVM library independently of the Silk language.

These docs are plain Markdown with no frontmatter. [`lib/source.ts`](lib/source.ts) derives each
page title from its leading `# H1` (falling back to the file name), so the source stays useful both
on the site and on GitHub.

The app pins TypeScript 5.9 because Next.js does not yet support the compiler API exposed by the
repository's TypeScript 7 toolchain.
