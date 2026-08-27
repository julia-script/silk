import { getSlugs, loader } from 'fumadocs-core/source';
import { applyMdxPreset, frontmatterSchema } from 'fumadocs-mdx/config';
import { defineDocs } from 'fumadocs-mdx/macro';
import { TextMate } from '@silk-lang/language';
import { remarkH1Title } from './remark-h1-title.mjs';

/**
 * The language and compiler docs are plain Markdown with no frontmatter, so they remain useful
 * when read directly on GitHub and do not carry site-specific metadata.
 *
 * Frontmatter is validated before remark runs, so the title falls back to the file name here and
 * `remarkH1Title` upgrades it to the document's `# H1`. An author-written title always wins.
 */
const schema = (ctx: { path: string }) =>
  frontmatterSchema.extend({ title: frontmatterSchema.shape.title.optional() }).transform((data) => ({
    ...data,
    title: data.title ?? fallbackTitle(ctx.path),
    titleFromFileName: data.title === undefined,
  }));

/** `how-to/debug-metadata.md` -> `Debug metadata`. Only used when a doc has no `# H1`. */
function fallbackTitle(path: string): string {
  const name = path.split('/').pop()?.replace(/\.mdx?$/, '') ?? path;
  const words = name.replace(/^\d+-/, '').replace(/[-_]/g, ' ');
  return words.charAt(0).toUpperCase() + words.slice(1);
}

// ponytail: one collection over the app-owned `content/` tree. Loader record keys are type
// discriminators, not slug prefixes, so namespacing comes from the directory layout.
const docs = defineDocs({
  dir: './content',
  docs: {
    schema,
    // Collection-level mdxOptions replace the defaults outright, so extend the preset.
    mdxOptions: applyMdxPreset({
      remarkPlugins: (v) => [remarkH1Title, ...v],
      rehypeCodeOptions: {
        // Bundled languages lazy-load regardless of `langs`, so this only adds Silk on top.
        // The grammar's own types are readonly; Shiki's registration type is not.
        langs: [TextMate.grammar as never],
        // Shiki has no grammar for some fences the docs use (e.g. `ebnf`). Render those as
        // plain text instead of failing the build — and without editing the published Markdown.
        fallbackLanguage: 'text',
        // Fumadocs fills these in at runtime when absent, but the option type demands them.
        themes: { light: 'github-light', dark: 'github-dark' },
      },
    }),
  },
});

/** Sidebar label for each top-level content folder, keyed by directory name. */
const folderTitles: Record<string, string> = {
  language: 'Silk language',
  reference: 'Prescriptive reference',
};

export const source = loader({
  baseUrl: '/docs',
  source: docs.toFumadocsSource(),
  pageTree: {
    transformers: [
      {
        folder(node, folderPath) {
          const title = folderTitles[folderPath];
          return title ? { ...node, name: title } : node;
        },
      },
    ],
  },
  // The app-owned sections use `README.md` as their entry so GitHub renders a useful landing page,
  // where Fumadocs expects `index.md`. Treat them the same so their section routes resolve.
  slugs(file) {
    const slugs = getSlugs(file.path);
    return slugs.at(-1) === 'README' ? slugs.slice(0, -1) : undefined;
  },
});
