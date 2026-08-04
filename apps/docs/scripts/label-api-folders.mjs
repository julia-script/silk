// Typedoc wipes its output directory on every run, so the Fumadocs `meta.json` that names each
// generated folder in the sidebar has to be (re)written here rather than committed.
import { writeFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';

const content = fileURLToPath(new URL('../content/', import.meta.url));

writeFileSync(
  `${content}llvm-api/meta.json`,
  `${JSON.stringify({ title: 'API reference', description: 'Generated from TSDoc.' }, null, 2)}\n`,
);
