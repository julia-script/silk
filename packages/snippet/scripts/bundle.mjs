/**
 * Builds the self-registering browser bundle beside the library output.
 *
 * The bundle exists for generated documentation sites: one relative `<script type="module">` and
 * the element registers itself, with CodeMirror and the compiler (stdlib sources included) inside.
 */

import { existsSync } from 'node:fs'
import { build } from 'esbuild'

if (!existsSync('dist/register.js')) {
  // Scaffold state: nothing to bundle yet.
  process.exit(0)
}

await build({
  entryPoints: ['dist/register.js'],
  bundle: true,
  format: 'esm',
  platform: 'browser',
  target: 'es2022',
  minify: true,
  outfile: 'dist/silk-snippet.bundle.js',
  logLevel: 'warning',
})
