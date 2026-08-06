/**
 * Regenerates the VS Code extension's grammar and language configuration from the built
 * `TextMate` module. VS Code requires contributed files to live inside the extension folder, so
 * they are generated copies; `test/TextMate.test.ts` fails when they drift from the source.
 */
import { execFileSync } from 'node:child_process'
import { mkdirSync, writeFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { grammar, languageConfiguration } from '../dist/TextMate.js'

const extensionRoot = resolve(dirname(new URL(import.meta.url).pathname), '../../vscode')
const grammarPath = resolve(extensionRoot, 'syntaxes/silk.tmLanguage.json')
const configurationPath = resolve(extensionRoot, 'language-configuration.json')

mkdirSync(dirname(grammarPath), { recursive: true })
writeFileSync(grammarPath, `${JSON.stringify(grammar, null, 2)}\n`)
writeFileSync(configurationPath, `${JSON.stringify(languageConfiguration, null, 2)}\n`)
execFileSync('pnpm', ['exec', 'biome', 'format', '--write', grammarPath, configurationPath], {
  stdio: 'inherit',
})
console.log(`wrote ${grammarPath}\nwrote ${configurationPath}`)
