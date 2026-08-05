/**
 * Committed fixture generation and verification.
 *
 * `regenerate` rebuilds every fixture module through the built package and rewrites the
 * committed expected bytes (`.wasm.hex`) and text (`.wat`).
 *
 * `verify` rebuilds the same modules and requires byte-identical binary output and
 * character-identical text output against the committed files. It uses no external tools, so it
 * runs in CI everywhere; oracle checks live in `oracle.mjs`.
 */
import { readFileSync, writeFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { fixtures } from './fixture-modules.mjs'

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const fixtureRoot = resolve(packageRoot, 'fixtures')
const command = process.argv[2] ?? 'verify'

const toHex = (bytes) =>
  `${[...bytes]
    .map((byte) => byte.toString(16).padStart(2, '0'))
    .join('')
    .replace(/(.{64})/g, '$1\n')
    .trim()}\n`

const fromHex = (text) => Buffer.from(text.replace(/\s+/g, ''), 'hex')

let failures = 0
for (const fixture of fixtures) {
  const { bytes, text } = await fixture.run()
  const hexPath = resolve(fixtureRoot, `${fixture.name}.wasm.hex`)
  const watPath = resolve(fixtureRoot, `${fixture.name}.wat`)
  if (command === 'regenerate') {
    writeFileSync(hexPath, toHex(bytes))
    writeFileSync(watPath, text)
    process.stdout.write(`regenerated ${fixture.name} (${bytes.length} bytes)\n`)
    continue
  }
  const expectedBytes = fromHex(readFileSync(hexPath, 'utf8'))
  const expectedText = readFileSync(watPath, 'utf8')
  if (!expectedBytes.equals(Buffer.from(bytes))) {
    process.stderr.write(`FAIL ${fixture.name}: binary output differs from committed fixture\n`)
    failures += 1
  }
  if (expectedText !== text) {
    process.stderr.write(`FAIL ${fixture.name}: text output differs from committed fixture\n`)
    failures += 1
  }
}

if (command === 'regenerate') {
  writeFileSync(
    resolve(fixtureRoot, 'MANIFEST'),
    `${fixtures.map((fixture) => fixture.name).join('\n')}\n`,
  )
}

if (command === 'verify') {
  const manifest = readFileSync(resolve(fixtureRoot, 'MANIFEST'), 'utf8').trim().split('\n')
  if (manifest.join('\n') !== fixtures.map((fixture) => fixture.name).join('\n')) {
    process.stderr.write('FAIL fixture manifest does not match the fixture inventory\n')
    failures += 1
  }
  if (failures > 0) {
    process.stderr.write(`${failures} fixture comparison(s) failed\n`)
    process.exit(1)
  }
  process.stdout.write(`${fixtures.length} fixtures verified\n`)
}
