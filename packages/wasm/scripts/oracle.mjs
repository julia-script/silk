/**
 * Oracle verification against the pinned `wasm-tools` release.
 *
 * For every committed fixture: the binary must pass `wasm-tools validate`, and the committed
 * text, assembled with `wasm-tools parse`, must produce bytes identical to the committed binary.
 * The negative corpus asserts oracle agreement from the other side: constructions the builder
 * rejects must also be rejected by `wasm-tools validate`.
 *
 * Runtime code never invokes the oracle; this script is development-time verification only.
 * Point `WASM_TOOLS` at the pinned binary if it is not on PATH.
 */
import { execFileSync } from 'node:child_process'
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { negativeCorpus } from './negative-corpus.mjs'

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const fixtureRoot = resolve(packageRoot, 'fixtures')
const wasmTools = process.env.WASM_TOOLS ?? 'wasm-tools'

/** The pinned oracle release. Update UPSTREAM.md when this changes. */
const PINNED_VERSION = 'wasm-tools 1.255.0'

/** Features matching the package baseline; validation must not assume more. */
const FEATURES = [
  'mutable-global',
  'saturating-float-to-int',
  'sign-extension',
  'multi-value',
  'bulk-memory',
  'reference-types',
  'tail-call',
  'extended-const',
  'multi-memory',
  'simd',
  'relaxed-simd',
  'threads',
  'memory64',
]

const run = (args, options = {}) => execFileSync(wasmTools, args, { stdio: 'pipe', ...options })

const version = run(['--version']).toString().trim()
if (version !== PINNED_VERSION) {
  process.stderr.write(
    `FAIL oracle version drift: expected ${JSON.stringify(PINNED_VERSION)} ` +
      `but found ${JSON.stringify(version)}\n`,
  )
  process.exit(1)
}

const fromHex = (text) => Buffer.from(text.replace(/\s+/g, ''), 'hex')
const fixtureNames = readFileSync(resolve(fixtureRoot, 'MANIFEST'), 'utf8')
  .trim()
  .split('\n')
  .filter((line) => line.length > 0)

const temporary = mkdtempSync(resolve(tmpdir(), 'silk-effect-wasm-oracle-'))
let failures = 0
try {
  const features = ['--features', FEATURES.join(',')]
  for (const name of fixtureNames) {
    const bytes = fromHex(readFileSync(resolve(fixtureRoot, `${name}.wasm.hex`), 'utf8'))
    const binaryPath = resolve(temporary, `${name}.wasm`)
    writeFileSync(binaryPath, bytes)
    try {
      run(['validate', ...features, binaryPath])
    } catch (error) {
      failures += 1
      process.stderr.write(`FAIL ${name}: wasm-tools validate rejected the binary fixture\n`)
      process.stderr.write(`${String(error.stderr ?? error)}\n`)
    }
    const assembledPath = resolve(temporary, `${name}.roundtrip.wasm`)
    try {
      run(['parse', resolve(fixtureRoot, `${name}.wat`), '-o', assembledPath])
      const assembled = readFileSync(assembledPath)
      if (!assembled.equals(bytes)) {
        failures += 1
        process.stderr.write(
          `FAIL ${name}: text round-trip differs from the binary fixture ` +
            `(${assembled.length} vs ${bytes.length} bytes)\n`,
        )
      }
    } catch (error) {
      failures += 1
      process.stderr.write(`FAIL ${name}: wasm-tools could not parse the text fixture\n`)
      process.stderr.write(`${String(error.stderr ?? error)}\n`)
    }
  }

  for (const entry of negativeCorpus) {
    const watPath = resolve(temporary, `${entry.name}.wat`)
    writeFileSync(watPath, entry.wat)
    const binaryPath = resolve(temporary, `${entry.name}.invalid.wasm`)
    let parsed = true
    try {
      run(['parse', watPath, '-o', binaryPath])
    } catch {
      parsed = false
    }
    if (!parsed) {
      // The construct is not even encodable; agreement holds trivially.
      continue
    }
    let rejected = false
    try {
      run(['validate', ...features, binaryPath])
    } catch {
      rejected = true
    }
    if (!rejected) {
      failures += 1
      process.stderr.write(
        `FAIL negative ${entry.name}: the oracle accepted a module the builder rejects\n`,
      )
    }
  }
} finally {
  rmSync(temporary, { recursive: true, force: true })
}

if (failures > 0) {
  process.stderr.write(`${failures} oracle check(s) failed\n`)
  process.exit(1)
}
process.stdout.write(
  `oracle ${version}: ${fixtureNames.length} fixtures validated and round-tripped, ` +
    `${negativeCorpus.length} negative cases rejected\n`,
)
