import { execFileSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const fixtureRoot = resolve(packageRoot, 'fixtures/core')
const moduleScript = resolve(packageRoot, 'scripts/core-module.mjs')
const command = process.argv[2] ?? 'verify'
const executable = (name, variable) =>
  Effect.runSync(Config.string(variable).pipe(Config.withDefault(name)))
const currentText = () => execFileSync(process.execPath, [moduleScript, 'text'])
const currentBitcode = () => execFileSync(process.execPath, [moduleScript, 'bitcode'])

if (command === 'regenerate') {
  const text = currentText()
  const bitcode = currentBitcode()
  writeFileSync(resolve(fixtureRoot, 'representative.ll'), text)
  writeFileSync(resolve(fixtureRoot, 'representative.bc.hex'), `${bitcode.toString('hex')}\n`)
  process.stdout.write(`${createHash('sha256').update(bitcode).digest('hex')}\n`)
  process.exit(0)
}

if (command !== 'verify') throw new Error(`Unknown fixture command: ${command}`)

const expectedText = readFileSync(resolve(fixtureRoot, 'representative.ll'))
const expectedBitcode = Buffer.from(
  readFileSync(resolve(fixtureRoot, 'representative.bc.hex'), 'utf8').trim(),
  'hex',
)
if (!currentText().equals(expectedText)) throw new Error('core textual fixture is stale')
if (!currentBitcode().equals(expectedBitcode)) throw new Error('core bitcode fixture is stale')

const temporary = mkdtempSync(resolve(tmpdir(), 'silk-effect-llvm-core-'))
try {
  const text = resolve(fixtureRoot, 'representative.ll')
  const directBitcode = resolve(temporary, 'direct.bc')
  writeFileSync(directBitcode, expectedBitcode)
  execFileSync(executable('llvm-as', 'LLVM_AS'), [text, '-o', resolve(temporary, 'assembled.bc')])
  execFileSync(executable('llvm-dis', 'LLVM_DIS'), [
    directBitcode,
    '-o',
    resolve(temporary, 'direct.ll'),
  ])
  execFileSync(executable('llvm-bcanalyzer', 'LLVM_BCANALYZER'), [directBitcode, '-dump'], {
    stdio: 'ignore',
  })
  execFileSync(executable('opt', 'LLVM_OPT'), ['-passes=verify', directBitcode, '-disable-output'])
} finally {
  rmSync(temporary, { recursive: true, force: true })
}
