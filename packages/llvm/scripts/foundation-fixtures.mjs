import { execFileSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const fixtureRoot = resolve(packageRoot, 'fixtures/foundation')
const oracle = resolve(packageRoot, 'scripts/foundation-oracle.zig')
const command = process.argv[2] ?? 'verify'
const executable = (name, variable) => process.env[variable] ?? name

const decodeHex = (path) => Buffer.from(readFileSync(path, 'utf8').trim(), 'hex')

if (command === 'regenerate') {
  const bytes = execFileSync(executable('zig', 'ZIG_EXE'), ['run', oracle])
  writeFileSync(resolve(fixtureRoot, 'upstream-configured.bc.hex'), `${bytes.toString('hex')}\n`)
  process.stdout.write(`${createHash('sha256').update(bytes).digest('hex')}\n`)
  process.exit(0)
}

if (command !== 'verify') throw new Error(`Unknown fixture command: ${command}`)

const temporary = mkdtempSync(resolve(tmpdir(), 'silk-effect-llvm-fixtures-'))
try {
  const bitcode = resolve(temporary, 'upstream-configured.bc')
  writeFileSync(bitcode, decodeHex(resolve(fixtureRoot, 'upstream-configured.bc.hex')))
  execFileSync(executable('llvm-dis', 'LLVM_DIS'), [bitcode, '-o', resolve(temporary, 'module.ll')])
  execFileSync(executable('llvm-bcanalyzer', 'LLVM_BCANALYZER'), [bitcode, '-dump'], {
    stdio: 'ignore',
  })
  execFileSync(executable('llvm-as', 'LLVM_AS'), [
    resolve(fixtureRoot, 'configured.ll'),
    '-o',
    resolve(temporary, 'configured.bc'),
  ])
  execFileSync(executable('opt', 'LLVM_OPT'), [
    '-passes=verify',
    resolve(temporary, 'configured.bc'),
    '-disable-output',
  ])
} finally {
  rmSync(temporary, { recursive: true, force: true })
}
