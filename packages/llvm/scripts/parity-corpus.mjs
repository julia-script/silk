import { execFileSync } from 'node:child_process'
import { writeFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const diagnostic = resolve(packageRoot, 'parity/last-diagnostics.txt')
try {
  const output = execFileSync('pnpm', ['fixtures:verify'], {
    cwd: packageRoot,
    encoding: 'utf8',
    env: process.env,
    stdio: 'pipe',
  })
  writeFileSync(diagnostic, 'complete corpus accepted\n')
  process.stdout.write(output)
} catch (cause) {
  const raw = `${cause.stdout ?? ''}\n${cause.stderr ?? ''}`
  const normalized = raw.replaceAll(/\/[^\s:]+\/T\/[^\s]+/g, '<temporary>')
  writeFileSync(diagnostic, normalized)
  throw new Error(`LLVM corpus verification failed; diagnostics: ${diagnostic}`)
}
