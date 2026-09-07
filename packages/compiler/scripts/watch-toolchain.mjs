import { execFileSync, spawn } from 'node:child_process'
import { watch } from 'node:fs'
import { fileURLToPath } from 'node:url'

const packageRoot = fileURLToPath(new URL('../', import.meta.url))
const generator = fileURLToPath(new URL('./generate-toolchain-integrity.mjs', import.meta.url))
const generate = () => execFileSync(process.execPath, [generator], { stdio: 'inherit' })

// Refresh the identity before startup and on source edits. Writing only changed output keeps
// the generated module's own filesystem event from causing a watch loop.
generate()
const watcher = watch(new URL('../src/', import.meta.url), { recursive: true }, (_event, name) => {
  // Directory additions/removals can also change the recursive source inventory.
  if (name !== 'ToolchainIntegrity.generated.ts') generate()
})
const compiler = spawn('tsc', ['-p', 'tsconfig.json', '--watch', '--preserveWatchOutput'], {
  cwd: packageRoot,
  stdio: 'inherit',
})

for (const signal of ['SIGINT', 'SIGTERM']) {
  process.on(signal, () => {
    watcher.close()
    compiler.kill(signal)
  })
}
process.on('exit', () => compiler.kill())
compiler.on('error', (error) => {
  watcher.close()
  process.stderr.write(`${error}\n`)
  process.exitCode = 1
})
compiler.on('exit', (code, signal) => {
  watcher.close()
  if (signal !== null) {
    process.removeAllListeners(signal)
    process.kill(process.pid, signal)
  } else process.exitCode = code ?? 1
})
