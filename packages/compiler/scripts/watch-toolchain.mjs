import { execFileSync, spawn } from 'node:child_process'
import { watch } from 'node:fs'
import { fileURLToPath } from 'node:url'

const packageRoot = fileURLToPath(new URL('../', import.meta.url))
const generator = fileURLToPath(new URL('./generate-toolchain-integrity.mjs', import.meta.url))
const generate = () => execFileSync(process.execPath, [generator], { stdio: 'inherit' })

// Refresh the identity before startup, on compiler source edits, and when the `@silklang/llvm`
// watch build rewrites its `dist`. Writing only changed output keeps the generated module's own
// filesystem event from causing a watch loop. Directory additions/removals can also change a
// recursive inventory.
generate()
const watchers = [
  watch(new URL('../src/', import.meta.url), { recursive: true }, (_event, name) => {
    if (name !== 'ToolchainIntegrity.generated.ts') generate()
  }),
  watch(new URL('../../llvm/dist/', import.meta.url), { recursive: true }, generate),
]
const closeWatchers = () => {
  for (const watcher of watchers) watcher.close()
}
const compiler = spawn('tsc', ['-p', 'tsconfig.json', '--watch', '--preserveWatchOutput'], {
  cwd: packageRoot,
  stdio: 'inherit',
})

for (const signal of ['SIGINT', 'SIGTERM']) {
  process.on(signal, () => {
    closeWatchers()
    compiler.kill(signal)
  })
}
process.on('exit', () => compiler.kill())
compiler.on('error', (error) => {
  closeWatchers()
  process.stderr.write(`${error}\n`)
  process.exitCode = 1
})
compiler.on('exit', (code, signal) => {
  closeWatchers()
  if (signal !== null) {
    process.removeAllListeners(signal)
    process.kill(process.pid, signal)
  } else process.exitCode = code ?? 1
})
