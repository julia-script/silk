import { execFileSync } from 'node:child_process'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const modules = ['declaration', 'core', 'advanced', 'metadata']
const modes = ['text', 'bitcode']
const results = []
for (const module of modules) {
  const script = resolve(packageRoot, `scripts/${module}-module.mjs`)
  for (const mode of modes) {
    const first = execFileSync(process.execPath, [script, mode])
    const second = execFileSync(process.execPath, [script, mode])
    if (!first.equals(second)) throw new Error(`${module}/${mode} is not deterministic`)
    results.push({ module, mode, bytes: first.length })
  }
}
process.stdout.write(
  `${JSON.stringify({ runtime: process.version, architecture: process.arch, results })}\n`,
)
