import { execFileSync } from 'node:child_process'
import { readFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { extractInventory, fetchSources, inventoryKinds } from './parity-lib.mjs'

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const arguments_ = process.argv.slice(2).filter((argument) => argument !== '--')
const commit = arguments_.find((argument) => !argument.startsWith('--'))
if (commit === undefined) {
  throw new Error('Usage: pnpm upstream:candidate -- <zig-commit> [--regenerate-fixtures]')
}
if (!/^[0-9a-f]{40}$/.test(commit)) {
  throw new Error('The candidate must be an explicit 40-character lowercase Zig commit hash')
}
const baseline = JSON.parse(
  readFileSync(resolve(packageRoot, 'parity/upstream-inventory.json'), 'utf8'),
)
const candidate = extractInventory(commit, await fetchSources(commit))
const changes = {}
for (const kind of inventoryKinds) {
  const previous = new Set(baseline[kind].map((entry) => entry.id))
  const next = new Set(candidate[kind].map((entry) => entry.id))
  changes[kind] = {
    added: [...next].filter((id) => !previous.has(id)),
    removed: [...previous].filter((id) => !next.has(id)),
  }
}
process.stdout.write(
  `${JSON.stringify(
    {
      candidate: { commit, sources: candidate.sources },
      changes,
      adopted: false,
      next: 'Open a reviewed OpenSpec change before updating the committed baseline or manifest.',
    },
    undefined,
    2,
  )}\n`,
)
if (arguments_.includes('--regenerate-fixtures')) {
  execFileSync('pnpm', ['fixtures:regenerate'], { cwd: packageRoot, stdio: 'inherit' })
}
