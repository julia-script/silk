import { writeFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { extractInventory, fetchSources } from './parity-lib.mjs'

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const commit = process.argv[2]
if (commit === undefined) throw new Error('Usage: node scripts/parity-inventory.mjs <zig-commit>')
const inventory = extractInventory(commit, await fetchSources(commit))
writeFileSync(
  resolve(packageRoot, 'parity/upstream-inventory.json'),
  `${JSON.stringify(inventory, undefined, 2)}\n`,
)
process.stdout.write(
  `${JSON.stringify(
    Object.fromEntries(
      Object.entries(inventory)
        .filter(([, value]) => Array.isArray(value))
        .map(([key, value]) => [key, value.length]),
    ),
  )}\n`,
)
