import assert from 'node:assert/strict'
import { existsSync, readdirSync, readFileSync, statSync } from 'node:fs'
import { dirname, join, relative, resolve } from 'node:path'
import { test } from 'node:test'
import { fileURLToPath } from 'node:url'

/**
 * The workspace's test defaults only hold for packages that actually extend them, and the way this
 * repo has lost that property four times is not disagreement — it is a new package arriving with
 * no `vitest.config.ts`, silently taking Vitest's 5 s default, and becoming the first suite to be
 * reported as a timeout the next time the runner is busy. `packages/lsp` reached that state and
 * cost two pull requests a CI round before anyone looked at which packages had a config.
 *
 * So this is checked rather than remembered. A new package with tests fails the gate here, at the
 * point where it is cheap to fix, instead of failing somebody else's unrelated branch later.
 */

const workspaceRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const sharedConfig = 'vitest.shared.js'

const directoriesIn = (group) => {
  const groupRoot = join(workspaceRoot, group)
  if (!existsSync(groupRoot)) return []
  return readdirSync(groupRoot)
    .map((entry) => join(groupRoot, entry))
    .filter((entry) => statSync(entry).isDirectory())
}

const testFilesIn = (directory) => {
  const found = []
  const walk = (current) => {
    for (const entry of readdirSync(current, { withFileTypes: true })) {
      if (entry.name === 'node_modules' || entry.name.startsWith('.')) continue
      const path = join(current, entry.name)
      if (entry.isDirectory()) walk(path)
      else if (entry.name.endsWith('.test.ts')) found.push(path)
    }
  }
  walk(directory)
  return found
}

const packagesWithTests = () =>
  [...directoriesIn('packages'), ...directoriesIn('apps')].filter((directory) => {
    const manifest = join(directory, 'package.json')
    if (!existsSync(manifest)) return false
    const { scripts = {} } = JSON.parse(readFileSync(manifest, 'utf8'))
    if (typeof scripts.test !== 'string' || !scripts.test.includes('vitest')) return false
    return testFilesIn(directory).length > 0
  })

test('every package that runs vitest extends the workspace test defaults', () => {
  const offenders = packagesWithTests().filter((directory) => {
    const config = join(directory, 'vitest.config.ts')
    if (!existsSync(config)) return true
    return !readFileSync(config, 'utf8').includes(sharedConfig)
  })

  assert.deepEqual(
    offenders.map((directory) => relative(workspaceRoot, directory)),
    [],
    `Each of these runs vitest but does not extend ${sharedConfig}, so it silently takes vitest's ` +
      `5 s default timeout. Add a vitest.config.ts that re-exports the workspace defaults.`,
  )
})

test('the workspace defaults are the only place the shared numbers are written down', () => {
  const shared = readFileSync(join(workspaceRoot, 'vitest.shared.ts'), 'utf8')
  assert.match(shared, /maxWorkers/, 'vitest.shared.ts should decide the per-suite worker count')

  // A package may raise a timeout above the shared floor with a measurement that justifies it, but
  // a second `maxWorkers` is how the per-package caps this replaced came back: a local cap looks
  // like the bound, so the next person adds one instead of touching the workspace-level run.
  const capped = packagesWithTests().filter((directory) => {
    const config = join(directory, 'vitest.config.ts')
    return existsSync(config) && readFileSync(config, 'utf8').includes('maxWorkers')
  })

  assert.deepEqual(
    capped.map((directory) => relative(workspaceRoot, directory)),
    [],
    'Worker counts are bounded for the whole run by scripts/concurrency.mjs and vitest.shared.ts. ' +
      'A per-package maxWorkers bounds one package while the run stays oversubscribed.',
  )
})
