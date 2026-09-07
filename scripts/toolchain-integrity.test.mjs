import assert from 'node:assert/strict'
import { execFileSync, spawn } from 'node:child_process'
import { once } from 'node:events'
import { chmod, copyFile, mkdtemp, mkdir, readFile, rm, stat, writeFile } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { setTimeout } from 'node:timers/promises'
import { test } from 'node:test'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'

const fixture = async (t) => {
  const root = await mkdtemp(join(tmpdir(), 'silk-toolchain-'))
  t.after(() => rm(root, { recursive: true, force: true }))
  await mkdir(join(root, 'src'))
  await mkdir(join(root, 'scripts'))
  for (const script of ['generate-toolchain-integrity.mjs', 'watch-toolchain.mjs'])
    await copyFile(
      new URL(`../packages/compiler/scripts/${script}`, import.meta.url),
      join(root, 'scripts', script),
    )
  const source = join(root, 'src', 'Compiler.ts')
  await writeFile(source, 'export const version = 1\n')
  const output = join(root, 'src', 'ToolchainIntegrity.generated.ts')
  const generate = (...args) =>
    execFileSync(
      process.execPath,
      [join(root, 'scripts/generate-toolchain-integrity.mjs'), ...args],
      { stdio: 'pipe' },
    )
  return { root, source, output, generate }
}

void test('identity bootstraps, stays untouched when current, and detects source changes', async (t) => {
  const { source, output, generate } = await fixture(t)
  generate()
  const original = await readFile(output, 'utf8')
  const metadata = await stat(output)
  generate()
  assert.equal((await stat(output)).mtimeMs, metadata.mtimeMs)
  generate('--check')
  await writeFile(source, 'export const version = 2\n')
  assert.throws(() => generate('--check'), /Generated toolchain identity is stale/)
  generate()
  assert.notEqual(await readFile(output, 'utf8'), original)
  generate('--check')
})

void test('development watcher refreshes identity and terminates with its compiler', async (t) => {
  const { root, source, output, generate } = await fixture(t)
  await mkdir(join(root, 'bin'))
  const executable = join(root, 'bin', 'tsc')
  await writeFile(
    executable,
    `#!${process.execPath}\nconsole.log('ready'); setInterval(() => {}, 1000)\n`,
  )
  await chmod(executable, 0o755)
  const child = spawn(process.execPath, [join(root, 'scripts/watch-toolchain.mjs')], {
    env: { PATH: `${join(root, 'bin')}:${Effect.runSync(Config.string('PATH'))}` },
    stdio: ['ignore', 'pipe', 'inherit'],
  })
  t.after(() => child.kill('SIGKILL'))
  await once(child.stdout, 'data')
  const original = await readFile(output, 'utf8')
  await writeFile(source, 'export const version = 2\n')
  for (let attempt = 0; attempt < 100; attempt++) {
    if ((await readFile(output, 'utf8')) !== original) break
    await setTimeout(50)
  }
  assert.notEqual(await readFile(output, 'utf8'), original)
  generate('--check')
  const exited = once(child, 'exit')
  child.kill('SIGTERM')
  assert.deepEqual(await exited, [null, 'SIGTERM'])
})
