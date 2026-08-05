import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import type * as NativeToolchain from '../src/NativeToolchain.js'
import { corpus } from './support/corpus.js'

const clang = '/usr/bin/clang'
const toolchain: NativeToolchain.Toolchain = Object.freeze({ _tag: 'Toolchain', clang })

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-driver-test-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

const compileSource = (
  name: string,
  text: string,
  overrides: Partial<Driver.CompileRequest> = {},
): Driver.Outcome =>
  Driver.compile({
    compilation: {
      rootModule: 'memory://driver.silk',
      sources: new Map([['memory://driver.silk', ascii(text)]]),
    },
    toolchain,
    profile: 'release',
    destination: join(destinationRoot, name),
    ...overrides,
  })

const expectedPhases = [
  'closure',
  'declaration-index',
  'elaboration',
  'ownership',
  'instance-discovery',
  'mir-lowering',
  'backend',
  'object',
  'shim',
  'link',
]

it('compiles the nested program to a running executable matching the interpreter', () => {
  const nested = corpus.find((program) => program.name === 'nested')
  assert.notStrictEqual(nested, undefined)
  if (nested === undefined) return
  const outcome = compileSource('nested', nested.source)

  assert.strictEqual(outcome._tag, 'Compiled')
  if (outcome._tag !== 'Compiled') return
  assert.strictEqual(existsSync(outcome.executable), true)
  const run = spawnSync(outcome.executable, [], { encoding: 'utf8' })
  const interpreted = Analysis.evaluate(
    Analysis.ofSource('memory://driver.silk', ascii(nested.source)),
  )
  assert.strictEqual(interpreted._tag, 'Completed')
  if (interpreted._tag !== 'Completed') return
  assert.strictEqual(run.status, interpreted.result.value)
})

it('reports every phase in order with counts and totals', () => {
  const outcome = compileSource('report', 'pub fn main() -> I32 { return 42 }')

  assert.strictEqual(outcome._tag, 'Compiled')
  if (outcome._tag !== 'Compiled') return
  assert.deepEqual(
    outcome.report.map((entry) => entry.phase),
    expectedPhases,
  )
  for (const entry of outcome.report) {
    assert.isAtLeast(entry.elapsedMs, 0, entry.phase)
    assert.isAtLeast(entry.outputs, 0, entry.phase)
    assert.isAbove(entry.heapBytes, 0, entry.phase)
  }
  const closure = outcome.report.at(0)
  assert.strictEqual(closure?.inputs, 1)
  assert.strictEqual(closure?.outputs, 1)
})

it('surfaces a missing entry as a closed outcome without invoking the toolchain', () => {
  const outcome = compileSource('no-entry', 'pub fn answer() -> I32 { return 42 }')

  assert.strictEqual(outcome._tag, 'NoEntry')
  if (outcome._tag !== 'NoEntry') return
  assert.strictEqual(outcome.reason, 'MissingEntry')
  assert.strictEqual(
    outcome.report.some((entry) => entry.phase === 'object'),
    false,
  )
})

it('names the failing native stage with command provenance', () => {
  const outcome = compileSource('bad-toolchain', 'pub fn main() -> I32 { return 42 }', {
    toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/nonexistent/clang' }),
  })

  assert.strictEqual(outcome._tag, 'Failed')
  if (outcome._tag !== 'Failed') return
  assert.strictEqual(outcome.stage, 'object')
  assert.strictEqual(outcome.failure.planned.command, '/nonexistent/clang')
})

it('keeps the interpreter and native execution in agreement across the corpus', () => {
  for (const program of corpus) {
    const interpreted = Analysis.evaluate(
      Analysis.ofSource('memory://driver.silk', ascii(program.source)),
    )
    const outcome = compileSource(`corpus-${program.name}`, program.source)

    if (program.expected._tag === 'UnavailableEntry') {
      assert.strictEqual(outcome._tag, 'NoEntry', program.name)
      continue
    }

    assert.strictEqual(outcome._tag, 'Compiled', program.name)
    if (outcome._tag !== 'Compiled') continue

    if (program.expected._tag === 'Completes') {
      assert.strictEqual(interpreted._tag, 'Completed', program.name)
      const run = spawnSync(outcome.executable, [], { encoding: 'utf8' })
      assert.strictEqual(
        run.status,
        interpreted._tag === 'Completed' ? interpreted.result.value : -1,
        `differential divergence on ${program.name}: interpreter ${
          interpreted._tag === 'Completed' ? interpreted.result.value : interpreted._tag
        }, native ${run.status}`,
      )
      continue
    }

    if (program.expected._tag === 'Trap') {
      const run = spawnSync(outcome.executable, [], { encoding: 'utf8' })
      assert.strictEqual(
        run.signal !== null || (run.status !== null && run.status !== 0),
        true,
        `differential divergence on ${program.name}: interpreter trapped, native exited ${run.status}`,
      )
    }

    // RecursiveCycle: the program must compile; native unbounded recursion is not executed.
  }
})
