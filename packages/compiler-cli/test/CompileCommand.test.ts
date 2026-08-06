import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { NodeServices } from '@effect/platform-node'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as CompileCommand from '../src/CompileCommand.js'

/** Matches the compiler's own driver tests: the toolchain is pinned, never discovered on PATH. */
const clang = '/usr/bin/clang'

const root = mkdtempSync(join(tmpdir(), 'silk-cli-test-'))
afterAll(() => {
  rmSync(root, { recursive: true, force: true })
})

const sourceFile = (name: string, text: string): string => {
  const path = join(root, name)
  writeFileSync(path, text)
  return path
}

const run = (
  source: string,
  overrides: Partial<Parameters<typeof CompileCommand.run>[0]> = {},
): Effect.Effect<CompileCommand.ExitStatus, CompileCommand.CompilationFailed | Error> =>
  CompileCommand.run({
    source,
    output: join(root, 'out'),
    target: undefined,
    profile: 'debug',
    clang,
    saveTemps: false,
    timings: false,
    ...overrides,
  }).pipe(Effect.provide(NodeServices.layer)) as Effect.Effect<
    CompileCommand.ExitStatus,
    CompileCommand.CompilationFailed | Error
  >

it.effect('compiles a single file to a runnable executable and exits zero', () =>
  Effect.gen(function* () {
    const source = sourceFile('main.silk', 'pub fn main() -> I32 { return 42 }')
    const output = join(root, 'answer')

    const status = yield* run(source, { output })

    assert.strictEqual(status, 0)
    // The executable is the deliverable, so the test runs it rather than trusting the exit status.
    assert.strictEqual(spawnSync(output).status, 42)
  }),
)

it.effect('exits non-zero when the root module declares no entry point', () =>
  Effect.gen(function* () {
    const source = sourceFile('noentry.silk', 'pub fn other() -> I32 { return 1 }')

    const status = yield* run(source, { output: join(root, 'noentry') })

    assert.strictEqual(status, 1)
  }),
)

it.effect('exits non-zero when the source carries a semantic diagnostic', () =>
  Effect.gen(function* () {
    const source = sourceFile('bad.silk', 'pub fn main() -> Nope { return 42 }')

    const status = yield* run(source, { output: join(root, 'bad') })

    assert.strictEqual(status, 1)
  }),
)

it.effect('reports an unresolvable target rather than compiling for the host', () =>
  Effect.gen(function* () {
    const source = sourceFile('wasm.silk', 'pub fn main() -> I32 { return 42 }')

    // WebAssembly is a known target but not a native one; the driver must stop before linking.
    const status = yield* run(source, {
      output: join(root, 'wasm'),
      target: 'wasm32-unknown-unknown',
    })

    assert.strictEqual(status, 1)
  }),
)
