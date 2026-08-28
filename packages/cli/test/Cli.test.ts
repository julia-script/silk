import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Result from 'effect/Result'
import { Command } from 'effect/unstable/cli'
import * as Cli from '../src/Cli.js'
import * as CompilerHost from './CompilerHost.js'
import * as Timeouts from './timeouts.js'

it('exposes the project-first command surface without a compile alias', () => {
  const names = Cli.command.subcommands.flatMap((group) =>
    group.commands.map((command) => command.name),
  )

  assert.deepStrictEqual(names, [
    'init',
    'build',
    'check',
    'clean',
    'doc',
    'doctest',
    'docs-site',
    'format',
    'run',
    'build-exe',
  ])
  assert.strictEqual(names.includes('compile'), false)
})

it.effect(
  'initializes a project that loads, checks, builds, and runs with defaults',
  () =>
    Effect.gen(function* () {
      const fileSystem = yield* FileSystem.FileSystem
      const root = yield* fileSystem.makeTempDirectoryScoped()
      const execute = (arguments_: ReadonlyArray<string>) =>
        Effect.result(Command.runWith(Cli.command, { version: 'test' })(arguments_))

      const projectRoot = `${root}/hello`
      const initialized = yield* execute(['init', projectRoot])
      assert.strictEqual(Result.isSuccess(initialized), true)
      assert.strictEqual(
        yield* fileSystem.readFileString(`${projectRoot}/src/main.silk`),
        'import silk.effect { Effect }\nimport silk.logger { Logger }\n\npub effect fn main() -> () ! Logger.LogError {\n  let mut logger = Logger.stdoutProvider()\n\n  run Effect.log("Hello, world!")\n    |> Effect.provideMut(&mut logger)\n}\n',
      )
      const loaded = yield* execute(['check', '--manifest-path', `${projectRoot}/silk.toml`])
      assert.strictEqual(Result.isSuccess(loaded), true)
      const built = yield* execute(['build', '--manifest-path', `${projectRoot}/silk.toml`])
      assert.strictEqual(Result.isSuccess(built), true)
      const directWasm = yield* execute([
        'build',
        '--manifest-path',
        `${projectRoot}/silk.toml`,
        '--backend',
        'wasm',
        '--target',
        'wasm32-unknown-unknown',
        '--target',
        'wasm32-unknown-unknown',
      ])
      assert.strictEqual(Result.isSuccess(directWasm), true)
      assert.strictEqual(
        yield* fileSystem.exists(
          `${projectRoot}/build/wasm/wasm32-unknown-unknown/debug/hello.wasm`,
        ),
        true,
      )
      const ran = yield* execute(['run', '--manifest-path', `${projectRoot}/silk.toml`])
      assert.strictEqual(Result.isSuccess(ran), true)
    }).pipe(Effect.scoped, Effect.provide(CompilerHost.layer)),
  Timeouts.nativeBuild,
)

it.effect(
  'parses run arguments after -- and preserves the program exit status',
  () =>
    Effect.gen(function* () {
      const fileSystem = yield* FileSystem.FileSystem
      const root = yield* fileSystem.makeTempDirectoryScoped()
      yield* fileSystem.writeFileString(
        `${root}/silk.toml`,
        '[package]\nname = "cli-integration"\nversion = "0.1.0"\nroot = "Main.silk"\n',
      )
      yield* fileSystem.writeFileString(`${root}/Main.silk`, 'pub fn main() -> i32 { return 42 }')

      const executed = yield* Effect.result(
        Command.runWith(Cli.command, { version: 'test' })([
          'run',
          '--manifest-path',
          `${root}/silk.toml`,
          '--',
          '--literal-flag',
          'value',
        ]),
      )

      assert.strictEqual(Result.isFailure(executed), true)
      if (Result.isFailure(executed)) {
        assert.strictEqual(executed.failure._tag, 'CommandExit')
        if (executed.failure._tag === 'CommandExit') assert.strictEqual(executed.failure.status, 42)
      }
    }).pipe(Effect.scoped, Effect.provide(CompilerHost.layer)),
  Timeouts.nativeBuild,
)

it('lists clean with its purpose in root help', () => {
  const clean = Cli.command.subcommands
    .flatMap((group) => group.commands)
    .find((command) => command.name === 'clean')

  assert.notStrictEqual(clean, undefined)
  assert.strictEqual(clean?.description, 'Remove the build artifacts of the nearest Silk project.')
})

it.effect('rejects the removed compile subcommand', () =>
  Effect.gen(function* () {
    const executed = yield* Effect.result(
      Command.runWith(Cli.command, { version: 'test' })(['compile']),
    )
    assert.strictEqual(Result.isFailure(executed), true)
    if (Result.isFailure(executed)) assert.notStrictEqual(executed.failure._tag, 'CommandExit')
  }).pipe(Effect.provide(CompilerHost.layer)),
)
