import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Result from 'effect/Result'
import { Command } from 'effect/unstable/cli'
import * as Cli from '../src/Cli.js'

it('exposes the project-first command surface without a compile alias', () => {
  const names = Cli.command.subcommands.flatMap((group) =>
    group.commands.map((command) => command.name),
  )

  assert.deepStrictEqual(names, ['build', 'check', 'format', 'run', 'build-exe'])
  assert.strictEqual(names.includes('compile'), false)
})

it.effect('parses run arguments after -- and preserves the program exit status', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* fileSystem.writeFileString(
      `${root}/silk.toml`,
      '[package]\nname = "cli-integration"\nroot = "Main.silk"\n',
    )
    yield* fileSystem.writeFileString(`${root}/Main.silk`, 'pub fn main() -> I32 { return 42 }')

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
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('rejects the removed compile subcommand', () =>
  Effect.gen(function* () {
    const executed = yield* Effect.result(
      Command.runWith(Cli.command, { version: 'test' })(['compile']),
    )
    assert.strictEqual(Result.isFailure(executed), true)
    if (Result.isFailure(executed)) assert.notStrictEqual(executed.failure._tag, 'CommandExit')
  }).pipe(Effect.provide(NodeServices.layer)),
)
