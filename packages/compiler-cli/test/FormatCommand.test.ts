import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Result from 'effect/Result'
import { Command } from 'effect/unstable/cli'
import * as Cli from '../src/Cli.js'

const compact = 'pub fn main() -> I32 { return 42 }'
const canonical = 'pub fn main() -> I32 {\n  return 42\n}\n'

const makeProject = Effect.fnUntraced(function* (root: string, source = compact) {
  const fileSystem = yield* FileSystem.FileSystem
  yield* fileSystem.makeDirectory(`${root}/src`, { recursive: true })
  yield* fileSystem.writeFileString(
    `${root}/silk.toml`,
    '[package]\nname = "format-command"\nroot = "src/Main.silk"\n',
  )
  yield* fileSystem.writeFileString(`${root}/src/Main.silk`, source)
})

const execute = (arguments_: ReadonlyArray<string>) =>
  Effect.result(Command.runWith(Cli.command, { version: 'test' })(arguments_))

const status = (
  result: Result.Result<void, { readonly _tag: string; readonly status?: number }>,
): number => {
  if (Result.isSuccess(result)) return 0
  return result.failure._tag === 'CommandExit' ? (result.failure.status ?? -1) : -1
}

it.effect('writes canonical source and then passes check mode', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(root)

    const written = yield* execute(['format', '--manifest-path', `${root}/silk.toml`])
    const checked = yield* execute(['format', '--manifest-path', `${root}/silk.toml`, '--check'])

    assert.strictEqual(status(written), 0)
    assert.strictEqual(status(checked), 0)
    assert.strictEqual(yield* fileSystem.readFileString(`${root}/src/Main.silk`), canonical)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('reports check drift without writing', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(root)

    const checked = yield* execute(['format', '--manifest-path', `${root}/silk.toml`, '--check'])

    assert.strictEqual(status(checked), 1)
    assert.strictEqual(yield* fileSystem.readFileString(`${root}/src/Main.silk`), compact)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('continues writing complete files but exits one for damaged syntax', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    const broken = 'pub fn broken() -> I32 { return 1'
    yield* makeProject(root)
    yield* fileSystem.writeFileString(`${root}/src/Broken.silk`, broken)

    const executed = yield* execute(['format', '--manifest-path', `${root}/silk.toml`])

    assert.strictEqual(status(executed), 1)
    assert.strictEqual(yield* fileSystem.readFileString(`${root}/src/Main.silk`), canonical)
    assert.strictEqual(yield* fileSystem.readFileString(`${root}/src/Broken.silk`), broken)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('ignores semantic errors and rejects selections outside the source root', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    const semantic = 'pub fn main(value: Missing) -> Missing {\n  return unknown\n}\n'
    yield* makeProject(root, semantic)
    yield* fileSystem.writeFileString(`${root}/Outside.silk`, compact)

    const checked = yield* execute(['format', '--manifest-path', `${root}/silk.toml`, '--check'])
    const invalid = yield* execute([
      'format',
      '--manifest-path',
      `${root}/silk.toml`,
      `${root}/Outside.silk`,
    ])

    assert.strictEqual(status(checked), 0)
    assert.strictEqual(status(invalid), 2)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('maps read and write failures to exit class two without corrupting source', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const readRoot = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(readRoot, canonical)
    yield* fileSystem.writeFileString(`${readRoot}/src/A-Unreadable.silk`, compact)
    const readFailure = yield* Effect.acquireUseRelease(
      fileSystem.chmod(`${readRoot}/src/A-Unreadable.silk`, 0o000),
      () => execute(['format', '--manifest-path', `${readRoot}/silk.toml`]),
      () => fileSystem.chmod(`${readRoot}/src/A-Unreadable.silk`, 0o644),
    )

    const writeRoot = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(writeRoot)
    const writeFailure = yield* Effect.acquireUseRelease(
      fileSystem.chmod(`${writeRoot}/src`, 0o555),
      () => execute(['format', '--manifest-path', `${writeRoot}/silk.toml`]),
      () => fileSystem.chmod(`${writeRoot}/src`, 0o755),
    )

    assert.strictEqual(status(readFailure), 2)
    assert.strictEqual(status(writeFailure), 2)
    assert.strictEqual(yield* fileSystem.readFileString(`${writeRoot}/src/Main.silk`), compact)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)
