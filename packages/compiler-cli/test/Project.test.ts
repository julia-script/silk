import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Project from '../src/Project.js'

const source = 'pub fn main() -> I32 { return 42 }'

const writeFile = Effect.fnUntraced(function* (path: string, text: string) {
  const fileSystem = yield* FileSystem.FileSystem
  const slash = path.lastIndexOf('/')
  if (slash >= 0) yield* fileSystem.makeDirectory(path.slice(0, slash), { recursive: true })
  yield* fileSystem.writeFileString(path, text)
})

it.effect('loads the minimal manifest and defaults the source root to the entry directory', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* writeFile(`${root}/silk.toml`, '[package]\nname = "hello"\nroot = "src/Main.silk"\n')
    yield* writeFile(`${root}/src/Main.silk`, source)

    const project = yield* Project.load({ workingDirectory: root })

    assert.strictEqual(project.name, 'hello')
    assert.strictEqual(project.manifestPath, `${root}/silk.toml`)
    assert.strictEqual(project.directory, root)
    assert.strictEqual(project.entry.module, 'Main')
    assert.strictEqual(project.entry.sourceRoot, `${root}/src`)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('uses an explicit manifest-relative source root for the root module identity', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* writeFile(
      `${root}/silk.toml`,
      '[package]\nname = "nested-app"\nroot = "src/app/Main.silk"\nsource-root = "src"\n',
    )
    yield* writeFile(`${root}/src/app/Main.silk`, source)

    const project = yield* Project.load({ manifestPath: `${root}/silk.toml` })

    assert.strictEqual(project.entry.module, 'app/Main')
    assert.strictEqual(project.entry.sourceRoot, `${root}/src`)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('discovers the nearest ancestor manifest deterministically', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    const nested = `${root}/workspace/app`
    yield* writeFile(`${root}/silk.toml`, '[package]\nname = "outer"\nroot = "Main.silk"\n')
    yield* writeFile(`${root}/Main.silk`, source)
    yield* writeFile(`${nested}/silk.toml`, '[package]\nname = "inner"\nroot = "src/Main.silk"\n')
    yield* writeFile(`${nested}/src/Main.silk`, source)
    yield* fileSystem.makeDirectory(`${nested}/src/deep`, { recursive: true })

    const first = yield* Project.load({ workingDirectory: `${nested}/src/deep` })
    const second = yield* Project.load({ workingDirectory: `${nested}/src/deep` })

    assert.strictEqual(first.name, 'inner')
    assert.strictEqual(first.manifestPath, `${nested}/silk.toml`)
    assert.deepStrictEqual(first, second)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('does not fall back when an explicit manifest is absent', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* writeFile(`${root}/silk.toml`, '[package]\nname = "outer"\nroot = "Main.silk"\n')
    yield* writeFile(`${root}/Main.silk`, source)

    const error = yield* Effect.flip(
      Project.load({ workingDirectory: root, manifestPath: 'missing.toml' }),
    )

    assert.strictEqual(error.reason._tag, 'WrappedFailure')
    assert.strictEqual(error.manifestPath, `${root}/missing.toml`)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('explains when discovery reaches the filesystem root', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    const error = yield* Effect.flip(Project.load({ workingDirectory: root }))

    assert.strictEqual(error.reason._tag, 'ManifestNotFound')
    assert.strictEqual(error.message.includes('create one or pass --manifest-path'), true)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('rejects malformed TOML and invalid package names', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* writeFile(`${root}/silk.toml`, '[package\nname =')
    const malformed = yield* Effect.flip(Project.load({ workingDirectory: root }))
    assert.strictEqual(malformed.reason._tag, 'WrappedFailure')

    yield* writeFile(`${root}/silk.toml`, '[package]\nname = "Not Portable"\nroot = "Main.silk"\n')
    const invalidName = yield* Effect.flip(Project.load({ workingDirectory: root }))
    assert.strictEqual(invalidName.reason._tag, 'InvalidManifest')
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('rejects roots that are not exact Silk files or escape the source root', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* writeFile(`${root}/Main.txt`, source)
    yield* writeFile(
      `${root}/silk.toml`,
      '[package]\nname = "wrong-extension"\nroot = "Main.txt"\n',
    )
    const extension = yield* Effect.flip(Project.load({ workingDirectory: root }))
    assert.strictEqual(extension.reason._tag, 'InvalidEntry')

    yield* writeFile(`${root}/Main.silk`, source)
    yield* writeFile(
      `${root}/silk.toml`,
      '[package]\nname = "outside-root"\nroot = "Main.silk"\nsource-root = "src"\n',
    )
    const outside = yield* Effect.flip(Project.load({ workingDirectory: root }))
    assert.strictEqual(outside.reason._tag, 'InvalidEntry')
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)
