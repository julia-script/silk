import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Project from '../src/Project.js'

const source = 'pub fn main() -> i32 { return 42 }'

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
    yield* writeFile(
      `${root}/silk.toml`,
      '[package]\nname = "hello"\nversion = "0.1.0"\nroot = "src/Main.silk"\n',
    )
    yield* writeFile(`${root}/src/Main.silk`, source)

    const project = yield* Project.load({ workingDirectory: root })

    assert.strictEqual(project.name, 'hello')
    assert.strictEqual(project.version, '0.1.0')
    assert.strictEqual(project.manifestPath, `${root}/silk.toml`)
    assert.strictEqual(project.directory, root)
    assert.strictEqual(project.entry.module, 'Main')
    assert.strictEqual(project.entry.sourceRoot, `${root}/src`)
    assert.deepStrictEqual(project.build, {
      backend: 'llvm',
      targets: ['host'],
      outputDirectory: `${root}/build`,
      artifact: 'NativeExecutable',
      nativeLinkInputs: [],
    })
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('uses an explicit manifest-relative source root for the root module identity', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* writeFile(
      `${root}/silk.toml`,
      '[package]\nname = "nested-app"\nversion = "0.1.0"\nroot = "src/app/Main.silk"\nsource-root = "src"\n',
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
    yield* writeFile(
      `${root}/silk.toml`,
      '[package]\nname = "outer"\nversion = "0.1.0"\nroot = "Main.silk"\n',
    )
    yield* writeFile(`${root}/Main.silk`, source)
    yield* writeFile(
      `${nested}/silk.toml`,
      '[package]\nname = "inner"\nversion = "0.1.0"\nroot = "src/Main.silk"\n',
    )
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
    yield* writeFile(
      `${root}/silk.toml`,
      '[package]\nname = "outer"\nversion = "0.1.0"\nroot = "Main.silk"\n',
    )
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

    yield* writeFile(
      `${root}/silk.toml`,
      '[package]\nname = "Not Portable"\nversion = "0.1.0"\nroot = "Main.silk"\n',
    )
    const invalidName = yield* Effect.flip(Project.load({ workingDirectory: root }))
    assert.strictEqual(invalidName.reason._tag, 'InvalidManifest')
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('materializes explicit build configuration and WebAssembly selection', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* writeFile(`${root}/src/Main.silk`, source)
    yield* writeFile(
      `${root}/silk.toml`,
      '[package]\nname = "configured"\nversion = "1.2.3-beta.1+build.7"\nroot = "src/Main.silk"\n\n[build]\nbackend = "llvm"\ntargets = ["host", "wasm32-unknown-unknown"]\noutput-dir = "artifacts"\nartifact = "shared-library"\nnative-link-inputs = [{ search-path = "native/lib" }, { library = "c", mode = "dynamic" }, { object = "native/add.o" }, { static-archive = "native/libmath.a" }, { framework = "CoreFoundation" }]\n',
    )
    const configured = yield* Project.load({ workingDirectory: root })
    assert.deepStrictEqual(configured.build, {
      backend: 'llvm',
      targets: ['host', 'wasm32-unknown-unknown'],
      outputDirectory: `${root}/artifacts`,
      artifact: 'NativeSharedLibrary',
      nativeLinkInputs: [
        { _tag: 'SearchPath', path: `${root}/native/lib` },
        { _tag: 'Library', name: 'c', mode: 'Dynamic' },
        { _tag: 'Object', path: `${root}/native/add.o` },
        { _tag: 'StaticArchive', path: `${root}/native/libmath.a` },
        { _tag: 'Framework', name: 'CoreFoundation' },
      ],
    })

    yield* writeFile(
      `${root}/silk.toml`,
      '[package]\nname = "configured"\nversion = "1.2.3"\nroot = "src/Main.silk"\n\n[build]\nbackend = "llvm"\ntargets = ["wasm32-unknown-unknown"]\n',
    )
    const wasm = yield* Project.load({ workingDirectory: root })
    assert.deepStrictEqual(wasm.build.targets, ['wasm32-unknown-unknown'])
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('rejects malformed build metadata before loading the entry', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    const invalid = [
      '[package]\nname = "invalid"\nversion = "1.0"\nroot = "Main.silk"\n',
      '[package]\nname = "invalid"\nversion = "1.0.0"\nroot = "Main.silk"\n[build]\ntargets = []\n',
      '[package]\nname = "invalid"\nversion = "1.0.0"\nroot = "Main.silk"\n[build]\nbackend = "native"\n',
      '[package]\nname = "invalid"\nversion = "1.0.0"\nroot = "Main.silk"\n[build]\noutput-dir = "../outside"\n',
      '[package]\nname = "invalid"\nversion = "1.0.0"\nroot = "Main.silk"\n[build]\nartifact = "dynamic-library"\n',
      '[package]\nname = "invalid"\nversion = "1.0.0"\nroot = "Main.silk"\n[build]\nnative-link-inputs = [{ library = "-Wl,--export-dynamic", mode = "dynamic" }]\n',
      '[package]\nname = "invalid"\nversion = "1.0.0"\nroot = "Main.silk"\n[build]\nnative-link-inputs = [{ library = "m", mode = "dynamic", object = "m.o" }]\n',
      '[package]\nname = "invalid"\nversion = "1.0.0"\nroot = "Main.silk"\n[build]\nnative-link-inputs = [{ object = "../outside.o" }]\n',
      '[package]\nname = "invalid"\nversion = "1.0.0"\nroot = "Main.silk"\n[build]\nnative-link-inputs = "m"\n',
    ]
    for (const manifest of invalid) {
      yield* writeFile(`${root}/silk.toml`, manifest)
      const error = yield* Effect.flip(Project.load({ workingDirectory: root }))
      assert.strictEqual(error.reason._tag, 'InvalidManifest')
      if (error.reason._tag === 'InvalidManifest' && manifest.includes('native-link-inputs'))
        assert.include(error.reason.detail, 'build.native-link-inputs')
    }
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('rejects roots that are not exact Silk files or escape the source root', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* writeFile(`${root}/Main.txt`, source)
    yield* writeFile(
      `${root}/silk.toml`,
      '[package]\nname = "wrong-extension"\nversion = "0.1.0"\nroot = "Main.txt"\n',
    )
    const extension = yield* Effect.flip(Project.load({ workingDirectory: root }))
    assert.strictEqual(extension.reason._tag, 'InvalidEntry')

    yield* writeFile(`${root}/Main.silk`, source)
    yield* writeFile(
      `${root}/silk.toml`,
      '[package]\nname = "outside-root"\nversion = "0.1.0"\nroot = "Main.silk"\nsource-root = "src"\n',
    )
    const outside = yield* Effect.flip(Project.load({ workingDirectory: root }))
    assert.strictEqual(outside.reason._tag, 'InvalidEntry')
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)
