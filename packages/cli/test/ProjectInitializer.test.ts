import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Project from '@silklang/compiler/Project'
import * as Deferred from 'effect/Deferred'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as Fiber from 'effect/Fiber'
import * as FileSystem from 'effect/FileSystem'
import * as Layer from 'effect/Layer'
import * as ProjectInitializer from '../src/ProjectInitializer.js'

it.effect('creates the canonical sparse executable project in a missing child directory', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    const initialized = yield* ProjectInitializer.initialize({
      path: 'hello',
      workingDirectory: root,
    })
    assert.strictEqual(initialized.name, 'hello')
    assert.strictEqual(
      yield* fileSystem.readFileString(`${root}/hello/silk.toml`),
      '[package]\nname = "hello"\nversion = "0.1.0"\nroot = "src/main.silk"\n',
    )
    assert.strictEqual(yield* fileSystem.readFileString(`${root}/hello/.gitignore`), '/build/\n')
    assert.strictEqual(
      yield* fileSystem.readFileString(`${root}/hello/src/main.silk`),
      'import silk.effect { Effect }\nimport silk.logger { Logger, LogError }\n\npub effect fn main() -> () ! LogError {\n  let mut logger = Logger.stdoutProvider()\n\n  run Effect.log("Hello, world!")\n    |> Effect.provideMut(&mut logger)\n}\n',
    )
    const project = yield* Project.load({ workingDirectory: `${root}/hello` })
    assert.strictEqual(project.entry.module, 'main')
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect(
  'initializes beside unrelated files and merges gitignore without rewriting its bytes',
  () =>
    Effect.gen(function* () {
      const fileSystem = yield* FileSystem.FileSystem
      const root = yield* fileSystem.makeTempDirectoryScoped()
      yield* fileSystem.writeFileString(`${root}/README.md`, 'keep me')
      yield* fileSystem.writeFile(`${root}/.gitignore`, new TextEncoder().encode('dist/\r\n'))
      yield* ProjectInitializer.initialize({ workingDirectory: root, name: 'existing-repo' })
      assert.strictEqual(yield* fileSystem.readFileString(`${root}/README.md`), 'keep me')
      assert.deepStrictEqual(
        yield* fileSystem.readFile(`${root}/.gitignore`),
        new TextEncoder().encode('dist/\r\n/build/\n'),
      )
    }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('keeps an existing exact build rule unique', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    const ignore = new TextEncoder().encode('dist/\n/build/\n')
    yield* fileSystem.writeFile(`${root}/.gitignore`, ignore)
    yield* ProjectInitializer.initialize({ workingDirectory: root, name: 'existing-repo' })
    assert.deepStrictEqual(yield* fileSystem.readFile(`${root}/.gitignore`), ignore)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('rejects invalid names and both protected-file collisions before mutation', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    const invalid = yield* Effect.flip(
      ProjectInitializer.initialize({ path: 'Not Portable', workingDirectory: root }),
    )
    assert.strictEqual(invalid.reason._tag, 'InvalidPackageName')
    assert.include(invalid.message, '--name')
    const overridden = yield* ProjectInitializer.initialize({
      path: 'Not Portable',
      name: 'portable-name',
      workingDirectory: root,
    })
    assert.strictEqual(overridden.name, 'portable-name')

    for (const protectedPath of ['silk.toml', 'src/main.silk']) {
      const selected = `${root}/${protectedPath.replace('/', '-')}`
      yield* fileSystem.makeDirectory(`${selected}/src`, { recursive: true })
      yield* fileSystem.writeFileString(`${selected}/${protectedPath}`, 'owned')
      const collision = yield* Effect.flip(
        ProjectInitializer.initialize({ workingDirectory: selected, name: 'collision' }),
      )
      assert.strictEqual(collision.reason._tag, 'Collision')
      assert.strictEqual(yield* fileSystem.readFileString(`${selected}/${protectedPath}`), 'owned')
      assert.strictEqual(yield* fileSystem.exists(`${selected}/.gitignore`), false)
    }
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('restores exact prior state after an injected mid-write defect', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    const ignore = new TextEncoder().encode('dist/\r\n')
    yield* fileSystem.writeFile(`${root}/.gitignore`, ignore)
    const failingFileSystem = Layer.succeed(FileSystem.FileSystem, {
      ...fileSystem,
      writeFileString: (path, data, options) =>
        path.endsWith('/src/main.silk')
          ? Effect.die(new Error('injected source write defect'))
          : fileSystem.writeFileString(path, data, options),
    })
    const attempted = yield* Effect.exit(
      ProjectInitializer.initialize({ workingDirectory: root, name: 'rollback' }).pipe(
        Effect.provide(failingFileSystem),
      ),
    )
    assert.strictEqual(Exit.isFailure(attempted), true)
    assert.deepStrictEqual(yield* fileSystem.readFile(`${root}/.gitignore`), ignore)
    assert.strictEqual(yield* fileSystem.exists(`${root}/silk.toml`), false)
    assert.strictEqual(yield* fileSystem.exists(`${root}/src/main.silk`), false)
    assert.strictEqual(yield* fileSystem.exists(`${root}/src`), false)

    const nested = yield* Effect.exit(
      ProjectInitializer.initialize({
        path: 'new/deep',
        workingDirectory: root,
        name: 'rollback',
      }).pipe(Effect.provide(failingFileSystem)),
    )
    assert.strictEqual(Exit.isFailure(nested), true)
    assert.strictEqual(yield* fileSystem.exists(`${root}/new`), false)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('rolls back managed paths when initialization is interrupted', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    const started = yield* Deferred.make<void>()
    const suspendedFileSystem = Layer.succeed(FileSystem.FileSystem, {
      ...fileSystem,
      writeFileString: (path, data, options) =>
        path.endsWith('/src/main.silk')
          ? Deferred.succeed(started, undefined).pipe(Effect.andThen(Effect.never))
          : fileSystem.writeFileString(path, data, options),
    })
    const fiber = yield* Effect.forkChild(
      ProjectInitializer.initialize({
        path: 'interrupted',
        workingDirectory: root,
      }).pipe(Effect.provide(suspendedFileSystem)),
    )
    yield* Deferred.await(started)
    yield* Fiber.interrupt(fiber)
    assert.strictEqual(yield* fileSystem.exists(`${root}/interrupted`), false)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)
