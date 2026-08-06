import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Project from '../src/Project.js'
import * as Workflow from '../src/Workflow.js'

const source = 'pub fn main() -> I32 { return 42 }'

const writeFile = Effect.fnUntraced(function* (path: string, text: string) {
  const fileSystem = yield* FileSystem.FileSystem
  const slash = path.lastIndexOf('/')
  if (slash >= 0) yield* fileSystem.makeDirectory(path.slice(0, slash), { recursive: true })
  yield* fileSystem.writeFileString(path, text)
})

const makeProject = Effect.fnUntraced(function* (root: string, program = source) {
  yield* writeFile(`${root}/silk.toml`, '[package]\nname = "hello"\nroot = "src/Main.silk"\n')
  yield* writeFile(`${root}/src/Main.silk`, program)
})

const options = (workingDirectory: string): Workflow.ProjectSelection => ({
  workingDirectory,
  profile: 'debug',
})

it.effect('checks a whole project without creating build artifacts', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(root)
    yield* writeFile(`${root}/src/library/Answer.silk`, 'pub fn answer() -> I32 { return 42 }')
    yield* writeFile(
      `${root}/src/Main.silk`,
      'import library.Answer { answer }\npub fn main() -> I32 { return answer() }',
    )

    const status = yield* Workflow.check(options(root))

    assert.strictEqual(status, 0)
    assert.strictEqual(yield* fileSystem.exists(`${root}/.silk`), false)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('separates source diagnostics from operational resolver failures during check', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(root, 'pub fn main() -> Mystery { return 42 }')
    assert.strictEqual(yield* Workflow.check(options(root)), 1)

    yield* writeFile(
      `${root}/src/Main.silk`,
      'import unreadable\npub fn main() -> I32 { return 42 }',
    )
    yield* fileSystem.makeDirectory(`${root}/src/unreadable.silk`)
    assert.strictEqual(yield* Workflow.check(options(root)), 2)
    assert.strictEqual(yield* fileSystem.exists(`${root}/.silk`), false)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('builds to the deterministic target and profile path', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(root)

    const status = yield* Workflow.build(options(root))
    const project = yield* Project.load({ workingDirectory: root })

    assert.strictEqual(status, 0)
    const targetDirectory = `${root}/.silk/build/`
    const entries = yield* fileSystem.readDirectory(targetDirectory)
    assert.strictEqual(entries.length, 1)
    assert.strictEqual(
      yield* fileSystem.exists(`${targetDirectory}${entries[0]}/debug/${project.name}`),
      true,
    )
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('returns source and toolchain failure classes without leaving executables', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(root, 'pub fn main() -> Mystery { return 42 }')
    assert.strictEqual(yield* Workflow.build(options(root)), 1)

    yield* writeFile(`${root}/src/Main.silk`, source)
    const project = yield* Project.load({ workingDirectory: root })
    const destination = `${root}/broken-toolchain`
    const attempted = yield* Workflow.compile({
      entry: project.entry,
      profile: 'debug',
      destination,
      toolchain: { _tag: 'Toolchain', clang: '/silk-test/missing-clang' },
      scopeName: 'broken-toolchain',
    })
    assert.deepStrictEqual(attempted, { _tag: 'NotBuilt', status: 2 })
    assert.strictEqual(yield* fileSystem.exists(destination), false)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('builds and returns the compiled program exact exit status', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(root)

    const status = yield* Workflow.run(options(root), ['--literal', 'argument'])

    assert.strictEqual(status, 42)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)
