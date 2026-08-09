import { existsSync } from 'node:fs'
import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Backend from '@silk-effect/compiler/Backend'
import * as Target from '@silk-effect/compiler/Target'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Project from '../src/Project.js'
import * as Workflow from '../src/Workflow.js'

const source = 'pub fn main() -> i32 { return 42 }'

const wasmClang =
  process.env.SILK_TEST_CLANG ??
  (existsSync('/opt/homebrew/opt/llvm/bin/clang')
    ? '/opt/homebrew/opt/llvm/bin/clang'
    : existsSync('/usr/local/opt/llvm/bin/clang')
      ? '/usr/local/opt/llvm/bin/clang'
      : 'clang')

const writeFile = Effect.fnUntraced(function* (path: string, text: string) {
  const fileSystem = yield* FileSystem.FileSystem
  const slash = path.lastIndexOf('/')
  if (slash >= 0) yield* fileSystem.makeDirectory(path.slice(0, slash), { recursive: true })
  yield* fileSystem.writeFileString(path, text)
})

const makeProject = Effect.fnUntraced(function* (root: string, program = source) {
  yield* writeFile(
    `${root}/silk.toml`,
    '[package]\nname = "hello"\nversion = "0.1.0"\nroot = "src/Main.silk"\n',
  )
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
    yield* writeFile(`${root}/src/library/Answer.silk`, 'pub fn answer() -> i32 { return 42 }')
    yield* writeFile(
      `${root}/src/Main.silk`,
      'import library.Answer { answer }\npub fn main() -> i32 { return answer() }',
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
      'import unreadable\npub fn main() -> i32 { return 42 }',
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
    const host = yield* Target.host()
    assert.strictEqual(
      yield* fileSystem.exists(`${root}/build/llvm/${host.id}/debug/${project.name}`),
      true,
    )
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('builds ordered native and LLVM-Wasm targets to independent artifacts', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(root)
    yield* fileSystem.writeFileString(
      `${root}/silk.toml`,
      '[package]\nname = "hello"\nversion = "0.1.0"\nroot = "src/Main.silk"\n\n[build]\nbackend = "llvm"\ntargets = ["host", "wasm32-unknown-unknown"]\n',
    )
    assert.strictEqual(yield* Workflow.build({ ...options(root), clang: wasmClang }), 0)
    const host = yield* Target.host()
    assert.strictEqual(yield* fileSystem.exists(`${root}/build/llvm/${host.id}/debug/hello`), true)
    assert.strictEqual(
      yield* fileSystem.exists(`${root}/build/llvm/wasm32-unknown-unknown/debug/hello.wasm`),
      true,
    )
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('retains a successful sibling when another target rejects target-dependent source', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(
      root,
      `fn exact() -> usize { return 9007199254740993 }
pub fn main() -> i32 {
  if exact() == 9007199254740993 { return 42 }
  return 0
}`,
    )
    const status = yield* Workflow.build({
      ...options(root),
      targets: ['host', 'wasm32-unknown-unknown'],
    })
    const host = yield* Target.host()
    assert.strictEqual(status, 1)
    assert.strictEqual(yield* fileSystem.exists(`${root}/build/llvm/${host.id}/debug/hello`), true)
    assert.strictEqual(
      yield* fileSystem.exists(`${root}/build/llvm/wasm32-unknown-unknown/debug/hello.wasm`),
      false,
    )
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('attempts a source rejection and operational failure, preferring exit two', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(
      root,
      `fn exact() -> usize { return 9007199254740993 }
pub fn main() -> i32 {
  if exact() == 9007199254740993 { return 42 }
  return 0
}`,
    )
    const status = yield* Workflow.build({
      ...options(root),
      targets: ['wasm32-unknown-unknown', 'host'],
      clang: '/nonexistent/clang',
    })
    assert.strictEqual(status, 2)
    assert.strictEqual(yield* fileSystem.exists(`${root}/build`), true)
    const host = yield* Target.host()
    assert.strictEqual(yield* fileSystem.exists(`${root}/build/llvm/${host.id}/debug/hello`), false)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('preflights incompatible batches before creating output', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(root)
    const status = yield* Workflow.build({ ...options(root), backend: 'wasm', targets: ['host'] })
    assert.strictEqual(status, 2)
    assert.strictEqual(yield* fileSystem.exists(`${root}/build`), false)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('checks every configured target without creating output and keeps run host-only', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(root)
    assert.strictEqual(
      yield* Workflow.check({
        ...options(root),
        targets: ['host', 'wasm32-unknown-unknown'],
      }),
      0,
    )
    assert.strictEqual(yield* fileSystem.exists(`${root}/build`), false)
    assert.strictEqual(yield* Workflow.run({ ...options(root), backend: 'wasm' }), 2)
    assert.strictEqual(yield* fileSystem.exists(`${root}/build`), false)
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
      backend: Backend.LlvmBackend,
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
