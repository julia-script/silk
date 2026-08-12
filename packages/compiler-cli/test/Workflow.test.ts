import { existsSync } from 'node:fs'
import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Backend from '@silk-effect/compiler/Backend'
import * as Target from '@silk-effect/compiler/Target'
import * as Effect from 'effect/Effect'
import * as Fiber from 'effect/Fiber'
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

/** Polls a watch-mode side effect, which lands on a filesystem event rather than a returned value. */
const waitUntil = Effect.fnUntraced(function* (condition: () => boolean) {
  for (let attempt = 0; attempt < 200; attempt += 1) {
    if (condition()) return
    yield* Effect.sleep('50 millis')
  }
  throw new Error('Timed out waiting for the watch mode to compile again')
})

/**
 * Rewrites a source file until the watch reacts. The watcher registers after the first
 * compilation finishes, so a single early write can land before anything is listening.
 */
const editUntilRecompiled = Effect.fnUntraced(function* (
  file: string,
  text: string,
  recompiled: () => boolean,
) {
  for (let attempt = 0; attempt < 20; attempt += 1) {
    yield* writeFile(file, text)
    for (let poll = 0; poll < 20; poll += 1) {
      if (recompiled()) return
      yield* Effect.sleep('50 millis')
    }
  }
  throw new Error('Timed out waiting for the watch mode to compile again')
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

it.effect(
  'builds to the deterministic target and profile path',
  () =>
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
  30_000,
)

it.effect(
  'builds ordered native and LLVM-Wasm targets to independent artifacts',
  () =>
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
      assert.strictEqual(
        yield* fileSystem.exists(`${root}/build/llvm/${host.id}/debug/hello`),
        true,
      )
      assert.strictEqual(
        yield* fileSystem.exists(`${root}/build/llvm/wasm32-unknown-unknown/debug/hello.wasm`),
        true,
      )
    }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
  30_000,
)

it.effect(
  'retains a successful sibling when another target rejects target-dependent source',
  () =>
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
      assert.strictEqual(
        yield* fileSystem.exists(`${root}/build/llvm/${host.id}/debug/hello`),
        true,
      )
      assert.strictEqual(
        yield* fileSystem.exists(`${root}/build/llvm/wasm32-unknown-unknown/debug/hello.wasm`),
        false,
      )
    }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
  30_000,
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

it.effect(
  'builds and returns the compiled program exact exit status',
  () =>
    Effect.gen(function* () {
      const fileSystem = yield* FileSystem.FileSystem
      const root = yield* fileSystem.makeTempDirectoryScoped()
      yield* makeProject(root)

      const status = yield* Workflow.run(options(root), ['--literal', 'argument'])

      assert.strictEqual(status, 42)
    }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
  30_000,
)

it.effect(
  'removes the build artifacts and keeps the source files',
  () =>
    Effect.gen(function* () {
      const fileSystem = yield* FileSystem.FileSystem
      const root = yield* fileSystem.makeTempDirectoryScoped()
      yield* makeProject(root)
      assert.strictEqual(yield* Workflow.build(options(root)), 0)
      assert.strictEqual(yield* fileSystem.exists(`${root}/build`), true)

      const status = yield* Workflow.clean(options(root))

      assert.strictEqual(status, 0)
      assert.strictEqual(yield* fileSystem.exists(`${root}/build`), false)
      assert.strictEqual(yield* fileSystem.exists(`${root}/src/Main.silk`), true)
      assert.strictEqual(yield* fileSystem.exists(`${root}/silk.toml`), true)
    }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
  30_000,
)

it.effect('exits zero cleaning a project that was never built', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(root)
    assert.strictEqual(yield* fileSystem.exists(`${root}/build`), false)

    assert.strictEqual(yield* Workflow.clean(options(root)), 0)
    assert.strictEqual(yield* fileSystem.exists(`${root}/src/Main.silk`), true)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.live(
  'checks again after a watched source file changes',
  () =>
    Effect.gen(function* () {
      const fileSystem = yield* FileSystem.FileSystem
      const root = yield* fileSystem.makeTempDirectoryScoped()
      yield* makeProject(root)
      const passes: Array<Workflow.ExitStatus> = []
      const record = (selection: Workflow.ProjectSelection) =>
        Workflow.check(selection).pipe(
          Effect.tap((status) => Effect.sync(() => passes.push(status))),
        )

      const watching = yield* Effect.forkChild(Workflow.watch(record, options(root)))
      yield* waitUntil(() => passes.length >= 1)
      yield* editUntilRecompiled(
        `${root}/src/Main.silk`,
        'pub fn main() -> i32 { return 7 }',
        () => passes.length >= 2,
      )
      yield* Fiber.interrupt(watching)

      assert.deepStrictEqual(passes.slice(0, 2), [0, 0])
    }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
  30_000,
)

it.live(
  'keeps watching after a compilation that reports a diagnostic',
  () =>
    Effect.gen(function* () {
      const fileSystem = yield* FileSystem.FileSystem
      const root = yield* fileSystem.makeTempDirectoryScoped()
      yield* makeProject(root, 'pub fn main() -> Mystery { return 42 }')
      const passes: Array<Workflow.ExitStatus> = []
      const record = (selection: Workflow.ProjectSelection) =>
        Workflow.check(selection).pipe(
          Effect.tap((status) => Effect.sync(() => passes.push(status))),
        )

      const watching = yield* Effect.forkChild(Workflow.watch(record, options(root)))
      yield* waitUntil(() => passes.length >= 1)
      assert.strictEqual(passes[0], 1)

      yield* editUntilRecompiled(`${root}/src/Main.silk`, source, () => passes.length >= 2)

      assert.strictEqual(passes[1], 0)
      yield* Fiber.interrupt(watching)
    }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
  30_000,
)
