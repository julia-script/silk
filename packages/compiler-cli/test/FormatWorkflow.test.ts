import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Result from 'effect/Result'
import * as FormatWorkflow from '../src/FormatWorkflow.js'

const canonical = 'pub fn main() -> I32 {\n  return 42\n}\n'
const compact = 'pub fn main() -> I32 { return 42 }'

const writeFile = Effect.fnUntraced(function* (path: string, text: string) {
  const fileSystem = yield* FileSystem.FileSystem
  const slash = path.lastIndexOf('/')
  if (slash >= 0) yield* fileSystem.makeDirectory(path.slice(0, slash), { recursive: true })
  yield* fileSystem.writeFileString(path, text)
})

const makeProject = Effect.fnUntraced(function* (root: string, main = compact) {
  yield* writeFile(`${root}/silk.toml`, '[package]\nname = "format-app"\nroot = "src/Main.silk"\n')
  yield* writeFile(`${root}/src/Main.silk`, main)
})

it.effect('selects every exact source-root file in canonical order without reachability', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(root, canonical)
    yield* writeFile(`${root}/src/Zeta.silk`, compact)
    yield* writeFile(`${root}/src/draft/Alpha.silk`, compact)
    yield* writeFile(`${root}/src/ignored.txt`, compact)
    yield* writeFile(`${root}/src/Ignored.SILK`, compact)
    const canonicalRoot = yield* fileSystem.realPath(root)

    const summary = yield* FormatWorkflow.run({ workingDirectory: root, check: true })

    assert.deepEqual(
      summary.outcomes.map((outcome) => [
        outcome.path.slice(canonicalRoot.length + 1),
        outcome._tag,
      ]),
      [
        ['src/Main.silk', 'Unchanged'],
        ['src/Zeta.silk', 'Changed'],
        ['src/draft/Alpha.silk', 'Changed'],
      ],
    )
    assert.strictEqual(FormatWorkflow.exitStatus(summary), 1)
    assert.strictEqual(yield* fileSystem.readFileString(`${root}/src/Zeta.silk`), compact)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('restricts, deduplicates, and recursively expands positional selections', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(root, canonical)
    yield* writeFile(`${root}/src/model/B.silk`, compact)
    yield* writeFile(`${root}/src/model/A.silk`, compact)
    yield* writeFile(`${root}/src/other/C.silk`, compact)
    const canonicalRoot = yield* fileSystem.realPath(root)

    const directory = yield* FormatWorkflow.run({
      workingDirectory: root,
      paths: ['src/model', 'src/model/A.silk'],
      check: true,
    })
    const single = yield* FormatWorkflow.run({
      workingDirectory: root,
      paths: ['src/other/C.silk'],
      check: true,
    })

    assert.deepEqual(
      directory.outcomes.map((outcome) => outcome.path.slice(canonicalRoot.length + 1)),
      ['src/model/A.silk', 'src/model/B.silk'],
    )
    assert.deepEqual(
      single.outcomes.map((outcome) => outcome.path.slice(canonicalRoot.length + 1)),
      ['src/other/C.silk'],
    )
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('rejects external selections and symlink escapes while default walks skip symlinks', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(root, canonical)
    yield* writeFile(`${root}/Outside.silk`, compact)
    yield* writeFile(`${root}/outside-directory/Hidden.silk`, compact)
    yield* fileSystem.symlink(`${root}/Outside.silk`, `${root}/src/Linked.silk`)
    yield* fileSystem.symlink(`${root}/outside-directory`, `${root}/src/linked-directory`)
    const canonicalRoot = yield* fileSystem.realPath(root)

    const outside = yield* Effect.result(
      FormatWorkflow.run({ workingDirectory: root, paths: ['Outside.silk'], check: true }),
    )
    const linked = yield* Effect.result(
      FormatWorkflow.run({ workingDirectory: root, paths: ['src/Linked.silk'], check: true }),
    )
    const defaultSelection = yield* FormatWorkflow.run({ workingDirectory: root, check: true })

    assert.strictEqual(Result.isFailure(outside), true)
    assert.strictEqual(Result.isFailure(linked), true)
    if (Result.isFailure(outside))
      assert.strictEqual(outside.failure.reason._tag, 'OutsideSourceRoot')
    if (Result.isFailure(linked))
      assert.strictEqual(linked.failure.reason._tag, 'OutsideSourceRoot')
    assert.deepEqual(
      defaultSelection.outcomes.map((outcome) => outcome.path),
      [`${canonicalRoot}/src/Main.silk`],
    )
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect(
  'continues after damaged source and atomically replaces only complete changed files',
  () =>
    Effect.gen(function* () {
      const fileSystem = yield* FileSystem.FileSystem
      const root = yield* fileSystem.makeTempDirectoryScoped()
      yield* makeProject(root, compact)
      yield* writeFile(`${root}/src/Broken.silk`, 'pub fn broken() -> I32 { return 1')
      yield* fileSystem.chmod(`${root}/src/Main.silk`, 0o744)

      const summary = yield* FormatWorkflow.run({ workingDirectory: root })
      const entries = yield* fileSystem.readDirectory(`${root}/src`)
      const info = yield* fileSystem.stat(`${root}/src/Main.silk`)

      assert.deepEqual(
        summary.outcomes.map((outcome) => outcome._tag),
        ['Damaged', 'Changed'],
      )
      assert.strictEqual(FormatWorkflow.exitStatus(summary), 1)
      assert.strictEqual(yield* fileSystem.readFileString(`${root}/src/Main.silk`), canonical)
      assert.strictEqual(
        yield* fileSystem.readFileString(`${root}/src/Broken.silk`),
        'pub fn broken() -> I32 { return 1',
      )
      assert.strictEqual(info.mode & 0o777, 0o744)
      assert.strictEqual(
        entries.some((entry) => entry.startsWith('.silk-format-')),
        false,
      )
    }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('check mode performs no writes and semantic-only errors do not affect status', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    const semantic = 'pub fn main(value: Missing) -> Missing { return unknown }'
    yield* makeProject(root, semantic)

    const drift = yield* FormatWorkflow.run({ workingDirectory: root, check: true })
    assert.strictEqual(FormatWorkflow.exitStatus(drift), 1)
    assert.strictEqual(yield* fileSystem.readFileString(`${root}/src/Main.silk`), semantic)

    yield* FormatWorkflow.run({ workingDirectory: root })
    const canonicalCheck = yield* FormatWorkflow.run({ workingDirectory: root, check: true })
    assert.strictEqual(FormatWorkflow.exitStatus(canonicalCheck), 0)
    assert.deepEqual(
      canonicalCheck.outcomes.map((outcome) => outcome._tag),
      ['Unchanged'],
    )
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('reports storage failures, continues in order, and gives exit class two precedence', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(root, canonical)
    yield* writeFile(`${root}/src/A-Unreadable.silk`, compact)
    yield* writeFile(`${root}/src/Zeta.silk`, compact)

    const summary = yield* Effect.acquireUseRelease(
      fileSystem.chmod(`${root}/src/A-Unreadable.silk`, 0o000),
      () => FormatWorkflow.run({ workingDirectory: root }),
      () => fileSystem.chmod(`${root}/src/A-Unreadable.silk`, 0o644),
    )

    assert.deepEqual(
      summary.outcomes.map((outcome) => outcome._tag),
      ['Failed', 'Unchanged', 'Changed'],
    )
    assert.strictEqual(FormatWorkflow.exitStatus(summary), 2)
    assert.strictEqual(yield* fileSystem.readFileString(`${root}/src/Zeta.silk`), canonical)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('reports an atomic commit failure without replacing the original file', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* makeProject(root, compact)

    const summary = yield* Effect.acquireUseRelease(
      fileSystem.chmod(`${root}/src`, 0o555),
      () => FormatWorkflow.run({ workingDirectory: root }),
      () => fileSystem.chmod(`${root}/src`, 0o755),
    )

    assert.deepEqual(
      summary.outcomes.map((outcome) => outcome._tag),
      ['Failed'],
    )
    assert.strictEqual(FormatWorkflow.exitStatus(summary), 2)
    assert.strictEqual(yield* fileSystem.readFileString(`${root}/src/Main.silk`), compact)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)
