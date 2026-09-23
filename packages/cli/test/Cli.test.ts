import { assert, it } from '@effect/vitest'
import * as Console from 'effect/Console'
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
    'test',
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
        'import silk.effect { Effect }\nimport silk.logger { LogError }\nimport silk.os_logger { StdoutLogger }\n\npub effect fn main() -> () ! LogError {\n  let mut logger = StdoutLogger.make()\n\n  run Effect.log("Hello, world!", &())\n    |> Effect.provideMut(&mut logger)\n}\n',
      )
      const loaded = yield* execute(['check', '--manifest-path', `${projectRoot}/silk.toml`])
      assert.strictEqual(Result.isSuccess(loaded), true)
      const built = yield* execute(['build', '--manifest-path', `${projectRoot}/silk.toml`])
      assert.strictEqual(Result.isSuccess(built), true)
      const ran = yield* execute(['run', '--manifest-path', `${projectRoot}/silk.toml`])
      assert.strictEqual(Result.isSuccess(ran), true)
    }).pipe(Effect.scoped, Effect.provide(CompilerHost.layer)),
  // Check, build and run each drive a compiler pipeline; CI exceeds one build budget.
  3 * Timeouts.nativeBuild,
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

it.effect(
  'runs tests from an import-only root with combined runtime filters',
  () =>
    Effect.gen(function* () {
      const fileSystem = yield* FileSystem.FileSystem
      const root = yield* fileSystem.makeTempDirectoryScoped()
      yield* fileSystem.writeFileString(
        `${root}/silk.toml`,
        '[package]\nname = "test-integration"\nversion = "0.1.0"\nroot = "Main.silk"\n',
      )
      yield* fileSystem.writeFileString(`${root}/Main.silk`, 'pub fn main() -> i32 { return 9 }')
      yield* fileSystem.writeFileString(`${root}/Tests.silk`, 'import Cases')
      yield* fileSystem.writeFileString(
        `${root}/Cases.silk`,
        `import silk.effect { Effect }
service Clock { effect fn value() -> i32 ? &Clock }
struct Fixed { value: i32 }
effect fn fixedValue(self: &Fixed) -> i32 { return self.value }
impl Clock for Fixed { value: Fixed.fixedValue }
effect fn read() -> i32 ? &Clock { return run Clock.value() }
effect fn readWithFixedClock() -> () {
  let fixed = Fixed { value: 42 }
  let value = run read() |> Effect.provide<Clock>(&fixed)
  drop value
  return ()
}
test fn succeeds() {}
test effect fn fails() ! bool { fail false }
test effect fn locallyProvided() -> () ! bool {
  run readWithFixedClock()
  fail false
}`,
      )

      const executed = yield* Effect.result(
        Command.runWith(Cli.command, { version: 'test' })([
          'test',
          '--manifest-path',
          `${root}/silk.toml`,
          '--root',
          'Tests.silk',
          '--file',
          'Cases.silk',
          '--filter',
          'PROVIDED',
        ]),
      )

      assert.strictEqual(Result.isFailure(executed), true)
      if (Result.isFailure(executed)) {
        assert.strictEqual(executed.failure._tag, 'CommandExit')
        if (executed.failure._tag === 'CommandExit') assert.strictEqual(executed.failure.status, 1)
      }
    }).pipe(Effect.scoped, Effect.provide(CompilerHost.layer)),
  Timeouts.nativeBuild,
)

it.effect(
  'reuses only passing tests whose complete execution identity is unchanged',
  () =>
    Effect.gen(function* () {
      const fileSystem = yield* FileSystem.FileSystem
      const root = yield* fileSystem.makeTempDirectoryScoped()
      const nativeRoot = yield* fileSystem.realPath(root)
      yield* fileSystem.writeFileString(
        `${root}/silk.toml`,
        '[package]\nname = "test-result-cache"\nversion = "0.1.0"\nroot = "Main.silk"\n',
      )
      yield* fileSystem.writeFileString(`${root}/Main.silk`, 'pub fn main() -> i32 { return 0 }')
      yield* fileSystem.writeFileString(`${root}/Tests.silk`, 'import Cases')
      const source = (
        alphaExclusive: number,
        shared: number,
      ) => `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.filesystem { FileError, FileSystem, Path }
import silk.os_filesystem { OsFileSystem }

effect fn observe(path: string) -> () ! FileError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut fs = run OsFileSystem.make(b"${nativeRoot}") |> Effect.provideMut(&mut allocator)
  let marker = run Path.make(path) |> Effect.provideMut(&mut allocator)
  run FileSystem.removeFile(&marker) |> Effect.provideMut(&mut fs)
}

fn sharedHelper() -> i32 { return ${shared} }
fn alphaHelper() -> i32 { return sharedHelper() + ${alphaExclusive} }
fn betaHelper() -> i32 { return sharedHelper() + 2 }

test effect fn alphaPass() -> () ! FileError | OutOfMemoryError {
  let observed = alphaHelper()
  drop observed
  run observe("/alpha-marker")
}

test effect fn betaPass() -> () ! FileError | OutOfMemoryError {
  let observed = betaHelper()
  drop observed
  run observe("/beta-marker")
}

test effect fn failsEveryTime() -> () ! bool | FileError | OutOfMemoryError {
  run observe("/failure-marker")
  fail false
}`
      yield* fileSystem.writeFileString(`${root}/Cases.silk`, source(1, 40))

      const run = (arguments_: ReadonlyArray<string>) =>
        Effect.result(
          Command.runWith(Cli.command, { version: 'test' })([
            'test',
            '--manifest-path',
            `${root}/silk.toml`,
            '--root',
            'Tests.silk',
            ...arguments_,
          ]),
        )
      const seed = Effect.fnUntraced(function* (names: ReadonlyArray<string>) {
        for (const name of names) yield* fileSystem.writeFileString(`${root}/${name}`, name)
      })
      const exists = (name: string) => fileSystem.exists(`${root}/${name}`)
      const resultRecords = Effect.fnUntraced(function* () {
        const directory = `${root}/build/.silk-cache/test-results-v1`
        const names = (yield* fileSystem.readDirectory(directory)).toSorted()
        return yield* Effect.forEach(names, (name) =>
          fileSystem
            .readFile(`${directory}/${name}`)
            .pipe(Effect.map((bytes) => [name, Array.from(bytes)] as const)),
        )
      })

      yield* seed(['alpha-marker', 'beta-marker'])
      const firstRun = yield* run(['--filter', 'Pass'])
      assert.isTrue(
        Result.isSuccess(firstRun),
        Result.isFailure(firstRun) ? String(firstRun.failure) : undefined,
      )
      assert.isFalse(yield* exists('alpha-marker'))
      assert.isFalse(yield* exists('beta-marker'))

      yield* seed(['alpha-marker', 'beta-marker'])
      assert.isTrue(Result.isSuccess(yield* run(['--filter', 'pass'])))
      assert.isTrue(yield* exists('alpha-marker'))
      assert.isTrue(yield* exists('beta-marker'))

      for (let attempt = 0; attempt < 2; attempt += 1) {
        yield* seed(['failure-marker'])
        const failed = yield* run(['--filter', 'failsEveryTime'])
        assert.isTrue(Result.isFailure(failed))
        if (Result.isFailure(failed)) {
          assert.strictEqual(failed.failure._tag, 'CommandExit')
          if (failed.failure._tag === 'CommandExit') assert.strictEqual(failed.failure.status, 1)
        }
        assert.isFalse(yield* exists('failure-marker'))
      }

      yield* fileSystem.writeFileString(`${root}/Cases.silk`, source(7, 40))
      yield* seed(['alpha-marker', 'beta-marker'])
      assert.isTrue(Result.isSuccess(yield* run(['--filter', 'Pass'])))
      assert.isFalse(yield* exists('alpha-marker'))
      assert.isTrue(yield* exists('beta-marker'))

      yield* fileSystem.writeFileString(`${root}/Cases.silk`, source(7, 41))
      yield* seed(['alpha-marker', 'beta-marker'])
      assert.isTrue(Result.isSuccess(yield* run(['--filter', 'Pass'])))
      assert.isFalse(yield* exists('alpha-marker'))
      assert.isFalse(yield* exists('beta-marker'))

      yield* seed(['alpha-marker', 'beta-marker'])
      assert.isTrue(Result.isSuccess(yield* run(['--filter', 'BETAPASS'])))
      assert.isTrue(yield* exists('alpha-marker'))
      assert.isTrue(yield* exists('beta-marker'))

      const recordsBeforeBypass = yield* resultRecords()
      assert.strictEqual(recordsBeforeBypass.length, 5)
      yield* seed(['alpha-marker', 'beta-marker'])
      assert.isTrue(Result.isSuccess(yield* run(['--filter', 'Pass', '--no-cache'])))
      assert.isFalse(yield* exists('alpha-marker'))
      assert.isFalse(yield* exists('beta-marker'))
      assert.deepStrictEqual(yield* resultRecords(), recordsBeforeBypass)

      yield* seed(['alpha-marker', 'beta-marker'])
      assert.isTrue(Result.isSuccess(yield* run(['--filter', 'Pass'])))
      assert.isTrue(yield* exists('alpha-marker'))
      assert.isTrue(yield* exists('beta-marker'))
    }).pipe(Effect.scoped, Effect.provide(CompilerHost.layer)),
  // One scenario intentionally covers successive runner processes and compiler-cache reuse.
  9 * Timeouts.nativeBuild,
)

it.effect(
  'rejects a discovered broken test before applying its nonmatching runtime filter',
  () =>
    Effect.gen(function* () {
      const fileSystem = yield* FileSystem.FileSystem
      const root = yield* fileSystem.makeTempDirectoryScoped()
      yield* fileSystem.writeFileString(
        `${root}/silk.toml`,
        '[package]\nname = "test-filter-boundary"\nversion = "0.1.0"\nroot = "Main.silk"\n',
      )
      yield* fileSystem.writeFileString(
        `${root}/Main.silk`,
        `service Clock { fn now() -> i32 }
test fn selected() {}
test effect fn broken() -> () ? &Clock { return () }`,
      )

      const executed = yield* Effect.result(
        Command.runWith(Cli.command, { version: 'test' })([
          'test',
          '--manifest-path',
          `${root}/silk.toml`,
          '--filter',
          'selected',
        ]),
      )

      assert.strictEqual(Result.isFailure(executed), true)
      if (Result.isFailure(executed)) {
        assert.strictEqual(executed.failure._tag, 'CommandExit')
        if (executed.failure._tag === 'CommandExit') assert.strictEqual(executed.failure.status, 1)
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

it('lists test with its discovery purpose in root help', () => {
  const test = Cli.command.subcommands
    .flatMap((group) => group.commands)
    .find((command) => command.name === 'test')

  assert.notStrictEqual(test, undefined)
  assert.strictEqual(
    test?.description,
    'Build and run tests reachable from one project source root.',
  )
})

it.effect('documents the result-cache bypass in test help', () =>
  Effect.gen(function* () {
    const output: Array<string> = []
    const reportingConsole: Console.Console = Object.assign(Object.create(console), {
      log: (...args: ReadonlyArray<unknown>) => output.push(args.join(' ')),
      error: (...args: ReadonlyArray<unknown>) => output.push(args.join(' ')),
    })
    const executed = yield* Effect.result(
      Command.runWith(Cli.command, { version: 'test' })(['test', '--help']).pipe(
        Effect.provideService(Console.Console, reportingConsole),
      ),
    )
    assert.isTrue(Result.isSuccess(executed))
    assert.include(output.join('\n'), '--no-cache')
    assert.include(
      output.join('\n'),
      'Execute selected tests without reading or writing persistent results.',
    )
    assert.notInclude(output.join('\n'), '--watch')
  }).pipe(Effect.provide(CompilerHost.layer)),
)

it.effect('rejects the removed compile subcommand', () =>
  Effect.gen(function* () {
    const executed = yield* Effect.result(
      Command.runWith(Cli.command, { version: 'test' })(['compile']),
    )
    assert.strictEqual(Result.isFailure(executed), true)
    if (Result.isFailure(executed)) assert.notStrictEqual(executed.failure._tag, 'CommandExit')
  }).pipe(Effect.provide(CompilerHost.layer)),
)
