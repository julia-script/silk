import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import { lineTable, positionOf } from '../src/Backend.js'
import type * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import type * as Termination from '../src/Termination.js'
import * as Json from './support/Json.js'
import * as Driver from './support/TestDriver.js'

/*
 * Native program termination and reporting (TERM-003/004/006/008/010): the standalone adapter
 * names the active failure member, prints the classification, origin, and logical path, and
 * reports a fatal trap before dying. Each case also checks evaluator parity for the identity,
 * origin, and path so the two engines cannot drift apart silently.
 */

const defaultClang = (): string => {
  if (existsSync('/opt/homebrew/opt/llvm/bin/clang')) return '/opt/homebrew/opt/llvm/bin/clang'
  if (existsSync('/usr/local/opt/llvm/bin/clang')) return '/usr/local/opt/llvm/bin/clang'
  return 'clang'
}
const clang = Effect.runSync(
  Config.string('SILK_TEST_CLANG').pipe(Config.withDefault(defaultClang())),
)
const toolchain: NativeToolchain.Toolchain = Object.freeze({ _tag: 'Toolchain', clang })
const encode = (value: string): Uint8Array => new TextEncoder().encode(value)
const module = 'memory/driver'

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-native-termination-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

const compileAndRun = Effect.fnUntraced(function* (
  name: string,
  source: string,
  imports: Readonly<Record<string, string>> = {},
) {
  const sources = new Map([[module, source], ...Object.entries(imports)])
  const resolver = SourceResolver.memory(
    new Map(Object.entries(imports).map(([id, text]) => [id, encode(text)] as const)),
  )
  const snapshot = yield* Analysis.ofSourceRealized(module, encode(source)).pipe(
    Effect.provide(resolver),
  )
  assert.strictEqual(
    snapshot.mir._tag,
    'Available',
    Analysis.diagnostics(snapshot)
      .map((diagnostic) => diagnostic.code)
      .join(','),
  )
  const evaluated = Analysis.evaluate(snapshot)
  const compiled = yield* Driver.compile({
    compilation: { root: SourceFile.make(module, encode(source)) },
    toolchain,
    profile: 'release',
    destination: join(destinationRoot, name),
  }).pipe(Effect.provide(resolver))
  let detail: string = compiled._tag
  if (compiled._tag === 'BackendFailed') {
    detail = `${compiled.error.message}\n${Json.stringify(compiled.error.reason)}`
  } else if (compiled._tag === 'Rejected') {
    detail = compiled.diagnostics.map((diagnostic) => diagnostic.code).join(',')
  }
  assert.strictEqual(compiled._tag, 'Compiled', detail)
  if (compiled._tag !== 'Compiled') throw new Error('unreachable')
  return { evaluated, sources, run: spawnSync(compiled.path, [], { encoding: 'utf8' }) }
})

/** The report's `  at` frame names, innermost first, without their source positions. */
const frameLines = (stderr: string): ReadonlyArray<string> =>
  stderr
    .split('\n')
    .filter((line) => line.startsWith('  at '))
    .map((line) => line.slice('  at '.length).replace(/ \(.*\)$/, ''))

const logicalName = (frame: Termination.LogicalFrame): string =>
  `${frame.function.module}.${frame.function.name.replace(/\$effect\$-1$/, '')}`

/** Evaluator logical path, innermost first, in the report's spelling. */
const evaluatedPath = (evaluated: ReturnType<typeof Analysis.evaluate>): ReadonlyArray<string> =>
  evaluated._tag === 'UnhandledFailure' || evaluated._tag === 'Trap'
    ? [...evaluated.logicalPath].reverse().map(logicalName)
    : []

/** The evaluator origin rendered the way the native report renders it. */
const evaluatedOrigin = (
  evaluated: ReturnType<typeof Analysis.evaluate>,
  sources: ReadonlyMap<string, string>,
): string => {
  assert.strictEqual(evaluated._tag === 'UnhandledFailure' || evaluated._tag === 'Trap', true)
  if (evaluated._tag !== 'UnhandledFailure' && evaluated._tag !== 'Trap') return ''
  const innermost = evaluated.logicalPath.at(-1)
  assert.isDefined(innermost)
  const span = evaluated.provenance
  const position = positionOf(lineTable(encode(sources.get(span.sourceId) ?? '')), span.start)
  return `  at ${logicalName(innermost as Termination.LogicalFrame)} (${span.sourceId}:${position.line}:${position.column})`
}

it.effect('names the active failure-union member, not the first declared one', () =>
  Effect.gen(function* () {
    const { evaluated, sources, run } = yield* compileAndRun(
      'union-second',
      `pub struct NotFoundError {}
pub struct OfflineError {}

pub effect fn main() ! NotFoundError | OfflineError {
  fail OfflineError {}
}
`,
    )
    assert.strictEqual(evaluated._tag, 'UnhandledFailure')
    if (evaluated._tag !== 'UnhandledFailure') return
    assert.strictEqual(evaluated.identity, `${module}.OfflineError`)
    assert.strictEqual(run.status, 1)
    assert.strictEqual(
      run.stderr,
      `unhandled error: ${module}.OfflineError\n${evaluatedOrigin(evaluated, sources)}\n`,
    )

    const third = yield* compileAndRun(
      'union-third',
      `pub struct AError {}
pub struct BError {}
pub struct CError {}

effect fn pick(n: i32) -> i32 ! AError | BError | CError {
  if n == 1 { fail AError {} }
  if n == 2 { fail BError {} }
  fail CError {}
}

pub effect fn main() ! AError | BError | CError {
  let v = run pick(3)
  return ()
}
`,
    )
    assert.strictEqual(third.run.status, 1)
    assert.match(third.run.stderr, new RegExp(`^unhandled error: ${module}\\.CError\n`))
    assert.deepEqual(frameLines(third.run.stderr), [`${module}.pick`, `${module}.main`])
    assert.deepEqual(frameLines(third.run.stderr), evaluatedPath(third.evaluated))
  }),
)

it.effect('reports the origin and the logical path from the failure outward to main', () =>
  Effect.gen(function* () {
    const { evaluated, sources, run } = yield* compileAndRun(
      'trace',
      `pub struct NotFoundError {}

effect fn load() -> i32 ! NotFoundError {
  fail NotFoundError {}
}

effect fn middle() -> i32 ! NotFoundError {
  let v = run load()
  return v + 1
}

pub effect fn main() ! NotFoundError {
  let v = run middle()
  return ()
}
`,
    )
    assert.strictEqual(run.status, 1)
    const lines = run.stderr.split('\n')
    assert.strictEqual(lines[0], `unhandled error: ${module}.NotFoundError`)
    assert.strictEqual(lines[1], evaluatedOrigin(evaluated, sources))
    assert.deepEqual(frameLines(run.stderr), [
      `${module}.load`,
      `${module}.middle`,
      `${module}.main`,
    ])
    assert.deepEqual(frameLines(run.stderr), evaluatedPath(evaluated))
  }),
)

it.effect('keeps the handled failure as a while-handling cause when recovery fails', () =>
  Effect.gen(function* () {
    const { evaluated, run } = yield* compileAndRun(
      'while-handling',
      `import silk.effect { Effect }

pub struct NotFoundError {}
pub struct OfflineError {}

effect fn load() -> i32 ! NotFoundError {
  fail NotFoundError {}
}

effect fn recover(error: NotFoundError) -> i32 ! OfflineError {
  fail OfflineError {}
}

pub effect fn main() ! OfflineError {
  let v = run Effect.catch<NotFoundError>(load(), recover)
  return ()
}
`,
    )
    assert.strictEqual(evaluated._tag, 'UnhandledFailure')
    assert.strictEqual(run.status, 1)
    const [primary, cause] = run.stderr.split('while handling: ')
    assert.match(primary ?? '', new RegExp(`^unhandled error: ${module}\\.OfflineError\n`))
    assert.strictEqual(frameLines(primary ?? '').at(0), `${module}.recover`)
    assert.strictEqual(frameLines(primary ?? '').at(-1), `${module}.main`)
    assert.match(cause ?? '', new RegExp(`^${module}\\.NotFoundError\n  at ${module}\\.load \\(`))

    const recovered = yield* compileAndRun(
      'recovered-then-fails',
      `import silk.effect { Effect }

pub struct NotFoundError {}
pub struct OfflineError {}

effect fn load() -> i32 ! NotFoundError {
  fail NotFoundError {}
}

effect fn recover(error: NotFoundError) -> i32 {
  return 1
}

pub effect fn main() ! OfflineError {
  let v = run Effect.catch<NotFoundError>(load(), recover)
  fail OfflineError {}
}
`,
    )
    assert.strictEqual(recovered.run.status, 1)
    assert.notInclude(recovered.run.stderr, 'while handling')
  }),
)

it.effect('reports a fatal trap with its reason and origin before dying', () =>
  Effect.gen(function* () {
    const { evaluated, sources, run } = yield* compileAndRun(
      'trap-div',
      `fn calculate(a: i32, b: i32) -> i32 {
  return a / b
}

pub effect fn main() {
  let z = calculate(1, 0)
  return ()
}
`,
    )
    assert.strictEqual(evaluated._tag, 'Trap')
    assert.strictEqual(run.signal !== null || run.status !== 0, true)
    assert.strictEqual(
      run.stderr,
      `fatal trap: division by zero\n${evaluatedOrigin(evaluated, sources)}\n`,
    )

    const ordinary = yield* compileAndRun(
      'trap-ordinary',
      `fn calculate(a: i32, b: i32) -> i32 {
  return a / b
}

pub fn main() -> i32 {
  return calculate(1, 0)
}
`,
    )
    assert.strictEqual(ordinary.run.signal !== null || ordinary.run.status !== 0, true)
    assert.match(
      ordinary.run.stderr,
      new RegExp(`^fatal trap: division by zero\n  at ${module}\\.calculate \\(`),
    )
  }),
)

it.effect('labels frames from other modules with their own source position', () =>
  Effect.gen(function* () {
    const { run } = yield* compileAndRun(
      'multimodule',
      `import errors.kinds { NotFoundError, load }
pub effect fn main() ! NotFoundError {
  let v = run load()
  return ()
}
`,
      {
        'errors/kinds': `pub struct NotFoundError { id: i32 }
pub effect fn load() -> i32 ! NotFoundError {
  fail NotFoundError { id: 1 }
}
`,
      },
    )
    assert.strictEqual(run.status, 1)
    const lines = run.stderr.split('\n')
    assert.strictEqual(lines[0], 'unhandled error: errors/kinds.NotFoundError')
    assert.match(lines[1] ?? '', /^ {2}at errors\/kinds\.load \(errors\/kinds:[23]:\d+\)$/)
    assert.deepEqual(frameLines(run.stderr), ['errors/kinds.load', `${module}.main`])
  }),
)
