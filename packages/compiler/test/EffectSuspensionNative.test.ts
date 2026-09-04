import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Driver from './support/TestDriver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const clang = existsSync('/opt/homebrew/opt/llvm/bin/clang')
  ? '/opt/homebrew/opt/llvm/bin/clang'
  : '/usr/bin/clang'
const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang,
  llvmAr: 'llvm-ar',
  runtimeObjectCache: NativeToolchain.makeRuntimeObjectCache(),
})
const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-effect-suspension-native-'))

afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

const successSource = `import silk.effect { Effect }
effect fn delayed() -> i32 {
  return run Effect.suspend(effect { return 2 })
}
pub fn main() -> i32 { return run delayed() }`

const retryFailureSource = `import silk.effect { Effect }
struct Problem {}
effect fn attempt() -> i32 ! Problem {
  let resumed = run Effect.suspend(effect { return () })
  fail Problem {}
}
effect fn recover(error: Problem) -> i32 { return 7 }
pub fn main() -> i32 {
  return run Effect.catchAll(
    attempt() |> Effect.retry(1),
    recover
  )
}`

const recursiveSource = (depth: number): string => `import silk.effect { Effect }
struct Owner { value: i32 }
effect fn count(value: i32) -> i32 {
  if value == 0 { return 0 }
  let next = run Effect.suspend(effect { return value - 1 })
  let inner = run count(next)
  return inner + 1
}
effect fn retainOwner(owner: &mut Owner, value: i32) -> i32 {
  let answer = run count(value)
  return owner.value + answer - answer + 1
}
pub fn main() -> i32 {
  let mut owner = Owner { value: 41 }
  return run retainOwner(&mut owner, ${depth})
}`

it.effect('runs one million suspended native recursive frames with bounded machine stack', () =>
  Effect.gen(function* () {
    const compiled = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('suspension-native/deep', ascii(recursiveSource(1_000_000))),
      },
      toolchain,
      profile: 'release',
      artifactKind: 'NativeExecutable',
      destination: join(destinationRoot, 'deep'),
    }).pipe(Effect.provide(SourceResolver.empty))

    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.path, [], { encoding: 'utf8', timeout: 60_000 })
    assert.strictEqual(run.signal, null, run.stderr)
    assert.strictEqual(run.status, 42, run.stderr)

    const exhausted = spawnSync(compiled.path, [], {
      encoding: 'utf8',
      timeout: 60_000,
      env: { ...process.env, SILK_PRIVATE_EXECUTION_STACK_LIMIT_BYTES: '1' },
    })
    assert.isTrue(
      exhausted.signal !== null || exhausted.status !== 42,
      'private execution-stack exhaustion must terminate instead of entering Effect failure',
    )
  }),
)

it.effect('uses a private iterative native coroutine-frame protocol', () =>
  Effect.gen(function* () {
    const analysis = yield* Analysis.ofSourceRealized(
      'suspension-native/shape',
      ascii(successSource),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(analysis), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(analysis)), [])
    assert.isTrue(
      Analysis.loweredMir(analysis).functions.some(
        (fn) => fn.suspension?.classification === 'Suspendable',
      ),
    )
    const artifact = yield* Analysis.codegen(analysis, { mode: 'release' })

    assert.include(artifact.ir, '$suspend_step')
    assert.include(artifact.ir, 'suspend_drive')
    assert.include(artifact.ir, 'silk_suspend_resume_')
    assert.include(artifact.ir, 'silk_coroutine_frame_push_v1')
    assert.include(artifact.ir, 'silk_coroutine_frame_pop_v1')
    assert.notInclude(artifact.ir, 'declare ptr @malloc')
    assert.notInclude(artifact.ir, 'declare void @free')
    assert.notInclude(artifact.ir, 'llvm.coro.')
    assert.notInclude(artifact.ir, 'musttail')
    assert.notInclude(artifact.ir, 'setjmp')
    assert.notInclude(artifact.ir, 'longjmp')
  }),
)

it.effect('propagates a failure after a resumed retry into its native handler', () =>
  Effect.gen(function* () {
    const compiled = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('suspension-native/retry-failure', ascii(retryFailureSource)),
      },
      toolchain,
      profile: 'release',
      artifactKind: 'NativeExecutable',
      destination: join(destinationRoot, 'retry-failure'),
    }).pipe(Effect.provide(SourceResolver.empty))

    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
    assert.strictEqual(run.signal, null, run.stderr)
    assert.strictEqual(run.status, 7, run.stderr)
  }),
)
