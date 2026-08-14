import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as Mir from '../src/Mir.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const clang = existsSync('/opt/homebrew/opt/llvm/bin/clang')
  ? '/opt/homebrew/opt/llvm/bin/clang'
  : '/usr/bin/clang'
const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang,
  shimCache: NativeToolchain.makeShimCache(),
})
const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-effect-suspension-native-'))

afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

const successSource = `effect fn delayed() -> i32 ! OutOfMemory ? &mut Allocator {
  return run Effect.suspend(effect { return 2 })
}
effect fn recover(error: OutOfMemory) -> i32 { return 7 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catch(delayed() |> Effect.provideMut(&mut allocator), recover)
}`

const retainedStateSource = `effect fn delayed() -> i32 ! OutOfMemory ? &mut Allocator {
  let left = 40
  return left + run Effect.suspend(effect { return 2 })
}
effect fn recover(error: OutOfMemory) -> i32 { return 7 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catch(delayed() |> Effect.provideMut(&mut allocator), recover)
}`

const typedFailureSource = `struct Problem { code: i32 }
effect fn delayed() -> i32 ! Problem | OutOfMemory ? &mut Allocator {
  let value = run Effect.suspend(effect { return 2 })
  if value == 2 { fail Problem { code: 35 } }
  return value
}
effect fn recover(error: Problem | OutOfMemory) -> i32 { return 42 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  let handled = delayed() |> Effect.catch(recover)
  return run (move handled |> Effect.provideMut(&mut allocator))
}`

const recursiveSource = (
  depth: number,
): string => `effect fn count(value: i32) -> i32 ! OutOfMemory ? &mut Allocator {
  if value == 0 { return 0 }
  let next = run Effect.suspend(effect { return value - 1 })
  let inner = run count(next)
  return inner + 1
}
effect fn recover(error: OutOfMemory) -> i32 { return 1 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  let answer = run Effect.catch(count(${depth}) |> Effect.provideMut(&mut allocator), recover)
  if answer == ${depth} { return 42 }
  return 2
}`

it.effect('resumes native continuation frames from inner to outer', () =>
  Effect.gen(function* () {
    const compiled = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('suspension-native/success', ascii(successSource)),
      },
      toolchain,
      profile: 'release',
      destination: join(destinationRoot, 'success'),
    }).pipe(Effect.provide(SourceResolver.empty))

    assert.strictEqual(
      compiled._tag,
      'Compiled',
      compiled._tag === 'Rejected'
        ? compiled.diagnostics.map((diagnostic) => diagnostic.message).join('\n')
        : compiled._tag === 'BackendFailed'
          ? compiled.error.message
          : undefined,
    )
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
    assert.strictEqual(run.signal, null, run.stderr)
    assert.strictEqual(run.status, 2, run.stderr)
  }),
)

it.effect('restores native scalar state retained after run', () =>
  Effect.gen(function* () {
    const compiled = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('suspension-native/state', ascii(retainedStateSource)),
      },
      toolchain,
      profile: 'release',
      destination: join(destinationRoot, 'state'),
    }).pipe(Effect.provide(SourceResolver.empty))

    assert.strictEqual(
      compiled._tag,
      'Compiled',
      compiled._tag === 'Rejected'
        ? compiled.diagnostics.map((diagnostic) => diagnostic.message).join('\n')
        : compiled._tag === 'BackendFailed'
          ? compiled.error.message
          : undefined,
    )
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
    assert.strictEqual(run.signal, null, run.stderr)
    assert.strictEqual(run.status, 42, run.stderr)
  }),
)

it.effect('propagates a resumed typed failure through native handlers', () =>
  Effect.gen(function* () {
    const analysis = yield* Analysis.ofSourceRealized(
      'suspension-native/failure',
      ascii(typedFailureSource),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Mir.verify(Analysis.loweredMir(analysis)), [])
    const compiled = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('suspension-native/failure', ascii(typedFailureSource)),
      },
      toolchain,
      profile: 'release',
      destination: join(destinationRoot, 'failure'),
    }).pipe(Effect.provide(SourceResolver.empty))

    assert.strictEqual(
      compiled._tag,
      'Compiled',
      compiled._tag === 'Rejected'
        ? compiled.diagnostics.map((diagnostic) => diagnostic.message).join('\n')
        : compiled._tag === 'BackendFailed'
          ? compiled.error.message
          : undefined,
    )
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
    assert.strictEqual(run.signal, null, run.stderr)
    assert.strictEqual(run.status, 42, run.stderr)
  }),
)

it.effect('runs one million suspended native recursive frames with bounded machine stack', () =>
  Effect.gen(function* () {
    const compiled = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('suspension-native/deep', ascii(recursiveSource(1_000_000))),
      },
      toolchain,
      profile: 'release',
      destination: join(destinationRoot, 'deep'),
    }).pipe(Effect.provide(SourceResolver.empty))

    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.path, [], { encoding: 'utf8', timeout: 60_000 })
    assert.strictEqual(run.signal, null, run.stderr)
    assert.strictEqual(run.status, 42, run.stderr)
  }),
)

it.effect('uses a private iterative native suspension protocol', () =>
  Effect.gen(function* () {
    const analysis = yield* Analysis.ofSourceRealized(
      'suspension-native/shape',
      ascii(successSource),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(analysis), [])
    assert.deepEqual(Mir.verify(Analysis.loweredMir(analysis)), [])
    assert.isTrue(
      Analysis.loweredMir(analysis).functions.some(
        (fn) => fn.suspension?.classification === 'Suspendable',
      ),
    )
    const artifact = yield* Analysis.codegen(analysis, { mode: 'release' })

    assert.include(artifact.ir, '$suspend_step')
    assert.include(artifact.ir, 'suspend_drive')
    assert.include(artifact.ir, 'silk_suspend_resume_')
    assert.notInclude(artifact.ir, 'llvm.coro.')
    assert.notInclude(artifact.ir, 'musttail')
    assert.notInclude(artifact.ir, 'setjmp')
    assert.notInclude(artifact.ir, 'longjmp')
  }),
)
