import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
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
import * as TestToolchain from './support/TestToolchain.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const runtimeObjectCache = NativeToolchain.makeRuntimeObjectCache()
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

const providedBorrowedCallbackSource = `import silk.effect { Effect }
unsafe extern "C" fn observe(value: i32) -> i32
service Value { effect fn get() -> i32 ? &mut Value }
struct SuspendedValue { value: i32 }
effect fn get(self: &mut SuspendedValue) -> i32 {
  return run Effect.suspend(effect { return self.value })
}
impl Value for SuspendedValue { get: SuspendedValue.get }
struct Resource { value: i32 }
effect fn borrowedRead(resource: &mut Resource) -> i32 ? &mut Value {
  let stable = resource.value
  while true {
    let observed = unsafe observe(stable)
    if observed == 0 { return 0 }
    let ignored = run Value.get()
    drop ignored
  }
  return 0
}
effect fn scoped<'env, A, E, ?R>(
  callback: for<'call> once fn<'env>(
    &'call mut Resource
  ) -> once Effect<'call; A ! E ? R>,
) -> A ! E ? R {
  let mut resource = Resource { value: 42 }
  return run callback(&mut resource)
}
effect fn child() -> i32 ? &mut Value { return run scoped(borrowedRead) }
pub fn main() -> i32 {
  let mut provider = SuspendedValue { value: 42 }
  return run Effect.provideMut<Value>(child(), &mut provider)
}`

it.effect('runs one million suspended native recursive frames with bounded machine stack', () =>
  Effect.gen(function* () {
    const compiled = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('suspension-native/deep', ascii(recursiveSource(1_000_000))),
      },
      toolchain: { ...(yield* TestToolchain.configured), runtimeObjectCache },
      optimization: 'release',
      artifactKind: 'NativeExecutable',
      destination: join(destinationRoot, 'deep'),
    }).pipe(Effect.provide(SourceResolver.empty))

    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.path, [], { encoding: 'utf8', timeout: 60_000 })
    assert.strictEqual(run.signal, null, run.stderr)
    assert.strictEqual(run.status, 42, run.stderr)
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
    assert.deepEqual(yield* MirVerification.verify(Analysis.loweredMir(analysis)), [])
    assert.isTrue(
      Analysis.loweredMir(analysis).functions.some(
        (fn) => fn.suspension?.classification === 'Suspendable',
      ),
    )
    const artifact = yield* Analysis.codegen(analysis, { mode: 'release' })

    assert.include(artifact.ir, '$suspend_step')
    assert.include(artifact.ir, 'suspend_drive')
    assert.include(artifact.ir, 'silk_suspend_resume_')
    assert.isDefined(Analysis.loweredMir(analysis).executionStorage)
    assert.isFalse(
      artifact.nativeRuntimeSymbols.some((symbol) => symbol.startsWith('silk_coroutine_frame_')),
    )
    assert.include(artifact.ir, 'suspend_storage_create')
    assert.match(artifact.ir, /call void @silk_\S*execution_storage_release\S*\(ptr [^,]+, ptr /)
    assert.include(artifact.ir, 'suspend_initial_result_storage_release')
    assert.include(artifact.ir, 'declare ptr @malloc')
    assert.include(artifact.ir, 'declare void @free')
    assert.notInclude(artifact.ir, 'llvm.coro.')
    assert.notInclude(artifact.ir, 'musttail')
    assert.notInclude(artifact.ir, 'setjmp')
    assert.notInclude(artifact.ir, 'longjmp')
  }),
)

it.effect(
  'retains loop-carried state through a provided scoped borrowed callback',
  () =>
    Effect.gen(function* () {
      const analysis = yield* Analysis.ofSourceRealized(
        'suspension-native/provided-borrowed-callback',
        ascii(providedBorrowedCallbackSource),
        'aarch64-apple-darwin',
      )
      assert.deepEqual(Analysis.diagnostics(analysis), [])
      assert.deepEqual(yield* MirVerification.verify(Analysis.loweredMir(analysis)), [])
      const provided = Analysis.loweredMir(analysis).functions.find(
        (fn) =>
          fn.id.name.startsWith('borrowedRead$effect$-1$provided$') &&
          fn.suspension?.classification === 'Suspendable',
      )
      assert.isDefined(provided)
      assert.isTrue(
        provided.suspension?.frame?.states.some((state) =>
          state.slots.some((slot) => slot.type._tag === 'i32'),
        ),
      )
      yield* Analysis.codegen(analysis, { mode: 'debug' })
    }),
  30000,
)

it.effect('propagates a failure after a resumed retry into its native handler', () =>
  Effect.gen(function* () {
    const compiled = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('suspension-native/retry-failure', ascii(retryFailureSource)),
      },
      toolchain: { ...(yield* TestToolchain.configured), runtimeObjectCache },
      optimization: 'release',
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
