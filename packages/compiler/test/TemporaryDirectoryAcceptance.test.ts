import { spawnSync } from 'node:child_process'
import { existsSync, mkdirSync, mkdtempSync, readdirSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as IntrinsicAvailability from '../src/IntrinsicAvailability.js'
import type * as OsFileSystemHost from '../src/OsFileSystemHost.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Driver from './support/TestDriver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const nativeRoot = mkdtempSync(join(tmpdir(), 'silk-temporary-directory-'))
const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-temporary-directory-artifacts-'))
afterAll(() => {
  rmSync(nativeRoot, { recursive: true, force: true })
  rmSync(destinationRoot, { recursive: true, force: true })
})

/**
 * `TemporaryDirectory` has no `Drop` hook, so every one of these assertions is about an *explicit*
 * release. That is the decision this ticket settled: every removal path is
 * `effect fn ... ! FileError ? &mut FileSystem`, a `Drop` hook may carry neither row, and the only
 * way to fit one would have been an infallible intrinsic wrapping a fallible syscall. So release is
 * named at the call site — `release` when the caller wants the failure, `releaseIgnored` when it is
 * being handed to `Effect.ensuring`, whose finalizer is typed `! never`.
 */
const prelude = `import silk.os_filesystem { make as osMake }
import silk.filesystem {
  FileError, FileSystem, Path, TemporaryDirectory,
  createDirectoriesRecursively, exists, fromBytes as pathFromBytes, join as pathJoin,
  make as pathMake, rawBytes as pathRawBytes, release, releaseIgnored,
  removeDirectoryRecursively, temporaryDirectory, view as pathView
}
import silk.result { Failure, Result, Success }

struct Sentinel { code: i32 }

/// Work that fails, so the finalizer's run is observable on the failure path rather than assumed.
effect fn failingWork() -> i32 ! Sentinel { fail Sentinel { code: 7 } }

/// Reads the failure's own payload back, so what is asserted is *which* failure survived.
effect fn recoverSentinel(error: Sentinel) -> i32 { return error.code }`

const epilogue = `import silk.core { OutOfMemoryError }
import silk.filesystem { FileError }
import silk.result { Result }
pub fn main() -> i32 {
  let completed = run Intrinsic.effectResult(program())
  return match move completed {
    Result<i32, FileError | OutOfMemoryError> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<FileError | OutOfMemoryError> { error } => match move error {
        FileError failure => 100 + failure.reason.code
        OutOfMemoryError exhausted => 99
      }
    }
  }
}`

/**
 * The whole lifecycle against a real confined root. Each numbered return is one acceptance
 * criterion, so a native exit status names which one failed rather than merely that one did.
 */
const nativeSource = `import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effect as Effect
import silk.filesystem { FileError }
import silk.filesystem { FileSystem }
import silk.u8 as u8
${prelude}

effect fn program() -> i32 ! FileError | OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let mut fs = run osMake("${nativeRoot}") |> Effect.provideMut(&mut allocator)
  let parent = run pathMake("/scopes") |> Effect.provideMut(&mut allocator)
  let prepared = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(createDirectoriesRecursively(&parent), &mut fs),
    &mut allocator
  )
  let first = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(temporaryDirectory(&parent, "silk-build-"), &mut fs),
    &mut allocator
  )
  let second = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(temporaryDirectory(&parent, "silk-build-"), &mut fs),
    &mut allocator
  )

  // Two makes, two paths.
  if pathView(&first.path) == pathView(&second.path) { return 1 }

  // The made directory is there.
  if run Intrinsic.bindRequirementMut(exists(&first.path), &mut fs) {} else { return 2 }

  // The artifact a caller keeps: written to a durable path outside the scope before it releases.
  let payload = [u8.toU8(1), u8.toU8(2), u8.toU8(3)]
  let durable = run pathJoin(&parent, "promoted.bin") |> Effect.provideMut(&mut allocator)
  let promoted = run Intrinsic.bindRequirementMut(FileSystem.writeFile(&durable, &payload), &mut fs)

  let released = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(release(move first), &mut fs),
    &mut allocator
  )
  let releasedSecond = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(release(move second), &mut fs),
    &mut allocator
  )
  return 42
}

${epilogue}`

/**
 * Recursive removal against a real populated tree. `release` removes contents, not just an empty
 * directory, and the primitive underneath removes exactly one *empty* directory — so the two-pass
 * walk is the part that has to be right, and this is it running on a real filesystem.
 */
const nativeTreeSource = `import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effect as Effect
import silk.filesystem { FileError }
${prelude}

effect fn program() -> i32 ! FileError | OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let mut fs = run osMake("${nativeRoot}") |> Effect.provideMut(&mut allocator)
  let target = run pathMake("/tree") |> Effect.provideMut(&mut allocator)
  let removed = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(removeDirectoryRecursively(&target), &mut fs),
    &mut allocator
  )
  return 42
}

${epilogue}`

/**
 * Several populated scopes, each released while the others are still owned. This is the shape that
 * trapped at `-O0` while #130's crash was being narrowed: one owner's cleanup is a conditional arm,
 * and the values the arm reloads are read again at the arm's join. Both a populated tree and a live
 * neighbour are needed — two bare scopes released in sequence do not reach it.
 */
const nativeManySource = `import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effect as Effect
import silk.filesystem { FileError }
import silk.filesystem { FileSystem }
import silk.u8 as u8
${prelude}

effect fn program() -> i32 ! FileError | OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let mut fs = run osMake("${nativeRoot}") |> Effect.provideMut(&mut allocator)
  let parent = run pathMake("/many") |> Effect.provideMut(&mut allocator)
  let prepared = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(createDirectoriesRecursively(&parent), &mut fs),
    &mut allocator
  )
  let payload = [u8.toU8(1), u8.toU8(2), u8.toU8(3)]
${[0, 1, 2]
  .map(
    (index) => `  let scope${index} = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(temporaryDirectory(&parent, "silk-many${index}-"), &mut fs),
    &mut allocator
  )
  let nested${index} = run pathJoin(&scope${index}.path, "nested") |> Effect.provideMut(&mut allocator)
  let madeNested${index} = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(createDirectoriesRecursively(&nested${index}), &mut fs),
    &mut allocator
  )
  let file${index} = run pathJoin(&nested${index}, "payload.bin") |> Effect.provideMut(&mut allocator)
  let wrote${index} = run Intrinsic.bindRequirementMut(FileSystem.writeFile(&file${index}, &payload), &mut fs)
  if run Intrinsic.bindRequirementMut(exists(&scope${index}.path), &mut fs) {} else { return ${index + 1} }`,
  )
  .join('\n')}
${[0, 1, 2]
  .map(
    (index) => `  let released${index} = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(release(move scope${index}), &mut fs),
    &mut allocator
  )`,
  )
  .join('\n')}
  return 42
}

${epilogue}`

/**
 * The provider protocol on the evaluator: the provider chooses the name, so the created name comes
 * back through the output buffer, and a buffer too small creates nothing and reports the capacity
 * it needs. The retry counter is what proves the second half.
 */
const evaluatorSource = `import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effect as Effect
import silk.filesystem { FileError }
${prelude}

effect fn program() -> i32 ! FileError | OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let mut fs = run osMake("/root") |> Effect.provideMut(&mut allocator)
  let parent = run pathMake("/scopes") |> Effect.provideMut(&mut allocator)
  let scope = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(temporaryDirectory(&parent, "silk-"), &mut fs),
    &mut allocator
  )
  if pathView(&scope.path) != "/scopes/silk-0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" {
    return 1
  }
  let released = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(release(move scope), &mut fs),
    &mut allocator
  )
  return 42
}

${epilogue}`

const completed: OsFileSystemHost.CommandResult = Object.freeze({ _tag: 'Completed' })
const end: OsFileSystemHost.DirectoryNextResult = Object.freeze({ _tag: 'End' })
const unsupported: OsFileSystemHost.Failure = Object.freeze({
  _tag: 'Failure',
  reason: 'Unsupported',
})

it.effect(
  'creates a unique directory, removes it with its contents, and keeps a promoted artifact',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'temporary-directory/native',
        ascii(nativeSource),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('temporary-directory/native', ascii(nativeSource)),
        },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        // Release, so this also stands as the regression test for #130: the backend used to let
        // a cleanup arm's reloaded lanes escape into the arm's join block, which is invalid SSA,
        // and Clang crashed on it at -O2 instead of diagnosing it.
        profile: 'release',
        destination: join(destinationRoot, 'native'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled', JSON.stringify(compiled).slice(0, 2500))
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(
        run.status,
        42,
        JSON.stringify({ signal: run.signal, stderr: run.stderr, stdout: run.stdout }),
      )
      // The same statement read off the real filesystem rather than through the program: both
      // scopes are gone and the promoted artifact is the only thing the parent still holds.
      assert.deepEqual(readdirSync(join(nativeRoot, 'scopes')), ['promoted.bin'])
    }),
  240_000,
)

it.effect(
  'removes a directory that still has contents, including a nested one',
  () =>
    Effect.gen(function* () {
      mkdirSync(join(nativeRoot, 'tree', 'nested'), { recursive: true })
      writeFileSync(join(nativeRoot, 'tree', 'shallow.bin'), 'a')
      writeFileSync(join(nativeRoot, 'tree', 'nested', 'deep.bin'), 'b')
      const snapshot = yield* Analysis.ofSourceRealized(
        'temporary-directory/native-tree',
        ascii(nativeTreeSource),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('temporary-directory/native-tree', ascii(nativeTreeSource)),
        },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        // Release for the same reason as above — this is the walk #130 crashed on.
        profile: 'release',
        destination: join(destinationRoot, 'native-tree'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled', JSON.stringify(compiled).slice(0, 2500))
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(run.status, 42, JSON.stringify({ signal: run.signal, stderr: run.stderr }))
      // The file, the nested directory's file, the nested directory, and the directory itself.
      assert.isFalse(existsSync(join(nativeRoot, 'tree')))
    }),
  240_000,
)

it.effect(
  'releases each of several populated scopes while the rest are still owned',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'temporary-directory/native-many',
        ascii(nativeManySource),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('temporary-directory/native-many', ascii(nativeManySource)),
        },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        profile: 'release',
        destination: join(destinationRoot, 'native-many'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled', JSON.stringify(compiled).slice(0, 2500))
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      // Before #130 was fixed this trapped (SIGILL) even at -O0: a cleanup arm's reloaded lanes
      // escaped into the arm's join block, so the join read an undefined union tag and fell
      // through to the invalid-tag trap. The status, not just the absence of a crash, is what
      // says the releases actually ran.
      assert.strictEqual(
        run.status,
        42,
        JSON.stringify({ signal: run.signal, stderr: run.stderr, stdout: run.stdout }),
      )
      assert.deepEqual(readdirSync(join(nativeRoot, 'many')), [])
    }),
  360_000,
)

it.effect(
  'retries a name that does not fit without creating anything, then joins the one that does',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'temporary-directory/evaluator',
        ascii(evaluatorSource),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const chosen = `silk-${'0123456789abcdef'.repeat(4)}`
      let retries = 0
      let created = 0
      let removed = 0
      const provider: OsFileSystemHost.Provider = {
        fileOpen: () => unsupported,
        fileRead: () => unsupported,
        fileWrite: () => unsupported,
        directoryOpen: () => ({ _tag: 'Opened', handle: { identity: 1, kind: 'Directory' } }),
        directoryNext: () => end,
        pathInspect: () => unsupported,
        directoryCreate: () => completed,
        directoryCreateUnique: (_root, _parent, _prefix, capacity) => {
          const name = Array.from(chosen, (character) => character.charCodeAt(0))
          if (capacity < name.length) {
            retries += 1
            return { _tag: 'BufferTooSmall', requiredCapacity: name.length }
          }
          created += 1
          return { _tag: 'Created', name }
        },
        fileRemove: () => completed,
        directoryRemove: () => {
          removed += 1
          return completed
        },
        handleClose: () => completed,
      }

      const evaluated = Analysis.evaluate(snapshot, { osFileSystem: provider })
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
      // Exactly one short buffer, one creation for it, and one directory removed by the release.
      assert.strictEqual(retries, 1)
      assert.strictEqual(created, 1)
      assert.strictEqual(removed, 1)
    }),
)

it.effect('reports the provider failure that stopped a make', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'temporary-directory/evaluator-failure',
      ascii(evaluatorSource),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const provider: OsFileSystemHost.Provider = {
      fileOpen: () => unsupported,
      fileRead: () => unsupported,
      fileWrite: () => unsupported,
      directoryOpen: () => unsupported,
      directoryNext: () => end,
      pathInspect: () => unsupported,
      directoryCreate: () => completed,
      directoryCreateUnique: () => ({
        _tag: 'Failure',
        reason: 'PermissionDenied',
        nativeCode: 13,
      }),
      fileRemove: () => completed,
      directoryRemove: () => completed,
      handleClose: () => completed,
    }
    const evaluated = Analysis.evaluate(snapshot, { osFileSystem: provider })
    assert.strictEqual(evaluated._tag, 'Completed')
    // 100 + PermissionDenied: the provider's reason reaches the caller rather than a generic one.
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 102n)
  }),
)

/**
 * OS builtins target the evaluator and LLVM only, so the third engine's statement is a rejection
 * rather than an agreement — the same shape `StandardInput` and `ChildProcess` are held to.
 */
it.effect('rejects the unique-directory primitive for direct Wasm', () =>
  Effect.gen(function* () {
    const wasm = yield* Analysis.ofSourceRealized(
      'temporary-directory/wasm',
      ascii(evaluatorSource),
      'wasm32-unknown-unknown',
    )
    const availability = IntrinsicAvailability.select(wasm.instances.intrinsics, 'Wasm')
    assert.strictEqual(availability._tag, 'Unavailable')
    if (availability._tag === 'Unavailable')
      assert.include(
        availability.diagnostics.map((diagnostic) => diagnostic.code),
        'SEM0093',
      )
  }),
)
