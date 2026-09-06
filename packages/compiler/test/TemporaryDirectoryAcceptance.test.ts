import { spawnSync } from 'node:child_process'
import { existsSync, mkdirSync, mkdtempSync, readdirSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'
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
const prelude = `import silk.os_filesystem { OsFileSystem }
import silk.effect { Effect }
import silk.filesystem { FileError, FileSystem, Path, TemporaryDirectory }
import silk.result { Result }

struct Sentinel { code: i32 }

/// Work that fails, so the finalizer's run is observable on the failure path rather than assumed.
effect fn failingWork() -> i32 ! Sentinel { fail Sentinel { code: 7 } }

/// Reads the failure's own payload back, so what is asserted is *which* failure survived.
effect fn recoverSentinel(error: Sentinel) -> i32 { return error.code }`

/**
 * The native programs read their confined root from SILK_TEST_ROOT at runtime instead of baking
 * the test run's mkdtemp path into the source text. A per-run path in the source made every
 * compilation byte-unique, so the content-addressed emission and executable caches could never
 * hit; with a stable source they hit on every warm run.
 */
const nativeRootResolution = `import silk.bytes { Bytes }
import silk.host_input { HostInputError, HostInput }
import silk.option { Option }
import silk.os_host_input { OsHostInput }
import silk.string { InvalidUtf8, String }

effect fn missingRoot() -> Bytes ! HostInputError {
  fail HostInput.inputFailure()
}

effect fn requiredRoot(found: Option<Bytes>) -> Bytes ! HostInputError {
  return match move found {
    Option<Bytes>.Some { value: bytes } => move bytes
    Option<Bytes>.None => run missingRoot()
  }
}

/// A root that is not valid UTF-8 is a harness defect, not a program outcome; trap like OsFileSystem.make
/// does for its own malformed-root preconditions.
effect fn invalidRoot() -> String ! OutOfMemoryError ? &mut Allocator {
  let invalid = 1 / 0
  return run String.copy("/")
}

effect fn confinedRootString() -> String ! HostInputError | OutOfMemoryError ? &mut Allocator {
  let mut hostInput = OsHostInput.make()
  let found = run Effect.provideMut(HostInput.variableNamed("SILK_TEST_ROOT"), &mut hostInput)
  let rootBytes = run requiredRoot(move found)
  let copied = run String.copyUtf8(Bytes.asSlice(&rootBytes))
  return match move copied {
    Result<String, InvalidUtf8>.Success { value } => move value
    Result<String, InvalidUtf8>.Failure { error } => run invalidRoot()
  }
}`

const nativeEpilogue = `import silk.allocator { OutOfMemoryError }
import silk.filesystem { FileError }
import silk.host_input { HostInputError }
import silk.result { Result }
pub fn main() -> i32 {
  let completed = run Effect.result(program())
  return match move completed {
      Result<i32, FileError | OutOfMemoryError | HostInputError>.Success { value } => value
      Result<i32, FileError | OutOfMemoryError | HostInputError>.Failure { error } => match move error {
        FileError failure => 100 + failure.reason.code
        OutOfMemoryError exhausted => 99
        HostInputError missing => 98
      }
  }
}`

/**
 * The whole lifecycle against a real confined root. Each numbered return is one acceptance
 * criterion, so a native exit status names which one failed rather than merely that one did.
 */
const nativeSource = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.filesystem { FileError }
import silk.filesystem { FileSystem }
import silk.u8 as u8
${prelude}
${nativeRootResolution}

effect fn program() -> i32 ! FileError | OutOfMemoryError | HostInputError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let root = run confinedRootString() |> Effect.provideMut(&mut allocator)
  let mut fs = run OsFileSystem.make(String.view(&root)) |> Effect.provideMut(&mut allocator)
  let parent = run Path.make("/scopes") |> Effect.provideMut(&mut allocator)
  let prepared = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(FileSystem.createDirectoriesRecursively(&parent), &mut fs),
    &mut allocator
  )
  let first = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(FileSystem.temporaryDirectory(&parent, "silk-build-"), &mut fs),
    &mut allocator
  )
  let second = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(FileSystem.temporaryDirectory(&parent, "silk-build-"), &mut fs),
    &mut allocator
  )

  // Two makes, two paths.
  if Path.view(&first.path) == Path.view(&second.path) { return 1 }

  // The made directory is there.
  if run Intrinsic.bindRequirementMut(FileSystem.exists(&first.path), &mut fs) {} else { return 2 }

  // The artifact a caller keeps: written to a durable path outside the scope before it releases.
  let payload = [u8.toU8(1), u8.toU8(2), u8.toU8(3)]
  let durable = run Path.join(&parent, "promoted.bin") |> Effect.provideMut(&mut allocator)
  let promoted = run Intrinsic.bindRequirementMut(FileSystem.writeFile(&durable, &payload), &mut fs)

  let released = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(FileSystem.release(move first), &mut fs),
    &mut allocator
  )
  let releasedSecond = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(FileSystem.release(move second), &mut fs),
    &mut allocator
  )
  return 42
}

${nativeEpilogue}`

/**
 * Recursive removal against a real populated tree. `release` removes contents, not just an empty
 * directory, and the primitive underneath removes exactly one *empty* directory — so the two-pass
 * walk is the part that has to be right, and this is it running on a real filesystem.
 */
const nativeTreeSource = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.filesystem { FileError }
${prelude}
${nativeRootResolution}

effect fn program() -> i32 ! FileError | OutOfMemoryError | HostInputError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let root = run confinedRootString() |> Effect.provideMut(&mut allocator)
  let mut fs = run OsFileSystem.make(String.view(&root)) |> Effect.provideMut(&mut allocator)
  let target = run Path.make("/tree") |> Effect.provideMut(&mut allocator)
  let removed = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(FileSystem.removeDirectoryRecursively(&target), &mut fs),
    &mut allocator
  )
  return 42
}

${nativeEpilogue}`

/**
 * Several populated scopes, each released while the others are still owned. This is the shape that
 * trapped at `-O0` while #130's crash was being narrowed: one owner's cleanup is a conditional arm,
 * and the values the arm reloads are read again at the arm's join. Both a populated tree and a live
 * neighbour are needed — two bare scopes released in sequence do not reach it.
 */
const nativeManySource = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.filesystem { FileError }
import silk.filesystem { FileSystem }
import silk.u8 as u8
${prelude}
${nativeRootResolution}

effect fn program() -> i32 ! FileError | OutOfMemoryError | HostInputError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let root = run confinedRootString() |> Effect.provideMut(&mut allocator)
  let mut fs = run OsFileSystem.make(String.view(&root)) |> Effect.provideMut(&mut allocator)
  let parent = run Path.make("/many") |> Effect.provideMut(&mut allocator)
  let prepared = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(FileSystem.createDirectoriesRecursively(&parent), &mut fs),
    &mut allocator
  )
  let payload = [u8.toU8(1), u8.toU8(2), u8.toU8(3)]
${[0, 1, 2]
  .map(
    (index) => `  let scope${index} = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(FileSystem.temporaryDirectory(&parent, "silk-many${index}-"), &mut fs),
    &mut allocator
  )
  let nested${index} = run Path.join(&scope${index}.path, "nested") |> Effect.provideMut(&mut allocator)
  let madeNested${index} = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(FileSystem.createDirectoriesRecursively(&nested${index}), &mut fs),
    &mut allocator
  )
  let file${index} = run Path.join(&nested${index}, "payload.bin") |> Effect.provideMut(&mut allocator)
  let wrote${index} = run Intrinsic.bindRequirementMut(FileSystem.writeFile(&file${index}, &payload), &mut fs)
  if run Intrinsic.bindRequirementMut(FileSystem.exists(&scope${index}.path), &mut fs) {} else { return ${index + 1} }`,
  )
  .join('\n')}
${[0, 1, 2]
  .map(
    (index) => `  let released${index} = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(FileSystem.release(move scope${index}), &mut fs),
    &mut allocator
  )`,
  )
  .join('\n')}
  return 42
}

${nativeEpilogue}`

it.effect(
  'creates a unique directory, removes it with its contents, and keeps a promoted artifact',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'temporary-directory/native',
        ascii(nativeSource),
        'aarch64-apple-darwin',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('temporary-directory/native', ascii(nativeSource)),
        },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang', llvmAr: 'llvm-ar' }),
        // Release, so this also stands as the regression test for #130: the backend used to let
        // a cleanup arm's reloaded lanes escape into the arm's join block, which is invalid SSA,
        // and Clang crashed on it at -O2 instead of diagnosing it.
        optimization: 'release',
        artifactKind: 'NativeExecutable',
        destination: join(destinationRoot, 'native'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled', Json.stringify(compiled).slice(0, 2500))
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], {
        encoding: 'utf8',
        env: { ...process.env, SILK_TEST_ROOT: nativeRoot },
      })
      assert.strictEqual(
        run.status,
        42,
        Json.stringify({ signal: run.signal, stderr: run.stderr, stdout: run.stdout }),
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
        'aarch64-apple-darwin',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('temporary-directory/native-tree', ascii(nativeTreeSource)),
        },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang', llvmAr: 'llvm-ar' }),
        // Release for the same reason as above — this is the walk #130 crashed on.
        optimization: 'release',
        artifactKind: 'NativeExecutable',
        destination: join(destinationRoot, 'native-tree'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled', Json.stringify(compiled).slice(0, 2500))
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], {
        encoding: 'utf8',
        env: { ...process.env, SILK_TEST_ROOT: nativeRoot },
      })
      assert.strictEqual(run.status, 42, Json.stringify({ signal: run.signal, stderr: run.stderr }))
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
        'aarch64-apple-darwin',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('temporary-directory/native-many', ascii(nativeManySource)),
        },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang', llvmAr: 'llvm-ar' }),
        optimization: 'release',
        artifactKind: 'NativeExecutable',
        destination: join(destinationRoot, 'native-many'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled', Json.stringify(compiled).slice(0, 2500))
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], {
        encoding: 'utf8',
        env: { ...process.env, SILK_TEST_ROOT: nativeRoot },
      })
      // Before #130 was fixed this trapped (SIGILL) even at -O0: a cleanup arm's reloaded lanes
      // escaped into the arm's join block, so the join read an undefined union tag and fell
      // through to the invalid-tag trap. The status, not just the absence of a crash, is what
      // says the releases actually ran.
      assert.strictEqual(
        run.status,
        42,
        Json.stringify({ signal: run.signal, stderr: run.stderr, stdout: run.stdout }),
      )
      assert.deepEqual(readdirSync(join(nativeRoot, 'many')), [])
    }),
  360_000,
)
