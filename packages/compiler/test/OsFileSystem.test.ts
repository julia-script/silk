import { spawnSync } from 'node:child_process'
import { mkdirSync, mkdtempSync, readFileSync, rmSync, symlinkSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Target from '../src/Target.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Driver from './support/TestDriver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const nativeRoot = mkdtempSync(join(tmpdir(), 'silk-os-filesystem-'))
const outsideRoot = mkdtempSync(join(tmpdir(), 'silk-os-filesystem-outside-'))
const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-os-filesystem-artifacts-'))
const outsideMarker = join(outsideRoot, 'marker')
writeFileSync(outsideMarker, 'untouched')
mkdirSync(join(nativeRoot, 'nested'))
symlinkSync(outsideRoot, join(nativeRoot, 'escape'))
afterAll(() => {
  rmSync(nativeRoot, { recursive: true, force: true })
  rmSync(outsideRoot, { recursive: true, force: true })
  rmSync(destinationRoot, { recursive: true, force: true })
})

it.effect('loads the ordinary canonical OS provider without compiler-known library privilege', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'os-filesystem/importer',
      ascii(`import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.os_filesystem { OsFileSystem }
pub effect fn construct(root: string) -> OsFileSystem ! OutOfMemoryError ? &mut Allocator {
  return run OsFileSystem.make(Intrinsic.stringUtf8Bytes(root))
}`),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect(
  'lowers the OS directory-list provider runner',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'os-filesystem/list-runner',
        ascii(`import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.os_filesystem { OsFileSystem }
import silk.filesystem { DirectoryEntry, FileError, FileSystem, Path }
import silk.vector { Vector }

pub effect fn main() -> () ! FileError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut fs = run OsFileSystem.make(b"/tmp") |> Effect.provideMut(&mut allocator)
  let path = run Path.root() |> Effect.provideMut(&mut allocator)
  let entries = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(FileSystem.listDirectory(&path), &mut fs),
    &mut allocator
  )
  drop entries
  return ()
}`),
        'aarch64-apple-darwin',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
      yield* Analysis.codegen(snapshot, { mode: 'release' })
    }),
  60_000,
)

it.effect(
  'runs the ordinary OS provider against a confined native root',
  () =>
    Effect.gen(function* () {
      const source = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.u32 as u32
import silk.u8 as u8
import silk.usize as usize
import silk.os_filesystem { OsFileSystem }
import silk.native_filesystem { NativeFileSystem }
import silk.bytes { Bytes }
import silk.filesystem { FileError, FileSystem, Path }
import silk.result { Result }
import silk.vector { Vector }
effect fn program() -> i32 ! FileError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut fs = run OsFileSystem.make(b"${nativeRoot}") |> Effect.provideMut(&mut allocator)
  let path = run Path.make("/hello.txt") |> Effect.provideMut(&mut allocator)
  let input = [u8.toU8(104), u8.toU8(101), u8.toU8(108), u8.toU8(108), u8.toU8(111)]
  let written = run Intrinsic.bindRequirementMut(FileSystem.writeFile(&path, &input), &mut fs)
  let owned = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(FileSystem.readFile(&path), &mut fs),
    &mut allocator
  )
  let bytes = Bytes.asSlice(&owned)
  if bytes.length != usize.add(0, 5) { return 1 }
  if bytes[usize.add(0, 0)] != u8.toU8(104) { return 2 }
  let removed = run Intrinsic.bindRequirementMut(FileSystem.removeFile(&path), &mut fs)
  let empty = run Path.make("/empty") |> Effect.provideMut(&mut allocator)
  let created = run Intrinsic.bindRequirementMut(FileSystem.createDirectory(&empty), &mut fs)
  let removedEmpty = run Intrinsic.bindRequirementMut(FileSystem.removeDirectory(&empty), &mut fs)
  let parent = run Path.make("/nested") |> Effect.provideMut(&mut allocator)
  let rawName = [u8.toU8(${process.platform === 'darwin' ? 195 : 255}), u8.toU8(${process.platform === 'darwin' ? 169 : 97})]
  let rawPath = run Path.joinBytes(&parent, &rawName) |> Effect.provideMut(&mut allocator)
  run FileSystem.writeFile(&rawPath, &input) |> Effect.provideMut(&mut fs)
  let listed = run FileSystem.listDirectory(&parent) |> Effect.provideMut(&mut fs) |> Effect.provideMut(&mut allocator)
  if Vector.length(&listed) != 1 { return 5 }
  let listedEntries = Vector.asSlice(&listed)
  let listedPath = Path.rawBytes(&listedEntries[0].path)
  if listedPath.length != 10 || listedPath[8] != u8.toU8(${process.platform === 'darwin' ? 195 : 255}) { return 6 }
  run FileSystem.removeFile(&rawPath) |> Effect.provideMut(&mut fs)
  let mut kind = 0
  let mut length: usize = 0
  let escaped = run Effect.result(NativeFileSystem.inspect(b"${nativeRoot}", b"/../outside", &mut kind, &mut length, 2))
  let rejected = match move escaped {
    Result<(), FileError>.Success { value } => false
    Result<(), FileError>.Failure { error } => true
  }
  if rejected == false { return 3 }
  let followed = run Effect.result(NativeFileSystem.inspect(b"${nativeRoot}", b"/escape/marker", &mut kind, &mut length, 2))
  let blocked = match move followed {
    Result<(), FileError>.Success { value } => false
    Result<(), FileError>.Failure { error } => true
  }
  if blocked == false { return 4 }
  return 42
}

pub fn main() -> i32 {
  let completed = run Effect.result(program())
  return match move completed {
      Result<i32, FileError | OutOfMemoryError>.Success { value } => value
      Result<i32, FileError | OutOfMemoryError>.Failure { error } => match move error {
        FileError failure => 20 + failure.operation.code * 20 + failure.reason.code
        OutOfMemoryError exhausted => 250
      }
  }
}`
      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('os-filesystem/native-provider', ascii(source)),
        },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: 'clang', llvmAr: 'llvm-ar' }),
        optimization: 'release',
        artifactKind: 'NativeExecutable',
        destination: join(destinationRoot, 'native-provider'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(
        compiled._tag,
        'Compiled',
        Json.stringify(compiled._tag === 'BackendFailed' ? compiled.error : compiled),
      )
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(
        run.status,
        42,
        Json.stringify({ signal: run.signal, stderr: run.stderr, stdout: run.stdout }),
      )
      assert.strictEqual(readFileSync(outsideMarker, 'utf8'), 'untouched')
    }),
  60_000,
)

it.effect('omits native filesystem providers from Wasm and no-libc selections', () =>
  Effect.gen(function* () {
    const source = 'import silk.os_filesystem { OsFileSystem }\npub fn main() -> i32 { return 42 }'
    for (const target of Target.all) {
      const snapshot = yield* Analysis.makeRealized({
        root: SourceFile.make('filesystem/unavailable', ascii(source)),
        configuration: {
          profile: { target: target.id, artifact: 'object', libc: 'none', entry: { kind: 'none' } },
        },
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => [
          diagnostic.code,
          diagnostic.span.start,
          diagnostic.span.end,
        ]),
        [
          [
            'SEM0014',
            source.indexOf('OsFileSystem'),
            source.indexOf('OsFileSystem') + 'OsFileSystem'.length,
          ],
        ],
      )
      assert.deepEqual(snapshot.instances.foreignCalls, [])
    }
  }),
)

it.effect('consumes a native file handle exactly once in source ownership analysis', () =>
  Effect.gen(function* () {
    const source = `import silk.native_filesystem { NativeFileSystem, FileHandle }
import silk.filesystem { FileSystem, FileError }
pub effect fn closeTwice(handle: FileHandle) -> () ! FileError {
  run NativeFileSystem.closeFile(move handle, FileSystem.readFileOperation())
  run NativeFileSystem.closeFile(move handle, FileSystem.readFileOperation())
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'filesystem/consumed-handle',
      ascii(source),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => [
        diagnostic.code,
        diagnostic.span.start,
        diagnostic.span.end,
      ]),
      [
        [
          'OWN0001',
          source.lastIndexOf('move handle'),
          source.lastIndexOf('move handle') + 'move handle'.length,
        ],
      ],
    )
  }),
)
