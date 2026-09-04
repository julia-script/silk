import { spawnSync } from 'node:child_process'
import { mkdirSync, mkdtempSync, readFileSync, rmSync, symlinkSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'
import * as OsRuntime from '../src/OsRuntime.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Termination from '../src/Termination.js'
import * as ToolchainPlan from '../src/ToolchainPlan.js'
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

const lowLevelSource = `import silk.u32 as u32
import silk.usize as usize
pub fn main() -> i32 {
  let mut kind = 0
  let mut length = usize.add(0, 0)
  let mut reason = 0
  let mut nativeCode = u32.toU32(0)
  let mut inspected = false
  unsafe {
    inspected = run Intrinsic.osPathInspect(Intrinsic.stringUtf8Bytes("/tmp"), Intrinsic.stringUtf8Bytes("/file"), &mut kind, &mut length, &mut reason, &mut nativeCode)
  }
  if inspected { return kind + usize.toI32(length) }
  return reason
}`
const returnedStatusTermination: Termination.Contract = Object.freeze({
  _tag: 'EntryTermination',
  success: 'ReturnedStatus',
  failures: Object.freeze([]),
  logicalFrames: Object.freeze([]),
  report: Termination.emptyReport,
})

it.effect('loads the ordinary canonical OS provider without compiler-known library privilege', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'os-filesystem/importer',
      ascii(`import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.os_filesystem { OsFileSystem }
pub effect fn construct(root: string) -> OsFileSystem ! OutOfMemoryError ? &mut Allocator {
  return run OsFileSystem.make(root)
}`),
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
  let mut fs = run OsFileSystem.make("/tmp") |> Effect.provideMut(&mut allocator)
  let path = run Path.root() |> Effect.provideMut(&mut allocator)
  let entries = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(FileSystem.listDirectory(&path), &mut fs),
    &mut allocator
  )
  drop entries
  return ()
}`),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      yield* Analysis.codegen(snapshot, { mode: 'release' })
    }),
  60_000,
)

it.effect('keeps OsHandle opaque, affine, consuming, and unsafe-only', () =>
  Effect.gen(function* () {
    const construct = yield* Analysis.ofSourceRealized(
      'os-filesystem/construct-handle',
      ascii(`pub fn main() -> i32 {
  let handle = OsHandle {}
  return 0
}`),
    )
    assert.include(
      Analysis.diagnostics(construct).map((diagnostic) => diagnostic.code),
      'SEM0021',
    )

    const copied = yield* Analysis.ofSourceRealized(
      'os-filesystem/copy-handle',
      ascii(`pub fn copy(handle: OsHandle) -> () {
  let copied = handle
  drop copied
  return ()
}
pub fn main() -> i32 { return 0 }`),
    )
    assert.include(
      Analysis.diagnostics(copied).map((diagnostic) => diagnostic.code),
      'OWN0003',
    )

    const inspectedHandle = yield* Analysis.ofSourceRealized(
      'os-filesystem/inspect-handle',
      ascii(`pub fn inspect(handle: &OsHandle) -> i32 { return handle.identity }
pub fn main() -> i32 { return 0 }`),
    )
    assert.isAbove(Analysis.diagnostics(inspectedHandle).length, 0)

    const unacknowledged = yield* Analysis.ofSourceRealized(
      'os-filesystem/safe-call',
      ascii(
        lowLevelSource
          .replace('  unsafe {\n    inspected = ', '  inspected = ')
          .replace('\n  }\n  if inspected', '\n  if inspected'),
      ),
    )
    assert.include(
      Analysis.diagnostics(unacknowledged).map((diagnostic) => diagnostic.code),
      'SEM0082',
    )

    const reused = yield* Analysis.ofSourceRealized(
      'os-filesystem/reused-handle',
      ascii(`import silk.u32 as u32
pub effect fn twice(handle: OsHandle) -> bool {
  let mut reason = 0
  let mut code = u32.toU32(0)
  unsafe {
    let first = run Intrinsic.osHandleClose(move handle, &mut reason, &mut code)
    return run Intrinsic.osHandleClose(move handle, &mut reason, &mut code)
  }
  return false
}
pub fn main() -> i32 { return 0 }`),
    )
    assert.include(
      Analysis.diagnostics(reused).map((diagnostic) => diagnostic.code),
      'OWN0001',
    )
  }),
)

it.effect('navigates provider policy to Silk source and low-level calls to Intrinsic', () =>
  Effect.gen(function* () {
    const source = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.u32 as u32
import silk.usize as usize
import silk.os_filesystem { OsFileSystem }
pub effect fn construct(root: string) -> OsFileSystem ! OutOfMemoryError ? &mut Allocator {
  return run OsFileSystem.make(root)
}
pub fn main() -> i32 {
  let mut kind = 0
  let mut length = usize.add(0, 0)
  let mut reason = 0
  let mut code = u32.toU32(0)
  unsafe {
    let inspected = run Intrinsic.osPathInspect(Intrinsic.stringUtf8Bytes("/root"), Intrinsic.stringUtf8Bytes("/path"), &mut kind, &mut length, &mut reason, &mut code)
  }
  return 42
}`
    const snapshot = yield* Analysis.ofSourceRealized('os-filesystem/navigation', ascii(source))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const providerOccurrence = Analysis.semanticOccurrenceAt(
      snapshot,
      'os-filesystem/navigation',
      source.indexOf('make(root)'),
    )
    assert.strictEqual(providerOccurrence?.declaration?.module, 'silk/os_filesystem')
    const intrinsicOccurrence = Analysis.semanticOccurrenceAt(
      snapshot,
      'os-filesystem/navigation',
      source.indexOf('osPathInspect'),
    )
    assert.strictEqual(intrinsicOccurrence?.declaration, undefined)
    assert.strictEqual(intrinsicOccurrence?.resolution._tag, 'Available')
    if (intrinsicOccurrence?.resolution._tag === 'Available') {
      assert.strictEqual(intrinsicOccurrence.resolution.identity._tag, 'IntrinsicOperationIdentity')
    }
  }),
)

it.effect('keeps analysis browser-safe and native runtime pay-for-use', () =>
  Effect.gen(function* () {
    for (const source of ['../src/Analysis.ts', '../src/OsRuntime.ts']) {
      assert.notInclude(readFileSync(new URL(source, import.meta.url), 'utf8'), "from 'node:")
    }

    const pure = yield* Analysis.ofSourceRealized(
      'os-filesystem/pure',
      ascii('pub fn main() -> i32 { return 42 }'),
    )
    const artifact = yield* Analysis.codegen(pure, { mode: 'release' })
    assert.deepEqual(artifact.nativeRuntimeSymbols, [])
    for (const symbol of OsRuntime.symbols) assert.notInclude(artifact.ir, symbol)

    const selected = OsRuntime.source(['silk_os_path_inspect_v1'])
    assert.include(selected, 'silk_os_path_inspect_v1')
    assert.notInclude(selected, 'silk_os_file_open_v1(')
    const checked = spawnSync(
      '/usr/bin/clang',
      [
        '-x',
        'c',
        '-std=c11',
        '-Wall',
        '-Wextra',
        '-Werror',
        '-Wno-unused-function',
        '-fsyntax-only',
        '-',
      ],
      {
        input: ToolchainPlan.executableSource(returnedStatusTermination, [
          'silk_os_path_inspect_v1',
        ]),
        encoding: 'utf8',
      },
    )
    assert.strictEqual(checked.status, 0, checked.stderr)

    const securitySource = `${ToolchainPlan.executableSource(returnedStatusTermination, [
      'silk_os_path_inspect_v1',
    ])}
static int rejected(const unsigned char *root, size_t root_length,
                    const unsigned char *path, size_t path_length) {
  int kind = 0;
  size_t length = 0;
  int reason = 0;
  uint32_t code = 0;
  int inspected = silk_os_path_inspect_v1(
    root, root_length, path, path_length, &kind, &length, &reason, &code
  );
  return inspected == 0;
}

int silk_main(void) {
  static const unsigned char root[] = "${nativeRoot}";
  static const unsigned char dot[] = "/./outside";
  static const unsigned char dotdot[] = "/nested/../../outside";
  static const unsigned char doubled[] = "//outside";
  static const unsigned char relative[] = "relative";
  static const unsigned char symlinked[] = "/escape/marker";
  static const unsigned char nul_path[] = { '/', 0, 'x' };
  static const unsigned char invalid_utf8[] = { '/', 255 };
  size_t root_length = sizeof(root) - 1;
  if (!rejected(root, root_length, dot, sizeof(dot) - 1)) return 1;
  if (!rejected(root, root_length, dotdot, sizeof(dotdot) - 1)) return 2;
  if (!rejected(root, root_length, doubled, sizeof(doubled) - 1)) return 3;
  if (!rejected(root, root_length, relative, sizeof(relative) - 1)) return 4;
  if (!rejected(root, root_length, nul_path, sizeof(nul_path))) return 5;
  if (!rejected(root, root_length, invalid_utf8, sizeof(invalid_utf8))) return 6;
  if (!rejected(root, root_length, symlinked, sizeof(symlinked) - 1)) return 7;
  return 42;
}
`
    const securitySourcePath = join(destinationRoot, 'runtime-security.c')
    const securityExecutable = join(destinationRoot, 'runtime-security')
    writeFileSync(securitySourcePath, securitySource)
    const built = spawnSync(
      '/usr/bin/clang',
      ['-std=c11', '-O2', securitySourcePath, '-o', securityExecutable],
      { encoding: 'utf8' },
    )
    assert.strictEqual(built.status, 0, built.stderr)
    const secured = spawnSync(securityExecutable, [], { encoding: 'utf8' })
    assert.strictEqual(secured.status, 42, secured.stderr)
    assert.strictEqual(readFileSync(outsideMarker, 'utf8'), 'untouched')
  }),
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
import silk.bytes { Bytes }
import silk.filesystem { FileError, FileSystem, Path }
import silk.result { Result }

effect fn program() -> i32 ! FileError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut fs = run OsFileSystem.make("${nativeRoot}") |> Effect.provideMut(&mut allocator)
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
  let mut kind = 0
  let mut length = usize.add(0, 0)
  let mut reason = 0
  let mut nativeCode = u32.toU32(0)
  let mut escaped = false
  unsafe {
    escaped = run Intrinsic.osPathInspect(Intrinsic.stringUtf8Bytes("${nativeRoot}"), Intrinsic.stringUtf8Bytes("/../outside"), &mut kind, &mut length, &mut reason, &mut nativeCode)
  }
  if escaped { return 3 }
  if reason != 3 { return 4 }
  reason = 0
  unsafe {
    escaped = run Intrinsic.osPathInspect(Intrinsic.stringUtf8Bytes("${nativeRoot}"), Intrinsic.stringUtf8Bytes("/escape/marker"), &mut kind, &mut length, &mut reason, &mut nativeCode)
  }
  if escaped { return 5 }
  if reason == 0 { return 6 }
  return 42
}

pub fn main() -> i32 {
  let completed = run Effect.result(program())
  return match move completed {
      Result<i32, FileError | OutOfMemoryError>.Success { value } => value
      Result<i32, FileError | OutOfMemoryError>.Failure { error } => 10
  }
}`
      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('os-filesystem/native-provider', ascii(source)),
        },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang', llvmAr: 'llvm-ar' }),
        profile: 'release',
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
