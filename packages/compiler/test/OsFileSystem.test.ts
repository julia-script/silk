import { spawnSync } from 'node:child_process'
import { mkdirSync, mkdtempSync, readFileSync, rmSync, symlinkSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as IntrinsicAvailability from '../src/IntrinsicAvailability.js'
import type * as OsFileSystemHost from '../src/OsFileSystemHost.js'
import * as OsRuntime from '../src/OsRuntime.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

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

const lowLevelSource = `pub fn main() -> i32 {
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

const completed: OsFileSystemHost.CommandResult = Object.freeze({ _tag: 'Completed' })
const end: OsFileSystemHost.DirectoryNextResult = Object.freeze({ _tag: 'End' })
const inspected: OsFileSystemHost.InspectResult = Object.freeze({
  _tag: 'Inspected',
  kind: 'File',
  byteLength: 42,
})
const unsupported: OsFileSystemHost.Failure = Object.freeze({
  _tag: 'Failure',
  reason: 'Unsupported',
})
const provider: OsFileSystemHost.Provider = Object.freeze({
  fileOpen: () => unsupported,
  fileRead: () => unsupported,
  fileWrite: () => unsupported,
  directoryOpen: () => unsupported,
  directoryNext: () => end,
  pathInspect: () => inspected,
  directoryCreate: () => completed,
  fileRemove: () => completed,
  directoryRemove: () => completed,
  handleClose: () => completed,
})

it.effect('loads the ordinary canonical OS provider without compiler-known library privilege', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'os-filesystem/importer',
      ascii(`import silk.os_filesystem { OsFileSystem, make }
pub effect fn construct(root: string) -> OsFileSystem ! OutOfMemory ? &mut Allocator {
  return run make(root)
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
        ascii(`import silk.os_filesystem { OsFileSystem, make as osMake }
import silk.filesystem { DirectoryEntry, FileError, FileSystem, Path, root as pathRoot }
import silk.vector { Vector }

impl Report for OutOfMemory {}

pub effect fn main() -> () ! FileError | OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let mut fs = run osMake("/tmp") |> Effect.provideMut(&mut allocator)
  let path = run pathRoot() |> Effect.provideMut(&mut allocator)
  let entries = run Intrinsic.bindRequirement(
    Intrinsic.bindRequirement(FileSystem.listDirectory(&path), &mut fs),
    &mut allocator
  )
  drop entries
  return ()
}`),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      yield* Analysis.codegen(snapshot, { mode: 'release' })
    }),
  15_000,
)

it.effect('blocks OS evaluation without an injected adapter and uses one when supplied', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'os-filesystem/low-level',
      ascii(lowLevelSource),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const blocked = Analysis.evaluate(snapshot)
    assert.strictEqual(blocked._tag, 'Blocked')
    if (blocked._tag === 'Blocked') {
      assert.strictEqual(blocked.reason._tag, 'MissingOsFileSystemHost')
    }
    const evaluated = Analysis.evaluate(snapshot, { osFileSystem: provider })
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42)
  }),
)

it.effect(
  'runs partial I/O and preserves primary failures through the injected evaluator host',
  () =>
    Effect.gen(function* () {
      const source = `import silk.os_filesystem { make as osMake }
import silk.bytes { asSlice as bytesSlice }
import silk.filesystem { FileError, FileSystem, make as pathMake }
import silk.result { Failure, Result, Success }

impl Report for OutOfMemory {}

effect fn program() -> i32 ! FileError | OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let mut fs = run osMake("/root") |> Effect.provideMut(&mut allocator)
  let path = run pathMake("/message") |> Effect.provideMut(&mut allocator)
  let input = [u8.toU8(104), u8.toU8(101), u8.toU8(108), u8.toU8(108), u8.toU8(111)]
  let written = run Intrinsic.bindRequirement(FileSystem.writeFile(&path, &input), &mut fs)
  let owned = run Intrinsic.bindRequirement(
    Intrinsic.bindRequirement(FileSystem.readFile(&path), &mut fs),
    &mut allocator
  )
  if bytesSlice(&owned).length == usize.add(0, 5) { return 42 }
  return 1
}

pub fn main() -> i32 {
  let attempted = run Intrinsic.effectResult(program())
  return match move attempted {
    Result<i32, FileError | OutOfMemory> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<FileError | OutOfMemory> { error } => match move error {
        FileError failure => failure.reason.code
        OutOfMemory exhausted => 99
      }
    }
  }
}`
      const snapshot = yield* Analysis.ofSourceRealized(
        'os-filesystem/evaluator-provider',
        ascii(source),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const makeProvider = (
        reads: Array<ReadonlyArray<number> | OsFileSystemHost.Failure>,
        failReadClose: boolean,
        writes: Array<ReadonlyArray<number>>,
      ): OsFileSystemHost.Provider => {
        let nextHandle = 0
        let closeCount = 0
        return {
          fileOpen: () => ({
            _tag: 'Opened',
            handle: { identity: ++nextHandle, kind: 'File' },
          }),
          fileRead: () => {
            const selected = reads.shift() ?? []
            return 'reason' in selected ? selected : { _tag: 'Read', bytes: selected }
          },
          fileWrite: (_handle, input) => {
            writes.push(Object.freeze([...input]))
            return { _tag: 'Transferred', count: Math.min(1, input.length) }
          },
          directoryOpen: () => unsupported,
          directoryNext: () => end,
          pathInspect: () => inspected,
          directoryCreate: () => completed,
          fileRemove: () => completed,
          directoryRemove: () => completed,
          handleClose: () => {
            closeCount += 1
            return failReadClose && closeCount === 2
              ? { _tag: 'Failure', reason: 'PermissionDenied', nativeCode: 13 }
              : completed
          },
        }
      }

      const writes: Array<ReadonlyArray<number>> = []
      const successful = Analysis.evaluate(snapshot, {
        osFileSystem: makeProvider([[104, 101], [108, 108, 111], []], false, writes),
      })
      assert.strictEqual(successful._tag, 'Completed')
      if (successful._tag === 'Completed') assert.strictEqual(successful.result.value, 42)
      assert.deepEqual(writes, [
        [104, 101, 108, 108, 111],
        [101, 108, 108, 111],
        [108, 108, 111],
        [108, 111],
        [111],
      ])

      const readFailure: OsFileSystemHost.Failure = {
        _tag: 'Failure',
        reason: 'NotFound',
        nativeCode: 2,
      }
      const primary = Analysis.evaluate(snapshot, {
        osFileSystem: makeProvider([readFailure], true, []),
      })
      assert.strictEqual(primary._tag, 'Completed')
      if (primary._tag === 'Completed') assert.strictEqual(primary.result.value, 0)

      const closeFailure = Analysis.evaluate(snapshot, {
        osFileSystem: makeProvider([[]], true, []),
      })
      assert.strictEqual(closeFailure._tag, 'Completed')
      if (closeFailure._tag === 'Completed') assert.strictEqual(closeFailure.result.value, 2)
    }),
)

it.effect('retries oversized directory entries without advancing and sorts complete paths', () =>
  Effect.gen(function* () {
    const source = `import silk.os_filesystem { make as osMake }
import silk.filesystem { DirectoryEntry, FileError, FileSystem, root as pathRoot, view as pathView }
import silk.result { Failure, Result, Success }
import silk.vector { asSlice as vectorSlice }

impl Report for OutOfMemory {}

fn pathMatches(entries: &[DirectoryEntry], index: usize, expected: string) -> bool {
  return match &entries[index] {
    DirectoryEntry { path, kind } => pathView(&path) == expected
  }
}

effect fn program() -> i32 ! FileError | OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let mut fs = run osMake("/root") |> Effect.provideMut(&mut allocator)
  let root = run pathRoot() |> Effect.provideMut(&mut allocator)
  let entries = run Intrinsic.bindRequirement(
    Intrinsic.bindRequirement(FileSystem.listDirectory(&root), &mut fs),
    &mut allocator
  )
  let listed = vectorSlice<DirectoryEntry>(&entries)
  if listed.length != usize.add(0, 3) { return 1 }
  if pathMatches(listed, usize.add(0, 0), "/a") == false { return 2 }
  if pathMatches(listed, usize.add(0, 2), "/z") == false { return 3 }
  return 42
}

pub fn main() -> i32 {
  let attempted = run Intrinsic.effectResult(program())
  return match move attempted {
    Result<i32, FileError | OutOfMemory> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<FileError | OutOfMemory> { error } => 10
    }
  }
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'os-filesystem/evaluator-directory',
      ascii(source),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const oversized = Object.freeze(Array.from({ length: 80 }, () => 120))
    const pending: Array<ReadonlyArray<number>> = [[122], oversized, [97]]
    let retries = 0
    const directoryProvider: OsFileSystemHost.Provider = {
      ...provider,
      directoryOpen: () => ({
        _tag: 'Opened',
        handle: { identity: 1, kind: 'Directory' },
      }),
      directoryNext: (_handle, capacity) => {
        const name = pending.at(0)
        if (name === undefined) return end
        if (capacity < name.length) {
          retries += 1
          return { _tag: 'BufferTooSmall', requiredCapacity: name.length }
        }
        pending.shift()
        return { _tag: 'Entry', name, kind: 'File' }
      },
    }
    const evaluated = Analysis.evaluate(snapshot, { osFileSystem: directoryProvider })
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42)
    assert.strictEqual(retries, 1)

    let invalidPending = true
    const invalidDirectoryProvider: OsFileSystemHost.Provider = {
      ...provider,
      directoryOpen: () => ({
        _tag: 'Opened',
        handle: { identity: 2, kind: 'Directory' },
      }),
      directoryNext: () => {
        if (invalidPending === false) return end
        invalidPending = false
        return { _tag: 'Entry', name: [255], kind: 'File' }
      },
    }
    const invalid = Analysis.evaluate(snapshot, { osFileSystem: invalidDirectoryProvider })
    assert.strictEqual(invalid._tag, 'Completed')
    if (invalid._tag === 'Completed') assert.strictEqual(invalid.result.value, 10)
  }),
)

it.effect(
  'emits reachable native runtime declarations and rejects the same call for direct Wasm',
  () =>
    Effect.gen(function* () {
      const native = yield* Analysis.ofSourceRealized('os-filesystem/native', ascii(lowLevelSource))
      const artifact = yield* Analysis.codegen(native, { mode: 'release' })
      assert.include(artifact.ir, 'silk_os_path_inspect_v1')

      const wasm = yield* Analysis.ofSourceRealized(
        'os-filesystem/wasm',
        ascii(lowLevelSource),
        'wasm32-unknown-unknown',
      )
      const availability = IntrinsicAvailability.select(wasm.instances.intrinsics, 'Wasm')
      assert.strictEqual(availability._tag, 'Unavailable')
      if (availability._tag === 'Unavailable') {
        assert.deepEqual(
          availability.diagnostics.map((diagnostic) => diagnostic.code),
          ['SEM0093'],
        )
      }
    }),
)

it.effect('keeps OsHandle opaque, affine, consuming, and unsafe-only', () =>
  Effect.gen(function* () {
    const construct = yield* Analysis.ofSourceRealized(
      'os-filesystem/construct-handle',
      ascii(`pub fn main() -> i32 { let handle = OsHandle {}; return 0 }`),
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
      ascii(`pub effect fn twice(handle: OsHandle) -> bool {
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
    const source = `import silk.os_filesystem { OsFileSystem, make as osMake }
pub effect fn construct(root: string) -> OsFileSystem ! OutOfMemory ? &mut Allocator {
  return run osMake(root)
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
      source.indexOf('osMake(root)'),
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

it.effect('keeps the evaluator boundary browser-safe and native runtime pay-for-use', () =>
  Effect.gen(function* () {
    for (const source of [
      '../src/Analysis.ts',
      '../src/BootstrapEvaluation.ts',
      '../src/OsFileSystemHost.ts',
      '../src/OsRuntime.ts',
    ]) {
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
      { input: selected, encoding: 'utf8' },
    )
    assert.strictEqual(checked.status, 0, checked.stderr)

    const securitySource = `${selected}
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

int main(void) {
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
      const source = `import silk.os_filesystem { make as osMake }
import silk.bytes { asSlice as bytesSlice }
import silk.filesystem { FileError, FileSystem, make as pathMake }
import silk.result { Failure, Result, Success }

impl Report for OutOfMemory {}

effect fn program() -> i32 ! FileError | OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let mut fs = run osMake("${nativeRoot}") |> Effect.provideMut(&mut allocator)
  let path = run pathMake("/hello.txt") |> Effect.provideMut(&mut allocator)
  let input = [u8.toU8(104), u8.toU8(101), u8.toU8(108), u8.toU8(108), u8.toU8(111)]
  let written = run Intrinsic.bindRequirement(FileSystem.writeFile(&path, &input), &mut fs)
  let owned = run Intrinsic.bindRequirement(
    Intrinsic.bindRequirement(FileSystem.readFile(&path), &mut fs),
    &mut allocator
  )
  let bytes = bytesSlice(&owned)
  if bytes.length != usize.add(0, 5) { return 1 }
  if bytes[usize.add(0, 0)] != u8.toU8(104) { return 2 }
  let removed = run Intrinsic.bindRequirement(FileSystem.removeFile(&path), &mut fs)
  let empty = run pathMake("/empty") |> Effect.provideMut(&mut allocator)
  let created = run Intrinsic.bindRequirement(FileSystem.createDirectory(&empty), &mut fs)
  let removedEmpty = run Intrinsic.bindRequirement(FileSystem.removeDirectory(&empty), &mut fs)
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
  let completed = run Intrinsic.effectResult(program())
  return match move completed {
    Result<i32, FileError | OutOfMemory> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<FileError | OutOfMemory> { error } => 10
    }
  }
}`
      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('os-filesystem/native-provider', ascii(source)),
        },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        profile: 'release',
        destination: join(destinationRoot, 'native-provider'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(
        compiled._tag,
        'Compiled',
        JSON.stringify(compiled._tag === 'BackendFailed' ? compiled.error : compiled),
      )
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(
        run.status,
        42,
        JSON.stringify({ signal: run.signal, stderr: run.stderr, stdout: run.stdout }),
      )
      assert.strictEqual(readFileSync(outsideMarker, 'utf8'), 'untouched')
    }),
  60_000,
)
