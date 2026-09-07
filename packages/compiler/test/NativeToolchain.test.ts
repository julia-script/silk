import type * as Backend from '../src/Backend.js'
import * as ForeignContract from '../src/ForeignContract.js'
import * as Result from 'effect/Result'
import * as HelperCapability from '../src/HelperCapability.js'
import * as ObjectSymbols from '../src/internal/ObjectSymbols.js'
import * as Schema from 'effect/Schema'
import { NodeServices } from '@effect/platform-node'
import * as PlatformSupply from '../src/PlatformSupply.js'
import * as PlatformSupplyResolver from '../src/PlatformSupplyResolver.js'
import * as NativeLinkResolver from '../src/NativeLinkResolver.js'
import * as LinkerScript from '../src/internal/LinkerScript.js'
import * as NativeStub from '../src/internal/NativeStub.js'
import * as CompilationProfile from '../src/CompilationProfile.js'
import { spawnSync } from 'node:child_process'
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'
import * as Fiber from 'effect/Fiber'
import * as Analysis from '../src/Analysis.js'
import * as CoroutineRuntime from '../src/CoroutineRuntime.js'
import * as OsRuntime from '../src/OsRuntime.js'
import * as NativeLinkInput from '../src/NativeLinkInput.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as LlvmWasmRuntime from '../src/LlvmWasmRuntime.js'
import * as Target from '../src/Target.js'
import * as Termination from '../src/Termination.js'
import * as ToolchainPlan from '../src/ToolchainPlan.js'

const defaultClang = (): string => {
  if (existsSync('/opt/homebrew/opt/llvm/bin/clang')) return '/opt/homebrew/opt/llvm/bin/clang'
  if (existsSync('/usr/local/opt/llvm/bin/clang')) return '/usr/local/opt/llvm/bin/clang'
  return 'clang'
}

const testPath = Effect.runSync(Config.string('PATH').pipe(Config.withDefault('')))
const clang = Effect.runSync(
  Config.string('SILK_TEST_CLANG').pipe(Config.withDefault(defaultClang())),
)
const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang,
  llvmAr: 'llvm-ar',
})

const testRoot = mkdtempSync(join(tmpdir(), 'silk-native-boundary-test-'))
afterAll(() => {
  rmSync(testRoot, { recursive: true, force: true })
})

it('releases acquired startup pipes and preserves errors through close-on-exec failure', () => {
  const source = `#define _GNU_SOURCE 1
#define _DARWIN_C_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#define pipe silk_test_pipe
#define fcntl silk_test_fcntl
#define close silk_test_close
#define fork silk_test_fork
#define read silk_test_read
#define poll silk_test_poll
#define waitpid silk_test_waitpid
${OsRuntime.source(['silk_os_process_execute_v1'])}
#include <stdio.h>
#include <stdarg.h>

static int scenario;
static int pipe_calls, fcntl_calls, fork_calls, unexpected;
static int closed[6];

int silk_test_pipe(int descriptors[2]) {
  int ordinal = pipe_calls++;
  if (ordinal == 2 && scenario == 1) { errno = EMFILE; return -1; }
  descriptors[0] = 90 + 2 * ordinal;
  descriptors[1] = 91 + 2 * ordinal;
  return 0;
}
int silk_test_fcntl(int fd, int command, ...) {
  va_list args;
  va_start(args, command);
  int flags = va_arg(args, int);
  va_end(args);
  fcntl_calls += 1;
  if (fd != 95 || command != F_SETFD || flags != FD_CLOEXEC) unexpected += 1;
  if (scenario == 0) { errno = EACCES; return -1; }
  return 0;
}
int silk_test_close(int fd) {
  if (fd >= 90 && fd < 96) closed[fd - 90] += 1;
  else unexpected += 1;
  errno = EINTR;
  return -1;
}
pid_t silk_test_fork(void) {
  fork_calls += 1;
  return 123;
}
ssize_t silk_test_read(int fd, void *output, size_t length) {
  (void)output; (void)length;
  if (fd != 90 && fd != 92 && fd != 94) unexpected += 1;
  return 0;
}
int silk_test_poll(struct pollfd *fds, nfds_t count, int timeout) {
  (void)timeout;
  for (nfds_t index = 0; index < count; index += 1) fds[index].revents = POLLHUP;
  return (int)count;
}
pid_t silk_test_waitpid(pid_t child, int *status, int options) {
  if (child != 123 || options != 0) unexpected += 1;
  *status = 42 << 8;
  return child;
}
int main(void) {
  for (scenario = 0; scenario < 3; scenario += 1) {
    pipe_calls = 0; fcntl_calls = 0; fork_calls = 0; unexpected = 0;
    memset(closed, 0, sizeof(closed));
    int status = -1, code = -1, reason = -1;
    uint32_t native_code = 0;
    size_t output_length = 0, error_length = 0;
    const unsigned char empty[] = "";
    int result = silk_os_process_execute_v1((const unsigned char *)"fixture", 7,
      empty, 0, empty, 0, empty, 0, &status, &code, &output_length, &error_length,
      &reason, &native_code);
    uint32_t expected_error = scenario == 0 ? EACCES : scenario == 1 ? EMFILE : 0;
    printf("%d %d %d %d %d %d %d", result, reason, native_code == expected_error,
      pipe_calls, fcntl_calls, fork_calls, unexpected);
    for (int index = 0; index < 6; index += 1) printf(" %d", closed[index]);
    printf(" %d %d %d\\n", status, code, output_length == 0 && error_length == 0);
  }
  return 0;
}
`
  const sourcePath = join(testRoot, 'startup-pipe-cleanup.c')
  const executable = join(testRoot, 'startup-pipe-cleanup')
  writeFileSync(sourcePath, source)
  const built = spawnSync(
    clang,
    [
      '-std=c11',
      '-Wall',
      '-Wextra',
      '-Werror',
      '-Wno-unused-function',
      sourcePath,
      '-o',
      executable,
    ],
    { encoding: 'utf8' },
  )
  assert.strictEqual(built.status, 0, built.stderr)
  const run = spawnSync(executable, [], { encoding: 'utf8' })
  assert.strictEqual(run.status, 0, run.stderr)
  const outcomes = run.stdout
    .trim()
    .split('\n')
    .map((line) => line.split(' ').map(Number))
  // result, reason, preserved errno, pipe/fcntl/fork calls, unexpected calls, six closes,
  // child status/code, empty captures. Each row exercises the same emitted operation.
  assert.deepStrictEqual(outcomes, [
    [0, 2, 1, 3, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1],
    [0, 10, 1, 3, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1],
    [1, 0, 1, 3, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 42, 1],
  ])
})

const finalize = Effect.fnUntraced(function* (
  tools: NativeToolchain.Toolchain,
  scope: NativeToolchain.BuildScope,
  kind: ToolchainPlan.NativeArtifactKind,
  target: Target.Target,
  objects: ReadonlyArray<NativeToolchain.PathArtifact>,
  inputs: ReadonlyArray<NativeLinkInput.NativeLinkInput>,
  destination: string,
) {
  const profile = yield* profileFor(target).pipe(Effect.orDie)
  const selection = { kind: 'default' } as const
  const plan = yield* NativeToolchain.planNativeLink(
    tools,
    scope,
    kind,
    profile,
    objects,
    inputs,
    destination,
    { request: selection, composition: selection, resolved: selection },
  )
  return yield* NativeToolchain.NativeFinalizer.finalize(plan, kind, destination)
})

it('denies native final-cache admission without complete tool and implicit-input identities', () => {
  for (const kind of ['NativeExecutable', 'NativeSharedLibrary', 'NativeStaticLibrary'] as const) {
    assert.deepStrictEqual(NativeToolchain.finalArtifactCacheAdmission(kind), {
      _tag: 'Ineligible',
      reason: 'IncompleteNativeInputAccounting',
    })
  }
  assert.deepStrictEqual(NativeToolchain.finalArtifactCacheAdmission('WebAssemblyModule'), {
    _tag: 'ExistingWebAssemblyPolicy',
  })
})

const termination = (...identities: ReadonlyArray<string>): Termination.Contract =>
  Object.freeze({
    _tag: 'EntryTermination',
    success: identities.length === 0 ? 'ReturnedStatus' : 'Zero',
    failures: Object.freeze(
      identities.map((identity, ordinal) => Object.freeze({ tag: ordinal + 1, identity })),
    ),
    logicalFrames: Object.freeze([]),
    report: Termination.emptyReport,
  })

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const setUint16LittleEndian = (bytes: Uint8Array, offset: number, value: number): void => {
  bytes[offset] = value & 0xff
  bytes[offset + 1] = (value >>> 8) & 0xff
}

const setUint32LittleEndian = (bytes: Uint8Array, offset: number, value: number): void => {
  setUint16LittleEndian(bytes, offset, value & 0xffff)
  setUint16LittleEndian(bytes, offset + 2, value >>> 16)
}

const machOImage = (fileType: 1 | 2 | 6): Uint8Array => {
  const bytes = new Uint8Array(32)
  bytes.set([0xcf, 0xfa, 0xed, 0xfe])
  setUint32LittleEndian(bytes, 4, 0x0100_000c)
  setUint32LittleEndian(bytes, 12, fileType)
  return bytes
}

const elfImage = (machine: 62 | 183, fileType: 1 | 2 | 3, hasInterpreter = false): Uint8Array => {
  const bytes = new Uint8Array(fileType === 3 ? 68 : 64)
  bytes.set([0x7f, 0x45, 0x4c, 0x46, 2, 1])
  setUint16LittleEndian(bytes, 16, fileType)
  setUint16LittleEndian(bytes, 18, machine)
  if (fileType === 3) {
    setUint32LittleEndian(bytes, 32, 64)
    setUint16LittleEndian(bytes, 54, 4)
    setUint16LittleEndian(bytes, 56, 1)
    setUint32LittleEndian(bytes, 64, hasInterpreter ? 3 : 1)
  }
  return bytes
}

const nativeObjectFor = (target: Target.Target): Uint8Array => {
  switch (target.id) {
    case 'aarch64-apple-darwin':
      return machOImage(1)
    case 'aarch64-unknown-linux-gnu':
      return elfImage(183, 1)
    case 'x86_64-unknown-linux-gnu':
      return elfImage(62, 1)
    case 'wasm32-unknown-unknown':
      return assert.fail('WebAssembly has no native object container')
  }
}

const staticArchive = (...objects: ReadonlyArray<Uint8Array>): Uint8Array => {
  const memberLength = (object: Uint8Array): number => 60 + object.length + (object.length % 2)
  const bytes = new Uint8Array(
    8 + objects.reduce((length, object) => length + memberLength(object), 0),
  )
  bytes.set(ascii('!<arch>\n'))
  let offset = 8
  for (const object of objects) {
    const header = new Uint8Array(60)
    header.fill(0x20)
    header.set(ascii('member.o/       '), 0)
    header.set(ascii(String(object.length).padEnd(10, ' ')), 48)
    header.set([0x60, 0x0a], 58)
    bytes.set(header, offset)
    bytes.set(object, offset + header.length)
    offset += memberLength(object)
  }
  return bytes
}

const malformedExtendedNameArchive = (nameLength: number): Uint8Array => {
  const bytes = new Uint8Array(8 + 60 + nameLength)
  bytes.set(ascii('!<arch>\n'))
  const header = new Uint8Array(60)
  header.fill(0x20)
  header.set(ascii(`#1/${String(nameLength)}`), 0)
  header.set(ascii(String(nameLength).padEnd(10, ' ')), 48)
  header.set([0x60, 0x0a], 58)
  bytes.set(header, 8)
  return bytes
}

const profileFor = Effect.fnUntraced(function* (
  target: Target.Target,
  optimization: ToolchainPlan.OptimizationProfile = 'release',
) {
  const initial = yield* CompilationProfile.normalize({
    target: target.id,
    optimization: optimization === 'debug' ? 'none' : 'speed',
    debug: optimization !== 'release',
  })
  return yield* CompilationProfile.publish(initial, [])
})

const artifactFor = Effect.fnUntraced(function* (
  target: Target.Target,
  profile: ToolchainPlan.OptimizationProfile,
) {
  const snapshot = yield* Analysis.ofSourceRealized(
    'memory/native',
    ascii(
      'pub fn identity(value: i32) -> i32 { return value }\npub fn main() -> i32 { return identity(identity(42)) }',
    ),
    target.id,
  )
  return yield* Analysis.codegen(snapshot, { mode: ToolchainPlan.codegenModeFor(profile) })
})

it.effect('plans fixed profile arguments against the canonical target id', () =>
  Effect.gen(function* () {
    const target = Target.aarch64AppleDarwin
    const debug = ToolchainPlan.objectCommand(
      clang,
      yield* profileFor(target, 'debug'),
      'in.bc',
      'out.o',
    )
    const release = ToolchainPlan.objectCommand(
      clang,
      yield* profileFor(target, 'release'),
      'in.bc',
      'out.o',
    )
    assert.deepEqual(debug.arguments, [
      '--target=aarch64-apple-darwin',
      '-c',
      '-x',
      'ir',
      'in.bc',
      '-fPIC',
      '-O0',
      '-g',
      '-mmacosx-version-min=11.0.0',
      '-o',
      'out.o',
    ])
    assert.deepEqual(release.arguments, [
      '--target=aarch64-apple-darwin',
      '-c',
      '-x',
      'ir',
      'in.bc',
      '-fPIC',
      '-O2',
      '-mmacosx-version-min=11.0.0',
      '-o',
      'out.o',
    ])
    const runtime = ToolchainPlan.cObjectCommand(clang, target, 'silk_runtime.c', 'silk_runtime.o')
    assert.deepEqual(runtime.arguments, [
      '--target=aarch64-apple-darwin',
      '-c',
      '-x',
      'c',
      'silk_runtime.c',
      '-O2',
      '-fPIC',
      '-fvisibility=hidden',
      '-o',
      'silk_runtime.o',
    ])
    const link = ToolchainPlan.nativeCommand(
      { clang, llvmAr: 'llvm-ar' },
      'NativeExecutable',
      target,
      ['program.o', 'silk_runtime.o', 'extra.o'],
      [NativeLinkInput.library('m', 'Dynamic'), NativeLinkInput.library('c', 'Dynamic')],
      'out',
    )
    if (link._tag !== 'PlannedCommand') return assert.fail('expected executable link plan')
    assert.deepEqual(link.arguments, [
      '--target=aarch64-apple-darwin',
      'program.o',
      'silk_runtime.o',
      'extra.o',
      '-lm',
      '-lc',
      '-o',
      'out',
    ])
  }),
)

it('plans shared links and deterministic static archives from structured inputs', () => {
  const shared = ToolchainPlan.nativeCommand(
    { clang, llvmAr: 'llvm-ar' },
    'NativeSharedLibrary',
    Target.aarch64AppleDarwin,
    ['program.o', 'runtime.o'],
    [
      NativeLinkInput.searchPath('/sdk/lib'),
      NativeLinkInput.library('z', 'Dynamic'),
      NativeLinkInput.object('/objects/extra.o'),
      NativeLinkInput.staticArchive('/archives/liblocal.a'),
      NativeLinkInput.framework('CoreFoundation'),
    ],
    'libprogram.dylib',
  )
  if (shared._tag !== 'PlannedCommand') return assert.fail('expected shared link plan')
  assert.deepStrictEqual(shared.arguments, [
    '--target=aarch64-apple-darwin',
    '-dynamiclib',
    '-Wl,-install_name,@rpath/libprogram.dylib',
    'program.o',
    'runtime.o',
    '-L/sdk/lib',
    '-lz',
    '/objects/extra.o',
    '/archives/liblocal.a',
    '-framework',
    'CoreFoundation',
    '-o',
    'libprogram.dylib',
  ])

  const archive = ToolchainPlan.nativeCommand(
    { clang, llvmAr: '/tool/llvm-ar' },
    'NativeStaticLibrary',
    Target.aarch64AppleDarwin,
    ['program.o', 'runtime.o'],
    [NativeLinkInput.object('/objects/extra.o')],
    'libprogram.a',
  )
  if (archive._tag !== 'PlannedCommand') return assert.fail('expected archive plan')
  assert.strictEqual(archive.command, '/tool/llvm-ar')
  assert.deepStrictEqual(archive.arguments, [
    'rcsD',
    'libprogram.a',
    'program.o',
    'runtime.o',
    '/objects/extra.o',
  ])

  const linuxStatic = ToolchainPlan.nativeCommand(
    { clang, llvmAr: 'llvm-ar' },
    'NativeSharedLibrary',
    Target.x8664UnknownLinuxGnu,
    [],
    [NativeLinkInput.library('answer', 'Static')],
    'libprogram.so',
  )
  if (linuxStatic._tag !== 'PlannedCommand') return assert.fail('expected Linux link plan')
  assert.deepStrictEqual(linuxStatic.arguments, [
    '--target=x86_64-unknown-linux-gnu',
    '-shared',
    '-Wl,-Bstatic',
    '-lanswer',
    '-Wl,-Bdynamic',
    '-o',
    'libprogram.so',
  ])
})

it('rejects target- and artifact-incompatible structured link inputs', () => {
  const staticLibrary = NativeLinkInput.library('answer', 'Static')
  const framework = NativeLinkInput.framework('CoreFoundation')
  const searchPath = NativeLinkInput.searchPath('/sdk/lib')
  const darwinStatic = ToolchainPlan.nativeCommand(
    { clang, llvmAr: 'llvm-ar' },
    'NativeSharedLibrary',
    Target.aarch64AppleDarwin,
    [],
    [staticLibrary],
    'libanswer.dylib',
  )
  const linuxFramework = ToolchainPlan.nativeCommand(
    { clang, llvmAr: 'llvm-ar' },
    'NativeExecutable',
    Target.x8664UnknownLinuxGnu,
    [],
    [framework],
    'answer',
  )
  const archiveSearch = ToolchainPlan.nativeCommand(
    { clang, llvmAr: 'llvm-ar' },
    'NativeStaticLibrary',
    Target.aarch64AppleDarwin,
    [],
    [searchPath],
    'libanswer.a',
  )
  if (darwinStatic._tag !== 'UnsupportedNativePlan') return assert.fail('expected rejection')
  assert.strictEqual(darwinStatic.artifactKind, 'NativeSharedLibrary')
  assert.deepStrictEqual(darwinStatic.target, Target.aarch64AppleDarwin)
  assert.deepStrictEqual(darwinStatic.input, staticLibrary)
  assert.strictEqual(darwinStatic.reason, 'StaticLibraryTarget')

  if (linuxFramework._tag !== 'UnsupportedNativePlan') return assert.fail('expected rejection')
  assert.strictEqual(linuxFramework.artifactKind, 'NativeExecutable')
  assert.deepStrictEqual(linuxFramework.target, Target.x8664UnknownLinuxGnu)
  assert.deepStrictEqual(linuxFramework.input, framework)
  assert.strictEqual(linuxFramework.reason, 'FrameworkTarget')

  if (archiveSearch._tag !== 'UnsupportedNativePlan') return assert.fail('expected rejection')
  assert.strictEqual(archiveSearch.artifactKind, 'NativeStaticLibrary')
  assert.deepStrictEqual(archiveSearch.target, Target.aarch64AppleDarwin)
  assert.deepStrictEqual(archiveSearch.input, searchPath)
  assert.strictEqual(archiveSearch.reason, 'StaticArchiveInput')

  const relativeObject = NativeLinkInput.object('-Wl,--export-dynamic')
  const unsafePath = ToolchainPlan.nativeCommand(
    { clang, llvmAr: 'llvm-ar' },
    'NativeExecutable',
    Target.x8664UnknownLinuxGnu,
    [],
    [relativeObject],
    '/tmp/out',
  )
  assert.strictEqual(unsafePath._tag, 'UnsupportedNativePlan')
  if (unsafePath._tag !== 'UnsupportedNativePlan') return
  assert.strictEqual(unsafePath.artifactKind, 'NativeExecutable')
  assert.strictEqual(unsafePath.target, Target.x8664UnknownLinuxGnu)
  assert.strictEqual(unsafePath.input, relativeObject)
  assert.strictEqual(unsafePath.reason, 'PathNotAbsolute')
})

it('generates effect-reporting runtime source from escaped identities with closed status handling', () => {
  const source = ToolchainPlan.executableSource(termination('module.Error"\\nameé'))
  assert.include(source, 'identity = "module.Error\\"\\\\name\\303\\251";')
  assert.notInclude(source, 'Error: module.Error"\\name')
  assert.include(source, 'silk_write_text("unhandled error: ")')
  assert.include(source, 'default:\n      return 2;')
  assert.include(source, 'return ok ? 1 : 2;')
})

it('reports trap sites and failure paths only when the artifact declares them', () => {
  const bare = ToolchainPlan.executableSource(termination('module.Error'))
  assert.notInclude(bare, 'silk_trap_report_v1')
  assert.notInclude(bare, 'silk_write_path')
  const contract: Termination.Contract = Object.freeze({
    ...termination('module.Error'),
    report: Object.freeze({
      frames: Object.freeze(['module.main (module:1:1)']),
      failureSites: Object.freeze([
        Object.freeze({ identity: 'module.Error', origin: 'module.load (module:3:3)' }),
      ]),
      trapSites: Object.freeze([
        Object.freeze({ reason: 'division by zero', origin: 'module.calc (module:8:10)' }),
      ]),
    }),
  })
  const full = ToolchainPlan.executableSource(contract)
  assert.include(full, 'void silk_trap_report_v1(int site)')
  assert.include(full, '"fatal trap: "')
  assert.include(full, '"module.calc (module:8:10)"')
  assert.include(full, '"while handling: "')
  assert.include(full, '"module.load (module:3:3)"')
  assert.include(full, '"module.main (module:1:1)"')
})

it('includes coroutine storage only when suspension requests it', () => {
  const direct = ToolchainPlan.executableSource(termination())
  const suspended = ToolchainPlan.executableSource(termination(), CoroutineRuntime.symbols)
  assert.notInclude(direct, CoroutineRuntime.pushSymbol)
  assert.include(suspended, CoroutineRuntime.pushSymbol)
  assert.include(suspended, CoroutineRuntime.popSymbol)
})

it('separates a process entry from library-only hidden runtime source', () => {
  const executable = ToolchainPlan.executableSource(termination(), CoroutineRuntime.symbols)
  const library = ToolchainPlan.runtimeSource(CoroutineRuntime.symbols)
  assert.include(executable, 'int main(void)')
  assert.include(executable, 'extern int silk_main(void)')
  assert.notInclude(library, 'int main(')
  assert.notInclude(library, 'silk_main')
  assert.include(library, CoroutineRuntime.pushSymbol)
  assert.include(library, CoroutineRuntime.popSymbol)
})

it.effect(
  'includes the selected LLVM-Wasm freestanding runtime in the artifact cache identity',
  () =>
    Effect.gen(function* () {
      const compilationwasm32UnknownUnknown = yield* profileFor(Target.wasm32UnknownUnknown)
      const bitcode = Uint8Array.from([0, 1, 2, 3])
      const runtimeSource = NativeToolchain.artifactRuntimeSource(
        'WebAssemblyModule',
        termination(),
        [],
        true,
      )
      assert.strictEqual(runtimeSource, LlvmWasmRuntime.source)
      const keyFor = (runtimeSource: string) =>
        NativeToolchain.wasmArtifactCacheKey(
          toolchain,
          compilationwasm32UnknownUnknown,
          bitcode,
          runtimeSource,
        )
      const original = yield* keyFor(runtimeSource)
      const changed = yield* keyFor(`${runtimeSource}\n/* cache identity mutation */`)
      assert.notStrictEqual(original, changed)
      assert.match(original, /\.wasm$/)
      assert.match(changed, /\.wasm$/)
    }),
)

it.effect('authenticates artifact-cache payloads and rejects body or trailing corruption', () =>
  Effect.gen(function* () {
    let stored: Uint8Array | undefined
    const cache: NativeToolchain.ArtifactCache = Object.freeze({
      _tag: 'ArtifactCache',
      get: () => Effect.succeed(stored),
      set: (_key: string, bytes: Uint8Array) =>
        Effect.sync(() => {
          stored = Uint8Array.from(bytes)
        }),
    })
    const payload = Uint8Array.from([1, 2, 3, 4])
    yield* NativeToolchain.writeArtifactCache(cache, 'entry', payload)
    assert.deepStrictEqual(yield* NativeToolchain.readArtifactCache(cache, 'entry'), payload)
    assert.strictEqual(yield* NativeToolchain.readArtifactCache(cache, 'other-entry'), undefined)
    if (stored === undefined) return assert.fail('expected encoded cache entry')
    const encoded = stored

    const corrupted = Uint8Array.from(encoded)
    corrupted[corrupted.length - 1] = (corrupted[corrupted.length - 1] ?? 0) ^ 0xff
    stored = corrupted
    assert.strictEqual(yield* NativeToolchain.readArtifactCache(cache, 'entry'), undefined)

    const appended = new Uint8Array(encoded.length + 1)
    appended.set(encoded)
    stored = appended
    assert.strictEqual(yield* NativeToolchain.readArtifactCache(cache, 'entry'), undefined)
  }),
)

it.effect('yields a typed spawn failure with command, stage, and arbitrary cause', () =>
  Effect.gen(function* () {
    const target = yield* NativeToolchain.hostTarget()
    const compilation = yield* profileFor(target)
    const artifact = yield* artifactFor(target, 'release')
    let scopeRoot = ''
    const result = yield* Effect.result(
      NativeToolchain.withBuildScope('spawn-failure', (scope) => {
        scopeRoot = scope.root
        return NativeToolchain.emitObject(
          { _tag: 'Toolchain', clang: '/nonexistent/clang', llvmAr: 'llvm-ar' },
          scope,
          artifact,
          compilation,
        )
      }),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag !== 'Failure') return
    assert.strictEqual(result.failure._tag, 'ToolchainError')
    assert.strictEqual(result.failure.stage, 'object')
    assert.strictEqual(result.failure.reason._tag, 'SpawnFailed')
    if (result.failure.reason._tag !== 'SpawnFailed') return
    assert.strictEqual(result.failure.reason.planned.command, '/nonexistent/clang')
    assert.instanceOf(result.failure.reason.cause, Error)
    assert.strictEqual(existsSync(scopeRoot), false)
  }),
)

it.effect('reuses explicitly shared runtime bytes across cleaned build scopes', () =>
  Effect.gen(function* () {
    const target = yield* NativeToolchain.hostTarget()
    const cache = NativeToolchain.makeRuntimeObjectCache()
    const cachedToolchain = Object.freeze({ ...toolchain, runtimeObjectCache: cache })
    yield* NativeToolchain.withBuildScope('runtime-miss', (scope) =>
      NativeToolchain.compileExecutableRuntime(cachedToolchain, scope, target, termination()),
    )
    yield* NativeToolchain.withBuildScope('runtime-hit', (scope) =>
      NativeToolchain.compileExecutableRuntime(cachedToolchain, scope, target, termination()),
    )
    assert.deepEqual(NativeToolchain.runtimeObjectCacheStats(cache), {
      entries: 1,
      hits: 1,
      misses: 1,
    })
  }),
)

it.effect('removes a build scope after interruption', () =>
  Effect.gen(function* () {
    let scopeRoot = ''
    const fiber = yield* Effect.forkChild(
      NativeToolchain.withBuildScope('interrupted', (scope) => {
        scopeRoot = scope.root
        return Effect.never
      }),
    )
    yield* Effect.yieldNow
    yield* Fiber.interrupt(fiber)
    assert.isNotEmpty(scopeRoot)
    assert.strictEqual(existsSync(scopeRoot), false)
  }),
)

it.effect('retries a throwing scope cleanup without replacing the protected failure', () =>
  Effect.gen(function* () {
    const protectedFailure = Object.freeze({ _tag: 'ProtectedFailure' as const })
    let scopeRoot = ''
    let cleanupAttempts = 0
    const result = yield* Effect.result(
      NativeToolchain.withBuildScope(
        'cleanup-retry',
        (scope) => {
          scopeRoot = scope.root
          return Effect.fail(protectedFailure)
        },
        {
          cleanup: {
            remove: (path, options) => {
              cleanupAttempts += 1
              if (cleanupAttempts === 1) throw new Error('injected cleanup failure')
              rmSync(path, options)
            },
          },
        },
      ),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag !== 'Failure') return
    assert.strictEqual(result.failure, protectedFailure)
    assert.strictEqual(cleanupAttempts, 2)
    assert.strictEqual(existsSync(scopeRoot), false)
  }),
)

it.effect('surfaces persistent cleanup failure after a successful protected operation', () =>
  Effect.gen(function* () {
    const cleanupFailure = Object.freeze({ injected: 'persistent-scope-cleanup' })
    let scopeRoot = ''
    const result = yield* Effect.result(
      NativeToolchain.withBuildScope(
        'cleanup-failure-after-success',
        (scope) => {
          scopeRoot = scope.root
          return Effect.succeed(42)
        },
        {
          cleanup: {
            remove: () => {
              throw cleanupFailure
            },
          },
        },
      ),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag !== 'Failure') return
    assert.strictEqual(result.failure.operation, 'NativeToolchain.cleanupPath')
    assert.strictEqual(result.failure.stage, 'scope-cleanup')
    assert.strictEqual(result.failure.reason._tag, 'StorageFailed')
    if (result.failure.reason._tag !== 'StorageFailed') return
    assert.deepEqual(result.failure.reason.cause, {
      first: cleanupFailure,
      retry: cleanupFailure,
    })
    assert.strictEqual(existsSync(scopeRoot), false)
  }),
)

it.effect('preserves a primary failure when persistent cleanup also fails', () =>
  Effect.gen(function* () {
    const primaryFailure = Object.freeze({ _tag: 'ProtectedFailure' as const })
    const cleanupFailure = Object.freeze({ injected: 'persistent-scope-cleanup' })
    let scopeRoot = ''
    const result = yield* Effect.result(
      NativeToolchain.withBuildScope(
        'primary-and-cleanup-failure',
        (scope) => {
          scopeRoot = scope.root
          return Effect.fail(primaryFailure)
        },
        {
          cleanup: {
            remove: () => {
              throw cleanupFailure
            },
          },
        },
      ),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag !== 'Failure') return
    assert.strictEqual(result.failure, primaryFailure)
    assert.strictEqual(existsSync(scopeRoot), false)
  }),
)

it.effect('failed rename removes its temporary sibling and preserves the destination', () =>
  Effect.gen(function* () {
    const destination = join(testRoot, 'occupied-destination')
    mkdirSync(destination)
    const result = yield* Effect.result(
      NativeToolchain.atomicCommit(destination, Uint8Array.of(1, 2, 3)),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag !== 'Failure') return
    assert.strictEqual(result.failure.reason._tag, 'StorageFailed')
    assert.strictEqual(existsSync(destination), true)
    assert.deepEqual(
      readdirSync(testRoot).filter((name) => name.startsWith('occupied-destination.silk-tmp-')),
      [],
    )
  }),
)

it.effect('commits exact native-library interface siblings beside the primary artifact', () =>
  Effect.gen(function* () {
    const destination = join(testRoot, 'interface-success', 'libanswer.a')
    const primary = yield* NativeToolchain.atomicCommit(destination, ascii('archive'))
    const companions = yield* NativeToolchain.commitLibraryInterface(
      primary,
      destination,
      'answer',
      ascii('header\n'),
      ascii('{"silkForeignAbi":1}\n'),
    )
    assert.deepStrictEqual(companions, {
      _tag: 'LibraryInterfaceArtifacts',
      cHeader: join(testRoot, 'interface-success', 'answer.h'),
      abiManifest: join(testRoot, 'interface-success', 'answer.abi.json'),
    })
    assert.deepStrictEqual(readFileSync(companions.cHeader), ascii('header\n'))
    assert.deepStrictEqual(readFileSync(companions.abiManifest), ascii('{"silkForeignAbi":1}\n'))
  }),
)

it.effect('removes the primary and stale companions when manifest staging fails', () =>
  Effect.gen(function* () {
    const directory = join(testRoot, 'interface-failure')
    const destination = join(directory, 'libanswer.a')
    const primary = yield* NativeToolchain.atomicCommit(destination, ascii('archive'))
    const header = join(directory, 'answer.h')
    const manifest = join(directory, 'answer.abi.json')
    writeFileSync(header, ascii('stale header\n'))
    writeFileSync(manifest, ascii('{"stale":true}\n'))
    const unstagedManifest = new Proxy(ascii('{}\n'), {})

    const result = yield* Effect.result(
      NativeToolchain.commitLibraryInterface(
        primary,
        destination,
        'answer',
        ascii('header\n'),
        unstagedManifest,
      ),
    )

    assert.strictEqual(result._tag, 'Failure')
    assert.strictEqual(existsSync(primary), false)
    assert.strictEqual(existsSync(header), false)
    assert.strictEqual(existsSync(manifest), false)
  }),
)

it.effect('retries throwing atomic cleanup and leaves no staged sibling', () =>
  Effect.gen(function* () {
    const destination = join(testRoot, 'occupied-cleanup-retry')
    mkdirSync(destination)
    let cleanupAttempts = 0
    const result = yield* Effect.result(
      NativeToolchain.atomicCommit(destination, Uint8Array.of(4, 5, 6), {
        cleanup: {
          remove: (path, options) => {
            cleanupAttempts += 1
            if (cleanupAttempts === 1) throw new Error('injected cleanup failure')
            rmSync(path, options)
          },
        },
      }),
    )
    assert.strictEqual(result._tag, 'Failure')
    assert.strictEqual(cleanupAttempts, 2)
    assert.deepEqual(
      readdirSync(testRoot).filter((name) => name.startsWith('occupied-cleanup-retry.silk-tmp-')),
      [],
    )
  }),
)

it.effect('falls back to Node cleanup when injected atomic cleanup keeps failing', () =>
  Effect.gen(function* () {
    const destination = join(testRoot, 'occupied-cleanup-fallback')
    mkdirSync(destination)
    let cleanupAttempts = 0
    const result = yield* Effect.result(
      NativeToolchain.atomicCommit(destination, Uint8Array.of(7, 8, 9), {
        cleanup: {
          remove: () => {
            cleanupAttempts += 1
            throw new Error('injected persistent cleanup failure')
          },
        },
      }),
    )
    assert.strictEqual(result._tag, 'Failure')
    assert.strictEqual(cleanupAttempts, 2)
    assert.deepEqual(
      readdirSync(testRoot).filter((name) =>
        name.startsWith('occupied-cleanup-fallback.silk-tmp-'),
      ),
      [],
    )
  }),
)

it.effect('translates synchronously throwing runtime-cache reads with cache-stage provenance', () =>
  Effect.gen(function* () {
    const target = yield* NativeToolchain.hostTarget()
    const cause = Object.freeze({ injected: 'cache-read' })
    const cache: NativeToolchain.RuntimeObjectCache = Object.freeze({
      _tag: 'RuntimeObjectCache',
      get: () => {
        throw cause
      },
      set: () => Effect.void,
      stats: () => Object.freeze({ entries: 0, hits: 0, misses: 0 }),
    })
    const result = yield* Effect.result(
      NativeToolchain.withBuildScope('cache-read-failure', (scope) =>
        NativeToolchain.compileExecutableRuntime(
          Object.freeze({ ...toolchain, runtimeObjectCache: cache }),
          scope,
          target,
          termination(),
        ),
      ),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag !== 'Failure') return
    assert.strictEqual(result.failure.stage, 'cache-read')
    assert.strictEqual(result.failure.operation, 'NativeToolchain.RuntimeObjectCache.get')
    assert.strictEqual(result.failure.reason._tag, 'StorageFailed')
    if (result.failure.reason._tag !== 'StorageFailed') return
    assert.strictEqual(result.failure.reason.cause, cause)
  }),
)

it.effect(
  'translates synchronously throwing runtime-cache writes with cache-stage provenance',
  () =>
    Effect.gen(function* () {
      const target = yield* NativeToolchain.hostTarget()
      const cause = Object.freeze({ injected: 'cache-write' })
      const cache: NativeToolchain.RuntimeObjectCache = Object.freeze({
        _tag: 'RuntimeObjectCache',
        get: () => Effect.as(Effect.void, undefined),
        set: () => {
          throw cause
        },
        stats: () => Object.freeze({ entries: 0, hits: 0, misses: 0 }),
      })
      const result = yield* Effect.result(
        NativeToolchain.withBuildScope('cache-write-failure', (scope) =>
          NativeToolchain.compileExecutableRuntime(
            Object.freeze({ ...toolchain, runtimeObjectCache: cache }),
            scope,
            target,
            termination(),
          ),
        ),
      )
      assert.strictEqual(result._tag, 'Failure')
      if (result._tag !== 'Failure') return
      assert.strictEqual(result.failure.stage, 'cache-write')
      assert.strictEqual(result.failure.operation, 'NativeToolchain.RuntimeObjectCache.set')
      assert.strictEqual(result.failure.reason._tag, 'StorageFailed')
      if (result.failure.reason._tag !== 'StorageFailed') return
      assert.strictEqual(result.failure.reason.cause, cause)
    }),
)

it('classifies cached artifacts by container, kind, and every canonical target', () => {
  const fixtures: ReadonlyArray<{
    readonly kind: NativeToolchain.FinalArtifact['kind']
    readonly target: Target.Target
    readonly bytes: Uint8Array
  }> = [
    {
      kind: 'NativeExecutable',
      target: Target.aarch64AppleDarwin,
      bytes: machOImage(2),
    },
    {
      kind: 'NativeSharedLibrary',
      target: Target.aarch64AppleDarwin,
      bytes: machOImage(6),
    },
    {
      kind: 'NativeExecutable',
      target: Target.aarch64UnknownLinuxGnu,
      bytes: elfImage(183, 2),
    },
    {
      kind: 'NativeExecutable',
      target: Target.aarch64UnknownLinuxGnu,
      bytes: elfImage(183, 3, true),
    },
    {
      kind: 'NativeSharedLibrary',
      target: Target.aarch64UnknownLinuxGnu,
      bytes: elfImage(183, 3),
    },
    {
      kind: 'NativeExecutable',
      target: Target.x8664UnknownLinuxGnu,
      bytes: elfImage(62, 2),
    },
    {
      kind: 'NativeExecutable',
      target: Target.x8664UnknownLinuxGnu,
      bytes: elfImage(62, 3, true),
    },
    {
      kind: 'NativeSharedLibrary',
      target: Target.x8664UnknownLinuxGnu,
      bytes: elfImage(62, 3),
    },
    ...Target.native.map((target) => ({
      kind: 'NativeStaticLibrary' as const,
      target,
      bytes: staticArchive(nativeObjectFor(target)),
    })),
    {
      kind: 'WebAssemblyModule',
      target: Target.wasm32UnknownUnknown,
      bytes: Uint8Array.from([0, 97, 115, 109, 1, 0, 0, 0]),
    },
  ]
  const kinds: ReadonlyArray<NativeToolchain.FinalArtifact['kind']> = [
    'NativeExecutable',
    'NativeSharedLibrary',
    'NativeStaticLibrary',
    'WebAssemblyModule',
  ]

  for (const fixture of fixtures) {
    for (const kind of kinds) {
      for (const target of Target.all) {
        assert.strictEqual(
          NativeToolchain.isCachedArtifact(fixture.bytes, kind, target),
          kind === fixture.kind && target.id === fixture.target.id,
          `${fixture.kind}/${fixture.target.id} classified as ${kind}/${target.id}`,
        )
      }
    }
  }

  for (const kind of kinds) {
    for (const target of Target.all) {
      assert.strictEqual(NativeToolchain.isCachedArtifact(new Uint8Array(), kind, target), false)
      assert.strictEqual(
        NativeToolchain.isCachedArtifact(Uint8Array.from([0x7f, 0x45, 0x4c, 0x46]), kind, target),
        false,
      )
    }
  }

  const mixedArchive = staticArchive(machOImage(2), elfImage(62, 2))
  assert.strictEqual(
    NativeToolchain.isCachedArtifact(
      mixedArchive,
      'NativeStaticLibrary',
      Target.aarch64AppleDarwin,
    ),
    false,
  )
  assert.strictEqual(
    NativeToolchain.isCachedArtifact(
      mixedArchive,
      'NativeStaticLibrary',
      Target.x8664UnknownLinuxGnu,
    ),
    false,
  )
  assert.strictEqual(
    NativeToolchain.isCachedArtifact(
      malformedExtendedNameArchive(200_000),
      'NativeStaticLibrary',
      Target.x8664UnknownLinuxGnu,
    ),
    false,
  )
})

it.effect('returns validated cached WebAssembly bytes in memory', () =>
  Effect.gen(function* () {
    const target = Target.wasm32UnknownUnknown
    const destination = join(testRoot, 'cached-module.wasm')
    const bytes = Uint8Array.from([0, 97, 115, 109, 1, 0, 0, 0])
    const committed = yield* NativeToolchain.commitCachedArtifact(
      bytes,
      'WebAssemblyModule',
      target,
      destination,
    )
    assert.deepEqual(committed.bytes, bytes)
    assert.deepEqual(readFileSync(destination), bytes)
  }),
)

it.effect('rejects a missing linker input in the typed link channel', () =>
  Effect.gen(function* () {
    const target = yield* NativeToolchain.hostTarget()
    const result = yield* Effect.result(
      NativeToolchain.withBuildScope('missing-link-input', (scope) =>
        finalize(
          toolchain,
          scope,
          'NativeExecutable',
          target,
          [
            Object.freeze({
              _tag: 'PathArtifact',
              scope: scope.name,
              path: join(scope.root, 'missing.o'),
              target,
            }),
          ],
          [],
          join(testRoot, 'never-written'),
        ),
      ),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag !== 'Failure') return
    assert.strictEqual(result.failure.reason._tag, 'LinkFailed')
    if (result.failure.reason._tag !== 'LinkFailed') return
    assert.include(result.failure.reason.output, 'missing linker input')
  }),
)

it.effect('rejects missing archive paths and unsupported inputs before spawning', () =>
  Effect.gen(function* () {
    const target = yield* NativeToolchain.hostTarget()
    const missing = join(testRoot, 'missing-library.a')
    const missingResult = yield* Effect.result(
      NativeToolchain.withBuildScope('missing-archive-input', (scope) =>
        finalize(
          toolchain,
          scope,
          'NativeSharedLibrary',
          target,
          [],
          [NativeLinkInput.staticArchive(missing)],
          join(testRoot, 'never-written-shared'),
        ),
      ),
    )
    assert.strictEqual(missingResult._tag, 'Failure')
    if (missingResult._tag === 'Failure') {
      assert.strictEqual(missingResult.failure.reason._tag, 'LinkFailed')
      if (missingResult.failure.reason._tag === 'LinkFailed')
        assert.include(missingResult.failure.reason.output, missing)
    }

    const unsupportedInput = NativeLinkInput.library('answer', 'Dynamic')
    const neverToolchain: NativeToolchain.Toolchain = Object.freeze({
      _tag: 'Toolchain',
      clang: join(testRoot, 'missing-clang'),
      llvmAr: join(testRoot, 'missing-llvm-ar'),
    })
    const unsupportedResult = yield* Effect.result(
      NativeToolchain.withBuildScope('unsupported-archive-input', (scope) =>
        finalize(
          neverToolchain,
          scope,
          'NativeStaticLibrary',
          target,
          [],
          [unsupportedInput],
          join(testRoot, 'never-written-archive'),
        ),
      ),
    )
    assert.strictEqual(unsupportedResult._tag, 'Failure')
    if (unsupportedResult._tag !== 'Failure') return
    assert.strictEqual(unsupportedResult.failure.reason._tag, 'UnsupportedPlan')
    if (unsupportedResult.failure.reason._tag !== 'UnsupportedPlan') return
    assert.strictEqual(unsupportedResult.failure.reason.plan.artifactKind, 'NativeStaticLibrary')
    assert.deepStrictEqual(unsupportedResult.failure.reason.plan.target, target)
    assert.deepStrictEqual(unsupportedResult.failure.reason.plan.input, unsupportedInput)
    assert.strictEqual(unsupportedResult.failure.reason.plan.reason, 'StaticArchiveInput')
  }),
)

it.effect('creates byte-identical deterministic archives from the same object input', () =>
  Effect.gen(function* () {
    const target = yield* NativeToolchain.hostTarget()
    const firstDestination = join(testRoot, 'deterministic-first.a')
    const secondDestination = join(testRoot, 'deterministic-second.a')
    const artifacts = yield* NativeToolchain.withBuildScope('deterministic-archive', (scope) =>
      Effect.gen(function* () {
        const object = yield* NativeToolchain.writeArtifact(
          scope,
          target,
          'fixture.o',
          nativeObjectFor(target),
        )
        const first = yield* finalize(
          toolchain,
          scope,
          'NativeStaticLibrary',
          target,
          [object],
          [],
          firstDestination,
        )
        const second = yield* finalize(
          toolchain,
          scope,
          'NativeStaticLibrary',
          target,
          [object],
          [],
          secondDestination,
        )
        return Object.freeze({ first, second })
      }),
    )
    const { first, second } = artifacts
    assert.deepStrictEqual(first.bytes, second.bytes)
    assert.strictEqual(
      NativeToolchain.isCachedArtifact(first.bytes, 'NativeStaticLibrary', target),
      true,
    )
    assert.deepStrictEqual(first.bytes, readFileSync(firstDestination))
    assert.deepStrictEqual(second.bytes, readFileSync(secondDestination))
  }),
)

it.effect(
  'links the runtime and program while returning executable bytes in memory',
  () =>
    Effect.gen(function* () {
      const target = yield* NativeToolchain.hostTarget()
      const compilation = yield* profileFor(target)
      const artifact = yield* artifactFor(target, 'release')
      const destination = join(testRoot, 'linked-program')
      const linked = yield* NativeToolchain.withBuildScope('link-run', (scope) =>
        Effect.gen(function* () {
          const object = yield* NativeToolchain.emitObject(toolchain, scope, artifact, compilation)
          const runtime = yield* NativeToolchain.compileExecutableRuntime(
            toolchain,
            scope,
            target,
            artifact.termination,
          )
          assert.include(runtime.planned.arguments, 'cpp-output')
          assert.include(runtime.planned.arguments, join(scope.root, 'silk_runtime.i'))
          return yield* finalize(
            toolchain,
            scope,
            'NativeExecutable',
            target,
            [object.artifact, runtime.artifact],
            [],
            destination,
          )
        }),
      )
      assert.isAbove(linked.bytes.length, 0)
      assert.deepEqual(linked.bytes, readFileSync(linked.path))
      const run = spawnSync(linked.path, [], { encoding: 'utf8' })
      assert.strictEqual(run.status, 42)
    }),
  15_000,
)

it('keeps relocatable form, exact loader symbols and ordered scripts distinct', () => {
  const tools = { clang, llvmAr: 'llvm-ar' }
  const target = Target.x8664UnknownLinuxGnu
  const object = ToolchainPlan.nativeCommand(
    tools,
    'NativeObject',
    target,
    ['/program.o'],
    [{ _tag: 'Object', path: '/support.o' }],
    '/result.o',
  )
  assert.strictEqual(object._tag, 'PlannedCommand')
  if (object._tag === 'PlannedCommand') {
    assert.include(object.arguments, '-r')
    assert.include(object.arguments, '-nostdlib')
    assert.isBelow(object.arguments.indexOf('/program.o'), object.arguments.indexOf('/support.o'))
  }
  const entry = ToolchainPlan.nativeCommand(
    tools,
    'NativeExecutable',
    target,
    ['/program.o'],
    [{ _tag: 'LinkerScript', path: '/layout.ld' }],
    '/program',
    { kind: 'named', name: 'machine_start' },
  )
  assert.strictEqual(entry._tag, 'PlannedCommand')
  if (entry._tag === 'PlannedCommand') {
    assert.include(entry.arguments, '-nostartfiles')
    assert.include(entry.arguments, 'machine_start')
    assert.include(entry.arguments, '/layout.ld')
  }
})

it.effect('selects one provider with artifact pin precedence and no cross-host fallback', () =>
  Effect.gen(function* () {
    const target = Target.aarch64AppleDarwin
    const pin: PlatformSupply.Explicit = {
      kind: 'explicit',
      target: target.id,
      root: '/pinned/sdk',
      linker: '/pinned/ld',
      origin: 'artifact pin',
    }
    const chosen = yield* PlatformSupply.select(
      target,
      Target.x8664UnknownLinuxGnu.id,
      { kind: 'automatic' },
      pin,
      { kind: 'managed', name: 'unused' },
    )
    assert.strictEqual(chosen.origin, 'artifact')
    assert.deepEqual(chosen.request, pin)
    assert.isTrue(Object.isFrozen(chosen.request))
    for (const [request, code] of [
      [{ kind: 'automatic' }, 'HostMismatch'],
      [{ kind: 'managed', name: 'deferred' }, 'UnsupportedProvider'],
      [{ ...pin, target: Target.x8664UnknownLinuxGnu.id }, 'TargetMismatch'],
    ] as const) {
      const result = yield* Effect.result(
        PlatformSupply.select(target, Target.x8664UnknownLinuxGnu.id, request),
      )
      assert.strictEqual(result._tag, 'Failure')
      if (result._tag === 'Failure') assert.strictEqual(result.failure.code, code)
    }
    const invalid = yield* Effect.result(
      PlatformSupply.decode(
        { kind: 'explicit', target: target.id, root: '/sdk', linker: '/ld', flags: ['-bad'] },
        'manifest',
      ),
    )
    assert.strictEqual(invalid._tag, 'Failure')
  }),
)

it('preserves GNU input groups and discovers recursive script references without flattening', () => {
  const source =
    '/* contract */ SEARCH_DIR("=/usr/lib") GROUP ( libfirst.a AS_NEEDED ( -lsecond ) ) INCLUDE "layout.ld"\nSECTIONS { .text : { *(.text) } }'
  const parsed = LinkerScript.parse(source)
  assert.strictEqual(parsed._tag, 'Success')
  if (parsed._tag !== 'Success') return
  assert.deepEqual(
    parsed.success.references.map((reference) => [reference.kind, reference.value]),
    [
      ['search', '=/usr/lib'],
      ['input', 'libfirst.a'],
      ['input', '-lsecond'],
      ['include', 'layout.ld'],
    ],
  )
  const rendered = LinkerScript.render(parsed.success, [
    '/sdk/usr/lib',
    '/sdk/first.a',
    '/sdk/second.so',
    '/scope/layout.ld',
  ])
  assert.include(rendered, 'GROUP ( "/sdk/first.a" AS_NEEDED ( "/sdk/second.so" ) )')
  assert.include(rendered, 'SECTIONS { .text : { *(.text) } }')
  assert.strictEqual(LinkerScript.parse('INPUT ( missing.o')._tag, 'Failure')
  assert.strictEqual(LinkerScript.parse('STARTUP(other.o)')._tag, 'Failure')
  assert.strictEqual(LinkerScript.parse('LIB(hidden.a)')._tag, 'Failure')
})

it('distinguishes inline Darwin stub reexports from external and incompatible targets', () => {
  const source = `--- !tapi-tbd
tbd-version: 4
targets: [ arm64-macos ]
install-name: '/usr/lib/root.dylib'
reexported-libraries:
  - targets: [ arm64-macos ]
    libraries: [ '/usr/lib/inline.dylib', '/usr/lib/external.dylib' ]
  - targets: [ x86_64-macos ]
    libraries: [ '/usr/lib/wrong.dylib' ]
--- !tapi-tbd
tbd-version: 4
targets: [ arm64-macos ]
install-name: '/usr/lib/inline.dylib'
...
`
  const parsed = NativeStub.parse(source)
  assert.strictEqual(parsed._tag, 'Success')
  if (parsed._tag !== 'Success') return
  assert.deepEqual(parsed.success.imports, ['/usr/lib/external.dylib'])
  assert.strictEqual(
    NativeStub.parse(source.replaceAll('arm64-macos', 'arm64-ios'))._tag,
    'Failure',
  )
  assert.strictEqual(
    NativeStub.parse(source.replaceAll('tbd-version: 4', 'tbd-version: 5'))._tag,
    'Failure',
  )
})

it('reads driver argv without interpreting shell syntax', () => {
  const parsed = NativeLinkResolver.argumentsOf(
    ' "/path with spaces/ld" "-o" "$(unchanged)" "escaped\\\"quote"',
  )
  assert.strictEqual(parsed._tag, 'Success')
  if (parsed._tag === 'Success')
    assert.deepEqual(parsed.success, [
      '/path with spaces/ld',
      '-o',
      '$(unchanged)',
      'escaped"quote',
    ])
  assert.strictEqual(NativeLinkResolver.argumentsOf('"unfinished')._tag, 'Failure')
})

it.effect(
  'accounts for selected bytes and input ordering while ignoring non-emitted path relocation',
  () =>
    Effect.gen(function* () {
      const target = yield* NativeToolchain.hostTarget()
      const profile = yield* profileFor(target)
      const selected = yield* NativeToolchain.resolveToolchain(toolchain, profile)
      yield* NativeToolchain.withBuildScope(
        'supply-identities',
        Effect.fnUntraced(function* (scope) {
          const a = yield* NativeToolchain.writeArtifact(
            scope,
            target,
            'a.o',
            nativeObjectFor(target),
          )
          const b = yield* NativeToolchain.writeArtifact(
            scope,
            target,
            'b.o',
            new Uint8Array([...nativeObjectFor(target), 1]),
          )
          mkdirSync(join(scope.root, 'relocated'))
          const relocated = yield* NativeToolchain.writeArtifact(
            scope,
            target,
            'relocated/a.o',
            nativeObjectFor(target),
          )
          const entry = {
            request: { kind: 'default' },
            composition: { kind: 'default' },
            resolved: { kind: 'default' },
          } as const
          const plan = Effect.fnUntraced(function* (
            objects: ReadonlyArray<NativeToolchain.PathArtifact>,
            name = 'output.a',
            helpers: ReadonlyArray<HelperCapability.Report> = [],
          ) {
            return yield* NativeToolchain.planNativeLink(
              selected,
              scope,
              'NativeStaticLibrary',
              profile,
              objects,
              [],
              join(scope.root, name),
              entry,
              helpers,
            )
          })
          const first = yield* plan([a, b])
          const moved = yield* plan([relocated, b], 'relocated.a')
          const swapped = yield* plan([b, a])
          assert.strictEqual(first.identity, moved.identity)
          assert.notStrictEqual(first.identity, swapped.identity)
          const helperInventory: ObjectSymbols.Inventory = {
            format: target.operatingSystem === 'darwin' ? 'macho' : 'elf',
            symbols: [
              {
                name: target.operatingSystem === 'darwin' ? '_memcpy' : 'memcpy',
                defined: false,
                weak: false,
                visibility: 'default',
              },
            ],
            references: [],
          }
          const origins = { foreignImports: [], foreignStatics: [], nativeRuntimeSymbols: [] }
          const hostedHelper = yield* HelperCapability.reconcile(
            helperInventory,
            origins,
            profile,
            a.path,
            'same-object',
          )
          const bareProfile = yield* CompilationProfile.normalize({
            target: target.id,
            artifact: 'object',
            libc: 'none',
          })
          const bareHelper = yield* HelperCapability.reconcile(
            helperInventory,
            origins,
            bareProfile,
            a.path,
            'same-object',
          )
          assert.notStrictEqual(hostedHelper.identity, bareHelper.identity)
          const hostedPlan = yield* plan([a, b], 'hosted.a', [hostedHelper])
          const barePlan = yield* plan([a, b], 'bare.a', [bareHelper])
          assert.notDeepEqual(
            NativeToolchain.finalArtifactCacheAdmission('NativeStaticLibrary', hostedPlan),
            NativeToolchain.finalArtifactCacheAdmission('NativeStaticLibrary', barePlan),
          )
          const wrongTarget =
            target.operatingSystem === 'darwin'
              ? Target.x8664UnknownLinuxGnu
              : Target.aarch64AppleDarwin
          const wrongObject = yield* NativeToolchain.writeArtifact(
            scope,
            target,
            'wrong.o',
            nativeObjectFor(wrongTarget),
          )
          const incompatible = yield* Effect.result(plan([wrongObject]))
          assert.strictEqual(incompatible._tag, 'Failure')
          if (incompatible._tag === 'Failure') {
            assert.strictEqual(incompatible.failure.reason._tag, 'SupplyFailed')
            if (incompatible.failure.reason._tag === 'SupplyFailed')
              assert.strictEqual(incompatible.failure.reason.failure.code, 'TargetMismatch')
          }

          assert.deepEqual(
            NativeToolchain.finalArtifactCacheAdmission('NativeStaticLibrary', first),
            { _tag: 'CompleteNativePlan', identity: first.identity },
          )
          const changed = new Uint8Array([...nativeObjectFor(target), 2])
          writeFileSync(a.path, changed)
          const stale = yield* Effect.result(
            PlatformSupplyResolver.validateFiles(first.inputs).pipe(
              Effect.provide(NodeServices.layer),
            ),
          )
          assert.strictEqual(stale._tag, 'Failure')
          if (stale._tag === 'Failure') assert.strictEqual(stale.failure.code, 'ChangedInput')
          assert.notStrictEqual(first.identity, (yield* plan([a, b])).identity)
        }),
      )
    }),
)

it('freezes only admitted discovery environment channels', () => {
  const environment = {
    PATH: '/selected/tools',
    SDKROOT: '/selected/sdk',
    CPATH: '/unrecorded/include',
  }
  const resolver = PlatformSupplyResolver.make(environment)
  environment.SDKROOT = '/changed/sdk'
  assert.deepEqual(resolver.environment, { PATH: '/selected/tools', SDKROOT: '/selected/sdk' })
  assert.isTrue(Object.isFrozen(resolver.environment))
})

it.effect(
  'validates explicit Darwin SDK architecture, deployment and libSystem independently',
  () =>
    Effect.gen(function* () {
      const target = Target.aarch64AppleDarwin
      const profile = yield* profileFor(target)
      const sdk = join(testRoot, 'sdk-capabilities')
      mkdirSync(join(sdk, 'usr/lib'), { recursive: true })
      const metadata = join(sdk, 'SDKSettings.json')
      const system = join(sdk, 'usr/lib/libSystem.tbd')
      const settings = {
        Version: '15.5',
        SupportedTargets: { macosx: { Archs: ['arm64'], MaximumDeploymentTarget: '15.5.99' } },
      }
      writeFileSync(
        metadata,
        yield* Schema.encodeEffect(Schema.fromJsonString(Schema.Unknown))(settings).pipe(
          Effect.orDie,
        ),
      )
      writeFileSync(
        system,
        "--- !tapi-tbd\ntbd-version: 4\ntargets: [ arm64-macos ]\ninstall-name: '/usr/lib/libSystem.B.dylib'\n",
      )
      const resolver = PlatformSupplyResolver.make({
        PATH: testPath,
        SDKROOT: '/must-not-be-selected',
      })
      const resolve = Effect.fnUntraced(function* (deployment?: string) {
        return yield* PlatformSupplyResolver.resolveSupply(resolver, {
          profile: { ...profile, deployment },
          host: Target.x8664UnknownLinuxGnu.id,
          clang,
          llvmAr: toolchain.llvmAr,
          request: {
            kind: 'explicit',
            target: target.id,
            root: sdk,
            linker: clang,
            origin: 'explicit SDK fixture',
          },
        }).pipe(Effect.provide(NodeServices.layer))
      })
      const selected = yield* resolve()
      assert.strictEqual(selected.version, '15.5')
      assert.isUndefined(selected.consultedEnvironment['SDKROOT'])
      const deployment = yield* Effect.result(resolve('16.0.0'))
      assert.strictEqual(deployment._tag, 'Failure')
      if (deployment._tag === 'Failure')
        assert.strictEqual(deployment.failure.code, 'DeploymentMismatch')
      writeFileSync(
        metadata,
        yield* Schema.encodeEffect(Schema.fromJsonString(Schema.Unknown))({
          ...settings,
          SupportedTargets: { macosx: { Archs: ['x86_64'] } },
        }).pipe(Effect.orDie),
      )
      const architecture = yield* Effect.result(resolve())
      assert.strictEqual(architecture._tag, 'Failure')
      if (architecture._tag === 'Failure')
        assert.strictEqual(architecture.failure.code, 'TargetMismatch')
      writeFileSync(
        metadata,
        yield* Schema.encodeEffect(Schema.fromJsonString(Schema.Unknown))(settings).pipe(
          Effect.orDie,
        ),
      )
      writeFileSync(system, 'wrong libSystem contract')
      const library = yield* Effect.result(resolve())
      assert.strictEqual(library._tag, 'Failure')
      if (library._tag === 'Failure') {
        assert.strictEqual(library.failure.code, 'TargetMismatch')
        assert.strictEqual(library.failure.origin, 'SDK libSystem contract')
      }
      rmSync(system)
      const missing = yield* Effect.result(resolve())
      assert.strictEqual(missing._tag, 'Failure')
      if (missing._tag === 'Failure') assert.include(missing.failure.subject, 'libSystem.tbd')
    }),
)

it.effect('keys C objects by consumed headers and freezes preprocessing for inspection', () =>
  Effect.gen(function* () {
    const target = yield* NativeToolchain.hostTarget()
    const profile = yield* profileFor(target)
    const cache = NativeToolchain.makeRuntimeObjectCache()
    const selected = yield* NativeToolchain.resolveToolchain(
      { ...toolchain, runtimeObjectCache: cache },
      profile,
    )
    yield* NativeToolchain.withBuildScope(
      'header-identities',
      Effect.fnUntraced(function* (scope) {
        const header = join(scope.root, 'selected.h')
        writeFileSync(header, '#define VALUE 42\n')
        const source = '#include "selected.h"\nint value(void) { return VALUE; }'
        const first = yield* NativeToolchain.compileCObject(
          selected,
          scope,
          target,
          'consumer',
          source,
        )
        writeFileSync(join(scope.root, 'unused.h'), '#define UNUSED 9\n')
        const same = yield* NativeToolchain.compileCObject(
          selected,
          scope,
          target,
          'consumer',
          source,
        )
        writeFileSync(header, '#define VALUE 43\n')
        const changed = yield* NativeToolchain.compileCObject(
          selected,
          scope,
          target,
          'consumer',
          source,
        )
        assert.isDefined(first.artifact.translation)
        assert.strictEqual(
          first.artifact.translation?.identity,
          same.artifact.translation?.identity,
        )
        assert.notStrictEqual(
          first.artifact.translation?.identity,
          changed.artifact.translation?.identity,
        )
        assert.include(first.artifact.translation?.source ?? '', 'return 42')
        assert.include(changed.artifact.translation?.source ?? '', 'return 43')
        assert.deepEqual(NativeToolchain.runtimeObjectCacheStats(cache), {
          entries: 2,
          hits: 1,
          misses: 2,
        })
        assert.isTrue(
          first.artifact.translation?.headers.every((input) => !input.path.endsWith('unused.h')),
        )
      }),
    )
  }),
)

it.effect(
  'resolves foreign absolute symlinks inside the sysroot and rejects retargeted snapshots',
  () =>
    Effect.gen(function* () {
      const root = join(testRoot, 'symlink-root')
      mkdirSync(join(root, 'usr/lib'), { recursive: true })
      writeFileSync(join(root, 'usr/lib/first'), 'same bytes')
      writeFileSync(join(root, 'usr/lib/second'), 'same bytes')
      symlinkSync('/usr/lib', join(root, 'lib'))
      symlinkSync('first', join(root, 'usr/lib/selected'))
      const selected = yield* PlatformSupplyResolver.file(
        join(root, 'lib/selected'),
        'library',
        'sysroot fixture',
        root,
      )
      assert.strictEqual(
        selected.path,
        yield* PlatformSupplyResolver.physicalPath(join(root, 'usr/lib/first')),
      )
      rmSync(join(root, 'usr/lib/selected'))
      symlinkSync('second', join(root, 'usr/lib/selected'))
      const changed = yield* Effect.result(PlatformSupplyResolver.validateFiles([selected]))
      assert.strictEqual(changed._tag, 'Failure')
      if (changed._tag === 'Failure') assert.strictEqual(changed.failure.code, 'ChangedInput')
      symlinkSync('loop', join(root, 'usr/lib/loop'))
      const cycle = yield* Effect.result(
        PlatformSupplyResolver.physicalPath(join(root, 'lib/loop'), root),
      )
      assert.strictEqual(cycle._tag, 'Failure')
      if (cycle._tag === 'Failure') assert.strictEqual(cycle.failure.code, 'UnsupportedInput')
    }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect(
  'reads symbols and relocations from pinned native and Wasm objects and rejects truncated tables',
  () =>
    Effect.gen(function* () {
      const fixture = yield* Schema.decodeEffect(
        Schema.fromJsonString(
          Schema.Struct({
            objects: Schema.Record(Schema.String, Schema.String),
            cycles: Schema.Record(Schema.String, Schema.String),
          }),
        ),
      )(readFileSync(new URL('./fixtures/helper-object-symbols.json', import.meta.url), 'utf8'))
      for (const target of [...Target.native, Target.wasm32UnknownUnknown]) {
        const encoded = fixture.objects[target.id]
        assert.isDefined(encoded)
        if (encoded === undefined) return assert.fail('Missing object fixture')
        const bytes = Uint8Array.from(Buffer.from(encoded, 'base64'))
        const inventory = ObjectSymbols.inspect(bytes, target)
        if (Result.isFailure(inventory)) return assert.fail(inventory.failure.detail)
        const normalized = inventory.success.symbols.map((entry) => ({
          ...entry,
          name: HelperCapability.symbolName(target, entry.name),
        }))
        assert.deepEqual(
          normalized.find((entry) => entry.name === 'helper_entry'),
          {
            name: 'helper_entry',
            defined: true,
            weak: false,
            visibility: 'hidden',
          },
        )
        assert.isTrue(
          normalized.some((entry) => entry.name === 'weak_entry' && entry.defined && entry.weak),
        )
        assert.deepEqual(
          normalized
            .filter((entry) => !entry.defined && !entry.name.startsWith('__'))
            .map((entry) => entry.name)
            .sort(),
          ['foreign_data', 'foreign_value'],
        )
        assert.deepEqual(
          inventory.success.references
            .map((name) => HelperCapability.symbolName(target, name))
            .filter((name) => !name.startsWith('__')),
          ['foreign_data', 'foreign_value'],
        )
        const cycleBytes = fixture.cycles[target.id]
        if (cycleBytes === undefined) return assert.fail('Missing cycle object fixture')
        const cycle = ObjectSymbols.inspect(
          Uint8Array.from(Buffer.from(cycleBytes, 'base64')),
          target,
        )
        if (Result.isFailure(cycle)) return assert.fail(cycle.failure.detail)
        const profile = yield* CompilationProfile.normalize({ target: target.id })
        const provider = yield* HelperCapability.provider('memcpy', profile)
        const verified = yield* Effect.result(
          HelperCapability.verifyProvider(provider, cycle.success, target),
        )
        if (Result.isSuccess(verified))
          return assert.fail('Emitted self-reference escaped verification')
        assert.strictEqual(verified.failure.code, 'ProviderCycle')
        for (const length of [0, 7, 24, bytes.length - 1])
          assert.isTrue(Result.isFailure(ObjectSymbols.inspect(bytes.subarray(0, length), target)))
      }
    }),
)

it.effect(
  'accounts emitted helper ABIs separately from source foreign calls and runtime contracts',
  () =>
    Effect.gen(function* () {
      const profile = yield* CompilationProfile.normalize({ target: 'x86_64-unknown-linux-gnu' })
      const foreign: Backend.ForeignImport = {
        symbol: 'foreign_read',
        parameters: [],
        result: 'i32',
        variadic: false,
        contract: ForeignContract.conservative,
      }
      const input = {
        foreignImports: [foreign],
        foreignStatics: [],
        nativeRuntimeSymbols: ['malloc'],
      }
      const symbols = ['memcpy', 'fmodf', 'malloc', 'foreign_read'].map((name) => ({
        name,
        defined: false,
        weak: false,
        visibility: 'default' as const,
      }))
      const report = yield* HelperCapability.reconcile(
        { format: 'elf', symbols, references: symbols.map((entry) => entry.name) },
        input,
        profile,
        'program.o',
        'object-digest',
      )
      assert.deepEqual(
        report.requirements.map((entry) => [
          entry.contract.symbol,
          entry.contract.parameters,
          entry.contract.result,
        ]),
        [
          ['fmodf', ['f32', 'f32'], 'f32'],
          ['memcpy', ['pointer', 'pointer', 'u64'], 'pointer'],
        ],
      )
      assert.deepEqual(report.runtime, ['malloc'])
      assert.deepEqual(report.foreign, ['foreign_read'])
      assert.deepEqual(HelperCapability.linkInputs([report]), [
        NativeLinkInput.library('m', 'Dynamic'),
      ])
      const empty = yield* HelperCapability.reconcile(
        { format: 'elf', symbols: [], references: [] },
        input,
        profile,
        'empty.o',
        'empty-digest',
      )
      assert.deepEqual(HelperCapability.linkInputs([empty]), [])
      assert.notStrictEqual(report.identity, empty.identity)
      for (const [symbol, code] of [
        ['unknown_helper', 'UnexplainedSymbol'],
        ['__atomic_load_16', 'UnsupportedFamily'],
        ['__stack_chk_fail', 'UnsupportedFamily'],
        ['__divti3', 'UnsupportedFamily'],
        ['__rust_probestack', 'UnsupportedFamily'],
        ['__asan_report_load8', 'UnsupportedFamily'],
        ['_Unwind_Resume', 'UnsupportedFamily'],
      ]) {
        const result = yield* Effect.result(HelperCapability.provider(symbol ?? '', profile))
        if (Result.isSuccess(result)) return assert.fail('Unexpected helper provider')
        assert.strictEqual(result.failure.code, code)
      }
      const noLibc = yield* CompilationProfile.normalize({
        target: profile.target.id,
        libc: 'none',
        artifact: 'object',
        entry: { kind: 'none' },
        runtime: { kind: 'none' },
      })
      const absent = yield* Effect.result(HelperCapability.provider('fmod', noLibc))
      if (Result.isSuccess(absent)) return assert.fail('Unexpected no-libc arithmetic provider')
      assert.strictEqual(absent.failure.code, 'MissingProvider')
    }),
)

it.effect(
  'rejects direct and transitive provider cycles, incompatible targets and emitted self dependencies',
  () =>
    Effect.gen(function* () {
      const profile = yield* CompilationProfile.normalize({ target: 'aarch64-apple-darwin' })
      const copy = yield* HelperCapability.provider('memcpy', profile)
      const move = yield* HelperCapability.provider('memmove', profile)
      const abi = yield* Effect.result(
        HelperCapability.verifyExports(
          copy,
          [
            {
              symbol: 'memcpy',
              parameters: [],
              result: 'i32',
              variadic: false,
              contract: ForeignContract.conservative,
            },
          ],
          profile.target,
        ),
      )
      if (Result.isSuccess(abi)) return assert.fail('Mismatched C helper signature was admitted')
      assert.strictEqual(abi.failure.code, 'IncompatibleProvider')

      for (const [providers, code] of [
        [[{ ...copy, requires: ['memcpy'] }], 'ProviderCycle'],
        [
          [
            { ...copy, requires: ['memmove'] },
            { ...move, requires: ['memcpy'] },
          ],
          'ProviderCycle',
        ],
        [[{ ...copy, requires: ['missing'] }], 'MissingProvider'],
        [[{ ...copy, targets: [] }], 'IncompatibleProvider'],
      ] as const) {
        const outcome = yield* Effect.result(
          HelperCapability.closure(['memcpy'], providers, profile.target),
        )
        if (Result.isSuccess(outcome)) return assert.fail('Unexpected valid provider graph')
        assert.strictEqual(outcome.failure.code, code)
      }
      const outcome = yield* Effect.result(
        HelperCapability.verifyProvider(
          copy,
          {
            format: 'macho',
            symbols: [{ name: '_memcpy', defined: true, weak: false, visibility: 'default' }],
            references: ['_memcpy'],
          },
          profile.target,
        ),
      )
      if (Result.isSuccess(outcome)) return assert.fail('Unexpected self-reference admission')
      assert.strictEqual(outcome.failure.code, 'ProviderCycle')
      assert.deepEqual(outcome.failure.origins, [copy.id, 'memcpy', copy.id])
    }),
)
