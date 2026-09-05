import * as CompilationProfile from '../src/CompilationProfile.js'
import { spawnSync } from 'node:child_process'
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
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

const machOImage = (fileType: 2 | 6): Uint8Array => {
  const bytes = new Uint8Array(32)
  bytes.set([0xcf, 0xfa, 0xed, 0xfe])
  setUint32LittleEndian(bytes, 4, 0x0100_000c)
  setUint32LittleEndian(bytes, 12, fileType)
  return bytes
}

const elfImage = (machine: 62 | 183, fileType: 2 | 3, hasInterpreter = false): Uint8Array => {
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
      return machOImage(2)
    case 'aarch64-unknown-linux-gnu':
      return elfImage(183, 2)
    case 'x86_64-unknown-linux-gnu':
      return elfImage(62, 2)
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

it.effect('includes the selected native clock runtime in the artifact cache identity', () =>
  Effect.gen(function* () {
    const target = yield* NativeToolchain.hostTarget()
    const compilation = yield* profileFor(target)
    const selected = [
      [],
      ['silk_os_monotonic_clock_now_v1'],
      ['silk_os_monotonic_clock_now_v1', 'silk_os_monotonic_clock_wait_until_v1'],
    ] as const
    const keys = []
    for (const symbols of selected) {
      keys.push(
        yield* NativeToolchain.artifactCacheKey(
          toolchain,
          'NativeExecutable',
          compilation,
          Uint8Array.from([0, 1, 2, 3]),
          ToolchainPlan.executableSource(termination(), symbols),
          join(testRoot, 'clock-runtime-cache'),
        ),
      )
    }
    assert.strictEqual(new Set(keys).size, selected.length)
  }),
)

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
      )
      assert.strictEqual(runtimeSource, LlvmWasmRuntime.source)
      const keyFor = (runtimeSource: string) =>
        NativeToolchain.artifactCacheKey(
          toolchain,
          'WebAssemblyModule',
          compilationwasm32UnknownUnknown,
          bitcode,
          runtimeSource,
          join(testRoot, 'runtime.wasm'),
        )
      const original = yield* keyFor(runtimeSource)
      const changed = yield* keyFor(`${runtimeSource}\n/* cache identity mutation */`)
      assert.notStrictEqual(original, changed)
      assert.match(original, /\.wasm$/)
      assert.match(changed, /\.wasm$/)
    }),
)

it.effect('separates every final artifact kind in cache identity and extension', () =>
  Effect.gen(function* () {
    const target = yield* NativeToolchain.hostTarget()
    const compilation = yield* profileFor(target)
    const bitcode = Uint8Array.from([0, 1, 2, 3])
    const kinds = ['NativeExecutable', 'NativeSharedLibrary', 'NativeStaticLibrary'] as const
    const keys = []
    for (const kind of kinds) {
      keys.push(
        yield* NativeToolchain.artifactCacheKey(
          toolchain,
          kind,
          compilation,
          bitcode,
          kind === 'NativeExecutable'
            ? ToolchainPlan.executableSource(termination())
            : ToolchainPlan.runtimeSource(),
          join(testRoot, 'artifact'),
        ),
      )
    }
    assert.strictEqual(new Set(keys).size, kinds.length)
    assert.match(keys[0] ?? '', /\.bin$/)
    assert.match(keys[1] ?? '', /\.(dylib|so)$/)
    assert.match(keys[2] ?? '', /\.a$/)
  }),
)

it.effect('separates Darwin shared-library install names in one artifact cache', () =>
  Effect.gen(function* () {
    const compilationaarch64AppleDarwin = yield* profileFor(Target.aarch64AppleDarwin)
    const cache = NativeToolchain.makeDiskArtifactCache(join(testRoot, 'darwin-install-cache'))
    const keyFor = (name: string) =>
      NativeToolchain.artifactCacheKey(
        toolchain,
        'NativeSharedLibrary',
        compilationaarch64AppleDarwin,
        Uint8Array.from([0, 1, 2, 3]),
        ToolchainPlan.runtimeSource(),
        join(testRoot, name),
      )
    const foo = yield* keyFor('libfoo.dylib')
    const bar = yield* keyFor('libbar.dylib')
    assert.notStrictEqual(foo, bar)
    yield* NativeToolchain.writeArtifactCache(cache, foo, Uint8Array.from([42]))
    assert.strictEqual(yield* NativeToolchain.readArtifactCache(cache, bar), undefined)
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

it.effect('covers request-supplied object bytes and the ordered library list in the key', () =>
  Effect.gen(function* () {
    const target = yield* NativeToolchain.hostTarget()
    const compilation = yield* profileFor(target)
    const objectA = join(testRoot, 'key-a.o')
    const objectB = join(testRoot, 'key-b.o')
    writeFileSync(objectA, Uint8Array.from([1, 2, 3]))
    writeFileSync(objectB, Uint8Array.from([1, 2, 4]))
    const keyFor = (inputs: ReadonlyArray<NativeLinkInput.NativeLinkInput>) =>
      NativeToolchain.artifactCacheKey(
        toolchain,
        'NativeExecutable',
        compilation,
        Uint8Array.from([0, 1, 2, 3]),
        ToolchainPlan.executableSource(termination()),
        join(testRoot, 'native-input-cache'),
        inputs,
      )
    const keys = [
      yield* keyFor([]),
      yield* keyFor([NativeLinkInput.object(objectA)]),
      yield* keyFor([NativeLinkInput.object(objectB)]),
      yield* keyFor([NativeLinkInput.object(objectA), NativeLinkInput.library('c', 'Dynamic')]),
      yield* keyFor([
        NativeLinkInput.object(objectA),
        NativeLinkInput.library('c', 'Dynamic'),
        NativeLinkInput.library('m', 'Dynamic'),
      ]),
      yield* keyFor([
        NativeLinkInput.object(objectA),
        NativeLinkInput.library('m', 'Dynamic'),
        NativeLinkInput.library('c', 'Dynamic'),
      ]),
    ]
    assert.strictEqual(new Set(keys).size, keys.length)
    assert.strictEqual(
      yield* keyFor([NativeLinkInput.object(objectA), NativeLinkInput.library('c', 'Dynamic')]),
      keys[3],
    )
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
        NativeToolchain.NativeFinalizer.finalize(
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
        NativeToolchain.NativeFinalizer.finalize(
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
        NativeToolchain.NativeFinalizer.finalize(
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
        const first = yield* NativeToolchain.NativeFinalizer.finalize(
          toolchain,
          scope,
          'NativeStaticLibrary',
          target,
          [object],
          [],
          firstDestination,
        )
        const second = yield* NativeToolchain.NativeFinalizer.finalize(
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
          assert.deepEqual(runtime.planned.arguments, [
            `--target=${target.id}`,
            '-c',
            '-x',
            'c',
            join(scope.root, 'silk_runtime.c'),
            '-O2',
            '-fPIC',
            '-fvisibility=hidden',
            '-o',
            join(scope.root, 'silk_runtime.o'),
          ])
          return yield* NativeToolchain.NativeFinalizer.finalize(
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
