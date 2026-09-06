import type * as CompilationProfile from './CompilationProfile.js'
/**
 * Pure planning for the pinned native toolchain: fixed optimization profiles, the exact
 * structured commands the orchestration issues (never a shell string), and the minimal C runtime
 * runtime source. Browser-safe by construction — execution lives in `NativeToolchain`.
 */

import type * as Backend from './Backend.js'
import type * as ArtifactKind from './ArtifactKind.js'
import * as CoroutineRuntime from './CoroutineRuntime.js'
import * as NativeLinkInput from './NativeLinkInput.js'
import * as NativeTermination from './NativeTermination.js'
import * as OsRuntime from './OsRuntime.js'
import type * as Target from './Target.js'
import type * as Termination from './Termination.js'

/** The fixed optimization profiles. There is no configurable pass pipeline. */
export type OptimizationProfile = 'debug' | 'release' | 'release-with-debug'

/** One planned process invocation: the pinned command and its structured arguments. */
export interface PlannedCommand {
  readonly _tag: 'PlannedCommand'
  readonly target: Target.Target
  readonly command: string
  readonly arguments: ReadonlyArray<string>
}

/** The codegen mode a profile's bitcode is emitted with. */
export const codegenModeFor = (profile: OptimizationProfile): 'debug' | 'release' =>
  profile === 'release' ? 'release' : 'debug'

/** Projects logical optimization/debug choices into the fixed toolchain optimization labels. */
export const optimizationFor = (
  self: Pick<CompilationProfile.Input, 'optimization' | 'debug'>,
): OptimizationProfile => {
  if (self.optimization !== 'speed') return 'debug'
  return self.debug === false ? 'release' : 'release-with-debug'
}

/** Lowers logical code-generation choices into deterministic Clang arguments. */
export const compilationArguments = (
  profile: CompilationProfile.CompilationProfile,
): ReadonlyArray<string> =>
  Object.freeze([
    profile.optimization === 'none' ? '-O0' : '-O2',
    ...(profile.debug ? ['-g'] : []),
    ...(profile.target.kind === 'Native' && profile.codeModel === 'large'
      ? ['-mcmodel=large']
      : []),
    ...(profile.target.operatingSystem === 'darwin' && profile.deployment !== undefined
      ? [`-mmacosx-version-min=${profile.deployment}`]
      : []),
  ])

/** Plans the pinned Clang `-c` invocation that turns bitcode into a target object. */
export const objectCommand = (
  clang: string,
  profile: CompilationProfile.CompilationProfile,
  bitcodePath: string,
  objectPath: string,
): PlannedCommand =>
  Object.freeze({
    _tag: 'PlannedCommand',
    target: profile.target,
    command: clang,
    arguments: Object.freeze([
      `--target=${profile.target.id}`,
      '-c',
      '-x',
      'ir',
      bitcodePath,
      profile.relocation === 'pic' ? '-fPIC' : '-fno-pic',
      ...compilationArguments(profile),
      '-o',
      objectPath,
    ]),
  })

export type NativeArtifactKind = Exclude<ArtifactKind.ArtifactKind, 'WebAssemblyModule'>

/** A target or artifact combination that cannot preserve the requested typed link semantics. */
export interface UnsupportedNativePlan {
  readonly _tag: 'UnsupportedNativePlan'
  readonly artifactKind: NativeArtifactKind
  readonly target: Target.Target
  readonly input: NativeLinkInput.NativeLinkInput
  readonly reason:
    | 'FrameworkTarget'
    | 'LinkerScriptTarget'
    | 'StaticLibraryTarget'
    | 'StaticArchiveInput'
    | 'PathNotAbsolute'
}

export type NativePlan = PlannedCommand | UnsupportedNativePlan

const unsupported = (
  artifactKind: NativeArtifactKind,
  target: Target.Target,
  input: NativeLinkInput.NativeLinkInput,
  reason: UnsupportedNativePlan['reason'],
): UnsupportedNativePlan =>
  Object.freeze({ _tag: 'UnsupportedNativePlan', artifactKind, target, input, reason })

const clangInputArguments = (
  artifactKind: 'NativeExecutable' | 'NativeSharedLibrary',
  target: Target.Target,
  input: NativeLinkInput.NativeLinkInput,
): ReadonlyArray<string> | UnsupportedNativePlan => {
  switch (input._tag) {
    case 'Object':
    case 'StaticArchive':
      return [input.path]
    case 'LinkerScript':
      return target.operatingSystem === 'linux'
        ? ['-Xlinker', '-T', '-Xlinker', input.path]
        : unsupported(artifactKind, target, input, 'LinkerScriptTarget')
    case 'SearchPath':
      return [`-L${input.path}`]
    case 'Framework':
      return target.id === 'aarch64-apple-darwin'
        ? ['-framework', input.name]
        : unsupported(artifactKind, target, input, 'FrameworkTarget')
    case 'Library':
      if (input.mode === 'Dynamic') return [`-l${input.name}`]
      return target.id === 'aarch64-apple-darwin'
        ? unsupported(artifactKind, target, input, 'StaticLibraryTarget')
        : ['-Wl,-Bstatic', `-l${input.name}`, '-Wl,-Bdynamic']
  }
}

const fileName = (path: string): string => path.split(/[/\\]/).at(-1) ?? path

/** Plans one native executable/shared link or deterministic static archive. */
export const nativeCommand = (
  tools: { readonly clang: string; readonly llvmAr: string },
  artifactKind: NativeArtifactKind,
  target: Target.Target,
  generatedObjects: ReadonlyArray<string>,
  inputs: ReadonlyArray<NativeLinkInput.NativeLinkInput>,
  destination: string,
  entry: CompilationProfile.Selection = { kind: 'default' },
): NativePlan => {
  for (const input of inputs) {
    if (!NativeLinkInput.hasAbsolutePath(input))
      return unsupported(artifactKind, target, input, 'PathNotAbsolute')
  }
  if (artifactKind === 'NativeObject') {
    for (const input of inputs)
      if (input._tag !== 'Object' && input._tag !== 'StaticArchive')
        return unsupported(artifactKind, target, input, 'StaticArchiveInput')
    return Object.freeze({
      _tag: 'PlannedCommand',
      target,
      command: tools.clang,
      arguments: Object.freeze([
        `--target=${target.id}`,
        '-r',
        '-nostdlib',
        ...generatedObjects,
        ...inputs.flatMap((input) =>
          input._tag === 'Object' || input._tag === 'StaticArchive' ? [input.path] : [],
        ),
        '-o',
        destination,
      ]),
    })
  }
  if (artifactKind === 'NativeStaticLibrary') {
    const members = [...generatedObjects]
    for (const input of inputs) {
      if (input._tag !== 'Object')
        return unsupported(artifactKind, target, input, 'StaticArchiveInput')
      members.push(input.path)
    }
    return Object.freeze({
      _tag: 'PlannedCommand',
      target,
      command: tools.llvmAr,
      arguments: Object.freeze(['rcsD', destination, ...members]),
    })
  }
  let sharedArguments: ReadonlyArray<string> = []
  if (artifactKind === 'NativeSharedLibrary')
    sharedArguments =
      target.id === 'aarch64-apple-darwin'
        ? ['-dynamiclib', `-Wl,-install_name,@rpath/${fileName(destination)}`]
        : ['-shared']
  let entryArguments: ReadonlyArray<string> = []
  if (artifactKind === 'NativeExecutable' && entry.kind === 'named')
    entryArguments = ['-nostartfiles', '-Xlinker', '-e', '-Xlinker', entry.name]
  else if (artifactKind === 'NativeExecutable' && entry.kind === 'none')
    entryArguments = ['-nostartfiles', '-Wl,-e,0']
  const arguments_: Array<string> = [
    `--target=${target.id}`,
    ...sharedArguments,
    ...entryArguments,
    ...generatedObjects,
  ]
  for (const input of inputs) {
    const encoded = clangInputArguments(artifactKind, target, input)
    if ('_tag' in encoded) return encoded
    arguments_.push(...encoded)
  }
  arguments_.push('-o', destination)
  return Object.freeze({
    _tag: 'PlannedCommand',
    target,
    command: tools.clang,
    arguments: Object.freeze(arguments_),
  })
}

/** Plans the pinned Clang invocation that compiles one runtime translation unit. */
export const cObjectCommand = (
  clang: string,
  target: Target.Target,
  sourcePath: string,
  objectPath: string,
): PlannedCommand =>
  Object.freeze({
    _tag: 'PlannedCommand',
    target,
    command: clang,
    arguments: Object.freeze([
      `--target=${target.id}`,
      '-c',
      '-x',
      'c',
      sourcePath,
      '-O2',
      '-fPIC',
      '-fvisibility=hidden',
      '-o',
      objectPath,
    ]),
  })

/** Plans standalone LLVM-bitcode to WebAssembly finalization through pinned Clang. */
export const wasmCommand = (
  clang: string,
  profile: CompilationProfile.CompilationProfile,
  bitcodePath: string,
  runtimeObjectPath: string,
  destination: string,
): PlannedCommand =>
  Object.freeze({
    _tag: 'PlannedCommand',
    target: profile.target,
    command: clang,
    arguments: Object.freeze([
      `--target=${profile.target.id}`,
      '-nostdlib',
      '-x',
      'ir',
      bitcodePath,
      '-x',
      'none',
      runtimeObjectPath,
      ...compilationArguments(profile),
      '-Wl,--no-entry',
      '-Wl,--export=silk_main',
      '-o',
      destination,
    ]),
  })

/**
 * The minimal C runtime boundary: a private, compiler-versioned scalar ABI reaching a closed
 * native entry. The executable source's `main` receives the process command line, holds it for the host-input runtime,
 * and returns `silk_main`'s `i32` result as the process exit status. Silk `main` itself keeps its
 * zero-parameter shape: arguments reach a program through a service, never through the entry
 * signature. Not user-facing FFI; issue 07 owns the ABI's growth.
 */
const standardStreamsShimSource = `#include <errno.h>
#include <stddef.h>
#include <unistd.h>

int silk_standard_stream_write_v1(int destination, const unsigned char *bytes, size_t length) {
  const int descriptor = destination == 0 ? 1 : 2;
  size_t offset = 0;
  while (offset < length) {
    const ssize_t written = write(descriptor, bytes + offset, length - offset);
    if (written < 0 && errno == EINTR) continue;
    if (written <= 0) return 1;
    offset += (size_t)written;
  }
  return 0;
}
`

const commandLineStateSource = `int silk_host_argc_v1 = 0;
char **silk_host_argv_v1 = 0;
`

/*
 * This is the sole owner of feature-test macros for the generated translation unit. Keep it
 * immediately after the leading comment: libc examines these macros while processing its first
 * header, so defining them in a later capability fragment is too late when fragments are mixed.
 */
const translationUnitPreamble = `#if defined(__APPLE__)
#ifndef _DARWIN_C_SOURCE
#define _DARWIN_C_SOURCE 1
#endif
#elif defined(__linux__)
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#endif
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
`

/** Renders text as a C string literal; octal escapes keep every byte outside printable ASCII exact. */
export const cString = (text: string): string => {
  let rendered = '"'
  for (const byte of new TextEncoder().encode(text)) {
    if (byte === 0x22 || byte === 0x5c || byte === 0x3f) {
      rendered += `\\${String.fromCharCode(byte)}`
    } else if (byte >= 0x20 && byte < 0x7f) {
      rendered += String.fromCharCode(byte)
    } else {
      rendered += `\\${byte.toString(8).padStart(3, '0')}`
    }
  }
  return `${rendered}"`
}

const cStringTable = (name: string, entries: ReadonlyArray<string>): string =>
  `static const char *const ${name}[] = { ${entries.length === 0 ? '0' : entries.map(cString).join(', ')} };`

/* Best-effort stderr writer shared by the unhandled-error and fatal-trap reports. */
const reportSupportSource = `#include <stddef.h>
#include <string.h>
#include <unistd.h>

static int silk_write_all(const unsigned char *bytes, size_t length) {
  size_t offset = 0;
  while (offset < length) {
    const ssize_t written = write(2, bytes + offset, length - offset);
    if (written <= 0) return 0;
    offset += (size_t)written;
  }
  return 1;
}

static int silk_write_text(const char *text) {
  return silk_write_all((const unsigned char *)text, strlen(text));
}
`

const trapReportSource = (report: Termination.Report): string => `
${cStringTable(
  'silk_trap_reasons',
  report.trapSites.map((site) => site.reason),
)}
${cStringTable(
  'silk_trap_origins',
  report.trapSites.map((site) => site.origin),
)}

void ${NativeTermination.trapReportSymbol}(int site) {
  if (site <= 0 || site > ${report.trapSites.length}) return;
  silk_write_text("fatal trap: ");
  silk_write_text(silk_trap_reasons[site - 1]);
  silk_write_text("\\n  at ");
  silk_write_text(silk_trap_origins[site - 1]);
  silk_write_text("\\n");
}
`

const failurePathSource = (report: Termination.Report): string => `
extern int ${NativeTermination.failureSiteSymbol};
extern int ${NativeTermination.failureDepthSymbol};
extern int ${NativeTermination.failurePathSymbol}[${NativeTermination.pathCapacity}];
extern int ${NativeTermination.causeSiteSymbol};
extern int ${NativeTermination.causeDepthSymbol};
extern int ${NativeTermination.causePathSymbol}[${NativeTermination.pathCapacity}];
${cStringTable('silk_frame_labels', report.frames)}
${cStringTable(
  'silk_failure_origins',
  report.failureSites.map((site) => site.origin),
)}
${cStringTable(
  'silk_failure_identities',
  report.failureSites.map((site) => site.identity),
)}

static int silk_write_path(int site, int depth, const int *path) {
  int ok = 1;
  if (site <= 0 || site > ${report.failureSites.length}) return ok;
  ok &= silk_write_text("  at ");
  ok &= silk_write_text(silk_failure_origins[site - 1]);
  ok &= silk_write_text("\\n");
  if (depth > ${NativeTermination.pathCapacity}) depth = ${NativeTermination.pathCapacity};
  for (int index = 1; index < depth; index += 1) {
    const int frame = path[index];
    if (frame < 0 || frame >= ${report.frames.length}) continue;
    ok &= silk_write_text("  at ");
    ok &= silk_write_text(silk_frame_labels[frame]);
    ok &= silk_write_text("\\n");
  }
  return ok;
}
`

const runtimeSourceFor = (
  nativeRuntimeSymbols: ReadonlyArray<string> = Object.freeze([]),
  forceStandardStreams = false,
): string => {
  const osRuntime = OsRuntime.source(nativeRuntimeSymbols)
  const coroutineRuntime = CoroutineRuntime.source(nativeRuntimeSymbols)
  const needsCommandLine = nativeRuntimeSymbols.some(
    (symbol) =>
      symbol === 'silk_os_host_argument_count_v1' || symbol === 'silk_os_host_argument_v1',
  )
  const needsStandardStreams =
    forceStandardStreams || nativeRuntimeSymbols.includes('silk_standard_stream_write_v1')
  const commandLine = needsCommandLine ? commandLineStateSource : ''
  return `/* silk-effect native runtime — private, compiler-versioned. */
${translationUnitPreamble}
${needsStandardStreams ? standardStreamsShimSource : ''}
${commandLine}
${osRuntime}
${coroutineRuntime}
`
}

/** Generates only the hidden runtime definitions required by a native library. */
export const runtimeSource = (
  nativeRuntimeSymbols: ReadonlyArray<string> = Object.freeze([]),
): string => runtimeSourceFor(nativeRuntimeSymbols)

/** Generates the private process adapter and required runtime definitions for an executable. */
export const executableSource = (
  termination: Backend.Termination,
  nativeRuntimeSymbols: ReadonlyArray<string> = Object.freeze([]),
): string => {
  const runtime = runtimeSourceFor(nativeRuntimeSymbols, termination.failures.length > 0)
  const needsCommandLine = nativeRuntimeSymbols.some(
    (symbol) =>
      symbol === 'silk_os_host_argument_count_v1' || symbol === 'silk_os_host_argument_v1',
  )
  const mainParameters = needsCommandLine ? 'int argc, char **argv' : 'void'
  const initializeCommandLine = needsCommandLine
    ? '  silk_host_argc_v1 = argc;\n  silk_host_argv_v1 = argv;\n'
    : ''
  const { report } = termination
  const reportsFailures = termination.failures.length > 0
  const reportsTraps = report.trapSites.length > 0
  const reportsPaths = reportsFailures && report.failureSites.length > 0
  const prelude = `${runtime}
extern int silk_main(void);
${reportsFailures || reportsTraps ? reportSupportSource : ''}
${reportsTraps ? trapReportSource(report) : ''}
${reportsPaths ? failurePathSource(report) : ''}`
  if (!reportsFailures)
    return `${prelude}
int main(${mainParameters}) {
${initializeCommandLine}
  return silk_main();
}
`
  const cases = termination.failures.map(
    (failure) =>
      `    case ${failure.tag}:\n      identity = ${cString(failure.identity)};\n      break;`,
  )
  const paths = reportsPaths
    ? `  ok &= silk_write_path(${NativeTermination.failureSiteSymbol}, ${NativeTermination.failureDepthSymbol}, ${NativeTermination.failurePathSymbol});
  if (${NativeTermination.causeSiteSymbol} > 0 && ${NativeTermination.causeSiteSymbol} <= ${report.failureSites.length}) {
    ok &= silk_write_text("while handling: ");
    ok &= silk_write_text(silk_failure_identities[${NativeTermination.causeSiteSymbol} - 1]);
    ok &= silk_write_text("\\n");
    ok &= silk_write_path(${NativeTermination.causeSiteSymbol}, ${NativeTermination.causeDepthSymbol}, ${NativeTermination.causePathSymbol});
  }
`
    : ''
  return `${prelude}
extern int ${NativeTermination.failureTagSymbol};

int main(${mainParameters}) {
${initializeCommandLine}
  if (silk_main() == 0) return 0;
  const char *identity = 0;
  switch (${NativeTermination.failureTagSymbol}) {
${cases.join('\n')}
    default:
      return 2;
  }
  int ok = 1;
  ok &= silk_write_text("unhandled error: ");
  ok &= silk_write_text(identity);
  ok &= silk_write_text("\\n");
${paths}  return ok ? 1 : 2;
}
`
}
