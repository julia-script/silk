/**
 * Pure planning for the pinned native toolchain: fixed optimization profiles, the exact
 * structured commands the orchestration issues (never a shell string), and the minimal C runtime
 * shim source. Browser-safe by construction — execution lives in `NativeToolchain`.
 */

import type * as Backend from './Backend.js'
import * as CoroutineRuntime from './CoroutineRuntime.js'
import * as OsRuntime from './OsRuntime.js'
import type * as Target from './Target.js'

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

const profileArguments = (profile: OptimizationProfile): ReadonlyArray<string> => {
  switch (profile) {
    case 'debug':
      return ['-O0', '-g']
    case 'release':
      return ['-O2']
    case 'release-with-debug':
      return ['-O2', '-g']
  }
}

/** Plans the pinned Clang `-c` invocation that turns bitcode into a target object. */
export const objectCommand = (
  clang: string,
  target: Target.Target,
  profile: OptimizationProfile,
  bitcodePath: string,
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
      'ir',
      bitcodePath,
      ...profileArguments(profile),
      '-o',
      objectPath,
    ]),
  })

/** Plans the pinned Clang driver invocation that links objects into an executable. */
export const linkCommand = (
  clang: string,
  target: Target.Target,
  objects: ReadonlyArray<string>,
  libraries: ReadonlyArray<string>,
  destination: string,
): PlannedCommand =>
  Object.freeze({
    _tag: 'PlannedCommand',
    target,
    command: clang,
    arguments: Object.freeze([
      `--target=${target.id}`,
      ...objects,
      ...libraries.map((library) => `-l${library}`),
      '-o',
      destination,
    ]),
  })

/** Plans the pinned Clang invocation that compiles the runtime shim. */
export const shimCommand = (
  clang: string,
  target: Target.Target,
  shimSourcePath: string,
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
      shimSourcePath,
      '-O2',
      '-o',
      objectPath,
    ]),
  })

/** Plans standalone LLVM-bitcode to WebAssembly finalization through pinned Clang. */
export const wasmCommand = (
  clang: string,
  target: Target.Target,
  profile: OptimizationProfile,
  bitcodePath: string,
  destination: string,
): PlannedCommand =>
  Object.freeze({
    _tag: 'PlannedCommand',
    target,
    command: clang,
    arguments: Object.freeze([
      `--target=${target.id}`,
      '-nostdlib',
      '-x',
      'ir',
      bitcodePath,
      ...profileArguments(profile),
      '-Wl,--no-entry',
      '-Wl,--export=silk_main',
      '-o',
      destination,
    ]),
  })

/**
 * The minimal C runtime shim: a private, compiler-versioned scalar ABI reaching a closed native
 * entry. The shim's `main` receives the process command line, holds it for the host-input runtime,
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

const failureBytes = (identity: string): ReadonlyArray<number> =>
  Array.from(new TextEncoder().encode(`Error: ${identity}\n`))

/** Generates the private native adapter for the artifact's exact termination contract. */
export const shimSource = (
  termination: Backend.Termination,
  nativeRuntimeSymbols: ReadonlyArray<string> = Object.freeze([]),
): string => {
  const osRuntime = OsRuntime.source(nativeRuntimeSymbols)
  const coroutineRuntime = CoroutineRuntime.source(nativeRuntimeSymbols)
  const needsCommandLine = nativeRuntimeSymbols.some(
    (symbol) =>
      symbol === 'silk_os_host_argument_count_v1' || symbol === 'silk_os_host_argument_v1',
  )
  const needsStandardStreams = nativeRuntimeSymbols.includes('silk_standard_stream_write_v1')
  const commandLine = needsCommandLine ? commandLineStateSource : ''
  const mainParameters = needsCommandLine ? 'int argc, char **argv' : 'void'
  const initializeCommandLine = needsCommandLine
    ? '  silk_host_argc_v1 = argc;\n  silk_host_argv_v1 = argv;\n'
    : ''
  if (termination.failures.length === 0)
    return `/* silk-effect bootstrap runtime shim — private, compiler-versioned. */
${translationUnitPreamble}
${needsStandardStreams ? standardStreamsShimSource : ''}
${commandLine}
${osRuntime}
${coroutineRuntime}
extern int silk_main(void);

int main(${mainParameters}) {
${initializeCommandLine}
  return silk_main();
}
`
  const declarations = termination.failures.map(
    (failure) =>
      `static const unsigned char silk_failure_${failure.tag}[] = { ${failureBytes(failure.identity).join(', ')} };`,
  )
  const cases = termination.failures.map(
    (failure) =>
      `    case ${failure.tag}:\n      return silk_write_all(silk_failure_${failure.tag}, sizeof(silk_failure_${failure.tag})) ? 1 : 2;`,
  )
  return `/* silk-effect bootstrap runtime shim — private, compiler-versioned. */
${translationUnitPreamble}
${standardStreamsShimSource}
${commandLine}
${osRuntime}
${coroutineRuntime}

extern int silk_main(void);
${declarations.join('\n')}

static int silk_write_all(const unsigned char *bytes, size_t length) {
  size_t offset = 0;
  while (offset < length) {
    const ssize_t written = write(2, bytes + offset, length - offset);
    if (written <= 0) return 0;
    offset += (size_t)written;
  }
  return 1;
}

int main(${mainParameters}) {
${initializeCommandLine}
  const int tag = silk_main();
  if (tag == 0) return 0;
  switch (tag) {
${cases.join('\n')}
    default:
      return 2;
  }
}
`
}
