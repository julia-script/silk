/**
 * Pure planning for the pinned native toolchain: fixed optimization profiles, the exact
 * structured commands the orchestration issues (never a shell string), and the minimal C runtime
 * shim source. Browser-safe by construction — execution lives in `NativeToolchain`.
 */
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
      `--target=${target.triple}`,
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
      `--target=${target.triple}`,
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
      `--target=${target.triple}`,
      '-c',
      '-x',
      'c',
      shimSourcePath,
      '-O2',
      '-o',
      objectPath,
    ]),
  })

/**
 * The minimal C runtime shim: a private, compiler-versioned scalar ABI reaching a closed native
 * entry. The shim's `main` returns `silk_main`'s `I32` result as the process exit status. Not
 * user-facing FFI; issue 07 owns the ABI's growth.
 */
export const shimSource = `/* silk-effect bootstrap runtime shim — private, compiler-versioned. */
extern int silk_main(void);

int main(void) {
  return silk_main();
}
`
