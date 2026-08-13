import { spawnSync } from 'node:child_process'

/**
 * Locating the LLVM command line tools a test cross-checks itself against.
 *
 * `opt`, `llvm-as`, and `llvm-dis` ship in LLVM's tools package rather than alongside `clang`, and
 * the Linux distributions install them under a versioned prefix without symlinking them into
 * `/usr/bin`. A machine that compiles Silk programs therefore does not necessarily have any of
 * them on `PATH` — including this repository's CI runner, which has `clang` and nothing else.
 * Look where the distributions put them before concluding a tool is absent.
 */

const searchPaths: ReadonlyArray<string> = [
  '/opt/homebrew/opt/llvm/bin',
  '/usr/local/opt/llvm/bin',
  ...['21', '20', '19', '18', '17', '16'].map((version) => `/usr/lib/llvm-${version}/bin`),
]

/** The override for a tool: `opt` reads `SILK_TEST_OPT`, `llvm-as` reads `SILK_TEST_LLVM_AS`. */
export const overrideVariable = (tool: string): string =>
  `SILK_TEST_${tool.toUpperCase().replaceAll('-', '_')}`

/**
 * The command that runs `tool`, or `undefined` when no candidate answers `--version`.
 *
 * An explicit override wins, then `PATH`, then the distribution prefixes newest first.
 */
export const findLlvmTool = (tool: string): string | undefined =>
  [
    process.env[overrideVariable(tool)],
    tool,
    ...searchPaths.map((directory) => `${directory}/${tool}`),
  ].find(
    (candidate) =>
      candidate !== undefined &&
      spawnSync(candidate, ['--version'], { encoding: 'utf8' }).status === 0,
  )

export interface LlvmToolchain {
  /** The command for one of the requested tools. */
  readonly command: (tool: string) => string
  /**
   * Whether the suite has to stop, called as the first line of each test that needs the toolchain.
   *
   * A cross-check that turns itself off when its reference implementation is missing reports
   * success in exactly the situation where it verified nothing, which is the failure mode these
   * suites exist to remove — a round-trip test that skips is a golden test comparing text against
   * itself. So a contributor without LLVM's tools still gets a useful suite, while CI gets a red
   * build, answered by installing LLVM rather than by a quieter test.
   */
  readonly unavailable: () => boolean
}

/**
 * Resolves every tool a suite needs, reporting once which commands it will use or what is missing.
 *
 * `purpose` names the check in both messages, so a red CI build says what stopped running.
 */
export const llvmToolchain = (tools: ReadonlyArray<string>, purpose: string): LlvmToolchain => {
  const resolved = new Map(tools.map((tool) => [tool, findLlvmTool(tool)] as const))
  const missing = tools.filter((tool) => resolved.get(tool) === undefined)

  if (missing.length === 0) {
    console.log(`${purpose} is using ${tools.map((tool) => resolved.get(tool)).join(', ')}`)
  } else if (process.env.CI === undefined) {
    console.log(`${missing.join(', ')} was not found; skipping ${purpose}`)
  }

  return {
    command: (tool) => {
      const command = resolved.get(tool)
      if (command === undefined) {
        throw new Error(`${tool} was not among the tools ${purpose} asked for`)
      }
      return command
    },
    unavailable: () => {
      if (missing.length === 0) return false
      if (process.env.CI !== undefined) {
        throw new Error(
          `${missing.join(', ')} not found in CI, so ${purpose} cannot run. Install LLVM in the workflow, or point ${missing.map(overrideVariable).join(', ')} at the binar${missing.length === 1 ? 'y' : 'ies'}.`,
        )
      }
      return true
    },
  }
}
