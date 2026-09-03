import * as Target from '@silklang/compiler/Target'
import type * as ToolchainPlan from '@silklang/compiler/ToolchainPlan'
import * as Data from 'effect/Data'
import * as Result from 'effect/Result'
import { Flag } from 'effect/unstable/cli'

export const profiles = ['debug', 'release', 'release-with-debug'] as const
const targetIds = ['host', ...Target.all.map((candidate) => candidate.id)]
const backendIds = ['llvm'] as const

export const manifestPath = Flag.string('manifest-path').pipe(
  Flag.withDescription('Path to a Silk project manifest. Disables upward discovery.'),
  Flag.optional,
)

export const backend = Flag.choice('backend', backendIds).pipe(
  Flag.withDescription('Backend id. Replaces the project manifest backend when supplied.'),
  Flag.optional,
)

export const targets = Flag.choice('target', targetIds).pipe(
  Flag.withDescription('Compilation target selector. Repeat to build more than one target.'),
  Flag.atLeast(0),
)

export const profile = Flag.choice('profile', profiles).pipe(
  Flag.withDescription('Compilation profile.'),
  Flag.optional,
)

export const release = Flag.boolean('release').pipe(
  Flag.withDescription('Build with the release profile.'),
  Flag.withDefault(false),
)

export const watch = Flag.boolean('watch').pipe(
  Flag.withDescription('Run again after every change to a project source file.'),
  Flag.withDefault(false),
)

export interface Input {
  readonly manifestPath?: string
  readonly backend?: string
  readonly targets?: ReadonlyArray<string>
  readonly profile?: ToolchainPlan.OptimizationProfile
  readonly release: boolean
}

export interface ProjectOptions {
  readonly manifestPath?: string
  readonly backend?: string
  readonly targets?: ReadonlyArray<string>
  readonly profile: ToolchainPlan.OptimizationProfile
}

/** Project command flags contradict one another. */
export class ProjectOptionsError extends Data.TaggedError('ProjectOptionsError')<{
  readonly operation: 'ProjectOptions.resolve'
  readonly message: string
  readonly reason: { readonly _tag: 'ConflictingProfile'; readonly profile: string }
}> {}

/** Resolves shared project flags before project discovery or compilation begins. */
export const resolve = (input: Input): Result.Result<ProjectOptions, ProjectOptionsError> => {
  if (input.release && input.profile !== undefined && input.profile !== 'release') {
    return Result.fail(
      new ProjectOptionsError({
        operation: 'ProjectOptions.resolve',
        message: `--release conflicts with --profile ${input.profile}`,
        reason: { _tag: 'ConflictingProfile', profile: input.profile },
      }),
    )
  }
  return Result.succeed(
    Object.freeze({
      ...(input.manifestPath === undefined ? {} : { manifestPath: input.manifestPath }),
      ...(input.backend === undefined ? {} : { backend: input.backend }),
      ...(input.targets === undefined || input.targets.length === 0
        ? {}
        : { targets: input.targets }),
      profile: input.release ? ('release' as const) : (input.profile ?? 'debug'),
    }),
  )
}
