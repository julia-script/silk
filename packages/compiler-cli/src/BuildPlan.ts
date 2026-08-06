import { join } from 'node:path'
import type * as NativeToolchain from '@silk-effect/compiler/NativeToolchain'
import * as Target from '@silk-effect/compiler/Target'
import type * as ToolchainPlan from '@silk-effect/compiler/ToolchainPlan'
import * as Data from 'effect/Data'
import * as Result from 'effect/Result'
import * as Project from './Project.js'

export type Purpose = 'build' | 'run'

export interface BuildPlan {
  readonly _tag: 'BuildPlan'
  readonly project: Project.Project
  readonly target: Target.Target
  readonly profile: ToolchainPlan.OptimizationProfile
  readonly destination: string
  readonly toolchain: NativeToolchain.Toolchain
}

export type BuildPlanErrorReason =
  | { readonly _tag: 'InvalidPackageName'; readonly name: string }
  | { readonly _tag: 'TargetUnavailable'; readonly error: Target.TargetError }
  | { readonly _tag: 'NonNativeTarget'; readonly target: Target.Id }
  | { readonly _tag: 'ForeignRunTarget'; readonly target: Target.Id; readonly host: Target.Id }

/** Project target/profile selection cannot produce the requested native workflow. */
export class BuildPlanError extends Data.TaggedError('BuildPlanError')<{
  readonly operation: 'BuildPlan.make'
  readonly message: string
  readonly reason: BuildPlanErrorReason
}> {}

export interface Options {
  readonly target?: string
  readonly profile: ToolchainPlan.OptimizationProfile
  readonly purpose?: Purpose
  readonly clang?: string
}

/** Resolves target policy and deterministic artifact layout for one project workflow. */
export const make = (
  project: Project.Project,
  options: Options,
): Result.Result<BuildPlan, BuildPlanError> => {
  if (!Project.isPackageName(project.name)) {
    return Result.fail(
      new BuildPlanError({
        operation: 'BuildPlan.make',
        message: `Project package name ${project.name} is not portable`,
        reason: { _tag: 'InvalidPackageName', name: project.name },
      }),
    )
  }
  const selected = Target.select(options.target)
  if (selected._tag === 'Unavailable') {
    return Result.fail(
      new BuildPlanError({
        operation: 'BuildPlan.make',
        message: selected.error.message,
        reason: { _tag: 'TargetUnavailable', error: selected.error },
      }),
    )
  }
  if (!Target.isNative(selected.target)) {
    return Result.fail(
      new BuildPlanError({
        operation: 'BuildPlan.make',
        message: `Project executables require a native target; received ${selected.target.id}`,
        reason: { _tag: 'NonNativeTarget', target: selected.target.id },
      }),
    )
  }
  if (options.purpose === 'run') {
    const host = Target.select(undefined)
    if (host._tag === 'Unavailable') {
      return Result.fail(
        new BuildPlanError({
          operation: 'BuildPlan.make',
          message: host.error.message,
          reason: { _tag: 'TargetUnavailable', error: host.error },
        }),
      )
    }
    if (selected.target.id !== host.target.id) {
      return Result.fail(
        new BuildPlanError({
          operation: 'BuildPlan.make',
          message: `Cannot run target ${selected.target.id} on host ${host.target.id}`,
          reason: {
            _tag: 'ForeignRunTarget',
            target: selected.target.id,
            host: host.target.id,
          },
        }),
      )
    }
  }

  return Result.succeed(
    Object.freeze({
      _tag: 'BuildPlan' as const,
      project,
      target: selected.target,
      profile: options.profile,
      destination: join(
        project.directory,
        '.silk',
        'build',
        selected.target.id,
        options.profile,
        project.name,
      ),
      toolchain: Object.freeze({ _tag: 'Toolchain' as const, clang: options.clang ?? 'clang' }),
    }),
  )
}
