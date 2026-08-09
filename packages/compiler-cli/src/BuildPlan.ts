import { join } from 'node:path'
import type * as Backend from '@silk-effect/compiler/Backend'
import * as BackendRegistry from '@silk-effect/compiler/BackendRegistry'
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
  readonly backend: Backend.Backend
  readonly target: Target.Target
  readonly profile: ToolchainPlan.OptimizationProfile
  readonly destination: string
  readonly toolchain: NativeToolchain.Toolchain
}

export type BuildPlanErrorReason =
  | { readonly _tag: 'InvalidPackageName'; readonly name: string }
  | {
      readonly _tag: 'IncompatibleBackendTarget'
      readonly error: BackendRegistry.BackendRegistryError
    }
  | { readonly _tag: 'NonNativeRunBackend'; readonly backend: Backend.Id }
  | { readonly _tag: 'ForeignRunTarget'; readonly target: Target.Id; readonly host: Target.Id }
  | { readonly _tag: 'HostUnavailable'; readonly error: Target.TargetError }

/** Project backend/target/profile selection cannot produce the requested workflow. */
export class BuildPlanError extends Data.TaggedError('BuildPlanError')<{
  readonly operation: 'BuildPlan.make'
  readonly message: string
  readonly reason: BuildPlanErrorReason
}> {}

export interface Options {
  readonly backend: Backend.Backend
  readonly target: Target.Target
  readonly profile: ToolchainPlan.OptimizationProfile
  readonly purpose?: Purpose
  readonly clang?: string
}

/** Creates one immutable, already-resolved build plan with a backend-qualified destination. */
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
  const compatible = BackendRegistry.requireTarget(options.backend, options.target)
  if (Result.isFailure(compatible)) {
    return Result.fail(
      new BuildPlanError({
        operation: 'BuildPlan.make',
        message: compatible.failure.message,
        reason: { _tag: 'IncompatibleBackendTarget', error: compatible.failure },
      }),
    )
  }
  if (options.purpose === 'run') {
    if (options.backend.id !== 'llvm' || !Target.isNative(options.target)) {
      return Result.fail(
        new BuildPlanError({
          operation: 'BuildPlan.make',
          message: `Backend ${options.backend.id} cannot produce a host executable`,
          reason: { _tag: 'NonNativeRunBackend', backend: options.backend.id },
        }),
      )
    }
    const host = Target.select(undefined)
    if (host._tag === 'Unavailable') {
      return Result.fail(
        new BuildPlanError({
          operation: 'BuildPlan.make',
          message: host.error.message,
          reason: { _tag: 'HostUnavailable', error: host.error },
        }),
      )
    }
    if (options.target.id !== host.target.id) {
      return Result.fail(
        new BuildPlanError({
          operation: 'BuildPlan.make',
          message: `Cannot run target ${options.target.id} on host ${host.target.id}`,
          reason: {
            _tag: 'ForeignRunTarget',
            target: options.target.id,
            host: host.target.id,
          },
        }),
      )
    }
  }

  const fileName = options.target.kind === 'WebAssembly' ? `${project.name}.wasm` : project.name
  return Result.succeed(
    Object.freeze({
      _tag: 'BuildPlan' as const,
      project,
      backend: options.backend,
      target: options.target,
      profile: options.profile,
      destination: join(
        project.build.outputDirectory,
        options.backend.id,
        options.target.id,
        options.profile,
        fileName,
      ),
      toolchain: Object.freeze({ _tag: 'Toolchain' as const, clang: options.clang ?? 'clang' }),
    }),
  )
}
