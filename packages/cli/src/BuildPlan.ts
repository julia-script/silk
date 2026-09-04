import { join } from 'node:path'
import * as ArtifactKind from '@silklang/compiler/ArtifactKind'
import * as NativeToolchain from '@silklang/compiler/NativeToolchain'
import type * as NativeLinkInput from '@silklang/compiler/NativeLinkInput'
import * as Project from '@silklang/compiler/Project'
import * as Target from '@silklang/compiler/Target'
import * as ToolchainPlan from '@silklang/compiler/ToolchainPlan'
import * as Data from 'effect/Data'
import * as Result from 'effect/Result'

export type Purpose = 'build' | 'run'

export interface BuildPlan {
  readonly _tag: 'BuildPlan'
  readonly project: Project.Project
  readonly target: Target.Target
  readonly artifactKind: ArtifactKind.ArtifactKind
  readonly profile: ToolchainPlan.OptimizationProfile
  readonly destination: string
  readonly toolchain: NativeToolchain.Toolchain
  /** The manifest's ordered native inputs for a native target; empty for WebAssembly. */
  readonly nativeLinkInputs: ReadonlyArray<NativeLinkInput.NativeLinkInput>
}

export type BuildPlanErrorReason =
  | { readonly _tag: 'InvalidPackageName'; readonly name: string }
  | { readonly _tag: 'NonExecutableRunArtifact'; readonly artifactKind: ArtifactKind.ArtifactKind }
  | {
      readonly _tag: 'IncompatibleArtifactTarget'
      readonly artifactKind: ArtifactKind.ArtifactKind
      readonly target: Target.Id
    }
  | { readonly _tag: 'NativeInputsForWebAssembly'; readonly target: Target.Id }
  | {
      readonly _tag: 'UnsupportedNativePlan'
      readonly plan: ToolchainPlan.UnsupportedNativePlan
    }
  | { readonly _tag: 'ForeignRunTarget'; readonly target: Target.Id; readonly host: Target.Id }
  | { readonly _tag: 'HostUnavailable'; readonly error: Target.TargetError }

/** Project target/profile selection cannot produce the requested workflow. */
export class BuildPlanError extends Data.TaggedError('BuildPlanError')<{
  readonly operation: 'BuildPlan.make'
  readonly message: string
  readonly reason: BuildPlanErrorReason
}> {}

export interface Options {
  readonly target: Target.Target
  readonly profile: ToolchainPlan.OptimizationProfile
  readonly purpose?: Purpose
  readonly clang?: string
  readonly llvmAr?: string
}

/** Creates one immutable, already-resolved LLVM build plan. */
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
  if (options.purpose === 'run') {
    if (project.build.artifact !== 'NativeExecutable') {
      return Result.fail(
        new BuildPlanError({
          operation: 'BuildPlan.make',
          message: `Cannot run ${ArtifactKind.manifestSpelling(project.build.artifact)} artifact`,
          reason: {
            _tag: 'NonExecutableRunArtifact',
            artifactKind: project.build.artifact,
          },
        }),
      )
    }
    const host = NativeToolchain.hostSelection()
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

  const artifactKind = Target.isNative(options.target)
    ? project.build.artifact
    : ArtifactKind.webAssemblyModule
  if (!Target.isNative(options.target) && project.build.artifact !== 'NativeExecutable') {
    return Result.fail(
      new BuildPlanError({
        operation: 'BuildPlan.make',
        message: `Artifact ${ArtifactKind.manifestSpelling(project.build.artifact)} is incompatible with target ${options.target.id}`,
        reason: {
          _tag: 'IncompatibleArtifactTarget',
          artifactKind: project.build.artifact,
          target: options.target.id,
        },
      }),
    )
  }
  if (!Target.isNative(options.target) && project.build.nativeLinkInputs.length > 0) {
    return Result.fail(
      new BuildPlanError({
        operation: 'BuildPlan.make',
        message: `Native link inputs are incompatible with target ${options.target.id}`,
        reason: { _tag: 'NativeInputsForWebAssembly', target: options.target.id },
      }),
    )
  }
  const destination = join(
    project.build.outputDirectory,
    'llvm',
    options.target.id,
    options.profile,
    ArtifactKind.fileName(artifactKind, project.name, options.target),
  )
  const toolchain = Object.freeze({
    _tag: 'Toolchain' as const,
    clang: options.clang ?? 'clang',
    llvmAr: options.llvmAr ?? 'llvm-ar',
  })
  if (Target.isNative(options.target)) {
    const nativePlan = ToolchainPlan.nativeCommand(
      toolchain,
      project.build.artifact,
      options.target,
      Object.freeze([]),
      project.build.nativeLinkInputs,
      destination,
    )
    if (nativePlan._tag === 'UnsupportedNativePlan') {
      return Result.fail(
        new BuildPlanError({
          operation: 'BuildPlan.make',
          message: `Native link input ${nativePlan.input._tag} is unsupported for ${ArtifactKind.manifestSpelling(artifactKind)} on ${options.target.id}`,
          reason: { _tag: 'UnsupportedNativePlan', plan: nativePlan },
        }),
      )
    }
  }
  return Result.succeed(
    Object.freeze({
      _tag: 'BuildPlan' as const,
      project,
      target: options.target,
      artifactKind,
      profile: options.profile,
      destination,
      toolchain,
      nativeLinkInputs: Target.isNative(options.target)
        ? project.build.nativeLinkInputs
        : Object.freeze([]),
    }),
  )
}
