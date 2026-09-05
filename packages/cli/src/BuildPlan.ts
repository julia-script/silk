import type * as ProjectProfile from '@silklang/compiler/ProjectProfile'
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
  readonly configuration?: ProjectProfile.Profile
  readonly optimization: ToolchainPlan.OptimizationProfile
  readonly destination: string
  readonly toolchain: NativeToolchain.Toolchain
  /** The manifest's ordered native inputs for a native target; empty for WebAssembly. */
  readonly nativeLinkInputs: ReadonlyArray<NativeLinkInput.NativeLinkInput>
}

export type BuildPlanErrorReason =
  | { readonly _tag: 'UnsupportedProfileArtifact'; readonly artifact: string }
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

/** Project target/optimization selection cannot produce the requested workflow. */
export class BuildPlanError extends Data.TaggedError('BuildPlanError')<{
  readonly operation: 'BuildPlan.make'
  readonly message: string
  readonly reason: BuildPlanErrorReason
}> {}

export interface Options {
  readonly target: Target.Target
  readonly configuration?: ProjectProfile.Profile
  readonly optimization: ToolchainPlan.OptimizationProfile
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
  let artifactKind = Target.isNative(options.target)
    ? project.build.artifact
    : ArtifactKind.webAssemblyModule
  const logicalArtifact = options.configuration?.input.artifact
  if (logicalArtifact !== undefined) {
    if (logicalArtifact === 'object')
      return Result.fail(
        new BuildPlanError({
          operation: 'BuildPlan.make',
          message: 'Object-only profiles require the compiler object-emission API',
          reason: { _tag: 'UnsupportedProfileArtifact', artifact: logicalArtifact },
        }),
      )
    if (Target.isNative(options.target)) {
      if (logicalArtifact === 'executable') artifactKind = 'NativeExecutable'
      else if (logicalArtifact === 'static-archive') artifactKind = 'NativeStaticLibrary'
      else artifactKind = 'NativeSharedLibrary'
    }
  }
  if (options.purpose === 'run') {
    if (artifactKind !== 'NativeExecutable') {
      return Result.fail(
        new BuildPlanError({
          operation: 'BuildPlan.make',
          message: `Cannot run ${ArtifactKind.manifestSpelling(artifactKind)} artifact`,
          reason: {
            _tag: 'NonExecutableRunArtifact',
            artifactKind,
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

  if (
    !Target.isNative(options.target) &&
    logicalArtifact === undefined &&
    project.build.artifact !== 'NativeExecutable'
  ) {
    return Result.fail(
      new BuildPlanError({
        operation: 'BuildPlan.make',
        message: `Artifact ${ArtifactKind.manifestSpelling(artifactKind)} is incompatible with target ${options.target.id}`,
        reason: {
          _tag: 'IncompatibleArtifactTarget',
          artifactKind,
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
    options.optimization,
    ArtifactKind.fileName(artifactKind, project.name, options.target),
  )
  const toolchain = Object.freeze({
    _tag: 'Toolchain' as const,
    clang: options.clang ?? 'clang',
    llvmAr: options.llvmAr ?? 'llvm-ar',
  })
  if (Target.isNative(options.target) && artifactKind !== 'WebAssemblyModule') {
    const nativePlan = ToolchainPlan.nativeCommand(
      toolchain,
      artifactKind,
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
      optimization: options.optimization,
      ...(options.configuration === undefined ? {} : { configuration: options.configuration }),
      destination,
      toolchain,
      nativeLinkInputs: Target.isNative(options.target)
        ? project.build.nativeLinkInputs
        : Object.freeze([]),
    }),
  )
}
