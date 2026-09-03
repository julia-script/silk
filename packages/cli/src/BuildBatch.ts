import * as BackendRegistry from '@silklang/compiler/BackendRegistry'
import * as NativeToolchain from '@silklang/compiler/NativeToolchain'
import type * as Project from '@silklang/compiler/Project'
import * as TargetSelector from '@silklang/compiler/TargetSelector'
import type * as ToolchainPlan from '@silklang/compiler/ToolchainPlan'
import * as Data from 'effect/Data'
import * as Result from 'effect/Result'
import * as BuildPlan from './BuildPlan.js'

/** One preflighted backend and an ordered non-empty collection of single-target plans. */
export interface BuildBatch {
  readonly _tag: 'BuildBatch'
  readonly plans: readonly [BuildPlan.BuildPlan, ...Array<BuildPlan.BuildPlan>]
}

export type BuildBatchErrorReason =
  | { readonly _tag: 'Backend'; readonly error: BackendRegistry.BackendRegistryError }
  | { readonly _tag: 'Target'; readonly error: TargetSelector.TargetSelectorError }
  | { readonly _tag: 'Plan'; readonly error: BuildPlan.BuildPlanError }
  | { readonly _tag: 'EmptyTargets' }

/** A complete project build batch could not pass preflight. */
export class BuildBatchError extends Data.TaggedError('BuildBatchError')<{
  readonly operation: 'BuildBatch.make'
  readonly message: string
  readonly reason: BuildBatchErrorReason
}> {}

export interface Options {
  readonly backend?: string
  readonly targets?: ReadonlyArray<string>
  readonly profile: ToolchainPlan.OptimizationProfile
  readonly purpose?: BuildPlan.Purpose
  readonly clang?: string
  readonly llvmAr?: string
}

/** Resolves one backend and all targets before returning any executable build work. */
export const make = (
  project: Project.Project,
  options: Options,
): Result.Result<BuildBatch, BuildBatchError> => {
  const selectedBackend = BackendRegistry.resolve(options.backend ?? project.build.backend)
  if (Result.isFailure(selectedBackend)) {
    return Result.fail(
      new BuildBatchError({
        operation: 'BuildBatch.make',
        message: selectedBackend.failure.message,
        reason: { _tag: 'Backend', error: selectedBackend.failure },
      }),
    )
  }
  const selectors = options.targets ?? project.build.targets
  const selectedTargets = TargetSelector.resolveAll(selectors, NativeToolchain.hostSelection())
  if (Result.isFailure(selectedTargets)) {
    return Result.fail(
      new BuildBatchError({
        operation: 'BuildBatch.make',
        message: selectedTargets.failure.message,
        reason: { _tag: 'Target', error: selectedTargets.failure },
      }),
    )
  }
  const plans: Array<BuildPlan.BuildPlan> = []
  for (const target of selectedTargets.success) {
    const plan = BuildPlan.make(project, {
      backend: selectedBackend.success,
      target,
      profile: options.profile,
      ...(options.purpose === undefined ? {} : { purpose: options.purpose }),
      ...(options.clang === undefined ? {} : { clang: options.clang }),
      ...(options.llvmAr === undefined ? {} : { llvmAr: options.llvmAr }),
    })
    if (Result.isFailure(plan)) {
      return Result.fail(
        new BuildBatchError({
          operation: 'BuildBatch.make',
          message: plan.failure.message,
          reason: { _tag: 'Plan', error: plan.failure },
        }),
      )
    }
    plans.push(plan.success)
  }
  const first = plans.at(0)
  if (first === undefined) {
    return Result.fail(
      new BuildBatchError({
        operation: 'BuildBatch.make',
        message: 'A build batch requires at least one target',
        reason: { _tag: 'EmptyTargets' },
      }),
    )
  }
  const ordered: readonly [BuildPlan.BuildPlan, ...Array<BuildPlan.BuildPlan>] = Object.freeze([
    first,
    ...plans.slice(1),
  ])
  return Result.succeed(Object.freeze({ _tag: 'BuildBatch', plans: ordered }))
}
