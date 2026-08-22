import * as Data from 'effect/Data'
import * as Result from 'effect/Result'
import type * as Backend from './Backend.js'
import * as LlvmBackend from './LlvmBackend.js'
import type * as Target from './Target.js'
import * as WasmBackend from './WasmBackend.js'

/** Stable backend-id resolution and explicit backend-target compatibility validation. */

/** Every backend the bootstrap compiler can dispatch to, in deterministic order. */
export const backends: ReadonlyArray<Backend.Backend> = Object.freeze([
  LlvmBackend.LlvmBackend,
  WasmBackend.WasmBackend,
])

export type BackendRegistryErrorReason =
  | { readonly _tag: 'UnknownBackend'; readonly id: string }
  | {
      readonly _tag: 'IncompatibleTarget'
      readonly backend: Backend.Id
      readonly target: Target.Id
    }

/** Explicit backend selection or compatibility validation failed. */
export class BackendRegistryError extends Data.TaggedError('BackendRegistryError')<{
  readonly operation: 'BackendRegistry.resolve' | 'BackendRegistry.requireTarget'
  readonly message: string
  readonly reason: BackendRegistryErrorReason
}> {}

/** Resolves a stable backend id without deriving it from a target. */
export const resolve = (id: string): Result.Result<Backend.Backend, BackendRegistryError> => {
  const backend = backends.find((candidate) => candidate.id === id)
  return backend === undefined
    ? Result.fail(
        new BackendRegistryError({
          operation: 'BackendRegistry.resolve',
          message: `Unknown backend ${id}; expected llvm or wasm`,
          reason: { _tag: 'UnknownBackend', id },
        }),
      )
    : Result.succeed(backend)
}

/** Validates an explicitly selected backend-target pair before backend construction. */
export const requireTarget = (
  backend: Backend.Backend,
  target: Target.Target,
): Result.Result<Backend.Backend, BackendRegistryError> =>
  backend.targets.includes(target.id)
    ? Result.succeed(backend)
    : Result.fail(
        new BackendRegistryError({
          operation: 'BackendRegistry.requireTarget',
          message: `Backend ${backend.id} does not support target ${target.id}`,
          reason: { _tag: 'IncompatibleTarget', backend: backend.id, target: target.id },
        }),
      )
