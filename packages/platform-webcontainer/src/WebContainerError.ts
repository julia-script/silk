import * as Data from 'effect/Data'
import * as Predicate from 'effect/Predicate'

/**
 * Semantic reason for caller input rejected before a WebContainer operation runs.
 *
 * @category errors
 * @since 0.0.0
 */
export interface InvalidInput {
  readonly _tag: 'InvalidInput'
  readonly description: string
}

/**
 * Semantic reason for an operation rejected by the package's lifecycle state.
 *
 * @category errors
 * @since 0.0.0
 */
export interface InvalidState {
  readonly _tag: 'InvalidState'
  readonly description: string
}

/**
 * Reason preserving a thrown or rejected value from an external boundary.
 *
 * @category errors
 * @since 0.0.0
 */
export interface WrappedFailure {
  readonly _tag: 'WrappedFailure'
  readonly cause: unknown
}

/**
 * Recovery reasons exposed by {@link WebContainerError}.
 *
 * @category errors
 * @since 0.0.0
 */
export type Reason = InvalidInput | InvalidState | WrappedFailure

/**
 * Optional operation context attached to a {@link WebContainerError}.
 *
 * @category errors
 * @since 0.0.0
 */
export interface Details {
  readonly path?: string
  readonly command?: string
}

interface Options extends Details {
  readonly operation: string
  readonly message: string
  readonly reason: Reason
}

interface TaggedFields extends Options {
  readonly cause?: unknown
}

/**
 * Typed failure for WebContainer runtime, process, event, and primitive filesystem boundaries.
 *
 * **Details**
 *
 * Only `WrappedFailure` reasons carry causal ancestry. Semantic input and state failures remain
 * stable recovery data without a fabricated JavaScript cause.
 *
 * @category errors
 * @since 0.0.0
 */
export class WebContainerError extends Data.TaggedError('WebContainerError')<TaggedFields> {
  constructor(options: Options) {
    super(
      options.reason._tag === 'WrappedFailure'
        ? { ...options, cause: options.reason.cause }
        : options,
    )
  }
}

/**
 * Creates a typed failure for invalid caller input.
 *
 * @category errors
 * @since 0.0.0
 */
export const invalidInput = (
  operation: string,
  message: string,
  description: string,
  details: Details = {},
): WebContainerError =>
  new WebContainerError({
    ...details,
    operation,
    message,
    reason: { _tag: 'InvalidInput', description },
  })

/**
 * Creates a typed failure for package-detected lifecycle misuse.
 *
 * @category errors
 * @since 0.0.0
 */
export const invalidState = (
  operation: string,
  message: string,
  description: string,
  details: Details = {},
): WebContainerError =>
  new WebContainerError({
    ...details,
    operation,
    message,
    reason: { _tag: 'InvalidState', description },
  })

/**
 * Creates a typed failure preserving an external rejection or thrown value.
 *
 * @category errors
 * @since 0.0.0
 */
export const wrappedFailure = (
  operation: string,
  message: string,
  cause: unknown,
  details: Details = {},
): WebContainerError =>
  new WebContainerError({
    ...details,
    operation,
    message,
    reason: { _tag: 'WrappedFailure', cause },
  })

/**
 * Reads a string message from an unknown failure without assuming its shape.
 *
 * @category error handling
 * @since 0.0.0
 */
export const messageOf = (cause: unknown): string | undefined =>
  Predicate.hasProperty(cause, 'message') && typeof cause.message === 'string'
    ? cause.message
    : undefined

/**
 * Reads a string error code from an unknown failure without casting it.
 *
 * @category error handling
 * @since 0.0.0
 */
export const codeOf = (cause: unknown): string | undefined =>
  Predicate.hasProperty(cause, 'code') && typeof cause.code === 'string' ? cause.code : undefined
