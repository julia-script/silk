import * as Data from 'effect/Data'

/**
 * Describes caller input rejected by a WebAssembly operation.
 *
 * @category errors
 * @since 0.0.0
 */
export interface InvalidInput {
  readonly _tag: 'InvalidInput'
  readonly input: unknown
}

/**
 * Describes an invalid builder state, handle, or ownership relation.
 *
 * @category errors
 * @since 0.0.0
 */
export interface InvalidState {
  readonly _tag: 'InvalidState'
  readonly state: unknown
}

/**
 * Describes a module or function body rejected by WebAssembly specification validation.
 *
 * @category errors
 * @since 0.0.0
 */
export interface ValidationFailed {
  readonly _tag: 'ValidationFailed'
  readonly detail: unknown
}

/**
 * Describes an underlying JavaScript failure caught at a WebAssembly boundary.
 *
 * @category errors
 * @since 0.0.0
 */
export interface WrappedFailure {
  readonly _tag: 'WrappedFailure'
  readonly cause: unknown
}

/**
 * Semantic reason for an expected WebAssembly operation failure.
 *
 * @category errors
 * @since 0.0.0
 */
export type Reason = InvalidInput | InvalidState | ValidationFailed | WrappedFailure

/**
 * Constructor fields accepted by {@link WasmError}.
 *
 * @category errors
 * @since 0.0.0
 */
export interface Options {
  readonly operation: string
  readonly message: string
  readonly reason: Reason
}

interface TaggedFields extends Options {
  readonly cause?: unknown
}

/**
 * The typed failure channel shared by every effectful WebAssembly operation.
 *
 * **Details**
 *
 * `operation` identifies the actor function that rejected the request and `message` provides
 * stable human context. `reason` distinguishes invalid input, invalid state or ownership, a
 * specification validation failure, and a wrapped implementation failure. Only a wrapped failure
 * populates JavaScript's `cause` property.
 *
 * **Example** (Recovering from a rejected declaration)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silk-effect/wasm/Builder'
 * import * as Type from '@silk-effect/wasm/Type'
 * import * as ValType from '@silk-effect/wasm/ValType'
 *
 * const reasonTag = Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   return yield* Type.func(builder, [ValType.i32], [ValType.i32]).pipe(
 *     Effect.map(() => 'accepted'),
 *     Effect.catchTag('WasmError', (error) => Effect.succeed(error.reason._tag)),
 *   )
 * })
 * ```
 *
 * @category errors
 * @since 0.0.0
 */
export class WasmError extends Data.TaggedError('WasmError')<TaggedFields> {
  constructor(options: Options) {
    super(
      options.reason._tag === 'WrappedFailure'
        ? { ...options, cause: options.reason.cause }
        : options,
    )
  }
}

/**
 * Constructs a WebAssembly failure for rejected caller input.
 *
 * @category errors
 * @since 0.0.0
 */
export const invalidInput = (options: {
  readonly operation: string
  readonly message: string
  readonly input: unknown
}): WasmError =>
  new WasmError({
    operation: options.operation,
    message: options.message,
    reason: { _tag: 'InvalidInput', input: options.input },
  })

/**
 * Constructs a WebAssembly failure for invalid builder state or ownership.
 *
 * @category errors
 * @since 0.0.0
 */
export const invalidState = (options: {
  readonly operation: string
  readonly message: string
  readonly state: unknown
}): WasmError =>
  new WasmError({
    operation: options.operation,
    message: options.message,
    reason: { _tag: 'InvalidState', state: options.state },
  })

/**
 * Constructs a WebAssembly failure for a module or body rejected by specification validation.
 *
 * @category errors
 * @since 0.0.0
 */
export const validationFailed = (options: {
  readonly operation: string
  readonly message: string
  readonly detail: unknown
}): WasmError =>
  new WasmError({
    operation: options.operation,
    message: options.message,
    reason: { _tag: 'ValidationFailed', detail: options.detail },
  })

/**
 * Wraps an underlying JavaScript failure with WebAssembly operation context.
 *
 * @category errors
 * @since 0.0.0
 */
export const wrappedFailure = (options: {
  readonly operation: string
  readonly message: string
  readonly cause: unknown
}): WasmError =>
  new WasmError({
    operation: options.operation,
    message: options.message,
    reason: { _tag: 'WrappedFailure', cause: options.cause },
  })
