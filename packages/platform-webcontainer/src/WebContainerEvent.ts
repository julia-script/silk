import * as Effect from 'effect/Effect'
import * as Queue from 'effect/Queue'
import type * as Scope from 'effect/Scope'
import * as Stream from 'effect/Stream'
import type * as WebContainerError from './WebContainerError.js'

/**
 * Notification that a runtime port opened or closed.
 *
 * @category models
 * @since 0.0.0
 */
export interface Port {
  readonly _tag: 'Port'
  readonly port: number
  readonly status: 'open' | 'close'
  readonly url: string
}

/**
 * Notification that an HTTP server is ready to receive requests.
 *
 * @category models
 * @since 0.0.0
 */
export interface ServerReady {
  readonly _tag: 'ServerReady'
  readonly port: number
  readonly url: string
}

/**
 * Internal runtime error delivered as an event value rather than a stream failure.
 *
 * @category models
 * @since 0.0.0
 */
export interface InternalError {
  readonly _tag: 'InternalError'
  readonly message: string
}

/**
 * Shared preview identity and URL fields carried by every preview message.
 *
 * @category models
 * @since 0.0.0
 */
export interface PreviewLocation {
  readonly previewId: string
  readonly port: number
  readonly pathname: string
  readonly search: string
  readonly hash: string
}

/**
 * Uncaught exception forwarded from a preview frame.
 *
 * @category models
 * @since 0.0.0
 */
export interface PreviewUncaughtException extends PreviewLocation {
  readonly _tag: 'PreviewUncaughtException'
  readonly message: string
  readonly stack: string | undefined
}

/**
 * Unhandled rejection forwarded from a preview frame.
 *
 * @category models
 * @since 0.0.0
 */
export interface PreviewUnhandledRejection extends PreviewLocation {
  readonly _tag: 'PreviewUnhandledRejection'
  readonly message: string
  readonly stack: string | undefined
}

/**
 * `console.error` call forwarded from a preview frame.
 *
 * @category models
 * @since 0.0.0
 */
export interface PreviewConsoleError extends PreviewLocation {
  readonly _tag: 'PreviewConsoleError'
  readonly args: ReadonlyArray<unknown>
  readonly stack: string
}

/**
 * Typed preview messages emitted by the runtime service.
 *
 * @category models
 * @since 0.0.0
 */
export type PreviewMessage =
  | PreviewUncaughtException
  | PreviewUnhandledRejection
  | PreviewConsoleError

/**
 * Effect-native cleanup returned after registering one event listener.
 *
 * @category resource management
 * @since 0.0.0
 */
export interface Subscription {
  readonly unsubscribe: Effect.Effect<void>
}

/**
 * Callback registration capability consumed by {@link stream}.
 *
 * @category protocols
 * @since 0.0.0
 */
export type Subscribe<A> = (
  listener: (event: A) => void,
) => Effect.Effect<Subscription, WebContainerError.WebContainerError>

/**
 * Creates a typed port event value.
 *
 * @category constructors
 * @since 0.0.0
 */
export const port = (port: number, status: Port['status'], url: string): Port => ({
  _tag: 'Port',
  port,
  status,
  url,
})

/**
 * Creates a typed server-ready event value.
 *
 * @category constructors
 * @since 0.0.0
 */
export const serverReady = (port: number, url: string): ServerReady => ({
  _tag: 'ServerReady',
  port,
  url,
})

/**
 * Creates a typed internal runtime error event.
 *
 * @category constructors
 * @since 0.0.0
 */
export const internalError = (message: string): InternalError => ({
  _tag: 'InternalError',
  message,
})

/**
 * Converts a synchronous callback subscription into a lazy, per-consumer stream.
 * The upstream callback has no backpressure protocol, so each consumer receives
 * its own unbounded queue and must not abandon a running stream.
 *
 * **When to use**
 *
 * Use when adapting a callback API whose registration and cleanup already cross typed Effect
 * boundaries.
 *
 * **Gotchas**
 *
 * The upstream callback is synchronous and cannot apply backpressure. Interrupt consumers that no
 * longer need events so their subscription and queue are finalized.
 *
 * @category converting
 * @since 0.0.0
 */
export const stream = <A>(
  subscribe: Subscribe<A>,
): Stream.Stream<A, WebContainerError.WebContainerError> =>
  Stream.callback((queue) =>
    Effect.acquireRelease(
      subscribe((event) => {
        Queue.offerUnsafe(queue, event)
      }),
      ({ unsubscribe }) => unsubscribe,
    ),
  )

/**
 * Scope type retained for integrations that describe event-stream ownership.
 *
 * @category utility types
 * @since 0.0.0
 */
export type StreamScope = Scope.Scope
