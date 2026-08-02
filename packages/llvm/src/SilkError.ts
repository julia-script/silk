import * as Data from 'effect/Data'

/**
 * The typed failure channel shared by every effectful LLVM operation.
 *
 * **Details**
 *
 * `operation` identifies the actor function that rejected the input, `message` is stable human
 * context, and `cause` retains the offending value or wrapped implementation failure. Programs
 * normally handle this through Effect's typed error combinators rather than JavaScript `catch`.
 *
 * **Example** (Recovering from validation failure)
 *
 * Recover from an LLVM validation failure by its Effect error tag.
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as AddrSpace from '@silk-effect/llvm/AddrSpace'
 *
 * const description = await Effect.runPromise(
 *   AddrSpace.make(-1).pipe(
 *     Effect.map((space) => `address space ${space.value}`),
 *     Effect.catchTag('SilkError', (error) =>
 *       Effect.succeed(`${error.operation}: ${error.message}`),
 *     ),
 *   ),
 * )
 * // description starts with 'AddrSpace.make:'
 * ```
 *
 * @category errors
 * @since 0.0.0
 */
export class SilkError extends Data.TaggedError('SilkError')<{
  readonly operation: string
  readonly message: string
  readonly cause: unknown
}> {}
