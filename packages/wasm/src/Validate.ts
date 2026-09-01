import * as Effect from 'effect/Effect'
import { invalidState, type WasmError, wrappedFailure } from './WasmError.js'

/**
 * Specification validation of an encoded module, through the host's own WebAssembly
 * implementation.
 *
 * **Details**
 *
 * `Binary.encode` produces bytes; nothing in the builder guarantees the host will accept them. A
 * module that fails validation cannot be instantiated at all, and the failure surfaces wherever
 * the module is finally used rather than where it was emitted. Validating at emission keeps the
 * two together.
 *
 * `WebAssembly.validate` answers only yes or no, so a rejected module is compiled a second time to
 * recover the host's diagnostic, which names the offending function.
 *
 * @since 0.0.0
 */

/**
 * One validation finding, naming the function the host rejected.
 *
 * @category models
 * @since 0.0.0
 */
export interface Violation {
  /** The offending function's name, its index when the host gives no name, or `module`. */
  readonly function: string
  /** The host's diagnostic. */
  readonly message: string
  /** Additional context lines, when the host supplies any. */
  readonly detail: ReadonlyArray<string>
}

/**
 * V8 reports a rejected body as `Compiling function #12:"name" failed: ...`. Recovering the name
 * turns "invalid module" into a finding that points at one function.
 *
 * @internal
 */
const functionOf = (message: string): string => {
  const named = /function #(\d+):"([^"]*)"/.exec(message)
  if (named?.[2] !== undefined && named[2] !== '') return named[2]
  const numbered = /function #(\d+)/.exec(message)
  return numbered?.[1] === undefined ? 'module' : `#${numbered[1]}`
}

/**
 * The host's WebAssembly implementation, named narrowly because the package builds against the
 * ES library alone: it is the boundary this actor owns, not an ambient global it assumes.
 *
 * @internal
 */
interface Host {
  readonly validate: (bytes: Uint8Array) => boolean
  readonly Module: new (bytes: Uint8Array) => unknown
}

/**
 * The package builds against the ES library, which does not describe `WebAssembly`, and its
 * consumers build against libraries that describe it differently. Declaring the one shape this
 * actor uses keeps both builds honest without a cast, and `typeof` reaches for it without
 * assuming a runtime that has it.
 *
 * @internal
 */
declare const WebAssembly: Host | undefined

/** @internal */
const host = (): Host | undefined => (typeof WebAssembly === 'undefined' ? undefined : WebAssembly)

/** @internal */
const compile = (implementation: Host, bytes: Uint8Array): ReadonlyArray<Violation> => {
  // A Uint8Array view over a larger buffer would hand the host the whole buffer.
  const module = bytes.slice()
  if (implementation.validate(module)) return Object.freeze([])
  try {
    new implementation.Module(module)
  } catch (cause) {
    const message = cause instanceof Error ? cause.message : String(cause)
    return Object.freeze([
      Object.freeze({ function: functionOf(message), message, detail: Object.freeze([]) }),
    ])
  }
  // `validate` said no and compilation said yes: report the disagreement rather than passing.
  return Object.freeze([
    Object.freeze({
      function: 'module',
      message: 'WebAssembly.validate rejected a module the host then compiled',
      detail: Object.freeze([]),
    }),
  ])
}

/**
 * Validates an encoded module and returns the violations the host reported.
 *
 * **Details**
 *
 * An empty result means the host accepts the module. A non-empty result carries the host's own
 * diagnostic and, where the host names one, the function it was found in.
 *
 * **Example** (Failing a build on an invalid module)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Binary from '@silklang/wasm/Binary'
 * import * as Validate from '@silklang/wasm/Validate'
 *
 * const check = Effect.gen(function* () {
 *   const bytes = yield* Binary.encode(builder)
 *   const violations = yield* Validate.validate(bytes)
 *   if (violations.length > 0) return yield* Effect.fail(Validate.format(violations))
 * })
 * ```
 *
 * @category validation
 * @since 0.0.0
 */
export const validate = Effect.fnUntraced(function* (
  bytes: Uint8Array,
): Effect.fn.Return<ReadonlyArray<Violation>, WasmError> {
  const implementation = host()
  if (implementation === undefined) {
    return yield* invalidState({
      operation: 'Validate.validate',
      message: 'The host has no WebAssembly implementation to validate against',
      state: 'WebAssembly',
    })
  }
  return yield* Effect.try({
    try: () => compile(implementation, bytes),
    catch: (cause) =>
      wrappedFailure({
        operation: 'Validate.validate',
        message: 'The host could not validate the encoded module',
        cause,
      }),
  })
})

/**
 * Renders violations one per finding, naming the function and the diagnostic.
 *
 * @category validation
 * @since 0.0.0
 */
export const format = (violations: ReadonlyArray<Violation>): string =>
  violations
    .map((violation) =>
      [`${violation.message} (in ${violation.function})`, ...violation.detail].join('\n'),
    )
    .join('\n')
