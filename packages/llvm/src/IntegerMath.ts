/**
 * Immutable overflow and exactness promises for integer instructions.
 *
 * @category integer math
 * @since 0.0.0
 */
export interface IntegerMath {
  readonly noSignedWrap: boolean
  readonly noUnsignedWrap: boolean
  readonly exact: boolean
}

/**
 * Optional fields accepted by {@link make}.
 *
 * @category integer math
 * @since 0.0.0
 */
export interface Input {
  readonly noSignedWrap?: boolean | undefined
  readonly noUnsignedWrap?: boolean | undefined
  readonly exact?: boolean | undefined
}

/**
 * Integer arithmetic with no overflow or exactness promises.
 *
 * @category integer math
 * @since 0.0.0
 */
export const none: IntegerMath = Object.freeze({
  noSignedWrap: false,
  noUnsignedWrap: false,
  exact: false,
})

/**
 * Constructs immutable integer instruction flags.
 *
 * **Gotchas**
 *
 * `nuw`, `nsw`, and `exact` let LLVM assume undefined behavior when their promise is violated.
 * Only enable a flag when the source language or prior analysis proves it.
 *
 * **Example** (Creating integer flags)
 *
 * ```ts
 * import * as IntegerMath from '@silk-effect/llvm/IntegerMath'
 *
 * const flags = IntegerMath.make({ noUnsignedWrap: true })
 * // IntegerMath.toText(flags) equals ['nuw']
 * ```
 *
 * @category integer math
 * @since 0.0.0
 */
export const make = (input: Input = {}): IntegerMath =>
  Object.freeze({
    noSignedWrap: input.noSignedWrap ?? false,
    noUnsignedWrap: input.noUnsignedWrap ?? false,
    exact: input.exact ?? false,
  })

/**
 * Returns a copy with the signed-overflow promise enabled or disabled.
 *
 * @category integer math
 * @since 0.0.0
 */
export const withNoSignedWrap = (self: IntegerMath, enabled = true): IntegerMath =>
  Object.freeze({ ...self, noSignedWrap: enabled })

/**
 * Returns a copy with the unsigned-overflow promise enabled or disabled.
 *
 * @category integer math
 * @since 0.0.0
 */
export const withNoUnsignedWrap = (self: IntegerMath, enabled = true): IntegerMath =>
  Object.freeze({ ...self, noUnsignedWrap: enabled })

/**
 * Returns a copy with the exact division or shift promise enabled or disabled.
 *
 * @category integer math
 * @since 0.0.0
 */
export const withExact = (self: IntegerMath, enabled = true): IntegerMath =>
  Object.freeze({ ...self, exact: enabled })

/**
 * Encodes flags for the corresponding LLVM instruction-record field.
 *
 * @category integer math
 * @since 0.0.0
 */
export const toBitcode = (self: IntegerMath): number =>
  (self.noUnsignedWrap ? 1 : 0) | (self.noSignedWrap ? 2 : 0) | (self.exact ? 1 : 0)

/**
 * Renders enabled flags in LLVM's canonical textual order.
 *
 * @category integer math
 * @since 0.0.0
 */
export const toText = (self: IntegerMath): ReadonlyArray<string> =>
  Object.freeze(
    [
      self.noUnsignedWrap ? 'nuw' : '',
      self.noSignedWrap ? 'nsw' : '',
      self.exact ? 'exact' : '',
    ].filter((flag) => flag !== ''),
  )
