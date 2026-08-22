import type * as Mir from './Mir.js'
import * as Scalar from './Scalar.js'

export type IntegralResult =
  | { readonly _tag: 'Integer'; readonly type: Scalar.IntegerSpelling; readonly value: bigint }
  | { readonly _tag: 'Comparison'; readonly value: boolean }
  | { readonly _tag: 'Trap'; readonly reason: string }

export const compare = (
  operation: Mir.BinaryOperator,
  left: bigint,
  right: bigint,
): boolean | undefined =>
  operation === 'Equals'
    ? left === right
    : operation === 'NotEquals'
      ? left !== right
      : operation === 'LessThan'
        ? left < right
        : operation === 'LessOrEqual'
          ? left <= right
          : operation === 'GreaterThan'
            ? left > right
            : operation === 'GreaterOrEqual'
              ? left >= right
              : undefined

/** Evaluates one target-width integer binary operation for every bootstrap execution path. */
export const integralBinary = (
  operation: Mir.BinaryOperator,
  scalar: Scalar.IntegerScalar,
  pointerBits: 32 | 64,
  left: bigint,
  right: bigint,
): IntegralResult => {
  const compared = compare(operation, left, right)
  if (compared !== undefined) return Object.freeze({ _tag: 'Comparison', value: compared })
  if ((operation === 'Divide' || operation === 'Remainder') && right === 0n)
    return Object.freeze({ _tag: 'Trap', reason: 'division by zero' })
  const width = Scalar.bits(scalar, pointerBits)
  if (
    (operation === 'ShiftLeft' || operation === 'ShiftRight') &&
    (right < 0n || right >= BigInt(width))
  )
    return Object.freeze({ _tag: 'Trap', reason: `invalid ${operation} count ${right}` })
  const fromBits = (input: bigint): bigint =>
    scalar.signedness === 'Signed' ? BigInt.asIntN(width, input) : BigInt.asUintN(width, input)
  const leftBits = BigInt.asUintN(width, left)
  const rightBits = BigInt.asUintN(width, right)
  const rotate = Number(right % BigInt(width))
  const rotatedLeft =
    rotate === 0
      ? leftBits
      : BigInt.asUintN(width, (leftBits << BigInt(rotate)) | (leftBits >> BigInt(width - rotate)))
  const rotatedRight =
    rotate === 0
      ? leftBits
      : BigInt.asUintN(width, (leftBits >> BigInt(rotate)) | (leftBits << BigInt(width - rotate)))
  const exact =
    operation === 'Add' || operation === 'WrappingAdd' || operation === 'SaturatingAdd'
      ? left + right
      : operation === 'Subtract' ||
          operation === 'WrappingSubtract' ||
          operation === 'SaturatingSubtract'
        ? left - right
        : operation === 'Multiply' ||
            operation === 'WrappingMultiply' ||
            operation === 'SaturatingMultiply'
          ? left * right
          : operation === 'Divide'
            ? left / right
            : operation === 'Remainder'
              ? left % right
              : operation === 'BitAnd'
                ? fromBits(leftBits & rightBits)
                : operation === 'BitOr'
                  ? fromBits(leftBits | rightBits)
                  : operation === 'BitXor'
                    ? fromBits(leftBits ^ rightBits)
                    : operation === 'ShiftLeft'
                      ? fromBits(leftBits << right)
                      : operation === 'ShiftRight'
                        ? scalar.signedness === 'Signed'
                          ? left >> right
                          : fromBits(leftBits >> right)
                        : operation === 'RotateLeft'
                          ? fromBits(rotatedLeft)
                          : fromBits(rotatedRight)
  const range = Scalar.range(scalar, pointerBits)
  const wrapping =
    operation === 'WrappingAdd' ||
    operation === 'WrappingSubtract' ||
    operation === 'WrappingMultiply'
  const saturating =
    operation === 'SaturatingAdd' ||
    operation === 'SaturatingSubtract' ||
    operation === 'SaturatingMultiply'
  if (!wrapping && !saturating && (exact < range.minimum || exact > range.maximum))
    return Object.freeze({
      _tag: 'Trap',
      reason:
        scalar.signedness === 'Unsigned' && exact < 0n
          ? 'arithmetic underflow'
          : 'arithmetic overflow',
    })
  const value = wrapping
    ? fromBits(exact)
    : saturating
      ? exact < range.minimum
        ? range.minimum
        : exact > range.maximum
          ? range.maximum
          : exact
      : exact
  return Object.freeze({ _tag: 'Integer', type: scalar.spelling, value })
}

/** Computes the exact checked integer result; undefined represents trap/None. */
export const checked = (
  operation: string,
  left: bigint,
  right: bigint | undefined,
): bigint | undefined =>
  operation.startsWith('CheckedConvertTo')
    ? left
    : operation === 'CheckedAdd' && right !== undefined
      ? left + right
      : operation === 'CheckedSubtract' && right !== undefined
        ? left - right
        : operation === 'CheckedMultiply' && right !== undefined
          ? left * right
          : operation === 'CheckedDivide' && right !== undefined && right !== 0n
            ? left / right
            : operation === 'CheckedRemainder' && right !== undefined && right !== 0n
              ? left % right
              : undefined
