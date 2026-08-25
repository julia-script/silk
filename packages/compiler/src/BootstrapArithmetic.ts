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
): boolean | undefined => {
  if (operation === 'Equals') {
    return left === right
  }
  if (operation === 'NotEquals') {
    return left !== right
  }
  if (operation === 'LessThan') {
    return left < right
  }
  if (operation === 'LessOrEqual') {
    return left <= right
  }
  if (operation === 'GreaterThan') {
    return left > right
  }
  if (operation === 'GreaterOrEqual') {
    return left >= right
  }
  return undefined
}

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
  let exact: bigint
  switch (operation) {
    case 'Add':
    case 'WrappingAdd':
    case 'SaturatingAdd':
      exact = left + right
      break
    case 'Subtract':
    case 'WrappingSubtract':
    case 'SaturatingSubtract':
      exact = left - right
      break
    case 'Multiply':
    case 'WrappingMultiply':
    case 'SaturatingMultiply':
      exact = left * right
      break
    case 'Divide':
      exact = left / right
      break
    case 'Remainder':
      exact = left % right
      break
    case 'BitAnd':
      exact = fromBits(leftBits & rightBits)
      break
    case 'BitOr':
      exact = fromBits(leftBits | rightBits)
      break
    case 'BitXor':
      exact = fromBits(leftBits ^ rightBits)
      break
    case 'ShiftLeft':
      exact = fromBits(leftBits << right)
      break
    case 'ShiftRight':
      exact = scalar.signedness === 'Signed' ? left >> right : fromBits(leftBits >> right)
      break
    case 'RotateLeft':
      exact = fromBits(rotatedLeft)
      break
    default:
      exact = fromBits(rotatedRight)
  }
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
  let value = exact
  if (wrapping) value = fromBits(exact)
  if (saturating && exact < range.minimum) value = range.minimum
  if (saturating && exact > range.maximum) value = range.maximum
  return Object.freeze({ _tag: 'Integer', type: scalar.spelling, value })
}

/** Computes the exact checked integer result; undefined represents trap/None. */
export const checked = (
  operation: string,
  left: bigint,
  right: bigint | undefined,
): bigint | undefined => {
  if (operation.startsWith('CheckedConvertTo')) {
    return left
  }
  if (operation === 'CheckedAdd' && right !== undefined) {
    return left + right
  }
  if (operation === 'CheckedSubtract' && right !== undefined) {
    return left - right
  }
  if (operation === 'CheckedMultiply' && right !== undefined) {
    return left * right
  }
  if (operation === 'CheckedDivide' && right !== undefined && right !== 0n) {
    return left / right
  }
  if (operation === 'CheckedRemainder' && right !== undefined && right !== 0n) {
    return left % right
  }
  return undefined
}
