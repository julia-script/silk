import * as Scalar from '../../src/Scalar.js'

const integerArguments = (operation: Scalar.Operation): string => {
  switch (operation.code) {
    case 'Subtract':
    case 'WrappingSubtract':
    case 'SaturatingSubtract':
    case 'CheckedSubtract':
      return '44, 2'
    case 'Multiply':
    case 'WrappingMultiply':
    case 'SaturatingMultiply':
    case 'CheckedMultiply':
      return '21, 2'
    case 'Divide':
    case 'CheckedDivide':
      return '84, 2'
    case 'Remainder':
    case 'CheckedRemainder':
      return '42, 100'
    case 'BitAnd':
      return '42, 63'
    case 'ShiftLeft':
      return '21, 1'
    case 'ShiftRight':
      return '84, 1'
    case 'RotateLeft':
    case 'RotateRight':
      return '42, 0'
    case 'Equals':
      return '42, 42'
    case 'NotEquals':
    case 'LessThan':
      return '41, 42'
    case 'LessOrEqual':
    case 'GreaterOrEqual':
      return '42, 42'
    case 'GreaterThan':
      return '43, 42'
    default:
      return operation.arity === 1 ? '42' : '40, 2'
  }
}

const integerCase = (
  scalar: Scalar.IntegerScalar,
  operation: Scalar.Operation,
  ordinal: number,
): string => {
  const target =
    Scalar.conversionTarget(operation.code) ??
    Scalar.floatConversionTarget(operation.code) ??
    scalar
  let invocation: string
  if (operation.code === 'BitNot') {
    invocation = `${scalar.spelling}.bitNot(${scalar.spelling}.bitNot(42))`
  } else if (
    operation.code === 'Negate' ||
    operation.code === 'WrappingNegate' ||
    operation.code === 'SaturatingNegate'
  ) {
    invocation = `${scalar.spelling}.${operation.spelling}(-42)`
  } else {
    invocation = `${scalar.spelling}.${operation.spelling}(${integerArguments(operation)})`
  }
  if (operation.result === 'Boolean')
    return `fn integerCase${ordinal}() -> i32 { if ${invocation} { return 42 } return 0 }`
  if (operation.result === 'OptionSelf' || operation.result === 'OptionTarget')
    return `import silk.option { Option }
fn integerCase${ordinal}() -> i32 {
  return match move ${invocation} {
    Option<${target.spelling}>.None => 0
    Option<${target.spelling}>.Some { value } => ${target.spelling === 'i32' ? 'value' : `${target.spelling}.toI32(value)`}
  }
}`
  return `fn integerCase${ordinal}() -> i32 { return ${target.spelling === 'i32' ? invocation : `${target.spelling}.toI32(${invocation})`} }`
}

export const integerOperationMatrix = (() => {
  const cases = Scalar.integers().flatMap((scalar) =>
    scalar.operations.map((operation) => ({ scalar, operation })),
  )
  const imports = Scalar.integers()
    .map((scalar) => `import silk.${scalar.spelling} as ${scalar.spelling}`)
    .join('\n')
  return `${imports}
import silk.f32 as f32
import silk.f64 as f64
import silk.option { Option }
${cases.map(({ scalar, operation }, ordinal) => integerCase(scalar, operation, ordinal)).join('\n')}
fn verify(value: i32) -> () { if value != 42 { let boom = 1 / 0 } }
pub fn main() -> i32 {
${cases.map((_, ordinal) => `  let checked${ordinal} = verify(integerCase${ordinal}())`).join('\n')}
  return 42
}`
})()

const floatArguments = (scalar: Scalar.FloatScalar, operation: Scalar.Operation): string => {
  switch (operation.code) {
    case 'Subtract':
      return '44.0, 2.0'
    case 'Multiply':
      return '21.0, 2.0'
    case 'Divide':
      return '84.0, 2.0'
    case 'Remainder':
      return '42.0, 100.0'
    case 'Negate':
      return '-42.0'
    case 'Equals':
      return '42.0, 42.0'
    case 'NotEquals':
    case 'LessThan':
      return '41.0, 42.0'
    case 'LessOrEqual':
    case 'GreaterOrEqual':
      return '42.0, 42.0'
    case 'GreaterThan':
      return '43.0, 42.0'
    case 'IsNaN':
      return `${scalar.spelling}.fromBits(${scalar.spelling === 'f32' ? '2143289344' : '9221120237041090560'})`
    case 'IsInfinite':
      return `${scalar.spelling}.fromBits(${scalar.spelling === 'f32' ? '2139095040' : '9218868437227405312'})`
    case 'IsSubnormal':
      return `${scalar.spelling}.fromBits(1)`
    case 'IsSignNegative':
      return '-42.0'
    case 'TotalOrder':
      return `${scalar.spelling}.fromBits(${scalar.spelling === 'f32' ? '2147483648' : '9223372036854775808'}), 0.0`
    case 'Sqrt':
      return '1764.0'
    case 'Sin':
    case 'Cos':
      return '0.0'
    case 'FromBits':
      return scalar.spelling === 'f32' ? '1109917696' : '4631107791820423168'
    default:
      return operation.arity === 1 ? '42.0' : '40.0, 2.0'
  }
}

export const floatOperationMatrix = (() => {
  const cases = Scalar.floats().flatMap((scalar) =>
    scalar.operations.map((operation) => ({ scalar, operation })),
  )
  const functions = cases.map(({ scalar, operation }, ordinal) => {
    const invocation = `${scalar.spelling}.${operation.spelling}(${floatArguments(scalar, operation)})`
    if (operation.result === 'Boolean')
      return `fn floatCase${ordinal}() -> i32 { if ${invocation} { return 42 } return 0 }`
    if (operation.code === 'Sin' || operation.code === 'Cos') {
      let expected = '0'
      if (operation.code === 'Cos')
        expected = scalar.spelling === 'f32' ? '1065353216' : '4607182418800017408'
      return `fn floatCase${ordinal}() -> i32 { if ${scalar.spelling}.toBits(${invocation}) == ${expected} { return 42 } return 0 }`
    }
    if (operation.code === 'ToBits')
      return `fn floatCase${ordinal}() -> i32 { return ${scalar.spelling}.toI32(${scalar.spelling}.fromBits(${invocation})) }`
    const target = Scalar.find(operation.result === 'Self' ? scalar.spelling : operation.result)
    return `fn floatCase${ordinal}() -> i32 { return ${target?.spelling ?? scalar.spelling}.toI32(${invocation}) }`
  })
  const imports = [...Scalar.floats(), ...Scalar.integers()]
    .map((scalar) => `import silk.${scalar.spelling} as ${scalar.spelling}`)
    .join('\n')
  return `${imports}
${functions.join('\n')}
fn verify(value: i32) -> () { if value != 42 { let boom = 1 / 0 } }
pub fn main() -> i32 {
${cases.map((_, ordinal) => `  let checked${ordinal} = verify(floatCase${ordinal}())`).join('\n')}
  return 42
}`
})()
