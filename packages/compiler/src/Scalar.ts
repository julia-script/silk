/** The closed lowercase integer vocabulary admitted by the bootstrap compiler. */
export type IntegerSpelling =
  | 'u8'
  | 'u16'
  | 'u32'
  | 'u64'
  | 'usize'
  | 'i8'
  | 'i16'
  | 'i32'
  | 'i64'
  | 'isize'

/** Every compiler-known scalar spelling, including the canonical Boolean. */
export type Spelling = 'bool' | IntegerSpelling

export type FixedBits = 8 | 16 | 32 | 64
export type ByteWidth = 1 | 2 | 4 | 8

/** Whether a scalar width is fixed by its identity or selected by the compilation target. */
export type Width =
  | { readonly _tag: 'FixedWidth'; readonly bits: FixedBits }
  | { readonly _tag: 'PointerWidth' }

/** How a scalar occupies storage before a concrete target is selected. */
export type Layout =
  | {
      readonly _tag: 'FixedLayout'
      readonly size: ByteWidth
      readonly alignment: ByteWidth
    }
  | { readonly _tag: 'PointerLayout' }

/** The backend-neutral operation code used by scalar intrinsic contracts. */
export type OperationCode =
  | 'Add'
  | 'Subtract'
  | 'Multiply'
  | 'Divide'
  | 'Remainder'
  | 'Negate'
  | 'Equals'
  | 'NotEquals'
  | 'LessThan'
  | 'LessOrEqual'
  | 'GreaterThan'
  | 'GreaterOrEqual'
  | 'BitAnd'
  | 'BitOr'
  | 'BitXor'
  | 'BitNot'
  | 'ShiftLeft'
  | 'ShiftRight'
  | 'RotateLeft'
  | 'RotateRight'
  | 'WrappingAdd'
  | 'WrappingSubtract'
  | 'WrappingMultiply'
  | 'WrappingNegate'
  | 'SaturatingAdd'
  | 'SaturatingSubtract'
  | 'SaturatingMultiply'
  | 'SaturatingNegate'
  | 'CheckedAdd'
  | 'CheckedSubtract'
  | 'CheckedMultiply'
  | 'CheckedDivide'
  | 'CheckedRemainder'
  | ConversionOperationCode
  | CheckedConversionOperationCode
  | 'Not'

export type ConversionOperationCode =
  | 'ConvertToU8'
  | 'ConvertToU16'
  | 'ConvertToU32'
  | 'ConvertToU64'
  | 'ConvertToUsize'
  | 'ConvertToI8'
  | 'ConvertToI16'
  | 'ConvertToI32'
  | 'ConvertToI64'
  | 'ConvertToIsize'

export type CheckedConversionOperationCode =
  | 'CheckedConvertToU8'
  | 'CheckedConvertToU16'
  | 'CheckedConvertToU32'
  | 'CheckedConvertToU64'
  | 'CheckedConvertToUsize'
  | 'CheckedConvertToI8'
  | 'CheckedConvertToI16'
  | 'CheckedConvertToI32'
  | 'CheckedConvertToI64'
  | 'CheckedConvertToIsize'

/** One operation declared by a scalar actor. */
export interface Operation {
  readonly spelling: string
  readonly code: OperationCode
  readonly arity: 1 | 2
  readonly result: 'Self' | 'Boolean' | 'OptionSelf' | 'OptionTarget' | Spelling
}

/** The backend lane selected from a scalar's logical width. */
export interface BackendLanes {
  readonly llvm: 'LogicalWidth'
  readonly wasm: 'I32' | 'I64' | 'Pointer'
}

/** One immutable integer entry in the authoritative scalar catalog. */
export interface IntegerScalar {
  readonly spelling: IntegerSpelling
  readonly category: 'Integer'
  readonly width: Width
  readonly signedness: 'Signed' | 'Unsigned'
  readonly layout: Layout
  readonly lanes: BackendLanes
  readonly operations: ReadonlyArray<Operation>
}

/** The immutable Boolean entry in the authoritative scalar catalog. */
export interface BooleanScalar {
  readonly spelling: 'bool'
  readonly category: 'Boolean'
  readonly width: Extract<Width, { readonly _tag: 'FixedWidth' }>
  readonly signedness: undefined
  readonly layout: Extract<Layout, { readonly _tag: 'FixedLayout' }>
  readonly lanes: BackendLanes
  readonly operations: ReadonlyArray<Operation>
}

/** One immutable source of truth for a compiler-known scalar. */
export type Scalar = IntegerScalar | BooleanScalar

const fixedWidth = (bits: FixedBits): Extract<Width, { readonly _tag: 'FixedWidth' }> =>
  Object.freeze({ _tag: 'FixedWidth', bits })

const pointerWidth: Extract<Width, { readonly _tag: 'PointerWidth' }> = Object.freeze({
  _tag: 'PointerWidth',
})

const fixedLayout = (size: ByteWidth): Extract<Layout, { readonly _tag: 'FixedLayout' }> =>
  Object.freeze({ _tag: 'FixedLayout', size, alignment: size })

const pointerLayout: Extract<Layout, { readonly _tag: 'PointerLayout' }> = Object.freeze({
  _tag: 'PointerLayout',
})

const operation = (
  spelling: string,
  code: OperationCode,
  arity: Operation['arity'],
  result: Operation['result'],
): Operation => Object.freeze({ spelling, code, arity, result })

const equalityOperations = Object.freeze([
  operation('equals', 'Equals', 2, 'Boolean'),
  operation('notEquals', 'NotEquals', 2, 'Boolean'),
])

const integerSpellings: ReadonlyArray<IntegerSpelling> = Object.freeze([
  'u8',
  'u16',
  'u32',
  'u64',
  'usize',
  'i8',
  'i16',
  'i32',
  'i64',
  'isize',
])

const conversionName = (target: IntegerSpelling): string =>
  `to${target[0]?.toUpperCase() ?? ''}${target.slice(1)}`

const conversionCode = (target: IntegerSpelling): ConversionOperationCode => {
  switch (target) {
    case 'u8':
      return 'ConvertToU8'
    case 'u16':
      return 'ConvertToU16'
    case 'u32':
      return 'ConvertToU32'
    case 'u64':
      return 'ConvertToU64'
    case 'usize':
      return 'ConvertToUsize'
    case 'i8':
      return 'ConvertToI8'
    case 'i16':
      return 'ConvertToI16'
    case 'i32':
      return 'ConvertToI32'
    case 'i64':
      return 'ConvertToI64'
    case 'isize':
      return 'ConvertToIsize'
  }
}

const conversionOperations = Object.freeze(
  integerSpellings.flatMap((target) => [
    operation(conversionName(target), conversionCode(target), 1, target),
    operation(
      `checked${conversionName(target)[0]?.toUpperCase() ?? ''}${conversionName(target).slice(1)}`,
      `Checked${conversionCode(target)}` as CheckedConversionOperationCode,
      1,
      'OptionTarget',
    ),
  ]),
)

const comparisonOperations = Object.freeze([
  ...equalityOperations,
  operation('lessThan', 'LessThan', 2, 'Boolean'),
  operation('lessOrEqual', 'LessOrEqual', 2, 'Boolean'),
  operation('greaterThan', 'GreaterThan', 2, 'Boolean'),
  operation('greaterOrEqual', 'GreaterOrEqual', 2, 'Boolean'),
])

const arithmeticOperations = Object.freeze([
  operation('add', 'Add', 2, 'Self'),
  operation('subtract', 'Subtract', 2, 'Self'),
  operation('multiply', 'Multiply', 2, 'Self'),
  operation('divide', 'Divide', 2, 'Self'),
  operation('remainder', 'Remainder', 2, 'Self'),
  operation('bitAnd', 'BitAnd', 2, 'Self'),
  operation('bitOr', 'BitOr', 2, 'Self'),
  operation('bitXor', 'BitXor', 2, 'Self'),
  operation('bitNot', 'BitNot', 1, 'Self'),
  operation('shiftLeft', 'ShiftLeft', 2, 'Self'),
  operation('shiftRight', 'ShiftRight', 2, 'Self'),
  operation('rotateLeft', 'RotateLeft', 2, 'Self'),
  operation('rotateRight', 'RotateRight', 2, 'Self'),
  operation('wrappingAdd', 'WrappingAdd', 2, 'Self'),
  operation('wrappingSubtract', 'WrappingSubtract', 2, 'Self'),
  operation('wrappingMultiply', 'WrappingMultiply', 2, 'Self'),
  operation('saturatingAdd', 'SaturatingAdd', 2, 'Self'),
  operation('saturatingSubtract', 'SaturatingSubtract', 2, 'Self'),
  operation('saturatingMultiply', 'SaturatingMultiply', 2, 'Self'),
  operation('checkedAdd', 'CheckedAdd', 2, 'OptionSelf'),
  operation('checkedSubtract', 'CheckedSubtract', 2, 'OptionSelf'),
  operation('checkedMultiply', 'CheckedMultiply', 2, 'OptionSelf'),
  operation('checkedDivide', 'CheckedDivide', 2, 'OptionSelf'),
  operation('checkedRemainder', 'CheckedRemainder', 2, 'OptionSelf'),
  ...comparisonOperations,
])

const integer = <const S extends IntegerSpelling>(
  spelling: S,
  signedness: IntegerScalar['signedness'],
  width: Width,
  layout: Layout,
  wasm: BackendLanes['wasm'],
): IntegerScalar & { readonly spelling: S } =>
  Object.freeze({
    spelling,
    category: 'Integer',
    width,
    signedness,
    layout,
    lanes: Object.freeze({ llvm: 'LogicalWidth', wasm }),
    operations:
      signedness === 'Signed'
        ? Object.freeze([
            operation('negate', 'Negate', 1, 'Self'),
            operation('wrappingNegate', 'WrappingNegate', 1, 'Self'),
            operation('saturatingNegate', 'SaturatingNegate', 1, 'Self'),
            ...conversionOperations,
            ...arithmeticOperations,
          ])
        : Object.freeze([...conversionOperations, ...arithmeticOperations]),
  })

const u8 = integer('u8', 'Unsigned', fixedWidth(8), fixedLayout(1), 'I32')
const u16 = integer('u16', 'Unsigned', fixedWidth(16), fixedLayout(2), 'I32')
const u32 = integer('u32', 'Unsigned', fixedWidth(32), fixedLayout(4), 'I32')
const u64 = integer('u64', 'Unsigned', fixedWidth(64), fixedLayout(8), 'I64')

/** The target-sized unsigned integer used by addresses and allocation contracts. */
export const pointerInteger = integer('usize', 'Unsigned', pointerWidth, pointerLayout, 'Pointer')

const i8 = integer('i8', 'Signed', fixedWidth(8), fixedLayout(1), 'I32')
const i16 = integer('i16', 'Signed', fixedWidth(16), fixedLayout(2), 'I32')

/** The default integer selected for an unconstrained integer expression. */
export const defaultInteger = integer('i32', 'Signed', fixedWidth(32), fixedLayout(4), 'I32')

const i64 = integer('i64', 'Signed', fixedWidth(64), fixedLayout(8), 'I64')
const isize = integer('isize', 'Signed', pointerWidth, pointerLayout, 'Pointer')

/** The canonical Boolean scalar. */
export const boolean: BooleanScalar = Object.freeze({
  spelling: 'bool',
  category: 'Boolean',
  width: fixedWidth(32),
  signedness: undefined,
  layout: fixedLayout(4),
  lanes: Object.freeze({ llvm: 'LogicalWidth', wasm: 'I32' }),
  operations: Object.freeze([...equalityOperations, operation('not', 'Not', 1, 'Self')]),
})

const catalog: ReadonlyArray<Scalar> = Object.freeze([
  boolean,
  u8,
  u16,
  u32,
  u64,
  pointerInteger,
  i8,
  i16,
  defaultInteger,
  i64,
  isize,
])

/** Returns every scalar in stable source-presentation order. */
export const all = (): ReadonlyArray<Scalar> => catalog

/** Returns every integer in stable source-presentation order. */
export const integers = (): ReadonlyArray<IntegerScalar> =>
  catalog.filter((candidate): candidate is IntegerScalar => candidate.category === 'Integer')

/** Finds a scalar by its accepted source spelling. */
export const find = (spelling: string): Scalar | undefined =>
  catalog.find((candidate) => candidate.spelling === spelling)

/** Tests whether an unknown value is an accepted scalar spelling. */
export const isSpelling = (value: unknown): value is Spelling =>
  typeof value === 'string' && find(value) !== undefined

/** Tests whether an unknown value is an accepted integer spelling. */
export const isIntegerSpelling = (value: unknown): value is IntegerSpelling =>
  typeof value === 'string' && find(value)?.category === 'Integer'

/** Resolves the destination of one explicit integer conversion operation. */
export const conversionTarget = (operation: string): IntegerScalar | undefined => {
  const prefix = operation.startsWith('CheckedConvertTo')
    ? 'CheckedConvertTo'
    : operation.startsWith('ConvertTo')
      ? 'ConvertTo'
      : undefined
  if (prefix === undefined) return undefined
  const suffix = operation.slice(prefix.length)
  const spelling = `${suffix[0]?.toLowerCase() ?? ''}${suffix.slice(1)}`
  const target = find(spelling)
  return target?.category === 'Integer' ? target : undefined
}

/** Tests whether an integer operation returns the canonical recoverable Option outcome. */
export const isCheckedOperation = (operation: string): boolean => operation.startsWith('Checked')

/** Resolves the scalar's logical width for one target pointer width. */
export const bits = (self: Scalar, pointerBits: 32 | 64): FixedBits =>
  self.width._tag === 'PointerWidth' ? pointerBits : self.width.bits

/** Resolves the scalar's size and alignment for one target pointer layout. */
export const resolveLayout = (
  self: Scalar,
  pointerSize: 4 | 8,
  pointerAlignment: 4 | 8,
): { readonly size: ByteWidth; readonly alignment: ByteWidth } =>
  self.layout._tag === 'PointerLayout'
    ? Object.freeze({ size: pointerSize, alignment: pointerAlignment })
    : Object.freeze({ size: self.layout.size, alignment: self.layout.alignment })

/** Returns the inclusive exact range of one integer for a selected target. */
export const range = (
  self: IntegerScalar,
  pointerBits: 32 | 64,
): { readonly minimum: bigint; readonly maximum: bigint } => {
  const width = BigInt(bits(self, pointerBits))
  return self.signedness === 'Signed'
    ? Object.freeze({ minimum: -(1n << (width - 1n)), maximum: (1n << (width - 1n)) - 1n })
    : Object.freeze({ minimum: 0n, maximum: (1n << width) - 1n })
}
