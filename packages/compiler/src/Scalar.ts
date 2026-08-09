/** The executable scalar spellings admitted by the current bootstrap compiler. */
export type Spelling = 'I32' | 'Usize' | 'Bool'

/** Whether a scalar width is fixed by its identity or selected by the compilation target. */
export type Width =
  | { readonly _tag: 'FixedWidth'; readonly bits: 32 }
  | { readonly _tag: 'PointerWidth' }

/** How a scalar occupies storage before a concrete target is selected. */
export type Layout =
  | { readonly _tag: 'FixedLayout'; readonly size: 4; readonly alignment: 4 }
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
  | 'Not'

/** One operation declared by a scalar actor. */
export interface Operation {
  readonly spelling: string
  readonly code: OperationCode
  readonly arity: 1 | 2
  readonly result: 'Self' | 'Boolean'
}

/** The backend lane selected from a scalar's logical width. */
export interface BackendLanes {
  readonly llvm: 'LogicalWidth'
  readonly wasm: 'I32' | 'Pointer'
}

type FixedWidth = Extract<Width, { readonly _tag: 'FixedWidth' }>
type PointerWidth = Extract<Width, { readonly _tag: 'PointerWidth' }>
type FixedLayout = Extract<Layout, { readonly _tag: 'FixedLayout' }>
type PointerLayout = Extract<Layout, { readonly _tag: 'PointerLayout' }>

/** One immutable source of truth for a compiler-known scalar. */
export type Scalar =
  | {
      readonly spelling: 'Bool'
      readonly category: 'Boolean'
      readonly width: FixedWidth
      readonly signedness: undefined
      readonly layout: FixedLayout
      readonly lanes: BackendLanes
      readonly operations: ReadonlyArray<Operation>
    }
  | {
      readonly spelling: 'I32'
      readonly category: 'Integer'
      readonly width: FixedWidth
      readonly signedness: 'Signed'
      readonly layout: FixedLayout
      readonly lanes: BackendLanes
      readonly operations: ReadonlyArray<Operation>
    }
  | {
      readonly spelling: 'Usize'
      readonly category: 'Integer'
      readonly width: PointerWidth
      readonly signedness: 'Unsigned'
      readonly layout: PointerLayout
      readonly lanes: BackendLanes
      readonly operations: ReadonlyArray<Operation>
    }

const fixed32: FixedWidth = Object.freeze({ _tag: 'FixedWidth', bits: 32 })
const pointerWidth: PointerWidth = Object.freeze({ _tag: 'PointerWidth' })
const fixed32Layout: FixedLayout = Object.freeze({
  _tag: 'FixedLayout',
  size: 4,
  alignment: 4,
})
const pointerLayout: PointerLayout = Object.freeze({ _tag: 'PointerLayout' })
const logicalI32Lane: BackendLanes = Object.freeze({ llvm: 'LogicalWidth', wasm: 'I32' })
const logicalPointerLane: BackendLanes = Object.freeze({ llvm: 'LogicalWidth', wasm: 'Pointer' })

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

const comparisonOperations = Object.freeze([
  ...equalityOperations,
  operation('lessThan', 'LessThan', 2, 'Boolean'),
  operation('lessOrEqual', 'LessOrEqual', 2, 'Boolean'),
  operation('greaterThan', 'GreaterThan', 2, 'Boolean'),
  operation('greaterOrEqual', 'GreaterOrEqual', 2, 'Boolean'),
])

/** The default integer selected for an unconstrained integer expression. */
export const defaultInteger: Extract<Scalar, { readonly spelling: 'I32' }> = Object.freeze({
  spelling: 'I32',
  category: 'Integer',
  width: fixed32,
  signedness: 'Signed',
  layout: fixed32Layout,
  lanes: logicalI32Lane,
  operations: Object.freeze([
    operation('negate', 'Negate', 1, 'Self'),
    operation('add', 'Add', 2, 'Self'),
    operation('subtract', 'Subtract', 2, 'Self'),
    operation('multiply', 'Multiply', 2, 'Self'),
    operation('divide', 'Divide', 2, 'Self'),
    operation('remainder', 'Remainder', 2, 'Self'),
    ...comparisonOperations,
  ]),
})

/** The target-sized unsigned integer used by addresses and allocation contracts. */
export const pointerInteger: Extract<Scalar, { readonly spelling: 'Usize' }> = Object.freeze({
  spelling: 'Usize',
  category: 'Integer',
  width: pointerWidth,
  signedness: 'Unsigned',
  layout: pointerLayout,
  lanes: logicalPointerLane,
  operations: Object.freeze([
    operation('add', 'Add', 2, 'Self'),
    operation('subtract', 'Subtract', 2, 'Self'),
    operation('multiply', 'Multiply', 2, 'Self'),
    operation('divide', 'Divide', 2, 'Self'),
    operation('remainder', 'Remainder', 2, 'Self'),
    ...comparisonOperations,
  ]),
})

/** The canonical Boolean scalar. */
export const boolean: Extract<Scalar, { readonly spelling: 'Bool' }> = Object.freeze({
  spelling: 'Bool',
  category: 'Boolean',
  width: fixed32,
  signedness: undefined,
  layout: fixed32Layout,
  lanes: logicalI32Lane,
  operations: Object.freeze([...equalityOperations, operation('not', 'Not', 1, 'Self')]),
})

const catalog: ReadonlyArray<Scalar> = Object.freeze([defaultInteger, pointerInteger, boolean])

/** Returns every scalar in stable source-presentation order. */
export const all = (): ReadonlyArray<Scalar> => catalog

/** Finds a scalar by its accepted source spelling. */
export const find = (spelling: string): Scalar | undefined =>
  catalog.find((candidate) => candidate.spelling === spelling)

/** Tests whether an unknown value is an accepted scalar spelling. */
export const isSpelling = (value: unknown): value is Spelling =>
  typeof value === 'string' && find(value) !== undefined

/** Resolves the scalar's logical width for one target pointer width. */
export const bits = (self: Scalar, pointerBits: 32 | 64): 32 | 64 =>
  self.width._tag === 'PointerWidth' ? pointerBits : self.width.bits

/** Resolves the scalar's size and alignment for one target pointer layout. */
export const resolveLayout = (
  self: Scalar,
  pointerSize: 4 | 8,
  pointerAlignment: 4 | 8,
): { readonly size: 4 | 8; readonly alignment: 4 | 8 } =>
  self.layout._tag === 'PointerLayout'
    ? Object.freeze({ size: pointerSize, alignment: pointerAlignment })
    : Object.freeze({ size: self.layout.size, alignment: self.layout.alignment })
