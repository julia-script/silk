/**
 * The V1 C ABI admission relation and target-aware classification for foreign function
 * signatures. Admission is judged on type spelling alone so a foreign header is accepted or
 * rejected once per module; classification resolves pointer-width integers for one target.
 */
import type * as Target from './Target.js'
import * as Type from './Type.js'

/** One C-classified scalar exchanged by value across a foreign call. */
export type CAbiType =
  | { readonly _tag: 'Void' }
  | { readonly _tag: 'Integer'; readonly bits: 8 | 16 | 32 | 64; readonly signed: boolean }
  | { readonly _tag: 'Float'; readonly bits: 32 | 64 }
  /**
   * The C pointer class. Admission never examines the pointee (opaque pointers are admitted); it
   * is carried so a call site can be checked against the declared pointee.
   */
  | { readonly _tag: 'Pointer'; readonly mutable: boolean; readonly pointee: Type.Type }
  | {
      readonly _tag: 'FunctionPointer'
      readonly parameters: ReadonlyArray<CAbiType>
      readonly result: CAbiType
    }

/** The classified C signature that identifies one foreign symbol within an executable. */
export interface CAbiSignature {
  readonly parameters: ReadonlyArray<CAbiType>
  readonly result: CAbiType
}

/** Canonical public spelling of one classified C ABI type. */
export type TypeText =
  | 'void'
  | 'i8'
  | 'u8'
  | 'i16'
  | 'u16'
  | 'i32'
  | 'u32'
  | 'i64'
  | 'u64'
  | 'f32'
  | 'f64'
  | '*const'
  | '*mut'
  | `extern "C" fn(${string})->${string}`

export type Position = 'Parameter' | 'Result'

export type Admission =
  | { readonly _tag: 'Admitted'; readonly type: Type.Type }
  | { readonly _tag: 'NotAdmitted'; readonly type: Type.Type; readonly position: Position }

const void_: CAbiType = Object.freeze({ _tag: 'Void' })

const integer = (bits: 8 | 16 | 32 | 64, signed: boolean): CAbiType =>
  Object.freeze({ _tag: 'Integer', bits, signed })

const float = (bits: 32 | 64): CAbiType => Object.freeze({ _tag: 'Float', bits })

/** The closed scalar switch; `bool` and `char` are deliberately outside the C subset. */
const scalar = (spelling: Type.Builtin, pointerBits: 32 | 64): CAbiType | undefined => {
  switch (spelling) {
    case 'i8':
      return integer(8, true)
    case 'u8':
      return integer(8, false)
    case 'i16':
      return integer(16, true)
    case 'u16':
      return integer(16, false)
    case 'i32':
      return integer(32, true)
    case 'u32':
      return integer(32, false)
    case 'i64':
      return integer(64, true)
    case 'u64':
      return integer(64, false)
    case 'isize':
      return integer(pointerBits, true)
    case 'usize':
      return integer(pointerBits, false)
    case 'f32':
      return float(32)
    case 'f64':
      return float(64)
    case 'bool':
    case 'char':
      return undefined
  }
}

const classifyOrUndefined = (
  type: Type.Type,
  position: Position,
  pointerBits: 32 | 64,
): CAbiType | undefined => {
  if (Type.isBuiltin(type)) return scalar(type, pointerBits)
  if (Type.isPointer(type))
    return Object.freeze({ _tag: 'Pointer', mutable: type.mutable, pointee: type.pointee })
  if (Type.isForeignFunction(type)) {
    const parameters = type.parameters.map((parameter) =>
      classifyOrUndefined(parameter, 'Parameter', pointerBits),
    )
    const result = classifyOrUndefined(type.result, 'Result', pointerBits)
    if (parameters.some((parameter) => parameter === undefined) || result === undefined)
      return undefined
    return Object.freeze({
      _tag: 'FunctionPointer',
      parameters: Object.freeze(
        parameters.flatMap((parameter) => (parameter === undefined ? [] : [parameter])),
      ),
      result,
    })
  }
  if (position === 'Result' && Type.equals(type, Type.unit)) return void_
  return undefined
}

/** Target-independent admission of one parameter or result type into the V1 C subset. */
export const admit = (type: Type.Type, position: Position): Admission =>
  classifyOrUndefined(type, position, 64) === undefined
    ? Object.freeze({ _tag: 'NotAdmitted', type, position })
    : Object.freeze({ _tag: 'Admitted', type })

const pointerBits = (target: Target.Target): 32 | 64 => (target.pointerSize === 4 ? 32 : 64)

/** Classifies one admitted type for the selected target. Throws on a type `admit` rejects. */
export const classify = (type: Type.Type, target: Target.Target, position: Position): CAbiType => {
  const classified = classifyOrUndefined(type, position, pointerBits(target))
  if (classified === undefined)
    throw new RangeError(`${Type.encode(type)} is not admitted by the C ABI as a ${position}`)
  return classified
}

export const signature = (
  parameters: ReadonlyArray<Type.Type>,
  result: Type.Type,
  target: Target.Target,
): CAbiSignature =>
  Object.freeze({
    parameters: Object.freeze(parameters.map((type) => classify(type, target, 'Parameter'))),
    result: classify(result, target, 'Result'),
  })

/** The C spelling recorded on artifacts: `i32`, `u64`, `f32`, `f64`, or `void`. */
export const typeText = (self: CAbiType): TypeText => {
  switch (self._tag) {
    case 'Void':
      return 'void'
    case 'Integer':
      return `${self.signed ? 'i' : 'u'}${self.bits}`
    case 'Float':
      return `f${self.bits}`
    case 'Pointer':
      return self.mutable ? '*mut' : '*const'
    case 'FunctionPointer':
      return `extern "C" fn(${self.parameters.map(typeText).join(',')})->${typeText(self.result)}`
  }
}

/** The canonical identity two declarations of one symbol must share, e.g. `(i32,u64)->f64`. */
export const signatureKey = (self: CAbiSignature): string =>
  `(${self.parameters.map(typeText).join(',')})->${typeText(self.result)}`
