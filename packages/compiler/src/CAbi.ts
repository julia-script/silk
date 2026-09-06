import * as ForeignContract from './ForeignContract.js'
/**
 * The V1 C ABI admission relation and target-aware classification for foreign function
 * signatures. Admission is judged on type spelling alone so a foreign header is accepted or
 * rejected once per module; classification resolves pointer-width integers for one target.
 */
import * as Target from './Target.js'
import * as Type from './Type.js'

/** One C-classified scalar exchanged by value across a foreign call. */
export type CAbiType =
  | { readonly _tag: 'Void' }
  | {
      readonly _tag: 'Integer'
      readonly bits: 8 | 16 | 32 | 64
      readonly signed: boolean
      readonly extension: 'None' | 'Sign' | 'Zero'
    }
  | { readonly _tag: 'Float'; readonly bits: 32 | 64 }
  /**
   * The C pointer class. Admission never examines the pointee (opaque pointers are admitted); it
   * is carried so a call site can be checked against the declared pointee.
   */
  | { readonly _tag: 'Pointer'; readonly type: Type.Pointer }
  | {
      readonly _tag: 'FunctionPointer'
      readonly nullable: false
      readonly contract: ForeignContract.ForeignContract
      readonly parameters: ReadonlyArray<CAbiType>
      readonly result: CAbiType
    }

/** The classified C signature that identifies one foreign symbol within an executable. */
export interface CAbiSignature {
  readonly contract: ForeignContract.ForeignContract
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
  | `pointer<${string}>`
  | `extern "C" fn(${string})->${string}`

export type Position = 'Parameter' | 'Result'

export type Admission =
  | { readonly _tag: 'Admitted'; readonly type: Type.Type }
  | { readonly _tag: 'NotAdmitted'; readonly type: Type.Type; readonly position: Position }

const void_: CAbiType = Object.freeze({ _tag: 'Void' })

const integer = (bits: 8 | 16 | 32 | 64, signed: boolean, target: Target.Target): CAbiType => {
  // Apple ARM64 and System V x86-64 extend narrow C integers to 32 bits. AAPCS64 does not.
  // The pinned authorities and independent Clang fixtures live in native-boundary conformance.
  const extended =
    bits < 32 && (target.id === 'aarch64-apple-darwin' || target.id === 'x86_64-unknown-linux-gnu')
  const extension = signed ? 'Sign' : 'Zero'
  return Object.freeze({
    _tag: 'Integer',
    bits,
    signed,
    extension: extended ? extension : 'None',
  })
}

const float = (bits: 32 | 64): CAbiType => Object.freeze({ _tag: 'Float', bits })

/** The closed scalar switch; `bool` and `char` are deliberately outside the C subset. */
const physicalBits = (primitive: Target.Primitive): 8 | 16 | 32 | 64 => {
  switch (primitive.size) {
    case 1:
      return 8
    case 2:
      return 16
    case 4:
      return 32
    case 8:
      return 64
    default:
      throw new RangeError('Unsupported audited scalar width')
  }
}

const scalar = (spelling: Type.Builtin, target: Target.Target): CAbiType | undefined => {
  switch (spelling) {
    case 'i8':
      return integer(physicalBits(target.primitives.i8), true, target)
    case 'u8':
      return integer(physicalBits(target.primitives.i8), false, target)
    case 'i16':
      return integer(physicalBits(target.primitives.i16), true, target)
    case 'u16':
      return integer(physicalBits(target.primitives.i16), false, target)
    case 'i32':
      return integer(physicalBits(target.primitives.i32), true, target)
    case 'u32':
      return integer(physicalBits(target.primitives.i32), false, target)
    case 'i64':
      return integer(physicalBits(target.primitives.i64), true, target)
    case 'u64':
      return integer(physicalBits(target.primitives.i64), false, target)
    case 'isize':
      return integer(pointerBits(target), true, target)
    case 'usize':
      return integer(pointerBits(target), false, target)
    case 'f32':
      return float(target.primitives.f32.size === 4 ? 32 : 64)
    case 'f64':
      return float(target.primitives.f64.size === 4 ? 32 : 64)
    case 'bool':
    case 'char':
      return undefined
  }
}

const classifyOrUndefined = (
  type: Type.Type,
  position: Position,
  target: Target.Target,
): CAbiType | undefined => {
  if (Type.isBuiltin(type)) return scalar(type, target)
  if (Type.isPointer(type))
    return admittedType(type, position) ? Object.freeze({ _tag: 'Pointer', type }) : undefined
  if (Type.isForeignFunction(type)) {
    if (!admittedType(type, position)) return undefined
    const classified = signature(type.parameters, type.result, target, type.contract)
    return Object.freeze({ _tag: 'FunctionPointer', nullable: false, ...classified })
  }
  if (position === 'Result' && Type.equals(type, Type.unit)) return void_
  return undefined
}

const admittedType = (type: Type.Type, position: Position): boolean => {
  if (Type.isBuiltin(type)) return type !== 'bool' && type !== 'char'
  if (Type.isPointer(type))
    return (
      type.addressSpace === 0 &&
      (type.alignment === 'Natural' || Type.isPointerAlignment(type.alignment))
    )
  if (Type.isForeignFunction(type))
    return (
      type.nullable === false &&
      type.parameters.every((parameter, ordinal) =>
        Type.isReference(parameter)
          ? type.contract.borrow.includes(ordinal)
          : admittedType(parameter, 'Parameter'),
      ) &&
      admittedType(type.result, 'Result')
    )
  return position === 'Result' && Type.equals(type, Type.unit)
}

/** Target-independent admission of a parameter or result into the scalar C subset. */
export const admit = (type: Type.Type, position: Position): Admission =>
  admittedType(type, position)
    ? Object.freeze({ _tag: 'Admitted', type })
    : Object.freeze({ _tag: 'NotAdmitted', type, position })

const pointerBits = (target: Target.Target): 32 | 64 => (target.pointerSize === 4 ? 32 : 64)

/** Classifies one admitted type for the selected target. Throws on a type `admit` rejects. */
export const classify = (type: Type.Type, target: Target.Target, position: Position): CAbiType => {
  if (!Target.isCanonical(target))
    throw new RangeError('C ABI classification requires an audited target description')
  const classified = classifyOrUndefined(type, position, target)
  if (classified === undefined)
    throw new RangeError(`${Type.encode(type)} is not admitted by the C ABI as a ${position}`)
  return classified
}

export const signature = (
  parameters: ReadonlyArray<Type.Type>,
  result: Type.Type,
  target: Target.Target,
  contract: ForeignContract.ForeignContract = ForeignContract.conservative,
): CAbiSignature =>
  Object.freeze({
    contract,
    parameters: Object.freeze(
      parameters.map((type, ordinal) =>
        classify(
          contract.borrow.includes(ordinal) && Type.isReference(type)
            ? Type.pointer({
                mutable: type.access === 'Exclusive',
                pointee: type.target,
                nullable: false,
                extent: 'Single',
                alignment: 'Natural',
                addressSpace: 0,
              })
            : type,
          target,
          'Parameter',
        ),
      ),
    ),
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
      return `pointer<${self.type.mutable ? 'mut' : 'const'};${encodeURIComponent(Type.runtimeKey(self.type)).replaceAll(/[!'()*]/g, (character) => `%${character.charCodeAt(0).toString(16).toUpperCase()}`)}>`
    case 'FunctionPointer':
      return `extern "C" fn(${self.parameters.map(typeText).join(',')})->${typeText(self.result)}!nonnull:${ForeignContract.key(self.contract).replaceAll(',', '.')}`
  }
}

/** Verifies target-dependent facts carried by a classified C value before native lowering. */
export const isCanonical = (self: CAbiType, target: Target.Target): boolean => {
  if (!Target.isCanonical(target)) return false
  switch (self._tag) {
    case 'Void':
      return true
    case 'Pointer':
      return admittedType(self.type, 'Parameter')
    case 'Float':
      return self.bits === 32 || self.bits === 64
    case 'Integer': {
      const expected = integer(self.bits, self.signed, target)
      return expected._tag === 'Integer' && self.extension === expected.extension
    }
    case 'FunctionPointer':
      return self.nullable === false && isCanonicalSignature(self, target)
  }
}

/** The canonical identity two declarations of one symbol must share, e.g. `(i32,u64)->f64`. */
export const signatureKey = (self: CAbiSignature): string =>
  `(${self.parameters.map(typeText).join(',')})->${typeText(self.result)}!${ForeignContract.key(self.contract)}`

/** Decoded public ABI type spelling; pointer identity remains opaque but participates in equality. */
export type TextShape =
  | { readonly _tag: 'Scalar'; readonly type: TypeText }
  | { readonly _tag: 'Pointer'; readonly mutable: boolean }
  | {
      readonly _tag: 'FunctionPointer'
      readonly contract: ForeignContract.ForeignContract
      readonly parameters: ReadonlyArray<TextShape>
      readonly result: TextShape
    }

/** Inspects the finite ABI spelling grammar without evaluating source or accepting C syntax. */
export const inspectText = (text: string, depth = 0): TextShape | undefined => {
  if (depth > 64) return undefined
  switch (text) {
    case 'void':
    case 'i8':
    case 'u8':
    case 'i16':
    case 'u16':
    case 'i32':
    case 'u32':
    case 'i64':
    case 'u64':
    case 'f32':
    case 'f64':
      return Object.freeze({ _tag: 'Scalar', type: text })
  }
  const pointer = /^pointer<(mut|const);(?:[A-Za-z0-9_.~-]|%[0-9A-F]{2})+>$/.exec(text)
  if (pointer !== null) return Object.freeze({ _tag: 'Pointer', mutable: pointer[1] === 'mut' })
  const prefix = 'extern "C" fn('
  if (!text.startsWith(prefix)) return undefined
  const contractSeparator = text.lastIndexOf('!nonnull:')
  if (contractSeparator < 0) return undefined
  const encodedContract = text.slice(contractSeparator + '!nonnull:'.length)
  text = text.slice(0, contractSeparator)
  const parameters: Array<TextShape> = []
  const parameterTexts: Array<string> = []
  let nesting = 1
  let start = prefix.length
  for (let index = start; index < text.length; index += 1) {
    if (text[index] === '(') nesting += 1
    if (text[index] === ')') nesting -= 1
    if (nesting === 0 || (nesting === 1 && text[index] === ',')) {
      if (start !== index || nesting !== 0 || parameters.length > 0) {
        const parameter = inspectText(text.slice(start, index), depth + 1)
        if (parameter === undefined || (parameter._tag === 'Scalar' && parameter.type === 'void'))
          return undefined
        parameters.push(parameter)
        parameterTexts.push(text.slice(start, index))
      }
      start = index + 1
    }
    if (nesting === 0) {
      if (text.slice(index, index + 3) !== ')->') return undefined
      const resultText = text.slice(index + 3)
      const result = inspectText(resultText, depth + 1)
      const contract = ForeignContract.inspectKey(encodedContract, parameterTexts, resultText)
      return result === undefined ||
        contract === undefined ||
        parameters.some(
          (parameter) =>
            parameter._tag === 'FunctionPointer' &&
            !ForeignContract.callbackAccessAdmitted(contract, parameter.contract),
        )
        ? undefined
        : Object.freeze({
            _tag: 'FunctionPointer',
            contract,
            parameters: Object.freeze(parameters),
            result,
          })
    }
  }
  return undefined
}

/** Narrows external data only after the canonical ABI type grammar accepts it. */
export const isTypeText = (input: unknown): input is TypeText =>
  typeof input === 'string' && inspectText(input) !== undefined

/** Checks both target machine facts and canonical behavior before trusting a transported signature. */
export const isCanonicalSignature = (self: CAbiSignature, target: Target.Target): boolean =>
  isCanonical(self.result, target) &&
  self.parameters.every((type) => type._tag !== 'Void' && isCanonical(type, target)) &&
  ForeignContract.inspect(self.contract, self.parameters.map(typeText), typeText(self.result)) !==
    undefined &&
  self.parameters.every(
    (parameter) =>
      parameter._tag !== 'FunctionPointer' ||
      ForeignContract.callbackAccessAdmitted(self.contract, parameter.contract),
  ) &&
  self.contract.borrow.every((ordinal) => {
    const parameter = self.parameters[ordinal]
    return (
      parameter?._tag === 'Pointer' &&
      parameter.type.extent === 'Single' &&
      !parameter.type.nullable &&
      parameter.type.alignment === 'Natural' &&
      parameter.type.addressSpace === 0
    )
  })
