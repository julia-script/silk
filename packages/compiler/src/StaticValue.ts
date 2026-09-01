import type * as DeclarationFacts from './DeclarationFacts.js'
import * as FloatingPoint from './FloatingPoint.js'
import * as Canonical from './internal/Canonical.js'
import * as Scalar from './Scalar.js'
import type * as Type from './Type.js'

/** One finite, immutable value admitted to compiler-owned static evaluation. */
export type Value =
  | UnitValue
  | BooleanValue
  | CharacterValue
  | IntegerValue
  | FloatValue
  | EnumValue
  | TextValue
  | AggregateValue

export interface UnitValue {
  readonly _tag: 'UnitValue'
}

export interface BooleanValue {
  readonly _tag: 'BooleanValue'
  readonly value: boolean
}

export interface CharacterValue {
  readonly _tag: 'CharacterValue'
  readonly value: number
}

export interface IntegerValue {
  readonly _tag: 'IntegerValue'
  readonly type: Scalar.IntegerSpelling
  readonly value: bigint
}

export interface FloatValue {
  readonly _tag: 'FloatValue'
  readonly type: Scalar.FloatSpelling
  readonly bits: bigint
}

export interface EnumValue {
  readonly _tag: 'EnumValue'
  readonly type: DeclarationFacts.CanonicalId
  readonly member: string
  readonly representation: Scalar.EnumRepresentationSpelling
  readonly discriminant: bigint
}

export interface TextValue {
  readonly _tag: 'TextValue'
  readonly bytes: ReadonlyArray<number>
}

/** The declaration-independent identity needed to distinguish canonical aggregate values. */
export type AggregateIdentity = NominalAggregateIdentity | ArrayAggregateIdentity

export interface NominalAggregateIdentity {
  readonly _tag: 'NominalAggregateIdentity'
  readonly declaration: DeclarationFacts.CanonicalId
  /** Canonical generic-argument encodings in declared order. */
  readonly typeArguments: ReadonlyArray<string>
  readonly variant?: {
    readonly ordinal: number
    readonly name: string
  }
}

export interface ArrayAggregateIdentity {
  readonly _tag: 'ArrayAggregateIdentity'
  /** Canonical semantic type encoding for the repeated element. */
  readonly element: string
  readonly length: number
}

export interface AggregateField {
  readonly ordinal: number
  readonly value: Value
}

export interface AggregateValue {
  readonly _tag: 'AggregateValue'
  readonly identity: AggregateIdentity
  readonly fields: ReadonlyArray<AggregateField>
  /** Snapshot-local field identities used only when embedding the canonical value into HIR. */
  readonly runtimeFields?: ReadonlyArray<{
    readonly id: DeclarationFacts.FieldId
    readonly type: Type.Type
  }>
}

export type RejectionReason =
  | 'UnsupportedValue'
  | 'InvalidCharacter'
  | 'InvalidInteger'
  | 'InvalidFloat'
  | 'InvalidEnum'
  | 'InvalidText'
  | 'InvalidAggregate'
  | 'CyclicValue'

/** A closed admission result; rejected candidates never become evaluator values. */
export type Admission =
  | { readonly _tag: 'Admitted'; readonly value: Value }
  | {
      readonly _tag: 'Rejected'
      readonly reason: RejectionReason
      readonly path: ReadonlyArray<number>
      readonly detail: string
    }

export interface AdmissionOptions {
  /** Concrete target width used only to validate `usize` and `isize` values. */
  readonly pointerBits: 32 | 64
}

const unitValue: UnitValue = Object.freeze({ _tag: 'UnitValue' })

/** The canonical static unit value. */
export const unit = (): UnitValue => unitValue

/** Constructs one canonical static Boolean. */
export const boolean = (value: boolean): BooleanValue =>
  Object.freeze({ _tag: 'BooleanValue', value })

const rejected = (
  reason: RejectionReason,
  detail: string,
  path: ReadonlyArray<number> = Object.freeze([]),
): Admission => Object.freeze({ _tag: 'Rejected', reason, path: Object.freeze([...path]), detail })

const admitted = (value: Value): Admission => Object.freeze({ _tag: 'Admitted', value })

const isRecord = (value: unknown): value is Readonly<Record<string, unknown>> =>
  typeof value === 'object' && value !== null && !Array.isArray(value)

const canonicalId = (value: unknown): DeclarationFacts.CanonicalId | undefined => {
  if (
    !isRecord(value) ||
    value._tag !== 'CanonicalDeclarationId' ||
    typeof value.module !== 'string' ||
    typeof value.name !== 'string'
  )
    return undefined
  return Object.freeze({
    _tag: 'CanonicalDeclarationId',
    module: value.module,
    name: value.name,
  })
}

const isEnumRepresentation = (value: unknown): value is Scalar.EnumRepresentationSpelling =>
  value === 'u8' ||
  value === 'u16' ||
  value === 'u32' ||
  value === 'u64' ||
  value === 'i8' ||
  value === 'i16' ||
  value === 'i32' ||
  value === 'i64'

const integerValue = (
  type: Scalar.IntegerSpelling,
  value: bigint,
  pointerBits: 32 | 64,
): Admission => {
  const scalar = Scalar.find(type)
  if (scalar?.category !== 'Integer')
    return rejected('InvalidInteger', `unknown integer scalar ${type}`)
  const range = Scalar.range(scalar, pointerBits)
  return value < range.minimum || value > range.maximum
    ? rejected('InvalidInteger', `${value.toString()} is outside ${type}`)
    : admitted(Object.freeze({ _tag: 'IntegerValue', type, value }))
}

const floatValue = (type: Scalar.FloatSpelling, bits: bigint): Admission => {
  const width = type === 'f32' ? 32 : 64
  if (bits < 0n || bits >= 1n << BigInt(width))
    return rejected('InvalidFloat', `bits are outside ${type}`)
  const value = Object.freeze({ width, bits })
  return admitted(
    Object.freeze({
      _tag: 'FloatValue',
      type,
      bits: FloatingPoint.isNotANumber(value) ? FloatingPoint.canonicalNaN(width) : bits,
    }),
  )
}

const textValue = (bytes: unknown): Admission => {
  if (
    !Array.isArray(bytes) ||
    bytes.some((byte) => !Number.isInteger(byte) || byte < 0 || byte > 0xff)
  )
    return rejected('InvalidText', 'static text must contain only bytes')
  const canonicalBytes = Object.freeze([...bytes])
  try {
    new TextDecoder('utf-8', { fatal: true }).decode(Uint8Array.from(canonicalBytes))
  } catch {
    return rejected('InvalidText', 'static text must be valid UTF-8')
  }
  return admitted(Object.freeze({ _tag: 'TextValue', bytes: canonicalBytes }))
}

const aggregateIdentity = (value: unknown): AggregateIdentity | undefined => {
  if (!isRecord(value)) return undefined
  if (value._tag === 'ArrayAggregateIdentity') {
    if (
      typeof value.element !== 'string' ||
      typeof value.length !== 'number' ||
      !Number.isSafeInteger(value.length) ||
      value.length < 0
    )
      return undefined
    return Object.freeze({
      _tag: 'ArrayAggregateIdentity',
      element: value.element,
      length: value.length,
    })
  }
  if (value._tag !== 'NominalAggregateIdentity') return undefined
  const declaration = canonicalId(value.declaration)
  if (
    declaration === undefined ||
    !Array.isArray(value.typeArguments) ||
    value.typeArguments.some((argument) => typeof argument !== 'string')
  )
    return undefined
  let variant: NominalAggregateIdentity['variant']
  if (value.variant !== undefined) {
    if (
      !isRecord(value.variant) ||
      typeof value.variant.ordinal !== 'number' ||
      !Number.isSafeInteger(value.variant.ordinal) ||
      value.variant.ordinal < 0 ||
      typeof value.variant.name !== 'string'
    )
      return undefined
    variant = Object.freeze({ ordinal: value.variant.ordinal, name: value.variant.name })
  }
  return Object.freeze({
    _tag: 'NominalAggregateIdentity',
    declaration,
    typeArguments: Object.freeze([...value.typeArguments]),
    ...(variant === undefined ? {} : { variant }),
  })
}

const admissionAt = (
  input: unknown,
  options: AdmissionOptions,
  path: ReadonlyArray<number>,
  active: WeakSet<object>,
): Admission => {
  if (!isRecord(input)) return rejected('UnsupportedValue', 'value is not static data', path)
  if (active.has(input)) return rejected('CyclicValue', 'static values must be finite', path)
  active.add(input)
  const result = (() => {
    switch (input._tag) {
      case 'UnitValue':
        return admitted(unitValue)
      case 'BooleanValue':
        return typeof input.value === 'boolean'
          ? admitted(boolean(input.value))
          : rejected('UnsupportedValue', 'Boolean value is unavailable', path)
      case 'CharacterValue':
        return typeof input.value === 'number' &&
          Number.isSafeInteger(input.value) &&
          Scalar.isUnicodeScalarValue(BigInt(input.value))
          ? admitted(Object.freeze({ _tag: 'CharacterValue', value: input.value }))
          : rejected('InvalidCharacter', 'character is not a Unicode scalar', path)
      case 'IntegerValue':
        return Scalar.isIntegerSpelling(input.type) && typeof input.value === 'bigint'
          ? integerValue(input.type, input.value, options.pointerBits)
          : rejected('InvalidInteger', 'integer type or value is unavailable', path)
      case 'FloatValue':
        return Scalar.isFloatSpelling(input.type) && typeof input.bits === 'bigint'
          ? floatValue(input.type, input.bits)
          : rejected('InvalidFloat', 'float type or bits are unavailable', path)
      case 'EnumValue': {
        const type = canonicalId(input.type)
        const representation = input.representation
        const scalar = Scalar.find(isEnumRepresentation(representation) ? representation : '')
        if (
          type === undefined ||
          typeof input.member !== 'string' ||
          !isEnumRepresentation(representation) ||
          scalar?.category !== 'Integer' ||
          scalar.width._tag !== 'FixedWidth' ||
          typeof input.discriminant !== 'bigint'
        )
          return rejected('InvalidEnum', 'enum identity or discriminant is unavailable', path)
        const range = Scalar.range(scalar, 64)
        if (input.discriminant < range.minimum || input.discriminant > range.maximum)
          return rejected('InvalidEnum', 'enum discriminant exceeds its representation', path)
        return admitted(
          Object.freeze({
            _tag: 'EnumValue',
            type,
            member: input.member,
            representation,
            discriminant: input.discriminant,
          }),
        )
      }
      case 'TextValue':
        return textValue(input.bytes)
      case 'AggregateValue': {
        const identity = aggregateIdentity(input.identity)
        if (identity === undefined || !Array.isArray(input.fields))
          return rejected('InvalidAggregate', 'aggregate identity or fields are unavailable', path)
        const fields: Array<AggregateField> = []
        const ordinals = new Set<number>()
        for (const field of input.fields) {
          if (
            !isRecord(field) ||
            typeof field.ordinal !== 'number' ||
            !Number.isSafeInteger(field.ordinal) ||
            field.ordinal < 0 ||
            ordinals.has(field.ordinal)
          )
            return rejected('InvalidAggregate', 'aggregate field ordinals must be unique', path)
          ordinals.add(field.ordinal)
          const nested = admissionAt(field.value, options, [...path, field.ordinal], active)
          if (nested._tag === 'Rejected') return nested
          fields.push(Object.freeze({ ordinal: field.ordinal, value: nested.value }))
        }
        fields.sort((left, right) => left.ordinal - right.ordinal)
        if (
          identity._tag === 'ArrayAggregateIdentity' &&
          (fields.length !== identity.length ||
            fields.some((field, ordinal) => field.ordinal !== ordinal))
        )
          return rejected(
            'InvalidAggregate',
            'array fields must exactly match the declared length',
            path,
          )
        return admitted(
          Object.freeze({
            _tag: 'AggregateValue',
            identity,
            fields: Object.freeze(fields),
          }),
        )
      }
      default:
        return rejected(
          'UnsupportedValue',
          `static evaluation does not admit ${String(input._tag)}`,
          path,
        )
    }
  })()
  active.delete(input)
  return result
}

/**
 * Rebuilds arbitrary candidate data as one canonical value, rejecting resources and identity.
 *
 * Runtime `Copy`, ownership, and cleanup are deliberately not consulted: admission describes only
 * the evaluator's closed value domain.
 */
export const admit = (input: unknown, options: AdmissionOptions): Admission =>
  admissionAt(input, options, Object.freeze([]), new WeakSet())

const identityEncoding = (identity: AggregateIdentity): string =>
  identity._tag === 'ArrayAggregateIdentity'
    ? Canonical.record('ArrayAggregateIdentity', [identity.element, String(identity.length)])
    : Canonical.record('NominalAggregateIdentity', [
        identity.declaration.module,
        identity.declaration.name,
        Canonical.array(identity.typeArguments),
        identity.variant === undefined
          ? Canonical.record('NoVariant')
          : Canonical.record('Variant', [String(identity.variant.ordinal), identity.variant.name]),
      ])

/** Encodes one static value without host identity or ambiguous concatenation. */
export const encode = (self: Value): string => {
  switch (self._tag) {
    case 'UnitValue':
      return Canonical.record('UnitValue')
    case 'BooleanValue':
      return Canonical.record('BooleanValue', [self.value ? 'true' : 'false'])
    case 'CharacterValue':
      return Canonical.record('CharacterValue', [String(self.value)])
    case 'IntegerValue':
      return Canonical.record('IntegerValue', [self.type, self.value.toString()])
    case 'FloatValue':
      return Canonical.record('FloatValue', [self.type, self.bits.toString(16)])
    case 'EnumValue':
      return Canonical.record('EnumValue', [
        self.type.module,
        self.type.name,
        self.member,
        self.representation,
        self.discriminant.toString(),
      ])
    case 'TextValue':
      return Canonical.record('TextValue', [
        self.bytes.map((byte) => byte.toString(16).padStart(2, '0')).join(''),
      ])
    case 'AggregateValue':
      return Canonical.record('AggregateValue', [
        identityEncoding(self.identity),
        Canonical.array(
          self.fields.map((field) =>
            Canonical.record('Field', [String(field.ordinal), encode(field.value)]),
          ),
        ),
      ])
  }
}

/** Returns the stable specialization-key component for one static value. */
export const key = encode

/** Tests static value equality without observing compiler object identity. */
export const equals = (left: Value, right: Value): boolean => key(left) === key(right)

const floatPresentation = (self: FloatValue): string => {
  const width = self.type === 'f32' ? 8 : 16
  return `${self.type}(bits=0x${self.bits.toString(16).padStart(width, '0')})`
}

const identityPresentation = (identity: AggregateIdentity): string => {
  if (identity._tag === 'ArrayAggregateIdentity')
    return `Array<${identity.element}, ${identity.length}>`
  const arguments_ =
    identity.typeArguments.length === 0 ? '' : `<${identity.typeArguments.join(', ')}>`
  const variant = identity.variant === undefined ? '' : `.${identity.variant.name}`
  return `${identity.declaration.module}.${identity.declaration.name}${arguments_}${variant}`
}

/** Presents one value for deterministic semantic facts and static traces. */
export const presentation = (self: Value): string => {
  switch (self._tag) {
    case 'UnitValue':
      return '()'
    case 'BooleanValue':
      return self.value ? 'true' : 'false'
    case 'CharacterValue':
      return `char(U+${self.value.toString(16).toUpperCase().padStart(4, '0')})`
    case 'IntegerValue':
      return `${self.value.toString()}${self.type}`
    case 'FloatValue':
      return floatPresentation(self)
    case 'EnumValue':
      return `${self.type.module}.${self.type.name}.${self.member}`
    case 'TextValue':
      return JSON.stringify(new TextDecoder().decode(Uint8Array.from(self.bytes)))
    case 'AggregateValue':
      return `${identityPresentation(self.identity)} { ${self.fields
        .map((field) => `#${field.ordinal}: ${presentation(field.value)}`)
        .join(', ')} }`
  }
}

const textEncoder = new TextEncoder()

/** Counts retained canonical bytes for deterministic evaluator budgeting. */
export const retainedSize = (self: Value): number => textEncoder.encode(encode(self)).byteLength
