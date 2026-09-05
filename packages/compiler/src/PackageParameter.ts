import * as Effect from 'effect/Effect'
import * as ConfigurationError from './ConfigurationError.js'
import type * as ConfigurationOrigin from './ConfigurationOrigin.js'
import * as ConfigurationValue from './ConfigurationValue.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as TypeInference from './internal/TypeInference.js'
import * as Scalar from './Scalar.js'
import * as StaticValue from './StaticValue.js'
import * as Target from './Target.js'
import * as Type from './Type.js'

/** A field of a package-owned serializable record, with its substituted concrete schema. */
export interface Field {
  readonly name: string
  readonly id: DeclarationFacts.FieldId
  readonly schema: Schema
}

/** A projection of ordinary Silk types into the admitted configuration value domain. */
export type Schema = { readonly type: Type.Type } & (
  | { readonly kind: 'boolean' }
  | { readonly kind: 'integer'; readonly scalar: Scalar.IntegerSpelling }
  | { readonly kind: 'string' }
  | {
      readonly kind: 'enum'
      readonly name: string
      readonly declaration: DeclarationFacts.EnumFact
    }
  | { readonly kind: 'array'; readonly length: number; readonly element: Schema }
  | {
      readonly kind: 'record'
      readonly identity: StaticValue.NominalAggregateIdentity
      readonly fields: ReadonlyArray<Field>
    }
  | {
      readonly kind: 'optional'
      readonly absent: StaticValue.NominalAggregateIdentity
      readonly present: StaticValue.NominalAggregateIdentity
      readonly field: Field
    }
)

/** All source ownership comes from the source/package graph, never library spellings. */
export interface Context {
  readonly index: DeclarationIndex.Index
  readonly target: Target.Target
  readonly packages: ReadonlyMap<string, { readonly package: string; readonly module: string }>
}

const invalid = (origin: ConfigurationOrigin.ConfigurationOrigin, subject = 'parameter type') =>
  ConfigurationError.make('PackageParameter', 'InvalidType', subject, [origin])

const describeFields = Effect.fnUntraced(function* (
  context: Context,
  fields: ReadonlyArray<DeclarationFacts.FieldFact>,
  substitution: Type.Substitution,
  origin: ConfigurationOrigin.ConfigurationOrigin,
  active: ReadonlySet<string>,
): Effect.fn.Return<ReadonlyArray<Field>, ConfigurationError.ConfigurationError> {
  const described: Array<Field> = []
  for (const field of fields) {
    if (
      field.name._tag !== 'Present' ||
      field.declaredType._tag !== 'Resolved' ||
      field.state._tag !== 'Unique'
    )
      return yield* invalid(origin)
    const schema = yield* describeType(
      context,
      Type.substitute(field.declaredType.type, substitution),
      origin,
      active,
    )
    described.push(Object.freeze({ name: field.name.spelling, id: field.id, schema }))
  }
  return Object.freeze(described)
})

const describeType = Effect.fnUntraced(function* (
  context: Context,
  type: Type.Type,
  origin: ConfigurationOrigin.ConfigurationOrigin,
  active: ReadonlySet<string>,
): Effect.fn.Return<Schema, ConfigurationError.ConfigurationError> {
  if (type === 'bool') return Object.freeze({ kind: 'boolean', type })
  if (Type.isString(type)) return Object.freeze({ kind: 'string', type })
  if (typeof type === 'string') {
    const scalar = Scalar.find(type)
    if (scalar?.category === 'Integer')
      return Object.freeze({ kind: 'integer', type, scalar: scalar.spelling })
    return yield* invalid(origin)
  }
  const key = Type.encode(type)
  if (active.has(key) || active.size >= 128)
    return yield* ConfigurationError.make(
      'PackageParameter.describe',
      'DependencyCycle',
      'parameter schema',
      [origin],
      [...active, key],
    )
  const next = new Set([...active, key])
  if (type._tag === 'FixedArrayType') {
    const element = yield* describeType(context, type.element, origin, next)
    return Object.freeze({ kind: 'array', type, length: type.length, element })
  }
  if (!Type.isNominal(type) || type.sealed !== undefined) return yield* invalid(origin)
  const declaration = context.index.modules
    .find((module) => module.module === type.module)
    ?.members.find(
      (member) => member.canonical._tag === 'Canonical' && member.canonical.id.name === type.name,
    )
  if (declaration?.canonical._tag !== 'Canonical') return yield* invalid(origin)
  const ownership = context.packages.get(type.module)
  if (ownership === undefined) return yield* invalid(origin, 'type package identity')
  if (declaration._tag === 'EnumDeclaration') {
    if (declaration.validity._tag !== 'Valid' || declaration.representation._tag !== 'Available')
      return yield* invalid(origin)
    return Object.freeze({
      kind: 'enum',
      type,
      declaration,
      name: `${ownership.package}/${ownership.module}/${type.name}`,
    })
  }
  if (declaration._tag !== 'StructDeclaration' && declaration._tag !== 'UnionDeclaration')
    return yield* invalid(origin)
  const substitution = TypeInference.substitution(
    declaration.typeParameters.map((parameter) => parameter.type),
    type.arguments,
  )
  if (substitution === undefined) return yield* invalid(origin)
  const identity: StaticValue.NominalAggregateIdentity = Object.freeze({
    _tag: 'NominalAggregateIdentity',
    declaration: declaration.canonical.id,
    typeArguments: Object.freeze(type.arguments.map(Type.encodeGenericArgument)),
  })
  if (declaration._tag === 'StructDeclaration') {
    if (declaration.aggregateKind !== 'Named' && declaration.aggregateKind !== 'AnonymousNamed')
      return yield* invalid(origin)
    const fields = yield* describeFields(context, declaration.fields, substitution, origin, next)
    return Object.freeze({ kind: 'record', type, identity, fields })
  }
  // The optional transport is structural: no standard-library variant or field name is privileged.
  const absent = declaration.variants.find((variant) => variant.fields.length === 0)
  const present = declaration.variants.find((variant) => variant.fields.length === 1)
  if (
    declaration.validity._tag !== 'Valid' ||
    declaration.variants.length !== 2 ||
    absent?.name._tag !== 'Present' ||
    present?.name._tag !== 'Present'
  )
    return yield* invalid(origin)
  const fields = yield* describeFields(context, present.fields, substitution, origin, next)
  const field = fields[0]
  if (field === undefined) return yield* invalid(origin)
  return Object.freeze({
    kind: 'optional',
    type,
    field,
    absent: Object.freeze({
      ...identity,
      variant: Object.freeze({ ordinal: absent.id.ordinal, name: absent.name.spelling }),
    }),
    present: Object.freeze({
      ...identity,
      variant: Object.freeze({ ordinal: present.id.ordinal, name: present.name.spelling }),
    }),
  })
})

/** Resolves a concrete admitted schema from ordinary source declarations and types. */
export const describe = Effect.fn('PackageParameter.describe')(function* (
  context: Context,
  type: Type.Type,
  origin: ConfigurationOrigin.ConfigurationOrigin,
): Effect.fn.Return<Schema, ConfigurationError.ConfigurationError> {
  return yield* describeType(context, type, origin, new Set())
})

const makeAggregate = (
  identity: StaticValue.AggregateIdentity,
  fields: ReadonlyArray<StaticValue.AggregateField>,
  runtimeFields?: StaticValue.AggregateValue['runtimeFields'],
): StaticValue.AggregateValue =>
  Object.freeze({
    _tag: 'AggregateValue',
    identity,
    fields: Object.freeze(fields),
    ...(runtimeFields === undefined ? {} : { runtimeFields: Object.freeze(runtimeFields) }),
  })

const convert = Effect.fnUntraced(function* (
  schema: Schema,
  value: ConfigurationValue.ConfigurationValue,
  origin: ConfigurationOrigin.ConfigurationOrigin,
): Effect.fn.Return<StaticValue.Value, ConfigurationError.ConfigurationError> {
  switch (schema.kind) {
    case 'boolean':
      if (value.kind === 'boolean') return StaticValue.boolean(value.value)
      break
    case 'integer':
      if (value.kind === 'integer')
        return Object.freeze({
          _tag: 'IntegerValue',
          type: schema.scalar,
          value: BigInt(value.value),
        })
      break
    case 'string':
      if (value.kind === 'string')
        return Object.freeze({
          _tag: 'TextValue',
          bytes: Object.freeze([...new TextEncoder().encode(value.value)]),
        })
      break
    case 'enum': {
      if (value.kind !== 'enum' || value.type !== schema.name) break
      const member = schema.declaration.members.find(
        (candidate) =>
          candidate.name._tag === 'Present' && candidate.name.spelling === value.member,
      )
      if (
        member?.discriminant._tag !== 'Available' ||
        schema.declaration.canonical._tag !== 'Canonical' ||
        schema.declaration.representation._tag !== 'Available'
      )
        break
      return Object.freeze({
        _tag: 'EnumValue',
        type: schema.declaration.canonical.id,
        member: value.member,
        discriminant: member.discriminant.value,
        representation: schema.declaration.representation.scalar.spelling,
      })
    }
    case 'array': {
      if (value.kind !== 'array' || value.values.length !== schema.length) break
      const fields: Array<StaticValue.AggregateField> = []
      for (const [ordinal, item] of value.values.entries())
        fields.push(Object.freeze({ ordinal, value: yield* convert(schema.element, item, origin) }))
      return makeAggregate(
        Object.freeze({
          _tag: 'ArrayAggregateIdentity',
          element: Type.encode(schema.element.type),
          length: schema.length,
        }),
        fields,
      )
    }
    case 'record': {
      if (value.kind !== 'record' || Object.keys(value.fields).length !== schema.fields.length)
        break
      const fields: Array<StaticValue.AggregateField> = []
      for (const field of schema.fields) {
        const input = Object.hasOwn(value.fields, field.name) ? value.fields[field.name] : undefined
        if (input === undefined) return yield* invalid(origin)
        fields.push(
          Object.freeze({
            ordinal: field.id.ordinal,
            value: yield* convert(field.schema, input, origin),
          }),
        )
      }
      return makeAggregate(
        schema.identity,
        fields,
        schema.fields.map((field) => Object.freeze({ id: field.id, type: field.schema.type })),
      )
    }
    case 'optional':
      if (value.kind === 'none') return makeAggregate(schema.absent, [])
      if (value.kind === 'some')
        return makeAggregate(
          schema.present,
          [
            Object.freeze({
              ordinal: schema.field.id.ordinal,
              value: yield* convert(schema.field.schema, value.value, origin),
            }),
          ],
          [Object.freeze({ id: schema.field.id, type: schema.field.schema.type })],
        )
      break
  }
  return yield* invalid(origin)
})

/** Checks typed external data and admits the resulting immutable static value for this target. */
export const bind = Effect.fn('PackageParameter.bind')(function* (
  schema: Schema,
  input: unknown,
  origin: ConfigurationOrigin.ConfigurationOrigin,
  target: Target.Target,
): Effect.fn.Return<StaticValue.Value, ConfigurationError.ConfigurationError> {
  const value = yield* ConfigurationValue.decode(input, origin)
  const converted = yield* convert(schema, value, origin)
  const admitted = StaticValue.admit(converted, { pointerBits: target.pointerSize === 4 ? 32 : 64 })
  if (admitted._tag === 'Rejected') return yield* invalid(origin)
  return admitted.value
})

const sameNominal = (
  actual: StaticValue.AggregateIdentity,
  expected: StaticValue.NominalAggregateIdentity,
): boolean =>
  actual._tag === 'NominalAggregateIdentity' &&
  actual.declaration.module === expected.declaration.module &&
  actual.declaration.name === expected.declaration.name &&
  actual.typeArguments.length === expected.typeArguments.length &&
  actual.typeArguments.every((argument, ordinal) => argument === expected.typeArguments[ordinal]) &&
  actual.variant?.ordinal === expected.variant?.ordinal &&
  actual.variant?.name === expected.variant?.name

const unbindValue = Effect.fnUntraced(function* (
  schema: Schema,
  value: StaticValue.Value,
  origin: ConfigurationOrigin.ConfigurationOrigin,
): Effect.fn.Return<ConfigurationValue.ConfigurationValue, ConfigurationError.ConfigurationError> {
  switch (schema.kind) {
    case 'boolean':
      if (value._tag === 'BooleanValue')
        return Object.freeze({ kind: 'boolean', value: value.value })
      break
    case 'integer':
      if (value._tag === 'IntegerValue' && value.type === schema.scalar)
        return Object.freeze({ kind: 'integer', value: value.value.toString() })
      break
    case 'string':
      if (value._tag === 'TextValue')
        return Object.freeze({
          kind: 'string',
          value: new TextDecoder().decode(Uint8Array.from(value.bytes)),
        })
      break
    case 'enum':
      if (
        value._tag === 'EnumValue' &&
        schema.declaration.canonical._tag === 'Canonical' &&
        schema.declaration.representation._tag === 'Available' &&
        value.representation === schema.declaration.representation.scalar.spelling &&
        value.type.module === schema.declaration.canonical.id.module &&
        value.type.name === schema.declaration.canonical.id.name &&
        schema.declaration.members.some(
          (member) =>
            member.name._tag === 'Present' &&
            member.name.spelling === value.member &&
            member.discriminant._tag === 'Available' &&
            member.discriminant.value === value.discriminant,
        )
      )
        return Object.freeze({ kind: 'enum', type: schema.name, member: value.member })
      break
    case 'array': {
      if (
        value._tag !== 'AggregateValue' ||
        value.identity._tag !== 'ArrayAggregateIdentity' ||
        value.identity.length !== schema.length ||
        value.identity.element !== Type.encode(schema.element.type) ||
        value.fields.length !== schema.length
      )
        break
      const values: Array<ConfigurationValue.ConfigurationValue> = []
      for (let ordinal = 0; ordinal < schema.length; ordinal++) {
        const field = value.fields.find((candidate) => candidate.ordinal === ordinal)
        if (field === undefined) return yield* invalid(origin)
        values.push(yield* unbindValue(schema.element, field.value, origin))
      }
      return Object.freeze({ kind: 'array', values: Object.freeze(values) })
    }
    case 'record': {
      if (
        value._tag !== 'AggregateValue' ||
        !sameNominal(value.identity, schema.identity) ||
        value.fields.length !== schema.fields.length
      )
        break
      const fields: Array<readonly [string, ConfigurationValue.ConfigurationValue]> = []
      for (const field of schema.fields) {
        const item = value.fields.find((candidate) => candidate.ordinal === field.id.ordinal)
        if (item === undefined) return yield* invalid(origin)
        fields.push([field.name, yield* unbindValue(field.schema, item.value, origin)])
      }
      return Object.freeze({ kind: 'record', fields: Object.freeze(Object.fromEntries(fields)) })
    }
    case 'optional':
      if (value._tag !== 'AggregateValue') break
      if (sameNominal(value.identity, schema.absent) && value.fields.length === 0)
        return Object.freeze({ kind: 'none' })
      if (sameNominal(value.identity, schema.present) && value.fields.length === 1) {
        const field = value.fields.find(
          (candidate) => candidate.ordinal === schema.field.id.ordinal,
        )
        if (field !== undefined)
          return Object.freeze({
            kind: 'some',
            value: yield* unbindValue(schema.field.schema, field.value, origin),
          })
      }
      break
  }
  return yield* invalid(origin)
})

/** Validates a default's actual value against its source schema before canonical publication. */
export const unbind = Effect.fn('PackageParameter.unbind')(function* (
  schema: Schema,
  value: StaticValue.Value,
  origin: ConfigurationOrigin.ConfigurationOrigin,
  target: Target.Target,
): Effect.fn.Return<ConfigurationValue.ConfigurationValue, ConfigurationError.ConfigurationError> {
  const admitted = StaticValue.admit(value, { pointerBits: target.pointerSize === 4 ? 32 : 64 })
  if (admitted._tag === 'Rejected') return yield* invalid(origin)
  return yield* unbindValue(schema, admitted.value, origin)
})
