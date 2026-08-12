import * as Fn from 'effect/Function'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Type from './Type.js'

/**
 * The canonical semantic meaning that a module exposes to other modules.
 *
 * The representation is length-framed and contains semantic values only. It deliberately excludes
 * syntax, spans, diagnostic identities, bodies, and project-owned object references.
 */
export interface ModuleSurface {
  readonly _tag: 'ModuleSurface'
  readonly module: string
  readonly canonical: string
}

const frame = (value: string): string => `${value.length}:${value}`

const record = (tag: string, fields: ReadonlyArray<string> = []): string =>
  `${frame(tag)}${fields.map(frame).join('')}`

const array = (values: ReadonlyArray<string>): string => record('Array', values)

const optional = (value: string | undefined): string =>
  value === undefined ? record('None') : record('Some', [value])

const boolean = (value: boolean): string => record(value ? 'True' : 'False')

const number = (value: number): string => record('Number', [String(value)])

const exhaustive = (value: never): never => {
  throw new RangeError(`Unknown module-surface fact: ${String(value)}`)
}

const type = (value: Type.Type): string => record('Type', [Type.key(value)])

const canonicalId = (value: DeclarationIndex.CanonicalId): string =>
  record('CanonicalId', [value.module, value.name])

const declarationIdOrdinal = (value: DeclarationIndex.DeclarationId): string =>
  record('DeclarationOrdinal', [number(value.ordinal)])

const name = (value: DeclarationIndex.DeclaredName): string =>
  value._tag === 'Present' ? record('PresentName', [value.spelling]) : record('UnavailableName')

const canonicalState = (value: DeclarationIndex.CanonicalState): string => {
  switch (value._tag) {
    case 'Canonical':
      return record('Canonical', [canonicalId(value.id)])
    case 'Duplicate':
      return record('Duplicate', [canonicalId(value.original)])
    case 'Unidentified':
      return record('Unidentified')
    default:
      return exhaustive(value)
  }
}

const typePath = (value: DeclarationIndex.TypePathFact): string =>
  record('TypePath', [value.spelling, array(value.segments.map((segment) => segment.spelling))])

const arrayLength = (value: DeclarationIndex.ArrayLengthFact): string => {
  switch (value._tag) {
    case 'Available':
      return record('AvailableLength', [number(value.value)])
    case 'OutOfRange':
      return record('OutOfRangeLength', [value.spelling])
    case 'Unavailable':
      return record('UnavailableLength')
    default:
      return exhaustive(value)
  }
}

const declaredType = (value: DeclarationIndex.DeclaredTypeFact): string => {
  switch (value._tag) {
    case 'Resolved':
      return record('ResolvedType', [type(value.type), boolean(value.exposureCause !== undefined)])
    case 'Unresolved':
      return record('UnresolvedType', [
        value.spelling,
        typePath(value.path),
        optional(value.candidate === undefined ? undefined : type(value.candidate)),
        boolean(value.cause !== undefined),
      ])
    case 'FixedArray':
      return record('FixedArrayType', [declaredType(value.element), arrayLength(value.length)])
    case 'Slice':
      return record('SliceType', [
        value.access,
        declaredType(value.element),
        boolean(value.cause !== undefined),
      ])
    case 'Reference':
      return record('ReferenceType', [
        value.access,
        declaredType(value.target),
        boolean(value.cause !== undefined),
      ])
    case 'Callable':
      return record('CallableType', [
        value.mode,
        array(value.parameters.map(declaredType)),
        declaredType(value.result),
        boolean(value.cause !== undefined),
      ])
    case 'Applied':
      return record('AppliedType', [
        declaredType(value.target),
        array(value.arguments.map(declaredType)),
        boolean(value.cause !== undefined),
      ])
    case 'Effect':
      return record('EffectType', [
        declaredType(value.success),
        array(value.failures.map(declaredType)),
        array(value.failureParameters.map(type)),
        array(
          value.requirements.map((requirement) =>
            record('Requirement', [
              declaredType(requirement.capability),
              requirement.role,
              requirement.access,
            ]),
          ),
        ),
        array(value.requirementParameters.map(type)),
        boolean(value.cause !== undefined),
      ])
    case 'Union':
      return record('UnionType', [
        array(value.members.map(declaredType)),
        boolean(value.cause !== undefined),
      ])
    case 'Unavailable':
      return record('UnavailableType', [boolean(value.cause !== undefined)])
    default:
      return exhaustive(value)
  }
}

const typeParameter = (value: DeclarationIndex.TypeParameterFact): string =>
  record('TypeParameter', [
    type(value.type),
    name(value.name),
    optional(value.duplicateOf === undefined ? undefined : type(value.duplicateOf)),
  ])

const parameter = (value: DeclarationIndex.ParameterFact): string =>
  record('Parameter', [
    number(value.id.ordinal),
    name(value.name),
    declaredType(value.declaredType),
  ])

const failureRow = (value: DeclarationIndex.FailureRowFact): string =>
  record('FailureRow', [
    boolean(value.available),
    array(value.members.map(declaredType)),
    array(value.parameters.map(type)),
    array(value.failures.map(type)),
  ])

const requirementRow = (value: DeclarationIndex.RequirementRowFact): string =>
  record('RequirementRow', [
    boolean(value.available),
    array(
      value.entries.map((entry) =>
        record('RequirementEntry', [declaredType(entry.capability), entry.role, entry.access]),
      ),
    ),
    array(value.parameters.map(type)),
    array(
      value.requirements.map((requirement) =>
        record('ResolvedRequirement', [
          type(requirement.capability),
          requirement.role,
          requirement.access,
        ]),
      ),
    ),
  ])

const declaration = (value: DeclarationIndex.DeclarationFact): string =>
  record('FunctionDeclaration', [
    declarationIdOrdinal(value.id),
    canonicalState(value.canonical),
    value.visibility,
    value.functionKind,
    array(value.typeParameters.map(typeParameter)),
    number(value.parameterCount),
    array(value.parameters.map(parameter)),
    name(value.name),
    declaredType(value.returnType),
    failureRow(value.failureRow),
    requirementRow(value.requirementRow),
  ])

const fieldState = (value: DeclarationIndex.FieldState): string => {
  switch (value._tag) {
    case 'Unique':
      return record('UniqueField', [number(value.id.ordinal)])
    case 'Duplicate':
      return record('DuplicateField', [number(value.original.ordinal)])
    case 'Unidentified':
      return record('UnidentifiedField')
    default:
      return exhaustive(value)
  }
}

const field = (value: DeclarationIndex.FieldFact): string =>
  record('StructField', [
    number(value.id.ordinal),
    fieldState(value.state),
    value.visibility,
    name(value.name),
    declaredType(value.declaredType),
  ])

const structDependency = (value: DeclarationIndex.StructDependency): string =>
  record(value._tag === 'Available' ? 'AvailableStructDependency' : 'UnavailableStructDependency', [
    array(value.types.map(type)),
  ])

const struct = (value: DeclarationIndex.StructFact): string =>
  record('StructDeclaration', [
    declarationIdOrdinal(value.id),
    canonicalState(value.canonical),
    value.visibility,
    array(value.typeParameters.map(typeParameter)),
    name(value.name),
    array(value.fields.map(field)),
    structDependency(value.dependency),
  ])

const serviceOperationState = (value: DeclarationIndex.ServiceOperationState): string => {
  switch (value._tag) {
    case 'Unique':
      return record('UniqueServiceOperation', [value.id.name])
    case 'Duplicate':
      return record('DuplicateServiceOperation', [value.original.name])
    case 'Unidentified':
      return record('UnidentifiedServiceOperation')
    default:
      return exhaustive(value)
  }
}

const serviceOperation = (value: DeclarationIndex.ServiceOperationFact): string =>
  record('ServiceOperation', [
    declarationIdOrdinal(value.id),
    serviceOperationState(value.state),
    value.functionKind,
    array(value.typeParameters.map(typeParameter)),
    number(value.parameterCount),
    array(value.parameters.map(parameter)),
    name(value.name),
    declaredType(value.returnType),
    failureRow(value.failureRow),
    requirementRow(value.requirementRow),
  ])

const service = (value: DeclarationIndex.ServiceFact | DeclarationIndex.InterfaceFact): string =>
  record(value._tag, [
    declarationIdOrdinal(value.id),
    canonicalState(value.canonical),
    value.visibility,
    array(value.typeParameters.map(typeParameter)),
    name(value.name),
    array(value.operations.map(serviceOperation)),
  ])

const constantLiteral = (value: DeclarationIndex.ConstantLiteralFact): string => {
  switch (value._tag) {
    case 'BooleanLiteral':
      return record('BooleanLiteral', [boolean(value.value)])
    case 'IntegerLiteral':
      return record('IntegerLiteral', [value.value.toString()])
    case 'FloatingLiteral':
      return record('FloatingLiteral', [value.spelling])
    case 'StringLiteral':
      return record('StringLiteral', [value.data.id])
    case 'Malformed':
      return record('MalformedLiteral', [value.detail])
    case 'Unavailable':
      return record('UnavailableLiteral')
    default:
      return exhaustive(value)
  }
}

const constant = (value: DeclarationIndex.ConstantFact): string =>
  record('ConstantDeclaration', [
    declarationIdOrdinal(value.id),
    canonicalState(value.canonical),
    value.visibility,
    array(value.typeParameters.map(typeParameter)),
    name(value.name),
    declaredType(value.declaredType),
    constantLiteral(value.literal),
  ])

const member = (value: DeclarationIndex.MemberFact): string => {
  switch (value._tag) {
    case 'FunctionDeclaration':
      return declaration(value)
    case 'StructDeclaration':
      return struct(value)
    case 'ServiceDeclaration':
      return service(value)
    case 'InterfaceDeclaration':
      return service(value)
    case 'ConstantDeclaration':
      return constant(value)
    default:
      return exhaustive(value)
  }
}

const dropHook = (value: DeclarationIndex.DropHookFact): string =>
  record('DropHook', [
    name(value.name),
    value.functionKind,
    number(value.typeParameterCount),
    number(value.parameterCount),
    name(value.parameterName),
    declaredType(value.parameterType),
    declaredType(value.returnType),
    failureRow(value.failureRow),
    requirementRow(value.requirementRow),
  ])

const conformance = (value: DeclarationIndex.ConformanceFact): string =>
  record('Conformance', [
    value.module,
    number(value.ordinal),
    array(value.typeParameters.map(typeParameter)),
    declaredType(value.capability),
    declaredType(value.provider),
    array(
      value.operations.map((operation) =>
        record('ConformanceOperation', [
          name(operation.name),
          operation.target._tag === 'TypePath'
            ? typePath(operation.target)
            : record('UnavailableTarget'),
        ]),
      ),
    ),
    optional(value.hook === undefined ? undefined : dropHook(value.hook)),
  ])

/** Construct the exact surface for one module's completed headers. */
export const make = (headers: DeclarationIndex.ModuleHeaders): ModuleSurface =>
  Object.freeze({
    _tag: 'ModuleSurface',
    module: headers.module,
    canonical: record('ModuleSurface', [
      headers.module,
      array(headers.members.map(member)),
      array(headers.conformances.map(conformance)),
    ]),
  })

/** Construct canonically ordered surfaces for a completed declaration index. */
export const fromIndex = (index: DeclarationIndex.Index): ReadonlyMap<string, ModuleSurface> =>
  new Map(
    [...index.modules]
      .sort((left, right) => left.module.localeCompare(right.module))
      .map((headers) => [headers.module, make(headers)]),
  )

/** Compare complete canonical semantic representations. */
export const equals: {
  (that: ModuleSurface): (self: ModuleSurface) => boolean
  (self: ModuleSurface, that: ModuleSurface): boolean
} = Fn.dual(
  2,
  (self: ModuleSurface, that: ModuleSurface): boolean =>
    self.module === that.module && self.canonical === that.canonical,
)
