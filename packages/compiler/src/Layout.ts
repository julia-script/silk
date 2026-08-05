import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Hir from './Hir.js'
import type * as Instances from './Instances.js'
import * as Target from './Target.js'

/** The initial closed representation vocabulary for concrete runtime types. */
export type Representation =
  | { readonly _tag: 'SignedInteger'; readonly bits: 32 }
  | { readonly _tag: 'Boolean'; readonly bits: 32; readonly falseValue: 0; readonly trueValue: 1 }

/** One compiler-owned concrete layout entry. */
export interface Entry {
  readonly _tag: 'LayoutEntry'
  readonly type: DeclarationIndex.SemanticType
  readonly size: number
  readonly alignment: number
  readonly representation: Representation
}

/** The complete layout plan for one target-aware MIR program. */
export interface Plan {
  readonly _tag: 'LayoutPlan'
  readonly target: Target.Target
  readonly entries: ReadonlyArray<Entry>
}

/** One deterministic explanation of a malformed layout plan. */
export interface Violation {
  readonly _tag: 'LayoutViolation'
  readonly rule: 'NonCanonicalTarget' | 'DuplicateType' | 'NonCanonicalOrder' | 'InvalidScalar'
  readonly type?: DeclarationIndex.SemanticType
  readonly detail: string
}

const i32 = (): Entry =>
  Object.freeze({
    _tag: 'LayoutEntry',
    type: 'I32',
    size: 4,
    alignment: 4,
    representation: Object.freeze({ _tag: 'SignedInteger', bits: 32 }),
  })

const bool = (): Entry =>
  Object.freeze({
    _tag: 'LayoutEntry',
    type: 'Bool',
    size: 4,
    alignment: 4,
    representation: Object.freeze({
      _tag: 'Boolean',
      bits: 32,
      falseValue: 0,
      trueValue: 1,
    }),
  })

const makeEntry = (type: DeclarationIndex.SemanticType): Entry => (type === 'Bool' ? bool() : i32())

const addExpressionTypes = (
  types: Set<DeclarationIndex.SemanticType>,
  expression: Hir.Expression,
): void => {
  if (expression._tag === 'Unavailable') return
  types.add(expression.type)
  if (expression._tag === 'Move') addExpressionTypes(types, expression.subject)
  if (expression._tag === 'Call' || expression._tag === 'BuiltinCall') {
    for (const argument of expression.arguments) addExpressionTypes(types, argument)
  }
}

const addStatementTypes = (
  types: Set<DeclarationIndex.SemanticType>,
  statements: ReadonlyArray<Hir.Statement>,
): void => {
  for (const statement of statements) {
    if (statement._tag === 'Bind') addExpressionTypes(types, statement.initializer)
    if (statement._tag === 'Return') addExpressionTypes(types, statement.expression)
    if (statement._tag === 'If') {
      addExpressionTypes(types, statement.condition)
      addStatementTypes(types, statement.taken)
      addStatementTypes(types, statement.otherwise)
    }
  }
}

const addFunctionTypes = (types: Set<DeclarationIndex.SemanticType>, fn: Hir.HirFunction): void => {
  for (const parameter of fn.declaration.parameters) {
    if (parameter.declaredType._tag === 'Resolved') types.add(parameter.declaredType.type)
  }
  if (fn.declaration.returnType._tag === 'Resolved') types.add(fn.declaration.returnType.type)
  addStatementTypes(types, fn.statements)
}

/** Plans only the concrete logical types reached by discovered runtime instances. */
export const plan = (target: Target.Target, discovery: Instances.Discovery): Plan => {
  const types = new Set<DeclarationIndex.SemanticType>()
  for (const instance of discovery.instances) addFunctionTypes(types, instance.function)
  return Object.freeze({
    _tag: 'LayoutPlan',
    target,
    entries: Object.freeze([...types].sort().map(makeEntry)),
  })
}

/** Constructs a plan for hand-built MIR samples and focused tests. */
export const make = (
  target: Target.Target,
  types: ReadonlyArray<DeclarationIndex.SemanticType>,
): Plan =>
  Object.freeze({
    _tag: 'LayoutPlan',
    target,
    entries: Object.freeze([...new Set(types)].sort().map(makeEntry)),
  })

/** Looks up one canonical type entry. */
export const entry = (self: Plan, type: DeclarationIndex.SemanticType): Entry | undefined =>
  self.entries.find((candidate) => candidate.type === type)

const representationEquals = (left: Representation, right: Representation): boolean =>
  left._tag === right._tag &&
  left.bits === right.bits &&
  (left._tag !== 'Boolean' ||
    (right._tag === 'Boolean' &&
      left.falseValue === right.falseValue &&
      left.trueValue === right.trueValue))

/** Verifies canonical target, ordering, uniqueness, and scalar representation facts. */
export const verify = (self: Plan): ReadonlyArray<Violation> => {
  const violations: Array<Violation> = []
  if (!Target.isCanonical(self.target)) {
    violations.push(
      Object.freeze({
        _tag: 'LayoutViolation',
        rule: 'NonCanonicalTarget',
        detail: `target ${self.target.id} does not match its canonical profile`,
      }),
    )
  }
  const seen = new Set<DeclarationIndex.SemanticType>()
  let previous: DeclarationIndex.SemanticType | undefined
  for (const candidate of self.entries) {
    if (seen.has(candidate.type)) {
      violations.push(
        Object.freeze({
          _tag: 'LayoutViolation',
          rule: 'DuplicateType',
          type: candidate.type,
          detail: `layout contains duplicate ${candidate.type} entry`,
        }),
      )
    }
    if (previous !== undefined && previous.localeCompare(candidate.type) > 0) {
      violations.push(
        Object.freeze({
          _tag: 'LayoutViolation',
          rule: 'NonCanonicalOrder',
          type: candidate.type,
          detail: `${candidate.type} follows ${previous} out of canonical order`,
        }),
      )
    }
    const expected = makeEntry(candidate.type)
    if (
      candidate.size !== expected.size ||
      candidate.alignment !== expected.alignment ||
      !representationEquals(candidate.representation, expected.representation)
    ) {
      violations.push(
        Object.freeze({
          _tag: 'LayoutViolation',
          rule: 'InvalidScalar',
          type: candidate.type,
          detail: `${candidate.type} does not match the canonical scalar layout`,
        }),
      )
    }
    seen.add(candidate.type)
    previous = candidate.type
  }
  return Object.freeze(violations)
}

const representationText = (representation: Representation): string =>
  representation._tag === 'SignedInteger'
    ? `signed-i${representation.bits}`
    : `bool-i${representation.bits} false=${representation.falseValue} true=${representation.trueValue}`

/** Deterministic textual encoding of a complete layout plan. */
export const encode = (self: Plan): string =>
  [
    `target ${Target.encode(self.target)}`,
    ...self.entries.map(
      (candidate) =>
        `layout ${candidate.type} size=${candidate.size} align=${candidate.alignment} repr=${representationText(candidate.representation)}`,
    ),
    '',
  ].join('\n')
