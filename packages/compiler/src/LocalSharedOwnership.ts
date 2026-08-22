import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Diagnostic from './Diagnostic.js'
import * as ExecutionAffinity from './ExecutionAffinity.js'
import * as TypeInference from './internal/TypeInference.js'
import * as Type from './Type.js'

/** The only compiler-published ownership role carried by a local strong handle. */
export const strongRole = 'LocalSharedStrong' as const

/** Representation-free semantic inspection of one available sealed core specialization. */
export interface CoreFact {
  readonly _tag: 'LocalSharedCoreFact'
  readonly identity: 'Intrinsic.SharedCore'
  readonly type: Type.Nominal
  readonly element: Type.Type
  readonly role: typeof strongRole
  readonly category: 'Affine'
  readonly affinity: Extract<
    ExecutionAffinity.ExecutionAffinity,
    { readonly _tag: 'LocalExecution' }
  >
}

/** Inspects a core without exposing address, count, access, layout, or reclaim lanes. */
export const inspect = (type: Type.Type): CoreFact | undefined =>
  Type.isSharedCore(type)
    ? Object.freeze({
        _tag: 'LocalSharedCoreFact',
        identity: 'Intrinsic.SharedCore',
        type,
        element: type.arguments[0],
        role: strongRole,
        category: 'Affine',
        affinity: ExecutionAffinity.localExecution,
      })
    : undefined

/** The exact structural location of a live strong-handle obligation. */
export type ObligationPlan =
  | { readonly _tag: 'NoLocalSharedObligation' }
  | {
      readonly _tag: 'LocalSharedStrong'
      readonly core: Type.Nominal & {
        readonly module: 'Intrinsic'
        readonly name: 'SharedCore'
        readonly arguments: readonly [Type.Type]
      }
    }
  | { readonly _tag: 'ParameterDependent'; readonly parameter: Type.Parameter }
  | { readonly _tag: 'Product'; readonly components: ReadonlyArray<ObligationPlan> }
  | { readonly _tag: 'Repeat'; readonly length: number; readonly element: ObligationPlan }
  | {
      readonly _tag: 'ActiveUnion'
      readonly cases: ReadonlyArray<{
        readonly member: Type.Type
        readonly obligations: ObligationPlan
      }>
    }
  | { readonly _tag: 'Unavailable'; readonly causes: ReadonlyArray<Diagnostic.Identity> }

export const none: ObligationPlan = Object.freeze({ _tag: 'NoLocalSharedObligation' })

const unavailable = (causes: ReadonlyArray<Diagnostic.Identity>): ObligationPlan =>
  Object.freeze({ _tag: 'Unavailable', causes: Object.freeze([...causes]) })

const causeOf = (fact: DeclarationFacts.DeclaredTypeFact): ReadonlyArray<Diagnostic.Identity> =>
  'cause' in fact && fact.cause !== undefined ? Object.freeze([fact.cause]) : Object.freeze([])

const product = (components: ReadonlyArray<ObligationPlan>): ObligationPlan => {
  const retained = components.filter((component) => component._tag !== 'NoLocalSharedObligation')
  if (retained.length === 0) return none
  const only = retained.at(0)
  return retained.length === 1 && only !== undefined
    ? only
    : Object.freeze({ _tag: 'Product', components: Object.freeze(retained) })
}

/** Combines independently retained structural obligations without inventing a representation. */
export const combine = (components: ReadonlyArray<ObligationPlan>): ObligationPlan =>
  product(components)

const ofTypeInner = (
  index: DeclarationIndex.Index,
  type: Type.Type,
  active: ReadonlySet<string>,
): ObligationPlan => {
  if (Type.isSharedCore(type)) return Object.freeze({ _tag: 'LocalSharedStrong', core: type })
  if (Type.isParameter(type)) return Object.freeze({ _tag: 'ParameterDependent', parameter: type })
  if (Type.isFixedArray(type))
    return type.length === 0
      ? none
      : Object.freeze({
          _tag: 'Repeat',
          length: type.length,
          element: ofTypeInner(index, type.element, active),
        })
  if (Type.isUnion(type))
    return Object.freeze({
      _tag: 'ActiveUnion',
      cases: Object.freeze(
        type.members.map((member) =>
          Object.freeze({ member, obligations: ofTypeInner(index, member, active) }),
        ),
      ),
    })
  // Lexical/raw-storage views do not own initialized element values as structural fields.
  if (Type.isSlot(type) || Type.isRawBuffer(type)) return none
  // Borrows retain affinity, not an independently owned strong-count obligation.
  if (
    Type.isSlice(type) ||
    Type.isReference(type) ||
    Type.isCallable(type) ||
    Type.isEffect(type) ||
    Type.isRepresented(type) ||
    !Type.isNominal(type)
  )
    return none
  if (Type.isIntrinsicNominal(type))
    return product(
      type.arguments.flatMap((argument) =>
        Type.isTypeArgument(argument) ? [ofTypeInner(index, argument, active)] : [],
      ),
    )
  // Declaration identity, rather than the fully specialized type key, is the recursion
  // boundary. Invalid polymorphic recursion must remain a recoverable semantic diagnostic.
  const key = `${type.module}\0${type.name}`
  if (active.has(key)) return none
  const declaration = DeclarationFacts.byCanonical(index, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (declaration?._tag !== 'StructDeclaration') return none
  const substitution =
    TypeInference.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      type.arguments,
    ) ?? new Map()
  const next = new Set(active).add(key)
  return product(
    declaration.fields.map((field) =>
      field.declaredType._tag === 'Resolved'
        ? ofTypeInner(index, Type.substitute(field.declaredType.type, substitution), next)
        : unavailable(causeOf(field.declaredType)),
    ),
  )
}

/** Publishes the structural strong-handle obligations for one owned value. */
export const ofType = (index: DeclarationIndex.Index, type: Type.Type): ObligationPlan =>
  ofTypeInner(index, type, new Set())

/** Publishes obligations for an available or causally unavailable declared type. */
export const ofDeclaredType = (
  index: DeclarationIndex.Index,
  fact: DeclarationFacts.DeclaredTypeFact,
): ObligationPlan =>
  fact._tag === 'Resolved' ? ofType(index, fact.type) : unavailable(causeOf(fact))

/** Publishes owned environment obligations; borrowed captures do not manufacture strong handles. */
export const ofEnvironment = (
  index: DeclarationIndex.Index,
  components: ReadonlyArray<{
    readonly access: Type.CaptureAccess
    readonly type?: Type.Type
    readonly cause?: Diagnostic.Identity
  }>,
): ObligationPlan =>
  product(
    components.map((component) =>
      component.access !== 'Take'
        ? none
        : component.type === undefined
          ? unavailable(component.cause === undefined ? [] : [component.cause])
          : ofType(index, component.type),
    ),
  )

/** Counts a concrete plan; callers choose the active union case explicitly. */
export const count = (self: ObligationPlan, activeUnionCase = 0): number => {
  switch (self._tag) {
    case 'NoLocalSharedObligation':
    case 'ParameterDependent':
    case 'Unavailable':
      return 0
    case 'LocalSharedStrong':
      return 1
    case 'Product':
      return self.components.reduce((total, component) => total + count(component), 0)
    case 'Repeat':
      return self.length * count(self.element)
    case 'ActiveUnion':
      return count(self.cases.at(activeUnionCase)?.obligations ?? none)
  }
}

/** Deterministic inspection encoding for semantic and ownership goldens. */
export const encode = (self: ObligationPlan): string => {
  switch (self._tag) {
    case 'NoLocalSharedObligation':
      return 'none'
    case 'LocalSharedStrong':
      return `${strongRole}<${Type.encode(self.core.arguments[0])}>`
    case 'ParameterDependent':
      return `parameter<${Type.key(self.parameter)}>`
    case 'Product':
      return `product(${self.components.map(encode).join(',')})`
    case 'Repeat':
      return `repeat(${self.length},${encode(self.element)})`
    case 'ActiveUnion':
      return `active-union(${self.cases.map((entry) => `${Type.key(entry.member)}:${encode(entry.obligations)}`).join(',')})`
    case 'Unavailable':
      return `unavailable(${self.causes.map((cause) => `${cause.code}@${cause.span.sourceId}:${cause.span.start}:${cause.ordinal}`).join(',')})`
  }
}
