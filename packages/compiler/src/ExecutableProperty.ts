import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as ExecutionAffinity from './ExecutionAffinity.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as TypeInference from './internal/TypeInference.js'
import type * as SourceSpan from './SourceSpan.js'
import * as SuspensionMode from './SuspensionMode.js'
import * as Type from './Type.js'

export type Property = Type.SealedStaticProperty

export interface Cause {
  readonly _tag: 'ExecutablePropertyCause'
  readonly reason: 'LexicalLoan' | 'ProviderLoan' | 'NestedLoan' | 'ExternalPark' | 'Unavailable'
  readonly path: ReadonlyArray<string>
}

export type Verdict =
  | { readonly _tag: 'Satisfied' }
  | { readonly _tag: 'Unsatisfied'; readonly causes: ReadonlyArray<Cause> }

export interface Fact {
  readonly _tag: 'ExecutablePropertyFact'
  readonly subject:
    | { readonly _tag: 'Effect'; readonly identity: string }
    | { readonly _tag: 'Callable'; readonly identity: string }
  readonly affinity: ExecutionAffinity.ExecutionAffinity
  readonly detached: Verdict
  readonly nonParking: Verdict
}

export const satisfied: Verdict = Object.freeze({ _tag: 'Satisfied' })

const cause = (reason: Cause['reason'], path: ReadonlyArray<string>): Cause =>
  Object.freeze({ _tag: 'ExecutablePropertyCause', reason, path: Object.freeze(Array.from(path)) })

const causeKey = (self: Cause): string => `${self.reason}\0${self.path.join('\0')}`

const compareText = (left: string, right: string): number => {
  if (left < right) {
    return -1
  }
  if (left > right) {
    return 1
  }
  return 0
}

const verdict = (causes: ReadonlyArray<Cause>): Verdict => {
  const distinct = [...new Map(causes.map((entry) => [causeKey(entry), entry])).values()].sort(
    (left, right) => compareText(causeKey(left), causeKey(right)),
  )
  return distinct.length === 0
    ? satisfied
    : Object.freeze({ _tag: 'Unsatisfied', causes: Object.freeze(distinct) })
}

const nestedLoanCauses = (
  index: DeclarationIndex.Index,
  type: Type.Type,
  path: ReadonlyArray<string>,
  active: ReadonlySet<string> = new Set(),
): ReadonlyArray<Cause> => {
  if (Type.isString(type) || Type.isReference(type) || Type.isSlice(type) || Type.isSlot(type))
    return Object.freeze([cause('NestedLoan', [...path, Type.encode(type)])])
  if (Type.isFixedArray(type))
    return nestedLoanCauses(index, type.element, [...path, 'element'], active)
  if (Type.isUnion(type))
    return type.members.flatMap((member, ordinal) =>
      nestedLoanCauses(index, member, [...path, `member#${ordinal}`], active),
    )
  if (Type.isCallable(type) || Type.isEffect(type) || Type.isRepresented(type)) return []
  if (!Type.isNominal(type) || Type.isIntrinsicNominal(type)) return []
  const identity = `${type.module}.${type.name}`
  if (active.has(identity)) return []
  const declaration = DeclarationFacts.byCanonical(index, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (
    declaration === undefined ||
    (declaration._tag !== 'StructDeclaration' && declaration._tag !== 'UnionDeclaration')
  )
    return []
  const substitution =
    TypeInference.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      type.arguments,
    ) ?? new Map()
  const next = new Set(active).add(identity)
  const fields =
    declaration._tag === 'StructDeclaration'
      ? declaration.fields.map((field) => ({ field, owner: identity }))
      : declaration.variants.flatMap((variant) => {
          const variantName =
            variant.name._tag === 'Present' ? variant.name.spelling : `#${variant.id.ordinal}`
          return variant.fields.map((field) => ({ field, owner: `${identity}.${variantName}` }))
        })
  return fields.flatMap(({ field, owner }) => {
    if (field.declaredType._tag === 'Resolved') {
      return nestedLoanCauses(
        index,
        Type.substitute(field.declaredType.type, substitution),
        [
          ...path,
          `${owner}.${field.name._tag === 'Present' ? field.name.spelling : `#${field.id.ordinal}`}`,
        ],
        next,
      )
    }
    return [cause('Unavailable', [...path, `${owner}.#${field.id.ordinal}`])]
  })
}

export interface EnvironmentCapture {
  readonly ordinal: number
  readonly access: Type.CaptureAccess
  readonly type: Type.Type
  readonly providedRequirement?: { readonly providerAccess: 'Shared' | 'Exclusive' | 'Take' }
}

const detachedCapture = (
  index: DeclarationIndex.Index,
  capture: EnvironmentCapture,
): ReadonlyArray<Cause> => {
  const path = [`capture#${capture.ordinal}`, Type.encode(capture.type)]
  if (
    capture.providedRequirement?.providerAccess !== undefined &&
    capture.providedRequirement.providerAccess !== 'Take'
  )
    return Object.freeze([cause('ProviderLoan', path)])
  if (
    capture.access === 'Shared' ||
    capture.access === 'Exclusive' ||
    Type.isReference(capture.type) ||
    Type.isSlice(capture.type) ||
    Type.isString(capture.type) ||
    Type.isSlot(capture.type)
  )
    return Object.freeze([cause('LexicalLoan', path)])
  return nestedLoanCauses(index, capture.type, path)
}

/** Proves detachment from retained invocation/drop dependencies, never from result payload rows. */
export const detachedOfEnvironment = (
  index: DeclarationIndex.Index,
  captures: ReadonlyArray<EnvironmentCapture>,
): Verdict => verdict(captures.flatMap((capture) => detachedCapture(index, capture)))

/** Proves that one ordinary value representation owns its complete retained environment. */
export const detachedOfType = (index: DeclarationIndex.Index, type: Type.Type): Verdict =>
  Type.isParameter(type) && type.staticProperties.includes('Intrinsic.Detached')
    ? satisfied
    : verdict(nestedLoanCauses(index, type, [Type.encode(type)]))

/** Proves NonParking from external-park reachability only; nested transfer remains admissible. */
export const nonParkingOfSummary = (summary: SuspensionMode.Summary): Verdict =>
  SuspensionMode.has(summary, 'ExternalPark')
    ? verdict(
        summary.causes
          .filter((entry) => entry.mode === 'ExternalPark')
          .map((entry) => cause('ExternalPark', entry.path)),
      )
    : satisfied

const sameArguments = (
  left: ReadonlyArray<Type.GenericArgument>,
  right: ReadonlyArray<Type.GenericArgument>,
): boolean =>
  left.length === right.length &&
  left.every((argument, ordinal) => {
    const candidate = right.at(ordinal)
    return candidate !== undefined && Type.equalsGenericArgument(argument, candidate)
  })

const callableSubjectOf = (
  discovery: Instances.Discovery,
  identity: Type.CallableIdentityArgument,
): string | undefined => {
  const environment = identity.environment
  if (environment === undefined) return undefined
  const candidate = discovery.callables.find((callable) => {
    if (callable.target._tag !== 'DeclarationCallableTarget') return false
    if (identity.target._tag !== 'Declaration') return false
    return (
      callable.target.declaration.module === identity.target.module &&
      callable.target.declaration.name === identity.target.name &&
      sameArguments(callable.typeArguments, identity.typeArguments) &&
      Type.callableEnvironmentKey(Instances.callableEnvironmentIdentity(callable)) ===
        Type.callableEnvironmentKey(environment)
    )
  })
  return candidate === undefined ? undefined : `Callable:${Instances.callableIdentity(candidate)}`
}

const representedSubjectsOfType = (
  discovery: Instances.Discovery,
  index: DeclarationIndex.Index,
  type: Type.Type,
  active: ReadonlySet<string> = new Set(),
): ReadonlyArray<string> => {
  if (Type.isRepresented(type)) {
    const argument = type.representation.argument
    let alternatives: readonly Type.ExactRepresentationArgument[]
    if (Type.isExactRepresentationArgument(argument)) {
      alternatives = [argument]
    } else if (Type.isCompositeEffectRepresentationArgument(argument)) {
      alternatives = argument.alternatives
    } else {
      alternatives = []
    }
    return Object.freeze(
      alternatives.flatMap((alternative) => {
        const identity = alternative.identity
        if (identity._tag === 'CallableIdentityArgument') {
          const subject = callableSubjectOf(discovery, identity)
          return subject === undefined ? [] : [subject]
        }
        const candidate = discovery.effects.find(
          (effect) =>
            effect.identity === identity.identity ||
            effect.representationIdentity === identity.identity,
        )
        return candidate === undefined ? [] : [`Effect:${candidate.identity}`]
      }),
    )
  }
  if (Type.isFixedArray(type))
    return representedSubjectsOfType(discovery, index, type.element, active)
  if (Type.isUnion(type))
    return type.members.flatMap((member) =>
      representedSubjectsOfType(discovery, index, member, active),
    )
  if (!Type.isNominal(type) || Type.isIntrinsicNominal(type)) return []
  const identity = `${type.module}.${type.name}`
  if (active.has(identity)) return []
  const declaration = DeclarationFacts.byCanonical(index, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (
    declaration === undefined ||
    (declaration._tag !== 'StructDeclaration' && declaration._tag !== 'UnionDeclaration')
  )
    return []
  const substitution =
    TypeInference.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      type.arguments,
    ) ?? new Map()
  const next = new Set(active).add(identity)
  const fields =
    declaration._tag === 'StructDeclaration'
      ? declaration.fields
      : declaration.variants.flatMap((variant) => variant.fields)
  return fields.flatMap((field) =>
    field.declaredType._tag === 'Resolved'
      ? representedSubjectsOfType(
          discovery,
          index,
          Type.substitute(field.declaredType.type, substitution),
          next,
        )
      : [],
  )
}

/** Derives exact executable properties without conflating detachment with local affinity. */
export const derive = (
  discovery: Instances.Discovery,
  index: DeclarationIndex.Index,
  callableIdentity: (self: Instances.CallableInstance) => string,
): ReadonlyArray<Fact> => {
  const effects = discovery.effects.map((effect): Fact =>
    Object.freeze({
      _tag: 'ExecutablePropertyFact',
      subject: Object.freeze({ _tag: 'Effect', identity: effect.identity }),
      affinity: ExecutionAffinity.ofEnvironment(
        index,
        effect.captures.map((capture) => ({ type: capture.type })),
      ),
      detached: detachedOfEnvironment(index, effect.captures),
      nonParking: nonParkingOfSummary(effect.suspension),
    }),
  )
  const callables = discovery.callables.map((callable): Fact => {
    const identity = callableIdentity(callable)
    return Object.freeze({
      _tag: 'ExecutablePropertyFact',
      subject: Object.freeze({ _tag: 'Callable', identity }),
      affinity: ExecutionAffinity.ofEnvironment(
        index,
        callable.captures.map((capture) => ({ type: capture.type })),
      ),
      detached: detachedOfEnvironment(index, callable.captures),
      nonParking: (() => {
        if (callable.target._tag !== 'DeclarationCallableTarget') return satisfied
        const declaration = callable.target.declaration
        const target = discovery.instances.find(
          (instance) =>
            instance.key.declaration.module === declaration.module &&
            instance.key.declaration.name === declaration.name &&
            instance.key.typeArguments.length === callable.typeArguments.length &&
            instance.key.typeArguments.every((argument, ordinal) => {
              const expected = callable.typeArguments.at(ordinal)
              return expected !== undefined && Type.equalsGenericArgument(argument, expected)
            }),
        )
        return target === undefined
          ? verdict([cause('Unavailable', [`callable:${identity}`])])
          : nonParkingOfSummary(
              discovery.suspension.find(
                (fact) => fact.subject._tag === 'Instance' && fact.subject.key === target.key,
              )?.summary ?? SuspensionMode.direct,
            )
      })(),
    })
  })
  const nodes = new Map<
    string,
    {
      readonly direct: Verdict
      readonly dependencies: ReadonlyArray<{ readonly label: string; readonly target: string }>
    }
  >()
  for (const [ordinal, effect] of discovery.effects.entries()) {
    const fact = effects.at(ordinal)
    if (fact === undefined) continue
    nodes.set(
      `Effect:${effect.identity}`,
      Object.freeze({
        direct: fact.detached,
        dependencies: Object.freeze(
          effect.captures.flatMap((capture) => {
            let directTarget: string | undefined
            if (capture.effectIdentity !== undefined) {
              directTarget = `Effect:${capture.effectIdentity}`
            } else if (capture.callableIdentity === undefined) {
              directTarget = undefined
            } else {
              directTarget = callableSubjectOf(discovery, capture.callableIdentity)
            }
            const targets = [
              ...(directTarget === undefined ? [] : [directTarget]),
              ...representedSubjectsOfType(discovery, index, capture.type),
            ]
            return [...new Set(targets)].map((target) =>
              Object.freeze({ label: `capture#${capture.ordinal}`, target }),
            )
          }),
        ),
      }),
    )
  }
  for (const [ordinal, callable] of discovery.callables.entries()) {
    const fact = callables.at(ordinal)
    if (fact === undefined) continue
    nodes.set(
      `Callable:${Instances.callableIdentity(callable)}`,
      Object.freeze({
        direct: fact.detached,
        dependencies: Object.freeze(
          callable.captures.flatMap((capture) => {
            const directTarget =
              capture.callableIdentity === undefined
                ? undefined
                : callableSubjectOf(discovery, capture.callableIdentity)
            const targets = [
              ...(directTarget === undefined ? [] : [directTarget]),
              ...representedSubjectsOfType(discovery, index, capture.type),
            ]
            return [...new Set(targets)].map((target) =>
              Object.freeze({ label: `capture#${capture.ordinal}`, target }),
            )
          }),
        ),
      }),
    )
  }
  const detachedThrough = (subject: string): Verdict => {
    const visit = (nodeKey: string, active: ReadonlySet<string>): ReadonlyArray<Cause> => {
      const node = nodes.get(nodeKey)
      if (node === undefined || active.has(nodeKey)) return []
      const next = new Set(active).add(nodeKey)
      const own = node.direct._tag === 'Unsatisfied' ? node.direct.causes : []
      return [
        ...own,
        ...node.dependencies
          .slice()
          .sort((left, right) =>
            compareText(`${left.label}\0${left.target}`, `${right.label}\0${right.target}`),
          )
          .flatMap((dependency) =>
            visit(dependency.target, next).map((entry) =>
              cause(entry.reason, [dependency.label, dependency.target, ...entry.path]),
            ),
          ),
      ]
    }
    return verdict(visit(subject, new Set()))
  }
  return Object.freeze(
    [...effects, ...callables]
      .map((fact): Fact =>
        Object.freeze({
          ...fact,
          detached: detachedThrough(`${fact.subject._tag}:${fact.subject.identity}`),
        }),
      )
      .sort((left, right) => compareText(encodeSubject(left), encodeSubject(right))),
  )
}

const encodeSubject = (self: Fact): string => `${self.subject._tag}:${self.subject.identity}`

const matchesEffectIdentity = (
  identity: Type.EffectIdentityArgument,
  candidate: Instances.EffectInstance,
): boolean => {
  if (candidate.identity === identity.identity) return true
  if (candidate.representationIdentity !== identity.identity) return false
  return (
    identity.owner === undefined ||
    (candidate.owner.declaration.module === identity.owner.declaration.module &&
      candidate.owner.declaration.name === identity.owner.declaration.name &&
      sameArguments(
        candidate.owner.typeArguments.filter(
          (argument) => !Type.isHiddenExecutableArgument(argument),
        ),
        identity.owner.typeArguments.filter(
          (argument) => !Type.isHiddenExecutableArgument(argument),
        ),
      ))
  )
}

const factOfExact = (
  self: Instances.Discovery,
  exact: Type.ExactRepresentationArgument,
  facts: ReadonlyArray<Fact>,
): Fact | undefined => {
  if (exact.identity._tag === 'EffectIdentityArgument') {
    const effectIdentity = exact.identity
    const effect = self.effects.find((candidate) =>
      matchesEffectIdentity(effectIdentity, candidate),
    )
    return effect === undefined
      ? undefined
      : facts.find(
          (fact) => fact.subject._tag === 'Effect' && fact.subject.identity === effect.identity,
        )
  }
  const identity = exact.identity
  const callable = self.callables.find((candidate) => {
    if (candidate.target._tag !== 'DeclarationCallableTarget') return false
    if (identity.target._tag !== 'Declaration') return false
    if (
      candidate.target.declaration.module !== identity.target.module ||
      candidate.target.declaration.name !== identity.target.name ||
      !sameArguments(candidate.typeArguments, identity.typeArguments)
    )
      return false
    const environment = identity.environment
    return (
      environment !== undefined &&
      Type.callableEnvironmentKey(Instances.callableEnvironmentIdentity(candidate)) ===
        Type.callableEnvironmentKey(environment)
    )
  })
  if (callable !== undefined) {
    const identity = Instances.callableIdentity(callable)
    return facts.find(
      (fact) => fact.subject._tag === 'Callable' && fact.subject.identity === identity,
    )
  }
  if (identity.environment !== undefined) return undefined
  const targetIdentity = identity.target
  const target =
    targetIdentity._tag === 'Declaration'
      ? self.instances.find(
          (instance) =>
            instance.key.declaration.module === targetIdentity.module &&
            instance.key.declaration.name === targetIdentity.name &&
            sameArguments(instance.key.typeArguments, identity.typeArguments),
        )
      : undefined
  const summary =
    target === undefined ? SuspensionMode.direct : Instances.suspensionOf(self, target.key)
  return Object.freeze({
    _tag: 'ExecutablePropertyFact',
    subject: Object.freeze({ _tag: 'Callable', identity: identity.identity }),
    affinity: ExecutionAffinity.unrestricted,
    detached: satisfied,
    nonParking: nonParkingOfSummary(summary),
  })
}

const exactAlternatives = (
  argument: Type.GenericArgument,
): ReadonlyArray<Type.ExactRepresentationArgument> => {
  if (Type.isExactRepresentationArgument(argument)) {
    return Object.freeze([argument])
  }
  if (Type.isCompositeEffectRepresentationArgument(argument)) {
    return argument.alternatives
  }
  return Object.freeze([])
}

const nominalApplications = (type: Type.Type): ReadonlyArray<Type.Nominal> => {
  if (Type.isNominal(type))
    return Object.freeze([
      type,
      ...type.arguments.flatMap((argument) =>
        Type.isTypeArgument(argument) ? nominalApplications(argument) : [],
      ),
    ])
  if (Type.isFixedArray(type)) return nominalApplications(type.element)
  if (Type.isSlice(type)) return nominalApplications(type.element)
  if (Type.isReference(type)) return nominalApplications(type.target)
  if (Type.isUnion(type)) return type.members.flatMap(nominalApplications)
  if (Type.isCallable(type)) return [...type.parameters, type.result].flatMap(nominalApplications)
  if (Type.isEffect(type))
    return [
      type.success,
      ...Type.failureMembers(type),
      ...Type.requirementMembers(type).map((requirement) => requirement.capability),
    ].flatMap(nominalApplications)
  if (Type.isRepresented(type))
    return [type.contract, type.representation.requiredBound].flatMap(nominalApplications)
  return Object.freeze([])
}

/** Diagnoses reachable concrete substitutions that fail one sealed static-property obligation. */
export const violationDiagnostics = (
  self: Instances.Discovery,
  index: DeclarationIndex.Index,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  const facts = derive(self, index, Instances.callableIdentity)
  const incoming = new Map<string, Instances.CallInstance>()
  for (const call of self.calls) {
    const key = Instances.keyText(call.target)
    const current = incoming.get(key)
    if (
      current === undefined ||
      call.span.sourceId < current.span.sourceId ||
      (call.span.sourceId === current.span.sourceId && call.span.start < current.span.start)
    )
      incoming.set(key, call)
  }
  const diagnosticsFor = (
    parameter: DeclarationFacts.TypeParameterFact,
    argument: Type.GenericArgument,
    span: SourceSpan.SourceSpan,
  ): ReadonlyArray<Diagnostic.Diagnostic> => {
    if (parameter.staticProperties.length === 0) return []
    const alternatives = exactAlternatives(argument)
    const ordinary = Type.isTypeArgument(argument) ? argument : undefined
    if (alternatives.length === 0 && ordinary === undefined) return []
    const exactFacts = alternatives.map((alternative) => factOfExact(self, alternative, facts))
    return parameter.staticProperties.flatMap((property) => {
      let ordinaryVerdict: Verdict | undefined
      if (ordinary === undefined) {
        ordinaryVerdict = undefined
      } else if (property === 'Intrinsic.Detached') {
        ordinaryVerdict = detachedOfType(index, ordinary)
      } else {
        ordinaryVerdict = verdict([cause('Unavailable', [Type.encode(ordinary)])])
      }
      let failed: readonly Cause[]
      if (ordinaryVerdict === undefined) {
        failed = exactFacts.flatMap((fact, factOrdinal) => {
          const propertyVerdict =
            property === 'Intrinsic.Detached' ? fact?.detached : fact?.nonParking
          if (propertyVerdict === undefined) {
            return [cause('Unavailable', [`alternative#${factOrdinal}`])]
          }
          if (propertyVerdict._tag === 'Unsatisfied') return propertyVerdict.causes
          return []
        })
      } else if (ordinaryVerdict._tag === 'Unsatisfied') {
        failed = ordinaryVerdict.causes
      } else {
        failed = []
      }
      return failed.length === 0
        ? []
        : [
            Diagnostic.unsatisfiedExecutableProperty(
              property,
              failed.map((entry) => `${entry.reason}:${entry.path.join(' -> ')}`),
              span,
            ),
          ]
    })
  }
  const functionApplications = self.instances.flatMap((instance) => {
    const visibleArguments = instance.key.typeArguments.filter(
      (argument) => !Type.isHiddenExecutableArgument(argument),
    )
    return instance.function.declaration.typeParameters.flatMap((parameter, ordinal) => {
      const argument = visibleArguments.at(ordinal)
      if (argument === undefined) return []
      const span = incoming.get(Instances.keyText(instance.key))?.span ?? parameter.syntax.span
      return diagnosticsFor(parameter, argument, span)
    })
  })
  const nominalApplications_ = self.instances.flatMap((instance) =>
    instance.function.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
      .flatMap((expression) => {
        if (expression._tag === 'Unavailable') {
          return []
        }
        return nominalApplications(expression.type).flatMap((application) => {
          const declaration = DeclarationFacts.byCanonical(index, {
            _tag: 'CanonicalDeclarationId',
            module: application.module,
            name: application.name,
          })
          if (declaration === undefined || !('typeParameters' in declaration)) return []
          return declaration.typeParameters.flatMap((parameter, ordinal) => {
            const argument = application.arguments.at(ordinal)
            return argument === undefined
              ? []
              : diagnosticsFor(parameter, argument, expression.span)
          })
        })
      }),
  )
  const distinct = new Map<string, Diagnostic.Diagnostic>()
  for (const diagnostic of [...functionApplications, ...nominalApplications_]) {
    const key = `${diagnostic.code}\0${diagnostic.message}`
    const current = distinct.get(key)
    if (
      current === undefined ||
      diagnostic.span.sourceId < current.span.sourceId ||
      (diagnostic.span.sourceId === current.span.sourceId &&
        diagnostic.span.start < current.span.start)
    )
      distinct.set(key, diagnostic)
  }
  return Object.freeze([...distinct.values()])
}

export const encodeVerdict = (self: Verdict): string =>
  self._tag === 'Satisfied'
    ? 'Satisfied'
    : `Unsatisfied<${self.causes.map((entry) => `${entry.reason}:${entry.path.join(' -> ')}`).join(';')}>`

export const encode = (self: Fact): string =>
  `${encodeSubject(self)} affinity=${ExecutionAffinity.encode(self.affinity)} Detached=${encodeVerdict(self.detached)} NonParking=${encodeVerdict(self.nonParking)}`
