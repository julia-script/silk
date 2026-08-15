import * as Hir from './Hir.js'
import type * as Instances from './Instances.js'
import * as RepresentationField from './RepresentationField.js'
import * as Type from './Type.js'

/**
 * The runtime half of nominal callable storage.
 *
 * `RepresentationField` owns the semantic half: stable field identities, the concrete or explicitly
 * unavailable representation argument, the substituted required bound, and admissibility. This
 * actor consumes one of those resolutions and enriches it with everything a runtime phase needs —
 * the static target, its concrete arguments, the ordered capture environment, the receiver access
 * each invocation demands, capture loans, liveness, and cleanup.
 *
 * Ownership, layout, MIR, and every engine read this one record. None of them recovers callable
 * identity from initializer syntax: the target and captures come from the representation argument
 * the frontend already retained and from the specialized callable instance it names.
 */

/** How a capture reaches the callable environment stored inside the enclosing aggregate. */
export type CaptureAccess = 'Copy' | 'Shared' | 'Exclusive' | 'Take'

/** The aggregate receiver access one invocation mode demands. */
export type ReceiverAccess = Type.CallableMode

/** The statically known declaration or builtin every realized field invocation calls. */
export type StaticTarget = Type.CallableIdentityArgument['target']

/** One ordered inline capture lane contributed to the enclosing nominal's ABI. */
export interface CaptureSlot {
  readonly _tag: 'CallableCaptureSlot'
  readonly ordinal: number
  readonly parameterOrdinal: number
  readonly access: CaptureAccess
  readonly type: Type.Type
  /** Owned lanes are the only lanes the enclosing aggregate must clean. */
  readonly owned: boolean
  /** Borrowed lanes keep one loan alive for as long as the aggregate stores the callable. */
  readonly borrowed: boolean
}

/** One borrow the enclosing aggregate keeps alive on behalf of a stored callable environment. */
export interface LoanDependency {
  readonly _tag: 'CallableCaptureLoan'
  readonly capture: number
  readonly access: 'Shared' | 'Exclusive'
  readonly type: Type.Type
}

/** How long the stored environment's lanes stay live inside the enclosing aggregate. */
export interface Liveness {
  readonly _tag: 'CallableFieldLiveness'
  /** A field carrying owned or borrowed lanes keeps its enclosing nominal move-only. */
  readonly moveOnly: boolean
  readonly ownedLanes: number
  readonly borrowedLanes: number
}

/** The exactly-once cleanup obligation the enclosing aggregate carries for one stored callable. */
export interface Cleanup {
  readonly _tag: 'CallableFieldCleanup'
  /** Ordered owned capture ordinals, cleaned exactly once when the aggregate's storage ends. */
  readonly lanes: ReadonlyArray<number>
  /** Take invocation consumes the whole aggregate, so cleanup runs there instead of at scope exit. */
  readonly consumedByInvocation: boolean
}

/** One resolved callable field enriched with everything construction through cleanup needs. */
export interface Realization {
  readonly _tag: 'CallableFieldRealization'
  readonly field: RepresentationField.Id
  readonly instance: Type.Nominal
  readonly contract: Type.Callable
  readonly target: StaticTarget
  readonly targetArguments: ReadonlyArray<Type.GenericArgument>
  /** Present when the target is reached through a capturing section rather than a named function. */
  readonly environment?: Type.CallableEnvironmentIdentity
  /** The specialized environment's own site, which names its lanes to layout, MIR, and cleanup. */
  readonly site?: Hir.CallableSiteId
  readonly captures: ReadonlyArray<CaptureSlot>
  readonly invocation: ReceiverAccess
  readonly loans: ReadonlyArray<LoanDependency>
  readonly liveness: Liveness
  readonly cleanup: Cleanup
}

/** Why one resolved representation field has no runtime realization. */
export type UnsupportedReason =
  | { readonly _tag: 'UnresolvedRepresentation' }
  | { readonly _tag: 'EffectRepresentation' }
  | { readonly _tag: 'NonCallableBound' }
  | { readonly _tag: 'MissingCallableEnvironment'; readonly environment: string }
  | { readonly _tag: 'AmbiguousCallableEnvironment'; readonly environment: string }
  | {
      readonly _tag: 'UnsupportedCaptureLayout'
      readonly capture: number
      readonly type: Type.Type
    }

/** The complete support proof one storage fence consults before admitting a stored callable. */
export type Support =
  | { readonly _tag: 'Supported'; readonly realization: Realization }
  | {
      readonly _tag: 'Unsupported'
      readonly field: RepresentationField.Id
      readonly instance: Type.Nominal
      readonly reason: UnsupportedReason
    }

/** One deterministic realization lookup entry keyed by complete instance and field identity. */
export interface Entry {
  readonly _tag: 'CallableFieldRealizationEntry'
  readonly key: string
  readonly support: Support
}

/** Complete-instance lookup table for realized and explicitly unsupported callable fields. */
export interface Index {
  readonly _tag: 'CallableFieldRealizationIndex'
  readonly entries: ReadonlyArray<Entry>
}

/** Canonical realization key; identical to the field resolution key it enriches. */
export const key = (instance: Type.Nominal, id: RepresentationField.Id): string =>
  RepresentationField.key(instance, id)

const ownedAccess = (access: CaptureAccess): boolean => access === 'Take'

const borrowedAccess = (access: CaptureAccess): boolean =>
  access === 'Shared' || access === 'Exclusive'

const captureSlot = (capture: Instances.CallableInstance['captures'][number]): CaptureSlot =>
  Object.freeze({
    _tag: 'CallableCaptureSlot' as const,
    ordinal: capture.ordinal,
    parameterOrdinal: capture.parameterOrdinal,
    access: capture.access,
    type: capture.type,
    owned: ownedAccess(capture.access),
    borrowed: borrowedAccess(capture.access),
  })

const compareCaptures = (left: CaptureSlot, right: CaptureSlot): number =>
  left.ordinal - right.ordinal || left.parameterOrdinal - right.parameterOrdinal

const loansOf = (captures: ReadonlyArray<CaptureSlot>): ReadonlyArray<LoanDependency> =>
  Object.freeze(
    captures.flatMap(
      (capture): ReadonlyArray<LoanDependency> =>
        capture.access === 'Shared' || capture.access === 'Exclusive'
          ? [
              Object.freeze({
                _tag: 'CallableCaptureLoan' as const,
                capture: capture.ordinal,
                access: capture.access,
                type: capture.type,
              }),
            ]
          : [],
    ),
  )

const livenessOf = (captures: ReadonlyArray<CaptureSlot>): Liveness => {
  const ownedLanes = captures.filter((capture) => capture.owned).length
  const borrowedLanes = captures.filter((capture) => capture.borrowed).length
  return Object.freeze({
    _tag: 'CallableFieldLiveness' as const,
    // This milestone keeps every representation-bearing nominal move-only, including the zero-lane
    // named-callable case, so borrow and move rules never depend on capture arity.
    moveOnly: true,
    ownedLanes,
    borrowedLanes,
  })
}

const cleanupOf = (captures: ReadonlyArray<CaptureSlot>, invocation: ReceiverAccess): Cleanup =>
  Object.freeze({
    _tag: 'CallableFieldCleanup' as const,
    lanes: Object.freeze(captures.flatMap((capture) => (capture.owned ? [capture.ordinal] : []))),
    consumedByInvocation: invocation === 'Take',
  })

/** Structural executable values need their own hidden identity and cannot be an inline lane yet. */
const hasUnsupportedCaptureLayout = (type: Type.Type): boolean =>
  Type.isCallable(type) ||
  Type.isEffect(type) ||
  Type.isRepresented(type) ||
  (Type.isFixedArray(type) && hasUnsupportedCaptureLayout(type.element))

const unsupported = (
  field: RepresentationField.Id,
  instance: Type.Nominal,
  reason: UnsupportedReason,
): Support => Object.freeze({ _tag: 'Unsupported', field, instance, reason })

/** The capture environment a retained representation argument names, or why it is unreachable. */
type EnvironmentCaptures =
  | {
      readonly _tag: 'ResolvedEnvironment'
      readonly slots: ReadonlyArray<CaptureSlot>
      readonly site?: Hir.CallableSiteId
    }
  | { readonly _tag: 'UnresolvedEnvironment'; readonly reason: UnsupportedReason }

const sameArguments = (
  left: ReadonlyArray<Type.GenericArgument>,
  right: ReadonlyArray<Type.GenericArgument>,
): boolean =>
  left.length === right.length &&
  left.every((argument, ordinal) => {
    const candidate = right.at(ordinal)
    return candidate !== undefined && Type.equalsGenericArgument(argument, candidate)
  })

const sameTarget = (left: StaticTarget, right: Instances.CallableInstance['target']): boolean =>
  left._tag === 'Declaration'
    ? right._tag === 'DeclarationCallableTarget' &&
      left.module === right.declaration.module &&
      left.name === right.declaration.name
    : right._tag === 'BuiltinCallableTarget' &&
      left.actor === right.actor &&
      left.operation === right.operation &&
      left.intrinsic.actor === right.intrinsic.actor &&
      left.intrinsic.name === right.intrinsic.name

export const matchesIdentity = (
  identity: Type.CallableIdentityArgument,
  candidate: Instances.CallableInstance,
): boolean =>
  identity.environment !== undefined &&
  Type.equalsCallableEnvironmentIdentity(
    identity.environment,
    Hir.callableEnvironmentIdentity(candidate.site, candidate.owner),
  ) &&
  sameTarget(identity.target, candidate.target) &&
  sameArguments(identity.typeArguments, candidate.typeArguments)

/** One capture shape signature, used only to detect indistinguishable environments. */
const captureShape = (callable: Instances.CallableInstance): string =>
  callable.captures
    .map(
      (capture) =>
        `${capture.ordinal}:${capture.parameterOrdinal}:${capture.access}:${Type.key(capture.type)}`,
    )
    .join(' ')

/**
 * Selects the specialized capture environment a section's retained identity names. A named function
 * carries no environment identity and therefore contributes no capture lanes at all.
 */
const environmentCaptures = (
  identity: Type.CallableIdentityArgument,
  callables: ReadonlyArray<Instances.CallableInstance>,
): EnvironmentCaptures => {
  const environment = identity.environment
  if (environment === undefined)
    return Object.freeze({ _tag: 'ResolvedEnvironment', slots: Object.freeze([]) })
  const candidates = callables.filter((callable) => matchesIdentity(identity, callable))
  const selected = candidates.at(0)
  if (selected === undefined)
    return Object.freeze({
      _tag: 'UnresolvedEnvironment',
      reason: Object.freeze({
        _tag: 'MissingCallableEnvironment',
        environment: Type.callableEnvironmentKey(environment),
      }),
    })
  if (new Set(candidates.map(captureShape)).size > 1)
    return Object.freeze({
      _tag: 'UnresolvedEnvironment',
      reason: Object.freeze({
        _tag: 'AmbiguousCallableEnvironment',
        environment: Type.callableEnvironmentKey(environment),
      }),
    })
  return Object.freeze({
    _tag: 'ResolvedEnvironment',
    slots: Object.freeze([...selected.captures.map(captureSlot)].sort(compareCaptures)),
    site: selected.site,
  })
}

/**
 * Enriches one resolved representation field into a runtime realization. The callable instances are
 * the discovery's already-specialized sections; this function never inspects initializer syntax.
 */
export const realizeField = (
  resolution: RepresentationField.Resolution,
  callables: ReadonlyArray<Instances.CallableInstance>,
): Support => {
  if (resolution._tag !== 'ResolvedRepresentationField')
    return unsupported(
      resolution.id,
      resolution.instance,
      Object.freeze({ _tag: 'UnresolvedRepresentation' }),
    )
  const identity = resolution.argument.identity
  if (!Type.isCallableIdentityArgument(identity))
    return unsupported(
      resolution.id,
      resolution.instance,
      Object.freeze({ _tag: 'EffectRepresentation' }),
    )
  const contract = Type.isCallable(resolution.requiredBound)
    ? resolution.requiredBound
    : Type.isCallable(resolution.argument.contract)
      ? resolution.argument.contract
      : undefined
  if (contract === undefined)
    return unsupported(
      resolution.id,
      resolution.instance,
      Object.freeze({ _tag: 'NonCallableBound' }),
    )
  const environment = identity.environment
  const captures = environmentCaptures(identity, callables)
  if (captures._tag === 'UnresolvedEnvironment')
    return unsupported(resolution.id, resolution.instance, captures.reason)
  const slots = captures.slots
  const unsupportedCapture = slots.find(
    (capture) => !capture.borrowed && hasUnsupportedCaptureLayout(capture.type),
  )
  if (unsupportedCapture !== undefined)
    return unsupported(
      resolution.id,
      resolution.instance,
      Object.freeze({
        _tag: 'UnsupportedCaptureLayout',
        capture: unsupportedCapture.ordinal,
        type: unsupportedCapture.type,
      }),
    )
  const invocation = contract.mode
  return Object.freeze({
    _tag: 'Supported',
    realization: Object.freeze({
      _tag: 'CallableFieldRealization' as const,
      field: resolution.id,
      instance: resolution.instance,
      contract,
      target: identity.target,
      targetArguments: Object.freeze([...identity.typeArguments]),
      ...(environment === undefined ? {} : { environment }),
      ...(captures.site === undefined ? {} : { site: captures.site }),
      captures: slots,
      invocation,
      loans: loansOf(slots),
      liveness: livenessOf(slots),
      cleanup: cleanupOf(slots, invocation),
    }),
  })
}

/** Realizes every callable field of one resolved field index in deterministic key order. */
export const realize = (
  fields: RepresentationField.Index,
  callables: ReadonlyArray<Instances.CallableInstance>,
): Index => {
  const entries = new Map<string, Entry>()
  for (const resolution of fields.resolutions) {
    const entryKey = key(resolution.instance, resolution.id)
    if (entries.has(entryKey)) continue
    entries.set(
      entryKey,
      Object.freeze({
        _tag: 'CallableFieldRealizationEntry' as const,
        key: entryKey,
        support: realizeField(resolution, callables),
      }),
    )
  }
  return Object.freeze({
    _tag: 'CallableFieldRealizationIndex',
    entries: Object.freeze(
      [...entries.values()].sort((left, right) =>
        left.key < right.key ? -1 : left.key > right.key ? 1 : 0,
      ),
    ),
  })
}

/** Looks one realization up by complete nominal instance and stable field identity. */
export const lookup = (
  self: Index,
  instance: Type.Nominal,
  id: RepresentationField.Id,
): Support | undefined => {
  const expected = key(instance, id)
  return self.entries.find((entry) => entry.key === expected)?.support
}

/** Returns the realization when the field is supported, and nothing when it stays fenced. */
export const realizationOf = (
  self: Index,
  instance: Type.Nominal,
  id: RepresentationField.Id,
): Realization | undefined => {
  const support = lookup(self, instance, id)
  return support?._tag === 'Supported' ? support.realization : undefined
}

/** True when every realized field of one complete instance has a runtime realization. */
export const supportsInstance = (self: Index, instance: Type.Nominal): boolean => {
  const entries = self.entries.filter((entry) =>
    Type.equals(
      entry.support._tag === 'Supported'
        ? entry.support.realization.instance
        : entry.support.instance,
      instance,
    ),
  )
  return entries.length > 0 && entries.every((entry) => entry.support._tag === 'Supported')
}

/** True when a discovered callable is the complete specialization this realization names. */
export const matchesCallable = (
  self: Realization,
  candidate: Instances.CallableInstance,
): boolean =>
  self.site !== undefined &&
  Hir.sameExecutableSite(self.site, candidate.site) &&
  sameTarget(self.target, candidate.target) &&
  sameArguments(self.targetArguments, candidate.typeArguments) &&
  self.environment !== undefined &&
  Type.equalsCallableEnvironmentIdentity(
    self.environment,
    Hir.callableEnvironmentIdentity(candidate.site, candidate.owner),
  )

/** Structural equality for the runtime fact owned by this actor. */
export const equals = (left: Realization, right: Realization): boolean =>
  key(left.instance, left.field) === key(right.instance, right.field) &&
  Type.equals(left.contract, right.contract) &&
  left.target._tag === right.target._tag &&
  (left.target._tag === 'Declaration' && right.target._tag === 'Declaration'
    ? left.target.module === right.target.module && left.target.name === right.target.name
    : left.target._tag === 'Builtin' && right.target._tag === 'Builtin'
      ? left.target.actor === right.target.actor &&
        left.target.operation === right.target.operation &&
        left.target.intrinsic.actor === right.target.intrinsic.actor &&
        left.target.intrinsic.name === right.target.intrinsic.name
      : false) &&
  sameArguments(left.targetArguments, right.targetArguments) &&
  ((left.environment === undefined && right.environment === undefined) ||
    (left.environment !== undefined &&
      right.environment !== undefined &&
      Type.equalsCallableEnvironmentIdentity(left.environment, right.environment))) &&
  ((left.site === undefined && right.site === undefined) ||
    (left.site !== undefined &&
      right.site !== undefined &&
      Hir.sameExecutableSite(left.site, right.site))) &&
  left.captures.length === right.captures.length &&
  left.captures.every((capture, ordinal) => {
    const candidate = right.captures.at(ordinal)
    return (
      candidate !== undefined &&
      capture.ordinal === candidate.ordinal &&
      capture.parameterOrdinal === candidate.parameterOrdinal &&
      capture.access === candidate.access &&
      Type.equals(capture.type, candidate.type) &&
      capture.owned === candidate.owned &&
      capture.borrowed === candidate.borrowed
    )
  }) &&
  left.invocation === right.invocation &&
  left.cleanup.consumedByInvocation === right.cleanup.consumedByInvocation &&
  left.cleanup.lanes.length === right.cleanup.lanes.length &&
  left.cleanup.lanes.every((lane, ordinal) => lane === right.cleanup.lanes.at(ordinal))

/** Every invocation mode one receiver access admits, weakest receiver first. */
export const admittedModes = (receiver: ReceiverAccess): ReadonlyArray<Type.CallableMode> =>
  receiver === 'Shared'
    ? Object.freeze(['Shared'] as const)
    : receiver === 'Exclusive'
      ? Object.freeze(['Shared', 'Exclusive'] as const)
      : Object.freeze(['Shared', 'Exclusive', 'Take'] as const)

/**
 * True when one aggregate receiver access may invoke one callable mode.
 *
 * Ownership decides this before specialization, from the field's semantic contract alone, and every
 * later phase decides it from the realization. Both reach the same rule through this one function so
 * the pre-layout rejection and the runtime invocation can never disagree.
 */
export const admitsMode = (receiver: ReceiverAccess, mode: Type.CallableMode): boolean =>
  admittedModes(receiver).includes(mode)

/** The weaker of two receiver accesses; a borrow anywhere in a place weakens the whole place. */
export const weakerAccess = (left: ReceiverAccess, right: ReceiverAccess): ReceiverAccess =>
  admittedModes(left).length <= admittedModes(right).length ? left : right

/** True when one aggregate receiver access may invoke a realization's callable mode. */
export const admitsInvocation = (self: Realization, receiver: ReceiverAccess): boolean =>
  admitsMode(receiver, self.invocation)
