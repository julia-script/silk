import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as Hir from './Hir.js'
import * as Intrinsic from './Intrinsic.js'
import * as Ownership from './Ownership.js'
import * as RepresentationField from './RepresentationField.js'
import * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

/**
 * Instance discovery: which concrete runtime instances are reachable from the user entry. Keys
 * are canonical declaration identities plus normalized type and contract-row arguments — both
 * empty in the frozen slice. The worklist records an instance before following it, so ordinary
 * recursion terminates.
 */

/** One normalized concrete instance key. */
export interface InstanceKey {
  readonly _tag: 'InstanceKey'
  readonly declaration: DeclarationIndex.CanonicalId
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly contractRow: ReadonlyArray<string>
}

/** One discovered instance with its elaborated HIR function. */
export interface Instance {
  readonly _tag: 'Instance'
  readonly key: InstanceKey
  readonly function: Hir.HirFunction
  readonly substitution: Type.Substitution
  readonly resultEffect?: string
  readonly effectSuccesses?: ReadonlyArray<{
    readonly site: Hir.EffectSiteId
    readonly identity: string
  }>
}

/** One concrete hidden callable-section construction reachable from an instance. */
export interface CallableInstance {
  readonly _tag: 'CallableInstance'
  readonly owner: InstanceKey
  readonly site: Hir.CallableSiteId
  readonly target: Hir.CallableTarget
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly substitution: Type.Substitution
  readonly captureTypes: ReadonlyArray<Type.Type>
  readonly captures: ReadonlyArray<{
    readonly ordinal: number
    readonly parameterOrdinal: number
    readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
    readonly type: Type.Type
  }>
  readonly type: Type.Callable
  readonly mode: Type.CallableMode
}

/** One monomorphic ordinary/effect constructor call with hidden Effect identities resolved. */
export interface CallInstance {
  readonly _tag: 'CallInstance'
  readonly owner: InstanceKey
  readonly span: Hir.Expression['span']
  readonly target: InstanceKey
  readonly resultEffect?: string
}

/** One exact sealed intrinsic call retained by executable instance closure. */
export interface IntrinsicCall {
  readonly _tag: 'ReachableIntrinsicCall'
  readonly operation: Intrinsic.OperationId
  readonly span: Hir.Expression['span']
}

/** One normalized reportable failure retained by an effectful user entry. */
export interface EntryFailure {
  readonly type: Type.Nominal
  readonly report: string
}

/** The resolved or explicitly unavailable user entry. */
export type Entry =
  | { readonly _tag: 'Resolved'; readonly kind: 'Ordinary'; readonly key: InstanceKey }
  | {
      readonly _tag: 'Resolved'
      readonly kind: 'Effect'
      readonly key: InstanceKey
      readonly failures: ReadonlyArray<EntryFailure>
      readonly requirements: ReadonlyArray<Type.Requirement>
    }
  | {
      readonly _tag: 'Unavailable'
      readonly reason:
        | 'MissingEntry'
        | 'AmbiguousEntry'
        | 'GenericEntry'
        | 'ParameterizedEntry'
        | 'UntypedEntry'
        | 'InvalidOrdinaryEntryResult'
        | 'InvalidEffectEntryResult'
        | 'EffectEntryRequirements'
        | 'UnreportableEntryFailure'
        | 'InvalidSource'
    }

/** The deterministic discovery result. */
export interface Discovery {
  readonly _tag: 'InstanceDiscovery'
  readonly rootModule: string
  readonly entry: Entry
  readonly instances: ReadonlyArray<Instance>
  readonly callables: ReadonlyArray<CallableInstance>
  readonly calls: ReadonlyArray<CallInstance>
  readonly intrinsics: ReadonlyArray<IntrinsicCall>
  /** Exact concrete function executions that can suspend, excluding lazy result runners. */
  readonly suspendableExecutions: ReadonlyArray<InstanceKey>
  readonly suspendable: ReadonlyArray<InstanceKey>
  readonly suspendableEffects: ReadonlyArray<string>
  readonly violations: ReadonlyArray<PolymorphicRecursion>
}

/** A recursive generic edge that changes an ancestor declaration's concrete arguments. */
export interface PolymorphicRecursion {
  readonly _tag: 'PolymorphicRecursion'
  readonly caller: InstanceKey
  readonly target: InstanceKey
}

/**
 * Rejects reachable Drop hook instantiations whose concrete provider is Copy. A parametric
 * conformance defers the header-time Copy prohibition here, where the arguments are known.
 */
export const copyDropViolations = (
  self: Discovery,
  index: DeclarationIndex.Index,
): ReadonlyArray<Diagnostic.Diagnostic> =>
  Object.freeze(
    self.instances.flatMap((instance) => {
      // Hook members carry the compiler-reserved canonical name shape `drop@impl#<ordinal>`.
      if (!instance.key.declaration.name.startsWith('drop@impl#')) return []
      if (instance.key.typeArguments.length === 0) return []
      const parameter = instance.function.declaration.parameters.at(0)
      if (parameter?.declaredType._tag !== 'Resolved') return []
      const self_ = Type.substitute(parameter.declaredType.type, instance.substitution)
      if (!Type.isReference(self_)) return []
      const provider = self_.target
      return DeclarationIndex.copyType(index, provider)
        ? [
            Diagnostic.invalidDropHook(
              `Copy type ${Type.encode(provider)} cannot implement Drop`,
              instance.function.declaration.syntax.span,
            ),
          ]
        : []
    }),
  )

const requirementBindings = (
  fn: Hir.HirFunction,
): ReadonlyArray<Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>> =>
  fn.statements.flatMap((statement) =>
    Hir.statementExpressions(statement).flatMap((expression) =>
      Hir.expressionTree(expression).flatMap((candidate) =>
        candidate._tag === 'EffectBindRequirement' ? [candidate] : [],
      ),
    ),
  )

const requirementBindingWitness = (
  binding: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>,
  substitution: Type.Substitution,
  index: DeclarationIndex.Index,
): DeclarationIndex.ConformanceWitness | undefined => {
  const capability = Type.substitute(binding.provider.capability, substitution)
  const provider = Type.substitute(binding.provider.providerType, substitution)
  return Type.isNominal(capability) && Type.isNominal(provider)
    ? (binding.provider.witness ?? DeclarationIndex.witness(index, provider, capability))
    : undefined
}

const forwardedRequirementBinding = (
  fn: Hir.HirFunction,
): Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }> | undefined => {
  const returned = fn.statements.at(-1)
  if (fn.statements.length !== 1 || returned?._tag !== 'Return') return undefined
  const block = returned.expression
  if (block._tag !== 'EffectBlock' || block.statements.length !== 2) return undefined
  const binding = block.statements.at(0)
  const completed = block.statements.at(1)
  if (
    binding?._tag !== 'Bind' ||
    binding.initializer._tag !== 'EffectBindRequirement' ||
    binding.initializer.provider.access !== 'Exclusive' ||
    binding.initializer.protected._tag !== 'Move' ||
    binding.initializer.protected.subject._tag !== 'ParameterReference' ||
    binding.initializer.protected.subject.parameter.ordinal !== 0 ||
    binding.initializer.provider.parameter?.ordinal !== 1 ||
    completed?._tag !== 'Return' ||
    completed.expression._tag !== 'Run' ||
    completed.expression.subject._tag !== 'BindingReference' ||
    completed.expression.subject.binding.ordinal !== binding.binding.ordinal
  )
    return undefined
  return binding.initializer
}

/** Rejects concrete requirement bindings whose provider does not implement the capability. */
export const requirementBindingViolations = (
  self: Discovery,
  index: DeclarationIndex.Index,
): ReadonlyArray<Diagnostic.Diagnostic> =>
  Object.freeze(
    self.instances.flatMap((instance) =>
      requirementBindings(instance.function).flatMap((binding) => {
        const capability = Type.substitute(binding.provider.capability, instance.substitution)
        const provider = Type.substitute(binding.provider.providerType, instance.substitution)
        if (requirementBindingWitness(binding, instance.substitution, index) !== undefined)
          return []
        return [
          Diagnostic.invalidEffectProvision(
            `provider type ${Type.encode(provider)} does not match ${Type.encode(capability)}`,
            binding.provider.span,
          ),
        ]
      }),
    ),
  )

/**
 * Rejects reachable bound operation calls whose specialization selects no witness that lowers.
 *
 * A bound operation names its answer twice over: the conformance says which witness answers it, and
 * the witness kind says how — a sealed intrinsic, or a function of the provider's own actor. Both
 * are lowered, so a conformance that selects neither leaves the call with nothing to run. Lowering
 * has no diagnostic channel and would drop the call, leaving the specialized instance to fail MIR
 * validation with no user-visible cause, so the check runs here, where the substitution is already
 * known and diagnostics are already reported.
 *
 * Every witness today's conformance checker admits is one of the two kinds, so this reports nothing
 * for source the frontend accepts. That agreement is the invariant worth pinning: it is what broke
 * when witness admissibility widened and lowering was not taught to look, and a call that passes
 * analysis and produces no code is a reported error rather than a silent miscompile.
 */
export const unlowerableWitnessViolations = (
  self: Discovery,
  index: DeclarationIndex.Index,
): ReadonlyArray<Diagnostic.Diagnostic> =>
  Object.freeze(
    self.instances.flatMap((instance) =>
      instance.function.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .flatMap((expression) => {
          if (expression._tag !== 'BoundOperationCall') return []
          const capability = Type.substitute(expression.capability, instance.substitution)
          const provider = Type.substitute(expression.provider, instance.substitution)
          if (!Type.isNominal(capability)) return []
          const intrinsic = DeclarationIndex.interfaceOperationIntrinsic(
            index,
            provider,
            capability,
            expression.operation,
          )
          const witness = DeclarationIndex.interfaceWitnessImplementation(
            index,
            provider,
            capability,
            expression.operation,
          )
          if (intrinsic?.rule._tag === 'BuiltinRule' || witness !== undefined) return []
          return [
            Diagnostic.unlowerableBoundWitness(
              `${capability.name}.${expression.operation}`,
              Type.encode(provider),
              expression.span,
            ),
          ]
        }),
    ),
  )

/**
 * Rejects reachable constructions that would store a bare callable value inside an aggregate.
 *
 * A direct callable value keeps its hidden concrete identity beside its structural type, so
 * higher-order calls monomorphize; a `Construct` or `ArrayConstruct` result carries only the
 * declared type, and nominal layout planning then meets a bare `fn(...) -> ...` field it refuses
 * to size (#184). Lowering has no diagnostic channel and the program would otherwise pass a clean
 * frontend and die in MIR validation, so the check runs here, where each instance's substitution
 * is concrete and diagnostics are already reported.
 *
 * Scoped to reachable instances on purpose: a callable-bearing struct that is only declared, or
 * only constructed in unreachable code, compiles and runs today and stays accepted.
 *
 * Provenance follows where the callable was written. A construction whose declared type already
 * stores a callable — `Parser<A> { decode: fn(i32) -> A }` — names the fault itself, so its own
 * span is primary. One that stores a callable only after substitution — `Holder<T> { value: T }`
 * specialized at a callable — was decided at the call that chose the type arguments, so the
 * earliest specializing call site (by source ID, then position) is primary and the generic body's
 * construction is retained as related provenance. That keeps a stdlib-internal construction such
 * as `Option.some(i32.add(1))` pointing at the user's call rather than into `silk/option`.
 */
export interface StoredRepresentation {
  readonly path: ReadonlyArray<string>
  readonly contract: Type.RepresentationBound
  readonly open: boolean
}

/** Finds represented storage exclusively through the specialization field-resolution seam. */
export const storedRepresentation = (
  index: DeclarationIndex.Index,
  type: Type.Type,
  kind: 'Callable' | 'Effect',
  seen: ReadonlySet<string> = new Set(),
): StoredRepresentation | undefined => {
  if (Type.isRepresented(type)) {
    if (
      (kind === 'Callable' && !Type.isCallable(type.contract)) ||
      (kind === 'Effect' && !Type.isEffect(type.contract))
    )
      return undefined
    return Object.freeze({
      path: Object.freeze([]),
      contract: type.contract,
      open: Type.isRepresentationParameterArgument(type.representation.argument),
    })
  }
  if (Type.isFixedArray(type) || Type.isSlice(type))
    return storedRepresentation(index, type.element, kind, seen)
  if (Type.isUnion(type)) {
    for (const member of type.members) {
      const found = storedRepresentation(index, member, kind, seen)
      if (found !== undefined) return found
    }
    return undefined
  }
  if (!Type.isNominal(type) || Type.isIntrinsicNominal(type)) return undefined
  const typeKey = Type.key(type)
  if (seen.has(typeKey)) return undefined
  const declaration = DeclarationIndex.byCanonical(index, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (declaration?._tag !== 'StructDeclaration') return undefined
  const substitution =
    Type.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      type.arguments,
    ) ?? new Map()
  const fieldIndex = RepresentationField.resolveFields(index, [type])
  const plans = RepresentationField.plansOf(index, type)
  const next = new Set(seen).add(typeKey)
  for (const [ordinal, field] of declaration.fields.entries()) {
    if (field.declaredType._tag !== 'Resolved' || field.name._tag !== 'Present') continue
    const fieldPlans = plans.filter((candidate) => candidate.id.ordinal === ordinal)
    for (const plan of fieldPlans) {
      const resolution = RepresentationField.lookup(fieldIndex, type, plan.id)
      if (resolution !== undefined) {
        const contract =
          resolution._tag === 'ResolvedRepresentationField'
            ? resolution.argument.contract
            : resolution.reason.requiredBound
        if (
          (kind === 'Callable' && Type.isCallable(contract)) ||
          (kind === 'Effect' && Type.isEffect(contract))
        )
          return Object.freeze({
            path: Object.freeze([field.name.spelling]),
            contract,
            open: resolution._tag === 'UnavailableRepresentationField',
          })
      }
    }
    const nested = storedRepresentation(
      index,
      Type.substitute(field.declaredType.type, substitution),
      kind,
      next,
    )
    if (nested !== undefined)
      return Object.freeze({
        path: Object.freeze([field.name.spelling, ...nested.path]),
        contract: nested.contract,
        open: nested.open,
      })
  }
  return undefined
}

/** Orders call sites by source ID, then position, for a deterministic primary origin. */
const compareCallSites = (left: CallInstance, right: CallInstance): number =>
  left.span.sourceId === right.span.sourceId
    ? left.span.start - right.span.start || left.span.end - right.span.end
    : left.span.sourceId < right.span.sourceId
      ? -1
      : 1

interface StoredExecutable {
  readonly path: ReadonlyArray<string>
  readonly contract: Type.RepresentationBound
  readonly represented: boolean
  readonly open: boolean
}

const storedExecutable = (
  index: DeclarationIndex.Index,
  type: Type.Type,
  kind: 'Callable' | 'Effect',
): StoredExecutable | undefined => {
  if (kind === 'Callable') {
    const bare = DeclarationIndex.storedCallable(index, type)
    if (bare !== undefined)
      return Object.freeze({
        path: bare.path,
        contract: bare.callable,
        represented: false,
        open: false,
      })
  }
  const represented = storedRepresentation(index, type, kind)
  return represented === undefined
    ? undefined
    : Object.freeze({
        ...represented,
        represented: true,
      })
}

/** Stable semantic identity of one executable-storage violation before presentation encoding. */
interface StoredExecutableViolationKey {
  readonly aggregate: Type.Type
  readonly path: ReadonlyArray<string>
  readonly contract: Type.RepresentationBound
  readonly represented: boolean
  readonly span: SourceSpan.SourceSpan
  readonly constructionSpan: SourceSpan.SourceSpan
}

const sameStoredExecutableViolationKey = (
  left: StoredExecutableViolationKey,
  right: StoredExecutableViolationKey,
): boolean =>
  Type.equals(left.aggregate, right.aggregate) &&
  left.path.length === right.path.length &&
  left.path.every((part, index) => part === right.path[index]) &&
  Type.equals(left.contract, right.contract) &&
  left.represented === right.represented &&
  SourceSpan.equals(left.span, right.span) &&
  SourceSpan.equals(left.constructionSpan, right.constructionSpan)

/** Collects every reachable aggregate construction that retains executable storage. */
const storedExecutableViolations = (
  self: Discovery,
  index: DeclarationIndex.Index,
  kind: 'Callable' | 'Effect',
): ReadonlyArray<Diagnostic.Diagnostic> => {
  const specializingCalls = new Map<string, CallInstance>()
  for (const call of self.calls) {
    const target = keyText(call.target)
    const current = specializingCalls.get(target)
    if (current === undefined || compareCallSites(call, current) < 0)
      specializingCalls.set(target, call)
  }
  const reported: Array<StoredExecutableViolationKey> = []
  return Object.freeze(
    self.instances.flatMap((instance) =>
      instance.function.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .flatMap((expression) => {
          if (expression._tag !== 'Construct' && expression._tag !== 'ArrayConstruct') return []
          const aggregate = Type.substitute(expression.type, instance.substitution)
          const found = storedExecutable(index, aggregate, kind)
          if (found === undefined) return []
          const declared = storedExecutable(index, expression.type, kind)
          const specializing =
            declared === undefined || declared.open
              ? specializingCalls.get(keyText(instance.key))
              : undefined
          const span = specializing?.span ?? expression.span
          const constructionSpan = expression.span
          const key: StoredExecutableViolationKey = {
            aggregate,
            path: found.path,
            contract: found.contract,
            represented: found.represented,
            span,
            constructionSpan,
          }
          if (reported.some((candidate) => sameStoredExecutableViolationKey(candidate, key)))
            return []
          reported.push(key)
          const path = found.path.length === 0 ? undefined : found.path.join('.')
          const related = specializing === undefined ? undefined : expression.span
          if (kind === 'Callable' && Type.isCallable(found.contract))
            return [
              Diagnostic.storedCallableConstruction(
                Type.encode(aggregate),
                path,
                Type.encode(found.contract),
                span,
                related,
                found.represented,
              ),
            ]
          if (kind === 'Effect' && Type.isEffect(found.contract))
            return [
              Diagnostic.storedRepresentedEffectConstruction(
                Type.encode(aggregate),
                path,
                Type.encode(found.contract),
                span,
                related,
              ),
            ]
          return []
        }),
    ),
  )
}

/** Rejects reachable constructions that retain bare or represented callable values. */
export const storedCallableViolations = (
  self: Discovery,
  index: DeclarationIndex.Index,
): ReadonlyArray<Diagnostic.Diagnostic> => storedExecutableViolations(self, index, 'Callable')

/** Rejects reachable constructions that retain represented Effect values. */
export const storedEffectViolations = (
  self: Discovery,
  index: DeclarationIndex.Index,
): ReadonlyArray<Diagnostic.Diagnostic> => storedExecutableViolations(self, index, 'Effect')

/** Produces semantic diagnostics for every finite-discovery violation. */
export const violationDiagnostics = (self: Discovery): ReadonlyArray<Diagnostic.Diagnostic> =>
  Object.freeze(
    self.violations.flatMap((violation) => {
      const caller = self.instances.find(
        (instance) => keyText(instance.key) === keyText(violation.caller),
      )
      if (caller === undefined) return []
      const callerText = `${violation.caller.declaration.name}<${violation.caller.typeArguments
        .map(Type.encodeGenericArgument)
        .join(', ')}>`
      const targetText = `${violation.target.declaration.name}<${violation.target.typeArguments
        .map(Type.encodeGenericArgument)
        .join(', ')}>`
      return [
        Diagnostic.polymorphicRecursion(
          callerText,
          targetText,
          caller.function.declaration.syntax.span,
        ),
      ]
    }),
  )

/** Retains an explicit unavailable entry when frontend errors prevent discovery. */
export const invalid = (rootModule: string): Discovery =>
  Object.freeze({
    _tag: 'InstanceDiscovery',
    rootModule,
    entry: Object.freeze({ _tag: 'Unavailable', reason: 'InvalidSource' }),
    instances: Object.freeze([]),
    callables: Object.freeze([]),
    calls: Object.freeze([]),
    intrinsics: Object.freeze([]),
    suspendableExecutions: Object.freeze([]),
    suspendable: Object.freeze([]),
    suspendableEffects: Object.freeze([]),
    violations: Object.freeze([]),
  })

const compareIntrinsicCalls = (left: IntrinsicCall, right: IntrinsicCall): number =>
  Intrinsic.operationText(left.operation).localeCompare(Intrinsic.operationText(right.operation)) ||
  left.span.sourceId.localeCompare(right.span.sourceId) ||
  left.span.start - right.span.start ||
  left.span.end - right.span.end

/**
 * Collects the canonical intrinsic identities retained by reachable function instances.
 *
 * A bound operation names no intrinsic until its type argument is known, so its identity is read
 * from the witness this instance's substitution selects. Two instances of one body therefore
 * contribute two identities, which is exactly what target availability has to see.
 */
const reachableIntrinsics = (
  instances: ReadonlyArray<Instance>,
  index: DeclarationIndex.Index,
): ReadonlyArray<IntrinsicCall> => {
  const calls = new Map<string, IntrinsicCall>()
  for (const instance of instances) {
    for (const statement of instance.function.statements) {
      for (const root of Hir.statementExpressions(statement)) {
        for (const expression of Hir.expressionTree(root)) {
          const selected =
            expression._tag === 'BoundOperationCall'
              ? (() => {
                  const capability = Type.substitute(expression.capability, instance.substitution)
                  return Type.isNominal(capability)
                    ? DeclarationIndex.interfaceOperationIntrinsic(
                        index,
                        Type.substitute(expression.provider, instance.substitution),
                        capability,
                        expression.operation,
                      )?.id
                    : undefined
                })()
              : undefined
          const operation =
            expression._tag === 'BuiltinCall'
              ? expression.intrinsic
              : (expression._tag === 'FunctionItem' || expression._tag === 'CallableSection') &&
                  expression.target._tag === 'BuiltinCallableTarget'
                ? expression.target.intrinsic
                : selected
          if (operation === undefined) continue
          const span = expression.span
          const key = `${Intrinsic.operationText(operation)}\u0000${span.sourceId}\u0000${span.start}\u0000${span.end}`
          calls.set(key, Object.freeze({ _tag: 'ReachableIntrinsicCall', operation, span }))
        }
      }
    }
  }
  return Object.freeze([...calls.values()].sort(compareIntrinsicCalls))
}

const keyOf = (
  declaration: DeclarationIndex.CanonicalId,
  contract: Hir.ContractFact,
  typeParameters: ReadonlyArray<Type.Parameter> = [],
  typeArguments: ReadonlyArray<Type.GenericArgument> = [],
): InstanceKey =>
  (() => {
    const substitution = Type.substitution(
      typeParameters,
      typeArguments.filter((argument) => !Type.isHiddenIdentityArgument(argument)),
    )
    if (substitution === undefined) {
      throw new RangeError('Instance key type arguments do not match declaration parameters')
    }
    return Object.freeze({
      _tag: 'InstanceKey',
      declaration,
      typeArguments: Object.freeze(Array.from(typeArguments)),
      contractRow:
        contract._tag === 'Contract'
          ? Object.freeze([
              ...contract.parameters.map((type) => Type.key(Type.substitute(type, substitution))),
              `result:${Type.key(Type.substitute(contract.result, substitution))}`,
              ...(contract.failures ?? []).map(
                (failure) => `failure:${Type.key(Type.substitute(failure, substitution))}`,
              ),
              ...(contract.requirements ?? []).map((requirement) => {
                const capability = Type.substitute(requirement.capability, substitution)
                return `requirement:${requirement.access}:${Type.key(capability)}@${requirement.role}`
              }),
            ])
          : Object.freeze([]),
    })
  })()

export const keyText = (key: InstanceKey): string =>
  `${key.declaration.module}\u0000${key.declaration.name}\u0000${key.typeArguments
    .map(Type.genericArgumentKey)
    .join('\u0000')}\u0002${key.contractRow.join('\u0000')}`

export const effectIdentity = (owner: InstanceKey, site: Hir.EffectSiteId): string =>
  `${keyText(owner)}\u0004${Hir.executableSiteKey(site)}`

const resolveEntry = (root: Elaboration.Result, index: DeclarationIndex.Index): Entry => {
  const lookup = Elaboration.declarationByName(root, 'main')
  if (lookup._tag === 'Missing')
    return Object.freeze({ _tag: 'Unavailable', reason: 'MissingEntry' })
  if (lookup._tag === 'Ambiguous') {
    return Object.freeze({ _tag: 'Unavailable', reason: 'AmbiguousEntry' })
  }
  const declaration = lookup.declaration
  if (declaration.typeParameters.length > 0) {
    return Object.freeze({ _tag: 'Unavailable', reason: 'GenericEntry' })
  }
  if (declaration.parameterCount > 0) {
    return Object.freeze({ _tag: 'Unavailable', reason: 'ParameterizedEntry' })
  }
  if (
    declaration.visibility !== 'Public' ||
    declaration.returnType._tag !== 'Resolved' ||
    declaration.canonical._tag !== 'Canonical' ||
    !declaration.failureRow.available ||
    !declaration.requirementRow.available
  ) {
    return Object.freeze({ _tag: 'Unavailable', reason: 'UntypedEntry' })
  }
  if (declaration.functionKind === 'Ordinary') {
    if (
      declaration.failureRow.failures.length !== 0 ||
      declaration.requirementRow.requirements.length !== 0 ||
      declaration.returnType.type !== 'i32'
    ) {
      return Object.freeze({ _tag: 'Unavailable', reason: 'InvalidOrdinaryEntryResult' })
    }
    return Object.freeze({
      _tag: 'Resolved',
      kind: 'Ordinary',
      key: keyOf(declaration.canonical.id, Hir.contractOf(declaration)),
    })
  }
  if (!Type.equals(declaration.returnType.type, Type.unit)) {
    return Object.freeze({ _tag: 'Unavailable', reason: 'InvalidEffectEntryResult' })
  }
  if (declaration.requirementRow.requirements.length > 0) {
    return Object.freeze({ _tag: 'Unavailable', reason: 'EffectEntryRequirements' })
  }
  if (
    declaration.failureRow.failures.some(
      (failure) => !DeclarationIndex.conforms(index, failure, Type.reportCapability),
    )
  ) {
    return Object.freeze({ _tag: 'Unavailable', reason: 'UnreportableEntryFailure' })
  }
  return Object.freeze({
    _tag: 'Resolved',
    kind: 'Effect',
    key: keyOf(declaration.canonical.id, Hir.contractOf(declaration)),
    requirements: Object.freeze(declaration.requirementRow.requirements),
    failures: Object.freeze(
      declaration.failureRow.failures.map((failure) =>
        Object.freeze({ type: failure, report: Type.encode(failure) }),
      ),
    ),
  })
}

interface CallTarget {
  readonly declaration: DeclarationIndex.CanonicalId
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly structurallyDescending?: boolean
}

const witnessCallTargets = (
  witness: DeclarationIndex.ConformanceWitness | undefined,
): ReadonlyArray<CallTarget> =>
  witness?._tag === 'SourceConformanceWitness'
    ? witness.operations.map((operation) =>
        Object.freeze({
          declaration: operation.implementation,
          typeArguments: Object.freeze([]),
        }),
      )
    : []

/** Collects every Drop hook a cleanup plan will invoke, so cleanup reaches hook instances. */
const hookCalls = (cleanup: Ownership.CleanupPlan): ReadonlyArray<CallTarget> => {
  switch (cleanup._tag) {
    case 'HookCleanup':
      return [
        Object.freeze({ declaration: cleanup.hook, typeArguments: cleanup.typeArguments }),
        ...hookCalls(cleanup.inner),
      ]
    case 'StructCleanup':
      return cleanup.fields.flatMap((field) => hookCalls(field.cleanup))
    case 'ArrayCleanup':
      return hookCalls(cleanup.element)
    case 'UnionCleanup':
      return cleanup.cases.flatMap((entry) => hookCalls(entry.cleanup))
    case 'RawBufferCleanup':
      return hookCalls(cleanup.allocation)
    case 'CallableCleanup':
      return cleanup.slots.flatMap((slot) => hookCalls(slot.cleanup))
    default:
      return []
  }
}

/**
 * Reports whether a call must carry hidden identity arguments to be lowerable.
 *
 * Effect and callable values are both compiler-private: neither has a target layout, and both are
 * erased by monomorphizing the target on the argument's hidden concrete identity. A call passing
 * either therefore cannot be specialized on its explicit type arguments alone — the target would
 * be instantiated without the identity, and lowering would drop the parameter it cannot type.
 */
const carriesHiddenIdentity = (
  expression: Extract<Hir.Expression, { readonly _tag: 'Call' | 'EffectConstruct' }>,
): boolean =>
  Type.isEffect(expression.type) ||
  expression.arguments.some(
    (argument) =>
      argument._tag !== 'Unavailable' &&
      (Type.isEffect(argument.type) || Type.isCallable(argument.type)),
  )

const callTargets = (expression: Hir.Expression): ReadonlyArray<CallTarget> => {
  if (expression._tag === 'Run') return callTargets(expression.subject)
  if (expression._tag === 'EffectResult') return callTargets(expression.protected)
  if (expression._tag === 'EffectCatch')
    return [...callTargets(expression.protected), ...callTargets(expression.handler)]
  if (expression._tag === 'EffectBindRequirement') {
    // A source-declared witness makes provision dispatch to its qualified operation, so the
    // operation is reachable even though no ordinary call names it.
    const witness = expression.provider.witness
    return [...callTargets(expression.protected), ...witnessCallTargets(witness)]
  }
  if (expression._tag === 'Move') return callTargets(expression.subject)
  if (expression._tag === 'RuntimeStringView') return callTargets(expression.source)
  if (expression._tag === 'StringEquality' || expression._tag === 'ShortCircuit') {
    return [...callTargets(expression.left), ...callTargets(expression.right)]
  }
  if (expression._tag === 'UnionConvert') return callTargets(expression.source)
  if (expression._tag === 'Project') return callTargets(expression.subject)
  if (expression._tag === 'IndexPlace') {
    return [...callTargets(expression.subject), ...callTargets(expression.index)]
  }
  if (expression._tag === 'SliceLength') return callTargets(expression.slice)
  if (expression._tag === 'SliceIndexPlace') {
    return [...callTargets(expression.slice), ...callTargets(expression.index)]
  }
  if (expression._tag === 'Construct') {
    return expression.fields.flatMap((field) => callTargets(field.value))
  }
  if (expression._tag === 'ArrayConstruct') {
    return expression.elements.flatMap((element) => callTargets(element))
  }
  if (expression._tag === 'BuiltinCall' || expression._tag === 'BoundOperationCall') {
    return expression.arguments.flatMap((argument) => callTargets(argument))
  }
  if (expression._tag === 'FunctionItem') return []
  if (expression._tag === 'CallableSection') {
    return expression.captures.flatMap((capture) => callTargets(capture.value))
  }
  if (expression._tag === 'CallableApply') {
    return [
      ...callTargets(expression.callee),
      ...expression.arguments.flatMap((argument) => callTargets(argument)),
    ]
  }
  if (expression._tag === 'Match') {
    return [
      ...callTargets(expression.scrutinee),
      ...expression.arms.flatMap((arm) =>
        arm.reachable
          ? [...(arm.guard === undefined ? [] : callTargets(arm.guard)), ...callTargets(arm.result)]
          : [],
      ),
    ]
  }
  if (expression._tag === 'EffectBlock') {
    return expression.statements.flatMap((statement) =>
      Hir.statementExpressions(statement).flatMap(callTargets),
    )
  }
  if (
    expression._tag !== 'Call' &&
    expression._tag !== 'EffectConstruct' &&
    expression._tag !== 'ServiceEffectConstruct'
  )
    return []
  const nested = expression.arguments.flatMap((argument) => callTargets(argument))
  if (expression._tag === 'ServiceEffectConstruct') return nested
  return carriesHiddenIdentity(expression)
    ? nested
    : [
        Object.freeze({ declaration: expression.target, typeArguments: expression.typeArguments }),
        ...nested,
      ]
}

const bodyCallTargets = (fn: Hir.HirFunction): ReadonlyArray<CallTarget> =>
  fn.statements.flatMap((statement) => Hir.statementExpressions(statement).flatMap(callTargets))

const requirementBindingCallTargets = (
  fn: Hir.HirFunction,
  substitution: Type.Substitution,
  index: DeclarationIndex.Index,
): ReadonlyArray<CallTarget> =>
  (forwardedRequirementBinding(fn) === undefined ? requirementBindings(fn) : []).flatMap(
    (binding) => {
      const witness = requirementBindingWitness(binding, substitution, index)
      return witnessCallTargets(witness)
    },
  )

const forwardedRequirementCallTargets = (
  calls: ReadonlyArray<CallInstance>,
  results: ReadonlyMap<string, Elaboration.Result>,
  index: DeclarationIndex.Index,
): ReadonlyArray<CallTarget> =>
  calls.flatMap((call) => {
    const target = functionByKey(results, call.target)
    const binding = target === undefined ? undefined : forwardedRequirementBinding(target)
    if (target === undefined || binding === undefined) return []
    const substitution = instanceSubstitution(target, call.target)
    if (substitution === undefined) return []
    const witness = requirementBindingWitness(binding, substitution, index)
    return witnessCallTargets(witness)
  })

const slotDropHookTargets = (
  fn: Hir.HirFunction,
  index: DeclarationIndex.Index,
  substitution: Type.Substitution,
): ReadonlyArray<CallTarget> => {
  const walk = (expression: Hir.Expression): ReadonlyArray<CallTarget> => {
    const own =
      expression._tag === 'BuiltinCall' && expression.operation === 'SlotDrop'
        ? expression.typeArguments.flatMap((argument) =>
            hookCalls(Ownership.cleanupPlan(index, Type.substitute(argument, substitution))),
          )
        : []
    if (expression._tag === 'Match') {
      return [
        ...own,
        ...walk(expression.scrutinee),
        ...expression.arms.flatMap((arm) =>
          arm.reachable
            ? [...(arm.guard === undefined ? [] : walk(arm.guard)), ...walk(arm.result)]
            : [],
        ),
      ]
    }
    return [...own, ...Hir.expressionChildren(expression).flatMap(walk)]
  }
  return fn.statements.flatMap((statement) => Hir.statementExpressions(statement).flatMap(walk))
}

/**
 * Collects the provider functions a specialized body's bound operations dispatch to.
 *
 * A source witness is reachable through the operator that spells its operation, or through the
 * bound's own name when no operator spells it, and through no ordinary call — so discovery has to
 * read the conformance itself. Both spellings walk one conformance, because the witness a
 * specialization selects does not depend on how the body names the operation; a scalar argument
 * maps the same operation to a sealed intrinsic and contributes no target.
 */
const interfaceWitnessTargets = (
  fn: Hir.HirFunction,
  index: DeclarationIndex.Index,
  substitution: Type.Substitution,
): ReadonlyArray<CallTarget> => {
  const walk = (expression: Hir.Expression): ReadonlyArray<CallTarget> => {
    const bound =
      expression._tag === 'BuiltinCall'
        ? expression.interfaceOperation
        : expression._tag === 'BoundOperationCall'
          ? expression
          : undefined
    const capability =
      bound === undefined ? undefined : Type.substitute(bound.capability, substitution)
    const provider = bound === undefined ? undefined : Type.substitute(bound.provider, substitution)
    const target =
      bound === undefined ||
      provider === undefined ||
      capability === undefined ||
      !Type.isNominal(capability)
        ? undefined
        : DeclarationIndex.interfaceWitnessTarget(index, provider, capability, bound.operation)
    const dependencies =
      provider === undefined || capability === undefined || !Type.isNominal(capability)
        ? []
        : DeclarationIndex.interfaceWitnessDependencyTargets(index, provider, capability)
    // A conditional witness is generic in its header's binders, so the target carries the arguments
    // this specialization proved rather than reaching code through an unsubstituted declaration.
    const own =
      target === undefined && dependencies.length === 0
        ? []
        : [
            ...dependencies.map((dependency) =>
              Object.freeze({
                declaration: dependency.implementation,
                typeArguments: dependency.typeArguments,
                structurallyDescending: true,
              }),
            ),
            ...(target === undefined
              ? []
              : [
                  Object.freeze({
                    declaration: target.implementation,
                    typeArguments: target.typeArguments,
                    structurallyDescending: target.structurallyDescending,
                  }),
                ]),
          ]
    if (expression._tag === 'Match') {
      return [
        ...own,
        ...walk(expression.scrutinee),
        ...expression.arms.flatMap((arm) =>
          arm.reachable
            ? [...(arm.guard === undefined ? [] : walk(arm.guard)), ...walk(arm.result)]
            : [],
        ),
      ]
    }
    return [...own, ...Hir.expressionChildren(expression).flatMap(walk)]
  }
  return fn.statements.flatMap((statement) => Hir.statementExpressions(statement).flatMap(walk))
}

const callableBindings = (fn: Hir.HirFunction): ReadonlyMap<number, Hir.Expression> => {
  const bindings = new Map<number, Hir.Expression>()
  const expression = (value: Hir.Expression): void => {
    if (value._tag === 'EffectBlock') {
      statements(value.statements)
      return
    }
    for (const child of Hir.expressionChildren(value)) expression(child)
  }
  const statements = (body: ReadonlyArray<Hir.Statement>): void => {
    for (const statement of body) {
      if (statement._tag === 'Bind') {
        bindings.set(statement.binding.ordinal, statement.initializer)
        expression(statement.initializer)
      }
      if (statement._tag === 'Unsafe') statements(statement.statements)
      else if (statement._tag === 'If') {
        expression(statement.condition)
        statements(statement.taken)
        statements(statement.otherwise)
      } else if (statement._tag === 'While') {
        expression(statement.condition)
        statements(statement.body)
      } else if (statement._tag !== 'Bind') {
        for (const root of Hir.statementExpressions(statement)) expression(root)
      }
    }
  }
  statements(fn.statements)
  return bindings
}

const instanceSubstitution = (
  fn: Hir.HirFunction,
  key: InstanceKey,
): Type.Substitution | undefined =>
  Type.substitution(
    fn.declaration.typeParameters.map((parameter) => parameter.type),
    key.typeArguments.filter((argument) => !Type.isHiddenIdentityArgument(argument)),
  )

const effectParameterOrdinals = (
  fn: Hir.HirFunction,
  substitution: Type.Substitution,
): ReadonlyArray<number> =>
  fn.contract._tag === 'Contract'
    ? fn.contract.parameters.flatMap((parameter, ordinal) =>
        Type.isEffect(Type.substitute(parameter, substitution)) ? [ordinal] : [],
      )
    : Object.freeze([])

export const parameterEffectIdentity = (
  fn: Hir.HirFunction,
  key: InstanceKey,
  ordinal: number,
): string | undefined => {
  const substitution = instanceSubstitution(fn, key)
  if (substitution === undefined) return undefined
  const position = effectParameterOrdinals(fn, substitution).indexOf(ordinal)
  if (position < 0) return undefined
  return key.typeArguments.filter(Type.isEffectIdentityArgument).at(position)?.identity
}

const callableParameterOrdinals = (
  fn: Hir.HirFunction,
  substitution: Type.Substitution,
): ReadonlyArray<number> =>
  fn.contract._tag === 'Contract'
    ? fn.contract.parameters.flatMap((parameter, ordinal) =>
        Type.isCallable(Type.substitute(parameter, substitution)) ? [ordinal] : [],
      )
    : Object.freeze([])

export const parameterCallableIdentity = (
  fn: Hir.HirFunction,
  key: InstanceKey,
  ordinal: number,
): Type.CallableIdentityArgument | undefined => {
  const substitution = instanceSubstitution(fn, key)
  if (substitution === undefined) return undefined
  const position = callableParameterOrdinals(fn, substitution).indexOf(ordinal)
  if (position < 0) return undefined
  return key.typeArguments.filter(Type.isCallableIdentityArgument).at(position)
}

const callableTargetIdentity = (
  target: Hir.CallableTarget,
): Type.CallableIdentityArgument['target'] =>
  target._tag === 'DeclarationCallableTarget'
    ? Object.freeze({
        _tag: 'Declaration' as const,
        module: target.declaration.module,
        name: target.declaration.name,
      })
    : Object.freeze({
        _tag: 'Builtin' as const,
        actor: target.actor,
        operation: target.operation,
        intrinsic: target.intrinsic,
      })

const callableOriginOf = (
  expression: Hir.Expression,
  context: EffectOriginContext,
): Type.CallableIdentityArgument | undefined => {
  if (expression._tag === 'FunctionItem') {
    const target = callableTargetIdentity(expression.target)
    const identity =
      target._tag === 'Declaration'
        ? `declaration:${target.module}:${target.name}`
        : `builtin:${target.actor}:${target.operation}`
    return Type.callableIdentityArgument(identity, target)
  }
  if (expression._tag === 'CallableSection') {
    const typeArguments = expression.typeArguments.map((argument) =>
      Type.substituteGenericArgument(argument, context.substitution),
    )
    const identity = `${keyText(context.owner)}\u0001${Hir.executableSiteKey(expression.site)}\u0001${typeArguments.map(Type.genericArgumentKey).join('\u0000')}`
    return Type.callableIdentityArgument(
      identity,
      callableTargetIdentity(expression.target),
      typeArguments,
      identity,
    )
  }
  if (expression._tag === 'ParameterReference')
    return parameterCallableIdentity(context.fn, context.owner, expression.parameter.ordinal)
  if (expression._tag === 'BindingReference') {
    const initializer = callableBindings(context.fn).get(expression.binding.ordinal)
    return initializer === undefined ? undefined : callableOriginOf(initializer, context)
  }
  if (expression._tag === 'Move') return callableOriginOf(expression.subject, context)
  return undefined
}

const callableSubstitutionOf = (
  expression: Hir.Expression,
  context: EffectOriginContext,
): Type.Substitution => {
  if (expression._tag === 'CallableSection')
    return new Map(
      Array.from(expression.substitution.entries()).map(([key, argument]) => [
        key,
        Type.substituteGenericArgument(argument, context.substitution),
      ]),
    )
  if (expression._tag === 'BindingReference') {
    const initializer = callableBindings(context.fn).get(expression.binding.ordinal)
    return initializer === undefined ? new Map() : callableSubstitutionOf(initializer, context)
  }
  if (expression._tag === 'Move') return callableSubstitutionOf(expression.subject, context)
  return new Map()
}

const appliedCallableOriginOf = (
  expression: Extract<Hir.Expression, { readonly _tag: 'CallableApply' }>,
  context: EffectOriginContext,
): Type.CallableIdentityArgument | undefined => {
  const callable = callableOriginOf(expression.callee, context)
  if (callable?.target._tag !== 'Declaration') return callable
  const declaration: DeclarationIndex.CanonicalId = Object.freeze({
    _tag: 'CanonicalDeclarationId',
    module: callable.target.module,
    name: callable.target.name,
  })
  const target = targetFunction(context.results, declaration)
  if (target === undefined) return callable
  const inferredAtSection = callableSubstitutionOf(expression.callee, context)
  const inferred = target.declaration.typeParameters.map((parameter, ordinal) => {
    const argument =
      expression.substitution.get(Type.key(parameter.type)) ??
      inferredAtSection.get(Type.key(parameter.type)) ??
      (callable.typeArguments.length === target.declaration.typeParameters.length
        ? callable.typeArguments.at(ordinal)
        : undefined)
    return argument === undefined
      ? undefined
      : Type.substituteGenericArgument(argument, context.substitution)
  })
  if (inferred.some((argument) => argument === undefined)) return callable
  return Type.callableIdentityArgument(
    callable.identity,
    callable.target,
    inferred.filter((argument): argument is Type.GenericArgument => argument !== undefined),
    callable.environment,
  )
}

const callableApplicationArgument = (
  expression: Extract<Hir.Expression, { readonly _tag: 'CallableApply' }>,
  ordinal: number,
): Hir.Expression | undefined => {
  const section = expression.callee._tag === 'CallableSection' ? expression.callee : undefined
  if (section === undefined) return expression.arguments.at(ordinal)
  const captured = section.captures.find((capture) => capture.parameterOrdinal === ordinal)
  if (captured !== undefined) return captured.value
  return ordinal === section.omittedParameter ? expression.arguments.at(0) : undefined
}

const targetKeyOfCallableApply = (
  expression: Extract<Hir.Expression, { readonly _tag: 'CallableApply' }>,
  context: EffectOriginContext,
): InstanceKey | undefined => {
  const callable = appliedCallableOriginOf(expression, context)
  if (callable?.target._tag !== 'Declaration') return undefined
  const declaration: DeclarationIndex.CanonicalId = Object.freeze({
    _tag: 'CanonicalDeclarationId',
    module: callable.target.module,
    name: callable.target.name,
  })
  const target = targetFunction(context.results, declaration)
  if (target === undefined) return undefined
  const parameters = target.declaration.typeParameters.map((parameter) => parameter.type)
  const targetSubstitution = Type.substitution(parameters, callable.typeArguments)
  if (targetSubstitution === undefined) return undefined
  const hiddenArguments: Array<Type.EffectIdentityArgument | Type.CallableIdentityArgument> = []
  for (const ordinal of effectParameterOrdinals(target, targetSubstitution)) {
    const argument = callableApplicationArgument(expression, ordinal)
    const identity = argument === undefined ? undefined : effectOriginOf(argument, context)
    if (identity === undefined) return undefined
    hiddenArguments.push(Type.effectIdentityArgument(identity))
  }
  for (const ordinal of callableParameterOrdinals(target, targetSubstitution)) {
    const argument = callableApplicationArgument(expression, ordinal)
    const identity = argument === undefined ? undefined : callableOriginOf(argument, context)
    if (identity === undefined) return undefined
    hiddenArguments.push(identity)
  }
  return keyOf(declaration, target.contract, parameters, [
    ...callable.typeArguments,
    ...hiddenArguments,
  ])
}

interface EffectOriginContext {
  readonly fn: Hir.HirFunction
  readonly owner: InstanceKey
  readonly substitution: Type.Substitution
  readonly results: ReadonlyMap<string, Elaboration.Result>
  readonly index: DeclarationIndex.Index
  readonly resolving: ReadonlySet<string>
}

const returnedExpression = (fn: Hir.HirFunction): Hir.Expression | undefined => {
  const terminal = fn.statements.at(-1)
  return terminal?._tag === 'Return' ? terminal.expression : undefined
}

const targetKeyOfCall = (
  expression: Extract<Hir.Expression, { readonly _tag: 'Call' | 'EffectConstruct' }>,
  context: EffectOriginContext,
): InstanceKey | undefined => {
  const target = targetFunction(context.results, expression.target)
  if (target === undefined) return undefined
  const typeArguments = expression.typeArguments.map((argument) =>
    Type.substituteGenericArgument(argument, context.substitution),
  )
  const targetSubstitution = Type.substitution(
    target.declaration.typeParameters.map((parameter) => parameter.type),
    typeArguments,
  )
  if (targetSubstitution === undefined) return undefined
  const hiddenArguments: Array<Type.EffectIdentityArgument | Type.CallableIdentityArgument> = []
  for (const ordinal of effectParameterOrdinals(target, targetSubstitution)) {
    const argument = expression.arguments.at(ordinal)
    const identity = argument === undefined ? undefined : effectOriginOf(argument, context)
    if (identity === undefined) {
      // An exact requirement-forwarding wrapper is specialized into its protected recipe before
      // lowering. Compiler recipes therefore do not need a reified Effect identity merely to
      // make the concrete provider specialization discoverable.
      if (forwardedRequirementBinding(target) !== undefined) continue
      return undefined
    }
    hiddenArguments.push(Type.effectIdentityArgument(identity))
  }
  for (const ordinal of callableParameterOrdinals(target, targetSubstitution)) {
    const argument = expression.arguments.at(ordinal)
    const identity = argument === undefined ? undefined : callableOriginOf(argument, context)
    if (identity === undefined) return undefined
    hiddenArguments.push(identity)
  }
  return keyOf(
    expression.target,
    target.contract,
    target.declaration.typeParameters.map((parameter) => parameter.type),
    [...typeArguments, ...hiddenArguments],
  )
}

const resultEffectIdentity = (
  fn: Hir.HirFunction,
  owner: InstanceKey,
  results: ReadonlyMap<string, Elaboration.Result>,
  index: DeclarationIndex.Index,
  resolving: ReadonlySet<string> = new Set(),
): string | undefined => {
  const substitution = instanceSubstitution(fn, owner)
  const expression = returnedExpression(fn)
  if (substitution === undefined || expression === undefined) return undefined
  if (
    fn.contract._tag !== 'Contract' ||
    !Type.isEffect(Type.substitute(fn.contract.result, substitution))
  )
    return undefined
  const identity = keyText(owner)
  if (resolving.has(identity)) return undefined
  return effectOriginOf(expression, {
    fn,
    owner,
    substitution,
    results,
    index,
    resolving: new Set(resolving).add(identity),
  })
}

const effectOriginOf = (
  expression: Hir.Expression,
  context: EffectOriginContext,
): string | undefined => {
  if (expression._tag === 'EffectBlock') return effectIdentity(context.owner, expression.site)
  if (expression._tag === 'ParameterReference')
    return parameterEffectIdentity(context.fn, context.owner, expression.parameter.ordinal)
  if (expression._tag === 'BindingReference') {
    const initializer = callableBindings(context.fn).get(expression.binding.ordinal)
    return initializer === undefined ? undefined : effectOriginOf(initializer, context)
  }
  if (expression._tag === 'Move') return effectOriginOf(expression.subject, context)
  if (expression._tag === 'UnionConvert') return effectOriginOf(expression.source, context)
  if (expression._tag === 'Match') {
    const identities = expression.arms.flatMap((arm) => {
      if (!arm.reachable) return []
      const identity = effectOriginOf(arm.result, context)
      return identity === undefined ? [] : [identity]
    })
    return identities.length !== 0 && new Set(identities).size === 1 ? identities.at(0) : undefined
  }
  if (expression._tag === 'ServiceEffectConstruct') {
    const service = Type.substitute(expression.service, context.substitution)
    const selected = requirementBindings(context.fn).find((binding) => {
      const capability = Type.substitute(binding.provider.capability, context.substitution)
      return (
        Type.isNominal(capability) &&
        Type.equals(capability, service) &&
        binding.provider.role === expression.role &&
        (expression.access === 'Shared' || binding.provider.access === 'Exclusive')
      )
    })
    const witness =
      selected === undefined
        ? undefined
        : requirementBindingWitness(selected, context.substitution, context.index)
    const operation =
      witness?._tag === 'SourceConformanceWitness'
        ? DeclarationIndex.witnessOperation(witness, expression.operation)
        : undefined
    const target = operation === undefined ? undefined : targetFunction(context.results, operation)
    const typeArguments = expression.typeArguments.map((argument) =>
      Type.substituteGenericArgument(argument, context.substitution),
    )
    const targetKey =
      operation === undefined || target === undefined
        ? undefined
        : keyOf(
            operation,
            target.contract,
            target.declaration.typeParameters.map((parameter) => parameter.type),
            typeArguments,
          )
    return targetKey === undefined || target === undefined
      ? undefined
      : resultEffectIdentity(target, targetKey, context.results, context.index, context.resolving)
  }
  if (expression._tag === 'CallableApply') {
    const targetKey = targetKeyOfCallableApply(expression, context)
    if (targetKey === undefined) return undefined
    const target = targetFunction(context.results, targetKey.declaration)
    if (target === undefined) return undefined
    return resultEffectIdentity(
      target,
      targetKey,
      context.results,
      context.index,
      context.resolving,
    )
  }
  if (expression._tag !== 'Call' && expression._tag !== 'EffectConstruct') return undefined
  const targetKey = targetKeyOfCall(expression, context)
  const target =
    targetKey === undefined ? undefined : targetFunction(context.results, expression.target)
  if (targetKey === undefined || target === undefined) return undefined
  return resultEffectIdentity(target, targetKey, context.results, context.index, context.resolving)
}

const effectSuccesses = (
  fn: Hir.HirFunction,
  owner: InstanceKey,
  substitution: Type.Substitution,
  results: ReadonlyMap<string, Elaboration.Result>,
  index: DeclarationIndex.Index,
): NonNullable<Instance['effectSuccesses']> => {
  const context: EffectOriginContext = {
    fn,
    owner,
    substitution,
    results,
    index,
    resolving: new Set(),
  }
  return Object.freeze(
    fn.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
      .flatMap((expression) => {
        if (expression._tag !== 'EffectBlock') return []
        const success = Type.substitute(expression.type.success, substitution)
        if (!Type.isEffect(success)) return []
        const terminal = expression.statements.at(-1)
        const identity =
          terminal?._tag === 'Return' ? effectOriginOf(terminal.expression, context) : undefined
        return identity === undefined ? [] : [Object.freeze({ site: expression.site, identity })]
      }),
  )
}

const directCallInstances = (
  fn: Hir.HirFunction,
  owner: InstanceKey,
  substitution: Type.Substitution,
  results: ReadonlyMap<string, Elaboration.Result>,
  index: DeclarationIndex.Index,
): ReadonlyArray<CallInstance> => {
  const context: EffectOriginContext = {
    fn,
    owner,
    substitution,
    results,
    index,
    resolving: new Set(),
  }
  return fn.statements
    .flatMap(Hir.statementExpressions)
    .flatMap(Hir.expressionTree)
    .flatMap((expression): ReadonlyArray<CallInstance> => {
      if (expression._tag === 'CallableApply' && Type.isEffect(expression.type)) {
        const target = targetKeyOfCallableApply(expression, context)
        if (target === undefined) return []
        const targetFn = targetFunction(results, target.declaration)
        if (targetFn === undefined) return []
        const resultEffect = resultEffectIdentity(targetFn, target, results, index)
        return [
          Object.freeze({
            _tag: 'CallInstance',
            owner,
            span: expression.span,
            target,
            ...(resultEffect === undefined ? {} : { resultEffect }),
          }),
        ]
      }
      if (expression._tag !== 'Call' && expression._tag !== 'EffectConstruct') return []
      // Calls carrying neither an Effect nor a callable value remain on the original
      // finite-specialization path. Resolving them here as well would bypass its
      // polymorphic-recursion guard.
      if (!carriesHiddenIdentity(expression)) return []
      const target = targetKeyOfCall(expression, context)
      const targetFn = target === undefined ? undefined : targetFunction(results, expression.target)
      if (target === undefined || targetFn === undefined) return []
      const resultEffect = resultEffectIdentity(targetFn, target, results, index)
      return [
        Object.freeze({
          _tag: 'CallInstance',
          owner,
          span: expression.span,
          target,
          ...(resultEffect === undefined ? {} : { resultEffect }),
        }),
      ]
    })
}

const callableValue = (
  expression: Hir.Expression,
  bindings: ReadonlyMap<number, Hir.Expression>,
): Extract<Hir.Expression, { readonly _tag: 'FunctionItem' | 'CallableSection' }> | undefined => {
  if (expression._tag === 'FunctionItem' || expression._tag === 'CallableSection') return expression
  if (expression._tag === 'BindingReference') {
    const initializer = bindings.get(expression.binding.ordinal)
    return initializer === undefined ? undefined : callableValue(initializer, bindings)
  }
  if (expression._tag === 'Move') return callableValue(expression.subject, bindings)
  return undefined
}

const mergeSubstitution = (
  first: Type.Substitution,
  second: Type.Substitution,
): Type.Substitution => new Map([...first, ...second])

const callableExpressions = (fn: Hir.HirFunction): ReadonlyArray<Hir.Expression> =>
  fn.statements.flatMap((statement) =>
    Hir.statementExpressions(statement).flatMap(Hir.expressionTree),
  )

const declarationTarget = (target: Hir.CallableTarget): DeclarationIndex.CanonicalId | undefined =>
  target._tag === 'DeclarationCallableTarget' ? target.declaration : undefined

const targetFunction = (
  results: ReadonlyMap<string, Elaboration.Result>,
  target: DeclarationIndex.CanonicalId,
): Hir.HirFunction | undefined =>
  results
    .get(target.module)
    ?.hir.functions.find(
      (candidate) =>
        candidate.declaration.canonical._tag === 'Canonical' &&
        candidate.declaration.canonical.id.name === target.name,
    )

const targetArguments = (
  target: Hir.CallableTarget,
  substitution: Type.Substitution,
  results: ReadonlyMap<string, Elaboration.Result>,
): ReadonlyArray<Type.GenericArgument> | undefined => {
  const declaration = declarationTarget(target)
  if (declaration === undefined) return Object.freeze([])
  const fn = targetFunction(results, declaration)
  if (fn === undefined) return undefined
  const arguments_ = fn.declaration.typeParameters.flatMap((parameter) => {
    const type = substitution.get(Type.key(parameter.type))
    return type === undefined ? [] : [type]
  })
  return arguments_.length === fn.declaration.typeParameters.length
    ? Object.freeze(arguments_)
    : undefined
}

const callableCallTargets = (
  fn: Hir.HirFunction,
  results: ReadonlyMap<string, Elaboration.Result>,
): ReadonlyArray<CallTarget> => {
  const bindings = callableBindings(fn)
  const targets: Array<CallTarget> = []
  for (const expression of callableExpressions(fn)) {
    const value =
      expression._tag === 'CallableApply'
        ? callableValue(expression.callee, bindings)
        : expression._tag === 'FunctionItem' || expression._tag === 'CallableSection'
          ? expression
          : undefined
    if (value === undefined) continue
    const declaration = declarationTarget(value.target)
    if (declaration === undefined) continue
    const substitution =
      value._tag === 'CallableSection'
        ? mergeSubstitution(
            value.substitution,
            expression._tag === 'CallableApply' ? expression.substitution : new Map(),
          )
        : expression._tag === 'CallableApply'
          ? expression.substitution
          : new Map()
    const arguments_ = targetArguments(value.target, substitution, results)
    if (arguments_ !== undefined) {
      targets.push(Object.freeze({ declaration, typeArguments: arguments_ }))
    }
  }
  return Object.freeze(targets)
}

const forwardedRequirementTargets = (
  targets: ReadonlyArray<CallTarget>,
  results: ReadonlyMap<string, Elaboration.Result>,
  index: DeclarationIndex.Index,
): ReadonlyArray<CallTarget> =>
  targets.flatMap((target) => {
    const fn = targetFunction(results, target.declaration)
    const binding = fn === undefined ? undefined : forwardedRequirementBinding(fn)
    if (fn === undefined || binding === undefined) return []
    const substitution = Type.substitution(
      fn.declaration.typeParameters.map((parameter) => parameter.type),
      target.typeArguments.filter((argument) => !Type.isHiddenIdentityArgument(argument)),
    )
    if (substitution === undefined) return []
    const witness = requirementBindingWitness(binding, substitution, index)
    return witnessCallTargets(witness)
  })

export const callableIdentity = (self: CallableInstance): string =>
  `${keyText(self.owner)}\u0001${Hir.executableSiteKey(self.site)}\u0001${self.typeArguments.map(Type.genericArgumentKey).join('\u0000')}`

const concreteCallables = (
  fn: Hir.HirFunction,
  owner: InstanceKey,
  ownerSubstitution: Type.Substitution,
  results: ReadonlyMap<string, Elaboration.Result>,
): ReadonlyArray<CallableInstance> => {
  const expressions = callableExpressions(fn)
  const bindings = callableBindings(fn)
  const sections = expressions.flatMap((expression) =>
    expression._tag === 'CallableSection' ? [expression] : [],
  )
  const seen = new Set<string>()
  const instances: Array<CallableInstance> = []
  for (const section of sections) {
    const site = Hir.executableSiteKey(section.site)
    if (seen.has(site)) continue
    seen.add(site)
    const applications = expressions.flatMap((expression) =>
      expression._tag === 'CallableApply' && callableValue(expression.callee, bindings) === section
        ? [expression]
        : [],
    )
    const candidates: ReadonlyArray<Type.Substitution> =
      applications.length === 0
        ? [new Map()]
        : applications.map((application) => application.substitution)
    for (const applicationSubstitution of candidates) {
      const raw = mergeSubstitution(section.substitution, applicationSubstitution)
      const substitution = new Map(
        [...raw].map(([parameter, argument]) => [
          parameter,
          Type.substituteGenericArgument(argument, ownerSubstitution),
        ]),
      )
      const type = Type.substitute(Type.substitute(section.type, ownerSubstitution), substitution)
      const arguments_ = targetArguments(section.target, substitution, results)
      const captureTypes = section.captures.flatMap((capture) =>
        capture.value._tag === 'Unavailable'
          ? []
          : [Type.substitute(Type.substitute(capture.value.type, ownerSubstitution), substitution)],
      )
      if (
        !Type.isCallable(type) ||
        !Type.isConcrete(type) ||
        arguments_ === undefined ||
        arguments_.some((argument) => !Type.isConcreteGenericArgument(argument)) ||
        captureTypes.length !== section.captures.length ||
        captureTypes.some((capture) => !Type.isConcrete(capture))
      ) {
        continue
      }
      instances.push(
        Object.freeze({
          _tag: 'CallableInstance',
          owner,
          site: section.site,
          target: section.target,
          typeArguments: arguments_,
          substitution,
          captureTypes: Object.freeze(captureTypes),
          captures: Object.freeze(
            section.captures.flatMap((capture, ordinal) => {
              const type_ = captureTypes.at(ordinal)
              return type_ === undefined
                ? []
                : [
                    Object.freeze({
                      ordinal: capture.ordinal,
                      parameterOrdinal: capture.parameterOrdinal,
                      access: capture.access,
                      type: type_,
                    }),
                  ]
            }),
          ),
          type,
          mode: section.mode,
        }),
      )
    }
  }
  return Object.freeze(instances)
}

const functionByKey = (
  results: ReadonlyMap<string, Elaboration.Result>,
  key: InstanceKey,
): Hir.HirFunction | undefined =>
  results
    .get(key.declaration.module)
    ?.hir.functions.find(
      (fn) =>
        fn.declaration.canonical._tag === 'Canonical' &&
        fn.declaration.canonical.id.name === key.declaration.name,
    )

const compareInstanceKeys = (left: InstanceKey, right: InstanceKey): number => {
  const leftText = keyText(left)
  const rightText = keyText(right)
  return leftText < rightText ? -1 : leftText > rightText ? 1 : 0
}

const instanceNode = (key: InstanceKey): string => `instance\u0000${keyText(key)}`
const effectNode = (identity: string): string => `effect\u0000${identity}`

interface SuspensionGraph {
  readonly roots: ReadonlySet<string>
  readonly dependencies: ReadonlyMap<string, ReadonlySet<string>>
  readonly effectIdentities: ReadonlySet<string>
}

const suspensionGraph = (
  instances: ReadonlyArray<Instance>,
  results: ReadonlyMap<string, Elaboration.Result>,
  index: DeclarationIndex.Index,
): SuspensionGraph => {
  const roots = new Set<string>()
  const dependencies = new Map<string, Set<string>>()
  const effectIdentities = new Set<string>()
  const addDependency = (owner: string, target: string): void => {
    const targets = dependencies.get(owner) ?? new Set<string>()
    targets.add(target)
    dependencies.set(owner, targets)
  }

  for (const instance of instances) {
    const context: EffectOriginContext = {
      fn: instance.function,
      owner: instance.key,
      substitution: instance.substitution,
      results,
      index,
      resolving: new Set(),
    }
    const bindings = callableBindings(instance.function)

    const effectOrigins = (expression: Hir.Expression): ReadonlyArray<string> => {
      if (expression._tag === 'EffectBindRequirement') return effectOrigins(expression.protected)
      if (expression._tag === 'BindingReference') {
        const initializer = bindings.get(expression.binding.ordinal)
        return initializer === undefined ? [] : effectOrigins(initializer)
      }
      if (expression._tag === 'Move') return effectOrigins(expression.subject)
      if (expression._tag === 'UnionConvert') return effectOrigins(expression.source)
      if (expression._tag === 'Match') {
        return Object.freeze([
          ...new Set(
            expression.arms.flatMap((arm) => (arm.reachable ? effectOrigins(arm.result) : [])),
          ),
        ])
      }
      const identity = effectOriginOf(expression, context)
      return identity === undefined ? [] : Object.freeze([identity])
    }

    const executionTargets = (expression: Hir.Expression): ReadonlyArray<string> => {
      if (expression._tag === 'EffectBindRequirement') return executionTargets(expression.protected)
      if (expression._tag === 'EffectResult') return executionTargets(expression.protected)
      if (expression._tag === 'BindingReference') {
        const initializer = bindings.get(expression.binding.ordinal)
        return initializer === undefined ? [] : executionTargets(initializer)
      }
      if (expression._tag === 'Move') return executionTargets(expression.subject)
      if (expression._tag === 'UnionConvert') return executionTargets(expression.source)
      if (expression._tag === 'Match') {
        return Object.freeze([
          ...new Set(
            expression.arms.flatMap((arm) => (arm.reachable ? executionTargets(arm.result) : [])),
          ),
        ])
      }
      if (expression._tag === 'EffectConstruct') {
        const target = targetKeyOfCall(expression, context)
        const targetFn =
          target === undefined ? undefined : targetFunction(results, target.declaration)
        const identity =
          target === undefined || targetFn === undefined
            ? undefined
            : resultEffectIdentity(targetFn, target, results, index)
        return identity !== undefined
          ? Object.freeze([effectNode(identity)])
          : target === undefined
            ? []
            : Object.freeze([instanceNode(target)])
      }
      if (expression._tag === 'ServiceEffectConstruct') {
        const service = Type.substitute(expression.service, instance.substitution)
        const selected = requirementBindings(instance.function).find((binding) => {
          const capability = Type.substitute(binding.provider.capability, instance.substitution)
          return (
            Type.isNominal(capability) &&
            Type.equals(capability, service) &&
            binding.provider.role === expression.role &&
            (expression.access === 'Shared' || binding.provider.access === 'Exclusive')
          )
        })
        const witness =
          selected === undefined
            ? undefined
            : requirementBindingWitness(selected, instance.substitution, index)
        const operation =
          witness?._tag !== 'SourceConformanceWitness'
            ? undefined
            : DeclarationIndex.witnessOperation(witness, expression.operation)
        const target = operation === undefined ? undefined : targetFunction(results, operation)
        const typeArguments = expression.typeArguments.map((argument) =>
          Type.substituteGenericArgument(argument, instance.substitution),
        )
        const targetKey =
          operation === undefined || target === undefined
            ? undefined
            : keyOf(
                operation,
                target.contract,
                target.declaration.typeParameters.map((parameter) => parameter.type),
                typeArguments,
              )
        const identity =
          targetKey === undefined || target === undefined
            ? undefined
            : resultEffectIdentity(target, targetKey, results, index)
        return identity !== undefined
          ? Object.freeze([effectNode(identity)])
          : targetKey === undefined
            ? []
            : Object.freeze([instanceNode(targetKey)])
      }
      return Object.freeze(effectOrigins(expression).map(effectNode))
    }

    const isSuspensionSubject = (expression: Hir.Expression): boolean => {
      if (expression._tag === 'BuiltinCall') return expression.operation === 'EffectSuspend'
      if (expression._tag === 'BindingReference') {
        const initializer = bindings.get(expression.binding.ordinal)
        return initializer !== undefined && isSuspensionSubject(initializer)
      }
      if (expression._tag === 'Move') return isSuspensionSubject(expression.subject)
      if (expression._tag === 'UnionConvert') return isSuspensionSubject(expression.source)
      if (expression._tag === 'EffectBindRequirement')
        return isSuspensionSubject(expression.protected)
      return false
    }

    const scanExpression = (expression: Hir.Expression, execution: string): void => {
      if (expression._tag === 'EffectBlock') {
        const identity = effectIdentity(instance.key, expression.site)
        effectIdentities.add(identity)
        scanStatements(expression.statements, effectNode(identity))
        return
      }
      if (expression._tag === 'Call') {
        const target = targetKeyOfCall(expression, context)
        if (target !== undefined) addDependency(execution, instanceNode(target))
      } else if (expression._tag === 'EffectConstruct') {
        const target = targetKeyOfCall(expression, context)
        if (target !== undefined) addDependency(execution, instanceNode(target))
      } else if (expression._tag === 'ServiceEffectConstruct') {
        for (const target of executionTargets(expression)) addDependency(execution, target)
      } else if (expression._tag === 'CallableApply') {
        const target = targetKeyOfCallableApply(expression, context)
        if (target !== undefined) addDependency(execution, instanceNode(target))
      } else if (expression._tag === 'Run') {
        if (isSuspensionSubject(expression.subject)) {
          roots.add(execution)
        } else {
          for (const target of executionTargets(expression.subject))
            addDependency(execution, target)
        }
      }
      for (const child of Hir.expressionChildren(expression)) scanExpression(child, execution)
    }
    const scanStatements = (statements: ReadonlyArray<Hir.Statement>, execution: string): void => {
      for (const statement of statements) {
        for (const expression of Hir.statementExpressions(statement))
          scanExpression(expression, execution)
      }
    }
    scanStatements(instance.function.statements, instanceNode(instance.key))
  }

  return Object.freeze({ roots, dependencies, effectIdentities })
}

const suspendableNodes = (graph: SuspensionGraph): ReadonlySet<string> => {
  const suspendable = new Set(graph.roots)
  let changed = true
  while (changed) {
    changed = false
    for (const [owner, targets] of graph.dependencies) {
      if (suspendable.has(owner) || ![...targets].some((target) => suspendable.has(target)))
        continue
      suspendable.add(owner)
      changed = true
    }
  }
  return suspendable
}

/** Reports whether one exact concrete instance can reach the suspension intrinsic. */
export const isSuspendable = (self: Discovery, key: InstanceKey): boolean =>
  self.suspendable.some((candidate) => keyText(candidate) === keyText(key))

/** Reports whether executing one exact concrete function can suspend, excluding its lazy result. */
export const isExecutionSuspendable = (self: Discovery, key: InstanceKey): boolean =>
  self.suspendableExecutions.some((candidate) => keyText(candidate) === keyText(key))

/** Reports whether one exact hidden Effect runner can reach the suspension intrinsic. */
export const isEffectSuspendable = (self: Discovery, identity: string): boolean =>
  self.suspendableEffects.includes(identity)

const sameInstanceKey = (left: InstanceKey, right: InstanceKey): boolean =>
  keyText(left) === keyText(right)

const sameDeclaration = (
  left: DeclarationIndex.CanonicalId,
  right: DeclarationIndex.CanonicalId,
): boolean => left.module === right.module && left.name === right.name

/**
 * Rejects a concrete allocator selected for a suspendable Effect when its allocation operation can
 * itself suspend. Such a provider would need continuation storage in order to allocate that same
 * continuation storage, so the execution boundary could never bootstrap.
 */
export const continuationAllocatorViolations = (
  self: Discovery,
  index: DeclarationIndex.Index,
  results: ReadonlyMap<string, Elaboration.Result>,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  const diagnostics = new Map<string, Diagnostic.Diagnostic>()
  for (const instance of self.instances) {
    const context: EffectOriginContext = {
      fn: instance.function,
      owner: instance.key,
      substitution: instance.substitution,
      results,
      index,
      resolving: new Set(),
    }
    for (const binding of requirementBindings(instance.function)) {
      const capability = Type.substitute(binding.provider.capability, instance.substitution)
      const provider = Type.substitute(binding.provider.providerType, instance.substitution)
      if (
        !Type.isNominal(capability) ||
        !Type.equals(capability, Type.allocator) ||
        !Type.isNominal(provider)
      )
        continue
      const protectedIdentity = effectOriginOf(binding.protected, context)
      if (protectedIdentity === undefined || !isEffectSuspendable(self, protectedIdentity)) continue
      const witness = requirementBindingWitness(binding, instance.substitution, index)
      if (witness?._tag !== 'SourceConformanceWitness') continue
      const implementation = DeclarationIndex.witnessOperation(witness, 'allocate')
      if (implementation === undefined) continue
      const implementationInstances = self.instances.filter((candidate) =>
        sameDeclaration(candidate.key.declaration, implementation),
      )
      const implementationSuspends = implementationInstances.some(
        (candidate) =>
          isSuspendable(self, candidate.key) ||
          (candidate.resultEffect !== undefined &&
            isEffectSuspendable(self, candidate.resultEffect)),
      )
      if (!implementationSuspends) continue

      const provisionSpans = self.calls
        .filter((call) => sameInstanceKey(call.target, instance.key))
        .map((call) => call.span)
      const spans = provisionSpans.length === 0 ? [binding.provider.span] : provisionSpans
      const providerText = Type.encode(provider)
      const implementationText = `${implementation.module}.${implementation.name}`
      for (const span of spans) {
        const key = `${span.sourceId}\u0000${span.start}\u0000${span.end}`
        if (diagnostics.has(key)) continue
        diagnostics.set(
          key,
          Diagnostic.suspendingContinuationAllocator(providerText, implementationText, span),
        )
      }
    }
  }
  return Object.freeze(
    [...diagnostics.values()].sort(
      (left, right) =>
        (left.span.sourceId < right.span.sourceId
          ? -1
          : left.span.sourceId > right.span.sourceId
            ? 1
            : 0) ||
        left.span.start - right.span.start ||
        left.span.end - right.span.end,
    ),
  )
}

/**
 * Discovers the reachable instances from the root module's entry. The worklist records an
 * instance before following its calls, so directly and mutually recursive programs terminate.
 */
export const discover = (
  rootModule: string,
  results: ReadonlyMap<string, Elaboration.Result>,
  ownership: ReadonlyMap<string, Ownership.ModuleOwnership>,
  index: DeclarationIndex.Index,
): Discovery => {
  const root = results.get(rootModule)
  if (root === undefined) {
    throw new RangeError(`Instance discovery lost its root module ${rootModule}`)
  }
  const entry = resolveEntry(root, index)
  if (entry._tag !== 'Resolved') {
    return Object.freeze({
      _tag: 'InstanceDiscovery',
      rootModule,
      entry,
      instances: Object.freeze([]),
      callables: Object.freeze([]),
      calls: Object.freeze([]),
      intrinsics: Object.freeze([]),
      suspendableExecutions: Object.freeze([]),
      suspendable: Object.freeze([]),
      suspendableEffects: Object.freeze([]),
      violations: Object.freeze([]),
    })
  }

  const recorded = new Map<string, Instance>()
  const recordedCallables = new Map<string, CallableInstance>()
  const recordedCalls = new Map<string, CallInstance>()
  const scannedContexts = new Set<string>()
  interface WorkItem {
    readonly key: InstanceKey
    readonly ancestors: ReadonlyMap<string, InstanceKey>
    readonly cleanupReachable: boolean
  }
  const declarationText = (key: InstanceKey): string =>
    `${key.declaration.module}\u0000${key.declaration.name}`
  const sameArguments = (left: InstanceKey, right: InstanceKey): boolean =>
    left.typeArguments.length === right.typeArguments.length &&
    left.typeArguments.every((argument, index) => {
      const candidate = right.typeArguments.at(index)
      return (
        candidate !== undefined &&
        Type.genericArgumentKey(argument) === Type.genericArgumentKey(candidate)
      )
    })
  const pending: Array<WorkItem> = [
    Object.freeze({
      key: entry.key,
      ancestors: new Map([[declarationText(entry.key), entry.key]]),
      cleanupReachable: false,
    }),
  ]
  const violations: Array<PolymorphicRecursion> = []
  const contextText = (item: WorkItem): string =>
    `${item.cleanupReachable ? 'cleanup' : 'ordinary'}\u0001${keyText(item.key)}\u0001${[
      ...item.ancestors.entries(),
    ]
      .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
      .map(([declaration, key]) => `${declaration}\u0002${keyText(key)}`)
      .join('\u0003')}`
  while (pending.length > 0) {
    const item = pending.shift()
    if (item === undefined) continue
    const context = contextText(item)
    if (scannedContexts.has(context)) continue
    scannedContexts.add(context)
    const key = item.key
    const fn = functionByKey(results, key)
    if (fn === undefined) continue
    const parameters = fn.declaration.typeParameters.map((parameter) => parameter.type)
    const substitution = Type.substitution(
      parameters,
      key.typeArguments.filter((argument) => !Type.isHiddenIdentityArgument(argument)),
    )
    if (substitution === undefined) continue
    if (!recorded.has(keyText(key))) {
      const resultEffect = resultEffectIdentity(fn, key, results, index)
      recorded.set(
        keyText(key),
        Object.freeze({
          _tag: 'Instance',
          key,
          function: fn,
          substitution,
          effectSuccesses: effectSuccesses(fn, key, substitution, results, index),
          ...(resultEffect === undefined ? {} : { resultEffect }),
        }),
      )
    }
    for (const callable of concreteCallables(fn, key, substitution, results)) {
      recordedCallables.set(callableIdentity(callable), callable)
    }
    const functionOwnership = ownership
      ?.get(key.declaration.module)
      ?.functions.find(
        (candidate) => candidate.declaration.id.ordinal === fn.declaration.id.ordinal,
      )
    // Deferred effect-body bindings publish only through exit releases, so the joined binding
    // facts and the exit releases both feed hook reachability.
    const cleanupHooks = [
      ...Ownership.allBindings(functionOwnership).map((binding) => binding.cleanup),
      ...(functionOwnership?.exits.flatMap((exit) =>
        exit.releases.map((release) => release.cleanup),
      ) ?? []),
    ]
      .map((cleanup) =>
        Ownership.specializeCleanup(cleanup, substitution, (type) =>
          Ownership.cleanupPlan(index, type),
        ),
      )
      .flatMap(hookCalls)
    const calls = new Map<string, CallTarget>()
    const directCalls = directCallInstances(fn, key, substitution, results, index)
    const callableTargets = callableCallTargets(fn, results)
    for (const call of directCalls) {
      recordedCalls.set(
        `${keyText(call.owner)}\u0005${call.span.sourceId}:${call.span.start}:${call.span.end}`,
        call,
      )
    }
    const cleanupTargets = [
      ...slotDropHookTargets(fn, index, substitution),
      ...cleanupHooks,
      ...(entry.kind === 'Effect' && keyText(key) === keyText(entry.key)
        ? entry.failures.flatMap((failure) => hookCalls(Ownership.cleanupPlan(index, failure.type)))
        : []),
    ]
    const identityOfCall = (call: CallTarget): string =>
      `${call.declaration.module}\u0000${call.declaration.name}\u0000${call.typeArguments.map(Type.genericArgumentKey).join('\u0000')}`
    const cleanupIdentities = new Set(cleanupTargets.map(identityOfCall))
    const reachableCalls: ReadonlyArray<CallTarget> = [
      ...bodyCallTargets(fn),
      ...interfaceWitnessTargets(fn, index, substitution),
      ...requirementBindingCallTargets(fn, substitution, index),
      ...directCalls.map((call) => ({
        declaration: call.target.declaration,
        typeArguments: call.target.typeArguments,
      })),
      ...forwardedRequirementCallTargets(directCalls, results, index),
      ...callableTargets,
      ...forwardedRequirementTargets(callableTargets, results, index),
      ...cleanupTargets,
    ]
    for (const call of reachableCalls) {
      const identity = identityOfCall(call)
      const recorded = calls.get(identity)
      // An ordinary edge must keep the recursion guard even when the same target is also reached
      // through a proved conditional requirement. Only an exclusively structural edge earns the
      // narrow exception below.
      if (recorded === undefined || recorded.structurallyDescending === true)
        calls.set(
          identity,
          recorded?.structurallyDescending === true && call.structurallyDescending === true
            ? recorded
            : call,
        )
    }
    for (const call of calls.values()) {
      const target = call.declaration
      const targetFunction = results
        .get(target.module)
        ?.hir.functions.find(
          (candidate) =>
            candidate.declaration.canonical._tag === 'Canonical' &&
            candidate.declaration.canonical.id.name === target.name,
        )
      if (targetFunction === undefined) continue
      const targetArguments = call.typeArguments.map((argument) =>
        Type.substituteGenericArgument(argument, substitution),
      )
      const targetKey = keyOf(
        target,
        targetFunction.contract,
        targetFunction.declaration.typeParameters.map((parameter) => parameter.type),
        targetArguments,
      )
      const ancestor = item.ancestors.get(declarationText(targetKey))
      if (
        ancestor !== undefined &&
        !sameArguments(ancestor, targetKey) &&
        call.structurallyDescending !== true &&
        !(
          recorded.has(keyText(targetKey)) &&
          (item.cleanupReachable || cleanupIdentities.has(identityOfCall(call)))
        )
      ) {
        violations.push(
          Object.freeze({ _tag: 'PolymorphicRecursion', caller: key, target: targetKey }),
        )
        continue
      }
      pending.push(
        Object.freeze({
          key: targetKey,
          ancestors: new Map(item.ancestors).set(declarationText(targetKey), targetKey),
          cleanupReachable: item.cleanupReachable || cleanupIdentities.has(identityOfCall(call)),
        }),
      )
    }
  }

  const instances = Object.freeze([...recorded.values()])
  const graph = suspensionGraph(instances, results, index)
  const suspendable = suspendableNodes(graph)
  return Object.freeze({
    _tag: 'InstanceDiscovery',
    rootModule,
    entry,
    instances,
    callables: Object.freeze([...recordedCallables.values()]),
    calls: Object.freeze([...recordedCalls.values()]),
    intrinsics: reachableIntrinsics(instances, index),
    suspendableExecutions: Object.freeze(
      instances
        .filter((instance) => suspendable.has(instanceNode(instance.key)))
        .map((instance) => instance.key)
        .sort(compareInstanceKeys),
    ),
    suspendable: Object.freeze(
      instances
        .flatMap((instance) => {
          const runnerSuspendable =
            instance.resultEffect !== undefined &&
            suspendable.has(effectNode(instance.resultEffect))
          return suspendable.has(instanceNode(instance.key)) || runnerSuspendable
            ? [instance.key]
            : []
        })
        .sort(compareInstanceKeys),
    ),
    suspendableEffects: Object.freeze(
      [...graph.effectIdentities]
        .filter((identity) => suspendable.has(effectNode(identity)))
        .sort(),
    ),
    violations: Object.freeze(violations),
  })
}
