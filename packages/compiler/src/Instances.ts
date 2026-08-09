import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as Hir from './Hir.js'
import * as Ownership from './Ownership.js'
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
  readonly typeArguments: ReadonlyArray<Type.Type>
  readonly contractRow: ReadonlyArray<string>
}

/** One discovered instance with its elaborated HIR function. */
export interface Instance {
  readonly _tag: 'Instance'
  readonly key: InstanceKey
  readonly function: Hir.HirFunction
  readonly substitution: ReadonlyMap<string, Type.Type>
}

/** One concrete hidden callable-section construction reachable from an instance. */
export interface CallableInstance {
  readonly _tag: 'CallableInstance'
  readonly owner: InstanceKey
  readonly site: Hir.CallableSiteId
  readonly target: Hir.CallableTarget
  readonly typeArguments: ReadonlyArray<Type.Type>
  readonly substitution: ReadonlyMap<string, Type.Type>
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

/** Produces semantic diagnostics for every finite-discovery violation. */
export const violationDiagnostics = (self: Discovery): ReadonlyArray<Diagnostic.Diagnostic> =>
  Object.freeze(
    self.violations.flatMap((violation) => {
      const caller = self.instances.find(
        (instance) => keyText(instance.key) === keyText(violation.caller),
      )
      if (caller === undefined) return []
      const callerText = `${violation.caller.declaration.name}<${violation.caller.typeArguments
        .map(Type.encode)
        .join(', ')}>`
      const targetText = `${violation.target.declaration.name}<${violation.target.typeArguments
        .map(Type.encode)
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
    violations: Object.freeze([]),
  })

const keyOf = (
  declaration: DeclarationIndex.CanonicalId,
  contract: Hir.ContractFact,
  typeParameters: ReadonlyArray<Type.Parameter> = [],
  typeArguments: ReadonlyArray<Type.Type> = [],
): InstanceKey =>
  (() => {
    const substitution = Type.substitution(typeParameters, typeArguments)
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

const keyText = (key: InstanceKey): string =>
  `${key.declaration.module}\u0000${key.declaration.name}\u0000${key.typeArguments
    .map(Type.key)
    .join('\u0000')}`

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
  if (
    declaration.requirementRow.requirements.some(
      (requirement) =>
        requirement.access !== 'Shared' ||
        requirement.role !== 'DefaultRole' ||
        !Type.equals(requirement.capability, Type.standardStreams),
    )
  ) {
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
  readonly typeArguments: ReadonlyArray<Type.Type>
}

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

const callTargets = (expression: Hir.Expression): ReadonlyArray<CallTarget> => {
  if (expression._tag === 'Run') return callTargets(expression.subject)
  if (expression._tag === 'EffectCatch') {
    const handler =
      expression.handler._tag === 'FunctionItem' || expression.handler._tag === 'CallableSection'
        ? expression.handler
        : undefined
    const target = handler?.target
    return [
      ...callTargets(expression.protected),
      ...(target?._tag === 'DeclarationCallableTarget'
        ? [
            Object.freeze({
              declaration: target.declaration,
              typeArguments:
                handler?._tag === 'CallableSection' ? handler.typeArguments : Object.freeze([]),
            }),
          ]
        : []),
      ...callTargets(expression.handler),
    ]
  }
  if (expression._tag === 'EffectRetry')
    return [...callTargets(expression.protected), ...callTargets(expression.retries)]
  if (expression._tag === 'EffectTransform')
    return [...callTargets(expression.protected), ...callTargets(expression.callback)]
  if (expression._tag === 'EffectProvide') {
    // A source-declared witness makes provision dispatch to its qualified operation, so the
    // operation is reachable even though no ordinary call names it.
    const witness = expression.provider.witness
    return [
      ...callTargets(expression.protected),
      ...(witness._tag === 'SourceConformanceWitness' && witness.operation !== undefined
        ? [Object.freeze({ declaration: witness.operation, typeArguments: Object.freeze([]) })]
        : []),
    ]
  }
  if (expression._tag === 'EffectProvideWith')
    return [...callTargets(expression.protected), ...callTargets(expression.acquisition)]
  if (expression._tag === 'Move') return callTargets(expression.subject)
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
  if (expression._tag === 'BuiltinCall') {
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
  if (expression._tag !== 'Call' && expression._tag !== 'EffectConstruct') return []
  return [
    Object.freeze({ declaration: expression.target, typeArguments: expression.typeArguments }),
    ...expression.arguments.flatMap((argument) => callTargets(argument)),
  ]
}

const bodyCallTargets = (fn: Hir.HirFunction): ReadonlyArray<CallTarget> =>
  fn.statements.flatMap((statement) => Hir.statementExpressions(statement).flatMap(callTargets))

const slotDropHookTargets = (
  fn: Hir.HirFunction,
  index: DeclarationIndex.Index,
  substitution: ReadonlyMap<string, Type.Type>,
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

const callableBindings = (fn: Hir.HirFunction): ReadonlyMap<number, Hir.Expression> => {
  const bindings = new Map<number, Hir.Expression>()
  const statements = (body: ReadonlyArray<Hir.Statement>): void => {
    for (const statement of body) {
      if (statement._tag === 'Bind') bindings.set(statement.binding.ordinal, statement.initializer)
      if (statement._tag === 'Unsafe') statements(statement.statements)
      else if (statement._tag === 'If') {
        statements(statement.taken)
        statements(statement.otherwise)
      } else if (statement._tag === 'While') statements(statement.body)
    }
  }
  statements(fn.statements)
  return bindings
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
  first: ReadonlyMap<string, Type.Type>,
  second: ReadonlyMap<string, Type.Type>,
): ReadonlyMap<string, Type.Type> => new Map([...first, ...second])

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
  substitution: ReadonlyMap<string, Type.Type>,
  results: ReadonlyMap<string, Elaboration.Result>,
): ReadonlyArray<Type.Type> | undefined => {
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

const callableKeyText = (self: CallableInstance): string =>
  `${keyText(self.owner)}\u0001${self.site.function.sourceId}:${self.site.function.ordinal}:${self.site.span.start}:${self.site.span.end}\u0001${self.typeArguments.map(Type.key).join('\u0000')}`

const concreteCallables = (
  fn: Hir.HirFunction,
  owner: InstanceKey,
  ownerSubstitution: ReadonlyMap<string, Type.Type>,
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
    const site = `${section.site.function.sourceId}:${section.site.function.ordinal}:${section.site.span.start}:${section.site.span.end}`
    if (seen.has(site)) continue
    seen.add(site)
    const applications = expressions.flatMap((expression) =>
      expression._tag === 'CallableApply' && callableValue(expression.callee, bindings) === section
        ? [expression]
        : [],
    )
    const candidates: ReadonlyArray<ReadonlyMap<string, Type.Type>> =
      applications.length === 0
        ? [new Map()]
        : applications.map((application) => application.substitution)
    for (const applicationSubstitution of candidates) {
      const raw = mergeSubstitution(section.substitution, applicationSubstitution)
      const substitution = new Map(
        [...raw].map(([parameter, type]) => [parameter, Type.substitute(type, ownerSubstitution)]),
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
        arguments_.some((argument) => !Type.isConcrete(argument)) ||
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
      violations: Object.freeze([]),
    })
  }

  const recorded = new Map<string, Instance>()
  const recordedCallables = new Map<string, CallableInstance>()
  const scannedContexts = new Set<string>()
  interface WorkItem {
    readonly key: InstanceKey
    readonly ancestors: ReadonlyMap<string, InstanceKey>
  }
  const declarationText = (key: InstanceKey): string =>
    `${key.declaration.module}\u0000${key.declaration.name}`
  const sameArguments = (left: InstanceKey, right: InstanceKey): boolean =>
    left.typeArguments.length === right.typeArguments.length &&
    left.typeArguments.every((argument, index) => {
      const candidate = right.typeArguments.at(index)
      return candidate !== undefined && Type.equals(argument, candidate)
    })
  const pending: Array<WorkItem> = [
    Object.freeze({
      key: entry.key,
      ancestors: new Map([[declarationText(entry.key), entry.key]]),
    }),
  ]
  const violations: Array<PolymorphicRecursion> = []
  const contextText = (item: WorkItem): string =>
    `${keyText(item.key)}\u0001${[...item.ancestors.entries()]
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
    const substitution = Type.substitution(parameters, key.typeArguments)
    if (substitution === undefined) continue
    if (!recorded.has(keyText(key))) {
      recorded.set(
        keyText(key),
        Object.freeze({ _tag: 'Instance', key, function: fn, substitution }),
      )
    }
    for (const callable of concreteCallables(fn, key, substitution, results)) {
      recordedCallables.set(callableKeyText(callable), callable)
    }
    const functionOwnership = ownership
      ?.get(key.declaration.module)
      ?.functions.find(
        (candidate) => candidate.declaration.id.ordinal === fn.declaration.id.ordinal,
      )
    // Deferred effect-body bindings publish only through exit releases, so both fact sources
    // feed hook reachability.
    const cleanupHooks = [
      ...(functionOwnership?.bindings.map((binding) => binding.cleanup) ?? []),
      ...(functionOwnership?.deferredBindings.map((binding) => binding.cleanup) ?? []),
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
    for (const call of [
      ...bodyCallTargets(fn),
      ...slotDropHookTargets(fn, index, substitution),
      ...callableCallTargets(fn, results),
      ...cleanupHooks,
      ...(entry.kind === 'Effect' && keyText(key) === keyText(entry.key)
        ? entry.failures.flatMap((failure) => hookCalls(Ownership.cleanupPlan(index, failure.type)))
        : []),
    ]) {
      const identity = `${call.declaration.module}\u0000${call.declaration.name}\u0000${call.typeArguments.map(Type.key).join('\u0000')}`
      if (!calls.has(identity)) calls.set(identity, call)
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
        Type.substitute(argument, substitution),
      )
      const targetKey = keyOf(
        target,
        targetFunction.contract,
        targetFunction.declaration.typeParameters.map((parameter) => parameter.type),
        targetArguments,
      )
      const ancestor = item.ancestors.get(declarationText(targetKey))
      if (ancestor !== undefined && !sameArguments(ancestor, targetKey)) {
        violations.push(
          Object.freeze({ _tag: 'PolymorphicRecursion', caller: key, target: targetKey }),
        )
        continue
      }
      pending.push(
        Object.freeze({
          key: targetKey,
          ancestors: new Map(item.ancestors).set(declarationText(targetKey), targetKey),
        }),
      )
    }
  }

  return Object.freeze({
    _tag: 'InstanceDiscovery',
    rootModule,
    entry,
    instances: Object.freeze([...recorded.values()]),
    callables: Object.freeze([...recordedCallables.values()]),
    violations: Object.freeze(violations),
  })
}
