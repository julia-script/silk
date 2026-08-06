import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as Hir from './Hir.js'
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

/** The resolved or explicitly unavailable user entry. */
export type Entry =
  | { readonly _tag: 'Resolved'; readonly key: InstanceKey }
  | {
      readonly _tag: 'Unavailable'
      readonly reason:
        | 'MissingEntry'
        | 'AmbiguousEntry'
        | 'GenericEntry'
        | 'ParameterizedEntry'
        | 'UntypedEntry'
        | 'InvalidSource'
    }

/** The deterministic discovery result. */
export interface Discovery {
  readonly _tag: 'InstanceDiscovery'
  readonly rootModule: string
  readonly entry: Entry
  readonly instances: ReadonlyArray<Instance>
  readonly violations: ReadonlyArray<PolymorphicRecursion>
}

/** A recursive generic edge that changes an ancestor declaration's concrete arguments. */
export interface PolymorphicRecursion {
  readonly _tag: 'PolymorphicRecursion'
  readonly caller: InstanceKey
  readonly target: InstanceKey
}

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
            ])
          : Object.freeze([]),
    })
  })()

const keyText = (key: InstanceKey): string =>
  `${key.declaration.module}\u0000${key.declaration.name}\u0000${key.typeArguments
    .map(Type.key)
    .join('\u0000')}`

const resolveEntry = (root: Elaboration.Result): Entry => {
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
    declaration.returnType._tag !== 'Resolved' ||
    declaration.returnType.type !== 'I32' ||
    declaration.canonical._tag !== 'Canonical'
  ) {
    return Object.freeze({ _tag: 'Unavailable', reason: 'UntypedEntry' })
  }
  return Object.freeze({
    _tag: 'Resolved',
    key: keyOf(declaration.canonical.id, Hir.contractOf(declaration)),
  })
}

interface CallTarget {
  readonly declaration: DeclarationIndex.CanonicalId
  readonly typeArguments: ReadonlyArray<Type.Type>
}

const callTargets = (expression: Hir.Expression): ReadonlyArray<CallTarget> => {
  if (expression._tag === 'Move') return callTargets(expression.subject)
  if (expression._tag === 'UnionConvert') return callTargets(expression.source)
  if (expression._tag === 'Project') return callTargets(expression.subject)
  if (expression._tag === 'IndexPlace') {
    return [...callTargets(expression.subject), ...callTargets(expression.index)]
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
  if (expression._tag !== 'Call') return []
  return [
    Object.freeze({ declaration: expression.target, typeArguments: expression.typeArguments }),
    ...expression.arguments.flatMap((argument) => callTargets(argument)),
  ]
}

const bodyCallTargets = (fn: Hir.HirFunction): ReadonlyArray<CallTarget> =>
  fn.statements.flatMap((statement) => Hir.statementExpressions(statement).flatMap(callTargets))

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
): Discovery => {
  const root = results.get(rootModule)
  if (root === undefined) {
    throw new RangeError(`Instance discovery lost its root module ${rootModule}`)
  }
  const entry = resolveEntry(root)
  if (entry._tag !== 'Resolved') {
    return Object.freeze({
      _tag: 'InstanceDiscovery',
      rootModule,
      entry,
      instances: Object.freeze([]),
      violations: Object.freeze([]),
    })
  }

  const recorded = new Map<string, Instance>()
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
    for (const call of bodyCallTargets(fn)) {
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
    violations: Object.freeze(violations),
  })
}
