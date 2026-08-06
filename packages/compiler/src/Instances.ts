import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Elaboration from './Elaboration.js'
import * as Hir from './Hir.js'
import * as Type from './Type.js'

/**
 * Instance discovery: which concrete runtime instances are reachable from the user entry. Keys
 * are canonical declaration identities plus normalized type and contract-row arguments — both
 * empty in the frozen slice. The worklist records an instance before following it, so ordinary
 * recursion terminates.
 */

/** One normalized instance key. Argument lists stay empty until generics exist. */
export interface InstanceKey {
  readonly _tag: 'InstanceKey'
  readonly declaration: DeclarationIndex.CanonicalId
  readonly typeArguments: ReadonlyArray<string>
  readonly contractRow: ReadonlyArray<string>
}

/** One discovered instance with its elaborated HIR function. */
export interface Instance {
  readonly _tag: 'Instance'
  readonly key: InstanceKey
  readonly function: Hir.HirFunction
}

/** The resolved or explicitly unavailable user entry. */
export type Entry =
  | { readonly _tag: 'Resolved'; readonly key: InstanceKey }
  | {
      readonly _tag: 'Unavailable'
      readonly reason: 'MissingEntry' | 'AmbiguousEntry' | 'ParameterizedEntry' | 'UntypedEntry'
    }

/** The deterministic discovery result. */
export interface Discovery {
  readonly _tag: 'InstanceDiscovery'
  readonly rootModule: string
  readonly entry: Entry
  readonly instances: ReadonlyArray<Instance>
}

const keyOf = (
  declaration: DeclarationIndex.CanonicalId,
  contract: Hir.ContractFact,
): InstanceKey =>
  Object.freeze({
    _tag: 'InstanceKey',
    declaration,
    typeArguments: Object.freeze([]),
    contractRow:
      contract._tag === 'Contract'
        ? Object.freeze([
            ...contract.parameters.map(Type.key),
            `result:${Type.key(contract.result)}`,
          ])
        : Object.freeze([]),
  })

const keyText = (key: InstanceKey): string =>
  `${key.declaration.module}\u0000${key.declaration.name}`

const resolveEntry = (root: Elaboration.Result): Entry => {
  const lookup = Elaboration.declarationByName(root, 'main')
  if (lookup._tag === 'Missing')
    return Object.freeze({ _tag: 'Unavailable', reason: 'MissingEntry' })
  if (lookup._tag === 'Ambiguous') {
    return Object.freeze({ _tag: 'Unavailable', reason: 'AmbiguousEntry' })
  }
  const declaration = lookup.declaration
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

const callTargets = (expression: Hir.Expression): ReadonlyArray<DeclarationIndex.CanonicalId> => {
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
  return [expression.target, ...expression.arguments.flatMap((argument) => callTargets(argument))]
}

const bodyCallTargets = (fn: Hir.HirFunction): ReadonlyArray<DeclarationIndex.CanonicalId> =>
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
    })
  }

  const recorded = new Map<string, Instance>()
  const pending: Array<InstanceKey> = [entry.key]
  while (pending.length > 0) {
    const key = pending.shift()
    if (key === undefined || recorded.has(keyText(key))) continue
    const fn = functionByKey(results, key)
    if (fn === undefined) continue
    recorded.set(keyText(key), Object.freeze({ _tag: 'Instance', key, function: fn }))
    for (const target of bodyCallTargets(fn)) {
      const targetFunction = results
        .get(target.module)
        ?.hir.functions.find(
          (candidate) =>
            candidate.declaration.canonical._tag === 'Canonical' &&
            candidate.declaration.canonical.id.name === target.name,
        )
      if (targetFunction === undefined) continue
      const targetKey = keyOf(target, targetFunction.contract)
      if (!recorded.has(keyText(targetKey))) pending.push(targetKey)
    }
  }

  return Object.freeze({
    _tag: 'InstanceDiscovery',
    rootModule,
    entry,
    instances: Object.freeze([...recorded.values()]),
  })
}
