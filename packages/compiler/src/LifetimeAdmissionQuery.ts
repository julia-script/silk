import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as LifetimeAdmission from './LifetimeAdmission.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

interface Application {
  readonly target: string
  readonly substitution: Type.Substitution
  readonly span: SourceSpan.SourceSpan
}

const key = (module: string, name: string): string => JSON.stringify([module, name])

/** Replays checked generic storage obligations without realizing or rechecking a function body. */
export const check = (
  index: DeclarationIndex.Index,
  modules: ReadonlyMap<string, Elaboration.Result>,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  const context = LifetimeAdmission.withAggregates(
    LifetimeAdmission.context(index),
    [...modules.values()].flatMap((module) => module.generatedAggregates),
  )
  const functions = new Map<string, Elaboration.FunctionFact>()
  const calls = new Map<string, ReadonlyArray<Application>>()
  for (const module of modules.values()) {
    for (const fn of [...module.functions, ...module.hiddenFunctions]) {
      const identity = fn.declaration.canonical
      if (identity._tag !== 'Canonical') continue
      const owner = key(identity.id.module, identity.id.name)
      functions.set(owner, fn)
      const applications: Array<Application> = []
      Elaboration.visitStatementFacts(fn.statements, {
        expression: (expression) => {
          if (
            expression._tag !== 'Call' &&
            expression._tag !== 'CallableSection' &&
            expression._tag !== 'FunctionItem'
          )
            return
          if (expression.reference._tag !== 'Resolved') return
          const target = expression.reference.declaration
          if (target.canonical._tag !== 'Canonical') return
          let substitution: Type.Substitution
          if (expression._tag === 'Call') {
            if (expression.contract._tag !== 'Compatible') return
            substitution = expression.contract.substitution
          } else if (expression._tag === 'CallableSection') substitution = expression.substitution
          else
            substitution = new Map(
              target.typeParameters.flatMap((parameter, ordinal) => {
                const argument = expression.typeArguments.at(ordinal)
                return argument === undefined ? [] : [[Type.key(parameter.type), argument] as const]
              }),
            )
          applications.push({
            target: key(target.canonical.id.module, target.canonical.id.name),
            substitution,
            span: expression.syntax.span,
          })
        },
      })
      calls.set(owner, applications)
    }
  }
  type Feature = Parameters<typeof Diagnostic.unsupportedLifetimeFeature>[0]
  interface Node {
    readonly features: Set<Feature>
    readonly predecessors: Set<Node>
  }
  const nodes = new Map<string, Node>()
  const pending: Array<{ readonly node: Node; readonly feature: Feature }> = []
  const instantiate = (
    application: Application,
    parent: Type.Substitution,
    active: ReadonlyMap<string, Node>,
  ): Node | undefined => {
    const target = functions.get(application.target)
    if (target === undefined) return undefined
    const substitution = new Map(
      [...application.substitution].map(
        ([parameter, argument]) =>
          [parameter, Type.substituteGenericArgument(argument, parent)] as const,
      ),
    )
    const identity = JSON.stringify([
      application.target,
      [...substitution]
        .map(([parameter, argument]) => [parameter, Type.genericArgumentKey(argument)])
        .sort((left, right) => JSON.stringify(left).localeCompare(JSON.stringify(right))),
    ])
    const previous = nodes.get(identity) ?? active.get(application.target)
    if (previous !== undefined) return previous
    const node: Node = { features: new Set(), predecessors: new Set() }
    nodes.set(identity, node)
    for (const diagnostic of LifetimeAdmission.instantiate(
      context,
      target.lifetimeAdmission ?? [],
      substitution,
      application.span,
    )) {
      if (
        diagnostic.reason._tag === 'UnsupportedLifetimeFeature' &&
        !node.features.has(diagnostic.reason.feature)
      ) {
        node.features.add(diagnostic.reason.feature)
        pending.push({ node, feature: diagnostic.reason.feature })
      }
    }
    // Existing polymorphic-recursion checking rejects expanding substitutions. Repeated selected
    // declarations form graph edges; the finite feature propagation below handles their SCCs.
    const next = new Map(active).set(application.target, node)
    for (const nested of calls.get(application.target) ?? [])
      instantiate(nested, substitution, next)?.predecessors.add(node)
    return node
  }
  const roots: Array<{ readonly node: Node; readonly span: SourceSpan.SourceSpan }> = []
  for (const applications of calls.values())
    for (const application of applications) {
      const node = instantiate(application, new Map(), new Map())
      if (node !== undefined) roots.push({ node, span: application.span })
    }
  for (let ordinal = 0; ordinal < pending.length; ordinal++) {
    const current = pending[ordinal]
    if (current === undefined) continue
    for (const predecessor of current.node.predecessors) {
      if (predecessor.features.has(current.feature)) continue
      predecessor.features.add(current.feature)
      pending.push({ node: predecessor, feature: current.feature })
    }
  }
  return Diagnostic.merge(
    roots.flatMap(({ node, span }) =>
      [...node.features].map((feature) => Diagnostic.unsupportedLifetimeFeature(feature, span)),
    ),
  )
}
