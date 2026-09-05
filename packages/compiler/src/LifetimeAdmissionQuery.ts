import * as ConformanceGoal from './ConformanceGoal.js'
import * as ConformanceProof from './ConformanceProof.js'
import * as TypeInference from './internal/TypeInference.js'
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
  readonly proofs?: ReadonlyArray<ConformanceGoal.Proof>
}

interface InterfaceApplication {
  readonly provider: Type.Type
  readonly capability: Type.Nominal
  readonly operation: string
  readonly proofs?: ReadonlyArray<ConformanceGoal.Proof>
  readonly span: SourceSpan.SourceSpan
}
interface ProofEvidence {
  readonly proof: ConformanceGoal.Proof
  readonly substitution: Type.Substitution
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
  const interfaceCalls = new Map<string, ReadonlyArray<InterfaceApplication>>()
  for (const module of modules.values()) {
    for (const fn of [...module.functions, ...module.hiddenFunctions]) {
      const identity = fn.declaration.canonical
      if (identity._tag !== 'Canonical') continue
      const owner = key(identity.id.module, identity.id.name)
      functions.set(owner, fn)
      const applications: Array<Application> = []
      const interfaceApplications: Array<InterfaceApplication> = []
      Elaboration.visitStatementFacts(fn.statements, {
        expression: (expression) => {
          if (expression._tag === 'CallableApply') {
            const target = expression.sourceTarget?.declaration.canonical
            if (target?._tag === 'Canonical')
              applications.push({
                target: key(target.id.module, target.id.name),
                substitution: expression.substitution,
                span: expression.syntax.span,
                proofs: expression.selectedConformances ?? [],
              })
            return
          }
          if (
            expression._tag !== 'Call' &&
            expression._tag !== 'Operator' &&
            expression._tag !== 'CallableSection' &&
            expression._tag !== 'FunctionItem'
          )
            return
          if (expression.reference._tag === 'ResolvedInterfaceOperation') {
            interfaceApplications.push({
              ...expression.reference,
              span: expression.syntax.span,
              ...(expression._tag === 'Call' || expression._tag === 'Operator'
                ? { proofs: expression.selectedConformances }
                : {}),
            })
            return
          }
          if (expression._tag === 'Operator' || expression.reference._tag !== 'Resolved') return
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
            proofs: expression.selectedConformances ?? [],
          })
        },
      })
      calls.set(owner, applications)
      interfaceCalls.set(owner, interfaceApplications)
    }
  }
  type Feature = Parameters<typeof Diagnostic.unsupportedLifetimeFeature>[0]
  interface Node {
    readonly features: Set<Feature>
    readonly predecessors: Set<Node>
  }
  const nodes = new Map<string, Node>()
  const pending: Array<{ readonly node: Node; readonly feature: Feature }> = []
  const retainProofs = (
    proofs: Map<string, ProofEvidence>,
    selected: ReadonlyArray<ConformanceGoal.Proof>,
    substitution: Type.Substitution,
  ): void => {
    const retainProof = (proof: ConformanceGoal.Proof): void => {
      const capability = Type.substitute(proof.goal.capability, substitution)
      const provider = Type.substitute(proof.goal.provider, substitution)
      if (Type.isNominal(capability))
        proofs.set(Type.conformanceKey(capability, provider), { proof, substitution })
      if (proof._tag === 'Proved')
        for (const requirement of proof.requirements) retainProof(requirement)
    }
    for (const proof of selected) retainProof(proof)
  }
  const resolveInterface = (
    nested: InterfaceApplication,
    substitution: Type.Substitution,
    proofs: Map<string, ProofEvidence>,
  ): Application | undefined => {
    retainProofs(proofs, nested.proofs ?? [], substitution)
    const capability = Type.substitute(nested.capability, substitution)
    const provider = Type.substitute(nested.provider, substitution)
    if (!Type.isNominal(capability)) return undefined
    const evidence = proofs.get(Type.conformanceKey(capability, provider))
    if (evidence === undefined) return undefined
    const selected = ConformanceProof.selectedInterfaceTarget(
      index,
      evidence.proof,
      nested.operation,
    )
    if (selected === undefined) return undefined
    const targetKey = key(selected.implementation.module, selected.implementation.name)
    const target = functions.get(targetKey)
    if (target === undefined) return undefined
    const selectedArguments = selected.typeArguments.map((argument) =>
      Type.substituteGenericArgument(argument, evidence.substitution),
    )
    const selectedSubstitution = TypeInference.substitution(
      target.declaration.typeParameters.map((parameter) => parameter.type),
      selectedArguments,
    )
    if (selectedSubstitution === undefined) return undefined
    return { target: targetKey, substitution: selectedSubstitution, span: nested.span }
  }
  const instantiate = (
    application: Application,
    parent: Type.Substitution,
    active: ReadonlyMap<string, Node>,
    inheritedProofs: ReadonlyMap<string, ProofEvidence>,
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
    const proofs = new Map(inheritedProofs)
    retainProofs(proofs, application.proofs ?? [], parent)
    for (const nested of calls.get(application.target) ?? [])
      instantiate(nested, substitution, next, proofs)?.predecessors.add(node)
    for (const nested of interfaceCalls.get(application.target) ?? []) {
      const selected = resolveInterface(nested, substitution, proofs)
      if (selected !== undefined)
        instantiate(selected, new Map(), next, proofs)?.predecessors.add(node)
    }
    return node
  }
  const roots: Array<{ readonly node: Node; readonly span: SourceSpan.SourceSpan }> = []
  for (const applications of calls.values())
    for (const application of applications) {
      const node = instantiate(application, new Map(), new Map(), new Map())
      if (node !== undefined) roots.push({ node, span: application.span })
    }
  for (const applications of interfaceCalls.values())
    for (const application of applications) {
      const proofs = new Map<string, ProofEvidence>()
      const selected = resolveInterface(application, new Map(), proofs)
      if (selected === undefined) continue
      const node = instantiate(selected, new Map(), new Map(), proofs)
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
