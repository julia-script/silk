import * as ConformanceProof from './ConformanceProof.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as Type from './Type.js'

/** Rejects reachable Drop-hook instances whose concrete provider is Copy. */
export const copyDropViolations = (
  self: Instances.Discovery,
  index: DeclarationFacts.Index,
): ReadonlyArray<Diagnostic.Diagnostic> =>
  Object.freeze(
    self.instances.flatMap((instance) => {
      if (!instance.key.declaration.name.startsWith('drop@impl#')) return []
      if (instance.key.typeArguments.length === 0) return []
      const parameter = instance.function.declaration.parameters.at(0)
      if (parameter?.declaredType._tag !== 'Resolved') return []
      const selfType = Type.substitute(parameter.declaredType.type, instance.substitution)
      if (!Type.isReference(selfType)) return []
      return DeclarationIndex.copyType(index, selfType.target)
        ? [
            Diagnostic.invalidDropHook(
              `Copy type ${Type.encode(selfType.target)} cannot implement Drop`,
              instance.function.declaration.syntax.span,
            ),
          ]
        : []
    }),
  )

/** Rejects concrete requirement bindings whose provider does not implement the capability. */
export const requirementBindingViolations = (
  self: Instances.Discovery,
  index: DeclarationFacts.Index,
): ReadonlyArray<Diagnostic.Diagnostic> =>
  Object.freeze(
    self.instances.flatMap((instance) =>
      Instances.requirementBindings(instance.function).flatMap((binding) => {
        const proof = Instances.requirementSelection(instance, binding.provider)
        const capability = proof?.selected.capability
        const provider =
          proof?.provider ?? Type.substitute(binding.provider.providerType, instance.substitution)
        if (
          capability !== undefined &&
          Type.isNominal(capability) &&
          ConformanceProof.witness(index, provider, capability) !== undefined
        )
          return []
        return [
          Diagnostic.invalidEffectProvision(
            `provider type ${Type.encode(provider)} does not match ${capability === undefined ? 'one concrete selected requirement' : Type.encode(capability)}`,
            binding.provider.span,
          ),
        ]
      }),
    ),
  )

/** Rejects reachable bound calls whose selected witness has no lowerable implementation. */
export const unlowerableWitnessViolations = (
  self: Instances.Discovery,
  index: DeclarationFacts.Index,
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
          const intrinsic = ConformanceProof.interfaceOperationIntrinsic(
            index,
            provider,
            capability,
            expression.operation,
          )
          const witness = ConformanceProof.interfaceWitnessImplementation(
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

/** Rejects reachable constructions that retain bare or represented callable values. */
export const storedCallableViolations = (
  self: Instances.Discovery,
  index: DeclarationFacts.Index,
): ReadonlyArray<Diagnostic.Diagnostic> =>
  Instances.storedExecutableViolations(self, index, 'Callable')

/** Rejects reachable constructions that retain represented Effect values. */
export const storedEffectViolations = (
  self: Instances.Discovery,
  index: DeclarationFacts.Index,
): ReadonlyArray<Diagnostic.Diagnostic> =>
  Instances.storedExecutableViolations(self, index, 'Effect')

/** Produces semantic diagnostics for every finite-discovery violation. */
export const violationDiagnostics = (
  self: Instances.Discovery,
): ReadonlyArray<Diagnostic.Diagnostic> =>
  Object.freeze([
    ...self.specializationFailures.map((failure) =>
      Diagnostic.nonConcreteSpecialization(
        `${failure.key.declaration.module}.${failure.key.declaration.name}`,
        failure.span,
      ),
    ),
    ...self.violations.flatMap((violation) => {
      const caller = self.instances.find(
        (instance) => Instances.keyText(instance.key) === Instances.keyText(violation.caller),
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
  ])
