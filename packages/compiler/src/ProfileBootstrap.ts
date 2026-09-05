import * as Effect from 'effect/Effect'
import * as CompilationProfile from './CompilationProfile.js'
import * as ConfigurationError from './ConfigurationError.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Elaboration from './Elaboration.js'
import * as Canonical from './internal/Canonical.js'
import type * as NameResolution from './NameResolution.js'
import * as PackageConfiguration from './PackageConfiguration.js'
import * as PackageParameter from './PackageParameter.js'
import * as Residualization from './Residualization.js'
import * as StaticEvaluation from './StaticEvaluation.js'
import type * as StaticValue from './StaticValue.js'
import * as Type from './Type.js'

/** The unconditional source graph needed to resolve package defaults before specialization. */
export interface Source {
  readonly index: DeclarationIndex.Index
  readonly results: ReadonlyMap<string, Elaboration.Result>
  readonly resolution: NameResolution.Resolution
  readonly modules: ReadonlyArray<PackageConfiguration.Module>
}

const failure = (
  parameter: PackageConfiguration.Parameter,
  value: StaticEvaluation.StaticFailure,
  validation: boolean,
) =>
  new ConfigurationError.ConfigurationError({
    ...ConfigurationError.make(
      'ProfileBootstrap.complete',
      value._tag === 'Cycle'
        ? 'DependencyCycle'
        : validation
          ? 'ValidationFailed'
          : 'InvalidDefault',
      CompilationProfile.parameterKey(parameter),
      [parameter.origin],
      value._tag === 'Cycle' ? [`${value.declaration.module}/${value.declaration.name}`] : [],
    ),
    staticFailure: value,
  })

/** A completed profile paired with immutable source-typed values for ordinary specialization. */
export interface Completion {
  readonly profile: CompilationProfile.CompilationProfile
  readonly values: ReadonlyMap<string, StaticValue.Value>
}

/** Resolves defaults and validates every final value before publishing an immutable profile. */
export const complete = Effect.fn('ProfileBootstrap.complete')(function* (
  initial: CompilationProfile.Initial,
  source: Source,
  bindings: ReadonlyArray<PackageConfiguration.Binding> = [],
): Effect.fn.Return<Completion, ConfigurationError.ConfigurationError> {
  const parameters = yield* PackageConfiguration.prepare(
    source.index,
    initial.target,
    source.modules,
    bindings,
  )
  const explicit = new Map<string, StaticValue.Value>()
  for (const parameter of parameters) {
    const canonical = parameter.declaration.canonical
    if (canonical._tag === 'Canonical' && parameter.explicit !== undefined)
      explicit.set(
        Canonical.record('PackageParameter', [canonical.id.module, canonical.id.name]),
        parameter.explicit,
      )
  }
  // This coordinator is private to the bootstrap. All explicit values exist before any demand;
  // recursive defaults, including demands made by helpers, use the ordinary evaluator's cycle guard.
  const coordinator = Residualization.make(
    initial,
    source.results,
    source.resolution,
    source.index,
    StaticEvaluation.defaultLimits,
    explicit,
  )
  const completed: Array<CompilationProfile.Parameter> = []
  const values = new Map<string, StaticValue.Value>()
  for (const parameter of parameters) {
    const evaluated = Residualization.evaluateConstant(coordinator, parameter.declaration)
    if (evaluated._tag === 'Failed') return yield* failure(parameter, evaluated.failure, false)
    const canonical = parameter.declaration.canonical
    if (canonical._tag === 'Canonical') values.set(Canonical.record('PackageParameter', [canonical.id.module, canonical.id.name]), evaluated.value)
    const value = yield* PackageParameter.unbind(
      parameter.schema,
      evaluated.value,
      parameter.origin,
      initial.target,
    )
    completed.push(
      Object.freeze({
        package: parameter.package,
        module: parameter.module,
        parameter: parameter.parameter,
        type: Type.encode(parameter.schema.type),
        value,
        origin: parameter.origin,
      }),
    )
  }
  for (const parameter of parameters) {
    const evaluated = Residualization.evaluateParameterPredicate(coordinator, parameter.declaration)
    if (evaluated._tag === 'Failed') return yield* failure(parameter, evaluated.failure, true)
    if (evaluated.value._tag !== 'BooleanValue' || !evaluated.value.value)
      return yield* ConfigurationError.make(
        'ProfileBootstrap.complete',
        'ValidationFailed',
        CompilationProfile.parameterKey(parameter),
        [parameter.origin],
      )
  }
  return Object.freeze({profile: yield* CompilationProfile.publish(initial, completed), values})
})
