import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Hir from '../../dist/Hir.js'
import * as Instances from '../../dist/Instances.js'
import * as RepresentationField from '../../dist/RepresentationField.js'
import * as Type from '../../dist/Type.js'

const validSource = `struct Parser<A, F: fn(A) -> A> { parse: F }
struct Wrapper<A, F: fn(A) -> A> { first: Parser<A, F> second: Parser<A, F> }
struct Deferred<F: Effect<i32>> { operation: F }
struct EffectWrapper<F: Effect<i32>> { first: Deferred<F> second: Deferred<F> }
struct CallableLeaf<F: fn(i32) -> i32> { operation: F }
struct EffectLeaf<G: Effect<i32>> { operation: G }
struct MultipleInner<F: fn(i32) -> i32, G: Effect<i32>> { callable: F deferred: G }
struct MultipleOuter<F: fn(i32) -> i32, G: Effect<i32>> { inner: MultipleInner<F, G> }
struct MultipleUnion<F: fn(i32) -> i32, G: Effect<i32>> {
  value: CallableLeaf<F> | EffectLeaf<G>
}
fn decode(value: i32) -> i32 { return value }
fn consume<A, F: fn(A) -> A>(parser: Parser<A, F>) -> i32 { return 0 }
pub fn main() -> i32 {
  let parser = Parser<i32> { parse: decode }
  let deferred = Deferred { operation: effect { return 1 } }
  return consume<i32>(move parser)
}`
const conflictSource = `struct Mapper<F: fn(i32) -> i32> { first: F second: F }
struct First {}
struct Second {}
fn decimal(value: i32) -> i32 { return value }
fn hexadecimal(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let parser = Mapper { first: decimal, second: hexadecimal }
  return 0
}
fn choose(input: First | Second) -> i32 {
  let parser = match move input {
    First {} => Mapper { first: decimal, second: decimal }
    Second {} => Mapper { first: hexadecimal, second: hexadecimal }
  }
  return 0
}
struct SharedParser<A, F: fn(A) -> A> { parse: F }
fn incompatible<A, F: once fn(A) -> A>(parse: F) -> i32 {
  let parser = SharedParser<A> { parse: move parse }
  return 0
}
struct SharedDeferred<A, F: Effect<A>> { operation: F }
fn incompatibleEffect<A, F: once Effect<A>>(operation: F) -> i32 {
  let deferred = SharedDeferred<A> { operation: move operation }
  return 0
}`
const fenceSource = `struct Parser<F: fn(i32) -> i32> { parse: F }
struct Deferred<F: Effect<i32>> { operation: F }
fn decode(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let parser = Parser { parse: decode }
  let deferred = Deferred { operation: effect { return 1 } }
  let decoded = parser.parse(1)
  let completed = run deferred.operation
  return decoded + completed
}`
const identitySource = `struct Mappers<F: fn(i32) -> i32, G: fn(i32) -> i32> { first: F second: G }
struct Deferred<F: Effect<i32>, G: Effect<i32>> { first: F second: G }
pub fn main() -> i32 {
  let mappers = Mappers { first: i32.add(1), second: i32.add(1) }
  let deferred = Deferred { first: effect { return 1 }, second: effect { return 1 } }
  return 0
}`
const shiftedIdentitySource = `// Moving source trivia must not rename executable sites.

struct Mappers<F: fn(i32) -> i32, G: fn(i32) -> i32> { first: F second: G }
struct Deferred<F: Effect<i32>, G: Effect<i32>> { first: F second: G }
pub fn main() -> i32 {
  // Same-shaped sites remain distinct while retaining their structural ordinals.
  let mappers = Mappers { first: i32.add(1), second: i32.add(1) }
  let deferred = Deferred { first: effect { return 1 }, second: effect { return 1 } }
  return 0
}`
const runtimeIdentitySource = `pub fn main() -> i32 {
  let plusOne = i32.add(1)
  let answer = plusOne(41)
  let pending = effect { return answer }
  return run pending
}`
const shiftedRuntimeIdentitySource = `// Source offsets are diagnostic provenance, not symbols.

pub fn main() -> i32 {
  // The callable and effect keep the same preorder sites.
  let plusOne = i32.add(1)
  let answer = plusOne(41)

  let pending = effect { return answer }
  return run pending
}`
const encoder = new TextEncoder()
const validModule = 'fixture/representation-determinism'
const conflictModule = 'fixture/representation-determinism-conflict'
const valid = await Effect.runPromise(
  Analysis.ofSourceRealized(validModule, encoder.encode(validSource)),
)
const conflict = await Effect.runPromise(
  Analysis.ofSource(conflictModule, encoder.encode(conflictSource)),
)
const fences = await Effect.runPromise(
  Analysis.ofSourceRealized('fixture/representation-fences', encoder.encode(fenceSource)),
)
const identityModule = 'fixture/representation-identity-stability'
const identity = await Effect.runPromise(
  Analysis.ofSource(identityModule, encoder.encode(identitySource)),
)
const shiftedIdentity = await Effect.runPromise(
  Analysis.ofSource(identityModule, encoder.encode(shiftedIdentitySource)),
)
const runtimeIdentity = await Effect.runPromise(
  Analysis.ofSourceRealized(
    'fixture/representation-runtime-identity',
    encoder.encode(runtimeIdentitySource),
    'wasm32-unknown-unknown',
  ),
)
const shiftedRuntimeIdentity = await Effect.runPromise(
  Analysis.ofSourceRealized(
    'fixture/representation-runtime-identity',
    encoder.encode(shiftedRuntimeIdentitySource),
    'wasm32-unknown-unknown',
  ),
)
const result = Analysis.rootAnalysis(valid)
const main = result.functions.at(2)
const statement = main?.statements.at(0)
const binding = statement?._tag === 'BindStatement' ? statement.binding : undefined
const representedNominals = (main?.statements ?? []).flatMap((candidate) =>
  candidate._tag === 'BindStatement' &&
  candidate.binding.inferredType._tag === 'Available' &&
  Type.isNominal(candidate.binding.inferredType.type)
    ? [candidate.binding.inferredType.type]
    : [],
)
const parserInstance = representedNominals.find((type) => type.name === 'Parser')
const deferredInstance = representedNominals.find((type) => type.name === 'Deferred')
const wrapperInstance =
  parserInstance === undefined
    ? undefined
    : Type.nominal(validModule, 'Wrapper', parserInstance.arguments)
const effectWrapperInstance =
  deferredInstance === undefined
    ? undefined
    : Type.nominal(validModule, 'EffectWrapper', deferredInstance.arguments)
const callableArgument = parserInstance?.arguments.find(Type.isExactRepresentationArgument)
const effectArgument = deferredInstance?.arguments.find(Type.isExactRepresentationArgument)
const multipleOuterInstance =
  callableArgument === undefined || effectArgument === undefined
    ? undefined
    : Type.nominal(validModule, 'MultipleOuter', [callableArgument, effectArgument])
const multipleUnionInstance =
  callableArgument === undefined || effectArgument === undefined
    ? undefined
    : Type.nominal(validModule, 'MultipleUnion', [callableArgument, effectArgument])
const fieldInstances = [
  parserInstance,
  wrapperInstance,
  deferredInstance,
  effectWrapperInstance,
  multipleOuterInstance,
  multipleUnionInstance,
].filter((type) => type !== undefined)
const fieldPlans = fieldInstances.flatMap((instance) =>
  RepresentationField.plansOf(valid.index, instance),
)
const resolvedFields = RepresentationField.resolveFields(valid.index, fieldInstances)
const openInstances = fieldInstances.flatMap((instance) => {
  const plans = RepresentationField.plansOf(valid.index, instance)
  const parameters = []
  for (const plan of plans) {
    if (!parameters.some((parameter) => Type.key(parameter) === Type.key(plan.parameter)))
      parameters.push(plan.parameter)
  }
  let representationOrdinal = 0
  const arguments_ = instance.arguments.map((argument) => {
    if (!Type.isExactRepresentationArgument(argument)) return argument
    const parameter = parameters.at(representationOrdinal++)
    return parameter === undefined ? argument : Type.representationParameterArgument(parameter)
  })
  return [Type.nominal(instance.module, instance.name, arguments_)]
})
const unavailableFields = RepresentationField.resolveFields(valid.index, openInstances)
const hover = Analysis.hoverSubjectAt(valid, validModule, validSource.indexOf('parser ='))

const encodeField = (resolution) => ({
  instance: Type.key(resolution.instance),
  field: RepresentationField.idKey(resolution.id),
  key: RepresentationField.key(resolution.instance, resolution.id),
  ...(resolution._tag === 'ResolvedRepresentationField'
    ? {
        argument: Type.genericArgumentKey(resolution.argument),
        requiredBound: Type.key(resolution.requiredBound),
        admissibility: resolution.admissibility._tag,
      }
    : {
        requiredBound: Type.key(resolution.reason.requiredBound),
        reason: resolution.reason._tag,
        provenance: resolution.provenance,
      }),
})

const identityFacts = (snapshot) => {
  const main = Analysis.rootAnalysis(snapshot).functions.find(
    (fact) => fact.declaration.name._tag === 'Present' && fact.declaration.name.spelling === 'main',
  )
  const instances = (main?.statements ?? []).flatMap((statement) =>
    statement._tag === 'BindStatement' &&
    statement.binding.inferredType._tag === 'Available' &&
    Type.isNominal(statement.binding.inferredType.type)
      ? [statement.binding.inferredType.type]
      : [],
  )
  return instances.flatMap((instance) => {
    const resolutions = RepresentationField.resolveFields(snapshot.index, [instance])
    return RepresentationField.plansOf(snapshot.index, instance).map((plan) => {
      const resolution = RepresentationField.lookup(resolutions, instance, plan.id)
      return {
        nominal: Type.key(instance),
        field: RepresentationField.key(instance, plan.id),
        argument:
          resolution?._tag === 'ResolvedRepresentationField'
            ? Type.genericArgumentKey(resolution.argument)
            : '',
      }
    })
  })
}

const runtimeIdentityFacts = (snapshot) => {
  const discovery = Analysis.instancesOf(snapshot)
  const layout = Analysis.layoutOf(snapshot)
  return {
    callables: discovery.callables.map(Instances.callableIdentity),
    effects:
      layout._tag === 'Available'
        ? layout.value.effectEnvironments.map((environment) =>
            Instances.effectIdentity(environment.instance, environment.site),
          )
        : [],
    runners: Analysis.loweredMir(snapshot)
      .functions.map((fn) => fn.id.name)
      .filter((name) => name.includes('$effect$')),
  }
}

process.stdout.write(
  JSON.stringify({
    semantic:
      binding?.inferredType._tag === 'Available'
        ? Type.key(binding.inferredType.type)
        : 'unavailable',
    hir: Hir.encode(result.hir),
    instances: Analysis.instancesOf(valid).instances.map((instance) => ({
      declaration: instance.key.declaration,
      arguments: instance.key.typeArguments.map(Type.genericArgumentKey),
    })),
    presentation: hover?.presentation.text,
    representationFields: {
      plans: fieldPlans.map((plan) => ({
        field: RepresentationField.idKey(plan.id),
        parameter: Type.key(plan.parameter),
        requiredBound: Type.key(plan.requiredBound),
      })),
      resolved: resolvedFields.resolutions.map(encodeField),
      unavailable: unavailableFields.resolutions.map(encodeField),
    },
    identityStability: {
      baseline: identityFacts(identity),
      shifted: identityFacts(shiftedIdentity),
    },
    runtimeIdentityStability: {
      baseline: runtimeIdentityFacts(runtimeIdentity),
      shifted: runtimeIdentityFacts(shiftedRuntimeIdentity),
    },
    diagnostics: Analysis.diagnostics(conflict).map((diagnostic) => ({
      code: diagnostic.code,
      message: diagnostic.message,
      reason: diagnostic.reason,
      span: diagnostic.span,
      relatedSpans: diagnostic.relatedSpans,
    })),
    fences: {
      diagnostics: Analysis.diagnostics(fences).map((diagnostic) => ({
        code: diagnostic.code,
        message: diagnostic.message,
        reason: diagnostic.reason,
        span: diagnostic.span,
        relatedSpans: diagnostic.relatedSpans,
      })),
      layout: fences.layout._tag,
      mir: fences.mir._tag,
    },
  }),
)
