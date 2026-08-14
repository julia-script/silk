import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Hir from '../../dist/Hir.js'
import * as RepresentationField from '../../dist/RepresentationField.js'
import * as Type from '../../dist/Type.js'

const validSource = `struct Parser<A, F: fn(A) -> A> { parse: F }
struct Wrapper<A, F: fn(A) -> A> { first: Parser<A, F> second: Parser<A, F> }
struct Deferred<F: Effect<i32>> { operation: F }
struct EffectWrapper<F: Effect<i32>> { first: Deferred<F> second: Deferred<F> }
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
const fieldInstances = [
  parserInstance,
  wrapperInstance,
  deferredInstance,
  effectWrapperInstance,
].filter((type) => type !== undefined)
const fieldPlans = fieldInstances.flatMap((instance) =>
  RepresentationField.plansOf(valid.index, instance),
)
const resolvedFields = RepresentationField.resolveFields(valid.index, fieldInstances)
const openInstances = fieldInstances.flatMap((instance) => {
  const plan = RepresentationField.plansOf(valid.index, instance).at(0)
  if (plan === undefined) return []
  const arguments_ = instance.arguments.map((argument) =>
    Type.isExactRepresentationArgument(argument)
      ? Type.representationParameterArgument(plan.parameter)
      : argument,
  )
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
