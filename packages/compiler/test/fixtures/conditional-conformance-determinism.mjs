import { createHash } from 'node:crypto'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as ConformanceGoal from '../../dist/ConformanceGoal.js'
import * as ConformanceHead from '../../dist/ConformanceHead.js'
import * as ConformanceProof from '../../dist/ConformanceProof.js'
import * as MirEncoding from '../../dist/MirEncoding.js'
import * as Type from '../../dist/Type.js'

/**
 * One base interface, one base witness for each of two unrelated types, and two wrappers whose
 * decoder conformances hold exactly when their source type has one.
 *
 * The declarations are written in a deliberately unhelpful order: the outer wrapper precedes the
 * inner one, and both precede every base witness they can only be proved through. Nothing here may
 * be answered by reading the file top to bottom, so a canonical fact that survives this ordering
 * survives any ordering. Each wrapper also adds a distinct amount to what its source decodes, so a
 * specialization that reached the wrong witness cannot produce the expected number by accident.
 */
const module_ = 'fixture/conditional-conformance-determinism'
const source = `interface Decoder {
  fn decode(value: &Self) -> i32
}

struct OptionalSchema<S> { source: S }

fn optionalDecode<S: Decoder>(value: &OptionalSchema<S>) -> i32 {
  return Decoder.decode(&value.source) + 2
}

impl<S: Decoder> Decoder for OptionalSchema<S> {
  decode: OptionalSchema.optionalDecode
}

struct MappedSchema<S> { source: S }

fn mappedDecode<S: Decoder>(value: &MappedSchema<S>) -> i32 {
  return Decoder.decode(&value.source) + 1
}

impl<S: Decoder> Decoder for MappedSchema<S> {
  decode: MappedSchema.mappedDecode
}

struct Loose { weight: i32 }

struct Tally { count: i32 }

fn tallyDecode(value: &Tally) -> i32 { return value.count }

impl Decoder for Tally { decode: Tally.tallyDecode }

struct Schema { tag: i32 }

fn schemaDecode(value: &Schema) -> i32 { return value.tag }

impl Decoder for Schema { decode: Schema.schemaDecode }

fn decodeOf<T: Decoder>(value: T) -> i32 { return Decoder.decode(&value) }

pub fn main() -> i32 {
  let nested = decodeOf<OptionalSchema<MappedSchema<Schema>>>(
    OptionalSchema<MappedSchema<Schema>> {
      source: MappedSchema<Schema> { source: Schema { tag: 41 } }
    }
  )
  let plain = decodeOf<MappedSchema<Tally>>(MappedSchema<Tally> { source: Tally { count: 7 } })
  return nested + plain
}`

const bytes = new TextEncoder().encode(source)
const hash = (value) => createHash('sha256').update(value).digest('hex')
const analyze = (target) => Effect.runPromise(Analysis.ofSourceRealized(module_, bytes, target))

// Both targets are pinned. A host-resolved triple would make the artifact hashes describe the
// machine that ran the fixture rather than the compiler that produced them.
const wasm = await analyze('wasm32-unknown-unknown')
const native = await analyze('aarch64-apple-darwin')
const wasmArtifact = await Effect.runPromise(Analysis.codegenWasm(wasm, { mode: 'release' }))
const nativeArtifact = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))

const index = Analysis.declarationIndex(wasm)
const nominal = (name, arguments_ = []) => Type.nominal(module_, name, arguments_)
const decoder = () => nominal('Decoder')
const schema = nominal('Schema')
const tally = nominal('Tally')
const loose = nominal('Loose')
const mappedSchema = nominal('MappedSchema', [schema])
const mappedTally = nominal('MappedSchema', [tally])
const nestedSchema = nominal('OptionalSchema', [mappedSchema])
const optionalLoose = nominal('OptionalSchema', [loose])

/**
 * The goals, asked in an order chosen to be useless.
 *
 * The deepest goal comes first, so every memo entry the shallower goals could reuse is written by a
 * proof that was already in flight rather than by the direct question; the unprovable goal sits
 * between two provable ones, so a failure is neither the first nor the last thing the memo saw; and
 * one base goal is asked before the wrapper that descends into it while the other is asked after, so
 * neither arrival direction is the only one exercised. The report lists the answers in
 * canonical key order — the order `ConformanceGoal.compare` defines — which is the whole point:
 * asking order is provenance and must not reach the emitted bytes.
 */
const askedProviders = [nestedSchema, schema, optionalLoose, mappedTally, tally, mappedSchema]

const proofDependencies = (proof) => {
  const found = new Map()
  const visit = (current) => {
    if (current._tag !== 'Proved') return
    for (const requirement of current.requirements) visit(requirement)
    const key = ConformanceGoal.key(current.goal)
    if (!found.has(key)) found.set(key, current.goal)
  }
  visit(proof)
  return Object.freeze([...found.values()])
}

const encodeProof = ({ goal, proof }) => ({
  goal: ConformanceGoal.key(goal),
  encoded: ConformanceGoal.encode(goal),
  proved: proof._tag === 'Proved',
  selection: proof._tag === 'Proved' ? proof.selection._tag : proof.failure._tag,
  typeArguments:
    proof._tag === 'Proved' ? proof.typeArguments.map(Type.genericArgumentKey) : Object.freeze([]),
  dependencies: proofDependencies(proof).map(ConformanceGoal.key),
  trace: ConformanceGoal.traceLines(proof),
})

const byGoal = (left, right) => {
  if (left.goal < right.goal) return -1
  if (left.goal > right.goal) return 1
  return 0
}

const proveAll = (target, providers) =>
  providers
    .map((provider) => ({
      goal: ConformanceGoal.make(decoder(provider), provider),
      proof: ConformanceProof.prove(target, provider, decoder(provider)),
    }))
    .map(encodeProof)
    .sort(byGoal)

const proofs = proveAll(index, askedProviders)

/**
 * The same six answers, asked backwards of a second index that has never been asked anything.
 *
 * Proof memoization is per-index, so a fresh index is the only way to reach the search with an empty
 * memo and a different arrival order. Emitting both lists lets the two be compared as a single
 * equality rather than trusted: if asking order leaked into a proof — into which candidate was
 * selected, which dependencies were recorded, or the order they were recorded in — the two lists
 * disagree and the blob says so instead of hiding it behind a stable-but-wrong answer.
 */
const reversedIndex = Analysis.declarationIndex(await analyze('wasm32-unknown-unknown'))
const reversedProofs = proveAll(reversedIndex, [...askedProviders].reverse())

const conformanceFacts = index.modules.flatMap((module) =>
  module.conformances.map((conformance) => ({
    module: module.module,
    ordinal: conformance.ordinal,
    head: conformance.head === undefined ? '' : ConformanceHead.key(conformance.head),
    encoded: conformance.head === undefined ? '' : ConformanceHead.encode(conformance.head),
    coherence: conformance.coherence._tag,
    termination: conformance.termination._tag,
    requirements: conformance.requirements.map((requirement) => requirement.spelling),
    requirementProviders:
      conformance.head === undefined
        ? Object.freeze([])
        : conformance.head.requirements.map(
            (requirement) =>
              `${Type.key(requirement.capability)} for ${Type.key(requirement.provider)}`,
          ),
    parameters: conformance.head === undefined ? 0 : conformance.head.parameters.length,
  })),
)

/**
 * One head built twice from binders that disagree about everything except shape.
 *
 * The second spelling declares its binders under a different owner, at different ordinals, with
 * different names, and mentions them in the opposite order in the parameter list it was written
 * from. Alpha-normalization renumbers both by first occurrence in the canonical term, so the two
 * keys are equal or the normal form is reading provenance it promised to discard.
 */
const pairHead = (owner, first, second) => {
  const left = Type.parameter(owner, first.ordinal, first.name)
  const right = Type.parameter(owner, second.ordinal, second.name)
  const provider = nominal('Pair', [left, right])
  return ConformanceHead.make(decoder(provider), provider, [
    { capability: decoder(right), provider: right },
    { capability: decoder(left), provider: left },
  ])
}
const canonicalHead = pairHead(
  { module: module_, name: 'impl#0' },
  { ordinal: 0, name: 'S' },
  { ordinal: 1, name: 'T' },
)
const scrambledHead = pairHead(
  { module: 'other/module', name: 'impl#41' },
  { ordinal: 7, name: 'Zeta' },
  { ordinal: 2, name: 'Alpha' },
)

const declaredHeads = index.modules
  .flatMap((module) => module.conformances)
  .flatMap((conformance) => (conformance.head === undefined ? [] : [conformance.head]))

/**
 * The evaluation reduced to the ordered shape of what ran.
 *
 * The result is the load-bearing half: each wrapper adds a different constant, so only a run that
 * reached the mapped witness through the optional one and both through their base witnesses can
 * produce it. The event sequence keeps the ordering-sensitive part without carrying spans that
 * would only restate the source text.
 */
const outcome = Analysis.evaluate(wasm)
const evaluation = {
  outcome: outcome._tag,
  result: outcome._tag === 'Completed' ? Number(outcome.result.value) : undefined,
  events: outcome.trace.map(
    (event) => `${event._tag} ${'function' in event ? event.function.name : ''}`,
  ),
}

process.stdout.write(
  JSON.stringify(
    {
      diagnostics: Analysis.diagnostics(wasm).map((diagnostic) => ({
        code: diagnostic.code,
        message: diagnostic.message,
      })),
      conformances: conformanceFacts,
      normalization: {
        canonical: ConformanceHead.key(canonicalHead),
        scrambled: ConformanceHead.key(scrambledHead),
        canonicalRequirements: canonicalHead.requirements.map((requirement) =>
          Type.key(requirement.provider),
        ),
        scrambledRequirements: scrambledHead.requirements.map((requirement) =>
          Type.key(requirement.provider),
        ),
        // Every distinct pair of declared heads, in both directions, so an asymmetric overlap answer
        // would show up as a disagreeing pair rather than hide behind one arbitrary orientation.
        overlaps: declaredHeads.flatMap((left, leftOrdinal) =>
          declaredHeads.flatMap((right, rightOrdinal) =>
            leftOrdinal === rightOrdinal
              ? []
              : [
                  {
                    left: ConformanceHead.key(left),
                    right: ConformanceHead.key(right),
                    overlaps: ConformanceHead.mayOverlap(left, right),
                  },
                ],
          ),
        ),
      },
      proofs,
      reversedProofs,
      instances: Analysis.instancesOf(wasm).instances.map((instance) => ({
        declaration: instance.key.declaration.name,
        module: instance.key.declaration.module,
        arguments: instance.key.typeArguments.map(Type.genericArgumentKey),
        contractRow: instance.key.contractRow,
      })),
      mir: MirEncoding.encode(Analysis.loweredMir(wasm)),
      evaluation,
      wasm: hash(wasmArtifact.bytes),
      native: hash(nativeArtifact.bitcode),
    },
    (_, value) => (typeof value === 'bigint' ? value.toString() : value),
  ),
)
