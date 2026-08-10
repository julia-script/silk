import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'
import * as MirNormalization from '../src/MirNormalization.js'

const encoder = new TextEncoder()
const source = `import silk.result { Result, Success, Failure }
effect fn succeed(value: i32) -> i32 { return value }
fn addOne(value: i32) -> i32 { return value + 1 }
effect fn userMap(self: once Effect<i32>, onSuccess: once fn(i32) -> i32) -> i32 {
  let completed = run Effect.result(move self)
  return match move completed {
    Result<i32, never> { value: outcome } => match move outcome {
      Success<i32> { value } => onSuccess(move value)
      Failure<never> { error } => move error
    }
  }
}
pub fn main() -> i32 { return run succeed(41) |> userMap(addOne) }`

const snapshot = (normalizeMir: boolean) =>
  Analysis.ofSourceRealized(
    'test/mir-normalization',
    encoder.encode(source),
    'wasm32-unknown-unknown',
    { normalizeMir },
  )

const mainOperations = (program: Mir.Module): ReadonlyArray<Mir.Operation> => {
  const main = program.functions.find((fn) => fn.id.name === 'main')
  return main === undefined ? Object.freeze([]) : Mir.operations(main)
}

it.effect('folds copied constructor shapes and direct static runs without name privilege', () =>
  Effect.gen(function* () {
    const raw = yield* snapshot(false)
    const normalized = yield* snapshot(true)
    assert.deepEqual(Analysis.diagnostics(raw), [])
    assert.deepEqual(Analysis.diagnostics(normalized), [])

    const rawProgram = Analysis.loweredMir(raw)
    const normalizedProgram = Analysis.loweredMir(normalized)
    assert.isTrue(
      mainOperations(rawProgram).some((operation) => operation._tag === 'ApplyCallable'),
    )
    assert.isTrue(
      mainOperations(rawProgram).some((operation) => operation._tag === 'RunEffectValue'),
    )
    assert.isFalse(
      mainOperations(normalizedProgram).some((operation) => operation._tag === 'ApplyCallable'),
    )
    assert.isTrue(
      mainOperations(normalizedProgram).some((operation) => operation._tag === 'RunStaticEffect'),
    )
    assert.isTrue(
      (normalizedProgram.normalization ?? []).some(
        (verdict) => verdict._tag === 'Normalized' && verdict.kind === 'FoldedConstructor',
      ),
    )
    assert.isTrue(
      (normalizedProgram.normalization ?? []).some(
        (verdict) => verdict._tag === 'Normalized' && verdict.kind === 'DirectStaticRun',
      ),
    )
    assert.deepEqual(Mir.verify(normalizedProgram), [])
    assert.strictEqual(MirNormalization.normalize(normalizedProgram), normalizedProgram)
    const rawEvaluation = Analysis.evaluate(raw)
    const normalizedEvaluation = Analysis.evaluate(normalized)
    assert.strictEqual(rawEvaluation._tag, 'Completed')
    assert.strictEqual(normalizedEvaluation._tag, 'Completed')
    if (rawEvaluation._tag === 'Completed' && normalizedEvaluation._tag === 'Completed')
      assert.deepEqual(rawEvaluation.result, normalizedEvaluation.result)
  }),
)

it.effect('rejects the whole candidate when suspension is unknown', () =>
  Effect.gen(function* () {
    const raw = yield* snapshot(false)
    const program = Analysis.loweredMir(raw)
    const rejected = MirNormalization.normalize(program, { suspension: 'Unknown' })
    assert.strictEqual(Mir.encode(rejected).includes('run-static-effect'), false)
    assert.isTrue(
      (rejected.normalization ?? []).some(
        (verdict) => verdict._tag === 'Rejected' && verdict.reason === 'SuspensionUnknown',
      ),
    )
    assert.deepEqual(Mir.verify(rejected), [])
  }),
)

it.effect('keeps affine captures materialized and ownership explicit', () =>
  Effect.gen(function* () {
    const raw = yield* snapshot(false)
    assert.deepEqual(Analysis.diagnostics(raw), [])
    const rawProgram = Analysis.loweredMir(raw)
    const affineProgram: Mir.Module = Object.freeze({
      ...rawProgram,
      functions: Object.freeze(
        rawProgram.functions.map((fn) =>
          Object.freeze({
            ...fn,
            regions: Object.freeze(
              fn.regions.map((region) =>
                region._tag !== 'OperationRegion'
                  ? region
                  : Object.freeze({
                      ...region,
                      operations: Object.freeze(
                        region.operations.map((operation) =>
                          operation._tag !== 'MakeEffect'
                            ? operation
                            : Object.freeze({
                                ...operation,
                                captures: Object.freeze(
                                  operation.captures.map((capture) =>
                                    Object.freeze({ ...capture, access: 'Take' as const }),
                                  ),
                                ),
                              }),
                        ),
                      ),
                    }),
              ),
            ),
          }),
        ),
      ),
    })
    const program = MirNormalization.normalize(affineProgram)
    assert.isTrue(
      (program.normalization ?? []).some(
        (verdict) => verdict._tag === 'Rejected' && verdict.reason === 'AffineCapture',
      ),
    )
    assert.isFalse(
      mainOperations(program).some((operation) => operation._tag === 'RunStaticEffect'),
    )
  }),
)

it.effect('reports complex constructors and reusable Effects without partial static dispatch', () =>
  Effect.gen(function* () {
    const complex = yield* Analysis.ofSourceRealized(
      'test/mir-normalization-complex',
      encoder.encode(`fn complex(value: i32) -> Effect<i32> {
  let adjusted = value + 1
  return effect { return adjusted }
}
pub fn main() -> i32 { return run complex(41) }`),
      'wasm32-unknown-unknown',
    )
    const reused = yield* Analysis.ofSourceRealized(
      'test/mir-normalization-reused',
      encoder.encode(`effect fn succeed() -> i32 { return 21 }
pub fn main() -> i32 {
  let pending = succeed()
  let first = run pending
  let second = run pending
  return first + second
}`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(complex), [])
    assert.deepEqual(Analysis.diagnostics(reused), [])
    assert.isTrue(
      Analysis.effectNormalizationOf(complex).some(
        (verdict) => verdict._tag === 'Rejected' && verdict.reason === 'ComplexConstructor',
      ),
    )
    assert.isTrue(
      Analysis.effectNormalizationOf(reused).some(
        (verdict) => verdict._tag === 'Rejected' && verdict.reason === 'EffectEscapes',
      ),
      JSON.stringify(Analysis.effectNormalizationOf(reused)),
    )
    assert.isFalse(
      mainOperations(Analysis.loweredMir(complex)).some(
        (operation) => operation._tag === 'RunStaticEffect',
      ),
    )
    assert.isFalse(
      mainOperations(Analysis.loweredMir(reused)).some(
        (operation) => operation._tag === 'RunStaticEffect',
      ),
    )
  }),
)

it.effect('verifier rejects dangling normalization identities', () =>
  Effect.gen(function* () {
    const normalized = yield* snapshot(true)
    const program = Analysis.loweredMir(normalized)
    const verdict = program.normalization?.at(0)
    assert.isDefined(verdict)
    if (verdict === undefined) return
    const malformed: Mir.Module = Object.freeze({
      ...program,
      normalization: Object.freeze([
        Object.freeze({
          ...verdict,
          region: Object.freeze({ _tag: 'Region', ordinal: 999 }),
        }),
      ]),
    })
    assert.isTrue(
      Mir.verify(malformed).some((violation) => violation.rule === 'InvalidNormalization'),
    )

    const main = program.functions.find((fn) => fn.id.name === 'main')
    const mainRegion = main?.regions.find(
      (region): region is Mir.OperationRegion => region._tag === 'OperationRegion',
    )
    const run = mainRegion?.operations.find(
      (operation): operation is Extract<Mir.Operation, { readonly _tag: 'RunStaticEffect' }> =>
        operation._tag === 'RunStaticEffect',
    )
    assert.isDefined(main)
    assert.isDefined(mainRegion)
    assert.isDefined(run)
    if (main === undefined || mainRegion === undefined || run === undefined) return
    const inconsistent: Mir.Module = Object.freeze({
      ...program,
      functions: Object.freeze(
        program.functions.map((fn) =>
          fn !== main
            ? fn
            : Object.freeze({
                ...fn,
                regions: Object.freeze(
                  fn.regions.map((region) =>
                    region !== mainRegion
                      ? region
                      : Object.freeze({
                          ...region,
                          operations: Object.freeze(
                            region.operations.map((operation) =>
                              operation !== run
                                ? operation
                                : Object.freeze({ ...run, captures: Object.freeze([]) }),
                            ),
                          ),
                        }),
                  ),
                ),
              }),
        ),
      ),
    })
    assert.isTrue(
      Mir.verify(inconsistent).some((violation) => violation.rule === 'InvalidNormalization'),
    )
  }),
)
