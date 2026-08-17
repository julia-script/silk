import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'
import * as MirNormalization from '../src/MirNormalization.js'
import * as ProvisionalMir from '../src/ProvisionalMir.js'

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

const allOperations = (program: Mir.Module): ReadonlyArray<Mir.Operation> =>
  program.functions.flatMap(Mir.operations)

const provisionalOf = (self: Analysis.Snapshot): ProvisionalMir.Module => {
  const provisional = Analysis.provisionalMirOf(self)
  if (provisional._tag === 'Unavailable') throw provisional.error
  return provisional.value
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
    assert.strictEqual(
      MirNormalization.normalize(normalizedProgram, provisionalOf(normalized)),
      normalizedProgram,
    )
    const rawEvaluation = Analysis.evaluate(raw)
    const normalizedEvaluation = Analysis.evaluate(normalized)
    assert.strictEqual(rawEvaluation._tag, 'Completed')
    assert.strictEqual(normalizedEvaluation._tag, 'Completed')
    if (rawEvaluation._tag === 'Completed' && normalizedEvaluation._tag === 'Completed')
      assert.deepEqual(rawEvaluation.result, normalizedEvaluation.result)
  }),
)

it.effect('retains concrete suspendable runs without a global suspension mode', () =>
  Effect.gen(function* () {
    const raw = yield* Analysis.ofSourceRealized(
      'test/mir-normalization-suspendable',
      encoder.encode(`effect fn delayed(value: i32) -> i32 ! OutOfMemory ? &mut Allocator {
  return run Effect.suspend(effect { return value })
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  let selected = delayed(42) |> Effect.provideMut(&mut allocator)
  return run Effect.catchAll(move selected, recover)
}`),
      'wasm32-unknown-unknown',
      { normalizeMir: false },
    )
    assert.deepEqual(Analysis.diagnostics(raw), [])
    const program = Analysis.loweredMir(raw)
    const provisional = provisionalOf(raw)
    const rejected = MirNormalization.normalize(program, provisional)
    assert.strictEqual(Mir.encode(rejected).includes('run-static-effect'), false)
    assert.isTrue(
      (rejected.normalization ?? []).some(
        (verdict) => verdict._tag === 'Rejected' && verdict.reason === 'SuspendableRunner',
      ),
    )
    assert.include(ProvisionalMir.encode(provisional), 'relay=existing')
    assert.deepEqual(Mir.verify(rejected), [])
  }),
)

it.effect('retains only the exact execution whose runner fact is unknown', () =>
  Effect.gen(function* () {
    const raw = yield* snapshot(false)
    const program = Analysis.loweredMir(raw)
    const provisional = provisionalOf(raw)
    const unknownMain: ProvisionalMir.Module = Object.freeze({
      ...provisional,
      executions: Object.freeze(
        provisional.executions.map((execution) =>
          execution.key._tag === 'InstanceExecution' &&
          execution.key.instance.declaration.name === 'main'
            ? Object.freeze({ ...execution, classification: 'Unknown' as const })
            : execution,
        ),
      ),
    })
    const retained = MirNormalization.normalize(program, unknownMain)
    assert.isFalse(
      mainOperations(retained).some((operation) => operation._tag === 'RunStaticEffect'),
    )
    assert.isTrue(
      (retained.normalization ?? []).some(
        (verdict) => verdict._tag === 'Rejected' && verdict.reason === 'SuspensionUnknown',
      ),
    )
    assert.isTrue(
      retained.functions
        .filter((fn) => fn.id.name !== 'main')
        .flatMap(Mir.operations)
        .some((operation) => operation._tag === 'RunStaticEffect'),
      Mir.encode(retained),
    )
  }),
)

it.effect('retains suspendable reification and effect-entry closure control', () =>
  Effect.gen(function* () {
    const reified = yield* Analysis.ofSourceRealized(
      'test/mir-normalization-reify-suspendable',
      encoder.encode(`effect fn seed(value: i32) -> i32 ! OutOfMemory ? &mut Allocator {
  return run Effect.suspend(effect { return value })
}
fn increment(value: i32) -> i32 { return value + 1 }
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  let pending = seed(41) |> Effect.map(increment) |> Effect.provideMut(&mut allocator)
  return run Effect.catchAll(move pending, recover)
}`),
      'wasm32-unknown-unknown',
    )
    const entry = yield* Analysis.ofSourceRealized(
      'test/mir-normalization-entry-suspendable',
      encoder.encode('pub effect fn main() -> () { return () }'),
      'wasm32-unknown-unknown',
      { normalizeMir: false },
    )
    assert.deepEqual(Analysis.diagnostics(reified), [])
    assert.deepEqual(Analysis.diagnostics(entry), [])
    const reifiedProgram = Analysis.loweredMir(reified)
    const rawEntryProgram = Analysis.loweredMir(entry)
    const closure = allOperations(rawEntryProgram).find(
      (operation): operation is Extract<Mir.Operation, { readonly _tag: 'CloseEffectEntry' }> =>
        operation._tag === 'CloseEffectEntry',
    )
    assert.isDefined(closure)
    if (closure === undefined) return
    const entryFacts = provisionalOf(entry)
    const suspendableEntryFacts: ProvisionalMir.Module = Object.freeze({
      ...entryFacts,
      executions: Object.freeze(
        entryFacts.executions.map((execution) =>
          execution.key._tag !== 'InstanceExecution' &&
          execution.key.runner.module === closure.runner.module &&
          execution.key.runner.name === closure.runner.name
            ? Object.freeze({ ...execution, classification: 'Suspendable' as const })
            : execution,
        ),
      ),
    })
    const entryProgram = MirNormalization.normalize(rawEntryProgram, suspendableEntryFacts)
    assert.isTrue(
      allOperations(reifiedProgram).some((operation) => operation._tag === 'ReifyEffect'),
      Mir.encode(reifiedProgram),
    )
    assert.isTrue(
      allOperations(entryProgram).some((operation) => operation._tag === 'CloseEffectEntry'),
      Mir.encode(entryProgram),
    )
    for (const program of [reifiedProgram, entryProgram]) {
      assert.isTrue(
        (program.normalization ?? []).some(
          (verdict) => verdict._tag === 'Rejected' && verdict.reason === 'SuspendableRunner',
        ),
        Mir.encode(program),
      )
    }
  }),
)

it.effect('retains a provider-specialized suspendable runner', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'test/mir-normalization-provided-suspendable',
      encoder.encode(`service Value {
  effect fn get() -> i32 ! OutOfMemory ? &Value | &mut Allocator
}
struct SuspendedValue { value: i32 }
effect fn get(self: &SuspendedValue) -> i32 ! OutOfMemory ? &mut Allocator {
  return run Effect.suspend(effect { return self.value })
}
impl Value for SuspendedValue { get: SuspendedValue.get }
effect fn read() -> i32 ! OutOfMemory ? &Value | &mut Allocator { return run Value.get() }
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  let provider = SuspendedValue { value: 42 }
  let mut allocator = SystemAllocator.make()
  let selected = read() |> Effect.provide(&provider)
  let complete = move selected |> Effect.provideMut(&mut allocator)
  return run Effect.catchAll(move complete, recover)
}`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const program = Analysis.loweredMir(self)
    assert.isTrue(
      allOperations(program).some(
        (operation) =>
          (operation._tag === 'RunEffectValue' || operation._tag === 'ReifyEffect') &&
          operation.runner.name.includes('$provided$'),
      ),
      Mir.encode(program),
    )
    assert.isTrue(
      (program.normalization ?? []).some(
        (verdict) => verdict._tag === 'Rejected' && verdict.reason === 'SuspendableRunner',
      ),
      Mir.encode(program),
    )
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
    const program = MirNormalization.normalize(affineProgram, provisionalOf(raw))
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
