import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

interface Structure {
  readonly bytes: number
  readonly hash?: string
  readonly calls: number
  readonly branches: number
  readonly allocations: number
  readonly indirectCalls: number
  readonly suspensionTerms: number
  readonly effectTerms: number
}

interface RunnerClassification {
  readonly regions: ReadonlyArray<{ readonly tag: string; readonly count: number }>
  readonly outcomes: ReadonlyArray<{ readonly tag: string; readonly count: number }>
  readonly operations: ReadonlyArray<{ readonly tag: string; readonly count: number }>
  readonly nestedMatches: number
  readonly directCalls: number
  readonly dynamicCalls: number
  readonly effectOperations: number
  readonly loans: number
  readonly releases: number
  readonly cleanup: number
  readonly recursive: boolean
  readonly affineAccesses: number
  readonly estimatedClonedSize: number
  readonly blockers: ReadonlyArray<string>
  readonly prototypeEligible: boolean
}

interface CostCase {
  readonly id: string
  readonly pair: string
  readonly kind: 'baseline' | 'effect'
  readonly expected: number | 'trap'
  readonly behavior: {
    readonly evaluator: number | 'trap'
    readonly unnormalizedEvaluator: number | 'trap'
    readonly wasm: number | 'trap'
    readonly unnormalizedWasm: number | 'trap'
    readonly dropCalls: number
    readonly unnormalizedDropCalls: number
  }
  readonly applicability: 'DirectStaticRun' | 'ConstructorOnly' | 'None'
  readonly normalization: {
    readonly accepted: number
    readonly rejected: number
    readonly foldedConstructors: number
    readonly directStaticRuns: number
  }
  readonly runners: ReadonlyArray<RunnerClassification>
  readonly pipeTokens: { readonly hir: number; readonly mir: number }
  readonly mir: Structure
  readonly optimizedLlvm: Structure
  readonly optimizedLlvmEntry: Structure
  readonly assembly: Structure
  readonly assemblyEntry: Structure
  readonly wasm: Structure & { readonly binaryBytes: number }
  readonly wasmEntry: Structure
  readonly unnormalizedWasm: Structure & { readonly binaryBytes: number }
  readonly unnormalizedWasmEntry: Structure
}

interface CostReport {
  readonly schema: 1
  readonly clang: string
  readonly node: string
  readonly cases: ReadonlyArray<CostCase>
}

const fixture = fileURLToPath(new URL('./fixtures/synchronous-effect-cost.mjs', import.meta.url))
const run = () => spawnSync(process.execPath, [fixture], { encoding: 'utf8', maxBuffer: 8_000_000 })

it('captures synchronous Effect costs deterministically in fresh processes', () => {
  const first = run()
  const second = run()

  assert.strictEqual(first.status, 0, first.stderr)
  assert.strictEqual(second.status, 0, second.stderr)
  assert.strictEqual(first.stdout, second.stdout)

  const report = JSON.parse(first.stdout) as CostReport
  assert.strictEqual(report.schema, 1)
  assert.match(report.clang, /clang version/)
  assert.strictEqual(report.cases.length, 18)
  assert.deepEqual(
    new Set(report.cases.map((sample) => sample.pair)),
    new Set([
      'pure-pipe',
      'map',
      'map-both-success',
      'map-both-failure',
      'flat-map',
      'provide',
      'stored',
      'affine',
      'trap',
    ]),
  )

  for (const sample of report.cases) {
    assert.strictEqual(sample.behavior.evaluator, sample.expected, sample.id)
    assert.strictEqual(sample.behavior.unnormalizedEvaluator, sample.expected, sample.id)
    assert.strictEqual(sample.behavior.wasm, sample.expected, sample.id)
    assert.strictEqual(sample.behavior.unnormalizedWasm, sample.expected, sample.id)
    assert.strictEqual(sample.pipeTokens.hir, 0, sample.id)
    assert.strictEqual(sample.pipeTokens.mir, 0, sample.id)
    assert.strictEqual(sample.mir.suspensionTerms, 0, sample.id)
    assert.strictEqual(sample.optimizedLlvm.suspensionTerms, 0, sample.id)
    assert.strictEqual(sample.optimizedLlvmEntry.suspensionTerms, 0, sample.id)
    assert.strictEqual(sample.assembly.suspensionTerms, 0, sample.id)
    assert.strictEqual(sample.assemblyEntry.suspensionTerms, 0, sample.id)
    assert.strictEqual(sample.wasm.suspensionTerms, 0, sample.id)
    assert.strictEqual(sample.wasmEntry.suspensionTerms, 0, sample.id)
    assert.strictEqual(sample.unnormalizedWasm.suspensionTerms, 0, sample.id)
    assert.strictEqual(sample.unnormalizedWasmEntry.suspensionTerms, 0, sample.id)
    assert.isAbove(sample.wasm.binaryBytes, 0, sample.id)
    assert.isAbove(sample.unnormalizedWasm.binaryBytes, 0, sample.id)
    if (sample.applicability === 'DirectStaticRun') {
      assert.isAbove(sample.normalization.foldedConstructors, 0, sample.id)
      assert.isAbove(sample.normalization.directStaticRuns, 0, sample.id)
      assert.isBelow(sample.wasmEntry.calls, sample.unnormalizedWasmEntry.calls, sample.id)
      assert.strictEqual(sample.runners.length, sample.normalization.directStaticRuns, sample.id)
      assert.isAbove(sample.wasmEntry.calls, 0, sample.id)
    } else if (sample.applicability === 'ConstructorOnly') {
      assert.isAbove(sample.normalization.foldedConstructors, 0, sample.id)
      assert.strictEqual(sample.normalization.directStaticRuns, 0, sample.id)
      assert.isBelow(sample.wasmEntry.calls, sample.unnormalizedWasmEntry.calls, sample.id)
    } else {
      assert.strictEqual(sample.normalization.directStaticRuns, 0, sample.id)
      assert.deepEqual(sample.runners, [], sample.id)
    }
  }

  const runners = report.cases.flatMap((sample) => sample.runners)
  assert.strictEqual(runners.length, 13)
  assert.isTrue(runners.every((runner) => runner.estimatedClonedSize > 0))
  assert.isTrue(runners.every((runner) => !runner.prototypeEligible))
  assert.deepEqual(
    new Set(runners.flatMap((runner) => runner.blockers)),
    new Set([
      'AffineOperation',
      'Cleanup',
      'DynamicCallable',
      'Loan',
      'NestedEffectExecution',
      'NestedMatch',
      'StructuredRegion',
    ]),
  )
  assert.isTrue(
    runners.every(
      (runner) =>
        runner.regions.length > 0 && runner.outcomes.length > 0 && runner.operations.length > 0,
    ),
  )

  const affine = report.cases.filter((sample) => sample.pair === 'affine')
  assert.strictEqual(affine.length, 2)
  assert.isAbove(affine[0]?.behavior.dropCalls ?? 0, 0)
  assert.strictEqual(affine[0]?.behavior.dropCalls, affine[1]?.behavior.dropCalls)
  for (const sample of affine) {
    assert.strictEqual(sample.behavior.dropCalls, sample.behavior.unnormalizedDropCalls, sample.id)
  }

  for (const pair of new Set(report.cases.map((sample) => sample.pair))) {
    const samples = report.cases.filter((sample) => sample.pair === pair)
    const baseline = samples.find((sample) => sample.kind === 'baseline')
    const effect = samples.find((sample) => sample.kind === 'effect')
    assert.isDefined(baseline, pair)
    assert.isDefined(effect, pair)
    if (baseline === undefined || effect === undefined) continue
    assert.isAtLeast(effect.optimizedLlvm.allocations, 0, pair)
    assert.isAtLeast(effect.wasm.allocations, 0, pair)
  }
}, 30_000)
