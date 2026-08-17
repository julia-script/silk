import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

interface Structure {
  readonly bytes: number
  readonly hash?: string
  readonly calls: number
  readonly branches: number
  readonly allocations: number
  readonly allocationSites: ReadonlyArray<string>
  readonly indirectCalls: number
  readonly suspensionTerms: number
  readonly effectTerms: number
}

interface EntryStructure extends Structure {
  readonly abi?: {
    readonly parameters: ReadonlyArray<string>
    readonly results: ReadonlyArray<string>
  }
}

interface TagCount {
  readonly tag: string
  readonly count: number
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
  readonly suspendability: {
    readonly instances: ReadonlyArray<string>
    readonly executions: ReadonlyArray<string>
    readonly effects: ReadonlyArray<string>
  }
  readonly continuationDescriptors: number
  readonly pipeTokens: { readonly hir: number; readonly mir: number }
  readonly mirOperationTags: ReadonlyArray<TagCount>
  readonly suspensionOperationTags: ReadonlyArray<TagCount>
  readonly mir: Structure
  readonly optimizedLlvm: Structure
  readonly optimizedLlvmEntry: EntryStructure
  readonly assembly: Structure
  readonly assemblyEntry: Structure
  readonly wasm: Structure & { readonly binaryBytes: number }
  readonly wasmEntry: EntryStructure
  readonly unnormalizedWasm: Structure & { readonly binaryBytes: number }
  readonly unnormalizedWasmEntry: EntryStructure
  readonly symbols: {
    readonly native: ReadonlyArray<string>
    readonly wasm: ReadonlyArray<string>
  }
  readonly linkage: {
    readonly nativeRuntime: ReadonlyArray<string>
    readonly nativeDeclarations: ReadonlyArray<string>
    readonly wasmImports: ReadonlyArray<string>
    readonly unnormalizedWasmImports: ReadonlyArray<string>
  }
  readonly suspensionLinkage: ReadonlyArray<string>
}

interface CostReport {
  readonly schema: 3
  readonly clang: string
  readonly node: string
  readonly cases: ReadonlyArray<CostCase>
}

const fixture = fileURLToPath(new URL('./fixtures/synchronous-effect-cost.mjs', import.meta.url))
const run = () => spawnSync(process.execPath, [fixture], { encoding: 'utf8', maxBuffer: 8_000_000 })

it('captures synchronous Effect entry structure', () => {
  // One run: the structural verdicts below are the claim. Fresh-process artifact determinism is
  // the canary determinism gates' job, not one more double-spawn here.
  const first = run()

  assert.strictEqual(first.status, 0, first.stderr)

  const report = JSON.parse(first.stdout) as CostReport
  assert.strictEqual(report.schema, 3)
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
    // This is derived from the MIR object graph, so a suspension operation cannot hide behind an
    // encoder spelling change. The fixture's centralized reserved vocabulary classifies new tags.
    assert.isAbove(sample.mirOperationTags.length, 0, sample.id)
    assert.deepEqual(sample.suspensionOperationTags, [], sample.id)
    assert.deepEqual(sample.suspendability.instances, [], sample.id)
    assert.deepEqual(sample.suspendability.executions, [], sample.id)
    assert.deepEqual(sample.suspendability.effects, [], sample.id)
    assert.strictEqual(sample.continuationDescriptors, 0, sample.id)
    // These values come from backend symbol tables, native-runtime requirements, LLVM
    // declarations, and Wasm imports. An unreferenced but linked suspension component is covered.
    assert.deepEqual(sample.suspensionLinkage, [], sample.id)
    assert.strictEqual(sample.mir.suspensionTerms, 0, sample.id)
    assert.strictEqual(sample.optimizedLlvm.suspensionTerms, 0, sample.id)
    assert.strictEqual(sample.optimizedLlvmEntry.suspensionTerms, 0, sample.id)
    assert.strictEqual(sample.assembly.suspensionTerms, 0, sample.id)
    assert.strictEqual(sample.assemblyEntry.suspensionTerms, 0, sample.id)
    assert.strictEqual(sample.wasm.suspensionTerms, 0, sample.id)
    assert.strictEqual(sample.wasmEntry.suspensionTerms, 0, sample.id)
    assert.strictEqual(sample.unnormalizedWasm.suspensionTerms, 0, sample.id)
    assert.strictEqual(sample.unnormalizedWasmEntry.suspensionTerms, 0, sample.id)
    // A synchronous entry has no indirect dispatcher. Direct calls remain valid for ordinary
    // user code and Effect runners.
    assert.strictEqual(sample.optimizedLlvmEntry.indirectCalls, 0, sample.id)
    assert.strictEqual(sample.wasmEntry.indirectCalls, 0, sample.id)
    assert.strictEqual(sample.unnormalizedWasmEntry.indirectCalls, 0, sample.id)
    assert.isDefined(sample.optimizedLlvmEntry.abi, sample.id)
    assert.isDefined(sample.wasmEntry.abi, sample.id)
    assert.isDefined(sample.unnormalizedWasmEntry.abi, sample.id)
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
  assert.strictEqual(runners.length, 21)
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
    // Compare semantic entry properties with an imperative program that performs the same work.
    // This avoids platform-dependent instruction snapshots while proving that merely using a
    // non-suspending Effect adds no allocation path or mandatory complete-vs-pending branch.
    assert.isAtMost(
      effect.optimizedLlvmEntry.allocations,
      baseline.optimizedLlvmEntry.allocations,
      pair,
    )
    assert.isAtMost(effect.wasmEntry.allocations, baseline.wasmEntry.allocations, pair)
    assert.isAtMost(
      effect.unnormalizedWasmEntry.allocations,
      baseline.unnormalizedWasmEntry.allocations,
      pair,
    )
    assert.isAtMost(effect.optimizedLlvmEntry.branches, baseline.optimizedLlvmEntry.branches, pair)
    assert.isAtMost(effect.wasmEntry.branches, baseline.wasmEntry.branches, pair)
    assert.isAtMost(
      effect.unnormalizedWasmEntry.branches,
      baseline.unnormalizedWasmEntry.branches,
      pair,
    )
    // The exported entry keeps the same parameter/result ABI; no pending result is exposed.
    assert.deepEqual(effect.optimizedLlvmEntry.abi, baseline.optimizedLlvmEntry.abi, pair)
    assert.deepEqual(effect.wasmEntry.abi, baseline.wasmEntry.abi, pair)
    assert.deepEqual(effect.unnormalizedWasmEntry.abi, baseline.unnormalizedWasmEntry.abi, pair)
  }
}, 180_000)
