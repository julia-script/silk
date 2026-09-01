import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const forbiddenArtifactKeys = new Set([
  'allocationScope',
  'destinationScope',
  'finalizerRecord',
  'finalizerRecords',
  'providerDependency',
  'providerDependencies',
  'allocatorKind',
  'runnerCallback',
  'pendingStep',
  'requirementContainer',
  'requirementsContainer',
  'runtimeRequirements',
])

const visit = (value: unknown, path: string, violations: Array<string>): void => {
  if (value === null || typeof value !== 'object') return
  if (Array.isArray(value)) {
    for (const [index, item] of value.entries()) visit(item, `${path}[${index}]`, violations)
    return
  }
  for (const [key, child] of Object.entries(value)) {
    if (forbiddenArtifactKeys.has(key)) violations.push(`${path}.${key}`)
    visit(child, `${path}.${key}`, violations)
  }
}

it.effect(
  'keeps allocation scopes finalizers provider dependencies and allocator kinds out of artifacts',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'architecture/artifacts',
        ascii(
          'fn consume(value: Allocation) -> i32 { return 0 }\npub fn main() -> i32 { return 42 }',
        ),
        'wasm32-unknown-unknown',
      )
      const violations: Array<string> = []
      visit(snapshot, 'snapshot', violations)
      assert.deepEqual(violations, [])
      assert.notInclude(
        Json.stringify(snapshot, (_key, value) =>
          typeof value === 'bigint' ? value.toString() : value,
        ),
        'Arena',
      )
    }),
)

it.effect('keeps synchronous Effect core artifacts free of concurrency runtime ABI', () =>
  Effect.gen(function* () {
    const source = `import silk.effect as Effect
import silk.result { Result }
service Clock { effect fn value() -> i32 ? &Clock }
struct FixedClock { value: i32 }
effect fn clockValue(self: &FixedClock) -> i32 { return self.value }
impl Clock for FixedClock { value: FixedClock.clockValue }
effect fn read() -> i32 ? &Clock { return 42 }
pub fn main() -> i32 {
  let clock = FixedClock { value: 42 }
  let closed = Intrinsic.bindRequirement(read(), &clock)
  let completed = run Effect.result(closed)
  return match move completed {
      Result<i32, never>.Success { value: answer } => answer
      Result<i32, never>.Failure { error: impossible } => 0
  }
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'architecture/effect-core',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const violations: Array<string> = []
    visit(snapshot, 'snapshot', violations)
    assert.deepEqual(violations, [])

    const llvm = yield* Analysis.codegen(snapshot, { mode: 'release' })
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    for (const artifact of [llvm.ir, wasm.wat]) {
      assert.notMatch(artifact, /silk_(?:scheduler|fiber|suspend|atomic)/i)
      assert.notInclude(artifact, 'complete-or-suspended')
      assert.notInclude(artifact, 'requirement_container')
    }
  }),
)
