import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ExecutionAffinity from '../src/ExecutionAffinity.js'
import * as MirVerification from '../src/MirVerification.js'
import * as ProvisionalMir from '../src/ProvisionalMir.js'
import * as SuspensionOwnership from '../src/SuspensionOwnership.js'
import * as Type from '../src/Type.js'

const source = `import silk.core as Core
import silk.effect as Effect
import silk.execution as Execution
struct Guard { value: i32 }
fn register(wake: Intrinsic.Wake) -> Guard {
  Intrinsic.wake(move wake)
  return Guard { value: 1 }
}
effect fn parked() -> () {
  return run Execution.park(register)
}
fn ready(state: &()) -> () { return () }
effect fn program() -> () ! Core.OutOfMemoryError {
  let mut allocator = Core.make()
  let execution = run Execution.make(parked(), (), ready)
    |> Effect.provideMut<Core.Allocator>(&mut allocator)
  drop execution
  return ()
}
effect fn recover(error: Core.OutOfMemoryError) -> () { return () }
pub fn main() -> () { return run Effect.catchAll(program(), recover) }`

it.effect('seals Wake and lowers ordinary-source park and wake through verified MIR', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'external-wake-parking/lowering',
      new TextEncoder().encode(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(snapshot.mir._tag, 'Available')
    assert.strictEqual(snapshot.layout._tag, 'Available')
    if (snapshot.mir._tag !== 'Available' || snapshot.layout._tag !== 'Available') return
    assert.deepEqual(MirVerification.verify(snapshot.mir.value), [])
    const operations = snapshot.mir.value.functions.flatMap(MirVerification.operations)
    assert.lengthOf(
      operations.filter((operation) => operation._tag === 'ExecutionWake'),
      1,
    )
    const parkOperations = operations.filter((operation) => operation._tag === 'ExecutionPark')
    assert.lengthOf(parkOperations, 1)
    const provisional = ProvisionalMir.build(
      snapshot.instances,
      snapshot.layout.value,
      snapshot.index,
    )
    const ownership = SuspensionOwnership.plan(snapshot.mir.value, provisional, snapshot.index)
    assert.deepEqual(ownership.violations, [])
    const package_ = ownership.executionPackages.find((plan) => plan.package.readinessStorage)
    assert.strictEqual(package_?.wakeControl, 'StableGenerationCell')
    assert.strictEqual(package_?.wakeAllocation, 'IndivisibleUntilFinalAuthority')
    const parkOperation = parkOperations.at(0)
    const parked = ownership.plans.find(
      (plan) =>
        parkOperation !== undefined && plan.span.start === parkOperation.provenance.span.start,
    )
    assert.isDefined(parked, SuspensionOwnership.encode(ownership))
    assert.lengthOf(parked?.success.releases ?? [], 1)
  }),
)

it.effect('assigns only the sealed nominal Wake the local-execution affinity seed', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'external-wake-parking/affinity',
      new TextEncoder().encode(`struct Wake {}
fn intrinsic(value: Intrinsic.Wake) -> () { drop value return () }
fn ordinary(value: Wake) -> () { drop value return () }
pub fn main() -> i32 { return 42 }`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const declarations = snapshot.index.modules.at(0)?.declarations ?? []
    const parameter = (name: string): Type.Type | undefined => {
      const declared = declarations
        .find(
          (declaration) =>
            declaration.name._tag === 'Present' && declaration.name.spelling === name,
        )
        ?.parameters.at(0)?.declaredType
      return declared?._tag === 'Resolved' ? declared.type : undefined
    }
    const intrinsic = parameter('intrinsic')
    const ordinary = parameter('ordinary')
    assert.isTrue(intrinsic !== undefined && Type.isWake(intrinsic))
    assert.strictEqual(
      intrinsic === undefined
        ? 'Unrestricted'
        : ExecutionAffinity.ofType(snapshot.index, intrinsic)._tag,
      'LocalExecution',
    )
    assert.strictEqual(
      ordinary === undefined ? 'Missing' : ExecutionAffinity.ofType(snapshot.index, ordinary)._tag,
      'Unrestricted',
    )
  }),
)

it.effect('rejects external parking at a complete entry without an explicit Execution owner', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'external-wake-parking/unowned-entry',
      new TextEncoder().encode(`import silk.execution as Execution
struct Guard {}
fn register(wake: Intrinsic.Wake) -> Guard { drop wake return Guard {} }
pub fn main() -> () { return run Execution.park(register) }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0140'],
    )
  }),
)
