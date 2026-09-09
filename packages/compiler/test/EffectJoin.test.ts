import * as AnalysisFixture from './support/AnalysisFixture.js'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Type from '../src/Type.js'
import * as ValueStorage from '../src/ValueStorage.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const nonFiniteSource = `struct First {}
struct Second {}
fn choose<'env, F: Effect<'env; i32>>(input: First | Second, operation: F) -> Effect<'env; i32> {
  return match move input {
    First {} => operation
    Second {} => effect { return 42 }
  }
}`

const divergingArmSource = `struct First {}
struct Second {}
fn diverge() -> never { return diverge() }
fn choose(input: First | Second) -> Effect<'static; i32> {
  return match move input {
    First {} => effect { return 42 }
    Second {} => diverge()
  }
}
pub fn main() -> i32 {
  return run choose(First {})
}`

const requirementSource = `import silk.effect { Effect }
service LeftClock { effect fn read() -> i32 ? &LeftClock }
service RightClock { effect fn read() -> i32 ? &RightClock }
struct Left { value: i32 }
struct Right { value: i32 }
effect fn readLeft(self: &Left) -> i32 { return self.value }
effect fn readRight(self: &Right) -> i32 { return self.value }
impl LeftClock for Left { read: Left.readLeft }
impl RightClock for Right { read: Right.readRight }
effect fn useLeft() -> i32 ? &LeftClock { return run LeftClock.read() }
effect fn useRight() -> i32 ? &RightClock { return run RightClock.read() }
struct First {}
struct Second {}
fn choose(input: First | Second) -> Effect<'static; i32 ? &LeftClock | &RightClock> {
  return match move input {
    First {} => useLeft()
    Second {} => useRight()
  }
}
pub fn main() -> i32 {
  let left = Left { value: 41 }
  let right = Right { value: 42 }
  let selected = choose(Second {})
    |> Effect.provide<LeftClock>(&left)
    |> Effect.provide<RightClock>(&right)
  return run selected
}`

const snapshotOf = (name: string, text: string) =>
  AnalysisFixture.retainingMain(name, ascii(text), 'wasm32-unknown-unknown')

it.effect('finalizes chosen capture environments through ordinary suspension controls', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf(
      'effect-join/finalized',
      `import silk.effect { Effect }
struct Problem { code: i32 }
struct OtherProblem { code: i32 }
effect fn success(value: i32) -> i32 { return run Effect.suspend(effect { return value }) }
effect fn failure(left: i32, right: i32) -> i32 ! Problem { fail Problem { code: left + right } }
effect fn other(value: i32) -> i32 ! OtherProblem { run Effect.suspend(effect { return () }) fail OtherProblem { code: value } }
fn choose(flag: i32) -> Effect<'static; i32 ! Problem | OtherProblem> {
  if flag == 0 { return success(42) }
  if flag == 1 { return failure(20, 22) }
  return other(42)
}
effect fn finish() -> () { return () }
effect fn delayedFinish(value: i32) -> () { let completed = run Effect.suspend(effect { return value }) return () }
fn chooseFinish(flag: bool) -> Effect<'static; ()> {
  if flag { return finish() }
  return delayedFinish(42)
}
effect fn recover(error: Problem | OtherProblem) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catchAll(Effect.ensuring(choose(2), chooseFinish(false)), recover) }`,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [],
    )
    const mir = Analysis.loweredMir(snapshot)
    assert.deepEqual(MirVerification.verify(mir), [])
    const composites = mir.layout.valueStorage.filter((view) => view.role === 'CompositeCarrier')
    assert.isNotEmpty(composites)
    assert.deepEqual(ValueStorage.verify(mir.layout), [])
    for (const view of composites) {
      assert.strictEqual(view._tag, 'ValueStorage')
      if (view._tag !== 'ValueStorage') continue
      assert.isDefined(view.stored)
      assert.notStrictEqual(view.key, view.stored?.key)
      assert.deepEqual(
        view.members.map((member) => member.tag),
        view.stored?.alternatives.map((alternative) => alternative.tag),
      )
      for (const alternative of view.stored?.alternatives ?? []) {
        for (const slot of alternative.slots)
          assert.isAtMost(slot.offset + slot.size, alternative.size)
        const missing = {
          ...mir.layout,
          entries: mir.layout.entries.filter((entry) => !Type.equals(entry.type, alternative.type)),
        }
        assert.isTrue(
          ValueStorage.plan(missing).some(
            (candidate) =>
              candidate.key === view.key && candidate._tag === 'UnavailableValueStorage',
          ),
        )
        assert.deepEqual(
          ValueStorage.verify(missing).map((violation) => violation.rule),
          ['InvalidValueStorage'],
        )
      }
    }
    const operations = mir.functions.flatMap(MirVerification.operations)
    assert.isTrue(operations.some((operation) => operation._tag === 'UnpackEffectComposite'))
    assert.isTrue(
      operations.some(
        (operation) =>
          operation._tag === 'PropagateEffectFailure' && operation.outcome !== undefined,
      ),
    )
    assert.isTrue(
      mir.functions.some((fn) =>
        fn.suspension?.regions.some(
          (region) =>
            region._tag === 'RunSuspendableEffectRegion' && region.operation._tag === 'CatchEffect',
        ),
      ),
    )
    yield* Analysis.codegen(snapshot, { mode: 'release' })
  }),
)

it.effect('diagnoses a join whose representation is not a closed finite set', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource('effect-join/non-finite', ascii(nonFiniteSource))
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0132'],
    )
  }),
)

it.effect('accepts a join between an Effect arm and a diverging never arm', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf('effect-join/diverging-arm', divergingArmSource)
    assert.notInclude(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'SEM0132',
    )
  }),
)

it.effect('normalizes both requirement rows and retains both provider targets', () =>
  Effect.gen(function* () {
    const module = 'effect-join/requirements'
    const snapshot = yield* snapshotOf(module, requirementSource)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const available = Analysis.expressionsOf(snapshot, module).flatMap((expression) =>
      expression.type._tag === 'Available' ? [Type.encode(expression.type.type)] : [],
    )
    assert.include(available, `Effect<'static; i32 ? &${module}.LeftClock | &${module}.RightClock>`)
    const targets = Analysis.instancesOf(snapshot)
      .calls.filter(
        (call) =>
          call.target.declaration.name === 'readLeft' ||
          call.target.declaration.name === 'readRight',
      )
      .map((call) => call.target.declaration.name)
      .sort()
    assert.deepEqual(targets, ['readLeft', 'readRight'])
  }),
)
