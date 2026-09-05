import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Type from '../src/Type.js'

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
  Analysis.ofSourceRealized(name, ascii(text), 'wasm32-unknown-unknown')

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
