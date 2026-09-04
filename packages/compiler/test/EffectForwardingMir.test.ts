import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'

const encoder = new TextEncoder()

it.effect('terminates direct recursion discovered through a bound service implementation', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'effect-forwarding/direct-service-recursion',
      encoder.encode(`import silk.effect { Effect }
service Loop {
  effect fn next() -> i32 ? &mut Loop
}
struct Provider {}
effect fn next(self: &mut Provider) -> i32 ? &mut Loop {
  return run Loop.next()
}
impl Loop for Provider { next: Provider.next }
effect fn program() -> i32 ? &mut Loop {
  return run Loop.next()
}
pub fn main() -> i32 {
  let provider = Provider {}
  return run Effect.bindRequirementOwned<Loop>(program(), move provider)
}`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const implementations = Analysis.instancesOf(self).instances.filter(
      (instance) => instance.key.declaration.name === 'next',
    )
    assert.strictEqual(implementations.length, 1)
  }),
)

it.effect('retains provider targets through expression and ordinary statement arms', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'effect-forwarding/service-branches',
      encoder.encode(`import silk.effect { Effect }
service Choice {
  effect fn left() -> i32 ? &mut Choice
  effect fn right() -> i32 ? &mut Choice
}
struct Provider {}
effect fn left(self: &mut Provider) -> i32 { return 20 }
effect fn right(self: &mut Provider) -> i32 { return 22 }
impl Choice for Provider { left: Provider.left right: Provider.right }
struct First {}
struct Second {}
effect fn select(input: First | Second) -> i32 ? &mut Choice {
  return match move input {
    First {} => { return run Choice.left() }
    Second {} => run Choice.right()
  }
}
effect fn use(input: First | Second) -> i32 ? &mut Choice {
  return run select(move input)
}
pub fn main() -> i32 {
  let provider = Provider {}
  return run Effect.bindRequirementOwned<Choice>(use(Second {}), move provider)
}`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const providerTargets = Analysis.instancesOf(self).calls.filter(
      (call) =>
        call.owner.declaration.name === 'select' &&
        (call.target.declaration.name === 'left' || call.target.declaration.name === 'right'),
    )
    assert.deepEqual(providerTargets.map((call) => call.target.declaration.name).sort(), [
      'left',
      'right',
    ])
    const mir = Analysis.loweredMir(self)
    assert.deepEqual(MirVerification.verify(mir), [], MirEncoding.encode(mir))
  }),
)
