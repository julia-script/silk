import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

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
