import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const encoder = new TextEncoder()

it.effect('closes elaboration and ownership into one index-independent module artifact', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'artifact/Main',
      encoder.encode('pub fn main() -> i32 { return 42 }'),
    )
    const artifact = snapshot.semantics.get('artifact/Main')

    assert.isDefined(artifact)
    if (artifact === undefined) return
    assert.strictEqual(artifact.module, 'artifact/Main')
    assert.strictEqual(artifact.elaboration, snapshot.results.get('artifact/Main'))
    assert.strictEqual(artifact.ownership, snapshot.ownership.get('artifact/Main'))
    assert.notProperty(artifact.elaboration, 'index')
    assert.isTrue(Object.isFrozen(artifact))
  }),
)
