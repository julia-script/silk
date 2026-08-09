import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const encoder = new TextEncoder()

it.effect('closes one module semantic input and its editor indexes into one artifact', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'tooling/Main',
      encoder.encode('pub fn main() -> i32 { let answer = 42 return answer }'),
    )
    const artifact = snapshot.toolingModules.get('tooling/Main')

    assert.isDefined(artifact)
    if (artifact === undefined) return
    assert.strictEqual(artifact.module, 'tooling/Main')
    assert.strictEqual(artifact.semantics, snapshot.semantics.get('tooling/Main'))
    assert.strictEqual(
      artifact.semanticOccurrences,
      snapshot.semanticOccurrences.modules.get('tooling/Main'),
    )
    assert.strictEqual(
      artifact.anonymousExpressions,
      snapshot.anonymousExpressions.get('tooling/Main'),
    )
    assert.isTrue(Object.isFrozen(artifact))
    assert.isTrue(Object.isFrozen(artifact.semanticOccurrences))
    assert.isTrue(Object.isFrozen(artifact.anonymousExpressions))
  }),
)
