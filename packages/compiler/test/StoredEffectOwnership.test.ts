import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import { unreachable } from './support/raise.js'

/**
 * Aggregate receiver access for Effect representations stored in nominal fields.
 *
 * This is an ownership-only contract: every concrete storage attempt remains fenced by `SEM0107`
 * until later slices prove the complete runtime realization in every engine.
 */

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const realized = Effect.fnUntraced(function* (name: string, source: string) {
  return yield* Analysis.ofSourceRealized(name, ascii(source), 'wasm32-unknown-unknown')
})

const codesOf = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)

const declarations = `struct Shared<F: Effect<i32>> { operation: F }
struct Exclusive<F: mut Effect<i32>> { operation: F }
struct Once<F: once Effect<i32>> { operation: F }
`

it.effect('runs a shared stored Effect through a shared aggregate borrow', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'stored-effect-ownership/shared-through-shared',
      `${declarations}fn runShared<F: Effect<i32>>(value: &Shared<F>) -> i32 {
  return run value.operation
}
pub fn main() -> i32 { return 0 }`,
    )

    assert.notInclude(codesOf(snapshot), 'OWN0015')
  }),
)

it.effect('rejects an exclusive stored Effect through a shared aggregate borrow', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'stored-effect-ownership/exclusive-through-shared',
      `${declarations}fn runExclusive<F: mut Effect<i32>>(value: &Exclusive<F>) -> i32 {
  return run value.operation
}
pub fn main() -> i32 { return 0 }`,
    )
    const diagnostic =
      Analysis.diagnostics(snapshot).find((candidate) => candidate.code === 'OWN0015') ??
      unreachable('expected a stored-Effect run access rejection')

    assert.strictEqual(diagnostic.reason._tag, 'StoredEffectRunAccess')
    if (diagnostic.reason._tag !== 'StoredEffectRunAccess') return
    assert.strictEqual(diagnostic.reason.receiver, 'Shared')
    assert.strictEqual(diagnostic.reason.required, 'Exclusive')
    assert.strictEqual(diagnostic.reason.field, '#0')
    assert.include(diagnostic.message, 'access to the whole aggregate')
  }),
)

it.effect('runs an exclusive stored Effect through an exclusive aggregate borrow', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'stored-effect-ownership/exclusive-through-exclusive',
      `${declarations}fn runExclusive<F: mut Effect<i32>>(value: &mut Exclusive<F>) -> i32 {
  return run value.operation
}
pub fn main() -> i32 { return 0 }`,
    )

    assert.notInclude(codesOf(snapshot), 'OWN0015')
  }),
)

it.effect('rejects a consuming stored Effect through shared and exclusive aggregate borrows', () =>
  Effect.gen(function* () {
    const shared = yield* realized(
      'stored-effect-ownership/consuming-through-shared',
      `${declarations}fn runOnce<F: once Effect<i32>>(value: &Once<F>) -> i32 {
  return run value.operation
}
pub fn main() -> i32 { return 0 }`,
    )
    const exclusive = yield* realized(
      'stored-effect-ownership/consuming-through-exclusive',
      `${declarations}fn runOnce<F: once Effect<i32>>(value: &mut Once<F>) -> i32 {
  return run value.operation
}
pub fn main() -> i32 { return 0 }`,
    )

    assert.deepEqual(
      [shared, exclusive].map((snapshot) => {
        const diagnostic = Analysis.diagnostics(snapshot).find(
          (candidate) => candidate.code === 'OWN0015',
        )
        return diagnostic?.reason._tag === 'StoredEffectRunAccess'
          ? [diagnostic.reason.receiver, diagnostic.reason.required]
          : undefined
      }),
      [
        ['Shared', 'Take'],
        ['Exclusive', 'Take'],
      ],
    )
  }),
)

it.effect('admits a consuming stored Effect only through whole-owner aggregate access', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'stored-effect-ownership/consuming-through-owner',
      `${declarations}fn runOnce<F: once Effect<i32>>(value: Once<F>) -> i32 {
  return run value.operation
}
pub fn main() -> i32 { return 0 }`,
    )

    assert.notInclude(codesOf(snapshot), 'OWN0015')
  }),
)
