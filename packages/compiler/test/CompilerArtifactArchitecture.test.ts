import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
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
        JSON.stringify(snapshot, (_key, value) =>
          typeof value === 'bigint' ? value.toString() : value,
        ),
        'Arena',
      )
    }),
)
