import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as CleanupPlan from '../src/CleanupPlan.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as MirVerification from '../src/MirVerification.js'
import * as SourceFile from '../src/SourceFile.js'
import * as Stdlib from '../src/Stdlib.js'
import * as Projections from './support/projections.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const diagnosticSummary = (snapshot: Analysis.Snapshot) =>
  Analysis.diagnostics(snapshot).map((diagnostic) => ({
    code: diagnostic.code,
    message: diagnostic.message,
    sourceId: diagnostic.span.sourceId,
    start: diagnostic.span.start,
  }))

it.effect('ships String as navigable ordinary source with private storage', () =>
  Effect.gen(function* () {
    const source = `import silk.string { String }
fn inspect(value: &String) -> string { return String.view(value) }
pub fn main() -> i32 { return 42 }`
    const snapshot = yield* Analysis.ofSourceRealized('string-stdlib/navigation', ascii(source))
    assert.deepEqual(diagnosticSummary(snapshot), [])

    const callOffset = source.lastIndexOf('view')
    const occurrence = Analysis.semanticOccurrenceAt(
      snapshot,
      'string-stdlib/navigation',
      callOffset,
    )
    assert.strictEqual(occurrence?.declaration?.module, 'silk/string')
    assert.isDefined(occurrence?.declaration?.selectionSpan)

    const canonical = Projections.syntaxOf(snapshot, 'silk/string')?.source
    assert.isDefined(canonical)
    const canonicalText =
      canonical === undefined ? '' : new TextDecoder().decode(SourceFile.toUint8Array(canonical))
    assert.include(canonicalText, 'pub struct String')
    assert.include(canonicalText, 'bytes: Bytes')
    assert.notInclude(canonicalText, 'pub bytes: Bytes')

    const archived = Stdlib.find('silk/string')
    assert.strictEqual(archived?.namespace, 'String')
    assert.deepEqual(
      archived?.bytes,
      canonical === undefined ? undefined : SourceFile.toUint8Array(canonical),
    )
    assert.isFalse(Intrinsic.all().some((actor) => actor.spelling === 'String'))

    const forged = yield* Analysis.ofSourceRealized(
      'string-stdlib/private-storage',
      ascii(`import silk.string { String }
fn expose(value: &String) -> usize { return value.bytes.values.length }
pub fn main() -> i32 { return 42 }`),
    )
    assert.include(
      Analysis.diagnostics(forged).map((diagnostic) => diagnostic.code),
      'SEM0028',
    )
  }),
)

it.effect('plans allocation cleanup for owned String storage', () =>
  Effect.gen(function* () {
    const module = 'string-stdlib/owned-cleanup'
    const snapshot = yield* Analysis.ofSourceRealized(
      module,
      ascii(`import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.string { String }
effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let value = run String.copy("silk") |> Effect.provideMut(&mut allocator)
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`),
    )
    assert.deepEqual(diagnosticSummary(snapshot), [])
    const build = Analysis.loweredMir(snapshot).functions.find((fn) =>
      fn.id.name.startsWith('build$effect$'),
    )
    assert.isDefined(build)
    const cleanup = build === undefined ? [] : MirVerification.operations(build)
    assert.isTrue(
      cleanup.some(
        (operation) => operation._tag === 'Drop' && CleanupPlan.reclaims(operation.cleanup),
      ),
    )
  }),
)
