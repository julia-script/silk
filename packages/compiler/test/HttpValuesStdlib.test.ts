import * as AnalysisFixture from './support/AnalysisFixture.js'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as CleanupPlan from '../src/CleanupPlan.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as MirVerification from '../src/MirVerification.js'
import * as SourceFile from '../src/SourceFile.js'
import * as Stdlib from '../src/Stdlib.js'
import * as Projections from './support/projections.js'
import { httpValuesAcceptanceSource } from './support/httpValuesAcceptance.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const diagnosticSummary = (snapshot: Analysis.Snapshot) =>
  Analysis.diagnostics(snapshot).map((diagnostic) => ({
    code: diagnostic.code,
    message: diagnostic.message,
    sourceId: diagnostic.span.sourceId,
    start: diagnostic.span.start,
  }))

const sourceSection = (source: string, start: string, end: string): string => {
  const startOffset = source.indexOf(start)
  assert.notStrictEqual(startOffset, -1, `missing section start: ${start}`)
  const endOffset = source.indexOf(end, startOffset + start.length)
  assert.notStrictEqual(endOffset, -1, `missing section end: ${end}`)
  return source.slice(startOffset, endOffset)
}

it.effect(
  'ships HTTP values as navigable ordinary source',
  () =>
    Effect.gen(function* () {
      const source = `import silk.http { Header, Method, RequestHead, ResponseHead, Status, Version }
import silk.http_headers { Headers, Limits, OwnedHeaders }
import silk.http_target { HttpAuthority, RequestTarget, TargetForm }
pub fn main() -> i32 { return 42 }`
      const snapshot = yield* AnalysisFixture.retainingMain('http-values/navigation', ascii(source))
      const diagnostics = diagnosticSummary(snapshot)
      assert.deepEqual(diagnostics.slice(0, 20), [])
      assert.strictEqual(diagnostics.length, 0)

      for (const module of ['silk/http', 'silk/http_headers', 'silk/http_target']) {
        const canonical = Projections.syntaxOf(snapshot, module)?.source
        assert.isDefined(canonical)
        const archived = Stdlib.find(module)
        assert.deepEqual(
          archived?.bytes,
          canonical === undefined ? undefined : SourceFile.toUint8Array(canonical),
        )
      }

      // Safe Silk cannot construct an addressable slice large enough to overflow usize, so retain
      // structural evidence that every aggregate-size path uses checked arithmetic and maps its
      // unrepresentable branch to the public SizeOverflow reason.
      const headersSource = Projections.syntaxOf(snapshot, 'silk/http_headers')?.source
      const headSource = Projections.syntaxOf(snapshot, 'silk/http')?.source
      const targetSource = Projections.syntaxOf(snapshot, 'silk/http_target')?.source
      assert.isDefined(headersSource)
      assert.isDefined(headSource)
      assert.isDefined(targetSource)
      const decode = (file: SourceFile.SourceFile | undefined): string =>
        file === undefined ? '' : new TextDecoder().decode(SourceFile.toUint8Array(file))
      const headersText = decode(headersSource)
      const headText = decode(headSource)
      const targetText = decode(targetSource)
      const checkedAdd = sourceSection(headersText, 'fn checkedAdd(', '\nfn copyBytes')
      assert.include(checkedAdd, 'usize.checkedAdd')
      assert.include(checkedAdd, 'sizeFailure(component)')
      assert.include(headersText, 'ValueReason.SizeOverflow')

      const headersMake = sourceSection(
        headersText,
        '  pub fn make(',
        '\n  /// Returns the number of fields',
      )
      assert.lengthOf(headersMake.match(/checkedAdd/g) ?? [], 2)
      const headersCopy = sourceSection(
        headersText,
        '  pub effect fn copy(',
        '\n  /// Computes the payload-plus-index bytes',
      )
      assert.include(headersCopy, 'usize.checkedMultiply')
      assert.include(headersCopy, 'checkedAdd(')
      assert.include(headersCopy, 'sizeFailure(ValueComponent.Headers)')
      const requiredOwnedBytes = sourceSection(
        headersText,
        '  pub fn requiredOwnedBytes(',
        '\n  /// Formats only ordered field lines',
      )
      assert.include(requiredOwnedBytes, 'usize.checkedMultiply')
      assert.include(requiredOwnedBytes, 'checkedAdd(')
      assert.include(requiredOwnedBytes, 'sizeFailure(ValueComponent.Headers)')
      const formatInto = sourceSection(
        headersText,
        '  pub fn formatInto(',
        '\n  /// Iterates Connection field tokens',
      )
      assert.lengthOf(formatInto.match(/checkedAdd/g) ?? [], 3)

      const headOwnedBytes = sourceSection(headText, 'fn headOwnedBytes(', '\nfn absurd')
      assert.lengthOf(headOwnedBytes.match(/usize\.checkedAdd/g) ?? [], 3)
      assert.lengthOf(headOwnedBytes.match(/sizeFailure\(/g) ?? [], 3)
      const fromUri = sourceSection(
        targetText,
        '  pub fn fromUri(',
        '\n/// Validates Host cardinality',
      )
      assert.lengthOf(fromUri.match(/usize\.checkedAdd/g) ?? [], 2)
      assert.lengthOf(fromUri.match(/sizeFailure\(/g) ?? [], 2)

      assert.isFalse(
        Intrinsic.all().some((actor) =>
          [
            'Header',
            'Headers',
            'Method',
            'RequestHead',
            'RequestTarget',
            'ResponseHead',
            'Status',
          ].includes(actor.spelling),
        ),
      )

      const forgedSource = `import silk.http_target { RequestTarget }
pub fn main() -> i32 {
  let target = RequestTarget.Origin { text: "bad target" }
  return 0
}`
      const forged = yield* AnalysisFixture.retainingMain(
        'http-values/forged-target',
        ascii(forgedSource),
      )
      assert.deepEqual(
        Analysis.diagnostics(forged)
          .filter((diagnostic) => diagnostic.span.sourceId === 'http-values/forged-target')
          .map((diagnostic) => ({
            code: diagnostic.code,
            sourceId: diagnostic.span.sourceId,
            start: diagnostic.span.start,
          })),
        [{ code: 'SEM0021', sourceId: 'http-values/forged-target', start: 79 }],
      )
    }),
  60_000,
)

it.effect(
  'analyzes the shared HTTP value acceptance program without diagnostics',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* AnalysisFixture.retainingMain(
        'http-values/acceptance',
        ascii(httpValuesAcceptanceSource),
      )
      const diagnostics = diagnosticSummary(snapshot)
      assert.deepEqual(diagnostics.slice(0, 20), [])
      assert.strictEqual(diagnostics.length, 0)
    }),
  60_000,
)

it.effect(
  'rejects HTTP owner-derived views, iterators, and yielded fields that escape',
  () =>
    Effect.gen(function* () {
      const source = `import silk.http { Header, OwnedRequestHead, OwnedResponseHead, RequestHead, ResponseHead }
import silk.http_headers { HeaderIterator, Headers, OwnedHeaders }
import silk.option { Option }
fn headersView(owner: OwnedHeaders) -> Headers<'static> {
  return OwnedHeaders.view(&owner)
}
fn headerIterator(owner: OwnedHeaders) -> HeaderIterator<'static> {
  let view = OwnedHeaders.view(&owner)
  return Headers.iter(&view)
}
fn yieldedHeader(owner: OwnedHeaders) -> Option<Header<'static>> {
  let view = OwnedHeaders.view(&owner)
  let mut iterator = Headers.iter(&view)
  return HeaderIterator.next(&mut iterator)
}
fn requestView(owner: OwnedRequestHead) -> RequestHead<'static> {
  return OwnedRequestHead.view(&owner)
}
fn responseView(owner: OwnedResponseHead) -> ResponseHead<'static> {
  return OwnedResponseHead.view(&owner)
}
pub fn main() -> i32 { return 0 }`
      const snapshot = yield* AnalysisFixture.retainingMain(
        'http-values/ownership-escapes',
        ascii(source),
      )
      const diagnostics = Analysis.diagnostics(snapshot)
      assert.isAtLeast(diagnostics.length, 5)
      assert.isTrue(
        diagnostics.every((diagnostic) => ['OWN0019', 'SEM0212'].includes(diagnostic.code)),
      )
      assert.includeMembers(
        diagnostics.map((diagnostic) => diagnostic.code),
        ['OWN0019', 'SEM0212'],
      )
    }),
  60_000,
)

it.effect(
  'plans cleanup for partially constructed owned HTTP storage',
  () =>
    Effect.gen(function* () {
      const source = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.http { Header, ValueError }
import silk.http_headers { Headers, Limits, OwnedHeaders }
import silk.result { Result }
fn limits() -> Limits {
  return Limits { maxMethodBytes: 8, maxTargetBytes: 32, maxNameBytes: 16, maxValueBytes: 16, maxFields: 2, maxFieldBytes: 32, maxOwnedBytes: 256 }
}
effect fn copyMade<'value>(made: Result<Headers<'value>, ValueError>) -> ()
! OutOfMemoryError ? &mut Allocator {
  let headers = match move made {
    Result<Headers<'value>, ValueError>.Failure {error} => { return () }
    Result<Headers<'value>, ValueError>.Success {value} => value
  }
  let copied = run Headers.copy(&headers, limits())
  drop copied
  return ()
}
effect fn build() -> i32 ! OutOfMemoryError {
  let header = match move Header.make("X-Test", b"value", limits()) {
    Result<Header<'static>, ValueError>.Failure {error} => { return 0 }
    Result<Header<'static>, ValueError>.Success {value} => value
  }
  let entries = [header]
  let mut allocator = Allocator.systemAllocatorProvider()
  run copyMade(Headers.make(&entries, limits())) |> Effect.provideMut<Allocator>(&mut allocator)
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`
      const snapshot = yield* AnalysisFixture.retainingMain(
        'http-values/owned-cleanup',
        ascii(source),
      )
      assert.deepEqual(diagnosticSummary(snapshot), [])
      for (const { module, name, bindings } of [
        { module: 'silk/http_headers', name: 'Headers.copy', bindings: ['payload', 'records'] },
        { module: 'silk/http', name: 'RequestHead.copy', bindings: ['value', 'methodOwned'] },
        { module: 'silk/http', name: 'ResponseHead.copy', bindings: ['headers'] },
      ] as const) {
        const facts = Analysis.ownershipOf(snapshot, module)?.functions.find(
          (candidate) =>
            candidate.declaration.canonical._tag === 'Canonical' &&
            candidate.declaration.canonical.id.name === name,
        )
        const propagations = facts?.exits.filter((exit) => exit.kind === 'Propagation') ?? []
        assert.isNotEmpty(propagations, name)
        for (const exit of propagations) {
          const names = exit.releases.map((release) => release.binding.name)
          assert.strictEqual(new Set(names).size, names.length, `${name}: releases exactly once`)
        }
        for (const binding of bindings) {
          const releases = propagations.flatMap((exit) =>
            exit.releases.filter((release) => release.binding.name === binding),
          )
          assert.isNotEmpty(releases, `${name}: ${binding}`)
          assert.isTrue(
            releases.every((release) => CleanupPlan.reclaims(release.cleanup)),
            `${name}: ${binding}`,
          )
        }
      }
      const copy = Analysis.loweredMir(snapshot).functions.find(
        (fn) =>
          fn.id.module === 'silk/http_headers' && fn.id.name.startsWith('Headers.copy$effect$'),
      )
      assert.isDefined(copy)
      assert.isTrue(
        (copy === undefined ? [] : MirVerification.operations(copy)).some(
          (operation) => operation._tag === 'Drop' && CleanupPlan.reclaims(operation.cleanup),
        ),
      )
    }),
  60_000,
)
