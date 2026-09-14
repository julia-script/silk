import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'
import * as MirVerification from '../src/MirVerification.js'
import * as RowAlgebra from '../src/RowAlgebra.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Type from '../src/Type.js'
import * as AnalysisFixture from './support/AnalysisFixture.js'
import {
  httpContentAnalysisPrelude,
  httpContentAcceptanceSource,
  httpContentPlanningSource,
} from './support/httpContentAcceptance.js'

const encoder = new TextEncoder()
const source = readFileSync(new URL('../stdlib/silk/http_content.silk', import.meta.url), 'utf8')
const reference = readFileSync(
  new URL('../../../apps/docs/content/reference/http-content-decoding.md', import.meta.url),
  'utf8',
)

const providedResultCollisionSource = `${httpContentAcceptanceSource}

effect fn providedResultCollision() -> i32 ! BufferError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut clock = FixedClock {}
  let matrix = run reviewRuntimeMatrix()
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if matrix != 0 { return matrix }
  return run typedCallbackFailureReleases()
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
}`

const contractRejectionSource = `${httpContentAnalysisPrelude}

fn forgeContext<'head, 'method, 'names>(
  head: ResponseHead<'head>,
  method: Method<'method>,
  policy: TrailerPolicy<'names>,
  selection: Selection<'head>,
  framing: Framing,
  anomaly: Anomaly,
) -> ResponseContext<'head, 'method, 'names> {
  return ResponseContext<'head, 'method, 'names> {
    head: head,
    requestMethod: method,
    trailerPolicy: policy,
    selection: move selection,
    framing: framing,
    anomaly: anomaly,
  }
}

fn forgeCompletion<'owner>(marker: &'owner u8) -> ContentCompletion<'owner> {
  return ContentCompletion<'owner> {
    kindValue: BodyCompletionKind.Delimited,
    decodedValue: 0,
    anomalyValue: Anomaly.None,
    reuseValue: ReuseDisposition.Reusable,
    marker: marker,
  }
}

effect<'call> fn mutateWithMetadataBorrowed<
  'call,
  'reader: 'call,
  'transport: 'reader,
  'head: 'reader,
>(
  reader: &'call mut ContentReader<'reader, 'transport, 'head, MemoryByteDuplex>,
) -> i32 {
  let metadata = ContentReader.representation(&reader.*)
  let mut output: [u8; 1] = [0]
  let mut clock = FixedClock {}
  let attempted = run Effect.result(
    ContentReader.readSome(&mut reader.*, &mut output, Option.none<Instant>())
      |> Effect.provideMut<MonotonicClock>(&mut clock)
  )
  drop metadata
  drop attempted
  return 0
}

effect<'session> fn reusePlan<
  'session,
  'head: 'session,
  'method: 'session,
  'names: 'session,
>(
  plan: CodingPlan<'head, 'method, 'names>,
  body: &'session mut BufferedDuplex<'session, MemoryByteDuplex>,
) -> () ? &mut Allocator {
  let first = run Effect.result(withReader<i32, never>(move plan, &mut body.*, analysisReader))
  drop first
  let second = run Effect.result(withReader<i32, never>(move plan, &mut body.*, analysisReader))
  drop second
  return ()
}

effect<'session> fn accessBodyWhileReaderOpen<
  'session,
  'head: 'session,
  'method: 'session,
  'names: 'session,
>(
  plan: CodingPlan<'head, 'method, 'names>,
  body: &'session mut BufferedDuplex<'session, MemoryByteDuplex>,
) -> () ? &mut Allocator {
  let opened = withReader<i32, never>(move plan, &mut body.*, analysisReader)
  let direct = BufferedDuplex.peek(&body.*)
  drop direct
  let result = run Effect.result(move opened)
  drop result
  return ()
}

effect<'session> fn concurrentReaders<
  'session,
  'firstHead: 'session,
  'firstMethod: 'session,
  'firstNames: 'session,
  'secondHead: 'session,
  'secondMethod: 'session,
  'secondNames: 'session,
>(
  firstPlan: CodingPlan<'firstHead, 'firstMethod, 'firstNames>,
  secondPlan: CodingPlan<'secondHead, 'secondMethod, 'secondNames>,
  body: &'session mut BufferedDuplex<'session, MemoryByteDuplex>,
) -> () ? &mut Allocator {
  let first = withReader<i32, never>(move firstPlan, &mut body.*, analysisReader)
  let second = withReader<i32, never>(move secondPlan, &mut body.*, analysisReader)
  let firstResult = run Effect.result(move first)
  drop firstResult
  let secondResult = run Effect.result(move second)
  drop secondResult
  return ()
}

effect<'call> fn leakReader<
  'call,
  'reader: 'call,
  'transport: 'reader,
  'head: 'reader,
>(
  reader: &'call mut ContentReader<'reader, 'transport, 'head, MemoryByteDuplex>,
) -> &'call mut ContentReader<'reader, 'transport, 'head, MemoryByteDuplex> {
  return move reader
}

effect<'session> fn escapeReader<
  'session,
  'head: 'session,
  'method: 'session,
  'names: 'session,
>(
  plan: CodingPlan<'head, 'method, 'names>,
  body: &'session mut BufferedDuplex<'session, MemoryByteDuplex>,
) -> &'session mut ContentReader<'session, 'session, 'head, MemoryByteDuplex>
! ContentError<'head> | OutOfMemoryError
? &mut Allocator {
  return run withReader<
    &'session mut ContentReader<'session, 'session, 'head, MemoryByteDuplex>,
    never,
  >(move plan, &mut body.*, leakReader)
}`

it.effect(
  'compiles the HTTP content-decoding reference example',
  () =>
    Effect.gen(function* () {
      const example = reference.match(/```silk\n([\s\S]*?)\n```/)?.[1]
      assert.isString(example)
      if (example === undefined) return
      const snapshot = yield* AnalysisFixture.retainingMain(
        'http-content/reference-example',
        encoder.encode(example),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    }),
  60_000,
)

it.effect(
  'realizes the public-import HTTP content acceptance program',
  () =>
    Effect.gen(function* () {
      const module = 'http-content/planning'
      const configuration = AnalysisFixture.configuration(module, 'x86_64-unknown-linux-gnu', [
        'planning',
      ])
      const snapshot = yield* Analysis.makeRealized({
        root: SourceFile.make(module, encoder.encode(httpContentPlanningSource)),
        configuration,
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    }),
  180_000,
)

it.effect(
  'keeps runtime borrows and provided recovery success shapes valid in one MIR graph',
  () =>
    Effect.gen(function* () {
      const module = 'http-content/provided-result-collision'
      const configuration = AnalysisFixture.configuration(module, 'x86_64-unknown-linux-gnu', [
        'portableProgram',
        'providedResultCollision',
      ])
      const snapshot = yield* Analysis.makeRealized({
        root: SourceFile.make(module, encoder.encode(providedResultCollisionSource)),
        configuration,
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const mir = Analysis.loweredMir(snapshot)
      assert.deepEqual(MirVerification.verify(mir), [])
      const contextualBuffered = mir.functions
        .flatMap((fn) => fn.localTypes)
        .find(
          (type): type is Extract<Mir.Type, { readonly _tag: 'EffectValue' }> =>
            type._tag === 'EffectValue' &&
            type.environment.instance.declaration.name === 'withBufferedCapacity' &&
            type.type.access === 'Exclusive' &&
            type.environment.effect.access === 'Take',
        )
      assert.isDefined(contextualBuffered)
      if (contextualBuffered === undefined) return
      assert.isTrue(
        Type.equals(contextualBuffered.type.success, contextualBuffered.environment.effect.success),
      )
      assert.isTrue(
        RowAlgebra.equals(
          Type.failureRowPolicy(),
          contextualBuffered.type.failureRow,
          contextualBuffered.environment.effect.failureRow,
        ),
      )
      assert.isTrue(
        RowAlgebra.equals(
          Type.requirementRowPolicy(),
          contextualBuffered.type.requirementRow,
          contextualBuffered.environment.effect.requirementRow,
        ),
      )
    }),
  300_000,
)

it.effect(
  'enforces response-context and reader evidence opacity in one analysis',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* AnalysisFixture.declarations(
        'http-content/contract-rejections',
        encoder.encode(contractRejectionSource),
      )
      const diagnostics = Analysis.diagnostics(snapshot)
      const owners = [
        'forgeContext',
        'forgeCompletion',
        'mutateWithMetadataBorrowed',
        'reusePlan',
        'accessBodyWhileReaderOpen',
        'concurrentReaders',
        'leakReader',
        'escapeReader',
      ] as const
      const starts = owners.map((name) => contractRejectionSource.indexOf(`fn ${name}`))
      const ownerAt = (offset: number): (typeof owners)[number] | undefined =>
        owners.findLast((_, index) => (starts.at(index) ?? Number.MAX_SAFE_INTEGER) <= offset)
      assert.deepEqual(
        diagnostics.map(({ code, span }) => ({ code, owner: ownerAt(span.start) })),
        [
          { code: 'SEM0021', owner: 'forgeContext' },
          { code: 'SEM0021', owner: 'forgeCompletion' },
          { code: 'OWN0010', owner: 'mutateWithMetadataBorrowed' },
          { code: 'OWN0001', owner: 'reusePlan' },
          { code: 'OWN0010', owner: 'accessBodyWhileReaderOpen' },
          { code: 'OWN0011', owner: 'accessBodyWhileReaderOpen' },
          { code: 'OWN0010', owner: 'concurrentReaders' },
          { code: 'SEM0076', owner: 'escapeReader' },
        ],
      )
      const spans = diagnostics.map(({ span }) =>
        contractRejectionSource.slice(span.start, span.end).trim(),
      )
      assert.include(spans.at(0) ?? '', 'ResponseContext')
      assert.include(spans.at(1) ?? '', 'ContentCompletion')
      assert.strictEqual(spans.at(2), '&mut reader.*')
      assert.strictEqual(spans.at(3), 'move plan')
    }),
  60_000,
)

it.effect(
  'realizes the ordinary-source HTTP content actor without compiler privilege',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* AnalysisFixture.declarations(
        'http-content/module',
        encoder.encode(source),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
    }),
  60_000,
)
