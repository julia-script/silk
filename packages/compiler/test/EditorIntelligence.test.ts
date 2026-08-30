import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as NameResolution from '../src/NameResolution.js'
import * as Presentation from '../src/Presentation.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Type from '../src/Type.js'
import {
  allocatorSource,
  nestedBindingSource,
  recoveredMemberSource,
} from './support/editorCorpus.js'
import * as Projections from './support/projections.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

const documentationText = (
  snapshot: Analysis.Snapshot,
  block: ReturnType<typeof Analysis.documentationAt>,
) => {
  if (block === undefined) return undefined
  const source = Projections.syntaxOf(snapshot, block.span.sourceId)?.source
  if (source === undefined) return undefined
  return decoder.decode(SourceFile.toUint8Array(source).slice(block.span.start, block.span.end))
}

const occurrenceAt = (
  snapshot: Analysis.FrontendSnapshot,
  source: string,
  spelling: string,
  occurrence = 0,
) => {
  let offset = -1
  for (let index = 0; index <= occurrence; index += 1) offset = source.indexOf(spelling, offset + 1)
  return Analysis.semanticOccurrenceAt(snapshot, 'main', offset)
}

it.effect('indexes allocator tokens as source binding, actor, and function identities', () =>
  Analysis.ofSourceRealized('main', encoder.encode(allocatorSource)).pipe(
    Effect.map((snapshot) => {
      const source = new TextDecoder().decode(
        SourceFile.toUint8Array(Analysis.rootAnalysis(snapshot).syntax.source),
      )
      const binding = occurrenceAt(snapshot, source, 'allocator', 2)
      const actor = occurrenceAt(snapshot, source, 'Allocator', 2)
      const operation = occurrenceAt(snapshot, source, 'systemAllocatorProvider')
      assert.strictEqual(binding?.role, 'Declaration')
      assert.strictEqual(actor?.role, 'Actor')
      assert.strictEqual(operation?.role, 'Value')
      assert.isDefined(binding?.declaration)
      assert.strictEqual(actor?.resolution._tag, 'Available')
      assert.isDefined(operation?.declaration)
      assert.strictEqual(
        binding === undefined
          ? undefined
          : Analysis.occurrencePresentation(snapshot, 'main', binding)?.text,
        'let mut allocator: SystemAllocator',
      )
      assert.strictEqual(
        operation === undefined
          ? undefined
          : Analysis.occurrencePresentation(snapshot, 'main', operation)?.text,
        'pub fn systemAllocatorProvider() -> SystemAllocator',
      )
      return undefined
    }),
  ),
)

it.effect('presents and navigates source service declarations and operation contracts', () => {
  const source = `/// A portable logging contract.
pub service Logger {
  /// Reports whether logging is enabled.
  fn enabled() -> bool
}
pub fn main() -> i32 { return 0 }`
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const service = occurrenceAt(snapshot, source, 'Logger')
      const operation = occurrenceAt(snapshot, source, 'enabled()')

      assert.strictEqual(service?.role, 'Declaration')
      assert.strictEqual(operation?.role, 'Declaration')
      assert.isDefined(service?.declaration)
      assert.isDefined(operation?.declaration)
      assert.strictEqual(
        service === undefined
          ? undefined
          : Analysis.occurrencePresentation(snapshot, 'main', service)?.text,
        'pub service Logger',
      )
      assert.strictEqual(
        operation === undefined
          ? undefined
          : Analysis.occurrencePresentation(snapshot, 'main', operation)?.text,
        'fn enabled() -> bool',
      )
      assert.strictEqual(
        documentationText(
          snapshot,
          Analysis.documentationAt(snapshot, 'main', source.indexOf('enabled()')),
        ),
        '/// Reports whether logging is enabled.',
      )
      return undefined
    }),
  )
})

it.effect('presents nominal unions, variants, and variant fields from canonical facts', () => {
  const source = `/// A computation outcome.
pub union Result<A, E> {
  /// A successful payload.
  Success { pub value: A },
  Failure { pub error: E },
}
pub fn main() -> i32 { return 0 }`
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const declaration = occurrenceAt(snapshot, source, 'Result')
      const variant = occurrenceAt(snapshot, source, 'Success')
      const field = occurrenceAt(snapshot, source, 'value')
      const union = Analysis.unionByName(snapshot, 'main', 'Result')

      assert.strictEqual(declaration?.role, 'Declaration')
      assert.strictEqual(variant?.role, 'Declaration')
      assert.strictEqual(field?.role, 'Declaration')
      assert.strictEqual(variant?.resolution._tag, 'Available')
      assert.strictEqual(
        declaration === undefined
          ? undefined
          : Analysis.occurrencePresentation(snapshot, 'main', declaration)?.text,
        'pub union Result<A, E>',
      )
      assert.strictEqual(
        variant === undefined
          ? undefined
          : Analysis.occurrencePresentation(snapshot, 'main', variant)?.text,
        'Result<A, E>.Success { value: A }: Result<A, E>',
      )
      assert.strictEqual(
        documentationText(
          snapshot,
          Analysis.documentationAt(snapshot, 'main', source.indexOf('Success')),
        ),
        '/// A successful payload.',
      )
      assert.strictEqual(union._tag, 'Resolved')
      if (union._tag !== 'Resolved') return undefined
      const selected = Analysis.unionVariantByName(union.declaration, 'Success')
      assert.strictEqual(selected._tag, 'Resolved')
      if (selected._tag !== 'Resolved') return undefined
      assert.strictEqual(
        Analysis.unionVariantFieldByName(selected.variant, 'value')._tag,
        'Resolved',
      )
      return undefined
    }),
  )
})

it.effect('completes variants from a nominal union qualifier', () => {
  const source = `union State { Ready, Waiting { count: i32 } }
pub fn main() -> i32 { let state = State. return 0 }`
  return Analysis.ofSource('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const offset = source.indexOf('State.') + 'State.'.length
      const completion = Analysis.completionAt(snapshot, 'main', offset)
      assert.deepEqual(
        completion?.candidates.map((candidate) => [candidate.label, candidate.kind]),
        [
          ['Ready', 'Constructor'],
          ['Waiting', 'Constructor'],
        ],
      )
      return undefined
    }),
  )
})

it.effect('completes variants and module operations from a file-named nominal union', () => {
  const source = `import state { State }
pub fn main() -> i32 { let state = State. return 0 }`
  return Analysis.makeRealized({ root: SourceFile.make('main', encoder.encode(source)) }).pipe(
    Effect.provide(
      SourceResolver.memory(
        new Map([
          [
            'state',
            encoder.encode(
              'pub union State { Ready, Waiting { count: i32 } }\npub fn ready() -> State { return State.Ready }',
            ),
          ],
        ]),
      ),
    ),
    Effect.map((snapshot) => {
      const offset = source.indexOf('State.') + 'State.'.length
      const completion = Analysis.completionAt(snapshot, 'main', offset)
      assert.deepEqual(
        completion?.candidates.map((candidate) => [candidate.label, candidate.kind]),
        [
          ['Ready', 'Constructor'],
          ['Waiting', 'Constructor'],
          ['ready', 'Function'],
          ['State', 'Type'],
        ],
      )
      return undefined
    }),
  )
})

it.effect('navigates constructor and pattern variants through one canonical identity', () => {
  const source = `union Option<T> { Some { value: T }, None }
fn unwrap(option: Option<i32>) -> i32 {
  return match move option {
    Option<i32>.Some { value } => value
    Option<i32>.None => 0
  }
}
pub fn main() -> i32 { return unwrap(Option<i32>.Some { value: 42 }) }`
  return Analysis.ofSource('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const declaration = occurrenceAt(snapshot, source, 'Some')
      const pattern = occurrenceAt(snapshot, source, 'Some', 1)
      const construction = occurrenceAt(snapshot, source, 'Some', 2)

      assert.strictEqual(declaration?.role, 'Declaration')
      assert.strictEqual(pattern?.role, 'Value')
      assert.strictEqual(construction?.role, 'Value')
      assert.deepEqual(pattern?.resolution, declaration?.resolution)
      assert.deepEqual(construction?.resolution, declaration?.resolution)
      assert.deepEqual(pattern?.declaration, declaration?.declaration)
      assert.deepEqual(construction?.declaration, declaration?.declaration)
      return undefined
    }),
  )
})

it.effect('answers raw documentation for modules, declarations, children, and references', () => {
  const source = `//! Recovery module.
/// A recoverable problem.
pub struct Problem {
  /// Numeric problem code.
  pub code: i32
}
/// Recovers one problem.
pub effect fn recover(
  /// Problem to inspect.
  problem: Problem,
) -> i32 {
  return problem.code
}
pub fn main() -> i32 { return recover(Problem { code: 41 }) }
`
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      assert.strictEqual(
        documentationText(snapshot, Analysis.moduleDocumentation(snapshot, 'main')),
        '//! Recovery module.',
      )
      assert.strictEqual(
        documentationText(
          snapshot,
          Analysis.documentationAt(snapshot, 'main', source.indexOf('recover(')),
        ),
        '/// Recovers one problem.',
      )
      assert.strictEqual(
        documentationText(
          snapshot,
          Analysis.documentationAt(snapshot, 'main', source.lastIndexOf('recover')),
        ),
        '/// Recovers one problem.',
      )
      assert.strictEqual(
        documentationText(
          snapshot,
          Analysis.documentationAt(snapshot, 'main', source.indexOf('problem:')),
        ),
        '/// Problem to inspect.',
      )
      assert.strictEqual(
        documentationText(
          snapshot,
          Analysis.documentationAt(snapshot, 'main', source.indexOf('code:')),
        ),
        '/// Numeric problem code.',
      )
      return undefined
    }),
  )
})

it.effect('links public Effect operations to visible standard-library source', () => {
  const source = `import silk.effect as Effect
fn increment(value: i32) -> i32 { return value + 1 }
effect fn answer() -> i32 { return 41 }
pub effect fn main() -> i32 { return run answer() |> Effect.map(increment) }`
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const occurrence = occurrenceAt(snapshot, source, 'map')
      assert.strictEqual(occurrence?.role, 'Value')
      assert.strictEqual(occurrence?.declaration?.module, 'silk/effect')
      assert.strictEqual(
        documentationText(
          snapshot,
          Analysis.documentationAt(snapshot, 'main', source.indexOf('map')),
        ),
        `/// Applies a pure callback to success while preserving typed failure and requirements.
///
/// # Details
///
/// \`onSuccess\` runs once only after \`self\` succeeds. A typed failure propagates without invoking the
/// callback. Use [\`flatMap\`] when the callback itself needs an Effect.`,
      )
      assert.strictEqual(
        occurrence === undefined
          ? undefined
          : Analysis.occurrencePresentation(snapshot, 'main', occurrence)?.text,
        'pub effect fn map<A, B, E, ?R>(self: once Effect<A ! E ? R>, onSuccess: once fn(A) -> B) -> B ! E ? R',
      )
      return undefined
    }),
  )
})

it.effect(
  'links Scheduler and Fiber hovers to their canonical documented Silk declarations',
  () => {
    const source = `import silk.allocator as Allocator
import silk.fiber as Fiber
import silk.local_scheduler as LocalScheduler
import silk.monotonic_clock as MonotonicClock
import silk.scheduler as Scheduler

effect fn child() -> i32
? &mut Scheduler.Scheduler | &mut MonotonicClock.MonotonicClock {
  return 42
}

effect fn prepareOnly() -> Scheduler.PendingPublication<i32, never>
! Allocator.OutOfMemoryError | Scheduler.TaskIdExhaustedError
? &mut Scheduler.Scheduler | &mut MonotonicClock.MonotonicClock {
  return run Scheduler.prepare<i32, never>(child())
}

effect fn program() -> i32
! Allocator.OutOfMemoryError | Scheduler.TaskIdExhaustedError | Fiber.Cancelled
? &mut Scheduler.Scheduler | &mut MonotonicClock.MonotonicClock {
  let childFiber = run Fiber.forkChild<i32, never>(child())
  return run Fiber.join<i32, never>(move childFiber)
}

effect fn observe(fiber: Fiber.Fiber<i32, never>) -> Fiber.Outcome<i32, never> {
  return run Fiber.await<i32, never>(move fiber)
}

fn inspectErrors(
  exhausted: &Scheduler.TaskIdExhaustedError,
  cancelled: &Fiber.Cancelled,
  stalled: &LocalScheduler.StalledError,
) -> () { return () }

effect fn runProgram(scheduler: &mut LocalScheduler.LocalScheduler) -> i32
! Allocator.OutOfMemoryError
  | Scheduler.TaskIdExhaustedError
  | Fiber.Cancelled
  | LocalScheduler.StalledError
? &mut MonotonicClock.MonotonicClock {
  return run LocalScheduler.execute(move scheduler, program())
}

pub fn main() -> i32 {
  let scheduler = LocalScheduler.make()
  drop scheduler
  return 42
}`
    return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
      Effect.map((snapshot) => {
        assert.deepEqual(Analysis.diagnostics(snapshot), [])

        for (const [module, summary, declaration] of [
          [
            'silk/scheduler',
            '//! Provider protocol for preparing and atomically publishing child Fibers.',
            'pub service Scheduler',
          ],
          [
            'silk/fiber',
            '//! Affine Fiber handles, typed outcomes, and one-observer completion.',
            'pub struct Fiber<A, E>',
          ],
          [
            'silk/local_scheduler',
            '//! Deterministic single-threaded execution for structured Fibers.',
            'pub struct LocalScheduler',
          ],
        ] as const) {
          const moduleDocumentation = documentationText(
            snapshot,
            Analysis.moduleDocumentation(snapshot, module),
          )
          assert.isTrue(moduleDocumentation?.startsWith(summary), module)
          const canonical = Projections.syntaxOf(snapshot, module)?.source
          assert.isDefined(canonical, module)
          const canonicalText =
            canonical === undefined ? '' : decoder.decode(SourceFile.toUint8Array(canonical))
          assert.isTrue(canonicalText.startsWith(summary), module)
          assert.include(canonicalText, declaration, module)
        }

        for (const [needle, offset, module, presentation, summary] of [
          [
            'Scheduler.prepare<i32',
            'Scheduler.'.length,
            'silk/scheduler',
            'pub effect fn prepare<A, E>',
            '/// Prepares one lazy child task through the active Scheduler provider.',
          ],
          [
            'Scheduler.PendingPublication',
            'Scheduler.'.length,
            'silk/scheduler',
            'pub struct PendingPublication<A, E>',
            '/// A prepared child Fiber and the canonical data required for atomic publication.',
          ],
          [
            'Scheduler.TaskIdExhaustedError',
            'Scheduler.'.length,
            'silk/scheduler',
            'pub struct TaskIdExhaustedError',
            '/// A typed failure that reports exhaustion of the task identity space.',
          ],
          [
            'Fiber.forkChild',
            'Fiber.'.length,
            'silk/fiber',
            'pub effect fn forkChild<A, E>',
            '/// Prepares and atomically publishes one child task, then returns its affine Fiber.',
          ],
          [
            'Fiber.Outcome',
            'Fiber.'.length,
            'silk/fiber',
            'pub struct Outcome<A, E>',
            '/// The three possible terminal observations of a Fiber.',
          ],
          [
            'Fiber.Cancelled',
            'Fiber.'.length,
            'silk/fiber',
            'pub struct Cancelled',
            '/// A Fiber outcome that reports structured task cancellation.',
          ],
          [
            'LocalScheduler.execute',
            'LocalScheduler.'.length,
            'silk/local_scheduler',
            'pub effect fn execute<A, E>',
            '/// Runs one lazy root program under this Scheduler and returns its typed outcome.',
          ],
          [
            'LocalScheduler.StalledError',
            'LocalScheduler.'.length,
            'silk/local_scheduler',
            'pub struct StalledError',
            '/// Reports that no task is ready while the root is incomplete.',
          ],
        ] as const) {
          const position = source.indexOf(needle) + offset
          const occurrence = Analysis.semanticOccurrenceAt(snapshot, 'main', position)
          assert.strictEqual(occurrence?.resolution._tag, 'Available', needle)
          assert.strictEqual(occurrence?.declaration?.module, module, needle)
          assert.strictEqual(occurrence?.declaration?.selectionSpan?.sourceId, module, needle)
          assert.include(
            Analysis.hoverSubjectAt(snapshot, 'main', position)?.presentation.text ?? '',
            presentation,
            needle,
          )
          const documentation = documentationText(
            snapshot,
            Analysis.documentationAt(snapshot, 'main', position),
          )
          assert.isTrue(documentation?.startsWith(summary), needle)
        }
        return undefined
      }),
    )
  },
)

it.effect('navigates and presents the source-defined Vector lexical view accessors', () => {
  const source = `import silk.usize as usize
import silk.vector { make, asSlice, asMutSlice }
pub fn main() -> i32 {
  let mut values = make<i32>()
  let shared = asSlice<i32>(&values)
  let exclusive = asMutSlice<i32>(&mut values)
  return usize.toI32(shared.length + exclusive.length)
}`
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      for (const [name, documentation] of [
        [
          'asSlice',
          `/// Borrows the initialized elements as one shared lexical slice.
///
/// # Gotchas
///
/// Do not retain this slice across an operation that can grow the vector.`,
        ],
        [
          'asMutSlice',
          `/// Borrows all initialized elements as one exclusive lexical slice.
///
/// # Gotchas
///
/// Do not retain this slice across an operation that can grow the vector.`,
        ],
      ] as const) {
        const occurrence = occurrenceAt(snapshot, source, name, 1)
        assert.strictEqual(occurrence?.resolution._tag, 'Available')
        assert.strictEqual(occurrence?.declaration?.module, 'silk/vector')
        assert.isDefined(occurrence?.declaration?.selectionSpan)
        assert.include(
          occurrence === undefined
            ? ''
            : (Analysis.occurrencePresentation(snapshot, 'main', occurrence)?.text ?? ''),
          `fn ${name}<T>`,
        )
        assert.strictEqual(
          documentationText(
            snapshot,
            Analysis.documentationAt(snapshot, 'main', source.lastIndexOf(name)),
          ),
          documentation,
        )
      }
      const vectorSource = Projections.syntaxOf(snapshot, 'silk/vector')?.source
      assert.isDefined(vectorSource)
      assert.include(
        vectorSource === undefined ? '' : decoder.decode(SourceFile.toUint8Array(vectorSource)),
        'RawBuffer.view<T>',
      )
      const rawBufferSource = Projections.syntaxOf(snapshot, 'silk/raw_buffer')?.source
      assert.include(
        rawBufferSource === undefined
          ? ''
          : decoder.decode(SourceFile.toUint8Array(rawBufferSource)),
        'return Intrinsic.rawBufferView<T>',
      )
      return undefined
    }),
  )
})

it.effect('navigates and presents the source-defined owned Bytes surface', () => {
  const source = `import silk.bytes { Bytes }
import silk.usize as usize
import silk.bytes { Bytes as OwnedBytes, make, copy, append, length, asSlice, asMutSlice }
pub fn main() -> i32 {
  let bytes = Bytes.make()
  return usize.toI32(length(&bytes))
}`
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      for (const [name, documentation] of [
        ['make', '/// Creates an empty `Bytes` value without allocating storage.'],
        ['copy', '/// Copies a complete borrowed byte sequence into independently owned storage.'],
        [
          'append',
          `/// Appends a complete borrowed byte sequence in source order.
///
/// # Details
///
/// If growth fails, the original bytes and their length remain unchanged.`,
        ],
        ['length', '/// Returns the initialized byte count.'],
        ['asSlice', '/// Borrows all initialized octets as one shared lexical slice.'],
        [
          'asMutSlice',
          `/// Borrows all initialized octets as one exclusive lexical slice.
///
/// # Gotchas
///
/// Do not retain this slice across [\`append\`], because an append can replace the allocation.`,
        ],
      ] as const) {
        const occurrence = occurrenceAt(snapshot, source, name, 0)
        assert.strictEqual(occurrence?.resolution._tag, 'Available')
        assert.strictEqual(occurrence?.declaration?.module, 'silk/bytes')
        assert.isDefined(occurrence?.declaration?.selectionSpan)
        assert.include(
          occurrence === undefined
            ? ''
            : (Analysis.occurrencePresentation(snapshot, 'main', occurrence)?.text ?? ''),
          `fn ${name}`,
        )
        assert.strictEqual(
          documentationText(
            snapshot,
            Analysis.documentationAt(snapshot, 'main', source.indexOf(name)),
          ),
          documentation,
        )
      }
      const bytesSource = Projections.syntaxOf(snapshot, 'silk/bytes')?.source
      assert.isDefined(bytesSource)
      const text =
        bytesSource === undefined ? '' : decoder.decode(SourceFile.toUint8Array(bytesSource))
      assert.include(text, 'pub struct Bytes')
      assert.include(text, 'values: Vector<u8>')
      assert.notInclude(text, 'FileSystem')
      const completionOffset = source.indexOf('Bytes.') + 'Bytes.'.length
      const labels = Analysis.completionAt(snapshot, 'main', completionOffset)?.candidates.map(
        (candidate) => candidate.label,
      )
      for (const name of ['make', 'copy', 'append', 'length', 'asSlice', 'asMutSlice'])
        assert.include(labels ?? [], name)
      return undefined
    }),
  )
})

it.effect('completes and navigates the source-defined logging surface', () => {
  const source = `import silk.effect as Effect
import silk.logger { LogError }
import silk.logger { LogLevel }
import silk.logger { Logger }
effect fn pending() -> () ! LogError ? &mut Logger {
  return run Effect.logWarning("ready")
}
effect fn direct() -> () ! LogError ? &mut Logger {
  return run Logger.log(LogLevel.Info, "direct")
}
pub fn main() -> i32 {
  let memory = Logger.inMemoryProvider()
  let output = Logger.stdoutProvider()
  return 42
}`
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const logger = occurrenceAt(snapshot, source, 'Logger', 3)
      assert.strictEqual(logger?.role, 'Actor')
      assert.strictEqual(logger?.declaration?.module, 'silk/logger')

      for (const [spelling, module, ordinal] of [
        ['logWarning', 'silk/effect', 0],
        ['log(', 'silk/logger', 0],
        ['Info', 'silk/logger', 0],
        ['inMemoryProvider', 'silk/logger', 0],
        ['stdoutProvider', 'silk/logger', 0],
      ] as const) {
        const occurrence = occurrenceAt(snapshot, source, spelling, ordinal)
        assert.isDefined(occurrence, spelling)
        assert.strictEqual(occurrence?.declaration?.module, module, spelling)
        assert.isDefined(
          occurrence === undefined
            ? undefined
            : Analysis.occurrencePresentation(snapshot, 'main', occurrence),
          spelling,
        )
      }

      assert.strictEqual(
        documentationText(
          snapshot,
          Analysis.documentationAt(snapshot, 'main', source.indexOf('logWarning')),
        ),
        `/// Sends one complete message at \`LogLevel.Warning\` through the required mutable [\`Logger\`].`,
      )
      for (const [prefix, expected] of [
        ['Effect.', ['log', 'logAt', 'logTrace', 'logDebug', 'logInfo', 'logWarning', 'logError']],
        ['Logger.', ['log', 'LogLevel', 'inMemoryProvider', 'stdoutProvider', 'length', 'levelAt']],
      ] as const) {
        const offset = source.indexOf(prefix) + prefix.length
        const labels = Analysis.completionAt(snapshot, 'main', offset)?.candidates.map(
          (candidate) => candidate.label,
        )
        for (const label of expected) assert.include(labels ?? [], label, prefix)
      }
      return undefined
    }),
  )
})

it.effect('completes and navigates the portable Path and FileSystem surface', () => {
  const source = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.filesystem { FileError, FileSystem, Path, exists, resolve }
effect fn inspect(path: &Path) -> bool ! FileError ? &mut FileSystem {
  let info = run FileSystem.stat(path)
  return run exists(path)
}
effect fn locate(base: &Path) -> Path ! FileError | OutOfMemoryError ? &mut Allocator {
  return run resolve(base, "child")
}
effect fn canonical() -> Path ! OutOfMemoryError ? &mut Allocator { return run FileSystem.root() }
fn code(error: &FileError) -> i32 { return FileSystem.operationCode(error.operation) }
pub fn main() -> i32 { return 42 }`
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      for (const [spelling, ordinal, expected] of [
        ['stat', 0, ['effect fn stat(', '! FileError ? &mut FileSystem']],
        ['exists', 1, ['pub effect fn exists(', '-> bool ! FileError ? &mut FileSystem']],
        [
          'resolve',
          1,
          ['pub effect fn resolve(', '! FileError | OutOfMemoryError ? &mut Allocator'],
        ],
      ] as const) {
        const occurrence = occurrenceAt(snapshot, source, spelling, ordinal)
        assert.strictEqual(occurrence?.declaration?.module, 'silk/filesystem', spelling)
        const presentation =
          occurrence === undefined
            ? ''
            : (Analysis.occurrencePresentation(snapshot, 'main', occurrence)?.text ?? '')
        for (const fragment of expected) assert.include(presentation, fragment, spelling)
        assert.isDefined(occurrence?.declaration?.selectionSpan, spelling)
      }

      const fileError = occurrenceAt(snapshot, source, 'FileError', 1)
      assert.strictEqual(fileError?.declaration?.module, 'silk/filesystem')
      assert.strictEqual(
        documentationText(
          snapshot,
          Analysis.documentationAt(snapshot, 'main', source.indexOf('resolve(base')),
        ),
        `/// Resolves relative text lexically against an explicit absolute base.
///
/// # Details
///
/// Empty text and \`.\` keep the base; \`..\` removes components; ordinary components append. An
/// absolute relative value, an empty interior component, NUL, or any attempt to escape above root
/// fails with the \`InvalidPath\` reason. Resolution is lexical and never accesses the filesystem.`,
      )

      for (const [prefix, expected] of [
        [
          'FileSystem.',
          [
            'readFile',
            'writeFile',
            'stat',
            'listDirectory',
            'createDirectory',
            'removeFile',
            'removeDirectory',
            'make',
            'root',
            'join',
            'resolve',
            'isRoot',
            'name',
            'parent',
            'error',
            'errorWithCode',
            'providerCode',
          ],
        ],
      ] as const) {
        const offset = source.indexOf(prefix) + prefix.length
        const labels = Analysis.completionAt(snapshot, 'main', offset)?.candidates.map(
          (candidate) => candidate.label,
        )
        for (const label of expected) assert.include(labels ?? [], label, prefix)
      }
      return undefined
    }),
  )
})

it.effect('answers deterministic inferred hints and recovered completions', () =>
  Analysis.ofSourceRealized('main', encoder.encode(recoveredMemberSource)).pipe(
    Effect.map((snapshot) => {
      const source = new TextDecoder().decode(
        SourceFile.toUint8Array(Analysis.rootAnalysis(snapshot).syntax.source),
      )
      const hints = Analysis.typeHints(snapshot, 'main', 0, encoder.encode(source).length)
      assert.deepEqual(
        hints.map((hint) => hint.presentation.text),
        ['SystemAllocator'],
      )
      const offset = encoder.encode(
        source.slice(0, source.indexOf('Effect.') + 'Effect.'.length),
      ).length
      const first = Analysis.completionAt(snapshot, 'main', offset)
      const second = Analysis.completionAt(snapshot, 'main', offset)
      assert.deepEqual(second, first)
      assert.include(first?.candidates.map((candidate) => candidate.label) ?? [], 'catch')
      return undefined
    }),
  ),
)

it.effect('presents canonical string in hover, inlay hints, and semantic occurrences', () => {
  const source = `fn identity(value: string) -> string {
  let result = value
  return result
}`
  return Analysis.ofSource('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const occurrence = occurrenceAt(snapshot, source, 'string')
      const hover = Analysis.hoverSubjectAt(snapshot, 'main', source.indexOf('string'))
      const hints = Analysis.typeHints(snapshot, 'main', 0, encoder.encode(source).length)

      assert.strictEqual(occurrence?.role, 'Type')
      assert.deepEqual(occurrence?.resolution, {
        _tag: 'Available',
        identity: {
          _tag: 'IntrinsicActorIdentity',
          id: { _tag: 'IntrinsicActorId', name: 'string' },
        },
      })
      assert.strictEqual(hover?.presentation.text, 'intrinsic type string')
      assert.deepEqual(
        hints.map((hint) => hint.presentation.text),
        ['string'],
      )
      assert.strictEqual(
        Analysis.expressionsOf(snapshot, 'main').some(
          (expression) =>
            expression.type._tag === 'Available' && Type.isString(expression.type.type),
        ),
        true,
      )
      return undefined
    }),
  )
})

it.effect('publishes proved hover contracts and inferred provider-selector hints', () => {
  const source = `import silk.standard_streams as Streams
import silk.effect as Effect

fn inspect(value: Streams.NativeStandardStreams) -> () { return () }

pub effect fn main() -> () ! Streams.StreamWriteError {
  let mut streams = Streams.nativeStandardStreamProvider()
  return run Streams.send(Streams.stdout(), b"Hello, world!")
    |> Effect.provideMut(&mut streams)
}`
  return Analysis.ofSource('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const contractsAt = (spelling: string, occurrence = 0) => {
        let offset = -1
        for (let index = 0; index <= occurrence; index += 1)
          offset = source.indexOf(spelling, offset + 1)
        return (
          Analysis.hoverSubjectAt(snapshot, 'main', offset)?.implementedContracts.map(
            (contract) => contract.text,
          ) ?? []
        )
      }

      assert.deepEqual(contractsAt('NativeStandardStreams'), ['Streams.StandardStreams'])
      assert.deepEqual(contractsAt('nativeStandardStreamProvider'), ['Streams.StandardStreams'])
      assert.deepEqual(contractsAt('streams', 1), ['Streams.StandardStreams'])
      assert.deepEqual(
        Analysis.hoverSubjectAt(
          snapshot,
          'main',
          source.indexOf('nativeStandardStreamProvider()') + 'nativeStandardStreamProvider'.length,
        )?.implementedContracts.map((contract) => contract.text) ?? [],
        ['Streams.StandardStreams'],
      )

      const hints = Analysis.typeHints(snapshot, 'main', 0, encoder.encode(source).length)
      const selectors = hints.filter((hint) => hint._tag === 'ProviderSelectorTypeHint')
      assert.strictEqual(selectors.length, 1)
      assert.strictEqual(selectors.at(0)?.presentation.text, 'Streams.StandardStreams')
      assert.strictEqual(
        selectors.at(0)?.span.start,
        source.indexOf('provideMut(') + 'provideMut'.length,
      )
      const selectorOffset = source.indexOf('provideMut(') + 'provideMut'.length
      assert.deepEqual(
        Analysis.typeHints(snapshot, 'main', 0, selectorOffset).filter(
          (hint) => hint._tag === 'ProviderSelectorTypeHint',
        ),
        [],
      )
      assert.deepEqual(
        Analysis.typeHints(snapshot, 'main', selectorOffset, selectorOffset + 1)
          .filter((hint) => hint._tag === 'ProviderSelectorTypeHint')
          .map((hint) => hint.presentation.text),
        ['Streams.StandardStreams'],
      )
      assert.deepEqual(
        Analysis.typeHints(snapshot, 'main', 0, encoder.encode(source).length),
        hints,
      )
      return undefined
    }),
  )
})

it.effect('presents selected imports and shared and owned provider selectors', () => {
  const selectedImportSource = `import silk.standard_streams { StandardStreams }
import silk.effect as Effect

pub effect fn main() -> () ! StandardStreams.StreamWriteError {
  let mut streams = StandardStreams.nativeStandardStreamProvider()
  return run StandardStreams.send(StandardStreams.stdout(), b"selected\\n")
    |> Effect.provideMut(&mut streams)
}`
  const accessFormsSource = `import silk.effect as Effect

service Clock {}
struct FixedClock {}
impl Clock for FixedClock {}
effect fn read() -> i32 ? &Clock { return 42 }

pub fn main() -> i32 {
  let sharedClock = FixedClock {}
  let shared = read() |> Effect.provide(&sharedClock)
  drop shared
  let ownedClock = FixedClock {}
  let owned = read() |> Effect.bindRequirementOwned(move ownedClock)
  return run owned
}`
  return Effect.all([
    Analysis.ofSource('main', encoder.encode(selectedImportSource)),
    Analysis.ofSource('main', encoder.encode(accessFormsSource)),
  ]).pipe(
    Effect.map(([selectedImport, accessForms]) => {
      assert.deepEqual(Analysis.diagnostics(selectedImport), [])
      assert.deepEqual(
        Analysis.typeHints(selectedImport, 'main', 0, encoder.encode(selectedImportSource).length)
          .filter((hint) => hint._tag === 'ProviderSelectorTypeHint')
          .map((hint) => hint.presentation.text),
        ['StandardStreams'],
      )

      assert.deepEqual(Analysis.diagnostics(accessForms), [])
      assert.deepEqual(
        Analysis.typeHints(accessForms, 'main', 0, encoder.encode(accessFormsSource).length)
          .filter((hint) => hint._tag === 'ProviderSelectorTypeHint')
          .map((hint) => hint.presentation.text),
        ['Clock', 'Clock'],
      )
      return undefined
    }),
  )
})

it.effect('omits ambiguous and invalid selectors while preserving recovered facts', () => {
  const ambiguousSource = `import silk.effect as Effect
service Alpha {}
service Beta {}
struct Provider {}
impl Alpha for Provider {}
impl Beta for Provider {}
effect fn work() -> i32 ? &Alpha | &Beta { return 42 }
pub fn main() -> i32 {
  let provider = Provider {}
  let recipe = work() |> Effect.provide(&provider)
  return 0
}`
  const recoveredSource = `import silk.effect as Effect
service Clock {}
struct FixedClock {}
impl Clock for FixedClock {}
effect fn read() -> i32 ? &Clock { return 42 }
pub fn main() -> i32 {
  let clock = FixedClock {}
  let valid = read() |> Effect.provide(&clock)
  let invalid = read() |> Effect.provide(&missing)
  return Effect.
}`
  return Effect.all([
    Analysis.ofSource('main', encoder.encode(ambiguousSource)),
    Analysis.ofSource('main', encoder.encode(recoveredSource)),
  ]).pipe(
    Effect.map(([ambiguous, recovered]) => {
      assert.include(
        Analysis.diagnostics(ambiguous).map((diagnostic) => diagnostic.code),
        'SEM0125',
      )
      assert.deepEqual(
        Analysis.typeHints(ambiguous, 'main', 0, encoder.encode(ambiguousSource).length).filter(
          (hint) => hint._tag === 'ProviderSelectorTypeHint',
        ),
        [],
      )

      assert.isAbove(Analysis.diagnostics(recovered).length, 0)
      assert.deepEqual(
        Analysis.typeHints(recovered, 'main', 0, encoder.encode(recoveredSource).length)
          .filter((hint) => hint._tag === 'ProviderSelectorTypeHint')
          .map((hint) => hint.presentation.text),
        ['Clock'],
      )
      assert.deepEqual(
        Analysis.hoverSubjectAt(
          recovered,
          'main',
          recoveredSource.indexOf('FixedClock'),
        )?.implementedContracts.map((contract) => contract.text),
        ['Clock'],
      )
      return undefined
    }),
  )
})

it.effect('omits explicit provider selectors while retaining binding hints', () => {
  const source = `import silk.standard_streams as Streams
import silk.effect as Effect

pub effect fn main() -> () ! Streams.StreamWriteError {
  let mut streams = Streams.nativeStandardStreamProvider()
  return run Streams.send(Streams.stdout(), b"ok\\n")
    |> Effect.provideMut<Streams.StandardStreams>(&mut streams)
}`
  return Analysis.ofSource('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const hints = Analysis.typeHints(snapshot, 'main', 0, encoder.encode(source).length)
      assert.deepEqual(
        hints.map((hint) => hint._tag),
        ['BindingTypeHint'],
      )
      assert.strictEqual(hints.at(0)?.presentation.text, 'Streams.NativeStandardStreams')
      return undefined
    }),
  )
})

it.effect('sorts multiple hover contracts and proves conditional applications', () => {
  const source = `service Beta {}
service Alpha {}
struct Provider {}
impl Beta for Provider {}
impl Alpha for Provider {}

interface Eligible {}
service Wrapped {}
struct Good {}
struct Bad {}
impl Eligible for Good {}
struct Box<T> { value: T }
impl<T: Eligible> Wrapped for Box<T> {}

fn provider() -> Provider { return Provider {} }
fn good() -> Box<Good> { return Box<Good> { value: Good {} } }
fn bad() -> Box<Bad> { return Box<Bad> { value: Bad {} } }
pub fn main() -> i32 { return 0 }`
  return Analysis.ofSource('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const contractsAt = (spelling: string) =>
        Analysis.hoverSubjectAt(
          snapshot,
          'main',
          source.indexOf(spelling),
        )?.implementedContracts.map((contract) => contract.text) ?? []

      assert.deepEqual(contractsAt('Provider'), ['Alpha', 'Beta'])
      assert.deepEqual(contractsAt('provider()'), ['Alpha', 'Beta'])
      assert.deepEqual(contractsAt('good()'), ['Wrapped'])
      assert.deepEqual(contractsAt('bad()'), [])
      return undefined
    }),
  )
})

it.effect('qualifies same-spelled contracts and excludes invalid conformances', () => {
  const qualifiedSource = `import contracts as Other
service Contract {}
struct Provider {}
impl Contract for Provider {}
impl Other.Contract for Provider {}
pub fn main() -> Provider { return Provider {} }`
  const contracts = `pub service Contract {}`
  const invalidSource = `service Broken { fn value() -> i32 }
struct Provider {}
impl Broken for Provider {}
pub fn main() -> Provider { return Provider {} }`
  return Effect.all([
    Analysis.make({ root: SourceFile.make('main', encoder.encode(qualifiedSource)) }).pipe(
      Effect.provide(SourceResolver.memory(new Map([['contracts', encoder.encode(contracts)]]))),
    ),
    Analysis.ofSource('main', encoder.encode(invalidSource)),
  ]).pipe(
    Effect.map(([qualified, invalid]) => {
      assert.deepEqual(
        Analysis.hoverSubjectAt(
          qualified,
          'main',
          qualifiedSource.indexOf('Provider'),
        )?.implementedContracts.map((contract) => contract.text),
        ['Other.Contract', 'Contract'],
      )
      assert.isAbove(Analysis.diagnostics(invalid).length, 0)
      assert.deepEqual(
        Analysis.hoverSubjectAt(invalid, 'main', invalidSource.indexOf('Provider'))
          ?.implementedContracts,
        [],
      )
      return undefined
    }),
  )
})

it.effect('hints user-defined provider-selection combinators without name checks', () => {
  const source = `service Clock {}
struct FixedClock {}
impl Clock for FixedClock {}

effect fn read() -> i32 ? &mut Clock { return 42 }

effect fn bind<?S, A, P, E, ?R>(
  self: once Effect<A ! E ? R>,
  provider: &mut P
) -> A ! E ? Without<R, S>
where &mut P provides S from R {
  return run Intrinsic.bindRequirementMut<S>(move self, provider)
}

pub fn main() -> i32 {
  let mut clock = FixedClock {}
  return run bind(read(), &mut clock)
}`
  return Analysis.ofSource('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const hints = Analysis.typeHints(snapshot, 'main', 0, encoder.encode(source).length)
      const selector = hints.find((hint) => hint._tag === 'ProviderSelectorTypeHint')
      assert.strictEqual(selector?.presentation.text, 'Clock')
      assert.strictEqual(selector?.span.start, source.indexOf('bind(read') + 'bind'.length)
      return undefined
    }),
  )
})

it.effect('excludes conformances whose provider or contract endpoint is private', () => {
  const root = `import lib as Lib
pub fn main() -> i32 {
  let provider = Lib.make()
  return 0
}`
  const privateContractLibrary = `service Hidden {}
pub struct Provider {}
impl Hidden for Provider {}
pub fn make() -> Provider { return Provider {} }`
  const privateProviderLibrary = `pub service Visible {}
struct Provider {}
impl Visible for Provider {}
pub fn make() -> Provider { return Provider {} }`
  const analyze = Effect.fnUntraced(function* (library: string) {
    return yield* Analysis.make({ root: SourceFile.make('main', encoder.encode(root)) }).pipe(
      Effect.provide(SourceResolver.memory(new Map([['lib', encoder.encode(library)]]))),
    )
  })
  return Effect.all([analyze(privateContractLibrary), analyze(privateProviderLibrary)]).pipe(
    Effect.map((snapshots) => {
      for (const snapshot of snapshots) {
        assert.deepEqual(
          Analysis.hoverSubjectAt(snapshot, 'main', root.indexOf('make'))?.implementedContracts,
          [],
        )
        assert.deepEqual(
          Analysis.hoverSubjectAt(snapshot, 'main', root.indexOf('provider'))?.implementedContracts,
          [],
        )
      }
      return undefined
    }),
  )
})

it.effect(
  'retains exact semantic tokens for calls, constructors, initializers, and projections',
  () =>
    Analysis.ofSourceRealized(
      'main',
      encoder.encode(`import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
struct Pair { left: i32 }
fn pick() -> i32 {
  let pair = Pair { left: 1 }
  let allocator = Allocator.systemAllocatorProvider()
  return pair.left
}`),
    ).pipe(
      Effect.map((snapshot) => {
        const expressions = Analysis.expressionsOf(snapshot, 'main')
        const call = expressions.find((expression) => expression._tag === 'Call')
        const literal = expressions.find((expression) => expression._tag === 'StructLiteral')
        const projection = expressions.find((expression) => expression._tag === 'FieldProjection')
        assert.strictEqual(call?._tag === 'Call' ? call.path._tag : undefined, 'ReferencePath')
        assert.isTrue(
          call?._tag === 'Call' &&
            call.path._tag === 'ReferencePath' &&
            call.path.qualifier !== undefined &&
            call.path.qualifier.span.end <= call.path.member.span.start,
        )
        assert.isDefined(
          literal?._tag === 'StructLiteral' && literal.target._tag === 'Resolved'
            ? literal.target.token
            : undefined,
        )
        assert.isDefined(
          literal?._tag === 'StructLiteral' ? literal.initializers.at(0)?.token : undefined,
        )
        assert.isDefined(projection?._tag === 'FieldProjection' ? projection.fieldToken : undefined)
        return undefined
      }),
    ),
)

it.effect('recursively indexes generic nominal and type-parameter references', () =>
  Analysis.ofSourceRealized(
    'main',
    encoder.encode(`struct Problem {}
struct Box<T> { value: T }
fn unwrap(box: Box<Problem>) -> Problem { return box.value }
pub fn main() -> i32 { return 0 }`),
  ).pipe(
    Effect.map((snapshot) => {
      const source = new TextDecoder().decode(
        SourceFile.toUint8Array(Analysis.rootAnalysis(snapshot).syntax.source),
      )
      const typeParameterUse = occurrenceAt(snapshot, source, 'T', 1)
      const appliedTarget = occurrenceAt(snapshot, source, 'Box', 1)
      const appliedArgument = occurrenceAt(snapshot, source, 'Problem', 1)
      assert.strictEqual(typeParameterUse?.role, 'Type')
      assert.strictEqual(appliedTarget?.role, 'Type')
      assert.strictEqual(appliedArgument?.role, 'Type')
      assert.isDefined(typeParameterUse?.declaration)
      assert.isDefined(appliedTarget?.declaration)
      assert.isDefined(appliedArgument?.declaration)
      return undefined
    }),
  ),
)

it.effect('indexes every row-expression and constraint binder reference', () => {
  const source = `effect fn bind<?S, P, ?R>(
  self: once Effect<i32 ? R>,
  provider: &mut P
) -> i32 ? Without<R, S>
where &mut P provides S from R {
  return run self
}`
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const occurrences = snapshot.semanticOccurrences.modules.get('main')?.occurrences ?? []
      const spellings = occurrences.map((occurrence) =>
        decoder.decode(encoder.encode(source).slice(occurrence.span.start, occurrence.span.end)),
      )
      const count = (spelling: string): number =>
        spellings.filter((candidate) => candidate === spelling).length

      assert.strictEqual(count('S'), 3)
      assert.strictEqual(count('P'), 3)
      assert.strictEqual(count('R'), 4)
      for (const spelling of ['S', 'P', 'R']) {
        const references = occurrences.filter(
          (occurrence, ordinal) =>
            spellings.at(ordinal) === spelling && occurrence.role !== 'Declaration',
        )
        assert.isTrue(references.length > 0)
        assert.isTrue(references.every((occurrence) => occurrence.role === 'Type'))
        assert.isTrue(references.every((occurrence) => occurrence.declaration !== undefined))
      }
      return undefined
    }),
  )
})

it.effect('completes from the innermost lexical scope and excludes later declarations', () => {
  const source = nestedBindingSource
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const analysis = Analysis.rootAnalysis(snapshot)
      const scopes = analysis.lexicalScopes
      assert.isTrue(scopes.some((scope) => scope.parent !== undefined))

      const nestedOffset = encoder.encode(source.slice(0, source.indexOf('val\n') + 3)).length
      const nested = Analysis.completionAt(snapshot, 'main', nestedOffset)
      const values =
        nested?.candidates.filter((candidate) => candidate.label === 'value') ?? Object.freeze([])
      assert.strictEqual(values.length, 1)
      const innerBinding = analysis.functions
        .at(0)
        ?.bindings.filter(
          (binding) => binding.name._tag === 'Present' && binding.name.spelling === 'value',
        )
        .at(-1)
      const identity = values.at(0)?.identity
      assert.strictEqual(
        identity?._tag === 'SemanticCandidate' && identity.identity._tag === 'BindingIdentity'
          ? identity.identity.id.ordinal
          : undefined,
        innerBinding?.id.ordinal,
      )

      const beforeOffset = encoder.encode(source.slice(0, source.indexOf('lat\n') + 3)).length
      const before = Analysis.completionAt(snapshot, 'main', beforeOffset)
      assert.notInclude(before?.candidates.map((candidate) => candidate.label) ?? [], 'later')
      return undefined
    }),
  )
})

it.effect('includes match pattern bindings only inside their arm scope', () => {
  const source = `struct Full { value: i32 }
pub fn main() -> i32 {
  let full = Full { value: 1 }
  return match move full {
    Full { value } => val
  }
}`
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const offset = encoder.encode(source.slice(0, source.lastIndexOf('val') + 3)).length
      const completion = Analysis.completionAt(snapshot, 'main', offset)
      const value = completion?.candidates.find((candidate) => candidate.label === 'value')
      assert.strictEqual(
        value?.identity._tag === 'SemanticCandidate' ? value.identity.identity._tag : undefined,
        'PatternBindingIdentity',
      )
      return undefined
    }),
  )
})

it.effect('scopes let-pattern and if-let bindings for completion and navigation', () => {
  const source = `struct Full { value: i32 }
pub fn main() -> i32 {
  let first = Full { value: 1 }
  let Full { value } = move first
  let copied = val
  let second = Full { value: 2 }
  if let Full inner = &second { let taken = inn } else { let missed = inn }
  return value
}`
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const atCompletion = (prefix: string) =>
        Analysis.completionAt(snapshot, 'main', source.indexOf(prefix) + prefix.length)
      assert.include(
        atCompletion('let copied = val')?.candidates.map((candidate) => candidate.label) ?? [],
        'value',
      )
      assert.include(
        atCompletion('let taken = inn')?.candidates.map((candidate) => candidate.label) ?? [],
        'inner',
      )
      assert.notInclude(
        atCompletion('let missed = inn')?.candidates.map((candidate) => candidate.label) ?? [],
        'inner',
      )

      const declarationOffset = source.indexOf('value }')
      const useOffset = source.lastIndexOf('value')
      const declaration = Analysis.semanticOccurrenceAt(snapshot, 'main', declarationOffset)
      const use = Analysis.semanticOccurrenceAt(snapshot, 'main', useOffset)
      assert.strictEqual(declaration?.role, 'Declaration')
      assert.strictEqual(use?.declaration?.selectionSpan.start, declaration?.span.start)
      assert.strictEqual(
        Analysis.hoverSubjectAt(snapshot, 'main', useOffset)?.presentation.text,
        'let value: i32',
      )
      assert.strictEqual(
        Projections.statementsOf(snapshot, 'main').filter(
          (statement) => statement._tag === 'BindStatement',
        ).length,
        5,
      )
      return undefined
    }),
  )
})

it.effect('retains exact import, alias, qualifier, and unavailable-member tokens', () => {
  const root = `import lib as Library { answer as read, hidden }
pub fn main() -> i32 { return read() }`
  const library = `pub fn answer() -> i32 { return 42 }
fn hidden() -> i32 { return 0 }`
  return Analysis.makeRealized({ root: SourceFile.make('root', encoder.encode(root)) }).pipe(
    Effect.provide(SourceResolver.memory(new Map([['lib', encoder.encode(library)]]))),
    Effect.map((snapshot) => {
      const at = (spelling: string, occurrence = 0) => {
        let offset = -1
        for (let index = 0; index <= occurrence; index += 1)
          offset = root.indexOf(spelling, offset + 1)
        return Analysis.semanticOccurrenceAt(snapshot, 'root', offset)
      }
      const alias = at('Library')
      const sourceMember = at('answer')
      const localMember = at('read')
      const use = at('read', 1)
      const unavailable = at('hidden')
      assert.strictEqual(alias?.role, 'Import')
      assert.strictEqual(alias?.declaration?.module, 'root')
      assert.strictEqual(sourceMember?.role, 'Import')
      assert.strictEqual(sourceMember?.declaration?.module, 'lib')
      assert.strictEqual(localMember?.role, 'Import')
      assert.strictEqual(localMember?.span.start, root.indexOf('read'))
      assert.strictEqual(use?.declaration?.module, 'lib')
      assert.strictEqual(unavailable?.resolution._tag, 'Unavailable')
      return undefined
    }),
  )
})

it.effect('keeps struct field tooling visibility-aware across modules', () => {
  const root = `import lib as Model { Secret, make }
fn invalid() -> i32 {
  let secret = Model.Secret { value: 1, key: 2 }
  return 0
}
pub fn main() -> i32 {
  let secret = Model.make(1)
  return secret.
}`
  const library = `pub struct Secret { pub value: i32 key: i32 }
pub fn make(value: i32) -> Secret { return Secret { value: value, key: 7 } }`
  return Analysis.makeRealized({ root: SourceFile.make('main', encoder.encode(root)) }).pipe(
    Effect.provide(SourceResolver.memory(new Map([['lib', encoder.encode(library)]]))),
    Effect.map((snapshot) => {
      const completionOffset = root.lastIndexOf('secret.') + 'secret.'.length
      const labels = Analysis.completionAt(snapshot, 'main', completionOffset)?.candidates.map(
        (candidate) => candidate.label,
      )
      assert.include(labels ?? [], 'value')
      assert.notInclude(labels ?? [], 'key')

      const privateInitializer = Analysis.semanticOccurrenceAt(
        snapshot,
        'main',
        root.indexOf('key:'),
      )
      assert.strictEqual(privateInitializer?.resolution._tag, 'Available')
      assert.strictEqual(privateInitializer?.declaration?.module, 'lib')
      assert.include(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        'SEM0021',
      )
      return undefined
    }),
  )
})

it.effect('renders inferred types through unambiguous imports and canonical fallbacks', () => {
  const root = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.box { Box }
import types.Models as Schema { Box as Selected }
struct Selected {}
struct Problem {}
pub fn main() -> i32 { return 0 }`
  const models = `pub struct Box<T> { value: T }
pub struct Other {}`
  return Analysis.makeRealized({ root: SourceFile.make('main', encoder.encode(root)) }).pipe(
    Effect.provide(SourceResolver.memory(new Map([['types/Models', encoder.encode(models)]]))),
    Effect.map((snapshot) => {
      const scope = NameResolution.scopeOf(snapshot.resolution, 'main')
      const box = Type.nominal('types/Models', 'Box', Object.freeze(['i32']))
      const other = Type.nominal('types/Models', 'Other')
      const problem = Type.nominal('main', 'Problem')
      const failureRow = Type.parameter({ module: 'main', name: 'transform' }, 0, 'E')
      const requirementRow = Type.parameter(
        { module: 'main', name: 'transform' },
        1,
        'R',
        'RequirementRow',
      )
      assert.strictEqual(Presentation.type(box, 'main', scope), 'Schema.Box<i32>')
      assert.strictEqual(Presentation.type(other, 'main', scope), 'Schema.Other')
      assert.strictEqual(Presentation.type(box, 'detached'), 'types/Models.Box<i32>')
      assert.strictEqual(
        Presentation.scopedNominal(Type.nominal('silk/allocator', 'Allocator'), 'main', scope).text,
        'Allocator',
      )
      assert.strictEqual(Presentation.scopedNominal(box, 'main', scope).text, 'Schema.Box<i32>')

      const effect = Type.effect(
        Type.reference('Exclusive', box),
        [problem, failureRow],
        'Shared',
        [
          Object.freeze({
            capability: Type.nominal('silk/allocator', 'Allocator'),
            role: 'Heap',
            access: 'Exclusive' as const,
          }),
        ],
        [requirementRow],
      )
      assert.strictEqual(
        Presentation.type(effect, 'main', scope),
        'Effect<&mut Schema.Box<i32> ! Problem | E ? &mut Allocator at Heap | R>',
      )
      assert.strictEqual(
        Presentation.genericArgument(Type.failureValue([problem]), 'main', scope),
        'Problem',
      )
      assert.strictEqual(
        Presentation.genericArgument(
          Type.requirementRowArgument([
            {
              capability: Type.nominal('silk/allocator', 'Allocator'),
              role: 'Heap',
              access: 'Exclusive',
            },
          ]),
          'main',
          scope,
        ),
        '? &mut Allocator at Heap',
      )
      const union = Type.union(
        Object.freeze([problem, Type.nominal('silk/allocator', 'OutOfMemoryError')]),
      )
      assert.strictEqual(
        union._tag === 'Normalized' ? Presentation.type(union.type, 'main', scope) : undefined,
        'Problem | OutOfMemoryError',
      )
      return undefined
    }),
  )
})

it.effect('keeps recovered Unicode-adjacent occurrence indexes compact and deterministic', () => {
  const source = `import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
// π🙂
struct Problem {}
pub fn main() -> i32 {
  let mut allocator = Allocator.systemAllocatorProvider()
  return 0
}
fn damaged( -> {`
  return Effect.gen(function* () {
    const first = yield* Analysis.ofSourceRealized('main', encoder.encode(source))
    const second = yield* Analysis.ofSourceRealized('main', encoder.encode(source))
    const firstIndex = first.semanticOccurrences.modules.get('main')
    const secondIndex = second.semanticOccurrences.modules.get('main')
    assert.deepEqual(secondIndex, firstIndex)
    assert.strictEqual(firstIndex?.prefixMaximumEnd.length, firstIndex?.occurrences.length)

    const byteOffset = encoder.encode(
      source.slice(0, source.indexOf('let mut allocator') + 'let mut '.length),
    ).length
    const occurrence = Analysis.semanticOccurrenceAt(first, 'main', byteOffset)
    assert.strictEqual(occurrence?.role, 'Declaration')
    assert.isDefined(occurrence?.declaration)
    assert.isUndefined(
      occurrence === undefined
        ? undefined
        : Analysis.semanticOccurrenceAt(first, 'main', occurrence.span.end),
    )
    return undefined
  })
})

it.effect('preserves ambiguous, missing, namespace, and type completion contexts', () =>
  Effect.gen(function* () {
    // Two bindings the module wrote itself still collide, and an ambiguous qualifier offers nothing.
    const ambiguousSource = `import silk.vector { Vector }
struct Vector {}
pub fn main() -> i32 { return Vector. }`
    const ambiguous = yield* Analysis.ofSourceRealized('main', encoder.encode(ambiguousSource))
    const ambiguousResult = Analysis.completionAt(
      ambiguous,
      'main',
      ambiguousSource.indexOf('return Vector.') + 'return Vector.'.length,
    )
    assert.deepEqual(ambiguousResult?.context, {
      _tag: 'ValueMemberContext',
      state: 'Ambiguous',
    })
    assert.deepEqual(ambiguousResult?.candidates, [])

    // A seeded standard-library namespace is a prelude, so a local declaration of the same spelling
    // takes it rather than colliding with it: the qualifier is the empty local struct, which has no
    // members to offer, and completion is unavailable rather than ambiguous.
    const shadowedSource = `struct SystemAllocator {}
pub fn main() -> i32 { return SystemAllocator. }`
    const shadowed = yield* Analysis.ofSourceRealized('main', encoder.encode(shadowedSource))
    const shadowedResult = Analysis.completionAt(
      shadowed,
      'main',
      shadowedSource.indexOf('SystemAllocator.') + 'SystemAllocator.'.length,
    )
    assert.deepEqual(shadowedResult?.context, {
      _tag: 'ValueMemberContext',
      state: 'Unavailable',
    })
    // The source is deliberately truncated after the dot, so it carries the parser's recovery
    // diagnostic; what matters is that no binding collision joins it.
    assert.notInclude(
      Analysis.diagnostics(shadowed).map((diagnostic) => diagnostic.code),
      'SEM0016',
    )

    const missingSource = `pub fn main() -> i32 { return Mystery. }`
    const missing = yield* Analysis.ofSourceRealized('main', encoder.encode(missingSource))
    assert.deepEqual(
      Analysis.completionAt(missing, 'main', missingSource.indexOf('Mystery.') + 'Mystery.'.length)
        ?.context,
      { _tag: 'ValueMemberContext', state: 'Missing' },
    )

    const namespaceSource = `import lib as Library
struct Local {}
pub fn main(value: i32) -> i32 { return Library. }`
    const namespace = yield* Analysis.makeRealized({
      root: SourceFile.make('main', encoder.encode(namespaceSource)),
    }).pipe(
      Effect.provide(
        SourceResolver.memory(
          new Map([
            [
              'lib',
              encoder.encode(
                'pub fn visible() -> i32 { return 1 }\nfn hidden() -> i32 { return 0 }',
              ),
            ],
          ]),
        ),
      ),
    )
    const namespaceResult = Analysis.completionAt(
      namespace,
      'main',
      namespaceSource.indexOf('Library.') + 'Library.'.length,
    )
    assert.deepEqual(namespaceResult?.context, { _tag: 'ActorMemberContext', actor: 'lib' })
    assert.include(namespaceResult?.candidates.map((candidate) => candidate.label) ?? [], 'visible')
    assert.notInclude(
      namespaceResult?.candidates.map((candidate) => candidate.label) ?? [],
      'hidden',
    )

    const serviceSource = `service LocalLogger { fn enabled() -> bool }
pub fn main() -> i32 { return LocalLogger. }`
    const serviceSnapshot = yield* Analysis.ofSourceRealized('main', encoder.encode(serviceSource))
    const serviceResult = Analysis.completionAt(
      serviceSnapshot,
      'main',
      serviceSource.indexOf('LocalLogger.') + 'LocalLogger.'.length,
    )
    assert.deepEqual(serviceResult?.context, {
      _tag: 'ActorMemberContext',
      actor: 'LocalLogger',
    })
    assert.deepEqual(
      serviceResult?.candidates.map((candidate) => candidate.label),
      ['enabled'],
    )

    const importedServiceSource = `import contracts { ContractLogger }
pub fn main() -> i32 { return ContractLogger. }`
    const importedService = yield* Analysis.makeRealized({
      root: SourceFile.make('main', encoder.encode(importedServiceSource)),
    }).pipe(
      Effect.provide(
        SourceResolver.memory(
          new Map([
            ['contracts', encoder.encode('pub service ContractLogger { fn enabled() -> bool }')],
          ]),
        ),
      ),
    )
    const importedServiceResult = Analysis.completionAt(
      importedService,
      'main',
      importedServiceSource.indexOf('ContractLogger.') + 'ContractLogger.'.length,
    )
    assert.deepEqual(importedServiceResult?.context, {
      _tag: 'ActorMemberContext',
      actor: 'ContractLogger',
    })
    assert.deepEqual(
      importedServiceResult?.candidates.map((candidate) => candidate.label),
      ['enabled'],
    )

    const typeSource = `struct Local {}
union LocalChoice { Empty }
service Logger { fn enabled() -> bool }
fn identity<T>(value: ) -> i32 { return 0 }`
    const typeSnapshot = yield* Analysis.ofSourceRealized('main', encoder.encode(typeSource))
    const typeResult = Analysis.completionAt(
      typeSnapshot,
      'main',
      typeSource.indexOf('value: ') + 'value: '.length,
    )
    assert.deepEqual(typeResult?.context, { _tag: 'DeclaredTypeContext' })
    assert.include(typeResult?.candidates.map((candidate) => candidate.label) ?? [], 'Local')
    assert.include(typeResult?.candidates.map((candidate) => candidate.label) ?? [], 'LocalChoice')
    assert.include(typeResult?.candidates.map((candidate) => candidate.label) ?? [], 'Logger')
    assert.include(typeResult?.candidates.map((candidate) => candidate.label) ?? [], 'f32')
    assert.include(typeResult?.candidates.map((candidate) => candidate.label) ?? [], 'f64')
    assert.include(typeResult?.candidates.map((candidate) => candidate.label) ?? [], 'string')
    assert.notInclude(typeResult?.candidates.map((candidate) => candidate.label) ?? [], 'true')

    const importedUnionSource = `import contracts { ContractChoice }
fn identity(value: ) -> i32 { return 0 }`
    const importedUnion = yield* Analysis.makeRealized({
      root: SourceFile.make('main', encoder.encode(importedUnionSource)),
    }).pipe(
      Effect.provide(
        SourceResolver.memory(
          new Map([['contracts', encoder.encode('pub union ContractChoice { Empty }')]]),
        ),
      ),
    )
    const importedUnionResult = Analysis.completionAt(
      importedUnion,
      'main',
      importedUnionSource.indexOf('value: ') + 'value: '.length,
    )
    assert.include(
      importedUnionResult?.candidates.map((candidate) => candidate.label) ?? [],
      'ContractChoice',
    )
  }),
)

it.effect('presents and completes the sealed suspension intrinsic with its exact rows', () => {
  const source = `pub fn main() -> i32 {
  let deferred = Intrinsic.suspendEffect(effect { return 42 })
  let pending = Intrinsic.
  return 42
}`
  return Analysis.ofSource('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const operationOffset = source.indexOf('suspendEffect')
      const hover = Analysis.hoverSubjectAt(snapshot, 'main', operationOffset)
      assert.strictEqual(
        hover?.presentation.text,
        'fn Intrinsic.suspendEffect<A, E, ?R>(deferred: once Effect<A ! E ? R>) -> Effect<A ! E ? R>',
      )

      const completionOffset = encoder.encode(
        source.slice(0, source.indexOf('Intrinsic.', operationOffset) + 'Intrinsic.'.length),
      ).length
      const completion = Analysis.completionAt(snapshot, 'main', completionOffset)
      const candidate = completion?.candidates.find(
        (candidate) => candidate.label === 'suspendEffect',
      )
      assert.strictEqual(
        candidate?.detail?.text,
        'fn Intrinsic.suspendEffect<A, E, ?R>(deferred: once Effect<A ! E ? R>) -> Effect<A ! E ? R>',
      )
      return undefined
    }),
  )
})
