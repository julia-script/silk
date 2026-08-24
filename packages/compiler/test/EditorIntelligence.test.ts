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
      const binding = occurrenceAt(snapshot, source, 'allocator')
      const actor = occurrenceAt(snapshot, source, 'SystemAllocator', 1)
      const operation = occurrenceAt(snapshot, source, 'make')
      assert.strictEqual(binding?.role, 'Declaration')
      assert.strictEqual(actor?.role, 'Actor')
      assert.strictEqual(operation?.role, 'Value')
      assert.isDefined(binding?.declaration)
      assert.strictEqual(actor?.resolution._tag, 'Available')
      assert.isDefined(actor?.declaration)
      assert.isDefined(operation?.declaration)
      assert.strictEqual(
        binding === undefined
          ? undefined
          : Analysis.occurrencePresentation(snapshot, 'main', binding)?.text,
        'let mut allocator: SystemAllocator',
      )
      assert.strictEqual(
        actor === undefined
          ? undefined
          : Analysis.occurrencePresentation(snapshot, 'main', actor)?.text,
        'pub struct SystemAllocator',
      )
      assert.strictEqual(
        operation === undefined
          ? undefined
          : Analysis.occurrencePresentation(snapshot, 'main', operation)?.text,
        'pub fn make() -> SystemAllocator',
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
import silk.logging { InMemoryLogger }
import silk.logging { LogError }
import silk.logging { LogLevel }
import silk.logging { StdoutLogger }
import silk.logging { Logger }
effect fn pending() -> () ! LogError ? &mut Logger {
  return run Effect.logAt(LogLevel.warning(), "ready")
}
effect fn direct() -> () ! LogError ? &mut Logger {
  return run Logger.log(LogLevel.info(), "direct")
}
pub fn main() -> i32 {
  let memory = InMemoryLogger.memory()
  let output = StdoutLogger.stdout()
  return 42
}`
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const logger = occurrenceAt(snapshot, source, 'Logger', 3)
      assert.strictEqual(logger?.role, 'Type')
      assert.strictEqual(logger?.declaration?.module, 'silk/logging')

      for (const [spelling, module, ordinal] of [
        ['logAt', 'silk/effect', 0],
        ['log(', 'silk/logging', 0],
        ['warning', 'silk/logging', 0],
        ['memory', 'silk/logging', 1],
        ['stdout', 'silk/logging', 0],
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
          Analysis.documentationAt(snapshot, 'main', source.indexOf('logAt')),
        ),
        `/// Sends one complete message at \`level\` through the required mutable [\`Logger\`].
///
/// # Details
///
/// The message is one logging event rather than a fragment. The provider controls formatting and
/// destination; its [\`LogError\`] propagates unchanged.`,
      )
      assert.strictEqual(
        documentationText(
          snapshot,
          Analysis.documentationAt(snapshot, 'main', source.indexOf('warning')),
        ),
        '/// Returns the Warning severity for a recoverable abnormal condition.',
      )

      for (const [prefix, expected] of [
        ['Effect.', ['log', 'logAt']],
        ['Logger.', ['log']],
        ['LogLevel.', ['trace', 'debug', 'info', 'warning', 'error']],
        ['InMemoryLogger.', ['memory', 'memoryFailAt', 'length', 'messageByteAt']],
        ['StdoutLogger.', ['stdout']],
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
  const source = `import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.filesystem { FileError, FileSystem, Path, exists, resolve }
effect fn inspect(path: &Path) -> bool ! FileError ? &mut FileSystem {
  let info = run FileSystem.stat(path)
  return run exists(path)
}
effect fn locate(base: &Path) -> Path ! FileError | OutOfMemoryError ? &mut Allocator {
  return run resolve(base, "child")
}
effect fn canonical() -> Path ! OutOfMemoryError ? &mut Allocator { return run Path.root() }
fn code(error: &FileError) -> i32 { return FileError.operationCode(error.operation) }
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
          ],
        ],
        [
          'Path.',
          ['make', 'root', 'join', 'joinUtf8', 'resolve', 'view', 'isRoot', 'name', 'parent'],
        ],
        ['FileError.', ['error', 'errorWithCode', 'providerCode']],
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

it.effect(
  'retains exact semantic tokens for calls, constructors, initializers, and projections',
  () =>
    Analysis.ofSourceRealized(
      'main',
      encoder.encode(`import silk.core { SystemAllocator }
struct Pair { left: i32 }
fn pick() -> i32 {
  let pair = Pair { left: 1 }
  let allocator = SystemAllocator.make()
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
  const root = `import silk.box { Box }
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

      const effect = Type.effect(
        Type.reference('Exclusive', box),
        [problem, failureRow],
        'Shared',
        [
          Object.freeze({
            capability: Type.nominal('silk/core', 'Allocator'),
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
              capability: Type.nominal('silk/core', 'Allocator'),
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
        Object.freeze([problem, Type.nominal('silk/core', 'OutOfMemoryError')]),
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
  const source = `import silk.core { SystemAllocator }
// π🙂
struct Problem {}
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
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

    const byteOffset = encoder.encode(source.slice(0, source.indexOf('allocator'))).length
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
    assert.include(typeResult?.candidates.map((candidate) => candidate.label) ?? [], 'Logger')
    assert.include(typeResult?.candidates.map((candidate) => candidate.label) ?? [], 'f32')
    assert.include(typeResult?.candidates.map((candidate) => candidate.label) ?? [], 'f64')
    assert.include(typeResult?.candidates.map((candidate) => candidate.label) ?? [], 'string')
    assert.notInclude(typeResult?.candidates.map((candidate) => candidate.label) ?? [], 'true')
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
