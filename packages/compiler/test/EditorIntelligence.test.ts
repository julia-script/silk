import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as NameResolution from '../src/NameResolution.js'
import * as Presentation from '../src/Presentation.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Type from '../src/Type.js'
import {
  allocatorSource,
  effectHandlerSource,
  nestedBindingSource,
  recoveredMemberSource,
} from './support/editorCorpus.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

const documentationText = (
  snapshot: Analysis.Snapshot,
  block: ReturnType<typeof Analysis.documentationAt>,
) => {
  if (block === undefined) return undefined
  const source = Analysis.syntaxOf(snapshot, block.span.sourceId)?.source
  if (source === undefined) return undefined
  return decoder.decode(SourceFile.toUint8Array(source).slice(block.span.start, block.span.end))
}

const occurrenceAt = (
  snapshot: Analysis.Snapshot,
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
      const actor = occurrenceAt(snapshot, source, 'SystemAllocator')
      const operation = occurrenceAt(snapshot, source, 'make')
      assert.strictEqual(binding?.role, 'Declaration')
      assert.strictEqual(actor?.role, 'Actor')
      assert.strictEqual(operation?.role, 'Value')
      assert.isDefined(binding?.declaration)
      assert.isUndefined(actor?.declaration)
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
        'pub fn make() -> SystemAllocator',
      )
      return undefined
    }),
  ),
)

it.effect('presents effect function declarations and references identically', () =>
  Analysis.ofSourceRealized('main', encoder.encode(effectHandlerSource)).pipe(
    Effect.map((snapshot) => {
      const source = new TextDecoder().decode(
        SourceFile.toUint8Array(Analysis.rootAnalysis(snapshot).syntax.source),
      )
      const declaration = occurrenceAt(snapshot, source, 'recover')
      const reference = occurrenceAt(snapshot, source, 'recover', 1)
      assert.isDefined(declaration)
      assert.isDefined(reference)
      const declared =
        declaration === undefined
          ? undefined
          : Analysis.occurrencePresentation(snapshot, 'main', declaration)
      const referenced =
        reference === undefined
          ? undefined
          : Analysis.occurrencePresentation(snapshot, 'main', reference)
      assert.strictEqual(declared?.text, 'effect fn recover(error: OutOfMemory) -> i32')
      assert.deepEqual(referenced, declared)
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

it.effect('answers a canonical declaration document through a cross-module reference', () => {
  const root = `import lib { recover }
pub fn main() -> i32 { return recover(1) }`
  const library = `/// Library recovery.
pub fn recover(value: i32) -> i32 { return value }`
  return Analysis.makeRealized({ root: SourceFile.make('main', encoder.encode(root)) }).pipe(
    Effect.provide(SourceResolver.memory(new Map([['lib', encoder.encode(library)]]))),
    Effect.map((snapshot) => {
      assert.strictEqual(
        documentationText(
          snapshot,
          Analysis.documentationAt(snapshot, 'main', root.lastIndexOf('recover')),
        ),
        '/// Library recovery.',
      )
      return undefined
    }),
  )
})

it.effect('links public Effect operations to visible standard-library source', () => {
  const source = `fn increment(value: i32) -> i32 { return value + 1 }
effect fn answer() -> i32 { return 41 }
pub effect fn main() -> i32 { return run answer() |> Effect.map(increment) }`
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const occurrence = occurrenceAt(snapshot, source, 'map')
      assert.strictEqual(occurrence?.role, 'Value')
      assert.strictEqual(occurrence?.declaration?.module, 'silk/effects')
      assert.strictEqual(
        documentationText(
          snapshot,
          Analysis.documentationAt(snapshot, 'main', source.indexOf('map')),
        ),
        '/// Transforms the success channel of an Effect.',
      )
      assert.strictEqual(
        occurrence === undefined
          ? undefined
          : Analysis.occurrencePresentation(snapshot, 'main', occurrence)?.text,
        'pub effect fn map<A, B, !E, ?R>(self: once Effect<A ! E ? R>, onSuccess: once fn(A) -> B) -> B ! E ? R',
      )
      return undefined
    }),
  )
})

it.effect('indexes declaration and nominal type reference locations', () =>
  Analysis.ofSourceRealized(
    'main',
    encoder.encode(`struct Problem {}
fn recover(error: Problem) -> i32 { return 0 }
pub fn main() -> i32 { return recover(0) }`),
  ).pipe(
    Effect.map((snapshot) => {
      const source = new TextDecoder().decode(
        SourceFile.toUint8Array(Analysis.rootAnalysis(snapshot).syntax.source),
      )
      const declaration = occurrenceAt(snapshot, source, 'Problem')
      const typeReference = occurrenceAt(snapshot, source, 'Problem', 1)
      const functionReference = occurrenceAt(snapshot, source, 'recover', 1)
      assert.strictEqual(declaration?.role, 'Declaration')
      assert.strictEqual(typeReference?.role, 'Type')
      assert.strictEqual(typeReference?.declaration?.selectionSpan.start, source.indexOf('Problem'))
      assert.strictEqual(
        functionReference?.declaration?.selectionSpan.start,
        source.indexOf('recover'),
      )
      return undefined
    }),
  ),
)

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

it.effect(
  'retains exact semantic tokens for calls, constructors, initializers, and projections',
  () =>
    Analysis.ofSourceRealized(
      'main',
      encoder.encode(`struct Pair { left: i32 }
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

it.effect('renders inferred types through unambiguous imports and canonical fallbacks', () => {
  const root = `import types.Models as Schema { Box as Selected }
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
      const failureRow = Type.parameter({ module: 'main', name: 'transform' }, 0, 'E', 'FailureRow')
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
        Object.freeze([problem]),
        'Shared',
        Object.freeze([
          Object.freeze({
            capability: Type.allocator,
            role: 'Heap',
            access: 'Exclusive' as const,
          }),
        ]),
        [failureRow],
        [requirementRow],
      )
      assert.strictEqual(
        Presentation.type(effect, 'main', scope),
        'Effect<&mut Schema.Box<i32> ! Problem | E ? &mut Allocator@Heap | R>',
      )
      assert.strictEqual(
        Presentation.genericArgument(Type.failureRowArgument([problem]), 'main', scope),
        '! Problem',
      )
      assert.strictEqual(
        Presentation.genericArgument(
          Type.requirementRowArgument([
            { capability: Type.allocator, role: 'Heap', access: 'Exclusive' },
          ]),
          'main',
          scope,
        ),
        '? &mut Allocator@Heap',
      )
      const union = Type.union(Object.freeze([problem, Type.outOfMemory]))
      assert.strictEqual(
        union._tag === 'Normalized' ? Presentation.type(union.type, 'main', scope) : undefined,
        'Problem | OutOfMemory',
      )
      return undefined
    }),
  )
})

it.effect('keeps recovered Unicode-adjacent occurrence indexes compact and deterministic', () => {
  const source = `// π🙂
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

it.effect('indexes and completes source-backed Effect provision operations', () => {
  const source = `struct Clock {}
effect fn read() -> i32 ? &Clock { return 42 }
pub fn main() -> i32 {
  let clock = Clock {}
  let recipe = read() |> Effect.provide(&clock)
  return run recipe
}`
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const qualifier = occurrenceAt(snapshot, source, 'Effect')
      const operation = occurrenceAt(snapshot, source, 'provide')
      assert.strictEqual(qualifier?.role, 'Actor')
      assert.isUndefined(qualifier?.declaration)
      assert.strictEqual(operation?.role, 'Value')
      assert.strictEqual(operation?.declaration?.module, 'silk/effects')
      assert.include(
        operation === undefined
          ? ''
          : (Analysis.occurrencePresentation(snapshot, 'main', operation)?.text ?? ''),
        'effect fn provide',
      )

      const offset = source.lastIndexOf('Effect.provide') + 'Effect.'.length
      const completion = Analysis.completionAt(snapshot, 'main', offset)
      assert.include(completion?.candidates.map((candidate) => candidate.label) ?? [], 'provide')
      assert.include(
        completion?.candidates.map((candidate) => candidate.label) ?? [],
        'provideWith',
      )
      return undefined
    }),
  )
})

it.effect('links provideMut to visible standard-library source', () => {
  const source = `struct Clock {}
effect fn read() -> i32 ? &mut Clock { return 42 }
pub fn main() -> i32 {
  let mut clock = Clock {}
  let recipe = read() |> Effect.provideMut(&mut clock)
  return run recipe
}`
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const operation = occurrenceAt(snapshot, source, 'provideMut')
      assert.strictEqual(operation?.role, 'Value')
      assert.strictEqual(operation?.declaration?.module, 'silk/effects')
      assert.strictEqual(
        documentationText(
          snapshot,
          Analysis.documentationAt(snapshot, 'main', source.indexOf('provideMut')),
        ),
        '/// Satisfies one typed service requirement with an existing exclusive provider.',
      )
      assert.include(
        operation === undefined
          ? ''
          : (Analysis.occurrencePresentation(snapshot, 'main', operation)?.text ?? ''),
        'effect fn provideMut',
      )
      return undefined
    }),
  )
})

it.effect('preserves ambiguous, missing, namespace, and type completion contexts', () =>
  Effect.gen(function* () {
    const ambiguousSource = `struct SystemAllocator {}
pub fn main() -> i32 { return SystemAllocator. }`
    const ambiguous = yield* Analysis.ofSourceRealized('main', encoder.encode(ambiguousSource))
    const ambiguousResult = Analysis.completionAt(
      ambiguous,
      'main',
      ambiguousSource.indexOf('SystemAllocator.') + 'SystemAllocator.'.length,
    )
    assert.deepEqual(ambiguousResult?.context, {
      _tag: 'ValueMemberContext',
      state: 'Ambiguous',
    })
    assert.deepEqual(ambiguousResult?.candidates, [])

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

    const serviceSource = `service Logger { fn enabled() -> bool }
pub fn main() -> i32 { return Logger. }`
    const serviceSnapshot = yield* Analysis.ofSourceRealized('main', encoder.encode(serviceSource))
    const serviceResult = Analysis.completionAt(
      serviceSnapshot,
      'main',
      serviceSource.indexOf('Logger.') + 'Logger.'.length,
    )
    assert.deepEqual(serviceResult?.context, {
      _tag: 'ActorMemberContext',
      actor: 'Logger',
    })
    assert.deepEqual(
      serviceResult?.candidates.map((candidate) => candidate.label),
      ['enabled'],
    )

    const importedServiceSource = `import contracts { Logger }
pub fn main() -> i32 { return Logger. }`
    const importedService = yield* Analysis.makeRealized({
      root: SourceFile.make('main', encoder.encode(importedServiceSource)),
    }).pipe(
      Effect.provide(
        SourceResolver.memory(
          new Map([['contracts', encoder.encode('pub service Logger { fn enabled() -> bool }')]]),
        ),
      ),
    )
    const importedServiceResult = Analysis.completionAt(
      importedService,
      'main',
      importedServiceSource.indexOf('Logger.') + 'Logger.'.length,
    )
    assert.deepEqual(importedServiceResult?.context, {
      _tag: 'ActorMemberContext',
      actor: 'Logger',
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
    assert.notInclude(typeResult?.candidates.map((candidate) => candidate.label) ?? [], 'true')
  }),
)

it('keeps the intrinsic catalog identities and ordering stable', () => {
  const first = Intrinsic.all()
  const second = Intrinsic.all()
  assert.deepEqual(second, first)
  const identities = first.flatMap((actor) =>
    actor.operations.map((operation) => `${operation.id.actor}.${operation.id.name}`),
  )
  assert.strictEqual(new Set(identities).size, identities.length)
  assert.isTrue(
    first.every((actor) =>
      actor.operations.every((operation) =>
        Intrinsic.signature(operation).includes(operation.spelling),
      ),
    ),
  )
})
