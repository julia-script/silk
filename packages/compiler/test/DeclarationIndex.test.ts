import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as DeclarationIndex from '../src/DeclarationIndex.js'
import * as ModuleClosure from '../src/ModuleClosure.js'
import * as NameResolution from '../src/NameResolution.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const collect = (
  rootModule: string,
  entries: ReadonlyArray<readonly [string, string]>,
): Effect.Effect<DeclarationIndex.Index> => {
  const rootText = entries.find(([name]) => name === rootModule)?.[1]
  if (rootText === undefined) throw new RangeError(`Fixture has no root source ${rootModule}`)
  return Effect.map(
    ModuleClosure.load({ root: SourceFile.make(rootModule, ascii(rootText)) }).pipe(
      Effect.provide(
        SourceResolver.memory(
          new Map(
            entries
              .filter(([name]) => name !== rootModule)
              .map(([name, text]) => [name, ascii(text)] as const),
          ),
        ),
      ),
    ),
    (closure) => NameResolution.analyze(closure).index,
  )
}

it.effect('assigns distinct canonical identities to same-named declarations across modules', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      ['root', 'import lib\npub fn answer() -> I32 { return 1 }'],
      ['lib', 'pub fn answer() -> I32 { return 2 }'],
    ])

    const canonicals = index.modules.flatMap((module) =>
      module.declarations.map((declaration) => declaration.canonical),
    )
    assert.deepEqual(canonicals, [
      { _tag: 'Canonical', id: { _tag: 'CanonicalDeclarationId', module: 'lib', name: 'answer' } },
      { _tag: 'Canonical', id: { _tag: 'CanonicalDeclarationId', module: 'root', name: 'answer' } },
    ])
    assert.deepEqual(index.diagnostics, [])
  }),
)

it.effect('marks later duplicates as caused duplicates of the first occurrence', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      ['root', 'pub fn same() -> I32 { return 1 }\npub fn same() -> I32 { return 2 }'],
    ])
    const headers = index.modules.at(0)?.declarations ?? []

    assert.strictEqual(headers.at(0)?.canonical._tag, 'Canonical')
    const duplicate = headers.at(1)?.canonical
    assert.strictEqual(duplicate?._tag, 'Duplicate')
    if (duplicate?._tag !== 'Duplicate') return
    assert.deepEqual(duplicate.original, {
      _tag: 'CanonicalDeclarationId',
      module: 'root',
      name: 'same',
    })
    assert.strictEqual(duplicate.cause.code, 'SEM0003')
    assert.deepEqual(
      index.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0003'],
    )
  }),
)

it.effect('keeps unavailable names unidentified without extra diagnostics', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [['root', 'pub fn () -> I32 { return 0 }']])
    const header = index.modules.at(0)?.declarations.at(0)

    assert.strictEqual(header?.canonical._tag, 'Unidentified')
    assert.strictEqual(header?.name._tag, 'Unavailable')
    assert.deepEqual(index.diagnostics, [])
  }),
)

it.effect('resolves header signatures and diagnoses unknown types at exact spans', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      ['root', 'pub fn choose(left: I32, right: Mystery) -> I32 { return left }'],
    ])
    const header = index.modules.at(0)?.declarations.at(0)

    assert.strictEqual(header?.parameterCount, 2)
    assert.strictEqual(header?.parameters.at(0)?.declaredType._tag, 'Resolved')
    assert.strictEqual(header?.parameters.at(1)?.declaredType._tag, 'Unresolved')
    assert.strictEqual(header?.returnType._tag, 'Resolved')
    assert.deepEqual(
      index.diagnostics.map((diagnostic) => ({
        code: diagnostic.code,
        start: diagnostic.span.start,
        end: diagnostic.span.end,
      })),
      [{ code: 'SEM0001', start: 32, end: 39 }],
    )
  }),
)

it.effect('normalizes effect failure rows while retaining source members', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      [
        'root',
        'struct First {}\nstruct Second {}\n' +
          'effect fn work() -> I32 ! Second | First | Second { return 1 }\n' +
          'fn plain() -> I32 { return 0 }',
      ],
    ])
    const effect = index.modules.at(0)?.declarations.at(0)
    const plain = index.modules.at(0)?.declarations.at(1)

    assert.strictEqual(effect?.functionKind, 'Effect')
    assert.strictEqual(effect?.failureRow.members.length, 3)
    assert.deepEqual(
      effect?.failureRow.failures.map((failure) => `${failure.module}.${failure.name}`),
      ['root.First', 'root.Second'],
    )
    assert.strictEqual(effect?.failureRow.available, true)
    assert.strictEqual(plain?.functionKind, 'Ordinary')
    assert.deepEqual(plain?.failureRow.failures, [])
    assert.deepEqual(index.diagnostics, [])
  }),
)

it.effect('resolves imported failure members and preserves invalid row facts', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      [
        'root',
        'import errors.Error\n' +
          'effect fn imported() -> I32 ! Error.Error { return 1 }\n' +
          'effect fn generic<T>() -> I32 ! T { return 1 }\n' +
          'effect fn scalar() -> I32 ! I32 { return 1 }\n' +
          'effect fn missing() -> I32 ! Mystery { return 1 }',
      ],
      ['errors/Error', 'pub struct Error {}'],
    ])
    const declarations =
      index.modules.find((module) => module.module === 'root')?.declarations ?? []

    assert.deepEqual(
      declarations.map((declaration) => ({
        available: declaration.failureRow.available,
        failures: declaration.failureRow.failures.map(
          (failure) => `${failure.module}.${failure.name}`,
        ),
      })),
      [
        { available: true, failures: ['errors/Error.Error'] },
        { available: false, failures: [] },
        { available: false, failures: [] },
        { available: false, failures: [] },
      ],
    )
    assert.deepEqual(
      index.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0061', 'SEM0061', 'SEM0001'],
    )
  }),
)

it.effect('rejects failure rows on ordinary functions without losing the row', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      ['root', 'struct Problem {}\nfn bad() -> I32 ! Problem { return 0 }'],
    ])
    const declaration = index.modules.at(0)?.declarations.at(0)

    assert.strictEqual(declaration?.functionKind, 'Ordinary')
    assert.deepEqual(
      declaration?.failureRow.failures.map((failure) => failure.name),
      ['Problem'],
    )
    assert.deepEqual(
      index.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0062'],
    )
  }),
)

it.effect('resolves explicit Effect contracts and canonical requirement rows', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      [
        'root',
        `struct Problem {}
struct FileSystem {}
struct Allocator {}
fn later() -> Effect<I32 ! Problem ? &FileSystem | &mut Allocator@Scratch> {
  return effect { return 1 }
}
effect fn work() -> I32 ! Problem ? &FileSystem | &mut Allocator@Scratch { return 1 }`,
      ],
    ])
    const [later, work] = index.modules.at(0)?.declarations ?? []
    assert.deepEqual(index.diagnostics, [])
    assert.strictEqual(later?.returnType._tag, 'Resolved')
    const laterType = later?.returnType._tag === 'Resolved' ? later.returnType.type : undefined
    assert.isTrue(laterType !== undefined && Type.isEffect(laterType))
    if (laterType === undefined || !Type.isEffect(laterType)) return
    assert.deepEqual(
      laterType.failures.map((failure) => failure.name),
      ['Problem'],
    )
    assert.deepEqual(
      laterType.requirements.map((requirement) => ({
        capability: requirement.capability.name,
        role: requirement.role,
        access: requirement.access,
      })),
      [
        { capability: 'FileSystem', role: 'DefaultRole', access: 'Shared' },
        { capability: 'Allocator', role: 'Scratch', access: 'Exclusive' },
      ],
    )
    assert.deepEqual(work?.requirementRow.requirements, laterType.requirements)
  }),
)

it.effect('orders modules canonically and answers per-module lookups', () =>
  Effect.gen(function* () {
    const index = yield* collect('zeta', [
      ['zeta', 'import alpha\npub fn main() -> I32 { return 42 }'],
      ['alpha', 'pub fn helper() -> I32 { return 1 }'],
    ])

    assert.deepEqual(
      index.modules.map((module) => module.module),
      ['alpha', 'zeta'],
    )
    assert.strictEqual(DeclarationIndex.lookup(index, 'zeta', 'main')._tag, 'Resolved')
    assert.strictEqual(DeclarationIndex.lookup(index, 'zeta', 'helper')._tag, 'Missing')
    assert.strictEqual(DeclarationIndex.lookup(index, 'alpha', 'helper')._tag, 'Resolved')
  }),
)

it.effect('collects identical indexes across repeated fresh runs', () =>
  Effect.gen(function* () {
    const entries: ReadonlyArray<readonly [string, string]> = [
      ['root', 'import lib\npub fn main() -> Mystery { return lib }'],
      ['lib', 'pub fn same() -> I32 { return 1 }\npub fn same() -> I32 { return 2 }'],
    ]
    const first = yield* collect('root', entries)
    const second = yield* collect('root', [...entries].reverse())

    assert.deepEqual(first, second)
  }),
)

it.effect('indexes mixed struct and function declarations in one canonical namespace', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      [
        'root',
        'pub struct Token { pub kind: I32 lexeme: Bool }\n' +
          'pub fn make(kind: I32) -> I32 { return kind }\n' +
          'fn Token() -> I32 { return 0 }',
      ],
    ])
    const module = index.modules.at(0)

    assert.deepEqual(
      module?.members.map((member) => member._tag),
      ['StructDeclaration', 'FunctionDeclaration', 'FunctionDeclaration'],
    )
    assert.strictEqual(module?.structs.at(0)?.fields.at(0)?.visibility, 'Public')
    assert.strictEqual(module?.structs.at(0)?.fields.at(1)?.visibility, 'Private')
    assert.strictEqual(module?.members.at(2)?.canonical._tag, 'Duplicate')
    assert.deepEqual(
      index.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0003'],
    )
  }),
)

it.effect('retains duplicate and damaged struct fields without losing later fields', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      ['root', 'struct Pair { value: I32 value: Bool pub : I32 tail: Bool }'],
    ])
    const fields = index.modules.at(0)?.structs.at(0)?.fields ?? []

    assert.deepEqual(
      fields.map((field) => field.state._tag),
      ['Unique', 'Duplicate', 'Unidentified', 'Unique'],
    )
    assert.strictEqual(fields.at(3)?.declaredType._tag, 'Resolved')
    assert.deepEqual(
      index.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0017'],
    )
  }),
)

it.effect('diagnoses private exposure and inline recursive struct components canonically', () =>
  Effect.gen(function* () {
    const exposedSource =
      'struct Hidden {}\n' +
      'pub struct Public { pub hidden: Hidden private: Hidden }\n' +
      'pub fn reveal(value: Hidden) -> Hidden { return value }'
    const exposed = yield* collect('root', [['root', exposedSource]])
    assert.deepEqual(
      exposed.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0019', 'SEM0019', 'SEM0019'],
    )
    assert.deepEqual(
      exposed.diagnostics.map((diagnostic) =>
        exposedSource.slice(diagnostic.span.start, diagnostic.span.end),
      ),
      ['Hidden', 'Hidden', 'Hidden'],
    )

    const recursiveSource = 'import b.B\npub struct A { value: B.B }'
    const recursive = yield* collect('a/A', [
      ['a/A', recursiveSource],
      ['b/B', 'import a.A\npub struct B { value: A.A }'],
    ])
    assert.deepEqual(
      recursive.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0020'],
    )
    assert.deepEqual(
      recursive.diagnostics.map((diagnostic) => ({
        sourceId: diagnostic.span.sourceId,
        text: recursiveSource.slice(diagnostic.span.start, diagnostic.span.end),
      })),
      [{ sourceId: 'a/A', text: 'A' }],
    )
    assert.strictEqual(recursive.modules.at(0)?.structs.at(0)?.dependency._tag, 'Unavailable')
    assert.strictEqual(recursive.modules.at(1)?.structs.at(0)?.dependency._tag, 'Unavailable')

    const direct = yield* collect('direct', [
      ['direct', 'struct Node { next: Node }\npub fn main() -> I32 { return 0 }'],
    ])
    assert.deepEqual(
      direct.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0020'],
    )
    assert.strictEqual(direct.modules.at(0)?.structs.at(0)?.dependency._tag, 'Unavailable')
  }),
)

it.effect('resolves forward nominal fields into a canonical acyclic dependency set', () =>
  Effect.gen(function* () {
    const index = yield* collect('geometry', [
      [
        'geometry',
        'struct Span { first: Position second: Position }\n' +
          'struct Position { x: I32 }\n' +
          'pub fn main() -> I32 { return 0 }',
      ],
    ])
    const span = index.modules.at(0)?.structs.at(0)

    assert.strictEqual(span?.dependency._tag, 'Available')
    assert.deepEqual(span?.dependency.types, [
      { _tag: 'NominalType', module: 'geometry', name: 'Position', arguments: [] },
    ])
    assert.deepEqual(
      span?.fields.map((field) =>
        field.declaredType._tag === 'Resolved' ? field.declaredType.type : field.declaredType._tag,
      ),
      [
        { _tag: 'NominalType', module: 'geometry', name: 'Position', arguments: [] },
        { _tag: 'NominalType', module: 'geometry', name: 'Position', arguments: [] },
      ],
    )
  }),
)

it.effect('resolves direct generic slice parameters and rejects borrowed storage types', () =>
  Effect.gen(function* () {
    const source =
      'fn scan<T>(shared: &[T], exclusive: &mut [T]) -> I32 { return 0 }\n' +
      'fn badParameter(values: [&[I32]; 1]) -> I32 { return 0 }\n' +
      'fn badReturn() -> &[I32] { return 0 }\n' +
      'struct BadField { values: &[I32] }'
    const index = yield* collect('slices', [['slices', source]])
    const scan = index.modules.at(0)?.declarations.at(0)

    assert.deepEqual(
      scan?.parameters.map((parameter) =>
        parameter.declaredType._tag === 'Resolved'
          ? parameter.declaredType.spelling
          : parameter.declaredType._tag,
      ),
      ['&[T]', '&mut [T]'],
    )
    assert.deepEqual(
      index.diagnostics.map((diagnostic) => ({
        code: diagnostic.code,
        position:
          diagnostic.reason._tag === 'SliceTypePosition' ? diagnostic.reason.position : undefined,
      })),
      [
        { code: 'SEM0054', position: 'parameter' },
        { code: 'SEM0054', position: 'return' },
        { code: 'SEM0054', position: 'field' },
      ],
    )
  }),
)
