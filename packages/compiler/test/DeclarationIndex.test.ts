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
      ['root', 'import lib\npub fn answer() -> i32 { return 1 }'],
      ['lib', 'pub fn answer() -> i32 { return 2 }'],
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

it.effect('indexes source services and their operation contracts as distinct canonical facts', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      [
        'root',
        `pub struct WriteFailure {}
pub service Logger<T> {
  effect fn log(message: &[u8], value: T) -> () ! WriteFailure ? &mut Logger<T>
  fn enabled() -> bool
}`,
      ],
    ])
    const service = index.modules.at(0)?.services.at(0)

    assert.strictEqual(service?._tag, 'ServiceDeclaration')
    assert.deepEqual(service?.canonical, {
      _tag: 'Canonical',
      id: { _tag: 'CanonicalDeclarationId', module: 'root', name: 'Logger' },
    })
    assert.deepEqual(
      service?.operations.map((operation) => ({
        name: operation.name._tag === 'Present' ? operation.name.spelling : 'Unavailable',
        state: operation.state._tag,
        kind: operation.functionKind,
        parameters: operation.parameters.map((parameter) =>
          parameter.declaredType._tag === 'Resolved'
            ? Type.encode(parameter.declaredType.type)
            : parameter.declaredType._tag,
        ),
        result:
          operation.returnType._tag === 'Resolved'
            ? Type.encode(operation.returnType.type)
            : operation.returnType._tag,
        failures: operation.failureRow.failures.map(Type.encode),
        requirements: operation.requirementRow.requirements.map((requirement) => ({
          type: Type.encode(requirement.capability),
          access: requirement.access,
        })),
      })),
      [
        {
          name: 'log',
          state: 'Unique',
          kind: 'Effect',
          parameters: ['&[u8]', 'T'],
          result: '()',
          failures: ['root.WriteFailure'],
          requirements: [{ type: 'root.Logger<T>', access: 'Exclusive' }],
        },
        {
          name: 'enabled',
          state: 'Unique',
          kind: 'Ordinary',
          parameters: [],
          result: 'bool',
          failures: [],
          requirements: [],
        },
      ],
    )
    assert.deepEqual(index.diagnostics, [])
  }),
)

it.effect('rejects service operation bodies in semantic declaration analysis', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      ['root', 'service Logger { fn enabled() -> bool { return true } }'],
    ])

    assert.deepEqual(
      index.diagnostics.map((diagnostic) => ({
        code: diagnostic.code,
        reason: diagnostic.reason._tag,
      })),
      [{ code: 'SEM0090', reason: 'InvalidServiceDeclaration' }],
    )
  }),
)

it.effect('validates arbitrary source service conformances without recognizing service names', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      [
        'root',
        `pub struct Problem {}
pub service Logger {
  effect fn log(message: i32) -> i32 ! Problem ? &mut Logger
  fn enabled() -> bool ? &Logger
}
pub struct Console {}
effect fn log(self: &mut Console, message: i32) -> i32 ! Problem { return message }
fn enabled(self: &Console) -> bool { return true }
impl Logger for Console { enabled: Console.enabled log: Console.log }`,
      ],
    ])
    const logger = Type.nominal('root', 'Logger')
    const console = Type.nominal('root', 'Console')
    const witness = DeclarationIndex.witness(index, console, logger)

    assert.strictEqual(witness?._tag, 'SourceConformanceWitness')
    assert.deepEqual(witness?._tag === 'SourceConformanceWitness' ? witness.operations : [], [
      {
        name: 'log',
        implementation: {
          _tag: 'CanonicalDeclarationId',
          module: 'root',
          name: 'log',
        },
      },
      {
        name: 'enabled',
        implementation: {
          _tag: 'CanonicalDeclarationId',
          module: 'root',
          name: 'enabled',
        },
      },
    ])
    assert.deepEqual(index.diagnostics, [])
  }),
)

it.effect('rejects incomplete and stronger source service operation mappings', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      [
        'root',
        `pub struct Problem {}
pub struct Extra {}
pub service Logger {
  effect fn log(message: i32) -> i32 ! Problem ? &Logger
  fn enabled() -> bool
}
pub struct Console {}
effect fn log(self: &mut Console, message: i32) -> i32 ! Problem | Extra { return message }
impl Logger for Console { log: Console.log unknown: Console.unknown }`,
      ],
    ])

    assert.deepEqual(
      index.diagnostics.map((diagnostic) => diagnostic.reason._tag),
      ['InvalidConformance'],
    )
    assert.include(index.diagnostics.at(0)?.message ?? '', 'missing enabled')
    assert.include(index.diagnostics.at(0)?.message ?? '', 'unknown unknown')
  }),
)

it.effect('substitutes concrete generic service contracts during conformance', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      [
        'root',
        `pub service Store<T> {
  effect fn load(fallback: T) -> T ? &Store<T>
}
pub struct IntStore {}
effect fn load(self: &IntStore, fallback: i32) -> i32 { return fallback }
impl Store<i32> for IntStore { load: IntStore.load }`,
      ],
    ])
    const store = Type.nominal('root', 'Store', ['i32'])
    const provider = Type.nominal('root', 'IntStore')
    const witness = DeclarationIndex.witness(index, provider, store)

    assert.deepEqual(index.diagnostics, [])
    assert.deepEqual(witness?._tag === 'SourceConformanceWitness' ? witness.operations : [], [
      {
        name: 'load',
        implementation: {
          _tag: 'CanonicalDeclarationId',
          module: 'root',
          name: 'load',
        },
      },
    ])
  }),
)

it.effect('rejects a mapped service implementation with stronger access and failure rows', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      [
        'root',
        `pub struct Problem {}
pub struct Extra {}
pub service Logger {
  effect fn log(message: i32) -> i32 ! Problem ? &Logger
}
pub struct Console {}
effect fn log(self: &mut Console, message: i32) -> i32 ! Problem | Extra { return message }
impl Logger for Console { log: Console.log }`,
      ],
    ])

    assert.deepEqual(
      index.diagnostics.map((diagnostic) => diagnostic.reason._tag),
      ['InvalidConformance'],
    )
    assert.include(index.diagnostics.at(0)?.message ?? '', 'incompatible with Logger.log')
  }),
)

it.effect('marks later duplicates as caused duplicates of the first occurrence', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      ['root', 'pub fn same() -> i32 { return 1 }\npub fn same() -> i32 { return 2 }'],
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
    const index = yield* collect('root', [['root', 'pub fn () -> i32 { return 0 }']])
    const header = index.modules.at(0)?.declarations.at(0)

    assert.strictEqual(header?.canonical._tag, 'Unidentified')
    assert.strictEqual(header?.name._tag, 'Unavailable')
    assert.deepEqual(index.diagnostics, [])
  }),
)

it.effect('resolves header signatures and diagnoses unknown types at exact spans', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      ['root', 'pub fn choose(left: i32, right: Mystery) -> i32 { return left }'],
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

it.effect('resolves callable parameter and result contracts canonically', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      [
        'root',
        'fn apply<T>(shared: fn(T) -> T, exclusive: mut fn(T) -> bool, consuming: once fn() -> T) -> T { return shared }',
      ],
    ])
    const declaration = index.modules.at(0)?.declarations.at(0)

    assert.deepEqual(
      declaration?.parameters.map((parameter) =>
        parameter.declaredType._tag === 'Resolved'
          ? Type.encode(parameter.declaredType.type)
          : parameter.declaredType._tag,
      ),
      ['fn(T) -> T', 'mut fn(T) -> bool', 'once fn() -> T'],
    )
    assert.deepEqual(index.diagnostics, [])
  }),
)

it.effect('indexes value, failure-row, and requirement-row binders as distinct kinds', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      [
        'root',
        'effect fn transform<A, !E, ?R>(self: Effect<A ! E ? R>, value: A) -> Effect<A ! E ? R> ! E ? R { return self }',
      ],
    ])
    const declaration = index.modules.at(0)?.declarations.at(0)

    assert.deepEqual(
      declaration?.typeParameters.map((parameter) => ({
        name: parameter.type.name,
        kind: parameter.type.kind,
      })),
      [
        { name: 'A', kind: 'Value' },
        { name: 'E', kind: 'FailureRow' },
        { name: 'R', kind: 'RequirementRow' },
      ],
    )
    assert.deepEqual(
      declaration?.failureRow.parameters.map((parameter) => parameter.name),
      ['E'],
    )
    assert.deepEqual(
      declaration?.requirementRow.parameters.map((parameter) => parameter.name),
      ['R'],
    )
    const parameterEffect = declaration?.parameters.at(0)?.declaredType
    const returnEffect = declaration?.returnType
    assert.strictEqual(parameterEffect?._tag, 'Resolved')
    assert.strictEqual(returnEffect?._tag, 'Resolved')
    if (
      parameterEffect?._tag === 'Resolved' &&
      Type.isEffect(parameterEffect.type) &&
      returnEffect?._tag === 'Resolved' &&
      Type.isEffect(returnEffect.type)
    ) {
      assert.deepEqual(
        [parameterEffect.type, returnEffect.type].map((effect) => ({
          failures: effect.failureParameters.map((parameter) => parameter.name),
          requirements: effect.requirementParameters.map((parameter) => parameter.name),
        })),
        [
          { failures: ['E'], requirements: ['R'] },
          { failures: ['E'], requirements: ['R'] },
        ],
      )
    }
    assert.deepEqual(index.diagnostics, [])
  }),
)

it.effect('diagnoses generic row binders used in the wrong channel and unbound row names', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      [
        'root',
        `effect fn bad<!E, ?R>(left: E, right: R) -> E ! R ? E { return left }
effect fn unbound() -> i32 ? MissingRow { return 0 }`,
      ],
    ])

    assert.deepEqual(
      index.diagnostics.map((diagnostic) => ({
        code: diagnostic.code,
        reason: diagnostic.reason._tag,
      })),
      [
        { code: 'SEM0088', reason: 'GenericParameterKindMismatch' },
        { code: 'SEM0088', reason: 'GenericParameterKindMismatch' },
        { code: 'SEM0088', reason: 'GenericParameterKindMismatch' },
        { code: 'SEM0088', reason: 'GenericParameterKindMismatch' },
        { code: 'SEM0088', reason: 'GenericParameterKindMismatch' },
        { code: 'SEM0001', reason: 'UnknownType' },
      ],
    )
  }),
)

it.effect('keeps cross-kind duplicate binders attached to the first canonical identity', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      ['root', 'effect fn duplicate<A, !A, ?A>(value: A) -> A { return value }'],
    ])
    const parameters = index.modules.at(0)?.declarations.at(0)?.typeParameters ?? []

    assert.strictEqual(parameters.at(1)?.type, parameters.at(0)?.type)
    assert.strictEqual(parameters.at(2)?.type, parameters.at(0)?.type)
    assert.strictEqual(parameters.at(1)?.duplicateOf, parameters.at(0)?.type)
    assert.strictEqual(parameters.at(2)?.duplicateOf, parameters.at(0)?.type)
    assert.strictEqual(parameters.at(0)?.type.kind, 'Value')
    assert.deepEqual(
      index.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0050', 'SEM0050'],
    )
  }),
)

it.effect('normalizes effect failure rows while retaining source members', () =>
  Effect.gen(function* () {
    const index = yield* collect('root', [
      [
        'root',
        'struct First {}\nstruct Second {}\n' +
          'effect fn work() -> i32 ! Second | First | Second { return 1 }\n' +
          'fn plain() -> i32 { return 0 }',
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
          'effect fn imported() -> i32 ! Error.Error { return 1 }\n' +
          'effect fn generic<T>() -> i32 ! T { return 1 }\n' +
          'effect fn scalar() -> i32 ! i32 { return 1 }\n' +
          'effect fn missing() -> i32 ! Mystery { return 1 }',
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
      ['root', 'struct Problem {}\nfn bad() -> i32 ! Problem { return 0 }'],
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
fn later() -> Effect<i32 ! Problem ? &FileSystem | &mut Allocator@Scratch> {
  return effect { return 1 }
}
effect fn work() -> i32 ! Problem ? &FileSystem | &mut Allocator@Scratch { return 1 }`,
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
        { capability: 'Allocator', role: 'Scratch', access: 'Exclusive' },
        { capability: 'FileSystem', role: 'DefaultRole', access: 'Shared' },
      ],
    )
    assert.deepEqual(work?.requirementRow.requirements, laterType.requirements)
  }),
)

it.effect('orders modules canonically and answers per-module lookups', () =>
  Effect.gen(function* () {
    const index = yield* collect('zeta', [
      ['zeta', 'import alpha\npub fn main() -> i32 { return 42 }'],
      ['alpha', 'pub fn helper() -> i32 { return 1 }'],
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
      ['lib', 'pub fn same() -> i32 { return 1 }\npub fn same() -> i32 { return 2 }'],
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
        'pub struct Token { pub kind: i32 lexeme: bool }\n' +
          'pub fn make(kind: i32) -> i32 { return kind }\n' +
          'fn Token() -> i32 { return 0 }',
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
      ['root', 'struct Pair { value: i32 value: bool pub : i32 tail: bool }'],
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
      ['direct', 'struct Node { next: Node }\npub fn main() -> i32 { return 0 }'],
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
          'struct Position { x: i32 }\n' +
          'pub fn main() -> i32 { return 0 }',
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
      'fn scan<T>(shared: &[T], exclusive: &mut [T]) -> i32 { return 0 }\n' +
      'fn badParameter(values: [&[i32]; 1]) -> i32 { return 0 }\n' +
      'fn badReturn() -> &[i32] { return 0 }\n' +
      'struct BadField { values: &[i32] }'
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

it.effect('indexes nominal service conformance without erasing the provider type', () =>
  Effect.gen(function* () {
    const index = yield* collect('allocator', [
      [
        'allocator',
        `service Allocator { effect fn allocate(layout: i32) -> i32 ? &mut Allocator }
struct TestAllocator { remaining: i32 }
effect fn allocate(self: &mut TestAllocator, layout: i32) -> i32 { return layout }
impl Allocator for TestAllocator { allocate: TestAllocator.allocate }
pub fn main() -> i32 { return 0 }`,
      ],
    ])
    const witness = index.modules.at(0)?.conformances.at(0)
    assert.strictEqual(witness?.capability._tag, 'Resolved')
    assert.strictEqual(witness?.provider._tag, 'Resolved')
    assert.deepEqual(
      witness?.operations.map((operation) => operation.name._tag),
      ['Present'],
    )
    assert.isTrue(
      DeclarationIndex.conforms(
        index,
        Type.nominal('allocator', 'TestAllocator'),
        Type.nominal('allocator', 'Allocator'),
      ),
    )
  }),
)

it.effect('validates service mappings and rejects duplicate or foreign witnesses', () =>
  Effect.gen(function* () {
    const valid = yield* collect('allocator-valid', [
      [
        'allocator-valid',
        `service Allocator { effect fn allocate(layout: i32) -> i32 ? &mut Allocator }
struct TestAllocator { remaining: i32 }
effect fn allocate(self: &mut TestAllocator, layout: i32) -> i32 { return layout }
impl Allocator for TestAllocator { allocate: TestAllocator.allocate }`,
      ],
    ])
    assert.deepEqual(valid.diagnostics, [])
    assert.isTrue(
      DeclarationIndex.conforms(
        valid,
        Type.nominal('allocator-valid', 'TestAllocator'),
        Type.nominal('allocator-valid', 'Allocator'),
      ),
    )

    const invalid = yield* collect('allocator-invalid', [
      [
        'allocator-invalid',
        `service Allocator { effect fn allocate(layout: i32) -> i32 ? &mut Allocator }
struct TestAllocator { remaining: i32 }
fn allocate(self: &TestAllocator) -> i32 { return 0 }
impl Allocator for TestAllocator { allocate: Foreign.allocate }
impl Allocator for TestAllocator { allocate: TestAllocator.allocate }`,
      ],
    ])
    assert.deepEqual(
      invalid.diagnostics
        .filter((diagnostic) => diagnostic.code === 'SEM0083')
        .map((diagnostic) => diagnostic.reason._tag),
      ['InvalidConformance', 'InvalidConformance'],
    )
    assert.isFalse(
      DeclarationIndex.conforms(
        invalid,
        Type.nominal('allocator-invalid', 'TestAllocator'),
        Type.nominal('allocator-invalid', 'Allocator'),
      ),
    )
  }),
)

it.effect('accepts one affine Drop hook and rejects Copy targets and malformed headers', () =>
  Effect.gen(function* () {
    const index = yield* collect('drop-hooks', [
      [
        'drop-hooks',
        `struct Guard { allocation: Allocation }
struct CopyValue { value: i32 }
struct Left { value: i32 }
struct Right { value: i32 }
struct UnionHolder { value: Left | Right }
impl Drop for Guard { fn drop(self: &mut Guard) -> () { return () } }
impl Drop for CopyValue { fn drop(self: &mut CopyValue) -> () { return () } }
impl Drop for UnionHolder { fn drop(self: &mut UnionHolder) -> () { return () } }
impl Drop for Guard { effect fn dispose(value: &Guard) -> i32 { return 0 } }`,
      ],
    ])
    assert.deepEqual(
      index.diagnostics
        .filter((diagnostic) => diagnostic.code === 'SEM0084')
        .map((diagnostic) =>
          diagnostic.reason._tag === 'InvalidDropHook' ? diagnostic.reason.detail : undefined,
        ),
      [
        'Copy type drop-hooks.CopyValue cannot implement Drop',
        'Copy type drop-hooks.UnionHolder cannot implement Drop',
      ],
    )
    assert.include(
      index.diagnostics
        .filter((diagnostic) => diagnostic.code === 'SEM0083')
        .map((diagnostic) =>
          diagnostic.reason._tag === 'InvalidConformance' ? diagnostic.reason.detail : undefined,
        ),
      'duplicate Drop implementation for drop-hooks.Guard',
    )
  }),
)

it.effect('accepts operation-free Report markers and rejects marker bodies', () =>
  Effect.gen(function* () {
    const valid = yield* collect('report-valid', [
      [
        'report-valid',
        `struct SomeError { code: i32 }
impl Report for SomeError {}
pub fn main() -> i32 { return 0 }`,
      ],
    ])
    assert.deepEqual(valid.diagnostics, [])
    assert.isTrue(
      DeclarationIndex.conforms(
        valid,
        Type.nominal('report-valid', 'SomeError'),
        Type.reportCapability,
      ),
    )

    const malformed = yield* collect('report-malformed', [
      [
        'report-malformed',
        `struct SomeError { code: i32 }
fn describe(error: &SomeError) -> i32 { return error.code }
impl Report for SomeError { report: SomeError.describe }
pub fn main() -> i32 { return 0 }`,
      ],
    ])
    assert.include(
      malformed.diagnostics
        .filter((diagnostic) => diagnostic.code === 'SEM0083')
        .map((diagnostic) =>
          diagnostic.reason._tag === 'InvalidConformance' ? diagnostic.reason.detail : undefined,
        ),
      'Report is an operation-free marker capability',
    )
    assert.isFalse(
      DeclarationIndex.conforms(
        malformed,
        Type.nominal('report-malformed', 'SomeError'),
        Type.reportCapability,
      ),
    )
  }),
)

it.effect('indexes parametric conformances with bound parameters', () =>
  Effect.gen(function* () {
    const index = yield* collect('parametric', [
      [
        'parametric',
        `struct Vector<T> { storage: Allocation }
impl<T> Drop for Vector<T> { fn drop(self: &mut Vector<T>) -> () { return () } }
pub fn main() -> i32 { return 0 }`,
      ],
    ])
    assert.deepEqual(index.diagnostics, [])
    const conformance = index.modules.at(0)?.conformances.at(0)
    assert.strictEqual(conformance?.typeParameters.length, 1)
    assert.strictEqual(conformance?.provider._tag, 'Resolved')
    if (conformance?.provider._tag === 'Resolved' && Type.isNominal(conformance.provider.type)) {
      const argument = conformance.provider.type.arguments.at(0)
      assert.isTrue(argument !== undefined && Type.isParameter(argument))
    }
  }),
)

it.effect('rejects unbound, duplicate, and overlapping parametric conformances', () =>
  Effect.gen(function* () {
    const details = (index: DeclarationIndex.Index, code: string) =>
      index.diagnostics
        .filter((diagnostic) => diagnostic.code === code)
        .map((diagnostic) =>
          'detail' in diagnostic.reason ? diagnostic.reason.detail : diagnostic.reason._tag,
        )

    const unbound = yield* collect('parametric-unbound', [
      [
        'parametric-unbound',
        `struct Vector<T> { storage: Allocation }
impl<T, U> Drop for Vector<T> { fn drop(self: &mut Vector<T>) -> () { return () } }`,
      ],
    ])
    assert.deepEqual(details(unbound, 'SEM0083'), [
      'impl type parameter U is not used by the provider type',
    ])

    const duplicate = yield* collect('parametric-duplicate', [
      [
        'parametric-duplicate',
        `struct Vector<T> { storage: Allocation }
impl<T, T> Drop for Vector<T> { fn drop(self: &mut Vector<T>) -> () { return () } }`,
      ],
    ])
    assert.deepEqual(
      duplicate.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0050'],
    )

    const overlapping = yield* collect('parametric-overlap', [
      [
        'parametric-overlap',
        `struct Vector<T> { storage: Allocation }
impl<T> Drop for Vector<T> { fn drop(self: &mut Vector<T>) -> () { return () } }
impl<U> Drop for Vector<U> { fn drop(self: &mut Vector<U>) -> () { return () } }`,
      ],
    ])
    assert.deepEqual(details(overlapping, 'SEM0083'), [
      'duplicate Drop implementation for parametric-overlap.Vector<U>',
    ])
  }),
)
