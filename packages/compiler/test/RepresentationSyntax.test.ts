import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as FormattedDocument from '../src/FormattedDocument.js'
import * as Lexer from '../src/Lexer.js'
import * as ModuleClosure from '../src/ModuleClosure.js'
import * as NameResolution from '../src/NameResolution.js'
import * as Parser from '../src/Parser.js'
import * as Presentation from '../src/Presentation.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as SuspensionMode from '../src/SuspensionMode.js'
import * as SyntaxFormatter from '../src/SyntaxFormatter.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import * as Type from '../src/Type.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

const parse = (id: string, source: string) =>
  Parser.parse(Lexer.lex(SourceFile.make(id, encoder.encode(source))))

const descendants = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Element> =>
  node.children.flatMap((child): ReadonlyArray<SyntaxTree.Element> =>
    SyntaxTree.isNode(child) ? [child, ...descendants(child)] : [child],
  )

const index = (id: string, source: string) =>
  Effect.map(
    ModuleClosure.load({ root: SourceFile.make(id, encoder.encode(source)) }).pipe(
      Effect.provide(SourceResolver.memory(new Map())),
    ),
    (closure) => NameResolution.analyze(closure).index,
  )

it.effect('parses and formats callable and Effect representation parameter bounds', () =>
  Effect.gen(function* () {
    const source = `pub struct Mapper<A,B,F:fn(A)->B>{transform:F}
pub struct Shared<A,F:Effect<A>>{operation:F}
pub struct Deferred<A,E,?R,F:once Effect<A!E?R>>{operation:F}
pub struct Exclusive<A,F:mut Effect<A>>{operation:F}`
    const syntax = parse('representation-syntax/positive', source)
    const parameters = descendants(syntax.root).filter(
      (element): element is SyntaxTree.Node =>
        SyntaxTree.isNode(element) && element.kind === 'TypeParameter',
    )

    assert.strictEqual(parameters.length, 11)
    assert.strictEqual(
      parameters.filter(
        (parameter) => SyntaxTree.directNode(parameter, 'CallableType') !== undefined,
      ).length,
      1,
    )
    assert.strictEqual(
      parameters.filter(
        (parameter) => SyntaxTree.directNode(parameter, 'AppliedType') !== undefined,
      ).length,
      3,
    )
    assert.deepEqual(syntax.parserDiagnostics, [])

    const formatted = yield* SyntaxFormatter.format(syntax)
    assert.strictEqual(
      decoder.decode(FormattedDocument.toUint8Array(formatted)),
      `pub struct Mapper<A, B, F: fn(A) -> B> {
  transform: F
}

pub struct Shared<A, F: Effect<A>> {
  operation: F
}

pub struct Deferred<A, E, ?R, F: once Effect<A ! E ? R>> {
  operation: F
}

pub struct Exclusive<A, F: mut Effect<A>> {
  operation: F
}
`,
    )
  }),
)

it.effect('indexes ordered representation kinds, bounds, and represented field uses', () =>
  Effect.gen(function* () {
    const analyzed = yield* index(
      'representation-syntax/indexed',
      `pub struct Mapper<A, B, F: fn(A) -> B> { transform: F }
pub struct Deferred<A, E, ?R, F: once Effect<A ! E ? R>> { operation: F }`,
    )
    const [mapper, deferred] = analyzed.modules.at(0)?.structs ?? []

    assert.deepEqual(
      mapper?.typeParameters.map((parameter) => parameter.type.kind),
      ['Value', 'Value', 'CallableRepresentation'],
    )
    assert.deepEqual(
      deferred?.typeParameters.map((parameter) => parameter.type.kind),
      ['Value', 'Value', 'RequirementRow', 'EffectRepresentation'],
    )
    assert.strictEqual(mapper?.typeParameters.at(2)?.representationBound?.kind, 'Callable')
    assert.strictEqual(deferred?.typeParameters.at(3)?.representationBound?.kind, 'Effect')
    const mapperField = mapper?.fields.at(0)?.declaredType
    const deferredField = deferred?.fields.at(0)?.declaredType
    assert.strictEqual(
      mapperField?._tag === 'Resolved' && Type.isRepresented(mapperField.type),
      true,
    )
    assert.strictEqual(
      deferredField?._tag === 'Resolved' && Type.isRepresented(deferredField.type),
      true,
    )
    assert.deepEqual(analyzed.diagnostics, [])
  }),
)

it.effect('retains only the closed executable static-property set in canonical order', () =>
  Effect.gen(function* () {
    const analyzed = yield* index(
      'representation-syntax/static-properties',
      `interface Marker {}
struct Deferred<F: once Effect<i32> + Intrinsic.NonParking + Intrinsic.Detached> { operation: F }
struct Forwarded<F: once Effect<i32> + Intrinsic.Detached + Intrinsic.NonParking> { value: Deferred<F> }
struct NotRepresentation<F: once Effect<i32> + Marker> { value: F }`,
    )
    const [_, deferred, forwarded, invalid] = analyzed.modules.at(0)?.members ?? []

    assert.deepEqual(deferred?.typeParameters.at(0)?.staticProperties, [
      'Intrinsic.Detached',
      'Intrinsic.NonParking',
    ])
    assert.deepEqual(forwarded?.typeParameters.at(0)?.staticProperties, [
      'Intrinsic.Detached',
      'Intrinsic.NonParking',
    ])
    assert.strictEqual(deferred?.typeParameters.at(0)?.type.kind, 'EffectRepresentation')
    assert.strictEqual(forwarded?.typeParameters.at(0)?.type.kind, 'EffectRepresentation')
    assert.strictEqual(invalid?.typeParameters.at(0)?.type.kind, 'EffectRepresentation')
    assert.deepEqual(invalid?.typeParameters.at(0)?.staticProperties, [])
    assert.deepEqual(
      SuspensionMode.openExecutable(deferred?.typeParameters.at(0)?.staticProperties ?? []).modes,
      ['NestedTransfer'],
    )
    assert.deepEqual(SuspensionMode.openExecutable([]).modes, ['NestedTransfer', 'ExternalPark'])
  }),
)

it.effect('rejects ordinary conjuncts without reinterpreting the exact executable binder', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSource(
      'representation-syntax/invalid-property-conjunct',
      encoder.encode(`interface Marker {}
struct Deferred<F: Effect<i32> + Marker> { operation: F }
pub fn main() -> i32 { return 0 }`),
    )
    const deferred = self.index.modules.at(0)?.structs.at(0)

    assert.strictEqual(deferred?.typeParameters.at(0)?.type.kind, 'EffectRepresentation')
    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['SEM0141'],
    )
  }),
)

it.effect('forwards every open generic kind through one ordered nominal application', () =>
  Effect.gen(function* () {
    const analyzed = yield* index(
      'representation-syntax/kinded-application',
      `struct Inner<A, E, ?R, F: fn(A) -> A> { value: A operation: F }
struct Outer<A, E, ?R, F: fn(A) -> A> { inner: Inner<A, E, R, F> }`,
    )
    const outer = analyzed.modules.at(0)?.structs.at(1)
    const field = outer?.fields.at(0)?.declaredType

    assert.strictEqual(field?._tag, 'Resolved')
    if (field?._tag !== 'Resolved' || !Type.isNominal(field.type)) return
    assert.strictEqual(Type.isTypeArgument(field.type.arguments.at(0) ?? Type.unit), true)
    assert.strictEqual(Type.isTypeArgument(field.type.arguments.at(1) ?? Type.unit), true)
    assert.strictEqual(Type.isRequirementRowArgument(field.type.arguments.at(2) ?? Type.unit), true)
    assert.strictEqual(
      Type.isRepresentationParameterArgument(field.type.arguments.at(3) ?? Type.unit),
      true,
    )
    assert.deepEqual(analyzed.diagnostics, [])
  }),
)

it.effect('diagnoses incompatible callable and Effect representation access direction', () =>
  Effect.gen(function* () {
    const analyzed = yield* index(
      'representation-syntax/incompatible-bounds',
      `struct SharedCallable<A, F: fn(A) -> A> { operation: F }
struct OnceCallable<A, F: once fn(A) -> A> { operation: F }
struct SharedEffect<A, F: Effect<A>> { operation: F }
struct OnceEffect<A, F: once Effect<A>> { operation: F }
fn invalidCallable<A, F: once fn(A) -> A>(operation: F) -> SharedCallable<A, F> { loop {} }
fn validCallable<A, F: fn(A) -> A>(operation: F) -> OnceCallable<A, F> { loop {} }
fn invalidEffect<A, F: once Effect<A>>(operation: F) -> SharedEffect<A, F> { loop {} }
fn validEffect<A, F: Effect<A>>(operation: F) -> OnceEffect<A, F> { loop {} }`,
    )
    const diagnostics = analyzed.diagnostics.filter((diagnostic) => diagnostic.code === 'SEM0106')

    assert.strictEqual(diagnostics.length, 2)
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.reason._tag),
      ['IncompatibleRepresentationBound', 'IncompatibleRepresentationBound'],
    )
    assert.include(diagnostics.at(0)?.message ?? '', 'requires fn(A) -> A')
    assert.include(diagnostics.at(0)?.message ?? '', 'supplied bound once fn(A) -> A')
    assert.include(diagnostics.at(1)?.message ?? '', 'requires Effect<A>')
    assert.include(diagnostics.at(1)?.message ?? '', 'supplied bound once Effect<A>')
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.relatedSpans?.map((related) => related.label)),
      [
        [
          'required representation bound declared here',
          'supplied representation bound declared here',
        ],
        [
          'required representation bound declared here',
          'supplied representation bound declared here',
        ],
      ],
    )
  }),
)

it.effect('recovers damaged bounds and diagnoses duplicate, unbound, and wrong-kind uses', () =>
  Effect.gen(function* () {
    const damaged = parse(
      'representation-syntax/damaged',
      `struct Broken<A, F: fn(A) ->> { value: A }
struct Next { value: i32 }`,
    )
    assert.isAbove(damaged.parserDiagnostics.length, 0)
    assert.strictEqual(SyntaxTree.directNodes(damaged.root, 'StructDeclaration').length, 2)

    const analyzed = yield* index(
      'representation-syntax/negative',
      `struct Duplicate<F, F: fn(i32) -> i32> { value: i32 }
struct Unbound<F: fn(Missing) -> i32> { value: i32 }
struct Wrong<E, F: fn(E) -> i32> { value: i32 }
struct ValueBox<T> { value: T }
struct WrongUse<A, F: fn(A) -> A> { value: ValueBox<F> }
struct RowBox<E> {}
struct WrongRow<A, E> { value: RowBox<A> }`,
    )
    assert.deepEqual(
      analyzed.diagnostics.map((diagnostic) => ({
        code: diagnostic.code,
        reason: diagnostic.reason._tag,
      })),
      [
        { code: 'SEM0050', reason: 'DuplicateTypeParameter' },
        { code: 'SEM0001', reason: 'UnknownType' },
        { code: 'SEM0088', reason: 'GenericParameterKindMismatch' },
      ],
    )
  }),
)

it.effect('classifies only a direct Effect target as an Effect representation bound', () =>
  Effect.gen(function* () {
    const analyzed = yield* index(
      'representation-syntax/effect-target',
      `struct Wrapper<T> {}
struct NotRepresentation<F: Wrapper<Effect<i32>>> { value: F }`,
    )
    const declaration = analyzed.modules.at(0)?.structs.at(1)

    assert.strictEqual(declaration?.typeParameters.at(0)?.type.kind, 'Value')
  }),
)

it.effect('presents forwarded failure and requirement row parameters', () =>
  Effect.gen(function* () {
    const analyzed = yield* index(
      'representation-syntax/row-presentation',
      `service Console {}
fn forward<E, ?R>() -> i32 ! E ? R { return 0 }`,
    )
    const declaration = analyzed.modules.at(0)?.declarations.at(0)

    assert.strictEqual(
      declaration === undefined ? undefined : Presentation.functionDeclaration(declaration).text,
      'fn forward<E, ?R>() -> i32 ! E ? R',
    )
  }),
)

it.effect('presents mutable owned parameter storage in the source declaration', () =>
  Effect.gen(function* () {
    const analyzed = yield* index(
      'representation-syntax/mutable-parameter-presentation',
      `struct Counter { value: i32 }
fn update(mut counter: Counter) -> Counter { return move counter }`,
    )
    const declaration = analyzed.modules.at(0)?.declarations.at(0)

    assert.strictEqual(
      declaration === undefined ? undefined : Presentation.functionDeclaration(declaration).text,
      'fn update(mut counter: Counter) -> Counter',
    )
  }),
)
