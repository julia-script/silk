import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'
import * as FormattedDocument from '../src/FormattedDocument.js'
import * as Formatter from '../src/Formatter.js'
import * as Layout from '../src/Layout.js'
import * as Lexer from '../src/Lexer.js'
import * as Mir from '../src/Mir.js'
import * as Ownership from '../src/Ownership.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import * as Type from '../src/Type.js'

const source = `fn identity<T>(value: T) -> T { return move value }
pub fn main() -> i32 {
  let flag = identity(true)
  return identity<i32>(42)
}`

const file = SourceFile.make('generics/Main', new TextEncoder().encode(source))

const descendants = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
  node.children.flatMap(
    (child): ReadonlyArray<SyntaxTree.Node> =>
      SyntaxTree.isNode(child) ? [child, ...descendants(child)] : [],
  )

it('parses declaration parameters and explicit call specialization losslessly', () => {
  const syntax = Parser.parse(Lexer.lex(file))
  const kinds = descendants(syntax.root).map((node) => node.kind)

  assert.include(kinds, 'TypeParameterList')
  assert.include(kinds, 'TypeParameter')
  assert.include(kinds, 'CallTypeArgumentList')
  assert.deepEqual(syntax.parserDiagnostics, [])
})

it('keeps generic angles contextual and recovers damaged lists deterministically', () => {
  const comparisons = Parser.parse(
    Lexer.lex(
      SourceFile.make(
        'generics/Comparison',
        new TextEncoder().encode('pub fn main() -> i32 { if 1 < 2 { return 42 } return 0 }'),
      ),
    ),
  )
  assert.notInclude(
    descendants(comparisons.root).map((node) => node.kind),
    'CallTypeArgumentList',
  )
  assert.deepEqual(comparisons.parserDiagnostics, [])

  const missingArgument = Parser.parse(
    Lexer.lex(
      SourceFile.make(
        'generics/MissingArgument',
        new TextEncoder().encode(
          'fn identity<T>(value: T) -> T { return move value }\npub fn main() -> i32 { return identity<>(1) }',
        ),
      ),
    ),
  )
  assert.include(
    descendants(missingArgument.root).map((node) => node.kind),
    'CallTypeArgumentList',
  )
  assert.include(
    missingArgument.parserDiagnostics.map((diagnostic) => diagnostic.code),
    'PAR0001',
  )

  const missingClose = Parser.parse(
    Lexer.lex(
      SourceFile.make(
        'generics/MissingClose',
        new TextEncoder().encode(
          'struct Box<T> { value: T }\nfn broken(value: Box<i32) -> i32 { return 0 }',
        ),
      ),
    ),
  )
  assert.include(
    missingClose.parserDiagnostics.map((diagnostic) => diagnostic.code),
    'PAR0001',
  )
})

it.effect('formats generic declarations, applications, and calls idempotently', () =>
  Effect.gen(function* () {
    const syntax = Parser.parse(
      Lexer.lex(
        SourceFile.make(
          'generics/format',
          new TextEncoder().encode(
            'struct Box < T >{value:T}\nfn keep < T >(value:Box < T >)->Box<T>{return identity < T >(value)}',
          ),
        ),
      ),
    )
    const formatted = yield* Formatter.format(syntax)
    const text = new TextDecoder().decode(FormattedDocument.toUint8Array(formatted))
    assert.strictEqual(
      text,
      'struct Box<T> {\n  value: T\n}\n\nfn keep<T>(value: Box<T>) -> Box<T> {\n  return identity<T>(value)\n}\n',
    )
    const again = yield* Formatter.format(
      Parser.parse(Lexer.lex(SourceFile.make('generics/format', new TextEncoder().encode(text)))),
    )
    assert.strictEqual(new TextDecoder().decode(FormattedDocument.toUint8Array(again)), text)
  }),
)

it.effect('infers and explicitly selects finite concrete instances before MIR', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.make({ root: file }).pipe(
      Effect.provide(SourceResolver.memory(new Map())),
    )

    assert.deepEqual(snapshot.diagnostics, [])
    assert.deepEqual(
      snapshot.instances.instances.map((instance) => ({
        name: instance.key.declaration.name,
        arguments: instance.key.typeArguments.map(Type.encode),
      })),
      [
        { name: 'main', arguments: [] },
        { name: 'identity', arguments: ['bool'] },
        { name: 'identity', arguments: ['i32'] },
      ],
    )
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.mir._tag !== 'Available') return
    assert.strictEqual(snapshot.mir.value.functions.length, 3)
    assert.notInclude(Mir.encode(snapshot.mir.value), 'TypeParameter')
    const outcome = BootstrapEvaluation.evaluate(snapshot.instances, snapshot.mir.value)
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome))
    if (outcome._tag === 'Completed') {
      assert.strictEqual(outcome.result.value, 42)
      assert.deepEqual(
        outcome.trace.flatMap((event) =>
          event._tag === 'Call' && event.target.name === 'identity'
            ? [event.targetInstance.typeArguments.map(Type.encode)]
            : [],
        ),
        [['bool'], ['i32']],
      )
    }
  }),
)

it.effect('uses complete explicit arguments as value-argument context', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'generics/ExplicitContext',
      new TextEncoder().encode(`struct Left { value: i32 }
struct Right { value: i32 }
fn accept<T>(value: T) -> i32 { return 42 }
pub fn main() -> i32 {
  let empty = accept<[i32; 0]>([])
  return accept<Left | Right>(Left { value: empty })
}`),
    )
    assert.deepEqual(snapshot.diagnostics, [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('retains unresolved type-argument causes without fabricating arity failures', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'generics/UnresolvedArgument',
      new TextEncoder().encode(
        'struct Box<T> { value: T }\nfn bad(value: Box<Missing>) -> i32 { return 0 }\npub fn main() -> i32 { return 42 }',
      ),
    )
    const codes = snapshot.diagnostics.map((diagnostic) => diagnostic.code)
    assert.include(codes, 'SEM0001')
    assert.notInclude(codes, 'SEM0051')
  }),
)

it.effect('does not fabricate a second identity for duplicate type parameters', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'generics/DuplicateIdentity',
      new TextEncoder().encode(
        'fn bad<T, T>(value: T) -> T { return move value }\npub fn main() -> i32 { return 42 }',
      ),
    )
    const declaration = Analysis.genericDeclarationsOf(snapshot).at(0)
    const first = declaration?.typeParameters.at(0)
    const duplicate = declaration?.typeParameters.at(1)
    assert.notStrictEqual(first, undefined)
    assert.strictEqual(duplicate?.type, first?.type)
    assert.strictEqual(duplicate?.duplicateOf, first?.type)
  }),
)

it.effect('keeps open cleanup symbolic and specializes it before MIR', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'generics/Cleanup',
      new TextEncoder().encode(`struct Payload {}
fn discard<T>(value: T) -> i32 { return 42 }
pub fn main() -> i32 { return discard<Payload>(Payload {}) }`),
    )
    assert.deepEqual(snapshot.diagnostics, [])
    const ownership = Analysis.ownershipOf(snapshot, 'generics/Cleanup')
    assert.notStrictEqual(ownership, undefined)
    if (ownership !== undefined) {
      assert.include(Ownership.encode(ownership), 'release p0')
      const discard = ownership.functions.find(
        (fn) =>
          fn.declaration.canonical._tag === 'Canonical' &&
          fn.declaration.canonical.id.name === 'discard',
      )
      assert.strictEqual(discard?.exits.at(0)?.releases.at(0)?.cleanup._tag, 'ParameterCleanup')
    }
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.mir._tag !== 'Available') return
    assert.notInclude(Mir.encode(snapshot.mir.value), 'TypeParameter')
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
  }),
)

it.effect('lays out and evaluates only the reached applied struct', () =>
  Effect.gen(function* () {
    const structFile = SourceFile.make(
      'generics/Box',
      new TextEncoder().encode(`struct Box<T> { value: T }
pub fn main() -> i32 {
  let box = Box<i32> { value: 42 }
  return box.value
}`),
    )
    const snapshot = yield* Analysis.make({ root: structFile }).pipe(
      Effect.provide(SourceResolver.memory(new Map())),
    )

    assert.deepEqual(snapshot.diagnostics, [])
    assert.strictEqual(snapshot.layout._tag, 'Available')
    if (snapshot.layout._tag !== 'Available' || snapshot.mir._tag !== 'Available') return
    assert.deepEqual(
      snapshot.layout.value.entries.map((entry) => Type.encode(entry.type)),
      ['i32', 'generics/Box.Box<i32>'],
    )
    const outcome = BootstrapEvaluation.evaluate(snapshot.instances, snapshot.mir.value)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('resolves inferred and explicit generic calls across module namespaces', () =>
  Effect.gen(function* () {
    const root = SourceFile.make(
      'app/Main',
      new TextEncoder().encode(`import library.Generic
pub fn main() -> i32 { return Generic.identity<i32>(Generic.identity(42)) }`),
    )
    const snapshot = yield* Analysis.make({ root }).pipe(
      Effect.provide(
        SourceResolver.memory(
          new Map([
            [
              'library/Generic',
              new TextEncoder().encode('pub fn identity<T>(value: T) -> T { return move value }'),
            ],
          ]),
        ),
      ),
    )

    assert.deepEqual(snapshot.diagnostics, [])
    assert.strictEqual(snapshot.instances.instances.length, 2)
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.mir._tag !== 'Available') return
    const outcome = BootstrapEvaluation.evaluate(snapshot.instances, snapshot.mir.value)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('cuts off recursive generic calls that change an ancestor specialization', () =>
  Effect.gen(function* () {
    const recursive = SourceFile.make(
      'generics/Recursive',
      new TextEncoder().encode(`fn expand<T>(value: T) -> i32 {
  return expand<[T; 1]>([move value])
}
pub fn main() -> i32 { return expand<i32>(1) }`),
    )
    const snapshot = yield* Analysis.make({ root: recursive }).pipe(
      Effect.provide(SourceResolver.memory(new Map())),
    )

    assert.strictEqual(snapshot.instances.violations.length, 1)
    assert.deepEqual(snapshot.instances.violations.at(0)?.target.typeArguments.map(Type.encode), [
      'Array<i32, 1>',
    ])
    assert.strictEqual(snapshot.instances.instances.length, 2)
    assert.deepEqual(
      snapshot.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0053'],
    )
    assert.strictEqual(snapshot.layout._tag, 'Unavailable')
    assert.strictEqual(snapshot.mir._tag, 'Unavailable')
  }),
)

it.effect('keeps same-argument generic recursion finite and omits unused declarations', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'generics/SameRecursion',
      new TextEncoder().encode(`fn recurse<T>(value: T) -> i32 {
  if false { return recurse<T>(move value) }
  return 42
}
fn unused<T>(value: T) -> T { return move value }
pub fn main() -> i32 { return recurse<i32>(1) }`),
    )
    assert.deepEqual(snapshot.diagnostics, [])
    assert.deepEqual(
      snapshot.instances.instances.map((instance) => instance.key.declaration.name),
      ['main', 'recurse'],
    )
    assert.deepEqual(snapshot.instances.violations, [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('detects parameter-changing recursion across a mutual generic cycle', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'generics/MutualRecursion',
      new TextEncoder().encode(`fn first<T>(value: T) -> i32 {
  return second<[T; 1]>([move value])
}
fn second<U>(value: U) -> i32 {
  return first<U>(move value)
}
pub fn main() -> i32 { return first<i32>(1) }`),
    )
    assert.deepEqual(
      snapshot.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0053'],
    )
    assert.strictEqual(snapshot.instances.violations.length, 1)
    assert.strictEqual(snapshot.layout._tag, 'Unavailable')
    assert.strictEqual(snapshot.mir._tag, 'Unavailable')
  }),
)

it.effect('checks repeated instances under every recursive ancestor context', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'generics/ContextualRecursion',
      new TextEncoder().encode(`fn a<T>(value: T) -> i32 { return x<T>(move value) }
fn x<T>(value: T) -> i32 {
  if false { return a<bool>(true) }
  return 0
}
pub fn main() -> i32 {
  let first = a<i32>(1)
  let second = a<bool>(true)
  return x<i32>(first + second)
}`),
    )
    assert.include(
      snapshot.diagnostics.map((diagnostic) => diagnostic.code),
      'SEM0053',
    )
    assert.isAtLeast(snapshot.instances.violations.length, 1)
    assert.strictEqual(snapshot.layout._tag, 'Unavailable')
    assert.strictEqual(snapshot.mir._tag, 'Unavailable')
  }),
)

const invalidCases: ReadonlyArray<readonly [string, string, string]> = [
  [
    'duplicate parameter',
    'fn bad<T, T>(value: T) -> T { return move value }\npub fn main() -> i32 { return 0 }',
    'SEM0050',
  ],
  [
    'unbound parameter',
    'fn bad<T>(value: U) -> T { return move value }\npub fn main() -> i32 { return 0 }',
    'SEM0001',
  ],
  [
    'missing nominal arguments',
    'struct Box<T> { value: T }\nfn bad(value: Box) -> i32 { return 0 }\npub fn main() -> i32 { return 0 }',
    'SEM0051',
  ],
  [
    'non-generic nominal application',
    'struct Plain { value: i32 }\nfn bad(value: Plain<i32>) -> i32 { return 0 }\npub fn main() -> i32 { return 0 }',
    'SEM0051',
  ],
  [
    'excess explicit arguments',
    'fn id<T>(value: T) -> T { return move value }\npub fn main() -> i32 { return id<i32, bool>(1) }',
    'SEM0051',
  ],
  [
    'non-generic builtin specialization',
    'pub fn main() -> i32 { return i32.add<i32>(40, 2) }',
    'SEM0051',
  ],
  [
    'conflicting inference',
    'fn same<T>(left: T, right: T) -> T { return move left }\npub fn main() -> i32 { return same(1, true) }',
    'SEM0052',
  ],
  [
    'return-only inference',
    'fn make<T>() -> T {}\npub fn main() -> i32 { return make() }',
    'SEM0052',
  ],
  [
    'concrete-only operation in an open body',
    'fn addOne<T>(value: T) -> i32 { return i32.add(move value, 1) }\npub fn main() -> i32 { return addOne<i32>(41) }',
    'SEM0012',
  ],
]

for (const [name, text, code] of invalidCases) {
  it.effect(`diagnoses ${name} before target-dependent phases`, () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSource(
        `generics/invalid/${name.replaceAll(' ', '-')}`,
        new TextEncoder().encode(text),
      )
      assert.include(
        snapshot.diagnostics.map((diagnostic) => diagnostic.code),
        code,
      )
      assert.strictEqual(snapshot.instances.instances.length, 0)
      assert.strictEqual(snapshot.layout._tag, 'Unavailable')
      assert.strictEqual(snapshot.mir._tag, 'Unavailable')
    }),
  )
}

it.effect(
  'substitutes generic nominal pattern fields without rechecking the declaration body',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSource(
        'generics/Pattern',
        new TextEncoder().encode(`struct Box<T> { value: T }
fn take<T>(input: Box<T>) -> T {
  return match move input { Box<T> { value } => move value }
}
pub fn main() -> i32 { return take(Box<i32> { value: 42 }) }`),
      )
      assert.deepEqual(snapshot.diagnostics, [])
      const outcome = Analysis.evaluate(snapshot)
      assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome))
      if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
    }),
)

it.effect('substitutes cleanup through applied pattern paths and omitted fields', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'generics/PatternCleanup',
      new TextEncoder().encode(`struct Token { value: i32 }
struct Pair<A, B> { first: A second: B }
fn take<T>(pair: Pair<i32, T>) -> i32 {
  return match move pair { Pair<i32, T> { first, .. } => first }
}
pub fn main() -> i32 {
  return take<Token>(Pair<i32, Token> { first: 42, second: Token { value: 0 } })
}`),
    )
    assert.deepEqual(snapshot.diagnostics, [])
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.mir._tag !== 'Available') return
    const cleanup = snapshot.mir.value.functions
      .flatMap((fn) => fn.regions)
      .flatMap((region) => (region._tag === 'OperationRegion' ? region.operations : []))
      .flatMap(Mir.operationTree)
      .flatMap((operation) =>
        operation._tag === 'Match'
          ? operation.arms.flatMap((arm) => arm.selected.cleanup.map((entry) => entry.cleanup))
          : [],
      )
    assert.include(
      cleanup.map((entry) => entry._tag),
      'StructCleanup',
    )
    assert.include(
      cleanup.map((entry) => Type.encode(entry.type)),
      'generics/PatternCleanup.Token',
    )
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('classifies generic writes after substituting the concrete element type', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'generics/Write',
      new TextEncoder().encode(`fn replace<T>(values: [T; 1], value: T) -> i32 {
  let mut result = move values
  result[0] = move value
  return 42
}
pub fn main() -> i32 { return replace<i32>([1], 2) }`),
    )
    assert.deepEqual(snapshot.diagnostics, [])
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.mir._tag !== 'Available') return
    const replacements = snapshot.mir.value.functions
      .flatMap((fn) => fn.regions)
      .flatMap((region) => (region._tag === 'OperationRegion' ? region.operations : []))
      .flatMap(Mir.operationTree)
      .flatMap((operation) => (operation._tag === 'WritePlace' ? [operation.replacement] : []))
    assert.deepEqual(replacements, ['Copy'])
  }),
)

it.effect('links an open HIR call through every reached caller instance', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'generics/Facade',
      new TextEncoder().encode(`fn inner<T>(value: T) -> T { return move value }
fn outer<T>(value: T) -> T { return inner<T>(move value) }
pub fn main() -> i32 {
  let flag = outer(true)
  if flag { return outer(42) }
  return 0
}`),
    )
    const call = Analysis.genericCallsOf(snapshot).find(
      (candidate) => candidate.target.name === 'inner',
    )
    assert.notStrictEqual(call, undefined)
    if (call === undefined) return
    assert.deepEqual(
      Analysis.instancesOfCall(snapshot, call).map((link) =>
        link.target.key.typeArguments.map(Type.encode),
      ),
      [['bool'], ['i32']],
    )
  }),
)

it.effect('rejects residual open MIR and keeps specialization symbols injective', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'generics/MirBoundary',
      new TextEncoder().encode(source),
    )
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.mir._tag !== 'Available') return
    const fn = snapshot.mir.value.functions.at(0)
    assert.notStrictEqual(fn, undefined)
    if (fn === undefined) return
    const parameter = Type.parameter({ module: 'malformed', name: 'fn' }, 0, 'T')
    const malformed = Object.freeze({
      ...snapshot.mir.value,
      functions: Object.freeze([
        Object.freeze({
          ...fn,
          instance: Object.freeze({ ...fn.instance, typeArguments: Object.freeze([parameter]) }),
        }),
      ]),
    })
    assert.include(
      Mir.verify(malformed).map((violation) => violation.rule),
      'InvalidInstance',
    )

    const collision = (module: string): Mir.MirFunction => {
      const declaration = Object.freeze({
        _tag: 'CanonicalDeclarationId' as const,
        module,
        name: 'same',
      })
      return Object.freeze({
        ...fn,
        id: declaration,
        instance: Object.freeze({ ...fn.instance, declaration }),
      })
    }
    assert.notStrictEqual(
      Backend.symbolFor(collision('a/b'), 1),
      Backend.symbolFor(collision('a_b'), 1),
    )
  }),
)

it.effect('emits the same concrete specialization set through LLVM and WebAssembly', () =>
  Effect.gen(function* () {
    const text = `struct Pair { left: i32 right: i32 }
struct Box<T> { value: T }
fn identity<T>(value: T) -> T { return move value }
pub fn main() -> i32 {
  let scalar = Box<i32> { value: identity(0) }
  let pair = Box<Pair> { value: identity<Pair>(Pair { left: 40, right: 2 }) }
  return scalar.value + pair.value.left + pair.value.right
}`
    const native = yield* Analysis.ofSource(
      'generics/Backend',
      new TextEncoder().encode(text),
      'aarch64-apple-darwin',
    )
    const wasm = yield* Analysis.ofSource(
      'generics/Backend',
      new TextEncoder().encode(text),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(native.diagnostics, [])
    assert.deepEqual(wasm.diagnostics, [])
    const nativeArtifact = yield* Analysis.codegen(native, { mode: 'release' })
    const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    assert.deepEqual(
      nativeArtifact.symbols.map((entry) => entry.symbol),
      wasmArtifact.symbols.map((entry) => entry.symbol),
    )
    assert.deepEqual(
      wasmArtifact.symbols.map((entry) => entry.instance.typeArguments.map(Type.encode)),
      [[], ['i32'], ['generics/Backend.Pair']],
    )
    assert.deepEqual(
      native.layout._tag === 'Available'
        ? native.layout.value.entries
            .map((entry) => Type.encode(entry.type))
            .filter((type) => type.includes('Box<'))
        : [],
      ['generics/Backend.Box<i32>', 'generics/Backend.Box<generics/Backend.Pair>'],
    )
    if (native.layout._tag === 'Available') {
      const plan = native.layout.value
      const boxes = plan.entries.filter(
        (entry) => Type.isNominal(entry.type) && entry.type.name === 'Box',
      )
      assert.deepEqual(
        boxes.map((entry) => entry.size),
        [4, 8],
      )
      assert.deepEqual(
        boxes.map((entry) => Layout.callingShape(plan, entry.type)?.laneCount),
        [1, 2],
      )
    }
    const instance = new WebAssembly.Instance(
      new WebAssembly.Module(wasmArtifact.bytes.slice()),
      {},
    )
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)
