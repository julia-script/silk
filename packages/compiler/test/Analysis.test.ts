import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ConformanceProof from '../src/ConformanceProof.js'
import * as ExecutionAffinity from '../src/ExecutionAffinity.js'
import * as Hir from '../src/Hir.js'
import * as LocalSharedOwnership from '../src/LocalSharedOwnership.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Type from '../src/Type.js'
import { invalidMatchCorpus } from './support/corpus.js'
import * as Projections from './support/projections.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (
  rootModule: string,
  entries: ReadonlyArray<readonly [string, string]>,
): Effect.Effect<Analysis.Snapshot> => {
  const rootText = entries.find(([name]) => name === rootModule)?.[1]
  if (rootText === undefined) throw new RangeError(`Fixture has no root source ${rootModule}`)
  const imports = new Map(
    entries
      .filter(([name]) => name !== rootModule)
      .map(([name, text]) => [name, ascii(text)] as const),
  )
  return Analysis.makeRealized({ root: SourceFile.make(rootModule, ascii(rootText)) }).pipe(
    Effect.provide(SourceResolver.memory(imports)),
  )
}

it.effect('answers multi-module queries from one snapshot', () =>
  Effect.gen(function* () {
    const self = yield* snapshot('root', [
      ['root', 'import lib\npub fn main() -> i32 { return 42 }'],
      ['lib', 'pub fn answer() -> i32 { return 1 }'],
    ])
    assert.deepEqual(
      Analysis.modules(self).map((module) => module.name),
      ['lib', 'root'],
    )
    assert.strictEqual(Projections.syntaxOf(self, 'lib')?.source.id, 'lib')
    assert.strictEqual(Analysis.declarationByName(self, 'lib', 'answer')._tag, 'Resolved')
    assert.strictEqual(Analysis.declarationByName(self, 'root', 'answer')._tag, 'Missing')
    assert.strictEqual(Projections.hirOf(self, 'root')?.functions.length, 1)
    assert.strictEqual(Analysis.moduleAnalysis(self, 'absent'), undefined)
    assert.deepEqual(Analysis.cycles(self), [])
    assert.deepEqual([...Analysis.sources(self).keys()], ['lib', 'root'])
  }),
)

it.effect('constructs frontend snapshots for deterministic damaged-source edits', () =>
  Effect.gen(function* () {
    const accepted = `pub fn inspect(value: i32) -> i32 {
  return match value { Token {} => 0 }
}`
    const damaged = [
      accepted.slice(0, accepted.indexOf('Token')),
      accepted.replace('Token {}', '['),
      accepted.replace('Token {}', '&'),
      accepted.replace('Token {}', '('),
      accepted.replace('Token {}', 'fn() -> i32 {}'),
      accepted.replace('Token {}', 'Without<i32, i32> {}'),
      accepted.replace('Token {}', 'Token {'),
      accepted.replace('=> 0', ''),
    ]

    for (const [ordinal, source] of damaged.entries()) {
      const self = yield* Analysis.make({
        root: SourceFile.make(`damaged-${ordinal}`, ascii(source)),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(Analysis.rootAnalysis(self).syntax.source.id, `damaged-${ordinal}`)
    }
  }),
)

it.effect('resolves imported declarations as stored callable values', () =>
  Effect.gen(function* () {
    const source =
      'import lib as Lib\npub fn main() -> i32 { let callback = Lib.identity return callback(42) }'
    const self = yield* snapshot('root', [
      ['root', source],
      ['lib', 'pub fn identity(value: i32) -> i32 { return value }'],
    ])
    const root = self.results.get('root')
    const main = root?.functions.at(0)
    const binding = main?.statements.at(0)

    assert.strictEqual(
      binding?._tag === 'BindStatement' ? binding.binding.initializer._tag : undefined,
      'FunctionItem',
    )
    assert.strictEqual(main?.returnedExpression._tag, 'CallableApply')
    assert.strictEqual(
      Analysis.semanticOccurrenceAt(self, 'root', source.indexOf('identity'))?.resolution._tag,
      'Available',
    )
    assert.strictEqual(
      Analysis.semanticOccurrenceAt(self, 'root', source.lastIndexOf('callback'))?.resolution._tag,
      'Available',
    )
    assert.deepEqual(root?.diagnostics, [])
  }),
)

it.effect('indexes automatic section targets and piped callable bindings', () =>
  Effect.gen(function* () {
    const source = `fn add(value: i32, amount: i32) -> i32 { return value + amount }
pub fn main() -> i32 { let increment = add(2) return 40 |> increment }`
    const self = yield* Analysis.ofSourceRealized('main', ascii(source))

    assert.strictEqual(
      Analysis.semanticOccurrenceAt(self, 'main', source.lastIndexOf('add'))?.resolution._tag,
      'Available',
    )
    assert.strictEqual(
      Analysis.semanticOccurrenceAt(self, 'main', source.lastIndexOf('increment'))?.resolution._tag,
      'Available',
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
  }),
)

it.effect('retains inaccessible imported callable targets without inventing a value lookup', () =>
  Effect.gen(function* () {
    const self = yield* snapshot('root', [
      ['root', 'import lib as Lib\npub fn main() -> i32 { let callback = Lib.hidden return 0 }'],
      ['lib', 'fn hidden(value: i32) -> i32 { return value }'],
    ])
    const root = self.results.get('root')
    const binding = root?.functions.at(0)?.statements.at(0)
    const initializer = binding?._tag === 'BindStatement' ? binding.binding.initializer : undefined

    assert.strictEqual(initializer?._tag, 'FunctionItem')
    assert.strictEqual(
      initializer?._tag === 'FunctionItem' ? initializer.reference._tag : undefined,
      'Missing',
    )
    assert.include(root?.diagnostics.map((diagnostic) => diagnostic.code) ?? [], 'SEM0015')
  }),
)

it.effect('merges diagnostics while keeping unrelated facts queryable', () =>
  Effect.gen(function* () {
    const self = yield* snapshot('root', [
      ['root', 'import lib\nimport missing\npub fn main( -> Mystery { return @ 42 }'],
      ['lib', 'pub fn answer() -> i32 { return 1 }'],
    ])
    assert.strictEqual(Analysis.declarationByName(self, 'lib', 'answer')._tag, 'Resolved')
    const libFunction = Projections.hirOf(self, 'lib')?.functions.at(0)
    assert.strictEqual(
      libFunction === undefined ? undefined : Hir.returned(libFunction)._tag,
      'IntegerLiteral',
    )
    assert.include(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      'MOD0001',
    )
  }),
)

it.effect('answers repeated snapshots deterministically', () =>
  Effect.gen(function* () {
    const entries: ReadonlyArray<readonly [string, string]> = [
      ['root', 'import lib\npub fn main() -> i32 { return 42 }'],
      ['lib', 'pub fn same() -> i32 { return 1 }\npub fn same() -> i32 { return 2 }'],
    ]
    const first = yield* snapshot('root', entries)
    const second = yield* snapshot('root', [...entries].reverse())
    assert.deepEqual(first.closure, second.closure)
    assert.deepEqual(first.index, second.index)
    assert.deepEqual(Analysis.diagnostics(first), Analysis.diagnostics(second))
  }),
)

it.effect('evaluates and answers ownership through the single-source convenience', () =>
  Effect.gen(function* () {
    const evaluated = yield* Analysis.ofSourceRealized(
      'memory/facade',
      ascii('pub fn main() -> i32 { return 42 }'),
    )
    const outcome = Analysis.evaluate(evaluated)
    assert.strictEqual(outcome._tag, 'Completed')

    const owned = yield* Analysis.ofSourceRealized(
      'memory/ownership',
      ascii('pub fn identity(value: i32) -> i32 { return value }'),
    )
    const facts = Analysis.ownershipOf(owned, 'memory/ownership')
    assert.strictEqual(facts?.functions.at(0)?.verdict._tag, 'Satisfied')
    assert.strictEqual(facts?.functions.at(0)?.bindings.at(0)?.category._tag, 'Copyable')
  }),
)

it.effect('reports only actionable diagnostics for empty and final-expression sources', () =>
  Effect.gen(function* () {
    const empty = yield* Analysis.ofSourceRealized('memory/empty', new Uint8Array())
    assert.deepEqual(Analysis.diagnostics(empty), [])
    assert.deepEqual(Analysis.rootAnalysis(empty).functions, [])

    const recovered = yield* Analysis.ofSourceRealized(
      'memory/recovered-return',
      ascii('pub fn main() -> i32 { foo }'),
    )
    assert.deepEqual(
      Analysis.diagnostics(recovered).map((diagnostic) => ({
        code: diagnostic.code,
        message: diagnostic.message,
      })),
      [
        { code: 'SEM0006', message: 'Unknown value foo' },
        { code: 'SEM0130', message: 'A reachable path must return i32' },
      ],
    )
  }),
)

it.effect('answers stable match facts across semantic, HIR, ownership, MIR, and trace phases', () =>
  Effect.gen(function* () {
    const source = `struct Left { value: i32 }
struct Right { value: i32 }
fn inspect(input: Left | Right) -> i32 {
  return match &input {
    Left { value } if false => 0
    Left { value: answer } => answer
    Right { value } => value
  }
}
pub fn main() -> i32 { return inspect(Left { value: 42 }) }`
    const first = yield* Analysis.ofSourceRealized(
      'memory/match-facade',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    const second = yield* Analysis.ofSourceRealized(
      'memory/match-facade',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    const answer = (self: Analysis.Snapshot) => ({
      semantic: Projections.matchesOf(self, 'memory/match-facade'),
      hir: Projections.hirMatchesOf(self, 'memory/match-facade'),
      ownership: Analysis.ownershipMatchesOf(self, 'memory/match-facade'),
      mir: Analysis.mirMatchesOf(self),
      trace: Projections.traceOf(Analysis.evaluate(self)).filter((event) =>
        event._tag.startsWith('Match'),
      ),
    })

    assert.deepEqual(answer(first), answer(second))
    assert.strictEqual(answer(first).semantic.at(0)?.arms.length, 3)
    assert.strictEqual(answer(first).hir.at(0)?.arms.at(0)?.guard?._tag, 'BooleanLiteral')
    assert.strictEqual(answer(first).ownership.at(0)?.arms.at(0)?.provisionalGuard, true)
    assert.strictEqual(answer(first).mir.at(0)?.decisions.length, 2)
    assert.strictEqual(answer(first).trace.at(-1)?._tag, 'MatchBorrowEnd')
  }),
)

it.effect('answers immutable fixed-array facts across semantics, layout, MIR, and evaluation', () =>
  Effect.gen(function* () {
    const source = `fn choose(values: [i32; 2], index: usize) -> i32 { return values[index] }
pub fn main() -> i32 { return choose([10, 42], 1) }`
    const self = yield* Analysis.ofSourceRealized(
      'memory/array-facade',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    const types = Analysis.fixedArrayTypesOf(self, 'memory/array-facade')
    const literals = Analysis.arrayLiteralsOf(self, 'memory/array-facade')
    const indexes = Analysis.indexProjectionsOf(self, 'memory/array-facade')
    const layouts = Analysis.repeatedLayoutsOf(self)
    const shapes = Analysis.arrayCallingShapesOf(self)
    const outcome = Analysis.evaluate(self)
    const events = Projections.arrayTraceEventsOf(outcome)

    assert.deepEqual(types.map(Type.encode), ['Array<i32, 2>'])
    assert.strictEqual(literals.at(0)?.state._tag, 'Complete')
    assert.strictEqual(literals.at(0)?.elements.length, 2)
    assert.strictEqual(indexes.at(0)?.bounds._tag, 'Runtime')
    assert.strictEqual(layouts.at(0)?.representation._tag, 'Repeated')
    assert.deepEqual(
      shapes.at(0)?.lanes.map((lane) => lane.path.at(0)?._tag),
      ['ElementSelector', 'ElementSelector'],
    )
    assert.deepEqual(
      events.map((event) => event._tag),
      ['ArrayConstruct', 'PlaceRead'],
    )
    assert.strictEqual(Object.isFrozen(types), true)
    assert.strictEqual(Object.isFrozen(literals), true)
    assert.strictEqual(Object.isFrozen(events), true)
  }),
)

it.effect(
  'answers nominal declaration, field, dependency, and layout facts through the facade',
  () =>
    Effect.gen(function* () {
      const self = yield* Analysis.ofSourceRealized(
        'memory/nominal-facade',
        ascii('struct Pair { left: i32 right: bool }\npub fn main() -> i32 { return 42 }'),
        'aarch64-apple-darwin',
      )
      const lookup = Analysis.structByName(self, 'memory/nominal-facade', 'Pair')
      assert.strictEqual(
        Analysis.memberByName(self, 'memory/nominal-facade', 'Pair')._tag,
        'Resolved',
      )
      assert.strictEqual(lookup._tag, 'Resolved')
      if (lookup._tag !== 'Resolved') return
      assert.strictEqual(Analysis.fieldByName(lookup.declaration, 'right')._tag, 'Resolved')
      assert.strictEqual(lookup.declaration.dependency._tag, 'Available')
      const layout = Analysis.nominalLayout(self, Type.nominal('memory/nominal-facade', 'Pair'))
      assert.strictEqual(layout?._tag, 'LayoutEntry')
    }),
)

it.effect(
  'keeps cross-module nominal identity and recursive unavailability queryable through the facade',
  () =>
    Effect.gen(function* () {
      const self = yield* snapshot('app/Main', [
        [
          'app/Main',
          'import model.Tree { Node }\nstruct Root { node: Node }\nstruct Loop { next: Loop }\npub fn main() -> i32 { return 42 }',
        ],
        ['model/Tree', 'pub struct Node { value: i32 }'],
      ])
      const root = Analysis.structByName(self, 'app/Main', 'Root')
      const loop = Analysis.structByName(self, 'app/Main', 'Loop')
      assert.strictEqual(root._tag, 'Resolved')
      assert.strictEqual(loop._tag, 'Resolved')
      if (root._tag !== 'Resolved' || loop._tag !== 'Resolved') return
      const node = Analysis.fieldByName(root.declaration, 'node')
      assert.strictEqual(node._tag, 'Resolved')
      if (node._tag !== 'Resolved' || node.field.declaredType._tag !== 'Resolved') return
      assert.deepEqual(node.field.declaredType.type, {
        _tag: 'NominalType',
        module: 'model/Tree',
        name: 'Node',
        arguments: [],
      })
      assert.strictEqual(
        Analysis.nominalLayout(self, Type.nominal('model/Tree', 'Node'))?._tag,
        'LayoutEntry',
      )
      const unavailable = Analysis.nominalLayout(self, Type.nominal('app/Main', 'Loop'))
      assert.strictEqual(unavailable?._tag, 'UnavailableLayoutEntry')
      assert.strictEqual(
        unavailable?._tag === 'UnavailableLayoutEntry' ? unavailable.cause?.code : undefined,
        'SEM0020',
      )
    }),
)

it.effect('emits clean snapshots and refuses diagnosed snapshots before the backend', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'memory/codegen',
      ascii('pub fn main() -> i32 { return 42 }'),
      'aarch64-apple-darwin',
    )
    const release = yield* Analysis.codegen(self, { mode: 'release' })
    assert.strictEqual(release._tag, 'LlvmBitcodeArtifact')
    assert.include(release.ir, 'silk_main')

    const invalid = yield* Analysis.ofSourceRealized(
      'memory/invalid',
      ascii('pub fn main() -> Mystery { return 42 }'),
      'aarch64-apple-darwin',
    )
    const blocked = yield* Effect.result(Analysis.codegen(invalid, { mode: 'release' }))
    assert.strictEqual(blocked._tag, 'Failure')
    if (blocked._tag === 'Failure') assert.strictEqual(blocked.failure._tag, 'CodegenUnavailable')
  }),
)

it.effect('preserves one exact target and layout plan across facade queries and MIR', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'memory/plan',
      ascii(
        'import silk.i32 as i32\npub fn main() -> i32 { if i32.equals(1, 1) { return 42 } return 0 }',
      ),
      'wasm32-unknown-unknown',
    )
    const target = Analysis.targetOf(self)
    const catalog = Analysis.layoutCatalogOf(self)
    const layout = Analysis.layoutOf(self)
    const mir = Analysis.mirOf(self)

    assert.strictEqual(target._tag, 'Resolved')
    assert.strictEqual(catalog._tag, 'Available')
    assert.strictEqual(layout._tag, 'Available')
    assert.strictEqual(mir._tag, 'Available')
    if (
      target._tag !== 'Resolved' ||
      catalog._tag !== 'Available' ||
      layout._tag !== 'Available' ||
      mir._tag !== 'Available'
    )
      return
    assert.strictEqual(catalog.value.target, target.target)
    assert.strictEqual(layout.value.target, target.target)
    assert.strictEqual(mir.value.layout, layout.value)
    assert.deepEqual(
      layout.value.entries.map((entry) => [entry.type, entry.size, entry.alignment]),
      [
        ['bool', 4, 4],
        ['i32', 4, 4],
      ],
    )
  }),
)

it.effect('keeps unsupported targets explicit and queryable without manufacturing MIR', () =>
  Effect.gen(function* () {
    const unsupported = yield* Analysis.ofSourceRealized(
      'memory/unsupported',
      ascii('pub fn main() -> i32 { return 42 }'),
      'mips-unknown-none',
    )

    assert.strictEqual(Analysis.targetOf(unsupported)._tag, 'Unavailable')
    assert.strictEqual(Analysis.layoutCatalogOf(unsupported)._tag, 'Unavailable')
    assert.strictEqual(Analysis.layoutOf(unsupported)._tag, 'Unavailable')
    assert.strictEqual(Analysis.mirOf(unsupported)._tag, 'Unavailable')
  }),
)

it.effect('keeps invalid match corpus failures phase-owned and downstream facts queryable', () =>
  Effect.gen(function* () {
    for (const program of invalidMatchCorpus) {
      const self = yield* Analysis.ofSourceRealized(
        `memory/${program.name}`,
        ascii(program.source),
        'wasm32-unknown-unknown',
      )
      const codes = Analysis.diagnostics(self).map((diagnostic) => diagnostic.code)
      for (const code of program.codes) assert.include(codes, code, program.name)
      assert.isAtLeast(
        Projections.matchesOf(self, `memory/${program.name}`).length,
        1,
        program.name,
      )
    }
  }),
)

it.effect(
  'answers local, parameter, callable, field, and half-open semantic occurrence queries',
  () =>
    Effect.gen(function* () {
      const source = `struct Pair { left: i32 right: i32 }
fn identity(value: i32) -> i32 {
  let local = value
  let pair = Pair { left: local, right: 0 }
  return pair.left
}
pub fn main() -> i32 { return identity(42) }`
      const self = yield* Analysis.ofSourceRealized('main', ascii(source))
      const targetAt = (spelling: string, occurrence = 0) => {
        let offset = -1
        for (let index = 0; index <= occurrence; index += 1)
          offset = source.indexOf(spelling, offset + 1)
        return Analysis.semanticOccurrenceAt(self, 'main', offset)
      }

      assert.strictEqual(targetAt('value', 1)?.resolution._tag, 'Available')
      assert.strictEqual(targetAt('local', 1)?.resolution._tag, 'Available')
      assert.strictEqual(targetAt('Pair', 1)?.resolution._tag, 'Available')
      assert.strictEqual(targetAt('left', 2)?.resolution._tag, 'Available')
      assert.strictEqual(targetAt('identity', 1)?.resolution._tag, 'Available')

      const localReference = source.indexOf('local right')
      assert.strictEqual(
        Analysis.semanticOccurrenceAt(self, 'main', localReference + 'local'.length),
        undefined,
      )
      assert.strictEqual(
        Analysis.semanticOccurrenceAt(self, 'main', source.indexOf('return pair') - 1),
        undefined,
      )
    }),
)

it.effect('resolves imported and qualified declarations without spelling lookup', () =>
  Effect.gen(function* () {
    const root = `import lib { answer }
import other as tools
pub fn main() -> i32 { return answer() + tools.answer() }`
    const self = yield* snapshot('root', [
      ['root', root],
      ['lib', 'pub fn answer() -> i32 { return 42 }'],
      ['other', 'pub fn answer() -> i32 { return 7 }'],
    ])
    const selected = [
      Analysis.semanticOccurrenceAt(self, 'root', root.indexOf('answer()')),
      Analysis.semanticOccurrenceAt(self, 'root', root.lastIndexOf('answer()')),
    ]
    for (const [index, target] of selected.entries()) {
      assert.strictEqual(target?.resolution._tag, 'Available')
      if (target?.resolution._tag !== 'Available') continue
      assert.strictEqual(target.declaration?.module, index === 0 ? 'lib' : 'other')
      assert.strictEqual(target.declaration?.selectionSpan.start, 'pub fn '.length)
    }
  }),
)

it.effect('keeps unavailable and damaged semantic occurrences isolated and deterministic', () =>
  Effect.gen(function* () {
    const source =
      'fn valid(value: i32) -> i32 { return value }\nfn damaged( -> i32 { return missing() }'
    const first = yield* Analysis.ofSourceRealized('main', ascii(source))
    const second = yield* Analysis.ofSourceRealized('main', ascii(source))
    const availableOffset = source.indexOf('value }')
    const missingOffset = source.indexOf('missing')
    assert.deepEqual(
      Analysis.semanticOccurrenceAt(first, 'main', availableOffset),
      Analysis.semanticOccurrenceAt(second, 'main', availableOffset),
    )
    assert.strictEqual(
      Analysis.semanticOccurrenceAt(first, 'main', availableOffset)?.resolution._tag,
      'Available',
    )
    assert.strictEqual(
      Analysis.semanticOccurrenceAt(first, 'main', missingOffset)?.resolution._tag,
      'Missing',
    )
  }),
)

it.effect('constructs a frontend snapshot without runtime realization', () =>
  Effect.gen(function* () {
    const frontend = yield* Analysis.ofSource('main', ascii('pub fn main() -> i32 { return 42 }'))

    assert.deepEqual(
      Analysis.phases(frontend).map((entry) => entry.phase),
      [
        'closure',
        'declaration-collection',
        'declaration-index',
        'name-resolution',
        'module-surface',
        'elaboration',
        'ownership',
        'opaque-realization',
        'semantic-occurrences',
        'anonymous-expressions',
      ],
    )
    assert.strictEqual(Object.hasOwn(frontend, 'instances'), false)
    assert.strictEqual(Object.hasOwn(frontend, 'layout'), false)
    assert.strictEqual(Object.hasOwn(frontend, 'mir'), false)
    assert.deepEqual(Analysis.diagnostics(frontend), [])
  }),
)

it.effect('publishes sealed local-shared identity, affinity, and structural obligations', () =>
  Effect.gen(function* () {
    const source = `struct Token { value: i32 }
struct Generic<T> { value: T }
struct Pair<T> { first: Intrinsic.SharedCore<T> second: Intrinsic.SharedCore<T> }
struct LocalWrap<T> { core: Intrinsic.SharedCore<T> }
struct Damaged { first: Missing second: AlsoMissing }
struct Shared { value: i32 }
struct SharedCore { value: i32 }
struct Deferred { value: i32 }
struct Scheduler { value: i32 }
struct LocalRuntimeHandle { value: i32 }
impl Copy for LocalWrap<i32> {}
fn moveCore<T>(value: Intrinsic.SharedCore<T>) -> Intrinsic.SharedCore<T> { return move value }
fn duplicate(value: Intrinsic.SharedCore<i32>) -> Intrinsic.SharedCore<i32> { return value }
fn select(value: i32, core: Intrinsic.SharedCore<i32>) -> i32 { return value }
fn capture(core: Intrinsic.SharedCore<i32>) -> once fn(i32) -> i32 { return select(move core) }
fn moveCapture(core: Intrinsic.SharedCore<i32>) -> i32 {
  let first = select(move core)
  let second = move first
  drop second
  return 0
}
fn captureEffect(core: Intrinsic.SharedCore<i32>) -> i32 {
  let pending = effect { drop move core return 0 }
  drop pending
  return 0
}
fn moveEffect(core: Intrinsic.SharedCore<i32>) -> i32 {
  let first = effect { drop move core return 0 }
  let second = move first
  drop second
  return 0
}
pub fn main() -> i32 { return 0 }`
    const self = yield* Analysis.ofSource('main', ascii(source))
    const ownership = self.ownership.get('main')
    const moveCore = ownership?.functions.find(
      (fn) => fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'moveCore',
    )
    const binding = moveCore?.bindings.at(0)
    const openElement = Type.parameter({ module: 'main', name: 'moveCore' }, 0, 'T')
    const openCore = Type.sharedCore(openElement)

    assert.isTrue(binding?.type !== undefined && Type.equals(binding.type, openCore))
    assert.strictEqual(binding?.category._tag, 'MoveOnly')
    assert.strictEqual(binding?.executionAffinity._tag, 'LocalExecution')
    assert.strictEqual(
      LocalSharedOwnership.count(binding?.localSharedObligations ?? LocalSharedOwnership.none),
      1,
    )
    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['SEM0001', 'SEM0001', 'SEM0083', 'OWN0003'],
    )
    const duplicateRead = Analysis.diagnostics(self).find(
      (diagnostic) => diagnostic.code === 'OWN0003',
    )
    assert.strictEqual(
      duplicateRead === undefined
        ? undefined
        : source.slice(duplicateRead.span.start, duplicateRead.span.end).trim(),
      'value',
    )
    const duplicate = ownership?.functions.find(
      (fn) =>
        fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'duplicate',
    )
    assert.strictEqual(
      LocalSharedOwnership.count(
        duplicate?.bindings.at(0)?.localSharedObligations ?? LocalSharedOwnership.none,
      ),
      1,
    )

    const pair = Type.nominal('main', 'Pair', ['i32'])
    assert.isFalse(ConformanceProof.copyType(self.index, Type.sharedCore('i32')))
    assert.isFalse(
      ConformanceProof.copyType(self.index, Type.sharedCore(Type.nominal('main', 'Token'))),
    )
    assert.strictEqual(ExecutionAffinity.ofType(self.index, pair)._tag, 'LocalExecution')
    assert.strictEqual(LocalSharedOwnership.count(LocalSharedOwnership.ofType(self.index, pair)), 2)
    assert.strictEqual(
      LocalSharedOwnership.count(
        LocalSharedOwnership.ofType(self.index, Type.fixedArray(Type.sharedCore('i32'), 2)),
      ),
      2,
    )
    const capture = ownership?.functions.find(
      (fn) => fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'capture',
    )
    assert.strictEqual(capture?.callables.at(0)?.executionAffinity._tag, 'LocalExecution')
    assert.strictEqual(
      LocalSharedOwnership.count(
        capture?.callables.at(0)?.localSharedObligations ?? LocalSharedOwnership.none,
      ),
      1,
    )
    const moveCapture = ownership?.functions.find(
      (fn) =>
        fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'moveCapture',
    )
    const movedCallable = moveCapture?.bindings.find((candidate) => candidate.name === 'second')
    assert.strictEqual(movedCallable?.executionAffinity._tag, 'LocalExecution')
    assert.strictEqual(
      LocalSharedOwnership.count(
        movedCallable?.localSharedObligations ?? LocalSharedOwnership.none,
      ),
      1,
    )
    const captureEffect = ownership?.functions.find(
      (fn) =>
        fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'captureEffect',
    )
    const pending = captureEffect?.bindings.find((candidate) => candidate.name === 'pending')
    assert.strictEqual(pending?.executionAffinity._tag, 'LocalExecution')
    assert.strictEqual(
      LocalSharedOwnership.count(pending?.localSharedObligations ?? LocalSharedOwnership.none),
      1,
    )
    const moveEffect = ownership?.functions.find(
      (fn) =>
        fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'moveEffect',
    )
    const movedEffect = moveEffect?.bindings.find((candidate) => candidate.name === 'second')
    assert.strictEqual(movedEffect?.executionAffinity._tag, 'LocalExecution')
    assert.strictEqual(
      LocalSharedOwnership.count(movedEffect?.localSharedObligations ?? LocalSharedOwnership.none),
      1,
    )
    const union = Type.union([Type.sharedCore('i32'), Type.nominal('main', 'Token')])
    assert.strictEqual(union._tag, 'Normalized')
    if (union._tag !== 'Normalized' || !Type.isUnion(union.type)) return
    const unionObligations = LocalSharedOwnership.ofType(self.index, union.type)
    assert.strictEqual(unionObligations._tag, 'ActiveUnion')
    if (unionObligations._tag !== 'ActiveUnion') return
    assert.deepEqual(
      unionObligations.cases.map((entry, ordinal) => [
        Type.isSharedCore(entry.member),
        LocalSharedOwnership.count(unionObligations, ordinal),
      ]),
      [
        [false, 0],
        [true, 1],
      ],
    )

    const genericParameter = Type.parameter({ module: 'main', name: 'Generic' }, 0, 'T')
    const openGeneric = Type.nominal('main', 'Generic', [genericParameter])
    assert.strictEqual(ExecutionAffinity.ofType(self.index, openGeneric)._tag, 'ParameterDependent')
    assert.strictEqual(
      ExecutionAffinity.ofType(self.index, Type.nominal('main', 'Generic', ['i32']))._tag,
      'Unrestricted',
    )
    assert.strictEqual(
      ExecutionAffinity.ofType(
        self.index,
        Type.nominal('main', 'Generic', [Type.sharedCore('i32')]),
      )._tag,
      'LocalExecution',
    )
    const damaged = ExecutionAffinity.ofType(self.index, Type.nominal('main', 'Damaged'))
    assert.strictEqual(damaged._tag, 'Unavailable')
    if (damaged._tag !== 'Unavailable') return
    assert.deepEqual(
      damaged.causes.map((cause) => cause.code),
      ['SEM0001', 'SEM0001'],
    )

    assert.strictEqual(ExecutionAffinity.ofBorrow(self.index, 'i32', pair)._tag, 'LocalExecution')
    assert.strictEqual(
      ExecutionAffinity.ofEnvironment(self.index, [{ type: pair }])._tag,
      'LocalExecution',
    )
    assert.strictEqual(
      LocalSharedOwnership.count(
        LocalSharedOwnership.ofEnvironment(self.index, [{ access: 'Take', type: pair }]),
      ),
      2,
    )
    for (const name of ['Shared', 'SharedCore', 'Deferred', 'Scheduler', 'LocalRuntimeHandle']) {
      const ordinary = Type.nominal('main', name)
      assert.isFalse(Type.isSharedCore(ordinary))
      assert.strictEqual(ExecutionAffinity.ofType(self.index, ordinary)._tag, 'Unrestricted')
      assert.strictEqual(
        LocalSharedOwnership.count(LocalSharedOwnership.ofType(self.index, ordinary)),
        0,
      )
    }
    const forgedCore = Type.nominal('Intrinsic', 'SharedCore', ['i32'])
    assert.isFalse(Type.isSharedCore(forgedCore))
    assert.isFalse(Type.isIntrinsicNominal(forgedCore))
    assert.strictEqual(ExecutionAffinity.ofType(self.index, forgedCore)._tag, 'Unrestricted')
    assert.strictEqual(
      LocalSharedOwnership.count(LocalSharedOwnership.ofType(self.index, Type.slot(openCore))),
      0,
    )

    const fact = LocalSharedOwnership.inspect(Type.sharedCore(Type.nominal('main', 'Token')))
    assert.deepEqual(fact, {
      _tag: 'LocalSharedCoreFact',
      identity: 'Intrinsic.SharedCore',
      type: Type.sharedCore(Type.nominal('main', 'Token')),
      element: Type.nominal('main', 'Token'),
      role: 'LocalSharedStrong',
      category: 'Affine',
      affinity: { _tag: 'LocalExecution' },
    })
    assert.notInclude(Object.keys(fact ?? {}), 'address')
    assert.notInclude(Object.keys(fact ?? {}), 'count')
    assert.notInclude(Object.keys(fact ?? {}), 'layout')
    assert.strictEqual(
      ExecutionAffinity.encode(fact?.affinity ?? ExecutionAffinity.unrestricted),
      'LocalExecution',
    )

    const recovery = yield* Analysis.ofSource(
      'recovery',
      ascii(
        'fn inspect(value: Intrinsic.SharedCore<Missing>) -> i32 { return 0 }\npub fn main() -> i32 { return 0 }',
      ),
    )
    const recovered = recovery.ownership.get('recovery')?.functions.at(0)?.bindings.at(0)
    assert.deepEqual(
      Analysis.diagnostics(recovery).map((diagnostic) => diagnostic.code),
      ['SEM0001'],
    )
    assert.strictEqual(recovered?.category._tag, 'Unavailable')
    assert.strictEqual(recovered?.executionAffinity._tag, 'Unavailable')
    assert.strictEqual(recovered?.localSharedObligations._tag, 'Unavailable')
    assert.strictEqual(
      LocalSharedOwnership.count(recovered?.localSharedObligations ?? LocalSharedOwnership.none),
      0,
    )
  }),
)

it.effect('realizes immutable target snapshots from the same frontend facts', () =>
  Effect.gen(function* () {
    const frontend = yield* Analysis.ofSource('main', ascii('pub fn main() -> i32 { return 42 }'))
    const native = Analysis.realize(frontend)
    const wasm = Analysis.realize(frontend, 'wasm32-unknown-unknown')

    assert.strictEqual(native.results, frontend.results)
    assert.strictEqual(wasm.results, frontend.results)
    assert.strictEqual(native.ownership, frontend.ownership)
    assert.strictEqual(wasm.ownership, frontend.ownership)
    assert.strictEqual(Object.hasOwn(frontend, 'instances'), false)
    assert.strictEqual(native.target._tag, 'Resolved')
    assert.strictEqual(wasm.target._tag, 'Resolved')
    if (native.target._tag !== 'Resolved' || wasm.target._tag !== 'Resolved') return
    assert.strictEqual(native.target.target.kind, 'Native')
    assert.strictEqual(wasm.target.target.id, 'wasm32-unknown-unknown')
    assert.deepEqual(
      Analysis.phases(native)
        .slice(Analysis.phases(frontend).length)
        .map((entry) => entry.phase),
      ['instance-discovery', 'target-layout', 'mir-lowering'],
    )
  }),
)
