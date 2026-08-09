import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Type from '../src/Type.js'
import { invalidMatchCorpus } from './support/corpus.js'

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
  return Analysis.make({ root: SourceFile.make(rootModule, ascii(rootText)) }).pipe(
    Effect.provide(SourceResolver.memory(imports)),
  )
}

it.effect('answers multi-module queries from one snapshot', () =>
  Effect.gen(function* () {
    const self = yield* snapshot('root', [
      ['root', 'import lib\npub fn main() -> I32 { return 42 }'],
      ['lib', 'pub fn answer() -> I32 { return 1 }'],
    ])
    assert.deepEqual(
      Analysis.modules(self).map((module) => module.name),
      ['lib', 'root'],
    )
    assert.strictEqual(Analysis.syntaxOf(self, 'lib')?.source.id, 'lib')
    assert.strictEqual(Analysis.declarationByName(self, 'lib', 'answer')._tag, 'Resolved')
    assert.strictEqual(Analysis.declarationByName(self, 'root', 'answer')._tag, 'Missing')
    assert.strictEqual(Analysis.hirOf(self, 'root')?.functions.length, 1)
    assert.strictEqual(Analysis.moduleAnalysis(self, 'absent'), undefined)
    assert.deepEqual(Analysis.cycles(self), [])
    assert.deepEqual([...Analysis.sources(self).keys()], ['lib', 'root'])
  }),
)

it.effect('resolves imported declarations as stored callable values', () =>
  Effect.gen(function* () {
    const source =
      'import lib as Lib\npub fn main() -> I32 { let callback = Lib.identity return callback(42) }'
    const self = yield* snapshot('root', [
      ['root', source],
      ['lib', 'pub fn identity(value: I32) -> I32 { return value }'],
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
    const source = `fn add(value: I32, amount: I32) -> I32 { return value + amount }
pub fn main() -> I32 { let increment = add(2) return 40 |> increment }`
    const self = yield* Analysis.ofSource('main', ascii(source))

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
      ['root', 'import lib as Lib\npub fn main() -> I32 { let callback = Lib.hidden return 0 }'],
      ['lib', 'fn hidden(value: I32) -> I32 { return value }'],
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
      ['lib', 'pub fn answer() -> I32 { return 1 }'],
    ])
    assert.strictEqual(Analysis.declarationByName(self, 'lib', 'answer')._tag, 'Resolved')
    const libFunction = Analysis.hirOf(self, 'lib')?.functions.at(0)
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
      ['root', 'import lib\npub fn main() -> I32 { return 42 }'],
      ['lib', 'pub fn same() -> I32 { return 1 }\npub fn same() -> I32 { return 2 }'],
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
    const evaluated = yield* Analysis.ofSource(
      'memory/facade',
      ascii('pub fn main() -> I32 { return 42 }'),
    )
    const outcome = Analysis.evaluate(evaluated)
    assert.strictEqual(outcome._tag, 'Completed')

    const owned = yield* Analysis.ofSource(
      'memory/ownership',
      ascii('pub fn identity(value: I32) -> I32 { return value }'),
    )
    const facts = Analysis.ownershipOf(owned, 'memory/ownership')
    assert.strictEqual(facts?.functions.at(0)?.verdict._tag, 'Satisfied')
    assert.strictEqual(facts?.functions.at(0)?.bindings.at(0)?.category._tag, 'Copyable')
  }),
)

it.effect('answers stable match facts across semantic, HIR, ownership, MIR, and trace phases', () =>
  Effect.gen(function* () {
    const source = `struct Left { value: I32 }
struct Right { value: I32 }
fn inspect(input: Left | Right) -> I32 {
  return match &input {
    Left { value } if false => 0
    Left { value: answer } => answer
    Right { value } => value
  }
}
pub fn main() -> I32 { return inspect(Left { value: 42 }) }`
    const first = yield* Analysis.ofSource(
      'memory/match-facade',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    const second = yield* Analysis.ofSource(
      'memory/match-facade',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    const answer = (self: Analysis.Snapshot) => ({
      semantic: Analysis.matchesOf(self, 'memory/match-facade'),
      hir: Analysis.hirMatchesOf(self, 'memory/match-facade'),
      ownership: Analysis.ownershipMatchesOf(self, 'memory/match-facade'),
      mir: Analysis.mirMatchesOf(self),
      trace: Analysis.traceOf(Analysis.evaluate(self)).filter((event) =>
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
    const source = `fn choose(values: [I32; 2], index: I32) -> I32 { return values[index] }
pub fn main() -> I32 { return choose([10, 42], 1) }`
    const self = yield* Analysis.ofSource(
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
    const events = Analysis.arrayTraceEventsOf(outcome)

    assert.deepEqual(types.map(Type.encode), ['Array<I32, 2>'])
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
      const self = yield* Analysis.ofSource(
        'memory/nominal-facade',
        ascii('struct Pair { left: I32 right: Bool }\npub fn main() -> I32 { return 42 }'),
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
          'import model.Tree { Node }\nstruct Root { node: Node }\nstruct Loop { next: Loop }\npub fn main() -> I32 { return 42 }',
        ],
        ['model/Tree', 'pub struct Node { value: I32 }'],
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
    const self = yield* Analysis.ofSource(
      'memory/codegen',
      ascii('pub fn main() -> I32 { return 42 }'),
      'aarch64-apple-darwin',
    )
    const release = yield* Analysis.codegen(self, { mode: 'release' })
    assert.strictEqual(release._tag, 'BackendArtifact')
    assert.include(release.ir, 'silk_main')

    const invalid = yield* Analysis.ofSource(
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
    const self = yield* Analysis.ofSource(
      'memory/plan',
      ascii('pub fn main() -> I32 { if I32.equals(1, 1) { return 42 } return 0 }'),
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
        ['Bool', 4, 4],
        ['I32', 4, 4],
      ],
    )
  }),
)

it.effect('keeps unsupported targets explicit and queryable without manufacturing MIR', () =>
  Effect.gen(function* () {
    const unsupported = yield* Analysis.ofSource(
      'memory/unsupported',
      ascii('pub fn main() -> I32 { return 42 }'),
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
      const self = yield* Analysis.ofSource(
        `memory/${program.name}`,
        ascii(program.source),
        'wasm32-unknown-unknown',
      )
      const codes = Analysis.diagnostics(self).map((diagnostic) => diagnostic.code)
      for (const code of program.codes) assert.include(codes, code, program.name)
      assert.isAtLeast(Analysis.matchesOf(self, `memory/${program.name}`).length, 1, program.name)
    }
  }),
)

it.effect(
  'answers local, parameter, callable, field, and half-open semantic occurrence queries',
  () =>
    Effect.gen(function* () {
      const source = `struct Pair { left: I32 right: I32 }
fn identity(value: I32) -> I32 {
  let local = value
  let pair = Pair { left: local, right: 0 }
  return pair.left
}
pub fn main() -> I32 { return identity(42) }`
      const self = yield* Analysis.ofSource('main', ascii(source))
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
pub fn main() -> I32 { return answer() + tools.answer() }`
    const self = yield* snapshot('root', [
      ['root', root],
      ['lib', 'pub fn answer() -> I32 { return 42 }'],
      ['other', 'pub fn answer() -> I32 { return 7 }'],
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
      'fn valid(value: I32) -> I32 { return value }\nfn damaged( -> I32 { return missing() }'
    const first = yield* Analysis.ofSource('main', ascii(source))
    const second = yield* Analysis.ofSource('main', ascii(source))
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
