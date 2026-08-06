import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Mir from '../src/Mir.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (root: string, sources: Readonly<Record<string, string>>): Analysis.Snapshot =>
  Analysis.make({
    rootModule: root,
    sources: new Map(Object.entries(sources).map(([name, source]) => [name, ascii(source)])),
  })

it('binds dotted namespace aliases and emits an ordinary canonical HIR call', () => {
  const self = snapshot('app/Main', {
    'app/Main': 'import compiler.Syntax as Tree\npub fn main() -> I32 { return Tree.parse() }',
    'compiler/Syntax': 'pub fn parse() -> I32 { return 42 }\nfn hidden() -> I32 { return 0 }',
  })
  const scope = Analysis.moduleScope(self, 'app/Main')
  assert.strictEqual(
    scope?.bindings.find((binding) => binding.spelling === 'Tree')?._tag,
    'ModuleNamespace',
  )
  const main = Analysis.hirOf(self, 'app/Main')?.functions.at(0)
  const returned = main === undefined ? undefined : Hir.returned(main)
  assert.strictEqual(returned?._tag, 'Call')
  if (returned?._tag !== 'Call') return
  assert.deepEqual(returned.target, {
    _tag: 'CanonicalDeclarationId',
    module: 'compiler/Syntax',
    name: 'parse',
  })
  assert.deepEqual(
    self.instances.instances.map(
      (instance) => `${instance.key.declaration.module}.${instance.key.declaration.name}`,
    ),
    ['app/Main.main', 'compiler/Syntax.parse'],
  )
})

it('resolves namespace pipeline targets as ordinary imported calls', () => {
  const self = snapshot('app/Main', {
    'app/Main': 'import compiler.Syntax as Tree\npub fn main() -> I32 { return 40 |> Tree.add(2) }',
    'compiler/Syntax': 'pub fn add(left: I32, right: I32) -> I32 { return left + right }',
  })
  const main = Analysis.hirOf(self, 'app/Main')?.functions.at(0)
  const returned = main === undefined ? undefined : Hir.returned(main)

  assert.deepEqual(Analysis.diagnostics(self), [])
  assert.strictEqual(returned?._tag, 'Call')
  if (returned?._tag !== 'Call') return
  assert.deepEqual(returned.target, {
    _tag: 'CanonicalDeclarationId',
    module: 'compiler/Syntax',
    name: 'add',
  })
  const evaluation = Analysis.evaluate(self)
  assert.strictEqual(evaluation._tag, 'Completed')
  assert.strictEqual(evaluation._tag === 'Completed' ? evaluation.result.value : undefined, 42)
})

it.effect('emits imported pipelines identically across source order and both backends', () =>
  Effect.gen(function* () {
    const entries = [
      [
        'app/Main',
        'import compiler.Syntax as Tree\npub fn main() -> I32 { return 40 |> Tree.add(2) }',
      ],
      ['compiler/Syntax', 'pub fn add(left: I32, right: I32) -> I32 { return left + right }'],
    ] as const
    const make = (ordered: ReadonlyArray<readonly [string, string]>, target: string) =>
      Analysis.make({
        rootModule: 'app/Main',
        target,
        sources: new Map(ordered.map(([name, source]) => [name, ascii(source)])),
      })
    const first = make(entries, 'aarch64-apple-darwin')
    const second = make([...entries].reverse(), 'aarch64-apple-darwin')
    const firstLlvm = yield* Analysis.codegen(first, { mode: 'release' })
    const secondLlvm = yield* Analysis.codegen(second, { mode: 'release' })
    const firstWasm = yield* Analysis.codegenWasm(make(entries, 'wasm32-unknown-unknown'), {
      mode: 'release',
    })
    const secondWasm = yield* Analysis.codegenWasm(
      make([...entries].reverse(), 'wasm32-unknown-unknown'),
      { mode: 'release' },
    )

    assert.strictEqual(firstLlvm.ir, secondLlvm.ir)
    assert.deepEqual(firstLlvm.bitcode, secondLlvm.bitcode)
    assert.strictEqual(firstWasm.ir, secondWasm.ir)
    assert.deepEqual(firstWasm.bitcode, secondWasm.bitcode)
    assert.deepEqual(
      firstLlvm.symbols.map((entry) => entry.symbol),
      ['silk_main', 'silk_1_add'],
    )
  }),
)

it('binds selected members and rejects private selections without cascading calls', () => {
  const self = snapshot('app/Main', {
    'app/Main':
      'import compiler.Syntax { parse as read, hidden }\npub fn main() -> I32 { return hidden() }',
    'compiler/Syntax': 'pub fn parse() -> I32 { return 42 }\nfn hidden() -> I32 { return 0 }',
  })
  assert.strictEqual(Analysis.lookupName(self, 'app/Main', 'read')._tag, 'Resolved')
  assert.strictEqual(Analysis.lookupName(self, 'app/Main', 'hidden')._tag, 'Unavailable')
  assert.deepEqual(
    Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
    ['SEM0015'],
  )
})

it('keeps cyclic cross-module calls finite and canonical', () => {
  const self = snapshot('app/Main', {
    'app/Main': 'import cycle.A { a }\npub fn main() -> I32 { return a() }',
    'cycle/A': 'import cycle.B { b }\npub fn a() -> I32 { return b() }',
    'cycle/B': 'import cycle.A { a }\npub fn b() -> I32 { return a() }',
  })
  assert.deepEqual(self.closure.cycles, [['cycle/A', 'cycle/B']])
  assert.deepEqual(
    self.instances.instances.map((instance) => instance.key.declaration.module),
    ['app/Main', 'cycle/A', 'cycle/B'],
  )
})

const threeModuleSources = {
  'app/Main': 'import library.Answer { answer }\npub fn main() -> I32 { return answer() }',
  'library/Answer':
    'import values.Number\nfn local() -> I32 { return 40 }\npub fn answer() -> I32 { return I32.add(local(), Number.two()) }',
  'values/Number': 'pub fn two() -> I32 { return 2 }',
}

it.effect('interprets and emits LLVM and WebAssembly across three canonical modules', () =>
  Effect.gen(function* () {
    const self = snapshot('app/Main', threeModuleSources)
    const evaluation = Analysis.evaluate(self)
    assert.strictEqual(evaluation._tag, 'Completed')
    if (evaluation._tag !== 'Completed') return
    assert.strictEqual(evaluation.result.value, 42)
    const llvm = yield* Analysis.codegen(self, { mode: 'release' })
    assert.match(llvm.ir, /define i32 @silk_main/)
    assert.match(llvm.ir, /call i32/)
    const wasm = yield* Analysis.codegenWasm(
      Analysis.make({
        rootModule: 'app/Main',
        target: 'wasm32-unknown-unknown',
        sources: new Map(
          Object.entries(threeModuleSources).map(([name, source]) => [name, ascii(source)]),
        ),
      }),
      { mode: 'release' },
    )
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
    const main = instance.exports.silk_main
    assert.strictEqual(typeof main, 'function')
    if (typeof main !== 'function') return
    assert.strictEqual(main(), 42)
  }),
)

it('keeps HIR, MIR, diagnostics, and instances independent of source supply order', () => {
  const entries = Object.entries(threeModuleSources)
  const analyze = (ordered: ReadonlyArray<readonly [string, string]>) =>
    Analysis.make({
      rootModule: 'app/Main',
      sources: new Map(ordered.map(([name, source]) => [name, ascii(source)])),
    })
  const forward = analyze(entries)
  const reverse = analyze([...entries].reverse())
  assert.deepEqual(
    [...forward.results.values()].map((result) => Hir.encode(result.hir)),
    [...reverse.results.values()].map((result) => Hir.encode(result.hir)),
  )
  assert.strictEqual(
    Mir.encode(Analysis.loweredMir(forward)),
    Mir.encode(Analysis.loweredMir(reverse)),
  )
  assert.deepEqual(forward.instances, reverse.instances)
  assert.deepEqual(forward.diagnostics, reverse.diagnostics)
})

it('diagnoses duplicate imports, redundant aliases, unknown members, and flat conflicts stably', () => {
  const cases: ReadonlyArray<
    readonly [string, Readonly<Record<string, string>>, ReadonlyArray<string>]
  > = [
    [
      'duplicate',
      {
        root: 'import library.One\nimport library.One as Again\npub fn main() -> I32 { return 0 }',
        'library/One': 'pub fn one() -> I32 { return 1 }',
      },
      ['MOD0003'],
    ],
    [
      'redundant alias',
      {
        root: 'import library.One as One\npub fn main() -> I32 { return 0 }',
        'library/One': 'pub fn one() -> I32 { return 1 }',
      },
      ['SEM0013'],
    ],
    [
      'unknown member',
      {
        root: 'import library.One { missing }\npub fn main() -> I32 { return missing() }',
        'library/One': 'pub fn one() -> I32 { return 1 }',
      },
      ['SEM0014'],
    ],
    [
      'local collision',
      {
        root: 'import library.One { one }\npub fn one() -> I32 { return 0 }\npub fn main() -> I32 { return one() }',
        'library/One': 'pub fn one() -> I32 { return 1 }',
      },
      ['SEM0016'],
    ],
    [
      'intrinsic collision',
      { root: 'fn I32() -> I32 { return 0 }\npub fn main() -> I32 { return 0 }' },
      ['SEM0016'],
    ],
  ]
  for (const [label, sources, codes] of cases) {
    const self = snapshot('root', sources)
    assert.deepEqual(
      self.diagnostics.map((diagnostic) => diagnostic.code),
      codes,
      label,
    )
  }
})

it('keeps a recovered alias parser-owned and does not fabricate a namespace binding', () => {
  const self = snapshot('root', {
    root: 'import library.One as\npub fn main() -> I32 { return 0 }',
    'library/One': 'pub fn one() -> I32 { return 1 }',
  })
  assert.deepEqual(
    self.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assert.strictEqual(Analysis.lookupName(self, 'root', 'One')._tag, 'Missing')
})

it('discovers one shared diamond target and excludes unreachable imported declarations', () => {
  const self = snapshot('root', {
    root: 'import left.Api { left }\nimport right.Api { right }\npub fn main() -> I32 { return I32.add(left(), right()) }',
    'left/Api':
      'import shared.Value { value }\npub fn left() -> I32 { return value() }\npub fn unused() -> I32 { return 0 }',
    'right/Api': 'import shared.Value { value }\npub fn right() -> I32 { return value() }',
    'shared/Value': 'pub fn value() -> I32 { return 21 }',
  })
  assert.deepEqual(
    self.instances.instances.map(
      (instance) => `${instance.key.declaration.module}.${instance.key.declaration.name}`,
    ),
    ['root.main', 'left/Api.left', 'right/Api.right', 'shared/Value.value'],
  )
  assert.strictEqual(
    self.instances.instances.some((instance) => instance.key.declaration.name === 'unused'),
    false,
  )
})

it('resolves local, selected, and qualified nominal types in declaration contracts', () => {
  const self = snapshot('app/Main', {
    'app/Main':
      'import syntax.Tree as Ast { Node }\n' +
      'struct Local { node: Node qualified: Ast.Node }\n' +
      'fn identity(value: Local) -> Local { return value }\n' +
      'pub fn main() -> I32 { return 0 }',
    'syntax/Tree': 'pub struct Node { value: I32 }',
  })
  const local = self.index.modules.find((module) => module.module === 'app/Main')?.structs.at(0)
  const identity = self.index.modules
    .find((module) => module.module === 'app/Main')
    ?.declarations.find(
      (declaration) =>
        declaration.name._tag === 'Present' && declaration.name.spelling === 'identity',
    )
  const identityParameter = identity?.parameters.at(0)?.declaredType

  assert.deepEqual(
    local?.fields.map((field) =>
      field.declaredType._tag === 'Resolved' ? field.declaredType.type : field.declaredType._tag,
    ),
    [
      { _tag: 'NominalType', module: 'syntax/Tree', name: 'Node' },
      { _tag: 'NominalType', module: 'syntax/Tree', name: 'Node' },
    ],
  )
  assert.deepEqual(identityParameter?._tag === 'Resolved' ? identityParameter.type : undefined, {
    _tag: 'NominalType',
    module: 'app/Main',
    name: 'Local',
  })
  assert.deepEqual(Analysis.diagnostics(self), [])
})

it('keeps type-kind and imported visibility failures explicit without fallback lookup', () => {
  const wrongKind = snapshot('root', {
    root:
      'fn Value() -> I32 { return 0 }\n' +
      'struct Box { value: Value }\n' +
      'pub fn main() -> I32 { return 0 }',
  })
  assert.deepEqual(
    wrongKind.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0018'],
  )
  assert.deepEqual(
    wrongKind.diagnostics.map((diagnostic) => ({
      sourceId: diagnostic.span.sourceId,
      start: diagnostic.span.start,
      end: diagnostic.span.end,
    })),
    [{ sourceId: 'root', start: 51, end: 56 }],
  )

  const privateRoot =
    'import types.Secret { Secret }\n' +
    'struct Box { secret: Secret }\n' +
    'pub fn main() -> I32 { return 0 }'
  const privateImport = snapshot('root', {
    root: privateRoot,
    'types/Secret': 'struct Secret { value: I32 }',
  })
  const field = privateImport.index.modules
    .find((module) => module.module === 'root')
    ?.structs.at(0)
    ?.fields.at(0)
  assert.deepEqual(
    privateImport.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0015'],
  )
  const privateDiagnostic = privateImport.diagnostics.at(0)
  assert.strictEqual(
    privateDiagnostic === undefined
      ? undefined
      : privateRoot.slice(privateDiagnostic.span.start, privateDiagnostic.span.end),
    'Secret',
  )
  assert.strictEqual(field?.declaredType._tag, 'Unresolved')
  assert.deepEqual(
    field?.declaredType._tag === 'Unresolved' ? field.declaredType.candidate : undefined,
    {
      _tag: 'NominalType',
      module: 'types/Secret',
      name: 'Secret',
    },
  )
})

it('distinguishes harmless import cycles from inline nominal dependency cycles', () => {
  const harmless = snapshot('a/A', {
    'a/A': 'import b.B\npub struct A { value: I32 }\npub fn main() -> I32 { return 0 }',
    'b/B': 'import a.A\npub struct B { value: Bool }',
  })
  assert.deepEqual(harmless.closure.cycles, [['a/A', 'b/B']])
  assert.deepEqual(harmless.diagnostics, [])
  assert.strictEqual(
    harmless.index.modules.every((module) =>
      module.structs.every((struct) => struct.dependency._tag === 'Available'),
    ),
    true,
  )
})
