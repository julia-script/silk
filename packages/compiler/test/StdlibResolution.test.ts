import { spawnSync } from 'node:child_process'
import { readFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ModuleClosure from '../src/ModuleClosure.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Stdlib from '../src/Stdlib.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const importing = `import silk.vector { Vector, length }

pub fn main() -> i32 {
  return 42
}`

it('keeps the generated manifest ordered and byte-identical to canonical Silk files', () => {
  assert.deepEqual(
    Stdlib.manifest.map((entry) => entry.module),
    [...Stdlib.manifest.map((entry) => entry.module)].sort(),
  )
  assert.include(
    Stdlib.manifest.map((entry) => entry.module),
    'silk/logging',
  )
  for (const entry of Stdlib.manifest) {
    assert.strictEqual(entry.path, `${entry.module}.silk`)
    assert.deepEqual(entry.bytes, new Uint8Array(readFileSync(entry.sourceUrl)))
    assert.deepEqual(Stdlib.sources.get(entry.module), entry.bytes)
  }
})

it('declares one namespace for every standard-library module', () => {
  // A module without a manifest namespace is never auto-injected, so qualified use of it cannot
  // resolve. Every shipped module declares one; option keeps its member aliases alongside.
  assert.deepEqual(
    Stdlib.manifest.filter((entry) => entry.namespace === undefined).map((entry) => entry.module),
    [],
  )
  assert.strictEqual(Stdlib.findNamespace('Option')?.module, 'silk/option')
  assert.strictEqual(Stdlib.findNamespace('Result')?.module, 'silk/result')
  assert.strictEqual(Stdlib.findNamespace('Vector')?.module, 'silk/vector')
  assert.deepEqual(Stdlib.find('silk/option')?.aliases, ['None', 'Some'])
})

/** A namespace named only in a comment is prose, not a qualified actor. */
const commented = `// TODO: replace with Result.succeed once effect rows land
pub struct Result<A, F> { value: A }

pub fn main() -> i32 {
  return 0
}`

/** A namespace named only inside a static literal is inert text, not a qualified actor. */
const quoted = `pub fn label() -> string {
  return "Result.succeed"
}

pub struct Result<A, F> { value: A }

pub fn main() -> i32 {
  return 0
}`

/** A namespace genuinely called in code still injects its module. */
const called = `pub fn main() -> i32 {
  let outcome = Result.succeed<i32, i32>(42)
  drop outcome
  return 0
}`

it.effect('never injects a namespace named only inside a comment', () =>
  Effect.gen(function* () {
    // The closure scan is lexical: a commented mention leaves the user's own Result alone.
    const snapshot = yield* Analysis.ofSourceRealized('stdlib/commented', ascii(commented))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.notInclude(
      Analysis.modules(snapshot).map((module) => module.name),
      'silk/result',
    )
  }),
)

it.effect('never injects a namespace named only inside a static literal', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized('stdlib/quoted', ascii(quoted))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.notInclude(
      Analysis.modules(snapshot).map((module) => module.name),
      'silk/result',
    )
  }),
)

it.effect('keeps injecting a namespace that one qualified call actually names', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized('stdlib/called', ascii(called))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.include(
      Analysis.modules(snapshot).map((module) => module.name),
      'silk/result',
    )
  }),
)

it.effect('resolves standard-library imports without vendoring source', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized('stdlib/importer', ascii(importing))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(
      Analysis.modules(snapshot).map((module) => module.name),
      [
        'silk/core',
        'silk/layout',
        'silk/option',
        'silk/order',
        'silk/raw-buffer',
        'silk/slot',
        'silk/usize',
        'silk/vector',
        'stdlib/importer',
      ],
    )
    // The resolved declarations carry the library's canonical module identity.
    const index = Analysis.declarationIndex(snapshot)
    const library = index.modules.find((module) => module.module === 'silk/vector')
    assert.isDefined(library)
    assert.isTrue(
      library?.structs.some(
        (struct) => struct.canonical._tag === 'Canonical' && struct.canonical.id.name === 'Vector',
      ),
    )
  }),
)

it.effect('never consults a user resolver inside the reserved namespace', () =>
  Effect.gen(function* () {
    // A user resolver claims to supply silk/vector with hostile bytes; the closure must take
    // the compiler-shipped source instead.
    const hostile = new Map([['silk/vector', ascii('pub fn stolen() -> i32 { return 0 }')]])
    const closure = yield* ModuleClosure.load({
      root: SourceFile.make('stdlib/hostile-importer', ascii(importing)),
    }).pipe(Effect.provide(SourceResolver.memory(hostile)))
    const library = closure.modules.find((module) => module.name === 'silk/vector')
    assert.isDefined(library)
    const librarySource = closure.sources.get('silk/vector')
    assert.isTrue(
      librarySource !== undefined &&
        new TextDecoder()
          .decode(SourceFile.toUint8Array(librarySource))
          .includes('struct Vector<T>'),
    )
    assert.strictEqual(librarySource?.origin._tag, 'Memory')
  }),
)

it.effect('rejects a user root claiming the reserved namespace', () =>
  Effect.gen(function* () {
    const closure = yield* ModuleClosure.load({
      root: SourceFile.make('silk/impostor', ascii('pub fn main() -> i32 { return 0 }')),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.include(
      closure.diagnostics.map((diagnostic) => diagnostic.code),
      'MOD0004',
    )
  }),
)

it.effect('compiles library source with ordinary diagnostics and no privilege', () =>
  Effect.gen(function* () {
    // Inject a defect into the shipped library table for this test only: it must surface as an
    // ordinary semantic diagnostic attributed to the library module, exactly like user code.
    const sources = Stdlib.sources as Map<string, Uint8Array>
    const original = sources.get('silk/vector')
    assert.isDefined(original)
    if (original === undefined) return
    sources.set('silk/vector', ascii('pub fn broken() -> Missing { return 0 }'))
    try {
      const snapshot = yield* Analysis.ofSourceRealized(
        'stdlib/defective-importer',
        ascii(importing),
      )
      const attributed = Analysis.diagnostics(snapshot).filter(
        (diagnostic) => diagnostic.span.sourceId === 'silk/vector',
      )
      assert.isAbove(attributed.length, 0)
      assert.include(
        attributed.map((diagnostic) => diagnostic.code),
        'SEM0001',
      )
      assert.strictEqual(Analysis.sources(snapshot).get('silk/vector')?.origin._tag, 'Memory')
    } finally {
      sources.set('silk/vector', original)
    }
  }),
)

it('keeps stdlib-importing artifacts byte-identical across fresh processes', () => {
  const fixture = fileURLToPath(new URL('./fixtures/stdlib-determinism.mjs', import.meta.url))
  const run = () => spawnSync(process.execPath, [fixture], { encoding: 'utf8' })
  const first = run()
  const second = run()

  assert.strictEqual(first.status, 0, first.stderr)
  assert.strictEqual(second.status, 0, second.stderr)
  assert.strictEqual(first.stdout, second.stdout)
  const encoded = JSON.parse(first.stdout) as {
    readonly diagnostics: ReadonlyArray<unknown>
    readonly modules: ReadonlyArray<string>
  }
  assert.deepEqual(encoded.diagnostics, [])
  assert.include(encoded.modules, 'silk/vector')
})

/**
 * `app/helper` genuinely calls the standard library's `Result`, so the closure scan pulls
 * `silk/result` in and its manifest namespace is seeded into every module of the closure —
 * including one that declares a `Result` of its own. Seeding is a prelude, so the declaration wins.
 */
const shadowingRoot = `import app.helper { helped }

pub struct Result<A, F> { value: A }

fn own(value: Result<i32, i32>) -> i32 {
  drop value
  return 1
}

pub fn main() -> i32 {
  return helped()
}`

const shadowedHelper = `pub fn helped() -> i32 {
  let outcome = Result.succeed<i32, i32>(42)
  drop outcome
  return 0
}`

/** The shadowing module reaches the standard-library module it shadowed through a namespace import. */
const shadowingWithAlias = `import app.helper { helped }
import silk.result as StdResult

pub struct Result<A, F> { value: A }

fn own(value: Result<i32, i32>) -> i32 {
  drop value
  return 1
}

pub fn main() -> i32 {
  let outcome = StdResult.succeed<i32, i32>(7)
  drop outcome
  return helped()
}`

const withHelper = (root: string): Effect.Effect<Analysis.Snapshot> =>
  Analysis.makeRealized({ root: SourceFile.make('app/main', ascii(root)) }).pipe(
    Effect.provide(SourceResolver.memory(new Map([['app/helper', ascii(shadowedHelper)]]))),
  )

it.effect("lets a module's own declaration shadow a seeded standard-library namespace", () =>
  Effect.gen(function* () {
    const snapshot = yield* withHelper(shadowingRoot)
    // `silk/result` really is in the closure: without shadowing this is the SEM0016 collision.
    assert.include(
      Analysis.modules(snapshot).map((module) => module.name),
      'silk/result',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const lookup = Analysis.lookupName(snapshot, 'app/main', 'Result')
    assert.strictEqual(lookup._tag, 'Resolved')
    if (lookup._tag !== 'Resolved') return
    assert.strictEqual(lookup.declaration.canonical._tag, 'Canonical')
    if (lookup.declaration.canonical._tag !== 'Canonical') return
    assert.strictEqual(lookup.declaration.canonical.id.module, 'app/main')
    // The helper never declared `Result`, so its own seeded binding is untouched.
    const seeded = Analysis.lookupName(snapshot, 'app/helper', 'Result')
    assert.notStrictEqual(
      seeded._tag === 'Resolved' && seeded.declaration.canonical._tag === 'Canonical'
        ? seeded.declaration.canonical.id.module
        : undefined,
      'app/main',
    )
  }),
)

it.effect('reaches a shadowed standard-library module through an ordinary import', () =>
  Effect.gen(function* () {
    const snapshot = yield* withHelper(shadowingWithAlias)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const alias = Analysis.lookupName(snapshot, 'app/main', 'StdResult')
    assert.deepEqual(alias._tag === 'Namespace' ? alias.module : alias._tag, 'silk/result')
    // The local declaration keeps the bare spelling.
    const local = Analysis.lookupName(snapshot, 'app/main', 'Result')
    assert.strictEqual(
      local._tag === 'Resolved' && local.declaration.canonical._tag === 'Canonical'
        ? local.declaration.canonical.id.module
        : undefined,
      'app/main',
    )
  }),
)
