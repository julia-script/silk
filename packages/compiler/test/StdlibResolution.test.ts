import { spawnSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ModuleClosure from '../src/ModuleClosure.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Stdlib from '../src/Stdlib.js'
import * as ToolchainIntegrity from '../src/ToolchainIntegrity.js'

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
    'silk/logger',
  )
  for (const entry of Stdlib.manifest) {
    assert.strictEqual(entry.path, `${entry.module}.silk`)
    assert.deepEqual(entry.bytes, new Uint8Array(readFileSync(entry.sourceUrl)))
    assert.deepEqual(Stdlib.sources.get(entry.module), entry.bytes)
  }
})

it('derives deterministic catalog metadata and enforces portable dependency direction', () => {
  const byModule = new Map(Stdlib.manifest.map((entry) => [entry.module, entry] as const))
  for (const entry of Stdlib.manifest) {
    const source = new TextDecoder().decode(entry.bytes)
    assert.strictEqual(entry.sourceIdentity, entry.module)
    assert.strictEqual(entry.documentation, entry.path)
    assert.strictEqual(createHash('sha256').update(entry.bytes).digest('hex'), entry.digest)
    assert.deepEqual(
      entry.runtimeInventory,
      [
        ...new Set(
          [...source.matchAll(/\bIntrinsic\.([A-Za-z_][A-Za-z0-9_]*)/g)].flatMap((match) =>
            match[1] === undefined ? [] : [match[1]],
          ),
        ),
      ].sort(),
    )
    if (entry.layer === 'portable') assert.isUndefined(entry.providerTargets)
    else assert.isAbove(entry.providerTargets?.length ?? 0, 0)

    for (const imported of source.matchAll(/\bimport\s+([A-Za-z0-9_]+(?:\.[A-Za-z0-9_]+)*)/g)) {
      const dependency = byModule.get(imported[1]?.replaceAll('.', '/') ?? '')
      if (entry.layer === 'portable') assert.notStrictEqual(dependency?.layer, 'target-provider')
    }
  }
})

it('publishes one deterministic matched identity graph over compiler, catalog, sources, and intrinsics', () => {
  const graph = ToolchainIntegrity.installed()
  assert.strictEqual(graph.schema, 'silk-toolchain-v1')
  assert.strictEqual(graph.digest.length, 64)
  assert.deepEqual(ToolchainIntegrity.validateFrontend(graph), {
    _tag: 'Matched',
    graph,
  })
  assert.deepEqual(
    graph.components.map((component) => `${component.kind}:${component.id}`),
    [...graph.components]
      .sort((left, right) => left.kind.localeCompare(right.kind) || left.id.localeCompare(right.id))
      .map((component) => `${component.kind}:${component.id}`),
  )
})

it('rejects stale catalog metadata and exact source bytes independently', () => {
  const installed = ToolchainIntegrity.installed()
  const staleCatalog = ToolchainIntegrity.make(
    installed.components.map((component) =>
      component.kind === 'Catalog' ? { ...component, digest: '0'.repeat(64) } : component,
    ),
  )
  const catalogValidation = ToolchainIntegrity.validateFrontend(staleCatalog)
  assert.strictEqual(catalogValidation._tag, 'Invalid')
  if (catalogValidation._tag === 'Invalid')
    assert.isTrue(
      catalogValidation.failures.some(
        (failure) => failure.reason._tag === 'DigestMismatch' && failure.reason.kind === 'Catalog',
      ),
    )

  const sources = new Map(Stdlib.sources)
  sources.set('silk/vector', ascii('stale source'))
  const sourceValidation = ToolchainIntegrity.validateFrontend(installed, sources)
  assert.strictEqual(sourceValidation._tag, 'Invalid')
  if (sourceValidation._tag === 'Invalid')
    assert.isTrue(
      sourceValidation.failures.some(
        (failure) =>
          failure.reason._tag === 'DigestMismatch' && failure.reason.id === 'source/silk/vector',
      ),
    )
})

it('computes browser-safe SHA-256 identities over exact UTF-8 bytes', () => {
  for (const value of ['', 'silk', 'λ'])
    assert.strictEqual(
      ToolchainIntegrity.contentDigest(value),
      createHash('sha256').update(value).digest('hex'),
    )
})

it('declares one discoverable namespace for every standard-library module', () => {
  // Catalog namespaces drive tooling discovery only; they never enter source scope implicitly.
  assert.deepEqual(
    Stdlib.manifest.filter((entry) => entry.namespace === undefined).map((entry) => entry.module),
    [],
  )
  assert.strictEqual(Stdlib.findNamespace('Option')?.module, 'silk/option')
  assert.strictEqual(Stdlib.findNamespace('Result')?.module, 'silk/result')
  assert.strictEqual(Stdlib.findNamespace('Vector')?.module, 'silk/vector')
  assert.strictEqual(Stdlib.findNamespace('Effect')?.module, 'silk/effect')
  assert.strictEqual(Stdlib.findNamespace('Random')?.module, 'silk/random')
  assert.strictEqual(Stdlib.findNamespace('InsecureRandom')?.module, 'silk/insecure_random')
  assert.strictEqual(Stdlib.findNamespace('InsecureSeed')?.module, 'silk/insecure_seed')
  assert.strictEqual(Stdlib.findNamespace('OsRandom')?.module, 'silk/os_random')
  assert.strictEqual(Stdlib.findNamespace('Fiber')?.module, 'silk/fiber')
  assert.strictEqual(Stdlib.findNamespace('LocalScheduler')?.module, 'silk/local_scheduler')
  assert.strictEqual(Stdlib.findNamespace('Scheduler')?.module, 'silk/scheduler')
  assert.isDefined(Stdlib.find('silk/effect'))
  assert.isUndefined(Stdlib.find('silk/effects'))
  assert.deepEqual(Stdlib.find('silk/option')?.aliases, ['None', 'Some'])
  assert.deepEqual(Stdlib.find('silk/insecure_random')?.aliases, ['Xoshiro256StarStar'])
  assert.deepEqual(Stdlib.find('silk/insecure_seed')?.aliases, ['FixedInsecureSeed', 'Seed'])
  assert.isUndefined(Stdlib.find('silk/random')?.aliases)
  assert.includeMembers([...(Stdlib.find('silk/fiber')?.aliases ?? [])], ['Cancelled', 'Outcome'])
  assert.deepEqual(Stdlib.find('silk/local_scheduler')?.aliases, ['StalledError'])
  assert.includeMembers(
    [...(Stdlib.find('silk/scheduler')?.aliases ?? [])],
    ['PendingPublication', 'TaskId', 'TaskIdExhaustedError'],
  )
})

const insecureRandomImporter = `import silk.insecure_random as InsecureRandom

pub fn main() -> i32 {
  let provider = InsecureRandom.seeded(0)
  drop provider
  return 42
}`

it.effect('resolves the complete InsecureRandom surface to canonical portable Silk source', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'stdlib/insecure-random-importer',
      ascii(insecureRandomImporter),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const entry = Stdlib.find('silk/insecure_random')
    assert.isDefined(entry)
    assert.strictEqual(entry?.layer, 'portable')
    assert.deepEqual(entry?.runtimeInventory, [])
    assert.isUndefined(entry?.providerTargets)
    assert.isTrue(entry?.sourceUrl.pathname.endsWith('/stdlib/silk/insecure_random.silk') ?? false)

    const module = Analysis.declarationIndex(snapshot).modules.find(
      (candidate) => candidate.module === 'silk/insecure_random',
    )
    assert.isDefined(module)
    const canonicals = [
      ...(module?.declarations ?? []),
      ...(module?.services ?? []),
      ...(module?.structs ?? []),
    ].flatMap((declaration) =>
      declaration.canonical._tag === 'Canonical' ? [declaration.canonical.id] : [],
    )
    assert.includeMembers(
      canonicals.map((canonical) => canonical.name),
      [
        'InsecureRandom',
        'Xoshiro256StarStar',
        'seeded',
        'nextU64',
        'nextBool',
        'below',
        'fillBytes',
      ],
    )
    assert.isTrue(canonicals.every((canonical) => canonical.module === 'silk/insecure_random'))
  }),
)

it.effect('keeps a renamed copy of the InsecureRandom implementation ordinary and executable', () =>
  Effect.gen(function* () {
    const source = Stdlib.sources.get('silk/insecure_random')
    assert.isDefined(source)
    if (source === undefined) return
    const renamed = new TextDecoder()
      .decode(source)
      .replaceAll('Xoshiro256StarStar', 'Sequence256')
      .replaceAll('InsecureRandom', 'Entropy')
    const root = `import app.entropy as Entropy
import silk.effect as Effect

pub fn main() -> i32 {
  let mut provider = Entropy.seeded(0)
  let word = run Entropy.nextU64()
    |> Effect.provideMut<Entropy.Entropy>(&mut provider)
  if word != 0x99ec5f36cb75f2b4 { return 1 }
  return 42
}`
    const snapshot = yield* Analysis.makeRealized({
      root: SourceFile.make('app/main', ascii(root)),
    }).pipe(Effect.provide(SourceResolver.memory(new Map([['app/entropy', ascii(renamed)]]))))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('resolves secure Random and InsecureSeed to distinct canonical portable source', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'stdlib/random-capabilities-importer',
      ascii(`import silk.insecure_seed as InsecureSeed
import silk.random as Random
pub fn main() -> i32 {
  let provider = InsecureSeed.fixed(20, 22)
  drop provider
  return 42
}`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    for (const moduleName of ['silk/random', 'silk/insecure_seed']) {
      const entry = Stdlib.find(moduleName)
      assert.strictEqual(entry?.layer, 'portable')
      assert.deepEqual(entry?.runtimeInventory, [])
      assert.isUndefined(entry?.providerTargets)
    }
    const modules = Analysis.declarationIndex(snapshot).modules
    const names = (moduleName: string): Array<string> => {
      const module = modules.find((candidate) => candidate.module === moduleName)
      return [
        ...(module?.declarations ?? []),
        ...(module?.services ?? []),
        ...(module?.structs ?? []),
      ].flatMap((declaration) =>
        declaration.canonical._tag === 'Canonical' ? [declaration.canonical.id.name] : [],
      )
    }
    assert.includeMembers(names('silk/random'), [
      'Random',
      'fillBytes',
      'nextU64',
      'nextBool',
      'below',
    ])
    assert.notInclude(names('silk/random'), 'seeded')
    assert.notInclude(names('silk/random'), 'Xoshiro256StarStar')
    assert.includeMembers(names('silk/insecure_seed'), [
      'Seed',
      'InsecureSeed',
      'FixedInsecureSeed',
      'first',
      'second',
      'fixed',
      'fromRandom',
      'get',
    ])
  }),
)

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

/** A qualified call without an import remains unresolved. */
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

it.effect('requires an explicit import for a qualified standard-library call', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized('stdlib/called', ascii(called))
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0009'],
    )
    assert.notInclude(
      Analysis.modules(snapshot).map((module) => module.name),
      'silk/result',
    )
  }),
)

it.effect('resolves standard-library imports without vendoring source', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized('stdlib/importer', ascii(importing))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    // `usize` renders and reads decimal text, so naming it reaches the formatting stack and the
    // owned text it produces. The closure is an analysis fact, not an artifact cost: this program's
    // emitted module is byte-identical to the one it produced before those functions existed,
    // because codegen emits only what `main` reaches.
    assert.deepEqual(
      Analysis.modules(snapshot).map((module) => module.name),
      [
        'silk/allocator',
        'silk/bool',
        'silk/bytes',
        'silk/char',
        'silk/f32',
        'silk/f64',
        'silk/format',
        'silk/i16',
        'silk/i32',
        'silk/i64',
        'silk/i8',
        'silk/isize',
        'silk/layout',
        'silk/option',
        'silk/order',
        'silk/raw_buffer',
        'silk/result',
        'silk/slot',
        'silk/string',
        'silk/u16',
        'silk/u32',
        'silk/u64',
        'silk/u8',
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
 * `app/helper` explicitly imports the standard library's `Result`. That ordinary dependency enters
 * the closure, but its catalog namespace does not enter the root module's scope.
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

const shadowedHelper = `import silk.result as Result

pub fn helped() -> i32 {
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

it.effect('keeps catalog declarations out of an importing sibling module', () =>
  Effect.gen(function* () {
    const snapshot = yield* withHelper(shadowingRoot)
    // `silk/result` is in the closure only because the helper imported it.
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
    const helperImport = Analysis.lookupName(snapshot, 'app/helper', 'Result')
    assert.deepEqual(
      helperImport._tag === 'Namespace' ? helperImport.module : helperImport._tag,
      'silk/result',
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
