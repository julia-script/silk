import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as Option from 'effect/Option'
import * as ArtifactComposition from '../src/ArtifactComposition.js'
import * as CompilationProfile from '../src/CompilationProfile.js'
import * as Preparation from '../src/Preparation.js'
import * as SourceOrigin from '../src/SourceOrigin.js'
import * as SourceResolver from '../src/SourceResolver.js'
import { unreachable } from './support/raise.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/** A resolver that records every module it is asked for; the standard library stays embedded. */
const counting = (sources: ReadonlyMap<string, Uint8Array>) => {
  const calls: Array<string> = []
  const layer = Layer.succeed(SourceResolver.SourceResolver, {
    resolveStandardLibrary: SourceResolver.resolveEmbeddedStandardLibrary,
    toolchainSources: SourceResolver.embeddedToolchainSources,
    resolve: (module: string) =>
      Effect.sync(() => {
        calls.push(module)
        const bytes = sources.get(module)
        return bytes === undefined
          ? Option.none()
          : Option.some(SourceResolver.resolved(bytes, SourceOrigin.memory()))
      }),
  })
  return { calls, layer }
}

const provider = ascii(`
export "C" fn newState() -> ?*mut u8 { return Intrinsic.pointerNull<u8>() }
export "C" fn reserve(state: ?*mut u8, size: usize, alignment: usize) -> ?*mut u8 { return Intrinsic.pointerNull<u8>() }
export "C" fn reclaim(state: ?*mut u8, frame: ?*mut u8) -> () {}
export "C" fn finish(state: ?*mut u8) -> () {}
`)

const storageRequest = Effect.gen(function* () {
  const profile = yield* CompilationProfile.decode({
    target: 'wasm32-unknown-unknown',
    artifact: 'object',
    runtime: { kind: 'none' },
  })
  const component = {
    capability: 'execution-storage',
    bindings: [
      ['create', 'newState'],
      ['acquire', 'reserve'],
      ['release', 'reclaim'],
      ['destroy', 'finish'],
    ].map(([operation, declaration]) => ({
      operation: operation ?? unreachable('operation'),
      module: 'custom/storage',
      declaration: declaration ?? unreachable('declaration'),
    })),
  }
  return {
    root: 'component/application',
    configuration: {
      profile: CompilationProfile.input(profile),
      composition: {
        ...ArtifactComposition.defaults(profile),
        retention: [{ module: 'component/application', declaration: 'main' }],
        components: [component],
      },
    },
  }
})

it.effect('seals demanded storage once and leaves an unused catalog unloaded', () =>
  Effect.gen(function* () {
    const request = yield* storageRequest
    const suspending = counting(
      new Map([
        [
          'component/application',
          ascii(
            'import silk.effect { Effect }\npub fn main() -> i32 { return run Effect.suspend(effect { return 42 }) }',
          ),
        ],
        ['custom/storage', provider],
      ]),
    )
    const bundle = yield* Preparation.prepare(request, 'executable').pipe(
      Effect.provide(suspending.layer),
    )
    const resolvedBeforeDownstream = suspending.calls.length
    // Discovery resolved the provider exactly once across the demand passes.
    assert.strictEqual(suspending.calls.filter((call) => call === 'custom/storage').length, 1)
    assert.isTrue(bundle.frontend.closure.sources.has('custom/storage'))
    assert.deepEqual(bundle.components, [{ module: 'custom/storage', reason: 'execution-storage' }])
    // Downstream realization is a resolver-free projection of the sealed bundle.
    const realized = Preparation.realization(bundle)
    assert.deepEqual(realized.diagnostics, [])
    assert.strictEqual(realized.mir._tag, 'Available')
    if (realized.mir._tag === 'Available')
      assert.strictEqual(realized.mir.value.executionStorage?.acquire.symbol, 'reserve')
    assert.strictEqual(suspending.calls.length, resolvedBeforeDownstream)
    const manifest = Preparation.manifest(bundle)
    assert.strictEqual(manifest.intent, 'executable')
    assert.strictEqual(manifest.status, 'SourceClosed')
    assert.strictEqual(manifest.target, 'wasm32-unknown-unknown')
    assert.includeMembers(
      manifest.modules.map((module) => module.name),
      ['component/application', 'custom/storage'],
    )
    // A synchronous application with the same catalog never resolves the component.
    const synchronous = counting(
      new Map([
        ['component/application', ascii('pub fn main() -> i32 { return 42 }')],
        ['custom/storage', provider],
      ]),
    )
    const unused = yield* Preparation.prepare(request, 'executable').pipe(
      Effect.provide(synchronous.layer),
    )
    assert.notInclude(synchronous.calls, 'custom/storage')
    assert.isFalse(unused.frontend.closure.sources.has('custom/storage'))
    assert.deepEqual(unused.components, [])
    assert.deepEqual(Preparation.realization(unused).diagnostics, [])
  }),
)

it.effect('keeps analysis intent frontend-only and promotes into a new bundle', () =>
  Effect.gen(function* () {
    const request = yield* storageRequest
    const sources = new Map([
      ['component/application', ascii('pub fn main() -> i32 { return 42 }')],
      ['custom/storage', provider],
    ])
    const analysis = yield* Preparation.prepare(request, 'analysis').pipe(
      Effect.provide(counting(sources).layer),
    )
    const phases = analysis.frontend.report.map((phase) => phase.phase)
    assert.include(phases, 'elaboration')
    for (const phase of ['instance-discovery', 'target-layout', 'mir-lowering'])
      assert.notInclude(phases, phase)
    assert.isTrue(Object.isFrozen(analysis))
    const manifestBefore = Preparation.manifest(analysis)
    assert.strictEqual(manifestBefore.intent, 'analysis')
    assert.strictEqual(manifestBefore.status, 'SourceClosed')
    // Frontend facts are complete after sealing; no resolver or parser is consulted again.
    assert.deepEqual(analysis.frontend.diagnostics, [])
    const promoted = yield* Preparation.promote(analysis.frontend, undefined).pipe(
      Effect.provide(SourceResolver.empty),
    )
    assert.strictEqual(promoted.intent, 'executable')
    assert.strictEqual(Preparation.realization(promoted).mir._tag, 'Available')
    assert.deepEqual(Preparation.manifest(analysis), manifestBefore)
    assert.strictEqual(promoted.frontend.results, analysis.frontend.results)
    assert.notStrictEqual(promoted.identity, analysis.identity)
  }),
)
