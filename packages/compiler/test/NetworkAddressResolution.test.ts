import * as AnalysisFixture from './support/AnalysisFixture.js'
import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import {
  networkAddressValueAcceptanceSource,
  nativeResolverAcceptanceSource,
  deadlineResolverFixtureSource,
} from './support/networkAddressResolutionAcceptance.js'

const implementation = readFileSync(
  new URL('../stdlib/silk/network_address.silk', import.meta.url),
  'utf8',
)
const reference = readFileSync(
  new URL('../../../apps/docs/content/reference/network-address-resolution.md', import.meta.url),
  'utf8',
)
const resolverImplementation = readFileSync(
  new URL('../stdlib/silk/resolver.silk', import.meta.url),
  'utf8',
).replace('import silk.network_address {DomainHost, Endpoint, Host, IpAddress, Port}\n', '')
const nativeResolverImplementation = readFileSync(
  new URL('../stdlib/silk/native_resolver.silk', import.meta.url),
  'utf8',
)
  .replace(
    '  import silk.network_address {DomainHost, Endpoint, IpAddress, Ipv4Address, Ipv6Address, Port}\n',
    '',
  )
  .replace(
    '  import silk.resolver {FamilySelection, NativeResolverOperation, NativeResultReason, ResolveRequest, ResolvedEndpoints, Resolver, ResolverError}\n',
    '',
  )
  .replace('  import silk.allocator {Allocator, OutOfMemoryError}\n', '')
  .replace('  import silk.option {Option}\n', '')
  .replace('  import silk.result {Result}\n', '')
  .replace('  import silk.slice {Slice}\n', '')
  .replace('  import silk.u16\n', '')
  .replace('  import silk.usize\n', '')
const encoder = new TextEncoder()

it.effect('realizes the consolidated owned address value contract on the native target', () =>
  Effect.gen(function* () {
    const source = `${implementation}\n${networkAddressValueAcceptanceSource}`
    const snapshot = yield* AnalysisFixture.retainingMain(
      'network-address/value-x86_64',
      encoder.encode(source),
      'x86_64-unknown-linux-gnu',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => ({
        code: diagnostic.code,
        message: diagnostic.message,
        start: diagnostic.span.start,
      })),
      [],
    )
    assert.deepEqual(yield* MirVerification.verify(Analysis.loweredMir(snapshot)), [])
  }),
)

it.effect('keeps the reference example executable', () =>
  Effect.gen(function* () {
    const opening = '```silk\n'
    const start = reference.indexOf(opening)
    const end = reference.indexOf('\n```', start + opening.length)
    assert.isAtLeast(start, 0)
    assert.isAbove(end, start)
    const example = reference
      .slice(start + opening.length, end)
      .replace('import silk.network_address {AddressError, Host, Port}\n', '')
      .replace('import silk.result {Result}\n', '')
    const snapshot = yield* AnalysisFixture.retainingMain(
      'network-address/reference-example',
      encoder.encode(`${implementation}\n${example}`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(yield* MirVerification.verify(Analysis.loweredMir(snapshot)), [])
  }),
)

it.effect(
  'selects the synchronous native boundary without leaking it to Wasm',
  () =>
    Effect.gen(function* () {
      for (const target of [
        'x86_64-unknown-linux-gnu',
        'aarch64-apple-darwin',
        'wasm32-unknown-unknown',
      ] as const) {
        const entry =
          target === 'wasm32-unknown-unknown'
            ? 'pub fn main() -> i32 { return 42 }'
            : nativeResolverAcceptanceSource
        const source =
          target === 'wasm32-unknown-unknown'
            ? `${nativeResolverImplementation}\n${entry}`
            : `${implementation}\n${resolverImplementation}\n${nativeResolverImplementation}\n${entry}`
        const sourceId = `network-address/native-${target}`
        const snapshot = yield* Analysis.makeRealized({
          root: SourceFile.make(sourceId, encoder.encode(source)),
          configuration: AnalysisFixture.configuration(sourceId, target, [
            target === 'wasm32-unknown-unknown' ? 'main' : 'nativeProgram',
          ]),
        }).pipe(Effect.provide(SourceResolver.empty))
        assert.deepEqual(
          Analysis.diagnostics(snapshot).map((diagnostic) => ({
            code: diagnostic.code,
            message: diagnostic.message,
            start: diagnostic.span.start,
          })),
          [],
        )
        assert.deepEqual(yield* MirVerification.verify(Analysis.loweredMir(snapshot)), [])
        const imports = Analysis.instancesOf(snapshot).foreignCalls.map((call) => call.symbol)
        if (target === 'wasm32-unknown-unknown') {
          assert.deepEqual(imports, [])
        } else if (target === 'aarch64-apple-darwin') {
          assert.deepEqual(imports, ['__error', 'freeaddrinfo', 'getaddrinfo'])
        } else {
          assert.deepEqual(imports, ['__errno_location', 'freeaddrinfo', 'getaddrinfo'])
        }
      }
      const rejectedImport = yield* AnalysisFixture.retainingMain(
        'network-address/native-public-import-wasm',
        encoder.encode(`import silk.native_resolver {NativeSystemResolver}
pub fn main() -> i32 { return 42 }`),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(
        Analysis.diagnostics(rejectedImport).map((diagnostic) => ({
          code: diagnostic.code,
          start: diagnostic.span.start,
        })),
        [{ code: 'SEM0014', start: 29 }],
      )
    }),
  45000,
)

it.effect(
  'retains deadline-provider registrations and owned results across parking',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* AnalysisFixture.retainingMain(
        'network-address/deadline-provider',
        encoder.encode(
          `${implementation}\n${resolverImplementation}\n${deadlineResolverFixtureSource}`,
        ),
        'x86_64-unknown-linux-gnu',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      assert.deepEqual(yield* MirVerification.verify(Analysis.loweredMir(snapshot)), [])
      if (snapshot.mir._tag !== 'Available') return
      const parks = snapshot.mir.value.functions
        .flatMap(MirVerification.operations)
        .filter((operation) => operation._tag === 'ExecutionPark')
      assert.lengthOf(parks, 2)
      assert.isTrue(parks.every((park) => park.guardCleanup._tag !== 'NoCleanup'))
    }),
  60000,
)
