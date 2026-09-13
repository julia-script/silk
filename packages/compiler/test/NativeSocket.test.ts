import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as AnalysisFixture from './support/AnalysisFixture.js'
import {
  nativeSocketAcceptanceSource,
  nativeSocketCancellationAnalysisSource,
  nativeSocketCorpusProgram,
  nativeSocketDarwinWitnessSource,
  nativeSocketGnuWitnessSource,
  nativeSocketStubSource,
} from './support/nativeSocketAcceptance.js'

const implementation = readFileSync(
  new URL('../stdlib/silk/native_socket.silk', import.meta.url),
  'utf8',
)
const reference = readFileSync(
  new URL('../../../apps/docs/content/reference/native-socket-connections.md', import.meta.url),
  'utf8',
)
const encoder = new TextEncoder()
const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang: 'clang',
  llvmAr: 'llvm-ar',
})

it.effect(
  'realizes the complete acquisition and ByteDuplex state machines',
  () =>
    Effect.gen(function* () {
      for (const target of ['x86_64-unknown-linux-gnu', 'aarch64-apple-darwin'] as const) {
        const snapshot = yield* AnalysisFixture.retainingMain(
          `native-socket/complete-${target}`,
          encoder.encode(nativeSocketAcceptanceSource),
          target,
        )
        assert.deepEqual(
          Analysis.diagnostics(snapshot).map((diagnostic) => ({
            code: diagnostic.code,
            message: diagnostic.message,
            start: diagnostic.span.start,
          })),
          [],
        )
        assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
        const actorSymbols = Analysis.instancesOf(snapshot)
          .foreignCalls.map((call) => call.symbol)
          .filter(
            (symbol) =>
              !symbol.startsWith('silk_socket_') && symbol !== 'malloc' && symbol !== 'free',
          )
          .sort()
        assert.deepEqual(
          actorSymbols,
          target === 'aarch64-apple-darwin'
            ? [
                '__error',
                'close$NOCANCEL',
                'connect',
                'fcntl',
                'getsockopt',
                'poll',
                'recv',
                'send',
                'setsockopt',
                'shutdown',
                'socket',
              ]
            : [
                '__errno_location',
                'close',
                'connect',
                'getsockopt',
                'poll',
                'recv',
                'send',
                'setsockopt',
                'shutdown',
                'socket',
              ],
          target,
        )
      }
    }),
  30_000,
)

it.effect('retains scoped connection cleanup across cancellation parking', () =>
  Effect.gen(function* () {
    const snapshot = yield* AnalysisFixture.retainingMain(
      'native-socket/cancellation-cleanup',
      encoder.encode(nativeSocketCancellationAnalysisSource),
      'x86_64-unknown-linux-gnu',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    if (snapshot.mir._tag === 'Available') {
      const parks = snapshot.mir.value.functions
        .flatMap(MirVerification.operations)
        .filter((operation) => operation._tag === 'ExecutionPark')
      assert.isAtLeast(parks.length, 1)
      assert.isTrue(parks.some((park) => park.guardCleanup._tag !== 'NoCleanup'))
    }
  }),
)

it('exports one profile-agnostic native corpus program with independent ABI witnesses', () => {
  assert.strictEqual(nativeSocketCorpusProgram.name, 'native-socket-connections')
  assert.strictEqual(nativeSocketCorpusProgram.expected.result, 42)
  assert.include(nativeSocketCorpusProgram.nativeSource, 'connectResolved(')
  assert.include(nativeSocketCorpusProgram.nativeSource, 'connectUnix(')
  assert.include(implementation, 'close$NOCANCEL')
  assert.include(nativeSocketDarwinWitnessSource, 'sun_path) == 104')
  assert.include(nativeSocketGnuWitnessSource, 'sun_path) == 108')
})

it.effect('keeps the local pathname reference example executable', () =>
  Effect.gen(function* () {
    const opening = '```silk\n'
    const start = reference.indexOf(opening)
    const end = reference.indexOf('\n```', start + opening.length)
    assert.isAtLeast(start, 0)
    assert.isAbove(end, start)
    const example = reference.slice(start + opening.length, end)
    const snapshot = yield* AnalysisFixture.retainingMain(
      'native-socket/reference-example',
      encoder.encode(`${example}\npub fn main() -> i32 { return 42 }`),
      'x86_64-unknown-linux-gnu',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
  }),
)

it.effect(
  'compiles the host socket stub and independent platform ABI witnesses',
  () =>
    Effect.gen(function* () {
      const target = yield* NativeToolchain.hostTarget()
      yield* NativeToolchain.withBuildScope('native-socket-witnesses', (scope) =>
        Effect.gen(function* () {
          yield* NativeToolchain.compileCObject(
            toolchain,
            scope,
            target,
            'native-socket-stub',
            nativeSocketStubSource,
          )
          yield* NativeToolchain.compileCObject(
            toolchain,
            scope,
            target,
            'native-socket-darwin-witness',
            nativeSocketDarwinWitnessSource,
          )
          yield* NativeToolchain.compileCObject(
            toolchain,
            scope,
            target,
            'native-socket-gnu-witness',
            nativeSocketGnuWitnessSource,
          )
        }),
      )
    }),
  30_000,
)

const optionProbe = `
pub fn main() -> i32 {
  let options = ConnectOptions.defaults()
  if ConnectOptions.noDelay(&options) { return 1 }
  if ConnectOptions.pollInterval(&options) != 1000000 { return 2 }
  if ConnectOptions.maxAttempts(&options) != 64 { return 3 }
  return 42
}
`

it.effect(
  'realizes the selected native socket actor and excludes it on Wasm',
  () =>
    Effect.gen(function* () {
      for (const target of [
        'x86_64-unknown-linux-gnu',
        'aarch64-apple-darwin',
        'wasm32-unknown-unknown',
      ] as const) {
        const entry =
          target === 'wasm32-unknown-unknown' ? 'pub fn main() -> i32 { return 42 }' : optionProbe
        const snapshot = yield* AnalysisFixture.retainingMain(
          `native-socket/${target}`,
          encoder.encode(`${implementation}\n${entry}`),
          target,
        )
        assert.deepEqual(
          Analysis.diagnostics(snapshot).map((diagnostic) => ({
            code: diagnostic.code,
            message: diagnostic.message,
            start: diagnostic.span.start,
          })),
          [],
        )
        assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
        if (target === 'wasm32-unknown-unknown') {
          assert.deepEqual(Analysis.instancesOf(snapshot).foreignCalls, [])
        }
      }
    }),
  30_000,
)

it.effect('rejects extracting ownership from a borrowed connection', () =>
  Effect.gen(function* () {
    const source = `${implementation}
fn steal(connection: &mut Connection) -> Connection { return move connection.* }
pub fn main() -> i32 { return 42 }
`
    const snapshot = yield* AnalysisFixture.retainingMain(
      'native-socket/ownership-rejection',
      encoder.encode(source),
      'x86_64-unknown-linux-gnu',
    )
    const diagnostics = Analysis.diagnostics(snapshot)
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.code),
      ['OWN0012'],
    )
    const diagnostic = diagnostics.at(0)
    assert.isDefined(diagnostic)
    if (diagnostic !== undefined)
      assert.strictEqual(
        source.slice(diagnostic.span.start, diagnostic.span.end),
        ' move connection.*',
      )
  }),
)
