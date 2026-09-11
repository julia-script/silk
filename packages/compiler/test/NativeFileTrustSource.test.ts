import { createHash, X509Certificate } from 'node:crypto'
import { readFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Target from '../src/Target.js'
import * as AnalysisFixture from './support/AnalysisFixture.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const constructorSource = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.bytes { Bytes }
import silk.filesystem { Path }
import silk.native_file_trust_source { NativeFileTrustSource }
import silk.trust_snapshot { TrustSourceError }
pub effect fn construct(root: Bytes, path: Path) -> NativeFileTrustSource
! TrustSourceError | OutOfMemoryError
? &mut Allocator {
  let provider = run NativeFileTrustSource.make(Bytes.asSlice(&root), &path)
  drop root
  drop path
  return move provider
}`

it.effect('owns accepted configuration and selects all three verified libc profiles', () =>
  Effect.gen(function* () {
    for (const target of Target.native) {
      const snapshot = yield* AnalysisFixture.declarations(
        `native-file-trust-source/${target.id}`,
        ascii(constructorSource),
        target.id,
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], target.id)
    }
  }),
)

it.effect('omits the provider from Wasm and every no-libc profile', () =>
  Effect.gen(function* () {
    const source =
      'import silk.native_file_trust_source { NativeFileTrustSource }\npub fn main() -> i32 { return 42 }'
    for (const target of Target.all) {
      const snapshot = yield* Analysis.makeRealized({
        root: SourceFile.make(`native-file-trust-source/unavailable/${target.id}`, ascii(source)),
        configuration: {
          profile: { target: target.id, artifact: 'object', libc: 'none', entry: { kind: 'none' } },
        },
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => [
          diagnostic.code,
          diagnostic.span.start,
          diagnostic.span.end,
        ]),
        [
          [
            'SEM0014',
            source.indexOf('NativeFileTrustSource'),
            source.indexOf('NativeFileTrustSource') + 'NativeFileTrustSource'.length,
          ],
        ],
        target.id,
      )
      assert.deepEqual(snapshot.instances.foreignCalls, [], target.id)
    }
  }),
)

it('pins the synthetic PEM and decoded DER identities', () => {
  const fixture = readFileSync(
    fileURLToPath(new URL('../conformance/native-filesystem/trust.pem', import.meta.url)),
  )
  const certificate = new X509Certificate(fixture)
  assert.strictEqual(fixture.length, 603)
  assert.strictEqual(
    createHash('sha256').update(fixture).digest('hex'),
    'e409d0b059a0e9124f42c0dbf95b2611525d605de9ef591a5f48e6e8754fe7b6',
  )
  assert.strictEqual(certificate.raw.length, 404)
  assert.strictEqual(
    createHash('sha256').update(certificate.raw).digest('hex'),
    '2ffcf7efc81cbd3f7f2aa126fa6de0663e011ed71e7cdf2bec63a6c2af828455',
  )
})
