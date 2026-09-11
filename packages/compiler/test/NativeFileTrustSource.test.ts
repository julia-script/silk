import { createHash, X509Certificate } from 'node:crypto'
import { readFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceCatalog from '../src/SourceCatalog.js'
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

it.effect('owns accepted root and path configuration', () =>
  Effect.gen(function* () {
    const snapshot = yield* AnalysisFixture.declarations(
      'native-file-trust-source/ownership',
      ascii(constructorSource),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect('selects only the three verified native libc profiles', () =>
  Effect.gen(function* () {
    const source = 'import silk.native_file_trust_source as provider'
    for (const target of Target.all) {
      const selection = yield* SourceCatalog.analyze({
        roots: [SourceFile.make(`native-file-trust-source/${target.id}`, ascii(source))],
        configuration: {
          profile: { target: target.id, artifact: 'object', runtime: { kind: 'none' } },
        },
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.deepEqual(selection.closure.resolutionFailures, [], target.id)
      assert.deepEqual(selection.closure.diagnostics, [], target.id)
      assert.strictEqual(
        (selection.catalog?.modules.get('silk/native_file_trust_source')?.publicDeclarations
          .length ?? 0) > 0,
        target.kind === 'Native',
        target.id,
      )
    }
    for (const target of Target.all) {
      const selection = yield* SourceCatalog.analyze({
        roots: [SourceFile.make(`native-file-trust-source/no-libc/${target.id}`, ascii(source))],
        configuration: {
          profile: {
            target: target.id,
            artifact: 'object',
            libc: 'none',
            runtime: { kind: 'none' },
          },
        },
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.deepEqual(selection.closure.resolutionFailures, [], target.id)
      assert.deepEqual(selection.closure.diagnostics, [], target.id)
      assert.strictEqual(
        selection.catalog?.modules.get('silk/native_file_trust_source')?.publicDeclarations.length,
        0,
        target.id,
      )
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
