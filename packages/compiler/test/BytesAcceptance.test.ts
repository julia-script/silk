import * as AnalysisFixture from './support/AnalysisFixture.js'
import { assert, it } from '@effect/vitest'
import { createHash } from 'node:crypto'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as CleanupPlan from '../src/CleanupPlan.js'
import certificateProfileFixtures from './fixtures/certificate-profile-limbo.json' with { type: 'json' }
import certificatePathFixtures from './fixtures/certificate-path-limbo.json' with { type: 'json' }

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it('pins certificate-profile fixture provenance and DER digests', () => {
  assert.strictEqual(
    certificateProfileFixtures.source,
    'https://github.com/C2SP/x509-limbo/blob/3f8cba420e90322223486086054401189b7b320e/limbo.json',
  )
  assert.strictEqual(
    certificateProfileFixtures.sourceSha256,
    '563805f46937ad25ac9d4e41341c414070aced32a22294821b5c5fe526e2c52d',
  )
  assert.strictEqual(certificateProfileFixtures.license, 'Apache-2.0')
  for (const fixture of certificateProfileFixtures.fixtures) {
    assert.strictEqual(
      createHash('sha256').update(Buffer.from(fixture.der, 'base64')).digest('hex'),
      fixture.sha256,
      fixture.id,
    )
    assert.isNotEmpty(fixture.profileOutcome, fixture.id)
  }
})

it('pins certificate-path fixture provenance, ordering, and DER/key digests', () => {
  assert.strictEqual(
    certificatePathFixtures.source,
    'https://github.com/C2SP/x509-limbo/blob/3f8cba420e90322223486086054401189b7b320e/limbo.json',
  )
  assert.strictEqual(
    certificatePathFixtures.sourceSha256,
    '563805f46937ad25ac9d4e41341c414070aced32a22294821b5c5fe526e2c52d',
  )
  assert.strictEqual(
    certificatePathFixtures.zigHttpParityCommit,
    '1bc892110da738d6137b3f0b7e8e3a586ce09928',
  )
  assert.strictEqual(certificatePathFixtures.cases.length, 22)
  for (const fixture of certificatePathFixtures.cases) {
    const certificates = [fixture.peer, ...fixture.intermediates, ...fixture.anchors]
    for (const certificate of certificates) {
      assert.strictEqual(
        createHash('sha256').update(Buffer.from(certificate.der, 'base64')).digest('hex'),
        certificate.sha256,
        fixture.id,
      )
    }
    assert.strictEqual(fixture.peerKeyPemSha256.length, 64, fixture.id)
    assert.deepEqual(
      fixture.ordering.intermediates,
      fixture.intermediates.map((_, index) => index),
      fixture.id,
    )
    assert.deepEqual(
      fixture.ordering.anchors,
      fixture.anchors.map((_, index) => index),
      fixture.id,
    )
  }
  assert.deepEqual(
    certificatePathFixtures.cases.find(
      (fixture) => fixture.id === 'rfc5280::nc::nc-forbids-alternate-chain-ica',
    )?.expectedSilk,
    { result: 'Success', anchorIndex: 0, intermediateIndices: [2, 0] },
  )
  assert.deepEqual(
    certificatePathFixtures.projectFixtures.map((fixture) => fixture.id),
    [
      'source::rfc5280::no-keyusage/peer_certificate',
      'silk::wrong-signature-first/intermediates[0]',
      'silk::ignored-anchor-validity-self-signature/trusted_certs[0]',
      'silk::missing-ec-parameters/trusted_certs[0]',
      'silk::unsupported-signature-parameters/intermediates[0]',
      'silk::unsupported-signature-algorithm/intermediates[0]',
      'silk::mismatching-signature-algorithms/intermediates[0]',
      'silk::certificate-policy/intermediates[0]',
      'silk::tls-feature/intermediates[0]',
    ],
  )
  for (const fixture of certificatePathFixtures.projectFixtures) {
    assert.strictEqual(
      createHash('sha256').update(Buffer.from(fixture.der, 'base64')).digest('hex'),
      fixture.sha256,
      fixture.id,
    )
    assert.isNotEmpty(fixture.sourceCase, fixture.id)
    assert.isNotEmpty(fixture.sourceCertificateRole, fixture.id)
    assert.strictEqual(fixture.sourceCertificateSha256.length, 64, fixture.id)
    assert.isNotEmpty(fixture.mutation, fixture.id)
    assert.match(fixture.expectedSilk.result, /^(Success|Failure)$/, fixture.id)
  }
})

it.effect(
  'keeps Bytes move-only and rejects exclusive field projection through shared access',
  () =>
    Effect.gen(function* () {
      const moved = yield* AnalysisFixture.retainingMain(
        'bytes-acceptance/moved',
        ascii(`import silk.usize
import silk.bytes { Bytes }
pub fn main() -> i32 {
  let first = Bytes.make()
  let second = move first
  return usize.toI32(Bytes.length(&first))
}`),
      )
      assert.include(
        Analysis.diagnostics(moved).map((diagnostic) => diagnostic.code),
        'OWN0001',
      )

      const shared = yield* AnalysisFixture.retainingMain(
        'bytes-acceptance/shared-field',
        ascii(`struct Wrapper { values: [u8; 1] }
fn consume(values: &mut [u8]) -> () { return () }
fn invalid(self: &Wrapper) -> () { return consume(&mut self.values) }
pub fn main() -> i32 { return 0 }`),
      )
      assert.include(
        Analysis.diagnostics(shared).map((diagnostic) => diagnostic.code),
        'SEM0057',
      )
    }),
)

it.effect('keeps decoded certificate owners move-only and their returned views borrowed', () =>
  Effect.gen(function* () {
    const source = `import silk.certificate { Certificate }
import silk.certificate_bundle { CertificateBundle }
import silk.certificate_profile { CertificateProfile, CertificateRole, ProfileLimits, ProfileError }
import silk.certificate_path { CertificatePath }
import silk.result { Result }
import silk.trust_anchor { TrustAnchor }
import silk.usize
fn moved(value: Certificate) -> usize {
  let next = move value
  return Certificate.extensionCount(&value)
}
fn bundleMoved(value: CertificateBundle) -> usize {
  let next = move value
  return CertificateBundle.length(&value)
}
fn viewed(value: Certificate) -> usize {
  let bytes = Certificate.der(&value)
  drop value
  return bytes.length
}
fn profile<'a>(certificate: &'a Certificate) -> Result<CertificateProfile<'a>, ProfileError> {
  return CertificateProfile.inspect(certificate, CertificateRole.Anchor, ProfileLimits.defaults())
}
fn anchor(certificate: Certificate) -> usize {
  let value = TrustAnchor.fromCertificate(move certificate)
  return TrustAnchor.encodedBytes(&value)
}
fn anchorMoved(value: TrustAnchor) -> usize {
  let next = move value
  return TrustAnchor.encodedBytes(&value)
}
pub fn main() -> i32 { return 0 }`
    const ownership = yield* AnalysisFixture.retainingMain(
      'certificate-acceptance/owner-and-view',
      ascii(source),
    )
    // The same snapshot includes decoder ownership facts: partial DER traversal/index storage,
    // decoded PEM bytes, and a partially accumulated bundle must have reclaiming failure exits.
    const partialStates = [
      { module: 'silk/certificate', operation: 'openValue', bindings: ['frames'] },
      { module: 'silk/certificate', operation: 'Certificate.decodeDer', bindings: ['extensions'] },
      { module: 'silk/certificate', operation: 'Certificate.decodePem', bindings: ['value'] },
      {
        module: 'silk/certificate_bundle',
        operation: 'CertificateBundle.decodePem',
        bindings: ['certificates', 'block', 'certificate'],
      },
      {
        module: 'silk/certificate',
        operation: 'Certificate.copy',
        bindings: ['bytes', 'extensions'],
      },
      {
        module: 'silk/trust_anchor',
        operation: 'TrustAnchor.fromCertificateWithConstraints',
        bindings: ['certificate'],
      },
      { module: 'silk/trust_anchor', operation: 'TrustAnchor.clone', bindings: ['certificate'] },
      {
        module: 'silk/certificate_path',
        operation: 'CertificatePath.validate',
        bindings: ['path', 'frames'],
      },
    ]
    for (const state of partialStates) {
      const operation = Analysis.ownershipOf(ownership, state.module)?.functions.find(
        (candidate) =>
          candidate.declaration.canonical._tag === 'Canonical' &&
          candidate.declaration.canonical.id.name === state.operation,
      )
      const releases =
        operation?.exits
          .filter((exit) => exit.kind === 'Propagation')
          .flatMap((exit) => exit.releases) ?? []
      for (const binding of state.bindings) {
        const partial = releases.filter((release) => release.binding.name === binding)
        assert.isNotEmpty(partial, `${state.operation}: ${binding}`)
        assert.isTrue(
          partial.every((release) => CleanupPlan.reclaims(release.cleanup)),
          `${state.operation}: ${binding}`,
        )
      }
    }
    const diagnostics = Analysis.diagnostics(ownership)
    assert.deepEqual(
      diagnostics.map((diagnostic) => ({
        code: diagnostic.code,
        text: source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      })),
      [
        { code: 'OWN0001', text: '&value' },
        { code: 'OWN0001', text: '&value' },
        { code: 'OWN0011', text: 'value' },
        { code: 'OWN0019', text: 'bytes.length' },
        { code: 'OWN0001', text: '&value' },
      ],
    )
  }),
)

it.effect('keeps HTTPS reference and SAN payload borrows within caller storage', () =>
  Effect.gen(function* () {
    const source = `import silk.https_identity { HttpsIdentity, OriginHost, ReferenceIdentity, CertificateIdentities, PresentedIdentity, IdentityLimits, IdentityMatch, IdentityError }
import silk.result { Result }
import silk.certificate { Certificate }
import silk.certificate_identities { CertificateSan, SanDecodeLimits, SanDecodeSummary, SanDecodeError }
import silk.usize
fn decodeBorrowed<'a>(certificate: &'a Certificate, storage: &mut [PresentedIdentity<'a>]) -> Result<SanDecodeSummary, SanDecodeError> {
  return CertificateSan.decode(certificate, &mut storage, SanDecodeLimits.standard())
}
fn decodeEscapes<'a>(certificate: Certificate, storage: &mut [PresentedIdentity<'a>]) -> Result<SanDecodeSummary, SanDecodeError> {
  return CertificateSan.decode(&certificate, &mut storage, SanDecodeLimits.standard())
}
fn forwarded<'a>(bytes: &'a [u8]) -> Result<ReferenceIdentity<'a>, IdentityError> {
  return HttpsIdentity.reference(OriginHost.Dns { bytes: bytes })
}
fn escaped<'a>(input: &'a [u8]) -> Result<ReferenceIdentity<'a>, IdentityError> {
  let bytes: [u8; 1] = [97]
  return HttpsIdentity.reference(OriginHost.Dns { bytes: &bytes })
}
fn certificateView<'a>(entries: &'a [PresentedIdentity<'a>]) -> CertificateIdentities<'a> {
  return CertificateIdentities<'a>.Decoded { entries: entries }
}
fn conflict() -> Result<IdentityMatch, IdentityError> {
  let mut bytes: [u8; 1] = [97]
  let reference = ReferenceIdentity.Dns { bytes: &bytes }
  let entries = [PresentedIdentity.Dns { bytes: b"a" }]
  let certificate = certificateView(&entries)
  bytes[0] = 98
  return HttpsIdentity.verify(&reference, &certificate, IdentityLimits.standard())
}
pub fn main() -> i32 { return 0 }`
    const snapshot = yield* AnalysisFixture.retainingMain('https-identity/ownership', ascii(source))
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => ({
        code: diagnostic.code,
        span: source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      })),
      [
        { code: 'SEM0212', span: '&certificate' },
        { code: 'OWN0019', span: 'HttpsIdentity.reference(OriginHost.Dns { bytes: &bytes })' },
        { code: 'OWN0019', span: 'HttpsIdentity.reference(OriginHost.Dns { bytes: &bytes })' },
        { code: 'SEM0212', span: '&bytes' },
        { code: 'OWN0011', span: 'bytes[0]' },
        { code: 'OWN0019', span: '&reference' },
      ],
    )
  }),
)

it.effect('binds validated certificate paths to every borrowed certificate input', () =>
  Effect.gen(function* () {
    const source = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.certificate { Certificate }
import silk.certificate_path { CertificatePath, ValidatedPath, ValidationError, ValidationLimits }
import silk.result { Result }
import silk.system_clock { Instant }
import silk.trust_anchor { TrustAnchor }
effect fn forwarded<'a>(
  leaf: &'a Certificate,
  intermediates: &'a [Certificate],
  anchors: &'a [TrustAnchor],
  at: &Instant,
) -> Result<ValidatedPath<'a>, ValidationError> ! OutOfMemoryError ? &mut Allocator {
  return run CertificatePath.validate(leaf, intermediates, anchors, at, ValidationLimits.defaults())
}
effect fn leafEscapes<'a>(
  leaf: Certificate,
  intermediates: &'a [Certificate],
  anchors: &'a [TrustAnchor],
  at: &Instant,
) -> Result<ValidatedPath<'a>, ValidationError> ! OutOfMemoryError ? &mut Allocator {
  return run CertificatePath.validate(&leaf, intermediates, anchors, at, ValidationLimits.defaults())
}
effect fn intermediatesEscape<'a>(
  leaf: &'a Certificate,
  intermediates: [Certificate; 1],
  anchors: &'a [TrustAnchor],
  at: &Instant,
) -> Result<ValidatedPath<'a>, ValidationError> ! OutOfMemoryError ? &mut Allocator {
  return run CertificatePath.validate(leaf, &intermediates, anchors, at, ValidationLimits.defaults())
}
effect fn anchorsEscape<'a>(
  leaf: &'a Certificate,
  intermediates: &'a [Certificate],
  anchors: [TrustAnchor; 1],
  at: &Instant,
) -> Result<ValidatedPath<'a>, ValidationError> ! OutOfMemoryError ? &mut Allocator {
  return run CertificatePath.validate(leaf, intermediates, &anchors, at, ValidationLimits.defaults())
}
fn moved<'a>(value: ValidatedPath<'a>) -> usize {
  let next = move value
  return value.anchorIndex()
}
pub fn main() -> i32 { return 0 }`
    const snapshot = yield* Analysis.ofSourceRealized(
      'certificate-path/borrowed-result',
      ascii(source),
    )
    const diagnostics = Analysis.diagnostics(snapshot).map((diagnostic) => ({
      code: diagnostic.code,
      span: source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
    }))
    const distinct = diagnostics.filter(
      (diagnostic, index) =>
        diagnostics.findIndex(
          (candidate) => candidate.code === diagnostic.code && candidate.span === diagnostic.span,
        ) === index,
    )
    assert.deepEqual(distinct, [
      {
        code: 'OWN0019',
        span: 'run CertificatePath.validate(&leaf, intermediates, anchors, at, ValidationLimits.defaults())',
      },
      { code: 'SEM0212', span: '&leaf' },
      {
        code: 'OWN0019',
        span: 'run CertificatePath.validate(leaf, &intermediates, anchors, at, ValidationLimits.defaults())',
      },
      { code: 'SEM0212', span: '&intermediates' },
      {
        code: 'OWN0019',
        span: 'run CertificatePath.validate(leaf, intermediates, &anchors, at, ValidationLimits.defaults())',
      },
      { code: 'SEM0212', span: '&anchors' },
      { code: 'OWN0001', span: 'value' },
    ])
  }),
)
