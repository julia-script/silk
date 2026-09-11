import * as AnalysisFixture from './support/AnalysisFixture.js'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as CleanupPlan from '../src/CleanupPlan.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

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
