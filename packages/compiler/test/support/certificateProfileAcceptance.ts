import fixtures from '../fixtures/certificate-profile-limbo.json' with { type: 'json' }

const literal = (base64: string): string => {
  const bytes = Buffer.from(base64, 'base64')
  return `b"${Array.from(bytes, (byte) => `\\x${byte.toString(16).padStart(2, '0')}`).join('')}"`
}

const fixture = (id: string): string => {
  const selected = fixtures.fixtures.find((candidate) => candidate.id === id)
  if (selected === undefined) throw new Error(`Missing certificate-profile fixture ${id}`)
  return literal(selected.der)
}

const fixtureBytes = (id: string): Buffer => {
  const selected = fixtures.fixtures.find((candidate) => candidate.id === id)
  if (selected === undefined) throw new Error(`Missing certificate-profile fixture ${id}`)
  return Buffer.from(selected.der, 'base64')
}

const mutateUnique = (
  id: string,
  needle: ReadonlyArray<number>,
  mutate: (bytes: Buffer, offset: number) => void,
): string => {
  const bytes = fixtureBytes(id)
  const found: Array<number> = []
  for (let offset = 0; offset <= bytes.length - needle.length; offset += 1) {
    if (needle.every((value, index) => bytes[offset + index] === value)) found.push(offset)
  }
  if (found.length !== 1) {
    throw new Error(`Expected one ${id} mutation site, found ${found.length}`)
  }
  const offset = found[0]
  if (offset === undefined) throw new Error(`Missing ${id} mutation offset`)
  mutate(bytes, offset)
  return literal(bytes.toString('base64'))
}

const mutateFinalBitStringUnused = (id: string): string => {
  const bytes = fixtureBytes(id)
  for (let offset = bytes.length - 1; offset >= 0; offset -= 1) {
    if (bytes[offset] !== 0x03) continue
    const firstLength = bytes[offset + 1]
    if (firstLength === undefined) continue
    const lengthOctets = (firstLength & 0x80) === 0 ? 0 : firstLength & 0x7f
    let length = (firstLength & 0x80) === 0 ? firstLength : 0
    for (let index = 0; index < lengthOctets; index += 1) {
      const octet = bytes[offset + 2 + index]
      if (octet === undefined) throw new Error(`Truncated final BIT STRING in ${id}`)
      length = length * 256 + octet
    }
    const headerLength = 2 + lengthOctets
    if (offset + headerLength + length !== bytes.length || length === 0) continue
    const finalOctet = bytes.at(-1)
    if (finalOctet === undefined) throw new Error(`Missing final BIT STRING payload in ${id}`)
    bytes[offset + headerLength] = 1
    bytes[bytes.length - 1] = finalOctet & 0xfe
    return literal(bytes.toString('base64'))
  }
  throw new Error(`Missing final BIT STRING in ${id}`)
}

const mismatchOuterSignatureAlgorithm = (id: string): string => {
  const bytes = fixtureBytes(id)
  const ecdsaSha256 = Buffer.from([
    0x30, 0x0a, 0x06, 0x08, 0x2a, 0x86, 0x48, 0xce, 0x3d, 0x04, 0x03, 0x02,
  ])
  const rsaPkcs1Sha256 = Buffer.from([
    0x30, 0x0d, 0x06, 0x09, 0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0b, 0x05, 0x00,
  ])
  const first = bytes.indexOf(ecdsaSha256)
  const second = bytes.indexOf(ecdsaSha256, first + 1)
  if (first < 0 || second < 0 || bytes.indexOf(ecdsaSha256, second + 1) >= 0) {
    throw new Error(`Expected exactly two ECDSA signature algorithms in ${id}`)
  }
  if (bytes[0] !== 0x30 || bytes[1] !== 0x82 || bytes.readUInt16BE(2) !== bytes.length - 4) {
    throw new Error(`Unexpected certificate envelope in ${id}`)
  }
  const result = Buffer.concat([
    bytes.subarray(0, second),
    rsaPkcs1Sha256,
    bytes.subarray(second + ecdsaSha256.length),
  ])
  result.writeUInt16BE(result.length - 4, 2)
  return literal(result.toString('base64'))
}

const leaf = fixture('rfc5280::no-keyusage/peer_certificate')
const root = fixture('rfc5280::no-keyusage/trusted_certs[0]')
const zeroSerial = fixture('rfc5280::serial::zero/peer_certificate')
const wrongEku = fixture('rfc5280::eku::ee-wrong-eku/peer_certificate')
const wildcardConstraint = fixture('rfc5280::nc::invalid-dnsname-wildcard/trusted_certs[0]')
const duplicateExtension = mutateUnique(
  'rfc5280::no-keyusage/trusted_certs[0]',
  [0x06, 0x03, 0x55, 0x1d, 0x0f],
  (bytes, offset) => {
    bytes[offset + 4] = 0x13
  },
)
const unknownCriticalExtension = mutateUnique(
  'rfc5280::no-keyusage/trusted_certs[0]',
  [0x06, 0x03, 0x55, 0x1d, 0x13],
  (bytes, offset) => {
    bytes[offset + 4] = 0x7f
  },
)
const malformedBasicConstraints = mutateUnique(
  'rfc5280::no-keyusage/trusted_certs[0]',
  [0x04, 0x05, 0x30, 0x03, 0x01, 0x01, 0xff],
  (bytes, offset) => {
    bytes[offset + 6] = 0x01
  },
)
const publicKeyUnusedBits = mutateUnique(
  'rfc5280::no-keyusage/peer_certificate',
  [0x03, 0x42, 0x00, 0x04],
  (bytes, offset) => {
    const finalOctet = bytes[offset + 67]
    if (finalOctet === undefined) throw new Error('Missing public-key BIT STRING payload')
    bytes[offset + 2] = 1
    bytes[offset + 67] = finalOctet & 0xfe
  },
)
const signatureUnusedBits = mutateFinalBitStringUnused('rfc5280::no-keyusage/peer_certificate')
const mismatchedSignatureAlgorithm = mismatchOuterSignatureAlgorithm(
  'rfc5280::no-keyusage/peer_certificate',
)
const invalidSignature = mutateUnique(
  'rfc5280::no-keyusage/peer_certificate',
  [0x30, 0x45, 0x02, 0x21],
  (bytes) => {
    const finalOctet = bytes.at(-1)
    if (finalOctet === undefined) throw new Error('Missing certificate signature payload')
    bytes[bytes.length - 1] = finalOctet ^ 0x01
  },
)

/**
 * One consolidated semantic-profile and owned-anchor witness. The selected certificates retain
 * their x509-limbo IDs, source pin, license, DER hashes, and profile-specific expected outcomes in
 * certificate-profile-limbo.json; this program does not claim full path validation.
 */
export const certificateProfileAcceptanceSource = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.certificate { Certificate, DecodeError, DecodeLimits, ExtensionView }
import silk.certificate_profile { CertificateProfile, CertificateRole, ProfileClass, ProfileError, ProfileLimits, ProfileOffsetSpace, ProfileReason }
import silk.effect { Effect }
import silk.option { Option }
import silk.result { Result }
import silk.trust_anchor { TrustAnchor }
import silk.usize

fn profileFailure<'a>(
  outcome: Result<CertificateProfile<'a>, ProfileError>,
  kind: ProfileClass,
  reason: ProfileReason,
) -> bool {
  return match move outcome {
    Result<CertificateProfile<'a>, ProfileError>.Success {value} => false
    Result<CertificateProfile<'a>, ProfileError>.Failure {error} => error.kind == kind && error.reason == reason
  }
}

fn profilesLink<'leaf, 'root>(
  leaf: Result<CertificateProfile<'leaf>, ProfileError>,
  root: Result<CertificateProfile<'root>, ProfileError>,
) -> bool {
  return match move leaf {
    Result<CertificateProfile<'leaf>, ProfileError>.Failure {error} => false
    Result<CertificateProfile<'leaf>, ProfileError>.Success {value: leafProfile} => {
      return match move root {
        Result<CertificateProfile<'root>, ProfileError>.Failure {error} => false
        Result<CertificateProfile<'root>, ProfileError>.Success {value: rootProfile} => {
          return match move CertificateProfile.verifyIssuedBy(&leafProfile, &rootProfile) {
            Result<(), ProfileError>.Failure {error} => false
            Result<(), ProfileError>.Success {value} => true
          }
        }
      }
    }
  }
}

fn profilesReject<'subject, 'issuer>(
  subject: Result<CertificateProfile<'subject>, ProfileError>,
  issuer: Result<CertificateProfile<'issuer>, ProfileError>,
  reason: ProfileReason,
) -> bool {
  return match move subject {
    Result<CertificateProfile<'subject>, ProfileError>.Failure {error} => false
    Result<CertificateProfile<'subject>, ProfileError>.Success {value: subjectProfile} => {
      return match move issuer {
        Result<CertificateProfile<'issuer>, ProfileError>.Failure {error} => false
        Result<CertificateProfile<'issuer>, ProfileError>.Success {value: issuerProfile} => {
          return match move CertificateProfile.verifyIssuedBy(&subjectProfile, &issuerProfile) {
            Result<(), ProfileError>.Success {value} => false
            Result<(), ProfileError>.Failure {error} => error.reason == reason
          }
        }
      }
    }
  }
}

fn pathLength(value: Option<usize>, expected: usize) -> bool {
  return match move value {
    Option<usize>.None => false
    Option<usize>.Some {value: found} => found == expected
  }
}

fn equalBytes(left: &[u8], right: &[u8]) -> bool {
  if left.length != right.length { return false }
  let mut index = usize.ZERO
  while index < left.length {
    if left[index] != right[index] { return false }
    index = index + usize.ONE
  }
  return true
}

fn optionBytes(value: Option<&[u8]>, expected: &[u8]) -> bool {
  return match move value {
    Option<&[u8]>.None => false
    Option<&[u8]>.Some {value: found} => equalBytes(found, expected)
  }
}

fn sameExtension<'left, 'right>(
  left: Option<ExtensionView<'left>>,
  right: Option<ExtensionView<'right>>,
) -> bool {
  return match move left {
    Option<ExtensionView<'left>>.None => match move right {
      Option<ExtensionView<'right>>.None => true
      Option<ExtensionView<'right>>.Some {value} => false
    }
    Option<ExtensionView<'left>>.Some {value: leftValue} => match move right {
      Option<ExtensionView<'right>>.None => false
      Option<ExtensionView<'right>>.Some {value: rightValue} => {
        return leftValue.critical == rightValue.critical
            && leftValue.offset == rightValue.offset
            && leftValue.valueOffset == rightValue.valueOffset
            && equalBytes(leftValue.der, rightValue.der)
            && equalBytes(leftValue.oid, rightValue.oid)
            && equalBytes(leftValue.value, rightValue.value)
      }
    }
  }
}

fn sameCertificate(left: &Certificate, right: &Certificate) -> bool {
  if !equalBytes(Certificate.der(left), Certificate.der(right)) { return false }
  let count = Certificate.extensionCount(left)
  if count != Certificate.extensionCount(right) { return false }
  let mut index = usize.ZERO
  while index < count {
    if !sameExtension(Certificate.extension(left, index), Certificate.extension(right, index)) {
      return false
    }
    index = index + usize.ONE
  }
  return true
}

effect fn decoded(input: &[u8]) -> Result<Certificate, DecodeError>
! OutOfMemoryError
? &mut Allocator {
  return run Certificate.decodeDer(input, DecodeLimits.defaults())
}

effect fn suite() -> i32 ! OutOfMemoryError ? &mut Allocator {
  let leafResult = run decoded(${leaf})
  let leafCertificate = match move leafResult {
    Result<Certificate, DecodeError>.Failure {error} => { return 1 }
    Result<Certificate, DecodeError>.Success {value} => move value
  }
  let rootResult = run decoded(${root})
  let rootCertificate = match move rootResult {
    Result<Certificate, DecodeError>.Failure {error} => { return 2 }
    Result<Certificate, DecodeError>.Success {value} => move value
  }
  let inspectedLeaf = CertificateProfile.inspect(
    &leafCertificate,
    CertificateRole.ServerLeaf,
    ProfileLimits.defaults(),
  )
  let inspectedRoot = CertificateProfile.inspect(
    &rootCertificate,
    CertificateRole.Anchor,
    ProfileLimits.defaults(),
  )
  if !profilesLink(move inspectedLeaf, move inspectedRoot) { return 5 }
  if !profilesReject(
    CertificateProfile.inspect(&rootCertificate, CertificateRole.Anchor, ProfileLimits.defaults()),
    CertificateProfile.inspect(&leafCertificate, CertificateRole.ServerLeaf, ProfileLimits.defaults()),
    ProfileReason.IssuerName,
  ) { return 22 }

  let invalidSignatureResult = run decoded(${invalidSignature})
  let invalidSignatureCertificate = match move invalidSignatureResult {
    Result<Certificate, DecodeError>.Failure {error} => { return 23 }
    Result<Certificate, DecodeError>.Success {value} => move value
  }
  if !profilesReject(
    CertificateProfile.inspect(&invalidSignatureCertificate, CertificateRole.ServerLeaf, ProfileLimits.defaults()),
    CertificateProfile.inspect(&rootCertificate, CertificateRole.Anchor, ProfileLimits.defaults()),
    ProfileReason.SignatureRejected,
  ) { return 24 }

  let zeroResult = run decoded(${zeroSerial})
  let zeroCertificate = match move zeroResult {
    Result<Certificate, DecodeError>.Failure {error} => { return 6 }
    Result<Certificate, DecodeError>.Success {value} => move value
  }
  if !profileFailure(
    CertificateProfile.inspect(&zeroCertificate, CertificateRole.ServerLeaf, ProfileLimits.defaults()),
    ProfileClass.Malformed,
    ProfileReason.Serial,
  ) { return 7 }

  let wrongResult = run decoded(${wrongEku})
  let wrongCertificate = match move wrongResult {
    Result<Certificate, DecodeError>.Failure {error} => { return 8 }
    Result<Certificate, DecodeError>.Success {value} => move value
  }
  if !profileFailure(
    CertificateProfile.inspect(&wrongCertificate, CertificateRole.ServerLeaf, ProfileLimits.defaults()),
    ProfileClass.Unsupported,
    ProfileReason.MissingServerAuth,
  ) { return 9 }
  let unsupportedStored = TrustAnchor.fromCertificate(move wrongCertificate)
  if TrustAnchor.encodedBytes(&unsupportedStored) != ${Buffer.from(fixtures.fixtures.find((value) => value.id === 'rfc5280::eku::ee-wrong-eku/peer_certificate')?.der ?? '', 'base64').length} { return 10 }

  let wildcardResult = run decoded(${wildcardConstraint})
  let wildcardCertificate = match move wildcardResult {
    Result<Certificate, DecodeError>.Failure {error} => { return 11 }
    Result<Certificate, DecodeError>.Success {value} => move value
  }
  if !profileFailure(
    CertificateProfile.inspect(&wildcardCertificate, CertificateRole.Anchor, ProfileLimits.defaults()),
    ProfileClass.Malformed,
    ProfileReason.NameConstraints,
  ) { return 12 }

  let duplicateResult = run decoded(${duplicateExtension})
  let duplicateCertificate = match move duplicateResult {
    Result<Certificate, DecodeError>.Failure {error} => { return 25 }
    Result<Certificate, DecodeError>.Success {value} => move value
  }
  if !profileFailure(
    CertificateProfile.inspect(&duplicateCertificate, CertificateRole.Anchor, ProfileLimits.defaults()),
    ProfileClass.Malformed,
    ProfileReason.DuplicateExtension,
  ) { return 26 }

  let criticalResult = run decoded(${unknownCriticalExtension})
  let criticalCertificate = match move criticalResult {
    Result<Certificate, DecodeError>.Failure {error} => { return 27 }
    Result<Certificate, DecodeError>.Success {value} => move value
  }
  if !profileFailure(
    CertificateProfile.inspect(&criticalCertificate, CertificateRole.Anchor, ProfileLimits.defaults()),
    ProfileClass.Unsupported,
    ProfileReason.UnknownCriticalExtension,
  ) { return 28 }

  let malformedBcResult = run decoded(${malformedBasicConstraints})
  let malformedBcCertificate = match move malformedBcResult {
    Result<Certificate, DecodeError>.Failure {error} => { return 29 }
    Result<Certificate, DecodeError>.Success {value} => move value
  }
  if !profileFailure(
    CertificateProfile.inspect(&malformedBcCertificate, CertificateRole.Anchor, ProfileLimits.defaults()),
    ProfileClass.Malformed,
    ProfileReason.BasicConstraints,
  ) { return 30 }

  let keyBitsResult = run decoded(${publicKeyUnusedBits})
  let keyBitsCertificate = match move keyBitsResult {
    Result<Certificate, DecodeError>.Failure {error} => { return 31 }
    Result<Certificate, DecodeError>.Success {value} => move value
  }
  if !profileFailure(
    CertificateProfile.inspect(&keyBitsCertificate, CertificateRole.ServerLeaf, ProfileLimits.defaults()),
    ProfileClass.Unsupported,
    ProfileReason.PublicKey,
  ) { return 32 }

  let signatureBitsResult = run decoded(${signatureUnusedBits})
  let signatureBitsCertificate = match move signatureBitsResult {
    Result<Certificate, DecodeError>.Failure {error} => { return 33 }
    Result<Certificate, DecodeError>.Success {value} => move value
  }
  if !profileFailure(
    CertificateProfile.inspect(&signatureBitsCertificate, CertificateRole.ServerLeaf, ProfileLimits.defaults()),
    ProfileClass.Unsupported,
    ProfileReason.SignatureAlgorithm,
  ) { return 34 }

  let mismatchResult = run decoded(${mismatchedSignatureAlgorithm})
  let mismatchCertificate = match move mismatchResult {
    Result<Certificate, DecodeError>.Failure {error} => { return 35 }
    Result<Certificate, DecodeError>.Success {value} => move value
  }
  if !profileFailure(
    CertificateProfile.inspect(&mismatchCertificate, CertificateRole.ServerLeaf, ProfileLimits.defaults()),
    ProfileClass.Unsupported,
    ProfileReason.SignatureAlgorithmMismatch,
  ) { return 36 }

  let configured: [u8; 19] = [
    48, 17, 160, 15, 48, 13, 130, 11, 101, 120,
    97, 109, 112, 108, 101, 46, 99, 111, 109,
  ]
  let ownedRootResult = run decoded(${root})
  let ownedRoot = match move ownedRootResult {
    Result<Certificate, DecodeError>.Failure {error} => { return 13 }
    Result<Certificate, DecodeError>.Success {value} => move value
  }
  let anchorResult = run TrustAnchor.fromCertificateWithConstraints(
    move ownedRoot,
    Option.some<usize>(2),
    Option.some<&[u8]>(&configured),
    ProfileLimits.defaults(),
  )
  let anchor = match move anchorResult {
    Result<TrustAnchor, ProfileError>.Failure {error} => { return 14 }
    Result<TrustAnchor, ProfileError>.Success {value} => move value
  }
  if !pathLength(TrustAnchor.configuredPathLength(&anchor), 2) { return 15 }
  if !optionBytes(TrustAnchor.configuredNameConstraints(&anchor), &configured) { return 16 }
  if TrustAnchor.encodedBytes(&anchor) != ${Buffer.from(fixtures.fixtures.find((value) => value.id === 'rfc5280::no-keyusage/trusted_certs[0]')?.der ?? '', 'base64').length} + 19 { return 17 }
  let cloned = run TrustAnchor.clone(&anchor)
  drop anchor
  if !pathLength(TrustAnchor.configuredPathLength(&cloned), 2) { return 18 }
  if !optionBytes(TrustAnchor.configuredNameConstraints(&cloned), &configured) { return 19 }
  if !sameCertificate(TrustAnchor.certificate(&cloned), &rootCertificate) { return 37 }

  let malformed = CertificateProfile.validateConfiguredNameConstraints(b"\\x30\\x00", ProfileLimits.defaults())
  let malformedAccepted = match move malformed {
    Result<(), ProfileError>.Success {value} => true
    Result<(), ProfileError>.Failure {error} => error.kind != ProfileClass.Malformed
        || error.reason != ProfileReason.NameConstraints
        || error.offsetSpace != ProfileOffsetSpace.ConfiguredConstraintDer
  }
  if malformedAccepted { return 20 }
  let mut noNodes = ProfileLimits.defaults()
  noNodes.nodes = usize.ZERO
  let limited = CertificateProfile.validateConfiguredNameConstraints(&configured, noNodes)
  let limitAccepted = match move limited {
    Result<(), ProfileError>.Success {value} => true
    Result<(), ProfileError>.Failure {error} => error.kind != ProfileClass.ResourceLimit || error.reason != ProfileReason.Nodes
  }
  if limitAccepted { return 21 }
  return 42
}

effect fn allocated() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  return run suite() |> Effect.provideMut<Allocator>(&mut allocator)
}

effect fn allocationFailure(error: OutOfMemoryError) -> i32 { return 99 }

pub fn main() -> i32 { return run Effect.catchAll(allocated(), allocationFailure) }
`

/** Compact target-portability witness: unsupported certificate bytes remain storable on Wasm. */
export const certificateProfileWasmSource = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.certificate { Certificate, DecodeError, DecodeLimits }
import silk.effect { Effect }
import silk.result { Result }
import silk.trust_anchor { TrustAnchor }

effect fn allocated() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let decoded = run Certificate.decodeDer(
    ${literal('MFMwRqADAgEBAgEBMAMGASowADAeFw01MDAxMDEwMDAwMDBaFw00OTEyMzEyMzU5NTlaMAAwCzAFBgEqBQADAgOggQIHgIICAf4wBQYBKwUAAwIC/A==')},
    DecodeLimits.defaults(),
  ) |> Effect.provideMut<Allocator>(&mut allocator)
  return match move decoded {
    Result<Certificate, DecodeError>.Failure {error} => 1
    Result<Certificate, DecodeError>.Success {value} => {
      let anchor = TrustAnchor.fromCertificate(move value)
      if TrustAnchor.encodedBytes(&anchor) == 85 { return 42 }
      return 2
    }
  }
}

effect fn allocationFailure(error: OutOfMemoryError) -> i32 { return 3 }
pub fn main() -> i32 { return run Effect.catchAll(allocated(), allocationFailure) }
`
