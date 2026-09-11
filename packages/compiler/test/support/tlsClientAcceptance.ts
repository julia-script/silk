import { readFileSync } from 'node:fs'
import { createCipheriv, createDecipheriv, createHmac, X509Certificate } from 'node:crypto'

const fixture = (name: string): Uint8Array =>
  readFileSync(new URL(`../fixtures/tls-client/${name}`, import.meta.url))
// One byte-string token keeps fixture-heavy acceptance sources economical for the analysis graph;
// spelling every byte as an array element multiplies syntax nodes without adding coverage.
const silkBytes = (bytes: Uint8Array): string =>
  `b"${[...bytes].map((byte) => `\\x${byte.toString(16).padStart(2, '0')}`).join('')}"`

const rootPem = fixture('keys/root-cert.pem')
const clientHello = fixture('captures/rsa-x25519-client-hello.bin')
const serverFlight = fixture('captures/rsa-x25519-server-flight.bin')
const wrongNameClientHello = fixture('captures/rsa-x25519-wrong-name-client-hello.bin')
const wrongNameServerFlight = fixture('captures/rsa-x25519-wrong-name-server-flight.bin')
const ipClientHello = fixture('captures/rsa-x25519-ip-client-hello.bin')
const ipServerFlight = fixture('captures/rsa-x25519-ip-server-flight.bin')
const aes128ClientHello = fixture('captures/ecdsa-x25519-aes128-client-hello.bin')
const aes128ServerFlight = fixture('captures/ecdsa-x25519-aes128-server-flight.bin')
const aes128NoAlpnServerFlight = fixture('captures/ecdsa-x25519-aes128-no-alpn-server-flight.bin')
const aes256ClientHello = fixture('captures/ecdsa-p256-aes256-client-hello.bin')
const aes256ClientRetry = fixture('captures/ecdsa-p256-aes256-client-retry.bin')
const aes256ServerRetry = fixture('captures/ecdsa-p256-aes256-server-retry.bin')
const aes256ServerFlight = fixture('captures/ecdsa-p256-aes256-server-flight.bin')
const alpnBoundaryProtocols = [1, 2, 3, 4, 5].map((value) =>
  Uint8Array.from({ length: 255 }, () => value),
)
const alpnBoundaryBytes = Buffer.concat(alpnBoundaryProtocols)

type Suite = 'chacha' | 'aes128' | 'aes256'

const keyLogSecret = (id: string, label: string): Buffer => {
  const line = readFileSync(
    new URL(`../fixtures/tls-client/captures/${id}-keylog.TEST-ONLY.txt`, import.meta.url),
    'utf8',
  )
    .split('\n')
    .find((candidate) => candidate.startsWith(`${label} `))
  if (line === undefined) throw new Error(`missing ${label} in ${id}`)
  const encoded = line.split(' ').at(2)
  if (encoded === undefined) throw new Error(`malformed ${label} in ${id}`)
  return Buffer.from(encoded, 'hex')
}

const suiteParameters = (suite: Suite) => ({
  hash: suite === 'aes256' ? ('sha384' as const) : ('sha256' as const),
  keyLength: suite === 'aes128' ? 16 : 32,
})

const expandLabel = (secret: Uint8Array, label: string, length: number, suite: Suite): Buffer => {
  const info = Buffer.concat([
    Buffer.from([length >> 8, length & 0xff, 6 + label.length]),
    Buffer.from(`tls13 ${label}`),
    Buffer.from([0, 1]),
  ])
  return createHmac(suiteParameters(suite).hash, secret).update(info).digest().subarray(0, length)
}

const nonceFor = (iv: Uint8Array, sequence: number): Buffer => {
  const nonce = Buffer.from(iv)
  const encoded = Buffer.alloc(8)
  encoded.writeBigUInt64BE(BigInt(sequence))
  for (let index = 0; index < 8; index += 1) {
    nonce[index + 4] = (nonce[index + 4] ?? 0) ^ (encoded[index] ?? 0)
  }
  return nonce
}

const protectRecord = (
  plaintext: Uint8Array,
  contentType: number,
  secret: Uint8Array,
  suite: Suite,
  sequence: number,
): Buffer => {
  const parameters = suiteParameters(suite)
  const key = expandLabel(secret, 'key', parameters.keyLength, suite)
  const iv = expandLabel(secret, 'iv', 12, suite)
  const payloadLength = plaintext.length + 1 + 16
  const header = Buffer.from([23, 3, 3, payloadLength >> 8, payloadLength & 0xff])
  const nonce = nonceFor(iv, sequence)
  const cipher = (() => {
    if (suite === 'chacha')
      return createCipheriv('chacha20-poly1305', key, nonce, { authTagLength: 16 })
    if (suite === 'aes128') return createCipheriv('aes-128-gcm', key, nonce)
    return createCipheriv('aes-256-gcm', key, nonce)
  })()
  cipher.setAAD(header, { plaintextLength: plaintext.length + 1 })
  const encrypted = Buffer.concat([
    cipher.update(plaintext),
    cipher.update(Buffer.from([contentType])),
    cipher.final(),
  ])
  return Buffer.concat([header, encrypted, cipher.getAuthTag()])
}

const unprotectRecord = (
  record: Uint8Array,
  secret: Uint8Array,
  suite: Suite,
  sequence: number,
): Buffer => {
  const parameters = suiteParameters(suite)
  const header = Buffer.from(record.subarray(0, 5))
  const body = Buffer.from(record.subarray(5))
  const ciphertext = body.subarray(0, -16)
  const key = expandLabel(secret, 'key', parameters.keyLength, suite)
  const nonce = nonceFor(expandLabel(secret, 'iv', 12, suite), sequence)
  const decipher = (() => {
    if (suite === 'chacha')
      return createDecipheriv('chacha20-poly1305', key, nonce, { authTagLength: 16 })
    if (suite === 'aes128') return createDecipheriv('aes-128-gcm', key, nonce)
    return createDecipheriv('aes-256-gcm', key, nonce)
  })()
  decipher.setAAD(header, { plaintextLength: ciphertext.length })
  decipher.setAuthTag(body.subarray(-16))
  return Buffer.concat([decipher.update(ciphertext), decipher.final()])
}

interface RecordRange {
  readonly start: number
  readonly end: number
  readonly contentType: number
}

const recordRanges = (flight: Uint8Array): ReadonlyArray<RecordRange> => {
  const ranges: Array<RecordRange> = []
  let cursor = 0
  while (cursor < flight.length) {
    const length = (flight[cursor + 3] ?? 0) * 256 + (flight[cursor + 4] ?? 0)
    ranges.push({ start: cursor, end: cursor + 5 + length, contentType: flight[cursor] ?? 0 })
    cursor += 5 + length
  }
  if (cursor !== flight.length) throw new Error('truncated TLS fixture record')
  return ranges
}

interface HandshakeRange {
  readonly start: number
  readonly end: number
  readonly kind: number
}

const handshakeRanges = (messages: Uint8Array): ReadonlyArray<HandshakeRange> => {
  const ranges: Array<HandshakeRange> = []
  let cursor = 0
  while (cursor < messages.length) {
    const length =
      (messages[cursor + 1] ?? 0) * 65_536 +
      (messages[cursor + 2] ?? 0) * 256 +
      (messages[cursor + 3] ?? 0)
    ranges.push({ start: cursor, end: cursor + 4 + length, kind: messages[cursor] ?? 0 })
    cursor += 4 + length
  }
  if (cursor !== messages.length) throw new Error('truncated TLS fixture handshake')
  return ranges
}

const handshakeMessage = (kind: number, body: Uint8Array): Buffer =>
  Buffer.concat([
    Buffer.from([kind, body.length >> 16, (body.length >> 8) & 0xff, body.length & 0xff]),
    body,
  ])

const mutateHandshakeFlight = (
  id: string,
  suite: Suite,
  flight: Uint8Array,
  mutate: (messages: Buffer) => Buffer,
): Buffer => {
  const protectedRange = recordRanges(flight).find((range) => range.contentType === 23)
  if (protectedRange === undefined) throw new Error(`missing protected handshake in ${id}`)
  const secret = keyLogSecret(id, 'SERVER_HANDSHAKE_TRAFFIC_SECRET')
  const plaintext = unprotectRecord(
    flight.subarray(protectedRange.start, protectedRange.end),
    secret,
    suite,
    0,
  )
  if (plaintext.at(-1) !== 22) throw new Error(`wrong protected handshake type in ${id}`)
  const changed = mutate(Buffer.from(plaintext.subarray(0, -1)))
  const protectedRecord = protectRecord(changed, 22, secret, suite, 0)
  return Buffer.concat([
    flight.subarray(0, protectedRange.start),
    protectedRecord,
    flight.subarray(protectedRange.end),
  ])
}

const certificateChainDer = (
  id: string,
  suite: Suite,
  flight: Uint8Array,
): ReadonlyArray<Buffer> => {
  const protectedRange = recordRanges(flight).find((range) => range.contentType === 23)
  if (protectedRange === undefined) throw new Error(`missing protected handshake in ${id}`)
  const secret = keyLogSecret(id, 'SERVER_HANDSHAKE_TRAFFIC_SECRET')
  const plaintext = unprotectRecord(
    flight.subarray(protectedRange.start, protectedRange.end),
    secret,
    suite,
    0,
  )
  if (plaintext.at(-1) !== 22) throw new Error(`wrong protected handshake type in ${id}`)
  const messages = plaintext.subarray(0, -1)
  const certificate = handshakeRanges(messages).find((range) => range.kind === 11)
  if (certificate === undefined) throw new Error(`missing Certificate in ${id}`)
  const message = messages.subarray(certificate.start, certificate.end)
  const contextLength = message[4] ?? 0
  const listStart = 5 + contextLength
  const listLength =
    (message[listStart] ?? 0) * 65_536 +
    (message[listStart + 1] ?? 0) * 256 +
    (message[listStart + 2] ?? 0)
  let cursor = listStart + 3
  const listEnd = cursor + listLength
  if (listEnd !== message.length) throw new Error(`malformed Certificate list in ${id}`)
  const chain: Array<Buffer> = []
  while (cursor < listEnd) {
    const derLength =
      (message[cursor] ?? 0) * 65_536 +
      (message[cursor + 1] ?? 0) * 256 +
      (message[cursor + 2] ?? 0)
    cursor += 3
    const derEnd = cursor + derLength
    if (derEnd + 2 > listEnd) throw new Error(`truncated Certificate entry in ${id}`)
    chain.push(Buffer.from(message.subarray(cursor, derEnd)))
    cursor = derEnd
    const extensionLength = (message[cursor] ?? 0) * 256 + (message[cursor + 1] ?? 0)
    cursor += 2 + extensionLength
    if (cursor > listEnd) throw new Error(`truncated Certificate extensions in ${id}`)
  }
  return chain
}

const expectedEcdsaLeafDer = new X509Certificate(fixture('keys/ecdsa-leaf-cert.pem')).raw
const expectedRsaLeafDer = new X509Certificate(fixture('keys/rsa-leaf-cert.pem')).raw
const expectedRootDer = new X509Certificate(fixture('keys/root-cert.pem')).raw
const capturedEcdsaChain = certificateChainDer('ecdsa-x25519-aes128', 'aes128', aes128ServerFlight)
if (
  capturedEcdsaChain.length !== 2 ||
  !capturedEcdsaChain[0]?.equals(expectedEcdsaLeafDer) ||
  !capturedEcdsaChain[1]?.equals(expectedRootDer)
) {
  throw new Error('ECDSA rustls capture does not contain the pinned leaf and root chain')
}

for (const [id, flight] of [
  ['rsa-x25519-wrong-name', wrongNameServerFlight],
  ['rsa-x25519-ip', ipServerFlight],
] as const) {
  const chain = certificateChainDer(id, 'chacha', flight)
  if (
    chain.length !== 2 ||
    !chain[0]?.equals(expectedRsaLeafDer) ||
    !chain[1]?.equals(expectedRootDer)
  ) {
    throw new Error(`${id} rustls capture does not contain the pinned leaf and root chain`)
  }
}

const clientHelloSni = (record: Uint8Array): Buffer | undefined => {
  if (record[0] !== 22 || record[5] !== 1) throw new Error('expected a ClientHello record')
  let cursor = 5 + 4 + 2 + 32
  const sessionLength = record[cursor] ?? 0
  cursor += 1 + sessionLength
  const suitesLength = (record[cursor] ?? 0) * 256 + (record[cursor + 1] ?? 0)
  cursor += 2 + suitesLength
  const compressionLength = record[cursor] ?? 0
  cursor += 1 + compressionLength
  const extensionsLength = (record[cursor] ?? 0) * 256 + (record[cursor + 1] ?? 0)
  cursor += 2
  const extensionsEnd = cursor + extensionsLength
  if (extensionsEnd !== record.length) throw new Error('malformed ClientHello extensions')
  while (cursor < extensionsEnd) {
    const kind = (record[cursor] ?? 0) * 256 + (record[cursor + 1] ?? 0)
    const length = (record[cursor + 2] ?? 0) * 256 + (record[cursor + 3] ?? 0)
    cursor += 4
    const end = cursor + length
    if (end > extensionsEnd) throw new Error('truncated ClientHello extension')
    if (kind === 0) {
      if (length < 5 || record[cursor + 2] !== 0) throw new Error('malformed SNI extension')
      const nameLength = (record[cursor + 3] ?? 0) * 256 + (record[cursor + 4] ?? 0)
      if (nameLength + 5 !== length) throw new Error('malformed SNI name')
      return Buffer.from(record.subarray(cursor + 5, end))
    }
    cursor = end
  }
  return undefined
}

if (clientHelloSni(wrongNameClientHello)?.toString('ascii') !== 'wrong.example') {
  throw new Error('wrong-name ClientHello does not contain exact wrong.example SNI')
}
if (clientHelloSni(ipClientHello) !== undefined) {
  throw new Error('IP ClientHello unexpectedly contains an SNI extension')
}

const replaceHandshake = (
  messages: Buffer,
  kind: number,
  replacement: (message: Buffer) => Buffer,
): Buffer => {
  const range = handshakeRanges(messages).find((candidate) => candidate.kind === kind)
  if (range === undefined) throw new Error(`missing handshake ${kind}`)
  return Buffer.concat([
    messages.subarray(0, range.start),
    replacement(messages.subarray(range.start, range.end)),
    messages.subarray(range.end),
  ])
}

const flipHandshakeByte = (id: string, suite: Suite, flight: Uint8Array, kind: number): Buffer =>
  mutateHandshakeFlight(id, suite, flight, (messages) =>
    replaceHandshake(messages, kind, (message) => {
      const changed = Buffer.from(message)
      changed[changed.length - 1] = (changed.at(-1) ?? 0) ^ 1
      return changed
    }),
  )

const badCertificateDerFlight = mutateHandshakeFlight(
  'rsa-x25519',
  'chacha',
  serverFlight,
  (messages) =>
    replaceHandshake(messages, 11, (message) => {
      const changed = Buffer.from(message)
      const contextLength = changed[4] ?? 0
      const certificateDer = 4 + 1 + contextLength + 3 + 3
      changed[certificateDer] = 0
      return changed
    }),
)
const badCertificateVerifyFlight = flipHandshakeByte('rsa-x25519', 'chacha', serverFlight, 15)
const badFinishedFlight = flipHandshakeByte('rsa-x25519', 'chacha', serverFlight, 20)
const unsolicitedExtensionFlight = mutateHandshakeFlight(
  'rsa-x25519',
  'chacha',
  serverFlight,
  (messages) =>
    replaceHandshake(messages, 8, (message) => {
      const changed = Buffer.from(message)
      changed[changed.length - 1] = 1
      return changed
    }),
)

const mutateCertificateRequest = (mutate: (message: Buffer) => Buffer): Buffer =>
  mutateHandshakeFlight('ecdsa-x25519-aes128', 'aes128', aes128ServerFlight, (messages) =>
    replaceHandshake(messages, 13, mutate),
  )
// Keep every malformed CertificateRequest the same size as the valid rustls message. Besides
// producing sharper parser regressions, this lets the native corpus encode each authenticated
// mutation as a sparse ciphertext/tag patch instead of embedding another full server flight.
const missingSignatureAlgorithmsFlight = mutateCertificateRequest((message) => {
  const changed = Buffer.from(message)
  changed[7] = 0
  changed[8] = 50
  return changed
})
const oddSignatureAlgorithmsFlight = mutateCertificateRequest((message) => {
  const changed = Buffer.from(message)
  changed[11] = 0
  changed[12] = 17
  return changed
})
const malformedAuthoritiesFlight = mutateCertificateRequest((message) => {
  const changed = Buffer.from(message)
  changed[37] = 0
  changed[38] = 51
  return changed
})
const duplicateRequestExtensionFlight = mutateCertificateRequest((message) => {
  const changed = Buffer.from(message)
  changed[31] = 0
  changed[32] = 13
  return changed
})
const unsolicitedRequestExtensionFlight = mutateCertificateRequest((message) => {
  const changed = Buffer.from(message)
  changed[31] = 0
  changed[32] = 51
  return changed
})

const emptyCookieRetry = (() => {
  const changed = Buffer.from(aes256ServerRetry)
  const handshakeStart = 5
  let cursor = handshakeStart + 44
  const end =
    handshakeStart +
    4 +
    ((changed[handshakeStart + 1] ?? 0) * 65_536 +
      (changed[handshakeStart + 2] ?? 0) * 256 +
      (changed[handshakeStart + 3] ?? 0))
  while (cursor < end) {
    const kind = (changed[cursor] ?? 0) * 256 + (changed[cursor + 1] ?? 0)
    const length = (changed[cursor + 2] ?? 0) * 256 + (changed[cursor + 3] ?? 0)
    if (kind === 51 && length === 2) {
      changed[cursor] = 0
      changed[cursor + 1] = 44
      changed[cursor + 4] = 0
      changed[cursor + 5] = 0
      return changed
    }
    cursor += 4 + length
  }
  throw new Error('missing HRR key_share')
})()

const invalidCcs = Buffer.from([20, 3, 3, 0, 1, 2])
const validCcs = Buffer.from([20, 3, 3, 0, 1, 1])
const applicationSecret = (id: string): Buffer => keyLogSecret(id, 'SERVER_TRAFFIC_SECRET_0')
const postHandshakeRecord = (id: string, suite: Suite, body: Uint8Array): Buffer =>
  protectRecord(body, 22, applicationSecret(id), suite, 1)
const malformedTicketRecord = postHandshakeRecord(
  'rsa-x25519',
  'chacha',
  handshakeMessage(
    4,
    Buffer.from([0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 1, 1, 0, 1, 1, 0, 4, 0, 42, 0, 3]),
  ),
)
const duplicateTicketExtensionRecord = postHandshakeRecord(
  'rsa-x25519',
  'chacha',
  handshakeMessage(
    4,
    Buffer.from([
      0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 1, 1, 0, 1, 1, 0, 16, 0, 42, 0, 4, 0, 0, 0, 0, 0, 42, 0, 4, 0,
      0, 0, 0,
    ]),
  ),
)
const peerKeyUpdateRecord = postHandshakeRecord(
  'rsa-x25519',
  'chacha',
  handshakeMessage(24, Buffer.from([1])),
)
const fatalAlertRecord = protectRecord(
  Buffer.from([2, 40]),
  21,
  applicationSecret('rsa-x25519'),
  'chacha',
  1,
)
const badTagFlight = Buffer.from(serverFlight)
badTagFlight[badTagFlight.length - 1] = (badTagFlight.at(-1) ?? 0) ^ 1

const silkFixtureFailure = (
  name: string,
  changed: Uint8Array,
  expected: number,
  h2 = false,
): string => {
  const helper = h2 ? 'expectedH2FlightFailure' : 'expectedFlightFailure'
  const h2Argument = h2 ? 'h2Protocols, ' : ''
  return `// TLS_CLIENT_FIXTURE_${name}_BEGIN
  let ${name} = ${silkBytes(changed)}
  if !(run ${helper}(${h2Argument}${name}, ${expected})
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)) { return ${expected + 100} }
  // TLS_CLIENT_FIXTURE_${name}_END`
}

const nativeFixtureIds = {
  rootPem: 0,
  clientHello: 1,
  serverFlight: 2,
  aes128ClientHello: 3,
  aes128ServerFlight: 4,
  aes128NoAlpnServerFlight: 5,
  aes256ClientHello: 6,
  aes256ClientRetry: 7,
  aes256ServerRetry: 8,
  aes256ServerFlight: 9,
  badCertificateDerFlight: 10,
  badCertificateVerifyFlight: 11,
  badFinishedFlight: 12,
  unsolicitedExtensionFlight: 13,
  missingSignatureAlgorithmsFlight: 14,
  oddSignatureAlgorithmsFlight: 15,
  malformedAuthoritiesFlight: 16,
  duplicateRequestExtensionFlight: 17,
  unsolicitedRequestExtensionFlight: 18,
  emptyCookieRetry: 19,
  invalidCcs: 20,
  badTagFlight: 21,
  malformedTicketRecord: 22,
  duplicateTicketExtensionRecord: 23,
  peerKeyUpdateRecord: 24,
  fatalAlertRecord: 25,
  validCcs: 26,
  wrongNameClientHello: 27,
  wrongNameServerFlight: 28,
  ipClientHello: 29,
  ipServerFlight: 30,
} as const

const nativeFixtures: ReadonlyArray<readonly [number, Uint8Array]> = [
  [nativeFixtureIds.rootPem, rootPem],
  [nativeFixtureIds.clientHello, clientHello],
  [nativeFixtureIds.serverFlight, serverFlight],
  [nativeFixtureIds.aes128ClientHello, aes128ClientHello],
  [nativeFixtureIds.aes128ServerFlight, aes128ServerFlight],
  [nativeFixtureIds.aes128NoAlpnServerFlight, aes128NoAlpnServerFlight],
  [nativeFixtureIds.aes256ClientHello, aes256ClientHello],
  [nativeFixtureIds.aes256ClientRetry, aes256ClientRetry],
  [nativeFixtureIds.aes256ServerRetry, aes256ServerRetry],
  [nativeFixtureIds.aes256ServerFlight, aes256ServerFlight],
  [nativeFixtureIds.badCertificateDerFlight, badCertificateDerFlight],
  [nativeFixtureIds.badCertificateVerifyFlight, badCertificateVerifyFlight],
  [nativeFixtureIds.badFinishedFlight, badFinishedFlight],
  [nativeFixtureIds.unsolicitedExtensionFlight, unsolicitedExtensionFlight],
  [nativeFixtureIds.missingSignatureAlgorithmsFlight, missingSignatureAlgorithmsFlight],
  [nativeFixtureIds.oddSignatureAlgorithmsFlight, oddSignatureAlgorithmsFlight],
  [nativeFixtureIds.malformedAuthoritiesFlight, malformedAuthoritiesFlight],
  [nativeFixtureIds.duplicateRequestExtensionFlight, duplicateRequestExtensionFlight],
  [nativeFixtureIds.unsolicitedRequestExtensionFlight, unsolicitedRequestExtensionFlight],
  [nativeFixtureIds.emptyCookieRetry, emptyCookieRetry],
  [nativeFixtureIds.invalidCcs, invalidCcs],
  [nativeFixtureIds.badTagFlight, badTagFlight],
  [nativeFixtureIds.malformedTicketRecord, malformedTicketRecord],
  [nativeFixtureIds.duplicateTicketExtensionRecord, duplicateTicketExtensionRecord],
  [nativeFixtureIds.peerKeyUpdateRecord, peerKeyUpdateRecord],
  [nativeFixtureIds.fatalAlertRecord, fatalAlertRecord],
  [nativeFixtureIds.validCcs, validCcs],
  [nativeFixtureIds.wrongNameClientHello, wrongNameClientHello],
  [nativeFixtureIds.wrongNameServerFlight, wrongNameServerFlight],
  [nativeFixtureIds.ipClientHello, ipClientHello],
  [nativeFixtureIds.ipServerFlight, ipServerFlight],
]

const cBytes = (bytes: Uint8Array): string =>
  [...bytes].map((byte) => `0x${byte.toString(16).padStart(2, '0')}`).join(',')

/** Data-only native carrier. Silk remains the TLS oracle and verifies every copied length. */
export const tlsClientNativeFixtureSource = `#include <stddef.h>
#include <stdint.h>
#include <string.h>
${nativeFixtures
  .map(([id, bytes]) => `static const uint8_t fixture_${id}[] = {${cBytes(bytes)}};`)
  .join('\n')}

size_t silk_tls_fixture_size(int32_t id) {
  switch (id) {
${nativeFixtures.map(([id, bytes]) => `    case ${id}: return ${bytes.length};`).join('\n')}
    default: return 0;
  }
}

size_t silk_tls_fixture_copy(int32_t id, uint8_t *output, size_t capacity) {
  const uint8_t *source = NULL;
  size_t length = 0;
  switch (id) {
${nativeFixtures
  .map(([id, bytes]) => `    case ${id}: source = fixture_${id}; length = ${bytes.length}; break;`)
  .join('\n')}
    default: return 0;
  }
  if (output == NULL || capacity != length) return 0;
  memcpy(output, source, length);
  return length;
}
`

const tlsClientSourceTemplate = `import silk.allocator {Allocator, OutOfMemoryError}
import silk.effect {Effect}
import silk.https_identity {HttpsIdentity, IdentityError, OriginHost, ReferenceIdentity}
import silk.random {Random}
import silk.result {Result}
import silk.option {Option}
import silk.slice {Slice}
import silk.system_clock {SystemClock}
import silk.tls_client {
  AlpnConfig,
  AlpnProtocol,
  Authentication,
  Client,
  ClientConfig,
  ClientLimits,
  ClientOperation,
  Demand,
  NamedGroup,
  Progress,
  ProtocolReason,
  TlsError,
}
import silk.trust_anchor {TrustAnchor}
import silk.tls_record {CipherSuite}
import silk.trust_snapshot {SnapshotLimits, TrustLoadLimits, TrustSnapshot, TrustSourceError}
import silk.certificate_path {ValidationReason}
import silk.u8
import silk.usize
import silk.vector {Vector}

// Deterministic test double only. Production code must supply a CSPRNG implementation.
struct ScriptedRandom { filled: usize }

impl Random for ScriptedRandom {
  effect fn fillBytes(self: &mut Self, output: &mut [u8]) -> () {
    let mut index = usize.ZERO
    while index < output.length {
      output[index] = usize.toU8((self.filled + index + usize.ONE) % 251 + usize.ONE)
      index = index + usize.ONE
    }
    self.filled = self.filled + output.length
    return ()
  }
}

fn emptyTrust() -> TrustSnapshot {
  let made = TrustSnapshot.fromAnchors(
    Vector.make<TrustAnchor>(),
    SnapshotLimits {anchors: 0, encodedBytes: 0},
  )
  return match move made {
    Result<TrustSnapshot, TrustSourceError>.Success {value} => move value
    Result<TrustSnapshot, TrustSourceError>.Failure {error} => {
      let invalid = 1 / 0
      return emptyTrust()
    }
  }
}

// TLS_CLIENT_ROOT_TRUST_BEGIN
effect fn rootTrust() -> TrustSnapshot ! OutOfMemoryError ? &mut Allocator {
  let pem = ${silkBytes(rootPem)}
  let made = run TrustSnapshot.fromPem(pem, TrustLoadLimits.defaults())
  return match move made {
    Result<TrustSnapshot, TrustSourceError>.Success {value} => move value
    Result<TrustSnapshot, TrustSourceError>.Failure {error} => {
      let invalid = 1 / 0
      return run rootTrust()
    }
  }
}
// TLS_CLIENT_ROOT_TRUST_END

fn reference<'a>(bytes: &'a [u8]) -> ReferenceIdentity<'a> {
  let admitted = HttpsIdentity.reference(OriginHost<'a>.Dns {bytes: bytes})
  return match move admitted {
    Result<ReferenceIdentity<'a>, IdentityError>.Success {value} => value
    Result<ReferenceIdentity<'a>, IdentityError>.Failure {error} => {
      let invalid = 1 / 0
      return reference(bytes)
    }
  }
}

// TLS_CLIENT_NATIVE_HELPERS_BEGIN
fn hasBytes(haystack: &[u8], needle: &[u8]) -> bool {
  if needle.length > haystack.length { return false }
  let mut start = usize.ZERO
  while start <= haystack.length - needle.length {
    let mut equal = true
    let mut index = usize.ZERO
    while index < needle.length {
      if haystack[start + index] != needle[index] { equal = false }
      index = index + usize.ONE
    }
    if equal { return true }
    start = start + usize.ONE
  }
  return false
}

fn isNeedOutput(result: Result<Progress, TlsError>) -> bool {
  return match move result {
    Result<Progress, TlsError>.Success {value} => value.demand == Demand.NeedOutput
    Result<Progress, TlsError>.Failure {error} => false
  }
}

fn isNeedInput(result: Result<Progress, TlsError>) -> bool {
  return match move result {
    Result<Progress, TlsError>.Success {value} => value.demand == Demand.NeedInput
    Result<Progress, TlsError>.Failure {error} => false
  }
}

fn isPrematureWrite(result: Result<Progress, TlsError>) -> bool {
  return match move result {
    Result<Progress, TlsError>.Success {value} => false
    Result<Progress, TlsError>.Failure {error} => match move error {
      TlsError.InvalidState {operation} => operation == ClientOperation.WritePlaintext
      _ => false
    }
  }
}

fn isTruncated(result: Result<Progress, TlsError>) -> bool {
  return match move result {
    Result<Progress, TlsError>.Success {value} => false
    Result<Progress, TlsError>.Failure {error} => match move error {
      TlsError.Truncated => true
      _ => false
    }
  }
}

fn isFailureCode(result: Result<Progress, TlsError>, expected: i32) -> bool {
  return match move result {
    Result<Progress, TlsError>.Success {value} => false
    Result<Progress, TlsError>.Failure {error} => feedFailureCode(move error) == expected
  }
}

fn isInvalidTime(result: Result<Client, TlsError>) -> bool {
  return match move result {
    Result<Client, TlsError>.Success {value} => false
    Result<Client, TlsError>.Failure {error} => match move error {
      TlsError.CertificatePath {error: pathError} => pathError.reason == ValidationReason.InvalidTime
      _ => false
    }
  }
}

fn validAes128Authentication<'a>(value: &Authentication<'a>) -> bool {
  let selected = value.selectedAlpn()
  return match move selected {
    Option.None => false
    Option.Some {value: selectedBytes} => selectedBytes.length == 2
      && selectedBytes[0] == 104
      && selectedBytes[1] == 50
      && value.suite() == CipherSuite.Aes128GcmSha256
      && value.group() == NamedGroup.X25519
  }
}

fn invalidLimit(result: Result<Client, TlsError>, expected: usize) -> bool {
  return match move result {
    Result<Client, TlsError>.Success {value} => false
    Result<Client, TlsError>.Failure {error} => match move error {
      TlsError.LimitExceeded {kind, limit} => limit == expected
      _ => false
    }
  }
}
// TLS_CLIENT_NATIVE_HELPERS_END

fn protocolFailureCode(reason: ProtocolReason) -> i32 {
  return match reason {
    ProtocolReason.Record => 40
    ProtocolReason.UnexpectedMessage => 41
    ProtocolReason.InvalidLength => 42
    ProtocolReason.Extension => 43
    ProtocolReason.ServerHello => 44
    ProtocolReason.HelloRetryRequest => 45
    ProtocolReason.CompatibilityCcs => 46
    ProtocolReason.CertificateRequest => 47
    ProtocolReason.Certificate => 48
    ProtocolReason.PostHandshake => 49
    ProtocolReason.InternalState => 50
  }
}

fn feedFailureCode(error: TlsError) -> i32 {
  return match move error {
    TlsError.PeerAlert {code} => 30
    TlsError.BadRecordMac => 31
    TlsError.ProtocolViolation {reason} => protocolFailureCode(reason)
    TlsError.CertificateDecode {error: decodeError} => 51
    TlsError.CertificatePath {error: pathError} => 52
    TlsError.CertificateIdentity {error: identityError} => 53
    TlsError.CertificateVerify {error: verifyError} => 54
    TlsError.Finished => 55
    TlsError.UnsupportedProfile => 56
    TlsError.LimitExceeded {kind, limit} => 57
    TlsError.InvalidState {operation} => 58
    TlsError.EmptyBuffer => 59
    TlsError.HandshakeTruncated => 60
    TlsError.Truncated => 61
    TlsError.NoApplicationProtocol => 62
  }
}

fn alpnProtocol<'a>(bytes: &'a [u8]) -> AlpnProtocol<'a> {
  return AlpnProtocol<'a> {bytes: bytes}
}

// TLS_CLIENT_WASM_DEFAULTS_BEGIN
effect fn makeDefaultWithTrust(
  trust: TrustSnapshot,
  validationSeconds: i64,
) -> Result<Client, TlsError>
! OutOfMemoryError
? &mut Allocator | &mut Random {
  let host = b"ExAmPlE.com"
  let config = ClientConfig {
    reference: reference(&host),
    alpn: AlpnConfig.defaults(),
    limits: ClientLimits.defaults(),
  }
  return run Client.make(&config, move trust, SystemClock.make(validationSeconds, 123456789))
}

effect fn makeDefaultClient() -> Result<Client, TlsError>
! OutOfMemoryError
? &mut Allocator | &mut Random {
  let trust = run rootTrust()
  return run makeDefaultWithTrust(move trust, 1789156800)
}
// TLS_CLIENT_WASM_DEFAULTS_END

effect fn feedAll(
  client: &mut Client,
  input: &[u8],
) -> i32
! OutOfMemoryError
? &mut Allocator | &mut Random {
  let mut offset = usize.ZERO
  while offset < input.length {
    let remaining = Slice.view<u8>(input, offset, input.length - offset)
    let fed = run Client.feedInput(&mut client, remaining)
    let accepted = match move fed {
      Result<Progress, TlsError>.Success {value} => value.consumed
      Result<Progress, TlsError>.Failure {error} => { return feedFailureCode(move error) }
    }
    if accepted == usize.ZERO { return 21 }
    offset = offset + accepted
    let mut steps = usize.ZERO
    while steps < 8 {
      let advanced = run Client.progress(&mut client)
      if let Result<Progress, TlsError>.Failure {error} = move advanced {
        return feedFailureCode(move error)
      }
      steps = steps + usize.ONE
    }
  }
  return 0
}

effect fn authenticate(
  client: &mut Client,
  flight: &[u8],
) -> i32
! OutOfMemoryError
? &mut Allocator | &mut Random {
  let fed = run feedAll(&mut client.*, flight)
  if fed != 0 { return fed }
  let mut outputSteps = usize.ZERO
  while outputSteps < 3 {
    let mut steps = usize.ZERO
    while client.pendingOutput().length == 0 && steps < 8 {
      let advanced = run Client.progress(&mut client)
      if let Result<Progress, TlsError>.Failure {error} = move advanced {
        return feedFailureCode(move error)
      }
      steps = steps + usize.ONE
    }
    let outputLength = Client.pendingOutput(&client.*).length
    if outputLength == 0 { return 23 }
    let acknowledged = Client.ackWritten(&mut client, outputLength)
    match move acknowledged {
      Result<Progress, TlsError>.Success {value} => {
        if value.demand == Demand.Authenticated { return 0 }
      }
      Result<Progress, TlsError>.Failure {error} => { return feedFailureCode(move error) }
    }
    outputSteps = outputSteps + usize.ONE
  }
  return 24
}

// TLS_CLIENT_WASM_EXPECTATIONS_BEGIN
effect fn expectedFlightFailure(
  flight: &[u8],
  expected: i32,
) -> bool
! OutOfMemoryError
? &mut Allocator | &mut Random {
  let made = run makeDefaultClient()
  let mut client = match move made {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return false }
  }
  let initial = client.pendingOutput().length
  drop client.ackWritten(initial)
  let result = run feedAll(&mut client, flight)
  if result != expected { return false }
  let sticky = run client.progress()
  return isFailureCode(move sticky, expected)
}

effect fn expectedH2FlightFailure<'a>(
  protocols: &'a [AlpnProtocol<'a>],
  flight: &[u8],
  expected: i32,
) -> bool
! OutOfMemoryError
? &mut Allocator | &mut Random {
  let host = b"ExAmPlE.com"
  let config = ClientConfig {
    reference: reference(&host),
    alpn: AlpnConfig.Offered {protocols: protocols, required: true},
    limits: ClientLimits.defaults(),
  }
  let trust = run rootTrust()
  let made = run Client.make(&config, move trust, SystemClock.make(1789156800, 123456789))
  let mut client = match move made {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return false }
  }
  let initial = client.pendingOutput().length
  drop client.ackWritten(initial)
  let result = run feedAll(&mut client, flight)
  return result == expected
}

effect fn expectedPostHandshakeFailure(
  flight: &[u8],
  record: &[u8],
) -> bool
! OutOfMemoryError
? &mut Allocator | &mut Random {
  let made = run makeDefaultClient()
  let mut client = match move made {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return false }
  }
  let hello = client.pendingOutput().length
  drop client.ackWritten(hello)
  let authenticated = run authenticate(&mut client, flight)
  if authenticated != 0 { return false }
  let mut discard: [u8; 40] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  ]
  drop client.readPlaintext(&mut discard)
  let result = run feedAll(&mut client, record)
  return result == 49
}
// TLS_CLIENT_WASM_EXPECTATIONS_END

// TLS_CLIENT_CASES_BEGIN
effect fn cases<'a>(h2Protocols: &'a [AlpnProtocol<'a>]) -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut random = ScriptedRandom {filled: 0}
  let mixed = b"ExAmPlE.com"
  let config = ClientConfig {
    reference: reference(&mixed),
    alpn: AlpnConfig.defaults(),
    limits: ClientLimits.defaults(),
  }
  let trust = run rootTrust() |> Effect.provideMut<Allocator>(&mut allocator)
  let made = run Client.make(&config, move trust, SystemClock.make(1789156800, 123456789))
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut client = match move made {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return 1 }
  }
  if random.filled != 64 { return 2 }
  let pending = client.pendingOutput()
  if pending.length < 6 || pending[0] != 22 || !hasBytes(pending, b"example.com") { return 3 }
  if hasBytes(pending, &mixed) { return 4 }
  let expectedHello = ${silkBytes(clientHello)}
  if pending.length != ${clientHello.length} { return 10 }
  let mut helloIndex: usize = 5
  while helloIndex < pending.length {
    if pending[helloIndex] != expectedHello[helloIndex] { return 10 }
    helloIndex = helloIndex + usize.ONE
  }
  let second = pending[1]
  let total = pending.length
  drop pending
  if !isNeedOutput(client.ackWritten(usize.ONE)) { return 5 }
  let suffix = client.pendingOutput()
  if suffix.length + usize.ONE != total || suffix[0] != second { return 6 }
  drop suffix
  if !isNeedInput(client.ackWritten(total - usize.ONE)) { return 7 }
  if !isPrematureWrite(client.writePlaintext(b"blocked")) { return 8 }

  let flight = ${silkBytes(serverFlight)}
  let mut offset = usize.ZERO
  while offset < ${serverFlight.length} {
    let input = Slice.view<u8>(flight, offset, ${serverFlight.length} - offset)
    let fed = run client.feedInput(input)
      |> Effect.provideMut<Random>(&mut random)
      |> Effect.provideMut<Allocator>(&mut allocator)
    let accepted = match move fed {
      Result<Progress, TlsError>.Success {value} => value.consumed
      Result<Progress, TlsError>.Failure {error} => { return feedFailureCode(move error) }
    }
    if accepted == usize.ZERO { return 21 }
    offset = offset + accepted
    let mut bufferedSteps = usize.ZERO
    while bufferedSteps < 8 {
      let advanced = run client.progress()
        |> Effect.provideMut<Random>(&mut random)
        |> Effect.provideMut<Allocator>(&mut allocator)
      if let Result<Progress, TlsError>.Failure {error} = move advanced { return 22 }
      bufferedSteps = bufferedSteps + usize.ONE
    }
  }
  let mut steps = usize.ZERO
  while client.pendingOutput().length == 0 && steps < 8 {
    let advanced = run client.progress()
      |> Effect.provideMut<Random>(&mut random)
      |> Effect.provideMut<Allocator>(&mut allocator)
    if let Result<Progress, TlsError>.Failure {error} = move advanced { return 22 }
    steps = steps + usize.ONE
  }
  let finishedLength = client.pendingOutput().length
  if finishedLength == 0 { return 23 }
  let completed = client.ackWritten(finishedLength)
  let authenticated = match move completed {
    Result<Progress, TlsError>.Success {value} => value.demand == Demand.Authenticated
    Result<Progress, TlsError>.Failure {error} => false
  }
  if !authenticated { return 24 }
  let metadata = client.authentication()
  let validMetadata = match move metadata {
    Option.None => false
    Option.Some {value} => value.suite() == CipherSuite.ChaCha20Poly1305Sha256
      && value.group() == NamedGroup.X25519
      && value.anchorIndex() == 0
      && value.sanIndex() == 0
      && value.leafDer().length > 0
  }
  if !validMetadata { return 25 }

  let expectedPlaintext = b"coalesced authenticated plaintext"
  let mut plaintext: [u8; 40] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  ]
  let read = client.readPlaintext(&mut plaintext)
  let readLength = match move read {
    Result<Progress, TlsError>.Success {value} => value.written
    Result<Progress, TlsError>.Failure {error} => { return 26 }
  }
  if readLength != expectedPlaintext.length { return 27 }
  let mut plaintextIndex = usize.ZERO
  while plaintextIndex < readLength {
    if plaintext[plaintextIndex] != expectedPlaintext[plaintextIndex] { return 28 }
    plaintextIndex = plaintextIndex + usize.ONE
  }

  let madeDrain = run makeDefaultClient()
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut drainClient = match move madeDrain {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return 66 }
  }
  let drainHello = drainClient.pendingOutput().length
  drop drainClient.ackWritten(drainHello)
  let drainCode = run authenticate(&mut drainClient, flight)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if drainCode != 0 { return 67 }
  let drainEnded = drainClient.endInput()
  let drainReady = match move drainEnded {
    Result<Progress, TlsError>.Success {value} => value.demand == Demand.PlaintextReady
    Result<Progress, TlsError>.Failure {error} => false
  }
  if !drainReady { return 68 }
  let drained = drainClient.readPlaintext(&mut plaintext)
  let drainedLength = match move drained {
    Result<Progress, TlsError>.Success {value} => value.written
    Result<Progress, TlsError>.Failure {error} => { return 69 }
  }
  if drainedLength != expectedPlaintext.length { return 70 }
  if !isTruncated(drainClient.readPlaintext(&mut plaintext)) { return 71 }

  if !isNeedOutput(client.closeWrite()) { return 29 }
  let closeLength = client.pendingOutput().length
  if closeLength == 0 || !isNeedInput(client.ackWritten(closeLength)) { return 63 }
  if !isTruncated(client.endInput()) { return 64 }
  if !isTruncated(client.ackWritten(usize.ZERO)) { return 65 }

  let mut narrow = ClientLimits.defaults()
  narrow.handshakeBodyBytes = usize.ONE
  let invalidConfig = ClientConfig {
    reference: reference(&mixed),
    alpn: AlpnConfig.defaults(),
    limits: narrow,
  }
  let before = random.filled
  let rejected = run Client.make(
    &invalidConfig,
    emptyTrust(),
    SystemClock.make(1704067200, 999999999),
  )
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if !invalidLimit(move rejected, usize.ONE) || random.filled != before { return 9 }

  let aes128Hello = ${silkBytes(aes128ClientHello)}
  let aes128Flight = ${silkBytes(aes128ServerFlight)}
  let aes128Host = b"ExAmPlE.com"
  let aes128Config = ClientConfig {
    reference: reference(&aes128Host),
    alpn: AlpnConfig.Offered {protocols: h2Protocols, required: true},
    limits: ClientLimits.defaults(),
  }
  let aes128Trust = run rootTrust() |> Effect.provideMut<Allocator>(&mut allocator)
  let madeAes128 = run Client.make(
    &aes128Config,
    move aes128Trust,
    SystemClock.make(1789156800, 123456789),
  )
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut aes128Client = match move madeAes128 {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return 72 }
  }
  let actualAes128Hello = aes128Client.pendingOutput()
  if actualAes128Hello.length != aes128Hello.length { return 73 }
  let mut aes128HelloIndex: usize = 5
  while aes128HelloIndex < aes128Hello.length {
    if actualAes128Hello[aes128HelloIndex] != aes128Hello[aes128HelloIndex] { return 74 }
    aes128HelloIndex = aes128HelloIndex + usize.ONE
  }
  drop actualAes128Hello
  drop aes128Client.ackWritten(aes128Hello.length)
  let aes128Result = run authenticate(&mut aes128Client, aes128Flight)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if aes128Result != 0 { return 75 }
  let aes128Auth = aes128Client.authentication()
  let aes128Metadata = match move aes128Auth {
    Option.None => false
    Option.Some {value} => validAes128Authentication(&value)
  }
  if !aes128Metadata { return 76 }

  let retryFlight = ${silkBytes(aes256ServerRetry)}
  let retryHello = ${silkBytes(aes256ClientRetry)}
  let aes256Hello = ${silkBytes(aes256ClientHello)}
  let aes256Flight = ${silkBytes(aes256ServerFlight)}
  let madeAes256 = run makeDefaultClient()
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut aes256Client = match move madeAes256 {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return 77 }
  }
  let initialAes256 = aes256Client.pendingOutput()
  if initialAes256.length != aes256Hello.length { return 78 }
  drop initialAes256
  drop aes256Client.ackWritten(aes256Hello.length)
  let retryResult = run feedAll(&mut aes256Client, retryFlight)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if retryResult != 0 || random.filled < 96 { return 79 }
  let actualRetry = aes256Client.pendingOutput()
  if actualRetry.length != retryHello.length { return 80 }
  let mut retryIndex: usize = 5
  while retryIndex < retryHello.length {
    if actualRetry[retryIndex] != retryHello[retryIndex] { return 81 }
    retryIndex = retryIndex + usize.ONE
  }
  drop actualRetry
  drop aes256Client.ackWritten(retryHello.length)
  let aes256Result = run authenticate(&mut aes256Client, aes256Flight)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if aes256Result != 0 { return 82 }
  let aes256Auth = aes256Client.authentication()
  let aes256Metadata = match move aes256Auth {
    Option.None => false
    Option.Some {value} => value.suite() == CipherSuite.Aes256GcmSha384
      && value.group() == NamedGroup.P256
  }
  if !aes256Metadata { return 83 }

  ${silkFixtureFailure('badDer', badCertificateDerFlight, 51)}
  ${silkFixtureFailure('badVerify', badCertificateVerifyFlight, 54)}
  ${silkFixtureFailure('badFinished', badFinishedFlight, 55)}
  ${silkFixtureFailure('badExtension', unsolicitedExtensionFlight, 43)}

  ${silkFixtureFailure('missingSignatureAlgorithms', missingSignatureAlgorithmsFlight, 47, true)}
  ${silkFixtureFailure('oddSignatureAlgorithms', oddSignatureAlgorithmsFlight, 47, true)}
  ${silkFixtureFailure('malformedAuthorities', malformedAuthoritiesFlight, 47, true)}
  ${silkFixtureFailure('duplicateRequestExtension', duplicateRequestExtensionFlight, 47, true)}
  ${silkFixtureFailure('unsolicitedRequestExtension', unsolicitedRequestExtensionFlight, 47, true)}

  let emptyCookie = ${silkBytes(emptyCookieRetry)}
  let madeEmptyCookie = run makeDefaultClient()
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut emptyCookieClient = match move madeEmptyCookie {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return 93 }
  }
  let emptyCookieHello = emptyCookieClient.pendingOutput().length
  drop emptyCookieClient.ackWritten(emptyCookieHello)
  let emptyCookieCode = run feedAll(&mut emptyCookieClient, emptyCookie)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if emptyCookieCode != 45 { return 94 }

  let invalidCcsBytes = ${silkBytes(invalidCcs)}
  if !(run expectedFlightFailure(invalidCcsBytes, 46)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)) { return 95 }
  let absentAlpn = ${silkBytes(aes128NoAlpnServerFlight)}
  if !(run expectedH2FlightFailure(h2Protocols, absentAlpn, 62)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)) { return 96 }

  ${silkFixtureFailure('badTag', badTagFlight, 31)}

  let optionalHost = b"ExAmPlE.com"
  let optionalConfig = ClientConfig {
    reference: reference(&optionalHost),
    alpn: AlpnConfig.Offered {protocols: h2Protocols, required: false},
    limits: ClientLimits.defaults(),
  }
  let optionalTrust = run rootTrust() |> Effect.provideMut<Allocator>(&mut allocator)
  let madeOptionalAlpn = run Client.make(
    &optionalConfig,
    move optionalTrust,
    SystemClock.make(1789156800, 123456789),
  )
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut optionalAlpnClient = match move madeOptionalAlpn {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return 118 }
  }
  let optionalHello = optionalAlpnClient.pendingOutput().length
  drop optionalAlpnClient.ackWritten(optionalHello)
  let optionalResult = run authenticate(&mut optionalAlpnClient, absentAlpn)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if optionalResult != 0 { return 119 }
  let optionalAuthentication = optionalAlpnClient.authentication()
  let optionalAbsent = match move optionalAuthentication {
    Option.None => false
    Option.Some {value} => match move value.selectedAlpn() {
      Option.None => true
      Option.Some {value: selected} => false
    }
  }
  if !optionalAbsent { return 120 }

  let emptyPathMade = run makeDefaultWithTrust(emptyTrust(), 1789156800)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut emptyPathClient = match move emptyPathMade {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return 121 }
  }
  let emptyPathHello = emptyPathClient.pendingOutput().length
  drop emptyPathClient.ackWritten(emptyPathHello)
  let emptyPathCode = run feedAll(&mut emptyPathClient, flight)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if emptyPathCode != 52 { return 122 }

  let wrongDns = b"wrong.example"
  let wrongDnsConfig = ClientConfig {
    reference: reference(&wrongDns),
    alpn: AlpnConfig.defaults(),
    limits: ClientLimits.defaults(),
  }
  let wrongDnsTrust = run rootTrust() |> Effect.provideMut<Allocator>(&mut allocator)
  let wrongDnsMade = run Client.make(
    &wrongDnsConfig,
    move wrongDnsTrust,
    SystemClock.make(1789156800, 123456789),
  )
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut wrongDnsClient = match move wrongDnsMade {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return 123 }
  }
  let wrongDnsHello = wrongDnsClient.pendingOutput().length
  drop wrongDnsClient.ackWritten(wrongDnsHello)
  let wrongDnsCode = run feedAll(&mut wrongDnsClient, flight)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if wrongDnsCode != 53 { return 124 }

  let ipAdmission = HttpsIdentity.reference(
    OriginHost<'static>.Ipv4 {bytes: [127, 0, 0, 1]},
  )
  let ipReference = match move ipAdmission {
    Result<ReferenceIdentity<'static>, IdentityError>.Success {value} => value
    Result<ReferenceIdentity<'static>, IdentityError>.Failure {error} => { return 125 }
  }
  let ipConfig = ClientConfig {
    reference: ipReference,
    alpn: AlpnConfig.defaults(),
    limits: ClientLimits.defaults(),
  }
  let ipTrust = run rootTrust() |> Effect.provideMut<Allocator>(&mut allocator)
  let ipMade = run Client.make(
    &ipConfig,
    move ipTrust,
    SystemClock.make(1789156800, 123456789),
  )
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut ipClient = match move ipMade {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return 126 }
  }
  let ipHello = ipClient.pendingOutput()
  if hasBytes(ipHello, b"example.com") { return 127 }
  let ipHelloLength = ipHello.length
  drop ipHello
  drop ipClient.ackWritten(ipHelloLength)
  let ipCode = run feedAll(&mut ipClient, flight)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if ipCode != 53 { return 128 }

  let invalidTimeTrust = run rootTrust() |> Effect.provideMut<Allocator>(&mut allocator)
  let beforeInvalidTime = random.filled
  let invalidTime = run makeDefaultWithTrust(move invalidTimeTrust, 253402300800)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if !isInvalidTime(move invalidTime) || random.filled != beforeInvalidTime { return 97 }

  let malformedTicket = ${silkBytes(malformedTicketRecord)}
  let duplicateTicket = ${silkBytes(duplicateTicketExtensionRecord)}
  let ticketFailures = [malformedTicket, duplicateTicket]
  let mut ticketFailureIndex = usize.ZERO
  while ticketFailureIndex < 2 {
    let matched = run expectedPostHandshakeFailure(flight, ticketFailures[ticketFailureIndex])
      |> Effect.provideMut<Random>(&mut random)
      |> Effect.provideMut<Allocator>(&mut allocator)
    if !matched { return 99 }
    ticketFailureIndex = ticketFailureIndex + usize.ONE
  }

  let mut discard: [u8; 40] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  ]

  let peerUpdate = ${silkBytes(peerKeyUpdateRecord)}
  let madeControl = run makeDefaultClient()
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut controlClient = match move madeControl {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return 105 }
  }
  let controlHello = controlClient.pendingOutput().length
  drop controlClient.ackWritten(controlHello)
  let controlAuth = run authenticate(&mut controlClient, flight)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if controlAuth != 0 { return 106 }
  drop controlClient.readPlaintext(&mut discard)
  let acceptedWrite = controlClient.writePlaintext(b"queued-before-update")
  let accepted = match move acceptedWrite {
    Result<Progress, TlsError>.Success {value} => value.consumed
    Result<Progress, TlsError>.Failure {error} => usize.ZERO
  }
  if accepted != 20 || !isNeedOutput(controlClient.requestKeyUpdate())
    || !isNeedOutput(controlClient.requestKeyUpdate()) { return 107 }
  let applicationOutput = controlClient.pendingOutput().length
  let afterApplication = controlClient.ackWritten(applicationOutput)
  if !isNeedOutput(move afterApplication) || controlClient.pendingOutput().length == 0 { return 108 }
  if !isNeedOutput(controlClient.requestKeyUpdate()) { return 109 }
  let keyUpdateOutput = controlClient.pendingOutput().length
  if keyUpdateOutput == 0 { return 110 }
  let afterKeyUpdate = controlClient.ackWritten(keyUpdateOutput)
  if !isNeedInput(move afterKeyUpdate) || controlClient.pendingOutput().length != 0 { return 111 }
  let peerUpdateCode = run feedAll(&mut controlClient, peerUpdate)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if peerUpdateCode != 0 || controlClient.pendingOutput().length == 0 { return 112 }
  let responseLength = controlClient.pendingOutput().length
  if !isNeedInput(controlClient.ackWritten(responseLength)) { return 113 }

  let fatalAlert = ${silkBytes(fatalAlertRecord)}
  let madeAlert = run makeDefaultClient()
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut alertClient = match move madeAlert {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return 114 }
  }
  let alertHello = alertClient.pendingOutput().length
  drop alertClient.ackWritten(alertHello)
  let alertAuth = run authenticate(&mut alertClient, flight)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if alertAuth != 0 { return 115 }
  drop alertClient.readPlaintext(&mut discard)
  let alertCode = run feedAll(&mut alertClient, fatalAlert)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let stickyAlert = run alertClient.progress()
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if alertCode != 30 || !isFailureCode(move stickyAlert, 30) { return 116 }
  return 42
}
// TLS_CLIENT_CASES_END

effect fn recover(error: OutOfMemoryError) -> i32 { return 98 }

// TLS_CLIENT_MAIN_BEGIN
pub fn main() -> i32 {
  let h2: [u8; 2] = [104, 50]
  let protocols = [alpnProtocol(&h2)]
  return run Effect.catchAll(cases(&protocols), recover)
}
// TLS_CLIENT_MAIN_END
`

const tlsClientWasmCases = `effect fn cases<'a>(h2Protocols: &'a [AlpnProtocol<'a>]) -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut random = ScriptedRandom {filled: 0}
  let made = run makeDefaultClient()
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut client = match move made {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return 1 }
  }
  let pending = client.pendingOutput()
  if pending.length != ${clientHello.length} { return 2 }
  drop pending
  drop client.ackWritten(${clientHello.length})
  let flight = ${silkBytes(serverFlight)}
  let authenticated = run authenticate(&mut client, flight)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if authenticated != 0 { return 3 }
  let metadata = client.authentication()
  let valid = match move metadata {
    Option.None => false
    Option.Some {value} => value.suite() == CipherSuite.ChaCha20Poly1305Sha256
      && value.group() == NamedGroup.X25519
      && value.leafDer().length > 0
  }
  if !valid { return 4 }
  let mut plaintext: [u8; 40] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  ]
  let read = client.readPlaintext(&mut plaintext)
  return match move read {
    Result<Progress, TlsError>.Success {value} => {
      if value.written == 33 { return 42 }
      return 5
    }
    Result<Progress, TlsError>.Failure {error} => 6
  }
}

`

const tlsClientNativeCases = `effect fn scenarioTrust(id: i32) -> TrustSnapshot
! OutOfMemoryError
? &mut Allocator {
  if id == 18 || id == 22 || id == 27 || id == 28 { return emptyTrust() }
  return run rootTrust()
}

fn scenarioReference<'a>(id: i32, normal: &'a [u8], wrong: &'a [u8]) -> ReferenceIdentity<'a> {
  if id == 20 {
    let admitted = HttpsIdentity.reference(OriginHost<'a>.Ipv4 {bytes: [127, 0, 0, 1]})
    return match move admitted {
      Result<ReferenceIdentity<'a>, IdentityError>.Success {value} => value
      Result<ReferenceIdentity<'a>, IdentityError>.Failure {error} => reference(normal)
    }
  }
  if id == 19 { return reference(wrong) }
  return reference(normal)
}

fn scenarioAlpn<'a>(
  id: i32,
  protocols: &'a [AlpnProtocol<'a>],
  acceptedBoundary: &'a [AlpnProtocol<'a>],
  rejectedBoundary: &'a [AlpnProtocol<'a>],
) -> AlpnConfig<'a> {
  if id == 27 { return AlpnConfig.Offered {protocols: acceptedBoundary, required: false} }
  if id == 28 { return AlpnConfig.Offered {protocols: rejectedBoundary, required: false} }
  if id == 2 || id == 8 || id == 9 || id == 10 || id == 11 || id == 12 || id == 15 {
    return AlpnConfig.Offered {protocols: protocols, required: true}
  }
  if id == 17 { return AlpnConfig.Offered {protocols: protocols, required: false} }
  return AlpnConfig.defaults()
}

fn scenarioHelloFixture(id: i32) -> i32 {
  if id == 2 || id == 8 || id == 9 || id == 10 || id == 11 || id == 12 || id == 15 || id == 17 {
    return ${nativeFixtureIds.aes128ClientHello}
  }
  if id == 3 || id == 13 { return ${nativeFixtureIds.aes256ClientHello} }
  if id == 19 { return ${nativeFixtureIds.wrongNameClientHello} }
  if id == 20 { return ${nativeFixtureIds.ipClientHello} }
  return ${nativeFixtureIds.clientHello}
}

fn scenarioFlightFixture(id: i32) -> i32 {
  if id == 2 { return ${nativeFixtureIds.aes128ServerFlight} }
  if id == 3 { return ${nativeFixtureIds.aes256ServerFlight} }
  if id == 4 { return ${nativeFixtureIds.badCertificateDerFlight} }
  if id == 5 { return ${nativeFixtureIds.badCertificateVerifyFlight} }
  if id == 6 { return ${nativeFixtureIds.badFinishedFlight} }
  if id == 7 { return ${nativeFixtureIds.unsolicitedExtensionFlight} }
  if id == 8 { return ${nativeFixtureIds.missingSignatureAlgorithmsFlight} }
  if id == 9 { return ${nativeFixtureIds.oddSignatureAlgorithmsFlight} }
  if id == 10 { return ${nativeFixtureIds.malformedAuthoritiesFlight} }
  if id == 11 { return ${nativeFixtureIds.duplicateRequestExtensionFlight} }
  if id == 12 { return ${nativeFixtureIds.unsolicitedRequestExtensionFlight} }
  if id == 14 { return ${nativeFixtureIds.invalidCcs} }
  if id == 15 || id == 17 { return ${nativeFixtureIds.aes128NoAlpnServerFlight} }
  if id == 16 { return ${nativeFixtureIds.badTagFlight} }
  if id == 19 { return ${nativeFixtureIds.wrongNameServerFlight} }
  if id == 20 { return ${nativeFixtureIds.ipServerFlight} }
  return ${nativeFixtureIds.serverFlight}
}

fn scenarioFailure(id: i32) -> i32 {
  if id == 4 { return 51 }
  if id == 5 { return 54 }
  if id == 6 { return 55 }
  if id == 7 { return 43 }
  if id >= 8 && id <= 12 { return 47 }
  if id == 14 { return 40 }
  if id == 15 { return 62 }
  if id == 16 { return 31 }
  if id == 18 { return 52 }
  if id == 19 || id == 20 { return 53 }
  return 0
}

fn retryFixture(id: i32) -> i32 {
  if id == 13 { return ${nativeFixtureIds.emptyCookieRetry} }
  return ${nativeFixtureIds.aes256ServerRetry}
}

fn ticketFixture(id: i32) -> i32 {
  if id == 23 { return ${nativeFixtureIds.malformedTicketRecord} }
  return ${nativeFixtureIds.duplicateTicketExtensionRecord}
}

fn sameHello(left: &[u8], right: &[u8]) -> bool {
  if left.length != right.length { return false }
  if left.length < 5 { return false }
  let mut index: usize = 5
  while index < left.length {
    if left[index] != right[index] { return false }
    index = index + usize.ONE
  }
  return true
}

fn validPlaintext(bytes: &[u8], written: usize) -> bool {
  let expected = b"coalesced authenticated plaintext"
  if written != expected.length { return false }
  let mut index = usize.ZERO
  while index < written {
    if bytes[index] != expected[index] { return false }
    index = index + usize.ONE
  }
  return true
}

fn validScenarioMetadata<'a>(id: i32, value: &Authentication<'a>) -> bool {
  if id == 2 { return validAes128Authentication(value) }
  if id == 3 {
    return value.suite() == CipherSuite.Aes256GcmSha384
      && value.group() == NamedGroup.P256
  }
  if id == 17 {
    return match move value.selectedAlpn() {
      Option.None => true
      Option.Some {value: selected} => false
    }
  }
  return value.suite() == CipherSuite.ChaCha20Poly1305Sha256
    && value.group() == NamedGroup.X25519
    && value.anchorIndex() == 0
    && value.sanIndex() == 0
    && value.leafDer().length > 0
}

effect fn runScenario<'a>(
  id: i32,
  h2Protocols: &'a [AlpnProtocol<'a>],
  acceptedBoundary: &'a [AlpnProtocol<'a>],
  rejectedBoundary: &'a [AlpnProtocol<'a>],
) -> bool
! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut random = ScriptedRandom {filled: 0}
  let normal = b"ExAmPlE.com"
  let wrong = b"wrong.example"
  let selectedReference = scenarioReference(id, &normal, &wrong)
  let selectedAlpn = scenarioAlpn(id, h2Protocols, acceptedBoundary, rejectedBoundary)
  let mut limits = ClientLimits.defaults()
  if id == 22 { limits.handshakeBodyBytes = usize.ONE }
  let config = ClientConfig {
    reference: selectedReference,
    alpn: selectedAlpn,
    limits: limits,
  }
  let trust = run scenarioTrust(id) |> Effect.provideMut<Allocator>(&mut allocator)
  let before = random.filled
  let mut seconds: i64 = 1789156800
  if id == 21 { seconds = 253402300800 }
  let made = run Client.make(&config, move trust, SystemClock.make(seconds, 123456789))
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if id == 21 { return isInvalidTime(move made) && random.filled == before }
  if id == 22 { return invalidLimit(move made, usize.ONE) && random.filled == before }
  if id == 28 { return invalidLimit(move made, 1024) && random.filled == before }
  let mut client = match move made {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return false }
  }
  if id == 27 { return random.filled == 64 }

  let pending = client.pendingOutput()
  if id == 20 {
    if hasBytes(pending, b"example.com") { return false }
  } else if id == 19 {
    if !hasBytes(pending, &wrong) { return false }
  }
  let expectedOwner = run loadFixture(scenarioHelloFixture(id))
    |> Effect.provideMut<Allocator>(&mut allocator)
  let expected = Bytes.asSlice(&expectedOwner)
  if !sameHello(pending, expected) { return false }
  if id == 29 {
    drop pending
    let ccsOwner = run loadFixture(${nativeFixtureIds.validCcs})
      |> Effect.provideMut<Allocator>(&mut allocator)
    let code = run feedAll(&mut client, Bytes.asSlice(&ccsOwner))
      |> Effect.provideMut<Random>(&mut random)
      |> Effect.provideMut<Allocator>(&mut allocator)
    if code != 46 { return false }
    let sticky = run client.progress()
      |> Effect.provideMut<Random>(&mut random)
      |> Effect.provideMut<Allocator>(&mut allocator)
    return isFailureCode(move sticky, 46)
  }
  let pendingLength = pending.length
  if id == 0 {
    let second = pending[1]
    drop pending
    if !isNeedOutput(client.ackWritten(usize.ONE)) { return false }
    let suffix = client.pendingOutput()
    if suffix.length + usize.ONE != pendingLength || suffix[0] != second { return false }
    drop suffix
    if !isNeedInput(client.ackWritten(pendingLength - usize.ONE)) { return false }
    if !isPrematureWrite(client.writePlaintext(b"blocked")) { return false }
  } else {
    drop pending
    drop client.ackWritten(pendingLength)
  }
  if id == 3 || id == 13 {
    let retryId = retryFixture(id)
    let retryOwner = run loadFixture(retryId) |> Effect.provideMut<Allocator>(&mut allocator)
    let retryCode = run feedAll(&mut client, Bytes.asSlice(&retryOwner))
      |> Effect.provideMut<Random>(&mut random)
      |> Effect.provideMut<Allocator>(&mut allocator)
    if id == 13 { return retryCode == 45 }
    if retryCode != 0 || random.filled < 96 { return false }
    let retryHelloOwner = run loadFixture(${nativeFixtureIds.aes256ClientRetry})
      |> Effect.provideMut<Allocator>(&mut allocator)
    let actualRetry = client.pendingOutput()
    if !sameHello(actualRetry, Bytes.asSlice(&retryHelloOwner)) { return false }
    let retryLength = actualRetry.length
    drop actualRetry
    drop client.ackWritten(retryLength)
  }

  let flightOwner = run loadFixture(scenarioFlightFixture(id))
    |> Effect.provideMut<Allocator>(&mut allocator)
  let failure = scenarioFailure(id)
  if failure != 0 {
    let code = run feedAll(&mut client, Bytes.asSlice(&flightOwner))
      |> Effect.provideMut<Random>(&mut random)
      |> Effect.provideMut<Allocator>(&mut allocator)
    if code != failure { return false }
    let sticky = run client.progress()
      |> Effect.provideMut<Random>(&mut random)
      |> Effect.provideMut<Allocator>(&mut allocator)
    return isFailureCode(move sticky, failure)
  }

  let authenticated = run authenticate(&mut client, Bytes.asSlice(&flightOwner))
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if authenticated != 0 { return false }
  let metadata = client.authentication()
  let validMetadata = match move metadata {
    Option.None => false
    Option.Some {value} => validScenarioMetadata(id, &value)
  }
  if !validMetadata { return false }
  if id == 2 || id == 3 || id == 17 { return true }

  let mut plaintext: [u8; 40] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  ]
  if id == 1 {
    let ended = client.endInput()
    let ready = match move ended {
      Result<Progress, TlsError>.Success {value} => value.demand == Demand.PlaintextReady
      Result<Progress, TlsError>.Failure {error} => false
    }
    if !ready { return false }
    let drained = client.readPlaintext(&mut plaintext)
    let written = match move drained {
      Result<Progress, TlsError>.Success {value} => value.written
      Result<Progress, TlsError>.Failure {error} => usize.ZERO
    }
    return validPlaintext(&plaintext, written)
      && isTruncated(client.readPlaintext(&mut plaintext))
  }
  let read = client.readPlaintext(&mut plaintext)
  let written = match move read {
    Result<Progress, TlsError>.Success {value} => value.written
    Result<Progress, TlsError>.Failure {error} => usize.ZERO
  }
  if !validPlaintext(&plaintext, written) { return false }

  if id == 23 || id == 24 {
    let ticketId = ticketFixture(id)
    let ticketOwner = run loadFixture(ticketId) |> Effect.provideMut<Allocator>(&mut allocator)
    let ticketCode = run feedAll(&mut client, Bytes.asSlice(&ticketOwner))
      |> Effect.provideMut<Random>(&mut random)
      |> Effect.provideMut<Allocator>(&mut allocator)
    return ticketCode == 49
  }
  if id == 25 {
    let acceptedWrite = client.writePlaintext(b"queued-before-update")
    let accepted = match move acceptedWrite {
      Result<Progress, TlsError>.Success {value} => value.consumed
      Result<Progress, TlsError>.Failure {error} => usize.ZERO
    }
    if accepted != 20 || !isNeedOutput(client.requestKeyUpdate())
      || !isNeedOutput(client.requestKeyUpdate()) { return false }
    let applicationLength = client.pendingOutput().length
    if !isNeedOutput(client.ackWritten(applicationLength)) { return false }
    if !isNeedOutput(client.requestKeyUpdate()) { return false }
    let updateLength = client.pendingOutput().length
    if updateLength == 0 || !isNeedInput(client.ackWritten(updateLength)) { return false }
    let peerOwner = run loadFixture(${nativeFixtureIds.peerKeyUpdateRecord})
      |> Effect.provideMut<Allocator>(&mut allocator)
    let peerCode = run feedAll(&mut client, Bytes.asSlice(&peerOwner))
      |> Effect.provideMut<Random>(&mut random)
      |> Effect.provideMut<Allocator>(&mut allocator)
    if peerCode != 0 || client.pendingOutput().length == 0 { return false }
    let responseLength = client.pendingOutput().length
    return isNeedInput(client.ackWritten(responseLength))
  }
  if id == 26 {
    let alertOwner = run loadFixture(${nativeFixtureIds.fatalAlertRecord})
      |> Effect.provideMut<Allocator>(&mut allocator)
    let alertCode = run feedAll(&mut client, Bytes.asSlice(&alertOwner))
      |> Effect.provideMut<Random>(&mut random)
      |> Effect.provideMut<Allocator>(&mut allocator)
    let sticky = run client.progress()
      |> Effect.provideMut<Random>(&mut random)
      |> Effect.provideMut<Allocator>(&mut allocator)
    return alertCode == 30 && isFailureCode(move sticky, 30)
  }
  if id == 0 {
    if !isNeedOutput(client.closeWrite()) { return false }
    let closeLength = client.pendingOutput().length
    if closeLength == 0 || !isNeedInput(client.ackWritten(closeLength)) { return false }
    if !isTruncated(client.endInput()) { return false }
    return isTruncated(client.ackWritten(usize.ZERO))
  }
  return true
}

effect fn cases<'a>(
  h2Protocols: &'a [AlpnProtocol<'a>],
  acceptedBoundary: &'a [AlpnProtocol<'a>],
  rejectedBoundary: &'a [AlpnProtocol<'a>],
) -> i32 ! OutOfMemoryError {
  let mut id = 0
  while id <= 29 {
    if !(run runScenario(id, h2Protocols, acceptedBoundary, rejectedBoundary)) { return id + 1 }
    id = id + 1
  }
  return 42
}

`

const tlsClientNativeMain = `// TLS_CLIENT_MAIN_BEGIN
struct AlpnBoundaries<'a> {
  accepted: [AlpnProtocol<'a>; 4]
  rejected: [AlpnProtocol<'a>; 5]
}

fn alpnBoundaries<'a>(bytes: &'a [u8]) -> AlpnBoundaries<'a> {
  let one: &'a [u8] = Slice.view<u8>(bytes, 0, 255)
  let two: &'a [u8] = Slice.view<u8>(bytes, 255, 255)
  let three: &'a [u8] = Slice.view<u8>(bytes, 510, 255)
  let four: &'a [u8] = Slice.view<u8>(bytes, 765, 255)
  let five: &'a [u8] = Slice.view<u8>(bytes, 1020, 255)
  let rejectedFour: &'a [u8] = Slice.view<u8>(four, 0, 251)
  let rejectedFive: &'a [u8] = Slice.view<u8>(five, 0, 4)
  let accepted: [AlpnProtocol<'a>; 4] = [
    AlpnProtocol<'a> {bytes: one},
    AlpnProtocol<'a> {bytes: two},
    AlpnProtocol<'a> {bytes: three},
    AlpnProtocol<'a> {bytes: four},
  ]
  let rejected: [AlpnProtocol<'a>; 5] = [
    AlpnProtocol<'a> {bytes: one},
    AlpnProtocol<'a> {bytes: two},
    AlpnProtocol<'a> {bytes: three},
    AlpnProtocol<'a> {bytes: rejectedFour},
    AlpnProtocol<'a> {bytes: rejectedFive},
  ]
  return AlpnBoundaries<'a> {accepted: accepted, rejected: rejected}
}

pub fn main() -> i32 {
  let h2 = b"h2"
  let protocols = [alpnProtocol(&h2)]
  let boundaryBytes = ${silkBytes(alpnBoundaryBytes)}
  let boundaries = alpnBoundaries(boundaryBytes)
  return run Effect.catchAll(
    cases(&protocols, &boundaries.accepted, &boundaries.rejected),
    recover,
  )
}
// TLS_CLIENT_MAIN_END`

const nativeFixtureSupport = `import silk.bytes {Bytes}
import silk.pointer {Pointer}

unsafe extern "C" fn silk_tls_fixture_size(id: i32) -> usize
unsafe extern "C" fn silk_tls_fixture_copy(
  id: i32,
  output: ?[*]mut u8,
  capacity: usize,
) -> usize with Intrinsic.foreign(noCapture: ("output",))
effect fn loadFixture(id: i32) -> Bytes ! OutOfMemoryError ? &mut Allocator {
  let length = unsafe silk_tls_fixture_size(id)
  let mut bytes = run Bytes.zeroed(length)
  let copied = unsafe silk_tls_fixture_copy(id, Pointer.fromMutSlice(Bytes.asMutSlice(&mut bytes)), length)
  if copied != length { return Bytes.make() }
  return move bytes
}

`

const replaceExactlyOnce = (source: string, search: string, replacement: string): string => {
  const first = source.indexOf(search)
  if (first < 0 || source.indexOf(search, first + search.length) >= 0) {
    throw new Error(`expected one TLS source marker: ${search.slice(0, 48)}`)
  }
  return `${source.slice(0, first)}${replacement}${source.slice(first + search.length)}`
}

const replaceMarkerBlock = (source: string, marker: string, replacement: string): string => {
  const begin = `// ${marker}_BEGIN`
  const end = `// ${marker}_END`
  const start = source.indexOf(begin)
  const finish = source.indexOf(end, start + begin.length)
  if (
    start < 0 ||
    finish < 0 ||
    source.indexOf(begin, start + begin.length) >= 0 ||
    source.indexOf(end, finish + end.length) >= 0
  ) {
    throw new Error(`expected one TLS source marker block: ${marker}`)
  }
  return `${source.slice(0, start)}${replacement}${source.slice(finish + end.length)}`
}

let tlsClientWasm = replaceMarkerBlock(
  tlsClientSourceTemplate,
  'TLS_CLIENT_CASES',
  `// TLS_CLIENT_CASES_BEGIN\n${tlsClientWasmCases}// TLS_CLIENT_CASES_END`,
)
tlsClientWasm = replaceMarkerBlock(tlsClientWasm, 'TLS_CLIENT_NATIVE_HELPERS', '')
tlsClientWasm = replaceMarkerBlock(tlsClientWasm, 'TLS_CLIENT_WASM_EXPECTATIONS', '')
export const tlsClientWasmSource = tlsClientWasm

let tlsClientNative = replaceExactlyOnce(
  tlsClientSourceTemplate,
  'import silk.effect {Effect}\n',
  `import silk.effect {Effect}\n${nativeFixtureSupport}`,
)
tlsClientNative = replaceMarkerBlock(
  tlsClientNative,
  'TLS_CLIENT_CASES',
  `// TLS_CLIENT_CASES_BEGIN\n${tlsClientNativeCases}// TLS_CLIENT_CASES_END`,
)
tlsClientNative = replaceMarkerBlock(tlsClientNative, 'TLS_CLIENT_MAIN', tlsClientNativeMain)
tlsClientNative = replaceMarkerBlock(tlsClientNative, 'TLS_CLIENT_WASM_DEFAULTS', '')
tlsClientNative = replaceMarkerBlock(tlsClientNative, 'TLS_CLIENT_WASM_EXPECTATIONS', '')
tlsClientNative = replaceExactlyOnce(
  tlsClientNative,
  `let pem = ${silkBytes(rootPem)}`,
  `let pemOwner = run loadFixture(${nativeFixtureIds.rootPem})
  if Bytes.length(&pemOwner) != ${rootPem.length} {
    let invalid = 1 / 0
    return run rootTrust()
  }
  let pem = Bytes.asSlice(&pemOwner)`,
)

export const tlsClientNativeSource = tlsClientNative
export const tlsClientAcceptanceSource = tlsClientWasmSource
export const tlsClientFullMatrixSourceLength = tlsClientSourceTemplate.length
