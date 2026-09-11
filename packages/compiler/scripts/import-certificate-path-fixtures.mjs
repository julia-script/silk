#!/usr/bin/env node

import { createHash } from 'node:crypto'
import { readFileSync, writeFileSync } from 'node:fs'
import { resolve } from 'node:path'

const sourceSha256 = '563805f46937ad25ac9d4e41341c414070aced32a22294821b5c5fe526e2c52d'
const sourceUrl =
  'https://github.com/C2SP/x509-limbo/blob/3f8cba420e90322223486086054401189b7b320e/limbo.json'

const expected = new Map([
  ['pathlen::ee-with-intermediate-pathlen-0', ['Success']],
  ['pathlen::intermediate-violates-pathlen-0', ['Failure', 'PathLengthExceeded']],
  ['pathlen::self-issued-certs-pathlen', ['Success']],
  ['rfc5280::validity::notbefore-exact', ['Success']],
  ['rfc5280::validity::notbefore-fractional', ['Failure', 'InvalidTime']],
  ['rfc5280::validity::notafter-exact', ['Success']],
  [
    'rfc5280::validity::notafter-fractional',
    ['Failure', 'InvalidTime', 'Silk treats certificate endpoints as nanosecond zero.'],
  ],
  ['rfc5280::unknown-critical-extension-ee', ['Failure', 'UnsupportedExtension']],
  ['rfc5280::unknown-critical-extension-root', ['Failure', 'UnsupportedExtension']],
  ['rfc5280::unknown-critical-extension-unrelated-root', ['Success']],
  ['rfc5280::unknown-critical-extension-unrelated-intermediate', ['Success']],
  ['rfc5280::eku::ee-wrong-eku', ['Failure', 'InvalidUsage']],
  ['rfc5280::eku::ee-without-eku', ['Success']],
  ['rfc5280::nc::permitted-dns-match', ['Success']],
  ['rfc5280::nc::excluded-ipv4-match', ['Failure', 'NameConstraintViolation']],
  ['rfc5280::nc::permitted-ipv6-match', ['Success']],
  [
    'rfc5280::nc::permitted-dn-match',
    ['Failure', 'UnsupportedConstraint', 'Silk does not implement directoryName constraints.'],
  ],
  ['rfc5280::nc::permitted-self-issued', ['Success']],
  ['rfc5280::nc::excluded-self-issued-leaf', ['Failure', 'NameConstraintViolation']],
  [
    'rfc5280::nc::restrictive-permits-in-intermediates-widens',
    ['Failure', 'NameConstraintViolation'],
  ],
  ['rfc5280::nc::nc-forbids-alternate-chain-ica', ['Success']],
  ['rfc5280::nc::nc-forbids-dnsname-wildcard-san', ['Failure', 'NameConstraintViolation']],
])

const selectedPaths = new Map([
  ['pathlen::ee-with-intermediate-pathlen-0', [0]],
  ['pathlen::self-issued-certs-pathlen', [2, 1, 0]],
  ['rfc5280::validity::notbefore-exact', [0]],
  ['rfc5280::validity::notafter-exact', [0]],
  ['rfc5280::unknown-critical-extension-unrelated-root', []],
  ['rfc5280::unknown-critical-extension-unrelated-intermediate', [0]],
  ['rfc5280::eku::ee-without-eku', []],
  ['rfc5280::nc::permitted-dns-match', []],
  ['rfc5280::nc::permitted-ipv6-match', []],
  ['rfc5280::nc::permitted-self-issued', [0]],
  ['rfc5280::nc::nc-forbids-alternate-chain-ica', [2, 0]],
])

const input = process.argv[2]
if (input === undefined) {
  throw new Error('usage: import-certificate-path-fixtures.mjs /path/to/pinned/limbo.json')
}
const sourceBytes = readFileSync(resolve(input))
const digest = createHash('sha256').update(sourceBytes).digest('hex')
if (digest !== sourceSha256) throw new Error(`unexpected x509-limbo digest ${digest}`)
const source = JSON.parse(sourceBytes.toString('utf8'))

const der = (pem) => {
  const base64 = pem
    .replace('-----BEGIN CERTIFICATE-----', '')
    .replace('-----END CERTIFICATE-----', '')
    .replaceAll(/\s/g, '')
  const bytes = Buffer.from(base64, 'base64')
  return {
    der: bytes.toString('base64'),
    sha256: createHash('sha256').update(bytes).digest('hex'),
  }
}

const derLength = (length) => {
  if (length < 0x80) return [length]
  const octets = []
  let remaining = length
  while (remaining > 0) {
    octets.unshift(remaining & 0xff)
    remaining = Math.floor(remaining / 256)
  }
  return [0x80 | octets.length, ...octets]
}

const derNode = (bytes, start) => {
  const tag = bytes[start]
  const firstLength = bytes[start + 1]
  if (tag === undefined || firstLength === undefined) throw new Error('truncated DER node')
  const lengthOctets = (firstLength & 0x80) === 0 ? 0 : firstLength & 0x7f
  if (lengthOctets > 4) throw new Error('unsupported fixture DER length width')
  let length = (firstLength & 0x80) === 0 ? firstLength : 0
  for (let index = 0; index < lengthOctets; index += 1) {
    const octet = bytes[start + 2 + index]
    if (octet === undefined) throw new Error('truncated fixture DER length')
    length = length * 256 + octet
  }
  const content = start + 2 + lengthOctets
  const end = content + length
  if (end > bytes.length) throw new Error('fixture DER node escapes its input')
  return { start, content, end, tag }
}

const rebuildDerNode = (bytes, node, targetStart, targetEnd, replacement) => {
  if (node.start === targetStart && node.end === targetEnd) return replacement
  if (targetStart < node.content || targetEnd > node.end || (node.tag & 0x20) === 0) {
    throw new Error('replacement target is not a complete constructed DER child')
  }
  const children = []
  let childStart = node.content
  let replaced = false
  while (childStart < node.end) {
    const child = derNode(bytes, childStart)
    if (targetStart >= child.start && targetEnd <= child.end) {
      children.push(rebuildDerNode(bytes, child, targetStart, targetEnd, replacement))
      replaced = true
    } else {
      children.push(bytes.subarray(child.start, child.end))
    }
    childStart = child.end
  }
  if (childStart !== node.end || !replaced) {
    throw new Error('replacement target is not in the fixture DER tree')
  }
  const content = Buffer.concat(children)
  return Buffer.concat([Buffer.from([node.tag, ...derLength(content.length)]), content])
}

const rewriteDerNodes = (input, id, needle, replacement, count) => {
  const selected = Buffer.from(needle)
  let bytes = Buffer.from(input)
  for (let ordinal = 0; ordinal < count; ordinal += 1) {
    const offset = bytes.indexOf(selected)
    if (offset < 0) throw new Error(`missing DER replacement ${ordinal} in ${id}`)
    const target = derNode(bytes, offset)
    if (target.end !== offset + selected.length) {
      throw new Error(`replacement is not one DER node in ${id}`)
    }
    bytes = rebuildDerNode(bytes, derNode(bytes, 0), offset, target.end, Buffer.from(replacement))
  }
  if (bytes.indexOf(selected) >= 0) throw new Error(`unexpected extra DER replacement in ${id}`)
  return bytes
}

const rewriteDerNodeOccurrence = (input, id, needle, replacement, ordinal) => {
  const bytes = Buffer.from(input)
  const selected = Buffer.from(needle)
  let offset = -1
  for (let index = 0; index <= ordinal; index += 1) {
    offset = bytes.indexOf(selected, offset + 1)
    if (offset < 0) throw new Error(`missing DER replacement occurrence ${ordinal} in ${id}`)
  }
  const target = derNode(bytes, offset)
  if (target.end !== offset + selected.length) {
    throw new Error(`replacement occurrence is not one DER node in ${id}`)
  }
  return rebuildDerNode(bytes, derNode(bytes, 0), offset, target.end, Buffer.from(replacement))
}

const mutateUnique = (input, id, needle, mutate) => {
  const bytes = Buffer.from(input)
  const selected = Buffer.from(needle)
  const offset = bytes.indexOf(selected)
  if (offset < 0 || bytes.indexOf(selected, offset + 1) >= 0) {
    throw new Error(`expected one mutation target in ${id}`)
  }
  mutate(bytes, offset)
  return bytes
}

const testcase = (id) => {
  const selected = source.testcases.find((candidate) => candidate.id === id)
  if (selected === undefined) throw new Error(`missing x509-limbo testcase ${id}`)
  return selected
}

const derived = (id, sourceCase, role, original, mutation, bytes, result, reason) => ({
  id,
  sourceCase,
  sourceCertificateRole: role,
  sourceCertificateSha256: original.sha256,
  mutation,
  der: bytes.toString('base64'),
  sha256: createHash('sha256').update(bytes).digest('hex'),
  expectedSilk: { result, ...(reason === undefined ? {} : { reason }) },
})

const cases = []
for (const [id, selection] of expected) {
  const testcase = source.testcases.find((candidate) => candidate.id === id)
  if (testcase === undefined) throw new Error(`missing x509-limbo testcase ${id}`)
  if (
    id === 'rfc5280::nc::nc-forbids-alternate-chain-ica' &&
    (!testcase.description.includes("ICA_B' (SAN:Y) -> No root to chain to") ||
      !testcase.description.includes("ICA_B'' (no SAN) -> ICA_A (NC forbids SAN:Y) -> Root"))
  ) {
    throw new Error(`unexpected alternate-chain graph description for ${id}`)
  }
  const [result, reason, divergence] = selection
  const selectedPath = selectedPaths.get(id)
  if (result === 'Success' && selectedPath === undefined) {
    throw new Error(`missing selected-path evidence for ${id}`)
  }
  cases.push({
    id,
    expectedSilk: {
      result,
      ...(reason === undefined ? {} : { reason }),
      ...(divergence === undefined ? {} : { divergence }),
      ...(selectedPath === undefined ? {} : { anchorIndex: 0, intermediateIndices: selectedPath }),
    },
    upstreamResult: testcase.expected_result,
    validationTime: testcase.validation_time ?? '2024-03-15T00:00:00Z',
    peer: der(testcase.peer_certificate),
    peerKeyPemSha256: createHash('sha256').update(testcase.peer_certificate_key).digest('hex'),
    intermediates: testcase.untrusted_intermediates.map(der),
    anchors: testcase.trusted_certs.map(der),
    ordering: {
      anchors: testcase.trusted_certs.map((_, index) => index),
      intermediates: testcase.untrusted_intermediates.map((_, index) => index),
    },
  })
}

const pathLengthFixture = cases.find(
  (fixture) => fixture.id === 'pathlen::ee-with-intermediate-pathlen-0',
)
if (pathLengthFixture === undefined) throw new Error('missing path-length source fixture')
const wrongSignatureBytes = Buffer.from(pathLengthFixture.intermediates[0].der, 'base64')
const lastSignatureByte = wrongSignatureBytes.at(-1)
if (lastSignatureByte === undefined) throw new Error('empty intermediate fixture')
wrongSignatureBytes[wrongSignatureBytes.length - 1] = lastSignatureByte ^ 0x01

const noKeyUsageCase = testcase('rfc5280::no-keyusage')
const noKeyUsageLeaf = der(noKeyUsageCase.peer_certificate)
const noKeyUsageRoot = der(noKeyUsageCase.trusted_certs[0])
const noKeyUsageRootBytes = Buffer.from(noKeyUsageRoot.der, 'base64')
const ignoredAnchorMetadataBytes = mutateUnique(
  noKeyUsageRootBytes,
  'rfc5280::no-keyusage/trusted_certs[0] validity',
  Buffer.from('29690503000001Z', 'ascii'),
  (bytes, offset) => Buffer.from('20000101000001Z', 'ascii').copy(bytes, offset),
)
const ignoredSignatureOctet = ignoredAnchorMetadataBytes.at(-1)
if (ignoredSignatureOctet === undefined) throw new Error('missing anchor signature')
ignoredAnchorMetadataBytes[ignoredAnchorMetadataBytes.length - 1] = ignoredSignatureOctet ^ 0x01

const ecCurveOid = [0x06, 0x08, 0x2a, 0x86, 0x48, 0xce, 0x3d, 0x03, 0x01, 0x07]
const missingEcParametersBytes = rewriteDerNodes(
  noKeyUsageRootBytes,
  'rfc5280::no-keyusage/trusted_certs[0] parameters',
  ecCurveOid,
  [],
  1,
)

const pathIntermediate = pathLengthFixture.intermediates[0]
const pathIntermediateBytes = Buffer.from(pathIntermediate.der, 'base64')
const ecdsaSha256 = [0x30, 0x0a, 0x06, 0x08, 0x2a, 0x86, 0x48, 0xce, 0x3d, 0x04, 0x03, 0x02]
const ecdsaSha256Null = [
  0x30, 0x0c, 0x06, 0x08, 0x2a, 0x86, 0x48, 0xce, 0x3d, 0x04, 0x03, 0x02, 0x05, 0x00,
]
const unsupportedParametersBytes = rewriteDerNodes(
  pathIntermediateBytes,
  `${pathLengthFixture.id}/untrusted_intermediates[0] signature parameters`,
  ecdsaSha256,
  ecdsaSha256Null,
  2,
)
const unsupportedAlgorithmBytes = Buffer.from(pathIntermediateBytes)
let algorithmOffset = unsupportedAlgorithmBytes.indexOf(Buffer.from(ecdsaSha256))
let algorithmCount = 0
while (algorithmOffset >= 0) {
  unsupportedAlgorithmBytes[algorithmOffset + ecdsaSha256.length - 1] = 0x03
  algorithmCount += 1
  algorithmOffset = unsupportedAlgorithmBytes.indexOf(Buffer.from(ecdsaSha256), algorithmOffset + 1)
}
if (algorithmCount !== 2) throw new Error('expected two ECDSA algorithms in path intermediate')
const mismatchingAlgorithmBytes = rewriteDerNodeOccurrence(
  pathIntermediateBytes,
  `${pathLengthFixture.id}/untrusted_intermediates[0] mismatching algorithm`,
  ecdsaSha256,
  [0x30, 0x0d, 0x06, 0x09, 0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0b, 0x05, 0x00],
  1,
)

const basicConstraintsOid = [0x06, 0x03, 0x55, 0x1d, 0x13]
const policyBytes = mutateUnique(
  pathIntermediateBytes,
  `${pathLengthFixture.id}/untrusted_intermediates[0] policy`,
  basicConstraintsOid,
  (bytes, offset) => {
    bytes[offset + basicConstraintsOid.length - 1] = 0x20
  },
)
const tlsFeatureBytes = rewriteDerNodes(
  pathIntermediateBytes,
  `${pathLengthFixture.id}/untrusted_intermediates[0] TLS Feature`,
  basicConstraintsOid,
  [0x06, 0x08, 0x2b, 0x06, 0x01, 0x05, 0x05, 0x07, 0x01, 0x18],
  1,
)
const projectFixtures = [
  {
    id: 'source::rfc5280::no-keyusage/peer_certificate',
    sourceCase: 'rfc5280::no-keyusage',
    sourceCertificateRole: 'peer_certificate',
    sourceCertificateSha256: noKeyUsageLeaf.sha256,
    mutation: 'none; exact pinned source certificate',
    der: noKeyUsageLeaf.der,
    sha256: noKeyUsageLeaf.sha256,
    expectedSilk: { result: 'Success' },
  },
  {
    id: 'silk::wrong-signature-first/intermediates[0]',
    sourceCase: pathLengthFixture.id,
    sourceCertificateRole: 'untrusted_intermediates[0]',
    sourceCertificateSha256: pathLengthFixture.intermediates[0].sha256,
    mutation: 'xor 0x01 into the final ECDSA signature octet',
    der: wrongSignatureBytes.toString('base64'),
    sha256: createHash('sha256').update(wrongSignatureBytes).digest('hex'),
    expectedSilk: { result: 'Failure', reason: 'InvalidSignature' },
  },
  derived(
    'silk::ignored-anchor-validity-self-signature/trusted_certs[0]',
    'rfc5280::no-keyusage',
    'trusted_certs[0]',
    noKeyUsageRoot,
    'replace notAfter with 2000-01-01 and xor 0x01 into the final self-signature octet',
    ignoredAnchorMetadataBytes,
    'Success',
    undefined,
  ),
  derived(
    'silk::missing-ec-parameters/trusted_certs[0]',
    'rfc5280::no-keyusage',
    'trusted_certs[0]',
    noKeyUsageRoot,
    'remove the named-curve parameters OID from SubjectPublicKeyInfo',
    missingEcParametersBytes,
    'Failure',
    'InvalidKey',
  ),
  derived(
    'silk::unsupported-signature-parameters/intermediates[0]',
    pathLengthFixture.id,
    'untrusted_intermediates[0]',
    pathIntermediate,
    'append DER NULL parameters to both ECDSA-with-SHA256 AlgorithmIdentifiers',
    unsupportedParametersBytes,
    'Failure',
    'UnsupportedAlgorithm',
  ),
  derived(
    'silk::unsupported-signature-algorithm/intermediates[0]',
    pathLengthFixture.id,
    'untrusted_intermediates[0]',
    pathIntermediate,
    'replace both ECDSA-with-SHA256 OIDs with ECDSA-with-SHA384',
    unsupportedAlgorithmBytes,
    'Failure',
    'UnsupportedAlgorithm',
  ),
  derived(
    'silk::mismatching-signature-algorithms/intermediates[0]',
    pathLengthFixture.id,
    'untrusted_intermediates[0]',
    pathIntermediate,
    'replace the outer ECDSA-with-SHA256 AlgorithmIdentifier with RSA-PKCS1-SHA256',
    mismatchingAlgorithmBytes,
    'Failure',
    'UnsupportedParameters',
  ),
  derived(
    'silk::certificate-policy/intermediates[0]',
    pathLengthFixture.id,
    'untrusted_intermediates[0]',
    pathIntermediate,
    'replace the BasicConstraints OID with certificatePolicies while retaining its DER value',
    policyBytes,
    'Failure',
    'UnsupportedPolicy',
  ),
  derived(
    'silk::tls-feature/intermediates[0]',
    pathLengthFixture.id,
    'untrusted_intermediates[0]',
    pathIntermediate,
    'replace the BasicConstraints OID with TLS Feature while retaining its DER value',
    tlsFeatureBytes,
    'Failure',
    'UnsupportedExtension',
  ),
]

const output = {
  schemaVersion: 1,
  source: sourceUrl,
  sourceSha256,
  license: 'Apache-2.0',
  licenseUrl:
    'https://github.com/C2SP/x509-limbo/blob/3f8cba420e90322223486086054401189b7b320e/LICENSE',
  zigHttpParityCommit: '1bc892110da738d6137b3f0b7e8e3a586ce09928',
  defaultValidationTime: '2024-03-15T00:00:00Z',
  cases,
  projectFixtures,
}

const destination = new URL('../test/fixtures/certificate-path-limbo.json', import.meta.url)
writeFileSync(destination, `${JSON.stringify(output, undefined, 2)}\n`)
