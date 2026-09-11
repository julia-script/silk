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
      ...(selectedPath === undefined
        ? {}
        : { anchorIndex: 0, intermediateIndices: selectedPath }),
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
const projectFixtures = [
  {
    id: 'silk::wrong-signature-first/intermediates[0]',
    sourceCase: pathLengthFixture.id,
    sourceCertificateSha256: pathLengthFixture.intermediates[0].sha256,
    mutation: 'xor 0x01 into the final ECDSA signature octet',
    der: wrongSignatureBytes.toString('base64'),
    sha256: createHash('sha256').update(wrongSignatureBytes).digest('hex'),
    expectedSilk: { result: 'Failure', reason: 'InvalidSignature' },
  },
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
