import assert from 'node:assert/strict'
import { test } from 'node:test'

import { checkManifest, fetchRegistryManifest } from './verify-release-trust.mjs'

const trusted = {
  version: '0.1.0',
  _npmUser: { trustedPublisher: { id: 'github' } },
  dist: { attestations: { provenance: {} } },
}

void test('accepts a trusted publish with provenance', () => {
  assert.deepEqual(checkManifest('@silklang/llvm', '0.1.0', trusted), [])
})

void test('rejects an untrusted publish without provenance', () => {
  const failures = checkManifest('@silklang/llvm', '0.1.0', {
    version: '0.1.0',
    _npmUser: { name: 'manual' },
  })

  assert.equal(failures.length, 2)
})

void test('reads the requested version from the registry packument', async () => {
  const fetchImpl = async () => ({
    ok: true,
    json: async () => ({ versions: { '0.1.0': trusted } }),
  })
  const manifest = await fetchRegistryManifest('@silklang/llvm', '0.1.0', { fetchImpl })

  assert.equal(manifest.version, '0.1.0')
})

void test('surfaces registry errors', async () => {
  const fetchImpl = async () => ({ ok: false, status: 503 })

  await assert.rejects(
    () => fetchRegistryManifest('@silklang/llvm', '0.1.0', { fetchImpl }),
    /returned 503/u,
  )
})
