const registry = process.env.NPM_REGISTRY ?? 'https://registry.npmjs.org'

function readPublishedPackages() {
  const raw = process.env.PUBLISHED_PACKAGES
  if (!raw) throw new Error('PUBLISHED_PACKAGES is empty')

  const published = JSON.parse(raw)
  if (!Array.isArray(published) || published.length === 0) {
    throw new Error('PUBLISHED_PACKAGES must contain a non-empty array')
  }
  return published
}

export async function fetchRegistryManifest(
  name,
  version,
  { fetchImpl = fetch, retries = 5 } = {},
) {
  const url = new URL(encodeURIComponent(name).replace(/%40/u, '@'), `${registry}/`).href

  for (let attempt = 0; ; attempt++) {
    const response = await fetchImpl(url, { headers: { Accept: 'application/json' } })
    if (!response.ok) throw new Error(`Registry returned ${response.status} for ${name}`)

    const packument = await response.json()
    const manifest = packument?.versions?.[version]
    if (manifest) return manifest
    if (attempt >= retries) {
      throw new Error(`Registry does not list ${name}@${version} after ${retries + 1} attempts`)
    }
    await new Promise((resolve) => setTimeout(resolve, 2000 * (attempt + 1)))
  }
}

export function checkManifest(name, version, manifest) {
  const failures = []

  if (manifest.version !== version) {
    failures.push(`registry reports ${manifest.version}, expected ${version}`)
  }
  if (manifest._npmUser?.trustedPublisher === undefined) {
    failures.push('the version was not published through a trusted publisher')
  }
  if (manifest.dist?.attestations?.provenance === undefined) {
    failures.push('the version has no provenance attestation')
  }

  return failures.map((failure) => `${name}@${version}: ${failure}`)
}

async function main() {
  const published = readPublishedPackages()
  const failures = (
    await Promise.all(
      published.map(async ({ name, version }) => ({
        name,
        version,
        manifest: await fetchRegistryManifest(name, version),
      })),
    )
  ).flatMap(({ name, version, manifest }) => checkManifest(name, version, manifest))

  if (failures.length > 0) {
    throw new Error(`Release trust verification failed:\n${failures.join('\n')}`)
  }
}

if (process.env.PUBLISHED_PACKAGES !== undefined) await main()
