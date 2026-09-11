import { createHash } from 'node:crypto'
import { fileURLToPath } from 'node:url'
import { NodeRuntime, NodeServices } from '@effect/platform-node'
import * as Console from 'effect/Console'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Schema from 'effect/Schema'

const fixtureId = 'rfc5280::no-keyusage/trusted_certs[0]'
const expectedDerSha256 = '2ffcf7efc81cbd3f7f2aa126fa6de0663e011ed71e7cdf2bec63a6c2af828455'
const expectedPemSha256 = 'e409d0b059a0e9124f42c0dbf95b2611525d605de9ef591a5f48e6e8754fe7b6'
const catalogPath = fileURLToPath(
  new URL('../../test/fixtures/certificate-profile-limbo.json', import.meta.url),
)
const destinationPath = fileURLToPath(new URL('./trust.pem', import.meta.url))

const sha256 = (bytes) => createHash('sha256').update(bytes).digest('hex')
const Catalog = Schema.Struct({
  fixtures: Schema.Array(
    Schema.Struct({
      id: Schema.String,
      profileOutcome: Schema.String,
      sha256: Schema.String,
      der: Schema.String,
    }),
  ),
})

const program = Effect.gen(function* () {
  const fs = yield* FileSystem.FileSystem
  const catalogText = yield* fs.readFileString(catalogPath)
  const catalog = yield* Schema.decodeEffect(Schema.fromJsonString(Catalog))(catalogText)
  const selected = catalog.fixtures.find((fixture) => fixture.id === fixtureId)
  if (selected === undefined)
    return yield* Effect.die(`Missing certificate-profile fixture: ${fixtureId}`)

  const der = Buffer.from(selected.der, 'base64')
  const derSha256 = sha256(der)
  if (selected.sha256 !== expectedDerSha256 || derSha256 !== expectedDerSha256)
    return yield* Effect.die(
      `Unexpected DER identity for ${fixtureId}: catalog=${selected.sha256}, decoded=${derSha256}`,
    )

  const base64 = der.toString('base64')
  const lines = []
  for (let offset = 0; offset < base64.length; offset += 64)
    lines.push(base64.slice(offset, offset + 64))
  const pem = Buffer.from(
    `-----BEGIN CERTIFICATE-----\n${lines.join('\n')}\n-----END CERTIFICATE-----\n`,
    'ascii',
  )
  const pemSha256 = sha256(pem)
  if (pemSha256 !== expectedPemSha256)
    return yield* Effect.die(`Unexpected canonical PEM identity for ${fixtureId}: ${pemSha256}`)

  const mode = process.argv[2] ?? '--check'
  if (mode === '--stdout') {
    yield* Console.log(pem.toString('ascii').trimEnd())
  } else if (mode === '--write') {
    yield* fs.writeFile(destinationPath, pem)
  } else if (mode === '--check') {
    const committed = yield* fs.readFile(destinationPath)
    if (!Buffer.from(committed).equals(pem))
      return yield* Effect.die(`${destinationPath} does not match deterministic catalog generation`)
    yield* Console.log(
      `${fixtureId}: ${pem.length} PEM bytes ${pemSha256}; ${der.length} DER bytes ${derSha256}`,
    )
  } else {
    return yield* Effect.die(`Unknown mode ${mode}; expected --check, --stdout, or --write`)
  }
})

NodeRuntime.runMain(program.pipe(Effect.provide(NodeServices.layer)))
