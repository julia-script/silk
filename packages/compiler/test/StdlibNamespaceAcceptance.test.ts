import * as AnalysisFixture from './support/AnalysisFixture.js'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const namespaceProgram = `import silk.option { Option }
import silk.result { Result }
import silk.vector { Vector }
pub fn main() -> i32 {
  let values = Vector.make<i32>()
  let optional = Option.some<i32>(40)
  let result = Result.succeed<i32, i32>(2)
  drop values
  drop optional
  drop result
  return 42
}`

it.effect('resolves Option, Result, and Vector operations through their namespaces', () =>
  Effect.gen(function* () {
    const module = 'stdlib-namespace/qualified'
    const snapshot = yield* AnalysisFixture.retainingMain(module, ascii(namespaceProgram))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    for (const [spelling, expectedModule] of [
      ['Vector.make', 'silk/vector'],
      ['Option.some', 'silk/option'],
      ['Result.succeed', 'silk/result'],
    ] as const) {
      const offset = namespaceProgram.indexOf(spelling) + spelling.indexOf('.') + 1
      const occurrence = Analysis.semanticOccurrenceAt(snapshot, module, offset)
      assert.strictEqual(occurrence?.role, 'Value', spelling)
      assert.strictEqual(occurrence?.declaration?.module, expectedModule, spelling)
    }
  }),
)

it.effect('resolves the bounded byte duplex and scripted memory provider surfaces', () =>
  Effect.gen(function* () {
    const source = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.byte_duplex { ByteDuplex, ByteIoError, ReadTransfer }
import silk.bytes { Bytes }
import silk.effect { Effect }
import silk.memory_byte_duplex { MemoryByteDuplex, MemoryReadEvent, MemoryWriteAction, MemoryWriteEvent }
import silk.option { Option }
import silk.system_clock { SystemClock }
import silk.tls_connection { Connection, ConnectionError, ConnectionOptions, ConnectionPhase }
import silk.vector { Vector }
effect fn makeProvider() -> MemoryByteDuplex ! OutOfMemoryError ? &mut Allocator {
  let data = run Bytes.copy(&b"hello")
  let mut reads = Vector.make<MemoryReadEvent>()
  run Vector.append<MemoryReadEvent>(&mut reads, MemoryReadEvent.Data {
    readyAt: SystemClock.make(1, 0),
    bytes: move data,
  })
  let mut writes = Vector.make<MemoryWriteEvent>()
  run Vector.append<MemoryWriteEvent>(&mut writes, MemoryWriteEvent {
    readyAt: SystemClock.make(1, 0),
    action: MemoryWriteAction.Accept {count: 2},
  })
  return run MemoryByteDuplex.make(move reads, move writes, 16, 8, Option.none<i32>())
}
pub fn main() -> i32 { return 42 }`
    const snapshot = yield* AnalysisFixture.retainingMain(
      'stdlib-namespace/byte-duplex',
      ascii(source),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect('resolves selected scope actors for nonprimitive operation modules', () =>
  Effect.gen(function* () {
    const source = `import silk.execution { Execution }
import silk.format { Format }
import silk.hash { Hash }
import silk.metrics { Metrics }
import silk.numeric { Numeric }
import silk.raw_buffer { RawBuffer }
import silk.slot { Slot }
import silk.unicode { Unicode }
import silk.unicode_tables { UnicodeTables }

fn rawCount(buffer: &RawBuffer<i32>) -> usize {
  unsafe { return RawBuffer.count<i32>(buffer) }
  return 0
}

fn take(slot: Slot<i32>) -> i32 {
  unsafe { return Slot.take<i32>(move slot) }
  return 0
}

fn notify(execution: &mut Intrinsic.Execution<i32>) -> () {
  return Execution.notifyInitial<i32>(move execution)
}

pub fn main() -> i32 {
  let parsed = Format.signedValue("42")
  let seed = Hash.seed(17)
  let metrics = Metrics.make()
  let answer = Numeric.add<i32>(40, 2)
  let unicodeVersion = Unicode.dataVersion()
  let tableVersion = UnicodeTables.dataVersion()
  drop parsed
  drop seed
  drop metrics
  drop unicodeVersion
  drop tableVersion
  return answer
}`
    const snapshot = yield* AnalysisFixture.retainingMain(
      'stdlib-namespace/scope-actors',
      ascii(source),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect('consumes SHA and HMAC states when finish finalizes them', () =>
  Effect.gen(function* () {
    const source = `import silk.sha2 { Sha256 }
import silk.hmac { HmacSha256, HmacSha384 }
pub fn main() -> i32 {
  let empty: [u8; 0] = []
  let mut state = Sha256.make()
  state.update(&empty)
  let checkpoint = state.checkpoint()
  state.update(&empty)
  let checkpointAgain = state.checkpoint()
  let digest = state.finish()
  state.update(&empty)
  drop checkpoint
  drop checkpointAgain
  drop digest
  let mut hmac256 = HmacSha256.make(&empty)
  let tag256 = hmac256.finish()
  hmac256.update(&empty)
  drop tag256
  let hmac384 = HmacSha384.make(&empty)
  let tag384 = hmac384.finish()
  let again = hmac384.finish()
  drop tag384
  drop again
  return 42
}`
    const snapshot = yield* AnalysisFixture.retainingMain(
      'stdlib-namespace/consuming-sha-finish',
      ascii(source),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['OWN0001', 'OWN0001', 'OWN0001'],
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) =>
        source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      ),
      ['state', 'hmac256', 'hmac384'],
    )
  }),
)

it.effect('rejects overlapping ChaCha20-Poly1305 input and output borrows', () =>
  Effect.gen(function* () {
    const source = `import silk.chacha20_poly1305 { ChaCha20Poly1305 }
pub fn main() -> i32 {
  let key: [u8; 0] = []
  let mut output: [u8; 0] = []
  let mut tag: [u8; 0] = []
  let sealed = ChaCha20Poly1305.seal(&key, &key, &key, &key, &mut output, &mut output)
  let opened = ChaCha20Poly1305.open(&key, &key, &key, &output, &tag, &mut output)
  drop sealed
  drop opened
  return 42
}`
    const snapshot = yield* AnalysisFixture.retainingMain(
      'stdlib-namespace/chacha20-poly1305',
      ascii(source),
    )
    const diagnostics = Analysis.diagnostics(snapshot)
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.code),
      ['OWN0010', 'OWN0010'],
    )
    assert.deepEqual(
      diagnostics.map((diagnostic) =>
        source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      ),
      ['&mut output', '&mut output'],
    )
  }),
)

it.effect('enforces P-256 scalar ownership and explicit Random', () =>
  Effect.gen(function* () {
    const source = `import silk.p256 { P256, P256Error }
import silk.result { Result }
pub fn consume() -> i32 {
  let bytes: [u8; 32] = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  let admitted = P256.fromBytes(&bytes)
  return match move admitted {
    Result<P256, P256Error>.Success { value: scalar } => {
      let publicKey = scalar.publicKey()
      let shared = scalar.agree(&publicKey)
      drop shared
      let again = scalar.publicKey()
      drop again
      return 42
    }
    Result<P256, P256Error>.Failure { error } => 1
  }
}
pub effect fn main() -> i32 {
  let scalar = run P256.generate()
  drop scalar
  return consume()
}`
    const snapshot = yield* AnalysisFixture.retainingMain(
      'stdlib-namespace/p256-owner-and-random',
      ascii(source),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => ({
        code: diagnostic.code,
        text: source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      })),
      [
        { code: 'OWN0001', text: 'scalar' },
        { code: 'SEM0071', text: 'run P256.generate()' },
      ],
    )
  }),
)

it.effect('verifies ECDSA messages without providers or retained input loans', () =>
  Effect.gen(function* () {
    const source = `import silk.p256 { EcdsaP256Sha256 }
pub fn main() -> i32 {
  let mut key: [u8; 1] = [0]
  let mut message: [u8; 1] = [0]
  let mut signature: [u8; 1] = [0]
  let result = EcdsaP256Sha256.verify(&key, &message, &signature)
  key[0] = 1
  message[0] = 1
  signature[0] = 1
  drop result
  let again = EcdsaP256Sha256.verify(&key, &message, &signature)
  drop again
  return 42
}`
    const snapshot = yield* AnalysisFixture.retainingMain(
      'stdlib-namespace/ecdsa-borrowed-inputs',
      ascii(source),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect('keeps admitted RSA public key representation private', () =>
  Effect.gen(function* () {
    const source = `import silk.rsa { RsaPublicKey }
fn width(key: &RsaPublicKey) -> usize {return key.width}
pub fn main() -> i32 {return 0}`
    const snapshot = yield* AnalysisFixture.retainingMain('stdlib-namespace/rsa', ascii(source))
    const diagnostics = Analysis.diagnostics(snapshot)
    assert.deepEqual(
      diagnostics.map((d) => d.code),
      ['SEM0028'],
    )
    assert.deepEqual(
      diagnostics.map((d) => source.slice(d.span.start, d.span.end)),
      ['width'],
    )
  }),
)

it.effect('checks TLS digest widths and rejects overlapping secret/output borrows', () =>
  Effect.gen(function* () {
    const source = `import silk.tls_hkdf { TlsHkdfSha256 }
pub fn main() -> i32 {
  let mut secret: [u8; 32] = [${Array.from({ length: 32 }, () => 0).join(', ')}]
  let wrong: [u8; 48] = [${Array.from({ length: 48 }, () => 0).join(', ')}]
  let label: [u8; 1] = [1]
  let empty: [u8; 0] = []
  let badHash = TlsHkdfSha256.deriveSecretFromHash(&secret, &label, &wrong)
  let overlap = TlsHkdfSha256.expandLabel(&secret, &label, &empty, &mut secret)
  drop badHash
  drop overlap
  return 0
}`
    const snapshot = yield* AnalysisFixture.retainingMain(
      'stdlib-namespace/tls-contracts',
      ascii(source),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((d) => ({
        code: d.code,
        span: source.slice(d.span.start, d.span.end),
      })),
      [
        { code: 'SEM0056', span: 'wrong' },
        { code: 'OWN0010', span: ' &mut secret' },
      ],
    )
  }),
)

it.effect(
  'keeps TLS record views tied to their receive owner and rotates epochs by ownership',
  () =>
    Effect.gen(function* () {
      const source = `import silk.tls_record { CipherSuite, TlsRecordReceiver, TlsRecordSender }
fn inspect(receiver: &mut TlsRecordReceiver, input: &[u8]) -> i32 {
  let record = TlsRecordReceiver.record(&receiver.*)
  let progress = TlsRecordReceiver.feedInput(move receiver, input)
  drop record
  drop progress
  return 42
}
fn rotateSender(sender: &mut TlsRecordSender, secret: &[u8]) -> i32 {
  let replaced = TlsRecordSender.replaceEpoch(
    &mut sender.*,
    CipherSuite.Aes128GcmSha256,
    secret,
  )
  let remaining = sender.recordsRemaining()
  let pending = sender.pendingOutput()
  drop replaced
  drop remaining
  drop pending
  return 42
}
fn rotateReceiver(receiver: &mut TlsRecordReceiver, secret: &[u8]) -> i32 {
  let replaced = TlsRecordReceiver.replaceEpoch(
    &mut receiver.*,
    CipherSuite.Aes128GcmSha256,
    secret,
  )
  let record = receiver.record()
  drop replaced
  drop record
  return 42
}
pub fn main() -> i32 { return 42 }`
      const snapshot = yield* AnalysisFixture.retainingMain(
        'stdlib-namespace/tls-record-view',
        ascii(source),
      )
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => ({
          code: diagnostic.code,
          span: source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
        })),
        [{ code: 'OWN0011', span: 'receiver' }],
      )
    }),
)

it.effect('pins the authenticated TLS client provider row and output lifetime', () =>
  Effect.gen(function* () {
    const source = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.random { Random }
import silk.result { Result }
import silk.system_clock { Instant }
import silk.tls_client { Client, ClientConfig, TlsError }
import silk.trust_snapshot { TrustSnapshot }
import silk.usize
effect fn construct<'a>(config: &ClientConfig<'a>, trust: TrustSnapshot, time: Instant) -> Result<Client, TlsError>
! OutOfMemoryError
? &mut Allocator | &mut Random {
  return run Client.make(config, move trust, move time)
}
fn retainAcrossMutation(client: &mut Client) -> usize {
  let pending = Client.pendingOutput(&client.*)
  drop Client.ackWritten(&mut client.*, usize.ZERO)
  return pending.length
}
pub fn main() -> i32 { return 42 }`
    const snapshot = yield* AnalysisFixture.retainingMain(
      'stdlib-namespace/authenticated-tls-client',
      ascii(source),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => ({
        code: diagnostic.code,
        span: source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      })),
      [{ code: 'OWN0010', span: '&mut client.*' }],
    )
  }),
)
