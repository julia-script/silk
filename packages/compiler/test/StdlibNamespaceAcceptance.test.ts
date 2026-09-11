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
  let digest = state.finish()
  state.update(&empty)
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
