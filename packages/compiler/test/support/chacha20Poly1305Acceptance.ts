import { readFileSync } from 'node:fs'
import { unreachable } from './raise.js'

interface AeadVector {
  readonly name: string
  readonly key: string
  readonly nonce: string
  readonly aad: string
  readonly plaintext: string
  readonly ciphertext: string
  readonly tag: string
}
interface PolyVector {
  readonly name: string
  readonly key: string
  readonly message: string
  readonly tag: string
}
const vectors: {
  readonly aead: ReadonlyArray<AeadVector>
  readonly poly: ReadonlyArray<PolyVector>
  readonly block: {
    readonly key: string
    readonly nonce: string
    readonly counter: number
    readonly output: string
  }
} = JSON.parse(readFileSync(new URL('../fixtures/chacha20-poly1305.json', import.meta.url), 'utf8'))

const bytes = (hex: string): string => `[${Array.from(Buffer.from(hex, 'hex')).join(', ')}]`
const declaration = (name: string, hex: string, mutable = false): string =>
  `let ${mutable ? 'mut ' : ''}${name}: [u8; ${hex.length / 2}] = ${bytes(hex)}`
const filled = (length: number): string => `[${Array.from({ length }, () => 165).join(', ')}]`

const checks = `fn equalBytes(actual: &[u8], expected: &[u8]) -> bool {
  if actual.length < expected.length { return false }
  let mut index: usize = 0
  while index < expected.length {
    if actual[index] != expected[index] { return false }
    index = index + 1
  }
  while index < actual.length {
    if actual[index] != 165 { return false }
    index = index + 1
  }
  return true
}
fn sentinels(actual: &[u8]) -> bool {
  let empty: [u8; 0] = []
  return equalBytes(actual, &empty)
}
fn succeeded(result: Result<(), AeadError>) -> bool {
  return match move result {
    Result<(), AeadError>.Success {value} => true
    Result<(), AeadError>.Failure {error} => false
  }
}
fn errorCode(result: Result<(), AeadError>) -> i32 {
  return match move result {
    Result<(), AeadError>.Success {value} => 0
    Result<(), AeadError>.Failure {error} => match move error {
      AeadError.InvalidKey => 1
      AeadError.InvalidNonce => 2
      AeadError.InvalidTag => 3
      AeadError.OutputTooSmall => 4
      AeadError.LimitExceeded => 5
      AeadError.AuthenticationFailed => 6
    }
  }
}`
const aeadCheck = (vector: AeadVector, index: number): string => `fn aead${index}() -> bool {
  ${declaration('key', vector.key)}
  ${declaration('nonce', vector.nonce, true)}
  ${declaration('aad', vector.aad, true)}
  ${declaration('plain', vector.plaintext)}
  ${declaration('expected', vector.ciphertext)}
  ${declaration('expectedTag', vector.tag)}
  let mut cipher: [u8; ${vector.plaintext.length / 2 + 1}] = ${filled(vector.plaintext.length / 2 + 1)}
  let mut tag: [u8; 16] = ${filled(16)}
  if !succeeded(ChaCha20Poly1305.seal(&key, &nonce, &aad, &plain, &mut cipher, &mut tag)) { return false }
  if !equalBytes(&cipher, &expected) || !equalBytes(&tag, &expectedTag) { return false }
  let mut opened: [u8; ${vector.plaintext.length / 2 + 1}] = ${filled(vector.plaintext.length / 2 + 1)}
  if !succeeded(ChaCha20Poly1305.open(&key, &nonce, &aad, &expected, &tag, &mut opened)) { return false }
  return equalBytes(&opened, &plain)
}`

// Tamper each authenticated input independently, and check every destination byte after rejection.
const tamperVector = vectors.aead.at(4) ?? unreachable('expected boundary-17 vector')
const rejection = `fn rejected() -> bool {
  ${declaration('key', tamperVector.key)}
  ${declaration('nonce', tamperVector.nonce, true)}
  ${declaration('aad', tamperVector.aad, true)}
  ${declaration('cipher', tamperVector.ciphertext, true)}
  ${declaration('tag', tamperVector.tag, true)}
  let mut output: [u8; 66] = ${filled(66)}
  tag[15] = u8.bitXor(tag[15], 1)
  if errorCode(ChaCha20Poly1305.open(&key, &nonce, &aad, &cipher, &tag, &mut output)) != 6 || !sentinels(&output) { return false }
  tag[15] = u8.bitXor(tag[15], 1)
  aad[16] = u8.bitXor(aad[16], 1)
  if errorCode(ChaCha20Poly1305.open(&key, &nonce, &aad, &cipher, &tag, &mut output)) != 6 || !sentinels(&output) { return false }
  aad[16] = u8.bitXor(aad[16], 1)
  nonce[11] = u8.bitXor(nonce[11], 1)
  if errorCode(ChaCha20Poly1305.open(&key, &nonce, &aad, &cipher, &tag, &mut output)) != 6 || !sentinels(&output) { return false }
  nonce[11] = u8.bitXor(nonce[11], 1)
  cipher[64] = u8.bitXor(cipher[64], 1)
  if errorCode(ChaCha20Poly1305.open(&key, &nonce, &aad, &cipher, &tag, &mut output)) != 6 || !sentinels(&output) { return false }
  let wrong: [u8; 0] = []
  let mut wrongTag: [u8; 15] = ${filled(15)}
  let mut short: [u8; 64] = ${filled(64)}
  let mut sealedTag: [u8; 16] = ${filled(16)}
  if errorCode(ChaCha20Poly1305.seal(&wrong, &wrong, &aad, &cipher, &mut short, &mut wrongTag)) != 1 { return false }
  if errorCode(ChaCha20Poly1305.seal(&key, &wrong, &aad, &cipher, &mut short, &mut wrongTag)) != 2 { return false }
  if errorCode(ChaCha20Poly1305.seal(&key, &nonce, &aad, &cipher, &mut short, &mut wrongTag)) != 3 { return false }
  if errorCode(ChaCha20Poly1305.seal(&key, &nonce, &aad, &cipher, &mut short, &mut sealedTag)) != 4 { return false }
  if !sentinels(&short) || !sentinels(&wrongTag) || !sentinels(&sealedTag) { return false }
  if errorCode(ChaCha20Poly1305.open(&wrong, &wrong, &aad, &cipher, &wrong, &mut short)) != 1 { return false }
  if errorCode(ChaCha20Poly1305.open(&key, &wrong, &aad, &cipher, &wrong, &mut short)) != 2 { return false }
  if errorCode(ChaCha20Poly1305.open(&key, &nonce, &aad, &cipher, &wrong, &mut short)) != 3 { return false }
  if errorCode(ChaCha20Poly1305.open(&key, &nonce, &aad, &cipher, &tag, &mut short)) != 4 { return false }
  return sentinels(&short)
}`

const implementation = readFileSync(
  new URL('../../stdlib/silk/chacha20_poly1305.silk', import.meta.url),
  'utf8',
)
const components = vectors.poly.map(
  (vector, index) => `fn poly${index}() -> bool {
  ${declaration('key', vector.key)}
  ${declaration('message', vector.message)}
  ${declaration('expected', vector.tag)}
  let mut state = polyMake(&key)
  polyUpdate(&mut state, &message, ${vector.message.length / 2})
  let tag = polyFinish(move state)
  return equalBytes(&tag, &expected)
}`,
)

/** One native corpus entry exercises RFC components, composition, bounds and rejection preservation. */
export const chacha20Poly1305NativeSource = `${implementation}
${checks}
${vectors.aead.map(aeadCheck).join('\n')}
${rejection}
${components.join('\n')}
fn components() -> bool {
  ${declaration('key', vectors.block.key)}
  ${declaration('nonce', vectors.block.nonce)}
  ${declaration('expected', vectors.block.output)}
  let output = block(&key, &nonce, ${vectors.block.counter})
  if !equalBytes(&output, &expected) { return false }
  // Scalar preflight exercises the exact counter boundary without inventing enormous slices.
  if errorCode(preflight(32, 12, 16, 274877906880, 274877906880)) != 0 { return false }
  if errorCode(preflight(32, 12, 16, 274877906881, 274877906881)) != 5 { return false }
  ${vectors.poly.map((_, index) => `if !poly${index}() { return false }`).join('\n  ')}
  return true
}
pub fn main() -> i32 {
  ${vectors.aead.map((_, index) => `if !aead${index}() { return ${index + 1} }`).join('\n  ')}
  if !rejected() { return 10 }
  if !components() { return 11 }
  return 42
}`

/** A small public-import witness crosses the stream-block boundary on wasm32. */
export const chacha20Poly1305WasmSource = `import silk.chacha20_poly1305 {ChaCha20Poly1305, AeadError}
import silk.result {Result}
${checks}
${vectors.aead.slice(4, 5).map(aeadCheck).join('\n')}
pub fn main() -> i32 {
  if !aead0() { return 1 }
  return 42
}`
