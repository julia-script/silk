import { readFileSync } from 'node:fs'
import vectors from '../fixtures/aes-gcm/vectors.json' with { type: 'json' }

const bytes = (hex: string): string => `[${Array.from(Buffer.from(hex, 'hex')).join(', ')}]`
const array = (name: string, hex: string): string =>
  `let ${name}: [u8; ${hex.length / 2}] = ${bytes(hex)}`
const filled = (length: number, value = 165): string =>
  `[${Array.from({ length }, () => value).join(', ')}]`

const imports = `import silk.aes_gcm { AesGcm, AesGcmError }
import silk.result { Result }
import silk.u8
import silk.u64
import silk.usize
`

const helpers = `
fn equal(actual: &[u8], expected: &[u8]) -> bool {
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
fn vector(key: &[u8], nonce: &[u8], aad: &[u8], input: &[u8], expected: &[u8], expectedTag: &[u8]) -> bool {
  let mut output: [u8; 40] = ${filled(40)}
  let mut tag: [u8; 16] = ${filled(16)}
  let sealed = AesGcm.seal(key, nonce, aad, input, &mut output, &mut tag)
  if let Result.Failure { error } = move sealed { return false }
  if equal(&output, expected) == false || equal(&tag, expectedTag) == false { return false }
  let mut plain: [u8; 40] = ${filled(40)}
  let opened = AesGcm.open(key, nonce, aad, expected, expectedTag, &mut plain)
  if let Result.Failure { error } = move opened { return false }
  return equal(&plain, input)
}
fn errorCode(result: Result<(), AesGcmError>) -> i32 {
  return match move result {
    Result.Success { value } => 0
    Result.Failure { error } => match move error {
      AesGcmError.InvalidKeyLength => 1
      AesGcmError.InvalidNonceLength => 2
      AesGcmError.InvalidTagLength => 3
      AesGcmError.OutputTooSmall => 4
      AesGcmError.LimitExceeded => 5
      AesGcmError.AuthenticationFailed => 6
    }
  }
}
fn rejectedOpen(code: i32, key: &[u8], nonce: &[u8], aad: &[u8], ciphertext: &[u8], tag: &[u8]) -> bool {
  let mut output: [u8; 40] = ${filled(40)}
  let empty: [u8; 0] = []
  let result = AesGcm.open(key, nonce, aad, ciphertext, tag, &mut output)
  return errorCode(move result) == code && equal(&output, &empty)
}
fn rejectedSeal(code: i32, key: &[u8], nonce: &[u8], input: &[u8]) -> bool {
  let mut output: [u8; 40] = ${filled(40)}
  let mut tag: [u8; 16] = ${filled(16)}
  let empty: [u8; 0] = []
  let result = AesGcm.seal(key, nonce, &empty, input, &mut output, &mut tag)
  return errorCode(move result) == code && equal(&output, &empty) && equal(&tag, &empty)
}
`
const caseSources = vectors.map(
  (entry, index) => `
fn case${index}() -> bool {
  ${array('key', entry.Key)}
  ${array('nonce', entry.IV)}
  ${array('aad', entry.AAD)}
  ${array('input', entry.PT)}
  ${array('expected', entry.CT)}
  ${array('tag', entry.Tag)}
  return vector(&key, &nonce, &aad, &input, &expected, &tag)
}`,
)

const cases = caseSources.join('\n')

// One nonempty fixture gives independent authentication failures for every bound input.
const tamper = vectors.find((entry) => entry.Key.length === 32 && entry.PT.length === 34)
if (tamper === undefined) throw new Error('Missing AES-GCM boundary fixture')
const negatives = `fn negatives() -> bool {
  ${array('key', tamper.Key)}
  ${array('nonce', tamper.IV)}
  ${array('aad', tamper.AAD)}
  ${array('ciphertext', tamper.CT)}
  ${array('tag', tamper.Tag)}
  let mut wrongKey = key
  wrongKey[0] = u8.bitXor(wrongKey[0], 1)
  if rejectedOpen(6, &wrongKey, &nonce, &aad, &ciphertext, &tag) == false { return false }
  let mut wrongNonce = nonce
  wrongNonce[0] = u8.bitXor(wrongNonce[0], 1)
  if rejectedOpen(6, &key, &wrongNonce, &aad, &ciphertext, &tag) == false { return false }
  let mut wrongAad = aad
  wrongAad[0] = u8.bitXor(wrongAad[0], 1)
  if rejectedOpen(6, &key, &nonce, &wrongAad, &ciphertext, &tag) == false { return false }
  let mut wrongCiphertext = ciphertext
  wrongCiphertext[16] = u8.bitXor(wrongCiphertext[16], 1)
  if rejectedOpen(6, &key, &nonce, &aad, &wrongCiphertext, &tag) == false { return false }
  let mut index: usize = 0
  while index < 16 {
    let mut wrongTag = tag
    wrongTag[index] = u8.bitXor(wrongTag[index], 1)
    if rejectedOpen(6, &key, &nonce, &aad, &ciphertext, &wrongTag) == false { return false }
    index = index + 1
  }
  let empty: [u8; 0] = []
  ${[0, 15, 17, 24, 31, 33]
    .map(
      (n) => `let key${n}: [u8; ${n}] = ${filled(n, 0)}
  if rejectedOpen(1, &key${n}, &nonce, &aad, &ciphertext, &tag) == false || rejectedSeal(1, &key${n}, &nonce, &ciphertext) == false { return false }`,
    )
    .join('\n')}
  ${[0, 11, 13]
    .map(
      (n) => `let nonce${n}: [u8; ${n}] = ${filled(n, 0)}
  if rejectedOpen(2, &key, &nonce${n}, &aad, &ciphertext, &tag) == false || rejectedSeal(2, &key, &nonce${n}, &ciphertext) == false { return false }`,
    )
    .join('\n')}
  ${[0, 15, 17]
    .map(
      (n) => `let mut tag${n}: [u8; ${n}] = ${filled(n)}
  if rejectedOpen(3, &key, &nonce, &aad, &ciphertext, &tag${n}) == false { return false }
  let mut out${n}: [u8; 40] = ${filled(40)}
  let badTag${n} = AesGcm.seal(&key, &nonce, &aad, &ciphertext, &mut out${n}, &mut tag${n})
  if errorCode(move badTag${n}) != 3 || equal(&out${n}, &empty) == false || equal(&tag${n}, &empty) == false { return false }`,
    )
    .join('\n')}
  let mut short: [u8; 16] = ${filled(16)}
  let mut tagOut: [u8; 16] = ${filled(16)}
  let shortSeal = AesGcm.seal(&key, &nonce, &aad, &ciphertext, &mut short, &mut tagOut)
  if errorCode(move shortSeal) != 4 || equal(&short, &empty) == false || equal(&tagOut, &empty) == false { return false }
  let shortOpen = AesGcm.open(&key, &nonce, &aad, &ciphertext, &tag, &mut short)
  return errorCode(move shortOpen) == 4 && equal(&short, &empty)
}`

// The actual private arithmetic predicate is extracted rather than publishing a test API or
// constructing invalid slices that pretend to own enormous address ranges.
const implementation = readFileSync(
  new URL('../../stdlib/silk/aes_gcm.silk', import.meta.url),
  'utf8',
)
const start = implementation.indexOf('fn lengthsAdmitted(')
const end = implementation.indexOf('fn validate(', start)
if (start < 0 || end < 0) throw new Error('Missing AES-GCM length admission predicate')
const limits = implementation.slice(start, end)
export const aesGcmAcceptanceSource = `${imports}${helpers}${cases}
${negatives}
${limits}
pub fn main() -> i32 {
  ${vectors.map((_, index) => `if case${index}() == false { return ${index + 1} }`).join('\n  ')}
  if negatives() == false { return 20 }
  if lengthsAdmitted(68719476704, 2305843009213693951) == false { return 21 }
  if lengthsAdmitted(68719476705, 0) || lengthsAdmitted(0, 2305843009213693952) { return 22 }
  if lengthsAdmitted(u64.MAX, 0) || lengthsAdmitted(0, u64.MAX) { return 23 }
  return 42
}`

// One partial AES-256 block exercises 64-bit GHASH on wasm32, plus tag rejection before output.
export const aesGcmWasmAcceptanceSource = `${imports}${helpers}${caseSources[9]}
pub fn main() -> i32 {
  if case9() == false { return 1 }
  ${array('key', tamper.Key)}
  ${array('nonce', tamper.IV)}
  ${array('aad', tamper.AAD)}
  ${array('ciphertext', tamper.CT)}
  let tag: [u8; 16] = ${filled(16, 0)}
  if rejectedOpen(6, &key, &nonce, &aad, &ciphertext, &tag) == false { return 2 }
  return 0
}`
