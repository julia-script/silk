import { unreachable } from './raise.js'
import vectors from '../fixtures/x25519/vectors.json' with { type: 'json' }

const bytes = (hex: string): string => `[${Array.from(Buffer.from(hex, 'hex')).join(', ')}]`
const array = (name: string, hex: string): string =>
  `let ${name}: [u8; ${hex.length / 2}] = ${bytes(hex)}`
const imports = `import silk.x25519 { X25519, X25519Error }
import silk.result { Result }
import silk.random { Random }
import silk.effect { Effect }
import silk.u8
import silk.usize
`
const helpers = `
fn equal(left: &[u8], right: &[u8]) -> bool {
  if left.length != right.length { return false }
  let mut index: usize = 0
  while index < left.length {
    if left[index] != right[index] { return false }
    index = index + 1
  }
  return true
}
fn check(scalar: &[u8], peer: &[u8], expected: &[u8], publicOnly: bool, failure: bool) -> bool {
  let imported = X25519.fromSecret(scalar)
  return match move imported {
    Result.Failure { error } => false
    Result.Success { value: key } => {
      if publicOnly {
        let actual = X25519.publicKey(&key)
        return equal(&actual, expected)
      }
      let agreed = X25519.agree(move key, peer)
      return match move agreed {
        Result.Success { value } => failure == false && equal(&value, expected)
        Result.Failure { error } => match move error {
          X25519Error.AllZeroSharedSecret => failure
          X25519Error.InvalidScalarLength => false
          X25519Error.InvalidPeerLength => false
        }
      }
    }
  }
}
`
const runtimeVectors = vectors.filter((_, index) => index < 2 || index > 5)
const cases = runtimeVectors.map(
  (entry, index) => `
fn case${index}() -> bool {
  ${array('scalar', entry.scalar)}
  ${array('peer', entry.peer)}
  ${array('expected', entry.expected ?? '')}
  return check(&scalar, &peer, &expected, ${entry.kind === 'public'}, ${entry.expected === null})
}`,
)
const alice = vectors[2] ?? unreachable('Missing Alice fixture')
const bob = vectors[3] ?? unreachable('Missing Bob fixture')
const alicePublic = alice.expected ?? unreachable('Missing Alice public bytes')
const bobPublic = bob.expected ?? unreachable('Missing Bob public bytes')
const exchangeShared = vectors[4]?.expected ?? unreachable('Missing shared exchange bytes')
const generation = `
// Scripted entropy is test-only. Production requires a conforming secure Random provider.
struct Scripted {
  bytes: usize
  calls: usize
}
impl Random for Scripted {
  effect fn fillBytes(self: &mut Self, output: &mut [u8]) -> () {
    ${array('draws', alice.scalar + bob.scalar)}
    let mut index: usize = 0
    while index < output.length {
      output[index] = draws[self.bytes + index]
      index = index + 1
    }
    self.bytes = self.bytes + output.length
    self.calls = self.calls + 1
  }
}
fn generation() -> bool {
  let mut provider = Scripted { bytes: 0, calls: 0 }
  let first = run X25519.generate() |> Effect.provideMut<Random>(&mut provider)
  if provider.bytes != 32 || provider.calls != 1 { return false }
  let second = run X25519.generate() |> Effect.provideMut<Random>(&mut provider)
  if provider.bytes != 64 || provider.calls != 2 { return false }
  ${array('expectedFirst', alicePublic)}
  ${array('expectedSecond', bobPublic)}
  let publicFirst = X25519.publicKey(&first)
  let publicSecond = X25519.publicKey(&second)
  if equal(&publicFirst, &expectedFirst) == false || equal(&publicSecond, &expectedSecond) == false { return false }
  ${array('expectedShared', exchangeShared)}
  let firstResult = X25519.agree(move first, &publicSecond)
  let secondResult = X25519.agree(move second, &publicFirst)
  return match move firstResult {
    Result.Failure { error } => false
    Result.Success { value: firstShared } => match move secondResult {
      Result.Failure { error } => false
      Result.Success { value: secondShared } => equal(&firstShared, &expectedShared) && equal(&secondShared, &expectedShared)
    }
  }
}
fn invalidScalar(bytes: &[u8]) -> bool {
  let imported = X25519.fromSecret(bytes)
  return match move imported {
    Result.Success { value } => false
    Result.Failure { error } => match move error {
      X25519Error.InvalidScalarLength => true
      X25519Error.InvalidPeerLength => false
      X25519Error.AllZeroSharedSecret => false
    }
  }
}
fn invalidPeer(peer: &[u8]) -> bool {
  ${array('scalar', alice.scalar)}
  let imported = X25519.fromSecret(&scalar)
  return match move imported {
    Result.Failure { error } => false
    Result.Success { value: key } => {
      let result = X25519.agree(move key, peer)
      return match move result {
        Result.Success { value } => false
        Result.Failure { error } => match move error {
          X25519Error.InvalidPeerLength => true
          X25519Error.InvalidScalarLength => false
          X25519Error.AllZeroSharedSecret => false
        }
      }
    }
  }
}
fn invalidWidths() -> bool {
  ${[0, 31, 33]
    .map(
      (n) => `${array(`bytes${n}`, '00'.repeat(n))}
  if invalidScalar(&bytes${n}) == false || invalidPeer(&bytes${n}) == false { return false }`,
    )
    .join('\n')}
  return true
}
`
export const x25519AcceptanceSource = `${imports}${helpers}${cases.join('\n')}${generation}
pub fn main() -> i32 {
  ${runtimeVectors.map((_, index) => `if case${index}() == false { return ${index + 1} }`).join('\n  ')}
  if generation() == false { return 30 }
  if invalidWidths() == false { return 31 }
  return 42
}`

// One RFC multiplication covers the bounded u64 field operations and owned key on wasm32.
export const x25519WasmAcceptanceSource = `${imports}${helpers}${cases[0]}
pub fn main() -> i32 { if case0() == false { return 1 } return 0 }`
