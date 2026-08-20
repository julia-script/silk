/**
 * The shared evaluation corpus: programs with pinned expected outcomes. The MIR interpreter's
 * tests consume it today; the native acceptance differential reuses it to compare interpreter
 * results against compiled output. Expected results were pinned against the fact-based
 * evaluator before the MIR retarget.
 */
import { readFileSync } from 'node:fs'
import * as Transcendental from '../../src/Transcendental.js'
import { floatMathPrograms } from './floatMath.js'
import {
  auditAllocatorSuspension,
  mixedServiceProviderSuspension,
  ownedAllocatorSuspensionFailure,
  ownedAllocatorSuspensionSuccess,
  ownedProviderSuspendedFailure,
  ownedProviderSuspendedSuccess,
} from './ownedAllocatorSuspension.js'
import { storedCatchSuspension } from './storedCatchSuspension.js'

// folded from Transcendental.test.ts: the canonical-bits program is generated from the pinned
// high-precision vectors plus the fixed edge cases, so the expected bits can never drift from the
// reference implementation. Transcendental.test.ts imports this source for its IR assertions.
interface TranscendentalVector {
  readonly width: 32 | 64
  readonly inputBits: string
  readonly operation: 'Sin' | 'Cos'
}

const transcendentalFixture = JSON.parse(
  readFileSync(new URL('../fixtures/transcendental-vectors.json', import.meta.url), 'utf8'),
) as { readonly vectors: ReadonlyArray<TranscendentalVector> }

const transcendentalVectors: ReadonlyArray<TranscendentalVector> = [
  ...transcendentalFixture.vectors,
  { width: 32, inputBits: '0x00000000', operation: 'Sin' },
  { width: 32, inputBits: '0x80000000', operation: 'Sin' },
  { width: 32, inputBits: '0x00000000', operation: 'Cos' },
  { width: 32, inputBits: '0x7f800000', operation: 'Sin' },
  { width: 32, inputBits: '0xff800000', operation: 'Cos' },
  { width: 32, inputBits: '0x7fc12345', operation: 'Sin' },
  { width: 64, inputBits: '0x0000000000000000', operation: 'Sin' },
  { width: 64, inputBits: '0x8000000000000000', operation: 'Sin' },
  { width: 64, inputBits: '0x0000000000000000', operation: 'Cos' },
  { width: 64, inputBits: '0x7ff0000000000000', operation: 'Sin' },
  { width: 64, inputBits: '0xfff0000000000000', operation: 'Cos' },
  { width: 64, inputBits: '0x7ff8123456789abc', operation: 'Sin' },
]

/** Canonical-bits transcendental program: bit-exact sin/cos parity across every engine. */
export const transcendentalCanonicalBits = `pub fn main() -> i32 {
${transcendentalVectors
  .map((vector, index) => {
    const inputBits = BigInt(vector.inputBits)
    const expectedBits = Transcendental.evaluate(vector.operation, {
      width: vector.width,
      bits: inputBits,
    }).bits
    return `  if f${vector.width}.toBits(f${vector.width}.${vector.operation.toLowerCase()}(f${vector.width}.fromBits(${inputBits.toString()}))) != ${expectedBits.toString()} { return ${index + 1} }`
  })
  .join('\n')}
  return 42
}`

export interface CorpusProgram {
  readonly name: string
  readonly source: string
  readonly expected:
    | { readonly _tag: 'Completes'; readonly result: number }
    | { readonly _tag: 'Trap' }
    | { readonly _tag: 'UnavailableEntry'; readonly reason: string }
}

export interface InvalidCorpusProgram {
  readonly name: string
  readonly source: string
  readonly codes: ReadonlyArray<string>
}

export const constrainedCallableForwarding = `service Counter {
  effect fn get() -> i32 ? &Counter
}
struct Fixed { value: i32 }
effect fn get(self: &Fixed) -> i32 { return self.value }
impl Counter for Fixed { get: Fixed.get }
effect fn read() -> i32 ? &Counter { return run Counter.get() }
fn forward<F>(value: F) -> F { return move value }
fn forwardAgain<F>(value: F) -> F {
  let forwarded = forward(move value)
  return move forwarded
}
pub fn main() -> i32 {
  let fixed = Fixed { value: 42 }
  let bind = forwardAgain(Effect.provide<Counter>(&fixed))
  return run bind(read())
}`

/** Failure payloads retain member bits while rows change their widest physical carrier lane. */
export const heterogeneousFailurePayload = `import silk.effects as Effect
struct Selected { code: i32 }
struct Small { code: i32 }
struct Wide { code: f64 }
effect fn risky() -> i32 ! Selected | Small { fail Small { code: 41 } }
effect fn recoverSelected(problem: Selected) -> i32 ! Wide { return problem.code }
effect fn recoverAny(problem: Small | Wide) -> i32 {
  return match move problem {
    Small { code } => code + 1
    Wide { code } => 0
  }
}
effect fn widenedResidual() -> i32 ! Small | Wide {
  return run Effect.catch<Selected>(risky(), recoverSelected)
}
effect fn completeResidual() -> i32 {
  return run Effect.catchAll(widenedResidual(), recoverAny)
}
pub fn main() -> i32 { return run completeResidual() }
`

/** Detached address payloads survive a row carrier widened by a floating handler failure. */
export const heterogeneousOwnedFailurePayload = `import silk.effects as Effect
struct Selected { code: i32 }
struct Owned { storage: Allocation }
struct Wide { code: f64 }
effect fn risky() -> i32 ! Selected | Owned | OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<i32>()
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let storage = run pending
  fail Owned { storage: move storage }
}
effect fn recoverSelected(problem: Selected) -> i32 ! Wide { return problem.code }
fn release(storage: Allocation) -> i32 {
  drop storage
  return 42
}
effect fn recoverAny(problem: Owned | OutOfMemoryError | Wide) -> i32 {
  return match move problem {
    Owned { storage } => release(move storage)
    OutOfMemoryError {} => 0
    Wide { code } => 0
  }
}
effect fn widenedResidual() -> i32 ! Owned | OutOfMemoryError | Wide {
  return run Effect.catch<Selected>(risky(), recoverSelected)
}
effect fn completeResidual() -> i32 {
  return run Effect.catchAll(widenedResidual(), recoverAny)
}
pub fn main() -> i32 { return run completeResidual() }
`

/** Reified residual unions release owned address payloads from a floating carrier when dropped. */
export const heterogeneousOwnedFailureResultDrop = `import silk.effects as Effect
struct Selected { code: i32 }
struct Owned { storage: Allocation }
struct Wide { code: f64 }
effect fn risky() -> i32 ! Selected | Owned | OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let pending = Allocator.allocate(Layout.of<i32>()) |> Effect.provideMut(&mut allocator)
  let storage = run pending
  fail Owned { storage: move storage }
}
effect fn recoverSelected(problem: Selected) -> i32 ! Wide { return problem.code }
pub fn main() -> i32 {
  let selected = Effect.catch<Selected>(risky(), recoverSelected)
  let completed = run Effect.result(move selected)
  drop completed
  return 42
}
`

const staticCompositionFixture = readFileSync(
  new URL('../fixtures/static-composition/static-composition-acceptance.silk', import.meta.url),
  'utf8',
)

const staticCompositionScenarios: ReadonlyArray<{
  readonly name: string
  readonly selection: string
  readonly result: number
  readonly cleanupWitness: boolean
}> = [
  { name: 'success', selection: 'RunRequest { value: 40 }', result: 42, cleanupWitness: false },
  { name: 'help', selection: 'HelpRequest {}', result: 10, cleanupWitness: false },
  {
    name: 'selection-failure',
    selection: 'SelectionFailureRequest {}',
    result: 20,
    cleanupWitness: false,
  },
  {
    name: 'decode-failure',
    selection: 'DecodeFailureRequest {}',
    result: 30,
    cleanupWitness: false,
  },
  {
    name: 'uncalled-cleanup',
    selection: 'UncalledCleanupRequest { value: 40 }',
    result: 40,
    cleanupWitness: true,
  },
  {
    name: 'called-cleanup',
    selection: 'CalledCleanupRequest { value: 40 }',
    result: 40,
    cleanupWitness: true,
  },
  {
    name: 'suspension',
    selection: 'SuspensionRequest { value: 42 }',
    result: 42,
    cleanupWitness: true,
  },
]

const selectStaticCompositionScenario = (selection: string): string =>
  staticCompositionFixture.replace('RunRequest {value: 40}', selection)

const withTrappingStaticCompositionDrop = (source: string): string =>
  source.replace(
    `    if self.value == 0 {
      let boom = 1 / 0
      return ()
    }
    self.value = 0
    return ()`,
    `    let values = [self.value]
    self.value = values[i32.toUsize(self.value)]
    return ()`,
  )

const staticCompositionCorpus: ReadonlyArray<CorpusProgram> = [
  ...staticCompositionScenarios.map(
    (scenario): CorpusProgram => ({
      name: `static-composition-${scenario.name}`,
      source: selectStaticCompositionScenario(scenario.selection),
      expected: { _tag: 'Completes', result: scenario.result },
    }),
  ),
  ...staticCompositionScenarios
    .filter((scenario) => scenario.cleanupWitness)
    .map(
      (scenario): CorpusProgram => ({
        name: `static-composition-${scenario.name}-cleanup-witness`,
        source: withTrappingStaticCompositionDrop(
          selectStaticCompositionScenario(scenario.selection),
        ),
        expected: { _tag: 'Trap' },
      }),
    ),
]

export const corpus: ReadonlyArray<CorpusProgram> = [
  {
    name: 'literal',
    source: 'pub fn main() -> i32 { return 42 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'constrained-callable-forwarding',
    source: constrainedCallableForwarding,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'identity',
    source: `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(42) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'second-parameter',
    source: `pub fn second(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { return second(10, 42) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'nested',
    source: `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(42)) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'nested-siblings',
    source: `pub fn identity(value: i32) -> i32 { return value }
pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { return choose(identity(1), identity(2)) }`,
    expected: { _tag: 'Completes', result: 2 },
  },
  {
    name: 'generic-specializations',
    source: `struct Pair { left: i32 right: i32 }
struct Box<T> { value: T }
fn identity<T>(value: T) -> T { return move value }
pub fn main() -> i32 {
  let scalar = Box<i32> { value: identity(0) }
  let pair = Box<Pair> { value: identity<Pair>(Pair { left: 40, right: 2 }) }
  return scalar.value + pair.value.left + pair.value.right
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'generic-partial-type-arguments',
    source: `struct Box<T> { value: T }
fn pick<A, B>(left: A, right: B) -> A { return move left }
fn phantom<A, B>(value: A) -> A { return move value }
pub fn main() -> i32 {
  let picked = pick<i32>(40, true)
  let boxed = pick<Box<i32>>(Box<i32> { value: 1 }, picked)
  return picked + boxed.value + phantom<i32, bool>(1)
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'same-specialization-recursion',
    source: `fn recurse<T>(value: T, remaining: i32) -> i32 {
  if remaining > 0 { return recurse<T>(move value, remaining - 1) }
  return 42
}
pub fn main() -> i32 { return recurse<i32>(1, 4) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'recursive-aggregate-return',
    source: `struct Pair { left: i32 right: i32 }
fn build(remaining: i32) -> Pair {
  if remaining == 0 { return Pair { left: 40, right: 2 } }
  return build(remaining - 1)
}
pub fn main() -> i32 {
  let pair = build(4)
  return pair.left + pair.right
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'recursive-mutable-slice',
    source: `fn fill(values: &mut [i32], index: usize) -> i32 {
  if index == 4 { return values[0] + values[1] + values[2] + values[3] }
  values[index] = usize.toI32(index) + 9
  return fill(&mut values, index + 1)
}
pub fn main() -> i32 {
  let mut values = [0, 0, 0, 0]
  return fill(&mut values, 0)
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'forward-call',
    source: `pub fn main() -> i32 { return answer() }
pub fn answer() -> i32 { return 42 }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'direct-recursion',
    source: `fn countdown(value: i32) -> i32 {
  if value == 0 { return 42 }
  return countdown(value - 1)
}
pub fn main() -> i32 { return countdown(4) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'mutual-recursion',
    source: `fn even(value: i32) -> i32 {
  if value == 0 { return 42 }
  return odd(value - 1)
}
fn odd(value: i32) -> i32 { return even(value - 1) }
pub fn main() -> i32 { return odd(5) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'unknown-call-trap',
    source: 'pub fn main() -> i32 { return missing() }',
    expected: { _tag: 'Trap' },
  },
  {
    name: 'inner-blocked-trap',
    source: `pub fn identity(value: i32) -> i32 { return value }
pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { return choose(identity(1), missing(2)) }`,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'binding',
    source: `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { let value = identity(42) return value }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'binding-chain',
    source: `pub fn main() -> i32 { let first = 40 let second = 2 return first }`,
    expected: { _tag: 'Completes', result: 40 },
  },
  {
    name: 'moved-binding',
    source: `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { let value = 42 return identity(move value) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'use-after-move-trap',
    source: `pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { let value = 42 return choose(move value, value) }`,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'arithmetic',
    source: 'pub fn main() -> i32 { return i32.subtract(i32.multiply(6, 7), 0) }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'operator-precedence',
    source: 'pub fn main() -> i32 { return 2 + 5 * 8 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'operator-pipeline',
    source: 'pub fn main() -> i32 { return 2 |> i32.add(40) }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'closed-operator-surface',
    source: `pub fn main() -> i32 {
if 6 * 7 != 42 { return 0 }
if 84 / 2 != 42 { return 0 }
if 85 % 43 != 42 { return 0 }
if 44 - 2 != 42 { return 0 }
if 40 + 2 != 42 { return 0 }
if !(1 < 2) { return 0 }
if !(2 <= 2) { return 0 }
if !(3 > 2) { return 0 }
if !(3 >= 3) { return 0 }
if true != true { return 0 }
if false == true { return 0 }
return (40 + 2) * 1
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'unary-bool-pipeline',
    source: 'pub fn main() -> i32 { if true |> bool.not { return 0 } return 42 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'signed-truncation',
    source: 'pub fn main() -> i32 { return i32.add(i32.divide(-7, 2), 45) }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'remainder-sign',
    source: 'pub fn main() -> i32 { return i32.add(i32.remainder(-7, 2), 43) }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'overflow-trap',
    source: 'pub fn main() -> i32 { return i32.add(2147483647, 1) }',
    expected: { _tag: 'Trap' },
  },
  {
    name: 'operator-negation-overflow-trap',
    source: 'pub fn main() -> i32 { return -(-2147483648) }',
    expected: { _tag: 'Trap' },
  },
  {
    name: 'divide-by-zero-trap',
    source: 'pub fn main() -> i32 { return i32.divide(1, 0) }',
    expected: { _tag: 'Trap' },
  },
  {
    name: 'minimum-division-trap',
    source: 'pub fn main() -> i32 { return i32.divide(-2147483648, -1) }',
    expected: { _tag: 'Trap' },
  },
  {
    name: 'branch-taken',
    source: 'pub fn main() -> i32 { if i32.equals(1, 1) { return 42 } return 0 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'branch-otherwise',
    source: 'pub fn main() -> i32 { if i32.equals(1, 2) { return 0 } return 42 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'branch-else',
    source:
      'pub fn main() -> i32 { if i32.lessThan(2, 1) { return 1 } else { return 42 } return 0 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'bool-not',
    source: 'pub fn main() -> i32 { if bool.not(i32.equals(1, 2)) { return 42 } return 0 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'operator-bool-not',
    source: 'pub fn main() -> i32 { if !(1 == 2) { return 42 } return 0 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'bool-through-function',
    source: `pub fn check(flag: bool) -> i32 { if flag { return 42 } return 0 }
pub fn main() -> i32 { return check(i32.greaterOrEqual(3, 3)) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'arm-binding',
    source:
      'pub fn main() -> i32 { let base = 40 if i32.equals(base, 40) { let bonus = 2 return i32.add(base, bonus) } return 0 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-inferred',
    source: 'pub fn main() -> i32 { let values = [10, 42] return values[1] }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-contextual-empty',
    source: `fn empty() -> [i32; 0] { return [] }
fn consume(values: [i32; 0]) -> i32 { return 42 }
pub fn main() -> i32 { return consume(empty()) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-nested',
    source: `fn choose(values: [[i32; 2]; 2], outer: usize, inner: usize) -> i32 { return values[outer][inner] }
pub fn main() -> i32 { return choose([[10, 11], [42, 43]], 1, 0) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-indexed-struct-field',
    source: `struct Pair { left: i32 right: i32 }
fn choose(values: [Pair; 2], index: usize) -> i32 { return values[index].left }
pub fn main() -> i32 { return choose([Pair { left: 10, right: 11 }, Pair { left: 42, right: 43 }], 1) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-whole-move',
    source: `struct Token { value: i32 }
pub fn main() -> i32 {
  let tokens = [Token { value: 10 }, Token { value: 42 }]
  let moved = move tokens
  return moved[1].value
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-negative-index-trap',
    source: `fn choose(values: [i32; 2], index: usize) -> i32 { return values[index] }
pub fn main() -> i32 { return choose([10, 42], -1) }`,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'array-upper-index-trap',
    source: `fn choose(values: [i32; 2], index: usize) -> i32 { return values[index] }
pub fn main() -> i32 { return choose([10, 42], 2) }`,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'array-zero-index-trap',
    source: `fn choose(values: [i32; 0], index: usize) -> i32 { return values[index] }
pub fn main() -> i32 { return choose([], 0) }`,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'mutable-scalar-loop',
    source: `pub fn main() -> i32 {
  let mut count = 0
  while count < 42 { count = count + 1 }
  return count
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'mutable-array-loop',
    source: `pub fn main() -> i32 {
  let mut values = [40, 0]
  let mut index = usize.add(0, 0)
  while index < 2 {
    values[index] = values[index] + 1
    index = index + 1
  }
  return values[0] + values[1]
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'loop-continue-break',
    source: `pub fn main() -> i32 {
  let mut index = usize.add(0, 0)
  while index < 50 {
    index = index + 1
    if index == 2 { continue }
    if index == 42 { break }
  }
  return usize.toI32(index)
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'mutable-struct-loop',
    source: `struct Pair { left: i32 right: i32 }
pub fn main() -> i32 {
  let mut pair = Pair { left: 0, right: 40 }
  while pair.left < 2 { pair.left = pair.left + 1 }
  return pair.left + pair.right
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'match-guarded-union-shared',
    source: `struct Left { value: i32 }
struct Right { value: i32 }
fn inspect(input: Left | Right) -> i32 {
  return match &input {
    Left { value } if false => 0
    Left { value: answer } => answer + 1
    Right { value } => value
  }
}
pub fn main() -> i32 { return inspect(Left { value: 41 }) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'match-move-nested-cleanup',
    source: `struct Token { value: i32 }
struct Box { token: Token discarded: Token }
pub fn main() -> i32 {
  let box = Box { token: Token { value: 42 }, discarded: Token { value: 0 } }
  return match move box {
    Box { token: Token { value }, .. } => value
  }
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'match-universal-fallback',
    source: `struct Left { value: i32 }
struct Right { value: i32 }
fn inspect(input: Left | Right) -> i32 {
  return match &input { Left { value } => value _ => 42 }
}
pub fn main() -> i32 { return inspect(Right { value: 0 }) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'match-exclusive-mutable',
    source: `struct Token { value: i32 }
pub fn main() -> i32 {
  let mut token = Token { value: 42 }
  return match &mut token { Token { value } => value }
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'nested-loops',
    source: `pub fn main() -> i32 {
  let mut outer = 0
  let mut total = 0
  while outer < 6 {
    let mut inner = 0
    while inner < 7 {
      total = total + 1
      inner = inner + 1
    }
    outer = outer + 1
  }
  return total
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'missing-entry',
    source: 'pub fn answer() -> i32 { return 42 }',
    expected: { _tag: 'UnavailableEntry', reason: 'MissingEntry' },
  },
  {
    name: 'generic-entry',
    source: 'pub fn main<T>() -> i32 { return 42 }',
    expected: { _tag: 'UnavailableEntry', reason: 'GenericEntry' },
  },
  {
    name: 'parameterized-entry',
    source: 'pub fn main(value: i32) -> i32 { return value }',
    expected: { _tag: 'UnavailableEntry', reason: 'ParameterizedEntry' },
  },
  // folded from StringAcceptance.test.ts: literals, owned copy/view/append, exact equality, and
  // scalar traversal.
  {
    name: 'string-owned-scalars',
    source: `import silk.string {
  ScalarCursor,
  ScalarStep,
  copy,
  append,
  view,
  scalarCursor,
  nextScalar,
  scalarValue,
  nextCursor
}
import silk.option { Some, None }

fn scalarSum(value: string, cursor: ScalarCursor) -> u32 {
  return match move nextScalar(value, move cursor) {
    Some<ScalarStep> { value: step } => continueSum(value, move step)
    None nothing => u32.toU32(0)
  }
}

fn continueSum(value: string, step: ScalarStep) -> u32 {
  let scalar = scalarValue(&step)
  let cursor = nextCursor(move step)
  return scalar + scalarSum(value, move cursor)
}

effect fn build() -> i32 ! OutOfMemoryError {
  let literal = "A\\u{a2}"
  if literal == "A\\u{a2}" {} else { return 1 }
  if literal != "A\\u{a3}" {} else { return 2 }

  let mut allocator = SystemAllocator.make()
  let copying = copy(literal) |> Effect.provideMut(&mut allocator)
  let mut owned = run copying
  let appending = append(&mut owned, "\\u{20ac}\\u{10348}")
    |> Effect.provideMut(&mut allocator)
  let appended = run appending
  let borrowed = view(&owned)
  if borrowed == "A\\u{a2}\\u{20ac}\\u{10348}" {} else { return 3 }
  if scalarSum(borrowed, scalarCursor()) == u32.toU32(74967) {} else { return 4 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from UnicodeNormalization.test.ts: the two normalized owners compared directly, which
  // the evaluator and native answer correctly while direct WebAssembly still cannot.
  {
    name: 'unicode-compared-directly',
    source: `import silk.string { String, view }
import silk.unicode { normalizeNfc }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let left = run normalizeNfc("\\u{e9}") |> Effect.provideMut(&mut allocator)
  let right = run normalizeNfc("e\\u{301}") |> Effect.provideMut(&mut allocator)
  if view(&left) == view(&right) {} else { return 1 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from CharacterLiteral.test.ts: every accepted escape, multi-byte scalars, and the six
  // comparisons. The source deliberately carries non-ASCII literals.
  {
    name: 'character-literal-acceptance',
    source: `import silk.char { equals, notEquals, lessThan, lessOrEqual, greaterThan, greaterOrEqual }

const asciiSpace: char = ' '
const asciiTab: char = '\\t'
const snowman: char = '\\u{2603}'

fn eq(left: char, right: char) -> bool { return left == right }
fn ne(left: char, right: char) -> bool { return left != right }
fn lt(left: char, right: char) -> bool { return left < right }
fn le(left: char, right: char) -> bool { return left <= right }
fn gt(left: char, right: char) -> bool { return left > right }
fn ge(left: char, right: char) -> bool { return left >= right }

pub fn main() -> i32 {
  if eq('a', 'a') {} else { return 1 }
  if ne('a', 'b') {} else { return 2 }
  if lt('a', 'b') {} else { return 3 }
  if le('a', 'a') {} else { return 4 }
  if gt('b', 'a') {} else { return 5 }
  if ge('a', 'a') {} else { return 6 }
  if eq('\\n', '\\u{a}') {} else { return 7 }
  if eq('\\r', '\\u{d}') {} else { return 8 }
  if eq('\\t', asciiTab) {} else { return 9 }
  if eq('\\0', '\\u{0}') {} else { return 10 }
  if eq('\\\\', '\\u{5c}') {} else { return 11 }
  if eq('\\'', '\\u{27}') {} else { return 12 }
  if eq('\\"', '"') {} else { return 13 }
  if eq('\\x41', 'A') {} else { return 14 }
  if eq(' ', asciiSpace) {} else { return 15 }
  if eq('é', '\\u{e9}') {} else { return 16 }
  if eq('☃', snowman) {} else { return 17 }
  if lt('é', snowman) {} else { return 18 }
  if gt('😀', snowman) {} else { return 19 }
  if equals('a', 'a') {} else { return 20 }
  if notEquals('a', 'b') {} else { return 21 }
  if lessThan('a', 'b') {} else { return 22 }
  if lessOrEqual('a', 'b') {} else { return 23 }
  if greaterThan('b', 'a') {} else { return 24 }
  if greaterOrEqual('b', 'a') {} else { return 25 }
  return 42
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from ShortCircuitOperatorAcceptance.test.ts: the counter proves the right operand of
  // `&&`/`||` runs exactly when short-circuiting says it must.
  {
    name: 'short-circuit-counting',
    source: `fn bump(counter: &mut [i32], answer: bool) -> bool {
  counter[0] = counter[0] + 1
  return answer
}

fn conjunction(gate: bool) -> i32 {
  let mut counter = [0]
  if gate && bump(&mut counter, true) { return counter[0] }
  return counter[0]
}

fn disjunction(gate: bool) -> i32 {
  let mut counter = [0]
  if gate || bump(&mut counter, true) { return counter[0] }
  return counter[0]
}

pub fn main() -> i32 {
  if conjunction(false) == 0 {} else { return 1 }
  if conjunction(true) == 1 {} else { return 2 }
  if disjunction(true) == 0 {} else { return 3 }
  if disjunction(false) == 1 {} else { return 4 }
  return 42
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from Transcendental.test.ts: bit-exact sin/cos results across every engine.
  {
    name: 'transcendental-canonical-bits',
    source: transcendentalCanonicalBits,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from HashedCollections.test.ts: seeded map growth with checked reads.
  {
    name: 'hashed-map-growth',
    source: `import silk.hash { HashKey, HashSeed, Word }
import silk.hash_map { HashMap, bucketCount, contains, get, insert, length, make, remove }
import silk.option { Option, Some, None }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let mut map = make<Word, i32>(HashKey.seed(4242))
  let mut key = 0
  while key < 40 {
    let previous = run insert<Word, i32>(&mut map, HashKey.word(i32.toU64(key)), key * 3)
      |> Effect.provideMut(&mut allocator)
    drop previous
    key = key + 1
  }
  if length<Word, i32>(&map) != 40 { return 1 }
  if bucketCount<Word, i32>(&map) <= 40 { return 2 }
  let mut probe = 0
  let mut total = 0
  while probe < 40 {
    let found = Option.unwrapOr<i32>(get<Word, i32>(&map, HashKey.word(i32.toU64(probe))), -1)
    if found != probe * 3 { return 3 }
    total = total + found
    probe = probe + 1
  }
  if total != 2340 { return 4 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 99 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from VectorAcceptance.test.ts: growth past the initial capacity with boundary reads.
  {
    name: 'vector-growth-reads',
    source: `import silk.vector { Vector, make, append, get, length, capacity }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let mut values = make<i32>()
  let pending0 = append<i32>(&mut values, 10) |> Effect.provideMut(&mut allocator)
  let appended0 = run pending0
  let pending1 = append<i32>(&mut values, 11) |> Effect.provideMut(&mut allocator)
  let appended1 = run pending1
  let pending2 = append<i32>(&mut values, 12) |> Effect.provideMut(&mut allocator)
  let appended2 = run pending2
  let pending3 = append<i32>(&mut values, 13) |> Effect.provideMut(&mut allocator)
  let appended3 = run pending3
  let pending4 = append<i32>(&mut values, 14) |> Effect.provideMut(&mut allocator)
  let appended4 = run pending4
  let pending5 = append<i32>(&mut values, 15) |> Effect.provideMut(&mut allocator)
  let appended5 = run pending5
  if length<i32>(&values) == 6 {} else { return 0 }
  if capacity<i32>(&values) == 8 {} else { return 1 }
  let first = get<i32>(&values, 0)
  let last = get<i32>(&values, 5)
  return first + last + 17
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from OwnedAllocationDispatch.test.ts: quota refusal propagates typed OutOfMemoryError.
  {
    name: 'owned-allocation-quota-refusal',
    source: `struct QuotaAllocator { remaining: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  if self.remaining == 0 { fail OutOfMemoryError {} }
  self.remaining = self.remaining - 1
  let mut inner = SystemAllocator.make()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
  let block = run recipe
  return move block
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = QuotaAllocator { remaining: 1 }
  let first = Layout.of<[i32; 2]>()
  let recipeA = Allocator.allocate(move first) |> Effect.provideMut(&mut allocator)
  let a = run recipeA
  let second = Layout.of<[i32; 2]>()
  let recipeB = Allocator.allocate(move second) |> Effect.provideMut(&mut allocator)
  let b = run recipeB
  drop a
  drop b
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 {
  return run Effect.catchAll(build(), recover)
}`,
    expected: { _tag: 'Completes', result: 7 },
  },
  // folded from SlotLaneWidth.test.ts: u8 lane writes, copies, and takes through a raw buffer.
  {
    name: 'slot-lane-u8',
    source: `effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[u8; 4]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<u8>(move allocation, 4)
    let firstWritten = Slot.write<u8>(RawBuffer.slot(&mut buffer, 0), 7)
    let secondWritten = Slot.write<u8>(RawBuffer.slot(&mut buffer, 3), 11)
    let firstCopy = Slot.copy(RawBuffer.slot(&mut buffer, 0))
    let secondCopy = Slot.copy(RawBuffer.slot(&mut buffer, 3))
    let firstTake = Slot.take(RawBuffer.slot(&mut buffer, 0))
    let secondTake = Slot.take(RawBuffer.slot(&mut buffer, 3))
    drop buffer
    return 100 + u8.toI32(firstCopy) + u8.toI32(secondCopy) + u8.toI32(firstTake) + u8.toI32(secondTake)
  }
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
    expected: { _tag: 'Completes', result: 136 },
  },
  // folded from SlotLaneWidth.test.ts: f64 lane parity including a negative value.
  {
    name: 'slot-lane-f64',
    source: `effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[f64; 4]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<f64>(move allocation, 4)
    let firstWritten = Slot.write<f64>(RawBuffer.slot(&mut buffer, 0), -7.0)
    let secondWritten = Slot.write<f64>(RawBuffer.slot(&mut buffer, 1), 11.0)
    let firstCopy = Slot.copy(RawBuffer.slot(&mut buffer, 0))
    let secondCopy = Slot.copy(RawBuffer.slot(&mut buffer, 1))
    let firstTake = Slot.take(RawBuffer.slot(&mut buffer, 0))
    let secondTake = Slot.take(RawBuffer.slot(&mut buffer, 1))
    drop buffer
    return 100 + f64.toI32(firstCopy) + f64.toI32(secondCopy) + f64.toI32(firstTake) + f64.toI32(secondTake)
  }
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
    expected: { _tag: 'Completes', result: 108 },
  },
  // folded from BytesAcceptance.test.ts: copy, append, and mutate through byte slices (exit 180).
  {
    name: 'bytes-parity',
    source: `import silk.bytes { Bytes, copy, append, asMutSlice, asSlice, length }

fn octet(value: u8) -> u8 { return value }

fn checksum(values: &[u8]) -> i32 {
  let mut index = usize.add(0, 0)
  let mut total = 0
  while index < values.length {
    total = total + u8.toI32(values[index])
    index = index + usize.add(0, 1)
  }
  return total
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let source = [octet(0), octet(255), octet(128), octet(1)]
  let copying = copy(&source) |> Effect.provideMut(&mut allocator)
  let mut bytes = run copying
  let suffix = [octet(42), octet(7)]
  let appending = append(&mut bytes, &suffix) |> Effect.provideMut(&mut allocator)
  let appended = run appending
  let mut writable = asMutSlice(&mut bytes)
  writable[1] = octet(2)
  let readable = asSlice(&bytes)
  if length(&bytes) == 6 {} else { return 1 }
  return checksum(move readable)
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 180 },
  },
  // folded from StaticByteViewIndexing.test.ts: out-of-bounds static byte read traps.
  {
    name: 'static-byte-view-bounds',
    source: `pub fn main() -> i32 {
  let bytes = b"\\x99\\x13\\x1d\\x00"
  let index = usize.add(0, 4)
  return u8.toI32(bytes[index])
}`,
    expected: { _tag: 'Trap' },
  },
  // folded from OwnedAllocationAcceptance.test.ts: guarded slot writes and takes release cleanly.
  {
    name: 'owned-allocation-guard',
    source: `struct Element { value: i32 }

effect fn build(count: usize) -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[Element; 4]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<Element>(move allocation, 4)
    let head0 = Element { value: 11 }
    let tail0 = Element { value: 31 }
    let first = Slot.write(RawBuffer.slot(&mut buffer, 0), move head0)
    let second = Slot.write(RawBuffer.slot(&mut buffer, 1), move tail0)
    let head = Slot.take(RawBuffer.slot(&mut buffer, 0))
    let tail = Slot.take(RawBuffer.slot(&mut buffer, 1))
    drop buffer
    return head.value + tail.value
  }
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 {
  return run Effect.catchAll(build(4), recover)
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from RuntimeSliceAcceptance.test.ts: exclusive slice writes reach the caller.
  {
    name: 'runtime-slice-exclusive',
    source: `struct Token {
  value: i32
}

fn replace(values: &mut [Token], index: usize) -> i32 {
  values[index] = Token {value: 42}
  return usize.toI32(values.length)
}

pub fn main() -> i32 {
  let mut values = [Token {value: 1}, Token {value: 2}]
  let length = replace(&mut values, 0)
  if length != 2 {
    return 0
  }
  return values[0].value
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from EffectRuntime.test.ts: Effect.retry gives every attempt fresh locals while the
  // exclusive capture persists, and the third attempt's count is the exit (3, not 42).
  {
    name: 'effect-retry-captures',
    source: `struct Problem { code: i32 }
effect fn retrying() -> i32 ! Problem {
  let mut counter = 0
  let work = effect {
    counter = counter + 1
    if counter < 3 { fail Problem { code: counter } }
    return counter
  }
  let retried = move work |> Effect.retry(2)
  return run retried
}
effect fn recover(problem: Problem) -> i32 { return 99 }
pub fn main() -> i32 {
  let handled = retrying() |> Effect.catchAll(recover)
  return run handled
}`,
    expected: { _tag: 'Completes', result: 3 },
  },
  // folded from EffectSuspensionComposition.test.ts: a suspended source inside Effect.retry fails
  // every attempt, and the recovery answers with the failure exit (7).
  {
    name: 'suspension-retry-failure',
    source: `struct Problem { code: i32 }
effect fn attempt() -> i32 ! Problem {
  let observed = run Effect.suspend(effect { return 1 })
  fail Problem { code: observed }
}
effect fn recover(error: Problem) -> i32 { return 7 }
pub fn main() -> i32 {
  return run Effect.catchAll(
    attempt() |> Effect.retry(2),
    recover
  )
}`,
    expected: { _tag: 'Completes', result: 7 },
  },
  {
    name: 'suspension-repeated-states',
    source: `effect fn twice() -> i32 {
  let left = run Effect.suspend(effect { return 40 })
  let right = run Effect.suspend(effect { return 2 })
  return left + right
}
pub fn main() -> i32 { return run twice() }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'stored-catch-suspension',
    source: storedCatchSuspension,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'owned-allocator-suspension-success',
    source: ownedAllocatorSuspensionSuccess,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'owned-allocator-suspension-failure',
    source: ownedAllocatorSuspensionFailure,
    expected: { _tag: 'Completes', result: 7 },
  },
  {
    name: 'audit-allocator-suspension',
    source: auditAllocatorSuspension,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from StoredCallableRuntime.test.ts: an uncalled stored callable owning a Drop guard is
  // cleaned exactly once when a typed failure exits the frame.
  {
    name: 'stored-callable-cleanup-typed-failure',
    source: `struct Guard {
  tag: i32
  storage: Allocation
}
impl Drop for Guard {
  fn drop(self: &mut Guard) -> () {
    return ()
  }
}
struct Holder<F: once fn(i32) -> i32> { step: F }
fn consume(value: i32, guard: Guard) -> i32 { return value + guard.tag }
fn keep<F: once fn(i32) -> i32>(holder: Holder<F>) -> i32 { return 42 }
effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  let guard = Guard { tag: 2, storage: move allocation }
  let holder = Holder { step: consume(move guard) }
  fail OutOfMemoryError {}
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from DropHookExecution.test.ts: a guard live at a failing run releases through its hook
  // before the typed failure propagates to the recovery (exit 7).
  {
    name: 'drop-hook-failure-propagation',
    source: `struct Guard {
  tag: i32
  storage: Allocation
}

impl Drop for Guard {
  fn drop(self: &mut Guard) -> () { return () }
}

struct ExhaustedAllocator { tag: i32 }

effect fn allocate(self: &mut ExhaustedAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  fail OutOfMemoryError {}
}

impl Allocator for ExhaustedAllocator { allocate: ExhaustedAllocator.allocate }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let mut empty = ExhaustedAllocator { tag: 0 }
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  let guard = Guard { tag: 5, storage: move allocation }
  let second = Layout.of<[i32; 2]>()
  let refused = Allocator.allocate(move second) |> Effect.provideMut(&mut empty)
  let never = run refused
  drop never
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 7 },
  },
  // folded from OpaqueRepresentationEngines.test.ts: opaque callable returns keep their hidden
  // concrete identity, so two captures of the same shape stay distinct.
  {
    name: 'opaque-callable',
    source: `fn add(left: i32, right: i32) -> i32 { return left + right }
fn make(value: i32) -> some<F: fn(i32) -> i32> F { return add(value) }
pub fn main() -> i32 {
  let first = make(40)
  let second = make(1)
  return first(1) + second(0)
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // One program covers success bypass, selected recovery, and residual propagation. Keeping the
  // complete branch matrix in the shared corpus proves native execution without a feature-local
  // compile/link test.
  {
    name: 'effect-selective-catch',
    source: `import silk.effects as Effect
struct Selected { code: i32 }
struct Residual { code: i32 }
effect fn risky(mode: i32) -> i32 ! Selected | Residual {
  if mode == 0 { return 10 }
  if mode == 1 { fail Selected { code: 10 } }
  fail Residual { code: 16 }
}
effect fn recoverSelected(problem: Selected) -> i32 { return problem.code + 4 }
effect fn recoverResidual(problem: Residual) -> i32 { return problem.code + 2 }
effect fn selective(mode: i32) -> i32 ! Residual {
  return run Effect.catch<Selected>(risky(mode), recoverSelected)
}
effect fn completed(mode: i32) -> i32 {
  return run Effect.catchAll(selective(mode), recoverResidual)
}
pub fn main() -> i32 {
  return (run completed(0)) + (run completed(1)) + (run completed(2))
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'effect-selective-catch-direct-stored',
    source: `import silk.effects as Effect
struct Selected { code: i32 }
struct Residual { code: i32 }
effect fn risky() -> i32 ! Selected | Residual { fail Selected { code: 10 } }
effect fn recoverSelected(problem: Selected) -> i32 { return problem.code + 1 }
effect fn recoverResidual(problem: Residual) -> i32 { return problem.code + 2 }
pub fn main() -> i32 {
  let selected = Intrinsic.catchFailure<Selected>(risky(), recoverSelected)
  return run Effect.catchAll(move selected, recoverResidual)
}`,
    expected: { _tag: 'Completes', result: 11 },
  },
  {
    name: 'effect-heterogeneous-failure-payload',
    source: heterogeneousFailurePayload,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'effect-heterogeneous-owned-failure-payload',
    source: heterogeneousOwnedFailurePayload,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'effect-heterogeneous-owned-failure-result-drop',
    source: heterogeneousOwnedFailureResultDrop,
    expected: { _tag: 'Completes', result: 42 },
  },
  // Affine owned service providers remain in the parent frame while a pre-read scalar suspends,
  // then release exactly once after either success or typed failure.
  {
    name: 'owned-provider-suspended-success',
    source: ownedProviderSuspendedSuccess,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'owned-provider-suspended-failure',
    source: ownedProviderSuspendedFailure,
    expected: { _tag: 'Completes', result: 7 },
  },
  // One service call site is specialized with both a synchronous and a suspendable provider. The
  // outer allocator binding must relay through the intermediate wrappers without contaminating
  // either provider-specific execution classification.
  {
    name: 'mixed-service-provider-suspension',
    source: mixedServiceProviderSuspension,
    expected: { _tag: 'Completes', result: 42 },
  },
  // The float math conformance programs join the corpus so the native differential compiles and
  // runs each one, which is the third engine behind the evaluator and direct WebAssembly.
  ...floatMathPrograms.map((program) => ({
    name: program.name,
    source: program.source,
    expected: { _tag: 'Completes', result: 42 } as const,
  })),
]

/**
 * Native-only extensions to the shared evaluator corpus. These programs retain the optimized
 * single compile/link loop without making the evaluator's pinned-outcome gate repeat large
 * feature fixtures that already have focused evaluator coverage.
 */
export const nativeCorpus: ReadonlyArray<CorpusProgram> = [
  ...corpus,
  // Static-composition runtime parity belongs in the shared native differential rather than a
  // feature-local compile/link loop. Trapping Drop variants causally prove the three cleanup exits.
  ...staticCompositionCorpus,
]

/** Invalid generic programs that must stop before target layout and MIR. */
export const invalidGenericCorpus: ReadonlyArray<InvalidCorpusProgram> = [
  {
    name: 'generic-explicit-arity',
    source:
      'fn identity<T>(value: T) -> T { return move value }\npub fn main() -> i32 { return identity<i32, bool>(42) }',
    codes: ['SEM0051'],
  },
  {
    name: 'generic-explicit-arity-past-prefix',
    source:
      'fn pair<A, B>(left: A, right: B) -> A { return move left }\npub fn main() -> i32 { return pair<i32, bool, u8>(1, true) }',
    codes: ['SEM0051'],
  },
  {
    name: 'generic-uninferred-prefix-remainder',
    source:
      'fn phantom<A, B>(value: A) -> A { return move value }\npub fn main() -> i32 { return phantom<i32>(1) }',
    codes: ['SEM0099'],
  },
  {
    name: 'generic-contradicted-prefix',
    source:
      'fn pair<A, B>(left: A, right: B) -> A { return move left }\npub fn main() -> i32 { return pair<bool>(1, true) }',
    codes: ['SEM0100'],
  },
  {
    name: 'generic-conflicting-inference',
    source:
      'fn same<T>(left: T, right: T) -> T { return move left }\npub fn main() -> i32 { return same(1, true) }',
    codes: ['SEM0052'],
  },
  {
    name: 'generic-polymorphic-recursion',
    source: `fn expand<T>(value: T) -> i32 { return expand<[T; 1]>([move value]) }
pub fn main() -> i32 { return expand<i32>(1) }`,
    codes: ['SEM0053'],
  },
]

/** Phase-owned invalid matching programs shared by diagnostics and release-gate tests. */
export const invalidMatchCorpus: ReadonlyArray<InvalidCorpusProgram> = [
  {
    name: 'match-incomplete',
    source: `struct Left {}
struct Right {}
fn inspect(input: Left | Right) -> i32 { return match &input { Left {} => 1 } }
pub fn main() -> i32 { return 0 }`,
    codes: ['SEM0044'],
  },
  {
    name: 'match-unreachable',
    source: `struct Token {}
fn inspect(input: Token) -> i32 { return match &input { _ => 0 Token {} => 1 } }
pub fn main() -> i32 { return 0 }`,
    codes: ['SEM0043'],
  },
  {
    name: 'match-invalid-member-and-field',
    source: `struct Token { value: i32 }
struct Other {}
fn inspect(input: Token) -> i32 { return match &input { Other {} => 0 Token { value, missing } => value } }
pub fn main() -> i32 { return 0 }`,
    codes: ['SEM0042', 'SEM0022'],
  },
  {
    name: 'match-invalid-guard-and-join',
    source: `struct Left {}
struct Right {}
fn inspect(input: Left | Right) -> i32 { return match &input { Left {} if 1 => 0 Left {} => 0 Right {} => false } }
pub fn main() -> i32 { return 0 }`,
    codes: ['SEM0045'],
  },
  {
    name: 'match-guard-consumes',
    source: `struct Payload {}
struct Box { value: Payload }
fn accept(value: Payload) -> bool { return true }
fn inspect(input: Box) -> i32 { return match move input { Box { value } if accept(move value) => 1 Box { value: fallback } => 0 } }
pub fn main() -> i32 { return 0 }`,
    codes: ['OWN0008'],
  },
  {
    name: 'match-borrow-escape-exclusive-immutable',
    source: `struct Payload {}
struct Box { value: Payload }
fn escape(input: Box) -> Payload { return match &input { Box { value } => value } }
fn exclusive(input: Box) -> i32 { return match &mut input { Box { .. } => 0 } }
pub fn main() -> i32 { return 0 }`,
    codes: ['OWN0006', 'OWN0007'],
  },
  {
    name: 'match-malformed-pattern',
    source: `struct Token { value: i32 }
fn inspect(input: Token) -> i32 { return match &input { Token { value: } 0 } }
pub fn main() -> i32 { return 0 }`,
    codes: ['PAR0001'],
  },
]
