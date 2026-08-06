/**
 * The shared evaluation corpus: programs with pinned expected outcomes. The MIR interpreter's
 * tests consume it today; the native acceptance differential reuses it to compare interpreter
 * results against compiled output. Expected results were pinned against the fact-based
 * evaluator before the MIR retarget.
 */

export interface CorpusProgram {
  readonly name: string
  readonly source: string
  readonly expected:
    | { readonly _tag: 'Completes'; readonly result: number }
    | { readonly _tag: 'Trap' }
    | { readonly _tag: 'RecursiveCycle'; readonly cycle: ReadonlyArray<string> }
    | { readonly _tag: 'UnavailableEntry'; readonly reason: string }
}

export interface InvalidCorpusProgram {
  readonly name: string
  readonly source: string
  readonly codes: ReadonlyArray<string>
}

export const corpus: ReadonlyArray<CorpusProgram> = [
  {
    name: 'literal',
    source: 'pub fn main() -> I32 { return 42 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'identity',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(42) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'second-parameter',
    source: `pub fn second(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return second(10, 42) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'nested',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'nested-siblings',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return choose(identity(1), identity(2)) }`,
    expected: { _tag: 'Completes', result: 2 },
  },
  {
    name: 'forward-call',
    source: `pub fn main() -> I32 { return answer() }
pub fn answer() -> I32 { return 42 }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'direct-recursion',
    source: 'pub fn main() -> I32 { return main() }',
    expected: { _tag: 'RecursiveCycle', cycle: ['main', 'main'] },
  },
  {
    name: 'mutual-recursion',
    source: `pub fn main() -> I32 { return other() }
pub fn other() -> I32 { return main() }`,
    expected: { _tag: 'RecursiveCycle', cycle: ['main', 'other', 'main'] },
  },
  {
    name: 'unknown-call-trap',
    source: 'pub fn main() -> I32 { return missing() }',
    expected: { _tag: 'Trap' },
  },
  {
    name: 'inner-blocked-trap',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return choose(identity(1), missing(2)) }`,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'binding',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { let value = identity(42) return value }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'binding-chain',
    source: `pub fn main() -> I32 { let first = 40 let second = 2 return first }`,
    expected: { _tag: 'Completes', result: 40 },
  },
  {
    name: 'moved-binding',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { let value = 42 return identity(move value) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'use-after-move-trap',
    source: `pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { let value = 42 return choose(move value, value) }`,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'arithmetic',
    source: 'pub fn main() -> I32 { return I32.subtract(I32.multiply(6, 7), 0) }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'operator-precedence',
    source: 'pub fn main() -> I32 { return 2 + 5 * 8 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'operator-pipeline',
    source: 'pub fn main() -> I32 { return 2 |> I32.add(40) }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'closed-operator-surface',
    source: `pub fn main() -> I32 {
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
    source: 'pub fn main() -> I32 { if true |> Bool.not { return 0 } return 42 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'signed-truncation',
    source: 'pub fn main() -> I32 { return I32.add(I32.divide(-7, 2), 45) }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'remainder-sign',
    source: 'pub fn main() -> I32 { return I32.add(I32.remainder(-7, 2), 43) }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'overflow-trap',
    source: 'pub fn main() -> I32 { return I32.add(2147483647, 1) }',
    expected: { _tag: 'Trap' },
  },
  {
    name: 'operator-negation-overflow-trap',
    source: 'pub fn main() -> I32 { return -(-2147483648) }',
    expected: { _tag: 'Trap' },
  },
  {
    name: 'divide-by-zero-trap',
    source: 'pub fn main() -> I32 { return I32.divide(1, 0) }',
    expected: { _tag: 'Trap' },
  },
  {
    name: 'minimum-division-trap',
    source: 'pub fn main() -> I32 { return I32.divide(-2147483648, -1) }',
    expected: { _tag: 'Trap' },
  },
  {
    name: 'branch-taken',
    source: 'pub fn main() -> I32 { if I32.equals(1, 1) { return 42 } return 0 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'branch-otherwise',
    source: 'pub fn main() -> I32 { if I32.equals(1, 2) { return 0 } return 42 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'branch-else',
    source:
      'pub fn main() -> I32 { if I32.lessThan(2, 1) { return 1 } else { return 42 } return 0 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'bool-not',
    source: 'pub fn main() -> I32 { if Bool.not(I32.equals(1, 2)) { return 42 } return 0 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'operator-bool-not',
    source: 'pub fn main() -> I32 { if !(1 == 2) { return 42 } return 0 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'bool-through-function',
    source: `pub fn check(flag: Bool) -> I32 { if flag { return 42 } return 0 }
pub fn main() -> I32 { return check(I32.greaterOrEqual(3, 3)) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'arm-binding',
    source:
      'pub fn main() -> I32 { let base = 40 if I32.equals(base, 40) { let bonus = 2 return I32.add(base, bonus) } return 0 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-inferred',
    source: 'pub fn main() -> I32 { let values = [10, 42] return values[1] }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-contextual-empty',
    source: `fn empty() -> [I32; 0] { return [] }
fn consume(values: [I32; 0]) -> I32 { return 42 }
pub fn main() -> I32 { return consume(empty()) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-nested',
    source: `fn choose(values: [[I32; 2]; 2], outer: I32, inner: I32) -> I32 { return values[outer][inner] }
pub fn main() -> I32 { return choose([[10, 11], [42, 43]], 1, 0) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-indexed-struct-field',
    source: `struct Pair { left: I32 right: I32 }
fn choose(values: [Pair; 2], index: I32) -> I32 { return values[index].left }
pub fn main() -> I32 { return choose([Pair { left: 10, right: 11 }, Pair { left: 42, right: 43 }], 1) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-whole-move',
    source: `struct Token { value: I32 }
pub fn main() -> I32 {
  let tokens = [Token { value: 10 }, Token { value: 42 }]
  let moved = move tokens
  return moved[1].value
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-negative-index-trap',
    source: `fn choose(values: [I32; 2], index: I32) -> I32 { return values[index] }
pub fn main() -> I32 { return choose([10, 42], -1) }`,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'array-upper-index-trap',
    source: `fn choose(values: [I32; 2], index: I32) -> I32 { return values[index] }
pub fn main() -> I32 { return choose([10, 42], 2) }`,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'array-zero-index-trap',
    source: `fn choose(values: [I32; 0], index: I32) -> I32 { return values[index] }
pub fn main() -> I32 { return choose([], 0) }`,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'mutable-scalar-loop',
    source: `pub fn main() -> I32 {
  let mut count = 0
  while count < 42 { count = count + 1 }
  return count
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'mutable-array-loop',
    source: `pub fn main() -> I32 {
  let mut values = [40, 0]
  let mut index = 0
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
    source: `pub fn main() -> I32 {
  let mut index = 0
  while index < 50 {
    index = index + 1
    if index == 2 { continue }
    if index == 42 { break }
  }
  return index
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'mutable-struct-loop',
    source: `struct Pair { left: I32 right: I32 }
pub fn main() -> I32 {
  let mut pair = Pair { left: 0, right: 40 }
  while pair.left < 2 { pair.left = pair.left + 1 }
  return pair.left + pair.right
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'match-guarded-union-shared',
    source: `struct Left { value: I32 }
struct Right { value: I32 }
fn inspect(input: Left | Right) -> I32 {
  return match &input {
    Left { value } if false => 0
    Left { value: answer } => answer + 1
    Right { value } => value
  }
}
pub fn main() -> I32 { return inspect(Left { value: 41 }) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'match-move-nested-cleanup',
    source: `struct Token { value: I32 }
struct Box { token: Token discarded: Token }
pub fn main() -> I32 {
  let box = Box { token: Token { value: 42 }, discarded: Token { value: 0 } }
  return match move box {
    Box { token: Token { value }, .. } => value
  }
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'match-universal-fallback',
    source: `struct Left { value: I32 }
struct Right { value: I32 }
fn inspect(input: Left | Right) -> I32 {
  return match &input { Left { value } => value _ => 42 }
}
pub fn main() -> I32 { return inspect(Right { value: 0 }) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'match-exclusive-mutable',
    source: `struct Token { value: I32 }
pub fn main() -> I32 {
  let mut token = Token { value: 42 }
  return match &mut token { Token { value } => value }
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'nested-loops',
    source: `pub fn main() -> I32 {
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
    source: 'pub fn answer() -> I32 { return 42 }',
    expected: { _tag: 'UnavailableEntry', reason: 'MissingEntry' },
  },
  {
    name: 'parameterized-entry',
    source: 'pub fn main(value: I32) -> I32 { return value }',
    expected: { _tag: 'UnavailableEntry', reason: 'ParameterizedEntry' },
  },
]

/** Phase-owned invalid matching programs shared by diagnostics and release-gate tests. */
export const invalidMatchCorpus: ReadonlyArray<InvalidCorpusProgram> = [
  {
    name: 'match-incomplete',
    source: `struct Left {}
struct Right {}
fn inspect(input: Left | Right) -> I32 { return match &input { Left {} => 1 } }
pub fn main() -> I32 { return 0 }`,
    codes: ['SEM0044'],
  },
  {
    name: 'match-unreachable',
    source: `struct Token {}
fn inspect(input: Token) -> I32 { return match &input { _ => 0 Token {} => 1 } }
pub fn main() -> I32 { return 0 }`,
    codes: ['SEM0043'],
  },
  {
    name: 'match-invalid-member-and-field',
    source: `struct Token { value: I32 }
struct Other {}
fn inspect(input: Token) -> I32 { return match &input { Other {} => 0 Token { value, missing } => value } }
pub fn main() -> I32 { return 0 }`,
    codes: ['SEM0042', 'SEM0022'],
  },
  {
    name: 'match-invalid-guard-and-join',
    source: `struct Left {}
struct Right {}
fn inspect(input: Left | Right) -> I32 { return match &input { Left {} if 1 => 0 Left {} => 0 Right {} => false } }
pub fn main() -> I32 { return 0 }`,
    codes: ['SEM0045', 'SEM0049'],
  },
  {
    name: 'match-guard-consumes',
    source: `struct Payload {}
struct Box { value: Payload }
fn accept(value: Payload) -> Bool { return true }
fn inspect(input: Box) -> I32 { return match move input { Box { value } if accept(move value) => 1 Box { value: fallback } => 0 } }
pub fn main() -> I32 { return 0 }`,
    codes: ['OWN0008'],
  },
  {
    name: 'match-borrow-escape-exclusive-immutable',
    source: `struct Payload {}
struct Box { value: Payload }
fn escape(input: Box) -> Payload { return match &input { Box { value } => value } }
fn exclusive(input: Box) -> I32 { return match &mut input { Box { .. } => 0 } }
pub fn main() -> I32 { return 0 }`,
    codes: ['OWN0006', 'OWN0007'],
  },
  {
    name: 'match-malformed-pattern',
    source: `struct Token { value: I32 }
fn inspect(input: Token) -> I32 { return match &input { Token { value: } 0 } }
pub fn main() -> I32 { return 0 }`,
    codes: ['PAR0001'],
  },
]
