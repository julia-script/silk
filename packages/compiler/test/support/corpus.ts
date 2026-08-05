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
