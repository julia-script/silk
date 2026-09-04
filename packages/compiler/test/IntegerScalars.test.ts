import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as MirEncoding from '../src/MirEncoding.js'

const contextualCallSource = `import silk.u8 as u8
fn selectByte(
  source: &[u8],
  index: usize,
  first: u8,
  second: u8
) -> u8 {
  if source[index] == first { return second }
  return u8.add(0, 0)
}

fn identity<T>(value: T) -> T { return move value }

fn acceptByte(value: u8) -> u8 { return value }

fn isCarriageReturn(value: u8) -> bool { return value == 13 }

pub fn main() -> i32 {
  let direct = selectByte(b"//", 0, 47, 42)
  let explicit = identity<u8>(42)
  let piped = 42 |> acceptByte
  if !isCarriageReturn(13) { return 0 }
  return u8.toI32(direct) + u8.toI32(explicit) + u8.toI32(piped) - 84
}`

it.effect(
  'rejects duration range and type mismatches before HIR without duplicate recovery errors',
  () =>
    Effect.gen(function* () {
      const overflow = yield* Analysis.ofSourceRealized(
        'integer/duration-overflow',
        new TextEncoder().encode('pub fn main() -> u64 { return 18446744073709551616ns }'),
      )
      assert.deepEqual(
        Analysis.diagnostics(overflow).map((diagnostic) => diagnostic.code),
        ['SEM0204', 'SEM0170'],
      )
      assert.notInclude(Hir.encode(Analysis.rootAnalysis(overflow).hir), '18446744073709551616')

      const constantOverflow = yield* Analysis.ofSourceRealized(
        'integer/duration-constant-overflow',
        new TextEncoder().encode(
          'pub const timeout: u64 = 18446744073709551616ns pub fn main() -> i32 { return 42 }',
        ),
      )
      assert.deepEqual(
        Analysis.diagnostics(constantOverflow).map((diagnostic) => diagnostic.code),
        ['SEM0170'],
      )

      const mismatch = yield* Analysis.ofSourceRealized(
        'integer/duration-mismatch',
        new TextEncoder().encode(
          'fn accept(value: i32) -> i32 { return value } pub fn main() -> i32 { return accept(1s) }',
        ),
      )
      assert.deepEqual(
        Analysis.diagnostics(mismatch).map((diagnostic) => diagnostic.code),
        ['SEM0012'],
      )
      assert.isTrue(
        Analysis.expressionsOf(mismatch, 'integer/duration-mismatch').some(
          (expression) =>
            expression._tag === 'Duration' &&
            expression.type._tag === 'Available' &&
            expression.type.type === 'u64',
        ),
      )

      const malformed = yield* Analysis.ofSourceRealized(
        'integer/duration-malformed',
        new TextEncoder().encode('pub fn main() -> u64 { return 1h60m }'),
      )
      assert.deepEqual(
        Analysis.diagnostics(malformed).map((diagnostic) => diagnostic.code),
        ['SEM0204', 'LEX0012'],
      )
    }),
)

it.effect('rejects contextual overflow and already-typed integer mismatches before MIR', () =>
  Effect.gen(function* () {
    const overflow = yield* Analysis.ofSourceRealized(
      'integer/contextual-overflow',
      new TextEncoder().encode(
        'fn accept(value: u8) -> u8 { return value } pub fn main() -> i32 { let value = accept(256) return 42 }',
      ),
    )
    assert.isAbove(Analysis.diagnostics(overflow).length, 0)
    assert.isTrue(
      Analysis.expressionsOf(overflow, 'integer/contextual-overflow').some(
        (expression) => expression._tag === 'Integer' && expression.integer._tag === 'OutOfRange',
      ),
    )
    assert.notInclude(Hir.encode(Analysis.rootAnalysis(overflow).hir), 'literal 256')

    const mismatch = yield* Analysis.ofSourceRealized(
      'integer/contextual-mismatch',
      new TextEncoder().encode(`import silk.i32 as i32
fn accept(value: u8) -> u8 { return value }
pub fn main() -> i32 {
  let wider = i32.add(40, 2)
  let value = accept(wider)
  return 42
}`),
    )
    assert.include(
      Analysis.diagnostics(mismatch).map((diagnostic) => diagnostic.code),
      'SEM0012',
    )
  }),
)

it.effect('uses call and pipeline parameters as exact integer literal contexts', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'integer/contextual-calls',
      new TextEncoder().encode(contextualCallSource),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const contextualValues = Analysis.expressionsOf(snapshot, 'integer/contextual-calls').flatMap(
      (expression) =>
        expression._tag === 'Integer' &&
        expression.integer._tag === 'Available' &&
        expression.integer.value === 42n
          ? [expression.integer.type]
          : [],
    )
    assert.isAtLeast(contextualValues.filter((type) => type === 'u8').length, 3)
    assert.include(Hir.encode(Analysis.rootAnalysis(snapshot).hir), 'literal 42 : u8')
    assert.include(Hir.encode(Analysis.rootAnalysis(snapshot).hir), 'literal 13 : u8')
    assert.include(MirEncoding.encode(Analysis.loweredMir(snapshot)), 'literal 42 : u8')
  }),
)

it.effect('lets a declared scalar operand drive literal-first infix arithmetic', () =>
  Effect.gen(function* () {
    const id = 'integer/literal-first-infix'
    const snapshot = yield* Analysis.ofSourceRealized(
      id,
      new TextEncoder().encode(`import silk.u16 as u16
fn mixed(value: u16) -> i32 {
  let literalFirst = 5 + value
  let literalLast = value + 5
  let defaulted = 5 + 5
  return u16.toI32(literalFirst) + u16.toI32(literalLast) - defaulted
}
pub fn main() -> i32 { return mixed(21) }`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const fives = Analysis.expressionsOf(snapshot, id).flatMap((expression) =>
      expression._tag === 'Integer' &&
      expression.integer._tag === 'Available' &&
      expression.integer.value === 5n
        ? [expression.integer.type]
        : [],
    )
    assert.isAtLeast(fives.filter((type) => type === 'u16').length, 2)
    assert.include(Hir.encode(Analysis.rootAnalysis(snapshot).hir), 'literal 5 : u16')
  }),
)
