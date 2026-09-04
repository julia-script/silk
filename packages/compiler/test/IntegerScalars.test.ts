import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'

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
