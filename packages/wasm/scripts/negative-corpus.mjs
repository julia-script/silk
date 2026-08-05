/**
 * The negative corpus: one text-format module per builder validation rule, each expressing a
 * construct the builder rejects at define or emit time. `oracle.mjs` asserts the pinned
 * `wasm-tools` also rejects every encodable entry, so builder validation never accepts what the
 * specification forbids — and never rejects for reasons the specification does not.
 *
 * Builder-side rejection of each corresponding construction is asserted by the package's unit
 * tests (`test/Validation.test.ts`, `test/Declarations.test.ts`).
 */
export const negativeCorpus = [
  {
    name: 'stack-underflow',
    wat: '(module (func (result i32) i32.const 1 i32.add))',
  },
  {
    name: 'operand-type-mismatch',
    wat: '(module (func (result i32) i32.const 1 i64.const 2 i32.add))',
  },
  {
    name: 'missing-result',
    wat: '(module (func (result i32)))',
  },
  {
    name: 'extra-stack-values',
    wat: '(module (func i32.const 1))',
  },
  {
    name: 'local-out-of-range',
    wat: '(module (func (local i32) local.get 3 drop))',
  },
  {
    name: 'unreachable-still-typed',
    wat: '(module (func (result i32) unreachable i64.const 1 i32.add))',
  },
  {
    name: 'branch-depth',
    wat: '(module (func br 1))',
  },
  {
    name: 'br-table-arity',
    wat: `(module (func (param i32) (result i32)
      block (result i32)
        block
          i32.const 0
          local.get 0
          br_table 0 1
        end
        i32.const 2
      end))`,
  },
  {
    name: 'if-missing-else',
    wat: '(module (func (param i32) (result i32) local.get 0 if (result i32) i32.const 1 end))',
  },
  {
    name: 'call-argument-missing',
    wat: '(module (func $callee (param i32)) (func call $callee))',
  },
  {
    name: 'tail-call-result-mismatch',
    wat: '(module (func $callee (result i64) i64.const 1) (func (result i32) return_call $callee))',
  },
  {
    name: 'call-indirect-extern-table',
    wat: '(module (type (func)) (table 1 externref) (func i32.const 0 call_indirect (type 0)))',
  },
  {
    name: 'global-set-immutable',
    wat: '(module (global i32 (i32.const 1)) (func i32.const 2 global.set 0))',
  },
  {
    name: 'over-aligned-load',
    wat: '(module (memory 1) (func (result i32) i32.const 0 i32.load align=8 drop i32.const 0))',
  },
  {
    name: 'select-on-references',
    wat: '(module (func ref.null func ref.null func i32.const 1 select drop))',
  },
  {
    name: 'table-copy-type-mismatch',
    wat: `(module (table $a 1 funcref) (table $b 1 externref)
      (func i32.const 0 i32.const 0 i32.const 1 table.copy $a $b))`,
  },
  {
    name: 'undeclared-ref-func',
    wat: '(module (func $f) (func (export "user") ref.func $f drop))',
  },
  {
    name: 'duplicate-export',
    wat: '(module (func $f) (export "x" (func $f)) (export "x" (func $f)))',
  },
  {
    name: 'start-signature',
    wat: '(module (func $s (param i32)) (start $s))',
  },
  {
    name: 'const-expr-not-constant',
    wat: '(module (global i32 i32.const 1 i32.eqz))',
  },
  {
    name: 'const-expr-mutable-global',
    wat: '(module (global $m (mut i32) (i32.const 1)) (global i32 (global.get $m)))',
  },
  {
    name: 'const-expr-wrong-type',
    wat: '(module (global i32 (i64.const 1)))',
  },
  {
    name: 'elem-item-type-mismatch',
    wat: '(module (table 1 externref) (elem (table 0) (offset i32.const 0) externref (ref.null func)))',
  },
  {
    name: 'data-offset-type',
    wat: '(module (memory 1) (data (offset i64.const 0) "x"))',
  },
  {
    name: 'simd-lane-out-of-range',
    wat: `(module (func
      v128.const i64x2 0 0
      f32x4.extract_lane 4
      drop))`,
  },
  {
    name: 'simd-shuffle-selector-out-of-range',
    wat: `(module (func
      v128.const i64x2 0 0
      v128.const i64x2 0 0
      i8x16.shuffle 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 32
      drop))`,
  },
  {
    name: 'atomic-misaligned',
    wat: '(module (memory 1) (func i32.const 0 i32.atomic.load align=2 drop))',
  },
  {
    name: 'shared-memory-without-max',
    wat: '(module (memory 1 shared))',
  },
  {
    name: 'memory64-i32-address',
    wat: '(module (memory i64 1) (func (result i64) i32.const 0 i64.load))',
  },
  {
    name: 'table64-i32-index',
    wat: '(module (table i64 1 funcref) (func i32.const 0 table.get 0 drop))',
  },
]
