## Context

See `proposal.md` for motivation. The compiler's scalar path is currently intentionally uniform:
`Type.Builtin` is `I32 | Bool`, HIR and MIR literals use JavaScript `number`, every ordinary calling
lane becomes LLVM `i32` or Wasm `i32`, and evaluator scalar values store a number. Target layout
already knows pointer width and internal address lanes already vary between native 64-bit pointers
and Wasm 32-bit offsets, so the target-aware authority exists but ordinary scalar consumers still
assume one width.

The target is selected after target-independent semantic analysis and before runtime layout planning
and MIR lowering. A `Usize` literal can therefore have a canonical source type and exact magnitude
before the compiler knows whether its valid range ends at 32 or 64 bits.

## Goals / Non-Goals

**Goals:**

- Add one honest target-width source scalar without generalizing every bootstrap integer at once.
- Preserve exact 64-bit unsigned values in every compiler representation and inspection encoding.
- Make scalar calling lanes heterogeneous by compiler-selected representation, extending the address-
  lane work without disturbing programs that do not reach `Usize`.
- Give the allocation proposal permanent checked size/count arithmetic.

**Non-Goals:**

- Add `Isize`, fixed-width unsigned or wider scalar execution, integer conversion actors, shifts,
  bitwise operations, wrapping/saturating operations, or literal suffixes.
- Change fixed-array natural-number parameters or the runtime-slice `I32` length in this ticket.
- Expose pointer values or promise an external ABI.

## Decisions

### 1. Add only `Usize` and keep its canonical type target-independent

`Usize` is one canonical builtin type on every target. Width is a property of the selected target
layout, not part of type identity or generic instance keys. One generic `identity<Usize>` instance is
therefore shared across target-specific compilation plans and across literal magnitudes.

Adding `Usize` and `Isize` together was rejected because allocation needs unsigned size arithmetic
only; signed pointer differences have no demonstrated workload. Reusing the internal `Address`
scalar was rejected because addresses are not source integers and have different safety semantics.

### 2. Retain exact integer magnitude as `bigint` from elaboration onward

Decimal source text is parsed to `bigint` after its contextual type is known. HIR and MIR literal
payloads become exact integer magnitudes; I32 values are range-validated and converted to host
numbers only at APIs that specifically require I32. Deterministic text uses ordinary base-ten
digits, so existing I32 encodings remain byte-identical despite the internal payload change.

Using JavaScript `number` with a 64-bit-range guard was rejected because values above `2^53 - 1`
would already be rounded before validation. Carrying raw digit strings through MIR was rejected
because arithmetic, equality, and backend constants require a canonical numeric value.

### 3. Validate target range between target selection and MIR lowering

Elaboration publishes a typed exact `Usize` literal independent of target. Target planning adds one
deterministic literal-validation table or equivalent phase-owned fact for every reachable literal.
The plan accepts `0..2^width-1` and retains unavailable entries with diagnostics for larger values.
MIR lowering consumes only validated values and never truncates.

Making elaboration target-dependent was rejected because it would duplicate otherwise identical
syntax, resolution, and semantic snapshots per target. Deferring validation to a backend was
rejected because stopped paths and diagnostics belong to the compiler before MIR emission.

### 4. Generalize scalar lanes by planned representation, not a global width

Layout gains `UnsignedInteger { bits: 32 | 64 }` for `Usize`. Its native entry is size/alignment
eight; its Wasm entry is size/alignment four. `CallingLane` retains the canonical source scalar, and
consumers resolve its physical type through that scalar's selected layout entry. LLVM creates `i64`
only when a reachable lane or operation needs it; Wasm continues using `i32` for `Usize` on the
current `wasm32` target.

This removes the native backend's single-global-`i32` assumption and its hard-coded one-lane i32
result path. Aggregates and unions may therefore contain mixed I32, Bool, Usize, and Address lanes
without changing canonical field or payload ordering.

Representing native `Usize` as an address lane was rejected because integers are not pointers.
Eagerly creating i64 types for every program was rejected because prior slice work proved that
unreachable type construction can perturb deterministic bitcode type numbering.

### 5. Extend existing operators with a signedness-aware scalar plan

Operator facts remain the canonical arithmetic identities already used by HIR and MIR, while the
operand type selects signed checked I32 or unsigned checked Usize semantics. LLVM uses unsigned
overflow intrinsics for add/subtract/multiply and unsigned division, remainder, and comparisons.
Wasm emits explicit unsigned overflow/underflow checks around wrapping `i32` arithmetic, plus
`div_u`, `rem_u`, and unsigned comparisons. Division by zero remains a trap; unsigned division has
no signed-minimum edge case.

Mixed I32/Usize operands and unary minus on Usize remain semantic errors. Implicit promotion was
rejected because Wayfinder requires existing numeric values never convert implicitly.

### 6. Make the evaluator and execution adapters expose canonical unsigned values

Evaluator `UsizeValue` stores `bigint` and applies a target-derived maximum. Comparisons and checked
arithmetic operate directly on bigint and trace values encode as unsigned decimal text. Existing
I32 values remain numbers at public convenience boundaries, with exact integer helpers separating
the two cases.

Wasm's JavaScript boundary normalizes returned `i32` bits with `BigInt(value >>> 0)` when the logical
result is Usize. Native acceptance for values above 32 bits uses a real i64 function signature and
an in-program exact comparison that returns the existing process-level success scalar; it does not
pretend an OS exit status carries 64 bits.

### 7. Prove both shared-range parity and native-only exactness

The shared fixture uses values around `2^31`, exercises all arithmetic and comparisons without
exceeding 32 bits, and must agree across evaluator, native, and Wasm. The native-only fixture uses
values above `2^53` and `2^32`, returns and passes Usize through ordinary functions, compares the
exact result, and proves native/evaluator agreement. Targeting that exact source at Wasm stops after
target literal validation.

Focused negative fixtures cover target overflow, addition/multiplication overflow, subtraction
underflow, division by zero, mixed operands, unary minus, malformed MIR widths, and signed-vs-
unsigned comparison traps. `/labs` exposes the canonical fixture through the unified inspector.

## Risks / Trade-offs

- [Risk] Bigint payload changes perturb existing text or binary artifacts. → Preserve decimal text,
  construct i64 types lazily, and assert byte-identical non-Usize LLVM and Wasm goldens.
- [Risk] A remaining one-lane-i32 assumption truncates native results or aggregate fields. → Drive
  every lane type from the compiler layout and add mixed aggregate/call/return fixtures.
- [Risk] Wasm host APIs expose unsigned i32 values as negative JavaScript numbers. → Normalize only
  at the logical Usize result boundary and retain raw bits inside Wasm.
- [Risk] Target-dependent literal rejection leaks into semantic type identity. → Keep the type and
  exact magnitude target-independent and publish range availability only after target selection.
- [Trade-off] Slice length remains I32 temporarily. → Keep that debt explicit; migration can be a
  later breaking change over an already-proven Usize runtime path.

## Migration Plan

Implement one vertical scalar capability: exact literals and type identity; operator facts; HIR and
instances; target range validation and layouts; MIR and verifier; evaluator; native mixed lanes and
unsigned operations; Wasm unsigned operations and result normalization; acceptance and `/labs`.
Retain lazy type construction so existing allocation-free and Usize-free artifacts stay stable.

If three-engine parity fails, remove the source type and all `UnsignedInteger` representation paths
together. Existing I32/Bool, internal addresses, slice layouts, and artifacts remain the rollback
boundary.
