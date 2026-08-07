## 1. Exact literal and type foundation

- [x] 1.1 Add canonical builtin `Usize` identity, encoding, ordering, equality, substitution, traversal, Copy classification, and no-cleanup behavior.
- [x] 1.2 Retain contextual decimal literal magnitudes as exact bigint values through semantic facts and HIR while preserving existing I32 text encodings.
- [x] 1.3 Resolve `Usize` in parameter, result, field, array element, union member argument, and generic type positions without adding syntax or literal suffixes.
- [x] 1.4 Reject negative `Usize` literals, uncontextualized values outside I32, and unavailable expected types with stable phase-owned diagnostics.
- [x] 1.5 Add type, elaboration, and HIR tests for exact values above `2^53`, generic substitution, aggregate fields, damaged source, and byte-stable I32 encodings.

## 2. Usize operator semantics

- [x] 2.1 Extend operator resolution for homogeneous `Usize` addition, subtraction, multiplication, division, remainder, equality, and ordered comparison.
- [x] 2.2 Select checked unsigned arithmetic and reject mixed I32/Usize operands, unary minus, and unavailable operator paths without implicit conversion.
- [x] 2.3 Retain operand type and unsigned behavior in semantic operator facts and canonical HIR operations.
- [x] 2.4 Add focused operator tests for signed-boundary comparisons, overflow, underflow, division by zero, result types, mixed operands, and deterministic encodings.

## 3. Instance discovery and target validation

- [x] 3.1 Extend instance discovery through `Usize` signatures, aggregates, unions, arrays, and operations without including literal magnitude or target width in instance keys.
- [x] 3.2 Add deterministic target-owned validation for every reachable exact `Usize` literal after target selection and before MIR lowering.
- [x] 3.3 Publish explicit available or unavailable literal verdicts and diagnostics for 32-bit and 64-bit target bounds without truncation.
- [x] 3.4 Add tests proving one generic `Usize` instance across magnitudes, native acceptance above 32 bits, Wasm rejection, and unrelated-instance stability.

## 4. Target layout and heterogeneous scalar lanes

- [x] 4.1 Add `UnsignedInteger { bits: 32 | 64 }` layout facts for `Usize`, using size/alignment eight on native and four on Wasm.
- [x] 4.2 Generalize calling-lane consumers to resolve each builtin scalar's physical type from its compiler-planned layout rather than a global I32 width.
- [x] 4.3 Extend aggregate, repeated, union, slice-adjacent, parameter, and result shapes for mixed I32, Bool, Usize, and Address lanes without changing canonical ordering.
- [x] 4.4 Construct Usize/i64 layout and backend types lazily so programs with no reachable `Usize` preserve existing layout, LLVM, and Wasm bytes.
- [x] 4.5 Extend layout verification and deterministic text/encodings for unsigned width, scalar lanes, target literal verdicts, and malformed mixed shapes.
- [x] 4.6 Add target-layout tests for all four profiles, nested padded aggregates, unions, arrays, calls/returns, out-of-range literals, and non-Usize byte stability.

## 5. Structured MIR and verification

- [x] 5.1 Add monomorphic `Usize` MIR type and exact literal payloads while preserving the structured control DAG and canonical provenance.
- [x] 5.2 Lower accepted unsigned arithmetic and comparisons with selected target word facts and explicit trap semantics.
- [x] 5.3 Generalize MIR locals, moves, calls, aggregates, unions, reads/writes, and returns for heterogeneous scalar widths.
- [x] 5.4 Verify exact literal range, operand/result types, selected word width, unsigned operation semantics, calling-lane agreement, and required traps.
- [x] 5.5 Extend MIR traversal, instance substitution, deterministic encoding/text, samples, and malformed fixtures for Usize and mixed-width values.

## 6. Logical evaluation

- [x] 6.1 Add exact bigint-backed `UsizeValue` and target-derived maximum while keeping I32 convenience values and aggregate storage unchanged.
- [x] 6.2 Evaluate checked unsigned arithmetic, underflow, division/remainder by zero, equality, and ordering without host-number precision loss.
- [x] 6.3 Preserve Usize values through calls, returns, arrays, structs, unions, match, mutation, and loops with canonical unsigned decimal traces.
- [x] 6.4 Add evaluator tests at zero, signed maximum boundaries, Wasm maximum, above `2^53`, native maximum, every trap, and recursive/nested calls.

## 7. Native LLVM lowering

- [x] 7.1 Resolve LLVM lane and result types from the compiler layout, creating i64 only for reachable native Usize values and removing the one-lane-i32 assumption.
- [x] 7.2 Emit exact unsigned i64 constants, moves, parameters, calls, returns, aggregate/union lanes, loads/stores, and comparisons.
- [x] 7.3 Emit checked unsigned add/subtract/multiply overflow paths plus unsigned divide/remainder and preserve existing signed I32 behavior.
- [x] 7.4 Add native IR and execution fixtures for mixed-width signatures, padded aggregates, values above `2^53`, native maximum comparison, traps, and bitcode determinism.
- [x] 7.5 Assert byte-identical existing non-Usize LLVM IR/bitcode goldens and lazy type numbering.

## 8. Direct Wasm lowering

- [x] 8.1 Map compiler-planned Wasm Usize lanes to i32 across locals, parameters, calls, returns, aggregates, unions, and memory operations.
- [x] 8.2 Emit unsigned i32 comparison, division, and remainder instructions plus explicit add/subtract/multiply overflow or underflow checks.
- [x] 8.3 Normalize exported logical Usize results to canonical unsigned bigint values without changing raw Wasm i32 bits or Bool/I32 results.
- [x] 8.4 Add Wasm text/byte and execution fixtures around `2^31`, `2^32 - 1`, every trap, nested calls, mixed aggregates, and deterministic artifacts.
- [x] 8.5 Assert byte-identical existing non-Usize Wasm text/binary goldens.

## 9. Acceptance, inspector, and repository gates

- [x] 9.1 Add one shared-range fixture exercising Usize arithmetic and comparisons across evaluator, native, and Wasm with identical logical results.
- [x] 9.2 Add one native-only exactness fixture above `2^53` with a real i64 call/return and in-program observable comparison; require Wasm rejection before MIR.
- [x] 9.3 Extend fresh-process determinism coverage across semantic values, operators, HIR, instances, target verdicts, layout, MIR, evaluation, LLVM, and Wasm artifacts.
- [x] 9.4 Add the canonical Usize preset to unified `/labs` with coordinated native/Wasm width, literal verdict, lane, stopped-path, and execution projections.
- [x] 9.5 Update the project roadmap and scoped-allocation prerequisite references from `add-pointer-sized-integers` to the shipped `add-usize-scalar` change.
- [x] 9.6 Run focused compiler and workbench tests throughout implementation and resolve every in-scope failure.
- [x] 9.7 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`; report exact provenance for any failure.
- [x] 9.8 Run strict OpenSpec validation and inspect the final diff for Isize/fixed-width creep, implicit conversions, literal suffixes, slice-length migration, eager i64 construction, backend-owned widths, allocation, or unrelated changes.
