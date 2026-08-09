## Context

See `proposal.md` for motivation. Integer expression analysis already accepts an expected semantic type and performs exact range selection before MIR. Call argument and pipeline analysis correctly pass concrete parameter types. Operator analysis first applies an enclosing expected scalar type to both operands, then normally reruns homogeneous operands using the first operand's resolved type. It currently skips that second pass whenever any enclosing expectation was present, so `bool` from a return position leaves the literal in `byte == 13` at its unconstrained `i32` default.

The repair must keep Silk's current rules: literals are exact until a context selects them, unconstrained literals default to `i32`, generic call inference remains available, and non-literal numeric expressions never receive an implicit conversion.

## Goals / Non-Goals

**Goals:**

- Lock in the existing parameter-to-argument contextual-typing path for direct declaration calls and callable-value applications where their concrete contracts are known.
- Let a resolved first operand refine homogeneous operator operands even when the enclosing expression supplied a different result expectation.
- Preserve exact magnitude and the existing pre-MIR range diagnostic.
- Keep pipeline insertion consistent with ordinary application.
- Make the lexer example consume bytes as `u8` without an `i32` comparison boundary.

**Non-Goals:**

- Numeric promotion, coercion, overload resolution, or a new conversion syntax.
- Inferring an unresolved generic integer parameter solely from a contextual target that does not yet exist.
- Porting another compiler module to Silk or expanding the self-hosting roadmap.

## Decisions

### Keep calls on their existing resolved-contract path

Focused characterization confirms that argument syntax is already paired with its effective parameter type before expression analysis. Declaration calls use resolved parameter declarations after applying explicit type arguments. Callable-value and pipeline applications use the callable type already produced by expression analysis. A syntactic integer literal then follows the existing `analyzeInteger(expected)` path; other expressions retain their own established type.

These paths need regression coverage, not another implementation mechanism. Retyping an already-analyzed `i32` fact during contract validation remains rejected because it would duplicate diagnostics, lose exact-source information, and risk becoming implicit conversion.

### Refine homogeneous operands from the resolved first operand

Operator analysis will retain its provisional enclosing expectation because it is useful when the result and operand type coincide, such as integer arithmetic in an integer return position or Boolean equality. After that provisional pass, a resolved scalar first operand will become the expected type for all homogeneous operands even if the enclosing expectation was present. This makes `byte == 13` select `u8` for `13` while preserving `flag == true` as Boolean equality.

Discarding the enclosing expectation entirely was rejected because two otherwise-unconstrained integer literals still benefit from an integer result context. Special-casing comparison operators was rejected because equality can legitimately operate on `bool`; the decisive fact is the resolved operand actor, not the operator spelling.

### Do not guess unresolved generic contexts

If substitution leaves a type parameter rather than a concrete integer spelling, the argument remains unconstrained for literal selection and follows existing generic inference/defaulting behavior. Explicit type arguments may make that context concrete before argument analysis.

This is narrower than adding bidirectional generic inference from all argument expressions. That larger design is unnecessary for the pressure-program defect and could change call selection semantics.

### Treat pipeline insertion as ordinary first-parameter application

The pipeline input uses the callable's effective first parameter as its expected type, then the normal call contract checks the resulting fact. This preserves the pay-for-use pipeline lowering and avoids a pipeline-specific numeric rule.

### Prove behavior at semantic and execution boundaries

Regression coverage will inspect selected integer facts and pre-MIR failures, then exercise evaluator, native LLVM, and direct WebAssembly outcomes. The lexer pressure corpus will remain the real-code acceptance test, while a small focused matrix isolates contextual-typing failures.

## Risks / Trade-offs

- **[Risk] Reanalyzing operands can duplicate diagnostics or change source order.** → Replace the provisional argument result with the refined pass exactly as the existing no-enclosing-context path does; assert deterministic diagnostics and artifacts.
- **[Risk] Applying an unresolved generic parameter as an expected type can accidentally default or reject valid calls.** → Pass an expected type only when substitution yields a concrete integer spelling; retain the existing inference path otherwise.
- **[Risk] A semantic-only fix can hide a backend width mismatch.** → Assert HIR/MIR lane selection and run all three execution engines.

## Migration Plan

This is a compiler correctness repair during the unreleased bootstrap stage. Land the semantic fix and tests, simplify the lexer source, then remove the corresponding workaround finding. Rollback is the single change commit; no persisted data or package migration is required.
