## Context

See `proposal.md` for motivation. Integer expression analysis already accepts an expected semantic type and performs exact range selection before MIR. Call argument analysis also has an expected-type input, but the lexer pressure program demonstrated that the concrete declaration contract is not reliably preserved for ordinary multi-argument calls. Pipeline analysis independently obtains the first callable parameter as its input context.

The repair must keep Silk's current rules: literals are exact until a context selects them, unconstrained literals default to `i32`, generic call inference remains available, and non-literal numeric expressions never receive an implicit conversion.

## Goals / Non-Goals

**Goals:**

- Establish one parameter-to-argument contextual-typing path shared by direct declaration calls and callable-value applications where their concrete contracts are known.
- Preserve exact magnitude and the existing pre-MIR range diagnostic.
- Keep pipeline insertion consistent with ordinary application.
- Make the lexer example consume bytes as `u8` without an `i32` comparison boundary.

**Non-Goals:**

- Numeric promotion, coercion, overload resolution, or a new conversion syntax.
- Inferring an unresolved generic integer parameter solely from a contextual target that does not yet exist.
- Porting another compiler module to Silk or expanding the self-hosting roadmap.

## Decisions

### Analyze arguments against the resolved callable contract

Argument syntax will be paired with its effective parameter type before expression analysis. Declaration calls use resolved parameter declarations after applying explicit type arguments. Callable-value and pipeline applications use the callable type already produced by expression analysis. A syntactic integer literal then follows the existing `analyzeInteger(expected)` path; other expressions retain their own established type.

This keeps contextual typing at the analysis boundary where exact literal facts are created. Retyping an already-analyzed `i32` fact during contract validation was rejected because it would duplicate diagnostics, lose exact-source information, and risk becoming implicit conversion.

### Do not guess unresolved generic contexts

If substitution leaves a type parameter rather than a concrete integer spelling, the argument remains unconstrained for literal selection and follows existing generic inference/defaulting behavior. Explicit type arguments may make that context concrete before argument analysis.

This is narrower than adding bidirectional generic inference from all argument expressions. That larger design is unnecessary for the pressure-program defect and could change call selection semantics.

### Treat pipeline insertion as ordinary first-parameter application

The pipeline input uses the callable's effective first parameter as its expected type, then the normal call contract checks the resulting fact. This preserves the pay-for-use pipeline lowering and avoids a pipeline-specific numeric rule.

### Prove behavior at semantic and execution boundaries

Regression coverage will inspect selected integer facts and pre-MIR failures, then exercise evaluator, native LLVM, and direct WebAssembly outcomes. The lexer pressure corpus will remain the real-code acceptance test, while a small focused matrix isolates contextual-typing failures.

## Risks / Trade-offs

- **[Risk] Section/partial-call parameter alignment can shift the expected ordinal.** → Derive the same effective parameter slice once and use it for both argument analysis and contract mapping; cover ordinary calls, sections, and pipeline insertion separately.
- **[Risk] Applying an unresolved generic parameter as an expected type can accidentally default or reject valid calls.** → Pass an expected type only when substitution yields a concrete integer spelling; retain the existing inference path otherwise.
- **[Risk] A semantic-only fix can hide a backend width mismatch.** → Assert HIR/MIR lane selection and run all three execution engines.

## Migration Plan

This is a compiler correctness repair during the unreleased bootstrap stage. Land the semantic fix and tests, simplify the lexer source, then remove the corresponding workaround finding. Rollback is the single change commit; no persisted data or package migration is required.
