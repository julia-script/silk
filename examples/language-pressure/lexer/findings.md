# Lexer pressure findings

The categories are intentionally stable so later pressure programs can be compared without turning
their local annoyances into language doctrine. Evidence and dispositions are completed alongside
the acceptance gates.

| Category | Finding | Evidence | Disposition |
| --- | --- | --- | --- |
| Language | Token identity wants a closed named-value vocabulary, but Silk has neither enums nor constants. Encoding 67 kinds as local `u8` values is correct yet loses names inside Silk. | `main.silk` (`Token.kind`, `keywordKind`, and punctuation classifiers) plus the exhaustive mapping in `LexerPressure.test.ts` | Deferred. Do not design enums from this example alone; compare with another program that needs a closed value vocabulary. |
| Standard library | `Vector.get` requires `&mut Vector<T>` even for Copy reads, and nested vector fields cannot be projected through `&mut Lexed`. Observation therefore consumes `Lexed`, destructures it, and binds each vector as a mutable local. | `fingerprint`, `fingerprintVectors`, and the `silk.vector` public `get` signature | Defer a read-only vector/view proposal until another consumer confirms the needed borrow surface. The workaround is explicit and has no hidden runtime cost. |
| Compiler defect | LLVM's private cleanup/conditional inliner removed blocks using structural incoming counts even when an outcome-lowered branch still referenced them, producing `Backend branch to missing block`. | The valid and invalid native gates in `LexerPressure.test.ts`; fixed in `Backend.ts` by retaining rewritten references and the entry | Repaired here as a general CFG correctness fix; evaluator, LLVM, and Wasm now agree. |
| Compiler defect | Concrete call and pipeline parameters already select exact integer literal types, but a `bool` result expectation suppressed homogeneous operand refinement in expressions such as `return byte == 13`, leaving `13` as `i32` instead of `u8`. | Focused direct-call, explicit-generic, pipeline, return-comparison, overflow, and mismatch cases in `IntegerScalars.test.ts`; the lexer now compares bytes and literals as `u8` without a classifier-wide conversion boundary | Repaired by allowing a resolved first scalar operand to refine the remaining operands even when the enclosing expression supplied a different result expectation. Evaluator, LLVM, Wasm, and fresh-process lexer artifacts agree. |
| Tooling / ergonomics | Without named constants, the numeric token representation makes the Silk source harder to navigate than the TypeScript `TokenKind` union even though stdlib go-to-definition works. | Side-by-side `main.silk`, `packages/compiler/src/Token.ts`, and the centralized TypeScript mapping | Accepted locally for this exercise; revisit with the language finding rather than adding lexer-specific aliases or compiler magic. |
| Performance / cost | Keyword classification performs a visible linear sequence of at most 25 byte-slice comparisons, while vector growth moves initialized records element by element. The cost is pay-for-use and no scheduler/runtime abstraction appears, but it is not yet optimized. | `keywordKind`, `silk.vector`, general-operation MIR assertion, four allocation ordinals, and fresh-process artifacts | Accept for bootstrap correctness. Benchmark before proposing tables, bulk moves, or another primitive; none is justified by this corpus alone. |

The contextual-literal follow-up is complete and also corrected the initial diagnosis: calls were
already sound; enclosing operator-result context was the live defect. This does not schedule a
parser port. Enum/constant design and vector read-only access still need evidence from another
pressure program before they become proposals.
