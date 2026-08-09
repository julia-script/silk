# Lexer pressure findings

The categories are intentionally stable so later pressure programs can be compared without turning
their local annoyances into language doctrine. Evidence and dispositions are completed alongside
the acceptance gates.

| Category | Finding | Evidence | Disposition |
| --- | --- | --- | --- |
| Language | Token identity wants a closed named-value vocabulary, but Silk has neither enums nor constants. Encoding 67 kinds as local `u8` values is correct yet loses names inside Silk. | `main.silk` (`Token.kind`, `keywordKind`, and punctuation classifiers) plus the exhaustive mapping in `LexerPressure.test.ts` | Deferred. Do not design enums from this example alone; compare with another program that needs a closed value vocabulary. |
| Standard library | `Vector.get` requires `&mut Vector<T>` even for Copy reads, and nested vector fields cannot be projected through `&mut Lexed`. Observation therefore consumes `Lexed`, destructures it, and binds each vector as a mutable local. | `fingerprint`, `fingerprintVectors`, and the `silk.vector` public `get` signature | Defer a read-only vector/view proposal until another consumer confirms the needed borrow surface. The workaround is explicit and has no hidden runtime cost. |
| Compiler defect | LLVM's private cleanup/conditional inliner removed blocks using structural incoming counts even when an outcome-lowered branch still referenced them, producing `Backend branch to missing block`. | The valid and invalid native gates in `LexerPressure.test.ts`; fixed in `Backend.ts` by retaining rewritten references and the entry | Repaired here as a general CFG correctness fix; evaluator, LLVM, and Wasm now agree. |
| Compiler defect | Exact integer literal context reaches intrinsic calls and many result positions but not ordinary user-function arguments such as `hasPair(source, index, 47, 47)`, which reports `SEM0012` instead of selecting `u8`. | Initial pressure diagnostics; `main.silk` currently converts compared bytes to `i32` at the classifier boundary | Defer to a focused contextual-literal proposal. The workaround is readable but repetitive and the existing integer spec already requires immediate contexts to select a representable type. |
| Tooling / ergonomics | Without named constants, the numeric token representation makes the Silk source harder to navigate than the TypeScript `TokenKind` union even though stdlib go-to-definition works. | Side-by-side `main.silk`, `packages/compiler/src/Token.ts`, and the centralized TypeScript mapping | Accepted locally for this exercise; revisit with the language finding rather than adding lexer-specific aliases or compiler magic. |
| Performance / cost | Keyword classification performs a visible linear sequence of at most 25 byte-slice comparisons, while vector growth moves initialized records element by element. The cost is pay-for-use and no scheduler/runtime abstraction appears, but it is not yet optimized. | `keywordKind`, `silk.vector`, general-operation MIR assertion, four allocation ordinals, and fresh-process artifacts | Accept for bootstrap correctness. Benchmark before proposing tables, bulk moves, or another primitive; none is justified by this corpus alone. |

The immediate high-leverage follow-up is ordinary-call contextual integer literals because it is a
small, already-specified compiler defect that affected many classifier calls. This does not schedule
a parser port; enum/constant design and vector read-only access need evidence from another pressure
program before they become proposals.
