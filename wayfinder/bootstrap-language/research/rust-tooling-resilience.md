# Rust tooling over incomplete and invalid programs

Research date: 2026-08-03. Sources are official rust-analyzer documentation and source, the Rust
Compiler Development Guide, and official nightly compiler API documentation.

## Finding

Rust does not get resilient editing support simply by exposing every `rustc` pass. The robust
editing model is primarily rust-analyzer: a separate, long-lived analysis database whose syntax and
semantic products are designed for incomplete code. `rustc` also recovers locally and uses error
sentinels to limit cascades, but it retains compilation-oriented abort points and does not preserve
a lossless syntax tree.

## Syntax and parser recovery

- rust-analyzer's syntax tree is deliberately lossless: comments and whitespace are nodes, and
  converting source to a tree and back is an identity even for invalid input. Missing grammatical
  children remain absent, unexpected input becomes an `ERROR` node, and typed AST accessors return
  `Option`. Its parser contract is data plus diagnostics, `(T, Vec<Error>)`, rather than
  `Result<T, Error>`. [Syntax design](https://rust-analyzer.github.io/book/contributing/syntax.html#design-goals),
  [architecture](https://rust-analyzer.github.io/book/contributing/architecture.html#crates-parser)
- The lossless syntax layer is independent of Salsa, HIR, and LSP and is itself an API boundary.
  This lets purely syntactic tools work without a buildable project. Semantic state is kept out of
  syntax nodes so refactors can transform value-like trees. [rust-analyzer syntax boundary](https://rust-analyzer.github.io/book/contributing/architecture.html#crates-syntax)
- `rustc` has substantial parser recovery, but it is targeted rather than a total-tree contract.
  Its `recover_stmt` documentation calls the heuristic best-effort, and recovery can be disabled in
  contexts such as macro arguments. The AST has recovery placeholders such as `ExprKind::Err`, so a
  syntax error need not stop all later parsing. [rustc parser recovery](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_parse/parser/struct.Parser.html#method.recover_stmt),
  [`ExprKind::Err`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_ast/ast/enum.ExprKind.html#variant.Err)
- `rustc`'s parser input is not lossless. The compiler lexer explicitly skips non-documentation
  comments and whitespace while cooking tokens; documentation comments are retained as language
  syntax. [rustc lexer source](https://doc.rust-lang.org/nightly/nightly-rustc/src/rustc_parse/lexer/mod.rs.html#188-228)

## Semantic recovery and diagnostics

- rust-analyzer treats broken code as ordinary input: core analysis commonly computes a value and a
  collection of errors rather than failing the whole request. [Error-handling invariant](https://rust-analyzer.github.io/book/contributing/architecture.html#error-handling)
- Type inference produces an `InferenceResult` even when parts are unresolved. It contains
  expression, pattern, and binding types alongside a diagnostics vector, an error flag, and an
  interned error type. Final write-back replaces unresolved type variables with the error type and
  emits a `TypeMustBeKnown` diagnostic, allowing unrelated inferred facts to remain usable.
  [Current `InferenceResult` source](https://github.com/rust-lang/rust-analyzer/blob/5f258f4534e3b4bdaa45a1299b53a66cf014d803/crates/hir-ty/src/infer.rs#L755-L797),
  [current inference recovery source](https://github.com/rust-lang/rust-analyzer/blob/5f258f4534e3b4bdaa45a1299b53a66cf014d803/crates/hir-ty/src/infer/unify.rs#L610-L668)
- Recovery has an explicit frontier. rust-analyzer MIR lowering can return `IncompleteExpr`,
  `IncompletePattern`, `UnresolvedName`, `HasErrors`, and related failures. Error types preserve HIR
  queries around damaged code; they do not promise a fabricated valid MIR for every body.
  [`MirLowerError`](https://rust-lang.github.io/rust-analyzer/hir_ty/mir/enum.MirLowerError.html)
- `rustc` follows the same local anti-cascade principle. `TyKind::Error` propagates after a primary
  diagnostic so dependent errors can be suppressed, and it can only be constructed with proof that
  an error was already emitted. Name resolution and the AST likewise have `Res::Err` and recovery
  nodes. [`rustc` error types](https://rustc-dev-guide.rust-lang.org/ty.html#type-errors),
  [`ErrorGuaranteed`](https://rustc-dev-guide.rust-lang.org/diagnostics/error-guaranteed.html),
  [`Res::Err`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_hir/def/enum.Res.html#variant.Err)
- Neither system blindly accumulates every possible downstream diagnostic. rust-analyzer filters
  diagnostics whose receiver already contains an error type, while `rustc` exposes
  `abort_if_errors` for boundaries where continuing would produce spurious or uninteresting errors.
  The distinction is controlled recovery, not “never abort.”
  [rust-analyzer cascade suppression](https://github.com/rust-lang/rust-analyzer/blob/5f258f4534e3b4bdaa45a1299b53a66cf014d803/crates/hir-ty/src/infer/unify.rs#L570-L600),
  [`rustc` diagnostic context](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_errors/struct.DiagCtxtHandle.html#method.abort_if_errors)

## Query boundaries and external consumers

- rust-analyzer keeps source files and a crate graph as client-provided ground state, derives the
  semantic model lazily, and invalidates only affected computations. `ItemTree` isolates
  declaration-level facts from body edits, and a body change must not invalidate facts about other
  functions. [rust-analyzer database architecture](https://rust-analyzer.github.io/book/contributing/architecture.html#bird-s-eye-view),
  [HIR architecture](https://rust-analyzer.github.io/book/contributing/architecture.html#crates-hir-expand-crates-hir-def-crates-hir_ty)
- Raw `hir-*` crates are explicitly not API boundaries. The `hir` crate is the semantic facade;
  `ide` is the editor-oriented boundary. `AnalysisHost` accepts changes and `Analysis` provides a
  cancellable snapshot with queries such as parse, diagnostics, hover, completion, references, and
  rename. The LSP server converts those results into protocol types rather than serializing internal
  HIR directly. [API boundaries](https://rust-analyzer.github.io/book/contributing/architecture.html#crates-hir),
  [`Analysis`](https://rust-lang.github.io/rust-analyzer/ide/struct.Analysis.html)
- The stable external contract is mainly LSP. rust-analyzer documents its in-process `ide` API as
  explicitly unstable, even though it is intentionally shaped for embedding in other tools. This is
  better than exposing raw internals, but it is not a semver-stable semantic SDK.
  [Stability guarantees](https://rust-analyzer.github.io/book/contributing/architecture.html#stability-guarantees)
- `rustc` also uses memoized, demand-driven queries through `TyCtxt`, but these organize a compiler
  invocation rather than a persistent editor snapshot. `rustc_interface` is documented as an
  unstable wrapper around the query system, and external `rustc_private` drivers require compiler
  development components. [rustc queries](https://rustc-dev-guide.rust-lang.org/query.html),
  [compiler source architecture](https://rustc-dev-guide.rust-lang.org/compiler-src.html),
  [external drivers](https://rustc-dev-guide.rust-lang.org/rustc-driver/external-rustc-drivers.html)
- `rustc_public` is an active attempt to offer third-party access to types, MIR, monomorphized
  instances, and ABI details. As of the research date it is not published and remains subject to
  breaking changes; its queries also run inside a compiler-session callback. It is therefore not
  yet a substitute for rust-analyzer's incomplete-code editing model.
  [`rustc_public` status](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_public/index.html#status)

## Relevant limitations

- rust-analyzer reimplements significant Rust semantics instead of reusing all of `rustc`.
  Its own testing guide acknowledges that type mismatch analysis can miss errors or report bogus
  ones. Resilience and responsiveness therefore come with a semantic-parity burden.
  [rust-analyzer testing guide](https://rust-analyzer.github.io/book/contributing/testing.html)
- rust-analyzer does not expose every intermediate representation as a stable public contract. The
  raw incremental HIR layers are deliberately internal, and MIR can be unavailable for incomplete
  bodies. The lesson is to expose supported questions and durable facades, not freeze every storage
  structure.
- `rustc` can recover far enough to report multiple useful errors, but phase barriers may abort once
  additional results would be unreliable. Its AST is span-rich but not trivia-preserving, and its
  internal query APIs are not a stable foundation for third-party live tooling.

## Design lessons for Silk Effect

1. Make every tooling-relevant phase return a **partial snapshot plus diagnostics**, not a single
   success/failure gate. A syntax error in one body must not erase declarations, references, types,
   contracts, or ownership facts that remain knowable elsewhere.
2. Keep the lossless `SyntaxFile` as the durable source product. Preserve every byte, missing
   children, unexpected-token nodes, stable source IDs, and byte spans. Do not make comment
   retention a parser mode.
3. Give HIR explicit `Missing`, `Unresolved`, and `Error` sentinels with provenance to the primary
   diagnostic. Analyses must propagate them and suppress dependent cascades without treating them as
   valid executable semantics.
4. Define a recovery frontier per query. Parsing, declaration collection, local resolution, and
   best-effort type/contract/ownership queries should survive damage. MIR and code generation may be
   unavailable **per body or instance**, while healthy bodies remain queryable.
5. Separate internal representations from supported tooling APIs. Expose immutable analysis
   snapshots and stable queries over source IDs, declaration IDs, spans, types, references,
   diagnostics, and source maps. Do not require outside tools to fork the compiler or depend on raw
   HIR layout.
6. Keep batch compilation as one client of the same analysis components. Its policy may reject any
   snapshot containing errors before MIR emission or linking; that policy must not be embedded in
   parsing, resolution, type checking, contract checking, or ownership analysis.
7. Test malformed programs at every phase boundary. Fixtures should assert both the primary
   diagnostic and the useful facts still available before, inside, and after the damaged region.
   Semantic parity tests must compare the bootstrap compiler, the self-hosted compiler, and tooling
   answers so resilience does not create a second language implementation.
