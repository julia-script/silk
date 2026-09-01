## Why

Silk can specialize generic types and target layouts, but source cannot deliberately compute a
value during compilation or use that value to select the runtime program. Adding one explicit,
bounded static-evaluation model enables target pruning and reusable compile-time validation without
introducing a macro language or adopting Zig's implicit phase blending and separate observable
compile-time memory behavior.

## What Changes

- Add explicit static functions, parameters, local bindings, conditionals, and compile errors. A static
  function executes wholly during compilation; an ordinary function may mix explicitly static
  operations with runtime parameters and residual runtime operations.
- Let literals satisfy static contexts directly, while requiring a static parameter or binding when
  compile-time availability must cross a function or local-binding boundary.
- Define static values as deterministic, identity-free, and freely reusable without granting their
  runtime types `Copy`. Static functions may use ordinary loops and mutable-binding replacement,
  but the first surface exposes no static references, in-place mutation, runtime Effects, or
  observable allocator.
- Select `static if` before semantic elaboration of either branch: both branches parse, only the
  selected branch contributes semantic and runtime program facts, and declarations cannot be
  conditional.
- Add the dedicated `compileError(message)` expression to terminate specialization with a
  source-traced compiler diagnostic and no residual runtime path. It is inherently compile-time and
  requires no `static` prefix; evaluation-limit failures remain distinct from source-requested
  compile errors.
- Expose the selected target through a closed enum-based ordinary standard-library API over the
  smallest sealed static intrinsic, then use target specialization as the acceptance case.
- Replace the syntax-only `Target.<fact>` constant-initializer exception with the same ordinary
  static-value path, while retaining explicit primitive constant contracts and zero runtime
  storage.
- Apply intrinsic availability and executable reachability only to the residual program after
  static pruning, so an inactive target-specific call contributes neither a compatibility error nor
  backend support.

## Capabilities

### New Capabilities

- `static-evaluation`: Defines static functions, mixed staged functions, static parameters and
  bindings, static conditionals and compile errors, the static value domain, residualization, evaluator
  limits, and diagnostic traces.

### Modified Capabilities

- `bootstrap-syntax`: Adds lossless, recoverable syntax for the initial static declarations,
  parameters, bindings, conditionals, and compile-error form without admitting conditional declarations.
- `bootstrap-intrinsic-boundary`: Adds only the sealed target-query primitive required for ordinary
  source to implement the public enum-based target API and static target facts.
- `bootstrap-typed-constants`: Replaces the syntax-only target-fact initializer exception with
  statically evaluated primitive initializers while retaining explicit types and prohibiting
  aggregate, inferred, runtime, or effectful initialization.
- `bootstrap-intrinsic-target-availability`: Makes the statically pruned residual executable closure
  the input to target-availability validation and backend inventory selection.
- `bootstrap-instances`: Adds static argument values to canonical instance identity and realizes a
  residual body before following its runtime call edges.
- `bootstrap-ownership`: Runs ownership and cleanup planning only over residual runtime
  specializations; static values never acquire runtime ownership or cleanup facts.

## Impact

- The lexer, parser, syntax tree, formatter, diagnostics, semantic facts, HIR presentation, and
  tooling gain explicit static constructs and phase information.
- Declaration analysis and specialization gain a deterministic static evaluator that runs before
  residual runtime ownership, reachability, MIR lowering, and backend planning.
- The intrinsic catalog gains one minimal target-query seam; the standard library gains ordinary
  target enums and wrappers rather than a compiler-known `Target` actor.
- The current target-dependent-constant special case and its selector-specific lowering path become
  obsolete and are removed in favor of the general static path.
- Evaluation, direct WebAssembly, and native LLVM continue consuming one target-neutral residual
  program and never execute static functions at runtime.
- Heterogeneous `static for`, type/field reflection, tuples, ephemeral named records, formatting,
  generic debug generation, conditional declarations, static references, and in-place static
  mutation remain follow-up work.
