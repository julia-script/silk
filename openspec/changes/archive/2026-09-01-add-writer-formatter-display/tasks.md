## 1. Scalar Conformance Ownership And Validation

- [x] 1.1 Centralize source-conformance owner selection so nominal providers remain provider-owned,
      scalars become contract-owned, and other structural providers are rejected; verify declaration
      tests cover local and foreign nominal, local and foreign scalar, and structural heads.
- [x] 1.2 Resolve inline witness declarations from conformance identity and
      `conformanceImplementation` metadata instead of nominal provider shape; verify the original
      effectful `Display for i32` signature is admitted and a deliberately stronger row reports the
      precise `SEM0083` component.
- [x] 1.3 Preserve provider-actor mappings for nominal providers and sealed `Intrinsic.*` mappings,
      while rejecting an ordinary mapped scalar target with the dedicated inline-witness diagnostic;
      verify existing nominal and numeric intrinsic witness suites remain green.

## 2. Scalar Witness Selection And Execution

- [x] 2.1 Generalize ConformanceProof source-target lookup so an inline scalar mapping returns its
      canonical declaration from the conformance module; verify proof tests distinguish inline source,
      mapped nominal, and intrinsic scalar selections.
- [x] 2.2 Carry the canonical scalar target through executable-origin discovery and instance
      reachability; verify a witness with no direct ordinary caller remains in the discovered instance
      set and produces no unlowerable-witness diagnostic.
- [x] 2.3 Exercise an effectful named bound-operation call specialized at `i32`, preserving
      `WriterError` and exclusive Writer requirements through HIR, witness-effect runner construction,
      MIR, evaluation, and direct Wasm; verify one shared source snapshot proves each phase claim.
- [x] 2.4 Add the representative scalar Display program to the existing native differential corpus
      rather than a new per-feature native test; verify `DriverNativeAcceptance` agrees with evaluation
      at the ordinary corpus gate.

## 3. Referent Projection Syntax And Facts

- [x] 3.1 Add the postfix `.*` syntax node, parser projection-chain support, lossless formatting, and
      source correspondence; verify parse, round-trip, chaining, precedence, multiplication, and
      recovery cases.
- [x] 3.2 Resolve reference subjects into referent target, access, provenance, availability, span,
      and canonical place-chain facts; verify shared, exclusive, chained, and invalid-subject cases with
      diagnostic codes and spans.

## 4. Referent Ownership And Reborrowing

- [x] 4.1 Admit bare referent reads only for sealed-Copy targets, preserve the backing owner, reject
      affine extraction, and reject source `Copy` implementations for shared and exclusive references;
      verify the existing compiler-proven shared-reference Copy rule remains intact.
- [x] 4.2 Generalize call-scoped reborrowing for value-reference parameters so shared and exclusive
      child access follows compatibility, conflicting parent use is suspended, and the parent is
      restored after the call; verify repeated Formatter helper calls and strengthening conflicts.
- [x] 4.3 Support exclusive referent replacement with ordinary cleanup and loan checks; verify shared
      mutation rejection, exact-once cleanup, and post-call owner restoration.

## 5. Referent HIR, MIR, And Engines

- [x] 5.1 Represent typed referent places, Copy reads, reborrows, replacements, provenance, and spans
      explicitly in HIR and its deterministic encoding without introducing an intrinsic.
- [x] 5.2 Lower canonical referent places through MIR and extend verification and deterministic
      encoding for subject, target, access, provenance, read, reborrow, and replacement invariants.
- [x] 5.3 Execute referent reads, call-scoped reborrows, and exclusive replacements through backing
      identity in evaluation and direct Wasm; verify scalar, chained, zero-lane, and cleanup parity from
      shared Analysis snapshots.
- [x] 5.4 Add the representative referent and scalar Display programs to the existing native
      differential corpus rather than new per-feature native or fresh-process tests.

## 6. Writer-Backed Formatter API

- [x] 6.1 Define and document `Alignment`, `Sign`, `FormatOptions`, canonical defaults, and Formatter
      construction/accessors in `silk.format`; verify semantic tests observe every default and explicit
      option without an Allocator requirement.
- [x] 6.2 Define Formatter actor operations for byte emission, UTF-8 fill encoding, visible-width
      padding arithmetic, and bounded repeated-fill writes through `? &mut Writer`; verify left, right,
      centered odd-width, multibyte-fill, and arbitrarily larger-than-buffer padding cases.
- [x] 6.3 Define the effectful `Display` interface plus default and options-based generic entry
      operations; verify a user nominal Display implementation writes through two replaceable Writer
      providers with identical requested bytes and typed provider-specific failure.
- [x] 6.4 Implement color permission as a default-false option exposed to Display implementations;
      verify false suppresses option-induced ANSI SGR bytes, true permits a balanced styled user
      implementation, and styling bytes do not affect visible-width padding.
- [x] 6.5 Verify Formatter stops at the first Writer failure, preserves the original `WriterError`,
      and documents that an already accepted prefix is not rolled back or compensated.

## 7. Allocation-Free Integer Display

- [x] 7.1 Replace append-based unsigned and signed rendering with bounded reverse-digit engines and
      borrowed populated suffixes, retaining a non-positive signed magnitude; verify zero plus every
      widest and narrowest integer bound emits exact decimal bytes with no per-digit Writer loop.
- [x] 7.2 Implement sign, minimum-digit precision, zero-padding precedence, fill, and alignment over
      the bounded integer core; verify an option matrix covers positive, negative, zero, odd centered
      padding, explicit precision, and widths larger than the fill chunk.
- [x] 7.3 Declare interface-owned inline Display conformances for every signed and unsigned catalog
      integer in `silk.format`, reading scalar receivers explicitly with `self.*`; verify one generic
      `T: Display` entry selects each scalar witness and requests Writer but never Allocator.
- [x] 7.4 Preserve complete decimal parsing and its typed offsets/range failures while sharing only
      allocation-free numeric policy that remains appropriate; verify default Display bytes round-trip
      through every integer actor and existing malformed/out-of-range cases remain unchanged.

## 8. Breaking Migration And Documentation

- [x] 8.1 Update every standard-library module, repository caller, test, fixture, and example that
      consumes integer-to-String rendering to use the Writer-backed surface; verify repository search
      finds no stale calls or allocating formatting expectations.
- [x] 8.2 Delete the superseded String-returning integer APIs, allocator failure rows, integer
      rendering helpers, and obsolete imports with no alias or forwarding shim while retaining the
      general `String.appendOwned` and `String.append` operations; verify stale integer-rendering source
      gets ordinary unavailable-member diagnostics.
- [x] 8.3 Rewrite public format and integer documentation for Display, options, visible-width units,
      color permission, Writer partial-failure behavior, and preserved parsing; verify documentation
      policy, generated pages, examples, links, hover, and navigation checks pass.
- [x] 8.4 Regenerate the canonical stdlib embedding, manifest-derived source, and tracked generated
      documentation; verify the stdlib, toolchain-integrity, and documentation generation checks report
      no drift.

## 9. Repository Verification

- [x] 9.1 Run the focused syntax, correspondence, semantic-fact, ownership, reference, HIR, MIR,
      evaluation, direct-Wasm, conformance, bound-witness, formatting, integer, standard-stream,
      allocation, and native-corpus tests; verify every targeted suite passes without redundant
      fresh-process or per-feature native tests.
- [x] 9.2 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test` in the required order;
      record any failure with its exact command and whether it predates this change.
- [x] 9.3 Run `pnpm check` and verify the complete repository gate passes.
- [x] 9.4 Run `pnpm release:candidate` because shipped standard-library contents and public APIs
      change; verify package contents, exports, generated artifacts, and release validation pass.
