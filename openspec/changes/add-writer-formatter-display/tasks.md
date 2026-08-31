## 1. Scalar Conformance Ownership And Validation

- [ ] 1.1 Centralize source-conformance owner selection so nominal providers remain provider-owned,
  scalars become contract-owned, and other structural providers are rejected; verify declaration
  tests cover local and foreign nominal, local and foreign scalar, and structural heads.
- [ ] 1.2 Resolve inline witness declarations from conformance identity and
  `conformanceImplementation` metadata instead of nominal provider shape; verify the original
  effectful `Display for i32` signature is admitted and a deliberately stronger row reports the
  precise `SEM0083` component.
- [ ] 1.3 Preserve provider-actor mappings for nominal providers and sealed `Intrinsic.*` mappings,
  while rejecting an ordinary mapped scalar target with the dedicated inline-witness diagnostic;
  verify existing nominal and numeric intrinsic witness suites remain green.

## 2. Scalar Witness Selection And Execution

- [ ] 2.1 Generalize ConformanceProof source-target lookup so an inline scalar mapping returns its
  canonical declaration from the conformance module; verify proof tests distinguish inline source,
  mapped nominal, and intrinsic scalar selections.
- [ ] 2.2 Carry the canonical scalar target through executable-origin discovery and instance
  reachability; verify a witness with no direct ordinary caller remains in the discovered instance
  set and produces no unlowerable-witness diagnostic.
- [ ] 2.3 Exercise an effectful named bound-operation call specialized at `i32`, preserving
  `WriterError` and exclusive Writer requirements through HIR, witness-effect runner construction,
  MIR, evaluation, and direct Wasm; verify one shared source snapshot proves each phase claim.
- [ ] 2.4 Add the representative scalar Display program to the existing native differential corpus
  rather than a new per-feature native test; verify `DriverNativeAcceptance` agrees with evaluation
  at the ordinary corpus gate.

## 3. Writer-Backed Formatter API

- [ ] 3.1 Define and document `Alignment`, `Sign`, `FormatOptions`, canonical defaults, and Formatter
  construction/accessors in `silk.format`; verify semantic tests observe every default and explicit
  option without an Allocator requirement.
- [ ] 3.2 Define Formatter actor operations for byte emission, UTF-8 fill encoding, visible-width
  padding arithmetic, and bounded repeated-fill writes through `? &mut Writer`; verify left, right,
  centered odd-width, multibyte-fill, and arbitrarily larger-than-buffer padding cases.
- [ ] 3.3 Define the effectful `Display` interface plus default and options-based generic entry
  operations; verify a user nominal Display implementation writes through two replaceable Writer
  providers with identical requested bytes and typed provider-specific failure.
- [ ] 3.4 Implement color permission as a default-false option exposed to Display implementations;
  verify false suppresses option-induced ANSI SGR bytes, true permits a balanced styled user
  implementation, and styling bytes do not affect visible-width padding.
- [ ] 3.5 Verify Formatter stops at the first Writer failure, preserves the original `WriterError`,
  and documents that an already accepted prefix is not rolled back or compensated.

## 4. Allocation-Free Integer Display

- [ ] 4.1 Replace append-based unsigned and signed rendering with bounded reverse-digit engines and
  borrowed populated suffixes, retaining a non-positive signed magnitude; verify zero plus every
  widest and narrowest integer bound emits exact decimal bytes with no per-digit Writer loop.
- [ ] 4.2 Implement sign, minimum-digit precision, zero-padding precedence, fill, and alignment over
  the bounded integer core; verify an option matrix covers positive, negative, zero, odd centered
  padding, explicit precision, and widths larger than the fill chunk.
- [ ] 4.3 Declare interface-owned inline Display conformances for every signed and unsigned catalog
  integer in `silk.format`; verify one generic `T: Display` entry selects each scalar witness and
  requests Writer but never Allocator.
- [ ] 4.4 Preserve complete decimal parsing and its typed offsets/range failures while sharing only
  allocation-free numeric policy that remains appropriate; verify default Display bytes round-trip
  through every integer actor and existing malformed/out-of-range cases remain unchanged.

## 5. Breaking Migration And Documentation

- [ ] 5.1 Update every standard-library module, repository caller, test, fixture, and example that
  consumes integer-to-String rendering to use the Writer-backed surface; verify repository search
  finds no stale calls or allocating formatting expectations.
- [ ] 5.2 Delete the superseded String-returning integer APIs, allocator failure rows, append-based
  rendering helpers, and obsolete imports with no alias or forwarding shim; verify stale source gets
  ordinary unavailable-member diagnostics.
- [ ] 5.3 Rewrite public format and integer documentation for Display, options, visible-width units,
  color permission, Writer partial-failure behavior, and preserved parsing; verify documentation
  policy, generated pages, examples, links, hover, and navigation checks pass.
- [ ] 5.4 Regenerate the canonical stdlib embedding, manifest-derived source, and tracked generated
  documentation; verify the stdlib, toolchain-integrity, and documentation generation checks report
  no drift.

## 6. Repository Verification

- [ ] 6.1 Run the focused conformance, bound-witness, formatting, integer, standard-stream, allocation,
  and native-corpus tests; verify every targeted suite passes without adding redundant fresh-process
  or per-feature native tests.
- [ ] 6.2 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test` in the required order;
  record any failure with its exact command and whether it predates this change.
- [ ] 6.3 Run `pnpm check` and verify the complete repository gate passes.
- [ ] 6.4 Run `pnpm release:candidate` because shipped standard-library contents and public APIs
  change; verify package contents, exports, generated artifacts, and release validation pass.
