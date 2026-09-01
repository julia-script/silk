## 1. Add static iteration syntax and declaration facts

- [x] 1.1 Reserve the `for` form following `static`, add lossless syntax nodes for `static for <binding> in <expression> { ... }`, and verify lexer, parser, and concrete-tree tests retain every token, trivia slice, and span.
- [x] 1.2 Add bounded recovery for missing static-iteration bindings, `in`, iterable expressions, braces, and bodies while rejecting declaration-list use; verify damaged-form fixtures preserve following statements and declarations.
- [x] 1.3 Extend the syntax formatter and syntax inspectors for static iteration and verify parse-format-parse idempotence plus deterministic inspection encodings.
- [x] 1.4 Add semantic template facts for static iterables and per-iteration scopes without publishing runtime facts, and verify declaration and module-surface snapshots remain target-neutral and deterministic.

## 2. Model canonical reflection and static sequences

- [x] 2.1 Extend `StaticValue` with canonical type descriptors, owner-and-value-typed field descriptors, heterogeneous field sequences, and homogeneous static sequences; verify admission, stable encoding, presentation, equality, nominal distinction, and retained-byte accounting.
- [x] 2.2 Derive ordered aggregate descriptors from concrete named tuples, anonymous positional aggregates, anonymous records, and visible named fields; verify generic substitution, tuple ordinals, anonymous labels, canonical order, and private-field filtering with semantic tests.
- [x] 2.3 Admit ordinary typed member projection from static aggregate values without publishing runtime projection, ownership, or cleanup facts; verify nominal field identity, generic substitution, nested pure values, invalid members, and the absence of static union matching.
- [x] 2.4 Implement immutable static-sequence empty, append, concatenate, length, and indexed-read evaluation with complete binding replacement; verify nested pure aggregate elements, reuse, bounds failures, budget boundaries, and rejection from runtime values.
- [x] 2.5 Add sealed phase-only `Type<Owner>`, `Fields<Owner>`, `Field<Owner, Value>`, and `StaticSequence<Element>` nominals plus the minimal metadata and sequence intrinsic contracts; verify their generic arities, lack of runtime targets, and phase violations in residual signatures, bindings, or calls.
- [x] 2.6 Add a mixed shared-projection intrinsic contract whose owner-reference lane remains runtime and whose required descriptor lane is consumed during specialization; verify the catalog, calling-shape verifier, and residualizer reject a surviving descriptor lane or intrinsic call.
- [x] 2.7 Add canonical ordinary `silk.reflect` and `silk.static_sequence` source actors over the intrinsic seam, update the standard-library manifest, and verify navigation reaches source declarations while equivalent user wrappers receive the same semantics.

## 3. Residualize heterogeneous static iteration

- [x] 3.1 Evaluate finite static iterables and atomically re-elaborate one fresh body scope per canonical element with its concrete binding type; verify homogeneous sequences, heterogeneous fields, nested iteration, zero elements, lexical shadowing, deterministic identities, and rollback of all generated HIR, instance, and ownership facts when any later iteration fails at the `Analysis.evaluate` tier.
- [x] 3.2 Reject runtime, unbounded, effectful, service-dependent, unsafe, host-dependent, and otherwise inadmissible iterables before body elaboration; verify each failure uses its owning static diagnostic and publishes no partial residual body.
- [x] 3.3 Implement the shared field-projection bridge from `&Owner` plus static `Field<Owner, Value>` to ordinary residual `&Value`; verify wrong-owner, inaccessible-field, runtime-descriptor, owned-projection, and escaping-borrow cases are rejected.
- [x] 3.4 Publish only generated ordinary HIR with loop and descriptor provenance, and verify HIR goldens contain typed projections and calls but no static-for, descriptor, sequence, iterator, reflection lookup, or intrinsic projection node.
- [x] 3.5 Extend residual specialization and instance discovery so each generated field operation selects its concrete evidence and call edges; verify heterogeneous `Display` applications deduplicate equal keys and keep unequal owner, template, visibility, and static inputs distinct.
- [x] 3.6 Run ownership and cleanup only over generated runtime bindings and shared projections; verify borrowed anonymous temporaries remain live through every generated operation while descriptors, sequences, template plans, and inactive iterations create no ownership or cleanup facts.
- [x] 3.7 Charge iteration evaluation and generated operations against existing step, call-depth, retained-value, and residual-growth limits while expansion is in progress; verify boundary failures retain iteration traces and produce no partial executable closure.

## 4. Build compile-time template formatting in ordinary source

- [x] 4.1 Define one flat homogeneous static `Part` struct with an enum mode and ordinary projected fields, then implement UTF-8 parsing with `silk.static_text` plus immutable sequence replacement for literal segments, `{{`, `}}`, `{}`, and `{name}`; verify parsing covers valid, malformed, escaped, empty, and multibyte templates without static union matching.
- [x] 4.2 Implement general static-text range composition so slices retain their authored template expression plus transformed UTF-8 byte start and end, and make ordinary `compileError` preserve that range without recognizing formatting APIs; verify nested slicing, static bindings, helper calls, and non-literal static parameters.
- [x] 4.3 Implement static validation for separate positional and named modes, tuple arity, aggregate kind, visible field lookup, repeated named fields, unused named fields, and mixed-mode rejection; verify failures reach `compileError` with the exact transformed template byte provenance and no runtime body.
- [x] 4.4 Add ordinary-source `Display<string>` through the existing Writer/string UTF-8 path and verify direct display, Writer failure prefix preservation, and absence of an intermediate owned String or second text-writing route.
- [x] 4.5 Add `Format.format<Args>(static template: string, args: &Args) -> () ! WriterError ? &mut Writer` as ordinary `silk.format` source that statically iterates template parts and reflected fields; verify static template lanes and descriptors are absent from its runtime calling shape.
- [x] 4.6 Residualize literal parts to existing Writer text operations and placeholders to shared projections plus independently selected `Display` operations; verify operation order, Writer requirement and failure rows, atomic specialization failure, prefix preservation, and absence of intermediate String allocation.
- [x] 4.7 Verify direct calls with `&("Julia", 31)`, `&.{ name: "Julia", age: 31 }`, and reusable `&args` locals preserve hidden-owner and ordinary shared-borrow behavior without consuming or copying the argument pack.
- [x] 4.8 Verify uncalled invalid formatting applications remain unevaluated, missing `Display` evidence retains the ordinary interface diagnostic, and inaccessible named fields are neither selectable nor leaked through candidate diagnostics.

## 5. Complete semantic, engine, and documentation coverage

- [x] 5.1 Add deterministic semantic encodings that connect authored static loops, canonical elements, template segments, selected fields, and generated residual operations; verify repeated realizations produce byte-identical facts and diagnostics.
- [x] 5.2 Add one shared semantic acceptance corpus for positional tuple formatting, named anonymous-record formatting, visible named-struct fields, escaped braces, repeated fields, and representative failures, using one analysis snapshot per source program.
- [x] 5.3 Add formatting programs to the global differential acceptance corpus and targeted Wasm structure coverage only where reflection erasure or effect lowering is backend-relevant; verify evaluator, direct Wasm, and native agree without per-feature native duplicate tests.
- [x] 5.4 Update the prescriptive language reference for static iteration, descriptors, visibility, static sequences, projection, template grammar, borrowed argument packs, diagnostics, and runtime erasure; verify every documentation example parses under the implemented grammar.
- [x] 5.5 Regenerate the diagnostic catalog, intrinsic inventory, standard-library manifest/embeddings, toolchain integrity, and affected semantic goldens; verify generated-content checks find no stale file or compiler-known `silk.reflect`, `silk.static_sequence`, or `silk.format` spelling.

## 6. Run repository and release gates

- [x] 6.1 Run `pnpm typecheck`, `pnpm exec biome check .`, and focused compiler/standard-library tests in that order; report any failure with the exact command and whether it predates the change.
- [ ] 6.2 Run `pnpm test` and `pnpm check`, verify `git diff --check`, and confirm no debug instrumentation, runtime reflection representation, compatibility parser, variadic fallback, or duplicate template path remains.
- [ ] 6.3 Run `pnpm release:candidate` because compiler and standard-library package contents change, and report the exact result before implementation handoff.
