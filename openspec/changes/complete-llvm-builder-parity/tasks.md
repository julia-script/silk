## 1. Build the parity ledger

- [x] 1.1 Define and document the machine-readable parity manifest schema, dispositions, fixture classes, and compatibility metadata.
- [x] 1.2 Add the pinned commit, Builder.zig, bitcode_writer.zig, and ir.zig hashes and the authoritative LLVM toolchain version.
- [x] 1.3 Implement source-inventory extraction for public operations, enum cases, intrinsic entries, block records, abbreviation records, TODOs, and panic-only paths.
- [x] 1.4 Implement manifest validation that rejects missing, duplicate, stale, or evidence-free dispositions.
- [x] 1.5 Add a human-readable parity report generated from the validated manifest.

## 2. Audit foundation and declarations

- [x] 2.1 Reconcile every bitstream primitive, operand encoding, block rule, alignment rule, and backpatch behavior with bitcode_writer.zig.
- [x] 2.2 Reconcile every data-layout field, address space, alignment, calling convention, linkage, visibility, storage, and thread-local case.
- [x] 2.3 Reconcile every type tag, type query, named type behavior, and target-extension representation.
- [x] 2.4 Reconcile every attribute kind, storage payload, attribute group, and function-attribute operation.
- [x] 2.5 Reconcile every constant tag, exact numeric encoding, constant expression, and formatting case.
- [x] 2.6 Reconcile every global, variable, alias, function declaration, naming, replacement, and property operation.
- [x] 2.7 Close each supported gap found in the declaration audit and record every intentional deviation in the manifest and documentation.

## 3. Audit functions and advanced IR

- [x] 3.1 Reconcile every function-body lifecycle, block, argument, instruction, name, value-index, and type-query behavior.
- [x] 3.2 Reconcile every unary, binary, cast, comparison, aggregate, vector, memory, control-flow, call, vararg, and assembly instruction tag.
- [x] 3.3 Reconcile every fast-math, no-wrap, exact, atomic, volatile, synchronization, alignment, tail-kind, and operand-bundle setting.
- [x] 3.4 Reconcile the complete intrinsic catalog, overload recipes, canonical attributes, and convenience calls.
- [x] 3.5 Reconcile relative value indices, signed phi indices, block numbering, constant offsets, and every function bitcode record.
- [x] 3.6 Close each supported gap found in the function audit and record every intentional deviation.

## 4. Audit metadata and text output

- [x] 4.1 Reconcile every metadata identity kind, forward-reference rule, named metadata behavior, node tag, field, and flag.
- [x] 4.2 Reconcile every global and instruction attachment, metadata kind, debug location, branch weight, and strip-mode behavior.
- [x] 4.3 Reconcile textual escaping, identifiers, module headers, types, attributes, constants, declarations, instructions, and metadata formatting.
- [x] 4.4 Reconcile every ir.zig block id, record code, abbreviation operation, field width, literal, and adapter conversion.
- [x] 4.5 Close each supported gap found in the metadata and text audit and record every intentional deviation.

## 5. Complete interoperability evidence

- [x] 5.1 Expand exact-byte Zig fixtures to cover every bitstream primitive and every construct whose ordering matches the pinned builder.
- [x] 5.2 Expand canonical semantic fixtures for modules where harmless ordering or API differences make exact byte comparison inappropriate.
- [x] 5.3 Add boundary fixtures for zero and maximum widths, multiword VBRs, bigint constants, raw floats, large blobs, forward values, and recursive metadata.
- [x] 5.4 Add malformed public-input fixtures for every actor and verify SilkError failures without partial mutation.
- [x] 5.5 Run llvm-as, llvm-dis, the verifier, and llvm-bcanalyzer across the complete corpus and persist normalized diagnostic output on failure.
- [x] 5.6 Run fresh-process determinism checks across every supported runtime and architecture available in CI.

## 6. Measure and harden performance

- [x] 6.1 Add benchmarks for interning-heavy modules, one large function, many small functions, control-flow-heavy modules, metadata-heavy modules, and large blobs.
- [x] 6.2 Measure traced and candidate untraced bitstream and instruction loops with repeated samples and recorded medians.
- [x] 6.3 Retain only materially beneficial Effect.fnUntraced or imperative hot paths and add the required measured-reason comments.
- [x] 6.4 Add memory-growth and stack-safety checks for large integer, instruction, metadata, and cyclic graph workloads.

## 7. Finalize release and update workflow

- [x] 7.1 Audit the root barrel and package.json so every public actor has one explicit namespace and one documented subpath export.
- [x] 7.2 Complete README explanations and verified examples for builder lifecycle, function bodies, bytes, bigint, targets, errors, debug modes, text, and bitcode.
- [x] 7.3 Add compatibility, known-deviation, provenance, and upstream-update documentation generated or checked against the parity manifest.
- [x] 7.4 Implement the explicit upstream candidate update command with hash verification, inventory diffing, fixture regeneration, and no automatic baseline adoption.
- [x] 7.5 Expand release-candidate validation to pack and import every root and deep API and confirm no Zig, LLVM, source, or undeclared runtime dependency ships.
- [x] 7.6 Run pnpm typecheck, pnpm exec biome check ., and pnpm test in that order and resolve all change-related failures.
- [x] 7.7 Run pnpm check and pnpm release:candidate, generate the final parity report, and record the releasable handoff.
