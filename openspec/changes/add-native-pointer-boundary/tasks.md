## 1. Contract and supplies

- [x] 1.1 Complete proposal, design, deltas and prescriptive reference before implementation; pin normative documents, compilers, linkers and headers and pass strict OpenSpec validation.

## 2. Qualified pointer representation

- [x] 2.1 Replace Pointer construction, parsing, formatting, type keys, substitution and serialization with every qualifier; verify nested forms, invalid qualifier spans and semantic round trips.
- [x] 2.2 Implement safe weakening and explicit unsafe qualification/null/index/unaligned primitives with ordinary source wrappers; verify nullability, access, extent, alignment and slice conversion diagnostics through shared analysis snapshots.
- [x] 2.3 Migrate MIR operations, validation and LLVM lane accesses, preserving address-space and minimum-alignment guarantees; verify IR structure and existing LLVM-to-Wasm behavior.

## 3. Storage and authoritative layout

- [x] 3.1 Add the minimal raw-slot address primitive and ordinary Copy output-state owners; verify safe pre-initialization rejection, unchanged state after foreign calls, explicit unsafe assumptions and one-time extraction under ordinary ownership.
- [x] 3.2 Make audited target facts drive primitive layout, existing external structs/arrays and scalar ABI; verify missing/inconsistent facts fail before lowering and LLVM consumes semantic results.

## 4. Conformance and migration

- [x] 4.1 Extend independent C/object fixtures and shared native corpus for both call directions, buffer writes, scalar accessor/null boundaries, nested pointers and unaligned access; require debug/optimized compile/link/inspection/execution on pinned Darwin ARM64 and GNU x86-64, plus GNU ARM64 inspection/execution on its available runner.
- [x] 4.2 Add failing-on-missing-supply designated conformance commands and explicit unsupported-LTO rejection; verify pinned supplies and no skipped conformance cases.
- [x] 4.3 Migrate every existing source pointer consumer, public export/catalog, fixture and reference; regenerate artifacts and verify no superseded pointer contract remains.

## 5. Delivery

- [x] 5.1 Run pnpm typecheck, pnpm format:check, pnpm lint, pnpm test, pnpm check and pnpm release:candidate; record exact failures and whether they predate the change.
- [x] 5.2 Commit JUL-123 above JUL-121 with gh stack, submit/update both draft PR descriptions with validation evidence and verify the final stack order.
