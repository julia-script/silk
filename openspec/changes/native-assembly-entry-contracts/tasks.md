## 1. Contract and authorities

- [ ] 1.1 Publish proposal/design/spec/reference before code; record exact prior art and authority/header/tool pins.
- [ ] 1.2 Validate OpenSpec and enumerate the JUL-136 consumers and deliberate breadth exclusions.

## 2. Source and native implementation

- [ ] 2.1 Add typed assembly metadata validation, intrinsic inventory and structured diagnostics.
- [ ] 2.2 Add constrained native function properties and reject incompatible source bodies/profiles.
- [ ] 2.3 Preserve assembly contracts through HIR/MIR, no-return control flow and semantic identities.
- [ ] 2.4 Reuse LLVM assembly construction and naked attributes, with direct naked export symbols.

## 3. Evidence and integration

- [ ] 3.1 Add cheap analysis/MIR/LLVM assertions for accepted and rejected contracts.
- [ ] 3.2 Run pinned x86-64/ARM64 debug/optimized object/disassembly and independent machine fixtures.
- [ ] 3.3 Update reference, inventories, generated docs, examples and public package contents together.
- [ ] 3.4 Run typecheck, format:check, lint, test, check and release:candidate; record exact results.
- [ ] 3.5 Submit the JUL-135 branch through gh stack without merging.
