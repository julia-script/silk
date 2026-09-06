## 1. Source and semantic contract

- [x] 1.1 Publish the prescriptive source clause, defaults, validity, complete-call loans, identity and forbidden-unwind outcome before code changes; validate all planning artifacts.
- [x] 1.2 Implement clause parsing, normalized contract data and completion diagnostics; verify defaults, ordering equivalence, invalid properties/types and imported origins with shared analysis snapshots.
- [x] 1.3 Admit only explicitly call-borrowed single-value references and preserve initialized-state, overlapping loan and end-of-call behavior; verify raw output pointers gain no implicit initialization or ownership.

## 2. Native contracts and unwind enforcement

- [x] 2.1 Carry behavior through semantic surfaces, executable/MIR records, codecs, ABI manifests and caches; verify visible mismatches and equivalent records, including supplied cross-unit interfaces.
- [x] 2.2 Emit precisely asserted LLVM memory, capture, alias and no-return properties while retaining conservative reloads and ordinary error-state loads; verify structural output and updated goldens.
- [x] 2.3 Add typed invoke/landingpad builder, verifier, IR and bitcode support and generate the fatal unwinder guard for all admitted immediate foreign calls, including outbound calls from exported callbacks; verify valid LLVM serialization and actual object unwind structure.

## 3. Independent evidence and integration

- [x] 3.1 Pin authority bytes and native supplies; run distinguishing renamed operation/accessor, native write/alias and enclosing-C++-catch throw fixtures on all three native targets in debug and optimized modes; fail missing supplies and record exact results.
- [x] 3.2 Migrate declarations, consumers, diagnostics, interface fixtures, docs and generated artifacts; remove obsolete type-only function behavior records and audit unsupported unwind/retention/LTO requests.
- [x] 3.3 Run typecheck, format:check, lint, test, check and release:candidate, record failures and baseline status.

- [ ] 3.4 Submit the JUL-124 branch using gh stack.
