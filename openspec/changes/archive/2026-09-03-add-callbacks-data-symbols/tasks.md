## 1. Syntax and semantic models

- [x] 1.1 Add `extern "C" fn(P...) -> R` syntax, formatting, and type facts; verify parser and formatter cases cover canonical output and unsupported/missing ABI recovery.
- [x] 1.2 Add foreign/exported static declaration syntax and facts; verify parser, formatter, declaration-index, and module-surface cases preserve ABI, local/native names, type, and initializer boundaries.
- [x] 1.3 Add C-ABI callback/static admission and diagnostics; verify focused semantic tests reject mismatched, ordinary, generic, suspending, capturing, mutable-binding, and non-C-layout forms by diagnostic code and span.

## 2. Executable facts and MIR

- [x] 2.1 Add contextual export-function-to-C-callback conversion and executable discovery facts; verify only exact reachable export thunks enter the callback inventory.
- [x] 2.2 Add foreign-static load and exported-static root facts to HIR/MIR; verify MIR structure, encoding, and verification cover exact types, symbols, reachability, and deterministic order.
- [x] 2.3 Extend foreign availability by operation kind; verify evaluator/direct Wasm reject reachable callbacks/statics with empty/no-artifact outcomes while ignoring unreachable declarations.

## 3. Native lowering and acceptance

- [x] 3.1 Lower C callback values to export-thunk addresses and pass them through foreign calls; verify a native corpus program sorts through `qsort` with a Silk comparator.
- [x] 3.2 Lower foreign static reads to external-global loads; verify a native corpus program reads the host `environ` symbol.
- [x] 3.3 Lower exported scalar statics to C-visible global definitions and artifact metadata; verify separately compiled C reads `silk_abi_version == 1` from a produced library.

## 4. Documentation and specification

- [x] 4.1 Update reference documentation and the diagnostic catalog for callbacks, data symbols, mutability, and non-native boundaries; verify documentation generation, policy, and examples pass.
- [x] 4.2 Sync and archive the OpenSpec deltas after implementation; verify strict OpenSpec validation passes before and after archive.

## 5. Repository verification and handoff

- [x] 5.1 Run `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, and `pnpm release:candidate` in repository order and record exact outcomes.
- [x] 5.2 Obtain independent correctness and mandatory test-economics approval for the exact committed issue diff, then create and read back the stacked draft PR and Linear handoff evidence.
