## 1. Artifact and Link Domain

- [x] 1.1 Add public `ArtifactKind` and `NativeLinkInput` actors with closed constructors, validation, compatibility, filenames, and canonical encoding; verify their focused unit tests pass.
- [x] 1.2 Replace manifest `native-libraries` decoding with artifact and inline-table native-link-input decoding, resolving safe paths relative to the manifest; verify valid and invalid manifest tests pass.

## 2. Library Reachability and LLVM Surface

- [x] 2.1 Thread artifact root policy through realization and instance discovery, add an explicit MIR library entry, and root native libraries at canonical C exports without `main`; verify focused instance, lowering, and driver no-entry tests pass.
- [x] 2.2 Emit compiler implementations and generated helpers with non-public LLVM visibility while leaving C export thunks default-visible; verify LLVM IR/bitcode tests distinguish internal implementations from public thunks.
- [x] 2.3 Split process-entry and runtime-only C source generation and compile runtime support with hidden visibility; verify shim planning and source tests pass for executable and library modes.

## 3. Native Finalization

- [x] 3.1 Extend pure tool planning for executable/shared Clang links, deterministic `llvm-ar rcsD` archives, ordered typed input translation, and typed unsupported combinations; verify ToolchainPlan unit tests pass.
- [x] 3.2 Extend the Effect toolchain boundary, cache identity, atomic commits, and final artifact data for all native kinds; verify NativeToolchain tests cover success, cache separation, missing paths, unsupported inputs, and failures.
- [x] 3.3 Update Driver orchestration and public exports to carry artifact kind and structured link inputs end to end; verify Driver unit and native acceptance tests pass.

## 4. Project Workflow and Documentation

- [x] 4.1 Update CLI batch/build/run planning for artifact compatibility and platform filenames, deleting old native-library arrays; verify BuildPlan, BuildBatch, Workflow, and command tests pass.
- [x] 4.2 Add a native acceptance case that builds a shared library, inspects its exported symbols, and calls it from a separately compiled C consumer; add deterministic static-archive coverage and verify the focused native suite passes.
- [x] 4.3 Update manifest examples and the CLI/reference documentation for artifact and native-link-input syntax; verify documentation checks and searches find no superseded `native-libraries` contract.

## 5. Verification and Archive

- [x] 5.1 Run `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`, recording any exact pre-existing failure.
- [x] 5.2 Run `openspec validate add-native-library-artifacts --strict`, archive the completed change, and verify the main specs contain the merged requirements with no active change directory.
