## 1. Foreign host contract

- [x] 1.1 Add the public `ForeignHost` actor with canonical signature classes, immutable table construction, lookup, tagged invocation results, and the versioned Wasm module name; verify focused type tests exercise table isolation and signature identity.
- [x] 1.2 Export the actor through the compiler barrel and package subpath; verify `pnpm release:candidate` accepts the published surface.

## 2. Evaluator binding and execution

- [x] 2.1 Replace blanket evaluator foreign unavailability with pre-execution table admission for every reachable symbol; verify focused evaluator tests cover exact, missing, mismatched, and per-run bindings with an empty trace on admission failure.
- [x] 2.2 Execute `ForeignCall` operations through admitted bindings and validate returned values; verify focused tests cover scalar success, void success, typed host failure, and invalid host result without untyped exceptions.
- [x] 2.3 Replace the obsolete evaluator blocked reason and update labs/inspector formatting and spans; verify focused presentation assertions name the symbol, signatures, message, and call provenance.

## 3. Direct WebAssembly imports

- [x] 3.1 Make foreign availability consumer-specific so direct Wasm is admitted while LLVM wasm32 remains rejected; verify the existing availability suite distinguishes all three surfaces.
- [x] 3.2 Declare canonical direct-Wasm imports and lower `ForeignCall` arguments/results through their handles; verify focused Wasm execution covers integer, float, pointer-width, and void mappings.
- [x] 3.3 Populate direct-Wasm `foreignImports` and `hostImports` metadata deterministically; verify agreeing redeclarations deduplicate and equivalent input order produces identical WAT and bytes.

## 4. Specification and documentation

- [x] 4.1 Update the foreign-functions reference, SEM0193 documentation, and evaluator/Wasm host examples; verify docs build and diagnostic catalog generation remain clean.
- [x] 4.2 Sync and archive the OpenSpec deltas after implementation; verify `openspec validate --strict` passes before and after archive.

## 5. Repository verification and handoff

- [x] 5.1 Run `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, and `pnpm release:candidate` in repository order and record exact outcomes.
- [x] 5.2 Obtain independent correctness and mandatory test-economics approval for the exact committed issue diff, then create and read back the stacked draft PR and Linear handoff evidence.
