## 1. Positional sections

- [x] 1.1 Give `finishCallableSection`, `analyzeSectionContract`, and `sectionCallableType` explicit captured ordinals (default trailing), compute remaining parameters as the uncaptured ordinals, and allow zero remaining parameters. Register `FieldProjectionExpression` nodes in `executableSites` after call sites.
- [x] 1.2 Add `Mir.applyOperands` and use it in `BootstrapEvaluation` (`invokeStoredCallable`, local-shared callbacks, `ApplyCallable`), `WasmBackend`, `NativeCallOperation`, and `NativeExecutionOperation.applyCallable`. `MirNormalization.parametersFor` already fills holes by ordinal and is unchanged.

## 2. Bound method values

- [x] 2.1 In `analyzeProjection`, resolve an inherent receiver method into a receiver section through `synthesizeReceiver`; report `SEM0198` for `NoReceiver`; reject a temporary receiver of a reference parameter with the borrow-operand diagnostic. Delete `receiverMethodOf` and `SEM0199`.
- [x] 2.2 Verify on the evaluator: `&Self`, `&mut Self`, and `Self` bindings, receiver-only binding (`fn() -> i32`), use-after-move, exclusive-loan conflict, temporary rejection, `SEM0198` on `value.zero`, unchanged generic-receiver diagnostic, hover `fn() -> i32` on the bound member. Add one corpus program for the native differential and one bound program to the LLVM/Wasm section realization test.

## 3. Specs and documentation

- [x] 3.1 Update the functions/callables reference page and regenerate the diagnostic index.
- [x] 3.2 Run `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`.
