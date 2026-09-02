## 1. Syntax and type layer

- [ ] 1.1 Parse `*const <type>` and `*mut <type>` as a `PointerType` node in `Parser/Type.ts` with missing-mutability recovery, add `Star` to `Grammar.typeStarts`, and verify parser fixtures cover a nested `*mut *const u8`, a bare `*u8`, pointer types in parameter, return, field, and generic-argument positions including `identity<*const i32>(x)`, and that `a < *b` in expression position is unchanged.
- [ ] 1.2 Render pointer types in `SyntaxFormatter` as `*const T` / `*mut T` and verify an idempotent formatting test from irregular spacing.
- [ ] 1.3 Add `Type.Pointer` (`PointerType`, `mutable`, `pointee`) and thread it through `fold`, `key`, `encode`, `substitute`, `DeclarationCollection`, `CallResolution`, `ModuleSurface`, `SyntaxTree`, the inspectors, and every `isReference`/`isSlice` predicate chain (at least `internal/TypeInference`, `CleanupPlan`, `ConformanceProof.copyProof`, `Layout`, `ExecutionAffinity`, `Presentation`, `Completion`), and verify a key/encode round-trip distinguishes `*const i32`, `*mut i32`, and `*const u32`, a surface round-trip encodes a public function with a pointer parameter, `identity<T>` infers `T` from a pointer argument, and a pointer field contributes no cleanup.
- [ ] 1.4 Add the `*mut T` to `*const T` immediate-boundary conversion beside the `ReferenceAccess` arm in `TypeCompatibility` and its `HirLowering` consumer, and verify a call passing `*mut u8` to `*const u8` is accepted while the reverse and a nested `*mut *mut u8` to `*mut *const u8` are rejected.

## 2. Intrinsics and source API

- [ ] 2.1 Add the `Pointer` intrinsic actor with all-target operations `null`, `isNull`, `fromRef`, `fromMutRef`, `fromSlice`, `fromMutSlice`, `offset`, `offsetMut`, `read`, and `write`, marking `offset`, `offsetMut`, `read`, and `write` unsafe with an `invariant` each and adding the actor's arm to the admission switch, and verify the catalog test admits them with their invariants.
- [ ] 2.2 Add `packages/compiler/stdlib/silk/pointer.silk` exposing the `Pointer` API with `read<T: Copy>` and `write<T: Copy>` bounds, documentation, and doctests, register it in the stdlib manifest, and verify `stdlib:check`, the documentation policy, the doctests, and a test that `Pointer.read` on `*const Vector<i32>` is rejected at the bound.

## 3. Layout, ownership, MIR

- [ ] 3.1 Plan `PointerType` in `Layout` as one address-width scalar with pointer alignment on every target, Copy through `Layout.entry.copy` and `copyProof`, and one `Address` calling lane, and verify a layout test reports size and alignment 8 on native targets and 4 on wasm32 and that the type is Copy without `impl Copy`.
- [ ] 3.2 Treat formation as an ordinary read of the borrow with no loan in ownership and verify tests: a struct with a pointer field accepts `impl Copy`, forming a pointer then moving the root is accepted, and forming a slice pointer then writing the array directly is accepted.
- [ ] 3.3 Add the MIR pointer type and the operations for null, null test, formation from a reference or slice address lane, offset, read, and write in `Mir.ts`, lower the intrinsics in `LowerBuiltin`, verify pointee Copy (`isCopy`) and operand agreement in `MirVerification`, add them to `MirEncoding`, and verify a MIR test shows a slice formation feeding an offset and a write, a direct-intrinsic move-only write reports one violation, and encodings are byte-identical across processes.
- [ ] 3.4 Include pointee and mutability in pointer-bearing instance keys and verify `identity<*const i32>` and `identity<*mut i32>` discover as two instances while a `*mut Vector<i32>` parameter discovers no `Vector<i32>` cleanup instance.

## 4. Evaluator

- [ ] 4.1 Add `PointerValue` (null, frame-backed, or ticket-backed) to `BootstrapValue`, execute every pointer primitive in `BootstrapEvaluation` through `selectStored`/`placeAccess` and the ticket path, and verify corpus programs that form a slice pointer, offset by two, write, and read `array[2]`, and that read the second element of a `RawBuffer.view` through a pointer, observe the writes.
- [ ] 4.2 Produce the `Trap` blocked step on a null or dead-frame `read`, `write`, `offset`, or `offsetMut`, and verify a test that returns a pointer to a local from a callee and dereferences it reports the violation naming the primitive.

## 5. Backends

- [ ] 5.1 Lower the pointer type to an LLVM pointer lane in `NativeType`, and the operations (null constant, formation from the borrow's address lane, typed `getelementptr` offset, load/store of pointee lanes) in the native operation actors, and verify the LLVM module verifies and IR text shows a `store` through a formed pointer.
- [ ] 5.2 Verify (no new mechanism) that the extern change's `ForeignCall` arm reloads address roots: an IR text test for a program that forms a pointer to a local, calls a foreign function, and reads the local shows a `load` from the root's storage after the `call`.
- [ ] 5.3 Lower the pointer type and operations in `WasmLanes`/`WasmBackend` over linear memory, include pointer lanes in `carriesBorrowAddress` reload reachability, and verify the pointer corpus programs report the same exit status on the evaluator, LLVM, and direct Wasm, including a program whose Silk callee writes through a `*mut` parameter.
- [ ] 5.4 Add the `Pointer` arm to `CAbi.admit`/`classify` (pointer class, pointee unexamined, mutability in the key) and, when `add-export-c-functions` has landed, forward pointer lanes through export thunks, and verify the classifier table test and an IR test declaring `declare ptr @malloc(i64)` on a 64-bit target.

## 6. Native acceptance

- [ ] 6.1 Extend the C fixture with functions that store through `int32_t *` and fill a `uint8_t *` buffer of a given length, and verify Silk programs observe both writes through `fromMutRef` and `fromMutSlice` on the host target.
- [ ] 6.2 Add the libc corpus: `malloc`, `free`, `memcmp`, `memcpy`, `strlen`, `write`, `read`, and `close` declared as externs with pointer signatures (agreeing with the backend's own `malloc`/`free`/`memcmp` declarations), and verify a program that allocates, copies bytes from a Silk slice, compares, writes them to standard output, and frees returns the expected status and output.
- [ ] 6.3 Add the programs to the native acceptance corpus.

## 7. Tooling and documentation

- [ ] 7.1 Add pointer types to LSP hover and inlay rendering, the syntax inspector, and docgen type rendering, and verify the LSP and inspector fixtures show `*mut u8`.
- [ ] 7.2 Add a pointer section to `values-and-types.md` (types, Copy and null, formation, unsafe dereference, the observability rule) and a `silk/pointer` stdlib reference, update the FFI section and glossary, and verify the docs snippet tests compile every new example.
- [ ] 7.3 Regenerate the diagnostic index and stdlib tables, verify the staleness checks pass, and run the full gate with `node scripts/turbo.mjs run test`.
