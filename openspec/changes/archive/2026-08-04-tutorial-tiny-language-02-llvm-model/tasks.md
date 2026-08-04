## 1. LLVM lesson content

- [x] 1.1 Create the numbered LLVM mental-model lesson and link it from Lesson 1.
- [x] 1.2 Add a Clang-validated minimal `define i32 @main()` example annotated by module, signature, block, instruction, type, and terminator.
- [x] 1.3 Explain frontend, LLVM IR, backend, and linker responsibilities without introducing SSA or PHI early.

## 2. Artifacts and checkpoints

- [x] 2.1 Add the textual IR/bitcode/object/executable producer-consumer table.
- [x] 2.2 Add misconception callouts for LLVM-as-VM, `.ll` as CPU assembly, and bitcode as directly runnable bytecode.
- [x] 2.3 Add the IR-labeling and artifact-order checkpoint with observable expected answers.

## 3. Verification

- [x] 3.1 Compile the annotated IR with pinned Clang to confirm the example is valid.
- [x] 3.2 Check previous/next navigation and non-visual text alternatives.
- [x] 3.3 Run `pnpm check` and `pnpm release:candidate`, reporting any pre-existing failure separately.
