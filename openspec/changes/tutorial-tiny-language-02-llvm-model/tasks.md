## 1. LLVM lesson content

- [ ] 1.1 Create the numbered LLVM mental-model lesson and link it from Lesson 1.
- [ ] 1.2 Add a Clang-validated minimal `define i32 @main()` example annotated by module, signature, block, instruction, type, and terminator.
- [ ] 1.3 Explain frontend, LLVM IR, backend, and linker responsibilities without introducing SSA or PHI early.

## 2. Artifacts and checkpoints

- [ ] 2.1 Add the textual IR/bitcode/object/executable producer-consumer table.
- [ ] 2.2 Add misconception callouts for LLVM-as-VM, `.ll` as CPU assembly, and bitcode as directly runnable bytecode.
- [ ] 2.3 Add the IR-labeling and artifact-order checkpoint with observable expected answers.

## 3. Verification

- [ ] 3.1 Compile the annotated IR with pinned Clang to confirm the example is valid.
- [ ] 3.2 Check previous/next navigation and non-visual text alternatives.
- [ ] 3.3 Run `pnpm check` and `pnpm release:candidate`, reporting any pre-existing failure separately.


