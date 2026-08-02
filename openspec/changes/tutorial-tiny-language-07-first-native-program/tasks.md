## 1. Minimal lowering

- [ ] 1.1 Add the initial `Compiler` actor that lowers `fn main() = 42` with one builder, `i32 ()` signature, entry block, signed constant, and return terminator.
- [ ] 1.2 Render textual LLVM IR through `IrText.render` and preserve `LlvmError` in the Effect error channel.
- [ ] 1.3 Keep Clang invocation outside the compiler core and LLVM package.

## 2. Lesson and checkpoints

- [ ] 2.1 Create Lesson 7 pairing each builder operation with the minimal IR structure introduced in Lesson 2.
- [ ] 2.2 Add a fixture and IR assertion for `define i32 @main()` and `ret i32 42`.
- [ ] 2.3 Document the Clang compile/run commands, POSIX and PowerShell exit-code checks, and recovery for signature, terminator, and ownership failures.

## 3. Native verification

- [ ] 3.1 Compile the generated IR with pinned Clang and assert native exit code `42`.
- [ ] 3.2 Run consumer typecheck and compiler tests against the packed package.
- [ ] 3.3 Run `pnpm check` and `pnpm release:candidate`, reporting any pre-existing failure separately.


