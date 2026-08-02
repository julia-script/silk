## 1. Diagnostic behavior

- [ ] 1.1 Add or finalize distinct lexical, parse, and resolution tagged errors while preserving `LlvmError` unchanged.
- [ ] 1.2 Implement one CLI diagnostic renderer that writes phase, message, and source span or LLVM operation to stderr.
- [ ] 1.3 Add fixtures/tests for invalid characters, missing `else`, unknown functions, wrong arity, and LLVM body validation.

## 2. Failure and bitcode lesson

- [ ] 2.1 Create Lesson 12 with deliberate break/restore boundaries and an explanation of transactional `Function.buildBody` failure.
- [ ] 2.2 Add the `Bitcode.encode` variation from the same committed module and explain bitcode versus bytecode, `.ll`, objects, and executables.
- [ ] 2.3 Verify deterministic bytes and the `42 43 C0 DE` magic header.

## 3. Verification

- [ ] 3.1 Confirm failed examples do not modify the canonical completed compiler state.
- [ ] 3.2 Run consumer typecheck, diagnostic tests, and bitcode checks against the packed package.
- [ ] 3.3 Run `pnpm check` and `pnpm release:candidate`, reporting any pre-existing failure separately.


