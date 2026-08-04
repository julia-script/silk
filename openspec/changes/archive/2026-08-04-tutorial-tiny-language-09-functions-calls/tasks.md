## 1. Two-pass resolution

- [x] 1.1 Add a first pass that rejects duplicate function names, validates `main`, creates all-`i32` signatures, and declares every function.
- [x] 1.2 Add a second pass that maps parameter names to `Value.argument`, lowers each body, and resolves direct calls through the function table.
- [x] 1.3 Return typed compiler diagnostics for unknown parameters/functions, wrong arity, and impossible void direct-call results.

## 2. Lesson and fixtures

- [x] 2.1 Create Lesson 9 explaining declaration-before-body with a visible function-table checkpoint.
- [x] 2.2 Add reordered-definition and recursive factorial fixtures plus expected IR assertions.
- [x] 2.3 Explain why recursion is static emitted control flow and does not require a JIT.

## 3. Verification

- [x] 3.1 Run multi-function, forward-call, wrong-arity, duplicate-name, and factorial tests.
- [x] 3.2 Compile and execute factorial with a result below the process exit-code limit.
- [x] 3.3 Run `pnpm check` and `pnpm release:candidate`, reporting any pre-existing failure separately.
