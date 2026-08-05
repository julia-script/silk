## 1. Tokens and diagnostics

- [x] 1.1 Add `Minus` and `Dot` tokens (arrow precedence preserved), `SEM0009`/`SEM0010` codes
      with reason data and constructors, and the signed minimum on the `SEM0002` reason; extend
      lexer and diagnostic tests

## 2. Syntax

- [x] 2.1 Parse signed literals (minus folded into the integer-literal branch) and qualified
      callees (actor, dot, operation) with recovery for missing operation names and dangling
      minus; extend parser tests

## 3. Elaboration and HIR

- [x] 3.1 Signed exact values with full-range checking; the built-in `I32` actor table; qualified
      call resolution with arity checking; `BuiltinCall` HIR expression; encoder + goldens;
      elaboration tests

## 4. MIR, interpreter, backend

- [x] 4.1 `Binary` MIR operation (verifier + encoder + goldens); lowering from builtin calls
- [x] 4.2 Interpreter: exact arithmetic with overflow/div-zero/MIN-by-minus-one traps
- [x] 4.3 Backend: with-overflow intrinsics and guarded division branching to a per-function trap
      block; IR/bitcode goldens
- [x] 4.4 Corpus: arithmetic, overflow-trap, div-zero-trap programs; differential harness green
      natively

## 5. Labs and acceptance

- [x] 5.1 Labs cover qualified callees, builtin HIR calls, binary MIR ops, checked IR; browser
      verification
- [x] 5.2 `pnpm check` and `pnpm release:candidate` green; tick tasks, validate strict
