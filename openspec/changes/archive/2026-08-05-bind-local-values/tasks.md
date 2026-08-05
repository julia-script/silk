## 1. Diagnostics and lexer

- [x] 1.1 Add the `'ownership'` phase (rank 4), `SEM0008` rebinding and `OWN0001` use-after-move
      codes with reason data and constructors to `Diagnostic.ts`; update diagnostic tests
- [x] 1.2 Add `let` and `move` keywords and the `=` token to the lexer with keyword-prefix
      protection; extend lexer tests

## 2. Syntax

- [x] 2.1 Parse statement sequences: binding statements (`let name = expression`) and `move`
      operands as concrete branches with bounded recovery (missing name/equals/initializer/
      return, bare move); extend parser tests and the syntax encoder + goldens

## 3. Elaboration and HIR

- [x] 3.1 Add HIR statements (`Bind`, `Return`), `BindingReference` and `Move` expressions with
      function-local binding identities; extend the HIR encoder + goldens
- [x] 3.2 Elaborate statement sequences: ordered scope map over parameters and completed
      bindings, initializer-type inference, `SEM0008` non-shadowing, `SEM0006` for unknown
      names including moves; extend elaboration tests

## 4. Ownership

- [x] 4.1 Real liveness: `let` bindings live from statement to last use; moves end liveness;
      `OWN0001` + `Violation` verdict for use-after-move; releases (LIFO, moved bindings
      excluded) in the cleanup plan; extend encoder + goldens and tests

## 5. Lowering, evaluation, backend

- [x] 5.1 Lower bindings to typed locals and plan releases to generated `Drop`s before exit
      terminators; violations lower to generated traps; extend lowered goldens
- [x] 5.2 Interpreter executes binding programs and drops; extend the corpus with binding,
      moved-binding, and use-after-move programs; differential harness covers them natively
- [x] 5.3 Confirm backend emission of binding programs (locals + ignored drops) against IR and
      bitcode goldens

## 6. Facade, labs, acceptance

- [x] 6.1 Expose binding facts and violation verdicts through the analysis facade; keep the
      facade-boundary test green
- [x] 6.2 Ownership lab: `let` timelines, move-shortened ranges, violation display; MIR lab
      shows lowered drops; verify both in the browser
- [x] 6.3 `pnpm check` and `pnpm release:candidate` green; tick tasks, validate strict
