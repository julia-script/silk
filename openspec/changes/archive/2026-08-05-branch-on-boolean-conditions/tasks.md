## 1. Tokens, types, diagnostics

- [x] 1.1 `if`/`else`/`true`/`false` keywords; `Bool` in `SemanticType` and declared-type
      resolution; `SEM0011`/`SEM0012` codes with reason data; lexer and diagnostic tests

## 2. Syntax

- [x] 2.1 Conditional statements (condition, arms, optional else) and boolean literal
      expressions with bounded recovery; parser tests

## 3. Elaboration and HIR

- [x] 3.1 Boolean literal facts; per-operation built-in contracts (comparisons, `Bool.not`);
      condition type checking; argument type checking for user and built-in calls; HIR `If`
      statement and `BooleanLiteral` with arm scopes and unique binding identities; encoder +
      goldens; tests

## 4. Ownership, MIR, interpreter, backend

- [x] 4.1 Arm-scoped liveness, per-return exits, conservative conditional moves; encoder +
      tests
- [x] 4.2 Comparison `Binary` operators (`Bool` type, non-trapping); conditional lowering to
      branch diamonds with joins and arm drops; `Bool.not` via equality; goldens
- [x] 4.3 Interpreter comparison/branch execution; corpus branching programs (both arms,
      arm-scoped drops); backend `icmp`+`zext` emission; differential harness green natively

## 5. Labs and acceptance

- [x] 5.1 Labs show conditionals, diamonds, arm scopes; browser verification
- [x] 5.2 `pnpm check` and `pnpm release:candidate` green; tick tasks, validate strict
