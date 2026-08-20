## 1. Ordinary failure types

- [x] 1.1 Remove the distinct failure-row kind, `!E` binder facts, and `Row<!E>` value conversion.
- [x] 1.2 Parse Effect channel labels while resolving `E` through ordinary type parameters and structural unions.
- [x] 1.3 Use `never` as the empty failure channel and enforce detached owned payloads through ordinary ownership.
- [x] 1.4 Migrate semantic facts, caches, encodings, HIR outcomes, MIR outcomes, and specialization.

## 2. Recovery and source APIs

- [x] 2.1 Implement ordinary `Without<E,S>` membership and subtraction for concrete selected types and unions.
- [x] 2.2 Generalize catch handlers, residual failures, and `A | B` success joins.
- [x] 2.3 Migrate the canonical Effect standard library and every repository source use.
- [x] 2.4 Rename shipped errors to `*Error`, including `OutOfMemoryError`, and delete all old aliases.

## 3. Verification and cleanup

- [x] 3.1 Add propagation, catch, refail, generic-unknown, union-difference, ownership, and nested-Effect tests.
- [x] 3.2 Update diagnostics, generated catalogs/docs, canonical specs, and language evidence.
- [x] 3.3 Scan for and delete every legacy failure-row node, spelling, compatibility branch, and fixture.
- [x] 3.4 Run typecheck, Biome, evaluator/Wasm tests, native corpus where representation changes, full tests, and `pnpm check`.
