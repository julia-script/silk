## Context

Existing user-interface witness machinery is deliberately narrow: operations are pure, target
generic arguments are absent, and value operands are adapted to shared borrows. General decoders and
representation-dependent wrappers require literal ownership and complete flow contracts.

## Goals / Non-Goals

**Goals:**

- Preserve literal parameter ownership and complete rows in interface operations.
- Infer and specialize generic mapped witness functions.
- Keep generic call contracts exact while admitting smaller witnesses by subsumption.

**Non-Goals:**

- Runtime interface dispatch, service slots, associated output rows, implicit `Self`, or interface
  inheritance.
- Provider-specific source contract narrowing after generic type checking.

## Decisions

### Make source ownership literal

Interface parameter syntax has ordinary Silk meaning: `T` transfers, `&T` shares, and `&mut T`
grants exclusive access. Remove blanket by-value-to-borrow adaptation for user interfaces. A target
must match parameter shapes and cannot demand stronger receiver access than the interface promises.
Any temporary adapter for compiler-sealed operators remains inside intrinsic lowering only.

### Treat interface rows as exact caller contracts

Generic bodies type-check against the instantiated interface rows. A witness with fewer failures,
fewer requirements, or weaker required access may satisfy that contract through explicit subsumption;
its contract widens at the interface boundary. Specialization may optimize unreachable machinery but
does not change source-visible caller types.

### Infer mapped target binders from one substituted contract

Substitute the conformance head and specialized interface arguments, then unify the target function's
receiver, ordinary parameters, success, rows, and representation contracts in declaration order.
Every target binder must resolve uniquely. Keep the generic HIR witness question unresolved while
retaining its substituted provider, interface application, and operation contract; retain the
resulting arguments on the conformance mapping and concrete instance key. Reject missing or
conflicting inference before MIR.

### Migrate ordinary operator interfaces explicitly

Update `Order` and `HashKey` declarations and witnesses so their intended borrows appear in source.
Do not preserve old source spelling through hidden general adaptation, because that would make Decoder
ownership ambiguous and perpetuate two interface calling conventions.

## Risks / Trade-offs

- [Migration breaks pre-stable interface source] → Update the standard library and acceptance corpus
  atomically; backward compatibility is not a goal at this stage.
- [Row subsumption accidentally narrows generic callers] → Assert source contracts before and after
  specialization and compare HIR/MIR-facing rows.
- [Generic witness inference becomes ambiguous] → Require every binder to be determined by the
  substituted operation/conformance contract and diagnose the first unresolved binder.

## Migration Plan

1. Extend interface operation and application facts with literal operands and rows.
2. Add contract substitution, variance, and subsumption checking.
3. Add mapped target generic inference and witness instance arguments.
4. Migrate `Order`, `HashKey`, and their ordinary witnesses.
5. Confine or remove legacy intrinsic operand adapters and add negative ownership fixtures.

Rollback would require restoring the old standard-library declarations and blanket adaptation
together; mixed conventions are not supported.
