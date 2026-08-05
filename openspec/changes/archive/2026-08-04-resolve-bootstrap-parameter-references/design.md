## Context

The preceding syntax change preserves ordered typed parameters, bare identifier expressions, and
call arguments but deliberately assigns no local semantic meaning. Top-level function declarations
already use deterministic source-local identities and closed lookup outcomes, providing a small
pattern that can be reused without building a general scope graph.

## Goals / Non-Goals

**Goals:**

- Give parameters stable identities, declared types, lookup, reference facts, and diagnostics.
- Resolve references consistently in returned expressions and call arguments.
- Keep each function's namespace isolated and preserve exact syntax provenance.
- Surface every resolution state in the inspector.

**Non-Goals:**

- Locals, nested scopes, captures, shadowing rules, top-level values, or functions as first-class values.
- Argument-to-parameter binding or call-contract checking.
- A reusable general scope graph, AST, HIR, MIR, or execution.

## Decisions

### Nest parameter identity under function identity

A parameter identity consists of the existing owning function identity plus its concrete parameter
ordinal. This remains deterministic for missing and duplicate names and prevents same-spelled
parameters in different functions from colliding. Global source offsets alone were rejected because
they obscure ownership and make inspector relationships harder to read.

### Build one closed lookup table per function

Analysis collects every parameter header for a function before resolving any identifier expression
in that function. Lookup returns resolved, missing, or ambiguous data and never selects the first
duplicate. Reusing top-level declaration lookup directly was rejected because it would accidentally
merge namespaces and imply that a function name is a value expression.

### Reuse type resolution but retain parameter provenance

Parameter type identifiers follow the existing exact `I32` rule and `SEM0001` diagnostic. The
resolved type fact retains parameter-type syntax rather than borrowing the function return type.
This avoids a second type system while keeping diagnostics and visual links precise.

### Keep reference resolution separate from call binding

Every identifier argument can resolve to the caller's local parameter now, but it is not mapped to a
target parameter until the next change. This separates lexical scope from call contract and keeps
failures independently inspectable.

## Risks / Trade-offs

- [Parallel lookup implementations could diverge] → Use equivalent closed outcome vocabulary and deterministic ordering while keeping function and parameter actors distinct.
- [Diagnostic codes may become crowded] → Reserve `SEM0005` for duplicate parameter declarations and `SEM0006` for missing local references, with reason data carrying exact provenance.
- [A later scope graph will replace this lookup] → Keep the public contract concept-oriented so a prerelease breaking implementation change need not alter observable facts unnecessarily.

## Migration Plan

Add parameter facts and lookup first, then resolve identifier expression facts and types, then wire
return compatibility and diagnostics. Update inspector relationships, documentation, and release
validation after the semantic API is stable. No compatibility layer is required.
