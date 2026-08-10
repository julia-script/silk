## Context

See `proposal.md`. `RunStaticEffect` removes local environment materialization but still invokes a
runner whose MIR may contain several regions, nested matches, `ReifyEffect`, callback application,
provider loans, typed propagation, and cleanup. Direct Wasm emits that call; LLVM `-O2` inlines it.

## Goals / Non-Goals

**Goals:**

- Describe the actual remaining runner graphs rather than extrapolating from API names.
- Test whether deterministic local/region/exit remapping is mechanically expressible.
- Identify prerequisites and a narrow first production subset, if one exists.

**Non-Goals:**

- Mutating compiler MIR or backend output.
- Claiming semantic safety from instruction-count improvement alone.
- Supporting suspension, recursion, affine capture transfer, or arbitrary general inlining.

## Decisions

### 1. Classify from `RunStaticEffect` roots

The harness follows each direct static runner identity into its concrete MIR function and records
region kinds, outcomes, nested matches, direct/dynamic calls, effect operations, loans, releases,
and cleanup-bearing values. This is operation-shape evidence, not Effect declaration privilege.

### 2. Prototype remapping as immutable test data

A test-only remapper clones synthetic region/local identities and rewrites `Forward`/`Return` exits
into a supplied continuation map. It must be deterministic and reject unknown locals, cycles,
lexical loop exits, cleanup, or multiple returns. The prototype is intentionally incapable of
reaching backend emission.

### 3. Require a prerequisite decision

A production proposal is justified only if at least one useful corpus runner is closed, acyclic,
non-affine, loan-free, and has an unambiguous typed-exit map. Otherwise the spike records the
blocking semantic work rather than weakening guards.

## Risks / Trade-offs

- **[A synthetic remapper hides real complexity]** → Compare its accepted vocabulary with every
  corpus runner and report rejected operations explicitly.
- **[The classifier becomes API-specific]** → Root from MIR `RunStaticEffect` and concrete instance
  identities only.
- **[A production proposal recursively expands scope]** → Select one disposition: propose a narrow
  implementation, name a prerequisite, or close with backend optimization only.

## Migration Plan

No migration. The spike adds test/research evidence only and is deleted or archived without a
runtime switch.
