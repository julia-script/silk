## Context

See proposal.md. The existing `bootstrap-silk-stdlib` scope-actor requirement applies to static utilities as well as runtime APIs. `Pointer` already has an owner; several callers still import its module under that name. User changes already span the repository and must be preserved.

## Goals / Non-Goals

**Goals:** Complete source-owned operation membership and canonical imports across every textual representation.

**Non-Goals:** Remove alias grammar, change primitive namespaces, add compiler special cases, or reinterpret JavaScript `as` type assertions as Silk aliases.

## Decisions

- Put public functions on ordinary owner structs. Constants and supporting enums remain separately selected root declarations because inherent impls contain operations. Preserve target guards around raw Linux impls.
- Keep existing principal data owners; do not add redundant dummy types to modules which already have an owner.
- Prefer canonical module names in the self-hosted compiler; update qualifiers with each import. Resolve actual conflicts with more specific declaration names where practical.
- Audit all tracked text, including historical OpenSpec artifacts. Explicit alias grammar, collision, and invalid-import tests may retain aliases because the alias is the behavior under test; normal examples use canonical names.
- Regenerate derived source/documentation artifacts through repository scripts.

## Risks / Trade-offs

- Moving static operations may reveal assumptions about module-level lookup → validate static evaluation and target selection with existing focused suites.
- Renaming embedded identifiers can alter spans or test intentions → inspect changed fixtures and update exact expectations where necessary.
- Existing user edits may already fail checks → preserve a baseline diff and report attribution accurately.

## Migration Plan

Update declarations and all callers together, regenerate catalogs, audit remaining aliases, then run the repository verification sequence and release-candidate validation.
