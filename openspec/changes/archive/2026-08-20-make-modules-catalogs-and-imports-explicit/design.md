## Context

Catalog metadata, implicit scope, provider layering, runtime inventory, completion, and import diagnostics currently overlap. The migration must first make the catalog descriptive, then remove ambient bindings while tooling can repair source.

## Goals / Non-Goals

**Goals:** explicit scope; layered catalog; non-semantic redundancy; auto-import completion; Effect contract actions; pay-for-use inventories.

**Non-goals:** package management, re-exports, ambient defaults, implicit prelude aliases, or runtime dependency containers.

## Decisions

1. Generate one canonical catalog record per distributed module with identity, digest, docs, layer, and target metadata.
2. Keep source closure driven exclusively by explicit import edges.
3. Preserve duplicate binding semantics and move redundancy opinions to LSP diagnostics/actions.
4. Build an indexed declaration search over the catalog for completion, then synthesize collision-aware explicit imports.
5. Derive target runtime inventory from reachable sealed intrinsics and structured reporting support.

## Risks / Trade-offs

- Removing the prelude requires a repository-wide source migration; auto-import support should land in the same batch.
- Catalog corruption becomes a toolchain-integrity error, not a source diagnostic.

## Migration Plan

Extend catalog generation, implement completion/import edits, migrate all source to explicit imports, remove prelude injection and semantic redundancy errors, enforce layers/runtime inventory, then update docs/tests.

## Open Questions

Re-export syntax and native library distribution remain intentionally separate.
