# Proposal

## Why

Semantic analysis currently precedes HIR, and declaration identities are source-snapshot ordinals.
This prevents source-independent compiler inputs and forces editor reuse to rebind syntax-backed
facts. Julia requested one coordinated compiler-phase milestone, tracked as JUL-205 through JUL-208.

## What Changes

- Introduce complete untyped authored HIR, module-owned literal/name pools, logical owner identities,
  separate presentation, and versioned canonical header/body fingerprints (A1 / JUL-205).
- Lower every loaded source form, including inactive arms and static bodies, before imported
  semantics; retain explicit recovery and exact literal values (A2 / JUL-206).
- **BREAKING** Replace syntax-backed semantics with HIR-to-TIR analysis, indexed semantic results,
  and one typed executable body; migrate tooling and preserve existing in-process reuse (A3 / JUL-207).
- **BREAKING** Publish intent-specific prepared bundles with normalized configuration and a sealed
  selected source/component closure (A4 / JUL-208).
- These work packages form one integration landing. A1's foundation draft cannot independently
  ship an unused competing production pipeline. Persistent caches and generalized query reuse are excluded.

## Capabilities

### New Capabilities

- `authored-hir`: Complete source-independent authored artifacts, stable owners, presentation,
  recovery, exact pools and canonical encodings, plus the shared phase-boundary integration contract.

### Modified Capabilities

None in A1. A3 owns the coordinated terminology and consumer migration of `bootstrap-hir` to typed
TIR, preserving its language behavior; A2–A4 will extend this shared change with the corresponding
existing-capability deltas before their implementation. Existing semantic-surface fingerprints
remain distinct from authored fingerprints.

## Impact

Compiler representation and structural fixtures in A1; parser lowering, semantic analysis,
realization, project reuse, CLI/LSP/docgen and inspection in the integrated milestone. No new
package manager, runtime dependency, disk format, or semantic cache is introduced.
