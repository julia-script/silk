# Design

## Decisions

### Authored identity
Classify each available `ModuleNamespace` and `ImportedMember` from name resolution. A use must resolve to the same semantic target and carry the authored local spelling; import-role occurrences are excluded. This distinguishes aliases sharing a canonical declaration while respecting shadowing.

### Cascade suppression
Unavailable and conflicting bindings are excluded. Declarations owned by exact-duplicate or consolidation warnings remain owned by LSP0001/LSP0003.

### Safe edits
The compiler emits a `SourceAction.ChangePlan` bound to the exact source snapshot. A sole binding removes its declaration line; a member in a list owns the adjacent delimiter. Any comment in the proposed owned range suppresses the edit but not the warning.

### Protocol
The LSP maps facts to Warning-severity LSP0004 diagnostics and quick fixes titled “Remove unused import”. Snapshot preconditions prevent stale offsets.
