# Design

## Decisions

### Authored identity

Classify each available effective `ModuleNamespace` and `ImportedMember` from name resolution. Semantic occurrences retain the exact authored import-name span that supplied an unqualified lookup separately from the canonical declaration identity used by navigation. A qualified member use belongs to its namespace binding, not to an independently authored direct selector with the same target and spelling. Import-role occurrences are excluded. This keeps aliases of one declaration, repeated same-target bindings, receiver members, and shadowed locals independent.

The semantic occurrence index traverses conformance headers, conditional requirements, providers, mapped operation targets, and drop-hook contracts so an import used only by an `impl` remains used.

### Cascade suppression

Unavailable, conflicting, non-effective, and recovered import declarations are excluded. Import declarations owned by LSP0001-LSP0003 retain that ownership; unrelated declarations in the same document remain eligible for LSP0004.

### Safe edits

The compiler emits a `SourceAction.ChangePlan` bound to the exact source snapshot. A sole binding removes only its declaration, anchored at the `import` token rather than parser-owned leading trivia. A member in a list owns one adjacent delimiter. Hybrid imports remove only the unused namespace alias or member-list clause when another binding remains. CRLF is preserved, and any comment whose attachment could change suppresses the edit but not the warning.

### Protocol

The LSP maps facts to Warning-severity LSP0004 diagnostics and unresolved quick fixes titled “Remove unused import”. Each action carries the document URI, version, module, authored spelling, and exact binding span. Resolve reacquires the unused binding and a fresh compiler plan from that exact accepted revision; changed documents and no-longer-applicable bindings are disabled before any workspace edit is returned.
