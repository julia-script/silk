## Context

See `proposal.md`,
[IMPORT-007](../../../apps/docs/content/reference/modules-names-and-visibility.md#import-007--reserved-words-may-appear-in-import-paths-but-cannot-become-implicit-bindings),
and [TOOLING-001](../../../apps/docs/content/reference/runtime-and-standard-library.md#tooling-001--tooling-presents-library-source-and-derived-availability-honestly)
for motivation and language direction. Import-path parsing currently requires `Identifier` tokens,
while downstream consumers independently filter path children by that token kind. The
standard-library manifest names `silk/effects` and records `Effect` as its preferred namespace.
Compiler completion has one Effect-specific module lookup, while LSP import discovery indexes public
declarations and its edit planner only creates selected-member imports. Catalog namespaces are also
currently seeded into semantic scope, which conflicts with the explicit-import contract already
established by the module specifications.

## Goals / Non-Goals

**Goals:**

- Give every compiler and tooling consumer one syntax-owned interpretation of contextual import
  path segments.
- Keep namespace discovery, semantic binding, and edit planning separate and explicit.
- Derive namespace completion generically from catalog metadata rather than an Effect spelling
  exception.
- Remove the plural standard-library identity and all current callers in one green-field migration.

**Non-Goals:**

- Change lexical keyword classification, Effect type syntax, runtime semantics, or backend behavior.
- Add a source declaration named `Effect`, a hidden import, an intrinsic, or a compatibility module.
- Redesign project package resolution beyond canonical module identities already accepted by the
  source resolver.

## Decisions

### Introduce a syntax-owned contextual import-path query

The parser will recognize the closed set of keyword token kinds plus `Identifier` when it expects a
path segment, while retaining the original token. A single import-path actor/query will return the
ordered available segment tokens and their text. Module closure, summaries, import planning, name
resolution, and LSP import inspection will consume that query.

This is preferred over reclassifying keywords in the lexer because lexical meaning must stay stable,
and over adding `EffectKeyword` checks at each consumer because that would make the general rule
fragmented and error-prone.

### Diagnose an unusable reserved final segment during parsing

Once the complete import shape is known, the parser will report one construct-level diagnostic when
the final segment is reserved and neither an explicit alias nor selected-member list exists. The
lossless path remains available for tooling and closure recovery, but name resolution creates no
implicit reserved binding.

This is preferred over fabricating an identifier or silently loading a dependency with no usable
binding. Selected-member imports remain valid because they intentionally create no namespace.

### Treat catalog namespaces as inventory, never scope

Preferred namespace metadata will be exposed as deterministic completion inventory, not seeded
bindings. Semantic availability will continue to come only from source declarations, language
bindings, and explicit imports. Existing standard-library uses will be migrated to explicit imports.

This is preferred over a special exception for Effect because the catalog's metadata has one
meaning across all modules and the language already specifies explicit standard-library imports.

### Add a namespace variant to import planning

Import planning will distinguish selected declarations from module namespaces at the request type.
A namespace request carries canonical module and preferred local spelling, searches existing
imports for an equivalent namespace binding, applies the existing deterministic collision policy,
and otherwise emits `import <path> as <local>`. Completion receives the chosen insertion spelling
and source edit together.

This is preferred over synthesizing a declaration called `Effect`, because no such export exists,
and over post-processing selected-member edits, which would conflate two different binding forms.

### Keep compiler completion semantic and LSP enrichment revision-bound

The compiler remains authoritative for recovered completion context, including type versus
non-type classification and qualified member sets. The LSP will merge catalog namespace inventory
only for compiler-reported applicable non-type contexts and will filter by the partial source token.
The inventory and edit plan will be taken from the same accepted workspace/toolchain revision.

This preserves the existing facade boundary while allowing a candidate that has no declaration
identity. It also ensures `Effect<...>` remains the built-in type candidate without an import edit.

### Perform a single breaking distribution migration

Rename the canonical source file and manifest identity to `silk/effect`, regenerate embedded and
documentation artifacts through repository generators, and mechanically update all current source,
tests, fixtures, and documentation. Do not retain `silk/effects` in resolution tables.

## Risks / Trade-offs

- [Broad import-path consumer drift] → Centralize segment extraction and add parser, closure,
  summary, name-resolution, formatter, and LSP tests around a keyword segment.
- [Completion duplicates the built-in Effect type] → Gate namespace inventory on compiler-reported
  non-type contexts and test partial, complete, type, and qualified positions.
- [Removing catalog-seeded scope reveals latent implicit uses] → Migrate the whole repository and
  let compiler diagnostics identify every missing explicit import before handoff.
- [Generated artifacts retain the plural identity] → Run manifest/documentation generators and
  release-candidate validation after migration, then search distribution inputs and outputs.
- [Alias behavior diverges between completion and quick fixes] → Put collision and import-shape
  decisions in the shared import planner rather than protocol presentation code.

## Migration Plan

1. Land contextual import-path syntax and downstream segment consumption with focused tests.
2. Remove catalog namespace seeding and introduce catalog namespace completion plus namespace import
   planning.
3. Rename the Effect source and manifest identity, then migrate all current repository references.
4. Regenerate checked-in distribution and documentation artifacts.
5. Run focused tests, full repository checks, package-content verification, and an implementation
   audit against the canonical language rules and these delta specifications.

Rollback is a normal source revert before stable release; no compatibility or data migration is
required in this green-field repository.
