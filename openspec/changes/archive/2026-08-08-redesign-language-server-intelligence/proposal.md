## Why

The current language-server hover and navigation behavior came from a narrow spike: hover reports
the type of an enclosing expression while definition consults a separate, incomplete reference
index. As a result, individual tokens often report the same misleading type, declarations and type
references have no editor identity, source-level function signatures are lost, and completion and
inferred-type hints cannot be added without duplicating compiler semantics in the LSP.

## What Changes

- **BREAKING** Replace the reference-only semantic-target query with a compiler-owned, token-level
  semantic-occurrence model covering declaration sites and resolved value, type, field, actor,
  operation, parameter, binding, import, and qualified-name occurrences.
- Make hover consume semantic occurrences and render source-like declarations and signatures,
  preserving names, parameter names, generic parameters, source-visible type spellings, mutability,
  and the `effect fn` distinction. Retain expression-type hover only as a fallback for anonymous
  expressions such as literals.
- Extend go-to-definition to every occurrence with a source declaration, including declared types,
  type arguments, declaration-site names, fields, bindings, parameters, imports, and qualified
  references. Intrinsic actors and operations without source declarations remain hoverable but do
  not invent definition locations.
- Introduce a first-class intrinsic signature catalog so built-in actors and operations such as
  `Effect.catch` and `SystemAllocator.make` have one semantic identity and source-like presentation
  shared by hover and qualified completion.
- Add context-aware completion for visible values, declarations, and types; type positions; actor or
  namespace members; and fields of typed values, while preserving explicit unavailable states in
  incomplete or recovered source.
- Add inferred-type inlay hints for local bindings, using compiler inference rather than LSP-side
  reconstruction.
- Remove the spike's expression-span scanning from identifier hover once the semantic occurrence
  model covers the supported language surface.

## Capabilities

### New Capabilities

- `language-server-hover`: Token-specific, source-like semantic hover for declarations, references,
  types, bindings, intrinsic actors and operations, with an expression-type fallback.
- `language-server-completion`: Recovery-aware, context-sensitive completion for values, types,
  members, fields, and language syntax.
- `language-server-inlay-hints`: Inferred local-binding type annotations delivered through the LSP
  inlay-hint protocol.

### Modified Capabilities

- `language-server-navigation`: Expand navigation from the spike's reference subset to all
  source-backed semantic occurrences and define behavior for declaration sites and intrinsic
  occurrences.
- `bootstrap-analysis-facade`: Replace the narrow position-to-reference query with immutable
  semantic-occurrence, hover-presentation, completion, and inlay-hint query data that remains the
  exclusive semantic boundary for tooling.

## Impact

- `packages/compiler`: analysis snapshot shape, editor semantic index, declaration/type traversal,
  intrinsic metadata, presentation rendering, and completion-scope queries.
- `packages/lsp`: document queries, server capabilities and handlers, protocol conversions, and
  unit/stdio coverage for hover, definition, completion, and inlay hints.
- Public compiler subpaths and the `Analysis` facade may change incompatibly; backward compatibility
  with the unreleased spike API will not be preserved.
- Snapshot memory grows with token-level occurrence data and completion metadata, requiring compact
  immutable representations and deterministic ordering tests.
- No new runtime dependency is expected; language behavior remains server-owned and editor clients
  remain thin protocol consumers.
