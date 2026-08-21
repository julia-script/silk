## Why

Silk's Effect operation module is plural only because the import parser rejects the reserved word
`effect` as a path segment, and the LSP cannot currently discover the module namespace from a
partial spelling. The language should expose the singular concept name while keeping Effect type
syntax and operation imports contextually distinct.

## What Changes

- **BREAKING** Rename the canonical standard-library module `silk/effects` to `silk/effect`, with
  no compatibility module or fallback.
- Admit reserved-word tokens as contextual import-path segments without changing their lexical
  classification or making reserved words legal bindings.
- Require a reserved final path segment to be paired with an explicit namespace alias or a
  selected-member list, so an import cannot create an unusable implicit binding.
- Make catalog namespace metadata discoverable by completion from complete and partial spellings in
  non-type contexts.
- Add namespace-import completion edits that materialize `import silk.effect as Effect`, reuse an
  equivalent import, and apply ordinary deterministic collision aliasing.
- Preserve `Effect<...>` as closed language type syntax whose completion requires no import.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-syntax`: Import paths accept contextual reserved-word segments and diagnose an
  unusable reserved final segment.
- `bootstrap-module-closure`: Canonical module identity is derived from every contextual import
  path segment, regardless of its retained token kind.
- `bootstrap-name-resolution`: Catalog namespace metadata does not create implicit scope; reserved
  final segments bind only through a legal explicit form.
- `bootstrap-silk-stdlib`: The canonical ordinary-source Effect operation module becomes
  `silk/effect`.
- `language-server-completion`: Catalog namespaces are deterministic completion candidates from
  partial spellings in applicable non-type contexts, while closed Effect type completion remains
  import-free.
- `language-server-auto-import`: Completion can plan an explicit namespace import, reuse an
  equivalent binding, and choose a deterministic alias on collision.

## Impact

This affects import parsing and recovery, import-path syntax queries, module closure and summaries,
name resolution, import edit planning, compiler completion data, LSP completion presentation, the
standard-library manifest and embedded distribution artifacts, and all current source, tests,
fixtures, and documentation that name `silk.effects`. It introduces no intrinsic, runtime, Effect
execution, ownership, backend, or target change. The accepted language direction is SLP-0012.
