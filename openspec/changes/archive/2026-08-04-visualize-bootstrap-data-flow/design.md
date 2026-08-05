## Context

By this point the inspector receives exact facts for declarations, arguments, parameters,
references, positional mappings, types, and call contracts. It currently presents related facts in
cards, but understanding the first complete program requires mentally joining several identities.
The view must remain a consumer of compiler facts and preserve the inspector's browser-only state.

## Goals / Non-Goals

**Goals:**

- Derive one small, deterministic data-flow model from semantic identities and provenance.
- Make the successful `42 → identity.value → value → main` path visually obvious.
- Keep incomplete and ambiguous paths equally inspectable and accessible.
- Support source-span navigation without mutating or persisting source.

**Non-Goals:**

- New compiler facts, analysis, execution order, runtime values, profiling, or control flow.
- A general graph editor, automatic layout engine, canvas renderer, or external visualization dependency.
- Replacing the existing concrete tree, fact cards, or diagnostics.

## Decisions

### Derive a dedicated inspector view model

A pure browser-side projection converts semantic facts into ordered flow nodes and edges. Nodes use
existing declaration, parameter, argument, and expression identities; edges exist only for resolved
references and recorded positional mappings. Rendering directly from ad hoc nested fact access was
rejected because incomplete states would become scattered UI conditionals and difficult to test.

### Use a compact semantic lane with an accessible parallel structure

The visual surface uses ordinary document elements and lightweight connectors, paired with an
ordered textual relationship list. Both render from the same view model. A canvas or graph library
was rejected because the graph is tiny, source-order layout is stable, and keyboard and screen-reader
navigation matter more than automatic layout.

### Source navigation is selection, not editing

Activating a flow item stores its source span as ephemeral selection and emphasizes the matching
source and detail cards. It does not move tokens, rewrite text, or persist state. Nodes without exact
provenance remain non-navigable and explain why.

### Model failure as edges that stop or branch

Wrong arity retains known positional pairs and unmatched items. Missing and unavailable references
end at a labeled state. Ambiguous references branch to every recorded candidate. Drawing a faded
successful edge was rejected because it would imply a relationship the analyzer explicitly refused
to choose.

## Risks / Trade-offs

- [The browser projection could become a second analyzer] → Permit only identity joins already guaranteed by semantic facts and unit-test that absent facts never produce edges.
- [Connectors may become unreadable on narrow screens] → Collapse to a vertical lane while preserving the same ordered accessible relationship list.
- [Source emphasis could rely on color] → Pair styling with labels, focus state, and explicit owner-qualified byte ranges.

## Migration Plan

Build and test the pure view model first, then render the complete path, add incomplete states and
source selection, and finally add presets and responsive/accessibility verification. No compiler or
package migration is required.
