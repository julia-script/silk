# SLP-0012: Reserved module path segments and explicit namespace completion

SLP: 0012
Status: Accepted direction
Revision: 1
Author: Julia Ortiz
Created: 2026-08-21
Updated: 2026-08-21
Discussion: —
Review record: —
Depends on: SLP-0005, SLP-0008
Split from: —
Split into: —
Supersedes: —
Superseded by: —
Revisit when: —
Resolution: Accepted by the author for immediate OpenSpec handoff and implementation on 2026-08-21.
OpenSpec handoff: `rename-effect-module`

## Summary

Silk import paths denote module identities rather than program bindings, so a reserved word may be
used as a path segment when the import grammar unambiguously expects a segment. The standard
library adopts singular `silk.effect`, imported as the ordinary namespace binding `Effect` through
`import silk.effect as Effect`. Tooling treats catalog namespace metadata as an explicit-import
completion source: typing a complete or partial `Effect` in a non-type context offers a completion
that inserts the namespace binding and its import, while `Effect<A ! E ? R>` remains closed language
type syntax and requires no import.

## Problem and evidence

The Effect combinator module is currently named `silk.effects` only because `effect` is lexed as
`EffectKeyword` and import-path parsing accepts only `Identifier` tokens. That couples external
module identity to the program-identifier grammar even though module loading already accepts the
canonical identity `silk/effect`. The workaround produces the awkward plural spelling:

```silk
import silk.effects as Effect
```

The catalog already records `Effect` as the preferred namespace for that module, but completion
indexes source declarations. Because the closed `Effect` type has no ordinary declaration in the
module, partial `Eff` completion cannot offer the namespace import. The result is a mismatch: the
language requires an explicit ordinary import for combinators, while tooling cannot discover that
import from the catalog metadata that defines it.

## Driving examples: current and desired

### Case: Import the Effect combinator namespace under its singular concept name

#### Intent

Use ordinary Effect combinators through the conventional `Effect` actor spelling while preserving
`effect fn` and `effect {}` as reserved language syntax.

#### Current Silk

```silk
import silk.effects as Effect

effect fn delayed() -> i32 {
  return run Effect.suspend(effect { return 42 })
}
```

Renaming the module to `silk/effect` currently makes the import path fail because the lexer retains
`effect` as `EffectKeyword` while the import parser expects an `Identifier` segment.

#### Desired Silk

```silk
import silk.effect as Effect

effect fn delayed() -> i32 {
  return run Effect.suspend(effect { return 42 })
}
```

#### Observable result

The import resolves canonical module `silk/effect`; the program evaluates to `42`; the module
namespace is the ordinary explicit binding `Effect`; and the two effect-language constructs retain
their existing meanings.

#### Boundary case

```silk
fn invalid(effect: i32) -> i32 {
  return effect
}
```

This remains invalid. Accepting a reserved token as an import-path segment does not make it a legal
declaration, parameter, alias, selected member, expression identifier, or type name.

### Case: Complete the namespace import from a partial spelling

#### Intent

Discover the ordinary Effect actor without remembering its module path or first writing an invalid
complete reference.

#### Current Silk

```silk
pub effect fn main() {
  let deferred = Eff
}
```

Completion can know the closed language type `Effect`, but it cannot offer an import-bearing
namespace candidate because workspace inventory contains public declarations rather than catalog
namespace aliases.

#### Desired Silk

At the `Eff` cursor, completion includes an item rendered as `Effect` from `silk/effect`. Accepting
it produces:

```silk
import silk.effect as Effect

pub effect fn main() {
  let deferred = Effect
}
```

The same namespace candidate is available for a complete non-type spelling when the explicit
namespace import is absent. Completion acceptance reuses an existing equivalent import and never
adds a duplicate.

#### Observable result

The completion item inserts the requested source spelling and an explicit namespace import in one
workspace edit. Subsequent `Effect.` completion exposes the public operations of `silk/effect`.

#### Boundary case

```silk
fn retain(value: Effect<i32>) -> Effect<i32> {
  return value
}
```

In a declared-type or type-argument context, `Effect` is offered as closed language type syntax
without an import edit. The LSP does not insert `silk.effect` merely because the type is written.

### Case: Keep a reserved final path segment from creating an unusable implicit binding

#### Intent

Ensure contextual path acceptance does not create a namespace name that source cannot reference.

#### Current Silk

```silk,ignore
import silk.effect
```

The spelling is currently rejected syntactically before a binding can be considered.

#### Desired Silk

```silk,ignore
import silk.effect
```

This remains invalid because the final reserved segment would become the implicit namespace
binding `effect`. Source must use an explicit alias or selected-member list.

#### Observable result

The compiler diagnoses the missing usable binding form at the import rather than admitting an
unreferenceable namespace.

#### Boundary case

```silk
import silk.effect as Effect
import toolkit.effect.helpers as Helpers
```

Both are valid: the first supplies an explicit alias, and the second ends in the ordinary binding
`helpers` even though an interior segment is reserved.

## Goals and non-goals

### Goals

- Separate import-path segment recognition from ordinary identifier eligibility.
- Rename the canonical standard-library module from `silk/effects` to `silk/effect` without a
  compatibility alias.
- Preserve the reserved meanings of `effect fn` and `effect {}` everywhere outside import paths.
- Make catalog namespace aliases first-class completion candidates with explicit namespace-import
  edits, including for partial spellings.
- Distinguish the importable `Effect` operation namespace from closed `Effect<...>` type syntax.
- Reuse equivalent imports and preserve ordinary collision handling.

### Non-goals

- Make reserved words legal declaration or local-binding names.
- Export a source declaration named `Effect` from the standard-library module.
- Add an implicit Effect prelude or compiler-known standard-library actor.
- Preserve `silk.effects` as an alias, fallback, or migration path.
- Design package acquisition, package aliases, or filesystem names outside canonical module
  identity.

## Current language model

The lexer classifies every complete `effect` spelling as `EffectKeyword`. Import paths are parsed as
dot-separated `Identifier` tokens, and downstream consumers recover their module identity by
filtering the path syntax for `Identifier`. Canonical module validation itself already accepts the
text `effect`. Standard-library manifest entries may declare a preferred namespace, but workspace
completion candidates are built from public source declarations and import planning produces
selected-member imports.

`Effect<A ! E ? R>` is closed language type syntax. The standard-library Effect module contains
ordinary combinator functions, not a declaration that owns the closed type.

## Proposed language model

An import-path segment is a contextual path name. The parser accepts an ordinary identifier or a
reserved-word token wherever it unambiguously expects a path segment, retaining the original token
kind and bytes in the lossless syntax tree. Import-path consumers read ordered segment tokens from
the `ImportPath` actor rather than reconstructing paths by filtering for ordinary identifiers.

A final reserved segment cannot supply an implicit namespace binding. It is valid when an explicit
alias follows, when selected members are imported without a namespace binding, or when it is not the
final segment. Aliases and selected-member names continue to require ordinary identifiers.

The canonical Effect combinator module is `silk/effect`, with manifest namespace `Effect`. Catalog
namespace metadata is tooling input, not semantic injection. Completion may offer `Effect` from
`silk/effect` in expression or actor positions even though the module exports no same-named source
declaration. Accepting that item creates `import silk.effect as Effect`. Type positions continue to
offer the closed Effect type without an import.

## Worked language experience

```silk
import silk.effect as Effect
import silk.result { Result }

effect fn inspect<A, E, ?R>(self: once Effect<A ! E ? R>) -> Result<A, E> ? R {
  return run Effect.result(move self)
}
```

The two `Effect` uses are intentionally different but locally predictable. The type application is
language syntax. The qualified `Effect.result` lookup resolves through the explicit module namespace
binding. Removing the import leaves the type valid and the operation namespace unavailable.

When `Effect` conflicts with an existing local binding, completion follows ordinary import-collision
policy and proposes a deterministic alias rather than shadowing or injecting another lookup tier.
The edit remains explicit source, for example:

```silk
import silk.effect as SilkEffect
```

## Semantic sketch

- Lexical keyword classification is unchanged.
- Import parsing contextually admits reserved-word tokens as path segments.
- Ordered path segments map one-for-one to canonical slash-separated module identity.
- A reserved final segment requires an explicit alias or a selected-member import.
- The module `silk/effects` ceases to exist; `silk/effect` is its replacement identity.
- `Effect` remains closed type syntax and requires no import in a type context.
- `Effect` as a qualified operation namespace is an ordinary explicit module binding.
- Namespace completion is derived from catalog metadata and results in a source edit.
- Partial-spelling completion does not wait for an unresolved complete semantic occurrence.
- No runtime, ownership, Effect execution, failure-row, or requirement-row semantics change.

## Compiler–standard library boundary

### Compiler necessity

Only the parser and compile-time module/name machinery can distinguish a keyword token used as
language syntax from the same bytes used inside an import path. Ordinary Silk cannot define import
grammar or canonical module resolution. Completion context and source-edit planning likewise belong
to compiler/tooling layers.

### Smallest target-neutral primitive

No source-callable intrinsic is added. The smallest compiler change is contextual import-path
recognition plus a syntax-owned segment query used by module and tooling consumers.

### Standard-library construction

The Effect operations remain ordinary Silk in canonical module `silk/effect`. Its namespace is
catalog metadata used for discovery; it grants the declarations no semantic privilege.

### Privilege audit

The compiler does not recognize `silk/effect` during semantic analysis, HIR, MIR, evaluation, or a
backend. It recognizes only import grammar and generic catalog namespace metadata in tooling. The
explicit import participates in ordinary module closure and name resolution. No hidden actor,
prelude binding, spelling-based operation dispatch, intrinsic, runtime service, or target behavior
is introduced.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Affected | Import paths admit contextual reserved segments; final reserved segments need a usable binding form. |
| Types and abstraction | Not affected — closed Effect type is preserved | `Effect<A ! E ? R>` remains language type syntax and library declarations remain ordinary. |
| Execution contracts | Not affected — import and completion are static | Effect construction, execution, failures, and requirements do not change. |
| Ownership and resources | Not affected — no value semantics change | Imports and completion edits do not move, borrow, allocate, or clean values. |
| Runtime and targets | Not affected — module identity erases before execution | Native, Wasm, and evaluator behavior remain identical after resolution. |
| Compiler | Affected | Parser recovery, import-path segment extraction, module closure, summaries, plans, and name resolution must use contextual segments. |
| Standard library | Affected | The Effect source and manifest identity become singular with no compatibility module. |
| Tooling and diagnostics | Affected | Formatting must preserve keyword tokens in paths; completion and auto-import gain namespace candidates and namespace edits. |
| Learning and use | Affected | Programmers distinguish closed type use from explicitly imported operations by context. |

## Scope cohesion

The grammar change, singular module identity, and namespace completion are one decision: an ordinary
catalog module must be importable and discoverable under the concept name `effect` without turning
that word or the `Effect` type into a library declaration. Separating completion would leave the
explicit-import model technically correct but practically undiscoverable; separating the rename
would remove the driving case for the contextual path rule.

## Complexity and subtraction budget

Add one contextual path-segment abstraction and one namespace-import candidate/edit shape. Remove
the plural module identity and every hard-coded `silk/effects` reference. Do not add compatibility
aliases, a second Effect binding model, lexer modes, token rewriting, hidden imports, or a
spelling-specific semantic exception.

## Surface displacement

The proposal replaces `silk.effects` with `silk.effect`, broadens only import-path segment syntax,
and promotes existing catalog namespace metadata into completion input. All callers, tests,
fixtures, generated artifacts, and current documentation migrate in the same change.

## Drawbacks and risks

- A keyword-colored token inside an import path may require syntax-aware highlighting refinement.
- Namespace candidates can duplicate a same-spelled language type or exported declaration unless
  completion preserves context and labels the import source clearly.
- Import planning must distinguish namespace imports from selected-member imports; conflating them
  would generate `import silk.effect { Effect }`, which is invalid because no such declaration
  exists.
- A generic reserved-segment rule exposes unusable implicit bindings unless the final-segment rule
  is enforced.

## Alternatives and prior art

### Status quo

Keep `silk.effects` and declaration-only completion. This avoids changes but preserves a module name
chosen around a parser accident and leaves the ordinary Effect operation namespace undiscoverable
from partial completion.

### Smaller primitive or library solution

Special-case only `EffectKeyword` in `parseImportPath` and hard-code one LSP item. This meets the
driving example with less initial code but creates two spelling exceptions and does not establish a
coherent path-name or catalog-completion model.

### Strongest competing language model

Make `effect` contextual everywhere and let the lexer emit `Identifier` outside `effect fn` and
`effect {}`. This can make more names legal, but it changes declaration, expression, formatting,
highlighting, and recovery rules far beyond module identity and weakens the simple reserved-word
model.

## Falsifiers and acceptance blockers

- Import grammar cannot determine reserved segments without ambiguity or loss of syntax fidelity.
- A namespace completion cannot be expressed as an explicit ordinary import without giving the
  standard-library module semantic privilege.
- Type and namespace completion contexts cannot be distinguished reliably enough to avoid unwanted
  imports for `Effect<...>`.
- The final-reserved-segment rule requires a second name-resolution tier rather than an ordinary
  alias or selected import.

Repository inspection found none of these blockers: the import grammar is fenced, syntax retains
original token kinds, canonical module validation already accepts `effect`, the manifest already
records namespace aliases, and completion already distinguishes declared-type, expression, and
actor-member contexts.

## Open realization questions

- Choose the stable diagnostic code and wording for an alias-less import ending in a reserved
  segment.
- Decide whether syntax highlighting presents a reserved path segment as a keyword or module path;
  this cannot reverse the accepted grammar or binding model.
- Choose the completion-item kind and label details for a catalog namespace candidate.

## Future directions

The same contextual segment model can admit canonical module spellings that are not ordinary
identifiers, provided the canonical identity grammar and source spelling are reconciled explicitly.
Package namespaces and third-party catalog metadata can later reuse namespace completion without
changing name resolution.

## OpenSpec realization map

| Slice | Required reconciliation |
| --- | --- |
| Import-path grammar | Accept reserved-word tokens contextually, preserve lossless tokens, and reject unusable implicit reserved bindings. |
| Module closure and naming | Derive ordered canonical segments independently of ordinary identifier token kind. |
| Standard-library identity | Rename the canonical module, source, manifest entry, generated catalog, documentation, and all current callers without a compatibility path. |
| Namespace completion | Index catalog namespace aliases, match partial spellings by completion context, and distinguish language-type candidates from import-bearing actor candidates. |
| Import edits | Plan and deduplicate explicit namespace imports separately from selected-member imports, preserving collision aliases. |
| Verification | Cover parsing, resolution, formatting, completion, edit resolution, migration, generated artifacts, and required repository checks. |

## Revision and decision record

| Revision | Date | Change or decision |
| --- | --- | --- |
| 1 | 2026-08-21 | Recorded the author-accepted singular `silk.effect` import, contextual reserved module-path segments, and partial namespace auto-import completion with closed Effect type contexts excluded. |
