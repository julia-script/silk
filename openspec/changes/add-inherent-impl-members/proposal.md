## Why

Silk has no type-owned members. `Option.map` resolves only because `Option` happens to match the
basename of `option.silk`: `NameResolution.scopedModule` projects a module's public functions
through any nominal whose normalized name equals the file's normalized name, and that projection is
re-implemented in call resolution, expression analysis, and completion. Membership therefore depends
on filesystem spelling rather than declaration intent, every public root function of such a module
is implicitly a "member", and a type import silently doubles as a module namespace. This change
introduces explicit inherent members so that `Type.member` means a declared member of `Type`.

## What Changes

- Accept an inherent impl declaration `impl [<Binders>] Owner[<Binders>] { fn ... }` beside the
  existing conformance form `impl [<Binders>] Contract for Provider { ... }`. One syntax node; the
  absence of `for` classifies the declaration as inherent.
- Introduce associated-member facts: an inherent impl attaches its functions to one canonical
  nominal owner. A member whose first parameter is spelled `self` and typed as the owner (`Self`,
  `&Self`, `&mut Self`, or the owner applied to the impl binders) is a receiver method; any other
  member is an associated function. Both keep the ordinary explicit-receiver callable contract with
  the receiver as parameter zero.
- Restrict V1 inherent impls to the module that declares the owner, exactly as conformances are
  provider-local today. Several impl blocks for one owner MAY appear in that module. Reject foreign
  owners, alias owners, and specialized or conditional heads such as `impl Option<i32>`.
- Require one member name to identify at most one associated item per owner across every impl
  block, and reject an inherent member whose name collides with a field, union variant, enum
  member, generated operation, or declared contract operation of the owner. No source-order winner.
- Resolve `Type.member(...)`, `Type<Args>.member(...)`, `Type.member` as a function item, and
  `Type.member(trailing)` as a section through the associated-member set. A declared member wins over
  every other type-qualified projection for that spelling; the legacy filename projection is
  untouched otherwise and is retired by `migrate-stdlib-to-inherent-impls`.
- Order an associated member's canonical generic sequence as unbound owner binders followed by
  member-local binders, so `Option.map<i32, i32>(...)` and `Option<i32>.map<i64>(...)` both work.
- Treat struct, union, enum, service, and interface owners through one associated-member path. A
  service differs from an interface only by dependency eligibility; any qualifier-resolution branch
  that distinguishes them beyond that is out of spec and is unified here.
- Encode inherent members in the module semantic surface so dependents invalidate when a member
  contract changes, and expose them through formatting, highlighting, document structure, hover,
  definition, references, rename, `Type.` completion, and generated documentation.
- Leave `value.member(...)` untouched: it stays a callable-field application until
  `add-method-call-syntax`.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-syntax`: parse and losslessly retain inherent impl declarations beside conformances.
- `bootstrap-declaration-index`: index inherent members under their canonical owner with receiver
  classification, head validation, ownership locality, and duplicate detection.
- `bootstrap-name-resolution`: resolve type-qualified associated members through declared
  membership, with declared members taking precedence over any other type-qualified projection, and
  never attaching a root declaration to a type.
- `bootstrap-type-generics`: bind an associated member's generics as owner binders then local
  binders, pre-bound by an applied qualifier.
- `bootstrap-callable-values`: make associated members first-class callables and sections.
- `bootstrap-module-semantic-surface`: encode inherent members in the exported surface.
- `silk-source-formatting`: give inherent impl declarations one canonical layout.
- `language-server-completion`: list associated members after `Type.`.
- `language-server-navigation`: give every associated member one semantic identity across its call
  forms.
- `silk-documentation-model`: document inherent members under their owner.

## Impact

Parser and formatter (`Parser/Declaration.ts`, `SyntaxFormatter.ts`), declaration collection and
index (`DeclarationCollection.ts`, `DeclarationFacts.ts`, `DeclarationIndex.ts`,
`DeclarationCompletion.ts`), qualified lookup (`NameResolution.ts`, `CallResolution.ts`,
`ExpressionAnalysis.ts`, `TypeInference.ts`), module surface (`ModuleSurface.ts`,
`ModuleSummary.ts`), editor intelligence (`Completion.ts`, hover, navigation, LSP document
structure), docgen, highlighting tables, and the syntax inspector. No HIR, MIR, layout, or backend
change: an inherent member lowers as the ordinary function it is. No source in the repository moves
yet; the standard library and documentation migrate in `migrate-stdlib-to-inherent-impls`.
