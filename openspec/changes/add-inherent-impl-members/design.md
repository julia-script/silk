## Context

See proposal.md for motivation. The relevant current state:

- `Parser/Declaration.ts` parses `impl [<T>] Capability for Target { ... }` into one
  `ImplDeclaration` node; the `for` keyword is mandatory. Bodies hold `ImplOperation` mappings
  (`name: path`) or inline `FunctionDeclaration`s.
- `DeclarationCollection.ts` turns every `ImplDeclaration` into a `ConformanceFact`. Inline
  operations already elaborate as hidden `FunctionDeclaration` member facts named
  `impl@N.name`, carrying the impl's binders, a `Self` binder, and a `conformanceImplementation`
  back-reference. Ordinary elaboration, ownership, and lowering compile them with no hook-shaped
  special case.
- `NameResolution.scopedModule` projects a module's public root declarations through a nominal
  whose normalized name equals the basename. `lookupQualified`, `CallResolution.ts`,
  `ExpressionAnalysis.ts` (two sites), and `Completion.ts` (three sites) each re-derive it.
  `Effect.` is an intrinsic qualifier that separately looks up `silk/effect`.
- `ExpressionAnalysis` already routes `Owner<Args>.member(...)` through
  `resolveAppliedInterfaceOperationTarget`, and bare `Owner.member` through
  `analyzeFunctionItem` / `lookupQualified`. Services and interfaces reach qualified operations
  through `serviceOperation`, but the service branch and the interface branch in
  `ExpressionAnalysis` diverge in fallbacks and diagnostics.
- `ModuleSurface.ts` encodes conformances and public declarations; `ModuleSummary.DeclarationKind`
  enumerates kinds.
- No `self` or `Self` keyword exists in the lexer; `Self` is an environment binder inside impls and
  interfaces, and `self` is an ordinary parameter identifier used by convention.
- `Project.ts` knows no dependencies. The only package boundary today is `Stdlib.isReserved`
  (`silk/*`) versus the current project, and conformances are already module-local by spec
  ("Conformances are coherent provider-local facts").

## Goals / Non-Goals

**Goals:**

- One impl node, one collection path, one qualified-member lookup for every nominal owner.
- Inherent members are ordinary functions after collection; nothing after declaration completion
  needs an "associated member" concept except presentation.
- The legacy filename projection keeps working for spellings that have no declared member, so the
  standard library can migrate in a separate change without a flag day inside this one.

**Non-Goals:**

- `value.member(...)` dispatch, receiver adaptation, interface-backed member exposure
  (`add-method-call-syntax`).
- Cross-module impl blocks or a package-wide impl inventory. V1 locality is the owner's module.
- Deleting `scopedModule`, migrating stdlib, or rewriting docs (`migrate-stdlib-to-inherent-impls`).
- Bound method values (`let f = value.map`).

## Decisions

### `for` is optional; classification is the absence of the token

`parseImplDeclaration` parses `impl [<Binders>] Type` and then branches on the next significant
token: `for` continues into the existing conformance parse, `{` starts an inherent body. The node
kind stays `ImplDeclaration`; collection classifies by whether a `ForKeyword` child exists. The
inherent body accepts only `FunctionDeclaration` members; an `ImplOperation` mapping inside an
inherent body is parsed for recovery and rejected by collection.

Alternative: a distinct `InherentImplDeclaration` node. Rejected because formatter, DocBlock,
document structure, inspector, and highlighting would all need a second case for a shape that
differs by one token.

### An inherent impl is a head fact; its members are hidden `FunctionDeclaration` facts

Collection records each inherent impl as an `InherentImplFact { ordinal, self, typeParameters,
owner: DeclaredTypeFact, syntax }` on `ModuleHeaders.inherentImpls`, mirroring `ConformanceFact`'s
head. Each member reuses the inline-conformance member path: a `FunctionDeclaration` member fact in
the declaring module with canonical name `Owner.member` (a dot cannot appear in a source
identifier, so it cannot collide with a root declaration), `name.spelling` equal to the bare member
name with the member's own identifier token (so hover, structure, and rename present `map`, not
`Option.map`), `typeParameters` equal to the impl binders followed by the function's own binders,
`Self` bound to the impl's `Self` binder, and `associatedMember: { ordinal, owner: CanonicalId,
receiver: boolean }`. `receiver` is true when parameter zero is spelled `self` and its declared
type, after stripping one reference and any ownership marker, is `Self` or the owner applied to
the impl binders. A `self` parameter of another type (the standard library's `Effect` struct is a
zero-data owner whose members take `self: once Effect<A ! E ? R>`, the builtin effect type) makes
an associated function, not a method; receiver syntax never applies to it.

Private inherent members are legal and resolve through `Owner.member` inside the declaring module
exactly as a private root function resolves unqualified there. This is what keeps a same-module
conformance mapping `name: Owner.member` working when the target is a private member, which the
standard library relies on for its provider modules.

`DeclarationCompletion` resolves each inherent head's owner type with `resolveDeclaredType`
(exactly as it resolves a conformance provider) and closes `Self` on every member of that head with
the substitution `closeConformanceSelf` applies to inline conformance operations. That function
reads the `Self` binder from `conformanceImplementation` today and is a no-op otherwise, so it
gains an explicit `self: Type.Parameter` argument and the closing loop keys on `associatedMember`
as well. Member headers are resolved before the loop runs, so `Self` survives resolution
symbolically, including inside a member's own bounds.

Because canonical name and `name.spelling` now differ, `DeclarationFacts.memberIndex` keys a
canonical member by its canonical name and any other member by its spelling. `byCanonical`,
scope bindings, and `import { map }` therefore see the root `map` and the member `Option.map` as
distinct keys, and a root `map` beside `impl Option { fn map }` is not a duplicate. The consumers that present
`headers.members` or `headers.declarations` as module-level items (`DeclarationFacts.lookup`,
`Analysis.declarationByName` / `memberByName`, `Completion.namespaceCandidates`, LSP document
symbols, the docgen module model) skip facts with `associatedMember`; document symbols and docgen
group them under the owner instead. Elaboration's body walk and the parameter-identity walks that
serve hover and definition on a member's own parameters keep seeing them.

No separate owner index is needed: the V1 locality rule puts every member in the owner's module,
so `lookupAssociated(index, ownerDeclaration, name)` scans that module's members for
`associatedMember.owner === owner && name.spelling === name`, memoized per module headers.
Duplicates across impl blocks are detected at collection and reported at both declarations; the
losing facts keep a `Duplicate` canonical state so lookup reports the collision with no winner.

Alternative: a separate `AssociatedMemberFact` kind carried through elaboration. Rejected: every
consumer that handles `FunctionDeclaration` would need a second arm, and the research's own data
model is exactly a function with metadata.

### Head validation splits between collection and completion

Collection sees one file with no scope, so only syntactic head checks run there, each a new
semantic diagnostic at the head span: head arguments are not exactly the impl binders in order,
each once → specialized head; any impl binder carries a bound → conditional head; a mapped
`name: path` operation or a `fn drop` hook member in the body → rejected member. Collection also
detects a duplicate member name across the impl blocks of one owner spelling in the module and
gives both facts a `Duplicate` canonical state whose `original` is the shared would-be identity
`Owner.member`, so `byCanonical` finds neither and no source-order winner exists.

`DeclarationCompletion` resolves the head's owner with `resolveDeclaredType` and runs the checks
that need scope beside the existing conformance provider-locality check: owner canonical module ≠
the impl's module → foreign owner; the head's leading identifier binds an alias declaration (looked
up before erasure) → alias owner. A head rejected at completion unpublishes its members by giving
them a non-canonical state before `closedMembers` is built; their bodies still elaborate for
diagnostics, as conformance bodies do today.

### One qualified-member resolver replaces five projections

Add `NameResolution.lookupAssociated(index, ownerDeclaration, name)` returning
`Inherent | Duplicate | Inaccessible | Missing`. It is consulted first, before any existing
qualifier branch, at every site that turns `Qualifier.member` into a reference:
`lookupQualified`; `resolvedFunctionReference` in `CallResolution` (which today returns `undefined`
for every non-namespace qualifier, so bare `Option.map` as a function item currently falls through
to a field projection); the argument-expected-type branch in `CallResolution`; the qualified-call
dispatch in `ExpressionAnalysis`, above the service branch that today fires `unknownActorOperation`
whenever the file name does not match; and `Completion`. An inherent hit produces an ordinary
`Resolved` call reference (the tag navigation and occurrence classification treat as a value
declaration), never `ResolvedServiceOperation` or `ResolvedInterfaceOperation`. Only on `Missing`
does each site continue into its existing branches, including the legacy projection. `Effect`
binds as the ordinary imported `Effect` struct (only `string` and `Intrinsic` are intrinsic
actors), so `Effect.member` reaches an inherent member through the same qualified path as any
owner; the intrinsic-qualifier module lookup is unreachable for `Effect` and is deleted by the
migration change.

This is also where the service/interface divergence is removed: both `ContractFact` kinds reach
`lookupAssociated` through one branch. The existing interface-only escape (`?? own module` when
the member is a declared operation) and the service-only early diagnostic are folded into that
one branch. The bound-operation path (`Bound.operation(args)` inside a generic body) is untouched
and keeps its precedence over module lookup; it never sees an inherent member because bounds name
interfaces, not inherent members. The requirement-channel eligibility check stays where it is.

### Generic order falls out of the fact's binder list

Because the member's `typeParameters` are already owner-then-local, `Owner.member<Args>` uses the
existing `prefixSubstitution`. `Owner<Args>.member<Locals>(...)` is today three different paths
(`resolveAppliedInterfaceOperationTarget` for interfaces, a service short-circuit to a diagnostic
inside it, and `analyzeAggregateLiteral` for union variants). Inherent members add no fourth path:
when the applied owner resolves to a nominal with an inherent member of that name, the call is
rewritten to the bare-qualifier form with the applied arguments prepended to the explicit generic
prefix (`Option<i32>.map<i64>` ≡ `Option.map<i32, i64>`). The ordinary prefix arity check then
rejects an over-long local list, and disagreement between qualifier and argument evidence is
reported by the ordinary mismatch machinery at the receiver argument because the prefix is applied
before arguments are analyzed. The check runs before the service short-circuit and before the
interface gate so a service or interface owner with an inherent member is not diagnosed as an
unknown operation; the variant path is untouched.

### Surface already encodes members

`ModuleSurface.make` encodes every module member header and every conformance head. Inherent
members are member facts, so they are in the surface with no new encoder; the inherent head facts
join the encoding beside conformances so an owner or binder change is visible. `ModuleSummary`
is the auto-import catalog keyed by syntax kinds and is not touched: members are not
auto-importable. The surface encodes private headers and declaration ordinals today for every
declaration kind (a variant reorder is observable by design), and inherent members follow that
rule: reordering impl blocks is a surface change, and a private member edit invalidates importers
exactly as a private root function edit does.

### Presentation labels come from `receiver`

Completion, hover, document structure, and docgen label a member "method" when `receiver` is set
and "associated function" otherwise. Hover on `Owner.member` prints the full contract including
parameter zero; the receiver-bound presentation is deferred to `add-method-call-syntax`.

## Risks / Trade-offs

- [Two projections coexist until migration] → declared members win unconditionally; the legacy
  path is consulted only on `Missing`, and a dedicated test pins that a root function and an
  inherent member of the same name resolve to the member.
- [Hidden canonical name `Owner.member` leaks into presentation] → there is no existing mapping
  for `impl@N.name` (those facts are private and filtered before presentation), so `name.spelling`
  is the bare member name and only `canonical.id.name` carries the dotted form.
- [Surface growth invalidates more importers] → member bodies are not encoded; headers are,
  matching every other declaration kind.
- [Enum owners: qualifier is reserved for members and `value`] → the uniqueness rule rejects
  collisions with members and generated operations; nothing else about enums changes.
- [`self` is a convention, not a keyword] → receiver classification is by spelling of parameter
  zero inside an impl only; root functions are never inspected, and a later change may reserve the
  word without affecting these facts.
- [Recovery table for impl bodies lists `Identifier` as a member start] → keep it so a mapped
  operation in an inherent body recovers cleanly and is rejected semantically; add `pub` and
  `static` to the member-start table, which today admits only `fn`, `unsafe`, `effect`, and
  identifiers, so every spec scenario's `pub fn` parses.
- [`collectTypeParameters` ignores enclosing binders when a member has no `<...>` list] → fix the
  early return to seed from the enclosing list; this changes existing inline-conformance facts too,
  so it lands first with the conformance suite green.
- [`drop fn` parses inside an inherent body and would mint a `drop@impl#N` hook] → reject it at
  collection with a member-scoped diagnostic; only a `Drop` conformance owns a hook.
- [Alias owners resolve to their target before the head check sees an alias] → the completion-time
  head check looks the leading identifier up in module scope and rejects an alias declaration
  before erasure. Alias qualifiers at call sites do not resolve today (the qualified dispatch
  accepts no `AliasDeclaration`), so the associated lookup erases an alias qualifier to its
  canonical owner at every site it is consulted.
- [Enum owners are unreachable in expression position] → the qualified-call dispatch admits
  `EnumDeclaration`, and the function-item path consults the associated lookup before the enum
  member path so `Status.describe` reaches a member while `Status.Ready` keeps its meaning.
- [Formatter reads impl members positionally after two type nodes] → branch on the `for` token and
  slice members from one type node for the inherent form.
- [Documentation links `[`member`]` resolve through an unqualified module lookup in
  `docgen/Project.ts`] → `targetOf` consults the associated members of the enclosing owner (and
  any owner in the module) before the root lookup, with a test that a member link resolves;
  otherwise the standard-library migration would silently drop about 230 links while the
  documentation staleness check stays green.
