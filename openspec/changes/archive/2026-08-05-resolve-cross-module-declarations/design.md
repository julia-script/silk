## Context

See `proposal.md` for motivation and the delta specs for observable behavior.

The current compiler already has most of the correct phase boundaries, but they do not yet connect:

- `ModuleClosure.load` parses a root-driven closure, but an import contains only one identifier and
  uses that spelling directly as the source-map key.
- `DeclarationIndex.collect` assigns canonical `(module, name)` identities across the closure, but
  `Elaboration.elaborateModule` calls `collectModule` again and resolves calls only among that
  recollected module-local array.
- `Analysis.make` constructs the closure and index, then elaborates every syntax file without
  passing either shared artifact to elaboration.
- HIR, instance keys, lowering, and both backends already carry canonical declaration identities.
  `Instances.discover` can follow a target into another module's HIR map once elaboration produces
  such a target, so no cross-module runtime call representation is missing.

The design must preserve lossless syntax and bounded recovery, explicit unavailable facts,
canonical deterministic ordering, cycle-tolerant header collection, facade-only tooling, actor
modules, and the repository's no-compatibility policy for unreleased APIs.

## Goals / Non-Goals

**Goals:**

- Turn the accepted namespace, selective, aliased, and hybrid imports into one immutable
  closure-wide binding artifact.
- Make the existing declaration index the only header authority used by body elaboration and every
  downstream canonical reference.
- Resolve private local calls and public imported calls across both acyclic and cyclic module
  graphs without module initialization or per-module codegen units.
- Establish a flat binding representation that later declaration kinds can join without replacing
  the resolver.
- Keep all binding, conflict, visibility, and failed-reference facts inspectable and deterministic.

**Non-Goals:**

- Loading source files from disk, resolving symlinks, choosing a source-root CLI, packages,
  dependency solving, or standard-library placement.
- Struct/type dependency resolution, re-exports, wildcard imports, implicit preludes, local or
  conditional imports, and runtime module values.
- A general declaration dependency solver. This slice resolves function headers, whose contracts
  are already explicit; later aliases, structs, constants, and generics will add their own finite
  dependency rules.
- Changing MIR or backend call operations, adding operator syntax, or preserving the current
  phase-local compiler API.

## Decisions

### 1. Concrete dotted imports map to canonical slash identities

The concrete tree will represent an import path as ordered identifier segments and dot tokens.
`ModuleClosure` derives the canonical target by joining available segments with `/`:
`compiler.Syntax` becomes `compiler/Syntax`. The source map and `rootModule` continue to use the
canonical slash form. A request validates all logical identities once before loading; malformed
source syntax remains parser data and does not become a caller error.

This keeps source syntax readable while preserving the path-derived identity already accepted by
Wayfinder. Keeping dotted strings as canonical identities was rejected because it would contradict
the slash-separated identity used by declarations and future source-root loading. Letting the host
filesystem normalize case or separators was rejected because compilation meaning would become
host-dependent.

### 2. Import syntax gets explicit concrete substructure

Add an `as` keyword and concrete nodes for an import path, optional namespace alias, member list,
and member entry. A member entry owns its source member name and optional changed local alias. The
parser retains all dots, braces, commas, trivia, missing tokens, and error regions. Function parsing
accepts `fn` with an optional preceding `pub`; visibility is interpreted only during header
collection.

The existing flat `ImportDeclaration` with a first-identifier search cannot distinguish path
segments from aliases or members and would make recovery-dependent semantic guessing inevitable.
A generic expression-path parser is deliberately not introduced: this change needs import paths
and the existing two-part qualified call only.

### 3. The declaration index becomes the single header authority

`DeclarationIndex.collect(closure)` remains responsible for assigning every canonical declaration
identity in canonical module order. `DeclarationFact.visibility` becomes `Public | Private`, derived
from the concrete `pub` token, and both kinds remain indexed. `Elaboration` no longer invokes
`collectModule`; it receives the exact `ModuleHeaders` selected from the published index.

Reusing the index avoids equal-looking but independently constructed declaration graphs and makes
it mechanically true that import bindings, HIR calls, instance keys, and lowered calls refer to the
same header authority. A compatibility overload for phase-local elaboration was rejected because it
would preserve the architecture defect and allow tests or tools to bypass cross-module semantics.

### 4. `NameResolution` owns module scopes and binding lookup

Introduce one actor module, `NameResolution.ts`, centered on an immutable `Resolution` artifact.
The artifact contains canonically ordered `ModuleScope` values. A scope contains interpreted import
facts, local and intrinsic bindings, imported namespace/member bindings, conflicts, and diagnostics.
Bindings are a closed union with a common local spelling and provenance:

- `LocalDeclaration` targets one indexed canonical declaration and retains visibility.
- `IntrinsicActor` represents the language-owned `I32` and `Bool` actor namespaces.
- `ModuleNamespace` targets one canonical imported module.
- `ImportedMember` targets one public indexed declaration and retains its source member spelling.
- `Unavailable` retains damaged syntax or a diagnostic cause without a fabricated target.

Public operations resolve unqualified and qualified names against a `ModuleScope`, returning closed
`Resolved`, `Missing`, `Inaccessible`, `Conflict`, or `Unavailable` outcomes. Resolved declaration
lookups always return the indexed `DeclarationFact`; they do not return a copied signature.

Putting bindings inside `ModuleClosure` was rejected because closure loading only answers which
syntax artifacts exist; member visibility requires the completed declaration index. Putting lookup
inside `Elaboration` was rejected because inspector tools and future type resolution need the same
facts independently of a particular body walk.

### 5. Scope construction is multi-pass but non-recursive

`NameResolution.resolve(closure, index)` uses a deterministic pass sequence:

1. Seed each module scope with intrinsic actors and every canonical local declaration.
2. Interpret each resolved import's default namespace, explicit namespace alias, and selected
   member clauses against the complete index.
3. Diagnose duplicate target imports, redundant aliases, unknown or private members, and malformed
   clauses while retaining explicit invalid import/member facts.
4. Group candidate bindings by local spelling, publish every multi-candidate conflict, and make
   lookup through a conflict unavailable.

The pass iterates modules in canonical order and imports/members in concrete order; diagnostics use
the repository-wide deterministic merge. Import cycles require no recursive resolver because
re-exports are absent and all target headers already exist. This is simpler than a dependency
worklist and still supports mutually recursive cross-module functions with explicit contracts.

The later occurrence of a repeated canonical target receives the duplicate-import diagnostic and
creates no bindings. In contrast, ordinary name collisions retain every candidate and choose no
winner, because declaration kind or source order must not affect lookup.

### 6. Intrinsic actors participate in the flat namespace

`I32` and `Bool` remain compiler-known actor namespaces, but `NameResolution` publishes them as
intrinsic bindings in every module. A local declaration or import attempting to reuse either name
therefore produces the same binding-conflict shape as any other shadowing attempt. During qualified
call elaboration, a resolved `ModuleNamespace` leads to an ordinary imported call, while an
`IntrinsicActor` leads to the existing `BuiltinCall` path.

Trying module aliases first and falling back to builtins was rejected because an import could then
silently shadow language-owned actors. Keeping builtins entirely outside the resolver was rejected
because the supposedly flat namespace would have an invisible exception.

### 7. New diagnostics stay in existing phases

Module-target failures remain module diagnostics. Allocate `MOD0003` for a repeated canonical
target import. Binding and visibility decisions are semantic diagnostics, ordered with other
semantic facts:

- `SEM0013`: an explicit namespace or member alias repeats its default name;
- `SEM0014`: a selected or qualified imported member does not exist;
- `SEM0015`: a selected or qualified imported member is private;
- `SEM0016`: two visible bindings claim one module-scope spelling.

Parser damage suppresses these downstream diagnostics when the required path, alias, or member is
unavailable. An unknown qualified name with no import binding retains the existing unknown-actor or
unknown-function behavior appropriate to its concrete call form. Introducing a new diagnostic phase
was rejected because binding resolution is part of semantic analysis and the existing phase order
already places module-target errors before it.

### 8. Elaboration consumes indexed headers plus one module scope

Replace the phase-local entry point with a closure-aware operation that receives a module's syntax,
its indexed headers, and its completed `ModuleScope`. Unqualified call lookup sees local declarations
and selected members; qualified lookup first resolves the namespace/intrinsic binding and then the
member. A successfully imported call becomes the existing HIR `Call` with the target module's
canonical declaration ID. Alias spellings and import facts remain analysis data and do not enter
HIR.

All module results are still published as a canonical-name-keyed map. `Instances.discover`, lowering,
ownership, MIR interpretation, and backends require no new call variant; their work is to accept and
test the now-reachable cross-module identities. The root entry remains the root module's unique
public zero-parameter `I32 main`, never an imported `main`.

### 9. The snapshot owns resolution and the facade exposes it

`Analysis.make` becomes:

```text
closure → declaration index → name resolution → per-module elaboration
        → ownership → instances → layout → MIR
```

The snapshot stores the `Resolution` artifact and merges its diagnostics before body-elaboration
diagnostics. Facade queries expose module scopes, import binding facts, and closed unqualified or
qualified lookups. Tooling receives these values directly and may not reconstruct them from syntax.

The docs add a direct-link `/docs/labs/name-resolution` lab with namespace, selective, hybrid,
private-member, conflict, damaged-import, and cyclic presets. Existing declaration-index views add
visibility, while HIR and instance views link imported calls through canonical IDs. The facade-only
import check grows to cover the new lab and actor.

### 10. End-to-end evidence uses a small cross-module corpus

Add deterministic fixtures for:

- namespace, selective, member-alias, and hybrid calls;
- a private local helper and rejected private imports;
- unknown members, redundant aliases, repeated imports, and flat-scope collisions;
- a diamond import graph with one reachable shared function;
- mutually importing functions reachable from root `main`; and
- equivalent native and WebAssembly programs calling across at least three modules.

Update syntax and HIR goldens where the artifact vocabulary changes. Preserve the existing
single-module corpus unchanged except for fact-shape/API migrations; this makes accidental semantic
regressions visible instead of replacing the old evidence with only multi-module cases.

## Risks / Trade-offs

- **[Risk] Full import syntax substantially widens parser recovery.** → Give paths, aliases, and
  member lists their own concrete nodes and recovery boundaries; add malformed cases before adding
  semantic interpretation.
- **[Risk] Visibility and collision errors cascade into misleading unknown-call diagnostics.** →
  Carry diagnostic causes through invalid bindings and suppress downstream errors when resolution
  already explains the unavailable reference.
- **[Risk] Future structs require type-name lookup as well as calls.** → Keep binding targets and
  lookup outcomes declaration-oriented rather than function-call-oriented; only elaboration's
  current consumer narrows resolved targets to functions.
- **[Risk] Private-by-default changes the shape of every declaration fact.** → Migrate fixtures and
  APIs directly, while retaining `pub` on every existing externally used fixture and entry.
- **[Trade-off] The in-memory source map cannot itself prove filesystem case or symlink identity.**
  → Enforce exact canonical logical keys now; the future source-root loader owns filesystem
  canonicalization and rejection before constructing the request.
- **[Trade-off] No general dependency solver lands with cyclic imports.** → Function headers are
  complete before resolution, so the current slice needs no recursive semantic computation. Later
  type/constant changes must add dependency handling only when their concrete requirements arrive.

## Migration Plan

1. Expand tokens and concrete syntax, migrate import/parser tests, and retain the old simple import
   as the one-segment namespace form.
2. Add canonical identity validation and multi-segment target derivation to closure loading.
3. Add declaration visibility and migrate every consumer to the closure-wide index.
4. Introduce `NameResolution`, its diagnostics, deterministic encodings or projections, and focused
   unit tests.
5. Change elaboration and `Analysis.make` to consume resolution, then migrate facade and docs labs.
6. Add cross-module instance, MIR, interpreter, LLVM, WebAssembly, driver, and determinism evidence.
7. Run the full repository and packed release-candidate gates; remove superseded phase-local APIs in
   the same change.

Because the compiler API is unreleased, rollback is a normal commit revert rather than a dual API
or data migration. No persisted user data or network protocol changes.
