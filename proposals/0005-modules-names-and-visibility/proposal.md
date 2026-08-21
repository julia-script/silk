# SLP-0005: Explicit modules, names, and visibility

SLP: 0005
Status: Draft
Revision: 4
Author: Julia Ortiz
Created: 2026-08-19
Updated: 2026-08-19
Discussion: —
Review record: —
Depends on: SLP-0003
Split from: —
Split into: —
Supersedes: —
Superseded by: —
Revisit when: Re-export syntax — when Silk's native library/package system design begins
Resolution: —
OpenSpec handoff: —

## Summary

Confirmed Draft direction: each Silk source file is one module whose stable identity comes from its
case-sensitive, source-root-relative logical path. Imports create explicit compile-time bindings;
they execute no code and expose no name that they do not state. Declarations are private to their
defining module unless marked `pub`, collisions are errors rather than order-dependent shadowing,
and aliases are the ordinary way to resolve imported-name conflicts. Only language bindings are
implicit; every standard-library actor namespace is imported explicitly.

## Problem and evidence

Silk already supports dotted namespace imports, selective imports, namespace and member aliases,
private-by-default declarations, qualified lookup, and import cycles. Those rules are spread across
source resolution, module closure, declaration indexing, name resolution, tooling, and bootstrap
standard-library specifications. Current rules also grant implicit prelude namespaces, reject
semantically harmless aliases, and forbid importing one target module more than once. Those rules
conflict with the confirmed explicit-binding model.

A programmer needs to predict a name from the source in front of them: which file owns it, whether
it is local or imported, whether qualification is required, which alias resolves a collision, and
whether another module may access it.

## Driving examples: current and desired

### Case: Compose two modules without hidden bindings

#### Intent

Define a public `User` type and constructor in `model/User.silk`, then use both through one explicit
module namespace in `app/Main.silk`.

#### Current Silk

```silk
// model/User.silk
pub struct User { pub id: i32 }

pub fn make(id: i32) -> User {
  return User { id: id }
}

// app/Main.silk
import model.User

pub fn main() -> i32 {
  let user = User.make(42)
  return user.id
}
```

The current compiler can resolve this shape, but the complete programmer-facing identity,
collision, visibility, and import-effect rules are not gathered in one language contract.

#### Desired Silk

```silk
// model/User.silk
pub struct User { pub id: i32 }

pub fn make(id: i32) -> User {
  return User { id: id }
}

// app/Main.silk
import model.User

pub fn main() -> i32 {
  let user = User.make(42)
  return user.id
}
```

The source remains unchanged. `model/User` is the module identity, `User` is the local namespace
binding created by the import, and only public declarations are accessible through it.

#### Observable result

The program returns `42`. Renaming the file changes the module identity and requires imports to
name the new path. Merely importing a module performs no runtime work.

#### Boundary case

```silk
// model/User.silk
fn secret() -> i32 { return 42 }

// app/Main.silk
import model.User

pub fn main() -> i32 {
  return User.secret()
}
```

The qualified name finds a private declaration but cannot access it; the diagnostic must distinguish
inaccessibility from an unknown member.

### Case: Make standard-library dependencies visible in source

#### Intent

Use an ordinary standard-library actor while keeping every non-language name traceable to source.

#### Current Silk

```silk
pub fn main() -> i32 {
  let value = Option.some<i32>(42)
  drop value
  return 42
}
```

The current compiler lexically recognizes `Option.` and silently loads and injects the manifest
namespace even though the module declares no import.

#### Desired Silk

```silk
import silk.option as Option

pub fn main() -> i32 {
  let value = Option.some<i32>(42)
  drop value
  return 42
}
```

#### Observable result

The program returns `42`, and `Option` has one visible source origin. Removing the import makes
`Option` unresolved and permits tooling to offer an explicit auto-import edit.

#### Boundary case

```silk
pub fn identity(value: i32) -> i32 {
  return value
}
```

Foundational type names remain language bindings and need no import. Calling ordinary functions from
the `silk/i32` actor does require `import silk.i32`.

### Case: Treat import organization as style rather than semantics

#### Intent

Use one module both as a qualified actor and as the owner of a frequently named type.

#### Current Silk

```silk,ignore
import model.User as UserApi
import model.User { User }
```

The current compiler rejects the second declaration with `MOD0003`. It also rejects an unchanged
alias such as `import model.User as User` with `SEM0013`.

#### Desired Silk

```silk,ignore
import model.User as UserApi
import model.User { User }

fn reset(user: User) -> User {
  return UserApi.withId(move user, 0)
}
```

#### Observable result

Both declarations resolve canonical module `model/User`; `UserApi` and `User` are distinct local
bindings. Tooling may offer to combine them into one hybrid import without changing meaning.

#### Boundary case

```silk,ignore
import text.Parser { parse }
import binary.Parser { parse }
```

This remains invalid because the declarations create different bindings with the same local
spelling. Redundancy is harmless; ambiguity is not.

## Goals and non-goals

### Goals

- Define source-file module identity independently from host filesystem accidents.
- Define namespace, selective, aliased, and qualified imports.
- Define local collisions, cross-module visibility, and cycle behavior.
- Define whether imports re-export names or otherwise alter runtime behavior.
- Define the boundary between implicit language names and explicitly imported standard-library
  actors.
- Give unresolved, inaccessible, colliding, and malformed names distinct diagnostics.

### Non-goals

- Define package publication, dependency version solving, or package registries.
- Complete generic, interface, or specialization lookup rules.
- Define compile-time constant evaluation or runtime module initialization.
- Treat compiler artifact reuse or invalidation as programmer-visible module semantics.
- Define re-export syntax before the native library/package model establishes its public-surface
  requirements.

## Current language model

The compilation root and transitive dotted imports determine a reachable module closure. Canonical
module identities are case-sensitive, extensionless slash-separated paths. Each module has a flat
top-level namespace composed from local declarations, explicit import bindings, selected prelude
namespaces, and sealed language bindings. Public declarations may be resolved across module
boundaries; private declarations remain local. Import cycles are permitted.

## Proposed language model

The confirmed foundation keeps path-derived identity, explicit import bindings, static namespace
qualification, and cycle-safe lookup. Namespace imports expose every public declaration of their
target through qualified lookup without creating unqualified member bindings. Bindings are private
unless their declarations are explicitly public; public contracts cannot expose private nominal
types. Harmless import redundancy is accepted, while different bindings claiming one local name are
rejected. Only language bindings are implicit. Ordinary imports never re-export; explicit
re-export syntax is deferred until the native library/package model is designed.

## Worked language experience

General examples prefer namespace imports and qualified actor calls such as `User.make`. Selective
imports remain available when a type or other declaration is clearer unqualified. A module may
import one target more than once to obtain both shapes, or express both in one hybrid import.

The compiler distinguishes an unknown member from a known private member. Import aliases resolve
collisions but never change canonical identity. Redundant aliases and exact duplicate imports are
valid no-ops that tooling may simplify.

Standard-library modules use the same visible source rule: `import silk.effect as Effect` exposes
the ordinary `Effect` actor, while `Effect<A ! E ? R>` remains language type syntax. Auto-import
inserts source rather than creating a hidden prelude binding.

## Semantic sketch

- One source file defines one module.
- Source text does not declare or override its module identity.
- An import path identifies one module and creates only its stated local bindings.
- Namespace qualification does not imply runtime dispatch or a method call.
- Local declarations are visible throughout their module regardless of source order.
- Cross-module access requires a public declaration and a visible import binding.
- Name conflicts are diagnosed; source order never chooses a winner.
- Import cycles do not fail merely for being cycles.
- Redundant aliases and repeated target imports are valid when their resulting bindings agree.
- Foundational types and sealed language namespaces are implicit; standard-library actor namespaces
  require explicit imports.
- Imports are private bindings and do not re-export declarations.

## Compiler–standard library boundary

### Compiler necessity

Module identity, source resolution, lexical bindings, visibility, and canonical declaration
identity are compile-time language operations and cannot be supplied by an ordinary runtime library.

### Smallest target-neutral primitive

No new source-callable intrinsic is provisionally required. The compiler needs only ordinary
module-loading and static name-resolution machinery.

### Standard-library construction

Standard-library modules use the same declaration, import, alias, collision, and visibility rules
as project modules. The toolchain may reserve and resolve their `silk/` source origin, but source
must import their actor namespaces explicitly.

### Privilege audit

The standard library may have reserved source identities supplied by the toolchain, but semantic
analysis must not inject or recognize an ordinary library declaration by name. Module closure
follows explicit imports rather than lexically scanning `Namespace.` spellings against a manifest.
The manifest may still support source resolution and auto-import discovery. Sealed language
namespaces such as `Intrinsic` remain language bindings rather than library imports. Closed
foundational type syntax may coexist with an explicitly imported same-spelled actor namespace; this
does not grant ordinary declarations separate type and value scopes.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Affected | Module paths, imports, aliases, qualification, collisions, and visibility are central. |
| Types and abstraction | Affected | Nominal identity and cross-module interface/type lookup depend on canonical declarations. |
| Execution contracts | Not affected — imports are static | Importing does not construct or run an Effect. |
| Ownership and resources | Not affected — bindings name declarations | Imports do not move, borrow, allocate, or clean values. |
| Runtime and targets | Not affected — module lookup is erased | Source origin may differ, but runtime semantics do not. |
| Compiler | Affected | Parsing, closure loading, declaration indexing, and name resolution implement the model. |
| Standard library | Affected | Reserved source origin remains, while implicit manifest namespaces become explicit imports. |
| Tooling and diagnostics | Affected | Completion, auto-import, navigation, rename, and collision diagnostics depend on exact bindings. |
| Learning and use | Affected | Programmers need one local rule for where every visible name came from. |

## Scope cohesion

Module identity, imports, qualification, visibility, and collisions form one thesis because each
answers whether a name denotes one canonical declaration at a source location. Package management
and generic specialization solve independent problems and remain separate.

## Complexity and subtraction budget

Prefer explicit bindings and deterministic errors over wildcard imports, order-sensitive shadowing,
implicit re-exports, runtime module values, or multiple lookup tiers programmers must memorize.

## Surface displacement

The Draft removes implicit standard-library namespace injection, `SEM0013` redundant-alias errors,
and `MOD0003` repeated-target errors. It preserves the reserved `silk/` source origin, explicit
binding collisions, and the absence of implicit re-exports.

## Drawbacks and risks

- Path-derived identity couples declaration identity to source organization.
- A flat namespace can require aliases when type, function, and module names collide.
- Strict explicit imports can add ceremony for frequently used standard-library actors.
- Permitted cycles may complicate initialization if runtime module state is ever introduced.

## Alternatives and prior art

### Status quo

Keep the distributed bootstrap rules as the de facto language. This avoids immediate design work
but leaves implicit-prelude and repeated-import behavior difficult to explain.

### Smaller primitive or library solution

Treat every source as one global namespace and use naming conventions for separation. This removes
imports but loses isolation, canonical ownership, and scalable collision handling.

### Strongest competing language model

Let source declare nested modules, allow wildcard imports and local shadowing, and treat modules as
runtime initialization units. This is expressive but adds several lookup and execution models.

## Falsifiers and acceptance blockers

- A common real program that requires names to resolve differently based on import order would
  falsify deterministic collision rejection.
- If path-derived identity makes ordinary refactoring semantically unsafe beyond import updates,
  the identity model needs revision.
- If explicit standard-library imports make foundational programs depend on circular imports that
  cannot be expressed under the ordinary closure model, the prelude boundary needs revision.

## Open realization questions

- Assign or reuse the ordinary unknown-name diagnostics for an omitted standard-library import in
  every value, type, and qualified-operation position.
- Decide whether redundant-import tooling uses diagnostics, hints, or source actions; it must not
  affect compilation.

## Future directions

Package manifests, dependency aliases, generated modules, and compile-time module reflection remain
possible future work. Explicit re-exports should be designed when Silk's native library/package
system establishes requirements for curated public surfaces, nested namespaces, and identity-
preserving facade modules.

## OpenSpec realization map

| Slice | Required reconciliation |
| --- | --- |
| Module identity and closure | Preserve exact root-relative identities, transitive explicit imports, deterministic cycles, and reserved `silk/` source resolution. |
| Import bindings | Preserve namespace, selected, aliased, and hybrid bindings; permit repeated targets and redundant aliases. |
| Name resolution | Preserve one flat explicit module scope, source-order independence, collision rejection, and canonical qualified lookup. |
| Visibility | Preserve private defaults, explicit `pub`, inaccessible-versus-unknown outcomes, and public-contract visibility closure. |
| Standard-library boundary | Remove manifest namespace injection and lexical actor scans; retain explicit standard-library imports and auto-import discovery. |
| Publication boundary | Preserve non-re-exporting ordinary imports and leave explicit re-export syntax outside this handoff. |
| Tooling and diagnostics | Retire `SEM0013` and `MOD0003` as errors; retain simplification actions as non-blocking tooling and add missing imports through source edits. |

## Revision and decision record

| Revision | Date | Change or decision |
| --- | --- | --- |
| 1 | 2026-08-19 | Initial Draft with path-derived identity, explicit bindings, private-by-default visibility, and deterministic collision rejection as the provisional thesis. |
| 2 | 2026-08-19 | Confirmed module identity, root-relative closure loading, static cycle-safe imports, namespace and selective binding forms, and namespace-qualified operations as the preferred documentation style. |
| 3 | 2026-08-19 | Confirmed source-order-independent top-level lookup, one flat module namespace, deterministic collision rejection, aliases, private-by-default declarations, distinct inaccessible-member diagnostics, and public-contract visibility closure. |
| 4 | 2026-08-19 | Confirmed harmless import redundancy, repeated target imports, explicit standard-library actor imports, and non-re-exporting ordinary imports; deferred explicit re-exports until native library/package design. |
