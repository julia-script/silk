# Define modules, visibility, imports, and name resolution

Type: grilling
Status: resolved

## Question

What is the smallest explicit multi-file module system that can organize the self-hosted compiler,
including module identity, imports, visibility, namespaces, initialization rules, cycle policy, and
deterministic name resolution without introducing a package manager or build system? Imports must
not silently change the abilities or method set of an existing type; extensions from separate files
must remain explicit at their use site.

## Answer

One source file defines exactly one source module. A module's canonical identity is the
case-sensitive, extensionless, slash-separated path of that file relative to the compilation's
source root. Source text does not contain an independent module declaration, modules cannot be
reopened or assembled from partial files, and moving a file deliberately changes the identity of
its declarations. Canonical nominal identity therefore consists of the canonical module identity
and the declaration name rather than a filesystem-dependent absolute path.

A compilation request supplies one source root and one root-module identity. The compiler interns
the root module, follows imports transitively, and compiles only that reachable closure; unrelated
files below the source root are not part of the compilation. Import paths are logical module
identities rather than operating-system paths. Absolute paths, source extensions, `.` and `..`
segments, alternate identities obtained through symlinks, and casing that does not exactly match
the canonical identity are invalid. Importing the current module is rejected as redundant. The
concrete command-line spelling and standard-library source-root arrangement remain for issues 07
and 09.

Every module owns one flat top-level member namespace shared by types, interfaces, functions,
constants, and import bindings. Top-level names are unique regardless of declaration kind. Modules
and types do not introduce nestable or reopenable source namespaces, types cannot own methods or
nested declarations, and there is no specially collapsed principal declaration. A module may
contain private supporting declarations even though its public design should remain centered on
one actor. Consequently a `Target` module containing a `Target` type and `matches` function exposes
them as `Target.Target` and `Target.matches` through a namespace import.

Imports are unconditional top-level declarations with three binding outcomes. A namespace import
binds the imported module under its final path segment by default or under an explicit changed
alias. A selective import binds an explicit finite list of public module members, each under its
declared name unless the member import gives it a changed local alias. A hybrid import performs
both outcomes in one declaration: it binds the module namespace and selected members. An alias
identical to the default name is redundant and invalid. A source file may name a canonical module
in at most one import declaration. Namespace bindings, selected members, and local top-level
declarations share the same namespace, so every collision is an error. Wildcard imports, implicit
preludes, side-effect-only imports, re-exports, local imports, and conditional imports are excluded
from bootstrap. Issue 08 owns their concrete spelling.

Name resolution never uses overload selection, declaration-kind priority, import order, or source
order to choose between candidates. Top-level declarations are order-independent. Local bindings
are visible only after their declaration, and no parameter, local, pattern binding, import, or
top-level declaration may shadow another visible binding in an overlapping scope. The same name may
be reused in disjoint scopes such as separate match branches. Field names do not conflict with
lexical bindings because field access is explicitly qualified through a value. An unqualified name
therefore resolves to its sole visible binding, while a qualified module access begins with a
visible module alias and resolves exactly one member of that module.

Top-level declarations and struct fields are private by default. One explicit public modifier makes
a declaration or field accessible to importing modules. There is no package, directory, protected,
or friend visibility. A public function contract, public field, public alias, or public interface
cannot expose a private type through an externally nameable position. Private struct fields may use
private types because their representation remains compiler-visible while their names remain
module-private. All operations declared by a public interface belong to that public contract rather
than having separate per-operation visibility.

Imports never activate behavior. Behavior defined outside a nominal type's source module is an
ordinary data-first function and must be called through an explicitly imported namespace binding or
selected function binding. Importing its module does not add a method, operator, overload candidate,
or conformance to another type. A conformance remains legal only in the nominal type's defining
module. A public conformance is a canonical fact about that type and is available wherever the type
is used; a private conformance is usable only inside its defining module. Conformances are not
independently imported. A third-party type/interface combination uses a nominal adapter type owned
by the adapting module.

Cyclic module imports are valid, including cycles spanning more than two modules. A canonical
module is interned once, and importing a module performs no initialization. The compiler discovers
the reachable file closure and collects top-level declaration headers independently of file order
before resolving the declaration dependencies demanded by program use. Canonical module and
declaration identity determines stable work and diagnostic ordering where an implementation needs
a tie-breaker; traversal order cannot affect program meaning.

An import cycle does not excuse an irreducible semantic dependency cycle. Mutually recursive
functions are valid when they obey the explicit-contract rule from issue 03, and recursive nominal
types are valid when `Box`, another finite indirection, or stable IDs make their layouts finite.
The compiler rejects and reports the complete declaration path for an inline type-layout cycle, a
transparent-alias cycle with no nominal result, cyclic constant evaluation, contract inference that
depends on its own unfinished result, or any equivalent dependency that requires an incomplete
value to finish itself. This follows the useful boundary verified in Zig while preserving Silk
Effect's one-file module identity.

Finally, source modules are inert declaration namespaces rather than runtime initialization units.
Bootstrap permits side-effect-free compile-time constants but no top-level mutable variables,
runtime-owned global values, initialization blocks, deinitialization hooks, or import-time code.
Constant evaluation is order-independent and memoized by declaration identity; an initializer may
not fail, require a service, or create a runtime resource needing cleanup. Runtime state is
constructed explicitly by functions and owned within ordinary lexical scopes, normally beneath the
native entry path. Issue 07 owns the exact constant-expression and static-data subset.

All import and declaration syntax above is semantic notation only. Issue 08 owns concrete spelling.

## Amendment — 2026-08-05

The import model now admits namespace, selective, and hybrid bindings in one explicit declaration.
The canonical path-derived module identity remains unchanged: case must match exactly even on a
case-insensitive filesystem, and moving or renaming a file deliberately changes its declarations'
canonical identities.
