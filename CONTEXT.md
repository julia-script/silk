# Silk Effect Language

The language-design context for Silk Effect, from its initial bootstrap subset through later
self-hosted implementations.

## Language

**Silk Effect**:
A low-level systems programming language prioritizing explicit control, memory safety, predictable
performance, and suitability for systems software. Effect informs parts of its semantic model;
interoperability with Effect is an important later convenience, not the language's purpose.
_Avoid_: native Effect, systems Effect

**Bootstrap language**:
The smallest credible subset of Silk Effect whose reference program is the compiler itself; a
feature belongs in this subset when the compiler or its conformance tests require it. It is a
coherent subset of the intended language rather than a disposable dialect, although early
implementations may impose restrictions and alpha discoveries may still justify breaking changes.
_Avoid_: v0 language, minimal language

**Direct WebAssembly backend**:
A compiler backend that emits WebAssembly without passing through LLVM. It is a stretch path for
reducing Silk Effect's long-term dependence on LLVM, not a prerequisite for the bootstrap language.
_Avoid_: custom Wasm backend, LLVM-free compiler

**Self-hosting compiler**:
A compiler written in Silk Effect that can compile its own source into a working native compiler
without Node.js or TypeScript at runtime. The first self-hosting compiler may emit LLVM IR and use
the LLVM toolchain for code generation and linking.
_Avoid_: frontend-only self-host, LLVM-independent compiler

**Safe code**:
Code outside an explicit unsafe boundary. Safe code cannot cause undefined behavior, use-after-free,
double-free, invalid aliasing, or data races; its resources are released deterministically without
requiring a tracing garbage collector.
_Avoid_: managed code, garbage-collected code

**Unsafe boundary**:
A small, explicit region that may perform operations whose safety invariants the compiler cannot
prove, such as low-level foreign-function or memory access. Unsafe behavior does not implicitly
spread into ordinary code.
_Avoid_: escape hatch, unchecked mode

**Allocation requirement**:
A typed capability in a function signature indicating that the function may perform dynamic
allocation. Allocation requirements propagate through callers and are satisfied by an allocator
provided through the service environment; ownership, lifetimes, and scopes determine reclamation
without ordinary code calling `free`.
_Avoid_: allocator parameter, manual allocation

**Function contract**:
The type-level description of what a function returns, which typed failures it may produce, and
which service capabilities it requires. Failures and requirements propagate through callers; a
pure function has neither.
_Avoid_: effect wrapper, hidden effects

**Type row**:
An unordered, duplicate-free set of nominal types used to describe the failures or service
requirements in a function contract. A row is not an ordered list, tagged record, or key-value map.
_Avoid_: type list, dependency map

**Requirement row**:
A type row naming the service capabilities required by a function. It exists in the function
contract at compile time and does not itself create a runtime union or require runtime tag checks.
_Avoid_: dependency union, service map

**Failure row**:
A type row naming the error types a function may fail with. When a failure occurs, its value has one
active member and can be discriminated by that member's nominal type.
_Avoid_: error list, error codes

**Type union**:
A value type written as a set of alternatives such as `A | B`. A union value has one active nominal
member and an implicit discriminant, allowing exhaustive matching without user-defined tag-field
names.
_Avoid_: tagged record, variant map

**Service capability**:
A nominal compile-time interface named in a function's requirement row. Implementations declare
conformance explicitly; provisioning supplies a runtime value through statically known arguments or
environment slots rather than runtime tag lookup. Allocation is an ordinary service capability.
_Avoid_: global service, injected object

**Service implementation**:
A runtime value satisfying a service capability's interface. Implementations are replaceable even
though the required contract and its provision path are checked at compile time.
_Avoid_: dependency tag, service singleton

**Effect reference model**:
The TypeScript Effect library as a semantic reference for how programs compose through typed
failures, requirements, services, scopes, concurrency, interruption, and observability. Silk Effect
does not target Effect API parity, source compatibility, identical runtime behavior, or Effect
integration as a primary goal.
_Avoid_: Effect port, native Effect runtime
