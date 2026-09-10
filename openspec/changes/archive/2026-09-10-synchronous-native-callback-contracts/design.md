## Context

See proposal.md. Work base is `03ec67f665ea8848f173a8b834ed025f9a7346b0`, containing merged JUL-123/124. Export address conversion and direct foreign unwind guards exist. Native indirect application does not: a foreign pointer binding reaches the ordinary callable checker and fails SEM0075. This correction is recorded in JUL-142 before implementation.

## Goals / Non-Goals

Give the admitted synchronous boundary one inspectable behavioral identity and a real indirect call instruction. Keep raw addresses non-owning and unsafe, ordinary source loans authoritative, and C callbacks free of Effect failure channels. No retained registration, thread migration, nullable invocation, captured C closure, permitted unwind, new personality ABI, or LTO admission.

## Decisions

### Separate address behavior from invocation lifetime

Extend the existing `extern "C" fn(P...) -> R` type with an optional `with Intrinsic.foreign(...)` clause. Its normalized contract uses the existing memory/locality/noCapture/borrow/returned/noReturn/forbidden-unwind model. Type clauses identify unnamed parameters with decimal ordinal strings (`"0"`, `"1"`); declarations continue to use names. The type is explicitly nonnull, represented by one address. Nullable forms are rejected; there is no unchecked null-to-call conversion.

Add `callbacks: ("parameter", ...)` to foreign behavioral clauses. Each listed function-pointer parameter promises invocation only synchronously on the calling thread within that call's dynamic extent, without retaining it for later invocation. These fixed admitted obligations are explicit normalized data, not optimizer attributes. Every function-pointer parameter crossing a foreign entry or call must be listed. Omission stays conservative and is rejected because unrestricted callback invocation is unadmitted. Unknown escape/thread/unwind forms diagnose rather than imply a capability.

An exported source function can declare its own foreign behavior using the same clause. Stronger assertions require `unsafe export "C" fn`; the body author owns their truth. Contextual address conversion compares the export's actual declared behavior with the expected pointer type; it never acquires stronger promises from the expected type. Conservative unannotated exports remain available for conservative pointer types. All existing scalar/generic/capture/suspension restrictions remain.

Native pointer reference parameters use per-call lifetime binders, including explicit `for<'a> extern "C" fn(...)` syntax. Lifetime-only exported functions remain eligible: their binders are proof data and erase at runtime. Type/value generics and naked machine exports cannot supply callback addresses. Compatibility alpha-compares lifetime signatures in both directions while keeping behavior invariant.

Reusing one contract avoids parallel callback memory vocabularies. Invocation is attached to the foreign caller's parameter, rather than pretending that a process-lifetime code address has the lifetime of a borrowed context.

### Loans and reentrancy

Borrowed single-value reference parameters in native pointer signatures require explicit `borrow` ordinals and use the existing loan machinery. Each indirect call keeps those loans active for its complete invocation. Nested calls can reborrow according to ordinary reference rules and use independently owned storage; overlapping exclusive/shared access diagnoses at the nested argument. Returning borrowed references is unadmitted.

When an enclosing foreign contract lends references and accepts callbacks, callback access must be argument-local (or memory-none). A callback allowed to access arbitrary external storage cannot establish nonconflict with the enclosing loan and is rejected for that combination. An unsafe foreign declaration owns the truth that callback arguments designate valid storage, that reborrowing respects each active loan, and that no callback outlives the call. No name-based qsort recognition or inferred raw-pointer provenance is added.

qsort continues to receive raw array pointers. Its source declaration explicitly promises synchronous callback invocation; its comparator declares argument-local read access. Its wrapper keeps array storage alive and avoids exposing competing references. The compiler does not infer a loan from Pointer.fromMutSlice or claim to prove arbitrary C writes safe. Analysis tests for nested conflicts use typed references, not disguised raw aliases.

### Native indirect calls and termination

Add a distinct checked foreign-application expression and HIR/MIR operation carrying callee value, complete classified signature, behavioral contract, arguments, result and source origin. This avoids conflating native address invocation with statically specialized Silk callable environments. Calls require unsafe acknowledgement, exact arity and admitted types, and reject unsupported targets before emission.

Generalize the existing native guard to accept a runtime callee address. Its guarded body invokes that value with the exact C signature and target extension attributes. Retain a noinline guard frame and the existing fatal Itanium/DWARF personality and cleanup trap. Export thunks also establish a fatal boundary around the source implementation. A foreign exception cannot traverse Silk and reach an outer C++ catch. Fatal traps do not promise source cleanup; ordinary and typed source wrapper exits do.

### Identity and tooling

Include normalized callback behavior and nonnullness in type equality, inference, substitution, specialization, semantic/module surfaces, C ABI keys, MIR verification, backend caches, interfaces and presentation. Canonical C headers erase non-C behavioral syntax while manifests preserve it. Migrate the manifest schema and consumers together; no legacy decoder. LTO remains rejected by the existing profile gate.

### Authorities and evidence

Reuse JUL-124's pinned LLVM/Clang/LLD 22.1.8, AAPCS64 2025Q1, x86-64 psABI revision, Itanium exception ABI 1.22, Darwin SDK 15.5, and GNU glibc 2.36 supplies. POSIX qsort constrains comparator access and reordering, but does not by itself establish a same-thread promise. Pin the selected Darwin/libc implementation or platform declaration establishing synchronous direct comparator invocation and independently observe thread identity and dynamic extent in the native fixture. Record exact source revisions/digests and deliberate divergences in supplies and verification before claiming conformance.

Zig's std/c declarations and C ABI tests inform exact scalar/pointer signatures; Rust's extern unwind codegen tests distinguish nounwind assumptions from explicit boundary termination. Neither language supplies Silk's checked callback-loan model or serves as the ABI oracle. Reuse the three native lanes and add only distinguishing pointer invocation, nested callback and throw cases. Semantic claims use shared analysis snapshots, C ABI claims use independent C/C++ callers and objects, and target-neutral behavior stays in the existing shared corpus.

## Risks / Trade-offs

- Raw pointer provenance is unknowable → keep unsafe obligations explicit and test loan claims with references.
- Expected types could launder stronger export promises → compare declared normalized contracts before conversion.
- Optimizers could erase the fatal frame → retain noinline/no-tail structure and inspect optimized object unwind data and execution.
- Function type property suffixes can be grammatically ambiguous → parenthesize nested result function types and preserve parser/formatter round trips.
- More conservative callback admission breaks old declarations → migrate every source consumer in this change.

## Migration Plan

Complete and validate planning, implement the one representation, migrate source and interface artifacts, run focused analysis/native checks, then all repository gates. Obtain independent code and test-economics reviews, commit the exact approved diff, push a draft PR and update JUL-142. The work is green-field; rollback is reverting the complete commit rather than retaining dual contracts.
