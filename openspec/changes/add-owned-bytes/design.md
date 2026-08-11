## Context

Portable filesystem reads and future text construction need an owned byte sequence. Exposing
`Vector<u8>` directly would couple every domain API to a general-purpose growable collection, while
making `Bytes` compiler-known would violate the minimal intrinsic boundary. Returned lexical borrows
provide the missing mechanism for a source-defined wrapper to expose zero-copy views.

This change therefore depends on `add-returned-lexical-borrows` and adds no compiler primitive.

## Goals / Non-Goals

**Goals:**

- provide a canonical owned sequence of arbitrary octets;
- make allocation, ownership, cleanup, and borrowing explicit in ordinary Silk contracts;
- preserve evaluator, native LLVM, and direct-Wasm parity;
- create a narrow foundation for later filesystem and String APIs.

**Non-Goals:**

- UTF-8 validation, String construction, interpolation, or formatting;
- filesystem-specific errors or path behavior;
- a compiler-known byte collection or specialized backend representation;
- advanced search, splitting, comparison, hashing, or builder APIs.

## Decisions

### Bytes is a nominal wrapper over Vector<u8>

The standard library owns the distinction between generic elements and domain-neutral octets. A
nominal wrapper permits future byte-focused operations without exposing Vector as the public result
of every I/O API, while reusing its allocation and cleanup behavior. The compiler sees an ordinary
struct and ordinary calls.

### The first API is intentionally small

Empty construction, copy, append, length, and shared/exclusive views are enough to receive owned
data, compose it, inspect it, and pass it to effects. Operations that allocate carry
`OutOfMemory ? &mut Allocator`; views and queries do not. More elaborate builders and algorithms can
be justified by actual consumers later.

### Bytes is encoding-neutral

Every `u8` sequence is valid `Bytes`. UTF-8 belongs to a future String abstraction because validation,
character indexing, and formatting are separate policy decisions. Filesystem providers likewise
consume and return bytes without changing their meaning.

### Borrowing delegates to returned Vector views

`Bytes.asSlice` and `Bytes.asMutSlice` return lexical views obtained from the wrapped Vector. They do
not copy or allocate, and the ownership checker ties them to the `Bytes` owner. This is why returned
lexical borrows are a hard prerequisite rather than a convenience.

## Risks / Trade-offs

- Wrapping Vector adds nominal API surface and may initially duplicate a few forwarding functions.
  That cost buys a stable byte-domain boundary and keeps later String behavior separate.
- Append may expose Vector growth details through allocation timing. Only semantic success/failure and
  resulting bytes are specified; capacity remains private.
- The minimal API may need additions soon. New operations remain ordinary source and can be proposed
  without widening compiler privilege.

## Migration Plan

Add the canonical actor and manifest entry after returned-borrow support ships. Convert new portable
APIs to use `Bytes`; no existing public compatibility layer is required during alpha. Rollback removes
the actor and its fixtures because there is no serialized format or compiler special case.

## Open Questions

None. String and formatting deliberately remain separate follow-up planning.
