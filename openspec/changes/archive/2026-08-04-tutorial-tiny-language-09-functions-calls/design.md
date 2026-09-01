## Context

Expression lowering works only inside one function. The confirmed central program and recursion option require module-level symbol collection, parameter environments, and direct calls. See `proposal.md` for motivation.

## Goals / Non-Goals

**Goals:**

- Support multiple functions, forward references, direct calls, and self recursion.
- Give resolution failures a compiler-owned typed error.
- Preserve the simple all-`i32` language contract.

**Non-Goals:**

- Support overloads, function values, indirect calls, extern declarations, or polymorphism.
- Add a separate general-purpose type checker.
- Permit missing or parameterized `main`.

## Decisions

### Declare all function signatures before building any body

This enables forward and recursive calls and demonstrates the compiler pass boundary. Source-order-only resolution would be simpler but less language-like.

### Store a function table entry with handle, arity, and source definition

The table supplies direct-call lowering and useful duplicate/arity diagnostics without exposing LLVM internals to the AST.

### Use a separate parameter map per body

Parameter and module function scopes remain explicit; `Value.argument` is resolved once at body entry.

### Treat an undefined direct-call result as a typed compiler invariant failure

All Tiny functions return `i32`, so non-null assertions are unnecessary and forbidden.

## Risks / Trade-offs

- [Risk] Two passes feel abstract → Print the declaration table before lowering bodies and reverse definitions in a checkpoint.
- [Risk] LLVM duplicate-name errors obscure Tiny diagnostics → Validate duplicate names before calling `Function.declare`.
- [Risk] Recursion is mistaken for JIT behavior → Explain that recursion is ordinary static function calling in emitted IR.

## Migration Plan

Extend compiler state and diagnostics, add multi-function and factorial fixtures/tests, and add Lesson 9. No changes to the parser contract are required.
