## Context

The successful compiler path is complete, but learners have not inspected phase failures or the package's second serialization format. Both topics should be taught without derailing the primary `.ll` workflow. See `proposal.md` for motivation.

## Goals / Non-Goals

**Goals:**

- Make compiler failures attributable to lexing, parsing, resolution, or LLVM validation.
- Demonstrate `Function.buildBody` transactional failure semantics.
- Explain and verify bitcode as an alternate serialization.

**Non-Goals:**

- Build a multi-error recovery engine or polished IDE diagnostic renderer.
- Make bitcode the primary tutorial path.
- Execute bitcode in the browser or add server infrastructure.

## Decisions

### Keep phase-specific tagged errors rather than flattening everything into one message

Callers can recover precisely, and `LlvmError` retains its operation/reason contract.

### Render a single human diagnostic at the CLI boundary

The compiler core returns structured errors; formatting and stderr ownership remain application concerns.

### Demonstrate transactional failure by omitting one terminator and retrying

This directly reveals the package guarantee without introducing synthetic faults.

### Teach bitcode as a short side path from the same committed builder

`Bitcode.encode` yields deterministic bytes; Clang consumption is mentioned, but readable IR remains the learning medium.

## Risks / Trade-offs

- [Risk] Failure exercises encourage editing code into an invalid final state → Mark every deliberate break/restore boundary and back it with fixtures rather than the canonical example.
- [Risk] Learners call bitcode bytecode → Use a comparison table and repeat the exact LLVM term.
- [Risk] Error unions become noisy in prose → Show one representative value per phase and defer exhaustive shapes to reference.

## Migration Plan

Add diagnostic rendering/tests, invalid fixtures, bitcode variation, and Lesson 12. These are additive to the successful compiler path and can be removed without changing core semantics.
