## Context

The preceding changes provide function declarations and module-owned values but no executable bodies. Zig's WipFunction mutates local arrays and requires an explicit finish; leaking or abandoning it leaves lifecycle obligations to the caller. This design retains local efficient mutation while making commit and rollback part of one Effect operation.

## Goals / Non-Goals

**Goals:**

- Make incomplete function state unobservable and failure atomic.
- Preserve LLVM SSA numbering, forward references, block identity, and relative bitcode operands.
- Validate local structural and type rules as early as practical.

**Non-Goals:**

- Implement memory, atomic, vector-specific, inline-assembly, or debug operations assigned to later changes.
- Replace LLVM's verifier with a whole-program semantic verifier.
- Allow editing a committed function body in place.

## Decisions

### Build bodies through one scoped callback

Function.buildBody accepts a function handle and an Effect-producing callback that receives an opaque FunctionBody draft. Arguments, blocks, and instructions are created through data-first sibling functions. The draft is invalidated when the callback exits. On success, final validation runs and the immutable body snapshot commits under the module gate; on failure, the draft is discarded.

Alternative considered: expose init, finish, and deinit like Zig. Manual lifecycle APIs are easy to misuse and provide no atomic rollback.

### Keep draft mutation local to one fiber

A body draft owns compact mutable instruction, block, name, extra-data, and value-index tables and records the creating fiber's ownership token. Operations on the draft are synchronous state changes wrapped in Effect.fn. The package rejects concurrent use of one draft; callers can build different functions concurrently because drafts share module state only when interning module values or committing.

### Represent instructions as exhaustive tagged records

Each instruction stores a semantic tag and a tag-specific payload. Opcode, result type, terminator, text, and bitcode behavior are derived from exhaustive tables colocated with FunctionBody, not scattered boolean combinations. Fast-math variants required by this slice carry an explicit flag value rather than duplicating public methods.

### Validate incrementally and at commit

Operation-level checks cover owner, operand type, result type, signature, cursor, and already-terminated blocks before allocating an instruction. Commit-level validation covers block termination, predecessor relationships, phi incoming coverage and types, unresolved forward values, and return compatibility. Failures include operation and entity context in SilkError.

### Model phi construction as a bounded draft

Creating a phi reserves its result value and returns an opaque phi handle owned by the body. Incoming pairs may then reference forward values. Finalization seals the incoming list; body commit rejects unsealed phis or mismatched predecessor sets. This preserves the ability to form loops without exposing raw extra arrays.

### Derive relative bitcode indices from the finalized snapshot

The committed body stores semantic operands and stable instruction order. The bitcode adapter calculates module offsets, function-local value indices, signed phi offsets, and block indices during encoding. The builder does not store bitcode-relative numbers in public values.

## Risks / Trade-offs

- [A long body callback holds substantial draft memory] → Keep compact numeric arrays internally and release all references immediately after commit or failure.
- [Validation differs from the LLVM verifier] → Treat local checks as safety feedback and continue to run LLVM verification on emitted fixtures.
- [Concurrent builds can commit in scheduler-dependent order] → Serialize commits and define output determinism in terms of committed order.
- [Forward references complicate errors] → Preserve symbolic local handles until final indexing and report every unresolved handle at commit.

## Migration Plan

Apply after add-llvm-types-and-declarations. Add core instruction families in dependency order, with a complete renderer, encoder, and fixture for each family before moving on. Function declarations without bodies remain valid throughout. Rollback removes body-building exports while retaining declaration-only modules.
