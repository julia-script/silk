# Behavior and guarantees

This reference records package-wide runtime behavior. Individual actor options and operation-level
validation are described in their exported TSDoc.

## Builder defaults

`Builder.make()` creates one module with these defaults:

| Option | Default |
| --- | --- |
| `strip` | `true` |
| `moduleName` | empty bytes |
| `sourceFilename` | empty bytes |
| `targetTriple` | empty bytes |
| `dataLayout` | empty bytes |
| `moduleAssembly` | no fragments |

`moduleName`, `sourceFilename`, `targetTriple`, `dataLayout`, and `moduleAssembly` accept a
`ByteString`, `Uint8Array`, or JavaScript string. String inputs are UTF-8 encoded. An invalid data
layout fails builder construction with `LlvmError`.

## Ownership

Every type, attribute, constant, global, function, metadata node, block, and local value belongs to
the builder that created it. A consuming operation validates this owner before mutation. A handle
from another builder fails with `LlvmError`.

Blocks and local values additionally belong to one function-body draft. They cannot be consumed by
another function body, by a child fiber, or after the body callback closes.

## Serialized mutation

Each builder has one Effect semaphore. Module mutations pass through that gate and commit in
serialized order. Concurrent fibers cannot overwrite a committed update with an older snapshot.

## Function-body transactions

`Function.buildBody` creates a mutable draft scoped to its callback and creating fiber. The draft
commits only when all of these conditions hold:

- the callback succeeds;
- every basic block has a valid terminator;
- operands and results satisfy instruction type rules;
- branches, switches, and PHI nodes are complete and internally consistent;
- calls and returns match their function types.

A callback failure, validation failure, defect, or interruption discards the whole draft, closes
its handles, releases the reservation, and leaves the function as a declaration. The original exit
is preserved and a valid body can then be retried. A committed function body cannot be replaced.

## Interning and declaration identity

Types, attributes, constants, and uniqued metadata reuse an existing identity for the same semantic
value in one builder. Distinct metadata constructors allocate a distinct identity.

Compatible repeated function declarations return the existing function identity. Variables,
aliases, and functions share one ordered global name table; incompatible declarations and
cross-kind name collisions fail without insertion.

## Debug stripping

Debug information is stripped by default. With `strip: true`, debug constructors return
`undefined`, debug attachments retain nothing, and output contains no dangling debug state. With
`strip: false`, debug metadata is retained and emitted.

`Metadata.forward` creates a placeholder for a recursive graph. It can be resolved exactly once
and only to a value in the same builder and compatible metadata category. Output fails for an
unresolved forward reference only when that reference is reachable from named metadata or an
attachment.

## Errors

Public validation failures use `LlvmError` in the Effect error channel.

| Field | Meaning |
| --- | --- |
| `_tag` | Always `LlvmError`. |
| `operation` | Actor operation that rejected the state or input. |
| `message` | Human-readable failure context. |
| `reason` | `InvalidInput` with `input`, `InvalidState` with `state`, or `WrappedFailure` with `cause`. |
| `cause` | Present only for `WrappedFailure`, preserving genuine JavaScript causal ancestry. |

Expected validation never crosses the public boundary as a throw. Unexpected exceptions remain
defects, and private renderer or encoder exceptions are translated once into `WrappedFailure` at
their public Effect boundary.

## Output

`IrText.render` returns a deterministic LLVM assembly `string`. `Bitcode.encode` returns a
deterministic `Uint8Array` beginning with the LLVM bitcode magic bytes `42 43 C0 DE` in hexadecimal.
Both operations snapshot the same semantic module state and do not consume or mutate the builder.

The runtime does not read or write files, spawn LLVM tools, optimize modules, execute a JIT, emit
object files, or link programs.
