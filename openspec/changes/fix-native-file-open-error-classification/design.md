## Context

See proposal.md. Generated file-open currently combines syscall failure and a logical kind check.
The existing filesystem test file already compiles generated C boundary fixtures.

## Goals / Non-Goals

Preserve the existing numeric reason/native-code ABI and ordinary provider mapping. Do not migrate
providers or redesign filesystem lifecycles.

## Decisions

Use separate branches for failed fstat and successful nonregular metadata. Save errno before close
only for the failed call; use the protocol WrongType path for the successful kind check. Clearing
errno before fstat would retain the incorrect dependency on error state after a successful call.

Compile the actual generated operation with controlled syscall replacements. Inspect structured
reason/code outputs and descriptor-close records, including cleanup that overwrites errno.

## Risks / Trade-offs

A fixture could accidentally bypass the faulty operation. Inject only external syscalls and invoke
the generated file-open ABI, preserving its metadata and cleanup code verbatim.
