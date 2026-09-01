# Actor reference

The public API is organized as actor modules. Import actors from their package subpaths so code
states which part of the LLVM model it uses.

```typescript
import * as Builder from '@silklang/llvm/Builder'
import * as Type from '@silklang/llvm/Type'
```

Individual exported declarations, parameters, return values, and error cases are documented in
their TSDoc. This page defines the responsibility and boundary of every public actor.

## Module ownership and output

| Actor       | Responsibility                                                                                                                                                                                                                               |
| ----------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Builder`   | Owns one serialized LLVM module state. Captures module headers, data layout, target triple, initial assembly, and debug-strip policy.                                                                                                        |
| `Bitcode`   | Encodes a builder snapshot as deterministic LLVM bitcode in a `Uint8Array`.                                                                                                                                                                  |
| `IrText`    | Renders a builder snapshot as deterministic textual LLVM IR.                                                                                                                                                                                 |
| `LlvmError` | Tagged error in the typed failure channel of public Effect operations. Contains stable `operation` and `message` fields plus an `InvalidInput`, `InvalidState`, or `WrappedFailure` reason. Only wrapped failures expose JavaScript `cause`. |

## Bytes, layout, and flags

| Actor          | Responsibility                                                                                                                                                           |
| -------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `ByteString`   | Immutable byte-oriented names and strings. JavaScript strings are encoded as UTF-8 conveniences.                                                                         |
| `AddrSpace`    | Unsigned 24-bit LLVM address-space identifiers and textual suffix rendering.                                                                                             |
| `Alignment`    | Power-of-two byte alignments and LLVM alignment encoding.                                                                                                                |
| `DataLayout`   | Parsed target data-layout strings and integer, float, vector, and pointer layout lookup.                                                                                 |
| `FastMath`     | Immutable fast-math flag sets with textual and bitcode forms. `combine` supports data-first and pipeable calls.                                                          |
| `IntegerMath`  | Immutable integer arithmetic flags such as `nsw`, `nuw`, and `exact`; every `with*` transformation supports data-first and pipeable calls.                               |
| `MemoryAccess` | Immutable volatile, alignment, sync-scope, and atomic-ordering settings plus legality validation. `withVolatile` and `withAtomic` support data-first and pipeable calls. |
| `DIFlags`      | LLVM debug-information flags with textual and bitcode forms.                                                                                                             |
| `DISPFlags`    | LLVM subprogram flags with textual and bitcode forms.                                                                                                                    |

## Types and values

| Actor      | Responsibility                                                                                                               |
| ---------- | ---------------------------------------------------------------------------------------------------------------------------- |
| `Type`     | Interned scalar, pointer, array, vector, structure, named structure, and function types. Provides shape and layout queries.  |
| `Constant` | Interned integer, floating-point, aggregate, expression, global-reference, block-address, poison, undef, and zero constants. |
| `Value`    | Function arguments, instruction results, and the shared `Value.Input` operand union used by body operations.                 |

## Module-level declarations

Variables, aliases, and functions occupy one ordered global symbol table.

| Actor       | Responsibility                                                                                                                     |
| ----------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| `Global`    | Shared global-value identity, name, linkage, visibility, section, unnamed-address, DLL storage, preemption, and metadata behavior. |
| `Variable`  | Global variables, initializers, constness, address spaces, thread-local settings, alignment, and debug expressions.                |
| `Alias`     | Global aliases and aliasee validation.                                                                                             |
| `Attribute` | Interned enum, integer, type, string, attribute-set, and function-attribute-set values.                                            |
| `Function`  | Function declarations, declaration configuration, debug subprogram attachment, and transactional body construction.                |

## Function bodies

| Actor          | Responsibility                                                                                                                                               |
| -------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `Block`        | Function-owned basic blocks and the current insertion point.                                                                                                 |
| `FunctionBody` | Instructions, terminators, calls, casts, aggregate and vector operations, memory operations, atomics, PHI and switch construction, and instruction metadata. |
| `Intrinsic`    | The pinned LLVM intrinsic catalog, canonical overload resolution, and named intrinsic helpers.                                                               |

`FunctionBody` is the largest actor because its operations share one lifecycle and validation
boundary: the callback passed to `Function.buildBody`.

That callback receives a draft valid only in its creating fiber and scope. Body construction is
bracketed: success validates and atomically commits, while typed failure, validation failure,
defect, or interruption closes the draft and releases the function reservation without exposing
partial instructions.

## Debug metadata

| Actor      | Responsibility                                                                                                                                              |
| ---------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Metadata` | Metadata strings and nodes, named metadata, debug types, compile units, subprograms, locations, expressions, attachments, and recursive forward references. |

Debug constructors return `undefined` when the builder uses its default `strip: true` mode. Generic
metadata tuples and named metadata follow the same builder ownership rules as every other handle.
