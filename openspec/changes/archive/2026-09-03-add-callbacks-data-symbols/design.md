## Context

The current compiler has distinct ordinary callable representations, C-ABI classification for
scalar/pointer function signatures, reachable foreign-call inventories, generated native export
thunks, and explicit evaluator/direct-Wasm function bindings. It has no representation for a raw C
function address or a declaration/fact path for C data symbols. See `proposal.md` for motivation.

## Goals / Non-Goals

**Goals:**

- Keep C callback pointers nominally distinct from Silk callables and their captured environments.
- Reuse the generated C export thunk as the sole addressable callback entry.
- Model imported/exported globals as declarations and executable facts, never as compiler-known
  library spellings.
- Preserve reachability-based cost and deterministic artifact inventories.

**Non-Goals:**

- Capturing closures, callback context boxes, variadic functions, thread/signal safety policy, or
  callback lifetime management.
- Mutable assignment to a global binding, thread-local storage, weak symbols, dynamic lookup, or
  `errno` macro emulation.
- Evaluator or WebAssembly callback tables/global bindings; those require explicit host contracts.

## Decisions

### Use a distinct foreign-function type

`extern "C" fn(P...) -> R` becomes a dedicated structural type carrying one classified signature.
It is represented as one pointer but is not callable through Silk's ordinary callable application
path and has no capture mode, environment schema, failure row, or requirement row. Reusing
`CallableType` was rejected because that would make captured environments and Silk calling
conventions appear ABI-compatible by representation accident.

### Make conversion contextual and export-owned

When the expected type is a C function pointer, a named function-item expression may convert only
if its declaration already owns an exact synchronous `export "C"` thunk. Lowering records the
export identity and emits that thunk's address. A general pointer cast or address-of syntax was
rejected because it could bypass the export checks or expose internal function ABI.

### Add one static-declaration actor and immutable access operation

Foreign and exported statics share a declaration fact containing ABI, logical name, native symbol,
declared type, and optional initializer. Imported static references lower to a `ForeignStaticLoad`;
exports contribute a `ForeignStaticExport` definition. V1 has no store operation. Keeping reads as
an explicit MIR operation makes reachability and non-native rejection exact.

### Restrict initializers to target-independent scalar constants

An exported static initializer must be a statically evaluated C-admitted scalar constant. This is
enough for ABI/version symbols and avoids inventing native relocation/constructor semantics. Raw
pointer and aggregate initializers remain future work.

### Extend availability by operation kind

The executable inventory distinguishes function calls, callback addresses, and static loads.
Native emission admits all three. Evaluator/direct Wasm continue to admit JUL-100 function calls
but reject callback/static operations before execution or emission. Unreachable declarations add no
inventory entry.

## Risks / Trade-offs

- [A combined ticket touches the full compiler pipeline] → Keep callback and data-symbol facts
  separate, share only C classification and symbol validation, and validate each acceptance path at
  the cheapest tier before the two native integration cases.
- [Platform `environ` spelling differs] → The acceptance harness selects the host-supported symbol
  spelling while the language model remains the portable explicit `as "symbol"` form.
- [A callback outlives its Silk state] → V1 callbacks carry no environment and address only static
  export thunks, so no borrowed/captured lifetime can escape.
- [Exported data could be stripped as unreachable] → Treat public data exports as library roots for
  native library artifacts, mirroring public C function exports.

## Migration Plan

This is additive in a green-field repository. Land syntax/facts, type checking, MIR/availability,
native lowering, and docs/tests together; remove the parser's obsolete blanket rejection of static
data declarations in the same change. Rollback is the single stacked issue commit.
