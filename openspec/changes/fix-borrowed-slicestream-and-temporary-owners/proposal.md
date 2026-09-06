## Why

JUL-151 reproduces four compiler gaps in an ordinary-source borrowed SliceStream: elided conformance headers fail, receiver calls acquire the wrong loan, direct slice-field indexing crashes native lowering, and borrowed array initializers lose their backing owner. The example needs four workarounds despite existing borrowed-value and receiver-equivalence contracts.

## What Changes

- Elaborate omitted conformance owner lifetimes using completed nominal arity and preserve their operation contracts.
- Preserve stored shared-view access independently of exclusive wrapper receiver access.
- Carry slice descriptors, runtime lengths and element layouts through field indexing.
- Materialize borrowed arrays in binding initializers as lexical hidden owners with ordinary inference, evaluation order, lifetime checking, suspension storage and cleanup.
- Reject hidden-local escapes and verify one consumed example without workarounds.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-declaration-index`: conformance owner lifetime completion and operation binder inheritance.
- `bootstrap-ownership`: binding-local hidden array owners, stored shared-view receiver loans and lifecycle boundaries.
- `bootstrap-mir`: projected slice descriptors and hidden-owner cleanup/suspension storage.

## Impact

Compiler declaration completion, expression/lifetime/ownership analysis, HIR/MIR lowering and native places; focused existing test files, shared native acceptance, and the prescriptive reference. No standard-library Stream API, compiler-known library actor, new runtime lifetime representation or external dependency.
