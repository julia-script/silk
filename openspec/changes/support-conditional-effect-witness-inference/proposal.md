## Why

Conditional interface conformances whose operations preserve generic failure and requirement rows
can be declared, but cannot always be selected from a generic caller. This blocks ordinary-source
nominal adapters such as the streaming HTTP server's handler-to-buffered-context bridge even when
the caller carries the exact conditional bound.

## What Changes

- Make conditional-conformance selection reuse the caller's declared bound when the provider and
  interface application contain generic failure or requirement-row arguments.
- Make operation-witness matching infer target row binders from an exact mapped Effect contract.
- Preserve exact requirement-row subtraction and reject genuinely missing or mismatched bounds
  with the existing finite conditional-requirement trace.
- Prove a selected open requirement row is a subset of an identically composed source union by
  forward normalized-row comparison, without binding or subtracting any row parameter.
- Add a reduced semantic regression plus realization coverage proving the selected witness remains
  static and monomorphic.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-conditional-interface-conformance`: Generic callers can select a coherent conditional
  conformance and its Effect-polymorphic operation when the required provider conformance is already
  present in the caller's bounds.

## Impact

- Affects conditional conformance proof selection and operation-witness compatibility in the
  compiler frontend, forward-only structural requirement proofs, and instance discovery.
- Adds focused analysis and realization regressions derived from a minimal pair of nominal
  interfaces and a generic context adapter.
- Unblocks `silk.http_server.withConnection` without changing its handler, error, requirement, or
  ownership contract.
- Does not add runtime dictionaries, new syntax, implicit `Self`, service dispatch, backend-specific
  behavior, or relaxed coherence/termination rules.
