## Why

Character literals currently require explicit conversion or replacement with numeric spellings at
integer boundaries, even when the compiler knows their exact scalar value. JUL-157 requests readable
character spellings in those contexts while retaining `char` and its Unicode validity guarantee.

## What Changes

- Default character literals to `char`, with immediate concrete integer contexts selecting an exact,
  representable Unicode scalar number instead.
- Apply the rule to existing literal contexts, primitive constants, and static evaluation; preserve
  exact compatibility for already-typed values and reject invalid Unicode before type selection.
- Report selected integer types and exact bounds in range diagnostics.
- Update reference documentation and replace tests that require unconditional `char` literal typing.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-integer-scalars`: contextual character literal selection and truthful range diagnostics.

## Impact

Compiler expression/constant analysis, static literal evaluation, the existing integer diagnostic,
focused tests, and the language reference. Integer-selected literals use the existing integer
representation; character values, string traversal, encodings, and backend operations remain intact.

Tracked by [JUL-157](https://linear.app/juliaortiz/issue/JUL-157/allow-character-literals-to-take-an-immediate-integer-type).
