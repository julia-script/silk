## Context

The preceding changes provide ordered concrete arguments, function-local parameter facts, argument
expression types, and top-level call targets. They intentionally stop before relating a caller's
arguments to the target declaration's parameters. Bootstrap Silk currently has only one available
type, `I32`, so count errors are observable while available-type disagreement is not yet expressible.

## Goals / Non-Goals

**Goals:**

- Publish stable ordered argument facts and positional argument-to-parameter mappings.
- Distinguish compatible, wrong-arity, and unavailable call contracts without cascading errors.
- Keep call-contract checking separate from function return-type compatibility.
- Make every contract state inspectable.

**Non-Goals:**

- Conversions, overloads, generics, inference, labels, defaults, variadics, or additional types.
- Changing call resolution, parameter lookup, or the meaning of return compatibility.
- Execution, evaluation order, recursion policy, AST, HIR, MIR, or lowering.

## Decisions

### Introduce a distinct call-contract fact

The call fact gains ordered arguments, positional mappings, and a closed contract outcome:
`Compatible`, `ArityMismatch`, or `Unavailable`. Function return compatibility continues to answer
only whether the returned expression's available type matches the declared return type. Combining
the two was rejected because a call can have an `I32` result type while still being invoked with the
wrong number of arguments, and both facts are useful diagnostic data.

### Bind only after unique call resolution

Mappings use existing argument ordinals and the uniquely resolved target's parameter ordinals.
Missing, ambiguous, and syntax-unavailable targets yield an unavailable contract and no mappings.
Guessing against an ambiguous declaration or binding by name was rejected because call arguments
are positional and target identity must remain exact.

### Diagnose only representable semantic errors

`SEM0007` reports expected and actual counts for a uniquely resolved target. When a mapped type is
unavailable, the contract is unavailable without a second diagnostic. No type-mismatch code is
introduced while `I32` is the only available type; adding one now would create an untestable branch
and imply language behavior that source programs cannot reach.

### Retain partial mappings on arity mismatch

Pairs that exist on both sides remain visible by ordinal even when counts differ, while unmatched
arguments and parameters remain explicit. This gives the inspector useful evidence and prepares the
evaluator to reject the whole contract without losing provenance.

## Risks / Trade-offs

- [Return compatibility may be mistaken for call validity] → Name and document the facts distinctly and show both side by side in the inspector.
- [Future types require a new mismatch outcome] → Add it only when a second available type makes the behavior observable and testable.
- [Partial mappings could be treated as executable] → Require an overall `Compatible` call contract before evaluation.

## Migration Plan

Add argument facts first, then positional mappings and outcomes, then `SEM0007` and deterministic
tests. Update the inspector, README, changeset, and release candidate after the public fact shape is
stable. Breaking prerelease public unions directly is preferred over adapters.
