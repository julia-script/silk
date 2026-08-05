## Context

The analyzer now publishes a recursive semantic expression fact for integer literals, parameter
references, and calls. The evaluator already has closed completed and blocked outcomes, positional
environments, an active declaration path for cycle detection, and deterministic trace events, but
it deliberately blocks when a call appears inside an argument. The hidden inspector renders those
outcomes from the same compiler data.

See `proposal.md` for motivation and the delta specs for observable behavior.

## Goals / Non-Goals

**Goals:**

- Evaluate the recursive semantic expression union without creating another representation.
- Make call-by-value ordering, failure propagation, active-call tracking, and trace ordering exact.
- Give every implementation task direct visual feedback in the existing inspector.

**Non-Goals:**

- Add operators, local variables, side effects, general recursion, optimization, or native code
  generation.
- Turn the inspector into a persistent playground or runtime debugger.
- Introduce asynchronous or parallel argument evaluation.

## Decisions

### Evaluate through one recursive expression operation

The evaluator will use one internal recursive operation over the semantic expression union. Integer
facts yield their exact values, parameter references read the current function environment, and
call facts delegate to the existing call machinery after recursively producing their argument
values. The operation preserves the existing typed completed-or-blocked result and trace state.

Adding a nested-call-only evaluator was rejected because it would duplicate call validation and
become obsolete as soon as another expression position accepts calls. Lowering to a new instruction
form was rejected because this milestone needs neither optimization nor a backend boundary.

### Use strict left-to-right call-by-value ordering

When a call expression is reached, its call trace event is recorded first to preserve the current
flat trace contract. Its argument expressions are then evaluated completely in concrete order. Only
after every argument succeeds are positional binding events emitted and the target body entered.

This makes partial failure precise: earlier arguments may contribute completed trace events, but no
environment or target execution exists for the enclosing call until all values are ready. Binding
each argument immediately was rejected because it would expose a partial callee environment for a
call that never begins.

### Propagate the deepest blocked outcome unchanged

If a nested expression blocks, the evaluator will retain its exact reason and provenance and append
no enclosing binding, parameter read, or return. Earlier trace events remain in order. Wrapping each
inner reason in a generic nested-failure tag was rejected because it would make callers unpack a
second error layer and would hide the existing actionable reason.

The temporary `UnsupportedNestedExpression` branch becomes unreachable and is removed rather than
kept as compatibility debt in this prerelease package.

### Track active declarations only after arguments succeed

The current function remains active while its return expression is evaluated. For a new call, the
target declaration is checked against that active path, but it joins the path only after all of its
arguments produce values and the evaluator enters its body. Calls encountered inside those
arguments therefore see the actual active functions, not an enclosing target that has not started.

This is ordinary call-by-value behavior and gives exact cycles for nested arguments without a
separate recursion mechanism.

### Project trace nesting from identities and order

Trace events remain a flat deterministic sequence. Call-site identity, function identity, argument
and parameter identity, and source provenance are sufficient for the inspector to group nested
work. Adding mutable parent pointers or a second tree-shaped trace was rejected because it could
drift from the authoritative sequence. The later visualization change can derive richer grouping
from the same data.

## Risks / Trade-offs

- **Host recursion depth may still bound extreme programs** → Exercise representative deep nested
  fixtures and add an explicit iterative work stack if normal inputs approach the host limit.
- **A call event precedes argument evaluation even when the target never starts** → Document that
  the event means the call expression was reached; bindings and returns distinguish actual entry.
- **Nested blocked traces can become visually noisy** → Keep the compiler sequence lossless and let
  the inspector group events without dropping accessible details.
- **Cycle paths can be corrupted by premature target entry** → Test nested argument failures and
  cycles specifically, including absence of the unentered enclosing target.

## Migration Plan

Implement recursive expression evaluation behind the existing public evaluation operation, replace
the temporary blocked case, then update inspector projections and documentation. Existing flat
programs and their trace order remain regression fixtures. Rollback must restore the temporary
unsupported-nested-expression case together with the non-recursive evaluator so analyzed nested
facts never fall through an unhandled branch.
