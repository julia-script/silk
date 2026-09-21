# Why

Declaration/header queries now have one typed session boundary, but body checking and static
evaluation still expose their older stores directly. The semantic session must coordinate these
operations without discarding cross-revision editor reuse or value-sensitive evaluation identity.

# What Changes

- Make `CheckBody` a typed semantic-session request while retaining `BodyQuery` as the
  cross-revision artifact reuse store.
- Record dependencies at semantic reads and preserve fresh execution as a correctness oracle.
- Route static evaluation through `Semantic.evaluate` while retaining the existing target,
  application, budget, recursion, and failure cache semantics.
- Keep artifact identity distinct from value-sensitive evaluation identity.

# Capabilities

## New Capabilities

- `semantic-body-queries`: session-owned body checking and dependency observations.
- `semantic-evaluation-queries`: value-sensitive evaluation requests coordinated by the semantic
  boundary.

# Impact

Compiler semantic dispatch, residualization/static evaluation, body reuse fixtures, public exports,
and the architecture reference are affected. Syntax and source-resolver capabilities remain outside
the sealed semantic session.
