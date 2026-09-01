## MODIFIED Requirements

### Requirement: Ownership facts are produced once per residual specialization

After private residual and cleanup-call candidate closure is complete, the ownership phase SHALL run
once for each residual runtime HIR specialization and SHALL publish
one immutable ownership fact for that specialization: its runtime bindings with their ownership
category and live range over source spans, and a closed verdict. Static parameters, static local
bindings, static-function locals, inactive static arms, and static evaluator storage MUST NOT appear
as owned bindings or cleanup obligations. Runtime parameters and residual `let` statements SHALL
retain their ordinary liveness and move behavior.

A specialization whose residual HIR body or runtime contract is unavailable SHALL carry an
explicitly unavailable verdict retaining the originating diagnostic identity where one exists and
MUST NOT report a satisfied check it could not perform. A static evaluation that fails before
producing residual HIR SHALL publish its static diagnostic and no ownership fact for that failed
specialization.

Cleanup-call candidate discovery MAY use a target-neutral prepass over residual types and exits, but
that prepass MUST NOT publish ownership, liveness, borrow, or cleanup-plan facts and MUST NOT admit
executable reachability before the residual graph is closed.

#### Scenario: Check a copyable parameter

- **WHEN** a mixed function specializes to a residual body reading one runtime `i32` parameter
- **THEN** its ownership fact lists that copyable parameter through the residual body and omits every static input

#### Scenario: Keep unavailable bodies explicit

- **WHEN** selected residual HIR is unavailable after recovery or an unresolved selected reference
- **THEN** its ownership verdict is explicitly unavailable and carries the originating diagnostic identity

#### Scenario: Omit a failed static specialization

- **WHEN** `compileError` or an evaluation limit prevents a specialization from producing residual HIR
- **THEN** ownership publishes no satisfied, violated, or partial fact for that specialization

#### Scenario: Range a let binding's liveness

- **WHEN** a selected runtime arm binds `let value = 42` and returns `value`
- **THEN** the residual ownership fact lists the binding from its statement through the residual return

#### Scenario: End liveness at a consuming move

- **WHEN** a residual body moves one runtime binding into a call argument and later source does not use it
- **THEN** the binding's live range ends at that move while any unselected-arm use is absent from ownership analysis

### Requirement: The cleanup plan is a target-neutral artifact

The phase SHALL produce one cleanup plan per successful residual runtime specialization: every
structured residual exit path with its ordered releases in last-acquired, first-released order. A
release SHALL record the end of one runtime binding's ownership at that exit; bindings already
consumed by a move before the exit MUST NOT be released again. Static values and evaluator storage
MUST NOT produce releases. The plan SHALL remain target-neutral, SHALL record runtime releases
uniformly whether or not the released type carries cleanup behavior, and SHALL expose a
deterministic textual encoding gated by committed golden files.

#### Scenario: Plan the single return exit

- **WHEN** static selection leaves one residual return path with runtime parameters
- **THEN** the cleanup plan contains that return exit and only the releases required by residual runtime bindings

#### Scenario: Match the cleanup golden encoding

- **WHEN** a committed mixed-function fixture is specialized repeatedly with the same inputs
- **THEN** its cleanup encoding is byte-for-byte identical and names no static binding or inactive-arm release

#### Scenario: Release bindings in reverse binding order

- **WHEN** a selected runtime arm declares `let first = 1` then `let second = 2` and returns a literal
- **THEN** the residual exit releases `second` before `first`

#### Scenario: Skip a moved binding at the exit

- **WHEN** a residual body moves its only runtime binding before return
- **THEN** the exit release list omits that binding while static values remain outside the plan
