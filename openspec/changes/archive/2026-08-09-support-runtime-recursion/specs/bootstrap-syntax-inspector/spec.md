## MODIFIED Requirements

### Requirement: Incomplete data flow remains explicit
The data-flow view SHALL represent missing, ambiguous, incompatible, unavailable, blocked, cyclic,
and resource-limited relationships as terminal or branched states rather than drawing a successful
enclosing path. It SHALL preserve all known nested provenance and link each stopped edge to the
semantic fact, evaluation reason, trace prefix, or phase-owned diagnostic that explains it. A
completed earlier argument branch SHALL remain visible when a later sibling blocks, but bindings or
returns that did not occur MUST NOT be drawn as evaluated flow.

#### Scenario: Stop at wrong arity
- **WHEN** a resolved call at any nesting depth has an arity-mismatch contract
- **THEN** the view shows any available positional pairs, marks unmatched arguments or parameters, and stops before claiming a valid result from that call or its enclosing call

#### Scenario: Branch at an ambiguous reference
- **WHEN** a parameter or function reference has multiple matches
- **THEN** the view exposes all candidates without selecting a successful edge

#### Scenario: Stop at unavailable syntax or type
- **WHEN** parser recovery or an unresolved type makes a required nested fact unavailable
- **THEN** the affected branch ends at a labeled unavailable state with the available syntax and diagnostic context retained

#### Scenario: Stop at a nested evaluation failure
- **WHEN** an inner argument blocks after an earlier argument completed
- **THEN** the earlier completed branch and partial trace remain visible while the inner reason terminates the enclosing evaluated path before its bindings

#### Scenario: Show an evaluation limit as a closed terminal path
- **WHEN** nested evaluation exhausts its step or call-depth limit
- **THEN** the view lists the limit, active call identities, and stopping span without drawing an infinite or successful path

### Requirement: Data-flow presets remain disposable
The inspector SHALL provide complete flat, complete nested, nested sibling, wrong-arity,
unknown-reference, ambiguous-reference, syntax-damaged, inner-blocked, recursive-complete, and
evaluation-limit data-flow presets. Data-flow mode, evaluation overlay, selection, and source
emphasis SHALL remain in browser memory and SHALL reset to the canonical preset on reload.

#### Scenario: Compare complete and incomplete paths
- **WHEN** a developer switches among flat, nested, recursive, and blocked data-flow presets
- **THEN** the projection and accessible description recompute locally from each preset's current semantic facts and optional evaluation outcome

#### Scenario: Compare static and evaluated flow
- **WHEN** a developer analyzes and then explicitly evaluates a nested preset
- **THEN** the same semantic relationships remain visible while reachable order, exact values, and any blocked endpoint are added from that outcome

#### Scenario: Reload after selecting a flow node
- **WHEN** the inspector is reloaded after source edits, evaluation, or flow navigation
- **THEN** the canonical source and unselected canonical flow are restored without persisted state

### Requirement: Evaluate the current bootstrap program
The Syntax Inspector SHALL provide an explicit browser-local evaluation action for the current
analyzed source. It SHALL display either the completed exact `i32` result or the closed blocked
reason and SHALL render the ordered evaluation trace with links to existing function, call,
argument, parameter, reference, and source provenance. Evaluation MUST NOT make a network request,
write files, persist results, or imply native compilation.

#### Scenario: Evaluate the canonical identity program
- **WHEN** a developer activates evaluation for `main` returning `identity(42)`
- **THEN** the inspector displays result `42` and an ordered trace matching the visible semantic data-flow path

#### Scenario: Inspect a blocked evaluation
- **WHEN** a developer evaluates a preset with a missing entry, wrong call arity, unavailable fact, or evaluation limit
- **THEN** the inspector shows the exact blocked reason, partial trace, and relevant source relationships without becoming unresponsive

#### Scenario: Edit after evaluation
- **WHEN** source text changes after an outcome is displayed
- **THEN** the stale outcome is cleared and the edited source must be analyzed before a new explicit evaluation

#### Scenario: Reload after evaluation
- **WHEN** the page reloads after a completed or blocked evaluation
- **THEN** the canonical source returns with no persisted result or trace

### Requirement: Inspect recursive evaluation outcomes
The Syntax Inspector SHALL provide terminating-recursion, inner-blocked, and evaluation-limit
presets and SHALL render every activation's trace events beside the semantic expression and source
provenance that produced them. Successful recursive results SHALL connect through their caller
bindings and returns, while a limited path SHALL end before events that did not occur. The trace
SHALL remain available as an ordered accessible text structure and MUST NOT rely on indentation,
position, or color alone to communicate nesting.

#### Scenario: Inspect completed recursion
- **WHEN** a developer evaluates a recursive countdown that reaches its base case
- **THEN** the inspector distinguishes each activation, binding, base-case return, and unwound return in trace order

#### Scenario: Inspect a call-depth limit
- **WHEN** recursive evaluation exhausts its configured call-depth limit
- **THEN** the inspector shows the configured limit, complete active call identities, stopping span, and partial trace without a completed result

#### Scenario: Read recursive trace order without graphics
- **WHEN** the recursive trace is consumed through its accessible text representation
- **THEN** activation depth, event order, values, identities, states, and source ranges communicate the same outcome as the visual trace
