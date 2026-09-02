## ADDED Requirements

### Requirement: Anonymous callable facts bind explicit contracts and implicit captures

Analysis SHALL bind each anonymous callable parameter and body in a lexical scope nested inside its
containing expression scope. Parameter types and the ordinary result or effect success, failure, and
requirement contracts SHALL be explicit and SHALL be checked under the surrounding generic
substitution. A contextual callable expectation MAY constrain compatibility and surrounding generic
selection, but MUST NOT supply an omitted parameter or result contract. References to the nearest
visible outer local or parameter SHALL become implicit captures in deterministic first-reference
source order; module declarations, type names, and the anonymous callable's own parameters MUST NOT
be captures. The first slice SHALL reject nested anonymous bodies, self-reference, independent type
parameters, declaration modifiers, and overload participation with semantic diagnostics.

#### Scenario: Resolve a lexical capture

- **WHEN** an anonymous body reads an outer `offset` after declaring its own `value` parameter
- **THEN** facts resolve `value` to the anonymous parameter and record `offset` once as the selected outer binding

#### Scenario: Preserve explicit contracts under context

- **WHEN** an authored `fn(value: A) -> B { ... }` appears where a compatible callable is expected
- **THEN** analysis checks the authored parameter and result under surrounding substitutions rather than replacing them with the expected signature

#### Scenario: Reject an excluded nested body

- **WHEN** an anonymous callable body contains another anonymous callable expression
- **THEN** analysis reports the first-slice exclusion while preserving bounded syntax and the outer body's remaining facts

### Requirement: Capture access derives anonymous invocation mode

For each implicit capture, analysis SHALL derive Copy snapshot, shared loan, exclusive loan, or moved
affine ownership transfer from the body operation that uses the selected binding. A callable whose
environment permits shared repeated invocation SHALL have `fn` mode; any reusable capture requiring
exclusive access SHALL raise the mode to `mut fn`; and any capture consumed by moving an affine value
SHALL raise it to `once fn`. Copy moves SHALL remain reusable. The derived mode SHALL participate in
the existing callable substitution order without an authored construction modifier.

#### Scenario: Derive every invocation mode

- **WHEN** representative anonymous bodies capture a Copy value, a shared borrow, an exclusive borrow, and a moved affine owner
- **THEN** their facts derive `fn`, `fn`, `mut fn`, and `once fn` respectively with the exact capture access recorded

#### Scenario: Reject a consuming value as reusable

- **WHEN** a moved-affine anonymous callable is required where `fn(A) -> B` is expected
- **THEN** analysis reports the incompatible consuming mode before lowering
