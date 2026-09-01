## ADDED Requirements

### Requirement: HIR retains applied interface operation evidence canonically

A resolved applied interface operation SHALL carry the canonical interface identity, normalized
interface arguments, implicit provider, selected operation, fully substituted callable contract,
and static witness evidence in HIR before specialization. Direct calls and immediately applied
pipeline sections that select the same provider-interface goal SHALL produce the same canonical call
plan apart from their source provenance. HIR and later phases MUST NOT introduce an applied-interface
runtime lookup, namespace object, generated helper call, or witness dictionary. Compiler concepts
used by both enclosing-bound and explicitly applied forms SHALL describe general static
interface-operation calls rather than classifying concrete applied calls as bound-only.

#### Scenario: Elaborate an applied interface operation

- **WHEN** `Encodable<u32>.encode(&age)` resolves through `Age: Encodable<u32>`
- **THEN** HIR records that complete application, provider, substituted effect contract, and witness without retaining a runtime interface application

#### Scenario: Canonicalize direct and piped operation calls

- **WHEN** direct `Encodable<u32>.encode(&age)` and piped `&age |> Encodable<u32>.encode` forms are elaborated
- **THEN** both identify the same static witness and ordered call operands while retaining their distinct complete source spans

#### Scenario: Preserve effect construction and execution identity

- **WHEN** an applied interface operation is an `effect fn` and its result is executed by `run`
- **THEN** HIR preserves the selected witness's Effect constructor and the run site through instance discovery and lowering

#### Scenario: Keep unresolved applied operations unavailable

- **WHEN** semantic analysis cannot determine the complete application, provider, operation, or witness
- **THEN** HIR contains only the unavailable expression with its originating diagnostic and exposes no executable call to realization
