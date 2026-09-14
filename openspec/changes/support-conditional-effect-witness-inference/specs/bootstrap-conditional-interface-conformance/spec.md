## MODIFIED Requirements

### Requirement: Concrete specialization proves every requirement

At each reachable concrete interface goal, analysis SHALL prove all conditional requirements before
admitting one witness. Within a checked generic body, a conditional conformance whose provider and
interface head match symbolically SHALL be usable when every conditional requirement is satisfied
by an exact declared bound of that body; the unresolved goal SHALL remain static until concrete
specialization. Generic failure and requirement-row arguments in the conformance head or operation
contract SHALL participate in the same substitution and witness-target inference as ordinary type
arguments. A known provider or conformance application MAY retain generic arguments owned by the
enclosing declaration, while inference SHALL bind only the callee's open parameters. Once those
binders are independently known, an open selected requirement row SHALL be accepted as a subset of
a source row only when forward normalized structure proves the relation and neither row retains
unresolved member-well-formed obligations. Missing, unavailable,
non-terminating, cyclic, or row-incompatible proofs MUST retain a finite requirement trace and MUST
NOT create runtime dictionaries or provisional witnesses.

#### Scenario: Report a missing base witness

- **WHEN** one mapped provider's source type lacks the required decoder conformance
- **THEN** the call is rejected with the conditional requirement chain

#### Scenario: Select an Effect-polymorphic conditional adapter from a generic bound

- **WHEN** a generic body carries an exact handler conformance and passes a nominal context whose
  conditional conformance maps an operation preserving generic success, failure, and requirement
  rows
- **THEN** analysis admits the symbolic call, retains the unresolved conformance until concrete
  specialization, and preserves the operation's exact Effect contract

#### Scenario: Preserve an exact split requirement union and exclusions

- **WHEN** a nominal context fixes one additional requirement row and a generic caller supplies the
  other, both carry direct capability-exclusion bounds, and the selected Effect and source Effect
  normalize to the same `R | Q` union
- **THEN** analysis first infers the context-owned row from the known conformance, checks its direct
  exclusion, and proves the remaining subset forward without reasoning backward through `Without`

#### Scenario: Reject an unpromised conditional adapter bound

- **WHEN** the mapped witness requires a handler conformance that the generic caller does not
  declare, or its substituted failure or requirement row differs
- **THEN** analysis rejects the call with the responsible conditional requirement and does not use
  an expected result type to invent or alter row arguments
