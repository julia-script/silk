# Spec Delta

## ADDED Requirements

### Requirement: Reusable lookup outcomes are independent of caller locations

Name resolution SHALL separate its reusable semantic outcome and definition-owned causes from
caller-specific anchors and diagnostic spans. A missing, inaccessible, conflicting, or otherwise
rejected lookup SHALL be reusable only as a location-independent reason; every use SHALL instantiate
diagnostic provenance at its current site, including multiple uses of the same rejected lookup in
one revision.

#### Scenario: Present two uses of one rejected qualified lookup

- **WHEN** two current use sites request the same missing qualified member
- **THEN** both sites report the rejection at their own spans while sharing the location-independent semantic lookup result

#### Scenario: Move one rejected use

- **WHEN** one of two rejected uses moves and the underlying lookup inputs remain equal
- **THEN** the moved diagnostic follows its current anchor without invalidating the shared semantic outcome

### Requirement: Qualified and associated lookups observe exact selection inputs

A qualified or associated lookup SHALL observe its qualifier result, exact namespace or owner
membership, visibility/conflict outcome, imported binding selection, and complete relevant
associated or conformance candidate set. It MUST NOT record a fabricated query dependency for work
that no provider can execute.

#### Scenario: Retarget a qualifier

- **WHEN** an import alias retains its spelling but selects another canonical module
- **THEN** the binding-selection observation changes and the qualified member lookup executes

#### Scenario: Repair a missing associated member

- **WHEN** a previously missing associated member is added to an already loaded owner
- **THEN** the owner candidate-set observation changes and the lookup returns the current member
