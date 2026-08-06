## ADDED Requirements

### Requirement: Union facts expose canonical and source member structure

Semantic analysis SHALL retain each source-written union member and separator with its resolution,
provenance, and causal availability while publishing one normalized type outcome containing the
canonical ordered nominal member set. Failed members SHALL remain queryable and MUST make the
dependent union outcome unavailable without erasing independent resolved members.

#### Scenario: Analyze an equivalent duplicate union

- **WHEN** source spells `End | Token | End`
- **THEN** facts retain three source members while the available semantic type contains canonical `End` and `Token` exactly once

#### Scenario: Retain one unresolved member

- **WHEN** one member of `Token | Missing | End` cannot resolve
- **THEN** the `Token` and `End` member facts remain available and the union outcome names the missing member's cause

### Requirement: Contextual union conversions are explicit facts

Every accepted union injection or widening SHALL publish the source type, target union, immediate
expected-context owner, canonical source-to-target member mapping, access mode, exact span, and
complete outcome. Rejected conversions SHALL report missing target members or an unavailable source
without changing the source's inferred type or fabricating a later conversion.

#### Scenario: Record a contextual argument injection

- **WHEN** a precise `Token` binding is passed to a `Token | End` parameter
- **THEN** facts keep the binding type `Token` and attach one injection with the parameter as its expected context

#### Scenario: Diagnose a non-containing target

- **WHEN** `Token | Fault` is returned where `Token | End` is declared
- **THEN** the return facts retain both source members and identify `Fault` as preventing conversion

