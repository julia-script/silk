# persisted-checked-units Specification

## Purpose

Define selective, strict, Storage-backed persistence for complete checked-unit semantic query
records across compiler process restarts.

## ADDED Requirements

### Requirement: Persistence selects complete CheckBody records only

The compiler SHALL persist only eligible completed `CheckBody` query records in this slice. Each
record SHALL contain the primary authored body, all owned hidden/generated bodies, typed tables and
referenced generated data, unit diagnostics, exact query descriptor/key, result fingerprint, and
ordered dependency observations. Header/name answers, evaluation, instances, layouts, MIR, and
backend artifacts SHALL NOT be persisted by this capability.

#### Scenario: Complete hidden unit crosses restart

- **WHEN** a checked declaration owns generated hidden bodies and diagnostics and its record is
  reused after restart
- **THEN** the entire unit is restored or the entire unit recomputes
- **AND** no partial body/table/diagnostic subset is exposed

### Requirement: Answer and manifest share one versioned envelope

Storage SHALL atomically publish one envelope per stable query address containing codec/address
schema versions, compiler semantic identity, exact address, result fingerprint, ordered dependency
manifest, complete checked unit, and canonical integrity digest. Concurrent writers MAY leave
either complete envelope and every reader SHALL validate its winner independently.

#### Scenario: Mixed or incompatible envelope recomputes

- **WHEN** any version, compiler identity, address, fingerprint, manifest, payload, or digest field
  is missing, mismatched, or corrupt
- **THEN** the candidate is rejected as typed incompatible or corrupt data
- **AND** the current provider recomputes without exposing stored content

### Requirement: Complete-unit decoding is bounded and strict

The codec SHALL enforce bounds for total bytes, nesting depth, collection sizes, strings/byte
arrays, numeric forms, tags/fields, and identifier ranges. It SHALL validate owners, hidden-parent
relationships, node/local/evidence/cause references, typed tables, diagnostics, and provenance
before returning a complete unit. Unknown tags, extra/missing fields, invalid references, and
partial units SHALL reject the whole candidate.

#### Scenario: Malicious structure cannot allocate unbounded state

- **WHEN** an encoded candidate exceeds a byte, depth, collection, string, byte-array, or identifier
  bound
- **THEN** decoding fails with the corresponding typed codec rejection before unbounded allocation

#### Scenario: Invalid internal reference rejects the unit

- **WHEN** any body, table, diagnostic, provenance, or hidden-parent reference points outside the
  validated complete envelope
- **THEN** no checked body is published and the owner recomputes

### Requirement: Stored references resolve by stable identity

The codec SHALL encode declaration/artifact references through stable semantic identities or an
equivalent dictionary and SHALL resolve them through current declaration authority. A serialized
ordinal SHALL NOT identify whatever declaration currently occupies that slot. Hidden bodies SHALL
remain parent-owned.

#### Scenario: Unrelated insertion cannot redirect a reference

- **WHEN** an unrelated declaration insertion changes current ordinals after restart
- **THEN** restored references still resolve to their stable declarations or invalidate
- **AND** no reference silently points at the inserted declaration

### Requirement: Loaded candidates use shared validation and presentation

A decoded record SHALL become an ordinary prior completed query candidate. The shared recursive
validator SHALL replay its ordered observations against current HIR, selected closure, headers,
resolution, configuration, target, and nested query results. Missing child records SHALL execute
current providers and SHALL NOT imply unchanged. Only admitted answers SHALL be presented through
current anchors/spans; parser-recovered owners retain their byte-identical restriction.

#### Scenario: Body edit invalidates only affected work

- **WHEN** one body changes after restart while another owner's dependencies remain unchanged
- **THEN** the changed owner recomputes and the unrelated owner reuses its persisted unit

#### Scenario: Presentation-only movement uses current spans

- **WHEN** source movement leaves a persisted unit semantically valid
- **THEN** semantic facts agree with a forced-fresh result
- **AND** every diagnostic primary, label, and related span uses the current revision

### Requirement: Persistence failure policy is explicit

Missing, stale, incompatible, corrupt, reference-invalid, and oversize candidates SHALL record a
typed rejection and recompute. Expected external Storage read failure SHALL record a typed cache
failure and recompute. Expected publication failure SHALL preserve the valid current answer and
skip publication. Invalid logical address/limit SHALL fail as configuration rather than a miss.
Interruption and unexpected defects SHALL propagate. Aborted or cyclic queries SHALL publish no
record.

#### Scenario: External cache failure does not become semantic rejection

- **WHEN** optional Storage read fails externally
- **THEN** the current body provider executes and may return a valid checked unit
- **AND** the report records the cache failure distinctly from language diagnostics

#### Scenario: Cancellation publishes nothing

- **WHEN** checking or publication is interrupted before a complete record is committed
- **THEN** no partial answer is admitted or persisted
- **AND** interruption remains the request exit

### Requirement: Fresh and disabled modes bypass every cache tier

Explicit fresh or cache-disabled demands SHALL bypass persisted reads, persisted publication, and
in-memory prior-answer reuse through the same providers. Cached and forced-fresh completed outputs
SHALL agree in semantic facts, diagnostic codes, and current spans.

#### Scenario: Forced fresh ignores a valid stored record

- **WHEN** a valid checked-unit envelope exists and the caller requests forced-fresh analysis
- **THEN** the provider executes without reading or publishing the envelope
- **AND** its output agrees with the cached mode result

### Requirement: Persistence behavior is observable

Semantic reports and traces SHALL distinguish persisted lookup, validation, reuse, recomputation,
candidate rejection, Storage failure, and publication. Structural counters SHALL prove avoided
body work without timing assertions.

#### Scenario: Restart reuse reports avoided checking

- **WHEN** an unchanged complete unit is admitted after restart
- **THEN** reports identify persisted reuse
- **AND** checked-body provider counters show that body construction did not execute
