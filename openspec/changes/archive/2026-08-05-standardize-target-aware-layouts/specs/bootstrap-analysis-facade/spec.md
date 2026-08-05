## ADDED Requirements

### Requirement: Target and layout are facade queries

The facade SHALL expose the snapshot's canonical target selection and immutable completed layout
plan as supported queries. The lowered MIR, interpreter, and codegen queries SHALL consume that
same plan by value; no facade query or tooling consumer may construct a replacement target layout.
An unsupported target SHALL remain an explicit queryable outcome and SHALL make lowering,
evaluation, and codegen unavailable without inventing fallback facts.

#### Scenario: Query one shared layout plan

- **WHEN** a supported snapshot discovers instances using `I32` and `Bool`
- **THEN** its target query, layout query, lowered MIR query, evaluation query, and codegen query all identify the same canonical target and scalar layout entries

#### Scenario: Query an unsupported target

- **WHEN** a snapshot request selects an unsupported target
- **THEN** the facade exposes the target failure and marks layout, MIR, evaluation, and codegen unavailable without invoking a backend

#### Scenario: Emit WebAssembly from a WebAssembly snapshot

- **WHEN** a snapshot selects `wasm32-unknown-unknown` and codegen uses the compatible direct WebAssembly backend
- **THEN** codegen consumes the snapshot's existing MIR layout plan without replacing its target or scalar entries
