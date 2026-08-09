## MODIFIED Requirements

### Requirement: One snapshot answers supported queries

The facade SHALL build one immutable frontend analysis snapshot from a compilation request (with a
single-source convenience for one module) and SHALL answer queries over sources, syntax artifacts,
import facts and cycles, collected declarations and lookups, elaborated function facts with their
types, references, and contracts, HIR facts, and ownership facts with their cleanup plans. Runtime
instance, target, layout, MIR, evaluation, and codegen queries SHALL require an immutable realized
snapshot derived from that exact frontend snapshot. Query results SHALL be immutable values, and
repeated frontend construction and realization of identical input SHALL answer every supported query
identically.

#### Scenario: Query a multi-module snapshot

- **WHEN** a frontend snapshot is built from a request whose root imports another module
- **THEN** the facade lists both modules, answers each module's syntax artifact and declarations, and resolves declaration lookups per module without realizing runtime facts

#### Scenario: Repeat snapshot construction

- **WHEN** the same request is analyzed and realized repeatedly in fresh processes
- **THEN** every supported frontend and runtime query answers identically

#### Scenario: Query ownership facts

- **WHEN** a frontend snapshot's module contains checked functions
- **THEN** the facade answers the module's ownership facts and cleanup plans as immutable values without requiring runtime realization

## ADDED Requirements

### Requirement: Runtime realization derives a new coherent snapshot

The facade SHALL explicitly derive each realized snapshot from one completed frontend snapshot and a
target selection. Realization SHALL NOT mutate the frontend snapshot, replace any frontend fact, or
combine facts from different source revisions. Multiple target realizations of one frontend snapshot
SHALL share the same frontend answers by value while retaining distinct target-owned runtime facts.

#### Scenario: Realize one frontend snapshot

- **WHEN** a valid frontend snapshot is realized for a supported target
- **THEN** the resulting immutable snapshot exposes instances, target, layout, and MIR derived from that frontend snapshot while every frontend query remains unchanged

#### Scenario: Reject hidden lazy mutation

- **WHEN** frontend queries are repeated before and after another caller realizes the same frontend snapshot
- **THEN** the frontend snapshot returns identical answers and exposes no newly attached runtime state

### Requirement: Analysis observations identify executed work

Each frontend and realized snapshot SHALL expose immutable phase observations naming only the work
executed to produce that snapshot, with elapsed time and deterministic input, output, and diagnostic
counts. Observations are operational data and SHALL NOT participate in deterministic artifact
identity.

#### Scenario: Observe frontend-only analysis

- **WHEN** a request produces only a frontend snapshot
- **THEN** its observations include closure, declaration, resolution, elaboration, tooling-index, and ownership work and exclude instance discovery, target layout, and MIR lowering

#### Scenario: Observe runtime realization

- **WHEN** a frontend snapshot is explicitly realized
- **THEN** the realized snapshot retains the frontend observations and appends only the runtime phases that executed
