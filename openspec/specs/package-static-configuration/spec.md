# package-static-configuration Specification

## Purpose

Define package-owned typed static configuration, deterministic default resolution and validation before an immutable compilation profile is published.

## Requirements

### Requirement: Packages declare unconditional typed schemas

A package SHALL declare `[pub] param name: Type = expression [where predicate]` at unconditional module scope. The default SHALL be optional for public parameters; a private parameter SHALL require a default. The optional predicate SHALL be a statically evaluable boolean expression and SHALL observe the final resolved parameter through its name. Defaults and predicates SHALL support ordinary imported and forward-referenced Silk static helpers. Package parameters SHALL be immutable static values. A schema SHALL NOT depend on conditional declaration availability or runtime operations.

Parameter identity SHALL be the tuple of logical package name/version, package-relative canonical module and declaration name. Import aliases and physical paths MUST NOT change identity. Unequal package sources claiming the same package identity SHALL be rejected within one graph.

#### Scenario: Resolve a forward helper

- **WHEN** a public parameter's default calls a static helper declared later in its module
- **THEN** the default resolves through ordinary Silk semantics and retains that helper as a dependency

#### Scenario: Imported target-dependent default

- **WHEN** a parameter default calls an unconditional imported helper which inspects a primitive target fact
- **THEN** resolution evaluates the helper using immutable initial target facts and records the import/default dependency

### Requirement: Bindings admit deterministic serializable values

Parameters SHALL admit integers, booleans, strings, nominal enums, optionals, arrays and records recursively composed of admitted types. Values SHALL be checked against the declared type, including integer range, nominal enum identity, record fields and nested element types. Runtime handles, callbacks, resource-bearing values and ambient access SHALL be rejected. External transport SHALL preserve integer precision and distinguish nominal enum, optional, array and record values without ambiguous coercion.

#### Scenario: Bind a nested record

- **WHEN** a public record parameter receives correctly typed arrays, enums and optional fields
- **THEN** the resolved immutable value is observable by ordinary static helpers and participates in canonical identity

#### Scenario: Reject the wrong nominal type

- **WHEN** a binding supplies an enum member from a different declared enum or an out-of-range integer
- **THEN** a structured type diagnostic identifies the binding origin and schema declaration without coercing the value

### Requirement: Precedence retains one value and origin

Resolution SHALL use package default, then workspace/project binding, then artifact/profile binding. Workspace and project SHALL share one tier; artifact and selected profile SHALL share one higher tier. Multiple bindings at one tier SHALL be conflicts even if their values are equal. All supplied bindings SHALL be checked for identity, visibility, type and provenance. Winning explicit values SHALL suppress execution of replaced defaults. Unknown, missing-required and private bindings SHALL have distinct structured diagnostics with applicable origins and declaration spans.

#### Scenario: Override a default dependency cycle

- **WHEN** two defaults refer to one another but an explicit winning value replaces one default
- **THEN** resolution follows only the demanded default dependencies and succeeds when the remaining default resolves from the explicit value

#### Scenario: Same-tier conflict

- **WHEN** project and workspace bindings both address one parameter
- **THEN** resolution reports both origins as a same-tier conflict rather than selecting by input order

#### Scenario: Reject a private binding

- **WHEN** an artifact attempts to bind a private parameter by its exact identity
- **THEN** resolution rejects the binding with its origin and the private declaration span

### Requirement: Bootstrap publishes only a complete validated profile

Bootstrap SHALL freeze initial target/artifact/build facts, discover unconditional schemas and imports, resolve admitted schema types, validate bindings, resolve final values by dependencies, run all validation predicates, and only then publish a normalized profile. Schema types dependent on parameters under construction SHALL be rejected as bootstrap cycles. Default dependencies SHALL observe other parameters' winning final values. In-progress value dependencies SHALL report a deterministic cycle trace. Validation SHALL run in stable parameter identity order after all values resolve and SHALL NOT mutate those values. A failed default or false predicate SHALL prevent publication. Published profiles MUST NOT be mutated or reused as provisional resolution state.

#### Scenario: Diagnose an unbroken cycle

- **WHEN** demanded defaults form a dependency cycle without a replacing explicit binding
- **THEN** a cycle diagnostic identifies the participating source spans and binding/default origins and no completed profile is published

#### Scenario: Validate an override

- **WHEN** an explicit value has the correct type but violates its declaration's predicate
- **THEN** configuration fails with the predicate and binding origins before ordinary specialization

#### Scenario: Failed bootstrap cannot contaminate the next request

- **WHEN** a failing profile is followed by a valid profile in one process
- **THEN** the valid request resolves independently and cannot observe partial values from the failure

### Requirement: Bootstrap dependencies invalidate results

Bootstrap cache identity SHALL include immutable initial facts, effective binding inputs and source dependencies, including imported helpers and referenced defaults. Changes to a demanded dependency SHALL invalidate its affected results. Uncalled static helper bodies SHALL not execute merely because they are loaded. Completed semantic caches SHALL use the published profile plus ordinary source/application identity. Diagnostics SHALL retain current applicable source spans and binding origins.

#### Scenario: Edit a default helper

- **WHEN** a helper used by a default changes while the target and explicit bindings remain equal
- **THEN** the dependent default is reevaluated and the published identity reflects the new resolved value

### Requirement: Static configuration excludes secrets and ambient discovery

Static configuration SHALL NOT access ambient environment, runtime discovery, callbacks or physical supply objects. External input provenance SHALL distinguish literal and explicitly translated public values from secret, physical-supply and runtime values. The latter three SHALL be rejected before values are echoed into diagnostics or caches. Deterministic build-tool translation SHALL provide a concrete public logical value and translator identity. The compiler SHALL NOT claim to identify arbitrary secret strings heuristically.

#### Scenario: Reject a labeled secret

- **WHEN** a binding is marked secret-bearing
- **THEN** configuration reports its identity and origin without including its value and publishes no profile

#### Scenario: Explicit public translation

- **WHEN** an application-edge translator supplies a deterministic public boolean
- **THEN** the static parameter receives that boolean without any environment or translator callback becoming available to Silk
