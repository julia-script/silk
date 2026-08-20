# Bootstrap Service Declarations Specification

## Purpose

Allow Silk source to define portable runtime service contracts and implementations that participate
in requirement rows and lexical provision without adding compiler-known service names.

## Requirements

### Requirement: Source modules declare service contracts

A public `service` declaration SHALL define one nominal service and the complete function contract
of each operation, including success, typed-failure, requirement, and shared or exclusive access.
The declaration SHALL contain no storage, runtime initialization, implementation selection, or
implicit default provider. Its operations SHALL form a closed service interface owned by its source
module.

#### Scenario: Declare a mutable Logger service

- **WHEN** source declares a Logger operation whose contract requires exclusive Logger access
- **THEN** calls retain `&mut Logger` in their requirement row until a conforming implementation is provided

#### Scenario: Reject service storage

- **WHEN** a service declaration contains a field or initialization expression
- **THEN** analysis rejects it rather than constructing a hidden service instance

### Requirement: Service implementations map existing actor functions

An `impl Service for Provider` declaration SHALL map every required service operation to an
existing function in the provider's actor module. The mapped function MAY have weaker access needs
or smaller failure and requirement rows, but MUST NOT strengthen the service contract. The mapping
SHALL add no instance method and SHALL produce a statically shaped service witness.

#### Scenario: Implement a source-defined service

- **WHEN** `StdoutLogger` maps every Logger operation to compatible `StdoutLogger` actor functions
- **THEN** the compiler creates a Logger witness for `StdoutLogger` without recognizing either nominal name

#### Scenario: Reject a stronger implementation

- **WHEN** a mapped provider operation adds a failure or requirement absent from the service operation
- **THEN** conformance analysis rejects the mapping before instance discovery

### Requirement: Provision works for every source-defined service

General Effect provision SHALL accept any provider with a valid service conformance, remove exactly
the selected capability-role requirement, preserve all residual rows, and restore the outer service
after the lexical region. No compiler phase MAY use a closed list of service names to validate,
dispatch, lower, or execute provision.

#### Scenario: Replace one service implementation

- **WHEN** the same Effect is provided first with one Logger implementation and then with another
- **THEN** both calls use their statically selected witnesses while the Effect's source contract remains unchanged

#### Scenario: Reject a missing conformance

- **WHEN** provision receives a value with no implementation of the selected source-defined service
- **THEN** analysis rejects the provision with the provider and service identities preserved

### Requirement: Services remain distinct from ordinary interfaces

A service SHALL name a runtime requirement that is lexically provided through a service slot. An
ordinary interface SHALL describe static type conformance and MUST NOT create a requirement row,
service slot, provider lifetime, or `Effect.provide` operation merely because it has operations.

#### Scenario: Use a numeric interface

- **WHEN** generic integer addition selects an `Integer` conformance for `i32`
- **THEN** specialization selects its operation without adding an Integer service requirement

#### Scenario: Use a Logger service

- **WHEN** a function calls a Logger operation
- **THEN** its function contract retains the Logger requirement until lexical service provision

### Requirement: Service declarations add only dependency eligibility

A `service` declaration SHALL produce the same static contract fact and use the same conformance,
witness, bound, specialization, and qualified-operation machinery as an `interface` declaration.
The contract fact SHALL carry one declaration classification that permits the service to be used as
an Effect dependency. No other operation or witness behavior SHALL branch on that classification.

#### Scenario: Use a service as an ordinary bound

- **WHEN** a generic parameter declares a service contract as a bound
- **THEN** the compiler proves and specializes that bound exactly as it would an interface bound

#### Scenario: Admit a service requirement

- **WHEN** an Effect requirement names a concrete service declaration
- **THEN** dependency construction accepts the requirement after checking its eligibility classification

#### Scenario: Reject an interface requirement

- **WHEN** an Effect requirement names an ordinary interface declaration
- **THEN** dependency construction rejects it without changing the interface's validity in bounds or conformances

#### Scenario: Dispatch through an ordinary service witness

- **WHEN** a provider conforms to a service and a service operation is selected
- **THEN** the compiler uses the same canonical witness identity and static operation target used for an interface conformance

### Requirement: Roles are nominal compile-time declarations

`role Name` SHALL declare an ordinary nominal compile-time identity that MAY qualify a service
requirement as `Service at Name`. Roles SHALL participate in normal visibility, import, semantic
fact, navigation, completion, formatting, and deterministic module-surface behavior. A role SHALL
not create a provider, service conformance, runtime value, runtime slot, or access mode.

#### Scenario: Inspect a role declaration

- **WHEN** a source module declares `pub role Primary` and another module imports it
- **THEN** tooling resolves and navigates `Clock at Primary` to that declaration using ordinary visibility

#### Scenario: Reject a non-role qualifier

- **WHEN** `Clock at Value` names a visible declaration that is not a role
- **THEN** semantic analysis rejects the requirement entry at the qualifier
