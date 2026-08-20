## ADDED Requirements

### Requirement: Service declarations add only dependency eligibility

A `service` declaration SHALL produce the same static contract fact and use the same conformance, witness, bound, specialization, and qualified-operation machinery as an `interface` declaration. The contract fact SHALL carry one declaration classification that permits the service to be used as an Effect dependency. No other operation or witness behavior SHALL branch on that classification.

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
