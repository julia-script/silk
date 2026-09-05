## ADDED Requirements

### Requirement: Independent environments and completion outcomes prove validity separately

Independent execution SHALL require the existing exact executable detachment proof, reject external caller/provider loans, and preserve existing affinity and NonParking rules independently. Completion SHALL reject references into package-owned storage. Boundaries requiring detached outcomes SHALL check full nested data lifetimes independently of the environment proof. Provision, allocation, Copy or an empty constrained variant MUST NOT manufacture detachment.

#### Scenario: Reject a provided external loan

- **WHEN** an Effect with an empty service row still retains a lexical provider loan
- **THEN** independent construction rejects it

#### Scenario: Reject nested completion escape

- **WHEN** a completion outcome wraps a short-lived or package-owned reference in an aggregate or generic container
- **THEN** the relevant completion or detached-output boundary rejects the payload even if its environment is detached
