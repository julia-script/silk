## Purpose

Permit unsafe bounded inspection of external variable-sized records without loading their nominal whole object representation.

## ADDED Requirements

### Requirement: Raw byte projection preserves pointer identity

The sealed unsafe pointer operation SHALL project a nullable single readonly typed pointer to a nullable many readonly byte pointer with the same address and address space. It MUST NOT load memory, allocate, reconstruct an address from an integer, grant ownership, or assert a readable extent. Ordinary Pointer source SHALL expose the reusable wrapper. Each subsequent access requires the caller to prove readable initialized storage. Requalification SHALL continue to preserve pointee type.

#### Scenario: Project a short external record

- **WHEN** an external pointer designates a valid prefix shorter than the nominal record type
- **THEN** projection performs no whole-record load and proved prefix bytes can be inspected individually

#### Scenario: Preserve null

- **WHEN** a null typed pointer is projected
- **THEN** the result remains null and cannot be dereferenced without the existing unsafe obligations

#### Scenario: Reject forged output qualifiers

- **WHEN** MIR supplies a mutable, non-byte or incompatible-address-space result for byte projection
- **THEN** verification rejects the instruction before emission
