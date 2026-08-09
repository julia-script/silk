# language-server-inlay-hints Specification

## Purpose

Defines lightweight inferred-type annotations for Silk local bindings, derived from immutable
compiler inference facts and exposed through the standard LSP inlay-hint protocol.

## Requirements

### Requirement: The server advertises inferred-type inlay hints

The language server SHALL advertise inlay-hint support and SHALL answer range requests from the
exact synchronized document version and negotiated position encoding.

#### Scenario: Client initializes inlay-hint support

- **WHEN** a compatible client initializes a Silk language-server session
- **THEN** the returned capabilities advertise inlay-hint support

### Requirement: Available local bindings receive type hints

For each local binding in the requested range whose inferred type is available, the language server
SHALL place a type inlay hint immediately after the declared binding name. The hint SHALL preserve
the source-level type presentation used by hover and SHALL not alter the document text.

#### Scenario: Hint an inferred allocator binding

- **WHEN** the requested range contains `let mut allocator = SystemAllocator.make()`
- **THEN** an inlay hint displaying `: SystemAllocator` appears after `allocator`

#### Scenario: Limit hints to the requested range

- **WHEN** a document contains bindings both inside and outside the requested inlay-hint range
- **THEN** the server returns hints only for binding names in that range

### Requirement: Unavailable inference produces no speculative hint

A binding with an unavailable, ambiguous, or damaged inferred type SHALL produce no type hint.
Damage in another binding or declaration MUST NOT suppress hints whose inferred types remain
available.

#### Scenario: Binding inference is unavailable

- **WHEN** a recovered initializer leaves one binding's inferred type unavailable
- **THEN** that binding receives no speculative type hint

#### Scenario: Unrelated binding remains available

- **WHEN** one binding is damaged and a later binding retains an available inferred type
- **THEN** the later binding still receives its type hint

### Requirement: Inlay hints are deterministic and deduplicated

Identical source, snapshot, and requested range SHALL produce identically ordered inlay hints. Each
binding name SHALL receive at most one inferred-type hint even when nested semantic facts overlap.

#### Scenario: Repeat an inlay-hint request

- **WHEN** the same document range is queried repeatedly from identical snapshots
- **THEN** the hint positions, labels, kinds, and order are identical without duplicates
