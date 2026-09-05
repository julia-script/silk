## Purpose

Proves compiler support for ordinary fixed-item iteration with lifetime-bearing payloads without defining a competing public stream library or buffering lifecycle.

## ADDED Requirements

### Requirement: Fixed items remain independent of fresh receiver loans

The compiler SHALL admit ordinary-source fixed-item contracts equivalent to `Stream<Item, E, ?R>` with `take<'call>(&'call mut Self) -> Option<Item> ! E ? R`, including `Item = &'data A`. It SHALL NOT recognize Stream by name or add public buffering operators. JUL-21 retains the public library design. Invocation-scoped lending from self-owned scratch MUST NOT silently acquire a fresh lifetime in a fixed item type.

#### Scenario: Keep successive externally borrowed results

- **WHEN** a source-backed stream returns two items through successive temporary Effects and the wrapper is destroyed
- **THEN** both results remain usable while external backing storage stays valid

#### Scenario: Reject backing invalidation

- **WHEN** external source storage is moved, mutated or destroyed before a retained item use
- **THEN** the compiler rejects the conflicting action

#### Scenario: Preserve copied and moved payload contents

- **WHEN** takeCopy copies an item containing a reference or an owning stream transfers an affine dependent item
- **THEN** the item's nested lifetimes remain required and Copy alone does not imply detached output

#### Scenario: Reject fixed-item scratch lending

- **WHEN** an implementation returns a view into reusable receiver-owned scratch as a fixed external item type
- **THEN** conformance or escape checking rejects the unsupported lifetime promise
