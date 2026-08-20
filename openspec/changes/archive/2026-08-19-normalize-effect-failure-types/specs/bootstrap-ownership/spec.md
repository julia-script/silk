## ADDED Requirements

### Requirement: Failure payloads obey ordinary detached ownership

Every value admitted as an Effect failure type SHALL use its ordinary Copy, move, Drop, union-tag,
and cleanup behavior. `fail`, propagation, selective recovery, whole-channel recovery, and re-fail
SHALL transfer one ordinary payload without a row wrapper. A failure payload SHALL be detached and
owned; a lexical or provider borrow that could escape SHALL be rejected by ordinary ownership.

#### Scenario: Propagate an affine ordinary failure once

- **WHEN** an affine failure payload crosses nested Effect calls before recovery
- **THEN** ownership transfers one payload and schedules exactly one cleanup if it remains unconsumed

#### Scenario: Reject an escaping borrowed failure

- **WHEN** `fail` attempts to publish a lexical borrow as the Effect failure value
- **THEN** the ordinary borrow-escape diagnostic rejects it before executable lowering

#### Scenario: Recover a structural failure union

- **WHEN** a handler receives one selected alternative from an ordinary failure union
- **THEN** its pattern narrowing, moves, and cleanup use the same ownership rules as that union in any other value position
