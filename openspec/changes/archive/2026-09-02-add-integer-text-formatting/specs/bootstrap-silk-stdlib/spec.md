## ADDED Requirements

### Requirement: Integers render as decimal text and read back

Canonical standard-library source SHALL define, for every integer type the scalar catalog knows, a
function rendering a value as decimal text into an owned `String` and a function reading complete
decimal text back into that type. Both SHALL be ordinary Silk with no compiler privilege: no phase
SHALL know either by name, and no intrinsic SHALL be added for them.

Rendering SHALL allocate, so it SHALL declare the allocation failure and the allocator requirement
the rest of the owned-storage library declares. Reading SHALL neither allocate nor fail as an
effect; it SHALL return a completed outcome carrying either the value or a typed reason.

The radix SHALL be ten. Rendering SHALL emit a leading `-` for a negative value and no sign
otherwise, SHALL emit no grouping separator, and SHALL emit no leading zero beyond the single digit
that spells zero itself. The text of a value SHALL NOT depend on the type it was rendered from: the
same number rendered from a narrow type and from a wide one SHALL be the same bytes.

The text SHALL be identical on the evaluator, the Wasm backend, and the native backend, for every
value including each type's own bounds.

#### Scenario: Render a bound

- **WHEN** a program renders the largest or the smallest value of an integer type
- **THEN** the text spells that bound exactly, on every engine

#### Scenario: Round trip a value

- **WHEN** a program reads the text it rendered from a value of the same type
- **THEN** it receives that value back

#### Scenario: Render a value whose magnitude has no positive counterpart

- **WHEN** a program renders the smallest value of a signed type
- **THEN** the text spells it, without the rendering overflowing on the way

### Requirement: A reading failure names its reason

Reading decimal text SHALL fail with data the caller can narrow, not one opaque error. Text that is
not a decimal number SHALL report the byte offset at which reading stopped; empty text and text
carrying only a sign SHALL report the offset at which a digit was expected. A well-formed decimal
number outside the target type's range SHALL report that it is out of range instead.

An unsigned type SHALL treat a leading `-` as text that is not a number rather than as a smaller
number, because a sign is not part of its spelling.

#### Scenario: Reject text that is not a number

- **WHEN** a program reads text containing a byte that is not a decimal digit
- **THEN** it receives the not-a-number reason carrying that byte's offset

#### Scenario: Reject a value that does not fit

- **WHEN** a program reads a well-formed decimal number larger than the target type holds
- **THEN** it receives the out-of-range reason, and the rejection happens before any overflow

### Requirement: Owned text appends owned text

`String` SHALL provide a companion to its borrowed-text append that appends another owned `String`,
consuming it. Appending SHALL be atomic with respect to allocation failure: a failed allocation
SHALL leave the receiving string exactly as it was.

Appending SHALL NOT copy the receiving string's existing contents into fresh storage in order to
grow. Composing a message from several pieces is the operation this API exists for, and a copy per
piece makes it quadratic in the message length.

#### Scenario: Compose a message from runtime values

- **WHEN** a program appends rendered text to a message and then appends more borrowed text
- **THEN** the message reads as the pieces in order, and every allocation it made is released

#### Scenario: Preserve a message when appending cannot allocate

- **WHEN** the allocator cannot satisfy an append
- **THEN** the caller receives the ordinary allocation failure and the message is unchanged
