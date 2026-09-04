# duration-literals Specification

## Purpose

Define readable, exact elapsed-duration source literals that retain ordinary `u64` nanosecond
behavior without introducing a nominal duration type or runtime representation.

## Requirements

### Requirement: Duration literals use a closed whole-component grammar

A duration literal SHALL contain one or more contiguous components, each written as a whole decimal
amount immediately followed by exactly one lowercase unit from `ns`, `us`, `ms`, `s`, `m`, `h`,
`d`, or `w`. The amount SHALL admit the ordinary between-digits `_` separator rule and SHALL admit
leading-zero padding. Trivia SHALL NOT appear within one duration literal. Fractions, exponents,
base-prefixed amounts, uppercase units, full unit names, plural forms, and aliases such as `µs`
SHALL NOT be valid duration components.

#### Scenario: Accept every duration unit

- **WHEN** source spells `1ns 1us 1ms 1s 1m 1h 1d 1w` as separate expressions
- **THEN** each spelling is accepted as one duration literal using its named unit

#### Scenario: Accept separators and leading-zero padding

- **WHEN** source spells `01h05m00s` or `1_000ms`
- **THEN** the complete spelling is accepted without removing its padding or separators

#### Scenario: Reject non-whole and non-decimal components

- **WHEN** source spells `1.5s`, `1e3ms`, or `0x10s`
- **THEN** the spelling is rejected because duration components are whole decimal amounts

#### Scenario: Keep spaced durations separate

- **WHEN** source spells `1h 30m` without an operator
- **THEN** it is not treated as one compound duration literal

### Requirement: Compound duration components are structurally canonical

Components in one duration literal SHALL appear in strictly descending order
`w`, `d`, `h`, `m`, `s`, `ms`, `us`, `ns`, and a unit SHALL appear at most once. Units MAY be
omitted, and zero-valued components SHALL remain valid in any position. The first component MAY
contain any non-negative amount whose scaled total remains representable. Every later component
SHALL remain within its natural field bound: `d < 7`, `h < 24`, `m < 60`, `s < 60`, and
`ms`, `us`, and `ns < 1000`, even when an intermediate unit is omitted.

#### Scenario: Accept an ordered compound with omitted units

- **WHEN** source spells `1h30m30s`, `1h30s`, or `2d500us`
- **THEN** each literal is accepted because its units descend without repetition and every later component is in range

#### Scenario: Preserve zero fields for aligned source

- **WHEN** source spells `0h30m00s`, `1h00m30s`, or `2h15m00s`
- **THEN** each zero component remains a valid part of the compound literal

#### Scenario: Reject reordered or repeated units

- **WHEN** source spells `30s1m` or `1h2h`
- **THEN** the literal is rejected at the first reordered or repeated unit

#### Scenario: Reject an out-of-range subordinate field

- **WHEN** source spells `1w7d`, `1d24h`, `1h60m`, `1m60s`, or `1s1000ms`
- **THEN** the literal is rejected at the subordinate component outside its natural field bound

#### Scenario: Leave the first field unnormalized

- **WHEN** source spells `90s` or `1500ms` as a single-component literal
- **THEN** the literal is accepted without requiring conversion to a larger unit

### Requirement: Duration values are exact u64 nanoseconds

Every duration literal SHALL have the fixed type `u64`, independent of its expected context. Its
value SHALL be the exact checked sum of its components after scaling `ns` by 1, `us` by 1,000,
`ms` by 1,000,000, `s` by 1,000,000,000, `m` by 60 seconds, `h` by 60 minutes, `d` by 24 hours,
and `w` by 7 days. Days and weeks SHALL be fixed elapsed-time units and SHALL NOT acquire calendar,
timezone, daylight-saving, or leap-second behavior. A scaled total greater than `u64.MAX` SHALL be
rejected before executable lowering without truncation, wrapping, or host-number rounding.

#### Scenario: Scale representative literals exactly

- **WHEN** source evaluates `300ms`, `3s`, `1h30m30s`, `1d`, and `1w`
- **THEN** their values are respectively `300_000_000`, `3_000_000_000`, `5_430_000_000_000`, `86_400_000_000_000`, and `604_800_000_000_000` as `u64`

#### Scenario: Refuse contextual retyping

- **WHEN** a duration literal appears where `i64`, `usize`, or another non-`u64` scalar is required
- **THEN** analysis reports the ordinary type mismatch rather than retyping or converting the duration

#### Scenario: Retain u64 in an unconstrained binding

- **WHEN** `let elapsed = 3s` has no expected type
- **THEN** `elapsed` has type `u64` rather than the ordinary integer-literal default `i32`

#### Scenario: Reject a total above u64

- **WHEN** a grammatically valid duration literal scales to more than `18_446_744_073_709_551_615` nanoseconds
- **THEN** analysis reports an exact out-of-range diagnostic before executable lowering

### Requirement: Duration values compose as ordinary u64 values

After a duration literal establishes its value, every call boundary, comparison, and arithmetic
operation SHALL treat it as an ordinary `u64`. Existing `u64` overflow, conversion, and operation
semantics SHALL apply without a duration-specific runtime operation. A duration literal SHALL be
valid as a `u64` constant initializer, and an exported constant SHALL publish the same fixed type
and exact value to importing modules.

#### Scenario: Add separate duration literals

- **WHEN** source evaluates `1h + 30m + 30s`
- **THEN** ordinary homogeneous `u64` addition produces the same value as `1h30m30s`

#### Scenario: Preserve ordinary u64 overflow behavior

- **WHEN** arithmetic between individually valid duration literals exceeds the `u64` range
- **THEN** the operation follows the existing trapping `u64` arithmetic contract rather than wrapping specially for durations

#### Scenario: Publish a duration constant

- **WHEN** a public constant declared as `const timeout: u64 = 3s` is imported by another module
- **THEN** the importer observes an ordinary `u64` constant with exact value `3_000_000_000`

#### Scenario: Preserve target consistency without duration runtime support

- **WHEN** a program returns or compares a duration literal under WebAssembly or native execution
- **THEN** every supported target observes the same ordinary `u64` value and behavior

### Requirement: Formatting preserves authored duration spelling

The canonical source formatter SHALL treat a complete duration literal as indivisible token
content. It SHALL preserve unit spelling, component spelling, digit separators, leading-zero
padding, omitted components, and explicit zero-valued components while formatting surrounding
syntax. A second formatting pass SHALL be byte-stable under the existing line-ending policy.

#### Scenario: Preserve aligned duration literals

- **WHEN** a formatted source sequence contains `01h05m00s`, `02h00m30s`, and `03h10m00s`
- **THEN** formatting preserves each complete literal spelling while canonicalizing only surrounding layout

#### Scenario: Format compound and additive forms without rewriting values

- **WHEN** source contains `waitFor(1h30m)` and `waitFor(1h + 30m)`
- **THEN** formatting preserves each duration token and applies ordinary spacing only around the addition operator
