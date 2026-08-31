## ADDED Requirements

### Requirement: Formatting streams through Writer under explicit options

Canonical `silk.format` source SHALL define `FormatOptions`, `Formatter`, and the static `Display`
interface. `Display.display` SHALL receive the displayed value by shared borrow and one mutable
Formatter session, return unit, fail only with `WriterError`, and require exclusive access to the
ordinary `Writer` service. Formatter SHALL carry width, alignment, fill, sign, alternate-form,
zero-padding, precision, and color-permission options and SHALL expose ordinary source helpers for
writing content and padding. It MUST NOT own, capture, select, or replace the Writer provider.

`Display` SHALL mean the default human-readable presentation. Radix-specific or diagnostic
presentations MUST NOT silently reinterpret `Display`; they require separately named presentation
contracts or operations.

#### Scenario: Display through a supplied Writer

- **WHEN** a generic function displays a conforming value with one Formatter session
- **THEN** the emitted bytes reach the lexically supplied mutable Writer and any Writer failure remains typed

#### Scenario: Format with defaults

- **WHEN** a caller displays a value without overriding options
- **THEN** Formatter uses the canonical default alignment, fill, sign, padding, precision, alternate-form, and color policy

#### Scenario: Keep Formatter independent from provider selection

- **WHEN** the same Formatter options are used with two different Writer providers
- **THEN** formatting emits the same requested byte sequence while each provider retains its own effects and failures

### Requirement: Formatting options have deterministic streaming semantics

Width SHALL be a minimum visible Unicode-scalar count, fill SHALL contribute one visible scalar per
repetition, and styling control bytes permitted by the color option SHALL not contribute to width.
Alignment SHALL determine how required fill is divided before and after content. Sign,
alternate-form, zero-padding, and precision SHALL be available to presentation implementations
without forcing unrelated types to invent numeric behavior. A Formatter helper MUST NOT buffer an
unbounded completed rendering merely to discover its width; a Display implementation that honors
width SHALL determine its content width before emission.

The color option SHALL be permission rather than a mandate. When color is false, a conforming
Display implementation MUST NOT emit ANSI styling because of formatting. When color is true, an
implementation MAY emit balanced ANSI SGR styling and SHALL exclude those control bytes from its
reported or calculated visible width.

#### Scenario: Right-align visible content

- **WHEN** content has visible width three and options request width five with right alignment
- **THEN** formatting emits two fill scalars before the content regardless of either sequence's UTF-8 byte length

#### Scenario: Disable color

- **WHEN** color permission is false
- **THEN** formatting emits the unstyled representation with no option-induced ANSI styling bytes

#### Scenario: Permit color without requiring it

- **WHEN** color permission is true for a Display implementation that has no colored presentation
- **THEN** its ordinary uncolored representation remains conforming

#### Scenario: Stream a value with known width

- **WHEN** a Display implementation can determine its content width from the value and options
- **THEN** it emits padding and content directly without first allocating the completed rendering

### Requirement: Every integer has an allocation-free Display conformance

Canonical standard-library source SHALL define an interface-owned inline `Display` conformance for
every signed and unsigned integer type known to the scalar catalog. Integer Display SHALL emit the
canonical decimal representation, including zero and each type's minimum and maximum value, without
an owned `String`, allocator requirement, formatting intrinsic, one-Writer-call-per-digit loop, or
compiler recognition of formatting declarations. It SHALL honor width, alignment, fill, sign,
zero-padding, and precision consistently, while decimal alternate form and color permission SHALL
not change the digits unless a separately documented presentation adds styling.

#### Scenario: Display an integer bound

- **WHEN** an integer's minimum or maximum value is displayed with default options
- **THEN** Writer receives its exact canonical decimal spelling on every supported engine

#### Scenario: Pad a signed integer without allocation

- **WHEN** options request a width larger than a negative integer's sign and digits
- **THEN** formatting places fill or zero padding according to alignment and sign policy without requesting Allocator

#### Scenario: Propagate a Writer failure

- **WHEN** Writer rejects an integer rendering after accepting any prefix
- **THEN** display fails with that `WriterError`, performs no allocator operation, and makes no atomic-output guarantee for the already accepted prefix

#### Scenario: Keep integer formatting ordinary source

- **WHEN** equivalent formatter, interface, and integer implementations are copied under legal user names
- **THEN** they receive the same conformance, Effect, ownership, and lowering behavior without intrinsic registration

### Requirement: Integer parsing survives the rendering rewrite

Every integer actor SHALL continue to parse complete canonical decimal text without allocation and
return either the exact in-range value or the existing typed not-a-number or out-of-range reason.
Removing owned-String rendering MUST NOT change accepted text, rejection offsets, range checks, or
engine parity.

#### Scenario: Parse a displayed integer

- **WHEN** a caller captures an integer's default Display bytes as valid text and parses them through the same integer actor
- **THEN** parsing returns the original value without allocation

#### Scenario: Preserve parse failures

- **WHEN** complete text is malformed or outside the destination integer range
- **THEN** parsing returns the existing precise reason and never depends on a Writer or Formatter

### Requirement: Allocating integer rendering has no compatibility path

The superseded integer APIs that return owned `String` values, their allocation-failure contracts,
and their Formatter-internal append engine SHALL be removed. Canonical source, generated embeddings,
manifests, documentation, examples, and repository callers MUST use the Writer-backed formatting
surface, with no deprecated alias, forwarding wrapper, dual implementation, or hidden conversion
back to owned text.

#### Scenario: Inspect the migrated public surface

- **WHEN** integer and format modules are inspected after the change
- **THEN** no public or private integer rendering path constructs an owned String before writing

#### Scenario: Reject a stale allocating caller

- **WHEN** source calls a removed integer-to-String rendering operation
- **THEN** ordinary name resolution reports that the operation is unavailable rather than selecting a compatibility shim
