# bootstrap-static-text Specification

## Purpose
Provide immutable static UTF-8 and byte literals for program data and output without fixing the representation or service model of Silk's eventual owning string type.
## Requirements
### Requirement: Static text literals preserve Unicode content

An escaped text literal using either one-quote or three-quote delimiters SHALL decode escapes into
immutable program-lifetime UTF-8, retain exact source provenance, and expose its bytes and `usize`
byte length without allocation. Multiline decoding SHALL retain every content character without
automatic dedenting or removal of structural-looking first or last line endings. Physical CRLF
pairs inside multiline content SHALL decode as LF; an explicit `\r\n` escape sequence SHALL decode
as CR followed by LF. The literal MUST NOT define the owning `string` layout, mutability, or target
representation.

#### Scenario: Materialize non-ASCII text

- **WHEN** a literal contains Unicode scalar values and escapes
- **THEN** its view exposes the exact decoded UTF-8 bytes on every target

#### Scenario: Preserve multiline text content

- **WHEN** a triple-delimited text literal contains an initial newline, indented lines, trailing spaces, and a final newline before its closing delimiter
- **THEN** every newline and space belongs to the decoded value except that each physical CRLF pair is represented by one LF

#### Scenario: Request exact CRLF text

- **WHEN** a text literal contains the explicit escapes `\r\n`
- **THEN** its decoded UTF-8 bytes contain CR followed by LF rather than the physical-line-ending normalization

### Requirement: Static byte literals preserve exact bytes

An escaped byte-string literal using either one-quote or three-quote delimiters SHALL expose
immutable program-lifetime `u8` values. Literal content and escapes SHALL be decoded atomically,
without automatic dedenting or structural newline removal. Physical CRLF pairs inside multiline
content SHALL decode as the single byte LF; explicit `\r\n` escapes SHALL decode as CR followed by
LF. Decoded values outside the byte range and malformed escapes SHALL produce deterministic
diagnostics without partial data.

#### Scenario: Materialize bytes

- **WHEN** source contains `b"life\n"`
- **THEN** the view exposes four ASCII letters and one newline byte

#### Scenario: Preserve multiline byte indentation

- **WHEN** a triple-delimited byte literal contains physical newlines and leading or trailing spaces on its content lines
- **THEN** the decoded byte view retains those spaces and normalized LF bytes exactly without dedenting

#### Scenario: Request exact CRLF bytes

- **WHEN** a byte literal contains the explicit escapes `\r\n`
- **THEN** the decoded view contains byte `0x0D` followed by byte `0x0A`

### Requirement: Escaped literal policies are independent from delimiter width

Single-line and multiline forms of the same text-or-byte category SHALL accept the same escape
vocabulary and reject the same malformed escapes. A backslash followed by a physical line ending
SHALL be invalid and MUST NOT remove that line ending or any following whitespace. One or two
unescaped consecutive quotes SHALL be content in a triple-delimited literal; the first unescaped
run of three quotes SHALL be its closing delimiter. Raw escape behavior SHALL require a separately
recognized future modifier and MUST NOT be inferred from delimiter width.

#### Scenario: Decode the same escape in both widths

- **WHEN** corresponding single-line and multiline literals contain `\n`, `\x41`, or another escape valid for their category
- **THEN** each pair produces identical decoded bytes for that escape

#### Scenario: Reject physical line continuation

- **WHEN** a backslash is followed immediately by a physical line ending inside a multiline literal
- **THEN** literal decoding reports one malformed-escape diagnostic and publishes no partial static data

#### Scenario: Include the triple delimiter as content

- **WHEN** a triple-delimited literal body spells three escaped quotes as `\"\"\"`
- **THEN** the decoded value contains three quote characters without closing at that escaped sequence

### Requirement: Static storage is target-neutral compiler data

Literal identity and content SHALL be deterministic before backend selection. Backends MAY place or coalesce data differently, but observable bytes, length, immutability, and lifetime MUST match evaluation.

#### Scenario: Reuse a literal

- **WHEN** one literal is referenced multiple times
- **THEN** all references observe identical bytes regardless of storage coalescing

### Requirement: Static byte views support checked indexing

An immutable static byte view SHALL expose its `usize` byte length and support ordinary runtime
indexing that produces one `u8` without allocation. Indexing MUST use the same bounds behavior and
source provenance as an immutable runtime slice.

#### Scenario: Read committed binary bytes

- **WHEN** source indexes `b"\x99\x13\x1d\x00"` at each valid position
- **THEN** it observes `153`, `19`, `29`, and `0` in order without allocating

#### Scenario: Reject a static-byte overrun

- **WHEN** source indexes a four-byte static view at index four
- **THEN** execution traps at the indexing expression with the same bounds contract as a shared slice

### Requirement: Text and byte literals retain distinct semantic types

A valid text literal SHALL elaborate directly to `string` while retaining its decoded immutable
UTF-8 data, source provenance, and program-lifetime storage. A byte-string literal SHALL continue
to elaborate to an immutable `u8` view. The compiler MUST NOT use one shared slice type for both
literal categories.

#### Scenario: Elaborate corresponding literal spellings

- **WHEN** source contains `"silk"` and `b"silk"`
- **THEN** the first expression has type `string`, the second has type `&[u8]`, and both retain the same four encoded bytes

#### Scenario: Preserve an allocation-free text literal

- **WHEN** a valid non-ASCII text literal is evaluated without conversion to owned `String`
- **THEN** it remains an allocation-free program-lifetime `string` with exact decoded UTF-8 content
