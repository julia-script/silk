## Purpose

Define compile-time-validated Writer-backed formatting over borrowed tuple and record argument packs
without variadic calls, runtime template parsing, reflection tables, or intermediate strings.

## ADDED Requirements

### Requirement: Format templates are explicit static specialization inputs

The ordinary `silk.format` source API SHALL provide a Writer-backed effect operation equivalent to
`format<Args>(static template: string, args: &Args) -> () ! WriterError ? &mut Writer`. A literal
template SHALL satisfy the static parameter directly. A retained non-literal template SHALL require
an explicitly static binding. `args` SHALL remain one ordinary shared runtime reference and its
concrete aggregate type SHALL participate in generic specialization without becoming a static value.

The operation SHALL write through the existing mutable `Writer` requirement and existing `Display`
contract. The standard library SHALL provide ordinary-source `Display<string>` through the same
Writer/string UTF-8 path used by existing presentation operations, without a second text-writing
route or intermediate owned String. The formatting operation MUST NOT allocate or return an
intermediate runtime string.

#### Scenario: Format a borrowed anonymous record temporary

- **WHEN** source runs `Format.format("Hello, {name}", &.{ name: "Julia" })`
- **THEN** the template is consumed during specialization, the anonymous record remains a borrowed runtime temporary, and execution writes `Hello, Julia`

#### Scenario: Reuse a named argument pack

- **WHEN** a local anonymous record is passed to formatting as `&args` more than once
- **THEN** each call reads the same live owner without consuming it

#### Scenario: Display a string field through the ordinary conformance

- **WHEN** `{name}` selects a `string` field containing `"Julia"`
- **THEN** ordinary `Display<string>` writes the existing UTF-8 bytes through Writer and preserves its original failure semantics

### Requirement: The initial template grammar is finite and unambiguous

A template SHALL consist of UTF-8 literal segments, escaped braces `{{` and `}}`, positional
placeholders `{}`, and named placeholders `{name}` where `name` follows the ordinary field-label
identifier grammar. Escaped braces SHALL emit one literal brace. A template MUST use either
positional placeholders or named placeholders, not both. The initial grammar SHALL provide no
format specifiers, dynamic widths, runtime placeholder names, interpolation expressions, or nested
replacement fields.

A positional template SHALL require a tuple or anonymous positional argument pack and consume its
positions from zero in placeholder order. Every position SHALL be consumed exactly once. A named
template SHALL require a record-like argument pack and select each named visible field; a named
field MAY be selected more than once and unrelated visible fields MAY remain unused.

#### Scenario: Format a positional tuple

- **WHEN** source runs `Format.format("My name is {}, I'm {}", &("Julia", 31))`
- **THEN** specialization maps the placeholders to positions zero and one and runtime execution displays them in template order

#### Scenario: Format escaped braces

- **WHEN** a template contains `{{name}}`
- **THEN** specialization retains one literal `{name}` segment and performs no field lookup for that text

#### Scenario: Reject mixed placeholder modes

- **WHEN** one template contains both `{}` and `{name}`
- **THEN** specialization fails at the first conflicting placeholder and emits no runtime formatter body

### Requirement: Template validation completes during specialization

Template parsing and aggregate matching SHALL occur only through ordinary static source and the
static reflection surface. Specialization SHALL fail before runtime HIR is published when braces are
malformed, a placeholder spelling is invalid, a positional count differs from tuple arity, a named
field is absent or inaccessible, the argument is not the required aggregate kind, or a selected
field lacks applicable `Display` evidence. An uncalled formatting specialization SHALL not parse its
template or report its failures.

Validation failures SHALL use the ordinary static diagnostic path, retain the template origin and
applicable transformed UTF-8 byte start and end, and include the static formatting/reflection trace. They MUST NOT
become runtime failures, traps, backend errors, host exceptions, or compiler-known behavior selected
by the `silk.format` spelling.

The parser SHALL represent every plan element as one homogeneous flat `Part` struct containing an
enum mode and ordinary projected fields. It MUST NOT rely on static union matching or introduce a
compiler-known template-part representation.

#### Scenario: Reject an unknown named field

- **WHEN** `{missing}` is selected for `&.{ name: "Julia" }`
- **THEN** specialization reports the placeholder's template byte range and available visible field context and publishes no residual call

#### Scenario: Reject missing display evidence

- **WHEN** a selected argument field has no applicable `Display` implementation
- **THEN** interface selection fails at that placeholder specialization before Writer execution is emitted

#### Scenario: Ignore an unreachable invalid template

- **WHEN** source declares but never reaches a formatting call with a malformed static template
- **THEN** executable realization reports no template diagnostic for that call

### Requirement: Formatting residualizes to ordinary Writer and Display operations

Each literal template segment SHALL residualize to the existing Writer text operation, and each
placeholder SHALL residualize to an ordinary statically selected `Display` operation over the
corresponding shared field projection. Residual operations SHALL retain template order, ordinary
Writer prefix-preservation semantics, and the original runtime error and requirement rows. Runtime
code MUST NOT contain the template, a parser, a placeholder table, a field descriptor, a reflection
loop, a variadic calling convention, or an argument-pack copy.

Specialization SHALL be atomic: a malformed later part or missing later `Display` witness MUST NOT
publish earlier literal writes, calls, projections, instance selections, ownership facts, or an
executable partial formatter body.

#### Scenario: Preserve Writer failure behavior

- **WHEN** the Writer fails after accepting an earlier literal or displayed field
- **THEN** the accepted prefix remains written and the original `WriterError` is preserved without rollback or a formatting-specific runtime error

#### Scenario: Agree across execution engines

- **WHEN** one accepted template specialization runs through evaluation, direct WebAssembly, and native LLVM
- **THEN** every engine performs the same ordered Writer and Display operations with no engine-specific parsing or reflection path
