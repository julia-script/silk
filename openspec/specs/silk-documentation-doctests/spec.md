# Silk Documentation Doctests Specification

## Purpose

Define what a fenced Silk example inside a documentation comment promises, how that promise is
checked, and what a broken example must report.

## Requirements

### Requirement: Doctests are collected from the documentation JSON

The doctest workflow SHALL take its examples from a documentation JSON value produced by
documentation generation, and SHALL collect every fenced code block whose language token names Silk,
from module documentation and from the documentation of every declaration and every nested child.
It MUST NOT re-parse `///` comments out of source text to find examples, and MUST NOT require any
addition to the documentation JSON schema.

Collection SHALL be deterministic and SHALL preserve the JSON's source order, so two runs over the
same value report the same examples in the same sequence.

#### Scenario: Collect an example from a nested declaration

- **WHEN** a documented function parameter, struct field, or service operation carries a fenced Silk block
- **THEN** the workflow collects that block as an example of the declaration that owns it

#### Scenario: Ignore a block in another language

- **WHEN** a documentation comment carries a fenced block whose language token is absent or names something other than Silk
- **THEN** the workflow collects no example from it and reports nothing about it

### Requirement: A fence opts out through a comma-delimited attribute

Fence attributes SHALL be read from the language token as comma-delimited words, because the
documentation JSON records that token and does not record the rest of the info string. A block
fenced ` ```silk,ignore ` SHALL be collected and SHALL be reported as skipped rather than
compiled.

An attribute the workflow does not recognize SHALL fail the example rather than being discarded, so
a misspelled marker cannot silently turn an opted-out example back into a compiled one.

Because a space-separated attribute is dropped before the JSON is written, a block fenced
` ```silk ignore ` inside a documentation comment is not opted out. A failure report for an
example whose language token is exactly Silk SHALL name the comma-delimited form, so an author who
wrote the space-separated form is told the form that works.

#### Scenario: Skip an opted-out example

- **WHEN** an example is fenced ` ```silk,ignore `
- **THEN** the workflow reports it as skipped, does not compile it, and does not fail

#### Scenario: Reject an unknown attribute

- **WHEN** an example is fenced with a Silk language token carrying an attribute the workflow does not define
- **THEN** the example fails and the report names the unknown attribute

### Requirement: An example compiles as a complete module

An example without an opt-out attribute SHALL be compiled as one complete Silk module, exactly as
written. The workflow MUST NOT prepend an implicit prelude, synthesize a wrapping declaration, or
concatenate an example with the module that documents it.

An example SHALL fail when compiling it reports any diagnostic, and SHALL pass otherwise.

#### Scenario: Compile a whole-module example

- **WHEN** an example is a complete module that compiles without a diagnostic
- **THEN** the example passes

#### Scenario: Fail a wrong example

- **WHEN** an example names a declaration that does not exist, or is otherwise rejected by the compiler
- **THEN** the example fails and the report carries every diagnostic the compiler produced for it

### Requirement: A failure names the file, the line, and the declaration

A failure report SHALL identify the example by the source identity the documentation JSON carries
for it, by a one-based line number within that source, and by the module and declaration whose
documentation holds the example. The line SHALL be derived from the byte offset in the JSON's source
range against the bytes of that source, so the reported position is the position in the original
file rather than a position within the extracted example.

Source bytes SHALL be supplied to the workflow by its caller rather than resolved by a policy the
workflow owns, so the standard library and a user project can each answer for their own layout. An
example whose source cannot be supplied SHALL still be compiled and still be reported, with its
position reported as unavailable rather than as a guessed line.

#### Scenario: Report a failing example with its position

- **WHEN** an example fails to compile
- **THEN** the report carries its source identity, its one-based line, its declaring module, its declaring declaration, and its diagnostics

#### Scenario: Report an example whose source is unavailable

- **WHEN** an example fails and its source bytes cannot be supplied
- **THEN** the report still identifies the source and the declaration, and reports the line as unavailable

### Requirement: The workflow reports a non-zero status when any example fails

A run SHALL summarize how many examples were compiled, how many passed, how many were skipped, and
how many failed, and SHALL exit non-zero when at least one example failed. A run that finds no
examples at all SHALL be reported as such rather than as success, because an empty run and a
verified run are not the same outcome.

#### Scenario: Fail the run on one failing example

- **WHEN** a run compiles many passing examples and one failing example
- **THEN** the run exits non-zero and the summary counts one failure

#### Scenario: Distinguish an empty run

- **WHEN** a run finds no fenced Silk examples
- **THEN** the summary reports zero collected examples rather than reporting a successful verification
