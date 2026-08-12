# language-server-code-actions Specification

## Purpose

Defines how the language server presents a diagnostic's machine-applicable edits to the author as
quick fixes, so an unambiguous correction is applied from the editor rather than retyped by hand.

## Requirements

### Requirement: The server advertises quick-fix code actions

The language server SHALL advertise code-action support to compatible clients and SHALL declare
the `quickfix` kind among the kinds it provides.

#### Scenario: Client initializes code-action support

- **WHEN** a compatible client initializes a Silk language-server session
- **THEN** the returned capabilities advertise code-action support declaring the `quickfix` kind

### Requirement: Each diagnostic edit becomes one quick fix

Every `Edit` carried by a diagnostic whose primary span belongs to the requested document SHALL
become exactly one code action of the `quickfix` kind. The action SHALL reference the diagnostic
that carried the edit, and its title SHALL name the correction the edit performs rather than
repeat the diagnostic's message. The action's workspace edit SHALL address the requested
document and SHALL express the edit's byte span as a range in the negotiated position encoding.

#### Scenario: Fix a redundant alias

- **WHEN** a code-action request covers an import whose alias repeats the name it renames
- **THEN** the server returns one `quickfix` titled for removing the redundant alias, referencing that diagnostic, whose edit deletes the ` as name` clause

#### Scenario: Range follows the negotiated encoding

- **WHEN** an edit-carrying diagnostic follows non-ASCII source text on its line
- **THEN** the returned edit range uses the negotiated position encoding rather than raw byte offsets

### Requirement: A diagnostic without an edit produces no action

A diagnostic that carries no `Edit` SHALL contribute no code action, and the server MUST NOT
invent a correction for it. A request covering only such diagnostics SHALL return an empty list.

#### Scenario: An unresolved name offers no fix

- **WHEN** a code-action request covers a diagnostic for an unresolved name, whose correction needs a choice the author must make
- **THEN** the server returns no code action

### Requirement: Quick fixes are deterministic and range-limited

Code actions SHALL be returned in the order of the diagnostics that carried them, so identical
source and snapshot produce identically ordered actions. Only diagnostics whose published range
intersects the requested range SHALL contribute an action.

#### Scenario: Two fixable diagnostics in one file

- **WHEN** a code-action request covers a file holding two edit-carrying diagnostics
- **THEN** the server returns their actions in the same relative order as the diagnostics

#### Scenario: Limit actions to the requested range

- **WHEN** a code-action request covers a range that excludes the only edit-carrying diagnostic in the file
- **THEN** the server returns no code action
