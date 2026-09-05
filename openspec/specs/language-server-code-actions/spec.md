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

A diagnostic that carries no `Edit` SHALL contribute no diagnostic-derived replacement action, and
the server MUST NOT invent corrected bytes from diagnostic prose. A compiler-owned source-action
query MAY nevertheless use the same accepted semantic snapshot and diagnostic identity to offer a
candidate-generating correction whose applicability and complete change plan are established
independently of `Diagnostic.Edit`. A request for which neither mechanism applies SHALL return an
empty list.

#### Scenario: A non-actionable diagnostic remains without a fix

- **WHEN** a code-action request covers a diagnostic with no edit and no applicable compiler-owned source action
- **THEN** the server returns no action for that diagnostic

#### Scenario: An unresolved name offers auto-import choices

- **WHEN** a code-action request covers an unresolved name with importable declarations in two modules
- **THEN** the server returns the two compiler-owned auto-import actions even though the unresolved-name diagnostic carries no embedded edit

### Requirement: Quick fixes are deterministic and range-limited

Code actions SHALL be returned in deterministic order for one accepted snapshot. Diagnostic-derived
actions SHALL preserve diagnostic order, and candidate-generating actions for one diagnostic SHALL
preserve their compiler-owned ranking. Only diagnostics and source-action targets whose published
range intersects the requested range SHALL contribute an action.

#### Scenario: Two fixable diagnostics in one file

- **WHEN** a code-action request covers a file holding two edit-carrying diagnostics
- **THEN** the server returns their actions in the same relative order as the diagnostics

#### Scenario: Order several auto-import candidates

- **WHEN** one unresolved reference has several applicable auto-import candidates
- **THEN** the candidates appear contiguously in their deterministic compiler-owned order

#### Scenario: Limit actions to the requested range

- **WHEN** a code-action request covers a range that excludes the only edit-carrying diagnostic and every source-action target in the file
- **THEN** the server returns no code action

### Requirement: Unused import warning and action

The server SHALL publish Warning-severity `LSP0004` on the exact authored name of each accepted unused import binding and SHALL expose an unresolved “Remove unused import” quick fix when the compiler supplies a current plan. Invalid, recovered, conflicting, and non-effective bindings and declarations owned by LSP0001-LSP0003 SHALL not receive an unused cascade. Redundancy ownership SHALL be declaration-local and MUST NOT hide an unrelated LSP0004 in the same document. Resolving the action SHALL reacquire the same binding and plan from the exact document version before returning an edit.

#### Scenario: Current action

- **WHEN** an unused-import action is resolved against the exact accepted document version and the same binding remains safely removable
- **THEN** the server returns the compiler-owned workspace edit titled “Remove unused import”

#### Scenario: Stale snapshot

- **WHEN** source changes after action discovery
- **THEN** the server disables the action and stale byte offsets are never returned as a workspace edit

#### Scenario: Unrelated redundancy

- **WHEN** one import is owned by LSP0001-LSP0003 and a separate declaration contains an unused valid binding
- **THEN** the server publishes the redundancy diagnostic and the unrelated LSP0004 without a cascade on the owned declaration

### Requirement: Make inferred lifetimes explicit uses a current compiler-owned edit

The server SHALL offer Make lifetimes explicit for supported elided declarations and signature type occurrences when compiler facts establish a complete semantics-preserving expansion. The action SHALL use the accepted document version, negotiated position encoding, stable nonconflicting binder names, the exact retained environment annotation and type/lifetime outlives predicates, and the requested target range. Resolving a stale action SHALL disable it rather than return stale byte offsets. The server MUST NOT infer a public lifetime relationship from function bodies or fabricate an edit for ambiguous or unavailable lifetime facts.

#### Scenario: Expand implicit field parameters

- **WHEN** a current code-action request targets a struct with two independently omitted borrowed field lifetimes
- **THEN** the edit adds distinct lifetime binders and annotates each field while preserving comments and canonical semantic identity

#### Scenario: Disable a stale lifetime expansion

- **WHEN** the document changes between discovering and resolving Make lifetimes explicit
- **THEN** the action is disabled and no stale workspace edit is returned

#### Scenario: Avoid guessing an ambiguous result

- **WHEN** a declaration has two borrowed inputs and an unresolved elided result relationship
- **THEN** no semantics-changing Make lifetimes explicit edit is invented from its return body
