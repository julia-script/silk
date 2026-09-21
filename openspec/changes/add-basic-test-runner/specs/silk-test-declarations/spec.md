## Purpose

Define explicit test declarations while preserving Silk's ordinary function, Effect, visibility, and ownership semantics.

## ADDED Requirements

### Requirement: Test qualification preserves ordinary declaration syntax and meaning

Silk SHALL accept `[pub] test fn` and `[pub] test effect fn` on named functions at module declaration scope, including selected module-level static branches. The contextual `test` qualifier SHALL be retained losslessly with its source span and in authored declaration metadata. It SHALL NOT make an ordinary function effectful or change ordinary identifier/module-path uses of the spelling `test`. Formatting, highlighting, inspection, and declaration tooling SHALL preserve and distinguish the qualifier.

#### Scenario: Declare an Effect test

- **WHEN** source declares `test effect fn additionWorks() ! string { if 1 + 1 != 2 { fail "expected two" } }`
- **THEN** it declares a test whose call constructs an ordinary unit-success Effect with failure type `string`

#### Scenario: Preserve a plain function

- **WHEN** source declares `test fn someTest() {}`
- **THEN** it declares an ordinary unit-returning test function with no implicit failure or requirement channel

#### Scenario: Recover a damaged declaration locally

- **WHEN** a malformed test function is followed by a valid declaration
- **THEN** lossless parsing retains the test qualifier and damaged syntax, and recovery preserves the following declaration

### Requirement: Test entry contracts are finite and parameterless

A test SHALL be a safe runtime function with a body, zero parameters, no generic or lifetime binders, and unit success, written explicitly or obtained through ordinary omitted-unit syntax. Applying `test` to static, unsafe, foreign/exported, anonymous, associated, or non-function declarations SHALL be rejected. Ordinary naming and duplicate-declaration rules SHALL apply. An Effect test's typed failures and service requirements SHALL remain its ordinary precise contract; the qualifier SHALL NOT restrict failure payloads to a special assertion type or erase requirements.

#### Scenario: Reject a parameterized test

- **WHEN** a test declares a value parameter or generic binder
- **THEN** analysis reports a diagnostic at the invalid test contract rather than choosing arguments or specializing it implicitly

#### Scenario: Reject a non-unit test

- **WHEN** a plain or Effect test declares `i32` success
- **THEN** analysis rejects the test entry contract rather than interpreting that integer as a result status

#### Scenario: Preserve a requirement for a future host

- **WHEN** an otherwise valid Effect test declares a service requirement
- **THEN** its test declaration and typed callable retain that requirement, and any runner invoking it must supply or propagate it through ordinary rules

### Requirement: Test qualification grants bounded discovery authority

A private test SHALL be eligible for discovery and typed callable extraction under an authorized test-discovery request. The qualifier SHALL NOT make that declaration public, relax ordinary qualified name resolution, or expose other private declarations. Test bodies SHALL retain their normal lexical access to private helpers.

#### Scenario: Execute a private test using a private helper

- **WHEN** a discovered private test calls a private helper in its own module
- **THEN** a source runner can invoke the test through its descriptor and the helper call retains ordinary module-local resolution

#### Scenario: Reject an unrelated private call

- **WHEN** a runner directly names that private helper or the private test through an ordinary cross-module qualification
- **THEN** normal visibility checking rejects the access

### Requirement: Ordinary builds do not implicitly execute or retain tests

Outside a test-discovery executable, active test declarations SHALL undergo ordinary analysis and remain ordinarily callable where visible. Merely adding the qualifier SHALL NOT retain their bodies as executable roots, call them, activate test-only imports, or install a runner. Inactive declarations SHALL follow existing module static-selection rules.

#### Scenario: Build an application containing an unused test

- **WHEN** an application imports a module containing an otherwise unreferenced test
- **THEN** the test is analyzed but its qualifier alone contributes no executable call or retention root

#### Scenario: Exclude an inactive test

- **WHEN** a test occurs only in an inactive module-level branch
- **THEN** it is not an active test declaration and contributes no discovery entry
