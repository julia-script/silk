## ADDED Requirements

### Requirement: Seeded Random is tested through the minimal Silk surface

The pressure corpus SHALL include readable ordinary Silk tests for the existing seeded `Random`
service in both standard-library and representative user-package placement. Application Random
providers SHALL be installed inside each closed test. Both the catalog-rooted standard-library
placement and manifest-rooted user placement SHALL assert the published first scalar with
`Test.assert` and compare the same single-source-of-truth seeded `fillBytes` vector through
`Test.equalBytes`. The witness MUST NOT depend on generic equality, value rendering, assertion
messages, skips, build configuration, or a future SHA actor.

#### Scenario: Test the first seeded scalar

- **WHEN** the Random witness constructs the documented seeded provider and runs a closed marked test
- **THEN** `Test.assert` validates the published first scalar and the default suite reports the case as passed

#### Scenario: Test the published byte vector

- **WHEN** the witness fills a byte buffer from the documented seed
- **THEN** `Test.equalBytes` validates the existing published vector without a host-language assertion loop

#### Scenario: Exercise user and standard-library placement

- **WHEN** equivalent closed tests are rooted through a user manifest and the standard-library catalog
- **THEN** both use the same marker, eligibility, inventory, invocation, assertion, and reporting semantics

### Requirement: Testing acceptance covers customization and command edges

Focused acceptance cases SHALL cover the default runner, an ordinary custom runner, a mutable
custom Reporter with public event fields, default and explicit test roots, distinct runner-root
exclusion, canonical ordering, duplicate-root de-duplication, ASCII case-insensitive filters, exact
non-ASCII and invalid filter bytes, no-match status 2, selected failure status 1, all-pass status 0,
reporting infrastructure failure, source rejection, and fatal traps. Every selected test reached
before infrastructure or fatal termination SHALL run exactly once; later selected tests MUST remain
uninvoked after either termination. Isolated properties SHALL cite frozen prerequisite evidence;
this slice SHALL add only connected witnesses with a distinct composition falsifier.

#### Scenario: Count outcomes with a custom reporter

- **WHEN** a custom runner shares one mutable counting Reporter across selected cases
- **THEN** it observes each completed public Event exactly once, derives one exact nonzero i32 status from its state, and the command preserves that status independently of standard presentation

#### Scenario: Compose runner and test-root roles

- **WHEN** one module is both the configured custom runner and an explicit test root and imports a runner-only helper module
- **THEN** its private marked test enters inventory through the test-root role while marked declarations reachable only through the runner role remain excluded

#### Scenario: Focus a mixed-case substring

- **WHEN** a command filter differs from a matching test ID only by ASCII letter case
- **THEN** only matching cases run in canonical relative order and the resulting status reflects those selected cases

#### Scenario: Prove exact non-ASCII matching

- **WHEN** a non-ASCII test ID is selected once by its exact bytes and separately queried with a Unicode-normalized or non-ASCII case-fold near-miss
- **THEN** only the exact byte sequence matches and the near-miss produces the ordinary no-match result

#### Scenario: Keep an invalid-byte filter inert in OR selection

- **WHEN** an invalid UTF-8 filter is injected at the post-parser HostInput seam alongside one matching ASCII filter
- **THEN** the ASCII match runs in canonical order and the invalid bytes neither decode, reject the command, nor abort selection

#### Scenario: Distinguish complete path from presentation

- **WHEN** a nested helper failure is observed by both the standard reporter and a custom reporter
- **THEN** standard output omits only runner infrastructure frames while the custom reporter observes the complete identical owned StackPath

#### Scenario: Keep the three status classes distinct

- **WHEN** separate suites pass, contain a selected failure, and match no explicit filter
- **THEN** their command statuses are 0, 1, and 2 respectively without conflating source rejection or a fatal trap with a case outcome

#### Scenario: Stop on reporting infrastructure failure

- **WHEN** reporting a reached case returns ReportError after an earlier case completed
- **THEN** status 2 overrides prior case status, the current Event and StackPath are reclaimed exactly once, and every later selected test remains uninvoked

#### Scenario: Reject source before execution

- **WHEN** one connected test graph has a source or eligibility diagnostic and no operational failure
- **THEN** the command returns the source-rejection class, invokes no runner, and fabricates no runner status

#### Scenario: Leave traps fatal

- **WHEN** a reached selected test traps before later selected tests
- **THEN** the trap retains its existing fatal classification, produces no Failed outcome or runner status 0, 1, or 2, and later tests remain uninvoked

### Requirement: Native testing remains actor-agnostic and evaluator-only

The testing findings report SHALL map every SLP-0004 goal and falsifier to a fixture or artifact
assertion, record the frozen owned-StackPath and platform-byte gate results, classify every found
wall and its disposition, and inventory syntax, semantic facts, HIR, MIR, evaluator, intrinsic,
backend, and command artifacts. It SHALL demonstrate that no privileged phase recognizes `Test`,
Reporter, assertion, equality, filtering, presentation, or runner actor spellings. Testing-specific
branches MAY exist only for the marker, inventory, metadata, opaque invocation, owned logical-path
capture, or checked logical-path inspection. Any unproven falsifier, failed prerequisite gate, or
source-actor spelling privilege MUST make the sufficiency finding non-passing and SHALL return
SLP-0004 to Candidate when its accepted revisit rule applies. Renaming equivalent ordinary source
actors SHALL preserve behavior.

Initial acceptance SHALL execute only through the evaluator and SHALL keep the complete deferred
surface outside this slice: future SHA use, generic equality and arbitrary slice/struct comparison,
value rendering, assertion messages and exact callsite events, skips, tags, glob/regex/exclusion
filters, shuffling, parallelism, retries, snapshots, fuzzing, coverage, watch mode, source-visible
target/build configuration, native/Wasm or target-matrix execution, process isolation, and
recoverable-trap behavior.

#### Scenario: Rename ordinary testing actors

- **WHEN** equivalent source wrappers and reporter actors are renamed in a focused fixture
- **THEN** semantic and runtime behavior is unchanged and compiler artifacts contain no actor-specific privilege

#### Scenario: Inspect every privileged branch

- **WHEN** the compiler and intrinsic catalogs are audited after the witness passes
- **THEN** every testing-specific branch is justified solely by marker, opaque inventory, metadata, invocation, owned logical-path capture, or checked logical-path inspection, and final backend evidence is inspected or cited from an unchanged prerequisite artifact

#### Scenario: Keep the initial execution boundary narrow

- **WHEN** the complete pressure suite runs in the initial release
- **THEN** evaluator coverage proves the accepted contract without adding native, WebAssembly, target-matrix, process-isolation, or recoverable-trap modes

#### Scenario: Refuse an incomplete sufficiency finding

- **WHEN** one SLP goal or falsifier lacks evidence, a prerequisite gate fails, a wall lacks a disposition, or a privileged artifact recognizes an ordinary actor spelling
- **THEN** the findings report cannot pass and records the required return or follow-up disposition
