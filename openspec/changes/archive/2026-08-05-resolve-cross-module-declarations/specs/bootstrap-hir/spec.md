## MODIFIED Requirements

### Requirement: One integrated elaboration phase constructs HIR

Elaboration SHALL consume the closure-wide declaration index and the containing module's completed
name-resolution scope, then resolve every function body in one integrated phase: local bindings,
unqualified or namespace-qualified declaration references, expression typing, and positional
contract validation together with HIR construction. It MUST NOT recollect declaration headers or
construct import bindings independently. Elaboration SHALL preserve the existing body diagnostics
(`SEM0002`, `SEM0004`, `SEM0006`, `SEM0007`) with their codes, spans, and reasons while adding the
stable name-resolution diagnostics required by imported references. It SHALL return complete ordered
facts and diagnostics rather than throw for source mistakes.

#### Scenario: Elaborate the accepted fixture

- **WHEN** `pub fn main() -> I32 { return 42 }` is elaborated
- **THEN** the result contains one HIR function whose body is a typed `I32` integer-literal return with exact source provenance and no diagnostics

#### Scenario: Elaborate against the published module scope

- **WHEN** a module scope contains a valid selected public function binding used by one body
- **THEN** elaboration resolves that call through the existing binding and does not rebuild the imported module's headers

#### Scenario: Preserve body diagnostics

- **WHEN** a body contains an out-of-range literal, an unknown call target, an unknown parameter reference, and a wrong-arity call across functions
- **THEN** elaboration reports the same stable codes at the same spans as the superseded analysis

## ADDED Requirements

### Requirement: Cross-module calls are ordinary canonical HIR calls

A call resolved through a selected-member binding or namespace binding SHALL lower into the same HIR
call operation as a local function call, carrying the imported function's indexed canonical
declaration identity, typed ordered arguments, result type, and exact call-site provenance. The HIR
MUST NOT carry import aliases, module traversal state, runtime namespace objects, or a distinct
cross-module call operation. Missing, conflicting, unknown-member, and inaccessible-member lookups
SHALL remain explicit unavailable HIR expressions carrying their originating diagnostic cause.

#### Scenario: Elaborate a selected imported call

- **WHEN** root `main` calls a uniquely selected public `answer()` from module `library/Answer`
- **THEN** HIR contains an ordinary typed call targeting canonical declaration `library/Answer.answer`

#### Scenario: Elaborate a namespace-qualified call

- **WHEN** root imports `library.Answer as Answers` and calls `Answers.answer()`
- **THEN** HIR contains the same canonical call target as the selective form while retaining the qualified call's source span

#### Scenario: Elaborate a private local call

- **WHEN** a public function calls a unique private helper in its own module
- **THEN** HIR resolves the helper's canonical local identity and preserves its private visibility only as declaration metadata

#### Scenario: Keep an inaccessible imported call unavailable

- **WHEN** a qualified call names a private function in another module
- **THEN** HIR contains an unavailable expression caused by the inaccessible-member diagnostic and no call target

#### Scenario: Encode cross-module HIR deterministically

- **WHEN** equivalent cyclic or acyclic closures are elaborated repeatedly in fresh processes
- **THEN** every module's HIR encoding names identical canonical call targets and is byte-identical
