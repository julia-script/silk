## MODIFIED Requirements

### Requirement: One integrated elaboration phase constructs HIR

Elaboration SHALL consume the authored module with its semantic context, the closure-wide
declaration index and the containing module's completed name-resolution scope, then resolve every
function body in one integrated phase: local bindings, unqualified or namespace-qualified declaration
references, expression typing, and positional contract validation together with typed TIR
construction. It MUST NOT recollect declaration headers, construct import bindings independently, or
read source or syntax. Elaboration SHALL preserve the existing body diagnostics (`SEM0002`,
`SEM0004`, `SEM0006`, `SEM0007`) with their codes, spans, and reasons while adding the stable
name-resolution diagnostics required by imported references; spans are resolved from authored
anchors through the current presentation. It SHALL return complete ordered facts and diagnostics
rather than throw for source mistakes.

#### Scenario: Elaborate the accepted fixture

- **WHEN** `pub fn main() -> i32 { return 42 }` is elaborated
- **THEN** the result contains one TIR function whose body is a typed `i32` integer-literal return with exact authored provenance and no diagnostics

#### Scenario: Elaborate against the published module scope

- **WHEN** a module scope contains a valid selected public function binding used by one body
- **THEN** elaboration resolves that call through the existing binding and does not rebuild the imported module's headers

#### Scenario: Preserve body diagnostics

- **WHEN** a body contains an out-of-range literal, an unknown call target, an unknown parameter reference, and a wrong-arity call across functions
- **THEN** elaboration reports the same stable codes at the same spans as the superseded analysis
