## MODIFIED Requirements

### Requirement: Arms scope their bindings and every return is an exit

A binding declared inside a conditional or ordinary match arm SHALL be live from its statement to the end of its
arm and SHALL be released at that arm's boundary — its arm's return exit where one exists,
otherwise the arm's end — never at an exit outside its arm. Every return statement SHALL be its
own exit in the cleanup plan, releasing the bindings live and unconsumed on paths reaching it in
last-acquired, first-released order. For ownership after a match, only paths reaching its continuation SHALL participate in the join. A move on any such path SHALL conservatively count as consuming for every subsequent use; a path that returns, fails, breaks, or continues out of that continuation SHALL NOT invalidate a value used only on a surviving path. Other conditional joins SHALL retain their existing conservative behavior.

#### Scenario: Release an arm binding inside its arm

- **WHEN** an arm declares `let inner = 1` and returns it while the body declares `let outer = 2`
- **THEN** the arm's return exit releases `inner` then `outer`, and the trailing return exit releases only `outer`

#### Scenario: Treat a conditional move conservatively

- **WHEN** one arm moves a body binding and the trailing return reads it
- **THEN** the later read is an `OWN0001` violation even though the move was conditional

#### Scenario: Join only match paths that continue

- **WHEN** one ordinary arm moves an outer owner and returns while another arm reaches the match continuation with that owner live
- **THEN** the owner remains usable after the match on the continuing path and every return path retains its independent cleanup plan

#### Scenario: Reject a move on a completing arm

- **WHEN** one reachable ordinary arm moves an outer owner and completes normally while another leaves it live
- **THEN** a use after the match receives `OWN0001` at the later invalid reference span

## ADDED Requirements

### Requirement: Ordinary arm ownership follows selected statements and expression transfers

Ordinary arm blocks SHALL introduce lexical ownership scope without a capture environment. A Boolean-false guard SHALL leave provisional bindings, loans, and payload ownership available to later candidate arms; ownership SHALL commit only for the selected arm. A transfer during guard evaluation SHALL take its ordinary exit with applicable provisional cleanup and SHALL NOT advance to a later candidate. Normal completion and each early transfer SHALL clean live arm owners, omitted consumed fields, and earlier enclosing-expression temporaries exactly once according to ordinary acquisition and transfer rules. Loans SHALL end before owner cleanup, narrowed borrows and block locals SHALL NOT escape, and cleanup belonging only to a continuation SHALL NOT execute on a path that never reaches it.

#### Scenario: Reject a guarded block before ownership commits

- **WHEN** a consuming guarded candidate has an ordinary block but its guard is false before a later candidate selects the payload
- **THEN** the rejected block performs no statements or cleanup, and the later selected arm owns the complete available payload exactly once

#### Scenario: Clean earlier arguments after a nested return

- **WHEN** earlier call arguments acquire live owned temporaries and a later match argument selects a block that returns
- **THEN** the current arm owners and untransferred earlier temporaries are released exactly once; later arguments and the call do not execute

#### Scenario: End borrowed selection before a loop transfer

- **WHEN** a borrowed ordinary arm inside an enclosing loop executes break or continue with live block locals
- **THEN** the arm loans end before affected owners are released, and the lexical loop transfer receives exactly the required cleanup

#### Scenario: Release omitted fields on early failure

- **WHEN** a consuming block arm binds one affine field, omits another with `..`, and fails after transferring the bound field onward
- **THEN** cleanup releases only still-owned active fields once, with no cleanup for inactive members, the consumed source, or the transferred field

#### Scenario: Clean a guard transfer without selecting another candidate

- **WHEN** evaluating a guard reaches a nested ordinary arm that returns or transfers to an enclosing loop
- **THEN** ownership ends provisional loans and releases the obligations left live on that exit exactly once, without executing or committing a later candidate
