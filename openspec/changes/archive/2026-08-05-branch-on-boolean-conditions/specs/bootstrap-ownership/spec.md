## ADDED Requirements

### Requirement: Arms scope their bindings and every return is an exit

A binding declared inside a conditional arm SHALL be live from its statement to the end of its
arm and SHALL be released at that arm's boundary — its arm's return exit where one exists,
otherwise the arm's end — never at an exit outside its arm. Every return statement SHALL be its
own exit in the cleanup plan, releasing the bindings live and unconsumed on paths reaching it in
last-acquired, first-released order. A move inside any arm SHALL conservatively count as
consuming for every use after the conditional, keeping the affine check sound without
path-sensitive analysis.

#### Scenario: Release an arm binding inside its arm

- **WHEN** an arm declares `let inner = 1` and returns it while the body declares `let outer = 2`
- **THEN** the arm's return exit releases `inner` then `outer`, and the trailing return exit releases only `outer`

#### Scenario: Treat a conditional move conservatively

- **WHEN** one arm moves a body binding and the trailing return reads it
- **THEN** the later read is an `OWN0001` violation even though the move was conditional
