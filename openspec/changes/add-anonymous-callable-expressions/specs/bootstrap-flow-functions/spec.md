## ADDED Requirements

### Requirement: Effectful anonymous callables preserve both delayed boundaries

Evaluating an `effect fn` anonymous expression SHALL acquire its implicit lexical captures and
produce an exact callable value without entering the authored body. Invoking that callable SHALL
evaluate its supplied arguments and construct one exact Effect value with the declared success,
failure, and requirement channels without entering the body. Running that Effect SHALL enter the
body. The returned Effect SHALL retain, borrow, or consume the callable environment consistently
with the derived callable mode and existing Effect recipe identity rules.

#### Scenario: Delay an inline recovery handler body until run

- **WHEN** an inline effectful recovery handler captures an observable lexical value
- **THEN** literal evaluation acquires the capture, handler invocation constructs the Effect, and only running that Effect observes the body

#### Scenario: Do not duplicate a consuming capture through Effect construction

- **WHEN** an effectful anonymous callable moves an affine lexical value
- **THEN** its mode is `once fn`, one invocation may transfer that environment into the returned Effect, and repeated invocation is rejected
