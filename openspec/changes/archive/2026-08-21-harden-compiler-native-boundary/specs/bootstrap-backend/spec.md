## ADDED Requirements

### Requirement: Expected request-validation failures yield BackendError

A backend SHALL model every expected caller-caused failure (invalid MIR, invalid module, invalid target, invalid request parameters) in its typed `BackendError` channel. It SHALL NOT throw inside an Effect generator for an expected failure.

#### Scenario: An invalid private stack page bound is a typed failure

- **WHEN** a wasm emit request specifies an invalid `privateExecutionStackPages` bound
- **THEN** the backend yields a `BackendError`, never a thrown `RangeError` defect, and error-channel mapping observes it
