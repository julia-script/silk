## ADDED Requirements

### Requirement: Struct facts retain their physical-layout contract

Every semantic struct fact SHALL identify whether its physical layout is Silk-private or promised under the C ABI. Source C-layout records SHALL retain ABI `C`; ordinary source structs, tuples, and compiler-synthesized aggregates SHALL explicitly retain Silk-private layout. An invalid ABI or C-layout field SHALL remain represented as diagnostic-backed unavailability rather than silently becoming a valid C-layout promise.

#### Scenario: Record a C-layout fact

- **WHEN** declaration analysis completes a valid `extern "C" struct Timespec`
- **THEN** the struct fact retains ABI `C` independently of its visibility and field order

#### Scenario: Keep generated aggregates private

- **WHEN** analysis creates tuple or anonymous aggregate facts
- **THEN** every generated fact explicitly carries Silk-private layout
