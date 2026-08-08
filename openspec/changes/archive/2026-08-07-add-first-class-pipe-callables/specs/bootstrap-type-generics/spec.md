## MODIFIED Requirements

### Requirement: Calls infer only from supplied arguments

A complete generic call SHALL either supply the complete ordered type-argument list or infer every
type argument from its supplied call arguments. Forming an automatic leading-argument section
SHALL infer from the supplied trailing arguments and retain any unresolved parameter that is
determined by the omitted leading parameter in the section's callable type; applying that section
SHALL complete inference from the leading argument. Partial explicit type-argument lists, expected
return types, and uses after the complete application MUST NOT contribute inference. Missing,
conflicting, or excess arguments MUST produce deterministic diagnostics at the responsible section
or application.

#### Scenario: Infer identity from its argument

- **WHEN** `identity(value)` calls `identity<T>(value: T)` with a `Token`
- **THEN** the call specializes `T` as `Token`

#### Scenario: Infer through a generic section

- **WHEN** a generic data-first function forms a section from trailing arguments and is then piped a leading `Token`
- **THEN** the complete application resolves one canonical `Token` specialization

#### Scenario: Refuse return-only inference

- **WHEN** `empty()` calls `empty<T>() -> T` without explicit type arguments
- **THEN** specialization fails even when the call result is later used where `Token` is expected

#### Scenario: Specialize explicitly

- **WHEN** `empty<Token>()` calls `empty<T>() -> T`
- **THEN** the call records the concrete `Token` specialization

## ADDED Requirements

### Requirement: Callable specialization remains finite and monomorphic

Generic function references, sections, callable fields, and higher-order applications SHALL reach
runtime only through deterministic concrete callable instances. Specialization MUST NOT introduce
runtime generic dictionaries, type descriptors, or unbounded polymorphic closure families.

#### Scenario: Specialize one generic mapper twice

- **WHEN** the same generic mapper section is reached for `I32` and `Token`
- **THEN** instance discovery records exactly two concrete callable environments and terminates
