## ADDED Requirements

### Requirement: Enum backing-value observation uses one sealed intrinsic

The compiler SHALL expose `Intrinsic.enumValue` as the sole source-callable primitive needed to
observe a verified scalar enum's declared discriminant. Each enum declaration's generated
`EnumName.value` associated wrapper SHALL lower through that intrinsic using the enum's canonical
representation result type. No backend or semantic phase SHALL recognize the wrapper name `value` or
a standard-library declaration by spelling, and the intrinsic SHALL NOT construct an enum from an
integer or add conversion policy beyond backing-value projection.

#### Scenario: Lower a generated value wrapper

- **WHEN** source calls `Status.value(Status.Unknown)`
- **THEN** resolution selects the wrapper contributed by the canonical `Status` declaration and lowering uses `Intrinsic.enumValue` with result type `Status`'s representation

#### Scenario: Keep reverse conversion unprivileged

- **WHEN** source attempts to construct `Status` from its representation integer
- **THEN** no enum intrinsic or generated wrapper accepts the operation
