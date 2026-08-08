## ADDED Requirements

### Requirement: Whole-member bindings extract union payloads

Match arms SHALL accept the whole-member binding form `Member name`, binding the entire matched
member payload as one value instead of destructuring its fields. The binding SHALL follow the
scrutinee's access mode, participate in coverage exactly like a field-destructuring pattern for
the same member, and leave nothing omitted: the binding owns the complete payload, so no
per-field cleanup is planned for the arm.

#### Scenario: Extract an affine member

- **WHEN** an arm binds `Full full` on a moved union scrutinee and the arm result moves the binding onward
- **THEN** the payload transfers exactly once, all three engines agree on the result, and no field of the member is separately released

#### Scenario: Extract an intrinsic result member

- **WHEN** an arm binds `Layout value` on the result of `Layout.repeat`
- **THEN** the binding is a usable `Layout` for allocation and the overflow arm still covers the remaining member
