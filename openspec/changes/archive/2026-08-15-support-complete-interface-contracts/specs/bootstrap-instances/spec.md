## ADDED Requirements

### Requirement: Generic interface witnesses specialize mapped targets

Instance discovery SHALL retain mapped generic witness functions with their inferred ordinary type,
failure-row, requirement-row, and representation arguments. The canonical witness key SHALL include
the concrete provider/interface application and mapped target arguments, and MIR SHALL receive one
direct target with no runtime dictionary.

#### Scenario: Discover two mapped target specializations

- **WHEN** two concrete providers select one generic witness declaration with different kinded arguments
- **THEN** discovery records two concrete witness target instances in deterministic order

#### Scenario: Reject an unresolved target binder

- **WHEN** a mapped witness target has a generic binder not inferable from its conformance and operation contract
- **THEN** analysis rejects the mapping before instance discovery can create an open key
