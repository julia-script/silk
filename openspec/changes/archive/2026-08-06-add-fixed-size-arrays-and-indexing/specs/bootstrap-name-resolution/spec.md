## ADDED Requirements

### Requirement: Array element types resolve canonically

Type resolution SHALL recursively resolve an array's element type through the same local, selected,
and namespace-qualified module scope used by ordinary contracts. It SHALL canonicalize the decimal
length without treating `Array` as a source declaration or importing an element type implicitly.

#### Scenario: Resolve an imported element type

- **WHEN** a contract names `Array<Model.Token, 8>` through a valid namespace alias
- **THEN** the array element resolves to the defining canonical `Token` identity and length eight

#### Scenario: Retain an unavailable nested element

- **WHEN** a nested array names a private or unknown external element type
- **THEN** the array type remains explicitly unavailable with the original lookup cause
