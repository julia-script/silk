## MODIFIED Requirements

### Requirement: Generated module references present canonical import forms

The standard-library reference renderer SHALL present a selected import of the module's public
owner type for each nonprimitive module and an unaliased namespace import for each primitive
module. The rendered instruction SHALL be valid source for the same generated module revision.

#### Scenario: Render a nonprimitive module import

- **WHEN** the renderer emits the reference page for `silk/raw_buffer`
- **THEN** the page presents `import silk.raw_buffer { RawBuffer }`

#### Scenario: Render a primitive module import

- **WHEN** the renderer emits the reference page for `silk/u32`
- **THEN** the page presents `import silk.u32` without an alias or selected list

#### Scenario: Compile rendered examples

- **WHEN** documentation validation collects examples containing canonical imports
- **THEN** every non-ignored example resolves its preserved qualifiers without missing-member diagnostics
