## ADDED Requirements

### Requirement: Preserve nested argument syntax before recursive analysis

When a call argument contains a newly parsed nested call that the current semantic slice does not
yet analyze recursively, semantic analysis SHALL retain the ordered argument identity and exact
nested syntax provenance while marking its expression type and enclosing call contract unavailable.
It MUST NOT reinterpret the nested call as an integer or identifier, invent a positional binding,
or emit a misleading arity or name-resolution diagnostic for facts it has not analyzed.

#### Scenario: Analyze parsed nested syntax before nested resolution exists

- **WHEN** analysis receives `identity(identity(42))` after only the nested parsing change
- **THEN** the outer argument retains its identity and nested call span while its semantic expression and outer contract are explicitly unavailable

#### Scenario: Keep parser-owned nested damage separate

- **WHEN** a nested argument contains parser recovery nodes
- **THEN** semantic analysis preserves an unavailable placeholder without duplicating the parser-owned diagnostic
