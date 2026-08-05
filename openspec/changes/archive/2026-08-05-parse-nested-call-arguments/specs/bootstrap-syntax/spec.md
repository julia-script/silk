## ADDED Requirements

### Requirement: Parse nested call arguments losslessly
The bootstrap parser SHALL accept a call expression wherever a call argument expression is allowed.
It SHALL preserve each nested call and argument list as its own concrete branch with every token,
separator, trivia slice, and owner-qualified half-open byte span retained exactly once. This grammar
extension MUST NOT imply that nested calls are already semantically resolved or evaluated.

#### Scenario: Parse one nested identity call
- **WHEN** a function returns `identity(identity(42))`
- **THEN** the outer argument contains a complete inner call-expression branch whose literal `42` and both parenthesis pairs retain exact source order and spans

#### Scenario: Parse two nested arguments
- **WHEN** a function returns `choose(identity(1), identity(2))`
- **THEN** both outer arguments contain independent nested call branches separated by the outer comma

#### Scenario: Recover a damaged inner call
- **WHEN** damaged inner syntax reaches an outer sibling boundary or an inner call lacks a closing parenthesis before the outer closing parenthesis
- **THEN** recovery records the inner error or missing token and keeps the outer argument boundary, following arguments, and enclosing call visible

#### Scenario: Preserve a following declaration after nested damage
- **WHEN** malformed nested call syntax is followed by another `pub fn` declaration
- **THEN** recovery remains bounded to the damaged function and the following declaration remains a separate complete concrete branch
