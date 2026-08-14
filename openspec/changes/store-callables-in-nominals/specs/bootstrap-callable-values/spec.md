## MODIFIED Requirements

### Requirement: Callable arguments monomorphize their target

A call passing a callable value SHALL specialize its target on that callable's hidden concrete
identity, exactly as a call passing an Effect value does. Structural callable and Effect contracts
have no standalone target layout. A concrete callable representation stored through a complete
representation-dependent nominal MAY contribute its environment to that enclosing nominal's inline
layout while retaining static monomorphization. Discovery SHALL route every open callable parameter
off the executable path and require one finite concrete instance before layout or MIR.

#### Scenario: Distinguish two callables behind one signature
- **WHEN** one function taking a `once fn(i32) -> i32` is called with two different named functions
- **THEN** each call reaches its own specialized instance naming its target statically, and neither
  instance drops the callable parameter from its lowered contract

#### Scenario: Keep a structural callable unlayoutable
- **WHEN** layout receives only `fn(i32) -> i32` without a concrete representation argument
- **THEN** it reports the existing unavailable layout rather than choosing a uniform closure shape

#### Scenario: Layout a represented callable field
- **WHEN** a complete nominal specialization identifies one callable target and capture environment
- **THEN** layout includes that environment inline without giving the structural callable contract a standalone ABI
