## ADDED Requirements

### Requirement: Direct WebAssembly lowers reachable foreign calls to deterministic imports

The direct WebAssembly backend SHALL declare one imported function per reachable foreign symbol in
canonical symbol order. The import SHALL use a versioned compiler-owned module name, the foreign
symbol as its field name, and WebAssembly scalar parameters and result derived from the classified
C signature for `wasm32-unknown-unknown`. Calls SHALL target that imported handle directly. The
artifact SHALL record the same canonical foreign inventory and include each concrete module/field
pair in its host-import inventory.

#### Scenario: Lower integer, float, pointer, and void classes

- **WHEN** reachable foreign signatures use admitted integer, float, pointer, and void C classes
- **THEN** the backend maps them respectively to `i32` or `i64`, `f32` or `f64`, `i32`, and no WebAssembly result

#### Scenario: Deduplicate agreeing declarations

- **WHEN** multiple reachable declarations name one symbol with the same classified signature
- **THEN** the module contains one import and every corresponding call targets that import

#### Scenario: Preserve deterministic import metadata

- **WHEN** equivalent programs discover agreeing foreign declarations in different source orders
- **THEN** their function imports, artifact foreign inventory, host-import inventory, WAT, and binary bytes are identical
