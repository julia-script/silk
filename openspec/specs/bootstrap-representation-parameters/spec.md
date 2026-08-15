# bootstrap-representation-parameters Specification

## Purpose

Define kinded callable and Effect representation parameters that let nominal identities preserve
statically known executable representations without runtime erasure or dispatch.

## Requirements

### Requirement: Declarations bind representation parameters

Generic declarations SHALL accept `F: CallableContract` and `F: EffectContract` as representation
parameters distinct from ordinary types, failure rows, and requirement rows. Duplicate, unbound,
wrong-kind, or incompatible representation uses MUST produce deterministic source diagnostics.

#### Scenario: Bind a callable representation

- **WHEN** `struct Mapper<A, B, F: fn(A) -> B> { transform: F }` is analyzed
- **THEN** `F` is a representation parameter whose value contract is the declared callable bound

#### Scenario: Reject a representation as an ordinary type

- **WHEN** source supplies a failure row, requirement row, or unrelated value type for `F`
- **THEN** analysis reports the kind mismatch before specialization

### Requirement: Representation identity is separate from use bounds

A concrete representation SHALL retain one intrinsic identity and intrinsic callable or Effect
contract. A represented use SHALL separately retain the parameter's required bound and an
admissibility proof; exact representation equality MUST NOT change when one reusable callable is
admitted under both `fn` and `once fn` bounds.

#### Scenario: Admit one function under two access bounds

- **WHEN** the same named reusable function initializes fields bounded by `fn(A) -> B` and
  `once fn(A) -> B`
- **THEN** both uses retain the same exact representation identity and distinct admissibility proofs

### Requirement: Nominal applications carry kinded arguments

A complete nominal application SHALL carry one ordered, kind-checked argument vector containing its
declared ordinary type, failure-row, requirement-row, and representation arguments. Construction
SHALL infer representation arguments from field initializers, and every repeated use of one binder
MUST unify to one representation.

#### Scenario: Infer one representation through repeated fields

- **WHEN** two fields declared with the same `F` are initialized from values with one concrete
  callable identity
- **THEN** construction records one canonical `F` argument for the complete nominal type

#### Scenario: Reject two identities for one binder

- **WHEN** repeated `F` fields are initialized from distinct callable identities
- **THEN** construction fails at the conflicting initializer instead of erasing either identity

### Requirement: Representation arguments survive specialization

Representation arguments SHALL survive inference, substitution, nested nominal applications,
borrows, non-owning field projection, function parameters and results, HIR, and instance discovery.
Every reachable layout and MIR boundary MUST receive a concrete representation or an explicit
unavailable fact; no backend may recover an identity from source syntax.

#### Scenario: Forward a nested representation generically

- **WHEN** a generic wrapper receives and returns a nominal carrying open representation `F`
- **THEN** reachable specialization substitutes one concrete representation through both nesting
  levels before layout and MIR

### Requirement: Representation joins remain static

Assignment, branch, result, and aggregate joins SHALL accept representation-dependent nominal values
only when their complete arguments are equal. A failed join MUST identify the first deterministic
divergent representation origin and MUST NOT insert allocation, existential packaging, or erasure.

#### Scenario: Converge after consuming distinct representations

- **WHEN** two branches consume different parsers internally and each returns `i32`
- **THEN** the `i32` results join even though the parser representations never do

#### Scenario: Reject a parser join before consumption

- **WHEN** the branches return parsers containing two distinct callable identities
- **THEN** analysis reports the divergent representation before MIR

### Requirement: Representation facts are deterministic and non-runtime

Canonical equality, ordering, hashing, encoding, presentation, and diagnostics SHALL be deterministic
across fresh processes. Representation arguments SHALL produce no runtime type descriptor,
dictionary, actor-name lookup, or standard-library spelling dependency.

#### Scenario: Repeat representation analysis

- **WHEN** equivalent source is analyzed in separate fresh processes
- **THEN** its representation facts, nominal keys, HIR, instance ordering, and diagnostics are
  byte-identical
