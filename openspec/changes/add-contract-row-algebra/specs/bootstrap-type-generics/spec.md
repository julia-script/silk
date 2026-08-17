## MODIFIED Requirements

### Requirement: Contract rows support finite generic algebra

Failure and requirement rows SHALL support normalized union, checked membership and subset
relations, and kind-preserving difference written `Without<R, S>`. A singleton member SHALL be
lifted to a singleton row, and a row on the right SHALL remove every exactly matching member.
Difference SHALL be total: an absent or access-mismatched right-hand member removes nothing.
Operations that promise to handle or provide a member MUST carry a separate checked constraint;
such a constraint proves a type relationship but does not itself discharge an Effect requirement or
failure.

Failure rows SHALL be finite sets under canonical nominal identity. Requirement rows SHALL be
finite maps keyed by canonical capability-role identity and labelled with `Shared` or `Exclusive`
access. Requirement union SHALL join colliding labels to the stronger access. Requirement
membership, subset, and difference SHALL compare the normalized stored access exactly: a shared and
an exclusive entry with the same capability-role key are access-mismatched, not interchangeable.
Provider compatibility is a separate relation and MAY allow an exclusive or owned provider to
satisfy a shared stored requirement; provider selection MUST return the exact stored entry before
difference is evaluated.
Finite-row intersection SHALL retain only exact normalized stored members. For requirements,
shared intersected with exclusive is empty in either order; only shared/shared and
exclusive/exclusive retain that respectively labelled entry.

Checked `S in R` SHALL be kind-directed in the common constraint model. An ordinary value-type
left operand against a failure row SHALL mean singleton nominal membership. While checking a
generic declaration, an open ordinary parameter SHALL lift to a symbolic singleton member term and
its declared membership SHALL be available as assumed evidence. At every complete application it
MUST substitute to one concrete nominal failure member of `R`. A row-kind left operand SHALL mean
normalized subset in the same row domain. Singleton membership and row subset SHALL produce
distinct evidence, and invalid concrete selectors such as `never`, structural or multi-member
unions, references, and non-failure values MUST fail before any operation-specific hook or
availability check. Membership and subset constraints SHALL only check an independently inferred
left operand; they MUST NOT enumerate the source row to synthesize one. Provider selection is the
only constraint in this change that MAY bind an unbound selected row, through its unique-candidate
rule.

`Without` SHALL be a forward-computed form, not an invertible inference pattern. Each row parameter
or lifted member parameter used beneath `Without` MUST be fixed independently by an explicit
generic argument, an exact whole-row occurrence, a supplied value-argument occurrence, or the
provider-selection unique-candidate rule. Analysis MUST report an underconstrained-row diagnostic
rather than infer operands backwards from a `Without` result or enumerate a membership/subset
source row. Substitution MUST renormalize collisions before equality, keys, subset, difference,
candidate counting, or concretization.

Substituting a symbolic singleton member SHALL yield one of three results: a rewritten residual
symbolic member when referenced parameters remain open, one concrete domain member when closed and
valid, or an invalid-singleton result when closed and invalid. Generic-to-generic row substitution
MUST compose parameter identity and member substitutions rather than demanding premature
concreteness; the separate constraint/specialization layer MUST substitute and rebind assumed
evidence without introducing a RowAlgebra-to-evidence dependency.

Every symbolic member SHALL retain a well-formedness obligation until it becomes one valid concrete
member. Normalization and open algebraic simplification MUST preserve that obligation even when the
visible member expression cancels or is removed. Definitional keys and serialized semantic surfaces
MUST include residual obligations, and concrete-row consumers MUST receive only rows whose
obligations have been discharged.

Equal obligation keys from multiple symbolic-member occurrences SHALL coalesce into one semantic
obligation with a canonical ordered set of source origins. Origins MUST be unioned through
normalization and substitution but excluded from definitional equality, keys, and serialized module
identity. Invalid specialization SHALL emit one diagnostic per obligation key, primary at the
responsible explicit argument or application when available and otherwise at the canonical first
origin, with every remaining origin attached as an ordered secondary span.

Open expressions SHALL use a deterministic definitional normal form with associative,
commutative, and idempotent union plus explicitly defined sound reductions; the compiler is not
required to prove arbitrary extensional equality between different open expression shapes. Once
substitution closes an expression, equality and keys SHALL use its concrete extensional row.

Declaration constraints SHALL be assumptions while checking that declaration's generic body and
obligations when the declaration is applied. A callee obligation MAY be discharged only by an
in-scope assumption that is definitionally equivalent after substitution; this change introduces no
implicit access weakening, reborrow, subset transitivity, or open-row weakening proof rules. Partial
application SHALL retain quantified binders, constraints, and evidence in the semantic callable
value until static application. Those nested callable obligations are distinct from obligations of
the enclosing instance and SHALL remain compiler metadata without becoming runtime dictionaries.
Every complete reachable application MUST resolve symbolic evidence and prove its rows finite and
concrete with every member-well-formedness obligation discharged before row-dependent dependency
discovery, witness reachability, layout, ownership specialization, or lowering.
Callable-constraint semantics SHALL be independent of whether the contract originated in Silk
source or the sealed intrinsic inventory.

#### Scenario: Subtract one failure member

- **WHEN** `Without<Problem | Other, Problem>` is specialized
- **THEN** it normalizes to the singleton failure row `Other`

#### Scenario: Subtract a failure row from a failure row

- **WHEN** `Without<First | Second | Third, First | Third>` is specialized
- **THEN** it normalizes to the singleton failure row `Second`

#### Scenario: Treat an absent difference member as a no-op

- **WHEN** `Without<First | Second, Other>` is specialized and `Other` is absent
- **THEN** it normalizes to `First | Second` without reporting an absent-member diagnostic

#### Scenario: Join colliding requirement access

- **WHEN** `&Logger@DefaultRole | &mut Logger@DefaultRole` is normalized
- **THEN** it contains exactly the exclusive entry `&mut Logger@DefaultRole`

#### Scenario: Preserve an exclusive requirement after weaker subtraction

- **WHEN** `Without<&mut Logger@DefaultRole, &Logger@DefaultRole>` is specialized
- **THEN** the access labels do not match and the result remains `&mut Logger@DefaultRole`

#### Scenario: Preserve a shared requirement after stronger subtraction

- **WHEN** `Without<&Logger@DefaultRole, &mut Logger@DefaultRole>` is specialized
- **THEN** the access labels do not match and the result remains `&Logger@DefaultRole`

#### Scenario: Remove the exact stored requirement

- **WHEN** `Without<&mut Logger@DefaultRole | &Clock, &mut Logger@DefaultRole>` is specialized
- **THEN** it normalizes to the singleton requirement row `&Clock`

#### Scenario: Reject access-mismatched checked membership

- **WHEN** a declaration requires `&Logger@DefaultRole` to be a member of a row containing only `&mut Logger@DefaultRole`
- **THEN** analysis reports an access mismatch even though both entries have the same capability-role key

#### Scenario: Preserve an open difference

- **WHEN** a generic declaration contains `Without<Problem | Rest, Problem>` and `Rest` remains open
- **THEN** analysis preserves the equivalent definitional form `Without<Rest, Problem>` rather than assuming `Rest` excludes `Problem`

#### Scenario: Reject inverse difference inference

- **WHEN** the only evidence for `R` is an equation equivalent to `Without<R, Problem> = Other`
- **THEN** analysis reports that `R` is underconstrained instead of choosing among `Other`, `Problem | Other`, or other inverse solutions

#### Scenario: Reject membership-driven selector inference

- **WHEN** the only possible source for ordinary `S` is a checked constraint `S in Problem | Other`
- **THEN** analysis reports `S` as underconstrained rather than enumerating the source row or selecting either member

#### Scenario: Rewrite a symbolic member through generic forwarding

- **WHEN** generic `outer<T, !E> where T in E` calls a generic operation whose member parameter `S` is specialized to still-open caller parameter `T`
- **THEN** row substitution produces a residual symbolic member referring to `T`, the separate constraint layer composes assumed member evidence, and concrete nominal validation is deferred to `outer`'s complete applications

#### Scenario: Rewrite a symbolic requirement member through generic forwarding

- **WHEN** a generic requirement member `&mut P@Audit` is substituted with still-open caller capability parameter `Q`
- **THEN** row substitution produces residual `&mut Q@Audit` with fixed exclusive access and resolved `Audit` role, and the retained well-formedness obligation refers to `Q`

#### Scenario: Concretize a valid symbolic requirement member

- **WHEN** residual requirement member `&mut P@Audit` is completely specialized with capability `P = Logger`
- **THEN** substitution produces exactly concrete member `&mut Logger@Audit` and discharges its member-well-formedness obligation

#### Scenario: Reject an invalid symbolic requirement member

- **WHEN** residual requirement member `&mut P@Audit` is completely specialized with a non-capability value type
- **THEN** substitution reports invalid requirement singleton before row normalization or any row-dependent consumer

#### Scenario: Retain validation after identical symbolic subtraction

- **WHEN** open `Without<Singleton(S), Singleton(S)>` simplifies to an empty visible row and a later complete application supplies `S = First | Second`
- **THEN** the retained member-well-formedness obligation rejects the multi-member substitution instead of accepting the empty result

#### Scenario: Retain validation when subtraction cannot affect the row

- **WHEN** open `Without<never, Singleton(S)>` has an empty left row and a later complete application supplies `S = never`
- **THEN** the retained member-well-formedness obligation rejects the invalid singleton even though valid subtraction would leave the row empty

#### Scenario: Discover a parameter retained only by member validation

- **WHEN** implicit ordinary `S` occurs only in open `Without<Singleton(S), Singleton(S)>` and the visible expression simplifies to empty
- **THEN** parameter discovery traverses the retained well-formedness obligation and reports `S` as underconstrained at its source origin

#### Scenario: Merge repeated member-validation origins deterministically

- **WHEN** two distinct occurrences of the same symbolic singleton normalize to one obligation and a complete application substitutes an invalid member
- **THEN** analysis emits one invalid-singleton diagnostic at the responsible application with both member origins attached in canonical source order, independent of union or substitution order

#### Scenario: Renormalize after substitution collision

- **WHEN** substituting `C = Logger` and `D = Logger` changes `&C | &mut D` into colliding requirement entries
- **THEN** the substituted row renormalizes to exactly `&mut Logger` before any equality, selection, or difference

#### Scenario: Use a declaration constraint inside its generic body

- **WHEN** a generic body calls an operation whose provider-selection obligation is entailed by the body's declared provider-selection constraint
- **THEN** the body type-checks once over open parameters and forwards symbolic evidence without enumerating the open requirement row

#### Scenario: Carry an open nominal member through a generic wrapper

- **WHEN** a generic wrapper declares ordinary `S` with `where S in E`, uses `Without<E, S>` in its result, and calls an operation requiring the same membership
- **THEN** declaration checking lifts `S` to a symbolic singleton member term and forwards assumed member evidence without requiring `S` to be concrete

#### Scenario: Specialize a generic nominal-member wrapper validly

- **WHEN** that wrapper is completely applied with `S = Problem` and concrete `E = Problem | Other`
- **THEN** specialization converts the symbolic singleton to concrete nominal `Problem`, upgrades assumed membership to concrete member evidence, and computes remainder `Other`

#### Scenario: Reject an invalid generic nominal-member specialization

- **WHEN** that wrapper is completely applied with `S = never`, `S = First | Third`, or another non-nominal value
- **THEN** specialization reports invalid nominal singleton selection before difference, dependency discovery, or operation-specific availability

#### Scenario: Retain a constraint through partial application

- **WHEN** a provider argument is captured before the protected Effect row is supplied
- **THEN** the callable value's semantic type, identity, substitution, and serialized surface retain the quantified provider-selection obligation and solve it when the remaining argument is statically applied

#### Scenario: Drop an unapplied constrained callable

- **WHEN** a concrete enclosing instance creates and then drops a provider section without applying its protected Effect argument
- **THEN** the section's nested quantified obligation is erased with the callable and is not rejected as an unresolved obligation of the enclosing instance

#### Scenario: Reject a constrained callable that escapes static application

- **WHEN** a constrained callable with unresolved quantified row obligations reaches an opaque module boundary, runtime representation, or indirect invocation without a statically visible complete application
- **THEN** after any closed-world identity-flow analysis needed to prove the absence of a complete terminal application, analysis rejects the escape before runtime representation, row-dependent consumers, or lowering can treat it as a runtime-polymorphic callable

#### Scenario: Forward a constrained callable through a closed static chain

- **WHEN** a constrained callable is passed through one or more statically visible functions and the closed chain ends in a complete application
- **THEN** its quantified binders, constraints, substitutions, and evidence follow the callable through the chain and are solved at that complete application before representation or lowering

#### Scenario: Do not infer selection from an expected remainder

- **WHEN** a provider matches multiple requirements and an expected result row would reveal which member must have been subtracted
- **THEN** analysis still reports provider ambiguity because expected results may validate the computed remainder but MUST NOT bind the selected row or invert `Without`

#### Scenario: Reject a residual row before instance consumers

- **WHEN** a reachable instance still has an open row, unresolved difference, unsatisfied obligation, or ambiguous selection after its substitution is known
- **THEN** analysis rejects that instance before row-dependent dependencies, witnesses, layout, ownership specialization, or lowering consume it

#### Scenario: Solve equivalent source and intrinsic contracts uniformly

- **WHEN** an ordinary Silk declaration and a sealed intrinsic operation have equivalent binders, fixed parameter modes, row expressions, and checked constraints
- **THEN** calls to both produce the same substitutions, evidence, normalized result, diagnostic identity, and canonical candidate payload, with spans local to each call

### Requirement: Calls infer only from supplied arguments

A generic call MAY supply a contiguous, ordered, kind-correct prefix of explicit generic arguments,
including value-type, failure-row, and requirement-row arguments. Analysis SHALL bind that prefix
positionally and infer every remaining suffix argument only from supplied call arguments and checked
constraints according to each constraint's binding policy: membership and subset are checking-only,
while provider selection may bind its selected row only by the unique-candidate rule. Expected
return types and uses after complete application MUST NOT bind generic arguments or prune constraint
candidates.

Forming an automatic leading-argument section SHALL infer from supplied trailing arguments and
retain every unresolved binder and constraint determined by the omitted leading parameter in the
section's semantic callable type. Applying that section SHALL complete inference from the leading
argument. A missing, conflicting, wrong-kind, or excess explicit argument MUST produce a
deterministic diagnostic at the responsible prefix or application.

#### Scenario: Infer identity from its argument

- **WHEN** `identity(value)` calls `identity<T>(value: T)` with a `Token`
- **THEN** the call specializes `T` as `Token`

#### Scenario: Infer through a generic section

- **WHEN** a generic data-first function forms a section from trailing arguments and is then piped a leading `Token`
- **THEN** the complete application resolves one canonical `Token` specialization

#### Scenario: Refuse return-only inference

- **WHEN** `empty()` calls `empty<T>() -> T` without explicit type arguments
- **THEN** specialization fails even when the call result is later used where `Token` is expected

#### Scenario: Specialize explicitly

- **WHEN** `empty<Token>()` calls `empty<T>() -> T`
- **THEN** the call records the concrete `Token` specialization

#### Scenario: Supply a requirement-row prefix and infer the suffix

- **WHEN** `Effect.provideMut<&mut Logger>(effect, &mut provider)` supplies first binder `?S` and leaves later binders implicit
- **THEN** analysis accepts the requirement-row argument, fixes `S`, and infers the suffix only from `effect`, `provider`, and their checked constraint

#### Scenario: Supply a row prefix through a pipeline

- **WHEN** `effect |> Effect.provideMut<&mut Logger>(&mut provider)` supplies the same first row binder on a trailing-argument section
- **THEN** the section retains the omitted Effect-dependent suffix and completes it from the pipeline input without consulting the expected result

#### Scenario: Lift a failure singleton into a failure-row prefix

- **WHEN** a call whose first binder is `!E` supplies nominal `Problem` as its first explicit generic argument
- **THEN** analysis lifts `Problem` to the singleton failure row `Problem` and infers the remaining suffix from supplied arguments and constraints

#### Scenario: Reject a wrong-kind explicit prefix

- **WHEN** an explicit argument cannot form a member of the binder's row domain, such as `&Logger` for a failure-row binder or non-capability nominal `Problem` for a requirement-row binder
- **THEN** analysis reports a kind mismatch at that explicit generic argument rather than treating every value type as a valid row singleton
