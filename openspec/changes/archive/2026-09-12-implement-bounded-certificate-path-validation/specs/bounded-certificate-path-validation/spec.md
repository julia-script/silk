## Purpose

Defines deterministic, resource-bounded TLS-server certificate-path construction and validation against caller-supplied explicit trust anchors.

## ADDED Requirements

### Requirement: Validation has an explicit borrowed and capability-free contract

The standard library SHALL expose `CertificatePath.validate` over borrowed leaf, intermediate, anchor, and `Instant` inputs. The operation SHALL require only a mutable allocator, SHALL return owned scalar rejection evidence, and SHALL not read time, perform I/O, mutate trust input, promote peers to anchors, or copy certificate/key bytes into an error. A successful `ValidatedPath` SHALL retain the leaf and caller-order selected indices, copy the supplied validation time, and report revocation as `NotChecked`.

#### Scenario: Successful result preserves input identity

- **WHEN** validation selects an intermediate and anchor from caller-supplied slices
- **THEN** the result borrows the original leaf and reports the exact original intermediate and anchor indices

#### Scenario: No implicit trust or capability

- **WHEN** a self-issued peer leaf also appears among peer certificates but no explicit anchor can issue it
- **THEN** validation rejects it without a clock read, fetch, entropy source, trust cache, or peer-to-anchor promotion

### Requirement: Input and work bounds are finite and inclusive

Validation SHALL preflight checked input lengths and byte sums before semantic inspection or allocation. `ValidationLimits.defaults()` SHALL permit 64 total peer certificates including the leaf, 65536 bytes per peer certificate, 1048576 aggregate peer DER bytes, 1024 anchors, 16777216 aggregate anchor encoded bytes, 8 path certificates excluding the anchor, 4096 issuer candidate visits, 100 signature verifications, 100 complete candidate paths, and 250000 name/constraint comparisons. Zero SHALL permit no corresponding resource or operation. Overflow and exceeded limits SHALL return a typed `ResourceLimit` error without partial success.

#### Scenario: Exact and one-over bounds

- **WHEN** an input or work count equals its configured limit
- **THEN** validation permits that count
- **WHEN** the next counted item or checked sum would exceed the limit
- **THEN** validation returns the named resource-limit dimension at call scope

#### Scenario: Allocation refusal is atomic

- **WHEN** the allocator refuses any path-search or result allocation
- **THEN** all partial owned state is released and no result or input mutation is published

### Requirement: Construction order and candidate identity are deterministic

Validation SHALL perform iterative depth-first search from the leaf. At each depth it SHALL try eligible anchors in supplied order before peer intermediates in supplied order. It SHALL consume one issuer-candidate visit before each candidate subject-name comparison, one signature verification before each primitive verification, one complete-path visit before validating each reached anchor path, and one comparison before each name/subtree comparison. Counters SHALL be global for the call and SHALL not reset for alternate paths. The first fully valid path SHALL win.

Exact duplicate peer intermediate DER SHALL share the lowest original index as one candidate. Distinct cross-signed certificates SHALL remain candidates. Every anchor SHALL remain a distinct ordered candidate, including anchors with identical certificate DER but different configured restrictions. A certificate whose exact DER already occurs on the current path SHALL not be revisited. Search SHALL use an explicit bounded stack and SHALL continue to shorter or otherwise valid alternate branches after an ordinary candidate rejection.

#### Scenario: Anchor-first alternate path succeeds

- **WHEN** earlier anchors or intermediates link but fail signature, profile, time, path-length, or constraint validation and a later path is valid
- **THEN** validation returns the first valid path in anchor-first and original-intermediate order

#### Scenario: Global work exhaustion terminates

- **WHEN** the next candidate, signature, complete path, or comparison would exceed its configured budget
- **THEN** validation returns that `ResourceLimit` immediately even if an unvisited path could succeed

#### Scenario: Duplicate and cyclic candidates are bounded

- **WHEN** peer input contains exact duplicate DER or a subject/issuer cycle
- **THEN** validation preserves lowest-index evidence, avoids repeated current-path DER, and terminates within the configured budgets

### Requirement: The fixed semantic profile governs every selected path

The leaf SHALL be inspected once as a server leaf before search. Selected intermediates and anchors SHALL use `CertificateProfile` under their respective roles. Each selected edge SHALL compare exact complete issuer and subject Name DER and verify the retained original TBS bytes with the admitted issuer key. Path-certificate dates SHALL satisfy `notBefore <= at <= notAfter` at nanosecond-zero endpoints after checked Gregorian conversion for years 1 through 9999. Reversed validity and caller instants outside that range SHALL reject.

Every non-self-issued intermediate CA below a certificate SHALL count against its embedded path length and any independently configured anchor path length. Self-issued means exact subject DER equals issuer DER; it SHALL skip only the specified name-constraint and path-length subordinate count, not profile, signature, date, or usage validation. Anchor validity, serial, inner/outer signature, and self-signature SHALL be ignored, while anchor SPKI, embedded restrictions, configured restrictions, unknown-critical handling, and required policy semantics SHALL remain enforced.

#### Scenario: Original signed bytes and exact name linkage

- **WHEN** an issuer candidate has a normalized-equivalent but byte-distinct name or the signature does not verify over retained TBS DER
- **THEN** that candidate path rejects and deterministic search continues

#### Scenario: Inclusive validity endpoint

- **WHEN** the caller time equals `notBefore` or `notAfter` exactly
- **THEN** the endpoint is valid
- **WHEN** the caller time is one nanosecond after `notAfter`
- **THEN** the selected candidate path rejects as expired

#### Scenario: Anchor exceptions stay bounded

- **WHEN** an explicit anchor has an expired validity period or unsupported self-signature but an admitted key and supported restrictions
- **THEN** those ignored anchor fields do not prevent its use as an issuer endpoint

### Requirement: DNS and IP NameConstraints accumulate root to leaf

Validation SHALL apply supported embedded and configured NameConstraints from anchor to leaf. Exclusions SHALL be evaluated before permissions in source order. Exclusions SHALL union. Permitted subtrees in one constraint source SHALL union, while separately encountered permitted sets SHALL intersect. Self-issued intermediates SHALL carry constraints forward but skip their own names; the leaf SHALL never skip name checks.

DNS comparisons SHALL be ASCII case-insensitive by complete label. Leading-dot constraints SHALL match proper subdomains only; other constraints SHALL match the domain and its subdomains. A presented one-leftmost-label wildcard SHALL be admitted only when an entire permitted subtree contains its possible one-label expansions, and any overlap with an excluded subtree SHALL reject. IP constraints SHALL use only contiguous leading-one IPv4 or IPv6 masks with zero host bits and SHALL compare equal families. No DNS/IP coercion, subject-DN fallback, public-suffix lookup, or network resolution SHALL occur.

#### Scenario: Cumulative permitted and excluded DNS constraints

- **WHEN** separately encountered CA restrictions narrow the permitted DNS space or any exclusion overlaps a presented SAN
- **THEN** the subordinate name must satisfy every permitted source and no exclusion

#### Scenario: IPv4 and IPv6 masks remain separate

- **WHEN** a subordinate IP SAN and a constraint have different address families
- **THEN** that constraint does not match the name
- **WHEN** their families match
- **THEN** the masked address determines permission or exclusion

#### Scenario: Unsupported required constraint semantics reject

- **WHEN** a selected anchor or CA constraint uses a directory name or another unsupported subtree form
- **THEN** that candidate path rejects as `UnsupportedConstraint` instead of bypassing the semantics

### Requirement: Rejections have stable precedence and location

`ValidationError` SHALL contain `InvalidInput`, `Rejected`, or `ResourceLimit`; a stable reason; `Leaf`, `Intermediate(index)`, `Anchor(index)`, or `Call`; an optional extension index; and an optional named limit kind. Invalid caller time and aggregate input bounds SHALL precede semantic errors. A leaf profile rejection SHALL return immediately. Exhausted search SHALL return `NoValidPath` with the first rejected candidate detail in stable traversal order, or `NoIssuer` when no candidate linked. No message string is contractual.

Recognized signature-algorithm parameter failures SHALL map to `UnsupportedParameters` while
unknown signature algorithms remain `UnsupportedAlgorithm`. An empty subject that lacks a nonempty
critical SAN SHALL map to `InvalidName`. These mappings SHALL preserve the profile error's exact
location, original candidate index, optional extension index, and offset.

Within one certificate, rejection precedence SHALL be version/serial/unique identifiers, algorithm consistency and key syntax, extension sequence, role and usage, then date. Path constraint checks SHALL run anchor-to-leaf with excluded checks before permitted checks.

#### Scenario: Stable first rejected candidate

- **WHEN** multiple linked candidates reject for different reasons and no path succeeds
- **THEN** `NoValidPath` carries the first candidate reason and original location selected by traversal order

#### Scenario: Leaf profile rejection is immediate

- **WHEN** the leaf violates the fixed certificate profile
- **THEN** validation returns that leaf reason before issuer traversal or signature work

#### Scenario: Profile taxonomy remains distinguishable

- **WHEN** a linked candidate has forbidden parameters on a recognized signature algorithm, or the leaf violates the empty-subject SAN invariant
- **THEN** validation reports `UnsupportedParameters` or `InvalidName` respectively with unchanged scalar location evidence and without preventing a later valid candidate after an ordinary candidate rejection
