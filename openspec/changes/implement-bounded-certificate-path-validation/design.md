## Context

JUL-185 supplies allocation-free borrowed `CertificateProfile` views and owned `TrustAnchor` values at the exact prerequisite head. Existing P256 and RSA actors verify certificate signatures over retained TBS DER. The path validator must remain ordinary Silk source, with no compiler recognition or runtime capability except the caller's allocator.

The profile actor validates individual SAN and NameConstraints DER but does not yet expose the bounded semantic iteration needed for cumulative path checks. The implementation can add small public, allocation-free profile operations that evaluate subordinate names against one constraint source while retaining profile parsing as the single owner of certificate semantics.

## Goals / Non-Goals

**Goals:**

- Preserve deterministic caller-order evidence and global work counters across alternate path search.
- Keep certificate and anchor inputs borrowed while limiting owned allocations to explicit search state and selected indices.
- Centralize certificate-profile and constraint grammar in `certificate_profile.silk`.
- Make all validation failure evidence scalar, stable, and usable without retaining rejected input.

**Non-Goals:**

- General RFC 5280 or browser Web PKI conformance.
- Hostname verification, TLS CertificateVerify/Finished, revocation, CT, AIA, CRL, OS trust policy, or peer promotion.
- Native recursion, unbounded sentinels, hidden clocks, network access, persistent caches, or compatibility APIs.

## Decisions

### Store iterative DFS state as bounded scalar frames

The validator holds a current path of original peer indices and a stack frame for each depth. A frame records the next anchor and intermediate cursor. This represents recursive DFS explicitly and supports backtracking without native call-stack growth. Pre-reserving all bounded frame/index storage after semantic-free input preflight gives clear atomic OOM behavior. An alternative recursive implementation was rejected because caller-raised depth bounds could exceed native stack capacity.

### Inspect candidate profiles lazily and keep counters global

The leaf is inspected before search. Intermediate and anchor profiles are inspected when a candidate links in traversal order. Candidate visits are counted before subject-name comparison; signature checks and complete paths consume their counters at the required boundary. This avoids semantic work before input preflight and avoids an eager profile failure from an unrelated candidate. An eager cache for all certificates was rejected because unrelated malformed inputs must not poison a valid path.

### Retain first rejection as scalar detail

Ordinary branch failures update a first-rejection slot only once, then search continues. Work-budget failures return immediately. The final `NoValidPath` wraps the retained reason/location, while a search that never linked any candidate reports `NoIssuer`. This keeps stable traversal evidence without storing borrowed DER in the error.

### Evaluate constraint sources without duplicating the profile parser

Small allocation-free `CertificateProfile` operations enumerate validated SAN/NameConstraints values and compare a subordinate profile against one supported constraint source. `certificate_path.silk` owns ordering, source accumulation, self-issued skipping, and global comparison accounting. The profile actor continues to own DER framing, DNS syntax, wildcard semantics, mask validation, and strict supported forms. Duplicating these parsers in the path actor was rejected because it would allow individual admission and cumulative validation to diverge.

### Convert certificate time with checked integer arithmetic

A private Gregorian conversion validates all date components and converts years 1 through 9999 to Unix seconds with checked bounds. Comparison uses the caller's canonical seconds/nanoseconds and second-granularity certificate endpoints. The anchor skips time conversion by design. A `SystemClock` requirement was rejected because one explicit instant must govern the whole call.

### Keep permanent evidence consolidated

One table-driven shared native corpus program covers traversal, policy, limits, and cleanup distinctions without a per-vector compiler process. Structured analysis proves borrowing and capability absence, and one compact Wasm witness proves intended target neutrality. The pull-request CI smoke list selects only the new corpus case. Large limbo sweeps and external engine execution remain offline fixture-generation evidence, not default test work.

## Risks / Trade-offs

- **[Constraint comparison logic is security-sensitive]** → Keep DER parsing in the existing profile actor, pin edge cases, and require independent correctness review.
- **[Alternate paths can magnify work]** → Count all visits globally, consume budgets at exact boundaries, and use finite inclusive caller limits.
- **[Lazy inspection can repeat work after backtracking]** → Cache only bounded scalar/profile state where borrowing permits; correctness relies on global limits rather than cache hits.
- **[The selected profile deliberately diverges from other engines]** → Record pinned source IDs, validation instants, fixture hashes, and each deliberate outcome difference.
- **[Adding a native smoke case increases pull-request runtime]** → Consolidate all runtime distinctions in one corpus case and measure the incremental cost against the exact base.

## Migration Plan

This is a new green-field API. Add the actor, registrations, generated docs, fixtures, and tests atomically. Rollback removes the new actor and its registrations; no stored data or compatibility path exists.
