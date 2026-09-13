## 1. Owned address values

- [x] 1.1 Add the `network_address` actor with owned numeric/domain/endpoint values and constructors;
      verify one consolidated acceptance program covers exact port, domain, ownership, equality, and
      unsupported-form boundaries.
- [x] 1.2 Implement strict RFC 3986 IPv4 and RFC 4291 IPv6 parsers plus RFC 5952 `formatInto` and
      Display behavior; verify tabled cases cover mapped identity, embedded IPv4, longest-leftmost
      compression, no one-group compression, and malformed spellings.

## 2. Portable resolution policy

- [x] 2.1 Add the `resolver` actor with finite requests/results, semantic errors, stable exact
      deduplication, and the replaceable domain Resolver service; verify invalid forged capacities and
      more-than-capacity behavior fail before partial publication.
- [x] 2.2 Implement numeric provider bypass, family filtering, and exact monotonic deadline
      preflight; verify a deterministic provider records zero calls and a deterministic clock records
      exactly one sample for numeric deadlines.
- [x] 2.3 Add a deadline-capable deterministic provider fixture with structured parking ownership;
      verify success and cancellation release the registration, Wake, and result owner without false
      success.

## 3. Selected synchronous native provider

- [x] 3.1 Add the `native_resolver` actor with selected Darwin/GNU declarations, exact rooted query
      and service construction, strict deadline rejection precedence, and typed EAI/errno mapping;
      verify native imports are absent on unsupported targets.
- [x] 3.2 Implement target-layout-aware chain validation, family filtering, stable deduplication,
      finite capacity, and one move-only addrinfo owner; verify focused analysis exposes the expected
      libc imports on GNU and none on Wasm.
- [x] 3.3 Add shared native-acceptance C stubs for success, invalid shape, limit, EAI_SYSTEM, and
      allocation refusal; verify exactly-once release in debug and optimized corpus runs.

## 4. Publication and integration

- [x] 4.1 Register the three actors in the shared standard-library manifest and generated catalog;
      verify ordinary imports resolve on native and Wasm while the selected native actor is unavailable
      on unsupported profiles.
- [x] 4.2 Add the dedicated network-address-resolution reference page and examples; verify it states
      the 64-result bound, native deadline rejection, indefinite scheduler blockage, and absence of
      libc memory/time guarantees.
- [ ] 4.3 **Deferred to JUL-23:** integrate hostname HTTP deadline mapping at the owning HTTP client
      boundary; verify `DeadlineUnsupported` becomes `UnsupportedDeadline` before hostname
      connection dispatch while numeric origins retain their absolute monotonic deadline.
