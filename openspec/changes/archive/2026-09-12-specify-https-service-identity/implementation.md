# Runtime implementation

Julia authorized implementation of the full created contract after the JUL-169 design handoff.
The runtime work is included in this change and draft PR #406:

- [JUL-183](https://linear.app/juliaortiz/issue/JUL-183), five points: the pure
  `silk.https_identity` matcher, public API/ownership, fixture matrix v1 and documentation.
- [JUL-184](https://linear.app/juliaortiz/issue/JUL-184), five points: the bounded
  `silk.certificate_identities` SAN adapter, explicit caller storage, complete extension selection,
  GeneralNames validation and certificate integration fixtures.

Tasks are tracked in tasks.md section 3. The matcher needs no allocator, decoder handle or provider.
The adapter consumes the existing JUL-182 certificate API and also accepts raw GeneralNames bytes.
Both use ordinary Silk source without new compiler operations.

Fixture matrix v1 remains the matcher policy oracle. All core matching cases run through one native
corpus program; SAN DER and certificate integration cases run through one additional corpus program.
Ownership failures use one shared semantic snapshot rather than native execution. Arithmetic
inspection proves overflow avoidance without enormous allocations. Fixture setup allocations do
not introduce allocation into either public runtime operation.

Generic URI syntax remains with JUL-164, certificate envelope parsing with JUL-167/182, trust/path
and name constraints with JUL-168, and transport/TLS composition with JUL-171. URI/IP text conversion,
origin routing/redirect integration and TLS authentication are not part of these implementations.
The explicit original-host boundary remains a caller obligation; peer names and DNS results never
supply an input implicitly.
