## 1. Reduced semantic contract

- [x] 1.1 Add one reduced conditional-context source to the existing conditional-conformance test
      and verify its exact handler bound, generic failure row, and generic requirement row reproduce
      the rejected symbolic selection without unrelated diagnostics.
- [x] 1.2 Add negative variants for a missing handler bound and a mismatched requirement row, and
      verify their exact diagnostic codes and responsible spans remain distinct.

## 2. Conditional witness selection

- [x] 2.1 Extend mapped operation witness inference across complete success, failure, and
      requirement contracts; verify resolvable row binders are accepted while unresolved,
      conflicting, and wrong-kind binders retain deterministic diagnostics.
- [x] 2.2 Admit a symbolically matching conditional conformance only from exact enclosing declared
      bounds; verify merely unifying, absent, and row-incompatible bounds are rejected with finite
      requirement evidence.
- [x] 2.3 Retain the symbolic conformance declaration in generic HIR and defer its requirements to
      concrete instance discovery; verify realized witnesses select the same canonical declaration
      and contain no runtime dictionary or provider lookup.
- [x] 2.4 Reuse forward-only normalized requirement-subset proof for an independently inferred open
      `R | Q` Effect row only when no member-well-formed obligation remains; verify the known
      provider may retain enclosing generics, both direct exclusion bounds are checked after
      context-row inference, and no row is inferred by reasoning backward through `Without`.

- [x] 2.5 Separate exact Effect execution channels and physical closure identity from outer
      access/lifetime views across construction, runner selection, verification, and suspension.
- [x] 2.6 Preserve provider-specialized constructor capture parameters and exclude requirement-row
      binders from stored-value lifetime obligations; add reduced positive and corruption cases.

## 3. Streaming server unblock

- [x] 3.1 Realize the reduced nominal handler-to-context adapter and the canonical
      `silk.http_server.withConnection` adapter through the corrected selection path, verifying
      generic success, failure, requirements, and temporary connection ownership remain exact.
