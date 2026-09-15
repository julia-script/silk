## 1. Bounded proxy policy

- [ ] 1.1 Add `silk.http_proxy` configuration, auth, bypass, route, route-key, and error values with checked owned-capacity validation; prove malformed URI/auth/capacity inputs fail before contact through focused static/analysis assertions.
- [ ] 1.2 Implement exact normalized-origin bypass and pure Direct/Forward/Tunnel selection, including unsupported encrypted-proxy refusal and recomputation hooks; cover the admitted route matrix and equivalent numeric host identities in the shared proxy support program.

## 2. Proxy wire preparation

- [ ] 2.1 Implement Forward absolute-form preparation with origin `Host`, configured-only `Proxy-Authorization`, preserved encoded path/query, and caller-override rejection; assert the emitted head bytes and plaintext credential boundary.
- [ ] 2.2 Implement body-free authority-form CONNECT preparation and bounded owned rejection/challenge metadata; exercise any-2xx, 101, 407, non-2xx, malformed, informational, field, and head-limit outcomes without duplicating the HTTP parser suite.

## 3. Scoped route composition

- [ ] 3.1 Extend the existing client tunnel with the smallest affine `ByteDuplex` adapter needed to preserve its unread suffix and transfer physical close authority exactly once; prove suffix-first reads and success/failure/cancellation release counts.
- [ ] 3.2 Compose Tunnel routes with owned trust and original-origin TLS under the unchanged absolute deadline, then lend the secured transport to the existing client; prove TLS starts only after CONNECT, rejects a proxy-only certificate, and emits no proxy credential inside the tunnel.
- [ ] 3.3 Add native route acquisition beside the existing direct client adapter, resolving and connecting only the selected proxy endpoint and preserving supported-target and synchronous-hostname deadline behavior in focused acceptance cases.

## 4. Registration, documentation, and durable evidence

- [ ] 4.1 Register `silk.http_proxy`, regenerate canonical standard-library/catalog surfaces, and publish a prescriptive proxy reference with the route matrix, plaintext credential warning, limits, deadlines, ownership, errors, and exclusions; verify generated output matches the manifest and examples type-check.
- [ ] 4.2 Integrate one compact proxy support source into the existing shared native and intended LLVM-to-Wasm corpus, reusing head/TLS fixtures and retaining only cases with distinct routing, credential, suffix, deadline, or cleanup signals.
