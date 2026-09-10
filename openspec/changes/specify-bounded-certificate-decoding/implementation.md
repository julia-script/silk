# Implementation follow-up

Status: implementation in progress under JUL-182, separately estimated at **8 points**.
JUL-167 delivered the contract in merged PR #405; Julia subsequently authorized this implementation.
The follow-up owns the complete portable decoder: private DER/schema traversal, strict PEM,
owned Certificate/CertificateBundle, all limits/errors/accessors, manifest fixtures, ordinary
allocator cleanup and generated documentation. Tasks 2.1–2.6 give the implementation order and
observable completion evidence. The estimate includes hostile-input handling and ownership
verification; no completed cryptographic primitives or OS providers are prerequisites.

Use `packages/compiler/stdlib/silk/certificate.silk` and `certificate_bundle.silk` as public
actor homes; private support stays under stdlib support. Register canonical source modules in
`packages/compiler/stdlib/manifest.json`, regenerate the generated stdlib and docs, and extend
existing compiler acceptance infrastructure. No compatibility layer or generic public ASN.1
surface is needed. Source-callable operations remain ordinary Silk, without compiler recognition.

Acceptance is the normative delta and all design tables. Fixture hashes/recipes must be reused,
not replaced with a live certificate download. Add only tests with distinct falsification value:
analysis for API/lifetimes, static evaluation where appropriate for pure computations, shared
native corpus for runtime parsing/cleanup, and a selected LLVM-to-Wasm case for portability.
Do not create a fresh process or compiler pipeline for every mutation.

Explicit exclusions: signature/key verification, path construction/validation, extension semantic
interpretation (including SAN matching), reference identities, root acquisition, transport,
private-key formats, encoding, and generic ASN.1 tooling. JUL-166/168/169/170 retain these seams.

Implementation intake: [JUL-182](https://linear.app/juliaortiz/issue/JUL-182), **8 points**,
In Progress. This implementation is a separate pull request from the merged design.
