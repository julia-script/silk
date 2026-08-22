## 1. Split the five actors

- [x] 1.1 Extract the complete fact vocabulary and lookup operations into DeclarationFacts.ts
- [x] 1.2 Extract syntax collection and all collection helpers into DeclarationCollection.ts
- [x] 1.3 Extract resolution and conformance-adjacent type queries into their owning DeclarationResolution/ConformanceProof actors
- [x] 1.4 Extract DeclarationCompletion.ts
- [x] 1.5 Extract ConformanceProof.ts and verify index tests pass
- [x] 1.6 Leave `DeclarationIndex` as index data/orchestration only, migrate behavioral callers to their owning actors, eliminate reverse behavioral imports, and remove forwarding exports

## 2. Dedup row analysis

- [x] 2.1 Extract analyzeAppliedRows and route the Effect + generic branches through it
- [x] 2.2 Extract collectRowExpression with a leaf discriminator and replace the two collectors

## 3. Verification

- [x] 3.1 Run pnpm typecheck and verify clean
- [x] 3.2 Run pnpm exec biome check . and verify clean
- [x] 3.3 Run pnpm test (diagnostic codes/spans unchanged)
