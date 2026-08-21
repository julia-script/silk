## 1. Split the five actors

- [x] 1.1 Extract DeclarationFacts.ts
- [x] 1.2 Extract DeclarationCollection.ts
- [x] 1.3 Extract DeclarationResolution.ts
- [x] 1.4 Extract DeclarationCompletion.ts
- [x] 1.5 Extract ConformanceProof.ts and verify index tests pass

## 2. Dedup row analysis

- [ ] 2.1 Extract analyzeAppliedRows and route the Effect + generic branches through it
- [ ] 2.2 Extract collectRowExpression with a leaf discriminator and replace the two collectors

## 3. Verification

- [x] 3.1 Run pnpm typecheck and verify clean
- [x] 3.2 Run pnpm exec biome check . and verify clean
- [x] 3.3 Run pnpm test (diagnostic codes/spans unchanged)