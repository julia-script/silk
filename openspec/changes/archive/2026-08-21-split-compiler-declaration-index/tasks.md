## 1. Split the five actors

- [ ] 1.1 Extract DeclarationFacts.ts
- [ ] 1.2 Extract DeclarationCollection.ts
- [ ] 1.3 Extract DeclarationResolution.ts
- [ ] 1.4 Extract DeclarationCompletion.ts
- [ ] 1.5 Extract ConformanceProof.ts and verify index tests pass

## 2. Dedup row analysis

- [ ] 2.1 Extract analyzeAppliedRows and route the Effect + generic branches through it
- [ ] 2.2 Extract collectRowExpression with a leaf discriminator and replace the two collectors

## 3. Verification

- [ ] 3.1 Run pnpm typecheck and verify clean
- [ ] 3.2 Run pnpm exec biome check . and verify clean
- [ ] 3.3 Run pnpm test (diagnostic codes/spans unchanged)