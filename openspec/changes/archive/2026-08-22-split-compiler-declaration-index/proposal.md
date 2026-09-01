## Why

**DeclarationIndex.ts** (8,088 lines) mixes the fact model, syntax->fact collection, type resolution, completion/validation (complete is ~1,280 lines of several sub-passes), and conformance proof into one file. Its Effect branch re-parses the generic AppliedType rows nearly verbatim, and its failure vs requirement row-expression collectors are near-duplicates.

## What Changes

- **Split DeclarationIndex.ts** into **DeclarationFacts** (fact model + lookups), **DeclarationCollection**, **DeclarationResolution**, **DeclarationCompletion**, and **ConformanceProof**; collect/complete stay the phase entry points.
- **Extract analyzeAppliedRows** so the Effect and generic AppliedType branches share one row parser.
- **Extract collectRowExpression** with a leaf discriminator, replacing the two near-duplicate RowExpressionFact collectors.

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

<!-- none: behavior-preserving refactor (skip_specs) -->

## Impact

Pure refactor of the frontend index. Diagnostics, codes, and spans stay byte-identical (the shared row parser returns the same diagnostics). Tarjan/import-path dedup is in the shared-helpers change. skip_specs: true.
