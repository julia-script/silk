## 1. Decoder implementation

- [x] 1.1 Implement the owned decoder, configuration, progress/error data, exact allocation, frame phases, cumulative budgets, and final-input rules; verify raw/RLE/skippable and boundary fixtures.
- [x] 1.2 Implement bounded compressed-block entropy decoding and sequence execution; verify independently encoded Huffman/FSE/repeat-table fixtures.
- [x] 1.3 Implement seed-zero XXH64 and frame size/checksum validation; verify published checksums and corruption failures.

## 2. Public surface and acceptance

- [x] 2.1 Register canonical source and regenerate embeddings/documentation; verify standard-library and documentation checks plus the prescriptive ownership/terminal contract.
- [x] 2.2 Add consolidated independent acceptance fixtures for chunking, progress, concatenation, malformed input, and resource boundaries; verify focused execution and measure cost against the work base.

## 3. Verification and delivery

- [x] 3.1 Run typecheck, format:check, lint, test, check, and release:candidate in required order; retain exact results.
- [ ] 3.2 Complete independent correctness review and separate test-economics review, fix verified findings, and obtain approval on the committed diff.
- [ ] 3.3 Push the issue branch, create and confirm a draft PR, and update Linear with verified head and evidence.
