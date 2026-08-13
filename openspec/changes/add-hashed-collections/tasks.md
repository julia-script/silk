## 1. Confirmation

- [x] 1.1 Confirm a two-operation bound is accepted and both halves are reachable from one generic
      body, with the second operation's name spelled by no operator.
- [x] 1.2 Confirm each specialization reaches its own witness for the non-operator operation.
- [x] 1.3 Confirm the contract shape `HashKey` needs — an operand whose type is not the interface's
      parameter — is admitted, and that every witness operand is forced through a shared borrow.
- [x] 1.4 Confirm the same call against a user-defined witness. **It does not lower.** Recorded as an
      asserted failing combination so the test fails when the gap closes, and reported on #34.

## 2. Blocked on the bound-operation call reaching a source witness

Everything below needs `HashKey.hash` to be callable. See `design.md`.

- [ ] 2.1 Lower a bound-operation call whose witness names a function of the provider's own actor,
      and walk the same conformance when discovering instances.

## 3. HashKey and HashSeed

- [ ] 3.1 Add the hashed collection module to the stdlib manifest under its own namespace, and
      regenerate the documentation and generated module table.
- [ ] 3.2 Declare `HashSeed` and the `HashKey<T>` interface with its equivalence and seeded hash.
- [ ] 3.3 Supply witnesses for the key types the collections are tested over, as ordinary Silk.

## 4. HashMap

- [ ] 4.1 Implement the storage over the allocator requirement and typed storage, with no new
      compiler primitive.
- [ ] 4.2 Implement insert, reporting whether an entry already existed under an equivalent key.
- [ ] 4.3 Implement lookup, distinguishing presence from absence.
- [ ] 4.4 Implement removal, transferring ownership of the removed value to the caller.
- [ ] 4.5 Implement growth that preserves every prior entry, and fails only with `OutOfMemory`
      leaving the map intact.
- [ ] 4.6 Release owned keys and values exactly once on removal, on overwrite, and on the map's own
      drop while non-empty.

## 5. HashSet

- [ ] 5.1 Implement insert, membership, and removal over the same storage.
- [ ] 5.2 Refuse to store a second element equivalent to one already held.

## 6. Acceptance

- [ ] 6.1 A test showing insert, lookup, and removal on `HashMap`.
- [ ] 6.2 A test showing two runs with one `HashSeed` give one order.
- [ ] 6.3 A test showing the MIR for a `HashMap` program contains no hash operation.
- [ ] 6.4 A test showing correct cleanup when the map owns move-only values, asserting acquires
      equal releases.
- [ ] 6.5 A test showing a key type with no `HashKey` witness is refused.
- [ ] 6.6 Three-engine parity: the same program under the evaluator, the native backend, and the
      WebAssembly backend produces one result and one iteration order.
