## 1. Confirmation

- [x] 1.1 Confirm a two-operation bound is accepted and both halves are reachable from one generic
      body, with the second operation's name spelled by no operator.
- [x] 1.2 Confirm each specialization reaches its own witness for the non-operator operation.
- [x] 1.3 Confirm the contract shape `HashKey` needs — an operand whose type is not the interface's
      parameter — is admitted, and that every witness operand is forced through a shared borrow.
- [x] 1.4 Confirm the same call against a user-defined witness. **It did not lower.** Recorded as an
      asserted failing combination so the test would fail when the gap closed, and reported on #34.

## 2. The bound-operation call reaching a source witness

- [x] 2.1 Lower a bound-operation call whose witness names a function of the provider's own actor,
      and walk the same conformance when discovering instances. Closed by #155 (PR #157), which also
      added `SEM0101` so a conformance selecting no lowerable witness is reported rather than dropped.
- [x] 2.2 Invert the tripwire of 1.4 to assert the working outcome, keeping the case rather than
      deleting it: it is the path every `HashKey.hash` call over a user-defined key takes.

## 3. HashKey and HashSeed

- [x] 3.1 Add the hashed collection modules to the stdlib manifest under their own namespaces, and
      regenerate the documentation and generated module table.
- [x] 3.2 Declare `HashSeed` and the `HashKey<T>` interface with its equivalence and seeded hash.
- [x] 3.3 Supply witnesses for the key types the collections are tested over, as ordinary Silk.
      `Word` is the standard library's integer key; the scalar types cannot witness `HashKey`
      themselves, which `design.md` records.

## 4. HashMap

- [x] 4.1 Implement the storage over the allocator requirement and typed storage, with no new
      compiler primitive.
- [x] 4.2 Implement insert, reporting whether an entry already existed under an equivalent key.
- [x] 4.3 Implement lookup, distinguishing presence from absence.
- [x] 4.4 Implement removal, transferring ownership of the removed value to the caller.
- [x] 4.5 Implement growth that preserves every prior entry, and fails only with `OutOfMemory`
      leaving the map intact.
- [x] 4.6 Release owned keys and values exactly once on removal, on overwrite, and on the map's own
      drop while non-empty.

## 5. HashSet

- [x] 5.1 Implement insert, membership, and removal over the same storage.
- [x] 5.2 Refuse to store a second element equivalent to one already held.

## 6. Acceptance

- [x] 6.1 A test showing insert, lookup, and removal on `HashMap`.
- [x] 6.2 A test showing two runs with one `HashSeed` give one order.
- [x] 6.3 A test showing the MIR for a `HashMap` program contains no hash operation.
- [x] 6.4 A test showing correct cleanup when the map owns move-only values, asserting acquires
      equal releases.
- [x] 6.5 A test showing a key type with no `HashKey` witness is refused.
- [x] 6.6 Three-engine parity: the same program under the evaluator, the native backend, and the
      WebAssembly backend produces one result and one iteration order.

## 7. Reported, not taken

- [x] 7.1 An `effect fn` whose result struct carries a 64-bit field fails WebAssembly emission. Found
      while writing the move-only ownership tests, bisected to a reproduction naming no collection,
      and reported on #34 for its own ticket. Worked around by widening an `i32` tag in the witness,
      which costs the tests nothing.
