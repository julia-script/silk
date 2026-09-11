## Context

See `proposal.md` for motivation and `specs/native-file-trust-source/spec.md` for the complete observable contract. JUL-170 already provides `TrustSource`, `TrustLoadLimits`, `TrustSourceError`, strict `TrustSnapshot.fromPem`, and independent snapshot ownership. `NativeFileSystem` is the sole selected-source libc owner: it validates regular files through descriptor-relative no-follow traversal, retries read EINTR, and gives affine `FileHandle` values one consuming close attempt plus Drop cleanup.

The portable `FileSystem.readFile` operation intentionally materializes an unbounded whole file, so it cannot enforce the acquisition limit. The new provider must call the bounded caller-buffer `NativeFileSystem.readFile` boundary directly while preserving JUL-170's semantic error partition: caller allocation refusal remains an Effect failure; native filesystem name-allocation exhaustion remains a `FileError(NoSpace)`.

## Goals / Non-Goals

**Goals:**

- Keep all trust-source policy in ordinary selected Silk source and all libc calls in `native_filesystem.silk`.
- Make every configured byte and every returned anchor independently owned.
- Enforce an inclusive input limit before retaining excess data and make close-before-decode publication ordering visible in typed outcomes.
- Provide deterministic, target-aware evidence at the cheapest tier that can falsify each ownership, selection, boundary, and native execution claim.

**Non-Goals:**

- Platform trust discovery, keychain/Security.framework integration, revocation, purpose constraints, defaults, fallbacks, environment configuration, directory scans, or command execution.
- A portable filesystem root, sandbox against privileged mounts, watcher, cache, asynchronous blocking offload, or point-in-time semantics for in-place host-file modification.
- New intrinsics, compiler-known library actors, foreign declarations, or duplicate ABI constants.

## Decisions

### Store root and path as separate owners

`NativeFileTrustSource` stores a `Bytes` copy of the native root and an independently rebuilt normalized `Path`. Construction validates root properties and checked combined length before allocation, then copies root and path under the caller allocator. The already-normalized `Path` invariant makes a copy-time `FileError` unreachable for valid input; the implementation nevertheless maps it to `InvalidConfiguration(PathBytes)` at the typed boundary instead of trapping.

This is preferred to concatenating native path bytes because `NativeFileSystem` already defines trusted-root and below-root traversal semantics, and preferred to borrowing configuration because the provider and every load must outlive the caller's inputs.

### Read directly through NativeFileSystem with bounded scratch

Each load opens with the existing `NativeFileSystem.openFile(root, Path.rawBytes(path), false, operation)`. Acquisition uses one reusable scratch buffer of at most 4096 bytes, shrinking the offered slice for the final remaining budget when necessary. Positive counts append only their initialized prefix; zero ends acquisition. Reaching the inclusive limit triggers exactly one separate one-byte read: EOF accepts, and a positive count returns `LimitExceeded(InputBytes)` without appending it.

This is preferred to `FileSystem.readFile` or stat-size preflight because neither limits bytes during acquisition and metadata cannot prove the stream will not grow. Geometric `Bytes` growth remains bounded by the checked remaining budget.

### Bracket acquisition explicitly and decode after close

The load method captures acquisition with `Effect.result`, then consumes the handle through `NativeFileSystem.closeFile`. A successful acquisition proceeds only after successful close; a failed close becomes `File(Close)`. Read failures become `File(Read)`, open failures become `File(Open)`, and cleanup is inspected only to ensure it occurred when a primary read, limit, or allocation outcome already exists. Helper functions consume secondary results without changing the primary error channel.

This mirrors `OsFileSystem`'s established precedence but keeps the distinct trust stages and defers `TrustSnapshot.fromPem` until no descriptor remains. It is preferred to decoding while open because close failure would otherwise occur after a snapshot had been constructed, complicating atomic publication and cleanup.

### Use ordinary selected-source availability

The complete actor and its imports live inside the same static target/profile guard used by `NativeFileSystem` and `OsFileSystem`. No empty compatibility actor exists on other supplies. Manifest and generated documentation therefore show members only for Darwin ARM64/system and GNU Linux ARM64/x86-64, and zero selected members for Wasm/no-libc profiles.

This is preferred to runtime target checks or compiler availability metadata because source selection already proves the intended absence without new privilege.

### Split evidence by the boundary it proves

One structured analysis snapshot per relevant source proves API resolution, ownership, and supported/unsupported target selection. Strict-decoder and portable ownership behavior continue to use the existing shared trust-source acceptance case. A compact native-file fixture exercises real temp-directory open/read/rename/symlink behavior on the host. The existing native-filesystem conformance harness is extended only where deterministic receiver-controlled short reads and read/close failure precedence cannot be induced safely with ordinary files; it reuses the same six target/optimization lanes and libc stubs rather than adding a competing boundary suite.

The real fixture reuses the existing scoped temporary-directory and native-process infrastructure. It mutates and releases caller configuration after construction, executes genuine regular-file loads, checks missing, wrong-kind, no-follow symlink, and deterministic non-root permission denial, and observes a replacement installed with atomic rename. Receiver stubs remain limited to partial reads, exact offered-slice accounting, allocation refusal, and combined acquisition/close faults that regular files cannot induce deterministically.

The synthetic CA fixture is deterministically generated from the named pinned catalog entry by a checked repository command, with canonical PEM wrapping and recorded source/DER hashes. Cases are consolidated around distinct failure boundaries; no live root store, exhaustive matrix, per-feature fresh-process determinism test, or redundant compiler-importing test worker is added.

### Bound cleanup specialization by the selected cleanup plan

Ordinary runtime specialization continues to reject a recursive generic edge that changes runtime-relevant type arguments. Cleanup discovery has a narrower exception because a concrete owner can require Drop implementations for strictly nested field types and the selected service provider owner can require the same nested cleanup. Instance discovery starts this exception only from an exact cleanup target or hook selected by `CleanupPlan.cleanupPlan` and records that plan's concrete owner type as an immutable root. Every later ordinary type argument must be runtime-equal to that root or a semantic structural subterm of it; nominal declarations may be unfolded again only at the exact same or a strictly smaller instantiation. Cross-arity helpers and sibling subterms are therefore admitted without replacing the root or permitting growth. Hidden callable, Effect, and composite-representation identities retain the ordinary recursion guard, except for the pre-existing terminal capture-free callable specialization. The cleanup measure is part of the work-item context, and an ordinary edge to the same target suppresses cleanup provenance, so no unrelated call can inherit the exception and a finite concrete root has only finitely many admitted subterms.

This is preferred to accepting arbitrary same-type continuation from a cleanup-reachable item, recognizing a library actor, or suppressing the ordinary recursion diagnostic. Positive nested Drop and provider-owner fixtures prove the intended path; an unrelated `expand<T>` to `expand<[T; 1]>` remains rejected.

## Risks / Trade-offs

- **[Silk currently exposes only shared subslice views, so the native read cannot receive a mutable prefix view of the scratch allocation.]** → Truncate the scratch's initialized length in place as the remaining budget falls below 4096; its existing allocation is reused and no second scratch allocation is required.
- **[In-place producer writes can mix generations.]** → Document atomic rename as the consistency mechanism and make no stronger promise.
- **[File paths commonly used by host distributions can themselves be symlinks.]** → Preserve the explicit no-follow contract; applications choose a suitable regular generated bundle or caller-managed export.
- **[Cross-target execution depends on configured runners and pinned supplies.]** → Reuse the existing conformance harness, record all available lanes, and state any unavailable runner rather than substituting host-only evidence.
- **[Synthetic certificate fixtures add repository bytes.]** → Reuse existing deterministic certificate fixture material where possible and record its exact digest rather than generating a large new corpus.

## Migration Plan

This is additive. Register the actor and generated surfaces, then callers may explicitly construct and provide it as `TrustSource`. Rollback removes the actor, its generated registrations, documentation, and fixture evidence together; no persistent data format or automatic system configuration is introduced.
