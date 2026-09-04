# bootstrap-language-pressure-programs Specification

## Purpose

Define how complete, recognizable Silk programs pressure the language with differential reference
checks, structural compiler evidence, and pinned native outcomes without being mistaken for a
commitment to self-hosting or replacement of a canonical implementation.

## Requirements

### Requirement: A real Silk lexer exercises ordinary language and library features

The repository SHALL contain a readable lexer written in ordinary Silk source that consumes a
runtime-sized borrowed byte slice and returns owned token and lexical-diagnostic data. It SHALL use
the public allocation and growable-sequence surface, and compiler phases and backends MUST NOT gain
lexer-specific or token-specific operations, layouts, or branches.

#### Scenario: Runtime-sized input produces owned results

- **WHEN** the Silk lexer receives borrowed source bytes whose length is known only at runtime
- **THEN** it returns owned token records with byte spans and owned diagnostics that remain valid independently of the input borrow

#### Scenario: Pressure program remains ordinary Silk

- **WHEN** its published source, MIR, native corpus outcome, or backend artifact is inspected
- **THEN** only general language, allocation, collection, control-flow, and cleanup mechanisms are present

### Requirement: The Silk lexer is checked against the canonical lexer

The Silk lexer SHALL be differentially checked against the TypeScript lexer, which remains the
canonical implementation. The corpus SHALL cover whitespace, comments, identifiers, every current
keyword, decimal integer and float forms, single-line and multiline text, byte, and raw literals
with valid and malformed escapes, recognized and unknown literal modifiers, physical LF and CRLF,
terminated and unterminated delimiters, every current single and compound punctuation token,
end-of-file, and unsupported byte runs.

#### Scenario: Valid source agrees token by token

- **WHEN** representative valid Silk source is lexed by both implementations
- **THEN** the ordered token kinds and half-open byte spans, including trivia and end of file, are identical

#### Scenario: Invalid source agrees on diagnostics

- **WHEN** source contains unsupported byte runs, unknown literal modifiers, or unterminated literal delimiters
- **THEN** both implementations produce identical token spans and lexical diagnostic identities and spans while applying the committed recovery boundary

#### Scenario: Multiline literal forms agree

- **WHEN** the differential corpus exercises escaped `"""` and `b"""` literals containing quotes, LF, CRLF, indentation, code-like content, and pipeline punctuation
- **THEN** both lexers agree on literal category, complete token boundaries, exact source spans, and following tokens

### Requirement: Execution and ownership evidence is independently pinned and deterministic

Representative valid and invalid lexer cases SHALL retain structural compiler assertions and pinned
native corpus outcomes, with LLVM-generated WebAssembly coverage for intended target behavior.
Allocation failure at every exercised growth ordinal SHALL preserve typed `OutOfMemoryError`,
release every acquired allocation exactly once, and leave subsequent runs deterministic. Structural
MIR evidence SHALL cover every ordinal; native execution SHALL cover only distinguishing boundary
cases including the first failure, one mid-growth ordinal, and unrestricted completion.

#### Scenario: Execute a representative valid case

- **WHEN** the valid acceptance case runs from the pinned native corpus and its intended WebAssembly behavior is checked through LLVM
- **THEN** the artifacts report the specified deterministic lexer fingerprint and successful cleanup

#### Scenario: Execute a representative invalid case

- **WHEN** the invalid acceptance case runs from the pinned native corpus
- **THEN** it reports the specified deterministic token-and-diagnostic fingerprint and successful cleanup

#### Scenario: Allocation failure rolls back cleanly

- **WHEN** allocation is rejected at any token or diagnostic vector growth ordinal exercised by the acceptance cases
- **THEN** structural MIR proves the rollback at every ordinal and the native corpus pins the distinguishing boundary outcomes

### Requirement: A bounded stack VM exercises execution and owned observations

The repository SHALL contain a readable bounded stack bytecode VM written in ordinary Silk source.
It SHALL consume a runtime-sized borrowed bytecode slice, execute general arithmetic and
control-flow instructions against a fixed-capacity operand stack, and return one owned growable
ordered `Step | VmDiagnostic` observation vector. Both members SHALL remain ordinary Copy records,
and the stream SHALL be inspected through shared sequence reads after execution. Compiler phases
and backends MUST NOT gain VM-specific, opcode-specific, operand-stack-specific, or event-union-
specific operations, layouts, or branches.

#### Scenario: Branching bytecode produces an ordered owned stream

- **WHEN** the Silk VM executes valid or malformed bytecode whose branch target and instruction count are known only at runtime
- **THEN** it returns the expected result and one ordered step-and-diagnostic stream that remains valid independently of the input borrow and can be read through a shared vector borrow

#### Scenario: Pressure VM remains ordinary Silk

- **WHEN** its published source, MIR, native corpus outcome, or backend artifact is inspected
- **THEN** only general language, allocation, collection, control-flow, failure, union-copy, and cleanup mechanisms are present

### Requirement: Pressure programs use shared sequence observation

The lexer and stack VM SHALL use the public shared `Vector.get` surface for recursively Copy
observations needed after construction. The VM's structural-union event elements SHALL preserve
their active-member and payload provenance. Neither program may regain read access by destructuring
an owned vector, taking an exclusive borrow solely to copy an element, or adding program-specific
compiler behavior. Their findings SHALL record both ordinary shared observation and structural-
union copy provenance as repaired.

#### Scenario: Inspect lexer results without consuming them

- **WHEN** the differential lexer harness fingerprints owned Copy token and diagnostic records
- **THEN** it reads both vectors through shared borrows and the vectors remain live for later observation and cleanup

#### Scenario: Inspect ordered VM union observations

- **WHEN** the stack-VM harness fingerprints its ordered `Step | VmDiagnostic` vector after execution
- **THEN** it reads each event through a shared vector borrow and observes the exact active member and payload in source order

#### Scenario: Retire the separate union defect

- **WHEN** the updated findings classify the observation changes
- **THEN** they mark structural-union copy provenance repaired without implying another self-hosting step

### Requirement: The stack VM is checked against a canonical reference

The Silk VM SHALL be differentially checked against a TypeScript reference VM over valid
arithmetic, taken and untaken branches, malformed opcodes and operands, stack underflow and
overflow, invalid jump targets, and bounded nontermination. The comparison SHALL include the
result, ordered executed steps, and ordered diagnostics.

#### Scenario: Valid programs agree step by step

- **WHEN** representative arithmetic and branching bytecode runs in both implementations
- **THEN** the result and every executed instruction observation are identical and ordered

#### Scenario: Malformed programs agree on recovery

- **WHEN** bytecode contains unsupported opcodes or invalid operands
- **THEN** both implementations emit the same ordered diagnostics and make the same continue-or-stop decision

### Requirement: Stack VM resource behavior is independently pinned and deterministic

Representative valid and malformed VM programs SHALL retain structural compiler assertions and
pinned native corpus outcomes, with LLVM-generated WebAssembly coverage for intended target
behavior. Allocation failure at every exercised trace or diagnostic growth ordinal SHALL preserve
typed `OutOfMemoryError`, release every acquired allocation exactly once, and leave subsequent
executions deterministic. Structural MIR evidence SHALL cover every ordinal; native execution SHALL
cover only distinguishing boundary cases including the first failure, one mid-growth ordinal, and
unrestricted completion.

#### Scenario: Execute VM fingerprints

- **WHEN** representative valid and malformed programs run from the pinned native corpus
- **THEN** each reports its specified deterministic result, trace-and-diagnostic fingerprint, and cleanup outcome

#### Scenario: VM observation allocation rolls back cleanly

- **WHEN** allocation is rejected at any trace or diagnostic vector growth ordinal exercised by the acceptance programs
- **THEN** structural MIR proves the rollback at every ordinal and the native corpus pins the distinguishing boundary outcomes

### Requirement: Pressure findings determine follow-up work

Each language-pressure program SHALL retain a checked-in findings report that classifies observed
walls as language, standard-library, compiler-defect, tooling/ergonomics, or performance/cost
findings. Each finding SHALL cite evidence and state whether it was repaired, deferred to a focused
proposal, or accepted as local complexity. Later reports SHALL compare repeated findings with
earlier programs before promoting a general design. Completing a pressure program MUST NOT
automatically schedule a neighboring compiler port, install the example as production
infrastructure, or begin continuous self-hosting.

#### Scenario: A wall is encountered during implementation

- **WHEN** completing the lexer requires awkward source, a missing general operation, a compiler repair, or a material cost
- **THEN** the report records its category, concrete evidence, disposition, and the smallest plausible general follow-up

#### Scenario: Independent programs expose the same wall

- **WHEN** a later pressure program independently reproduces a deferred language or standard-library finding
- **THEN** the later report compares both evidence sets and states whether they now justify a focused proposal

#### Scenario: A pressure exercise completes

- **WHEN** all acceptance gates for one pressure program pass
- **THEN** the next pressure program or repair is selected from the recorded evidence rather than from a predetermined self-hosting module order

### Requirement: Static composition pressures representation-dependent values

The repository SHALL maintain a complete ordinary-Silk pressure program that composes inspectable
nominal data with statically represented callable and Effect behavior, conditional compile-time
interfaces, and complete operation contracts. The program SHALL normalize different leaf results
before convergence, retain structural MIR evidence, and have independently pinned native outcomes
without compiler-known library actor names.

#### Scenario: Pressure the complete capability set

- **WHEN** the static-composition program is analyzed and run through the shared native corpus
- **THEN** it exercises each enabling language capability in one connected source flow, with MIR
  proving cleanup and the native case producing its pinned result

#### Scenario: Keep the CLI shape non-normative

- **WHEN** the example command and schema actor modules are renamed or replaced by equivalent ordinary source
- **THEN** the compiler continues to accept the program without actor-specific behavior

### Requirement: Local shared ownership removes the SLP-0001 shared-state wall

The repository SHALL contain one readable ordinary-Silk pressure program in which multiple dormant
callbacks retain explicit cloned handles to one fixed-capacity ready inbox and producer/waiter actors
retain cloned handles to one Deferred-style value-and-waiter state. Registration, publication, and
enqueue operations SHALL use short local shared mutation callbacks. Every readiness callback or
other external executable SHALL be moved out of shared state and invoked only after access is
restored. The witness MUST NOT require a compiler-known Shared, queue, Deferred, Scheduler, callback
registry, or execution actor.

The witness SHALL retain cloned handles in both an ordinary stored callable and a dormant unrun
Effect, and SHALL exercise cleanup of an Effect dropped before execution. Publication SHALL move the
offered affine value into shared state exactly once for the witness's one publication call, move only
readiness callbacks out before invoking them, and leave the published value source-owned in shared
state for shared observation by every waiter handle. Repeated-publication result policy SHALL remain
outside this evidence slice and MUST NOT be promoted into a public Deferred contract.

The witness SHALL retain structural MIR evidence for callback and allocation cleanup, plus pinned
native outcomes for enqueue order, one-time publication, and strong-count transitions. Its findings
report SHALL distinguish the removed shared-state wall from the execution-transfer and parking work
that remains owned by SLP-0001.

#### Scenario: Enqueue from two dormant callbacks

- **WHEN** two independently retained callbacks run sequentially through cloned handles to one ready inbox
- **THEN** the inbox contains both identifiers in source execution order without either callback retaining an exclusive lexical borrow

#### Scenario: Publish after extracting waiters

- **WHEN** one producer publishes an affine value to several registered waiters
- **THEN** publication stores exactly one value owner in shared state, moves only the callbacks out under one short mutation, restores access, invokes each callback afterward exactly once, and lets every waiter observe the stored value by shared borrow

#### Scenario: Retain and drop a dormant Effect

- **WHEN** two dormant Effects retain cloned inbox or state handles and one is dropped before it runs
- **THEN** the dropped Effect cleans its captured handle with one non-last decrement while the other Effect and source owner retain the shared state

#### Scenario: Drop unpublished state exactly once

- **WHEN** the last Deferred-style handle is dropped while it contains an unpublished affine value or unconsumed callback
- **THEN** ordinary local-shared cleanup destroys every retained owner exactly once before releasing the state allocation

#### Scenario: Drop one dormant callback early

- **WHEN** a dormant callback holding an inbox clone is dropped before it runs
- **THEN** its handle decrements without releasing the inbox while another handle remains, and final cleanup still balances one acquisition and release

#### Scenario: Pin the observable outcome

- **WHEN** the ready-inbox and Deferred-style acceptance cases run through the shared native corpus
- **THEN** the independently pinned results cover logical values, callback order, and count
  transitions while MIR assertions cover payload and allocation cleanup

#### Scenario: Roll back every construction failure

- **WHEN** construction fails at each exercised `Shared.make` allocation ordinal
- **THEN** structural MIR preserves typed `OutOfMemoryError`, publishes no partial witness actor, cleans every still-owned constructor input, and balances every prior acquisition and release, while the native corpus pins designated boundary outcomes

#### Scenario: Rename every witness actor

- **WHEN** the source-level inbox, Deferred-style state, and wrappers are renamed without changing their ordinary operations
- **THEN** semantic facts, verified MIR, and the pinned native result remain equivalent with no
  actor-specific compiler branch

#### Scenario: Record the remaining SLP-0001 boundary

- **WHEN** the pressure findings are finalized
- **THEN** they mark local shared state sufficient while leaving execution transfer, parking, and wake-order policy to the separate SLP-0001 handoff

#### Scenario: Classify the pressure findings completely

- **WHEN** the witness findings report is finalized
- **THEN** it classifies every observed wall, cites evidence, records disposition and smallest follow-up, compares repeated findings with the lexer and stack VM, and chooses any next work from evidence rather than a predetermined porting order

### Requirement: Independent execution pressure programs are connected ordinary Silk

The pressure corpus SHALL contain readable connected source programs for source-owned result
waiting, deferred first activation, same-thread timer readiness, cancellation before readiness, and
alternate Coroutine-shaped ownership. Each program SHALL expose its ordinary Allocator and failure
rows at source construction points, close every detachable execution environment with owned values,
and use the same general Execution/Wake substrate. Structured analysis and MIR assertions SHALL
cover lifecycle and cleanup facts, while independently pinned native cases cover observable values,
activation and readiness order, and declared runtime boundaries.

#### Scenario: Run the Scheduler-shaped connected witness

- **WHEN** the corpus drives a waiter and producer through ordinary task storage and a ready inbox
- **THEN** the pinned native result covers deferred activation, waiter park, producer publication,
  task-specific readiness, waiter resume, and the final value

#### Scenario: Run the timer-shaped connected witness

- **WHEN** the corpus drives an explicitly owned joining parent and same-thread reactor
- **THEN** the pinned native result covers sibling progress, timer notification, outer eligibility,
  and result data while MIR proves cancellation cleanup

#### Scenario: Run the alternate-owner witness

- **WHEN** the corpus drives one Coroutine-shaped source wrapper through two yielded payloads and completion
- **THEN** the pinned native result covers payload order and structural assertions prove reuse of
  the same intrinsic transitions without Scheduler facts

#### Scenario: Drop the alternate owner while yielded

- **WHEN** the Coroutine-shaped fixture drops its Execution while a yielded payload and Wake remain in the source port
- **THEN** the pinned native result covers late-Wake behavior while MIR assertions prove
  cancellation and exact port, frame, endpoint, and package cleanup without Scheduler-specific
  lowering

#### Scenario: Diagnose the unowned root boundary

- **WHEN** a complete entry closes providers but reaches external parking without explicit Execution ownership
- **THEN** the corpus records the stable diagnostic code and span for the missing delimiter without asserting message text or supplying an implicit owner

#### Scenario: Share analysis work cheaply

- **WHEN** several assertions or engines consume one pressure source program
- **THEN** tests build one realized Analysis snapshot, use structured assertions for compiler semantics, use LLVM-to-Wasm where target behavior matters, and route runtime coverage through the shared native corpus
