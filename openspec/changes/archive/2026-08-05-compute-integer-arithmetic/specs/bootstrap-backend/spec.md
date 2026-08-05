## ADDED Requirements

### Requirement: The backend emits checked native arithmetic

The LLVM backend SHALL lower each MIR binary operation to overflow-checked native code: add,
subtract, and multiply through the signed with-overflow intrinsics whose overflow flag branches
to a trapping block, and divide and remainder guarded by explicit zero-divisor and
minimum-by-minus-one checks branching to a trapping block before the `sdiv`/`srem` instruction.
The emitted program's behavior SHALL agree with the interpreter across the corpus — matching exit
values for completing programs and abnormal termination for trapping ones — and emission SHALL
remain deterministic, gated by the committed bitcode digest and IR goldens.

#### Scenario: Emit a checked addition

- **WHEN** a program adding two values is emitted
- **THEN** the textual IR contains the signed add-with-overflow intrinsic and a conditional branch to a trapping block

#### Scenario: Guard a division natively

- **WHEN** a program dividing two values is emitted and run with a zero divisor
- **THEN** the native executable terminates abnormally exactly as the interpreter blocked

#### Scenario: Keep arithmetic emission deterministic

- **WHEN** the committed arithmetic fixture is emitted repeatedly in fresh processes
- **THEN** the bitcode digest and IR text equal the committed goldens byte-for-byte
