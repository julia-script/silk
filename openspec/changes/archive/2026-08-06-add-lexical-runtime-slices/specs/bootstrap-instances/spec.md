## ADDED Requirements

### Requirement: Slice-bearing instances key element type without source length

A slice type in a reachable function contract SHALL contribute its canonical element type and access
mode to the instance key and deterministic encoding, but MUST NOT contribute the length of any fixed
array borrowed at a call site. Generic slice functions SHALL specialize by normalized concrete
element arguments under the existing finite monomorphization rules.

#### Scenario: Reuse one function for two source lengths

- **WHEN** one `fold(values: &[I32])` declaration is called with shared borrows of `Array<I32, 3>` and `Array<I32, 6>`
- **THEN** discovery records one `fold` instance and one emitted function symbol

#### Scenario: Distinguish generic element specializations

- **WHEN** a generic slice function is reached with `&[I32]` and `&[Token]`
- **THEN** discovery records distinct concrete element-type instances without adding either source array length to their keys

### Requirement: Slice reachability follows element behavior

Instance discovery SHALL follow the concrete element type of every reachable slice for layout,
Copy, projection, replacement, and cleanup requirements while keeping the borrowed source owner and
its fixed length local to the caller.

#### Scenario: Reach a move-only aggregate through a slice

- **WHEN** a reachable function accepts `&mut [Token]` and replaces an indexed `Token`
- **THEN** instance discovery includes the canonical `Token` layout and cleanup behavior without creating a runtime slice owner
